#include <string>
#include <fstream>
#include <array>
#include <map>
#include "Tokenizer.h"

namespace MyVMNamespace
{

	DEFINE_ENUM(SymbolEnum, SYMBOLENUM);

const std::map<std::string, SymbolEnum> keywords = {
	std::make_pair("module", SymbolEnum::MODULE),
	std::make_pair("class", SymbolEnum::CLASS),
	std::make_pair("instance", SymbolEnum::INSTANCE),
	std::make_pair("where", SymbolEnum::WHERE),
	std::make_pair("let", SymbolEnum::LET),
	std::make_pair("in", SymbolEnum::IN),
	std::make_pair("case", SymbolEnum::CASE),
	std::make_pair("of", SymbolEnum::OF),
	std::make_pair("->", SymbolEnum::ARROW),
	std::make_pair("data", SymbolEnum::DATA)
};

SymbolEnum nameOrKeyWord(const std::string& name)
{
	auto found = keywords.find(name);
	if (found != keywords.end())
	{
		return found->second;
	}
	else
	{
		return SymbolEnum::NAME;
	}
}

bool isOperator(char c)
{
	static std::string operators("+-*/.$:=<>|&");
	return operators.find(c) != -1;
}

//Gets the next char from the input stream and updates the current indentation level
bool Tokenizer::getChar(char& c)
{
	if (input.get(c))
	{
		previousLocation = currentLocation;
		++currentLocation.absolute;
		++currentLocation.column;
		if (c == '\n' || c == '\r')
		{
			currentLocation.column = 0;
			currentLocation.row++;
		}
		return true;
	}
	return false;
}


//Check if the previously parse token was a LET or OF
bool Tokenizer::previousTokenWasKeyword()
{
	if (!tokens.empty())
	{
		SymbolEnum& type = tokens.back().type;
		return type == SymbolEnum::LET || type == SymbolEnum::OF || type == SymbolEnum::WHERE;
	}
	return false;
}

//read in the next token and if a newline were found before finding the token
//Returns if the token was succesfully parsed
bool Tokenizer::readToken(Token& token, bool& newline)
{
	char c = '\0';
	//Skipe all whitespace before the token
	while (getChar(c) && isspace(c))
	{
		if (currentLocation.column == 0)//newline detected
		{
			newline = true;
		}
	}
	token.sourceLocation = this->currentLocation;
	token.name.clear();
	if (c == '\0')
		return false;
	token.name.push_back(c);

	//Decide how to tokenize depending on what the first char is
	//ie if its an operator then more operators will follow
	if (isOperator(c))
	{
		while (getChar(c) && isOperator(c))
		{
			token.name.push_back(c);
		}
		if (token.name == "=")
		{
			token.type = SymbolEnum::EQUALSSIGN;
		}
		else if (token.name == "->")
		{
			token.type = SymbolEnum::ARROW;
		}
		else if (token.name == "::")
		{
			token.type = SymbolEnum::TYPEDECL;
		}
		else
		{
			token.type = SymbolEnum::OPERATOR;
		}
		input.unget();
		currentLocation = previousLocation;
		return true;
	}
	else if (isdigit(c))
	{
		while (getChar(c) && isdigit(c))
		{
			token.name.push_back(c);
		}
		if (c == '.')
		{
			token.name.push_back(c);
			while (getChar(c) && isdigit(c))
			{
				token.name.push_back(c);
			}
			token.type = SymbolEnum::FLOAT;
		}
		else
		{
			token.type = SymbolEnum::NUMBER;
		}
		input.unget();
		currentLocation = previousLocation;
		return true;
	}
	else if (isalpha(c) || c == '_')
	{
		while (getChar(c) && (isalnum(c) || c == '_'))
		{
			token.name.push_back(c);
		}
		token.type = nameOrKeyWord(token.name);
		input.unget();
		currentLocation = previousLocation;
		return true;
	}
	else if (c == ';')
	{
		token.type = SymbolEnum::SEMICOLON;
	}
	else if (c == '(')
	{
		token.type = SymbolEnum::LPARENS;
		return true;
	}
	else if (c == ')')
	{
		token.type = SymbolEnum::RPARENS;
		return true;
	}
	else if (c == '[')
	{
		token.type = SymbolEnum::LBRACKET;
		return true;
	}
	else if (c == ']')
	{
		token.type = SymbolEnum::RBRACKET;
		return true;
	}
	else if (c == '{')
	{
		token.type = SymbolEnum::LBRACE;
		return true;
	}
	else if (c == '}')
	{
		token.type = SymbolEnum::RBRACE;
		return true;
	}
	else if (c == ',')
	{
		token.type = SymbolEnum::COMMA;
		return true;
	}
	return input.good();
}


const Token& Tokenizer::nextToken(bool (*parseError)(const Token&))
{
	//Check if the tokenizer has backtracked
	if (offset < 0)
	{
		offset++;
		return **this;
	}
	else if (!unprocessedTokens.empty())
	{
		nextLayoutIndependentToken(parseError);
		return **this;
	}
	tokenize(parseError);
	return **this;
}

Tokenizer& Tokenizer::operator++()
{
	nextToken();
	return *this;
}

const Token& Tokenizer::tokenizeModule()
{
	unprocessedTokens.reserve(unprocessedTokens.size() + 4);
	unprocessedTokens.push_back(Token());
	Token& tok = unprocessedTokens.back();
	bool newline = false;
	readToken(tok, newline);
	
	if (tok.type != SymbolEnum::LBRACE && tok.type != SymbolEnum::MODULE)
	{
		unprocessedTokens.push_back(Token(SymbolEnum::INDENTSTART, "{n}", tok.sourceLocation));
	}
	if (newline)
	{
		unprocessedTokens.push_back(Token(SymbolEnum::INDENTLEVEL, "<n>", tok.sourceLocation));
	}
	
	nextLayoutIndependentToken();
	return **this;
}

bool Tokenizer::tokenize(bool (*parseError)(const Token&))
{
	unprocessedTokens.reserve(unprocessedTokens.size() + 4);
	unprocessedTokens.push_back(Token());
	Token& tok = unprocessedTokens.back();
	bool newline = false;
	bool success = readToken(tok, newline);
	if (tok.type != SymbolEnum::LBRACE)
	{
		if (previousTokenWasKeyword())
		{
			unprocessedTokens.push_back(Token(SymbolEnum::INDENTSTART, "{n}", tok.sourceLocation));
		}
	}
	if (newline)
	{
		unprocessedTokens.push_back(Token(SymbolEnum::INDENTLEVEL, "<n>", tok.sourceLocation));
	}
	nextLayoutIndependentToken(parseError);
	return success;
}

bool parseError(const Token& tok)
{
	return tok.type == SymbolEnum::IN || tok.type == SymbolEnum::OF;//???
}

//Convert the next unprocessed token into the layout independent form (by inserting '{', ';' and '}')
//See the Haskell 98 report on syntax
bool Tokenizer::nextLayoutIndependentToken(bool (*parseError)(const Token&))
{
	if (!unprocessedTokens.empty())
	{
		const Token& tok = unprocessedTokens.back();
		if (tok.type == SymbolEnum::INDENTLEVEL)//<n> token
		{
			if (indentLevels.size() > 0)//m:ms
			{
				int m = indentLevels.back();
				if (m == tok.sourceLocation.column)//m == n
				{
					tokens.push_back(Token(SymbolEnum::SEMICOLON, ";"));
					unprocessedTokens.pop_back();
					return true;
				}
				else if (tok.sourceLocation.column < m)// n < m
				{
					//TODO
					indentLevels.pop_back();
					tokens.push_back(Token(SymbolEnum::RBRACE, "}"));
					return true;
				}
			}
			unprocessedTokens.pop_back();
			if (unprocessedTokens.empty())
				return tokenize();
			else
				return nextLayoutIndependentToken(parseError);
		}
		else if (tok.type == SymbolEnum::INDENTSTART)//{n} token
		{
			int n = tok.sourceLocation.column;
			if (!indentLevels.empty())//m:ms
			{
				int m = indentLevels.back();
				if (n > m)
				{
					unprocessedTokens.pop_back();
					tokens.push_back(Token(SymbolEnum::LBRACE, "{"));
					indentLevels.push_back(n);
					return true;
				}
			}
			if (n > 0)
			{
				tokens.push_back(Token(SymbolEnum::LBRACE, "{"));
				unprocessedTokens.pop_back();
				indentLevels.push_back(n);
				return true;
			}
			tokens.push_back(Token(SymbolEnum::LBRACE, "{"));
			tokens.push_back(Token(SymbolEnum::RBRACE, "}"));
			unprocessedTokens.back() = Token(SymbolEnum::INDENTLEVEL, "<n>", tok.sourceLocation);
			offset--;
			return true;
		}
		else if (tok.type == SymbolEnum::RBRACE)
		{
			if (indentLevels.size() > 0 && indentLevels.back() == 0)
			{
				tokens.push_back(tok);
				unprocessedTokens.pop_back();
				indentLevels.pop_back();
				return true;
			}
			else
			{
				return false;//parse-error
			}
		}
		else if (tok.type == SymbolEnum::LBRACE)
		{
			tokens.push_back(unprocessedTokens.back());
			unprocessedTokens.pop_back();
			indentLevels.push_back(0);
			return true;
		}

		if (!indentLevels.empty())//L (t:ts) (m:ms) 	= 	} : (L (t:ts) ms) 	if m /= 0 and parse-error(t) 
		{
			int m = indentLevels.back();
			if (m != 0 && parseError && parseError(tok))
			{
				tokens.push_back(Token(SymbolEnum::RBRACE, "}", tok.sourceLocation));
				indentLevels.pop_back();
				return true;
			}
		}
		tokens.push_back(unprocessedTokens.back());
		unprocessedTokens.pop_back();
		return true;
	}
	else //No unprocessed tokens
	{
		if (indentLevels.empty())//End of stream
		{
			return false;
		}
		else if (indentLevels.back() != 0)//Keep pusing righ brackets
		{
			indentLevels.pop_back();
			tokens.push_back(Token(SymbolEnum::RBRACE, "}"));
			return true;
		}
	}
	return false;
}

Token::Token()
	: type(SymbolEnum::NONE)
{
}

Token::Token(SymbolEnum type, const std::string& name, Location location)
	: type(type)
	, name(name)
	, sourceLocation(location)
{
}

}