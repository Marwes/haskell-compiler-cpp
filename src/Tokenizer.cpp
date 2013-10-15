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
	std::make_pair("where", SymbolEnum::WHERE),
	std::make_pair("let", SymbolEnum::LET),
	std::make_pair("in", SymbolEnum::IN),
	std::make_pair("case", SymbolEnum::CASE),
	std::make_pair("of", SymbolEnum::OF),
	std::make_pair("->", SymbolEnum::ARROW)
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

bool isTokenSeparator(char c)
{
	return isspace(c) || c == '(' || c == ')' || c == '[' || c == ']' || c == ',';
}


bool isOperator(char c)
{
	static std::string operators("+-*/.$=<>");
	return operators.find(c) != -1;
}

bool Tokenizer::getChar(char& c)
{
	if (input.get(c))
	{
		++indentLevel;
		if (c == '\n' || c == '\r')
		{
			this->indentLevel = 0;
		}
		return true;
	}
	return false;
}


bool Tokenizer::previousTokenWasKeyword()
{
	if (!tokens.empty())
	{
		SymbolEnum& type = tokens.back().type;
		return type == SymbolEnum::LET || type == SymbolEnum::OF;
	}
	return false;
}

bool Tokenizer::readToken(Token& token)
{
	bool newLine = false;
	char c = '\0';
	while (getChar(c) && isspace(c))
	{
		if (indentLevel == 0)//newline detected
		{
			newLine = true;
		}
	}
	token.indent = indentLevel;
	if (newLine)
	{
		unprocessedTokens.push_back(Token(SymbolEnum::INDENTLEVEL, "<n>", indentLevel));
	}
	token.name.clear();
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
		else
		{
			token.type = SymbolEnum::OPERATOR;
		}
		input.unget();
		--this->indentLevel;
		return true;
	}
	else if (isdigit(c))
	{
		while (getChar(c) && isdigit(c))
		{
			token.name.push_back(c);
		}
		token.type = SymbolEnum::NUMBER;
		input.unget();
		--this->indentLevel;
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
		--this->indentLevel;
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
	if (offset < 0)
	{
		offset++;
		return **this;
	}
	else if (!unprocessedTokens.empty())
	{
		tokenize2(parseError);
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
	bool success = false;
	if (readToken(tok))
	{
		if (tok.type != SymbolEnum::LBRACKET || tok.type != SymbolEnum::MODULE)
		{
			unprocessedTokens.push_back(Token(SymbolEnum::INDENTSTART, "{n}", tok.indent));
		}
		success = true;
	}
	tokenize2();
	return **this;
}

bool Tokenizer::tokenize(bool (*parseError)(const Token&))
{
	unprocessedTokens.reserve(unprocessedTokens.size() + 4);
	unprocessedTokens.push_back(Token());
	Token& tok = unprocessedTokens.back();
	bool success = false;
	if (readToken(tok))
	{
		if (tok.type != SymbolEnum::LBRACE)
		{
			if (previousTokenWasKeyword())
			{
				unprocessedTokens.push_back(Token(SymbolEnum::INDENTSTART, "{n}", tok.indent));
			}
		}
		success = true;
	}
	tokenize2(parseError);
	return success;
}

bool parseError(const Token& tok)
{
	return tok.type == SymbolEnum::IN || tok.type == SymbolEnum::OF;//???
}

bool Tokenizer::tokenize2(bool (*parseError)(const Token&))
{
	if (!unprocessedTokens.empty())
	{
		const Token& tok = unprocessedTokens.back();
		if (tok.type == SymbolEnum::INDENTLEVEL)//<n> token
		{
			if (indentLevels.size() > 0)//m:ms
			{
				int m = indentLevels.back();
				if (m == tok.indent)//m == n
				{
					tokens.push_back(Token(SymbolEnum::SEMICOLON, ";"));
					unprocessedTokens.pop_back();
					return true;
				}
				else if (tok.indent < m)// n < m
				{
					//TODO
					indentLevels.pop_back();
					tokens.push_back(Token(SymbolEnum::RBRACE, "}"));
					return true;
				}
			}
			else
			{
				unprocessedTokens.pop_back();
				if (unprocessedTokens.empty())
					return tokenize();
				else
					return tokenize2();
			}
		}
		else if (tok.type == SymbolEnum::INDENTSTART)//{n} token
		{
			int n = tok.indent;
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
			unprocessedTokens.back() = Token(SymbolEnum::INDENTLEVEL, "<n>", tok.indent);
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
				tokens.push_back(Token(SymbolEnum::RBRACE, "}"));
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

Token::Token(SymbolEnum type, const std::string& name, int indent)
	: type(type)
	, name(name)
	, indent(indent)
{
}

void Token::tokenize()
{
	if (name.empty())
	{
		type = SymbolEnum::NONE;
	}
	else if (name == "(")
	{
		type = SymbolEnum::LPARENS;
	}
	else if (name == ")")
	{
		type = SymbolEnum::RPARENS;
	}
	else if (std::all_of(name.begin(), name.end(), isOperator))
	{
		type = SymbolEnum::OPERATOR;
	}
	else if (std::all_of(name.begin(), name.end(), isdigit))
	{
		type = SymbolEnum::NUMBER;
	}
	else
	{
		type = SymbolEnum::NAME;
	}
}

}