#include <string>
#include <fstream>
#include <array>
#include <map>
#include "Tokenizer.h"

namespace MyVMNamespace
{

	DEFINE_ENUM(SymbolEnum, SYMBOLENUM);

const std::map<std::string, SymbolEnum> keywords = {
	std::make_pair("let", SymbolEnum::LET),
	std::make_pair("in", SymbolEnum::IN),
	std::make_pair("case", SymbolEnum::CASE),
	std::make_pair("of", SymbolEnum::OF)
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
	if (tokens.size() > 1)
	{
		SymbolEnum& type = (tokens.end() - 2)->type;
		return type == SymbolEnum::LET || type == SymbolEnum::OF;
	}
	return false;
}

std::istream& Tokenizer::readToken(Token& token)
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
	if (newLine)
	{
		tokens.push_back(Token(SymbolEnum::INDENTLEVEL, "<n>", indentLevel));
		--offset;
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
		else
		{
			token.type = SymbolEnum::OPERATOR;
		}
	}
	else if (c == ';')
	{
		getChar(c);//For consistency, otherwise '=' is put back into the stream
		token.type = SymbolEnum::SEMICOLON;
	}
	else if (isdigit(c))
	{
		while (getChar(c) && isdigit(c))
		{
			token.name.push_back(c);
		}
		token.type = SymbolEnum::NUMBER;
	}
	else if (isalpha(c))
	{
		while (getChar(c) && isalnum(c))
		{
			token.name.push_back(c);
		}
		token.type = nameOrKeyWord(token.name);
	}
	else if (c == '(')
	{
		token.type = SymbolEnum::LPARENS;
		return input;
	}
	else if (c == ')')
	{
		token.type = SymbolEnum::RPARENS;
		return input;
	}
	else if (c == '[')
	{
		token.type = SymbolEnum::LBRACKET;
		return input;
	}
	else if (c == ']')
	{
		token.type = SymbolEnum::RBRACKET;
		return input;
	}
	else if (c == ',')
	{
		token.type = SymbolEnum::COMMA;
		return input;
	}

	if (!isspace(c))
	{
		input.unget();
		input.bad();
		return input;
	}

	return input;
}


Tokenizer& Tokenizer::operator++()
{
	if (offset < 0)
	{
		offset++;
		return *this;
	}
	tokenize();
	tokenize2(**this);

	return *this;
}

bool Tokenizer::tokenize()
{
	tokens.push_back(Token());
	Token& tok = tokens.back();
	if (readToken(tok))
	{
		if (tok.type != SymbolEnum::LBRACKET)
		{
			if (previousTokenWasKeyword())
			{
				tokens.push_back(Token(SymbolEnum::INDENTSTART, "<n>", this->indentLevel));
				std::swap(*(tokens.end() - 2), tokens.back());
				--offset;
			}
		}
		return true;
	}
	return false;
}

bool parseError(const Token& tok)
{
	return tok.type == SymbolEnum::IN || tok.type == SymbolEnum::OF;//???
}

bool Tokenizer::tokenize2(const Token& tok)
{
	{
		if (tok.type == SymbolEnum::INDENTLEVEL)//<n> token
		{
			if (indentLevels.size() > 0)//m:ms
			{
				if (indentLevels.back() == tok.indent)//m == n
				{
					tokens.back() = Token(SymbolEnum::SEMICOLON, ";");
					return true;
				}
				else if (tok.indent < indentLevels.back())// n < m
				{
					tokens.push_back(Token(SymbolEnum::LBRACKET, "{"));
					std::swap(*(tokens.end() - 2), tokens.back());
					--offset;//Go back one step to return {
					return true;
				}
			}
			else
			{
				return tokenize();
			}
		}
		else if (tok.type == SymbolEnum::INDENTSTART)//{n} token
		{
			int n = tok.indent;
			if (indentLevels.size() > 0)//m:ms
			{
				int m = indentLevels.back();
				if (n > indentLevels.back())// n > m
				{
					tokens.back() = Token(SymbolEnum::LBRACKET, "{");
					indentLevels.push_back(m);
					indentLevels.push_back(n);
					return true;
				}
			}
			if (indentLevels.size() == 0 && n > 0)
			{
				*(tokens.end() - 2) = Token(SymbolEnum::LBRACKET, "{");
				indentLevels.push_back(n);
				return true;
			}
		}
		else if (tok.type == SymbolEnum::RBRACKET)
		{
			if (indentLevels.size() > 0 && indentLevels.back() == 0)
			{
				indentLevels.pop_back();
				return true;
			}
			else
			{
				return false;//parse-error
			}
		}
		else if (tok.type == SymbolEnum::LBRACKET)
		{
			indentLevels.push_back(0);
			return true;
		}

		if (!indentLevels.empty())
		{
			int m = indentLevels.back();
			if (m != 0 && parseError(tok))
			{
				tokens.push_back(Token(SymbolEnum::RBRACKET, "}"));
				std::swap(*(tokens.end() - 2), tokens.back());
				--offset;
				return true;
			}
		}
		return true;
	}
	if (!input)
	{
		if (indentLevels.empty())
		{
			return false;
		}
		else if (indentLevels.back() != 0)
		{
			tokens.push_back(Token(SymbolEnum::RBRACKET, "}"));
			return true;
		}
	}
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