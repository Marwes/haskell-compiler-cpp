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

std::istream& operator>>(std::istream& input, Token& token)
{
	char c = '\0';
	while (input.get(c) && isspace(c))
		;
	token.name.clear();
	token.name.push_back(c);

	//Decide how to tokenize depending on what the first char is
	//ie if its an operator then more operators will follow
	if (isOperator(c))
	{
		while (input.get(c) && isOperator(c))
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
		input.get(c);//For consistency, otherwise '=' is put back into the stream
		token.type = SymbolEnum::SEMICOLON;
	}
	else if (isdigit(c))
	{
		while (input.get(c) && isdigit(c))
		{
			token.name.push_back(c);
		}
		token.type = SymbolEnum::NUMBER;
	}
	else if (isalpha(c))
	{
		while (input.get(c) && isalnum(c))
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

Token::Token()
	: type(SymbolEnum::NONE)
{
}

Token::Token(SymbolEnum type, const std::string& name)
	: type(type)
	, name(name)
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