#include <string>
#include <fstream>
#include "Tokenizer.h"

namespace MyVMNamespace
{

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
		token.type = SymbolEnum::OPERATOR;
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
		token.type = SymbolEnum::NAME;
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

	if (!isspace(c))
	{
		input.unget();
		input.bad();
		return input;
	}

	token.tokenize();
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