#pragma once
#include <fstream>
#include <string>
#include <boost/circular_buffer.hpp>
#include "Util.h"

namespace MyVMNamespace
{
#define SYMBOLENUM(t, XX) \
	XX(t,NONE) \
	XX(t,NAME) \
	XX(t,OPERATOR) \
	XX(t,NUMBER) \
	XX(t,LPARENS) \
	XX(t,RPARENS) \
	XX(t,LBRACKET) \
	XX(t,RBRACKET) \
	XX(t,COMMA) \
	XX(t,EQUALSSIGN) \
	XX(t,SEMICOLON) \
	XX(t,LET) \
	XX(t,IN) \
	XX(t,CASE) \
	XX(t,OF) \

DECLARE_ENUM(SymbolEnum, SYMBOLENUM);


class Token
{
public:
	Token();
	Token(SymbolEnum type, const std::string& name);
	void tokenize();

	SymbolEnum type;
	std::string name;
};


class Tokenizer
{
public:
	Tokenizer(std::istream& input, size_t backTrack = 10)
		: input(input)
		, tokens(backTrack)
		, offset(0)
	{
	}

	const Token& nextToken()
	{
		++(*this);
		return **this;
	}

	bool tokenize();

	const Token& operator*() const
	{
		return tokens[int(tokens.size()) + offset - 1];
	}

	const Token* operator->() const
	{
		return &**this;
	}

	Tokenizer& operator++()
	{
		if (offset < 0)
		{
			offset++;
			return *this;
		}
		tokenize();
		return *this;
	}
	Tokenizer& operator--()
	{
		--offset;
		return *this;
	}

	operator bool()
	{
		return input.good();
	}

	bool operator!()
	{
		return !input;
	}

private:
	std::istream& readToken(Token& token);

	std::istream& input;
	boost::circular_buffer<Token> tokens;
	int offset;
};

}