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

std::istream& operator>>(std::istream& input, Token& token);


class Tokenizer
{
public:
	Tokenizer(std::istream& stream, size_t backTrack = 10)
		: stream(stream)
		, tokens(backTrack)
		, offset(0)
	{
	}

	const Token& nextToken()
	{
		++(*this);
		return **this;
	}

	bool tokenize()
	{
		tokens.push_back(Token());
		stream >> tokens.back();
		return *this;
	}

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
		return stream.good();
	}

	bool operator!()
	{
		return !stream;
	}

private:
	std::istream& stream;
	boost::circular_buffer<Token> tokens;
	int offset;
};

}