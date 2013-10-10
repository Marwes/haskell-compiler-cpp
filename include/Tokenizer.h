#pragma once
#include <fstream>
#include <string>
#include <boost/circular_buffer.hpp>
#include "Util.h"

namespace MyVMNamespace
{
#define SYMBOLENUM(XX) \
	XX(NONE) \
	XX(NAME) \
	XX(OPERATOR) \
	XX(NUMBER) \
	XX(LPARENS) \
	XX(RPARENS) \
	XX(EQUALSSIGN) \
	XX(SEMICOLON) \
	XX(LET) \
	XX(IN) \
	XX(CASE) \
	XX(OF) \

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