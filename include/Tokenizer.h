#pragma once
#include <fstream>
#include <string>
#include <vector>
#include <boost/circular_buffer.hpp>
#include "Util.h"

namespace MyVMNamespace
{
/*
	Enum defining all the possible tokens that can occur in a haskell file
	INDENTSTART and INDENTLEVEL are used internally to tokenize indentation based code but are never returned as a valid token
*/
#define SYMBOLENUM(t, XX) \
	XX(t,NONE) \
	XX(t,NAME) \
	XX(t,OPERATOR) \
	XX(t,NUMBER) \
	XX(t,FLOAT) \
	XX(t,LPARENS) \
	XX(t,RPARENS) \
	XX(t,LBRACKET) \
	XX(t,RBRACKET) \
	XX(t,LBRACE) \
	XX(t,RBRACE) \
	XX(t,INDENTSTART) \
	XX(t,INDENTLEVEL) \
	XX(t,COMMA) \
	XX(t,EQUALSSIGN) \
	XX(t,SEMICOLON) \
	XX(t,MODULE) \
	XX(t,CLASS) \
	XX(t,INSTANCE) \
	XX(t,WHERE) \
	XX(t,LET) \
	XX(t,IN) \
	XX(t,CASE) \
	XX(t,OF) \
	XX(t,ARROW) \
	XX(t,TYPEDECL) \
	XX(t,DATA)

DECLARE_ENUM(SymbolEnum, SYMBOLENUM);


class Token
{
public:
	Token();
	Token(SymbolEnum type, const std::string& name, Location location = Location());

	SymbolEnum type;
	std::string name;
	Location sourceLocation;
};


class Tokenizer
{
	//This class takes an istream and outputs converts it into a stream of tokens
public:
	Tokenizer(std::istream& input, size_t backTrack = 10)
		: input(input)
		, tokens(backTrack)
		, offset(0)
	{
		currentLocation.column = currentLocation.row = currentLocation.absolute = 0;
	}

	const Token& tokenizeModule();

	/*
	Takes a parseError function which returns true if the token is not a valid token
	Returns SymbolEnum::NONE if end of stream, otherwise the token found
	*/
	const Token& nextToken(bool (*parseError)(const Token&) = nullptr);

	bool tokenize(bool (*parseError)(const Token&) = nullptr);

	const Token& operator*() const
	{
		const Token& t = tokens[int(tokens.size()) + offset - 1];
		return t;
	}

	const Token* operator->() const
	{
		return &**this;
	}

	//Moves the tokenizer to the next token
	Tokenizer& operator++();

	Tokenizer& operator--()
	{
		--offset;
		return *this;
	}

	operator bool()
	{
		return offset < 0 || input.good();
	}

	bool operator!()
	{
		return !*this;
	}

	std::istream& getInputStream()
	{
		return input;
	}

private:
	bool readToken(Token& token, bool& newline);
	bool getChar(char& c);
	bool previousTokenWasKeyword();
	bool nextLayoutIndependentToken(bool (*parseError)(const Token&) = nullptr);

	std::istream& input;
	boost::circular_buffer<Token> tokens;
	std::vector<Token> unprocessedTokens;
	std::vector<int> indentLevels;
	int offset;
	Location currentLocation, previousLocation;
};

}