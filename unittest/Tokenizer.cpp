#include "Catch/include/catch.hpp"
#include "Tokenizer.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{
std::ostream& operator<<(std::ostream& out, SymbolEnum symbol)
{
	switch (symbol)
	{
	case SymbolEnum::NONE:
		return out << "NONE";
	case SymbolEnum::NAME:
		return out << "NAME";
	case SymbolEnum::OPERATOR:
		return out << "OPERATOR";
	case SymbolEnum::NUMBER:
		return out << "NUMBER";
	case SymbolEnum::LPARENS:
		return out << "LPARENS";
	case SymbolEnum::RPARENS:
		return out << "RPARENS";
	case SymbolEnum::EQUALSSIGN:
		return out << "EQUALSIGN";
	case SymbolEnum::LET:
		return out << "LET";
	case SymbolEnum::IN:
		return out << "IN";
	default:
		throw std::runtime_error("Unkown SymbolEnum");
		break;
	}
}

std::ostream& operator<<(std::ostream& out, const Token& token)
{
	return out << "{" << token.type << ", " << token.name << "}";
}

bool operator==(const Token& lhs, const Token& rhs)
{
    return lhs.type == rhs.type && lhs.name == rhs.name;
}
}

TEST_CASE("tokenizer/3 + (2 * 4)", "3 + (2 * 4)")
{
    std::stringstream stream("3 + ( 2 * 4 )");
    Tokenizer tokenizer(stream);

    Token expected[] = {
        Token(SymbolEnum::NUMBER, "3"),
        Token(SymbolEnum::OPERATOR, "+"),
        Token(SymbolEnum::LPARENS, "("),
        Token(SymbolEnum::NUMBER, "2"),
        Token(SymbolEnum::OPERATOR, "*"),
        Token(SymbolEnum::NUMBER, "4"),
        Token(SymbolEnum::RPARENS, ")")
    };
    tokenizer.tokenize();
    for (int ii = 0; tokenizer; ++tokenizer, ++ii)
    {
        REQUIRE (*tokenizer == expected[ii]);
    }
}


TEST_CASE("tokenizer/3 + (2 / 4) * one", "3 + (2 + 4)")
{
    std::stringstream stream("(3+ 2)/ 4*one");
    Tokenizer tokenizer(stream);

    Token expected[] = {
        Token(SymbolEnum::LPARENS, "("),
        Token(SymbolEnum::NUMBER, "3"),
        Token(SymbolEnum::OPERATOR, "+"),
        Token(SymbolEnum::NUMBER, "2"),
        Token(SymbolEnum::RPARENS, ")"),
        Token(SymbolEnum::OPERATOR, "/"),
        Token(SymbolEnum::NUMBER, "4"),
        Token(SymbolEnum::OPERATOR, "*"),
        Token(SymbolEnum::NAME, "one")
    };
    tokenizer.tokenize();
    for (int ii = 0; tokenizer; ++tokenizer, ++ii)
    {
        REQUIRE (*tokenizer == expected[ii]);
    }
}


TEST_CASE("tokenizer/let, in", "")
{
	std::stringstream stream("let two = 1 in 4*two");
	Tokenizer tokenizer(stream);

	Token expected [] = {
		Token(SymbolEnum::LET, "let"),
		Token(SymbolEnum::NAME, "two"),
		Token(SymbolEnum::EQUALSSIGN, "="),
		Token(SymbolEnum::NUMBER, "1"),
		Token(SymbolEnum::IN, "in"),
		Token(SymbolEnum::NUMBER, "4"),
		Token(SymbolEnum::OPERATOR, "*"),
		Token(SymbolEnum::NAME, "two")
	};
	tokenizer.tokenize();
	for (int ii = 0; tokenizer; ++tokenizer, ++ii)
	{
		REQUIRE(*tokenizer == expected[ii]);
	}
}

TEST_CASE("tokenizer/apply", "FUnction application")
{
	std::stringstream stream("f 3");
	Tokenizer tokenizer(stream);

	Token expected [] = {
		Token(SymbolEnum::NAME, "f"),
		Token(SymbolEnum::NUMBER, "3"),
	};
	tokenizer.tokenize();
	for (int ii = 0; tokenizer; ++tokenizer, ++ii)
	{
		REQUIRE(*tokenizer == expected[ii]);
	}
}


TEST_CASE("tokenizer/apply2", "FUnction application")
{
	std::stringstream stream("f 3 (2+two)");
	Tokenizer tokenizer(stream);

	Token expected [] = {
		Token(SymbolEnum::NAME, "f"),
		Token(SymbolEnum::NUMBER, "3"),
		Token(SymbolEnum::LPARENS, "("),
		Token(SymbolEnum::NUMBER, "2"),
		Token(SymbolEnum::OPERATOR, "+"),
		Token(SymbolEnum::NAME, "two"),
		Token(SymbolEnum::RPARENS, ")"),
	};
	tokenizer.tokenize();
	for (int ii = 0; tokenizer; ++tokenizer, ++ii)
	{
		REQUIRE(*tokenizer == expected[ii]);
	}
}

