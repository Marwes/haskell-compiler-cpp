#include "Catch/include/catch.hpp"
#include "Tokenizer.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{

std::ostream& operator<<(std::ostream& out, const Token& token)
{
	return out << "{" << enumToString(token.type) << ", " << token.name << "}";
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



TEST_CASE("tokenizer/let", "Let expression")
{
	std::stringstream stream("let x = 3 in (2+x)");
	Tokenizer tokenizer(stream);

	Token expected [] = {
		Token(SymbolEnum::LET, "let"),
		Token(SymbolEnum::LBRACKET, "{"),
		Token(SymbolEnum::NAME, "x"),
		Token(SymbolEnum::EQUALSSIGN, "="),
		Token(SymbolEnum::NUMBER, "3"),
		Token(SymbolEnum::RBRACKET, "}"),
		Token(SymbolEnum::IN, "in"),
		Token(SymbolEnum::LPARENS, "("),
		Token(SymbolEnum::NUMBER, "2"),
		Token(SymbolEnum::OPERATOR, "+"),
		Token(SymbolEnum::NAME, "x"),
		Token(SymbolEnum::RPARENS, ")"),
	};
	tokenizer.tokenize();
	for (int ii = 0; tokenizer; ++tokenizer, ++ii)
	{
		REQUIRE(*tokenizer == expected[ii]);
	}
}



TEST_CASE("tokenizer/let2", "Let expression")
{
const char* expr =
"let x = 3\n\
    y = 2\n\
in y*x";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);

	Token expected [] = {
		Token(SymbolEnum::LET, "let"),
		Token(SymbolEnum::LBRACKET, "{"),
		Token(SymbolEnum::NAME, "x"),
		Token(SymbolEnum::EQUALSSIGN, "="),
		Token(SymbolEnum::NUMBER, "3"),
		Token(SymbolEnum::SEMICOLON, ";"),
		Token(SymbolEnum::NAME, "y"),
		Token(SymbolEnum::EQUALSSIGN, "="),
		Token(SymbolEnum::NUMBER, "2"),
		Token(SymbolEnum::RBRACKET, "}"),
		Token(SymbolEnum::IN, "in"),
		Token(SymbolEnum::NAME, "y"),
		Token(SymbolEnum::OPERATOR, "*"),
		Token(SymbolEnum::NAME, "x"),
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

