#include "Catch/include/catch.hpp"
#include "Parser.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{
bool operator==(const Token& lhs, const Token& rhs)
{
    return lhs.type == rhs.type && lhs.name == rhs.name;
}
}

TEST_CASE("tokenizer/3 + (2 * 4)", "3 + (2 * 4)")
{
    std::stringstream stream("3 + ( 2 * 4 )");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

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
    Parser parser(tokenizer);

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

