#include "Catch/include/catch.hpp"
#include "Parser.h"

using namespace MyVMNamespace;

TEST_CASE("parser", "Test parsing")
{
    std::stringstream stream("3 + 2");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression);
    REQUIRE (typeid(*maybeExpression) == typeid(FunctionApplication));
}