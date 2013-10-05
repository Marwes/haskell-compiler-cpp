#include "Catch/include/catch.hpp"
#include "Parser.h"

using namespace MyVMNamespace;

namespace MyVMNamespace {
    
bool operator==(const Name& lhs, const Name& rhs)
{
    return lhs.name == rhs.name;
}

bool operator==(const Number& lhs, const Number& rhs)
{
    return lhs.value == rhs.value;
}

bool operator==(const FunctionApplication& lhs, const FunctionApplication& rhs)
{
    return lhs.function == rhs.function && lhs.arguments == rhs.arguments;
}



bool operator==(const Expression& lhs, const Expression& rhs)
{
    {
        auto r = dynamic_cast<const FunctionApplication*>(&rhs);
        auto l = dynamic_cast<const FunctionApplication*>(&lhs);
        if (r && l)
            return *r == *l;
    }
    
    {
        auto r = dynamic_cast<const Name*>(&rhs);
        auto l = dynamic_cast<const Name*>(&lhs);
        if (r && l)
            return *r == *l;
    }
    {
        auto r = dynamic_cast<const Number*>(&rhs);
        auto l = dynamic_cast<const Number*>(&lhs);
        if (r && l)
            return *r == *l;
    }
    return false;
}


bool operator!=(const Expression& lhs, const Expression& rhs)
{
    return !(lhs == rhs);
}

}

TEST_CASE("parser/3+2", "3 + 2")
{
    std::stringstream stream("3 + 2");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    std::unique_ptr<Expression> maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

    FunctionApplication* func = dynamic_cast<FunctionApplication*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (func->arguments.size() == 2);
    REQUIRE (*func->arguments[0] == Number(3));
    REQUIRE (*func->arguments[1] == Number(2));
    
}

TEST_CASE("parser/3", "3 + 2 + 4")
{
    std::stringstream stream("3 + 2 + 4");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

    FunctionApplication* func = dynamic_cast<FunctionApplication*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (func->arguments.size() == 2);
    REQUIRE (*func->arguments[0] == Number(3));

    const FunctionApplication* second = dynamic_cast<const FunctionApplication*>(func->arguments[1].get());
    REQUIRE (second != NULL);
    REQUIRE (*second->arguments[0] == Number(2));
    REQUIRE (*second->arguments[1] == Number(4));
}


TEST_CASE("parser/3 + (2 + 4)", "3 + 2 + 4")
{
    std::stringstream stream("3 + ( 2 + 4 )");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

    FunctionApplication* func = dynamic_cast<FunctionApplication*>(maybeExpression.get());
    REQUIRE (func != NULL);
    Name* name = dynamic_cast<Name*>(func->function.get());
    REQUIRE (name->name == "+");
    REQUIRE (func->arguments.size() == 2);
    REQUIRE (*func->arguments[0] == Number(3));

    const FunctionApplication* second = dynamic_cast<const FunctionApplication*>(func->arguments[1].get());
    REQUIRE (second != NULL);
    Name* secondName = dynamic_cast<Name*>(second->function.get());
    REQUIRE (secondName->name == "+");
    REQUIRE (*second->arguments[0] == Number(2));
    REQUIRE (*second->arguments[1] == Number(4));
}