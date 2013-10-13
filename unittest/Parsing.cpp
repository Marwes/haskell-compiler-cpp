#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
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

    PrimOP* func = dynamic_cast<PrimOP*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (*func->lhs == Number(3));
    REQUIRE (*func->rhs == Number(2));
    
}

TEST_CASE("parser/3", "3 + 2 + 4")
{
    std::stringstream stream("3 + 2 + 4");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

    PrimOP* func = dynamic_cast<PrimOP*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (*func->rhs == Number(4));

    const PrimOP* first = dynamic_cast<const PrimOP*>(func->lhs.get());
    REQUIRE (first != NULL);
    REQUIRE (*first->lhs == Number(3));
    REQUIRE (*first->rhs == Number(2));
}


TEST_CASE("parser/3 + (2 + 4)", "3 + ( 2 + 4 )")
{
    std::stringstream stream("3 + ( 2 + 4 )");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

    PrimOP* func = dynamic_cast<PrimOP*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (func->op == OP::ADD);
    REQUIRE (*func->lhs == Number(3));

    const PrimOP* second = dynamic_cast<const PrimOP*>(func->rhs.get());
    REQUIRE (second != NULL);
    REQUIRE (second->op == OP::ADD);
    REQUIRE (*second->lhs == Number(2));
    REQUIRE (*second->rhs == Number(4));
}



TEST_CASE("parser/three * 2 + 4", "three * 2 + 4")
{
	std::stringstream stream("three * 2 + 4");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	PrimOP* func = dynamic_cast<PrimOP*>(maybeExpression.get());
	REQUIRE(func != NULL);
	REQUIRE(func->op == OP::ADD);
	REQUIRE(*func->rhs == Number(4));

	const PrimOP* second = dynamic_cast<const PrimOP*>(func->lhs.get());
	REQUIRE(second != NULL);
	REQUIRE(second->op == OP::MULTIPLY);
	REQUIRE(*second->lhs == Name("three"));
	REQUIRE(*second->rhs == Number(2));
}


TEST_CASE("parser/let three = 3 in three + 4", "")
{
	std::stringstream stream("let three = 3 in three + 4");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	Let* let = dynamic_cast<Let*>(maybeExpression.get());
	REQUIRE(let != NULL);
	REQUIRE(let->bindings[0].first == "three");
	REQUIRE(*let->bindings[0].second == Number(3));
	PrimOP* op = dynamic_cast<PrimOP*>(let->expression.get());
	REQUIRE(op->op == OP::ADD);
	REQUIRE(*op->lhs == Name("three"));
	REQUIRE(*op->rhs == Number(4));
}


TEST_CASE("parser/let six = 3 * 2 in six + 4", "")
{
	std::stringstream stream("let three = 3 * 2 in three + 4");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	Let* let = dynamic_cast<Let*>(maybeExpression.get());
	REQUIRE(let != NULL);
	REQUIRE(let->bindings[0].first == "three");

	PrimOP* letPrimOP = dynamic_cast<PrimOP*>(let->bindings[0].second.get());
	REQUIRE(letPrimOP != NULL);
	REQUIRE(*letPrimOP->lhs == Number(3));
	REQUIRE(*letPrimOP->rhs == Number(2));

	PrimOP* op = dynamic_cast<PrimOP*>(let->expression.get());
	REQUIRE(op->op == OP::ADD);
	REQUIRE(*op->lhs == Name("three"));
	REQUIRE(*op->rhs == Number(4));
}

TEST_CASE("parser/apply", "Function application")
{
	std::stringstream stream("f 3");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	Apply& apply = dynamic_cast<Apply&>(*maybeExpression);
	REQUIRE(*apply.function == Name("f"));
	REQUIRE(*apply.arguments[0] == Number(3));
}

TEST_CASE("parser/apply2", "Function application")
{
	std::stringstream stream("f 3 (3+two)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	Apply& apply = dynamic_cast<Apply&>(*maybeExpression);
	REQUIRE(*apply.function == Name("f"));
	REQUIRE(*apply.arguments[0] == Number(3));

	PrimOP& op = dynamic_cast<PrimOP&>(*apply.arguments[1]);
	REQUIRE(*op.lhs == Number(3));
	REQUIRE(*op.rhs == Name("two"));
}

TEST_CASE("parser/applyOperator", "Function application")
{
	std::stringstream stream("f 3 one * two");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	PrimOP& op = dynamic_cast<PrimOP&>(*maybeExpression);

	Apply& apply = dynamic_cast<Apply&>(*op.lhs);
	REQUIRE(*apply.function == Name("f"));
	REQUIRE(*apply.arguments[0] == Number(3));
	REQUIRE(*apply.arguments[1] == Name("one"));

	REQUIRE(*op.rhs == Name("two"));
}