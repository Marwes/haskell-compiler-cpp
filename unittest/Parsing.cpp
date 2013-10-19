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

bool operator==(const Apply& lhs, const Apply& rhs)
{
    return lhs.function == rhs.function && lhs.arguments == rhs.arguments;
}



bool operator==(const Expression& lhs, const Expression& rhs)
{
    {
        auto r = dynamic_cast<const Apply*>(&rhs);
		auto l = dynamic_cast<const Apply*>(&lhs);
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

bool operator==(const PatternName& lhs, const PatternName& rhs)
{
	return lhs.name == rhs.name;
}
bool operator==(const NumberLiteral& lhs, const NumberLiteral& rhs)
{
	return lhs.value == rhs.value;
}

bool operator==(const Pattern& lhs, const Pattern& rhs)
{
	{
		auto r = dynamic_cast<const PatternName*>(&rhs);
		auto l = dynamic_cast<const PatternName*>(&lhs);
		if (r && l)
			return *r == *l;
	}

	{
		auto r = dynamic_cast<const NumberLiteral*>(&rhs);
		auto l = dynamic_cast<const NumberLiteral*>(&lhs);
		if (r && l)
			return *r == *l;
	}
	return false;
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
	REQUIRE(func->op == PrimOps::ADD);
    REQUIRE (*func->lhs == Number(3));

    const PrimOP* second = dynamic_cast<const PrimOP*>(func->rhs.get());
    REQUIRE (second != NULL);
	REQUIRE(second->op == PrimOps::ADD);
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
	REQUIRE(func->op == PrimOps::ADD);
	REQUIRE(*func->rhs == Number(4));

	const PrimOP* second = dynamic_cast<const PrimOP*>(func->lhs.get());
	REQUIRE(second != NULL);
	REQUIRE(second->op == PrimOps::MULTIPLY);
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
	REQUIRE(let->bindings[0].name == "three");
	REQUIRE(*let->bindings[0].expression == Number(3));
	PrimOP* op = dynamic_cast<PrimOP*>(let->expression.get());
	REQUIRE(op->op == PrimOps::ADD);
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
	REQUIRE(let->bindings[0].name == "three");

	PrimOP* letPrimOP = dynamic_cast<PrimOP*>(let->bindings[0].expression.get());
	REQUIRE(letPrimOP != NULL);
	REQUIRE(*letPrimOP->lhs == Number(3));
	REQUIRE(*letPrimOP->rhs == Number(2));

	PrimOP* op = dynamic_cast<PrimOP*>(let->expression.get());
	REQUIRE(op->op == PrimOps::ADD);
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

TEST_CASE("parser/tuple", "Function application")
{
	std::stringstream stream("(1,2)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	Apply& apply = dynamic_cast<Apply&>(*maybeExpression);

	REQUIRE(*apply.function == Name("(,)"));
	REQUIRE(*apply.arguments[0] == Number(1));
	REQUIRE(*apply.arguments[1] == Number(2));
}

TEST_CASE("parser/tuple2", "Function application")
{
	std::stringstream stream("f (one,2, 3)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	Apply& applyF = dynamic_cast<Apply&>(*maybeExpression);
	REQUIRE(*applyF.function == Name("f"));

	Apply& applyTuple = dynamic_cast<Apply&>(*applyF.arguments[0]);
	REQUIRE(*applyTuple.arguments[0] == Name("one"));
	REQUIRE(*applyTuple.arguments[1] == Number(2));
	REQUIRE(*applyTuple.arguments[2] == Number(3));
}


TEST_CASE("parser/case", "case expr of")
{
	std::stringstream stream("case 1 of { 1 -> True ; _ -> False }");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	Case& caseExpr = dynamic_cast<Case&>(*maybeExpression);
	REQUIRE(*caseExpr.expression == Number(1));

	REQUIRE(*caseExpr.alternatives[0].pattern == NumberLiteral(1));
	REQUIRE(*caseExpr.alternatives[0].expression == Name("True"));
	REQUIRE(*caseExpr.alternatives[1].pattern == PatternName("_"));
	REQUIRE(*caseExpr.alternatives[1].expression == Name("False"));
}



TEST_CASE("parser/case2", "case expr of")
{
	const char* expr =
"case 1 of\n\
    1 -> True\n\
    _ -> False\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	Case& caseExpr = dynamic_cast<Case&>(*maybeExpression);
	REQUIRE(*caseExpr.expression == Number(1));

	REQUIRE(*caseExpr.alternatives[0].pattern == NumberLiteral(1));
	REQUIRE(*caseExpr.alternatives[0].expression == Name("True"));
	REQUIRE(*caseExpr.alternatives[1].pattern == PatternName("_"));
	REQUIRE(*caseExpr.alternatives[1].expression == Name("False"));
}


TEST_CASE("parser/typedeclaration", "")
{
	const char* expr = "test :: Int";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "test");
	REQUIRE(type.type->toString() == "Int");
}

TEST_CASE("parser/typedeclaration2", "Function")
{
	const char* expr = "double :: Int -> Int";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "double");
	REQUIRE(type.type->toString() == "Int -> Int");
}

TEST_CASE("parser/typedeclaration3", "Function")
{
	const char* expr = "add :: (Int -> Double) -> Int";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "add");
	REQUIRE(type.type->toString() == "(Int -> Double) -> Int");
}

TEST_CASE("parser/typedeclaration4", "add")
{
	const char* expr =
"add :: Int -> Int -> Int\n\
add x y = x + y\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "add");
	REQUIRE(type.type->toString() == "Int -> Int -> Int");
	Binding& bind = module.bindings[0];
	REQUIRE(bind.name == "add");
	Lambda& lambda = dynamic_cast<Lambda&>(*bind.expression);
	REQUIRE(lambda.arguments.size() == 2);
}