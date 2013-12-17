#include "Catch/include/catch.hpp"
#include "TestUtil.h"
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

	Apply* func = dynamic_cast<Apply*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (*func->arguments[0] == Number(3));
    REQUIRE (*func->arguments[1]== Number(2));
    
}

TEST_CASE("parser/3", "3 + 2 + 4")
{
    std::stringstream stream("3 + 2 + 4");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

	Apply* func = dynamic_cast<Apply*>(maybeExpression.get());
    REQUIRE (func != NULL);
    REQUIRE (*func->arguments[1] == Number(4));

	const Apply* first = dynamic_cast<const Apply*>(func->arguments[0].get());
    REQUIRE (first != NULL);
    REQUIRE (*first->arguments[0] == Number(3));
    REQUIRE (*first->arguments[1] == Number(2));
}


TEST_CASE("parser/3 + (2 + 4)", "3 + ( 2 + 4 )")
{
    std::stringstream stream("3 + ( 2 + 4 )");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

    auto maybeExpression = parser.run();
    REQUIRE (maybeExpression.get() != NULL);

	Apply* func = dynamic_cast<Apply*>(maybeExpression.get());
    REQUIRE (func != NULL);
	REQUIRE (*func->function == Name("+"));
    REQUIRE (*func->arguments[0] == Number(3));

	const Apply* second = dynamic_cast<const Apply*>(func->arguments[1].get());
    REQUIRE (second != NULL);
	REQUIRE (*second->function == Name("+"));
    REQUIRE (*second->arguments[0] == Number(2));
    REQUIRE (*second->arguments[1] == Number(4));
}



TEST_CASE("parser/three * 2 + 4", "three * 2 + 4")
{
	std::stringstream stream("three * 2 + 4");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);

	Apply* func = dynamic_cast<Apply*>(maybeExpression.get());
	REQUIRE(func != NULL);
	REQUIRE(*func->function == Name("+"));
	REQUIRE(*func->arguments[1] == Number(4));

	const Apply* second = dynamic_cast<const Apply*>(func->arguments[0].get());
	REQUIRE(second != NULL);
	REQUIRE(*second->function == Name("*"));
	REQUIRE(*second->arguments[0] == Name("three"));
	REQUIRE(*second->arguments[1] == Number(2));
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
	Apply* op = dynamic_cast<Apply*>(let->expression.get());
	REQUIRE(*op->function == Name("+"));
	REQUIRE(*op->arguments[0] == Name("three"));
	REQUIRE(*op->arguments[1] == Number(4));
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

	Apply* letPrimOP = dynamic_cast<Apply*>(let->bindings[0].expression.get());
	REQUIRE(letPrimOP != NULL);
	REQUIRE(*letPrimOP->arguments[0] == Number(3));
	REQUIRE(*letPrimOP->arguments[1] == Number(2));

	Apply* op = dynamic_cast<Apply*>(let->expression.get());
	REQUIRE(*op->function == Name("+"));
	REQUIRE(*op->arguments[0] == Name("three"));
	REQUIRE(*op->arguments[1] == Number(4));
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

	Apply& op = dynamic_cast<Apply&>(*apply.arguments[1]);
	REQUIRE(*op.arguments[0] == Number(3));
	REQUIRE(*op.arguments[1] == Name("two"));
}

TEST_CASE("parser/applyOperator", "Function application")
{
	std::stringstream stream("f 3 one * two");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto maybeExpression = parser.run();
	REQUIRE(maybeExpression.get() != NULL);


	Apply& op = dynamic_cast<Apply&>(*maybeExpression);

	Apply& apply = dynamic_cast<Apply&>(*op.arguments[0]);
	REQUIRE(*apply.function == Name("f"));
	REQUIRE(*apply.arguments[0] == Number(3));
	REQUIRE(*apply.arguments[1] == Name("one"));

	REQUIRE(*op.arguments[1] == Name("two"));
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
	const char* expr = "test :: Int\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "test");
	REQUIRE(type.type == Type(TypeOperator("Int")));
}

TEST_CASE("parser/typedeclaration2", "Function")
{
	const char* expr = "double :: Int -> Int\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "double");
	REQUIRE(type.type == functionType(TypeOperator("Int"), TypeOperator("Int")));
}

TEST_CASE("parser/typedeclaration3", "Function")
{
	const char* expr = "add :: (Int -> Double) -> Int\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "add");
	REQUIRE(type.type == functionType(functionType(TypeOperator("Int"), TypeOperator("Double")), TypeOperator("Int")));
}

TEST_CASE("parser/constraints", "")
{
	const char* expr = "add :: Num a => a -> a -> a\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.constraints.size() == 1);
	TypeOperator& op = type.constraints[0];
	REQUIRE(op.name == "Num");
	REQUIRE(type.name == "add");
	REQUIRE(type.type == functionType(op.types[0], functionType(op.types[0], op.types[0])));
}

TEST_CASE("parser/constraints/multiple", "")
{
	const char* expr = "add :: (Eq a, Num a) => a -> a -> a\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.constraints.size() == 2);
	TypeOperator& opEq = type.constraints[0];
	REQUIRE(opEq.name == "Eq");
	TypeOperator& opNum = type.constraints[1];
	REQUIRE(opNum.name == "Num");
	REQUIRE(opEq.types == opNum.types);

	REQUIRE(type.name == "add");
	REQUIRE(type.type == functionType(opEq.types[0], functionType(opEq.types[0], opEq.types[0])));
}

TEST_CASE("parser/typedeclaration4", "add")
{
	const char* expr =
"add :: Int -> Int -> Int\n\
add x y = x + y\n";
	std::stringstream stream(expr);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeDeclaration& type = module.typeDeclaration[0];
	REQUIRE(type.name == "add");
	//REQUIRE(type.type->toString() == "Int -> Int -> Int");//TODO
	Binding& bind = module.bindings[0];
	REQUIRE(bind.name == "add");
	Lambda& lambda = dynamic_cast<Lambda&>(*bind.expression);
	REQUIRE(lambda.arguments.size() == 2);
}

TEST_CASE("parser/list/nil", "")
{
	const char* str = "[]";
	std::stringstream stream(str);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	REQUIRE(*expr == Name("[]"));
}

TEST_CASE("parser/list/cons1", "")
{
	const char* str = "[1]";
	std::stringstream stream(str);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();

	Apply& apply = dynamic_cast<Apply&>(*expr);
	REQUIRE(*apply.function == Name(":"));
	REQUIRE(*apply.arguments[0] == Number(1));
	REQUIRE(*apply.arguments[1] == Name("[]"));
}

TEST_CASE("parser/list/cons2", "")
{
	const char* str = "[1,2,3";
	std::stringstream stream(str);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();

	Apply& apply = dynamic_cast<Apply&>(*expr);
	REQUIRE(*apply.function == Name(":"));
	REQUIRE(*apply.arguments[0] == Number(1));
	Apply& apply2 = dynamic_cast<Apply&>(*apply.arguments[1]);
	REQUIRE(*apply2.function == Name(":"));
	REQUIRE(*apply2.arguments[0] == Number(2));
	Apply& apply3 = dynamic_cast<Apply&>(*apply2.arguments[1]);
	REQUIRE(*apply3.function == Name(":"));
	REQUIRE(*apply3.arguments[0] == Number(3));
}