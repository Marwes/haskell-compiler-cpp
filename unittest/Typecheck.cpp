#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;

TEST_CASE("typecheck/function1", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), TypeOperator("Int"));
	REQUIRE(type == wanted);
}

TEST_CASE("typecheck/function2", "")
{
	std::stringstream stream("test x y = 2 * x * y\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), functionType(TypeOperator("Int"), TypeOperator("Int")));
	REQUIRE(type == wanted);
}
TEST_CASE("typecheck/function3", "")
{
	std::stringstream stream(
"add x y = x + y\n\
test x = 2 * add 3 x\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), functionType(TypeOperator("Int"), TypeOperator("Int")));
	REQUIRE(type == wanted);
}

TEST_CASE("typecheck/tuple", "")
{
	std::stringstream stream("(1, 2)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	std::vector<Type> args(2);
	args[0] = TypeOperator("Int");
	args[1] = TypeOperator("Int");
	auto wanted = Type(TypeOperator("(,)", args));

	REQUIRE(type == wanted);
}


TEST_CASE("typecheck/typevariable", "")
{
	std::stringstream stream("undefined");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	
	REQUIRE_NOTHROW(boost::get<TypeVariable>(type));
}

TEST_CASE("typecheck/case", "")
{
	std::stringstream stream(
"let\n\
    x = undefined\n\
in case (1, x) of\n\
    (a, b) -> a\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}

TEST_CASE("typecheck/case2", "")
{
	std::stringstream stream(
		"let\n\
		    x = undefined\n\
			in case (1, x) of\n\
			    (a, b) -> b + x\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}

TEST_CASE("typecheck/module", "")
{
	std::stringstream stream(
"main = add2 3 * add2 3\n\
add2 x = x + 2\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	for (auto& bind : module.bindings)
	{
		bind.expression->typecheck(env);
	}

	REQUIRE(module.bindings[0].expression->getType() == Type(TypeOperator("Int")));
	REQUIRE(module.bindings[1].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
}

TEST_CASE("typecheck/module/recursive", "")
{
	std::stringstream stream("fib n = n * fib (n-1)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	for (auto& bind : module.bindings)
	{
		bind.expression->typecheck(env);
	}

	REQUIRE(module.bindings[0].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
}

TEST_CASE("typecheck/module/mutual_recursion", "")
{
	std::stringstream stream(
"test1 x = 3 * test2 x\n\
test2 y = 2 * test1 y\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	for (auto& bind : module.bindings)
	{
		bind.expression->typecheck(env);
	}

	REQUIRE(module.bindings[0].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
	REQUIRE(module.bindings[1].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
}