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
	std::cerr << type << std::endl;

	std::vector<Type> args(2);
	args[0] = TypeOperator("Int");
	args[1] = TypeOperator("Int");
	auto wanted = Type(TypeOperator("(,)", args));
	std::cerr << wanted << std::endl;
	REQUIRE(type == wanted);
}