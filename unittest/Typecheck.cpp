#include "Catch/include/catch.hpp"
#include "TestUtil.h"
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

TEST_CASE("typecheck/letrec", "")
{
	std::stringstream stream(
"let\n\
    t = h 2\n\
    f x = x + g x\n\
    g y = y * f y\n\
    h z = g z + 2\n\
in f 2\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}
TEST_CASE("typecheck/letrec/error", "")
{
	std::stringstream stream(
"let\n\
    g y = f y y\n\
    f x = 10\n\
in g 2\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto expr = parser.expression();
	TypeEnvironment env(nullptr);

	REQUIRE_THROWS(expr->typecheck(env));
}

TEST_CASE("typecheck/module/mutual_recursion", "")
{
	std::stringstream stream(
"test1 x = 3 * test2 x\n\
test2 y = 2 * test1 y\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	Type x = functionType(TypeVariable(), TypeOperator("Int"));
	REQUIRE(sameTypes(module.bindings[0].expression->getType(), x));
	REQUIRE(sameTypes(module.bindings[1].expression->getType(), x));
}

TEST_CASE("typecheck/nested_let", "")
{
	std::stringstream stream(
"test x =\n\
    let\n\
        first = x + x\n\
    in let\n\
        second = [first, x]\n\
       in second\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> args(1);
	args[0] = TypeOperator("Int");
	Type listType = TypeOperator("[]", args);
	Type x = functionType(TypeOperator("Int"), listType);
	REQUIRE(sameTypes(module.bindings[0].expression->getType(), x));

	Lambda& lambda = dynamic_cast<Lambda&>(*module.bindings[0].expression);
	Let& let = dynamic_cast<Let&>(*lambda.body);
	REQUIRE(let.bindings[0].expression->getType() == Type(TypeOperator("Int")));
	Let& innerLet = dynamic_cast<Let&>(*let.expression);
	REQUIRE(innerLet.bindings[0].expression->getType() == listType);
}

TEST_CASE("typecheck/list", "")
{
	std::stringstream stream(
"head :: [a] -> a\n\
head xs = case xs of\n\
    : y ys -> y\n\
tail xs = case xs of\n\
    : y ys -> ys\n\
main = head (tail [10, 20, 30])\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> args(1);
	args[0] = TypeVariable();
	Type listType = TypeOperator("[]", args);
	Type headType = functionType(listType, args[0]);
	Type tailType = functionType(listType, listType);
	
	CHECK(module.bindings[0].expression->getType() < headType);
	CHECK(module.bindings[1].expression->getType() < tailType);
	CHECK(module.bindings[2].expression->getType() < Type(TypeOperator("Int")));
}

TEST_CASE("typecheck/Maybe", "")
{
	std::stringstream stream(
"data Maybe a = Just a | Nothing\n\
test1 = Just 3\n\
test2 = Just undefined\n\
test3 = Nothing\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> intArgs(1);
	intArgs[0] = TypeOperator("Int");
	Type intMaybe = TypeOperator("Maybe", intArgs);
	std::vector<Type> args(1);
	args[0] = TypeVariable();
	Type genericMaybe = TypeOperator("Maybe", args);

	CHECK(sameTypes(module.bindings[0].expression->getType(), intMaybe));
	CHECK(sameTypes(module.bindings[1].expression->getType(), genericMaybe));
	CHECK(sameTypes(module.bindings[2].expression->getType(), genericMaybe));
}

TEST_CASE("typecheck/pair", "Check that it is possible to have multiple pairs with differing types")
{
	std::stringstream stream(
"test1 = (undefined, undefined)\n\
test2 = (1, 2)\n\
test3 = (undefined, 3)\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> args1(2);
	args1[0] = TypeVariable();
	args1[1] = TypeVariable();
	Type test1 = TypeOperator("(,)", args1);

	std::vector<Type> args2(2);
	args2[0] = TypeOperator("Int");
	args2[1] = TypeOperator("Int");
	Type test2 = TypeOperator("(,)", args2);

	std::vector<Type> args3(2);
	args3[0] = TypeVariable();
	args3[1] = TypeOperator("Int");
	Type test3 = TypeOperator("(,)", args3);

	CHECK(module.bindings[0].expression->getType() < test1);
	CHECK(module.bindings[1].expression->getType() < test2);
	CHECK(module.bindings[2].expression->getType() < test3);
}

TEST_CASE("typecheck/error_recursive", "Check that type errors are also thrown for using bindings declared later")
{
	std::stringstream stream(
"main = primError 2 3\n\
primError x = x");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE_THROWS_AS(module.typecheck(), TypeError);
}

TEST_CASE("typecheck/error", "")
{
	std::stringstream stream(
"test x = case x of\n\
    (a, b) -> a + b\n\
main = test 3\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE_THROWS(module.typecheck());
}

TEST_CASE("typecheck/class", "")
{
	std::stringstream stream(
"data Bool = True | False\n\
class Eq a where\n\
    (===) :: a -> a -> Bool\n\
instance Eq Int where\n\
    (===) = primIntEq\n\
main = 2 === 3");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	module.typecheck();

	REQUIRE(module.bindings[0].expression->getType() == Type(TypeOperator("Bool")));

	TypeVariable& var = module.classes[0].variable;
	Binding& eqInt = module.instances[0].bindings[0];
	REQUIRE(sameTypes(eqInt.expression->getType(), Type(functionType(var, functionType(var, TypeOperator("Bool"))))));
}

TEST_CASE("typecheck/class/error", "")
{
	std::stringstream stream(
"data Bool = True | False\n\
primError = True\n\
class Eq a where\n\
    (===) :: a -> a -> Bool\n\
instance Eq Int where\n\
    (===) = primError\n\
main = 2 === 3");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE_THROWS_AS(module.typecheck(), TypeError);
	std::cerr << "\n" << module.bindings[0].expression->getType() << std::endl;
	std::cerr << module.bindings[1].expression->getType() << std::endl;
	std::cerr << module.instances[0].bindings[0].expression->getType() << std::endl;
}