#include "Catch/include/catch.hpp"
#include <string>
#include <fstream>
#include <vector>
#include "Util.h"
#include "Array.h"
#include "GMachine.h"

using namespace MyVMNamespace;

int evaluateInt(const char* str)
{
	GMachine machine;
	std::string main("main = ");
	main += str;
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	return result.getNode()->number;
}

TEST_CASE("compiler/arithmetic", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("3+2") == 5);
	REQUIRE(evaluateInt("2 * 4 + 3") == 11);
	REQUIRE(evaluateInt("let three = 3 in 2 * 4 + three") == 11);
	REQUIRE(evaluateInt("let six = 3 * 2 in 2 * 4 + six") == 14);
	REQUIRE(evaluateInt("let six = 3 * 2; four = six - 2 in 2 * four + six") == 14);
	REQUIRE(evaluateInt("let f x = x * x in f 3") == 9);
	REQUIRE(evaluateInt("let f x y = x * x + y; five = 5 in f 3 five") == 14);
	REQUIRE(evaluateInt("let f x = x * x in f 3 + f 2") == 13);
}

TEST_CASE("compiler/compare", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("3==2") == 0);
	REQUIRE(evaluateInt("3<=2") == 0);
	REQUIRE(evaluateInt("3 > 2") == 1);
	REQUIRE(evaluateInt("3>=2") == 1);
	REQUIRE(evaluateInt("3 < 2") == 0);
	REQUIRE(evaluateInt("let one = 1 in one /= 2") == 1);
}


TEST_CASE("compiler/data", "")
{
	GMachine machine;
	const char* main =
"data Vec2 = Vec2 Int Int\n\
main = Vec2 1 2\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == CONSTRUCTOR);
	ConstructorNode ctor = result.getNode()->constructor;
	REQUIRE(ctor.arguments[0].getType() == NUMBER);
	REQUIRE(ctor.arguments[0].getNode()->number == 1);
	REQUIRE(ctor.arguments[1].getType() == NUMBER);
	REQUIRE(ctor.arguments[1].getNode()->number == 2);
}

#if 0
TEST_CASE("compiler/let", "")
{
	const char* expr =
"\n\
    let\n\
        one = 1\n\
        double x = 2*x\n\
    in double 3 + 1";
	REQUIRE(evaluateInt(expr) == 7);
}

TEST_CASE("compiler/case", "")
{
	const char* expr =
"case 1 of\n\
    1 -> 10\n\
    _ -> 2\n";
	REQUIRE(evaluateInt(expr) == 10);
}

TEST_CASE("compiler/case2", "")
{
	const char* expr =
"let f x = 2 * x in case 1 of\n\
    1 -> f 10\n\
    _ -> 2\n";
	REQUIRE(evaluateInt(expr) == 20);
}

TEST_CASE("compiler/case3", "")
{
	const char* expr =
"let f x = 2 * x in case 2 of\n\
    1 -> f 10\n\
    _ -> f 2\n";
	REQUIRE(evaluateInt(expr) == 4);
}

TEST_CASE("compiler/case4", "")
{
	const char* expr =
"let f x = 2 * x in case f 2 of\n\
    4 -> 7\n\
    _ -> f 2\n";
	REQUIRE(evaluateInt(expr) == 7);
}

TEST_CASE("compiler/case5", "")
{
	const char* expr =
"case 2 + 3 of\n\
    4 -> 7\n\
    _ -> 1000\n";
	REQUIRE(evaluateInt(expr) == 1000);
}
#endif

template<class T>
T runExpr(const std::string& str)
{
	GMachine machine;
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	return result.getNode()->number;
}

#if 0
TEST_CASE("compiler/tuple", "")
{
	const char* expr = "(1,2)";
	std::stringstream input(expr);
	Evaluator compiler(input);
	std::unique_ptr<VM> vm = make_unique<VM>();
	vm->assembly = compiler.compile();

	FunctionDefinition* def = vm->assembly.getFunction("main");
	MethodEnvironment env(&vm->assembly, vm->newStackFrame(), def);
	vm->execute(env);
	Object* obj = vm->getValue(2).pointerValue;
	REQUIRE(obj != NULL);
	REQUIRE(obj->getField(0).intValue == 1);
	REQUIRE(obj->getField(1).intValue == 2);
}
#endif

#if 0


TEST_CASE("compiler/module/1", "")
{
	std::stringstream stream(
"add :: Int -> Int -> Int\n\
add x y = x + y\n");
	Compiler compiler(stream);

	Assembly assembly = compiler.compile();

	std::unique_ptr<VM> vm = make_unique<VM>();
	vm->assembly = std::move(assembly);
	REQUIRE(runExpr<VMInt>(*vm, "add 2 3") == (2 + 3));
}

TEST_CASE("compiler/module/2", "Compile invalid type in primop return")
{
	std::stringstream stream(
"add :: Int -> Int -> String\n\
add x y = x + y\n");
	Compiler compiler(stream);

	REQUIRE_THROWS_AS(compiler.compile(), TypeError);
}


TEST_CASE("compiler/module/3", "Compile invalid type in primop argument")
{
	std::stringstream stream(
"add :: Int -> String -> Int\n\
add x y = x + y\n");
	Compiler compiler(stream);

	REQUIRE_THROWS_AS(compiler.compile(), TypeError);
}

TEST_CASE("compiler/module/divdeDoubles", "Test dividing doubles")
{
	std::stringstream stream(
"divide :: Double -> Double -> Double\n\
divide x y = x / y\n");
	Compiler compiler(stream);
	Assembly assembly = compiler.compile();

	std::unique_ptr<VM> vm = make_unique<VM>();
	vm->assembly = std::move(assembly);
	REQUIRE(runExpr<VMFloat>(*vm, "divide 3 2") == 3. / 2);
}

TEST_CASE("compiler/module/divideTuple", "Test dividing doubles")
{
	std::stringstream stream(
"divide :: (Double, Double) -> Double\n\
divide z = case z of\n\
    (x, y) -> x / y\n");
	Compiler compiler(stream);
	Assembly assembly = compiler.compile();

	std::unique_ptr<VM> vm = make_unique<VM>();
	vm->assembly = std::move(assembly);
	REQUIRE(runExpr<VMFloat>(*vm, "divide (3, 2)") == 3. / 2);
}

#endif