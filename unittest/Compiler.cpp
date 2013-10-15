#include "Catch/include/catch.hpp"
#include <VM.h>
#include <string>
#include <fstream>
#include <vector>
#include "Compiler.h"
#include "Util.h"
#include "Method.h"
#include "Array.h"
#include "TestUtility.h"

using namespace MyVMNamespace;

std::unique_ptr<VM> evaluateInt(const char* expr)
{
	std::stringstream input(expr);
	Compiler compiler(input);
	Assembly assembly = compiler.compile();

	std::unique_ptr<VM> vm = make_unique<VM>();
	Method method = Method::main(assembly);
	vm->assembly = std::move(assembly);
	MethodEnvironment env(vm->newStackFrame(), &method);
	vm->execute(env);
	return std::move(vm);
}

TEST_CASE("compiler/arithmetic", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("3+2")->getValue(0).intValue == 5);
	REQUIRE(evaluateInt("2 * 4 + 3")->getValue(0).intValue == 11);
	REQUIRE(evaluateInt("let three = 3 in 2 * 4 + three")->getValue(1).intValue == 11);
	REQUIRE(evaluateInt("let six = 3 * 2 in 2 * 4 + six")->getValue(1).intValue == 14);
	REQUIRE(evaluateInt("let six = 3 * 2; four = six - 2 in 2 * four + six")->getValue(2).intValue == 14);
	REQUIRE(evaluateInt("let f x = x * x in f 3")->getValue(0).intValue == 9);
	REQUIRE(evaluateInt("let f x y = x * x + y; five = 5 in f 3 five")->getValue(1).intValue == 14);
	REQUIRE(evaluateInt("let f x = x * x in f 3 + f 2")->getValue(0).intValue == 13);
}

TEST_CASE("compiler/compare", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("3==2")->getValue(0).intValue == 0);
	REQUIRE(evaluateInt("3<=2")->getValue(0).intValue == 0);
	REQUIRE(evaluateInt("3 > 2")->getValue(0).intValue == 1);
	REQUIRE(evaluateInt("3>=2")->getValue(0).intValue == 1);
	REQUIRE(evaluateInt("3 < 2")->getValue(0).intValue == 0);
	REQUIRE(evaluateInt("let one = 1 in one /= 2")->getValue(0).intValue == 1);
}


TEST_CASE("compiler/let", "")
{
	const char* expr =
"let one = 1\n\
    double x = 2*x\n\
in double 3 + 1";
	REQUIRE(evaluateInt(expr)->getValue(1).intValue == 7);
}

TEST_CASE("compiler/case", "")
{
	const char* expr =
"case 1 of\n\
    1 -> 10\n\
    _ -> 2\n";
	REQUIRE(evaluateInt(expr)->getValue(0).intValue == 10);
}

TEST_CASE("compiler/case2", "")
{
	const char* expr =
"let f x = 2 * x in case 1 of\n\
    1 -> f 10\n\
    _ -> 2\n";
	REQUIRE(evaluateInt(expr)->getValue(0).intValue == 20);
}

TEST_CASE("compiler/case3", "")
{
	const char* expr =
"let f x = 2 * x in case 2 of\n\
    1 -> f 10\n\
    _ -> f 2\n";
	REQUIRE(evaluateInt(expr)->getValue(0).intValue == 4);
}

TEST_CASE("compiler/case4", "")
{
	const char* expr =
"let f x = 2 * x in case f 2 of\n\
    4 -> 7\n\
    _ -> f 2\n";
	REQUIRE(evaluateInt(expr)->getValue(0).intValue == 7);
}

TEST_CASE("compiler/case5", "")
{
	const char* expr =
"case 2 + 3 of\n\
    4 -> 7\n\
    _ -> 1000\n";
	REQUIRE(evaluateInt(expr)->getValue(0).intValue == 1000);
}