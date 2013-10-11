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