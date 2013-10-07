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

TEST_CASE("compiler/arithmetic", "Test compiling an arithmetic expression")
{
    std::stringstream input("3+2");
    Compiler compiler(input);
    Assembly assembly = compiler.compile();

    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 5);
    }
}


TEST_CASE("compiler/arithmetic2", "Test compiling an arithmetic expression")
{
    std::stringstream input("2 * 4 + 3");
    Compiler compiler(input);
    Assembly assembly = compiler.compile();

    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 11);
    }
}



TEST_CASE("compiler/bind/arithmetic", "Test compiling an arithmetic expression")
{
	std::stringstream input("let three = 3 in 2 * 4 + three");
	Compiler compiler(input);
	Assembly assembly = compiler.compile();

	{
		VM vm;
		Method method = Method::main(assembly);
		MethodEnvironment env(vm.newStackFrame(), &method);
		vm.execute(env);
		REQUIRE(vm.getValue(1).intValue == 11);
	}
}