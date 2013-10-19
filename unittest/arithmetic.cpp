#include "Catch/include/catch.hpp"
#include <VM.h>
#include <string>
#include <fstream>
#include <vector>
#include "Util.h"
#include "Method.h"
#include "Array.h"
#include "TestUtility.h"

using namespace MyVMNamespace;

TEST_CASE("arithmetic", "test arithmetic")
{
    Assembly assembly;
    assembly.entrypoint = 0;
	assembly.addFunction("main", make_unique<FunctionDefinition>());
	FunctionDefinition& def = *assembly.getFunction("main");
    def.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5));
    def.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10));
    def.instructions.push_back(Instruction(OP::ADD_INT));
    def.instructions.push_back(Instruction(OP::LOAD, 0));
    def.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 20)); 
	def.instructions.push_back(Instruction(OP::ADD_INT));

    {
        VM vm;
        MethodEnvironment env(&vm.assembly, vm.newStackFrame(), &def);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 35);
    }

    // 15 10 5
    // [0] = 5 - 10
    def.instructions.push_back(Instruction(OP::SUBTRACT_INT));
    {
		VM vm;
		MethodEnvironment env(&vm.assembly, vm.newStackFrame(), &def);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -20);
    }

    // -20
    def.instructions.push_back(Instruction(OP::LOAD, 0));
    // -20 -20
    def.instructions.push_back(Instruction(OP::MULTIPLY_INT));
    // 400
    {
		VM vm;
		MethodEnvironment env(&vm.assembly, vm.newStackFrame(), &def);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 400);
    }

    def.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5));
    // 400 5
	def.instructions.push_back(Instruction(OP::DIVIDE_INT));
    // 80
    {
		VM vm;
		MethodEnvironment env(&vm.assembly, vm.newStackFrame(), &def);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 80);
    }
    // 80
    def.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 7));
    // 80 7
	def.instructions.push_back(Instruction(OP::REMAINDER_INT));
    // 3
    {
		VM vm;
		MethodEnvironment env(&vm.assembly, vm.newStackFrame(), &def);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 3);
    }
}