#include "Catch/include/catch.hpp"
#include <VM.h>

using namespace MyVMNamespace;

TEST_CASE("arithmetic", "test arithmetic")
{
    std::vector<Instruction> instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 0)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5)); 
    instructions.push_back(Instruction(OP::ADD, 0, 1, 2));
    {
        VM vm(instructions);
        RuntimeEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 10);
        REQUIRE(vm.getValue(2).intValue == 5);
    }

    // 15 10 5
    // [0] = 5 - 10
    instructions.push_back(Instruction(OP::SUBTRACT, 0, 2, 1));
    {
        VM vm(instructions);
        RuntimeEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -5);
    }

    // -5 10 5
    // [0] = -5 * 10
    instructions.push_back(Instruction(OP::MULTIPLY, 0, 0, 1));
    {
        VM vm(instructions);
        RuntimeEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -50);
    }
    // -50 10 5
    // [0] = 10 / 5
    instructions.push_back(Instruction(OP::DIVIDE, 0, 1, 2));
    {
        VM vm(instructions);
        RuntimeEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 2);
    }
    // 2 10 5
    // [0] = 5 % 2
    instructions.push_back(Instruction(OP::REMAINDER, 0, 2, 0));
    {
        VM vm(instructions);
        RuntimeEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 1);
    }
}