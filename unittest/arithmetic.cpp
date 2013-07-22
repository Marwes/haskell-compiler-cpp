#include "Catch/include/catch.hpp"
#include <VM.h>
#include <string>
#include <fstream>
#include <vector>
#include "Util.h"
#include "Method.h"

using namespace MyVMNamespace;


bool compareInstructions(const Instruction& lhs, const Instruction& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(lhs)) == 0;
}

TEST_CASE("arithmetic", "test arithmetic")
{
    std::vector<Instruction> instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 0));
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5)); 
    instructions.push_back(Instruction(OP::ADD, 0, 1, 2));

    std::vector<Instruction> fileInstructions(readAssemblyFile("Arithmetic.asm"));
    REQUIRE(std::equal(instructions.begin(), instructions.end(), fileInstructions.begin(), compareInstructions));
    instructions = fileInstructions;
    {
        VM vm;
        const Method method(instructions, std::vector<std::unique_ptr<Data>>(), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 10);
        REQUIRE(vm.getValue(2).intValue == 5);
    }

    // 15 10 5
    // [0] = 5 - 10
    instructions.push_back(Instruction(OP::SUBTRACT, 0, 2, 1));
    {
        VM vm;
        const Method method(instructions, std::vector<std::unique_ptr<Data>>(), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -5);
    }

    // -5 10 5
    // [0] = -5 * 10
    instructions.push_back(Instruction(OP::MULTIPLY, 0, 0, 1));
    {
        VM vm;
        const Method method(instructions, std::vector<std::unique_ptr<Data>>(), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -50);
    }
    // -50 10 5
    // [0] = 10 / 5
    instructions.push_back(Instruction(OP::DIVIDE, 0, 1, 2));
    {
        VM vm;
        const Method method(instructions, std::vector<std::unique_ptr<Data>>(), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 2);
    }
    // 2 10 5
    // [0] = 5 % 2
    instructions.push_back(Instruction(OP::REMAINDER, 0, 2, 0));
    {
        VM vm;
        const Method method(instructions, std::vector<std::unique_ptr<Data>>(), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 1);
    }
}