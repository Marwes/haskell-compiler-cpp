#include "Catch/include/catch.hpp"
#include <VM.h>
#include <string>
#include <fstream>
#include <vector>
#include "Util.h"
#include "Method.h"
#include "Array.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{

bool operator==(const Instruction& lhs, const Instruction& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(lhs)) == 0;
}

bool operator==(const Assembly& lhs, const Assembly& rhs)
{
    return lhs.entrypoint == rhs.entrypoint && lhs.instructions == rhs.instructions;
}

}

TEST_CASE("arithmetic", "test arithmetic")
{
    Assembly assembly;
    assembly.entrypoint = 1;
    assembly.instructions.push_back(Instruction(OP::ADD, 0, 1, 2));
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 0));
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10)); 
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5)); 
    assembly.instructions.push_back(Instruction(OP::ADD, 0, 1, 2));

    Assembly fileAssembly(readAssemblyFile("Arithmetic.asm"));
    REQUIRE(assembly == fileAssembly);
    Slice<Instruction> methodInstructions(assembly.instructions.data() + assembly.entrypoint, assembly.instructions.size() - assembly.entrypoint);
    assembly = fileAssembly;
    {
        VM vm;
        const Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 10);
        REQUIRE(vm.getValue(2).intValue == 5);
    }

    // 15 10 5
    // [0] = 5 - 10
    assembly.instructions.push_back(Instruction(OP::SUBTRACT, 0, 2, 1));
    {
        VM vm;
        const Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -5);
    }

    // -5 10 5
    // [0] = -5 * 10
    assembly.instructions.push_back(Instruction(OP::MULTIPLY, 0, 0, 1));
    {
        VM vm;
        const Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -50);
    }
    // -50 10 5
    // [0] = 10 / 5
    assembly.instructions.push_back(Instruction(OP::DIVIDE, 0, 1, 2));
    {
        VM vm;
        const Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 2);
    }
    // 2 10 5
    // [0] = 5 % 2
    assembly.instructions.push_back(Instruction(OP::REMAINDER, 0, 2, 0));
    {
        VM vm;
        const Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 1);
    }
}