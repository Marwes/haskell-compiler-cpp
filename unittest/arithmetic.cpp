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
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5));
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10));
    assembly.instructions.push_back(Instruction(OP::ADD));
    assembly.instructions.push_back(Instruction(OP::LOAD, 0));
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 20)); 
    assembly.instructions.push_back(Instruction(OP::ADD));

    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 35);
    }

    // 15 10 5
    // [0] = 5 - 10
    assembly.instructions.push_back(Instruction(OP::SUBTRACT));
    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -20);
    }

    // -20
    assembly.instructions.push_back(Instruction(OP::LOAD, 0));
    // -20 -20
    assembly.instructions.push_back(Instruction(OP::MULTIPLY));
    // 400
    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 400);
    }

    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5));
    // 400 5
    assembly.instructions.push_back(Instruction(OP::DIVIDE));
    // 80
    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 80);
    }
    // 80
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 7));
    // 80 7
    assembly.instructions.push_back(Instruction(OP::REMAINDER));
    // 3
    {
        VM vm;
        Method method = Method::main(assembly);
        MethodEnvironment env(vm.newStackFrame(), &method);
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 3);
    }
}