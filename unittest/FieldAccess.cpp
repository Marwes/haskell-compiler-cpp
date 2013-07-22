#include "Catch/include/catch.hpp"
#include "VM.h"
#include "Method.h"

using namespace MyVMNamespace;

TEST_CASE("field", "test field access")
{
    std::vector<Instruction> instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10));
    VMInt size = sizeof(StackObject) * 2;
    instructions.push_back(Instruction(OP::NEWOBJECT, size));
    instructions.push_back(Instruction(OP::SETFIELD, 0, 0, 2)); // field[0] = 10
    instructions.push_back(Instruction(OP::GETFIELD, 0, 2, 2));
    {
        VM vm;
        std::vector<std::unique_ptr<Data>> fields;
        fields.push_back(std::unique_ptr<Data>(new VMField(Type(TYPE_INT), 0)));
        const Method method(instructions, std::move(fields), std::vector<Type>());
        MethodEnvironment env(vm.newStackFrame(), &method);
        
        vm.execute(env);
        REQUIRE(vm.getValue(2).intValue == 10); // 10 obj 10
    }
}