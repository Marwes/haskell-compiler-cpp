#include "Catch/include/catch.hpp"
#include "VM.h"
#include "Method.h"

using namespace MyVMNamespace;

TEST_CASE("field", "test field access")
{
    Assembly assembly;
    assembly.instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10));
    VMInt size = sizeof(StackObject) * 2;
    assembly.instructions.push_back(Instruction(OP::NEWOBJECT, size));
    assembly.instructions.push_back(Instruction(OP::SETFIELD, 0, 0, 2)); // field[0] = 10
    assembly.instructions.push_back(Instruction(OP::GETFIELD, 0, 2, 2));
    Slice<Instruction> methodInstructions(assembly.instructions.data(), assembly.instructions.size());
    {
        VM vm;
        std::vector<RefCountedPointer> fields;
        fields.push_back(RefCountedPointer(new VMField(Type(TYPE_INT), 0)));
        Method method(methodInstructions, std::vector<Type>(), std::move(fields));
        MethodEnvironment env(vm.newStackFrame(), &method);
        
        vm.execute(env);
        REQUIRE(vm.getValue(2).intValue == 10); // 10 obj 10
    }
}