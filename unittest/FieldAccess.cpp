#include "Catch/include/catch.hpp"
#include "VM.h"
#include "Method.h"

using namespace MyVMNamespace;

TEST_CASE("field", "test field access")
{
    Assembly assembly;
	assembly.addFunction("main", make_unique<FunctionDefinition>());
	FunctionDefinition* function = assembly.getFunction("main");
	std::vector<Instruction>& instructions = function->instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10));
    VMInt size = sizeof(StackObject) * 2;
    instructions.push_back(Instruction(OP::NEWOBJECT, size));
    instructions.push_back(Instruction(OP::SETFIELD, 0, 0, 2)); // field[0] = 10
    instructions.push_back(Instruction(OP::GETFIELD, 0, 2, 2));
    Slice<Instruction> methodInstructions(instructions.data(), instructions.size());
    {
        VM vm;
        std::vector<RefCountedPointer> fields;
        fields.push_back(RefCountedPointer(new VMField(Type(TYPE_INT), 0)));
        MethodEnvironment env(&vm.assembly, vm.newStackFrame(), function);
        
        vm.execute(env);
        REQUIRE(vm.getValue(2).intValue == 10); // 10 obj 10
    }
}