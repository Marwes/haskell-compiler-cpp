#pragma once
#include <vector>
#include <memory>
#include <assert.h>
#include "Types.h"
#include "Instruction.h"
#include "Stack.h"

namespace MyVMNamespace
{
class VMI;

struct MethodEnvironment
{
    MethodEnvironment(const Assembly* assembly, const StackFrame& frame, FunctionDefinition* function)
        : stackFrame(frame)
		, function(function)
		, assembly(assembly)
    {
    }

    ~MethodEnvironment();

    StackFrame stackFrame;
	FunctionDefinition* function;
	const Assembly* assembly;
};

struct VM
{
    VM();
    ~VM();

    StackObject getValue(size_t index) { return stack[index]; }

    Array<StackObject>& getStack() { return stack; }

    StackFrame newStackFrame();

    void execute(MethodEnvironment& environment);
    void endFrame(MethodEnvironment& environment);

    void printstack();

	Assembly assembly;
private:
    
    Array<StackObject> stack;

    friend class VMI;
};

};