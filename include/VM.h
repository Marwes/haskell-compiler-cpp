#pragma once
#include <vector>
#include <memory>
#include <assert.h>
#include "Types.h"
#include "Instruction.h"
#include "Stack.h"

namespace MyVMNamespace
{

class Method;
class VMI;

struct MethodEnvironment
{
    MethodEnvironment(const StackFrame& frame, const Method* method)
        : stackFrame(frame)
        , method(method)
    {
    }

    StackFrame stackFrame;
    const Method* method;
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
private:
    
    Array<StackObject> stack;

    friend class VMI;
};

};