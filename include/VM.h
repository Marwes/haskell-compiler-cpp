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
    MethodEnvironment(const StackFrame& frame, const std::vector<VMField>& fieldData = std::vector<VMField>())
        : stackFrame(frame)
        , fieldData(fieldData)
    {
    }

    const std::vector<VMField> fieldData;

    StackLayout layout;
    StackFrame stackFrame;
};

struct VM
{
    VM(std::vector<const Instruction>& instructions);
    ~VM();

    StackObject getValue(size_t index) { return stack[index]; }

    Array<StackObject>& getStack() { return stack; }

    StackFrame newStackFrame();

    void execute(MethodEnvironment& environment);
    void endFrame(MethodEnvironment& environment);

    void printstack();
private:
    VMInt currentInstruction;
    std::vector<const Instruction> instructions;
    
    Array<StackObject> stack;

    friend class VMI;
};

};