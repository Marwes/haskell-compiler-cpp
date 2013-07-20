#include <assert.h>
#include <iostream>
#include "VM.h"

namespace MyVMNamespace
{

#define OP_CLASS(op, a0, a1, a2) \
class OP_##op \
{\
    OP_##op(Instruction& i) : value(i) {}\
    VMInt a0() { return value.arg0;}\
    VMInt a1() { return value.arg1;}\
    VMInt a2() { return value.arg2;}\
    const Instruction& value;\
};

OP_CLASS(MOVE, from, to, na);

class VMI
{
public:
    static void op_move(RuntimeEnvironment& environment, Instruction current)
    {
        environment.stackFrame.at<VMInt>(current.arg0) = environment.stackFrame.at<VMInt>(current.arg1);
    }

    static void op_load(VM& vm, Instruction current)
    {
    }

    static void op_load_int_constant(RuntimeEnvironment& environment, Instruction& current)
    {
        environment.stackFrame.push(current.arg0);
    }

    static void op_getfield(RuntimeEnvironment& environment, Instruction current)
    {
        unsigned char* obj = environment.stackFrame.top<unsigned char*>();
        unsigned char index = environment.stackFrame.at<unsigned char>(current.arg1);
        unsigned char resultStackIndex = environment.stackFrame.at<unsigned char>(current.arg2);

        const VMField& field = environment.data[index];
        //environment.stackFrame.at(resultStackIndex) = obj + field.offset;
    }
    
    static void op_setfield(RuntimeEnvironment& environment, Instruction current)
    {
        unsigned char* obj = environment.stackFrame.at<unsigned char*>(current.arg1);
        unsigned char offset = environment.stackFrame.at<unsigned char>(current.arg2);
        
        environment.stackFrame.at<unsigned char*>(current.arg0) = obj + offset;
    }

};

#define DO_ARITH(op, environment, instruction) {\
    VMInt lhs = environment.stackFrame.at<VMInt>(instruction.arg1);\
    VMInt rhs = environment.stackFrame.at<VMInt>(instruction.arg2);\
    environment.stackFrame.at<VMInt>(instruction.arg0) = lhs op rhs; }

typedef void (*execute_function_t)(VM& vm, Instruction current); 


VM::VM(std::vector<Instruction>& instructions)
    : instructions(instructions)
    , currentInstruction(0)
{
}

StackFrame VM::newStackFrame()
{
    return StackFrame(this->stack.data(), 0);
}

void printValue(StackObject obj)
{
    std::cout << obj.intValue << std::endl;
}

void VM::printstack()
{
    for (size_t ii = 0; ii < this->stack.size(); ++ii)
    {
        printValue(this->stack[ii]);
    }
}

void VM::execute(RuntimeEnvironment& environment)
{
    while (currentInstruction < this->instructions.size())
    {
        Instruction instruction = this->instructions[currentInstruction];
    
        switch (instruction.op)
        {
        case OP::MOVE:
            VMI::op_move(environment, instruction);
            break;
        case OP::LOAD_INT_CONST:
            environment.stackFrame.push(instruction.arg0);
            break;
        case OP::GETFIELD:
            VMI::op_getfield(environment, instruction);
            break;
        case OP::SETFIELD:
            VMI::op_setfield(environment, instruction);
            break;
        case OP::ADD:
            DO_ARITH(+, environment, instruction);
            break;
        case OP::SUBTRACT:
            DO_ARITH(-, environment, instruction);
            break;
        case OP::MULTIPLY:
            DO_ARITH(*, environment, instruction);
            break;
        case OP::DIVIDE:
            DO_ARITH(/, environment, instruction);
            break;
        case OP::REMAINDER:
            DO_ARITH(%, environment, instruction);
            break;

        default:
            std::cout << "No implementation for instruction : " << op2string(instruction.op) << std::endl;
            break;
        }
        this->currentInstruction++;
    }
}

};


