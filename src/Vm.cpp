#include "VM.h"
#include <assert.h>

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

    static void op_add(RuntimeEnvironment& environment, Instruction current)
    {
        VMInt lhs = environment.stackFrame.at<VMInt>(current.arg1);
        VMInt rhs = environment.stackFrame.at<VMInt>(current.arg2);
        environment.stackFrame.at<VMInt>(current.arg0) = lhs + rhs;
    }
};

typedef void (*execute_function_t)(VM& vm, Instruction current); 


void VM::execute()
{
    Instruction instruction = this->instructions[currentInstruction];
    
    switch (instruction.op)
    {
    case Instruction::MOVE:
        VMI::op_move(*this->currentEnvironment, instruction);
        break;
    case Instruction::GETFIELD:
        VMI::op_getfield(*this->currentEnvironment, instruction);
        break;
    case Instruction::SETFIELD:
        VMI::op_setfield(*this->currentEnvironment, instruction);
        break;
    case Instruction::ADD:
        VMI::op_add(*this->currentEnvironment, instruction);
        break;

    default:
        break;
    }
}

};


