#include <assert.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include "VM.h"
#include "Method.h"

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
    static void op_move(MethodEnvironment& environment, Instruction current)
    {
        environment.stackFrame.at<VMInt>(current.arg0) = environment.stackFrame.at<VMInt>(current.arg1);
    }
    
    static void op_load(MethodEnvironment& environment, Instruction current)
    {
        environment.stackFrame.push(environment.stackFrame[current.arg0]);
    }

    static void op_load_int_constant(MethodEnvironment& environment, Instruction& current)
    {
        environment.stackFrame.push(current.arg0);
    }

    static void op_load_string_constant(MethodEnvironment& environment, Instruction& current)
    {
        String* string = data_cast<String*>(environment.method->data[current.arg0].get());

        environment.stackFrame.push(string);
    }

    static void op_branch_true(MethodEnvironment& environment, Instruction& current, size_t instructionPointer)
    {
        StackObject& obj = environment.stackFrame.top();
        if (obj.intValue != 0)
        {
            assert(current.arg0 < (int)environment.method->code.size());
            instructionPointer = current.arg0;
        }
    }

    static void op_newobject(MethodEnvironment& environment, Instruction current)
    {
        VMInt size = current.arg0;
        Object* obj = static_cast<Object*>(malloc(size));
        std::memset(obj, 0, size);

        environment.stackFrame.push(obj);
    }

    static void op_getfield(MethodEnvironment& environment, Instruction current)
    {
        StackObject* obj = &environment.stackFrame.top();
        const VMField& field = data_cast<const VMField&>(*environment.method->data[current.arg0]);

        environment.stackFrame[current.arg1] = *(obj + field.offset);
    }

    static void op_call(VM& vm, MethodEnvironment& environment, Instruction current)
    {
        Method& method = data_cast<Method&>(*environment.method->data[current.arg0]);

        MethodEnvironment newEnvironment(environment.stackFrame.makeChildFrame(), &method);
        
        vm.execute(newEnvironment);
    }
    
    static void op_setfield(MethodEnvironment& environment, Instruction current)
    {
        StackObject* obj = &environment.stackFrame.top();
        const VMField& field = data_cast<const VMField&>(*environment.method->data[current.arg0]);
        
        *(obj + field.offset) = environment.stackFrame[current.arg1];
    }

};

#define DO_ARITH(op, environment, instruction) {\
    VMInt lhs = environment.stackFrame.at<VMInt>(instruction.arg1);\
    VMInt rhs = environment.stackFrame.at<VMInt>(instruction.arg2);\
    environment.stackFrame.at<VMInt>(instruction.arg0) = lhs op rhs; }

#define DO_TOP_ARITH(op, environment, instruction) {\
    VMInt rhs = environment.stackFrame.top().intValue;\
    environment.stackFrame.pop();\
    VMInt lhs = environment.stackFrame.top().intValue;\
    StackObject o;\
    o.intValue = lhs op rhs;\
    environment.stackFrame.top() = o; }

typedef void (*execute_function_t)(VM& vm, Instruction current); 


MethodEnvironment::~MethodEnvironment()
{
    const StackLayout& stackLayout = this->method->stackLayout;
    //cleanup the stack
    for (size_t ii = 0; ii < stackFrame.size() && ii < stackLayout.types.size(); ++ii)
    {
        TypeEnum type = stackLayout.types[ii];
        switch (type)
        {
        case TYPE_ARRAY:
        case TYPE_CLASS:
            stackFrame[ii].pointerValue->removeReference();
            break;
        case TYPE_METHOD:
            break;
        default:
            break;
        }

    }
}

VM::VM()
{ }

VM::~VM()
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

void VM::execute(MethodEnvironment& environment)
{
    size_t currentInstruction = 0;
    const Slice<Instruction>& code = environment.method->code;
    while (currentInstruction < code.size())
    {
        Instruction instruction = code[currentInstruction];
    
        switch (instruction.op)
        {
        case OP::MOVE:
            VMI::op_move(environment, instruction);
            break;
        case OP::LOAD:
            VMI::op_load(environment, instruction);
            break;
        case OP::LOAD_INT_CONST:
            environment.stackFrame.push(instruction.arg0);
            break;
        case OP::LOAD_STRING_CONST:
            VMI::op_load_string_constant(environment, instruction);
            break;
        case OP::BRANCH_TRUE:
            VMI::op_branch_true(environment, instruction, currentInstruction);
            break;
        case OP::NEWOBJECT:
            VMI::op_newobject(environment, instruction);
            break;
        case OP::GETFIELD:
            VMI::op_getfield(environment, instruction);
            break;
        case OP::SETFIELD:
            VMI::op_setfield(environment, instruction);
            break;
        case OP::ADD:
            DO_TOP_ARITH(+, environment, instruction);
            break;
        case OP::SUBTRACT:
            DO_TOP_ARITH(-, environment, instruction);
            break;
        case OP::MULTIPLY:
            DO_TOP_ARITH(*, environment, instruction);
            break;
        case OP::DIVIDE:
            DO_TOP_ARITH(/, environment, instruction);
            break;
        case OP::REMAINDER:
            DO_TOP_ARITH(%, environment, instruction);
            break;

        case OP::CALL:
            VMI::op_call(*this, environment, instruction);
            break;

        default:
            std::cout << "No implementation for instruction : " << op2string(instruction.op) << std::endl;
            break;
        }
        currentInstruction++;
    }
}

void VM::endFrame(MethodEnvironment& environment)
{
    int stackLevel = 0;
    unsigned char* base = reinterpret_cast<unsigned char*>(environment.stackFrame.data());
    for (TypeEnum type : environment.method->stackLayout.types)
    {
        switch (type)
        {
        case TYPE_ARRAY:
        case TYPE_CLASS:
            VMPointer stackPos = reinterpret_cast<VMPointer>(base + stackLevel);
            break;
        }
        stackLevel += sizeofType(type);
    }
}

};


