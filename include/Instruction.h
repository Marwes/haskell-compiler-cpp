#pragma once
#include "Types.h"

namespace MyVMNamespace
{
    enum class OP : unsigned char
    {
        MOVE,
        LOAD_INT_CONST,
        
        NEWOBJECT,

        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        REMAINDER,

        NOP,

        CALL,

        NUM_INSTRUCTIONS,
    };

    const char* op2string(OP op);

struct Instruction
{
    Instruction()
        : op(OP::NOP)
        , arg0(0)
        , arg1(0)
        , arg2(0)
    { }

    Instruction(OP op, VMInt arg0 = 0, unsigned char arg1 = 0, unsigned char arg2 = 0)
        : op(op)
        , arg0(arg0)
        , arg1(arg1)
        , arg2(arg2)
    {
    }

    OP op;
    VMInt arg0;
    unsigned char arg1;
    unsigned char arg2;

};

}

