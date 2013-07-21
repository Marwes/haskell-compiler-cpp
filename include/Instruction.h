#pragma once
#include "Types.h"

namespace MyVMNamespace
{
    enum class OP : unsigned char
    {
        MOVE,
        LOAD,
        LOAD_INT_CONST,
        
        NEWOBJECT,

        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        REMAINDER,

        NUM_INSTRUCTIONS,
    };

    const char* op2string(OP op);

struct Instruction
{
    Instruction(OP op, VMInt arg0 = 0, unsigned char arg1 = 0, unsigned char arg2 = 0)
        : op(op)
        , arg0(arg0)
        , arg1(arg1)
        , arg2(arg2)
    {
    }

    const OP op;
    const VMInt arg0;
    const unsigned char arg1;
    const unsigned char arg2;

};

}

