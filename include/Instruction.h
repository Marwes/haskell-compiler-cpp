#pragma once
#include "Types.h"

namespace MyVMNamespace
{

struct Instruction
{
    Instruction(unsigned char op, VMInt arg0 = 0, unsigned char arg1 = 0, unsigned char arg2 = 0)
        : op(op)
        , arg0(arg0)
        , arg1(arg1)
        , arg2(arg2)
    {
    }

    const unsigned char op;
    const VMInt arg0;
    const unsigned char arg1;
    const unsigned char arg2;

    enum
    {
        MOVE,
        LOAD,
        
        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,

        NUM_INSTRUCTIONS,
    };    
};

}

