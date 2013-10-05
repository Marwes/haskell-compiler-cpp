#pragma once
#include <stdint.h>
#include "Types.h"

namespace MyVMNamespace
{
    enum class OP : unsigned char
    {
        NOP,
        MOVE,
        LOAD,//Load stackvalue onto the top
        LOAD_INT_CONST,
        LOAD_STRING_CONST,

        BRANCH_TRUE,
        
        NEWOBJECT,

        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        REMAINDER,


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
    
    VMInt arg0;
    OP op;
    unsigned char arg1;
    unsigned char arg2;
};

class Assembly
{
public:
    Assembly() : entrypoint(0) { }
    Assembly(Assembly&& other)
        : entrypoint(other.entrypoint)
        , instructions(std::move(other.instructions))
    {
    }

    int32_t entrypoint;
    std::vector<Instruction> instructions;
};

}

