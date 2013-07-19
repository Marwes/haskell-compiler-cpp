#pragma once

struct Instruction
{
    int value;
    
    enum
    {
        MOVE,
        LOADK,
        
        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,
    };
    static Instruction add();
};

