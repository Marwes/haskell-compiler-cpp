#pragma once
#include <Instruction.h>
#include <vector>
#include <array>

struct VM
{

    void execute();
private:
    int currentInstruction;
    std::vector<Instruction> instructions;

    std::array<int, 4> registers;
};
