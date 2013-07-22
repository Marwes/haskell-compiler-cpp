#pragma once
#include <vector>
#include <Instruction.h>

namespace MyVMNamespace
{

std::vector<Instruction> readAssemblyFile(const char* filename);

}