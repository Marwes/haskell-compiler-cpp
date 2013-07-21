#pragma once
#include <vector>
#include <Instruction.h>

namespace MyVMNamespace
{

std::vector<const Instruction> readAssemblyFile(const char* filename);

}