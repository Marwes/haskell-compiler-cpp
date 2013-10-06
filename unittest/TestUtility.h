#include "Catch/include/catch.hpp"
#include "VM.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{

inline bool operator==(const Instruction& lhs, const Instruction& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(lhs)) == 0;
}

inline bool operator==(const Assembly& lhs, const Assembly& rhs)
{
    return lhs.entrypoint == rhs.entrypoint && lhs.instructions == rhs.instructions;
}

}