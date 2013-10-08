#include <cstring>
#include "Catch/include/catch.hpp"
#include "VM.h"

using namespace MyVMNamespace;

namespace MyVMNamespace
{

inline bool operator==(const Instruction& lhs, const Instruction& rhs)
{
    return std::memcmp(&lhs, &rhs, sizeof(lhs)) == 0;
}


inline bool operator==(const FunctionDefinition& lhs, const FunctionDefinition& rhs)
{
	return lhs.numArguments == rhs.numArguments && lhs.instructions == rhs.instructions;
}

inline bool operator==(const Assembly& lhs, const Assembly& rhs)
{
    return lhs.entrypoint == rhs.entrypoint && lhs.functionDefinitions == rhs.functionDefinitions;
}

}
