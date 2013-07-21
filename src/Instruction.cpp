#include <Instruction.h>

namespace MyVMNamespace
{

const char* op2string(OP op)
{
    static const char* ops[] = { "MOVE", "LOAD", "LOAD_INT_CONST", "GETFIELD", "SETFIELD", "ADD", "SUBTRACT" };
    size_t opLength = sizeof(ops) / sizeof(ops[0]);
    return (unsigned char)op < opLength ? ops[(int)op] : 0;
}

}