#include <Instruction.h>

namespace MyVMNamespace
{

const char* op2string(OP op)
{
    static const char* ops[] = { "MOVE", "LOAD", "LOAD_INT_CONST", "GETFIELD", "SETFIELD", "ADD", "SUBTRACT" };
    return ops[(int)op];
}

}