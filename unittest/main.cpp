#include <assert.h>
#include <VM.h>

int main()
{
    using namespace MyVMNamespace;

    std::vector<Instruction> instructions;
    VM vm(instructions);

    return 0;
}