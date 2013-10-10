#include <algorithm>
#include <fstream>
#include <string>
#include <stdexcept>
#include <Util.h>
#include <Method.h>

namespace MyVMNamespace
{

int swapEndian(int i)
{
    unsigned char b1, b2, b3, b4;

    b1 = i & 255;
    b2 = ( i >> 8 ) & 255;
    b3 = ( i>>16 ) & 255;
    b4 = ( i>>24 ) & 255;

    return ((int)b1 << 24) + ((int)b2 << 16) + ((int)b3 << 8) + b4;
}


#ifdef BIG_ENDIAN
#define FROM_BIGENDIAN(x) (x)
#else
#define FROM_BIGENDIAN(x) (std::reverse(static_cast<char*>(static_cast<void*>(x)), static_cast<char*>(static_cast<void*>(x)) + sizeof(x)))
#endif


std::istream& operator>>(std::ifstream& stream, Instruction& instruction)
{
    char op;
    VMInt arg0;
    char arg1;
    char arg2;
    if (stream.read(&op, sizeof(char)) && stream.read((char*)&arg0, sizeof(VMInt)) &&
        stream.read(&arg1, sizeof(char)) && stream.read(&arg2, sizeof(char)))
    {
        FROM_BIGENDIAN(&arg0);
        OP actual = static_cast<OP>(op);
        if (op2string(actual) == NULL)
        {
            throw std::runtime_error(std::string("The value ") + op + " does not represent a valid opcode.");
        }
        instruction = Instruction(actual, arg0, arg1, arg2);
    }
    return stream;
}

void checkStack(Method& method)
{
    auto& types = method.stackLayout.types;
    for (auto& instruction : method.code)
    {
        switch (instruction.op)
        {
        case OP::MOVE:
            {
            VMInt fromPos = instruction.arg0;
            unsigned char toPos = instruction.arg1;
            if (types.size() < toPos)
                types.reserve(toPos);
            
            if (types[toPos] != types[fromPos])
                throw std::runtime_error("");
            else
                types[toPos] = types[fromPos];
            break;
            }

        default:
            break;
        }
    }
}

Assembly readAssemblyFile(const char* filename)
{
    Assembly assembly;
    std::ifstream stream(filename, std::ios::binary);
    if (!stream.is_open() || stream.bad())
        throw new std::runtime_error(std::string("Could not find the file ") + filename);
    Instruction i;
    stream.read((char*)&assembly.entrypoint, sizeof(assembly.entrypoint));
    
    bool st = stream.good();
    FROM_BIGENDIAN(&assembly.entrypoint);

	assembly.addFunction("main", make_unique<FunctionDefinition>());
	FunctionDefinition& def = *assembly.getFunction("main");

    while (stream >> i)
    {
        def.instructions.push_back(i);
    }
    return std::move(assembly);
}

}
