#pragma once
#include <Util.h>
#include <fstream>
#include <string>

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
            throw new std::runtime_error(std::string("The value ") + op + " does not represent a valid opcode.");
        }
        instruction = Instruction(actual, arg0, arg1, arg2);
    }
    return stream;
}

Assembly readAssemblyFile(const char* filename)
{
    Assembly assembly;
    std::ifstream stream(filename, std::ios::binary);
    if (!stream.is_open() || stream.bad())
        throw new std::runtime_error(std::string("Could not find the file ") + filename);
    Instruction i;

    stream.read((char*)&assembly.entrypoint, sizeof(assembly.entrypoint));

    FROM_BIGENDIAN(&assembly.entrypoint);

    while (stream >> i)
    {
        assembly.instructions.push_back(i);
    }
    return std::move(assembly);
}
}