#include "Catch/include/catch.hpp"
#include <VM.h>
#include <string>
#include <fstream>
#include <vector>

using namespace MyVMNamespace;

int LongSwap (int i)
{
  unsigned char b1, b2, b3, b4;

  b1 = i & 255;
  b2 = ( i >> 8 ) & 255;
  b3 = ( i>>16 ) & 255;
  b4 = ( i>>24 ) & 255;

  return ((int)b1 << 24) + ((int)b2 << 16) + ((int)b3 << 8) + b4;
}

std::istream& operator>>(std::ifstream& stream, Instruction& instruction)
{
    char op;
    VMInt arg0;
    char arg1;
    char arg2;
    if (stream.read(&op, sizeof(char)) && stream.read((char*)&arg0, sizeof(VMInt)) &&
        stream.read(&arg1, sizeof(char)) && stream.read(&arg2, sizeof(char)))
    {
        arg0 = LongSwap(arg0);
        OP actual = static_cast<OP>(op);
        if (op2string(actual) == NULL)
        {
            throw new std::runtime_error(std::string("The value ") + op + " does not represent a valid opcode.");
        }
        instruction = Instruction(actual, arg0, arg1, arg2);
    }
    return stream;
}

std::vector<const Instruction> readAsm(const std::string& filename)
{
    std::vector<const Instruction> instructions;
    std::ifstream stream(filename.c_str(), std::ios::binary);
    if (!stream.is_open() || stream.bad())
        throw new std::runtime_error("Could not find the file " + filename);
    Instruction i;

    while (stream >> i)
    {
        instructions.push_back(i);
    }
    return std::move(instructions);
}

bool compareInstructions(const Instruction& lhs, const Instruction& rhs)
{
    return memcmp(&lhs, &rhs, sizeof(lhs)) == 0;
}

TEST_CASE("arithmetic", "test arithmetic")
{
    std::vector<const Instruction> instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 0));
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5)); 
    instructions.push_back(Instruction(OP::ADD, 0, 1, 2));

    std::vector<const Instruction> fileInstructions(readAsm("Arithmetic.asm"));
    REQUIRE(std::equal(instructions.begin(), instructions.end(), fileInstructions.begin(), compareInstructions));
    instructions = fileInstructions;
    {
        VM vm(instructions);
        MethodEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 15);
        REQUIRE(vm.getValue(1).intValue == 10);
        REQUIRE(vm.getValue(2).intValue == 5);
    }

    // 15 10 5
    // [0] = 5 - 10
    instructions.push_back(Instruction(OP::SUBTRACT, 0, 2, 1));
    {
        VM vm(instructions);
        MethodEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -5);
    }

    // -5 10 5
    // [0] = -5 * 10
    instructions.push_back(Instruction(OP::MULTIPLY, 0, 0, 1));
    {
        VM vm(instructions);
        MethodEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == -50);
    }
    // -50 10 5
    // [0] = 10 / 5
    instructions.push_back(Instruction(OP::DIVIDE, 0, 1, 2));
    {
        VM vm(instructions);
        MethodEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 2);
    }
    // 2 10 5
    // [0] = 5 % 2
    instructions.push_back(Instruction(OP::REMAINDER, 0, 2, 0));
    {
        VM vm(instructions);
        MethodEnvironment env(vm.newStackFrame());
        vm.execute(env);
        REQUIRE(vm.getValue(0).intValue == 1);
    }
}