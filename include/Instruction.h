#pragma once
#include <stdint.h>
#include <map>
#include "Types.h"
#include "Util.h"


namespace MyVMNamespace
{
struct VM;
class StackFrame;

#define OP_ENUM(t,XX) \
	XX(t, NOP) \
	XX(t, MOVE) \
	XX(t, LOAD) \
	XX(t, LOAD_FUNCTION) \
	XX(t, LOAD_INT_CONST) \
	XX(t, LOAD_STRING_CONST) \
	XX(t, POP) \
	XX(t, BRANCH_TRUE) \
	XX(t, JUMP) \
	XX(t, NEWOBJECT) \
	XX(t, GETFIELD) \
	XX(t, SETFIELD) \
	XX(t, ADD) \
	XX(t, SUBTRACT) \
	XX(t, MULTIPLY) \
	XX(t, DIVIDE) \
	XX(t, REMAINDER) \
	XX(t, COMPARE_EQ) \
	XX(t, COMPARE_NEQ) \
	XX(t, COMPARE_LT) \
	XX(t, COMPARE_GT) \
	XX(t, COMPARE_LE) \
	XX(t, COMPARE_GE) \
	XX(t, CALL) \
	XX(t, CALLI) \
	XX(t, CALLNATIVE) \
	XX(t, RETURN) \
	XX(t, NUM_INSTRUCTIONS) \

DECLARE_ENUM(OP, OP_ENUM);

struct Instruction
{
    Instruction()
        : op(OP::NOP)
        , arg0(0)
        , arg1(0)
        , arg2(0)
    { }

    Instruction(OP op, VMInt arg0 = 0, unsigned char arg1 = 0, unsigned char arg2 = 0)
        : op(op)
        , arg0(arg0)
        , arg1(arg1)
        , arg2(arg2)
    {
    }
    
    VMInt arg0;
    OP op;
    unsigned char arg1;
    unsigned char arg2;
};

class FunctionDefinition : public Object
{
public:
	FunctionDefinition()
		: numArguments(0)
	{}
	int numArguments;
	std::vector<Instruction> instructions;
};

typedef int (*VM_Function)(VM& vm, StackFrame& stack);

class Assembly
{
public:
	Assembly();
	Assembly(Assembly && other);

	Assembly& operator=(Assembly && other);

	int addFunction(const std::string& name, std::unique_ptr<FunctionDefinition> && def);

	FunctionDefinition* getFunction(size_t index) const;
	FunctionDefinition* getFunction(const std::string& name) const;


	VM_Function getNativeFunction(const std::string& name)
	{
		auto& found = nativeFunctionIndexes.find(name);
		if (found == nativeFunctionIndexes.end())
		{
			return nullptr;
		}
		return getNativeFunction(found->second);
	}

	VM_Function getNativeFunction(int ii)
	{
		return nativeFunctions[ii];
	}

	std::map<std::string, int> functionDefinitionsIndexes;
	std::vector<std::unique_ptr<FunctionDefinition>> functionDefinitions;
	std::map<std::string, int> nativeFunctionIndexes;
	std::vector<VM_Function> nativeFunctions;
	int32_t entrypoint;
};

}

