#pragma once
#include <stdint.h>
#include <map>
#include "Types.h"
#include "Util.h"


namespace MyVMNamespace
{

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

class FunctionDefinition
{
public:
	FunctionDefinition()
		: numArguments(0)
	{}
	int numArguments;
	std::vector<Instruction> instructions;
};

class Assembly
{
public:
    Assembly() : entrypoint(0) { }
    Assembly(Assembly&& other)
		: functionDefinitionsIndexes(std::move(other.functionDefinitionsIndexes))
		, functionDefinitions(std::move(other.functionDefinitions))
		, entrypoint(other.entrypoint)
    {
    }

	Assembly& operator=(Assembly && other)
	{
		functionDefinitionsIndexes = std::move(other.functionDefinitionsIndexes);
		functionDefinitions = std::move(other.functionDefinitions);
		entrypoint = other.entrypoint;
		return *this;
	}


	int addFunction(const std::string& name, std::unique_ptr<FunctionDefinition>&& def)
	{
		int index = functionDefinitionsIndexes.size();
		functionDefinitionsIndexes.insert(std::make_pair(name, index));
		functionDefinitions.push_back(std::move(def));
		return index;
	}

	FunctionDefinition* getFunction(size_t index)
	{
		return functionDefinitions[index].get();
	}

	FunctionDefinition* getFunction(const std::string& name)
	{
		auto& found = functionDefinitionsIndexes.find(name);
		if (found == functionDefinitionsIndexes.end())
		{
			return nullptr;
		}
		return functionDefinitions[found->second].get();
	}

	std::map<std::string, int> functionDefinitionsIndexes;
	std::vector<std::unique_ptr<FunctionDefinition>> functionDefinitions;
	int32_t entrypoint;
};

}

