#pragma once
#include <stdint.h>
#include <map>
#include "Types.h"

namespace MyVMNamespace
{
    enum class OP : unsigned char
    {
        NOP,
        MOVE,
        LOAD,//Load stackvalue onto the top
        LOAD_INT_CONST,
        LOAD_STRING_CONST,

        BRANCH_TRUE,
        
        NEWOBJECT,

        GETFIELD,
        
        SETFIELD,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        REMAINDER,


        CALL,

        NUM_INSTRUCTIONS,
    };

    const char* op2string(OP op);

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


	int addFunction(const std::string& name, const FunctionDefinition& def)
	{
		int index = functionDefinitionsIndexes.size();
		functionDefinitionsIndexes.insert(std::make_pair(name, index));
		functionDefinitions.push_back(def);
		return index;
	}

	FunctionDefinition* getFunction(const std::string& name)
	{
		auto& found = functionDefinitionsIndexes.find(name);
		if (found == functionDefinitionsIndexes.end())
		{
			return nullptr;
		}
		return &functionDefinitions[found->second];
	}

	std::map<std::string, int> functionDefinitionsIndexes;
	std::vector<FunctionDefinition> functionDefinitions;
	int32_t entrypoint;
};

}

