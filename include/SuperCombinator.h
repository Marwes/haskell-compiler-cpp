#pragma once
#include <vector>

namespace MyVMNamespace
{
enum class GOP : unsigned char
{
	SLIDE,
	ALLOC,
	UPDATE,
	POP,
	UNWIND,
	PUSH_GLOBAL,
	PUSH_INT,
	PUSH,
	MKAP,
	EVAL,
	PACK,
	SPLIT,
	CASEJUMP,
	JUMP,

    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
	NEGATE,

	COMPARE_EQ,
	COMPARE_NEQ,
	COMPARE_GT,
	COMPARE_GE,
	COMPARE_LT,
	COMPARE_LE
};

class GInstruction
{
public:
	GInstruction(GOP op, int value = 0)
		: op(op)
		, value(value)
	{}

	GOP op;
	int value;
};

class SuperCombinator
{
public:
	std::string name;
	int arity;
	std::vector<GInstruction> instructions;
};


class Constructor
{
public:
	Constructor(std::string name, Type type, int tag, int arity)
		: name(std::move(name))
		, type(std::move(type))
		, tag(tag)
		, arity(arity)
	{}
	std::string name;
	Type type;
	int tag;
	int arity;
};

class DataDefinition
{
public:
	std::string name;
	std::vector<Constructor> constructors;
};

};
