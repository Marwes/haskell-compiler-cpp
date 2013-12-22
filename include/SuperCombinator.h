#pragma once
#include <vector>
#include <map>
#include "Types.h"

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
	PUSH_DOUBLE,
	PUSH,
	PUSH_DICTIONARY_MEMBER,
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

	ADD_DOUBLE,
	SUBTRACT_DOUBLE,
	MULTIPLY_DOUBLE,
	DIVIDE_DOUBLE,
	NEGATE_DOUBLE,

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
	GInstruction(GOP op, double value)
		: op(op)
		, doubleValue(value)
	{}

	GOP op;
	union
	{
		int value;
		double doubleValue;
	};
};

class SuperCombinator
{
public:
	std::string name;
	Type type;
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
	Type type;
	std::map<std::string, Type> parameters;
	std::vector<Constructor> constructors;
};

};
