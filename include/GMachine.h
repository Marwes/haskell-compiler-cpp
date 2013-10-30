#pragma once
#include <vector>
#include <map>
#include "Stack.h"

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
	EVAL
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

enum NodeType
{
	NUMBER,
	APPLICATION,
	GLOBAL,
	INDIRECTION
};

class Node;

class Address
{
	NodeType type;
public:
	NodeType getType()
	{
		return type;
	}
	Node* getNode()
	{
		return node;
	}

	static Address number(Node* node)
	{
		Address a;
		a.node = node;
		a.type = NUMBER;
		return a;
	}
	static Address application(Node* node)
	{
		Address a;
		a.node = node;
		a.type = APPLICATION;
		return a;
	}
	static Address global(Node* node)
	{
		Address a;
		a.node = node;
		a.type = GLOBAL;
		return a;
	}
	static Address indirection(Node* node)
	{
		Address a;
		a.node = node;
		a.type = INDIRECTION;
		return a;
	}

private:
	Node* node;
};

class Node
{
public:

	Node()
	{}
	Node(int number)
		: number(number)
	{}
	Node(Address indirection)
		: indirection(indirection)
	{}
	Node(Address func, Address arg)
	{
		apply.func = func;
		apply.arg = arg;
	}
	Node(SuperCombinator* global)
		: global(global)
	{}
	union
	{
		int number;
		struct
		{
			Address func;
			Address arg;
		} apply;
		Address indirection;
		SuperCombinator* global;
	};
};


class GEnvironment
{
public:
	GEnvironment(StackFrame<Address> stack, SuperCombinator* combinator);

	GEnvironment child(SuperCombinator* combinator);

	StackFrame<Address> stack;
	SuperCombinator* combinator;
};


class GMachine
{
public:
	GMachine();


	StackFrame<Address> baseStack()
	{
		return StackFrame<Address>(stack.data(), stack.size());
	}

	void compile(std::istream& input);

	void execute(GEnvironment& environment);
	Address executeMain();

	SuperCombinator* getCombinator(const std::string& name)
	{
		return superCombinators[name].get();
	}

private:
	Array<Address> stack;
	std::vector<Address> globals;
	std::vector<Node> heap;

	std::map<std::string, std::unique_ptr<SuperCombinator>> superCombinators;
};

};