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
	GOP op;
	int value;
};

class SuperCombinator
{
public:
	int arity;
	std::vector<GInstruction> instructions;
};

enum NodeType
{
	NUMBER = 1 << 0,
	APPLICATION = 1 << 1,
	GLOBAL = 1 << 2
};

class Node;

class Address
{
public:
	NodeType getType()
	{
		uintptr_t ptr = uintptr_t(node);
		if (ptr & NUMBER)
			return NUMBER;
		if (ptr & APPLICATION)
			return APPLICATION;
		return GLOBAL;
	}
	Node* getNode()
	{
		uintptr_t ptr = uintptr_t(node);

		return reinterpret_cast<Node*>(ptr & ~3);
	}

	static Address number(Node* node)
	{
		Address a;
		uintptr_t ptr = uintptr_t(node);
		ptr |= NUMBER;
		a.node = reinterpret_cast<Node*>(ptr);
		return a;
	}
	static Address application(Node* node)
	{
		Address a;
		uintptr_t ptr = uintptr_t(node);
		ptr |= APPLICATION;
		a.node = reinterpret_cast<Node*>(ptr);
		return a;
	}
	static Address global(Node* node)
	{
		Address a;
		uintptr_t ptr = uintptr_t(node);
		ptr |= GLOBAL;
		a.node = reinterpret_cast<Node*>(ptr);
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

	void execute(GEnvironment& environment);

private:
	std::map<int, Address> globals;
	std::vector<Node> heap;
};

};