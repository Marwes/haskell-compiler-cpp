#pragma once
#include <vector>
#include <map>
#include "Stack.h"
#include "Parser.h"
#include "SuperCombinator.h"
#include "Compiler.h"

namespace MyVMNamespace
{

enum NodeType
{
	NUMBER,
	DOUBLE,
	APPLICATION,
	GLOBAL,
	INDIRECTION,
	CONSTRUCTOR,
};

class Node;

class Address
{
	NodeType type;
public:
	NodeType getType() const
	{
		return type;
	}
	Node* getNode() const
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
	static Address numberDouble(Node* node)
	{
		Address a;
		a.node = node;
		a.type = DOUBLE;
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
	static Address constructor(Node* node)
	{
		Address a;
		a.node = node;
		a.type = CONSTRUCTOR;
		return a;
	}

private:
	Node* node;
};

struct ConstructorNode
{
	int tag;
	Address* arguments;
};

class Node
{
public:

	Node()
	{}
	Node(int number)
		: number(number)
	{}
	Node(double number)
		: numberDouble(number)
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
	Node(int tag, Address* arguments)
	{
		constructor.tag = tag;
		constructor.arguments = arguments;
	}
	union
	{
		int number;
		double numberDouble;
		struct
		{
			Address func;
			Address arg;
		} apply;
		Address indirection;
		ConstructorNode constructor;
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

	Address evaluate(SuperCombinator& environment);
	void execute(GEnvironment& environment);
	Address executeMain();

	Assembly& addAssembly(Assembly&& assembly);

	SuperCombinator* getCombinator(const std::string& name);

private:
	Array<Address> stack;
	std::vector<Address> globals;
	std::vector<Node> heap;
	std::vector<Assembly> assemblies;


	bool debug;
};


std::ostream& operator<<(std::ostream& out, const Address& addr);

};
