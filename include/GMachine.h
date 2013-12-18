#pragma once
#include <vector>
#include <map>
#include "Stack.h"
#include "Parser.h"
#include "SuperCombinator.h"

namespace MyVMNamespace
{

enum NodeType
{
	NUMBER,
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

class Assembly
{
public:
	Assembly() {}
	Assembly(Assembly&& o)
		: superCombinators(std::move(o.superCombinators))
		, dataDefinitions(std::move(o.dataDefinitions))
		, instanceDictionaries(std::move(o.instanceDictionaries))
		, globalIndices(std::move(o.globalIndices))
		, instanceIndices(std::move(o.instanceIndices))
	{}

	Assembly& operator=(Assembly&& o)
	{
		superCombinators = std::move(o.superCombinators);
		dataDefinitions = std::move(o.dataDefinitions);
		instanceDictionaries = std::move(o.instanceDictionaries);
		globalIndices = std::move(o.globalIndices);
		instanceIndices = std::move(o.instanceIndices);
		return *this;
	}

	std::map<std::string, std::unique_ptr<SuperCombinator>> superCombinators;
	std::vector<Constructor> dataDefinitions;
	std::vector<InstanceDictionary> instanceDictionaries;
	std::map<SuperCombinator*, int> globalIndices;
	std::map<std::vector<TypeOperator>, int> instanceIndices;
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
		return assembly.superCombinators[name].get();
	}

private:
	Array<Address> stack;
	std::vector<Address> globals;
	std::vector<Node> heap;

	Assembly assembly;

	bool debug;
};

};
