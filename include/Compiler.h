#pragma once
#include <fstream>
#include <map>
#include "Expression.h"
#include "Tokenizer.h"
#include "Parser.h"

namespace MyVMNamespace
{

class Environment
{
public:
	Environment(Assembly& assembly);

	Environment childEnvironment() const;
	Assembly& getCurrentAssembly() { return assembly; }

	int newLocal(const std::string& name);

	size_t getStackTop()
	{
		return stackValues.size();
	}
	int getIndexForName(const std::string& name) const;

	int getFunction(const std::string& name)
	{
		auto found = assembly.functionDefinitionsIndexes.find(name);
		if (found == assembly.functionDefinitionsIndexes.end())
			return -1;
		else
			return found->second;
	}
	int getNativeFunction(const std::string& name)
	{
		auto found = assembly.nativeFunctionIndexes.find(name);
		if (found == assembly.nativeFunctionIndexes.end())
			return -1;
		else
			return found->second;
	}

	int addLambda(Lambda& expr);
	int addFunction(const std::string& name, Lambda& lambda);
private:
	const Environment* parent;
	Assembly& assembly;
	std::vector<std::string> stackValues;
	static int lambdaIndex;
};


class Evaluator
{
public:
	Evaluator(std::istream& input);

	Assembly compile();

private:
    Tokenizer tokenizer;
    Parser parser;
};

class Compiler
{
public:
	Compiler(std::istream& input);

	Assembly compile();
private:
	Tokenizer tokenizer;
	Parser parser;
};

}