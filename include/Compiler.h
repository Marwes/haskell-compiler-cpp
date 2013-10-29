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
	Environment(Environment&& other);

	Environment childEnvironment() const;
	Assembly& getCurrentAssembly() { return assembly; }

	int newLocal(const std::string& name, const Type* type);

	size_t getStackTop()
	{
		return stackValues.size();
	}
	Variable getVariable(const std::string& name) const;

	Variable getFunction(const std::string& name) const;
	int getNativeFunction(const std::string& name) const;

	int addLambda(Lambda& expr, const FunctionType& inferred);
	int addFunction(const std::string& name, const RecursiveType& type, Lambda& lambda);
private:
	const Environment* parent;
	Assembly& assembly;
	std::vector<std::string> stackValues;
	std::vector<const Type*> stackTypes;
	static int lambdaIndex;
};


class Evaluator
{
public:
	Evaluator(std::istream& input);

	Assembly compile();
	void compile(Assembly& assembly);

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