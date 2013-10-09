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

	Assembly& getCurrentAssembly() { return assembly; }

	int newLocal(const std::string& name);

	int getIndexForName(const std::string& name);


	int addLambda(Expression& expr);
private:
	Assembly& assembly;
	std::vector<std::string> stackValues;
	int lambdaIndex;
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