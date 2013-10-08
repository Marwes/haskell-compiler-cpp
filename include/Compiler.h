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
	int newLocal(const std::string& name);

	int getIndexForName(const std::string& name);
private:
	std::map<std::string, std::vector<Instruction>> functions;
	std::vector<std::string> stackValues;
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