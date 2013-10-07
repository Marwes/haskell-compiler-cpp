#include "Compiler.h"

namespace MyVMNamespace
{

int Environment::newLocal(const std::string& name)
{
	stackValues.push_back(name);
	return stackValues.size() - 1;
}

int Environment::getIndexForName(const std::string& name)
{
	auto found = std::find(stackValues.begin(), stackValues.end(), name);
	if (found != stackValues.end())
	{
		return std::distance(stackValues.begin(), found);
	}
	return -1;
}



Compiler::Compiler(std::istream& input)
	: tokenizer(input)
	, parser(tokenizer)
{
}

Assembly Compiler::compile()
{
	Assembly assembly;
	parser.run()->evaluate(assembly.instructions);
	return std::move(assembly);
}

}
