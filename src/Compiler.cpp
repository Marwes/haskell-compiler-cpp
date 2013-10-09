#include <sstream>
#include "Compiler.h"

namespace MyVMNamespace
{

Environment::Environment(Assembly& assembly)
	: assembly(assembly)
	, lambdaIndex(0)
{ }

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



int Environment::addLambda(Expression& expr)
{
	FunctionDefinition def;
	expr.evaluate(*this, def.instructions);
	std::stringstream name;
	name << "lambda" << lambdaIndex++;
	return assembly.addFunction(name.str(), def);
}


Compiler::Compiler(std::istream& input)
	: tokenizer(input)
	, parser(tokenizer)
{
}

Assembly Compiler::compile()
{
	Assembly assembly;
	Environment env(assembly);
	assembly.addFunction("main", FunctionDefinition());
	parser.run()->evaluate(env, assembly.getFunction("main")->instructions);
	return std::move(assembly);
}

}
