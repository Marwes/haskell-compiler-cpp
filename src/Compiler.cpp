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


int Environment::addFunction(const std::string& name, Lambda& lambda)
{
	std::unique_ptr<FunctionDefinition> def(new FunctionDefinition());
	def->numArguments = lambda.arguments.size();
	for (auto& arg : lambda.arguments)
	{
		newLocal(arg);
	}
	lambda.expression->evaluate(*this, def->instructions);
	return assembly.addFunction(name, std::move(def));
}

int Environment::addLambda(Expression& expr)
{
	std::unique_ptr<FunctionDefinition> def(new FunctionDefinition());
	expr.evaluate(*this, def->instructions);
	std::stringstream name;
	name << "lambda" << lambdaIndex++;
	return assembly.addFunction(name.str(), std::move(def));
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
	assembly.addFunction("main", make_unique<FunctionDefinition>());
	std::unique_ptr<Expression> expr = parser.run();
	FunctionDefinition* def = assembly.getFunction("main");
	assert(def);
	expr->evaluate(env, def->instructions);
	return std::move(assembly);
}

}
