#include <sstream>
#include "Compiler.h"

namespace MyVMNamespace
{

int Environment::lambdaIndex;

Environment::Environment(Assembly& assembly)
	: parent(nullptr)
	, assembly(assembly)
{ }

Environment Environment::childEnvironment() const
{
	Environment child(assembly);
	child.parent = this;
	return std::move(child);
}

int Environment::newLocal(const std::string& name)
{
	stackValues.push_back(name);
	return stackValues.size() - 1;
}

int Environment::getIndexForName(const std::string& name) const
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
	Environment child = childEnvironment();
	for (auto& arg : lambda.arguments)
	{
		child.newLocal(arg);
	}
	lambda.expression->evaluate(child, PolymorphicType::any, def->instructions);
	return assembly.addFunction(name, std::move(def));
}

int Environment::addLambda(Lambda& lambda)
{
	std::unique_ptr<FunctionDefinition> def(new FunctionDefinition());

	Environment child = childEnvironment();
	for (auto& arg : lambda.arguments)
	{
		child.newLocal(arg);
	}
	lambda.expression->evaluate(child, PolymorphicType::any, def->instructions);
	std::stringstream name;
	name << "lambda" << lambdaIndex++;
	return assembly.addFunction(name.str(), std::move(def));
}


Evaluator::Evaluator(std::istream& input)
	: tokenizer(input)
	, parser(tokenizer)
{
}

Assembly Evaluator::compile()
{
	Assembly assembly;
	Environment env(assembly);
	assembly.addFunction("main", make_unique<FunctionDefinition>());
	std::unique_ptr<Expression> expr = parser.run();
	FunctionDefinition* def = assembly.getFunction("main");
	assert(def);
	expr->evaluate(env, PolymorphicType::any, def->instructions);
	return std::move(assembly);
}

}
