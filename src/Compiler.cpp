#include <sstream>
#include "Compiler.h"

namespace MyVMNamespace
{

int Environment::lambdaIndex;

Environment::Environment(Assembly& assembly)
	: parent(nullptr)
	, assembly(assembly)
{ }

Environment::Environment(Environment&& other)
	: parent(other.parent)
	, assembly(other.assembly)
	, stackTypes(std::move(other.stackTypes))
	, stackValues(std::move(other.stackValues))
{ }

Environment Environment::childEnvironment() const
{
	Environment child(assembly);
	child.parent = this;
	return std::move(child);
}

int Environment::newLocal(const std::string& name, const Type* type)
{
	stackValues.push_back(name);
	stackTypes.push_back(type);
	const Environment* p = this->parent;
	size_t varIndex = stackValues.size() - 1;
	while (p != nullptr)
	{
		varIndex += p->stackValues.size();
		p = p->parent;
	}
	return varIndex;
}

Variable Environment::getVariable(const std::string& name) const
{
	auto found = std::find(stackValues.begin(), stackValues.end(), name);
	if (found != stackValues.end())
	{
		size_t typeIndex = std::distance(stackValues.begin(), found);
		assert(stackTypes[typeIndex] != nullptr);
		const Environment* p = this->parent;
		size_t varIndex = typeIndex;
		while (p != nullptr)
		{
			varIndex += p->stackValues.size();
			p = p->parent;
		}
		return Variable { VariableType::STACK, *stackTypes[typeIndex], varIndex };
	}
	if (parent == nullptr)
	{
		return this->getFunction(name);
	}
	else
	{
		return parent->getVariable(name);
	}
	return Variable { VariableType::STACK, PolymorphicType::any, -1 };
}


Variable Environment::getFunction(const std::string& name) const
{
	auto found = assembly.functionDefinitionsIndexes.find(name);
	if (found == assembly.functionDefinitionsIndexes.end())
		return Variable { VariableType::TOPLEVEL, PolymorphicType::any, -1 };
	else
		return Variable { VariableType::TOPLEVEL, PolymorphicType::any, found->second };//TODO
}
int Environment::getNativeFunction(const std::string& name) const
{
	auto found = assembly.nativeFunctionIndexes.find(name);
	if (found == assembly.nativeFunctionIndexes.end())
		return -1;
	else
		return found->second;
}


int Environment::addFunction(const std::string& name, const RecursiveType& type, Lambda& lambda)
{
	std::unique_ptr<FunctionDefinition> def(new FunctionDefinition(std::unique_ptr<RecursiveType>(type.copy())));
	Environment newEnv(this->assembly);
	const Type* exprType = &type;
	def->numArguments = lambda.arguments.size();
	for (auto& arg : lambda.arguments)
	{
		auto func = dynamic_cast<const RecursiveType*>(exprType);
		if (func != nullptr)
		{
			newEnv.newLocal(arg, &func->getArgumentType());
			exprType = &func->getReturnType();
		}
		if (exprType == nullptr)
			throw std::runtime_error("Not enough parameters for function!");
	}
	assert(exprType != nullptr);
	lambda.expression->evaluate(newEnv, *exprType, def->instructions);
	return assembly.addFunction(name, std::move(def));
}

int Environment::addLambda(Lambda& lambda, const FunctionType& inferred)
{
	std::unique_ptr<FunctionType> function(inferred.copy());
	std::unique_ptr<FunctionDefinition> def(new FunctionDefinition(std::move(function)));

	Environment child = childEnvironment();
	for (auto& arg : lambda.arguments)
	{
		child.newLocal(arg, &PolymorphicType::any);
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
	compile(assembly);
	return std::move(assembly);
}

void Evaluator::compile(Assembly& assembly)
{
	Environment env(assembly);
	std::unique_ptr<RecursiveType> mainType(PolymorphicType::any.copy());
	assembly.addFunction("main", std::unique_ptr<FunctionDefinition>(new FunctionDefinition(std::move(mainType))));
	std::unique_ptr<Expression> expr = parser.run();
	FunctionDefinition* def = assembly.getFunction("main");
	assert(def);
	TypeEnvironment typeEnv;
	for (auto& pair : assembly.functionDefinitionsIndexes)
	{
		auto& funcDef = assembly.functionDefinitions[pair.second];
		typeEnv.addType(pair.first, *funcDef->type);

	}
	expr->typecheck(typeEnv, PolymorphicType::any);
	expr->evaluate(env, PolymorphicType::any, def->instructions);
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
	Module module = parser.toplevel();

	assert(module.bindings.size() == module.typeDeclaration.size());
	for (size_t ii = 0; ii < module.bindings.size(); ++ii)
	{
		const Binding& bind = module.bindings[0];
		const TypeDeclaration& decl = module.typeDeclaration[0];
		TypeEnvironment env;

		bind.expression->typecheck(env, *decl.type);
	}

	for (size_t ii = 0; ii < module.bindings.size(); ++ii)
	{
		const Binding& bind = module.bindings[0];
		const TypeDeclaration& decl = module.typeDeclaration[0];
		if (Lambda* lambda = dynamic_cast<Lambda*>(bind.expression.get()))
		{
			if (auto funcType = dynamic_cast<const FunctionType*>(decl.type.get()))
				env.addFunction(bind.name, *funcType, *lambda);
			else
				throw TypeError("Expected: function type", *decl.type);
		}
		else
		{
			std::unique_ptr<FunctionDefinition> type(new FunctionDefinition(std::unique_ptr<RecursiveType>(PolymorphicType::any.copy())));
			assembly.addFunction(bind.name, std::move(type));
			FunctionDefinition* def = assembly.getFunction(bind.name);
			assert(def);
			bind.expression->evaluate(env, *decl.type, def->instructions);
		}
	}

	return std::move(assembly);
}

}
