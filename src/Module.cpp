#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"
#include "Module.h"

namespace MyVMNamespace
{

Binding::Binding(std::string name, std::unique_ptr<Expression> expression)
	: name(std::move(name))
	, expression(std::move(expression))
{
}
Binding::Binding(Binding&& other)
	: name(std::move(other.name))
	, expression(std::move(other.expression))
{
}

TypeDeclaration::TypeDeclaration(std::string name, Type type)
	: name(std::move(name))
	, type(std::move(type))
{
}
TypeDeclaration::TypeDeclaration(TypeDeclaration && other)
	: name(std::move(other.name))
	, type(std::move(other.type))
{
}

Module::Module()
{
	std::vector<Type> args(2);
	args[0] = TypeVariable();
	args[1] = TypeVariable();
	Constructor ctor("(,)", functionType(args[0], functionType(args[1], TypeOperator("(,)", args))), 0, 2);
	DataDefinition def;
	def.name = "(,)";
	def.constructors.push_back(ctor);
	dataDefinitions.push_back(def);
}

Module::Module(std::vector<Binding> && bindings, std::vector<TypeDeclaration> && typeDeclaration)
	: bindings(std::move(bindings))
	, typeDeclaration(std::move(typeDeclaration))
{
	std::vector<Type> args(2);
	args[0] = TypeVariable();
	args[1] = TypeVariable();
	Constructor ctor("(,)", functionType(args[0], functionType(args[1], TypeOperator("(,)", args))), 0, 2);
	DataDefinition def;
	def.name = "(,)";
	def.constructors.push_back(ctor);
	dataDefinitions.push_back(def);
}

void Module::typecheck()
{
	TypeEnvironment env(this);
	for (auto& bind : bindings)
	{
		env.bindName(bind.name, bind.expression->getType());
	}
	for (auto& bind : bindings)
	{
		Type newType = TypeVariable();
		Type& actual = bind.expression->typecheck(env);
		unify(env, newType, actual);
	}
}

}