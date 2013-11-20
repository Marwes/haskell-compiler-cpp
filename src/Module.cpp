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
TypeDeclaration::TypeDeclaration(std::string name, Type type, std::vector<Type> constraints)
	: name(std::move(name))
	, type(std::move(type))
	, constraints(std::move(constraints))
{
}
TypeDeclaration::TypeDeclaration(TypeDeclaration && other)
	: name(std::move(other.name))
	, type(std::move(other.type))
	, constraints(std::move(other.constraints))
{
}

std::shared_ptr<Module> createPrelude()
{
	std::shared_ptr<Module> prelude(std::make_shared<Module>());
	prelude->imports.clear();//Remove prelude from itself
	{
		std::vector<Type> args(2);
		args[0] = TypeVariable();
		args[1] = TypeVariable();
		Constructor ctor("(,)", functionType(args[0], functionType(args[1], TypeOperator("(,)", args))), 0, 2);
		DataDefinition def;
		def.name = "(,)";
		def.constructors.push_back(ctor);
		prelude->dataDefinitions.push_back(def);
	}

	{
		std::vector<Type> args(1);
		args[0] = TypeVariable();
		TypeOperator listType("[]", args);
		Constructor ctor(":", functionType(args[0], functionType(listType, listType)), 0, 2);
		Constructor ctor2("[]", listType, 0, 0);
		DataDefinition def;
		def.name = "[]";
		def.constructors.push_back(ctor);
		def.constructors.push_back(ctor2);
		prelude->dataDefinitions.push_back(def);
	}
	prelude->bindings.push_back(Binding("undefined", std::unique_ptr<Expression>(new Name("undefined"))));
	return prelude;
}
const std::shared_ptr<Module> Module::prelude(createPrelude());

Module::Module()
{
	imports.push_back(prelude);
}

Module::Module(std::vector<Binding> && bindings, std::vector<TypeDeclaration> && typeDeclaration)
	: bindings(std::move(bindings))
	, typeDeclaration(std::move(typeDeclaration))
{
	imports.push_back(prelude);
}

Module::Module(Module && other)
	: bindings(std::move(other.bindings))
	, typeDeclaration(std::move(other.typeDeclaration))
	, dataDefinitions(std::move(other.dataDefinitions))
	, classes(std::move(other.classes))
	, instances(std::move(other.instances))
	, imports(std::move(other.imports))
{
}

void Module::typecheck()
{
	TypeEnvironment env(this);
	for (auto& bind : bindings)
	{
		Type newType = TypeVariable();
		Type& actual = bind.expression->typecheck(env);
		unify(env, newType, actual);
	}
}

}