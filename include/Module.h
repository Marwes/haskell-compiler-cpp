#pragma once
#include <vector>
#include <memory>
#include "Expression.h"
#include "Types.h"

namespace MyVMNamespace
{

class Binding
{
public:
	Binding(std::string name, std::unique_ptr<Expression> expression);
	Binding(Binding && binding);

	std::string name;
	std::unique_ptr<Expression> expression;
};
class TypeDeclaration
{
public:
	TypeDeclaration() {}
	TypeDeclaration(std::string name, Type type);
	TypeDeclaration(std::string name, Type type, std::vector<Type> constraints);
	TypeDeclaration(TypeDeclaration && binding);

	std::string name;
	Type type;
	std::vector<Type> constraints;
};

class Instance
{
public:
	Instance() { }

	Instance(Instance && other)
		: className(std::move(other.className))
		, type(std::move(other.type))
		, bindings(std::move(other.bindings))
	{}

	std::string className;
	Type type;
	std::vector<Binding> bindings;
};

class Class
{
public:
	Class() { }

	Class(Class && other)
		: name(std::move(other.name))
		, variable(std::move(other.variable))
		, instances(std::move(other.instances))
		, declarations(std::move(other.declarations))
	{}


	std::string name;
	TypeVariable variable;
	std::vector<Instance> instances;
	std::map<std::string, TypeDeclaration> declarations;
};

class Module
{
public:
	Module();
	Module(std::vector<Binding> && bindings, std::vector<TypeDeclaration> && typeDeclaration);
	Module(Module && other);

	void typecheck();

	std::vector<Binding> bindings;
	std::vector<TypeDeclaration> typeDeclaration;
	std::vector<DataDefinition> dataDefinitions;
	std::vector<Class> classes;
	std::vector<Instance> instances;

	std::vector<std::shared_ptr<Module>> imports;

	static const std::shared_ptr<Module> prelude;
};

}