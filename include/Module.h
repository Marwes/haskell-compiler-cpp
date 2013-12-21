#pragma once
#include <vector>
#include <memory>
#include "Expression.h"
#include "Types.h"

namespace MyVMNamespace
{

class TypeDeclaration
{
public:
	TypeDeclaration() {}
	TypeDeclaration(std::string name, Type type);
	TypeDeclaration(std::string name, Type type, std::vector<TypeOperator> constraints);
	TypeDeclaration(TypeDeclaration && binding);

	std::string name;
	Type type;
	std::vector<TypeOperator> constraints;
};

class Binding
{
public:
	Binding(std::string name, std::unique_ptr<Expression> expression);
	Binding(Binding && binding);

	std::string name;
	std::unique_ptr<Expression> expression;
	TypeDeclaration type;
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
	Module(std::vector<Binding> bindings, std::vector<TypeDeclaration> typeDeclaration);
	Module(Module && other);

	TypeEnvironment typecheck(std::map<std::string, Assembly*> assemblies = std::map<std::string, Assembly*>());

	std::vector<Binding> bindings;
	std::vector<TypeDeclaration> typeDeclaration;
	std::vector<DataDefinition> dataDefinitions;
	std::vector<Class> classes;
	std::vector<Instance> instances;

	std::vector<std::string> imports;

	static Assembly prelude;
};

}