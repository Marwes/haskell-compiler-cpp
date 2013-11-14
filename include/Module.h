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
	TypeDeclaration(std::string name, Type type);
	TypeDeclaration(TypeDeclaration && binding);

	std::string name;
	Type type;
};

class Module
{
public:
	Module()
	{
	}
	Module(Module && other)
		: bindings(std::move(other.bindings))
		, typeDeclaration(std::move(other.typeDeclaration))
		, dataDefinitions(std::move(other.dataDefinitions))
	{
	}
	Module(std::vector<Binding> && bindings, std::vector<TypeDeclaration> && typeDeclaration)
		: bindings(std::move(bindings))
		, typeDeclaration(std::move(typeDeclaration))
	{
	}

	void typecheck();

	std::vector<Binding> bindings;
	std::vector<TypeDeclaration> typeDeclaration;
	std::vector<DataDefinition> dataDefinitions;
};

}