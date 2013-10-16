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

TypeDeclaration::TypeDeclaration(std::string name, std::unique_ptr<Type> type)
	: name(std::move(name))
	, type(std::move(type))
{
}
TypeDeclaration::TypeDeclaration(TypeDeclaration && other)
	: name(std::move(other.name))
	, type(std::move(other.type))
{
}

}