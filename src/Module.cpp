#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"
#include "Module.h"
#include "Typecheck.h"

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
	, type(std::move(other.type))
{
}

TypeDeclaration::TypeDeclaration(std::string name, Type type)
	: name(std::move(name))
	, type(std::move(type))
{
}
TypeDeclaration::TypeDeclaration(std::string name, Type type, std::vector<TypeOperator> constraints)
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
		Constructor ctor2("[]", listType, 1, 0);
		DataDefinition def;
		def.name = "[]";
		def.constructors.push_back(ctor);
		def.constructors.push_back(ctor2);
		prelude->dataDefinitions.push_back(def);
	}
	prelude->bindings.push_back(Binding("undefined", std::unique_ptr<Expression>(new Name("undefined"))));
	prelude->bindings.push_back(Binding("primIntEq", std::unique_ptr<Expression>(new Name("undefined"))));
	{
		TypeVariable var;
		prelude->bindings.back().expression->getType() = functionType(var, functionType(var, TypeOperator("Bool")));
	}
	{
		prelude->bindings.push_back(Binding("primIntAdd", std::unique_ptr<Expression>(new Name("undefined"))));
		TypeVariable varIntAdd;
		prelude->bindings.back().expression->getType() = functionType(varIntAdd, functionType(varIntAdd, varIntAdd));
	}
	{
		prelude->bindings.push_back(Binding("fromInteger", std::unique_ptr<Expression>(new Name("undefined"))));
		TypeVariable var;
		prelude->bindings.back().expression->getType() = functionType(TypeOperator("Int"), var);
	}
	return prelude;
}
const std::shared_ptr<Module> Module::prelude(createPrelude());

Module::Module()
{
	imports.push_back(prelude);
}

Module::Module(std::vector<Binding> bindings, std::vector<TypeDeclaration> typeDeclaration)
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

Class* getClass(Module& module, const std::string& name)
{
	for (Class& klass : module.classes)
	{
		if (klass.name == name)
			return &klass;
	}
	for (auto& m : module.imports)
	{
		if (Class* klass = getClass(*m, name))
		{
			return klass;
		}
	}
	return nullptr;
}

TypeEnvironment Module::typecheck()
{
	TypeEnvironment env(this);
	Graph graph;

	for (Class& klass : classes)
	{
		env.addConstraint(klass.variable, klass.name);
	}

	for (auto& instance : instances)
	{
		Class* klass = getClass(*this, instance.className);
		assert(klass != nullptr);
		//Type newType = klass->declarations[bind.name].type;
		for (Binding& bind : instance.bindings)
		{
			bind.expression->getType() = klass->declarations[bind.name].type;
		}
		addBindingsToGraph(graph, instance.bindings);
	}

	addBindingsToGraph(graph, bindings);
	typecheckDependecyGraph(env, graph);
	return env;
}

}