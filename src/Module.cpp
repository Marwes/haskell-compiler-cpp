#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"
#include "Module.h"
#include "Typecheck.h"
#include "Compiler.h"

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

Module::Module()
{
	imports.push_back("Prelude");
}

Module::Module(std::vector<Binding> bindings, std::vector<TypeDeclaration> typeDeclaration)
	: bindings(std::move(bindings))
	, typeDeclaration(std::move(typeDeclaration))
{
	imports.push_back("Prelude");
}

Module::Module(Module && other)
	: name(std::move(other.name))
	, bindings(std::move(other.bindings))
	, typeDeclaration(std::move(other.typeDeclaration))
	, dataDefinitions(std::move(other.dataDefinitions))
	, classes(std::move(other.classes))
	, instances(std::move(other.instances))
	, imports(std::move(other.imports))
{
}

Class* getClass(Assembly& module, const std::string& name)
{
	assert(0 && "Need to store classes in assembly");
	return nullptr;
}

Class* getClass(TypeEnvironment& env, Module& module, const std::string& name)
{
	for (Class& klass : module.classes)
	{
		if (klass.name == name)
			return &klass;
	}
	for (auto& m : module.imports)
	{
		Assembly* assembly = env.getAssembly(m);
		if (assembly != nullptr)
		{
			if (Class* klass = getClass(*assembly, name))
			{
				return klass;
			}
		}
	}
	return nullptr;
}

TypeEnvironment Module::typecheck(std::map<std::string, Assembly*> assemblies)
{
	for (const std::string& import : imports)
	{
		if (import == "Prelude")
		{
			assemblies.insert(std::make_pair("Prelude", &Assembly::prelude));
		}
		else
		{
			assert(0 && "import modules");
		}
	}
	TypeEnvironment env(this, assemblies);
	Graph graph;

	for (Class& klass : classes)
	{
		env.addConstraint(klass.variable, klass.name);
	}

	for (auto& instance : instances)
	{
		Class* klass = getClass(env, *this, instance.className);
		assert(klass != nullptr);
		//Type newType = klass->declarations[bind.name].type;
		for (Binding& bind : instance.bindings)
		{
			std::string decoded = decodeBindingName(boost::get<TypeOperator>(instance.type).name, bind.name);
			bind.expression->getType() = klass->declarations[decoded].type;
			bind.type.type = bind.expression->getType();
		}
		addBindingsToGraph(graph, instance.bindings);
	}

	addBindingsToGraph(graph, bindings);
	typecheckDependecyGraph(env, graph);
	return env;
}

}