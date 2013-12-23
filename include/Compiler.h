#pragma once
#include <string>
#include "SuperCombinator.h"
#include "Types.h"
#include "Module.h"

namespace MyVMNamespace
{
class Module;

enum class VariableType
{
	NONE,
	STACK,
	TOPLEVEL,
	CONSTRUCTOR,
	TYPECLASSFUNCTION
};

class Binding;
class Class;
class Instance;

struct Variable
{
	VariableType accessType;
	int index;
	Class* klass;
	Type* type;
};

struct InstanceDictionary
{
	std::vector<TypeOperator> constraints;
	std::vector<SuperCombinator*> dictionary;
};

class Assembly
{
public:
	Assembly() {}
	Assembly(Assembly && o)
		: superCombinators(std::move(o.superCombinators))
		, dataDefinitions(std::move(o.dataDefinitions))
		, instanceDictionaries(std::move(o.instanceDictionaries))
		, globalIndices(std::move(o.globalIndices))
		, instanceIndices(std::move(o.instanceIndices))
		, instances(std::move(o.instances))
		, classes(std::move(o.classes))
	{}

	Assembly& operator=(Assembly && o)
	{
		superCombinators = std::move(o.superCombinators);
		dataDefinitions = std::move(o.dataDefinitions);
		instanceDictionaries = std::move(o.instanceDictionaries);
		globalIndices = std::move(o.globalIndices);
		instanceIndices = std::move(o.instanceIndices);
		return *this;
	}

	std::map<std::string, std::unique_ptr<SuperCombinator>> superCombinators;
	std::vector<Constructor> dataDefinitions;
	std::vector<InstanceDictionary> instanceDictionaries;
	std::map<SuperCombinator*, int> globalIndices;
	std::map<std::vector<TypeOperator>, int> instanceIndices;
	std::vector<TypeOperator> instances;
	std::vector<Class> classes;

	static Assembly prelude;
};

class GCompiler
{
public:
	GCompiler(TypeEnvironment& typeEnv, Module* module, int globalStartIndex = 0, std::map<std::string, Assembly*> assemblies = std::map<std::string, Assembly*>());

	void newStackVariable(const std::string& name);
	void popStack(size_t n);
	Variable getVariable(const std::string& name);
	SuperCombinator& getGlobal(const std::string& name);

	//Returns the global index for the dictionary defined for the constraints in the constraints argument
	int getDictionaryIndex(const std::vector<TypeOperator>& constraints);
	//Returns the index that retrives the function from the dictionary which is specified by 'function'
	int getInstanceDictionaryIndex(const std::string& function) const;

	SuperCombinator& compileBinding(Binding& binding, const std::string& name);
	void compileInstance(Instance& instance);
	Assembly compileExpression(Expression& expr, const std::string& name, bool strict);

	Assembly compileModule(Module& module);

	const Binding& getCurrentBinding() const;

	Assembly* getAssembly(const std::string& name) const
	{
		auto found = assemblies.find(name);
		return found == assemblies.end() ? nullptr : found->second;
	}

	std::vector<std::string> stackVariables;

	TypeEnvironment& typeEnv;
private:
	//Keys are the class name which gives a map with keys which are the type which the dictionary is for
	//The last vector contains pointers to the functions which are defined for that type
	std::map<std::string, std::map<Type, std::vector<SuperCombinator*>>> classDictionaries;
	//The module in which to lookup other functions from
	Module* module;
	//A number used to always get a unique index for each globalQ
	int uniqueGlobalIndex;
	//The binding which is currently being compiled
	Binding* currentBinding;
	Assembly* assembly;
	std::map<std::string, Assembly*> assemblies;
};

}
