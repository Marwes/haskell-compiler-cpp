#pragma once
#include <string>
#include "SuperCombinator.h"
#include "Types.h"

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

class GCompiler
{
public:
	GCompiler(TypeEnvironment& typeEnv, Module* module);

	void newStackVariable(const std::string& name);
	void popStack(size_t n);
	Variable getVariable(const std::string& name);
	SuperCombinator& getGlobal(const std::string& name);

	int getDictionaryIndex(const std::vector<TypeOperator>& dict);
	int getInstanceDictionaryIndex(const std::string& function) const;

	SuperCombinator& compileBinding(Binding& binding, const std::string& name);
	void compileInstance(Instance& instance);

	const Binding& getCurrentBinding() const;

	std::vector<std::string> stackVariables;
	std::map<std::string, std::unique_ptr<SuperCombinator>> globals;
	std::map<SuperCombinator*, int> globalIndices;
	std::map<std::vector<TypeOperator>, int> instanceIndices;
	std::vector<Constructor> dataDefinitions;
	std::vector<InstanceDictionary> instanceDicionaries;

	TypeEnvironment& typeEnv;
private:
	std::map<std::string, std::map<Type, std::vector<SuperCombinator*>>> classes;
	Module* module;
	int index;
	Binding* currentBinding;
};

}
