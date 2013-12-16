#pragma once
#include <string>
#include <map>
#include <set>
#include "Types.h"

namespace MyVMNamespace
{

class Module;

class TypeEnvironment
{
public:
	TypeEnvironment(Module* module);
	TypeEnvironment(TypeEnvironment && env);

	TypeEnvironment child();

	void bindName(const std::string& name, Type& type);
	void registerType(Type& type);

	const Type& getType(const std::string& name) const;
	Type getFreshType(const std::string& name);
	std::vector<TypeOperator> getConstraints(const std::string& name, const Type& functionType) const;

	void addNonGeneric(const Type& type);
	bool isGeneric(const TypeVariable& var) const;
	void replace(TypeVariable replaceMe, const Type& replaceWith);
	void tryReplace(Type& toReplace, TypeVariable& replaceMe, const Type& replaceWith);

	void addConstraint(const TypeVariable& var, const std::string& className);
	void updateConstraints(const TypeVariable& oldVar, const TypeVariable& newVar);
	void lockVariable(TypeVariable var);
	bool isVariableLocked(TypeVariable var);
	const std::vector<std::string>& getConstraints(const TypeVariable& var) const;
private:
	Module* module;
	TypeEnvironment* parent;
	std::map<std::string, Type*> namedTypes;
	std::vector<Type*> types;
	std::vector<Type> nonGeneric;
	std::map<TypeVariable, std::vector<std::string>> constraints;
	//Variables which can't have more constraints added to them
	std::set<TypeVariable> lockedVariables;
};


void unify(TypeEnvironment& env, Type& lhs, Type& rhs);

extern Type intType;
extern Type doubleType;

}