#pragma once
#include <string>
#include <map>
#include <set>
#include "Types.h"

namespace MyVMNamespace
{

class Module;
class Assembly;

class TypeEnvironment
{
public:
	TypeEnvironment(Module* module = nullptr, std::map<std::string, Assembly*> assemblies = std::map<std::string, Assembly*>());
	TypeEnvironment(TypeEnvironment && env);

	TypeEnvironment child();

	TypeVariable newTypeVariable();

	void bindName(const std::string& name, Type& type);
	void registerType(Type& type);

	const Type* getType(const std::string& name) const;
	Type getFreshType(const std::string& name);
	std::vector<TypeOperator> getConstraints(const std::string& name, const Type& functionType) const;

	void addNonGeneric(const Type& type);
	void removeNonGenerics(size_t n);
	bool isGeneric(const TypeVariable& var) const;
	void replace(TypeVariable replaceMe, const Type& replaceWith);
	void tryReplace(Type& toReplace, TypeVariable& replaceMe, const Type& replaceWith);

	void addConstraint(const TypeVariable& var, const std::string& className);
	void updateConstraints(const TypeVariable& oldVar, const TypeVariable& newVar);
	void lockVariable(TypeVariable var);
	bool isVariableLocked(TypeVariable var);
	const std::vector<std::string>& getConstraints(const TypeVariable& var) const;

	Assembly* getAssembly(const std::string& name) const
	{
		auto found = assemblies.find(name);
		return found == assemblies.end() ? nullptr : found->second;
	}
	void addAssembly(const std::string& name, Assembly* assembly)
	{
		assemblies.insert(std::make_pair(name, assembly));
	}
	const std::map<TypeVariable, std::vector<std::string>>& getAllConstraints() const
	{
		return constraints;
	}
	void addConstraints(const std::map<TypeVariable, std::vector<std::string>>& otherConstraints)
	{
		for (auto& pair : otherConstraints)
		{
			std::vector<std::string>& x = constraints[pair.first];
			for (const std::string& constraint : pair.second)
			{
				if (std::find(x.begin(), x.end(), constraint) == x.end())
				{
					x.push_back(constraint);
				}
			}
		}
	}
private:
	Module* module;
	std::map<std::string, Assembly*> assemblies;
	TypeEnvironment* parent;
	std::map<std::string, Type*> namedTypes;
	std::vector<Type*> types;
	std::vector<Type> nonGeneric;
	std::map<TypeVariable, std::vector<std::string>> constraints;
	//Variables which can't have more constraints added to them
	std::set<TypeVariable> lockedVariables;

	int uniqueVariableId;
};


void unify(TypeEnvironment& env, Type& lhs, Type& rhs);

extern Type intType;
extern Type doubleType;

}
