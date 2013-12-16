#include <algorithm>
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{
GCompiler::GCompiler(TypeEnvironment& typeEnv, Module* module)
	: module(module)
	, index(0)
	, typeEnv(typeEnv)
{
}

void GCompiler::newStackVariable(const std::string& name)
{
	stackVariables.push_back(name);
}
void GCompiler::popStack(size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		stackVariables.pop_back();
	}
}

Variable findInModule(GCompiler& comp, Module& module, const std::string& name)
{
	for (Binding& bind : module.bindings)
	{
		if (bind.name == name)
		{
			size_t index = comp.globalIndices[&comp.getGlobal(name)];
			return Variable { VariableType::TOPLEVEL, index, nullptr };
		}
	}

	for (auto& dataDef : module.dataDefinitions)
	{
		for (auto& ctor : dataDef.constructors)
		{
			if (ctor.name == name)
			{
				//TODO ctor.tag must be a way to retrieve this constructor
				if (ctor.tag > (1 << 16))
				{
					throw std::runtime_error("Number of constructors are to large");
				}if (ctor.arity > (1 << 16))
				{
					throw std::runtime_error("Arity of constructor " + std::string(ctor.name) + " are to large");
				}
				int index = (ctor.arity << 16) | ctor.tag;
				return Variable { VariableType::CONSTRUCTOR, index, nullptr };
			}
		}
	}
	for (Class& klass : module.classes)
	{
		size_t index = 0;
		for (auto& decl : klass.declarations)
		{
			if (decl.second.name == name)
			{
				return Variable { VariableType::TYPECLASSFUNCTION, index, &klass };
			}
			index++;
		}
	}

	for (auto& import : module.imports)
	{
		Variable ret = findInModule(comp, *import, name);
		if (ret.index >= 0)
			return ret;
	}
	return Variable { VariableType::NONE, -1, nullptr };
}

Variable GCompiler::getVariable(const std::string& name)
{
	auto found = std::find(stackVariables.begin(), stackVariables.end(), name);
	if (found != stackVariables.end())
	{
		int index = std::distance(stackVariables.begin(), found);
		int distanceFromStackTop = stackVariables.size() - index - 1;
		return Variable { VariableType::STACK, index, nullptr };
	}
	auto foundGlobal = globals.find(name);
	if (foundGlobal != globals.end())
	{
		int i = globalIndices[foundGlobal->second.get()];
		return Variable { VariableType::TOPLEVEL, i, nullptr };
	}
	if (module != nullptr)
	{
		return findInModule(*this, *module, name);
	}
	return Variable { VariableType::NONE, -1, nullptr };
}

SuperCombinator& GCompiler::getGlobal(const std::string& name)
{
	auto found = globals.find(name);
	if (found == globals.end())
	{
		auto& ptr = globals[name] = std::unique_ptr<SuperCombinator>(new SuperCombinator());
		ptr->name = name;
		globalIndices[ptr.get()] = index++;
		return *ptr;
	}
	return *found->second;
}


int GCompiler::getDictionaryIndex(const std::vector<TypeOperator>& constraints)
{
	auto found = instanceIndices.find(constraints);
	if (found != instanceIndices.end())
	{
		return found->second;
	}
	//Add a new dictionary
	std::vector<SuperCombinator*> dict;
	for (const TypeOperator& op : constraints)
	{
		std::map<Type, std::vector<SuperCombinator*>>& klass = classes[op.name];
		std::vector<SuperCombinator*>& instanceFunctions = klass[op.types[0]];
		for (SuperCombinator* comb : instanceFunctions)
		{
			dict.push_back(comb);
		}
	}
	instanceDicionaries.push_back(InstanceDictionary { constraints, std::move(dict) });
	return instanceIndices[instanceDicionaries.back().constraints] = index++;
}

int GCompiler::getInstanceDictionaryIndex(const std::string& function) const
{
	for (const TypeOperator& op : currentBinding->type.constraints)
	{
		auto klass = classes.find(op.name);
		int ii = 0;
		for (SuperCombinator* comb : klass->second.begin()->second)
		{
			//Test if the end of the name is equal to the function
			if (comb->name.size() >= function.size()
				&& std::equal(comb->name.end() - function.size(), comb->name.end(), function.begin()))
				return ii;
			ii++;
		}
	}
	return -1;
}


SuperCombinator& GCompiler::compileBinding(Binding& binding, const std::string& name)
{
	currentBinding = &binding;

	SuperCombinator& sc = getGlobal(name);
	stackVariables.clear();
	if (Lambda* lambda = dynamic_cast<Lambda*>(binding.expression.get()))
	{
		sc.arity = lambda->arguments.size();
		if (!binding.type.constraints.empty())
		{
			sc.arity++;
			newStackVariable("$dict");
		}
		for (auto arg = lambda->arguments.rbegin(); arg != lambda->arguments.rend(); ++arg)
		{
			stackVariables.push_back(*arg);
		}
		lambda->body->compile(*this, sc.instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		sc.instructions.push_back(GInstruction(GOP::POP, sc.arity));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}
	else
	{
		sc.arity = 0;
		binding.expression->compile(*this, sc.instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		//sc.instructions.push_back(GInstruction(GOP::POP, 0));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}

	currentBinding = nullptr;
	return sc;
}

void GCompiler::compileInstance(Instance& instance)
{
	std::vector<SuperCombinator*> functions;
	for (Binding& bind : instance.bindings)
	{
		std::string name = "#" + boost::get<TypeOperator>(instance.type).name + bind.name;
		SuperCombinator& sc = compileBinding(bind, name);
		functions.push_back(&sc);
	}
	auto lowBound = classes.lower_bound(instance.className);
	if (lowBound == classes.end() || classes.key_comp()(instance.className, lowBound->first))
	{
		//Key does not exist
		lowBound = classes.insert(std::make_pair(instance.className, std::map<Type, std::vector<SuperCombinator*>>())).first;
	}
	std::map<Type, std::vector<SuperCombinator*>>& instances = lowBound->second;
	instances.insert(std::make_pair(instance.type, functions));
}
}
