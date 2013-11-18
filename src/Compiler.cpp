#include <algorithm>
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{
GCompiler::GCompiler(Module* module)
	: module(module)
	, index(0)
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

Variable findInModule(Module& module, const std::string& name)
{
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
				return Variable { VariableType::CONSTRUCTOR, TypeVariable(), index };
			}
		}
	}
	for (auto& import : module.imports)
	{
		Variable ret = findInModule(*import, name);
		if (ret.index >= 0)
			return ret;
	}
	return Variable { VariableType::STACK, TypeVariable(), -1 };
}

Variable GCompiler::getVariable(const std::string& name)
{
	auto found = std::find(stackVariables.begin(), stackVariables.end(), name);
	if (found != stackVariables.end())
	{
		int index = std::distance(stackVariables.begin(), found);
		int distanceFromStackTop = stackVariables.size() - index - 1;
		return Variable { VariableType::STACK, TypeVariable(), index };
	}
	auto foundGlobal = globals.find(name);
	if (foundGlobal != globals.end())
	{
		int i = globalIndices[foundGlobal->second.get()];
		return Variable { VariableType::TOPLEVEL, TypeVariable(), i };
	}
	if (module != nullptr)
	{
		return findInModule(*module, name);
	}
	return Variable { VariableType::STACK, TypeVariable(), -1 };
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

}
