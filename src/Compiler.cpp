#include <algorithm>
#include "Compiler.h"
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{
GCompiler::GCompiler(TypeEnvironment& typeEnv, Module* module, int globalStartIndex, std::map<std::string, Assembly*> assemblies)
	: module(module)
	, uniqueGlobalIndex(globalStartIndex)
	, typeEnv(typeEnv)
	, currentBinding(nullptr)
	, assembly(nullptr)
	, assemblies(std::move(assemblies))
{
	if (this->assemblies.count("Prelude") == 0)
		this->assemblies.insert(std::make_pair("Prelude", &Module::prelude));
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

Variable findInModule(GCompiler& comp, Assembly& assembly, Assembly& import, const std::string& name)
{
	for (auto& pair : import.superCombinators)
	{
		if (pair.first == name)
		{
			size_t index = import.globalIndices[pair.second.get()];
			return Variable{ VariableType::TOPLEVEL, index, nullptr, &pair.second->type };
		}
	}
	for (Constructor& ctor : import.dataDefinitions)
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
			return Variable{ VariableType::CONSTRUCTOR, index, nullptr };
		}
	}
	return Variable{ VariableType::NONE, -1, nullptr, nullptr };
}

Variable findInModule(GCompiler& comp, Assembly& assembly, Module& module, const std::string& name)
{
	for (Binding& bind : module.bindings)
	{
		if (bind.name == name)
		{
			size_t index = assembly.globalIndices[&comp.getGlobal(name)];
			return Variable { VariableType::TOPLEVEL, index, nullptr, &bind.expression->getType() };
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
	if (name.size() > 0 && name[0] == '#')
	{
		for (Instance& instance : module.instances)
		{
			const std::string& instanceName = boost::get<TypeOperator>(instance.type).name;
			if (name.compare(1, instanceName.size(), instanceName) == 0)
			{
				for (Binding& binding : instance.bindings)
				{
					if (name.compare(instanceName.size() + 1, name.size(), binding.name) == 0)
					{
						return Variable{ VariableType::NONE, -1, nullptr, &binding.expression->getType() };
					}
				}
			}
		}
	}

	for (auto& import : module.imports)
	{
		Assembly* importedAssembly = comp.getAssembly(import);
		if (importedAssembly != nullptr)
		{
			Variable ret = findInModule(comp, assembly, *importedAssembly, name);
			if (ret.index >= 0)
				return ret;
		}
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
	if (assembly != nullptr)
	{
		auto foundGlobal = assembly->superCombinators.find(name);
		if (foundGlobal != assembly->superCombinators.end())
		{
			int i = assembly->globalIndices[foundGlobal->second.get()];
			return Variable{ VariableType::TOPLEVEL, i, nullptr };
		}
	}
	if (module != nullptr)
	{
		return findInModule(*this, *assembly, *module, name);
	}
	return Variable { VariableType::NONE, -1, nullptr };
}

SuperCombinator& GCompiler::getGlobal(const std::string& name)
{
	auto found = assembly->superCombinators.find(name);
	if (found == assembly->superCombinators.end())
	{
		auto& ptr = assembly->superCombinators[name] = std::unique_ptr<SuperCombinator>(new SuperCombinator());
		ptr->name = name;
		assembly->globalIndices[ptr.get()] = uniqueGlobalIndex++;
		return *ptr;
	}
	return *found->second;
}


int GCompiler::getDictionaryIndex(const std::vector<TypeOperator>& constraints)
{
	auto found = assembly->instanceIndices.find(constraints);
	if (found != assembly->instanceIndices.end())
	{
		return found->second;
	}
	//Add a new dictionary
	std::vector<SuperCombinator*> dict;
	for (const TypeOperator& op : constraints)
	{
		std::map<Type, std::vector<SuperCombinator*>>& klass = classDictionaries[op.name];
		std::vector<SuperCombinator*>& instanceFunctions = klass[op.types[0]];
		for (SuperCombinator* comb : instanceFunctions)
		{
			dict.push_back(comb);
		}
	}
	assembly->instanceDictionaries.push_back(InstanceDictionary { constraints, std::move(dict) });
	return assembly->instanceIndices[assembly->instanceDictionaries.back().constraints] = uniqueGlobalIndex++;
}

int GCompiler::getInstanceDictionaryIndex(const std::string& function) const
{
	for (const TypeOperator& op : currentBinding->type.constraints)
	{
		auto klass = classDictionaries.find(op.name);
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

const Binding& GCompiler::getCurrentBinding() const
{
	assert(currentBinding != nullptr);
	return *currentBinding;
}


SuperCombinator& GCompiler::compileBinding(Binding& binding, const std::string& name)
{
	currentBinding = &binding;

	SuperCombinator& sc = getGlobal(name);
	sc.arity = 0;
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
	auto lowBound = classDictionaries.lower_bound(instance.className);
	if (lowBound == classDictionaries.end() || classDictionaries.key_comp()(instance.className, lowBound->first))
	{
		//Key does not exist
		lowBound = classDictionaries.insert(std::make_pair(instance.className, std::map < Type, std::vector < SuperCombinator* >> ())).first;
	}
	std::map<Type, std::vector<SuperCombinator*>>& instances = lowBound->second;
	instances.insert(std::make_pair(instance.type, functions));
}

Assembly GCompiler::compileModule(Module& module)
{
	Assembly result;
	assembly = &result;

	//Assign unique numbers for the tags so they can be correctly retrieved
	for (DataDefinition& dataDef : module.dataDefinitions)
	{
		int tag = 0;
		for (Constructor& ctor : dataDef.constructors)
		{
			ctor.tag = tag++;
			result.dataDefinitions.push_back(ctor);
		}
	}

	for (Instance& instance : module.instances)
	{
		compileInstance(instance);
	}
	for (Binding& bind : module.bindings)
	{
		compileBinding(bind, bind.name);
	}
	for (auto& pair : result.superCombinators)
	{
		const std::string& name = pair.first;
		SuperCombinator& comb = *pair.second;
		const Type* type = findInModule(*this, *assembly, module, name).type;
		comb.type = *type;
	}
	assembly = nullptr;
	return result;
}
}
