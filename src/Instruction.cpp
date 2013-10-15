#include <Instruction.h>
#include "VM.h"

namespace MyVMNamespace
{

DEFINE_ENUM(OP, OP_ENUM);


int allocatePair(VM& vm, StackFrame& stack)
{
	void* ptr = malloc(sizeof(Object) + sizeof(StackObject) * 2);
	Object* o = new (ptr) Object();
	for (int ii = 0; ii < 2; ++ii)
	{
		StackObject& obj = stack[stack.size() - 2 + ii];
		o->getField(ii) = obj;
	}
	stack.push(o);
	return 0;
}

Assembly::Assembly()
	: entrypoint(0)
{
	nativeFunctionIndexes.insert(std::make_pair("(,)", 0));
	nativeFunctions.push_back(allocatePair);
}

Assembly::Assembly(Assembly && other)
	: functionDefinitionsIndexes(std::move(other.functionDefinitionsIndexes))
	, functionDefinitions(std::move(other.functionDefinitions))
	, entrypoint(other.entrypoint)
{
}

Assembly& Assembly::operator=(Assembly && other)
{
	functionDefinitionsIndexes = std::move(other.functionDefinitionsIndexes);
	functionDefinitions = std::move(other.functionDefinitions);
	entrypoint = other.entrypoint;
	return *this;
}


int Assembly::addFunction(const std::string& name, std::unique_ptr<FunctionDefinition> && def)
{
	int index = functionDefinitionsIndexes.size();
	functionDefinitionsIndexes.insert(std::make_pair(name, index));
	functionDefinitions.push_back(std::move(def));
	return index;
}

FunctionDefinition* Assembly::getFunction(size_t index)
{
	return functionDefinitions[index].get();
}

FunctionDefinition* Assembly::getFunction(const std::string& name)
{
	auto& found = functionDefinitionsIndexes.find(name);
	if (found == functionDefinitionsIndexes.end())
	{
		return nullptr;
	}
	return functionDefinitions[found->second].get();
}

}