#include <Instruction.h>

namespace MyVMNamespace
{

DEFINE_ENUM(OP, OP_ENUM);

Assembly::Assembly() : entrypoint(0) { }

Assembly::Assembly(Assembly && other)
	: functionDefinitionsIndexes(std::move(other.functionDefinitionsIndexes))
	, functionDefinitions(std::move(other.functionDefinitions))
	, entrypoint(other.entrypoint)
{
}

}