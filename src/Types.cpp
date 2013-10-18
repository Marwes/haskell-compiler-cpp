#include "Types.h"

namespace MyVMNamespace
{
const PolymorphicType PolymorphicType::any;

const Type& PolymorphicType::getArgumentType() const
{
	return *this;
}
const Type& PolymorphicType::getReturnType() const
{
	return *this;
}


bool PolymorphicType::isCompatibleWith(const Type&) const
{
	return true;
}


bool FunctionType::isCompatibleWith(const Type& other) const
{
	return *this == other;
}

}