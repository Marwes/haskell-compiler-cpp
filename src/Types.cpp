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

}