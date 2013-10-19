#include <sstream>
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

std::string createTypeErrorString(const Type& expected, const Type& actual)
{
	std::stringstream err("Types are not compatible in PrimOP expression.\n");
	err << "Expected: " << expected.toString() << "\n";
	err << "Actual: " << actual.toString() << "\n";
	return err.str();
}

TypeError::TypeError(const Type& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual))
{
}

}