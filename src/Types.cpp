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

Type* Type::copy() const
{
	return new Type(*this);
}


bool PolymorphicType::isCompatibleWith(const Type&) const
{
	return true;
}

PolymorphicType* PolymorphicType::copy() const
{
	return new PolymorphicType(*this);
}

bool FunctionType::isCompatibleWith(const Type& other) const
{
	return *this == other;
}

FunctionType* FunctionType::copy() const
{
	return new FunctionType(*this);
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

std::string createTypeErrorString(const std::string& expected, const Type& actual)
{
	std::stringstream err("Types are not compatible.\n");
	err << expected << "\n";
	err << "Actual: " << actual.toString() << "\n";
	return err.str();
}


TypeError::TypeError(const std::string& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual))
{
}
}