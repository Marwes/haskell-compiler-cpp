#include <sstream>
#include "Types.h"

namespace MyVMNamespace
{

int TypeVariable::nextId;


TypeOperator functionType(const Type& arg, const Type& result)
{
	return TypeOperator("->", { arg, result });
}

std::string createTypeErrorString(const Type& expected, const Type& actual)
{
	std::stringstream err("Types are not compatible in PrimOP expression.\n");
	err << "Expected: " << expected << "\n";
	err << "Actual: " << actual << "\n";
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
	err << "Actual: " << actual << "\n";
	return err.str();
}


TypeError::TypeError(const std::string& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual))
{
}
}