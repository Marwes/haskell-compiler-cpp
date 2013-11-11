#include <sstream>
#include "Types.h"

namespace MyVMNamespace
{

int TypeVariable::nextId;
std::ostream& operator<<(std::ostream& str, const TypeVariable& x)
{
	return str << x.id;
}
std::ostream& operator<<(std::ostream& str, const TypeOperator& x)
{
	str << x.name << " { ";
	for (const Type& type : x.types)
	{
		str << type;
		if (&type != &x.types.back())
			str << ", ";
	}
	return str << " }";
}

Type functionType(const Type& arg, const Type& result)
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