#include <sstream>
#include "Types.h"
#include "Expression.h"

namespace MyVMNamespace
{

int TypeVariable::nextId;
std::ostream& operator<<(std::ostream& str, const TypeVariable& x)
{
	return str << x.id;
}
std::ostream& operator<<(std::ostream& str, const TypeOperator& x)
{
	if (!x.types.empty())
	{
		str << "(";
	}
	str << x.name;
	if (!x.types.empty())
	{
		for (auto& type : x.types)
		{
			str << " " << type;
		}
	}
	if (!x.types.empty())
	{
		str << ")";
	}
	return str;
}

Type functionType(const Type& arg, const Type& result)
{
	return TypeOperator("->", { arg, result });
}

struct TypeToString : boost::static_visitor<>
{
	TypeToString(TypeEnvironment* env, const Type& type)
		: type(type)
		, env(env)
		, out(nullptr)
	{}

	void operator()(const TypeVariable& var) const
	{
		if (env != nullptr)
		{
			auto& constraints = env->getConstraints(var);
			if (!constraints.empty())
			{
				*out << "(";
			}
			for (const std::string& constraint : constraints)
			{
				*out << constraint;
			}
			if (!constraints.empty())
			{
				*out << ") => ";
			}
		}

		*out << var.id;
	}

	void operator()(const TypeOperator& x) const
	{
		std::ostream& str = *out;
		if (!x.types.empty())
		{
			str << "(";
		}
		str << x.name;
		if (!x.types.empty())
		{
			for (auto& type : x.types)
			{
				str << " ";
				boost::apply_visitor(*this, type);
			}
		}
		if (!x.types.empty())
		{
			str << ")";
		}
	}

	mutable std::ostream* out;
	const Type& type;
	TypeEnvironment* env;
};
std::ostream& operator<<(std::ostream& str, const TypeToString& x)
{
	x.out = &str;
	boost::apply_visitor(x, x.type);
	return str;
}

std::ostream& toString(std::ostream& stream, TypeEnvironment& env, const Type& type)
{

	return stream;
}

std::string createTypeErrorString(const Type& expected, const Type& actual, TypeEnvironment* env)
{
	std::stringstream err;
	err << "Types are not compatible.\n";
	err << "Expected: " << TypeToString(env, expected) << "\n";
	err << "Actual: " << TypeToString(env, actual) << "\n";
	return err.str();
}


TypeError::TypeError(TypeEnvironment& env, const Type& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual, &env))
{

}

TypeError::TypeError(const Type& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual, nullptr))
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


TypeError::TypeError(const std::string& errorMessage)
	: std::runtime_error(errorMessage)
{

}

std::string createConstraintError(TypeVariable var, const std::string& klass)
{
	std::stringstream err;
	err << "Variable " << var << " can't be added the constraint for " << klass;
	return err.str();
}

CannotAddConstraintError::CannotAddConstraintError(TypeVariable var, const std::string& klass)
	: TypeError(createConstraintError(var, klass))
{

}
}