#include <sstream>
#include "Types.h"
#include "Typecheck.h"
#include "Expression.h"
#include "Typecheck.h"

namespace MyVMNamespace
{
int defaultTypeVariableId = -1;
TypeVariable::TypeVariable()
	: id(defaultTypeVariableId--)
{}


TypeVariable::TypeVariable(TypeEnvironment& env)
{
	*this = env.newTypeVariable();
}

Type::Type(TypeEnvironment& env)
	: TypeVariant(env)
{
	setDebugType();
}


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
	TypeToString(std::ostream& out, const TypeEnvironment& env, const Type& type)
		: env(env)
		, out(out)
		, type(type)
		, writtenStart(false)
	{
	}

	void operator()(const TypeVariable& var) const
	{
		auto& constraints = env.getConstraints(var);
		bool writeComma = writtenStart;
		if (!writtenStart && !constraints.empty())
		{
			writtenStart = true;
			out << "(";
		}
		for (const std::string& constraint : constraints)
		{
			if (writeComma)
			{
				out << ", ";
			}
			out << constraint << " " << var;
			writeComma = true;
		}
	}

	void operator()(const TypeOperator& x) const
	{
		for (auto& type : x.types)
		{
			boost::apply_visitor(*this, type);
		}
	}

	const TypeEnvironment& env;
	std::ostream& out;
	const Type& type;
	mutable bool writtenStart;
};

std::ostream& operator<<(std::ostream& str, const TypeToString& x)
{
	boost::apply_visitor(x, x.type);
	if (x.writtenStart)
	{
		str << ") => ";
	}
	return str << x.type;
}

std::string createTypeErrorString(const Type& expected, const Type& actual, TypeEnvironment& env)
{
	std::stringstream err;
	err << "Types are not compatible.\n";
	err << "Expected: " << TypeToString(err, env, expected) << "\n";
	err << "Actual: " << TypeToString(err, env, actual) << "\n";
	return err.str();
}


TypeError::TypeError(TypeEnvironment& env, const Type& expected, const Type& actual)
	: std::runtime_error(createTypeErrorString(expected, actual, env))
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