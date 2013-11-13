#pragma once
#include <boost/variant.hpp>
#include <iosfwd>
#include <assert.h>
#include <vector>
#include <memory>
#include <string>
#include <stdexcept>


template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args ... args)
{
	return std::unique_ptr<T>(new T(args...));
}

namespace MyVMNamespace
{

class TypeOperator;

class TypeVariable
{
public:
	TypeVariable()
		: id(nextId++)
	{
	}

	int id;
private:
	static int nextId;
};

inline bool operator==(const TypeVariable& l, const TypeVariable& r) { return l.id == r.id; }
inline bool operator!=(const TypeVariable& l, const TypeVariable& r) { return !(l == r); }

typedef boost::variant<TypeVariable, TypeOperator> Type;

std::ostream& operator<<(std::ostream& s, const TypeVariable& type);
std::ostream& operator<<(std::ostream& str, const TypeOperator& type);


class TypeOperator
{
public:
	TypeOperator(std::string name)
		: name(std::move(name))
	{}
	TypeOperator(std::string name, std::vector<Type> types)
		: name(std::move(name))
		, types(std::move(types))
	{
	}

	TypeOperator(const TypeOperator& op)
		: name(op.name)
		, types(op.types.size())
	{
		for (size_t ii = 0; ii < op.types.size(); ++ii)
		{
			types[ii] = op.types[ii];
		}
	}

	TypeOperator& operator=(const TypeOperator& op)
	{
		name = op.name;
		types.resize(op.types.size());
		for (size_t ii = 0; ii < op.types.size(); ++ii)
		{
			types[ii] = op.types[ii];
		}
		return *this;
	}

	std::string name;
	std::vector<Type> types;
};
inline bool operator==(const TypeOperator& l, const TypeOperator& r) { return l.name == r.name && l.types == r.types; }
inline bool operator!=(const TypeOperator& l, const TypeOperator& r) { return !(l == r); }

Type functionType(const Type& arg, const Type& result);

class TypeError : public std::runtime_error
{
public:
	TypeError(const Type& expected, const Type& actual);
	TypeError(const std::string& expected, const Type& actual);
};

}
