#pragma once
#include <map>
#include "Types.h"

using namespace MyVMNamespace;

//Check if the types are the same, ignoring which type variable they are
inline bool sameTypes(std::map<TypeVariable, TypeVariable>& idmap, const Type& lhs, const Type& rhs)
{
	if (lhs.which() == 0 && rhs.which() == 0)
	{
		auto& l = boost::get<TypeVariable>(lhs);
		auto& r = boost::get<TypeVariable>(rhs);
		if (idmap.count(l) > 0)
			return idmap[l] == r;
		idmap[l] = r;
		return true;
	}
	else if (lhs.which() == rhs.which())
	{
		auto& l = boost::get<TypeOperator>(lhs);
		auto& r = boost::get<TypeOperator>(rhs);
		if (l.name != r.name || l.types.size() != r.types.size())
			return false;

		for (size_t ii = 0; ii < l.types.size(); ii++)
		{
			if (!sameTypes(idmap, l.types[ii], r.types[ii]))
				return false;
		}
		return true;
	}
	return false;
}
inline bool sameTypes(const Type& lhs, const Type& rhs)
{
	std::map<TypeVariable, TypeVariable> idmap;
	return sameTypes(idmap, lhs, rhs);
}

namespace MyVMNamespace
{
	//Use operator> as alias to sameTypes to get more readable output from Catch
	inline bool operator>(const Type& lhs, const Type& rhs)
	{
		return sameTypes(lhs, rhs);
	}
}
