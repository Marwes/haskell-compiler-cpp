#pragma once
#include <map>
#include "Types.h"

using namespace MyVMNamespace;

//Check if the types are the same, ignoring which type variable they are
inline bool sameTypes(std::map<int, int>& idmap, const Type& lhs, const Type& rhs)
{
	if (lhs.which() == 0 && rhs.which() == 0)
	{
		auto& l = boost::get<TypeVariable>(lhs);
		auto& r = boost::get<TypeVariable>(rhs);
		if (idmap.count(l.id) > 0)
			return idmap[l.id] == r.id;
		idmap[l.id] = r.id;
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
	std::map<int, int> idmap;
	return sameTypes(idmap, lhs, rhs);
}
