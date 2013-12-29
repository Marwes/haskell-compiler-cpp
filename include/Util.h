#pragma once
#include <iosfwd>
#include <string.h>

namespace MyVMNamespace
{

struct Location
{
	Location()
		: column(-1)
		, row(-1)
	{}

	int column, row, absolute;
};

std::ostream& operator<<(std::ostream& out, const Location& loc);


}


// expansion macro for enum value definition
#define ENUM_VALUE(type, name) name,

// expansion macro for enum to string conversion
#define ENUM_CASE(type, name) case type::name: return #name;

// expansion macro for string to enum conversion
#define ENUM_STRCMP(type, name) if (!strcmp(str,#name)) return type::name;

/// declare the access function and define enum values
#define DECLARE_ENUM(EnumType,ENUM_DEF) \
enum class EnumType { \
\
	\
	ENUM_DEF(EnumType,ENUM_VALUE) \
}; \
	const char *enumToString(EnumType e); \
	EnumType EnumType##fromString(const char *string); \

	/// define the access function names
#define DEFINE_ENUM(EnumType,ENUM_DEF) \
	const char *enumToString(EnumType value) \
{ \
	switch (value) \
{ \
	ENUM_DEF(EnumType,ENUM_CASE) \
	default: return ""; /* handle input error */ \
} \
} \
	EnumType EnumType##fromString(const char *str) \
{ \
	ENUM_DEF(EnumType,ENUM_STRCMP) \
	return (EnumType) 0; /* handle input error */ \
}
