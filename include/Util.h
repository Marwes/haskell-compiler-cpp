#pragma once
#include <vector>
#include <Instruction.h>

namespace MyVMNamespace
{

Assembly readAssemblyFile(const char* filename);

}


// expansion macro for enum value definition
#define ENUM_VALUE(name) name,

// expansion macro for enum to string conversion
#define ENUM_CASE(name) case name: return #name;

// expansion macro for string to enum conversion
#define ENUM_STRCMP(name) if (!strcmp(str,#name)) return name;

/// declare the access function and define enum values
#define DECLARE_ENUM(EnumType,ENUM_DEF) \
enum EnumType { \
\
	\
	ENUM_DEF(ENUM_VALUE) \
}; \
	const char *enumToString(EnumType e); \
	EnumType GetEnumValue(const char *string); \

	/// define the access function names
#define DEFINE_ENUM(EnumType,ENUM_DEF) \
	const char *enumToString(EnumType value) \
{ \
	switch (value) \
{ \
	ENUM_DEF(ENUM_CASE) \
	default: return ""; /* handle input error */ \
} \
} \
	EnumType GetEnumValue(const char *str) \
{ \
	ENUM_DEF(ENUM_STRCMP) \
	return (EnumType) 0; /* handle input error */ \
}