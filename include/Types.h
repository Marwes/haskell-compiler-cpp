#pragma once
#include <assert.h>
#include <vector>

namespace MyVMNamespace
{

typedef int VMInt;
typedef float VMFloat;
typedef void* VMPointer;

enum TypeEnum
{
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_CHAR,
    TYPE_ARRAY,
    TYPE_CLASS,
    TYPE_METHOD,

};
inline size_t sizeofType(TypeEnum  e)
{
    switch (e)
    {
    case TYPE_INT:
        return sizeof(VMInt);
        break;
    case TYPE_FLOAT:
        return sizeof(VMFloat);
        break;
    case TYPE_CHAR:
        return sizeof(char);
        break;
    case TYPE_ARRAY:
    case TYPE_CLASS:
        return sizeof(VMPointer);
        break;
    default:
        assert(0);
        return -1;
        break;
    }
}

struct Type
{
    Type(TypeEnum e)
        : type(e)
    {}
    TypeEnum type;
};


union StackObject
{
    VMInt intValue;
    VMFloat floatValue;
    VMPointer pointerValue;
};

class Data
{
public:
    virtual ~Data() { }
};

template<class T, class U>
inline T data_cast(U&& v)
{
#ifdef DEBUG
    return dynamic_cast<T>(v);
#else
    return static_cast<T>(v);
#endif
}

class VMField : public Data
{
public:
    VMField(Type type, int offset)
        : type(type)
        , offset(offset)
    {}

    const Type type;
    const int offset;
};


}
