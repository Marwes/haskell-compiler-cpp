#pragma once
#include <assert.h>
#include <vector>
#include <memory>
#include <string>

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


class Object
{
public:
    Object() : refCount(1) { }
    virtual ~Object() { }

    void addReference()
    {
        ++refCount;
    }
    bool removeReference()
    {
        if (--refCount <= 0)
        {
            delete this;
            return true;
        }
        return false;
    }
private:
    size_t refCount;
};

union StackObject
{
    VMInt intValue;
    VMFloat floatValue;
    Object* pointerValue;
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


class VMField : public Object
{
public:
    VMField(Type type, int offset)
        : type(type)
        , offset(offset)
    {}

    const Type type;
    const int offset;
};

class String : public Object
{
public:
    String(std::string value)
        : value(std::move(value))
    { }
    std::string value;
};


}
