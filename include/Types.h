#pragma once
#include <assert.h>
#include <vector>
#include <memory>
#include <string>


template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args ... args)
{
	return std::unique_ptr<T>(new T(args...));
}

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

class Type
{
public:
    Type(TypeEnum e)
        : type(e)
	{}
	Type(std::string name, TypeEnum e)
		: type(e)
		, name(std::move(name))
	{}
	Type(Type && other)
		: type(other.type)
		, name(std::move(other.name))
	{
	}

	virtual ~Type()
	{
	}

	virtual const std::string& toString()
	{
		return name;
	}

	virtual bool operator==(const Type& other)
	{
		return type == other.type && name == other.name;
	}
	bool operator!=(const Type& other)
	{
		return !(*this == other);
	}

	static const Type any;

	TypeEnum type;
protected:
	std::string name;
};

class FunctionType : public Type
{
public:
	FunctionType(std::unique_ptr<Type>&& argumentType, std::unique_ptr<Type>&& returnType)
		: Type(TypeEnum::TYPE_METHOD)
		, argumentType(std::move(argumentType))
		, returnType(std::move(returnType))
		, typeNameCorrect(false)
	{}
	FunctionType(FunctionType&& other)
		: Type(other)
		, argumentType(std::move(other.argumentType))
		, returnType(std::move(other.returnType))
		, typeNameCorrect(other.typeNameCorrect)
	{}

	virtual const std::string& toString()
	{
		if (!typeNameCorrect)
		{
			if (typeid(*argumentType) == typeid(FunctionType))
				name = "(" + argumentType->toString() + ") -> " + returnType->toString();
			else
				name = argumentType->toString() + " -> " + returnType->toString();
		}
		return Type::toString();
	}

	virtual bool operator==(const Type& o)
	{
		if (typeid(*this) != typeid(o))
		{
			return false;
		}
		const FunctionType& other = static_cast<const FunctionType&>(o);
		bool equal = (argumentType != nullptr && other.argumentType != nullptr && *argumentType == *other.argumentType)
			|| argumentType == other.argumentType;
		if (!equal)
			return false;
		return (returnType != nullptr && other.argumentType != nullptr && *argumentType == *other.argumentType)
			|| argumentType == other.argumentType;
	}

private:
	std::unique_ptr<Type> argumentType;
	std::unique_ptr<Type> returnType;
	bool typeNameCorrect;
};

class Object;

union StackObject
{
	VMInt intValue;
	VMFloat floatValue;
	Object* pointerValue;
};

class Object
{
public:
    Object(int type = 0)
		: refCount(0)
		, type(type)
	{ }
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

	int getType()
	{
		return type;
	}

	StackObject& getField(int index)
	{
		StackObject* first = reinterpret_cast<StackObject*>(this + 1);
		return *(first + index);
	}

private:
	int type;
    int refCount;
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
    VMField(Type&& type, int offset)
        : type(std::move(type))
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
