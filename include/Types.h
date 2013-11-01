#pragma once
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
        return 0;
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

	virtual const std::string& toString() const
	{
		return name;
	}

	virtual Type* copy() const;

	virtual bool isCompatibleWith(const Type& other) const
	{
		return type == other.type && name == other.name;
	}

	virtual bool operator==(const Type& other) const
	{
		return type == other.type && name == other.name;
	}
	bool operator!=(const Type& other) const
	{
		return !(*this == other);
	}

	TypeEnum type;
protected:
	std::string name;
};

class RecursiveType : public Type
{
public:
	RecursiveType()
		: Type(TypeEnum::TYPE_METHOD)
	{}

	virtual const Type& getArgumentType() const = 0;
	virtual const Type& getReturnType() const = 0;
	virtual RecursiveType* copy() const = 0;
};

class PolymorphicType : public RecursiveType
{
public:

	virtual const Type& getArgumentType() const;
	virtual const Type& getReturnType() const;


	virtual bool isCompatibleWith(const Type& other) const;

	virtual PolymorphicType* copy() const;

	static const PolymorphicType any;
};



class FunctionType : public RecursiveType
{
public:

	FunctionType(std::shared_ptr<Type> && argumentType, std::shared_ptr<Type> && returnType)
		: argumentType(std::move(argumentType))
		, returnType(std::move(returnType))
		, typeNameCorrect(false)
	{
		assert(this->argumentType != nullptr);
		assert(this->returnType != nullptr);
	}
	FunctionType(FunctionType&& other)
		: argumentType(std::move(other.argumentType))
		, returnType(std::move(other.returnType))
		, typeNameCorrect(other.typeNameCorrect)
	{}

	virtual const Type& getArgumentType() const
	{
		return *argumentType;
	}
	virtual const Type& getReturnType() const
	{
		return *returnType;
	}

	virtual const std::string& toString() const
	{
		if (!typeNameCorrect)
		{
			if (typeid(*argumentType) == typeid(FunctionType))
				functionName = "(" + argumentType->toString() + ") -> " + returnType->toString();
			else
				functionName = argumentType->toString() + " -> " + returnType->toString();
		}
		return functionName;
	}

	virtual bool isCompatibleWith(const Type& other) const;

	virtual FunctionType* copy() const;

	virtual bool operator==(const Type& o) const
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

	static std::unique_ptr<FunctionType> create(const std::vector<const Type*>& types);
private:
	std::shared_ptr<Type> argumentType;
	std::shared_ptr<Type> returnType;
	mutable std::string functionName;
	bool typeNameCorrect;
};

class TypeError : public std::runtime_error
{
public:
	TypeError(const Type& expected, const Type& actual);
	TypeError(const std::string& expected, const Type& actual);
};

class Object;

union StackObject
{
	VMInt intValue;
	VMFloat floatValue;
	Object* pointerValue;
};


template<class T>
inline T& getObject(StackObject& o);

template<>
inline VMInt& getObject(StackObject& o)
{
	return o.intValue;
}
template<>
inline VMFloat& getObject(StackObject& o)
{
	return o.floatValue;
}
template<>
inline Object*& getObject(StackObject& o)
{
	return o.pointerValue;
}

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
