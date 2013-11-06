#pragma once
#include <boost/variant.hpp>
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

template<class Stream>
Stream& operator<<(Stream& s, const TypeVariable& type)
{
	return s << type.id;
}

typedef boost::variant<TypeVariable, boost::recursive_wrapper<TypeOperator>> Type;

class TypeOperator
{
public:
	TypeOperator(std::string name, std::vector<Type> types)
		: name(std::move(name))
		, types(std::move(types))
	{}

	std::string name;
	std::vector<Type> types;
};
inline bool operator==(const TypeOperator& l, const TypeOperator& r) { return l.name == r.name && l.types == r.types; }
inline bool operator!=(const TypeOperator& l, const TypeOperator& r) { return !(l == r); }

template<class Stream>
Stream& operator<<(Stream& s, const TypeOperator& type)
{
	s << type.name;
	for (auto& type : type.types)
	{
		s << type;
	}
	return s;
}

Type functionType(const Type& arg, const Type& result);

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
