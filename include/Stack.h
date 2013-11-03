#pragma once
#include <vector>
#include <stdarg.h>
#include "Types.h"
#include "Array.h"

namespace MyVMNamespace
{

struct StackLayout
{
    std::vector<TypeEnum> types;
};


template<class T>
class StackFrame : public Slice<T>
{
public:
	StackFrame(T* stackBase, size_t maxSize, size_t currentSize = 0)
		: Slice<T>(stackBase, maxSize)
		, currentSize(currentSize)
	{
	}
    
    void push(VMInt p)
    {
        StackObject o;
        o.intValue = p;
        push(o);
    }
    void push(VMFloat p)
    {
        StackObject o;
        o.floatValue = p;
        push(o);
    }
    void push(Object* p)
    {
        StackObject o;
        o.pointerValue = p;
		p->addReference();
        push(o);
    }
    T pop()
    {
        T o = top();
		top() = T();
        --currentSize;
        return std::move(o);
    }

    void push(const T& obj)
    {
        assert(currentSize < Slice<T>::size() - 1);
        (*this)[currentSize] = obj;
        currentSize++;
    }

    T& base()
    {
        return this->first();
    }

    T& top()
    {
		assert(currentSize != 0);
		T& x = *(Slice<T>::begin() + currentSize - 1);
        return x;
    }

    void setStack(VMField field, size_t index, VMPointer data)
    {
        switch (field.type.type)
        {
        case TYPE_INT:
            this->stackBase[index].intValue = *static_cast<VMInt*>(data);
        case TYPE_FLOAT:
            this->stackBase[index].floatValue = *static_cast<VMFloat*>(data);
        case TYPE_ARRAY:
        case TYPE_CLASS:
            this->stackBase[index].pointerValue = static_cast<Object*>(data);
            break;

        default:
            break;
        }
    }

    template<typename U>
    U& at(size_t index)
    {
        T& val = (*this)[index];
        return reinterpret_cast<U&>(val);
    }

    StackFrame<StackObject> makeChildFrame(size_t numParameters)
    {
		StackObject* newBase = Slice<T>::data() + currentSize - numParameters;
		return StackFrame<StackObject>(newBase, Slice<T>::size() - (currentSize - numParameters), numParameters);
    }

	size_t stackSize() const
	{
		return currentSize;
	}

private:
    size_t currentSize;
};
}