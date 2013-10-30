#pragma once
#include <vector>
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
		: Slice(stackBase, maxSize)
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
        assert(currentSize < size() - 1);
        (*this)[currentSize] = obj;
        currentSize++;
    }

    T& base()
    {
        return this->first();
    }

    T& top()
    {
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
		StackObject* newBase = data() + currentSize - numParameters;
		return StackFrame<StackObject>(newBase, size() - (currentSize - numParameters), numParameters);
    }

	size_t stackSize() const
	{
		return currentSize;
	}

    class Iterator
    {
    public:
        Iterator(StackObject* current)
            : current(current)
        {
        }

        StackObject& operator*()
        {
            return *current;
        }
        const StackObject& operator*() const
        {
            return *current;
        }

        Iterator& operator++()
        {
            ++current;
            return *this;
        }
        Iterator operator++(int)
        {
            Iterator before(*this);
            current++;
            return before;
        }

        bool operator==(const Iterator& other) { return current == other.current; }
        bool operator!=(const Iterator& other) { return current != other.current; }
    private:
        StackObject* current;
    };

    
    Iterator begin() const
    {
        return Iterator(stackBase);
    }

    Iterator end() const
    {
        return Iterator(stackBase + currentSize);
    }

private:
    size_t currentSize;
};
}