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


class StackFrame
{
public:
    StackFrame(StackObject* stackBase, size_t maxSize)
        : stackBase(stackBase)
        , maxSize(maxSize)
        , currentSize(0)
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
    StackObject pop()
    {
        StackObject o = top();
        --currentSize;
        return o;
    }

    void push(const StackObject& obj)
    {
        assert(currentSize < maxSize - 1);
        this->stackBase[currentSize] = obj;
        currentSize++;
    }

    template<typename T>
    T& base()
    {
        return *reinterpret_cast<T*>(this->stackBase);
    }

    StackObject& top()
    {
        return *(stackBase + currentSize - 1);
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

    template<typename T>
    T& at(size_t index)
    {
        StackObject& val = this->stackBase[index];
        return reinterpret_cast<T&>(val);
    }

    StackObject& operator[](size_t index)
    {
        StackObject& val = this->stackBase[index];
        return val;
    }

    StackObject* data()
    {
        return stackBase;
    }

    StackFrame makeChildFrame(size_t numParameters)
    {
        return StackFrame(stackBase + currentSize - numParameters, maxSize - currentSize);
    }

    size_t size()
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
    StackObject* stackBase;
    size_t currentSize;
    size_t maxSize;
};
}