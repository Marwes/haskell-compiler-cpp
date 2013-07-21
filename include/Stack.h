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

    
    template<typename T>
    void push(T& obj)
    {
        typedef std::remove_const<T>::type T_mutable;
        assert(currentSize < maxSize - 1);
        T_mutable* address = reinterpret_cast<T_mutable*>(this->stackBase + currentSize);
        *address = obj;
        static_assert(sizeof(T) % sizeof(StackObject) == 0, "");
        currentSize += sizeof(T) / sizeof(StackObject);
    }

    template<typename T>
    T& base()
    {
        return *reinterpret_cast<T*>(this->stackBase);
    }

    StackObject& top()
    {
        return *(this->stackBase - 1 - sizeof(StackObject));
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
            this->stackBase[index].pointerValue = data;
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

private:
    StackObject* stackBase;
    size_t currentSize;
    size_t maxSize;
};
}