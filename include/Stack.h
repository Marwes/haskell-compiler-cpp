#pragma once
#include <vector>
#include <stdarg.h>
#include "Types.h"
#include "Array.h"

namespace MyVMNamespace
{

template<class T>
class StackFrame : public Slice<T>
{
public:
	StackFrame(T* stackBase, size_t maxSize, size_t currentSize = 0)
		: Slice<T>(stackBase, maxSize)
		, currentSize(currentSize)
	{
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
		assert(currentSize != 0);
		T& x = *(Slice<T>::begin() + currentSize - 1);
        return x;
    }

    StackFrame<T> makeChildFrame(size_t numParameters)
    {
		StackObject* newBase = Slice<T>::data() + currentSize - numParameters;
		return StackFrame<T>(newBase, Slice<T>::size() - (currentSize - numParameters), numParameters);
    }

	size_t stackSize() const
	{
		return currentSize;
	}

private:
    size_t currentSize;
};
}