#pragma once
#include <stddef.h>

namespace MyVMNamespace
{

template<class T>
class Array
{
public:
    Array(size_t initialSize = 128)
        : dataPtr(new T[initialSize])
        , dataSize(initialSize)
    {
    }
    ~Array() { delete[] dataPtr; }

    T& operator[](size_t index) { return dataPtr[index]; }

    T* data() { return dataPtr; }
    size_t size() { return dataSize; }
private:
    size_t dataSize;
    T* dataPtr;
};


template<typename T>
class Slice
{
public:
    Slice(T* start, size_t maxSize)
        : start(start)
        , maxSize(maxSize)
    {
    }

    
    T* begin() const
    {
        return start;
    }

    T* end() const
    {
        return start + maxSize;
    }

    T& first()
    {
		assert(size() != 0);
        return *start;
    }

    T& last()
    {
		assert(size() != 0);
        return *(start + maxSize - 1);
    }

    T& operator[](size_t index)
    {
		assert(index < size());
        return start[index];
    }
    const T& operator[](size_t index) const
	{
		assert(index < size());
        return start[index];
    }

	T* data() const
	{
		return start;
	}

    size_t size() const
    {
        return maxSize;
    }

private:
    T* start;
    size_t maxSize;
};

}