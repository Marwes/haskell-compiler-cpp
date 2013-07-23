#pragma once

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
        memset(dataPtr, 0, dataSize * sizeof(T));
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
    Slice(T* begin, size_t maxSize)
        : begin(begin)
        , maxSize(maxSize)
    {
    }

    T& first()
    {
        return *begin;
    }

    T& last()
    {
        return *(begin + maxSize - 1);
    }

    T& operator[](size_t index)
    {
        return begin[index];
    }
    const T& operator[](size_t index) const
    {
        return begin[index];
    }

    size_t size() const
    {
        return maxSize;
    }

private:
    T* begin;
    size_t maxSize;
};

}