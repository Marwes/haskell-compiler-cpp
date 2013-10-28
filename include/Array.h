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
        return *start;
    }

    T& last()
    {
        return *(start + maxSize - 1);
    }

    T& operator[](size_t index)
    {
        return start[index];
    }
    const T& operator[](size_t index) const
    {
        return start[index];
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