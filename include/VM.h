#pragma once
#include <vector>
#include <memory>
#include <assert.h>
#include "Types.h"
#include "Instruction.h"

namespace MyVMNamespace
{


struct VMField
{
    const Type type;
    const int offset;
};

union StackObject
{
    VMInt intValue;
    VMFloat floatValue;
    VMPointer pointerValue;
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
    T& top()
    {
        return *reinterpret_cast<T*>(this->stackBase - 1 - sizeof(T));
    }
    

    void setStack(VMField field, size_t index, void* data)
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
private:
    StackObject* stackBase;
    size_t currentSize;
    size_t maxSize;
};

class VMI;

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

struct RuntimeEnvironment
{
    RuntimeEnvironment(StackFrame frame)
        : stackFrame(frame)
    {
    }

    const std::vector<VMField> data;

    StackFrame stackFrame;
};

struct VM
{
    VM(std::vector<Instruction>& instructions);

    StackObject getValue(size_t index) { return stack[index]; }

    Array<StackObject>& getStack() { return stack; }

    StackFrame newStackFrame();

    void execute(RuntimeEnvironment& environment);

    void printstack();
private:
    VMInt currentInstruction;
    std::vector<Instruction> instructions;
    
    Array<StackObject> stack;

    friend class VMI;
};

};