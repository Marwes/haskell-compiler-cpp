#pragma once
#include <vector>
#include <memory>
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
    {
    }

    
    template<typename T>
    void push(T& obj)
    {
        assert(currentSize < maxSize - 1);
        T& address = reinterpret_cast<T&>(*this->stackBase);
        address = obj;
        currentSize += sizeof(T);
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
        return reinterpret_cast<T&>(this->stackBase[index]);
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
    Array(size_t initialSize = 128)
        : dataPtr(new T[128])
    {
    }

    size_t size() { return dataSize; }
private:
    size_t dataSize;
    std::unique_ptr<T> dataPtr;
};

struct RuntimeEnvironment
{
    const std::vector<VMField> data;

    StackFrame stackFrame;
};

struct VM
{

    void execute();
private:
    VMInt currentInstruction;
    std::vector<Instruction> instructions;
    
    std::vector<unsigned char> stack;

    RuntimeEnvironment* currentEnvironment;

    friend class VMI;
};

};