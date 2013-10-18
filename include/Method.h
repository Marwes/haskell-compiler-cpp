#pragma once
#include <vector>
#include <memory>
#include "Types.h"
#include "Instruction.h"
#include "Stack.h"

namespace MyVMNamespace
{
    
    class RefCountedPointer
    {
    public:
        RefCountedPointer(Object* o)
            : value(o)
        {
            value->addReference();
        }

        ~RefCountedPointer()
        {
            if (value)
                value->removeReference();
        }

		RefCountedPointer(RefCountedPointer && other)
			: value(other.value)
		{
			other.value = nullptr;
		}

        Object* get() { return value; }
        const Object* get() const { return value; }

        Object* operator->() { return value; }
        const Object* operator->() const { return value; }

        Object& operator*() { return *value; }
        const Object& operator*() const { return *value; }

    private:
        Object* value;
    };

}