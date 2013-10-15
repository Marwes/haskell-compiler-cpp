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

class Method : public Object
{
public:
    typedef std::vector<RefCountedPointer> ObjectVector;
    Method(Slice<Instruction> code, std::vector<Type> parameters, ObjectVector&& data = ObjectVector())
        : code(std::move(code))
        , data(std::move(data))
        , parameterTypes(std::move(parameters))
    {
    }

    Method(Method&& method)
        : code(std::move(method.code))
        , data(std::move(method.data))
        , parameterTypes(std::move(method.parameterTypes))
        , stackLayout(std::move(method.stackLayout))
    {
    }

    static Method main(Assembly& assembly)
    {
		FunctionDefinition* funcDef = assembly.getFunction("main");
		assert(funcDef);
        Instruction* begin = funcDef->instructions.data() + assembly.entrypoint;
        size_t size = funcDef->instructions.size() - assembly.entrypoint;
		ObjectVector data;
        return Method(Slice<Instruction>(begin, size), std::vector<Type>(), std::move(data));
    }

    const Slice<Instruction> code;
    ObjectVector data;
    const std::vector<Type> parameterTypes;
    StackLayout stackLayout;
};

}