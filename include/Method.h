#pragma once
#include <vector>
#include <memory>
#include "Types.h"
#include "Instruction.h"
#include "Stack.h"

namespace MyVMNamespace
{
    
class Method : public Data
{
public:
    typedef std::vector<std::unique_ptr<Data>> DataVector;
    Method(Slice<Instruction> code, std::vector<Type> parameters, DataVector&& data = DataVector())
        : code(std::move(code))
        , data(std::move(data))
        , parameterTypes(std::move(parameters))
    {
    }

    Method(Method&& method)
        : code(std::move(method.code))
        , data(std::move(method.data))
        , parameterTypes(std::move(method.parameterTypes))
        , layout(std::move(method.layout))
    {
    }

    static Method main(Assembly& assembly)
    {
        Instruction* begin = assembly.instructions.data() + assembly.entrypoint;
        size_t size = assembly.instructions.size() - assembly.entrypoint;
        return Method(Slice<Instruction>(begin, size), std::vector<Type>());
    }

    const Slice<Instruction> code;
    std::vector<std::unique_ptr<Data>> data;
    const std::vector<Type> parameterTypes;
    StackLayout layout;
};

}