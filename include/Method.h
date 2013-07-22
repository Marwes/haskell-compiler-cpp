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
    Method(std::vector<Instruction> code, std::vector<std::unique_ptr<Data>>&& data, std::vector<Type> parameters)
        : code(std::move(code))
        , data(std::move(data))
        , parameterTypes(std::move(parameters))
    {
    }

    const std::vector<Instruction> code;
    std::vector<std::unique_ptr<Data>> data;
    const std::vector<Type> parameterTypes;
    StackLayout layout;
};

}