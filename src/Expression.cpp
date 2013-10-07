#include <algorithm>
#include "Expression.h"

namespace MyVMNamespace
{

class Environment
{
public:
    int addLocal(const std::string& name)
    {
        stackValues.push_back(name);
        return stackValues.size() - 1;
    }

    int getIndexForName(const std::string& name)
    {
        auto found = std::find(stackValues.begin(), stackValues.end(), name);
        if (found != stackValues.end())
        {
            return std::distance(stackValues.begin(), found);
        }
        return -1;
    }

private:
    std::vector<std::string> stackValues;
};

Name::Name(std::string name)
    : name(std::move(name))
{
}

void Name::evaluate(std::vector<Instruction>& instructions)
{
    
}

Number::Number(int value)
    : value(value)
{
}

void Number::evaluate(std::vector<Instruction>& instructions)
{
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
}

PrimOP::PrimOP(char op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

void PrimOP::evaluate(std::vector<Instruction>& instructions)
{
    lhs->evaluate(instructions);
    rhs->evaluate(instructions);
    
    OP iOP = OP::NOP;
    switch (op)
    {
    case '+': iOP = OP::ADD; break;
    case '-': iOP = OP::SUBTRACT; break;
    case '*': iOP = OP::MULTIPLY; break;
    case '/': iOP = OP::DIVIDE; break;
    case '%': iOP = OP::REMAINDER; break;
    default:
        break;
    }
    instructions.push_back(Instruction(iOP));
}

FunctionApplication::FunctionApplication(std::unique_ptr<Expression>&& function, std::vector<std::unique_ptr<Expression>>&& arguments)
    : function(std::move(function))
    , arguments(std::move(arguments))
{
}

void FunctionApplication::evaluate(std::vector<Instruction>& instructions)
{
    function->evaluate(instructions);

    for (auto& arg : arguments)
    {
        arg->evaluate(instructions);
    }
    instructions.push_back(Instruction(OP::CALL, arguments.size()));
}

}
