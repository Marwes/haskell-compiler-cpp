#pragma once
#include <string>
#include <vector>
#include <memory>
#include <map>
#include "Instruction.h"

template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args ... args)
{
    return std::unique_ptr<T>(new T(args...));
}

namespace MyVMNamespace
{

class Environment
{
public:
    void add(const std::string& name, int index)
    {

    }

    void retriveName(std::vector<Instruction>& instructions, const std::string& name)
    {
        auto found = std::find(stackValues.begin(), stackValues.end(), name);
        if (found != stackValues.end())
        {
            size_t index = std::distance(stackValues.begin(), found);
            instructions.push_back(Instruction(OP::LOAD, index));
            return;
        }
    }

private:
    std::vector<std::string> stackValues;
};

class Expression
{
public:
    virtual ~Expression() { }

    virtual void evaluate(std::vector<Instruction>& instructions) = 0;
};

class Name : public Expression
{
public:
    Name(std::string name)
        : name(std::move(name))
    {
    }
    
    virtual void evaluate(std::vector<Instruction>& instructions)
    {
        assert(0);
    }

    std::string name;
};

class Number : public Expression
{
public:
    Number(int value)
        : value(value)
    {
    }

    virtual void evaluate(std::vector<Instruction>& instructions)
    {
        instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
    }

    int value;
};

class PrimOP : public Expression
{
public:
    PrimOP(char op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs)
        : op(op)
        , lhs(std::move(lhs))
        , rhs(std::move(rhs))
    {
    }

    virtual void evaluate(std::vector<Instruction>& instructions)
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

    std::unique_ptr<Expression> lhs, rhs;
    char op;
};

class FunctionApplication : public Expression
{
public:
    FunctionApplication(std::unique_ptr<Expression>&& function, std::vector<std::unique_ptr<Expression>>&& arguments)
        : function(std::move(function))
        , arguments(std::move(arguments))
    {
    }

    virtual void evaluate(std::vector<Instruction>& instructions)
    {
        function->evaluate(instructions);

        for (auto& arg : arguments)
        {
            arg->evaluate(instructions);
        }
        instructions.push_back(Instruction(OP::CALL, arguments.size()));
    }
    
    std::unique_ptr<Expression> function;
    std::vector<std::unique_ptr<Expression>> arguments;
};

}