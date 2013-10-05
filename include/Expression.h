#pragma once
#include <string>
#include <vector>
#include <memory>

template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args ... args)
{
    return std::unique_ptr<T>(new T(args...));
}

class Expression
{
public:
    virtual ~Expression() { }
};

class Name : public Expression
{
public:
    Name(std::string name)
        : name(std::move(name))
    {
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
    int value;
};

class BinPrimOp : public Expression
{
public:
    std::unique_ptr<Expression> lhs, rhs;
    std::string name;
};

class FunctionApplication : public Expression
{
public:
    FunctionApplication(std::unique_ptr<Expression>&& function, std::vector<std::unique_ptr<Expression>>&& arguments)
        : function(std::move(function))
        , arguments(std::move(arguments))
    {
    }
    
    std::unique_ptr<Expression> function;
    std::vector<std::unique_ptr<Expression>> arguments;
};