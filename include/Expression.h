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
class Environment;

class Expression
{
public:
    virtual ~Expression() { }

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions) = 0;
};

class Name : public Expression
{
public:
    Name(std::string name);
    
	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);

    std::string name;
};

class Number : public Expression
{
public:
    Number(int value);

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);

    int value;
};

class PrimOP : public Expression
{
public:
    PrimOP(char op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs);

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);

    std::unique_ptr<Expression> lhs, rhs;
    char op;
};

class FunctionApplication : public Expression
{
public:
    FunctionApplication(std::unique_ptr<Expression>&& function, std::vector<std::unique_ptr<Expression>>&& arguments);

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);
    
    std::unique_ptr<Expression> function;
    std::vector<std::unique_ptr<Expression>> arguments;
};


class Let : public Expression
{
public:
	typedef std::vector<std::pair<std::string, std::unique_ptr<Expression>>> Bindings;
	Let(Bindings && arguments, std::unique_ptr<Expression>&& expression);

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);

	Bindings bindings;
	std::unique_ptr<Expression> expression;
};

class Lambda : public Expression
{
public:
	Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression);

	virtual void evaluate(Environment& env, std::vector<Instruction>& instructions);

	std::vector<std::string> arguments;
	std::unique_ptr<Expression> expression;
};

}
