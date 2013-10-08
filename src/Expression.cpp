#include <algorithm>
#include "Expression.h"
#include "Compiler.h"

namespace MyVMNamespace
{

Name::Name(std::string name)
    : name(std::move(name))
{
}

void Name::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
	int stackPos = env.getIndexForName(this->name);
	if (stackPos == -1)
	{
		throw std::runtime_error("Could not find local " + this->name);
	}
	instructions.push_back(Instruction(OP::LOAD, stackPos));
}

Number::Number(int value)
    : value(value)
{
}

void Number::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
}

PrimOP::PrimOP(char op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

void PrimOP::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
    lhs->evaluate(env, instructions);
    rhs->evaluate(env, instructions);
    
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

void FunctionApplication::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
    function->evaluate(env, instructions);

    for (auto& arg : arguments)
    {
        arg->evaluate(env, instructions);
    }
    instructions.push_back(Instruction(OP::CALL, arguments.size()));
}


Let::Let(Let::Bindings&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
{
}

void Let::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
	//Always causes evaluation of bindings before execution of the rest
	//bindings must be used in order of definition in order or it will fail
	for (auto& bind : bindings)
	{
		env.newLocal(bind.first);
		bind.second->evaluate(env, instructions);
	}
	expression->evaluate(env, instructions);
}


Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression)
	: arguments(std::move(arguments))
	, expression(std::move(expression))
{
}

void Lambda::evaluate(Environment& env, std::vector<Instruction>& instructions)
{
	for (auto& name : arguments)
	{
		env.newLocal(name);
	}
	expression->evaluate(env, instructions);
}


}
