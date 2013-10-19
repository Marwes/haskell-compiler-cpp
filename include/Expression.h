#pragma once
#include <string>
#include <vector>
#include <memory>
#include <map>
#include "Instruction.h"


namespace MyVMNamespace
{
class Environment;
class Binding;

enum class PrimOps
{
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	REMAINDER,
	COMPARE_EQ,
	COMPARE_NEQ,
	COMPARE_LT,
	COMPARE_GT,
	COMPARE_LE,
	COMPARE_GE,
};

class Expression
{
public:
    virtual ~Expression() { }

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions) = 0;
};

class Name : public Expression
{
public:
    Name(std::string name);
    
	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

    std::string name;
};

class Number : public Expression
{
public:
    Number(int value);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

    int value;
};

class PrimOP : public Expression
{
public:
	PrimOP(PrimOps op, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

    std::unique_ptr<Expression> lhs, rhs;
    PrimOps op;
};

class Let : public Expression
{
public:
	Let(std::vector<Binding> && arguments, std::unique_ptr<Expression>&& expression);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

	std::vector<Binding> bindings;
	std::unique_ptr<Expression> expression;
};

class Lambda : public Expression
{
public:
	Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

	std::vector<std::string> arguments;
	std::unique_ptr<Expression> expression;
};

class Apply : public Expression
{
public:
	Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

	std::unique_ptr<Expression> function;
	std::vector<std::unique_ptr<Expression>> arguments;
};

class Pattern
{
public:
	virtual ~Pattern() {}
};

class PatternName : public Pattern
{
public:
	PatternName(std::string name)
		: name(std::move(name))
	{}

	std::string name;
};

class NumberLiteral : public Pattern
{
public:
	NumberLiteral(int value)
		: value(value)
	{}
	int value;
};

class Alternative
{
public:
	Alternative(std::unique_ptr<Pattern> pattern, std::unique_ptr<Expression> expression)
		: pattern(std::move(pattern))
		, expression(std::move(expression))
	{}
	Alternative(Alternative&& other)
		: pattern(std::move(other.pattern))
		, expression(std::move(other.expression))
	{}

	std::unique_ptr<Pattern> pattern;
	std::unique_ptr<Expression> expression;
};

class Case : public Expression
{
public:
	Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives);

	virtual const Type& evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions);

	std::unique_ptr<Expression> expression;
	std::vector<Alternative> alternatives;
};

}
