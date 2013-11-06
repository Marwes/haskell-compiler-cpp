#pragma once
#include <string>
#include <vector>
#include <memory>
#include <map>
#include "Types.h"
#include "SuperCombinator.h"


namespace MyVMNamespace
{
class Environment;
class Binding;
class Module;

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

enum class VariableType
{
	NONE,
	STACK,
	TOPLEVEL,
	CONSTRUCTOR
};

struct Variable
{
	VariableType accessType;
	Type type;
	int index;
};


class TypeEnvironment
{
public:
	TypeEnvironment(Module* module);
	TypeEnvironment(TypeEnvironment&& env);

	TypeEnvironment child();

	Type& newTypeFor(const std::string& name);
	Type& addType(const std::string& name, const Type& type);

	Type& getType(const std::string& name);
private:
	Module* module;
	TypeEnvironment* parent;
	std::map<std::string, Type> types;
};

class GCompiler
{
public:
	GCompiler();

	void newStackVariable(const std::string& name);
	void popStack(size_t n);
	Variable getVariable(const std::string& name);
	SuperCombinator& getGlobal(const std::string& name);


	std::vector<std::string> stackVariables;
	std::map<std::string, std::unique_ptr<SuperCombinator>> globals;
	std::map<SuperCombinator*, int> globalIndices;
	std::vector<Constructor> dataDefinitions;
private:
	int index;
};

class Expression
{
public:
    virtual ~Expression() { }

	virtual Type& typecheck(TypeEnvironment& env, const Type& self) = 0;
	
	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict) = 0;

	virtual Type* getType() = 0;
};

class Name : public Expression
{
public:
    Name(std::string name);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	std::string name;
private:
	std::unique_ptr<Type> type;
};

class Rational : public Expression
{
public:
	Rational(double value);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	double value;
};

class Number : public Rational
{
public:
    Number(int value);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

    int value;
};

class PrimOP : public Expression
{
public:
	PrimOP(PrimOps op, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

    std::unique_ptr<Expression> lhs, rhs;
    PrimOps op;
};

class Let : public Expression
{
public:
	Let(std::vector<Binding> && arguments, std::unique_ptr<Expression>&& expression);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	bool isRecursive;
	std::vector<Binding> bindings;
	std::unique_ptr<Expression> expression;
};

class Lambda : public Expression
{
public:
	Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	std::vector<std::string> arguments;
	std::unique_ptr<Expression> body;
private:
	Type type;
};

class Apply : public Expression
{
public:
	Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments);

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	std::unique_ptr<Expression> function;
	std::vector<std::unique_ptr<Expression>> arguments;
private:
	Type type;
};

class Pattern
{
public:
	virtual ~Pattern() {}

	virtual void compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const
	{
		assert(0);
	}
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

class ConstructorPattern : public Pattern
{
public:
	ConstructorPattern(int tag, std::vector<std::unique_ptr<Pattern>> &&patterns)
		: patterns(std::move(patterns))
		, tag(tag)
	{}
	ConstructorPattern(std::vector<std::unique_ptr<Pattern>>&& patterns)
		: patterns(std::move(patterns))
		, tag(0)
	{}
	ConstructorPattern(ConstructorPattern&& other)
		: patterns(std::move(other.patterns))
		, tag(other.tag)
	{}

	virtual void compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const;

	std::vector<std::unique_ptr<Pattern>> patterns;
	int tag;
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

	virtual Type& typecheck(TypeEnvironment& env, const Type& self);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type* getType();

	std::unique_ptr<Expression> expression;
	std::vector<Alternative> alternatives;
};

}
