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

class Expression;

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

	void bindName(const std::string& name, Type& type);
	void registerType(Type& type);

	Type getType(const std::string& name);

	bool isGeneric(const TypeVariable& var) const;
	void replace(TypeVariable replaceMe, const Type& replaceWith);
private:
	Module* module;
	TypeEnvironment* parent;
	std::map<std::string, Type*> namedTypes;
	std::vector<Type*> types;
};


void unify(TypeEnvironment& env, Type& lhs, Type& rhs);

class GCompiler
{
public:
	GCompiler(Module* module);

	void newStackVariable(const std::string& name);
	void popStack(size_t n);
	Variable getVariable(const std::string& name);
	SuperCombinator& getGlobal(const std::string& name);


	std::vector<std::string> stackVariables;
	std::map<std::string, std::unique_ptr<SuperCombinator>> globals;
	std::map<SuperCombinator*, int> globalIndices;
	std::vector<Constructor> dataDefinitions;
private:
	Module* module;
	int index;
};

class Expression
{
public:
    virtual ~Expression() { }

	virtual Type& typecheck(TypeEnvironment& env) = 0;
	
	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict) = 0;

	virtual Type& getType() = 0;
};

class Name : public Expression
{
public:
    Name(std::string name);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	std::string name;
private:
	Type type;
};

class Rational : public Expression
{
public:
	Rational(double value);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	double value;
};

class Number : public Rational
{
public:
    Number(int value);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

    int value;
};

class Let : public Expression
{
public:
	Let(std::vector<Binding> && arguments, std::unique_ptr<Expression>&& expression);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	bool isRecursive;
	std::vector<Binding> bindings;
	std::unique_ptr<Expression> expression;
};

class Lambda : public Expression
{
public:
	Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	std::vector<std::string> arguments;
	std::unique_ptr<Expression> body;
private:
	Type type;
};

class Apply : public Expression
{
public:
	Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	std::unique_ptr<Expression> function;
	std::vector<std::unique_ptr<Expression>> arguments;
private:
	Type type;
};

class PrimOP : public Apply
{
public:
	PrimOP(std::string name, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);
};

class Pattern
{
public:
	virtual ~Pattern() {}

	virtual void addVariables(TypeEnvironment& env, Type& type) {}

	virtual void compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const
	{
		branches.push_back(instructions.size());
	}
};

class PatternName : public Pattern
{
public:
	PatternName(std::string name)
		: name(std::move(name))
	{}

	virtual void addVariables(TypeEnvironment& env, Type& type);

	Type type;
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
	ConstructorPattern(std::string name, std::vector<std::unique_ptr<Pattern>> &&patterns)
		: patterns(std::move(patterns))
		, name(std::move(name))
	{}
	ConstructorPattern(std::vector<std::unique_ptr<Pattern>>&& patterns)
		: patterns(std::move(patterns))
	{}
	ConstructorPattern(ConstructorPattern&& other)
		: patterns(std::move(other.patterns))
		, name(other.name)
	{}


	virtual void addVariables(TypeEnvironment& env, Type& type);

	virtual void compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const;

	std::vector<std::unique_ptr<Pattern>> patterns;
	std::string name;
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

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	std::unique_ptr<Expression> expression;
	std::vector<Alternative> alternatives;
};

}
