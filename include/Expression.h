#pragma once
#include <string>
#include <vector>
#include <memory>
#include <map>
#include <set>
#include "Types.h"
#include "SuperCombinator.h"
#include <boost/graph/adjacency_list.hpp>

namespace MyVMNamespace
{
class Environment;
class Binding;
class Module;
class Instance;

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
	CONSTRUCTOR,
	TYPECLASSFUNCTION
};

class Class;

struct Variable
{
	VariableType accessType;
	int index;
	Class* klass;
	Type* type;
};


class TypeEnvironment
{
public:
	TypeEnvironment(Module* module);
	TypeEnvironment(TypeEnvironment&& env);

	TypeEnvironment child();

	void bindName(const std::string& name, Type& type);
	void registerType(Type& type);

	const Type& getType(const std::string& name);
	Type getFreshType(const std::string& name);

	void addNonGeneric(const Type& type);
	bool isGeneric(const TypeVariable& var) const;
	void replace(TypeVariable replaceMe, const Type& replaceWith);
	void tryReplace(Type& toReplace, TypeVariable& replaceMe, const Type& replaceWith);

	void addConstraint(const TypeVariable& var, const std::string& className);
	void updateConstraints(const TypeVariable& oldVar, const TypeVariable& newVar);
	const std::vector<std::string>& getConstraints(const TypeVariable& var) const;
private:
	Module* module;
	TypeEnvironment* parent;
	std::map<std::string, Type*> namedTypes;
	std::vector<Type*> types;
	std::vector<Type> nonGeneric;
	std::map<TypeVariable, std::vector<std::string>> constraints;
};


void unify(TypeEnvironment& env, Type& lhs, Type& rhs);

typedef boost::adjacency_list<boost::listS, boost::vecS, boost::directedS, Binding*> Graph;
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge;

void addBindingsToGraph(Graph& graph, std::vector<Binding>& bindings);
void typecheckDependecyGraph(TypeEnvironment& env, Graph& graph);

struct InstanceDictionary
{
	std::vector<TypeOperator> constraints;
	std::vector<SuperCombinator*> dictionary;
};

class GCompiler
{
public:
	GCompiler(TypeEnvironment& typeEnv, Module* module);

	void newStackVariable(const std::string& name);
	void popStack(size_t n);
	Variable getVariable(const std::string& name);
	SuperCombinator& getGlobal(const std::string& name);

	int getDictionaryIndex(std::vector<TypeOperator>& dict);

	void compileInstance(Instance& instance);

	std::vector<std::string> stackVariables;
	std::map<std::string, std::unique_ptr<SuperCombinator>> globals;
	std::map<SuperCombinator*, int> globalIndices;
	std::vector<Constructor> dataDefinitions;
	std::vector<InstanceDictionary> instanceDicionaries;

	TypeEnvironment& typeEnv;
private:
	std::map<std::string, std::map<Type, std::vector<SuperCombinator*>>> classes;
	Module* module;
	int index;
};

class ExpressionVisitor;

class Expression
{
public:
    virtual ~Expression() { }

	virtual Type& typecheck(TypeEnvironment& env) = 0;
	
	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict) = 0;

	virtual Type& getType() = 0;

	virtual void accept(ExpressionVisitor& visitor) = 0;
};

class Name : public Expression
{
public:
    Name(std::string name);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	virtual void accept(ExpressionVisitor& visitor);

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

	virtual void accept(ExpressionVisitor& visitor);

	double value;
};

class Number : public Rational
{
public:
    Number(int value);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	virtual void accept(ExpressionVisitor& visitor);

    int value;
};

class Let : public Expression
{
public:
	Let(std::vector<Binding> && arguments, std::unique_ptr<Expression>&& expression);

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	virtual void accept(ExpressionVisitor& visitor);

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

	virtual void accept(ExpressionVisitor& visitor);

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

	virtual void accept(ExpressionVisitor& visitor);

	std::unique_ptr<Expression> function;
	std::vector<std::unique_ptr<Expression>> arguments;
private:
	Type type;
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

	virtual void accept(ExpressionVisitor& visitor);

	std::unique_ptr<Expression> expression;
	std::vector<Alternative> alternatives;
};


class ExpressionVisitor
{
public:
	virtual ~ExpressionVisitor()
	{}
	virtual void visit(Name&)
	{}
	virtual void visit(Number&)
	{}
	virtual void visit(Rational&)
	{}
	virtual void visit(Apply&)
	{}
	virtual void visit(Lambda&)
	{}
	virtual void visit(Let&)
	{}
	virtual void visit(Case&)
	{}
};

}
