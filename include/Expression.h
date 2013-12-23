#pragma once
#include <string>
#include <vector>
#include <memory>
#include <map>
#include <set>
#include "Types.h"
#include "SuperCombinator.h"
#include "Util.h"
#include <boost/graph/adjacency_list.hpp>

namespace MyVMNamespace
{
class Environment;
class Binding;
class Module;
class Instance;
class GCompiler;

class Expression;


typedef boost::adjacency_list<boost::listS, boost::vecS, boost::directedS, Binding*> Graph;
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge;

void addBindingsToGraph(Graph& graph, std::vector<Binding>& bindings);
void typecheckDependecyGraph(TypeEnvironment& env, Graph& graph);

class ExpressionVisitor;

class Expression
{
public:
	Expression(Location sourceLocation = Location());

    virtual ~Expression() { }

	virtual Type& typecheck(TypeEnvironment& env) = 0;
	
	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict) = 0;

	virtual Type& getType() = 0;

	virtual void accept(ExpressionVisitor& visitor) = 0;

	Location sourceLocation;
};

class Name : public Expression
{
public:
	Name(std::string name, Location location = Location());

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
	Rational(double value, Location location = Location());

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	virtual void accept(ExpressionVisitor& visitor);

	double value;
};

class Number : public Rational
{
public:
    Number(int value, Location location = Location());

	virtual Type& typecheck(TypeEnvironment& env);

	virtual void compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict);

	virtual Type& getType();

	virtual void accept(ExpressionVisitor& visitor);

	Type type;
    int value;
};

class Let : public Expression
{
public:
	Let(std::vector<Binding> arguments, std::unique_ptr<Expression> expression, Location location = Location());

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
	Lambda(std::vector<std::string> arguments, std::unique_ptr<Expression> expression, Location location = Location());

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
	Apply(std::unique_ptr<Expression> function, std::vector<std::unique_ptr<Expression>> arguments, Location location = Location());

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
	Case(std::unique_ptr<Expression> expr, std::vector<Alternative> alternatives, Location location = Location());

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
