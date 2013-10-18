#include <algorithm>
#include "Expression.h"
#include "Compiler.h"

namespace MyVMNamespace
{

Name::Name(std::string name)
    : name(std::move(name))
{
}

void Name::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
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

void Number::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
}

PrimOP::PrimOP(OP op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

void PrimOP::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	lhs->evaluate(env, inferred, instructions);
	rhs->evaluate(env, inferred, instructions);

	instructions.push_back(Instruction(op));
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
{
}

void Let::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	//Always causes evaluation of bindings before execution of the rest
	//bindings must be used in order of definition in order or it will fail
	for (auto& bind : bindings)
	{
		if (Lambda* lambda = dynamic_cast<Lambda*>(bind.expression.get()))
		{
			env.addFunction(bind.name, *lambda);
		}
		else
		{
			env.newLocal(bind.name);
			bind.expression->evaluate(env, Type::any, instructions);
		}
	}
	expression->evaluate(env, inferred, instructions);
}


Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression)
	: arguments(std::move(arguments))
	, expression(std::move(expression))
{
}

void Lambda::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	int index = env.addLambda(*this);
	instructions.push_back(Instruction(OP::LOAD_FUNCTION, index));//TODO, dont get stack index
}


Apply::Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments)
	: function(std::move(function))
	, arguments(std::move(arguments))
{
}


void Apply::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	for (auto& arg : arguments)
	{
		arg->evaluate(env, Type::any, instructions);
	}
	if (Name* name = dynamic_cast<Name*>(function.get()))
	{
		int index = env.getFunction(name->name);
		if (index >= 0)
		{
			instructions.push_back(Instruction(OP::CALLI, index));
		}
		else
		{
			index = env.getNativeFunction(name->name);
			assert(index != -1);
			instructions.push_back(Instruction(OP::CALLNATIVE, index));
		}
	}
	else
	{
		assert(0 && "Can only handle 'static' functions.");
	}
}

Case::Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives)
	: expression(std::move(expr))
	, alternatives(std::move(alternatives))
{}

void Case::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	expression->evaluate(env, Type::any, instructions);
	std::vector<size_t> branches;
	size_t beginSize = instructions.size();
	for (Alternative& alt : alternatives)
	{
		if (PatternName* pattern = dynamic_cast<PatternName*>(alt.pattern.get()))
		{
			instructions.push_back(Instruction(OP::JUMP));
			branches.push_back(instructions.size() - 1);
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			//Load the topmost value since it will be reduced by the comparison
			instructions.push_back(Instruction(OP::LOAD, env.getStackTop()));
			instructions.push_back(Instruction(OP::LOAD_INT_CONST, pattern->value));
			instructions.push_back(Instruction(OP::COMPARE_EQ));
			instructions.push_back(Instruction(OP::BRANCH_TRUE));
			branches.push_back(instructions.size() - 1);
			instructions.push_back(Instruction(OP::POP));
		}
	}
	for (size_t ii = 0; ii < alternatives.size(); ++ii)
	{
		const Alternative& alt = alternatives[ii];
		size_t jumpIndex = branches[ii];
		instructions[jumpIndex].arg0 = instructions.size();
		if (PatternName* pattern = dynamic_cast<PatternName*>(alt.pattern.get()))
		{
			Environment caseEnv = env.childEnvironment();
			caseEnv.newLocal(pattern->name);
			alt.expression->evaluate(caseEnv, inferred, instructions);
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			alt.expression->evaluate(env, inferred, instructions);
		}
		instructions.push_back(Instruction(OP::RETURN));
	}
}


}
