#include <algorithm>
#include "Expression.h"
#include "Compiler.h"

namespace MyVMNamespace
{

Name::Name(std::string name)
    : name(std::move(name))
{
}

const Type& Name::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	int stackPos = env.getIndexForName(this->name);
	if (stackPos == -1)
	{
		throw std::runtime_error("Could not find local " + this->name);
	}
	instructions.push_back(Instruction(OP::LOAD, stackPos));
	return inferred;
}

Number::Number(int value)
    : value(value)
{
}

const Type intType("Int", TypeEnum::TYPE_INT);

const Type& Number::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
	return intType;
}

PrimOP::PrimOP(OP op, std::unique_ptr<Expression>&& lhs, std::unique_ptr<Expression>&& rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

const Type& PrimOP::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	const Type& lhsType = lhs->evaluate(env, inferred, instructions);
	const Type& rhsType = rhs->evaluate(env, inferred, instructions);

	if (inferred.isCompatibleWith(lhsType) && inferred.isCompatibleWith(rhsType))
	{
		if (lhsType.isCompatibleWith(rhsType))
		{
			instructions.push_back(Instruction(op));
			return rhsType;
		}
		else if (rhsType.isCompatibleWith(lhsType))
		{
			instructions.push_back(Instruction(op));
			return lhsType;
		}
	}
	throw std::runtime_error("Types are not compatible in PrimOP expression");
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
{
}

const Type& Let::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	//Always causes evaluation of bindings before execution of the rest
	//bindings must be used in order of definition in order or it will fail
	for (auto& bind : bindings)
	{
		if (Lambda* lambda = dynamic_cast<Lambda*>(bind.expression.get()))
		{
			env.addFunction(bind.name, PolymorphicType::any, *lambda);
		}
		else
		{
			env.newLocal(bind.name);
			bind.expression->evaluate(env, PolymorphicType::any, instructions);
		}
	}
	return expression->evaluate(env, inferred, instructions);
}


Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression)
	: arguments(std::move(arguments))
	, expression(std::move(expression))
{
}

const Type& Lambda::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	int index = env.addLambda(*this);
	instructions.push_back(Instruction(OP::LOAD_FUNCTION, index));//TODO, dont get stack index
	
	return inferred;
}


Apply::Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments)
	: function(std::move(function))
	, arguments(std::move(arguments))
{
}


const Type& Apply::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	auto functionType = dynamic_cast<const RecursiveType*>(&inferred);
	if (functionType == nullptr)
	{
		throw std::runtime_error("Tried to apply non function type.");
	}

	const RecursiveType* curriedFunc = functionType;
	for (auto& arg : arguments)
	{
		if (curriedFunc == nullptr)
		{
			throw std::runtime_error("Function does not have enough arguments.");
		}
		arg->evaluate(env, curriedFunc->getArgumentType(), instructions);
		curriedFunc = dynamic_cast<const RecursiveType*>(&curriedFunc->getReturnType());
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
	return inferred;
}

Case::Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives)
	: expression(std::move(expr))
	, alternatives(std::move(alternatives))
{}

const Type& Case::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	const Type& caseType = expression->evaluate(env, PolymorphicType::any, instructions);
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
			if (!caseType.isCompatibleWith(intType))
			{
				throw std::runtime_error("Number literal is not valid in case alternative for non-number types");
			}
			//Load the topmost value since it will be reduced by the comparison
			instructions.push_back(Instruction(OP::LOAD, env.getStackTop()));
			instructions.push_back(Instruction(OP::LOAD_INT_CONST, pattern->value));
			instructions.push_back(Instruction(OP::COMPARE_EQ));
			instructions.push_back(Instruction(OP::BRANCH_TRUE));
			branches.push_back(instructions.size() - 1);
			instructions.push_back(Instruction(OP::POP));
		}
	}
	const Type* ret = nullptr;
	for (size_t ii = 0; ii < alternatives.size(); ++ii)
	{
		const Alternative& alt = alternatives[ii];
		size_t jumpIndex = branches[ii];
		instructions[jumpIndex].arg0 = instructions.size();
		if (PatternName* pattern = dynamic_cast<PatternName*>(alt.pattern.get()))
		{
			Environment caseEnv = env.childEnvironment();
			caseEnv.newLocal(pattern->name);
			const Type& t = alt.expression->evaluate(caseEnv, inferred, instructions);
			if (ret == nullptr)
				ret = &t;
			else
			{
				bool nextIsCompatible = t.isCompatibleWith(*ret);
				if (!nextIsCompatible && !ret->isCompatibleWith(t))
				{
					throw std::runtime_error("All case alternatives must have the same type");
				}
				if (!nextIsCompatible)
					ret = &t;
			}
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			const Type& t = alt.expression->evaluate(env, inferred, instructions);
			if (ret != nullptr && t != *ret)
			{
				throw std::runtime_error("All case alternatives must have the same type");
			}
			ret = &t;
		}
		instructions.push_back(Instruction(OP::RETURN));
	}
	assert(ret != nullptr);
	return *ret;
}


}
