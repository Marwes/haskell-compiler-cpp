#include <algorithm>
#include <sstream>
#include "Expression.h"
#include "Compiler.h"

namespace MyVMNamespace
{

TypeEnvironment::TypeEnvironment()
	: parent(nullptr)
{}

TypeEnvironment::TypeEnvironment(TypeEnvironment&& env)
	: types(std::move(env.types))
	, parent(env.parent)
{
}

TypeEnvironment TypeEnvironment::child()
{
	TypeEnvironment c;
	c.parent = this;
	return std::move(c);
}

void TypeEnvironment::addType(const std::string& name, const Type& type)
{
	types.insert({ name, std::unique_ptr<Type>(type.copy()) });
}

Type* TypeEnvironment::getType(const std::string& name)
{
	auto found = types.find(name);
	if (found != types.end())
		return found->second.get();
	if (parent != nullptr)
		return parent->getType(name);
	return nullptr;
}

Name::Name(std::string name)
    : name(std::move(name))
{
}

void Name::typecheck(TypeEnvironment& env, const Type& self)
{
	const Type* t = env.getType(name);
	if (t == nullptr)
		throw std::runtime_error("Could not find type " + name);
	if (!self.isCompatibleWith(*t))
		throw TypeError(self, *t);
	this->type = std::unique_ptr<Type>(t->copy());
}

const Type& Name::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	Variable variable = env.getVariable(this->name);
	if (variable.index == -1)
	{
		throw std::runtime_error("Could not find variable " + this->name);
	}
	if (variable.accessType == VariableType::STACK)
	{
		instructions.push_back(Instruction(OP::LOAD, variable.index));
	}
	else
	{
		instructions.push_back(Instruction(OP::LOAD_FUNCTION, variable.index));
	}
	return variable.type;
}

const Type* Name::getType() const
{
	return type.get();
}

const Type intType("Int", TypeEnum::TYPE_CLASS);
const Type doubleType("Double", TypeEnum::TYPE_CLASS);

Rational::Rational(double value)
	: value(value)
{
}

void Rational::typecheck(TypeEnvironment& env, const Type& self)
{
	if (!self.isCompatibleWith(doubleType))
		throw TypeError(self, doubleType);
}

const Type* Rational::getType() const
{
	return &doubleType;
}

const Type& Rational::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	instructions.push_back(Instruction(OP::LOAD_DOUBLE_CONST, value));
	return intType;
}

Number::Number(int value)
	: Rational(0)
	, value(value)
{
}

void Number::typecheck(TypeEnvironment& env, const Type& self)
{
	if (!self.isCompatibleWith(intType))
		throw TypeError(self, intType);
}

const Type& Number::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, value));
	return intType;
}

const Type* Number::getType() const
{
	return &intType;
}


PrimOP::PrimOP(PrimOps op, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

void throwNotCompatible(const Type& inferred, const Type& actual)
{
	if (!inferred.isCompatibleWith(actual))
		throw TypeError(inferred, actual);
}

void PrimOP::typecheck(TypeEnvironment& env, const Type& inferred)
{
	lhs->typecheck(env, inferred);
	rhs->typecheck(env, inferred);
	throwNotCompatible(inferred, *lhs->getType());
	throwNotCompatible(inferred, *rhs->getType());

	const Type& selfType = lhs->getType()->isCompatibleWith(*rhs->getType()) ? *rhs->getType() : *lhs->getType();
}


const Type* PrimOP::getType() const
{
	return lhs->getType();
}

OP translatePrimOp(PrimOps op, const Type& type)
{
#define OP_CASE(op) \
	case PrimOps::##op:\
	if (type.isCompatibleWith(intType))\
	return OP::##op##_INT; \
	if (type.isCompatibleWith(doubleType))\
		return OP::##op##_DOUBLE; \
	break;

	switch (op)
	{
		OP_CASE(ADD)
		OP_CASE(SUBTRACT)
		OP_CASE(MULTIPLY)
		OP_CASE(DIVIDE)
		OP_CASE(REMAINDER)
	case PrimOps::COMPARE_EQ:
		return OP::COMPARE_EQ;
	case PrimOps::COMPARE_NEQ:
		return OP::COMPARE_NEQ;
	case PrimOps::COMPARE_LT:
		return OP::COMPARE_LT;
	case PrimOps::COMPARE_GT:
		return OP::COMPARE_GT;
	case PrimOps::COMPARE_LE:
		return OP::COMPARE_LE;
	case PrimOps::COMPARE_GE:
		return OP::COMPARE_GE;
	default:
		break;
	}
#undef OP_CASE
	throw TypeError(intType, type);//TODO, multiple types
}

const Type& PrimOP::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	const Type& lhsType = lhs->evaluate(env, inferred, instructions);
	const Type& rhsType = rhs->evaluate(env, inferred, instructions);

	if (inferred.isCompatibleWith(intType) || inferred.isCompatibleWith(doubleType))
	{
		if (lhsType.isCompatibleWith(rhsType))
		{
			instructions.push_back(Instruction(translatePrimOp(op, rhsType)));
			return rhsType;
		}
		else if (rhsType.isCompatibleWith(lhsType))
		{
			instructions.push_back(Instruction(translatePrimOp(op, lhsType)));
			return lhsType;
		}
	}
	throw TypeError(inferred, intType);
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
{
}

void Let::typecheck(TypeEnvironment& env, const Type& self)
{
	TypeEnvironment child = env.child();
	for (auto& bind : bindings)
	{
		bind.expression->typecheck(child, PolymorphicType::any);
	}
}

const Type* Let::getType() const
{
	return expression->getType();
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
			env.newLocal(bind.name, &PolymorphicType::any);
			auto t = bind.expression->evaluate(env, PolymorphicType::any, instructions);
		}
	}
	return expression->evaluate(env, inferred, instructions);
}


Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression)
	: arguments(std::move(arguments))
	, expression(std::move(expression))
{
}

std::unique_ptr<Type> createFunctionType(const std::vector<const Type*>& types)
{
	assert(types.size() >= 2);
	std::unique_ptr<Type> func(types.back()->copy());
	for (size_t ii = types.size() - 2; ii >= 0; ii--)
	{
		func = std::unique_ptr<FunctionType>(
			new FunctionType(std::unique_ptr<Type>(types[ii]->copy()), std::move(func)));
	}
	return func;
}

void Lambda::typecheck(TypeEnvironment& env, const Type& inferred)
{
	const RecursiveType* funcType = dynamic_cast<const RecursiveType*>(&inferred);
	if (funcType == nullptr)
		throw TypeError("Expected: Function type", inferred);

	std::vector<const Type*> argTypes;
	argTypes.push_back(&funcType->getArgumentType());
	const Type* returnType = &funcType->getReturnType();
	for (size_t ii = 0; ii < arguments.size() - 1; ii++)
	{
		if (auto t = dynamic_cast<const RecursiveType*>(returnType))
		{
			argTypes.push_back(&t->getArgumentType());
			returnType = &t->getReturnType();
		}
		else
			throw TypeError("To few arguments", inferred);
	}
	assert(returnType != nullptr);
	expression->typecheck(env, *returnType);
	argTypes.push_back(expression->getType());
	this->type = createFunctionType(argTypes);
}

const Type& Lambda::evaluate(Environment& env, const Type& inferred, std::vector<Instruction>& instructions)
{
	if (auto t = dynamic_cast<const FunctionType*>(&inferred))
	{
		int index = env.addLambda(*this, *t);
		instructions.push_back(Instruction(OP::LOAD_FUNCTION, index));//TODO, dont get stack index
	}
	
	return inferred;
}

const Type* Lambda::getType() const
{
	return this->type.get();
}

Apply::Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments)
	: function(std::move(function))
	, arguments(std::move(arguments))
{
}

void Apply::typecheck(TypeEnvironment& env, const Type& self)
{
	function->typecheck(env, PolymorphicType::any);
	const FunctionType* funcType = dynamic_cast<const FunctionType*>(function->getType());
	if (funcType == nullptr)
		throw TypeError("Expected: function type", *function->getType());

	for (auto& arg : arguments)
	{
		if (funcType == nullptr)
			throw std::runtime_error("Not enough arguments to function");
		arg->typecheck(env, funcType->getArgumentType());
		funcType = dynamic_cast<const FunctionType*>(&funcType->getReturnType());
	}
	if (funcType == nullptr)
		throw std::runtime_error("Not enough arguments to function");
	this->type = std::unique_ptr<Type>(funcType->getReturnType().copy());
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
		Variable variable = env.getFunction(name->name);
		if (variable.index >= 0)
		{
			instructions.push_back(Instruction(OP::CALLI, variable.index));
		}
		else
		{
			int index = env.getNativeFunction(name->name);
			if (index == -1)
				throw std::runtime_error("Did not find function " + name->name);
			instructions.push_back(Instruction(OP::CALLNATIVE, index));
		}
	}
	else
	{
		assert(0 && "Can only handle 'static' functions.");
	}
	return inferred;
}

const Type* Apply::getType() const
{
	return type.get();
}

Case::Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives)
	: expression(std::move(expr))
	, alternatives(std::move(alternatives))
{}

void Case::typecheck(TypeEnvironment& env, const Type& self)
{
	expression->typecheck(env, PolymorphicType::any);

	const Type* returnType;
	for (Alternative& alt : alternatives)
	{
		//TODO alt.pattern->typecheck(altType);
		if (PatternName* pattern = dynamic_cast<PatternName*>(alt.pattern.get()))
		{
			TypeEnvironment caseEnv = env.child();
			caseEnv.addType(pattern->name, *expression->getType());
			alt.expression->typecheck(caseEnv, self);
			const Type& t = *alt.expression->getType();
			if (returnType == nullptr)//First alternative
				returnType = &t;
			else
			{
				bool nextIsCompatible = t.isCompatibleWith(*returnType);
				if (!nextIsCompatible && !returnType->isCompatibleWith(t))
				{
					throw std::runtime_error("All case alternatives must have the same type");
				}
				if (!nextIsCompatible)
					returnType = &t;
			}
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			alt.expression->typecheck(env, self);
			const Type& t = *alt.expression->getType();
			if (returnType != nullptr && t != *returnType)
			{
				throw std::runtime_error("All case alternatives must have the same type");
			}
			returnType = &t;
		}
	}
}

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
			instructions.push_back(Instruction(OP::LOAD, (VMInt)env.getStackTop()));
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
			caseEnv.newLocal(pattern->name, &caseType);
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

const Type* Case::getType() const
{
	return expression->getType();
}

}
