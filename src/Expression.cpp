#include <algorithm>
#include <sstream>
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{
class TypeCheck : boost::static_visitor<>
{
	void operator()(const TypeVariable& type)
	{

	}
	void operator()(const TypeOperator& type)
	{

	}
};

void analyseTop()
{

}

TypeVariable pair;

TypeEnvironment::TypeEnvironment(Module* module)
	: parent(nullptr)
	, module(module)
{
	auto type = functionType(TypeVariable(), functionType(TypeVariable(), pair));
	addType("(,)", type);
}

TypeEnvironment::TypeEnvironment(TypeEnvironment&& env)
	: types(std::move(env.types))
	, parent(env.parent)
	, module(env.module)
{
}

TypeEnvironment TypeEnvironment::child()
{
	TypeEnvironment c(module);
	c.parent = this;
	return std::move(c);
}

Type& TypeEnvironment::addType(const std::string& name, const Type& type)
{
	types.insert(std::make_pair(name, type));
	return types[name];
}

Type* TypeEnvironment::getType(const std::string& name)
{
	auto found = types.find(name);
	if (found != types.end())
		return &found->second;
	if (parent != nullptr)
		return parent->getType(name);
	if (module != nullptr)
	{
		auto found = std::find_if(module->bindings.begin(), module->bindings.end(), [&name](const Binding& bind)
		{
			return bind.name == name;
		});
		if (found != module->bindings.end())
		{
			return found->expression->getType();
		}
	}
	return nullptr;
}

Name::Name(std::string name)
    : name(std::move(name))
{
}

void Name::typecheck(TypeEnvironment& env, const Type& self)
{
}

Type* Name::getType() const
{
	return type.get();
}

Type intType;
Type doubleType;

Rational::Rational(double value)
	: value(value)
{
}

void Rational::typecheck(TypeEnvironment& env, const Type& self)
{
}

Type* Rational::getType() const
{
	return &doubleType;
}

Number::Number(int value)
	: Rational(0)
	, value(value)
{
}

void Number::typecheck(TypeEnvironment& env, const Type& self)
{
}

Type* Number::getType() const
{
	return &intType;
}


PrimOP::PrimOP(PrimOps op, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

void PrimOP::typecheck(TypeEnvironment& env, const Type& inferred)
{
	lhs->typecheck(env, inferred);
	rhs->typecheck(env, inferred);
}


Type* PrimOP::getType() const
{
	return lhs->getType();
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
	, isRecursive(false)
{
}

void Let::typecheck(TypeEnvironment& env, const Type& self)
{
	TypeEnvironment child = env.child();
	for (auto& bind : bindings)
	{
		child.addType(bind.name, TypeVariable());
		bind.expression->typecheck(child, TypeVariable());
	}
}

Type* Let::getType() const
{
	return expression->getType();
}

Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && expression)
	: arguments(std::move(arguments))
	, expression(std::move(expression))
{
}

void Lambda::typecheck(TypeEnvironment& env, const Type& inferred)
{
}

Type* Lambda::getType() const
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
	function->typecheck(env, TypeVariable());
}


Type* Apply::getType() const
{
	return type.get();
}

void ConstructorPattern::compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const
{
	instructions.push_back(GInstruction(GOP::CASEJUMP, tag));
	branches.push_back(instructions.size());
	instructions.push_back(GInstruction(GOP::JUMP));
}

Case::Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives)
	: expression(std::move(expr))
	, alternatives(std::move(alternatives))
{}

void Case::typecheck(TypeEnvironment& env, const Type& self)
{
	expression->typecheck(env, TypeVariable());

	const Type* returnType = nullptr;
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
			}
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			alt.expression->typecheck(env, self);
			const Type& t = *alt.expression->getType();
			if (returnType != nullptr && !(t == *returnType))
			{
				throw std::runtime_error("All case alternatives must have the same type");
			}
			returnType = &t;
		}
	}
}

Type* Case::getType() const
{
	return expression->getType();
}

GCompiler::GCompiler()
	: index(0)
{
	Constructor def;
	def.name = "(,)";
	def.tag = 0;
	def.arity = 2;
	dataDefinitions.push_back(def);
}

void GCompiler::newStackVariable(const std::string& name)
{
	stackVariables.push_back(name);
}
void GCompiler::popStack(size_t n)
{
	for (size_t i = 0; i < n; i++)
	{
		stackVariables.pop_back();
	}
}

Variable GCompiler::getVariable(const std::string& name)
{
	auto found = std::find(stackVariables.begin(), stackVariables.end(), name);
	if (found != stackVariables.end())
	{
		size_t index = std::distance(stackVariables.begin(), found);
		size_t distanceFromStackTop = stackVariables.size() - index - 1;
		return Variable { VariableType::STACK, TypeVariable(), index };
	}
	auto foundGlobal = globals.find(name);
	if (foundGlobal != globals.end())
	{
		int i = globalIndices[foundGlobal->second.get()];
		return Variable { VariableType::TOPLEVEL, TypeVariable(), i };
	}
	auto foundCtor = std::find_if(dataDefinitions.begin(), dataDefinitions.end(),
		[&name](const Constructor& def)
	{
		return def.name == name;
	});
	if (foundCtor != dataDefinitions.end())
	{
		int index = std::distance(dataDefinitions.begin(), foundCtor);
		return Variable { VariableType::CONSTRUCTOR, TypeVariable(), index };
	}
	return Variable { VariableType::STACK, TypeVariable(), -1 };
}

SuperCombinator& GCompiler::getGlobal(const std::string& name)
{
	auto found = globals.find(name);
	if (found == globals.end())
	{
		auto& ptr = globals[name] = std::unique_ptr<SuperCombinator>(new SuperCombinator());
		ptr->name = name;
		globalIndices[ptr.get()] = index++;
		return *ptr;
	}
	return *found->second;
}


void Name::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	Variable var = env.getVariable(this->name);
	if (var.index == -1)
		throw std::runtime_error("Did not find variable " + name);
	switch (var.accessType)
	{
	case VariableType::STACK:
		instructions.push_back(GInstruction(GOP::PUSH, var.index));
		break;
	case VariableType::TOPLEVEL:
		instructions.push_back(GInstruction(GOP::PUSH_GLOBAL, var.index));
		break;
	case VariableType::CONSTRUCTOR:
		instructions.push_back(GInstruction(GOP::PACK, var.index));
		break;
	default:
		assert(0 && "Could not find the variable");
		break;
	}
	if (strict)
		instructions.push_back(GInstruction(GOP::EVAL));
}

void Number::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	instructions.push_back(GInstruction(GOP::PUSH_INT, this->value));
}
void Rational::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	assert(0);
}

GOP toGOP(PrimOps op)
{
    switch(op)
    {
		case PrimOps::ADD: return GOP::ADD;
		case PrimOps::SUBTRACT: return GOP::SUBTRACT;
		case PrimOps::MULTIPLY: return GOP::MULTIPLY;
		case PrimOps::DIVIDE: return GOP::DIVIDE;
		case PrimOps::REMAINDER: return GOP::REMAINDER;
    }
    throw std::runtime_error("Unknown op");
}

void PrimOP::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool)
{
    lhs->compile(env, instructions, true);
	rhs->compile(env, instructions, true);
	instructions.push_back(GInstruction(toGOP(op)));
}
void Let::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	isRecursive = true;
	if (isRecursive)
		instructions.push_back(GInstruction(GOP::ALLOC, bindings.size()));

	for (auto& bind : bindings)
	{
		if (Lambda* lambda = dynamic_cast<Lambda*>(bind.expression.get()))
		{
			for (auto arg = lambda->arguments.rbegin(); arg != lambda->arguments.rend(); ++arg)
			{
				env.stackVariables.push_back(*arg);
			}
			SuperCombinator& sc = env.getGlobal(bind.name);
			sc.arity = lambda->arguments.size();
			lambda->expression->compile(env, sc.instructions, true);
			sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
			sc.instructions.push_back(GInstruction(GOP::POP, sc.arity));
			sc.instructions.push_back(GInstruction(GOP::UNWIND));
		}
		else
		{
			env.newStackVariable(bind.name);
			bind.expression->compile(env, instructions, false);
		}
		if (isRecursive)
		{
			instructions.push_back(GInstruction(GOP::UPDATE, env.stackVariables.size() - 1));
			env.newStackVariable("");//Add an extra variable since there will also be an indirection on the stack
		}
	}
	expression->compile(env, instructions, strict);
	instructions.push_back(GInstruction(GOP::SLIDE, bindings.size()));
	env.popStack(bindings.size());
	if (strict)
	{
		instructions.push_back(GInstruction(GOP::EVAL));
		env.popStack(bindings.size());
	}
}
void Lambda::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	assert(0);
}
void Apply::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	for (int ii = arguments.size() - 1; ii >= 0; --ii)
	{
		//TODO
		arguments[ii]->compile(env, instructions, false);
		env.newStackVariable("");
	}
	function->compile(env, instructions, strict);
	env.popStack(arguments.size());
	if (instructions[instructions.size() - 2].op == GOP::PACK)
		return;
	for (size_t ii = 0; ii < arguments.size(); ++ii)
		instructions.push_back(GInstruction(GOP::MKAP));
	if (strict)
		instructions.push_back(GInstruction(GOP::EVAL));
}	
void Case::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	expression->compile(env, instructions, strict);
	env.newStackVariable("");
	std::vector<size_t> branches;
	for (Alternative& alt : alternatives)
	{
		alt.pattern->compileGCode(env, branches, instructions);
	}
	for (size_t ii = 0; ii < alternatives.size(); ii++)
	{
		Alternative& alt = alternatives[ii];
		instructions[branches[ii]].value = instructions.size();

		auto& pattern = dynamic_cast<ConstructorPattern&>(*alt.pattern);
		instructions.push_back(GInstruction(GOP::SPLIT, pattern.patterns.size()));
		env.popStack(1);

		for (auto& varName : pattern.patterns)
		{
			auto& name = dynamic_cast<PatternName&>(*varName);
			env.newStackVariable(name.name);
		}

		alt.expression->compile(env, instructions, strict);

		for (auto& varName : pattern.patterns)
		{
			env.stackVariables.pop_back();
		}
	}
	if (strict)
		instructions.push_back(GInstruction(GOP::EVAL));
}
}
