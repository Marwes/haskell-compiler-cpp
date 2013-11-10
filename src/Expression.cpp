#include <algorithm>
#include <sstream>
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{

inline bool occurs(const TypeVariable& type, const Type& collection)
{
	switch (collection.which())
	{
	case 0:
		{
			const TypeVariable& variable = boost::get<const TypeVariable>(collection);
			return variable == type;
		}
		break;
	case 1:
		{

			const TypeOperator& op = boost::get<const TypeOperator>(collection);
			return std::any_of(op.types.begin(), op.types.end(), [&type](const Type& elem)
			{
				return occurs(type, elem);
			});
		}
		break;
	}
	return false;
}

class TypeToString : public boost::static_visitor<std::string>
{
public:
	std::string operator()(const TypeVariable& x) const
	{
		std::stringstream str;
		str << x.id;
		return str.str();
	}
	std::string operator()(const TypeOperator& x) const
	{
		std::stringstream str;
		str << x.name << " { ";
		for (const Type& type : x.types)
		{
			str << boost::apply_visitor(*this, type);
			if (&type != &x.types.back())
				str << ", ";
		}
		str << " }";
		return str.str();
	}
};

class RecursiveUnification : public std::runtime_error
{
public:
	RecursiveUnification(const Type& lhs, const Type& rhs)
		: std::runtime_error("Recursive unification.")
		, error("Recursive unification between: ")
	{
		error += boost::apply_visitor(TypeToString(), lhs);
		error += " and ";
		error += boost::apply_visitor(TypeToString(), rhs);
	}

	virtual const char* what() const
	{
		return error.c_str();
	}

private:
	std::string error;
};

class Unify : public boost::static_visitor<>
{
public:
	Unify(Type& lhs, Type& rhs)
		: lhs(lhs)
		, rhs(rhs)
	{
		boost::apply_visitor(*this, lhs, rhs);
	}

	void operator()(const TypeVariable& t1, const TypeVariable& t2) const
	{
		if (t1 != t2)
		{
			if (occurs(t1, t2))
				throw RecursiveUnification(t1, t2);


			lhs = t2;
		}
	}
	void operator()(const TypeVariable& t1, const TypeOperator& t2) const
	{
		if (occurs(t1, t2))
			throw RecursiveUnification(t1, t2);
		rhs = t1;
	}
	void operator()(const TypeOperator& t1, const TypeVariable& t2) const
	{
		operator()(t2, t1);//swap the arguments
	}
	void operator()(const TypeOperator& t1, const TypeOperator& t2) const
	{
		if (t1.name != t2.name || t1.types.size() != t2.types.size())
			throw TypeError(t1, t2);

		for (size_t ii = 0; ii < t1.types.size(); ii++)
		{
			boost::apply_visitor(*this, t1.types[ii], t2.types[ii]);
		}
	}

	Type& lhs;
	Type& rhs;
};

void analyseTop()
{

}

TypeVariable pair;

TypeEnvironment::TypeEnvironment(Module* module)
	: parent(nullptr)
	, module(module)
{
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

Type& TypeEnvironment::newTypeFor(const std::string& name)
{
	return types[name] = TypeVariable();
}

Type& TypeEnvironment::addType(const std::string& name, const Type& type)
{
	types.insert(std::make_pair(name, type));
	return types[name];
}


void TypeEnvironment::registerName(const std::string& name, Type* type)
{
	borrowedTypes.insert(std::make_pair(name, type));
}

Type& TypeEnvironment::getType(const std::string& name)
{
	auto found = types.find(name);
	if (found != types.end())
		return found->second;
	auto foundBorrowed = borrowedTypes.find(name);
	if (foundBorrowed != borrowedTypes.end())
		return *foundBorrowed->second;
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
	return newTypeFor(name);
}

Name::Name(std::string name)
    : name(std::move(name))
{
}

Type& Name::typecheck(TypeEnvironment& env, const Type& self)
{
	return env.getType(this->name);
}

Type& Name::getType()
{
	return type;
}

Type intType;
Type doubleType;

Rational::Rational(double value)
	: value(value)
{
}

Type& Rational::typecheck(TypeEnvironment& env, const Type& self)
{
	return doubleType;
}

Type& Rational::getType()
{
	return doubleType;
}

Number::Number(int value)
	: Rational(0)
	, value(value)
{
}

Type& Number::typecheck(TypeEnvironment& env, const Type& self)
{
	return intType;
}

Type& Number::getType()
{
	return intType;
}


PrimOP::PrimOP(PrimOps op, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs)
    : op(op)
    , lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

Type& PrimOP::typecheck(TypeEnvironment& env, const Type& inferred)
{
	Type& leftType = lhs->typecheck(env, inferred);
	Type& rightType = rhs->typecheck(env, inferred);
	//TODO

	return leftType;
}


Type& PrimOP::getType()
{
	return lhs->getType();
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
	, isRecursive(false)
{
}

Type& Let::typecheck(TypeEnvironment& env, const Type& self)
{
	TypeEnvironment child = env.child();
	for (auto& bind : bindings)
	{
		Type t = bind.expression->typecheck(child, TypeVariable());
		child.addType(bind.name, t);
	}
	return expression->typecheck(child, self);
}

Type& Let::getType()
{
	return expression->getType();
}

Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && body)
	: arguments(std::move(arguments))
	, body(std::move(body))
{
}

Type createFunctionType(TypeEnvironment& env, const std::vector<std::string>& arguments, Expression& body, size_t index)
{
	if (index < arguments.size())
	{
		Type func = functionType(TypeVariable(), createFunctionType(env, arguments, body, index + 1));
		auto& op = boost::get<TypeOperator>(func);
		env.registerName(arguments[index], &op.types[0]);
		return std::move(func);
	}
	else
	{
		return body.typecheck(env, TypeVariable());
	}
}

Type& Lambda::typecheck(TypeEnvironment& env, const Type& inferred)
{
	TypeEnvironment child = env.child();
	type = createFunctionType(env, arguments, *body, 0);
	return type;
}

Type& Lambda::getType()
{
	return this->type;
}

Apply::Apply(std::unique_ptr<Expression> && function, std::vector<std::unique_ptr<Expression>> && arguments)
	: function(std::move(function))
	, arguments(std::move(arguments))
{
}

Type typeCheckApply(TypeEnvironment& env, Apply& apply, int index)
{
	if (index >= 0)
	{
		Expression& arg = *apply.arguments[index];
		Type funcType = typeCheckApply(env, apply, index - 1);
		Type& argType = arg.typecheck(env, TypeVariable());
		Type funcReturn = functionType(argType, TypeVariable());
		Unify(funcReturn, funcType);
		return std::move(funcReturn);
	}
	else
	{
		return;
	}
}

Type& Apply::typecheck(TypeEnvironment& env, const Type& self)
{
	for (auto arg = arguments.rend(); arg != arguments.rbegin(); ++arg)
	{
		Type& argType = (*arg)->typecheck(env, TypeVariable());
		TypeVariable next = TypeVariable();
		Type temp = functionType(argType, next);
		Unify(temp, resultType);
		resultType = next;
	}
	Type funcType = function->typecheck(env, TypeVariable());
	Type& argType = arguments[0]->typecheck(env, TypeVariable());
	Type iterativeFuncType = functionType(funcType, argType);
	Type resultType = TypeVariable();
	Unify(iterativeFuncType, resultType);

	type = resultType;
	return type;//TODO
}


Type& Apply::getType()
{
	return type;
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

Type& Case::typecheck(TypeEnvironment& env, const Type& self)
{
	expression->typecheck(env, TypeVariable());

	const Type* returnType = nullptr;
	for (Alternative& alt : alternatives)
	{
		//TODO alt.pattern->typecheck(altType);
		if (PatternName* pattern = dynamic_cast<PatternName*>(alt.pattern.get()))
		{
			TypeEnvironment caseEnv = env.child();
			caseEnv.registerName(pattern->name, &expression->getType());
			alt.expression->typecheck(caseEnv, self);
			const Type& t = alt.expression->getType();
			if (returnType == nullptr)//First alternative
				returnType = &t;
			else
			{
			}
		}
		else if (NumberLiteral* pattern = dynamic_cast<NumberLiteral*>(alt.pattern.get()))
		{
			alt.expression->typecheck(env, self);
			const Type& t = alt.expression->getType();
			if (returnType != nullptr && !(t == *returnType))
			{
				throw std::runtime_error("All case alternatives must have the same type");
			}
			returnType = &t;
		}
	}
}

Type& Case::getType()
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
			lambda->body->compile(env, sc.instructions, true);
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
