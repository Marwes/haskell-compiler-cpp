#include <algorithm>
#include <sstream>
#include "Types.h"
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
			return std::any_of(op.types.begin(), op.types.end(),
				[&type](const std::shared_ptr<Type>& elem)
			{
				return occurs(type, *elem);
			});
		}
		break;
	}
	return false;
}

class RecursiveUnification : public std::runtime_error
{
public:
	RecursiveUnification(const Type& lhs, const Type& rhs)
		: std::runtime_error("Recursive unification.")
	{
		std::stringstream str;
		str << "Recursive unification between: ";
		str << lhs << " and " << rhs;
		error = str.str();
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
			boost::apply_visitor(*this, *t1.types[ii], *t2.types[ii]);
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
	return namedTypes[name] = TypeVariable();
}

Type& TypeEnvironment::newType()
{
	types.push_back(std::unique_ptr<Type>(new Type));
	return *types.back();
}

Type& TypeEnvironment::getType(const std::string& name)
{
	auto found = namedTypes.find(name);
	if (found != namedTypes.end())
		return found->second;
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

Type intType(TypeOperator("Int"));
Type doubleType(TypeOperator("Double"));

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
		Type& t = child.newTypeFor(bind.name);
		t = bind.expression->typecheck(child, TypeVariable());
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


Type& Lambda::typecheck(TypeEnvironment& env, const Type& inferred)
{
	TypeEnvironment child = env.child();
	Type* returnType = &body->typecheck(env, TypeVariable());
	std::cerr << *returnType << std::endl;
	for (auto arg = arguments.rbegin(); arg != arguments.rend(); ++arg)
	{
		Type& argType = env.getType(*arg);
		Type& funcType = env.newType();
		funcType = functionType(argType, *returnType);
		
		returnType = &funcType;
	}
	return *returnType;
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
		return TypeVariable();//TODO
	}
}

Type& Apply::typecheck(TypeEnvironment& env, const Type& self)
{
	Type& funcType = function->typecheck(env, TypeVariable());
	Type& argType = arguments[0]->typecheck(env, TypeVariable());
	Type* resultType = &env.newType();
	Type& iterativeFuncType = env.newType();
	iterativeFuncType = functionType(funcType, argType);
	Unify(iterativeFuncType, *resultType);
	for (size_t ii = 1; ii < arguments.size(); ii++)
	{
		auto& arg = arguments[ii];
		Type& argType = arg->typecheck(env, TypeVariable());
		Type& next = env.newType();
		Type& temp = env.newType();
		temp = functionType(argType, next);
		Unify(temp, *resultType);
		resultType = &next;
	}
	return *resultType;//TODO
}


Type& Apply::getType()
{
	return type;
}

std::vector<std::unique_ptr<Expression>> argVector(std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs)
{
	std::vector<std::unique_ptr<Expression>> args(2);
	args[0] = std::move(lhs);
	args[1] = std::move(rhs);
	return std::move(args);
}

PrimOP::PrimOP(std::string name, std::unique_ptr<Expression> && lhs, std::unique_ptr<Expression> && rhs)
	: Apply(std::unique_ptr<Expression>(new Name(std::move(name))), argVector(std::move(lhs), std::move(rhs)))
{
}


GOP toGOP(const std::string& op)
{
	if (op == "+") return GOP::ADD;
	if (op == "-") return GOP::SUBTRACT;
	if (op == "*") return GOP::MULTIPLY;
	if (op == "/") return GOP::DIVIDE;
	if (op == "%") return GOP::REMAINDER;
	throw std::runtime_error("Unknown op" + op);
}

void PrimOP::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool)
{
	arguments[0]->compile(env, instructions, true);
	arguments[1]->compile(env, instructions, true);
	instructions.push_back(GInstruction(toGOP(static_cast<Name&>(*function).name)));
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

	Type* returnType = nullptr;
	for (Alternative& alt : alternatives)
	{
		TypeEnvironment caseEnv = env.child();
		Type& t = alt.expression->typecheck(caseEnv, self);
		if (returnType == nullptr)//First alternative
			returnType = &t;
		else if (returnType != nullptr && !(t == *returnType))
		{
			throw std::runtime_error("All case alternatives must have the same type");
		}
	}
	assert(returnType != nullptr);
	return *returnType;
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
		int index = std::distance(stackVariables.begin(), found);
		int distanceFromStackTop = stackVariables.size() - index - 1;
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
