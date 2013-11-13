#include <algorithm>
#include <sstream>
#include "Types.h"
#include "Expression.h"
#include "Module.h"

namespace MyVMNamespace
{
Type intType(TypeOperator("Int"));
Type doubleType(TypeOperator("Double"));

Type binop = functionType(intType, functionType(intType, intType));

Type createPairCtor()
{
	std::vector<Type> args(2);
	args[0] = TypeVariable();
	args[1] = TypeVariable();
	Type pair(TypeOperator("(,)", args));
	return functionType(args[0], functionType(args[1], pair));
}

Type pairCtor = createPairCtor();
Type undefinedType = TypeVariable();

Name::Name(std::string name)
    : name(std::move(name))
{
}

Type& Name::getType()
{
	return type;
}

Rational::Rational(double value)
	: value(value)
{
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

Type& Let::getType()
{
	return expression->getType();
}

Lambda::Lambda(std::vector<std::string> && arguments, std::unique_ptr<Expression> && body)
	: arguments(std::move(arguments))
	, body(std::move(body))
{
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

void PatternName::addVariables(TypeEnvironment& env, Type& type)
{
	env.bindName(name, type);
}

void ConstructorPattern::addVariables(TypeEnvironment& env, Type& type)
{
	TypeOperator& typeOp = boost::get<TypeOperator>(type);
	if (typeOp.types.size() != patterns.size())
	{
		std::stringstream str;
		str << "Cannot match " << type << " in case alternative" << std::endl;
		throw std::runtime_error(str.str());
	}

	for (size_t ii = 0; ii < patterns.size(); ++ii)
	{
		patterns[ii]->addVariables(env, typeOp.types[ii]);
	}
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

Type& Case::getType()
{
	return expression->getType();
}

//typecheck functions

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
				[&type](const Type& elem)
			{
				return occurs(type, elem);
			});
		}
		break;
	}
	return false;
}

TypeEnvironment::TypeEnvironment(Module* module)
	: parent(nullptr)
	, module(module)
{
	bindName("+", binop);
	bindName("-", binop);
	bindName("*", binop);
	bindName("/", binop);
	bindName("%", binop);
	bindName("(,)", pairCtor);
	bindName("undefined", undefinedType);
}

TypeEnvironment::TypeEnvironment(TypeEnvironment && env)
	: parent(env.parent)
	, module(env.module)
{
}

TypeEnvironment TypeEnvironment::child()
{
	TypeEnvironment c(module);
	c.parent = this;
	return c;
}

void TypeEnvironment::bindName(const std::string& name, Type& type)
{
	namedTypes.insert(std::make_pair(name, &type));
}
void TypeEnvironment::registerType(Type& type)
{
	types.push_back(&type);
}

Type& TypeEnvironment::getType(const std::string& name)
{
	auto found = namedTypes.find(name);
	if (found != namedTypes.end())
		return *found->second;
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
	throw std::runtime_error("Could not find the identifier " + name);
}

void tryReplace(Type& toReplace, TypeVariable& replaceMe, const Type& replaceWith)
{
	if (toReplace.which() == 0)
	{
		TypeVariable& x = boost::get<TypeVariable>(toReplace);
		if (x == replaceMe)
		{
			toReplace = replaceWith;
		}
	}
	else
	{
		TypeOperator& x = boost::get<TypeOperator>(toReplace);
		for (Type& type : x.types)
		{
			tryReplace(type, replaceMe, replaceWith);
		}
	}
}

void TypeEnvironment::replace(TypeVariable replaceMe, const Type& replaceWith)
{
	for (auto& pair : namedTypes)
	{
		tryReplace(*pair.second, replaceMe, replaceWith);
	}
	for (Type* type : types)
	{
		tryReplace(*type, replaceMe, replaceWith);
	}
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
	Unify(TypeEnvironment& env, Type& lhs, Type& rhs)
		: env(env)
		, lhs(lhs)
		, rhs(rhs)
	{
		boost::apply_visitor(*this, lhs, rhs);
	}

	void operator()(TypeVariable& t1, TypeVariable& t2) const
	{
		if (t1 != t2)
		{
			if (occurs(t1, t2))
				throw RecursiveUnification(t1, t2);

			env.replace(t1, rhs);
			tryReplace(lhs, t1, rhs);
		}
	}
	void operator()(TypeVariable& t1, TypeOperator& t2) const
	{
		if (occurs(t1, t2))
			throw RecursiveUnification(t1, t2);

		env.replace(t1, rhs);
		tryReplace(lhs, t1, rhs);
	}
	void operator()(TypeOperator& t1, TypeVariable& t2) const
	{
		if (occurs(t2, t1))
			throw RecursiveUnification(t2, t1);

		env.replace(t2, lhs);
		tryReplace(rhs, t2, lhs);
	}
	void operator()(TypeOperator& t1, TypeOperator& t2) const
	{

		if (t1.name != t2.name || t1.types.size() != t2.types.size())
		{
			throw TypeError(t1, t2);
		}

		for (size_t ii = 0; ii < t1.types.size(); ii++)
		{
			boost::apply_visitor(Unify(env, t1.types[ii], t2.types[ii]), t1.types[ii], t2.types[ii]);
		}
	}

	TypeEnvironment& env;
	Type& lhs;
	Type& rhs;
};

Type& Name::typecheck(TypeEnvironment& env)
{
	return env.getType(this->name);
}

Type& Rational::typecheck(TypeEnvironment& env)
{
	return doubleType;
}

Type& Number::typecheck(TypeEnvironment& env)
{
	return intType;
}

Type& Let::typecheck(TypeEnvironment& env)
{
	TypeEnvironment& child = env.child();
	for (auto& bind : bindings)
	{
		child.bindName(bind.name, bind.expression->getType());
		Type& t = bind.expression->typecheck(child);
	}
	return expression->typecheck(child);
}

Type& Lambda::typecheck(TypeEnvironment& env)
{
	TypeEnvironment child = env.child();
	std::vector<Type> argTypes(arguments.size());
	for (size_t ii = 0; ii < argTypes.size(); ++ii)
	{
		child.bindName(arguments[ii], argTypes[ii]);
	}
	Type* returnType = &body->typecheck(child);

	for (auto arg = arguments.rbegin(); arg != arguments.rend(); ++arg)
	{
		Type& argType = child.getType(*arg);
		this->type = functionType(argType, *returnType);

		returnType = &this->type;
	}
	return *returnType;
}

Type& Apply::typecheck(TypeEnvironment& env)
{
	env.registerType(this->type);
	Type& funcType = function->typecheck(env);
	Type& argType = arguments[0]->typecheck(env);

	this->type = functionType(argType, TypeVariable());

	Unify(env, this->type, funcType);
	this->type = boost::get<TypeOperator>(this->type).types[1];
	for (size_t ii = 1; ii < arguments.size(); ii++)
	{
		auto& arg = arguments[ii];
		Type& argType = arg->typecheck(env);
		Type temp = functionType(argType, TypeVariable());

		Unify(env, temp, this->type);

		this->type = boost::get<TypeOperator>(temp).types[1];
	}
	return this->type;//TODO
}

Type& Case::typecheck(TypeEnvironment& env)
{
	Type& matchType = expression->typecheck(env);

	Type* returnType = nullptr;
	for (Alternative& alt : alternatives)
	{
		TypeEnvironment caseEnv = env.child();
		alt.pattern->addVariables(caseEnv, matchType);
		Type& t = alt.expression->typecheck(caseEnv);
		if (returnType == nullptr)//First alternative
			returnType = &t;
		else if (!(t == *returnType))
		{
			throw std::runtime_error("All case alternatives must have the same type");
		}
	}
	assert(returnType != nullptr);
	return *returnType;
}

//compile functions

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
