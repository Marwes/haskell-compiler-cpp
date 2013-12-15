#include <algorithm>
#include <sstream>
#include "Types.h"
#include "Expression.h"
#include "Module.h"
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/strong_components.hpp>

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
	return type;
}

Let::Let(std::vector<Binding>&& bindings, std::unique_ptr<Expression>&& expression)
	: bindings(std::move(bindings))
	, expression(std::move(expression))
	, isRecursive(false)
{
}

void Name::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);
}

void Number::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);
}

void Rational::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);
}

void Apply::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);

	function->accept(visitor);
	for (auto& arg : arguments)
		arg->accept(visitor);
}

void Lambda::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);

	body->accept(visitor);
}

void Let::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);

	for (Binding& bind : bindings)
		bind.expression->accept(visitor);
	expression->accept(visitor);
}

void Case::accept(ExpressionVisitor& visitor)
{
	visitor.visit(*this);

	expression->accept(visitor);
	for (auto& alt : alternatives)
		alt.expression->accept(visitor);
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



GOP toGOP(const std::string& op)
{
	if (op == "+" || op == "primIntAdd") return GOP::ADD;
	if (op == "-") return GOP::SUBTRACT;
	if (op == "*") return GOP::MULTIPLY;
	if (op == "/") return GOP::DIVIDE;
	if (op == "%") return GOP::REMAINDER;
	if (op == "==") return GOP::COMPARE_EQ;
	if (op == "/=") return GOP::COMPARE_NEQ;
	if (op == ">") return GOP::COMPARE_GT;
	if (op == "<") return GOP::COMPARE_LT;
	if (op == ">=") return GOP::COMPARE_GE;
	if (op == "<=") return GOP::COMPARE_LE;
	throw std::runtime_error("Unknown op" + op);
}

void PatternName::addVariables(TypeEnvironment& env, Type& type)
{
	unify(env, this->type, type);
	env.bindName(name, this->type);
	env.addNonGeneric(this->type);
}

Type getReturnType(Type t)
{
	while (t.which() == 1)
	{
		TypeOperator op = boost::get<TypeOperator>(t);
		if (op.name == "->")
		{
			t = op.types[1];
		}
		else
		{
			return t;
		}
	}
	return t;
}

void ConstructorPattern::addVariables(TypeEnvironment& env, Type& type)
{
	Type t = env.getFreshType(this->name);
	Type dataType = getReturnType(t);

	TypeOperator* funcType = &boost::get<TypeOperator>(t);
	for (size_t ii = 0; ii < patterns.size(); ++ii)
	{
		patterns[ii]->addVariables(env, funcType->types[0]);
		funcType = &boost::get<TypeOperator>(funcType->types[1]);
	}
	unify(env, dataType, type);
}

void ConstructorPattern::compileGCode(GCompiler& env, std::vector<size_t>& branches, std::vector<GInstruction>& instructions) const
{
	Variable var = env.getVariable(name);
	instructions.push_back(GInstruction(GOP::CASEJUMP, var.index & 0xFFFF));
	branches.push_back(instructions.size());
	instructions.push_back(GInstruction(GOP::JUMP));
}

Case::Case(std::unique_ptr<Expression> && expr, std::vector<Alternative> && alternatives)
	: expression(std::move(expr))
	, alternatives(std::move(alternatives))
{}

Type& Case::getType()
{
	return alternatives.at(0).expression->getType();
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


class Freshener : public boost::static_visitor<Type>
{
public:
	Freshener(TypeEnvironment& env)
		: env(env)
	{}

	Type operator()(const TypeVariable& type)
	{
		if (env.isGeneric(type))
		{
			if (mappings.count(type.id) != 0)
			{
				return mappings[type.id];
			}
			else
			{
				TypeVariable var;
				mappings[type.id] = var;
				env.updateConstraints(type, var);
				return var;
			}
		}
		return type;
	}
	Type operator()(const TypeOperator& type)
	{
		std::vector<Type> args;
		for (const Type& t : type.types)
		{
			args.emplace_back(boost::apply_visitor(*this, t));
		}
		return TypeOperator(type.name, std::move(args));
	}

private:
	TypeEnvironment& env;
	std::map<int, TypeVariable> mappings;
};

Type fresh(TypeEnvironment& env, const Type& type)
{
	Freshener f(env);
	return boost::apply_visitor(f, type);
}

TypeEnvironment::TypeEnvironment(Module* module)
	: parent(nullptr)
	, module(module == nullptr ? Module::prelude.get() : module)
{
	bindName("+", binop);
	bindName("-", binop);
	bindName("*", binop);
	bindName("/", binop);
	bindName("%", binop);
	bindName("==", binop);
	bindName("/=", binop);
	bindName("<", binop);
	bindName(">", binop);
	bindName("<=", binop);
	bindName(">=", binop);
}

TypeEnvironment::TypeEnvironment(TypeEnvironment && env)
	: module(std::move(env.module))
	, parent(std::move(env.parent))
	, namedTypes(std::move(env.namedTypes))
	, types(std::move(env.types))
	, nonGeneric(std::move(env.nonGeneric))
	, constraints(std::move(env.constraints))
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

const Type* findInModule(TypeEnvironment& env, Module& module, const std::string& name)
{
	auto found = std::find_if(module.bindings.begin(), module.bindings.end(), 
		[&name](const Binding& bind)
	{
		return bind.name == name;
	});
	if (found != module.bindings.end())
	{
		return &found->expression->getType();
	}
	for (auto& def : module.dataDefinitions)
	{
		for (auto& ctor : def.constructors)
		{
			if (ctor.name == name)
			{
				return &ctor.type;
			}
		}
	}
	for (auto& klass : module.classes)
	{
		for (auto& decl : klass.declarations)
		{
			if (decl.first == name)
			{
				return &decl.second.type;
			}
		}
	}
	for (auto& import : module.imports)
	{
		if (const Type* ret = findInModule(env, *import, name))
			return ret;
	}
	return nullptr;
}

const Type& TypeEnvironment::getType(const std::string& name)
{
	auto found = namedTypes.find(name);
	if (found != namedTypes.end())
		return *found->second;
	if (parent != nullptr)
		return parent->getType(name);
	if (module != nullptr)
	{
		const Type* t = findInModule(*this, *module, name);
		if (t != nullptr)
			return *t;
	}
	throw std::runtime_error("Could not find the identifier " + name);
}

Type TypeEnvironment::getFreshType(const std::string& name)
{
	return fresh(*this, getType(name));
}

void TypeEnvironment::updateConstraints(const TypeVariable& replaced, const TypeVariable& newVar)
{
	if (replaced == newVar)
		return;
	if (parent == nullptr)
	{
		//Add all constraints from the replaced variable
		auto found = constraints.find(replaced);
		if (found != constraints.end())
		{
			std::vector<std::string>& newConstraints = constraints[newVar];
			for (std::string& className : found->second)
			{
				//Only add the constraint if it is not already in the constraints of the replaced variable
				if (std::find(newConstraints.begin(), newConstraints.end(), className) == newConstraints.end())
				{
					if (isVariableLocked(replaced))
					{
						//Can't add a constraint to a variable that is locked which can be because of
						//typedeclarations or maybe number literals
						throw CannotAddConstraintError(replaced, className);
					}
					newConstraints.push_back(className);
				}
			}
			constraints.erase(found);
		}
	}
	else
	{
		parent->updateConstraints(replaced, newVar);
	}
}

bool hasInstance(const Module& module, const std::string& className, const Type& op)
{
	for (const Instance& instance : module.instances)
	{
		if (instance.className == className && instance.type == op)
		{
			return true;
		}
	}
	for (auto& import : module.imports)
	{
		if (hasInstance(*import, className, op))
			return true;
	}
	return false;
}

void TypeEnvironment::tryReplace(Type& toReplace, TypeVariable& replaceMe, const Type& replaceWith)
{
	if (toReplace.which() == 0)
	{
		TypeVariable& x = boost::get<TypeVariable>(toReplace);
		if (x == replaceMe)
		{
			if (replaceWith.which() == 0)//TypeVariable
			{
				//Merge the constraints from both variables
				updateConstraints(x, boost::get<TypeVariable>(replaceWith));
			}
			else
			{
				auto varConstraints = getConstraints(replaceMe);
				//Check that the TypeOperator fulfills all constraints of the variable
				for (const std::string& className : varConstraints)
				{
					assert(module != nullptr);
					if (!hasInstance(*module, className, replaceWith))
					{
						//Infer the Num type class as an Int for now
						//(Just assuming that Int has a Num instance)
						if (className == "Num" && replaceWith == intType)
						{
							continue;
						}
						else
						{
							throw TypeError(*this, replaceWith, toReplace);
						}
					}
				}
			}
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
	if (parent != nullptr)
		parent->replace(replaceMe, replaceWith);
}

void TypeEnvironment::addNonGeneric(const Type& type)
{
	nonGeneric.push_back(type);
}

bool TypeEnvironment::isGeneric(const TypeVariable& var) const
{
	for (const Type& type : nonGeneric)
	{
		if (occurs(var, type))
			return false;
	}
	if (parent != nullptr)
	{
		return parent->isGeneric(var);
	}
	return true;
}

void TypeEnvironment::addConstraint(const TypeVariable& var, const std::string& className)
{
	if (parent == nullptr)
		constraints[var].push_back(className);
	else
		parent->addConstraint(var, className);
}


const std::vector<std::string>& TypeEnvironment::getConstraints(const TypeVariable& var) const
{
	if (parent == nullptr)
	{
		auto found = constraints.find(var);
		if (found != constraints.end())
			return found->second;
		static const std::vector<std::string> empty;
		return empty;
	}
	else
	{
		return parent->getConstraints(var);
	}
}


void TypeEnvironment::lockVariable(TypeVariable var)
{
	lockedVariables.insert(var);
}
bool TypeEnvironment::isVariableLocked(TypeVariable var)
{
	return lockedVariables.find(var) != lockedVariables.end();
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
			env.tryReplace(lhs, t1, rhs);
		}
	}
	void operator()(TypeVariable& t1, TypeOperator& t2) const
	{
		if (occurs(t1, t2))
			throw RecursiveUnification(t1, t2);

		env.replace(t1, rhs);
		env.tryReplace(lhs, t1, rhs);
	}
	void operator()(TypeOperator& t1, TypeVariable& t2) const
	{
		if (occurs(t2, t1))
			throw RecursiveUnification(t2, t1);

		env.replace(t2, lhs);
		env.tryReplace(rhs, t2, lhs);
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

void unify(TypeEnvironment& env, Type& lhs, Type& rhs)
{
	Unify(env, lhs, rhs);
}

class DependencyVisitor : public ExpressionVisitor
{
public:
	DependencyVisitor(Graph& graph)
		: graph(graph)
	{}

	virtual void visit(Name& name)
	{
		if (bindings.count(name.name) == 0)
			return;

		boost::add_edge(bindings[function], bindings[name.name], graph);
	}

	std::string function;
	Graph& graph;
	std::map<std::string, Vertex> bindings;
};

Type& Name::typecheck(TypeEnvironment& env)
{
	env.registerType(this->type);
	this->type = env.getFreshType(this->name);
	return this->type;
}

Type& Rational::typecheck(TypeEnvironment& env)
{
	return doubleType;
}

Type& Number::typecheck(TypeEnvironment& env)
{
	return intType;
}

void addBindingsToGraph(Graph& graph, std::vector<Binding>& bindings)
{

	for (Binding& bind : bindings)
	{
		Vertex vert = boost::add_vertex(graph);
		graph[vert] = &bind;
	}
}
void typecheckDependecyGraph(TypeEnvironment& env, Graph& graph)
{
	DependencyVisitor visitor(graph);
	size_t ii = 0;
	for (auto& vertex : graph.m_vertices)
	{
		visitor.bindings.insert(std::make_pair(vertex.m_property->name, ii));
		ii++;
	}
	for (auto& vertex : graph.m_vertices)
	{
		visitor.function = vertex.m_property->name;
		vertex.m_property->expression->accept(visitor);
	}

	//Use Tarjan's strongly connected components algorithm to find mutually recursive bindings
	std::map<Vertex, size_t> verticesToGroups;
	boost::associative_property_map<std::map<Vertex, size_t>> map(verticesToGroups);
	boost::strong_components(graph, map);

	//The group indexes are ordered so that processing the groups in order
	//will give the correct typechecking order for bindings
	std::vector<Binding*> groupedBindings;
	size_t groupIndex = 0;
	while (true)
	{
		for (auto& vertToGroup : verticesToGroups)
		{
			Vertex vert = vertToGroup.first;
			size_t group = vertToGroup.second;
			if (group == groupIndex)
			{
				groupedBindings.push_back(graph[vert]);
			}
		}

		if (groupedBindings.empty())
		{
			break;
		}
		for (Binding* bind : groupedBindings)
		{
			env.bindName(bind->name, bind->expression->getType());
		}
		for (Binding* bind : groupedBindings)
		{
			Type newType = bind->expression->getType();
			env.addNonGeneric(newType);
			Type& actual = bind->expression->typecheck(env);
			unify(env, newType, actual);
		}

		groupIndex++;
		groupedBindings.clear();
	}
	for (auto& vert : graph.m_vertices)
	{
		Binding* bind = vert.m_property;
		bind->type.type = bind->expression->getType();
		//TODO update constraints here if they are not defined
		//bind->type.constraints
	}
}

void typecheckUnorderedBindings(TypeEnvironment& env, std::vector<Binding>& bindings)
{
	Graph graph;
	addBindingsToGraph(graph, bindings);
	typecheckDependecyGraph(env, graph);
}

Type& Let::typecheck(TypeEnvironment& env)
{
	isRecursive = true;
	TypeEnvironment& child = env.child();
	if (isRecursive)
	{
		typecheckUnorderedBindings(child, bindings);
	}
	else
	{
		for (auto& bind : bindings)
		{
			child.bindName(bind.name, bind.expression->getType());
			Type& t = bind.expression->typecheck(child);
		}
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
		child.addNonGeneric(boost::get<TypeVariable>(argTypes[ii]));
	}
	Type* returnType = &body->typecheck(child);

	for (auto arg = argTypes.rbegin(); arg != argTypes.rend(); ++arg)
	{
		this->type = functionType(*arg, *returnType);

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
	//Copy construct a object since we are assigning it to iteself
	this->type = Type(boost::get<TypeOperator>(this->type).types[1]);
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
		else
		{
			unify(env, *returnType, t);
		}
	}
	assert(returnType != nullptr);
	return *returnType;
}

//compile functions

//Find the position of the typevariable 'var' and then match this with the actual type
//In the function
const std::string* findMatchingTypeVariable(const Type& in, const Type& matcher, TypeVariable var)
{
	if (const TypeVariable* inVar = boost::get<TypeVariable>(&in))
	{
		if (*inVar == var)
		{
			auto op = boost::get<TypeOperator>(&matcher);
			return op == nullptr ? nullptr : &op->name;
		}
	}
	else if (const TypeOperator* inOp = boost::get<TypeOperator>(&in))
	{
		auto& matchOp = boost::get<TypeOperator>(matcher);
		assert(matchOp.types.size() == inOp->types.size());
		for (size_t ii = 0; ii < inOp->types.size(); ii++)
		{
			if (const std::string* ret = findMatchingTypeVariable(inOp->types[ii], matchOp.types[ii], var))
				return ret;
		}
	}
	return nullptr;
}

const std::string* findInstanceType(const Class& klass, const std::string& name, const Type& type)
{
	const TypeDeclaration& decl = klass.declarations.at(name);
	
	const std::string* ret = findMatchingTypeVariable(decl.type, type, klass.variable);
	if (ret == nullptr && klass.name == "Num")
	{
		//Default to Int if the Num class is undefined
		static std::string intName("Int");
		return &intName;
	}
	return ret;
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
	case VariableType::TYPECLASSFUNCTION:
		if (const std::string* nameOfInstanceType = findInstanceType(*var.klass, name, type))
		{
			//We have found the actual type for this class function so get that function directly
			std::string directVarName = "#" + *nameOfInstanceType + name;
			Variable fastVar = env.getVariable(directVarName);
			assert(fastVar.accessType == VariableType::TOPLEVEL);
			instructions.push_back(GInstruction(GOP::PUSH_GLOBAL, fastVar.index));
		}
		else
		{
			Variable dictVar = env.getVariable("$dict");
			if (dictVar.accessType == VariableType::STACK)
			{
				
			}
			else
			{
				assert(0);
			}
		}
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
			if (!bind.type.constraints.empty())
			{
				env.newStackVariable("$dict");
			}
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
	if (isRecursive)
	{
		env.popStack(bindings.size());
	}
}
void Lambda::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	assert(0);
}

class ClassEnvironment
{
public:

	std::vector<std::string> tupleArgs;
};

bool hasConstraints(TypeEnvironment& env, const Type& type)
{
	if (const TypeVariable* var = boost::get<TypeVariable>(&type))
	{
		return !env.getConstraints(*var).empty();
	}
	else if (const TypeOperator* op = boost::get<TypeOperator>(&type))
	{
		for (const Type& t : op->types)
		{
			if (hasConstraints(env, t))
				return true;
		}
	}
	return false;
}

void tryAddClassDictionary(GCompiler& compiler, std::vector<GInstruction>& instructions, const Type& type)
{
	if (!hasConstraints(compiler.typeEnv, type))
		return;

	Variable dictVar = compiler.getVariable("$dict");
	if (dictVar.accessType == VariableType::STACK)
	{
		instructions.push_back(GInstruction(GOP::PUSH, dictVar.index));
	}
	//Do nothing since we should be able to infer the actual function later from the type context
}

void Apply::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	if (Name* nameFunc = dynamic_cast<Name*>(function.get()))
	{
		try
		{
			GOP op = toGOP(nameFunc->name);
			assert(arguments.size() == 2);
			arguments[0]->compile(env, instructions, true);
			arguments[1]->compile(env, instructions, true);
			instructions.push_back(GInstruction(op));
			return;
		}
		catch (std::runtime_error&)
		{
		}
		try
		{
			const Type& type = env.typeEnv.getType(nameFunc->name);
			tryAddClassDictionary(env, instructions, type);
		}
		catch (std::runtime_error&)
		{
			//Did not find the function as a top  level function (probably)
			//This means the function cannot possibly need a dictionary so skip it
		}
	}

	for (int ii = arguments.size() - 1; ii >= 0; --ii)
	{
		//TODO
		arguments[ii]->compile(env, instructions, false);
		env.newStackVariable("");
	}
	function->compile(env, instructions, strict);
	env.popStack(arguments.size());
	if (strict && instructions[instructions.size() - 2].op == GOP::PACK)
		return;
	if (!strict && instructions.back().op == GOP::PACK)
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
	std::vector<size_t> branches;//vector holding the indexes of the JUMP instructions
	for (Alternative& alt : alternatives)
	{
		alt.pattern->compileGCode(env, branches, instructions);
	}
	for (size_t ii = 0; ii < alternatives.size(); ii++)
	{
		Alternative& alt = alternatives[ii];
		//Set the JUMP instructions value to the upcoming SPLIT instruction
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

		//Remove all the locals we allocated
		instructions.push_back(GInstruction(GOP::SLIDE, pattern.patterns.size()));
		for (auto& varName : pattern.patterns)
		{
			env.stackVariables.pop_back();
		}
		//Reuse the branch vector by storing this new instructions index
		branches[ii] = instructions.size();
		instructions.push_back(GInstruction(GOP::JUMP, 0));
	}
	for (size_t branch : branches)
	{
		//Update the jump instructions jump value
		instructions[branch].value = instructions.size();
	}
	if (strict)
		instructions.push_back(GInstruction(GOP::EVAL));
}
}
