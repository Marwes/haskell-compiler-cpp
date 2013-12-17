#include <algorithm>
#include <sstream>
#include "Types.h"
#include "Expression.h"
#include "Module.h"
#include "Typecheck.h"
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/strong_components.hpp>

namespace MyVMNamespace
{

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



bool toGOP(const std::string& op, GOP& ret)
{
	if (op == "+" || op == "primIntAdd") { ret = GOP::ADD; return true; }
	if (op == "-" || op == "primIntSubtract") { ret = GOP::SUBTRACT; return true; }
	if (op == "*" || op == "primIntMultiply") { ret = GOP::MULTIPLY; return true; }
	if (op == "/" || op == "primIntDivide") { ret = GOP::DIVIDE; return true; }
	if (op == "%" || op == "primIntRemainder") { ret = GOP::REMAINDER; return true; }
	if (op == "==") { ret = GOP::COMPARE_EQ; return true; }
	if (op == "/=") { ret = GOP::COMPARE_NEQ; return true; }
	if (op == ">") { ret = GOP::COMPARE_GT; return true; }
	if (op == "<") { ret = GOP::COMPARE_LT; return true; }
	if (op == ">=") { ret = GOP::COMPARE_GE; return true; }
	if (op == "<=") { ret = GOP::COMPARE_LE; return true; }
	return false;
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

class EachFunctionArg
{
public:
	EachFunctionArg(Type& t)
		: type(&t)
	{}

	Type* next()
	{
		if (TypeOperator* op = boost::get<TypeOperator>(type))
		{
			if (op->name == "->")
			{
				type = &op->types[1];
				return &op->types[0];
			}
		}
		return nullptr;
	}
	Type* type;
};

Type& Lambda::typecheck(TypeEnvironment& env)
{
	TypeEnvironment child = env.child();

	Type ret;
	Type* returnType = &ret;
	for (std::string& arg : arguments)
	{
		this->type = functionType(TypeVariable(), *returnType);

		returnType = &type;
	}
	EachFunctionArg argTypes(type);
	for (std::string& arg : arguments)
	{
		Type* argType = argTypes.next();
		assert(argType != nullptr);
		child.bindName(arg, *argType);
		child.addNonGeneric(boost::get<TypeVariable>(*argType));

		//At the end of this loop this will be a pointer to the actual return type
		returnType = argTypes.type;
	}
	*returnType = body->typecheck(child);

	return this->type;
}

Type& Apply::typecheck(TypeEnvironment& env)
{
	env.registerType(this->type);
	Type& funcType = function->typecheck(env);
	Type& argType = arguments[0]->typecheck(env);

	this->type = functionType(argType, TypeVariable());

	unify(env, this->type, funcType);
	//Copy construct a object since we are assigning it to iteself
	this->type = Type(boost::get<TypeOperator>(this->type).types[1]);
	for (size_t ii = 1; ii < arguments.size(); ii++)
	{
		auto& arg = arguments[ii];
		Type& argType = arg->typecheck(env);
		Type temp = functionType(argType, TypeVariable());

		unify(env, temp, this->type);

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
		env.newStackVariable(bind.name);
		bind.expression->compile(env, instructions, false);
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
	assert(0 && "Local binds are not possible, need to be dealt with using lambda lifting");
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

bool tryAddClassDictionary(GCompiler& compiler, std::vector<GInstruction>& instructions, Expression* function)
{
	if (Name* nameFunc = dynamic_cast<Name*>(function))
	{
		const Type* type = nullptr;
		try
		{
			type = &compiler.typeEnv.getType(nameFunc->name);
		}
		catch (std::runtime_error&)
		{
			return false;
		}
		if (!hasConstraints(compiler.typeEnv, *type))
			return false;

		Variable dictVar = compiler.getVariable("$dict");
		if (dictVar.accessType == VariableType::STACK)
		{
			int index = compiler.getInstanceDictionaryIndex(nameFunc->name);
			if (index < 0)
			{
				//Only the dictionary is requested
				instructions.push_back(GInstruction(GOP::PUSH, dictVar.index));
			}
			else
			{
				instructions.push_back(GInstruction(GOP::PUSH_DICTIONARY_MEMBER, index));
			}
		}
		else//Should be able to infer the actual dictionary from the context
		{
			std::vector<TypeOperator> constraints = compiler.typeEnv.getConstraints(nameFunc->name, nameFunc->getType());

			int index = compiler.getDictionaryIndex(constraints);//TODO
			assert(index >= 0);
			instructions.push_back(GInstruction(GOP::PUSH_GLOBAL, index));
		}
		return true;
	}
	return false;
}

void Apply::compile(GCompiler& env, std::vector<GInstruction>& instructions, bool strict)
{
	if (Name* nameFunc = dynamic_cast<Name*>(function.get()))
	{
		GOP op;
		if (toGOP(nameFunc->name, op))
		{
			assert(arguments.size() == 2);
			arguments[0]->compile(env, instructions, true);
			arguments[1]->compile(env, instructions, true);
			instructions.push_back(GInstruction(op));
			return;
		}
	}

	bool addedDict = tryAddClassDictionary(env, instructions, function.get());
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
	for (size_t ii = 0; ii < arguments.size() + size_t(addedDict); ++ii)
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
