#include <algorithm>
#include "Typecheck.h"
#include "Module.h"

namespace MyVMNamespace
{
Type intType(TypeOperator("Int"));
Type doubleType(TypeOperator("Double"));


TypeVariable var;
Type binop = functionType(var, functionType(var, var));

Type createPairCtor(TypeEnvironment& env)
{
	std::vector<Type> args(2);
	args[0] = TypeVariable(env);
	args[1] = TypeVariable(env);
	Type pair(TypeOperator("(,)", args));
	return functionType(args[0], functionType(args[1], pair));
}

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
			if (mappings.count(type) != 0)
			{
				return mappings[type];
			}
			else
			{
				TypeVariable var;
				mappings[type] = var;
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
	std::map<TypeVariable, TypeVariable> mappings;
};

Type fresh(TypeEnvironment& env, const Type& type)
{
	Freshener f(env);
	return boost::apply_visitor(f, type);
}

TypeEnvironment::TypeEnvironment(Module* module, std::map<std::string, Assembly*> inputAssemblies)
	: parent(nullptr)
	, module(module)
	, assemblies(std::move(inputAssemblies))
	, uniqueVariableId(0)
{
	if (assemblies.count("Prelude") == 0)
	{
		assemblies.insert(std::make_pair("Prelude", &Module::prelude));
	}
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
	, uniqueVariableId(0)
{
}

TypeEnvironment TypeEnvironment::child()
{
	TypeEnvironment c(module);
	c.parent = this;
	return c;
}

TypeVariable TypeEnvironment::newTypeVariable()
{
	if (parent == nullptr)
	{
		TypeVariable var;
		var.id = uniqueVariableId++;
		return var;
	}
	else
		return parent->newTypeVariable();
}


void TypeEnvironment::bindName(const std::string& name, Type& type)
{
	namedTypes.insert(std::make_pair(name, &type));
	registerType(type);
}
void TypeEnvironment::registerType(Type& type)
{
	if (parent == nullptr)
		types.push_back(&type);
	else
		parent->registerType(type);
}

const Type* findInAssembly(const TypeEnvironment& env, Assembly& assembly, const std::string& name)
{
	auto found = assembly.superCombinators.find(name);
	if (found != assembly.superCombinators.end())
	{
		return &found->second->type;
	}
	for (Constructor& ctor : assembly.dataDefinitions)
	{
		if (ctor.name == name)
			return &ctor.type;
	}
	
	return nullptr;
}

const Type* findInModule(const TypeEnvironment& env, Module& module, const std::string& name)
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
		Assembly* assembly = env.getAssembly(import);
		if (assembly != nullptr)
		{
			if (const Type* ret = findInAssembly(env, *assembly, name))
				return ret;
		}
	}
	return nullptr;
}

const Type* TypeEnvironment::getType(const std::string& name) const
{
	auto found = namedTypes.find(name);
	if (found != namedTypes.end())
		return found->second;
	if (parent != nullptr)
		return parent->getType(name);
	if (module != nullptr)
	{
		const Type* t = findInModule(*this, *module, name);
		if (t != nullptr)
			return t;
	}
	else
	{

		const Type* t = findInAssembly(*this, Module::prelude, name);
		if (t != nullptr)
			return t;
	}
	return nullptr;
}

Type TypeEnvironment::getFreshType(const std::string& name)
{
	const Type* t = getType(name);
	if (t == nullptr)
		throw std::runtime_error("Could not find the identifier " + name);
	return fresh(*this, *t);
}

struct ConstraintFinder : public boost::static_visitor<>
{
	ConstraintFinder(const TypeEnvironment& env, std::vector<TypeOperator>& constraints, const Type& actualType)
		: env(env)
		, constraints(constraints)
		, actualType(actualType)
	{}

	void operator()(const TypeVariable& var) const
	{
		for (auto name : env.getConstraints(var))
		{
			std::vector<Type> args(1);
			args[0] = actualType;
			TypeOperator op(name, std::move(args));
			if (std::find(constraints.begin(), constraints.end(), op) == constraints.end())
				constraints.push_back(std::move(op));
		}
	}
	void operator()(const TypeOperator& op) const
	{
		auto actualOp = boost::get<TypeOperator>(actualType);
		assert(op.types.size() == actualOp.types.size());
		for (size_t ii = 0; ii < op.types.size(); ii++)
		{
			boost::apply_visitor(ConstraintFinder(env, constraints, actualOp.types[ii]), op.types[ii]);
		}
	}

	const TypeEnvironment& env;
	std::vector<TypeOperator>& constraints;
	const Type& actualType;
};

std::vector<TypeOperator> TypeEnvironment::getConstraints(const std::string& name, const Type& functionType) const
{
	const Type* type = getType(name);
	if (type == nullptr)
		throw std::runtime_error("Did not find the type for " + name + " when looking up the cosnstraints");
	std::vector<TypeOperator> constraints;
	boost::apply_visitor(ConstraintFinder(*this, constraints, functionType), *type);
	return constraints;
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
		}
	}
	else
	{
		parent->updateConstraints(replaced, newVar);
	}
}

bool hasInstance(const TypeEnvironment& env, const Assembly& assembly, const std::string& className, const Type& type)
{
	for (const TypeOperator& op : assembly.instances)
	{
		if (op.name == className && type == op.types[0])
		{
			return true;
		}
	}
	return false;
}

bool hasInstance(const TypeEnvironment& env, const Module& module, const std::string& className, const Type& op)
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
		Assembly* assembly = env.getAssembly(import);
		if (assembly != nullptr && hasInstance(env, *assembly, className, op))
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
				constraints.erase(x);
			}
			else
			{
				auto varConstraints = getConstraints(replaceMe);
				//Check that the TypeOperator fulfills all constraints of the variable
				for (const std::string& className : varConstraints)
				{
					//No module == cant find an instance (where 
					if (module == nullptr || !hasInstance(*this, *module, className, replaceWith))
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

void TypeEnvironment::removeNonGenerics(size_t n)
{
	for (size_t ii = 0; ii < n; ii++)
	{
		nonGeneric.pop_back();
	}
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

}