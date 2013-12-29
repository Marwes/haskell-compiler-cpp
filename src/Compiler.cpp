#include <algorithm>
#include "Compiler.h"
#include "Expression.h"
#include "Module.h"
#include "Typecheck.h"
#include "Stack.h"
#include "GMachine.h"

namespace MyVMNamespace
{

size_t numAssemblyIds(Assembly& a)
{
	return a.superCombinators.size() + a.instanceDictionaries.size() + a.ffiFunctions.size();
}

GCompiler::GCompiler(TypeEnvironment& typeEnv, Module* module, int globalStartIndex, std::map<std::string, Assembly*> assemblies)
	: module(module)
	, uniqueGlobalIndex(globalStartIndex)
	, typeEnv(typeEnv)
	, currentBinding(nullptr)
	, assembly(nullptr)
	, assemblies(std::move(assemblies))
{
	if (this->assemblies.count("Prelude") == 0)
	{
		this->assemblies.insert(std::make_pair("Prelude", &Assembly::prelude));
	}
	if (globalStartIndex == 0)
	{
		uniqueGlobalIndex = numAssemblyIds(Assembly::prelude);
	}
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

Variable findInModule(GCompiler& comp, Assembly& assembly, Assembly& import, const std::string& name)
{
	for (auto& pair : import.superCombinators)
	{
		if (pair.first == name)
		{
			size_t index = import.globalIndices[pair.second.get()];
			return Variable{ VariableType::TOPLEVEL, index, nullptr, &pair.second->type };
		}
	}
	for (Constructor& ctor : import.dataDefinitions)
	{
		if (ctor.name == name)
		{
			//TODO ctor.tag must be a way to retrieve this constructor
			if (ctor.tag > (1 << 16))
			{
				throw std::runtime_error("Number of constructors are to large");
			}if (ctor.arity > (1 << 16))
			{
				throw std::runtime_error("Arity of constructor " + std::string(ctor.name) + " are to large");
			}
			int index = (ctor.arity << 16) | ctor.tag;
			return Variable{ VariableType::CONSTRUCTOR, index, nullptr };
		}
	}
	for (Class& klass : import.classes)
	{
		size_t ii = 0;
		for (auto& x : klass.declarations)
		{
			if (x.second.name == name)
			{
				return Variable { VariableType::TYPECLASSFUNCTION, ii, &klass, nullptr };
			}
			ii++;
		}
	}
	auto foundFFI = import.ffiFunctions.find(name);
	if (foundFFI != import.ffiFunctions.end())
	{
		int index = foundFFI->second.index;
		return Variable{ VariableType::TOPLEVEL, index, nullptr, nullptr };
	}
	return Variable{ VariableType::NONE, -1, nullptr, nullptr };
}

Variable findInModule(GCompiler& comp, Assembly& assembly, Module& module, const std::string& name)
{
	for (Binding& bind : module.bindings)
	{
		if (bind.name == name)
		{
			size_t index = assembly.globalIndices[&comp.getGlobal(name)];
			return Variable { VariableType::TOPLEVEL, index, nullptr, &bind.expression->getType() };
		}
	}

	for (auto& dataDef : module.dataDefinitions)
	{
		for (auto& ctor : dataDef.constructors)
		{
			if (ctor.name == name)
			{
				//TODO ctor.tag must be a way to retrieve this constructor
				if (ctor.tag > (1 << 16))
				{
					throw std::runtime_error("Number of constructors are to large");
				}if (ctor.arity > (1 << 16))
				{
					throw std::runtime_error("Arity of constructor " + std::string(ctor.name) + " are to large");
				}
				int index = (ctor.arity << 16) | ctor.tag;
				return Variable { VariableType::CONSTRUCTOR, index, nullptr };
			}
		}
	}
	for (Class& klass : module.classes)
	{
		size_t index = 0;
		for (auto& decl : klass.declarations)
		{
			if (decl.second.name == name)
			{
				return Variable { VariableType::TYPECLASSFUNCTION, index, &klass };
			}
			index++;
		}
	}
	if (name.size() > 0 && name[0] == '#')
	{
		for (Instance& instance : module.instances)
		{
			const std::string& instanceName = boost::get<TypeOperator>(instance.type).name;
			if (name.compare(1, instanceName.size(), instanceName) == 0)
			{
				for (Binding& binding : instance.bindings)
				{
					if (name == binding.name)
					{
						return Variable{ VariableType::NONE, -1, nullptr, &binding.expression->getType() };
					}
				}
			}
		}
	}

	for (auto& import : module.imports)
	{
		Assembly* importedAssembly = comp.getAssembly(import);
		if (importedAssembly != nullptr)
		{
			Variable ret = findInModule(comp, assembly, *importedAssembly, name);
			if (ret.accessType != VariableType::NONE)
				return ret;
		}
	}
	return Variable { VariableType::NONE, -1, nullptr };
}

Variable GCompiler::getVariable(const std::string& name)
{
	auto found = std::find(stackVariables.begin(), stackVariables.end(), name);
	if (found != stackVariables.end())
	{
		int index = std::distance(stackVariables.begin(), found);
		int distanceFromStackTop = stackVariables.size() - index - 1;
		return Variable { VariableType::STACK, index, nullptr };
	}
	if (assembly != nullptr)
	{
		auto foundGlobal = assembly->superCombinators.find(name);
		if (foundGlobal != assembly->superCombinators.end())
		{
			int i = assembly->globalIndices[foundGlobal->second.get()];
			return Variable{ VariableType::TOPLEVEL, i, nullptr };
		}
	}
	if (module != nullptr)
	{
		return findInModule(*this, *assembly, *module, name);
	}
	return Variable { VariableType::NONE, -1, nullptr };
}

SuperCombinator& GCompiler::getGlobal(const std::string& name)
{
	auto found = assembly->superCombinators.find(name);
	if (found == assembly->superCombinators.end())
	{
		auto& ptr = assembly->superCombinators[name] = std::unique_ptr<SuperCombinator>(new SuperCombinator());
		ptr->name = name;
		assembly->globalIndices[ptr.get()] = uniqueGlobalIndex++;
		return *ptr;
	}
	return *found->second;
}


int GCompiler::getDictionaryIndex(const std::vector<TypeOperator>& constraints)
{
	for (auto a : assemblies)
	{
		auto found = a.second->instanceIndices.find(constraints);
		if (found != a.second->instanceIndices.end())
		{
			return found->second;
		}
	}
	//Add a new dictionary
	std::vector<SuperCombinator*> dict;
	for (const TypeOperator& op : constraints)
	{
		std::map<Type, std::vector<SuperCombinator*>>& klass = classDictionaries[op.name];
		std::vector<SuperCombinator*>& instanceFunctions = klass[op.types[0]];
		for (SuperCombinator* comb : instanceFunctions)
		{
			dict.push_back(comb);
		}
	}
	assembly->instanceDictionaries.push_back(InstanceDictionary { constraints, std::move(dict) });
	return assembly->instanceIndices[assembly->instanceDictionaries.back().constraints] = uniqueGlobalIndex++;
}

int GCompiler::getInstanceDictionaryIndex(const std::string& function) const
{
	for (const TypeOperator& op : currentBinding->type.constraints)
	{
		auto klass = classDictionaries.find(op.name);
		int ii = 0;
		for (SuperCombinator* comb : klass->second.begin()->second)
		{
			//Test if the end of the name is equal to the function
			if (comb->name.size() >= function.size()
				&& std::equal(comb->name.end() - function.size(), comb->name.end(), function.begin()))
				return ii;
			ii++;
		}
	}
	return -1;
}

const Binding& GCompiler::getCurrentBinding() const
{
	assert(currentBinding != nullptr);
	return *currentBinding;
}


SuperCombinator& GCompiler::compileBinding(Binding& binding, const std::string& name)
{
	currentBinding = &binding;

	SuperCombinator& sc = getGlobal(name);
	sc.arity = 0;
	stackVariables.clear();
	if (Lambda* lambda = dynamic_cast<Lambda*>(binding.expression.get()))
	{
		sc.arity = lambda->arguments.size();
		if (!binding.type.constraints.empty())
		{
			sc.arity++;
			newStackVariable("$dict");
		}
		for (auto arg = lambda->arguments.rbegin(); arg != lambda->arguments.rend(); ++arg)
		{
			stackVariables.push_back(*arg);
		}
		lambda->body->compile(*this, sc.instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		sc.instructions.push_back(GInstruction(GOP::POP, sc.arity));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}
	else
	{
		sc.arity = 0;
		binding.expression->compile(*this, sc.instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		//sc.instructions.push_back(GInstruction(GOP::POP, 0));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}

	currentBinding = nullptr;
	return sc;
}


Assembly GCompiler::compileExpression(Expression& expr, const std::string& name, bool strict)
{
	Assembly result;
	assembly = &result;
	SuperCombinator& sc = getGlobal(name);
	sc.arity = 0;
	sc.type = expr.getType();
	expr.compile(*this, sc.instructions, strict);
	assembly = nullptr;
	return result;
}

void GCompiler::compileInstance(Instance& instance)
{
	assembly->instances.push_back(TypeOperator(instance.className, { instance.type }));
	std::vector<SuperCombinator*> functions;
	for (Binding& bind : instance.bindings)
	{
		SuperCombinator& sc = compileBinding(bind, bind.name);
		functions.push_back(&sc);
	}
	auto lowBound = classDictionaries.lower_bound(instance.className);
	if (lowBound == classDictionaries.end() || classDictionaries.key_comp()(instance.className, lowBound->first))
	{
		//Key does not exist
		lowBound = classDictionaries.insert(std::make_pair(instance.className, std::map < Type, std::vector < SuperCombinator* >> ())).first;
	}
	std::map<Type, std::vector<SuperCombinator*>>& instances = lowBound->second;
	instances.insert(std::make_pair(instance.type, functions));
}

Assembly GCompiler::compileModule(Module& module)
{
	Assembly result;
	assembly = &result;

	//Assign unique numbers for the tags so they can be correctly retrieved
	for (DataDefinition& dataDef : module.dataDefinitions)
	{
		int tag = 0;
		for (Constructor& ctor : dataDef.constructors)
		{
			ctor.tag = tag++;
			result.dataDefinitions.push_back(ctor);
		}
	}
	assembly->classes = module.classes;

	for (Instance& instance : module.instances)
	{
		compileInstance(instance);
	}
	for (Binding& bind : module.bindings)
	{
		compileBinding(bind, bind.name);
	}
	for (auto& pair : result.superCombinators)
	{
		const std::string& name = pair.first;
		SuperCombinator& comb = *pair.second;
		const Type* type = findInModule(*this, *assembly, module, name).type;
		assert(type != nullptr);
		comb.type = *type;
	}
	assembly = nullptr;
	return result;
}

void addPrimitiveFunctions(std::vector<Binding>& bindings, const std::string& typeName)
{
	Type type = functionType(TypeOperator(typeName), functionType(TypeOperator(typeName), TypeOperator(typeName)));

	std::array<const char*, 4> functions = { "Add", "Subtract", "Multiply", "Divide" };
	for (const char* func : functions)
	{
		std::string name = "prim" + typeName + func;
		bindings.push_back(Binding(name, std::unique_ptr<Expression>(new Name("undefined"))));
		bindings.back().type.type = type;
	}
}

template<bool (*op)(int, int)>
int primitiveBoolBinop(GMachine* machine, StackFrame<Address>* stackPtr)
{
	StackFrame<Address>& stack = *stackPtr;
	assert(stack.stackSize() == 3);//2 arguments + function ptr
	assert(stack[0].getType() == NUMBER && stack[1].getType() == NUMBER);
	if (op(stack[1].getNode()->number, stack[0].getNode()->number))
	{
		machine->heap.push_back(Node(1, nullptr));
	}
	else
	{
		machine->heap.push_back(Node(0, nullptr));
	}
	stack.top() = Address::constructor(&machine->heap.back());
	return 0;
}
template<bool(*op)(double, double)>
int primitiveBoolDoubleBinop(GMachine* machine, StackFrame<Address>* stackPtr)
{
	StackFrame<Address>& stack = *stackPtr;
	assert(stack.stackSize() == 3);//2 arguments + function ptr
	assert(stack[0].getType() == DOUBLE && stack[1].getType() == DOUBLE);
	if (op(stack[1].getNode()->numberDouble, stack[0].getNode()->numberDouble))
	{
		machine->heap.push_back(Node(1, nullptr));
	}
	else
	{
		machine->heap.push_back(Node(0, nullptr));
	}
	stack.top() = Address::constructor(&machine->heap.back());
	return 0;
}

template<class T>
bool primEq(T l, T r)
{
	return l == r;
}
template<class T>
bool primLt(T l, T r)
{
	return l < r;
}
template<class T>
bool primGt(T l, T r)
{
	return l > r;
}
template<class T>
bool primLe(T l, T r)
{
	return l <= r;
}
template<class T>
bool primGe(T l, T r)
{
	return l >= r;
}

template<class T>
T& getNode(Node& n)
{
	static_assert(false, "Need to specify a valid type for getNode");
}
template<>
double& getNode(Node& n)
{
	return n.numberDouble;
}
template<>
int& getNode(Node& n)
{
	return n.number;
}

template<class From, class To>
To primCast(From u)
{
	return static_cast<To>(u);
}

template<Address (*makeAddress)(Node*), class From, class To>
int primConvertNumber(GMachine* machine, StackFrame<Address>* stackPtr)
{
	StackFrame<Address>& stack = *stackPtr;
	assert(stack.stackSize() == 2);//1 arguments + function ptr
	assert(stack[0].getType() == NUMBER || stack[0].getType() == DOUBLE);
	
	To to = primCast<From, To>(getNode<From>(*stack[0].getNode()));
	
	machine->heap.push_back(Node(to));
	
	stack.top() = makeAddress(&machine->heap.back());
	return 0;
}


Assembly createPrelude()
{
	Module prelude;
	prelude.imports.clear();//Remove prelude from itself
	{
		std::vector<Type> args(2);
		args[0] = TypeVariable();
		args[1] = TypeVariable();
		Constructor ctor("(,)", functionType(args[0], functionType(args[1], TypeOperator("(,)", args))), 0, 2);
		DataDefinition def;
		def.name = "(,)";
		def.constructors.push_back(ctor);
		prelude.dataDefinitions.push_back(def);
	}

	{
		std::vector<Type> args(1);
		args[0] = TypeVariable();
		TypeOperator listType("[]", args);
		Constructor ctor(":", functionType(args[0], functionType(listType, listType)), 0, 2);
		Constructor ctor2("[]", listType, 1, 0);
		DataDefinition def;
		def.name = "[]";
		def.constructors.push_back(ctor);
		def.constructors.push_back(ctor2);
		prelude.dataDefinitions.push_back(def);
	}
	prelude.bindings.push_back(Binding("undefined", std::unique_ptr<Expression>(new Name("undefined"))));

	addPrimitiveFunctions(prelude.bindings, "Int");
	addPrimitiveFunctions(prelude.bindings, "Double");

	{
		prelude.bindings.push_back(Binding("primIntRemainder", std::unique_ptr<Expression>(new Name("undefined"))));
		TypeVariable varIntAdd;
		prelude.bindings.back().expression->getType() = functionType(varIntAdd, functionType(varIntAdd, varIntAdd));
	}

	TypeEnvironment typeEnv = prelude.typecheck();
	GCompiler comp(typeEnv, &prelude);
	Assembly result = comp.compileModule(prelude);
	int globalIndex = numAssemblyIds(result);

#define PRIM(name, funcName)\
	{\
	ForeignFunction func; \
	func.function = primitiveBoolBinop < funcName<int> >; \
	func.index = globalIndex++; \
	func.arity = 2; \
	result.ffiFunctions[#name] = func; \
}
	PRIM(primIntEq, primEq);
	PRIM(primIntGt, primGt);
	PRIM(primIntLt, primLt);
	PRIM(primIntGe, primGe);
	PRIM(primIntLe, primLe);

#undef PRIM
#define PRIM(name, funcName)\
	{\
	ForeignFunction func; \
	func.function = primitiveBoolDoubleBinop < funcName<double> >; \
	func.index = globalIndex++; \
	func.arity = 2; \
	result.ffiFunctions[#name] = func; \
}
	PRIM(primDoubleEq, primEq);
	PRIM(primDoubleGt, primGt);
	PRIM(primDoubleLt, primLt);
	PRIM(primDoubleGe, primGe);
	PRIM(primDoubleLe, primLe);

#undef PRIM

#define PRIM_CAST(addr, From, from, To, to)\
	{\
	ForeignFunction func; \
	func.function = primConvertNumber<addr, from , to>; \
	func.arity = 1; \
	func.index = globalIndex++; \
	result.ffiFunctions["prim"  From  "To"  To] = func; \
	}
	PRIM_CAST(Address::number, "Double", double, "Int", int);
	PRIM_CAST(Address::numberDouble, "Int", int, "Double", double);
#undef PRIM_CAST
	return result;
}
Assembly Assembly::prelude(createPrelude());
}
