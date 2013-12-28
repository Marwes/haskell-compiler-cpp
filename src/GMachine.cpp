#include <iostream>
#include "GMachine.h"
#include "Tokenizer.h"
#include "Parser.h"
#include "Typecheck.h"

namespace MyVMNamespace
{
GEnvironment::GEnvironment(StackFrame<Address> stack, SuperCombinator* combinator)
	: stack(stack)
	, combinator(combinator)
{}

GEnvironment GEnvironment::child(SuperCombinator* combinator)
{
	Address* base = &stack.top() - combinator->arity;
	size_t stackSize = stack.size() - stack.stackSize() + combinator->arity;
	StackFrame<Address> newFrame(base, stackSize, combinator->arity);
	return GEnvironment(newFrame, combinator);
}

GMachine::GMachine()
	: debug(false)
{
	heap.reserve(1024);//Just make sure the heap is large enough for small examples for now
	assemblies.reserve(100);
	addAssembly(Assembly::prelude);
}

Assembly compileInputStream(std::istream& file, int startIndex)
{
	Tokenizer tokens(file);
	Parser parser(tokens);
	Module module = parser.module();
	TypeEnvironment typeEnv = module.typecheck();
	std::map<std::string, Assembly*> assemblies = { { "Prelude", &Assembly::prelude } };
	GCompiler comp(typeEnv, &module, startIndex, assemblies);

	return comp.compileModule(module);
}

void GMachine::compile(std::istream& input)
{
	addAssembly(compileInputStream(input, globals.size()));
}

Assembly& GMachine::addAssembly(Assembly inputAssembly)
{
	assemblies.push_back(std::move(inputAssembly));
	Assembly& assembly = assemblies.back();

	globals.resize(globals.size() + assembly.superCombinators.size() + assembly.instanceDictionaries.size() + assembly.ffiFunctions.size());
	for (auto& sc : assembly.superCombinators)
	{
		int index = assembly.globalIndices[sc.second.get()];
		heap.push_back(Node(assembly.superCombinators[sc.first].get()));
		globals[index] = Address::global(&heap.back());
	}
	for (InstanceDictionary& dict : assembly.instanceDictionaries)
	{
		int index = assembly.instanceIndices[dict.constraints];
		Address* ctor = new Address[dict.dictionary.size() + 1];
		heap.push_back(Node(0, ctor));
		globals[index] = Address::constructor(&heap.back());
		for (size_t ii = 0; ii < dict.dictionary.size(); ii++)
		{
			ctor[ii] = globals[assembly.globalIndices[dict.dictionary[ii]]];
		}
		ctor[dict.dictionary.size()] = Address::indirection(nullptr);//endmarker
	}
	for (auto& pair : assembly.ffiFunctions)
	{
		ForeignFunction& func = pair.second;
		heap.push_back(Node(func.function, func.arity));
		globals[func.index] = Address::functionPointer(&heap.back());
	}
	return assembly;
}


SuperCombinator* GMachine::getCombinator(const std::string& name)
{
	for (Assembly& assembly : assemblies)
	{
		auto found = assembly.superCombinators.find(name);
		if (found != assembly.superCombinators.end())
			return found->second.get();
	}
	return  nullptr;
}

void slide(GEnvironment& environment, const GInstruction& instruction)
{
	Address top = environment.stack.top();
	for (int i = 0; i < instruction.value; i++)
	{
		environment.stack.pop();
	}
	environment.stack.top() = top;
}

Address GMachine::executeMain()
{
	SuperCombinator* main = getCombinator("main");
	int mainIndex = -1;
	for (size_t ii = 0; ii < globals.size(); ++ii)
	{
		Address addr = globals[ii];
		if (addr.getType() == GLOBAL && addr.getNode()->global == main)
		{
			mainIndex = ii;
			break;
		}
	}
	assert(mainIndex != -1);
	SuperCombinator sc;
	sc.name == "__main";
	sc.arity = 0;
	sc.instructions = std::vector<GInstruction> { GInstruction(GOP::PUSH_GLOBAL, mainIndex), GInstruction(GOP::EVAL) };
	return evaluate(sc);
}

std::ostream& operator<<(std::ostream& out, const Address& addr)
{
	switch (addr.getType())
	{
	case CONSTRUCTOR:
		{
			out << "(";
			ConstructorNode ctor = addr.getNode()->constructor;
			for (size_t ii = 0; ctor.arguments[ii].getNode() != nullptr; ii++)
			{
				out << ctor.arguments[ii] << " ";
			}
			out << ")";
		}
		break;
	case APPLICATION:
		{
			out << "(";
			out << addr.getNode()->apply.func << " ";
			out << addr.getNode()->apply.arg;
			out << ")";
		}
		break;
	case NUMBER:
		{
			out << addr.getNode()->number;
		}
		break;
	case DOUBLE:
		{
			out << addr.getNode()->numberDouble;
		}
		break;
	case GLOBAL:
		{
			out << addr.getNode()->global->name;
		}
		break;
	case INDIRECTION:
		{
			out << addr.getNode()->indirection;
		}
		break;
	default:
		break;
	}
	return out;
}

template<class T>
T add(T l, T r)
{
	return l + r;
}
template<class T>
T subtract(T l, T r)
{
	return l - r;
}
template<class T>
T multiply(T l, T r)
{
	return l * r;
}
template<class T>
T divide(T l, T r)
{
	return l / r;
}
template<class T>
T remainder(T l, T r)
{
	return l % r;
}

template<int (func)(int, int)>
void binopInt(GEnvironment& environment, std::vector<Node>& heap)
{
	Address rhs = environment.stack.pop();
	Address lhs = environment.stack.top();
	int result = func(lhs.getNode()->number, rhs.getNode()->number);
	heap.push_back(Node(result));
	environment.stack.top() = Address::number(&heap.back());
}
template<double(func)(double, double)>
void binopDouble(GEnvironment& environment, std::vector<Node>& heap)
{
	Address rhs = environment.stack.pop();
	Address lhs = environment.stack.top();
	double result = func(lhs.getNode()->numberDouble, rhs.getNode()->numberDouble);
	heap.push_back(Node(result));
	environment.stack.top() = Address::numberDouble(&heap.back());
}


Address GMachine::evaluate(SuperCombinator& combinator)
{
	GEnvironment env(baseStack(), &combinator);
	execute(env);
	return env.stack.top();
}

void GMachine::execute(GEnvironment& environment)
{
	const std::vector<GInstruction>& code = environment.combinator->instructions;
	StackFrame<Address>& stack = environment.stack;

	for (size_t index = 0; index < code.size(); index++)
	{
		const GInstruction& instruction = code[index];
		switch (instruction.op)
		{
		case GOP::ALLOC:
			{
				for (int i = 0; i < instruction.value; i++)
				{
					heap.push_back(Node(nullptr));
					environment.stack.push(Address::indirection(&heap.back()));
				}
			}
			break;
		case GOP::EVAL:
			{
				static SuperCombinator unwind { "__uniwnd", Type(), 0, std::vector<GInstruction>{ GInstruction(GOP::UNWIND) } };
				GEnvironment child = environment.child(&unwind);
				child.stack.push(environment.stack.top());
				execute(child);
				environment.stack.top() = child.stack.top();
			}
			break;
		case GOP::MKAP:
			{
				Address func = environment.stack.top();
				environment.stack.pop();
				Address arg = environment.stack.top();
				heap.push_back(Node(func, arg));
				environment.stack.top() = Address::application(&heap.back());
			}
			break;
		case GOP::PACK:
			{
				int tag = instruction.value & (0xFFFF);//Low two bytes
				int arity = instruction.value >> 16;//High two bytes
				heap.push_back(Node(tag, new Address[arity+1]));
				Node& ctor = heap.back();
				for (int ii = 0; ii < arity; ii++)
				{
					ctor.constructor.arguments[ii] = stack.pop();
				}
				ctor.constructor.arguments[arity + 1] = Address::indirection(nullptr);//Use as end of this constructor
				stack.push(Address::constructor(&ctor));
			}
			break;
		case GOP::SPLIT:
			{
				Address top = stack.pop();
				assert(top.getType() == CONSTRUCTOR);
				ConstructorNode& ctor = top.getNode()->constructor;
				for (int ii = 0; ii < instruction.value; ii++)
				{
					assert(ctor.arguments[ii].getType() != NodeType::INDIRECTION || ctor.arguments[ii].getNode() != nullptr);
					stack.push(ctor.arguments[ii]);
				}
			}
			break;
		case GOP::CASEJUMP:
			{
				Address top = stack.top();
				assert(top.getType() == CONSTRUCTOR);
				ConstructorNode& ctor = top.getNode()->constructor;
				if (ctor.tag != instruction.value)
					index++;//Skip the next instruction which is the jump instruction
			}
			break;
		case GOP::JUMP:
			index = instruction.value - 1;
			break;
		case GOP::POP:
			for (int i = 0; i < instruction.value; i++)
			{
				environment.stack.pop();
			}
			break;
		case GOP::PUSH:
			{
				Address addr = environment.stack[instruction.value];
				environment.stack.push(addr);
			}
			break;
		case GOP::PUSH_DICTIONARY_MEMBER:
			{
				assert(stack.base().getType() == NodeType::CONSTRUCTOR);//Must be instance dictionary
				ConstructorNode& ctor = stack.base().getNode()->constructor;
				Address& func = ctor.arguments[instruction.value];
				stack.push(func);
			}
			break;
		case GOP::PUSH_GLOBAL:
			{
				Address addr = globals.at(instruction.value);
				environment.stack.push(addr);
			}
			break;
		case GOP::PUSH_INT:
			{
				heap.push_back(Node(instruction.value));
				environment.stack.push(Address::number(&heap.back()));
			}
			break;
		case GOP::PUSH_DOUBLE:
			{
				heap.push_back(Node(instruction.doubleValue));
				environment.stack.push(Address::number(&heap.back()));
			}
			break;
		case GOP::SLIDE:
			{
				slide(environment, instruction);
			}
			break;
		case GOP::UNWIND:
			{
				Address top = environment.stack.top();
				switch (top.getType())
				{
				case NUMBER:
					break;
				case APPLICATION:
					{
						Node& n = *top.getNode();
						environment.stack.push(n.apply.func);
						--index;//Redo the unwind instruction
					}
					break;
				case FUNCTION_POINTER:
					{
						int arity = top.getNode()->function.args;
						if (stack.stackSize() - 1 < size_t(arity))
						{
							while (stack.stackSize() > 1)
							{
								stack.pop();
							}
						}
						else
						{
							size_t ii = environment.stack.stackSize() - arity - 1;
							for (; ii < environment.stack.stackSize() - 1; ii++)
							{
								Address& addr = environment.stack[ii];
								assert(addr.getType() == APPLICATION);
								addr = addr.getNode()->apply.arg;
							}
							t_ffi_func func = top.getNode()->function.ptr;
							assert(func != nullptr);
							StackFrame<Address> newStack = stack.makeChildFrame(arity + 1);
							func(this, &newStack);
							Address result = newStack.top();
							for (int i = 0; i < arity; i++)
								environment.stack.pop();
							environment.stack.push(result);
						}
					}
					break;
				case GLOBAL:
					{
						SuperCombinator* comb = top.getNode()->global;
						if (environment.stack.stackSize() - 1 < size_t(comb->arity))
						{
							while (stack.stackSize() > 1)
							{
								stack.pop();
							}
						}
						else
						{
							//Before calling the function, replace all applications on the stack with the actual arguments
							//This gives faster access to a functions arguments when using PUSH
							size_t ii = environment.stack.stackSize() - comb->arity - 1;
							for (; ii < environment.stack.stackSize() - 1; ii++)
							{
								Address& addr = environment.stack[ii];
								assert(addr.getType() == APPLICATION);
								addr = addr.getNode()->apply.arg;
							}

							GEnvironment child = environment.child(comb);
							if (debug)
							{
								std::cerr << "Executing function '" << comb->name << "'" << std::endl;
								std::cerr << "Arguments { ";
								for (size_t i = 0; i < child.stack.stackSize(); i++)
								{
									std::cerr << child.stack[i];
								}
								std::cerr << " }" << std::endl;
							}
							execute(child);
							Address result = child.stack.top();
							for (int i = 0; i < comb->arity; i++)
								environment.stack.pop();
							environment.stack.push(result);
						}
					}
					break;
				case INDIRECTION:
					{
						environment.stack.top() = top.getNode()->indirection;
						--index;//Redo the unwind instruction
					}
					break;
				default:
					break;
				}
			}
			break;
		case GOP::UPDATE:
			{
				Address top = environment.stack.top();
				heap.push_back(Node(top));
				environment.stack[instruction.value] = Address::indirection(&heap.back());
			}
			break;
#define BINOP2(op, opname) \
		case GOP:: opname:\
			{\
			Address rhs = environment.stack.pop(); \
			Address lhs = environment.stack.top(); \
			int result = lhs.getNode()->number op rhs.getNode()->number; \
			heap.push_back(Node(result)); \
			environment.stack.top() = Address::number(&heap.back()); \
			}\
            break;
#define BINOP(f, name) case GOP:: name: binopInt<f>(environment, heap); break;

			BINOP(add<int>, ADD)
			BINOP(subtract<int>, SUBTRACT)
			BINOP(multiply<int>, MULTIPLY)
			BINOP(divide<int>, DIVIDE)
			BINOP(remainder<int>, REMAINDER)
			
#define BINOP_DOUBLE(f, name) case GOP:: name: binopDouble<f>(environment, heap); break;

			BINOP_DOUBLE(add<double>, ADD_DOUBLE)
			BINOP_DOUBLE(subtract<double>, SUBTRACT_DOUBLE)
			BINOP_DOUBLE(multiply<double>, MULTIPLY_DOUBLE)
			BINOP_DOUBLE(divide<double>, DIVIDE_DOUBLE)

#undef BINOP_DOUBLE
		case GOP::NEGATE:
			{
				Address x = environment.stack.top();
				heap.push_back(Node(-x.getNode()->number));
				environment.stack.top() = Address::number(&heap.back());
			}
			break;

			BINOP2(== , COMPARE_EQ)
			BINOP2(!= , COMPARE_NEQ)
			BINOP2(> , COMPARE_GT)
			BINOP2(>=, COMPARE_GE)
			BINOP2(< , COMPARE_LT)
			BINOP2(<=, COMPARE_LE)

#undef BINOP
#undef BINOP2

		default:
			std::cout << "Unimplemented instruction " << int(code[index].op) << std::endl;
			break;
		}
	}
	if (debug)
	{
		std::cerr << "Returning '" << stack.top() << "' from '" << environment.combinator->name << "'" << std::endl;
	}
}

};
