#include <iostream>
#include "GMachine.h"
#include "Tokenizer.h"
#include "Parser.h"

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
}


void compileBinding(GCompiler& comp, Binding& binding, const std::string& name)
{
	comp.stackVariables.clear();
	if (Lambda* lambda = dynamic_cast<Lambda*>(binding.expression.get()))
	{
		for (auto arg = lambda->arguments.rbegin(); arg != lambda->arguments.rend(); ++arg)
		{
			comp.stackVariables.push_back(*arg);
		}
		SuperCombinator& sc = comp.getGlobal(name);
		sc.arity = lambda->arguments.size();
		lambda->body->compile(comp, sc.instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		sc.instructions.push_back(GInstruction(GOP::POP, sc.arity));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}
	else
	{
		SuperCombinator& sc = comp.getGlobal(name);
		sc.arity = 0;
		binding.expression->compile(comp, comp.getGlobal(name).instructions, true);
		sc.instructions.push_back(GInstruction(GOP::UPDATE, 0));
		//sc.instructions.push_back(GInstruction(GOP::POP, 0));
		sc.instructions.push_back(GInstruction(GOP::UNWIND));
	}
}

void GMachine::compile(std::istream& input)
{
	Tokenizer tokens(input);
	Parser parser(tokens);
	Module module = parser.module();

	module.typecheck();

	//Assign unique numbers for the tags so they can be correctly retrieved
	for (DataDefinition& dataDef : module.dataDefinitions)
	{
		int tag = 0;
		for (Constructor& ctor : dataDef.constructors)
		{
			ctor.tag = tag++;
			dataDefinitions.push_back(ctor);
		}
	}

	GCompiler comp(&module);
	for (Instance& instance : module.instances)
	{
		for (Binding& bind : instance.bindings)
		{
			std::string name = "#" + boost::get<TypeOperator>(instance.type).name + bind.name;
			compileBinding(comp, bind, name);
		}
	}
	for (Binding& bind : module.bindings)
	{
		compileBinding(comp, bind, bind.name);
	}

	superCombinators = std::move(comp.globals);
	globals.resize(superCombinators.size());
	for (auto& sc : superCombinators)
	{
		int index = comp.globalIndices[sc.second.get()];
		heap.push_back(Node(superCombinators[sc.first].get()));
		globals[index] = Address::global(&heap.back());
	}
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
	SuperCombinator* main = superCombinators.at("main").get();
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
	GEnvironment env(baseStack(), &sc);
	execute(env);
	return env.stack.top();
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
				static SuperCombinator unwind { "__uniwnd", 0, std::vector<GInstruction>{ GInstruction(GOP::UNWIND) } };
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
#define BINOP(op, opname) \
		case GOP:: opname:\
			{\
			Address rhs = environment.stack.pop(); \
			Address lhs = environment.stack.top(); \
			int result = lhs.getNode()->number op rhs.getNode()->number; \
			heap.push_back(Node(result)); \
			environment.stack.top() = Address::number(&heap.back()); \
			}\
            break;

			BINOP(+, ADD)
			BINOP(-, SUBTRACT)
			BINOP(*, MULTIPLY)
			BINOP(/ , DIVIDE)
			BINOP(%, REMAINDER)

		case GOP::NEGATE:
			{
				Address x = environment.stack.top();
				heap.push_back(Node(-x.getNode()->number));
				environment.stack.top() = Address::number(&heap.back());
			}
			break;

			BINOP(== , COMPARE_EQ)
			BINOP(!= , COMPARE_NEQ)
			BINOP(> , COMPARE_GT)
			BINOP(>=, COMPARE_GE)
			BINOP(< , COMPARE_LT)
			BINOP(<=, COMPARE_LE)

#undef BINOP

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
