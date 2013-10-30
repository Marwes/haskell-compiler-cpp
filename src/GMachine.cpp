#include <iostream>
#include "GMachine.h"
#include "Tokenizer.h"
#include "Parser.h"
#include "Compiler.h"

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
{
	heap.reserve(1024);//Just make sure the heap is large enough for small examples for now
}


void GMachine::compile(std::istream& input)
{
	Tokenizer tokens(input);
	Parser parser(tokens);
	Module module = parser.toplevel();

	GCompiler comp;
	for (Binding& bind : module.bindings)
	{
		comp.stackVariables.clear();
		std::vector<GInstruction> instructions;
		if (Lambda* lambda = dynamic_cast<Lambda*>(bind.expression.get()))
		{
			for (auto& arg : lambda->arguments)
			{
				comp.stackVariables.push_back(arg);
			}
			SuperCombinator& sc = comp.getGlobal(bind.name);
			sc.arity = lambda->arguments.size();
			lambda->expression->compile(comp, sc.instructions);
			sc.instructions.push_back(GInstruction(GOP::SLIDE, sc.arity + 1));
			sc.instructions.push_back(GInstruction(GOP::UNWIND));
		}
		else
		{
			SuperCombinator& sc = comp.getGlobal(bind.name);
			bind.expression->compile(comp, comp.getGlobal(bind.name).instructions);
			sc.instructions.push_back(GInstruction(GOP::SLIDE, 1));
			sc.instructions.push_back(GInstruction(GOP::UNWIND));
		}
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
	environment.stack.push(top);
}

void GMachine::execute(GEnvironment& environment)
{
	const std::vector<GInstruction>& code = environment.combinator->instructions;

	for (size_t index = 0; index < code.size(); index++)
	{
		const GInstruction& instruction = code[index];
		switch (instruction.op)
		{
		case GOP::ALLOC:
			break;
		case GOP::EVAL:
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
		case GOP::POP:
			break;
		case GOP::PUSH:
			{
				Address* addr = &environment.stack[environment.combinator->arity - instruction.value - 1];
				assert(addr->getType() == APPLICATION);
				environment.stack.push(addr->getNode()->apply.arg);
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
					environment.stack.push(top.getNode()->apply.func);
					--index;//Redo the unwind instruction
					break;
				case GLOBAL:
					{
						SuperCombinator* comb = top.getNode()->global;

						GEnvironment child = environment.child(comb);
						execute(child);
					}
					break;
				default:
					break;
				}
			}
			break;
		case GOP::UPDATE:
			break;

		default:
			std::cout << "Unimplemented instruction " << int(code[index].op) << std::endl;
			break;
		}
	}
}

};