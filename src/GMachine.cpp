#include <iostream>
#include "GMachine.h"

namespace MyVMNamespace
{
GEnvironment::GEnvironment(StackFrame<Address> stack, SuperCombinator* combinator)
	: stack(stack)
	, combinator(combinator)
{}

GEnvironment GEnvironment::child(SuperCombinator* combinator)
{
	return GEnvironment(StackFrame<Address>(&stack.top(), stack.size() - stack.stackSize()), combinator);
}

GMachine::GMachine()
	: heap(1024)//Just make sure the heap is large enough for small examples for now
{}

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
				Address* addr = &environment.stack.top() - instruction.value;
				assert(addr->getType() == APPLICATION);
				environment.stack.push(addr->getNode()->apply.arg);
			}
			break;
		case GOP::PUSH_GLOBAL:
			{
				Address addr = globals[instruction.value];
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