#include <assert.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include "VM.h"
#include "Method.h"

namespace MyVMNamespace
{

#define OP_CLASS(op, a0, a1, a2) \
class OP_##op \
{\
    OP_##op(Instruction& i) : value(i) {}\
    VMInt a0() { return value.arg0;}\
    VMInt a1() { return value.arg1;}\
    VMInt a2() { return value.arg2;}\
    const Instruction& value;\
};

OP_CLASS(MOVE, from, to, na);

class VMI
{
public:
    static void op_move(MethodEnvironment& environment, Instruction current)
    {
        environment.stackFrame.at<VMInt>(current.arg0) = environment.stackFrame.at<VMInt>(current.arg1);
    }
    
    static void op_load(MethodEnvironment& environment, Instruction current)
    {
		StackObject& o = environment.stackFrame[current.arg0];
        environment.stackFrame.push(o);
    }

	static void op_load_function(MethodEnvironment& environment, Instruction current)
	{
		FunctionDefinition* f = environment.assembly->getFunction(current.arg0);
		StackObject o;
		o.pointerValue = f;
		environment.stackFrame.push(f);
	}

    static void op_load_int_constant(MethodEnvironment& environment, Instruction& current)
    {
        environment.stackFrame.push(current.arg0);
    }

    static void op_load_string_constant(MethodEnvironment& environment, Instruction& current)
    {
		assert(0);
        //String* string = data_cast<String*>(environment.method->data[current.arg0].get());

		//environment.stackFrame.push(string);
    }

    static void op_branch_true(MethodEnvironment& environment, Instruction& current, size_t& instructionPointer)
    {
        StackObject& obj = environment.stackFrame.top();
        if (obj.intValue != 0)
        {
            assert(current.arg0 < (int)environment.function->instructions.size());
            instructionPointer = current.arg0;
        }
	}
	static void op_jump(MethodEnvironment& environment, Instruction& current, size_t& instructionPointer)
	{
		assert(current.arg0 < (int) environment.function->instructions.size());
		instructionPointer = current.arg0;
	}

    static void op_newobject(MethodEnvironment& environment, Instruction current)
    {
        VMInt size = current.arg0;
        Object* obj = static_cast<Object*>(malloc(size));
        std::memset(obj, 0, size);

        environment.stackFrame.push(obj);
    }

    static void op_getfield(MethodEnvironment& environment, Instruction current)
    {
		StackObject* obj = &environment.stackFrame.top();
		StackObject& field = obj->pointerValue->getField(current.arg0);
        environment.stackFrame[current.arg1] = field;
	}

	static void op_setfield(MethodEnvironment& environment, Instruction current)
	{
		StackObject* obj = &environment.stackFrame.top();

		obj->pointerValue->getField(current.arg0) = environment.stackFrame[current.arg1];
	}

    static void op_call(VM& vm, MethodEnvironment& environment, Instruction current)
    {
		assert(0);//TODO
	}

	static void op_calli(VM& vm, MethodEnvironment& environment, Instruction current)
	{
		FunctionDefinition* func = vm.assembly.getFunction(current.arg0);
		assert(func);

		MethodEnvironment newEnvironment(&vm.assembly, environment.stackFrame.makeChildFrame(func->numArguments), func);

		vm.execute(newEnvironment);
		size_t i = environment.stackFrame.size() - func->numArguments;
		environment.stackFrame[i] = newEnvironment.stackFrame.top();
	}

	static void op_callnative(VM& vm, MethodEnvironment& environment, Instruction current)
	{
		VM_Function func = vm.assembly.getNativeFunction(current.arg0);
		assert(func);
		func(vm, environment.stackFrame);
	}

	static void op_return(VM& vm, MethodEnvironment& environment, Instruction current)
	{
		environment.stackFrame[0] = environment.stackFrame.top();
	}

	static void op_pop(VM& vm, MethodEnvironment& environment, Instruction current)
	{
		environment.stackFrame.pop();
	}



	template<class T, T (*op)(T, T)>
	static void op_binop(MethodEnvironment& environment, Instruction current)
	{
		T rhs = getObject<T>(environment.stackFrame.top());
		environment.stackFrame.pop();
		T lhs = getObject<T>(environment.stackFrame.top());
		StackObject o;
		getObject<T>(o) = op(lhs, rhs);
		environment.stackFrame.top() = o;
	}

	template<class T>
	static T op_add(T l, T r)
	{
		return l + r;
	}
	template<class T>
	static T op_subtract(T l, T r)
	{
		return l - r;
	}
	template<class T>
	static T op_multiply(T l, T r)
	{
		return l * r;
	}
	template<class T>
	static T op_divide(T l, T r)
	{
		return l / r;
	}
	static VMInt op_remainder(VMInt l, VMInt r)
	{
		return l % r;
	}
	template<class T>
	static T op_equal(T l, T r)
	{
		return l == r;
	}
	template<class T>
	static T op_and(T l, T r)
	{
		return l && r;
	}
};

#define DO_TOP_BINOP(op, environment, instruction) {\
	VMInt rhs = environment.stackFrame.top().intValue; \
	environment.stackFrame.pop(); \
	VMInt lhs = environment.stackFrame.top().intValue; \
	StackObject o; \
	o.intValue = lhs op rhs; \
	environment.stackFrame.top() = o; }

typedef void (*execute_function_t)(VM& vm, Instruction current); 


MethodEnvironment::~MethodEnvironment()
{
	//TODO
#if 0
    const StackLayout& stackLayout = this->function->stackLayout;
    //cleanup the stack
    for (size_t ii = 0; ii < stackFrame.size() && ii < stackLayout.types.size(); ++ii)
    {
        TypeEnum type = stackLayout.types[ii];
        switch (type)
        {
        case TYPE_ARRAY:
        case TYPE_CLASS:
            stackFrame[ii].pointerValue->removeReference();
            break;
        case TYPE_METHOD:
            break;
        default:
            break;
        }

    }
#endif
}

VM::VM()
{ }

VM::~VM()
{
}

StackFrame VM::newStackFrame()
{
    return StackFrame(this->stack.data(), 0);
}

void printValue(StackObject obj)
{
    std::cout << obj.intValue << std::endl;
}

void VM::printstack()
{
    for (size_t ii = 0; ii < this->stack.size(); ++ii)
    {
        printValue(this->stack[ii]);
    }
}

void VM::execute(MethodEnvironment& environment)
{
#define ARITH_BINOPS(enumtype, type) \
	case OP::ADD_##enumtype:\
		VMI::op_binop<type, VMI::op_add<type>>(environment, instruction);\
		break; \
	case OP::SUBTRACT_##enumtype:\
		VMI::op_binop<type, VMI::op_subtract<type>>(environment, instruction); \
		break; \
	case OP::MULTIPLY_##enumtype:\
		VMI::op_binop<type, VMI::op_multiply<type>>(environment, instruction); \
		break; \
	case OP::DIVIDE_##enumtype:\
		VMI::op_binop<type, VMI::op_divide<type>>(environment, instruction); \
		break;

    size_t currentInstruction = 0;
	const std::vector<Instruction>& code = environment.function->instructions;
    while (currentInstruction < code.size())
    {
        Instruction instruction = code[currentInstruction];
		currentInstruction++;
    
        switch (instruction.op)
        {
        case OP::MOVE:
            VMI::op_move(environment, instruction);
            break;
        case OP::LOAD:
            VMI::op_load(environment, instruction);
            break;
		case OP::LOAD_FUNCTION:
			VMI::op_load_function(environment, instruction);
			break;
        case OP::LOAD_INT_CONST:
            environment.stackFrame.push(instruction.arg0);
			break;
		case OP::LOAD_DOUBLE_CONST:
			environment.stackFrame.push(instruction.arg0);
			break;
        case OP::LOAD_STRING_CONST:
            VMI::op_load_string_constant(environment, instruction);
            break;
        case OP::BRANCH_TRUE:
            VMI::op_branch_true(environment, instruction, currentInstruction);
			break;
		case OP::JUMP:
			VMI::op_jump(environment, instruction, currentInstruction);
			break;
        case OP::NEWOBJECT:
            VMI::op_newobject(environment, instruction);
            break;
        case OP::GETFIELD:
            VMI::op_getfield(environment, instruction);
            break;
        case OP::SETFIELD:
            VMI::op_setfield(environment, instruction);
            break;

			ARITH_BINOPS(INT, VMInt)

		case OP::REMAINDER_INT:\
			VMI::op_binop<VMInt, VMI::op_remainder>(environment, instruction);
			break;

			ARITH_BINOPS(DOUBLE, VMFloat)

		case OP::AND:
			VMI::op_binop<VMInt, VMI::op_and<VMInt>>(environment, instruction);
			break;
		case OP::COMPARE_EQ:
			VMI::op_binop<VMInt, VMI::op_equal<VMInt>>(environment, instruction);
			break;
		case OP::COMPARE_NEQ:
			DO_TOP_BINOP(!= , environment, instruction);
			break;
		case OP::COMPARE_LT:
			DO_TOP_BINOP(< , environment, instruction);
			break;
		case OP::COMPARE_GT:
			DO_TOP_BINOP(> , environment, instruction);
			break;
		case OP::COMPARE_LE:
			DO_TOP_BINOP(<= , environment, instruction);
			break;
		case OP::COMPARE_GE:
			DO_TOP_BINOP(>= , environment, instruction);
			break;

        case OP::CALL:
            VMI::op_call(*this, environment, instruction);
			break;
		case OP::CALLI:
			VMI::op_calli(*this, environment, instruction);
			break;
		case OP::CALLNATIVE:
			VMI::op_callnative(*this, environment, instruction);
			break;
		case OP::RETURN:
			VMI::op_return(*this, environment, instruction);
			return;
			break;
		case OP::POP:
			VMI::op_pop(*this, environment, instruction);
			break;

        default:
            std::cout << "No implementation for instruction : " << enumToString(instruction.op) << std::endl;
            break;
        }
    }
}

void VM::endFrame(MethodEnvironment& environment)
{
#if 0
    int stackLevel = 0;
    unsigned char* base = reinterpret_cast<unsigned char*>(environment.stackFrame.data());
    for (TypeEnum type : environment.method->stackLayout.types)
    {
        switch (type)
        {
        case TYPE_ARRAY:
        case TYPE_CLASS:
            VMPointer stackPos = reinterpret_cast<VMPointer>(base + stackLevel);
            break;
        }
        stackLevel += sizeofType(type);
    }
#endif
}

};


