#define CATCH_CONFIG_RUNNER
#include <assert.h>
#include "Catch/include/catch.hpp"
#include <VM.h>

int main(int argc, char* const argv [])
{
	Catch::ConfigData data;
	data.shouldDebugBreak = true;
	Catch::Config config(data);
	int ret = Catch::Main(argc, argv, config);
	return ret;
}

using namespace MyVMNamespace;

TEST_CASE("test", "test addition")
{
    std::vector<Instruction> instructions;
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 0)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 10)); 
    instructions.push_back(Instruction(OP::LOAD_INT_CONST, 5)); 
    instructions.push_back(Instruction(OP::ADD, 0, 1, 2));
    VM vm(instructions);
    RuntimeEnvironment env(vm.newStackFrame());
    vm.execute(env);
    REQUIRE(vm.getValue(0).intValue == 15);
    REQUIRE(vm.getValue(1).intValue == 10);
    REQUIRE(vm.getValue(2).intValue == 5);
}