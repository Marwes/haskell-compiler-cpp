#include "Catch/include/catch.hpp"
#include "GMachine.h"

using namespace MyVMNamespace;

TEST_CASE("gmachine", "")
{
	GMachine machine;
	std::stringstream expr("main = 4");
	machine.compile(expr);

	GEnvironment env(machine.baseStack(), machine.getCombinator("main"));
	machine.execute(env);
	REQUIRE(env.stack.top().getType() == NUMBER);
	REQUIRE(env.stack.top().getNode()->number == 4);

}