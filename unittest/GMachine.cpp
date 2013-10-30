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

TEST_CASE("gmachine/const", "const")
{
	GMachine machine;
	const char* str =
"const x y = x\n\
main = const 10 5\n";
	std::stringstream expr(str);
	machine.compile(expr);

	GEnvironment env(machine.baseStack(), machine.getCombinator("main"));
	machine.execute(env);
	REQUIRE(env.stack.base().getType() == NUMBER);
	REQUIRE(env.stack.base().getNode()->number == 10);

}