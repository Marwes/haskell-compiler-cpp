#include "Catch/include/catch.hpp"
#include "GMachine.h"

using namespace MyVMNamespace;

TEST_CASE("gmachine", "")
{
	GMachine machine;
	std::stringstream expr("main = 4");
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 4);
}

TEST_CASE("gmachine/const", "const")
{
	GMachine machine;
	const char* str =
"const x y = x\n\
main = const 10 5\n";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 10);

}