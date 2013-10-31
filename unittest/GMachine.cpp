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

TEST_CASE("gmachine/let/2", "")
{
	GMachine machine;
	const char* str =
"const x y = x\n\
main = let three = 3 in const three 2\n";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 3);
}

TEST_CASE("gmachine/let", "")
{
	GMachine machine;
	const char* str =
"main = \n\
    let\n\
        three = 3\n\
        alsoThree = three\n\
    in three\n";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 3);
}

TEST_CASE("gmachine/arithmetic", "")
{
	GMachine machine;
	const char* str =
		"main = 3 * 7";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 21);
}

TEST_CASE("gmachine/arithmetic/2", "")
{
	GMachine machine;
	const char* str =
		"main = let three = 3 in three * 7 - 5";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 16);
}