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
		"main = primIntMultiply 3 7";
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
		"main = let three = 3 in primIntSubtract (primIntMultiply three 7) 5";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 16);
}

TEST_CASE("gmachine/tuple", "")
{
	GMachine machine;
	const char* str =
		"main = case (1, 2) of { (x, y) -> primIntAdd x y }";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 3);
}
TEST_CASE("gmachine/tuple2", "")
{
	GMachine machine;
	const char* str =
"main =\n\
	let\n\
		ten = 10\n\
	in case (ten, 5) of { (x, y) -> primIntAdd x y }";
	std::stringstream expr(str);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 15);
}

TEST_CASE("gmachine/multiple_assemblies", "")
{
	GMachine machine;
	std::stringstream main("main = primIntMultiply 3 3");
	machine.compile(main);
	std::stringstream first("f x = primIntAdd x 2");
	machine.compile(first);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 9);
}