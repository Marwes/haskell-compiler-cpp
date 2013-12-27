#include "Catch/include/catch.hpp"
#include <string>
#include <fstream>
#include <vector>
#include "Util.h"
#include "Array.h"
#include "GMachine.h"

using namespace MyVMNamespace;

int evaluateInt(const char* str)
{
	GMachine machine;
	std::string main("main = ");
	main += str;
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	return result.getNode()->number;
}

TEST_CASE("compiler/arithmetic", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("primIntAdd 3 2") == 5);
	REQUIRE(evaluateInt("primIntAdd (primIntMultiply 2 4) 3") == 11);
	REQUIRE(evaluateInt("let three = 3 in primIntAdd (primIntMultiply 2 4) three") == 11);
	REQUIRE(evaluateInt("let six = primIntMultiply 3 2; four = primIntSubtract six 2 in primIntAdd (primIntMultiply 2 four) six") == 14);

	//Lambda lifting is not implemented so just avoid local functiosnm for now (even though they happen to work)
	//REQUIRE(evaluateInt("let f x = x * x in f 3") == 9);
	//REQUIRE(evaluateInt("let f x y = x * x + y; five = 5 in f 3 five") == 14);
	//REQUIRE(evaluateInt("let f x = x * x in f 3 + f 2") == 13);
}

TEST_CASE("compiler/compare", "Test compiling an arithmetic expression")
{
	REQUIRE(evaluateInt("3==2") == 0);
	REQUIRE(evaluateInt("3<=2") == 0);
	REQUIRE(evaluateInt("3 > 2") == 1);
	REQUIRE(evaluateInt("3>=2") == 1);
	REQUIRE(evaluateInt("3 < 2") == 0);
	REQUIRE(evaluateInt("let one = 1 in one /= 2") == 1);
}

TEST_CASE("compiler/arithmetic/double", "Test compiling an arithmetic expression")
{
	GMachine machine;
	std::stringstream expr(
"main = primDoubleMultiply 3.0 1.5\n");
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == DOUBLE);//TODO
	REQUIRE(result.getNode()->numberDouble == 3.0 * 1.5);
}


TEST_CASE("compiler/data", "")
{
	GMachine machine;
	const char* main =
"data Vec2 = Vec2 Int Int\n\
main = Vec2 1 2\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == CONSTRUCTOR);
	ConstructorNode ctor = result.getNode()->constructor;
	REQUIRE(ctor.arguments[0].getType() == NUMBER);
	REQUIRE(ctor.arguments[0].getNode()->number == 1);
	REQUIRE(ctor.arguments[1].getType() == NUMBER);
	REQUIRE(ctor.arguments[1].getNode()->number == 2);
}

TEST_CASE("compiler/data/different names", "")
{
	GMachine machine;
	const char* main =
"data Tuple = Triple Int Int Int\n\
main = Triple 1 2 3\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == CONSTRUCTOR);
	ConstructorNode ctor = result.getNode()->constructor;
	REQUIRE(ctor.arguments[0].getType() == NUMBER);
	REQUIRE(ctor.arguments[0].getNode()->number == 1);
	REQUIRE(ctor.arguments[1].getType() == NUMBER);
	REQUIRE(ctor.arguments[1].getNode()->number == 2);
	REQUIRE(ctor.arguments[2].getType() == NUMBER);
	REQUIRE(ctor.arguments[2].getNode()->number == 3);
}
TEST_CASE("compiler/data/list", "")
{
	GMachine machine;
	const char* main =
"data List = Cons Int List\n\
           | Nil\n\
main = Cons 1 Nil\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == CONSTRUCTOR);
	ConstructorNode ctor = result.getNode()->constructor;
	REQUIRE(ctor.arguments[0].getType() == NUMBER);
	REQUIRE(ctor.arguments[0].getNode()->number == 1);
	REQUIRE(ctor.arguments[1].getType() == CONSTRUCTOR);
	ConstructorNode nil = ctor.arguments[1].getNode()->constructor;
	REQUIRE(nil.tag == 1);
}

TEST_CASE("compiler/data/patternmatch", "")
{
	GMachine machine;
	const char* main =
"data List = Cons Int List\n\
           | Nil\n\
length xs = case xs of\n\
    Cons n ys -> primIntAdd 1 (length ys)\n\
    Nil -> 0\n\
main = length (Cons 10 (Cons 20 Nil))\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 2);
}

TEST_CASE("compiler/data/map", "")
{
	GMachine machine;
	const char* main =
"data List = Cons Int List\n\
           | Nil\n\
length xs = case xs of\n\
    Cons n ys -> primIntAdd 1 (length ys)\n\
    Nil -> 0\n\
map f xs = case xs of\n\
    Cons n ys -> Cons (f n) (map f ys)\n\
    Nil -> Nil\n\
add2 x = primIntAdd x 2\n\
sum xs = case xs of\n\
    Cons n ys -> primIntAdd n (sum ys)\n\
    Nil -> 0\n\
main = sum (map add2 (Cons 10 (Cons 20 Nil)))\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 34);
}


TEST_CASE("compiler/list", "")
{
	GMachine machine;
	const char* main =
"head xs = case xs of\n\
    : y ys -> y\n\
main = head [10,2,3]\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 10);
}
TEST_CASE("compiler/list/head_tail", "")
{
GMachine machine;
const char* main =
"head xs = case xs of\n\
    : y ys -> y\n\
tail xs = case xs of\n\
    : y ys -> ys\n\
main = head (tail [10,2222,3])\n";
	std::stringstream expr(main);
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 2222);
}

TEST_CASE("compiler/bool", "")
{
	std::stringstream expr(
"data Bool = True | False\n\
if bool x y = case bool of\n\
    True -> x\n\
    False -> y\n\
main = if True 6 9\n");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 6);
}

TEST_CASE("compiler/opererator", "")
{
	std::stringstream expr(
"(===) x y = x == y\n\
main = 7 === 3\n");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 0);
}

TEST_CASE("compiler/partial_application", "")
{
	std::stringstream expr(
"(.) f g x = f (g x)\n\
id x = x\n\
main = (id . id) 3\n");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 3);
}


TEST_CASE("compiler/typeclass", "")
{
	std::stringstream expr(
"class Num a where\n\
    ($+) :: a -> a -> a\n\
instance Num Int where\n\
    ($+) x y = primIntAdd x y\n\
main = 2 $+ 3");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 5);
}

TEST_CASE("compiler/typeclass/2", "")
{
	std::stringstream expr(
"class Num a where\n\
    ($+) :: a -> a -> a\n\
    ($-) :: a -> a -> a\n\
instance Num Int where\n\
    ($+) x y = primIntAdd x y\n\
    ($-) x y = primIntSubtract x y\n\
main = 2 $+ 3 $- 8");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == -3);
}


TEST_CASE("compiler/typeclass/unordered", "")
{
	std::stringstream expr(
"class Num a where\n\
	($+) :: a -> a -> a\n\
	($-) :: a -> a -> a\n\
instance Num Int where\n\
	($-) x y = primIntSubtract x y\n\
	($+) x y = primIntAdd x y\n\
main = 2 $+ 3 $- 8");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == -3);
}

TEST_CASE("compiler/typeclass/sum", "")
{
	std::stringstream expr(
"class Num a where\n\
    ($+) :: a -> a -> a\n\
instance Num Int where\n\
    ($+) x y = primIntAdd x y\n\
sum1 :: Num a => [a] -> a\n\
sum1 xs = case xs of\n\
    : y ys -> case ys of\n\
        : z zs -> y $+ sum1 (z : zs)\n\
        [] -> y\n\
main = sum1 [1, 2, 3]");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == 6);
}

TEST_CASE("compiler/typeclass/multiple", "")
{
	std::stringstream expr(
"class Number a where\n\
	number :: a -> Int\n\
instance Number Int where\n\
	number x = 1\n\
data Test = Test\n\
instance Number Test where\n\
	number x = 2\n\
main = primIntSubtract (number (primIntAdd 0 0)) (number Test)");
	GMachine machine;
	machine.compile(expr);

	Address result = machine.executeMain();
	REQUIRE(result.getType() == NUMBER);
	REQUIRE(result.getNode()->number == -1);
}