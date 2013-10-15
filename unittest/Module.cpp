#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;


TEST_CASE("module/function", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	std::vector<Binding> bindings = parser.toplevel();

	REQUIRE(bindings.size() == 1);
	REQUIRE(bindings[0].name == "test");
	REQUIRE(typeid(*bindings[0].expression) == typeid(Lambda));
}


TEST_CASE("module/function2", "")
{
	const char* file =
"test x = 2 * x\n\
\n\
f x y = x + y\n";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::vector<Binding> bindings = parser.toplevel();

	REQUIRE(bindings.size() == 2);
	REQUIRE(bindings[0].name == "test");
	REQUIRE(typeid(*bindings[0].expression) == typeid(Lambda));
	REQUIRE(bindings[1].name == "f");
	REQUIRE(typeid(*bindings[0].expression) == typeid(Lambda));
}


TEST_CASE("module/function/let", "")
{
	const char* file =
"test x = let id y = y in id 2 * x\n\
\n\
f x y = x + y\n";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::vector<Binding> bindings = parser.toplevel();

	REQUIRE(bindings.size() == 2);
	const Binding& test = bindings[0];
	REQUIRE(test.name == "test");
	const Lambda& testLambda = dynamic_cast<Lambda&>(*test.expression);
	REQUIRE(testLambda.arguments.size() == 1);
	const Let& letExpr = dynamic_cast<Let&>(*testLambda.expression);
	REQUIRE(letExpr.bindings[0].name == "id");

	REQUIRE(bindings[1].name == "f");
	REQUIRE(typeid(*bindings[0].expression) == typeid(Lambda));
}