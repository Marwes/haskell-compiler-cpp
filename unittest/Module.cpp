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