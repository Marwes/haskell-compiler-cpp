#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;

TEST_CASE("typecheck/basic", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);
	
	std::cerr << type << "\n";

	auto wanted = functionType(TypeOperator("Int"), TypeOperator("Int"));
	REQUIRE(type == wanted);
}