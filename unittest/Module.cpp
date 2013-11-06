#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;


TEST_CASE("module/function", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	Module module = parser.toplevel();
	std::vector<Binding>& bindings = module.bindings;

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

	Module module = parser.toplevel();
	std::vector<Binding>& bindings = module.bindings;

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

	Module module = parser.toplevel();
	std::vector<Binding>& bindings = module.bindings;

	REQUIRE(bindings.size() == 2);
	const Binding& test = bindings[0];
	REQUIRE(test.name == "test");
	const Lambda& testLambda = dynamic_cast<Lambda&>(*test.expression);
	REQUIRE(testLambda.arguments.size() == 1);
	const Let& letExpr = dynamic_cast<Let&>(*testLambda.body);
	REQUIRE(letExpr.bindings[0].name == "id");

	REQUIRE(bindings[1].name == "f");
	REQUIRE(typeid(*bindings[0].expression) == typeid(Lambda));
}


TEST_CASE("module/datadefinition/simple", "")
{
	const char* file =
		"data Test = Test";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();

	REQUIRE(module.dataDefinitions.size() == 1);
	DataDefinition& def = module.dataDefinitions[0];
	REQUIRE(def.name == "Test");
	REQUIRE(def.constructors[0].name == "Test");
	REQUIRE(def.constructors[0].arity == 0);
}

TEST_CASE("module/datadefinition/multiple", "")
{
	const char* file =
		"data Bool = True | False";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();

	REQUIRE(module.dataDefinitions.size() == 1);
	DataDefinition& def = module.dataDefinitions[0];
	REQUIRE(def.name == "Bool");
	REQUIRE(def.constructors[0].name == "True");
	REQUIRE(def.constructors[0].arity == 0);
	REQUIRE(def.constructors[1].name == "False");
	REQUIRE(def.constructors[1].arity == 0);
}

TEST_CASE("module/datadefinition/arguments", "")
{
	const char* file =
"data List = Cons Int List\n\
	       | Nil";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.toplevel();

	REQUIRE(module.dataDefinitions.size() == 1);
	DataDefinition& def = module.dataDefinitions[0];
	REQUIRE(def.name == "List");
	REQUIRE(def.constructors[0].name == "Cons");
	REQUIRE(def.constructors[0].arity == 2);
	REQUIRE(def.constructors[1].name == "Nil");
	REQUIRE(def.constructors[1].arity == 0);
}