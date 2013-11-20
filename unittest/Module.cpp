#include "Catch/include/catch.hpp"
#include "TestUtil.h"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;


TEST_CASE("module/function", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	Module module = parser.module();
	std::vector<Binding>& bindings = module.bindings;

	REQUIRE(bindings.size() != 0);
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

	Module module = parser.module();
	std::vector<Binding>& bindings = module.bindings;

	REQUIRE(bindings.size() >= 2);
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

	Module module = parser.module();
	std::vector<Binding>& bindings = module.bindings;

	REQUIRE(bindings.size() >= 2);
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

	Module module = parser.module();

	REQUIRE(module.dataDefinitions.size() > 0);
	DataDefinition& def = module.dataDefinitions.back();
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

	Module module = parser.module();

	REQUIRE(module.dataDefinitions.size() > 0);
	DataDefinition& def = module.dataDefinitions.back();
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

	Module module = parser.module();

	REQUIRE(module.dataDefinitions.size() > 0);
	DataDefinition& def = module.dataDefinitions.back();
	REQUIRE(def.name == "List");
	REQUIRE(def.constructors[0].name == "Cons");
	REQUIRE(def.constructors[0].arity == 2);
	REQUIRE(def.constructors[1].name == "Nil");
	REQUIRE(def.constructors[1].arity == 0);
}


TEST_CASE("module/datadefinition/typeargumentss", "")
{
	const char* file =
"data Maybe a = Just a\n\
              | Nothing";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE(module.dataDefinitions.size() > 0);
	DataDefinition& def = module.dataDefinitions.back();
	REQUIRE(def.name == "Maybe");
	REQUIRE(sameTypes(def.type, Type(TypeOperator("Maybe", { TypeVariable()}))));
	TypeOperator& maybe = boost::get<TypeOperator>(def.type);
	REQUIRE(def.constructors[0].name == "Just");
	REQUIRE(def.constructors[0].arity == 1);
	REQUIRE(def.constructors[0].type == functionType(maybe.types[0], TypeOperator("Maybe", maybe.types)));//TODO
	REQUIRE(def.constructors[1].name == "Nothing");
	REQUIRE(def.constructors[1].arity == 0);
}


TEST_CASE("module/operator", "")
{
	const char* file =
"(.) f g x = f (g x)";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE(module.bindings.size() > 0);

	REQUIRE(module.bindings.back().name == ".");
	Lambda& lambda = dynamic_cast<Lambda&>(*module.bindings.back().expression);
	REQUIRE(lambda.arguments.size() == 3);
}

TEST_CASE("module/class", "")
{
	const char* file =
"class Eq a where\n\
    (==) :: a -> a -> Bool\n";
	std::stringstream stream(file);
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();

	REQUIRE(module.classes.size() > 0);
	Class& eqClass = module.classes.back();
	REQUIRE(eqClass.declarations.size() == 1);
	REQUIRE(eqClass.declarations["=="].name == "==");
	Type equalType = functionType(eqClass.variable, functionType(eqClass.variable, TypeOperator("Bool")));
	REQUIRE(eqClass.declarations["=="].type == equalType);
}