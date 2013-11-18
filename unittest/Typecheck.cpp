#include "Catch/include/catch.hpp"
#include "Tokenizer.h"
#include "Parser.h"

using namespace MyVMNamespace;

TEST_CASE("typecheck/function1", "")
{
    std::stringstream stream("test x = 2 * x\n");
    Tokenizer tokenizer(stream);
    Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), TypeOperator("Int"));
	REQUIRE(type == wanted);
}

TEST_CASE("typecheck/function2", "")
{
	std::stringstream stream("test x y = 2 * x * y\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), functionType(TypeOperator("Int"), TypeOperator("Int")));
	REQUIRE(type == wanted);
}
TEST_CASE("typecheck/function3", "")
{
	std::stringstream stream(
"add x y = x + y\n\
test x = 2 * add 3 x\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	Type& type = module.bindings[0].expression->typecheck(env);

	auto wanted = functionType(TypeOperator("Int"), functionType(TypeOperator("Int"), TypeOperator("Int")));
	REQUIRE(type == wanted);
}

TEST_CASE("typecheck/tuple", "")
{
	std::stringstream stream("(1, 2)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	std::vector<Type> args(2);
	args[0] = TypeOperator("Int");
	args[1] = TypeOperator("Int");
	auto wanted = Type(TypeOperator("(,)", args));

	REQUIRE(type == wanted);
}


TEST_CASE("typecheck/typevariable", "")
{
	std::stringstream stream("undefined");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	
	REQUIRE_NOTHROW(boost::get<TypeVariable>(type));
}

TEST_CASE("typecheck/case", "")
{
	std::stringstream stream(
"let\n\
    x = undefined\n\
in case (1, x) of\n\
    (a, b) -> a\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}

TEST_CASE("typecheck/case2", "")
{
	std::stringstream stream(
		"let\n\
		    x = undefined\n\
			in case (1, x) of\n\
			    (a, b) -> b + x\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	std::unique_ptr<Expression> expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}

TEST_CASE("typecheck/module", "")
{
	std::stringstream stream(
"main = add2 3 * add2 3\n\
add2 x = x + 2\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	for (auto& bind : module.bindings)
	{
		bind.expression->typecheck(env);
	}

	REQUIRE(module.bindings[0].expression->getType() == Type(TypeOperator("Int")));
	REQUIRE(module.bindings[1].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
}

TEST_CASE("typecheck/module/recursive", "")
{
	std::stringstream stream("fib n = n * fib (n-1)");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	TypeEnvironment env(&module);
	for (auto& bind : module.bindings)
	{
		bind.expression->typecheck(env);
	}

	REQUIRE(module.bindings[0].expression->getType() == functionType(TypeOperator("Int"), TypeOperator("Int")));
}

TEST_CASE("typecheck/letrec", "")
{
	std::stringstream stream(
"let\n\
    f x = x + g x\n\
    g y = y * f y\n\
in f 2\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	auto expr = parser.expression();
	TypeEnvironment env(nullptr);
	Type& type = expr->typecheck(env);

	REQUIRE(type == Type(TypeOperator("Int")));
}


//Check if the types are the same, ignoring which type variable they are
bool sameTypes(std::map<int, int>& idmap, const Type& lhs, const Type& rhs)
{
	if (lhs.which() == 0 && rhs.which() == 0)
	{
		auto& l = boost::get<TypeVariable>(lhs);
		auto& r = boost::get<TypeVariable>(rhs);
		if (idmap.count(l.id) > 0)
			return idmap[l.id] == r.id;
		idmap[l.id] = r.id;
		return true;
	}
	else if (lhs.which() == rhs.which())
	{
		auto& l = boost::get<TypeOperator>(lhs);
		auto& r = boost::get<TypeOperator>(rhs);
		if (l.name != r.name || l.types.size() != r.types.size())
			return false;

		for (size_t ii = 0; ii < l.types.size(); ii++)
		{
			if (!sameTypes(idmap, l.types[ii], r.types[ii]))
				return false;
		}
		return true;
	}
	return false;
}
bool sameTypes(const Type& lhs, const Type& rhs)
{
	std::map<int, int> idmap;
	return sameTypes(idmap, lhs, rhs);
}

TEST_CASE("typecheck/module/mutual_recursion", "")
{
	std::stringstream stream(
"test1 x = 3 * test2 x\n\
test2 y = 2 * test1 y\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	Type x = functionType(TypeVariable(), TypeOperator("Int"));
	REQUIRE(sameTypes(module.bindings[0].expression->getType(), x));
	REQUIRE(sameTypes(module.bindings[1].expression->getType(), x));
}

TEST_CASE("typecheck/nested_let", "")
{
	std::stringstream stream(
"test x =\n\
    let\n\
        first = x + x\n\
    in let\n\
        second = [first, x]\n\
       in second\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> args(1);
	args[0] = TypeOperator("Int");
	Type listType = TypeOperator("[]", args);
	Type x = functionType(TypeOperator("Int"), listType);
	REQUIRE(sameTypes(module.bindings[0].expression->getType(), x));

	Lambda& lambda = dynamic_cast<Lambda&>(*module.bindings[0].expression);
	Let& let = dynamic_cast<Let&>(*lambda.body);
	REQUIRE(let.bindings[0].expression->getType() == Type(TypeOperator("Int")));
	Let& innerLet = dynamic_cast<Let&>(*let.expression);
	REQUIRE(innerLet.bindings[0].expression->getType() == listType);
}

TEST_CASE("typecheck/list", "")
{
	std::stringstream stream(
"head :: [a] -> a\n\
head xs = case xs of\n\
    : y ys -> y\n\
tail xs = case xs of\n\
    : y ys -> ys\n\
main = head (tail [10, 20, 30])\n");
	Tokenizer tokenizer(stream);
	Parser parser(tokenizer);

	Module module = parser.module();
	module.typecheck();

	std::vector<Type> args(1);
	args[0] = TypeVariable();
	Type listType = TypeOperator("[]", args);
	Type headType = functionType(listType, args[0]);
	Type tailType = functionType(listType, listType);
	
	CHECK(sameTypes(module.bindings[0].expression->getType(), headType));
	CHECK(sameTypes(module.bindings[1].expression->getType(), tailType));
	CHECK(sameTypes(module.bindings[2].expression->getType(), Type(TypeOperator("Int"))));
}