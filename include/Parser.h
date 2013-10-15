#pragma once
#include "Expression.h"
#include <vector>
#include <fstream>
#include <memory>

namespace MyVMNamespace
{
class Token;
class Tokenizer;


class Binding
{
public:
	Binding(std::string name, std::unique_ptr<Expression> expression);
	Binding(Binding&& binding);

	std::string name;
	std::unique_ptr<Expression> expression;
};


class Parser
{
public:
    Parser(Tokenizer& tokenizer);

    
	std::unique_ptr<Expression> run();

	std::vector<Binding> toplevel();
    std::unique_ptr<Expression> expression();
	std::unique_ptr<Expression> subExpression(bool (*parseError)(const Token&) = nullptr);
	std::unique_ptr<Expression> application();
	Binding binding();

	std::unique_ptr<Expression> parseOperatorExpression(std::unique_ptr<Expression> lhs, int minPrecedence);
private:
    Tokenizer& tokenizer;
};

}