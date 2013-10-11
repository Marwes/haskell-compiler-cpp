#pragma once
#include "Expression.h"
#include <vector>
#include <fstream>
#include <memory>

namespace MyVMNamespace
{
class Token;
class Tokenizer;


struct Binding
{
	std::string name;
	std::unique_ptr<Expression> expression;
};


class Parser
{
public:
    Parser(Tokenizer& tokenizer);

    
	std::unique_ptr<Expression> run();

    std::unique_ptr<Expression> expression(const Token& token);
	std::unique_ptr<Expression> subExpression(const Token& token);
	std::unique_ptr<Expression> application(const Token& token);
	std::pair<std::string, std::unique_ptr<Expression>> binding(const Token& token);

	std::unique_ptr<Expression> parseOperatorExpression(std::unique_ptr<Expression> lhs, int minPrecedence);
private:
    Tokenizer& tokenizer;
};

}