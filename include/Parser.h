#pragma once
#include "Expression.h"
#include "Tokenizer.h"
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
	Alternative alternative();
	Binding binding();

	template<class TResult>
	std::vector<TResult> many1(TResult (Parser::*parse)() , SymbolEnum delim);

	std::unique_ptr<Expression> parseOperatorExpression(std::unique_ptr<Expression> lhs, int minPrecedence);
private:
    Tokenizer& tokenizer;
};

template<class TResult>
inline std::vector<TResult> Parser::many1(TResult (Parser::*parse)(), SymbolEnum delim)
{
	const Token* tokenDelim;
	std::vector<TResult> result;
	do
	{
		TResult x = (this->*parse)();
		result.push_back(std::move(x));
		tokenDelim = &tokenizer.nextToken();
	} while (tokenDelim->type == delim);
	return std::move(result);
}

}