#pragma once
#include <vector>
#include <fstream>
#include <memory>
#include "Expression.h"
#include "Tokenizer.h"
#include "Module.h"

namespace MyVMNamespace
{
class Token;
class Tokenizer;

class Parser
{
public:
    Parser(Tokenizer& tokenizer);

    
	std::unique_ptr<Expression> run();

	Module toplevel();
    std::unique_ptr<Expression> expression();
	std::unique_ptr<Expression> subExpression(bool (*parseError)(const Token&) = nullptr);
	std::unique_ptr<Expression> application();
	Alternative alternative();
	Binding binding();
	TypeDeclaration typeDeclaration();
	std::unique_ptr<Type> type();

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