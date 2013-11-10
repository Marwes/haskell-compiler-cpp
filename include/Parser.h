#pragma once
#include <vector>
#include <fstream>
#include <memory>
#include "Expression.h"
#include "Tokenizer.h"
#include "Module.h"
#include "SuperCombinator.h"

namespace MyVMNamespace
{
class Token;
class Tokenizer;


class Parser
{
	//Recursive descent parser for the parsing haskell files
public:
    Parser(Tokenizer& tokenizer);

    
	std::unique_ptr<Expression> run();

	//Parse the tokens from the tokenizer as a haskell module (file)
	Module module();
    std::unique_ptr<Expression> expression();
	std::unique_ptr<Expression> subExpression(bool (*parseError)(const Token&) = nullptr);
	std::unique_ptr<Expression> application();
	Alternative alternative();
	Binding binding();
	std::unique_ptr<Pattern> pattern();
	TypeDeclaration typeDeclaration();
	Constructor constructor();
	DataDefinition dataDefinition();
	std::unique_ptr<Type> type();

	template<class TResult>
	std::vector<TResult> many1(TResult(Parser::*parse)(), SymbolEnum delim);
	template<class TResult, class F>
	std::vector<TResult> many1(TResult(Parser::*parse)(), F delim);

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
template<class TResult, class F>
inline std::vector<TResult> Parser::many1(TResult(Parser::*parse)(), F delim)
{
	const Token* tokenDelim;
	std::vector<TResult> result;
	do
	{
		TResult x = (this->*parse)();
		result.push_back(std::move(x));
		tokenDelim = &tokenizer.nextToken();
	} while (delim(*tokenDelim));
	return std::move(result);
}

}