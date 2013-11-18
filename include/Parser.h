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
	//Top level expression parsing function
    std::unique_ptr<Expression> expression();
	//Parse a expression which can be passed as an argument to a function application
	std::unique_ptr<Expression> subExpression(bool (*parseError)(const Token&) = nullptr);
	//Parse a function application
	std::unique_ptr<Expression> application();
	//Parse an alternative in 'case' expression (pattern -> expr;)
	Alternative alternative();
	//Parse a variable binding in 'let' expressions
	Binding binding();
	//Parse pattern matching in alternative
	std::unique_ptr<Pattern> pattern();

	std::vector<std::unique_ptr<Pattern>> patternParameter();
	
	TypeDeclaration typeDeclaration();
	Type type();

	//Parse a data definition, data NAME = sepBy1 constructor
	DataDefinition dataDefinition();
	Constructor constructor(const DataDefinition& dataType);

	//Parse 1 to N occurances of the argument parse, each seperated by 'delim'
	template<class TResult>
	std::vector<TResult> sepBy1(TResult(Parser::*parse)(), SymbolEnum delim);
	//Parse 1 to N occurances of the argument parse, each seperated by tokens giving a true result from the delim function
	template<class TResult, class F>
	std::vector<TResult> sepBy1(TResult(Parser::*parse)(), F delim);
	template<class TResult, class T, class F>
	std::vector<TResult> sepBy1(TResult(Parser::*parse)(const T& t), const T& t, F delim);

	//Parse a binary operator expression, taking into account the precedence of the operators
	std::unique_ptr<Expression> parseOperatorExpression(std::unique_ptr<Expression> lhs, int minPrecedence);
private:
    Tokenizer& tokenizer;
};

template<class TResult>
inline std::vector<TResult> Parser::sepBy1(TResult (Parser::*parse)(), SymbolEnum delim)
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
inline std::vector<TResult> Parser::sepBy1(TResult(Parser::*parse)(), F delim)
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
template<class TResult, class T, class F>
inline std::vector<TResult> Parser::sepBy1(TResult(Parser::*parse)(const T& t), const T& t, F delim)
{
	const Token* tokenDelim;
	std::vector<TResult> result;
	do
	{
		TResult x = (this->*parse)(t);
		result.push_back(std::move(x));
		tokenDelim = &tokenizer.nextToken();
	} while (delim(*tokenDelim));
	return std::move(result);
}

}