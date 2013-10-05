#include <string>
#include <fstream>
#include <algorithm>
#include "Expression.h"
#include "Parser.h"

namespace MyVMNamespace
{

bool isOperator(char c)
{
    static std::string operators("+-*/.");
    return operators.find(c) != -1;
}

Token::Token()
    : type(SymbolEnum::NONE)
{
}

void Token::tokenize()
{
    if (name.empty())
    {
        type = SymbolEnum::NONE;
    }
    else if (name == "(")
    {
        type = SymbolEnum::LPARENS;
    }
    else if (name == ")")
    {
        type = SymbolEnum::RPARENS;
    }
    else if (std::all_of(name.begin(), name.end(), isOperator))
    {
        type = SymbolEnum::OPERATOR;
    }
    else if (std::all_of(name.begin(), name.end(), isdigit))
    {
        type = SymbolEnum::NUMBER;
    }
    else
    {
        type = SymbolEnum::NAME;
    }
}

Parser::Parser(Tokenizer& tokenizer)
    : tokenizer(tokenizer)
{
}

    
std::unique_ptr<Expression> Parser::run()
{
    if (!tokenizer.tokenize())
    {
        return nullptr;
    }
    return expression(&*tokenizer);
}

std::unique_ptr<Expression> Parser::expression(const Token* token)
{
    bool inParens = false;
    if (token->type == SymbolEnum::LPARENS)
    {
        token = &tokenizer.nextToken();
        inParens = true;
    }

    std::unique_ptr<Expression> result;
    switch (token->type)
    {
    case SymbolEnum::NAME:
        result = std::unique_ptr<Expression>(new Name(token->name));
        break;
    case SymbolEnum::NUMBER:
        result = std::unique_ptr<Expression>(new Number(atoi(token->name.c_str())));
        break;
    default:
        return nullptr;
    }

    result = expressionEx(result, &tokenizer.nextToken());

    if (inParens && tokenizer.nextToken().type != SymbolEnum::RPARENS)
    {
        --tokenizer;
        return result;
    }
    return result;
}

std::unique_ptr<Expression> Parser::expressionEx(std::unique_ptr<Expression>& term, const Token* token)
{
    std::unique_ptr<Expression> result;
    switch (token->type)
    {
    case SymbolEnum::OPERATOR:
        {
        std::vector<std::unique_ptr<Expression>> arguments;
        arguments.push_back(std::move(term));
        arguments.push_back(expression(&tokenizer.nextToken()));
        std::unique_ptr<Expression> op(new Name(token->name));
        result.reset(new FunctionApplication(std::move(op), std::move(arguments)));
        break;
        }
    default:
        result = std::move(term);
    }
    return result;
}

}