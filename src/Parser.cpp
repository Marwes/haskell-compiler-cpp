#include <string>
#include <fstream>
#include <algorithm>
#include <array>
#include "Expression.h"
#include "Parser.h"

namespace MyVMNamespace
{

std::istream& operator>>(std::istream& input, Token& token)
{
    char c = '\0';
    while (input.get(c) && isspace(c))
        ;
    token.name.clear();
    token.name.push_back(c);
    
    //Decide how to tokenize depending on what the first char is
    //ie if its an operator then more operators will follow
    if (isOperator(c))
    {
        while (input.get(c) && isOperator(c))
        {
            token.name.push_back(c);
        }
        token.type = SymbolEnum::OPERATOR;
    }
    else if (isdigit(c))
    {
        while (input.get(c) && isdigit(c))
        {
            token.name.push_back(c);
        }
        token.type = SymbolEnum::NUMBER;
    }
    else if (isalpha(c))
    {
        while (input.get(c) && isalnum(c))
        {
            token.name.push_back(c);
        }
        token.type = SymbolEnum::NAME;
    }
    else if (c == '(')
    {
        token.type = SymbolEnum::LPARENS;
        return input;
    }
    else if (c == ')')
    {
        token.type = SymbolEnum::RPARENS;
        return input;
    }

    if (!isspace(c))
    {
        input.unget();
        input.bad();
        return input;
    }

    token.tokenize();
    return input;
}

Token::Token()
    : type(SymbolEnum::NONE)
{
}

Token::Token(SymbolEnum type, const std::string& name)
    : type(type)
    , name(name)
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



bool isPlusMinusOP(const Token& token)
{
    return token.type == SymbolEnum::OPERATOR && (token.name == "+" || token.name == "-");
}

bool isMultDivOp(const Token& token)
{
    return token.type == SymbolEnum::OPERATOR && (token.name == "*" || token.name == "/");
}

std::unique_ptr<Expression> Parser::expression(const Token* token)
{
    auto lhs = term(token);
    if (!lhs)
        return nullptr;

    while (true)
    {
        const Token& op = tokenizer.nextToken();
        if (isPlusMinusOP(op))
        {
            auto rhs = term(&tokenizer.nextToken());
            if (rhs)
            {
                lhs = std::unique_ptr<Expression>(new PrimOP(op.name[0], std::move(lhs), std::move(rhs)));
            }
            else
                return nullptr;
        }
        else
        {
            --tokenizer;
            break;
        }
    }
    return lhs;
}

std::unique_ptr<Expression> Parser::factor(const Token* token)
{
    if (token->type == SymbolEnum::LPARENS)
    {
        std::unique_ptr<Expression> result = expression(&tokenizer.nextToken());
    

        const Token& maybeParens = tokenizer.nextToken();
        if (maybeParens.type == SymbolEnum::RPARENS)
        {
            return result;
        }
        else
        {
            --tokenizer;
            return nullptr;
        }
    }
    else
    {
        switch (token->type)
        {
        case SymbolEnum::NAME:
            return std::unique_ptr<Expression>(new Name(token->name));
        case SymbolEnum::NUMBER:
            return std::unique_ptr<Expression>(new Number(atoi(token->name.c_str())));
        default:
            return nullptr;
        }
    }
}

std::unique_ptr<Expression> Parser::term(const Token* token)
{
    auto lhs = factor(token);
    if (!lhs)
        return nullptr;

    while (true)
    {
        const Token& op = tokenizer.nextToken();
        if (isMultDivOp(op))
        {
            auto rhs = factor(&tokenizer.nextToken());
            if (rhs)
            {
                lhs = std::unique_ptr<Expression>(new PrimOP(op.name[0], std::move(lhs), std::move(rhs)));
            }
            else
                return nullptr;
        }
        else
        {
            --tokenizer;
            break;
        }
    }
    return lhs;
}

bool isPrimOP(const std::string& op)
{
    static std::array<std::string, 5> operators = {
        "+",
        "-",
        "*",
        "/",
        "%"
    };
    return std::find(operators.begin(), operators.end(), op) != operators.end();
}

std::unique_ptr<Expression> Parser::expressionEx(std::unique_ptr<Expression>& term, const Token* token)
{
    std::unique_ptr<Expression> result;
    switch (token->type)
    {
    case SymbolEnum::OPERATOR:
        {
            if (isPrimOP(token->name))
            {
                result.reset(new PrimOP(token->name[0], std::move(term), expression(&tokenizer.nextToken())));
            }
            else
            {
                std::vector<std::unique_ptr<Expression>> arguments;
                arguments.push_back(std::move(term));
                arguments.push_back(expression(&tokenizer.nextToken()));
                std::unique_ptr<Expression> op(new Name(token->name));
                result.reset(new FunctionApplication(std::move(op), std::move(arguments)));
            }
        break;
        }
    default:
        result = std::move(term);
    }
    return result;
}

}