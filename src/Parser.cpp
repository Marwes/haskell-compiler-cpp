#include <string>
#include <fstream>
#include <algorithm>
#include <array>
#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"

namespace MyVMNamespace
{

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
    return expression(*tokenizer);
}



bool isPlusMinusOP(const Token& token)
{
    return token.type == SymbolEnum::OPERATOR && (token.name == "+" || token.name == "-");
}

bool isMultDivOp(const Token& token)
{
    return token.type == SymbolEnum::OPERATOR && (token.name == "*" || token.name == "/" || token.name == "%");
}

std::unique_ptr<Expression> Parser::expression(const Token& token)
{
    auto lhs = term(token);
    if (!lhs)
        return nullptr;

    while (true)
    {
        const Token& op = tokenizer.nextToken();
        if (isPlusMinusOP(op))
        {
            auto rhs = term(tokenizer.nextToken());
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

std::unique_ptr<Expression> Parser::factor(const Token& token)
{
	switch (token.type)
	{
	case SymbolEnum::LPARENS:
		{
			std::unique_ptr<Expression> result = expression(tokenizer.nextToken());
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
			break;
		}
	case SymbolEnum::LET:
		{
			auto result = bindings(tokenizer.nextToken());
			if (tokenizer.nextToken().type != SymbolEnum::IN)
				throw std::runtime_error("Expected 'in' token to end a let exprssion");
			Let::Bindings binds;
			binds.push_back(std::move(result));
			return std::unique_ptr<Expression>(new Let(std::move(binds), expression(tokenizer.nextToken())));
		}
		break;
    case SymbolEnum::NAME:
        return std::unique_ptr<Expression>(new Name(token.name));
    case SymbolEnum::NUMBER:
        return std::unique_ptr<Expression>(new Number(atoi(token.name.c_str())));
    default:
        return nullptr;
    }
}

std::unique_ptr<Expression> Parser::term(const Token& token)
{
    auto lhs = factor(token);
    if (!lhs)
        return nullptr;

    while (true)
    {
        const Token& op = tokenizer.nextToken();
        if (isMultDivOp(op))
        {
            auto rhs = factor(tokenizer.nextToken());
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



std::pair<std::string, std::unique_ptr<Expression>> Parser::bindings(const Token& token)
{
	if (token.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding");
	}
	if (tokenizer.nextToken().type != SymbolEnum::EQUALSSIGN)
	{
		throw std::runtime_error("Expected '=' in binding");
	}
	return std::make_pair(token.name, expression(tokenizer.nextToken()));
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

}