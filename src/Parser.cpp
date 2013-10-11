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


const std::map<std::string, int> precedence = {
	std::make_pair("+", 1),
	std::make_pair("-", 1),
	std::make_pair("*", 3),
	std::make_pair("/", 3),
	std::make_pair("%", 3),
	std::make_pair("==", 1),
};

OP getOperand(const std::string& name)
{
	static const std::map<std::string, OP> operators = {
		std::make_pair("+", OP::ADD),
		std::make_pair("-", OP::SUBTRACT),
		std::make_pair("*", OP::MULTIPLY),
		std::make_pair("/", OP::DIVIDE),
		std::make_pair("%", OP::REMAINDER),
		std::make_pair("==", OP::COMPARE_EQ),
	};
	return operators.at(name);
}


std::unique_ptr<Expression> Parser::expression(const Token& token)
{
	{
		return parseOperatorExpression(application(token), 0);
	}
}

std::unique_ptr<Expression> Parser::subExpression(const Token& token)
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
			Let::Bindings binds;
			do
			{
				auto bind = binding(tokenizer.nextToken());
				binds.push_back(std::move(bind));
			} while (tokenizer.nextToken().type == SymbolEnum::SEMICOLON);

			if (tokenizer->type != SymbolEnum::IN)
				throw std::runtime_error(std::string("Expected 'in' token to end a let exprssion, got ") + enumToString(tokenizer->type));
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


std::unique_ptr<Expression> Parser::parseOperatorExpression(std::unique_ptr<Expression> lhs, int minPrecedence)
{
	++tokenizer;
	while (tokenizer->type == SymbolEnum::OPERATOR
		&& precedence.at(tokenizer->name) >= minPrecedence)
	{
		const Token& op = *tokenizer;
		std::unique_ptr<Expression> rhs = application(tokenizer.nextToken());
		const Token& nextOP = tokenizer.nextToken();
		while (tokenizer && nextOP.type == SymbolEnum::OPERATOR
			&& precedence.at(nextOP.name) > precedence.at(op.name))
		{
			const Token& lookahead = *tokenizer;
			--tokenizer;
			rhs = parseOperatorExpression(std::move(rhs), precedence.at(lookahead.name));
		}
		lhs = std::unique_ptr<Expression>(new PrimOP(getOperand(op.name), std::move(lhs), std::move(rhs)));
	}
	--tokenizer;
	return lhs;
}

std::unique_ptr<Expression> Parser::application(const Token& token)
{
	std::unique_ptr<Expression> lhs = subExpression(token);
	if (!lhs)
		return nullptr;

	std::vector<std::unique_ptr<Expression>> expressions;
	while (auto expr = subExpression(tokenizer.nextToken()))
	{
		expressions.push_back(std::move(expr));
	}
	if (expressions.size() > 0)
	{
		lhs = std::unique_ptr<Expression>(new Apply(std::move(lhs), std::move(expressions)));
	}
	--tokenizer;
    return lhs;
}



std::pair<std::string, std::unique_ptr<Expression>> Parser::binding(const Token& token)
{
	if (token.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding");
	}
	std::vector<std::string> arguments;
	while (true)
	{
		const Token& token = tokenizer.nextToken();
		if (token.type == SymbolEnum::NAME)
		{
			arguments.push_back(token.name);
		}
		else
		{
			break;
		}
	}
	if (tokenizer->type != SymbolEnum::EQUALSSIGN)
	{
		throw std::runtime_error("Expected '=' in binding");
	}
	if (arguments.size() > 0)
	{
		std::unique_ptr<Expression> lambda(new Lambda(std::move(arguments), expression(tokenizer.nextToken())));
		return std::make_pair(token.name, std::move(lambda));
	}
	else
	{
		return std::make_pair(token.name, expression(tokenizer.nextToken()));
	}
}

}