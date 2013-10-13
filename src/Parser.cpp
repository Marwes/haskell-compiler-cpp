#include <string>
#include <fstream>
#include <algorithm>
#include <array>
#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"

namespace MyVMNamespace
{

Binding::Binding(std::string name, std::unique_ptr<Expression> expression)
	: name(std::move(name))
	, expression(std::move(expression))
{
}
Binding::Binding(Binding&& other)
	: name(std::move(other.name))
	, expression(std::move(other.expression))
{
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

struct Operator
{
	OP op;
	int precedence;
};

static const std::map<std::string, Operator> operators = {
	std::make_pair("+", Operator { OP::ADD, 1 }),
	std::make_pair("-", Operator { OP::SUBTRACT, 1 }),
	std::make_pair("*", Operator { OP::MULTIPLY, 3 }),
	std::make_pair("/", Operator { OP::DIVIDE, 3 }),
	std::make_pair("%", Operator { OP::REMAINDER, 3 }),
	std::make_pair("==", Operator { OP::COMPARE_EQ, 1 }),
	std::make_pair("/=", Operator { OP::COMPARE_NEQ, 1 }),
	std::make_pair("<", Operator { OP::COMPARE_LT, 1 }),
	std::make_pair(">", Operator { OP::COMPARE_GT, 1 }),
	std::make_pair("<=", Operator { OP::COMPARE_LE, 1 }),
	std::make_pair(">=", Operator { OP::COMPARE_GE, 1 }),
};

OP getOperand(const std::string& name)
{
	return operators.at(name).op;
}

int getPrecedence(const std::string& name)
{
	return operators.at(name).precedence;
}


std::vector<Binding> Parser::toplevel(const Token& token)
{
	std::vector<Binding> binds;

	while (true)
	{
		binding(token);
	}

	return std::move(binds);
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
			const Token& lbracket = tokenizer.nextToken();
			if (lbracket.type != SymbolEnum::LBRACKET)
			{
				throw std::runtime_error("Expected bracket after 'let' keyword");
			}
			std::vector<Binding> binds;
			const Token* semicolon;
			do
			{
				auto bind = binding(tokenizer.nextToken());
				binds.push_back(std::move(bind));
				semicolon = &tokenizer.nextToken();
			} while (semicolon->type == SymbolEnum::SEMICOLON);

			const Token& rBracket = *tokenizer;
			if (rBracket.type != SymbolEnum::RBRACKET)
			{
				throw std::runtime_error("Expected bracket after 'let' bindings");
			}
			const Token& inToken = tokenizer.nextToken();
			if (inToken.type != SymbolEnum::IN)
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
	while (tokenizer && tokenizer->type == SymbolEnum::OPERATOR
		&& getPrecedence(tokenizer->name) >= minPrecedence)
	{
		const Token& op = *tokenizer;
		std::unique_ptr<Expression> rhs = application(tokenizer.nextToken());
		const Token& nextOP = tokenizer.nextToken();
		while (tokenizer && nextOP.type == SymbolEnum::OPERATOR
			&& getPrecedence(nextOP.name) > getPrecedence(op.name))
		{
			const Token& lookahead = *tokenizer;
			--tokenizer;
			rhs = parseOperatorExpression(std::move(rhs), getPrecedence(lookahead.name));
		}
		if (rhs == nullptr)
		{
			return nullptr;
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



Binding Parser::binding(const Token& token)
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
		throw std::runtime_error("Expected '=' in binding, got " + std::string(enumToString(tokenizer->type)));
	}
	if (arguments.size() > 0)
	{
		std::unique_ptr<Expression> lambda(new Lambda(std::move(arguments), expression(tokenizer.nextToken())));
		return Binding(token.name, std::move(lambda));
	}
	else
	{
		return Binding(token.name, expression(tokenizer.nextToken()));
	}
}

}