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
    return expression();
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

bool toplevelError(const Token& t)
{
	return t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::RBRACKET
		&& t.type != SymbolEnum::SEMICOLON;
}

bool toplevelNewBindError(const Token& t)
{
	return t.type != SymbolEnum::RBRACKET
		&& t.type != SymbolEnum::SEMICOLON;
}


std::vector<Binding> Parser::toplevel()
{
	const Token& lBracket = tokenizer.tokenizeModule();
	if (lBracket.type != SymbolEnum::LBRACE)
	{
		throw std::runtime_error("Expected '{' in beginning of module, got " + std::string(enumToString(lBracket.type)));
	}

	std::vector<Binding> binds;

	const Token* semicolon;
	do
	{
		const Token& token = tokenizer.nextToken(toplevelError);
		if (token.type == SymbolEnum::NAME)
		{
			--tokenizer;
			auto bind = binding();
			binds.push_back(std::move(bind));
		}
		else
		{
			break;
		}
		semicolon = &tokenizer.nextToken(toplevelNewBindError);
	} while (semicolon->type == SymbolEnum::SEMICOLON);

	const Token& rBracket = *tokenizer;
	if (rBracket.type != SymbolEnum::RBRACE)
	{
		throw std::runtime_error("Expected '}' to end module, got " + std::string(enumToString(rBracket.type)));
	}

	return std::move(binds);
}

std::unique_ptr<Expression> Parser::expression()
{
	{
		return parseOperatorExpression(application(), 0);
	}
}

bool subExpressionError(const Token& t)
{
	return t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::LET
		&& t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::NUMBER
		&& t.type != SymbolEnum::SEMICOLON;
}


bool letExpressionEndError(const Token& t)
{
	return t.type != SymbolEnum::IN;
}

std::unique_ptr<Expression> Parser::subExpression(bool (*parseError)(const Token&))
{
	const Token& token = tokenizer.nextToken(parseError ? parseError : subExpressionError);
	switch (token.type)
	{
	case SymbolEnum::LPARENS:
		{
			std::unique_ptr<Expression> result = expression();
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
			if (lbracket.type != SymbolEnum::LBRACE)
			{
				throw std::runtime_error("Expected bracket after 'let' keyword");
			}
			std::vector<Binding> binds;
			const Token* semicolon;
			do
			{
				auto bind = binding();
				binds.push_back(std::move(bind));
				semicolon = &tokenizer.nextToken();
			} while (semicolon->type == SymbolEnum::SEMICOLON);

			const Token& rBracket = *tokenizer;
			if (rBracket.type != SymbolEnum::RBRACE)
			{
				throw std::runtime_error("Expected bracket after 'let' bindings");
			}
			const Token& inToken = tokenizer.nextToken(letExpressionEndError);
			if (inToken.type != SymbolEnum::IN)
				throw std::runtime_error(std::string("Expected 'in' token to end a let exprssion, got ") + enumToString(tokenizer->type));
			return std::unique_ptr<Expression>(new Let(std::move(binds), expression()));
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
		std::unique_ptr<Expression> rhs = application();
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


bool applicationError(const Token& t)
{
	return t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::LET
		&& t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::NUMBER
		&& t.type != SymbolEnum::OPERATOR
		&& t.type != SymbolEnum::SEMICOLON;
}

std::unique_ptr<Expression> Parser::application()
{
	std::unique_ptr<Expression> lhs = subExpression();
	if (!lhs)
		return nullptr;

	std::vector<std::unique_ptr<Expression>> expressions;
	while (auto expr = subExpression(applicationError))
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


bool errorIfNotName(const Token& tok)
{
	return tok.type != SymbolEnum::NAME;
}

bool errorIfNotNameOrEqul(const Token& tok)
{
	return tok.type != SymbolEnum::NAME && tok.type != SymbolEnum::EQUALSSIGN;
}

Binding Parser::binding()
{
	const Token nameToken = tokenizer.nextToken(errorIfNotName);
	if (nameToken.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding " + std::string(enumToString(nameToken.type)));
	}
	std::vector<std::string> arguments;
	while (true)
	{
		const Token& token = tokenizer.nextToken(errorIfNotNameOrEqul);
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
		std::unique_ptr<Expression> lambda(new Lambda(std::move(arguments), expression()));
		return Binding(nameToken.name, std::move(lambda));
	}
	else
	{
		return Binding(nameToken.name, expression());
	}
}

}