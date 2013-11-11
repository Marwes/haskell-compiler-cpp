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
	PrimOps op;
	int precedence;
};

static const std::map<std::string, Operator> operators = {
	std::make_pair("+", Operator { PrimOps::ADD, 1 }),
	std::make_pair("-", Operator { PrimOps::SUBTRACT, 1 }),
	std::make_pair("*", Operator { PrimOps::MULTIPLY, 3 }),
	std::make_pair("/", Operator { PrimOps::DIVIDE, 3 }),
	std::make_pair("%", Operator { PrimOps::REMAINDER, 3 }),
	std::make_pair("==", Operator { PrimOps::COMPARE_EQ, 1 }),
	std::make_pair("/=", Operator { PrimOps::COMPARE_NEQ, 1 }),
	std::make_pair("<", Operator { PrimOps::COMPARE_LT, 1 }),
	std::make_pair(">", Operator { PrimOps::COMPARE_GT, 1 }),
	std::make_pair("<=", Operator { PrimOps::COMPARE_LE, 1 }),
	std::make_pair(">=", Operator { PrimOps::COMPARE_GE, 1 }),
};

PrimOps getOperand(const std::string& name)
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
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::DATA;
}

bool toplevelNewBindError(const Token& t)
{
	return t.type != SymbolEnum::RBRACKET
		&& t.type != SymbolEnum::SEMICOLON;
}

bool bindingError(const Token& t)
{
	return t.type != SymbolEnum::EQUALSSIGN
		&& t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::TYPEDECL;
}


Module Parser::module()
{
	const Token& lBracket = tokenizer.tokenizeModule();
	if (lBracket.type != SymbolEnum::LBRACE)
	{
		throw std::runtime_error("Expected '{' in beginning of module, got " + std::string(enumToString(lBracket.type)));
	}

	Module module;

	const Token* semicolon;
	do
	{
		//Do a lookahead to see what the next top level binding is
		const Token& token = tokenizer.nextToken(toplevelError);
		if (token.type == SymbolEnum::NAME)
		{
			const Token& equalOrType = tokenizer.nextToken(bindingError);
			if (equalOrType.type == SymbolEnum::TYPEDECL)
			{
				--tokenizer;
				--tokenizer;
				auto bind = typeDeclaration();
				module.typeDeclaration.push_back(std::move(bind));
			}
			else
			{
				--tokenizer;
				--tokenizer;
				auto bind = binding();
				module.bindings.push_back(std::move(bind));
			}
		}
		else if (token.type == SymbolEnum::DATA)
		{
			--tokenizer;
			module.dataDefinitions.push_back(dataDefinition());
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

	if (tokenizer.nextToken().type != SymbolEnum::NONE)
	{
		throw std::runtime_error("Unexpected token after end of module, " + std::string(enumToString(tokenizer->type)));
	}

	return std::move(module);
}

std::unique_ptr<Expression> Parser::expression()
{
	{
		return parseOperatorExpression(application(), 0);
	}
}

//Create a tuple with the constructor name inferred from the number of arguments passed in
std::unique_ptr<Expression> newTuple(std::vector<std::unique_ptr<Expression>>&& arguments)
{
	std::string name(arguments.size() + 1, ',');
	name.front() = '(';
	name.back() = ')';
	return std::unique_ptr<Expression>(new Apply(std::unique_ptr<Expression>(new Name(name)), std::move(arguments)));
}

bool subExpressionError(const Token& t)
{
	return t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::LET
		&& t.type != SymbolEnum::CASE
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
			std::vector<std::unique_ptr<Expression>> expressions;
			const Token* comma;
			do
			{
				auto expr = expression();
				expressions.push_back(std::move(expr));
				comma = &tokenizer.nextToken();
			} while (comma->type == SymbolEnum::COMMA);

			const Token& maybeParens = *tokenizer;
			if (maybeParens.type == SymbolEnum::RPARENS)
			{
				if (expressions.size() == 1)
				{
					return std::move(expressions[0]);
				}
				else
				{
					return newTuple(std::move(expressions));
				}
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
				throw std::runtime_error(std::string("Expected 'in' token to end a let expression, got ") + enumToString(tokenizer->type));
			return std::unique_ptr<Expression>(new Let(std::move(binds), expression()));
		}
		break;
	case SymbolEnum::CASE:
		{
			std::unique_ptr<Expression> expr = expression();

			const Token& maybeOF = tokenizer.nextToken();
			if (maybeOF.type != SymbolEnum::OF)
			{
				throw std::runtime_error("Expected 'of' keyword, got " + std::string(enumToString(maybeOF.type)));
			}
			const Token& lBrace = tokenizer.nextToken();
			if (lBrace.type != SymbolEnum::LBRACE)
			{
				throw std::runtime_error("Expected bracket after 'of' keyword");
			}

			auto alts = many1(&Parser::alternative, SymbolEnum::SEMICOLON);
			const Token& rBrace = *tokenizer;
			if (rBrace.type != SymbolEnum::RBRACE)
			{
				throw std::runtime_error("Expected RBRACE after case expression");
			}
			return std::unique_ptr<Expression>(new Case(std::move(expr), std::move(alts)));
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

Alternative Parser::alternative()
{
	std::unique_ptr<Pattern> pat = pattern();

	const Token& arrow = tokenizer.nextToken();
	if (arrow.type != SymbolEnum::ARROW)
	{
		throw std::runtime_error("Expected '->' in alternative");
	}

	return Alternative(std::move(pat), expression());
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
		&& t.type != SymbolEnum::RPARENS
		&& t.type != SymbolEnum::LET
		&& t.type != SymbolEnum::OF
		&& t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::NUMBER
		&& t.type != SymbolEnum::OPERATOR
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::COMMA;
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
	//name1 = expr
	//or
	//name2 x y = expr
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

std::unique_ptr<Pattern> Parser::pattern()
{
	const Token& token = tokenizer.nextToken();
	switch (token.type)
	{
	case SymbolEnum::NAME:
		return std::unique_ptr<Pattern>(new PatternName(token.name));
	case SymbolEnum::NUMBER:
		return std::unique_ptr<Pattern>(new NumberLiteral(atoi(token.name.c_str())));
	case SymbolEnum::LPARENS:
		{
			auto tupleArgs = many1(&Parser::pattern, SymbolEnum::COMMA);
			const Token& rParens = *tokenizer;
			if (rParens.type != SymbolEnum::RPARENS)
			{
				throw std::runtime_error("Expected RPARENS, got" + std::string(enumToString(rParens.type)));
			}
			return std::unique_ptr<Pattern>(new ConstructorPattern(std::move(tupleArgs)));
		}
		break;
	default:
		break;
	}
	return nullptr;
}

TypeDeclaration Parser::typeDeclaration()
{
	const Token nameToken = tokenizer.nextToken(errorIfNotName);
	if (nameToken.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding " + std::string(enumToString(nameToken.type)));
	}
	const Token& decl = tokenizer.nextToken();
	if (decl.type != SymbolEnum::TYPEDECL)
	{
		throw std::runtime_error("Expected '::' in binding, got " + std::string(enumToString(tokenizer->type)));
	}
	Type t = type();
	--tokenizer;

	return TypeDeclaration(nameToken.name, std::move(t));
}

Constructor Parser::constructor()
{
	const Token& nameToken = tokenizer.nextToken();
	Constructor ctor;
	ctor.name = nameToken.name;
	const Token* tok = &tokenizer.nextToken();
	while (tok->type == SymbolEnum::NAME)
	{
		ctor.arity += 1;
		tok = &tokenizer.nextToken();
	}
	--tokenizer;
	return std::move(ctor);
}

DataDefinition Parser::dataDefinition()
{
	const Token& dataToken = tokenizer.nextToken();
	if (dataToken.type != SymbolEnum::DATA)
	{
		throw std::runtime_error("Expected DATA token");
	}
	const Token& dataName = tokenizer.nextToken();
	if (dataName.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME token");
	}
	const Token& equalToken = tokenizer.nextToken();
	if (equalToken.type != SymbolEnum::EQUALSSIGN)
	{
		throw std::runtime_error("Expected EQUAL token");
	}
	DataDefinition definition;
	definition.name = dataName.name;
	definition.constructors = many1(&Parser::constructor, [](const Token& t)
	{
		return t.type == SymbolEnum::OPERATOR && t.name == "|";
	});
	for (size_t ii = 0; ii < definition.constructors.size(); ii++)
	{
		definition.constructors[ii].tag = ii;
	}
	return std::move(definition);
}

bool typeParseError(const Token& t)
{
	return t.type != SymbolEnum::ARROW
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::RBRACE
		&& t.type != SymbolEnum::RPARENS;
}


Type tupleType(const std::vector<Type>& types)
{
	std::string name(types.size() + 1, ',');
	name.front() = '(';
	name.back() = ')';
	return TypeOperator(std::move(name), types);
}

Type Parser::type()
{
	Type result;
	const Token& token = tokenizer.nextToken();
	switch (token.type)
	{
	case SymbolEnum::LPARENS:
		{
			++tokenizer;
			const Token& maybeComma = tokenizer.nextToken();
			if (maybeComma.type == SymbolEnum::COMMA)
			{
				--tokenizer;
				--tokenizer;
				auto tupleArgs = many1(&Parser::type, SymbolEnum::COMMA);
				const Token& rParens = *tokenizer;
				if (rParens.type == SymbolEnum::RPARENS)
				{
					const Token& arrow = tokenizer.nextToken();
					if (arrow.type == SymbolEnum::ARROW)
					{
						return functionType(tupleType(tupleArgs), type());
					}
					return tupleType(tupleArgs);
				}
				throw std::runtime_error("Expected ')' to end tuple type");
			}
			else
			{
				--tokenizer;
				--tokenizer;
				auto arg = type();
				const Token& rParens = tokenizer.nextToken();
				if (rParens.type == SymbolEnum::RPARENS)
				{
					const Token& arrow = tokenizer.nextToken();
					if (arrow.type == SymbolEnum::ARROW)
					{
						return functionType(std::move(arg), type());
					}
					return std::move(arg);
				}
			}
		}
		break;
	case SymbolEnum::NAME:
		{
			const Token& arrow = tokenizer.nextToken(typeParseError);
			//TODO include find the correct type for the type variable
			if (arrow.type == SymbolEnum::ARROW)
			{
				return functionType(TypeVariable(), type());
			}
			if (arrow.type == SymbolEnum::COMMA || arrow.type == SymbolEnum::RPARENS)//in tuple type
				--tokenizer;
			return TypeVariable();
		}
		break;
	default:
		break;
	}
	return Type();
}

}