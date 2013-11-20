#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <array>
#include "Expression.h"
#include "Parser.h"
#include "Tokenizer.h"

namespace MyVMNamespace
{
std::string parseError(const Token& found, SymbolEnum expected)
{
	std::stringstream str;
	str << "Error: Unexpected token\n";
	str << "Expected: " << enumToString(expected) << "\n";
	str << "Found: " << enumToString(found.type) << " '" << found.name << "'";
	return str.str();
}
ParseError::ParseError(const Token& found, SymbolEnum expected)
	: std::runtime_error(parseError(found, expected).c_str())
{
}

Parser::Parser(Tokenizer& tokenizer)
    : tokenizer(tokenizer)
{
}


const Token& Parser::requireNext(SymbolEnum expected)
{
	const Token& tok = tokenizer.nextToken();
	if (tok.type != expected)
		throw ParseError(tok, expected);
	return tok;
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
	auto found = operators.find(name);
	if (found == operators.end())
		return 9;
	return found->second.precedence;
}

bool toplevelError(const Token& t)
{
	return t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::RBRACKET
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::DATA
		&& t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::CLASS
		&& t.type != SymbolEnum::INSTANCE;
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
		&& t.type != SymbolEnum::TYPEDECL
		&& t.type != SymbolEnum::OPERATOR;
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
		if (token.type == SymbolEnum::NAME || token.type == SymbolEnum::LPARENS)
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
		else if (token.type == SymbolEnum::CLASS)
		{
			--tokenizer;
			module.classes.push_back(klass());
		}
		else if (token.type == SymbolEnum::INSTANCE)
		{
			--tokenizer;
			module.instances.push_back(instance());
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

	const Token& none = tokenizer.nextToken();
	if (none.type != SymbolEnum::NONE)
	{
		throw std::runtime_error("Unexpected token after end of module, " + std::string(enumToString(tokenizer->type)));
	}

	return std::move(module);
}

Class Parser::klass()
{
	Class klass;

	requireNext(SymbolEnum::CLASS);

	const Token& className = requireNext(SymbolEnum::NAME);
	klass.name = className.name;

	const Token& typeVariable = requireNext(SymbolEnum::NAME);

	requireNext(SymbolEnum::WHERE);
	requireNext(SymbolEnum::LBRACE);
	std::map<std::string, TypeVariable> typeVariableMapping;
	typeVariableMapping[typeVariable.name] = klass.variable;
	std::vector<TypeDeclaration> decls = sepBy1(&Parser::typeDeclaration, typeVariableMapping, SymbolEnum::SEMICOLON);
	while (!decls.empty())
	{
		klass.declarations.insert(std::make_pair(decls.back().name, std::move(decls.back())));
		decls.pop_back();
	}
	
	--tokenizer;
	requireNext(SymbolEnum::RBRACE);

	return std::move(klass);
}

Instance Parser::instance()
{
	Instance inst;
	requireNext(SymbolEnum::INSTANCE);

	const Token& className = requireNext(SymbolEnum::NAME);
	//inst.name = className.name;
	
	inst.type = type();

	--tokenizer;
	requireNext(SymbolEnum::WHERE);
	requireNext(SymbolEnum::LBRACE);

	inst.bindings = sepBy1(&Parser::binding, SymbolEnum::SEMICOLON);

	--tokenizer;
	requireNext(SymbolEnum::RBRACE);
	return std::move(inst);
}

std::unique_ptr<Expression> Parser::expression()
{
	{
		return parseOperatorExpression(application(), 0);
	}
}

std::string tupleName(size_t size)
{
	std::string name(size + 1, ',');
	name.front() = '(';
	name.back() = ')';
	return std::move(name);
}

//Create a tuple with the constructor name inferred from the number of arguments passed in
std::unique_ptr<Expression> newTuple(std::vector<std::unique_ptr<Expression>>&& arguments)
{
	std::unique_ptr<Expression> name(new Name(tupleName(arguments.size())));
	return std::unique_ptr<Expression>(new Apply(std::move(name), std::move(arguments)));
}

bool subExpressionError(const Token& t)
{
	return t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::LET
		&& t.type != SymbolEnum::CASE
		&& t.type != SymbolEnum::NAME
		&& t.type != SymbolEnum::NUMBER
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::LBRACKET;
}


bool letExpressionEndError(const Token& t)
{
	return t.type != SymbolEnum::IN;
}

std::unique_ptr<Expression> parseList(Parser& parser, Tokenizer& tokenizer)
{
	std::vector<std::unique_ptr<Expression>> expressions;
	const Token* comma;
	do
	{
		auto expr = parser.expression();
		if (expr != nullptr)
			expressions.push_back(std::move(expr));
		comma = &tokenizer.nextToken();
	} while (comma->type == SymbolEnum::COMMA);

	if (expressions.empty())
	{
		return std::unique_ptr<Expression>(new Name("[]"));
	}

	std::unique_ptr<Expression> application;
	{
		std::vector<std::unique_ptr<Expression>> arguments(2);
		std::swap(arguments[0], expressions.back());
		expressions.pop_back();
		arguments[1] = std::unique_ptr<Expression>(new Name("[]"));

		application = std::unique_ptr<Expression>(new Apply(std::unique_ptr<Expression>(new Name(":")), std::move(arguments)));
	}
	while (!expressions.empty())
	{
		std::vector<std::unique_ptr<Expression>> arguments(2);
		std::swap(arguments[0], expressions.back());
		expressions.pop_back();
		arguments[1] = std::move(application);

		application = std::unique_ptr<Expression>(new Apply(std::unique_ptr<Expression>(new Name(":")), std::move(arguments)));
	}

	const Token& maybeParens = *tokenizer;
	if (maybeParens.type == SymbolEnum::RBRACKET)
	{
		return std::move(application);
	}
	else
	{
		--tokenizer;
		return nullptr;
	}
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
	case SymbolEnum::LBRACKET:
		{
			return parseList(*this, tokenizer);
		}
		break;
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

			auto alts = sepBy1(&Parser::alternative, SymbolEnum::SEMICOLON);
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
		std::unique_ptr<Expression> name(new Name(op.name));
		std::vector<std::unique_ptr<Expression>> args(2);
		args[0] = std::move(lhs);
		args[1] = std::move(rhs);
		lhs = std::unique_ptr<Expression>(new Apply(std::move(name), std::move(args)));
	}
	--tokenizer;
	return lhs;
}


bool applicationError(const Token& t)
{
	return t.type != SymbolEnum::LPARENS
		&& t.type != SymbolEnum::RPARENS
		&& t.type != SymbolEnum::LBRACKET
		&& t.type != SymbolEnum::RBRACKET
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


bool errorIfNotNameOrLParens(const Token& tok)
{
	return tok.type != SymbolEnum::NAME
		&& tok.type != SymbolEnum::LPARENS;
}
bool errorIfNotName(const Token& tok)
{
	return tok.type != SymbolEnum::NAME;
}
bool errorIfNotNameOrOperator(const Token& tok)
{
	return tok.type != SymbolEnum::NAME
		&& tok.type != SymbolEnum::OPERATOR;
}

bool errorIfNotNameOrEqual(const Token& tok)
{
	return tok.type != SymbolEnum::NAME && tok.type != SymbolEnum::EQUALSSIGN;
}
bool errorIfNotRParens(const Token& tok)
{
	return tok.type != SymbolEnum::RPARENS;
}

Binding Parser::binding()
{
	//name1 = expr
	//or
	//name2 x y = expr
	const Token& nameToken = tokenizer.nextToken(errorIfNotNameOrLParens);
	std::string name = nameToken.name;
	if (nameToken.type == SymbolEnum::LPARENS)
	{
		//Parse a name within parentheses
		const Token& functionName = tokenizer.nextToken(errorIfNotNameOrOperator);
		if (functionName.type != SymbolEnum::NAME && functionName.type != SymbolEnum::OPERATOR)
		{
			throw std::runtime_error("Expected NAME or OPERATOR on left side of binding " + std::string(enumToString(functionName.type)));
		}
		name = functionName.name;
		const Token& rParens = tokenizer.nextToken(errorIfNotRParens);
		if (rParens.type != SymbolEnum::RPARENS)
		{
			throw std::runtime_error("Expected RPARENS on left side of binding " + std::string(enumToString(rParens.type)));
		}
	}
	else if (nameToken.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding " + std::string(enumToString(nameToken.type)));
	}

	//Parse the arguments for the binding
	std::vector<std::string> arguments;
	while (true)
	{
		const Token& token = tokenizer.nextToken(errorIfNotNameOrEqual);
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
		return Binding(name, std::move(lambda));
	}
	else
	{
		return Binding(name, expression());
	}
}


std::vector<std::unique_ptr<Pattern>> Parser::patternParameter()
{
	std::vector<std::unique_ptr<Pattern>> parameters;
	while (true)
	{
		const Token& token = tokenizer.nextToken();
		switch (token.type)
		{
		case SymbolEnum::NAME:
			{
				parameters.push_back(std::unique_ptr<Pattern>(new PatternName(token.name)));
			}
			break;
		case SymbolEnum::NUMBER:
			parameters.push_back(std::unique_ptr<Pattern>(new NumberLiteral(atoi(token.name.c_str()))));
			break;
		case SymbolEnum::LPARENS:
			{
				auto pat = pattern();
				const Token& maybeComma = tokenizer.nextToken();
				if (maybeComma.type == SymbolEnum::COMMA)
				{
					auto tupleArgs = sepBy1(&Parser::pattern, SymbolEnum::COMMA);
					const Token& rParens = *tokenizer;
					if (rParens.type != SymbolEnum::RPARENS)
					{
						throw std::runtime_error("Expected RPARENS, got" + std::string(enumToString(rParens.type)));
					}
					tupleArgs.insert(tupleArgs.begin(), std::move(pat));
					parameters.push_back(std::unique_ptr<Pattern>(new ConstructorPattern(std::move(tupleArgs))));
				}
				else
				{
				}
			}
			break;
		default:
			goto endloop;
			break;
		}
	}
endloop:
	--tokenizer;
	return parameters;
}

std::unique_ptr<Pattern> Parser::pattern()
{
	const Token& nameToken = tokenizer.nextToken();
	switch (nameToken.type)
	{
	case SymbolEnum::OPERATOR:
	case SymbolEnum::NAME:
		{
			std::vector<std::unique_ptr<Pattern>> patterns = patternParameter();
			if (isupper(nameToken.name[0]) || nameToken.name == ":")
			{
				return std::unique_ptr<Pattern>(new ConstructorPattern(nameToken.name, std::move(patterns)));
			}
			else
			{
				assert(patterns.empty());
				return std::unique_ptr<Pattern>(new PatternName(nameToken.name));
			}
		}
	case SymbolEnum::NUMBER:
		return std::unique_ptr<Pattern>(new NumberLiteral(atoi(nameToken.name.c_str())));
	case SymbolEnum::LPARENS:
		{
			auto tupleArgs = sepBy1(&Parser::pattern, SymbolEnum::COMMA);
			const Token& rParens = *tokenizer;
			if (rParens.type != SymbolEnum::RPARENS)
			{
				throw std::runtime_error("Expected RPARENS, got" + std::string(enumToString(rParens.type)));
			}
			return std::unique_ptr<Pattern>(new ConstructorPattern(tupleName(tupleArgs.size()), std::move(tupleArgs)));
		}
		break;
	default:
		break;
	}
	return nullptr;
}

TypeDeclaration Parser::typeDeclaration()
{
	std::map<std::string, TypeVariable> typeVariableMapping;
	return typeDeclaration(typeVariableMapping);
}

TypeDeclaration Parser::typeDeclaration(std::map<std::string, TypeVariable>& typeVariableMapping)
{
	const Token& nameToken = tokenizer.nextToken(errorIfNotNameOrLParens);
	std::string name = nameToken.name;
	if (nameToken.type == SymbolEnum::LPARENS)
	{
		//Parse a name within parentheses
		const Token& functionName = tokenizer.nextToken(errorIfNotNameOrOperator);
		if (functionName.type != SymbolEnum::NAME && functionName.type != SymbolEnum::OPERATOR)
		{
			throw std::runtime_error("Expected NAME or OPERATOR on left side of binding " + std::string(enumToString(functionName.type)));
		}
		name = functionName.name;
		const Token& rParens = tokenizer.nextToken(errorIfNotRParens);
		if (rParens.type != SymbolEnum::RPARENS)
		{
			throw std::runtime_error("Expected RPARENS on left side of binding " + std::string(enumToString(rParens.type)));
		}
	}
	else if (nameToken.type != SymbolEnum::NAME)
	{
		throw std::runtime_error("Expected NAME on left side of binding " + std::string(enumToString(nameToken.type)));
	}
	const Token& decl = tokenizer.nextToken();
	if (decl.type != SymbolEnum::TYPEDECL)
	{
		throw std::runtime_error("Expected '::' in binding, got " + std::string(enumToString(tokenizer->type)));
	}
	Type typeOrContext = type(typeVariableMapping);
	if (tokenizer->type == SymbolEnum::OPERATOR && tokenizer->name == "=>")
	{
		Type t = type(typeVariableMapping);
		--tokenizer;
		TypeOperator& op = boost::get<TypeOperator>(typeOrContext);
		if (op.name[0] == '(')
		{
			return TypeDeclaration(std::move(name), std::move(t), std::move(op.types));
		}
		else
		{
			return TypeDeclaration(std::move(name), std::move(t), { typeOrContext });
		}
	}
	--tokenizer;

	return TypeDeclaration(std::move(name), std::move(typeOrContext));
}

bool constructorError(const Token& tok)
{
	return tok.type != SymbolEnum::NAME
		&& tok.type != SymbolEnum::OPERATOR
		&& tok.type != SymbolEnum::LPARENS;
}

Type constructorType(Tokenizer& tokenizer, int& arity, const DataDefinition& dataDef)
{
	const Token* token = &tokenizer.nextToken(constructorError);
	if (token->type == SymbolEnum::NAME)
	{
		arity++;
		if (islower(token->name[0]))
		{
			auto existingVariable = dataDef.parameters.find(token->name);
			if (existingVariable == dataDef.parameters.end())
				throw std::runtime_error("Undefined type parameter " + token->name);
			return functionType(existingVariable->second, constructorType(tokenizer, arity, dataDef));
		}
		else
			return functionType(TypeOperator(token->name), constructorType(tokenizer, arity, dataDef));
	}
	else
	{
		return dataDef.type;
	}
}

Constructor Parser::constructor(const DataDefinition& dataDef)
{
	const Token& nameToken = tokenizer.nextToken();
	int arity = 0;
	Type type = constructorType(tokenizer, arity, dataDef);
	--tokenizer;
	return Constructor(nameToken.name, type, 0, arity);
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
	DataDefinition definition;
	definition.type = TypeOperator(dataName.name);
	TypeOperator& op = boost::get<TypeOperator>(definition.type);
	while (tokenizer.nextToken().type == SymbolEnum::NAME)
	{
		op.types.push_back(TypeVariable());
		definition.parameters.insert(std::make_pair(tokenizer->name, op.types.back()));
	}
	const Token& equalToken = *tokenizer;
	if (equalToken.type != SymbolEnum::EQUALSSIGN)
	{
		throw std::runtime_error("Expected EQUAL token");
	}
	definition.name = dataName.name;
	definition.constructors = sepBy1(&Parser::constructor, definition,
		[](const Token& t)
	{
		return t.type == SymbolEnum::OPERATOR && t.name == "|";
	});
	for (size_t ii = 0; ii < definition.constructors.size(); ii++)
	{
		definition.constructors[ii].tag = ii;
	}
	--tokenizer;
	return std::move(definition);
}

bool typeParseError(const Token& t)
{
	return t.type != SymbolEnum::ARROW
		&& t.type != SymbolEnum::SEMICOLON
		&& t.type != SymbolEnum::RBRACE
		&& t.type != SymbolEnum::RPARENS
		&& t.type != SymbolEnum::RBRACKET;
}

Type tupleType(const std::vector<Type>& types)
{
	return TypeOperator(tupleName(types.size()), types);
}

Type Parser::type()
{
	std::map<std::string, TypeVariable> vars;
	return type(vars);
}
Type Parser::type(std::map<std::string, TypeVariable>& typeVariableMapping)
{
	Type result;
	const Token& token = tokenizer.nextToken();
	switch (token.type)
	{
	case SymbolEnum::LBRACKET:
		{
			Type t = type(typeVariableMapping);
			const Token& endList = tokenizer.nextToken();
			if (endList.type != SymbolEnum::RBRACKET)
			{
				throw std::runtime_error("Expected end of list in type declaration");
			}
			std::vector<Type> args(1);
			args[0] = t;
			Type listType = TypeOperator("[]", std::move(args));

			const Token& arrow = tokenizer.nextToken();
			if (arrow.type == SymbolEnum::ARROW)
			{
				return functionType(listType, type(typeVariableMapping));
			}
			return listType;
		}
	case SymbolEnum::LPARENS:
		{
			++tokenizer;
			const Token& maybeComma = tokenizer.nextToken();
			if (maybeComma.type == SymbolEnum::COMMA)
			{
				--tokenizer;
				--tokenizer;
				auto tupleArgs = sepBy1(&Parser::type, typeVariableMapping, SymbolEnum::COMMA);
				const Token& rParens = *tokenizer;
				if (rParens.type == SymbolEnum::RPARENS)
				{
					const Token& arrow = tokenizer.nextToken();
					if (arrow.type == SymbolEnum::ARROW)
					{
						return functionType(tupleType(tupleArgs), type(typeVariableMapping));
					}
					return tupleType(tupleArgs);
				}
				throw std::runtime_error("Expected ')' to end tuple type");
			}
			else
			{
				--tokenizer;
				--tokenizer;
				auto arg = type(typeVariableMapping);
				const Token& rParens = tokenizer.nextToken();
				if (rParens.type == SymbolEnum::RPARENS)
				{
					const Token& arrow = tokenizer.nextToken();
					if (arrow.type == SymbolEnum::ARROW)
					{
						return functionType(std::move(arg), type(typeVariableMapping));
					}
					return std::move(arg);
				}
			}
		}
		break;
	case SymbolEnum::NAME:
		{
			const Token* next = &tokenizer.nextToken();
			std::vector<Type> typeArguments;
			while (next->type == SymbolEnum::NAME)
			{
				if (typeVariableMapping.count(next->name) == 0)
				{
					typeVariableMapping[next->name] = TypeVariable();
				}
				typeArguments.push_back(typeVariableMapping[next->name]);
				next = &tokenizer.nextToken();
			}
			Type thisType;
			if (isupper(token.name[0]))
			{
				thisType = TypeOperator(token.name, typeArguments);
			}
			else
			{
				auto found = typeVariableMapping.find(token.name);
				if (found == typeVariableMapping.end())
				{
					thisType = typeVariableMapping[token.name] = TypeVariable();
				}
				else
				{
					thisType = found->second;
				}
			}
			if (next->type == SymbolEnum::ARROW)
			{
				thisType = functionType(thisType, type(typeVariableMapping));
			}
			if (next->type == SymbolEnum::COMMA
				|| next->type == SymbolEnum::RPARENS
				|| next->type == SymbolEnum::RBRACKET)//in tuple or list
				--tokenizer;
			return thisType;
		}
		break;
	default:
		break;
	}
	return Type();
}

}