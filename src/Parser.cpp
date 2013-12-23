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

ParseError::ParseError(const std::string& errorMessage)
	: std::runtime_error(errorMessage)
{

}

ParseError::ParseError(const Token& found, SymbolEnum expected)
	: std::runtime_error(parseError(found, expected).c_str())
{
}

std::string parseError(Tokenizer& tokenizer, SymbolEnum expected)
{
	std::istream& input = tokenizer.getInputStream();
	const Token& token = *tokenizer;
	input.clear();
	auto originalPos = input.tellg();
	int start = std::max(0, token.sourceLocation.absolute - 20);
	input.seekg(start);
	char buffer[40] = { '\0' };
	input.read(buffer, 39);
	input.seekg(originalPos);

	std::ostringstream str;
	str << "Error: Unexpected token at " << token.sourceLocation.row << ":" << token.sourceLocation.column << "\n";
	str << "Found: " << enumToString(token.type) << " '" << token.name << "'\n";
	str << "Around: '" << buffer << "'\n";
	str << std::string(std::min(token.sourceLocation.absolute, 20) + 10, ' ') << "^";
	return str.str();
}

ParseError::ParseError(Tokenizer& tokenizer, SymbolEnum expected)
	: std::runtime_error(parseError(tokenizer, expected))
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

static const std::map<std::string, int> operators = {
	std::make_pair("+", 1),
	std::make_pair("-", 1),
	std::make_pair("*", 3),
	std::make_pair("/", 3),
	std::make_pair("%", 3),
	std::make_pair("==", 1),
	std::make_pair("/=", 1),
	std::make_pair("<", 1),
	std::make_pair(">", 1),
	std::make_pair("<=", 1),
	std::make_pair(">=", 1),
};


int getPrecedence(const std::string& name)
{
	auto found = operators.find(name);
	if (found == operators.end())
		return 9;
	return found->second;
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
		&& t.type != SymbolEnum::OPERATOR
		&& t.type != SymbolEnum::RPARENS;
}


Module Parser::module()
{
	Module module;
	const Token& lBracketOrModule = tokenizer.tokenizeModule();
	if (lBracketOrModule.type == SymbolEnum::MODULE)
	{
		const Token& modulename = requireNext(SymbolEnum::NAME);
		module.name = modulename.name;
		requireNext(SymbolEnum::WHERE);
		requireNext(SymbolEnum::LBRACE);
	}
	else if (lBracketOrModule.type == SymbolEnum::LBRACE)
	{
		//No module declaration was found so default to Main
		module.name = "Main";
	}
	else
	{
		throw ParseError(tokenizer, SymbolEnum::LBRACE);
	}


	const Token* semicolon;
	do
	{
		//Do a lookahead to see what the next top level binding is
		const Token& token = tokenizer.nextToken(toplevelError);
		if (token.type == SymbolEnum::NAME || token.type == SymbolEnum::LPARENS)
		{
			int numberOfLookaheads = 2;
			const Token* equalOrType = &tokenizer.nextToken(bindingError);
			while (equalOrType->type != SymbolEnum::TYPEDECL
				&& equalOrType->type != SymbolEnum::EQUALSSIGN)
			{
				equalOrType = &tokenizer.nextToken(bindingError);
				numberOfLookaheads++;
			}
			for (int ii = 0; ii < numberOfLookaheads; ii++)
			{
				--tokenizer;
			}

			if (equalOrType->type == SymbolEnum::TYPEDECL)
			{
				auto bind = typeDeclaration();
				module.typeDeclaration.push_back(std::move(bind));
			}
			else
			{
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
		throw ParseError(tokenizer, SymbolEnum::RBRACE);
	}

	const Token& none = tokenizer.nextToken();
	if (none.type != SymbolEnum::NONE)
	{
		throw ParseError("Unexpected token after end of module, " + std::string(enumToString(tokenizer->type)));
	}

	for (TypeDeclaration& decl : module.typeDeclaration)
	{
		for (Binding& bind : module.bindings)
		{
			if (decl.name == bind.name)
			{
				bind.type = decl;
			}
		}
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
	inst.className = className.name;
	
	inst.type = type();

	--tokenizer;
	requireNext(SymbolEnum::WHERE);
	requireNext(SymbolEnum::LBRACE);

	inst.bindings = sepBy1(&Parser::binding, SymbolEnum::SEMICOLON);
	for (Binding& bind : inst.bindings)
	{
		bind.name = encodeBindingName(boost::get<TypeOperator>(inst.type).name, bind.name);
	}

	--tokenizer;
	requireNext(SymbolEnum::RBRACE);
	return std::move(inst);
}

std::unique_ptr<Expression> Parser::expression()
{
	auto app = application();
	if (app == nullptr)
		return nullptr;
	return parseOperatorExpression(std::move(app), 0);
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
		&& t.type != SymbolEnum::FLOAT
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
	if (maybeParens.type != SymbolEnum::RBRACKET)
	{
		throw ParseError(tokenizer, SymbolEnum::RBRACKET);
	}
	else
	{
		return std::move(application);
	}
}

std::unique_ptr<Expression> Parser::subExpression(bool (*parseError)(const Token&))
{
	const Token& token = tokenizer.nextToken(parseError);
	switch (token.type)
	{
	case SymbolEnum::LPARENS:
		{
			std::vector<std::unique_ptr<Expression>> expressions = sepBy1(&Parser::expression, SymbolEnum::COMMA);

			const Token& maybeParens = *tokenizer;

			if (maybeParens.type != SymbolEnum::RPARENS)
			{
				throw ParseError(tokenizer, SymbolEnum::RPARENS);
			}
			if (expressions.size() == 1)
			{
				return std::move(expressions[0]);
			}
			else
			{
				return newTuple(std::move(expressions));
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
			requireNext(SymbolEnum::LBRACE);

			std::vector<Binding> binds = sepBy1(&Parser::binding, SymbolEnum::SEMICOLON);

			const Token& rBracket = *tokenizer;
			if (rBracket.type != SymbolEnum::RBRACE)
			{
				throw ParseError(tokenizer, SymbolEnum::RBRACE);
			}
			const Token& inToken = tokenizer.nextToken(letExpressionEndError);
			if (inToken.type != SymbolEnum::IN)
				throw ParseError(tokenizer, SymbolEnum::IN);
			return std::unique_ptr<Expression>(new Let(std::move(binds), expression(), token.sourceLocation));
		}
		break;
	case SymbolEnum::CASE:
		{
			std::unique_ptr<Expression> expr = expression();

			requireNext(SymbolEnum::OF);
			requireNext(SymbolEnum::LBRACE);

			auto alts = sepBy1(&Parser::alternative, SymbolEnum::SEMICOLON);
			const Token& rBrace = *tokenizer;
			if (rBrace.type != SymbolEnum::RBRACE)
			{
				throw ParseError(tokenizer, SymbolEnum::RBRACE);
			}
			return std::unique_ptr<Expression>(new Case(std::move(expr), std::move(alts), token.sourceLocation));
		}
		break;
    case SymbolEnum::NAME:
        return std::unique_ptr<Expression>(new Name(token.name, token.sourceLocation));
    case SymbolEnum::NUMBER:
        return std::unique_ptr<Expression>(new Number(atoi(token.name.c_str()), token.sourceLocation));
	case SymbolEnum::FLOAT:
		return std::unique_ptr<Expression>(new Rational(atof(token.name.c_str()), token.sourceLocation));
	default:
		--tokenizer;
        return nullptr;
    }
}

Alternative Parser::alternative()
{
	std::unique_ptr<Pattern> pat = pattern();

	requireNext(SymbolEnum::ARROW);

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
			++tokenizer;
		}
		if (rhs == nullptr)
		{
			return nullptr;
		}
		std::unique_ptr<Expression> name(new Name(op.name, op.sourceLocation));
		std::vector<std::unique_ptr<Expression>> args(2);
		args[0] = std::move(lhs);
		args[1] = std::move(rhs);
		Location loc = args[0]->sourceLocation;
		lhs = std::unique_ptr<Expression>(new Apply(std::move(name), std::move(args), loc));
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
		&& t.type != SymbolEnum::FLOAT
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
		Location loc = lhs->sourceLocation;
		lhs = std::unique_ptr<Expression>(new Apply(std::move(lhs), std::move(expressions), loc));
	}
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
			throw ParseError(tokenizer, SymbolEnum::RPARENS);
		}
	}
	else if (nameToken.type != SymbolEnum::NAME)
	{
		throw ParseError(tokenizer, SymbolEnum::NAME);
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
		throw ParseError(tokenizer, SymbolEnum::EQUALSSIGN);
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
						throw ParseError(tokenizer, SymbolEnum::RPARENS);
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
	case SymbolEnum::LBRACKET:
		{
			if (tokenizer.nextToken().type != SymbolEnum::RBRACKET)
			{
				throw ParseError(tokenizer, SymbolEnum::RBRACKET);
			}
			return std::unique_ptr<Pattern>(new ConstructorPattern("[]", std::vector<std::unique_ptr<Pattern>>()));
		}
		break;
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
				throw ParseError(tokenizer, SymbolEnum::RPARENS);
			}
			return std::unique_ptr<Pattern>(new ConstructorPattern(tupleName(tupleArgs.size()), std::move(tupleArgs)));
		}
		break;
	default:
		break;
	}
	return nullptr;
}

std::vector<TypeOperator> createTypeConstraints(const Type& context)
{
	std::vector<TypeOperator> mapping;

	const TypeOperator& op = boost::get<TypeOperator>(context);
	if (op.name[0] == '(')
	{
		for (const Type& type : op.types)
		{
			mapping.push_back(boost::get<TypeOperator>(type));
		}
	}
	else
	{
		mapping.push_back(op);
	}
	return mapping;
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
			throw ParseError(tokenizer, SymbolEnum::RPARENS);
		}
	}
	else if (nameToken.type != SymbolEnum::NAME)
	{
		throw ParseError(tokenizer, SymbolEnum::NAME);
	}
	const Token& decl = tokenizer.nextToken();
	if (decl.type != SymbolEnum::TYPEDECL)
	{
		throw ParseError(tokenizer, SymbolEnum::TYPEDECL);
	}
	Type typeOrContext = type(typeVariableMapping);
	const Token& maybeContextArrow = *tokenizer;
	if (maybeContextArrow.type == SymbolEnum::OPERATOR && maybeContextArrow.name == "=>")
	{
		Type t = type(typeVariableMapping);
		--tokenizer;
		TypeOperator& op = boost::get<TypeOperator>(typeOrContext);
		return TypeDeclaration(std::move(name), std::move(t), createTypeConstraints(op));
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
	requireNext(SymbolEnum::DATA);
	const Token& dataName = requireNext(SymbolEnum::NAME);

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
		throw ParseError(tokenizer, SymbolEnum::EQUALSSIGN);
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
			requireNext(SymbolEnum::RBRACKET);
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
			Type t = type(typeVariableMapping);
			const Token& maybeComma = tokenizer.nextToken();
			if (maybeComma.type == SymbolEnum::COMMA)
			{
				auto tupleArgs = sepBy1(&Parser::type, typeVariableMapping, SymbolEnum::COMMA);
				tupleArgs.insert(tupleArgs.begin(), t);
				const Token& rParens = *tokenizer;
				if (rParens.type != SymbolEnum::RPARENS)
				{
					throw ParseError(tokenizer, SymbolEnum::RPARENS);
				}
				const Token& arrow = tokenizer.nextToken();
				if (arrow.type == SymbolEnum::ARROW)
				{
					return functionType(tupleType(tupleArgs), type(typeVariableMapping));
				}
				return tupleType(tupleArgs);
			}
			else if (maybeComma.type == SymbolEnum::RPARENS)
			{
				const Token& arrow = tokenizer.nextToken();
				if (arrow.type == SymbolEnum::ARROW)
					return functionType(t, type());
				else
				{
					--tokenizer;
					return t;
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