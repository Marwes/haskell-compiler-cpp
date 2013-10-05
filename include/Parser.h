#pragma once
#include "Expression.h"
#include <vector>
#include <fstream>
#include <memory>

namespace MyVMNamespace
{
enum class SymbolEnum
{
    NONE,
    NAME,
    OPERATOR,
    NUMBER,
    LPARENS,
    RPARENS
};

    
struct Token
{
    Token();
    void tokenize();

    SymbolEnum type;
    std::string name;
};

inline std::istream& operator>>(std::istream& input, Token& token)
{
    input >> token.name;
    token.tokenize();
    return input;
}

class Tokenizer
{
public:
    Tokenizer(std::istream& stream, size_t backTrack = 10)
        : stream(stream)
        , tokens(backTrack, Token())
        , currentToken(0)
    {
    }

    const Token& nextToken()
    {
        ++(*this);
        return **this;
    }

    bool tokenize()
    {
        stream >> tokens[currentToken];
        return *this;
    }

    const Token& operator*() const
    {
        return tokens[currentToken];
    }

    Tokenizer& operator++()
    {
        ++currentToken;
        tokenize();
        return *this;
    }
    Tokenizer& operator--()
    {
        --currentToken;
        return *this;
    }

    operator bool()
    {
        return stream.good();
    }

    bool operator!()
    {
        return !stream;
    }

private:
    std::istream& stream;
    std::vector<Token> tokens;
    size_t currentToken;
};

class Parser
{
public:
    Parser(Tokenizer& tokenizer);

    
    std::unique_ptr<Expression> run();

    std::unique_ptr<Expression> expression(const Token* token);

    std::unique_ptr<Expression> expressionEx(std::unique_ptr<Expression>& term, const Token* token);

    Tokenizer& tokenizer;
};

}