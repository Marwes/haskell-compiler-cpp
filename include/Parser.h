#pragma once
#include "Expression.h"
#include <vector>
#include <fstream>
#include <memory>

namespace MyVMNamespace
{
class Token;
class Tokenizer;

class Parser
{
public:
    Parser(Tokenizer& tokenizer);

    
    std::unique_ptr<Expression> run();

    std::unique_ptr<Expression> expression(const Token& token);
    std::unique_ptr<Expression> factor(const Token& token);
    std::unique_ptr<Expression> term(const Token& token);

    Tokenizer& tokenizer;
};

}