#pragma once
#include <fstream>
#include "Expression.h"
#include "Parser.h"

namespace MyVMNamespace
{

class Compiler
{
public:
    Compiler(std::istream& input)
        : tokenizer(input)
        , parser(tokenizer)
    {
    }

    std::vector<Instruction> compile()
    {
        std::vector<Instruction> ii;
        parser.run()->evaluate(ii);
        return std::move(ii);
    }

private:
    Tokenizer tokenizer;
    Parser parser;
};

}