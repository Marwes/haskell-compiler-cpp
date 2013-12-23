#include <iostream>
#include <fstream>
#include <string>
#include "Typecheck.h"
#include "GMachine.h"

using namespace MyVMNamespace;

std::string readWord(const std::string& str, size_t start)
{
	size_t ii = start;
	//Skip preceding whitespace
	while (ii < str.size() && isspace(str[ii]))
	{
		ii++;
	}
	std::string result;
	for (; ii < str.size(); ii++)
	{
		if (isspace(str[ii]))
			break;
		else
			result.push_back(str[ii]);
	}
	return result;
}

class REPL
{
public:
	REPL()
		: globalTypeEnv(&module)
	{}

	void runCommand(const std::string& line)
	{
		if (line.size() < 2)
		{
			throw std::runtime_error("Unknown command" + line);
		}
		std::string command = readWord(line, 1);
		if (command == "l")
		{
			size_t startOfFilename = command.size() + 1;
			while (startOfFilename < line.size() && isspace(line[startOfFilename]))
			{
				startOfFilename++;
			}
			if (startOfFilename >= line.size())
			{
				throw std::runtime_error("Expected filename to load command");
			}
			std::string filename(line.begin() + startOfFilename, line.end());

			std::ifstream file(filename);

			Tokenizer tokenizer(file);
			Parser parser(tokenizer);
			Module newModule = parser.module();
			TypeEnvironment typeEnv = newModule.typecheck(assemblies);
			GCompiler compiler(typeEnv, &newModule);
			char buffer[32];
			std::string name = itoa(assemblyIndex++, buffer, 10);
			Assembly& assembly = machine.addAssembly(compiler.compileModule(newModule));
			assemblies.insert(std::make_pair(name, &assembly));
			module.imports.push_back(name);
			globalTypeEnv.addAssembly(name, &assembly);
			globalTypeEnv.addConstraints(typeEnv.getAllConstraints());
		}
		else if (command == "q")
		{
			exit(0);
		}
		else
		{
			throw std::runtime_error("Unknown command " + line);
		}
	}

	void evaluate(const std::string& line)
	{
		GCompiler compiler(globalTypeEnv, &module, 0, assemblies);
		std::stringstream stream(line);
		Tokenizer tokenizer(stream);
		Parser parser(tokenizer);
		if (std::unique_ptr<Expression> expr = parser.expression())
		{
			Type& type = expr->typecheck(globalTypeEnv);
			//assert(type.which() != 0);

			SuperCombinator comb;
			comb.arity = 0;
			expr->compile(compiler, comb.instructions, true);
			Address addr = machine.evaluate(comb);

			std::cout << addr << std::endl;
		}
		else
		{
			std::cout << "Failed to parse expression" << std::endl;
		}
	}

private:
	int assemblyIndex;
	TypeEnvironment globalTypeEnv;
	std::map<std::string, Assembly*> assemblies;
	Module module;
	GMachine machine;
};

int main(int argc, char** argv)
{
	REPL repl;
	std::string line;
	while (std::getline(std::cin, line))
	{
		if (line.size() > 0)
		{
			try
			{
				if (line[0] == ':')
				{
					repl.runCommand(line);
				}
				else
				{
					repl.evaluate(line);
				}
			}
			catch (std::runtime_error& exc)
			{
				std::cout << exc.what() << std::endl;
			}
		}
	}

	return 0;
}