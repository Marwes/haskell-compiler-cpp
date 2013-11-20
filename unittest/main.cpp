#define CATCH_CONFIG_RUNNER
#include <assert.h>
#include <direct.h>
#include "Parser.h"
#include "Catch/include/catch.hpp"

std::string parseErrorTranslator(MyVMNamespace::ParseError& exc)
{
	return exc.what();
}
int main(int argc, char* const argv [])
{
    _chdir("unittest");
    char buffer[1024];
    printf("%s", _getcwd(buffer, sizeof(buffer)));
	Catch::ConfigData data;
	data.shouldDebugBreak = true;
	Catch::Config config(data);
	Catch::ExceptionTranslatorRegistrar::ExceptionTranslatorRegistrar(parseErrorTranslator);
	int ret = Catch::Main(argc, argv, config);
    if (ret != 0)//Wait for input if something has gone wrong
    {
        char c;
        std::cin.getline(&c, 1);
    }
	return ret;
}