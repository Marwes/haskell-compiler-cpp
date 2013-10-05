#define CATCH_CONFIG_RUNNER
#include <assert.h>
#include <direct.h>
#include "Catch/include/catch.hpp"
#include <VM.h>

int main(int argc, char* const argv [])
{
    _chdir("unittest");
    char buffer[1024];
    printf("%s", _getcwd(buffer, sizeof(buffer)));
	Catch::ConfigData data;
	data.shouldDebugBreak = true;
	Catch::Config config(data);
	int ret = Catch::Main(argc, argv, config);
    if (ret != 0)//Wait for input if something has gone wrong
    {
        char c;
        std::cin.getline(&c, 1);
    }
	return ret;
}