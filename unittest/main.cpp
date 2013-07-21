#define CATCH_CONFIG_RUNNER
#include <assert.h>
#include "Catch/include/catch.hpp"
#include <VM.h>

int main(int argc, char* const argv [])
{
	Catch::ConfigData data;
	data.shouldDebugBreak = true;
	Catch::Config config(data);
	int ret = Catch::Main(argc, argv, config);
	return ret;
}