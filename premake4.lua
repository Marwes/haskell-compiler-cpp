
DEV_VAR = os.getenv("DEV")
BOOST_DIR = path.join(DEV_VAR, "boost_1_54_0")

solution "VM"
    configurations { "Debug", "Release" }
	
    configuration "Debug"
     	flags { "Symbols" }
     	defines { "DEBUG" }

	configuration "Release"
     	flags { "Optimize" } 
     	defines { "NDEBUG" }

    project "unittest"

    project "vm"
        language "C++"
        kind "StaticLib"
        targetdir "bin"
        includedirs { "include", BOOST_DIR }

        files {
           "include/*.h",
           "src/*.cpp"
        }

        configuration "Debug"
        configuration "Release"

    project "unittest"
        language "C++"
        kind "ConsoleApp"
        targetdir "bin"
        includedirs { "include", BOOST_DIR }

        links { "vm" }
        
        files {
           "unittest/*.h",
           "unittest/*.cpp",
        }

        configuration "Debug"
        configuration "Release"
        

