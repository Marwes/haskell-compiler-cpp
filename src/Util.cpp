#include <algorithm>
#include <fstream>
#include <string>
#include <stdexcept>
#include <Util.h>

namespace MyVMNamespace
{

std::ostream& operator<<(std::ostream& out, const Location& loc)
{
	return out << loc.row << ":" << loc.column;
}

int swapEndian(int i)
{
    unsigned char b1, b2, b3, b4;

    b1 = i & 255;
    b2 = ( i >> 8 ) & 255;
    b3 = ( i>>16 ) & 255;
    b4 = ( i>>24 ) & 255;

    return ((int)b1 << 24) + ((int)b2 << 16) + ((int)b3 << 8) + b4;
}


#ifdef BIG_ENDIAN
#define FROM_BIGENDIAN(x) (x)
#else
#define FROM_BIGENDIAN(x) (std::reverse(static_cast<char*>(static_cast<void*>(x)), static_cast<char*>(static_cast<void*>(x)) + sizeof(x)))
#endif

}
