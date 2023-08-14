#include <stdio.h>
#include <wtypes.h>

extern "C" int _imp___vsnprintf(
	char *buffer,
	size_t count,
	const char *format,
	va_list argptr
	)
{
	return vsnprintf( buffer, count, format, argptr );
}
