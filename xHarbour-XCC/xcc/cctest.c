#include <stdio.h>

#pragma comment(lib, "xcc.lib")

__declspec(dllimport) int __cdecl libmain(int argc, char **argv);

int main(int argc, char *argv[])
{
    int ret = libmain(argc, argv);

    printf("\nDLL C Compiler returned %d\n", ret);

    return ret;
}


