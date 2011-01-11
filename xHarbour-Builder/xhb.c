#include <stdio.h>
#include <stdlib.h>

#include <windows.h>

#define _HB_PP_INTERNAL
#include "hbcomp.h"

#if 0
   void xHbStartDemo( void )
   {
      #ifdef DEMO
         MessageBox( 0, (LPCSTR) "This is a Demo of xHarbour, distributed by http://www.xHarbour.com", (LPCSTR) "http://www.xHarbour.com", 0 );
      #endif
   }

   void xHbEndDemo( void )
   {
      //MessageBox( 0, (LPCSTR) "Thanks for evaluating xHarbour, distributed by http://www.xHarbour.com", (LPCSTR) "http://www.xHarbour.com", 0 );
   }

   #pragma startup xHbStartDemo
   #pragma exit    xHbEndDemo
#endif

extern int libmain(int argc, char **argv);

#define MIN_ARGUMENTS 13

int xHarbourBackEnd( char *szFileName )
{
   int argc = MIN_ARGUMENTS;
   char **argv, *szObjName, *szOutSwitch, *pTmp;
   int ret, iLen;
   char szModulePath[ _MAX_PATH + 2 ], szPath_C_Include[ _MAX_PATH ], szPath_C_IncludeWin[ _MAX_PATH ], szPath_C_IncludeMSVC[ _MAX_PATH ];
   DWORD Len;
   HB_PATHNAMES *pInclude;
   void *pCopy;

   argv = (char **) malloc( sizeof( char* ) * MIN_ARGUMENTS );

   argv[ 0] = "<dummy don't use>";
   argv[ 1] = "-Ot";

   szModulePath[0] = '-';
   szModulePath[1] = 'I';

   Len = GetModuleFileName( NULL, szModulePath + 2, _MAX_PATH ) + 2;

   while( Len && szModulePath[Len] != '\\' )
   {
      Len--;
   }

   if( Len > 4 )
   {
      Len -= 4;
   }

   szModulePath[Len + 1] = '\0';

   strcpy( szPath_C_Include, szModulePath );
   strcat( szPath_C_Include, "c_include" );

   argv[2] = szPath_C_Include;

   strcpy( szPath_C_IncludeWin, szPath_C_Include );
   strcat( szPath_C_IncludeWin, "\\win" );

   argv[3] = szPath_C_IncludeWin;

   strcpy( szPath_C_IncludeMSVC, szPath_C_Include );
   strcat( szPath_C_IncludeMSVC, "\\msvc" );

   argv[4] = szPath_C_IncludeMSVC;

   strcat( szModulePath, "include" );

   argv[5] = szModulePath;

   szObjName = malloc( strlen( szFileName ) + 4 );

   pTmp = strrchr( szFileName, '.' );

   if( pTmp )
   {
      iLen = pTmp - szFileName + 1;

      strncpy( szObjName, szFileName, iLen );

      szObjName[iLen++] = 'o';
      szObjName[iLen++] = 'b';
      szObjName[iLen++] = 'j';
      szObjName[iLen++] = '\0';
   }
   else
   {
      iLen = strlen( szFileName );

      strncpy( szObjName, szFileName, iLen );

      szObjName[iLen++] = '.';
      szObjName[iLen++] = 'o';
      szObjName[iLen++] = 'b';
      szObjName[iLen++] = 'j';
      szObjName[iLen++] = '\0';
   }

   szOutSwitch = malloc( iLen + 3 );

   szOutSwitch[0] = '-';
   szOutSwitch[1] = 'F';
   szOutSwitch[2] = 'o';
   szOutSwitch[3] = '\0';
   strcat( szOutSwitch, szObjName );

   printf( "Generating object output to \'%s\'...\n", szObjName );
   free( szObjName );

   argv[ 6] = szOutSwitch;
   argv[ 7] = szFileName;

   argv[ 8] = "-DWIN32";

   argv[ 9] = "-DHB_NO_DEFAULT_API_MACROS";
   argv[10] = "-DHB_NO_DEFAULT_STACK_MACROS";

   argv[11] = "-Ot";

   argv[12] = "-Zi";

   pInclude = hb_comp_PP->pIncludePath;

   while( pInclude )
   {
      if( pInclude->szPath && pInclude->szPath[0] )
      {
         argv = (char **) realloc( argv, sizeof( char * ) * ++argc );
         argv[ argc - 1 ] = malloc( strlen( pInclude->szPath ) + 2 + 1 );
         strcpy( argv[ argc - 1 ], "-I" );
         strcat( argv[ argc - 1 ], pInclude->szPath );
      }

      pInclude = pInclude->pNext;
   }

   extern char **ArgV;
   extern int ArgC;

   {
      int i;

      for( i = 1; i < ArgC; i++ )
      {
         //MessageBox( 0, ArgV[i], "xHB.exe", 0 );

         if( ArgV[i][0] == '-' && ( ArgV[i][1] == 'd' || ArgV[i][1] == 'D' ) )
         {
            if( strncmp( ArgV[i] + 2, "HB_THREAD_SUPPORT", 17 + 1 ) == 0 )
            {
               argv = (char **) realloc( argv, sizeof( char * ) * ++argc );
               argv[ argc - 1 ] = malloc( 3 + 1 );
               strcpy( argv[ argc - 1 ], "-MT" );
            }

            argv = (char **) realloc( argv, sizeof( char * ) * ++argc );
            argv[ argc - 1 ] = malloc( strlen( ArgV[i] ) + 1 );
            strcpy( argv[ argc - 1 ], ArgV[i] );
         }
      }
   }

   #if 0
   //MessageBox( 0, szOutSwitch, "xHB.exe", 0 );
   {
      int i;

      for ( i = 0; i < argc; i++ )
      {
         OutputDebugString( argv[i] );
         OutputDebugString( " " );
      }
      OutputDebugString( "\n" );
   }
   #endif

   pCopy = (void *) argv;
   ret = libmain( argc, argv );

   free( szOutSwitch );

   while( --argc >= MIN_ARGUMENTS )
   {
      #if 0
      OutputDebugString( "Release: " );
      OutputDebugString( argv[ argc ] );
      OutputDebugString( "\n" );
      #endif
      free( (void *) argv[ argc ] );
   }

   free( pCopy );

   #if 0
   if( ret == 0 )
   {
      OutputDebugString( "Delete: " );
      OutputDebugString( szFileName );
      OutputDebugString( "\n" );

      remove( szFileName );
   }
   #endif

   return ret;
}

#include "hbcomp.h"

void hb_compGenObj32( PHB_FNAME pFileName )
{
}

/****************************************************************************
 *                                                                          *
 * File    : _putfield.c                                                    *
 *                                                                          *
 * Purpose : __putfield function.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <wchar.h>
#include "\xharbour.com\xharbour-xcc\xcc\crt\include\xmath.h"
#include "\xharbour.com\xharbour-xcc\xcc\crt\include\xstdio.h"

/* convert a field for __printf */
int __putfield(__prtinfo *px, va_list *pap, char code, char *ac)
{
    switch (code)
    {
        /* switch on conversion specifier */
        case 'c':  /* convert a single character */
            if (px->qual != 'l')
            {
                ac[px->n0++] = va_arg(*pap, int);
            }
            else
            {
                /* convert as wide string */
                wchar_t wc[2];
#if WCHAR_MAX < INT_MAX
                wint_t wi = (wint_t)va_arg(*pap, int);
#else /* WCHAR_MAX < INT_MAX */
                wint_t wi = va_arg(*pap, wint_t);
#endif /* WCHAR_MAX < INT_MAX */

                wc[0] = wi, wc[1] = L'\0';
                px->prec = -1;
                if (__putstr(px, (const wchar_t *)wc) < 0)
                    return EOF;
            }
            break;

        case 'd': case 'i':  /* convert a signed decimal */
            px->v.li = px->qual == 'l' ? va_arg(*pap, long) :
                       px->qual == 'q' ? va_arg(*pap, long long) :
                       px->qual == 'j' ? va_arg(*pap, intmax_t) : va_arg(*pap, int);

            if (px->qual == 'h')
                px->v.li = (short)px->v.li;
            else if (px->qual == 'b')
                px->v.li = (signed char)px->v.li;
            else if (px->qual == 't' || px->qual == 'z')
                px->v.li = (ptrdiff_t)px->v.li;

            if (px->v.li < 0)   /* negate safely in __litob */
                ac[px->n0++] = '-';
            else if (px->flags & _FPL)
                ac[px->n0++] = '+';
            else if (px->flags & _FSP)
                ac[px->n0++] = ' ';

            px->s = &ac[px->n0];
            __litob(px, code);
            break;

        case 'o': case 'u':
        case 'x': case 'X':  /* convert unsigned */
            px->v.li = px->qual == 'l' ? va_arg(*pap, unsigned long) :
                       px->qual == 'q' ? va_arg(*pap, unsigned long long) :
                       px->qual == 'j' ? va_arg(*pap, uintmax_t) : va_arg(*pap, unsigned int);

            if (px->qual == 'h')
                px->v.li = (unsigned short)px->v.li;
            else if (px->qual == 'b')
                px->v.li = (unsigned char)px->v.li;
            else if (px->qual == 't' || px->qual == 'z')
                px->v.li = (size_t)px->v.li;

            if (px->flags & _FNO && px->v.li != 0 && (code == 'x' || code == 'X'))
                ac[px->n0++] = '0', ac[px->n0++] = code;

            px->s = &ac[px->n0];
            __litob(px, code);
            break;

        case 'e': case 'E':  /* convert floating */
        case 'g': case 'G':
        case 'f': case 'F':
        case 'a': case 'A':
            px->v.ld = (px->qual == 'L') ? va_arg(*pap, long double) : va_arg(*pap, double);

            if (LSIGN(px->v.ld))
                ac[px->n0++] = '-';
            else if (px->flags & _FPL)
                ac[px->n0++] = '+';
            else if (px->flags & _FSP)
                ac[px->n0++] = ' ';

            px->s = &ac[px->n0];
            __ldtob(px, code);
            break;

        case 'n':  /* return output count */
            switch (px->qual)
            {
                /* store in specified integer type */
                case 'b':
                    *va_arg(*pap, signed char *) = px->nchar;
                    break;

                case 'q':
                    *va_arg(*pap, long long *) = px->nchar;
                    break;

                case 'j':
                    *va_arg(*pap, intmax_t *) = px->nchar;
                    break;

                case 't':
                    *va_arg(*pap, ptrdiff_t *) = px->nchar;
                    break;

                case 'z':
                    *va_arg(*pap, size_t *) = px->nchar;
                    break;

                case 'h':
                    *va_arg(*pap, short *) = px->nchar;
                    break;

                case 'l':
                    *va_arg(*pap, long *) = px->nchar;
                    break;

                default:
                    *va_arg(*pap, int *) = px->nchar;
                    break;
            }
            break;

        case 'p':  /* convert a pointer, hex long version */
            px->v.li = (long long)((char *)va_arg(*pap, void *) - (char *)0);

            if (sizeof(void *) == sizeof(unsigned long))
                px->v.li &= ULONG_MAX;

            px->s = &ac[px->n0];
            __litob(px, 'x');
            break;

        case 's':  /* convert a string */
            if (px->qual != 'l')
            {
                /* determine length safely */
                char *s1;

                px->s = va_arg(*pap, char *);
                if( px->s == NULL )
                {
                   px->s = "(NULL)";
                }

                px->n1 = (px->prec < 0) ? strlen(px->s) : (s1 = (char *)memchr((void *)px->s, '\0', px->prec)) != 0 ? s1 - px->s : px->prec;
            }
            else if (__putstr(px, va_arg(*pap, const wchar_t *)) < 0)
                return EOF;
            break;

        case '%':  /* put a '%' */
            ac[px->n0++] = '%';
            break;

        default:  /* undefined specifier, print it out */
            ac[px->n0++] = (code != '\0') ? code : '%';
    }

    return 0;
}

