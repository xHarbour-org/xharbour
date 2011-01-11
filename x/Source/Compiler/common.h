/*
 * ClipNet Project source code:
 * Common defines.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef COMMON_DEFINED

   #define COMMON_DEFINED
 
   #include <stdio.h>
   #include <stdlib.h>
   #include <string.h>
   #include <limits.h>
   #include <ctype.h>

   // Types
   #define BOOL int
   #define TRUE 1
   #define FALSE 0
   #define BYTE unsigned char
   #define LONG long
   #define ULONG unsigned long

   #include "errors.h"
   #include "new.h"
   #include "parser.h"
   #include "lexer.h"
   #include "pp.h"
   #include "reducer.h"
   #include "release.h"
   #include "tokens.h"
   #include "symbol.h"

   #ifdef __DEBUG__
      #define ASSERT( x ) ( (x) ? 0 : ( printf( #x ", ASSERT Failed line %i in: " __FILE__,  __LINE__ ), exit(1), 0 ) )
   #else
      #define ASSERT( x )
   #endif

   #define SYMBOL_UNUSED(x) (void) (x)

   #define ClipNet_IdentifierNew( p, b )  ClipNet_strdup( p )
   #define ClipNet_alloc( size )          malloc( size )
   #define ClipNet_free( p )              free( p )
   #define ClipNet_realloc( p, size )     realloc( p, size )

   #define CASE_STRING( x ) case x: return #x;

   #define STRINGIFY(x) #x
   #define TOSTRING(x) STRINGIFY(x)
   #define __SOURCE__                     __FILE__ ":" TOSTRING( __LINE__ )

   #if defined(__cplusplus)
      extern "C" {
   #endif

   char * ClipNet_strdup( const char *pString );
   char * ClipNet_LineKind( LINE_KIND Kind );

   #if defined(__cplusplus)
      }
   #endif

#endif