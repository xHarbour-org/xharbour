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
   #include <assert.h>

   // Types
   #define BOOL int
   #define TRUE 1
   #define FALSE 0
   #define BYTE unsigned char
   #define LONG long
   #define ULONG unsigned long

   #include "tokens.h"
   #include "parser.h"
   #include "errors.h"
   #include "new.h"
   #include "lexer.h"
   #include "pp.h"
   #include "reducer.h"
   #include "release.h"
   #include "symbol.h"
   #include "iterator.h"
   #include "emitter.h"
   #include "map.h"

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

   extern char * ClipNet_strdup( const char *pString );

   extern const char * ClipNet_MacroKind( const MACRO *pMacro );
   extern const char * ClipNet_DeclaredKind( const DECLARED *pDeclared );
   extern const char * ClipNet_LValueKind( const VALUE *pLValue );
   extern const char * ClipNet_LineKind( const LINE *pLine );
   extern const char * ClipNet_ValueKind( const VALUE *pValue );
   extern const char * ClipNet_BinaryKind( const BINARY *pBinary );
   extern const char * ClipNet_UnaryKind( const UNARY *pUnary );

   #if defined(__cplusplus)
      }
   #endif

#endif
