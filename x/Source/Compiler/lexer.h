/*
 * ClipNet Project source code:
 * Lexer defines.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef LEXER_DEFINED

   #define LEXER_DEFINED

   #define USING_CARGO TRUE
   #define CARGO Parser_pContext
   #define DEF_CARGO PARSER_CONTEXT * CARGO

   extern int yylex( DEF_CARGO );
#endif
