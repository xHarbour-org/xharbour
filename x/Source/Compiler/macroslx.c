/*
 * ClipNet Project source code:
 * Lexer defines.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#define HB_MACRO_SUPPORT

#define yylex ClipNet_MacroLexer

#include "macroy.h"

#define MAX_STREAM_STARTER                          2
#define MAX_STREAM_TERMINATOR                       2
#define MAX_STREAM_EXCLUSIONS                       2

#define TOKEN_SIZE             HB_SYMBOL_NAME_LEN + 1

#define SLX_RULES "macro.slx"

/* this is relative to position of macroslx.c in harbour source tree */
#include "simplex.c"