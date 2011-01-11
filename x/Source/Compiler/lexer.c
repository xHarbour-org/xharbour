/*
 * ClipNet Project source code:
 * Lexer main file
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */


#include "common.h"

#define MAX_STREAM_STARTER                          3 /* "[[" */
#define MAX_STREAM_TERMINATOR                       3 /* "]]"   */
#define MAX_STREAM_EXCLUSIONS                       2

#define SYMBOL_NAME_LEN                            64
#define TOKEN_SIZE                SYMBOL_NAME_LEN + 1

#define SLX_RULES "clipnet.slx"

#include "simplex.c"