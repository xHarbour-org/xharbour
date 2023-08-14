/*
* C Header for SQL Parser
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef SQL_PARSER_INCLUDED
#define SQL_PARSER_INCLUDED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbsql.ch"

#if !defined(NULL)
#define NULL ((void*) 0)
#endif

#define PARSE_ALL_QUERY    0
#define isalnum_(c) (isalnum(c) || c == '_')

typedef struct sql_stmt_s {
   int command;
   int numParam;
   int errMsg;
   int where;
   const char* query;
   int queryLen;
   const char* queryPtr;
   const char* errPtr;
   PHB_ITEM pArray;
   PHB_ITEM pTemp;
} sql_stmt;

/* Prototypes */

PHB_ITEM SQLpCodeGenInt( int code );
PHB_ITEM SQLpCodeGenItemInt( PHB_ITEM value, int code );
PHB_ITEM SQLpCodeGenIntItem( int code, PHB_ITEM value );
PHB_ITEM SQLpCodeGenIntItem2( int code, PHB_ITEM value, int code2, PHB_ITEM value2 );
PHB_ITEM SQLpCodeGenIntArray( int code, PHB_ITEM pArray );
PHB_ITEM SQLpCodeGenArrayIntInt( PHB_ITEM pArray, int code, int code2 );
PHB_ITEM SQLpCodeGenArrayInt( PHB_ITEM pArray, int code );
PHB_ITEM SQLpCodeGenArrayJoin( PHB_ITEM pArray1, PHB_ITEM pArray2 );

#endif
