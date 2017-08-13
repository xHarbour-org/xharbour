/*
* SQLPARSER
* PRG level pCode Header for SQL Parser (used from C also)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

/* pCode Instruction Set */

#define SQL_PCODE_VERSION                    1

#define SQL_PCODE_SELECT                     0
#define SQL_PCODE_INSERT                     1
#define SQL_PCODE_UPDATE                     2
#define SQL_PCODE_DELETE                     3

#define SQL_PCODE_COLUMN_NAME                4
#define SQL_PCODE_COLUMN_BY_VALUE            5
#define SQL_PCODE_COLUMN_PARAM               6
#define SQL_PCODE_COLUMN_BINDVAR             7
#define SQL_PCODE_COLUMN_ALIAS               8
#define SQL_PCODE_COLUMN_NO_AS               9
#define SQL_PCODE_COLUMN_AS                  10
#define SQL_PCODE_COLUMN_NAME_BINDVAR        11
#define SQL_PCODE_COLUMN_NAME_PARAM          12
#define SQL_PCODE_COLUMN_PARAM_NOTNULL       13


#define SQL_PCODE_LOCK                       20
#define SQL_PCODE_NOLOCK                     21

#define SQL_PCODE_NO_WHERE                   22
#define SQL_PCODE_WHERE                      23

#define SQL_PCODE_TABLE_NAME                 25
#define SQL_PCODE_TABLE_NO_ALIAS             26
#define SQL_PCODE_TABLE_ALIAS                27
#define SQL_PCODE_TABLE_PARAM                28
#define SQL_PCODE_TABLE_BINDVAR              29

#define SQL_PCODE_COLUMN_LIST_SEPARATOR      80

#define SQL_PCODE_START_EXPR                 100
#define SQL_PCODE_STOP_EXPR                  101
#define SQL_PCODE_NOT_EXPR                   102

#define SQL_PCODE_FUNC_COUNT_AST             200
#define SQL_PCODE_FUNC_COUNT                 201
#define SQL_PCODE_FUNC_ABS                   202
#define SQL_PCODE_FUNC_AVG                   203
#define SQL_PCODE_FUNC_ISNULL                204
#define SQL_PCODE_FUNC_MAX                   205
#define SQL_PCODE_FUNC_MIN                   206
#define SQL_PCODE_FUNC_POWER                 207
#define SQL_PCODE_FUNC_ROUND                 208
#define SQL_PCODE_FUNC_SUBSTR                209
#define SQL_PCODE_FUNC_SUBSTR2               210
#define SQL_PCODE_FUNC_SUM                   211
#define SQL_PCODE_FUNC_TRIM                  212
#define SQL_PCODE_FUNC_DATE                  213

#define SQL_PCODE_SELECT_ITEM_ASTERISK       300
#define SQL_PCODE_SELECT_ITEM_ALIAS_ASTER    301
#define SQL_PCODE_SELECT_ALL                 302
#define SQL_PCODE_SELECT_DISTINCT            303
#define SQL_PCODE_SELECT_NO_LIMIT            304
#define SQL_PCODE_SELECT_LIMIT               305
#define SQL_PCODE_SELECT_ORDER_ASC           306
#define SQL_PCODE_SELECT_ORDER_DESC          307
#define SQL_PCODE_SELECT_ORDER               308
#define SQL_PCODE_SELECT_NO_ORDER            309
#define SQL_PCODE_SELECT_NO_GROUPBY          310
#define SQL_PCODE_SELECT_GROUPBY             311
#define SQL_PCODE_SELECT_FROM                312
#define SQL_PCODE_SELECT_UNION               313
#define SQL_PCODE_SELECT_UNION_ALL           314

#define SQL_PCODE_INSERT_NO_LIST             400
#define SQL_PCODE_INSERT_VALUES              401

#define SQL_PCODE_OPERATOR_BASE              1000

#define SQL_PCODE_OPERATOR_IN                1002
#define SQL_PCODE_OPERATOR_NOT_IN            1003
#define SQL_PCODE_OPERATOR_IS_NULL           1004
#define SQL_PCODE_OPERATOR_IS_NOT_NULL       1005
#define SQL_PCODE_OPERATOR_AND               1006
#define SQL_PCODE_OPERATOR_OR                1007
#define SQL_PCODE_OPERATOR_EQ                1008
#define SQL_PCODE_OPERATOR_NE                1009
#define SQL_PCODE_OPERATOR_GT                1010
#define SQL_PCODE_OPERATOR_GE                1011
#define SQL_PCODE_OPERATOR_LT                1012
#define SQL_PCODE_OPERATOR_LE                1013
#define SQL_PCODE_OPERATOR_LIKE              1014
#define SQL_PCODE_OPERATOR_PLUS              1015
#define SQL_PCODE_OPERATOR_MINUS             1016
#define SQL_PCODE_OPERATOR_MULT              1017
#define SQL_PCODE_OPERATOR_DIV               1018
#define SQL_PCODE_OPERATOR_CONCAT            1019
#define SQL_PCODE_OPERATOR_NOT_LIKE          1020

#define SQL_PCODE_OPERATOR_JOIN              1100
#define SQL_PCODE_OPERATOR_LEFT_OUTER_JOIN   1101
#define SQL_PCODE_OPERATOR_RIGHT_OUTER_JOIN  1102

/* Error Messages */

#define SQL_PARSER_ERROR_PARSE               1

#define SQL_PARSER_ERROR_NUMBER              2
#define SQL_PARSER_ERROR_NUMBER_NEGATIVE     3
#define SQL_PARSER_ERROR_NUMBER_EOF          4
#define SQL_PARSER_ERROR_NUMBER_INTEGER      5
#define SQL_PARSER_ERROR_NUMBER_FLOAT        6

#define SQL_PARSER_ERROR_STRING              10
#define SQL_PARSER_ERROR_STRING_QUOTED       11
#define SQL_PARSER_ERROR_STRING_DATE         12

#define SQL_PARSER_ERROR_MEM                 110
#define SQL_PARSER_ERROR_OUT_OF_BOUNDS       111
#define SQL_PARSER_ERROR_INTERNAL            112
#define SQL_PARSER_ERROR_LIMIT               113

#define SQL_SINTAX_ERROR_OUTER_JOIN          400
#define SQL_SINTAX_ERROR_OUTER_JOIN_OR       401

/* Supported Database Engines */

#ifndef SYSTEMID_UNKNOW
   #define SYSTEMID_UNKNOW                0
   #define SYSTEMID_ORACLE                1
   #define SYSTEMID_MSSQL6                2
   #define SYSTEMID_MSSQL7                3
   #define SYSTEMID_SQLANY                4
   #define SYSTEMID_SYBASE                5
   #define SYSTEMID_ACCESS                6
   #define SYSTEMID_INGRES                7
   #define SYSTEMID_SQLBAS                8
   #define SYSTEMID_ADABAS                9
   #define SYSTEMID_INFORM               10
   #define SYSTEMID_IBMDB2               11
   #define SYSTEMID_MYSQL                12
   #define SYSTEMID_POSTGR               13
   #define SYSTEMID_FIREBR               14
   #define SYSTEMID_CACHE                15
   #define SYSTEMID_OTERRO               16
   #define SYSTEMID_PERVASIVE            17
   #define SYSTEMID_AZURE                18
   #define SYSTEMID_MARIADB              19
   #define SYSTEMID_FIREBR3              20 
#endif

/* Context Analisys Constants */

#define SQL_CONTEXT_RESET                 0

#define SQL_CONTEXT_SELECT_LIST           1
#define SQL_CONTEXT_SELECT_FROM           2
#define SQL_CONTEXT_SELECT_PRE_WHERE      3
#define SQL_CONTEXT_SELECT_PRE_WHERE2     4
#define SQL_CONTEXT_SELECT_WHERE          5
#define SQL_CONTEXT_SELECT_GROUP          6
#define SQL_CONTEXT_SELECT_ORDER          7
#define SQL_CONTEXT_SELECT_UNION          8

#define SQL_CONTEXT_INSERT                11
#define SQL_CONTEXT_UPDATE                21
#define SQL_CONTEXT_DELETE                31

