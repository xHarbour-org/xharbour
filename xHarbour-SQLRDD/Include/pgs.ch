/*
* SQLRDD Postgres native constants
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef LIBPQ_FE_H

   #define CONNECTION_OK                       0
   #define CONNECTION_BAD                      1
   #define CONNECTION_STARTED                  2
   #define CONNECTION_MADE                     3
   #define CONNECTION_AWAITING_RESPONSE        4
   #define CONNECTION_AUTH_OK                  5
   #define CONNECTION_SETENV                   6
   #define PGRES_EMPTY_QUERY                   0
   #define PGRES_COMMAND_OK                    1
   #define PGRES_TUPLES_OK                     2
   #define PGRES_COPY_OUT                      3
   #define PGRES_COPY_IN                       4
   #define PGRES_BAD_RESPONSE                  5
   #define PGRES_NONFATAL_ERROR                6
   #define PGRES_FATAL_ERROR                   7
   #define PGRES_POLLING_FAILED                0
   #define PGRES_POLLING_READING               1
   #define PGRES_POLLING_WRITING               2
   #define PGRES_POLLING_OK                    3
   #define PGRES_POLLING_ACTIVE                4

#endif

#define BOOLOID               16
#define BYTEAOID              17
#define CHAROID               18
#define NAMEOID               19
#define INT8OID               20
#define INT2OID               21
#define INT2VECTOROID         22
#define INT4OID               23
#define REGPROCOID            24
#define TEXTOID               25
#define OIDOID                26
#define TIDOID                27
#define XIDOID                28
#define CIDOID                29
#define OIDVECTOROID          30
#define POINTOID              600
#define LSEGOID               601
#define PATHOID               602
#define BOXOID                603
#define POLYGONOID            604
#define LINEOID               628
#define FLOAT4OID             700
#define FLOAT8OID             701
#define ABSTIMEOID            702
#define RELTIMEOID            703
#define TINTERVALOID          704
#define UNKNOWNOID            705
#define CIRCLEOID             718
#define CASHOID               790
#define MACADDROID            829
#define INETOID               869
#define CIDROID               650
#define ACLITEMOID            1033
#define BPCHAROID             1042
#define VARCHAROID            1043
#define DATEOID               1082
#define TIMEOID               1083
#define TIMESTAMPOID          1114
#define TIMESTAMPTZOID        1184
#define INTERVALOID           1186
#define TIMETZOID             1266
#define BITOID                1560
#define VARBITOID             1562
#define NUMERICOID            1700
#define REFCURSOROID          1790
#define REGPROCEDUREOID       2202
#define REGOPEROID            2203
#define REGOPERATOROID        2204
#define REGCLASSOID           2205
#define REGTYPEOID            2206
#define RECORDOID             2249
#define CSTRINGOID            2275
#define ANYOID                2276
#define ANYARRAYOID           2277
#define VOIDOID               2278
#define TRIGGEROID            2279
#define LANGUAGE_HANDLEROID   2280
#define INTERNALOID           2281
#define OPAQUEOID             2282

