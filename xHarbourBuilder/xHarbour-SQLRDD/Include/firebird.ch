/*
* SQLRDD Firebird native constants
* Copyright (c) 2004 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#define IB_SQL_TEXT                           452
#define IB_SQL_VARYING                        448
#define IB_SQL_SHORT                          500
#define IB_SQL_LONG                           496
#define IB_SQL_FLOAT                          482
#define IB_SQL_DOUBLE                         480
#define IB_SQL_D_FLOAT                        530
#define IB_SQL_TIMESTAMP                      510
#define IB_SQL_BLOB                           520
#define IB_SQL_ARRAY                          540
#define IB_SQL_QUAD                           550
#define IB_SQL_TYPE_TIME                      560
#define IB_SQL_TYPE_DATE                      570
#define IB_SQL_INT64                          580
#define IB_SQL_DATE                        IB_SQL_TIMESTAMP

#define IB_DIALECT_V5				   1
#define IB_DIALECT_V6_TRANSITION    2
#define IB_DIALECT_V6				   3
#define IB_DIALECT_CURRENT		      IB_DIALECT_V6
