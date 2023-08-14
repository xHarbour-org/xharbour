
/*
* SQLRDD PRG Header (used by C code also)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef SQLRDD_CH
#define SQLRDD_CH

#define HB_SR__VERSION_STRING       "SQLRDD(EX) 9.0"
#define HB_SR__MGMNT_VERSION        "MGMNT 1.72"

#define MAX_FIELDS                  512

#define HB_SQLRDD_BUILD             15
#define DBI_RDD_BUILD               1030
#define DBI_INTERNAL_OBJECT         1001
#define DBI_CPCONVERTTO             1002
#define TOP_BOTTOM_SCOPE            2

#define WORKAREA_CLASS              "SR_WORKAREA"
#define HASH_TABLE_SIZE             1000

#define CONNECT_ODBC                   1
#define CONNECT_RPC                    2
#define CONNECT_MYSQL                  3
#define CONNECT_POSTGRES               4
#define CONNECT_ORACLE                 5
#define CONNECT_FIREBIRD               6
#define CONNECT_MARIA                  7
#define CONNECT_ORACLE2                8
#define CONNECT_FIREBIRD3              9

#define CONNECT_QUERY_ONLY           100
#define CONNECT_NOEXLOCK              50

#define CONNECT_ODBC_QUERY_ONLY        1 + CONNECT_QUERY_ONLY
#define CONNECT_RPC_QUERY_ONLY         2 + CONNECT_QUERY_ONLY
#define CONNECT_MYSQL_QUERY_ONLY       3 + CONNECT_QUERY_ONLY
#define CONNECT_POSTGRES_QUERY_ONLY    4 + CONNECT_QUERY_ONLY
#define CONNECT_ORACLE_QUERY_ONLY      5 + CONNECT_QUERY_ONLY
#define CONNECT_FIREBIRD_QUERY_ONLY    6 + CONNECT_QUERY_ONLY
#define CONNECT_MARIA_QUERY_ONLY       7 + CONNECT_QUERY_ONLY
#define CONNECT_ORACLE2_QUERY_ONLY     8 + CONNECT_QUERY_ONLY
#define CONNECT_FIREBIRD3_QUERY_ONLY   9 + CONNECT_QUERY_ONLY

#define CONNECT_ODBC_NOEXLOCK          1 + CONNECT_NOEXLOCK
#define CONNECT_RPC_NOEXLOCK           2 + CONNECT_NOEXLOCK
#define CONNECT_MYSQL_NOEXLOCK         3 + CONNECT_NOEXLOCK
#define CONNECT_POSTGRES_NOEXLOCK      4 + CONNECT_NOEXLOCK
#define CONNECT_ORACLE_NOEXLOCK        5 + CONNECT_NOEXLOCK
#define CONNECT_FIREBIRD_NOEXLOCK      6 + CONNECT_NOEXLOCK
#define CONNECT_MARIA_NOEXLOCK         7 + CONNECT_NOEXLOCK
#define CONNECT_ORACLE2_NOEXLOCK       8 + CONNECT_NOEXLOCK
#define CONNECT_FIREBIRD3_NOEXLOCK     9 + CONNECT_NOEXLOCK
#define CONNECT_CUSTOM               999

/* some errors */
#define ESQLRDD_OPEN                1001
#define ESQLRDD_CLOSE               1002
#define ESQLRDD_CREATE              1004
#define ESQLRDD_READ                1010
#define ESQLRDD_WRITE               1011
#define ESQLRDD_CORRUPT             1012
#define ESQLRDD_DATATYPE            1020
#define ESQLRDD_DATAWIDTH           1021
#define ESQLRDD_SHARED              1023
#define ESQLRDD_READONLY            1025
#define ESQLRDD_NOT_COMMITED_YET    1026

/* supported RDBMS */

#define SUPPORTED_DATABASES           21

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

#define LASTREC_POS             99999998

/* Log changes to SR_MGMNTLOGCHG - modes */

#define SQLLOGCHANGES_NOLOG                           0     /* Does not log */
#define SQLLOGCHANGES_BEFORE_COMMAND                  1     /* May log wrong statements */
#define SQLLOGCHANGES_AFTER_COMMAND                  10     /* Logs only if command was succefull */
#define SQLLOGCHANGES_IN_TRANSACTION                100     /* Otherwise log goes by second connection, outside transaction control */
#define SQLLOGCHANGES_LOCKS                        1000     /* Log Line LOCKS */
#define SQLLOGCHANGES_DELETE_AFTER_TRANSACTION    10000     /* Delete all previous ocourrences from current database connection when transaction is finished */
#define SQLLOGCHANGES_LOG_CALLSTACK              100000     /* Log Call Stack - good when looking for locked peers */

#define SQLLOGCHANGES_SIZE                            6

/* SR_MGMNTLOGCHG type column */

#define SQLLOGCHANGES_TYPE_DML                "01"
#define SQLLOGCHANGES_TYPE_DDL                "02"
#define SQLLOGCHANGES_TYPE_LOCK               "03"

/* Sequence per table */

#define SEQ_NOTDEFINED                         0
#define SEQ_PER_TABLE                          1
#define SEQ_PER_DATABASE                       2

/* Workarea Type */

#define WATYPE_UNDEF                   0
#define WATYPE_ISAM                    1
// #define WATYPE_CACHE                   2     --  Deprecated

/* eval( SQLGetTableInfoBlock(), cTableName ) return array constants */

#define TABLE_INFO_SIZE               16

#define TABLE_INFO_TABLE_NAME          1
#define TABLE_INFO_FILTERS             2
#define TABLE_INFO_PRIMARY_KEY         3
#define TABLE_INFO_RELATION_TYPE       4
#define TABLE_INFO_OWNER_NAME          5
#define TABLE_INFO_ALL_IN_CACHE        6
#define TABLE_INFO_CUSTOM_SQL          7
#define TABLE_INFO_CAN_INSERT          8
#define TABLE_INFO_CAN_UPDATE          9
#define TABLE_INFO_CAN_DELETE         10
#define TABLE_INFO_HISTORIC           11
#define TABLE_INFO_RECNO_NAME         12
#define TABLE_INFO_DELETED_NAME       13
#define TABLE_INFO_CONNECTION         14
#define TABLE_INFO_QUALIFIED_NAME     15
#define TABLE_INFO_NO_TRANSAC         16

/* Table Relation Methods */

#define TABLE_INFO_RELATION_TYPE_SELECT     0
#define TABLE_INFO_RELATION_TYPE_JOIN       1
#define TABLE_INFO_RELATION_TYPE_OUTER_JOIN 2

/* Historic management */

#define HISTORIC_ACTIVE_RECORD              0
#define HISTORIC_LAST_RECORD                1
#define HISTORIC_FIRST_RECORD               2

/* dbCreate Record Array Structure */

#define FIELD_INFO_SIZE                11

#define FIELD_NAME                      1
#define FIELD_TYPE                      2
#define FIELD_LEN                       3
#define FIELD_DEC                       4
#define FIELD_NULLABLE                  5
#define FIELD_DOMAIN                    6  /* Not used by dbCreate */
#define FIELD_MULTILANG                 7
#define FIELD_ENUM                      8  /* Not used by dbCreate */
#define FIELD_WAOFFSET                  9  /* Not used by dbCreate */
#define FIELD_PRIMARY_KEY              10
#define FIELD_UNIQUE                   11

#define MULTILANG_FIELD_ON             .T.
#define MULTILANG_FIELD_OFF            .F.

#ifndef SQL_ERRCODES
#include "sqlodbc.ch"
#endif

#ifndef SQLRDD_H
#define IS_SQLRDD   (Select() > 0 .and. (RddName()=="SQLRDD" .or. RddName()=="SQLEX"))
#endif

#endif
