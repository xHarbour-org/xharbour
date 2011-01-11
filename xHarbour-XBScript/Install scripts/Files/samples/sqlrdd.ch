
/*
* SQLRDD PRG Header (used by C code also)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef SQLRDD_CH
#define SQLRDD_CH

#define HB_SR__VERSION_STRING       "SQLRDD 5.5"
#define HB_SR__MGMNT_VERSION        "MGMNT 1.50"

#define HB_SQLRDD_BUILD               80
#define DBI_RDD_BUILD               1030

#define DBI_INTERNAL_OBJECT         1001
#define EXCLUSIVE_TABLE_LOCK_SIGN   "SQL_EXCLUSIVE_TABLE_$_"
#define FLOCK_TABLE_LOCK_SIGN       "SQL_FLOCK_TABLE_$_"
#define SHARED_TABLE_LOCK_SIGN      "SQL_SHARED_TABLE_$_"
#define LAST_CHAR                   "z"
#define TOP_BOTTOM_SCOPE            2

#define WORKAREA_CLASS              "SR_WORKAREA"
#define HASH_TABLE_SIZE             1000

#define CONNECT_ODBC                   1
#define CONNECT_RPC                    2
#define CONNECT_MYSQL                  3
#define CONNECT_POSTGRES               4
#define CONNECT_ORACLE                 5
#define CONNECT_FIREBIRD               6

#define CONNECT_QUERY_ONLY           100
#define CONNECT_NOEXLOCK              50

#define CONNECT_ODBC_QUERY_ONLY        1 + CONNECT_QUERY_ONLY
#define CONNECT_RPC_QUERY_ONLY         2 + CONNECT_QUERY_ONLY
#define CONNECT_MYSQL_QUERY_ONLY       3 + CONNECT_QUERY_ONLY
#define CONNECT_POSTGRES_QUERY_ONLY    4 + CONNECT_QUERY_ONLY
#define CONNECT_ORACLE_QUERY_ONLY      5 + CONNECT_QUERY_ONLY
#define CONNECT_FIREBIRD_QUERY_ONLY    6 + CONNECT_QUERY_ONLY

#define CONNECT_ODBC_NOEXLOCK          1 + CONNECT_NOEXLOCK
#define CONNECT_RPC_NOEXLOCK           2 + CONNECT_NOEXLOCK
#define CONNECT_MYSQL_NOEXLOCK         3 + CONNECT_NOEXLOCK
#define CONNECT_POSTGRES_NOEXLOCK      4 + CONNECT_NOEXLOCK
#define CONNECT_ORACLE_NOEXLOCK        5 + CONNECT_NOEXLOCK
#define CONNECT_FIREBIRD_NOEXLOCK      6 + CONNECT_NOEXLOCK

#define CONNECT_CUSTOM               999

#define AINFO_BOF                      1
#define AINFO_EOF                      2
#define AINFO_FOUND                    3
#define AINFO_RECNO                    4
#define AINFO_FCOUNT                   5
#define AINFO_RCOUNT                   6
#define AINFO_HOT                      7
#define AINFO_DELETED                  8
#define AINFO_HNRECNO                  9
#define AINFO_HNDELETED               10
#define AINFO_SHARED                  11
#define AINFO_ISINSERT                12
#define AINFO_BOF_AT                  13
#define AINFO_EOF_AT                  14
#define AINFO_BUFFER_VALID            15
#define AINFO_INDEXORD                16
#define AINFO_REVERSE_INDEX           17

#define AORDER_NAME                    1
#define AORDER_FOR                     2
#define AORDER_TYPE                    3
#define AORDER_UNIQUE                  4

#define CACHEINFO_LEN                 13
#define CACHEINFO_TABINFO              1
#define CACHEINFO_TABNAME              2
#define CACHEINFO_CONNECT              3
#define CACHEINFO_INDEX                4
#define CACHEINFO_AFIELDS              5
#define CACHEINFO_ANAMES               6
#define CACHEINFO_ABLANK               7
#define CACHEINFO_HNRECNO              8
#define CACHEINFO_HNDELETED            9
#define CACHEINFO_INIFIELDS           10
#define CACHEINFO_HNPOSDTHIST         11
#define CACHEINFO_HNCOLPK             12
#define CACHEINFO_ANAMES_LOWER        13


#define ORDER_TYPE_ASCEND              1
#define ORDER_TYPE_DESASCEND           0

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

/* supported RDBMS */

#define SUPPORTED_DATABASES           15

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

/* cache flags */

#define ORD_INVALID                    0
#define ORD_DIR_FWD                    1
#define ORD_DIR_BWD                    2
#define ORD_NATURAL                    3

#define SQL_NULL_HSTMT                 0

/* aIndex array structure */

#define ORDER_ASCEND                   1
#define ORDER_DESEND                   2
#define INDEX_FIELDS                   3
#define INDEX_KEY                      4
#define FOR_CLAUSE                     5
#define STR_DESCENDS                   6
#define SEEK_CODEBLOCK                 7
#define FOR_CODEBLOCK                  8
#define ORDER_TAG                      9
#define ORDER_NAME                    10
#define TOP_SCOPE                     11
#define BOTTOM_SCOPE                  12
#define SCOPE_SQLEXPR                 13
#define ORDER_SKIP_UP                 14
#define ORDER_SKIP_DOWN               15
#define SYNTH_INDEX_COL_POS           16
#define DESCEND_INDEX_ORDER           17

#define LASTREC_POS             99999998

/* Sequence per table */

#define SEQ_NOTDEFINED                         0
#define SEQ_PER_TABLE                          1
#define SEQ_PER_DATABASE                       2

/* Workarea Type */

#define WATYPE_UNDEF                   0
#define WATYPE_ISAM                    1
#define WATYPE_CACHE                   2

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

/* dbCreate Record Array Structure */

#define FIELD_INFO_SIZE                11

#define FIELD_NAME                      1
#define FIELD_TYPE                      2
#define FIELD_LEN                       3
#define FIELD_DEC                       4
#define FIELD_NULLABLE                  5
#define FIELD_DOMAIN                    6  /* Not used by dbCreate */
#define FIELD_MULTILANG                 7
#define FIELD_SQL_RESERVED1             8  /* Not used by dbCreate */
#define FIELD_SQL_RESERVED2             9  /* Not used by dbCreate */
#define FIELD_PRIMARY_KEY              10
#define FIELD_UNIQUE                   11

#define MULTILANG_FIELD_ON             .T.
#define MULTILANG_FIELD_OFF            .F.

#define ARRAY_BLOCK1      1
#define ARRAY_BLOCK2      10
#define ARRAY_BLOCK3      50
#define ARRAY_BLOCK4      100
#define ARRAY_BLOCK5      500

#ifndef SQLRDD_H

#define IS_SQLRDD   (RddName()=="SQLRDD")
#define LOCK_NOWAIT     if(::oSql:nSystemID==SYSTEMID_ORACLE," NOWAIT","")
#define SQL_SERIALIZED_SIGNATURE     "!#SERIAL#!"

#endif
#endif
