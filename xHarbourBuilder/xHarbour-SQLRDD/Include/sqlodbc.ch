/*
* SQLRDD PRG Header (ODBC DEFINES)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#define SQL_ERRCODES

#define SQL_ERROR                           -1
#define SQL_INVALID_HANDLE                  -2
#define SQL_NEED_DATA                       99
#define SQL_NO_DATA_FOUND                  100
#define SQL_SUCCESS                          0
#define SQL_SUCCESS_WITH_INFO                1
#define SQL_DROP                             1

#define SQL_AUTOCOMMIT_OFF                   0
#define SQL_AUTOCOMMIT_ON                    1
#define SQL_AUTOCOMMIT                     102

#define SQL_CHAR                             1
#define SQL_NUMERIC                          2
#define SQL_DECIMAL                          3
#define SQL_INTEGER                          4
#define SQL_SMALLINT                         5
#define SQL_FLOAT                            6
#define SQL_REAL                             7
#define SQL_DOUBLE                           8
#define SQL_DATE                             9
#define SQL_TIME                            10
#define SQL_TIMESTAMP                       11
#define SQL_VARCHAR                         12
#define SQL_LONGVARCHAR                     -1
#define SQL_BINARY                          -2
#define SQL_VARBINARY                       -3
#define SQL_LONGVARBINARY                   -4
#define SQL_BIGINT                          -5
#define SQL_TINYINT                         -6
#define SQL_BIT                             -7
#define SQL_NVARCHAR                        -9
#define SQL_DB2_CLOB                        -99
#define SQL_FAKE_LOB                        -100
#define SQL_FAKE_DATE                       -101
#define SQL_FAKE_NUM                        -102
#define SQL_GUID                            -11

#define SQL_WCHAR		 	                    -8
#define SQL_WVARCHAR	 	                    SQL_NVARCHAR
#define SQL_WLONGVARCHAR 	                 -10
#define SQL_C_WCHAR			                 SQL_WCHAR

#ifdef UNICODE
#define SQL_C_TCHAR		SQL_C_WCHAR
#else
#define SQL_C_TCHAR		SQL_C_CHAR
#endif

#define SQLNAME                              1
#define SQLCTYPE                             2
#define SQLLEN                               3
#define SQLDEC                               4
#define SQLNULL                              5
#define SQLNTYPE                             6

#define SQL_INFO_FIRST                       0
#define SQL_ACTIVE_CONNECTIONS               0
#define SQL_ACTIVE_STATEMENTS                1
#define SQL_DATA_SOURCE_NAME                 2
#define SQL_DRIVER_HDBC                      3
#define SQL_DRIVER_HENV                      4
#define SQL_DRIVER_HSTMT                     5
#define SQL_DRIVER_NAME                      6
#define SQL_DRIVER_VER                       7
#define SQL_FETCH_DIRECTION                  8
#define SQL_ODBC_API_CONFORMANCE             9
#define SQL_ODBC_VER                        10
#define SQL_ROW_UPDATES                     11
#define SQL_ODBC_SAG_CLI_CONFORMANCE        12
#define SQL_SERVER_NAME                     13
#define SQL_SEARCH_PATTERN_ESCAPE           14
#define SQL_ODBC_SQL_CONFORMANCE            15

#define SQL_DATABASE_NAME                   16
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

#define SQL_ACCESSIBLE_TABLES               19
#define SQL_ACCESSIBLE_PROCEDURES           20
#define SQL_PROCEDURES                      21
#define SQL_CONCAT_NULL_BEHAVIOR            22
#define SQL_CURSOR_COMMIT_BEHAVIOR          23
#define SQL_CURSOR_ROLLBACK_BEHAVIOR        24
#define SQL_DATA_SOURCE_READ_ONLY           25
#define SQL_DEFAULT_TXN_ISOLATION           26
#define SQL_EXPRESSIONS_IN_ORDERBY          27
#define SQL_IDENTIFIER_CASE                 28
#define SQL_IDENTIFIER_QUOTE_CHAR           29
#define SQL_MAX_COLUMN_NAME_LEN             30
#define SQL_MAX_CURSOR_NAME_LEN             31
#define SQL_MAX_OWNER_NAME_LEN              32
#define SQL_MAX_PROCEDURE_NAME_LEN          33
#define SQL_MAX_QUALIFIER_NAME_LEN          34
#define SQL_MAX_TABLE_NAME_LEN              35
#define SQL_MULT_RESULT_SETS                36
#define SQL_MULTIPLE_ACTIVE_TXN             37
#define SQL_OUTER_JOINS                     38
#define SQL_OWNER_TERM                      39
#define SQL_PROCEDURE_TERM                  40
#define SQL_QUALIFIER_NAME_SEPARATOR        41
#define SQL_QUALIFIER_TERM                  42
#define SQL_SCROLL_CONCURRENCY              43
#define SQL_SCROLL_OPTIONS                  44
#define SQL_TABLE_TERM                      45
#define SQL_TXN_CAPABLE                     46
#define SQL_USER_NAME                       47

#define SQL_CONVERT_FUNCTIONS               48
#define SQL_NUMERIC_FUNCTIONS               49
#define SQL_STRING_FUNCTIONS                50
#define SQL_SYSTEM_FUNCTIONS                51
#define SQL_TIMEDATE_FUNCTIONS              52

#define SQL_CONVERT_BIGINT                  53
#define SQL_CONVERT_BINARY                  54
#define SQL_CONVERT_BIT                     55
#define SQL_CONVERT_CHAR                    56
#define SQL_CONVERT_DATE                    57
#define SQL_CONVERT_DECIMAL                 58
#define SQL_CONVERT_DOUBLE                  59
#define SQL_CONVERT_FLOAT                   60
#define SQL_CONVERT_INTEGER                 61
#define SQL_CONVERT_LONGVARCHAR             62
#define SQL_CONVERT_NUMERIC                 63
#define SQL_CONVERT_REAL                    64
#define SQL_CONVERT_SMALLINT                65
#define SQL_CONVERT_TIME                    66
#define SQL_CONVERT_TIMESTAMP               67
#define SQL_CONVERT_TINYINT                 68
#define SQL_CONVERT_VARBINARY               69
#define SQL_CONVERT_VARCHAR                 70
#define SQL_CONVERT_LONGVARBINARY           71

#define SQL_TXN_ISOLATION_OPTION            72
#define SQL_ODBC_SQL_OPT_IEF                73

#define CONN_OPT_MIN            SQL_ACCESS_MODE
#define CONN_OPT_MAX            SQL_ODBC_CURSORS

#define SQL_QUERY_TIMEOUT       0
#define SQL_MAX_ROWS            1
#define SQL_NOSCAN              2
#define SQL_MAX_LENGTH          3
#define SQL_ASYNC_ENABLE        4
#define SQL_BIND_TYPE           5

#define SQL_C_DEFAULT             99

/* SQLExtendedFetch "fFetchType" values */

#define SQL_FETCH_BOOKMARK         8

/* SQLExtendedFetch "rgfRowStatus" element values */

#define SQL_ROW_SUCCESS          0
#define SQL_ROW_DELETED          1
#define SQL_ROW_UPDATED          2
#define SQL_ROW_NOROW            3
#define SQL_ROW_ADDED            4
#define SQL_ROW_ERROR            5

/* SQL_CONCURRENCY options */

#define SQL_CONCUR_READ_ONLY      1
#define SQL_CONCUR_DEFAULT         SQL_CONCUR_READ_ONLY   /* Default value */

/* SQL_CURSOR_TYPE options */

#define SQL_CURSOR_TYPE_DEFAULT      SQL_CURSOR_FORWARD_ONLY   /* Default value */

/* options for SQLGetStmtOption/SQLSetStmtOption */

#define SQL_KEYSET_SIZE          8
#define SQL_SIMULATE_CURSOR       10
#define SQL_RETRIEVE_DATA         11
#define SQL_USE_BOOKMARKS         12
#define SQL_GET_BOOKMARK         13   /*   GetStmtOption Only */
#define SQL_STMT_OPT_MAX         SQL_ROW_NUMBER
#define SQL_STMT_OPT_MIN         SQL_QUERY_TIMEOUT

/* Operations in SQLSetPos */

#define SQL_UPDATE               2
#define SQL_DELETE               3
#define SQL_ADD                  4

/* Lock options in SQLSetPos */

#define SQL_LOCK_UNLOCK          2
#define SQL_C_DOUBLE               8

/* identifiers of fields in the SQL descriptor */

#define SQL_DESC_COUNT                  1001
#define SQL_DESC_TYPE                   1002
#define SQL_DESC_LENGTH                 1003
#define SQL_DESC_OCTET_LENGTH_PTR       1004
#define SQL_DESC_PRECISION              1005
#define SQL_DESC_SCALE                  1006
#define SQL_DESC_DATETIME_INTERVAL_CODE 1007
#define SQL_DESC_NULLABLE               1008
#define SQL_DESC_INDICATOR_PTR          1009
#define SQL_DESC_DATA_PTR               1010
#define SQL_DESC_NAME                   1011
#define SQL_DESC_UNNAMED                1012
#define SQL_DESC_OCTET_LENGTH           1013
#define SQL_DESC_ALLOC_TYPE             1099

#define SQL_ATTR_QUERY_TIMEOUT            SQL_QUERY_TIMEOUT

#define SQL_ATTR_CURRENT_CATALOG         109
