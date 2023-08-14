/*
* SQLRDD PRG Header (Oracle DEFINES)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#define OCI_SUCCESS 0                      /* maps to SQL_SUCCESS of SAG CLI */
#define OCI_SUCCESS_WITH_INFO 1             /* maps to SQL_SUCCESS_WITH_INFO */
#define OCI_RESERVED_FOR_INT_USE 200                            /* reserved */
#define OCI_NO_DATA 100                               /* maps to SQL_NO_DATA */
#define OCI_ERROR -1                                    /* maps to SQL_ERROR */
#define OCI_INVALID_HANDLE -2                  /* maps to SQL_INVALID_HANDLE */
#define OCI_NEED_DATA 99                            /* maps to SQL_NEED_DATA */
#define OCI_STILL_EXECUTING -3123                   /* OCI would block error */
#define OCI_CONTINUE -24200    /* Continue with the body of the OCI function */

#define SQL_ERROR                           OCI_ERROR
#define SQL_INVALID_HANDLE                  OCI_INVALID_HANDLE
#define SQL_NEED_DATA                       OCI_NEED_DATA
#define SQL_NO_DATA_FOUND                   OCI_NO_DATA
#define SQL_SUCCESS                         OCI_SUCCESS
#define SQL_SUCCESS_WITH_INFO               OCI_SUCCESS_WITH_INFO
#define SQL_DROP                            OCI_SUCCESS_WITH_INFO



#define SQL_AUTOCOMMIT_OFF		               0
#define SQL_AUTOCOMMIT_ON		               1
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
#define SQL_NVARCHAR 		                 -9
#define SQL_DB2_CLOB                        -99
#define SQL_FAKE_LOB                        -100

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

#define SQL_C_DEFAULT 			   99

/* SQLExtendedFetch "fFetchType" values */

#define SQL_FETCH_BOOKMARK			8

/* SQLExtendedFetch "rgfRowStatus" element values */

#define SQL_ROW_SUCCESS 			0
#define SQL_ROW_DELETED 			1
#define SQL_ROW_UPDATED 			2
#define SQL_ROW_NOROW				3
#define SQL_ROW_ADDED				4
#define SQL_ROW_ERROR				5

/* SQL_CONCURRENCY options */

#define SQL_CONCUR_READ_ONLY		1
#define SQL_CONCUR_DEFAULT			SQL_CONCUR_READ_ONLY	/* Default value */

/* SQL_CURSOR_TYPE options */

#define SQL_CURSOR_TYPE_DEFAULT		SQL_CURSOR_FORWARD_ONLY	/* Default value */

/* options for SQLGetStmtOption/SQLSetStmtOption */

#define SQL_KEYSET_SIZE 			8
#define SQL_SIMULATE_CURSOR 		10
#define SQL_RETRIEVE_DATA			11
#define SQL_USE_BOOKMARKS			12
#define SQL_GET_BOOKMARK			13	/*	GetStmtOption Only */
#define SQL_STMT_OPT_MAX			SQL_ROW_NUMBER
#define SQL_STMT_OPT_MIN			SQL_QUERY_TIMEOUT

/* Operations in SQLSetPos */

#define SQL_UPDATE					2
#define SQL_DELETE					3
#define SQL_ADD						4

/* Lock options in SQLSetPos */

#define SQL_LOCK_UNLOCK 			2
#define SQL_C_DOUBLE				   8

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

#define SQL_ATTR_QUERY_TIMEOUT				SQL_QUERY_TIMEOUT

#define SQLT_CHR  1                        /* (ORANET TYPE) character string */
#define SQLT_NUM  2                          /* (ORANET TYPE) oracle numeric */
#define SQLT_INT  3                                 /* (ORANET TYPE) integer */
#define SQLT_FLT  4                   /* (ORANET TYPE) Floating point number */
#define SQLT_STR  5                                /* zero terminated string */
#define SQLT_VNU  6                        /* NUM with preceding length byte */
#define SQLT_PDN  7                  /* (ORANET TYPE) Packed Decimal Numeric */
#define SQLT_LNG  8                                                  /* long */
#define SQLT_VCS  9                             /* Variable character string */
#define SQLT_NON  10                      /* Null/empty PCC Descriptor entry */
#define SQLT_RID  11                                                /* rowid */
#define SQLT_DAT  12                                /* date in oracle format */
#define SQLT_VBI  15                                 /* binary in VCS format */
#define SQLT_BIN  23                                  /* binary data(DTYBIN) */
#define SQLT_LBI  24                                          /* long binary */
#define SQLT_UIN  68                                     /* unsigned integer */
#define SQLT_SLS  91                        /* Display sign leading separate */
#define SQLT_LVC  94                                  /* Longer longs (char) */
#define SQLT_LVB  95                                  /* Longer long binary */
#define SQLT_AFC  96                                      /* Ansi fixed char */
#define SQLT_AVC  97                                        /* Ansi Var char */
#define SQLT_CUR  102                                        /* cursor  type */
#define SQLT_RDD  104                                    /* rowid descriptor */
#define SQLT_LAB  105                                          /* label type */
#define SQLT_OSL  106                                        /* oslabel type */

#define SQLT_NTY  108                                   /* named object type */
#define SQLT_REF  110                                            /* ref type */
#define SQLT_CLOB 112                                       /* character lob */
#define SQLT_BLOB 113                                          /* binary lob */
#define SQLT_BFILEE 114                                    /* binary file lob */
#define SQLT_CFILEE 115                                 /* character file lob */
#define SQLT_RSET 116                                     /* result set type */
#define SQLT_NCO  122      /* named collection type (varray or nested table) */
#define SQLT_VST  155                                      /* OCIString type */
#define SQLT_ODT  156                                        /* OCIDate type */

/* datetimes and intervals */
#define SQLT_DATE                      184                      /* ANSI Date */
#define SQLT_TIME                      185                           /* TIME */
#define SQLT_TIME_TZ                   186            /* TIME WITH TIME ZONE */
#define SQLT_TIMESTAMP                 187                      /* TIMESTAMP */
#define SQLT_TIMESTAMP_TZ              188       /* TIMESTAMP WITH TIME ZONE */
#define SQLT_INTERVAL_YM               189         /* INTERVAL YEAR TO MONTH */
#define SQLT_INTERVAL_DS               190         /* INTERVAL DAY TO SECOND */
#define SQLT_TIMESTAMP_LTZ             232        /* TIMESTAMP WITH LOCAL TZ */

#define SQLT_PNTY   241              /* pl/sql representation of named types */
