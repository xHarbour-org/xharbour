/*
* ODBCRDD C Header
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef SQLEXORA_H

#define SQLEXORA_H
#include "sqlorastru.h"
#include "hbsetup.h"
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "sqlrdd.ch"
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

#define SQL_TYPE_DATE                       91
#define SQL_TYPE_TIME                       92
#define SQL_TYPE_TIMESTAMP                  93
// #define SQL_BINARY  (-2)
// #define SQL_VARBINARY  (-3)
// #define SQL_LONGVARBINARY  (-4)
// #define SQL_BIGINT  (-5)
// #define SQL_TINYINT  (-6)
// #define SQL_BIT  (-7)
#if !defined (SQL_DATETIME)
#define SQL_DATETIME                        -40
#endif
#define SQL_VARCHAR                         12
#define SQL_NVARCHAR                        -9
#define SQL_C_WCHAR			                 SQL_WCHAR
#define SQL_C_CHAR  SQL_CHAR
#define SQL_C_LONG  SQL_INTEGER
#define SQL_C_SHORT  SQL_SMALLINT
#define SQL_C_FLOAT  SQL_REAL
// #define SQL_C_DOUBLE  SQL_DOUBLE
#define SQL_C_DATE  SQL_DATE
#define SQL_C_TIME  SQL_TIME
#define SQL_C_TIMESTAMP  SQL_TIMESTAMP
#define SQL_C_TYPE_DATE  SQL_TYPE_DATE
#define SQL_C_TYPE_TIME  SQL_TYPE_TIME
#define SQL_C_TYPE_TIMESTAMP  SQL_TYPE_TIMESTAMP
#define SQL_C_NUMERIC  SQL_NUMERIC
#define SQL_C_BIT  SQL_BIT
#define SQL_C_BINARY  SQL_BINARY
#ifdef UNICODE
#define SQL_C_TCHAR		SQL_C_WCHAR
#else
#define SQL_C_TCHAR		SQL_C_CHAR
#endif
#define SQL_NULL_DATA                   (-1)
#define SQL_NTS                         (-3)
#define SUPERTABLE                           ( &sqlExOraSuper )

#define MAX_INDEXES                           51  // 50 + Natural Order
#define MAX_INDEX_COLS                        10  // Seek will work on indexes with up to MAX_INDEX_COLS columns
#define PREPARED_SQL_LEN                     400
#define RECORD_LIST_SIZE                     250
#define COLUMN_BLOCK_SIZE                   8192
#define FIELD_LIST_SIZE                     6000
#define MAX_SQL_QUERY_LEN                  32000
#define PAGE_READ_SIZE                        50
#define BUFFER_POOL_SIZE                     250
#define DEFAULT_INDEX_COLUMN_MAX_LEN         200
#define INITIAL_MEMO_ALLOC                  128

//                           0 1 2 34 5 6 7 8 9 0 1 23 4 5 67 8 9
static char * openQuotes  = "\"\"\"[\"\"\"\"\"\"\"\"`\"\"\"`\"\"\"";
static char * closeQuotes = "\"\"\"]\"\"\"\"\"\"\"\"`\"\"\"`\"\"\"";

#define CHECK_SQL_N_OK( res )    ((res != SQL_SUCCESS) && (res != SQL_SUCCESS_WITH_INFO))
#define OPEN_QUALIFIER( thiswa ) ( openQuotes[thiswa->nSystemID] )
#define CLOSE_QUALIFIER( thiswa ) ( closeQuotes[thiswa->nSystemID] )
#define RESULTSET_OK      HB_SUCCESS + 10
#define RESULTSET_NOK     HB_SUCCESS + 11

#undef HB_TRACE
#define HB_TRACE(x, y)

#define SQL_NVARCHAR                        -9
#define SQL_DB2_CLOB                        -99
#define SQL_FAKE_LOB                        -100
#define SQL_FAKE_DATE                       -101
#define SQL_FAKE_NUM                        -102

/*
*  ODBC Column binding. I know that some information in structure below does exist in
*  other parts... I mean, it's duplicated, but I prefer to waste a few bytes more and have
*  things at hand, making this RDD faster
*/


typedef struct _STATEMENT_DATA
{
   OCI_Statement * pStmt;
} STATEMENT_DATA;

typedef struct _SQL_CHAR_STRUCT
{
   char * value;
   int size;
   int size_alloc;
} SQL_CHAR_STRUCT;

typedef struct tagDATE_STRUCTORA {
    unsigned int year;
    unsigned int month;
    unsigned int day;
} DATE_STRUCTORA,SQL_DATE_STRUCTORA;

typedef struct tagTIME_STRUCTORA {
    unsigned int hour;
    unsigned int minute;
    unsigned int second;
} TIME_STRUCTORA; 

typedef struct tagTIMESTAMP_STRUCTORA {
    unsigned int  year;
    unsigned int  month;
    unsigned int  day;
    unsigned int  hour;
    unsigned int  minute;
    unsigned int  second;
    unsigned int fraction;
} TIMESTAMP_STRUCORAT,SQL_TIMESTAMP_STRUCTORA;

//typedef DATE_STRUCTORA SQL_DATE_STRUCTORA;
//typedef TIMESTAMP_STRUCTORA SQL_TIMESTAMP_STRUCTORA;

typedef struct _INDEXBINDORA
{
   HB_LONG lFieldPosDB;                   // Relative field position in aFields
   HB_LONG hIndexOrder;                   // Index order
   int  iLevel;                        // The current column in index
   int  iIndexColumns;                 // How many index columns in index
   OCI_Statement  * SkipFwdStmt;                  // Index stmt handle for SQL phrase in this level for FWD movment
   OCI_Statement  * SkipBwdStmt;                  // Index stmt handle for SQL phrase in this level for BWD movment
   OCI_Statement  * SeekFwdStmt;                  // Index stmt handle for SQL phrase in this level for FWD movment
   OCI_Statement  * SeekBwdStmt;                  // Index stmt handle for SQL phrase in this level for BWD movment
   char SkipFwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SkipBwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SeekFwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SeekBwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
} INDEXBINDORA;

typedef INDEXBINDORA * INDEXBINDORAP;

typedef struct _COLUMNBINDORA

{
   int  iParNum;                       // Parameter number in binded parameters
   int  iSQLType;                      // SQL data type of column
   int  iCType;                        // C data type of the argument. It determines
                                       // the as* member to be used, like xHB iTem API 'type'
   HB_LONG lFieldPosDB;                   // Relative field position in aFields
   HB_LONG lFieldPosWA;                   // Relative field position in aBuffer. NULL if RECNO or DELETED column
   char szBindName[50];
   char * colName;                     // Fully qualified column name to be used in queries
   SQL_CHAR_STRUCT      asChar;        // Support for char data types
   OCI_Date * asDate1;
   ULONGLONG             asLogical;     // Support for logical data type
   SQL_DATE_STRUCTORA      asDate;        // I suppose ODBC driver will suport default
                                       // convertion to TIMESTAMP when needed
   SQL_TIMESTAMP_STRUCTORA asTimestamp;   // Timestamp support, always converted to DATE
   OCI_Date * asDate2;
   ULONGLONG           asNumeric;
   double            asDouble;     // I suppose all ODBC drivers has build in
                                       // convertion from this type to all types
                                       // of numeric variables in SQL
   unsigned int	ColumnSize;             // To make an easy bind
   unsigned short	DecimalDigits;          // To make an easy bind
   BOOL isNullable;                    // Is this column NULLABLE ?
   BOOL isArgumentNull;                // Value to be bound is NULL ?
   BOOL isBoundNULL;                   // Field was bound as NULL ?
   int lIndPtr;                     // Buffer lenght pointer to be used in SQLBindParam()
   BOOL isMemo;                        // Field is MEMO ?
   BOOL isMultiLang;                   // Fiels is multi language ?
   OCI_Lob *lob1;

} COLUMNBINDORA;

typedef COLUMNBINDORA * COLUMNBINDORAP;

typedef struct _COLUMNSTATEMENT
{
   OCI_Statement * st;
} COLUMNSTATEMENT,* PCOLUMNSTATEMENT;  

/*
*  SQL WORKAREA
*/

typedef struct _SQLAREA
{
   AREA area;

   /*
   *  SQLRDD's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below
   */
   PHB_CODEPAGE cdPageCnv; /* Area's codepage convert pointer */
   char * szDataFileName;  /* file name */
   LONG hOrdCurrent;       /* current index order */
   BOOL shared;
   BOOL readonly;          /* only SELECT allowed */
   BOOL creating;          /* TRUE when creating table */
   BOOL firstinteract;     /* TRUE when workarea was not used yet */
   BOOL isam;              /* ISAM Simulator ? */
   BOOL wasdel;
   BOOL initialized;       /* Workarea Initialization done */
   BOOL sqlfilter;         /* SET FILTER converted to SQL */

   PHB_ITEM oWorkArea;      /* SQL Workarea object */
   PHB_ITEM aInfo;          /* Status array */
   PHB_ITEM aBuffer;        /* Record buffer */
   PHB_ITEM aOrders;        /* Indexes */
   PHB_ITEM aStruct;        /* Table xBase structure */
   PHB_ITEM aLocked;        /* Locked lines */
   PHB_ITEM aCreate;        /* Structure received by dbCreate() */
   PHB_ITEM aCache;         /* Workarea recordset cache */
   PHB_ITEM aOldBuffer;     /* Last workarea buffer */
   PHB_ITEM aEmptyBuff;     /* Empty buffer to be in eof()+1 */
   PHB_ITEM aSelectList;

   HB_ULONG ulhRecno;          /* Recno position in field list */
   HB_ULONG ulhDeleted;        /* Deleted position in field list */

   int * uiBufferIndex;     /* Field offset in fields array */
   int * uiFieldList;       /* Keeps a field list for SELECT statements */
   int iFieldListStatus;    /* field list status - see sqlprototypes.h */

   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */
   char editMask[MAX_FIELDS];    /* Flags if a column was updated - must be cleared on every GO_COLD - USED BY ODBCRDD */

} SQLAREA;

typedef SQLAREA * LPSQLAREA;

#ifndef SQLAREAP
#define SQLAREAP LPSQLAREA
#endif

typedef struct _SQLEXORAAREA
{
   SQLAREA sqlarea;
   
   /*
   *  SQLRDD's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below
   */
   ///PHB_CODEPAGE cdPageCnv; /* Area's codepage convert pointer */
   ///char * szDataFileName;  /* file name */
   ///LONG hOrdCurrent;       /* current index order */   
   //BOOL shared;
   //BOOL readonly;          /* only SELECT allowed */
   //BOOL creating;          /* TRUE when creating table */
   //BOOL firstinteract;     /* TRUE when workarea was not used yet */
   ///BOOL isam;              /* ISAM Simulator ? */
   ///BOOL wasdel;
   ///BOOL initialized;       /* Workarea Initialization done */
   ///BOOL sqlfilter;         /* SET FILTER converted to SQL */
   ///
   ///PHB_ITEM oWorkArea;      /* SQL Workarea object */
   ///PHB_ITEM aInfo;          /* Status array */
   ///PHB_ITEM aBuffer;        /* Record buffer */
   ///PHB_ITEM aOrders;        /* Indexes */
   ///PHB_ITEM aStruct;        /* Table xBase structure */
   ///PHB_ITEM aLocked;        /* Locked lines */
   ///PHB_ITEM aCreate;        /* Structure received by dbCreate() */
   ///PHB_ITEM aCache;         /* Workarea recordset cache */
   ///PHB_ITEM aOldBuffer;     /* Last workarea buffer */
   ///PHB_ITEM aEmptyBuff;     /* Empty buffer to be in eof()+1 */
   ///PHB_ITEM aSelectList;
   ///
   ///ULONG ulhRecno;          /* Recno position in field list */
   ///ULONG ulhDeleted;        /* Deleted position in field list */
   ///
   ///int * uiBufferIndex;     /* Field offset in fields array */
   ///int * uiFieldList;       /* Keeps a field list for SELECT statements */
   ///int iColumnListStatus;   /* field list status - see sqlprototypes.h */
   ///
   ///LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */

   /*
   *  SQLEX's additions to the SQLRDD workarea structure
   */

   PHB_ITEM oSql;             /* SQL connection object */
   PHB_ITEM aFields;          /* Table structure in DB */
   PHB_ITEM hBufferPool;      /* Hash containing the Buffer Pool */
   PHB_ITEM pIndexMgmnt;      // Existing Indexes in database catalog (SR_MGMNTINDEXES) array */

   OCI_Statement  * hStmt;               /* Statement handle */
   OCI_Statement  * hStmtBuffer;         /* Statement handle with prepared statement to retrieve line */
   OCI_Statement  * hStmtInsert;         /* Statement handle with prepared phrase to insert a new record */
   OCI_Statement  * hStmtNextval;        /* Statement handle with prepared phrase to get inserted record */
   OCI_Statement  * hStmtUpdate;         /* Statement handle with prepared phrase to insert a new record */
   OCI_Statement  * hStmtSkip;           /* Statement handle with prepared phrase to insert a new record */   
   OCI_ORASESSION  *hDbc;                /* Database connection handle */
   int nSystemID;             /* Connected database ID */
   ULONGLONG lCurrentRecord;      /* Should be filled by GetCurrentRecordNumOra() to be used in SKIP bindings */
   ULONGLONG lUpdatedRecord;      /* Should be filled by GetCurrentRecordNumOra() to be used in UPDATE bindings */
   ULONGLONG lBofAt;              /* BOF Record optimizer */
   ULONGLONG lEofAt;              /* EOF Record optimizer */
   ULONGLONG lLastRec;            /* LastRec() + 1 */
   ULONGLONG * lRecordToRetrieve; /* To be used with Binded Parameter */
   ULONGLONG * recordList;        /* record list to skip on */
   char * deletedList;        /* deleted list relative to record list */
   int recordListPos;         /* record list position */
   int recordListSize;        /* record list size */
   int recordListDirection;   /* LIST_FORWARD or LIST_BACKWARD */
   int iTCCompat;             /* Top Connect compatible */

   int indexColumns;          /* current index column list lenght */
   int indexLevel;            /* index column list level in current skip sequence */

   int skipDirection;         /* 1 - FWD, (-1) - BWD, 0 - none */

   char * sFields;            /* field list string */
   char * sSql;               /* Last SQL phrase */
   char * sSqlBuffer;         /* SQL Statement to get WA buffer */
   char * sTable;             /* Qualified table name */
   char * sOwner;             /* Database schema included in sTable */
   char * sWhere;             /* Where clause */
   char * sOrderBy;           /* Order By clause */
   char * sRecnoName;         /* Recno column name */
   char * sDeletedName;       /* Deleted column name */
   char sLimit1[50];          /* String for recordset limit */
   char sLimit2[50];          /* String for recordset limit */

   BOOL bufferHot;            /* Does it have to write buffer down to database ? */
   BOOL bIsInsert;            /* TRUE if appending a new record */
   BOOL bConnVerified;        /* Already checked for ODBC connection ? */
   BOOL bReverseIndex;        /* If current index is in DESCENDING order */
   //INDEXBINDP 
   //IndexBindings[MAX_INDEXES];   /* Index column and prepared SQL expression handles for SKIP */
   INDEXBINDORAP *
   IndexBindings;   /* Index column and prepared SQL expression handles for SKIP */
   
//    OCI_Statement  * colStmt;              /* Single column retrieving statements */
   STATEMENT_DATA   * colStmt;
   BOOL bConditionChanged1;      /* If any of conditions like filters, scope, historic, has
                                    changed, prepared statements handles for Record List
                                    are no longer valid - USED FOR SKIP / GO TOP / BO BOTTOM */
   BOOL bConditionChanged2;      /* If any of conditions like filters, scope, historic, has
                                    changed, prepared statements handles for Record List
                                    are no longer valid - USED FOR SEEK */
   BOOL bOrderChanged;           /* If order has changed, we should fix column bindings
                                    before use then */
   BOOL bRebuildSeekQuery;       /* If query for Seek must be recreated due to NULL interference */
   BOOL bHistoric;               /* TRUE if workarea has historic */
   COLUMNBINDORAP InsertRecord;     /* Column bindings to INSERT */
   COLUMNBINDORAP CurrRecord;       /* Current record bindings for SKIP / UPDATE */
   char editMask[MAX_FIELDS];    /* Flags if a column was updated - must be cleared on every GO_COLD */
   char updatedMask[MAX_FIELDS]; /* Copy of updateMask in currently prepared UPDATE stmt */
   char specialMask[MAX_FIELDS]; /* Same of updateMask but for special cols (INDKEY_xx and FORKEY_xx) */
   BOOL bIndexTouchedInUpdate;   /* If any index column is affected by UPDATE */
   BOOL bIsSelect;               /* Table open is an select statement*/
   BOOL bOracle12;
} SQLEXORAAREA;

typedef SQLEXORAAREA * LPSQLEXORAAREA;

#ifndef SQLEXORAAREAP
#define SQLEXORAAREAP LPSQLEXORAAREA
#endif

		

/* prototypes */

int sqlKeyCompare( AREAP thiswa, PHB_ITEM pKey, BOOL fExact );
void odbcErrorDiag( OCI_Statement  * hStmt, char * routine, char * szSql, int line );
// void odbcErrorDiagRTE( OCI_Statement  * hStmt, char * routine, char * szSql, int res, int line, char * module );
void OraErrorDiagRTE( OCI_Statement *hStmt, char * routine, char * szSql, int res, int line, char * module );
void odbcFieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, LONG lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate );
char * QuoteTrimEscapeString( char * FromBuffer, ULONG iSize, int idatabase, BOOL bRTrim, ULONG * iSizeOut );
char * quotedNull( PHB_ITEM pFieldData, PHB_ITEM pFieldLen, PHB_ITEM pFieldDec, BOOL bNullable, int nSystemID, BOOL bTCCompat, BOOL bMemo, BOOL * bNullArgument );
BOOL SR_itemEmpty2( PHB_ITEM pItem );
void commonError( AREAP ThisDb, USHORT uiGenCode, USHORT uiSubCode, char* filename );
HB_ERRCODE SetBindEmptylValue2( COLUMNBINDORAP BindStructure );
HB_ERRCODE SetBindValue2( PHB_ITEM pFieldData, COLUMNBINDORAP BindStructure, OCI_Statement  * hStmt );
char * QualifyName2( char * szName, SQLEXORAAREAP thiswa );
COLUMNBINDORAP GetBindStructOra( SQLEXORAAREAP thiswa, INDEXBINDORAP IndexBind );
BOOL getColumnListOra( SQLEXORAAREAP thiswa );
void SolveFiltersOra( SQLEXORAAREAP thiswa, BOOL bWhere );
void getOrderByExpressionOra( SQLEXORAAREAP thiswa, BOOL bUseOptimizerHints );
void setResultSetLimitOra( SQLEXORAAREAP thiswa, int iRows );
void SetIndexBindStructureOra( SQLEXORAAREAP thiswa );
void SetInsertRecordStructureOra( SQLEXORAAREAP thiswa );
ULONGLONG GetCurrentRecordNumOra( SQLEXORAAREAP thiswa );

/* INSERT and UPDATE prototypes */

void CreateInsertStmtOra( SQLEXORAAREAP thiswa );
void SetCurrRecordStructureOra( SQLEXORAAREAP thiswa );
HB_ERRCODE CreateUpdateStmtOra( SQLEXORAAREAP thiswa );
HB_ERRCODE PrepareInsertStmtOra( SQLEXORAAREAP thiswa );
HB_ERRCODE BindInsertColumnsOra( SQLEXORAAREAP thiswa );
HB_ERRCODE FeedRecordColsOra( SQLEXORAAREAP thiswa, BOOL bUpdate );
HB_ERRCODE ExecuteInsertStmtOra( SQLEXORAAREAP thiswa );
HB_ERRCODE ExecuteUpdateStmtOra( SQLEXORAAREAP thiswa );

/* SEEK Prototypes */

HB_ERRCODE FeedSeekKeyToBindingsOra( SQLEXORAAREAP thiswa, PHB_ITEM pKey, int * queryLevel );
BOOL CreateSeekStmtora( SQLEXORAAREAP thiswa, int queryLevel );
void BindSeekStmtora( SQLEXORAAREAP thiswa, int queryLevel );
HB_ERRCODE getPreparedSeekora( SQLEXORAAREAP thiswa, int queryLevel, USHORT * iIndex, OCI_Statement  * * hStmt ,OCI_Resultset ** rs);
OCI_Connection * GetConnection( OCI_ORASESSION *  p );
void SQLO_FieldGet( PHB_ITEM pField, PHB_ITEM pItem, int iField, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,OCI_Resultset * rs );
#endif
