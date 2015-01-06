/*
* ODBCRDD C Header
* Copyright (c) 2008 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef ODBCRDD_H

#define ODBCRDD_H

#include "hbsetup.h"
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "sqlrdd.ch"

#define SUPERTABLE                           ( &sqlExSuper )

#define MAX_INDEXES                           51  // 50 + Natural Order
#define MAX_INDEX_COLS                        10  // Seek will work on indexes with up to MAX_INDEX_COLS columns
#define PREPARED_SQL_LEN                     400
#define RECORD_LIST_SIZE                     250
#define COLUMN_BLOCK_SIZE                     64
#define FIELD_LIST_SIZE                     6000
#define FIELD_LIST_SIZE_PARAM                600
#define MAX_SQL_QUERY_LEN                  32000
#define PAGE_READ_SIZE                        50
#define BUFFER_POOL_SIZE                     250
#define DEFAULT_INDEX_COLUMN_MAX_LEN         200
#define INITIAL_MEMO_ALLOC                   256

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

typedef struct _SQL_CHAR_STRUCT
{
   SQLCHAR * value;
   int size;
   int size_alloc;
} SQL_CHAR_STRUCT;

typedef struct _INDEXBIND
{
   LONG lFieldPosDB;                   // Relative field position in aFields
   LONG hIndexOrder;                   // Index order
   int  iLevel;                        // The current column in index
   int  iIndexColumns;                 // How many index columns in index
   HSTMT SkipFwdStmt;                  // Index stmt handle for SQL phrase in this level for FWD movment
   HSTMT SkipBwdStmt;                  // Index stmt handle for SQL phrase in this level for BWD movment
   HSTMT SeekFwdStmt;                  // Index stmt handle for SQL phrase in this level for FWD movment
   HSTMT SeekBwdStmt;                  // Index stmt handle for SQL phrase in this level for BWD movment
   char SkipFwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SkipBwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SeekFwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
   char SeekBwdSql[PREPARED_SQL_LEN];  // Partial prepared query for debugging pourposes
} INDEXBIND;

typedef INDEXBIND * INDEXBINDP;

typedef struct _COLUMNBIND

{
   int  iParNum;                       // Parameter number in binded parameters
   int  iSQLType;                      // SQL data type of column
   int  iCType;                        // C data type of the argument. It determines
                                       // the as* member to be used, like xHB iTem API 'type'
   LONG lFieldPosDB;                   // Relative field position in aFields
   LONG lFieldPosWA;                   // Relative field position in aBuffer. NULL if RECNO or DELETED column
   char * colName;                     // Fully qualified column name to be used in queries
   SQL_CHAR_STRUCT      asChar;        // Support for char data types
   SQLCHAR              asLogical;     // Support for logical data type
   SQL_DATE_STRUCT      asDate;        // I suppose ODBC driver will suport default
                                       // convertion to TIMESTAMP when needed
   SQL_TIMESTAMP_STRUCT asTimestamp;   // Timestamp support, always converted to DATE
   SQLDOUBLE            asNumeric;     // I suppose all ODBC drivers has build in
                                       // convertion from this type to all types
                                       // of numeric variables in SQL
   SQLUINTEGER	ColumnSize;             // To make an easy bind
   SQLSMALLINT	DecimalDigits;          // To make an easy bind
   BOOL isNullable;                    // Is this column NULLABLE ?
   BOOL isArgumentNull;                // Value to be bound is NULL ?
   BOOL isBoundNULL;                   // Field was bound as NULL ?
   SQLLEN lIndPtr;                     // Buffer lenght pointer to be used in SQLBindParam()
   BOOL isMemo;                        // Field is MEMO ?
   BOOL isMultiLang;                   // Fiels is multi language ?

} COLUMNBIND;

typedef COLUMNBIND * COLUMNBINDP;

/*
*  SQL WORKAREA
*/

typedef struct _SQLEXAREA
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

   ULONG ulhRecno;          /* Recno position in field list */
   ULONG ulhDeleted;        /* Deleted position in field list */

   int * uiBufferIndex;     /* Field offset in fields array */
   int * uiFieldList;       /* Keeps a field list for SELECT statements */
   int iColumnListStatus;   /* field list status - see sqlprototypes.h */

   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */

   /*
   *  SQLEX's additions to the SQLRDD workarea structure
   */

   PHB_ITEM oSql;             /* SQL connection object */
   PHB_ITEM aFields;          /* Table structure in DB */
   PHB_ITEM hBufferPool;      /* Hash containing the Buffer Pool */
   PHB_ITEM pIndexMgmnt;      // Existing Indexes in database catalog (SR_MGMNTINDEXES) array */

   HSTMT hStmt;               /* Statement handle */
   HSTMT hStmtBuffer;         /* Statement handle with prepared statement to retrieve line */
   HSTMT hStmtInsert;         /* Statement handle with prepared phrase to insert a new record */
   HSTMT hStmtNextval;        /* Statement handle with prepared phrase to get inserted record */
   HSTMT hStmtUpdate;         /* Statement handle with prepared phrase to insert a new record */
   HDBC  hDbc;                /* Database connection handle */
   int nSystemID;             /* Connected database ID */
   ULONG lCurrentRecord;      /* Should be filled by GetCurrentRecordNum() to be used in SKIP bindings */
   ULONG lUpdatedRecord;      /* Should be filled by GetCurrentRecordNum() to be used in UPDATE bindings */
   ULONG lBofAt;              /* BOF Record optimizer */
   ULONG lEofAt;              /* EOF Record optimizer */
   ULONG lLastRec;            /* LastRec() + 1 */
   ULONG * lRecordToRetrieve; /* To be used with Binded Parameter */
   ULONG * recordList;        /* record list to skip on */
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
   INDEXBINDP *
   IndexBindings;   /* Index column and prepared SQL expression handles for SKIP */
   
   HSTMT * colStmt;              /* Single column retrieving statements */
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
   COLUMNBINDP InsertRecord;     /* Column bindings to INSERT */
   COLUMNBINDP CurrRecord;       /* Current record bindings for SKIP / UPDATE */
   char editMask[MAX_FIELDS];    /* Flags if a column was updated - must be cleared on every GO_COLD */
   char updatedMask[MAX_FIELDS]; /* Copy of updateMask in currently prepared UPDATE stmt */
   char specialMask[MAX_FIELDS]; /* Same of updateMask but for special cols (INDKEY_xx and FORKEY_xx) */
   BOOL bIndexTouchedInUpdate;   /* If any index column is affected by UPDATE */
   BOOL bIsSelect;               /* Table open is an select statement*/

} SQLEXAREA;

typedef SQLEXAREA * LPSQLEXAREA;

#ifndef SQLEXAREAP
#define SQLEXAREAP LPSQLEXAREA
#endif

/* prototypes */

int sqlKeyCompare( AREAP thiswa, PHB_ITEM pKey, BOOL fExact );
void odbcErrorDiag( HSTMT hStmt, char * routine, char * szSql, int line );
void odbcErrorDiagRTE( HSTMT hStmt, char * routine, char * szSql, SQLRETURN res, int line, char * module );
void odbcFieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, HB_ISIZ lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate );
char * QuoteTrimEscapeString( char * FromBuffer, ULONG iSize, int idatabase, BOOL bRTrim, ULONG * iSizeOut );
char * quotedNull( PHB_ITEM pFieldData, PHB_ITEM pFieldLen, PHB_ITEM pFieldDec, BOOL bNullable, int nSystemID, BOOL bTCCompat, BOOL bMemo, BOOL * bNullArgument );
BOOL SR_itemEmpty( PHB_ITEM pItem );
void commonError( AREAP ThisDb, USHORT uiGenCode, USHORT uiSubCode, char* filename );
HB_ERRCODE SetBindEmptylValue( COLUMNBINDP BindStructure );
HB_ERRCODE SetBindValue( PHB_ITEM pFieldData, COLUMNBINDP BindStructure, HSTMT hStmt );
char * QualifyName( char * szName, SQLEXAREAP thiswa );
COLUMNBINDP GetBindStruct( SQLEXAREAP thiswa, INDEXBINDP IndexBind );
BOOL getColumnList( SQLEXAREAP thiswa );
void SolveFilters( SQLEXAREAP thiswa, BOOL bWhere );
void getOrderByExpression( SQLEXAREAP thiswa, BOOL bUseOptimizerHints );
void setResultSetLimit( SQLEXAREAP thiswa, int iRows );
void SetIndexBindStructure( SQLEXAREAP thiswa );
void SetInsertRecordStructure( SQLEXAREAP thiswa );
ULONG GetCurrentRecordNum( SQLEXAREAP thiswa );
extern void odbcGetData( SQLHSTMT hStmt, PHB_ITEM pField,PHB_ITEM pItem,  BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,USHORT ui  );
/* INSERT and UPDATE prototypes */

void CreateInsertStmt( SQLEXAREAP thiswa );
void SetCurrRecordStructure( SQLEXAREAP thiswa );
HB_ERRCODE CreateUpdateStmt( SQLEXAREAP thiswa );
HB_ERRCODE PrepareInsertStmt( SQLEXAREAP thiswa );
HB_ERRCODE BindInsertColumns( SQLEXAREAP thiswa );
HB_ERRCODE FeedRecordCols( SQLEXAREAP thiswa, BOOL bUpdate );
HB_ERRCODE ExecuteInsertStmt( SQLEXAREAP thiswa );
HB_ERRCODE ExecuteUpdateStmt( SQLEXAREAP thiswa );

/* SEEK Prototypes */

HB_ERRCODE FeedSeekKeyToBindings( SQLEXAREAP thiswa, PHB_ITEM pKey, int * queryLevel );
BOOL CreateSeekStmt( SQLEXAREAP thiswa, int queryLevel );
void BindSeekStmt( SQLEXAREAP thiswa, int queryLevel );
HB_ERRCODE getPreparedSeek( SQLEXAREAP thiswa, int queryLevel, USHORT * iIndex, HSTMT * hStmt );

#endif
