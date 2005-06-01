/*
 * $Id: hbrddntx.h,v 1.26 2005/05/06 23:44:58 druzus Exp $
 */

/*
 * Harbour Project source code:
 * DBFNTX RDD
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_RDDNTX_H_
#define HB_RDDNTX_H_

#include "hbapirdd.h"
#include "hbdbferr.h"
#ifndef HB_CDP_SUPPORT_OFF
#include "hbapicdp.h"
#endif
#define HB_EXTERNAL_RDDDBF_USE
#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* DBFNTX default extensions */
#define NTX_INDEXEXT                             ".ntx"

/* DBFNTX constants declarations */

#define TOP_RECORD                                    1
#define BTTM_RECORD                                   2
#define PREV_RECORD                                   3
#define NEXT_RECORD                                   4

#define NTX_IGNORE_REC_NUM                        0x0UL
#define NTX_MAX_REC_NUM                    0xFFFFFFFFUL

#define NTX_DUMMYNODE                      0xFFFFFFFFUL

#define NTX_FLAG_DEFALUT         0x06
#define NTX_FLAG_FORITEM         0x01
#define NTX_FLAG_CUSTOM          0x08
#define NTX_FLAG_TEMPORARY       0x10
#define NTX_FLAG_EXTLOCK         0x20
#define NTX_FLAG_MASK            0x3F


#define NTX_MAX_KEY           256      /* Max len of key */
#define NTXBLOCKSIZE         1024      /* Size of block in NTX file */
#define NTX_MAX_TAGNAME        12      /* Max len of tag name */
#define NTX_HDR_UNUSED        473      /* the unused part of header */
#define NTX_PAGES_PER_TAG      32
#define NTX_STACKSIZE          32

/* forward declarations
 */
struct _RDDFUNCS;
struct _NTXAREA;
struct _TAGINFO;
struct _NTXINDEX;

typedef struct _KEYINFO
{
   ULONG    Tag;      /* page number */
   ULONG    Xtra;     /* record number */
   char     key[ 1 ]; /* key value */
} KEYINFO;
typedef KEYINFO * LPKEYINFO;

typedef struct _TREE_STACK
{
   ULONG    page;
   SHORT    ikey;
}  TREE_STACK;
typedef TREE_STACK * LPTREESTACK;

typedef struct HB_PAGEINFO_STRU
{
   ULONG     Page;
   BOOL      Changed;
   int       iUsed;
   USHORT    uiKeys;
   char*     buffer;
} HB_PAGEINFO;
typedef HB_PAGEINFO * LPPAGEINFO;

typedef struct _HB_NTXSCOPE
{
   PHB_ITEM   scopeItem;
   LPKEYINFO  scopeKey;
   USHORT     scopeKeyLen;
} HB_NTXSCOPE;
typedef HB_NTXSCOPE * PHB_NTXSCOPE;

typedef struct _TAGINFO
{
   char *     TagName;
   char *     KeyExpr;
   char *     ForExpr;
   PHB_ITEM   pKeyItem;
   PHB_ITEM   pForItem;
   HB_NTXSCOPE top;
   HB_NTXSCOPE bottom;
   BOOL       fTagName;
   BOOL       fUsrDescend;
   BOOL       AscendKey;
   BOOL       UniqueKey;
   BOOL       Custom;
   BOOL       TagChanged;
   BOOL       TagBOF;
   BOOL       TagEOF;
   BOOL       Memory;
   BYTE       KeyType;
   BYTE       OptFlags;
   ULONG      TagBlock;       /* next free page ??? */
   ULONG      RootBlock;      /* root page offset */
   USHORT     nField;
   USHORT     KeyLength;
   USHORT     KeyDec;
   USHORT     MaxKeys;
   LPTREESTACK stack;
   USHORT     stackSize;
   USHORT     stackDepth;
   USHORT     stackLevel;
   ULONG      keyCount;
   ULONG      ulPagesDepth;
   ULONG      ulPages;
   ULONG      ulPageLast;
   ULONG      ulPagesStart;
   LPKEYINFO  CurKeyInfo;
   LPKEYINFO  HotKeyInfo;
   LPPAGEINFO pages;
   BOOL       InIndex;
   char*      buffer;
   struct    _NTXINDEX * Owner;
   struct    _TAGINFO * pNext;
} TAGINFO;

typedef TAGINFO * LPTAGINFO;

typedef struct _NTXINDEX
{
   char *      IndexName;
   ULONG       NextAvail;
   USHORT      Version;       /* The index VERSION filed to signal index updates for other stations */
   struct     _NTXAREA * Owner;
   FHANDLE     DiskFile;
   BOOL        fFlush;
   HB_FOFFSET  ulLockPos;     /* readlock position for CL53 lock scheme */
   int         lockWrite;     /* number of write lock set */
   int         lockRead;      /* number of read lock set */
   LPTAGINFO   CompoundTag;
   struct     _NTXINDEX * pNext;   /* The next index in the list */
} NTXINDEX;

typedef NTXINDEX * LPNTXINDEX;

/* for index creation */
typedef struct
{
   HB_FOFFSET  nOffset;    /* offset in temporary file */
   ULONG       ulKeys;     /* number of keys in page */
   ULONG       ulKeyBuf;   /* number of keys in memory buffer */
   ULONG       ulCurKey;   /* current key in memory buffer */
   BYTE *      pKeyPool;   /* memory buffer */
} NTXSWAPPAGE;
typedef NTXSWAPPAGE * LPNTXSWAPPAGE;

typedef struct _NTXHEADER    /* Header of NTX file */
{
   BYTE  type[2];
   BYTE  version[2];
   BYTE  root[4];
   BYTE  next_page[4];
   BYTE  item_size[2];
   BYTE  key_size[2];
   BYTE  key_dec[2];
   BYTE  max_item[2];
   BYTE  half_page[2];
   BYTE  key_expr[ NTX_MAX_KEY ];
   BYTE  unique[1];
   BYTE  unknown1[1];
   BYTE  descend[1];
   BYTE  unknown2[1];
   BYTE  for_expr[ NTX_MAX_KEY ];
   BYTE  tag_name[ NTX_MAX_TAGNAME ];
   BYTE  custom[1];
   BYTE  unused[ NTX_HDR_UNUSED ];
} NTXHEADER;

typedef NTXHEADER * LPNTXHEADER;

typedef struct
{
   LPTAGINFO pTag;             /* current Tag */
   FHANDLE  hTempFile;        /* handle to temporary file */
   char *   szTempFileName;   /* temporary file name */
   int      keyLen;           /* key length */
   BOOL     fUnique;          /* TRUE if index is unique */
   ULONG    ulMaxRec;         /* the highest record number */
   ULONG    ulTotKeys;        /* total number of keys indexed */
   ULONG    ulKeys;           /* keys in curently created page */
   ULONG    ulPages;          /* number of pages */
   ULONG    ulCurPage;        /* current page */
   ULONG    ulPgKeys;         /* maximum number of key in page memory buffer */
   ULONG    ulMaxKey;         /* maximum number of keys in single page */
   BYTE *   pKeyPool;         /* memory buffer for current page then for pages */
   LPNTXSWAPPAGE pSwapPage;   /* list of pages */
   LPPAGEINFO NodeList[ NTX_STACKSIZE ];   /* Stack of pages */
   ULONG    ulFirst;
   ULONG *  pSortedPages;
   BYTE     pLastKey[ NTX_MAX_KEY ]; /* last key val */
   ULONG    ulLastRec;
   BYTE *   pRecBuff;
} NTXSORTINFO;
typedef NTXSORTINFO * LPNTXSORTINFO;

/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFNTX RDD
 *
 */

typedef struct _NTXAREA
{
   struct _RDDFUNCS * lprfsHost; /* Virtual method table for this workarea */
   USHORT uiArea;                /* The number assigned to this workarea */
   void * atomAlias;             /* Pointer to the alias symbol for this workarea */
   USHORT uiFieldExtent;         /* Total number of fields allocated */
   USHORT uiFieldCount;          /* Total number of fields used */
   LPFIELD lpFields;             /* Pointer to an array of fields */
   void * lpFieldExtents;        /* Void ptr for additional field properties */
   PHB_ITEM valResult;           /* All purpose result holder */
   BOOL fTop;                    /* TRUE if "top" */
   BOOL fBottom;                 /* TRUE if "bottom" */
   BOOL fBof;                    /* TRUE if "bof" */
   BOOL fEof;                    /* TRUE if "eof" */
   BOOL fFound;                  /* TRUE if "found" */
   DBSCOPEINFO dbsi;             /* Info regarding last LOCATE */
   DBFILTERINFO dbfi;            /* Filter in effect */
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   LPDBRELINFO lpdbRelations;    /* Parent/Child relationships used */
   USHORT uiParents;             /* Number of parents for this area */
   USHORT heap;
   USHORT heapSize;
   USHORT rddID;
   USHORT uiMaxFieldNameLength;

   /*
   *  DBFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   FHANDLE hDataFile;            /* Data file handle */
   FHANDLE hMemoFile;            /* Memo file handle */
   USHORT uiHeaderLen;           /* Size of header */
   USHORT uiRecordLen;           /* Size of record */
   ULONG ulRecCount;             /* Total records */
   char * szDataFileName;        /* Name of data file */
   char * szMemoFileName;        /* Name of memo file */
   USHORT uiMemoBlockSize;       /* Size of memo block */
   BYTE bMemoType;               /* MEMO type used in DBF memo fields */
   BOOL fHasMemo;                /* WorkArea with Memo fields */
   BOOL fHasTags;                /* WorkArea with MDX or CDX index */
   BOOL fDataFlush;              /* data was written to DBF and not commited */
   BOOL fMemoFlush;              /* data was written to MEMO and not commited */
   BYTE bVersion;                /* DBF version ID byte */
   BYTE bCodePage;               /* DBF codepage ID */
   BOOL fShared;                 /* Shared file */
   BOOL fReadonly;               /* Read only file */
   USHORT * pFieldOffset;        /* Pointer to field offset array */
   BYTE * pRecord;               /* Buffer of record data */
   BOOL fValidBuffer;            /* State of buffer */
   BOOL fPositioned;             /* Positioned record */
   ULONG ulRecNo;                /* Current record */
   BOOL fRecordChanged;          /* Record changed */
   BOOL fAppend;                 /* TRUE if new record is added */
   BOOL fDeleted;                /* TRUE if record is deleted */
   BOOL fEncrypted;              /* TRUE if record is encrypted */
   BOOL fUpdateHeader;           /* Update header of file */
   BOOL fFLocked;                /* TRUE if file is locked */
   BOOL fHeaderLocked;           /* TRUE if DBF header is locked */
   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */
   BYTE bYear;                   /* Last update */
   BYTE bMonth;
   BYTE bDay;
   BYTE bLockType;               /* Type of locking shemes */
   ULONG * pLocksPos;            /* List of records locked */
   ULONG ulNumLocksPos;          /* Number of records locked */
#ifndef HB_CDP_SUPPORT_OFF
   PHB_CODEPAGE cdPage;          /* Area's codepage pointer  */
#endif

   /*
   *  NTX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   LPTAGINFO      lpCurTag;      /* Pointer to current order */
   LPTAGINFO      lpNtxTag;      /* Pointer to tags list */
   BOOL           fNtxAppend;    /* TRUE if new record is added */
   LPNTXSORTINFO  pSort;         /* Index build structure */

} NTXAREA;

typedef NTXAREA * LPNTXAREA;

#ifndef NTXAREAP
#define NTXAREAP LPNTXAREA
#endif


/*
 * -- DBFNTX METHODS --
 */

#define SUPERTABLE                         ( &ntxSuper )

#define ntxBof                   NULL
#define ntxEof                   NULL
#define ntxFound                 NULL
static ERRCODE ntxGoBottom( NTXAREAP pArea );
#define ntxGoTo                  NULL
#define ntxGoToId                NULL
static ERRCODE ntxGoTop( NTXAREAP pArea );
static ERRCODE ntxSeek( NTXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast );
#define ntxSkip                  NULL
#define ntxSkipFilter            NULL
static ERRCODE ntxSkipRaw( NTXAREAP pArea, LONG lToSkip );
#define ntxAddField              NULL
/* static ERRCODE ntxAppend( NTXAREAP pArea, BOOL bUnLockAll ); */
#define ntxAppend                NULL
#define ntxCreateFields          NULL
#define ntxDeleteRec             NULL
#define ntxDeleted               NULL
#define ntxFieldCount            NULL
#define ntxFieldDisplay          NULL
#define ntxFieldInfo             NULL
#define ntxFieldName             NULL
static ERRCODE ntxFlush( NTXAREAP pArea );
#define ntxGetRec                NULL
#define ntxGetValue              NULL
#define ntxGetVarLen             NULL
static ERRCODE ntxGoCold( NTXAREAP pArea );
static ERRCODE ntxGoHot( NTXAREAP pArea );
#define ntxPutRec                NULL
#define ntxPutValue              NULL
#define ntxRecall                NULL
#define ntxRecCount              NULL
#define ntxRecInfo               NULL
#define ntxRecNo                 NULL
#define ntxSetFieldsExtent       NULL
#define ntxAlias                 NULL
static ERRCODE ntxClose( NTXAREAP pArea );
         /* Close workarea - at first we mus close all indexes and than close
            workarea */
#define ntxCreate                NULL
#define ntxInfo                  NULL
#define ntxNewArea               NULL
#define ntxOpen                  NULL
#define ntxRelease               NULL
static ERRCODE ntxStructSize( NTXAREAP pArea, USHORT * uiSize );
static ERRCODE ntxSysName( NTXAREAP pArea, BYTE * pBuffer );
#define ntxEval                  NULL
static ERRCODE ntxPack( NTXAREAP pArea );
#define ntPackRec                NULL
#define ntxSort                  NULL
#define ntxTrans                 NULL
#define ntxTransRec              NULL
static ERRCODE ntxZap( NTXAREAP pArea );
#define ntxchildEnd              NULL
#define ntxchildStart            NULL
#define ntxchildSync             NULL
#define ntxsyncChildren          NULL
#define ntxclearRel              NULL
#define ntxforceRel              NULL
#define ntxrelArea               NULL
#define ntxrelEval               NULL
#define ntxrelText               NULL
#define ntxsetRel                NULL
static ERRCODE ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
         /* Open next index */
static ERRCODE ntxOrderListClear( NTXAREAP pArea );
         /* Close all indexes */
#define ntxOrderListDelete       NULL
static ERRCODE ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
static ERRCODE ntxOrderListRebuild( NTXAREAP pArea );
static ERRCODE ntxOrderCondition( NTXAREAP area, LPDBORDERCONDINFO pOrdCondInfo );
static ERRCODE ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo );
         /* Create new Index */
#define ntxOrderDestroy          NULL
static ERRCODE ntxOrderInfo( NTXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo );
         /* Some information about index */
#define ntxClearFilter           NULL
#define ntxClearLocate           NULL
#define ntxClearScope            NULL
#define ntxCountScope            NULL
#define ntxFilterText            NULL
#define ntxScopeInfo             NULL
#define ntxSetFilter             NULL
#define ntxSetLocate             NULL
#define ntxSetScope              NULL
#define ntxSkipScope             NULL
#define ntxCompile               NULL
#define ntxError                 NULL
#define ntxEvalBlock             NULL
#define ntxRawLock               NULL
#define ntxLock                  NULL
#define ntxUnLock                NULL
#define ntxCloseMemFile          NULL
#define ntxCreateMemFile         NULL
#define ntxGetValueFile          NULL
#define ntxOpenMemFile           NULL
#define ntxPutValueFile          NULL
#define ntxReadDBHeader          NULL
#define ntxWriteDBHeader         NULL
#define ntxExit                  NULL
#define ntxDrop                  NULL
#define ntxExists                NULL
#define ntxWhoCares              NULL

HB_EXTERN_END

#endif /* HB_RDDNTX_H_ */
