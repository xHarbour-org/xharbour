/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Main File
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "sqlrdd.h"
#include "hbset.h"
#include "msg.ch"
#include "hbfast.h"
#include "hashapi.h"
#include "rddsys.ch"
//#include "hbtrace.h"
#include "sqlrddsetup.ch"
#include "sqlprototypes.h"

//#undef HB_IS_OBJECT
//#define HB_IS_OBJECT( p )  ( ( p ) && HB_IS_OF_TYPE( p, HB_IT_OBJECT ) && ( p )->item.asArray.value->uiClass != 0 )

#include <ctype.h>
#include <assert.h>

#ifdef _WIN32
   #include <Windows.h>
#else
   #include <stdlib.h>
   #include <unistd.h>
   #include <errno.h>
   #include <sys/types.h>
   #include <sys/wait.h>
#endif

// Support for old xHarbour versions

#ifndef HB_FT_STRING
#define HB_FT_STRING HB_IT_STRING
#endif

#ifndef HB_FT_MEMO
#define HB_FT_MEMO HB_IT_MEMO
#endif

#ifndef HB_FT_LONG
#define HB_FT_LONG HB_IT_LONG
#endif

#ifndef HB_FT_LOGICAL
#define HB_FT_LOGICAL HB_IT_LOGICAL
#endif

#ifndef HB_FT_DATE
#define HB_FT_DATE HB_IT_DATE
#endif

#ifdef SQLRDD_COMPAT_PRE_1_1
#ifndef HB_RDD_MAX_ALIAS_LEN
   #define HB_RDD_MAX_ALIAS_LEN            32
#endif
#define hb_setGetDeleted() hb_set.HB_SET_DELETED
#define hb_setGetAutOpen() hb_set.HB_SET_AUTOPEN
#define hb_cdppage()       hb_cdp_page
#define HB_ERRCODE ERRCODE
#define HB_SUCCESS SUCCESS
#define HB_FAILURE FAILURE
#endif

#undef HB_TRACE
#define HB_TRACE(x, y)

static RDDFUNCS sqlrddSuper;

void startSQLRDDSymbols(void);
/*
static PHB_ITEM loadTag( SQLAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder );
*/
HB_EXTERN_BEGIN
   PHB_ITEM loadTagDefault( SQLAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder );
HB_EXTERN_END

HB_FUNC_EXTERN( SR_END );
HB_FUNC_EXTERN( SR_INIT );
HB_FUNC_EXTERN( __SR_STARTSQL );

/*------------------------------------------------------------------------*/

static PHB_DYNS s_pSym_SQLGOBOTTOM;
static PHB_DYNS s_pSym_SQLGOTO;
static PHB_DYNS s_pSym_SQLGOTOP;
static PHB_DYNS s_pSym_SQLSEEK;
static PHB_DYNS s_pSym_SETBOF;
static PHB_DYNS s_pSym_SQLDELETEREC;
static PHB_DYNS s_pSym_SQLFLUSH;
static PHB_DYNS s_pSym_SQLRECALL;
static PHB_DYNS s_pSym_SQLCLOSE;
static PHB_DYNS s_pSym_SQLCREATE;
static PHB_DYNS s_pSym_SQLOPEN;
static PHB_DYNS s_pSym_SQLOPENALLINDEXES;
static PHB_DYNS s_pSym_SQLPACK;
static PHB_DYNS s_pSym_SQLZAP;
static PHB_DYNS s_pSym_SQLORDERLISTADD;
static PHB_DYNS s_pSym_SQLORDERLISTCLEAR;
static PHB_DYNS s_pSym_SQLORDERLISTFOCUS;
static PHB_DYNS s_pSym_SQLORDERCREATE;
static PHB_DYNS s_pSym_SQLORDERDESTROY;
static PHB_DYNS s_pSym_SQLORDERCONDITION;
static PHB_DYNS s_pSym_SQLORDERLISTNUM;
static PHB_DYNS s_pSym_SQLSETSCOPE;
static PHB_DYNS s_pSym_SQLLOCK;
static PHB_DYNS s_pSym_SQLUNLOCK;
static PHB_DYNS s_pSym_SQLDROP;
static PHB_DYNS s_pSym_SQLEXISTS;
static PHB_DYNS s_pSym_SQLKEYCOUNT;
static PHB_DYNS s_pSym_SQLRECSIZE;
static PHB_DYNS s_pSym_WRITEBUFFER;
static PHB_DYNS s_pSym_READPAGE;
static PHB_DYNS s_pSym_STABILIZE;
static PHB_DYNS s_pSym_NORMALIZE;
static PHB_DYNS s_pSym_SQLGETVALUE;
static PHB_DYNS s_pSym_SQLSETFILTER;
static PHB_DYNS s_pSym_SQLCLEARFILTER;
static PHB_DYNS s_pSym_SQLFILTERTEXT;

static PHB_DYNS s_pSym_SQLINIT  = NULL;
static PHB_DYNS s_pSym_SQLEXIT  = NULL;
static PHB_DYNS s_pSym_WORKAREA = NULL;

/*------------------------------------------------------------------------*/

void fixCachePointer( LONG * lPosCache )
{
   if( * lPosCache < 1 )
   {
      * lPosCache += (CAHCE_PAGE_SIZE * 3);
   }
   else if( * lPosCache > (CAHCE_PAGE_SIZE * 3) )
   {
      * lPosCache -= (CAHCE_PAGE_SIZE * 3);
   }
}

/*------------------------------------------------------------------------*/

HB_FUNC( SR_FIXCACHEPOINTER )
{
   LONG lPos = hb_parnl( 1 );
   fixCachePointer( &lPos );
   hb_retnl( lPos );
}

/*------------------------------------------------------------------------*/

BOOL isCachePointerInRange( LONG lPosCache, LONG lBegin, LONG lEnd )
{
   if( lBegin == lEnd )
   {
      return lPosCache == lBegin;
   }
   if( lBegin < lEnd )
   {
      return (lPosCache >= lBegin && lPosCache <= lEnd);
   }
   return (lPosCache >= lBegin || lPosCache <= lEnd);
}

/*------------------------------------------------------------------------*/

LONG searchCacheFWD( SQLAREAP thiswa, LONG lPreviousCacheStatus )
{
   LONG lBegin, lEnd;
   LONG lPosCache = hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE );

   if( !lPosCache )
   {
      return 0;
   }

   lBegin = hb_arrayGetNL( thiswa->aInfo, AINFO_NCACHEBEGIN );
   lEnd   = hb_arrayGetNL( thiswa->aInfo, AINFO_NCACHEEND );

   if( lPreviousCacheStatus )
   {
      lPosCache++;
   }
   fixCachePointer( &lPosCache );

   while( isCachePointerInRange( lPosCache, lBegin, lEnd ) )
   {
      if( !HB_IS_NIL( hb_arrayGetItemPtr( thiswa->aCache, lPosCache ) ) )
      {
         return lPosCache;
      }
      lPosCache++;
      fixCachePointer( &lPosCache );
   }
   return 0;
}

/*------------------------------------------------------------------------*/

LONG searchCacheBWD( SQLAREAP thiswa, LONG lPreviousCacheStatus )
{
   LONG lBegin, lEnd;
   LONG lPosCache = hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE );

   if( !lPosCache )
   {
      return 0;
   }

   lBegin = hb_arrayGetNL( thiswa->aInfo, AINFO_NCACHEBEGIN );
   lEnd   = hb_arrayGetNL( thiswa->aInfo, AINFO_NCACHEEND );

   if( lPreviousCacheStatus )
   {
      lPosCache--;
   }
   fixCachePointer( &lPosCache );

   while( isCachePointerInRange( lPosCache, lBegin, lEnd ) )
   {
      if( !HB_IS_NIL( hb_arrayGetItemPtr( thiswa->aCache, lPosCache ) ) )
      {
         return lPosCache;
      }
      lPosCache--;
      fixCachePointer( &lPosCache );
   }
   return 0;
}

/*------------------------------------------------------------------------*/

void readCachePageFWD( SQLAREAP thiswa )
{
   PHB_ITEM pOrd = hb_itemNew( NULL );
   PHB_ITEM pDel = hb_itemNew( NULL );
   hb_itemPutNL( pOrd, ORD_DIR_FWD );
   hb_itemPutL( pDel, thiswa->wasdel );
   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_READPAGE->pSymbol, 2, pOrd, pDel );
   hb_itemRelease( pOrd );
   hb_itemRelease( pDel );
}

/*------------------------------------------------------------------------*/

void readCachePageBWD( SQLAREAP thiswa )
{
   PHB_ITEM pOrd = hb_itemNew( NULL );
   PHB_ITEM pDel = hb_itemNew( NULL );
   hb_itemPutNL( pOrd, ORD_DIR_BWD );
   hb_itemPutL( pDel, thiswa->wasdel );
   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_READPAGE->pSymbol, 2, pOrd, pDel );
   hb_itemRelease( pOrd );
   hb_itemRelease( pDel );
}

/*------------------------------------------------------------------------*/

void setCurrentFromCache( SQLAREAP thiswa, LONG lPos )
{
   ULONG ulPos;
   PHB_ITEM pCacheRecord, pCol;

   hb_arraySetNL( thiswa->aInfo, AINFO_NPOSCACHE, lPos );

   pCacheRecord = (PHB_ITEM) hb_arrayGetItemPtr( thiswa->aCache, lPos );

   pCol = hb_itemNew( NULL );

   for( ulPos = 1; ulPos <= pCacheRecord->item.asArray.value->ulLen; ulPos++ )
   {

      hb_arrayGet( pCacheRecord, ulPos,  pCol );
      hb_arraySet( thiswa->aOldBuffer, ulPos, pCol );

      if( ulPos == thiswa->ulhRecno )
      {
         hb_arraySet( thiswa->aInfo, AINFO_RECNO, pCol );
      }

      if( ulPos == thiswa->ulhDeleted )
      {
         char * deleted = hb_itemGetCPtr( pCol );
         if( * deleted == 'T' || * deleted == '*' )
         {
            hb_arraySetL( thiswa->aInfo, AINFO_DELETED, TRUE );
         }
         else
         {
            hb_arraySetL( thiswa->aInfo, AINFO_DELETED, FALSE );
         }
      }
      hb_arraySetForward( thiswa->aBuffer, ulPos, pCol );
   }

   if( thiswa->ulhDeleted == 0 )
   {
      hb_arraySetL( thiswa->aInfo, AINFO_DELETED, FALSE );
   }

   hb_itemRelease( pCol );
}

/*------------------------------------------------------------------------*/

void sqlGetBufferFromCache2( SQLAREAP thiswa, LONG lPos )
{
   ULONG ulPos;
   PHB_ITEM pCacheRecord, pCol;

   pCacheRecord = (PHB_ITEM) hb_arrayGetItemPtr( thiswa->aCache, lPos );

   pCol = hb_itemNew( NULL );

   for( ulPos = 1; ulPos <= pCacheRecord->item.asArray.value->ulLen; ulPos++ )
   {
      hb_arrayGet( pCacheRecord, ulPos,  pCol );
      hb_arraySet( thiswa->aOldBuffer, ulPos, pCol );
      if( ulPos == thiswa->ulhRecno )
      {
         hb_arraySet( thiswa->aInfo, AINFO_RECNO, pCol );
      }
      if( ulPos == thiswa->ulhDeleted )
      {
         char * deleted = hb_itemGetCPtr( pCol );
         if( * deleted == 'T' || * deleted == '*' )
         {
            hb_arraySetL( thiswa->aInfo, AINFO_DELETED, TRUE );
         }
         else
         {
            hb_arraySetL( thiswa->aInfo, AINFO_DELETED, FALSE );
         }
      }
      hb_arraySetForward( thiswa->aBuffer, ulPos, pCol );
   }

   if( thiswa->ulhDeleted == 0 )
   {
      hb_arraySetL( thiswa->aInfo, AINFO_DELETED, FALSE );
   }

   hb_itemRelease( pCol );
   hb_arraySetNL( thiswa->aInfo, AINFO_NPOSCACHE, lPos );
}

/*------------------------------------------------------------------------*/

static void sqlGetCleanBuffer( SQLAREAP thiswa )
{
   ULONG ulPos;
   PHB_ITEM pCol;

   pCol = hb_itemNew( NULL );
   for( ulPos = 1; ulPos <= thiswa->aEmptyBuff->item.asArray.value->ulLen; ulPos++ )
   {
      hb_arrayGet( thiswa->aEmptyBuff, ulPos, pCol );
      hb_arraySet( thiswa->aOldBuffer, ulPos, pCol );
      hb_arraySetForward( thiswa->aBuffer, ulPos, pCol );
   }
   hb_arraySetL( thiswa->aInfo, AINFO_DELETED, FALSE );
   hb_arraySetL( thiswa->aInfo, AINFO_EOF, TRUE );
   hb_arrayGet( thiswa->aInfo, AINFO_RCOUNT, pCol );
   hb_itemPutNL( pCol, hb_itemGetNL( pCol ) + 1 );
   hb_arraySetForward( thiswa->aInfo, AINFO_RECNO, pCol );
   hb_itemRelease( pCol );
   if( !thiswa->isam )
   {
      hb_arraySetNL( thiswa->aInfo, AINFO_NPOSCACHE, thiswa->aCache->item.asArray.value->ulLen + 1 );
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlBof( SQLAREAP thiswa, BOOL * bof )
{
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) thiswa );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
   *bof = thiswa->area.fBof;

   // TraceLog( NULL, "sqlBof, returning %i\n", thiswa->area.fBof );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlEof( SQLAREAP thiswa, BOOL * eof )
{
   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) thiswa );

   if (hb_arrayGetL( thiswa->aInfo, AINFO_ISINSERT ) && hb_arrayGetL( thiswa->aInfo, AINFO_HOT ) )
   {
      *eof = FALSE;
   }
   else
   {
      thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
      *eof = thiswa->area.fEof;
   }

   // TraceLog( NULL, "sqlEof, returning %i\n", thiswa->area.fEof );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlFound( SQLAREAP thiswa, BOOL * found )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   thiswa->area.fFound = hb_arrayGetL( thiswa->aInfo, AINFO_FOUND );
   *found = thiswa->area.fFound;

   // TraceLog( NULL, "sqlFound, returning %i\n", thiswa->area.fFound );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGoBottom( SQLAREAP thiswa )
{
   LONG leof;
   PHB_ITEM eofat;

   eofat = hb_itemNew( NULL );

   // TraceLog( NULL, "sqlGoBottom\n" );

   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract  = 0;
   thiswa->wasdel         = 0;

   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
   }

   leof = hb_arrayGetNL( thiswa->aInfo, AINFO_EOF_AT );

   if (hb_arrayGetNL( thiswa->aInfo, AINFO_EOF_AT ))
   {
      hb_itemPutNL( eofat, leof );
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTO->pSymbol, 1, eofat );
   }
   else
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOBOTTOM->pSymbol, 0 );
   }

   thiswa->area.fTop = FALSE;
   thiswa->area.fBottom = TRUE;
   thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
   hb_itemRelease( eofat );

   SELF_SKIPFILTER( ( AREAP ) thiswa, -1 );

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGoTo( SQLAREAP thiswa, LONG recno )
{
   HB_ITEM Param1;

   Param1.type = HB_IT_LONG;

   // TraceLog( NULL, "sqlGoTo %i\n", recno );

   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   Param1.item.asLong.value = recno;

   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTO->pSymbol, 1, &Param1 );

   thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGoToId( SQLAREAP thiswa, PHB_ITEM pItem )
{

   HB_TRACE(HB_TR_DEBUG, ("sqlGoToId1(%p, %p)", thiswa, pItem));

   // TraceLog( NULL, "sqlGoToId\n" );

   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   if( HB_IS_NUMERIC( pItem ) )
   {
      return SELF_GOTO( (AREAP) thiswa, (LONG) hb_itemGetNL( pItem ) );
   }
   else
   {
      if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
      {
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
      }

      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTO->pSymbol, 1, pItem );

      thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
      thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );

      return ( HB_SUCCESS );
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGoTop( SQLAREAP thiswa )
{
   LONG lbof;
   HB_ITEM bofat;

   // TraceLog( NULL, "sqlGoTop\n" );

   bofat.type = HB_IT_NIL;

   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
   }

   lbof = hb_arrayGetNL( thiswa->aInfo, AINFO_BOF_AT );

   if ( lbof )
   {
      hb_itemPutNL( &bofat, lbof );
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTO->pSymbol, 1, &bofat );
   }
   else
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTOP->pSymbol, 0 );
   }

   thiswa->area.fTop = TRUE;
   thiswa->area.fBottom = FALSE;
   thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );

   SELF_SKIPFILTER( ( AREAP ) thiswa, 1 );

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

int sqlKeyCompare( AREAP thiswa, PHB_ITEM pKey, BOOL fExact )
{
   LONG lorder  = 0;
   PHB_ITEM pTag, pKeyVal, itemTemp;
   int iLimit, iResult = 0;
   BYTE len1, len2;
   char * val1;
   char * val2;

   // TraceLog( NULL, "sqlKeyCompare\n" );

   pTag = loadTagDefault( (SQLAREAP) thiswa, NULL, &lorder );
   if( pTag )
   {
      if( ((SQLAREAP)thiswa)->firstinteract )
      {
         SELF_GOTOP( (AREAP) thiswa );
         ((SQLAREAP)thiswa)->firstinteract = 0;
      }
      itemTemp = hb_itemArrayGet( pTag, INDEX_KEY_CODEBLOCK );
      if ( HB_IS_NUMBER( itemTemp ) )
      {
         pKeyVal = hb_itemArrayGet( ((SQLAREAP)thiswa)->aBuffer, hb_arrayGetNL( pTag, INDEX_KEY_CODEBLOCK ) );
         len1 = hb_strRTrimLen( hb_itemGetCPtr( pKeyVal ), hb_itemGetCLen( pKeyVal ), FALSE ) - 15;
         val1 = hb_itemGetCPtr( pKeyVal );
      }
      else
      {
         EVALINFO info;
         hb_evalNew( &info, hb_itemArrayGet( pTag, INDEX_KEY_CODEBLOCK ) );
         pKeyVal = hb_evalLaunch( &info );
         hb_evalRelease( &info );
         len1 = hb_itemGetCLen( pKeyVal );
         val1 = hb_itemGetCPtr( pKeyVal );
      }
      hb_itemRelease( itemTemp );
      hb_itemRelease( pTag );
   }
   else
      return 0;

   if( HB_IS_DATE( pKey ) )
   {
      len2 = 8;
      val2 = ( char * ) hb_xgrab( 9 );
      val2 = hb_itemGetDS( pKey, val2 );
   }
   else if( HB_IS_NUMBER( pKey ) )
   {
      PHB_ITEM pLen = hb_itemPutNL( NULL, (const LONG) len1 );
      val2 = hb_itemStr( pKey, pLen, NULL );
      len2 = strlen( val2 );
      hb_itemRelease( pLen );
   }
   else if( HB_IS_LOGICAL( pKey ) )
   {
      len2 = 1;
      val2 = hb_itemGetL( pKey ) ? "T" : "F";
   }
   else
   {
      len2 = hb_itemGetCLen( pKey );
      val2 = hb_itemGetCPtr( pKey );
   }

   iLimit = (len1 > len2) ? len2 : len1;

   if ( HB_IS_STRING( pKeyVal ) )
   {
      if ( iLimit > 0 )
         iResult = memcmp( val1, val2, iLimit );

      if ( iResult == 0 )
      {
         if ( len1 >= len2 )
            iResult = 1;
         else if ( len1 < len2 && fExact )
            iResult = -1;
      }
      else
         iResult = 0;
   }
   else
   {
      if ( iLimit == 0 || (iResult = memcmp( val1, val2, iLimit )) == 0 )
      {
         if ( len1 >= len2 )
            iResult = 1;
         else if ( len1 < len2 )
            iResult = -1;
      }
   }

   if( HB_IS_DATE( pKey ) || HB_IS_NUMBER( pKey ) )
      hb_xfree( val2 );

   hb_itemRelease( pKeyVal );

   return iResult;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSeek( SQLAREAP thiswa, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   HB_ITEM Soft, Last, isEof;
   HB_ERRCODE retvalue = HB_SUCCESS;

   isEof.type = HB_IT_NIL;

   // TraceLog( NULL, "sqlSeek(%p, %d, %p, %d)", thiswa, bSoftSeek, pKey, bFindLast);

   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   Soft.type = HB_IT_LOGICAL;
   Soft.item.asLogical.value = bSoftSeek;

   Last.type = HB_IT_LOGICAL;
   Last.item.asLogical.value = bFindLast;

#ifndef HB_CDP_SUPPORT_OFF
   if( HB_IS_STRING( pKey ) )
   {
      if( thiswa->cdPageCnv )
         hb_cdpTranslate( hb_itemGetCPtr( pKey ), thiswa->cdPageCnv, thiswa->area.cdPage );
      else
         hb_cdpTranslate( hb_itemGetCPtr( pKey ), hb_cdppage(), thiswa->area.cdPage );
   }
#endif

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLSEEK->pSymbol, 3, pKey, &Soft, &Last );

   thiswa->area.fFound = hb_arrayGetL( thiswa->aInfo, AINFO_FOUND );
   thiswa->area.fBof   = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
   thiswa->area.fEof   = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );

   if (( hb_setGetDeleted() || thiswa->area.dbfi.itmCobExpr != NULL ) && !thiswa->area.fEof )
   {
      retvalue = SELF_SKIPFILTER( ( AREAP ) thiswa, ( bFindLast ? -1 : 1 ) );

      if ( thiswa->area.fEof )
      {
         thiswa->area.fFound = FALSE;
         hb_itemPutL( &isEof, thiswa->area.fFound );
         hb_arraySetForward( thiswa->aInfo, AINFO_FOUND, &isEof );
      }
      else
      {
         if ( sqlKeyCompare( ( AREAP ) thiswa, pKey, FALSE ) != 0 )
         {
            thiswa->area.fFound = TRUE;
            hb_itemPutL( &isEof, thiswa->area.fFound );
            hb_arraySetForward( thiswa->aInfo, AINFO_FOUND, &isEof );
         }
         else
         {
            thiswa->area.fFound = FALSE;
            hb_itemPutL( &isEof, thiswa->area.fFound );
            hb_arraySetForward( thiswa->aInfo, AINFO_FOUND, &isEof );

            if( !bSoftSeek )
            {
               sqlGetCleanBuffer( thiswa );
            }
         }
      }
   }

   if( thiswa->area.lpdbRelations && retvalue == HB_SUCCESS )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return retvalue;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSkip( SQLAREAP thiswa, LONG lToSkip )
{
   HB_ERRCODE ret;

   // TraceLog( NULL, "sqlSkip %i\n", lToSkip );

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if ( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return( HB_FAILURE );
   }

   thiswa->area.fTop = thiswa->area.fBottom = FALSE;

   ret = SUPER_SKIP( ( AREAP ) thiswa, lToSkip );     // This will call SKIPRAW

   hb_arraySetL( thiswa->aInfo, AINFO_BOF, thiswa->area.fBof );
   hb_arraySetL( thiswa->aInfo, AINFO_EOF, thiswa->area.fEof );
   thiswa->wasdel = 0;

   return ret;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlSkipFilter( SQLAREAP thiswa, LONG lUpDown )
{
   BOOL bOutOfRange, bDeleted;
   PHB_ITEM pResult;
   HB_ERRCODE uiError;

   // TraceLog( NULL, "sqlSkipFilter %i\n", lUpDown );

   if( !hb_setGetDeleted() && thiswa->area.dbfi.itmCobExpr == NULL )
   {
      return HB_SUCCESS;
   }

   lUpDown = ( lUpDown > 0  ?  1 : -1 );
   bOutOfRange = FALSE;

   while( TRUE )
   {
      if( thiswa->area.fBof || thiswa->area.fEof )
      {
         bOutOfRange = TRUE;
         break;
      }

      /* SET FILTER TO */
      if( thiswa->area.dbfi.itmCobExpr )
      {
         pResult = hb_vmEvalBlock( thiswa->area.dbfi.itmCobExpr );
         if( HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult ) )
         {
            SELF_SKIPRAW( (AREAP) thiswa, lUpDown );
            continue;
         }
      }

      /* SET DELETED */
      if( hb_setGetDeleted() )
      {
         SELF_DELETED( (AREAP) thiswa, &bDeleted );
         if( bDeleted )
         {
            SELF_SKIPRAW( (AREAP) thiswa, lUpDown );
            continue;
         }
      }
      break;
   }

   if( bOutOfRange )
   {
      if( lUpDown < 0 )
      {
         thiswa->area.fEof = FALSE;
         uiError = SELF_GOTOP( (AREAP) thiswa );

         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SETBOF->pSymbol, 0 );

         thiswa->area.fBof = TRUE;
      }
      else
      {
         thiswa->area.fBof = FALSE;
         uiError = HB_SUCCESS;
      }
   }
   else
      uiError = HB_SUCCESS;

   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE ConcludeSkipraw( SQLAREAP thiswa, LONG lToSkip )
{
/*
   if( lToSkip != 0 )
   {
      thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
      thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   }
*/
   HB_SYMBOL_UNUSED( lToSkip );

   /* Force relational movement in child WorkAreas */

   if( thiswa->area.lpdbRelations )
   {
      return SELF_SYNCCHILDREN( ( AREAP ) thiswa );
   }
   else
   {
      return HB_SUCCESS;
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSkipRaw( SQLAREAP thiswa, LONG lToSkip )
{
   BOOL bEof, bBof;
   PHB_ITEM pToSkip;

   // TraceLog( NULL, "sqlSkipRaw %i\n", lToSkip );

   bEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   bBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );

   thiswa->area.fFound = FALSE;

   if( (bEof && bBof) && lToSkip == -1 )
   {
      return SELF_GOBOTTOM( (AREAP) thiswa );         // <===========| RETURNING
   }

   if( (bEof && bBof) || lToSkip == 0 )
   {
      // Table is empty
      thiswa->area.fEof = bEof;
      thiswa->area.fBof = bBof;
      return HB_SUCCESS;         // <===========| RETURNING
   }

   hb_arraySetNL( thiswa->aInfo, AINFO_SKIPCOUNT, hb_arrayGetNL( thiswa->aInfo, AINFO_SKIPCOUNT ) + lToSkip );

   if( thiswa->isam )
   {
      LONG lPosCache, lFound, lPreviousCacheStatus;
      BOOL bCurrentDeleted;

      lPosCache        = hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE );
      bCurrentDeleted  = lPosCache && HB_IS_NIL( hb_arrayGetItemPtr( thiswa->aCache, lPosCache ) );

      if( lToSkip > 0 )
      {
         if( bEof )
         {
            return HB_SUCCESS;               // <===========| RETURNING
         }
         if ( iTemCompEqual( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_EOF_AT ),
                             hb_arrayGetItemPtr( thiswa->aInfo, AINFO_RECNO ) ) )
         {
            hb_arraySetL( thiswa->aInfo, AINFO_EOF, TRUE );
            thiswa->area.fEof = TRUE;
            sqlGetCleanBuffer( thiswa );
            return ConcludeSkipraw( thiswa, lToSkip );
         }

         lFound = searchCacheFWD( thiswa, 1 );

         if( lFound )
         {
            setCurrentFromCache( thiswa, lFound );
            thiswa->area.fBof = FALSE;
            return ConcludeSkipraw( thiswa, lToSkip );
         }
         lPreviousCacheStatus = lPosCache;
         readCachePageFWD( thiswa );
         lFound = searchCacheFWD( thiswa, lPreviousCacheStatus );
         if( lFound )
         {
            setCurrentFromCache( thiswa, lFound );
            thiswa->area.fBof = FALSE;
            return ConcludeSkipraw( thiswa, lToSkip );
         }
         hb_arraySetL( thiswa->aInfo, AINFO_EOF, TRUE );
         thiswa->area.fEof = TRUE;
         sqlGetCleanBuffer( thiswa );
         return ConcludeSkipraw( thiswa, lToSkip );
      }
      else  // lToSkip < 0
      {
         if( bCurrentDeleted )
         {
            if( !bBof )
            {
               if( bEof )
               {
                  SELF_GOBOTTOM( (AREAP) thiswa );
                  return ConcludeSkipraw( thiswa, lToSkip );
               }
               lFound = searchCacheBWD( thiswa, 1 );
               if( lFound )
               {
                  setCurrentFromCache( thiswa, lFound );
                  return ConcludeSkipraw( thiswa, lToSkip );
               }
               lPreviousCacheStatus = lPosCache;
               readCachePageBWD( thiswa );
               lFound = searchCacheBWD( thiswa, lPreviousCacheStatus );
               if( lFound )
               {
                  setCurrentFromCache( thiswa, lFound );
                  return ConcludeSkipraw( thiswa, lToSkip );
               }
            }
            lFound = searchCacheFWD( thiswa, 1 );
            if( lFound )
            {
               setCurrentFromCache( thiswa, lFound );
               hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
               thiswa->area.fBof = TRUE;
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            readCachePageFWD( thiswa );
            lFound = searchCacheFWD( thiswa, 1 );
            if( lFound )
            {
               setCurrentFromCache( thiswa, lFound );
               hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
               thiswa->area.fBof = TRUE;
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
            thiswa->area.fBof = TRUE;
            hb_arraySetL( thiswa->aInfo, AINFO_EOF, TRUE );
            thiswa->area.fEof = TRUE;
            sqlGetCleanBuffer( thiswa );
         }
         else
         {
            if( bBof )
            {
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            if( iTemCompEqual( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_BOF_AT ),
                               hb_arrayGetItemPtr( thiswa->aInfo, AINFO_RECNO ) ) )
            {
               // Checking optimizer
               hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
               thiswa->area.fBof = TRUE;
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            if( bEof )
            {
               if( iTemCompEqual( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_EOF_AT ),
                                  hb_arrayGetItemPtr( thiswa->aInfo, AINFO_RECNO ) ) )
               {
                  SELF_GOTOID( (AREAP) thiswa, hb_arrayGetItemPtr( thiswa->aInfo, AINFO_EOF_AT ) );
               }
               else
               {
                  SELF_GOBOTTOM( (AREAP) thiswa );
               }
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            lFound = searchCacheBWD( thiswa, 1 );
            if( lFound )
            {
               setCurrentFromCache( thiswa, lFound );
               return ConcludeSkipraw( thiswa, lToSkip );
            }
			lPreviousCacheStatus = lPosCache;
            readCachePageBWD( thiswa );
            lFound = searchCacheBWD( thiswa, lPreviousCacheStatus );
            if( lFound )
            {
               setCurrentFromCache( thiswa, lFound );
               return ConcludeSkipraw( thiswa, lToSkip );
            }
            hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
            thiswa->area.fBof = TRUE;
            return ConcludeSkipraw( thiswa, lToSkip );
         }
      }
   }
   else     // ! ISAM
   {
      if( thiswa->aCache->item.asArray.value->ulLen == 0 )
      {
         sqlGetCleanBuffer( thiswa );
         if( lToSkip != 0 )
         {
            thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
            thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
         }
         return ConcludeSkipraw( thiswa, lToSkip );
      }

      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_STABILIZE->pSymbol, 0 );

      if( lToSkip == 0 )
      {
         hb_arraySetL( thiswa->aInfo, AINFO_BOF, bBof );
         hb_arraySetL( thiswa->aInfo, AINFO_EOF, bEof );
         return ConcludeSkipraw( thiswa, lToSkip );
      }
      else if( hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE ) + lToSkip > 0 && hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE ) + lToSkip <= ((LONG) (thiswa->aCache->item.asArray.value->ulLen )))
      {
         sqlGetBufferFromCache2( thiswa, hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE ) + lToSkip );
         hb_arraySetL( thiswa->aInfo, AINFO_BOF, FALSE );
         hb_arraySetL( thiswa->aInfo, AINFO_EOF, FALSE );
         hb_arraySetL( thiswa->aInfo, AINFO_FOUND, FALSE );
         pToSkip = hb_itemNew( NULL );
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_NORMALIZE->pSymbol, 1, pToSkip );
         hb_itemRelease( pToSkip );
         thiswa->area.fBof = FALSE;
         thiswa->area.fEof = FALSE;
         return ConcludeSkipraw( thiswa, lToSkip );
      }
      else if( hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE ) + lToSkip < 1 )
      {
         hb_arraySetNL( thiswa->aInfo, AINFO_NPOSCACHE, 1 );
         sqlGetBufferFromCache2( thiswa, 1 );
         hb_arraySetL( thiswa->aInfo, AINFO_BOF, TRUE );
         hb_arraySetL( thiswa->aInfo, AINFO_EOF, FALSE );
         hb_arraySetL( thiswa->aInfo, AINFO_FOUND, FALSE );
         thiswa->area.fBof = TRUE;
         thiswa->area.fEof = FALSE;
         return ConcludeSkipraw( thiswa, lToSkip );
      }
      else if( hb_arrayGetNL( thiswa->aInfo, AINFO_NPOSCACHE ) + lToSkip > ((LONG) (thiswa->aCache->item.asArray.value->ulLen)) )
      {
         hb_arraySetL( thiswa->aInfo, AINFO_BOF, FALSE );
         sqlGetCleanBuffer( thiswa );
      }
   }
   if( lToSkip == 0 )
   {
      thiswa->area.fBof = bBof;
      thiswa->area.fEof = bEof;
      return HB_SUCCESS;
   }
   if( lToSkip != 0 )
   {
      thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
      thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   }
   return ConcludeSkipraw( thiswa, lToSkip );
}

/*------------------------------------------------------------------------*/

#define sqlAddField    NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlAppend( SQLAREAP thiswa )
{
   HB_ITEM isInsert, goOut, hot;

   // TraceLog( NULL, "sqlAppend\n" );

   isInsert.type = HB_IT_NIL;
   goOut.type    = HB_IT_NIL ;
   hot.type = HB_IT_LOGICAL;

   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   hb_arraySize(thiswa->aLocked, 0);

   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
   }

   thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );

   hot.item.asLogical.value = 1;
   hb_arraySetForward( thiswa->aInfo, AINFO_HOT, &hot );

   hb_itemPutL( &isInsert, 1 );
   hb_itemPutNI( &goOut, 0 );
   hb_arraySetForward( thiswa->aInfo, AINFO_ISINSERT, &isInsert );
   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGOTO->pSymbol, 1, &goOut );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlCreateFields( SQLAREAP thiswa, PHB_ITEM pStruct )
{
   thiswa->aCreate = pStruct;
   return SUPER_CREATEFIELDS( ( AREAP ) thiswa, pStruct );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlDeleteRec( SQLAREAP thiswa )
{
   HB_ITEM hot, isInsert;

   // TraceLog( NULL, "sqlDeleteRec\n" );

   isInsert.type = HB_IT_LOGICAL;
   hot.type = HB_IT_LOGICAL;

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );
   }
   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLDELETEREC->pSymbol, 0 );
   thiswa->wasdel = 1;

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlDeleted( SQLAREAP thiswa, BOOL * isDeleted )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   * isDeleted = hb_arrayGetL( thiswa->aInfo, AINFO_DELETED );

   // TraceLog( NULL, "sqlDeleted, returning %i\n", * isDeleted );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlFieldCount( SQLAREAP thiswa, USHORT * fieldCount )
{
   *fieldCount = thiswa->area.uiFieldCount;
   // TraceLog( NULL, "sqlFieldCount, returning %i\n", thiswa->area.uiFieldCount );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define sqlFieldDisplay       NULL
#define sqlFieldInfo          NULL
#define sqlFieldName          NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlFlush( SQLAREAP thiswa )
{

   // TraceLog( NULL, "sqlFlush\n" );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLFLUSH->pSymbol, 0 );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlGetRec             NULL  /* leave it unUsed */

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGetValue( SQLAREAP thiswa, USHORT fieldNum, PHB_ITEM value  )
{
   PHB_ITEM itemTemp, itemTemp3;
   PHB_ITEM pFieldNum;
   HB_ITEM itemTemp2;
   ULONG ulPos;
   USHORT uiLen;
   LPFIELD pField ;
   itemTemp2.type = HB_IT_NIL;

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }
   pField = thiswa->area.lpFields + fieldNum - 1;
//   TraceLog("campo.txt"," field get campo - %s fieldnum %lu   pField->uiType %lu \n",  hb_dynsymName( ( PHB_DYNS ) pField->sym ),fieldNum,pField->uiType);
   itemTemp = hb_itemArrayGet( thiswa->aBuffer, thiswa->uiBufferIndex[fieldNum - 1] );

   if( HB_IS_NIL( itemTemp ) )
   {
      hb_itemRelease( itemTemp );
      pFieldNum = hb_itemNew( NULL );
      hb_itemPutNI( pFieldNum, thiswa->uiBufferIndex[fieldNum - 1] );
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGETVALUE->pSymbol, 1, pFieldNum );
      hb_itemRelease( pFieldNum );
      itemTemp = hb_itemArrayGet( thiswa->aBuffer, thiswa->uiBufferIndex[fieldNum - 1] );
   }

   if( !thiswa->uiFieldList[fieldNum - 1] )
   {
      hb_arraySetNL( thiswa->aSelectList, thiswa->uiBufferIndex[fieldNum - 1], 1 );
      thiswa->uiFieldList[fieldNum - 1] = 1;
      thiswa->iFieldListStatus          = FIELD_LIST_NEW_VALUE_READ;
   }

   if (HB_IS_ARRAY( itemTemp ))
   {
      itemTemp3 = hb_arrayClone( itemTemp, NULL );
      hb_itemForwardValue( value, itemTemp3 );
      hb_itemRelease( itemTemp3 );
   }
   else if(HB_IS_HASH( itemTemp ) && sr_isMultilang() )
   {
      LPFIELD pField = thiswa->area.lpFields + fieldNum - 1;

      if( pField->uiType == HB_FT_MEMO )
      {
         if( hb_hashScan( itemTemp, sr_getCurrentLang(), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getSecondLang(), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getRootLang(), &ulPos ) )
         {
            hb_itemCopy( value, hb_hashGetValueAt( itemTemp, ulPos ) );
         }
         else
         {
            hb_itemPutCPtr( &itemTemp2, '\0', 0 );
            hb_itemForwardValue( value, &itemTemp2 );
         }
      }
      else
      {
         char * empty = (char *) hb_xgrab( pField->uiLen + 1 );

         if( hb_hashScan( itemTemp, sr_getCurrentLang(), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getSecondLang(), &ulPos ) ||
             hb_hashScan( itemTemp, sr_getRootLang(), &ulPos ) )
         {
            itemTemp3 = hb_hashGetValueAt( itemTemp, ulPos );
            uiLen = (USHORT) hb_itemGetCLen( itemTemp3 );
            hb_xmemcpy( empty, hb_itemGetCPtr( itemTemp3 ), uiLen > pField->uiLen ? pField->uiLen : uiLen );
            if( pField->uiLen > uiLen )
   			{
   			   memset( empty + uiLen, ' ', pField->uiLen - uiLen );
   			}
            empty[pField->uiLen] = '\0';
         }
         else
         {
            memset( empty, ' ', pField->uiLen );
            empty[pField->uiLen] = '\0';
         }
#ifndef HB_CDP_SUPPORT_OFF
         if( pField->uiType == HB_FT_STRING  )
         {
            if( thiswa->cdPageCnv )
               hb_cdpnTranslate( empty, thiswa->area.cdPage, thiswa->cdPageCnv, pField->uiLen );
            else
               hb_cdpnTranslate( empty, thiswa->area.cdPage, hb_cdppage(), pField->uiLen );
         }
#endif
         hb_itemPutCPtr( &itemTemp2, empty, pField->uiLen );
         hb_itemForwardValue( value, &itemTemp2 );
      }
   }
   else
   {
      /*
      if( HB_IS_NIL( itemTemp ) )
      {
         TraceLog( NULL, "Empty buffer found at position %i, fieldpos %i\n", (int)thiswa->uiBufferIndex[fieldNum - 1], (int) fieldNum );
      }
      */
#ifndef HB_CDP_SUPPORT_OFF
      LPFIELD pField = thiswa->area.lpFields + fieldNum - 1;
      if( pField->uiType == HB_FT_STRING  )
      {
         if( thiswa->cdPageCnv )
            hb_cdpnTranslate( hb_itemGetCPtr( itemTemp ), thiswa->area.cdPage, thiswa->cdPageCnv, pField->uiLen );
         else
            hb_cdpnTranslate( hb_itemGetCPtr( itemTemp ), thiswa->area.cdPage, hb_cdppage(), pField->uiLen );
      }
#endif
      hb_itemForwardValue( value, itemTemp );
   }
   hb_itemRelease( itemTemp );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlGetVarLen          NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlGoCold( SQLAREAP thiswa )
{
   if (hb_arrayGetL( thiswa->aInfo, AINFO_HOT ) && (!hb_arrayGetL( thiswa->aInfo, AINFO_DELETED )))
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_WRITEBUFFER->pSymbol, 0 );  // GoCold
   }
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlGoHot              NULL
#define  sqlPutRec             NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlPutValue( SQLAREAP thiswa, USHORT fieldNum, PHB_ITEM value )
{
   PHB_ITEM pDest;
   LPFIELD pField;
   HB_ITEM hot;
   USHORT uiCount;
   char * cfield;
   double dNum;
   USHORT len, dec, fieldindex;
   PHB_ITEM pFieldNum;
   BOOL bOk = TRUE;
   PHB_DYNS s_pSym_SR_FROMXML = NULL;
   // TraceLog( NULL, "sqlPutValue, writing column %i\n", fieldNum );

   hot.type = HB_IT_LOGICAL;

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   fieldindex = (USHORT)thiswa->uiBufferIndex[fieldNum - 1];
   
    pDest  = hb_itemArrayGet( thiswa->aBuffer, fieldindex );
//                if( s_pSym_SR_FROMXML == NULL )
//                {
//                   hb_dynsymLock();
//                   s_pSym_SR_FROMXML = hb_dynsymFindName( "ESCREVE" );
//                   hb_dynsymUnlock();
//                   if ( s_pSym_SR_FROMXML  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
//                }
//                
//                hb_vmPushSymbol( s_pSym_SR_FROMXML->pSymbol );
//                hb_vmPushNil();
//                hb_vmPush(thiswa->aBuffer);
//                hb_vmDo( 1 );
//    
   if( HB_IS_NIL( pDest ) )
   {
      hb_itemRelease( pDest );
      pFieldNum = hb_itemNew( NULL );
      hb_itemPutNI( pFieldNum, thiswa->uiBufferIndex[fieldNum - 1] );
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLGETVALUE->pSymbol, 1, pFieldNum );
      hb_itemRelease( pFieldNum );
      pDest  = hb_itemArrayGet( thiswa->aBuffer, fieldindex );
   }

   if( !thiswa->uiFieldList[fieldNum - 1] )
   {
      hb_arraySetNL( thiswa->aSelectList, thiswa->uiBufferIndex[fieldNum - 1], 1 );
      thiswa->uiFieldList[fieldNum - 1] = 1;
   }

   pField = thiswa->area.lpFields + fieldNum - 1;
//   TraceLog("campo.txt"," campo - %s fieldnum %lu fieldindex %lu  pField->uiType %lu \n",  hb_dynsymName( ( PHB_DYNS ) pField->sym ),fieldNum,fieldindex,pField->uiType);

   /* test compatible datatypes */
   //if  ( HB_IS_TIMEFLAG( value ) )//|| HB_IS_DATE( pDest )) 
   //{	
	   //bOk = FALSE;
       //hb_arraySet( thiswa->aBuffer, fieldindex, value );  
   //}
   if( (HB_IS_NUMBER( pDest ) && HB_IS_NUMBER( value )) || (HB_IS_STRING( pDest ) && HB_IS_STRING( value )) ||
       (HB_IS_LOGICAL( pDest ) && HB_IS_LOGICAL( value )) || (HB_IS_DATE( pDest ) && HB_IS_DATE( value )) || 
        (HB_IS_DATETIME( pDest ) && HB_IS_DATETIME( value )))
   {

      if( pField->uiType == HB_FT_STRING )
      {
         uiCount = ( USHORT ) hb_itemGetCLen( value );

         if( uiCount > pField->uiLen )
         {
            uiCount = pField->uiLen;
         }

         cfield  = (char *) hb_xgrab( pField->uiLen + 1 );
         hb_xmemcpy( cfield, hb_itemGetCPtr( value ), uiCount );
#ifndef HB_CDP_SUPPORT_OFF
         if( thiswa->cdPageCnv )
             hb_cdpnTranslate( (char *) cfield, thiswa->cdPageCnv, thiswa->area.cdPage, uiCount );
         else if( thiswa->area.cdPage )
             hb_cdpnTranslate( (char *) cfield, hb_cdppage(), thiswa->area.cdPage, uiCount );
#endif
         memset( cfield + uiCount, ' ', pField->uiLen - uiCount);
         cfield[pField->uiLen] =  '\0';
         hb_itemPutCPtr( value, cfield, pField->uiLen );
      }
      else if( pField->uiType == HB_FT_LONG )
      {
         len = pField->uiLen;
         dec = pField->uiDec;
         if( dec > 0 )
         {
            len -= (dec + 1);
         }
         dNum = hb_itemGetND( value );
         hb_itemPutNLen( value, dNum, len, dec );
      }

      hb_arraySet( thiswa->aBuffer, fieldindex, value );
   }
   else if(HB_IS_STRING( value ) && HB_IS_HASH( pDest ) && sr_isMultilang() )
   {
      hb_hashAdd( pDest, ULONG_MAX, sr_getCurrentLang(), value );
   }
   else if( pField->uiType == HB_FT_MEMO )    // Memo fields can hold ANY datatype
   {
      hb_arraySet( thiswa->aBuffer, fieldindex, value );
   }
   else
   {
#ifdef SQLRDD_NWG_SPECIFIC
      hot.item.asLogical.value = 1;
      hb_arraySetForward( thiswa->aInfo, AINFO_HOT, &hot );
      return ( HB_SUCCESS );
#else
      char type_err[128];
      sprintf( type_err, "data type origin: %i - data type target %i", value->type, pDest->type );
      commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, type_err );
      return ( HB_FAILURE );
#endif
   }

   hot.item.asLogical.value = 1;
   hb_arraySetForward( thiswa->aInfo, AINFO_HOT, &hot );

   hb_itemRelease( pDest );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlRecall( SQLAREAP thiswa )
{

   // TraceLog( NULL, "sqlRecall\n" );

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLRECALL->pSymbol, 0 );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlRecCount( SQLAREAP thiswa, ULONG * recCount )
{

   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }

   if (hb_arrayGetL( thiswa->aInfo, AINFO_ISINSERT ) && hb_arrayGetL( thiswa->aInfo, AINFO_HOT ) )
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->aInfo, AINFO_RCOUNT ) + 1);
   }
   else
   {
      *recCount = (ULONG) (hb_arrayGetNL( thiswa->aInfo, AINFO_RCOUNT ));
   }

   // TraceLog( NULL, "sqlRecCount, returning %i\n", *recCount );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlRecInfo            NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlRecNo( SQLAREAP thiswa, ULONG * recno )
{
#ifdef SQLRDD_NWG_SPECIFIC
   if( hb_arrayGetNL( thiswa->aInfo, hb_arrayGetL( thiswa->aInfo, AINFO_ISINSERT ) ) )
   {
      commonError( (AREAP) thiswa, EG_ARG, ESQLRDD_NOT_COMMITED_YET, NULL );
      return ( HB_FAILURE );
   }
#endif
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   *recno = (ULONG) hb_arrayGetNL( thiswa->aInfo, AINFO_RECNO );

   // TraceLog( NULL, "sqlRecNo %i\n", hb_arrayGetNI( thiswa->aInfo, AINFO_RECNO ) );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlRecId( SQLAREAP thiswa, PHB_ITEM recno )
{
   if( thiswa->lpdbPendingRel )
   {
      SELF_FORCEREL( ( AREAP ) thiswa );
   }
   else if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if ( thiswa->initialized )
   {
      hb_arrayGet( thiswa->aInfo, AINFO_RECNO, recno );
   }
   else
   {
      hb_itemPutNL( recno, 0 );
   }

   // TraceLog( NULL, "sqlRecID %i\n", hb_arrayGetNI( thiswa->aInfo, AINFO_RECNO ) );

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSetFieldExtent( SQLAREAP thiswa, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("sqlSetFieldExtent(%p, %hu)", thiswa, uiFieldExtent));

   if( SUPER_SETFIELDEXTENT( ( AREAP ) thiswa, uiFieldExtent ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   if( !sr_lHideRecno() )
   {
      uiFieldExtent++;
   }

   if( !sr_lHideHistoric() )
   {
      uiFieldExtent++;
   }

   thiswa->uiBufferIndex = (int *) hb_xgrab( uiFieldExtent * sizeof( int ) );
   thiswa->uiFieldList   = (int *) hb_xgrab( uiFieldExtent * sizeof( int ) );
   memset( thiswa->uiBufferIndex, 0, uiFieldExtent * sizeof( int ) );
   memset( thiswa->uiFieldList,   0, uiFieldExtent * sizeof( int ) );

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

#define  sqlAlias              NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlClose( SQLAREAP thiswa )
{
   HB_ERRCODE uiError;

   // TraceLog( NULL, "sqlClose\n" );

   /* Reset parent rel struct */
   thiswa->lpdbPendingRel = NULL;

   if( thiswa->oWorkArea && HB_IS_OBJECT( thiswa->oWorkArea ) )
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLCLOSE->pSymbol, 0 );
   }
   else
   {
      return SUPER_CLOSE( (AREAP) thiswa );
   }

   uiError = SUPER_CLOSE( (AREAP) thiswa );

/*
   else
   {
      commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
      return HB_FAILURE;
   }
*/
   // Release the used objects

   hb_itemRelease( thiswa->aStruct );
   hb_itemRelease( thiswa->aInfo );
   hb_itemRelease( thiswa->aBuffer );
   hb_itemRelease( thiswa->aOrders );
   hb_itemRelease( thiswa->aSelectList );
   hb_itemRelease( thiswa->aLocked );
   hb_itemRelease( thiswa->aOldBuffer );
   hb_itemRelease( thiswa->aCache );
   hb_itemRelease( thiswa->aEmptyBuff );

   if( thiswa->szDataFileName )
   {
      hb_xfree( thiswa->szDataFileName );
      thiswa->szDataFileName = NULL;
   }

   if( thiswa->uiBufferIndex )
   {
      hb_xfree( thiswa->uiBufferIndex );
      thiswa->uiBufferIndex = NULL;
   }

   if( thiswa->uiFieldList )
   {
      hb_xfree( thiswa->uiFieldList );
      thiswa->uiFieldList = NULL;
   }

   if( thiswa->oWorkArea && HB_IS_OBJECT( thiswa->oWorkArea ) )
   {
      hb_itemRelease( thiswa->oWorkArea );
      thiswa->oWorkArea = NULL;
   }
   return uiError;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlCreate( SQLAREAP thiswa, LPDBOPENINFO pCreateInfo )
{
   PHB_ITEM pTable = hb_itemNew( NULL );
   PHB_ITEM pAlias = hb_itemNew( NULL );
   PHB_ITEM pArea = hb_itemNew( NULL );
   HB_ERRCODE errCode;

   // TraceLog( NULL, "sqlCreate(%p, %p)", thiswa, pCreateInfo);

   thiswa->creating = 1;

   thiswa->szDataFileName = ( char * ) hb_xgrab( strlen( (char * ) pCreateInfo->abName ) + 1 );
   strcpy( thiswa->szDataFileName, ( char * ) pCreateInfo->abName );

   pTable = hb_itemPutC( pTable, thiswa->szDataFileName );
   if( pCreateInfo->atomAlias )
      hb_itemPutC( pAlias, ( char * ) pCreateInfo->atomAlias );
   hb_itemPutNL( pArea, pCreateInfo->uiArea );

   thiswa->area.fTop    = 0;
   thiswa->area.fBottom = 1;
   thiswa->area.fBof    = 0;
   thiswa->area.fEof    = 1;

//   thiswa->valResult   = NULL;

   /* Create and run class object */
   hb_vmPushSymbol( s_pSym_WORKAREA->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   thiswa->oWorkArea = hb_itemNew( NULL );
   hb_itemCopy( thiswa->oWorkArea, hb_stackReturnItem() );
   if (s_pSym_SQLCREATE->pSymbol)
   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLCREATE->pSymbol, 4, thiswa->aCreate, pTable, pAlias, pArea );

   hb_objSendMsg( thiswa->oWorkArea, "AINFO", 0 );
   thiswa->aInfo = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ACACHE", 0 );
   thiswa->aCache = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AOLDBUFFER", 0 );
   thiswa->aOldBuffer = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ALOCALBUFFER", 0 );
   thiswa->aBuffer = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AEMPTYBUFFER", 0 );
   thiswa->aEmptyBuff = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AINDEX", 0 );
   thiswa->aOrders = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ASELECTLIST", 0 );
   thiswa->aSelectList = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AINIFIELDS", 0 );
   thiswa->aStruct = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ALOCKED", 0 );
   thiswa->aLocked = hb_itemNew( hb_stackReturnItem() );

   hb_itemRelease( pTable );
   hb_itemRelease( pAlias );
   hb_itemRelease( pArea );

   hb_objSendMsg( thiswa->oWorkArea, "LOPENED", 0 );

   if( ! hb_itemGetL( hb_stackReturnItem() ) )
   {
      return( HB_FAILURE );
   }

   if( ! SetFields( thiswa ) )
   {
      return( HB_FAILURE );
   }

   /* If successful call SUPER_CREATE to finish system jobs */
   errCode = SUPER_CREATE( ( AREAP ) thiswa, pCreateInfo );

   hb_objSendMsg( thiswa->oWorkArea, "LISAM", 0 );
   thiswa->isam = hb_itemGetL( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "HNRECNO", 0 );
   thiswa->ulhRecno = (ULONG) hb_itemGetNL( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "HNDELETED", 0 );
   thiswa->ulhDeleted = (ULONG) hb_itemGetNL( hb_stackReturnItem() );

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) thiswa );
      return errCode;
   }
   else
   {
      SELF_GOTOP( ( AREAP ) thiswa );
      thiswa->firstinteract = 0;
   }

   thiswa->wasdel = 0;

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlInfo( SQLAREAP thiswa, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL flag = TRUE;

   // TraceLog( NULL, "sqlInfo(%p, %hu, %p)", thiswa, uiIndex, pItem);

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, FALSE );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, 0 );
         break;

      case DBI_LASTUPDATE:
         hb_itemPutD( pItem, 2003, 12, 31 );
         break;

      case DBI_GETDELIMITER:
      case DBI_SETDELIMITER:
         break;

      case DBI_GETRECSIZE:
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLRECSIZE->pSymbol, 0 );
         hb_itemPutNI( pItem, hb_itemGetNI( hb_stackReturnItem() ) );
         break;

      case DBI_GETLOCKARRAY:
         hb_itemCopy( pItem, thiswa->aLocked );
         break;

      case DBI_TABLEEXT:
         hb_itemPutC( pItem, "" );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, "" );
         break;

      case DBI_MEMOBLOCKSIZE:
         hb_itemPutNI( pItem, 0 );
         break;

      case DBI_FULLPATH:
         hb_itemPutC( pItem, thiswa->szDataFileName);
         break;

      case DBI_FILEHANDLE:
         hb_itemPutNL( pItem, 0 );
         break;

      case DBI_BOF:
         SELF_BOF( (AREAP) thiswa, &flag );
         hb_itemPutL( pItem, flag );
         break;

      case DBI_EOF:
         SELF_EOF( (AREAP) thiswa, &flag );
         hb_itemPutL( pItem, flag );
         break;

      case DBI_ISFLOCK:
         if( HB_IS_OBJECT( thiswa->oWorkArea ) )
         {
            hb_objSendMsg( thiswa->oWorkArea, "LTABLELOCKED", 0 );
         }
         else
         {
            commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
            return HB_FAILURE;
         }
         hb_itemForwardValue( pItem, hb_stackReturnItem() );
         break;

      case DBI_MEMOHANDLE:
         hb_itemPutNL( pItem, 0 );
         break;

      case DBI_RDD_BUILD:
         hb_itemPutNL( pItem, HB_SQLRDD_BUILD );
         break;

      case DBI_RDD_VERSION:
         if( HB_IS_OBJECT( thiswa->oWorkArea ) )
         {
            hb_objSendMsg( thiswa->oWorkArea, "OSQL", 0 );
            hb_objSendMsg( hb_stackReturnItem(), "CMGMNTVERS", 0 );
         }
         else
         {
            commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
            return HB_FAILURE;
         }
         hb_itemForwardValue( pItem, hb_stackReturnItem() );
         break;

      case DBI_DB_VERSION:
         if( HB_IS_OBJECT( thiswa->oWorkArea ) )
         {
            hb_objSendMsg( thiswa->oWorkArea, "OSQL", 0 );
            hb_objSendMsg( hb_stackReturnItem(), "NSYSTEMID", 0 );
         }
         else
         {
            commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
            return HB_FAILURE;
         }
         hb_itemForwardValue( pItem, hb_stackReturnItem() );
         break;

      case DBI_INTERNAL_OBJECT:
         hb_itemCopy( pItem, thiswa->oWorkArea );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, thiswa->readonly );
         break;

      case DBI_SHARED:
         hb_itemPutL( pItem, thiswa->shared );
         break;

      case DBI_CPCONVERTTO:
         if( HB_IS_STRING( pItem ) )
         {
            PHB_CODEPAGE cdpage = hb_cdpFind( hb_itemGetCPtr( pItem ) );
            if ( cdpage )
               thiswa->cdPageCnv = cdpage;
         }
         if ( thiswa->cdPageCnv )
            hb_itemPutC( pItem, ( char * ) thiswa->cdPageCnv->id );
         else
            hb_itemPutC( pItem, "" );
         break;
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

void startSQLRDDSymbols()
{
   HB_THREAD_STUB

   if (s_pSym_WORKAREA == NULL)
   {

      hb_dynsymLock();

      s_pSym_WORKAREA            = hb_dynsymFindName( WORKAREA_CLASS );

      s_pSym_SQLGOBOTTOM         = hb_dynsymFindName( "SQLGOBOTTOM" );
      s_pSym_SQLGOTO             = hb_dynsymFindName( "SQLGOTO" );
      s_pSym_SQLGOTOP            = hb_dynsymFindName( "SQLGOTOP" );
      s_pSym_SQLSEEK             = hb_dynsymFindName( "SQLSEEK" );
      s_pSym_SETBOF              = hb_dynsymFindName( "SETBOF" );
      s_pSym_SQLDELETEREC        = hb_dynsymFindName( "SQLDELETEREC" );
      s_pSym_SQLFLUSH            = hb_dynsymFindName( "SQLFLUSH" );
      s_pSym_SQLRECALL           = hb_dynsymFindName( "SQLRECALL" );
      s_pSym_SQLCLOSE            = hb_dynsymFindName( "SQLCLOSE" );
      s_pSym_SQLCREATE           = hb_dynsymFindName( "SQLCREATE" );
      s_pSym_SQLOPEN             = hb_dynsymFindName( "SQLOPENAREA" );
      s_pSym_SQLOPENALLINDEXES   = hb_dynsymFindName( "SQLOPENALLINDEXES" );
      s_pSym_SQLPACK             = hb_dynsymFindName( "SQLPACK" );
      s_pSym_SQLZAP              = hb_dynsymFindName( "SQLZAP" );
      s_pSym_SQLORDERLISTADD     = hb_dynsymFindName( "SQLORDERLISTADD" );
      s_pSym_SQLORDERLISTCLEAR   = hb_dynsymFindName( "SQLORDERLISTCLEAR" );
      s_pSym_SQLORDERLISTFOCUS   = hb_dynsymFindName( "SQLORDERLISTFOCUS" );
      s_pSym_SQLORDERCREATE      = hb_dynsymFindName( "SQLORDERCREATE" );
      s_pSym_SQLORDERDESTROY     = hb_dynsymFindName( "SQLORDERDESTROY");
      s_pSym_SQLORDERCONDITION   = hb_dynsymFindName( "SQLORDERCONDITION" );
      s_pSym_SQLORDERLISTNUM     = hb_dynsymFindName( "SQLORDERLISTNUM" );
      s_pSym_SQLSETSCOPE         = hb_dynsymFindName( "SQLSETSCOPE" );
      s_pSym_SQLLOCK             = hb_dynsymFindName( "SQLLOCK" );
      s_pSym_SQLUNLOCK           = hb_dynsymFindName( "SQLUNLOCK" );
      s_pSym_SQLDROP             = hb_dynsymFindName( "SQLDROP" );
      s_pSym_SQLEXISTS           = hb_dynsymFindName( "SQLEXISTS" );
      s_pSym_WRITEBUFFER         = hb_dynsymFindName( "WRITEBUFFER" );
      s_pSym_READPAGE            = hb_dynsymFindName( "READPAGE" );
      s_pSym_STABILIZE           = hb_dynsymFindName( "STABILIZE" );
      s_pSym_NORMALIZE           = hb_dynsymFindName( "NORMALIZE" );
      s_pSym_SQLKEYCOUNT         = hb_dynsymFindName( "SQLKEYCOUNT" );
      s_pSym_SQLRECSIZE          = hb_dynsymFindName( "SQLRECSIZE" );
      s_pSym_SQLGETVALUE         = hb_dynsymFindName( "SQLGETVALUE" );
      s_pSym_SQLSETFILTER        = hb_dynsymFindName( "SQLSETFILTER" );
      s_pSym_SQLCLEARFILTER      = hb_dynsymFindName( "SQLCLEARFILTER" );
      s_pSym_SQLFILTERTEXT       = hb_dynsymFindName( "SQLFILTERTEXT" );

      if ( s_pSym_WORKAREA            == NULL ) printf( "Could not find Symbol %s\n", WORKAREA_CLASS );

      if ( s_pSym_SQLGOBOTTOM         == NULL ) printf( "Could not find Symbol %s\n", "SQLGOBOTTOM" );
      if ( s_pSym_SQLGOTO             == NULL ) printf( "Could not find Symbol %s\n", "SQLGOTO" );
      if ( s_pSym_SQLGOTOP            == NULL ) printf( "Could not find Symbol %s\n", "SQLGOTOP" );
      if ( s_pSym_SQLSEEK             == NULL ) printf( "Could not find Symbol %s\n", "SQLSEEK" );
      if ( s_pSym_SETBOF              == NULL ) printf( "Could not find Symbol %s\n", "SETBOF" );
      if ( s_pSym_SQLDELETEREC        == NULL ) printf( "Could not find Symbol %s\n", "SQLDELETEREC" );
      if ( s_pSym_SQLFLUSH            == NULL ) printf( "Could not find Symbol %s\n", "SQLFLUSH" );
      if ( s_pSym_SQLRECALL           == NULL ) printf( "Could not find Symbol %s\n", "SQLRECALL" );
      if ( s_pSym_SQLCLOSE            == NULL ) printf( "Could not find Symbol %s\n", "SQLCLOSE" );
      if ( s_pSym_SQLCREATE           == NULL ) printf( "Could not find Symbol %s\n", "SQLCREATE" );
      if ( s_pSym_SQLOPEN             == NULL ) printf( "Could not find Symbol %s\n", "SQLOPENAREA" );
      if ( s_pSym_SQLOPENALLINDEXES   == NULL ) printf( "Could not find Symbol %s\n", "SQLOPENALLINDEXES" );
      if ( s_pSym_SQLPACK             == NULL ) printf( "Could not find Symbol %s\n", "SQLPACK" );
      if ( s_pSym_SQLZAP              == NULL ) printf( "Could not find Symbol %s\n", "SQLZAP" );
      if ( s_pSym_SQLORDERLISTADD     == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERLISTADD" );
      if ( s_pSym_SQLORDERLISTCLEAR   == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERLISTCLEAR" );
      if ( s_pSym_SQLORDERLISTFOCUS   == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERLISTFOCUS" );
      if ( s_pSym_SQLORDERCREATE      == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERCREATE" );
      if ( s_pSym_SQLORDERDESTROY     == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERDESTROY" );
      if ( s_pSym_SQLORDERCONDITION   == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERCONDITION" );
      if ( s_pSym_SQLORDERLISTNUM     == NULL ) printf( "Could not find Symbol %s\n", "SQLORDERLISTNUM" );
      if ( s_pSym_SQLSETSCOPE         == NULL ) printf( "Could not find Symbol %s\n", "SQLSETSCOPE" );
      if ( s_pSym_SQLLOCK             == NULL ) printf( "Could not find Symbol %s\n", "SQLLOCK" );
      if ( s_pSym_SQLUNLOCK           == NULL ) printf( "Could not find Symbol %s\n", "SQLUNLOCK" );
      if ( s_pSym_SQLDROP             == NULL ) printf( "Could not find Symbol %s\n", "SQLDROP" );
      if ( s_pSym_SQLEXISTS           == NULL ) printf( "Could not find Symbol %s\n", "SQLEXISTS" );
      if ( s_pSym_SQLKEYCOUNT         == NULL ) printf( "Could not find Symbol %s\n", "SQLKEYCOUNT" );
      if ( s_pSym_WRITEBUFFER         == NULL ) printf( "Could not find Symbol %s\n", "WRITEBUFFER" );
      if ( s_pSym_READPAGE            == NULL ) printf( "Could not find Symbol %s\n", "READPAGE" );
      if ( s_pSym_STABILIZE           == NULL ) printf( "Could not find Symbol %s\n", "STABILIZE" );
      if ( s_pSym_NORMALIZE           == NULL ) printf( "Could not find Symbol %s\n", "NORMALIZE" );
      if ( s_pSym_SQLGETVALUE         == NULL ) printf( "Could not find Symbol %s\n", "SQLGETVALUE" );
      if ( s_pSym_SQLSETFILTER        == NULL ) printf( "Could not find Symbol %s\n", "SQLSETFILTER" );
      if ( s_pSym_SQLCLEARFILTER      == NULL ) printf( "Could not find Symbol %s\n", "SQLCLEARFILTER" );
      if ( s_pSym_SQLFILTERTEXT       == NULL ) printf( "Could not find Symbol %s\n", "SQLFILTERTEXT" );

      hb_dynsymUnlock();
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlNewArea( SQLAREAP thiswa )
{
   if( SUPER_NEW( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return HB_FAILURE;
   }

   thiswa->uiBufferIndex = NULL;
   thiswa->uiFieldList   = NULL;
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOpen( SQLAREAP thiswa, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pConnection = hb_itemNew( NULL );
   PHB_ITEM pTable = hb_itemNew( NULL );
   PHB_ITEM pArea = hb_itemNew( NULL );
   PHB_ITEM pAlias = hb_itemNew( NULL );
   PHB_ITEM pShared = hb_itemNew( NULL );
   PHB_ITEM pReadOnly = hb_itemNew( NULL );
   HB_ERRCODE errCode;

   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   // TraceLog( NULL, "sqlOpen\n" );

   thiswa->szDataFileName = (char *) hb_xgrab( strlen( (char *) pOpenInfo->abName ) + 1 );
   strcpy( thiswa->szDataFileName, ( char * ) pOpenInfo->abName );

   /* Create default alias if necessary */
   if( ! pOpenInfo->atomAlias )
   {
      PHB_FNAME pFileName;
      pFileName = hb_fsFNameSplit( ( char * ) pOpenInfo->abName );
      hb_strncpyUpperTrim( szAlias, pFileName->szName, HB_RDD_MAX_ALIAS_LEN );
      pOpenInfo->atomAlias = szAlias;
      hb_xfree( pFileName );
   }

   errCode = SUPER_OPEN( ( AREAP ) thiswa, pOpenInfo );

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) thiswa );
      return errCode;
   }

   thiswa->shared      = pOpenInfo->fShared;
   thiswa->readonly    = pOpenInfo->fReadonly;
   thiswa->hOrdCurrent = 0;
   thiswa->creating    = 0;
   thiswa->initialized = 0;
   thiswa->sqlfilter   = 0;

   thiswa->area.uiMaxFieldNameLength = 64;

   hb_itemPutNL( pConnection, pOpenInfo->ulConnection );
   hb_itemPutC( pTable, thiswa->szDataFileName );
   hb_itemPutNL( pArea, pOpenInfo->uiArea );
   hb_itemPutL( pShared, thiswa->shared );
   hb_itemPutL( pReadOnly, thiswa->readonly );
   hb_itemPutC( pAlias, ( char * ) pOpenInfo->atomAlias );

#ifndef HB_CDP_SUPPORT_OFF
   if( pOpenInfo->cdpId )
   {
      thiswa->area.cdPage = hb_cdpFind( (char *) pOpenInfo->cdpId );
      if( !thiswa->area.cdPage )
         thiswa->area.cdPage = hb_cdppage();
   }
   else
      thiswa->area.cdPage = hb_cdppage();
#endif

   hb_vmPushSymbol( s_pSym_WORKAREA->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );
   thiswa->oWorkArea = hb_itemNew( hb_stackReturnItem() );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLOPEN->pSymbol, 6, pTable, pArea, pShared, pReadOnly, pAlias, pConnection );

   hb_objSendMsg( thiswa->oWorkArea, "AINFO", 0 );
   thiswa->aInfo = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ACACHE", 0 );
   thiswa->aCache = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AOLDBUFFER", 0 );
   thiswa->aOldBuffer = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ALOCALBUFFER", 0 );
   thiswa->aBuffer = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AEMPTYBUFFER", 0 );
   thiswa->aEmptyBuff = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AINDEX", 0 );
   thiswa->aOrders = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ASELECTLIST", 0 );
   thiswa->aSelectList = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "AINIFIELDS", 0 );
   thiswa->aStruct = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "ALOCKED", 0 );
   thiswa->aLocked = hb_itemNew( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "LGOTOPONFIRSTINTERACT", 0 );
   thiswa->firstinteract  = hb_itemGetL( hb_stackReturnItem() );

   thiswa->initialized = 1;
   thiswa->wasdel = 0;

   hb_itemRelease( pTable );
   hb_itemRelease( pArea );
   hb_itemRelease( pAlias );
   hb_itemRelease( pShared );
   hb_itemRelease( pReadOnly );
   hb_itemRelease( pConnection );

   hb_objSendMsg( thiswa->oWorkArea, "LOPENED", 0 );

   if( ! hb_itemGetL( hb_stackReturnItem() ) )
   {
      return( HB_FAILURE );
   }

   hb_objSendMsg( thiswa->oWorkArea, "LISAM", 0 );
   thiswa->isam = hb_itemGetL( hb_stackReturnItem() );

   if( ! ProcessFields( thiswa ) )
   {
      return( HB_FAILURE );
   }

   hb_objSendMsg( thiswa->oWorkArea, "HNRECNO", 0 );
   thiswa->ulhRecno = (ULONG) hb_itemGetNL( hb_stackReturnItem() );

   hb_objSendMsg( thiswa->oWorkArea, "HNDELETED", 0 );
   thiswa->ulhDeleted = (ULONG) hb_itemGetNL( hb_stackReturnItem() );

   if( hb_setGetAutOpen() )
   {
      if( HB_IS_OBJECT( thiswa->oWorkArea ) )
      {
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLOPENALLINDEXES->pSymbol, 0 );
      }
      else
      {
         commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
         return HB_FAILURE;
      }

      if( hb_itemGetNL( hb_stackReturnItem() ) )
      {
         thiswa->hOrdCurrent = hb_itemGetNL( hb_stackReturnItem() );
      }
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

#define  sqlRelease            NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlStructSize( SQLAREAP thiswa, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( thiswa );     /* Avoid compiler warning */
   *StructSize = sizeof( SQLAREA );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlSysName            NULL
#define  sqlEval               NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlPack( SQLAREAP thiswa )
{

   // TraceLog( NULL, "sqlPack\n" );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLPACK->pSymbol, 0 );
   SELF_GOTOP( (AREAP) thiswa );
   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlPackRec            NULL
#define  sqlSort               NULL
#define  sqlTrans              NULL
#define  sqlTransRec           NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlZap( SQLAREAP thiswa )
{

   // TraceLog( NULL, "sqlZap\n" );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLZAP->pSymbol, 0 );

   thiswa->area.fEof = hb_arrayGetL( thiswa->aInfo, AINFO_EOF );
   thiswa->area.fBof = hb_arrayGetL( thiswa->aInfo, AINFO_BOF );
   thiswa->firstinteract = 0;
   thiswa->wasdel = 0;

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlChildEnd( SQLAREAP thiswa, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE uiError;

   // TraceLog( NULL, "sqlChildEnd\n" );

   HB_TRACE(HB_TR_DEBUG, ("sqlChildEnd(%p, %p)", thiswa, pRelInfo));

   if( thiswa->lpdbPendingRel == pRelInfo )
      uiError = SELF_FORCEREL( ( AREAP ) thiswa );
   else
      uiError = HB_SUCCESS;
   SUPER_CHILDEND( ( AREAP ) thiswa, pRelInfo );
   return uiError;
}

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlChildStart( SQLAREAP thiswa, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sqlChildStart(%p, %p)", thiswa, pRelInfo));

   // TraceLog( NULL, "sqlChildStart\n" );

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   SELF_CHILDSYNC( ( AREAP ) thiswa, pRelInfo );
   return SUPER_CHILDSTART( ( AREAP ) thiswa, pRelInfo );
}

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlChildSync( SQLAREAP thiswa, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sqlChildSync(%p, %p)", thiswa, pRelInfo));

   // TraceLog( NULL, "sqlChildSync\n" );

   thiswa->lpdbPendingRel = pRelInfo;
   SELF_SYNCCHILDREN( ( AREAP ) thiswa );

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

#define  sqlSyncChildren       NULL

/*------------------------------------------------------------------------*/

#define  sqlClearRel           NULL

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlForceRel( SQLAREAP thiswa )
{
   LPDBRELINFO lpdbPendingRel;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("sqlForceRel(%p)", thiswa));

   if( thiswa->lpdbPendingRel )
   {
      lpdbPendingRel = thiswa->lpdbPendingRel;
      thiswa->lpdbPendingRel = NULL;
      uiError = SELF_RELEVAL( ( AREAP ) thiswa, lpdbPendingRel );
      thiswa->firstinteract = 0;
      thiswa->wasdel = 0;
      return uiError;
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

#define  sqlRelArea            NULL
#define  sqlRelEval            NULL
#define  sqlRelText            NULL
#define  sqlSetRel             NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderListAdd( SQLAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ITEM Index, Tag;

   // TraceLog( NULL, "sqlOrderListAdd\n" );

   Index.type = HB_IT_NIL;
   Tag.type   = HB_IT_NIL;

   hb_itemCopy( &Index, pOrderInfo->atomBagName );

   if( pOrderInfo->itmOrder )
   {
      hb_itemCopy( &Tag, pOrderInfo->itmOrder );
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTADD->pSymbol, 2, &Index, &Tag );

   hb_itemClear( &Tag );
   hb_itemClear( &Index );

   if( hb_itemGetNL( hb_stackReturnItem() ) )
   {
      thiswa->hOrdCurrent = hb_itemGetNL( hb_stackReturnItem() );
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderListClear( SQLAREAP thiswa )
{

   // TraceLog( NULL, "sqlOrderListClear\n" );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTCLEAR->pSymbol, 0 );

   if( hb_itemGetL( hb_stackReturnItem() ) )
   {
      thiswa->hOrdCurrent = 0;
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/

#define  sqlOrderListDelete    NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderListFocus( SQLAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
//   HB_ITEM BagName;
   PHB_ITEM BagName;
   PHB_ITEM pTag;
   LONG lorder  = 0;

   // TraceLog( NULL, "sqlOrderListFocus\n" );

   //BagName.type = HB_IT_NIL;
   pTag = loadTagDefault( thiswa, NULL, &lorder );

   if( pTag )
   {
      hb_arrayGet( pTag, ORDER_TAG, pOrderInfo->itmResult );
      hb_itemRelease( pTag );
   }
   else
   {
      hb_itemPutC( pOrderInfo->itmResult, "" );
   }

   if( pOrderInfo->itmOrder )
   {
	  BagName = hb_itemNew( NULL );
      if( pOrderInfo->atomBagName )
      {
         hb_itemCopy( BagName, pOrderInfo->atomBagName );
      }

      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTFOCUS->pSymbol, 2, pOrderInfo->itmOrder, BagName );
      hb_itemClear( BagName );

      thiswa->hOrdCurrent = hb_itemGetNL( hb_stackReturnItem() );
      return HB_SUCCESS;
   }
   else
   {
      return HB_SUCCESS;
   }

}

/*------------------------------------------------------------------------*/

#define  sqlOrderListRebuild   NULL

/*------------------------------------------------------------------------*/

HB_ERRCODE sqlOrderCondition( SQLAREAP thiswa, LPDBORDERCONDINFO lpdbOrdCondInfo )
{
   HB_ITEM itemFor, itemWhile;
   HB_ITEM itemNext, itemRest, itemDesc, itemStart;
   PHB_ITEM pitemRecno;
   itemFor.type   = HB_IT_NIL;
   itemWhile.type = HB_IT_NIL;
   itemNext.type  = HB_IT_NIL;
   itemRest.type  = HB_IT_NIL;
   itemDesc.type  = HB_IT_NIL;
   itemStart.type = HB_IT_NIL;

   pitemRecno = hb_itemNew( NULL );

   if( lpdbOrdCondInfo )
   {
      if( lpdbOrdCondInfo->abFor )
      {
         hb_itemPutC( &itemFor, (char *) lpdbOrdCondInfo->abFor );
      }
      if( lpdbOrdCondInfo->abWhile )
      {
         hb_itemPutC( &itemWhile, (char *) lpdbOrdCondInfo->abWhile );
      }
	  if (lpdbOrdCondInfo->itmStartRecID)
	  {
         hb_itemPutNL( &itemStart, hb_itemGetNL( lpdbOrdCondInfo->itmStartRecID ) );
	  }
      hb_itemPutNL( &itemNext,  lpdbOrdCondInfo->lNextCount );
	  if( lpdbOrdCondInfo->itmRecID )
	  {
//         hb_itemPutNL( &itemRecno, hb_itemGetNL( lpdbOrdCondInfo->itmRecID ) );
         hb_itemCopy( pitemRecno, lpdbOrdCondInfo->itmRecID );
	  }
      hb_itemPutL( &itemRest,  lpdbOrdCondInfo->fRest );
      hb_itemPutL( &itemDesc,  lpdbOrdCondInfo->fDescending );
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERCONDITION->pSymbol, 7, &itemFor, &itemWhile,
                     &itemStart, &itemNext, pitemRecno, &itemRest, &itemDesc );
   hb_itemClear( &itemWhile );
   hb_itemClear( &itemFor );
   hb_itemRelease( pitemRecno );

   SUPER_ORDSETCOND( (AREAP) thiswa, lpdbOrdCondInfo );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderCreate( SQLAREAP thiswa, LPDBORDERCREATEINFO pOrderInfo )
{
   HB_ITEM BagName, AtomBagName, ConstrName, Target, Enable;

   // TraceLog( NULL, "sqlOrderCreate\n" );

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return( HB_FAILURE );
   }

   BagName.type = HB_IT_NIL;
   hb_itemPutC( &BagName, (char *) pOrderInfo->abBagName );

   AtomBagName.type = HB_IT_NIL;
   hb_itemPutC( &AtomBagName, (char *) pOrderInfo->atomBagName );

   if (pOrderInfo->lpdbConstraintInfo)
   {
      ConstrName.type = HB_IT_NIL;
      Target.type = HB_IT_NIL;
      Enable.type = HB_IT_NIL;

      hb_itemPutC( &ConstrName, (char *) pOrderInfo->lpdbConstraintInfo->abConstrName );
      hb_itemPutC( &Target, (char *) pOrderInfo->lpdbConstraintInfo->abTargetName );
      hb_itemPutL( &Enable, pOrderInfo->lpdbConstraintInfo->fEnabled );

      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERCREATE->pSymbol, 7, &BagName, pOrderInfo->abExpr, &AtomBagName, &ConstrName, &Target, pOrderInfo->lpdbConstraintInfo->itmRelationKey, &Enable );

      hb_itemClear( &ConstrName );
      hb_itemClear( &Target );
      hb_itemClear( &Enable );

   }
   else
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERCREATE->pSymbol, 3, &BagName, pOrderInfo->abExpr, &AtomBagName );
   }

   hb_itemClear( &BagName );
   hb_itemClear( &AtomBagName );

   if( hb_itemGetNL( hb_stackReturnItem() ) )
   {
      thiswa->hOrdCurrent = hb_itemGetNL( hb_stackReturnItem() );
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderDestroy( SQLAREAP thiswa, LPDBORDERINFO pOrderInfo )
{
   HB_ITEM BagName;
   PHB_ITEM pTag;
   LONG lorder  = 0;

   // TraceLog( NULL, "sqlOrderDestroy\n" );

   if( SELF_GOCOLD( ( AREAP ) thiswa ) == HB_FAILURE )
   {
      return( HB_FAILURE );
   }

   BagName.type = HB_IT_NIL;
   pTag = loadTagDefault( thiswa, NULL, &lorder );

   if( !pTag )
   {
      return HB_FAILURE;
   }

   if( pOrderInfo->itmOrder )
   {
      if( pOrderInfo->atomBagName )
      {
         hb_itemCopy( &BagName, pOrderInfo->atomBagName );
      }

      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERDESTROY->pSymbol, 2, pOrderInfo->itmOrder, &BagName );
      hb_itemClear( &BagName );

      if( hb_itemGetNL( hb_stackReturnItem() ) )
      {
         thiswa->hOrdCurrent = hb_itemGetNL( hb_stackReturnItem() );
         return HB_SUCCESS;
      }
   }
   else
   {
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

/*------------------------------------------------------------------------*/
/*
static PHB_ITEM loadTag( SQLAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder )
{
   PHB_ITEM pTag = NULL;

   if (pInfo && pInfo->itmOrder )
   {
      if( HB_IS_OBJECT( thiswa->oWorkArea ) )
      {
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTNUM->pSymbol, 1, pInfo->itmOrder );
      }
      else
      {
         commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
         return NULL;
      }

      if( hb_itemGetNL( hb_stackReturnItem() ) )
      {
         * lorder = hb_itemGetNL( hb_stackReturnItem() );
         pTag = hb_itemArrayGet( thiswa->aOrders, (ULONG) * lorder );
      }
   }
   return pTag;
}
*/
/*------------------------------------------------------------------------*/

PHB_ITEM loadTagDefault( SQLAREAP thiswa, LPDBORDERINFO pInfo, LONG * lorder )
{
   //HB_ITEM Order;
   PHB_ITEM Order = hb_itemNew(NULL);
   PHB_ITEM pTag = NULL;

//   Order.type = HB_IT_NIL;

   if( pInfo )
   {
      if( pInfo->itmOrder )
      {
         if( HB_IS_OBJECT( thiswa->oWorkArea ) )
         {
            hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTNUM->pSymbol, 1, pInfo->itmOrder );
         }
         else
         {
            commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
            return NULL;
         }
      }
      else
      {
         if( HB_IS_OBJECT( thiswa->oWorkArea ) )
         {
            hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTNUM->pSymbol, 1, Order );
         }
         else
         {
            commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
            return NULL;
         }
      }
   }
   else
   {
      if( HB_IS_OBJECT( thiswa->oWorkArea ) )
      {
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTNUM->pSymbol, 1, Order );
      }
      else
      {
         commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
         return NULL;
      }
   }

   * lorder = hb_itemGetNL( hb_stackReturnItem() );

   if( * lorder )
   {
      pTag = hb_itemArrayGet( thiswa->aOrders, (ULONG) * lorder );
   }

   return pTag;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSetServerSideIndexScope( SQLAREAP thiswa, int nScope, PHB_ITEM scopeValue )
{
   HB_ITEM scopetype;
   PHB_ITEM scopeval;
   int res;

   scopeval = hb_itemNew( NULL );

   scopetype.type = HB_IT_INTEGER;
   scopetype.item.asInteger.value  = nScope;
   scopetype.item.asInteger.length = 10;

   if ( scopeValue )
   {
      hb_itemCopy( scopeval, scopeValue );
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLSETSCOPE->pSymbol, 2, &scopetype, scopeval );
   res = hb_itemGetNI( hb_stackReturnItem() );
   hb_itemRelease( scopeval );

   if ((!res) && sr_GoTopOnScope() )
   {
      thiswa->firstinteract = 1;
   }

   return ( res == 0 ? HB_SUCCESS : HB_FAILURE );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlOrderInfo( SQLAREAP thiswa, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LONG lIndexes, lorder = 0;
   PHB_ITEM pTag, pTemp;
   HB_ITEM ScopeNull;
   HB_MACRO_PTR pMacro;

   // TraceLog( NULL, "sqlOrderInfo, order: %i\n", uiIndex );

   ScopeNull.type = HB_IT_NIL;

   HB_TRACE(HB_TR_DEBUG, ("sqlOrderInfo(%p, %hu, %p)", thiswa, uiIndex, pInfo));

   switch( uiIndex )
   {
      case DBOI_BAGEXT:
         hb_itemPutC( pInfo->itmResult, "" );
         return HB_SUCCESS;
   }

   lIndexes = hb_itemSize( thiswa->aOrders );

   if( lIndexes )      /* Exists opened orders ? */
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNTRAW:
         {
            ULONG ulRecCount = 0;
            SELF_RECCOUNT( ( AREAP ) thiswa, &ulRecCount );
            hb_itemPutNI( pInfo->itmResult, (int) ulRecCount );
            break;
         }
         case DBOI_KEYCOUNT:
            hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLKEYCOUNT->pSymbol, 0 );
            hb_itemPutNI( pInfo->itmResult, hb_itemGetNI( hb_stackReturnItem() ) );
            break;

         case DBOI_CONDITION:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               pTemp = hb_itemArrayGet( pTag, FOR_CLAUSE );
               hb_itemCopy( pInfo->itmResult, pTemp );
               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutC( pInfo->itmResult, "" );
            }
            break;

         case DBOI_EXPRESSION:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );

            if( pTag )
            {
               pTemp = hb_itemArrayGet( pTag, INDEX_KEY );
               hb_itemCopy( pInfo->itmResult , pTemp );

               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutC( pInfo->itmResult, "" );
            }
            break;

         case DBOI_NUMBER:
            hb_itemPutNILen( pInfo->itmResult, hb_arrayGetNI( thiswa->aInfo, AINFO_INDEXORD ), 3 );
            break;

         case DBOI_BAGNAME:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               pTemp = hb_itemArrayGet( pTag, ORDER_NAME );
               hb_itemCopy( pInfo->itmResult , pTemp );
               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutC( pInfo->itmResult, "" );
            }
            break;

         case DBOI_NAME:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               pTemp = hb_itemArrayGet( pTag, ORDER_TAG );
               hb_itemCopy( pInfo->itmResult , pTemp );
               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutC( pInfo->itmResult, "" );
            }
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
            hb_itemPutND( pInfo->itmResult,0 );
            break;
         case DBOI_ISCOND:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               pTemp = hb_itemArrayGet( pTag, FOR_CLAUSE );
               hb_itemPutL( pInfo->itmResult, !HB_IS_NIL( pTemp ) );
               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutL( pInfo->itmResult, 0 );
            }
            break;
         case DBOI_ISDESC:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               hb_itemPutL( pInfo->itmResult, hb_arrayGetL( pTag, DESCEND_INDEX_ORDER ) );

               if ( pInfo->itmNewVal && HB_IS_LOGICAL( pInfo->itmNewVal ) )
               {
                  hb_itemPutL( pInfo->itmResult, hb_arrayGetL( thiswa->aInfo, AINFO_REVERSE_INDEX ) );
                  hb_itemPutL( hb_arrayGetItemPtr( pTag, DESCEND_INDEX_ORDER ), hb_itemGetL( pInfo->itmNewVal ) );
                  hb_itemPutL( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_REVERSE_INDEX ), hb_itemGetL( pInfo->itmNewVal ) );
                  hb_arraySetForward( pTag, ORDER_SKIP_UP, hb_itemNew( NULL ) );
                  hb_arraySetForward( pTag, ORDER_SKIP_DOWN, hb_itemNew( NULL ) );
                  hb_itemPutNL( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_BOF_AT ), 0 );
                  hb_itemPutNL( hb_arrayGetItemPtr( thiswa->aInfo, AINFO_EOF_AT ), 0 );
               }
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutL( pInfo->itmResult, 0 );
            }
            break;
         case DBOI_UNIQUE:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               hb_itemPutL( pInfo->itmResult, hb_arrayGetL( pTag, AORDER_UNIQUE ) );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemPutL( pInfo->itmResult, 0 );
            }
            break;
         case DBOI_CUSTOM:
            hb_itemPutL( pInfo->itmResult, 0 );
            break;
         case DBOI_SCOPETOP:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               if( pInfo->itmResult )
               {
                  pTemp = hb_itemArrayGet( pTag, TOP_SCOPE );
                  hb_itemCopy( pInfo->itmResult, pTemp );
                  hb_itemRelease( pTemp );
               }
               if( pInfo->itmNewVal )
               {
                  sqlSetServerSideIndexScope( (SQLAREAP)thiswa, 0, pInfo->itmNewVal );
               }
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemClear( pInfo->itmResult );
            }
            break;
         case DBOI_SCOPEBOTTOM:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               if( pInfo->itmResult )
               {
                  pTemp = hb_itemArrayGet( pTag, BOTTOM_SCOPE );
                  hb_itemCopy( pInfo->itmResult, pTemp );
                  hb_itemRelease( pTemp );
               }
               if( pInfo->itmNewVal )
               {
                  sqlSetServerSideIndexScope( (SQLAREAP)thiswa, 1, pInfo->itmNewVal );
               }
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemClear( pInfo->itmResult );
            }
            break;

         case DBOI_SCOPETOPCLEAR :
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               sqlSetServerSideIndexScope( (SQLAREAP)thiswa, 0, NULL );
            }
            break;
         case DBOI_SCOPEBOTTOMCLEAR :
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               sqlSetServerSideIndexScope( (SQLAREAP)thiswa, 1, NULL );
            }
            break;
         case DBOI_SCOPESET:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               if( pInfo->itmResult )
               {
                  pTemp = hb_itemArrayGet( pTag, TOP_SCOPE );
                  hb_itemCopy( pInfo->itmResult, pTemp );
                  hb_itemRelease( pTemp );
               }
               if( pInfo->itmNewVal )
               {
                  sqlSetServerSideIndexScope( (SQLAREAP)thiswa, TOP_BOTTOM_SCOPE, pInfo->itmNewVal );
               }
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemClear( pInfo->itmResult );
            }
            break;
         case DBOI_KEYADD:
            hb_itemPutL( pInfo->itmResult, 0 );
            break;
         case DBOI_KEYDELETE:
            hb_itemPutL( pInfo->itmResult, 0 );
            break;
         case DBOI_KEYVAL:
            pTag = loadTagDefault( thiswa, pInfo, &lorder );
            if( pTag )
            {
               if( thiswa->firstinteract )
               {
                  SELF_GOTOP( (AREAP) thiswa );
                  thiswa->firstinteract = 0;
               }

               pTemp = hb_itemArrayGet( pTag, INDEX_KEY );
               pMacro = hb_macroCompile( hb_itemGetCPtr( pTemp ) );
               hb_macroRun( pMacro );
               hb_macroDelete( pMacro );
               hb_itemForwardValue( pInfo->itmResult , hb_stackItemFromTop( -1 ) );
               hb_itemRelease( pTemp );
               hb_itemRelease( pTag );
            }
            else
            {
               hb_itemClear( pInfo->itmResult );
            }
            break;

         case DBOI_ORDERCOUNT:
            hb_itemPutNI( pInfo->itmResult, lIndexes );
            break;
      }
   }
   else
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNTRAW:
         {
            ULONG ulRecCount = 0;
            SELF_RECCOUNT( ( AREAP ) thiswa, &ulRecCount );
            hb_itemPutND( pInfo->itmResult,ulRecCount );
            break;
         }
         case DBOI_KEYCOUNT:
            hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLKEYCOUNT->pSymbol, 0 );
            hb_itemPutND( pInfo->itmResult, hb_itemGetND( hb_stackReturnItem() ) );
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
            hb_itemPutND( pInfo->itmResult, 0 );
            SELF_RECID( ( AREAP ) thiswa, pInfo->itmResult );
            break;
         case DBOI_ISCOND:
         case DBOI_ISDESC:
         case DBOI_UNIQUE:
         case DBOI_CUSTOM:
         case DBOI_KEYADD:
         case DBOI_KEYDELETE:
            hb_itemPutL( pInfo->itmResult, 0 );
            break;
         case DBOI_KEYVAL:
         case DBOI_SCOPETOP :
         case DBOI_SCOPEBOTTOM :
         case DBOI_SCOPETOPCLEAR :
         case DBOI_SCOPEBOTTOMCLEAR :
            hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_ORDERCOUNT:
         case DBOI_NUMBER:
            hb_itemPutNI( pInfo->itmResult, 0 );
            break;
         default:
            hb_itemPutC( pInfo->itmResult, "" );
      }
   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlClearFilter( SQLAREAP thiswa )
{
   if( thiswa->sqlfilter )
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLCLEARFILTER->pSymbol, 0 );
      thiswa->sqlfilter = FALSE;
   }
   return SUPER_CLEARFILTER( ( AREAP ) thiswa );
}

/*------------------------------------------------------------------------*/

#define  sqlClearLocate        NULL

/*------------------------------------------------------------------------*/

#define  sqlClearScope         NULL

/*------------------------------------------------------------------------*/

#define  sqlCountScope         NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlFilterText( SQLAREAP thiswa, PHB_ITEM pFilter )
{
   if( thiswa->sqlfilter )
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLFILTERTEXT->pSymbol, 0 );
      hb_itemCopy( pFilter, hb_stackReturnItem() );
      return HB_SUCCESS;
   }
   else
   {
      return SUPER_FILTERTEXT( ( AREAP ) thiswa, pFilter );
   }
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlScopeInfo( SQLAREAP thiswa, USHORT nScope, PHB_ITEM pItem )
{

   LONG lIndexes, lorder;
   HB_ITEM Order;
   PHB_ITEM pTag, pTemp;

   // TraceLog( NULL, "sqlScopeInfo, nScope: %i\n", nScope );

   Order.type = HB_IT_NIL;

   hb_itemClear( pItem );
   lIndexes = hb_itemSize( thiswa->aOrders );

   if( lIndexes )      /* Exists opened orders ? */
   {
      if( HB_IS_OBJECT( thiswa->oWorkArea ) )
      {
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLORDERLISTNUM->pSymbol, 1, &Order );
      }
      else
      {
         commonError( (AREAP) thiswa, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
         return HB_FAILURE;
      }
      lorder = hb_itemGetNL( hb_stackReturnItem() );
      if( lorder )
      {
         pTag  = hb_itemArrayGet( thiswa->aOrders, (ULONG) lorder );
         pTemp = hb_itemArrayGet( pTag, ( nScope ? BOTTOM_SCOPE : TOP_SCOPE ) );
         hb_itemCopy( pItem, pTemp );
         hb_itemRelease( pTag );
         hb_itemRelease( pTemp );
      }
   }
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSetFilter( SQLAREAP thiswa, LPDBFILTERINFO pFilterInfo )
{

   PHB_ITEM filtertext;
   HB_ERRCODE res;

   if( thiswa->lpdbPendingRel )
   {
      if( SELF_FORCEREL( (AREAP) thiswa ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   // Try to translate the filter expression into a valid SQL statement

   SUPER_CLEARFILTER( ( AREAP ) thiswa );

   filtertext = hb_itemNew( pFilterInfo->abFilterText );

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLSETFILTER->pSymbol, 1, filtertext );
   hb_itemRelease( filtertext );
   res = hb_itemGetNI( hb_stackReturnItem() );

   if( res == HB_SUCCESS )
   {
      thiswa->sqlfilter = TRUE;
      return HB_SUCCESS;
   }
   else
   {
      return SUPER_SETFILTER( ( AREAP ) thiswa, pFilterInfo );
   }
}

/*------------------------------------------------------------------------*/

#define  sqlSetLocate          NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlSetScope( SQLAREAP thiswa, LPDBORDSCOPEINFO sInfo )
{
   HB_ITEM scopetype;
   PHB_ITEM scopeval;
   int res;

   // TraceLog( NULL, "sqlSetScope\n" );

   scopeval = hb_itemNew( NULL );

   scopetype.type = HB_IT_INTEGER;
   scopetype.item.asInteger.value  = (int)sInfo->nScope;
   scopetype.item.asInteger.length = 10;

   if ( sInfo->scopeValue )
   {
      hb_itemCopy( scopeval, sInfo->scopeValue );
#ifndef HB_CDP_SUPPORT_OFF
      if( HB_IS_STRING( scopeval ) )
      {
         if( thiswa->cdPageCnv )
            hb_cdpTranslate( hb_itemGetCPtr( scopeval ), thiswa->cdPageCnv, thiswa->area.cdPage );
         else
            hb_cdpTranslate( hb_itemGetCPtr( scopeval ), hb_cdppage(), thiswa->area.cdPage );
      }
#endif
   }

   hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLSETSCOPE->pSymbol, 2, &scopetype, scopeval );
   res = hb_itemGetNI( hb_stackReturnItem() );
   hb_itemRelease( scopeval );

   if ((!res) && sr_GoTopOnScope() )
   {
      thiswa->firstinteract = 1;
   }

   return ( res == 0 ? HB_SUCCESS : HB_FAILURE );
}

/*------------------------------------------------------------------------*/

#define  sqlSkipScope          NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlLocate( SQLAREAP thiswa, BOOL fContinue )
{
   HB_ERRCODE err;
   HB_ITEM isFound;

   isFound.type = HB_IT_NIL;

   err = SUPER_LOCATE( (AREAP) thiswa, fContinue );

   hb_itemPutL( &isFound, thiswa->area.fFound );
   hb_arraySetForward( thiswa->aInfo, AINFO_FOUND, &isFound );
   return ( err );
}


/*------------------------------------------------------------------------*/

#define  sqlCompile            NULL
#define  sqlError              NULL
#define  sqlEvalBlock          NULL
#define  sqlRawLock            NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlLock( SQLAREAP thiswa, LPDBLOCKINFO pLockInfo )
{
   HB_ITEM  Method;
   PHB_ITEM pRecord;

   // TraceLog( NULL, "sqlLock\n" );

   Method.type = HB_IT_LONG;
   Method.item.asLong.value = pLockInfo->uiMethod;

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if( hb_arrayGetL( thiswa->aInfo, AINFO_ISINSERT ) || hb_arrayGetL( thiswa->aInfo, AINFO_EOF ) )
   {
      pLockInfo->fResult = TRUE;
      return ( HB_SUCCESS );
   }

   if( thiswa->shared )
   {
      switch( pLockInfo->uiMethod )
      {
      case DBLM_EXCLUSIVE:
         pRecord = hb_itemNew( NULL );
         hb_arrayGet( thiswa->aInfo, AINFO_RECNO, pRecord );
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLLOCK->pSymbol, 2, &Method, pRecord );
         hb_itemRelease( pRecord );

         pLockInfo->fResult = hb_itemGetL( hb_stackReturnItem() );
         break;

      case DBLM_MULTIPLE:
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLLOCK->pSymbol, 2, &Method, pLockInfo->itmRecID );
         pLockInfo->fResult = hb_itemGetL( hb_stackReturnItem() );
         break;

      case DBLM_FILE:
         hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLLOCK->pSymbol, 1, &Method );
         pLockInfo->fResult = hb_itemGetL( hb_stackReturnItem() );
         break;

      default:
         pLockInfo->fResult = FALSE;
      }
   }
   else
   {
      pLockInfo->fResult = TRUE;
   }

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlUnLock( SQLAREAP thiswa, PHB_ITEM pRecNo )
{

   if( thiswa->firstinteract )
   {
      SELF_GOTOP( (AREAP) thiswa );
      thiswa->firstinteract = 0;
   }

   if (pRecNo)
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLUNLOCK->pSymbol, 1, pRecNo );
   }
   else
   {
      hb_objSendSymbol( thiswa->oWorkArea, s_pSym_SQLUNLOCK->pSymbol, 0 );
   }

   return ( HB_SUCCESS );
}

/*------------------------------------------------------------------------*/

#define  sqlCloseMemFile       NULL
#define  sqlCreateMemFile      NULL
#define  sqlGetValueFile       NULL
#define  sqlOpenMemFile        NULL
#define  sqlPutValueFile       NULL
#define  sqlReadDBHeader       NULL
#define  sqlWriteDBHeader      NULL
#define  sqlInit               NULL

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlExit( LPRDDNODE pRDD )
{
   HB_SYMBOL_UNUSED( pRDD );
   HB_TRACE(HB_TR_DEBUG, ("sqlExit(%p)", pRDD));

   if( !s_pSym_SQLEXIT )
   {
      s_pSym_SQLEXIT = hb_dynsymFindName( "SR_END" );
   }
   hb_vmPushSymbol( s_pSym_SQLEXIT->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );
   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlDrop( PHB_ITEM pItemTable )
{

   // TraceLog( NULL, "sqlDrop\n" );

   hb_vmPushSymbol( s_pSym_WORKAREA->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   if( hb_stackReturnItem() )
   {
      hb_objSendSymbol( hb_stackReturnItem(), s_pSym_SQLDROP->pSymbol, 1, pItemTable );
   }
   else
   {
      commonError( NULL, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
      return HB_FAILURE;
   }

   return ( HB_SUCCESS );
}

/* returns 1 if exists, 0 else */
/*------------------------------------------------------------------------*/

BOOL sqlExists( PHB_ITEM pItemTable, PHB_ITEM pItemIndex )
{
   // TraceLog( NULL, "sqlExists\n" );

   hb_vmPushSymbol( s_pSym_WORKAREA->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   if( HB_IS_OBJECT( hb_stackReturnItem() ) )
   {
      if( pItemTable )
      {
         hb_objSendSymbol( hb_stackReturnItem(), s_pSym_SQLEXISTS->pSymbol, 1, pItemTable );
      }
      else if( pItemIndex )
      {
         hb_objSendSymbol( hb_stackReturnItem(), s_pSym_SQLEXISTS->pSymbol, 1, pItemIndex );
      }
      else
      {
         return HB_FAILURE;
      }
   }
   else
   {
      commonError( NULL, EG_DATATYPE, ESQLRDD_DATATYPE, NULL );
      return HB_FAILURE;
   }

   return hb_itemGetL( hb_stackReturnItem() );
}

/*------------------------------------------------------------------------*/

static HB_ERRCODE sqlRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   char szExt = '\0';

   HB_TRACE(HB_TR_DEBUG, ("sqlRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      {
         hb_itemPutCPtr( pItem, (char *) szExt, 1 );
         break;
      }

      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_STRUCTORD:
      case RDDI_MULTIKEY:
         hb_itemPutL( pItem, TRUE );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return HB_SUCCESS;
}

/*------------------------------------------------------------------------*/

#define  sqlWhoCares           NULL

/*------------------------------------------------------------------------*/

static BOOL ProcessFields( SQLAREAP thiswa )
{
   DBFIELDINFO    field;
   LONG           numFields;
   BYTE *         fieldType;
   USHORT         i;
   PHB_ITEM       thisfield;

   if (hb_itemType( thiswa->aStruct ) != HB_IT_ARRAY )
   {
      HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Invalid structure array"));
      return (FALSE);
   }

   numFields = hb_itemSize( thiswa->aStruct );

   if (!numFields)
   {
      HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Empty structure array"));
      return(FALSE);
   }

   SELF_SETFIELDEXTENT( (AREAP) thiswa, (USHORT)numFields );

   for ( i = 1; i <= (USHORT)numFields; i++ )
   {
      thisfield = hb_itemArrayGet( thiswa->aStruct, i );

      if (hb_itemType( thisfield ) != HB_IT_ARRAY )
      {
         HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Empty structure field array: %i", i));
         return(FALSE);
      }

      /* Clear out the field */
      memset( &field, 0, sizeof( field ) );

      field.uiTypeExtended = 0;
      field.atomName       = hb_arrayGetC( thisfield, (USHORT)1 );
      field.uiDec          = (USHORT)0;
      field.uiLen          = (USHORT)hb_arrayGetNI( thisfield, (USHORT)3 );


      thiswa->uiBufferIndex[i-1]  = (int)hb_arrayGetNI( thisfield, (USHORT)5 );

      fieldType = (unsigned char *) hb_arrayGetCPtr( thisfield, (USHORT)2 );

      switch ( *fieldType )
      {
      case 'c':
      case 'C':
         field.uiType = HB_FT_STRING;
         break;
      case 'm':
      case 'M':
         field.uiType = HB_FT_MEMO;
         break;
      case 'n':
      case 'N':
         field.uiType = HB_FT_LONG;
         field.uiDec  = (USHORT)hb_arrayGetNI( thisfield, (USHORT)4 );
         break;
      case 'l':
      case 'L':
         field.uiType =HB_FT_LOGICAL;
         break;
      case 'd':
      case 'D':
         field.uiType =HB_FT_DATE;
         break;
      case 'v':
      case 'V':
         field.uiType = HB_FT_ANY;
         break;
      // new field type
      case 't':
      case 'T':
         field.uiType = HB_FT_DATETIME;
         break;
      default:
         field.uiType =HB_IT_NIL;
         break;
      }

      if ( field.uiType == HB_IT_NIL)
      {
         HB_TRACE( HB_TR_ALWAYS, ("SQLRDD: Unsuported datatype on field: %i", i));
         return(FALSE);
      }

      // Add the field

      if ( SELF_ADDFIELD( ( AREAP ) thiswa, (LPDBFIELDINFO)&field ) != HB_SUCCESS )
      {
         HB_TRACE( HB_TR_ALWAYS, ("SQLRDD: Could not add field: %i", i));
      }
      hb_itemRelease( thisfield );
      hb_xfree( ( void * ) field.atomName );
   }
   return(TRUE);
}

/*------------------------------------------------------------------------*/

static BOOL SetFields( SQLAREAP thiswa )
{
   LONG           numFields;
   USHORT         i;
   PHB_ITEM       thisfield;

   if (hb_itemType( thiswa->aStruct ) != HB_IT_ARRAY )
   {
      HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Invalid structure array"));
      return (FALSE);
   }

   numFields = hb_itemSize( thiswa->aStruct );

   if (!numFields)
   {
      HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Empty structure array"));
      return(FALSE);
   }

   for ( i = 1; i <= (USHORT)numFields; i++ )
   {
      thisfield = hb_itemArrayGet( thiswa->aStruct, i );

      if (hb_itemType( thisfield ) != HB_IT_ARRAY )
      {
         HB_TRACE(HB_TR_ALWAYS, ("SQLRDD: Empty structure field array: %i", i));
         return(FALSE);
      }

      thiswa->uiBufferIndex[i-1]  = (int)hb_arrayGetNI( thisfield, (USHORT)5 );
      hb_itemRelease( thisfield );
   }
   return(TRUE);
}

/*------------------------------------------------------------------------*/

void commonError( AREAP thiswa, USHORT uiGenCode, USHORT uiSubCode, char* filename )
{
   PHB_ITEM pError = hb_errNew();

   hb_errPutGenCode( pError, uiGenCode );
   hb_errPutSubCode( pError, uiSubCode );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutTries( pError, EF_NONE );
   hb_errPutModuleName( pError, "SQLRDD" );
   hb_errPutSubSystem( pError, "SQLRDD" );

   hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );

   if( filename )
   {
      hb_errPutFileName( pError, filename );
   }

   SUPER_ERROR( ( AREAP ) thiswa, pError );

   hb_itemRelease( pError );

   return;
}

/*------------------------------------------------------------------------*/

HB_FUNC( ITEMCMP )  /* ITEMCMP( cItem1, cItem2, nLenToCompare ) ==> 0 == identical, < 0 if cItem1 < cIten2, > 0 == cItem1 > cIten2 */
{
   int ret;
   char * val1;
   char * val2;

   val1 = hb_itemGetCPtr( hb_param( 1, HB_IT_ANY ) );
   val2 = hb_itemGetCPtr( hb_param( 2, HB_IT_ANY ) );
   ret = strncmp( (char *)val1, (char *)val2, hb_parnl( 3 ) );

   hb_retni( ret );
}

/*------------------------------------------------------------------------*/

BOOL iTemCompEqual( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   if( pItem1->type == HB_IT_NIL || pItem2->type == HB_IT_NIL )
      return FALSE;

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
      return hb_itemStrCmp( pItem1, pItem2, FALSE ) == 0;
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
      return hb_itemGetNInt( pItem1 ) == hb_itemGetNInt( pItem2 );
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      return hb_itemGetND( pItem1 ) == hb_itemGetND( pItem2 );
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
      return pItem1->item.asDate.value == pItem2->item.asDate.value;
   else
      return FALSE;
}

/*------------------------------------------------------------------------*/

#ifdef _WIN32

   HB_FUNC( SR_GETCURRINSTANCEID )
   {
      hb_retni(GetCurrentProcessId());

   }

#else

   HB_FUNC( SR_GETCURRINSTANCEID )
   {
      pid_t Thispid = getpid();
     hb_retni(Thispid);
   }

#endif

/*------------------------------------------------------------------------*/

static const RDDFUNCS sqlTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )  sqlBof,
   ( DBENTRYP_BP )  sqlEof,
   ( DBENTRYP_BP )  sqlFound,
   ( DBENTRYP_V )   sqlGoBottom,
   ( DBENTRYP_UL )  sqlGoTo,
   ( DBENTRYP_I )   sqlGoToId,
   ( DBENTRYP_V )   sqlGoTop,
   ( DBENTRYP_BIB ) sqlSeek,
   ( DBENTRYP_L )   sqlSkip,
   ( DBENTRYP_L )   sqlSkipFilter,
   ( DBENTRYP_L )   sqlSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )  sqlAddField,
   ( DBENTRYP_B )   sqlAppend,
   ( DBENTRYP_I )   sqlCreateFields,
   ( DBENTRYP_V )   sqlDeleteRec,
   ( DBENTRYP_BP )  sqlDeleted,
   ( DBENTRYP_SP )  sqlFieldCount,
   ( DBENTRYP_VF )  sqlFieldDisplay,
   ( DBENTRYP_SSI ) sqlFieldInfo,
   ( DBENTRYP_SCP ) sqlFieldName,
   ( DBENTRYP_V )   sqlFlush,
   ( DBENTRYP_PP )  sqlGetRec,
   ( DBENTRYP_SI )  sqlGetValue,
   ( DBENTRYP_SVL ) sqlGetVarLen,
   ( DBENTRYP_V )   sqlGoCold,
   ( DBENTRYP_V )   sqlGoHot,
   ( DBENTRYP_P )   sqlPutRec,
   ( DBENTRYP_SI )  sqlPutValue,
   ( DBENTRYP_V )   sqlRecall,
   ( DBENTRYP_ULP ) sqlRecCount,
   ( DBENTRYP_ISI ) sqlRecInfo,
   ( DBENTRYP_ULP ) sqlRecNo,
   ( DBENTRYP_I )   sqlRecId,
   ( DBENTRYP_S )   sqlSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_CP )    sqlAlias,
   ( DBENTRYP_V )     sqlClose,
   ( DBENTRYP_VO )    sqlCreate,
   ( DBENTRYP_SI )    sqlInfo,
   ( DBENTRYP_V )     sqlNewArea,
   ( DBENTRYP_VO )    sqlOpen,
   ( DBENTRYP_V )     sqlRelease,
   ( DBENTRYP_SP )    sqlStructSize,
   ( DBENTRYP_CP )    sqlSysName,
   ( DBENTRYP_VEI )   sqlEval,
   ( DBENTRYP_V )     sqlPack,
   ( DBENTRYP_LSP )   sqlPackRec,
   ( DBENTRYP_VS )    sqlSort,
   ( DBENTRYP_VT )    sqlTrans,
   ( DBENTRYP_VT )    sqlTransRec,
   ( DBENTRYP_V )     sqlZap,


   /* Relational Methods */

   ( DBENTRYP_VR )    sqlChildEnd,
   ( DBENTRYP_VR )    sqlChildStart,
   ( DBENTRYP_VR )    sqlChildSync,
   ( DBENTRYP_V )     sqlSyncChildren,
   ( DBENTRYP_V )     sqlClearRel,
   ( DBENTRYP_V )     sqlForceRel,
   ( DBENTRYP_SSP )   sqlRelArea,
   ( DBENTRYP_VR )    sqlRelEval,
   ( DBENTRYP_SI )    sqlRelText,
   ( DBENTRYP_VR )    sqlSetRel,


   /* Order Management */

   ( DBENTRYP_VOI )   sqlOrderListAdd,
   ( DBENTRYP_V )     sqlOrderListClear,
   ( DBENTRYP_VOI )   sqlOrderListDelete,
   ( DBENTRYP_VOI )   sqlOrderListFocus,
   ( DBENTRYP_V )     sqlOrderListRebuild,
   ( DBENTRYP_VOO )   sqlOrderCondition,
   ( DBENTRYP_VOC )   sqlOrderCreate,
   ( DBENTRYP_VOI )   sqlOrderDestroy,
   ( DBENTRYP_SVOI )  sqlOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     sqlClearFilter,
   ( DBENTRYP_V )     sqlClearLocate,
   ( DBENTRYP_V )     sqlClearScope,
   ( DBENTRYP_VPLP )  sqlCountScope,
   ( DBENTRYP_I )     sqlFilterText,
   ( DBENTRYP_SI )    sqlScopeInfo,
   ( DBENTRYP_VFI )   sqlSetFilter,
   ( DBENTRYP_VLO )   sqlSetLocate,
   ( DBENTRYP_VOS )   sqlSetScope,
   ( DBENTRYP_VPL )   sqlSkipScope,
   ( DBENTRYP_B )     sqlLocate,


   /* Miscellaneous */

   ( DBENTRYP_CC )    sqlCompile,
   ( DBENTRYP_I )     sqlError,
   ( DBENTRYP_I )     sqlEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP )   sqlRawLock,
   ( DBENTRYP_VL )    sqlLock,
   ( DBENTRYP_I )     sqlUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )     sqlCloseMemFile,
   ( DBENTRYP_VO )    sqlCreateMemFile,
   ( DBENTRYP_SCCS )  sqlGetValueFile,
   ( DBENTRYP_VO )    sqlOpenMemFile,
   ( DBENTRYP_SCCS )  sqlPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     sqlReadDBHeader,
   ( DBENTRYP_V )     sqlWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_R )     sqlInit,
   ( DBENTRYP_R )     sqlExit,
   ( DBENTRYP_RVVL )  sqlDrop,
   ( DBENTRYP_RVVL )  sqlExists,
   ( DBENTRYP_RVVVL ) NULL,   /* sqlRename */
   ( DBENTRYP_RSLV )  sqlRddInfo,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   sqlWhoCares
};

HB_FUNC( SQLRDD ) {;}

HB_FUNC( SQLRDD_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   startSQLRDDSymbols();

   uiCount = ( USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE(HB_TR_DEBUG, ("SQLRDD_GETFUNCTABLE(%p, %p)", uiCount, pTable));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInherit( pTable, &sqlTable, &sqlrddSuper, NULL );
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}

#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static void hb_sqlrddRddInit( void * cargo )
{

   USHORT usResult;
   HB_SYMBOL_UNUSED( cargo );

   usResult = hb_rddRegister( "SQLRDD", RDT_FULL );
   if( usResult <= 1 )
   {
      if( usResult == 0 )
      {

         PHB_DYNS pDynSym;
         if( !s_pSym_SQLINIT )
         {
            s_pSym_SQLINIT = hb_dynsymFindName( "SR_INIT" );
         }
         hb_vmPushSymbol( s_pSym_SQLINIT->pSymbol );
         hb_vmPushNil();
         hb_vmDo( 0 );

         pDynSym = hb_dynsymFind( "__SR_STARTSQL" );

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         {
            hb_vmPushSymbol( pDynSym->pSymbol );
            hb_vmPushNil();
            hb_vmDo(0);
         }

      }
      return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_INIT_SYMBOLS_BEGIN( sqlrdd1__InitSymbols )
{ "SQLRDD",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLRDD )}, NULL },
{ "SQLRDD_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SQLRDD_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( sqlrdd1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sqlrdd_rdd_init_ )
   hb_vmAtInit( hb_sqlrddRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sqlrdd_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sqlrdd1__InitSymbols
   #pragma startup _hb_sqlrdd_rdd_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_sqlrdd1__InitSymbols = sqlrdd1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_sqlrdd_rdd_init = _hb_sqlrdd_rdd_init_;
   #pragma data_seg()
#endif


HB_FUNC( SR_SETFOUND )
{
   SQLAREAP pArea = ( SQLAREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pFound = hb_param( 1, HB_IT_LOGICAL );
      if( pFound )
      {
         pArea->area.fFound = hb_itemGetL( pFound );
         hb_arraySetForward( pArea->aInfo, AINFO_FOUND, pFound );
      }

   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

