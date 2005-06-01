/*
 * $Id: dbfntx1.c,v 1.113 2005/05/19 02:54:45 druzus Exp $
 */

/*
 * Harbour Project source code:
 * DBFNTX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
/*
 * The following functions are added by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 * commonError()
 * hb_IncString()
 * ntxNumToStr()
 * checkLogicalExpr()
 * hb__ntxTagKeyCount()
 * hb_ntxInTopScope()
 * hb_ntxInBottomScope()
 * hb_ntxTagKeyNo()
 * hb_ntxTagKeyCount()
 * hb_ntxClearScope()
 * hb_ntxGoEof()
 * hb_ntxGetKeyType()
 * hb_ntxTagKeyFind()
 * hb_ntxPageKeySearch()
 * hb_ntxTagFindCurrentKey()
 * hb_ntxIsRecBad()
 * hb_ntxPageFindCurrentKey()
 * hb_ntxGetCurrentKey()
 * hb_ntxTagGoToNextKey()
 * hb_ntxTagGoToPrevKey()
 * hb_ntxTagGoToTopKey()
 * hb_ntxTagGoToBottomKey()
 * hb_ntxTagKeyGoTo()
 * hb_ntxPageRelease()
 * hb_ntxKeysMove()
 * hb_ntxPageSplit()
 * hb_ntxPageJoin()
 * hb_ntxPageBalance()
 * hb_ntxTagBalance()
 * hb_ntxPageKeyDel()
 * hb_ntxTagKeyAdd()
 * hb_ntxSwapPageSave()
 * hb_ntxKeysSort()
 * hb_ntxSortKeyAdd()
 * hb_ntxSortKeyEnd()
 * hb_ntxWritePage()
 * hb_ntxRootPage()
 * hb_ntxGetSortedKey()
 * hb_ntxBufferSave()
 * hb_ntxReadBuf()
 * hb_ntxPageFind()
 * ntxFindIndex()
 * hb_ntxOrdKeyAdd()
 * hb_ntxOrdKeyDel()
 * ntxGoBottom()
 * ntxGoTo()
 * ntxGoTop()
 * ntxSeek()
 * ntxSkipRaw()
 * ntxGoCold()
 * ntxGoHot()
 * ntxSysName()
 * ntxPack()
 * ntxZap()
 * ntxClearScope()
 * ntxScopeInfo()
 * ntxOrderListAdd()
 * ntxOrderListClear()
 * ntxOrderListFocus()
 * ntxOrderListRebuild()
 * ntxSetScope()
 */

/*
 * Copyright 2005 Przemyslaw Czerpak <druzus@priv.onet.pl>
 * in practice most of the code rewritten
 */


//#define HB_NTX_DEBUG
//#define HB_NTX_DEBUG_EXT

#include "hbapi.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbmath.h"
#include "hbrddntx.h"
#ifndef HB_CDP_SUPPORT_OFF
   #include "hbapicdp.h"
#endif

#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_FUNC( _DBFNTX );
HB_FUNC( DBFNTX_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "_DBFNTX",             HB_FS_PUBLIC, {HB_FUNCNAME( _DBFNTX )},             0 },
{ "DBFNTX_GETFUNCTABLE", HB_FS_PUBLIC, {HB_FUNCNAME( DBFNTX_GETFUNCTABLE)} , 0 }
HB_INIT_SYMBOLS_END( dbfntx1__InitSymbols )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup dbfntx1__InitSymbols
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_dbfntx1__InitSymbols = dbfntx1__InitSymbols;
   #pragma data_seg()
#endif

static RDDFUNCS ntxSuper;

static ERRCODE hb_ntxTagCreate( LPTAGINFO pTag );
static void hb_ntxSortFree( LPNTXSORTINFO pSort );


#define hb_ntxKeyFree(K) hb_xfree(K)


/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined( HB_LITTLE_ENDIAN ) && !defined( HB_STRICT_ALIGNMENT )

#define hb_ntxGetKeyCount(p)        HB_GET_LE_UINT16( (p)->buffer )
#define hb_ntxSetKeyCount(p,n)      HB_PUT_LE_UINT16( (p)->buffer, (n) )

#define hb_ntxGetKeyOffset(p,n)     HB_GET_LE_UINT16( (p)->buffer+2+((n)<<1) )
#define hb_ntxGetKeyPtr(p,n)        ( (p)->buffer + hb_ntxGetKeyOffset(p,n) )
#define hb_ntxGetKeyPage(p,n)       HB_GET_LE_UINT32( hb_ntxGetKeyPtr(p,n) )
#define hb_ntxGetKeyRec(p,n)        HB_GET_LE_UINT32( hb_ntxGetKeyPtr(p,n)+4 )
#define hb_ntxGetKeyVal(p,n)        ( hb_ntxGetKeyPtr(p,n)+8 )

#define hb_ntxSetKeyOffset(p,n,u)   HB_PUT_LE_UINT16( (p)->buffer+2+((n)<<1), u )
#define hb_ntxSetKeyPage(p,n,l)     HB_PUT_LE_UINT32( hb_ntxGetKeyPtr(p,n), l )
#define hb_ntxSetKeyRec(p,n,l)      HB_PUT_LE_UINT32( hb_ntxGetKeyPtr(p,n)+4, l )

#else

static USHORT hb_ntxGetKeyCount( LPPAGEINFO pPage )
{
   char * ptr = pPage->buffer;
   return HB_GET_LE_UINT16( ptr );
}

static void hb_ntxSetKeyCount( LPPAGEINFO pPage, USHORT uiKeys )
{
   char * ptr = pPage->buffer;
   HB_PUT_LE_UINT16( ptr, uiKeys );
}

static USHORT hb_ntxGetKeyOffset( LPPAGEINFO pPage, SHORT iKey )
{
   char * ptr = pPage->buffer + 2 + ( iKey << 1 );
   return HB_GET_LE_UINT16( ptr );
}

static void hb_ntxSetKeyOffset( LPPAGEINFO pPage, SHORT iKey, USHORT uiOffset )
{
   char * ptr = pPage->buffer + 2 + ( iKey << 1 );
   HB_PUT_LE_UINT16( ptr, uiOffset );
}

static char * hb_ntxGetKeyPtr( LPPAGEINFO pPage, SHORT iKey )
{
   return pPage->buffer + hb_ntxGetKeyOffset( pPage, iKey );
}

static ULONG hb_ntxGetKeyPage( LPPAGEINFO pPage, SHORT iKey )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey );
   return HB_GET_LE_UINT32( ptr );
}

static void hb_ntxSetKeyPage( LPPAGEINFO pPage, SHORT iKey, ULONG ulPage )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey );
   HB_PUT_LE_UINT32( ptr, ulPage );
}

static char * hb_ntxGetKeyVal( LPPAGEINFO pPage, SHORT iKey )
{
   return hb_ntxGetKeyPtr( pPage, iKey ) + 8;
}

static void hb_ntxSetKeyRec( LPPAGEINFO pPage, SHORT iKey, ULONG ulRec )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey ) + 4;
   HB_PUT_LE_UINT32( ptr, ulRec );
}

static ULONG hb_ntxGetKeyRec( LPPAGEINFO pPage, SHORT iKey )
{
   char * ptr = hb_ntxGetKeyPtr( pPage, iKey ) + 4;
   return HB_GET_LE_UINT32( ptr );
}

#endif

/*
 * generate Run-Time error
 */
static ERRCODE hb_ntxErrorRT( NTXAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char * filename, USHORT uiOsCode, USHORT uiFlags )
{
   PHB_ITEM pError;
   ERRCODE iRet = FAILURE;

   if ( hb_vmRequestQuery() == 0 )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, uiGenCode );
      hb_errPutSubCode( pError, uiSubCode );
      hb_errPutOsCode( pError, uiOsCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
      if ( filename )
         hb_errPutFileName( pError, filename );
      if ( uiFlags )
         hb_errPutFlags( pError, uiFlags );
      iRet = SELF_ERROR( ( AREAP ) pArea, pError );
      hb_errRelease( pError );
   }
   return iRet;
}

/*
 * convert numeric item into NTX key value
 */
static char * hb_ntxNumToStr( PHB_ITEM pItem, char* szBuffer, USHORT length, USHORT dec )
{
   char *ptr = szBuffer;

   hb_itemStrBuf( szBuffer, pItem, length, dec );

   while( *ptr == ' ' )
      *ptr++ = '0';

   if( *ptr == '-' )
   {
      *ptr = '0';
      for( ptr = &szBuffer[0]; *ptr; ptr++ )
      {
         if( *ptr >= '0' && *ptr <= '9' )
            *ptr = (char) ( '0' - ( *ptr - '0' ) - 4 );
            /*
             * I intentionally used the above formula to avoid problems on
             * non ASCII machines though many of other xHarbour codes is
             * hard coded to ASCII values and should be fixed. Druzus.
             */
      }
   }

   return szBuffer;
}

/*
 * convert numeric NTX key value into item
 */
static PHB_ITEM hb_ntxStrToNum( PHB_ITEM pItem, char* szKeyVal, USHORT length, USHORT dec )
{
   char szBuffer[ NTX_MAX_KEY + 1 ];
   char *ptr = szKeyVal, *ptr2, c;

   if( *ptr == '0' - 4 ) /* negative number */
   {
      ptr2 = szBuffer;
      while( ( c = *ptr++ ) != 0 )
      {
         if ( c != '.' )
            c = '0' - ( c - '0' + 4 );
         *ptr2++ = c;
      }
      szBuffer[ 0 ] = '-';
      *ptr2 = '\0';
      ptr = szBuffer;
   }
   return hb_itemPutNLen( pItem, hb_strVal( ptr, length ), length, dec );
}

/*
 * create new index key
 */
static LPKEYINFO hb_ntxKeyNew( LPKEYINFO pKeyFrom, int keylen )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) + keylen );
   if( pKeyFrom )
   {
      memcpy( pKey->key, pKeyFrom->key, keylen );
      pKey->Tag = pKeyFrom->Tag;
      pKey->Xtra = pKeyFrom->Xtra;
   }
   else
   {
      *(pKey->key) = '\0';
      pKey->Tag = pKey->Xtra = 0;
   }
   return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO hb_ntxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen )
{
   if ( !pKeyDest )
      pKeyDest = hb_ntxKeyNew( NULL, keylen );

   memcpy( pKeyDest->key, pKey->key, keylen );
   pKeyDest->Tag = pKey->Tag;
   pKeyDest->Xtra = pKey->Xtra;

   return pKeyDest;
}

/*
 * get ntx key type for given item
 */
static BYTE hb_ntxItemType( PHB_ITEM pItem )
{
   switch( hb_itemType( pItem ) )
   {
      case HB_IT_STRING:
      case HB_IT_STRING | HB_IT_MEMO:
         return 'C';

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         return 'N';

      case HB_IT_DATE:
         return 'D';

      case HB_IT_LOGICAL:
         return 'L';

      default:
         return 'U';
   }
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 */
static LPKEYINFO hb_ntxKeyPutItem( LPKEYINFO pKey, PHB_ITEM pItem, ULONG ulRecNo,
                                   LPTAGINFO pTag, BOOL fTrans, USHORT *puiLen )
{
   ULONG len;

   if ( !pKey )
      pKey = hb_ntxKeyNew( NULL, pTag->KeyLength );

   if ( puiLen )
      *puiLen = pTag->KeyLength;

   switch( hb_ntxItemType( pItem ) )
   {
      case 'C':
         len = pItem->item.asString.length;
         if ( len < ( ULONG ) pTag->KeyLength )
         {
            memcpy( pKey->key, pItem->item.asString.value, len );
            memset( pKey->key + len, ' ', pTag->KeyLength - len );
            if ( puiLen )
               *puiLen = len;
         }
         else
         {
            memcpy( pKey->key, pItem->item.asString.value, pTag->KeyLength );
         }
         pKey->key[ pTag->KeyLength ] = '\0';
#ifndef HB_CDP_SUPPORT_OFF
         if ( fTrans )
            hb_cdpnTranslate( pKey->key, hb_cdp_page, pTag->Owner->Owner->cdPage, pTag->KeyLength );
#endif
         break;
      case 'N':
         hb_ntxNumToStr( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
         break;
      case 'D':
         hb_itemGetDS( pItem, pKey->key );
         break;
      case 'L':
         pKey->key[0] = ( hb_itemGetL( pItem ) ? 'T':'F' );
         pKey->key[1] = 0;
         break;
      default:
         memset( pKey->key, '\0', pTag->KeyLength );
   }
   pKey->Xtra = ulRecNo;
   pKey->Tag = 0;

   return pKey;
}

/*
 * get Item from index key
 */
static PHB_ITEM hb_ntxKeyGetItem( PHB_ITEM pItem, LPKEYINFO pKey,
                                  LPTAGINFO pTag, BOOL fTrans )
{
   if ( pKey )
   {
      switch( pTag->KeyType )
      {
         case 'C':
#ifndef HB_CDP_SUPPORT_OFF
            if( fTrans && pTag->Owner->Owner->cdPage != hb_cdp_page )
            {
               char * pVal = ( char * ) hb_xgrab( pTag->KeyLength + 1 );
               memcpy( pVal, pKey->key, pTag->KeyLength );
               pVal[ pTag->KeyLength ] = '\0';
               hb_cdpnTranslate( pVal, pTag->Owner->Owner->cdPage, hb_cdp_page,
                                 pTag->KeyLength );
               pItem = hb_itemPutCPtr( pItem, pVal, pTag->KeyLength );
            }
            else
#endif
            {
               pItem = hb_itemPutCL( pItem, pKey->key, pTag->KeyLength );
            }
            break;
         case 'N':
            pItem = hb_ntxStrToNum( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
            break;
         case 'D':
            pItem = hb_itemPutDS( pItem, pKey->key );
            break;
         case 'L':
            pItem = hb_itemPutL( pItem, pKey->key[0] == 'T' );
            break;
         default:
            if ( pItem )
               hb_itemClear( pItem );
            else
               pItem = hb_itemNew( NULL );
      }
   }
   else if ( pItem )
      hb_itemClear( pItem );
   else
      pItem = hb_itemNew( NULL );

   return pItem;
}

/*
 * destroy compiled expression
 */
static void hb_ntxDestroyExp( PHB_ITEM pExp )
{
   if ( hb_itemType( pExp ) != HB_IT_BLOCK )
      hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pExp ) );
   hb_itemRelease( pExp );
}

/*
 * evaluate conditional expression and return the logical result
 */
static BOOL hb_ntxEvalCond( NTXAREAP pArea, PHB_ITEM pCondItem, BOOL fSetWA )
{
   int iCurrArea = 0;
   BOOL fRet;

   if ( fSetWA )
   {
      iCurrArea = hb_rddGetCurrentWorkAreaNumber();
      if ( iCurrArea != pArea->uiArea )
         hb_rddSelectWorkAreaNumber( pArea->uiArea );
      else
         iCurrArea = 0;
   }

   fRet = hb_itemGetL( hb_vmEvalBlockOrMacro( pCondItem ) );

   if ( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );

   return fRet;
}

/*
 * get ITEM type of key expression
 */
static BYTE hb_ntxGetKeyType( LPTAGINFO pTag )
{
   BYTE bType;

   if( pTag->nField )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pTag->Owner->Owner, pTag->nField, pItem );
      bType = hb_ntxItemType( pItem );
      hb_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if ( iCurrArea != pTag->Owner->Owner->uiArea )
         hb_rddSelectWorkAreaNumber( pTag->Owner->Owner->uiArea );
      else
         iCurrArea = 0;

      bType = hb_ntxItemType( hb_vmEvalBlockOrMacro( pTag->pKeyItem ) );

      if ( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }
   return bType;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO hb_ntxEvalKey( LPKEYINFO pKey, LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_ITEM pItem;
#ifndef HB_CDP_SUPPORT_OFF
   /* TODO: this hack is not thread safe, hb_cdp_page has to be thread specific */
   PHB_CODEPAGE cdpTmp = hb_cdp_page;
   hb_cdp_page = pArea->cdPage;
#endif

   if( pTag->nField )
   {
      pItem = hb_itemNew( NULL );
      SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
      pKey = hb_ntxKeyPutItem( pKey, pItem, pArea->ulRecNo, pTag, FALSE, NULL );
      hb_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      if ( iCurrArea != pArea->uiArea )
         hb_rddSelectWorkAreaNumber( pArea->uiArea );
      else
         iCurrArea = 0;

      pItem = hb_vmEvalBlockOrMacro( pTag->pKeyItem );
      pKey = hb_ntxKeyPutItem( pKey, pItem, pArea->ulRecNo, pTag, FALSE, NULL );

      if ( iCurrArea )
         hb_rddSelectWorkAreaNumber( iCurrArea );
   }

#ifndef HB_CDP_SUPPORT_OFF
   hb_cdp_page = cdpTmp;
#endif

   return pKey;
}

/*
 * find field index for single field expressions
 */
static USHORT hb_ntxFieldIndex( NTXAREAP pArea, char * cExpr )
{
   char szKeyExpr[ NTX_MAX_KEY + 1 ],
        szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   int i, j, l, n = 0;

   if ( SELF_ALIAS( ( AREAP ) pArea, ( BYTE * ) szAlias ) == SUCCESS )
      l = strlen( szAlias );
   else
      l = 0;

   hb_strncpyUpperTrim( szKeyExpr, cExpr, NTX_MAX_KEY );

   /*
    * strip the _FIELD-> and FIELD-> prefix, it could be nested so repeat
    * this process until all prefixes will be removed
    */
   do
   {
      j = n;
      if ( strncmp( &szKeyExpr[ n ], "FIELD", 5 ) == 0 )
         i = 5;
      else if ( strncmp( &szKeyExpr[ n ], "_FIELD", 6 ) == 0 )
         i = 6;
      else if ( l > 0 && strncmp( &szKeyExpr[ n ], szAlias, l ) == 0 )
         i = l;
      else
         i = 0;

      if ( i > 0 )
      {
         i = n + 5;
         while ( szKeyExpr[ i ] == ' ' )
            i++;
         if ( szKeyExpr[ i ] == '-' && szKeyExpr[ i + 1 ] == '>' )
         {
            n = i + 2;
            while ( szKeyExpr[ n ] == ' ' )
               n++;
         }
      }
   }
   while ( n != j );

   return hb_rddFieldIndex( ( AREAP ) pArea, &szKeyExpr[ n ] );
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_ntxValCompare( LPTAGINFO pTag, char* val1, int len1,
                             char* val2, int len2, BOOL fExact )
{
   int iLimit, iResult = 0;

   iLimit = (len1 > len2) ? len2 : len1;

   if ( pTag->KeyType == 'C' )
   {
      if ( iLimit > 0 )
      {
#ifndef HB_CDP_SUPPORT_OFF
         if ( pTag->Owner->Owner->cdPage->lSort )
            iResult = hb_cdpcmp( val1, val2, ( ULONG ) iLimit, pTag->Owner->Owner->cdPage, NULL );
         else
#endif
            iResult = memcmp( val1, val2, iLimit );
      }

      if ( iResult == 0 )
      {
         if ( len1 > len2 )
            iResult = 1;
         else if ( len1 < len2 && fExact )
            iResult = -1;
      }
   }
   else
   {
      if ( iLimit <= 0 || (iResult = memcmp( val1, val2, iLimit )) == 0 )
      {
         if ( len1 > len2 )
            iResult = 1;
         else if ( len1 < len2 )
            iResult = -1;
      }
   }
   return iResult;
}

/*
 * check if a given key is in top scope
 */
static BOOL hb_ntxInTopScope( LPTAGINFO pTag, char* key )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
   {
      int i = hb_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                 key, pTag->KeyLength, FALSE );
      return pTag->fUsrDescend ? i >= 0 : i <= 0;
   }
   else
      return TRUE;
}

/*
 * check if a given key is in bottom scope
 */
static BOOL hb_ntxInBottomScope( LPTAGINFO pTag, char* key )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
   {
      int i = hb_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                key, pTag->KeyLength, FALSE );
      return pTag->fUsrDescend ? i <= 0 : i >= 0;
   }
   else
      return TRUE;
}

/*
 * check if a given key is in current scope
 */
static BOOL hb_ntxKeyInScope( LPTAGINFO pTag, LPKEYINFO pKey )
{
   return hb_ntxInTopScope( pTag, pKey->key ) &&
          hb_ntxInBottomScope( pTag, pKey->key );
}

/*
 * clear top or bottom scope
 */
static void hb_ntxTagClearScope( LPTAGINFO pTag, USHORT nScope )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_NTXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if ( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if ( pScope->scopeKey )
   {
      hb_ntxKeyFree( pScope->scopeKey );
      pScope->scopeKey = NULL;
   }
   if ( pScope->scopeItem )
   {
      hb_itemRelease( pScope->scopeItem );
      pScope->scopeItem = NULL;
   }
   pScope->scopeKeyLen = 0;

   pTag->keyCount = 0;
}

/*
 * set top or bottom scope
 */
static void hb_ntxTagSetScope( LPTAGINFO pTag, USHORT nScope, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   pScopeVal = ( hb_itemType( pItem ) == HB_IT_BLOCK ) ?
                           hb_vmEvalBlock( pItem ) : pItem;

   if ( pTag->KeyType == hb_ntxItemType( pScopeVal ) )
   {
      PHB_NTXSCOPE pScope;
      BOOL fTop = ( nScope == 0 );

      if ( pTag->fUsrDescend )
         fTop = !fTop;

      pScope = fTop ? &pTag->top : &pTag->bottom;

      pScope->scopeKey = hb_ntxKeyPutItem( pScope->scopeKey, pScopeVal,
               ( fTop == pTag->AscendKey ) ? NTX_IGNORE_REC_NUM : NTX_MAX_REC_NUM,
               pTag, TRUE, &pScope->scopeKeyLen );

      if( pScope->scopeItem == NULL )
         pScope->scopeItem = hb_itemNew( NULL );
      hb_itemCopy( pScope->scopeItem, pItem );

      pTag->keyCount = 0;
   }
}

/*
 * get top or bottom scope item
 */
static void hb_ntxTagGetScope( LPTAGINFO pTag, USHORT nScope, PHB_ITEM pItem )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   PHB_NTXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if ( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if ( pScope->scopeItem )
      hb_itemCopy( pItem, pScope->scopeItem );
   else
      hb_itemClear( pItem );
}

/*
 * refresh top and bottom scope value if set as codeblock
 */
static void hb_ntxTagRefreshScope( LPTAGINFO pTag )
{
   PHB_ITEM pItem;

   if ( hb_itemType( pTag->top.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->top.scopeItem );
      pTag->top.scopeKey = hb_ntxKeyPutItem( pTag->top.scopeKey, pItem,
               pTag->top.scopeKey->Xtra, pTag, TRUE, &pTag->top.scopeKeyLen );
   }
   if ( hb_itemType( pTag->bottom.scopeItem ) == HB_IT_BLOCK )
   {
      pItem = hb_vmEvalBlock( pTag->bottom.scopeItem );
      pTag->bottom.scopeKey = hb_ntxKeyPutItem( pTag->bottom.scopeKey, pItem,
         pTag->bottom.scopeKey->Xtra, pTag, TRUE, &pTag->bottom.scopeKeyLen );
   }
}

#ifdef HB_NTX_DEBUG
static void hb_ntxTagCheckBuffers( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = pTag->pages;
   ULONG i;

   if( pTag->TagChanged && !pTag->Owner->lockWrite )
      hb_errInternal( 9999, "hb_ntxTagCheckBuffers: tag modified in unlocked index", "", "" );

   for( i = 0; i < pTag->ulPages; i++, pPage++ )
   {
      if( pPage->Changed && !pTag->Owner->lockWrite )
         hb_errInternal( 9999, "hb_ntxTagCheckBuffers: page modified in unlocked index", "", "" );
      if( pPage->iUsed )
         hb_errInternal( 9999, "hb_ntxTagCheckBuffers: page still allocated", "", "" );
   }
}

static void hb_ntxPageCheckKeys( LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType )
{
   USHORT u;
   int i;

   for ( u = 1; u < pPage->uiKeys; u++ )
   {
      i = hb_ntxValCompare( pTag,
                            hb_ntxGetKeyVal( pPage, u - 1 ), pTag->KeyLength,
                            hb_ntxGetKeyVal( pPage, u ), pTag->KeyLength, TRUE );
      if( !pTag->AscendKey )
         i = -i;
      if ( i > 0 )
      {
         printf("\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType,
                u - 1, pTag->KeyLength, hb_ntxGetKeyVal( pPage, u - 1 ),
                u, pTag->KeyLength, hb_ntxGetKeyVal( pPage, u ) );
         fflush(stdout);
         hb_errInternal( 9999, "hb_ntxPageCheckKeys: keys sorted wrong.", "", "" );
      }
   }
}
#endif

/*
 * write a given tag page to file
 */
static void hb_ntxPageSave( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   hb_ntxSetKeyCount( pPage, pPage->uiKeys );
   hb_fsSeek( pTag->Owner->DiskFile, pPage->Page, FS_SET );
   hb_fsWrite( pTag->Owner->DiskFile, (BYTE *) pPage->buffer, NTXBLOCKSIZE );
   pPage->Changed = FALSE;
   pTag->Owner->fFlush = TRUE;
   /* In shared mode we have to update counter in version field of
      NTXHEADER to signal for other stations that their index buffers
      has to be discarded */
   if ( pTag->Owner->Owner->fShared )
      pTag->TagChanged = TRUE;
}

/*
 * free buffers for pages in the tag
 */
static void hb_ntxFreePageBuffer( LPTAGINFO pTag )
{
   ULONG ul = 0, ulMax = pTag->ulPagesDepth;
   LPPAGEINFO pPage = pTag->pages;

   pTag->stackLevel = 0;

   if( pTag->Memory )
   {
      hb_xfree( pTag->pages[0].buffer );
      ul = pTag->ulPagesStart;
      pPage = pTag->pages + ul;
   }

   for( ; ul < ulMax; ul++, pPage++ )
   {
      if( pPage->buffer )
      {
         hb_xfree( pPage->buffer );
         pPage->buffer = NULL;
      }
   }
   pTag->ulPages = pTag->ulPageLast = 0;
}

/*
 * discard all tag buffers due to concurrent access
 */
static void hb_ntxDiscardBuffers( LPTAGINFO pTag )
{
   pTag->ulPages = pTag->ulPageLast = 0;
   pTag->TagBlock = NTX_DUMMYNODE;
   pTag->stackLevel = 0;
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO hb_ntxPageFind( LPTAGINFO pTag, ULONG ulPage )
{
   LPPAGEINFO pPage = pTag->pages;
   ULONG u;

   for( u = pTag->ulPages; u; u--, pPage++ )
      if( pPage->Page == ulPage )
         return pPage;
   return NULL;
}

/*
 * try to find free space in buffer
 */
static LPPAGEINFO hb_ntxPageGetBuffer( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;

   if ( pTag->ulPages < pTag->ulPagesDepth )
   {
      pPage = pTag->pages + pTag->ulPages++;
   }
   else
   {
      ULONG ul = pTag->ulPageLast;
      do
      {
         if ( ++ul >= pTag->ulPagesDepth )
            ul = 0;
         pPage = pTag->pages + ul;
         if ( !pPage->iUsed && !pPage->Changed )
         {
            pTag->ulPageLast = ul;
            break;
         }
         if ( ul == pTag->ulPageLast )
         {
            ul = pTag->ulPagesDepth;
            pTag->ulPagesDepth += NTX_PAGES_PER_TAG >> 1;
            pTag->pages = (LPPAGEINFO) hb_xrealloc( pTag->pages,
                                    sizeof(HB_PAGEINFO) * pTag->ulPagesDepth );
            memset( pTag->pages + ul, 0,
                         ( NTX_PAGES_PER_TAG >> 1 ) * sizeof( HB_PAGEINFO ) );
            pTag->ulPages++;
            pPage = pTag->pages + ul;
            pTag->ulPageLast = 0;
            break;
         }
      }
      while ( TRUE );
   }

   if( !pPage->buffer )
      pPage->buffer = ( char* ) hb_xgrab( NTXBLOCKSIZE );

   return pPage;
}

/*
 * free the index page for future reuse
 */
static void hb_ntxPageFree( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   hb_ntxSetKeyPage( pPage, 0, pTag->Owner->NextAvail );
   pTag->Owner->NextAvail = pPage->Page;
   pTag->TagChanged = pPage->Changed = TRUE;
}

/*
 * mark used page as free
 */
static void hb_ntxPageRelease( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   if( pTag->Memory )
      return;
   if ( --pPage->iUsed < 0 )
      hb_errInternal( 9999, "hb_ntxPageRelease: unused page freed.", "", "" );
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO hb_ntxPageLoad( LPTAGINFO pTag, ULONG ulPage )
{
   LPPAGEINFO pPage;

   if( !ulPage )
   {
      ulPage = pTag->RootBlock;
   }
   if( pTag->Memory )
   {
      return pTag->pages + ( ulPage / NTXBLOCKSIZE - 1 );
   }
   pPage = hb_ntxPageFind( pTag, ulPage );
   if( pPage )
   {
      pPage->iUsed++;
      return pPage;
   }
   pPage = hb_ntxPageGetBuffer( pTag );

   hb_fsSeek( pTag->Owner->DiskFile, ulPage, FS_SET );
   if( hb_fsRead( pTag->Owner->DiskFile, (BYTE *) pPage->buffer, NTXBLOCKSIZE )
                                                            != NTXBLOCKSIZE )
   {
      hb_ntxErrorRT( pTag->Owner->Owner, EG_CORRUPTION, EDBF_CORRUPT, pTag->Owner->IndexName, hb_fsError(), 0 );
      return NULL;
   }
   pPage->Page = ulPage;
   pPage->Changed = FALSE;
   pPage->iUsed++;
   pPage->uiKeys = hb_ntxGetKeyCount( pPage );

   return pPage;
}

/*
 * initialize empty page structure
 */
static void hb_ntxPageInit( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   USHORT u, o = ( pTag->MaxKeys + 2 ) << 1;

   memset( pPage->buffer, 0, NTXBLOCKSIZE );
   for( u = 0; u <= pTag->MaxKeys; u++, o += pTag->KeyLength + 8 )
      hb_ntxSetKeyOffset( pPage, u, o );
   hb_ntxSetKeyPage( pPage, 0, 0 );
}

/*
 * allocate new page in index file
 */
static LPPAGEINFO hb_ntxPageNew( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;

   if ( pTag->Owner->NextAvail != 0 )
   {
      /* Handling of a pool of empty pages.
         Some sources says that this address is in the first 4 bytes of
         a page ( http://www.e-bachmann.dk/docs/xbase.htm ).
         But as I understood, studying dumps of Clipper ntx'es, address of the
         next available page is in the address field of a first key item
         in the page - it is done here now in such a way.
         = Alexander Kresin =
      */
      pPage = hb_ntxPageLoad( pTag, pTag->Owner->NextAvail );
      if ( ! pPage )
         return NULL;
      pTag->Owner->NextAvail = hb_ntxGetKeyPage( pPage, 0 );
      hb_ntxSetKeyPage( pPage, 0, 0 );
      pTag->TagChanged = TRUE;
   }
   else
   {
      pPage = hb_ntxPageGetBuffer( pTag );
      hb_ntxPageInit( pTag, pPage );
      if ( pTag->TagBlock == NTX_DUMMYNODE )
         pTag->TagBlock = hb_fsSeek( pTag->Owner->DiskFile, 0, FS_END ) - NTXBLOCKSIZE;
      pPage->Page = pTag->TagBlock += NTXBLOCKSIZE;
      pPage->iUsed++;
      pPage->uiKeys = 0;
   }
   pPage->Changed = TRUE;

   return pPage;
}

/*
 * Write index header
 */
static void hb_ntxHeaderSave( LPNTXINDEX pIndex, BOOL fFull )
{
   NTXHEADER Header;
   LPTAGINFO pTag = pIndex->CompoundTag;
   USHORT uiSize = 12, type;

   if( pTag->Memory )
      return;

   pIndex->Version++;
   type = NTX_FLAG_DEFALUT | ( pTag->ForExpr ? NTX_FLAG_FORITEM : 0 ) |
      ( pIndex->Owner->bLockType == HB_SET_DBFLOCK_CL53EXT ? NTX_FLAG_EXTLOCK : 0 );

   HB_PUT_LE_UINT16( Header.type, type );
   HB_PUT_LE_UINT16( Header.version, pIndex->Version );
   HB_PUT_LE_UINT32( Header.root, pTag->RootBlock );
   HB_PUT_LE_UINT32( Header.next_page, pIndex->NextAvail );

   hb_fsSeek( pIndex->DiskFile, 0, FS_SET );

   if( fFull )
   {
      memset( ( BYTE * ) &Header + 12, 0, sizeof( NTXHEADER ) - 12 );

      HB_PUT_LE_UINT16( Header.item_size, pTag->KeyLength + 8 );
      HB_PUT_LE_UINT16( Header.key_size,  pTag->KeyLength );
      HB_PUT_LE_UINT16( Header.key_dec,   pTag->KeyDec );
      HB_PUT_LE_UINT16( Header.max_item,  pTag->MaxKeys );
      HB_PUT_LE_UINT16( Header.half_page, pTag->MaxKeys >> 1 );
      Header.unique[0]  = pTag->UniqueKey ? 1 : 0;
      Header.descend[0] = pTag->AscendKey ? 0 : 1;
      Header.custom[0]  = pTag->Custom    ? 1 : 0;
      strncpy( ( char * ) Header.key_expr, pTag->KeyExpr, NTX_MAX_KEY );
      if( pTag->ForExpr )
         strncpy( ( char * ) Header.for_expr, pTag->ForExpr, NTX_MAX_KEY );
      if( pTag->fTagName )
         strncpy( ( char * ) Header.tag_name, pTag->TagName, NTX_MAX_TAGNAME );
      uiSize = sizeof( NTXHEADER );
   }

   hb_fsWrite( pIndex->DiskFile, ( BYTE* ) &Header, uiSize );
   pTag->TagChanged = FALSE;
   pIndex->fFlush = TRUE;
}

/*
 * read index header and check for concurrent access
 */
static ERRCODE hb_ntxHeaderRead( LPNTXINDEX pIndex )
{
   NTXHEADER Header;
   USHORT usVersion;
   ULONG ulRootPage;

   hb_fsSeek( pIndex->DiskFile, 0, FS_SET );
   if( hb_fsRead( pIndex->DiskFile, ( BYTE* ) &Header, 12 ) != 12 )
      return FAILURE;

   usVersion = HB_GET_LE_UINT16( Header.version );
   ulRootPage = HB_GET_LE_UINT32( Header.root );
   pIndex->NextAvail = HB_GET_LE_UINT32( Header.next_page );
   if ( pIndex->Version != usVersion ||
        ulRootPage != pIndex->CompoundTag->RootBlock )
   {
      hb_ntxDiscardBuffers( pIndex->CompoundTag );
      pIndex->Version = usVersion;
      pIndex->CompoundTag->RootBlock = ulRootPage;
   }
   return SUCCESS;
}

/*
 * load index header and create new tag
 */
static ERRCODE hb_ntxHeaderLoad( LPNTXINDEX pIndex , char *szTagName )
{
   NTXHEADER Header;
   LPTAGINFO pTag;
   PHB_ITEM pKeyExp, pForExp = NULL;
   USHORT usType;

   hb_fsSeek( pIndex->DiskFile, 0, FS_SET );
   if( hb_fsRead( pIndex->DiskFile, (BYTE*) &Header, sizeof(NTXHEADER) ) != sizeof(NTXHEADER) )
      return FAILURE;

   usType = HB_GET_LE_UINT16( Header.type );

   if ( ( usType & ~NTX_FLAG_MASK ) || Header.key_expr[0] < 0x20 )
      return FAILURE;

   if( SELF_COMPILE( ( AREAP ) pIndex->Owner, Header.key_expr ) == FAILURE )
      return FAILURE;
   pKeyExp = pIndex->Owner->valResult;
   pIndex->Owner->valResult = NULL;

   if( usType & NTX_FLAG_FORITEM && Header.for_expr[0] >= 0x20 )
   {
      if( SELF_COMPILE( ( AREAP ) pIndex->Owner, Header.for_expr ) == FAILURE )
      {
         hb_ntxDestroyExp( pKeyExp );
         return FAILURE;
      }
      pForExp = pIndex->Owner->valResult;
      pIndex->Owner->valResult = NULL;
   }

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );

   pTag->Owner = pIndex;
   pIndex->CompoundTag = pTag;
   pIndex->Version = HB_GET_LE_UINT16( Header.version );
   pTag->RootBlock = HB_GET_LE_UINT32( Header.root );
   pIndex->NextAvail = HB_GET_LE_UINT32( Header.next_page );
   pTag->TagBlock = NTX_DUMMYNODE;

   pTag->TagName = (char *) hb_xgrab( NTX_MAX_TAGNAME + 1 );
   pTag->fTagName = ( Header.tag_name[0] > 20 );
   hb_strncpyUpper( pTag->TagName, pTag->fTagName ? (char *) Header.tag_name :
                                                szTagName, NTX_MAX_TAGNAME );
   pTag->KeyExpr = (char *) hb_xgrab( NTX_MAX_KEY + 1 );
   hb_strncpy( pTag->KeyExpr, (char *) Header.key_expr, NTX_MAX_KEY );
   pTag->nField = hb_ntxFieldIndex( pIndex->Owner, pTag->KeyExpr );
   if( pForExp )
   {
      pTag->ForExpr = (char *) hb_xgrab( NTX_MAX_KEY + 1 );
      hb_strncpy( pTag->ForExpr, (char *) Header.for_expr, NTX_MAX_KEY );
   }
   pTag->pKeyItem = pKeyExp;
   pTag->pForItem = pForExp;

   pTag->KeyLength = HB_GET_LE_UINT16( Header.key_size );
   pTag->KeyDec    = HB_GET_LE_UINT16( Header.key_dec );
   pTag->MaxKeys   = HB_GET_LE_UINT16( Header.max_item );

   pTag->UniqueKey = Header.unique[0] != 0;
   pTag->AscendKey = Header.descend[0] == 0;
   pTag->Custom    = Header.custom[0] != 0;
   pTag->fUsrDescend = !pTag->AscendKey;
   pTag->KeyType = hb_ntxGetKeyType( pTag );

   pTag->CurKeyInfo = hb_ntxKeyNew( NULL, pTag->KeyLength );
   pTag->stackSize  = NTX_STACKSIZE;
   pTag->stack = (LPTREESTACK) hb_xgrab( sizeof(TREE_STACK) * NTX_STACKSIZE );

   pTag->Memory = FALSE;
   pTag->ulPages = pTag->ulPageLast = 0;
   pTag->ulPagesDepth = NTX_PAGES_PER_TAG;
   pTag->pages = (LPPAGEINFO) hb_xgrab( sizeof(HB_PAGEINFO) * NTX_PAGES_PER_TAG );
   memset( pTag->pages, 0, sizeof(HB_PAGEINFO) * NTX_PAGES_PER_TAG );

   if ( usType & NTX_FLAG_EXTLOCK )
   {
      pIndex->Owner->bLockType = HB_SET_DBFLOCK_CL53EXT;
   }
   else if ( ! pIndex->Owner->bLockType )
   {
      pIndex->Owner->bLockType = usType & NTX_FLAG_EXTLOCK ?
                           HB_SET_DBFLOCK_CL53EXT : HB_SET_DBFLOCK_CLIP;
   }
   return SUCCESS;
}

/*
 * create the new tag structure
 */
static LPTAGINFO hb_ntxTagNew( LPNTXINDEX pIndex,
                               char * szTagName, BOOL fTagName,
                               char *szKeyExpr, PHB_ITEM pKeyExpr,
                               BYTE bKeyType, USHORT uiKeyLen, USHORT uiKeyDec,
                               char *szForExp, PHB_ITEM pForExp,
                               BOOL fAscendKey, BOOL fUnique, BOOL fCustom,
                               BOOL fMemory )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   pTag->TagName = szTagName;
   pTag->fTagName = fTagName;
   pTag->Owner = pIndex;
   if( szKeyExpr )
   {
      pTag->KeyExpr = (char *) hb_xgrab( NTX_MAX_KEY + 1 );
      hb_strncpy( pTag->KeyExpr, szKeyExpr, NTX_MAX_KEY );
   }
   if( szForExp )
   {
      pTag->ForExpr = (char *) hb_xgrab( NTX_MAX_KEY + 1 );
      hb_strncpy( pTag->ForExpr, szForExp, NTX_MAX_KEY );
   }
   pTag->nField = hb_ntxFieldIndex( pIndex->Owner, pTag->KeyExpr );
   pTag->pKeyItem = pKeyExpr;
   pTag->pForItem = pForExp;
   pTag->AscendKey = fAscendKey;
   pTag->fUsrDescend = !pTag->AscendKey;
   pTag->UniqueKey = fUnique;
   pTag->Custom = fCustom;
   pTag->KeyType = bKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->KeyDec = uiKeyDec;
   /*
    * TODO: keep during page update the offset to 'MaxKeys' key fixed
    * so we will be able to store 1 key more in the page
    */
   pTag->MaxKeys = ( NTXBLOCKSIZE - 2 ) / ( uiKeyLen + 10 ) - 1;

   /* TODO: is it necessary? It should not interact with well implemented
      algorithm */
   if( pTag->MaxKeys & 0x01 && pTag->MaxKeys > 2 )
      pTag->MaxKeys--;

   pTag->CurKeyInfo = hb_ntxKeyNew( NULL, pTag->KeyLength );
   pTag->stackSize = NTX_STACKSIZE;
   pTag->stack = (LPTREESTACK) hb_xgrab( sizeof(TREE_STACK) * NTX_STACKSIZE );
   pTag->ulPages = pTag->ulPageLast = 0;
   pTag->ulPagesDepth = NTX_PAGES_PER_TAG;
   if( !fMemory )
   {
      pTag->pages = (LPPAGEINFO) hb_xgrab( sizeof(HB_PAGEINFO) * NTX_PAGES_PER_TAG );
      memset( pTag->pages, 0, sizeof(HB_PAGEINFO) * NTX_PAGES_PER_TAG );
   }
   pTag->Memory = fMemory;

   return pTag;
}

/*
 * create new index structure
 */
static LPNTXINDEX hb_ntxIndexNew( NTXAREAP pArea )
{
   LPNTXINDEX pIndex;

   pIndex = ( LPNTXINDEX ) hb_xgrab( sizeof( NTXINDEX ) );
   memset( pIndex, 0, sizeof( NTXINDEX ) );

   pIndex->DiskFile = FS_ERROR;
   pIndex->Owner = pArea;
   pIndex->NextAvail = 0;
   pIndex->Version = 0;
   return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void hb_ntxIndexFree( LPNTXINDEX pIndex )
{
   LPTAGINFO pTag = pIndex->CompoundTag;

   if ( pTag )
   {
      hb_ntxFreePageBuffer( pTag );
      hb_xfree( pTag->TagName );

      if( pTag->KeyExpr )
         hb_xfree( pTag->KeyExpr );
      if( pTag->ForExpr )
         hb_xfree( pTag->ForExpr );
      if( pTag->pKeyItem )
         hb_ntxDestroyExp( pTag->pKeyItem );
      if( pTag->pForItem )
         hb_ntxDestroyExp( pTag->pForItem );
      if( pTag->HotKeyInfo )
         hb_ntxKeyFree( pTag->HotKeyInfo );
      hb_ntxKeyFree( pTag->CurKeyInfo );
      hb_ntxTagClearScope( pTag, 0 );
      hb_ntxTagClearScope( pTag, 1 );
      hb_xfree( pTag->stack );
      hb_xfree( pTag->pages );
      hb_xfree( pTag );
   }
   if ( pIndex->DiskFile != FS_ERROR )
      hb_fsClose( pIndex->DiskFile );
   if ( pIndex->IndexName )
      hb_xfree( pIndex->IndexName );
   hb_xfree( pIndex );
}

/*
 * write modified pages to index file
 */
static void hb_ntxTagFlush( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = pTag->pages;
   ULONG u;

   for( u = 0; u < pTag->ulPages; u++, pPage++ )
      if( pPage->Changed )
         hb_ntxPageSave( pTag, pPage );

   if( pTag->TagChanged )
      hb_ntxHeaderSave( pTag->Owner, FALSE );
}

/*
 * lock index for reading (shared lock)
 */
static BOOL hb_ntxIndexLockRead( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->Owner;
   BOOL fOK;

   if ( pIndex->lockRead > 0 || pIndex->lockWrite > 0 ||
        !pIndex->Owner->fShared || pTag->Memory )
   {
      fOK = TRUE;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                        FL_LOCK | FLX_SHARED | FLX_WAIT, &pIndex->ulLockPos );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if ( fOK )
      {
         if ( hb_ntxHeaderRead( pIndex ) == FAILURE )
         {
            hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
            hb_ntxErrorRT( pIndex->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                           pIndex->IndexName, hb_fsError(), 0 );
            return FALSE;
         }
      }
   }
   if ( fOK )
      pIndex->lockRead++;
   else
      hb_ntxErrorRT( pIndex->Owner, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0 );

   return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static BOOL hb_ntxIndexLockWrite( LPTAGINFO pTag, BOOL fCheck )
{
   LPNTXINDEX pIndex = pTag->Owner;
   BOOL fOK;

/*
   if ( pIndex->fReadonly )
      hb_errInternal( 9101, "hb_ntxIndexLockWrite: readonly index.", "", "" );
*/
   if ( pIndex->lockRead )
      hb_errInternal( 9105, "hb_ntxIndexLockWrite: writeLock after readLock.", "", "" );

   if ( pIndex->lockWrite > 0 ||
        !pIndex->Owner->fShared || pTag->Memory )
   {
      fOK = TRUE;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                               FL_LOCK | FLX_WAIT, &pIndex->ulLockPos );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK && fCheck )
      {
         if( hb_ntxHeaderRead( pIndex ) == FAILURE )
         {
            hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
            hb_ntxErrorRT( pIndex->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                           pIndex->IndexName, hb_fsError(), 0 );
            return FALSE;
         }
      }
   }
   if ( fOK )
      pIndex->lockWrite++;
   else
      hb_ntxErrorRT( pIndex->Owner, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0 );

   return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static BOOL hb_ntxIndexUnLockRead( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->Owner;
   BOOL fOK;

#ifdef HB_NTX_DEBUG
   hb_ntxTagCheckBuffers( pTag );
#endif

   pIndex->lockRead--;
   if ( pIndex->lockRead < 0 )
      hb_errInternal( 9106, "hb_ntxIndexUnLockRead: bad count of locks.", "", "" );

   if ( pIndex->lockRead || pIndex->lockWrite ||
        !pIndex->Owner->fShared || pTag->Memory )
   {
      fOK = TRUE;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
   }
   if ( !fOK )
      hb_errInternal( 9108, "hb_ntxIndexUnLockRead: unlock error.", "", "" );

   return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static BOOL hb_ntxIndexUnLockWrite( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->Owner;
   BOOL fOK;

#ifdef HB_NTX_DEBUG
   hb_ntxTagCheckBuffers( pTag );
#endif

   pIndex->lockWrite--;
   if ( pIndex->lockWrite < 0 )
      hb_errInternal( 9106, "hb_ntxIndexUnLockWrite: bad count of locks.", "", "" );
   if ( pIndex->lockRead )
      hb_errInternal( 9105, "hb_ntxIndexUnLockWrite: writeUnLock before readUnLock.", "", "" );

   if ( !pTag->Memory )
      hb_ntxTagFlush( pTag );

   if ( pIndex->lockWrite ||
        !pIndex->Owner->fShared || pTag->Memory )
   {
      fOK = TRUE;
   }
   else
   {
      fOK = hb_dbfLockIdxFile( pIndex->DiskFile, pIndex->Owner->bLockType,
                               FL_UNLOCK, &pIndex->ulLockPos );
   }
   if ( !fOK )
      hb_errInternal( 9108, "hb_ntxIndexUnLockWrite: unlock error.", "", "" );

   return fOK;
}

/*
 * retrive key from page
 */
static void hb_ntxPageGetKey( LPPAGEINFO pPage, USHORT uiKey, LPKEYINFO pKey, USHORT uiLen )
{
   if ( uiKey < pPage->uiKeys )
   {
      memcpy( pKey->key, hb_ntxGetKeyVal( pPage, uiKey ), uiLen );
      pKey->Xtra = hb_ntxGetKeyRec( pPage, uiKey );
      pKey->Tag = pPage->Page;
   }
   else
   {
      pKey->Xtra = pKey->Tag = 0;
   }
}

/*
 * set next page and key in page path
 */
static void hb_ntxTagSetPageStack( LPTAGINFO pTag, ULONG ulPage, USHORT uiKey )
{
   if ( pTag->stackLevel == pTag->stackSize )
   {
      pTag->stackSize += 32;
      pTag->stack = ( LPTREESTACK ) hb_xrealloc( pTag->stack,
                                    sizeof( TREE_STACK ) * pTag->stackSize );
   }
   pTag->stack[ pTag->stackLevel ].page = ulPage;
   pTag->stack[ pTag->stackLevel++ ].ikey = uiKey;
   if ( pTag->stackLevel > pTag->stackDepth )
      pTag->stackDepth = pTag->stackLevel;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO hb_ntxPageTopMove( LPTAGINFO pTag, ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if ( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if ( ! pPage )
         return NULL;
#ifdef HB_NTX_DEBUG_EXT
      if ( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
      {
         hb_errInternal( 9201, "hb_ntxPageTopMove: index corrupted.", "", "" );
         return pPage;
      }
#endif
      ulPage = hb_ntxGetKeyPage( pPage, 0 );
      hb_ntxTagSetPageStack( pTag, pPage->Page, 0 );
   }
   while ( ulPage );

   return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO hb_ntxPageBottomMove( LPTAGINFO pTag, ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if ( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if ( ! pPage )
         return NULL;
#ifdef HB_NTX_DEBUG_EXT
      if ( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
      {
         hb_errInternal( 9201, "hb_ntxPageBottomMove: index corrupted.", "", "" );
         return pPage;
      }
#endif
      ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
      hb_ntxTagSetPageStack( pTag, pPage->Page, pPage->uiKeys - ( ulPage ? 0 : 1 ) );
   }
   while ( ulPage );

   return pPage;
}

/*
 * set page path to the first key in tag
 */
static BOOL hb_ntxTagTopKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = hb_ntxPageTopMove( pTag, 0 );
   if ( ! pPage )
      return FALSE;
   hb_ntxPageGetKey( pPage, 0, pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   hb_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * set page path to the last key in tag
 */
static BOOL hb_ntxTagBottomKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = hb_ntxPageBottomMove( pTag, 0 );
   if ( ! pPage )
      return FALSE;
   hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                     pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   hb_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * update page path to the next key in tag
 */
static BOOL hb_ntxTagNextKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   ULONG ulPage = 0;

   if ( iLevel >= 0 )
   {
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if ( ! pPage )
         return FALSE;
      if ( pTag->stack[ iLevel ].ikey < pPage->uiKeys )
         ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey + 1 );
      if ( ulPage || pTag->stack[ iLevel ].ikey + 1 < pPage->uiKeys )
      {
         pTag->stack[ iLevel ].ikey++;
         if ( ulPage )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageTopMove( pTag, ulPage );
            if ( ! pPage )
               return FALSE;
         }
      }
      else
      {
         while ( --iLevel >= 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if ( ! pPage )
               return FALSE;
            if ( pTag->stack[ iLevel ].ikey < pPage->uiKeys )
               break;
         }
         if ( iLevel < 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            return FALSE;
         }
         pTag->stackLevel = iLevel + 1;
      }
      hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      hb_ntxPageRelease( pTag, pPage );
      return TRUE;
   }
   return FALSE;
}

/*
 * update page path to the previous key in tag
 */
static BOOL hb_ntxTagPrevKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;
   LPPAGEINFO pPage;
   ULONG ulPage;

   if ( iLevel >= 0 )
   {
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if ( ! pPage )
         return FALSE;
      ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if ( ulPage )
      {
         hb_ntxPageRelease( pTag, pPage );
         pPage = hb_ntxPageBottomMove( pTag, ulPage );
         if ( ! pPage )
            return FALSE;
      }
      else if ( pTag->stack[ iLevel ].ikey )
      {
         pTag->stack[ iLevel ].ikey--;
      }
      else
      {
         while ( --iLevel >= 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if ( ! pPage )
               return FALSE;
            if ( pTag->stack[ iLevel ].ikey )
            {
               pTag->stack[ iLevel ].ikey--;
               break;
            }
         }
         if ( iLevel < 0 )
         {
            hb_ntxPageRelease( pTag, pPage );
            return FALSE;
         }
         pTag->stackLevel = iLevel + 1;
      }
      hb_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      hb_ntxPageRelease( pTag, pPage );
      return TRUE;
   }
   return FALSE;
}

/*
 * find a key value in page
 */
static int hb_ntxPageKeyFind( LPTAGINFO pTag, LPPAGEINFO pPage,
                              char* key, SHORT keylen, BOOL fNext, BOOL *fStop )
{
   SHORT iLast = -1, iBegin = 0, iEnd = pPage->uiKeys - 1, k, i;

   *fStop = FALSE;
   while ( iBegin <= iEnd )
   {
      i = ( iBegin + iEnd ) >> 1;
      k = hb_ntxValCompare( pTag, key, keylen, hb_ntxGetKeyVal( pPage, i ),
                            pTag->KeyLength, FALSE );
      if( !pTag->AscendKey )
         k = -k;
      if( fNext ? k >= 0 : k > 0 )
         iBegin = i + 1;
      else
      {
         if ( k == 0 )
            *fStop = TRUE;
         iLast = i;
         iEnd = i - 1;
      }
   }
   return iLast >= 0 ? iLast : pPage->uiKeys;
}

/*
 * find a record in page starting from given key
 */
static BOOL hb_ntxPageFindRecNo( LPPAGEINFO pPage, int * iStart, ULONG ulRecno )
{
   int iKey = *iStart;
   while ( iKey < pPage->uiKeys )
   {
      if ( hb_ntxGetKeyRec( pPage, iKey ) == ulRecno )
      {
         *iStart = iKey;
         return TRUE;
      }
      iKey++;
   }
   return FALSE;
}

/*
 * set page path to given key in tag
 */
static BOOL hb_ntxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey, USHORT uiLen )
{
   LPPAGEINFO pPage = NULL;
   ULONG ulPage = 0, ulRecNo = 0;
   int iKey;
   BOOL fStop = FALSE, fNext = FALSE, fPrev = FALSE, fOut = FALSE;

   if ( pKey->Tag == NTX_MAX_REC_NUM )          /* for key add */
      fNext = TRUE;
   else if ( pKey->Xtra == NTX_MAX_REC_NUM )    /* for seek last */
      fNext = fPrev = TRUE;
   else if ( pKey->Xtra != NTX_IGNORE_REC_NUM ) /* for key del and current key */
      ulRecNo = pKey->Xtra;
   /* else -> normal seek */

   pTag->stackLevel = pTag->stackDepth = 0;
   do
   {
      if ( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, ulPage );
      if ( ! pPage )
         return FALSE;
      iKey = hb_ntxPageKeyFind( pTag, pPage, pKey->key, uiLen, fNext, &fStop );
      ulPage = hb_ntxGetKeyPage( pPage, iKey );
      hb_ntxTagSetPageStack( pTag, pPage->Page, iKey );
   } while ( ulPage != 0 );

   if ( ulRecNo ) /* small hack - should speedup in some cases */
   {
      if ( hb_ntxPageFindRecNo( pPage, &iKey, ulRecNo ) )
         pTag->stack[ pTag->stackLevel - 1 ].ikey = iKey;
   }

   hb_ntxPageGetKey( pPage, iKey, pTag->CurKeyInfo, pTag->KeyLength );
   hb_ntxPageRelease( pTag, pPage );

   if ( ulRecNo )
   {
      fStop = TRUE;
      while ( fStop && ulRecNo != pTag->CurKeyInfo->Xtra )
      {
         if ( ! hb_ntxTagNextKey( pTag ) ) /* Tag EOF */
         {
            fOut = TRUE;
            fStop = FALSE;
         }
         else
         {
            fStop = hb_ntxValCompare( pTag, pKey->key, uiLen,
                                      pTag->CurKeyInfo->key, pTag->KeyLength,
                                      FALSE ) == 0;
         }
      }
   }
   else if ( fPrev )
   {
      if ( !hb_ntxTagPrevKey( pTag ) )
      {
         fOut = TRUE;
         fStop = FALSE;
      }
      else
      {
         fStop = hb_ntxValCompare( pTag, pKey->key, uiLen, pTag->CurKeyInfo->key,
                                   pTag->KeyLength, FALSE ) == 0;
      }
   }
   else if ( !fNext && !fStop && pTag->CurKeyInfo->Xtra == 0 )
   {
      if ( ! hb_ntxTagNextKey( pTag ) ) /* Tag EOF */
      {
         fOut = TRUE;
         fStop = FALSE;
      }
      else
      {
         fStop = hb_ntxValCompare( pTag, pKey->key, uiLen,
                                   pTag->CurKeyInfo->key, pTag->KeyLength,
                                   FALSE ) == 0;
      }
   }

   pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->Xtra == 0;

   return fStop;
}

/*
 * set key in the given tag page
 */
static void hb_ntxPageKeySet( LPTAGINFO pTag, LPPAGEINFO pPage, USHORT uiPos,
                              ULONG ulPage, ULONG ulRec, char * keyVal )
{
   hb_ntxSetKeyPage( pPage, uiPos, ulPage );
   hb_ntxSetKeyRec( pPage, uiPos, ulRec );
   memcpy( hb_ntxGetKeyVal( pPage, uiPos ), keyVal, pTag->KeyLength );
   pPage->Changed = TRUE;
}

/*
 * add key to tag page
 */
static void hb_ntxPageKeyAdd( LPTAGINFO pTag, LPPAGEINFO pPage, USHORT uiPos,
                              ULONG ulPage, ULONG ulRec, char * keyVal )
{
   USHORT u, ntmp = hb_ntxGetKeyOffset( pPage, pPage->uiKeys + 1 );

   /* TODO: update to keep last key pointer fixed */
   for( u = pPage->uiKeys + 1; u > uiPos; u-- )
   {
      hb_ntxSetKeyOffset( pPage, u, hb_ntxGetKeyOffset( pPage, u - 1 ) );
   }
   hb_ntxSetKeyOffset( pPage, uiPos, ntmp );
   pPage->uiKeys++;

   hb_ntxPageKeySet( pTag, pPage, uiPos, ulPage, ulRec, keyVal );
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pPage, pTag, uiPos, 41 );
#endif
}

/*
 * del key from the page
 */
static void hb_ntxPageKeyDel( LPPAGEINFO pPage, USHORT uiPos )
{

   USHORT u, ntmp = hb_ntxGetKeyOffset( pPage, uiPos );

   /* TODO: update to keep last key pointer fixed */
   for( u = uiPos; u < pPage->uiKeys; u++ )
      hb_ntxSetKeyOffset( pPage, u, hb_ntxGetKeyOffset( pPage, u + 1 ) );
   hb_ntxSetKeyOffset( pPage, pPage->uiKeys, ntmp );

   pPage->uiKeys--;
   pPage->Changed = TRUE;
}

/*
 * split single page into two and return key to the new one
 */
static LPKEYINFO hb_ntxPageSplit( LPTAGINFO pTag, LPPAGEINFO pPage,
                                  LPKEYINFO pKey, USHORT uiPos )
{
   LPPAGEINFO pNewPage = hb_ntxPageNew( pTag );
   LPKEYINFO pKeyNew;
   USHORT uiKeys = pPage->uiKeys + 1, uiLen = pTag->KeyLength + 8,
          i, j, u, uiHalf;
   ULONG ulPage;

   if ( ! pNewPage )
      return NULL;
   pKeyNew = hb_ntxKeyNew( NULL, pTag->KeyLength );

   uiHalf = uiKeys >> 1;

   j = 0;
   while ( pNewPage->uiKeys < uiHalf )
   {
      if ( pNewPage->uiKeys == uiPos )
      {
         hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
         hb_ntxSetKeyRec( pNewPage, pNewPage->uiKeys, pKey->Xtra );
         memcpy( hb_ntxGetKeyVal( pNewPage, pNewPage->uiKeys ), pKey->key, pTag->KeyLength );
      }
      else
      {
         memcpy( hb_ntxGetKeyPtr( pNewPage, pNewPage->uiKeys ),
                 hb_ntxGetKeyPtr( pPage, j ), uiLen );
         j++;
      }
      pNewPage->uiKeys++;
   }

   if ( uiHalf == uiPos )
   {
      pKeyNew->Xtra = pKey->Xtra;
      memcpy( pKeyNew->key, pKey->key, pTag->KeyLength );
      hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
   }
   else
   {
      pKeyNew->Xtra = hb_ntxGetKeyRec( pPage, j );
      memcpy( pKeyNew->key, hb_ntxGetKeyVal( pPage, j ), pTag->KeyLength );
      hb_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, hb_ntxGetKeyPage( pPage, j ) );
      j++;
   }
   pKeyNew->Tag = pNewPage->Page;

   i = 0;
   while ( ++uiHalf < uiKeys )
   {
      if ( uiHalf == uiPos )
      {
         hb_ntxSetKeyPage( pPage, i, pKey->Tag );
         hb_ntxSetKeyRec( pPage, i, pKey->Xtra );
         memcpy( hb_ntxGetKeyVal( pPage, i ), pKey->key, pTag->KeyLength );
      }
      else
      {
         u = hb_ntxGetKeyOffset( pPage, j );
         hb_ntxSetKeyOffset( pPage, j, hb_ntxGetKeyOffset( pPage, i ) );
         hb_ntxSetKeyOffset( pPage, i, u );
         j++;
      }
      i++;
   }
   ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
   hb_ntxSetKeyPage( pPage, pPage->uiKeys, 0 );
   hb_ntxSetKeyPage( pPage, i, ulPage );
   pPage->uiKeys = i;

   pPage->Changed = pNewPage->Changed = TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pNewPage, pTag, uiPos, 1 );
   hb_ntxPageCheckKeys( pPage, pTag, uiPos - pNewPage->uiKeys, 2 );
#endif
   hb_ntxPageRelease( pTag, pNewPage );

   return pKeyNew;
}

/*
 * join two neighbour pages and update the parent page key
 */
static void hb_ntxPageJoin( LPTAGINFO pTag, LPPAGEINFO pBasePage, USHORT uiPos,
                            LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   USHORT uiLen = pTag->KeyLength + 8, i;

   hb_ntxSetKeyRec( pFirst, pFirst->uiKeys, hb_ntxGetKeyRec( pBasePage, uiPos ) );
   memcpy( hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
           hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
   pFirst->uiKeys++;
   hb_ntxPageKeyDel( pBasePage, uiPos );
   hb_ntxSetKeyPage( pBasePage, uiPos, pFirst->Page );
   for ( i = 0; i < pLast->uiKeys; i++ )
   {
      memcpy( hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
              hb_ntxGetKeyPtr( pLast, i ), uiLen );
      pFirst->uiKeys++;
   }
   hb_ntxSetKeyPage( pFirst, pFirst->uiKeys, hb_ntxGetKeyPage( pLast, pLast->uiKeys ) );
   pLast->uiKeys = 0;
   hb_ntxPageFree( pTag, pLast );
   pFirst->Changed = pLast->Changed = TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 11 );
   hb_ntxPageCheckKeys( pFirst, pTag, 0, 12 );
#endif
}

/*
 * balance keys in two neighbour pages and update the parent page key
 */
static void hb_ntxBalancePages( LPTAGINFO pTag, LPPAGEINFO pBasePage, USHORT uiPos,
                                LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   USHORT uiLen = pTag->KeyLength + 8, n;
   int i, j, iMove = ( ( pFirst->uiKeys + pLast->uiKeys + 1 ) >> 1 ) - pFirst->uiKeys;

   /*
    * such situation should not exist even max keys, though it does not cost
    * more and I want to be able to call hb_ntxBalancePages in any case for
    * some advanced balancing
    */
   if ( iMove == 0 )
      return;

#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 31 );
   hb_ntxPageCheckKeys( pFirst, pTag, iMove, 32 );
   hb_ntxPageCheckKeys( pLast, pTag, iMove, 33 );
#endif

   if ( iMove > 0 )
   {
      hb_ntxSetKeyRec( pFirst, pFirst->uiKeys, hb_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
              hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      pFirst->uiKeys++;
      i = 0;
      while ( --iMove )
      {
         memcpy( hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
                 hb_ntxGetKeyPtr( pLast, i ), uiLen );
         pFirst->uiKeys++;
         i++;
      }
      hb_ntxSetKeyRec( pBasePage, uiPos, hb_ntxGetKeyRec( pLast, i ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, uiPos ),
              hb_ntxGetKeyVal( pLast, i ), pTag->KeyLength );
      hb_ntxSetKeyPage( pFirst, pFirst->uiKeys, hb_ntxGetKeyPage( pLast, i ) );
      i++;
      pLast->uiKeys -= i;
      /* TODO: update to keep last key pointer fixed */
      for ( j = 0; j <= pLast->uiKeys; j++ )
      {
         n = hb_ntxGetKeyOffset( pLast, j );
         hb_ntxSetKeyOffset( pLast, j, hb_ntxGetKeyOffset( pLast, j + i ) );
         hb_ntxSetKeyOffset( pLast, j + i, n );
      }
   }
   else
   {
      /* TODO: update to keep last key pointer fixed */
      for ( j = pLast->uiKeys; j >= 0; j-- )
      {
         n = hb_ntxGetKeyOffset( pLast, j - iMove );
         hb_ntxSetKeyOffset( pLast, j - iMove, hb_ntxGetKeyOffset( pLast, j ) );
         hb_ntxSetKeyOffset( pLast, j, n );
      }
      i = -iMove - 1;
      hb_ntxSetKeyRec( pLast, i, hb_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( hb_ntxGetKeyVal( pLast, i ),
              hb_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      hb_ntxSetKeyPage( pLast, i, hb_ntxGetKeyPage( pFirst, pFirst->uiKeys ) );
      while ( --i >= 0 )
      {
         pFirst->uiKeys--;
         memcpy( hb_ntxGetKeyPtr( pLast, i ),
                 hb_ntxGetKeyPtr( pFirst, pFirst->uiKeys ), uiLen );
      }
      pLast->uiKeys -= iMove;
      pFirst->uiKeys--;
      hb_ntxSetKeyRec( pBasePage, uiPos, hb_ntxGetKeyRec( pFirst, pFirst->uiKeys ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, uiPos ),
              hb_ntxGetKeyVal( pFirst, pFirst->uiKeys ), pTag->KeyLength );
   }
   pFirst->Changed = pLast->Changed = pBasePage->Changed = TRUE;
#ifdef HB_NTX_DEBUG
   hb_ntxPageCheckKeys( pBasePage, pTag, uiPos, 21 );
   hb_ntxPageCheckKeys( pFirst, pTag, iMove, 22 );
   hb_ntxPageCheckKeys( pLast, pTag, iMove, 23 );
#endif
}

/*
 * add key to the index at the curret page path
 */
static BOOL hb_ntxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iKey;
   LPPAGEINFO pPage = NULL;
   LPKEYINFO pNewKey = NULL;
   ULONG ulPage;

   if ( pTag->UniqueKey )
   {
      ULONG ulRecNo = pKey->Xtra;
      BOOL fFound;

      pKey->Xtra = NTX_IGNORE_REC_NUM;
      fFound = hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Xtra = ulRecNo;
      if ( fFound )
         return FALSE;

      iLevel = pTag->stackLevel - 1;
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if ( ! pPage )
         return FALSE;
      ulPage = hb_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if ( ulPage )
      {
         hb_ntxPageRelease( pTag, pPage );
         pPage = hb_ntxPageBottomMove( pTag, ulPage );
         if ( ! pPage )
            return FALSE;
         iLevel = pTag->stackLevel - 1;
         if ( pTag->stack[ iLevel ].ikey < pPage->uiKeys )
            pTag->stack[ iLevel ].ikey++;
      }
   }
   else
   {
      pKey->Tag = NTX_MAX_REC_NUM;
      hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Tag = 0;
      iLevel = pTag->stackLevel - 1;
   }

   while ( iLevel >= 0 && pKey )
   {
      if ( pPage )
         hb_ntxPageRelease( pTag, pPage );
      pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if ( ! pPage )
         return FALSE;
      iKey = pTag->stack[ iLevel ].ikey;
      if( pPage->uiKeys < pTag->MaxKeys )
      {
         hb_ntxPageKeyAdd( pTag, pPage, iKey, pKey->Tag, pKey->Xtra, pKey->key );
         pKey = NULL;
      }
      else
      {
         pKey = hb_ntxPageSplit( pTag, pPage, pKey, iKey );
         if ( pNewKey )
            hb_ntxKeyFree( pNewKey );
         pNewKey = pKey;
      }
      iLevel--;
   }
   hb_ntxPageRelease( pTag, pPage );
   if ( pKey )
   {
      pPage = hb_ntxPageNew( pTag );
      if ( ! pPage )
         return FALSE;
      hb_ntxPageKeyAdd( pTag, pPage, 0, pKey->Tag, pKey->Xtra, pKey->key );
      hb_ntxSetKeyPage( pPage, 1, pTag->RootBlock );
      pTag->RootBlock = pPage->Page;
      pTag->TagChanged = TRUE;
      hb_ntxPageRelease( pTag, pPage );
   }
   if ( pNewKey )
      hb_ntxKeyFree( pNewKey );
   pTag->stackLevel = 0;
   return TRUE;
}

/*
 * del key at the curret page path from the index
 */
static BOOL hb_ntxTagKeyDel( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iBaseKey, iKey;
   LPPAGEINFO pBasePage, pPage;
   ULONG ulPage;

   pKey->Tag = 0;
   if ( ! hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength ) )
      return FALSE;

   iLevel = pTag->stackLevel - 1;

   pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
   if ( ! pPage )
      return FALSE;
   iKey = pTag->stack[ iLevel ].ikey;
   ulPage = hb_ntxGetKeyPage( pPage, iKey );

   if ( ulPage )
   {
      pBasePage = pPage;
      iBaseKey = iKey;
      pPage = hb_ntxPageBottomMove( pTag, ulPage );
      if ( ! pPage )
      {
         hb_ntxPageRelease( pTag, pBasePage );
         return FALSE;
      }
      iLevel = pTag->stackLevel - 1;
      iKey = pTag->stack[ iLevel ].ikey;

      hb_ntxSetKeyRec( pBasePage, iBaseKey, hb_ntxGetKeyRec( pPage, iKey ) );
      memcpy( hb_ntxGetKeyVal( pBasePage, iBaseKey ),
              hb_ntxGetKeyVal( pPage, iKey ), pTag->KeyLength );
      pBasePage->Changed = TRUE;
#ifdef HB_NTX_DEBUG
      hb_ntxPageCheckKeys( pBasePage, pTag, iBaseKey, 61 );
#endif
      hb_ntxPageRelease( pTag, pBasePage );
   }
   hb_ntxPageKeyDel( pPage, iKey );

   while ( iLevel > 0 )
   {
      if( pPage->uiKeys < pTag->MaxKeys >> 1 )
      {
         USHORT uiFirst, uiLast, uiBaseKey;

         pBasePage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel -1 ].page );
         if ( ! pBasePage )
         {
            hb_ntxPageRelease( pTag, pPage );
            return FALSE;
         }
         uiFirst = uiLast = uiBaseKey = pTag->stack[ iLevel -1 ].ikey;
         if ( uiLast < pBasePage->uiKeys && hb_ntxGetKeyPage( pBasePage, uiLast + 1 ) != 0 )
            uiLast++;
         else if ( uiFirst > 0 && hb_ntxGetKeyPage( pBasePage, uiFirst - 1 ) != 0 )
            uiFirst--;

         if ( uiFirst == uiLast )
         {
            if ( pPage->uiKeys == 0 )
            {
               hb_ntxSetKeyPage( pBasePage, uiBaseKey, 0 );
               hb_ntxPageFree( pTag, pPage );
            }
            hb_ntxPageRelease( pTag, pPage );
         }
         else
         {
            LPPAGEINFO pFirst, pLast;

            if ( uiFirst == uiBaseKey )
            {
               pFirst = pPage;
               pLast = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiLast ) );
               if ( ! pLast )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return FALSE;
               }
            }
            else
            {
               pLast = pPage;
               pFirst = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pBasePage, uiFirst ) );
               if ( ! pFirst )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return FALSE;
               }
            }
            if ( pFirst->uiKeys + pLast->uiKeys < pTag->MaxKeys )
               hb_ntxPageJoin( pTag, pBasePage, uiFirst, pFirst, pLast );
            else
               hb_ntxBalancePages( pTag, pBasePage, uiFirst, pFirst, pLast );
            hb_ntxPageRelease( pTag, pFirst );
            hb_ntxPageRelease( pTag, pLast );
         }
         pPage = pBasePage;
      }
      else
         break;
      iLevel--;
   }

   if ( pPage->uiKeys == 0 && pPage->Page == pTag->RootBlock )
   {
      ulPage = hb_ntxGetKeyPage( pPage, 0 );
      if ( ulPage != 0 )
      {
         pTag->RootBlock = ulPage;
         hb_ntxPageFree( pTag, pPage );
      }
   }
   hb_ntxPageRelease( pTag, pPage );
   pTag->stackLevel = 0;
   return TRUE;
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static BOOL hb_ntxCurKeyRefresh( LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if ( !pArea->fPositioned )
   {
      pTag->stackLevel = 0;
      pTag->TagEOF = TRUE;
      pTag->CurKeyInfo->Xtra = 0;
      return FALSE;
   }
   else if ( pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pArea->ulRecNo )
   {
      BYTE buf[ NTX_MAX_KEY ];
      BOOL fBuf = FALSE;
      LPKEYINFO pKey = NULL;
      /* Try to find previous if it's key for the same record */
      if ( pTag->CurKeyInfo->Xtra == pArea->ulRecNo )
      {
         fBuf = TRUE;
         memcpy( buf, pTag->CurKeyInfo->key, pTag->KeyLength );
         pKey = hb_ntxKeyCopy( pKey, pTag->CurKeyInfo, pTag->KeyLength );
         hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      }
      if ( pTag->CurKeyInfo->Xtra != pArea->ulRecNo )
      {
         BOOL fValidBuf = pArea->fValidBuffer;
         /* not found, create new key from DBF and if differs seek again */
         pKey = hb_ntxEvalKey( pKey, pTag );
         if ( !fBuf || memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
         {
            hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if ( pTag->CurKeyInfo->Xtra != pArea->ulRecNo && fValidBuf )
         {
            SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );
            memcpy( buf, pKey->key, pTag->KeyLength );
            pKey = hb_ntxEvalKey( pKey, pTag );
            if ( memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
               hb_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
      }
      hb_ntxKeyFree( pKey );
      return ( pTag->CurKeyInfo->Xtra != 0 && pTag->CurKeyInfo->Xtra == pArea->ulRecNo );
   }
   return TRUE;
}

/*
 * go to the first visiable record in Tag
 */
static void hb_ntxTagGoTop( LPTAGINFO pTag )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
      hb_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if ( pTag->fUsrDescend == pTag->AscendKey )
      hb_ntxTagBottomKey( pTag );
   else
      hb_ntxTagTopKey( pTag );

   pTag->TagBOF = pTag->TagEOF = pTag->CurKeyInfo->Xtra == 0 ||
                                 !hb_ntxKeyInScope( pTag, pTag->CurKeyInfo );
}

/*
 * go to the last visiable record in Tag
 */
static void hb_ntxTagGoBottom( LPTAGINFO pTag )
{
   PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
      hb_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if ( pTag->fUsrDescend == pTag->AscendKey )
      hb_ntxTagTopKey( pTag );
   else
      hb_ntxTagBottomKey( pTag );

   pTag->TagBOF = pTag->TagEOF = pTag->CurKeyInfo->Xtra == 0 ||
                                 !hb_ntxKeyInScope( pTag, pTag->CurKeyInfo );
}

/*
 * skip to Next Key in the Tag
 */
static void hb_ntxTagSkipNext( LPTAGINFO pTag )
{
   pTag->TagBOF = FALSE;

   if ( pTag->stackLevel == 0 )
      pTag->TagEOF = TRUE;
   else if ( ! hb_ntxInTopScope( pTag, pTag->CurKeyInfo->key ) )
      hb_ntxTagGoTop( pTag );
   else if ( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagEOF = !hb_ntxTagPrevKey( pTag );
   else
      pTag->TagEOF = !hb_ntxTagNextKey( pTag );

   if ( ! pTag->TagEOF && ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagEOF = TRUE;
}

/*
 * skip to Previous Key in the Tag
 */
static void hb_ntxTagSkipPrev( LPTAGINFO pTag )
{
   pTag->TagEOF = FALSE;

   if ( pTag->stackLevel == 0 )
      /* TODO: check if this is NTX behavior,
         for sure CDX works in such way */
      hb_ntxTagGoBottom( pTag );
   else if ( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagBOF = !hb_ntxTagNextKey( pTag );
   else
      pTag->TagBOF = !hb_ntxTagPrevKey( pTag );

   if ( ! pTag->TagBOF && ! hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagBOF = TRUE;
}

/*
 * count keys in the given page and all subpages
 */
static ULONG hb_ntxPageCountKeys( LPTAGINFO pTag, ULONG ulPage )
{
   LPPAGEINFO pPage = hb_ntxPageLoad( pTag, ulPage );
   ULONG ulKeys;
   USHORT u;

   if ( ! pPage )
      return 0;

   ulKeys = pPage->uiKeys;
   for( u = 0; u <= pPage->uiKeys; u++ )
   {
      ulPage = hb_ntxGetKeyPage( pPage, u );
      if( ulPage )
         ulKeys += hb_ntxPageCountKeys( pTag, ulPage );
   }
   hb_ntxPageRelease( pTag, pPage );

   return ulKeys;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO hb_ntxFindTag( NTXAREAP pArea, PHB_ITEM lpOrder )
{
   LPTAGINFO pTag = pArea->lpNtxTag;

   if ( ! lpOrder )
      pTag = pArea->lpCurTag;
   else if( pTag && lpOrder )
   {
      if( hb_itemType( lpOrder ) & HB_IT_STRING )
      {
         do
         {
            if( !hb_stricmp( pTag->TagName , lpOrder->item.asString.value ) )
               break;
            pTag = pTag->pNext;
         } while( pTag );
      }
      else
      {
         int iNum = hb_itemGetNI( lpOrder ), iCount = 0;
         while ( pTag && ++iCount != iNum )
            pTag = pTag->pNext;
      }
   }
   return pTag;
}

/*
 * find the given tag number
 */
static int hb_ntxFindTagNum( NTXAREAP pArea, LPTAGINFO pTag )
{
   LPTAGINFO pTagSkip = pArea->lpNtxTag;
   int iNum = 1;

   while ( pTagSkip )
   {
      if ( pTagSkip == pTag )
         return iNum;
      iNum++;
      pTagSkip = pTagSkip->pNext;
   }
   return 0;
}

/*
 * count number of keys in given tag
 */
static ULONG hb_ntxOrdKeyCount( LPTAGINFO pTag )
{
   ULONG ulKeyCount = 0;

   if( ( !pTag->Owner->Owner->fShared || pTag->Memory ) && pTag->keyCount )
      return pTag->keyCount;

   if ( ! hb_ntxIndexLockRead( pTag ) )
      return 0;
   hb_ntxTagRefreshScope( pTag );

   if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen )
   {
      hb_ntxTagGoTop( pTag );
      while ( !pTag->TagEOF )
      {
         ulKeyCount++;
         hb_ntxTagSkipNext( pTag );
      }
   }
   else
   {
      ulKeyCount = hb_ntxPageCountKeys( pTag, 0 );
   }
   pTag->keyCount = ulKeyCount;
   hb_ntxIndexUnLockRead( pTag );
   return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static ULONG hb_ntxOrdKeyNo( LPTAGINFO pTag )
{
   ULONG ulKeyNo = 0;

   if ( ! hb_ntxIndexLockRead( pTag ) )
      return 0;
   hb_ntxTagRefreshScope( pTag );

   if ( hb_ntxCurKeyRefresh( pTag ) )
   {
      if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen )
      {
         if( hb_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
         {
            do
            {
               ulKeyNo++;
               hb_ntxTagSkipPrev( pTag );
            }
            while ( !pTag->TagBOF );
         }
      }
      else
      {
         int iLevel = pTag->stackLevel, iKey;
         LPPAGEINFO pPage;
         ULONG ulPage;

         while ( --iLevel >= 0 )
         {
            pPage = hb_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if ( ! pPage )
               break;
            ulKeyNo += iKey = pTag->stack[ iLevel ].ikey;
            while ( --iKey >= 0 )
            {
               ulPage = hb_ntxGetKeyPage( pPage, iKey );
               if ( ulPage )
                  ulKeyNo += hb_ntxPageCountKeys( pTag, ulPage );
            }
            hb_ntxPageRelease( pTag, pPage );
         }
         ulKeyNo++;
      }
   }
   hb_ntxIndexUnLockRead( pTag );
   return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static BOOL hb_ntxOrdKeyGoto( LPTAGINFO pTag, ULONG ulKeyNo )
{
   if ( ! ulKeyNo || ! hb_ntxIndexLockRead( pTag ) )
      return FALSE;
   hb_ntxTagRefreshScope( pTag );
   hb_ntxTagGoTop( pTag );
   while ( !pTag->TagEOF && --ulKeyNo );
   {
      hb_ntxTagSkipNext( pTag );
   }
   hb_ntxIndexUnLockRead( pTag );
   return TRUE;
}

/*
 * skip to next/previous unique key
 */
static BOOL hb_ntxOrdSkipUnique( LPTAGINFO pTag, LONG lDir )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   BOOL fOut = FALSE, fEof = FALSE, fForward = ( lDir >= 0 );

   pArea->fTop = pArea->fBottom = FALSE;

   if ( hb_ntxIndexLockRead( pTag ) )
   {
      hb_ntxTagRefreshScope( pTag );
      if ( hb_ntxCurKeyRefresh( pTag ) )
      {
         char keyVal[ NTX_MAX_KEY ];
         memcpy( keyVal, pTag->CurKeyInfo->key, pTag->KeyLength );

         do
         {
            if ( fForward )
               hb_ntxTagSkipNext( pTag );
            else
               hb_ntxTagSkipPrev( pTag );
            fOut = pTag->TagEOF || pTag->TagBOF;
         }
         while ( !fOut && hb_ntxValCompare( pTag,
                                       pTag->CurKeyInfo->key, pTag->KeyLength,
                                       keyVal, pTag->KeyLength, TRUE ) == 0 );
      }
      else if ( !fForward && !pArea->fPositioned )
      {
         hb_ntxTagGoBottom( pTag );
         fEof = pTag->TagEOF;
      }
      else
      {
         fOut = TRUE;
      }
      if ( fOut )
      {
         if ( fForward )
            fEof = TRUE;
         else
         {
            hb_ntxTagGoTop( pTag );
            fEof = pTag->TagEOF;
         }
      }
      hb_ntxIndexUnLockRead( pTag );

      SELF_GOTO( ( AREAP ) pArea, fEof ? 0 : pTag->CurKeyInfo->Xtra );
      if ( !fEof )
      {
         SELF_SKIPFILTER( ( AREAP ) pArea, ( fForward || fOut ) ? 1 : -1 );
         if ( ! fForward && fOut )
            pArea->fBof = TRUE;
      }

      /* Update Bof and Eof flags */
      if( fForward )
         pArea->fBof = FALSE;
      else
         pArea->fEof = FALSE;
      return TRUE;
   }
   return FALSE;
}

/*
 * add key to custom tag (ordKeyAdd())
 * user key value is not implemented
 */
static BOOL hb_ntxOrdKeyAdd( LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   BOOL fResult = FALSE;
   LPKEYINFO pKey;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pTag->Custom || pArea->fEof || ( pTag->pForItem &&
       !hb_ntxEvalCond( pArea, pTag->pForItem, TRUE ) ) )
      return FALSE;

   pKey = hb_ntxEvalKey( NULL, pTag );
   hb_ntxTagRefreshScope( pTag );
   if( hb_ntxKeyInScope( pTag, pKey ) )
   {
      if( hb_ntxIndexLockWrite( pTag, TRUE ) )
      {
         if( hb_ntxTagKeyAdd( pTag, pKey ) )
         {
            fResult = TRUE;
            if( ( !pArea->fShared || pTag->Memory  ) && pTag->keyCount )
               pTag->keyCount++;
         }
         hb_ntxIndexUnLockWrite( pTag );
      }
   }
   hb_ntxKeyFree( pKey );
   return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static BOOL hb_ntxOrdKeyDel( LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->Owner->Owner;
   BOOL fResult = FALSE;
   LPKEYINFO pKey;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !pTag->Custom || pArea->fEof || ( pTag->pForItem &&
       !hb_ntxEvalCond( pArea, pTag->pForItem, TRUE ) ) )
      return FALSE;

   pKey = hb_ntxEvalKey( NULL, pTag );
   hb_ntxTagRefreshScope( pTag );
   if( hb_ntxKeyInScope( pTag, pKey ) )
   {
      if( hb_ntxIndexLockWrite( pTag, TRUE ) )
      {
         if( hb_ntxTagKeyDel( pTag, pKey ) )
         {
            fResult = TRUE;
            if( ( !pArea->fShared || pTag->Memory  ) && pTag->keyCount )
               pTag->keyCount--;
         }
         hb_ntxIndexUnLockWrite( pTag );
      }
   }
   hb_ntxKeyFree( pKey );
   return fResult;
}

/* ************************************************************************* */

/* Implementation of exported functions */

static ERRCODE ntxGoBottom( NTXAREAP pArea )
{
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("ntxGoBottom(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( !pArea->lpCurTag )
      return SUPER_GOBOTTOM( ( AREAP ) pArea );

   if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if ( ! hb_ntxIndexLockRead( pArea->lpCurTag ) )
      return FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   hb_ntxTagGoBottom( pArea->lpCurTag );

   pArea->fTop = FALSE;
   pArea->fBottom = TRUE;

   if ( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
      if ( retval != FAILURE && pArea->fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
   }
   hb_ntxIndexUnLockRead( pArea->lpCurTag );

   return retval;
}

static ERRCODE ntxGoTop( NTXAREAP pArea )
{
   ERRCODE retval;

   HB_TRACE(HB_TR_DEBUG, ("ntxGoTop(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( !pArea->lpCurTag )
      return SUPER_GOTOP( ( AREAP ) pArea );

   if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
      SELF_FORCEREL( ( AREAP ) pArea );

   if ( ! hb_ntxIndexLockRead( pArea->lpCurTag ) )
      return FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   hb_ntxTagGoTop( pArea->lpCurTag );

   pArea->fTop = TRUE;
   pArea->fBottom = FALSE;

   if ( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( ( AREAP ) pArea, 0 );
   else
   {
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
      if ( retval != FAILURE && pArea->fPositioned )
         retval = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
   }
   hb_ntxIndexUnLockRead( pArea->lpCurTag );

   return retval;
}

static ERRCODE ntxSeek( NTXAREAP pArea, BOOL fSoftSeek, PHB_ITEM pItem, BOOL fFindLast )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxSeek(%p, %d, %p, %d)", pArea, fSoftSeek, pKey, fFindLast));

   /* TODO: remove this - temporary hack for checking clipper compatibility */
/*
   if ( fFindLast )
   {
      fFindLast = FALSE;
   }
*/

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ! pArea->lpCurTag )
   {
      hb_ntxErrorRT( pArea, EG_NOORDER, 1201, NULL, 0, EF_CANDEFAULT );
      return FAILURE;
   }
   else
   {
      LPKEYINFO pKey;
      ERRCODE retval = SUCCESS;
      BOOL  fEOF = FALSE, fLast;
      USHORT uiLen;
      ULONG ulRec;

      if( pArea->lpdbPendingRel && pArea->lpdbPendingRel->isScoped )
         SELF_FORCEREL( ( AREAP ) pArea );

      pArea->fTop = pArea->fBottom = FALSE;
      pArea->fEof = FALSE;

      fLast = pArea->lpCurTag->fUsrDescend == pArea->lpCurTag->AscendKey ?
              !fFindLast : fFindLast;

      /* TODO: runtime error if valtype(pKeyItm) != pTag->KeyType */
      pKey = hb_ntxKeyPutItem( NULL, pItem, fLast ? NTX_MAX_REC_NUM :
                         NTX_IGNORE_REC_NUM, pArea->lpCurTag, TRUE, &uiLen );

      if( ! hb_ntxIndexLockRead( pArea->lpCurTag ) )
      {
         hb_ntxKeyFree( pKey );
         return FAILURE;
      }
      hb_ntxTagRefreshScope( pArea->lpCurTag );

      if ( hb_ntxTagKeyFind( pArea->lpCurTag, pKey, uiLen ) )
         ulRec = pArea->lpCurTag->CurKeyInfo->Xtra;
      else
         ulRec = 0;

      if ( ( ulRec == 0 && ! fSoftSeek ) || pArea->lpCurTag->TagEOF )
         fEOF = TRUE;
      else
      {
         if ( ! hb_ntxInBottomScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
            fEOF = TRUE;
         else if ( ! hb_ntxInTopScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
         {
            hb_ntxTagGoTop( pArea->lpCurTag );
            if ( pArea->lpCurTag->CurKeyInfo->Xtra == 0 ||
                 pArea->lpCurTag->TagEOF )
               fEOF = TRUE;
         }
      }
      hb_ntxIndexUnLockRead( pArea->lpCurTag );
      if ( !fEOF )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->CurKeyInfo->Xtra );
         if ( retval != FAILURE && pArea->fPositioned )
         {
            retval = SELF_SKIPFILTER( ( AREAP ) pArea, fFindLast ? -1 : 1 );
            if ( retval != FAILURE && ulRec && pArea->fPositioned )
            {
               pArea->fFound = ( ulRec == pArea->ulRecNo ||
                     hb_ntxValCompare( pArea->lpCurTag, pKey->key, uiLen,
                                       pArea->lpCurTag->CurKeyInfo->key,
                                       pArea->lpCurTag->KeyLength, FALSE ) == 0 );
               if ( ! pArea->fFound && ! fSoftSeek )
                  fEOF = TRUE;
            }
         }
      }
      if( retval != FAILURE && ( fEOF ||
          !hb_ntxKeyInScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo ) ) )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      if ( pArea->fPositioned || pArea->ulRecNo != 1 )
         pArea->fBof = FALSE;
      hb_ntxKeyFree( pKey );
      return retval;
   }
}

static ERRCODE ntxSkipRaw( NTXAREAP pArea, LONG lToSkip )
{
   ERRCODE retval;
   BOOL fOut = FALSE, fForward;

   HB_TRACE(HB_TR_DEBUG, ("ntxSkipRaw(%p, %ld)", pArea, lToSkip));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( ! pArea->lpCurTag || lToSkip == 0 )
      return SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );

   if ( ! hb_ntxIndexLockRead( pArea->lpCurTag ) )
      return FAILURE;
   hb_ntxTagRefreshScope( pArea->lpCurTag );

   fForward = ( lToSkip > 0 );

   if ( ! hb_ntxCurKeyRefresh( pArea->lpCurTag ) )
   {
      if ( fForward || pArea->fPositioned )
         fOut = TRUE;
      else
      {
         hb_ntxTagGoBottom( pArea->lpCurTag );
         fOut = pArea->lpCurTag->TagEOF;
         lToSkip++;
      }
   }

   if ( fForward )
   {
      while ( !fOut && !pArea->lpCurTag->TagEOF && lToSkip-- > 0 )
      {
         hb_ntxTagSkipNext( pArea->lpCurTag );
      }
      retval = SELF_GOTO( ( AREAP ) pArea,
                                    ( pArea->lpCurTag->TagEOF || fOut ) ? 0 :
                                    pArea->lpCurTag->CurKeyInfo->Xtra );
   }
   else /* if ( lToSkip < 0 ) */
   {
      while ( !fOut && !pArea->lpCurTag->TagBOF && lToSkip++ < 0 )
      {
         hb_ntxTagSkipPrev( pArea->lpCurTag );
      }
      if ( fOut || pArea->lpCurTag->TagBOF )
      {
         hb_ntxTagGoTop( pArea->lpCurTag );
         fOut = TRUE;
      }
      retval = SELF_GOTO( ( AREAP ) pArea, pArea->lpCurTag->TagEOF ? 0 :
                                          pArea->lpCurTag->CurKeyInfo->Xtra );
      pArea->fBof = fOut;
   }

   hb_ntxIndexUnLockRead( pArea->lpCurTag );
   /* Update Bof and Eof flags */
   /*
   if( fForward )
      pArea->fBof = FALSE;
   else
      pArea->fEof = FALSE;
   */
   return retval;
}

static ERRCODE ntxGoCold( NTXAREAP pArea )
{
   BOOL fRecordChanged = pArea->fRecordChanged;
   BOOL fAppend = pArea->fAppend;

   HB_TRACE(HB_TR_DEBUG, ("ntxGoCold(%p)", pArea));

   if( SUPER_GOCOLD( ( AREAP ) pArea ) == SUCCESS )
   {
      if( fRecordChanged || pArea->fNtxAppend )
      {
         if( fAppend && pArea->fShared )
         {
            pArea->fNtxAppend = TRUE;
         }
         else
         {
            LPTAGINFO pTag = pArea->lpNtxTag;
            LPKEYINFO pKey;
            BOOL InIndex;
            /* The pending relation may move the record pointer so we should
               disable them for KEY/FOR evaluation */
            LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
            pArea->lpdbPendingRel = NULL;

            while( pTag )
            {
               pKey = hb_ntxEvalKey( NULL, pTag );
               InIndex = ( pTag->pForItem == NULL || hb_ntxEvalCond( pArea, pTag->pForItem, TRUE ) );
               if( pArea->fNtxAppend || fAppend || InIndex != pTag->InIndex ||
                   hb_ntxValCompare( pTag, pTag->HotKeyInfo->key, pTag->KeyLength,
                                     pKey->key, pTag->KeyLength, TRUE ) )
               {
                  if ( ! hb_ntxIndexLockWrite( pTag, TRUE ) )
                  {
                     hb_ntxKeyFree( pKey );
                     pTag = pTag->pNext;
                     continue;
                  }
                  if( !pArea->fNtxAppend && !fAppend && pTag->InIndex )
                  {
                     if ( hb_ntxTagKeyDel( pTag, pTag->HotKeyInfo ) )
                     {
                        if( ( !pArea->fShared || pTag->Memory  ) && pTag->keyCount &&
                            hb_ntxKeyInScope( pTag, pTag->HotKeyInfo ) )
                           pTag->keyCount--;
                     }
                     else
                     {
                        printf( "\n\rntxGoCold: Cannot find current key. %ld",pTag->HotKeyInfo->Xtra );
                        fflush(stdout);
                     }
                  }
                  if( InIndex )
                  {
                     if( hb_ntxTagKeyAdd( pTag, pKey ) )
                     {
                        if( ( !pArea->fShared || pTag->Memory  ) && pTag->keyCount &&
                            hb_ntxKeyInScope( pTag, pKey ) )
                           pTag->keyCount++;
                     }
                  }
                  hb_ntxIndexUnLockWrite( pTag );
               }
               hb_ntxKeyFree( pKey );
               pTag = pTag->pNext;
            }
            pArea->fNtxAppend = FALSE;

            /* Restore disabled pending relation */
            pArea->lpdbPendingRel = lpdbPendingRel;
         }
      }
      return SUCCESS;
   }
   else
      return FAILURE;
}

static ERRCODE ntxGoHot( NTXAREAP pArea )
{
   LPTAGINFO pTag;

   HB_TRACE(HB_TR_DEBUG, ("ntxGoHot(%p)", pArea));

   if( SUPER_GOHOT( ( AREAP ) pArea ) == SUCCESS )
   {
      if( !pArea->fNtxAppend )
      {
         pTag = pArea->lpNtxTag;
         while( pTag )
         {
            pTag->HotKeyInfo = hb_ntxEvalKey( pTag->HotKeyInfo, pTag );
            pTag->InIndex = ( pTag->pForItem == NULL || hb_ntxEvalCond( pArea, pTag->pForItem, TRUE ) );
            pTag = pTag->pNext;
         }
      }
      return SUCCESS;
   }
   else
      return FAILURE;
}

/*
 * Flush _system_ buffers to disk
 */
static ERRCODE ntxFlush( NTXAREAP pArea )
{
   LPTAGINFO pTag;
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("ntxFlush(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   uiError = SUPER_FLUSH( ( AREAP ) pArea );

   if ( hb_set.HB_SET_HARDCOMMIT )
   {
      pTag = pArea->lpNtxTag;
      while( pTag )
      {
         if( !pTag->Memory && pTag->Owner->fFlush )
         {
            hb_fsCommit( pTag->Owner->DiskFile );
            pTag->Owner->fFlush = FALSE;
         }
         pTag = pTag->pNext;
      }
   }

   return uiError;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ERRCODE ntxStructSize( NTXAREAP pArea, USHORT * uiSize )

{
   HB_TRACE(HB_TR_DEBUG, ("ntxStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( NTXAREA );
   return SUCCESS;
}

static ERRCODE ntxSysName( NTXAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   strncpy( ( char * ) pBuffer, "DBFNTX", 7 /* HARBOUR_MAX_RDD_DRIVERNAME_LENGTH */ );
   return SUCCESS;
}

static ERRCODE ntxPack( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxPack(%p)", pArea ));

   if( SUPER_PACK( ( AREAP ) pArea ) == SUCCESS )
      return ntxOrderListRebuild( pArea );
   else
      return FAILURE;
}

static ERRCODE ntxZap( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxZap(%p)", pArea ));

   if( SUPER_ZAP( ( AREAP ) pArea ) == SUCCESS )
   {
      LPTAGINFO pTag;
      LPPAGEINFO pPage;

      pTag = pArea->lpNtxTag;
      while( pTag )
      {
         if ( hb_ntxIndexLockWrite( pTag, FALSE ) )
         {
            hb_ntxFreePageBuffer( pTag );
            pTag->TagBlock = pTag->Owner->NextAvail = pTag->keyCount = 0;
            pTag->Owner->Version = 0;

            if( pTag->Memory )
            {
               pTag->RootBlock = NTXBLOCKSIZE;
               pTag->ulPagesDepth = 1;
               hb_xfree( pTag->pages );
               pTag->pages = (LPPAGEINFO) hb_xgrab( sizeof(HB_PAGEINFO) );
               memset( pTag->pages , 0 ,sizeof( HB_PAGEINFO ) );
               pTag->pages[0].buffer = (char*) hb_xgrab( NTXBLOCKSIZE );
            }
            else
            {
               hb_fsSeek( pTag->Owner->DiskFile, NTXBLOCKSIZE, FS_SET );
               hb_fsWrite( pTag->Owner->DiskFile, NULL, 0 );
               pTag->Owner->fFlush = TRUE;
               pTag->TagChanged = TRUE;
               pPage = hb_ntxPageNew( pTag );
               if ( pPage )
               {
                  pTag->RootBlock = pPage->Page;
                  hb_ntxPageRelease( pTag, pPage );
               }
               else
                  pTag->RootBlock = 0;
               hb_ntxHeaderSave( pTag->Owner, TRUE );
            }
            hb_ntxIndexUnLockWrite( pTag );
         }
         pTag = pTag->pNext;
      }

      return SELF_GOTOP( ( AREAP ) pArea );
   }
   else
      return FAILURE;
}

static ERRCODE ntxOrderCondition( NTXAREAP area, LPDBORDERCONDINFO pOrdCondInfo )
{
#ifdef HB_EXTENSION
   if( pOrdCondInfo )
      pOrdCondInfo->fNoOptimize = hb_parl( 18 );
#endif
   return SUPER_ORDSETCOND( ( AREAP ) area, pOrdCondInfo );
}

static ERRCODE ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pResult, pKeyExp, pForExp = NULL;
   int uiLen, uiDec, iTagNameLen;
   char * szFileName, * szTagName;
   LPNTXINDEX pIndex;
   LPTAGINFO pTag;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   ERRCODE errCode;
   ULONG ulRecNo;
   BOOL fTagName;
   BYTE bType;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderCreate(%p, %p)", pArea, pOrderInfo));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != SUCCESS )
      return errCode;

   if( pArea->lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
      if( errCode != SUCCESS )
         return errCode;
   }

   if( !pArea->lpdbOrdCondInfo || ( pArea->lpdbOrdCondInfo->fAll &&
                                    !pArea->lpdbOrdCondInfo->fAdditive ) )
   {
      errCode = SELF_ORDLSTCLEAR( ( AREAP ) pArea );
      if( errCode != SUCCESS )
         return errCode;
   }

   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pKeyExp = hb_itemNew( pOrderInfo->itmCobExpr );
   else /* Otherwise, try compiling the key expression string */
   {
      errCode = SELF_COMPILE( ( AREAP ) pArea, ( BYTE * ) hb_itemGetCPtr( pOrderInfo->abExpr ) );
      if( errCode != SUCCESS )
         return errCode;
      pKeyExp = pArea->valResult;
      pArea->valResult = NULL;
   }

   /* Get a blank record before testing expression */
   ulRecNo = pArea->ulRecNo;
   errCode = SELF_GOTO( ( AREAP ) pArea, 0 );
   if( errCode != SUCCESS )
      return errCode;

   errCode = SELF_EVALBLOCK( ( AREAP ) pArea, pKeyExp );
   if( errCode != SUCCESS )
   {
      hb_ntxDestroyExp( pKeyExp );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      return errCode;
   }
   pResult = pArea->valResult;
   pArea->valResult = NULL;

   bType = hb_ntxItemType( pResult );
   uiLen = uiDec = 0;
   switch( bType )
   {
      case 'N':
         hb_itemGetNLen( pResult, (int*) &uiLen, (int*) &uiDec );
         if( uiDec )
            uiLen += uiDec + 1;
         break;
      case 'D':
         uiLen = 8;
         break;
      case 'L':
         uiLen = 1;
         break;
      case 'C':
         uiLen = hb_itemGetCLen( pResult );
         if( uiLen > NTX_MAX_KEY )
            uiLen = NTX_MAX_KEY;
         break;
      default:
         bType = 'U';
   }
   hb_itemRelease( pResult );

   /* Make sure KEY has proper type and uiLen is not 0 */
   if ( bType == 'U' || uiLen == 0 )
   {
      hb_ntxDestroyExp( pKeyExp );
      SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      hb_ntxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
                     1026, NULL, 0, 0 );
      return FAILURE;
   }

   /* Check conditional expression */
   if( pArea->lpdbOrdCondInfo )
   {
      if( pArea->lpdbOrdCondInfo->itmCobFor )
         /* If we have a codeblock for the conditional expression, use it */
         pForExp = hb_itemNew( pArea->lpdbOrdCondInfo->itmCobFor );
      else if ( pArea->lpdbOrdCondInfo->abFor )
      {
         /* Otherwise, try compiling the conditional expression string */
         errCode = SELF_COMPILE( ( AREAP ) pArea, pArea->lpdbOrdCondInfo->abFor );
         if( errCode != SUCCESS )
         {
            hb_ntxDestroyExp( pKeyExp );
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            return errCode;
         }
         pForExp = pArea->valResult;
         pArea->valResult = NULL;
      }
   }

   /* Test conditional expression */
   if ( pForExp )
   {
      USHORT uiType;

      errCode = SELF_EVALBLOCK( ( AREAP ) pArea, pForExp );
      if( errCode != SUCCESS )
      {
         hb_ntxDestroyExp( pKeyExp );
         hb_ntxDestroyExp( pForExp );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         return errCode;
      }
      uiType = hb_itemType( pArea->valResult );
      hb_itemRelease( pArea->valResult );
      pArea->valResult = NULL;
      if ( uiType != HB_IT_LOGICAL )
      {
         hb_ntxDestroyExp( pKeyExp );
         hb_ntxDestroyExp( pForExp );
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );
         /* TODO: !!! runtime error ? */
         return FAILURE;
      }
   }

   /* Check file name */
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
   if( strlen( ( char * ) pOrderInfo->abBagName ) == 0 )
   {
      pFileName = hb_fsFNameSplit( pArea->szDataFileName );
      memset( &pExtInfo, 0, sizeof( DBORDERINFO ) );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      pFileName->szExtension = hb_itemGetCPtr( pExtInfo.itmResult );
      hb_fsFNameMerge( szFileName, pFileName );
      hb_itemRelease( pExtInfo.itmResult );
   }
   else
   {
      hb_strncpy( szFileName, ( char * ) pOrderInfo->abBagName, _POSIX_PATH_MAX );
      pFileName = hb_fsFNameSplit( szFileName );
      if( !pFileName->szExtension )
      {
         memset( &pExtInfo, 0, sizeof( DBORDERINFO ) );
         pExtInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
         hb_strncat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ), _POSIX_PATH_MAX );
         hb_itemRelease( pExtInfo.itmResult );
      }
   }

   fTagName = pOrderInfo->atomBagName && pOrderInfo->atomBagName[0];
   iTagNameLen = strlen( fTagName ? ( char * ) pOrderInfo->atomBagName :
                                    pFileName->szName );
   if ( iTagNameLen > 10 )
      iTagNameLen = 10;
   szTagName = ( char * ) hb_xgrab( iTagNameLen + 1 );
   hb_strncpyUpper( szTagName, fTagName ? ( char * ) pOrderInfo->atomBagName :
                                          pFileName->szName, iTagNameLen );
   hb_xfree( pFileName );

   pIndex = hb_ntxIndexNew( pArea );
   pIndex->IndexName = szFileName;
   pTag = hb_ntxTagNew( pIndex, szTagName, fTagName, pOrderInfo->abExpr->item.asString.value,
                        pKeyExp, bType, (USHORT) uiLen, (USHORT) uiDec, (char *) ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->abFor : NULL ),
                        pForExp, pArea->lpdbOrdCondInfo ? !pArea->lpdbOrdCondInfo->fDescending : TRUE,
                        pOrderInfo->fUnique, pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->fCustom : FALSE,
                        pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->fNoOptimize : FALSE );
   pIndex->CompoundTag = pTag;

   if( !pTag->Memory )
   {
      do {
         pIndex->DiskFile = hb_spCreate( ( BYTE * ) szFileName , FC_NORMAL );
      } while( pIndex->DiskFile == FS_ERROR &&
               hb_ntxErrorRT( pArea, EG_CREATE, EDBF_CREATE, szFileName,
                     hb_fsError(), EF_CANRETRY | EF_CANDEFAULT ) == E_RETRY );

      if( pIndex->DiskFile == FS_ERROR )
      {
         hb_ntxIndexFree( pIndex );
         return FAILURE;
      }
      pTag->Owner->fFlush = TRUE;
      pTag->TagChanged = TRUE;
      pTag->TagBlock = 0;
   }
   if( !hb_ntxIndexLockWrite( pTag, FALSE ) )
   {
      hb_ntxIndexFree( pIndex );
      return FAILURE;
   }
   errCode = hb_ntxTagCreate( pTag );
   hb_ntxHeaderSave( pTag->Owner, TRUE );
   hb_ntxIndexUnLockWrite( pTag );
   if( errCode != SUCCESS )
   {
      hb_ntxIndexFree( pIndex );
      return errCode;
   }

   if( pArea->lpdbOrdCondInfo && !pArea->lpdbOrdCondInfo->fAll &&
                                 !pArea->lpdbOrdCondInfo->fAdditive )
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );
   if( pArea->lpdbOrdCondInfo && pArea->lpdbOrdCondInfo->fAdditive )
   {
      LPTAGINFO * pTagPtr = &pArea->lpNtxTag;
      while ( *pTagPtr )
         pTagPtr = &(*pTagPtr)->pNext;
      *pTagPtr = pTag;
   }
   else
   {
      pArea->lpNtxTag = pTag;
   }
   pArea->lpCurTag = pTag;
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
   return SELF_GOTOP( ( AREAP ) pArea );
}

static ERRCODE ntxOrderInfo( NTXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPTAGINFO pTag;
   HB_TRACE(HB_TR_DEBUG, ("ntxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pInfo));

/* TODO:
   case DBOI_KEYSINCLUDED:

   case DBOI_FINDREC:
   case DBOI_FINDRECCONT:
   case DBOI_SKIPEVAL:
   case DBOI_SKIPEVALBACK:
   case DBOI_SKIPWILD:
   case DBOI_SKIPWILDBACK:
   case DBOI_SKIPREGEX:
   case DBOI_SKIPREGEXBACK:
   case DBOI_SCOPEEVAL:
*/

   switch( uiIndex )
   {
      case DBOI_BAGEXT:
         hb_itemPutC( pInfo->itmResult, NTX_INDEXEXT );
         return SUCCESS;
      case DBOI_EVALSTEP:
         hb_itemPutNL( pInfo->itmResult,
                  pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->lStep : 0 );
         return SUCCESS;
      case DBOI_KEYSINCLUDED:
         hb_itemPutNL( pInfo->itmResult,
                       pArea->pSort ? pArea->pSort->ulTotKeys : 0 );
         return SUCCESS;
      case DBOI_LOCKOFFSET:
      case DBOI_HPLOCKING:
      {
         HB_FOFFSET ulPos, ulPool;
         hb_dbfLockIdxGetData( pArea->bLockType, &ulPos, &ulPool );
         if ( uiIndex == DBOI_LOCKOFFSET )
            hb_itemPutNInt( pInfo->itmResult, ulPos );
         else
            hb_itemPutL( pInfo->itmResult, ulPool > 0 );
         return SUCCESS;
      }
      case DBOI_ORDERCOUNT:
         if( pInfo->atomBagName && hb_itemGetCPtr( pInfo->atomBagName ) )
         {
            hb_itemPutNI( pInfo->itmResult, pArea->lpNtxTag ? 1 : 0 );
         }
         else
         {
            int i = 0;
            pTag = pArea->lpNtxTag;
            while ( pTag )
            {
               ++i;
               pTag = pTag->pNext;
            }
            hb_itemPutNI( pInfo->itmResult, i );
         }
         return SUCCESS;
   }

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pTag = hb_ntxFindTag( pArea , pInfo->itmOrder );

   if( pTag )
   {
      switch( uiIndex )
      {
         case DBOI_CONDITION:
            hb_itemPutC( pInfo->itmResult , pTag->ForExpr ? pTag->ForExpr : "" );
            break;
         case DBOI_EXPRESSION:
            hb_itemPutC( pInfo->itmResult , pTag->KeyExpr );
            break;
         case DBOI_BAGNAME:
            hb_itemPutC( pInfo->itmResult, pTag->Owner->IndexName );
            break;
         case DBOI_NAME:
            hb_itemPutC( pInfo->itmResult, pTag->TagName );
            break;
         case DBOI_NUMBER:
            hb_itemPutNI( pInfo->itmResult, hb_ntxFindTagNum( pArea, pTag ) );
            break;
         case DBOI_FILEHANDLE:
            hb_itemPutNInt( pInfo->itmResult, pTag->Owner->DiskFile );
            break;
         case DBOI_FULLPATH:
            hb_itemPutC( pInfo->itmResult, pTag->Owner->IndexName );
            break;
         case DBOI_KEYCOUNT:
            hb_itemPutNL( pInfo->itmResult, hb_ntxOrdKeyCount( pTag ) );
            break;
         case DBOI_POSITION:
            if ( pInfo->itmNewVal && hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_itemPutL( pInfo->itmResult,
                  hb_ntxOrdKeyGoto( pTag, hb_itemGetNL( pInfo->itmNewVal ) ) );
            else
               hb_itemPutNL( pInfo->itmResult, hb_ntxOrdKeyNo( pTag ) );
            break;
         case DBOI_ISCOND:
            hb_itemPutL( pInfo->itmResult, (pTag->ForExpr!=NULL) );
            break;
         case DBOI_ISDESC:
            hb_itemPutL( pInfo->itmResult, pTag->fUsrDescend );
            if( pInfo->itmNewVal && hb_itemType( pInfo->itmNewVal ) == HB_IT_LOGICAL )
               pTag->fUsrDescend = hb_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_UNIQUE:
            hb_itemPutL( pInfo->itmResult, pTag->UniqueKey );
            break;
         case DBOI_CUSTOM:
            hb_itemPutL( pInfo->itmResult, pTag->Custom );
            break;
         case DBOI_SCOPETOP:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
            break;
         case DBOI_SCOPEBOTTOM:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               hb_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            break;
         case DBOI_SCOPESET:
            if ( pInfo->itmNewVal )
            {
               hb_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
               hb_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
            if ( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_SCOPETOPCLEAR:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            hb_ntxTagClearScope( pTag, 0 );
            break;
         case DBOI_SCOPEBOTTOMCLEAR:
            if( pInfo->itmResult )
               hb_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            hb_ntxTagClearScope( pTag, 1 );
            break;
         case DBOI_SCOPECLEAR:
            hb_ntxTagClearScope( pTag, 0 );
            hb_ntxTagClearScope( pTag, 1 );
            if ( pInfo->itmResult )
               hb_itemClear( pInfo->itmResult );
         case DBOI_KEYADD:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdKeyAdd( pTag ) );
            break;
         case DBOI_KEYDELETE:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdKeyDel( pTag ) );
            break;
         case DBOI_KEYTYPE:
            {
               char szType[2];
               szType[0] = (char) pTag->KeyType;
               szType[1] = 0;
               hb_itemPutC( pInfo->itmResult, szType );
            }
            break;
         case DBOI_KEYSIZE:
            hb_itemPutNI( pInfo->itmResult, pTag->KeyLength );
            break;
         case DBOI_KEYVAL:
            if( hb_ntxCurKeyRefresh( pTag ) )
               hb_ntxKeyGetItem( pInfo->itmResult, pTag->CurKeyInfo, pTag, TRUE );
            else
               hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_SKIPUNIQUE:
            hb_itemPutL( pInfo->itmResult, hb_ntxOrdSkipUnique( pTag,
                                          hb_itemGetNL( pInfo->itmNewVal ) ) );
            break;
      }
   }
   else if( pInfo->itmResult )
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNT:
         {
            ULONG ulRecCount = 0;
            SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
            hb_itemPutNInt( pInfo->itmResult, ulRecCount );
            break;
         }
         case DBOI_POSITION:
            if( pInfo->itmNewVal && hb_itemType( pInfo->itmNewVal ) & HB_IT_NUMERIC )
               hb_itemPutL( pInfo->itmResult, SELF_GOTO( ( AREAP ) pArea,
                              hb_itemGetNL( pInfo->itmNewVal ) ) == SUCCESS );
            else
               SELF_RECNO( ( AREAP ) pArea, pInfo->itmResult );
            break;
         case DBOI_SKIPUNIQUE:
            hb_itemPutL( pInfo->itmResult, SELF_SKIP( ( AREAP ) pArea,
               hb_itemGetNL( pInfo->itmNewVal ) >= 0 ? 1 : -1 ) == SUCCESS );
            break;
         case DBOI_ISCOND:
         case DBOI_ISDESC:
         case DBOI_UNIQUE:
         case DBOI_CUSTOM:
         case DBOI_KEYADD:
         case DBOI_KEYDELETE:
            hb_itemPutL( pInfo->itmResult, FALSE );
            break;
         case DBOI_KEYVAL:
         case DBOI_SCOPETOP:
         case DBOI_SCOPEBOTTOM:
         case DBOI_SCOPESET:
         case DBOI_SCOPETOPCLEAR:
         case DBOI_SCOPEBOTTOMCLEAR:
         case DBOI_SCOPECLEAR:
            hb_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYSIZE:
         case DBOI_NUMBER:
         case DBOI_ORDERCOUNT:
            hb_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_FILEHANDLE:
            hb_itemPutNInt( pInfo->itmResult, FS_ERROR );
            break;
         default:
            hb_itemPutC( pInfo->itmResult, "" );
      }
   }
   return SUCCESS;
}

static ERRCODE ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   USHORT uiFlags;
   char * szFileName;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   LPNTXINDEX pIndex;
   LPTAGINFO *pTagPtr;
   ERRCODE errCode;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != SUCCESS )
      return errCode;

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
   hb_strncpy( szFileName, hb_itemGetCPtr( pOrderInfo->atomBagName ), _POSIX_PATH_MAX );
   if( strlen( szFileName ) == 0 )
   {
      hb_xfree( szFileName );
      /* TODO: !!! RT error */
      return FAILURE;
   }
   pFileName = hb_fsFNameSplit( szFileName );
   if( !pFileName->szExtension )
   {
      memset( &pExtInfo, 0, sizeof( DBORDERINFO ) );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      hb_strncat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ), _POSIX_PATH_MAX );
      hb_itemRelease( pExtInfo.itmResult );
   }
   pIndex = hb_ntxIndexNew( pArea );
   pIndex->IndexName = szFileName;

   /* Index file could be opened with FO_READ only in exclusive readonly mode
      to allow locking in other modes
      Alexander, it's not proper solution. It will cause that you cannot open
      readonly files in exclusive mode. Shared locks should be used instead
      this hack for readonly mode.
    */
   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );

   do
   {
      pIndex->DiskFile = hb_spOpen( ( BYTE * ) szFileName, uiFlags );
      if( pIndex->DiskFile == FS_ERROR )
      {
         bRetry = ( hb_ntxErrorRT( pArea, EG_OPEN, 1003, szFileName,
                    hb_fsError(), EF_CANRETRY | EF_CANDEFAULT ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );

   if( pIndex->DiskFile == FS_ERROR )
   {
      hb_ntxIndexFree( pIndex );
      hb_xfree( pFileName );
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );
      return FAILURE;
   }

   errCode = hb_ntxHeaderLoad( pIndex, pFileName->szName );
   hb_xfree( pFileName );
   if( errCode != SUCCESS )
   {
      hb_ntxErrorRT( pIndex->Owner, EG_CORRUPTION, EDBF_CORRUPT,
                     pIndex->IndexName, hb_fsError(), 0 );
      hb_ntxIndexFree( pIndex );
      return errCode;
   }

   pTagPtr = &pArea->lpNtxTag;
   while ( *pTagPtr )
      pTagPtr = &(*pTagPtr)->pNext;
   *pTagPtr = pIndex->CompoundTag;
   if( !pArea->lpCurTag )
   {
      pArea->lpCurTag = pIndex->CompoundTag;
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

static ERRCODE ntxOrderListClear( NTXAREAP pArea )
{
   LPTAGINFO pTag;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListClear(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->lpCurTag = NULL;
   while ( pArea->lpNtxTag )
   {
      pTag = pArea->lpNtxTag;
      pArea->lpNtxTag = pArea->lpNtxTag->pNext;
      hb_ntxIndexFree( pTag->Owner );
   }
   return SUCCESS;
}

static ERRCODE ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListFocus(%p, %p)", pArea, pOrderInfo));

   pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                             pArea->lpCurTag ? pArea->lpCurTag->TagName : "" );

   if( pOrderInfo->itmOrder )
   {
      pArea->lpCurTag = hb_ntxFindTag( pArea, pOrderInfo->itmOrder );
   }

   return SUCCESS;
}

static ERRCODE ntxOrderListRebuild( NTXAREAP pArea )
{
   LPTAGINFO pTag, pTagTmp;
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListRebuild(%p)", pArea));

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode != SUCCESS )
      return errCode;

   if( pArea->lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
      if( errCode != SUCCESS )
         return errCode;
   }
   pTagTmp = pArea->lpCurTag;
   pArea->lpCurTag = NULL;
   pTag = pArea->lpNtxTag;
   while( pTag && errCode == SUCCESS )
   {
      if( hb_ntxIndexLockWrite( pTag, FALSE ) )
      {
         hb_ntxFreePageBuffer( pTag );
         if( pTag->Memory )
         {
            pTag->ulPagesDepth = 0;
            hb_xfree( pTag->pages );
         }
         else
         {
            hb_fsSeek( pTag->Owner->DiskFile, NTXBLOCKSIZE, FS_SET );
            hb_fsWrite( pTag->Owner->DiskFile, NULL, 0 );
            pTag->Owner->fFlush = TRUE;
            pTag->TagChanged = TRUE;
         }
         pTag->RootBlock = pTag->TagBlock = pTag->Owner->NextAvail =
                           pTag->keyCount = 0;
         pTag->Owner->Version = 0;
         errCode = hb_ntxTagCreate( pTag );
         hb_ntxHeaderSave( pTag->Owner, TRUE );
         hb_ntxIndexUnLockWrite( pTag );
      }
      pTag = pTag->pNext;
   }
   if( errCode == SUCCESS )
   {
      pArea->lpCurTag = pTagTmp;
      errCode = SELF_GOTOP( ( AREAP ) pArea );
   }
   return errCode;
}

static ERRCODE ntxClose( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxClose(%p)", pArea));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if ( pArea->pSort )
   {
      hb_ntxSortFree( pArea->pSort );
      pArea->pSort = NULL;
   }
   SELF_ORDLSTCLEAR( ( AREAP ) pArea );
   return SUPER_CLOSE( ( AREAP ) pArea );
}

static RDDFUNCS ntxTable = { ntxBof,
                             ntxEof,
                             ntxFound,
                             ( DBENTRYP_V ) ntxGoBottom,
                             ntxGoTo,
                             ntxGoToId,
                             ( DBENTRYP_V ) ntxGoTop,
                             ( DBENTRYP_BIB ) ntxSeek,
                             ntxSkip,
                             ntxSkipFilter,
                             ( DBENTRYP_L ) ntxSkipRaw,
                             ntxAddField,
                             ( DBENTRYP_B ) ntxAppend,
                             ntxCreateFields,
                             ntxDeleteRec,
                             ntxDeleted,
                             ntxFieldCount,
                             ntxFieldDisplay,
                             ntxFieldInfo,
                             ntxFieldName,
                             ( DBENTRYP_V ) ntxFlush,
                             ntxGetRec,
                             ntxGetValue,
                             ntxGetVarLen,
                             ( DBENTRYP_V ) ntxGoCold,
                             ( DBENTRYP_V ) ntxGoHot,
                             ntxPutRec,
                             ntxPutValue,
                             ntxRecall,
                             ntxRecCount,
                             ntxRecInfo,
                             ntxRecNo,
                             ntxSetFieldsExtent,
                             ntxAlias,
                             ( DBENTRYP_V ) ntxClose,
                             ntxCreate,
                             ntxInfo,
                             ntxNewArea,
                             ntxOpen,
                             ntxRelease,
                             ( DBENTRYP_SP ) ntxStructSize,
                             ( DBENTRYP_P ) ntxSysName,
                             ntxEval,
                             ( DBENTRYP_V ) ntxPack,
                             ntPackRec,
                             ntxSort,
                             ntxTrans,
                             ntxTransRec,
                             ( DBENTRYP_V ) ntxZap,
                             ntxchildEnd,
                             ntxchildStart,
                             ntxchildSync,
                             ntxsyncChildren,
                             ntxclearRel,
                             ntxforceRel,
                             ntxrelArea,
                             ntxrelEval,
                             ntxrelText,
                             ntxsetRel,
                             ( DBENTRYP_OI ) ntxOrderListAdd,
                             ( DBENTRYP_V ) ntxOrderListClear,
                             ntxOrderListDelete,
                             ( DBENTRYP_OI ) ntxOrderListFocus,
                             ( DBENTRYP_V ) ntxOrderListRebuild,
                             ( DBENTRYP_VOI ) ntxOrderCondition,
                             ( DBENTRYP_VOC ) ntxOrderCreate,
                             ntxOrderDestroy,
                             ( DBENTRYP_OII ) ntxOrderInfo,
                             ntxClearFilter,
                             ntxClearLocate,
                             ntxClearScope,
                             ntxCountScope,
                             ntxFilterText,
                             ntxScopeInfo,
                             ntxSetFilter,
                             ntxSetLocate,
                             ntxSetScope,
                             ntxSkipScope,
                             ntxCompile,
                             ntxError,
                             ntxEvalBlock,
                             ntxRawLock,
                             ntxLock,
                             ntxUnLock,
                             ntxCloseMemFile,
                             ntxCreateMemFile,
                             ntxGetValueFile,
                             ntxOpenMemFile,
                             ntxPutValueFile,
                             ntxReadDBHeader,
                             ntxWriteDBHeader,
                             ntxExit,
                             ntxDrop,
                             ntxExists,
                             ntxWhoCares
                           };

HB_FUNC(_DBFNTX ) {;}

HB_FUNC( DBFNTX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );
   if( pTable )
   {
      SHORT iRet;

      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      iRet = hb_rddInherit( pTable, &ntxTable, &ntxSuper, ( BYTE * ) "DBFDBT" );
      if ( iRet == FAILURE )
         iRet = hb_rddInherit( pTable, &ntxTable, &ntxSuper, ( BYTE * ) "DBFFPT" );
      if ( iRet == FAILURE )
         iRet = hb_rddInherit( pTable, &ntxTable, &ntxSuper, ( BYTE * ) "DBF" );
      hb_retni( iRet );
   }
   else
   {
      hb_retni( FAILURE );
   }
}


/* ************************************************************************* */
/* create index: hb_ntxTagCreate() */
/* ************************************************************************* */

/* #define HB_NTX_INDEXREC */

static int hb_ntxQuickSortCompare( LPNTXSORTINFO pSort, BYTE * pKey1, BYTE * pKey2 )
{
   int iLen = pSort->keyLen, i;

   i = hb_ntxValCompare( pSort->pTag, (char *) pKey1, iLen, (char *) pKey2, iLen, TRUE );
   if( ! pSort->pTag->AscendKey )
   {
      i = -i;
   }
#ifdef HB_NTX_INDEXREC
   if( i == 0 )
   {
      i = ( HB_GET_LE_UINT32( pKey1 + iLen ) < HB_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;
   }
#endif

   return i;
}

static BOOL hb_ntxQSort( LPNTXSORTINFO pSort, BYTE * pSrc, BYTE * pBuf, LONG lKeys )
{
   if( lKeys > 1 )
   {
      int iLen = pSort->keyLen + 4;
      LONG l1, l2;
      BYTE * pPtr1, * pPtr2, *pDst;
      BOOL f1, f2;

      l1 = lKeys >> 1;
      l2 = lKeys - l1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ l1 * iLen ];

      f1 = hb_ntxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = hb_ntxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
      if( f1 )
      {
         pDst = pBuf;
      }
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( !f2 )
      {
         pPtr2 = &pBuf[ l1 * iLen ];
      }
      while( l1 > 0 && l2 > 0 )
      {
         if( hb_ntxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
         {
            memcpy( pDst, pPtr1, iLen );
            pPtr1 += iLen;
            l1--;
         }
         else
         {
            memcpy( pDst, pPtr2, iLen );
            pPtr2 += iLen;
            l2--;
         }
         pDst += iLen;
      }
      if( l1 > 0 )
      {
         memcpy( pDst, pPtr1, iLen * l1 );
      }
      else if( l2 > 0 && f1 == f2 )
      {
         memcpy( pDst, pPtr2, iLen * l2 );
      }
      return !f1;
   }
   return TRUE;
}

static void hb_ntxSortSortPage( LPNTXSORTINFO pSort )
{
   ULONG ulSize = pSort->ulKeys * ( pSort->keyLen + 4 );
   if( !hb_ntxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ ulSize ], pSort->ulKeys ) )
   {
      memcpy( pSort->pKeyPool, &pSort->pKeyPool[ ulSize ], ulSize );
   }
}

static void hb_ntxSortAddNodeKey( LPNTXSORTINFO pSort, BYTE *pKeyVal, ULONG ulRec )
{
   LPPAGEINFO pPage;
   ULONG ulPage = 0;
   int iLevel = 0;

   do
   {
      pPage = pSort->NodeList[ iLevel ];
      if( pPage == NULL )
      {
         pPage = pSort->NodeList[ iLevel ] = hb_ntxPageNew( pSort->pTag );
         break;
      }
      else if( pPage->uiKeys >= pSort->pTag->MaxKeys )
      {
         hb_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
         ulPage = pPage->Page;
         hb_ntxPageSave( pSort->pTag, pPage );
         hb_ntxPageRelease( pSort->pTag, pPage );
         pSort->NodeList[ iLevel++ ] = hb_ntxPageNew( pSort->pTag );
      }
      else
         break;
   }
   while( TRUE );

   memcpy( hb_ntxGetKeyVal( pPage, pPage->uiKeys ), pKeyVal, pSort->pTag->KeyLength );
   hb_ntxSetKeyRec( pPage, pPage->uiKeys, ulRec );
   hb_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
   pPage->uiKeys++;
}

static void hb_ntxSortWritePage( LPNTXSORTINFO pSort )
{
   ULONG ulSize = pSort->ulKeys * ( pSort->keyLen + 4 );

   hb_ntxSortSortPage( pSort );

   if( pSort->hTempFile == FS_ERROR )
   {
      BYTE szName[ _POSIX_PATH_MAX + 1 ];
      pSort->hTempFile = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->hTempFile == FS_ERROR )
      {
         hb_errInternal( 9999, "hb_ntxSortWritePage: Can't create temporary file.", "", "" );
      }
      pSort->szTempFileName = hb_strdup( ( char * ) szName );
   }
   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   pSort->pSwapPage[ pSort->ulCurPage ].nOffset = hb_fsSeekLarge( pSort->hTempFile, 0, FS_END );
   if( hb_fsWriteLarge( pSort->hTempFile, pSort->pKeyPool, ulSize ) != ulSize )
   {
      hb_errInternal( 9999, "hb_ntxSortWritePage: Write error in temporary file.", "", "" );
   }
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void hb_ntxSortGetPageKey( LPNTXSORTINFO pSort, ULONG ulPage,
                                  BYTE ** pKeyVal, ULONG *pulRec )
{
   int iLen = pSort->keyLen;

   if( pSort->pSwapPage[ ulPage ].ulKeyBuf == 0 )
   {
      ULONG ulKeys = HB_MIN( pSort->ulPgKeys, pSort->pSwapPage[ ulPage ].ulKeys );
      ULONG ulSize = ulKeys * ( iLen + 4 );

      if( hb_fsSeekLarge( pSort->hTempFile, pSort->pSwapPage[ ulPage ].nOffset, FS_SET ) != pSort->pSwapPage[ ulPage ].nOffset ||
          hb_fsReadLarge( pSort->hTempFile, pSort->pSwapPage[ ulPage ].pKeyPool, ulSize ) != ulSize )
      {
         hb_errInternal( 9999, "hb_ntxSortGetPageKey: Read error from temporary file.", "", "" );
      }
      pSort->pSwapPage[ ulPage ].nOffset += ulSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = HB_GET_LE_UINT32( *pKeyVal + iLen );
}

static void hb_ntxSortOrderPages( LPNTXSORTINFO pSort )
{
   int iLen = pSort->keyLen, i;
   LONG l, r, m;
   ULONG n, ulPage, ulRec;
   BYTE *pKey = NULL, *pTmp;

   pSort->ulFirst = 0;
   pSort->pSortedPages = ( ULONG * ) hb_xgrab( pSort->ulPages * sizeof( ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      for ( n = 0; n < pSort->ulPages; n++ )
      {
         hb_ntxSortGetPageKey( pSort, n, &pKey, &ulRec );
         l = 0;
         r = n - 1;
         while( l <= r )
         {
            m = ( l + r ) >> 1;
            ulPage = pSort->pSortedPages[ m ];
            pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
            i = hb_ntxValCompare( pSort->pTag, (char *) pKey, iLen, (char *) pTmp, iLen, TRUE );
            if( ! pSort->pTag->AscendKey )
               i = -i;
#ifdef HB_NTX_INDEXREC
            if( i == 0 )
               i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
#endif
            if( i >= 0 )
               l = m + 1;
            else
               r = m - 1;
         }
         for ( r = n; r > l; r-- )
            pSort->pSortedPages[ r ] = pSort->pSortedPages[ r - 1 ];
         pSort->pSortedPages[ l ] = n;
      }
   }
}

static BOOL hb_ntxSortKeyGet( LPNTXSORTINFO pSort, BYTE ** pKeyVal, ULONG *pulRec )
{
   int iLen = pSort->keyLen, i;
   LONG l, r, m;
   ULONG ulPage;

   ulPage = pSort->pSortedPages[ pSort->ulFirst ];

   /* check if first page has some keys yet */
   if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
   {
      BYTE *pKey, *pTmp;
      ULONG ulRec, ulPg;

      /*
       * last key was taken from this page - we have to resort it.
       * This is done intentionally here to be sure that the key
       * value return by this function will not be overwritten by
       * next keys in page read from temporary file in function
       * hb_ntxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      hb_ntxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         m = ( l + r ) >> 1;
         ulPg = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPg ].pKeyPool[ pSort->pSwapPage[ ulPg ].ulCurKey * ( iLen + 4 ) ];
         i = hb_ntxValCompare( pSort->pTag, (char *) pKey, iLen, (char *) pTmp, iLen, TRUE );
         if( ! pSort->pTag->AscendKey )
            i = -i;
#ifdef HB_NTX_INDEXREC
         if( i == 0 )
            i = ( ulRec < HB_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
#else
         if( i == 0 )
            i = ( ulPage < ulPg ) ? -1 : 1;
#endif
         if( i > 0 )
            l = m + 1;
         else
            r = m - 1;
      }
      if( l > ( LONG ) pSort->ulFirst + 1 )
      {
         ulPage = pSort->pSortedPages[ pSort->ulFirst ];
         for ( r = pSort->ulFirst + 1; r < l; r++ )
            pSort->pSortedPages[ r - 1 ] = pSort->pSortedPages[ r ];
         pSort->pSortedPages[ l - 1 ] = ulPage;
      }
   }
   else
   {
      pSort->ulFirst++;
   }
   if( pSort->ulFirst < pSort->ulPages )
   {
      ulPage = pSort->pSortedPages[ pSort->ulFirst ];
      hb_ntxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return TRUE;
   }
   return FALSE;
}

static void hb_ntxSortKeyAdd( LPNTXSORTINFO pSort, ULONG ulRec, char * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   BYTE *pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
   {
      hb_ntxSortWritePage( pSort );
   }
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], ' ', iLen - iKeyLen );
   }
   else
   {
      memcpy( pDst, pKeyVal, iLen );
   }
   HB_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPNTXSORTINFO hb_ntxSortNew( LPTAGINFO pTag, ULONG ulRecCount )
{
   LPNTXSORTINFO pSort;
   BYTE * pBuf;
   int iLen = pTag->KeyLength;
   ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPNTXSORTINFO ) hb_xgrab( sizeof( NTXSORTINFO ) );
   memset( pSort, 0, sizeof( NTXSORTINFO ) );
   ulMax = ulMin = ( ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
   ulSize = ( 1L << 20 ) / ( iLen + 4 );
   while( ulMax < ulSize )
      ulMax <<= 1;
   if( ulMax > ulRecCount )
      ulMax = ulRecCount;

   do
   {
      ulSize = ulMax * ( iLen + 4 );
      pBuf = ( BYTE * ) hb_xalloc( ulSize << 2 );
      if( pBuf )
      {
         hb_xfree( pBuf );
         pBuf = ( BYTE * ) hb_xalloc( ulSize << 1 );
      }
      else
      {
         ulMax >>= 1;
      }
   }
   while( ! pBuf && ulMax >= ulMin );

   if( ! pBuf )
   {
      /* call hb_xgrab() to force out of memory error,
       * though in multi process environment this call may return
       * with success when other process free some memory
       * (also the size of buf is reduced to absolute minimum).
       * Sorry but I'm to lazy to implement indexing with smaller
       * memory though it's possible - just simply I can even create
       * index on-line by key adding like in normal update process.
       * The memory necessary to index file is now ~
       *    ~ (keySize+4+sizeof(NTXSWAPPAGE)) * sqrt(ulRecCount) * 2
       * so the maximum is for DBF with 2^32 records and keySize 256 ~
       * ~ 2^17 * 284 ~=~ 37 Mb
       * this is not a problem for current computers and I do not see
       * any way to use DBFs with four billions records and indexes with
       * such long (256 bytes) keys on the old ones - they will be simply
       * to slow. IMHO it's also better to signal out of memory here and
       * force some system upgrades then run process which will have to
       * take many hours, Druzus.
       */
      ulMax = ulMin;
      pBuf = ( BYTE * ) hb_xgrab( ( ulMax << 1 ) * ( iLen + 4 ) );
   }

   pSort->pTag = pTag;
   pSort->hTempFile = FS_ERROR;
   pSort->keyLen = iLen;
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   pSort->pSwapPage = ( LPNTXSWAPPAGE ) hb_xgrab( sizeof( NTXSWAPPAGE ) * pSort->ulPages );
   memset( pSort->pSwapPage, 0, sizeof( NTXSWAPPAGE ) * pSort->ulPages );

   return pSort;
}

static void hb_ntxSortFree( LPNTXSORTINFO pSort )
{
   if( pSort->hTempFile != FS_ERROR )
   {
      hb_fsClose( pSort->hTempFile );
   }
   if( pSort->szTempFileName )
   {
      hb_fsDelete( (BYTE *)  ( pSort->szTempFileName ) );
      hb_xfree( pSort->szTempFileName );
   }
   if( pSort->pKeyPool )
   {
      hb_xfree( pSort->pKeyPool );
   }
   if( pSort->pSwapPage )
   {
      hb_xfree( pSort->pSwapPage );
   }
   if( pSort->pRecBuff )
   {
      hb_xfree( pSort->pRecBuff );
   }
   if( pSort->pSortedPages )
   {
      hb_xfree( pSort->pSortedPages );
   }
   hb_xfree( pSort );
}

static void hb_ntxSortOut( LPNTXSORTINFO pSort )
{
   BOOL fUnique = pSort->fUnique, fBalance, fNext;
   LPTAGINFO pTag = pSort->pTag;
   ULONG ulPage, ulRec, ulKey;
   USHORT uiHalf;
   BYTE * pKeyVal;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   if( pSort->ulPages > 1 )
   {
      BYTE * pBuf = pSort->pKeyPool;
      hb_ntxSortWritePage( pSort );
      for ( ulPage = 0; ulPage < pSort->ulPages; ulPage++ )
      {
         pSort->pSwapPage[ ulPage ].ulKeyBuf = 0;
         pSort->pSwapPage[ ulPage ].ulCurKey = 0;
         pSort->pSwapPage[ ulPage ].pKeyPool = pBuf;
         pBuf += pSort->ulPgKeys * ( pSort->keyLen + 4 );
      }
   }
   else
   {
      hb_ntxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pKeyPool;
   }

   hb_ntxSortOrderPages( pSort );

   for ( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! hb_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      {
         hb_errInternal( 9999, "hb_ntxSortOut: memory structure corrupted.", "", "" );
      }
      if( fUnique )
      {
         if( ulKey != 0 && hb_ntxValCompare( pTag, (char *) pSort->pLastKey, iLen, (char *) pKeyVal, iLen, TRUE ) == 0 )
         {
            continue;
         }
#ifndef HB_NTX_DEBUG_EXT
         else
         {
            memcpy( pSort->pLastKey, pKeyVal, iLen );
         }
#endif
      }
#ifdef HB_NTX_DEBUG_EXT
      if( ulKey != 0 )
      {
         int i = hb_ntxValCompare( pTag, (char *) pSort->pLastKey, iLen, (char *) pKeyVal, iLen, TRUE );
         if( ! pSort->pTag->AscendKey )
            i = -i;
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf("\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                   ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec); fflush(stdout);
            hb_errInternal( 9999, "hb_ntxSortOut: sorting fails.", "", "" );
         }
      }
      memcpy( pSort->pLastKey, pKeyVal, iLen );
      pSort->ulLastRec = ulRec;
#endif
      hb_ntxSortAddNodeKey( pSort, pKeyVal, ulRec );
   }

#ifdef HB_NTX_DEBUG
   if( hb_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
   {
      hb_errInternal( 9999, "hb_ntxSortOut: memory structure corrupted(2).", "", "" );
   }
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = hb_ntxPageNew( pTag );
   }

   iLevel = 0;
   fNext = TRUE;
   fBalance = FALSE;
   uiHalf = pTag->MaxKeys >> 1;
   do
   {
      if( iLevel + 1 == NTX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = FALSE;
      }
      else
      {
         hb_ntxSetKeyPage( pSort->NodeList[ iLevel + 1 ],
                           pSort->NodeList[ iLevel + 1 ]->uiKeys,
                           pSort->NodeList[ iLevel ]->Page );
         if( pSort->NodeList[ iLevel ]->uiKeys < uiHalf )
         {
            fBalance = TRUE;
         }
      }
      hb_ntxPageRelease( pTag, pSort->NodeList[ iLevel ] );
      iLevel++;
   }
   while( fNext );

   hb_ntxSortFree( pSort );

   if( fBalance )
   {
      LPPAGEINFO pPage, pFirst, pLast;

      ulPage = pTag->RootBlock;
      while( ulPage )
      {
         pPage = hb_ntxPageLoad( pTag, ulPage );
         if( ! pPage )
            return;
         ulPage = hb_ntxGetKeyPage( pPage, pPage->uiKeys );
         if( ulPage && pPage->uiKeys )
         {
            pLast = hb_ntxPageLoad( pTag, ulPage );
            if( ! pLast )
            {
               hb_ntxPageRelease( pTag, pPage );
               return;
            }
            if( pLast->uiKeys < uiHalf )
            {
               pFirst = hb_ntxPageLoad( pTag, hb_ntxGetKeyPage( pPage,
                                                         pPage->uiKeys - 1 ) );
               if( ! pFirst )
               {
                  hb_ntxPageRelease( pTag, pPage );
                  hb_ntxPageRelease( pTag, pLast );
                  return;
               }
               hb_ntxBalancePages( pTag, pPage, pPage->uiKeys - 1, pFirst, pLast );
               hb_ntxPageRelease( pTag, pFirst );
            }
            hb_ntxPageRelease( pTag, pLast );
         }
         hb_ntxPageRelease( pTag, pPage );
      }
   }
}

static ERRCODE hb_ntxTagCreate( LPTAGINFO pTag )
{
   LPNTXAREA pArea = pTag->Owner->Owner;
   PHB_ITEM pForItem, pWhileItem = NULL, pEvalItem = NULL, pItem = NULL;
   ULONG ulRecCount, ulRecNo = pArea->ulRecNo;
   LONG lStep = 0;
   ERRCODE errCode;

   errCode = SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
   if( errCode != SUCCESS )
      return errCode;

   if( pArea->lpdbOrdCondInfo )
   {
      pWhileItem = pArea->lpdbOrdCondInfo->itmCobWhile;
      lStep = pArea->lpdbOrdCondInfo->lStep;
      pEvalItem = pArea->lpdbOrdCondInfo->itmCobEval;
   }
   if( pTag->Custom || ulRecCount == 0 )
   {
      LPPAGEINFO pPage = hb_ntxPageNew( pTag );
      pTag->RootBlock = pPage->Page;
      hb_ntxPageRelease( pTag, pPage );
   }
   else
   {
      LPNTXSORTINFO pSort;
      LPTAGINFO pSaveTag = pArea->lpCurTag;
      ULONG ulStartRec = 0, ulNextCount = 0;
      BOOL fDirectRead, fUseFilert = FALSE;
      BYTE * pSaveRecBuff = pArea->pRecord;
      char szBuffer[ NTX_MAX_KEY ];
      int iRecBuff = 0, iRecBufSize = USHRT_MAX / pArea->uiRecordLen, iRec;
#ifndef HB_CDP_SUPPORT_OFF
      /* TODO: this hack is not thread safe, hb_cdp_page has to be thread specific */
      PHB_CODEPAGE cdpTmp = hb_cdp_page;
      hb_cdp_page = pArea->cdPage;
#endif

      pArea->pSort = pSort = hb_ntxSortNew( pTag, ulRecCount );
      pForItem = pTag->pForItem;
      if( pTag->nField )
         pItem = hb_itemNew( NULL );

      if( !pArea->lpdbOrdCondInfo || pArea->lpdbOrdCondInfo->fAll )
      {
         pArea->lpCurTag = NULL;
      }
      else if( pArea->lpdbOrdCondInfo->lRecno )
      {
         ulStartRec = pArea->lpdbOrdCondInfo->lRecno;
         ulNextCount = 1;
      }
      else if( pArea->lpdbOrdCondInfo->fRest || pArea->lpdbOrdCondInfo->lNextCount > 0 )
      {
         if( pArea->lpdbOrdCondInfo->lStartRecno )
            ulStartRec = pArea->lpdbOrdCondInfo->lStartRecno;
         else
            ulStartRec = ulRecNo;
         if( pArea->lpdbOrdCondInfo->lNextCount > 0 )
            ulNextCount = pArea->lpdbOrdCondInfo->lNextCount;
      }
      else if( !pArea->lpdbOrdCondInfo->fUseCurrent )
      {
         pArea->lpCurTag = NULL;
      }

      fDirectRead = !hb_set.HB_SET_STRICTREAD && /* !pArea->lpdbRelations && */
                    ( !pArea->lpdbOrdCondInfo || pArea->lpdbOrdCondInfo->fAll ||
                      pArea->lpCurTag == NULL );

      if( fDirectRead )
         pSort->pRecBuff = (BYTE *) hb_xgrab( pArea->uiRecordLen * iRecBufSize );

      if( ulStartRec == 0 && pArea->lpCurTag == NULL )
         ulStartRec = 1;

      if( ulStartRec == 0 )
      {
         errCode = SELF_GOTOP( ( AREAP ) pArea );
      }
      else
      {
         errCode = SELF_GOTO( ( AREAP ) pArea, ulStartRec );
         if( fUseFilert && errCode == SUCCESS )
            errCode = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
      }

      ulRecNo = pArea->ulRecNo;

      if( ulNextCount && ulRecCount >= ulRecNo + ulNextCount )
         ulRecCount = ulRecNo + ulNextCount - 1;

      while( errCode == SUCCESS )
      {
         if( ulRecNo > ulRecCount )
            pArea->fEof = TRUE;

         if( pArea->fEof )
            break;

         if( fDirectRead )
         {
            if( iRecBuff == 0 || iRecBuff >= iRecBufSize )
            {
               if( ulRecCount - ulRecNo >= (ULONG) iRecBufSize )
                  iRec = iRecBufSize;
               else
                  iRec = ulRecCount - ulRecNo + 1;
               if( iRec <= 0 )
                  break;
               hb_fsSeekLarge( pArea->hDataFile,
                               ( HB_FOFFSET ) pArea->uiHeaderLen + 
                               ( HB_FOFFSET ) ( ulRecNo - 1 ) * 
                               ( HB_FOFFSET ) pArea->uiRecordLen, FS_SET );
               hb_fsReadLarge( pArea->hDataFile, pSort->pRecBuff, pArea->uiRecordLen * iRec );
               iRecBuff = 0;
            }
            pArea->pRecord = pSort->pRecBuff + iRecBuff * pArea->uiRecordLen;
            pArea->fValidBuffer = TRUE;
            pArea->ulRecNo = ulRecNo;
            pArea->fDeleted = ( pArea->pRecord[ 0 ] == '*' );
            /* Force relational movement in child WorkAreas */
            if( pArea->lpdbRelations )
            {
               errCode = SELF_SYNCCHILDREN( ( AREAP ) pArea );
               if( errCode == SUCCESS )
                  break;
            }
            iRecBuff++;
         }

         if( pWhileItem && !hb_ntxEvalCond( NULL, pWhileItem, FALSE ) )
            break;

         if( pForItem == NULL || hb_ntxEvalCond( pArea, pForItem, FALSE ) )
         {
            if( pTag->nField )
               errCode = SELF_GETVALUE( ( AREAP ) pArea, pTag->nField, pItem );
            else
               pItem = hb_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( hb_itemType( pItem ) )
            {
               case HB_IT_STRING:
               case HB_IT_STRING | HB_IT_MEMO:
                  hb_ntxSortKeyAdd( pSort, pArea->ulRecNo,
                                    pItem->item.asString.value,
                                    pItem->item.asString.length );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DOUBLE:
                  hb_ntxNumToStr( pItem, szBuffer, pTag->KeyLength, pTag->KeyDec );
                  hb_ntxSortKeyAdd( pSort, pArea->ulRecNo, szBuffer, pTag->KeyLength );
                  break;

               case HB_IT_DATE:
                  hb_itemGetDS( pItem, szBuffer );
                  hb_ntxSortKeyAdd( pSort, pArea->ulRecNo, szBuffer, 8 );
                  break;

               case HB_IT_LOGICAL:
                  szBuffer[0] = hb_itemGetL( pItem ) ? 'T' : 'F';
                  hb_ntxSortKeyAdd( pSort, pArea->ulRecNo, szBuffer, 1 );
                  break;

               default:
                  if( hb_vmRequestQuery() )
                  {
                     pEvalItem = NULL;
                     ulNextCount = 1;
                  }
                  else
                  {
                     printf( "hb_ntxTagDoIndex: hb_itemType( pItem ) = %i", hb_itemType( pItem ) );
                     /* TODO: !!! runtime error */
                     errCode = FAILURE;
                  }
                  break;
            }
         }

         if( ulNextCount > 0 )
         {
            if( --ulNextCount == 0 )
               break;
         }

         if( pEvalItem )
         {
            if( lStep >= pArea->lpdbOrdCondInfo->lStep )
            {
               lStep = 0;
               if( !hb_ntxEvalCond( pArea, pEvalItem, FALSE ) )
                  break;
            }
            ++lStep;
         }

         if( fDirectRead )
            ulRecNo++;
         else if( errCode == SUCCESS )
         {
            errCode = SELF_SKIPRAW( ( AREAP ) pArea, 1 );
            if( fUseFilert && errCode == SUCCESS )
               errCode = SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
            ulRecNo = pArea->ulRecNo;
         }
      }

      if( errCode == SUCCESS )
         hb_ntxSortOut( pSort );
      else
         hb_ntxSortFree( pSort );
      pArea->pSort = NULL;
      if( pTag->nField )
         hb_itemRelease( pItem );

      if( fDirectRead )
      {
         pArea->pRecord = pSaveRecBuff;
         pArea->fValidBuffer = FALSE;
         if( errCode == SUCCESS )
            errCode = SELF_GOTO( ( AREAP ) pArea, ulRecNo );
      }
      pArea->lpCurTag = pSaveTag;
#ifndef HB_CDP_SUPPORT_OFF
      hb_cdp_page = cdpTmp;
#endif
   }
   return errCode;
}
