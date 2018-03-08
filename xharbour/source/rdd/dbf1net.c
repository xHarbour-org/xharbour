/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBF RDD module
 *
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 *
 * www - http://www.xharbour.org
 *
 * Copyright 2009 Miguel Angel Marchuet <soporte-2@dsgsoftware.com> (migration to client/server)
 * of DSG Software S.L.
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

#define HB_TRIGVAR_BYREF

#include "hbrdddbf.h"
#include "hbdbsort.h"

#include "hbapi.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapiitm.h"


#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbmath.h"

#include "hbsxfunc.h"
#include "error.ch"
#include "rddsys.ch"
#include "hbsxdef.ch"

#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

/* at net.c */
void hb_netname( char * pszNetName, BOOL bGetUser );

static RDDFUNCS dbfSuper;


/*
 * Common functions.
 */

#define HB_BLANK_APPEND    1
#define HB_BLANK_EOF       2
#define HB_BLANK_ROLLBACK  3

#define HB_BLANK_SKIP      100
#define HB_BLANK_AUTOINC   101

/*
 * generate Run-Time error
 */
static HB_ERRCODE hb_dbfErrorRT( DBFAREAP pArea,
                                 HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                                 const char * szFileName, HB_ERRCODE errOsCode,
                                 USHORT uiFlags, PHB_ITEM * pErrorPtr )
{
   PHB_ITEM    pError;
   HB_ERRCODE  errCode = HB_FAILURE;

   if( hb_vmRequestQuery() == 0 )
   {
      if( pErrorPtr )
      {
         if( ! *pErrorPtr )
            *pErrorPtr = hb_errNew();
         pError = *pErrorPtr;
      }
      else
         pError = hb_errNew();
      hb_errPutGenCode( pError, errGenCode );
      hb_errPutSubCode( pError, errSubCode );
      hb_errPutOsCode( pError, errOsCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( errGenCode ) );
      if( szFileName )
         hb_errPutFileName( pError, szFileName );
      if( uiFlags )
         hb_errPutFlags( pError, uiFlags );
      errCode = SELF_ERROR( ( AREAP ) pArea, pError );
      if( ! pErrorPtr )
         hb_errRelease( pError );
   }
   return errCode;
}

static HB_LONG hb_dbfGetRowVer( DBFAREAP pArea, USHORT uiField, HB_LONG * pValue )
{
   DBFFIELD dbField;
   BOOL     fLck = FALSE;

   *pValue = 0;
   if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
   {
      if( SELF_RAWLOCK( ( AREAP ) pArea, HEADER_LOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
      fLck = TRUE;
   }

   if( hb_fileNetReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      *pValue = HB_GET_LE_UINT64( dbField.bReserved2 ) + 1;
      HB_PUT_LE_UINT64( dbField.bReserved2, *pValue );
      hb_fileNetWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }

   if( fLck )
   {
      if( SELF_RAWLOCK( ( AREAP ) pArea, HEADER_UNLOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_LONG hb_dbfGetNextValue( DBFAREAP pArea, USHORT uiField )
{
   HB_LONG  nValue = 0;
   DBFFIELD dbField;

   if( hb_fileNetReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      nValue = HB_GET_LE_UINT32( dbField.bCounter );
      HB_PUT_LE_UINT32( dbField.bCounter, nValue + dbField.bStep );
      hb_fileNetWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }

   return nValue;
}

static HB_LONG hb_dbfSetNextValue( DBFAREAP pArea, USHORT uiField, HB_LONG nNextValue )
{
   HB_LONG  nPreviousValue = 0;
   DBFFIELD dbField;

   if( hb_fileNetReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      nPreviousValue = HB_GET_LE_UINT32( dbField.bCounter );
      HB_PUT_LE_UINT32( dbField.bCounter, nNextValue );
      hb_fileNetWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }

   return nPreviousValue;
}

/*
   0-1  The first two bytes (binary integer) tell whether a user has changed the record. Every committed change
     is counted encreasing the count by one.
   2-4  The next three characters tell the time a user placed the lock. (10h 09h 07h i.e. 16:09:07)
   5-7  The next three characters tell the date a user placed the lock. ( 60h 09h 0Bh i.e. (19)96-09-11 )
   8-24 The remaining 16 characters are optional. They tell the name of the user that placed the lock.
 */
static void hb_dbfUpdatedbaselockValue( DBFAREAP pArea, ULONG ulRecNo )
{
   if( pArea->uidbaselock && ulRecNo )
   {
      BYTE *   pPtr;
      BYTE *   pRecord = ( BYTE * ) hb_xgrab( pArea->uiRecordLen );


      if( hb_fileNetReadAt( pArea->pDataFile, pRecord, pArea->uiRecordLen,
                            ( HB_FOFFSET ) pArea->uiHeaderLen +
                            ( HB_FOFFSET ) ( ulRecNo - 1 ) *
                            ( HB_FOFFSET ) pArea->uiRecordLen ) == ( ULONG ) pArea->uiRecordLen )
      {

         pPtr = pRecord + pArea->pFieldOffset[ pArea->uidbaselock ] + 2;
         hb_dbaselockEncode( ( char * ) pPtr );
         /* Login name of user who locked record or file */
         if( pArea->area.lpFields[ pArea->uidbaselock ].uiLen >= 24 )
            hb_netname( ( char * ) pPtr + 6, 0 );

         hb_fileNetWriteAt( pArea->pDataFile, pPtr, pArea->area.lpFields[ pArea->uidbaselock ].uiLen - 3,
                            ( HB_FOFFSET ) pArea->uiHeaderLen +
                            ( HB_FOFFSET ) ( ( ulRecNo - 1 ) * pArea->uiRecordLen ) +
                            ( HB_FOFFSET ) ( pArea->pFieldOffset[ pArea->uidbaselock ] + 2 ) );
      }
      hb_xfree( pRecord );
   }
}

static void hb_dbfUpdateStampFields( DBFAREAP pArea )
{
   long     lJulian  = 0, lMilliSec = 0;
   HB_LONG  nRowVer  = 0;
   LPFIELD  pField;
   USHORT   uiCount;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      switch( pField->uiType )
      {
         case HB_FT_TIMESTAMP:
         case HB_FT_MODTIME:
         {
            BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( lJulian == 0 )
               hb_dateTimeStamp( &lJulian, &lMilliSec );
            HB_PUT_LE_UINT32( pPtr, lJulian );
            pPtr += 4;
            HB_PUT_LE_UINT32( pPtr, lMilliSec );
            break;
         }
         case HB_FT_ROWVER:
         {
            BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( nRowVer == 0 )
               hb_dbfGetRowVer( pArea, uiCount, &nRowVer );
            HB_PUT_LE_UINT64( pPtr, nRowVer );
            break;
         }
      }
   }
}

static void hb_dbfSetBlankRecord( DBFAREAP pArea, int iType )
{
   BYTE *   pPtr     = pArea->pRecord, bFill = ' ', bNext;
   ULONG    ulSize   = 1; /* 1 byte ' ' for DELETE flag */
   USHORT   uiCount;
   LPFIELD  pField;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      USHORT uiLen = pField->uiLen;

      switch( pField->uiType )
      {
         case HB_FT_MEMO:
         case HB_FT_PICTURE:
         case HB_FT_BLOB:
         case HB_FT_OLE:
         case HB_FT_BINARY:
            bNext = uiLen == 10 ? ' ' : '\0';
            break;

         case HB_FT_DATE:
            bNext = uiLen == 8 ? ' ' : '\0';
            break;

         case HB_FT_STRING:
         case HB_FT_LOGICAL:
            bNext = ' ';
            break;

         case HB_FT_LONG:
         case HB_FT_FLOAT:
            if( pField->uiFlags & HB_FF_AUTOINC )
            {
               if( iType == HB_BLANK_APPEND )
               {
                  bNext = HB_BLANK_AUTOINC;
                  break;
               }
               else if( iType == HB_BLANK_ROLLBACK )
               {
                  bNext = HB_BLANK_SKIP;
                  break;
               }
            }
            bNext = ' ';
            break;

         case HB_FT_AUTOINC:
            if( iType == HB_BLANK_APPEND )
               bNext = HB_BLANK_AUTOINC;
            else if( iType == HB_BLANK_ROLLBACK )
               bNext = HB_BLANK_SKIP;
            else
               bNext = '\0';
            break;

         case HB_FT_INTEGER:
         case HB_FT_DOUBLE:
            if( pField->uiFlags & HB_FF_AUTOINC )
            {
               if( iType == HB_BLANK_APPEND )
               {
                  bNext = HB_BLANK_AUTOINC;
                  break;
               }
               else if( iType == HB_BLANK_ROLLBACK )
               {
                  bNext = HB_BLANK_SKIP;
                  break;
               }
            }
            bNext = '\0';
            break;

         default:
            bNext = '\0';
            break;
      }

      if( bNext == bFill )
      {
         ulSize += uiLen;
      }
      else
      {
         memset( pPtr, bFill, ulSize );
         pPtr     += ulSize;
         ulSize   = 0;
         if( bNext == HB_BLANK_SKIP )
         {
            pPtr += uiLen;
         }
         else if( bNext == HB_BLANK_AUTOINC )
         {
            HB_LONG nValue = hb_dbfGetNextValue( pArea, uiCount );
            if( pField->uiDec )
               nValue = ( HB_LONG ) hb_numDecConv( ( double ) nValue, -( int ) pField->uiDec );
            if( pField->uiType == HB_FT_INTEGER ||
                pField->uiType == HB_FT_AUTOINC )
            {
               if( uiLen == 1 )
                  *pPtr = ( signed char ) nValue;
               else if( uiLen == 2 )
                  HB_PUT_LE_UINT16( pPtr, nValue );
               else if( uiLen == 3 )
                  HB_PUT_LE_UINT24( pPtr, nValue );
               else if( uiLen == 4 )
                  HB_PUT_LE_UINT32( pPtr, nValue );
               else if( uiLen == 8 )
                  HB_PUT_LE_UINT64( pPtr, nValue );
            }
            else if( pField->uiType == HB_FT_DOUBLE )
            {
               HB_PUT_LE_DOUBLE( pPtr, nValue );
            }
            else
            {
               USHORT ui = uiLen;
               do
               {
                  pPtr[ --ui ]   = ( BYTE ) nValue % 10 + '0';
                  nValue         /= 10;
               }
               while( ui && nValue >= 1 );
            }
            pPtr += uiLen;
         }
         else
         {
            ulSize   = uiLen;
            bFill    = bNext;
         }
      }
   }
   memset( pPtr, bFill, ulSize );

   ulSize = ( ULONG ) ( pArea->pRecord - pPtr - ulSize );
   if( ulSize < ( ULONG ) pArea->uiRecordLen )
      memset( pPtr, '\0', ( ULONG ) pArea->uiRecordLen - ulSize );
}

/*
 * Executes user trigger function
 */
static BOOL hb_dbfTriggerDo( DBFAREAP pArea, int iEvent,
                             int iField, PHB_ITEM pItem )
{
   BOOL fResult = TRUE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTriggerDo(%p,%d,%d,%p)", pArea, iEvent, iField, pItem ) );

   if( hb_vmRequestQuery() == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pArea->pTriggerSym );
         hb_vmPushNil();
         /* nEvent */
         hb_vmPushInteger( iEvent );
         /* nArea */
         hb_vmPushInteger( pArea->area.uiArea );
         /* nFieldPos (GET/PUT) */
         hb_vmPushInteger( iField );
         /* xTrigVal (PREUSE/GET/PUT) */
         if( pItem )
         {
#ifdef HB_TRIGVAR_BYREF
            hb_vmPushItemRef( pItem );
#else
            hb_vmPush( pItem );
#endif
            hb_vmDo( 4 );
         }
         else
         {
            /* SIx3 makes: hb_vmPushInteger( 0 ); */
            hb_vmDo( 3 );
         }
         fResult = hb_parl( -1 );
         hb_vmRequestRestore();
      }
   }

   return fResult;
}

/*
 * Set user trigger function
 */
static void hb_dbfTriggerSet( DBFAREAP pArea, PHB_ITEM pTrigger )
{
   const char * szName;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTriggerSet(%p,%p)", pArea, pTrigger ) );

   szName               = hb_itemGetCPtr( pTrigger );
   pArea->pTriggerSym   = *szName ? hb_dynsymFindName( szName ) : NULL;
   if( pArea->pTriggerSym && ! hb_dynsymIsFunction( pArea->pTriggerSym ) )
      pArea->pTriggerSym = NULL;
   pArea->fTrigger      = pArea->pTriggerSym != NULL;
}

/*
 * Return the total number of records.
 */
static ULONG hb_dbfCalcRecCount( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCalcRecCount(%p)", pArea ) );

   if( ! pArea->pDataFile )
      return 0;
   else
      return ( ULONG ) ( ( hb_fileNetSize( pArea->pDataFile ) -
                           pArea->uiHeaderLen ) / pArea->uiRecordLen );
}

/*
 * Read current record from file.
 */
static BOOL hb_dbfReadRecord( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfReadRecord(%p)", pArea ) );

   if( ! pArea->fPositioned )
   {
      pArea->fValidBuffer = TRUE;
      return TRUE;
   }

   if( pArea->ulRecNo > pArea->ulRecCount )
   {
      /* Update record count */
      if( pArea->fShared )
         pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

      if( pArea->ulRecNo > pArea->ulRecCount )
      {
         pArea->area.fEof = pArea->fValidBuffer = TRUE;
         return TRUE;
      }
   }

   /* Read data from file */
   if( hb_fileNetReadAt( pArea->pDataFile, pArea->pRecord, pArea->uiRecordLen,
                         ( HB_FOFFSET ) pArea->uiHeaderLen +
                         ( HB_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                         ( HB_FOFFSET ) pArea->uiRecordLen ) !=
       ( ULONG ) pArea->uiRecordLen )
   {
      hb_dbfErrorRT( pArea, EG_READ, EDBF_READ,
                     pArea->szDataFileName, hb_fsError(), 0, NULL );
      return FALSE;
   }

   if( SELF_GETREC( ( AREAP ) pArea, NULL ) == HB_FAILURE )
      return FALSE;

   /* Set flags */
   pArea->fValidBuffer  = pArea->fPositioned = TRUE;
   pArea->fDeleted      = pArea->pRecord[ 0 ] == '*';
   return TRUE;
}

/*
 * Write current record to file.
 */
static BOOL hb_dbfWriteRecord( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfWriteRecord(%p)", pArea ) );

   if( SELF_PUTREC( ( AREAP ) pArea, NULL ) == HB_FAILURE )
      return FALSE;

   pArea->fRecordChanged   = FALSE;
   pArea->fDataFlush       = TRUE;
   return TRUE;
}

/*
 * Transaction table ON/OFF
 */
static void hb_dbfTableTransaction( DBFAREAP pArea, BOOL fTransaction )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTableTransaction(%p,%d)", pArea, fTransaction ) );

   if( ! pArea->fReadonly )
   {
      if( pArea->dbfHeader.bTransaction && ! fTransaction )
      {
         /* TODO reset transaction of current session */
      }
      pArea->dbfHeader.bTransaction = fTransaction ? ( BYTE ) 1 : ( BYTE ) 0;
      pArea->fUpdateHeader          = TRUE;
      SELF_WRITEDBHEADER( ( AREAP ) pArea );
   }
}

/*
 * Set encryption password
 */
static BOOL hb_dbfPasswordSet( DBFAREAP pArea, PHB_ITEM pPasswd, BOOL fRaw )
{
   char  byBuffer[ 8 ];
   ULONG ulLen;
   BOOL  fKeySet = FALSE, fSet;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPasswordSet(%p,%p,%d)", pArea, pPasswd, fRaw ) );

   ulLen = ( ULONG ) hb_itemGetCLen( pPasswd );

   fSet  = ! pArea->fHasMemo && HB_IS_STRING( pPasswd ) && ( ! fRaw || ulLen == 8 );
   if( fSet )
   {
      ulLen = ( ULONG ) hb_itemGetCLen( pPasswd );
      if( ulLen > 0 )
      {
         if( ulLen < 8 )
         {
            HB_MEMCPY( byBuffer, hb_itemGetCPtr( pPasswd ), ( size_t ) ulLen );
            memset( byBuffer + ulLen, '\0', ( size_t ) ( 8 - ulLen ) );
         }
         else
            HB_MEMCPY( byBuffer, hb_itemGetCPtr( pPasswd ), ( size_t ) 8 );
      }
   }

   if( pArea->pCryptKey )
      hb_itemPutCL( pPasswd, pArea->pCryptKey, 8 );
   else
      hb_itemClear( pPasswd );

   if( fSet )
   {
      if( pArea->pRecord && pArea->fPositioned )
      {
         SELF_GOCOLD( ( AREAP ) pArea );
         pArea->fValidBuffer = FALSE;
      }
      if( pArea->pCryptKey )
      {
         /* clean the memory with password key - though it's not
          * a serious actions in such case ;-)
          */
         memset( pArea->pCryptKey, '\0', ( size_t ) 8 );
         hb_xfree( pArea->pCryptKey );
         pArea->pCryptKey = NULL;
      }
      if( ulLen > 0 )
      {
         /* at this moment only one encryption method is used,
            I'll add other later, [druzus] */
         pArea->bCryptType = DB_CRYPT_SIX;
         pArea->pCryptKey  = ( char * ) hb_xgrab( 8 );

         /* SIX encode the key with its own value before use */
         if( ! fRaw )
            hb_sxEnCrypt( byBuffer, pArea->pCryptKey, byBuffer, 8 );
         else
            HB_MEMCPY( pArea->pCryptKey, byBuffer, ( size_t ) 8 );
         fKeySet = TRUE;
      }
   }

   return fKeySet;
}

/*
 * Encrypt/Decrypt table
 */
static void hb_dbfTableCrypt( DBFAREAP pArea, PHB_ITEM pPasswd, BOOL fEncrypt )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTableCrypt(%p,%p,%d)", pArea, pPasswd, fEncrypt ) );

   if( ! pArea->fReadonly && ! pArea->fShared &&
       fEncrypt ? ! pArea->fTableEncrypted && ! pArea->fHasMemo :
       pArea->fTableEncrypted )
   {
      ULONG ulRecords, ulRecNo;

      if( SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == HB_SUCCESS )
      {
         HB_ERRCODE  errCode = HB_SUCCESS;
         char *      pOldCryptKey, * pNewCryptKey;

         pOldCryptKey      = pArea->pCryptKey;
         pArea->pCryptKey  = NULL;
         hb_dbfPasswordSet( pArea, pPasswd, FALSE );
         pNewCryptKey      = pArea->pCryptKey;
         if( ! fEncrypt && pNewCryptKey )
         {
            if( pOldCryptKey )
               hb_xfree( pNewCryptKey );
            else
               pOldCryptKey = pNewCryptKey;
            pNewCryptKey = NULL;
         }
         for( ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo )
         {
            pArea->pCryptKey  = pOldCryptKey;
            errCode           = SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            if( errCode != HB_SUCCESS )
               break;
            if( ! hb_dbfReadRecord( pArea ) )
            {
               errCode = HB_FAILURE;
               break;
            }
            pArea->pCryptKey = pNewCryptKey;
            /* Buffer is hot? */
            if( ! pArea->fRecordChanged )
            {
               errCode = SELF_GOHOT( ( AREAP ) pArea );
               if( errCode != HB_SUCCESS )
                  break;
            }
            /* Force record encryption/decryption */
            pArea->fEncrypted = fEncrypt;
            /* Save encrypted record */
            errCode           = SELF_GOCOLD( ( AREAP ) pArea );
            if( errCode != HB_SUCCESS )
               break;
         }
         pArea->pCryptKey = pNewCryptKey;
         if( pOldCryptKey )
            hb_xfree( pOldCryptKey );
         if( errCode == HB_SUCCESS )
         {
            pArea->fTableEncrypted  = fEncrypt;
            pArea->fUpdateHeader    = TRUE;
            SELF_WRITEDBHEADER( ( AREAP ) pArea );
         }
      }
   }
}

/*
 * Unlock all records.
 */
static HB_ERRCODE hb_dbfUnlockAllRecords( DBFAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockAllRecords(%p)", pArea ) );

   if( pArea->pLocksPos )
   {
      ULONG ulCount;

      errCode = SELF_GOCOLD( ( AREAP ) pArea );
      for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
         SELF_RAWLOCK( ( AREAP ) pArea, REC_UNLOCK, pArea->pLocksPos[ ulCount ] );
      hb_xfree( pArea->pLocksPos );
      pArea->pLocksPos = NULL;
   }
   pArea->ulNumLocksPos = 0;
   return errCode;
}

/*
 * Unlock a records.
 */
static HB_ERRCODE hb_dbfUnlockRecord( DBFAREAP pArea, ULONG ulRecNo )
{
   HB_ERRCODE  errCode = HB_SUCCESS;
   ULONG       ulCount, * pList;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockRecord(%p, %lu)", pArea, ulRecNo ) );

   /* Search the locked record */
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos &&
        pArea->pLocksPos[ ulCount ] != ulRecNo; ulCount++ )
   {
   }

   if( ulCount < pArea->ulNumLocksPos )
   {
      errCode = SELF_GOCOLD( ( AREAP ) pArea );
      SELF_RAWLOCK( ( AREAP ) pArea, REC_UNLOCK, ulRecNo );
      if( pArea->ulNumLocksPos == 1 )            /* Delete the list */
      {
         hb_xfree( pArea->pLocksPos );
         pArea->pLocksPos     = NULL;
         pArea->ulNumLocksPos = 0;
      }
      else                                       /* Resize the list */
      {
         pList             = pArea->pLocksPos + ulCount;
         memmove( pList, pList + 1, ( pArea->ulNumLocksPos - ulCount - 1 ) *
                  sizeof( ULONG ) );
         pArea->pLocksPos  = ( ULONG * ) hb_xrealloc( pArea->pLocksPos,
                                                      ( pArea->ulNumLocksPos - 1 ) *
                                                      sizeof( ULONG ) );
         pArea->ulNumLocksPos--;
      }
   }
   return errCode;
}

/*
 * Lock a record.
 */
static HB_ERRCODE hb_dbfLockRecord( DBFAREAP pArea, ULONG ulRecNo, USHORT * pResult,
                                    BOOL bExclusive )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLockRecord(%p, %lu, %p, %i)", pArea, ulRecNo,
                            pResult, ( int ) bExclusive ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( pArea->fFLocked )
   {
      *pResult = TRUE;
      return HB_SUCCESS;
   }

   if( ulRecNo == 0 )
      ulRecNo = pArea->ulRecNo;

   if( bExclusive )
   {
      hb_dbfUnlockAllRecords( pArea );
   }
   else if( pArea->ulNumLocksPos > 0 )
   {
      ULONG ul;
      for( ul = 0; ul < pArea->ulNumLocksPos; ul++ )
      {
         if( pArea->pLocksPos[ ul ] == ulRecNo )
         {
            *pResult = TRUE;
            return HB_SUCCESS;
         }
      }
   }

   if( SELF_RAWLOCK( ( AREAP ) pArea, REC_LOCK, ulRecNo ) == HB_SUCCESS )
   {
      if( pArea->ulNumLocksPos == 0 )               /* Create the list */
      {
         pArea->pLocksPos = ( ULONG * ) hb_xgrab( sizeof( ULONG ) );
      }
      else                                          /* Resize the list */
      {
         pArea->pLocksPos = ( ULONG * ) hb_xrealloc( pArea->pLocksPos,
                                                     ( pArea->ulNumLocksPos + 1 ) *
                                                     sizeof( ULONG ) );
      }
      pArea->pLocksPos[ pArea->ulNumLocksPos++ ]   = ulRecNo;
      *pResult                                     = TRUE;
      if( ulRecNo == pArea->ulRecNo )
      {
         if( ! pArea->fPositioned )
         {
            if( SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo ) != HB_SUCCESS )
               return HB_FAILURE;
         }
         else if( ! pArea->fRecordChanged )
         {
            if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
               return HB_FAILURE;
            pArea->fValidBuffer = FALSE;
         }
      }
      hb_dbfUpdatedbaselockValue( pArea, ulRecNo );
   }
   else
      *pResult = FALSE;
   return HB_SUCCESS;
}

/*
 * Lock a file.
 */
static HB_ERRCODE hb_dbfLockFile( DBFAREAP pArea, USHORT * pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLockFile(%p, %p)", pArea, pResult ) );

   if( ! pArea->fFLocked )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }

      hb_dbfUnlockAllRecords( pArea );

      SELF_RAWLOCK( ( AREAP ) pArea, FILE_LOCK, 0 );
      *pResult = ( USHORT ) pArea->fFLocked;

      if( ! pArea->fPositioned )
      {
         SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );
      }
      else if( ! pArea->fRecordChanged )
      {
         SELF_GOCOLD( ( AREAP ) pArea );
         pArea->fValidBuffer = FALSE;
      }
   }
   else
      *pResult = TRUE;

   return HB_SUCCESS;
}

/*
 * Unlock a file.
 */
static HB_ERRCODE hb_dbfUnlockFile( DBFAREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfUnlockFile(%p)", pArea ) );

   if( pArea->fFLocked )
   {
      errCode = SELF_GOCOLD( ( AREAP ) pArea );
      SELF_RAWLOCK( ( AREAP ) pArea, FILE_UNLOCK, 0 );
   }
   return errCode;
}

/*
 * Test if a record is locked.
 */
static BOOL hb_dbfIsLocked( DBFAREAP pArea, ULONG ulRecNo )
{
   ULONG ulCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfIsLocked(%p)", pArea ) );

   ulCount = pArea->ulNumLocksPos;
   while( ulCount > 0 )
   {
      if( pArea->pLocksPos[ ulCount - 1 ] == ulRecNo )
         return TRUE;
      ulCount--;
   }

   return FALSE;
}

/*
 * Return an array filled all locked records.
 */
static void hb_dbfGetLockArray( DBFAREAP pArea, PHB_ITEM pItem )
{
   ULONG ulCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetLockArray(%p, %p)", pArea, pItem ) );

   hb_arrayNew( pItem, pArea->ulNumLocksPos );
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
   {
      hb_arraySetNInt( pItem, ulCount + 1, pArea->pLocksPos[ ulCount ] );
   }
}

/*
 * Set lock using current locking schemes in additional files (MEMO, INDEX)
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
BOOL hb_dbfnetLockIdxFile( PHB_FILE pFile, BYTE bScheme, USHORT usMode, HB_FOFFSET * pPoolPos )
{
   HB_FOFFSET  ulPos, ulPool, ulSize = 1;
   BOOL        fRet = FALSE;

   if( ! hb_dbfLockIdxGetData( bScheme, &ulPos, &ulPool ) )
   {
      return fRet;
   }

   for(;; )
   {
      switch( usMode & FL_MASK )
      {
         case FL_LOCK:
            if( ulPool )
            {
               if( ( usMode & FLX_SHARED ) != 0 )
                  *pPoolPos = ( HB_FOFFSET ) ( hb_random_num() * ulPool ) + 1;
               else
               {
                  *pPoolPos   = 0;
                  ulSize      = ulPool + 1;
               }
            }
            else
            {
               *pPoolPos = 0;
            }
            break;

         case FL_UNLOCK:
            if( ulPool )
            {
               if( ! *pPoolPos )
                  ulSize = ulPool + 1;
            }
            else
            {
               *pPoolPos = 0;
            }
            break;

         default:
            return FALSE;
      }
      fRet = hb_fileNetLock( pFile, ulPos + *pPoolPos, ulSize, usMode );
      if( fRet || ( usMode & FLX_WAIT ) == 0 || ( usMode & FL_MASK ) != FL_LOCK )
         break;
      /* TODO: call special error handler (LOCKHANDLER) here if fWait */
      hb_releaseCPU( 0 );
   }

   return fRet;
}

/*
 * Get DBF locking parameters
 */
static HB_ERRCODE hb_dbfLockData( DBFAREAP pArea,
                                  HB_FOFFSET * ulPos, HB_FOFFSET * ulFlSize,
                                  HB_FOFFSET * ulRlSize, int * iDir )
{
   switch( pArea->bLockType )
   {
      case DB_DBFLOCK_CLIP:
         *ulPos      = DBF_LOCKPOS_CLIP;
         *iDir       = DBF_LOCKDIR_CLIP;
         *ulFlSize   = DBF_FLCKSIZE_CLIP;
         *ulRlSize   = DBF_RLCKSIZE_CLIP;
         break;

      case DB_DBFLOCK_CL53:
         *ulPos      = DBF_LOCKPOS_CL53;
         *iDir       = DBF_LOCKDIR_CL53;
         *ulFlSize   = DBF_FLCKSIZE_CL53;
         *ulRlSize   = DBF_RLCKSIZE_CL53;
         break;

      case DB_DBFLOCK_CL53EXT:
         *ulPos      = DBF_LOCKPOS_CL53EXT;
         *iDir       = DBF_LOCKDIR_CL53EXT;
         *ulFlSize   = DBF_FLCKSIZE_CL53EXT;
         *ulRlSize   = DBF_RLCKSIZE_CL53EXT;
         break;

      case DB_DBFLOCK_VFP:
         if( pArea->fHasTags )
         {
            *ulPos      = DBF_LOCKPOS_VFPX;
            *iDir       = DBF_LOCKDIR_VFPX;
            *ulFlSize   = DBF_FLCKSIZE_VFPX;
            *ulRlSize   = DBF_RLCKSIZE_VFPX;
         }
         else
         {
            *ulPos      = DBF_LOCKPOS_VFP;
            *iDir       = DBF_LOCKDIR_VFP;
            *ulFlSize   = DBF_FLCKSIZE_VFP;
            *ulRlSize   = DBF_RLCKSIZE_VFP;
         }
         break;

#ifndef HB_LONG_LONG_OFF
      case DB_DBFLOCK_XHB64:
         *ulPos      = DBF_LOCKPOS_XHB64;
         *iDir       = DBF_LOCKDIR_XHB64;
         *ulFlSize   = DBF_FLCKSIZE_XHB64;
         *ulRlSize   = DBF_RLCKSIZE_XHB64;
         break;
#endif
      default:
         *ulPos   = *ulFlSize = *ulRlSize = 0;
         *iDir    = 0;
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * -- DBF METHODS --
 */

/*
 * Determine logical beginning of file.
 */
static HB_ERRCODE hb_dbfBof( DBFAREAP pArea, BOOL * pBof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfBof(%p, %p)", pArea, pBof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pBof = pArea->area.fBof;
   return HB_SUCCESS;
}

/*
 * Determine logical end of file.
 */
static HB_ERRCODE hb_dbfEof( DBFAREAP pArea, BOOL * pEof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfEof(%p, %p)", pArea, pEof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pEof = pArea->area.fEof;
   return HB_SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
static HB_ERRCODE hb_dbfFound( DBFAREAP pArea, BOOL * pFound )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfFound(%p, %p)", pArea, pFound ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pFound = pArea->area.fFound;
   return HB_SUCCESS;
}

/*
 * Position cursor at the last record.
 */
static HB_ERRCODE hb_dbfGoBottom( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoBottom(%p)", pArea ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   pArea->area.fTop     = FALSE;
   pArea->area.fBottom  = TRUE;
   if( SELF_GOTO( ( AREAP ) pArea, pArea->ulRecCount ) != HB_SUCCESS )
      return HB_FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
}

/*
 * Position cursor at a specific physical record.
 */
static HB_ERRCODE hb_dbfGoTo( DBFAREAP pArea, ULONG ulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoTo(%p, %lu)", pArea, ulRecNo ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   /* Update record count */
   if( ulRecNo > pArea->ulRecCount && pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   if( ulRecNo <= pArea->ulRecCount && ulRecNo >= 1 )
   {
      pArea->ulRecNo       = ulRecNo;
      pArea->area.fBof     = pArea->area.fEof = pArea->fValidBuffer = FALSE;
      pArea->fPositioned   = TRUE;
   }
   else /* Out of space */
   {
      pArea->ulRecNo       = pArea->ulRecCount + 1;
      pArea->area.fBof     = pArea->area.fEof = pArea->fValidBuffer = TRUE;
      pArea->fPositioned   = pArea->fDeleted = pArea->fEncrypted = FALSE;

      /* Clear record buffer */
      hb_dbfSetBlankRecord( pArea, HB_BLANK_EOF );
   }
   pArea->area.fFound = FALSE;

   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );
   else
      return HB_SUCCESS;
}

/*
 * Position the cursor to a specific, physical identity.
 */
static HB_ERRCODE hb_dbfGoToId( DBFAREAP pArea, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoToId(%p, %p)", pArea, pItem ) );

   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( ( AREAP ) pArea, hb_itemGetNL( pItem ) );
   else
   {
      hb_dbfErrorRT( pArea, EG_DATATYPE, EDBF_DATATYPE, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
}

/*
 * Position cursor at the first record.
 */
static HB_ERRCODE hb_dbfGoTop( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoTop(%p)", pArea ) );

   pArea->area.fTop     = TRUE;
   pArea->area.fBottom  = FALSE;

   if( SELF_GOTO( ( AREAP ) pArea, 1 ) == HB_FAILURE )
      return HB_FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

#define hb_dbfSeek NULL

/*
 * Reposition cursor relative to current position.
 */
static HB_ERRCODE hb_dbfSkip( DBFAREAP pArea, LONG lToSkip )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSkip(%p, %ld)", pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   pArea->area.fTop = pArea->area.fBottom = FALSE;

   if( lToSkip == 0 || pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter ||
       hb_setGetDeleted() )
      return SUPER_SKIP( ( AREAP ) pArea, lToSkip );

   errCode = SELF_SKIPRAW( ( AREAP ) pArea, lToSkip );

   /* TODO: remove this hack - it's not necessary if SKIPRAW works
      as it should, Druzus */

   /* Move first record and set Bof flag */
   if( errCode == HB_SUCCESS && pArea->area.fBof && lToSkip < 0 )
   {
      errCode           = SELF_GOTOP( ( AREAP ) pArea );
      pArea->area.fBof  = TRUE;
   }

   /* Update Bof and Eof flags */
   if( lToSkip < 0 )
      pArea->area.fEof = FALSE;
   else /* if( lToSkip > 0 ) */
      pArea->area.fBof = FALSE;

   return errCode;
}

#define hb_dbfSkipFilter NULL

/*
 * Reposition cursor, regardless of filter.
 */
static HB_ERRCODE hb_dbfSkipRaw( DBFAREAP pArea, LONG lToSkip )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSkipRaw(%p, %ld)", pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( lToSkip == 0 )
   {
      BOOL bBof, bEof;

      /* Save flags */
      bBof              = pArea->area.fBof;
      bEof              = pArea->area.fEof;

      errCode           = SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );

      /* Restore flags */
      pArea->area.fBof  = bBof;
      pArea->area.fEof  = bEof;
   }
   else if( lToSkip < 0 && ( ULONG ) ( -lToSkip ) >= pArea->ulRecNo )
   {
      errCode           = SELF_GOTO( ( AREAP ) pArea, 1 );
      pArea->area.fBof  = TRUE;
   }
   else
   {
      errCode = SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo + lToSkip );
   }

   return errCode;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_dbfAddField( DBFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfAddField(%p, %p)", pArea, pFieldInfo ) );

   if( pArea->bMemoType == DB_MEMO_SMT &&
       ( pFieldInfo->uiType == HB_FT_MEMO ||
         pFieldInfo->uiType == HB_FT_BINARY ||
         pFieldInfo->uiType == HB_FT_PICTURE ||
         pFieldInfo->uiType == HB_FT_BLOB ||
         pFieldInfo->uiType == HB_FT_OLE ) )
      pFieldInfo->uiLen = 10;

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen                              += pFieldInfo->uiLen;
   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

/*
 * Append a record to the WorkArea.
 */
static HB_ERRCODE hb_dbfAppend( DBFAREAP pArea, BOOL bUnLockAll )
{
   ULONG       ulNewRecord;
   USHORT      fLocked;
   HB_ERRCODE  errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfAppend(%p, %d)", pArea, ( int ) bUnLockAll ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_APPEND, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   if( pArea->fShared )
   {
      fLocked = FALSE;
      if( SELF_RAWLOCK( ( AREAP ) pArea, APPEND_LOCK, 0 ) == HB_SUCCESS )
      {
         /* Update RecCount */
         pArea->ulRecCount = hb_dbfCalcRecCount( pArea );
         ulNewRecord       = pArea->ulRecCount + 1;
         if( pArea->fFLocked || hb_dbfIsLocked( pArea, ulNewRecord ) )
            fLocked = TRUE;
         else if( hb_dbfLockRecord( pArea, ulNewRecord, &fLocked, bUnLockAll ) != HB_SUCCESS )
         {
            if( fLocked )
               hb_dbfUnlockRecord( pArea, ulNewRecord );
            SELF_RAWLOCK( ( AREAP ) pArea, APPEND_UNLOCK, 0 );
            return HB_FAILURE;
         }
      }
      if( ! fLocked )
      {
         SELF_RAWLOCK( ( AREAP ) pArea, APPEND_UNLOCK, 0 );
         hb_dbfErrorRT( pArea, EG_APPENDLOCK, EDBF_APPENDLOCK, NULL, 0,
                        EF_CANDEFAULT, NULL );
         return HB_FAILURE;
      }
   }

   /* Clear record buffer and update pArea */
   hb_dbfSetBlankRecord( pArea, HB_BLANK_APPEND );

   pArea->fValidBuffer                                                                                         = pArea->fUpdateHeader = pArea->fRecordChanged =
                                                                                              pArea->fAppend   = pArea->fPositioned = TRUE;
   pArea->ulRecCount++;
   pArea->ulRecNo                                                                                              = pArea->ulRecCount;
   pArea->fDeleted                                                                                             = pArea->area.fBof = pArea->area.fEof =
                                                                                          pArea->area.fFound   = FALSE;
   pArea->fEncrypted                                                                                           = pArea->pCryptKey != NULL && ! pArea->fHasMemo;

   if( pArea->fShared )
   {
      errCode = SELF_GOCOLD( ( AREAP ) pArea );
      SELF_RAWLOCK( ( AREAP ) pArea, APPEND_UNLOCK, 0 );
      return errCode;
   }
   return HB_SUCCESS;
}

#define hb_dbfCreateFields NULL

/*
 * Delete a record.
 */
static HB_ERRCODE hb_dbfDeleteRec( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDeleteRec(%p)", pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_DELETE, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->pRecord[ 0 ]  = '*';
   pArea->fDeleted      = TRUE;
   return HB_SUCCESS;
}

/*
 * Determine deleted status for a record.
 */
static HB_ERRCODE hb_dbfDeleted( DBFAREAP pArea, BOOL * pDeleted )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDeleted(%p, %p)", pArea, pDeleted ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   *pDeleted = pArea->fDeleted;
   return HB_SUCCESS;
}

#define hb_dbfFieldCount   NULL
#define hb_dbfFieldDisplay NULL
#define hb_dbfFieldInfo    NULL
#define hb_dbfFieldName    NULL

/*
 * Write data buffer to the data store.
 */
static HB_ERRCODE hb_dbfFlush( DBFAREAP pArea )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfFlush(%p)", pArea ) );

   errCode = SELF_GOCOLD( ( AREAP ) pArea );
   if( errCode == HB_SUCCESS )
   {
      if( pArea->fUpdateHeader )
         errCode = SELF_WRITEDBHEADER( ( AREAP ) pArea );
   }

   if( hb_setGetHardCommit() && errCode == HB_SUCCESS )
   {
      if( pArea->fDataFlush )
      {
         hb_fileNetCommit( pArea->pDataFile );
         pArea->fDataFlush = FALSE;
      }
      if( pArea->fHasMemo && pArea->pMemoFile && pArea->fMemoFlush )
      {
         hb_fileNetCommit( pArea->pMemoFile );
         pArea->fMemoFlush = FALSE;
      }
   }

   return errCode;
}

/*
 * Retrieve current record buffer
 */
static HB_ERRCODE hb_dbfGetRec( DBFAREAP pArea, BYTE ** pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetRec(%p, %p)", pArea, pBuffer ) );

   if( pBuffer != NULL )
   {
      /* Read record */
      if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
         return HB_FAILURE;

      *pBuffer = pArea->pRecord;
   }
   else
   {
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         pArea->fEncrypted    = TRUE;
         pArea->pRecord[ 0 ]  = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
         if( pArea->pCryptKey && pArea->bCryptType == DB_CRYPT_SIX )
         {
            hb_sxDeCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pArea->pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }
      else
      {
         pArea->fEncrypted = FALSE;
      }
   }
   return HB_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static HB_ERRCODE hb_dbfGetValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD  pField;
   PHB_ITEM pError;
   BOOL     fError;
   char *   pszVal;
   ULONG    ulLen;;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetValue(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   fError   = FALSE;
   uiIndex--;
   pField   = pArea->area.lpFields + uiIndex;
   switch( pField->uiType )
   {
      case HB_FT_STRING:
         ulLen = pField->uiLen;
#ifndef HB_CDP_SUPPORT_OFF
         if( pArea->area.cdPage != hb_cdppage() && ( pField->uiFlags & HB_FF_BINARY ) == 0 )
         {
            pszVal = hb_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 ( HB_SIZE * ) &ulLen, pArea->area.cdPage, hb_cdppage() );
            hb_itemPutCPtr( pItem, pszVal, ulLen );

         }
         else
#endif
         {
            pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            hb_itemPutCL( pItem, pszVal, ulLen );
         }
         break;

      case HB_FT_LOGICAL:
         hb_itemPutL( pItem, pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'T' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 't' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'Y' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'y' );
         break;

      case HB_FT_DATE:
         if( pField->uiLen == 3 )
         {
            hb_itemPutDL( pItem, HB_GET_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         }
         else if( pField->uiLen == 4 )
         {
            hb_itemPutDL( pItem, HB_GET_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         }
         else
         {
            hb_itemPutDS( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         }
         break;

      case HB_FT_TIME:
         hb_itemPutDTL( pItem, 0, HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         break;
      case HB_FT_MODTIME:
      case HB_FT_DATETIME:
      case HB_FT_TIMESTAMP:
         hb_itemPutDTL( pItem,
                        HB_GET_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                        HB_GET_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + 4 ) );
         break;

      case HB_FT_INTEGER:
      case HB_FT_CURRENCY:
      case HB_FT_AUTOINC:
      case HB_FT_ROWVER:
         if( pField->uiDec )
         {
            double   dValue;
            int      iLen;
            switch( pField->uiLen )
            {
               case 1:
                  dValue   = ( SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
                  iLen     = 4;
                  break;
               case 2:
                  dValue   = HB_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen     = 6;
                  break;
               case 3:
                  dValue   = HB_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen     = 10;
                  break;
               case 4:
                  dValue   = HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen     = 10;
                  break;
               case 8:
                  dValue   = ( double ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen     = 20;
                  break;
               default:
                  dValue   = 0;
                  iLen     = 0;
                  fError   = TRUE;
                  break;
            }
            hb_itemPutNDLen( pItem, hb_numDecConv( dValue, ( int ) pField->uiDec ),
                             iLen, ( int ) pField->uiDec );
         }
         else
         {
            switch( pField->uiLen )
            {
               case 1:
                  hb_itemPutNILen( pItem, ( SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], 4 );
                  break;
               case 2:
                  hb_itemPutNILen( pItem, ( int ) HB_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 6 );
                  break;
               case 3:
                  hb_itemPutNIntLen( pItem, ( HB_LONG ) HB_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 4:
                  hb_itemPutNIntLen( pItem, ( HB_LONG ) HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 8:
#ifndef HB_LONG_LONG_OFF
                  hb_itemPutNIntLen( pItem, ( HB_LONG ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20 );
#else
                  hb_itemPutNLen( pItem, ( double ) HB_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20, 0 );
#endif
                  break;
               default:
                  fError = TRUE;
                  break;
            }
         }
         break;

      case HB_FT_DOUBLE:
      case HB_FT_CURDOUBLE:
         hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                          20 - ( pField->uiDec > 0 ? ( pField->uiDec + 1 ) : 0 ),
                          ( int ) pField->uiDec );
         break;

      case HB_FT_LONG:
      case HB_FT_FLOAT:
         /* DBASE documentation defines maximum numeric field size as 20
          * but Clipper allows to create longer fields so I remove this
          * limit, Druzus
          */
         /*
            if( pField->uiLen > 20 )
            fError = TRUE;
            else
          */
      {
         HB_LONG  lVal;
         double   dVal;
         BOOL     fDbl;

         fDbl = hb_strnToNum( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                              pField->uiLen, &lVal, &dVal );

         if( pField->uiDec )
         {
            hb_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                             ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                             ( int ) pField->uiDec );
         }
         else if( fDbl )
         {
            hb_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
         }
         else
         {
            hb_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
         }
      }
      break;

      case HB_FT_ANY:
         if( pField->uiLen == 3 )
         {
            hb_itemPutDL( pItem, hb_sxPtoD( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         }
         else if( pField->uiLen == 4 )
         {
            hb_itemPutNIntLen( pItem, ( HB_LONG ) HB_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
         }
         else
         {
            fError = TRUE;
         }
         break;

      case HB_FT_MEMO:
      case HB_FT_BINARY:
      case HB_FT_OLE:
      case HB_FT_PICTURE:
      case HB_FT_BLOB:
      default:
         fError = TRUE;
         break;
   }

   /* Any error? */
   if( fError )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return HB_FAILURE;
   }

#ifdef HB_COMPAT_FOXPRO
   uiIndex = pArea->area.uNullFlagField;
   if( uiIndex && pField->bNullPos && ( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] & pField->bNullPos ) )
      hb_itemPutNull( pItem );
#endif

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_GET, uiIndex + 1, pItem ) )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Obtain the length of a field value.
 */
static HB_ERRCODE hb_dbfGetVarLen( DBFAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength ) );

   *pLength = pArea->area.lpFields[ uiIndex - 1 ].uiLen;

   return HB_SUCCESS;
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_dbfGoCold( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoCold(%p)", pArea ) );

   if( pArea->fRecordChanged )
   {
      if( pArea->fTrigger )
      {
         /* The pending relation may move the record pointer so we should
            disable them for trigger evaluation */
         LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
         pArea->lpdbPendingRel = NULL;

         hb_dbfTriggerDo( pArea, EVENT_UPDATE, 0, NULL );

         /* Restore disabled pending relation */
         pArea->lpdbPendingRel = lpdbPendingRel;
      }

      if( pArea->fModStamp )
         hb_dbfUpdateStampFields( pArea );

      /* Write current record */
      if( ! hb_dbfWriteRecord( pArea ) )
         return HB_FAILURE;

      if( pArea->fAppend )
      {
         pArea->fUpdateHeader = TRUE;
         pArea->fAppend       = FALSE;
      }

      /* Update header */
      if( pArea->fShared && pArea->fUpdateHeader )
         return SELF_WRITEDBHEADER( ( AREAP ) pArea );
   }
   return HB_SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_dbfGoHot( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGoHot(%p)", pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   else if( pArea->fShared && ! pArea->fFLocked &&
            ! hb_dbfIsLocked( pArea, pArea->ulRecNo ) )
   {
      hb_dbfErrorRT( pArea, EG_UNLOCKED, EDBF_UNLOCKED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   pArea->fRecordChanged = TRUE;
   return HB_SUCCESS;
}

/*
 * Replace the current record.
 */
static HB_ERRCODE hb_dbfPutRec( DBFAREAP pArea, const BYTE * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutRec(%p, %p)", pArea, pBuffer ) );

   if( pBuffer != NULL )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
            return HB_FAILURE;
      }

      if( ! pArea->fPositioned )
         return HB_SUCCESS;

      if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;

      /* Copy data to buffer */
      HB_MEMCPY( pArea->pRecord, pBuffer, ( size_t ) pArea->uiRecordLen );

      /*
       * TODO: such operation should be forbidden
       * maybe it will be good to return HB_FAILURE when
       *    pArea->pRecord[ 0 ] != '*' && pArea->pRecord[ 0 ] != ' '
       */
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         if( ! pArea->fHasMemo )
            pArea->fEncrypted = TRUE;
         pArea->pRecord[ 0 ] = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
      }

      pArea->fDeleted = pArea->pRecord[ 0 ] == '*';
   }
   else /* if( pArea->fRecordChanged ) */
   {
      BYTE *   pRecord = pArea->pRecord;
      USHORT   uiWritten;

      if( pArea->pCryptKey )
      {
         /* This enables record encryption in update operation */
         if( pArea->bCryptType == DB_CRYPT_SIX && ! pArea->fHasMemo )
            pArea->fEncrypted = TRUE;

         if( pArea->bCryptType == DB_CRYPT_SIX && pArea->fEncrypted )
         {
            pRecord        = ( BYTE * ) hb_xgrab( pArea->uiRecordLen );
            pRecord[ 0 ]   = pArea->fDeleted ? 'D' : 'E';
            hb_sxEnCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }

      /* Write data to file */
      uiWritten = ( USHORT ) hb_fileNetWriteAt( pArea->pDataFile, pRecord, pArea->uiRecordLen,
                                                ( HB_FOFFSET ) pArea->uiHeaderLen +
                                                ( HB_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                                                ( HB_FOFFSET ) pArea->uiRecordLen );
      if( pRecord != pArea->pRecord )
         hb_xfree( pRecord );

      if( uiWritten != pArea->uiRecordLen )
      {
         hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        hb_fsError(), 0, NULL );
         return HB_FAILURE;
      }
   }
   return HB_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static HB_ERRCODE hb_dbfPutValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   USHORT   uiSize;
   LPFIELD  pField;
   /* this buffer is for date and number conversion,
    * DBASE documentation defines maximum numeric field size as 20
    * but Clipper allows to create longer fields so I removed this
    * limit [druzus]
    */
   char        szBuffer[ 256 ];
   PHB_ITEM    pError;
   HB_ERRCODE  errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutValue(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PUT, uiIndex, pItem ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   errCode  = HB_SUCCESS;
   uiIndex--;
   pField   = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_MEMO ||
       pField->uiType == HB_FT_BINARY ||
       pField->uiType == HB_FT_PICTURE ||
       pField->uiType == HB_FT_BLOB ||
       pField->uiType == HB_FT_OLE )
      errCode = EDBF_DATATYPE;
   else
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         if( pField->uiType == HB_FT_STRING )
         {
            uiSize = ( USHORT ) hb_itemGetCLen( pItem );
            if( uiSize > pField->uiLen )
               uiSize = pField->uiLen;
            HB_MEMCPY( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       hb_itemGetCPtr( pItem ), uiSize );
#ifndef HB_CDP_SUPPORT_OFF
            if( ( pField->uiFlags & HB_FF_BINARY ) == 0 )
               hb_cdpnTranslate( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ], hb_cdppage(), pArea->area.cdPage, uiSize );
#endif
            memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + uiSize,
                    ' ', ( size_t ) ( pField->uiLen - uiSize ) );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      /* Must precede HB_IS_NUMERIC() because a DATE is also a NUMERIC. (xHarbour) */
      else if( HB_IS_DATE( pItem ) )
      {
         if( pField->uiType == HB_FT_DATE )
         {
            if( pField->uiLen == 3 )
            {
               HB_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else if( pField->uiLen == 4 )
            {
               HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 hb_itemGetDL( pItem ) );
            }
            else
            {
               hb_itemGetDS( pItem, szBuffer );
               HB_MEMCPY( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
            }
         }
         else if( pField->uiType == HB_FT_DATETIME ||
                  pField->uiType == HB_FT_TIMESTAMP )
         {
            HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                              hb_itemGetDL( pItem ) );
            HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + 4,
                              hb_itemGetT( pItem ) );
         }
         else if( pField->uiType == HB_FT_ANY && pField->uiLen == 3 )
         {
            hb_sxDtoP( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       hb_itemGetDL( pItem ) );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( HB_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == HB_FT_LONG || pField->uiType == HB_FT_FLOAT )
         {
            if( hb_itemStrBuf( szBuffer, pItem, pField->uiLen, pField->uiDec ) )
            {
               HB_MEMCPY( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          szBuffer, pField->uiLen );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       '*', pField->uiLen );
            }
         }
         else if( pField->uiType == HB_FT_INTEGER )
         {
            HB_LONG  lVal;
            double   dVal;
            int      iSize;

            if( pField->uiDec )
            {
               dVal  = hb_numDecConv( hb_itemGetND( pItem ), -( int ) pField->uiDec );
               lVal  = ( HB_LONG ) dVal;
               if( ! HB_DBL_LIM_INT64( dVal ) )
                  iSize = 99;
               else
#ifndef HB_LONG_LONG_OFF
                  iSize = HB_LIM_INT8( lVal ) ? 1 :
                          ( HB_LIM_INT16( lVal ) ? 2 :
                            ( HB_LIM_INT24( lVal ) ? 3 :
                            ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = HB_DBL_LIM_INT8( dVal ) ? 1 :
                          ( HB_DBL_LIM_INT16( dVal ) ? 2 :
                            ( HB_DBL_LIM_INT24( dVal ) ? 3 :
                            ( HB_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else if( HB_IS_DOUBLE( pItem ) )
            {
               dVal  = hb_itemGetND( pItem );
               lVal  = ( HB_LONG ) dVal;
               if( ! HB_DBL_LIM_INT64( dVal ) )
                  iSize = 99;
               else
#ifndef HB_LONG_LONG_OFF
                  iSize = HB_LIM_INT8( lVal ) ? 1 :
                          ( HB_LIM_INT16( lVal ) ? 2 :
                            ( HB_LIM_INT24( lVal ) ? 3 :
                            ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = HB_DBL_LIM_INT8( dVal ) ? 1 :
                          ( HB_DBL_LIM_INT16( dVal ) ? 2 :
                            ( HB_DBL_LIM_INT24( dVal ) ? 3 :
                            ( HB_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else
            {
               lVal  = ( HB_LONG ) hb_itemGetNInt( pItem );
#ifdef HB_LONG_LONG_OFF
               dVal  = ( double ) lVal;
#endif
               iSize = HB_LIM_INT8( lVal ) ? 1 :
                       ( HB_LIM_INT16( lVal ) ? 2 :
                         ( HB_LIM_INT24( lVal ) ? 3 :
                         ( HB_LIM_INT32( lVal ) ? 4 : 8 ) ) );
            }

            if( iSize > pField->uiLen )
            {
               errCode = EDBF_DATAWIDTH;
            }
            else
            {
               switch( pField->uiLen )
               {
                  case 1:
                     pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = ( signed char ) lVal;
                     break;
                  case 2:
                     HB_PUT_LE_UINT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( UINT16 ) lVal );
                     break;
                  case 3:
                     HB_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( UINT32 ) lVal );
                     break;
                  case 4:
                     HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( UINT32 ) lVal );
                     break;
                  case 8:
#ifndef HB_LONG_LONG_OFF
                     HB_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( UINT64 ) lVal );
#else
                     HB_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], dVal );
#endif
                     break;
                  default:
                     errCode = EDBF_DATATYPE;
                     break;
               }
            }
         }
         else if( pField->uiType == HB_FT_DOUBLE )
         {
            HB_PUT_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], hb_itemGetND( pItem ) );
         }
         else if( pField->uiType == HB_FT_ANY && pField->uiLen == 4 )
         {
            HB_LONG lVal = hb_itemGetNInt( pItem );
            if( HB_IS_DOUBLE( pItem ) ?
                HB_DBL_LIM_INT32( hb_itemGetND( pItem ) ) :
                HB_LIM_INT32( lVal ) )
            {
               HB_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( UINT32 ) lVal );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
            }
         }
         else
         {
            errCode = EDBF_DATATYPE;
         }
      }
      else if( HB_IS_LOGICAL( pItem ) )
      {
         if( pField->uiType == HB_FT_LOGICAL )
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = hb_itemGetL( pItem ) ? 'T' : 'F';
         else
            errCode = EDBF_DATATYPE;
      }
      else
         errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, hb_dbfGetEGcode( errCode ) );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( hb_dbfGetEGcode( errCode ) ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, errCode );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return errCode == E_DEFAULT ? HB_SUCCESS : HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Undelete the current record.
 */
static HB_ERRCODE hb_dbfRecall( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecall(%p)", pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_RECALL, 0, NULL ) )
         return HB_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->pRecord[ 0 ]  = ' ';
   pArea->fDeleted      = FALSE;
   return HB_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static HB_ERRCODE hb_dbfRecCount( DBFAREAP pArea, ULONG * pRecCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecCount(%p, %p)", pArea, pRecCount ) );

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );

   *pRecCount = pArea->ulRecCount;
   return HB_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static HB_ERRCODE hb_dbfRecNo( DBFAREAP pArea, ULONG * pulRecNo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecNo(%p, %p)", pArea, pulRecNo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   *pulRecNo = pArea->ulRecNo;
   return HB_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static HB_ERRCODE hb_dbfRecId( DBFAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE  errCode;
   ULONG       ulRecNo;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecId(%p, %p)", pArea, pRecNo ) );

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );

#ifdef HB_C52_STRICT
   /* this is for strict Clipper compatibility but IMHO Clipper should not
      do that and always set fixed size independent to the record number */
   if( ulRecNo < 10000000 )
   {
      hb_itemPutNLLen( pRecNo, ulRecNo, 7 );
   }
   else
   {
      hb_itemPutNLLen( pRecNo, ulRecNo, 10 );
   }
#else
   hb_itemPutNInt( pRecNo, ulRecNo );
#endif
   return errCode;
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_dbfSetFieldExtent( DBFAREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSetFieldExtent(%p, %hu)", pArea, uiFieldExtent ) );

   if( SUPER_SETFIELDEXTENT( ( AREAP ) pArea, uiFieldExtent ) == HB_FAILURE )
      return HB_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
   {
      pArea->pFieldOffset = ( USHORT * ) hb_xgrab( uiFieldExtent * sizeof( USHORT ) );
      memset( pArea->pFieldOffset, 0, uiFieldExtent * sizeof( USHORT ) );
   }

   return HB_SUCCESS;
}

#define hb_dbfAlias NULL

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_dbfClose( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfClose(%p)", pArea ) );

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PRECLOSE, 0, NULL ) )
         return HB_FAILURE;
   }

   /* Reset parent rel struct */
   pArea->lpdbPendingRel = NULL;

   /* Update record and unlock records */
   if( pArea->pDataFile )
   {
      /* update buffers */
      SELF_GOCOLD( ( AREAP ) pArea );

      /* Unlock all records */
      SELF_UNLOCK( ( AREAP ) pArea, NULL );

      /* Update header */
      if( pArea->fUpdateHeader )
         SELF_WRITEDBHEADER( ( AREAP ) pArea );

      /* It's not Clipper compatible but it reduces the problem with
         buggy Windows network setting */
      if( hb_setGetHardCommit() )
         SELF_FLUSH( ( AREAP ) pArea );
   }

   SUPER_CLOSE( ( AREAP ) pArea );

   if( pArea->pDataFile )
   {
      USHORT uiRemote = hb_fileNetRemote( pArea->pDataFile );
      hb_fileNetClose( pArea->pDataFile );
      pArea->pDataFile = NULL;

      if( pArea->fTemporary )
         hb_fileNetDelete( pArea->szDataFileName, uiRemote );
   }

   /* Close the memo file */
   if( pArea->fHasMemo && pArea->pMemoFile )
   {
      USHORT uiRemote = hb_fileNetRemote( pArea->pMemoFile );
      hb_fileNetClose( pArea->pMemoFile );
      pArea->pMemoFile = NULL;

      if( pArea->fTemporary )
         hb_fileNetDelete( pArea->szMemoFileName, uiRemote );
   }

   pArea->fTemporary = FALSE;

   /* Free field offset array */
   if( pArea->pFieldOffset )
   {
      hb_xfree( pArea->pFieldOffset );
      pArea->pFieldOffset = NULL;
   }

   /* Free buffer */
   if( pArea->pRecord )
   {
      hb_xfree( pArea->pRecord );
      pArea->pRecord = NULL;
   }

   /* Free encryption password key */
   if( pArea->pCryptKey )
   {
      memset( pArea->pCryptKey, '\0', 8 );
      hb_xfree( pArea->pCryptKey );
      pArea->pCryptKey = NULL;
   }

   /* Free all filenames */
   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }
   if( pArea->szMemoFileName )
   {
      hb_xfree( pArea->szMemoFileName );
      pArea->szMemoFileName = NULL;
   }

   if( pArea->fTrigger )
   {
      hb_dbfTriggerDo( pArea, EVENT_POSTCLOSE, 0, NULL );
      pArea->fTrigger = FALSE;
   }

   return HB_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfCreate( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   HB_ERRCODE  errCode = HB_SUCCESS;
   ULONG       ulSize;
   USHORT      uiCount;
   BOOL        fError, fRawBlob;
   DBFFIELD *  pThisField;
   BYTE *      pBuffer;
   PHB_FNAME   pFileName;
   PHB_ITEM    pItem = NULL, pError;
   char        szFileName[ HB_PATH_MAX ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCreate(%p, %p)", pArea, pCreateInfo ) );

   pArea->lpdbOpenInfo = pCreateInfo;

   if( ! pArea->fTemporary )
   {
      pFileName = hb_fsFNameSplit( pCreateInfo->abName );

      if( ! pFileName->szExtension && hb_setGetDefExtension() )
      {
         pItem = hb_itemPutC( pItem, NULL );
         if( SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pItem ) != HB_SUCCESS )
         {
            hb_itemRelease( pItem );
            hb_xfree( pFileName );
            pArea->lpdbOpenInfo = NULL;
            return HB_FAILURE;
         }
         pFileName->szExtension = hb_itemGetCPtr( pItem );
         hb_fsFNameMerge( szFileName, pFileName );
      }
      else
      {
         hb_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
      }
      hb_xfree( pFileName );
   }

   pItem    = hb_itemPutL( pItem, FALSE );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pCreateInfo->ulConnection, pItem ) == HB_SUCCESS &&
              hb_itemGetL( pItem );

   if( pArea->bTableType == 0 )
   {
      pItem = hb_itemPutNI( pItem, 0 );
      if( SELF_INFO( ( AREAP ) pArea, DBI_TABLETYPE, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bTableType = ( BYTE ) hb_itemGetNI( pItem );
   }

   if( pArea->bLockType == 0 )
   {
      pItem = hb_itemPutNI( pItem, 0 );
      if( SELF_INFO( ( AREAP ) pArea, DBI_LOCKSCHEME, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bLockType = ( BYTE ) hb_itemGetNI( pItem );
      if( pArea->bLockType == 0 )
      {
         pArea->bLockType = DB_DBFLOCK_CLIP;
      }
   }

   if( pArea->bTableType == DB_DBF_VFP && ! fRawBlob )
   {
      pArea->bMemoType = DB_MEMO_FPT;
   }
   else if( pArea->bTableType == DB_DBF_IV )
   {
      pArea->bMemoType = DB_MEMO_DBT;
   }
   else if( pArea->bMemoType == 0 )
   {
      /* get memo type */
      pItem = hb_itemPutNI( pItem, 0 );
      if( SELF_INFO( ( AREAP ) pArea, DBI_MEMOTYPE, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bMemoType = ( BYTE ) hb_itemGetNI( pItem );
   }

   pArea->bCryptType = DB_CRYPT_NONE;

   if( pItem )
      hb_itemRelease( pItem );

   if( pArea->area.uiFieldCount * sizeof( DBFFIELD ) + sizeof( DBFHEADER ) +
       ( pArea->bTableType == DB_DBF_VFP ? 1 : 2 ) > UINT16_MAX )
   {
      hb_dbfErrorRT( pArea, EG_CREATE, EDBF_DATAWIDTH, pCreateInfo->abName, 0, 0, NULL );
      pArea->lpdbOpenInfo = NULL;
      return HB_FAILURE;
   }

   if( ! fRawBlob )
   {
      pError = NULL;
      /* Try create */
      do
      {
         if( pArea->fTemporary )
            pArea->pDataFile = hb_fileNetCreateTempEx( szFileName, NULL, NULL, NULL, FC_TEMPORARY );
         else
            pArea->pDataFile = hb_fileNetExtOpen( szFileName, NULL,
                                                  FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                                  FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                                  NULL, pError, TRUE );
         if( pArea->pDataFile )
            break;
      }
      while( hb_dbfErrorRT( pArea, EG_CREATE, EDBF_CREATE_DBF, szFileName, hb_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         hb_itemRelease( pError );

      if( ! pArea->pDataFile )
      {
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
   }

   pArea->szDataFileName   = hb_strdup( szFileName );

   ulSize                  = ( ULONG ) pArea->area.uiFieldCount * sizeof( DBFFIELD ) +
                             ( pArea->bTableType == DB_DBF_VFP ? 1 : 2 );
   pBuffer                 = ( BYTE * ) hb_xgrab( ulSize + sizeof( DBFFIELD ) + 1 );
   memset( pBuffer, 0, ulSize + sizeof( DBFFIELD ) + 1 );
   pThisField              = ( DBFFIELD * ) pBuffer;

   pArea->fHasMemo         = fError = FALSE;

   /* Size for deleted flag */
   pArea->uiRecordLen      = 1;

#ifdef HB_COMPAT_FOXPRO
   /* initialize flag count */
   pArea->area.bFlagCount = 0;
#endif

   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      LPFIELD pField = pArea->area.lpFields + uiCount;
      hb_strncpy( ( char * ) pThisField->bName,
                  hb_dynsymName( ( PHB_DYNS ) pField->sym ), sizeof( pThisField->bName ) - 1 );
      pArea->pFieldOffset[ uiCount ] = pArea->uiRecordLen;
      /* field offset */
      if( pArea->bTableType == DB_DBF_VFP )
         HB_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );

#ifdef HB_COMPAT_FOXPRO
      /* field flags */
      pThisField->bFieldFlags = ( BYTE ) pField->uiFlags;
#endif

      switch( pField->uiType )
      {
         case HB_FT_STRING:
            pThisField->bType    = 'C';
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) ( pField->uiLen >> 8 );
            pArea->uiRecordLen   += pField->uiLen;
            break;

         /* system fields */
         case HB_FT_NONE:
            pThisField->bType       = '0';
            pThisField->bLen        = ( BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_HIDDEN;
            pArea->uiRecordLen      += pField->uiLen;
            break;

         case HB_FT_LOGICAL:
            pThisField->bType = 'L';
            pThisField->bLen  = 1;
            pArea->uiRecordLen++;
            break;

         case HB_FT_MEMO:
            pThisField->bType    = 'M';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pArea->uiRecordLen   += pField->uiLen;
            pArea->fHasMemo      = TRUE;
            break;

         case HB_FT_BINARY:
            pThisField->bType    = 'B';
            pField->uiLen        = 10;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pArea->uiRecordLen   += pField->uiLen;
            pArea->fHasMemo      = TRUE;
            break;

         case HB_FT_BLOB:
            pThisField->bType       = 'W';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen        = ( BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen      += pField->uiLen;
            pArea->fHasMemo         = TRUE;
            break;
#ifdef HB_COMPAT_FOXPRO
         case HB_FT_PICTURE:
            pThisField->bType       = 'P';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen        = ( BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen      += pField->uiLen;
            pArea->fHasMemo         = TRUE;
            break;

         case HB_FT_OLE:
            pThisField->bType       = 'G';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen        = ( BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen      += pField->uiLen;
            pArea->fHasMemo         = TRUE;
            break;
#endif
         case HB_FT_ANY:
            pThisField->bType = 'V';
            if( pField->uiLen < 3 || pField->uiLen == 5 )
            {
               pField->uiLen = 6;
            }
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) ( pField->uiLen >> 8 );
            pArea->uiRecordLen   += pField->uiLen;
            if( pThisField->bLen >= 6 )
            {
               pArea->uiMemoVersion = DB_MEMOVER_SIX;
               pArea->fHasMemo      = TRUE;
            }
            /*
               if( pArea->bTableType == DB_DBF_VFP )
               fError = TRUE;
             */
            break;

         case HB_FT_DATE:
            pThisField->bType = 'D';
            if( pField->uiLen != 3 && pField->uiLen != 4 )
            {
               pField->uiLen = pThisField->bLen = 8;
            }
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_LONG:
            pThisField->bType    = 'N';
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) pField->uiDec;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_FLOAT:
            pThisField->bType    = 'F';
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) pField->uiDec;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_DOUBLE:
         case HB_FT_CURDOUBLE:
            pThisField->bType    = pArea->bTableType == DB_DBF_IV ? 'O' : 'B';
            pField->uiLen        = 8;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) pField->uiDec;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_INTEGER:
         case HB_FT_CURRENCY:
            pThisField->bType = ( pArea->bTableType == DB_DBF_VFP &&
                                  pField->uiLen == 8 && pField->uiDec == 4 ) ?
                                'Y' : 'I';
            if( ( pField->uiLen > 4 && pField->uiLen != 8 ) ||
                pField->uiLen == 0 )
            {
               pField->uiLen = 4;
            }
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) pField->uiDec;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_TIME:
         case HB_FT_DATETIME:
            pThisField->bType = 'T';
            if( pField->uiLen != 4 ) // Support HB_FT_TIME
            {
               pField->uiLen           = 8;
               pThisField->bFieldFlags |= HB_FF_BINARY;
            }
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pArea->uiRecordLen   += pField->uiLen;
            break;

         case HB_FT_TIMESTAMP:
            pThisField->bType       = '@';
            pField->uiLen           = 8;
            pThisField->bLen        = ( BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= HB_FF_BINARY;
            pArea->uiRecordLen      += pField->uiLen;
            pArea->fModStamp        = TRUE;
            break;

         case HB_FT_MODTIME:
            pThisField->bType    = '=';
            pField->uiLen        = 8;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pArea->uiRecordLen   += pField->uiLen;
            pArea->fModStamp     = TRUE;
            break;

         case HB_FT_ROWVER:
            pThisField->bType    = '^';
            pField->uiLen        = 8;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            HB_PUT_LE_UINT32( pThisField->bCounter, 1 );
            pThisField->bStep    = 1;
            pArea->uiRecordLen   += pField->uiLen;
            pArea->fModStamp     = TRUE;
            break;

         case HB_FT_AUTOINC:
            pThisField->bType    = '+';
            pField->uiLen        = 4;
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            HB_PUT_LE_UINT32( pThisField->bCounter, 1 );
            pThisField->bStep    = 1;
            pArea->uiRecordLen   += pField->uiLen;
            pArea->fAutoInc      = TRUE;
            break;
#ifdef HB_COMPAT_FOXPRO
         case HB_FT_VARLENGTH:
            pThisField->bType    = 'Q';
            pThisField->bLen     = ( BYTE ) pField->uiLen;
            pThisField->bDec     = ( BYTE ) ( pField->uiLen >> 8 );
            pArea->uiRecordLen   += pField->uiLen;
            break;
#endif
         default:
            fError = TRUE;
      }

#ifdef HB_COMPAT_FOXPRO
      if( pThisField->bFieldFlags & HB_FF_AUTOINC )
      {
         pArea->fAutoInc   = TRUE;
         pThisField->bStep = 1;
      }
      else if( pThisField->bFieldFlags & HB_FF_HIDDEN )
         pArea->area.uiFieldHidden++;

      if( pArea->bTableType == DB_DBF_VFP )
      {
         if( pThisField->bFieldFlags & HB_FF_NULLABLE )
            pField->bNullPos = ( pArea->area.bFlagCount++ );
         if( pField->uiType == HB_FT_VARLENGTH )
            pField->bVarPos = ( pArea->area.bFlagCount++ );
      }
#endif

      if( fError )
      {
         hb_xfree( pBuffer );
         SELF_CLOSE( ( AREAP ) pArea );
         hb_dbfErrorRT( pArea, EG_CREATE, fError ? EDBF_DATATYPE : EDBF_DATAWIDTH,
                        pCreateInfo->abName, 0, 0, NULL );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pThisField++;
   }

#ifdef HB_COMPAT_FOXPRO
   // Adding field _NullFlags automatically
   if( pArea->area.bFlagCount )
   {
      hb_strncpy( ( char * ) pThisField->bName, "_NullFlags", sizeof( pThisField->bName ) - 1 );
      pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;

      /* field offset */
      if( pArea->bTableType == DB_DBF_VFP )
         HB_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );

      /* field flags */
      pThisField->bFieldFlags = HB_FF_HIDDEN;

      pThisField->bType       = '0';
      uiCount                 = ( pArea->area.bFlagCount + 7 ) >> 3;
      pThisField->bLen        = ( BYTE ) uiCount;
      pThisField->bDec        = ( BYTE ) ( uiCount >> 8 );
      pArea->uiRecordLen      += uiCount;
   }
#endif

   pArea->fShared       = FALSE; /* pCreateInfo->fShared */
   pArea->fReadonly     = FALSE; /* pCreateInfo->fReadonly */
   pArea->ulRecCount    = 0;
   pArea->uiHeaderLen   = ( USHORT ) ( sizeof( DBFHEADER ) + ulSize );
   if( fRawBlob )
   {
      pArea->fHasMemo = TRUE;
   }
   if( ! pArea->fHasMemo )
   {
      pArea->bMemoType = DB_MEMO_NONE;
   }
   pArea->ulMemoBlockSize = 0;

#ifndef HB_CDP_SUPPORT_OFF
   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFind( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = hb_cdppage();
   }
   else
      pArea->area.cdPage = hb_cdppage();
#endif

   pItem = hb_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pCreateInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      if( hb_dbfPasswordSet( pArea, pItem, FALSE ) )
         pArea->fTableEncrypted = TRUE;
   }
   else
   {
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pCreateInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         if( hb_dbfPasswordSet( pArea, pItem, FALSE ) )
            pArea->fTableEncrypted = TRUE;
      }
   }
   hb_itemRelease( pItem );

   if( ! fRawBlob )
   {
      /* Force write new header */
      pArea->fUpdateHeader = TRUE;
      /* Write header */
      errCode              = SELF_WRITEDBHEADER( ( AREAP ) pArea );
      if( errCode != HB_SUCCESS )
      {
         hb_xfree( pBuffer );
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Write fields and eof mark */
      if( pArea->bTableType == DB_DBF_VFP )
         pBuffer[ ulSize - 1 ] = '\r';
      else
      {
         pBuffer[ ulSize - 2 ]   = '\r';
         pBuffer[ ulSize - 1 ]   = '\0';
      }
      pBuffer[ ulSize ] = '\032';
      if( hb_fileNetWriteAt( pArea->pDataFile, pBuffer, ulSize + 1,
                             sizeof( DBFHEADER ) ) != ulSize + 1 )
      {
         hb_xfree( pBuffer );
         hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        hb_fsError(), 0, NULL );
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->fDataFlush = TRUE;
   }
   hb_xfree( pBuffer );

   /* Create memo file */
   if( pArea->fHasMemo )
   {
      pFileName               = hb_fsFNameSplit( szFileName );
      pFileName->szExtension  = NULL;
      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );
      pCreateInfo->abName     = szFileName;
      errCode                 = SELF_CREATEMEMFILE( ( AREAP ) pArea, pCreateInfo );
   }
   /* If successful call SUPER_CREATE to finish system jobs */
   if( errCode == HB_SUCCESS )
      errCode = SUPER_CREATE( ( AREAP ) pArea, pCreateInfo );

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   /* Alloc buffer */
   pArea->pRecord       = ( BYTE * ) hb_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer  = FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount    = hb_dbfCalcRecCount( pArea );
   pArea->lpdbOpenInfo  = NULL;

   /* Position cursor at the first record */
   return SELF_GOTOP( ( AREAP ) pArea );
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_dbfInfo( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfInfo(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_GETHEADERSIZE:
         hb_itemPutNL( pItem, pArea->uiHeaderLen );
         break;

      case DBI_LASTUPDATE:
         hb_itemPutD( pItem, 1900 + pArea->dbfHeader.bYear,
                      pArea->dbfHeader.bMonth,
                      pArea->dbfHeader.bDay );
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETLOCKARRAY:
         hb_dbfGetLockArray( pArea, pItem );
         break;

      case DBI_TABLEEXT:
         hb_itemClear( pItem );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLEEXT, 0, pItem );

      case DBI_FULLPATH:
         hb_itemPutC( pItem, pArea->szDataFileName );
         break;

      case DBI_MEMOTYPE:
         hb_itemPutNI( pItem, DB_MEMO_NONE );
         break;

      case DBI_TABLETYPE:
         if( ! pArea->pDataFile )
         {
            hb_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem );
         }
         hb_itemPutNI( pItem, pArea->bTableType );
         break;

      case DBI_FILEHANDLE:
         hb_itemPutNInt( pItem, pArea->pDataFile ?
                         ( HB_NHANDLE ) hb_fileHandle( pArea->pDataFile ) : FS_ERROR );
         break;

      case DBI_MEMOHANDLE:
         hb_itemPutNInt( pItem, pArea->pMemoFile ?
                         ( HB_NHANDLE ) hb_fileHandle( pArea->pMemoFile ) : FS_ERROR );
         break;

      case DBI_SHARED:
      {
         BOOL fShared = pArea->fShared;

         if( HB_IS_LOGICAL( pItem ) )
         {
            pArea->fShared = hb_itemGetL( pItem );
         }
         hb_itemPutL( pItem, fShared );
         break;
      }

      case DBI_TTS_INCOMPLETE:
      {
         BOOL bTransaction = pArea->dbfHeader.bTransaction;
         if( HB_IS_LOGICAL( pItem ) )
            hb_dbfTableTransaction( pArea, hb_itemGetL( pItem ) );

         hb_itemPutL( pItem, bTransaction );
         break;
      }

      case DBI_ISFLOCK:
         hb_itemPutL( pItem, pArea->fFLocked );
         break;

      case DBI_ISREADONLY:
         hb_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_ISTEMPORARY:
         if( ! pArea->pDataFile && ! pArea->pMemoFile && HB_IS_LOGICAL( pItem ) )
            pArea->fTemporary = hb_itemGetL( pItem );
         else
            hb_itemPutL( pItem, pArea->fTemporary );
         break;

      case DBI_VALIDBUFFER:
         hb_itemPutL( pItem, pArea->fValidBuffer );
         break;

      case DBI_POSITIONED:
         hb_itemPutL( pItem, pArea->fPositioned );
         break;

      case DBI_ISENCRYPTED:
         hb_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_DECRYPT:
         hb_dbfTableCrypt( pArea, pItem, FALSE );
         hb_itemPutL( pItem, ! pArea->fTableEncrypted );
         break;

      case DBI_ENCRYPT:
         hb_dbfTableCrypt( pArea, pItem, TRUE );
         hb_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_LOCKCOUNT:
         hb_itemPutNL( pItem, pArea->ulNumLocksPos );
         break;

      case DBI_LOCKOFFSET:
      {
         HB_FOFFSET  ulPos, ulFlSize, ulRlSize;
         int         iDir;

         hb_dbfLockData( pArea, &ulPos, &ulFlSize, &ulRlSize, &iDir );
         hb_itemPutNInt( pItem, ulPos );
         break;
      }

      case DBI_LOCKSCHEME:
      {
         int iScheme = hb_itemGetNI( pItem );
         if( pArea->bLockType )
         {
            hb_itemPutNI( pItem, pArea->bLockType );
         }
         else
         {
            hb_itemClear( pItem );
            errCode = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_LOCKSCHEME, 0, pItem );
         }
         switch( iScheme )
         {
            case DB_DBFLOCK_CLIP:
            case DB_DBFLOCK_CL53:
            case DB_DBFLOCK_CL53EXT:
            case DB_DBFLOCK_VFP:
#ifndef HB_LONG_LONG_OFF
            case DB_DBFLOCK_XHB64:
#endif
               pArea->bLockType = ( BYTE ) iScheme;
         }
         break;
      }
      case DBI_ROLLBACK:
         if( pArea->fRecordChanged )
         {
            if( pArea->fAppend )
            {
               hb_dbfSetBlankRecord( pArea, HB_BLANK_ROLLBACK );
               pArea->fDeleted = FALSE;
            }
            else
            {
               pArea->fRecordChanged = pArea->fValidBuffer = FALSE;
            }
         }
         break;

      case DBI_PASSWORD:
         hb_dbfPasswordSet( pArea, pItem, FALSE );
         break;

      case DBI_TRIGGER:
         if( HB_IS_LOGICAL( pItem ) )
            pArea->fTrigger = pArea->pTriggerSym && hb_itemGetL( pItem );
         else
         {
            PHB_DYNS pTriggerSym = pArea->pTriggerSym;
            if( HB_IS_STRING( pItem ) )
               hb_dbfTriggerSet( pArea, pItem );
            hb_itemPutC( pItem, pTriggerSym ? hb_dynsymName( pTriggerSym ) : NULL );
         }
         break;

      case DBI_OPENINFO:
         hb_itemPutPtr( pItem, pArea->lpdbOpenInfo );
         break;

      case DBI_DIRTYREAD:
      {
         BOOL fDirty = HB_DIRTYREAD( pArea );

         if( HB_IS_LOGICAL( pItem ) )
            pArea->uiDirtyRead = hb_itemGetL( pItem ) ?
                                 HB_IDXREAD_DIRTY : HB_IDXREAD_CLEAN;
         else if( ! HB_IS_NIL( pItem ) )
            pArea->uiDirtyRead = HB_IDXREAD_DEFAULT;

         hb_itemPutL( pItem, fDirty );
         break;
      }
      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char  szBuf[ 64 ];
         int   iSub = hb_itemGetNI( pItem );

         if( iSub == 1 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DBF" );
         else if( iSub == 2 )
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DBF", pArea->area.rddID );
/*
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, pArea->pRddNode->szName, pArea->area.rddID );
 */
         else
            hb_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         hb_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );

   }

   return errCode;
}

/*
 * Retrieve information about a raw
 */
static HB_ERRCODE hb_dbfRecInfo( DBFAREAP pArea, PHB_ITEM pRecID, USHORT uiInfoType, PHB_ITEM pInfo )
{
   ULONG       ulRecNo     = hb_itemGetNL( pRecID ), ulPrevRec = 0;
   HB_ERRCODE  errResult   = HB_SUCCESS;
   BOOL        bDeleted;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRecInfo(%p, %p, %hu, %p)", pArea, pRecID, uiInfoType, pInfo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( ulRecNo == 0 )
   {
      ulRecNo = pArea->ulRecNo;
   }
   else if( ulRecNo != pArea->ulRecNo )
   {
      switch( uiInfoType )
      {
         case DBRI_DELETED:
         case DBRI_ENCRYPTED:
         case DBRI_RAWRECORD:
         case DBRI_RAWMEMOS:
         case DBRI_RAWDATA:
            ulPrevRec   = pArea->ulRecNo;
            errResult   = SELF_GOTO( ( AREAP ) pArea, ulRecNo );
            if( errResult != HB_SUCCESS )
               return errResult;
            break;
      }
   }

   switch( uiInfoType )
   {
      case DBRI_DELETED:
         errResult = SELF_DELETED( ( AREAP ) pArea, &bDeleted );
         if( errResult == HB_SUCCESS )
            hb_itemPutL( pInfo, bDeleted );
         break;

      case DBRI_LOCKED:
         /* Clipper also checks only fShared and RLOCK and ignore FLOCK */
         hb_itemPutL( pInfo, ! pArea->fShared || /* pArea->fFLocked || */
                      hb_dbfIsLocked( pArea, ulRecNo ) );
         break;

      case DBRI_RECSIZE:
         hb_itemPutNL( pInfo, pArea->uiRecordLen );
         break;

      case DBRI_RECNO:
         hb_itemPutNInt( pInfo, ulRecNo );
         break;

      case DBRI_UPDATED:
         hb_itemPutL( pInfo, ulRecNo == pArea->ulRecNo && pArea->fRecordChanged );
         break;

      case DBRI_ENCRYPTED:
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
            errResult = HB_FAILURE;
         else
            hb_itemPutL( pInfo, pArea->fEncrypted );
         break;

      case DBRI_RAWRECORD:
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
            errResult = HB_FAILURE;
         else
            hb_itemPutCL( pInfo, ( char * ) pArea->pRecord, pArea->uiRecordLen );
         break;

      case DBRI_RAWMEMOS:
      case DBRI_RAWDATA:
      {
         USHORT   uiFields;
         BYTE *   pResult;
         ULONG    ulLength, ulLen;

         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
         {
            errResult = HB_FAILURE;
            break;
         }
         ulLength = uiInfoType == DBRI_RAWDATA ? pArea->uiRecordLen : 0;
         pResult  = ( BYTE * ) hb_xgrab( ulLength + 1 );
         if( ulLength )
         {
            HB_MEMCPY( pResult, pArea->pRecord, ( size_t ) ulLength );
         }

         if( pArea->fHasMemo )
         {
            for( uiFields = 0; uiFields < pArea->area.uiFieldCount; uiFields++ )
            {
               if( pArea->area.lpFields[ uiFields ].uiType == HB_FT_MEMO ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_BINARY ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_PICTURE ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_BLOB ||
                   pArea->area.lpFields[ uiFields ].uiType == HB_FT_OLE )
               {
                  errResult   = SELF_GETVALUE( ( AREAP ) pArea, uiFields + 1, pInfo );
                  if( errResult != HB_SUCCESS )
                     break;
                  ulLen       = ( ULONG ) hb_itemGetCLen( pInfo );
                  if( ulLen > 0 )
                  {
                     pResult  = ( BYTE * ) hb_xrealloc( pResult, ulLength + ulLen + 1 );
                     HB_MEMCPY( pResult + ulLength, hb_itemGetCPtr( pInfo ), ( size_t ) ulLen );
                     ulLength += ulLen;
                  }
               }
            }
         }
         hb_itemPutCPtr( pInfo, ( char * ) pResult, ulLength );
         break;
      }
      case DBRI_DBASELOCK:
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) && ! pArea->fShared )
         {
            errResult = HB_FAILURE;
            hb_itemPutNull( pInfo );
            break;
         }
         /*
            0-1  The first two bytes (binary integer) tell whether a user has changed the record. Every committed change
              is counted encreasing the count by one.
            2-4  The next three characters tell the time a user placed the lock. (10h 09h 07h i.e. 16:09:07)
            5-7  The next three characters tell the date a user placed the lock. ( 60h 09h 0Bh i.e. (19)96-09-11 )
            8-24 The remaining 16 characters are optional. They tell the name of the user that placed the lock.
          */
         if( pArea->uidbaselock )
         {
            BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ pArea->uidbaselock ];

            if( SELF_RAWLOCK( ( AREAP ) pArea, REC_LOCK_TEST, pArea->ulRecNo ) == HB_SUCCESS )
            {
               errResult = HB_FAILURE;
               hb_itemPutNull( pInfo );
               break;
            }

            switch( hb_itemGetNL( pInfo ) )
            {
               case 0: /* Time when lock was placed */
               {
                  char szTime[ 9 ];
                  pPtr += 2;
                  hb_snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d", ( int ) pPtr[ 0 ], ( int ) pPtr[ 1 ], ( int ) pPtr[ 2 ] );
                  hb_itemPutC( pInfo, szTime );
                  break;
               }
               case 1: /* Date when lock was placed */
                  pPtr += 5;
                  hb_itemPutDL( pInfo, hb_dateEncode( pPtr[ 0 ] + 1900, pPtr[ 1 ], pPtr[ 2 ] ) );
                  break;
               case 2: /* Login name of user who locked record or file */
                  if( pArea->area.lpFields[ pArea->uidbaselock ].uiLen >= 24 )
                  {
                     pPtr += 8;
                     hb_itemPutCPtr( pInfo, ( char * ) pPtr, 16 );
                  }
                  else
                     hb_itemPutC( pInfo, "" );
                  break;
               default:
                  errResult = HB_FAILURE;
            }
         }
         else
            hb_itemPutNull( pInfo );
         break;


      default:
         errResult = SUPER_RECINFO( ( AREAP ) pArea, pRecID, uiInfoType, pInfo );
   }
   if( ulPrevRec != 0 )
   {
      if( SELF_GOTO( ( AREAP ) pArea, ulPrevRec ) != HB_SUCCESS &&
          errResult == HB_SUCCESS )
         errResult = HB_FAILURE;
   }
   return errResult;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_dbfNewArea( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfNewArea(%p)", pArea ) );

   if( SUPER_NEW( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->pDataFile     = pArea->pMemoFile = pArea->pMemoTmpFile = NULL;
   pArea->fDataFlush    = pArea->fMemoFlush = FALSE;
   /* Index dirty read flag initialized to global RDD setting */
   pArea->uiDirtyRead   = HB_IDXREAD_DEFAULT;
   /* Size for deleted records flag */
   pArea->uiRecordLen   = 1;

   {
      PHB_ITEM pItem = hb_itemPutNI( NULL, 0 );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem ) == HB_SUCCESS )
         pArea->bTableType = ( BYTE ) hb_itemGetNI( pItem );
      hb_itemRelease( pItem );
   }

   return HB_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_dbfOpen( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_ERRCODE  errCode, errOsCode;
   USHORT      uiFlags, uiFields, uiCount, uiSkip;
   ULONG       ulSize;
   BOOL        fRawBlob;
   PHB_ITEM    pError, pItem;
   PHB_FNAME   pFileName;
   BYTE *      pBuffer;
   LPDBFFIELD  pField;
   DBFIELDINFO dbFieldInfo;
   char        szFileName[ HB_PATH_MAX ];
   char        szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfOpen(%p, %p)", pArea, pOpenInfo ) );

   pArea->lpdbOpenInfo  = pOpenInfo;

   pItem                = hb_itemNew( NULL );

   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGTRIGGER,
                     pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      if( HB_IS_STRING( pItem ) )
         hb_dbfTriggerSet( pArea, pItem );
   }

   if( ! pArea->fTrigger )
   {
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TRIGGER,
                        pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         if( HB_IS_STRING( pItem ) )
            hb_dbfTriggerSet( pArea, pItem );
      }
   }

   if( pArea->fTrigger )
   {
      hb_itemPutC( pItem, pOpenInfo->abName );
      if( ! hb_dbfTriggerDo( pArea, EVENT_PREUSE, 0, pItem ) )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      hb_strncpy( szFileName, hb_itemGetCPtr( pItem ), sizeof( szFileName ) - 1 );
   }
   else
      hb_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );

   if( ! pArea->bLockType )
   {
      hb_itemClear( pItem );
      if( SELF_INFO( ( AREAP ) pArea, DBI_LOCKSCHEME, pItem ) != HB_SUCCESS )
      {
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pArea->bLockType = ( BYTE ) hb_itemGetNI( pItem );
      if( ! pArea->bLockType )
         pArea->bLockType = DB_DBFLOCK_CLIP;
   }
#ifndef HB_CDP_SUPPORT_OFF
   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = hb_cdpFind( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = hb_cdppage();
   }
   else
      pArea->area.cdPage = hb_cdppage();
#endif
   pArea->fShared    = pOpenInfo->fShared;
   pArea->fReadonly  = pOpenInfo->fReadonly;
   /* Force exclusive mode
    *   0: AUTOSHARE disabled.
    *   1: AUTOSHARE enabled.
    *   2: force exclusive mode.
    * */
   if( hb_setGetAutoShare() == 2 )
      pArea->fShared = FALSE;
   uiFlags     = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
                 ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );
   pError      = NULL;

   pFileName   = hb_fsFNameSplit( szFileName );
   /* Add default file name extension if necessary */
   if( ! pFileName->szExtension && hb_setGetDefExtension() )
   {
      hb_itemClear( pItem );
      if( SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pItem ) != HB_SUCCESS )
      {
         hb_xfree( pFileName );
         hb_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }
      pFileName->szExtension = hb_itemGetCPtr( pItem );
      hb_fsFNameMerge( szFileName, pFileName );
   }

   /* Create default alias if necessary */
   if( ! pOpenInfo->atomAlias && pFileName->szName )
   {
      const char * szName = strrchr( pFileName->szName, ':' );
      if( szName == NULL )
         szName = pFileName->szName;
      else
         ++szName;
      hb_strncpyUpperTrim( szAlias, szName, sizeof( szAlias ) - 1 );
      pOpenInfo->atomAlias = szAlias;
   }
   hb_xfree( pFileName );

   hb_itemClear( pItem );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pOpenInfo->ulConnection, pItem ) == HB_SUCCESS &&
              hb_itemGetL( pItem );

   hb_itemRelease( pItem );

   if( fRawBlob )
   {
      uiFields          = uiSkip = 0;
      pBuffer           = NULL;
      pArea->fHasMemo   = TRUE;
   }
   else
   {
      /* Try open */
      do
      {
         pArea->pDataFile = hb_fileNetExtOpen( szFileName, NULL, uiFlags |
                                               FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                               NULL, pError, TRUE );
         if( pArea->pDataFile )
            break;
      }
      while( hb_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, szFileName, hb_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );

      if( pError )
      {
         hb_itemRelease( pError );
         pError = NULL;
      }

      /* Exit if error */
      if( ! pArea->pDataFile )
      {
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return HB_FAILURE;
      }

      /* Allocate only after succesfully open file */
      pArea->szDataFileName   = hb_strdup( szFileName );

      /* Read file header and exit if error */
      errCode                 = SELF_READDBHEADER( ( AREAP ) pArea );
      if( errCode != HB_SUCCESS )
      {
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Add fields */
      uiSkip   = 0;
      uiFields = ( pArea->uiHeaderLen - sizeof( DBFHEADER ) ) / sizeof( DBFFIELD );
      ulSize   = ( ULONG ) uiFields * sizeof( DBFFIELD );
      pBuffer  = uiFields ? ( BYTE * ) hb_xgrab( ulSize ) : NULL;

      /* Read fields and exit if error */
      do
      {
         if( hb_fileNetReadAt( pArea->pDataFile, pBuffer, ulSize,
                               sizeof( DBFHEADER ) ) == ulSize )
         {
            errCode = HB_SUCCESS;
            break;
         }
         errOsCode   = hb_fsError();
         errCode     = HB_FAILURE;
      }
      while( hb_dbfErrorRT( pArea, errOsCode == 0 ? EG_CORRUPTION : EG_READ,
                            errOsCode == 0 ? EDBF_CORRUPT : EDBF_READ,
                            pArea->szDataFileName, errOsCode,
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         hb_itemRelease( pError );

      /* Exit if error */
      if( errCode != HB_SUCCESS )
      {
         if( pBuffer )
            hb_xfree( pBuffer );
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* some RDDs use the additional space in the header after field arrray
         for private data we should check for 0x0D marker to not use this
         data as fields description */
      for( uiCount = 0; uiCount < uiFields; uiCount++ )
      {
         pField = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );
         if( pField->bName[ 0 ] == 0x0d )
         {
            uiFields = uiCount;
            break;
         }
#ifndef HB_COMPAT_FOXPRO
         else if( pArea->bTableType == DB_DBF_VFP &&
                  pField->bFieldFlags & 0x01 )
         {
            uiSkip++;
         }
#endif
      }
      uiFields -= uiSkip;
   }

   /* CL5.3 allow to create and open DBFs without fields */
#ifdef HB_C52_STRICT
   if( uiFields == 0 )
   {
      errCode = HB_FAILURE;
   }
   else
#endif
   {
      errCode = SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );
      if( errCode != HB_SUCCESS )
      {
         SELF_CLOSE( ( AREAP ) pArea );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }
   }

#ifdef HB_COMPAT_FOXPRO
   /* Initializing number of hidden fields*/
   pArea->area.uiFieldHidden = 0;
#endif

   /* Clear dbFieldInfo structure */
   memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;
   for( uiCount = 0; uiCount < uiFields + uiSkip; uiCount++ )
   {
      pField                     = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );
      pField->bName[ 10 ]        = '\0';
      /* hb_strUpper( ( char * ) pField->bName ); */
      dbFieldInfo.atomName       = ( const char * ) pField->bName;
      dbFieldInfo.uiLen          = pField->bLen;
      dbFieldInfo.uiDec          = 0;
      dbFieldInfo.uiTypeExtended = 0;
      if( pArea->bTableType == DB_DBF_VFP )
         dbFieldInfo.uiFlags = pField->bFieldFlags;
      else
         dbFieldInfo.uiFlags = 0;
      switch( pField->bType )
      {
         case 'C':
            dbFieldInfo.uiType   = HB_FT_STRING;
            dbFieldInfo.uiLen    = pField->bLen + pField->bDec * 256;
            break;

         case 'L':
            dbFieldInfo.uiType   = HB_FT_LOGICAL;
            dbFieldInfo.uiLen    = 1;
            break;

         case 'D':
            dbFieldInfo.uiType = HB_FT_DATE;
            if( dbFieldInfo.uiLen != 3 && dbFieldInfo.uiLen != 4 )
               dbFieldInfo.uiLen = 8;
            break;

         case 'I':
            dbFieldInfo.uiType = HB_FT_INTEGER;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 4;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case 'Y':
            dbFieldInfo.uiType = HB_FT_CURRENCY;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case '2':
         case '4':
            dbFieldInfo.uiType   = HB_FT_INTEGER;
            dbFieldInfo.uiLen    = pField->bType - '0';
            break;

         case 'N':
            dbFieldInfo.uiType   = HB_FT_LONG;
            dbFieldInfo.uiDec    = pField->bDec;
            /* DBASE documentation defines maximum numeric field size as 20
             * but Clipper allows to create longer fields so I removed this
             * limit, Druzus
             */
            /*
               if( pField->bLen > 20 )
               errCode = HB_FAILURE;
             */
            break;

         case 'F':
            dbFieldInfo.uiType   = HB_FT_FLOAT;
            dbFieldInfo.uiDec    = pField->bDec;
            /* See note above */
            break;

         case 'B':
            if( pArea->bTableType == DB_DBF_IV && dbFieldInfo.uiLen == 10 )
            {
               dbFieldInfo.uiType   = HB_FT_BINARY;
               pArea->fHasMemo      = TRUE;
               dbFieldInfo.uiFlags  |= HB_FF_BINARY;
               break;
            }
         /* fallthrough */
         case 'O': /* dBase IV double */
         case '8':
            dbFieldInfo.uiType   = HB_FT_DOUBLE;
            dbFieldInfo.uiDec    = pField->bDec;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            break;

         /* types which are not supported by VM - mapped to different ones */
         case 'T':
            if( dbFieldInfo.uiLen == 4 )
               dbFieldInfo.uiType = HB_FT_TIME;
            else if( dbFieldInfo.uiLen == 8 )
               dbFieldInfo.uiType = HB_FT_DATETIME;
            else
               errCode = HB_FAILURE;
            break;

         case '@':
            dbFieldInfo.uiType   = HB_FT_TIMESTAMP;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            pArea->fModStamp     = TRUE;
            break;

         case '=':
            dbFieldInfo.uiType   = HB_FT_MODTIME;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            pArea->fModStamp     = TRUE;
            break;

         case '^':
            dbFieldInfo.uiType   = HB_FT_ROWVER;
            if( dbFieldInfo.uiLen != 8 )
               errCode = HB_FAILURE;
            pArea->fModStamp     = TRUE;
            break;

         case '+':
            dbFieldInfo.uiType   = HB_FT_AUTOINC;
            if( dbFieldInfo.uiLen != 4 )
               errCode = HB_FAILURE;
            pArea->fAutoInc      = TRUE;
            break;

         case 'Q':
            dbFieldInfo.uiType   = HB_FT_VARLENGTH;
            dbFieldInfo.uiFlags  |= HB_FF_BINARY;
            break;

         case 'V':
            if( pArea->bTableType == DB_DBF_VFP )
            {
               dbFieldInfo.uiType = HB_FT_VARLENGTH;
            }
            else
            {
               dbFieldInfo.uiType = HB_FT_ANY;
               if( dbFieldInfo.uiLen >= 6 )
               {
                  pArea->uiMemoVersion = DB_MEMOVER_SIX;
                  pArea->fHasMemo      = TRUE;
               }
            }
            break;

         case 'M':
            dbFieldInfo.uiType   = HB_FT_MEMO;
            pArea->fHasMemo      = TRUE;
            break;

#ifdef HB_COMPAT_FOXPRO
         case 'P':
            dbFieldInfo.uiType   = HB_FT_PICTURE;
            dbFieldInfo.uiFlags  |= HB_FF_BINARY;
            pArea->fHasMemo      = TRUE;
            break;
#endif
         case 'W':
            dbFieldInfo.uiType   = HB_FT_BLOB;
            dbFieldInfo.uiFlags  |= HB_FF_BINARY;
            pArea->fHasMemo      = TRUE;
            break;
#ifdef HB_COMPAT_FOXPRO
         case 'G':
            dbFieldInfo.uiType   = HB_FT_OLE;
            dbFieldInfo.uiFlags  |= HB_FF_BINARY;
            pArea->fHasMemo      = TRUE;
            break;
#endif
         case '0':
#ifdef HB_COMPAT_FOXPRO
            /* NULLABLE and VARLENGTH support
               if( memcmp( dbFieldInfo.atomName, "_NullFlags", 10 ) == 0 )
               For each Varchar and Varbinary field, one bit, or "varlength" bit, is allocated
               in the last system field, which is a hidden field and stores the null status for
               all fields that can be null. If the Varchar or Varbinary field can be null, the
               null bit follows the "varlength" bit. If the "varlength" bit is set to 1, the length
               of the actual field value length is stored in the last byte of the field. Otherwise,
               if the bit is set to 0, length of the value is equal to the field size.
             */
            if( memcmp( dbFieldInfo.atomName, "_NullFlags", 10 ) == 0 )
            {
               dbFieldInfo.uiType         = HB_FT_NONE;
               dbFieldInfo.uiFlags        |= HB_FF_HIDDEN; /* To support it under all DBF formats */
               pArea->area.uNullFlagField = uiCount;
               break;
            }
			/* fallthrough */
#else
            if( pArea->bTableType == DB_DBF_VFP && pField->bFieldFlags & 0x01 )
            {
               if( memcmp( dbFieldInfo.atomName, "_NullFlags", 10 ) == 0 )
               {
                  /* TODO: NULLABLE and VARLENGTH support */
               }
               pArea->uiRecordLen += dbFieldInfo.uiLen;
               continue;
            }
			/* fallthrough */
#endif
            else if( memcmp( dbFieldInfo.atomName, "_DBASELOCK", 10 ) == 0 )
            {
               dbFieldInfo.uiType   = HB_FT_NONE;
               dbFieldInfo.uiFlags  |= HB_FF_HIDDEN; /* To support it under all DBF formats */
               pArea->uidbaselock   = uiCount;
               break;
            }
         /* fallthrough */
         default:
            errCode = HB_FAILURE;
            break;
      }

      /* Add field */
      if( errCode == HB_SUCCESS )
         errCode = SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo );

      /* Exit if error */
      if( errCode != HB_SUCCESS )
         break;
#ifdef HB_COMPAT_FOXPRO
      if( dbFieldInfo.uiFlags & HB_FF_AUTOINC )
      {
         pArea->fAutoInc      = TRUE;
         dbFieldInfo.uiStep   = pField->bStep;
      }
#endif
   }
   if( pBuffer )
      hb_xfree( pBuffer );

   /* Exit if error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, pArea->szDataFileName,
                     0, EF_CANDEFAULT, NULL );
      SELF_CLOSE( ( AREAP ) pArea );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   pItem = hb_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
   {
      hb_dbfPasswordSet( pArea, pItem, FALSE );
   }
   else
   {
      hb_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pOpenInfo->ulConnection, pItem ) == HB_SUCCESS )
      {
         hb_dbfPasswordSet( pArea, pItem, FALSE );
      }
   }
   hb_itemRelease( pItem );

   /* Open memo file if exists */
   if( pArea->fHasMemo )
   {
      pFileName               = hb_fsFNameSplit( szFileName );
      pFileName->szExtension  = NULL;
      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );
      pOpenInfo->abName       = szFileName;
      errCode                 = SELF_OPENMEMFILE( ( AREAP ) pArea, pOpenInfo );
   }

   if( errCode == HB_SUCCESS )
   {
      /* If successful call SUPER_OPEN to finish system jobs */
      errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );
   }

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      pArea->lpdbOpenInfo = NULL;
      return HB_FAILURE;
   }

   /* Alloc buffer */
   pArea->pRecord       = ( BYTE * ) hb_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer  = FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount    = hb_dbfCalcRecCount( pArea );

   /* Position cursor at the first record */
   errCode              = SELF_GOTOP( ( AREAP ) pArea );

   if( pArea->fTrigger )
      hb_dbfTriggerDo( pArea, EVENT_POSTUSE, 0, NULL );

   pArea->lpdbOpenInfo = NULL;

   return errCode;
}

#define hb_dbfRelease NULL

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_dbfStructSize( DBFAREAP pArea, USHORT * uiSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfStrucSize(%p, %p)", pArea, uiSize ) );
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( DBFAREA );
   return HB_SUCCESS;
}

#define hb_dbfSysName   NULL
#define hb_dbfEval      NULL

/*
 * Pack helper function called for each packed record
 */
static HB_ERRCODE hb_dbfPackRec( DBFAREAP pArea, ULONG ulRecNo, BOOL * fWritten )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPackRec(%p, %lu, %p)", pArea, ulRecNo, fWritten ) );

   HB_SYMBOL_UNUSED( ulRecNo );

   *fWritten = ! pArea->fDeleted;

   return HB_SUCCESS;
}

/*
 * Remove records marked for deletion from a database.
 */
static HB_ERRCODE hb_dbfPack( DBFAREAP pArea )
{
   ULONG    ulRecIn, ulRecOut, ulEvery, ulUserEvery;
   PHB_ITEM pBlock;
   BOOL     fWritten;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPack(%p)", pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      hb_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_PACK, 0, NULL ) )
         return HB_FAILURE;
   }

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   /* This is bad hack but looks that people begins to use it :-(
    * so I'll add workaround to make it m ore safe
    */
   if( pArea->area.valResult && HB_IS_ARRAY( pArea->area.valResult ) &&
       hb_arrayLen( pArea->area.valResult ) == 2 &&
       ( hb_arrayGetType( pArea->area.valResult, 1 ) & HB_IT_BLOCK ) != 0 &&
       ( hb_arrayGetType( pArea->area.valResult, 2 ) & HB_IT_NUMERIC ) != 0 )
   {
      pBlock = hb_itemNew( NULL );
      hb_arrayGet( pArea->area.valResult, 1, pBlock );
      if( hb_arrayGetND( pArea->area.valResult, 2 ) >= 1 )
         ulUserEvery = hb_arrayGetNL( pArea->area.valResult, 2 );
      else
         ulUserEvery = 1;
   }
   else
   {
      pBlock      = NULL;
      ulUserEvery = 0;
   }

   ulRecOut = ulEvery = 0;
   ulRecIn  = 1;
   while( ulRecIn <= pArea->ulRecCount )
   {
      if( SELF_GOTO( ( AREAP ) pArea, ulRecIn ) != HB_SUCCESS )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }
      if( ! hb_dbfReadRecord( pArea ) )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }

      /* Execute the Code Block */
      if( pBlock )
      {
         if( ++ulEvery >= ulUserEvery )
         {
            ulEvery = 0;
            if( SELF_EVALBLOCK( ( AREAP ) pArea, pBlock ) != HB_SUCCESS )
            {
               hb_itemRelease( pBlock );
               return HB_FAILURE;
            }
         }
      }

      if( SELF_PACKREC( ( AREAP ) pArea, ulRecOut + 1, &fWritten ) != HB_SUCCESS )
      {
         if( pBlock )
            hb_itemRelease( pBlock );
         return HB_FAILURE;
      }

      if( fWritten )
      {
         ulRecOut++;
         if( pArea->ulRecNo != ulRecOut || pArea->fRecordChanged )
         {
            pArea->ulRecNo          = ulRecOut;
            pArea->fRecordChanged   = TRUE;
            if( ! hb_dbfWriteRecord( pArea ) )
            {
               if( pBlock )
                  hb_itemRelease( pBlock );
               return HB_FAILURE;
            }
         }
      }
      ulRecIn++;
   }

   /* Execute the Code Block for pending record */
   if( pBlock )
   {
      if( ulEvery > 0 )
      {
         if( SELF_EVALBLOCK( ( AREAP ) pArea, pBlock ) != HB_SUCCESS )
         {
            hb_itemRelease( pBlock );
            return HB_FAILURE;
         }
      }
      hb_itemRelease( pBlock );
   }

   if( pArea->ulRecCount != ulRecOut )
   {
      pArea->ulRecCount    = ulRecOut;
      /* Force write new header */
      pArea->fUpdateHeader = TRUE;
      if( SELF_WRITEDBHEADER( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return SELF_GOTO( ( AREAP ) pArea, 1 );
}

/*
 * Physically reorder a database.
 */
static HB_ERRCODE hb_dbfSort( DBFAREAP pArea, LPDBSORTINFO pSortInfo )
{
   ULONG       ulRecNo;
   USHORT      uiCount;
   BOOL        bMoreRecords, bLimited, bValidRecord;
   HB_ERRCODE  errCode;
   DBQUICKSORT dbQuickSort;
   BYTE *      pBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSort(%p, %p)", pArea, pSortInfo ) );

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   if( ! hb_dbQSortInit( &dbQuickSort, pSortInfo, pArea->uiRecordLen ) )
      return HB_FAILURE;

   errCode  = HB_SUCCESS;
   uiCount  = 0;
   pBuffer  = dbQuickSort.pBuffer;
   ulRecNo  = 1;
   if( pSortInfo->dbtri.dbsci.itmRecID )
   {
      errCode        = SELF_GOTOID( ( AREAP ) pArea, pSortInfo->dbtri.dbsci.itmRecID );
      bMoreRecords   = bLimited = TRUE;
   }
   else if( pSortInfo->dbtri.dbsci.lNext )
   {
      ulRecNo        = hb_itemGetNL( pSortInfo->dbtri.dbsci.lNext );
      bLimited       = TRUE;
      bMoreRecords   = ( ulRecNo > 0 );
   }
   else
   {
      if( ! pSortInfo->dbtri.dbsci.itmCobWhile &&
          ( ! pSortInfo->dbtri.dbsci.fRest ||
            ! hb_itemGetL( pSortInfo->dbtri.dbsci.fRest ) ) )
         errCode = SELF_GOTOP( ( AREAP ) pArea );
      bMoreRecords   = TRUE;
      bLimited       = FALSE;
   }

   while( errCode == HB_SUCCESS && ! pArea->area.fEof && bMoreRecords )
   {
      if( pSortInfo->dbtri.dbsci.itmCobWhile )
      {
         if( SELF_EVALBLOCK( ( AREAP ) pArea, pSortInfo->dbtri.dbsci.itmCobWhile ) != HB_SUCCESS )
         {
            hb_dbQSortExit( &dbQuickSort );
            return HB_FAILURE;
         }
         bMoreRecords = hb_itemGetL( pArea->area.valResult );
      }

      if( bMoreRecords && pSortInfo->dbtri.dbsci.itmCobFor )
      {
         if( SELF_EVALBLOCK( ( AREAP ) pArea, pSortInfo->dbtri.dbsci.itmCobFor ) != HB_SUCCESS )
         {
            hb_dbQSortExit( &dbQuickSort );
            return HB_FAILURE;
         }
         bValidRecord = hb_itemGetL( pArea->area.valResult );
      }
      else
         bValidRecord = bMoreRecords;

      if( bValidRecord )
      {
         if( uiCount == dbQuickSort.uiMaxRecords )
         {
            if( ! hb_dbQSortAdvance( &dbQuickSort, uiCount ) )
            {
               hb_dbQSortExit( &dbQuickSort );
               return HB_FAILURE;
            }
            pBuffer  = dbQuickSort.pBuffer;
            uiCount  = 0;
         }

         /* Read record */
         if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
         {
            hb_dbQSortExit( &dbQuickSort );
            return HB_FAILURE;
         }

         /* Copy data */
         HB_MEMCPY( pBuffer, pArea->pRecord, pArea->uiRecordLen );
#ifndef HB_CDP_SUPPORT_OFF
         if( pArea->area.cdPage != hb_cdppage() )
         {
            hb_dbfTranslateRec( pArea, pBuffer, pArea->area.cdPage, hb_cdppage() );
         }
#endif
         pBuffer += pArea->uiRecordLen;
         uiCount++;
      }

      if( bMoreRecords && bLimited )
         bMoreRecords = ( --ulRecNo > 0 );
      if( bMoreRecords )
         errCode = SELF_SKIP( ( AREAP ) pArea, 1 );
   }

   /* Copy last records */
   if( uiCount > 0 )
   {
      if( ! hb_dbQSortAdvance( &dbQuickSort, uiCount ) )
      {
         hb_dbQSortExit( &dbQuickSort );
         return HB_FAILURE;
      }
   }

   /* Sort records */
   hb_dbQSortComplete( &dbQuickSort );
   return HB_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_dbfTrans( DBFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfTrans(%p, %p)", pArea, pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( pArea->fHasMemo || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
         pTransInfo->uiFlags &= ~DBTF_PUTREC;
      else if( pArea->area.rddID == pTransInfo->lpaDest->rddID )
         pTransInfo->uiFlags |= DBTF_PUTREC;
      else
      {
         PHB_ITEM pPutRec = hb_itemPutL( NULL, FALSE );
         if( SELF_INFO( ( AREAP ) pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec ) != HB_SUCCESS )
         {
            hb_itemRelease( pPutRec );
            return HB_FAILURE;
         }
         if( hb_itemGetL( pPutRec ) )
            pTransInfo->uiFlags |= DBTF_PUTREC;
         else
            pTransInfo->uiFlags &= ~DBTF_PUTREC;
         hb_itemRelease( pPutRec );
      }
   }
   return SUPER_TRANS( ( AREAP ) pArea, pTransInfo );
}

#define hb_dbfTransRec NULL

/*
 * Physically remove all records from data store.
 */
static HB_ERRCODE hb_dbfZap( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfZap(%p)", pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      hb_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! hb_dbfTriggerDo( pArea, EVENT_ZAP, 0, NULL ) )
         return HB_FAILURE;
   }

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->fUpdateHeader = TRUE;
   pArea->ulRecCount    = 0;

#ifdef HB_COMPAT_FOXPRO
   if( pArea->fAutoInc )
   {
      USHORT uiField;
      for( uiField = 0; uiField < pArea->area.uiFieldCount; uiField++ )
      {
         if( pArea->area.lpFields[ uiField ].uiFlags & HB_FF_AUTOINC ||
             pArea->area.lpFields[ uiField ].uiType == HB_FT_AUTOINC )
            hb_dbfSetNextValue( pArea, uiField, 0 );
      }
   }
#endif

   if( SELF_WRITEDBHEADER( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;
   if( SELF_GOTO( ( AREAP ) pArea, 0 ) != HB_SUCCESS )
      return HB_FAILURE;

   /* Zap memo file */
   if( pArea->fHasMemo )
   {
      if( SELF_CREATEMEMFILE( ( AREAP ) pArea, NULL ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Report end of relation.
 */
static HB_ERRCODE hb_dbfChildEnd( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildEnd(%p, %p)", pArea, pRelInfo ) );

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
   else
      errCode = HB_SUCCESS;
   SUPER_CHILDEND( ( AREAP ) pArea, pRelInfo );
   return errCode;
}

/*
 * Report initialization of a relation.
 */
static HB_ERRCODE hb_dbfChildStart( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildStart(%p, %p)", pArea, pRelInfo ) );

   if( SELF_CHILDSYNC( ( AREAP ) pArea, pRelInfo ) != HB_SUCCESS )
      return HB_FAILURE;
   return SUPER_CHILDSTART( ( AREAP ) pArea, pRelInfo );
}

/*
 * Post a pending relational movement.
 */
static HB_ERRCODE hb_dbfChildSync( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfChildSync(%p, %p)", pArea, pRelInfo ) );

   /*
    * !!! The side effect of calling GOCOLD() inside CHILDSYNC() is
    * evaluation of index expressions (index KEY and FOR condition)
    * when the pArea is not the current one - it means that the
    * used RDD has to set proper work area before eval.
    * IMHO GOCOLD() could be safely removed from this place but I'm not
    * sure it's Clipper compatible - I will have to check it, Druzus.
    */
   /*
    * I've checked in CL5.3 Technical Reference Guide that only
    * FORCEREL() should ensure that the work area buffer is not HOT
    * and then call RELEVAL() - I hope it describes the CL5.3 DBF* RDDs
    * behavior so I replicate it - the GOCOLD() is moved from CHILDSYNC()
    * to FORCEREL(), Druzus.
    */
   /*
    * After some cleanups, the core DBF* code can work with GOCOLD() here
    * and in FORCEREL() without any problems. Because calling GOCOLD() in
    * FORCEREL() may interacts with badly written users RDD which inherits
    * from DBF* RDDs and/or user triggers then I decided to keep it here,
    * Druzus.
    */

   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

#define hb_dbfSyncChildren NULL
#define hb_dbfClearRel     NULL

/*
 * Force relational seeks in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfForceRel( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfForceRel(%p)", pArea ) );

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel          = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel   = NULL;

      /* update buffers */
      /* commented out - see comment above in CHILDSYNC() method, Druzus */
      /* SELF_GOCOLD( ( AREAP ) pArea ); */

      return SELF_RELEVAL( ( AREAP ) pArea, lpdbPendingRel );
   }
   return HB_SUCCESS;
}

#define hb_dbfRelArea            NULL
#define hb_dbfRelEval            NULL
#define hb_dbfRelText            NULL
#define hb_dbfSetRel             NULL

#define hb_dbfOrderListAdd       NULL
#define hb_dbfOrderListClear     NULL
#define hb_dbfOrderListDelete    NULL
#define hb_dbfOrderListFocus     NULL
#define hb_dbfOrderListRebuild   NULL
#define hb_dbfOrderCondition     NULL
#define hb_dbfOrderCreate        NULL
#define hb_dbfOrderDestroy       NULL
#define hb_dbfOrderInfo          NULL

/*
 * Clear the filter condition for the specified WorkArea.
 */
static HB_ERRCODE hb_dbfClearFilter( DBFAREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfClearFilter(%p)", pArea ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   return SUPER_CLEARFILTER( ( AREAP ) pArea );
}

#define hb_dbfClearLocate  NULL
#define hb_dbfClearScope   NULL
#define hb_dbfCountScope   NULL
#define hb_dbfFilterText   NULL
#define hb_dbfScopeInfo    NULL

/*
 * Set the filter condition for the specified WorkArea.
 */
static HB_ERRCODE hb_dbfSetFilter( DBFAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfSetFilter(%p, %p)", pArea, pFilterInfo ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   return SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo );
}

#define hb_dbfSetLocate NULL
#define hb_dbfSetScope  NULL
#define hb_dbfSkipScope NULL
#define hb_dbfLocate    NULL

#define hb_dbfCompile   NULL
#define hb_dbfError     NULL
#define hb_dbfEvalBlock NULL

/*
 * Perform a network lowlevel lock in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfRawLock( DBFAREAP pArea, USHORT uiAction, ULONG ulRecNo )
{
   HB_ERRCODE  uiErr = HB_SUCCESS;
   HB_FOFFSET  ulPos, ulFlSize, ulRlSize;
   int         iDir;
   BOOL        fLck;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRawLock(%p, %hu, %lu)", pArea, uiAction, ulRecNo ) );

   if( pArea->fShared )
   {
      if( hb_dbfLockData( pArea, &ulPos, &ulFlSize, &ulRlSize, &iDir ) == HB_FAILURE )
         return HB_FAILURE;

      switch( uiAction )
      {
         case FILE_LOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos - ulFlSize, ulFlSize, FL_LOCK );
               else
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + 1, ulFlSize, FL_LOCK );

               if( ! fLck )
                  uiErr = HB_FAILURE;
               else
                  pArea->fFLocked = TRUE;
            }
            break;

         case FILE_UNLOCK:
            if( pArea->fFLocked )
            {
               if( iDir < 0 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos - ulFlSize, ulFlSize, FL_UNLOCK );
               else
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + 1, ulFlSize, FL_UNLOCK );

               if( ! fLck )
                  uiErr = HB_FAILURE;
               pArea->fFLocked = FALSE;
            }
            break;

         case REC_LOCK:
         case REC_LOCK_TEST:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos - ulRecNo, ulRlSize, FL_LOCK );
               else if( iDir == 2 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen, ulRlSize, FL_LOCK );
               else
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + ulRecNo, ulRlSize, FL_LOCK );

               if( ! fLck )
                  uiErr = HB_FAILURE;
               else if( uiAction == REC_LOCK_TEST )
                  hb_dbfRawLock( pArea, REC_UNLOCK, ulRecNo );
            }
            break;

         case REC_UNLOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos - ulRecNo, ulRlSize, FL_UNLOCK );
               else if( iDir == 2 )
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen, ulRlSize, FL_UNLOCK );
               else
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos + ulRecNo, ulRlSize, FL_UNLOCK );
               if( ! fLck )
                  uiErr = HB_FAILURE;
            }
            break;

         case APPEND_LOCK:
         case HEADER_LOCK:
            if( ! pArea->fHeaderLocked )
            {
               for(;; )
               {
                  fLck = hb_fileNetLock( pArea->pDataFile, ulPos, 1, FL_LOCK | FLX_WAIT );
                  /* TODO: call special error handler (LOCKHANDLER) hiere if !fLck */
                  if( fLck )
                     break;
                  hb_releaseCPU( 0 );
               }
               if( ! fLck )
                  uiErr = HB_FAILURE;
               else
               {
                  /*
                     if( uiAction == APPEND_LOCK )
                     hb_dbfUpdatedbaselockValue( pArea );
                   */
                  pArea->fHeaderLocked = TRUE;
               }
            }
            break;

         case APPEND_UNLOCK:
         case HEADER_UNLOCK:
            if( pArea->fHeaderLocked )
            {
               if( ! hb_fileNetLock( pArea->pDataFile, ulPos, 1, FL_UNLOCK ) )
                  uiErr = HB_FAILURE;
               pArea->fHeaderLocked = FALSE;
            }
            break;
      }
   }
   return uiErr;
}

/*
 * Perform a network lock in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfLock( DBFAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfLock(%p, %p)", pArea, pLockInfo ) );

   if( pArea->fShared )
   {
      switch( pLockInfo->uiMethod )
      {
         case DBLM_EXCLUSIVE:
            return hb_dbfLockRecord( pArea, 0, &pLockInfo->fResult, TRUE );

         case DBLM_MULTIPLE:
            return hb_dbfLockRecord( pArea, hb_itemGetNL( pLockInfo->itmRecID ),
                                     &pLockInfo->fResult, FALSE );

         case DBLM_FILE:
            return hb_dbfLockFile( pArea, &pLockInfo->fResult );

         default:
            pLockInfo->fResult = FALSE;
      }
   }
   else
      pLockInfo->fResult = TRUE;

   return HB_SUCCESS;
}

/*
 * Release network locks in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfUnLock( DBFAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE( HB_TR_DEBUG, ( "dbfUnLock(%p, %p)", pArea, pRecNo ) );

   if( pArea->fShared )
   {
      if( pArea->ulNumLocksPos > 0 )
      {
         ULONG ulRecNo = hb_itemGetNL( pRecNo );
         /* Unlock all records? */
         if( ulRecNo == 0 )
            errCode = hb_dbfUnlockAllRecords( pArea );
         else if( hb_dbfIsLocked( pArea, ulRecNo ) )
            errCode = hb_dbfUnlockRecord( pArea, ulRecNo );
      }
      if( pArea->fFLocked )
      {
         errCode = hb_dbfUnlockFile( pArea );
      }
   }
   return errCode;
}

#define hb_dbfCloseMemFile NULL

/*
 * Create a memo file in the WorkArea.
 */
static HB_ERRCODE hb_dbfCreateMemFile( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfCreateMemFile(%p, %p)", pArea, pCreateInfo ) );

   if( pCreateInfo )
      hb_dbfErrorRT( pArea, EG_CREATE, EDBF_DATATYPE, pCreateInfo->abName, 0, 0, NULL );

   pArea->fHasMemo = FALSE;

   return HB_FAILURE;
}

/*
 * BLOB2FILE - retrieve memo contents into file
 */
static HB_ERRCODE hb_dbfGetValueFile( DBFAREAP pArea, USHORT uiIndex, const char * szFile, USHORT uiMode )
{
   USHORT   errCode = HB_SUCCESS;
   LPFIELD  pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfGetValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_STRING )
   {
      PHB_FILE pFile;

      pFile = hb_fileNetExtOpen( szFile, NULL, FO_WRITE | FO_EXCLUSIVE |
                                 FXO_DEFAULTS | FXO_SHARELOCK |
                                 ( uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE ),
                                 NULL, NULL, TRUE );
      if( ! pFile )
      {
         errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
      }
      else
      {
         if( hb_fileNetWriteAt( pFile, pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                pField->uiLen, hb_fileNetSize( pFile ) ) !=
             ( ULONG ) pField->uiLen )
         {
            errCode = EDBF_WRITE;
         }
         hb_fileNetClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? hb_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Open a memo file in the specified WorkArea.
 */
static HB_ERRCODE hb_dbfOpenMemFile( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfOpenMemFile(%p, %p)", pArea, pOpenInfo ) );

   hb_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, pOpenInfo->abName, 0, 0, NULL );

   return HB_FAILURE;
}

/*
 * FILE2BLOB - store file contents in MEMO
 */
static HB_ERRCODE hb_dbfPutValueFile( DBFAREAP pArea, USHORT uiIndex, const char * szFile, USHORT uiMode )
{
   HB_ERRCODE  errCode = HB_SUCCESS;
   USHORT      uiRead;
   LPFIELD     pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfPutValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode ) );

   HB_SYMBOL_UNUSED( uiMode );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! hb_dbfReadRecord( pArea ) )
      return HB_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_FAILURE;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == HB_FT_STRING )
   {
      PHB_FILE pFile;

      pFile = hb_fileNetExtOpen( szFile, NULL, FO_READ | FO_DENYNONE |
                                 FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL, TRUE );
      if( ! pFile )
      {
         errCode = EDBF_OPEN_DBF;
      }
      else
      {
         uiRead = ( USHORT ) hb_fileNetReadAt( pFile, pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                               pField->uiLen, 0 );
         if( uiRead != ( USHORT ) FS_ERROR && uiRead < pField->uiLen )
            memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + uiRead,
                    ' ', pField->uiLen - uiRead );
         hb_fileNetClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != HB_SUCCESS )
   {
      hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? hb_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Read the database file header record in the WorkArea.
 */
static HB_ERRCODE hb_dbfReadDBHeader( DBFAREAP pArea )
{
   HB_ERRCODE  errCode;
   PHB_ITEM    pError;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfReadDBHeader(%p)", pArea ) );

   pError = NULL;
   do
   {
      errCode = HB_SUCCESS;

      if( hb_fileNetReadAt( pArea->pDataFile, &pArea->dbfHeader,
                            sizeof( DBFHEADER ), 0 ) != sizeof( DBFHEADER ) )
      {
         errCode = EDBF_READ;
      }
      else
      {
         pArea->fAutoInc                                                            = pArea->fModStamp =
                                                           pArea->fTableEncrypted   = pArea->fHasMemo = FALSE;
         pArea->bTableType                                                          = DB_DBF_STD;
         pArea->bMemoType                                                           = DB_MEMO_NONE;
         pArea->bCryptType                                                          = DB_CRYPT_NONE;

         pArea->fHasTags                                                            = ( pArea->dbfHeader.bHasTags & 0x01 ) != 0;

         switch( pArea->dbfHeader.bVersion )
         {
            case 0x31:  /* Visual FoxPro w. AutoIncrement field */
            case 0x32:  /* Visual FoxPro, autoincrement enabled */
               pArea->fAutoInc   = TRUE;
			 /* fallthrough */   
            case 0x30:  /* Visual FoxPro w. DBC or Visual FoxPro */
               pArea->bTableType = DB_DBF_VFP;
               if( pArea->dbfHeader.bHasTags & 0x02 )
               {
                  pArea->bMemoType  = DB_MEMO_FPT;
                  pArea->fHasMemo   = TRUE;
               }
#ifndef HB_COMPAT_FOXPRO
               switch( pArea->dbfHeader.bCodePage )
               {
                  case 0x01:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_437 ); break;
                  case 0x69:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_MAZ ); break;
                  case 0x6A:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_737 ); break;
                  case 0x02:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_850 ); break;
                  case 0x64:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_852 ); break;
                  case 0x6B:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_857 ); break;
                  case 0x67:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_861 ); break;
                  case 0x66:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_865 ); break;
                  case 0x65:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_866 ); break;
                  case 0x7C:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_874 ); break;
                  case 0x68:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_KAM ); break;
                  case 0xC8:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1250 ); break;
                  case 0xC9:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1251 ); break;
                  case 0x03:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1252 ); break;
                  case 0xCB:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1253 ); break;
                  case 0xCA:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1254 ); break;
                  case 0x7D:
                     pArea->cdPage  = hb_cdpcharsetFind( HB_CPID_1255 ); break;
                  case 0x7E:
                     Area->cdPage   = hb_cdpcharsetFind( HB_CPID_1256 ); break;
               }
#endif
               break;

            case 0x02:  /* FoxBASE */
            case 0x03:  /* FoxBASE+/Dbase III plus, no memo */
               break;

            case 0x04:  /* dBASE IV w/o memo file */
            case 0x7B:  /* dBASE IV with memo */
            case 0x8B:  /* dBASE IV w. memo */
               pArea->bTableType = DB_DBF_IV;
			/* fallthrough */
            case 0x83:  /* dBASE III+ with memo file DBT*/
               pArea->fHasMemo   = TRUE;
               pArea->bMemoType  = DB_MEMO_DBT;
               break;

            case 0xE5: /* Clipper SIX driver w. SMT memo file.
                          Note! Clipper SIX driver sets lowest 3 bytes to 110 in descriptor of crypted databases.
                          So, 3->6, 83h->86h, F5->F6, E5->E6 etc. */
               pArea->fHasMemo   = TRUE;
               pArea->bMemoType  = DB_MEMO_SMT;
               break;

            case 0xF5: /* FoxPro w. memo file */
               pArea->fHasMemo   = TRUE;
               pArea->bMemoType  = DB_MEMO_FPT;
               break;

            case 0x06:
               pArea->fTableEncrypted  = TRUE;
               pArea->bCryptType       = DB_CRYPT_SIX;
               break;

            case 0x86:
               pArea->fTableEncrypted  = TRUE;
               pArea->fHasMemo         = TRUE;
               pArea->bCryptType       = DB_CRYPT_SIX;
               pArea->bMemoType        = DB_MEMO_DBT;
               break;

            case 0xE6:
               pArea->fHasMemo         = TRUE;
               pArea->fTableEncrypted  = TRUE;
               pArea->bCryptType       = DB_CRYPT_SIX;
               pArea->bMemoType        = DB_MEMO_SMT;
               break;

            case 0xF6:
               pArea->fHasMemo         = TRUE;
               pArea->fTableEncrypted  = TRUE;
               pArea->bCryptType       = DB_CRYPT_SIX;
               pArea->bMemoType        = DB_MEMO_FPT;
               break;

            default:
               errCode = EDBF_CORRUPT;

         }
         if( errCode == HB_SUCCESS )
            break;
      }
   }
   while( hb_dbfErrorRT( pArea, hb_dbfGetEGcode( errCode ), errCode,
                         pArea->szDataFileName, hb_fsError(),
                         EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
   if( pError )
      hb_itemRelease( pError );

   if( errCode != HB_SUCCESS )
      return HB_FAILURE;

   pArea->uiHeaderLen   = HB_GET_LE_UINT16( pArea->dbfHeader.uiHeaderLen );
   pArea->ulRecCount    = HB_GET_LE_UINT32( pArea->dbfHeader.ulRecCount );

   return HB_SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 */
static HB_ERRCODE hb_dbfWriteDBHeader( DBFAREAP pArea )
{
   int         iYear, iMonth, iDay;
   BOOL        fLck = FALSE;
   HB_ERRCODE  errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfWriteDBHeader(%p)", pArea ) );

   if( pArea->fReadonly )
   {
      hb_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return HB_FAILURE;
   }

   pArea->dbfHeader.bHasTags = pArea->fHasTags ? 0x01 : 0x00;
   if( pArea->bTableType == DB_DBF_VFP )
   {
      pArea->dbfHeader.bVersion = ( pArea->fAutoInc ? 0x31 : 0x30 );
      if( pArea->fHasMemo && pArea->bMemoType == DB_MEMO_FPT )
         pArea->dbfHeader.bHasTags |= 0x02;
      /*
         Code page   Platform                   Code page identifier    xharbour charset
         437         U.S. MS-DOS                x01                     HB_CPID_437
         620 *       Mazovia (Polish) MS-DOS    x69                     HB_CPID_MAZ
         737 *       Greek MS-DOS (437G)        x6A                     HB_CPID_737
         850         International MS-DOS       x02                     HB_CPID_850
         852         Eastern European MS-DOS    x64                     HB_CPID_852
         857         Turkish MS-DOS             x6B                     HB_CPID_857
         861         Icelandic MS-DOS           x67                     HB_CPID_861
         865         Nordic MS-DOS              x66                     HB_CPID_865
         866         Russian MS-DOS             x65                     HB_CPID_866
         874         Thai Windows               x7C                     HB_CPID_874
         895 *       Kamenicky (Czech) MS-DOS   x68                     HB_CPID_KAM
         932         Japanese Windows           x7B
         936         Chinese (PRC, Singapore) Windows    x7A
         949         Korean Windows             x79
         950         Chinese (Hong Kong SAR, Taiwan) Windows   x78
         1250        Eastern European Windows   xC8                     HB_CPID_1250
         1251        Russian Windows            xC9                     HB_CPID_1251
         1252        Windows ANSI               x03                     HB_CPID_1252
         1253        Greek Windows              xCB                     HB_CPID_1253
         1254        Turkish Windows            xCA                     HB_CPID_1254
         1255        Hebrew Windows             x7D                     HB_CPID_1255
         1256        Arabic Windows             x7E                     HB_CPID_1256
         10000       Standard Macintosh         x04                     HB_CPID_10000
         10006       Greek Macintosh            x98                     HB_CPID_10006
         10007 *     Russian Macintosh          x96                     HB_CPID_10007
         10029       Macintosh EE               x97                     HB_CPID_10029
       * Not detected when you include CODEPAGE=AUTO in your configuration file.
       */
      if( strcmp( pArea->area.cdPage->uniID, HB_CPID_437 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x01;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_MAZ ) == 0 )
         pArea->dbfHeader.bCodePage = 0x69;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_737 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x6A;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_850 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x02;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_852 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x64;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_857 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x6B;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_861 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x67;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_865 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x66;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_866 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x65;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_874 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x7C;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_KAM ) == 0 )
         pArea->dbfHeader.bCodePage = 0x68;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1250 ) == 0 )
         pArea->dbfHeader.bCodePage = 0xC8;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1251 ) == 0 )
         pArea->dbfHeader.bCodePage = 0xC9;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1252 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x03;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1253 ) == 0 )
         pArea->dbfHeader.bCodePage = 0xCB;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1254 ) == 0 )
         pArea->dbfHeader.bCodePage = 0xCA;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1255 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x7D;
      else if( strcmp( pArea->area.cdPage->uniID, HB_CPID_1256 ) == 0 )
         pArea->dbfHeader.bCodePage = 0x7E;
   }
   else if( pArea->bTableType == DB_DBF_IV )
   {
      if( pArea->fHasMemo )
         pArea->dbfHeader.bVersion = 0x8B;
      else
         pArea->dbfHeader.bVersion = 0x03;
   }
   else
   {
      pArea->dbfHeader.bVersion = 0x03;
      if( pArea->fHasMemo )
      {
         if( pArea->bMemoType == DB_MEMO_DBT )
            pArea->dbfHeader.bVersion = 0x83;
         else if( pArea->bMemoType == DB_MEMO_FPT )
            pArea->dbfHeader.bVersion = 0xF5;
         else if( pArea->bMemoType == DB_MEMO_SMT )
            pArea->dbfHeader.bVersion = 0xE5;
      }
      if( pArea->fTableEncrypted && pArea->bCryptType == DB_CRYPT_SIX )
         pArea->dbfHeader.bVersion = ( pArea->dbfHeader.bVersion & 0xf0 ) | 0x06;
   }

   hb_dateToday( &iYear, &iMonth, &iDay );
   pArea->dbfHeader.bYear  = ( BYTE ) ( iYear - 1900 );
   pArea->dbfHeader.bMonth = ( BYTE ) iMonth;
   pArea->dbfHeader.bDay   = ( BYTE ) iDay;

   /* Update record count */
   if( pArea->fShared )
   {
      if( ! pArea->fHeaderLocked )
      {
         if( SELF_RAWLOCK( ( AREAP ) pArea, HEADER_LOCK, 0 ) != HB_SUCCESS )
            return HB_FAILURE;
         fLck = TRUE;
      }
      pArea->ulRecCount = hb_dbfCalcRecCount( pArea );
   }
   else
   {
      /* Exclusive mode */
      /* write eof mark */
      HB_FOFFSET llOffset = ( HB_FOFFSET ) pArea->uiHeaderLen +
                            ( HB_FOFFSET ) pArea->uiRecordLen *
                            ( HB_FOFFSET ) pArea->ulRecCount;
      /* protection against file create */
      if( hb_fileNetSize( pArea->pDataFile ) > llOffset )
      {
         hb_fileNetWriteAt( pArea->pDataFile, "\032", 1, llOffset );
         hb_fileNetTruncAt( pArea->pDataFile, llOffset + 1 );
      }
   }

   HB_PUT_LE_UINT32( pArea->dbfHeader.ulRecCount, pArea->ulRecCount );
   HB_PUT_LE_UINT16( pArea->dbfHeader.uiHeaderLen, pArea->uiHeaderLen );
   HB_PUT_LE_UINT16( pArea->dbfHeader.uiRecordLen, pArea->uiRecordLen );
   if( hb_fileNetWriteAt( pArea->pDataFile, &pArea->dbfHeader,
                          sizeof( DBFHEADER ), 0 ) == sizeof( DBFHEADER ) )
   {
      errCode = HB_SUCCESS;
   }
   else
   {
      errCode = HB_FAILURE;
   }
   /* TODO: add RT error */
   pArea->fDataFlush    = TRUE;
   pArea->fUpdateHeader = FALSE;
   if( fLck )
   {
      if( SELF_RAWLOCK( ( AREAP ) pArea, HEADER_UNLOCK, 0 ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( errCode != HB_SUCCESS )
      hb_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                     hb_fsError(), 0, NULL );

   return errCode;
}

static HB_ERRCODE hb_dbfDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, ULONG ulConnect )
{
   char           szFileName[ HB_PATH_MAX ];
   const char *   szFile, * szExt;
   PHB_ITEM       pFileExt = NULL;
   PHB_FNAME      pFileName;
   BOOL           fTable   = FALSE, fResult = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfDrop(%p,%p,%p,%lu)", pRDD, pItemTable, pItemIndex, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile   = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable   = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutC( NULL, NULL );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   /* Use hb_FileNetExists first to locate table which can be in differ path */
   if( hb_FileNetExists( szFileName, szFileName ) )
   {
      fResult = hb_fileNetDelete( szFileName, 2 );
      if( fResult && fTable )
      {
         /*
          * Database table file has been deleted, now check if memo is
          * supported and if yes then try to delete memo file if it exists
          * in the same directory as table file
          * hb_fsFNameSplit() repeated intentionally to respect
          * the path set by hb_FileNetExists()
          */
         pFileName   = hb_fsFNameSplit( szFileName );
         pFileExt    = hb_itemPutC( pFileExt, NULL );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fileNetDelete( szFileName, 2 );
            }
         }
         /*
          * and try to delete production index also if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutC( pFileExt, NULL );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fileNetDelete( szFileName, 2 );
            }
         }
         hb_xfree( pFileName );
      }
   }

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE hb_dbfExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, ULONG ulConnect )
{
   char           szFileName[ HB_PATH_MAX ];
   const char *   szFile;
   PHB_ITEM       pFileExt = NULL;
   PHB_FNAME      pFileName;
   BOOL           fTable   = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfExists(%p,%p,%p,%lu)", pRDD, pItemTable, pItemIndex, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      szFile   = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable   = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      pFileExt = hb_itemPutC( NULL, NULL );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return hb_FileNetExists( szFileName, NULL ) ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE hb_dbfRename( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, PHB_ITEM pItemNew, ULONG ulConnect )
{
   char           szFileName[ HB_PATH_MAX ];
   char           szFileNew[ HB_PATH_MAX ];
   const char *   szFile, * szExt;
   PHB_ITEM       pFileExt = NULL;
   PHB_FNAME      pFileName, pFileNameNew;
   BOOL           fTable   = FALSE, fResult = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRename(%p,%p,%p,%p,%lu)", pRDD, pItemTable, pItemIndex, pItemNew, ulConnect ) );

   szFile = hb_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile   = hb_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return HB_FAILURE;
      fTable   = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutC( pFileExt, NULL );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   szFile = hb_itemGetCPtr( pItemNew );
   /* Use hb_FileNetExists first to locate table which can be in differ path */
   if( szFile[ 0 ] && hb_FileNetExists( szFileName, szFileName ) )
   {
      /* hb_fsFNameSplit() repeated intentionally to respect
       * the path set by hb_FileNetExists()
       */
      pFileName      = hb_fsFNameSplit( szFileName );

      pFileNameNew   = hb_fsFNameSplit( szFile );
      if( ! pFileNameNew->szExtension && ( ! fTable || hb_setGetDefExtension() ) )
      {
         /* Add default extension if missing */
         pFileExt = hb_itemPutC( pFileExt, NULL );
         if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == HB_SUCCESS )
            pFileNameNew->szExtension = hb_itemGetCPtr( pFileExt );
      }
      if( ! pFileNameNew->szPath )
         pFileNameNew->szPath = pFileName->szPath;
      hb_fsFNameMerge( szFileNew, pFileNameNew );

      fResult = hb_fileNetRename( szFileName, szFileNew );
      if( fResult && fTable )
      {
         /*
          * Database table file has been renamed, now check if memo is
          * supported and if yes then try to rename memo file if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutC( pFileExt, NULL );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension     = szExt;
               pFileNameNew->szExtension  = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsFNameMerge( szFileNew, pFileNameNew );
               hb_fileNetRename( szFileName, szFileNew );
            }
         }
         /*
          * and try to rename production index also if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutC( pFileExt, NULL );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension     = szExt;
               pFileNameNew->szExtension  = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsFNameMerge( szFileNew, pFileNameNew );
               hb_fileNetRename( szFileName, szFileNew );
            }
         }
      }
      hb_xfree( pFileName );
      hb_xfree( pFileNameNew );
   }

   if( pFileExt )
      hb_itemRelease( pFileExt );

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE hb_dbfInit( LPRDDNODE pRDD )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfInit(%p)", pRDD ) );

   DBFNODE_DATARAW( pRDD )             = ( LPDBFDATA ) hb_xgrab( sizeof( DBFDATA ) );
   memset( DBFNODE_DATA( pRDD ), 0, sizeof( DBFDATA ) );

   DBFNODE_DATA( pRDD )->bTableType    = DB_DBF_STD;
   DBFNODE_DATA( pRDD )->bCryptType    = DB_CRYPT_NONE;
   DBFNODE_DATA( pRDD )->uiDirtyRead   = HB_IDXREAD_CLEANMASK;

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfExit( LPRDDNODE pRDD )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfExit(%p)", pRDD ) );

   if( pRDD->lpvCargo )
   {
      LPDBFDATA pData = DBFNODE_DATA( pRDD );

      if( pData->szTrigger )
         hb_xfree( pData->szTrigger );
      if( pData->szPendingTrigger )
         hb_xfree( pData->szPendingTrigger );
      if( pData->szPasswd )
         hb_xfree( pData->szPasswd );
      if( pData->szPendingPasswd )
         hb_xfree( pData->szPendingPasswd );

      hb_xfree( pRDD->lpvCargo );
      pRDD->lpvCargo = NULL;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE hb_dbfRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   LPDBFDATA pData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbfRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem ) );

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_REMOTE:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_LOCAL:
         hb_itemPutL( pItem, FALSE );
         break;

      case RDDI_TABLEEXT:
      {
         const char *   szNew = hb_itemGetCPtr( pItem );
         char *         szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? hb_strdup( szNew ) : NULL;
         hb_itemPutC( pItem, pData->szTableExt[ 0 ] ? pData->szTableExt : DBF_TABLEEXT );
         if( szNewVal )
         {
            hb_strncpy( pData->szTableExt, szNewVal, sizeof( pData->szTableExt ) - 1 );
            hb_xfree( szNewVal );
         }
         break;
      }
      case RDDI_TABLETYPE:
      {
         int iType = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, pData->bTableType ? pData->bTableType : DB_DBF_STD );
         switch( iType )
         {
            case DB_DBF_STD:        /* standard dBase/Clipper DBF file */
            case DB_DBF_VFP:        /* VFP DBF file */
            case DB_DBF_IV:         /* dBase IV DBF file */
               pData->bTableType = ( BYTE ) iType;
         }
         break;
      }
      case RDDI_LOCKSCHEME:
      {
         int iScheme = hb_itemGetNI( pItem );

         hb_itemPutNI( pItem, pData->bLockType ? pData->bLockType :
                       hb_setGetDBFLockScheme() );
         switch( iScheme )
         {
            case DB_DBFLOCK_CLIP:
            case DB_DBFLOCK_CL53:
            case DB_DBFLOCK_CL53EXT:
            case DB_DBFLOCK_VFP:
#ifndef HB_LONG_LONG_OFF
            case DB_DBFLOCK_XHB64:
#endif
               pData->bLockType = ( BYTE ) iScheme;
         }
         break;
      }
      case RDDI_DIRTYREAD:
      {
         BOOL fDirty = ( pData->uiDirtyRead == HB_IDXREAD_DIRTYMASK );
         if( HB_IS_LOGICAL( pItem ) )
         {
            pData->uiDirtyRead = hb_itemGetL( pItem ) ?
                                 HB_IDXREAD_DIRTYMASK : HB_IDXREAD_CLEANMASK;
         }
         hb_itemPutL( pItem, fDirty );
         break;
      }
      case RDDI_TRIGGER:
      {
         char *   szTrigger   = pData->szTrigger;
         BOOL     fFree       = FALSE;

         if( HB_IS_STRING( pItem ) )
         {
            fFree             = TRUE;
            pData->szTrigger  = hb_itemGetCLen( pItem ) > 0 ?
                                hb_itemGetC( pItem ) : NULL;
         }

         if( fFree && szTrigger )
            hb_itemPutCPtr( pItem, szTrigger, strlen( szTrigger ) );
         else
            hb_itemPutC( pItem, szTrigger );

         if( ! szTrigger && ! fFree )
            return HB_FAILURE;

         break;
      }
      case RDDI_PENDINGTRIGGER:
         if( HB_IS_STRING( pItem ) )
         {
            if( pData->szPendingTrigger )
            {
               hb_xfree( pData->szPendingTrigger );
               pData->szPendingTrigger = NULL;
            }
            if( hb_itemGetCLen( pItem ) > 0 )
               pData->szPendingTrigger = hb_itemGetC( pItem );
         }
         else if( pData->szPendingTrigger )
         {
            hb_itemPutCPtr( pItem, pData->szPendingTrigger,
                            strlen( pData->szPendingTrigger ) );
            pData->szPendingTrigger = NULL;
         }
         else
            return HB_FAILURE;
         break;

      case RDDI_PASSWORD:
      {
         char *   szPasswd = pData->szPasswd;
         BOOL     fFree    = FALSE;

         if( HB_IS_STRING( pItem ) )
         {
            fFree             = TRUE;
            pData->szPasswd   = hb_itemGetCLen( pItem ) > 0 ?
                                hb_itemGetC( pItem ) : NULL;
         }

         if( fFree && szPasswd )
            hb_itemPutCPtr( pItem, szPasswd, strlen( szPasswd ) );
         else
            hb_itemPutC( pItem, szPasswd );

         if( ! szPasswd && ! fFree )
            return HB_FAILURE;

         break;
      }
      case RDDI_PENDINGPASSWORD:
         if( HB_IS_STRING( pItem ) )
         {
            if( pData->szPendingPasswd )
            {
               hb_xfree( pData->szPendingPasswd );
               pData->szPendingPasswd = NULL;
            }
            if( hb_itemGetCLen( pItem ) > 0 )
               pData->szPendingPasswd = hb_itemGetC( pItem );
         }
         else if( pData->szPendingPasswd )
         {
            hb_itemPutCPtr( pItem, pData->szPendingPasswd,
                            strlen( pData->szPendingPasswd ) );
            pData->szPendingPasswd = NULL;
         }
         else
            return HB_FAILURE;
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return HB_SUCCESS;
}

#define hb_dbfWhoCares NULL


static const RDDFUNCS dbfTable = { ( DBENTRYP_BP ) hb_dbfBof,
                                   ( DBENTRYP_BP ) hb_dbfEof,
                                   ( DBENTRYP_BP ) hb_dbfFound,
                                   ( DBENTRYP_V ) hb_dbfGoBottom,
                                   ( DBENTRYP_UL ) hb_dbfGoTo,
                                   ( DBENTRYP_I ) hb_dbfGoToId,
                                   ( DBENTRYP_V ) hb_dbfGoTop,
                                   ( DBENTRYP_BIB ) hb_dbfSeek,
                                   ( DBENTRYP_L ) hb_dbfSkip,
                                   ( DBENTRYP_L ) hb_dbfSkipFilter,
                                   ( DBENTRYP_L ) hb_dbfSkipRaw,
                                   ( DBENTRYP_VF ) hb_dbfAddField,
                                   ( DBENTRYP_B ) hb_dbfAppend,
                                   ( DBENTRYP_I ) hb_dbfCreateFields,
                                   ( DBENTRYP_V ) hb_dbfDeleteRec,
                                   ( DBENTRYP_BP ) hb_dbfDeleted,
                                   ( DBENTRYP_SP ) hb_dbfFieldCount,
                                   ( DBENTRYP_VF ) hb_dbfFieldDisplay,
                                   ( DBENTRYP_SSI ) hb_dbfFieldInfo,
                                   ( DBENTRYP_SCP ) hb_dbfFieldName,
                                   ( DBENTRYP_V ) hb_dbfFlush,
                                   ( DBENTRYP_PP ) hb_dbfGetRec,
                                   ( DBENTRYP_SI ) hb_dbfGetValue,
                                   ( DBENTRYP_SVL ) hb_dbfGetVarLen,
                                   ( DBENTRYP_V ) hb_dbfGoCold,
                                   ( DBENTRYP_V ) hb_dbfGoHot,
                                   ( DBENTRYP_P ) hb_dbfPutRec,
                                   ( DBENTRYP_SI ) hb_dbfPutValue,
                                   ( DBENTRYP_V ) hb_dbfRecall,
                                   ( DBENTRYP_ULP ) hb_dbfRecCount,
                                   ( DBENTRYP_ISI ) hb_dbfRecInfo,
                                   ( DBENTRYP_ULP ) hb_dbfRecNo,
                                   ( DBENTRYP_I ) hb_dbfRecId,
                                   ( DBENTRYP_S ) hb_dbfSetFieldExtent,
                                   ( DBENTRYP_CP ) hb_dbfAlias,
                                   ( DBENTRYP_V ) hb_dbfClose,
                                   ( DBENTRYP_VO ) hb_dbfCreate,
                                   ( DBENTRYP_SI ) hb_dbfInfo,
                                   ( DBENTRYP_V ) hb_dbfNewArea,
                                   ( DBENTRYP_VO ) hb_dbfOpen,
                                   ( DBENTRYP_V ) hb_dbfRelease,
                                   ( DBENTRYP_SP ) hb_dbfStructSize,
                                   ( DBENTRYP_CP ) hb_dbfSysName,
                                   ( DBENTRYP_VEI ) hb_dbfEval,
                                   ( DBENTRYP_V ) hb_dbfPack,
                                   ( DBENTRYP_LSP ) hb_dbfPackRec,
                                   ( DBENTRYP_VS ) hb_dbfSort,
                                   ( DBENTRYP_VT ) hb_dbfTrans,
                                   ( DBENTRYP_VT ) hb_dbfTransRec,
                                   ( DBENTRYP_V ) hb_dbfZap,
                                   ( DBENTRYP_VR ) hb_dbfChildEnd,
                                   ( DBENTRYP_VR ) hb_dbfChildStart,
                                   ( DBENTRYP_VR ) hb_dbfChildSync,
                                   ( DBENTRYP_V ) hb_dbfSyncChildren,
                                   ( DBENTRYP_V ) hb_dbfClearRel,
                                   ( DBENTRYP_V ) hb_dbfForceRel,
                                   ( DBENTRYP_SSP ) hb_dbfRelArea,
                                   ( DBENTRYP_VR ) hb_dbfRelEval,
                                   ( DBENTRYP_SI ) hb_dbfRelText,
                                   ( DBENTRYP_VR ) hb_dbfSetRel,
                                   ( DBENTRYP_VOI ) hb_dbfOrderListAdd,
                                   ( DBENTRYP_V ) hb_dbfOrderListClear,
                                   ( DBENTRYP_VOI ) hb_dbfOrderListDelete,
                                   ( DBENTRYP_VOI ) hb_dbfOrderListFocus,
                                   ( DBENTRYP_V ) hb_dbfOrderListRebuild,
                                   ( DBENTRYP_VOO ) hb_dbfOrderCondition,
                                   ( DBENTRYP_VOC ) hb_dbfOrderCreate,
                                   ( DBENTRYP_VOI ) hb_dbfOrderDestroy,
                                   ( DBENTRYP_SVOI ) hb_dbfOrderInfo,
                                   ( DBENTRYP_V ) hb_dbfClearFilter,
                                   ( DBENTRYP_V ) hb_dbfClearLocate,
                                   ( DBENTRYP_V ) hb_dbfClearScope,
                                   ( DBENTRYP_VPLP ) hb_dbfCountScope,
                                   ( DBENTRYP_I ) hb_dbfFilterText,
                                   ( DBENTRYP_SI ) hb_dbfScopeInfo,
                                   ( DBENTRYP_VFI ) hb_dbfSetFilter,
                                   ( DBENTRYP_VLO ) hb_dbfSetLocate,
                                   ( DBENTRYP_VOS ) hb_dbfSetScope,
                                   ( DBENTRYP_VPL ) hb_dbfSkipScope,
                                   ( DBENTRYP_B ) hb_dbfLocate,
                                   ( DBENTRYP_CC ) hb_dbfCompile,
                                   ( DBENTRYP_I ) hb_dbfError,
                                   ( DBENTRYP_I ) hb_dbfEvalBlock,
                                   ( DBENTRYP_VSP ) hb_dbfRawLock,
                                   ( DBENTRYP_VL ) hb_dbfLock,
                                   ( DBENTRYP_I ) hb_dbfUnLock,
                                   ( DBENTRYP_V ) hb_dbfCloseMemFile,
                                   ( DBENTRYP_VO ) hb_dbfCreateMemFile,
                                   ( DBENTRYP_SCCS ) hb_dbfGetValueFile,
                                   ( DBENTRYP_VO ) hb_dbfOpenMemFile,
                                   ( DBENTRYP_SCCS ) hb_dbfPutValueFile,
                                   ( DBENTRYP_V ) hb_dbfReadDBHeader,
                                   ( DBENTRYP_V ) hb_dbfWriteDBHeader,
                                   ( DBENTRYP_R ) hb_dbfInit,
                                   ( DBENTRYP_R ) hb_dbfExit,
                                   ( DBENTRYP_RVVL ) hb_dbfDrop,
                                   ( DBENTRYP_RVVL ) hb_dbfExists,
                                   ( DBENTRYP_RVVVL ) hb_dbfRename,
                                   ( DBENTRYP_RSLV ) hb_dbfRddInfo,
                                   ( DBENTRYP_SVP ) hb_dbfWhoCares };

HB_FUNC( _DBFNET )
{
   ;
}

HB_FUNC( DBFNET_GETFUNCTABLE )
{
   RDDFUNCS *  pTable;
   USHORT *    uiCount;

   uiCount  = ( USHORT * ) hb_parptr( 1 );
   pTable   = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE( HB_TR_DEBUG, ( "DBFNET_GETFUNCTABLE(%p, %p)", uiCount, pTable ) );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( uiCount )
         *uiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInherit( pTable, &dbfTable, &dbfSuper, NULL );
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}


#define __PRG_SOURCE__        __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER   HB_PCODE_VER
#endif

static void hb_dbfRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBFNET", RDT_FULL ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

HB_INIT_SYMBOLS_BEGIN( dbf1net__InitSymbols )
{
   "_DBFNET", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( _DBFNET ) }, NULL
},
{ "DBFNET_GETFUNCTABLE", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( DBFNET_GETFUNCTABLE ) }, NULL }
HB_INIT_SYMBOLS_END( dbf1net__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_dbfnet_rdd_init_ )
hb_vmAtInit( hb_dbfRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_dbfnet_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup dbf1net__InitSymbols
   #pragma startup _hb_dbfnet_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( dbf1net__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_dbfnet_rdd_init_ )
   #include "hbiniseg.h"
#endif
