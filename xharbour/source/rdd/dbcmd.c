/*
 * $Id: dbcmd.c,v 1.114 2004/05/18 10:07:41 lf_sfnet Exp $
 */

/*
 * Harbour Project source code:
 * Base RDD module
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
 *       Horacio Roldan <harbour_ar@yahoo.com.ar>
 *
 * ordKeyVal()
 * ordKeyAdd()
 * ordKeyDel()
 * hb_rddIterateWorkAreas()
 * hb_rddGetTempAlias
 * __RDDGETTEMPALIAS
 *
 */

/* JC1: optimizing stack access under MT */
#define HB_THREAD_OPTIMIZE_STACK

#include <ctype.h>
#include "hbstack.h"
#include "hbvm.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapiitm.h"
#include "hbrddwrk.h"
#include "hbfast.h"
#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

HB_FUNC_EXTERN( _DBF );
HB_FUNC_EXTERN( _SDF );
HB_FUNC_EXTERN( _DELIM );
HB_FUNC_EXTERN( RDDSYS );

#define __PRG_SOURCE__ __FILE__
HB_FUNC_EXTERN( FPARSEEX );
#undef HB_PRG_PCODE_VER
#define HB_PRG_PCODE_VER HB_PCODE_VER
HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_DBCMD )
{ "FPARSEEX", HB_FS_PUBLIC, {HB_FUNCNAME( FPARSEEX )}, NULL }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_DBCMD )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_vm_SymbolInit_DBCMD
#elif defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_SymbolInit_DBCMD = hb_vm_SymbolInit_DBCMD;
   #pragma data_seg()
#endif


static char s_szDefDriver[HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1] = ""; /* Default RDD name */
static LPRDDNODE * s_RddList = NULL;   /* Registered RDDs */
static USHORT s_uiRddMax = 0;          /* Number of registered RDD */

static AREAP * s_WaList = NULL;        /* Allocated WorkAreas */
static USHORT s_uiWaMax = 0;           /* Number of allocated WA */
static USHORT s_uiWaSpace = 0;           /* Number of allocated WA */

static USHORT * s_WaNums = NULL;       /* Allocated WorkAreas */
static USHORT s_uiWaNumMax = 0;        /* Number of allocated WA */

static BOOL s_bNetError = FALSE;       /* Error on Networked environments */
#ifndef HB_THREAD_SUPPORT
   static USHORT s_uiCurrArea = 1;     /* Selected area */
   static AREAP  s_pCurrArea = NULL;     /* Selected area */
   #define LOCK_AREA
   #define UNLOCK_AREA
   #define LOCK_AREA_INIT
   #define LOCK_AREA_DESTROY
#else
   #define s_uiCurrArea    HB_VM_STACK.uiCurrArea
   #define s_pCurrArea     HB_VM_STACK.pCurrArea
   HB_CRITICAL_T  s_mtxWorkArea;
   #if defined (HB_OS_WIN_32) || defined(HB_OS_OS2)
      static BOOL s_fMtLockInit = FALSE;
      #define LOCK_AREA          if ( s_fMtLockInit ) HB_CRITICAL_LOCK( s_mtxWorkArea );
      #define UNLOCK_AREA        if ( s_fMtLockInit ) HB_CRITICAL_UNLOCK( s_mtxWorkArea );
      #define LOCK_AREA_INIT     if ( !s_fMtLockInit ) { HB_CRITICAL_INIT( s_mtxWorkArea ); s_fMtLockInit = TRUE; }
      #define LOCK_AREA_DESTROY  if ( s_fMtLockInit ) { HB_CRITICAL_DESTROY( s_mtxWorkArea ); s_fMtLockInit = FALSE; }
   #else
      #define LOCK_AREA HB_CRITICAL_LOCK( s_mtxWorkArea );
      #define UNLOCK_AREA HB_CRITICAL_UNLOCK( s_mtxWorkArea );
      #define LOCK_AREA_INIT
      #define LOCK_AREA_DESTROY
   #endif
#endif

#define HB_SET_WA( n )  do \
            { \
               s_uiCurrArea = n; \
               s_pCurrArea = ( ( s_uiCurrArea < s_uiWaNumMax ) ? \
                                 s_WaList[ s_WaNums[ s_uiCurrArea ] ] : \
                                 NULL ); \
            } while ( 0 );

#define HB_GET_WA( n )  ( ( (n) < s_uiWaNumMax ) ? s_WaList[ s_WaNums[ ( n ) ] ] : NULL )
//#define HB_CURRENT_WA   HB_GET_WA( s_uiCurrArea )
#define HB_CURRENT_WA   s_pCurrArea

/*
 * -- DEFAULT METHODS --
 */

/*
 * Force link the built-in RDD's.
 */
static void hb_rddCheck( void )
{
   static BOOL fInit = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddCheck()"));

   if( !fInit )
   {
      /* Force link the built-in RDD's */
      HB_FUNCNAME( _DBF )();
      HB_FUNCNAME( _SDF )();
      HB_FUNCNAME( _DELIM )();
      HB_FUNCNAME( RDDSYS )();
      fInit = TRUE;
   }
}


#if 0
/*
 * Empty method.
 */
static ERRCODE hb_waNull( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waNull(%p)", pArea));
   HB_SYMBOL_UNUSED( pArea );

   return SUCCESS;
}
#endif

/*
 * Raise a runtime error if an method is not defined.
 */
static ERRCODE hb_waUnsupported( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waUnsupported(%p)", pArea));

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_itemRelease( pError );

   return FAILURE;
}

/*
 * The default virtual method table for all WorkAreas.
 */
static RDDFUNCS waTable = { hb_waBof,
                            hb_waEof,
                            hb_waFound,
                            hb_waGoBottom,
                            hb_waGoTo,
                            hb_waGoToId,
                            hb_waGoTop,
                            hb_waSeek,
                            hb_waSkip,
                            hb_waSkipFilter,
                            hb_waSkipRaw,
                            hb_waAddField,
                            hb_waAppend,
                            hb_waCreateFields,
                            hb_waDeleteRec,
                            hb_waDeleted,
                            hb_waFieldCount,
                            hb_waFieldDisplay,
                            hb_waFieldInfo,
                            hb_waFieldName,
                            hb_waFlush,
                            hb_waGetRec,
                            hb_waGetValue,
                            hb_waGetVarLen,
                            hb_waGoCold,
                            hb_waGoHot,
                            hb_waPutRec,
                            hb_waPutValue,
                            hb_waRecall,
                            hb_waRecCount,
                            hb_waRecInfo,
                            hb_waRecNo,
                            hb_waSetFieldExtent,
                            hb_waAlias,
                            hb_waClose,
                            hb_waCreate,
                            hb_waInfo,
                            hb_waNewArea,
                            hb_waOpen,
                            hb_waRelease,
                            hb_waStructSize,
                            hb_waSysName,
                            hb_waEval,
                            hb_waPack,
                            hb_waPackRec,
                            hb_waSort,
                            hb_waTrans,
                            hb_waTransRec,
                            hb_waZap,
                            hb_waChildEnd,
                            hb_waChildStart,
                            hb_waChildSync,
                            hb_waSyncChildren,
                            hb_waClearRel,
                            hb_waForceRel,
                            hb_waRelArea,
                            hb_waRelEval,
                            hb_waRelText,
                            hb_waSetRel,
                            hb_waOrderListAdd,
                            hb_waOrderListClear,
                            hb_waOrderListDelete,
                            hb_waOrderListFocus,
                            hb_waOrderListRebuild,
                            hb_waOrderCondition,
                            hb_waOrderCreate,
                            hb_waOrderDestroy,
                            hb_waOrderInfo,
                            hb_waClearFilter,
                            hb_waClearLocate,
                            hb_waClearScope,
                            hb_waCountScope,
                            hb_waFilterText,
                            hb_waScopeInfo,
                            hb_waSetFilter,
                            hb_waSetLocate,
                            hb_waSetScope,
                            hb_waSkipScope,
                            hb_waCompile,
                            hb_waError,
                            hb_waEvalBlock,
                            hb_waRawLock,
                            hb_waLock,
                            hb_waUnLock,
                            hb_waCloseMemFile,
                            hb_waCreateMemFile,
                            hb_waGetValueFile,
                            hb_waOpenMemFile,
                            hb_waPutValueFile,
                            hb_waReadDBHeader,
                            hb_waWriteDBHeader,
                            hb_rddExit,
                            hb_rddDrop,
                            hb_rddExists,
                            hb_waWhoCares
                           };


/*
 * Find a RDD node.
 */
static LPRDDNODE hb_rddFindNode( char * szDriver, USHORT * uiIndex )
{
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, uiIndex));

   for ( uiCount = 0; uiCount < s_uiRddMax; uiCount++ )
   {
      if( strcmp( s_RddList[ uiCount ]->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            * uiIndex = uiCount;
         return s_RddList[ uiCount ];
      }
   }
   if( uiIndex )
      * uiIndex = 0;
   return NULL;
}


/*
 * Register a RDD driver.
 */
static int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;
   USHORT uiFunctions;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddRegister(%s, %hu)", szDriver, uiType));

   if( hb_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      return 1;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_dynsymFindName( szGetFuncTable );
   hb_xfree( szGetFuncTable );
   if( !pGetFuncTable )
      return 2;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
   memset( pRddNewNode, 0, sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   strncat( pRddNewNode->szName, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   pRddNewNode->uiType = uiType;

   /* Call <szDriver>_GETFUNCTABLE() */
   hb_vmPushSymbol( pGetFuncTable->pSymbol );
   hb_vmPushNil();
   hb_vmPushPointer( ( void * ) &uiFunctions );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   hb_vmDo( 2 );
   if ( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return 3;                        /* Invalid FUNCTABLE */
   }

   if( s_uiRddMax == 0 )                /* First RDD node */
   {
      LOCK_AREA_INIT
      s_RddList = (LPRDDNODE *) hb_xgrab( sizeof(LPRDDNODE) );
   }
   else
      s_RddList = (LPRDDNODE *) hb_xrealloc( s_RddList, sizeof(LPRDDNODE) * ( s_uiRddMax + 1 ) );

   s_RddList[ s_uiRddMax++ ] = pRddNewNode;   /* Add the new RDD node */

   return 0;                           /* Ok */
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
 */
ERRCODE HB_EXPORT hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName));

   if( !pTable )
   {
      return FAILURE;
   }

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || ( uiCount = strlen( ( const char * ) szDrvName ) ) == 0 )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &waTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &waTable, sizeof( RDDFUNCS ) );
   }
   else
   {
      char szSuperName[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
      hb_strncpyUpper( szSuperName, ( char * ) szDrvName, HB_MIN( uiCount, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ) );
      pRddNode = hb_rddFindNode( szSuperName, NULL );

      if( !pRddNode )
      {
         return FAILURE;
      }

      memcpy( pTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
   }

   /* Copy the non NULL entries from pSubTable into pTable */
   pFunction = ( DBENTRYP_V * ) pTable;
   pSubFunction = ( DBENTRYP_V * ) pSubTable;
   for( uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++ )
   {
      if( * pSubFunction )
         * pFunction = * pSubFunction;
      pFunction ++;
      pSubFunction ++;
   }
   return SUCCESS;
}

/*
 * Find a WorkArea by the alias or return 0 without raising an Error.
 */
static USHORT hb_rddSelect( char * szAlias )
{
   PHB_DYNS pSymAlias;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelect(%s)", szAlias));

   pSymAlias = hb_dynsymFindName( szAlias );

   if( pSymAlias && pSymAlias->hArea )
   {
      return ( USHORT ) pSymAlias->hArea;
   }
   else
   {
      return 0;
   }
}

/*
 * Return the next free WorkArea for later use.
 */
static void hb_rddSelectFirstAvailable( void )
{
   HB_THREAD_STUB
   USHORT uiArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

   LOCK_AREA
   uiArea = 1;
   while ( uiArea < s_uiWaNumMax )
   {
      if ( s_WaNums[ uiArea ] == 0 )
         break;
      uiArea++;
   }
   HB_SET_WA( uiArea );
   UNLOCK_AREA
}

/*
 * Prepares a new WorkArea node.
 */
static AREAP hb_rddNewAreaNode( LPRDDNODE pRddNode, USHORT uiRddID )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p %d)", pRddNode,uiRddID));

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      USHORT uiSize;

      pArea = ( AREAP ) hb_xgrab( sizeof( AREA ) );
      memset( pArea, 0, sizeof( AREA ) );
      pArea->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( pArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pArea = ( AREAP ) hb_xrealloc( pArea, uiSize );
         memset( pArea, 0, uiSize );
         pArea->lprfsHost = &pRddNode->pTable;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pArea, 0, pRddNode->uiAreaSize );
      pArea->lprfsHost = &pRddNode->pTable;
   }

   pArea->rddID = uiRddID;
   SELF_NEW( pArea );

   return pArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
static void hb_rddReleaseCurrentArea( void )
{
   HB_THREAD_STUB
   USHORT uiWaPos;
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddReleaseCurrentArea()"));

   SELF_CLOSE( ( AREAP ) pArea );
   SELF_RELEASE( ( AREAP ) pArea );

   LOCK_AREA

   uiWaPos = s_WaNums[ s_uiCurrArea ];
   s_WaNums[ s_uiCurrArea ] = 0;
   s_uiWaMax--;
   if ( s_uiWaMax <= 1 )
   {
      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
   }
   else
   {
      while ( uiWaPos < s_uiWaMax )
      {
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos + 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos++;
      }
      s_WaList[ s_uiWaMax ] = NULL;
      if ( s_uiWaSpace - s_uiWaMax >= 256 )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = (AREAP *) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof(AREAP) );
      }
   }
   s_pCurrArea = NULL;

   UNLOCK_AREA
}

/*
 * Closes all WorkAreas.
 */
static void hb_rddCloseAll( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

   if ( s_uiWaMax > 0 )
   {
      BOOL isParents = TRUE, isFinish = FALSE;
      AREAP pArea;
      USHORT uiIndex;

      LOCK_AREA

      while( isParents )
      {
         isParents = FALSE;
         for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
         {
            pArea = s_WaList[ uiIndex ];
            HB_SET_WA( pArea->uiArea );
            if ( isFinish )
            {
               SELF_RELEASE( pArea );
               s_WaNums[ s_uiCurrArea ] = 0;
               s_pCurrArea = NULL;
            }
            else if( pArea->uiParents )
            {
               isParents = TRUE;
            }
            else
            {
               SELF_CLOSE( pArea );
            }
         }
         if( !isParents && !isFinish )
         {
            isParents = isFinish = TRUE;
         }
      }

      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
      HB_SET_WA( 1 );

      UNLOCK_AREA
   }
}


/*
 * -- FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

/*
 * Shutdown the RDD system.
 */
void HB_EXPORT hb_rddShutDown( void )
{
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));

   hb_rddCloseAll();

   s_szDefDriver[ 0 ] = '\0';

   if ( s_uiRddMax > 0 )
   {
      for ( uiCount = 0; uiCount < s_uiRddMax; uiCount++ )
      {
         if ( s_RddList[ uiCount ]->pTable.exit != NULL )
         {
           SELF_EXIT( s_RddList[ uiCount ] );
         }
         hb_xfree( s_RddList[ uiCount ] );
      }
      hb_xfree( s_RddList );
      s_RddList = NULL;

      LOCK_AREA_DESTROY
   }
}

/*
 * Insert the new WorkArea node
 */
USHORT HB_EXPORT hb_rddInsertAreaNode( char *szDriver )
{
   HB_THREAD_STUB

   USHORT uiRddID, uiWaPos;
   LPRDDNODE pRddNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInsertAreaNode(%s)", szDriver));

   pRddNode = hb_rddFindNode( szDriver, &uiRddID );

   if( !pRddNode )
   {
      return FALSE;
   }

   LOCK_AREA

   if ( s_uiCurrArea == 0 )
   {
      USHORT uiArea = 1;
      while ( uiArea < s_uiWaNumMax )
      {
         if ( s_WaNums[ uiArea ] == 0 )
            break;
         uiArea++;
      }
      HB_SET_WA( uiArea );
   }

   if ( s_uiCurrArea >= s_uiWaNumMax )
   {
      int iSize = ( ( s_uiCurrArea + 256 ) >> 8 ) << 8;

      if ( s_uiWaNumMax == 0 )
      {
         s_WaNums = (USHORT *) hb_xgrab( iSize * sizeof(USHORT) );
      }
      else
      {
         s_WaNums = (USHORT *) hb_xrealloc( s_WaNums, iSize * sizeof(USHORT) );
      }
      memset( &s_WaNums[ s_uiWaNumMax ], 0, ( iSize - s_uiWaNumMax ) * sizeof(USHORT) );
      s_uiWaNumMax = iSize;
   }

   if ( s_uiWaSpace == 0 )
   {
      s_uiWaSpace = 256;
      s_WaList = (AREAP *) hb_xgrab( s_uiWaSpace * sizeof(AREAP) );
      memset( &s_WaList[ 0 ], 0, s_uiWaSpace * sizeof(AREAP) );
      s_WaList[ 0 ] = NULL;
      uiWaPos = 1;
      s_uiWaMax = 2;
   }
   else
   {
      uiWaPos = s_uiWaMax++;
      if ( s_uiWaMax > s_uiWaSpace )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = (AREAP *) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof(AREAP) );
         memset( &s_WaList[ s_uiWaMax ], 0, ( s_uiWaSpace - s_uiWaMax ) * sizeof(AREAP) );
      }
      while ( uiWaPos > 1 )
      {
         if ( s_WaList[ uiWaPos - 1 ]->uiArea < s_uiCurrArea )
            break;
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos - 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos--;
      }
   }
   s_WaList[ uiWaPos ] = hb_rddNewAreaNode( pRddNode, uiRddID );
   s_WaNums[ s_uiCurrArea ] = uiWaPos;
   s_WaList[ uiWaPos ]->uiArea = s_uiCurrArea;
   s_pCurrArea = s_WaList[ uiWaPos ];

   UNLOCK_AREA

   return TRUE;
}

/*
 * Return the current WorkArea number.
 */
int HB_EXPORT hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return s_uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
ERRCODE HB_EXPORT hb_rddSelectWorkAreaNumber( int iArea )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

   LOCK_AREA
   HB_SET_WA( iArea );
   UNLOCK_AREA

   return ( HB_CURRENT_WA == NULL ) ? FAILURE : SUCCESS;
}

/*
 * Select a WorkArea by the symbol name.
 */
ERRCODE HB_EXPORT hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   ERRCODE bResult;
   char * szName;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   if( pSymAlias->pDynSym->hArea )
   {
      bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
   }
   else
   {
      szName = pSymAlias->pDynSym->pSymbol->szName;

      if( strlen( szName ) == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
      {
         bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
      }
      else
      {
         /*
          * generate an error with retry possibility
          * (user created error handler can open a missing database)
          */
         USHORT uiAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, pSymAlias->szName, 0, EF_CANRETRY );

         bResult = FAILURE;
         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               if( pSymAlias->pDynSym->hArea )
               {
                  bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
                  uiAction = E_DEFAULT;
               }
            }
         }

         hb_itemRelease( pError );
      }
   }

   return bResult;
}

/*
 * Select a WorkArea by the name.
 */
ERRCODE HB_EXPORT hb_rddSelectWorkAreaAlias( char * szName )
{
   ERRCODE bResult;
   ULONG ulLen;
   PHB_DYNS pSymArea;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szName));

   ulLen = strlen( szName );

   if( ulLen >= 1 && toupper( szName[ 0 ] ) > '0' && toupper( szName[ 0 ] ) <= '9' )
   {
      bResult = hb_rddSelectWorkAreaNumber( atoi( szName ) );
   }
   else if( ulLen == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
   {
      bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
   }
   else
   {
      pSymArea = hb_dynsymFindName( szName );

      if( pSymArea && pSymArea->hArea )
      {
         bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
      }
      else
      {
         /*
          * generate an error with retry possibility
          * (user created error handler can open a missing database)
          */
         uiAction = E_RETRY;
         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, szName, 0, EF_CANRETRY );

         bResult = FAILURE;

         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               pSymArea = hb_dynsymFindName( szName );

               if( pSymArea && pSymArea->hArea )
               {
                  bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
                  uiAction = E_DEFAULT;
               }
            }
         }

         hb_itemRelease( pError );
      }
   }

   return bResult;
}

/*
 *  Function for getting current workarea pointer
 */
void HB_EXPORT * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaPointer()"));

   return HB_CURRENT_WA;
}

/*
 * Find a field. ###
 */
USHORT hb_rddFieldIndex( AREAP pArea, char * szName )
{
   USHORT uiCount;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldIndex(%s)", szName));

   uiCount = 0;
   pField = pArea->lpFields;

   while( pField )
   {
      ++uiCount;
      if( strcmp( szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName ) == 0 )
         return uiCount;
      pField = pField->lpfNext;
   }
   return 0;
}

/*
 * call a pCallBack function with all open workareas ###
 */
ERRCODE HB_EXPORT hb_rddIterateWorkAreas ( WACALLBACK pCallBack, int data )
{
   USHORT uiIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddIterateWorkAreas(%p)", pCallBack));

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
   {
      if ( ! (*pCallBack)( s_WaList[ uiIndex ], data ) )
      {
         break;
      }
   }
   UNLOCK_AREA
   return SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
ERRCODE HB_EXPORT hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_THREAD_STUB
   LPFIELD pField;
   USHORT uiField;
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   if( pArea )
   {
      uiField = 1;
      pField = pArea->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            return SELF_GETVALUE( pArea, uiField, pItem );
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

/*
 * Assign a value to a field.
 */
ERRCODE HB_EXPORT hb_rddFieldPut( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_THREAD_STUB
   LPFIELD pField;
   USHORT uiField;
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   if( pArea )
   {
      uiField = 1;
      pField = pArea->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            return SELF_PUTVALUE( pArea, uiField, pItem );
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

/*
 * Obtain the current value of a field.
 */
ERRCODE HB_EXPORT hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );

   if( bSuccess == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      uiAction = E_RETRY;
      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR, NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );

            if( bSuccess == SUCCESS )
            {
               uiAction = E_DEFAULT;
            }
         }
      }
      hb_itemRelease( pError );
   }
   return bSuccess;
}

/*
 * Assign a value to a field.
 */
ERRCODE HB_EXPORT hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );

   if( bSuccess == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      uiAction = E_RETRY;
      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                             NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );

            if( bSuccess == SUCCESS )
            {
               uiAction = E_DEFAULT;
            }
         }
      }
      hb_itemRelease( pError );
   }
   return bSuccess;
}

/*
 * -- END OF FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

/*
 * -- BASIC RDD METHODS --
 */

/*
 * -- HARBOUR FUNCTIONS --
 */

HB_FUNC( AFIELDS )
{
   HB_THREAD_STUB

   PHB_ITEM pName, pType, pLen, pDec;
   HB_ITEM Item;
   USHORT uiFields, uiArrayLen, uiCount;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_retni( 0 );
      return;
   }

   pName = hb_param( 1, HB_IT_ARRAY );
   pType = hb_param( 2, HB_IT_ARRAY );
   pLen = hb_param( 3, HB_IT_ARRAY );
   pDec = hb_param( 4, HB_IT_ARRAY );
   if( !pName && !pType && !pLen && !pDec )
   {
      hb_retni( 0 );
      return;
   }

   uiArrayLen = 0;
   Item.type = HB_IT_NIL;
   SELF_FIELDCOUNT( pArea, &uiFields );
   if( pName )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pName );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_NAME, &Item );
         hb_arraySetForward( pName, uiCount, &Item );
      }
   }
   if( pType )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pType );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, &Item );
         hb_arraySetForward( pType, uiCount, &Item );
      }
   }
   if( pLen )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_LEN, &Item );
         hb_arraySetForward( pLen, uiCount, &Item );
      }
   }
   if( pDec )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_DEC, &Item );
         hb_arraySetForward( pDec, uiCount, &Item );
      }
   }

   hb_retni( uiArrayLen );
}

HB_FUNC( ALIAS )
{
   HB_THREAD_STUB
   USHORT uiArea;
   AREAP pArea;

   uiArea = hb_parni( 1 );
   pArea = uiArea ? HB_GET_WA( uiArea ) : HB_CURRENT_WA;
   if( pArea && pArea->atomAlias && ( ( PHB_DYNS ) pArea->atomAlias )->hArea )
   {
      char * szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
      SELF_ALIAS( pArea, ( BYTE * ) szAlias );
      hb_retcAdopt( szAlias );
      return;
   }
   hb_retc( NULL );
}

HB_FUNC( DBEVAL )
{
   HB_THREAD_STUB

   DBEVALINFO pEvalInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pEvalInfo.itmBlock = hb_param( 1, HB_IT_BLOCK );
      if( !pEvalInfo.itmBlock )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = hb_param( 2, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobFor && !ISNIL( 2 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobWhile = hb_param( 3, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobWhile && !ISNIL( 3 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.lNext = hb_param( 4, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.lNext && !ISNIL( 4 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmRecID = hb_param( 5, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.itmRecID && !ISNIL( 5 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.fRest = hb_param( 6, HB_IT_LOGICAL );
      if( !pEvalInfo.dbsci.fRest && !ISNIL( 6 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      SELF_DBEVAL( pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBEVAL" );
}

HB_FUNC( DBF )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea->atomAlias && ( ( PHB_DYNS ) pArea->atomAlias )->hArea )
   {
      char * szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
      SELF_ALIAS( pArea, ( BYTE * ) szAlias );
      hb_retcAdopt( szAlias );
      return;
   }
   hb_retc( NULL );
}

HB_FUNC( BOF )
{
   HB_THREAD_STUB
   BOOL bBof = TRUE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_BOF( pArea, &bBof );

   hb_retl( bBof );
}

HB_FUNC( DBAPPEND )
{
   HB_THREAD_STUB
   BOOL bUnLockAll;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      bUnLockAll = ISLOG( 1 ) ? hb_parl( 1 ) : TRUE;
      s_bNetError = FALSE;
      if( SELF_APPEND( pArea, bUnLockAll ) == FAILURE )
      {
         s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPPEND" );
}

HB_FUNC( DBCLEARFILTER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_CLEARFILTER( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCLEARFILTER" );
}

HB_FUNC( DBCLOSEALL )
{
   hb_rddCloseAll();
}

HB_FUNC( DBCLOSEAREA )
{
   HB_THREAD_STUB

   if( HB_CURRENT_WA )
   {
      hb_rddReleaseCurrentArea();
   }
}

HB_FUNC( DBCOMMIT )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_FLUSH( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCOMMIT" );
}

HB_FUNC( DBCOMMITALL )
{
   HB_THREAD_STUB
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_FLUSH( HB_CURRENT_WA );
   }
   UNLOCK_AREA
   hb_rddSelectWorkAreaNumber( uiArea );
}

HB_FUNC( __DBCONTINUE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCONTINUE" );
      return;
   }

   if( !pArea->dbsi.itmCobFor )
      return;

   pArea->fFound = FALSE;
   SELF_SKIP( pArea, 1 );
   if( pArea->fEof )
      return;

   pArea->fFound = hb_itemGetL( hb_vmEvalBlock( pArea->dbsi.itmCobFor ) );
   while( !pArea->fEof && !pArea->fFound )
   {
      SELF_SKIP( pArea, 1 );
      pArea->fFound = hb_itemGetL( hb_vmEvalBlock( pArea->dbsi.itmCobFor ) );
   }
}

HB_FUNC( DBCREATE )
{
   HB_THREAD_STUB
   char * szDriver;
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   char szAliasTmp[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   char szFileName[ _POSIX_PATH_MAX + 1 ], szSavedFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiSize, uiLen, uiPrevArea;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pStruct, pFieldDesc;
   BOOL bOpen, bCurr;
   BYTE * codePageId = (BYTE*) hb_parc(6);
   AREAP pArea;

   hb_ret();

   s_bNetError = FALSE;

   szFileName[0] = szFileName[_POSIX_PATH_MAX] = '\0';
   if ( ISCHAR( 1 ) )
   {
      strncpy( szFileName, hb_parcx( 1 ), _POSIX_PATH_MAX );
   }
   pStruct = hb_param( 2 , HB_IT_ARRAY );

   if( pStruct )
   {
      uiLen = ( USHORT ) hb_arrayLen( pStruct );
   }
   else
   {
      uiLen = 0;
   }

   if( ( strlen( szFileName ) == 0 ) || uiLen == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
      //hb_errRT_BASE( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
      //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
      return;
   }

   for( uiSize = 0; uiSize < uiLen; uiSize++ )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize + 1 );

      if( hb_arrayLen( pFieldDesc ) < 4 )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
         //hb_errRT_BASE( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );

         return;
      }

      /* Validate items types of fields */
      if( !( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          !( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
         //hb_errRT_BASE( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
         return;
      }
   }

   uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

   uiLen = ( USHORT ) hb_parclen( 3 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }

      hb_strncpyUpper( szDriverBuffer, hb_parc( 3 ), uiLen );
      szDriver = szDriverBuffer;
   }
   else
   {
      szDriver = s_szDefDriver;
   }

   pFileName = hb_fsFNameSplit( szFileName );

   szAlias[0] = szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH ] = '\0';
   if( ISCHAR(5) )
   {
      strncat( szAlias, hb_parc( 5 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   }

   uiLen = strlen( szAlias );

   if( uiLen == 0 )
   {
      strncat( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
   }
   else if( uiLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_xfree( pFileName );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
         //hb_errRT_BASE( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
         return;
      }
   }

   if( hb_rddGetTempAlias( szAliasTmp ) )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBCREATE" );
      //hb_errRT_BASE( EG_ARG, EDBCMD_DUPALIAS, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
      //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
      return;
   }
   if( !ISLOG( 4 ) )
   {
      bOpen = bCurr = FALSE;
      /*
       * 0 means chose first available in hb_rddInsertAreaNode()
       * This hack is necessary to avoid race condition in MT
       * if we don't want to lock whole RDD subsystem, Druzus
       */
      hb_rddSelectWorkAreaNumber( 0 );
   }
   else
   {
      bOpen = TRUE;
      if( hb_parl( 4 ) )
      {
         bCurr = FALSE;
         /* see note above */
         hb_rddSelectWorkAreaNumber( 0 );
      }
      else
      {
         bCurr = TRUE;
         if( HB_CURRENT_WA )       /* If current WorkArea is used then close it */
         {
            hb_rddReleaseCurrentArea();
         }
      }
   }

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_CREATE, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      //hb_errRT_BASE( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
      //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
      return;
   }
   pArea = HB_CURRENT_WA;

   if( ! pFileName->szExtension )
   {
      HB_ITEM extItm;
      extItm.type = HB_IT_NIL;
      SELF_INFO( pArea, DBI_TABLEEXT, &extItm );
      if( HB_IS_STRING( &extItm ) )
      {
         strncat( szFileName, extItm.item.asString.value, _POSIX_PATH_MAX - strlen( szFileName ) );
      }
      hb_itemClear( &extItm );
   }

   hb_xfree( pFileName );

   /* Save filename for later use */
   strcpy( szSavedFileName, szFileName );

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAliasTmp;
   pInfo.fShared = FALSE;
   pInfo.fReadonly = FALSE;
   pInfo.cdpId = codePageId;

   // pArea->atomAlias = hb_dynsymGet( ( char * ) szAlias );
   pArea->atomAlias = hb_dynsymGet( ( char * ) szAliasTmp );
   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = pArea->uiArea;

   if( SELF_CREATEFIELDS( pArea, pStruct ) == FAILURE )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
      hb_errRT_DBCMD( EG_CREATE, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      //hb_errRT_BASE( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
      //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
      return;
   }

   if( SELF_CREATE( pArea, &pInfo ) == FAILURE )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
      hb_errRT_DBCMD( EG_CREATE, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      //hb_errRT_BASE( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE", 6, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
      //               hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
      return;
   }

   hb_rddReleaseCurrentArea();

   if( ! bOpen )
   {
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }
   else
   {
      if ( !bCurr )
      {
         /*
          * 0 means chose first available in hb_rddInsertAreaNode()
          * This hack is necessary to avoid race condition in MT
          * if we don't want to lock whole RDD subsystem, Druzus
          */
         hb_rddSelectWorkAreaNumber( 0 );
      }
      bOpen = FALSE;
      if( hb_rddInsertAreaNode( szDriver ) )
      {
         pArea = HB_CURRENT_WA;

         pInfo.uiArea = pArea->uiArea;
         pInfo.abName = ( BYTE * ) szSavedFileName;
         pInfo.atomAlias = ( BYTE * ) szAlias;
         pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
         pInfo.fReadonly = FALSE;
         pInfo.cdpId = codePageId;

         bOpen = ( SELF_OPEN( pArea, &pInfo ) == SUCCESS );
         if ( !bOpen )
         {
            hb_rddReleaseCurrentArea();
         }
      }
      if ( !bOpen )
      {
         s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      }
      hb_retl( bOpen );
   }
}

HB_FUNC( DBDELETE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      SELF_DELETE( pArea );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBDELETE" );
   }
}

HB_FUNC( DBFILTER )
{
   HB_THREAD_STUB
   HB_ITEM Filter;
   AREAP pArea = HB_CURRENT_WA;

   Filter.type = HB_IT_NIL;

   if( pArea )
   {
      hb_itemPutC( &Filter, "" );
      SELF_FILTERTEXT( pArea, &Filter );
      hb_retc( (&Filter)->item.asString.value );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( DBGOBOTTOM )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_GOBOTTOM( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOBOTTOM" );
}

HB_FUNC( DBGOTO )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTO" );
      return;
   }

   pItem = hb_param( 1, HB_IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, "DBGOTO" );
   else
      SELF_GOTOID( pArea, pItem );
   hb_ret();
}

HB_FUNC( DBGOTOP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_GOTOP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTOP" );
}

HB_FUNC( __DBLOCATE )
{
   HB_THREAD_STUB
   PHB_ITEM pFor, pNewFor, pWhile, pNext, pRecord, pRest, pNewRest;
   DBSCOPEINFO pScopeInfo;
   ULONG lNext;
   BOOL bFor, bWhile;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, "__DBLOCATE" );
      return;
   }

   pArea->fFound = FALSE;
   memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
   pFor     = hb_param( 1, HB_IT_BLOCK );
   pWhile   = hb_param( 2, HB_IT_BLOCK );
   pNext    = hb_param( 3, HB_IT_NUMERIC );
   pRecord  = hb_param( 4, HB_IT_NUMERIC );
   pRest    = hb_param( 5, HB_IT_LOGICAL );
   pNewRest = NULL;

   if( pWhile )
   {
      pNewRest = hb_itemPutL( pNewRest, TRUE );
      pScopeInfo.fRest = pNewRest;
   }

   if( !pFor )
   {
      pNewFor = hb_itemPutL( NULL, TRUE );
   }
   else
   {
      pNewFor = hb_itemNew( pFor );
   }

   pScopeInfo.itmCobFor = pNewFor;

   if( !pRest )
   {
      pNewRest = hb_itemPutL( pNewRest, FALSE );
      pScopeInfo.fRest = pNewRest;
   }

   SELF_SETLOCATE( pArea, &pScopeInfo );
   pArea->fFound = FALSE;

   if( pRecord )
   {
      SELF_GOTOID( pArea, pRecord );

      if( pArea->fEof )
      {
         goto ExitLocate ;
      }

      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
      {
         bWhile = TRUE;
      }

      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
         pArea->fFound = ( bWhile && bFor );
      }
      else
      {
         pArea->fFound = ( bWhile && hb_itemGetL( pNewFor ) );
      }
   }
   else if( pWhile )
   {
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
      {
         bWhile = TRUE;
      }

      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      }
      else
      {
         bFor = hb_itemGetL( pNewFor );
      }

      if( pNext )
         lNext = hb_parnl( 3 );
      else
         lNext = 0xffffffffu;  /* maxed out */

      while( !pArea->fEof && lNext-- != 0 && bWhile && !bFor )
      {
         SELF_SKIP( pArea, 1 );

         if( pArea->fEof )
         {
            bFor = FALSE;
         }
         else
         {
            if( hb_itemType( pWhile ) == HB_IT_BLOCK )
            {
               bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
            }
            else
            {
               bWhile = TRUE;
            }

            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
            {
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            }
            else
            {
               bFor = hb_itemGetL( pNewFor );
            }
         }
      }
      pArea->fFound = bFor;
   }
   else if( pNext )
   {
      lNext = hb_parnl( 3 );

      if( pArea->fEof || lNext <= 0 )
      {
         goto ExitLocate ;
      }

      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
      {
         bWhile = TRUE;
      }

      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      }
      else
      {
         bFor = hb_itemGetL( pNewFor );
      }

      while( !pArea->fEof && lNext-- > 0 && bWhile && !bFor )
      {
         SELF_SKIP( pArea, 1 );

         if( pArea->fEof  )
         {
            bFor = FALSE;
         }
         else
         {
            if( hb_itemType( pWhile ) == HB_IT_BLOCK )
            {
               bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
            }
            else
            {
               bWhile = TRUE;
            }

            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
            {
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            }
            else
            {
               bFor = hb_itemGetL( pNewFor );
            }
         }
      }
      pArea->fFound = bFor;
   }
   else if( hb_itemGetL( pRest ) )
   {
      if( pArea->fEof )
      {
         goto ExitLocate ;
      }
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
      {
         bWhile = TRUE;
      }

      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      }
      else
      {
         bFor = hb_itemGetL( pNewFor );
      }

      while( !pArea->fEof && bWhile && !bFor )
      {
         SELF_SKIP( pArea, 1 );

         if( pArea->fEof  )
         {
            bFor = FALSE;
         }
         else
         {
            if( hb_itemType( pWhile ) == HB_IT_BLOCK )
            {
               bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
            }
            else
            {
               bWhile = TRUE;
            }

            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
            {
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            }
            else
            {
               bFor = hb_itemGetL( pNewFor );
            }
         }
      }

      pArea->fFound = bFor;
   }
   else
   {
      SELF_GOTOP( pArea );

      if( pArea->fEof )
      {
         goto ExitLocate ;
      }

      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      }
      else
      {
         bFor = hb_itemGetL( pNewFor );
      }

      while( !pArea->fEof && !bFor )
      {
         SELF_SKIP( pArea, 1 );

         if( pArea->fEof )
         {
            bFor = FALSE;
         }
         else
         {
            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
            {
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            }
            else
            {
               bFor = hb_itemGetL( pNewFor );
            }
         }
      }

      pArea->fFound = bFor;
   }

ExitLocate :
   /* Release items */
   hb_itemRelease( pNewFor );
   hb_itemRelease( pNewRest );
}

HB_FUNC( __DBSETLOCATE )
{
   HB_THREAD_STUB
   PHB_ITEM pLocate;
   DBSCOPEINFO pScopeInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pLocate = hb_param( 1, HB_IT_BLOCK );
      if( pLocate )
      {
         memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
         pScopeInfo.itmCobFor = pLocate;
         SELF_SETLOCATE( pArea, &pScopeInfo );
      }
   }
}

HB_FUNC( __DBPACK )
{
   HB_THREAD_STUB
   PHB_ITEM pBlock, pEvery;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      /*
       * Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
       * Code Block to execute for every record.
       */
      pBlock = hb_param( 1, HB_IT_BLOCK );
      if( pBlock )
      {
         hb_itemRelease( pArea->valResult );
         pArea->valResult = hb_itemArrayNew( 2 );
         hb_arraySet( pArea->valResult, 1, pBlock );
         pEvery = hb_param( 2, HB_IT_ANY );
         if( pEvery && HB_IS_NUMERIC( pEvery ) )
            hb_arraySet( pArea->valResult, 2, pEvery );
      }
      else
      {
         if ( pArea->valResult )
            hb_itemClear( pArea->valResult );
         else
            pArea->valResult = hb_itemNew( NULL );
      }
      SELF_PACK( pArea );
      if( pBlock )
         hb_itemClear( pArea->valResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBPACK" );
}

HB_FUNC( DBRECALL )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_RECALL( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECALL" );
}

HB_FUNC( DBRLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = hb_param( 1, HB_IT_ANY );
      if( !dbLockInfo.itmRecID || !HB_IS_NUMERIC( dbLockInfo.itmRecID ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( DBRLOCKLIST )
{
   HB_THREAD_STUB
   HB_ITEM List;
   AREAP pArea = HB_CURRENT_WA;

   List.type = HB_IT_NIL;
   hb_arrayNew( &List, 0 );
   if( pArea )
      SELF_INFO( pArea, DBI_GETLOCKARRAY, &List );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCKLIST" );

   hb_itemReturn( &List );
}

HB_FUNC( DBRUNLOCK )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_UNLOCK( pArea, hb_parnl( 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRUNLOCK" );
}

HB_FUNC( DBSEEK )
{
   HB_THREAD_STUB
   PHB_ITEM pKey;
   BOOL bSoftSeek, bFindLast;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if( !ISNIL( 1 ) )
      {
         pKey = hb_param( 1, HB_IT_ANY );
         bSoftSeek = ISLOG( 2 ) ? hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
         if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            hb_retl( pArea->fFound );
            return;
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, "DBSEEK" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSEEK" );

   hb_retl( FALSE );
}

HB_FUNC( DBSELECTAREA )
{
   LONG ulNewArea;

   if( ISCHAR( 1 ) )
   {
      char *szAlias = hb_parcx( 1 );
      USHORT ulLen = strlen( szAlias );

      if( ulLen >= 1 && szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
      {
         ulNewArea = atoi( szAlias );
      }
      else if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
      {
         ulNewArea = toupper( szAlias[ 0 ] ) - 'A' + 1;
      }
      else if( ulLen == 1 && toupper( szAlias[ 0 ] ) == 'M' )
      {
         ulNewArea = 0;
      }
      else
      {
         hb_rddSelectWorkAreaAlias( szAlias );
         ulNewArea = hb_rddGetCurrentWorkAreaNumber();
      }
   }
   else
   {
      ulNewArea = hb_parnl( 1 );
   }

   if( ulNewArea < 1 || ulNewArea > HARBOUR_MAX_RDD_AREA_NUM )
   {
      hb_rddSelectFirstAvailable();
   }
   else
   {
      hb_rddSelectWorkAreaNumber( ulNewArea );
   }
}

HB_FUNC( __DBSETFOUND )
{
   HB_THREAD_STUB
   PHB_ITEM pFound;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pFound = hb_param( 1, HB_IT_LOGICAL );
      if( pFound )
         pArea->fFound = hb_itemGetL( pFound );
   }
}

HB_FUNC( DBSETFILTER )
{
   HB_THREAD_STUB
   PHB_ITEM pBlock, pText;
   DBFILTERINFO pFilterInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pBlock = hb_param( 1, HB_IT_BLOCK );
      if( pBlock )
      {
         pText = hb_param( 2, HB_IT_STRING );
         pFilterInfo.itmCobExpr = pBlock;
         if( pText )
            pFilterInfo.abFilterText = pText;
         else
            pFilterInfo.abFilterText = hb_itemPutC( NULL, "" );
         SELF_SETFILTER( pArea, &pFilterInfo );
         if( !pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
      else
      {
         SELF_CLEARFILTER( pArea );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETFILTER" );
}

HB_FUNC( DBSKIP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_SKIP( pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIP" );
}

static void hb_dbfStructure( PHB_ITEM pStruct )
{
   HB_THREAD_STUB

   HB_ITEM Item, Data;
   USHORT uiFields, uiCount;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      SELF_FIELDCOUNT( pArea, &uiFields );

      Data.type = HB_IT_NIL;
      Item.type = HB_IT_NIL;

      hb_arrayNew( pStruct, 0 );

      for( uiCount = 1; uiCount <= uiFields; uiCount++ )
      {
         hb_arrayNew( &Item, 4 );

         SELF_FIELDINFO( pArea, uiCount, DBS_NAME, &Data );
         hb_arraySetForward( &Item, 1, &Data );

         SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, &Data );
         hb_arraySetForward( &Item, 2, &Data );

         SELF_FIELDINFO( pArea, uiCount, DBS_LEN, &Data );
         hb_arraySetForward( &Item, 3, &Data );

         SELF_FIELDINFO( pArea, uiCount, DBS_DEC, &Data );
         hb_arraySetForward( &Item, 4, &Data );

         hb_arrayAddForward( pStruct, &Item );
      }
   }
}

HB_FUNC( DBSTRUCT )
{
   HB_ITEM Struct;

   Struct.type = HB_IT_NIL;

   hb_dbfStructure( &Struct );
   hb_itemReturn( &Struct );
}

HB_FUNC( DBTABLEEXT )
{
   HB_THREAD_STUB
   HB_ITEM Item;
   AREAP pArea = HB_CURRENT_WA;

   Item.type = HB_IT_NIL;
   hb_itemPutC( &Item, "" );

   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      hb_rddCheck();
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
      if( pRddNode )
      {
         pArea = hb_rddNewAreaNode( pRddNode, uiRddID );
         if ( pArea )
         {
            SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, &Item );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
   {
      SELF_INFO( pArea, DBI_TABLEEXT, &Item );
   }
   hb_itemReturn( &Item );
}

HB_FUNC( DBUNLOCK )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_UNLOCK( pArea, 0 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBUNLOCK" );
}

HB_FUNC( DBUNLOCKALL )
{
   HB_THREAD_STUB
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_UNLOCK( HB_CURRENT_WA, 0 );
   }
   UNLOCK_AREA
   hb_rddSelectWorkAreaNumber( uiArea );
}

HB_FUNC( DBUSEAREA )
{
   HB_THREAD_STUB
   char * szDriver;
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   BYTE * codePageId = (BYTE*) hb_parcx(7);
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   AREAP pArea;

   s_bNetError = FALSE;

   /* New area? */
   if( ! hb_parl( 1 ) && HB_CURRENT_WA )
   {
      /* If current WorkArea is in use then close it */
      hb_rddReleaseCurrentArea();
   }

   uiLen = ( USHORT ) hb_parclen( 2 );

   if( ISCHAR(2) && ( uiLen > 0 ) )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }
      hb_strncpyUpper( szDriverBuffer, hb_parcx( 2 ), uiLen );
      szDriver = szDriverBuffer;
   }
   else
   {
      szDriver = s_szDefDriver;
   }

   if( ! ISCHAR(3) || ( strlen( hb_parcx( 3 ) ) == 0 ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }

   szFileName[ _POSIX_PATH_MAX ] = '\0';
   strncpy( szFileName, hb_parcx( 3 ), _POSIX_PATH_MAX );
   /* Convert FileName accoring to Sets (_SET_DIRCASE,_SET_FILECASE,_SET_DIRSEPARATOR) */
   hb_fileNameConv( szFileName );

   pFileName = hb_fsFNameSplit( szFileName );

   szAlias[ 0 ] = szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH ] = '\0';

   if( ISCHAR(4) )
   {
      strncat( szAlias, hb_parcx( 4 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   }

   if( strlen( szAlias ) == 0 )
   {
      strncat( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
   }

   uiLen = strlen( szAlias );

   if( szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
      return;
   }

   if( uiLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_xfree( pFileName );
         hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
         return;
      }
   }

   // Verify if the alias is already in use
   if( hb_rddSelect( szAlias ) )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
      return;
   }

   /* New area? */
   if( hb_parl( 1 ) )
   {
      /*
       * 0 means chose first available in hb_rddInsertAreaNode()
       * This hack is necessary to avoid race condition in MT
       * if we don't want to lock whole RDD subsystem, Druzus
       */
      hb_rddSelectWorkAreaNumber( 0 );
   }
   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }
   pArea = HB_CURRENT_WA;

   szFileName[ _POSIX_PATH_MAX ] = '\0';
   strncpy( szFileName, hb_parcx( 3 ), _POSIX_PATH_MAX );

   if( ! pFileName->szExtension )
   {
      HB_ITEM extItm;
      extItm.type = HB_IT_NIL;
      SELF_INFO( pArea, DBI_TABLEEXT, &extItm );
      if( HB_IS_STRING( &extItm ) )
      {
         strncat( szFileName, extItm.item.asString.value, _POSIX_PATH_MAX - strlen( szFileName ) );
      }
      hb_itemClear( &extItm );
   }
   hb_xfree( pFileName );

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;
   pInfo.cdpId = codePageId;

   /* Open file */
   if( SELF_OPEN( pArea, &pInfo ) == FAILURE )
   {
      s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      hb_rddReleaseCurrentArea();
      return;
   }
}

HB_FUNC( __DBZAP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ZAP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBZAP" );
}

HB_FUNC( DELETED )
{
   HB_THREAD_STUB
   BOOL bDeleted = FALSE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_DELETED( pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   HB_THREAD_STUB
   BOOL bEof = TRUE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_EOF( pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   HB_THREAD_STUB
   USHORT uiFields = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_FIELDCOUNT( pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDDEC )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;

      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         HB_ITEM Item;
         Item.type = HB_IT_NIL;

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_DEC, &Item ) == SUCCESS)
         {
            hb_itemForwardValue( &(HB_VM_STACK).Return, &Item );
            return;
         }
      }
   }

   hb_retni(0);
}

HB_FUNC( FIELDGET )
{
   HB_THREAD_STUB
   HB_ITEM Item;
   USHORT uiField, uiFields;
   AREAP pArea = HB_CURRENT_WA;

   Item.type = HB_IT_NIL;
   uiField = hb_parni( 1 );

   if( pArea && uiField )
   {
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiField > 0 && uiField <= uiFields )
         SELF_GETVALUE( pArea, uiField, &Item );
   }

   hb_itemForwardValue( &(HB_VM_STACK).Return, &Item );
}

HB_FUNC( FIELDLEN )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;
      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         HB_ITEM Item;
         Item.type = HB_IT_NIL;
         if( SELF_FIELDINFO( pArea, uiIndex, DBS_LEN, &Item ) == SUCCESS )
         {
            hb_itemForwardValue( &(HB_VM_STACK.Return), &Item );
            return;
         }
      }
   }

   hb_retni(0);
}

HB_FUNC( FIELDNAME )
{
   HB_THREAD_STUB
   char * szName;
   USHORT uiFields, uiIndex;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      uiIndex = hb_parni( 1 );
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
         SELF_FIELDNAME( pArea, hb_parni( 1 ), szName );
         hb_retcAdopt( szName );
         return;
      }
      /* This is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com>
       *
      hb_errRT_DBCMD( EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, NULL, "FIELDNAME" );
      */
   }
   hb_retc( "" ); /* Was NULL, which is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com> */
}

HB_FUNC( FIELDPOS )
{
   HB_THREAD_STUB
   /* char szName[ HARBOUR_MAX_RDD_FIELDNAME_LENGTH ]; */
   char * szName;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      char *szFieldName = hb_parcx( 1 );

      if( szFieldName )
      {
         int iLen = hb_parclen( 1 );

         if( iLen > (int) pArea->uiMaxFieldNameLength )
         {
            iLen = (int) pArea->uiMaxFieldNameLength;
         }

         szName = ( char * ) hb_xgrab( iLen + 1 );

         hb_strncpyUpperTrim( szName, szFieldName, iLen );

         hb_retni( hb_rddFieldIndex( pArea, szName ) );

         hb_xfree( szName );

         return;
      }
   }

   hb_retni( 0 );
}

HB_FUNC( FIELDPUT )
{
   HB_THREAD_STUB
   USHORT uiIndex;
   AREAP pArea = HB_CURRENT_WA;

   uiIndex = hb_parni( 1 );
   if( pArea && uiIndex )
   {
      PHB_ITEM pItem = hb_param( 2, HB_IT_ANY );
      if( SELF_PUTVALUE( pArea, uiIndex, pItem ) == SUCCESS )
      {
         hb_itemReturn( pItem );
      }
   }
}

HB_FUNC( FIELDTYPE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;

      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         HB_ITEM Item;
         Item.type = HB_IT_NIL;

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_TYPE, &Item ) == SUCCESS )
         {
            hb_itemForwardValue( &(HB_VM_STACK).Return, &Item );
            return;
         }
      }
   }

   hb_retc("");
}

HB_FUNC( FLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "FLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( FOUND )
{
   HB_THREAD_STUB
   BOOL bFound = FALSE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_FOUND( pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
      hb_retni( 0 );
   else
   {
      HB_ITEM RecSize;
      RecSize.type = HB_IT_NIL;
      SELF_INFO( pArea, DBI_GETHEADERSIZE, &RecSize );
      hb_itemForwardValue( &(HB_VM_STACK).Return, &RecSize );
   }
}

HB_FUNC( INDEXORD )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      DBORDERINFO pInfo;
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pInfo.itmOrder = NULL;
      pInfo.atomBagName = NULL;
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

/* Same as RECCOUNT() */
HB_FUNC( LASTREC )
{
   HB_THREAD_STUB
   ULONG ulRecCount = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_RECCOUNT( pArea, &ulRecCount );

   hb_retnl( ulRecCount );
}

HB_FUNC( LOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( LUPDATE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
      hb_itemPutDS( &(HB_VM_STACK.Return), "" );
   else
      SELF_INFO( pArea, DBI_LASTUPDATE, &(HB_VM_STACK.Return) );
}

HB_FUNC( NETERR )
{
   HB_THREAD_STUB
   if( ISLOG( 1 ) )
      s_bNetError = hb_parl( 1 );

   hb_retl( s_bNetError );
}

HB_FUNC( ORDBAGEXT )
{
   HB_THREAD_STUB
   DBORDERINFO pInfo;
   AREAP pArea = HB_CURRENT_WA;

   pInfo.itmOrder = NULL;
   pInfo.atomBagName = NULL;
   pInfo.itmResult = hb_itemPutC( NULL, "" );
   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      hb_rddCheck();
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
      if( pRddNode )
      {
         pArea = hb_rddNewAreaNode( pRddNode, uiRddID );
         if ( pArea )
         {
            SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
   {
      SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
   }
   hb_itemReturn( pInfo.itmResult );
   hb_itemRelease( pInfo.itmResult );
}

HB_FUNC( ORDBAGNAME )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDBAGNAME" );
            return;
         }
      }
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDBAGNAME" );
}

HB_FUNC( ORDCONDSET )
{
   HB_THREAD_STUB
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   char * szFor;
   ULONG ulLen;
   PHB_ITEM pItem;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      lpdbOrdCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
      szFor = hb_parcx( 1 );
      /* ulLen = strlen( szFor ); */
      if( ISCHAR(1) && ( ( ulLen = strlen( szFor ) ) > 0 ) )
      {
         lpdbOrdCondInfo->abFor = ( BYTE * ) hb_xgrab( ulLen + 1 );
         strcpy( ( char * ) lpdbOrdCondInfo->abFor, szFor );
      }
      else
      {
         lpdbOrdCondInfo->abFor = NULL;
      }

      if( ISCHAR( 17 ) && ( ulLen = hb_parclen( 17 ) ) > 0 )
      {
         lpdbOrdCondInfo->abWhile = ( BYTE * ) hb_xgrab( ulLen + 1 );
         strcpy( ( char * ) lpdbOrdCondInfo->abWhile, hb_parcx( 17 ) );
      }
      else
      {
         lpdbOrdCondInfo->abWhile = NULL;
      }

      pItem = hb_param( 2, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobFor = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobFor, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobFor = NULL;
      if( ISLOG( 3 ) )
         lpdbOrdCondInfo->fAll = hb_parl( 3 );
      else
         lpdbOrdCondInfo->fAll = TRUE;

      pItem = hb_param( 4, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobWhile = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobWhile, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobWhile = NULL;

      pItem = hb_param( 5, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobEval = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobEval, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobEval = NULL;

      lpdbOrdCondInfo->lStep       = hb_parnl( 6 );
      lpdbOrdCondInfo->lStartRecno = hb_parnl( 7 );
      lpdbOrdCondInfo->lNextCount  = hb_parnl( 8 );
      lpdbOrdCondInfo->lRecno      = hb_parnl( 9 );
      lpdbOrdCondInfo->fRest       = hb_parl( 10 );
      lpdbOrdCondInfo->fDescending = hb_parl( 11 );
      /* 12th parameter is always nil */
      lpdbOrdCondInfo->fAdditive   = hb_parl( 13 );
      lpdbOrdCondInfo->fUseCurrent = hb_parl( 14 );
      lpdbOrdCondInfo->fCustom     = hb_parl( 15 );
      lpdbOrdCondInfo->fNoOptimize = hb_parl( 16 );

      if( lpdbOrdCondInfo->itmCobWhile )
         lpdbOrdCondInfo->fRest = TRUE;
      if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->lRecno ||
               lpdbOrdCondInfo->fRest || lpdbOrdCondInfo->fUseCurrent )
         lpdbOrdCondInfo->fAll = FALSE;

      hb_retl( SELF_ORDSETCOND( pArea, lpdbOrdCondInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDCREATE )
{
   HB_THREAD_STUB
   DBORDERCREATEINFO dbOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      dbOrderInfo.abBagName = ( BYTE * ) hb_parcx( 1 );
      dbOrderInfo.atomBagName = ( BYTE * ) hb_parcx( 2 );
      dbOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( dbOrderInfo.abBagName == NULL || strlen( ( char * ) dbOrderInfo.abBagName ) == 0 ) &&
            ( dbOrderInfo.atomBagName == NULL || strlen( ( char * ) dbOrderInfo.atomBagName ) == 0 ) ) ||
          !dbOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDCREATE" );
         return;
      }
      dbOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );
      if( ISLOG( 5 ) )
         dbOrderInfo.fUnique = hb_parl( 5 );
      else
         dbOrderInfo.fUnique = hb_set.HB_SET_UNIQUE;
      SELF_ORDCREATE( pArea, &dbOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDCREATE" );
}

HB_FUNC( ORDDESTROY )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      SELF_ORDDESTROY( pArea, &pOrderInfo );
   }
}

HB_FUNC( ORDFOR )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDFOR" );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDFOR" );
}

HB_FUNC( ORDKEY )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDKEY" );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEY" );
}

#ifdef HB_COMPAT_C53
HB_FUNC( ORDKEYCOUNT )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYCOUNT" );

}

HB_FUNC( ORDKEYNO )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = NULL;
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYNO" );
}

HB_FUNC( ORDKEYGOTO )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1 , HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYGOTO" );
}

HB_FUNC( ORDSKIPUNIQUE )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSKIPUNIQUE" );
}

HB_FUNC( ORDKEYVAL )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYVAL, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYVAL" );
}

HB_FUNC( ORDKEYADD )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYADD, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYADD" );
}

HB_FUNC( ORDKEYDEL )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYDELETE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYDEL" );
}

HB_FUNC( ORDDESCEND )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_ISDESC, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDDESCEND" );
}

HB_FUNC( ORDISUNIQUE )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* HARBOUR extension: NewVal to set/reset unique flag */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_UNIQUE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDISUNIQUE" );
}

#endif

HB_FUNC( ORDLISTADD )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      BOOL bFirst;
      /*  determine if there are existing orders; if not, this becomes the controlling order
      */
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pOrderInfo.itmOrder = NULL;
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      bFirst = HB_IS_NUMERIC( pOrderInfo.itmResult ) &&
               hb_itemGetNI( pOrderInfo.itmResult ) == 0;

      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder  = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.atomBagName )
      {
         if ( hb_parinfo(1) != HB_IT_NIL )
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDLISTADD" );
         else
            hb_itemRelease( pOrderInfo.itmResult );
         return;
      }

      if (SELF_ORDLSTADD( pArea, &pOrderInfo ) == SUCCESS )
      {
         if ( bFirst )        /* set as controlling order and go top */
         {
            pOrderInfo.itmOrder = hb_itemPutNI( NULL, 1 );
            SELF_ORDLSTFOCUS( pArea, &pOrderInfo );
            hb_itemRelease( pOrderInfo.itmOrder );
            SELF_GOTOP( pArea );
         }
      }
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTADD" );

}

HB_FUNC( ORDLISTCLEAR )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ORDLSTCLEAR( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDLISTREBUILD )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ORDLSTREBUILD( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTREBUILD" );
}

HB_FUNC( ORDNAME )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNAME" );
         return;
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNAME" );
}

HB_FUNC( ORDNUMBER )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder && ! ISNIL(1))
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNUMBER" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNUMBER" );
}

HB_FUNC( ORDSETFOCUS )
{
   HB_THREAD_STUB
   DBORDERINFO pInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pInfo.itmOrder )
         pInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      hb_itemReturn( pInfo.itmResult );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSETFOCUS" );
}

HB_FUNC( RDDLIST )
{
   HB_THREAD_STUB
   USHORT uiType, uiCount;
   HB_ITEM Name;

   Name.type = HB_IT_NIL;

   hb_rddCheck();
   hb_arrayNew( &(HB_VM_STACK.Return), 0 );
   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   for ( uiCount = 0; uiCount < s_uiRddMax; uiCount++ )
   {
      if( ( uiType == 0 ) || ( s_RddList[ uiCount ]->uiType == uiType ) )
      {
         hb_arrayAddForward( &(HB_VM_STACK.Return), hb_itemPutC( &Name, s_RddList[ uiCount ]->szName ) );
      }
   }
}

HB_FUNC( RDDNAME )
{
   HB_THREAD_STUB
   char * pBuffer;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( pArea, ( BYTE * ) pBuffer );
      hb_retcAdopt( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RDDNAME" );
      hb_retc( NULL );
   }
}

HB_FUNC( RDDREGISTER )
{
   USHORT uiLen;
   char szDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

   hb_rddCheck();

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriver, hb_parcx( 1 ), uiLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) > 1 )
      {
         hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      }
   }
}

/* Same as LASTREC() */
HB_FUNC( RECCOUNT )
{
   HB_FUNCNAME( LASTREC )();
}

HB_FUNC( RECNO )
{
   HB_THREAD_STUB
   HB_ITEM RecNo;
   AREAP pArea = HB_CURRENT_WA;

   RecNo.type = HB_IT_NIL;
   hb_itemPutNL( &RecNo, 0 );
   if( pArea )
      SELF_RECNO( pArea, &RecNo );
   hb_itemReturn( &RecNo );
}

HB_FUNC( RECSIZE )
{
   HB_THREAD_STUB
   HB_ITEM RecSize;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      RecSize.type = HB_IT_NIL;
      SELF_INFO( pArea, DBI_GETRECSIZE, &RecSize );
      hb_itemForwardValue( &(HB_VM_STACK).Return, &RecSize );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( RLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( SELECT )
{
   HB_THREAD_STUB
   char * szAlias;
   ULONG ulLen;

   szAlias = hb_parcx( 1 );
   ulLen = strlen( szAlias );

   if( ulLen == 0 && ISCHAR( 1 ))
   {
      hb_retni( 0 );
   }
   else if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
   {
      hb_retni( toupper( szAlias[ 0 ] ) - 'A' + 1 );
   }
   else if( ulLen > 0 )
   {
      hb_retni( hb_rddSelect( szAlias ) );
   }
   else
   {
      hb_retni( hb_rddGetCurrentWorkAreaNumber() );
   }
}

HB_FUNC( USED )
{
   HB_THREAD_STUB
   hb_retl( HB_CURRENT_WA != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. [vszakats] */

HB_FUNC( __RDDSETDEFAULT )
{
   HB_THREAD_STUB
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }
      hb_strncpyUpper( s_szDefDriver, hb_parcx( 1 ), uiLen );
   }
}

HB_FUNC( RDDSETDEFAULT )
{
   HB_THREAD_STUB

   USHORT uiLen;
   char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }
      hb_strncpyUpper( szNewDriver, hb_parcx( 1 ), uiLen );
      if( ! hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "RDDSETDEFAULT" );
         return;
      }
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( DBSETDRIVER )
{
   HB_THREAD_STUB

   USHORT uiLen;
   char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }
      hb_strncpyUpper( szNewDriver, hb_parcx( 1 ), uiLen );
      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBSETDRIVER" );
         return;
      }
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( ORDSCOPE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if ( pArea )
   {
      DBORDSCOPEINFO sInfo;
      HB_ITEM ScopeValue;

      ScopeValue.type = HB_IT_NIL;
      sInfo.nScope = hb_parni( 1 );

      SELF_SCOPEINFO( pArea, sInfo.nScope, &ScopeValue );

      if ( hb_pcount() > 1 )
      {
         if ( ISNIL( 2 ) )                /* explicitly passed NIL, clear it */
            sInfo.scopeValue = NULL;
         else
            sInfo.scopeValue = hb_param( 2, HB_IT_ANY) ;

         /* rdd must not alter the scopeValue item -- it's not a copy */
         SELF_SETSCOPE( pArea, (LPDBORDSCOPEINFO) &sInfo );

         /* Clipper compatible - I'm not sure it's good to emulate it, Druzus */
         if ( ISNIL( 2 ) )
            hb_itemPutL( &ScopeValue, TRUE );
      }
      hb_itemForwardValue( &(HB_VM_STACK).Return, &ScopeValue );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSCOPE" );
}

HB_FUNC( DBRELATION )  /* (<nRelation>) --> cLinkExp */
{
   HB_THREAD_STUB
   char szExprBuff[ 256 ];  /*TODO: Correct buffer size initialization ??*/
   AREAP pArea = HB_CURRENT_WA;

   szExprBuff[ 0 ] = 0;
   if( pArea )
      SELF_RELTEXT( pArea, hb_parni(1), szExprBuff ) ;

   hb_retc(szExprBuff);
}

HB_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   HB_THREAD_STUB
   USHORT uiWorkArea = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_RELAREA( pArea, hb_parni(1), &uiWorkArea );

   hb_retni( uiWorkArea );
}

HB_FUNC( DBCLEARRELATION )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_CLEARREL( pArea );
}

HB_FUNC( DBSETRELATION )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      DBRELINFO dbRelations;
      AREAP pChildArea;
      USHORT uiChildArea;
      char * szAlias = NULL;

      if( hb_pcount() < 2 || ( !( hb_parinfo( 1 ) & HB_IT_NUMERIC ) && ( hb_parinfo( 1 ) != HB_IT_STRING ) ) || !( ISNIL( 4 ) || ISLOG( 4 ) )  )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "DBSETRELATION" );
         return;
      }

      if( hb_parinfo( 1 ) & HB_IT_NUMERIC )
      {
         uiChildArea = hb_parni( 1 );
      }
      else
      {
         USHORT uiArea = hb_rddGetCurrentWorkAreaNumber();

         szAlias = hb_parcx( 1 );
         hb_rddSelectWorkAreaAlias( szAlias );
         if( hb_vmRequestQuery() )
         {
            return;
         }
         uiChildArea = hb_rddGetCurrentWorkAreaNumber();

         hb_rddSelectWorkAreaNumber( uiArea );
      }

      pChildArea = HB_GET_WA( uiChildArea );

      if( !pChildArea )
      {
         hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
         UNLOCK_AREA
         return;
      }

      dbRelations.lpaChild = pChildArea;
      dbRelations.itmCobExpr = hb_itemNew( hb_param( 2, HB_IT_BLOCK ) );
      dbRelations.abKey = hb_itemNew( hb_param( 3, HB_IT_STRING ) );
      dbRelations.isScoped = ( hb_pcount() > 3 ) ? hb_parl( 4 ) : 0;
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( pArea, &dbRelations );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETRELATION" );
   }
}

HB_FUNC( __DBARRANGE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiNewArea, uiCount;
      ULONG ulSize;
      char * szFieldLine, * szFieldName, * szPos;
      PHB_ITEM pStruct, pFields;
      DBSORTINFO dbSortInfo;

      memset( &dbSortInfo, 0, sizeof( DBSORTINFO ) );
      dbSortInfo.dbtri.uiFlags = DBTF_PUTREC;
      uiNewArea = hb_parni( 1 );

      /* Fields structure of source WorkArea */
      pStruct = hb_param( 2 , HB_IT_ARRAY );
      if( pStruct )
      {
         dbSortInfo.dbtri.uiItemCount = ( USHORT ) hb_arrayLen( pStruct );
         if( dbSortInfo.dbtri.uiItemCount > 0 )
         {
            pFields = hb_itemNew( NULL );
            dbSortInfo.dbtri.lpTransItems = ( LPDBTRANSITEM )
                                            hb_xgrab( dbSortInfo.dbtri.uiItemCount *
                                                      sizeof( DBTRANSITEM ) );
            for( uiCount = 0; uiCount < dbSortInfo.dbtri.uiItemCount; uiCount++ )
            {
               if( hb_arrayGet( pStruct, uiCount + 1, pFields ) && HB_IS_ARRAY( pFields ) &&
                   ( USHORT ) hb_arrayLen( pFields ) > 0 )
               {
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiSource = hb_rddFieldIndex( pArea, hb_arrayGetCPtr( pFields, 1 ) );
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiDest = dbSortInfo.dbtri.lpTransItems[ uiCount ].uiSource;
               }
               else
               {
                  hb_xfree( dbSortInfo.dbtri.lpTransItems );
                  dbSortInfo.dbtri.uiItemCount = 0;
                  break;
               }
            }
            hb_itemRelease( pFields );
         }
      }
      else
         return;

      /* Invalid fields structure? */
      if( dbSortInfo.dbtri.uiItemCount == 0 )
         return;

      dbSortInfo.dbtri.dbsci.itmCobFor = hb_param( 3, HB_IT_BLOCK );
      dbSortInfo.dbtri.dbsci.lpstrFor = NULL;
      dbSortInfo.dbtri.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
      dbSortInfo.dbtri.dbsci.lpstrWhile = NULL;
      dbSortInfo.dbtri.dbsci.lNext = hb_param( 5, HB_IT_NUMERIC );
      dbSortInfo.dbtri.dbsci.itmRecID = hb_param( 6, HB_IT_NUMERIC );
      dbSortInfo.dbtri.dbsci.fRest = hb_param( 7, HB_IT_LOGICAL );
      dbSortInfo.dbtri.dbsci.fIgnoreFilter = dbSortInfo.dbtri.dbsci.fLast =
      dbSortInfo.dbtri.dbsci.fIgnoreDuplicates = FALSE;
      dbSortInfo.dbtri.dbsci.fIncludeDeleted = TRUE;

      pFields = hb_param( 8, HB_IT_ARRAY );
      if( pFields )
         dbSortInfo.uiItemCount = ( USHORT ) hb_arrayLen( pFields );
      else
         dbSortInfo.uiItemCount = 0;
      if( dbSortInfo.uiItemCount > 0 )
      {
         dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
         ulSize = 0;
         for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; uiCount++ )
         {
            if( hb_arrayGetCLen( pFields, uiCount ) > ulSize )
               ulSize = hb_arrayGetCLen( pFields, uiCount );
         }
         szFieldLine = ( char * ) hb_xgrab( ulSize + 1 );
         for( uiCount = 0; uiCount < dbSortInfo.uiItemCount; uiCount++ )
         {
            dbSortInfo.lpdbsItem[ uiCount ].uiFlags = 0;
            hb_strncpyUpper( szFieldLine, hb_arrayGetCPtr( pFields, uiCount + 1 ),
                             hb_arrayGetCLen( pFields, uiCount + 1 ) );
            szPos = strchr( szFieldLine, '/' );
            if( szPos )
            {
               if( * ( szPos + 1 ) == 'D' )
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_DESCEND;
               else if( * ( szPos + 1 ) == 'C' )
               {
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_CASE;
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
               }
               else
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
               if( * ( szPos + 1 ) != 0 && ( ( * ( szPos + 2 ) == 'C' ) ||
                   ( * ( szPos + 2 ) != 0 && * ( szPos + 3 ) == 'C' ) ) )
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_CASE;
               * szPos = 0;
            }
            else
            {
               dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
            }

            szFieldName = szFieldLine;

            while( szFieldName[ 0 ] == ' ' )
            {
               szFieldName++;
            }

            ulSize = strlen( szFieldName );
            while( ulSize > 1 && szFieldName[ ulSize - 1 ] == ' ' )
            {
               ulSize --;
               szFieldName[ ulSize ] = 0;
            }

            dbSortInfo.lpdbsItem[ uiCount ].uiField = hb_rddFieldIndex( pArea, szFieldName );

            /* Field not found */
            if( dbSortInfo.lpdbsItem[ uiCount ].uiField == 0 )
            {
               hb_xfree( dbSortInfo.lpdbsItem );
               dbSortInfo.lpdbsItem = NULL;
               break;
            }
         }
         hb_xfree( szFieldLine );
      }
      else
         return;

      /* Fields not found? */
      if( dbSortInfo.lpdbsItem == NULL )
         return;

      dbSortInfo.dbtri.lpaSource = pArea;
      dbSortInfo.dbtri.lpaDest = NULL;
      dbSortInfo.dbtri.lpaDest = HB_GET_WA( uiNewArea );

      SELF_SORT( pArea, &dbSortInfo );

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.uiItemCount > 0 )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }
}

#ifdef HB_COMPAT_C53

HB_FUNC( DBINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pType, pInfo;
      HB_ITEM Temp;
      Temp.type = HB_IT_NIL;
      pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {
         pInfo = hb_param( 2 , HB_IT_ANY );
         if( pInfo )
         {
            hb_itemCopy( &Temp, pInfo );
         }
         SELF_INFO( pArea, hb_itemGetNI( pType ), &Temp );
         hb_itemForwardValue( &(HB_VM_STACK).Return, &Temp );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBINFOBADPARAMETER, NULL, "DBINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBINFO" );
}

HB_FUNC( DBORDERINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pType;
      DBORDERINFO pOrderInfo;
      BOOL bDeleteItem;
      pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {
         pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
         /* atomBagName may be NIL */
         pOrderInfo.itmOrder = hb_param( 3, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
            pOrderInfo.itmOrder = hb_param( 3, HB_IT_NUMERIC );

         pOrderInfo.itmNewVal = hb_param( 4 , HB_IT_ANY );
         if( !pOrderInfo.itmNewVal )
         {
            pOrderInfo.itmNewVal = hb_itemNew( NULL );
            bDeleteItem = TRUE;
         }
         else
            bDeleteItem = FALSE;
         pOrderInfo.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, hb_itemGetNI( pType ), &pOrderInfo );
         hb_itemReturn( pOrderInfo.itmResult );
         hb_itemRelease( pOrderInfo.itmResult );

         if( bDeleteItem )
            hb_itemRelease( pOrderInfo.itmNewVal );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBORDERINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBORDERINFO" );
}

HB_FUNC( DBFIELDINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiFields, uiIndex;
      PHB_ITEM pType, pInfo;
      HB_ITEM Temp;

      Temp.type = HB_IT_NIL;
      pType = hb_param( 1 , HB_IT_NUMERIC );
      uiIndex = hb_parni( 2 );
      if( pType &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         pInfo = hb_param( 3 , HB_IT_ANY );
         if( pInfo )
         {
            hb_itemCopy( &Temp, pInfo );
         }

         SELF_FIELDINFO( pArea, uiIndex, hb_itemGetNI( pType ), &Temp );
         hb_itemForwardValue( &(HB_VM_STACK).Return, &Temp );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBFIELDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFIELDINFO" );
}

HB_FUNC( DBRECORDINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pType, pRecNo, pInfo;
      HB_ITEM Temp;
      Temp.type = HB_IT_NIL;
      pType = hb_param( 1 , HB_IT_NUMERIC );
      pRecNo = hb_param( 2 , HB_IT_NUMERIC );
      if( pType )
      {
         pInfo = hb_param( 3 , HB_IT_ANY );
         if( pInfo )
         {
            hb_itemCopy( &Temp, pInfo );
         }
         SELF_RECINFO( pArea, pRecNo, hb_itemGetNI( pType ), &Temp );
         hb_itemForwardValue( &(HB_VM_STACK).Return, &Temp );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_INFOBADPARAMETER, NULL, "DBRECORDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECORDINFO" );
}

HB_FUNC( DBFILEGET )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pFileName, pMode;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      USHORT uiFields, uiIndex;
      szFileName[ 0 ] = szFileName[ _POSIX_PATH_MAX ] = '\0';
      uiIndex = hb_parni( 1 );
      pFileName = hb_param( 2 , HB_IT_STRING );
      pMode = hb_param( 3 , HB_IT_NUMERIC );
      if( pFileName && pMode &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         strncat( szFileName, pFileName->item.asString.value, _POSIX_PATH_MAX );
         hb_retl( SELF_GETVALUEFILE( pArea, uiIndex, szFileName,
                                     hb_itemGetNI( pMode ) ) );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, NULL, "DBFILEGET" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEGET" );

   hb_retl( FALSE );
}

HB_FUNC( DBFILEPUT )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      USHORT uiFields, uiIndex;

      szFileName[ 0 ] = szFileName[ _POSIX_PATH_MAX ] = '\0';
      uiIndex = hb_parni( 1 );
      pFileName = hb_param( 2 , HB_IT_STRING );
      if( pFileName &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         strncat( szFileName, pFileName->item.asString.value, _POSIX_PATH_MAX );
         hb_retl( SELF_PUTVALUEFILE( pArea, uiIndex, szFileName ) );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, "DBFILEPUT" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEPUT" );

   hb_retl( FALSE );
}
#endif

/*******************************************/
/* here we have the NEW Database level functions DBDROP & DBEXISTS */
HB_FUNC( DBDROP )
{
   HB_THREAD_STUB
  LPRDDNODE  pRDDNode;
  USHORT     uiRddID;
  char      *szDriver;

  if ( ISCHAR( 2 ) ) /* we have a VIA RDD parameter */
    szDriver = hb_parcx( 2 );
  else
    szDriver = s_szDefDriver;

  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBDROP" );
    return;
  }

  if ( SELF_DROP( pRDDNode, hb_param( 1, HB_IT_STRING )) == 0 )
  {
     hb_retl( TRUE );
  }
  else
  {
     hb_retl( FALSE );
  }
}

HB_FUNC( DBEXISTS )
{
   HB_THREAD_STUB
  LPRDDNODE  pRDDNode;
  USHORT     uiRddID;
  char * szDriver;

  if ( ISCHAR( 3 ) ) /* we have a VIA RDD parameter */
    szDriver = hb_parcx( 3 );
  else
    szDriver = s_szDefDriver;

  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEXISTS" );
    return;
  }

  if( SELF_EXISTS( pRDDNode, ISCHAR( 1 ) ? hb_param( 1, HB_IT_STRING ) : NULL, ISCHAR( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL ) )
  {
     hb_retl( TRUE );
  }
  else
  {
     hb_retl( FALSE );
  }
}

/*******************************************/
/* as we are in C, the code is upside down,
   find __SBAPP & __DBCOPY at the bottom
*/

// check if the field is on the Fields Array
static BOOL IsFieldIn( char * fieldName, PHB_ITEM pFields )
{
  USHORT i, j, uiFields = ( USHORT ) hb_arrayLen( pFields );
  char *ptr;
  BOOL lresult;

  for ( i=0; i<uiFields; i++ )
  {
    PHB_ITEM pField = pFields->item.asArray.value->pItems + i;
    ptr = strrchr( (char *)pField->item.asString.value,'>' );
    if( ptr && ptr > (char *)pField->item.asString.value && *(ptr-1)=='-' )
       ptr ++;
    else
       ptr = (char *)pField->item.asString.value;
    lresult = TRUE;
    for( j=0;*ptr;j++,ptr++ )
        if( *(fieldName+j) != toupper(*ptr) )
        {
           lresult = FALSE;
           break;
        }
    if ( lresult )
      return TRUE;
  }
  return FALSE;
}

static void AddField( AREAP pArea, PHB_ITEM pFieldArray, USHORT uiCount )
{
   HB_ITEM Item, Data;
   Item.type = HB_IT_NIL;
   Data.type = HB_IT_NIL;

   hb_arrayNew( &Item, 4 );

   SELF_FIELDINFO( pArea, uiCount, DBS_NAME, &Data );
   hb_arraySetForward( &Item, 1, &Data );

   SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, &Data );
   hb_arraySetForward( &Item, 2, &Data );

   SELF_FIELDINFO( pArea, uiCount, DBS_LEN, &Data );
   hb_arraySetForward( &Item, 3, &Data );

   SELF_FIELDINFO( pArea, uiCount, DBS_DEC, &Data );
   hb_arraySetForward( &Item, 4, &Data );

   hb_arrayAddForward( pFieldArray, &Item );
}

/*   create a new AREANODE and open its Area
   If the file exists it will be deleted & a new one created
*/
static AREAP GetTheOtherArea( char *szDriver, char * szFileName, BOOL createIt, PHB_ITEM pFields )
{
   HB_THREAD_STUB
   AREAP      pNewArea, pOldArea;
   USHORT     uiOldArea, uiNewArea;
   PHB_FNAME  pFileName;
   DBOPENINFO pInfo;
   char szFile[ _POSIX_PATH_MAX + 1 ];

   pOldArea = HB_CURRENT_WA;
   uiOldArea = pOldArea->uiArea;
   /*
    * 0 means chose first available in hb_rddInsertAreaNode()
    * This hack is necessary to avoid race condition in MT
    * if we don't want to lock whole RDD subsystem, Druzus
    */
   hb_rddSelectWorkAreaNumber( 0 );
   /* create new area */
   if ( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_rddSelectWorkAreaNumber( uiOldArea );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
      return NULL;
   }
   pNewArea = HB_CURRENT_WA;
   uiNewArea = pNewArea->uiArea;
   /* Fill pInfo structure */
   memset( &pInfo, 0, sizeof(DBOPENINFO) );
   pInfo.uiArea = uiNewArea;
   pInfo.atomAlias = ( BYTE * ) "__TMPAREA";
   pInfo.fShared = FALSE;
   pInfo.fReadonly = FALSE;

   strncpy( szFile, szFileName, _POSIX_PATH_MAX );
   szFile[ _POSIX_PATH_MAX ] = '\0';

   /* check the extension */
   pFileName = hb_fsFNameSplit( szFile );
   if( ! pFileName->szExtension )
   {
      HB_ITEM extItm;
      extItm.type = HB_IT_NIL;
      SELF_INFO( pOldArea, DBI_TABLEEXT, &extItm );
      if( HB_IS_STRING( &extItm ) )
      {
         strncat( szFile, extItm.item.asString.value, _POSIX_PATH_MAX - strlen( szFile ) );
      }
      hb_itemClear( &extItm );
   }
   hb_xfree( pFileName );

   pInfo.abName = ( BYTE * ) szFile;

   if ( createIt )
   {
      PHB_ITEM pFieldArray;
      USHORT uiFields, uiCount;

      /* get the table structure */
      SELF_FIELDCOUNT( pOldArea, &uiFields );

      pFieldArray = hb_itemNew( NULL );
      hb_arrayNew( pFieldArray, 0 );

      if( pFields )
      {
         USHORT i;
         char *ptr;
         char *szFieldName = ( char * ) hb_xgrab( pOldArea->uiMaxFieldNameLength + 1 );

         uiFields = ( USHORT ) hb_arrayLen( pFields );
         for( i = 0; i < uiFields; i++ )
         {
            PHB_ITEM pField = pFields->item.asArray.value->pItems + i;
            ptr = strrchr( (char *) pField->item.asString.value, '>' );
            if( ptr && ptr > (char *) pField->item.asString.value && *(ptr-1) == '-' )
            {
               ptr++;
            }
            else
            {
               ptr = (char *) pField->item.asString.value;
            }
            hb_strncpyUpper( szFieldName, ptr, strlen( ptr ) );
            if( ( uiCount = hb_rddFieldIndex( pOldArea, szFieldName ) ) != 0 )
            {
               AddField( pOldArea, pFieldArray, uiCount );
            }
         }
         hb_xfree( szFieldName );
      }
      else
      {
         for( uiCount = 1; uiCount <= uiFields; uiCount++ )
         {
            /*if ( !pFields || IsFieldIn( (( PHB_DYNS )(pOldArea->lpFields + (uiCount-1))->sym )->pSymbol->szName,  pFields )) */
            AddField( pOldArea, pFieldArray, uiCount );
         }
      }

      if( ! hb_arrayLen( pFieldArray ) )
      {
         hb_itemRelease( pFieldArray );
         hb_rddReleaseCurrentArea();
         hb_rddSelectWorkAreaNumber( uiOldArea );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
         return NULL;
      }

      /* check for table existence and if true, drop it */
      {
         LPRDDNODE pRDDNode;
         pRDDNode = hb_rddFindNode( szDriver, NULL );  // find the RDD
         if ( pRDDNode )
         {
            HB_ITEM tableItm;
            tableItm.type = HB_IT_NIL;
            hb_itemPutCL( &tableItm, szFileName, strlen( szFile ) );
            if( SELF_EXISTS( pRDDNode, &tableItm, NULL ) )
            {
               SELF_DROP( pRDDNode, &tableItm );
            }
            hb_itemClear( &tableItm );
         }
      }

      /* now create a new table based on the current Area's record layout */
      pNewArea->atomAlias = hb_dynsymGet( ( char * ) pInfo.atomAlias );
      ( ( PHB_DYNS ) pNewArea->atomAlias )->hArea = pNewArea->uiArea;

      if( SELF_CREATEFIELDS( pNewArea, pFieldArray ) == FAILURE ||
          SELF_CREATE( pNewArea, &pInfo ) == FAILURE )
      {
         hb_itemRelease( pFieldArray );
         hb_rddReleaseCurrentArea();
         hb_rddSelectWorkAreaNumber( uiOldArea );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBAPP" );
         return NULL;
      }
      hb_itemRelease( pFieldArray );

      /* clear the new AREA before reopen it */
      hb_rddReleaseCurrentArea();
      /*
       * 0 means chose first available in hb_rddInsertAreaNode()
       * This hack is necessary to avoid race condition in MT
       * if we don't want to lock whole RDD subsystem, Druzus
       */
      hb_rddSelectWorkAreaNumber( 0 );
      if ( ! hb_rddInsertAreaNode( szDriver ) )
      {
         hb_rddSelectWorkAreaNumber( uiOldArea );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
         return NULL;
      }
      pNewArea = HB_CURRENT_WA;
      uiNewArea = pNewArea->uiArea;
      pInfo.uiArea = uiNewArea;
   }

   /* open it */
   if( SELF_OPEN( pNewArea, &pInfo ) == FAILURE )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiOldArea );
      if ( hb_vmRequestQuery() == 0 )
      {
         hb_errRT_DBCMD( EG_OPEN, 0, NULL, "DBAPP" ); /* Could not open it */
      }
      return NULL;
   }
   hb_rddSelectWorkAreaNumber( uiOldArea );
   return pNewArea;
}

/* move the Field Data between areas by name */
static void rddMoveFields( AREAP pAreaFrom, AREAP pAreaTo, PHB_ITEM pFields )
{
   USHORT i, f;
   HB_ITEM fieldValue;
   char * szName;

   fieldValue.type = HB_IT_NIL;
   szName = ( char * ) hb_xgrab( ( ( AREAP ) pAreaTo)->uiMaxFieldNameLength + 1 );

   for( i = 0; i < pAreaTo->uiFieldCount; i++ )
   {
      SELF_FIELDNAME( ( AREAP ) pAreaTo, i + 1, szName );
      /* field in the list?*/
      if ( pFields == NULL || IsFieldIn( szName, pFields ) )
      {
         f = hb_rddFieldIndex( pAreaFrom, szName );
         if ( f )
         {
            hb_rddSelectWorkAreaNumber( pAreaFrom->uiArea );
            SELF_GETVALUE( pAreaFrom, f, &fieldValue );
            hb_rddSelectWorkAreaNumber( pAreaTo->uiArea );
            SELF_PUTVALUE( pAreaTo, i + 1, &fieldValue );
         }
      }
   }
   hb_xfree( szName );
   hb_itemClear( &fieldValue );
}

/*move the records, filtering if apropiate*/
static ERRCODE rddMoveRecords( char *cAreaFrom, char *cAreaTo, PHB_ITEM pFields,
                               PHB_ITEM pFor, PHB_ITEM pWhile, LONG lNext,
                               ULONG lRec, BOOL bRest, char *cDriver )
{
   HB_THREAD_STUB
   char     * szDriver;
   AREAP      pAreaFrom, pAreaTo, pAreaRelease = NULL;
   USHORT     uiCurrAreaSaved = hb_rddGetCurrentWorkAreaNumber();
   AREAP      pCurrArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("rddMoveRecords(%s, %s, %p, %p, %p, %d, %lu, %d, %s )",
            cAreaFrom, cAreaTo, pFields, pFor, pWhile, lNext, lRec, bRest, cDriver));

   if ( !pCurrArea )   /*We need a current Area to APPEND TO or FROM*/
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
      return EG_NOTABLE;
   }

   /*get the RDD Driver to use for the "other" Area*/
   if ( cDriver )
   {
      szDriver = cDriver;
   }
   else
   {
      szDriver = s_szDefDriver;
   }
   if ( (!cAreaFrom) == (!cAreaTo) )          /*One File is needed*/
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
      return EG_ARG;
   }
   if ( pFields && hb_arrayLen( pFields ) == 0 )  /*no field clause?*/
   {
      pFields = NULL;
   }
   pAreaFrom = pAreaTo = pCurrArea;
   if( cAreaTo )  /*it's a COPY TO*/
   {
      pAreaRelease = pAreaTo = GetTheOtherArea( szDriver, cAreaTo, TRUE, pFields );
   }
   else if( cAreaFrom )       /*it's an APPEND FROM*/
   {                          /*make it current*/
      pAreaRelease = pAreaFrom = GetTheOtherArea( szDriver, cAreaFrom, FALSE, NULL );
      if( hb_vmRequestQuery() )
      {
         return EG_NOTABLE;
      }
   }
   /* one or the other but never none */
   if ( ! pAreaRelease )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
      return EG_NOTABLE;
   }

   hb_rddSelectWorkAreaNumber( pAreaFrom->uiArea );

   if( lRec > 0 )
   {                                      /* only one record */
      SELF_GOTO( pAreaFrom, lRec );       /* go there */
   }
   else if( ! pWhile && ! bRest && ! lNext )  /* these two stay current */
   {
      SELF_GOTOP( pAreaFrom );        /* else start from the top */
   }

   /*move those records assuming we are positioned on one.*/
   while ( ! pAreaFrom->fEof )
   {
      if ( pWhile && ! hb_itemGetL( hb_vmEvalBlock( pWhile ) ) )
      {
         break;
      }
      if ( !pFor || hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
      {
         hb_rddSelectWorkAreaNumber( pAreaTo->uiArea );
         SELF_APPEND( pAreaTo, FALSE );      /*put a new one on TO Area*/
         rddMoveFields( pAreaFrom, pAreaTo, pFields ); /*move the data*/
         hb_rddSelectWorkAreaNumber( pAreaFrom->uiArea );
      }
      if ( lNext > 0 )
      {
         if ( --lNext == 0 )
         {
            break;
         }
      }
      else if ( lRec != 0 ) /* only the one record? */
      {
         break;
      }
      SELF_SKIP( pAreaFrom, 1L );  /*get the next one*/
   }

   /*Close the File*/
   hb_rddSelectWorkAreaNumber( pAreaRelease->uiArea );
   hb_rddReleaseCurrentArea();
   hb_rddSelectWorkAreaNumber( uiCurrAreaSaved );
   return SUCCESS;
}

HB_FUNC( __DBAPP )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  hb_parcx( 1 ),                /* File From */
                     NULL,                        /* TO current area */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parcx( 8 ) : NULL ); /* RDD */
  }
}

HB_FUNC( __DBCOPY )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  NULL,                        /* fro CURRENT Area */
                     hb_parcx( 1 ),                /* To File */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parcx( 8 ) : NULL ); /* RDD */
  }
}

HB_FUNC( DBUSEAREAD )
{
   HB_THREAD_STUB
   char * szDriver;
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   BYTE * codePageId = (BYTE*) hb_parcx(7);
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   AREAP pArea;

   s_bNetError = FALSE;

   /* New area? */
   if( hb_parl( 1 ) )
   {
      hb_rddSelectFirstAvailable();
   }
   else if( HB_CURRENT_WA )   /* If current WorkArea is in use then close it */
   {
      hb_rddReleaseCurrentArea();
   }

   hb_rddCheck();
   uiLen = ( USHORT ) hb_parclen( 2 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriverBuffer, hb_parcx( 2 ), uiLen );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = s_szDefDriver;

   if( hb_parclen(3) == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }

   strncpy( szFileName, hb_parcx( 3 ), _POSIX_PATH_MAX );
   szFileName[ _POSIX_PATH_MAX ] = '\0';

   pFileName = hb_fsFNameSplit( szFileName );
   strncpy( szAlias, hb_parcx( 4 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH ] = '\0';

   if( strlen( szAlias ) == 0 )
      strncpy( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );

   hb_xfree( pFileName );

   uiLen = strlen( szAlias );
   if( szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
   {
      hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
      return;
   }
   if( uiLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
         return;
      }
   }

   /* Create a new WorkArea node */
   if( !hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }
   pArea = HB_CURRENT_WA;

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;
   pInfo.cdpId = codePageId;

   /* Open file */
   if( SELF_OPEN( pArea, &pInfo ) == FAILURE )
   {
      s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      hb_rddReleaseCurrentArea();
      return;
   }
}

ERRCODE hb_rddGetTempAlias( char * szAliasTmp )
{
   int i;

   // szAliasTmp[0] = '\0';
   for ( i = 1 ; i < 1000 ; i++ )
   {
      sprintf( szAliasTmp, "HBTMP%3.3i", i);

      if ( ! hb_rddSelect( szAliasTmp ) )
      {
         break;
      }
   }

   if ( i >= 1000 )
   {
      szAliasTmp[0] = '\0';
      return FAILURE;
   }
   else
   {
      return SUCCESS;
   }
}

HB_FUNC( __RDDGETTEMPALIAS )
{
   HB_THREAD_STUB
   char szAliasTmp[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   hb_rddGetTempAlias( szAliasTmp );
   hb_retc( szAliasTmp );
}

HB_FUNC( DBSKIPPER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {  LONG nSkipped    = 0;
      LONG nRecs       = 1;
      BOOL bBEof       = TRUE;
      if( hb_pcount() > 0 )
      {
         nRecs = hb_parnl( 1 ) ;
      }

      SELF_EOF( pArea, &bBEof );
      if( nRecs == 0 )
      {
         SELF_SKIP( pArea, 0 );
      }
      else if( nRecs > 0 && !bBEof  )
      {
         while( nSkipped < nRecs )
         {
            SELF_SKIP( pArea, 1 );
            if( pArea->fEof )
            {
               SELF_SKIP( pArea, -1 );
               nRecs = nSkipped ;
            }
            else
            {
               nSkipped++ ;
            }
         }
      }
      else if( nRecs < 0 )
      {
         while( nSkipped > nRecs )
         {
            SELF_SKIP( pArea, -1 );
            if( pArea->fBof )
            {
               nRecs = nSkipped ;
            }
            else
            {
               nSkipped-- ;
            }
         }
      }

      hb_retnl( nSkipped );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIPPER" );
}

/*
  AJ: Added 2004-03-10
*/
// Import values from delimited text file
static void hb_AppendToDb( PHB_ITEM pDelimitedFile, PHB_ITEM pDelimiter )
{
   HB_THREAD_STUB

   if ( pDelimitedFile )
   {
      AREAP pArea = HB_CURRENT_WA;
      // Getting return value from FARSEEX()
      PHB_ITEM pContent = hb_itemDoC( "FPARSEEX", 2, pDelimitedFile, pDelimiter );

      ULONG ulContent = pContent->item.asArray.value->ulLen;
      HB_ITEM Structure;
      USHORT uiStruct;
      ULONG ulData;

      Structure.type = HB_IT_NIL;
      hb_dbfStructure( &Structure );
      uiStruct = ( USHORT ) (&Structure)->item.asArray.value->ulLen;

      if ( ulContent > 0 )
      {
         for ( ulData = 0; ulData < ulContent; ulData ++ )
         {
            PHB_ITEM pData = hb_arrayGetItemPtr( pContent, ulData + 1 );
            USHORT uiData = ( USHORT ) pData->item.asArray.value->ulLen;
            USHORT uiField = ( uiData < uiStruct ) ? uiData : uiStruct;
            USHORT ui;

            s_bNetError = FALSE;

            if( SELF_APPEND( pArea, TRUE ) == FAILURE )
            {
               s_bNetError = TRUE;
            }
            else
            {
               for ( ui = 0; ui < uiField ; ui ++ )
               {
                  HB_ITEM FieldValue;
                  PHB_ITEM pFieldInfo = hb_arrayGetItemPtr( &Structure, ui + 1 );
                  char *cBuffer = hb_arrayGetC( pData, ui + 1 );
                  char *cFieldType = hb_arrayGetC( pFieldInfo, 2 );

                  ( &FieldValue )->type = HB_IT_NIL;

                  /* Create PHB_ITEM to be FIELDPUTted */
                  switch( cFieldType[0] )
                  {
                     /* It's a DATE field */
                     case 'D':
                        hb_itemPutDS( &FieldValue, cBuffer );
                        break;

                     /* It's a LOGICAL field '*/
                     case 'L':
                     {
                        BOOL bTrue;
                        hb_strupr( cBuffer );
                        bTrue = ( cBuffer[0] == 'T' );
                        hb_itemPutL( &FieldValue, bTrue );
                        break;
                     }

                     /* It's a NUMERIC field */
                     case 'N':
                        hb_itemPutND( &FieldValue, hb_strVal( cBuffer ) );
                        break;

                     /* It's a CHARACTER field */
                     default:
                        hb_itemPutC( &FieldValue, cBuffer );
                        break;
                  }

                  /* FieldPut */
                  SELF_PUTVALUE( pArea, ui + 1, &FieldValue );

                  /* Clean Ups */
                  hb_itemClear( &FieldValue );
                  hb_xfree( cBuffer );
                  hb_xfree( cFieldType );
               }
            }
         }
      }
      /* Clean Ups */
      hb_itemClear( &Structure );
      hb_itemRelease( pContent );
   }
}

// Escaping delimited strings. Need to be cleaned/optimized/improved
static char *hb_strescape( char *szInput, int lLen, char *cEsc )
{
   int     lCnt     = 0;
   char  * szChr;
   char  * szEscape = NULL;
   char  * szReturn = NULL;

   szReturn = szEscape = ( char * ) hb_xgrab( lLen * 2 + 4 );

   while( lLen && HB_ISSPACE( szInput[ lLen - 1 ] ) )
   {
      lLen--;
   }

   szChr = szInput;

   while ( *szChr && lCnt++ < lLen )
   {
      if( *szChr == *cEsc ) // *szChr == '\\' || *szChr == '"'  )
      {
         *szEscape++ = '\'';
      }
      *szEscape++ = *szChr++;
   }
   *szEscape = '\0';

   return szReturn;
}

// Export field values to text file
static BOOL hb_ExportVar( int handle, PHB_ITEM pValue, char *cDelim )
{
   switch( pValue->type )
   {
      // a "C" field
      case HB_IT_STRING:
      {
         char *szStrEsc;
         char *szString;

         szStrEsc = hb_strescape( pValue->item.asString.value, pValue->item.asString.length, cDelim );
         szString = hb_xstrcpy( NULL,cDelim,szStrEsc,cDelim,NULL);

         // FWrite( handle, szString )
         hb_fsWriteLarge( handle, (BYTE*) szString, strlen( szString ) );

         // Orphaned, get rif off it
         hb_xfree( szStrEsc );
         hb_xfree( szString );
         break;
      }
      // a "D" field
      case HB_IT_DATE:
      {
         char *szDate = (char*) hb_xgrab( 9 );

         hb_itemGetDS( pValue, szDate );
         hb_fsWriteLarge( handle, (BYTE*) szDate, strlen( szDate ) );
         hb_xfree( szDate );
         break;
      }
      // an "L" field
      case HB_IT_LOGICAL:
      {
         hb_fsWriteLarge( handle, (BYTE*) ( pValue->item.asLogical.value ? "T" : "F" ), 1 );
         break;
      }
      // an "N" field
      case HB_IT_INTEGER:
      case HB_IT_LONG:
#ifndef HB_LONG_LONG_OFF
      case HB_IT_LONGLONG:
#endif
      case HB_IT_DOUBLE:
      {
         char *szResult = hb_itemStr( pValue, NULL, NULL );

         if ( szResult )
         {
            ULONG ulLen = strlen( szResult );
            char * szTrimmed = hb_strLTrim( szResult, &ulLen );

            hb_fsWriteLarge( handle, (BYTE*) szTrimmed, strlen( szTrimmed ) );
            hb_xfree( szResult );
         }
         break;
      }
      // an "M" field or the other, might be a "V" in SixDriver
      default:
      // We do not want MEMO contents
         return FALSE;
   }
   return TRUE;
}

// Eval( bBlock )
static BOOL hb___Eval( PHB_ITEM pItem )
{
   HB_THREAD_STUB

   if( pItem )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      hb_vmDo( 0 );
      return (&HB_VM_STACK.Return)->item.asLogical.value ;
   }
   return TRUE;
}

// Export DBF content to text file
static void hb_Dbf2Text( PHB_ITEM pWhile, PHB_ITEM pFor, PHB_ITEM pFields,
                        char *cDelim, FHANDLE handle, BYTE *cSep, int nCount )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   int iSepLen;
   USHORT uiFields = 0;
   USHORT ui;
   HB_ITEM Tmp;
   BOOL bWriteSep = FALSE;

   BOOL bEof = TRUE;
   BOOL bBof = TRUE;

   BOOL bNoFieldPassed = ( pFields == NULL || pFields->item.asArray.value->ulLen == 0 ) ;

   if( ! handle )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBF2TEXT" );
      return;
   }

   if ( !cDelim )
   {
      cDelim = "\"";
   }

   if ( cSep )
   {
      iSepLen = strlen( (char*) cSep );
   }
   else
   {
      cSep = (BYTE*) ',';
      iSepLen = 1;
   }

   SELF_FIELDCOUNT( pArea, &uiFields );

   Tmp.type = HB_IT_NIL;

   while ( hb___Eval( pWhile ) && ( nCount == -1 || nCount > 0 ) )
   {
      // While !BOF() .AND. !EOF()
      SELF_EOF( pArea, &bEof );
      SELF_BOF( pArea, &bBof );

      if ( bEof || bBof )
      {
         break;
      }

      // For condition is met
      // if For is NULL, hb__Eval returns TRUE
      if ( hb___Eval ( pFor ) )
      {
         // User does not request fields, copy all fields
         if ( bNoFieldPassed )
         {
            for ( ui = 1; ui <= uiFields; ui ++ )
            {
               if ( bWriteSep )
               {
                  hb_fsWriteLarge( handle, cSep, iSepLen );
               }

               SELF_GETVALUE( pArea, ui, &Tmp );
               bWriteSep = hb_ExportVar( handle, &Tmp, cDelim );
               hb_itemClear( &Tmp );
            }
         }
         // Only requested fields are exorted here
         else
         {
            USHORT uiFieldCopy = ( USHORT ) pFields->item.asArray.value->ulLen;
            USHORT uiItter;

            for ( uiItter = 1; uiItter <= uiFieldCopy; uiItter++ )
            {
               char *szFieldName = hb_arrayGetC( pFields, uiItter );

               if ( bWriteSep )
               {
                  hb_fsWriteLarge( handle, cSep, iSepLen );
               }

               if ( szFieldName )
               {
                  int iFieldLen = strlen( szFieldName );
                  char *szName = ( char * ) hb_xgrab( iFieldLen + 1 );
                  int iPos;

                  hb_strncpyUpperTrim( szName, szFieldName, iFieldLen );
                  iPos = hb_rddFieldIndex( pArea, szName );
                  hb_xfree( szName );

                  SELF_GETVALUE( pArea, iPos, &Tmp );
                  bWriteSep = hb_ExportVar( handle, &Tmp, cDelim );
                  hb_itemClear( &Tmp );
               }

               hb_xfree( szFieldName );
            }
         }
         hb_fsWriteLarge( handle, (BYTE*) "\r\n", 2 );
         bWriteSep = FALSE;
      }

      if ( nCount != -1 )
      {
         nCount-- ;
      }

      // DBSKIP()
      SELF_SKIP( pArea, 1 );
   }

   // Writing EOF
   hb_fsWriteLarge( handle, (BYTE*) "\x1A", 1 );
}

/*
   AJ: 2004-03-12
   This concludes removal of the entire PRG codes for __DBDELIM()
*/
HB_FUNC( __DBDELIM )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pError = NULL;
      BOOL bExport = hb_parl( 1 );
      char *cFileName = hb_parcx( 2 );
      PHB_ITEM pDelimArg = hb_param( 3, HB_IT_STRING );
      PHB_ITEM pFields = hb_param( 4, HB_IT_ARRAY );
      PHB_ITEM pFor = hb_param( 5, HB_IT_BLOCK );
      PHB_ITEM pWhile = hb_param( 6, HB_IT_BLOCK );
      PHB_ITEM pNext = hb_param( 7, HB_IT_NUMERIC );
      PHB_ITEM pRecord = hb_param( 8, HB_IT_NUMERIC );
      PHB_ITEM pRest = hb_param( 9, HB_IT_LOGICAL );

      PHB_FNAME pFileName;
      FHANDLE handle;
      LONG lStart, lCount;
      char cSeparator[] = ",";
      char cDelim[] = "\"";
      char szFileName[ _POSIX_PATH_MAX + 1 ];

      BOOL bRetry;

      if( pDelimArg )
      {
         if( toupper( pDelimArg->item.asString.value[0] ) == 'B' &&
             toupper( pDelimArg->item.asString.value[1] ) == 'L' &&
             toupper( pDelimArg->item.asString.value[2] ) == 'A' &&
             toupper( pDelimArg->item.asString.value[3] ) == 'N' &&
             toupper( pDelimArg->item.asString.value[4] ) == 'K' &&
             pDelimArg->item.asString.value[5] == '\0' )
         {
            cDelim[0] = '\0';
            cSeparator[0] = ' ';
         }
         else
         {
            cDelim[0] = pDelimArg->item.asString.value[0];
         }
      }

      // Process the file name argument.
      pFileName = hb_fsFNameSplit( cFileName );

      if ( ! pFileName->szExtension )
      {
         // No file name extension, so provide the default.
         pFileName->szExtension = ".txt";
      }

      hb_fsFNameMerge( szFileName, pFileName );

      // Immediately cleared things, don't want it no more
      hb_xfree( pFileName );

      // Determine where to start and how many records to process.
      if( pRecord )
      {
        // The RECORD clause has the highest priority.
         lStart = hb_parnl( 8 );
         lCount = 1;
      }
      else if ( pNext )
      {
         // The NEXT clause has the next highest priority.
         lStart = -1;
         lCount = hb_parnl( 7 );
      }
      else if ( pWhile || ( pRest && pRest->item.asLogical.value ) )
      {
         // The WHILE and REST clauses have equal priority.
         lStart = -1;
         lCount = -1;
      }
      else
      {
         // Followed by the FOR clause or the ALL clause.
         lStart = 0;
         lCount = -1;
      }

      // COPY TO DELIMITED
      if ( bExport )
      {
         // Try to create text file
         do
         {
            handle = hb_fsCreate( (BYTE*) szFileName, FC_NORMAL );

            if( handle == F_ERROR )
            {
               if( pError == NULL )
               {
                  pError = hb_errNew();
                  hb_errPutSeverity( pError, ES_ERROR );
                  hb_errPutGenCode( pError, EG_CREATE );
                  hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
                  hb_errPutSubCode( pError, 1002 ); // Where is the Macro ?
                  hb_errPutFileName( pError, szFileName );
                  hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
                  hb_errPutSubSystem( pError, "DELIM" );
               }

               hb_errPutOsCode( pError, hb_fsError() );
               bRetry = hb_errLaunch( pError );
            }
            else
            {
               bRetry = FALSE;
            }
         } while( bRetry );

         if( pError )
         {
            hb_itemRelease( pError );
            pError = NULL;
         }

         if ( lStart > -1 )
         {
            // Only reposition if a starting record was specified or implied.
            if ( lStart == 0 )
            {
               // DBGOTOP()
               SELF_GOTOP( pArea );
            }
            else
            {
               // DBGOTO( lStart )
               HB_ITEM pGoto;
               pGoto.type = HB_IT_NIL;
               hb_itemPutNL( &pGoto, lStart );
               SELF_GOTOID( pArea, &pGoto );
               // Clean up
               hb_itemClear( &pGoto );
            }
         }

         // Doing things now
         hb_Dbf2Text( pWhile, pFor, pFields, cDelim, handle, ( BYTE *)cSeparator, lCount );

         hb_fsClose( handle );
      }
      // APPEND FROM DELIMITED
      else
      {
         // Container to pass to AppendToDb()
         HB_ITEM pDelimitedFile;
         HB_ITEM pSep;

         // NIL them at the first place
         pDelimitedFile.type = HB_IT_NIL;
         pSep.type = HB_IT_NIL;

         // Try to open the delimited text file
         do
         {
            handle = hb_fsOpen( (BYTE*) szFileName, FO_READ | FO_COMPAT );

            // Booo error in opening file
            if( handle == F_ERROR )
            {
               // Only create object once in this loop
               if( pError == NULL )
               {
                  pError = hb_errNew();
                  hb_errPutSeverity( pError, ES_ERROR );
                  hb_errPutGenCode( pError, EG_OPEN );
                  hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
                  hb_errPutSubCode( pError, 1001 ); // Where is the Macro ?
                  hb_errPutFileName( pError, szFileName );
                  hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
                  hb_errPutSubSystem( pError, "DELIM" );
               }

               hb_errPutOsCode( pError, hb_fsError() );

               // Execute the error handler
               bRetry = hb_errLaunch( pError );
            }
            else
            {
               bRetry = FALSE;
            }
         } while( bRetry );

         if( pError )
         {
            hb_itemRelease( pError );
            pError = NULL;
         }

         // We don't need this handle as the process is done in FPARSEEX()
         hb_fsClose( handle );

         // Assign value to HB_ITEM
         hb_itemPutC( &pSep, (char*) cSeparator );
         hb_itemPutC( &pDelimitedFile, szFileName );

         // The Job is being done here
         hb_AppendToDb( &pDelimitedFile, &pSep );

         // Clean ups
         hb_itemClear( &pSep );
         hb_itemClear( &pDelimitedFile );
      }
      // We are Done!
   }
   else
   {
      // No workarea to do the job
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBDELIM" );
   }
}

// Export field values to SQL script file
static BOOL hb_ExportSqlVar( int handle, PHB_ITEM pValue, char *cDelim )
{

   switch( pValue->type )
   {
      // a "C" field
      case HB_IT_STRING:
      {
         char *szStrEsc;
         char *szString;

         szStrEsc = hb_strescape( pValue->item.asString.value, pValue->item.asString.length, cDelim );
         szString = hb_xstrcpy( NULL,cDelim,szStrEsc,cDelim,NULL);

         // FWrite( handle, szString )
         hb_fsWriteLarge( handle, (BYTE*) szString, strlen( szString ) );

         // Orphaned, get rif off it
         hb_xfree( szStrEsc );
         hb_xfree( szString );
         break;
      }
      // a "D" field
      case HB_IT_DATE:
      {
         char *szDate = (char*) hb_xgrab( 9 );
         char *szSqlDate = (char*) hb_xgrab( 11 );
         char *szString;

         hb_itemGetDS( pValue, szDate );
         if( szDate[0] == ' ' )
         {
            strcpy( szSqlDate, "0100-01-01" );
         }
         else
         {
            sprintf( szSqlDate, "%c%c%c%c-%c%c-%c%c", szDate[0],szDate[1],szDate[2],szDate[3],szDate[4],szDate[5],szDate[6],szDate[7] );
         }
         szSqlDate[ 10 ] = '\0';
         szString = hb_xstrcpy( NULL,cDelim,szSqlDate,cDelim,NULL);
         hb_fsWriteLarge( handle, (BYTE*) szString, strlen( szString ) );
         hb_xfree( szDate );
         hb_xfree( szSqlDate );
         hb_xfree( szString );
         break;
      }
      // an "L" field
      case HB_IT_LOGICAL:
      {
         char *szString;
         szString = hb_xstrcpy( NULL,cDelim,( pValue->item.asLogical.value ? "Y" : "N" ),cDelim,NULL);
         hb_fsWriteLarge( handle, (BYTE*) szString, strlen( szString ) );
         hb_xfree( szString );
         break;
      }
      // an "N" field
      case HB_IT_INTEGER:
      case HB_IT_LONG:
#ifndef HB_LONG_LONG_OFF
      case HB_IT_LONGLONG:
#endif
      case HB_IT_DOUBLE:
      {
         char *szResult = hb_itemStr( pValue, NULL, NULL );

         if ( szResult )
         {
            ULONG ulLen = strlen( szResult );
            char * szTrimmed = hb_strLTrim( szResult, &ulLen );

            hb_fsWriteLarge( handle, (BYTE*) szTrimmed, strlen( szTrimmed ) );
            hb_xfree( szResult );
         }
         break;
      }
      // an "M" field or the other, might be a "V" in SixDriver
      default:
      // We do not want MEMO contents
         return FALSE;
   }
   return TRUE;
}

// Export DBF content to a SQL script file
static void hb_Dbf2Sql( PHB_ITEM pWhile, PHB_ITEM pFor, PHB_ITEM pFields,
                        char *cDelim, FHANDLE handle, BYTE *cSep, int nCount, char *cTable, char *cHeader )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   int iSepLen;
   USHORT uiFields = 0;
   USHORT ui;
   HB_ITEM Tmp;
   ULONG lRecNo = 0;
   BOOL bWriteSep = FALSE;
   char *szRecNo = ( char * ) hb_xgrab( 11 );
   char *szInsert = ( char * ) hb_xgrab( 13 + strlen( cTable ) + 12 );

   BOOL bEof = TRUE;
   BOOL bBof = TRUE;

   BOOL bNoFieldPassed = ( pFields == NULL || pFields->item.asArray.value->ulLen == 0 ) ;


   if( ! handle )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBF2SQL" );
      return;
   }

   if ( !cDelim )
   {
      cDelim = "\'";
   }

   if ( cSep )
   {
      iSepLen = strlen( (char*) cSep );
   }
   else
   {
      cSep = (BYTE*) ',';
      iSepLen = 1;
   }

   hb_fsWriteLarge( handle, (BYTE*) cHeader, strlen( cHeader ) );

   SELF_FIELDCOUNT( pArea, &uiFields );

   Tmp.type = HB_IT_NIL;

   strcpy( szInsert, "INSERT INTO \"" );
   strcat( szInsert, cTable );
   strcat( szInsert, "\" VALUES ( " );

   while ( hb___Eval( pWhile ) && ( nCount == -1 || nCount > 0 ) )
   {
      // While !BOF() .AND. !EOF()
      SELF_EOF( pArea, &bEof );
      SELF_BOF( pArea, &bBof );

      if ( bEof || bBof )
      {
         break;
      }

      // For condition is met
      // if For is NULL, hb__Eval returns TRUE
      if ( hb___Eval ( pFor ) )
      {
         hb_fsWriteLarge( handle, (BYTE*) szInsert, strlen( szInsert ) );

         sprintf( szRecNo, "%ld,", ++lRecNo );

         hb_fsWriteLarge( handle, (BYTE*) szRecNo, strlen( szRecNo ) );

         // User does not request fields, copy all fields
         if ( bNoFieldPassed )
         {
            for ( ui = 1; ui <= uiFields; ui ++ )
            {
               if ( bWriteSep )
               {
                  hb_fsWriteLarge( handle, cSep, iSepLen );
               }

               SELF_GETVALUE( pArea, ui, &Tmp );
               bWriteSep = hb_ExportSqlVar( handle, &Tmp, cDelim );
               hb_itemClear( &Tmp );
            }
         }
         // Only requested fields are exorted here
         else
         {
            USHORT uiFieldCopy = ( USHORT ) pFields->item.asArray.value->ulLen;
            USHORT uiItter;

            for ( uiItter = 1; uiItter <= uiFieldCopy; uiItter++ )
            {
               char *szFieldName = hb_arrayGetC( pFields, uiItter );

               if ( bWriteSep )
               {
                  hb_fsWriteLarge( handle, cSep, iSepLen );
               }

               if ( szFieldName )
               {
                  int iFieldLen = strlen( szFieldName );
                  char *szName = ( char * ) hb_xgrab( iFieldLen + 1 );
                  int iPos;

                  hb_strncpyUpperTrim( szName, szFieldName, iFieldLen );
                  iPos = hb_rddFieldIndex( pArea, szName );
                  hb_xfree( szName );

                  SELF_GETVALUE( pArea, iPos, &Tmp );
                  bWriteSep = hb_ExportSqlVar( handle, &Tmp, cDelim );
                  hb_itemClear( &Tmp );
               }

               hb_xfree( szFieldName );
            }
         }
         /*
          * use szRecNo buffer (it's big enough) to avoid double call to
          * hb_fsWriteLarge
          */
         strcpy( szRecNo, " );" );
         strcat( szRecNo, hb_conNewLine() );
         hb_fsWriteLarge( handle, (BYTE *) szRecNo, strlen( szRecNo ) );
         bWriteSep = FALSE;
      }

      if ( nCount != -1 )
      {
         nCount-- ;
      }

      // DBSKIP()
      SELF_SKIP( pArea, 1 );
   }

   // Writing EOF
   // hb_fsWriteLarge( handle, (BYTE*) "\x1A", 1 );

   hb_xfree( szInsert );
   hb_xfree( szRecNo );

}

HB_FUNC( __DBSQL )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pError = NULL;
      BOOL bExport = hb_parl( 1 );
      char *cFileName = hb_parcx( 2 );
      char *cTable = hb_parcx( 3 );
      PHB_ITEM pFields = hb_param( 4, HB_IT_ARRAY );
      PHB_ITEM pFor = hb_param( 5, HB_IT_BLOCK );
      PHB_ITEM pWhile = hb_param( 6, HB_IT_BLOCK );
      PHB_ITEM pNext = hb_param( 7, HB_IT_NUMERIC );
      PHB_ITEM pRecord = hb_param( 8, HB_IT_NUMERIC );
      PHB_ITEM pRest = hb_param( 9, HB_IT_LOGICAL );
      char *cHeader = hb_parcx( 10 );

      PHB_FNAME pFileName;
      FHANDLE handle;
      LONG lStart, lCount;
      char cSeparator[] = ",";
      char cDelim[] = "\'";
      char szFileName[ _POSIX_PATH_MAX + 1 ];

      BOOL bRetry;

      // Process the file name argument.
      pFileName = hb_fsFNameSplit( cFileName );

      if ( ! pFileName->szExtension )
      {
         // No file name extension, so provide the default.
         pFileName->szExtension = ".sql";
      }

      hb_fsFNameMerge( szFileName, pFileName );

      // Immediately cleared things, don't want it no more
      hb_xfree( pFileName );

      // Determine where to start and how many records to process.
      if( pRecord )
      {
        // The RECORD clause has the highest priority.
         lStart = hb_parnl( 8 );
         lCount = 1;
      }
      else if ( pNext )
      {
         // The NEXT clause has the next highest priority.
         lStart = -1;
         lCount = hb_parnl( 7 );
      }
      else if ( pWhile || ( pRest && pRest->item.asLogical.value ) )
      {
         // The WHILE and REST clauses have equal priority.
         lStart = -1;
         lCount = -1;
      }
      else
      {
         // Followed by the FOR clause or the ALL clause.
         lStart = 0;
         lCount = -1;
      }

      // COPY TO SQL
      if ( bExport )
      {
         // Try to create text file
         do
         {
            handle = hb_fsCreate( (BYTE*) szFileName, FC_NORMAL );

            if( handle == F_ERROR )
            {
               if( pError == NULL )
               {
                  pError = hb_errNew();
                  hb_errPutSeverity( pError, ES_ERROR );
                  hb_errPutGenCode( pError, EG_CREATE );
                  hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
                  hb_errPutSubCode( pError, 1002 ); // Where is the Macro ?
                  hb_errPutFileName( pError, szFileName );
                  hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
                  hb_errPutSubSystem( pError, "DBF2SQL" );
               }

               hb_errPutOsCode( pError, hb_fsError() );
               bRetry = hb_errLaunch( pError );
            }
            else
            {
               bRetry = FALSE;
            }
         } while( bRetry );

         if( pError )
         {
            hb_itemRelease( pError );
            pError = NULL;
         }

         if ( lStart > -1 )
         {
            // Only reposition if a starting record was specified or implied.
            if ( lStart == 0 )
            {
               // DBGOTOP()
               SELF_GOTOP( pArea );
            }
            else
            {
               // DBGOTO( lStart )
               HB_ITEM pGoto;
               pGoto.type = HB_IT_NIL;
               hb_itemPutNL( &pGoto, lStart );
               SELF_GOTOID( pArea, &pGoto );
               // Clean up
               hb_itemClear( &pGoto );
            }
         }

         // Doing things now
         hb_Dbf2Sql( pWhile, pFor, pFields, cDelim, handle, ( BYTE *)cSeparator, lCount, cTable, cHeader );

         hb_fsClose( handle );
      }
   }
   else
   {
      // No workarea to do the job
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBDELIM" );
   }
}


#if 0
#endif
