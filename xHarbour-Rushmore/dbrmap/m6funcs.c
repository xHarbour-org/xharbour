/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    Mach SIx compatible library
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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


#include "hbrddrm.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbvm.h"

#if defined( _MSC_VER ) && ! defined( __POCC__ )
   #define snprintf _snprintf
#endif

static ULONG s_ulRecords = 0;

static BOOL hb_rmCheckRecRange( PHB_RMFILTER pRM, ULONG ulRec )
{
   if( --ulRec < pRM->ulRecords )
      return TRUE;

   hb_rmSetError( M6ERR_RECRANGE );
   return FALSE;
}

/* for compatibility with Clipper MachSIX */
HB_FUNC( M6INIT ) { ; }

HB_FUNC( M6_ERROR )
{
   hb_retni( hb_rmGetError() );
}

HB_FUNC( M6_NEWFILTER )
{
   PHB_ITEM pQuery = hb_param( 1, HB_IT_STRING );
   PHB_RMFILTER pRM = NULL;

   if( pQuery )
   {
      AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();
      if( pArea )
         pRM = hb_rmNewQuery( pArea, hb_param( 1, HB_IT_ANY ) );
      if( pRM )
         s_ulRecords = hb_rmCountRecords( pRM );
   }
   else
   {
      pQuery = hb_param( 1, HB_IT_NUMERIC );
      if( pQuery )
         pRM = hb_rmCreate( hb_itemGetNL( pQuery ) );
      else
      {
         AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         ULONG ulRecords;

         if( ! pArea )
            hb_rmSetError( M6ERR_NOTABLE );
         else if( SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == SUCCESS )
            pRM = hb_rmCreate( ulRecords );
         else
            hb_rmSetError( M6ERR_TYPE );
      }
      s_ulRecords = 0;
   }
   hb_retni( pRM ? pRM->iHandle : 0 );
}

HB_FUNC( M6_RECCOUNT )
{
   hb_retnint( s_ulRecords );
}

HB_FUNC( M6_FREEFILTER )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      if( pRM->iArea || pRM->fLocked )
         pRM = NULL;
      else
         hb_rmDestroy( pRM );
   }
   hb_retl( pRM != NULL );
}

HB_FUNC( M6_GETAREAFILTER )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmGetAreaFilter();
   if( pRM )
   {
      iHandle = pRM->iHandle;
   }
   hb_retni( iHandle );
}

HB_FUNC( M6_SETAREAFILTER )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   BOOL fResult = FALSE;

   if( pRM )
      fResult = hb_rmSetAreaFilter( pRM );

   hb_retl( fResult );
}

HB_FUNC( M6_CHGOWNER )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      hb_rmDetach( pRM );
   }
   hb_retl( pRM != NULL );
}

HB_FUNC( M6_FILTADDREC )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   ULONG ulRec = hb_parnl( 2 );

   if( pRM && hb_rmCheckRecRange( pRM, ulRec ) )
   {
      hb_rmSetRecord( pRM, ulRec );
      hb_retl( TRUE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( M6_FILTDROPREC )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   ULONG ulRec = hb_parnl( 2 );

   if( pRM && hb_rmCheckRecRange( pRM, ulRec ) )
   {
      hb_rmClearRecord( pRM, ulRec );
      hb_retl( TRUE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( M6_FILTCHGREC )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   ULONG ulRec = hb_parnl( 2 );

   if( pRM && hb_rmCheckRecRange( pRM, ulRec ) )
   {
      if( hb_rmTestRecord( pRM, ulRec ) )
         hb_rmClearRecord( pRM, ulRec );
      else
         hb_rmSetRecord( pRM, ulRec );
      hb_retl( TRUE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( M6_ISFILTREC )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   ULONG ulRec = hb_parnl( 2 );

   if( pRM && hb_rmCheckRecRange( pRM, ulRec ) )
   {
      hb_retl( hb_rmTestRecord( pRM, ulRec ) );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( M6_FILTCOUNT )
{
   ULONG ulCount = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
      ulCount = hb_rmCountRecords( pRM );
   hb_retnint( ulCount );
}

HB_FUNC( M6_FILTCOPY )
{
   int iHandle = 0;
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      pRM = hb_rmDup( pRM );
      if( pRM )
      {
         iHandle = pRM->iHandle;
      }
   }
   hb_retni( iHandle );
}

HB_FUNC( M6_FILTINVERSE )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
      pRM = hb_rmNOT( pRM );

   hb_retl( pRM != NULL );
}

HB_FUNC( M6_FILTJOIN )
{
   PHB_RMFILTER pRM1 = hb_rmGetFilterPtr( hb_parni( 1 ) ),
                pRM2 = hb_rmGetFilterPtr( hb_parni( 2 ) ),
                pRM = NULL;

   if( pRM1 && pRM2 )
   {
      BOOL fLocked = FALSE;

      if( pRM2->iArea && !pRM2->fLocked )
      {
         pRM2->fLocked = fLocked = TRUE;
      }
      switch( hb_parni( 3 ) )
      {
         case JOIN_UNION:
            pRM = hb_rmOR( pRM1, pRM2 );
            break;
         case JOIN_INTERSECT:
            pRM = hb_rmAND( pRM1, pRM2 );
            break;
         case JOIN_DIFFERENCE:
            pRM = hb_rmXOR( pRM1, pRM2 );
            break;
         default:
            hb_rmSetError( M6ERR_JOINTYPE );
      }
      if( fLocked )
      {
         pRM2->fLocked = FALSE;
      }
   }

   hb_retl( pRM != NULL );
}

HB_FUNC( M6_FILTTOP )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      pRM->ulPos = hb_rmNextRecord( pRM, 0 );
      hb_retnint( pRM->ulPos );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( M6_FILTBOTT )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      pRM->ulPos = hb_rmPrevRecord( pRM, pRM->ulRecords );
      hb_retnint( pRM->ulPos );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( M6_FILTGOREC )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   ULONG ulRec = hb_parnl( 2 );

   if( pRM && hb_rmCheckRecRange( pRM, ulRec ) )
   {
      pRM->ulPos = ulRec;
      hb_retnint( pRM->ulPos );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( M6_FILTSKIP )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      LONG lSkip = hb_parnl( 2 );
      if( lSkip == 0 )
         lSkip = 1;

      if( lSkip > 0 )
         do
         {
            pRM->ulPos = hb_rmNextRecord( pRM, pRM->ulPos );
         } while( --lSkip && pRM->ulPos != 0 );
      else
         do
         {
            pRM->ulPos = hb_rmPrevRecord( pRM, pRM->ulPos );
         } while( ++lSkip && pRM->ulPos != 0 );

      hb_retnint( pRM->ulPos );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( M6_ADDSCOPED )
{
   ULONG ulResult = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

      if( pRM )
      {
         ulResult = hb_rmSetLoHi( pArea, pRM,
                                  hb_param( 2, HB_IT_ANY ),
                                  hb_param( 3, HB_IT_ANY ),
                                  hb_param( 4, HB_IT_ANY ),
                                  hb_param( 5, HB_IT_ANY ) );
      }
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }
   hb_retnint( ulResult );
}

HB_FUNC( M6_EVALPARTIAL )
{
   ULONG ulResult = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

      if( pRM )
      {
         PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

         if( pBlock )
         {
            pRM = hb_rmMakeMB( pRM );
            if( pRM )
               ulResult = hb_rmMaybeEval( pArea, pRM, pBlock );
         }
         else
            hb_rmSetError( M6ERR_TYPE );
      }
   }
   else
   {
      hb_rmSetError( M6ERR_NOTABLE );
   }
   hb_retnint( ulResult );
}

HB_FUNC( M6_ISFILTER )
{
   int iOptLvl = RM_OPT_NONE;
   PHB_RMFILTER pRM = hb_rmGetAreaFilter();

   if( pRM )
   {
      iOptLvl = pRM->iOptLvl;
   }
   hb_retni( iOptLvl );
}

HB_FUNC( M6_ISOPTIMIZE )
{
   int iOptLvl = RM_OPT_NONE;
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      AREAP pArea = ( AREAP ) hb_rmGetRMAreaPointer();

      if( pArea )
      {
         BOOL lStrict = hb_parl( 2 ), fFree = FALSE;
         PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

         if( lStrict && hb_setGetDeleted() )
         {
            char * szQuery;

            szQuery = ( char * ) hb_xgrab( ulLen + 17 );
            szQuery[ 0 ] = '(';
            memcpy( &szQuery[ 1 ], hb_itemGetCPtr( pItem ), ulLen );
            szQuery[ ulLen + 1 ] = ')';
            memcpy( &szQuery[ ulLen + 2 ], ".AND.!DELETED()", 15 );
            szQuery[ ulLen + 17 ] = '\0';
            pItem = hb_itemPutCPtr( NULL, szQuery, ulLen + 17 );
            fFree = TRUE;
         }
         iOptLvl = hb_rmqOptLevel( pArea, pItem );
         if( fFree )
            hb_itemRelease( pItem );
      }
   }
   else
   {
      hb_rmSetError( M6ERR_TYPE );
   }
   hb_retni( iOptLvl );
}

HB_FUNC( M6_FILTINFO )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );

   if( pRM )
   {
      PHB_ITEM pInfo = hb_itemNew( NULL );

      hb_arrayNew( pInfo, 7 );

      if( pRM->pExpr )
         hb_arraySet( pInfo, INFO_EXPR, pRM->pExpr );
      else
         hb_itemPutC( hb_arrayGetItemPtr( pInfo, INFO_EXPR ), "" );

      if( pRM->pNonExpr )
         hb_arraySet( pInfo, INFO_NONEXPR, pRM->pNonExpr );
      else
         hb_itemPutC( hb_arrayGetItemPtr( pInfo, INFO_NONEXPR ), "" );

      hb_itemPutNI( hb_arrayGetItemPtr( pInfo, INFO_OPTLVL ), pRM->iOptLvl );
      hb_itemPutNInt( hb_arrayGetItemPtr( pInfo, INFO_COUNT ),
                      hb_rmCountRecords( pRM ) );
      hb_itemPutNInt( hb_arrayGetItemPtr( pInfo, INFO_SIZE ), pRM->ulRecords );
      hb_itemPutNI( hb_arrayGetItemPtr( pInfo, INFO_OWNER ),
                    pRM->iArea ? OWN_SYSTEM : OWN_USER );
      hb_itemPutNInt( hb_arrayGetItemPtr( pInfo, INFO_POS ), pRM->ulPos );

      hb_itemReturn( pInfo );
      hb_itemRelease( pInfo );
   }
   else
      hb_ret();
}

HB_FUNC( M6_FILTRESTORE )
{
   const char * szFile = hb_parc( 1 );
   int iHandle = 0;

   if( szFile && *szFile )
   {
      PHB_RMFILTER pRM = hb_rmRestore( szFile );
      if( pRM )
         iHandle = pRM->iHandle;
   }
   else
   {
      hb_rmSetError( M6ERR_TYPE );
   }
   hb_retni( iHandle );
}

HB_FUNC( M6_FILTSAVE )
{
   PHB_RMFILTER pRM = hb_rmGetFilterPtr( hb_parni( 1 ) );
   BOOL fResult = FALSE;

   if( pRM )
   {
      const char * szFile = hb_parc( 2 );

      if( szFile && *szFile )
         fResult = hb_rmSave( pRM, szFile );
      else
         hb_rmSetError( M6ERR_TYPE );
   }
   hb_retl( fResult );
}

HB_FUNC( M6_VERSION )
{
   static const char szVer[] = "0.1";
   char buf[ 128 ];

   switch( hb_parni( 1 ) )
   {
      case 1:
         hb_retc( __DATE__ );
         break;
      case 2:
         hb_retc( __TIME__ );
         break;
      case 3:
         snprintf( buf, sizeof( buf ), "MachSIx emulation for xHarbour, v%s, %s, %s",
                   szVer, __DATE__, __TIME__ );
         hb_retc( buf );
         break;
      case 0:
      default:
         hb_retc( szVer );
         break;
   }
}
