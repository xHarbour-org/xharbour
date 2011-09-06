/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    RM* RDDs
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
 *
 */

#include "hbrddrm.h"
#include "hbapierr.h"
#include "hbinit.h"
#include "hbset.h"
#include "hbvm.h"
#include "rddsys.ch"

static HB_ERRCODE hb_rmSkip( RMAREAP pArea, HB_LONG lToSkip );
static HB_ERRCODE hb_rmSkipFilter( RMAREAP pArea, HB_LONG lUpDown );
static HB_ERRCODE hb_rmPutRec( RMAREAP pArea, const HB_BYTE * pBuffer );
static HB_ERRCODE hb_rmSetFilter( RMAREAP pArea, LPDBFILTERINFO pFilterInfo );
static HB_ERRCODE hb_rmClearFilter( RMAREAP pArea );
static HB_ERRCODE hb_rmCountScope( RMAREAP pArea, void * pPtr, HB_LONG * plRec );
static HB_ERRCODE hb_rmInfo( RMAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE hb_rmOrderInfo( RMAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pOrderInfo );
static HB_ERRCODE hb_rmExit( LPRDDNODE pRDD );
static HB_ERRCODE hb_rmRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem );

static RDDFUNCS rmSuper;
static const RDDFUNCS rmTable =
{
   /* Movement and positioning methods */
   ( DBENTRYP_BP )    NULL,              /* Bof        */
   ( DBENTRYP_BP )    NULL,              /* Eof        */
   ( DBENTRYP_BP )    NULL,              /* Found      */
   ( DBENTRYP_V )     NULL,              /* GoBottom   */
   ( DBENTRYP_UL )    NULL,              /* GoTo       */
   ( DBENTRYP_I )     NULL,              /* GoToId     */
   ( DBENTRYP_V )     NULL,              /* GoTop      */
   ( DBENTRYP_BIB )   NULL,              /* Seek       */
   ( DBENTRYP_L )     hb_rmSkip,         /* Skip       */
   ( DBENTRYP_L )     hb_rmSkipFilter,   /* SkipFilter */
   ( DBENTRYP_L )     NULL,              /* SkipRaw    */

   /* Data management */
   ( DBENTRYP_VF )    NULL,              /* AddField       */
   ( DBENTRYP_B )     NULL,              /* Append         */
   ( DBENTRYP_I )     NULL,              /* CreateFields   */
   ( DBENTRYP_V )     NULL,              /* DeleteRec      */
   ( DBENTRYP_BP )    NULL,              /* Deleted        */
   ( DBENTRYP_SP )    NULL,              /* FieldCount     */
   ( DBENTRYP_VF )    NULL,              /* FieldDisplay   */
   ( DBENTRYP_SSI )   NULL,              /* FieldInfo      */
   ( DBENTRYP_SCP )   NULL,              /* FieldName      */
   ( DBENTRYP_V )     NULL,              /* Flush          */
   ( DBENTRYP_PP )    NULL,              /* GetRec         */
   ( DBENTRYP_SI )    NULL,              /* GetValue       */
   ( DBENTRYP_SVL )   NULL,              /* GetVarLen      */
   ( DBENTRYP_V )     NULL,              /* GoCold         */
   ( DBENTRYP_V )     NULL,              /* GoHot          */
   ( DBENTRYP_P )     hb_rmPutRec,       /* PutRec         */
   ( DBENTRYP_SI )    NULL,              /* PutValue       */
   ( DBENTRYP_V )     NULL,              /* Recall         */
   ( DBENTRYP_ULP )   NULL,              /* RecCount       */
   ( DBENTRYP_ISI )   NULL,              /* RecInfo        */
   ( DBENTRYP_ULP )   NULL,              /* RecNo          */
   ( DBENTRYP_I )     NULL,              /* RecId          */
   ( DBENTRYP_S )     NULL,              /* SetFieldExtent */

   /* WorkArea/Database management */
   ( DBENTRYP_CP )    NULL,              /* Alias       */
   ( DBENTRYP_V )     NULL,              /* Close       */
   ( DBENTRYP_VO )    NULL,              /* Create      */
   ( DBENTRYP_SI )    hb_rmInfo,         /* Info        */
   ( DBENTRYP_V )     NULL,              /* NewArea     */
   ( DBENTRYP_VO )    NULL,              /* Open        */
   ( DBENTRYP_V )     NULL,              /* Release     */
   ( DBENTRYP_SP )    NULL,              /* StructSize  */
   ( DBENTRYP_CP )    NULL,              /* SysName     */
   ( DBENTRYP_VEI )   NULL,              /* Eval        */
   ( DBENTRYP_V )     NULL,              /* Pack        */
   ( DBENTRYP_LSP )   NULL,              /* PackRec     */
   ( DBENTRYP_VS )    NULL,              /* Sort        */
   ( DBENTRYP_VT )    NULL,              /* Trans       */
   ( DBENTRYP_VT )    NULL,              /* TransRec    */
   ( DBENTRYP_V )     NULL,              /* Zap         */

   /* Relational Methods */
   ( DBENTRYP_VR )    NULL,              /* ChildEnd      */
   ( DBENTRYP_VR )    NULL,              /* ChildStart    */
   ( DBENTRYP_VR )    NULL,              /* ChildSync     */
   ( DBENTRYP_V )     NULL,              /* SyncChildren  */
   ( DBENTRYP_V )     NULL,              /* ClearRel      */
   ( DBENTRYP_V )     NULL,              /* ForceRel      */
   ( DBENTRYP_SSP )   NULL,              /* RelArea       */
   ( DBENTRYP_VR )    NULL,              /* RelEval       */
   ( DBENTRYP_SI )    NULL,              /* RelText       */
   ( DBENTRYP_VR )    NULL,              /* SetRel        */

   /* Order Management */
   ( DBENTRYP_VOI )   NULL,              /* OrderListAdd      */
   ( DBENTRYP_V )     NULL,              /* OrderListClear    */
   ( DBENTRYP_VOI )   NULL,              /* OrderListDelete   */
   ( DBENTRYP_VOI )   NULL,              /* OrderListFocus    */
   ( DBENTRYP_V )     NULL,              /* OrderListRebuild  */
   ( DBENTRYP_VOO )   NULL,              /* OrderCondition    */
   ( DBENTRYP_VOC )   NULL,              /* OrderCreate       */
   ( DBENTRYP_VOI )   NULL,              /* OrderDestroy      */
   ( DBENTRYP_SVOI )  hb_rmOrderInfo,    /* OrderInfo         */

   /* Filters and Scope Settings */
   ( DBENTRYP_V )     hb_rmClearFilter,  /* ClearFilter  */
   ( DBENTRYP_V )     NULL,              /* ClearLocate  */
   ( DBENTRYP_V )     NULL,              /* ClearScope   */
   ( DBENTRYP_VPLP )  hb_rmCountScope,   /* CountScope   */
   ( DBENTRYP_I )     NULL,              /* FilterText   */
   ( DBENTRYP_SI )    NULL,              /* ScopeInfo    */
   ( DBENTRYP_VFI )   hb_rmSetFilter,    /* SetFilter    */
   ( DBENTRYP_VLO )   NULL,              /* SetLocate    */
   ( DBENTRYP_VOS )   NULL,              /* SetScope     */
   ( DBENTRYP_VPL )   NULL,              /* SkipScope    */
   ( DBENTRYP_B )     NULL,              /* Locate       */

   /* Miscellaneous */
   ( DBENTRYP_CC )    NULL,              /* Compile    */
   ( DBENTRYP_I )     NULL,              /* Error      */
   ( DBENTRYP_I )     NULL,              /* EvalBlock  */

   /* Network operations */
   ( DBENTRYP_VSP )   NULL,              /* RawLock  */
   ( DBENTRYP_VL )    NULL,              /* Lock     */
   ( DBENTRYP_I )     NULL,              /* UnLock   */

   /* Memofile functions */
   ( DBENTRYP_V )     NULL,              /* CloseMemFile   */
   ( DBENTRYP_VO )    NULL,              /* CreateMemFile  */
   ( DBENTRYP_SCCS )  NULL,              /* GetValueFile   */
   ( DBENTRYP_VO )    NULL,              /* OpenMemFile    */
   ( DBENTRYP_SCCS )  NULL,              /* PutValueFile   */

   /* Database file header handling */
   ( DBENTRYP_V )     NULL,              /* ReadDBHeader   */
   ( DBENTRYP_V )     NULL,              /* WriteDBHeader  */

   /* non WorkArea functions */
   ( DBENTRYP_R )     NULL,              /* Init    */
   ( DBENTRYP_R )     hb_rmExit,         /* Exit    */
   ( DBENTRYP_RVVL )  NULL,              /* Drop    */
   ( DBENTRYP_RVVL )  NULL,              /* Exists  */
   ( DBENTRYP_RVVVL ) NULL,              /* Rename  */
   ( DBENTRYP_RSLV )  hb_rmRddInfo,      /* RddInfo */

   /* Special and reserved methods */
   ( DBENTRYP_SVP )   NULL               /* WhoCares */
};


/*
 * get current order number
 */
static HB_ERRCODE hb_rmOrderNum( RMAREAP pArea, int * piOrder )
{
   DBORDERINFO OrderInfo;
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmOrderNum(%p,%p)", pArea, piOrder));

   memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
   if( ! pArea->valResult )
      pArea->valResult = hb_itemNew( NULL );
   OrderInfo.itmResult = pArea->valResult;
   errCode = SUPER_ORDINFO( ( AREAP ) pArea, DBOI_NUMBER, &OrderInfo );

   * piOrder = hb_itemGetNI( OrderInfo.itmResult );

   return errCode;
}

/*
 * check filter condition
 */
static HB_BOOL hb_rmEvalFilter( RMAREAP pArea, HB_BOOL fUpdate )
{
   HB_ULONG ulRecNo = 0;
   HB_BOOL fResult = HB_TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmEvalFilter(%p,%d)", pArea, fUpdate));

   if( SELF_RMFILTER( pArea ) )
   {
      SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
      if( !fUpdate && ! hb_rmTestRecord( SELF_RMFILTER( pArea ), ulRecNo ) )
      {
         return HB_FALSE;
      }
   }

   if( pArea->dbfi.itmCobExpr )
   {
      PHB_ITEM pResult;
      pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
      fResult = !HB_IS_LOGICAL( pResult ) || hb_itemGetL( pResult );
   }
   if( fResult && hb_setGetDeleted() )
   {
      HB_BOOL fDeleted;
      SELF_DELETED( ( AREAP ) pArea, &fDeleted );
      fResult = !fDeleted;
   }

   if( SELF_RMFILTER( pArea ) )
   {
      if( !fResult )
      {
         hb_rmClearRecord( SELF_RMFILTER( pArea ), ulRecNo );
      }
      else
      {
         hb_rmSetRecord( SELF_RMFILTER( pArea ), ulRecNo );
      }
   }
   return fResult;
}

/* ( DBENTRYP_L )     hb_rmSkip */
/*
 * Reposition cursor relative to current position.
 */
static HB_ERRCODE hb_rmSkip( RMAREAP pArea, HB_LONG lToSkip )
{
   int iOrder = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmSkip(%p, %ld)", pArea, lToSkip));

   if( lToSkip == 0 || !SELF_RMFILTER( pArea ) )
      return SUPER_SKIP( pArea, lToSkip );

   if( hb_rmOrderNum( pArea, &iOrder ) != HB_SUCCESS )
      return HB_FAILURE;

   if( iOrder == 0 )
   {
      HB_ULONG ulRecNo;
      HB_BOOL fBottom;

      if( SELF_RECNO( ( AREAP ) pArea, &ulRecNo ) != HB_SUCCESS )
         return HB_FAILURE;

      fBottom = pArea->fBottom;
      pArea->fTop = pArea->fBottom = HB_FALSE;
      if( lToSkip > 0 )
      {
         while( --lToSkip >= 0 && ulRecNo )
         {
            do
            {
               ulRecNo = hb_rmNextRecord( SELF_RMFILTER( pArea ), ulRecNo );
               if( SELF_GOTO( pArea, ulRecNo ) != HB_SUCCESS )
                  return HB_FAILURE;
            }
            while( ulRecNo && !hb_rmEvalFilter( pArea, HB_FALSE ) );
         }
         pArea->fBof = HB_FALSE;
      }
      else
      {
         while( ++lToSkip <= 0 && ulRecNo )
         {
            do
            {
               ulRecNo = hb_rmPrevRecord( SELF_RMFILTER( pArea ), ulRecNo );
               if( SELF_GOTO( pArea, ulRecNo ) != HB_SUCCESS )
                  return HB_FAILURE;
            }
            while( ulRecNo && !hb_rmEvalFilter( pArea, HB_FALSE ) );
         }
         if( ulRecNo == 0 )
         {
            if( fBottom )
            {
               if( SELF_GOTO( ( AREAP ) pArea, 0 ) != HB_SUCCESS )
                  return HB_FAILURE;
            }
            else
            {
               if( SELF_GOTOP( ( AREAP ) pArea ) != HB_SUCCESS )
                  return HB_FAILURE;
               pArea->fBof = HB_TRUE;
            }
            pArea->fEof = HB_FALSE;
         }
      }
      return HB_SUCCESS;
   }
   return SUPER_SKIP( pArea, lToSkip );
}

/* ( DBENTRYP_L )     hb_rmSkipFilter */
/*
 * Reposition cursor respecting any filter setting.
 */
static HB_ERRCODE hb_rmSkipFilter( RMAREAP pArea, HB_LONG lUpDown )
{
   HB_BOOL fBottom;
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmSkipFilter(%p, %ld)", pArea, lUpDown));

   if( !hb_setGetDeleted() && pArea->dbfi.itmCobExpr == NULL && !SELF_RMFILTER( pArea ) )
      return HB_SUCCESS;

   lUpDown = ( lUpDown < 0  ? -1 : 1 );
   fBottom = pArea->fBottom;
   while( !pArea->fBof && !pArea->fEof && !hb_rmEvalFilter( pArea, HB_FALSE ) )
   {
      errCode = SELF_SKIPRAW( ( AREAP ) pArea, lUpDown );
      if( errCode != HB_SUCCESS )
         return errCode;
   }

   if( pArea->fBof && lUpDown < 0 )
   {
      if( fBottom )
      {
         errCode = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      else
      {
         errCode = SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = HB_TRUE;
      }
   }
   else
   {
      errCode = HB_SUCCESS;
   }
   return errCode;
}

/* ( DBENTRYP_P )     hb_rmPutRec */
static HB_ERRCODE hb_rmPutRec( RMAREAP pArea, const HB_BYTE * pBuffer )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmPutRec(%p,%p)", pArea, pBuffer));

   errCode = SUPER_PUTREC( ( AREAP ) pArea, pBuffer );

   if( pBuffer == NULL && errCode == HB_SUCCESS && SELF_RMFILTER( pArea ) )
   {
      hb_rmEvalFilter( pArea, HB_TRUE );
   }

   return errCode;
}

/* ( DBENTRYP_V )     hb_rmClearFilter */
static HB_ERRCODE hb_rmClearFilter( RMAREAP pArea )
{
   HB_ERRCODE errCode;

   errCode = SUPER_CLEARFILTER( ( AREAP ) pArea );

   SELF_RMRELEASE( pArea );

   return errCode;
}

/* ( DBENTRYP_VFI )   hb_rmSetFilter */
static HB_ERRCODE hb_rmSetFilter( RMAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_ERRCODE errCode;

   errCode = SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( hb_setGetOptimize() )
   {
      PHB_RMFILTER pRM = hb_rmNewQuery( ( AREAP ) pArea, pFilterInfo->abFilterText );

      if( pRM )
      {
         SELF_RMSET( pArea, pRM );
         if( hb_setGetForceOpt() )
         {
            hb_rmDoLinear( ( AREAP ) pArea );
         }
      }
   }
   return HB_SUCCESS;
}

/* ( DBENTRYP_VPLP )  hb_rmCountScope */
static HB_ERRCODE hb_rmCountScope( RMAREAP pArea, void * pPtr, HB_LONG * plRec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rmCountScope(%p, %p, %p)", pArea, pPtr, plRec));

   if( pPtr == NULL )
   {
      if( SELF_RMFILTER( pArea ) && pArea->dbfi.fFilter &&
          ! hb_rmTestRecord( SELF_RMFILTER( pArea ), ( HB_ULONG ) *plRec ) )
      {
         *plRec = 0;
      }
      return HB_SUCCESS;
   }
   return SUPER_COUNTSCOPE( ( AREAP ) pArea, pPtr, plRec );
}

/* ( DBENTRYP_SI )    hb_rmInfo */
/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_rmInfo( RMAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   HB_ULONG ulRecords;
   HB_ERRCODE errCode;
   PHB_RMFILTER pRM;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_RM_SUPPORTED:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case DBI_RM_CREATE:
         errCode = SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords );
         if( errCode != HB_SUCCESS )
            return errCode;
         SELF_RMRELEASE( pArea );
         pRM = hb_rmCreate( ulRecords );
         SELF_RMSET( pArea, pRM );
         hb_itemPutL( pItem, pRM != NULL );
         break;

      case DBI_RM_REMOVE:
         SELF_RMRELEASE( pArea );
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case DBI_RM_CLEAR:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_rmClear( SELF_RMFILTER( pArea ) );
            hb_itemPutL( pItem, HB_TRUE );
         }
         else
         {
            hb_itemPutL( pItem, HB_FALSE );
         }
         break;

      case DBI_RM_FILL:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_rmFill( SELF_RMFILTER( pArea ) );
            hb_itemPutL( pItem, HB_TRUE );
         }
         else
         {
            hb_itemPutL( pItem, HB_FALSE );
         }
         break;

      case DBI_RM_ADD:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_rmSetRecord( SELF_RMFILTER( pArea ), hb_itemGetNL( pItem ) );
            hb_itemPutL( pItem, HB_TRUE );
         }
         else
         {
            hb_itemPutL( pItem, HB_FALSE );
         }
         break;

      case DBI_RM_DROP:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_rmClearRecord( SELF_RMFILTER( pArea ), hb_itemGetNL( pItem ) );
            hb_itemPutL( pItem, HB_TRUE );
         }
         else
         {
            hb_itemPutL( pItem, HB_FALSE );
         }
         break;

      case DBI_RM_TEST:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_itemPutL( pItem, hb_rmTestRecord( SELF_RMFILTER( pArea ),
                                                 hb_itemGetNL( pItem ) ) );
         }
         else
         {
            hb_itemPutL( pItem, HB_TRUE );
         }
         break;

      case DBI_RM_COUNT:
         if( SELF_RMFILTER( pArea ) )
         {
            hb_itemPutNInt( pItem, hb_rmCountRecords( SELF_RMFILTER( pArea ) ) );
         }
         else
         {
            hb_itemPutNI( pItem, 0 );
         }
         break;

      case DBI_RM_HANDLE:
      {
         HB_BOOL fSet = ( hb_itemType( pItem ) & HB_IT_NUMERIC ) != 0;
         int iHandle = hb_itemGetNI( pItem );

         if( SELF_RMFILTER( pArea ) )
         {
            hb_itemPutNI( pItem, SELF_RMFILTER( pArea )->iHandle );
            if( SELF_RMFILTER( pArea )->iHandle == iHandle )
            {
               fSet = HB_FALSE;
            }
            else if( fSet )
            {
               SELF_RMFILTER( pArea )->iArea = 0;
            }
         }
         else
         {
            hb_itemPutNI( pItem, 0 );
         }
         if( fSet )
         {
            pRM = hb_rmGetFilterPtr( iHandle );
            if( pRM && pRM->iArea != 0 )
               pRM = NULL;
            SELF_RMSET( pArea, pRM );
         }
         break;
      }

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );

   }

   return HB_SUCCESS;
}

/* ( DBENTRYP_SVOI )   hb_rmOrderInfo */
/*
 * Provides information about order management.
 */
static HB_ERRCODE hb_rmOrderInfo( RMAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   PHB_RMFILTER pRM = NULL;
   PHB_ITEM pItem = NULL;
   HB_ERRCODE errCode;
   int iOrder = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_rmOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));

   switch( uiIndex )
   {
      case DBOI_OPTLEVEL:
         pOrderInfo->itmResult = hb_itemPutNI( pOrderInfo->itmResult,
                  SELF_RMFILTER( pArea ) ? SELF_RMFILTER( pArea )->iOptLvl : RM_OPT_NONE );
         return HB_SUCCESS;

      case DBOI_POSITION:
      case DBOI_KEYCOUNT:
         if( SELF_RMFILTER( pArea ) )
         {
            if( hb_rmHasMB( SELF_RMFILTER( pArea ) ) )
               hb_rmDoLinear( ( AREAP ) pArea );
            errCode = hb_rmOrderNum( pArea, &iOrder );
            if( errCode != HB_SUCCESS )
               return errCode;
            if( iOrder == 0 )
            {
               if( uiIndex == DBOI_KEYCOUNT )
                  pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                                 hb_rmCountRecords( SELF_RMFILTER( pArea ) ) );
               else
               {
                  HB_ULONG ulRecNo;
                  SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
                  pOrderInfo->itmResult = hb_itemPutNL( pOrderInfo->itmResult,
                           hb_rmRecordPos( SELF_RMFILTER( pArea ), ulRecNo ) );
               }
               return HB_SUCCESS;
            }
         }
#ifdef OPTIMIZE_LOGICAL_POSITION /* it's not compatible with CL53 */
         else
#endif
         {
            pItem = pArea->dbfi.itmCobExpr;
            pArea->dbfi.itmCobExpr = NULL;
         }
         break;

      case DBOI_KEYNORAW:
      case DBOI_KEYCOUNTRAW:
         pRM = SELF_RMFILTER( pArea );
         SELF_RMDETACH( pArea );
         break;

      default:
         break;
   }

   errCode = SUPER_ORDINFO( ( AREAP ) pArea, uiIndex, pOrderInfo );

   if( pRM )
   {
      SELF_RMSET( pArea, pRM );
   }
   if( pItem )
      pArea->dbfi.itmCobExpr = pItem;

   return errCode;
}

/* ( DBENTRYP_R )   hb_rmExit */
/*
 * Unregister RDD
 */
static HB_ERRCODE hb_rmExit( LPRDDNODE pRDD )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rmExit(%p)", pRDD));

   HB_SYMBOL_UNUSED( pRDD );
   hb_rmDestroyAll();

   if( ISSUPER_EXIT( pRDD ) )
   {
      SUPER_EXIT( pRDD );
   }
   hb_rmDelRddID( pRDD->rddID );

   return HB_SUCCESS;
}

/* ( DBENTRYP_RSLV )   hb_rmRddInfo */
/*
 * Get/Set RDD parameters
 */
static HB_ERRCODE hb_rmRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rmRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_RECORDMAP:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return HB_SUCCESS;
}

#if   defined( HB_RMDBFNTX )
#  define HB_RM_SUPER   "DBFNTX"
#elif defined( HB_RMDBFNSX )
#  define HB_RM_SUPER   "DBFNSX"
#else /* HB_RMDBFCDX */
#  define HB_RM_SUPER   "DBFCDX"
#endif

HB_FUNC_STATIC( RMDBF_GETFUNCTABLE )
{
   RDDFUNCS * pTable, * pSuperTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   pSuperTable = ( RDDFUNCS * ) hb_parptr( 3 );
   uiRddId = ( HB_USHORT ) hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   HB_TRACE(HB_TR_DEBUG, ("RM" HB_RM_SUPER "_GETFUNCTABLE(%p, %p, %p, %hu, %p)", puiCount, pTable, pSuperTable, uiRddId, puiSuperRddId));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         * puiCount = RDDFUNCSCOUNT;
      if( !pSuperTable )
         pSuperTable = &rmSuper;

      errCode = hb_rddInheritEx( pTable, &rmTable, pSuperTable, HB_RM_SUPER, puiSuperRddId );
      hb_retni( errCode );
      if( errCode == HB_SUCCESS )
      {
         hb_rmSetRddID( uiRddId );
         if( pSuperTable != &rmSuper )
            memcpy( &rmSuper, pSuperTable, sizeof( RDDFUNCS ) );
      }
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_rmRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF", RDT_FULL ) <= 1 )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( HB_RM_SUPER, RDT_FULL ) <= 1 &&
          hb_rddRegister( "RM" HB_RM_SUPER, RDT_FULL ) <= 1 )
         return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

#if defined( HB_RMDBFNTX )
HB_FUNC_EXTERN( DBFNTX ); HB_FUNC( RMDBFNTX ) { HB_FUNC_EXEC( DBFNTX ); }
HB_INIT_SYMBOLS_BEGIN( _hb_rm_InitSymbols_ )
{ "RMDBFNTX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBFNTX )}, NULL },
{ "RMDBFNTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( _hb_rm_InitSymbols_ )
#elif defined( HB_RMDBFNSX )
HB_FUNC_EXTERN( DBFNSX ); HB_FUNC( RMDBFNSX ) { HB_FUNC_EXEC( DBFNSX ); }
HB_INIT_SYMBOLS_BEGIN( _hb_rm_InitSymbols_ )
{ "RMDBFNSX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBFNSX )}, NULL },
{ "RMDBFNSX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( _hb_rm_InitSymbols_ )
#else /* HB_RMDBFCDX */
HB_FUNC_EXTERN( DBFCDX ); HB_FUNC( RMDBFCDX ) { HB_FUNC_EXEC( DBFCDX ); }
HB_INIT_SYMBOLS_BEGIN( _hb_rm_InitSymbols_ )
{ "RMDBFCDX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBFCDX )}, NULL },
{ "RMDBFCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( RMDBF_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( _hb_rm_InitSymbols_ )
#endif

HB_CALL_ON_STARTUP_BEGIN( _hb_rm_rdd_init_ )
   hb_vmAtInit( hb_rmRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_rm_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
#  pragma startup _hb_rm_InitSymbols_
#  pragma startup _hb_rm_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_rm_InitSymbols_ ) \
                              HB_DATASEG_FUNC( _hb_rm_rdd_init_ )
   #include "hbiniseg.h"
#endif
