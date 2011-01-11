/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbvm.h"
#include "rddsys.ch"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "hbstack.h"
#include "rddsix.h"

#define SUPERTABLE ( &sixSuper )

extern RDDFUNCS sixSuper;
extern USHORT usSixRddID;
extern int sixFileType;
extern int iSxCurrArea;
extern BOOL fTriggerActive;
extern char cTriggerName[];
extern USHORT usDateFormat;
extern BOOL fCentury;
extern BOOL fMemoBinary;

extern int hb_sxBag( char* szBagName, int *iFirstTag );


int cmpmsk( char *mask, char *strcmp )
{
   char symb, * mptr, * sptr;
   int i, lenmask = strlen( mask );

   for( i=0,mptr=mask,sptr=strcmp; i<lenmask; i++,mptr++ )
   {
      symb = *mptr;
      if( symb == '*' )
      {
         if( i == lenmask-1 )
            return TRUE;
         while( TRUE )
         {
            sptr = strchr( sptr,*(mptr+1) );
            if( sptr )
            {
               if( cmpmsk(sptr,mptr+1) )
                  return TRUE;
               sptr++;
            }
            else
               return FALSE;
         }
      }
      else if( symb == '?' )
         sptr++;
      else
      {
        if( symb == *sptr )
           sptr++;
        else
           return FALSE;
      }
   }
   return TRUE;
}

SIXAREAP getCurrentArea( void )
{
   SIXAREAP pArea = (SIXAREAP) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      if( pArea->area.rddID != usSixRddID )
      {
         pArea = NULL;
      }
      else if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
   }
   return pArea;
}

static void __m6FltRestore( SIXAREAP pArea, ULONG ulRecNo )
{
   if( pArea->iFltCurrent )
      sx_RYOFilterActivate( pArea->iFltCurrent, RYOFILTER_NEW );
   else if( pArea->area.dbfi.abFilterText )
   {
      if( pArea->fFltOptimized )
         sx_Query( (unsigned char *) hb_itemGetCPtr( pArea->area.dbfi.abFilterText ) );
      else
         sx_SetFilter( (unsigned char *) hb_itemGetCPtr( pArea->area.dbfi.abFilterText ) );
   }
   else
      sx_Query( NULL );
   sx_Go( ulRecNo );
}

/* M6_* group of functions */

HB_FUNC( M6_FILTADDREC )
{
   getCurrentArea();
   hb_retl( sx_RYOFilterSetBit( (SHORT) hb_parni(1), hb_parnl(2), TRUE ) );
}

HB_FUNC( M6_FILTBOTT )
{
   getCurrentArea();
}

HB_FUNC( M6_FILTCHGREC )
{
   SHORT handle  = (SHORT)hb_parni(1);
   LONG  ulRecno = (LONG)hb_parnl(2);

   getCurrentArea();
   hb_retl( sx_RYOFilterSetBit( handle, ulRecno,
           !sx_RYOFilterGetBit( handle, ulRecno ) ) );
}

HB_FUNC( M6_FILTCOPY )
{
   SHORT iHandle = hb_parni(1);
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      ULONG ulRecNo = sx_RecNo();
      sx_RYOFilterActivate( iHandle, RYOFILTER_NEW );
      hb_retnl( sx_RYOFilterCopy() );
      __m6FltRestore( pArea, ulRecNo );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( M6_FILTCOUNT )
{
   SIXAREAP pArea = getCurrentArea();
   SHORT iHandle  = (SHORT)hb_parni(1);

   if( pArea )
   {
      if( iHandle == pArea->iFltCurrent )
         hb_retnl( sx_QueryRecCount() );
      else
      {
         ULONG ulRecNo = sx_RecNo();
         sx_RYOFilterActivate( iHandle, RYOFILTER_NEW );
         hb_retnl( sx_QueryRecCount() );
         __m6FltRestore( pArea, ulRecNo );
      }
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( M6_FILTDROPREC )
{
   getCurrentArea();
   hb_retl( sx_RYOFilterSetBit( (SHORT) hb_parni(1), hb_parnl(2), FALSE ) );
}

HB_FUNC( M6_FILTGOREC )
{
   getCurrentArea();
}

HB_FUNC( M6_FILTINFO )
{
   PHB_ITEM aFilter = hb_itemArrayNew( 7 );
   PHB_ITEM temp;
   SHORT iHandle = hb_parni(1);
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      long int nRecords;

      if( iHandle == pArea->iFltCurrent )
         nRecords = sx_QueryRecCount();
      else
      {
         ULONG ulRecNo = sx_RecNo();
         sx_RYOFilterActivate( iHandle, RYOFILTER_NEW );
         nRecords = sx_QueryRecCount();
         __m6FltRestore( pArea, ulRecNo );
      }

      temp = hb_itemPutC( NULL, "" );       // Complete filter expression
      hb_itemArrayPut( aFilter, 1, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutC( NULL, "" );       // non-indexed expression
      hb_itemArrayPut( aFilter, 2, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNI( NULL, 0 );       // Optimization level
      hb_itemArrayPut( aFilter, 3, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, nRecords ); // Records count
      hb_itemArrayPut( aFilter, 4, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, 0 );       // Maximum number of records
      hb_itemArrayPut( aFilter, 5, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNI( NULL, (iHandle==pArea->iFltCurrent)? 1:2 );       // Filter Owner
      hb_itemArrayPut( aFilter, 6, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNI( NULL, 1 );       // Current position
      hb_itemArrayPut( aFilter, 7, temp );
      hb_itemRelease( temp );

      hb_itemReturnForward( aFilter );
   }
   hb_itemRelease( aFilter );
}

HB_FUNC( M6_FILTJOIN )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea && ISNUM(1) && ISNUM(2) && ISNUM(3) )
   {
      SHORT handle1 = hb_parni(1), handle2 = hb_parni(2);
      SHORT iJoinType = hb_parni(3);
      BOOL  lResult;

      SUPER_CLEARFILTER( ( AREAP ) pArea );
      lResult = sx_RYOFilterActivate( handle2, RYOFILTER_NEW );
      if( lResult )
      {
         lResult = sx_RYOFilterActivate( handle1,
                    ( iJoinType==1 )? RYOFILTER_OR :
                    ( ( iJoinType==2 )? RYOFILTER_AND : RYOFILTER_XOR ) );
         sx_RYOFilterDestroy( handle1 );
         sx_RYOFilterDestroy( handle2 );
         pArea->iFltCurrent = sx_RYOFilterCopy();
      }
      hb_retl( lResult );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( M6_FILTRESTORE )
{
   getCurrentArea();
   if( sx_RYOFilterRestore( (unsigned char *) hb_parc(1) ) )
   {
      hb_retnl( sx_RYOFilterCopy() );
   }
   hb_retnl( 0 );
}

HB_FUNC( M6_FILTSAVE )
{
   getCurrentArea();
   hb_retnl( sx_RYOFilterSave( (SHORT) hb_parni(1), (unsigned char *) hb_parc(2) ) );
}

HB_FUNC( M6_FILTTOP )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      // if( pArea->iFltCurrent && ( hb_pcount()==0 || hb_parni(1)==pArea->iFltCurrent ) )
      //    dbGoTop();
   }
}

HB_FUNC( M6_FREEFILTER )
{
   getCurrentArea();
   hb_retl( sx_RYOFilterDestroy( (SHORT) hb_parni(1) ) );
}

HB_FUNC( M6_GETAREAFILTER )
{

   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      if( pArea->iFltCurrent )
         hb_retni( pArea->iFltCurrent );
      /*
      else if( pArea->fFltOptimized )
      {
         hb_retni( sx_RYOFilterCopy() );
      }
      */
      else if( pArea->area.dbfi.abFilterText )
      {
         ULONG ulSavedRecNo, ulRecNo;
         BOOL bEof = TRUE;
         SHORT iHandle;

         SELF_RECNO( (AREAP) pArea, &ulSavedRecNo );
         iHandle = sx_RYOFilterCreate();
         SELF_GOTOP( (AREAP) pArea );
         while( TRUE )
         {
            SELF_EOF( (AREAP) pArea, &bEof );
            if( bEof )
               break;
            SELF_RECNO( (AREAP) pArea, &ulRecNo );
            sx_RYOFilterSetBit( iHandle, ulRecNo, TRUE );
            SELF_SKIP( (AREAP) pArea, 1 );
         }
         SELF_GOTO( (AREAP) pArea, ulSavedRecNo );
         hb_retni( iHandle );
      }
      else
         hb_retni(0);
   }
   else
      hb_retni(0);
}

HB_FUNC( M6_GETCURRENTFILTER )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
      hb_retni( pArea->iFltCurrent );
   else
      hb_retni(0);
}

HB_FUNC( M6_ISFILTREC )
{
   getCurrentArea();
   hb_retl( sx_RYOFilterGetBit( (SHORT)hb_parni(1), (LONG)hb_parnl(2) ) );
}

HB_FUNC( M6_ISOPTIMIZE )
{
   getCurrentArea();
   hb_retni( sx_QueryTest( (unsigned char *) hb_parc(1) ) );
}

HB_FUNC( M6_NEWFILTER )
{
   SHORT iFilter;

   getCurrentArea();
   if( ISCHAR(1) )
   {
      sx_Query( (unsigned char *) hb_parc(1) );
      iFilter = sx_RYOFilterCopy();
      sx_Query( NULL );
   }
   else
      iFilter = sx_RYOFilterCreate();

   hb_retni( iFilter );
}

HB_FUNC( M6_RECCOUNT )
{
   getCurrentArea();
   hb_retnl( sx_QueryRecCount() );
}

HB_FUNC( M6_SETAREAFILTER )
{
   SHORT iFilter = hb_parni(1);
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      BOOL lResult;
      SELF_CLEARFILTER( ( AREAP ) pArea );
      lResult = sx_RYOFilterActivate( iFilter, RYOFILTER_NEW );
      hb_retl( lResult );
      if( lResult )
         pArea->iFltCurrent = iFilter;
   }
   else
      hb_retl(0);
}

/* Sx_* group of functions */

HB_FUNC( SX_AUTOOPEN )
{
   BOOL lAuto = !sx_SysProp( SDE_SP_GETDISABLEAUTO,NULL );
   if( ISLOG(1) )
      sx_SysProp( SDE_SP_SETDISABLEAUTO, (PVOID)(!hb_parl(1)) );
   hb_retl( lAuto );
}

HB_FUNC( SX_BLOB2FILE )
{

   getCurrentArea();
   if( ISCHAR(1) && ISCHAR(2) )
   {
      char * szFileName, * szFieldName;

      szFileName  = (char *) hb_parc(1);
      szFieldName = (char *) hb_parc(2);
      hb_retl( sx_BlobToFile( (unsigned char *) szFieldName, (unsigned char *) szFileName ) );
   }
   else
      hb_retl(0);
   return;

}

HB_FUNC( SX_DBFDECRYPT )
{
   getCurrentArea();
   if( hb_pcount() == 0 )
   {
      hb_retl( sx_DbfDecrypt() );
   }
}

HB_FUNC( SX_DBFENCRYPT )
{
   getCurrentArea();
   if( hb_pcount() == 0 )
   {
      hb_retl( sx_DbfEncrypt() );
   }
}

HB_FUNC( SX_DESCEND )
{

   SHORT iTag;
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      if( hb_pcount() )
      {
         if( ISCHAR(1) )
            iTag = sx_TagArea( (unsigned char *) hb_parc(1) );
         else
            iTag = hb_parni(1);
         if( iTag != pArea->iOrdCurrent )
            sx_SetOrder( iTag );
      }
      else
         iTag = pArea->iOrdCurrent;
      sx_IndexFlip();
      if( iTag != pArea->iOrdCurrent )
         sx_SetOrder( pArea->iOrdCurrent );
   }
}

HB_FUNC( SX_ENCRYPT )
{
   if( ISCHAR(1) )
   {
      char * cString = (char *) hb_parc(1);
      hb_retc( (char*) sx_Encrypt( (unsigned char *) cString, ISCHAR(2) ? (unsigned char *) hb_parc(2): NULL, strlen(cString) ) );
   }
}

HB_FUNC( SX_DECRYPT )
{
   if( ISCHAR(1) )
   {
      char * cString = (char *) hb_parc(1);
      hb_retc( (char*) sx_Decrypt( (unsigned char *) cString, ISCHAR(2) ? (unsigned char *) hb_parc(2) : NULL, strlen(cString) ) );
   }
}

HB_FUNC( SX_ERRORLEVEL )
{
   hb_retni( sx_ErrorLevel( hb_parni(1) ) );
}

HB_FUNC( SX_EVALTEST )
{
   getCurrentArea();
   hb_retni( sx_EvalTest( (unsigned char *) hb_parc(1) ) );
}

HB_FUNC( SX_FILE2BLOB )
{

   getCurrentArea();
   if( ISCHAR(1) && ISCHAR(2) )
   {
      char * szFileName, * szFieldName;

      szFileName  = (char *) hb_parc(1);
      szFieldName = (char *) hb_parc(2);
      sx_Replace( (unsigned char *) szFieldName, R_BLOBFILE, (PVOID) szFileName );
      hb_retl( 1 );
   }
   else
      hb_retl(0);
   return;

}

HB_FUNC( SX_GETBLOBLENGTH )
{

   getCurrentArea();
   if( ISCHAR(1) )
      hb_retnl( sx_GetBlobLength( (unsigned char *) hb_parc(1) ) );
   else
      hb_retnl( 0 );
}

HB_FUNC( SX_GETUDFPATH )
{
   char cBuffer[HB_PATH_MAX];

   sx_GetUDFPath( (unsigned char *) cBuffer, HB_PATH_MAX - 1 );
   hb_retc( cBuffer );

}

HB_FUNC( SX_IDTYPE )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
      hb_retni( ( sx_IsEncrypted( sx_RecNo() ) )? 2:1 );
   else
      hb_retni(0);
}

HB_FUNC( SX_ISFLOCKED )
{
   hb_retl( sx_Locked(0) );
}

HB_FUNC( SX_ISLOCKED )
{
   hb_retl( sx_Locked( hb_parnl(1) ) );
}

HB_FUNC( SX_ISREADONLY )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
      hb_retl( pArea->fReadonly );
   else
      hb_retl( FALSE );

}

HB_FUNC( SX_ISHARED )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
      hb_retl( pArea->fShared );
   else
      hb_retl(0);
}

HB_FUNC( SX_LOCKRETRY )
{
   sx_SetLockTimeout( hb_parni( 1 ) );
}

HB_FUNC( SX_QUERY )
{
   getCurrentArea();
   hb_retnl( sx_Query( ISCHAR(1) ? (unsigned char *) hb_parc(1) : NULL ) );
}

HB_FUNC( SX_SETCENTURY )
{
   BOOL f = hb_parl(1);

   sx_SetCentury( f );
   hb_retl( fCentury );
   fCentury = f;
}

HB_FUNC( SX_SETDATEFORMAT )
{
   usDateFormat = hb_parni(1);
   sx_SetDateFormat( usDateFormat );
}

HB_FUNC( SX_SETDELETED )
{

   USHORT usShowDeleted = sx_SysProp( SDE_SP_GETDELETED,NULL );
   sx_SetDeleted( hb_parl( 1 ) );
   hb_retl( usShowDeleted );

}

HB_FUNC( SX_SETEPOCH )
{
   hb_retni( sx_SetEpoch( hb_parni(1) ) );
}

HB_FUNC( SX_SETEXACT )
{

   USHORT usExact = sx_SysProp( SDE_SP_GETEXACT,NULL );
   sx_SetExact( hb_parl( 1 ) );
   hb_retl( usExact );

}

HB_FUNC( SX_SETFILETYPE )
{
   int fileType, oldType = sixFileType;
   if( ISNUM(1) )
   {
      fileType = hb_parni( 1 );
      if( fileType>0 && fileType<4 )
         sixFileType = fileType;
   }
   hb_retni( oldType );
}

HB_FUNC( SX_SETMEMOBLOCK )
{
   getCurrentArea();
   sx_SetMemoBlockSize( hb_parnl( 1 ) );

}

HB_FUNC( SX_SETPASS )
{
   getCurrentArea();
   if( ISCHAR(1) )
      sx_SetPassword( (unsigned char *) hb_parc(1) );

}

HB_FUNC( SX_SETSOFTSEEK )
{

   USHORT usSoftSeek = sx_SysProp( SDE_SP_GETSOFTSEEK,NULL );
   sx_SetSoftSeek( hb_parl( 1 ) );
   hb_retl( usSoftSeek );

}

HB_FUNC( SX_SETTRIGGER )
{
   int iAction = hb_parni(1);
   char * cTrigName = NULL;
   SIXAREAP pArea = getCurrentArea();

   if( ISCHAR(2) )
      cTrigName = (char *) hb_parc(2);

   if( pArea || iAction == 5 )
   {
      switch( iAction )
      {
        case 1:
           if( pArea->cTriggerName[0] )
              pArea->fTriggerActive = TRUE;
           break;
        case 2:
           pArea->fTriggerActive = FALSE;
           break;
        case 3:
           pArea->fTriggerActive = FALSE;
           pArea->cTriggerName[0] = '\0';
           break;
        case 4:
           if( cTrigName )
           {
              hb_strncpyUpper( pArea->cTriggerName, cTrigName, strlen(cTrigName) );
              pArea->fTriggerActive = TRUE;
           }
           break;
        case 5:
           if( cTrigName )
           {
              hb_strncpyUpper( cTriggerName, cTrigName, strlen(cTrigName) );
           }
           break;
      }
   }
}

HB_FUNC( SX_SETTURBO )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      hb_retl( pArea->fTurbo );
      pArea->fTurbo = hb_parl( 1 );
      sx_SetTurboRead( pArea->fTurbo );
   }
   else
      hb_retl( 0 );

}

HB_FUNC( SX_SETUDFPATH )
{
   char * pBuffer = (char *) hb_parc(1);

   sx_SetUDFPath( (unsigned char *) pBuffer );

}

HB_FUNC( SX_TABLETYPE )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
      hb_retni( ( sx_IsEncrypted(0) )? 2:1 );
   else
      hb_retni(0);
}

HB_FUNC( SX_TAGCOUNT )
{
   char szFileName[ HB_PATH_MAX - 1 ], *pBuffer;
   int  iOrd, iTags, iFirst;

   getCurrentArea();
   if( ISCHAR(1) )
      strcpy( szFileName, hb_parc(1) );
   else
   {
      iOrd = ( ISNUM(1) )? hb_parni(1) : sx_IndexOrd();
      if( !iOrd )
         iOrd = 1;
      pBuffer = (char*)sx_IndexName( iOrd );
      if( !pBuffer )
      {
         hb_retni(0);
         return;
      }
      else
         strcpy( szFileName, pBuffer );
   }

   iTags = hb_sxBag( szFileName, &iFirst );
   if( hb_pcount() > 1 )
      hb_storni( iFirst, 2 );
   hb_retni( iTags );
}

HB_FUNC( SX_VERSION )
{
    if( hb_pcount() > 0 && hb_parni(1) == 4 )
      hb_retc( RDD_VERSION );
    else
      hb_retc( (char*) sx_Version() );
}

HB_FUNC( SX_WILDMATCH )
{
   hb_retl( cmpmsk( (char *) hb_parc(1), (char *) hb_parc(2) ) );
}

HB_FUNC( SX_SETWRITEBLOBHDR )
{
   getCurrentArea();
   sx_SysProp( SDE_SP_SETWRITEBLOBHDR, (PVOID)hb_parl(1) );
}

HB_FUNC( SX_PACK )
{
   sx_Pack( );
}

HB_FUNC( SETMEMOBINARY )
{
   BOOL flag = fMemoBinary;
   if( hb_pcount() > 0 && ISLOG(1) )
      fMemoBinary = hb_parl(1);
   hb_retl( flag );
}
