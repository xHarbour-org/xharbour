/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 */

#define SUPERTABLE ( &sixSuper )
#define MAX_STR_LEN 255

#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbapicdp.h"
#include "rddsys.ch"

#include "hbstack.h"
#include "rddsix.h"

#undef HB_TRACE
#define HB_TRACE(x, y)

static LONG dwError;
typedef struct _MYERROR {
  DWORD lError ;
  DWORD lWorkArea;
  LPSTR sErrorMsg;
  LPSTR sExtraMsg;
} MYERROR,*PMYERROR;

MYERROR *pData = NULL;
void MyFunc(MYERROR *p);



static char s_szTableExt[ HB_MAX_FILE_EXT + 1 ] = ".dbf";
static int s_iHBSetHandle = 0;

int sixFileType = SDENSX;
int iSxCurrArea = 0;
char cTriggerName[20] = "";
USHORT usSixRddID = 0;
USHORT usDateFormat = 0;
BOOL fCentury = FALSE;
BOOL fMemoBinary = TRUE;

RDDFUNCS sixSuper;

static BOOL FindFile( char * pFilename, char * path )
{
   BOOL bIsFile = FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("FindFile(%s, %p)", (char*) pFilename, path));

   pFilepath = hb_fsFNameSplit( pFilename );
   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( path, pFilepath );
      bIsFile = hb_fsFile( path );
   }
   else
   {
      if( hb_setGetDefault() )
      {
         pFilepath->szPath = hb_setGetDefault();
         hb_fsFNameMerge( path, pFilepath );
         bIsFile = hb_fsFile( path );
      }

      if( !bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * nextPath = hb_setGetFirstSetPath();

         while( !bIsFile && nextPath )
         {
            pFilepath->szPath = nextPath->szPath;
            hb_fsFNameMerge( path, pFilepath );
            bIsFile = hb_fsFile( path );
            nextPath = nextPath->pNext;
         }
      }
   }

   hb_xfree( pFilepath );

   if( !bIsFile )
      *path = '\0';

   return bIsFile;
}

int hb_sxBag( char* szBagName, int *iFirstTag )
{
   int iTags = 0, iFirst = 0;
   int i, nTags = sx_SysProp(SDE_SP_GETINDEXCOUNT,NULL);
   BOOL lFound = FALSE;
   char *pBuffer, *ptr;

   ptr = strrchr( szBagName,'.' );
   if( ptr )
      *ptr = '\0';
   ptr = strrchr( szBagName,'\\' );
   if( ptr )
      szBagName = ptr + 1;
   hb_strUpper( szBagName, strlen(szBagName) );

   for( i=1; i<= nTags; i++ )
   {
      pBuffer = (char*) sx_IndexName( i );
      ptr = strrchr( pBuffer,'.' );
      if( ptr )
         *ptr = '\0';
      ptr = strrchr( pBuffer,'\\' );
      if( !ptr )
         ptr = pBuffer;
      hb_strUpper( ptr, strlen(ptr) );
      if( !strcmp( szBagName,ptr ) )
      {
         if( !lFound )
         {
            lFound = TRUE;
            iFirst = i;
         }
      }
      else
      {
         if( lFound )
         {
            iTags = i - iFirst;
            lFound = FALSE;
            break;
         }
      }
   }
   if( lFound )
      iTags = i - iFirst;
   if( iFirstTag )
      *iFirstTag = iFirst;

   return iTags;
}

static BOOL sxBuildLowScope( char * cScope, SHORT iType, BOOL lForce )
{
   if( lForce )
      cScope[0] = '\0';
   else
      strcpy( cScope, (char*)sx_GetScope(0) );

   if( !cScope[0] )
   {
      switch( iType )
      {
         case EVAL_CHARACTER:
            cScope[0] = '\1';
            cScope[1] = '\0';
            break;
         case EVAL_NUMERIC:
            {
               SHORT i, iLength = 21; //sx_KeyLength();

               cScope[0] = '-';
               for( i=1; i<iLength; i++ )
                  cScope[i] = '9';
               cScope[i] = '\0';
            }
            break;
         case EVAL_LOGICAL:
            cScope[0] = 'F';
            cScope[1] = '\0';
            break;
         case 4:
            hb_dateStrPut( cScope, 0, 1, 1 );
            cScope[9] = '\0';
            break;
      }
      return TRUE;
   }
   return FALSE;
}

static BOOL sxBuildHighScope( char * cScope, SHORT iType, BOOL lForce )
{
   SHORT i, iLength = sx_KeyLength();

   if( lForce )
      cScope[0] = '\0';
   else
      strcpy( cScope, (char*)sx_GetScope(1) );

   if( !cScope[0] )
   {
      switch( iType )
      {
         case EVAL_CHARACTER:
            for( i=0; i < iLength; i++ )
            {
               cScope[i] = (char)0xff;
            }
            cScope[i] = '\0';
            break;
         case EVAL_NUMERIC:
            for( i=0; i< 21 /*iLength*/; i++ )
               cScope[i] = '9';
            cScope[i] = '\0';
            break;
         case EVAL_LOGICAL:
            cScope[0] = 'T';
            cScope[1] = '\0';
            break;
         case 4:
            hb_dateStrPut( cScope, 9999, 12, 31 );
            cScope[9] = '\0';
            break;
      }
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM sxGetScope( SIXAREAP pArea, USHORT nScope, PHB_ITEM pItem, BOOL *pfEmpty )
{
   char pucScope[ SIX_MAX_KEY_LENGTH + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("sxGetScope(%p, %hu, %p)", pArea, nScope, pItem));

   if( pfEmpty )
      *pfEmpty = TRUE;

   if( pArea->iOrdCurrent )
   {
      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }

      if( pItem )
         hb_itemClear( pItem );

      strcpy( pucScope, (char*) sx_GetScope( nScope ) );

      if( pucScope[ 0 ] )
      {
         SHORT iType = sx_EvalTest( (unsigned char *) sx_IndexKey() );

         switch( iType )
         {
            case EVAL_CHARACTER:
               if( ( unsigned char ) pucScope[0] != ( nScope ? 0xff : 0x01 ) )
               {
                  if( pItem )
                     pItem = hb_itemPutC( pItem, pucScope );
                  if( pfEmpty )
                     *pfEmpty = FALSE;
               }
               break;

            case EVAL_NUMERIC:
            {
               int iDec, iLen;
               HB_LONG lValue;
               double dValue;

               iLen = pArea->iFileType == SDENTX ? sx_KeyLength() : 21;
               if( strncmp( pucScope, nScope ? "999999999999999999999" :
                                               "-99999999999999999999", iLen ) != 0 )
               {
                  if( pItem )
                  {
                     if( hb_valStrnToNum( pucScope, strlen( pucScope ), &lValue, &dValue, &iDec, &iLen ) )
                        pItem = hb_itemPutNDLen( pItem, dValue, iLen, iDec );
                     else
                        pItem = hb_itemPutNInt( pItem, lValue );
                  }
                  if( pfEmpty )
                     *pfEmpty = FALSE;
               }
               break;
            }
            case EVAL_LOGICAL:
               if( pItem )
                  pItem = hb_itemPutL( pItem, *pucScope == 'T' );
               if( pfEmpty )
               {
                  if( nScope ? *pucScope == 'F' : *pucScope == 'T' )
                     *pfEmpty = FALSE;
               }
               break;

            case EVAL_DATE:
               if( strcmp( pucScope, nScope ? "99991231" : "00000101" ) )
               {
                  if( pItem )
                     pItem = hb_itemPutDS( pItem, pucScope );
                  if( pfEmpty )
                     *pfEmpty = FALSE;
               }
               break;
         }
      }
   }
   return pItem;
}

static HB_ERRCODE sxSetScope( SIXAREAP pArea, USHORT nScope, PHB_ITEM pItem )
{
   char * pucScope = NULL;
   char cScopeBuf1[SIX_MAX_KEY_LENGTH + 1];
   char cScopeBuf2[SIX_MAX_KEY_LENGTH + 1];
   BOOL fEmpty, lResult, lNull;
   SHORT iType;
   LONG lRecno;

   HB_TRACE(HB_TR_DEBUG, ("sxSetScope(%p, %hu, %p)", pArea, nScope, pItem));

   if( pArea->iOrdCurrent )
   {
      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
      iType = sx_EvalTest( (unsigned char *) sx_IndexKey() );

      if( pItem )
      {
         switch( iType )
         {
            case EVAL_CHARACTER:
               if( HB_IS_STRING( pItem ) )
               {
                  if( hb_itemGetCLen( pItem ) > 0 )
                     pucScope = hb_itemGetCPtr( pItem );
               }
               else
                  return HB_FAILURE;
               break;

            case EVAL_NUMERIC:
            {
               if( HB_IS_NUMERIC( pItem ) )
               {
                  int iWidth, iDec;

                  hb_itemGetNLen( pItem, &iWidth, &iDec );
                  if( iDec )
                     iWidth += iDec + 1;
                  if( iWidth > SIX_MAX_KEY_LENGTH )
                     iWidth = SIX_MAX_KEY_LENGTH;
                  if( hb_itemStrBuf( cScopeBuf1, pItem, iWidth, iDec ) )
                     pucScope = cScopeBuf1;
                  else
                     return HB_FAILURE;
               }
               else
                  return HB_FAILURE;
               break;
            }

            case EVAL_LOGICAL:
               pucScope = hb_itemGetL( pItem ) ? "T" : "F";
               break;

            case EVAL_DATE:
               if( HB_IS_DATE( pItem ) )
                  pucScope = hb_itemGetDS( pItem, cScopeBuf1 );
               else
                  return HB_FAILURE;
               break;
         }
      }
      lRecno = sx_RecNo();

      if( !pucScope )
      {
         sxGetScope(  pArea, nScope ? 0 : 1, NULL, &fEmpty );
         if( fEmpty )
         {
            sx_SetScope( NULL, NULL );
            return HB_SUCCESS;
         }
      }

      if( nScope )
      {
         lNull = sxBuildLowScope( cScopeBuf2, iType, FALSE );
         if( !pucScope && lNull )
            lResult = sx_SetScope( NULL, NULL );
         else if( !pucScope && !lNull )
         {
            char cScopeHigh[SIX_MAX_KEY_LENGTH];
            sxBuildHighScope( cScopeHigh, iType, TRUE );
            lResult = sx_SetScope( (unsigned char *) cScopeBuf2, (unsigned char *) cScopeHigh );
         }
         else
            lResult = sx_SetScope( (unsigned char *) cScopeBuf2, (unsigned char *) pucScope );
      }
      else
      {
         lNull = sxBuildHighScope( cScopeBuf2, iType, FALSE );
         if( !pucScope && lNull )
            lResult = sx_SetScope( NULL, NULL );
         else if( !pucScope && !lNull )
         {
            char cScopeLow[SIX_MAX_KEY_LENGTH];
            sxBuildLowScope( cScopeLow, iType, TRUE );
            lResult = sx_SetScope( (unsigned char *) cScopeLow, (unsigned char *) cScopeBuf2 );
         }
         else
            lResult = sx_SetScope( (unsigned char *) pucScope, (unsigned char *) cScopeBuf2 );
      }
      sx_Go( lRecno );

      return (lResult)? HB_SUCCESS : HB_FAILURE;
   }
   else
      return HB_FAILURE;
}


static void sixHBSet( void )
{
   HB_TRACE(HB_TR_DEBUG, ("sixHBSet()"));

   sx_SetDeleted( hb_setGetDeleted() );
   /* TODO: try to covert date format in HB_SET_DATEFORMAT to SDE */
   sx_SetCentury( hb_setGetCentury() );
   sx_SetEpoch( hb_setGetEpoch() );
   sx_SetExact( hb_setGetExact() );
   sx_SysProp( SDE_SP_SETDISABLEAUTO,
               ( PVOID ) ( hb_setGetAutOpen() ? 0 : -1 ) );
}

static void sixHBSetCallback( HB_set_enum set, HB_set_listener_enum action )
{
   HB_TRACE(HB_TR_DEBUG, ("sixHBSetCallback(%d  %d)", setting, when));

   if( action == HB_SET_LISTENER_AFTER )  /* we don't do anything with BEFORE calls */
   {
      switch( set )
      {
         case HB_SET_DELETED:
            sx_SetDeleted( hb_setGetDeleted() );
            break;
         case HB_SET_DATEFORMAT:
            sx_SetCentury( hb_setGetCentury() );
            break;
         case HB_SET_EPOCH:
            sx_SetEpoch( hb_setGetEpoch() );
            break;
         case HB_SET_EXACT:
            sx_SetExact( hb_setGetExact() );
            break;
         case HB_SET_AUTOPEN:
            sx_SysProp( SDE_SP_SETDISABLEAUTO,
                        ( PVOID ) ( hb_setGetAutOpen() ? 0 : -1 ) );
            break;
         default:
            break;
      }
   }
}

static char * sixIndexExt( int iType )
{
   switch( iType )
   {
      case SDENTX:
         return ".ntx";
      case SDEFOX:
      #if defined(SDEVFOX)      
      case SDEVFOX:
      #endif
         return ".cdx";
      case SDENSX:
      #if defined(SDENSXDBT)
      case SDENSXDBT:
      #endif
      return ".nsx";

   }
   return "";
}

static char * sixMemoExt( int iType )
{
   switch( iType )
   {
      case SDENTX:
      #if defined(SDENSXDBT)
      case SDENSXDBT:
      #endif      
         return ".dbt";
      case SDEFOX:
      #if defined(SDEVFOX)      
      case SDEVFOX:
      #endif      
         return ".fpt";
      case SDENSX:
         return ".smt";
   }
   return "";
}

static SHORT sixEvalTest( char * szExpr )
{
   SHORT iErrorLevel = sx_ErrorLevel( 0 ), iType;

   iType = sx_EvalTest( ( PBYTE ) szExpr );
   sx_ErrorLevel( iErrorLevel );
   return iType;
}

static HB_ERRCODE hb_sixUpdateAreaFlags( SIXAREAP pArea, BOOL bFound )
{
   pArea->area.fBof = sx_Bof();
   pArea->area.fEof = sx_Eof();
   pArea->area.fFound = bFound ? sx_Found() : 0 ;

   return HB_SUCCESS;
}

static HB_ERRCODE sixRTError( SIXAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char* szFilename, USHORT uiFlags,USHORT uiOsCode )
{
   PHB_ITEM pError;
   HB_ERRCODE errCode;

   pError = hb_errNew();
   hb_errPutGenCode( pError, uiGenCode );
   hb_errPutSubCode( pError, uiSubCode );
   if( uiOsCode )
      hb_errPutOsCode( pError, uiOsCode );

   hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
   if( szFilename )
      hb_errPutFileName( pError, szFilename );
   if ( uiFlags )
      hb_errPutFlags( pError, uiFlags );
   errCode = SELF_ERROR( ( AREAP ) pArea, pError );
   hb_errRelease( pError );
   return errCode;
}

static char * sxReadArray( char * ptr, PHB_ITEM pItem )
{
   ULONG ulArLen, ul;

   ptr ++;
   ulArLen = HB_GET_LE_UINT16( ptr );
   ptr += 2;
   hb_arrayNew( pItem, ulArLen );
   for( ul = 1; ul <= ulArLen; ++ul )
   {
      switch( *ptr )
      {
         case '\6':           // Array
            ptr = sxReadArray( ptr, hb_arrayGetItemPtr( pItem, ul ) );
            break;

         case '\1':           // Char
         {
            ULONG ulLen;
            ++ptr;
            ulLen = HB_GET_LE_UINT16( ptr );
            ptr += 2;
            hb_itemPutCL( hb_arrayGetItemPtr( pItem, ul ), ptr, ulLen );
            ptr += ulLen;
            break;
         }
         case '\2':           // Int
            ++ptr;
            hb_itemPutNL( hb_arrayGetItemPtr( pItem, ul ), HB_GET_LE_INT32( ptr ) );
            ptr += 4;
            break;

         case '\3':           // Numeric
         {
            int iLen, iDec;
            ++ptr;
            iLen = (int) *ptr++;
            iDec = (int) *ptr++;
            hb_itemPutNDLen( hb_arrayGetItemPtr( pItem, ul ),
                             HB_GET_LE_DOUBLE( ptr ), iLen, iDec );
            ptr += 8;
            break;
         }
         case '\4':           // Date
            ++ptr;
            hb_itemPutDL( hb_arrayGetItemPtr( pItem, ul ), HB_GET_LE_INT32( ptr ) );
            ptr += 4;
            break;

         case '\5':           // Logical
            ++ptr;
            hb_itemPutL( hb_arrayGetItemPtr( pItem, ul ), *ptr++ != 0 );
            break;

         default:             // Nil
            ++ptr;
      }
   }
   return ptr;

}

static void sxBlob2Array( char * szFieldName, PHB_ITEM pItem )
{
   char * pBuffer;
   long lLen = sx_GetBlobLength( (unsigned char*) szFieldName );

   pBuffer = (char*) hb_xgrab( lLen+1 );
   sx_GetBlob( (unsigned char *) szFieldName, (PVOID) pBuffer );

   if( *pBuffer == '\6' )
      sxReadArray( pBuffer, pItem );

   hb_xfree( pBuffer );
}

static long int sxArrMemoSize( PHB_ITEM pArray )
{
   ULONG ulArrayLen, ul, ulLen;
   long int lMemoSize = 3;

   ulArrayLen = hb_arrayLen( pArray );
   if( ulArrayLen > USHRT_MAX )
      ulArrayLen = USHRT_MAX;

   for( ul = 1; ul <= ulArrayLen; ++ul )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr( pArray, ul );

      switch( hb_itemType( pItem ) )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
            ulLen = hb_itemGetCLen( pItem );
            if( ulLen > USHRT_MAX )
               ulLen = USHRT_MAX;
            lMemoSize += 3 + ulLen;
            break;

         case HB_IT_DATE:
            lMemoSize += 5;
            break;

         case HB_IT_LOGICAL:
            lMemoSize += 2;
            break;

         case HB_IT_ARRAY:
            lMemoSize += sxArrMemoSize( pItem );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         {
            HB_LONG lValue = hb_itemGetNInt( pItem );

            if( HB_LIM_INT32( lValue ) )
            {
               lMemoSize += 5;
               break;
            }
         }
         case HB_IT_DOUBLE:
            lMemoSize += 11;
            break;

         default:
            lMemoSize += 1;
            break;
      }
   }

   return lMemoSize;
}

static char * sxWriteArray( char * ptr, PHB_ITEM pArray )
{
   ULONG ulArrayLen, ul, ulLen;

   *ptr++ = '\6';
   ulArrayLen = hb_arrayLen( pArray );
   if( ulArrayLen > USHRT_MAX )
      ulArrayLen = USHRT_MAX;

   HB_PUT_LE_UINT16( ptr, ulArrayLen );
   ptr += 2;

   for( ul = 1; ul <= ulArrayLen; ++ul )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr( pArray, ul );

      switch( hb_itemType( pItem ) )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
            *ptr++ = '\1';
            ulLen = hb_itemGetCLen( pItem );
            if( ulLen > USHRT_MAX )
               ulLen = USHRT_MAX;
            HB_PUT_LE_UINT16( ptr, ulLen );
            ptr += 2;
            memcpy( ptr, hb_itemGetCPtr( pItem ), ulLen );
            ptr += ulLen;
            break;

         case HB_IT_DATE:
         {
            long lDate = hb_itemGetDL( pItem );
            *ptr++ = '\4';
            HB_PUT_LE_UINT32( ptr, lDate );
            ptr += 4;
            break;
         }

         case HB_IT_LOGICAL:
            *ptr++ = '\5';
            *ptr++ = hb_itemGetL( pItem );
            break;

         case HB_IT_ARRAY:
            ptr = sxWriteArray( ptr, pItem );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         {
            HB_LONG lValue = hb_itemGetNInt( pItem );

            if( HB_LIM_INT32( lValue ) )
            {
               *ptr++ = '\2';
               HB_PUT_LE_UINT32( ptr, lValue );
               ptr += 4;
               break;
            }
         }
         case HB_IT_DOUBLE:
         {
            int iWidth, iDec;
            double dVal;

            dVal = hb_itemGetND( pItem );
            hb_itemGetNLen( pItem, &iWidth, &iDec );
            *ptr++ = '\3';
            *ptr++ = (char) iWidth;
            *ptr++ = (char) iDec;
            HB_PUT_LE_DOUBLE( ptr, dVal );
            ptr += 8;
            break;
         }

         default:
            *ptr++ = '\0';
      }
   }

   return ptr;
}

static int sxArray2Blob( SIXAREAP pArea, char * szFieldName, PHB_ITEM pItem )
{
   int iResult = TRUE;
   long int lMemoSize = sxArrMemoSize( pItem );
   USHORT usOffset = sx_FieldOffset( (unsigned char *) szFieldName ) - 1;
   char * pBuffer = (char*) hb_xgrab( lMemoSize + 10 );

   sxWriteArray( pBuffer, pItem );
   sx_SysProp( SDE_SP_SETWRITEBLOBHDR, (PVOID)FALSE );
   if( sx_PutBlob( (unsigned char *) szFieldName, pBuffer, lMemoSize ) != lMemoSize )
      iResult = FALSE;
   sx_SysProp( SDE_SP_SETWRITEBLOBHDR, (PVOID)TRUE );

   sx_GetRecord( pArea->pRecord );
   *(pArea->pRecord+usOffset) = '\6';
   sx_PutRecord( pArea->pRecord );

   hb_xfree( pBuffer );
   return iResult;
}

BOOL sxTriggerCall( SIXAREAP pArea, SHORT nEvent, SHORT nArea, SHORT nField, PHB_ITEM xValue )
{
   PHB_SYMB pSym;
   BOOL lRes = TRUE;

   if( !pArea->fTriggerActive || !pArea->cTriggerName[0] )
   {
      return lRes;
   }

   if( ( pSym = hb_dynsymFindSymbol( pArea->cTriggerName ) ) != NULL )
   {
      hb_vmPushSymbol( pSym );
      hb_vmPushNil();
      hb_vmPushLong( ( LONG ) nEvent );
      hb_vmPushLong( ( LONG ) nArea );
      hb_vmPushLong( ( LONG ) nField );

      if( xValue )
      {
         hb_vmPushItemRef( xValue );
      }
      else
      {
         hb_vmPushLong( 0 );
      }

      hb_vmDo( 4 );
      lRes = hb_parl( -1 );

      /* restore the SIX work are if trigger function changed it */
      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
   }

   return lRes;
}

/*
 * -- SIX METHODS --
 */

HB_ERRCODE sixBof( SIXAREAP pArea, BOOL * pBof )
{
   HB_TRACE(HB_TR_DEBUG, ("sixBof(%p, %p)", pArea, pBof));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pBof = pArea->area.fBof;

   return HB_SUCCESS;
}

HB_ERRCODE sixEof( SIXAREAP pArea, BOOL * pEof )
{
   HB_TRACE(HB_TR_DEBUG, ("sixEof(%p, %p)", pArea, pEof));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pEof = pArea->area.fEof;

   return HB_SUCCESS;
}

HB_ERRCODE sixFound( SIXAREAP pArea, BOOL * pFound )
{
   HB_TRACE(HB_TR_DEBUG, ("sixFound(%p, %p)", pArea, pFound));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   *pFound = pArea->area.fFound;

   return HB_SUCCESS;
}

static HB_ERRCODE sixGoBottom( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixGoBottom(%p)", pArea));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pArea->lpdbPendingRel = NULL;
   pArea->area.fTop = FALSE;
   pArea->area.fBottom = TRUE;
   sx_GoBottom();

   hb_sixUpdateAreaFlags( pArea, 0 );
   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return SELF_SKIPFILTER( (AREAP) pArea, -1 );
}

static HB_ERRCODE sixGoTo( SIXAREAP pArea, ULONG ulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixGoTo(%p, %lu)", pArea, ulRecNo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pArea->lpdbPendingRel = NULL;
   pArea->area.fFound = FALSE;
   sx_Go( ulRecNo );

   hb_sixUpdateAreaFlags( pArea, 0 );
   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

static HB_ERRCODE sixGoToId( SIXAREAP pArea, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("sixGoToId(%p, %p)", pArea, pItem));

   if( HB_IS_NUMERIC( pItem ) )
   {
      return SELF_GOTO( ( AREAP ) pArea, hb_itemGetNL( pItem ) );
   }
   else
   {
      sixRTError( pArea, EG_DATATYPE, EDBF_DATATYPE, NULL, 0 ,0);
      return HB_FAILURE;
   }
}

static HB_ERRCODE sixGoTop( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixGoTop(%p)", pArea));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pArea->area.fTop = TRUE;
   pArea->area.fBottom = FALSE;
   pArea->lpdbPendingRel = NULL;

   sx_GoTop();

   hb_sixUpdateAreaFlags( pArea, 0 );
   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return SELF_SKIPFILTER( (AREAP)pArea, 1 );
}

static HB_ERRCODE sixSeek( SIXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   HB_TRACE(HB_TR_DEBUG, ("sixSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pArea->lpdbPendingRel = NULL;
   pArea->area.fTop = pArea->area.fBottom = FALSE;

   /* TODO: fix bSoftSeek and bFindLast */
   if( bSoftSeek != hb_setGetSoftSeek() )
   {
      sx_SetSoftSeek( bSoftSeek );
   }

   if( bFindLast )
   {
   }
   else
   {
      if(hb_itemType( pKey ) == HB_IT_STRING )
         pArea->area.fFound = sx_Seek( (unsigned char *) hb_itemGetCPtr( pKey ) );
      else if(hb_itemType( pKey ) & HB_IT_NUMERIC )
      {
         int iWidth, iDec;
         PHB_ITEM pDec;
         char * szResult;

         hb_itemGetNLen( pKey, &iWidth, &iDec );
         pDec = hb_itemPutNI( NULL, iDec );
         szResult = hb_itemStr( pKey, NULL, pDec );
         hb_itemRelease( pDec );
         pArea->area.fFound = sx_Seek( (unsigned char *) szResult );
         hb_xfree( szResult );
      }
      else if(hb_itemType( pKey ) & HB_IT_DATE )
      {
         int iYear, iMonth, iDay;
         char szDate[9];

         hb_dateDecode( hb_itemGetDL( pKey ), &iYear, &iMonth, &iDay );
         hb_dateStrPut( szDate, iYear, iMonth, iDay );
         szDate[9] = '\0';
         pArea->area.fFound = sx_Seek( (unsigned char *) szDate );
      }
      else if(hb_itemType( pKey ) & HB_IT_LOGICAL )
      {
         pArea->area.fFound = sx_Seek( (unsigned char *) ( hb_itemGetL( pKey )?  "T" : "F" ) );
      }
   }

   hb_sixUpdateAreaFlags( pArea, 1 );
   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   if( bSoftSeek != hb_setGetSoftSeek() )
   {
      sx_SetSoftSeek( hb_setGetSoftSeek() );
   }

   /* TODO: fix this - it can change fFound flag */
   if( pArea->area.dbfi.itmCobExpr )
   {
      return SELF_SKIPFILTER( (AREAP)pArea, 1 );
   }

   return HB_SUCCESS;
}

static HB_ERRCODE sixSkip( SIXAREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("sixSkip(%p, %ld)", pArea, lToSkip));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   if( lToSkip == 0 )
   {
      sx_Skip( lToSkip );
      return HB_SUCCESS;
   }
   else
   {
      pArea->area.fTop = pArea->area.fBottom = FALSE;

      /* TODO: fix this - it will not work as expected when filter is not
         recognized by SDE */
      sx_Skip( lToSkip );

      hb_sixUpdateAreaFlags( pArea, 0 );
      /* Force relational movement in child WorkAreas */
      if( pArea->area.lpdbRelations )
         SELF_SYNCCHILDREN( ( AREAP ) pArea );

      return (HB_ERRCODE) SELF_SKIPFILTER( (AREAP)pArea, (lToSkip<0)? -1:1 );
   }
}

#define  sixSkipFilter            NULL

static HB_ERRCODE sixSkipRaw( SIXAREAP pArea, LONG toSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("sixSkipRaw(%p)", pArea));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   sx_Skip( toSkip );

   hb_sixUpdateAreaFlags( pArea,0  );
   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}

static HB_ERRCODE sixAddField( SIXAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixAddField(%p, %p)", pArea, pFieldInfo));

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;

   return SUPER_ADDFIELD( ( AREAP ) pArea, pFieldInfo );
}

static HB_ERRCODE sixAppend( SIXAREAP pArea, BOOL fUnLockAll )
{
   BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("sixAppend(%p, %d)", pArea, (int) fUnLockAll));

   if( !sxTriggerCall( pArea, EVENT_APPEND, pArea->area.uiArea, 0, NULL ) )
      return HB_FAILURE;

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pArea->lpdbPendingRel = NULL;
   if( fUnLockAll )
      sx_Unlock( 0 );

   fResult = sx_AppendBlankEx();

   /*
    * Apollo SDE does not report appended records in rlocklist, so we lock it
    * ourself
    */
   if( fResult )
      sx_Rlock( sx_RecNo() );

   hb_sixUpdateAreaFlags( pArea,0 );

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE sixCreateFields( SIXAREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiItems, uiCount, uiLen, uiDec;
   DBFIELDINFO pFieldInfo;
   PHB_ITEM pFieldDesc;
   char *szFieldType;
   int iData;

   HB_TRACE(HB_TR_DEBUG, ("sixCreateFields(%p, %p)", pArea, pStruct));

   uiItems = ( USHORT ) hb_arrayLen( pStruct );
   SELF_SETFIELDEXTENT( (AREAP)pArea, uiItems );

   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      pFieldInfo.uiTypeExtended = 0;
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
      pFieldInfo.atomName = hb_arrayGetCPtr( pFieldDesc, 1 );
      iData = hb_arrayGetNI( pFieldDesc, 3 );
      if( iData < 0 )
         iData = 0;
      uiLen = pFieldInfo.uiLen = ( USHORT ) iData;
      iData = hb_arrayGetNI( pFieldDesc, 4 );
      if( iData < 0 )
         iData = 0;
      uiDec = ( USHORT ) iData;
      pFieldInfo.uiDec = 0;
      szFieldType = hb_arrayGetCPtr( pFieldDesc, 2 );
      iData = toupper( szFieldType[ 0 ] );
      switch( iData )
      {
         case 'C':
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = uiLen + uiDec * 256;
            break;

         case 'L':
            pFieldInfo.uiType = HB_FT_LOGICAL;
            pFieldInfo.uiLen = 1;
            break;

         case 'M':
            pFieldInfo.uiType = HB_FT_MEMO;
            pFieldInfo.uiLen = 10;
            break;

         case 'D':
            pFieldInfo.uiType = HB_FT_DATE;
            if( uiLen == 3 )
            {
               pFieldInfo.uiTypeExtended = SIX_SHORTDATE;
               pFieldInfo.uiLen = 3;
            }
            else
               pFieldInfo.uiLen = 8;
            break;

         case 'N':
            pFieldInfo.uiType = HB_FT_LONG;
            if( uiLen > 20 )
               return HB_FAILURE;
            else
               pFieldInfo.uiDec = uiDec;
            break;

         case 'V':
            pFieldInfo.uiType = HB_FT_MEMO;
            pFieldInfo.uiTypeExtended = SIX_VARCHAR;
            break;

         case 'F':
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiTypeExtended = SIX_FLOAT;
            pFieldInfo.uiLen = 8;
            break;

         case 'I':
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiTypeExtended = SIX_INTEGER;
            if( uiLen > 4 )
               return HB_FAILURE;
            else
               pFieldInfo.uiDec = 0;
            break;

         default:
            return HB_FAILURE;
      }
      /* Add field */
      if( SELF_ADDFIELD( (AREAP)pArea, &pFieldInfo ) == HB_FAILURE )
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixDeleteRec( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixDeleteRec(%p)", pArea));

   if( !sxTriggerCall( pArea, EVENT_DELETE, pArea->area.uiArea, 0, NULL ) )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   sx_Delete();

   return HB_SUCCESS;
}

static HB_ERRCODE sixDeleted( SIXAREAP pArea, BOOL * pDeleted )
{
   HB_TRACE(HB_TR_DEBUG, ("sixDeleted(%p, %p)", pArea, pDeleted));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   *pDeleted = sx_Deleted();

   return HB_SUCCESS;
}

static HB_ERRCODE sixFieldCount( SIXAREAP pArea, USHORT * uiFields )
{
   HB_TRACE(HB_TR_DEBUG, ("sixFieldCount(%p, %p)", pArea, uiFields));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   *uiFields = sx_FieldCount();
   return HB_SUCCESS;
}

#define  sixFieldDisplay          NULL

HB_ERRCODE sixFieldInfo( SIXAREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("sixFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem));

   if( uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( uiType != DBS_TYPE )
      return SUPER_FIELDINFO( ( AREAP ) pArea, uiIndex, uiType, pItem );

   pField = pArea->area.lpFields + uiIndex - 1;
   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( pField->uiTypeExtended == 0 )
            hb_itemPutC( pItem, "C" );
         break;

      case HB_FT_LOGICAL:
         hb_itemPutC( pItem, "L" );
         break;

      case HB_FT_MEMO:
         if( pField->uiTypeExtended == 0 )
            hb_itemPutC( pItem, "M" );
         else if( pField->uiTypeExtended == SIX_VARCHAR )
            hb_itemPutC( pItem, "V" );
         break;

      case HB_FT_DATE:
         hb_itemPutC( pItem, "D" );
         break;

      case HB_FT_LONG:
           
         if( pField->uiTypeExtended == 0 )
            hb_itemPutC( pItem, "N" );
         if (   pField->uiTypeExtended == SIX_FLOAT )
            hb_itemPutC( pItem, "F" );
         if (   pField->uiTypeExtended == SIX_INTEGER )
            hb_itemPutC( pItem, "I" );            
         break;
      default:
         hb_itemPutC( pItem, "U" );
         break;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixFieldName( SIXAREAP pArea, USHORT uiIndex, void * szName )
{
   HB_TRACE(HB_TR_DEBUG, ("sixFieldName(%p, %hu, %p)", pArea, uiIndex, szName));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   if( uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   strcpy( (char*)szName, (char*)sx_FieldName( uiIndex ) ); // socs // was -> strcpy( szName, (char*)sx_FieldName( uiIndex ) );
   return HB_SUCCESS;
}

static HB_ERRCODE sixFlush( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixFlush(%p)", pArea ));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   sx_Commit();
   return HB_SUCCESS;
}

#define  sixGetRec     NULL

static HB_ERRCODE sixGetValue( SIXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   BYTE *  pBuffer;
   char    szName[ SIX_MAX_FIELD_NAME + 1 ];
   DOUBLE  dVal;

   HB_TRACE(HB_TR_DEBUG, ("sixGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( !uiIndex || uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   /* in SIX trigger is called at the end of this method but I do not like it */
   if( !sxTriggerCall( pArea, EVENT_GET, pArea->area.uiArea, (SHORT) uiIndex, pItem ) )
      return HB_FAILURE;

   pField = pArea->area.lpFields + uiIndex - 1;

   strcpy( szName, (char*)sx_FieldName( uiIndex ) );

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         pBuffer = (BYTE*) sx_GetString( (unsigned char *) szName );
         hb_itemPutCL( pItem, ( char * ) pBuffer, pField->uiLen );
         break;

      case HB_FT_LONG:
         dVal = sx_GetDouble( (unsigned char *) szName );
         if( pField->uiDec )
         {
            hb_itemPutNDLen( pItem, dVal,
                           ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                           ( int ) pField->uiDec );
         }
         else
         {
            hb_itemPutNLen( pItem, dVal, ( int ) pField->uiLen, (int) 0 );
         }
         break;

      case HB_FT_DATE:
         {
            pBuffer = (BYTE*) sx_GetString( (unsigned char *) szName );
            hb_itemPutDS( pItem, (char *) pBuffer );
            break;
         }

      case HB_FT_LOGICAL:
         {
            SHORT pbValue = sx_GetLogical( (unsigned char *) szName );
            hb_itemPutL( pItem, pbValue);
            break;
         }

      case HB_FT_MEMO:
         {
           long lLen;

           if( pArea->iFileType == 3 )
           {
              USHORT usOffset = sx_FieldOffset( (unsigned char *) szName ) - 1;

              sx_GetRecord( pArea->pRecord );
              if( *(pArea->pRecord+usOffset) == '\6' &&
                                   *(pArea->pRecord+usOffset+1) == '\0' )
              {
                 sxBlob2Array( szName, pItem );
                 break;
              }
           }
           if( !pArea->fMemoBinary || pField->uiTypeExtended == SIX_VARCHAR )
           {
              int pulLen;
              pBuffer = (BYTE *) sx_GetMemo( (unsigned char *) szName, 0 );
              pulLen = lstrlen( (char *) pBuffer );
              if( pulLen > 0 )
              {
                 hb_itemPutCL( pItem, (char *) pBuffer, pulLen );
                 sx_MemDealloc( pBuffer );
              }
              else
                 hb_itemPutC( pItem, "" );
           }
           else
           {
              lLen = sx_GetBlobLength( (unsigned char *) szName );
              pBuffer = (BYTE*) hb_xgrab( lLen+1 );
              sx_GetBlob( (unsigned char *) szName, (PVOID) pBuffer );
              hb_itemPutCL( pItem, ( char * ) pBuffer, lLen );
              hb_xfree( pBuffer );
           }
           hb_itemSetCMemo( pItem );
           break;
         }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixGetVarLen( SIXAREAP pArea, USHORT uiIndex, ULONG * ulLen )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("sixGetVarLen(%p, %hu, %p)", pArea, uiIndex, ulLen));

   if( uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   /* TODO: fix for memos */
   pField = pArea->area.lpFields + uiIndex - 1;
   * ulLen = pField->uiLen;
   return HB_SUCCESS;
}

#define  sixGoCold                NULL
#define  sixGoHot                 NULL
#define  sixPutRec                NULL

static HB_ERRCODE sixPutValue( SIXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD pField;
   BOOL    bError;
   char szName[ SIX_MAX_FIELD_NAME + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("sixPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( !sxTriggerCall( pArea, EVENT_PUT, pArea->area.uiArea, ( SHORT ) uiIndex, pItem ) )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( !uiIndex || uiIndex > pArea->area.uiFieldCount || pArea->area.fEof )
      return HB_FAILURE;

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pField = pArea->area.lpFields + uiIndex - 1;
   bError = TRUE;
   strcpy( szName, (char*) sx_FieldName( uiIndex ) );

   switch( pField->uiType )
   {
      case HB_FT_STRING:
         if( HB_IS_STRING( pItem ) )
         {
            sx_Replace( (unsigned char *) szName, R_CHAR, (PVOID) hb_itemGetCPtr( pItem ) );
            bError = FALSE;
         }
         break;

      case HB_FT_LONG:
         if( HB_IS_NUMERIC( pItem ) )
         {
            double dVal = hb_itemGetND( pItem );
            sx_Replace( (unsigned char *) szName, R_DOUBLE, (PVOID) &dVal );
            bError = FALSE;
         }
         break;

      case HB_FT_DATE:
         if( HB_IS_DATE( pItem ) )
         {
            long lJulian = hb_itemGetDL( pItem );
            sx_Replace( (unsigned char *) szName, R_JULIAN, (PVOID) &lJulian );
            bError = FALSE;
         }
         break;

      case HB_FT_LOGICAL:
         if( HB_IS_LOGICAL( pItem ) )
         {
            BOOL bValue = hb_itemGetL( pItem );
            sx_Replace( (unsigned char *) szName, R_LOGICAL, (PVOID) &bValue );
            bError = FALSE;
         }
         break;

      case HB_FT_MEMO:
         if( HB_IS_STRING( pItem ) )
         {
            if( !pArea->fMemoBinary || pField->uiTypeExtended == SIX_VARCHAR )
            {
               sx_Replace( (unsigned char *) szName, R_MEMO, (PVOID) hb_itemGetCPtr( pItem ) );
            }
            else
            {
               sx_SysProp( SDE_SP_SETWRITEBLOBHDR, (PVOID)FALSE );
               sx_PutBlob( (unsigned char *) szName, (PVOID) hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ) );
               sx_SysProp( SDE_SP_SETWRITEBLOBHDR, (PVOID)TRUE );
            }
            bError = FALSE;
         }
         else if( HB_IS_ARRAY( pItem ) && (pArea->iFileType == 3  || pArea->iFileType == 4) )
         {
            if( sxArray2Blob( pArea, szName, pItem ) )
               bError = FALSE;
         }
         break;
   }

   if( bError )
   {
      sixRTError( pArea, EG_DATATYPE, EDBF_DATATYPE, NULL, 0 ,0);
      return HB_FAILURE;
   }

   return HB_SUCCESS;
}

static HB_ERRCODE sixRecall( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixRecall(%p)", pArea));

   if( !sxTriggerCall( pArea, EVENT_RECALL, pArea->area.uiArea, 0, NULL ) )
      return HB_FAILURE;

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   sx_Recall();

   return HB_SUCCESS;
}

static HB_ERRCODE sixRecCount( SIXAREAP pArea, ULONG * pRecCount )
{
   HB_TRACE(HB_TR_DEBUG, ("sixRecCount(%p, %p)", pArea, pRecCount));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   *pRecCount = sx_RecCount();

   return HB_SUCCESS;
}

#define  sixRecInfo               NULL

static HB_ERRCODE sixRecNo( SIXAREAP pArea, ULONG * ulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixRecNo(%p, %lu)", pArea, ulRecNo));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   *ulRecNo = sx_RecNo();

   return HB_SUCCESS;
}

static HB_ERRCODE sixRecId( SIXAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("sixRecId(%p, %p)", pArea, pRecNo));

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   hb_itemPutNL( pRecNo, ulRecNo );
   return errCode;
}

static HB_ERRCODE sixSetFieldExtent( SIXAREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("sixSetFieldExtent(%p, %hu)", pArea, uiFieldExtent));

   if( SUPER_SETFIELDEXTENT( ( AREAP ) pArea, uiFieldExtent ) == HB_FAILURE )
      return HB_FAILURE;

   /* Alloc field offsets array */
   pArea->pFieldOffset = ( USHORT * ) hb_xgrab( uiFieldExtent * sizeof( USHORT * ) );
   memset( pArea->pFieldOffset, 0, uiFieldExtent * sizeof( USHORT * ) );
   return HB_SUCCESS;
}

#define  sixAlias                 NULL

static HB_ERRCODE sixClose( SIXAREAP pArea )
{
   BOOL fOpened;

   HB_TRACE(HB_TR_DEBUG, ("sixClose(%p)", pArea));

   fOpened = pArea->uiSxArea != 0;
   if( fOpened )
   {
      if( !sxTriggerCall( pArea, EVENT_PRECLOSE, pArea->area.uiArea, 0, NULL ) )
         return HB_FAILURE;
   }

   /* Reset pending relation if any */
   pArea->lpdbPendingRel = NULL;

   SUPER_CLOSE( ( AREAP ) pArea );

   if( pArea->uiSxArea )
   {
      if( iSxCurrArea != pArea->uiSxArea && pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
      sx_Close();
      iSxCurrArea = pArea->uiSxArea = 0;
   }

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

   /* Free all filenames */
   if( pArea->szDataFileName )
   {
      hb_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }

   if( fOpened )
   {
      sxTriggerCall( pArea, EVENT_POSTCLOSE, pArea->area.uiArea, 0, NULL );
   }

   return HB_SUCCESS;
}

static HB_ERRCODE sixCreate( SIXAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   char path[ HB_PATH_MAX ];
   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   PHB_FNAME pFilepath;
   USHORT uiCount;
   LPFIELD pField;
   char cType[2];

   HB_TRACE(HB_TR_DEBUG, ("sixCreate(%p, %p)", pArea, pCreateInfo));

   pFilepath = hb_fsFNameSplit( pCreateInfo->abName );

   if( ! pFilepath->szPath && hb_setGetDefault() )
   {
      pFilepath->szPath = hb_setGetDefault();
   }

   if( ! pFilepath->szExtension )
   {
      pFilepath->szExtension = s_szTableExt;
   }

   hb_fsFNameMerge( path, pFilepath );

   /* Create default alias if necessary */
   if( !pCreateInfo->atomAlias || hb_strEmpty( pCreateInfo->atomAlias,
                          strlen( pCreateInfo->atomAlias ) ) )
   {
      /*
       * SIX does not support empty aliases, we have to use
       * temporary unique alias name
       */
      hb_rddGetTempAlias( szAlias );
      pCreateInfo->atomAlias = szAlias;
   }
   hb_xfree( pFilepath );

   pArea->szDataFileName = hb_strdup( path );

   pArea->uiSxArea = sx_CreateNew( (PBYTE)path, (PBYTE)pCreateInfo->atomAlias,
               ( SHORT ) sixFileType, ( SHORT ) pArea->area.uiFieldCount );

   if( pArea->uiSxArea == 0 )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      sixRTError( pArea, EG_CREATE, EDBF_CREATE_DBF, path, 0 ,0);
      return HB_FAILURE;
   }

   cType[1] = '\0';
   pField = pArea->area.lpFields;
   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      switch ( pField->uiType )
      {
        case HB_FT_DATE:
           cType[0] = 'D';
           break;
        case HB_FT_LOGICAL:
           cType[0] = 'L';
           break;
        case HB_FT_MEMO:
           if( pField->uiTypeExtended == SIX_VARCHAR )
              cType[0] = 'V';
           else
              cType[0] = 'M';
           break;
        case HB_FT_STRING:
           cType[0] = 'C';
           break;
        case HB_FT_INTEGER:
        case HB_FT_LONG:
        case HB_FT_DOUBLE:
           if( pField->uiTypeExtended == SIX_FLOAT )
              cType[0] = 'F';
           else if( pField->uiTypeExtended == SIX_INTEGER )
              cType[0] = 'I';
           else
              cType[0] = 'N';
           break;
      }

      sx_CreateField( (unsigned char *) hb_dynsymName( ( PHB_DYNS ) pField->sym ),
                      (unsigned char *) cType, pField->uiLen, pField->uiDec );
      pField++;
   }

   if( !sx_CreateExec() )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      sixRTError( pArea, EG_CREATE, EDBF_CREATE_DBF, path, 0 ,0);
      return HB_FAILURE;
   }
   if( SUPER_CREATE( ( AREAP ) pArea, pCreateInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }
   pArea->fMemoBinary = fMemoBinary;

   iSxCurrArea = pArea->uiSxArea;
   return HB_SUCCESS;
}

static HB_ERRCODE sixInfo( SIXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("sixInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( iSxCurrArea != pArea->uiSxArea && pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   switch( uiIndex )
   {
      case DBI_ISDBF:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_CANPUTREC:
      /* as long as real {PUT|GET}REC will not have been implemented it
         should return FALSE */
         hb_itemPutL( pItem, FALSE );
         break;

      case DBI_GETHEADERSIZE:
         /*
          * no API call to retrieve the real header size so we have
          * to calculate it ourself like for standard DBF files
          */
         hb_itemPutNL( pItem, 33 + pArea->area.uiFieldCount * 32 );
         break;

      case DBI_LASTUPDATE:
         {
            char * pBuffer;
            int iYear, iMonth, iDay;

            sx_SetDateFormat( 0 );
            pBuffer = (char*)sx_BaseDate();
            iYear =  2000 + ( ( pBuffer[6] - '0' ) * 10 ) +
                              ( pBuffer[7] - '0' );
            iMonth = ( ( pBuffer[0] - '0' ) * 10 ) + ( pBuffer[1] - '0' );
            iDay   = ( ( pBuffer[3] - '0' ) * 10 ) + ( pBuffer[4] - '0' );
            hb_itemPutDL( pItem, hb_dateEncode( iYear, iMonth, iDay ) );
            sx_SetDateFormat( usDateFormat );
         }
         break;

      case DBI_GETRECSIZE:
         hb_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETLOCKARRAY:
      {
         ULONG ulLockCount = ( ULONG ) sx_LockCount(), ul;
         hb_arrayNew( pItem, ulLockCount );
         if( ulLockCount )
         {
            PULONG pulList = ( PULONG ) hb_xgrab( ulLockCount * sizeof( ULONG ) );
            sx_DBRlockList( pulList );
            for( ul = 0; ul < ulLockCount; ++ul )
            {
               hb_itemPutNL( hb_arrayGetItemPtr( pItem, ul + 1 ), pulList[ ul ] );
            }
            hb_xfree( pulList );
         }
         break;
      }
      case DBI_LOCKCOUNT:
         hb_itemPutNL( pItem, ( LONG ) sx_LockCount() );
         break;

      case DBI_TABLEEXT:
         hb_itemPutC( pItem, s_szTableExt );
         break;

      case DBI_FULLPATH:
         hb_itemPutC( pItem, ( char * ) sx_BaseName() );
         break;

      case DBI_ISFLOCK:
         hb_itemPutL( pItem, pArea->fFLocked );
         break;

      case DBI_SHARED:
         hb_itemPutL( pItem, pArea->fShared );
         break;

      case DBI_MEMOEXT:
         hb_itemPutC( pItem, sixMemoExt( pArea->iFileType ) );
         break;

      case DBI_RM_SUPPORTED:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_RM_CREATE:
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_RM_REMOVE:
      case DBI_RM_CLEAR:
      case DBI_RM_FILL:
         hb_itemPutL( pItem, FALSE );
         break;

      case DBI_RM_ADD:
         sx_SetQueryBit( hb_itemGetNL( pItem ), TRUE );
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_RM_DROP:
         sx_SetQueryBit( hb_itemGetNL( pItem ), FALSE );
         hb_itemPutL( pItem, TRUE );
         break;

      case DBI_RM_TEST:
         hb_itemPutL( pItem, sx_GetQueryBit( hb_itemGetNL( pItem ) ) );
         break;

      case DBI_RM_COUNT:
         hb_itemPutNL( pItem, sx_QueryRecCount() );
         break;

      case DBI_RM_HANDLE:
         hb_itemPutNL( pItem, FS_ERROR );
         break;
      case DBI_BOF            :   /* BOF flag - alternate to bof() */
         hb_itemPutL( pItem, pArea->area.fEof );
         break;
      case DBI_EOF            :   /* EOF flag - alternate to eof() */
         hb_itemPutL( pItem, pArea->area.fBof );
         break;
      case DBI_FOUND          :   /* FOUND flag - alternate to found */
         hb_itemPutL( pItem, pArea->area.fFound );
         break;
      case DBI_FCOUNT         :   /* Number of fields */
         hb_itemPutNL( pItem, pArea->area.uiFieldCount );

/* TODO ... */
      //case DBI_DBFILTER:
      //   break;
      case DBI_ALIAS          :   /* Alias name of workarea */      
      case DBI_CHILDCOUNT     :   /* Number of opened relations */
      case DBI_FILEHANDLE     :   /* Handle of opened file */      
      case DBI_DBFILTER       :   /* Filter expression */
      case DBI_VALIDBUFFER    :   /* Is the current buffer valid */
      case DBI_GETSCOPE       :   /* Locate codeblock */
      case DBI_LOCKOFFSET     :   /* New locking offset */
      case DBI_MEMOHANDLE     :   /* Dos handle for memo file */
      case DBI_MEMOBLOCKSIZE  :   /* Blocksize in memo files */
      case DBI_DB_VERSION     :   /* HOST driver Version */
      case DBI_RDD_VERSION    :   /* RDD version (current RDD) */
         break;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixNewArea( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixNewArea(%p)", pArea));

   if( SUPER_NEW( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   /* Size for deleted records flag */
   pArea->uiRecordLen = 1;

   pArea->iFileType = sixFileType;

   return HB_SUCCESS;
}

static HB_ERRCODE sixOpen( SIXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFields, uiCount;
   BOOL fRetry;
   char szName[ SIX_MAX_FIELD_NAME + 1 ];
   char path[ HB_PATH_MAX ], szFileName[ HB_PATH_MAX ];
   PHB_FNAME pFileName;
   char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
   unsigned int pusBufLen;
   char cType[2];
   DBFIELDINFO dbFieldInfo;
   SHORT iErrorLevel;
   PHB_ITEM pError = NULL;
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("sixOpen(%p)", pArea));

   if( cTriggerName[0] )
   {
      PHB_ITEM pFileItm = hb_itemPutC( NULL, pOpenInfo->abName );
      BOOL fResult;

      strcpy( pArea->cTriggerName, cTriggerName );
      pArea->fTriggerActive = TRUE;
      cTriggerName[0] = '\0';
      fResult = sxTriggerCall( pArea, EVENT_PREUSE, pArea->area.uiArea, 0, pFileItm );
      hb_itemRelease( pFileItm );
      if( !fResult )
      {
         return HB_FAILURE;
      }
   }

   pFileName = hb_fsFNameSplit( ( char * ) pOpenInfo->abName );
   /* Add default file name extension if necessary */
   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = s_szTableExt;
   }
   hb_fsFNameMerge( ( char * ) szFileName, pFileName );
   /* Create default alias if necessary */
   if( !pOpenInfo->atomAlias && pFileName->szName )
   {
      hb_strncpyUpperTrim( szAlias, pFileName->szName, HB_RDD_MAX_ALIAS_LEN );
      pOpenInfo->atomAlias = szAlias;
   }
   hb_xfree( pFileName );

   pArea->iOrdCurrent = 0;

   while( !hb_spFile( szFileName, path ) )
   {
      if( !pError )
      {
         pError = hb_errNew();
         hb_errPutGenCode( pError, EG_OPEN );
         hb_errPutSubCode( pError, EDBF_OPEN_DBF );
         hb_errPutOsCode( pError, 2 );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
         hb_errPutFileName( pError, ( char * ) szFileName );
         hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
      }
      if( SELF_ERROR( ( AREAP ) pArea, pError ) != E_RETRY )
      {
         hb_itemRelease( pError );
         SELF_CLOSE( ( AREAP ) pArea );
         return HB_FAILURE;
      }
   }
   if( pError )
   {
      hb_itemRelease( pError );
   }

   pArea->szDataFileName = hb_strdup( szFileName );

   do  {

      iErrorLevel = sx_ErrorLevel( 2 );
      pArea->uiSxArea = sx_Use( (PBYTE)path, (PBYTE)pOpenInfo->atomAlias,
                                ( pOpenInfo->fReadonly ) ? READONLY :
                                ( ( pOpenInfo->fShared ) ? READWRITE : EXCLUSIVE ),
                                ( SHORT ) pArea->iFileType );
      sx_ErrorLevel( iErrorLevel );
      if( !pArea->uiSxArea )
      {
        USHORT uiOsCOde = dwError == 1800 || dwError == 200  ? 32 : 0;

        fRetry = sixRTError( pArea, EG_OPEN, dwError, pArea->szDataFileName, EF_CANRETRY | EF_CANDEFAULT,uiOsCOde ) == E_RETRY;

      }
      else
        fRetry = FALSE;

   }
   while( fRetry );
   if( !pArea->uiSxArea )
         return HB_FAILURE;

   iSxCurrArea = pArea->uiSxArea;
   pArea->fShared   = pOpenInfo->fShared;
   pArea->fReadonly = pOpenInfo->fReadonly;

   SELF_FIELDCOUNT( ( AREAP ) pArea, &uiFields );
   SELF_SETFIELDEXTENT( ( AREAP ) pArea, uiFields );

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;

   pusBufLen = pArea->area.uiMaxFieldNameLength;

   for( uiCount = 1; uiCount <= uiFields; uiCount++ )
   {
      strcpy( szName, ( char * ) sx_FieldName( uiCount ) );
      szName[pusBufLen] = '\0';
      dbFieldInfo.atomName = ( const char * ) szName;

      strcpy( cType, (char*) sx_FieldType( (unsigned char *) szName ) );

      dbFieldInfo.uiLen = ( USHORT ) sx_FieldWidth( (unsigned char *) szName );
      dbFieldInfo.uiDec = 0;

      dbFieldInfo.uiTypeExtended = 0;
      switch( cType[0] )
      {
         case 'C':
            dbFieldInfo.uiType = HB_FT_STRING;
            break;

         case 'N':
            dbFieldInfo.uiType = HB_FT_LONG;
            dbFieldInfo.uiDec = ( USHORT ) sx_FieldDecimals( (unsigned char *) szName );
            break;

         case 'L':
            dbFieldInfo.uiType = HB_FT_LOGICAL;
            break;

         case 'D':
            dbFieldInfo.uiType = HB_FT_DATE;
            break;

         case 'M':
            dbFieldInfo.uiType = HB_FT_MEMO;
            break;

         case 'V':
            dbFieldInfo.uiType = HB_FT_MEMO;
            dbFieldInfo.uiTypeExtended = SIX_VARCHAR;
            break;

         case 'F':
            dbFieldInfo.uiType = HB_FT_DOUBLE;
            dbFieldInfo.uiDec = 0;
            break;

         case 'I':
            dbFieldInfo.uiType = HB_FT_LONG;
            dbFieldInfo.uiDec = 0;
            break;

      }

      SELF_ADDFIELD( ( AREAP ) pArea, &dbFieldInfo );
   }

   pArea->fMemoBinary = fMemoBinary;

   /* Alloc buffer */
   pArea->pRecord = ( BYTE * ) hb_xgrab( sx_RecSize() + 1 );

   if( SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   errCode = SELF_GOTOP( ( AREAP ) pArea );

   if( errCode == HB_SUCCESS )
   {
      sxTriggerCall( pArea, EVENT_POSTUSE, pArea->area.uiArea, 0, NULL );
   }
   return errCode;
}

#define  sixRelease               NULL

static HB_ERRCODE sixStructSize( SIXAREAP pArea, USHORT * StructSize )
{
   HB_SYMBOL_UNUSED( pArea );

   * StructSize = sizeof( SIXAREA );
   return HB_SUCCESS;
}

static HB_ERRCODE sixSysName( SIXAREAP pArea, BYTE * pBuffer )
{

   HB_TRACE(HB_TR_DEBUG, ("sixSysName(%p, %p)", pArea, pBuffer));

   switch( pArea->iFileType )
   {
      case SDENTX:
         strcpy( (char *) pBuffer, "SIXNTX");
         break;
      case SDEFOX:
      #if defined(SDEVFOX)      
      case SDEVFOX:
      #endif
         strcpy( (char *) pBuffer, "SIXCDX");         
         break;         
      case SDENSX:
      #if defined(SDENSXDBT)      
      case SDENSXDBT:
      #endif
         strcpy( (char *) pBuffer, "SIXNSX");
         break;
   }

   return HB_SUCCESS;
}


#define  sixEval                  NULL

static HB_ERRCODE sixPack( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixPack(%p)", pArea));

   if( !sxTriggerCall( pArea, EVENT_PACK, pArea->area.uiArea, 0, NULL ) )
      return HB_SUCCESS;

   if( pArea->fReadonly )
   {
      sixRTError( pArea, EG_READONLY, EDBF_READONLY, NULL, 0 ,0);
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      sixRTError( pArea, EG_SHARED, EDBF_SHARED, NULL, 0 ,0);
      return HB_FAILURE;
   }

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   sx_Pack();

   return SELF_GOTOP( ( AREAP ) pArea );
}

#define  sixPackRec               NULL
#define  sixSort                  NULL
#define  sixTrans                 NULL
#define  sixTransRec              NULL

static HB_ERRCODE sixZap( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixZap(%p)", pArea));

   if( !sxTriggerCall( pArea, EVENT_ZAP, pArea->area.uiArea, 0, NULL ) )
      return HB_SUCCESS;

   if( pArea->fReadonly )
   {
      sixRTError( pArea, EG_READONLY, EDBF_READONLY, NULL, 0 ,0);
      return HB_FAILURE;
   }
   if( pArea->fShared )
   {
      sixRTError( pArea, EG_SHARED, EDBF_SHARED, NULL, 0 ,0);
      return HB_FAILURE;
   }

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   sx_Zap();

   return SELF_GOTOP( ( AREAP ) pArea );
}

static HB_ERRCODE sixChildEnd( SIXAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("sixChildEnd(%p, %p)", pArea, pRelInfo));

   if( pArea->lpdbPendingRel == pRelInfo )
      uiError = SELF_FORCEREL( ( AREAP ) pArea );
   else
      uiError = HB_SUCCESS;
   SUPER_CHILDEND( ( AREAP ) pArea, pRelInfo );
   return uiError;
}

static HB_ERRCODE sixChildStart( SIXAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixChildStart(%p, %p)", pArea, pRelInfo));

   SELF_CHILDSYNC( ( AREAP ) pArea, pRelInfo );
   return SUPER_CHILDSTART( ( AREAP ) pArea, pRelInfo );
}

static HB_ERRCODE sixChildSync( SIXAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixChildSync(%p, %p)", pArea, pRelInfo));
   pArea->lpdbPendingRel = pRelInfo;
   if( pArea->area.lpdbRelations )
      SELF_SYNCCHILDREN( ( AREAP ) pArea );
   return HB_SUCCESS;
}

#define sixSyncChildren                         NULL

static HB_ERRCODE sixClearRel( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixClearRel(%p)", pArea ));

   if( pArea->uiSxArea )
   {
      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
      sx_SetRelation( 0, ( char * ) "" );
   }
   return SUPER_CLEARREL( ( AREAP ) pArea );
}

static HB_ERRCODE sixForceRel( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixForceRel(%p)", pArea));

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;

      if( ! lpdbPendingRel->isOptimized )
      {
         SELF_RELEVAL( ( AREAP ) pArea, lpdbPendingRel );
      }

      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
      hb_sixUpdateAreaFlags( pArea,0 );
   }
   return HB_SUCCESS;
}

#define sixRelArea                              NULL
#define sixRelEval                              NULL
#define sixRelText                              NULL

static HB_ERRCODE sixSetRel( SIXAREAP pArea, LPDBRELINFO  lpdbRelations )
{
   char *szExp;

   HB_TRACE(HB_TR_DEBUG, ("sixSetRel(%p, %p)", pArea, lpdbRelations));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   szExp = hb_itemGetCPtr( lpdbRelations->abKey );
   if( lpdbRelations->lpaChild->rddID == usSixRddID && *szExp &&
       sixEvalTest( szExp ) != 0 && 0 )
   {
      lpdbRelations->isOptimized = TRUE;
      sx_SetRelation( ((SIXAREAP) lpdbRelations->lpaChild)->uiSxArea, szExp );
   }
   else
   {
      lpdbRelations->isOptimized = FALSE;
   }

   return SUPER_SETREL( ( AREAP ) pArea, lpdbRelations );
}

static HB_ERRCODE sixOrderListAdd( SIXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   USHORT iOrder, iMaxOrder, i;
   char * szFileName;
   char path[ HB_PATH_MAX - 1];
   PHB_FNAME pFileName;

   HB_TRACE(HB_TR_DEBUG, ("sixOrderListAdd(%p, %p)", pArea, pOrderInfo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   szFileName = ( char * ) hb_xgrab( HB_PATH_MAX + 2 );
   szFileName[ 0 ] = '\0';
   strcpy( szFileName, hb_itemGetCPtr( pOrderInfo->atomBagName ) );
   if( strlen( szFileName ) == 0 )
   {
      hb_xfree( szFileName );
      return HB_FAILURE;
   }
   pFileName = hb_fsFNameSplit( szFileName );
   if( !pFileName->szExtension )
   {
      DBORDERINFO pExtInfo;
      memset( (void*)&pExtInfo, 0 ,sizeof( DBORDERINFO ) );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strcat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ) );
      hb_itemRelease( pExtInfo.itmResult );
   }

   for( iMaxOrder=1; strlen( ( char * ) sx_TagName( iMaxOrder ) ) > 0; iMaxOrder++ );
   iMaxOrder --;
   if( FindFile( szFileName, path ) )
      iOrder = sx_IndexOpen( path );
   else
      iOrder = sx_IndexOpen( szFileName );
   for( i=1; strlen((char*)sx_TagName(i))>0; i++ );
   i --;

   if( i > iMaxOrder )
      sx_SetOrder( iMaxOrder+1 );
   else
   {
      if( pArea->iOrdCurrent )
         sx_SetOrder( pArea->iOrdCurrent );
   }

   hb_xfree( szFileName );
   hb_xfree( pFileName );

   if( !iOrder )
       return HB_FAILURE;

   return HB_SUCCESS;
}

static HB_ERRCODE sixOrderListClear( SIXAREAP pArea )
{

   char szFileName[ HB_PATH_MAX - 1 ], *pBuffer, *ptr, *ptrI;
   BOOL lProduction = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("sixOrderListClear(%p)", pArea));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   pBuffer = (char*) sx_BaseName();
   ptr = strrchr( pBuffer,'\\' );
   strcpy( szFileName, (ptr)? ptr+1 : pBuffer );
   ptr = strrchr( szFileName,'.' );
   if( ptr )
      *ptr = '\0';
   pBuffer = (char*) sx_IndexName( 1 );
   ptrI = strrchr( pBuffer,'.' );
   if( ptrI )
      *ptrI = '\0';
   ptr = strrchr( pBuffer,'\\' );
   if( !ptr )
      ptr = pBuffer;
   else
      ptr++;
   if( !strcmp( szFileName,ptr ) )
   {
      lProduction = TRUE;
      if( ptrI )
         *ptrI = '.';
      strcpy( szFileName,pBuffer );
   }

   sx_CloseIndexes();

   if( lProduction )
   {
      char path[ HB_PATH_MAX ];
      if( FindFile( szFileName, path ) )
         sx_IndexOpen( path );
      else
         sx_IndexOpen( szFileName );
   }

   if( pArea->iOrdCurrent < sx_SysProp(SDE_SP_GETINDEXCOUNT, NULL) )
      sx_SetOrder( pArea->iOrdCurrent );
   else
      pArea->iOrdCurrent = sx_IndexOrd();

   return HB_SUCCESS;
}

#define sixOrderListDelete                      NULL

static HB_ERRCODE sixOrderListFocus( SIXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixOrderListFocus(%p, %p)", pArea, pOrderInfo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   if( !pArea->iOrdCurrent )
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, "" );
   else
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                                 ( char* ) sx_TagName( pArea->iOrdCurrent ) );

   if( pOrderInfo->itmOrder )
   {
      SHORT usOrder;

      if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         usOrder = sx_TagArea( (unsigned char *) hb_itemGetCPtr( pOrderInfo->itmOrder ) );
         if( !usOrder )
            return HB_FAILURE;
      }
      else if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
      {
         usOrder = (SHORT) hb_itemGetNI( pOrderInfo->itmOrder );
      }
      else
      {
         return HB_FAILURE;
      }
      sx_SetOrder( usOrder );
      pArea->iOrdCurrent = sx_IndexOrd();
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixOrderListRebuild( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixOrderListRebuild(%p)", pArea));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   sx_Reindex();
   return SELF_GOTOP( ( AREAP ) pArea );
}

#define  sixOrderCondition        NULL

static HB_ERRCODE sixOrderCreate( SIXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   BOOL  isCompound = FALSE;
   SHORT iOptions = 0;
   PHB_ITEM   pExprItem = pOrderInfo->abExpr;
   USHORT usOrder;

   HB_TRACE(HB_TR_DEBUG, ("sixOrderCreate(%p, %p)", pArea, pOrderInfo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   if( !pArea->area.lpdbOrdCondInfo || ( pArea->area.lpdbOrdCondInfo->fAll &&
                                    !pArea->area.lpdbOrdCondInfo->fAdditive ) )
      SELF_ORDLSTCLEAR( ( AREAP ) pArea );

   if( !pOrderInfo->abBagName || *(pOrderInfo->abBagName) == '\0' )
      isCompound  = TRUE;
   else
   {
      if( pOrderInfo->atomBagName && *(pOrderInfo->atomBagName) != '\0' )
         isCompound  = TRUE;
   }

   if ( pArea->area.lpdbOrdCondInfo )
   {
      if( pArea->area.lpdbOrdCondInfo->fCustom )
         iOptions = IDX_EMPTY;
   }
   if ( pOrderInfo->fUnique )
      iOptions = IDX_UNIQUE;

   if( isCompound )
      usOrder = sx_IndexTag( (PBYTE)pOrderInfo->abBagName, (PBYTE)pOrderInfo->atomBagName,
          hb_itemGetCPtr( pExprItem ), iOptions,
          ( pArea->area.lpdbOrdCondInfo )? pArea->area.lpdbOrdCondInfo->fDescending : FALSE,
          ( pArea->area.lpdbOrdCondInfo && pArea->area.lpdbOrdCondInfo->abFor ) ? pArea->area.lpdbOrdCondInfo->abFor : "" );
   else
      usOrder = sx_Index( (PBYTE)pOrderInfo->abBagName, hb_itemGetCPtr( pExprItem ), iOptions,
        ( pArea->area.lpdbOrdCondInfo )? pArea->area.lpdbOrdCondInfo->fDescending : FALSE,
        ( pArea->area.lpdbOrdCondInfo && pArea->area.lpdbOrdCondInfo->abFor ) ? pArea->area.lpdbOrdCondInfo->abFor :  "" );

   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );

   if ( !usOrder )
   {
      sixRTError( pArea, EG_CREATE, ( USHORT ) 0, (char*) pOrderInfo->abBagName, 0 ,0);
      return HB_FAILURE;
   }
   else
      pArea->iOrdCurrent = usOrder;

   return SELF_GOTOP( ( AREAP ) pArea );
}

static HB_ERRCODE sixOrderDestroy( SIXAREAP pArea, LPDBORDERINFO pOrderInfo )
{

   HB_TRACE(HB_TR_DEBUG, ("sixOrderDestroy(%p, %p)", pArea, pOrderInfo));

   HB_SYMBOL_UNUSED( pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   if( pOrderInfo->itmOrder )
   {
      SHORT usOrder = 0;
      if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
      {
         usOrder = (SHORT) hb_itemGetNI( pOrderInfo->itmOrder );
      }
      else if( HB_IS_STRING( pOrderInfo->itmOrder ) )
      {
         usOrder = sx_TagArea( (unsigned char *) hb_itemGetCPtr( pOrderInfo->itmOrder ) );
      }
      if( usOrder )
      {
         if( pArea->iOrdCurrent != usOrder )
         {
            sx_SetOrder( usOrder );
         }

         sx_IndexClose();
         if( pArea->iOrdCurrent != usOrder )
            sx_SetOrder( pArea->iOrdCurrent );
         else
            pArea->iOrdCurrent = 0;
      }
      else
      {
         return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

static HB_ERRCODE sixOrderInfo( SIXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{

   USHORT usOrder = pArea->iOrdCurrent, usOldOrder = 0;
   SHORT  iErrorLevel = 0;
   char * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("sixOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( ( AREAP ) pArea );

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   if( uiIndex != DBOI_ORDERCOUNT && pOrderInfo->itmOrder && !HB_IS_NIL(pOrderInfo->itmOrder) )
   {
      if( HB_IS_NUMERIC( pOrderInfo->itmOrder ) )
         usOrder = (USHORT) hb_itemGetNI( pOrderInfo->itmOrder );
      else if( HB_IS_STRING( pOrderInfo->itmOrder ) )
         usOrder = sx_TagArea( (unsigned char *) hb_itemGetCPtr( pOrderInfo->itmOrder ) );
   }
   if( usOrder != pArea->iOrdCurrent )
   {
      iErrorLevel = sx_ErrorLevel( 0 );
      usOldOrder = sx_SetOrder( usOrder );
      if( sx_IndexOrd() != usOrder )
         usOrder = 0;
   }

   switch( uiIndex )
   {
      case DBOI_AUTOOPEN:
         hb_itemPutL( pOrderInfo->itmResult, !sx_SysProp( SDE_SP_GETDISABLEAUTO, NULL ) );
         break;
      case DBOI_CONDITION:
         if ( usOrder )
         {
            pBuffer = (char*) sx_IndexCondition();
            hb_itemPutCL( pOrderInfo->itmResult, pBuffer, strlen(pBuffer) );
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;
      case DBOI_EXPRESSION:
         if ( usOrder )
         {
            pBuffer = (char*) sx_IndexKey();
            hb_itemPutCL( pOrderInfo->itmResult, pBuffer, strlen(pBuffer) );
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_KEYSIZE:
      case DBOI_KEYDEC:
         if ( usOrder )
         {
            SHORT iType;
            char pExp[512];
            strcpy( pExp,(char*) sx_IndexKey() );
            iType = sx_EvalTest( (unsigned char *) pExp );
            switch( iType )
            {
               case EVAL_CHARACTER:
                  if( uiIndex == DBOI_KEYDEC )
                     hb_itemPutNL( pOrderInfo->itmResult, 0 );
                  else
                  {
                     char *ptr = (char*) sx_EvalString( (unsigned char *) pExp );
                     hb_itemPutNL( pOrderInfo->itmResult, strlen( ptr ) );
                  }
                  break;
               case EVAL_NUMERIC:
                  if( uiIndex == DBOI_KEYDEC )
                     hb_itemPutNL( pOrderInfo->itmResult, sx_FieldDecimals( (unsigned char *) pExp ) );
                  else
                     hb_itemPutNL( pOrderInfo->itmResult, sx_FieldWidth( (unsigned char *) pExp ) );
                  break;
               case EVAL_LOGICAL:
                  if( uiIndex == DBOI_KEYDEC )
                     hb_itemPutNL( pOrderInfo->itmResult, 0 );
                  else
                     hb_itemPutNI( pOrderInfo->itmResult, 1 );
                  break;
               case 4:
                  if( uiIndex == DBOI_KEYDEC )
                     hb_itemPutNL( pOrderInfo->itmResult, 0 );
                  else
                     hb_itemPutNI( pOrderInfo->itmResult, 8 );
                  break;
               default:
                  hb_itemPutNI( pOrderInfo->itmResult, 0 );
            }
         }
         else
            hb_itemPutNI( pOrderInfo->itmResult, 0 );
         break;
      case DBOI_KEYVAL:
         if( usOrder )
         {
            char szKeyVal[ SIX_MAX_KEY_LENGTH + 1 ];

            if( sx_Eof() )
               szKeyVal[ 0 ] = '\0';
            else
               hb_strncpy( szKeyVal, ( char * ) sx_KeyData(), SIX_MAX_KEY_LENGTH );
            if( szKeyVal[ 0 ] )
            {
               SHORT iType = sx_EvalTest( (unsigned char *) sx_IndexKey() );
               switch( iType )
               {
                  case EVAL_CHARACTER:
                     pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult,
                                                          szKeyVal );
                     break;

                  case EVAL_NUMERIC:
                  {
                     int iDec, iLen;
                     HB_LONG lValue;
                     double dValue;
                     if( hb_valStrnToNum( szKeyVal, strlen( szKeyVal ),
                                          &lValue, &dValue, &iDec, &iLen ) )
                        pOrderInfo->itmResult = hb_itemPutNDLen( pOrderInfo->itmResult,
                                                                 dValue, iLen, iDec );
                     else
                        pOrderInfo->itmResult = hb_itemPutNInt( pOrderInfo->itmResult,
                                                                lValue );
                     break;
                  }

                  case EVAL_LOGICAL:
                     pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult,
                                                          szKeyVal[ 0 ] == 'T' );
                     break;

                  case EVAL_DATE:
                     /* SEEMS that SDE documentation is wrong and always
                        CCYYMMDD format is used */
                     /*
                     if( pArea->iFileType == SDENTX )
                        pOrderInfo->itmResult = hb_itemPutDS( pOrderInfo->itmResult,
                                                              szKeyVal );
                     else
                        pOrderInfo->itmResult = hb_itemPutDL( pOrderInfo->itmResult,
                              ( LONG ) hb_strVal( szKeyVal, strlen( szKeyVal ) ) );
                     */
                     pOrderInfo->itmResult = hb_itemPutDS( pOrderInfo->itmResult,
                                                           szKeyVal );
                     break;

                  default:
                     if( pOrderInfo->itmResult )
                        hb_itemClear( pOrderInfo->itmResult );
                     break;
               }
            }
            else if( pOrderInfo->itmResult )
               hb_itemClear( pOrderInfo->itmResult );
         }
         break;
      case DBOI_KEYTYPE:
         if( usOrder )
         {
            SHORT iType;
            char pExp[512];
            strcpy( pExp,(char*) sx_IndexKey() );
            iType = sx_EvalTest( (unsigned char *) pExp );
            switch( iType )
            {
               case EVAL_CHARACTER:
                  if( uiIndex == DBOI_KEYVAL )
                     hb_itemPutC( pOrderInfo->itmResult,
                           (char*) sx_EvalString( (unsigned char *) pExp ) );
                  else
                     hb_itemPutC( pOrderInfo->itmResult, "C" );
                  break;
               case EVAL_NUMERIC:
                  if( uiIndex == DBOI_KEYVAL )
                     hb_itemPutND( pOrderInfo->itmResult,
                                   sx_EvalNumeric( (unsigned char *) pExp ) );
                  else
                     hb_itemPutC( pOrderInfo->itmResult, "N" );
                  break;
               case EVAL_LOGICAL:
                  if( uiIndex == DBOI_KEYVAL )
                     hb_itemPutL( pOrderInfo->itmResult,
                                  sx_EvalLogical( (unsigned char *) pExp ) != 0 );
                  else
                     hb_itemPutC( pOrderInfo->itmResult, "L" );
                  break;
               case 4:
                  if( uiIndex == DBOI_KEYVAL )
                     hb_itemPutDS( pOrderInfo->itmResult,
                           (char*) sx_EvalString( (unsigned char *) pExp ) );
                  else
                     hb_itemPutC( pOrderInfo->itmResult, "D" );
                  break;
               default:
                  if( uiIndex == DBOI_KEYVAL )
                     hb_itemPutC( pOrderInfo->itmResult, "" );
                  else
                     hb_itemPutC( pOrderInfo->itmResult, "U" );
            }
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_ISCOND:
         hb_itemPutL( pOrderInfo->itmResult, ( usOrder && ( sx_IndexType() >= INDEX_CONDITIONAL ) ) );
         break;

      case DBOI_ISDESC:
         hb_itemPutL( pOrderInfo->itmResult, (usOrder)? sx_SysProp( SDE_SP_GETDESCENDING,NULL ) : 0 );
         break;

      case DBOI_UNIQUE:
         hb_itemPutL( pOrderInfo->itmResult, ( usOrder && !(sx_IndexType() % 2) ) );
         break;

      case DBOI_CUSTOM :
         hb_itemPutL( pOrderInfo->itmResult, (usOrder)? sx_SysProp( SDE_SP_GETEMPTY, NULL ) : 0 );
         break;

      case DBOI_NAME:
         if( usOrder )
         {
            pBuffer = (char*)sx_TagName( usOrder );
            hb_itemPutCL( pOrderInfo->itmResult, pBuffer, strlen(pBuffer) );
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_FULLPATH :
      case DBOI_BAGNAME:
         if( usOrder )
         {
            char *ptr;
            pBuffer = (char*)sx_IndexName( usOrder );
            ptr = strrchr( pBuffer,'\\' );
            if( uiIndex == DBOI_BAGNAME )
            {
               if( ptr )
                  hb_itemPutCL( pOrderInfo->itmResult, ptr+1, strlen(ptr+1) );
               else
                  hb_itemPutCL( pOrderInfo->itmResult, pBuffer, strlen(pBuffer) );
            }
            else
            {
               if( ptr )
               {
                  *ptr = '\0';
                  hb_itemPutCL( pOrderInfo->itmResult, pBuffer, strlen(pBuffer) );
               }
               else
                  hb_itemPutC( pOrderInfo->itmResult, "" );
            }
         }
         else
            hb_itemPutC( pOrderInfo->itmResult, "" );
         break;

      case DBOI_NUMBER :
         hb_itemPutNI( pOrderInfo->itmResult, usOrder );
         break;

      case DBOI_BAGEXT:
         hb_itemPutC( pOrderInfo->itmResult, sixIndexExt( pArea->iFileType ) );
         break;

      case DBOI_POSITION :
         hb_itemPutNL( pOrderInfo->itmResult,
                       ( usOrder ) ? sx_OrderRecNo() : ( LONG ) sx_RecNo() );
         break;

      case DBOI_KEYCOUNTRAW :
      case DBOI_KEYCOUNT :
         hb_itemPutNL(pOrderInfo->itmResult, sx_QueryRecCount() );
         break;

      case DBOI_ORDERCOUNT:
         hb_itemPutNL( pOrderInfo->itmResult, sx_SysProp(SDE_SP_GETINDEXCOUNT,NULL) );
         break;

      case DBOI_SCOPETOP :
         if( pArea->iOrdCurrent )
         {
            if( pOrderInfo->itmResult )
               sxGetScope(  pArea, 0, pOrderInfo->itmResult, NULL );
            if( pOrderInfo->itmNewVal )
               sxSetScope( pArea, 0, pOrderInfo->itmNewVal );
         }
         else if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOM :
         if( pArea->iOrdCurrent )
         {
            if( pOrderInfo->itmResult )
               sxGetScope(  pArea, 1, pOrderInfo->itmResult, NULL );
            if( pOrderInfo->itmNewVal )
               sxSetScope( pArea, 1, pOrderInfo->itmNewVal );
         }
         else if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPESET:
         if( pOrderInfo->itmNewVal )
         {
            sxSetScope( pArea, 0, pOrderInfo->itmNewVal );
            sxSetScope( pArea, 1, pOrderInfo->itmNewVal );
         }
         if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPETOPCLEAR :
         if( pArea->iOrdCurrent )
         {
            if( pOrderInfo->itmResult )
               sxGetScope( pArea, 0, pOrderInfo->itmResult, NULL );
            sxSetScope( pArea, 0, NULL );
         }
         else if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOMCLEAR :
         if( pArea->iOrdCurrent )
         {
            if( pOrderInfo->itmResult )
               sxGetScope(  pArea, 1, pOrderInfo->itmResult, NULL );
            sxSetScope( pArea, 1, NULL );
         }
         else if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_SCOPECLEAR:
         sxSetScope( pArea, 0, NULL );
         sxSetScope( pArea, 1, NULL );
         if( pOrderInfo->itmResult )
            hb_itemClear( pOrderInfo->itmResult );

      default:
         SUPER_ORDINFO( ( AREAP ) pArea, uiIndex, pOrderInfo );
         break;
   }
   if( usOrder != pArea->iOrdCurrent )
   {
      sx_SetOrder( usOldOrder );
      sx_ErrorLevel( iErrorLevel );
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixClearFilter( SIXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("sixClearFilter(%p)", pArea));

   if( pArea->uiSxArea )
   {
      if( iSxCurrArea != pArea->uiSxArea )
      {
         iSxCurrArea = pArea->uiSxArea;
         sx_Select( iSxCurrArea );
      }
      sx_Query( NULL );
   }

   pArea->iFltCurrent = 0;
   pArea->fFltOptimized = FALSE;

   return SUPER_CLEARFILTER( ( AREAP ) pArea );
}

#define  sixClearLocate           NULL
#define  sixClearScope            NULL
#define  sixCountScope            NULL
#define  sixFilterText            NULL
#define  sixScopeInfo             NULL

static HB_ERRCODE sixSetFilter( SIXAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixSetFilter(%p, %p)", pArea, pFilterInfo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   if( SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo ) == HB_SUCCESS )
   {
      pArea->fFltOptimized = FALSE;
      if( sixEvalTest( hb_itemGetCPtr( pFilterInfo->abFilterText) ) )
      {
         if( hb_setGetOptimize() )
         {
            sx_Query( (unsigned char *) hb_itemGetCPtr( pFilterInfo->abFilterText ) );
            pArea->fFltOptimized = TRUE;
         }
         else
         {
            sx_SetFilter( (unsigned char *) hb_itemGetCPtr( pFilterInfo->abFilterText ) );
         }
      }
   }
   return HB_SUCCESS;
}

#define  sixSetLocate             NULL
#define  sixSetScope              NULL
#define  sixSkipScope             NULL
#define  sixLocate                NULL
#define  sixCompile               NULL
#define  sixError                 NULL
#define  sixEvalBlock             NULL

static HB_ERRCODE sixRawLock( SIXAREAP pArea, USHORT uiAction, ULONG lRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("sixRawLock(%p, %hu, %lu)", pArea, uiAction, lRecNo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   switch( uiAction )
   {
      case REC_LOCK:
         if( !pArea->fShared || pArea->fFLocked )
            return HB_SUCCESS;
         if( ! sx_Rlock( lRecNo ) )
            return HB_FAILURE;
         break;

      case REC_UNLOCK:
         if( !pArea->fShared || pArea->fFLocked )
            return HB_SUCCESS;
         sx_Unlock( lRecNo );

      case FILE_LOCK:
         if( !pArea->fShared || pArea->fFLocked )
            return HB_SUCCESS;
         if( !sx_Flock() )
            return HB_FAILURE;
         pArea->fFLocked = TRUE;
         break;

      case FILE_UNLOCK:
         if( !pArea->fShared )
            return TRUE;
         sx_Unlock( 0 );
         pArea->fFLocked = FALSE;
         break;
   }
   return HB_SUCCESS;
}

static HB_ERRCODE sixLock( SIXAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   USHORT uiAction;
   ULONG ulRecNo = 0;

   HB_TRACE(HB_TR_DEBUG, ("sixLock(%p, %p)", pArea, pLockInfo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }

   switch( pLockInfo->uiMethod )
   {
      case DBLM_EXCLUSIVE :
         sx_Unlock( 0 );
         uiAction = REC_LOCK ;
         break;

      case DBLM_MULTIPLE :
         if( pLockInfo->itmRecID )
            ulRecNo = ( ULONG ) hb_itemGetNL( pLockInfo->itmRecID ) ;
         uiAction = REC_LOCK ;
         break;

      case DBLM_FILE :
         uiAction = FILE_LOCK ;
         break;

      default  :
         pLockInfo->fResult = FALSE;
         return HB_FAILURE;
   }

   pLockInfo->fResult = SELF_RAWLOCK( ( AREAP ) pArea, uiAction,
                                 ulRecNo ? ulRecNo : sx_RecNo() ) == HB_SUCCESS;
   return HB_SUCCESS;
}

static HB_ERRCODE sixUnLock( SIXAREAP pArea, PHB_ITEM pRecNo )
{
   ULONG ulRecNo;

   HB_TRACE(HB_TR_DEBUG, ("sixUnLock(%p, %p)", pArea, pRecNo));

   if( iSxCurrArea != pArea->uiSxArea )
   {
      iSxCurrArea = pArea->uiSxArea;
      sx_Select( iSxCurrArea );
   }
   ulRecNo = hb_itemGetNL( pRecNo );

   return SELF_RAWLOCK( ( AREAP ) pArea,
                        ulRecNo ? REC_UNLOCK : FILE_UNLOCK, ulRecNo );
}

static HB_ERRCODE sixDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex )
{
   char szFileName[ HB_PATH_MAX ], * szFile, * szExt;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   BOOL fTable = FALSE, fResult = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("sixDrop(%p,%p,%p)", pRDD, pItemTable, pItemIndex));

   szFile = hb_itemGetCPtr( pItemIndex );
   if( !szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = hb_itemGetCPtr( pItemTable );
      if( !szFile[ 0 ] )
         return FALSE;
      fTable = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( !pFileName->szExtension )
   {
      /* Add default extension if missing */
      pFileExt = hb_itemPutC( NULL, "" );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, 0, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );

   /* Use hb_spFile first to locate table which can be in differ path */
   if( hb_spFile( szFileName, szFileName ) )
   {
      fResult = hb_fsDelete( szFileName );
      if( fResult && fTable )
      {
         /*
          * Database table file has been deleted, now check if memo is
          * supported and if yes then try to delete memo file if it exists
          * in the same directory as table file
          * hb_fsFNameSplit() repeated intentionally to respect
          * the path set by hb_spFile()
          */
         pFileName = hb_fsFNameSplit( szFileName );
         pFileExt = hb_itemPutC( pFileExt, "" );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, 0, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsDelete( szFileName );
            }
         }
         /*
          * and try to delete production index also if it exists
          * in the same directory as table file
          */
         pFileExt = hb_itemPutC( pFileExt, "" );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, 0, pFileExt ) == HB_SUCCESS )
         {
            szExt = hb_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               hb_fsFNameMerge( szFileName, pFileName );
               hb_fsDelete( szFileName );
            }
         }
         hb_xfree( pFileName );
      }
   }
   if( pFileExt )
   {
      hb_itemRelease( pFileExt );
   }

   return fResult ? HB_SUCCESS : HB_FAILURE;
}

#define  sixRename             NULL

static HB_ERRCODE sixExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex )
{
   char szFileName[ HB_PATH_MAX ], * szFile;
   PHB_ITEM pFileExt = NULL;
   PHB_FNAME pFileName;
   BOOL fTable = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("sixExists(%p,%p,%p)", pRDD, pItemTable, pItemIndex));

   szFile = hb_itemGetCPtr( pItemIndex );
   if( !szFile[ 0 ] )
   {
      szFile = hb_itemGetCPtr( pItemTable );
      if( !szFile[ 0 ] )
         return FALSE;
      fTable = TRUE;
   }

   pFileName = hb_fsFNameSplit( szFile );

   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, 0, pFileExt ) == HB_SUCCESS )
         pFileName->szExtension = hb_itemGetCPtr( pFileExt );
   }
   hb_fsFNameMerge( szFileName, pFileName );
   hb_xfree( pFileName );
   if( pFileExt )
   {
      hb_itemRelease( pFileExt );
   }

   return hb_spFile( szFileName, NULL ) ? HB_SUCCESS : HB_FAILURE;
}

static HB_ERRCODE sixRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("sixRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         hb_itemPutL( pItem, TRUE );
         break;

      case RDDI_TABLEEXT:
      {
         char * szTableExt = hb_strdup( s_szTableExt ), *szNew;

         szNew = hb_itemGetCPtr( pItem );
         if( szNew[0] == '.' && szNew[1] )
            hb_strncpy( s_szTableExt, szNew, HB_MAX_FILE_EXT );
         hb_itemPutCPtr( pItem, szTableExt, strlen( szTableExt ) );
         break;
      }
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
         hb_itemPutC( pItem, sixIndexExt( sixFileType ) );
         break;

      case RDDI_MEMOEXT:
         hb_itemPutC( pItem, sixMemoExt( sixFileType ) );
         break;

      case RDDI_TABLETYPE:
      {
         int iType = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, sixFileType );
         switch( iType )
         {
            case SDENTX:
            case SDEFOX:
            case SDENSX:
            #if defined(SDEVFOX)      
            case SDEVFOX:
            #endif
            #if defined(SDENSXDBT)      
            case SDENSXDBT:
            #endif
            
               sixFileType = iType;
         }
      }

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return HB_SUCCESS;
}

#define  sixInit                  NULL

static HB_ERRCODE sixExit( LPRDDNODE pRDD )
{
   HB_SYMBOL_UNUSED( pRDD );

   if( s_iHBSetHandle )
   {
      hb_setListenerRemove( s_iHBSetHandle ) ;
      s_iHBSetHandle = 0;
   }
   usSixRddID = 0;

   return HB_SUCCESS;
}

#define  sixCloseMemFile          NULL
#define  sixCreateMemFile         NULL
#define  sixGetValueFile          NULL
#define  sixOpenMemFile           NULL
#define  sixPutValueFile          NULL
#define  sixReadDBHeader          NULL
#define  sixWriteDBHeader         NULL
#define  sixWhoCares              NULL

static RDDFUNCS sixTable = { ( DBENTRYP_BP ) sixBof,
                             ( DBENTRYP_BP ) sixEof,
                             ( DBENTRYP_BP ) sixFound,
                             ( DBENTRYP_V ) sixGoBottom,
                             ( DBENTRYP_UL ) sixGoTo,
                             ( DBENTRYP_I ) sixGoToId,
                             ( DBENTRYP_V ) sixGoTop,
                             ( DBENTRYP_BIB ) sixSeek,
                             ( DBENTRYP_L ) sixSkip,
                             ( DBENTRYP_L ) sixSkipFilter,
                             ( DBENTRYP_L ) sixSkipRaw,
                             ( DBENTRYP_VF ) sixAddField,
                             ( DBENTRYP_B ) sixAppend,
                             ( DBENTRYP_I ) sixCreateFields,
                             ( DBENTRYP_V ) sixDeleteRec,
                             ( DBENTRYP_BP ) sixDeleted,
                             ( DBENTRYP_SP ) sixFieldCount,
                             ( DBENTRYP_VF ) sixFieldDisplay,
                             ( DBENTRYP_SSI ) sixFieldInfo,
                             ( DBENTRYP_SCP ) sixFieldName,
                             ( DBENTRYP_V ) sixFlush,
                             ( DBENTRYP_PP ) sixGetRec,
                             ( DBENTRYP_SI ) sixGetValue,
                             ( DBENTRYP_SVL ) sixGetVarLen,
                             ( DBENTRYP_V ) sixGoCold,
                             ( DBENTRYP_V ) sixGoHot,
                             ( DBENTRYP_P ) sixPutRec,
                             ( DBENTRYP_SI ) sixPutValue,
                             ( DBENTRYP_V ) sixRecall,
                             ( DBENTRYP_ULP ) sixRecCount,
                             ( DBENTRYP_ISI ) sixRecInfo,
                             ( DBENTRYP_ULP ) sixRecNo,
                             ( DBENTRYP_I ) sixRecId,
                             ( DBENTRYP_S ) sixSetFieldExtent,
                             ( DBENTRYP_CP ) sixAlias,
                             ( DBENTRYP_V ) sixClose,
                             ( DBENTRYP_VO ) sixCreate,
                             ( DBENTRYP_SI ) sixInfo,
                             ( DBENTRYP_V ) sixNewArea,
                             ( DBENTRYP_VO ) sixOpen,
                             ( DBENTRYP_V ) sixRelease,
                             ( DBENTRYP_SP ) sixStructSize,
                             ( DBENTRYP_CP ) sixSysName,
                             ( DBENTRYP_VEI ) sixEval,
                             ( DBENTRYP_V ) sixPack,
                             ( DBENTRYP_LSP ) sixPackRec,
                             ( DBENTRYP_VS ) sixSort,
                             ( DBENTRYP_VT ) sixTrans,
                             ( DBENTRYP_VT ) sixTransRec,
                             ( DBENTRYP_V ) sixZap,
                             ( DBENTRYP_VR ) sixChildEnd,
                             ( DBENTRYP_VR ) sixChildStart,
                             ( DBENTRYP_VR ) sixChildSync,
                             ( DBENTRYP_V ) sixSyncChildren,
                             ( DBENTRYP_V ) sixClearRel,
                             ( DBENTRYP_V ) sixForceRel,
                             ( DBENTRYP_SSP ) sixRelArea,
                             ( DBENTRYP_VR ) sixRelEval,
                             ( DBENTRYP_SI ) sixRelText,
                             ( DBENTRYP_VR ) sixSetRel,
                             ( DBENTRYP_VOI ) sixOrderListAdd,
                             ( DBENTRYP_V ) sixOrderListClear,
                             ( DBENTRYP_VOI ) sixOrderListDelete,
                             ( DBENTRYP_VOI ) sixOrderListFocus,
                             ( DBENTRYP_V ) sixOrderListRebuild,
                             ( DBENTRYP_VOO ) sixOrderCondition,
                             ( DBENTRYP_VOC ) sixOrderCreate,
                             ( DBENTRYP_VOI ) sixOrderDestroy,
                             ( DBENTRYP_SVOI ) sixOrderInfo,
                             ( DBENTRYP_V ) sixClearFilter,
                             ( DBENTRYP_V ) sixClearLocate,
                             ( DBENTRYP_V ) sixClearScope,
                             ( DBENTRYP_VPLP ) sixCountScope,
                             ( DBENTRYP_I ) sixFilterText,
                             ( DBENTRYP_SI ) sixScopeInfo,
                             ( DBENTRYP_VFI ) sixSetFilter,
                             ( DBENTRYP_VLO ) sixSetLocate,
                             ( DBENTRYP_VOS ) sixSetScope,
                             ( DBENTRYP_VPL ) sixSkipScope,
                             ( DBENTRYP_B ) sixLocate,
                             ( DBENTRYP_CC ) sixCompile,
                             ( DBENTRYP_I ) sixError,
                             ( DBENTRYP_I ) sixEvalBlock,
                             ( DBENTRYP_VSP ) sixRawLock,
                             ( DBENTRYP_VL ) sixLock,
                             ( DBENTRYP_I ) sixUnLock,
                             ( DBENTRYP_V ) sixCloseMemFile,
                             ( DBENTRYP_VO ) sixCreateMemFile,
                             ( DBENTRYP_SCCS ) sixGetValueFile,
                             ( DBENTRYP_VO ) sixOpenMemFile,
                             ( DBENTRYP_SCCS ) sixPutValueFile,
                             ( DBENTRYP_V ) sixReadDBHeader,
                             ( DBENTRYP_V ) sixWriteDBHeader,
                             ( DBENTRYP_R ) sixInit,
                             ( DBENTRYP_R ) sixExit,
                             ( DBENTRYP_RVVL ) sixDrop,
                             ( DBENTRYP_RVVL ) sixExists,
                             ( DBENTRYP_RVVVL ) sixRename,
                             ( DBENTRYP_RSLV ) sixRddInfo,
                             ( DBENTRYP_SVP ) sixWhoCares
                           };

HB_FUNC( SIX ) {;}

HB_FUNC( SIX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount, uiRddId;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );
   uiRddId = hb_parni( 4 );

   HB_TRACE(HB_TR_DEBUG, ("SIX_GETFUNCTABLE(%i, %p, NULL, %i)", uiCount, pTable, usSixRddID));

   if( pTable )
   {
      HB_ERRCODE errCode = hb_rddInherit( pTable, &sixTable, &sixSuper, NULL );

      if ( errCode == HB_SUCCESS )
      {
         /*
          * we HB_SUCCESSfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         usSixRddID = uiRddId;
         *uiCount = RDDFUNCSCOUNT;
         sixHBSet();
         if (!pData)
         {
         pData = (PMYERROR) hb_xgrab( sizeof(MYERROR));
         pData->lError =0;
         pData->lWorkArea=0;
         pData->sErrorMsg= (LPSTR)hb_xgrab(1024);
         pData->sExtraMsg=(LPSTR)hb_xgrab(1024);

         sx_SetErrorFunc  ((LONG) MyFunc, (LONG)pData);
         }
         //sx_SetErrorHook(200);
         //sx_SetErrorHook(1800);
         s_iHBSetHandle = hb_setListenerAdd( sixHBSetCallback );
      }
      hb_retni( errCode );
   }
   else
   {
      hb_retni( HB_FAILURE );
   }
   sx_SetStringType( 1 );

#ifdef DEMO
   {
      char cInfo[80] = RDD_VERSION;
      strcat( cInfo,"\r\n" );
      //strcat( cInfo,(char*)sx_Version() );
      MessageBox( GetActiveWindow(), cInfo, "http://www.xHarbour.com", MB_OK | MB_ICONINFORMATION );
   }
#endif
}

static void hb_sixRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SIX", RDT_FULL ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

#define __PRG_SOURCE__ __FILE__
#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_INIT_SYMBOLS_BEGIN( six1__InitSymbols )
{ "SIX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SIX )}, NULL },
{ "SIX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( SIX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( six1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_six_rdd_init_ )
   hb_vmAtInit( hb_sixRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_six_rdd_init_ )

#if defined(HB_PRAGMA_STARTUP)
#  pragma startup six1__InitSymbols
#  pragma startup _hb_six_rdd_init_
#elif defined(HB_MSC_STARTUP)
#  if _MSC_VER >= 1010
#     pragma data_seg( ".CRT$XIY" )
#     pragma comment( linker, "/Merge:.CRT=.data" )
#  else
#     pragma data_seg( "XIY" )
#  endif
   static HB_$INITSYM hb_vm_auto_six1__InitSymbols = six1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_six_rdd_init = _hb_six_rdd_init_;
#  pragma data_seg()
#endif


void MyFunc( MYERROR *w)
{
   dwError = w->lError;
}

