
/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

/*------------------------------------------------------------------------------
* Low Level C Generic Routines
*------------------------------------------------------------------------------*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "item.api"

void WG_error(LPCTSTR cError, LPCTSTR cWhere);
void WG_warning(LPCTSTR cError, LPCTSTR cWhere);

PHB_ITEM WG_GetObjectData( PHB_ITEM pObj, char* cData);
void WG_SetObjectDataDate( PHB_ITEM pObj, char* cData, char * szDate );
void WG_SetObjectDataDouble( PHB_ITEM pObj, char* cData, double dNew );
void WG_SetObjectDataInteger( PHB_ITEM pObj, char* cData, int iNew );
void WG_SetObjectDataLogical( PHB_ITEM pObj, char* cData, BOOL lNew );
void WG_SetObjectDataLong( PHB_ITEM pObj, char* cData, long nNew );
void WG_SetObjectDataString( PHB_ITEM pObj, char* cData, char *cNew );
void WG_SetObjectData( PHB_ITEM pObj, char* cData, PHB_ITEM pNew );



char * hb_itemVal2Str( PHB_ITEM pItem )
{
   PHB_ITEM pResult;
   char * buffer;
   ULONG ulLen;
   BOOL bFreeReq;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemVal2Str(%p)", pItem));

   buffer = hb_itemString( pItem, &ulLen, &bFreeReq );
   return buffer;
   //if( bFreeReq )
   //   hb_xfree( buffer );
}


// Funzione per la visualizzazione dell'errore
void WG_error(LPCTSTR cError, LPCTSTR cWhere)
{
    LPVOID lpMsgBuf;
    char   cErrorMsg[200];
    DWORD dw = GetLastError();
    static PHB_DYNS Dyns = 0 ;
    long int res;

    FormatMessage(
                   FORMAT_MESSAGE_ALLOCATE_BUFFER |
                   FORMAT_MESSAGE_FROM_SYSTEM |
                   FORMAT_MESSAGE_IGNORE_INSERTS,
                   NULL,
                   dw,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                   (LPTSTR) &lpMsgBuf,
                   0,
                   NULL
                 );

    sprintf(cErrorMsg, "Error: %s \nPosition %s.\n Error code: %u.\n %s\n",
        cError, cWhere, dw, (LPCTSTR)lpMsgBuf);

    // Display the string.
    MessageBox( NULL, (LPCTSTR)cErrorMsg, "Error", MB_OK | MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree( lpMsgBuf );

    if ( ! Dyns )
    {
        Dyns = hb_dynsymFind( "WG_GENMYERROR" );
    }

    hb_vmPushSymbol( Dyns->pSymbol );
    hb_vmPushNil();
    hb_vmPushString( cErrorMsg, strlen(cErrorMsg) );
    hb_vmDo( 1 );

    return;

}

// Funzione per la visualizzazione dell'errore
void WG_warning(LPCTSTR cError, LPCTSTR cWhere)
{
    LPVOID lpMsgBuf;
    char   cErrorMsg[200];
    DWORD dw = GetLastError();

    FormatMessage(
                   FORMAT_MESSAGE_ALLOCATE_BUFFER |
                   FORMAT_MESSAGE_FROM_SYSTEM |
                   FORMAT_MESSAGE_IGNORE_INSERTS,
                   NULL,
                   dw,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                   (LPTSTR) &lpMsgBuf,
                   0,
                   NULL
                 );

    sprintf(cErrorMsg, "Error: %s \nPosition %s.\n Error code: %u.\n %s\n",
        cError, cWhere, dw, (LPCTSTR)lpMsgBuf);

    // Display the string.
    MessageBox( NULL, (LPCTSTR)cErrorMsg, "Warning", MB_OK | MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree( lpMsgBuf );

}


PHB_ITEM WG_GetObjectData( PHB_ITEM pObj, char* cData )
{
   PHB_DYNS pDyns;
   char  cMsg[200];

   pDyns = hb_dynsymFind( cData );

   if( pDyns )
   {
      hb_vmPushSymbol( pDyns->pSymbol );   
      hb_vmPush( pObj );               
      hb_vmSend( 0 );
      return &hb_stack.Return;
   } 
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "GetObjectData");
     return 0;
   }
}

void WG_SetObjectDataDate( PHB_ITEM pObj, char* cData, char * szDate )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutDS( NULL, szDate );

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectDataDouble( PHB_ITEM pObj, char* cData, double dNew )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutND( NULL, dNew );

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectDataInteger( PHB_ITEM pObj, char* cData, int iNew )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutNI( NULL, iNew );

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectDataLogical( PHB_ITEM pObj, char* cData, BOOL lNew )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutL( NULL, lNew );

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectDataLong( PHB_ITEM pObj, char* cData, long nNew )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutNL( NULL, nNew );
   
   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectDataLong - pObj Type");

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectDataLong - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectDataString( PHB_ITEM pObj, char* cData, char *cNew )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   PHB_ITEM xValue = hb_itemPutCL( NULL, cNew, strlen(cNew) );

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );

}

void WG_SetObjectData( PHB_ITEM pObj, char* cData, PHB_ITEM xValue )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char * cSetData;
   
   strcpy( cSetData, "_" );
   strcat( cSetData, cData );
   
   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {
      
      WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj Type");
      
      hb_vmPushSymbol( pDyns->pSymbol );  
      hb_vmPush( pObj );              
      hb_vmPush( xValue );            
      hb_vmSend( 1 );
      
   }
   else 
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }
   
   hb_itemRelease( xValue );
   
}


/*
   Ron Pinkas example:

   // Same as oObj:Data
   hb_vmPushSymbol( hb_dynsymFind( "DATA" )->pSymbol )
   hb_vmPush( pObj  )
   hb_vmSend( 0 )
   
   Result is in &hb_stack.Return
   
   If you need to set a property you do like this:
   
   // Same as oObj:Data := xNew
   hb_vmPushSymbol( hb_dynsymFind( "_DATA" )->pSymbol ) // NOTE the _
   hb_vmPush( pObj  )
   hb_vmPush( pNew ) // The new value.
   hb_vmSend( 1 ) // 1 parameter.
   
   Ron
*/
