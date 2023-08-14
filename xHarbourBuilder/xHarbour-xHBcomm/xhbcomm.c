/*
 * $Id$
 */

/*
* Comm communication functions  for xBuilder
* Copyright (c) 2004 - Luiz Rafael Culik Guimaraes <culikr@bturbo.com.br>
* All Rights Reserved
*/

#include "xhbcomm.h"
#include "windows.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "tchar.h"
HB_FUNC( INIT_PORT)
/*  cPort, nBaudrate, nDatabits, nParity, nStopbits, nBuffersize

Main initiator/port opener.

   cPort may take either the form 'COMn' or the more general '\\.\COMn'
         The more general form is required for port numbers > 9.

   nParity codes: 0,1,2,3 -> none, odd, mark, even

   nStopBits codes: 0,1,2 -> 1, 1.5, 2

   Returns a "handle" >= 256.  A return of -1 means open failed.

   The handle must be used on all references to the port and is equivalent to
   nComm used in 16 bit Clipper/Fivewin routines.  All routines take the
   handle reference as their first argument.

*/

{
   void *Port = Tcomm_New();
   BYTE nParity;
   BYTE nStop;

   if ( ! Port )
   {
      hb_retptr( (void*) NULL) ;
   }

   Tcomm_SetComPort( Port, ( char * ) hb_parc( 1 ) ) ;

   if (!Tcomm_OpenCommPort( Port ))
   {
      hb_retptr( (void*) NULL) ;
   }
      
   Tcomm_SetBaudRate(Port, hb_parnl( 2 ) ) ;
   Tcomm_SetByteSize(Port, hb_parnl( 3 ) ) ;
   nParity = hb_parnl( 4 ) ;
   nStop = hb_parnl( 5 ) ;
   switch (nParity)
   {
   case 0:
      Tcomm_SetParity(Port, NOPARITY);
      break;
   case 1:
      Tcomm_SetParity(Port, ODDPARITY);
      break;

   case 2:
      Tcomm_SetParity(Port, MARKPARITY);
      break;

   case 3:
      Tcomm_SetParity(Port, EVENPARITY);
      break;

   }

   switch (nStop)
   {
   case 0 :
      Tcomm_SetStopBits(Port, ONESTOPBIT);
      break;
   case 1 :
      Tcomm_SetStopBits(Port, ONE5STOPBITS);
      break;

   case 2:
      Tcomm_SetStopBits(Port, TWOSTOPBITS);
      break;

   default:
      Tcomm_SetStopBits(Port, ONESTOPBIT);
      break;

   }
   
   hb_retptr( (void*) Port)    ;
}



HB_FUNC( OUTBUFCLR )
/* parameters
nHandle -> handle obtained with init_port call
return .t. if sucess otherwith .f.
*/
{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   { 
      hb_retl( Tcomm_PurgeOCommPort( (void *) pComm ));
   }
   else
   {
      hb_retl( 0 );
   }

}

HB_FUNC( ISWORKING )
/* parameters
nHandle -> handle obtained with init_port call
return .t. if sucess otherwith .f.
*/

{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   {
      hb_retl( Tcomm_GetConnected( (void *) pComm )) ;
   }
   else
   {
      hb_retl( 0 );
   }
}


HB_FUNC( INCHR )
{
/* parameters
nHandle -> handle obtained with init_port call
nLen -> How mamy bytes to read
szString -> Buffer with string to read
return -1 on errorm otherwise , number of bytes read
*/

   void * pComm = (void *) hb_parptr( 1 )  ;
   int iRet ;
   int  szLen = hb_parni( 2 );

   if ( ISPOINTER( 1 ) )
   {
      BYTE * szBuffer = (BYTE*) hb_xgrab( szLen + 1  ) ;
      memset( szBuffer, '\0', szLen  );
      iRet = Tcomm_ReadBytes( (void *) pComm, szBuffer, szLen) ;
   
      szBuffer[ szLen ] = '\0';
  
      if ( iRet >= 0 )
      {
         hb_storc( (char *) szBuffer , 3 ) ;
      }

      if (szBuffer)
      {
         hb_xfree(szBuffer);
      }

      hb_retnl( iRet );
   }
   else
   {
      hb_storc("",3);
      hb_retnl( -1 );
   }

}

HB_FUNC( OUTCHR )   // Send out characters.  Returns .t. if successful.
/* parameters
nHandle -> handle obtained with init_port call
szBuffer -> Stribg to write
return .t. if sucess otherwith .f.
*/

{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   {
      hb_retl( Tcomm_WriteBuffer( (void *) pComm, (BYTE *) hb_parc( 2 ),  ISNUM( 3 ) ? hb_parnl( 3 ) : hb_parclen( 2 ) ));
   }
   else
   {
      hb_retl( 0 ) ;
   }

}


HB_FUNC( INBUFSIZE )
/* parameters
nHandle -> handle obtained with init_port call
return Number of bytes on port to read
*/

{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   {
      hb_retnl( Tcomm_BytesAvailable( (void *) pComm) );
   }
   else
   {
      hb_retnl( -1 );
   }

}

HB_FUNC( OUTBUFSIZE )
{
/* parameters
nHandle -> handle obtained with init_port call
return Number of bytes on port to write
*/
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   {
      hb_retnl( Tcomm_BytesOAvailable( (void *) pComm) );
   }
   else
   {
      hb_retnl( -1 );
   }
}

HB_FUNC( UNINT_PORT ) // Close  the port
{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   {
      Tcomm_CloseCommPort( ( void * )  pComm);
   }
}

HB_FUNC( INIT_PORT2)
/*  cPort

Main initiator/port opener.

   cPort may take either the form 'COMn' or the more general '\\.\COMn'
         The more general form is required for port numbers > 9.

*/

{
   void *Port = Tcomm_New();
   if ( ! Port )
      hb_retptr( (void*) NULL) ;

   Tcomm_SetComPort( Port, ( char * ) hb_parc( 1 ) ) ;

   if (!Tcomm_ConfigDialog( Port ))
   {
      hb_retptr( (void*) NULL) ;
   }

   if (!Tcomm_OpenPort(Port));
   {
      hb_retptr( (void*) NULL) ;
   }

   hb_retptr( (void * ) Port )    ;
}


HB_FUNC( SETCOMOPTIONS)
{

  void * pComm = (void *) hb_parptr( 1 )  ;
  if ( ISPOINTER( 1 ) )
  {
     Tcomm_SetCommOptions( ( void * )  pComm, hb_parni( 2 ) );
  }
}


HB_FUNC( SETHANDSHAKE )
{
   void * pComm = (void *) hb_parptr( 1 )  ;
   if ( ISPOINTER( 1 ) )
   {
      hb_retl(Tcomm_SetHandShake( ( void * )  pComm, hb_parni( 2 ) ));
   }
   else
   {
     hb_retl( 0 ) ;
   }
}

HB_FUNC( GETMODEMSTATUS )
{
  void * pComm = (void *) hb_parptr( 1 )  ;
  DWORD ModemStatus=0 ;

  if ( ISPOINTER( 1 ) )
  {
     hb_retl( Tcomm_GetModemStatus( pComm ,&ModemStatus ) );
     hb_storl( ( ModemStatus & MS_CTS_ON )  != 0, 2 );
     hb_storl( ( ModemStatus & MS_DSR_ON )  != 0, 3 );
     hb_storl( ( ModemStatus & MS_RING_ON ) != 0, 4 );
     hb_storl( ( ModemStatus & MS_RLSD_ON ) != 0, 5 );
  }
  else
  {
     hb_retl(0);
     hb_storl( 0, 2);
     hb_storl( 0, 3);
     hb_storl( 0, 4);
     hb_storl( 0, 5);

  }
}

HB_FUNC( INBUFCLR )
/* parameters
nHandle -> handle obtained with init_port call
return .t. if sucess otherwith .f.
*/
{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   { 
      hb_retl( Tcomm_PurgeCommPort( (void *) pComm ));
   }
   else
   {
      hb_retl( 0 );
   }

}



HB_FUNC(GETNUMBEROFSERIALPORTS)
{
  int iTotalPorts = 0;
  PHB_ITEM pArray= hb_itemArrayNew(0);
  PHB_ITEM temp;

  UINT i;
  //Make sure we clear out any elements which may already be in the array

  //Determine what OS we are running on
  OSVERSIONINFO osvi;
  BOOL bGetVer;
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  bGetVer = GetVersionEx(&osvi);

  //On NT use the QueryDosDevice API
  if (bGetVer && (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT))
  {
    //Use QueryDosDevice to look for all devices of the form COMx. This is a better
    //solution as it means that no ports have to be opened at all.
    TCHAR szDevices[65535];
    DWORD dwChars = QueryDosDevice(NULL, szDevices, 65535);
    if (dwChars)
    {
      int i=0;

      for (;;)
      {
        //Get the current device name
        TCHAR* pszCurrentDevice = &szDevices[i];
        TCHAR* pSzValidPort = &szDevices[i];

        //If it looks like "COMX" then
        //add it to the array which will be returned
        int nLen = _tcslen(pszCurrentDevice);
        if (nLen > 3 && _tcsnicmp(pszCurrentDevice, _T("COM"), 3) == 0)
        {
          //Work out the port number
          int nPort = atoi(&pszCurrentDevice[3]);
          if (nPort >0)
          {
             temp = hb_itemPutCPtr(NULL,pSzValidPort,nLen);
             hb_arrayAddForward( pArray , temp );
             hb_itemRelease( temp );
             iTotalPorts++;
          }
        }

        // Go to next NULL character
        while(szDevices[i] != _T('\0'))
          i++;

        // Bump pointer to the next string
        i++;

        // The list is double-NULL terminated, so if the character is
        // now NULL, we're at the end
        if (szDevices[i] == _T('\0'))
          break;
      }
    }
    else
      iTotalPorts = -1 ;
  }
  else
  {
    //On 95/98 open up each port to determine their existence

    //Up to 255 COM ports are supported so we iterate through all of them seeing
    //if we can open them or if we fail to open them, get an access denied or general error error.
    //Both of these cases indicate that there is a COM port at that number. 
    for ( i=1; i<256; i++)
    {
      //Form the Raw device name
      char sPort[20]={0};
      BOOL bSuccess ;
      HANDLE hPort;

      sprintf(sPort,_T("\\\\.\\COM%d"), i);


      //Try to open the port
      bSuccess = FALSE;
      hPort = CreateFile(sPort, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, 0, 0);
      if (hPort == INVALID_HANDLE_VALUE)
      {
        DWORD dwError = GetLastError();

        //Check to see if the error was because some other app had the port open or a general failure
        if (dwError == ERROR_ACCESS_DENIED || dwError == ERROR_GEN_FAILURE)
          bSuccess = TRUE;
      }
      else
      {
        //The port was opened successfully
        bSuccess = TRUE;

        //Don't forget to close the port, since we are going to do nothing with it anyway
        CloseHandle(hPort);
      }

      //Add the port number to the array which will be returned
      if (bSuccess)
      {
         temp = hb_itemPutC(NULL,sPort);
         hb_arrayAddForward( pArray , temp );
         hb_itemRelease( temp );

         iTotalPorts++;
      }
    }
  }
//return iTotalPorts;
   hb_itemRelease(hb_itemReturn(pArray));
}

HB_FUNC(SETDTR)
{
   void * pComm = (void *) hb_parptr( 1 )  ;

   if ( ISPOINTER( 1 ) )
   { 
      hb_retl( Tcomm_SetDtr( (void *) pComm ,hb_parnl( 2 )));
   }
   else
   {
      hb_retl( 0 );
   }

}
