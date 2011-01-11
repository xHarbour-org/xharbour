/*
 * $Id$
 */

/*
* Comm communication functions  for xBuilder
* Copyright (c) 2004 - Luiz Rafael Culik Guimaraes <culikr@bturbo.com.br>
* All Rights Reserved
*/

#define HBCOMM_HAS_DLL
#define HBCOMM_BUILD_DLL

#include "comm.h"
#include "xhbcomm.h"

extern "C" HBCOMM_API void * Tcomm_New( void )
{
   TCommPort * port = new TCommPort;
   if (! port )
      return NULL;

   return (void  * ) port ;
}

extern "C" HBCOMM_API void Tcomm_SetComPort( void * port, char * szPort)
{
   ( ( TCommPort *) port)->SetCommPort( szPort );
}

extern "C" HBCOMM_API void Tcomm_SetBaudRate( void * port, unsigned int newBaud )

{
   ( ( TCommPort *) port)->SetBaudRate( newBaud );
}

extern "C" HBCOMM_API void Tcomm_SetParity( void * port, BYTE newParity)
{
   ( ( TCommPort *) port)->SetParity( newParity);
}


extern "C" HBCOMM_API void Tcomm_SetByteSize( void * port, BYTE newByte)
{
   ( ( TCommPort *) port)->SetByteSize( newByte);
}

extern "C" HBCOMM_API BOOL Tcomm_SetStopBits( void * port, BYTE newStop)
{
   try
   {
      ( ( TCommPort *) port)->SetStopBits( newStop);
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::BAD_STOP_BITS)
         return FALSE;
   }
   return TRUE;

}

extern "C" HBCOMM_API BOOL Tcomm_WriteString( void * port,const char *outString)
{
   try
   {
      ( ( TCommPort *) port)->WriteString(outString);
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::WRITE_ERROR)
         return FALSE;
   }
   return TRUE;

}

extern "C" HBCOMM_API BOOL Tcomm_WriteBuffer( void * port,BYTE  *buffer, unsigned int ByteCount)
{
   try
   {
      ( ( TCommPort *) port)->WriteBuffer(buffer, ByteCount);
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::WRITE_ERROR)
         return FALSE;
   }
   return TRUE;

}

extern "C" HBCOMM_API void Tcomm_WriteBufferSlowly( void * port,BYTE  *buffer, unsigned int ByteCount)
{
   ( ( TCommPort *) port)->WriteBufferSlowly(buffer,ByteCount);
}

extern "C" HBCOMM_API int Tcomm_ReadString( void * port,char *string, unsigned int MaxBytes)
{
   return ( ( TCommPort *) port)->ReadString(string, MaxBytes);
}

extern "C" HBCOMM_API int Tcomm_ReadBytes( void * port,BYTE *bytes, unsigned int byteCount)
{
   int Res ;
   try
   {
      Res = ( ( TCommPort *) port)->ReadBytes(bytes, byteCount);
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::READ_ERROR)
         return -1;
   }

   return Res;
}

extern "C" HBCOMM_API void Tcomm_DiscardBytes( void * port,unsigned int MaxBytes)
{
   ( ( TCommPort *) port)->DiscardBytes( MaxBytes);
}

extern "C" HBCOMM_API BOOL Tcomm_PurgeCommPort( void * port)
{
   try
   {
      ( ( TCommPort *) port)->PurgeCommPort();
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::PURGECOMM)
         return FALSE;
      if (e.Error == ECommError::PORT_NOT_OPEN)
         return FALSE;

   }
   return TRUE;
}

extern "C" HBCOMM_API BOOL Tcomm_PurgeOCommPort( void * port)
{
   try
   {
      ( ( TCommPort *) port)->PurgeOCommPort();
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::PURGECOMM)
         return FALSE;
      if (e.Error == ECommError::PORT_NOT_OPEN)
         return FALSE;

   }
   return TRUE;

}

extern "C" HBCOMM_API void Tcomm_FlushCommPort(void * port)
{                               
   ( ( TCommPort *) port)->FlushCommPort();
}

extern "C" HBCOMM_API void Tcomm_PutByte(void * port,BYTE value )
{
   ( ( TCommPort *) port)->PutByte( value );
}

extern "C" HBCOMM_API unsigned int Tcomm_BytesAvailable(void * port)
{
   int iRet ;
   try
   {
      iRet = ( ( TCommPort *) port)->BytesAvailable();
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::CLEARCOMMERROR)
         return 0;
   }
   return iRet;

}

extern "C" HBCOMM_API unsigned int Tcomm_BytesOAvailable(void * port)
{
   int iRet ;
   try
   {
   iRet = ( ( TCommPort *) port)->BytesOAvailable();
   }
   catch(ECommError &e)
   {
      if (e.Error == ECommError::CLEARCOMMERROR)
         return 0;
   }
   return iRet;

}

extern "C" HBCOMM_API BOOL Tcomm_GetConnected(void * port)
{
   return  ( ( TCommPort *) port)->GetConnected();
}


extern "C" HBCOMM_API BOOL Tcomm_OpenCommPort(void * port)
{
   try
   {
      ( ( TCommPort *) port)->OpenCommPort();
   }

   catch(ECommError &e)
   {
      if (e.Error == ECommError::OPEN_ERROR)
         return FALSE;
   }
   return TRUE;

}

extern "C" HBCOMM_API void  Tcomm_CloseCommPort(void * port)
{
   ( ( TCommPort *) port)->CloseCommPort();
   delete ( ( TCommPort *) port);

}

extern "C" HBCOMM_API BOOL Tcomm_OpenPort(void * port)
{
   try
   {
      ( ( TCommPort *) port)->OpenPort();
   }

   catch(ECommError &e)
   {
      if (e.Error == ECommError::OPEN_ERROR)
         return FALSE;
   }
   return TRUE;

}

extern "C" HBCOMM_API BOOL Tcomm_ConfigDialog( void * port )
{
   try
   {
      ( ( TCommPort *) port)->ConfigDialog();
   }

   catch(ECommError &e)
   {
         return FALSE;
   }
   return TRUE;

}


extern "C" HBCOMM_API BOOL Tcomm_SetCommOptions(void * Port,int iOption )
{
   try
   {
      ( ( TCommPort *) Port)->SetCommOptions( iOption );
   }

   catch(ECommError &e)
   {
         return FALSE;
   }
   return TRUE;

}


extern "C" HBCOMM_API BOOL Tcomm_SetHandShake(void * port ,int  d)
{

   BOOL bReturn = TRUE;        // EDR*7/03
   try {
        
       ( ( TCommPort *) port)->SetupHandshaking( d );  // EDR*7/03

   }

   catch(ECommError &e)  {
//      if (e.Error == ECommError::BAD_BAUD_RATE)
         bReturn=FALSE;
      }

   return bReturn;
}

extern "C" HBCOMM_API BOOL Tcomm_GetModemStatus(void * port ,DWORD *a) 
{
   BOOL bReturn = TRUE;        // EDR*7/03
   try {
        
       ( ( TCommPort *) port)->GetPortStatus( a );  // EDR*7/03

   }

   catch(ECommError &e)  {
         bReturn=FALSE;
      }

   return bReturn;


}

extern "C" HBCOMM_API BOOL Tcomm_SetDtr( void * port, unsigned int b)
{
   BOOL bReturn = TRUE;        // EDR*7/03
   try {

      ( ( TCommPort *) port)->SetDtr(b);

   }

   catch(ECommError &e)  {
         bReturn=FALSE;
      }

   return bReturn;


}
