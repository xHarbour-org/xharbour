/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    File Remote Server to execute as service
 * parameters:
 *    -i to install
 *    -s to start process
 *    -k to kill process
 *    -u to uninstall
 *
 * Copyright 2009 Miguel Angel Marchuet <soporte-2@dsgsoftware.com>
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
/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_INTERNAL_
//struct _HB_FILE;
//typedef struct _HB_FILE * PHB_FILE;

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE
#endif

#include "hbfilere.h"

#ifdef __POCC__
   #if __POCC__ >= 0600
      const unsigned char sockaddr_size[AF_MAX];
   #endif
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
/*
 * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
 * define and efectively enables lseek64/flock64/ftruncate64 functions
 * on 32bit machines.
 */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
#endif

/*
#define MY_DBG_
#define MY_DBG_2
#define MY_DBG_3
*/

#define MAXUSERS 1000

//static void StartListening( int iServerPort );
void ServiceExecution( void );
int filere_Send( HB_SOCKET_T hSocket, char *szBuffer, int iSend, int timeout );
int hb_ipSend( HB_SOCKET_T hSocket, char *szBuffer, int iSend, int timeout );

#if defined( HB_OS_WIN )
void SvcDebugOut( LPSTR String, void * Status );
#else
#define DWORD ULONG
void SvcDebugOut( char * String, void * Status );
#endif

static PUSERSTRU s_users = NULL;
static USHORT uiUsersMax = 0;         // Higher index of user structure, which was busy
static USHORT uiUsersAlloc = 0;       // Number of allocated user structures
static USHORT uiUsers = 0;            // Number of users connected

static BYTE * szOk    = ( BYTE * ) "+1";
static BYTE * szFalse = ( BYTE * ) "+0";

static char pFilename[HB_PATH_MAX];
static char pDefExt[HB_PATH_MAX];
static char pPaths[HB_PATH_MAX];

#define HB_LENGTH_ACK         4
static char szDataACK[HB_LENGTH_ACK];

static HB_SOCKET_T  hSocketMain;       // Initial server socket
#if defined( HB_OS_WIN )
static HANDLE       hProcessHeap = 0;
#if defined( LPFN_TRANSMITPACKETS )
static LPFN_TRANSMITPACKETS lpfnTransmitPackets;
#endif
#endif
int                 iServerPort = 2813;

#define		MAX_NUM_OF_PROCESS		1
#if defined( HB_OS_WIN )
/** Window Service **/
VOID ServiceMainProc();
VOID Install( char* pPath, char* pName, char* pDescription );
VOID UnInstall( char* pName );
BOOL KillService( char* pName );
BOOL RunService( char* pName );
void ExecuteSubProcess( void );

VOID WINAPI ServiceMain( DWORD dwArgc, LPTSTR *lpszArgv );
VOID WINAPI ServiceHandler(DWORD fdwControl);

char pServiceName[501];
char pExeFile[501];
char lpCmdLineData[501];

CRITICAL_SECTION		myCS;
SERVICE_TABLE_ENTRY	lpServiceStartTable[] =
{
	{ pServiceName, ServiceMain },
	{ NULL, NULL }
};

SERVICE_STATUS_HANDLE   hServiceStatusHandle;
SERVICE_STATUS          ServiceStatus;

#else
/** Linux daemon **/
void signal_handler( int sig );

static int              ServiceStatus;
#define SERVICE_STOPPED 1
#define SERVICE_PAUSED  2
#define SERVICE_RUNNING 3
#endif

void main( int argc, char * argv[] )
{
#if defined( HB_OS_WIN )
   char pModuleFile[501];
   PHB_FNAME pFilepath;
   DWORD dwSize = GetModuleFileName( NULL, pModuleFile, 500 );

   hProcessHeap = GetProcessHeap();

	/* initialize variables for .exe file name */
	pModuleFile[dwSize] = 0;
	if(dwSize>4 && pModuleFile[dwSize-4] == '.')
	{
		sprintf( pExeFile, "%s", pModuleFile );
		pModuleFile[dwSize-4] = 0;
	}

   pFilepath = hb_fsFNameSplit( pModuleFile );
   hb_fsChDir( pFilepath->szPath );

	if( argc >= 2 )
		strcpy( lpCmdLineData, argv[1] );

	strcpy( pServiceName, "File_Server" );

	InitializeCriticalSection( &myCS );

	if( strcmp( "-i", lpCmdLineData ) == 0 || strcmp( "-I", lpCmdLineData ) == 0 )
   {
		Install( pExeFile, pServiceName, "Senior File Server\0" );
      RunService( pServiceName );
   }
	else if( strcmp( "-k", lpCmdLineData ) == 0 || strcmp("-K", lpCmdLineData ) == 0 )
		KillService( pServiceName );
	else if( strcmp( "-u", lpCmdLineData ) == 0 || strcmp("-U", lpCmdLineData ) == 0 )
   {
		KillService( pServiceName );
		UnInstall( pServiceName );
   }
	else if( strcmp( "-s", lpCmdLineData ) == 0 || strcmp("-S", lpCmdLineData ) == 0 )
		RunService( pServiceName );
	else
		ExecuteSubProcess();
#else
   /* Our process ID and Session ID */
   pid_t pid, sid;

   // Setup signal handling before we start
   signal( SIGHUP, signal_handler );
   signal( SIGTERM, signal_handler );
   signal( SIGINT, signal_handler );
   signal( SIGQUIT, signal_handler );

   /* Fork off the parent process */
   pid = fork();
   if (pid < 0) {
      exit(EXIT_FAILURE);
   }
   /* If we got a good PID, then
      we can exit the parent process. */
   if (pid > 0) {
      exit(EXIT_SUCCESS);
   }

   /* Change the file mode mask */
   umask(0);

   /* Create a new SID for the child process */
   sid = setsid();
   if (sid < 0) {
      /* Log the failure */
      exit(EXIT_FAILURE);
   }

   ServiceStatus = SERVICE_RUNNING;
   ServiceExecution();

   exit(0);

#endif
}

#if defined( HB_OS_WIN )
int filere_Send( HB_SOCKET_T hSocket, char *szBuffer, int iSend, int timeout )
{
#if defined( LPFN_TRANSMITPACKETS )
   if( lpfnTransmitPackets )
   {
      TRANSMIT_PACKETS_ELEMENT lpPacketArray;
      lpPacketArray.dwElFlags = TP_ELEMENT_MEMORY;
      lpPacketArray.cLength = iSend;
      lpPacketArray.pBuffer = szBuffer;
      if( lpfnTransmitPackets( hSocket, &lpPacketArray, 1, iSend, NULL, TF_USE_DEFAULT_WORKER ) )
         return iSend;
      else
         return -1;
   }
   else
#endif
      return hb_ipSend( hSocket, szBuffer, iSend, timeout );
}

VOID Install( char* pPath, char* pName, char* pDescription )
{
	SC_HANDLE schSCManager = OpenSCManager( NULL, NULL, SC_MANAGER_CREATE_SERVICE );
	if( schSCManager == 0 )
	{
      SvcDebugOut( "OpenSCManager failed, error code = %d\n", ( void * ) GetLastError() );
	}
	else
	{
		SC_HANDLE schService = CreateService
		(
			schSCManager,	/* SCManager database      */
			pName,			/* name of service         */
			pDescription,  /* service name to display */
			SERVICE_ALL_ACCESS,        /* desired access          */
			SERVICE_WIN32_OWN_PROCESS|SERVICE_INTERACTIVE_PROCESS , /* service type            */
			SERVICE_AUTO_START,        /* start type              */
			SERVICE_ERROR_NORMAL,      /* error control type      */
			pPath,			            /* service's binary        */
			NULL,                      /* no load ordering group  */
			NULL,                      /* no tag identifier       */
			NULL,                      /* no dependencies         */
			NULL,                      /* LocalSystem account     */
			NULL
		);                            /* no password             */
		if( schService == 0 )
		{
			long nError = GetLastError();
			char pTemp[121];
			sprintf( pTemp, "Failed to create service %s, error code = %d\n", pName, nError );
			OutputDebugString( pTemp );
		}
		else
		{
			SvcDebugOut( "Service %s installed\n", pName );
			CloseServiceHandle( schService );
		}
		CloseServiceHandle( schSCManager );
	}
}

VOID UnInstall( char* pName )
{
	SC_HANDLE schSCManager = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );
	if( schSCManager == 0 )
	{
		SvcDebugOut( "OpenSCManager failed, error code = %d\n", ( void * ) GetLastError() );
	}
	else
	{
		SC_HANDLE schService = OpenService( schSCManager, pName, SERVICE_ALL_ACCESS);
		if( schService == 0 )
		{
			SvcDebugOut( "OpenService failed, error code = %d\n", ( void * ) GetLastError() );
		}
		else
		{
			if( ! DeleteService( schService ) )
				SvcDebugOut( "Failed to delete service %s\n", pName );
			else
				SvcDebugOut( "Service %s removed\n", pName );

			CloseServiceHandle( schService );
		}
		CloseServiceHandle( schSCManager );
	}
}

BOOL KillService( char* pName )
{
	/* kill service with given name */
	SC_HANDLE schSCManager = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );
	if( schSCManager == 0 )
	{
		SvcDebugOut( "OpenSCManager failed, error code = %d\n", ( void * ) GetLastError() );
	}
	else
	{
		/* open the service */
		SC_HANDLE schService = OpenService( schSCManager, pName, SERVICE_ALL_ACCESS );
		if( schService == 0 )
		{
			SvcDebugOut( "OpenService failed, error code = %d\n", ( void * ) GetLastError() );
		}
		else
		{
			/* call ControlService to kill the given service */
			SERVICE_STATUS status;
			if( ControlService( schService, SERVICE_CONTROL_STOP, &status ) )
			{
				CloseServiceHandle( schService );
				CloseServiceHandle( schSCManager );
				return TRUE;
			}
			else
			{
				SvcDebugOut( "ControlService failed, error code = %d\n", ( void * ) GetLastError() );
			}
			CloseServiceHandle( schService );
		}
		CloseServiceHandle( schSCManager );
	}
	return FALSE;
}

BOOL RunService( char* pName )
{
	/* run service with given name */
	SC_HANDLE schSCManager = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );
	if( schSCManager == 0 )
	{
		SvcDebugOut( "OpenSCManager failed, error code = %d\n", ( void * ) GetLastError() );
	}
	else
	{
		/* open the service */
		SC_HANDLE schService = OpenService( schSCManager, pName, SERVICE_ALL_ACCESS );
		if( schService == 0 )
		{
			SvcDebugOut( "OpenService failed, error code = %d\n", ( void * ) GetLastError() );
		}
		else
		{
			/* call StartService to run the service */
			if( StartService( schService, 0, (const char**) NULL ) )
			{
				CloseServiceHandle( schService );
				CloseServiceHandle( schSCManager );
				return TRUE;
			}
			else
			{
				SvcDebugOut( "StartService failed, error code = %d\n", ( void * ) GetLastError() );
			}
			CloseServiceHandle(schService);
		}
		CloseServiceHandle(schSCManager);
	}
	return FALSE;
}


void ExecuteSubProcess( void )
{
	if( ! StartServiceCtrlDispatcher( lpServiceStartTable ) )
	{
		SvcDebugOut( "StartServiceCtrlDispatcher failed, error code = %d\n", ( void * ) GetLastError() );
	}
	DeleteCriticalSection( &myCS );
}

VOID WINAPI ServiceMain( DWORD dwArgc, LPTSTR *lpszArgv )
{
   HB_SYMBOL_UNUSED( dwArgc );
   HB_SYMBOL_UNUSED( lpszArgv );

   ServiceStatus.dwServiceType        = SERVICE_WIN32;
   ServiceStatus.dwCurrentState       = SERVICE_START_PENDING;
   ServiceStatus.dwControlsAccepted   = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN | SERVICE_ACCEPT_PAUSE_CONTINUE;
   ServiceStatus.dwWin32ExitCode      = 0;
   ServiceStatus.dwServiceSpecificExitCode = 0;
   ServiceStatus.dwCheckPoint         = 0;
   ServiceStatus.dwWaitHint           = 0;

   hServiceStatusHandle = RegisterServiceCtrlHandler(pServiceName, ServiceHandler);
   if (hServiceStatusHandle == 0)
   {
		SvcDebugOut( "RegisterServiceCtrlHandler failed, error code = %d\n", ( void * ) GetLastError() );
      return;
   }

   /* Initialization complete - report running status */
   ServiceStatus.dwCurrentState = SERVICE_RUNNING;
   ServiceStatus.dwCheckPoint   = 0;
   ServiceStatus.dwWaitHint     = 0;
   if( ! SetServiceStatus( hServiceStatusHandle, &ServiceStatus ) )
   {
		SvcDebugOut( "SetServiceStatus failed, error code = %d\n", ( void * ) GetLastError() );
   }
   ServiceExecution();
}

VOID WINAPI ServiceHandler( DWORD fdwControl )
{
	switch(fdwControl)
	{
		case SERVICE_CONTROL_STOP:
		case SERVICE_CONTROL_SHUTDOWN:
			ServiceStatus.dwWin32ExitCode = 0;
			ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
			ServiceStatus.dwCheckPoint    = 0;
			ServiceStatus.dwWaitHint      = 0;
			break;
		case SERVICE_CONTROL_PAUSE:
			ServiceStatus.dwCurrentState = SERVICE_PAUSED;
			break;
		case SERVICE_CONTROL_CONTINUE:
			ServiceStatus.dwCurrentState = SERVICE_RUNNING;
			break;
		case SERVICE_CONTROL_INTERROGATE:
			break;
		default:
			if( fdwControl >= 128 && fdwControl < 256)
			{
				int nIndex = fdwControl & 0x7F;
				/* bounce a single process */
				if( nIndex >= 0 && nIndex < MAX_NUM_OF_PROCESS )
               ServiceExecution();

				/* bounce all processes */
				else if( nIndex == 127 )
               ServiceExecution();
			}
			else
			{
				SvcDebugOut( "Unrecognized opcode %d\n", ( void * ) fdwControl );
			}
	};
   if( ! SetServiceStatus( hServiceStatusHandle,  &ServiceStatus ) )
	{
		SvcDebugOut( "SetServiceStatus failed, error code = %d\n", ( void * ) GetLastError() );
   }
}

/* Memory functions */

static void * fl_alloc( ULONG ulSize )
{
   return ( void * ) HeapAlloc( hProcessHeap, 0, ulSize );
}

static void * fl_realloc( void * pHeapMem, ULONG ulSize )
{
   if( pHeapMem )
      pHeapMem = ( void * ) HeapReAlloc( hProcessHeap, 0, pHeapMem, ulSize );
   else
      pHeapMem = ( void * ) HeapAlloc( hProcessHeap, 0, ulSize );
   return pHeapMem;
}

static void fl_free( void * pHeapMem )
{
   HeapFree( hProcessHeap, 0, pHeapMem );
}
#else
int filere_Send( HB_SOCKET_T hSocket, char *szBuffer, int iSend, int timeout )
{
   return hb_ipSend(  hSocket, szBuffer,  iSend,  timeout );
}      

void signal_handler(int sig) {

    switch(sig) {
        case SIGHUP:
        case SIGINT:
        case SIGTERM:
        case SIGQUIT:
			   ServiceStatus = SERVICE_STOPPED;
            break;
    }
}

/* Memory functions */

static void * fl_alloc( ULONG ulSize )
{
   return ( void * ) malloc( ulSize );
}

static void * fl_realloc( void * pHeapMem, ULONG ulSize )
{
   if( pHeapMem )
      pHeapMem = ( void * ) realloc( pHeapMem, ulSize );
   else
      pHeapMem = ( void * ) malloc( ulSize );
   return pHeapMem;
}

static void fl_free( void * pHeapMem )
{
   free( pHeapMem );
}
#endif
/* File buffer functions */

static PHB_FILE hb_fileFind( PUSERSTRU pUStru, char * pFileName )
{
   if( pUStru->s_openFiles && pFileName )
   {
      PHB_FILE pFile = pUStru->s_openFiles;
      do
      {
         if( strcmp( pFile->pFileName, pFileName ) == 0 )
            return pFile;
         pFile = pFile->pNext;
      }
      while( pUStru->s_openFiles != pFile );
   }
   return NULL;
}

static PHB_FILE hb_fileFindByHandle( PUSERSTRU pUStru, HB_FHANDLE hFile )
{
   if( pUStru->s_openFiles && hFile )
   {
      PHB_FILE pFile = pUStru->s_openFiles;
      do
      {
         if( pFile->hFile == hFile )
            return pFile;
         pFile = pFile->pNext;
      }
      while( pUStru->s_openFiles != pFile );
   }
   return NULL;
}

static PHB_FILE hb_fileNew( PUSERSTRU pUStru, HB_FHANDLE hFile, BOOL fShared, char * pFileName )
{
   PHB_FILE pFile = hb_fileFind( pUStru, pFileName );

   if( !pFile )
   {
      pFile = ( PHB_FILE ) fl_alloc( sizeof( HB_FILE ) );
      memset( pFile, 0, sizeof( HB_FILE ) );
      pFile->pFileName = ( char * ) fl_alloc( HB_PATH_MAX );
      memcpy( pFile->pFileName, pFileName, strlen( pFileName ) );
      pFile->hFile     = hFile;
      pFile->shared    = fShared;
      pFile->hMap      = NULL;
      pFile->pView     = NULL;

      if( pUStru->s_openFiles )
      {
         pFile->pNext = pUStru->s_openFiles;
         pFile->pPrev = pUStru->s_openFiles->pPrev;
         pFile->pPrev->pNext = pFile;
         pUStru->s_openFiles->pPrev = pFile;
      }
      else
         pUStru->s_openFiles = pFile->pNext = pFile->pPrev = pFile;
   }
   pFile->used++;

   return pFile;
}

/* filere api */

static void filere_SendAnswer( PUSERSTRU pUStru, BYTE* szData, ULONG ulLen )
{
   HB_PUT_BE_UINT32( szDataACK, ulLen );
#if defined(MY_DBG_)
   OutputDebugString( ( char * ) szDataACK );
   OutputDebugString( ( char * ) szData );
#endif
   filere_Send( pUStru->hSocket, szDataACK, HB_LENGTH_ACK, -1 );
   if( filere_Send( pUStru->hSocket, ( char * ) szData, ulLen, -1 ) != ( int ) ulLen )
      SvcDebugOut( "Data send failed, error code = %s\n", ( void * ) hb_ipErrorDesc() );
}

static void filere_SendSingleAnswer( PUSERSTRU pUStru, BYTE* szData, ULONG ulLen )
{
#if defined(MY_DBG_)
   OutputDebugString( ( char * ) szData + HB_LENGTH_ACK );
#endif
   filere_Send( pUStru->hSocket, ( char * ) szData, ulLen, -1 );
}

static void filere_SendOkAnswer( PUSERSTRU pUStru )
{
   filere_Send( pUStru->hSocket, ( char * ) szOk, 2, -1 );
}

static void filere_SendFalseAnswer( PUSERSTRU pUStru )
{
   filere_Send( pUStru->hSocket, ( char * ) szFalse, 2, -1 );
}

static BYTE * hb_strToken( BYTE * szText, ULONG ulText, ULONG ulIndex, ULONG * pulLen )
{
   ULONG ulStart;
   ULONG ulEnd = 0;
   ULONG ulCounter = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_strToken(%s, %lu, %lu, %p)", szText, ulText, ulIndex, pulLen));

   do
   {
      ulStart = ulEnd;

      if( szText[ ulStart ] == '|' )
         ulStart++;
      if( ulStart < ulText && szText[ ulStart ] != '|' )
      {
         ulEnd = ulStart + 1;

         while( ulEnd < ulText && szText[ ulEnd ] != '|' )
            ulEnd++;
      }
      else
         ulEnd = ulStart;

   }
   while( ulCounter++ < ulIndex - 1 && ulEnd < ulText );

   if( ulCounter < ulIndex )
   {
      *pulLen = 0;
      return ( BYTE * ) "";
   }
   else
   {
      *pulLen = ulEnd - ulStart;
      return szText + ulStart;
   }
}

static void filere_ExtOpen( PUSERSTRU pUStru, BYTE * szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   USHORT uiExFlags;
   ULONG ulSize;
   PHB_FILE pFile = NULL;
   BOOL fShared;
   BOOL fResult;
#if defined( HB_USE_LARGEFILE64 )
   struct stat64 statbuf;
#else	      
   struct stat statbuf;
#endif

   // Reading params
   ptr = hb_strToken( szData, strlen( ( char * ) szData ), 4, &ulSize );
   if( ulSize )
   {
      memcpy( pPaths, ptr, ulSize );
      pPaths[ulSize] = '\0';
   }

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%hu", &uiExFlags );
   else
      uiExFlags = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
   {
      memcpy( pDefExt, ptr, ulSize );
      pDefExt[ulSize] = '\0';
   }

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
   }

   fShared = ( uiExFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0;

   // Clear error
   hb_fsSetError( 0 );
#  if defined( HB_USE_LARGEFILE64 )
   fResult = stat64( ( char * ) pFilename, &statbuf ) == 0;
#else   
   fResult = stat( ( char * ) pFilename, &statbuf ) == 0;
#endif   
   hb_fsSetIOError( fResult, 0 );

   if( fResult )
   {
      pFile = hb_fileFind( pUStru, pFilename );
      if( pFile )
      {
         if( !fShared || ! pFile->shared || ( uiExFlags & FXO_TRUNCATE ) != 0 )
            fResult = FALSE;
         else
            pFile->used++;
      }
   }

   if( pFile )
   {
      if( !fResult )
      {
         hb_fsSetError( ( uiExFlags & FXO_TRUNCATE ) ? 5 : 32 );
         hFileHandle = -1;
      }
      else
         hFileHandle = pFile->hFile;
   }
   else
   {
      /* hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError ) */
      hFileHandle = hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, NULL );
      if( hFileHandle != FS_ERROR )
      {
#if 0
         pFile = hb_fileNew( pUStru, hFileHandle, fShared, pFilename );
#else
         hb_fileNew( pUStru, hFileHandle, fShared, pFilename );
#endif
      }
   }

#if 0
   if( pFile && ! pFile->hMap )
   {
      pFile->hMap = CreateFileMapping( (HANDLE) hFileHandle, NULL, PAGE_READWRITE, 0, 0, NULL );
      pFile->pView = ( BYTE * ) MapViewOfFile( pFile->hMap, FILE_MAP_ALL_ACCESS, 0, 0, 0 );
   }
#endif

   if( pUStru->ulBufAnswerLen < 35 + HB_LENGTH_ACK )
   {
      pUStru->ulBufAnswerLen = 35 + HB_LENGTH_ACK;
      pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
   }
   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%p|%hu|\r\n", ( void * ) hFileHandle, hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_Close( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize;
   HB_FHANDLE hFileHandle;

   /* Reading params */
   ptr = ( BYTE * ) hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", ( void ** )  &hFileHandle );
   else
      hFileHandle = 0;

   if( hFileHandle )
   {
      PHB_FILE pFile = hb_fileFindByHandle( pUStru, hFileHandle );
      if( pFile )
      {
         if( --pFile->used == 0 )
         {
            if( pFile->pNext )
            {
               pFile->pPrev->pNext = pFile->pNext;
               pFile->pNext->pPrev = pFile->pPrev;
               if( pFile == pUStru->s_openFiles )
               {
                  pUStru->s_openFiles = pFile->pNext;
                  if( pFile == pUStru->s_openFiles )
                     pUStru->s_openFiles = NULL;
               }
            }
            hb_fsClose( hFileHandle );
            if( pFile->pFileName )
               fl_free( pFile->pFileName );
#if 0
            if( pFile->pView )
               UnmapViewOfFile( pFile->pView );
            if( pFile->hMap )
               CloseHandle( pFile->hMap );
#endif
            fl_free( pFile );
         }
      }
      filere_SendOkAnswer( pUStru );
   }
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_LockLarge( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   HB_FOFFSET ulStart, ulLen;
   USHORT iType;
   ULONG ulSize;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%" PFHL "i", &ulStart );
   else
      ulStart = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%" PFHL "i", &ulLen );
   else
      ulLen = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 4, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%hu", &iType );
   else
      iType = 0;

   if( hb_fsLockLarge( hFileHandle, ulStart, ulLen, iType ) )
      filere_SendOkAnswer( pUStru );
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_ReadAt( PUSERSTRU pUStru, BYTE* szData )
{
   ULONG ulCount, ulRead;
   HB_FHANDLE hFileHandle;
   HB_FOFFSET llOffset;
   PHB_FILE pFile;

   // Reading params
   sscanf( ( char * ) szData, "%p|%lu|%" PFHL "i|\r\n", ( void ** ) &hFileHandle, &ulCount, &llOffset );

   if( ulCount )
   {
      // hb_fsReadAt( pFile->hFile, pBuffer, ulSize, llOffset )
      if( pUStru->ulBufAnswerLen < ulCount + 9 )
      {
         pUStru->ulBufAnswerLen = ulCount + 9;
         pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
      }
      pFile = hb_fileFindByHandle( pUStru, hFileHandle );
      if( pFile && pFile->hMap )
      {
         ulRead = ulCount;
         memcpy( pUStru->pBufAnswer + 8, pFile->pView + ( long ) llOffset, ulCount );
      }
      else
         ulRead = hb_fsReadAt( hFileHandle, pUStru->pBufAnswer + 8, ulCount, llOffset );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, 4 + ulRead );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer + 4, hb_fsError() );
      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulRead + 8 );
   }
   else
   {
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, 4 );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer + 4, 0 );
      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, 8 );
   }
}

static void filere_ReadLarge( PUSERSTRU pUStru, BYTE* szData )
{
   ULONG ulCount, ulRead;
   HB_FHANDLE hFileHandle;

   /* Reading params */
   sscanf( ( char * ) szData, "%p|%lu|\r\n", ( void ** ) &hFileHandle, &ulCount );

   /* Clear error */
   hb_fsSetError( 0 );

   if( ulCount )
   {
      if( pUStru->ulBufAnswerLen < ulCount + 9 )
      {
         pUStru->ulBufAnswerLen = ulCount + 9;
         pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
      }
      ulRead = hb_fsReadLarge( hFileHandle, pUStru->pBufAnswer + 8, ulCount );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, 4 + ulRead );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer + 4, hb_fsError() );

      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulRead + 8 );
   }
   else
   {
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, 4 );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer + 4, 0 );
      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, 8 );
   }
}

static void filere_WriteAt( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   HB_FOFFSET llOffset;
   ULONG ulWriten, ulWrite, ulSize;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu", &ulWrite );
   else
      ulWrite = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%" PFHL "i", &llOffset );
   else
      llOffset = 0;

   /* Clear error */
   hb_fsSetError( 0 );

   if( ulWrite )
   {
      ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 4, &ulSize );
      ulWriten = hb_fsWriteAt( hFileHandle, ptr, ulWrite, llOffset );
   }
   else
      ulWriten = 0;

   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|%hu|\r\n", ulWriten, hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_WriteLarge( PUSERSTRU pUStru, BYTE * szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   ULONG ulWritten, ulCount, ulSize;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p|", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu|", &ulCount );
   else
      ulCount = 0;

   /* Clear error */
   hb_fsSetError( 0 );

   if( ulCount )
   {
      ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
      ulWritten = hb_fsWriteLarge( hFileHandle, ptr, ulCount );
   }
   else
      ulWritten = 0;

   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|%hu|\r\n", ulWritten, hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_Write( PUSERSTRU pUStru, BYTE * szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   ULONG ulSize;
   USHORT uiCount, uiWritten;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p|", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%hu|", &uiCount );
   else
      uiCount = 0;

   /* Clear error */
   hb_fsSetError( 0 );

   if( uiCount )
   {
      ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
      uiWritten = hb_fsWrite( hFileHandle, ptr, uiCount );
   }
   else
      uiWritten = 0;

   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|%hu|\r\n", uiWritten, hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_TruncAt( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   HB_FOFFSET llOffset;
   ULONG ulSize;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%" PFHL "i", &llOffset );
   else
      llOffset = 0;

   /* hb_fsTruncAt( pFile->hFile, llOffset ) */
   if( hb_fsTruncAt( hFileHandle, llOffset ) )
      filere_SendOkAnswer( pUStru );
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_SeekLarge( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   HB_FOFFSET llOffset,llRet;
   USHORT uiFlags;
   ULONG ulSize;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%" PFHL "i", &llOffset );
   else
      llOffset = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu", &uiFlags );
   else
      uiFlags = 0;

   /* Clear error */
   hb_fsSetError( 0 );

   /* hb_fsSeekLarge( pFile->hFile, HB_FOFFSET llOffset, USHORT uiFlags ) */
   llRet = hb_fsSeekLarge( hFileHandle, llOffset, uiFlags );

   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%" PFHL "i|%hu|\r\n", llRet, hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_Commit( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   ULONG ulLen;

   /* Reading params */
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulLen );
   if( ulLen )
      sscanf( ( char * ) ptr, "%p|", ( void ** ) &hFileHandle );
   else
      hFileHandle = 0;

   /* Clear error */
   hb_fsSetError( 0 );

   /* hb_fsCommit( pFile->hFile ) */
   hb_fsCommit( hFileHandle );

   ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|\r\n", hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulLen );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulLen + HB_LENGTH_ACK );
}

static void filere_Delete( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
      if( hb_fsDelete( pFilename ) )
         filere_SendOkAnswer( pUStru );
      else
         filere_SendFalseAnswer( pUStru );
   }
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_Rename( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   char * pFileOld;
   char * pFileNew;
   ULONG ulSize;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      pFileOld = ( char * ) fl_alloc( ( ulSize + 1 ) * sizeof( char ) );
      memcpy( pFileOld, ptr, ulSize );
      pFileOld[ulSize] = '\0';

      ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
      pFileNew = ( char * ) fl_alloc( ( ulSize + 1 ) * sizeof( char ) );
      memcpy( pFileNew, ptr, ulSize );
      pFileNew[ulSize] = '\0';

      if( hb_fsRename( pFileOld, pFileNew ) )
         filere_SendOkAnswer( pUStru );
      else
         filere_SendFalseAnswer( pUStru );

      fl_free( pFileOld );
      fl_free( pFileNew );
   }
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_MkDir( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize, ulLen;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';

      /* Clear error */
      hb_fsSetError( 0 );

      if( hb_fsMkDir( pFilename ) )
         ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|1|\r\n", hb_fsError() );
      else
         ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|0|\r\n", hb_fsError() );
   }
   else
      ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|0|\r\n", hb_fsError() );

   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulLen );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulLen + HB_LENGTH_ACK );
}

static void filere_RmDir( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize, ulLen;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';

      /* Clear error */
      hb_fsSetError( 0 );

      if( hb_fsRmDir( pFilename ) )
         ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|1|\r\n", hb_fsError() );
      else
         ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|0|\r\n", hb_fsError() );
   }
   else
      ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|0|\r\n", hb_fsError() );

   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulLen );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulLen + HB_LENGTH_ACK );
}

static void filere_GetFileAttributes( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      DWORD ulAttributes;
      ULONG ulLen;

      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';

      ulAttributes = hb_fsGetFileAttributes( ( char * ) pFilename );
      ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|\r\n", ulAttributes );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulLen );
      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulLen + HB_LENGTH_ACK );
   }
   else
   {
      ULONG ulLen = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|\r\n", 0 );
      HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulLen );
      filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulLen + HB_LENGTH_ACK );
   }
}

static void filere_Exists( PUSERSTRU pUStru, BYTE* szData )
{
   BYTE * ptr;
   ULONG ulSize;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
      if( hb_fsFileExists( ( const char * ) pFilename ) )
         filere_SendOkAnswer( pUStru );
      else
         filere_SendFalseAnswer( pUStru );
   }
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_File( PUSERSTRU pUStru, BYTE * szData )
{
   BYTE * ptr;
   ULONG ulSize;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
      if( hb_fsFile( pFilename ) )
         filere_SendOkAnswer( pUStru );
      else
         filere_SendFalseAnswer( pUStru );
   }
   else
      filere_SendFalseAnswer( pUStru );
}

static void filere_CreateTemp( PUSERSTRU pUStru, BYTE* szData  )
{
   BYTE * ptr;
   HB_FHANDLE hFileHandle;
   ULONG ulSize;
   ULONG ulAttr;

   /* Reading params */
   ptr = hb_strToken( szData, strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pPaths, ptr, ulSize );
      pPaths[ulSize] = '\0';
   }

   ptr = hb_strToken( szData, strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
   {
      memcpy( pDefExt, ptr, ulSize );
      pDefExt[ulSize] = '\0';
   }

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 3, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu", &ulAttr );
   else
      ulAttr = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 4, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
   }
   else
      pFilename[0] = '\0';

   /* Clear error */
   hb_fsSetError( 0 );
   if( pUStru->ulBufAnswerLen < HB_PATH_MAX + 30 + HB_LENGTH_ACK )
   {
      pUStru->ulBufAnswerLen = HB_PATH_MAX + 30 + HB_LENGTH_ACK;
      pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
   }

   hFileHandle = hb_fsCreateTemp( pPaths, pDefExt, ulAttr, pFilename );
   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%p|%hu|%s|\r\n", hFileHandle, hb_fsError(), pFilename );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

static void filere_CurDirBuffEx( PUSERSTRU pUStru, BYTE* szData )
{
   ULONG ulSize, ulLen;
   USHORT uiDrive, uiRet;
   BYTE * ptr;
   static char pbyDirBuffer[ HB_PATH_MAX ];

   // Reading params
   ptr = hb_strToken( szData, strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%hu", &uiDrive );
   else
      uiDrive = 0;

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu", &ulLen );
   else
      ulLen = 0;

   uiRet = hb_fsCurDirBuffEx( uiDrive, pbyDirBuffer, ulLen );
   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%s|%hu|\r\n", pbyDirBuffer, uiRet );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

/* hb_fsFindFirst( const char * pszFileName, ULONG ulAttr ); */
static void filere_FindFirst( PUSERSTRU pUStru, BYTE* szData )
{
   ULONG ulAttr;
   PHB_FFIND pffind;
   ULONG ulSize;
   BYTE * ptr;

   // Reading params
   ptr = hb_strToken( szData, strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      memcpy( pFilename, ptr, ulSize );
      pFilename[ulSize] = '\0';
   }

   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 2, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%lu", &ulAttr );
   else
      ulAttr = 0;

   if( pUStru->ulBufAnswerLen < HB_PATH_MAX + 200 )
   {
      pUStru->ulBufAnswerLen = HB_PATH_MAX + 200;
      pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
   }

   pffind = hb_fsFindFirst( ( char * ) pFilename, ulAttr );
   if( pffind )
      ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%p|%lu|%" PFHL "i|%lu|%8s|%hu|%s|\r\n", pffind, pffind->attr,
                        pffind->size, pffind->lDate, pffind->szTime, hb_fsError(), pffind->szName );
   else
      ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%p|%hu|\r\n", pffind, hb_fsError() );

   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

/* hb_fsFindNext( PHB_FFIND ffind ); */
static void filere_FindNext( PUSERSTRU pUStru, BYTE* szData  )
{
   PHB_FFIND pffind;
   ULONG ulSize;
   BYTE * ptr;

   // Reading params
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
      sscanf( ( char * ) ptr, "%p", &pffind );
   else
      pffind = 0;

   if( pUStru->ulBufAnswerLen < HB_PATH_MAX + 200 + HB_LENGTH_ACK )
   {
      pUStru->ulBufAnswerLen = HB_PATH_MAX + 200 + HB_LENGTH_ACK;
      pUStru->pBufAnswer = ( BYTE * ) fl_realloc( pUStru->pBufAnswer, pUStru->ulBufAnswerLen );
   }

   if( hb_fsFindNext( pffind ) )
      ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|%" PFHL "i|%lu|%8s|1|%hu|%s|\r\n", pffind->attr,
                        pffind->size, pffind->lDate, pffind->szTime, hb_fsError(), pffind->szName );
   else
      ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%lu|%" PFHL "i|%lu|%8s|0|%hu|%s|\r\n", pffind->attr,
                        pffind->size, pffind->lDate, pffind->szTime, hb_fsError(), pffind->szName );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

/* hb_fsFindClose( PHB_FFIND ffind ); */
static void filere_FindClose( PUSERSTRU pUStru, BYTE* szData  )
{
   PHB_FFIND pffind;
   ULONG ulSize;
   BYTE * ptr;

   // Reading params
   ptr = hb_strToken( szData, ( ULONG ) strlen( ( char * ) szData ), 1, &ulSize );
   if( ulSize )
   {
      sscanf( ( char * ) ptr, "%p", &pffind );
      if( pffind )
         hb_fsFindClose( pffind );
   }

   ulSize = sprintf( ( char * ) pUStru->pBufAnswer + HB_LENGTH_ACK, "+%hu|\r\n", hb_fsError() );
   HB_PUT_BE_UINT32( pUStru->pBufAnswer, ulSize );
   filere_SendSingleAnswer( pUStru, pUStru->pBufAnswer, ulSize + HB_LENGTH_ACK );
}

void filere_DelUser( PUSERSTRU pUStru )
{
   filere_SendOkAnswer( pUStru );

   printf( "LogOut user with IP %s at socket %p\r\n", pUStru->szAddr, pUStru->hSocket );

   hb_ip_rfd_clr( pUStru->hSocket );
   hb_ipclose( pUStru->hSocket );
   pUStru->hSocket = 0;

   if( pUStru->pBuffer )
   {
      fl_free( pUStru->pBuffer );
      pUStru->pBuffer = NULL;
   }

   if( pUStru->pBufAnswer )
   {
      fl_free( pUStru->pBufAnswer );
      pUStru->pBufAnswer = NULL;
      pUStru->ulBufAnswerLen = 0;
   }

   if( pUStru->pBufTemp )
   {
      fl_free( pUStru->pBufTemp );
      pUStru->pBufTemp = NULL;
      pUStru->ulBufTempLen = 0;
   }

   if( pUStru->szAddr )
   {
      fl_free( pUStru->szAddr );
      pUStru->szAddr = NULL;
   }

   // Close pendding opened files
   while( pUStru->s_openFiles )
   {
      PHB_FILE pFile = pUStru->s_openFiles;
      if( pFile->pNext )
      {
         pFile->pPrev->pNext = pFile->pNext;
         pFile->pNext->pPrev = pFile->pPrev;
         if( pFile == pUStru->s_openFiles )
         {
            pUStru->s_openFiles = pFile->pNext;
            if( pFile == pUStru->s_openFiles )
               pUStru->s_openFiles = NULL;
         }
      }
      hb_fsClose( pFile->hFile );
      if( pFile->pFileName )
         fl_free( pFile->pFileName );
      fl_free( pFile );
   }

   uiUsers--;
}

static void ParseCommand( PUSERSTRU pUStru )
{
   BYTE * ptr;

   ptr = pUStru->pBufRead;
#if defined(MY_DBG_)
   OutputDebugString( "PARSECOMMAND" );
   OutputDebugString( ( char * ) ptr );
#endif
   switch( *ptr )
   {
      /* hb_fsReadAt( pFile->hFile, buffer, ulSize, llOffset ) */
      case 'D':
         filere_ReadAt( pUStru, ptr + 2 );
         break;

      /* hb_fsWriteAt( pFile->hFile, buffer, ulSize, llOffset ) */
      case 'E':
         filere_WriteAt( pUStru, ptr + 2 );
         break;

      // hb_fsSeekLarge( pFile->hFile, 0, FS_END )
      case 'G':
         filere_SeekLarge( pUStru, ptr + 2 );
         break;

      // hb_fsCommit( pFile->hFile )
      case 'H':
         filere_Commit( pUStru, ptr + 2 );
         break;

      // hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError )
      case 'A':
         filere_ExtOpen( pUStru, ptr + 2 );
         break;

      // hb_fsClose( hFile )
      case 'B':
         filere_Close( pUStru, ptr + 2 );
         break;

      // hb_fsLockLarge( pFile->hFile, ulStart, ulLen, ( USHORT ) iType )
      case 'C':
         filere_LockLarge( pUStru, ptr + 2 );
         break;

      // hb_fsTruncAt( pFile->hFile, llOffset )
      case 'F':
         filere_TruncAt( pUStru, ptr + 2 );
         break;

      // hb_fsDelete( pFilename )
      case 'I':
         filere_Delete( pUStru, ptr + 2 );
         break;

      // hb_fsDelete( pFilename )
      case 'J':
         filere_WriteLarge( pUStru, ptr + 2 );
         break;

      // hb_fsReadLarge( HB_FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
      case 'K':
         filere_ReadLarge( pUStru, ptr + 2 );
         break;

      // hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, ULONG ulAttr, BYTE * pszName )
      case 'L':
         filere_CreateTemp( pUStru, ptr + 2 );
         break;

      // hb_fsFileExists( const char * pszFileName )
      case 'M':
         filere_Exists( pUStru, ptr + 2 );
         break;

      // hb_fsFile( BYTE * pFileName )
      case 'N':
         filere_File( pUStru, ptr + 2 );
         break;

      // hb_fsWrite( HB_FHANDLE hFileHandle, const BYTE * pBuff, USHORT uiCount )
      case 'O':
         filere_Write( pUStru, ptr + 2 );
         break;

      // hb_fsFindFirst( const char * pszFileName, ULONG ulAttr );
      case 'P':
         filere_FindFirst( pUStru, ptr + 2 );
         break;

      // hb_fsFindNext( PHB_FFIND ffind );
      case 'Q':
         filere_FindNext( pUStru, ptr + 2 );
         break;

      // hb_fsFindClose( PHB_FFIND ffind );
      case 'R':
         filere_FindClose( pUStru, ptr + 2 );
         break;

      // hb_fsFindClose( PHB_FFIND ffind );
      case 'S':
         filere_GetFileAttributes( pUStru, ptr + 2 );
         break;

      // hb_fsMkDir( BYTE * cPath );
      case 'T':
         filere_MkDir( pUStru, ptr + 2 );
         break;

      // hb_fsRmDir( BYTE * cPath );
      case 'U':
         filere_RmDir( pUStru, ptr + 2 );
         break;

      // hb_fsRename( BYTE * cFileOld, BYTE * cFileNew );
      case 'V':
         filere_Rename( pUStru, ptr + 2 );
         break;

      // USHORT hb_fsCurDirBuffEx( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen )
      case 'W':
         filere_CurDirBuffEx( pUStru, ptr + 2 );
         break;

      // quit
      case 'q':
         filere_DelUser( pUStru );
         break;
   }
}

void filere_ScanUser( void )
{
   PUSERSTRU pUStru;
   USHORT ui;
   int iLen;
   BOOL bRes;
   for( ui=0,pUStru=s_users; ui<uiUsersMax; ui++,pUStru++ )
   {
      if( hb_ip_rfd_isset( pUStru->hSocket ) )
      {
         bRes = ( pUStru->pBufRead == NULL );
         if( !bRes )
         {
            #if defined( HB_OS_WIN )
            Sleep( 0 );
            #else
            sleep( 0 );
            #endif
            continue;
         }

         if( pUStru->ulBufferLen - pUStru->ulDataRead < HB_SENDRECV_BUFFER_SIZE )
         {
            pUStru->ulBufferLen += HB_SENDRECV_BUFFER_SIZE;
            pUStru->pBuffer = ( BYTE * ) fl_realloc( pUStru->pBuffer, pUStru->ulBufferLen );
         }

         iLen = hb_ipRecv( pUStru->hSocket, ( char * ) pUStru->pBuffer + pUStru->ulDataRead, HB_SENDRECV_BUFFER_SIZE );

         if( ! hb_iperrorcode() )
         {
            pUStru->ulDataRead += iLen;

            {
               BYTE * ptr = pUStru->pBuffer;
#if defined(MY_DBG_3)
               char pp[50];
               sprintf( pp, "%lu - %lu", pUStru->ulDataRead, pUStru->ulDataLen );
               OutputDebugString( pp );
#endif
               if( pUStru->ulDataLen == 0 && pUStru->ulDataRead >= HB_LENGTH_ACK )
                  pUStru->ulDataLen = HB_GET_BE_UINT32( ptr );

               if( pUStru->ulDataLen > 0 && pUStru->ulDataRead == pUStru->ulDataLen + HB_LENGTH_ACK )
               {
                  *(ptr + pUStru->ulDataRead - 2) = '\0';
                  pUStru->pBufRead = ptr + HB_LENGTH_ACK;

                  ParseCommand( pUStru );

                  pUStru->ulDataLen = 0;
                  pUStru->ulDataRead = 0;
                  pUStru->pBufRead = NULL;
                }
            }
         }
         else
         {
            // Socket error while reading
            filere_DelUser( pUStru );
         }
      }
   }

}

PUSERSTRU filere_AddUser( HB_SOCKET_T hSocket )
{
   PUSERSTRU pUStru = s_users;
   USHORT ui = 0;

   while( ui < uiUsersAlloc && pUStru->hSocket )
   {
     pUStru ++;
     ui ++;
   }
   if( ui == uiUsersAlloc )
   {
      s_users = ( USERSTRU * ) fl_realloc( s_users, sizeof( USERSTRU ) * ( uiUsersAlloc + USERS_REALLOC ) );
      memset( s_users + uiUsersAlloc, 0, sizeof( USERSTRU ) * USERS_REALLOC );
      pUStru = s_users + uiUsersAlloc;
      uiUsersAlloc += USERS_REALLOC;
   }
   pUStru->hSocket = hSocket;
   pUStru->ulBufferLen = HB_SENDRECV_BUFFER_SIZE;
   pUStru->pBuffer = ( BYTE * ) fl_alloc( pUStru->ulBufferLen );

   pUStru->ulBufAnswerLen = HB_SENDRECV_BUFFER_SIZE;
   pUStru->pBufAnswer = ( BYTE * ) fl_alloc( pUStru->ulBufAnswerLen );

   pUStru->ulBufTempLen = HB_SENDRECV_BUFFER_SIZE;
   pUStru->pBufTemp = ( BYTE * ) fl_alloc( pUStru->ulBufTempLen );

   pUStru->s_openFiles = NULL;

   if( ++ui > uiUsersMax )
      uiUsersMax = ui;

   uiUsers++;

   return pUStru;
}

void ServiceExecution( void )
{
   char szBuffer[32];
   HB_SOCKET_T incoming;
   long int lTemp;

   hb_ipInit();

   hb_ip_rfd_zero();

   hSocketMain = hb_ipServer( ( int ) iServerPort, NULL, 10 );
   if( hSocketMain == -1 )
      return;
   hb_ip_rfd_set( hSocketMain );

#if defined( HB_OS_WIN )

#if defined( LPFN_TRANSMITPACKETS )
   EnterCriticalSection( &myCS );
   if( ! lpfnTransmitPackets )
   {
       /* Get pointers to the ws2_32 implementations.
        * NOTE: This assumes that ws2_32 contains only one implementation
        * of these functions, i.e. that you cannot get different functions
        * back by passing another socket in. If that ever changes, we'll need
        * to think about associating the functions with the socket and
        * exposing that information to this dll somehow.
        */
       const GUID guidTransmitPackets = WSAID_TRANSMITPACKETS;
       DWORD len;

       WSAIoctl( hSocketMain, SIO_GET_EXTENSION_FUNCTION_POINTER,
                 ( void * ) &guidTransmitPackets, sizeof( guidTransmitPackets ),
                 &lpfnTransmitPackets, sizeof( lpfnTransmitPackets ), &len, NULL, NULL );
   }
   LeaveCriticalSection( &myCS );
#endif

   while( ServiceStatus.dwCurrentState == SERVICE_RUNNING || ServiceStatus.dwCurrentState == SERVICE_PAUSED )
#else
   while( ServiceStatus == SERVICE_RUNNING || ServiceStatus == SERVICE_PAUSED )
#endif
   {
#if defined( HB_OS_WIN )
      if( ServiceStatus.dwCurrentState == SERVICE_PAUSED )
#else
      if( ServiceStatus == SERVICE_PAUSED )
#endif
      {
         #if defined( HB_OS_WIN )
         Sleep( 0 );
         #else
         sleep( 0 );
         #endif
         continue;
      }

      if( hb_ip_rfd_select( 1 ) > 0 )
      {
         if( hb_ip_rfd_isset( hSocketMain ) )
         {
            incoming = hb_ipAccept( hSocketMain, -1, szBuffer, &lTemp );

            if( ! hb_iperrorcode() && uiUsers <= MAXUSERS )
            {
               PUSERSTRU pUStru = filere_AddUser( incoming );

               hb_ip_rfd_set( incoming );

               lTemp = strlen( szBuffer );
               pUStru->szAddr = ( BYTE * ) fl_alloc( lTemp + 1 );
               memcpy( pUStru->szAddr, szBuffer, lTemp );
               pUStru->szAddr[lTemp] = '\0';
               filere_SendAnswer( pUStru, ( BYTE * ) szBuffer, strlen( szBuffer ) );
               printf( "LogIn user with IP %s at socket %p\r\n", pUStru->szAddr, incoming );
            }
         }
         filere_ScanUser();
      }
#ifdef __CONSOLE__
      if( hb_inkey( FALSE, 0, hb_setGetEventMask() ) == 27 )
         break;
#endif
   }
   hb_ipCleanup();

}

#if defined( HB_OS_WIN )
void SvcDebugOut( LPSTR String, void * Status )
{
   char pBuffer[500];
   sprintf( pBuffer, String, Status );
   OutputDebugString( pBuffer );
}
#else
void SvcDebugOut( char * String, void * Status )
{
   printf( String, Status );
}
#endif
