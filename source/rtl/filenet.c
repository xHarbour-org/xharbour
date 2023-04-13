/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    functions to access files with shared handles and locks
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

#if defined( _MSC_VER ) && ( _MSC_VER >= 1400 )
   #ifndef _CRT_SECURE_NO_WARNINGS
      #define _CRT_SECURE_NO_WARNINGS
   #endif
#endif

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_INTERNAL_

/* file name prefix used by this file IO implementation */
#define NETIO_FILE_PREFIX     "rem:"
#define NETIO_FILE_PREFIX_LEN strlen( NETIO_FILE_PREFIX )

#include "hbapi.h"
#include "hbinit.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbapierr.h"
// #include "hbstack.h"
#include "hbvm.h"
#include "hbipapi.h"
#include "thread.h"
#include "directry.ch"

#if ! defined( HB_OS_WIN_CE )
#  include <sys/types.h>
#  include <sys/stat.h>
#endif
#if  ( defined( __GNUC__ ) && \
   ( defined( HB_OS_LINUX ) || defined( HB_OS_DARWIN ) ) )
#include <netinet/in.h>
#endif

#define HB_FLOCK_RESIZE 16

#define HB_LENGTH_ACK   4
static char       szDataACK[ HB_LENGTH_ACK ];

static PHB_FILE   s_openFiles = NULL;

/*
   #define _DBG_
 */

typedef struct
{
   HB_FOFFSET start;
   HB_FOFFSET len;
}
HB_FLOCK, * PHB_FLOCK;

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   char * pFileName;
   int used;
   BOOL shared;
   BOOL BufferLock;
   HB_SOCKET_T hSocket;
   HB_FHANDLE hFile;
   PHB_FLOCK pLocks;
   UINT uiLocks;
   UINT uiSize;
   struct _HB_FILE * pNext;
   struct _HB_FILE * pPrev;
}
HB_FILE;

#if defined( HB_OS_WIN )
   #define HB_SOCKET_T  SOCKET
#else
   #define HB_SOCKET_T  int
#endif

static HB_SOCKET_T hSocket;
static char *        szBuffer    = NULL;
static ULONG         lBufferLen  = 0;
static const char *  szOk        = "+1";
static int           sBufferSize = 32767;

#define BUFFER_SIZE  sBufferSize

int hb_ipSend( HB_SOCKET_T hSocket, char * szBuffer, int iSend, int timeout );
static const HB_FILE_FUNCS * hb_fileNetMethods( void );
static BOOL s_fileAccept( const char * pFilename );

#if ( defined( _MSC_VER ) && ( _MSC_VER >= 1400 ) )
#define sscanf       sscanf_s
#endif

static PHB_FILE hb_fileFind( char * pFileName, HB_SOCKET_T hSocketLoc )
{
   if( s_openFiles && pFileName )
   {
      PHB_FILE pFile = s_openFiles;
      do
      {
         if( hSocketLoc == pFile->hSocket &&
             strncmp( ( const char * ) pFile->pFileName, ( char * ) pFileName, strlen( ( const char * ) pFileName ) ) == 0 )
            return pFile;
         pFile = pFile->pNext;
      }
      while( s_openFiles != pFile );
   }
   return NULL;
}

static PHB_FILE hb_fileFindByHandle( HB_FHANDLE hFile, HB_SOCKET_T hSocketLoc )
{
   if( s_openFiles && hFile )
   {
      PHB_FILE pFile = s_openFiles;
      do
      {
         if( hSocketLoc == pFile->hSocket && pFile->hFile == hFile )
            return pFile;
         pFile = pFile->pNext;
      }
      while( s_openFiles != pFile );
   }
   return NULL;
}

static PHB_FILE hb_fileNetNew( HB_FHANDLE hFile, BOOL fShared, BOOL fBufferLock, BOOL fRemote, char * pFileName )
{
   PHB_FILE pFile = ( pFileName && fRemote ? hb_fileFindByHandle( hFile, fRemote ? hSocket : 0 ) : NULL );

   if( ! pFile )
   {
//       pFile             = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );
//       memset( pFile, 0, sizeof( HB_FILE ) );
      pFile = (PHB_FILE) hb_xgrabz( sizeof( HB_FILE ) );
      pFile->pFileName  = ( char * ) hb_xalloc( HB_PATH_MAX );
      memset( pFile->pFileName, 0, sizeof( HB_PATH_MAX ) );
      if( pFileName )
         HB_MEMCPY( pFile->pFileName, pFileName, strlen( ( const char * ) pFileName ) );
      pFile->pFuncs     = hb_fileNetMethods();
      pFile->hFile      = hFile;
      pFile->shared     = fShared;
      pFile->BufferLock = fBufferLock;
      if( hSocket && fRemote )
         pFile->hSocket = hSocket;
      else
         pFile->hSocket = 0;

      if( s_openFiles )
      {
         pFile->pNext         = s_openFiles;
         pFile->pPrev         = s_openFiles->pPrev;
         pFile->pPrev->pNext  = pFile;
         s_openFiles->pPrev   = pFile;
      }
      else
         s_openFiles = pFile->pNext = pFile->pPrev = pFile;
   }
   pFile->used++;

   return pFile;
}

static UINT hb_fileNetFindOffset( PHB_FILE pFile, HB_FOFFSET ulOffset )
{
   UINT uiFirst  = 0;
   UINT uiLast   = pFile->uiLocks;
   UINT uiMiddle = uiLast >> 1;

   while( uiFirst < uiLast )
   {
      HB_FOFFSET ulEnd = pFile->pLocks[ uiMiddle ].start +
                         pFile->pLocks[ uiMiddle ].len;

      uiFirst  = ( ulEnd <= ulOffset ) ? uiMiddle + 1 : uiMiddle;

      if ( ulEnd <= ulOffset )
         uiFirst = uiMiddle + 1;
      else
         uiLast = uiMiddle;

      uiMiddle = ( uiFirst + uiLast ) >> 1;
   }

   return uiMiddle;
}

static void hb_fileNetInsertLock( PHB_FILE pFile, UINT uiPos,
                                  HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   if( pFile->uiLocks == pFile->uiSize )
   {
      pFile->uiSize  += HB_FLOCK_RESIZE;
      pFile->pLocks  = ( PHB_FLOCK ) hb_xrealloc( pFile->pLocks,
                                                  sizeof( HB_FLOCK ) * pFile->uiSize );
      memset( &pFile->pLocks[ pFile->uiLocks ], 0,
              sizeof( HB_FLOCK ) * HB_FLOCK_RESIZE );
   }
   memmove( &pFile->pLocks[ uiPos + 1 ], &pFile->pLocks[ uiPos ],
            ( pFile->uiLocks - uiPos ) * sizeof( HB_FLOCK ) );
   pFile->pLocks[ uiPos ].start  = ulStart;
   pFile->pLocks[ uiPos ].len    = ulLen;
   pFile->uiLocks++;
}

static void hb_fileNetDeleteLock( PHB_FILE pFile, UINT uiPos )
{
   pFile->uiLocks--;
   memmove( &pFile->pLocks[ uiPos ], &pFile->pLocks[ uiPos + 1 ],
            ( pFile->uiLocks - uiPos ) * sizeof( HB_FLOCK ) );
   if( pFile->uiSize - pFile->uiLocks >= ( HB_FLOCK_RESIZE << 1 ) )
   {
      pFile->uiSize  -= HB_FLOCK_RESIZE;
      pFile->pLocks  = ( PHB_FLOCK ) hb_xrealloc( pFile->pLocks,
                                                  sizeof( HB_FLOCK ) * pFile->uiSize );
   }
}

static BOOL hb_fileNetSetLock( PHB_FILE pFile, BOOL * pfLockFS,
                               HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   BOOL  fLJoin = FALSE, fRJoin = FALSE;
   UINT  uiPos  = hb_fileNetFindOffset( pFile, ulStart );

   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( ulStart + ulLen > pLock->start )
         return FALSE;
      if( ulStart + ulLen == pLock->start )
         fRJoin = TRUE;
   }
   if( uiPos > 0 )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos - 1 ];
      if( pLock->start + pLock->len == ulStart )
         fLJoin = TRUE;
   }
   if( fLJoin )
   {
      if( fRJoin )
      {
         pFile->pLocks[ uiPos - 1 ].len += ulLen + pFile->pLocks[ uiPos ].len;
         hb_fileNetDeleteLock( pFile, uiPos );
      }
      else
         pFile->pLocks[ uiPos - 1 ].len += ulLen;
   }
   else if( fRJoin )
   {
      pFile->pLocks[ uiPos ].start  -= ulLen;
      pFile->pLocks[ uiPos ].len    += ulLen;
   }
   else
      hb_fileNetInsertLock( pFile, uiPos, ulStart, ulLen );

   if( pFile->shared )
      *pfLockFS = TRUE;
   return TRUE;
}

static BOOL hb_fileNetUnlock( PHB_FILE pFile, BOOL * pfLockFS,
                              HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   BOOL  fResult = FALSE;
   UINT  uiPos   = hb_fileNetFindOffset( pFile, ulStart );

   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( ulStart >= pLock->start &&
          ulStart + ulLen <= pLock->start + pLock->len )
      {
         if( ulStart == pLock->start )
         {
            if( ulLen == pLock->len )
               hb_fileNetDeleteLock( pFile, uiPos );
            else
            {
               pLock->start   += ulLen;
               pLock->len     -= ulLen;
            }
         }
         else if( ulStart + ulLen == pLock->start + pLock->len )
            pLock->len -= ulLen;
         else
         {
            hb_fileNetInsertLock( pFile, uiPos + 1, ulStart + ulLen,
                                  pLock->start + pLock->len - ulStart - ulLen );
            pLock->len = ulStart - pLock->start;
         }
         if( pFile->shared )
            *pfLockFS = TRUE;
         fResult = TRUE;
      }
   }
   return fResult;
}

/*
 * net functions
 */

static char * hb_strToken( char * szText, HB_SIZE ulText, HB_SIZE ulIndex, HB_SIZE * pulLen )
{
   HB_SIZE ulStart;
   HB_SIZE ulEnd       = 0;
   HB_SIZE ulCounter   = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_strToken(%s, %" HB_PFS "u, %" HB_PFS "u, %d, %p)", szText, ulText, ulIndex, pulLen ) );

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
      return "";
   }
   else
   {
      *pulLen = ulEnd - ulStart;
      return szText + ulStart;
   }
}

static long int hb_Net_Recv( HB_SOCKET_T hSockets )
{
   ULONG ulDataLen;
   ULONG ulRead = 0;
   int   iRet;

   while( hb_ipDataReady( hSockets, 2 ) == 0 )
   {
   }

   while( ulRead < HB_LENGTH_ACK )
   {
      iRet = hb_ipRecv( hSockets, szBuffer + ulRead, lBufferLen - ulRead );
      if( iRet > 0 )
         ulRead += ( ULONG ) iRet;
   }
   ulDataLen = HB_GET_BE_UINT32( szBuffer );

   if( ulDataLen > 0 && ulRead < ( ulDataLen + HB_LENGTH_ACK ) )
   {

      if( lBufferLen < ulDataLen + 1 + HB_LENGTH_ACK )
      {
         lBufferLen  = ulDataLen + 1 + HB_LENGTH_ACK;
         szBuffer    = ( char * ) hb_xrealloc( szBuffer, lBufferLen );
      }
      while( ulRead < ( ulDataLen + HB_LENGTH_ACK ) )
      {
         iRet = hb_ipRecv( hSockets, szBuffer + ulRead, lBufferLen - ulRead );
         if( iRet > 0 )
            ulRead += ( ULONG ) iRet;
      }

      *( szBuffer + ulRead ) = '\0';
#if defined( _DBG_ )
      OutputDebugString( szBuffer + HB_LENGTH_ACK );
#endif
   }

   return ( long int ) ( ulDataLen );
}

static void hb_NetOpenConnection( const char * szAddr, int iPort )
{
   hSocket = hb_ipConnect( szAddr, htons( ( u_short ) iPort ), 5000 );

   if( hSocket != ( HB_SOCKET_T ) -1 )
   {
      int nLen = hb_ipRecv( hSocket, szBuffer, lBufferLen );
      if( nLen == 0 )
      {
         hb_ipclose( hSocket );
         hSocket = 0;
      }
      else if( nLen < HB_LENGTH_ACK + 1 )
         hb_ipRecv( hSocket, szBuffer, lBufferLen );
   }
   else
      hSocket = 0;
}

static long int hb_NetDataSingleSendRecv( HB_SOCKET_T hCurSocket, char * szData, HB_SIZE ulLen )
{
#if defined( _DBG_ )
   OutputDebugString( "---------1---------2---------3---------4" );
   OutputDebugString( szData + HB_LENGTH_ACK );
#endif
   if( hb_ipSend( hCurSocket, szData, ( int ) ulLen, -1 ) != ( int ) ulLen )
      return 0;

   return hb_Net_Recv( hCurSocket ) - 1;
}

static int hb_NetSingleSendRecv( HB_SOCKET_T hCurSocket, char * sData, HB_SIZE ulLen, int iErr )
{
   long int lRet;

   lRet = hb_NetDataSingleSendRecv( hCurSocket, sData, ulLen );

   if( ! lRet )
      hb_errRT_BASE( EG_DATATYPE, 1000, NULL, "Sending error", 0 );
   else if( *szBuffer == '-' && iErr )
   {
      hb_errRT_BASE( EG_DATATYPE, ( HB_ERRCODE ) iErr, NULL, szBuffer, 0 );
      return 0;
   }
   return lRet;
}

static long int hb_Net_SingleRecv( HB_SOCKET_T hSockets )
{
   while( hb_ipDataReady( hSockets, 2 ) == 0 )
   {
   }

   hb_ipRecv( hSockets, szBuffer, lBufferLen );

   *( szBuffer + 2 ) = '\0';

   return 2;
}

static long int hb_NetDataSendSingleRecv( HB_SOCKET_T hCurSocket, char * szData, HB_SIZE ulLen )
{
   HB_PUT_BE_UINT32( szDataACK, ulLen );
#if defined( _DBG_ )
   OutputDebugString( "---------------------1" );
   OutputDebugString( szData );
#endif
   if( hb_ipSend( hCurSocket, szDataACK, HB_LENGTH_ACK, -1 ) == HB_LENGTH_ACK )
   {
      if( hb_ipSend( hCurSocket, szData, ( int ) ulLen, -1 ) != ( int ) ulLen )
         return 0;
   }
   else
      return 0;

   return hb_Net_SingleRecv( hCurSocket ) - 1;
}

static int hb_NetSendSingleRecv( HB_SOCKET_T hCurSocket, char * sData, HB_SIZE ulLen, int iErr )
{
   long int lRet = hb_NetDataSendSingleRecv( hCurSocket, sData, ulLen );

   if( ! lRet )
      hb_errRT_BASE( EG_DATATYPE, 1000, NULL, "Server don't answer", 0 );
   else if( *szBuffer == '-' && iErr )
   {
      hb_errRT_BASE( EG_DATATYPE, ( HB_ERRCODE ) iErr, NULL, szBuffer, 0 );
      return 0;
   }
   return lRet;
}

static long int hb_NetDataSingleSendSingleRecv( HB_SOCKET_T hCurSocket, char * szData, HB_SIZE ulLen )
{
#if defined( _DBG_ )
   OutputDebugString( "---------------------1" );
   OutputDebugString( szData + HB_LENGTH_ACK );
#endif
   if( hb_ipSend( hCurSocket, szData, ( int ) ulLen, -1 ) != ( int ) ulLen )
      return 0;

   return hb_Net_SingleRecv( hCurSocket ) - 1;
}

static int hb_NetSingleSendSingleRecv( HB_SOCKET_T hCurSocket, char * sData, HB_SIZE ulLen, int iErr )
{
   long int lRet = hb_NetDataSingleSendSingleRecv( hCurSocket, sData, ulLen );

   if( ! lRet )
      hb_errRT_BASE( EG_DATATYPE, 1000, NULL, "Sending error", 0 );
   else if( *szBuffer == '-' && iErr )
   {
      hb_errRT_BASE( EG_DATATYPE, ( HB_ERRCODE ) iErr, NULL, szBuffer, 0 );
      return 0;
   }
   return lRet;
}

static int hb_NetGetCmdItem( char ** pptr, char * szDest )
{
   char * ptr = *pptr;

   while( *ptr && *ptr != '|' )
      ptr++;
   if( *ptr )
   {
      if( ptr > *pptr )
         HB_MEMCPY( szDest, *pptr, ptr - *pptr );
      szDest[ ptr - *pptr ]   = '\0';
      *pptr                   = ptr;
      return 1;
   }
   else
      return 0;
}

char * hb_NetFirstChar( void )
{
   return szBuffer + HB_LENGTH_ACK + 1;
}

/*
 * public API functions
 */

/* convert file name for hb_fsExtOpen
 * caller must free the returned buffer
 */
static void hb_NetCloseConnection( HB_SOCKET_T hCurSocket )
{
   if( hCurSocket )
   {
      /* Quit */
      hb_NetSendSingleRecv( hCurSocket, "q\r\n", 3, 0 );

      hb_ipclose( hCurSocket );
      if( hCurSocket == hSocket )
         hSocket = 0;
   }
}

USHORT hb_fileNetCurDirBuffEx( USHORT uiDrive, char * pbyBuffer, HB_SIZE ulLen )
{
   if( hSocket )
   {
      char  szData[ 25 + HB_LENGTH_ACK ];
      int   nSend;

      /* USHORT hb_fsCurDirBuffEx( USHORT uiDrive, char * pbyBuffer, ULONG ulLen ) */
      nSend = sprintf( szData + HB_LENGTH_ACK, "W|%hu|%lu|\r\n", uiDrive, ( long unsigned int ) ulLen );
      HB_PUT_BE_UINT32( szData, nSend );
      if( hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1020 ) )
      {
         char *   ptr;
         USHORT   uiRet;

         ptr = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptr, ( char * ) pbyBuffer ); ptr++;
         hb_NetGetCmdItem( &ptr, szData );
         sscanf( szData, "%hu|", &uiRet );
         return uiRet;
      }
      else
      {
         return 0;
      }
   }
   else
      return hb_fsCurDirBuffEx( uiDrive, pbyBuffer, ulLen );
}

static char * hb_NetExtName( const char * pFilename, const char * pDefExt, HB_FATTR uiExFlags, const char * pPaths )
{
   HB_PATHNAMES * pNextPath;
   PHB_FNAME      pFilepath = hb_fsFNameSplit( ( char * ) pFilename );
   BOOL           fIsFile   = FALSE;
   char *         szPath    = ( char * ) hb_xgrab( HB_PATH_MAX );

   if( pDefExt && ( ( uiExFlags & FXO_FORCEEXT ) || ! pFilepath->szExtension ) )
      pFilepath->szExtension = ( char * ) pDefExt;

   if( pFilepath->szPath )
      hb_fsFNameMerge( ( char * ) szPath, pFilepath );
   else if( uiExFlags & FXO_DEFAULTS )
   {
      char * szDefault = ( char * ) hb_setGetDefault();
      if( szDefault && *szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( ( char * ) szPath, pFilepath );
         fIsFile           = hb_FileNetFile( szPath );
      }
      else
      {
         char pbyDirBuffer[ HB_PATH_MAX ];
         hb_fileNetCurDirBuffEx( 0, pbyDirBuffer, HB_PATH_MAX );
         pFilepath->szPath = ( char * ) pbyDirBuffer;
         hb_fsFNameMerge( ( char * ) szPath, pFilepath );
         fIsFile           = hb_FileNetFile( szPath );
      }
      if( ! fIsFile && hb_setGetPath() )
      {
         pNextPath = hb_setGetFirstSetPath();
         while( ! fIsFile && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            hb_fsFNameMerge( ( char * ) szPath, pFilepath );
            fIsFile           = hb_FileNetFile( szPath );
            pNextPath         = pNextPath->pNext;
         }
      }
      if( ! fIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : NULL;
         hb_fsFNameMerge( ( char * ) szPath, pFilepath );
      }
   }
   else if( pPaths && *pPaths )
   {
      HB_PATHNAMES * pSearchPath = NULL;
      hb_fsAddSearchPath( ( char * ) pPaths, &pSearchPath );
      pNextPath = pSearchPath;
      while( ! fIsFile && pNextPath )
      {
         pFilepath->szPath = pNextPath->szPath;
         hb_fsFNameMerge( ( char * ) szPath, pFilepath );
         fIsFile           = hb_FileNetFile( szPath );
         pNextPath         = pNextPath->pNext;
      }
      hb_fsFreeSearchPath( pSearchPath );
      if( ! fIsFile )
      {
         pFilepath->szPath = NULL;
         hb_fsFNameMerge( ( char * ) szPath, pFilepath );
      }
   }
   else
      hb_fsFNameMerge( ( char * ) szPath, pFilepath );

   hb_xfree( pFilepath );

   return szPath;
}

PHB_FILE hb_fileNetExtOpen( const char * pFileName, const char * pDefExt,
                            HB_FATTR uiExFlags, const char * pPaths,
                            PHB_ITEM pError, BOOL fBufferLock )
{
   PHB_FILE    pFile;
   BOOL        fShared;
   HB_FHANDLE  hFile;
   char *      pszFile;

   if( s_fileAccept( pFileName ) )
      pFileName += NETIO_FILE_PREFIX_LEN;

   fShared  = ( uiExFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0;
   pszFile  = hb_NetExtName( pFileName, pDefExt, uiExFlags, pPaths );

   pFile    = hb_fileFind( pszFile, hSocket );
   if( pFile )
   {
      if( ! fShared || ! pFile->shared || ( uiExFlags & FXO_TRUNCATE ) != 0 )
      {
         hb_fsSetError( ( uiExFlags & FXO_TRUNCATE ) ? 5 : 32 );
         pFile = NULL;
      }
      else
      {
         pFile->used++;
      }
   }
   else
   {
      if( hSocket )
      {
         char  szData[ HB_PATH_MAX * 3 + 32 ];
         int   nSend = sprintf( szData + HB_LENGTH_ACK, "A|%s|%s|%hu|%s|\r\n", pszFile, ( pDefExt ? pDefExt : ( char * ) "" ),
                                uiExFlags, ( pPaths ? pPaths : ( char * ) "" ) );
         HB_PUT_BE_UINT32( szData, nSend );

         /* hFile = hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError ); */
         if( hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1000 ) )
         {
            char *   ptr;
            USHORT   uiError;

            ptr = hb_NetFirstChar();
            hb_NetGetCmdItem( &ptr, szData ); ptr++;
            sscanf( szData, "%p|", ( void ** ) &hFile );
            hb_NetGetCmdItem( &ptr, szData );
            sscanf( szData, "%hu|", &uiError );
            hb_fsSetError( uiError );
         }
         else
         {
            hFile = FS_ERROR;
            hb_fsSetError( ( USHORT ) FS_ERROR );
         }
      }
      else
         hFile = hb_fsExtOpen( pFileName, pDefExt, uiExFlags, pPaths, pError );

      if( hFile != FS_ERROR )
      {
         hb_threadLock( S_FILENETMTX );
         pFile = hb_fileNetNew( hFile, fShared, fBufferLock, TRUE, pszFile );
         hb_threadUnLock( S_FILENETMTX );
      }

      if( pError )
      {
         char * szPath;
         szPath = hb_NetExtName( pFileName, pDefExt, uiExFlags, pPaths );
         hb_errPutFileName( pError, ( char * ) szPath );
         if( hFile == FS_ERROR )
         {
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutGenCode( pError, ( USHORT ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
         }
      }

   }
   hb_xfree( pszFile );

   return pFile;
}

void hb_fileNetClose( PHB_FILE pFile )
{
   HB_FHANDLE  hFile       = FS_ERROR;
   HB_SOCKET_T hCurSocket = 0 ;

   hb_threadLock( S_FILENETMTX );

   if( --pFile->used == 0 )
   {
      if( pFile->pNext )
      {
         pFile->pPrev->pNext  = pFile->pNext;
         pFile->pNext->pPrev  = pFile->pPrev;
         if( pFile == s_openFiles )
         {
            s_openFiles = pFile->pNext;
            if( pFile == s_openFiles )
               s_openFiles = NULL;
         }
      }

      hFile       = pFile->hFile;
      hCurSocket  = pFile->hSocket;

      if( pFile->pLocks )
         hb_xfree( pFile->pLocks );
      if( pFile->pFileName )
         hb_xfree( pFile->pFileName );
      hb_xfree( pFile );
   }

   hb_threadUnLock( S_FILENETMTX );

   if( hFile != FS_ERROR )
   {
      char  szData[ 14 + HB_LENGTH_ACK ];
      ULONG nSend;

      if( hCurSocket )
      {
	     #if defined(HB_ARCH_64BIT)            	         
	        nSend = sprintf( szData + HB_LENGTH_ACK, "B|%p|\r\n", ( void * ) ((uintptr_t) hFile ));
	     #else 
            nSend = sprintf( szData + HB_LENGTH_ACK, "B|%p|\r\n", ( void * ) hFile );
         #endif
         HB_PUT_BE_UINT32( szData, nSend );
         hb_NetSingleSendSingleRecv( hCurSocket, szData, nSend + HB_LENGTH_ACK, 1001 );
      }
      else
         hb_fsClose( hFile );
   }
}

BOOL hb_fileNetLock( PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen, int iType )
{
   BOOL fResult = FALSE, fLockFS = FALSE;

   if( ( iType & FL_MASK ) == FL_UNLOCK )
   {
      if( pFile->BufferLock )
      {
         hb_threadLock( S_FILENETMTX );
         fResult = hb_fileNetUnlock( pFile, &fLockFS, ulStart, ulLen );
         hb_threadUnLock( S_FILENETMTX );
      }
      else
      {
         fLockFS  = TRUE;
         fResult  = TRUE;
      }

      if( fLockFS )
      {
         char  szData[ 75 + HB_LENGTH_ACK ] = { 0 };
         int   nSend;

         if( pFile->hSocket )
         {
         #if defined(HB_ARCH_64BIT)            	         
            nSend = sprintf( szData + HB_LENGTH_ACK, "C|%p|%" PFHL "i|%" PFHL "i|%hu|\r\n", ( void * ) ((uintptr_t)pFile->hFile), ulStart, ulLen, ( USHORT ) iType );
         #else
            nSend = sprintf( szData + HB_LENGTH_ACK, "C|%p|%" PFHL "i|%" PFHL "i|%hu|\r\n", ( void * ) pFile->hFile, ulStart, ulLen, ( USHORT ) iType );
         #endif            
            
            HB_PUT_BE_UINT32( szData, nSend );
            hb_NetSingleSendSingleRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1002 );
         }
         else
            hb_fsLockLarge( pFile->hFile, ulStart, ulLen, ( USHORT ) iType );
      }
   }
   else
   {
      if( pFile->BufferLock )
      {
         hb_threadLock( S_FILENETMTX );
         fResult = hb_fileNetSetLock( pFile, &fLockFS, ulStart, ulLen );
         hb_threadUnLock( S_FILENETMTX );
      }
      else
         fLockFS = TRUE;

      if( fLockFS )
      {
         char  szData[ 75 + HB_LENGTH_ACK ];
         int   nSend;

         if( pFile->hSocket )
         {
#if defined(HB_ARCH_64BIT)            
            nSend = sprintf( szData + HB_LENGTH_ACK, "C|%p|%" PFHL "i|%" PFHL "i|%hu|\r\n", ( void * ) ((uintptr_t)pFile->hFile), ulStart, ulLen, ( USHORT ) iType );
#else	         
            nSend = sprintf( szData + HB_LENGTH_ACK, "C|%p|%" PFHL "i|%" PFHL "i|%hu|\r\n", ( void * ) pFile->hFile, ulStart, ulLen, ( USHORT ) iType );
#endif            
            HB_PUT_BE_UINT32( szData, nSend );
            if( hb_NetSingleSendSingleRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1003 ) )
               fResult = ( strncmp( szBuffer, szOk, 2 ) == 0 );
            else
               fResult = FALSE;
         }
         else
            fResult = hb_fsLockLarge( pFile->hFile, ulStart, ulLen, ( USHORT ) iType );

         if( pFile->BufferLock && ! fResult )
         {
            hb_threadLock( S_FILENETMTX );
            hb_fileNetUnlock( pFile, &fLockFS, ulStart, ulLen );
            hb_threadUnLock( S_FILENETMTX );
         }
      }
   }
   return fResult;
}

HB_SIZE hb_fileNetReadAt( PHB_FILE pFile, void * pBuffer, HB_SIZE ulSize, HB_FOFFSET llOffset )
{
   HB_SIZE ulRead;

   if( pFile->hSocket )
   {
      char  szData[ 50 + HB_LENGTH_ACK ];
      ULONG ulLen;
      int   nSend;
#if defined(HB_ARCH_64BIT)                  
      nSend = sprintf( szData + HB_LENGTH_ACK, "D|%p|%lu|%" PFHL "i|\r\n", ( void * ) ((uintptr_t)pFile->hFile), ( long unsigned int ) ulSize, llOffset );
#else
      nSend = sprintf( szData + HB_LENGTH_ACK, "D|%p|%lu|%" PFHL "i|\r\n", ( void * ) pFile->hFile, ( long unsigned int ) ulSize, llOffset );
#endif
      HB_PUT_BE_UINT32( szData, nSend );

      ulLen = hb_NetSingleSendRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1004 );
      if( ulLen )
      {
         char * ptrBuf;
         USHORT uiError;

         ptrBuf  = szBuffer + HB_LENGTH_ACK;
         ulRead  = ulLen - 3;
         uiError = ( USHORT ) HB_GET_BE_UINT32( ptrBuf );
         hb_fsSetError( uiError );
         if( ulRead )
            HB_MEMCPY( pBuffer, ptrBuf + 4, ( size_t ) ulRead );
      }
      else
      {
         ulRead = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }
   }
   else
      ulRead = hb_fsReadAt( pFile->hFile, pBuffer, ulSize, llOffset );

   return ulRead;
}

HB_SIZE hb_fileNetReadLarge( PHB_FILE pFile, void * pBuffer, HB_SIZE ulSize )
{
   HB_SIZE ulRead;

   if( pFile->hSocket )
   {
      char  szData[ 30 + HB_LENGTH_ACK ];
      ULONG ulLen;
      int   nSend;
#if defined(HB_ARCH_64BIT)                  
      nSend = sprintf( szData + HB_LENGTH_ACK, "K|%p|%lu|\r\n", ( void * ) ((uintptr_t)pFile->hFile), ( long unsigned int ) ulSize );
#else
      nSend = sprintf( szData + HB_LENGTH_ACK, "K|%p|%lu|\r\n", ( void * ) pFile->hFile, ( long unsigned int ) ulSize );
#endif      
      HB_PUT_BE_UINT32( szData, nSend );
      ulLen = hb_NetSingleSendRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1008 );
      if( ulLen )
      {
         char * ptrBuf;
         USHORT uiError;

         ptrBuf  = szBuffer + HB_LENGTH_ACK;
         ulRead  = ulLen - 3;
         uiError = ( USHORT ) HB_GET_BE_UINT32( ptrBuf );
         hb_fsSetError( uiError );
         if( ulRead )
            HB_MEMCPY( pBuffer, ptrBuf + 4, ( size_t ) ulRead );
      }
      else
      {
         ulRead = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }
   }
   else
      ulRead = hb_fsReadLarge( pFile->hFile, pBuffer, ulSize );

   return ulRead;
}

HB_SIZE hb_fileNetWriteAt( PHB_FILE pFile, const void * buffer, HB_SIZE ulSize, HB_FOFFSET llOffset )
{
   HB_SIZE ulWrite;

   if( pFile->hSocket )
   {
      char *   szData;
      char *   ptr;
      ULONG    ulLen;

      szData   = ( char * ) hb_xgrab( 40 + ulSize );
#if defined(HB_ARCH_64BIT)                  
      ulLen    = sprintf( szData + HB_LENGTH_ACK, "E|%p|%lu|%" PFHL "i|", ( void * ) ((uintptr_t)pFile->hFile), ( long unsigned int ) ulSize, llOffset );
#else      
      ulLen    = sprintf( szData + HB_LENGTH_ACK, "E|%p|%lu|%" PFHL "i|", ( void * ) pFile->hFile, ( long unsigned int ) ulSize, llOffset );
#endif      
      ptr      = szData + HB_LENGTH_ACK + ulLen;
      HB_MEMCPY( ptr, ( char * ) buffer, ( size_t ) ulSize );
      ptr      = ptr + ulSize;
      HB_MEMCPY( ptr, "\r\n", 2 );
      HB_PUT_BE_UINT32( szData, ulSize + ulLen + 2 );
      if( hb_NetSingleSendRecv( pFile->hSocket, szData, ulLen + ulSize + 2 + HB_LENGTH_ACK, 1005 ) )
      {
         USHORT uiError;

         ptr = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptr, szData ); ptr++;
         sscanf( szData, "%lu|", (unsigned long int *) &ulWrite );
         hb_NetGetCmdItem( &ptr, szData );
         sscanf( szData, "%hu|", &uiError );
         hb_fsSetError( uiError );
      }
      else
      {
         ulWrite = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }

      hb_xfree( szData );
   }
   else
      ulWrite = hb_fsWriteAt( pFile->hFile, buffer, ulSize, llOffset );

   return ulWrite;
}

HB_SIZE hb_fileNetWriteLarge( PHB_FILE pFile, const void * pBuffer, HB_SIZE ulSize )
{
   HB_SIZE ulWrite;

   if( pFile->hSocket )
   {
      char *   szData;
      char *   ptr;
      ULONG    ulLen;

      szData   = ( char * ) hb_xgrab( 36 + ulSize + HB_LENGTH_ACK );
#if defined(HB_ARCH_64BIT)                        
      ulLen    = sprintf( szData + HB_LENGTH_ACK, "J|%p|%lu|", ( void * ) ((uintptr_t)pFile->hFile), ( long unsigned int ) ulSize );      
#else      
      ulLen    = sprintf( szData + HB_LENGTH_ACK, "J|%p|%lu|", ( void * ) pFile->hFile, ( long unsigned int ) ulSize );      
#endif
      ptr      = szData + HB_LENGTH_ACK + ulLen;
      
      HB_MEMCPY( ptr, ( char * ) pBuffer, ( size_t ) ulSize );
      ptr      = ptr + ulSize;
      HB_MEMCPY( ptr, "\r\n", 2 );
      HB_PUT_BE_UINT32( szData, ulSize + ulLen + 2 );
      if( hb_NetSingleSendRecv( pFile->hSocket, szData, ulLen + ulSize + 2 + HB_LENGTH_ACK, 1009 ) )
      {
         char *   ptrs;
         USHORT   uiError;

         ptrs = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptrs, szData ); ptrs++;
         sscanf( szData, "%lu", (unsigned long int *) &ulWrite );
         hb_NetGetCmdItem( &ptrs, szData );
         sscanf( szData, "%hu", &uiError );
         hb_fsSetError( uiError );
      }
      else
      {
         ulWrite = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }

      hb_xfree( szData );
   }
   else
      ulWrite = hb_fsWriteLarge( pFile->hFile, pBuffer, ulSize );

   return ulWrite;
}

USHORT hb_fileNetWrite( PHB_FILE pFile, const char * pBuffer, USHORT uiCount )
{
   USHORT uiWrite;

   if( pFile->hSocket )
   {
      char *   szData;
      char *   ptr;
      ULONG    ulLen;

      szData   = ( char * ) hb_xgrab( 36 + uiCount + HB_LENGTH_ACK );
      #if defined(HB_ARCH_64BIT)            
         ulLen    = sprintf( szData + HB_LENGTH_ACK, "O|%p|%hu|", ( void * ) ((uintptr_t)pFile->hFile), uiCount );
      #else   
      ulLen    = sprintf( szData + HB_LENGTH_ACK, "O|%p|%hu|", ( void * ) pFile->hFile, uiCount );
      #endif
      ptr      = szData + HB_LENGTH_ACK + ulLen;
      HB_MEMCPY( ptr, ( char * ) pBuffer, uiCount );
      ptr      = ptr + uiCount;
      HB_MEMCPY( ptr, "\r\n", 2 );
      HB_PUT_BE_UINT32( szData, uiCount + ulLen + 2 );
      if( hb_NetSingleSendRecv( pFile->hSocket, szData, ulLen + uiCount + 2 + HB_LENGTH_ACK, 1012 ) )
      {
         char *   ptrs;
         USHORT   uiError;

         ptrs = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptrs, szData ); ptrs++;
         sscanf( szData, "%hu", &uiWrite );
         hb_NetGetCmdItem( &ptrs, szData );
         sscanf( szData, "%hu", &uiError );
         hb_fsSetError( uiError );
      }
      else
      {
         uiWrite = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }

      hb_xfree( szData );
   }
   else
      uiWrite = hb_fsWrite( pFile->hFile, pBuffer, uiCount );

   return uiWrite;
}

BOOL hb_fileNetTruncAt( PHB_FILE pFile, HB_FOFFSET llOffset )
{
   if( pFile->hSocket )
   {
      char  szData[ 30 + HB_LENGTH_ACK ];
      int   nSend;
      #if defined(HB_ARCH_64BIT)            
         nSend = sprintf( szData + HB_LENGTH_ACK, "F|%p|%" PFHL "i|\r\n", ( void * ) ((uintptr_t)pFile->hFile), llOffset );
      #else
         nSend = sprintf( szData + HB_LENGTH_ACK, "F|%p|%" PFHL "i|\r\n", ( void * ) pFile->hFile, llOffset );
      #endif
      HB_PUT_BE_UINT32( szData, nSend );
      if( hb_NetSingleSendSingleRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1006 ) )
         return strncmp( szBuffer, szOk, 2 ) == 0;
      else
         return FALSE;
   }
   else
      return hb_fsTruncAt( pFile->hFile, llOffset );
}

HB_FOFFSET hb_fileNetSeekLarge( PHB_FILE pFile, HB_FOFFSET llOffset, USHORT uiFlags )
{
   HB_FOFFSET llRet;

   if( pFile->hSocket )
   {
      char  szData[ 50 + HB_LENGTH_ACK ];
      ULONG nSend;
      #if defined(HB_ARCH_64BIT)      
      nSend = sprintf( szData + HB_LENGTH_ACK, "G|%p|%" PFHL "i|%hu|\r\n", ( void * ) ((uintptr_t)pFile->hFile), llOffset, uiFlags );
      #else
      nSend = sprintf( szData + HB_LENGTH_ACK, "G|%p|%" PFHL "i|%hu|\r\n", ( void * ) pFile->hFile, llOffset, uiFlags );
      #endif
      HB_PUT_BE_UINT32( szData, nSend );
      if( hb_NetSingleSendRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1007 ) )
      {
         char *   ptr;
         USHORT   uiError;

         ptr = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptr, szData ); ptr++;
         sscanf( szData, "%" PFHL "i", &llRet );
         hb_NetGetCmdItem( &ptr, szData );
         sscanf( szData, "%hu", &uiError );
         hb_fsSetError( uiError );
      }
      else
      {
         llRet = 0;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }
   }
   else
      llRet = hb_fsSeekLarge( pFile->hFile, llOffset, uiFlags );

   return llRet;
}

HB_FOFFSET hb_fileNetSeek( PHB_FILE pFile, LONG lOffset, USHORT uiFlags )
{
   return hb_fileNetSeekLarge( pFile, lOffset, uiFlags );
}

HB_FOFFSET hb_fileNetSize( PHB_FILE pFile )
{
   return hb_fileNetSeekLarge( pFile, 0, FS_END );
}

void hb_fileNetFlush( PHB_FILE pFile, BOOL fDirty )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( fDirty );
}

void hb_fileNetCommit( PHB_FILE pFile )
{
   if( pFile->hSocket )
   {
      char  szData[ 36 + HB_LENGTH_ACK ];
      int   nSend;

//       nSend = sprintf( szData + HB_LENGTH_ACK, "H|%p|\r\n", ( void * ) pFile->hFile );
  #if defined(HB_ARCH_64BIT)  
         nSend = sprintf( szData + HB_LENGTH_ACK, "H|%p|\r\n", ( void * ) ((uintptr_t) pFile->hFile ));
  #else
  
      nSend = sprintf( szData + HB_LENGTH_ACK, "H|%p|\r\n", ( void * ) pFile->hFile );
  #endif
      HB_PUT_BE_UINT32( szData, nSend );
      if( hb_NetSingleSendRecv( pFile->hSocket, szData, nSend + HB_LENGTH_ACK, 1008 ) )
      {
         char *   ptr;
         USHORT   uiError;

         ptr = hb_NetFirstChar();
         hb_NetGetCmdItem( &ptr, szData );
         sscanf( szData, "%hu|", &uiError );
         hb_fsSetError( uiError );
      }
      else
         hb_fsSetError( ( USHORT ) FS_ERROR );
   }
   else
      hb_fsCommit( pFile->hFile );
}

BOOL hb_fileNetDelete( const char * pFileName, USHORT uiRemote )
{
   if( s_fileAccept( pFileName ) )
      pFileName += NETIO_FILE_PREFIX_LEN;

   if( uiRemote == 1 )
      return hb_fsDelete( pFileName );
   else
   {
      char  szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
      int   nSend = sprintf( szData + HB_LENGTH_ACK, "I|%s|\r\n", pFileName );

      HB_PUT_BE_UINT32( szData, nSend );
      hb_NetSingleSendSingleRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1008 );
      return strncmp( szBuffer, szOk, 2 ) == 0;
   }
}

BOOL hb_fileNetRename( const char * pOldName, const char * pNewName )
{
   char  szData[ HB_PATH_MAX + HB_PATH_MAX + 6 + HB_LENGTH_ACK ];
   int   nSend;

   if( s_fileAccept( pOldName ) )
      pOldName += NETIO_FILE_PREFIX_LEN;
   if( s_fileAccept( pNewName ) )
      pNewName += NETIO_FILE_PREFIX_LEN;

   /* BOOL hb_fsRename( char * pOldName, char * pNewName ) */
   nSend = sprintf( szData + HB_LENGTH_ACK, "V|%s|%s|\r\n", pOldName, pNewName );
   HB_PUT_BE_UINT32( szData, nSend );
   hb_NetSingleSendSingleRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1019 );
   return strncmp( szBuffer, szOk, 2 ) == 0;
}

HB_FHANDLE hb_fileNetHandle( PHB_FILE pFile )
{
   return pFile ? pFile->hFile : FS_ERROR;
}

char * hb_fileNetFileName( PHB_FILE pFile )
{
   return pFile->pFileName;
}

USHORT hb_fileNetRemote( PHB_FILE pFile )
{
   return ( USHORT ) ( ( pFile && pFile->hSocket ) ? 2 : 1 );
}

PHB_FILE hb_fileNetCreateTemp( const char * pszDir, const char * pszPrefix, HB_FATTR ulAttr, char * pszName )
{
   PHB_FILE    pFile = NULL;
   HB_FHANDLE  hFile;

   hFile = hb_fsCreateTemp( pszDir, pszPrefix, ulAttr, pszName );
   if( hFile != FS_ERROR )
      pFile = hb_fileNetNew( hFile, FALSE, FALSE, FALSE, NULL );

   return pFile;
}

static BOOL hb_NetFileExists( const char * pszFileName )
{
   if( s_fileAccept( pszFileName ) )
      pszFileName += NETIO_FILE_PREFIX_LEN;

   if( hSocket )
   {
      char  szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
      int   nSend = sprintf( szData + HB_LENGTH_ACK, "M|%s|\r\n", pszFileName );
      HB_PUT_BE_UINT32( szData, nSend );
      hb_NetSingleSendSingleRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1011 );
      return strncmp( szBuffer, szOk, 2 ) == 0;
   }
   else
      return hb_fsFileExists( pszFileName );
}

BOOL hb_FileNetFile( char * pFileName )
{
   if( s_fileAccept( pFileName ) )
      pFileName += NETIO_FILE_PREFIX_LEN;

   if( hSocket )
   {
      char  szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
      int   nSend = sprintf( szData + HB_LENGTH_ACK, "N|%s|\r\n", pFileName );
      HB_PUT_BE_UINT32( szData, nSend );
      hb_NetSingleSendSingleRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1012 );
      return strncmp( szBuffer, szOk, 2 ) == 0;
   }
   else
      return hb_fsFile( pFileName );
}

static BOOL s_fileAccept( const char * pFileName )
{
   return hb_strnicmp( pFileName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN ) == 0;
}

BOOL hb_FileNetExists( const char * pFileName, char * pRetPath )
{
   char *      Path;
   BOOL        bIsFile = FALSE;
   PHB_FNAME   pFilepath;

   if( s_fileAccept( pFileName ) )
      pFileName += NETIO_FILE_PREFIX_LEN;

   if( pRetPath )
      Path = pRetPath;
   else
      Path = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( ( char * ) pFileName );

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( ( char * ) Path, pFilepath );
      bIsFile = hb_NetFileExists( ( const char * ) Path );
   }
   else
   {
      char * szDefault = ( char * ) hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( ( char * ) Path, pFilepath );
         bIsFile           = hb_NetFileExists( ( const char * ) Path );
      }

      if( ! bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * NextPath = hb_setGetFirstSetPath();

         while( bIsFile == FALSE && NextPath )
         {
            pFilepath->szPath = NextPath->szPath;
            hb_fsFNameMerge( ( char * ) Path, pFilepath );
            bIsFile           = hb_NetFileExists( ( const char * ) Path );
            NextPath          = NextPath->pNext;
         }
      }

      /*
       * This code is intentional. To eliminate race condition,
       * in pending hb_spCreate()/hb_spOpen() call when we have to know
       * real path and file name we have to set its deterministic value
       * here. If it's not necessary the caller may drop this value.
       */
      if( ! bIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : ( char * ) ".";
         hb_fsFNameMerge( ( char * ) Path, pFilepath );
      }
   }

   hb_xfree( pFilepath );

   if( pRetPath == NULL )
      hb_xfree( Path );

   return bIsFile;
}

PHB_FILE hb_fileNetCreateTempEx( char * pszName,
                                 const char * pszDir,
                                 const char * pszPrefix,
                                 const char * pszExt,
                                 HB_FATTR ulAttr )
{
   PHB_FILE    pFile = NULL;
   HB_FHANDLE  hFile;

   hFile = hb_fsCreateTempEx( pszName, pszDir, pszPrefix, pszExt, ulAttr );
   if( hFile != FS_ERROR )
      pFile = hb_fileNetNew( hFile, FALSE, FALSE, FALSE, NULL );

   return pFile;
}

PHB_NETFFIND hb_FileNetFindFirst( const char * pszFileName, HB_SIZE ulAttr )
{
   char           szData[ HB_PATH_MAX + 25 + HB_LENGTH_ACK ];
   int            nSend;
   HB_SIZE        ulLen, ulSize;
   PHB_NETFFIND   pffind = ( PHB_NETFFIND ) hb_xgrab( sizeof( HB_NETFFIND ) );

   if( s_fileAccept( pszFileName ) )
      pszFileName += NETIO_FILE_PREFIX_LEN;

   /* hb_fsFindFirst( const char * pszFileName, ULONG ulAttr ); */
   nSend = sprintf( szData + HB_LENGTH_ACK, "P|%s|%lu|\r\n", pszFileName, ( long unsigned int ) ulAttr );
   HB_PUT_BE_UINT32( szData, nSend );
   ulLen = hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1013 );
   if( ulLen )
   {
      char *   ptr;
      char *   ptrBuf;
      USHORT   uiError;

      ptrBuf   = hb_NetFirstChar();
      ptr      = hb_strToken( ptrBuf, ulLen, 1, &ulSize );
      sscanf( ptr, "%p|", &pffind->pNetffind );

      if( pffind->pNetffind )
      {
         ptr                        = hb_strToken( ptrBuf, ulLen, 2, &ulSize );
         sscanf( ptr, "%u", &pffind->attr );
         ptr                        = hb_strToken( ptrBuf, ulLen, 3, &ulSize );
         sscanf( ptr, "%" PFHL "i", &pffind->size );
         ptr                        = hb_strToken( ptrBuf, ulLen, 4, &ulSize );
         sscanf( ptr, "%lu", &pffind->lDate );
         ptr                        = hb_strToken( ptrBuf, ulLen, 5, &ulSize );
         HB_MEMCPY( pffind->szTime, ptr, 8 );
         pffind->szTime[ 8 ]        = '\0';
         ptr                        = hb_strToken( ptrBuf, ulLen, 6, &ulSize );
         sscanf( ptr, "%hu", &uiError );
         hb_fsSetError( uiError );
         ptr                        = hb_strToken( ptrBuf, ulLen, 7, &ulSize );
         HB_MEMCPY( pffind->szName, ptr, ( size_t ) ulSize );
         pffind->szName[ ulSize ]   = '\0';
      }
      else
      {
         ptr = hb_strToken( ptrBuf, ulLen, 2, &ulSize );
         sscanf( ptr, "%hu", &uiError );
         hb_fsSetError( uiError );

         hb_xfree( pffind );
         pffind = NULL;
      }
   }
   return pffind;
}

BOOL hb_FileNetFindNext( PHB_NETFFIND pffind )
{
   char     szData[ 20 + HB_LENGTH_ACK ];
   int      nSend;
   HB_SIZE  ulLen, ulSize;
   BOOL     bFound;

   /* hb_fsFindNext( PHB_FFIND pffind ); */
   nSend = sprintf( szData + HB_LENGTH_ACK, "Q|%p|\r\n", pffind->pNetffind );
   HB_PUT_BE_UINT32( szData, nSend );
   ulLen = hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1014 );
   if( ulLen )
   {
      char *   ptr;
      char *   ptrBuf;
      USHORT   uiError;

      ptrBuf                     = hb_NetFirstChar();
      ptr                        = hb_strToken( ptrBuf, ulLen, 1, &ulSize );
      sscanf( ptr, "%u", &pffind->attr );
      ptr                        = hb_strToken( ptrBuf, ulLen, 2, &ulSize );
      sscanf( ptr, "%" PFHL "i", &pffind->size );
      ptr                        = hb_strToken( ptrBuf, ulLen, 3, &ulSize );
      sscanf( ptr, "%lu", &pffind->lDate );
      ptr                        = hb_strToken( ptrBuf, ulLen, 4, &ulSize );
      HB_MEMCPY( pffind->szTime, ptr, 8 );
      pffind->szTime[ 8 ]        = '\0';
      ptr                        = hb_strToken( ptrBuf, ulLen, 5, &ulSize );
      bFound                     = ( ptr[ 0 ] == '1' ? TRUE : FALSE );
      ptr                        = hb_strToken( ptrBuf, ulLen, 6, &ulSize );
      sscanf( ptr, "%hu", &uiError );
      hb_fsSetError( uiError );
      ptr                        = hb_strToken( ptrBuf, ulLen, 7, &ulSize );
      HB_MEMCPY( pffind->szName, ptr, ( size_t ) ulSize );
      pffind->szName[ ulSize ]   = '\0';
   }
   else
      bFound = FALSE;

   return bFound;
}

void hb_FileNetFindClose( PHB_NETFFIND pffind )
{
   char  szData[ 14 + HB_LENGTH_ACK ];
   int   nSend = sprintf( szData + HB_LENGTH_ACK, "R|%p|\r\n", pffind->pNetffind );

   /* hb_fsFindClose( PHB_FFIND ffind ); */
   HB_PUT_BE_UINT32( szData, nSend );
   if( hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1015 ) )
   {
      char *   ptr = hb_NetFirstChar();
      USHORT   uiError;

      hb_NetGetCmdItem( &ptr, szData ); ptr++;
      sscanf( szData, "%hu|", &uiError );
      hb_fsSetError( uiError );
   }

   hb_xfree( ( void * ) pffind );
}

static void hb_FileNetGrabDirectory( PHB_ITEM pDir, const char * szDirSpec, USHORT uiMask, PHB_FNAME fDirSpec, BOOL bFullPath, BOOL bDirOnly )
{
   PHB_NETFFIND pffind;

   /* Get the file list */
   if( ( pffind = hb_FileNetFindFirst( ( const char * ) szDirSpec, uiMask ) ) != NULL )
   {
      do
      {
         if( ! ( ( ( uiMask & HB_FA_HIDDEN ) == 0 && ( pffind->attr & HB_FA_HIDDEN ) != 0 ) ||
                 ( ( uiMask & HB_FA_SYSTEM ) == 0 && ( pffind->attr & HB_FA_SYSTEM ) != 0 ) ||
                 ( ( uiMask & HB_FA_LABEL ) == 0 && ( pffind->attr & HB_FA_LABEL ) != 0 ) ||
                 ( ( uiMask & HB_FA_DIRECTORY ) == 0 && ( pffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
         {
            char  buffer[ 32 ];
            BOOL  bAddEntry = TRUE;
            HB_ITEM_NEW( Subarray );

            hb_arrayNew( &Subarray, 5 );

            if( bFullPath )
            {
               char * szFullName = hb_xstrcpy( NULL, fDirSpec->szPath ? fDirSpec->szPath : "", pffind->szName, NULL );
               hb_arraySetC( &Subarray, F_NAME, szFullName );
               hb_xfree( szFullName );
            }
            else
               hb_arraySetC( &Subarray, F_NAME, pffind->szName );

            hb_arraySetNInt( &Subarray, F_SIZE, pffind->size );
            hb_arraySetDL( &Subarray, F_DATE, pffind->lDate );
            hb_arraySetC( &Subarray, F_TIME, pffind->szTime );
            hb_arraySetC( &Subarray, F_ATTR, hb_fsAttrDecode( pffind->attr, buffer ) );

            if( bDirOnly )
               bAddEntry = ( ( pffind->attr & HB_FA_DIRECTORY ) == HB_FA_DIRECTORY );

            if( bAddEntry )
               hb_arrayAddForward( pDir, &Subarray );
            else
               hb_itemClear( &Subarray );
         }
      }
      while( hb_FileNetFindNext( pffind ) );

      hb_FileNetFindClose( pffind );

   }
}

void hb_FileNetDirectory( PHB_ITEM pDir, const char * szSkleton, const char * szAttributes, BOOL bDirOnly, BOOL bFullPath )
{
   PHB_FNAME fDirSpec = NULL;
   USHORT    uiMask, uiMaskNoLabel;
   char *    szDirSpec;
   char *    pszFree  = NULL;

   /* Get the passed attributes and convert them to Harbour Flags */

   uiMask = HB_FA_ARCHIVE
            | HB_FA_READONLY
            | HB_FA_NORMAL
            | HB_FA_DEVICE
            | HB_FA_TEMPORARY
            | HB_FA_SPARSE
            | HB_FA_REPARSE
            | HB_FA_COMPRESSED
            | HB_FA_OFFLINE
            | HB_FA_NOTINDEXED
            | HB_FA_ENCRYPTED
            | HB_FA_VOLCOMP;

   uiMaskNoLabel = uiMask;

   hb_arrayNew( pDir, 0 );

   if( bDirOnly )
      szAttributes = "D";

   if( szAttributes && strlen( szAttributes ) > 0 )
   {
      if( ( uiMask |= ( USHORT ) hb_fsAttrEncode( szAttributes ) ) & HB_FA_LABEL )
      {
         /* NOTE: This is Clipper Doc compatible. (not operationally) */
         uiMask = HB_FA_LABEL;
      }
   }

   if( szSkleton && strlen( szSkleton ) > 0 )
      szDirSpec = ( char * ) hb_fsNameConv( ( char * ) szSkleton, &pszFree );
   else
      szDirSpec = ( char * ) HB_OS_ALLFILE_MASK;

   if( bDirOnly || bFullPath )
   {
      if( ( fDirSpec = hb_fsFNameSplit( ( char * ) szDirSpec ) ) != NULL )
      {
         if( fDirSpec->szDrive )
            hb_fsChDrv( ( BYTE ) ( fDirSpec->szDrive[ 0 ] - 'A' ) );

         if( fDirSpec->szPath )
            hb_fsChDir( ( char * ) fDirSpec->szPath );
      }
   }

   /* Get the file list */
   hb_FileNetGrabDirectory( pDir, ( const char * ) szDirSpec, uiMask, fDirSpec, bFullPath, bDirOnly );

   if( uiMask == HB_FA_LABEL )
   {
      uiMaskNoLabel |= ( USHORT ) hb_fsAttrEncode( szAttributes );
      uiMaskNoLabel &= ~HB_FA_LABEL;
      hb_FileNetGrabDirectory( pDir, ( const char * ) szDirSpec, uiMaskNoLabel, fDirSpec, bFullPath, bDirOnly );
   }

   if( fDirSpec != NULL )
      hb_xfree( fDirSpec );

   if( pszFree )
      hb_xfree( pszFree );
}

HB_SIZE hb_fileNetGetFileAttributes( char * pFilename )
{
   char  szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
   int   nSend = sprintf( szData + HB_LENGTH_ACK, "S|%s|\r\n", pFilename );
   ULONG ulLen;

   /* BOOL hb_fsGetFileAttributes( char * pFilename ) */
   HB_PUT_BE_UINT32( szData, nSend );
   ulLen = hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1016 );
   if( ulLen )
   {
      HB_SIZE  ulFileAttributes, ulSize;
      char *   ptrBuf   = hb_NetFirstChar();
      char *   ptr      = hb_strToken( ptrBuf, ulLen, 1, &ulSize );

      sscanf( ptr, "%lu", (unsigned long int *) &ulFileAttributes );
      return ulFileAttributes;
   }
   else
      return 0;
}

BOOL hb_fileNetMkDir( char * pPath )
{
   char     szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
   int      nSend = sprintf( szData + HB_LENGTH_ACK, "T|%s|\r\n", pPath );
   HB_SIZE  ulSize, ulLen;

   /* BOOL hb_fsMkDir( char * pPath ) */
   HB_PUT_BE_UINT32( szData, nSend );
   ulLen = hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1017 );

   if( ulLen )
   {
      USHORT   uiError;
      char *   ptrBuf   = hb_NetFirstChar();
      char *   ptr      = hb_strToken( ptrBuf, ulLen, 1, &ulSize );

      sscanf( ptr, "%hu", &uiError );
      hb_fsSetError( uiError );

      ptr = hb_strToken( ptrBuf, ulLen, 2, &ulSize );
      return ptr[ 0 ] == '1' ? TRUE : FALSE;
   }
   else
      return FALSE;
}

BOOL hb_fileNetRmDir( char * pPath )
{
   char     szData[ HB_PATH_MAX + 5 + HB_LENGTH_ACK ];
   int      nSend = sprintf( szData + HB_LENGTH_ACK, "U|%s|\r\n", pPath );
   HB_SIZE  ulSize, ulLen;

   /* BOOL hb_fsRmDir( char * pPath ) */
   HB_PUT_BE_UINT32( szData, nSend );
   ulLen = hb_NetSingleSendRecv( hSocket, szData, nSend + HB_LENGTH_ACK, 1018 );

   if( ulLen )
   {
      USHORT   uiError;
      char *   ptrBuf   = hb_NetFirstChar();
      char *   ptr      = hb_strToken( ptrBuf, ulLen, 1, &ulSize );

      sscanf( ptr, "%hu", &uiError );
      hb_fsSetError( uiError );

      ptr = hb_strToken( ptrBuf, ulLen, 2, &ulSize );
      return ptr[ 0 ] == '1' ? TRUE : FALSE;
   }
   else
      return FALSE;
}

static void blockeval( EVALINFO info, PHB_ITEM block, HB_SIZE count )
{
   if( hb_itemType( block ) == HB_IT_BLOCK )
   {
      HB_ITEM_NEW( Count );

      hb_evalPutParam( &info, hb_itemPutNL( &Count, ( const LONG ) count ) );

      hb_itemRelease( hb_evalLaunch( &info ) );
   }

}

static BOOL hb_FileNetCopyTo( const char * szSource, const char * szDest, PHB_ITEM block )
{
   BOOL     bRetVal = FALSE;
   FHANDLE  fhndSource;
   EVALINFO info = { 0, { 0 } };

   HB_TRACE( HB_TR_DEBUG, ( "hb_FileNetCopyTo(%s, %s)", szSource, szDest ) );

   while( ( fhndSource = hb_spOpen( ( char * ) szSource, FO_READ | FO_SHARED | FO_PRIVATE ) ) == FS_ERROR )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2012, NULL, szSource, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( fhndSource != FS_ERROR )
   {
      PHB_FILE pfDest;

      for(;; )
      {
         pfDest = hb_fileNetExtOpen( ( char * ) szDest, NULL,
                                     FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE | FXO_SHARELOCK | FXO_COPYNAME,
                                     NULL, NULL, FALSE );
         if( pfDest )
            break;
         else
         {
            USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );
            if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               break;
         }
      }

      if( pfDest )
      {
/*
   #if defined(HB_OS_UNIX)
         struct stat struFileInfo;
         int iSuccess = fstat( fhndSource, &struFileInfo );
   #elif ( defined( HB_OS_WIN ) || defined( __MINGW32__ ) ) && !defined( __CYGWIN__ )
         BY_HANDLE_FILE_INFORMATION hFileInfo;
         BOOL bSuccess = GetFileInformationByHandle( (HANDLE) fhndSource, &hFileInfo);
   #endif
 */
         char *   buffer = ( char * ) hb_xgrab( BUFFER_SIZE );
         HB_SIZE  ulRead;

         bRetVal  = TRUE;

         if( block )
            hb_evalNew( &info, block );

         while( ( ulRead = hb_fsReadLarge( fhndSource, buffer, BUFFER_SIZE ) ) != 0 )
         {
            while( hb_fileNetWriteLarge( pfDest, buffer, ulRead ) != ulRead )
            {
               USHORT uiAction = hb_errRT_BASE_Ext1( EG_WRITE, 2016, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

               if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               {
                  bRetVal = FALSE;
                  break;
               }
            }

            if( block )
               blockeval( info, block, ulRead );
         }

         hb_xfree( buffer );

         if( block )
            hb_evalRelease( &info );

         hb_fileNetClose( pfDest );
/*
   #if ( defined( HB_OS_WIN ) || defined( __MINGW32__ ) ) && !defined( __CYGWIN__ )
         if( bSuccess )
         {
            SetFileAttributes( (LPCSTR) szSource, hFileInfo.dwFileAttributes );
         }
   #endif
 */
      }

      hb_fsClose( fhndSource );
   }

   return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */
HB_FUNC( NET_COPYTO )
{
   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      if( hb_FileNetCopyTo( hb_parcx( 1 ), hb_parcx( 2 ), ISBLOCK( 3 ) ? hb_itemNew( hb_param( 3, HB_IT_BLOCK ) ) : NULL ) )
         hb_retl( TRUE );
      else
         hb_retl( FALSE );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2010, NULL, "NET_COPYTO", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      /* NOTE: Undocumented but existing Clipper Run-time error */
   }
}

static BOOL hb_FileNetCopyFrom( const char * szSource, const char * szDest, PHB_ITEM block )
{
   BOOL     bRetVal = FALSE;
   PHB_FILE pfSource;
   EVALINFO info = { 0, { 0 } };

   HB_TRACE( HB_TR_DEBUG, ( "hb_FileNetCopyFrom(%s, %s)", szSource, szDest ) );

   for(;; )
   {
      pfSource = hb_fileNetExtOpen( ( char * ) szSource, NULL,
                                    FO_READ | FO_SHARED | FXO_SHARELOCK | FXO_COPYNAME,
                                    NULL, NULL, FALSE );
      if( pfSource )
         break;
      else
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2012, NULL, szSource, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }

   if( pfSource )
   {
      HB_FHANDLE fhndDest;

      while( ( fhndDest = hb_fsCreate( ( char * ) szDest, FC_NORMAL ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhndDest != FS_ERROR )
      {
         HB_SIZE  ulRead;
         char*    buffer   = ( char * ) hb_xgrab( BUFFER_SIZE );

         bRetVal  = TRUE;

         if( block )
            hb_evalNew( &info, block );

         while( ( ulRead = hb_fileNetReadLarge( pfSource, buffer, BUFFER_SIZE ) ) != 0 )
         {
            while( hb_fsWriteLarge( fhndDest, buffer, ulRead ) != ulRead )
            {
               USHORT uiAction = hb_errRT_BASE_Ext1( EG_WRITE, 2016, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

               if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               {
                  bRetVal = FALSE;
                  break;
               }
            }

            if( block )
               blockeval( info, block, ulRead );
         }

         hb_xfree( buffer );

         if( block )
            hb_evalRelease( &info );

         hb_fsClose( fhndDest );
      }

      hb_fileNetClose( pfSource );
   }

   return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */
HB_FUNC( NET_COPYFROM )
{
   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      if( hb_FileNetCopyFrom( hb_parcx( 1 ), hb_parcx( 2 ), ISBLOCK( 3 ) ? hb_itemNew( hb_param( 3, HB_IT_BLOCK ) ) : NULL ) )
         hb_retl( TRUE );
      else
         hb_retl( FALSE );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2010, NULL, "NET_COPYFROM", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      /* NOTE: Undocumented but existing Clipper Run-time error */
   }
}

static BOOL hb_FileNetCopyFile( const char * szSource, const char * szDest, PHB_ITEM block )
{
   BOOL     bRetVal = FALSE;
   PHB_FILE pfSource;
   EVALINFO info = { 0, { 0 } };

   HB_TRACE( HB_TR_DEBUG, ( "hb_FileNetCopyFile(%s, %s)", szSource, szDest ) );

   for(;; )
   {
      pfSource = hb_fileNetExtOpen( ( char * ) szSource, NULL,
                                    FO_READ | FO_SHARED | FXO_SHARELOCK | FXO_COPYNAME,
                                    NULL, NULL, FALSE );
      if( pfSource )
         break;
      else
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2012, NULL, szSource, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }

   if( pfSource )
   {
      PHB_FILE pfDest;

      for(;; )
      {
         pfDest = hb_fileNetExtOpen( ( char * ) szDest, NULL,
                                     FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE | FXO_SHARELOCK | FXO_COPYNAME,
                                     NULL, NULL, FALSE );
         if( pfDest )
            break;
         else
         {
            USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

            if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               break;
         }
      }

      if( pfDest )
      {
         HB_SIZE  ulRead;
         char*    buffer   = ( char * ) hb_xgrab( BUFFER_SIZE );

         bRetVal  = TRUE;

         if( block )
            hb_evalNew( &info, block );

         while( ( ulRead = hb_fileNetReadLarge( pfSource, buffer, BUFFER_SIZE ) ) != 0 )
         {
            while( hb_fileNetWriteLarge( pfDest, buffer, ulRead ) != ulRead )
            {
               USHORT uiAction = hb_errRT_BASE_Ext1( EG_WRITE, 2016, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

               if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               {
                  bRetVal = FALSE;
                  break;
               }
            }

            if( block )
               blockeval( info, block, ulRead );
         }

         hb_xfree( buffer );

         if( block )
            hb_evalRelease( &info );

         hb_fileNetClose( pfDest );
      }

      hb_fileNetClose( pfSource );
   }

   return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */
HB_FUNC( NET_COPYFILE )
{
   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      if( hb_FileNetCopyFile( hb_parcx( 1 ), hb_parcx( 2 ), ISBLOCK( 3 ) ? hb_itemNew( hb_param( 3, HB_IT_BLOCK ) ) : NULL ) )
         hb_retl( TRUE );
      else
         hb_retl( FALSE );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2010, NULL, "NET_COPYFILE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      /* NOTE: Undocumented but existing Clipper Run-time error */
   }
}

PHB_FILE hb_fileNetGetFileToTemp( PHB_FILE pDataFile, char * pszFileName )
{
   HB_SIZE  ulRead;
   PHB_FILE pDataTemp   = hb_fileNetCreateTempEx( pszFileName, NULL, NULL, NULL, FC_TEMPORARY );
   char *   buffer      = ( char * ) hb_xgrab( BUFFER_SIZE );

   hb_fileNetSeekLarge( pDataFile, 0, SEEK_SET );
   while( ( ulRead = hb_fileNetReadLarge( pDataFile, buffer, BUFFER_SIZE ) ) != 0 )
   {
      while( hb_fileNetWriteLarge( pDataTemp, buffer, ulRead ) != ulRead )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_WRITE, 2016, NULL, ( char * ) hb_fileNetFileName( pDataFile ), hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );
         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }
   hb_xfree( buffer );
   return pDataTemp;
}

HB_FUNC( NET_FRENAME )
{
   USHORT uiError = 3;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      hb_retni( hb_fileNetRename( ( char * ) hb_parc( 1 ), ( char * ) hb_parc( 2 ) ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( NET_FERASE )
{
   USHORT uiError = 3;

   if( ISCHAR( 1 ) )
   {
      hb_retni( hb_fileNetDelete( ( char * ) hb_parc( 1 ), 2 ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( NET_MAKEDIR )
{
   USHORT uiErrorOld = hb_fsError();

   if( ISCHAR( 1 ) )
      hb_retni( hb_fileNetMkDir( ( char * ) hb_parcx( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( -1 );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC( NET_DIRREMOVE )
{
   USHORT uiErrorOld = hb_fsError();

   if( ISCHAR( 1 ) )
      hb_retni( hb_fileNetRmDir( ( char * ) hb_parcx( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( -1 );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC( NET_FILEATTR )
{
   hb_retnl( ( LONG ) hb_fileNetGetFileAttributes( ( char * ) hb_parcx( 1 ) ) );
}

HB_FUNC( NET_DIRECTORY )
{
   PHB_ITEM pDir = hb_itemNew( NULL );

   hb_FileNetDirectory( pDir, hb_parcx( 1 ), hb_parcx( 2 ), hb_parl( 3 ), hb_parl( 4 ) );
   hb_itemRelease( hb_itemReturnForward( pDir ) );
}

HB_FUNC( NET_OPENCONNECTION )
{
   hb_ipInit();
   hb_NetOpenConnection( hb_parc( 1 ), hb_parni( 2 ) );
   if( hSocket )
      #if defined(HB_ARCH_64BIT)           
         hb_retptr( ( void * ) ((uintptr_t)hSocket ));
      #else         
      hb_retptr( ( void * ) hSocket );
      #endif
   else
      hb_retnl( 0 );
}

HB_FUNC( NET_CLOSECONNECTION )
{
   hb_NetCloseConnection( ( HB_SOCKET_T ) hb_parptr( 1 ) );
}

HB_FUNC( NET_SETBUFFERSIZE )
{
   if( hb_parni( 1 ) )
      sBufferSize = hb_parni( 1 );

   hb_retni( sBufferSize );
}

static const HB_FILE_FUNCS * hb_fileNetMethods( void )
{
   static const HB_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,
      hb_FileNetExists,
      hb_fileNetDelete,
      hb_fileNetRename,
      hb_fileNetExtOpen,
      hb_fileNetClose,
      hb_fileNetLock,
      hb_fileNetReadAt,
      hb_fileNetWriteAt,
      hb_fileNetTruncAt,
      hb_fileNetSize,
      hb_fileNetSeekLarge,
      hb_fileNetWriteLarge,
      hb_fileNetReadLarge,
      hb_fileNetFlush,
      hb_fileNetCommit,
      hb_fileNetHandle
   };

   return &s_fileFuncs;
}

static int s_iFileInit = 0;
static void hb_fileNet_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_iFileInit )
   {
      if ( szBuffer )
         hb_xfree( szBuffer ) ;
      s_iFileInit = 0;
   }
}

static void hb_fileNet_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hb_threadLockInit( S_FILENETMTX );

   if( lBufferLen == 0 )
   {
      lBufferLen  = HB_SENDRECV_BUFFER_SIZE;
      szBuffer    = ( char * ) hb_xalloc( lBufferLen );
      hb_xautorelease( ( void * ) szBuffer );
      hb_fileRegister( hb_fileNetMethods() );
      hb_vmAtQuit( hb_fileNet_exit, NULL );      
      s_iFileInit = 1;
      
   }
}

HB_CALL_ON_STARTUP_BEGIN( _hb_fileNet_init_ )
hb_vmAtInit( hb_fileNet_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_fileNet_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_fileNet_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( _hb_fileNet_init_ )
   #include "hbiniseg.h"
#endif
