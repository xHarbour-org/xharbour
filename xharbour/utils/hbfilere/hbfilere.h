/*  $Id$  */

/*
 * xHarbour Project source code:
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

#include "hbapirdd.h"
#include "hbipapi.h"

#if defined( HB_OS_WIN )
   #include "ws2spi.h"
   #if !defined( __BORLANDC__ )
      #include "mswsock.h"
   #endif
#endif

#include "hbapi.h"
#include "hbvm.h"
#ifdef __XHARBOUR__
   #include "hbfast.h"
#else
   #include "hbapicls.h"
#endif
#include "hbapifs.h"
#ifdef __CONSOLE__
   #include "hbset.h"
#endif
#include "sys/stat.h"

#if !defined( HB_OS_WIN )
   #include <sys/types.h>
   #include <stdio.h>
   #include <stdlib.h>
   #include <fcntl.h>
   #include <errno.h>
   #include <unistd.h>
   #include <syslog.h>
   #include <string.h>
   #include <assert.h>
   #include <signal.h>
#endif
BOOL hb_ip_rfd_isset( HB_SOCKET_T hSocket );
int hb_ipRecv( HB_SOCKET_T hSocket, char * szBuffer, int iBufferLen );
void hb_ip_rfd_set( HB_SOCKET_T hSocket );

#if defined( HB_OS_WIN )
   #define HB_SOCKET_T SOCKET
#else
   #define HB_SOCKET_T int
   typedef void* HANDLE;
#endif 

#define USERS_REALLOC       20

typedef struct _HB_FILE
{
   char *            pFileName;
   int               used;
   BOOL              shared;
   HB_FHANDLE        hFile;
   struct _HB_FILE * pNext;
   struct _HB_FILE * pPrev;
   HANDLE            hMap;
   BYTE *            pView;
}
HB_FILE;

typedef struct
{
   HB_SOCKET_T hSocket;
   BYTE *      pBuffer;
   ULONG       ulBufferLen;
   BYTE *      pBufAnswer;
   ULONG       ulBufAnswerLen;
   BYTE *      pBufTemp;
   ULONG       ulBufTempLen;
   BYTE *      pBufRead;
   ULONG       ulDataLen;
   ULONG       ulDataRead;
   BYTE *      szAddr;
   PHB_FILE    s_openFiles;
} USERSTRU, *PUSERSTRU;
