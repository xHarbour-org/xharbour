/*
 * $Id: filestat.c,v 1.1 2004/02/11 15:09:45 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * FILESTATS() function
 *
 * Copyright 2004 Giancarlo Niccolai <gc -at- niccolai [dot] ws>
 *
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hbapierr.h"

#if defined( OS_UNIX_COMPATIBLE )
   #include <unistd.h>
   #include <sys/types.h>
   #include <errno.h>
   #include <time.h>
   #include <sys/types.h>
   #include <sys/stat.h>
#elif defined( HB_OS_WIN_32 )
   #include <windows.h>
   #if defined(__BORLANDC__) && ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES ((DWORD)(-1))
   #endif
#endif

BOOL hb_fsFileStats(
   BYTE *pszFileName,
   BYTE *pszAttr,
#ifndef HB_LONG_LONG_OFF
   LONGLONG *llSize,
#else
   LONG *llSize,
#endif
   LONG *lcDate,
   LONG *lcTime,
   LONG *lmDate,
   LONG *lmTime )
{
   #if defined( OS_UNIX_COMPATIBLE )
   struct stat statbuf;

   if( stat( ( char * ) pszFileName, &statbuf ) == 0 )
   {
      // determine if we can read/write/execute the file
      USHORT usAttr, ushbAttr = 0;
      time_t ftime;
      #if _POSIX_C_SOURCE >= 199506L
      struct tm tms;
      #endif
      struct tm *ptms;

      errno = 0;
      hb_fsSetError( 0 );

      /* See which attribs are applicable */
      if ( statbuf.st_uid == geteuid() )
      {
         usAttr =
            ((statbuf.st_mode & S_IRUSR ) ? 1 << 2 : 0) |
            ((statbuf.st_mode & S_IWUSR ) ? 1 << 1 : 0) |
            ((statbuf.st_mode & S_IXUSR ) ? 1 : 0);
      }
      else if ( statbuf.st_gid == getegid() )
      {
         usAttr =
            ((statbuf.st_mode & S_IRGRP ) ? 1 << 2 : 0) |
            ((statbuf.st_mode & S_IWGRP ) ? 1 << 1 : 0) |
            ((statbuf.st_mode & S_IXGRP ) ? 1 : 0);
      }
      else
      {
         usAttr =
            ((statbuf.st_mode & S_IROTH ) ? 1 << 2 : 0) |
            ((statbuf.st_mode & S_IWOTH ) ? 1 << 1 : 0) |
            ((statbuf.st_mode & S_IXOTH ) ? 1 : 0);
      }

      /* Standard characters */
      if ( (usAttr & 4) == 0 ) /* Hidden (can't read)*/
      {
         ushbAttr |= HB_FA_HIDDEN;
      }

      if ( (usAttr & 2) == 0 ) /* read only (can't write)*/
      {
         ushbAttr |= HB_FA_READONLY;
      }

      if ( (usAttr & 1) == 1 ) /* executable?  (xbit)*/
      {
         ushbAttr |= HB_FA_SYSTEM;
      }

      /* Extension characters */
      #ifdef HB_EXTENSION
      if ( ( statbuf.st_mode & S_IFLNK ) == S_IFLNK)
      {
         *pszAttr++ = 'Z'; /* Xharbour extension */
      }
      if ( ( statbuf.st_mode & S_IFSOCK ) == S_IFSOCK )
      {
         *pszAttr++ = 'K'; /* Xharbour extension */
      }
      /* device */
      if ( ( statbuf.st_mode & S_IFBLK ) == S_IFBLK ||
            ( statbuf.st_mode & S_IFCHR ) == S_IFCHR )
      {
         ushbAttr |= HB_FA_DEVICE; /* Xharbour extension */
      }
      if ( ( statbuf.st_mode & S_IFIFO ) == S_IFIFO )
      {
         *pszAttr++ = 'Y'; /* Xharbour extension */
      }
      #endif

      if ( S_ISDIR( statbuf.st_mode ) )
      {
         ushbAttr |= HB_FA_DIRECTORY; /* Xharbour extension */
      }
      /* Give the ARCHIVE if readwrite, not executable and not special */
      else if ( S_ISREG( statbuf.st_mode ) && ushbAttr == 0 )
      {
         ushbAttr |= HB_FA_ARCHIVE;
      }

      #ifdef HB_LONG_LONG_OFF
      *llSize = (LONG) statbuf.st_size;
      #else
      *llSize = (LONGLONG) statbuf.st_size;
      #endif

      ftime = statbuf.st_mtime;
      #if _POSIX_C_SOURCE >= 199506L
      ptms = localtime_r( &ftime, &tms );
      #else
      ptms = localtime( &ftime );
      #endif

      *lcDate = hb_dateEncode( ptms->tm_year + 1900,
               ptms->tm_mon + 1, ptms->tm_mday );
      *lcTime = ptms->tm_hour*3600 + ptms->tm_min * 60 + ptms->tm_sec;

      ftime = statbuf.st_atime;
      #if _POSIX_C_SOURCE >= 199506L
      ptms = localtime_r( &ftime, &tms );
      #else
      ptms = localtime( &ftime );
      #endif
      *lmDate = hb_dateEncode( ptms->tm_year + 1900,
               ptms->tm_mon + 1, ptms->tm_mday );
      *lmTime = ptms->tm_hour*3600 + ptms->tm_min * 60 + ptms->tm_sec;

      hb_fsAttrDecode( ushbAttr, pszAttr );

      return TRUE;
   }
   else
   {
      return FALSE;
   }

   #elif defined( HB_OS_WIN_32 )
   DWORD dwAttribs;
   WIN32_FIND_DATA ffind;
   HANDLE hFind;
   FILETIME filetime;
   SYSTEMTIME time;

   /* Get attributes... */
   dwAttribs = GetFileAttributes( pszFileName );
   if ( dwAttribs == INVALID_FILE_ATTRIBUTES )
   {
      /* return */
      return FALSE;
   }

   hb_fsAttrDecode( hb_fsAttrFromRaw( dwAttribs ), pszAttr );

   /* If file existed, do a findfirst */
   hFind = FindFirstFile( pszFileName, &ffind );
   if ( hFind == INVALID_HANDLE_VALUE )
   {
      return FALSE;
   }
   CloseHandle( hFind );

   /* get file times and work them out */
   #ifdef HB_LONG_LONG_OFF
   *llSize = ffind.nFileSizeLow;
   #else
   *llSize = ffind.nFileSizeLow + ( ((LONGLONG)ffind.nFileSizeHigh << 32 ));
   #endif

   if ( FileTimeToLocalFileTime( &ffind.ftCreationTime, &filetime ) &&
        FileTimeToSystemTime( &filetime, &time ) )
   {
      *lcDate = hb_dateEncode( time.wYear, time.wMonth, time.wDay );
      *lcTime = time.wHour * 3600 + time.wMinute * 60 + time.wSecond;
   }
   else {
      *lcDate = hb_dateEncode( 0, 0, 0 );
      *lcTime = 0;
   }

   if ( FileTimeToLocalFileTime( &ffind.ftLastAccessTime, &filetime ) &&
        FileTimeToSystemTime( &filetime, &time ) )
   {
      *lmDate = hb_dateEncode( time.wYear, time.wMonth, time.wDay );
      *lmTime = time.wHour * 3600 + time.wMinute * 60 + time.wSecond;
   }
   else {
      *lcDate = hb_dateEncode( 0, 0, 0 );
      *lcTime = 0;
   }
   return TRUE;

   #else
   PHB_FFIND findinfo;
   /* Generic algorithm based on findfirst */
   findinfo = hb_fsFindFirst( pszFileName, HB_FA_ALL );
   if ( ! findinfo )
   {
      return FALSE;
   }

   hb_fsAttrDecode( findinfo->attr, pszAttr );
   *llSize = findinfo->size;
   *lcDate = findinfo->lDate;
   *lcTime = (findinfo->szTime[0] - '0') * 36000 +
             (findinfo->szTime[1] - '0') * 3600 +
             (findinfo->szTime[3] - '0') * 600 +
             (findinfo->szTime[4] - '0') * 60 +
             (findinfo->szTime[6] - '0') * 10 +
             (findinfo->szTime[7] - '0');
   *lmDate = hb_dateEncode( 0, 0, 0 );
   *lmTime = 0;

   hb_fsFindClose( findinfo );

   return TRUE;
   #endif
}

#ifdef HB_EXTENSION
HB_FUNC( FILESTATS )
{
   PHB_ITEM pFileName = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pAttr  = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pSize  = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pCDate = hb_param( 4, HB_IT_BYREF );
   PHB_ITEM pCTime = hb_param( 5, HB_IT_BYREF );
   PHB_ITEM pMDate = hb_param( 6, HB_IT_BYREF );
   PHB_ITEM pMTime = hb_param( 7, HB_IT_BYREF );

   BYTE szAttr[21];
#ifndef HB_LONG_LONG_OFF
   LONGLONG llSize;
#else
   LONG llSize;
#endif
   LONG lcDate;
   LONG lcTime;
   LONG lmDate;
   LONG lmTime;

   /* Parameter checking */
   if ( pFileName == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "FILESTATS", 1,
            hb_paramError(1) );
      return;
   }

   if ( hb_fsFileStats( hb_itemGetCPtr( pFileName ),
            szAttr, &llSize, &lcDate, &lcTime, &lmDate, &lmTime ) )
   {
      if ( pAttr != NULL )
      {
         hb_itemPutC( pAttr, szAttr );
      }
      if ( pSize != NULL )
      {
         #ifndef HB_LONG_LONG_OFF
         hb_itemPutNLL( pSize, llSize );
         #else
         hb_itemPutNL( pSize, llSize );
         #endif
      }
      if ( pCDate != NULL )
      {
         hb_itemPutDL( pCDate, lcDate );
      }
      if ( pCTime != NULL )
      {
         hb_itemPutNI( pCTime, lcTime );
      }
      if ( pMDate != NULL )
      {
         hb_itemPutDL( pMDate, lmDate );
      }
      if ( pMTime != NULL )
      {
         hb_itemPutNI( pMTime, lmTime );
      }

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

#endif
