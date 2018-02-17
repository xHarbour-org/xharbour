/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour File Find API (C level)
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
 *                     Viktor Szakats <viktor.szakats@syenar.hu>
 *                     Paul Tucker <ptucker@sympatico.ca>
 * www - http://www.harbour-project.org
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#define _HB_FFIND_INTERNAL_
#define INCL_DOSFILEMGR
#define INCL_DOSERRORS


#include "hbapi.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hb_io.h"
#include "hbtrace.h"

/* ------------------------------------------------------------- */

#include <ctype.h>

#if defined( HB_OS_DOS )

   #if defined( __DJGPP__ ) || defined( __RSX32__ )
      #include <sys/param.h>
   #endif

   #if defined( __DJGPP__ ) || defined( __RSX32__ ) || defined( __BORLANDC__ ) || defined( __WATCOMC__ )
      #include <sys/stat.h>
   #endif

   #include <dos.h>

   #if defined( __WATCOMC__ )
       #include <io.h>
   #else
       #include <dir.h>
   #endif

   #include <time.h>

typedef struct
{

   #if defined( __WATCOMC__ )
   struct _finddata_t entry;
   LONG hHandle;
   #else
   struct ffblk entry;
   #endif

} HB_FFIND_INFO, * PHB_FFIND_INFO;

/* Needed For WatCom */
    #if ! defined( FA_ARCH )
        #define FA_NORMAL  _A_NORMAL
        #define FA_RDONLY  _A_RDONLY
        #define FA_HIDDEN  _A_HIDDEN
        #define FA_SYSTEM  _A_SYSTEM
        #define FA_SUBDIR  _A_SUBDIR
        #define FA_ARCH    _A_ARCH
        #define FA_DIREC   16
        #define FA_LABEL   8
    #endif

#elif defined( HB_OS_OS2 )

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <time.h>

typedef struct
{
   HDIR hFindFile;
   FILEFINDBUF3 entry;
   ULONG fileTypes;
   ULONG findSize;
   ULONG findCount;
} HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined( HB_OS_WIN )
#include "windows.h"

typedef struct
{
   HANDLE hFindFile;
   WIN32_FIND_DATA pFindFileData;
   DWORD dwAttr;
   DWORD dwExAttr;
   char szVolInfo[ MAX_PATH ];
   FILETIME fFileTime;

} HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined( HB_OS_UNIX )

   #ifndef __USE_BSD
      #define __USE_BSD
   #endif
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
   #include <time.h>
#if ! defined( __WATCOMC__ )
   #include <fnmatch.h>
#endif

typedef struct
{
   DIR * dir;
   struct dirent * entry;
   char pattern[ HB_PATH_MAX ];
   char szRootDir[ HB_PATH_MAX ];
} HB_FFIND_INFO, * PHB_FFIND_INFO;

#else

typedef void HB_FFIND_INFO, * PHB_FFIND_INFO;

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

#if defined( HB_OS_WIN ) && defined( HB_EXTENSION )
   #if ! defined( FILE_ATTRIBUTE_SPARSE_FILE )
      #define FILE_ATTRIBUTE_SPARSE_FILE           0x00000200
   #endif

   #if ! defined( FILE_ATTRIBUTE_REPARSE_POINT )
      #define FILE_ATTRIBUTE_REPARSE_POINT         0x00000400
   #endif

   #if ! defined( FILE_ATTRIBUTE_NOT_CONTENT_INDEXED )
      #define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED   0x00002000
   #endif

   #if ! defined( FILE_ATTRIBUTE_ENCRYPTED )
      #define FILE_ATTRIBUTE_ENCRYPTED             0x00004000
   #endif
   
   #if ! defined( FILE_ATTRIBUTE_PINNED )
      #define FILE_ATTRIBUTE_PINNED               0x00080000  
   #endif
   #if !defined( FILE_ATTRIBUTE_UNPINNED ) 
      #define FILE_ATTRIBUTE_UNPINNED             0x00100000  
   #endif
   
#endif

/* Internal funtion, Convert Windows Error Values to Dos Error Values */
#ifdef HB_OS_WIN

FILETIME GetOldesFile( const char * szPath )
{
   WIN32_FIND_DATA   Lastff32;
   HANDLE            hLastFind;
   FILETIME          ftLastWriteTime   = { 0x99999999, 0x9999999 };

   char *            szf               = ( char * ) hb_xgrab( 7 );

   hb_xstrcpy( szf, szPath, 0 );
   hb_xstrcat( szf, "*.*", 0 );

   hLastFind = FindFirstFile( szf, &Lastff32 );

   if( hLastFind != INVALID_HANDLE_VALUE )
   {
      do
      {
         if( Lastff32.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
            if( CompareFileTime( &ftLastWriteTime, &Lastff32.ftLastWriteTime ) == 1 )
               ftLastWriteTime = Lastff32.ftLastWriteTime;
      }
      while( FindNextFile( hLastFind, &Lastff32 ) );

      FindClose( hLastFind );
   }
   hb_xfree( szf );
   return ftLastWriteTime;
}

#endif

/* ------------------------------------------------------------- */

HB_FATTR hb_fsAttrFromRaw( HB_FATTR raw_attr )
{
   HB_FATTR ulAttr = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsAttrFromRaw(%u)", raw_attr ) );

#if defined( HB_OS_DOS )

   if( raw_attr & FA_ARCH )
      ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FA_HIDDEN )
      ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FA_RDONLY )
      ulAttr |= HB_FA_READONLY;
   if( raw_attr & FA_SYSTEM )
      ulAttr |= HB_FA_SYSTEM;
   if( raw_attr & FA_LABEL )
      ulAttr |= HB_FA_LABEL;
   if( raw_attr & FA_DIREC )
      ulAttr |= HB_FA_DIRECTORY;

#elif defined( HB_OS_OS2 )

   if( raw_attr & FILE_ARCHIVED )
      ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_DIRECTORY )
      ulAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_HIDDEN )
      ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_READONLY )
      ulAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_SYSTEM )
      ulAttr |= HB_FA_SYSTEM;

#elif defined( HB_OS_WIN )

   if( raw_attr & FILE_ATTRIBUTE_ARCHIVE )
      ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_ATTRIBUTE_DIRECTORY )
      ulAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_ATTRIBUTE_HIDDEN )
      ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_ATTRIBUTE_READONLY )
      ulAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_ATTRIBUTE_SYSTEM )
      ulAttr |= HB_FA_SYSTEM;
   if( raw_attr & FILE_ATTRIBUTE_NORMAL )
      ulAttr |= HB_FA_NORMAL;
   if( raw_attr & 0x00000008 )
      ulAttr |= HB_FA_LABEL;

  
  

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( raw_attr & FILE_ATTRIBUTE_ENCRYPTED )
      ulAttr |= HB_FA_ENCRYPTED;
   if( raw_attr & FILE_ATTRIBUTE_TEMPORARY )
      ulAttr |= HB_FA_TEMPORARY;
   if( raw_attr & FILE_ATTRIBUTE_SPARSE_FILE )
      ulAttr |= HB_FA_SPARSE;
   if( raw_attr & FILE_ATTRIBUTE_REPARSE_POINT )
      ulAttr |= HB_FA_REPARSE;
   if( raw_attr & FILE_ATTRIBUTE_COMPRESSED )
      ulAttr |= HB_FA_COMPRESSED;
   if( raw_attr & FILE_ATTRIBUTE_OFFLINE )
      ulAttr |= HB_FA_OFFLINE;
   /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED */
   /* not defined in some older winnt.h  */
   if( raw_attr & 0x00002000 )
      ulAttr |= HB_FA_NOTINDEXED;
   if( raw_attr & 0x00008000 )
      ulAttr |= HB_FA_VOLCOMP;
   if( raw_attr & FILE_ATTRIBUTE_PINNED )
      ulAttr |= HB_FA_PINNED;
   if( raw_attr & FILE_ATTRIBUTE_UNPINNED )
      ulAttr |= HB_FA_UNPINNED;
#endif

#elif defined( HB_OS_UNIX )

   if( S_ISREG( raw_attr ) )
      ulAttr |= HB_FA_ARCHIVE;
   if( S_ISDIR( raw_attr ) )
      ulAttr |= HB_FA_DIRECTORY;
   if( S_ISLNK( raw_attr ) )
      ulAttr |= HB_FA_REPARSE;
   if( S_ISCHR( raw_attr ) )
      ulAttr |= HB_FA_COMPRESSED;
   if( S_ISBLK( raw_attr ) )
      ulAttr |= HB_FA_DEVICE;
   if( S_ISFIFO( raw_attr ) )
      ulAttr |= HB_FA_TEMPORARY;
   if( S_ISSOCK( raw_attr ) )
      ulAttr |= HB_FA_SPARSE;

#else

   HB_SYMBOL_UNUSED( raw_attr );

#endif

   return ulAttr;
}

HB_FATTR hb_fsAttrToRaw( HB_FATTR ulAttr )
{
   HB_FATTR raw_attr = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsAttrToRaw(%u)", ulAttr ) );

#if defined( HB_OS_DOS )

   if( ulAttr & HB_FA_ARCHIVE )
      raw_attr |= FA_ARCH;
   if( ulAttr & HB_FA_DIRECTORY )
      raw_attr |= FA_DIREC;
   if( ulAttr & HB_FA_HIDDEN )
      raw_attr |= FA_HIDDEN;
   if( ulAttr & HB_FA_READONLY )
      raw_attr |= FA_RDONLY;
   if( ulAttr & HB_FA_LABEL )
      raw_attr |= FA_LABEL;
   if( ulAttr & HB_FA_SYSTEM )
      raw_attr |= FA_SYSTEM;

#elif defined( HB_OS_OS2 )

   if( ulAttr & HB_FA_ARCHIVE )
      raw_attr |= FILE_ARCHIVED;
   if( ulAttr & HB_FA_DIRECTORY )
      raw_attr |= FILE_DIRECTORY;
   if( ulAttr & HB_FA_HIDDEN )
      raw_attr |= FILE_HIDDEN;
   if( ulAttr & HB_FA_READONLY )
      raw_attr |= FILE_READONLY;
   if( ulAttr & HB_FA_SYSTEM )
      raw_attr |= FILE_SYSTEM;

#elif defined( HB_OS_WIN )

   if( ulAttr & HB_FA_ARCHIVE )
      raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
   if( ulAttr & HB_FA_DIRECTORY )
      raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
   if( ulAttr & HB_FA_HIDDEN )
      raw_attr |= FILE_ATTRIBUTE_HIDDEN;
   if( ulAttr & HB_FA_LABEL )
      raw_attr |= 0x00000008;
   if( ulAttr & HB_FA_READONLY )
      raw_attr |= FILE_ATTRIBUTE_READONLY;
   if( ulAttr & HB_FA_SYSTEM )
      raw_attr |= FILE_ATTRIBUTE_SYSTEM;
   if( ulAttr & HB_FA_NORMAL )
      raw_attr |= FILE_ATTRIBUTE_NORMAL;

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( ulAttr & HB_FA_ENCRYPTED )
      raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
   if( ulAttr & HB_FA_TEMPORARY )
      raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
   if( ulAttr & HB_FA_SPARSE )
      raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
   if( ulAttr & HB_FA_REPARSE )
      raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
   if( ulAttr & HB_FA_COMPRESSED )
      raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
   if( ulAttr & HB_FA_OFFLINE )
      raw_attr |= FILE_ATTRIBUTE_OFFLINE;
   if( ulAttr & HB_FA_NOTINDEXED )
      raw_attr |= FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
   if( ulAttr & HB_FA_VOLCOMP )
      raw_attr |= 0x00008000;
   if( ulAttr &  HB_FA_PINNED  )
      raw_attr |= FILE_ATTRIBUTE_PINNED;
   if( ulAttr & HB_FA_UNPINNED )
      raw_attr |=  FILE_ATTRIBUTE_UNPINNED ;
  
  

#endif

#elif defined( HB_OS_UNIX )

   if( ulAttr & HB_FA_ARCHIVE )
      raw_attr |= S_IFREG;
   if( ulAttr & HB_FA_DIRECTORY )
      raw_attr |= S_IFDIR;
   if( ulAttr & HB_FA_REPARSE )
      raw_attr |= S_IFLNK;
   if( ulAttr & HB_FA_COMPRESSED )
      raw_attr |= S_IFCHR;
   if( ulAttr & HB_FA_DEVICE )
      raw_attr |= S_IFBLK;
   if( ulAttr & HB_FA_TEMPORARY )
      raw_attr |= S_IFIFO;
   if( ulAttr & HB_FA_SPARSE )
      raw_attr |= S_IFSOCK;

#else

   HB_SYMBOL_UNUSED( ulAttr );

#endif

   return raw_attr;
}

/* Converts a CA-Cl*pper compatible file attribute string
   to the internal reprensentation. */

HB_FATTR hb_fsAttrEncode( const char * szAttr )
{
   const char *   pos      = szAttr;
   char           ch;
   HB_FATTR          ulAttr   = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsAttrEncode(%p)", szAttr ) );

   while( ( ch = ( char ) HB_TOUPPER( *pos ) ) != '\0' )
   {
      switch( ch )
      {
         case 'R': ulAttr  |= HB_FA_READONLY;   break;
         case 'H': ulAttr  |= HB_FA_HIDDEN;     break;
         case 'S': ulAttr  |= HB_FA_SYSTEM;     break;
         case 'V': ulAttr  |= HB_FA_LABEL;      break;
         case 'D': ulAttr  |= HB_FA_DIRECTORY;  break;
         case 'A': ulAttr  |= HB_FA_ARCHIVE;    break;
#ifdef HB_EXTENSION
         case 'E': ulAttr  |= HB_FA_ENCRYPTED;  break;
         case 'T': ulAttr  |= HB_FA_TEMPORARY;  break;
         case 'P': ulAttr  |= HB_FA_SPARSE;     break;
         case 'L': ulAttr  |= HB_FA_REPARSE;    break;
         case 'C': ulAttr  |= HB_FA_COMPRESSED; break;
         case 'O': ulAttr  |= HB_FA_OFFLINE;    break;
         case 'X': ulAttr  |= HB_FA_NOTINDEXED; break;
         case 'I': ulAttr  |= HB_FA_DEVICE;     break;
         case 'M': ulAttr  |= HB_FA_VOLCOMP;    break;
		 case 'W': ulAttr  |= HB_FA_PINNED;     break;
		 case 'U': ulAttr  |= HB_FA_UNPINNED;   break;
#endif
      }

      pos++;
   }

   return ulAttr;
}

/* Converts a file attribute (ffind->attr) to the CA-Cl*pper
   compatible file attribute string format. */

/* NOTE: szAttr buffer must be at least 16 chars long */

char * hb_fsAttrDecode( HB_FATTR ulAttr, char * szAttr )
{
   char * ptr = szAttr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsAttrDecode(%lu, %p)", ulAttr, szAttr ) );

   /* Using the same order as CA-Cl*pper did: RHSVDA. */
   if( ulAttr & HB_FA_READONLY )
      *ptr++ = 'R';
   if( ulAttr & HB_FA_HIDDEN )
      *ptr++ = 'H';
   if( ulAttr & HB_FA_SYSTEM )
      *ptr++ = 'S';
   if( ulAttr & HB_FA_LABEL )
      *ptr++ = 'V';
   if( ulAttr & HB_FA_DIRECTORY )
      *ptr++ = 'D';
   if( ulAttr & HB_FA_ARCHIVE )
      *ptr++ = 'A';
   if( ulAttr & HB_FA_NORMAL )
      *ptr++ = ' ';
#ifdef HB_EXTENSION
   if( ulAttr & HB_FA_ENCRYPTED )
      *ptr++ = 'E';
   if( ulAttr & HB_FA_TEMPORARY )
      *ptr++ = 'T';
   if( ulAttr & HB_FA_SPARSE )
      *ptr++ = 'P';
   if( ulAttr & HB_FA_REPARSE )
      *ptr++ = 'L';
   if( ulAttr & HB_FA_COMPRESSED )
      *ptr++ = 'C';
   if( ulAttr & HB_FA_OFFLINE )
      *ptr++ = 'O';
   if( ulAttr & HB_FA_NOTINDEXED )
      *ptr++ = 'X';
   if( ulAttr & HB_FA_DEVICE )
      *ptr++ = 'I';
   if( ulAttr & HB_FA_VOLCOMP )
      *ptr++ = 'M';
   if( ulAttr & HB_FA_PINNED )
      *ptr++ = 'W';
   if( ulAttr & HB_FA_UNPINNED )
      *ptr++ = 'U';

  
#endif

   *ptr = '\0';

   return szAttr;
}

static void hb_fsFindFill( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

   USHORT         lYear;
   USHORT         lMonth;
   USHORT         lDay;

   USHORT         lHour;
   USHORT         lMin;
   USHORT         lSec;

   HB_FATTR          raw_attr;
   BOOL           bIsLink = FALSE;

   /* Set the default values in case some platforms don't
      support some of these, or they may fail on them. */

   ffind->szName[ 0 ]   = '\0';

   ffind->size          = 0;
   ffind->attr          = 0;

   /* Convert platform specific find info structure into
      the Harbour spcific structure. */

#if defined( HB_OS_DOS )

   {

      time_t      ftime;
      struct tm * ft;
      struct stat sStat;

      #if ! defined( __WATCOMC__ )
      hb_strncpy( ffind->szName, info->entry.ff_name, sizeof( ffind->szName ) - 1 );
      ffind->size = info->entry.ff_fsize;
      #else
      hb_strncpy( ffind->szName, info->entry.name, sizeof( ffind->szName ) - 1 );
      ffind->size = info->entry.size;
      #endif

      #if ! defined( __WATCOMC__ )
      raw_attr = info->entry.ff_attrib;
      #else
      raw_attr = info->entry.attrib;
      #endif

      {
         #if ! defined( __WATCOMC__ )
         stat( info->entry.ff_name, &sStat );
         #else
         stat( info->entry.name, &sStat );
         #endif

         ftime    = sStat.st_mtime;
         ft       = localtime( &ftime );

         lYear    = ft->tm_year + 1900;
         lMonth   = ft->tm_mon + 1;
         lDay     = ft->tm_mday;

         lHour    = ft->tm_hour;
         lMin     = ft->tm_min;
         lSec     = ft->tm_sec;
      }
   }

#elif defined( HB_OS_OS2 )

   {
      hb_strncpy( ffind->szName, info->entry.achName, sizeof( ffind->szName ) - 1 );
      ffind->size = info->entry.cbFile;
      raw_attr    = info->entry.attrFile;

      lYear       = info->entry.fdateLastWrite.year + 1980;
      lMonth      = info->entry.fdateLastWrite.month;
      lDay        = info->entry.fdateLastWrite.day;

      lHour       = info->entry.ftimeLastWrite.hours;
      lMin        = info->entry.ftimeLastWrite.minutes;
      lSec        = info->entry.ftimeLastWrite.twosecs;
   }

#elif defined( HB_OS_WIN )
   {
      FILETIME    ft;
      SYSTEMTIME  time;

      hb_strncpy( ffind->szName, ( info->hFindFile == INVALID_HANDLE_VALUE ? ( const char * ) info->szVolInfo : ( const char * ) info->pFindFileData.cFileName ), sizeof( ffind->szName ) - 1 );

      if( info->hFindFile == INVALID_HANDLE_VALUE )
      {
         ffind->size = 0;
         raw_attr    = ( USHORT ) info->dwAttr;
      }
      else
      {
         if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
         {
            ffind->size = 0;
         }
         else
         {
            ffind->size = ( HB_FOFFSET ) info->pFindFileData.nFileSizeLow +
                          ( ( HB_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32 );
         }
         raw_attr =  info->pFindFileData.dwFileAttributes;
		 
      }


      /* NOTE: One of these may fail when searching on an UNC path, I
               don't know yet what's the reason. [vszakats] */

      if( FileTimeToLocalFileTime( ( info->hFindFile == INVALID_HANDLE_VALUE ? &info->fFileTime : &info->pFindFileData.ftLastWriteTime ), &ft ) &&
          FileTimeToSystemTime( &ft, &time ) )
      {
         lYear    = time.wYear;
         lMonth   = time.wMonth;
         lDay     = time.wDay;

         lHour    = time.wHour;
         lMin     = time.wMinute;
         lSec     = time.wSecond;
      }
      else
      {
         lYear    = 0;
         lMonth   = 0;
         lDay     = 0;
         lHour    = 0;
         lMin     = 0;
         lSec     = 0;
      }
   }
#elif defined( HB_OS_UNIX )
   {
#if defined( HB_USE_LARGEFILE64 )
      struct stat64  sStat;
#else
      struct stat    sStat;
#endif

      //struct stat sStat;
      time_t      ftime;
      struct tm * ft;
      char        szFindFile[ HB_PATH_MAX ];

      hb_strncpy( ffind->szName, info->entry->d_name, sizeof( ffind->szName ) - 1 );
      hb_strncpy( szFindFile, info->szRootDir, sizeof( szFindFile ) - 1 );
      hb_strncat( szFindFile, info->entry->d_name, sizeof( szFindFile ) - 1 );

#if defined( HB_USE_LARGEFILE64 )
      lstat64( szFindFile, &sStat );
      if( S_ISLNK( sStat.st_mode ) )
      {
         stat64( szFindFile, &sStat );
         bIsLink = TRUE;
      }
#else
      lstat( szFindFile, &sStat );
      if( S_ISLNK( sStat.st_mode ) )
      {
         stat( szFindFile, &sStat );
         bIsLink = TRUE;
      }
#endif

      ffind->size = sStat.st_size;
      raw_attr    = sStat.st_mode;
      ftime       = sStat.st_mtime;
      ft          = localtime( &ftime );
      lYear       = ft->tm_year + 1900;
      lMonth      = ft->tm_mon + 1;
      lDay        = ft->tm_mday;
      lHour       = ft->tm_hour;
      lMin        = ft->tm_min;
      lSec        = ft->tm_sec;
   }
#elif defined( HB_OS_MAC )

   {
      /* TODO */
   }

#else

   {
      HB_SYMBOL_UNUSED( info );

      raw_attr = 0;

      lYear    = 0;
      lMonth   = 0;
      lDay     = 0;
      lHour    = 0;
      lMin     = 0;
      lSec     = 0;
   }

#endif

   /* Do the conversions common for all platforms */

   ffind->szName[ HB_PATH_MAX - 1 ] = '\0';

   ffind->attr                      = hb_fsAttrFromRaw( raw_attr );
   if( bIsLink )
   {
      /* LINK attribute is missing since attributes are taken from
         destination file. */
      ffind->attr |= HB_FA_REPARSE;
   }

   ffind->lDate         = hb_dateEncode( lYear, lMonth, lDay );
   ffind->lTime         = hb_timeEncode( lHour, lMin, (double)lSec );
   hb_dateStrPut( ffind->szDate, lYear, lMonth, lDay );
   ffind->szDate[ 8 ]   = '\0';

   hb_snprintf( ffind->szTime, sizeof( ffind->szTime ), "%02d:%02d:%02d", lHour, lMin, lSec );
}

PHB_FFIND hb_fsFindFirst( const char * pszFileName, HB_FATTR ulAttr )
{
   PHB_FFIND   ffind = ( PHB_FFIND ) hb_xgrab( sizeof( HB_FFIND ) );
   BOOL        bFound;

   /* Make sure we have this cleared */
   ffind->info = NULL;

   /* Do platform dependant first search */

#if defined( HB_OS_DOS )
   {
      PHB_FFIND_INFO info;

      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info        = ( PHB_FFIND_INFO ) ffind->info;

      tzset();

      #if ! defined( __WATCOMC__ )
      bFound   = ( findfirst( pszFileName, &info->entry, ( ULONG ) hb_fsAttrToRaw( ulAttr ) ) == 0 );
      #else
      bFound   = ( info->hHandle = _findfirst( pszFileName, &info->entry ) != -1 );
      #endif
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_OS2 )
   {
      PHB_FFIND_INFO info;

      ffind->info       = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info              = ( PHB_FFIND_INFO ) ffind->info;
      info->hFindFile   = HDIR_CREATE;
      info->findCount   = 1;

      bFound            = DosFindFirst( pszFileName,
                                        &info->hFindFile,
                                        ( LONG ) hb_fsAttrToRaw( ulAttr ),
                                        &info->entry,
                                        sizeof( info->entry ),
                                        &info->findCount,
                                        FIL_STANDARD ) == NO_ERROR && info->findCount > 0;

      hb_fsSetIOError( bFound, 0 );

   }
#elif defined( HB_OS_WIN )
   {
      PHB_FFIND_INFO info;
      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info        = ( PHB_FFIND_INFO ) ffind->info;

      if( ulAttr == HB_FA_LABEL )
      {
         DWORD dwSysFlags;
         char  szPath[ 4 ] = { 0 };
         hb_strncpy( szPath, pszFileName, 3 );
         info->hFindFile = INVALID_HANDLE_VALUE;

         if( szPath[ 2 ] == '\0' )
         {
            // 2004-03-05 Clipper compatibility for Directory( "C:", "V" )
            szPath[ 2 ] = HB_OS_PATH_DELIM_CHR;
         }

         if( GetVolumeInformation( szPath, info->szVolInfo, MAX_PATH, NULL, NULL, &dwSysFlags, NULL, 0 ) )
         {
            info->fFileTime   = GetOldesFile( szPath );
            bFound            = TRUE;
            info->dwAttr      = ( DWORD ) hb_fsAttrToRaw( ulAttr );
         }
         else
         {
            bFound = FALSE;
         }
      }
      else
      {
         char *   pFileName;
         HB_SIZE  iNameLen = strlen( pszFileName );
         pFileName         = ( char * ) hb_xgrab( iNameLen + 4 ); // Allow room to add "*.*"
         info->hFindFile   = INVALID_HANDLE_VALUE;
         bFound            = FALSE;
         if( pFileName )
         {
            hb_xstrcpy( pFileName, pszFileName, 0 );
            if( pFileName[ iNameLen - 1 ] == HB_OS_PATH_DELIM_CHR )  //  '\\'
            {
               hb_xstrcat( pFileName, "*.*", 0 );                    // 26/01/2004: Clipper compatibility
            }
            info->hFindFile   = FindFirstFile( pFileName, &info->pFindFileData );
            info->dwAttr      = ( DWORD ) hb_fsAttrToRaw( ulAttr );

            if( info->hFindFile != INVALID_HANDLE_VALUE )
            {
               if( info->dwAttr == 0 ||
                   ( info->pFindFileData.dwFileAttributes == 0 ) ||
                   ( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_NORMAL ) ||
                   ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
               {
                  bFound = TRUE;
               }
               else if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ||
                        ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
               {
                  bFound = TRUE;
               }
               else
               {
                  bFound = FALSE;
                  while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
                  {
                     if( info->dwAttr == 0 ||
                         ( info->pFindFileData.dwFileAttributes == 0 ) ||
                         ( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_NORMAL ) ||
                         ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
                     {
						 						 
                        bFound = TRUE;
                        break;
                     }
                  }
               }
            }
            hb_xfree( pFileName );
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_UNIX )
   {
      PHB_FFIND_INFO info;
      char           string[ HB_PATH_MAX ];
      char           dirname[ HB_PATH_MAX ];
      char *         pos;

      HB_SYMBOL_UNUSED( ulAttr );

      ffind->info          = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info                 = ( PHB_FFIND_INFO ) ffind->info;
      bFound               = FALSE;
      dirname[ 0 ]         = '\0';
      info->pattern[ 0 ]   = '\0';

      if( pszFileName )
      {
         hb_strncpy( string, pszFileName, sizeof( string ) - 1 );
         pos = strrchr( string, HB_OS_PATH_DELIM_CHR );
         if( pos )
         {
            hb_strncpy( info->pattern, pos + 1, sizeof( info->pattern ) - 1 );
            *( pos + 1 ) = '\0';
            hb_strncpy( dirname, string, sizeof( dirname ) - 1 );
         }
         else
         {
            hb_strncpy( info->pattern, string, sizeof( info->pattern ) - 1 );
         }
      }
      if( ! *dirname )
      {
         dirname[ 0 ]   = '.';
         dirname[ 1 ]   = HB_OS_PATH_DELIM_CHR;
         dirname[ 2 ]   = '\0';
      }
      hb_strncpy( info->szRootDir, dirname, sizeof( info->szRootDir ) - 1 );

      if( ! *info->pattern )
      {
         hb_xstrcpy( info->pattern, "*", 0 );
      }

      tzset();

      info->dir = opendir( info->szRootDir );
      if( info->dir != NULL )
      {
         while( ( info->entry = readdir( info->dir ) ) != NULL )
         {
#if defined( __WATCOMC__ )
            if( hb_strMatchWild( info->entry->d_name, info->pattern ) )
#else
            if( fnmatch( info->pattern, info->entry->d_name, FNM_PERIOD | FNM_PATHNAME ) == 0 )
#endif
            {
               bFound = TRUE;
               break;
            }
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_MAC )
   {
      /* TODO */
      bFound = FALSE;
   }
#else
   {
      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( ulAttr );
      bFound = FALSE;
   }
#endif

   /* Return file info or close the search automatically */

   if( bFound )
   {
      hb_fsFindFill( ffind );
   }
   else
   {
      hb_fsFindClose( ffind );
      ffind = NULL;
   }

   return ffind;
}

BOOL hb_fsFindNext( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;
   BOOL           bFound;

   /* Do platform dependant search */

#if defined( HB_OS_DOS )
   {
      #if ! defined( __WATCOMC__ )
      bFound   = ( findnext( &info->entry ) == 0 );
      #else
      bFound   = ( _findnext( info->hHandle, &info->entry ) != -1 );
      #endif
      hb_fsSetIOError( bFound, 0 );

   }
#elif defined( HB_OS_OS2 )
   {
      bFound = DosFindNext( info->hFindFile, &info->entry, sizeof( info->entry ), &info->findCount ) == NO_ERROR &&
               info->findCount > 0;
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_WIN )
   {
      bFound = FALSE;
      if( FindNextFile( info->hFindFile, &info->pFindFileData ) )
      {
         if( info->dwAttr == 0 ||
             ( info->pFindFileData.dwFileAttributes == 0 ) ||
             ( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_NORMAL ) ||
             ( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN ) ||
             ( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_SYSTEM ) ||
             ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY ) ||
             ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
         {
            bFound = TRUE;
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_UNIX )
   {
      bFound = FALSE;
      while( ( info->entry = readdir( info->dir ) ) != NULL )
      {
#if defined( __WATCOMC__ )
         if( hb_strMatchWild( info->entry->d_name, info->pattern ) )
#else
         if( fnmatch( info->pattern, info->entry->d_name, FNM_PERIOD | FNM_PATHNAME ) == 0 )
#endif
         {
            bFound = TRUE;
            break;
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }
#elif defined( HB_OS_MAC )
   {
      /* TODO */
      bFound = FALSE;
   }
#else
   {
      HB_SYMBOL_UNUSED( info );
      bFound = FALSE;
   }
#endif
   /* Return file info */
   if( bFound )
   {
      hb_fsFindFill( ffind );
   }
   return bFound;
}

void hb_fsFindClose( PHB_FFIND ffind )
{
   if( ffind != NULL )
   {
      /* Do platform dependant cleanup */
      if( ffind->info != NULL )
      {
         PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

#if defined( HB_OS_DOS )
         #if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         {
            HB_SYMBOL_UNUSED( info );
         }
         #else
         {
            #if ! defined( __WATCOMC__ )
            findclose( &info->entry );
            #else
            _findclose( info->hHandle );
            #endif
         }
         #endif
#elif defined( HB_OS_OS2 )
         {
            DosFindClose( info->hFindFile );
         }
#elif defined( HB_OS_WIN )
         {
            if( info->hFindFile != INVALID_HANDLE_VALUE )
            {
               FindClose( info->hFindFile );
            }
         }
#elif defined( HB_OS_UNIX )
         {
            if( info->dir )
            {
               closedir( info->dir );
            }
         }
#elif defined( HB_OS_MAC )
         {
            /* TODO */
         }
#else
         {
            /* Intentionally do nothing */
            HB_SYMBOL_UNUSED( info );
         }
#endif
         hb_xfree( ( void * ) ffind->info );
      }

      hb_xfree( ( void * ) ffind );
   }
}
