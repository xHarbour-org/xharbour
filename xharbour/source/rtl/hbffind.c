/*
 * $Id: hbffind.c,v 1.6 2003/09/09 02:08:01 paultucker Exp $
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

#define INCL_DOSFILEMGR
#define INCL_DOSERRORS
#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hb_io.h"

HB_FILE_VER( "$Id: hbffind.c,v 1.6 2003/09/09 02:08:01 paultucker Exp $" )

/* ------------------------------------------------------------- */

#include <ctype.h>

#if defined(HB_OS_DOS)

   #if defined(__DJGPP__) || defined(__RSX32__)
      #include <errno.h>
      #include <sys/param.h>
   #endif

   #if defined(__DJGPP__) || defined(__RSX32__) || defined(__BORLANDC__) || defined(__WATCOMC__)
      #include <sys/stat.h>
   #endif

   #include <dos.h>

   #if defined(__WATCOMC__)
       #include <io.h>
   #else
       #include <dir.h>
   #endif

   #include <time.h>

   typedef struct
   {

   #if defined(__WATCOMC__)
      struct _finddata_t  entry;
      long hHandle;
   #else
      struct ffblk    entry;
   #endif

   } HB_FFIND_INFO, * PHB_FFIND_INFO;

   /* Needed For WatCom */
    #if !defined(FA_ARCH)
        #define FA_NORMAL _A_NORMAL
        #define FA_RDONLY _A_RDONLY
        #define FA_HIDDEN _A_HIDDEN
        #define FA_SYSTEM _A_SYSTEM
        #define FA_SUBDIR _A_SUBDIR
        #define FA_ARCH   _A_ARCH
        #define FA_DIREC        16
        #define FA_LABEL        8
    #endif

#elif defined(HB_OS_OS2)

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <time.h>

   typedef struct
   {
      HDIR            hFindFile;
      FILEFINDBUF3    entry;
      ULONG           fileTypes;
      ULONG           findSize;
      ULONG           findCount;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined(HB_OS_WIN_32)

   #include <errno.h>

   typedef struct
   {
      HANDLE          hFindFile;
      WIN32_FIND_DATA pFindFileData;
      DWORD           dwAttr;
      char            szVolInfo[MAX_PATH];
      FILETIME        fFileTime;

   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined(HB_OS_UNIX)

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>
   #include <dirent.h>
   #include <time.h>
   #include <fnmatch.h>
   typedef struct
   {
      DIR *           dir;
      struct dirent * entry;
      char   pattern[_POSIX_PATH_MAX +1];
      char   szRootDir[ _POSIX_PATH_MAX + 1 ];
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#else

   typedef void HB_FFIND_INFO, * PHB_FFIND_INFO;

#endif

/* Internal funtion , Convert Windows Error Values to Dos Error Values */
#ifdef HB_OS_WIN_32
int WintoDosError( unsigned long lError)
{
   int iReturn;
   switch( lError ) {
   case ERROR_ALREADY_EXISTS:
      iReturn = 5;
      break;
   case ERROR_FILE_NOT_FOUND:
      iReturn = 2;
      break;
   case ERROR_PATH_NOT_FOUND:
      iReturn = 3;
      break;
   case  ERROR_TOO_MANY_OPEN_FILES:
      iReturn = 4;
      break;
   case ERROR_INVALID_HANDLE:
      iReturn = 6;
      break;
   default:
         iReturn=0;
      break;
      }

   return iReturn;
}

FILETIME GetOldesFile( const char * szPath)
{
   WIN32_FIND_DATA  Lastff32;
   HANDLE hLastFind;
   FILETIME ftLastWriteTime ={0x99999999,0x9999999};

   char * szf = (char*)hb_xgrab(7);

   strcpy(szf,szPath);
   strcat(szf,"*.*");

   hLastFind = FindFirstFile( szf,&Lastff32 );

   if ( hLastFind != INVALID_HANDLE_VALUE )
   {
      do
       {
       if (Lastff32.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
          if(CompareFileTime( &ftLastWriteTime,&Lastff32.ftLastWriteTime ) ==1 )
             ftLastWriteTime = Lastff32.ftLastWriteTime;
      } while ( FindNextFile( hLastFind,&Lastff32 )) ;

      FindClose( hLastFind );
   }
   hb_xfree(szf);
   return ftLastWriteTime;
}

#endif



/* ------------------------------------------------------------- */

USHORT HB_EXPORT hb_fsAttrFromRaw( ULONG raw_attr )
{
   USHORT uiAttr =0 ;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrFromRaw(%hu)", raw_attr));

#if defined(HB_OS_DOS)

   if( raw_attr & FA_ARCH )   uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FA_HIDDEN ) uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FA_RDONLY ) uiAttr |= HB_FA_READONLY;
   if( raw_attr & FA_SYSTEM ) uiAttr |= HB_FA_SYSTEM;
   if( raw_attr & FA_LABEL )  uiAttr |= HB_FA_LABEL;
   if( raw_attr & FA_DIREC )  uiAttr |= HB_FA_DIRECTORY;

#elif defined(HB_OS_OS2)

   if( raw_attr & FILE_ARCHIVED )  uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_DIRECTORY ) uiAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_HIDDEN )    uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_READONLY )  uiAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_SYSTEM )    uiAttr |= HB_FA_SYSTEM;

#elif defined(HB_OS_WIN_32)

   if( raw_attr & FILE_ATTRIBUTE_ARCHIVE )   uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_ATTRIBUTE_DIRECTORY ) uiAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_ATTRIBUTE_HIDDEN )    uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_ATTRIBUTE_READONLY )  uiAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_ATTRIBUTE_SYSTEM )    uiAttr |= HB_FA_SYSTEM;
   if( raw_attr & FILE_ATTRIBUTE_NORMAL )    uiAttr |= HB_FA_NORMAL;
   if( raw_attr & 0x00000008 )                   uiAttr |= HB_FA_LABEL;

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( raw_attr & FILE_ATTRIBUTE_ENCRYPTED )     uiAttr |= HB_FA_ENCRYPTED;
   if( raw_attr & FILE_ATTRIBUTE_TEMPORARY )     uiAttr |= HB_FA_TEMPORARY;
   if( raw_attr & FILE_ATTRIBUTE_SPARSE_FILE )   uiAttr |= HB_FA_SPARSE;
   if( raw_attr & FILE_ATTRIBUTE_REPARSE_POINT ) uiAttr |= HB_FA_REPARSE;
   if( raw_attr & FILE_ATTRIBUTE_COMPRESSED )    uiAttr |= HB_FA_COMPRESSED;
   if( raw_attr & FILE_ATTRIBUTE_OFFLINE )       uiAttr |= HB_FA_OFFLINE;
   if( raw_attr & FILE_ATTRIBUTE_NOT_CONTENT_INDEXED )
                                                 uiAttr |= HB_FA_NOTINDEXED;
   if( raw_attr & 0x00008000 )                   uiAttr |= HB_FA_VOLCOMP;
#endif

#elif defined(HB_OS_UNIX)

   if( S_ISREG( raw_attr ) )  uiAttr |= HB_FA_ARCHIVE;
   if( S_ISDIR( raw_attr ) )  uiAttr |= HB_FA_DIRECTORY;
   if( S_ISLNK( raw_attr ) )  uiAttr |= HB_FA_REPARSE;
   if( S_ISCHR( raw_attr ) )  uiAttr |= HB_FA_COMPRESSED;
   if( S_ISBLK( raw_attr ) )  uiAttr |= HB_FA_DEVICE;
   if( S_ISFIFO( raw_attr ) ) uiAttr |= HB_FA_TEMPORARY;
   if( S_ISSOCK( raw_attr ) ) uiAttr |= HB_FA_SPARSE;

#else

   HB_SYMBOL_UNUSED( raw_attr );

#endif

   return uiAttr;
}

ULONG HB_EXPORT hb_fsAttrToRaw( USHORT uiAttr )
{
   ULONG raw_attr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrToRaw(%hu)", uiAttr));

#if defined(HB_OS_DOS)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FA_ARCH;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FA_DIREC;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FA_HIDDEN;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FA_RDONLY;
   if( uiAttr & HB_FA_LABEL )     raw_attr |= FA_LABEL;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FA_SYSTEM;

#elif defined(HB_OS_OS2)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ARCHIVED;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_DIRECTORY;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FILE_HIDDEN;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FILE_READONLY;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FILE_SYSTEM;

#elif defined(HB_OS_WIN_32)

   raw_attr = 0;

   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FILE_ATTRIBUTE_HIDDEN;
   if( uiAttr & HB_FA_LABEL   )   raw_attr |= 0x00000008;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FILE_ATTRIBUTE_READONLY;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FILE_ATTRIBUTE_SYSTEM;
   if( uiAttr & HB_FA_NORMAL )    raw_attr |= FILE_ATTRIBUTE_NORMAL;

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( uiAttr & HB_FA_ENCRYPTED )  raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
   if( uiAttr & HB_FA_TEMPORARY )  raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
   if( uiAttr & HB_FA_SPARSE )     raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
   if( uiAttr & HB_FA_REPARSE )    raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
   if( uiAttr & HB_FA_COMPRESSED ) raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
   if( uiAttr & HB_FA_OFFLINE )    raw_attr |= FILE_ATTRIBUTE_OFFLINE;
   if( uiAttr & HB_FA_NOTINDEXED ) raw_attr |= FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
   if( uiAttr & HB_FA_VOLCOMP )    raw_attr |= 0x00008000;

#endif

#elif defined(HB_OS_UNIX)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )    raw_attr |= S_IFREG;
   if( uiAttr & HB_FA_DIRECTORY )  raw_attr |= S_IFDIR;
   if( uiAttr & HB_FA_REPARSE )    raw_attr |= S_IFLNK;
   if( uiAttr & HB_FA_COMPRESSED ) raw_attr |= S_IFCHR;
   if( uiAttr & HB_FA_DEVICE )     raw_attr |= S_IFBLK;
   if( uiAttr & HB_FA_TEMPORARY )  raw_attr |= S_IFIFO;
   if( uiAttr & HB_FA_SPARSE )     raw_attr |= S_IFSOCK;

#else

   HB_SYMBOL_UNUSED( uiAttr );
   raw_attr = 0;

#endif

   return raw_attr;
}

/* Converts a CA-Cl*pper compatible file attribute string
   to the internal reprensentation. */

USHORT HB_EXPORT hb_fsAttrEncode( const char * szAttr )
{
   const char * pos = szAttr;
   char ch;
   USHORT uiAttr = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrEncode(%p)", szAttr));

   while( ( ch = toupper( *pos ) ) != '\0' )
   {
      switch( ch )
      {
         case 'R': uiAttr |= HB_FA_READONLY;   break;
         case 'H': uiAttr |= HB_FA_HIDDEN;     break;
         case 'S': uiAttr |= HB_FA_SYSTEM;     break;
         case 'V': uiAttr |= HB_FA_LABEL;      break;
         case 'D': uiAttr |= HB_FA_DIRECTORY;  break;
         case 'A': uiAttr |= HB_FA_ARCHIVE;    break;
#ifdef HB_EXTENSION
         case 'E': uiAttr |= HB_FA_ENCRYPTED;  break;
         case 'T': uiAttr |= HB_FA_TEMPORARY;  break;
         case 'P': uiAttr |= HB_FA_SPARSE;     break;
         case 'L': uiAttr |= HB_FA_REPARSE;    break;
         case 'C': uiAttr |= HB_FA_COMPRESSED; break;
         case 'O': uiAttr |= HB_FA_OFFLINE;    break;
         case 'X': uiAttr |= HB_FA_NOTINDEXED; break;
         case 'I': uiAttr |= HB_FA_DEVICE;     break;
         case 'M': uiAttr |= HB_FA_VOLCOMP;    break;
#endif
      }

      pos++;
   }

   return uiAttr;
}

/* Converts a file attribute (ffind->attr) to the CA-Cl*pper
   compatible file attribute string format. */

/* NOTE: szAttr buffer must be at least 16 chars long */

char HB_EXPORT * hb_fsAttrDecode( USHORT uiAttr, char * szAttr )
{
   char * ptr ;
   ptr=szAttr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrDecode(%hu, %p)", uiAttr, szAttr));

   /* Using the same order as CA-Cl*pper did: RHSVDA. */
   if( uiAttr & HB_FA_READONLY   ) *ptr++ = 'R';
   if( uiAttr & HB_FA_HIDDEN     ) *ptr++ = 'H';
   if( uiAttr & HB_FA_SYSTEM     ) *ptr++ = 'S';
   if( uiAttr & HB_FA_LABEL      ) *ptr++ = 'V';
   if( uiAttr & HB_FA_DIRECTORY  ) *ptr++ = 'D';
   if( uiAttr & HB_FA_ARCHIVE    ) *ptr++ = 'A';
   if( uiAttr & HB_FA_NORMAL     ) *ptr++ = ' ';
#ifdef HB_EXTENSION
   if( uiAttr & HB_FA_ENCRYPTED  ) *ptr++ = 'E';
   if( uiAttr & HB_FA_TEMPORARY  ) *ptr++ = 'T';
   if( uiAttr & HB_FA_SPARSE     ) *ptr++ = 'P';
   if( uiAttr & HB_FA_REPARSE    ) *ptr++ = 'L';
   if( uiAttr & HB_FA_COMPRESSED ) *ptr++ = 'C';
   if( uiAttr & HB_FA_OFFLINE    ) *ptr++ = 'O';
   if( uiAttr & HB_FA_NOTINDEXED ) *ptr++ = 'X';
   if( uiAttr & HB_FA_DEVICE     ) *ptr++ = 'I';
   if( uiAttr & HB_FA_VOLCOMP    ) *ptr++ = 'M';
#endif

   *ptr = '\0';

   return szAttr;
}

static void hb_fsFindFill( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

   USHORT lYear;
   USHORT lMonth;
   USHORT lDay;

   USHORT lHour;
   USHORT lMin;
   USHORT lSec;

   ULONG  raw_attr;

   /* Set the default values in case some platforms don't
      support some of these, or they may fail on them. */

   ffind->szName[ 0 ] = '\0';

   ffind->size = 0;
   ffind->attr = 0;

   /* Convert platform specific find info structure into
      the Harbour spcific structure. */

#if defined(HB_OS_DOS)

   {

      time_t ftime;
      struct tm * ft;
      struct stat sStat;

      #if !defined( __WATCOMC__)
         strncpy( ffind->szName, info->entry.ff_name, _POSIX_PATH_MAX );
         ffind->size = info->entry.ff_fsize;
      #else
         strncpy( ffind->szName, info->entry.name, _POSIX_PATH_MAX );
         ffind->size = info->entry.size;
      #endif

      #if !defined( __WATCOMC__)
         raw_attr = info->entry.ff_attrib;
      #else
         raw_attr = info->entry.attrib;
      #endif

      {
         #if !defined( __WATCOMC__ )
            stat( info->entry.ff_name, &sStat );
         #else
            stat( info->entry.name, &sStat );
         #endif

         ftime = sStat.st_mtime;
         ft = localtime( &ftime );

         lYear  = ft->tm_year + 1900;
         lMonth = ft->tm_mon + 1;
         lDay   = ft->tm_mday;

         lHour  = ft->tm_hour;
         lMin   = ft->tm_min;
         lSec   = ft->tm_sec;
      }
   }

#elif defined(HB_OS_OS2)

   {
      struct stat sStat;
      time_t ftime;
      struct tm * ft;

      stat( info->entry.achName, &sStat );

      strncpy( ffind->szName, info->entry.achName, _POSIX_PATH_MAX );
      ffind->size = sStat.st_size;

      raw_attr = info->entry.attrFile;

      {
         ftime = sStat.st_mtime;
         ft = localtime( &ftime );

         lYear  = ft->tm_year + 1900;
         lMonth = ft->tm_mon + 1;
         lDay   = ft->tm_mday;

         lHour  = ft->tm_hour;
         lMin   = ft->tm_min;
         lSec   = ft->tm_sec;
      }
   }

#elif defined(HB_OS_WIN_32)

   {
      FILETIME ft;
      SYSTEMTIME time;

      strncpy( ffind->szName,( info->hFindFile == INVALID_HANDLE_VALUE ? info->szVolInfo :info->pFindFileData.cFileName ), _POSIX_PATH_MAX );

      if (info->hFindFile == INVALID_HANDLE_VALUE )
      {
         ffind->size = 0;
         raw_attr = ( USHORT ) info->dwAttr;
      }
      else
      {
         if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY   )
         {
            ffind->size = 0;
         }
         else
         {
         #ifndef HB_LONG_LONG_OFF
            ffind->size = ( info->pFindFileData.nFileSizeHigh * MAXDWORD ) + info->pFindFileData.nFileSizeLow;
         #else
            ffind->size = info->pFindFileData.nFileSizeLow;
         #endif
         }
      raw_attr = ( USHORT ) info->pFindFileData.dwFileAttributes;
      }


      /* NOTE: One of these may fail when searching on an UNC path, I
               don't know yet what's the reason. [vszakats] */

      if( FileTimeToLocalFileTime( (info->hFindFile == INVALID_HANDLE_VALUE ? &info->fFileTime : &info->pFindFileData.ftLastWriteTime), &ft ) &&
             FileTimeToSystemTime( &ft, &time ) )
      {
         lYear  = time.wYear;
         lMonth = time.wMonth;
         lDay   = time.wDay;

         lHour  = time.wHour;
         lMin   = time.wMinute;
         lSec   = time.wSecond;
      }
      else
      {
         lYear  = 0;
         lMonth = 0;
         lDay   = 0;
         lHour  = 0;
         lMin   = 0;
         lSec   = 0;
      }
   }

#elif defined(HB_OS_UNIX)

   {
      struct stat sStat;
      time_t ftime;
      struct tm * ft;

      char   szFindFile[ _POSIX_PATH_MAX + 1 ];

      strncpy( ffind->szName, info->entry->d_name, 256 );
      strncpy(szFindFile,info->szRootDir,256);
      strcat(szFindFile,info->entry->d_name);

      stat(szFindFile,&sStat);
      strncpy( ffind->szName, info->entry->d_name, 256 );

      ffind->size = sStat.st_size;

      raw_attr = sStat.st_mode;

      ftime = sStat.st_mtime;
      ft = localtime( &ftime );
      lYear  = ft->tm_year + 1900;
      lMonth = ft->tm_mon + 1;
      lDay   = ft->tm_mday;
      lHour  = ft->tm_hour;
      lMin   = ft->tm_min;
      lSec   = ft->tm_sec;

   }

#elif defined(HB_OS_MAC)

   {
      /* TODO */
   }

#else

   {
      HB_SYMBOL_UNUSED( info );

      raw_attr = 0;

      lYear  = 0;
      lMonth = 0;
      lDay   = 0;
      lHour  = 0;
      lMin   = 0;
      lSec   = 0;
   }

#endif

   /* Do the conversions common for all platforms */

   ffind->szName[ _POSIX_PATH_MAX ] = '\0';

   ffind->attr = hb_fsAttrFromRaw( raw_attr );

   ffind->lDate = hb_dateEncode( lYear, lMonth, lDay );
   hb_dateStrPut( ffind->szDate, lYear, lMonth, lDay );
   ffind->szDate[ 8 ] = '\0';

   sprintf( ffind->szTime, "%02d:%02d:%02d", lHour, lMin, lSec );
}

PHB_FFIND HB_EXPORT hb_fsFindFirst( const char * pszFileName, USHORT uiAttr )
{
   PHB_FFIND ffind = ( PHB_FFIND ) hb_xgrab( sizeof( HB_FFIND ) );
   BOOL bFound;

   /* Make sure we have this cleared */

   ffind->info = NULL;

   /* Do platform dependant first search */

#if defined(HB_OS_DOS)

   {
      PHB_FFIND_INFO info;
      errno = 0;

      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info = ( PHB_FFIND_INFO ) ffind->info;

      tzset();

      #if !defined(__WATCOMC__)
          bFound = ( findfirst( pszFileName, &info->entry, ( USHORT ) hb_fsAttrToRaw( uiAttr ) ) == 0 );
      #else
          bFound = ( info->hHandle = _findfirst( pszFileName, &info->entry )  != -1 );
      #endif

      #if defined(__DJGPP__) || defined(__RSX32__)
        errno = GnuErrtoDosErr( errno );
      #endif

      hb_fsSetError( errno );
   }

#elif defined(HB_OS_OS2)

   {
      PHB_FFIND_INFO info;

      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info = ( PHB_FFIND_INFO ) ffind->info;

      tzset();

      info->hFindFile = HDIR_CREATE;
      info->findCount = 1;

      bFound = DosFindFirst( pszFileName,
                             &info->hFindFile,
                             ( LONG ) hb_fsAttrToRaw( uiAttr ),
                             &info->entry,
                             sizeof( info->entry ),
                             &info->findCount,
                             FIL_STANDARD ) == NO_ERROR && info->findCount > 0;
   }

#elif defined(HB_OS_WIN_32)

   {
      PHB_FFIND_INFO info;
      errno = 0;

      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info = ( PHB_FFIND_INFO ) ffind->info;

      if ( uiAttr == HB_FA_LABEL )
      {
         DWORD dwSysFlags;
         char szPath[ 4 ]  = {0};
         strncpy( szPath, pszFileName, 3);
         info->hFindFile = INVALID_HANDLE_VALUE;

         if (GetVolumeInformation( szPath, info->szVolInfo, MAX_PATH, NULL, NULL, &dwSysFlags, NULL, 0 ) )
         {
            info->fFileTime = GetOldesFile( pszFileName );
            bFound = TRUE;
            info->dwAttr    = ( DWORD ) hb_fsAttrToRaw( uiAttr );
         }
         else
            bFound = FALSE;
      }

      else
      {
      info->hFindFile = FindFirstFile( pszFileName, &info->pFindFileData );
      info->dwAttr    = ( DWORD ) hb_fsAttrToRaw( uiAttr );


      if( info->hFindFile != INVALID_HANDLE_VALUE )
      {
         if( info->dwAttr == 0 ||
           ( info->pFindFileData.dwFileAttributes == 0 ) ||
           ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_NORMAL ) ||
           ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY ) ||
           ( info->dwAttr & info->pFindFileData.dwFileAttributes ))
         {
            bFound = TRUE;
         }
         else if ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY  ||
           ( info->dwAttr & info->pFindFileData.dwFileAttributes ))
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
                 ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_NORMAL )  ||
                 ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY ) ||
                 ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
               {
                  bFound = TRUE;
                  break;
               }
            }
         }
      }
      else {
         bFound = FALSE;
         errno = WintoDosError( GetLastError() );
         hb_fsSetError( errno );
      }
      }
   }

#elif defined(HB_OS_UNIX)

   {
      PHB_FFIND_INFO info;
      char     string[ _POSIX_PATH_MAX + 1 ];
      char     dirname[ _POSIX_PATH_MAX + 1 ];
      char *   pos;

      ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
      info = ( PHB_FFIND_INFO ) ffind->info;
      bFound =0;
      dirname[ 0 ] = '\0';
      info->pattern[ 0 ] = '\0';

      if( pszFileName )
      {
         strcpy( string, pszFileName );
         pos = strrchr( string, OS_PATH_DELIMITER );

         if( pos )
         {
            strcpy( info->pattern, pos + 1 );
            *( pos + 1 ) = '\0';
            strcpy( dirname, string );
         }
         else
         {
            strcpy( info->pattern, string );
            strcpy( dirname, ".X" );
            dirname[ 1 ] = OS_PATH_DELIMITER;
            dirname[ 2 ] = '\0';
         }
      }

      strcpy(info->szRootDir,dirname);

      if( !*info->pattern )
      {
         strcpy( info->pattern, "*.*" );
      }

      tzset();

      info->dir = opendir( dirname );

      if( info->dir != NULL) {
         while( ( info->entry = readdir( info->dir ) ) != NULL )
         {
            strcpy( string, info->entry->d_name );

            if( fnmatch( info->pattern,string,FNM_PERIOD|FNM_PATHNAME)==0)
            {
               bFound=TRUE;
               break;
            }
         }

      }
      else
      {
         bFound = FALSE;
      }

   }

#elif defined(HB_OS_MAC)

   {
      /* TODO */

      bFound = FALSE;
   }

#else

   {
      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( uiAttr );

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

BOOL HB_EXPORT hb_fsFindNext( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;
   BOOL bFound;

   /* Do platform dependant search */

#if defined(HB_OS_DOS)

   {
      #if !defined(__WATCOMC__)
          bFound = ( findnext( &info->entry ) == 0 );
      #else
          bFound = ( _findnext( info->hHandle,&info->entry ) != -1 );
      #endif

      #if defined(__DJGPP__) || defined(__RSX32__)
        errno = GnuErrtoDosErr( errno );
      #endif

      hb_fsSetError( errno );
   }

#elif defined(HB_OS_OS2)

   {
      bFound = DosFindNext( info->hFindFile, &info->entry, sizeof( info->entry ), &info->findCount ) == NO_ERROR &&
                            info->findCount > 0;
   }

#elif defined(HB_OS_WIN_32)

   {
      errno = 0 ;
      bFound = FALSE;

      while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
      {
         if( info->dwAttr == 0 ||
             ( info->pFindFileData.dwFileAttributes == 0 ) ||
             ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_NORMAL ) ||
             ( info->pFindFileData.dwFileAttributes == FILE_ATTRIBUTE_DIRECTORY) ||
             ( info->dwAttr & info->pFindFileData.dwFileAttributes ))
         {
            bFound = TRUE;
            break;
         }
         else {
            errno = WintoDosError(GetLastError()) ;
            hb_fsSetError( errno );
            break;
         }

      }
   }

#elif defined(HB_OS_UNIX)

   {
      char     string[ _POSIX_PATH_MAX + 1 ];
      bFound=FALSE;

      while( ( info->entry = readdir( info->dir ) ) != NULL )
      {
         strcpy( string, info->entry->d_name );

         if( fnmatch( info->pattern,string,FNM_PERIOD|FNM_PATHNAME)==0)
         {
            bFound=TRUE;
            break;
         }
      }
   }

#elif defined(HB_OS_MAC)

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

   if( bFound ) {
      hb_fsFindFill( ffind );
   }

   return bFound;
}

void HB_EXPORT hb_fsFindClose( PHB_FFIND ffind )
{
   if( ffind != NULL )
   {
      /* Do platform dependant cleanup */
      if( ffind->info != NULL )
      {
         PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

#if defined(HB_OS_DOS)

         #if defined(__DJGPP__) || defined(__BORLANDC__)
         {
            HB_SYMBOL_UNUSED( info );
         }
         #else
         {
            #if !defined(__WATCOMC__)
               findclose( &info->entry );
            #else
               _findclose( info->hHandle );
            #endif
         }
         #endif

#elif defined(HB_OS_OS2)

         {
            DosFindClose( info->hFindFile );
         }

#elif defined(HB_OS_WIN_32)

         {
            FindClose( info->hFindFile );
         }

#elif defined(HB_OS_UNIX)

         {
            if (info->dir)
            {
               closedir( info->dir );
            }
         }

#elif defined(HB_OS_MAC)

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

