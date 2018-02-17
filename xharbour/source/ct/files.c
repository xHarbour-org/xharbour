/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 files functions
 *
 * FILESEEK,FILESIZE,FILEATTR,FILETIME,FILEDATE,SETFATTR
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *
 * FILESMAX, SETFDATI
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 *
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


#if defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2154)
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbdate.h"

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
#if defined( HB_OS_DOS ) && ! defined( __WATCOMC__ )
static struct ffblk fsOldFiles;
#endif

#if defined( HB_OS_OS2 ) && defined( __GNUC__ )

   #include "hb_io.h"

   #if defined( __EMX__ )
      #include <emx/syscalls.h>
      #define gethostname  __gethostname
   #endif

   #define MAXGETHOSTNAME  256     /* should be enough for a host name */

#elif defined( HB_OS_DOS ) && ! defined( __RSX32__ )

   #if defined( __DJGPP__ )
      #include <dpmi.h>
      #include <go32.h>
      #include <sys/farptr.h>
      #include <sys/param.h>
   #endif
      #include "hb_io.h"
      #include "dos.h"
    #if defined( __WATCOMC__ )
    #else
      #include <dir.h>
    #endif
#endif
#if defined( HB_OS_UNIX ) || ( defined( __GNUC__ ) && ! defined( __MINGW32__ ) )
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>
   #include <dirent.h>
   #include <time.h>
   #include <utime.h>
   #if ! defined( HAVE_POSIX_IO )
      #define HAVE_POSIX_IO
   #endif

#endif

#if ( defined( HB_OS_WIN ) || defined( __MINGW32__ ) ) && ! defined( __CYGWIN__ )
#include "windows.h"
static HANDLE           hLastFind;
static WIN32_FIND_DATA  Lastff32;
static LPTSTR GetDate( FILETIME * rTime );
static LPTSTR GetTime( FILETIME * rTime );

#if ! defined( _MSC_VER ) && ! defined( __RSXNT__ ) && ! defined( __WATCOMC__ )
   #include <dir.h>
#endif

 #if ! defined( FILE_ATTRIBUTE_PINNED )
      #define FILE_ATTRIBUTE_PINNED               0x00080000  
   #endif
   #if !defined( FILE_ATTRIBUTE_UNPINNED ) 
      #define FILE_ATTRIBUTE_UNPINNED             0x00100000  
   #endif

#endif

#if ! defined( FA_ARCH )
   #define FA_RDONLY       1        /* R */
   #define FA_HIDDEN       2        /* H */
   #define FA_SYSTEM       4        /* S */
   #define FA_LABEL        8        /* V */
   #define FA_DIREC        16       /* D */
   #define FA_ARCH         32       /* A */
   #define FA_NORMAL       0
   
#endif

#if ! defined( FA_DEVICE )
   #define FA_DEVICE       64       /* I */
/*   #define FA_NORMAL         128 */  /* N */  /* ignored */ /* Exists in BORLANDC */
   #define FA_TEMPORARY    256      /* T */
   #define FA_SPARSE       512      /* P */
   #define FA_REPARSE      1024     /* L */
   #define FA_COMPRESSED   2048     /* C */
   #define FA_OFFLINE      4096     /* O */
   #define FA_NOTINDEXED   8192     /* X */
   #define FA_ENCRYPTED    16384    /* E */
   #define FA_VOLCOMP      32768    /* M */
   #define FA_PINNED       524288
   #define FA_UNPINNED     1048576
#endif

#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )
static USHORT osToHarbourMask(  USHORT usMask  )
{
   USHORT usRetMask;

   HB_TRACE( HB_TR_DEBUG, ( "osToHarbourMask( %hu )", usMask ) );

   usRetMask = usMask;

   /* probably access denied when requesting mode */
   if( usMask == ( USHORT ) -1 )
      return 0;

   #if defined( HB_OS_UNIX )
   /* The use of any particular FA_ define here is meaningless */
   /* they are essentially placeholders */
   usRetMask = 0;
   if( S_ISREG(  usMask  ) )
      usRetMask |= FA_ARCH;            /* A */
   if( S_ISDIR(  usMask  ) )
      usRetMask |= FA_DIREC;           /* D */
   if( S_ISLNK(  usMask  ) )
      usRetMask |= FA_REPARSE;         /* L */
   if( S_ISCHR(  usMask  ) )
      usRetMask |= FA_COMPRESSED;      /* C */
   if( S_ISBLK(  usMask  ) )
      usRetMask |= FA_DEVICE;          /* B  ( I ) */
   if( S_ISFIFO(  usMask  ) )
      usRetMask |= FA_TEMPORARY;       /* F  ( T ) */
   if( S_ISSOCK(  usMask  ) )
      usRetMask |= FA_SPARSE;          /* K  ( P ) */
   #elif defined( HB_OS_OS2 )
   usRetMask = 0;
   if( usMask & FILE_ARCHIVED )
      usRetMask |= FA_ARCH;
   if( usMask & FILE_DIRECTORY )
      usRetMask |= FA_DIREC;
   if( usMask & FILE_HIDDEN )
      usRetMask |= FA_HIDDEN;
   if( usMask & FILE_READONLY )
      usRetMask |= FA_RDONLY;
   if( usMask & FILE_SYSTEM )
      usRetMask |= FA_SYSTEM;
   #endif

   return usRetMask;
}
#endif

#if defined( _MSC_VER )
   #if ! defined( DIRECTORY )
      #define DIRECTORY 0x08
   #endif
   #if ! defined( DRIVE )
      #define DRIVE     0x10
   #endif
#endif

HB_FATTR hb_fsGetFileAttributes( const char * szFile )
{
   #if defined( HB_OS_DOS )
      #if defined( __DJGPP__ ) || defined( __BORLANDC__ )
   if( szFile )
   {
      int iAttri = 0;

      #if defined( __BORLANDC__ ) && ( __BORLANDC__ >= 0x500 )
      /* NOTE: _chmod(  f, 0  ) => Get attribs
               _chmod(  f, 1, n  ) => Set attribs
               chmod(  ) though, _will_ change the attributes */
      iAttri = _rtl_chmod(  szFile, 0, 0  );
      #elif defined( __BORLANDC__ )
      iAttri = _chmod(  szFile, 0, 0  );
      #elif defined( __DJGPP__ )
      iAttri = _chmod(  szFile, 0  );
      #endif

      return ( HB_FATTR ) iAttri;
   }
   else
   {
      return ( HB_FATTR ) fsOldFiles.ff_attrib;
   }
      #endif

   #elif defined( HB_OS_WIN )
   {
      HB_FATTR dAttr;

      if( szFile )
      {
         LPCTSTR cFile = szFile;
         dAttr = GetFileAttributes( cFile );
      }
      else
      {
         dAttr = Lastff32.dwFileAttributes;
      }
      return ( HB_FATTR ) dAttr;

   }
   #else
   return ( HB_FATTR ) FA_ARCH;
   #endif
}

HB_FUNC( FILEATTR )
{
   if( ISCHAR( 1 ) )
      hb_retnl( hb_fsGetFileAttributes( hb_parcx( 1 ) ) );
   else
      hb_retnl( hb_fsGetFileAttributes( NULL ) );
}

HB_FUNC( SETFATTR )
{
   #if defined( HB_OS_DOS )
      #if defined( __DJGPP__ ) || defined( __BORLANDC__ )
   {

      int            iFlags;
      int            iReturn  = 0;
      const char *   szFile   = hb_parcx( 1 );

      if( ISNUM( 2 ) )
      {
         iFlags = hb_parni( 2 );
      }
      else
      {
         iFlags = 32;
      }

      if( ! ( iFlags & ~( FA_ARCH | FA_HIDDEN | FA_RDONLY | FA_SYSTEM ) ) )
      {
         iReturn = _chmod( szFile, 1, iFlags );
      }

      hb_retni( iReturn );
   }
      #endif

   #elif defined( HB_OS_WIN )
   {
      DWORD    dwFlags     = 0;
      DWORD    dwLastError = ERROR_SUCCESS;
      LPCTSTR  cFile       = hb_parcx( 1 );
      int      iAttr       = hb_parni( 2 );
      BOOL     lSuccess;

      if( iAttr & FA_RDONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;

      if( iAttr & FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;

      if( iAttr & FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;

      if( iAttr & FILE_ATTRIBUTE_ARCHIVE )
         dwFlags |= FILE_ATTRIBUTE_ARCHIVE;

	  if( iAttr & FA_PINNED )
         dwFlags |= FILE_ATTRIBUTE_PINNED;

	  if( iAttr & FA_UNPINNED )
         dwFlags |= FILE_ATTRIBUTE_UNPINNED;

	 
	 
      lSuccess = SetFileAttributes( cFile, dwFlags );

      if( lSuccess )
      {
         hb_retni( dwLastError );
      }
      else
      {
         dwLastError = GetLastError();

         switch( dwLastError )
         {
            case ERROR_FILE_NOT_FOUND:
               hb_retni( -2 );
               break;
            case ERROR_PATH_NOT_FOUND:
               hb_retni( -3 );
               break;
            case ERROR_ACCESS_DENIED:
               hb_retni( -5 );
               break;
         }
      }
   }

   #else
   {
      hb_retnl( -1 );
   }
   #endif
}

HB_FUNC( FILESEEK )
{
   BOOL bFound = FALSE;

   #if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
   {
      LPCTSTR szFile;

//    DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
//    int iAttr;

      if( hb_pcount() >= 1 )
      {
         szFile      = hb_parcx( 1 );
/*
         if ( ISNUM( 2 ) )
         {
            iAttr=hb_parnl( 2 );
         }
         else
         {
            iAttr=63;
         }

         if(  iAttr & FA_RDONLY  )
            dwFlags |= FILE_ATTRIBUTE_READONLY;

         if(  iAttr & FA_HIDDEN  )
            dwFlags |= FILE_ATTRIBUTE_HIDDEN;

         if(  iAttr & FA_SYSTEM  )
            dwFlags |= FILE_ATTRIBUTE_SYSTEM;

         if(  iAttr & FA_NORMAL  )
            dwFlags |=    FILE_ATTRIBUTE_NORMAL;
 */
         hLastFind   = FindFirstFile( szFile, &Lastff32 );

         if( hLastFind != INVALID_HANDLE_VALUE )
         {
            bFound = TRUE;
            hb_retc( Lastff32.cFileName );
         }
      }
      else
      {
         if( FindNextFile( hLastFind, &Lastff32 ) )
         {
            bFound = TRUE;
            hb_retc( Lastff32.cFileName );
         }
         else
         {
            FindClose( hLastFind );
            hLastFind = NULL;
         }
      }
   }

   #elif defined( HB_OS_DOS ) && ! defined( __WATCOMC__ )

   {
      int      iFind;
      char *   szFiles;
      int      iAttr;

      if( hb_pcount() >= 1 )
      {
         szFiles = hb_parcx( 1 );

         if( ISNUM( 2 ) )
         {
            iAttr = hb_parnl( 2 );
         }
         else
         {
            iAttr = 32;
         }

         iFind = findfirst( szFiles, &fsOldFiles, iAttr );

         if( ! iFind )
         {
            bFound = TRUE;
            hb_retc( fsOldFiles.ff_name );
         }
      }
      else
      {
         iFind = findnext( &fsOldFiles );
         if( ! iFind )
         {
            bFound = TRUE;
            hb_retc( fsOldFiles.ff_name );
         }
         #if ! defined ( __DJGPP__ )
         else
         {
            findclose( &fsOldFiles );
         }
         #endif
      }
   }

   #endif

   if( ! bFound )
   {
      hb_retc( "" );
   }
}

HB_FUNC( FILESIZE )
{
   #if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
   {
      HB_FOFFSET        dwFileSize;
      LPCTSTR           szFile;
      DWORD             dwFlags = FILE_ATTRIBUTE_ARCHIVE;
      HANDLE            hFind;
      WIN32_FIND_DATA   hFilesFind;
              

      int               iAttr;
      if( hb_pcount() >= 1 )
      {
         szFile = hb_parcx( 1 );


         if( ISNUM( 2 ) )
         {
            iAttr = hb_parnl( 2 );
         }
         else
         {
            iAttr = 63;
         }

         if( iAttr & FA_RDONLY )
            dwFlags |= FILE_ATTRIBUTE_READONLY;

         if( iAttr & FA_HIDDEN )
            dwFlags |= FILE_ATTRIBUTE_HIDDEN;

         if( iAttr & FA_SYSTEM )
            dwFlags |= FILE_ATTRIBUTE_SYSTEM;

         if( iAttr & FA_NORMAL )
            dwFlags |= FILE_ATTRIBUTE_NORMAL;

         hFind = FindFirstFile( szFile, &hFilesFind );

         if( hFind != INVALID_HANDLE_VALUE )
         {
            if( dwFlags & hFilesFind.dwFileAttributes )
            {
	           dwFileSize = ( HB_FOFFSET ) hFilesFind.nFileSizeLow + ( ( HB_FOFFSET ) hFilesFind.nFileSizeHigh << 32 ); 
//                if( hFilesFind.nFileSizeHigh > 0 )               
//                {
                  hb_retnint( dwFileSize);
//                }
//                else
//                {
//                   hb_retnint( hFilesFind.nFileSizeLow );
//                }
            }
            else
            {
               hb_retnl( -1 );
            }
            FindClose( hFind );
         }
         else
         {
            hb_retnl( -1 );
         }
      }
      else
      {
         //if( Lastff32.nFileSizeHigh > 0 )
        // {
            
            dwFileSize = ( HB_FOFFSET ) Lastff32.nFileSizeLow + ( ( HB_FOFFSET ) Lastff32.nFileSizeHigh << 32 ); 
        // }
        // else
         //{
         //   dwFileSize = Lastff32.nFileSizeLow;
        // }
         hb_retnint( dwFileSize );
      }
   }

#elif defined( HB_OS_DOS ) && ! defined( __WATCOMC__ )

   {
      int iFind;
      if( hb_pcount() > 0 )
      {
         char *         szFiles  = hb_parcx( 1 );
         int            iAttr    = 0;
         struct ffblk   fsFiles;

         if( ISNUM( 2 ) )
         {
            iAttr = hb_parni( 2 );
         }

         iFind = findfirst( szFiles, &fsFiles, iAttr );

         if( ! iFind )
            if( ( iAttr > 0 ) & ( iAttr & fsFiles.ff_attrib ) )
            {
               hb_retnl( fsFiles.ff_fsize );
            }
            else
            {
               hb_retnl( fsFiles.ff_fsize );
            }
         else
            hb_retnl( -1 );
      }
      else
      {
         hb_retnl( fsOldFiles.ff_fsize );
      }
   }

#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )

   {
      if( hb_pcount() > 0 )
      {
         const char *   szFile   = hb_parcx( 1 );
         USHORT         ushbMask = FA_ARCH;
         USHORT         usFileAttr;
#if  defined( HB_USE_LARGEFILE64 )
         struct stat64  sStat;
#else
         struct stat    sStat;
#endif

         if( ISNUM( 2 ) )
         {
            ushbMask = hb_parni( 2 );
         }
#if  defined( __USE_LARGEFILE64 ) || defined (HB_USE_LARGEFILE64 )
         if( stat64( szFile, &sStat  ) != -1 )
#else
         if( stat( szFile, &sStat  ) != -1 )
#endif
         {
            usFileAttr = osToHarbourMask( sStat.st_mode );

            if( ( ushbMask > 0 ) & ( ushbMask & usFileAttr ) )
               hb_retnint( sStat.st_size );
            else
               hb_retnint( sStat.st_size );
         }
         else
            hb_retnint( -1 );
      }
   }
   #else
   {
      hb_retnint( -1 );
   }
   #endif
}

HB_FUNC( FILEDATE )
{
#if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
   {
      LPCTSTR           szFile;
      DWORD             dwFlags = FILE_ATTRIBUTE_ARCHIVE;
      HANDLE            hFind;
      WIN32_FIND_DATA   hFilesFind;

      int               iAttr;

      if( hb_pcount() >= 1 )
      {
         szFile = hb_parcx( 1 );


         if( ISNUM( 2 ) )
         {
            iAttr = hb_parnl( 2 );
         }
         else
         {
            iAttr = 63;
         }

         if( iAttr & FA_RDONLY )
            dwFlags |= FILE_ATTRIBUTE_READONLY;

         if( iAttr & FA_HIDDEN )
            dwFlags |= FILE_ATTRIBUTE_HIDDEN;

         if( iAttr & FA_SYSTEM )
            dwFlags |= FILE_ATTRIBUTE_SYSTEM;

         if( iAttr & FA_NORMAL )
            dwFlags |= FILE_ATTRIBUTE_NORMAL;

         hFind = FindFirstFile( szFile, &hFilesFind );

         if( hFind != INVALID_HANDLE_VALUE )
         {
#if 0
            if( dwFlags & hFilesFind.dwFileAttributes )
               hb_retds( GetDate( &hFilesFind.ftLastWriteTime ) );
            else
               hb_retds( GetDate( &hFilesFind.ftLastWriteTime ) );
#else
            LPTSTR szDateString = GetDate( &hFilesFind.ftLastWriteTime );

            HB_SYMBOL_UNUSED( dwFlags );

            if( szDateString )
            {
               hb_retds( szDateString );
               hb_xfree( szDateString );
            }
            else
               hb_retds( "        " );
#endif
            FindClose( hFind );
         }
         else
            hb_retds( "        " );

      }
      else
      {
         LPTSTR szDateString = GetDate( &Lastff32.ftLastWriteTime );

         if( szDateString )
         {
            hb_retds( szDateString );
            hb_xfree( szDateString );
         }
         else
            hb_retds( "        " );
      }
   }
#elif defined( HB_OS_DOS ) && ! defined( __WATCOMC__ )
   {
      int iFind;
      if( hb_pcount() > 0 )
      {
         char *         szFiles  = hb_parcx( 1 );
         int            iAttr    = 0;
         struct ffblk   fsFiles;

         if( ISNUM( 2 ) )
            iAttr = hb_parni( 2 );

         iFind = findfirst( szFiles, &fsFiles, iAttr );

         if( ! iFind )
         {
            if( ( iAttr > 0 ) & ( iAttr & fsFiles.ff_attrib ) )
               hb_retd(  ( fsFiles.ff_fdate >> 9 ) + 1980,  ( ( fsFiles.ff_fdate & ~0xFE00 ) >> 5 ),  fsFiles.ff_fdate & ~0xFFE0 );
            else
               hb_retd(  ( fsFiles.ff_fdate >> 9 ) + 1980, ( ( fsFiles.ff_fdate & ~0xFE00 ) >> 5 ),  fsFiles.ff_fdate & ~0xFFE0 );
            hb_fsFindClose( iFind );
         }
         else
            hb_retds( "        " );
      }
      else
         hb_retd(  ( fsOldFiles.ff_fdate >> 9 ) + 1980,  ( ( fsOldFiles.ff_fdate & ~0xFE00 ) >> 5 ),  fsOldFiles.ff_fdate & ~0xFFE0 );
   }

#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )

   {
      if( hb_pcount() > 0 )
      {
         const char *   szFile   = hb_parcx( 1 );
#if  defined( HB_USE_LARGEFILE64 )
         struct stat64  sStat;
#else        
         struct stat    sStat;
#endif         
         time_t         tm_t     = 0;
         char           szDate[ 9 ];
         struct tm *    filedate;
         USHORT         ushbMask = FA_ARCH;
         USHORT         usFileAttr;

         if( ISNUM( 2 ) )
            ushbMask = hb_parni( 2 );
#if  defined( HB_USE_LARGEFILE64 )
         if( stat64( szFile, &sStat ) != -1 )
#else
         if( stat( szFile, &sStat ) != -1 )
#endif         
         {
            tm_t        = sStat.st_mtime;
            filedate    = localtime( &tm_t );
            hb_snprintf( szDate, sizeof( szDate ), "%04d%02d%02d", filedate->tm_year + 1900, filedate->tm_mon + 1, filedate->tm_mday );
            usFileAttr  = osToHarbourMask( sStat.st_mode );

            if( ( ushbMask > 0 ) & ( ushbMask & usFileAttr ) )
               hb_retds( szDate );
            else
               hb_retds( szDate );

         }
         else
         {
            hb_retds( "        " );
         }
      }
   }
   #else
   {
      hb_retds( "        " );
   }
   #endif
}

HB_FUNC( FILETIME )
{
#if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
   {
      LPTSTR            szDateString;
      LPCTSTR           szFile;
      DWORD             dwFlags = FILE_ATTRIBUTE_ARCHIVE;
      HANDLE            hFind;
      WIN32_FIND_DATA   hFilesFind;
      int               iAttr;

      if( hb_pcount() >= 1 )
      {
         szFile = hb_parcx( 1 );

         if( ISNUM( 2 ) )
         {
            iAttr = hb_parnl( 2 );
         }
         else
         {
            iAttr = 63;
         }

         if( iAttr & FA_RDONLY )
            dwFlags |= FILE_ATTRIBUTE_READONLY;

         if( iAttr & FA_HIDDEN )
            dwFlags |= FILE_ATTRIBUTE_HIDDEN;

         if( iAttr & FA_SYSTEM )
            dwFlags |= FILE_ATTRIBUTE_SYSTEM;

         if( iAttr & FA_NORMAL )
            dwFlags |= FILE_ATTRIBUTE_NORMAL;

         hFind = FindFirstFile( szFile, &hFilesFind );

         if( hFind != INVALID_HANDLE_VALUE )
         {
#if 0
            if( dwFlags & hFilesFind.dwFileAttributes )
               hb_retc( GetTime( &hFilesFind.ftLastWriteTime ) );
            else
               hb_retc( GetTime( &hFilesFind.ftLastWriteTime ) );
#else
            HB_SYMBOL_UNUSED( dwFlags );

            szDateString = GetTime( &hFilesFind.ftLastWriteTime );
            if( szDateString )
            {
               hb_retc( szDateString );
               hb_xfree( szDateString );
            }
            else
               hb_retc( "" );
#endif
            FindClose( hFind );
         }
         else
            hb_retc( "" );

      }
      else
      {
         szDateString = GetTime( &Lastff32.ftLastWriteTime );
         if( szDateString )
         {
            hb_retc( szDateString );
            hb_xfree( szDateString );
         }
         else
            hb_retc( "" );
      }
   }

#elif defined( HB_OS_DOS ) && ! defined( __WATCOMC__ )
   {

      char  szTime[ 9 ];
      int   iFind;

      if( hb_pcount() > 0 )
      {
         char *         szFiles  = hb_parcx( 1 );
         int            iAttr    = 0;
         struct ffblk   fsFiles;

         if( ISNUM( 2 ) )
         {
            iAttr = hb_parni( 2 );
         }

         iFind = findfirst( szFiles, &fsFiles, iAttr );

         if( ! iFind )
         {
            hb_snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d",
                         ( fsFiles.ff_ftime >> 11 ) & 0x1f,
                         ( fsFiles.ff_ftime >> 5 ) & 0x3f,
                         ( fsFiles.ff_ftime & 0x1f ) << 1 );
            hb_retc( szTime );
            hb_fsFindClose( iFind );
         }
         else
         {
            hb_retc( "" );
         }
      }
      else
      {
         hb_snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d",
                      ( fsOldFiles.ff_ftime >> 11 ) & 0x1f,
                      ( fsOldFiles.ff_ftime >> 5 ) & 0x3f,
                      ( fsOldFiles.ff_ftime & 0x1f ) << 1 );
         hb_retc( szTime );
      }
   }

#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 )

   {
      const char *   szFile   = hb_parcx( 1 );
#if  defined( HB_USE_LARGEFILE64 )
      struct stat64    sStat;
#else      
      struct stat    sStat;
#endif      
      time_t         tm_t     = 0;
      char           szTime[ 9 ];
      struct tm *    ft;
#  if defined( HB_USE_LARGEFILE64 )
      stat64( szFile, &sStat );
#else
      stat( szFile, &sStat );
#endif      
      tm_t  = sStat.st_mtime;
      ft    = localtime( &tm_t );
      hb_snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d", ft->tm_hour, ft->tm_min, ft->tm_sec );
      hb_retc( szTime );
   }

#else
   {
      hb_retc( "" );
   }
#endif

}

#if ( defined( HB_OS_WIN ) || defined( __MINGW32__ ) ) && ! defined( __CYGWIN__ ) && ! defined( __RSXNT__ )

#include <tchar.h>

static LPTSTR GetDate( FILETIME * rTime )
{
   static const LPTSTR  tszFormat = "yyyyMMdd";
   FILETIME             ft;
   int                  iSize;

   if( FileTimeToLocalFileTime( rTime, &ft ) )
   {
      SYSTEMTIME time;
      if( FileTimeToSystemTime( &ft, &time ) )
      {
         if( ( iSize = GetDateFormat( 0, 0, &time, tszFormat, NULL, 0 ) ) != 0 )
         {
            LPTSTR tszDateString;
            if( ( tszDateString = ( LPTSTR ) hb_xgrab( iSize + sizeof( TCHAR ) ) ) != NULL )
            {
               if( GetDateFormat( 0, 0, &time, tszFormat, tszDateString, iSize ) )
                  return tszDateString;

               hb_xfree( tszDateString );
            }
         }
      }
   }

   return NULL;
}

static LPTSTR GetTime( FILETIME * rTime )
{
   static const LPTSTR  tszFormat = "HH':'mm':'ss"; /*_T( "MM'\\'dd'\\'yyyy" );*/
   FILETIME             ft;

   if( FileTimeToLocalFileTime( rTime, &ft ) )
   {
      SYSTEMTIME time;
      if( FileTimeToSystemTime( &ft, &time ) )
      {
         int iSize;
         if( ( iSize = GetTimeFormat( 0, 0, &time, tszFormat, NULL, 0 ) ) != 0 )
         {
            LPTSTR tszDateString;
            if( ( tszDateString = ( LPTSTR ) hb_xgrab( iSize + sizeof( TCHAR ) ) ) != NULL )
            {
               if( GetTimeFormat( 0, 0, &time, tszFormat, tszDateString, iSize ) )
                  return tszDateString;

               hb_xfree( tszDateString );
            }
         }
      }
   }

   return NULL;
}
#endif

HB_FUNC( SETFDATI )
{
   if( hb_pcount() >= 1 )
   {
      PHB_ITEM       pDate, pTime;
      const char *   szFile   = hb_parcx( 1 );
      int            year     = 0, month = 0, day = 0, hour = 0, minute = 0, second = 0;

      pDate = hb_param( 2, HB_IT_DATE );
      if( ! pDate )
         pDate = hb_param( 3, HB_IT_DATE );
      if( pDate )
         hb_dateDecode( hb_itemGetDL( pDate ), &year, &month, &day );

      pTime = hb_param( 2, HB_IT_STRING );
      if( ! pTime )
         pTime = hb_param( 3, HB_IT_STRING );
      if( pTime )
         hb_timeStrGet( hb_itemGetCPtr( pTime ), &hour, &minute, &second, NULL );

#if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
      #if 1
      {
         long lJulian, lMillisec;

         lJulian     = pDate ? hb_dateEncode( year, month, day ) : -1;
         lMillisec   = pTime ? hb_timeStampEncode( hour, minute, second, 0 ) : -1;

         hb_retl( hb_fsSetFileTime( szFile, lJulian, lMillisec ) );
         return;
      }
      #else
      {
         FILETIME    ft, local_ft;
         SYSTEMTIME  st;
         HANDLE      f = ( HANDLE ) _lopen( szFile, OF_READWRITE | OF_SHARE_COMPAT );

         if( f != ( HANDLE ) HFILE_ERROR )
         {
            if( ! pDate || ! pTime )
            {
               GetLocalTime( &st );
            }
            if( pDate )
            {
               st.wYear    = ( WORD ) year;
               st.wMonth   = ( WORD ) month;
               st.wDay     = ( WORD ) day;
            }
            if( pTime )
            {
               st.wHour    = ( WORD ) hour;
               st.wMinute  = ( WORD ) minute;
               st.wSecond  = ( WORD ) second;
            }
            SystemTimeToFileTime( &st, &local_ft );
            LocalFileTimeToFileTime( &local_ft, &ft );
            hb_retl( SetFileTime( f, NULL, &ft, &ft ) );
            _lclose( ( HFILE ) f );
            return;
         }
      }
      #endif
#elif defined( HB_OS_OS2 )
      {
         FILESTATUS3 fs3;
         APIRET      ulrc;

         ulrc = DosQueryPathInfo( ( PCSZ ) szFile, FIL_STANDARD, &fs3, sizeof( fs3 ) );
         if( ulrc == NO_ERROR )
         {
            FDATE fdate;
            FTIME ftime;

            if( ! pDate || ! pTime )
            {
               DATETIME dt;

               DosGetDateTime( &dt );

               fdate.year     = dt.year - 1980;
               fdate.month    = dt.month;
               fdate.day      = dt.day;
               ftime.hours    = dt.hours;
               ftime.minutes  = dt.minutes;
               ftime.twosecs  = dt.seconds / 2;
            }

            if( pDate )
            {
               fdate.year  = year - 1980;
               fdate.month = month;
               fdate.day   = day;
            }
            if( pTime )
            {
               ftime.hours    = hour;
               ftime.minutes  = minute;
               ftime.twosecs  = second / 2;
            }
            fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
            fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
            ulrc              = DosSetPathInfo( ( PCSZ ) szFile, FIL_STANDARD,
                                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
         }
         hb_retl( ulrc == NO_ERROR );
         return;
      }
#elif defined( HB_OS_UNIX ) || defined( __DJGPP__ )

      if( ! pDate && ! pTime )
      {
         hb_retl( utime( szFile, NULL ) == 0 );
         return;
      }
      else
      {
         struct utimbuf buf;
         struct tm      new_value;

         if( ! pDate || ! pTime )
         {
            time_t current_time;

            current_time   = time( NULL );
#   if _POSIX_C_SOURCE < 199506L || defined( HB_OS_DARWIN_5 )
            new_value      = *localtime( &current_time );
#   else
            localtime_r( &current_time, &new_value );
#   endif
         }
         else
            memset( &new_value, 0, sizeof( new_value ) );

         if( pDate )
         {
#if defined( HB_OS_WIN ) && ! defined( __CYGWIN__ )
            new_value.tm_year = year;
            new_value.tm_mon  = month;
#else
            new_value.tm_year = year - 1900;
            new_value.tm_mon  = month - 1;
#endif
            new_value.tm_mday = day;
         }
         if( pTime )
         {
            new_value.tm_hour = hour;
            new_value.tm_min  = minute;
            new_value.tm_sec  = second;
         }
         buf.actime = buf.modtime = mktime( &new_value );
         hb_retl( utime( szFile, &buf ) == 0 );
         return;
      }
#else
      {
         long lJulian, lMillisec;

         lJulian     = pDate ? hb_dateEncode( year, month, day ) : -1;
         lMillisec   = pTime ? hb_timeStampEncode( hour, minute, second, 0 ) : -1;

         hb_retl( hb_fsSetFileTime( ( BYTE * ) szFile, lJulian, lMillisec ) );
         return;
      }
#endif
   }

   hb_retl( FALSE );
}

HB_FUNC( FILEDELETE )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      const char *   pDirSpec;
      PHB_FFIND      ffind;
      ULONG          ulAttr = HB_FA_ALL;
      char *         pszFree;

      pDirSpec = hb_fsNameConv( hb_parc( 1 ), &pszFree );
      if( ISNUM( 2 ) )
         ulAttr = hb_parnl( 2 );

      if( ( ffind = hb_fsFindFirst( pDirSpec, ulAttr ) ) != NULL )
      {
         PHB_FNAME pFilepath;

         pFilepath               = hb_fsFNameSplit( pDirSpec );
         pFilepath->szExtension  = NULL;

         do
         {
            char szPath[ HB_PATH_MAX ];

            pFilepath->szName = ffind->szName;
            hb_fsFNameMerge( szPath, pFilepath );

            if( hb_fsDelete( szPath ) )
               bReturn = TRUE;
         }
         while( hb_fsFindNext( ffind ) );

         hb_xfree( pFilepath );
         hb_fsFindClose( ffind );
      }
      if( pszFree )
         hb_xfree( pszFree );
   }

   hb_retl( bReturn );
}

HB_FUNC( FILESMAX )
{
#if defined( __DJGPP__ )
   __dpmi_regs r;
   unsigned    handles;
   ULONG       psp;

   r.h.ah   = 0x62;             /* Get PSP address */
   __dpmi_int( 0x21, &r );
   psp      = ( ( ( ULONG ) r.x.bx ) << 4 ) & 0xFFFFF;

   handles  = _farpeekw( _dos_ds, psp + 0x32 );
   hb_retni( handles );
#elif defined( _SC_OPEN_MAX )
   hb_retnl( sysconf( _SC_OPEN_MAX ) );
#else
   hb_retni( -1 );
#endif
}
