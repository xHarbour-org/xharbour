/*
 * $Id: filesys.c,v 1.83 2004/04/02 10:43:26 srobert Exp $
 */

/*
 * Harbour Project source code:
 * The FileSys API (C level)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_fsSetError()
 *    hb_fsSetDevMode()
 *    hb_fsReadLarge()
 *    hb_fsWriteLarge()
 *    hb_fsCurDirBuff()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_fsChDrv()
 *    hb_fsCurDrv()
 *    hb_fsIsDrv()
 *    hb_fsIsDevice()
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *            and David G. Holm <dholm@jsd-llc.com>
 *    hb_fsEof()
 *
 * Copyright 2001 Jose Gimenez (JFG) <jfgimenez@wanadoo.es>
 *                                   <tecnico.sireinsa@ctv.es>
 *    Added __WIN32__ check for any compiler to use the Win32
 *    API calls to allow openning an unlimited number of files
 *    simultaneously.
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *    hb_fsOpenProcess()
 *    hb_fsProcessValue()
 *    hb_fsCloseProcess()
 *    Unix sensible creation flags
 *    Thread safing and optimization
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET) will
         set the file pointer to the passed negative value, and the subsequent
         hb_fsWrite() call will fail. In CA-Clipper hb_fsSeek() will fail,
         the pointer will not be moved, and thus the hb_fsWrite() call will
         successfully write the buffer to the current file position. [vszakats]

   This has been corrected by ptucker
 */

#ifndef HB_OS_WIN_32_USED
   #define HB_OS_WIN_32_USED
#endif

#define HB_THREAD_OPTIMIZE_STACK

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hb_io.h"
#include "hbset.h"

#if defined(OS_UNIX_COMPATIBLE)
   #include <unistd.h>
   #include <signal.h>
   #include <sys/types.h>
   #include <sys/wait.h>
#endif


#if defined( X__WIN32__ )
   #if defined( _MSC_VER )

      #if _MSC_VER >= 1010
         __inline void * LongToHandle(  const long h )
         {
            return((void *) (INT_PTR) h );
         }

         __inline long HandleToLong( const void *h )
         {
            return((long) h );
         }
      #endif

      #if !defined( INVALID_SET_FILE_POINTER )
         #define INVALID_SET_FILE_POINTER ((DWORD)-1)
      #endif

   #endif
#endif

#if defined(__GNUC__) && !defined(__MINGW32__)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>

   #if defined(__CYGWIN__)
      #include <io.h>
   #endif

   #if defined(__DJGPP__)
      #include <dir.h>
      #define _getdrive getdisk
      #define _chdrive  setdisk
   #endif

   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif

   extern int rename( const char *, const char * );

#endif

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(_MSC_VER) || defined(__MINGW32__) || defined(__WATCOMC__)
   #include <sys/stat.h>
   #include <share.h>
   #include <fcntl.h>
   #include <direct.h>
   #include <process.h>
   #if defined(__BORLANDC__)
      #include <dir.h>
      #include <dos.h>
      #include <windows.h>
   #elif defined(__WATCOMC__)
      #include <dos.h>
   #endif

   #if defined(_MSC_VER) || defined(__MINGW32__)
      #include <sys/locking.h>
      #define ftruncate _chsize
      #if defined(__MINGW32__) && !defined(_LK_UNLCK)
         #define _LK_UNLCK _LK_UNLOCK
      #endif
      #if defined(_MSC_VER) && !defined(HAVE_POSIX_IO)
         #define HAVE_POSIX_IO
      #endif
   #else
      #define ftruncate chsize
      #if !defined(HAVE_POSIX_IO)
         #define HAVE_POSIX_IO
      #endif
   #endif
   #include <errno.h>
#endif

#if defined(__MPW__)
   #include <fcntl.h>
#endif

#if defined(HB_OS_DOS)
   #include <dos.h>
#endif

#ifndef O_BINARY
   #define O_BINARY     0       /* O_BINARY not defined on Linux */
#endif

#ifndef S_IEXEC
   #define S_IEXEC      0x0040  /* owner may execute <directory search> */
#endif


#ifndef S_IRWXU
   #define S_IRWXU      (S_IRUSR | S_IWUSR | S_IXUSR)
#endif

#ifndef S_IRUSR
   #define S_IRUSR      0x0100  /* owner may read */
#endif

#ifndef S_IWUSR
   #define S_IWUSR      0x0080  /* owner may write */
#endif

#ifndef S_IXUSR
   #define S_IXUSR      0x0040  /* owner may execute <directory search> */
#endif

#ifndef S_IRWXG
    #define S_IRWXG     (S_IRGRP | S_IWGRP | S_IXGRP)
#endif

#ifndef     S_IRGRP
    #define     S_IRGRP 0x00020 /* read permission, group */
#endif

#ifndef     S_IWGRP
    #define     S_IWGRP 0x00010 /* write permission, grougroup */
#endif

#ifndef     S_IXGRP
    #define     S_IXGRP 0x00008/* execute/search permission, group */
#endif

#ifndef     S_IRWXO
    #define     S_IRWXO    (S_IROTH | S_IWOTH | S_IXOTH)
#endif

#ifndef     S_IROTH
    #define     S_IROTH 0x00004 /* read permission, other */
#endif

#ifndef     S_IWOTH
    #define     S_IWOTH 0x00002 /* write permission, other */
#endif

#ifndef     S_IXOTH
    #define     S_IXOTH 0x00001/* execute/search permission, other */
#endif

#ifndef SH_COMPAT
   #define SH_COMPAT    0x00    /* Compatibility */
#endif

#ifndef SH_DENYRW
   #define SH_DENYRW    0x10    /* Deny read/write */
#endif

#ifndef SH_DENYWR
   #define SH_DENYWR    0x20    /* Deny write */
#endif

#ifndef SH_DENYRD
   #define SH_DENYRD    0x30    /* Deny read */
#endif

#ifndef SH_DENYNO
   #define SH_DENYNO    0x40    /* Deny nothing */
#endif

#ifndef HB_THREAD_SUPPORT
static USHORT s_uiErrorLast = 0;
static USHORT s_uiOsErrorLast = 0;
#else
#define s_uiErrorLast     HB_VM_STACK.uiErrorLast
#define s_uiOsErrorLast   HB_VM_STACK.uiOsErrorLast
#endif

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER) || defined(__MINGW32__)
/* Only compilers with Posix or Posix-like I/O support are supported */
   #define HB_FS_FILE_IO
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__IBMCPP__) || defined(__WATCOMC__)
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define HB_FS_SOPEN
#endif

#if ( defined(HAVE_POSIX_IO) && ( defined(HB_OS_OS2) || defined(HB_OS_DOS) || defined(HB_OS_WIN_32) ) && ! defined(__CYGWIN__) ) || defined(__MINGW32__)
/* These platforms and/or compilers have common drive letter support */
   #define HB_FS_DRIVE_LETTER
#endif

#if UINT_MAX == ULONG_MAX
   #define HB_FS_LARGE_OPTIMIZED
#else
   #define LARGE_MAX ( UINT_MAX - 1L )
#endif

#ifdef __WIN32__
   #if defined(X__WIN32__)
       extern int WintoDosError(DWORD dwError);
       HANDLE DostoWinHandle( FHANDLE fHandle);
   #endif
#endif
/* Convert HARBOUR flags to IO subsystem flags */

#if defined(HB_FS_FILE_IO)
extern char **environ;
static int convert_open_flags( USHORT uiFlags )
{
   /* by default FO_READ + FO_COMPAT is set */
   int result_flags = 0;

   HB_TRACE(HB_TR_DEBUG, ("convert_open_flags(%hu)", uiFlags));

   result_flags |= O_BINARY;
   HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_BINARY\n"));

#if defined(HB_FS_SOPEN)
   if( ( uiFlags & ( FO_WRITE | FO_READWRITE ) ) == FO_READ )
   {
      result_flags |= O_RDONLY;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDONLY\n"));
   }
#else
   if( ( uiFlags & ( FO_WRITE | FO_READWRITE ) ) == FO_READ )
   {
      result_flags |= ( O_RDONLY | SH_COMPAT );
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDONLY SH_COMPAT\n"));
   }
#endif

   /* read & write flags */
   if( uiFlags & FO_WRITE )
   {
      result_flags |= O_WRONLY;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_WRONLY\n"));
   }

   if( uiFlags & FO_READWRITE )
   {
      result_flags |= O_RDWR;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDWR\n"));
   }

#if ! defined(HB_FS_SOPEN) && ! defined( HB_OS_UNIX )
   /* shared flags */
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
   {
      result_flags |= SH_DENYRD;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYRD\n"));
   }

   else if( uiFlags & FO_EXCLUSIVE )
   {
      result_flags |= SH_DENYRW;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYRW\n"));
   }

   else if( uiFlags & FO_DENYWRITE )
   {
      result_flags |= SH_DENYWR;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYWR\n"));
   }

   if( uiFlags & FO_DENYNONE )
   {
      result_flags |= SH_DENYNO;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYNO\n"));
   }

   if( uiFlags & FO_SHARED )
   {
      result_flags |= SH_DENYNO;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYNO\n"));
   }
#endif

   HB_TRACE(HB_TR_INFO, ("convert_open_flags: result is 0x%04x\n", result_flags));

   return result_flags;
}

static int convert_seek_flags( USHORT uiFlags )
{
   /* by default FS_SET is set */
   int result_flags = SEEK_SET;

   HB_TRACE(HB_TR_DEBUG, ("convert_seek_flags(%hu)", uiFlags));

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}

static void convert_create_flags( USHORT uiFlags, int * result_flags, unsigned * result_pmode )
{
   HB_TRACE(HB_TR_DEBUG, ("convert_create_flags(%hu, %p, %p)", uiFlags, result_flags, result_pmode));

   /* by default FC_NORMAL is set */

   *result_flags = O_BINARY | O_CREAT | O_TRUNC | O_RDWR;
   //JC1: sorry, but at least under unix is more sensible to
   // create files without the X attribute
   *result_pmode = 0;
   if( uiFlags & FC_HIDDEN )
   {
      *result_pmode = S_IRUSR;
   }
   else
   {
      *result_pmode = S_IRUSR | S_IRGRP |  S_IROTH;
   }

   if( !( uiFlags & FC_READONLY) )
   {
      if( *result_pmode & S_IRUSR )  *result_pmode |= S_IWUSR;
      if( *result_pmode & S_IRGRP )  *result_pmode |= S_IWGRP;
      if( *result_pmode & S_IROTH )  *result_pmode |= S_IWOTH;
   }


   // JC1: give executable attribute where read is available
   if( uiFlags & FC_SYSTEM )
   {
      if( *result_pmode & S_IRUSR )  *result_pmode |= S_IXUSR;
      if( *result_pmode & S_IRGRP )  *result_pmode |= S_IXGRP;
      if( *result_pmode & S_IROTH )  *result_pmode |= S_IXOTH;
   }

   HB_TRACE(HB_TR_INFO, ("convert_create_flags: 0x%04x, 0x%04x\n", *result_flags, *result_pmode));
}

static void convert_create_flags_ex( USHORT uiAttr, USHORT uiFlags, int * result_flags, unsigned * result_pmode )
{
   HB_TRACE(HB_TR_DEBUG, ("convert_create_flags_ex(%hu, %hu, %p, %p)", uiAttr, uiFlags, result_flags, result_pmode));

   /* by default FC_NORMAL is set */

   /* *result_flags = O_BINARY | O_CREAT | O_TRUNC | O_RDWR; */
   *result_flags = convert_open_flags( uiFlags ) | O_BINARY | O_CREAT | O_TRUNC | O_RDWR;
   if ( uiFlags & FO_EXCL )
      *result_flags |= O_EXCL;

   *result_pmode = S_IRUSR | S_IWUSR;

   if( uiAttr & FC_READONLY )
   {
      *result_pmode = S_IRUSR;
      HB_TRACE(HB_TR_INFO, ("convert_create_flags_ex: S_IRUSR"));
   }

   if( uiAttr & FC_HIDDEN )
      *result_flags |= 0;

   if( uiAttr & FC_SYSTEM )
      *result_flags |= 0;

   HB_TRACE(HB_TR_INFO, ("convert_create_flags: 0x%04x, 0x%04x\n", *result_flags, *result_pmode));
}

#endif

BYTE HB_EXPORT * hb_filecase(char *str) {
   // Convert file and dir case. The allowed SET options are:
   // LOWER - Convert all caracters of file to lower
   // UPPER - Convert all caracters of file to upper
   // MIXED - Leave as is

   // The allowed environment options are:
   // FILECASE - define the case of file
   // DIRCASE - define the case of path
   // DIRSEPARATOR - define separator of path (Ex. "/")

   size_t a;
   char *filename;
   char *dirname=str;
   size_t dirlen;

   // Look for filename (Last "\" or DIRSEPARATOR)
   if( hb_set.HB_SET_DIRSEPARATOR != '\\' ) {
      for(a=0;a<strlen(str);a++)
         if( str[a] == '\\' )
            str[a] = hb_set.HB_SET_DIRSEPARATOR;
   }
   if(( filename = strrchr( str, hb_set.HB_SET_DIRSEPARATOR )) != NULL)
      filename++;
   else
      filename=str;
   dirlen=filename-str;

   // FILECASE
   if( hb_set.HB_SET_FILECASE == HB_SET_CASE_LOWER )
      hb_strLower( filename, strlen(filename) );
   else if( hb_set.HB_SET_FILECASE == HB_SET_CASE_UPPER )
      hb_strUpper( filename, strlen(filename) );

   // DIRCASE
   if( hb_set.HB_SET_DIRCASE == HB_SET_CASE_LOWER )
      hb_strLower(dirname,dirlen);
   else if( hb_set.HB_SET_DIRCASE == HB_SET_CASE_UPPER )
      hb_strUpper(dirname,dirlen);
   return (( BYTE * ) str);
}


/*
 * FILESYS.API FUNCTIONS --
 */

FHANDLE HB_EXPORT hb_fsPOpen( BYTE * pFilename, BYTE * pMode )
{
#if defined(OS_UNIX_COMPATIBLE)
   HB_THREAD_STUB
#endif

   FHANDLE hFileHandle;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPOpen(%p, %s)", pFilename, pMode));

   //JC1: Defaulting hFileHandle so compilers are happy (and code is more solid)
   hFileHandle = FS_ERROR;

#if defined(OS_UNIX_COMPATIBLE)
   {
#ifndef MAXFD
      #define MAXFD     1024
#endif
      FHANDLE hPipeHandle[2], hNullHandle;
      pid_t pid;
      BYTE * pbyTmp;
      BOOL bRead;
      ULONG ulLen;

      //JC1: unlocking the stack to allow cancelation points
      HB_STACK_UNLOCK;

      ulLen = strlen( pFilename );
      if( pMode && ( *pMode == 'r' || *pMode == 'w' ) )
         bRead = ( *pMode == 'r' );
      else
      {
         if( pFilename[0] == '|' )
            bRead = FALSE;
         else if( pFilename[ ulLen - 1 ] == '|' )
            bRead = TRUE;
         else
            bRead = FALSE;
      }

      if( pFilename[0] == '|' )
      {
          ++pFilename;
          --ulLen;
      }
      if( pFilename[ ulLen - 1 ] == '|' )
      {
          pbyTmp = hb_xgrab( ulLen );
          hb_xmemcpy( pbyTmp, pFilename, --ulLen );
          pbyTmp[ulLen] = 0;
          pFilename = pbyTmp;
      } else
          pbyTmp = NULL;

      errno = 0;
      if( pipe( hPipeHandle ) == 0 ) {
         if( ( pid = fork() ) != -1 ) {
            if( pid != 0 ) {
               if( bRead ) {
                  close( hPipeHandle[ 1 ] );
                  hFileHandle = hPipeHandle[ 0 ];
               } else {
                  close( hPipeHandle[ 0 ] );
                  hFileHandle = hPipeHandle[ 1 ];
               }
            } else {
               char *argv[4];
               argv[0] = "sh";
               argv[1] = "-c";
               argv[2] = ( char * ) pFilename;
               argv[3] = ( char * ) 0;
               hNullHandle = open("/dev/null", O_RDWR);
               if( bRead ) {
                  close( hPipeHandle[ 0 ] );
                  dup2( hPipeHandle[ 1 ], 1 );
                  dup2( hNullHandle, 0 );
                  dup2( hNullHandle, 2 );
               } else {
                  close( hPipeHandle[ 1 ] );
                  dup2( hPipeHandle[ 0 ], 0 );
                  dup2( hNullHandle, 1 );
                  dup2( hNullHandle, 2 );
               }
               for( hNullHandle = 3; hNullHandle < MAXFD; ++hNullHandle )
                  close(hNullHandle);
               setuid(getuid());
               setgid(getgid());
               execve("/bin/sh", argv, environ);
               exit(1);
            }
         }
         else
         {
            close( hPipeHandle[0] );
            close( hPipeHandle[1] );
         }
      }

      hb_fsSetError( errno );

      if( pbyTmp )
      {
         hb_xfree( pbyTmp );
      }
   }

   HB_STACK_LOCK;

#else

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( pMode );

   hb_fsSetError( (USHORT) FS_ERROR );

#endif

   return hFileHandle;
}

#ifndef X__WIN32__

int s_parametrize( char *out, char *in )
{
   int count = 0;  // we'll have at least one token

   // removes leading spaces
   while ( *in && isspace(*in) )
      in++;
   if (! *in ) return 0;

   while ( *in ) {
      if ( *in == '\"' || *in == '\'')
      {
         char quote = *in;
         in++;
         while ( *in && *in != quote ) {
            if ( *in == '\\' ) {
               in++;
            }
            if ( *in ) {
               *out = *in;
               out ++;
               in++;
            }
         }
         if (*in) {
            in++;
         }
         if ( *in ) {
            *out = '\0';
         }
         // out++ will be done later; if in is done,
         // '\0' will be added at loop exit.
      }
      else if (! isspace( *in ) ) {
         *out = *in;
         in++;
         out++;
      }
      else {
         *out = '\0';
         count ++;
         while (*in && isspace( *in ) )
            in++;
         out++;
      }
   }
   *out = '\0';
   count ++;

   return count;
}

char **s_argvize( char *params, int size )
{
   int i;
   char **argv = (char **) hb_xgrab( sizeof( char * ) * ( size + 1 ) );

   for( i = 0; i < size; i ++ )
   {
      argv[i] = params;
      while (*params) params++;
      params++;
   }
   return argv;
}

#endif

/*
JC1: Process Control functions
hb_fsOpenProcess creates a process and get the control of the 4 main
standard handlers. The handlers are returned in FHANDLE pointers;
each of them can be 0 if the owner process don't want to get
the ownership of that specific handler.

The process return a resource identificator that allows to
wait for the child process to be stopped. Incidentally, the
type of the process resource identificator is the same as
a file handle in all the systems were are using this functions:
in windows it is a HANDLE, while in unix is a PID_T (that is
an integer == file handle in all the GNU derived unces).

On success, a valid FHandle is returned, and FError returns
zero. On error, -1 is returned and FError() returns nonzero.
*/

FHANDLE HB_EXPORT hb_fsOpenProcess( char *pFilename,
      FHANDLE *fhStdin,
      FHANDLE *fhStdout,
      FHANDLE *fhStderr,
      BOOL bBackground
      )
{
   FHANDLE hRet = FS_ERROR;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpenProcess(%s, %p, %p, %p )",
      pFilename, fhStdin, fhStdout, fhStderr));

#if defined(OS_UNIX_COMPATIBLE) || ( defined( HB_OS_WIN_32 ) && ! defined( X__WIN32__) )
   {
#ifndef MAXFD
    #define MAXFD		1024
#endif
   FHANDLE hPipeIn[2], hPipeOut[2], hPipeErr[2];
   FHANDLE hNull;
   char **argv;
   int size;
   char *command;

   #ifdef HB_OS_WIN_32
   int pid;
   #define pipe(x)   _pipe( x, 2048, _O_BINARY )
   #else
   pid_t pid;
   #endif

   errno = 0;
   if( fhStdin != 0 && pipe( hPipeIn ) != 0 )
   {
      hb_fsSetError( errno );
      return (FHANDLE) -1;
   }

   if( fhStdout != 0 && pipe( hPipeOut ) != 0 )
   {
      hb_fsSetError( errno );
      if ( fhStdin != 0 )
      {
         close( hPipeIn[0] );
         close( hPipeIn[1] );
      }
      return (FHANDLE) -1;
   }


   if( fhStderr != 0 )
   {
      if( fhStderr != fhStdout )
      {
         if ( pipe( hPipeErr ) != 0)
         {
            hb_fsSetError( errno );
            if ( fhStdin != 0 )
            {
               close( hPipeIn[0] );
               close( hPipeIn[1] );
            }
            if ( fhStdout != 0 )
            {
               close( hPipeOut[0] );
               close( hPipeOut[1] );
            }
            return (FHANDLE) -1;
         }
      }
   }


   #ifdef HB_OS_WIN_32
   {
      int oldstdin, oldstdout, oldstderr;
      int iFlags;

      hNull = open("NUL:", O_RDWR);

      oldstdin = dup( 0 );
      oldstdout = dup( 1 );
      oldstderr = dup( 2 );

      if ( fhStdin != 0 )
      {
         dup2( hPipeIn[ 0 ], 0 );
      }
      else if ( bBackground )
      {
         dup2( hNull, 0 );
      }

      if ( fhStdout != 0 )
      {
         dup2( hPipeOut[ 1 ], 1 );
      }
      else if ( bBackground )
      {
         dup2( hNull, 1 );
      }

      if ( fhStderr != 0 )
      {
         if ( fhStderr != fhStdout )
         {
            dup2( hPipeErr[ 1 ], 2 );
         }
         else
         {
            dup2( hPipeOut[ 1 ], 2 );
         }
      }
      else if ( bBackground )
      {
         dup2( hNull, 2 );
      }

      command = ( char * )hb_xgrab( strlen(pFilename) + 2 );
      size = s_parametrize( command, pFilename );
      argv = s_argvize( command, size );
      argv[size] = 0;

      #if defined(__BORLANDC__) || defined(__WATCOMC__)
      iFlags = P_NOWAIT;
      pid = spawnvp( iFlags, argv[0], argv );
      #else
      iFlags = _P_NOWAIT;
      pid = _spawnvp( iFlags, argv[0], argv );
      #endif

      hb_xfree( command );
      hb_xfree( argv );

      dup2( oldstdin, 0 );
      dup2( oldstdout, 1 );
      dup2( oldstderr, 2 );
   }
   if ( pid < 0 )

   #else
   if( ( pid = fork() ) == -1 )
   #endif
   {
      hb_fsSetError( errno );
      // closing unused handles should be nice
      // TODO: check fs_Popen to close handles.
      if ( fhStdin != 0 )
      {
         close( hPipeIn[0] );
         close( hPipeIn[1] );
      }

      if ( fhStdout != 0 )
      {
         close( hPipeOut[0] );
         close( hPipeOut[1] );
      }

      if ( fhStderr != 0 && fhStderr != fhStdout )
      {
         close( hPipeErr[0] );
         close( hPipeErr[1] );
      }
      return (FHANDLE) -1;
   }

   if( pid != 0 ) {
      // I am the father
      if ( fhStdin != NULL )
      {
         *fhStdin = hPipeIn[1];
         close( hPipeIn[0] );
      }

      if ( fhStdout != NULL )
      {
         *fhStdout = hPipeOut[0];
         close( hPipeOut[1] );
      }

      if ( fhStderr != NULL && fhStderr != fhStdout )
      {
         *fhStderr = hPipeErr[0];
         close( hPipeErr[1] );
      }

      // father is done.
      hb_fsSetError( 0 );
      hRet = (FHANDLE) pid;

   }
   // I am che child
   #ifndef HB_OS_WIN_32
   else
   {
      command = hb_xgrab( strlen(pFilename) + 2 );
      size = s_parametrize( command, pFilename );
      argv = s_argvize( command, size );
      argv[size] = NULL;

/*
      // temporary solution
      char *argv[4];
      argv[0] = "sh";
      argv[1] = "-c";
      argv[2] = ( char * ) pFilename;
      argv[3] = ( char * ) 0; */
      // drop uncontrolled streams

      /* Initialize hNull to make compiler happy ;-) */
      hNull = bBackground ? open("/dev/null", O_RDWR) : FS_ERROR;

      // does father wants to control us?
      if ( fhStdin != NULL )
      {
         close( hPipeIn[ 1 ] ); // we don't write to stdin
         dup2( hPipeIn[ 0 ], 0 );
      }
      else if ( bBackground )
      {
         dup2( hNull, 0 );
      }

      if ( fhStdout != NULL )
      {
         close( hPipeOut[0] );
         dup2( hPipeOut[ 1 ], 1 );
      }
      else if ( bBackground )
      {
         dup2( hNull, 1 );
      }

      if ( fhStderr != NULL )
      {
         if ( fhStdout != fhStderr )
         {
            close( hPipeErr[0] );
            dup2( hPipeErr[ 1 ], 2 );
         }
         else
         {
            dup2( 1 , 2 );
         }
      }
      else if ( bBackground )
      {
         dup2( hNull, 2 );
      }

      if ( bBackground )
      {
         close( hNull );
      }

      /*
      for( hNull = 3; hNull < MAXFD; ++hNull )
         close(hNull);
      */

      // ????
      /*
       * This disable SUID and SGID, I added it for security to hb_fsPOpen
       * Just simply program can work using seteuid()/setegid() to access
       * database file what can cause that users cannot access database
       * file directly - only from program. When you run external program
       * with setuid(geteuid())/setgid(getegid()) then it inherits UID and
       * GID so is able to operate with their privileges. If this external
       * program is called without absolute path (beginning from "/") then
       * user can set his own PATH variable or change directory before
       * running xHarbour binaries to take control over EUID/EGID resources
       * Take a decision is it's important - maybe it should be set
       * by a parameter? Druzus.
       */
      /*
      setuid(getuid());
      setgid(getgid());*/

      execvp(argv[0], argv );
   }
   #endif
}

#elif defined( X__WIN32__ )
{
   STARTUPINFO si;
   PROCESS_INFORMATION proc;
   ULONG ulSize;
   // int iSize;
   // DWORD iRet;
   DWORD iFlags=0;
   char fullCommand[1024], cmdName[256];
   char *completeCommand, *pos;
   char *filePart;
   SECURITY_ATTRIBUTES secatt;

   HANDLE hPipeInRd=INVALID_HANDLE_VALUE, hPipeInWr=INVALID_HANDLE_VALUE;
   HANDLE hPipeOutRd=INVALID_HANDLE_VALUE, hPipeOutWr=INVALID_HANDLE_VALUE;
   HANDLE hPipeErrRd=INVALID_HANDLE_VALUE, hPipeErrWr=INVALID_HANDLE_VALUE;

   // prepare security attributes
   secatt.nLength = sizeof( secatt );
   secatt.lpSecurityDescriptor = NULL;
   secatt.bInheritHandle = TRUE;

   hb_fsSetError( 0 ); // reset error

   if ( fhStdin != NULL )
   {
      if ( !CreatePipe( &hPipeInRd, &hPipeInWr, &secatt, 0 ) )
      {
         hb_fsSetError( (USHORT) GetLastError() );
         return FS_ERROR;
      }
   }

   if ( fhStdout != NULL )
   {
      if ( !CreatePipe( &hPipeOutRd, &hPipeOutWr, &secatt, 0 ) )
      {
         hb_fsSetError( (USHORT) GetLastError() );
         hRet = FS_ERROR;
         goto ret_close_1;
      }
   }

   if ( fhStderr != NULL )
   {
      if ( fhStderr == fhStdout )
      {
         hPipeErrRd = hPipeOutRd;
         hPipeErrWr = hPipeOutWr;
      }

      if ( !CreatePipe( &hPipeErrRd, &hPipeErrWr, &secatt, 0 ) )
      {
         hb_fsSetError( (USHORT) GetLastError() );
         hRet = FS_ERROR;
         goto ret_close_2;
      }
   }

   // parameters are included in the command string
   pos = (char *) pFilename;
   while( *pos && *pos != ' ' && *pos != '\\' )
   {
      pos++;
   }

   ulSize = (unsigned) (pos - (char *)pFilename );
   if ( ulSize > 254 || *pos == '\\' )
   {
      // absolute path. We are ok
      strncpy( fullCommand, pFilename, 1023);
      fullCommand[1023] = '\0';
   }
   else
   {
      memcpy( cmdName, pFilename, ulSize );
      cmdName[ulSize+1] = 0;
      // find the command in the path
      SearchPath( NULL, cmdName, NULL, 1024, fullCommand, &filePart );
   }

   if ( *pos && *pos != '\\')
   {
      completeCommand = (char *) hb_xgrab( strlen( fullCommand ) + strlen( pos ) +2);
      sprintf( completeCommand, "%s %s",  fullCommand, pos+1);
   }
   else
   {
      completeCommand = (char *) hb_xgrab( strlen( fullCommand ) + 1);
      strcpy( completeCommand, fullCommand);
   }

   memset( &si, 0, sizeof( si ) );
   si.cb = sizeof( si );

   if ( bBackground )
   {
      // using show_hide AND using invalid handlers for unused streams
      si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
      si.wShowWindow = SW_HIDE;

      si.hStdInput = hPipeInRd;
      si.hStdOutput = hPipeOutWr;
      si.hStdError = hPipeErrWr;

      iFlags |= DETACHED_PROCESS;
   }
   else
   {
      si.dwFlags = STARTF_USESTDHANDLES;

      if ( fhStdin != NULL )
      {
         si.hStdInput = hPipeInRd;
      }
      else
      {
         si.hStdInput = GetStdHandle( STD_INPUT_HANDLE );
      }

      if ( fhStdout != NULL )
      {
         si.hStdOutput = hPipeOutWr;
      }
      else
      {
         si.hStdOutput = GetStdHandle( STD_OUTPUT_HANDLE );
      }

      if ( fhStderr != NULL )
      {
         si.hStdError = hPipeErrWr;
      }
      else
      {
         si.hStdError = GetStdHandle( STD_ERROR_HANDLE );
      }
   }

   if ( !CreateProcess( NULL,
      completeCommand,
      NULL,
      NULL,
      TRUE, //Inerhit handles!
      iFlags,
      NULL,
      NULL,
      &si,
      &proc
      ) )
   {
      hb_fsSetError( (USHORT) GetLastError() );
      hRet = FS_ERROR;
      hb_xfree( completeCommand );
      goto ret_close_3;
   }
   else
   {
      hb_xfree( completeCommand );
      hb_fsSetError( 0 );
      hRet = HandleToLong( proc.hProcess );

      if ( fhStdin != NULL )
      {
         *fhStdin = HandleToLong( hPipeInWr );
         CloseHandle( hPipeInRd );
      }
      if ( fhStdout != NULL )
      {
         *fhStdout = HandleToLong( hPipeOutRd );
         CloseHandle( hPipeOutWr );
      }
      if ( fhStderr != NULL )
      {
         *fhStderr = HandleToLong( hPipeErrRd );
         CloseHandle( hPipeErrWr );
      }

      CloseHandle( proc.hThread ); // unused
   }

   return hRet;

ret_close_3:
   if ( hPipeErrRd != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeErrRd );
   }
   if ( hPipeErrWr != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeErrWr );
   }

ret_close_2:
   if ( hPipeOutRd != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeOutRd );
   }
   if ( hPipeOutWr != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeOutWr );
   }

ret_close_1:
   if ( hPipeInRd != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeInRd );
   }
   if ( hPipeInWr != INVALID_HANDLE_VALUE )
   {
      CloseHandle( hPipeInWr );
   }
}

#else

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( fhStdin );
   HB_SYMBOL_UNUSED( fhStdout );
   HB_SYMBOL_UNUSED( fhStderr );

   hb_fsSetError( FS_ERROR );

#endif

   return hRet;
}

/*
   See if a process is still being executed. If bWait is true,
   the function will wait for the process to finish  before
   to return. When the process is terminated
   with a signal, the signal is returned as -signum. Notice that
   this does not apply to Windows.
*/

int HB_EXPORT hb_fsProcessValue( FHANDLE fhProc, BOOL bWait )
{
   HB_THREAD_STUB
   int iRetStatus = -1;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsProcessValue(%d, %d )", fhProc, bWait));

   hb_fsSetError( 0 );

#if defined(OS_UNIX_COMPATIBLE)
{
   int iStatus;

   if ( fhProc > 0 )
   {
      if ( ! bWait )
      {
         iRetStatus = waitpid( (pid_t) fhProc, &iStatus, WNOHANG );
      }
      else
      {
         HB_STACK_UNLOCK;
         iRetStatus = waitpid( (pid_t) fhProc, &iStatus, 0 );
         HB_STACK_LOCK;
      }
   }
   else
   {
      iRetStatus = 0;
   }

   #ifdef ERESTARTSYS
   if ( iRetStatus < 0 && errno != ERESTARTSYS)
   #else
   if ( iRetStatus < 0 )
   #endif
   {
      hb_fsSetError( errno );
      iRetStatus = -2;
   }
   else if ( iRetStatus == 0 )
   {
      iRetStatus = -1;
   }
   else
   {
      if( WIFEXITED( iStatus ) )
      {
         iRetStatus = WEXITSTATUS( iStatus );
      }
      else
      {
         iRetStatus = 0;
      }
   }
}
#elif defined( HB_OS_WIN_32 ) && ! defined( X__WIN32__ )
{
   int iPid;

   HB_SYMBOL_UNUSED( bWait );

   HB_STACK_UNLOCK
   HB_TEST_CANCEL_ENABLE_ASYN
   #ifdef __BORLANDC__
      iPid = cwait( &iRetStatus, (int) fhProc, 0 );
   #else
      iPid = _cwait( &iRetStatus, (int) fhProc, 0 );
   #endif
   HB_DISABLE_ASYN_CANC
   HB_STACK_LOCK;

   if ( iPid != (int) fhProc )
   {
      iRetStatus = -1;
   }
}
#elif defined( X__WIN32__ )
{
   DWORD dwTime;
   DWORD dwResult;

   if ( ! bWait )
   {
      dwTime = 0;
   }
   else
   {
      dwTime = INFINITE;
   }

   HB_STACK_UNLOCK
   HB_TEST_CANCEL_ENABLE_ASYN
   dwResult = WaitForSingleObject( DostoWinHandle(fhProc), dwTime );
   HB_DISABLE_ASYN_CANC
   HB_STACK_LOCK;

   if ( dwResult == WAIT_OBJECT_0 )
   {
      if ( GetExitCodeProcess( DostoWinHandle(fhProc), &dwResult ) )
      {
         iRetStatus = (int) dwResult;
         hb_fsSetError( 0 );
      }
      else
      {
         hb_fsSetError( (USHORT) GetLastError() );
         iRetStatus = -2;
      }
   }
   else
   {
      iRetStatus = -1;
   }
}
#else

   HB_SYMBOL_UNUSED( fhProc );
   HB_SYMBOL_UNUSED( bWait );

   hb_fsSetError( FS_ERROR );

#endif

   return iRetStatus;
}

/*
   Closes a process (that is, kill the application running the
   process); the handle is still valid until you
   catch it with hb_fsProcessValue. If bGentle is true, then
   a kind termination request is sent to the process, else
   the process is just killed.
   Retiurn
*/

BOOL HB_EXPORT hb_fsCloseProcess( FHANDLE fhProc, BOOL bGentle )
{
   BOOL bRet = FALSE;
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCloseProcess(%d, %d )", fhProc, bGentle));

   hb_fsSetError( 0 );

#if defined(OS_UNIX_COMPATIBLE)
   if ( fhProc > 0 ) {
      int iSignal = bGentle ? SIGTERM : SIGKILL;
      bRet = (kill( (pid_t) fhProc, iSignal ) == 0);
      if ( ! bRet )
      {
         hb_fsSetError( errno );
      }
   }

#elif defined( HB_OS_WIN_32 ) && !defined( X__WIN32__ )
{
   HANDLE hProc;

   hProc = OpenProcess( PROCESS_TERMINATE, FALSE, fhProc );

   if ( hProc != NULL )
   {
      bRet = (TerminateProcess( hProc, bGentle ? 0:1 ) != 0);
      if ( ! bRet )
      {
         hb_fsSetError( ( USHORT ) GetLastError() );
      }
      else
      {
         hb_fsSetError( 0 );
      }

   }
   else
   {
      hb_fsSetError( ( USHORT ) GetLastError() );
   }
}
#elif defined( X__WIN32__ )
   bRet = (TerminateProcess( DostoWinHandle( fhProc ), bGentle ? 0:1 ) != 0);
   if ( ! bRet )
   {
      hb_fsSetError( (USHORT) GetLastError() );
   }
   else
   {
      hb_fsSetError( 0 );
   }

#else

   HB_SYMBOL_UNUSED( fhProc );
   HB_SYMBOL_UNUSED( bGentle );
   hb_fsSetError( FS_ERROR );

#endif
   return bRet;
}


FHANDLE HB_EXPORT hb_fsOpen( BYTE * pFilename, USHORT uiFlags )
{
   HB_THREAD_STUB

   FHANDLE hFileHandle;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpen(%p, %hu)", pFilename, uiFlags));

   pFilename = hb_fileNameConv( hb_strdup( ( char * )pFilename) );

   // Unlocking stack to allow cancelation points
   HB_STACK_UNLOCK

#if defined(X__WIN32__)

   {
      DWORD dwFlags = 0;
      DWORD dwShare = FILE_SHARE_READ | FILE_SHARE_WRITE;
      HANDLE hFile;
      /* read & write flags */
      switch( uiFlags & 3)
      {
      case  FO_READWRITE :
         dwFlags |= GENERIC_READ | GENERIC_WRITE;
         break;

      case FO_WRITE:
         dwFlags |= GENERIC_WRITE;
         break;

      case FO_READ :
         dwFlags |= GENERIC_READ;
         break;
      }

/*      if( uiFlags & FO_READWRITE )
         dwFlags |= GENERIC_READ | GENERIC_WRITE;

      if( uiFlags & FO_WRITE )
         dwFlags |= GENERIC_WRITE;

      if( uiFlags & FO_READ )
         dwFlags |= GENERIC_READ;
*/

      /* shared flags */
      switch (uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE | FO_DENYNONE ) )
      {
         case FO_DENYREAD :
            dwShare = FILE_SHARE_WRITE;
            break;
         case FO_DENYWRITE :
            dwShare = FILE_SHARE_READ;
            break;
         case FO_EXCLUSIVE:
            dwShare = 0;
            break;
      }

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      hFile = ( HANDLE ) CreateFile( ( char * ) pFilename, dwFlags,
                dwShare, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

      if( hFile == ( HANDLE ) INVALID_HANDLE_VALUE )
      {
         hb_fsSetError( (USHORT) GetLastError() );
      }
      else
      {
         hb_fsSetError( 0 );
      }

      HB_DISABLE_ASYN_CANC

      hFileHandle=HandleToLong(hFile);
   }

#elif defined(_MSC_VER)

   {
      int iShare = _SH_DENYNO;

      if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
         iShare = _SH_DENYRD;

      else if( uiFlags & FO_EXCLUSIVE )
         iShare = _SH_DENYRW;

      else if( uiFlags & FO_DENYWRITE )
         iShare = _SH_DENYWR;

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      errno = 0;
      _doserrno = 0 ;
      if( iShare )
         hFileHandle = _sopen( ( char * ) pFilename, convert_open_flags( uiFlags ), iShare );
      else
         hFileHandle = _open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
      hb_fsSetError( _doserrno != 0 ? ( USHORT ) _doserrno : errno ) ;

      HB_DISABLE_ASYN_CANC
   }

#elif defined(HB_FS_SOPEN)

   {
      int iShare = SH_DENYNO;

      if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
         iShare = SH_DENYRD;

      else if( uiFlags & FO_EXCLUSIVE )
         iShare = SH_DENYRW;

      else if( uiFlags & FO_DENYWRITE )
         iShare = SH_DENYWR;

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      errno = 0;
      if( iShare )
         hFileHandle = sopen( ( char * ) pFilename, convert_open_flags( uiFlags ), iShare );
      else
         hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
      hb_fsSetError( errno );

      HB_DISABLE_ASYN_CANC
   }

#elif defined(HAVE_POSIX_IO)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN

   errno = 0;
   hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
   hb_fsSetError( errno );

   HB_DISABLE_ASYN_CANC
#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   hb_xfree( pFilename );

   return hFileHandle;
}

FHANDLE HB_EXPORT hb_fsCreate( BYTE * pFilename, USHORT uiAttr )
{
   HB_THREAD_STUB
   FHANDLE hFileHandle;

   #if defined(HB_FS_FILE_IO)
      int oflag;
      unsigned pmode;
   #endif

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%p, %hu)", pFilename, uiAttr));

   pFilename = hb_fileNameConv( hb_strdup( ( char * ) pFilename ) );

   HB_STACK_UNLOCK

#if defined(X__WIN32__)

   {
      DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;
      HANDLE hFile;
      if( uiAttr & FC_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;

      if( uiAttr & FC_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;

      if( uiAttr & FC_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      hFile = ( HANDLE ) CreateFile( ( char * ) pFilename,
                    GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
                    dwFlags, NULL );

      if( hFile == ( HANDLE ) INVALID_HANDLE_VALUE )
      {
         hb_fsSetError( (USHORT) GetLastError() );
      }
      else
      {
         hb_fsSetError( 0 );
      }

      HB_DISABLE_ASYN_CANC

      hFileHandle=HandleToLong(hFile);
   }

#elif defined(HB_FS_FILE_IO)

   convert_create_flags( uiAttr, &oflag, &pmode );

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN

   errno = 0;
   #if defined(_MSC_VER)
   _doserrno = 0;
   #endif

   hFileHandle = open( ( char * ) pFilename, oflag, pmode );

   #if defined(_MSC_VER)
   hb_fsSetError( _doserrno != 0 ? ( USHORT ) _doserrno : errno ) ;
   #else
   hb_fsSetError( errno );
   #endif

   HB_DISABLE_ASYN_CANC

#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   hb_xfree( pFilename );

   return hFileHandle;
}

/* Derived from hb_fsCreate()

   NOTE: The default opening mode differs from the one used in hb_fsCreate()
         [vszakats]
 */

FHANDLE HB_EXPORT hb_fsCreateEx( BYTE * pFilename, USHORT uiAttr, USHORT uiFlags )
{
   HB_THREAD_STUB
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreateEx(%p, %hu, %hu)", pFilename, uiAttr, uiFlags));

   pFilename = hb_filecase( hb_strdup( ( char * ) pFilename ) );
   hb_fsSetError( 0 );

   HB_STACK_UNLOCK

/* this function doesn't use WinAPI - it will be the source of problem
 * if someone will use it - it has to be fixed, Druzus
 */

#if defined(HB_FS_FILE_IO)

   convert_create_flags_ex( uiAttr, uiFlags, &oflag, &pmode );

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN

   errno = 0;
   hFileHandle = open( ( char * ) pFilename, oflag, pmode );
   hb_fsSetError( hFileHandle == -1 ? errno : 0 );

   HB_DISABLE_ASYN_CANC
#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   hb_xfree( pFilename );
   return hFileHandle;
}

void    HB_EXPORT hb_fsClose( FHANDLE hFileHandle )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", hFileHandle));


#if defined(HB_FS_FILE_IO)

   HB_STACK_UNLOCK

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN

   #if defined(X__WIN32__)
      hb_fsSetError( CloseHandle( DostoWinHandle( hFileHandle ) )
                     ? 0 : (USHORT) GetLastError() );
   #else
      errno = 0;
      hb_fsSetError( close( hFileHandle ) == 0 ? 0 : errno );
   #endif

   HB_DISABLE_ASYN_CANC

   HB_STACK_LOCK

#else

   hb_fsSetError( FS_ERROR );

#endif
}

void    HB_EXPORT hb_fsSetDevMode( FHANDLE hFileHandle, USHORT uiDevMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetDevMode(%p, %hu)", hFileHandle, uiDevMode));

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__DJGPP__) || defined(__CYGWIN__) || defined(__WATCOMC__)

   errno = 0;
   switch( uiDevMode )
   {
      case FD_BINARY:
         setmode( hFileHandle, O_BINARY );
         break;

      case FD_TEXT:
         setmode( hFileHandle, O_TEXT );
         break;
   }
   hb_fsSetError( errno );

#elif defined(_MSC_VER) || defined(__MINGW32__)

   errno = 0;
   switch( uiDevMode )
   {
      case FD_BINARY:
         _setmode( hFileHandle, _O_BINARY );
         break;

      case FD_TEXT:
         _setmode( hFileHandle, _O_TEXT );
         break;
   }
   hb_fsSetError( errno );

#elif defined( HB_OS_UNIX )

   HB_SYMBOL_UNUSED( hFileHandle );
   HB_SYMBOL_UNUSED( uiDevMode );
   hb_fsSetError( FS_ERROR );

#else

   hb_fsSetError( FS_ERROR );

#endif

}

USHORT  HB_EXPORT hb_fsRead( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   HB_THREAD_STUB
   USHORT uiRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRead(%p, %p, %hu)", hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   HB_STACK_UNLOCK


   #if defined(X__WIN32__)
      {
         DWORD dwRead ;
         BOOL bError;
         // allowing async cancelation here
         HB_TEST_CANCEL_ENABLE_ASYN

         bError = ReadFile( DostoWinHandle(hFileHandle), pBuff, (DWORD)uiCount, &dwRead, NULL );

         if (!bError)
         {
            hb_fsSetError( GetLastError() );
         }
         else
         {
            hb_fsSetError( 0 );
         }

         HB_DISABLE_ASYN_CANC

         uiRead = ( USHORT ) dwRead;
      }
   #else
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      uiRead = read( hFileHandle, pBuff, uiCount );
      hb_fsSetError( errno );
      HB_DISABLE_ASYN_CANC
   #endif

   if( uiRead == ( USHORT ) -1 )
      uiRead = 0;

   HB_STACK_LOCK
#else

   uiRead = 0;
   hb_fsSetError( FS_ERROR );

#endif

   return uiRead;
}

USHORT  HB_EXPORT hb_fsWrite( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   HB_THREAD_STUB
   USHORT uiWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWrite(%p, %p, %hu)", hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   HB_STACK_UNLOCK

   #if defined(X__WIN32__)
      {
         DWORD dwWritten = 0;
         BOOL bError;
         // allowing async cancelation here
         HB_TEST_CANCEL_ENABLE_ASYN

         if ( uiCount )
         {
             bError = WriteFile( DostoWinHandle(hFileHandle), pBuff, uiCount, &dwWritten, NULL );
         }
         else
         {
             dwWritten = 0;
             SetFilePointer( DostoWinHandle(hFileHandle), 0L, NULL, FILE_CURRENT );
             bError = SetEndOfFile( DostoWinHandle(hFileHandle) );
         }
         if (!bError)
         {
            hb_fsSetError( GetLastError() );
         }
         else
         {
            hb_fsSetError( 0 );
         }

         HB_DISABLE_ASYN_CANC

         uiWritten = ( USHORT ) dwWritten;
      }
   #else

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      errno = 0;
      if( uiCount )
      {
         uiWritten = write( hFileHandle, pBuff, uiCount );
         if( uiWritten == ( USHORT ) -1 )
            uiWritten = 0;
      }
      else
      {
         uiWritten = 0;
         ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) );
      }
      hb_fsSetError( errno );

      HB_DISABLE_ASYN_CANC

   #endif

   HB_STACK_LOCK

#else

   uiWritten = 0;
   hb_fsSetError( FS_ERROR );

#endif

   return uiWritten;
}

ULONG   HB_EXPORT hb_fsReadLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   HB_THREAD_STUB
   ULONG ulRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLarge(%p, %p, %lu)", hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   HB_STACK_UNLOCK

   #if defined(X__WIN32__)
      {
         BOOL bError;

         // allowing async cancelation here
         HB_TEST_CANCEL_ENABLE_ASYN

         hb_fsSetError( ReadFile( DostoWinHandle(hFileHandle), pBuff, ulCount, &ulRead, NULL )
                        ? 0 : GetLastError() );

         HB_DISABLE_ASYN_CANC
      }
   #elif defined(HB_FS_LARGE_OPTIMIZED)
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      ulRead = read( hFileHandle, pBuff, ulCount );
      hb_fsSetError( errno );
      HB_DISABLE_ASYN_CANC

   #else
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      {
         ULONG ulLeftToRead = ulCount;
         USHORT uiToRead;
         USHORT uiRead;
         BYTE * pPtr = pBuff;

         ulRead = 0;

         while( ulLeftToRead )
         {
            /* Determine how much to read this time */
            if( ulLeftToRead > ( ULONG ) INT_MAX )
            {
               uiToRead = INT_MAX;
               ulLeftToRead -= ( ULONG ) uiToRead;
            }
            else
            {
               uiToRead = ( USHORT ) ulLeftToRead;
               ulLeftToRead = 0;
            }

            // allowing async cancelation here

            uiRead = read( hFileHandle, pPtr, uiToRead );
            /* -1 on bad hFileHandle
                0 on disk full
             */

            if( uiRead == 0 )
               break;

            if( uiRead == ( USHORT ) -1 )
            {
               uiRead = 0;
               break;
            }

            ulRead += ( ULONG ) uiRead;
            pPtr += uiRead;
         }
      }
      hb_fsSetError( errno );
      HB_DISABLE_ASYN_CANC
   #endif

   HB_STACK_LOCK

#else

   ulRead = 0;
   hb_fsSetError( FS_ERROR );

#endif

   return ulRead;
}

ULONG   HB_EXPORT hb_fsWriteLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   HB_THREAD_STUB
   ULONG ulWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteLarge(%p, %p, %lu)", hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   #if defined(X__WIN32__)
   {
      BOOL bError;
      HB_STACK_UNLOCK

      if( ulCount )
      {
         bError = WriteFile( DostoWinHandle( hFileHandle), pBuff, ulCount, &ulWritten, NULL );
      }
      else
      {
          ulWritten = 0;
          SetFilePointer( DostoWinHandle(hFileHandle), 0L, NULL, FILE_CURRENT );
          bError = SetEndOfFile( DostoWinHandle(hFileHandle) );
      }

      HB_STACK_LOCK

      if( !bError )
      {
         hb_fsSetError( GetLastError() );
      }
      else
      {
         hb_fsSetError( 0 );
      }
   }
   #else
      HB_STACK_UNLOCK
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      errno = 0;
      if( ulCount )
      #if defined(HB_FS_LARGE_OPTIMIZED)
         {
            ulWritten = write( hFileHandle, pBuff, ulCount );
            if( ulWritten == ( ULONG ) -1 )
               ulWritten = 0;
         }
      #else
         {
            ULONG ulLeftToWrite = ulCount;
            USHORT uiToWrite;
            USHORT uiWritten;
            BYTE * pPtr = pBuff;

            ulWritten = 0;

            while( ulLeftToWrite )
            {
               /* Determine how much to write this time */
               if( ulLeftToWrite > ( ULONG ) INT_MAX )
               {
                  uiToWrite = INT_MAX;
                  ulLeftToWrite -= ( ULONG ) uiToWrite;
               }
               else
               {
                  uiToWrite = ( USHORT ) ulLeftToWrite;
                  ulLeftToWrite = 0;
               }

               uiWritten = write( hFileHandle, pPtr, uiToWrite );

               /* -1 on bad hFileHandle
                   0 on disk full
                */

               if( uiWritten == 0 )
                  break;

               if( uiWritten == ( USHORT ) -1 )
               {
                  uiWritten = 0;
                  break;
               }

               ulWritten += ( ULONG ) uiWritten;
               pPtr += uiWritten;
            }
         }
      #endif
      else
      {
         ulWritten = 0;
         ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) );
      }
      hb_fsSetError( errno );

      HB_DISABLE_ASYN_CANC
      HB_STACK_LOCK

   #endif

#else

   ulWritten = 0;
   hb_fsSetError( FS_ERROR );

#endif

   return ulWritten;
}

ULONG   HB_EXPORT hb_fsSeek( FHANDLE hFileHandle, LONG lOffset, USHORT uiFlags )
{
   HB_THREAD_STUB
   /* Clipper compatibility: under clipper, ulPos is returned as it was
      before; on error it is not changed. This is not thread compliant,
      but does not cares as MT prg are required to test FError(). */
   static ULONG ulPos = 0;
   USHORT Flags;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeek(%p, %ld, %hu)", hFileHandle, lOffset, uiFlags));

   Flags = convert_seek_flags( uiFlags );

   HB_STACK_UNLOCK

   if( lOffset < 0 && Flags == SEEK_SET )
   {
      /*
       * This is _BUGGY_ because 25 can be translated inside hb_fsSetError
       * ans it's not Clipper but some version of DOS compatibility!!!
       * I hate when someone try to fix his buggy .prg code which call
       * fseek with negative offset inside [x]Harbour internals.
       */
      hb_fsSetError( 25 ); /* 'Seek Error' */
      /* Clipper compatibility: do not touch ulPos */
   }
   else
   {

   #if defined(HB_OS_OS2)

      {
         ULONG ulOld = ulPos;
         APIRET ret = DosSetFilePtr( hFileHandle, lOffset, Flags, &ulPos );

         /* Clipper compatibility: on error reset lpos*/
         if( ret != 0 )
         {
            ulPos = ulOld;
         }
         hb_fsSetError(( USHORT ) ret );
      }

   #elif defined(HB_FS_FILE_IO)

      #if defined(X__WIN32__)
         // allowing async cancelation here
         HB_TEST_CANCEL_ENABLE_ASYN
         ulPos = (DWORD) SetFilePointer( DostoWinHandle(hFileHandle), lOffset, NULL, (DWORD)Flags );
         if( (DWORD)ulPos == INVALID_SET_FILE_POINTER )
         {
            hb_fsSetError( (USHORT) GetLastError() );
         }
         else
         {
            hb_fsSetError( 0 );
         }
         HB_DISABLE_ASYN_CANC

      #else
         // allowing async cancelation here
         {
            /* Clipper compatibility: do not touch ulPos */
            ULONG ulOld = ulPos;
            HB_TEST_CANCEL_ENABLE_ASYN
            errno = 0;
            ulPos = lseek( hFileHandle, lOffset, Flags );
            if( errno != 0 )
            {
               ulPos = ulOld;
            }
            hb_fsSetError( errno );
            HB_DISABLE_ASYN_CANC
         }
      #endif

   #else

      /* BUG - 25 can be translated to other value */
      hb_fsSetError( 25 );

   #endif

   }

   HB_STACK_LOCK

   return ulPos;
}

ULONG   HB_EXPORT hb_fsTell( FHANDLE hFileHandle )
{
   HB_THREAD_STUB
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsTell(%p)", hFileHandle));

   HB_STACK_UNLOCK

#if defined(HB_FS_FILE_IO)

   #if defined(X__WIN32__)
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      ulPos = (DWORD) SetFilePointer( DostoWinHandle(hFileHandle), 0, NULL, FILE_CURRENT );
      if( (DWORD)ulPos == INVALID_SET_FILE_POINTER )
      {
         hb_fsSetError( (USHORT) GetLastError() );
      }
      else
      {
         hb_fsSetError( 0 );
      }
      HB_DISABLE_ASYN_CANC

   #else
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
      hb_fsSetError( errno );
      HB_DISABLE_ASYN_CANC
   #endif


#else

   ulPos = 0;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   return ulPos;
}


BOOL HB_EXPORT hb_fsDelete( BYTE * pFilename )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", (char*) pFilename));

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)

   HB_TEST_CANCEL_ENABLE_ASYN
   bResult = DeleteFile( ( char * ) pFilename );
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HAVE_POSIX_IO)

   errno = 0;
   bResult = ( remove( ( char * ) pFilename ) == 0 );
   hb_fsSetError( bResult ? 0 : errno );

#elif defined(_MSC_VER) || defined(__MINGW32__)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   errno = 0;
   bResult = ( remove( ( char * ) pFilename ) == 0 );
   hb_fsSetError( bResult ? 0 : errno );
   HB_DISABLE_ASYN_CANC

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   return bResult;
}

BOOL HB_EXPORT hb_fsRename( BYTE * pOldName, BYTE * pNewName )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", (char*) pOldName, (char*) pNewName));

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   bResult = MoveFile( ( char * ) pOldName, ( char * ) pNewName );
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HB_FS_FILE_IO)

   errno = 0;
   bResult = ( rename( ( char * ) pOldName, ( char * ) pNewName ) == 0 );
   hb_fsSetError( bResult ? 0 : errno );

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif
   HB_STACK_LOCK

   return bResult;
}

BOOL HB_EXPORT    hb_fsLock   ( FHANDLE hFileHandle, ULONG ulStart,
                      ULONG ulLength, USHORT uiMode )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", hFileHandle, ulStart, ulLength, uiMode));

   HB_STACK_UNLOCK

#if defined(X__WIN32__)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
         bResult = LockFile( DostoWinHandle( hFileHandle ), ulStart,0, ulLength,0 );
         break;

      case FL_UNLOCK:
         bResult = UnlockFile( DostoWinHandle( hFileHandle ), ulStart,0, ulLength,0 );
         break;

      default:
         bResult = FALSE;
   }
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HB_OS_OS2)

   {
      struct _FILELOCK fl, ful;

      errno = 0;
      switch( uiMode & FL_MASK )
      {
      case FL_LOCK:

         fl.lOffset = ulStart;
         fl.lRange = ulLength;
         ful.lOffset = 0;
         ful.lRange = 0;

         /* lock region, 2 seconds timeout, exclusive access - no atomic */
         bResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
         break;

      case FL_UNLOCK:

         fl.lOffset = 0;
         fl.lRange = 0;
         ful.lOffset = ulStart;
         ful.lRange = ulLength;

         /* unlock region, 2 seconds timeout, exclusive access - no atomic */
         bResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
         break;

      default:
         bResult = FALSE;
      }
      hb_fsSetError( bResult ? 0 : errno );
   }

#elif defined(_MSC_VER)

   {
      ULONG ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            bResult = ( locking( hFileHandle, _LK_NBLCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetError( bResult ? 0 : errno );
      hb_fsSeek( hFileHandle, ulOldPos, FS_SET );
      HB_DISABLE_ASYN_CANC
   }

#elif defined(__MINGW32__)

   {
      ULONG ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      errno = 0;
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            bResult = ( _locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( _locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetError( bResult ? 0 : errno );
      hb_fsSeek( hFileHandle, ulOldPos, FS_SET );
      HB_DISABLE_ASYN_CANC
   }

#elif defined(__GNUC__) && defined(HB_OS_UNIX)

   {
      /* TODO: check for append locks (SEEK_END)
       */
      struct flock lock_info;

      errno = 0;
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle,
                               (uiMode & FLX_WAIT) ? F_SETLKW: F_SETLK,
                               &lock_info ) >= 0 );
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle, F_SETLK, &lock_info ) >= 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetError( bResult ? 0 : errno );
   }

#elif defined(HAVE_POSIX_IO) && !defined(__IBMCPP__) && ( !defined(__GNUC__) || defined(__DJGPP__) )

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   errno = 0;
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
         bResult = ( lock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      case FL_UNLOCK:
         bResult = ( unlock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      default:
         bResult = FALSE;
   }
   hb_fsSetError( bResult ? 0 : errno );
   HB_DISABLE_ASYN_CANC

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   return bResult;
}

void HB_EXPORT    hb_fsCommit( FHANDLE hFileHandle )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", hFileHandle));

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)
   {
      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN
      #if defined(X__WIN32__)
         hb_fsSetError( FlushFileBuffers( ( HANDLE ) DostoWinHandle( hFileHandle ) )
                        ? 0 : ( USHORT ) GetLastError() );
      #else
         #if defined(__WATCOMC__)
            hb_fsSetError( _dos_commit( hFileHandle ) == 0 ? 0 : errno );
         #else
            hb_fsSetError( _commit( hFileHandle ) == 0 ? 0 : errno );
         #endif
      #endif
      HB_DISABLE_ASYN_CANC
   }

#elif defined(HB_OS_OS2)

   {
      errno = 0;
      /* TODO: what about error code from DosResetBuffer() call? */
      DosResetBuffer( hFileHandle );
      hb_fsSetError( errno );
   }

#elif defined(HB_OS_UNIX)

   /* NOTE: close() functions releases all lock regardles if it is an
    * original or duplicated file handle
   */
   #if defined(_POSIX_SYNCHRONIZED_IO)
      /* faster - flushes data buffers only, without updating directory info
      */
      hb_fsSetError( fdatasync( hFileHandle ) == 0 ? 0 : errno );
   #else
      /* slower - flushes all file data buffers and i-node info
      */
      hb_fsSetError( fsync( hFileHandle ) == 0 ? 0 : errno );
   #endif

#elif defined(__WATCOMC__)

   hb_fsSetError( _dos_commit( hFileHandle ) == -1 ? errno : 0 );

#elif defined(HB_FS_FILE_IO) && !defined(HB_OS_OS2) && !defined(HB_OS_UNIX)

   /* This hack is very dangerous. POSIX standard define that if _ANY_
      file handle is closed all locks set by the process on the file
      pointed by this descriptor are removed. It doesn't matter they
      were done using different descriptor. It means that we now clean
      all locks on hFileHandle with the code below if the OS is POSIX
      compilant. I vote to disable it.
    */
   {
      int dup_handle;

      errno = 0;

      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
         close( dup_handle );

      hb_fsSetError( errno );
   }

#else

   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK
}

BOOL HB_EXPORT    hb_fsMkDir( BYTE * pDirname )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", (char*) pDirname));

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   bResult = CreateDirectory( ( char * ) pDirname, NULL );
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   #if !defined(__WATCOMC__) && !defined(__BORLANDC__) && !defined(__IBMCPP__) && !defined(__MINGW32__)
      bResult = ( mkdir( ( char * ) pDirname, S_IRWXU | S_IRWXG | S_IRWXO ) == 0 );
   #else
      bResult = ( mkdir( ( char * ) pDirname ) == 0 );
   #endif
   hb_fsSetError( bResult ? 0 : errno );

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   return bResult;
}

BOOL HB_EXPORT    hb_fsChDir( BYTE * pDirname )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", (char*) pDirname));

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN
   bResult = SetCurrentDirectory( ( char * ) pDirname );
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   bResult = ( chdir( ( char * ) pDirname ) == 0 );
   hb_fsSetError( bResult ? 0 : errno );

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   return bResult;
}

BOOL HB_EXPORT    hb_fsRmDir( BYTE * pDirname )
{
   HB_THREAD_STUB
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", (char*) pDirname));

   HB_STACK_LOCK

#if defined(HB_OS_WIN_32)

   HB_TEST_CANCEL_ENABLE_ASYN
   bResult = RemoveDirectory( ( char * ) pDirname );
   hb_fsSetError( bResult ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   bResult = ( rmdir( ( char * ) pDirname ) == 0 );
   hb_fsSetError( bResult ? 0 : errno );

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_UNLOCK

   return bResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

BYTE HB_EXPORT * hb_fsCurDir( USHORT uiDrive )
{
   static BYTE s_byDirBuffer[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDir(%hu)", uiDrive));

   hb_fsCurDirBuff( uiDrive, s_byDirBuffer, _POSIX_PATH_MAX + 1 );

   return ( BYTE * ) s_byDirBuffer;
}

/* NOTE: Thread safe version of hb_fsCurDir() */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

USHORT HB_EXPORT  hb_fsCurDirBuff( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%hu)", uiDrive));

   HB_SYMBOL_UNUSED( uiDrive );

   pbyBuffer[ 0 ] = '\0';

   HB_STACK_UNLOCK

#if defined(HB_OS_WIN_32)

   HB_TEST_CANCEL_ENABLE_ASYN
   hb_fsSetError( GetCurrentDirectory( ulLen, ( char * ) pbyBuffer )
                  ? 0 : (USHORT) GetLastError() );
   HB_DISABLE_ASYN_CANC
#elif defined(HAVE_POSIX_IO)

   errno = 0;
   hb_fsSetError( getcwd( ( char * ) pbyBuffer, ulLen ) ? 0 : errno );

#elif defined(__MINGW32__)

   errno = 0;
   _getdcwd( uiDrive, pbyBuffer, ulLen );
   hb_fsSetError( errno );

#else

   hb_fsSetError( FS_ERROR );

#endif

   HB_STACK_LOCK

   if ( hb_fsError() != 0 )
   {
      return hb_fsError();
   }

   /* Strip the leading drive spec, and leading backslash if there's one. */

   {
      BYTE * pbyStart = pbyBuffer;

      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

#if defined(OS_HAS_DRIVE_LETTER)
      if( pbyStart[ 1 ] == OS_DRIVE_DELIMITER )
         pbyStart += 2;
#endif
      if( strchr( OS_PATH_DELIMITER_LIST, pbyStart[ 0 ] ) )
         pbyStart++;

      if( pbyBuffer != pbyStart )
         memmove( pbyBuffer, pbyStart, ulLen );
   }

   /* Strip the trailing (back)slash if there's one */

   {
      ULONG ulLen = strlen( ( char * ) pbyBuffer );

      if( strchr( OS_PATH_DELIMITER_LIST, pbyBuffer[ ulLen - 1 ] ) )
         pbyBuffer[ ulLen - 1 ] = '\0';
   }

   return 0; // correct if it arrives here
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

USHORT HB_EXPORT  hb_fsChDrv( BYTE nDrive )
{
#if defined(HB_FS_DRIVE_LETTER)
   HB_THREAD_STUB
#endif

   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)
   HB_STACK_UNLOCK

 #if defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      UINT uiSave;
      UINT uiTotal;

      /* 1 = A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiSave );

      _dos_setdrive( nDrive + 1, &uiTotal );
      _dos_getdrive( &uiTotal );
      if( ( nDrive + 1 ) == uiTotal )
      {
         uiResult = 0;
         hb_fsSetError( 0 );
      }
      else
      {
         _dos_setdrive( uiSave, &uiTotal );
         uiResult = FS_ERROR;
         hb_fsSetError( FS_ERROR );
      }
   }
 #else
   {
      USHORT uiSave = _getdrive();

      // allowing async cancelation here
      HB_TEST_CANCEL_ENABLE_ASYN

      errno = 0;
      _chdrive( nDrive + 1 );
      if( ( nDrive + 1 ) == _getdrive() )
      {
         uiResult = 0;
         hb_fsSetError( errno );
      }
      else
      {
         _chdrive( uiSave );
         uiResult = (USHORT) FS_ERROR;
         hb_fsSetError( (USHORT) FS_ERROR );
      }
      HB_DISABLE_ASYN_CANC

   }
 #endif
    HB_STACK_UNLOCK

#else

   HB_SYMBOL_UNUSED( nDrive );
   uiResult = FS_ERROR;
   hb_fsSetError( FS_ERROR );

#endif

   return uiResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

/* TOFIX: This isn't fully compliant because CA-Cl*pper doesn't access
          the drive before checking. hb_fsIsDrv only returns TRUE
          if there is a disk in the drive. */

USHORT HB_EXPORT  hb_fsIsDrv( BYTE nDrive )
{
#if defined(HB_FS_DRIVE_LETTER)
   HB_THREAD_STUB
#endif

   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)
   HB_STACK_UNLOCK

 #if defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      UINT uiSave;
      UINT uiTotal;

      /* 1=  A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiSave );

      hb_fsSetError( 0 );
      uiResult = 0;
      _dos_setdrive( nDrive + 1, &uiTotal );
      _dos_getdrive( &uiTotal );
      if( ( nDrive + 1 ) != uiTotal )
      {
         hb_fsSetError( FS_ERROR );
         uiResult = FS_ERROR;
      }
      _dos_setdrive( uiSave, &uiTotal );
   }
 #else
   {
      USHORT uiSave = _getdrive();

      errno = 0;
      _chdrive( nDrive + 1 );
      if( ( nDrive + 1 ) == _getdrive() )
      {
         uiResult = 0;
         hb_fsSetError( errno );
      }
      else
      {
         uiResult = (USHORT) FS_ERROR;
         hb_fsSetError( (USHORT) FS_ERROR );
      }

      _chdrive( uiSave );
   }
 #endif
   HB_STACK_LOCK

#else

   HB_SYMBOL_UNUSED( nDrive );
   uiResult = FS_ERROR;
   hb_fsSetError( FS_ERROR );

#endif

   return uiResult;
}

BOOL   HB_EXPORT  hb_fsIsDevice( FHANDLE hFileHandle )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDevice(%p)", hFileHandle));

#if defined(HB_FS_DRIVE_LETTER)

   errno = 0;
   bResult = ( isatty( hFileHandle ) == 0 );
   hb_fsSetError( errno );

#else

   bResult = FALSE;
   hb_fsSetError( FS_ERROR );
   HB_SYMBOL_UNUSED( hFileHandle );

#endif

   return bResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

BYTE   HB_EXPORT  hb_fsCurDrv( void )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));

#if defined(HB_FS_DRIVE_LETTER)

   {
      errno = 0;
      uiResult = _getdrive();
      #if defined(__DJGPP__)
         /* _getdrive() returned a drive number, base 0. */
      #else
      if( uiResult < 65 )
      {
         /* _getdrive() returned a drive number, base 1. */
         uiResult--;
      }
      else
      {
         /* _getdrive() returned a drive letter. */
         uiResult -= 65;
      }
      #endif
      hb_fsSetError( errno );
   }

#elif defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      unsigned uiDrive;

      /* 1 = A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiDrive );
      hb_fsSetError( 0 );
      uiResult = ( USHORT ) uiDrive - 1;
   }

#else

   uiResult = 0;
   hb_fsSetError( FS_ERROR );

#endif

   return ( BYTE ) uiResult; /* Return the drive number, base 0. */
}

/* TODO: Implement hb_fsExtOpen */

FHANDLE HB_EXPORT  hb_fsExtOpen( BYTE * pFilename, BYTE * pDefExt,
                      USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsExtOpen(%s, %s, %hu, %p, %p)", (char*) pFilename, (char*) pDefExt, uiFlags, pPaths, pError));

   hb_fsSetError( (USHORT) FS_ERROR );

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( pDefExt );
   HB_SYMBOL_UNUSED( uiFlags );
   HB_SYMBOL_UNUSED( pPaths );
   HB_SYMBOL_UNUSED( pError );

   return hb_fsError();
}

BOOL HB_EXPORT hb_fsEof( FHANDLE hFileHandle )
{
#if defined(__DJGPP__) || defined(__CYGWIN__) || defined(OS_UNIX_COMPATIBLE)
   HB_THREAD_STUB
   LONG curPos;
   LONG endPos;
   LONG newPos;

   HB_STACK_UNLOCK

   // allowing async cancelation here
   HB_TEST_CANCEL_ENABLE_ASYN

   errno = 0;
   curPos = lseek( hFileHandle, 0L, SEEK_CUR );
   endPos = lseek( hFileHandle, 0L, SEEK_END );
   newPos = lseek( hFileHandle, curPos, SEEK_SET );

   if( newPos == -1L )
   {
      hb_fsSetError( errno );
   }
   else if( newPos != curPos )
   {
      hb_fsSetError( FS_ERROR );
   }

   HB_DISABLE_ASYN_CANC

   HB_STACK_LOCK

   return curPos >= endPos;
#else
   //JC1: Should not cause system lock
   return eof( hFileHandle ) != 0;
#endif
}

USHORT HB_EXPORT  hb_fsCurDirBuffEx( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen )
{
#if defined(HB_OS_WIN_32)
   HB_THREAD_STUB
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%hu)", uiDrive));

   HB_SYMBOL_UNUSED( uiDrive );

   pbyBuffer[ 0 ] = '\0';

#if defined(HB_OS_WIN_32)
{
   DWORD dwResult;

   HB_TEST_CANCEL_ENABLE_ASYN
   dwResult = GetCurrentDirectory( ulLen, ( char * ) pbyBuffer );
   HB_DISABLE_ASYN_CANC
   HB_STACK_LOCK

   if ( ! dwResult )
   {
      hb_fsSetError( ( USHORT ) GetLastError());
      return hb_fsError();
   }
   else
   {
      hb_fsSetError( 0 );
   }
}

#elif defined(HAVE_POSIX_IO)

   errno = 0;
   getcwd( ( char * ) pbyBuffer, ulLen );
   hb_fsSetError( errno );
   if ( errno )
   {
      return hb_fsError();
   }

#elif defined(__MINGW32__)

   errno = 0;
   _getdcwd( uiDrive, pbyBuffer, ulLen );
   hb_fsSetError( errno );
   if ( errno )
   {
      return hb_fsError();
   }

#else

   hb_fsSetError( FS_ERROR );
   return FS_ERROR;

#endif

   /* Strip the leading drive spec, and leading backslash if there's one. */

   {
      BYTE * pbyStart = pbyBuffer;

      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

#if defined(OS_HAS_DRIVE_LETTER)
      if( pbyStart[ 1 ] == OS_DRIVE_DELIMITER )
         pbyStart += 2;
#endif
      if( strchr( OS_PATH_DELIMITER_LIST, pbyStart[ 0 ] ) )
         pbyStart++;

      if( pbyBuffer != pbyStart )
         memmove( pbyBuffer, pbyStart, ulLen );
   }

   /* Strip the trailing (back)slash if there's one */

   {
      ULONG ulLen = strlen( ( char * ) pbyBuffer );

      if( strchr( OS_PATH_DELIMITER_LIST, pbyBuffer[ ulLen - 1 ] ) )
         pbyBuffer[ ulLen - 1 ] = '\0';
   }

   return 0; // if it reaches here, it is right.
}

#ifdef X__WIN32__
HANDLE DostoWinHandle( FHANDLE fHandle )
{
   HANDLE hHandle = LongToHandle( fHandle );

   switch (fHandle)
   {
      case 0:
        return GetStdHandle(STD_INPUT_HANDLE);

      case 1:
        return GetStdHandle(STD_OUTPUT_HANDLE);

      case 2:
        return GetStdHandle(STD_ERROR_HANDLE);

      default :
        return hHandle;
   }
}
#endif

int GnuErrtoDosErr( int ErrCode )
{

    int iResult = ErrCode;

#if defined(__DJGPP__) || defined(__RSX32__)
{

    if (ErrCode == EMFILE)
        iResult = 4 ;

    if (ErrCode == ESPIPE)
        iResult = 25;

    if (ErrCode == EACCES )
        iResult = 5  ;

    if (ErrCode == ENOENT)
        iResult = 2;

}
#elif defined(__MINGW32__)
{

    if (ErrCode == EMFILE)
        iResult = 4 ;

    if (ErrCode == ESPIPE)
        iResult = 25;

    if (ErrCode == EACCES )
        iResult = 5  ;

    if (ErrCode == ENOENT)
        iResult = 2;
}
#elif defined(__WATCOMC__)
{
   switch ( ErrCode )
   {
      case ENOENT:
         iResult = 2;
         break;
      case EMFILE:
         iResult = 4;
         break;
      case EACCES:
         iResult = 5;
         break;
      case EBADF:
         iResult = 6;
         break;
      case ESPIPE:
         iResult = 25;
         break;
      default:
         iResult = ErrCode;
         break;
   }
}
#elif defined(__GNUC__)
{

    if (ErrCode == EBADF)
        iResult = 6 ;

    if (ErrCode == EMFILE)
        iResult = 4 ;

    if (ErrCode == ESPIPE)
        iResult = 25;

    if (ErrCode == EACCES )
        iResult = 5  ;
}

#endif

    return iResult;
}


USHORT  HB_EXPORT hb_fsError( void )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsError()"));

   return s_uiErrorLast;
}

USHORT  HB_EXPORT hb_fsOsError( void )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsError()"));

   return s_uiOsErrorLast;
}

void  HB_EXPORT hb_fsSetError( USHORT uiError )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetError(%hu)", uiError));

/* Will lead to R/T if sharing violation for database !
   #if defined(_MSC_VER)
      if ( uiError == EBADF )
         uiError = 6;
      else if ( uiError == EACCES )
         uiError = 5;
   #endif
*/

   #if defined(X__WIN32__)
      s_uiErrorLast=WintoDosError(uiError);
   #else
      #if !defined( HB_OS_WIN_32 ) || defined(__WATCOMC__)
         s_uiErrorLast = GnuErrtoDosErr( uiError );
      #else
         s_uiErrorLast = uiError;
      #endif
   #endif
   s_uiOsErrorLast = uiError;

}

char HB_EXPORT * hb_fileTrim( BYTE * szFile)  /* Caller must free the buffer returned */
{

   char * szFileTrim ;
   ULONG ulPos;
   szFileTrim = (char * ) hb_xgrab( 255 );
   ulPos =  hb_strRTrimLen( (char*) szFile, strlen( (char*) szFile ), FALSE );
   szFile = (BYTE*) hb_strLTrim( (char*) szFile, &ulPos );
   strncpy( (char*) szFileTrim, (char*) szFile, ulPos );
   szFileTrim[ulPos]  = '\0';

   return szFileTrim;
}


BYTE HB_EXPORT * hb_fileNameConv(char *str) {
   // Convert file and dir case. The allowed SET options are:
   // LOWER - Convert all caracters of file to lower
   // UPPER - Convert all caracters of file to upper
   // MIXED - Leave as is

   // The allowed environment options are:
   // FILECASE - define the case of file
   // DIRCASE - define the case of path
   // DIRSEPARATOR - define separator of path (Ex. "/")

   size_t a;
   char *filename;
   char *dirname=str;
   size_t dirlen;
   char * szFileTrim =str;
   ULONG ulPos;
   if ( hb_set.HB_SET_TRIMFILENAME )
   {
      ulPos =  hb_strRTrimLen( szFileTrim, strlen( szFileTrim ), FALSE );
      szFileTrim = hb_strLTrim( szFileTrim, &ulPos );
      memcpy( str, szFileTrim, ulPos-1);
      str[ulPos] = '\0';
      dirname=str;
   }


   // Look for filename (Last "\" or DIRSEPARATOR)
   if( hb_set.HB_SET_DIRSEPARATOR != '\\' ) {
      for(a=0;a<strlen(str);a++)
         if( str[a] == '\\' )
            str[a] = hb_set.HB_SET_DIRSEPARATOR;
   }
   if(( filename = strrchr( str, hb_set.HB_SET_DIRSEPARATOR )) != NULL)
      filename++;
   else
      filename=str;
   dirlen=filename-str;

   // FILECASE
   if( hb_set.HB_SET_FILECASE == HB_SET_CASE_LOWER )
      hb_strLower( filename, strlen(filename) );
   else if( hb_set.HB_SET_FILECASE == HB_SET_CASE_UPPER )
      hb_strUpper( filename, strlen(filename) );

   // DIRCASE
   if( hb_set.HB_SET_DIRCASE == HB_SET_CASE_LOWER )
      hb_strLower(dirname,dirlen);
   else if( hb_set.HB_SET_DIRCASE == HB_SET_CASE_UPPER )
      hb_strUpper(dirname,dirlen);
   return (( BYTE * ) str);
}

