/*
 * $Id: disk.c,v 1.4 2004/02/26 18:58:52 lculik Exp $
 */
/*
 * xHarbour Project source code:
 * LibCT (Clipper Tools) Disk, File and Directory management.
 * 
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * DeleteFile()  - Ready. Source is in "disk.c"
 * DirMake()     - Ready. Already exist a MakeDir() function in xHarbour RT Library, but
 *                 this funtion return a more compatible error codes.
 * DirName()     - Ready.
 * DriveType()   - Ready.
 * FileMove()    - Ready.
 * 
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 * NUMDISKL()
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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
#include "hbapierr.h"
#include "hbapifs.h"

#if defined(HB_OS_WIN_32)

#include <windows.h>
#include <winbase.h>
#include <shellapi.h>

#define HB_OS_WIN_32_USED

#elif defined(HB_OS_DOS)

#include <dos.h>

#endif

/*  $DOC$
 *  $FUNCNAME$
 *      DELETEFILE()
 *  $CATEGORY$
 *      LIBCT File management
 *  $ONELINER$
 *      Delete an error-tolerant file.
 *  $SYNTAX$
 *      DELETEFILE(<cFileName>) --> nErrorCode
 *  $ARGUMENTS$
 *      <cFileName>  Designates which file name to delete.
 *  $RETURNS$
 *      DELETEFILE() returns a code that signifies its completion status:
 *
 *    Code    Symb. constants     Definition
 *       0      NO_DISK_ERR         No error occurs
 *      -2      ER_FILE_NOT_FOUND   File not found
 *      -3      ER_PATH_NOT_FOUND   Path not found
 *      -5      ER_ACCESS_DENIED    Access denied (e.g., file is read-only)
 *  $DESCRIPTION$
 *      In contrast to FILEDELETE(), which permits you to specify file groups
 *      with wildcards, DELETEFILE() only accepts specific file names.  However,
 *      the function avoids all DOS error messages and returns an error code
 *      directly to the calling program.  This makes error-tolerant erasures in
 *      networks possible (see Examples).
 *
 *      Note
 *
 *      You can use a drive designator and path name, but no
 *      wildcards.
 *  $EXAMPLES$
 *      How NOT to delete a file in a network environment:
 *
 *      IF FILE ("TEST.DBF")
 *       * Is it actually possible to delete the file?
 *       DELETE FILE TEST.DBF
 *      ENDIF
 *
 *      This is a better way:
 *
 *      nStatus  :=  DELETEFILE("TEST.DBF")
 *      IF nStatus == 0
 *       ? "File deleted."
 *      ELSE
 *       IF nStatus == -5
 *          ? "ACCESS DENIED!"
 *          ? "File in use elsewhere!"
 *       ENDIF
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *      FILEDELETE(),RENAMEFILE()
 *  $END$
 */
HB_FUNC ( DELETEFILE )
{
 
   BYTE * pFileName  = ( BYTE *) hb_parc( 1 ) ;
   int iRet;

   if ( hb_fsDelete( (BYTE*) pFileName) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *      DIRMAKE()
 *  $CATEGORY$
 *      LIBCT Directory management
 *  $ONELINER$
 *      Create a new directory
 *  $SYNTAX$
 *      DIRMAKE(<cDirectory>) --> nErrorCode
 *  $ARGUMENTS$
 *      <cDirectory>  Designates the new directory name to create.
 *  $RETURNS$
 *      DIRMAKE() returns a 0 when it has successfully created the directory;
 *      otherwise, an error code is passed.  The codes are defined as follows:
 *
 *      Code    Symb. constants     Definition
 *      0      NO_DISK_ERR         No error occurred
 *      -2      ER_FILE_NOT_FOUND   File not found
 *      -3      ER_PATH_NOT_FOUND   Path not found
 *      -5      ER_ACCESS_DENIED    Access denied (e.g., in network)
 *  $DESCRIPTION$
 *      When you install the program, directories must be the first thing you
 *      create.  The DIRMAKE() function allows you to create new directories
 *      from within your CA-Clipper application.
 *
 *      Note
 *
 *      <cDirectory> can contain a drive designator and a path,
 *      wildcards are not allowed.
 *  $EXAMPLES$
 *      Change to the "\DATA" directory:
 *
 *      IF DIRCHANGE("\DATA") == -3         // Path not found
 *        DIRMAKE("\DATA")
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *      DIRCHANGE(),DIRNAME(),DIRREMOVE()
 *  $END$
 */
HB_FUNC ( DIRMAKE )
{

   BYTE * pFileName  = ( BYTE *) hb_parc( 1 ) ;
   int iRet;

   if ( hb_fsMkDir( pFileName ) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *      DIRNAME()
 *  $CATEGORY$
 *      LIBCT Directory management
 *  $ONELINER$
 *      Determines the name of the current directory
 *  $SYNTAX$
 *      DIRNAME([<cDrive>]) --> cDirectory
 *  $ARGUMENTS$
 *      <cDrive>  Designates the drive for which the current directory is
 *      determined (a colon is unnecessary).  The default is the current drive.
 *  $RETURNS$
 *      DIRNAME() returns the current directory name on the <cDrive>.
 *  $DESCRIPTION$
 *      DIRNAME() determines the current directory name on the selected drive.
 *    	You can use this function to construct complete access paths.
 *
 *      Notes
 *
 *      The maximum length of the returned value is 65 characters.  If
 *      no drive designator is specified, the current drive is assumed.
 *
 *      If there is an invalid drive designation, DIRNAME() returns a
 *      null string.
 *  $EXAMPLES$
 *      Display the current directory name:
 *
 *      ? "Current Directory:  " + DIRNAME()
 *
 *      With a drive designator:
 *
 *      ? DIRNAME("A")  // Current directory on Drive A:
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *       DIRCHANGE() DIRMAKE() DIRREMOVE()
 *  $END$
 */
HB_FUNC ( DIRNAME )
{
   USHORT uiErrorOld = hb_fsError();
   BYTE * pbyBuffer = ( BYTE * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   hb_fsCurDirBuff( 0, pbyBuffer + 1, _POSIX_PATH_MAX  );
   pbyBuffer[0] = OS_PATH_DELIMITER;

   hb_retcAdopt( ( char * ) pbyBuffer );

   hb_fsSetError( uiErrorOld );
}


/*  $DOC$
 *  $FUNCNAME$
 *      DRIVETYPE()
 *  $CATEGORY$
 *      LIBCT Disk management
 *  $ONELINER$
 *      Determines the drive type.
 *  $SYNTAX$
 *      DRIVETYPE([<cDrive>]) --> nDriveType
 *  $ARGUMENTS$
 *      <cDrive>   Designates the drive (A, B, C, etc.) for which the type
 *      is determined.  The default is the current drive.
 *  $RETURNS$
 *      DRIVETYPE() returns a numeric value for the drive type.  The following
 *      codes apply:
 *
 *      Code  Definition
 *
 *      0     RAM Disk
 *      1     Floppy Disk (disk change cannot be established by system)
 *      2     Floppy Disk (disk change can be established by system)
 *      3     Hard Disk
 *      4     CD-Rom Drive  - xHarbour extension
 *      5     Network Drive - xHarbour extension
 *      9     Unknow Drive  - xHarbour extension
 *  $DESCRIPTION$
 *      This function determines if you are dealing with a floppy drive, a hard
 *      disk, RAM drive, CD-ROM or NETWORK disk.
 *
 *      Note
 *
 *      Although DRIVETYPE() returns a 0 value for logical DOS
 *      partitions, RAM floppies, and unavailable drives, you must also
 *      differentiate with DISKTYPE().  Here, each hard disk partition
 *      returns a 248 value.  A RAM floppy created with VDISK.SYS behaves
 *      like a single-sided floppy with eight sectors, which returns a 254
 *      value.  If there is an unavailable drive, the DISTYPE() returns a
 *      value of 0.
 *  $EXAMPLES$
 *       Determine if the drive is a RAM floppy:
 *
 *       IF DRIVETYPE() == 0 .AND. DISKTYPE() == 254
 *          ? "Drive " + DISKNAME() + ":  is a RAM-disk!"
 *       ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *      DISKTYPE(),DISKNAME()
 *  $END$
 */
HB_FUNC ( DRIVETYPE )
{
   #if defined(HB_OS_WIN_32)
      unsigned int uiType;
      char * pDrive = hb_parc( 1 );

      if ( strstr( pDrive, ":" ) == NULL )
      {
         strcat( pDrive, ":" ) ;
      }

      if ( strstr( pDrive, "\\" ) == NULL )
      {
         strcat( pDrive, "\\" ) ;
      }

      uiType = GetDriveType( pDrive );

      if ( uiType  == DRIVE_RAMDISK )
      {
         hb_retni( 0 );  // RAM Drive - Clipper compatible
      }
      else if (uiType == DRIVE_REMOVABLE )
      {
         hb_retni( 2 );  // Floppy Drive - Clipper compatible
      }
      else if (uiType == DRIVE_FIXED )
      {
         hb_retni( 3 );  // Hard Drive  - Clipper compatible
      }
      else if (uiType == DRIVE_CDROM )
      {
         hb_retni( 4 );  // CD-Rom Drive - xHarbour extension
      }
      else if (uiType == DRIVE_REMOTE )
      {
         hb_retni( 5 );  // Network Drive - xHarbour extension
      }
      else
      {
         hb_retni( 9 );  // Unknow Drive - xHarbour extension
      }
   #else
      hb_retni(9);
   #endif
}

/*  $DOC$
 *  $FUNCNAME$
 *    FILEMOVE()
 *  $CATEGORY$
 *    LIBCT File management
 *  $ONELINER$
 *      Move files to another directory.
 *  $SYNTAX$
 *    FILEMOVE(<cSourceFile>, <cTargetFile>) --> nErrorCode
 *  $ARGUMENTS$
 *    <cSourceFile>  Designates the name and the path of the source file.
 *
 *     <cTargetFile>  Designates the name and the path of the target file.
 *  $RETURNS$
 *      FILEMOVE() returns a value of 0 when the file can be moved; otherwise,
 *      an error code is returned.  The codes are defined below:
 *
 *      Code    Symb. constants        Definition
 *
 *      0       NO_DISK_ERR            No errors
 *      -2      ER_FILE_NOT_FOUND      File not found
 *      -3      ER_PATH_NOT_FOUND      Access path not found
 *      -5      ER_ACCESS_DENIED       Access denied (e.g., network)
 *      -17     ER_DIFFERENT_DEVICE    Target file not on same drive
 *  $DESCRIPTION$
 *      If a file is to be copied within a drive and then deleted it from the
 *      original position, it is quicker to move the file.  FILEMOVE() makes
 *      this possible.  The directory entries are also changed, which is much
 *      quicker than copying and deleting.
 *
 *    Notes
 *
 *      You can use drive designators and access paths in
 *      <cSourceFile> and <cTargetFile> file names.  Wildcards are not
 *      permitted.
 *
 *      You can only move a file within a drive.  If the target
 *      directory already contains a file with the same name as the one from
 *      <cSourceFile>, the move is not completed.  In this case, FILEMOVE()
 *      returns a value of -5.  The only option available in this situation,
 *      is to delete the file in the target directory.
 *  $EXAMPLES$
 *      Move a file from "\OLD" to "\NEW":
 *
 *      IF FILEMOVE("\OLD\CUST.DBF", "\NEW\CUST.DBF") = 0
 *       ? "The file is now in the \NEW directory"
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *    FILECOPY()
 *  $END$
 */
HB_FUNC ( FILEMOVE )
{
   BYTE * pSourceFile = ( BYTE *) hb_parc( 1 );
   BYTE *  pTargetFile = ( BYTE *) hb_parc( 2 );
   int iRet;

   if (hb_fsRename( pSourceFile , pTargetFile ) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMDISKL()
 *  $CATEGORY$
 *      CT3 disk functions
 *  $ONELINER$
 *      Gets the number of valid drive letters.
 *  $SYNTAX$
 *      NUMDISKL() --> nDisks
 *  $ARGUMENTS$
 *      None.
 *  $RETURNS$
 *      A numeric value which is the value of LASTDRIVE directive in CONFIG.SYS.
 *  $DESCRIPTION$
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      DJGPP, Windows, UNIX
 *  $FILES$
 *      Source is disk.c, library is libct.
 *  $SEEALSO$
 *      NUMDISKF(), NUMDISKH()
 *  $END$
 */
HB_FUNC( NUMDISKL )
#if defined( HB_OS_DOS )
#if defined( __DJGPP__ )
{
   unsigned cur_drive, n_drives;
   
   _dos_getdrive( &cur_drive );
   _dos_setdrive( cur_drive, &n_drives );
   hb_retni( n_drives );
}   
#else
{
   /* should be easily implementable somehow similar to DJGPP */
   hb_retni( 26 );
}
#endif
#elif defined( HB_OS_WIN_32 )
{
   /* LASTDRIVE does not affect Win32 apps, they always have 26 letters avail */
   hb_retni( 26 );
}
#else
{
   /* For Unix, return the most harmless value... or not? */
   hb_retni( 1 );
}
#endif
