/*
 * $Id: diskutil.prg v 1.0 2004/01/10 09:13:33 modalsist Exp $
 */
/*
 * xHarbour Project source code:
 * LibCT (Clipper Tools) Disk, File and Directory management.
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * www - http://www.xharbour.org
 *
 * DiskFormat()  - call dos/windows floppy format command.
 * DiskFree()    
 * DiskReady()   
 * DiskReadyW()  - Partially ready. See function for more details.
 * DiskTotal()   
 * DiskUsed()    - New function
 * FileValid()
 * FloppyType()  
 * RenameFile()  
 *
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

#include "fileio.ch"
#include "common.ch"
/*  $DOC$
 *  $FUNCNAME$
 *		DISKFORMAT()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Format a floppy disk drive in command prompt mode. Call the DOS Format command.
 *  $SYNTAX$
 *		DISKFORMAT( [<cDrive>] , [<cLabel>] , [<lBoot>] ,
 *                [<lQuickFormat>], [<cCapacity>] ) --> nDOSErrorCode
 *  $ARGUMENTS$
 *      <cDrive>  Designates the disk drive to format.  Only a floppy drive
 *      is permitted (A or B).  You may optionally specify a colon (:). The
 *      disk default is A.
 *
 *      <cLabel>  Designates the string label name to disk. Maximum len is 11
 *	 	characters.
 *
 *      <lBoot>  This optional parameter designates if the disk if bootable.
 *	 	if true the operational system is write to disk. Allowed only for
 *      Windows 95/98/ME. For WindowsNT/2000/XP this argument is ignored.
 *
 *      <lQuickFormat>  This optional parameter designates the quickly format
 *	 	capabilities of a operational system. Not recomended.
 *
 *   	<nCapacity>  This optionao argument, designates the appropriate disk
 *	 	capacity for the disk you are about to format.  Possible values are:
 *	 	160, 180, 320, 360, and 1200 for 5.25"; 720 and 1440 for 3.5" disks.
 *	 	The default is 0, the maximum drive capacity.
 *  $RETURNS$
 *      This FUNCTION return a DOSERROR() code. 0 if successfull.
 *  $DESCRIPTION$
 *      DiskFormat() call the format DOS command to format a disk. The Disformat()
 *		save and restore the screen before and after the format operation.
 *  $EXAMPLES$
 *		DiskFormat() --> Format disk "A" to maximum capacity.
 *		DiskFormat("B","Test") --> Format disk "B" with "Teste" label.
 *		DiskFormat("A","",.T.) --> Quick format the disk "A" without a label.
 *      DiskFormat("B",,,720) --> Full format the disk "A" with 720 K bytes.
 *  $TESTS$
 *		See examples.
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is not compatible with CA-Clipper Tools DiskFormat() FUNCTION.
 *  $PLATFORMS$
 *      DOS/WINDOWS
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *      DRIVETYPE(),FLOPPYTYPE()
 *  $END$
 */
**********************************************************************
FUNCTION DiskFormat( cDrive, cLabel , lBoot ,lQuickFormat , nCapacity)
**********************************************************************
   LOCAL cCommand,uScreen,nErr

   default( @cDrive, "A")          // floppy A
   default( @cLabel, " " )         // no label
   default( @lBoot, .F. )          // generates a boot disk (only for Win95/98/ME)
   default( @lQuickFormat , .F. )  // force full format.
   default( @nCapacity , 0 )       // max floppy capacity

   nErr := 0

   IF valtype( cDrive ) != "C"
      cDrive := "A"
   ENDIF

   IF valtype( cLabel ) != "C"
      cLabel := " "
   ENDIF

   IF valtype( lBoot ) != "L"
      lBoot := .F.
   ENDIF

   IF valtype( lQuickFormat ) != "L"
      lQuickFormat := .F.
   ENDIF
   
   IF valtype( nCapacity ) != "N"
      nCapacity := 0
   ENDIF


// Floppy disk capacities allowed are:

   IF nCapacity !=    0 .AND.;
      nCapacity !=  160 .AND.;
      nCapacity !=  180 .AND.;
      nCapacity !=  320 .AND.;
      nCapacity !=  360 .AND.;
      nCapacity !=  640 .AND.;
      nCapacity !=  720 .AND.;
      nCapacity != 1200 .AND.;
      nCapacity != 1440 .AND.;
      nCapacity != 2880

      nErr := 11 // invalid disk format

   ENDIF

   IF nErr == 0

      cDrive := Substr( cDrive,1,1)
      cLabel := Substr( cLabel,1,11)

      IF empty(cDrive) .OR. ( cDrive != "A" .AND. cDrive != "B" )
         nErr := 15 // invalid disk
      ENDIF

      IF nErr == 0 .AND. !DiskReady(cDrive)
         nErr := 21 // disk not ready
      ENDIF

      IF nErr == 0 .AND. !DiskReadyW(cDrive)
         nErr := 19 // disk is write protected.
      ENDIF

   ENDIF

   IF nErr == 0

      cCommand := "FORMAT "+cDrive+": /V:"+cLabel

      IF lQuickFormat
         cCommand += " /Q"
      ELSE
         IF WhatWin() < 4
            cCommand += " /U"  // Unconditional format. Only for Window95/98/ME.
           ENDIF
      ENDIF

      IF nCapacity > 0
         cCommand += " /F:"+alltrim(str(nCapacity))
      ENDIF

      IF lBoot .AND. WhatWin() < 4
         cCommand += " /S" // Only for Windows95/98/ME.
      ENDIF

      IF WhatWin() > 3 // WinNT/2000/XP
         cCommand += " /X" // force volume dismount IF necessary.
      ENDIF

	// Save previous screen before call dos/format command.
      uScreen := SaveScreen(0,0,maxrow(),maxcol())
      Run ( cCommand )
      nErr := DosError()
   // Restore previous screen after call dos/format commnad.
      RestScreen(0,0,maxrow(),maxcol(),uScreen)
   ENDIF

RETURN ( nErr )

/*  $DOC$
 *  $FUNCNAME$
 *		DISKFREE()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *      Determine the space available on a floppy or hard disk.
 *  $SYNTAX$
 *		DISKFREE([<cDrive>]) --> nFreeByte
 *  $ARGUMENTS$
 *      <cDrive>  Designates for which drive (A, B, C, etc.) the open
 *      capacity is determined.  The default value is the current drive.
 *  $RETURNS$
 *      DISKFREE() RETURNs the available memory capacity of the selected drive.
 *  $DESCRIPTION$
 *      DISKFREE() determines if a disk has sufficient storage capacity for a
 *      file.
 *
 *      Note
 *
 *      When <cDrive> is not specified, the FUNCTION automatically
 *      uses the current drive and if <cDrive> is out or open, then
 *      the FUNCTION RETURN a run time error.
 *
 *      The free space is determined with accessing the drive.
 *      The disk does have to be in the drive.
 *  $EXAMPLES$
 *      Determine if there is enough space to copy a database:
 *
 *      Required  :=  DBFSIZE()         // Database size
 *      IF DISKFREE("A") < Required
 *         ? "Insufficient disk space for copying!"
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		DISKTOTAL()
 *  $END$
 */

***************************
FUNCTION DiskFree( cDrive )
***************************

   default( @cDrive , DiskName() )

   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF

   IF at(":",cDrive)==0
      cDrive += ":"
   ENDIF
// hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
RETURN ( hb_DiskSpace( cDrive , HB_DISK_FREE ) )


/*  $DOC$
 *  $FUNCNAME$
 *		DISKREADY()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Tests to see if a disk drive is ready.
 *  $SYNTAX$
 *		DISKREADY([<cDrive>], [<lDOS/BIOS>]) --> lDiskReady
 *  $ARGUMENTS$
 *		<cDrive>  Designates the drive designator (A, B, C, etc.) of the
 *      drive to test.  The default is the current disk drive.
 *
 *      <lDOS/BIOS>  This logical parameter allows you to designate whether
 *      you want the FUNCTION tested through the BIOS (.F.) or DOS (.T.).  The
 *      default is discussed in the Description below.
 *  $RETURNS$
 *      DISKREADY() RETURNs .T. when the drive being tested is ready to use.
 *  $DESCRIPTION$
 *      To save space, you must frequently make multiple disk copies.
 *      DISKREADY() allows you to wait for a user disk change, without having to
 *      confront known DOS errors, or use CA-Clipper error trap FUNCTIONs.  Be
 *      sure to differentiate between floppy tests and "true" hard disks and
 *      logical partitions.
 *
 *      DOS or BIOS
 *
 *      In the simplest case, this FUNCTION tests the respective drive through
 *      DOS.  If drive B is unavailable, the message  "Please insert disk in
 *      drive B:..." is output from the operating system.  When you test floppy
 *      drives, drive A: and B:, the BIOS route is recommended, since no message
 *      is output.  In any event, A: and/or B: can be mapped drives within a
 *      network.  Use the following logical expression for all the previous
 *      situations:
 *
 *      (NETDISK(<cDrive>) .OR. <cDrive> >= "C")
 *
 *      This logical expression RETURNs .T. when the respective drive is either
 *      on the network or has a drive identifier of C: or higher.
 *  $EXAMPLES$
 *      The system waits between individual copy procedures until drive A: is
 *      ready again:
 *
 *      DO Copy                        // Call copy procedure
 *      ? "Please insert a disk in Drive A:!"
 *      DO WHILE .NOT. DISKREADY("A")
 *      *...
 *      ENDDO
 *      DO Copy                        // Call copy procedure
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		DISKREADYW()
 *  $END$
 */

************************************
FUNCTION DiskReady( cDrive , lMode )
************************************
   LOCAL lRETURN

   default( @cDrive , DiskName() )
   default( @lMode , .F. )
// lMode -> True = Windows/DOS mode. If a disk is not ready, open a dialog.
//          False = Bios mode. If a disk is not ready don´t open a dialog.

   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF

   IF valtype(lMode) != "L"
      lMode := .F.
   ENDIF

   IF lMode
      lRETURN := DiskChange( cDrive ) // Windows/Dos access mode. xHarbour RTL. Source
   ELSE                               // is in "dirdrive.c".
      lRETURN := IsDisk( cDrive )     // Bios access mode. xHarbour RTL. Source is in
   ENDIF                              // "dirdrive.c".

RETURN ( lRETURN )  // Bios access mode not implemented yet.


/*  $DOC$
 *  $FUNCNAME$
 *		DISKREADYW()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Queries whether you can write to a drive.
 *  $SYNTAX$
 *		DISKREADYW([<cDriveId>]), [<lDOS/BIOS>]) --> lDiskReady
 *  $ARGUMENTS$
 *      <cDriveId>  Designates which drive designator (A, B, C, etc.) to
 *      query.  The default value is the current disk drive.
 *
 *      <lDOS/BIOS>  This logical parameter allows you to specify whether
 *      you want to query the FUNCTION through the BIOS (.F.) or DOS (.T.).  The
 *      default is described in the Description below.
 *  $RETURNS$
 *      DISKREADYW() RETURNs .T. when you query a drive that is operational and
 *      can be written to.
 *  $DESCRIPTION$
 *      As with DISKREADY(), this FUNCTION determines if a drive is ready to
 *      use.  DISKREADYW() also determines if you can write to a drive.
 *      DISKREADY() cannot tell if a disk has a write-protect marker on it.  In
 *      this case, drive A: might be ready, but you could not write to it.  Use
 *      this FUNCTION to build write-protect detection for important disks into
 *      the program.
 *
 *		Network Drives
 *      
 *      In general, drives mapped within networks are viewed by the FUNCTION as
 *      accessible and RETURN .T. as a result.  For a disk, "accessible" means
 *      you can create, open, and delete files.  You can clearly differentiate
 *      these rights within networks such as Novell NETWARE.  Therefore, you
 *      must test for these accordingly.
 *      
 *		DOS or BIOS
 *
 *      In the simplest case, this FUNCTION always tests the respective drive
 *      through DOS.  If drive B: is unavailable, the message  "Please insert
 *      disk in drive B:" is output from the operating system.  When you test
 *      floppy drives drive A: and B:, the BIOS route is recommended, since no
 *      message is output.  In any event, A: and/or B: can be mapped drives
 *      within a network.  Use the following logical expression to cover all the
 *      previous situations:
 *
 *      (NETDISK(<cDrive>) .OR. <cDrive> >= "C")
 *
 *      This logical expression RETURNs .T. when the respective drive is either
 *      on the network or has a drive identifier of C: or higher.
 *      
 *		Note
 *
 *      Research indicates work in the OS/2 compatibility box must be
 *      in the BIOS mode.
 *  $EXAMPLES$
 *		Determine if you can write a file to A: drive:
 *
 *      IF DISKREADY("A")
 *         IF NETDISK ("A")
 *            IF NNETRIGHTS ("A:\")      // complete path
 *               * ...
 *            ENDIF
 *         ELSE
 *            IF DISKREADYW("A")
 *               ? "Disk is not write protected!"
 *            ELSE
 *               ? "Disk is write protected!"
 *            ENDIF
 *         ENDIF
 *      ELSE
 *            ? "Disk drive not ready!"
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		DISKREADY()
 *  $END$
 */

*************************************
FUNCTION DiskReadyW( cDrive , lMode )
*************************************
   LOCAL nHd,cFile,lRETURN

   default( @cDrive , DiskName() )
   default( @lMode  , .T. )
// lMode -> Windows/DOS write ready mode. Same as DiskReady().

   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF

   IF valtype(lMode) != "L"
      lMode := .T.
   ENDIF   

   IF lMode
      IF DiskChange( cDrive )
         cFile := "wwxxyyzz.xyz"
         nHd := FCreate( cFile , 0 )
         IF nHd > 0
            FClose( nHd )
            FErase( cFile )
            RETURN .T.
      ELSE
             RETURN .F.
      ENDIF
   ELSE
      RETURN .F.
   ENDIF
ENDIF

RETURN .F.

/*  $DOC$
 *  $FUNCNAME$
 *		DISKTOTAL()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Determines the total capacity of a floppy or hard disk.
 *  $SYNTAX$
 *		DISKTOTAL([<cDrive>]) --> nTotalSpace
 *  $ARGUMENTS$
 *      <cDrive>  Designates the drive for which memory capacity is
 *      determined.  The default is the current drive.
 *  $RETURNS$
 *      DISKTOTAL() RETURNs a value that corresponds to the total capacity of
 *      the data carrier in the selected drive.  The default is the current disk
 *      drive.
 *  $DESCRIPTION$
 *      DISKTOTAL() determines if the capacity of the disk in use is sufficient
 *      for back up.  The advantage of DISKTOTAL() over any capacity determined
 *      by the system, is that it recognizes bad sectors and does not include
 *      them. if <cDrive> is out or open, then the FUNCTION RETURN a run time
 *      error.
 *    
 *      Note
 *
 *      The total bytes is determined with accessing the drive. The disk does
 *      have to be in the drive.
 *  $EXAMPLES$
 *      Determine if disk capacity is sufficient:
 *
 *      nRequired  :=  DBFSIZE()
 *      IF DISKTOTAL("A") < nRequired
 *         ? "Error! Insufficient disk capacity!"
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		DISKFREE()
 *  $END$
 */

****************************
FUNCTION DiskTotal( cDrive )
****************************

   Default( @cDrive , DiskName() )
   
   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF
   
   IF at(":",cDrive) == 0
      cDrive += ":"
   ENDIF

// hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
RETURN ( hb_DiskSpace( cDrive , HB_DISK_TOTAL ) )

/*  $DOC$
 *  $FUNCNAME$
 *		DISKUSED()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Determines the used space on a floppy or hard disk.
 *  $SYNTAX$
 *		DISKUSED([<cDrive>]) --> nFreeByte
 *  $ARGUMENTS$
 *      <cDrive>  Designates for which drive (A, B, C, etc.) the used
 *      capacity is determined.  The default value is the current drive.
 *  $RETURNS$
 *      DISKUSED() RETURNs the used memory capacity of the selected drive.
 *  $DESCRIPTION$
 *      DISKUSED() determines the ammount of used space storage for a
 *      file.
 *
 *      Note
 *
 *      When <cDrive> is not specified, the FUNCTION automatically
 *      uses the current drive and if <cDrive> is out or open, then
 *      the FUNCTION RETURN a run time error.
 *
 *      The used space is determined with accessing the drive.
 *      The disk does have to be in the drive.
 *  $EXAMPLES$
 *      Determine used space to a disk:
 *
 *      DISKUSED("A") --> nBytes
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *      DISKTOTAL(),DISKFREE()
 *  $END$
 */

***************************
FUNCTION DiskUsed( cDrive )
***************************

   Default( @cDrive , DiskName() )

   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF

   IF at(":",cDrive) == 0
      cDrive += ":"
   ENDIF
// hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
RETURN (  hb_DiskSpace( cDrive , HB_DISK_TOTAL ) - hb_DiskSpace( cDrive , HB_DISK_FREE ) )


/*  $DOC$
 *  $FUNCNAME$
 *		FILEDELETE()
 *  $CATEGORY$
 *      LIBCT File management
 *  $ONELINER$
 *		Deletes file(s) by name and attribute
 *  $SYNTAX$
 *		FILEDELETE(<cFileMask>, [<nFileAttr>]) --> lDeleted
 *  $ARGUMENTS$
 *   	<cFileMask>  Designates the file or files to delete.
 *
 *      <nFileAttr>  Designates one of the following file attributes.  The
 *      default value is 32.
 *
 *      Value     Symb. constants   Definition
 *      0         FA_NORMAL
 *      1         FA_READONLY       Read-only
 *      2         FA_HIDDEN         HIDDEN (Concealed files)
 *      4         FA_SYSTEM         SYSTEM (System files)
 *      8         FA_VOLUME         VOLUME (Name of floppy/hard disk)
 *      32        FA_ARCHIVE        ARCHIVE (Changed since last backup)
 *  $RETURNS$
 *      FILEDELETE() RETURNs .T. when at least one or more files are deleted;
 *      otherwise, .F. is RETURNed.
 *  $DESCRIPTION$
 *      Occasionally, you will need to clean up a floppy or a hard disk or
 *      delete whole file groups.  This FUNCTION deletes entire file groups with
 *      a FUNCTION call, without using the RUN command.
 *
 *		Notes
 *
 *      Warning!  FILEDELETE() can also delete system files.
 *
 *      The default standard attribute is archive (32).  Drive and
 *      path designations, and wildcards are permitted.
 *
 *      Subdirectories must be deleted with the help of the
 *      DIRREMOVE() FUNCTION.
 *  $EXAMPLES$
 *   	Attempt to delete all index files, then display the completion status:
 *      
 *      IF FILEDELETE("*.NDX")
 *         ? "Files deleted."
 *      ELSE
 *         ? "No files found."
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is "diskutil.prg". Library is "libct.lib".
 *  $SEEALSO$
 *		DELETEFILE()
 *  $END$
 */


/*  $DOC$
 *  $FUNCNAME$
 *		FILEVALID()
 *  $CATEGORY$
 *		LIBCT File management
 *  $ONELINER$
 *		Tests whether a string has a valid file name
 *  $SYNTAX$
 *		FILEVALID(<cFileName>) --> lValid
 *  $ARGUMENTS$
 *		<cFileName>  Designates the file name you want to test for validity.
 *  $RETURNS$
 *      FILEVALID() RETURNs .T. when the designated name is valid.
 *  $DESCRIPTION$
 *      FILEVALID() determines if a string contains a valid file name.  You
 *      could also test the validity of user input in this way.  If necessary,
 *      path and drive designations must be removed from the string.  The
 *      tokenizer described in the string chapter is available for this purpose
 *      (see Examples).
 *  $EXAMPLES$
 *      The last token in a string with drive and path designations should
 *      contain the file name:
 *
 *      ACCEPT "Target_File  " TO cVar
 *      cFileName  := TOKEN(cVar, " \:")        // Last token
 *      DO WHILE .NOT. FILEVALID(cFileName)
 *         * Error message ...
 *         ACCEPT "Target_File  " TO cVar
 *         cFileName  := TOKEN(cVar, "\:")      // Last token
 *      ENDDO
 *  $TESTS$
 *  $STATUS$
 *    Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		TOKEN()
 *  $END$
 */

********************************
FUNCTION FileValid ( cFileName )
********************************
   LOCAL nHandle

   nHandle := FOpen( cFileName , FO_READWRITE )

   IF nHandle != 0
      FClose( nHandle )
   ENDIF

RETURN ( IIF( nHandle > 0 , .T. , .F. ) )


/*  $DOC$
 *  $FUNCNAME$
 *		FLOPPYTYPE()
 *  $CATEGORY$
 *		LIBCT Disk management
 *  $ONELINER$
 *		Determines the exact type of floppy drive
 *  $SYNTAX$
 *		FLOPPYTYPE([<cDrive>]) --> nFloppyType
 *  $ARGUMENTS$
 *      <cDrive>  Designates the drive (A, B, C, etc.) for which you want a
 *      type determined.  The default is the current disk drive.
 *  $RETURNS$
 *      The FUNCTION RETURNs the exact type of the designated disk drive, coded
 *      as follows:
 *
 *      0      No floppy drive
 *      1      360-kB drive
 *      2      1.2-MB drive
 *      3      720-kB drive
 *      4      1.44-MB drive
 *  $DESCRIPTION$
 *      This FUNCTION gives you a numeric value that specifies exact type and
 *      capacity of the selected floppy drive.  Whether or not the device is a
 *      floppy drive, is determined beforehand by DRIVETYPE().
 *
 *		Note
 *
 *      The drive type is determined with accessing the drive.
 *      The disk does have to be in the drive.
 *  $EXAMPLES$
 *   	Is the current disk drive a floppy and what type?
 *
 *      IF DRIVETYPE() = 1 .OR. DRIVETYPE() = 2
 *         DO CASE
 *            CASE FLOPPYTYPE() = 0
 *               ? "No floppy drive!"
 *            CASE FLOPPYTYPE() = 1
 *                ? "360 kB drive"
 *            CASE FLOPPYTYPE() = 2
 *                ? "1.2 MB drive"
 *            CASE FLOPPYTYPE() = 3
 *                ? "720 kB drive"
 *            CASE FLOPPYTYPE() = 4
 *                ? "1.44 MB drive"
 *         ENDCASE
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *      DISKTYPE(),DRIVETYPE()
 *  $END$
 */

******************************
FUNCTION FloppyType ( cDrive )
******************************
   LOCAL nTotalBytes,nFloppyType

   Default( @cDrive , DiskName() )

   IF empty(cDrive) .OR. !isalpha(cDrive)
      cDrive := DiskName()
   ENDIF

   IF DriveType( cDrive ) != 1 .AND. DriveType( cDrive ) != 2
      RETURN ( 0 )
   ENDIF

   nTotalBytes := nFloppyType := 0

   nTotalBytes := DiskTotal ( cDrive )

   IF nTotalBytes     ==  368640  // 360 Kb
      nFloppyType := 1
   ELSEIF nTotalBytes ==  737280  // 720 Kb
      nFloppyType := 3
   ELSEIF nTotalBytes == 1228800  // 1.2 Mb
      nFloppyType := 2
   ELSEIF nTotalBytes == 1457664  // 1.44 Mb
      nFloppyType := 4
   ENDIF

RETURN ( nFloppyType )

/*  $DOC$
 *  $FUNCNAME$
 *		RENAMEFILE()
 *  $CATEGORY$
 *		LIBCT File management
 *  $ONELINER$
 *		Fault tolerant renaming of a file.
 *  $SYNTAX$
 *		RENAMEFILE(<cOldFileName>, <cNewFileName>) --> nErrorCode
 *  $ARGUMENTS$
 *      <cOldFileName>   Designates the name and path of the existing file.
 *
 *      <cNewFileName>   Designates the new name and path for the file.
 *  $RETURNS$
 *      The FUNCTION RETURNs a 0 when the file can be renamed; otherwise, it
 *      RETURNs an error code.  The codes are defined below:
 *
 *      Code    Definition
 *       0      No error found
 *      -2      File not found
 *      -3      Path not found
 *      -5      Access denied (e.g., in network)
 *      -17     Target file not on same network
 *  $DESCRIPTION$
 *      Currently, you may not be able to rename a file on a network drive.
 *      Another user may currently have the file open.  RENAMEFILE() actually
 *      says "attempt a RENAME and, should the situation arise, RETURN an error
 *      code".  This follows the basic programming philosophy:  never fall into
 *      an error trap when you can avoid it.
 *      
 *      Notes
 *
 *      The <cNewFileName> must always contain the complete path for
 *      the designated file (see Examples).
 *      
 *      Wildcard characters cannot be used.
 *  $EXAMPLES$
 *		Rename a file from OLD to NEW:
 *
 *      IF RENAMEFILE("OLD", "NEW") = 0
 *         ? "The file can be renamed!"
 *      ENDIF
 *
 *      Use the path from the old file specification for the new name:
 *
 *      cFSpecOld   := "C:\TEST\TEST.TXT"
 *      cFileName   := TOKEN(cFSpecOld, ":\")      // last token
 *      cFSpecNew   := BEFOREATNUM(cFileName, cFSpecOld) + "TEST.NEW"
 *      RENAMEFILE(cFSpecOld, cFSpecNew)
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This FUNCTION is CA-Clipper Tools compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is diskutil.prg, library is libct.
 *  $SEEALSO$
 *		DELETEFILE()
 *  $END$
 */

***************************************************
FUNCTION RenameFile ( cOldFileName , cNewFileName )
***************************************************
// FileMove source is in "disk.c"
RETURN ( FileMove( cOldFileName , cNewFileName )  )

