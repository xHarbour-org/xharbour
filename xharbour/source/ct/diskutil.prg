/*
 * $Id: diskutil.prg,v 1.1 2004/02/01 11:36:59 lculik Exp $
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





********************************
FUNCTION FileValid ( cFileName )
********************************
   LOCAL nHandle

   nHandle := FOpen( cFileName , FO_READWRITE )

   IF nHandle != 0
      FClose( nHandle )
   ENDIF

RETURN ( IIF( nHandle > 0 , .T. , .F. ) )



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


***************************************************
FUNCTION RenameFile ( cOldFileName , cNewFileName )
***************************************************
// FileMove source is in "disk.c"
RETURN ( FileMove( cOldFileName , cNewFileName )  )

