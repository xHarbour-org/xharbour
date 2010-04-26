/*
 * $Id: diskutil.prg,v 1.10 2009/04/18 22:44:01 modalsist Exp $
 */
/*
 * xHarbour Project source code.
 * CT Disk, File and Directory management.
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 * www.xharbour.org
 *
 * DiskFormat()  - Format disk. See notes.
 * DiskFree()    - Return disk free available space.
 * DiskReady()   - Return true if disk is ready.
 * DiskReadyW()  - Partially ready. See function for more details.
 * DiskTotal()   - Return total disk size.
 * DiskUsed()    - Return ammount used into disk. This function is xHarbour extension.
 * FileValid()   - Return true if a string file name is valid.
 * FloppyType()  - Return a floppy type.
 * RenameFile()  - Rename a file.
 *
 * Copyright 2010 Pavel Tsarenko <tpe2@mail.ru>
 * www.xharbour.org
 * TEMPFILE()    - Creates a file for temporary use
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
#include "set.ch"


*----------------------------------------------------------------------------------------------------
FUNCTION DiskFormat( cDrive, nCapacity, cUDF, cBootText, nRepetitions, cLabel , lBoot ,lQuickFormat )
*----------------------------------------------------------------------------------------------------
   LOCAL cCommand,uScreen,nErr

   default( @cDrive, "A")          // floppy A
   default( @nCapacity , 0 )       // max floppy capacity

   // CT compatibility
   default( @cUDF, "" )            // by default, no UDF is called.
   default( @cBootText, "" )       // no boot text message.
   default( @nRepetitions, 1)      // one repetition if format fail.

   // xHarbour extensions.
   default( @cLabel, " " )         // without label.
   default( @lBoot, .F. )          // only for Win95/98/ME
   default( @lQuickFormat , .F. )  // force full format.

/*
NOTE: 

December/2004 - EF

1) The original CT diskformat() access low level assembly functions and bios
   calls to format a disk. I don't be able to do the same so, this function
   is a workaround that call the OS "format" command.
   
2) The <cUDF>, <cBootText> and <nRepetitions> parameters are include only for
   maintain original diskformat CT call covention. These functionalities are
   not yet implemented.
      
*/


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
         IF Os_IsWin9X()
            cCommand += " /U"  // Unconditional format. Only for Window95/98/ME.
         ENDIF
      ENDIF

      IF nCapacity > 0
         cCommand += " /F:"+alltrim(str(nCapacity))
      ENDIF

      IF lBoot .AND. OS_IsWin9X()
         cCommand += " /S" // Only for Windows95/98/ME.
      ENDIF

      IF OS_IsWinNT() // only for NT4/2000/XP/2003
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



*-----------------------------------
FUNCTION DiskReady( cDrive , lMode )
*-----------------------------------
LOCAL lReturn, cDsk, cCurDsk 

   default( @lMode , .F. )
// lMode -> True = Windows/DOS mode. If a disk is not ready, open a dialog.
//          False = Bios mode. If a disk is not ready don´t open a dialog.

   cCurDsk := DiskName()

   cDsk := _Drive( cDrive )

   IF valtype(lMode) != "L"
      lMode := .F.
   ENDIF

   IF lMode
      // Windows/DOS access mode. xHarbour RTL. Source is in "dirdrive.c".
      lReturn := DiskChange( cDsk )
      IF lReturn
         DiskChange( cCurDsk )
      ENDIF
   ELSE
      // Bios access mode. xHarbour RTL. Source is in "dirdrive.c".
      lReturn := IsDisk( cDsk )
   ENDIF

RETURN ( lReturn )



*------------------------------------
FUNCTION DiskReadyW( cDrive , lMode )
*------------------------------------
LOCAL cDsk, nHd, cFile, lReturn := .F., cCurrent := DiskName()

   default( @lMode  , .T. )
// lMode -> Windows/DOS write ready mode. Same as DiskReady().

   cDsk := _Drive(cDrive)

   IF valtype(lMode) != "L"
      lMode := .T.
   ENDIF

   IF lMode
      IF DiskChange( cDsk )
         cFile := "wwxxyyzz.xyz"
         nHd := FCreate( cFile , 0 )
         IF nHd > 0
            FClose( nHd )
            FErase( cFile )
            lReturn := .T.
         ENDIF
         DiskChange( cCurrent )
      ENDIF
   ENDIF

RETURN lReturn

*--------------------------
FUNCTION DiskFree( cDrive )
*--------------------------
Local nRet := 0
Local cDsk,cCurPath, cCurDir

   cCurDir  := DirName()
   cCurPath := DiskName()+":\"+cCurDir

   cDsk := _Drive( cDrive )

   if DiskReady( cDsk )
      /* hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
       * This function changes directory if you pass a drive letter than current,
       * so it's need to save the full path before call hb_DiskSpace and restore
       * them after.
       */
      nRet := hb_DiskSpace( cDsk , HB_DISK_FREE )
   endif

   if !( DirName() == cCurDir )
      DirChange( cCurPath )
   endif

RETURN (nRet)



*---------------------------
FUNCTION DiskTotal( cDrive )
*---------------------------
LOCAL nRet:=0
LOCAL cDsk, cCurPath, cCurDir

   cCurDir  := DirName()
   cCurPath := DiskName()+":\"+cCurDir

   cDsk := _Drive(cDrive)

   IF DiskReady( cDsk )
      /* hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
       * This function changes directory if you pass a drive letter than current,
       * so it's need to save the full path before call hb_DiskSpace and restore
       * them after.
       */
      nRet := hb_DiskSpace( cDsk , HB_DISK_TOTAL )
   ENDIF

   if !( DirName() == cCurDir )
      DirChange( cCurPath )
   endif

RETURN (nRet)

*--------------------------
FUNCTION DiskUsed( cDrive )
*--------------------------
Local nRet := 0
LOCAL cDsk, cCurPath, cCurDir

   cCurDir  := DirName()
   cCurPath := DiskName()+":\"+cCurDir

   cDsk := _Drive(cDrive)

   IF DiskReady( cDsk )
      /* hb_DiskSpace is a xHarbour RT Library FUNCTION. Source is "disksphb.c".
       * This function changes directory if you pass a drive letter than current,
       * so it's need to save the full path before call hb_DiskSpace and restore
       * them after.
       */
      nRet := hb_DiskSpace( cDsk , HB_DISK_USED )
   ENDIF

   if !( DirName() == cCurDir )
      DirChange( cCurPath )
   endif

RETURN ( nRet )


*----------------------------------------------------------------------------
FUNCTION FileValid( cFileName, nMaxName, nMaxExt, lWithoutExt, lSpaceInName )
*----------------------------------------------------------------------------
/*
This function return by default, the MS-DOS valid file name (8x3) or an other
format defined by user in accordance with <nMaxName> and <nMaxExt> values. 
nMaxName, nMaxExt, lWithoutExt and lSpaceInName are xHarbour extensions.
*/

 Local lRet  := .T.
 Local cName := ""
 Local cExt  := ""
 Local i     := 0
 Local cInvalid := ""
 Local nDecimalPoint := 0
 Local nFileLen := 0
 
 default cFileName to ""
 default nMaxName  to 8      // max file name len.  
 default nMaxExt   to 3      // max extension name len.
 default lWithoutExt to .T.  // allow file name without extension.
 default lSpaceInName to .F. // allow space char in file name. 

 if !IsCharacter(cFileName) .or. Empty(cFileName)
    Return .F.
 endif

 if !IsNumber(nMaxName)
    nMaxName := 8
 endif

 if !IsNumber(nMaxExt)
    nMaxExt := 3
 endif

 if !IsLogical(lWithoutExt)
    lWithoutExt := .T.
 endif

 if !IsLogical(lSpaceInName)
    lSpaceInName := .F.
 endif

 if nMaxName <= 0
    Return .F.
 endif   

 if nMaxExt <= 0
    nMaxExt := 0
    lWithoutExt := .T.
 endif   

 
 for i := 0 to 255
    if (i>=0  .and. i<=32) .or.;
       i=34 .or.;
       (i>=42 .and. i<=44) .or.;
       (i>=46 .and. i<=47) .or.;
       (i>=58 .and. i<=63) .or.;
       (i>=91 .and. i<=93) .or.;
        i=124 .or. i=127

        cInvalid += chr(i)
       
    endif   
 next

 if lSpaceInName
    cInvalid := StrTran(cInvalid," ","")
 endif

 cFileName := Rtrim(cFileName)
 
 nDecimalPoint := At(".",cFileName)
 nFileLen      := Len( cFileName )

if nFileLen=0 .or. nFileLen > (nMaxName+nMaxExt+1)
   lRet := .F.
elseif nDecimalPoint > (nMaxName+1)
   lRet := .F.
elseif nDecimalPoint > 0 .and. nMaxExt = 0
   lRet := .F.
elseif nDecimalPoint > 0 .and. nDecimalPoint <= (nMaxName+1)
   cName := SubStr(cFileName,1, nDecimalPoint-1 )
   cExt  := SubStr(cFileName,nDecimalPoint+1 )
   if empty(cName) .or. ( !lWithoutExt .and. empty(cExt) )
      lRet := .F.
   endif   
elseif nDecimalPoint=0 .and. !lWithoutExt
   lRet := .F.
elseif nDecimalPoint=0 .and. nFileLen > nMaxName
   lRet := .F.
elseif nDecimalPoint=0 .and. nFileLen <= nMaxName
   cName := cFileName
endif   

if lRet

 if !empty(cName)
    if Len(cName) > nMaxName
       lRet := .F.
    endif   
 else
    lRet := .F.
 endif

 if lRet .and. ( empty(cExt) .and. !lWithoutExt )
    lRet := .F.
 endif
 
 if lRet .and. !empty(cExt)
    if Len(cExt) > nMaxExt
       lRet := .F.
    endif   
 endif

endif

if lRet

   for i := 1 to Len(cName)
       if SubStr(cName,i,1) $ cInvalid
          lRet := .F.
          exit
       endif
   next

   if lRet .and. !empty(cExt)
      for i := 1 to Len(cExt)
          if SubStr(cExt,i,1) $ cInvalid
             lRet := .F.
             exit
          endif
      next
   endif

endif

Return (lRet)


*----------------------------
FUNCTION FloppyType( cDrive )
*----------------------------
LOCAL nTotalBytes,nFloppyType

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


*-------------------------------------------------
FUNCTION RenameFile( cOldFileName , cNewFileName )
*-------------------------------------------------
// FileMove function source is in "xharbour/source/ct/disk.c"
RETURN ( FileMove( cOldFileName , cNewFileName )  )


Function TempFile(cDir, cExt, nAttr)
LOCAL nFileHandle
LOCAL cFileName, cNewName
LOCAL nAt, cSep

IF cDir == nil
   cDir := CurDirX()
ENDIF
nFileHandle := HB_FTempCreate(cDir,, nAttr, @cFileName )
IF nFileHandle > 0
   FClose( nFileHandle )
   nAt := Rat(".", cFileName)
   cSep := Set(_SET_DIRSEPARATOR)
   IF nAt != 0 .and. Rat(cSep, cFileName) > nAt
      nAt := 0
   ENDIF
   IF cExt != nil
      IF nAt == 0
         cNewName := cFileName + "." + cExt
      ELSEIF ! (Substr(cFileName, nAt + 1) == cExt)
         cNewName := Left(cFileName, nAt - 1) + "." + cExt
      ENDIF
   ELSEIF nAt != 0
      cNewName := Left(cFileName, nAt - 1)
   ENDIF
   IF ! Empty(cNewName) .and. FRENAME(cFileName, cNewName) != -1
      cFileName := cNewName
   ENDIF
   IF (nAt := Rat(cSep, cFileName)) != 0
      cFileName := Substr(cFileName, nAt + 1)
   ENDIF
ENDIF
Return cFileName

*--------------------------------*
STATIC FUNCTION _Drive( cDsk )
*--------------------------------*
Local cCurDisk, cDrive

   cDrive := cDsk

   cCurDisk := DiskName()

   if cCurDisk[-1] != ":"
      cCurDisk += ":"
   endif

   if empty( cDrive ) .or. ! IsAlpha( cDrive )
      cDrive := cCurDisk
   endif

   cDrive := StrTran(cDrive,"/","")
   cDrive := StrTran(cDrive,"\","")

   if cDrive[-1] != ":"
      cDrive += ":"
   endif

Return cDrive
