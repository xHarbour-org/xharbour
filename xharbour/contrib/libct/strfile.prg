* ÚÄ Program ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³  Application: CA-TOOLS 3.0                                               ³
* ³  Description: StrFile() work around                                      ³
* ³    File Name: STRFILE.PRG                                                ³
* ³       Author: Flemming Ho                                                ³
* ³ Date created: 09-28-93              Date updated: þ09-28-93              ³
* ³ Time created: 02:39:32pm            Time updated: þ02:39:32pm            ³
* ³    Exec File: StrFile.obj           Docs By: Roy Johanson                ³
* ³    Copyright: (c) 1993 by Computer Associates                            ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

#include "fileio.ch"
function StrFile( cString, cFile, lOverwrite, nOffSet, lCutOff )

   local nHandle
   local nBytes := 0

   lOverWrite := IF( lOverWrite == NIL, .F., lOverWrite )
   lCutOff := IF( lCutOff == NIL, .F., lCutOff )

   if lOverWrite .and. file( cFile )
      /* TODO: when SETSHARE()/GETSHARE() will be implemented respect
               flags returned by GETSHARE() function */
      nHandle := Fopen( cFile, FO_WRITE )
   else
      /* TODO: when SETFCREATE() will be implemented use
               flags returned by this function */
      nHandle := Fcreate( cFile )
      lCutOff := .F.
   endif
   if nHandle > 0
      if nOffSet == NIL
         Fseek( nHandle, 0, FS_END )
         lCutOff := .F.
      else
         Fseek( nHandle, nOffSet, FS_SET )
      endif
      nBytes := Fwrite( nHandle, cString, len( cString ) )
      if lCutOff
         /* FWRITE with 0 bytes trunc the file */
         Fwrite( nHandle, "", 0 )
      endif
      Fclose( nHandle )
   endif
   return( nBytes )
