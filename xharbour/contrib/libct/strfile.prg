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

   local nBytes := 0
   local nHandle
   local nTmp
   local nEnd
   local nLen
   local nPos
   local cTempFile := "TMP" + alltrim( seconds() )

   lOverWrite := IF( lOverWrite == NIL, .F., lCutOff )
   lCutOff := IF( lCutOff == NIL, .F., lCutOff )

   if lCutOff
      copy file ( cFile ) to ( cTempFile )
   endif

   if lOverWrite .and. file( ( cFile ) ) .and. !lCutOff
      nHandle := Fopen( ( cFile ), FO_READWRITE )
   else
      nHandle := Fcreate( ( cFile ) )
      if lCutOff
         nTmp := Fopen( ( cTempFile ), FO_READWRITE )
         Transfer( nTmp, nHandle, nOffSet )
         Fclose( nTmp )
         Ferase( ( cTempFile ) )
      endif
   endif
   if nHandle > 0
      nEnd := Fseek( nHandle, 0, FS_END )
      nOffSet := IF( nOffSet == NIL, nEnd, nOffSet )
      nPos := Fseek( nHandle, nOffSet, FS_SET )
      nLen := len( cString )
      nBytes := Fwrite( nHandle, cString, nLen )
      Fclose( nHandle )
   endif
   return( nBytes )

Static function Transfer( nSourceHandle, nTargetHandle, nLen )

   local cSourceBuff
   local nBlock := 1024
   local nBuffSize
   local nPos := Fseek( nSourceHandle, 0, FS_RELATIVE )
   local nSize := Fseek( nSourceHandle, 0, FS_END )

   Fseek( nSourceHandle, 0, FS_SET )

   While nPos < nLen .and. nPos < nSize
      nBuffSize := IF( nPos + nBlock > nLen, nLen - nPos, nBlock )
      cSourceBuff := space( nBuffSize )
      Fread( nSourceHandle, @cSourceBuff, nBuffSize )
      Fwrite( nTargetHandle, cSourceBuff, nBuffSize )
      nPos := Fseek( nSourceHandle, 0, FS_RELATIVE )
   end
   return( nil )
