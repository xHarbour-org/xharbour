/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// IniFile.prg                                                                                          *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define ERROR_ACCESS_DENIED  5L

//--------------------------------------------------------------------------------------------------

CLASS IniFile
   DATA Name   EXPORTED
   DATA aIni   PROTECTED
   METHOD Init() CONSTRUCTOR

   METHOD Flush()
   METHOD GetSections()
   METHOD GetEntries()
   METHOD GetSectionEntries()
   METHOD DelSection()
   METHOD DelEntry()

   METHOD Read()
   METHOD Write()

   METHOD WriteString( cSection, cEntry, xValue )   INLINE ::Write( cSection, cEntry, xValue )
   METHOD WriteDate( cSection, cEntry, xValue )     INLINE ::Write( cSection, cEntry, xValue )
   METHOD WriteNumber( cSection, cEntry, xValue )   INLINE ::Write( cSection, cEntry, xValue )
   METHOD WriteInteger( cSection, cEntry, xValue )  INLINE ::Write( cSection, cEntry, Int( xValue ) )
   METHOD WriteLogical( cSection, cEntry, xValue )  INLINE ::Write( cSection, cEntry, IIF( xValue, "Yes","No" ) )

   METHOD ReadString( cSection, cEntry, xDefault )
   METHOD ReadDate( cSection, cEntry, xDefault )
   METHOD ReadNumber( cSection, cEntry, xDefault )
   METHOD ReadInteger( cSection, cEntry, xDefault )
   METHOD ReadLogical( cSection, cEntry, xDefault )
   METHOD ReadArray( cSection )
ENDCLASS

//--------------------------------------------------------------------------------------------------
METHOD Init( cFileName ) CLASS IniFile
   IF Valtype(cFileName)=="C" .AND. ! Empty(cFileName)
      ::Name := cFileName
   ENDIF
RETURN(SELF)

//--------------------------------------------------------------------------------------------------
METHOD GetSections() CLASS IniFile
   LOCAL Section, cRet, aSec, aRet:={}
   IF Empty( ::Name )
      cRet := GetProfileString( NIL, NIL, NIL )
   ELSE
      cRet := GetPrivateProfileString( NIL, NIL, NIL, ::Name )
   ENDIF
   aSec := __str2a( cRet, chr(0) )
   FOR EACH Section IN aSec
       IF !EMPTY( Section )
          AADD( aRet, Section )
       ENDIF
   NEXT
RETURN aRet

METHOD GetSectionEntries( cSection, lFixOld ) CLASS IniFile
   LOCAL cRet, n, aRet := {}
   IF ( cRet := GetPrivateProfileSection( cSection, ::Name ) ) != NIL
      aRet := hb_aTokens( cRet, chr(0) )
      IF lFixOld != NIL .AND. lFixOld
         FOR n := 1 TO LEN( aRet )
             IF aRet[n][-1] == "="
                aRet[n] := SUBSTR( aRet[n], 1, LEN( aRet[n] )-1 )
             ENDIF
         NEXT
      ENDIF
   ENDIF
RETURN aRet


//--------------------------------------------------------------------------------------------------
METHOD GetEntries( cSection ) CLASS IniFile
   LOCAL cRet, aEnt, Entry, aRet := {}

   IF Empty( ::Name )
      cRet := GetProfileString( cSection, NIL, NIL )
   ENDIF
   cRet := GetPrivateProfileString( cSection, NIL, NIL, ::Name )
   IF cRet != NIL
      aEnt := __str2a( cRet, chr(0) )
      FOR EACH Entry IN aEnt
          IF !EMPTY( Entry )
             AADD( aRet, Entry )
          ENDIF
      NEXT
   ENDIF
RETURN aRet

//--------------------------------------------------------------------------------------------------
METHOD DelSection( cSection ) CLASS IniFile
   IF Empty(::Name)
      RETURN WriteProfileString(cSection,NIL,NIL)
   ENDIF
RETURN WritePrivateProfileString(cSection,NIL,NIL,::Name)

//--------------------------------------------------------------------------------------------------
METHOD DelEntry( cSection, cEntry ) CLASS IniFile
   IF Empty(::Name)
      RETURN WriteProfileString(cSection,cEntry,NIL)
   ENDIF
RETURN WritePrivateProfileString(cSection,cEntry,NIL,::Name)

//--------------------------------------------------------------------------------------------------
METHOD Flush() CLASS IniFile
   IF Empty(::Name)
      RETURN WriteProfileString(NIL,NIL,NIL)
   ENDIF
RETURN WritePrivateProfileString(NIL,NIL,NIL,::Name)

//--------------------------------------------------------------------------------------------------
METHOD Write( cSection, acEntry, xValue ) CLASS IniFile
   LOCAL cSave := "", c
   IF VALTYPE( acEntry ) != "A"
      IF Empty( ::Name )
         RETURN WriteProfileString( cSection, acEntry, XSTR( xValue ) )
      ENDIF
      IF WritePrivateProfileString( cSection, acEntry, XSTR( xValue ), ::Name )
         RETURN .T.
       ELSE
         DEFAULT ::aIni TO HB_ReadIni( ::Name, .T., "=" )
         ::aIni[cSection][ acEntry ] := XSTR( xValue )
         HB_WriteIni( ::Name, ::aIni )
      ENDIF
    ELSE
      FOR EACH c IN acEntry
         cSave += c + CHR(0)
      NEXT
      cSave += CHR(0)
      IF Empty( ::Name )
         WriteProfileSection( cSection, NIL )
         WriteProfileSection( cSection, cSave )
       ELSE
         WritePrivateProfileSection( cSection, NIL, ::Name )
         WritePrivateProfileSection( cSection, cSave, ::Name )
      ENDIF
   ENDIF
RETURN .T.

//--------------------------------------------------------------------------------------------------
METHOD Read( cSection, cEntry, xDefault ) CLASS IniFile
   SWITCH ValType( xDefault )
      CASE "C"
           RETURN ::ReadString( cSection, cEntry, xDefault )
      CASE "D"
           RETURN ::ReadDate( cSection, cEntry, xDefault )
      CASE "N"
           RETURN ::ReadNumber( cSection, cEntry, xDefault )
   END

RETURN(NIL)

//--------------------------------------------------------------------------------------------------
METHOD ReadString( cSection, cEntry, xDefault ) CLASS IniFile
   DEFAULT xDefault TO ""
   IF Empty( ::Name )
      RETURN GetProfileString( cSection, cEntry, xDefault )
   ENDIF
RETURN GetPrivateProfileString( cSection, cEntry, xDefault, ::Name )

//--------------------------------------------------------------------------------------------------
METHOD ReadDate( cSection, cEntry, xDefault ) CLASS IniFile
   DEFAULT xDefault TO CTOD("")
   IF Empty( ::Name )
      RETURN GetProfileString( cSection, cEntry, xDefault )
   ENDIF
RETURN GetPrivateProfileString( cSection, cEntry, xDefault, ::Name )

//--------------------------------------------------------------------------------------------------
METHOD ReadNumber( cSection, cEntry, xDefault ) CLASS IniFile
   DEFAULT xDefault TO 0
   IF Empty( ::Name )
      RETURN VAL( GetProfileString(cSection, cEntry, STR( xDefault ) ) )
   ENDIF
RETURN VAL( GetPrivateProfileString(cSection, cEntry, STR( xDefault ), ::Name ) )

//--------------------------------------------------------------------------------------------------
METHOD ReadArray( cSection ) CLASS IniFile
   LOCAL cEntry, aEntry := {}
   IF Empty( ::Name )
      cEntry := GetProfileSection( cSection )
    ELSE
      cEntry := GetPrivateProfileSection( cSection, ::Name )
   ENDIF

   IF ! Empty( cEntry )
      aEntry := hb_aTokens( cEntry, chr(0) )
   ENDIF
RETURN aEntry

//--------------------------------------------------------------------------------------------------
METHOD ReadInteger( cSection, cEntry, xDefault ) CLASS IniFile
   DEFAULT xDefault TO 0
   IF Empty( ::Name )
      RETURN GetProfileInt( cSection, cEntry, xDefault )
   ENDIF
RETURN GetPrivateProfileInt( cSection, cEntry, xDefault, ::Name )
//--------------------------------------------------------------------------------------------------
METHOD ReadLogical( cSection, cEntry, xDefault ) CLASS IniFile
   DEFAULT xDefault TO .F.
   IF Empty( ::Name )
      RETURN UPPER( GetProfileString( cSection, cEntry, xDefault ) ) $ "YESON1"
   ENDIF
RETURN UPPER( GetPrivateProfileString( cSection, cEntry, xDefault, ::Name ) ) $ "YESON1"
