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
   METHOD WriteColor( cSection, cEntry, xValue )    INLINE ::Write( cSection, cEntry, xStr(GetRvalue( xValue ))+","+xStr(GetGvalue( xValue ))+","+xStr(GetBvalue( xValue )) )

   METHOD ReadColor( cSection, cEntry, xDefault )
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
   aSec := hb_aTokens( cRet, chr(0) )
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
      ADEL( aRet, -1, .T. )
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
      aEnt := hb_aTokens( cRet, chr(0) )
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
   LOCAL lRet := .F., cSave := "", c
   IF VALTYPE( acEntry ) != "A"
      IF Empty( ::Name )
         lRet := WriteProfileString( cSection, acEntry, XSTR( xValue ) )
      ELSEIF ! WritePrivateProfileString( cSection, acEntry, XSTR( xValue ), ::Name )
         ::aIni := HB_ReadIni( ::Name, .T., "=" )
         IF ::aIni != NIL
            ::aIni[cSection][ acEntry ] := XSTR( xValue )
            lRet := HB_WriteIni( ::Name, ::aIni )
         ENDIF
      ENDIF
   ELSE
      FOR EACH c IN acEntry
         cSave += c + CHR(0)
      NEXT
      cSave += CHR(0)
      IF Empty( ::Name )
         IF ( lRet := WriteProfileSection( cSection, NIL ) )
            lRet := WriteProfileSection( cSection, cSave )
         ENDIF
      ELSE
         IF ( lRet := WritePrivateProfileSection( cSection, NIL, ::Name ) )
            lRet := WritePrivateProfileSection( cSection, cSave, ::Name )
         ENDIF
      ENDIF
   ENDIF
RETURN lRet


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
METHOD ReadColor( cSection, cEntry, xDefault )
   LOCAL aColor, cColor
   DEFAULT xDefault TO ""
   IF Empty( ::Name )
      RETURN GetProfileString( cSection, cEntry, xDefault )
   ENDIF
   cColor := GetPrivateProfileString( cSection, cEntry, xDefault, ::Name )
   IF EMPTY(cColor)
      RETURN xDefault
   ENDIF
   aColor := hb_aTokens( cColor, "," )
   IF LEN(aColor) < 3
      RETURN VAL(cColor)
   ENDIF
RETURN RGB( VAL(aColor[1]), VAL(aColor[2]), VAL(aColor[3]) )

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
   LOCAL cValue
   IF Empty( ::Name )
      cValue := GetProfileString(cSection, cEntry, IIF( xDefault != NIL, STR( xDefault ), xDefault ) )
    ELSE
      cValue := GetPrivateProfileString(cSection, cEntry, IIF( xDefault != NIL, STR( xDefault ), xDefault ), ::Name )
   ENDIF
RETURN IIF( ! Empty(cValue), VAL( cValue ), xDefault )

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
      ADEL( aEntry, -1, .T. )
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
