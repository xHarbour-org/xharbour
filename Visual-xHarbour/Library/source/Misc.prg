/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Misc.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define KEY_ALL_ACCESS  0xF003F
#define REG_SZ                      1
#define REG_DWORD                   4
#define KEY_WOW64_64KEY 0x0100

CLASS Registry
   DATA nKey, cKey EXPORTED
   DATA Value      EXPORTED
   DATA aKeys      EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD Open()
   METHOD Create()
   METHOD GetKeys()
   METHOD DeleteAllKeys()          INLINE ::GetKeys( .T. )
   METHOD GetValue( cKey )         INLINE RegQueryValueEx( ATAIL( ::aKeys ), cKey, @::Value ), ::Value
   METHOD SetValue( cKey, xValue ) INLINE RegSetValueEx( ATAIL( ::aKeys ), cKey,, IIF( VALTYPE(xValue) == "C", REG_SZ, REG_DWORD ), xValue ) == 0
   METHOD Delete( cKey )           INLINE RegDeleteKey( ATAIL( ::aKeys ), cKey )
   METHOD Close()                  INLINE RegCloseKey( ATAIL( ::aKeys ) ), ADEL( ::aKeys, LEN( ::aKeys ), .T. )

   error HANDLER OnError()
ENDCLASS

METHOD OnError( xValue ) CLASS Registry
   LOCAL cKey := __GetMessage()
   IF xValue == NIL
      RETURN ::GetValue( cKey )
    ELSE
      ::SetValue( SUBSTR(cKey,2), xValue )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD Init( nKey, cKey ) CLASS Registry
   ::nKey := nKey
   ::cKey := cKey
RETURN Self

METHOD GetKeys( lDel ) CLASS Registry
   LOCAL cName, cType, xData, aRet := {}
   LOCAL n := 0
   DEFAULT lDel TO .F.
   WHILE RegEnumKey( ATAIL( ::aKeys ), n, @cName, @cType, @xData ) == 0
      IF ! lDel
         AADD( aRet, {cName, cType, xData} )
         n++
       ELSE
         RegDeleteKey( ATAIL( ::aKeys ), cName )
      ENDIF
   ENDDO
RETURN aRet

METHOD Open( nKey, cKey ) CLASS Registry
   LOCAL lRet, hKey
   DEFAULT nKey TO ::nKey
   DEFAULT cKey TO ::cKey
   IF VALTYPE( nKey ) == "C"
      cKey := nKey
      nKey := ATAIL( ::aKeys )
    ELSE
      ::nKey := nKey
      ::cKey := cKey
   ENDIF
   lRet := RegOpenKeyEx( nKey, cKey, 0, KEY_ALL_ACCESS | KEY_WOW64_64KEY, @hKey ) == 0
   IF lRet
      RegDisableReflectionKey( hKey ) 
      AADD( ::aKeys, hKey )
   ENDIF
RETURN lRet

METHOD Create( nKey, cKey ) CLASS Registry
   LOCAL lRet, hKey
   DEFAULT nKey TO ::nKey
   DEFAULT cKey TO ::cKey
   IF VALTYPE( nKey ) == "C"
      cKey := nKey
      nKey := ATAIL( ::aKeys )
    ELSE
      ::nKey := nKey
      ::cKey := cKey
   ENDIF
   lRet := RegCreateKeyEx( nKey, cKey, KEY_ALL_ACCESS | KEY_WOW64_64KEY, @hKey  ) == 0
   IF lRet
      RegDisableReflectionKey( hKey )
      AADD( ::aKeys, hKey )
   ENDIF
RETURN lRet
