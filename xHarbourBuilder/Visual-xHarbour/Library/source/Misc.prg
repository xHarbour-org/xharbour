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

#define KEY_READ        25 // ((STANDARD_RIGHTS_READ +  KEY_QUERY_VALUE + KEY_ENUMERATE_SUB_KEYS +  KEY_NOTIFY) & (~SYNCHRONIZE))
#define KEY_WRITE        6 // ((STANDARD_RIGHTS_WRITE +  KEY_SET_VALUE +  KEY_CREATE_SUB_KEY) & (~SYNCHRONIZE))
#define KEY_EXECUTE     25 // ((KEY_READ) & (~SYNCHRONIZE))

CLASS Registry
   DATA nKey, cKey EXPORTED
   DATA Value      EXPORTED
   DATA aKeys      EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD Open()
   METHOD Create()
   METHOD GetKeys()
   METHOD DeleteAllKeys()          INLINE ::GetKeys( .T. )
   METHOD GetValue( cKey )
   METHOD SetValue( cKey, xValue ) INLINE RegSetValueEx( ATAIL( ::aKeys ), cKey,, IIF( VALTYPE(xValue) == "C", REG_SZ, REG_DWORD ), xValue ) == 0
   METHOD Delete( cVal )           INLINE IIF( EMPTY(cVal), RegDeleteKey( ATAIL( ::aKeys ), "" ), RegDeleteValue( ATAIL( ::aKeys ), cVal ) )
   METHOD Close()                  INLINE RegCloseKey( ATAIL( ::aKeys ) ), ADEL( ::aKeys, LEN( ::aKeys ), .T. )

   error HANDLER OnError()
ENDCLASS

METHOD OnError( xValue ) CLASS Registry
   LOCAL cKey := __GetMessage()
   IF xValue == NIL
      RETURN ::GetValue( IIF( lower(cKey) == "default", "", cKey ) )
    ELSE
      ::SetValue( SUBSTR(cKey,2), xValue )
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Init( nKey, cKey ) CLASS Registry
   ::nKey := nKey
   ::cKey := cKey
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------
METHOD GetValue( cKey ) CLASS Registry
   LOCAL cValue
   RegQueryValueEx( ATAIL( ::aKeys ), cKey, @cValue )
   ::Value := cValue
RETURN cValue

//-----------------------------------------------------------------------------------------------------------------------------
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

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Open( nKey, cKey, nAccess ) CLASS Registry
   LOCAL lRet, hKey
   IF VALTYPE( nKey ) == "C"
      cKey := nKey
      nKey := ATAIL( ::aKeys )
    ELSE
      DEFAULT nKey TO ::nKey
      DEFAULT cKey TO ::cKey
      ::nKey := nKey
      ::cKey := cKey
   ENDIF
   DEFAULT nAccess TO KEY_ALL_ACCESS
   lRet := RegOpenKeyEx( nKey, cKey, 0, (nAccess | KEY_WOW64_64KEY), @hKey ) == 0
   IF lRet
      RegDisableReflectionKey( hKey )
      AADD( ::aKeys, hKey )
   ENDIF
RETURN lRet

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Create( ncKey, cKey ) CLASS Registry
   LOCAL lRet, hKey

   IF VALTYPE( ncKey ) == "C"
      cKey  := ncKey
      ncKey := ATAIL( ::aKeys )
    ELSE
      DEFAULT ncKey TO ::nKey
      DEFAULT cKey  TO ::cKey
      ::nKey := ncKey
      ::cKey := cKey
   ENDIF
   lRet := RegCreateKeyEx( ncKey, cKey, (KEY_ALL_ACCESS | KEY_WOW64_64KEY), @hKey  ) == 0
   IF lRet
      RegDisableReflectionKey( hKey )
      AADD( ::aKeys, hKey )
   ENDIF
RETURN lRet


FUNCTION __Proper(cStr)
   local n,ch,nLen
   local c:=""
   local l:=.T.
   //cStr:=strtran(lower(alltrim(cStr)),"_"," ")
   cStr:=lower(alltrim(cStr))
   nlen:=len(cStr)
   FOR n:=1 TO nLen
      ch:=substr(cStr,n,1)
      c+=if(l,upper(ch),ch)
      l:=(ch==" ".or.ch=="_")
   NEXT
RETURN(c)


FUNCTION GetDesktopRect()     // Gets full rectangle INCLUDING SECONDARY MONITOR. See also GETDESKTOPWA() in winapiX.c which ONLY gets the WorkArea
   local aDesktopRect := array(4), hWnd
   TRY
      aDesktopRect[1] := GetSystemMetrics(SM_YVIRTUALSCREEN)            // top
      aDesktopRect[2] := GetSystemMetrics(SM_XVIRTUALSCREEN)
      aDesktopRect[3] := aDesktopRect[1] + GetSystemMetrics(SM_CYVIRTUALSCREEN) //bottom
      aDesktopRect[4] := aDesktopRect[2] + GetSystemMetrics(SM_CXVIRTUALSCREEN) // right
   CATCH
      hWnd := GetDeskTopWindow()
      aDesktopRect := _GetWindowRect( hWnd )
   END
RETURN aDesktopRect
