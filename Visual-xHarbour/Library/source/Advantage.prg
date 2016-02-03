/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Advantage.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ADS

#include "vxh.ch"
#include "ord.ch"
#include "debug.ch"

//-------------------------------------------------------------------------------------------------------

CLASS AdsDataTable INHERIT DataTable

   DATA xDriver          PROTECTED INIT "ADSCDX"
   DATA __ExplorerFilter EXPORTED  INIT { { "DataTable / Advantage (*.dbf,*.adt)", "*.dbf;*.adt" } }
   DATA __xCtrlName      EXPORTED  INIT "AdsDataTable"
   DATA ClsName          EXPORTED  INIT "AdsDataTable"

   METHOD File2Blob( cFile, cField )          INLINE (::Area)->( AdsFile2Blob( cFile, cField ) )
   METHOD Blob2File( cFile, cField )          INLINE (::Area)->( AdsBlob2File( cFile, cField ) )
   METHOD AdsSetServerType(n)                 INLINE AdsSetServerType(n)
   METHOD BlobImport( nFieldPos, cFile )      INLINE (::Area)->( BlobImport( nFieldPos, cFile ) )

   METHOD BlobGet( nFieldNo, nStart, nCount ) INLINE (::Area)->( dbFieldInfo( DBS_BLOB_GET, nFieldNo, { nStart, nCount } ) )
   METHOD MemoExt()                           INLINE ".adm"
   METHOD Save()
   METHOD SetData()
   METHOD FieldPut()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Save() CLASS AdsDataTable
   LOCAL n, xValue
   IF ::__lNew
      ::Append()
   ELSEIF ::Shared
      WHILE ! ::RecLock()
         sleep(1000)
      ENDDO
   ENDIF
   FOR n := 1 TO LEN( ::Structure )
       IF HGetPos( ::FieldCtrl, ::Structure[n][1] ) > 0
          xValue := ::FieldCtrl[ ::Structure[n][1] ]:GetValue()
          IF ::Structure[n][2] == "BINARY" .AND. File( xValue )
             ::File2Blob( xValue, ::Structure[n][1] )
          ELSE
             (::Area)->( FieldPut( n, xValue ) )
          ENDIF

       ELSEIF ::__aData[n] != NIL
          ::SetData(n)
       ENDIF
   NEXT
   IF ::__lNew .OR. ::Shared
      ::Unlock()
   ENDIF
   ::__lNew := .F.
   ::__aData := {}
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD SetData(n) CLASS AdsDataTable
   IF ::Structure[n][2] == "BINARY" .AND. ! Empty( ::__aData[n] )
      ::File2Blob( ::__aData[n], ::Structure[n][1] )
    ELSE
      (::Alias)->( FieldPut( n, ::__aData[n] ) )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD FieldPut( nField, xVal ) CLASS AdsDataTable
   IF ! Empty( ::__aData )
      ::__aData[nField] := xVal
    ELSE
      IF ::Structure[nField][2] == "BINARY" .AND. ! Empty( xVal )
         ::File2Blob( xVal, ::Structure[nField][1] )
       ELSE
         ::Connector:FieldPut( nField, xVal )
      ENDIF
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS AdsServer INHERIT Component
   PROPERTY Type ROOT "Behavior"  SET AdsSetServerType( v ) DEFAULT 1
   DATA EnumType   EXPORTED  INIT { { "Local", "Remote", "Either" }, {1,2,3} }
   METHOD Init() CONSTRUCTOR
   METHOD Create()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS AdsServer
   DEFAULT ::__xCtrlName TO "AdsServer"
   DEFAULT ::ClsName     TO "AdsServer"
   ::ComponentType := "DataServer"
   ::Super:Init( oOwner )
RETURN Self

METHOD Create() CLASS AdsServer
   IF ! ::Owner:DesignMode
      AdsSetServerType( ::Type )
   ENDIF
RETURN Self

#pragma BEGINDUMP
   #pragma comment( lib, "ads.lib" )
   #pragma comment( lib, "ace32.lib" )
#pragma ENDDUMP

#endif