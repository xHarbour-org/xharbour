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
   DATA EnumServerType   EXPORTED  INIT { { "Local", "Remote", "Either" }, {1,2,3} }
   DATA ServerType       PUBLISHED INIT 1

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
   IF ::__lNew
      ::Append()
   ENDIF
   AEVAL( ::__aData, {|,n| ::SetData(n) } )
   IF ::__lNew .AND. ::Shared
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

#pragma BEGINDUMP
   #pragma comment( lib, "ads.lib" )
   #pragma comment( lib, "ace32.lib" )
#pragma ENDDUMP

#endif