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


#define ADS_NTX                           1
#define ADS_CDX                           2
#define ADS_ADT                           3
#define ADS_VFP                           4
//-------------------------------------------------------------------------------------------------------

CLASS AdsDataTable INHERIT DataTable

   PROPERTY TableType    ROOT "General" DEFAULT ADS_ADT
   DATA EnumTableType    EXPORTED INIT { { "NTX", "CDX", "ADT", "VFP" }, { ADS_NTX, ADS_CDX, ADS_ADT, ADS_VFP } }

   DATA xDriver          PROTECTED INIT "ADSCDX"
   DATA __ExplorerFilter EXPORTED  INIT { { "DataTable / Advantage (*.dbf,*.adt)", "*.dbf;*.adt" } }
   DATA __xCtrlName      EXPORTED  INIT "AdsDataTable"
   DATA ClsName          EXPORTED  INIT "AdsDataTable"

   METHOD File2Blob( cFile, cField )          INLINE (::Area)->( AdsFile2Blob( cFile, cField ) )
   METHOD Blob2File( cFile, cField )          INLINE (::Area)->( AdsBlob2File( cFile, cField ) )
   METHOD AdsSetServerType(n)                 INLINE AdsSetServerType(n)
   METHOD BlobImport( nFieldPos, cFile )      INLINE (::Area)->( BlobImport( nFieldPos, cFile ) )
   METHOD EnableEncryption( cKey )            INLINE (::Area)->( AdsEnableEncryption( cKey ) )
   METHOD EncryptTable()                      INLINE (::Area)->( AdsEncryptTable() )
   METHOD DecryptTable()                      INLINE (::Area)->( AdsDecryptTable() )
   METHOD IsTableEncrypted()                  INLINE (::Area)->( AdsIsTableEncrypted() )

   METHOD BlobGet( nFieldNo, nStart, nCount ) INLINE (::Area)->( dbFieldInfo( DBS_BLOB_GET, nFieldNo, { nStart, nCount } ) )
   METHOD MemoExt()                           INLINE ".adm"
   METHOD Append()                            INLINE ::Cancel(), (::Area)->( dbAppend(), AdsNull2Blank() )
   METHOD Save()
   METHOD SetData()
   METHOD FieldPut()
   METHOD NewInstance()
   METHOD SetIndexDirection( lInv )           INLINE AdsSetIndexDirection( lInv )
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD NewInstance( lSetCurPos ) CLASS AdsDataTable
   LOCAL n, oNewTable := AdsDataTable( ::Owner, ::Connection )

   n := 1
   WHILE Select( ::Alias+xStr(n) ) > 0
      n++
   ENDDO
   oNewTable:Area      := NIL
   oNewTable:xAlias    := ::Alias+xStr(n)
   oNewTable:xFileName := ::FileName
   oNewTable:Path      := ::Path
   oNewTable:Driver    := ::Driver
   oNewTable:Shared    := ::Shared
   oNewTable:bSave     := ::bSave

   oNewTable:Open()

   DEFAULT lSetCurPos TO .F.

   IF ! oNewTable:IsOpen
      oNewTable := NIL
   ELSEIF lSetCurPos
      oNewTable:OrdSetFocus( ::OrdSetFocus() )
      oNewTable:Goto( ::Recno() )
   ENDIF
RETURN oNewTable

//-------------------------------------------------------------------------------------------------------
METHOD Save() CLASS AdsDataTable
   LOCAL n, xValue
   IF ::bSave != NIL .AND. ! Eval( ::bSave, Self )
      ::__lNew := .F.
      ::__aData := {}
      RETURN Self
   ENDIF
   IF ::__lNew
      (::Area)->( dbAppend(), AdsNull2Blank() )
   ELSEIF ::Shared
      WHILE ! ::RecLock()
         sleep(1000)
      ENDDO
   ENDIF
   FOR n := 1 TO LEN( ::Structure )
       IF HGetPos( ::FieldCtrl, ::Structure[n][1] ) > 0
          IF ::FieldCtrl[ ::Structure[n][1] ]:bGetValue != NIL
             xValue := Eval( ::FieldCtrl[ ::Structure[n][1] ]:bGetValue )
             IF ::Structure[n][2] == "BINARY" .AND. File( xValue )
                ::File2Blob( xValue, ::Structure[n][1] )
             ELSE
                (::Area)->( FieldPut( n, xValue ) )
             ENDIF
          ENDIF

       ELSE
          IF LEN( ::__aData ) >= n .AND. ::__aData[n] != NIL
             ::SetData(n)
          ENDIF
       ENDIF
   NEXT
   IF ::__lNew .OR. ::Shared
      ::Unlock()
      ::Commit()
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
   IF Len( ::__aData ) >= nField
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
FUNCTION AdsNull2Blank( lAnyRDD )
   Local xData, nI, nFCount
   DEFAULT lAnyRDD TO .f.
   IF lAnyRDD .or. RddName() == "ADSADT"
      nFCount := FCount()
      FOR nI := 1 To nFCount
         IF Empty( xData := FieldGet( nI ) ) .and. ! ValType( xData ) == "M"   // Memo, Binary, Image, etc... don't have to worry about NULLs
            FieldPut( nI, xData )
         ENDIF
      NEXT
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
   #include <windows.h>
   #include "hbapi.h"
   #include "hbvm.h"
   #include "hbstack.h"
   #include "hbapiitm.h"
   #include "tchar.h"

   //-----------------------------------------------------------------------------
   HB_FUNC( ADSDDDELETEINDEX )
   {
   #if ADS_LIB_VERSION >= 600
      hb_retl( AdsDDDeleteIndex( HB_ADS_PARCONNECTION( 3 ) /* hConnect */,
                               ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                               ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */ ) == AE_SUCCESS );
   #else
      hb_retl( HB_FALSE );
   #endif
   }

   HB_FUNC( ADSDDREMOVEINDEXFILE )
   {
   #if ADS_LIB_VERSION >= 600
      hb_retl( AdsDDRemoveIndexFile( HB_ADS_PARCONNECTION( 4 ) /* hConnect */,
                                     ( UNSIGNED8 * ) hb_parcx( 1 ) /* pTableName */,
                                     ( UNSIGNED8 * ) hb_parcx( 2 ) /* pIndexName */,
                                     ( UNSIGNED16 ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parldef( 3, 0 ) ) /* usDeleteFiles */ ) == AE_SUCCESS );
   #else
      hb_retl( HB_FALSE );
   #endif
   }

   HB_FUNC( ADSDDFINDOBJECT )
   {
   #if ADS_LIB_VERSION >= 600
      UNSIGNED32 ulRetVal;
      UNSIGNED16 ulObjectType = ( UNSIGNED16 ) hb_parni( 1 );
      UNSIGNED8  ucParentName = ( UNSIGNED8 ) hb_parcx( 2 );
      UNSIGNED8  ucObjectName[ ADS_MAX_OBJECT_NAME ];
      UNSIGNED16 usObjectNameLen = sizeof( ucObjectName );
   #if ADS_LIB_VERSION >= 900
      ADSHANDLE  sHandle = 0;
   #else
      SIGNED32   sHandle = 0;
   #endif
      PHB_ITEM   pitmDir;
      ADSHANDLE  hConnect = HB_ADS_PARCONNECTION( 3 );

      pitmDir = hb_itemArrayNew( 0 );

      ulRetVal = AdsDDFindFirstObject( hConnect,
                                       ulObjectType,
                                       ( HB_ISCHAR( 2 ) ? ( UNSIGNED8 * ) hb_parc( 2 ): NULL ),
                                       ( UNSIGNED8 * ) ucObjectName,
                                       &usObjectNameLen,
                                       &sHandle );

      if( ulRetVal == AE_SUCCESS )
      {
         while( ulRetVal == AE_SUCCESS )
         {
            PHB_ITEM pitmFileName = hb_itemPutCL( NULL, ( char * ) ucObjectName, usObjectNameLen - 1 );
            hb_arrayAddForward( pitmDir, pitmFileName );

            usObjectNameLen = sizeof( ucObjectName );

            ulRetVal = AdsDDFindNextObject( hConnect,
                                            sHandle,
                                            ucObjectName,
                                            &usObjectNameLen );
         }

         AdsDDFindClose( hConnect, sHandle );
      }

      hb_itemReturnRelease( pitmDir );
   #else
      hb_reta( 0 );
   #endif
   }

   //AdsDDGetIndexProperty
   HB_FUNC( ADSDDGETINDEXPROPERTY )
   {
   #if ADS_LIB_VERSION >= 600
      UNSIGNED8  pTableName = ( UNSIGNED8 )  hb_parcx( 1 );
      UNSIGNED8  pIndexName = ( UNSIGNED8 )  hb_parcx( 2 );
      UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 3 );

      ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );


      char sBuffer[ ADS_MAX_PARAMDEF_LEN ];
      UNSIGNED16 ulLength = sizeof( sBuffer );

      if( AdsDDGetIndexProperty( hConnect, pTableName, pIndexName, ulProperty, &sBuffer, &ulLength ) != AE_SUCCESS )
      {
         sBuffer[ 0 ] = '\0';
         ulLength = 0;
      }
      hb_retclen( sBuffer, ulLength );
   #endif
   }

   HB_FUNC( ADSDDGETINDEXFILEPROPERTY )
   {
   #if ADS_LIB_VERSION >= 600
      UNSIGNED8  pTableName = ( UNSIGNED8 )  hb_parcx( 1 );
      UNSIGNED8  pIndexName = ( UNSIGNED8 )  hb_parcx( 2 );
      UNSIGNED16 ulProperty = ( UNSIGNED16 ) hb_parni( 3 );

      ADSHANDLE hConnect = HB_ADS_PARCONNECTION( 4 );

      char sBuffer[ 260 ];
      UNSIGNED16 ulLength;

      if( AdsDDGetIndexFileProperty( hConnect, pTableName, pIndexName, ulProperty, &sBuffer, &ulLength ) != AE_SUCCESS )
      {
         sBuffer[ 0 ] = '\0';
         ulLength = 0;
      }
      hb_retclen( sBuffer, ulLength );
   #endif
   }

   #pragma comment( lib, "ads.lib" )
   #pragma comment( lib, "ace32.lib" )
#pragma ENDDUMP

#endif