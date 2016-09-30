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
#include "ads.ch"

static hFTConnection, s_aTables

//-------------------------------------------------------------------------------------------------------

CLASS AdsDataTable INHERIT DataTable
   CLASSDATA bLockMessage

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
   METHOD RecLock()
   METHOD UnLock()                            INLINE (::Area)->( dbRUnlock() )
   METHOD GetLockOwner()
   METHOD Create()
   METHOD CreateOrder()
   METHOD CreateTable()
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
METHOD CreateTable( aStruc, cFile ) CLASS AdsDataTable
   LOCAL n, aTables, cTableName
   DEFAULT cFile  TO ::FileName
   DEFAULT aStruc TO ::Structure
   IF ! Empty( cFile ) .AND. ! File( cFile ) .AND. ! Empty( aStruc )
      IF ! ::Connector:CreateTable( cFile, aStruc, ::Driver )
         IF Left( ::FileName, 2 ) .AND. ::Connection != NIL
            n       := RAT( "\", cFile )
            cTableName := SUBSTR( cFile, n+1 )
            n       := RAT( ".", cTableName )
            cTableName := SUBSTR( cTableName, 1, n-1 )
            aTables := AdsDDFindObject( ADS_DD_TABLE_OBJECT, , ::Connection )

            IF ASCAN( aTables, {|cTable| Upper(cTable)==Upper(cTableName)} ) > 0
               // Disaster recovery: Table file deleted
               IF ! AdsDDRemoveTable( cTableName, 0, ::Connection )
                  view AdsGetLastError()
               ELSE
                  ::Connector:CreateTable( cFile, aStruc, ::Driver )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

   ELSEIF File( cFile ) .AND. ! Empty( aStruc )
      ::__aTmpStruct := aClone( aStruc )

   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create( lIgnoreAO ) CLASS AdsDataTable
   LOCAL lChanged, n, cFileName, cPath, cMemo, cData, cTable, aIndex
   IF ValType( ::Socket ) == "C" .AND. Ascan( ::Form:__hObjects:Keys, {|c| Upper(c) == Upper(::Socket) } ) > 0
      ::Socket := ::Form:__hObjects[ ::Socket ]
      IF ! ::DesignMode
         ::Connector := SocketRdd( Self )
      ENDIF
   ENDIF

   IF ::Connection != NIL .AND. Left( ::FileName, 2 ) != "\\"
      cFileName := ::FileName
      n := RAT( "\", cFileName )
      cPath  := SUBSTR( cFileName, 1, n-1 )

      cTable := SubStr( cFileName, n+1 )
      n := RAT( ".", cTable )
      cTable  := Left( cTable, n-1 )

      aIndex := AdsDDFindObject( ADS_DD_INDEX_FILE_OBJECT, cTable, ::Connection )

      // Disaster recovery: Index file deleted
      IF ! Empty(aIndex) .AND. ! File( cPath + "\" + aIndex[1] )
         AdsDDRemoveIndexFile( cTable, aIndex[1], 0, ::Connection )
      ENDIF

      AdsDDAddTable( , ::FileName, ::TableType, ::Connection )
   ENDIF
   ::Connector:Create( lIgnoreAO )
   IF Left( ::FileName, 2 ) != "\\" .AND. ! Empty( ::__aTmpStruct ) .AND. ! Empty ( ::Structure )
      lChanged := LEN(::__aTmpStruct) <> LEN(::Structure)
      IF ! lChanged
         FOR n := 1 TO LEN( ::__aTmpStruct )
             lChanged := Upper(::__aTmpStruct[n][1]) != Upper(::Structure[n][1]) .OR.;
                         ::__aTmpStruct[n][2] != ::Structure[n][2] .OR.;
                         ::__aTmpStruct[n][3] != ::Structure[n][3] .OR.;
                         ::__aTmpStruct[n][4] != ::Structure[n][4]
             IF lChanged
                EXIT
             ENDIF
         NEXT
      ENDIF
      IF lChanged
         cFileName := ::FileName
         n         := RAT( "\", cFileName )
         cData     := SUBSTR( cFileName, 1, n-1 )
         cPath     := GetTempPath()

         IF ::Connection != NIL
            IF hFTConnection == NIL
               IF ! AdsConnect60( SUBSTR( cFileName, 1, n-1 ), , , , , @hFTConnection )
                  RETURN NIL
               ENDIF
            ELSE
               AdsConnection( hFTConnection )
            ENDIF
         ENDIF

         cFileName := SUBSTR( cFileName, n+1 )

         dbCreate( cPath + "__" + cFileName, ::__aTmpStruct, ::Driver, , , , , 0 )

         IF FILE( cPath + "__" + cFileName )
            dbCloseArea( ::Area )

            IF ::Connection != NIL
               AdsDDRemoveTable( cFileName, 0, ::Connection )
            ENDIF

            dbUseArea( ! ::__lMemory, ::Driver, cPath + "__" + cFileName, "modstru", .F., .F.,, 0 )

            SELECT "modstru"
            APPEND FROM (::FileName) VIA (::Driver)
            modstru->( dbCloseArea() )

            FERASE( ::FileName )

            FRENAME( cPath + "__" + cFileName, ::FileName )

            n := RAT( ".", cFileName )
            cMemo := Left( cFileName, n-1 ) + ::MemoExt()

            FERASE( cData + "\" + cMemo )
            FRENAME( cPath + "__" + cMemo, cData + "\" + cMemo )

            IF ::Connection != NIL
               AdsDDAddTable( , ::FileName, ::TableType, ::Connection )
               cFileName := SUBSTR( ::FileName, Rat( "\", ::FileName )+1 )
            ELSE
               cFileName := ::FileName
            ENDIF

         ENDIF

         IF ::Connection != NIL
            AdsConnection( ::Connection )
         ENDIF
         dbSelectArea( ::Area )
         dbUseArea( .F., ::Driver, cFileName, ::Alias, ::Shared, ::ReadOnly, ::CodePage )

      ENDIF
   ENDIF
RETURN Self

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
      IF ! ::RecLock()
         RETURN Self
      ENDIF
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
    ELSEIF ::Structure[n][2] != "A"
      (::Alias)->( FieldPut( n, ::__aData[n] ) )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD FieldPut( nField, xVal ) CLASS AdsDataTable
   IF ::Structure[nField][2] != "A"
      IF Len( ::__aData ) >= nField
         ::__aData[nField] := xVal
       ELSE
         IF ::Structure[nField][2] == "BINARY" .AND. ! Empty( xVal )
            ::File2Blob( xVal, ::Structure[nField][1] )
         ELSE
            ::Connector:FieldPut( nField, xVal )
         ENDIF
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
METHOD CreateOrder( cOrderBagName, cTag, cKey, cFor, bFor, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest, lUnique, lDescend, lAll ) CLASS AdsDataTable
   LOCAL n, cFileName, cPath

   IF Left( ::FileName, 2 ) != "\\" .AND. (::Area)->( OrdNumber( cTag, cOrderBagName ) ) == 0 .OR. ! Upper(cKey) == Upper((::Area)->( IndexKey( OrdNumber( cTag, cOrderBagName ) ) ))
      cFileName := ::FileName
      n         := RAT( "\", cFileName )
      cPath     := Left( cFileName, n-1 )
      cFileName := SubStr( cFileName, n+1 )

      dbCloseArea( ::Area )

      IF ::Connection != NIL
         IF ! AdsDDRemoveTable( Left(cFileName,len(cFileName)-4), 0, ::Connection )
            view AdsGetLastError()
            RETURN NIL
         ENDIF
         AdsDDRemoveIndexFile( Left(cFileName,len(cFileName)-4), cFileName, 0, ::Connection )

         IF hFTConnection == NIL
            IF ! AdsConnect60( cPath, , , , , @hFTConnection )
               RETURN NIL
            ENDIF
         ELSE
            AdsConnection( hFTConnection )
         ENDIF
      ENDIF

      dbSelectArea( ::Area )
      dbUseArea( .F., ::Driver, cPath + "\" + cFileName, ::Alias, .F., ::ReadOnly, ::CodePage )

      (::Area)->( OrdCondSet( cFor, bFor, lAll, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest, lDescend ) )
      (::Area)->( OrdCreate( cOrderBagName, cTag, cKey,, lUnique ) )

      dbCloseArea( ::Area )

      IF ::Connection != NIL
         AdsDDAddTable( , cPath + "\" + cFileName, ::TableType, ::Connection )
         AdsConnection( ::Connection )
      ELSE
         cFileName := cPath + "\" + cFileName
      ENDIF

      dbSelectArea( ::Area )
      dbUseArea( .F., ::Driver, cFileName, ::Alias, ::Shared, ::ReadOnly, ::CodePage )

      RETURN (::Area)->( OrdNumber( cTag, cOrderBagName ) ) > 0 .AND. Upper((::Area)->( IndexKey( OrdNumber( cTag, cOrderBagName ) ) )) == Upper(cKey)
   ENDIF
RETURN .T.

METHOD RecLock( nSecs ) CLASS AdsDataTable
   LOCAL lForever, aInfo
   DEFAULT nSecs TO 5

   IF (::Area)->( dbrlock() )
      RETURN(.T.)
   ENDIF

   lForever := (nSecs == 0)

   IF ::bLockMessage != NIL
      Eval( ::bLockMessage, (::Area)->(Alias()) )
   ENDIF
   DO WHILE (lForever .OR. nSecs > 0)
      IF (::Area)->( dbrlock() )
         IF ::bLockMessage != NIL
            Eval( ::bLockMessage )
         ENDIF
         RETURN(.T.)
      ELSE
         sleep(500)
         nSecs := nSecs - 0.5
      ENDIF
   ENDDO

   aInfo := ::GetLockOwner( (::Area)->( Recno() ) )
   IF ISARRAY( aInfo ) .AND. ! empty( aInfo[1] )
      IF ::bLockMessage != NIL
         Eval( ::bLockMessage, (::Area)->(Alias()), aInfo[1] )
      ENDIF
   ENDIF
   IF ::bLockMessage != NIL
      Eval( ::bLockMessage )
   ENDIF
RETURN .F.

METHOD GetLockOwner( nRec ) CLASS AdsDataTable
   local aInfo, cTableName := (::Area)->(dbInfo(DBI_FULLPATH))
   IF AdsMgConnect( cTableName ) != 0
      RETURN NIL
   ENDIF
   aInfo := (::Area)->(AdsMgGetLockOwner( cTableName, nRec ))
   AdsMgDisConnect( cTableName )
   IF empty(aInfo)
      RETURN NIL
   ENDIF
RETURN aInfo

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS AdsServer INHERIT Component
   DATA hConnection

   PROPERTY Type ROOT "Behavior"  SET AdsSetServerType( v ) DEFAULT ADS_LOCAL_SERVER
   DATA EnumType EXPORTED  INIT { { "Local", "Remote", "Internet" }, {ADS_LOCAL_SERVER,ADS_REMOTE_SERVER,ADS_AIS_SERVER} }

   PROPERTY FileType ROOT "Behavior"  SET AdsSetFileType( v ) DEFAULT ADS_ADT
   DATA EnumFileType EXPORTED  INIT { { "NTX", "CDX", "ADT", "VFP" }, {ADS_NTX,ADS_CDX,ADS_ADT,ADS_VFP} }

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Disconnect( lAll ) INLINE AdsDisconnect( if( ValType(lAll)=="L" .AND. lAll, 0, ::hConnection ) )
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
      AdsSetFileType( ::FileType )
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