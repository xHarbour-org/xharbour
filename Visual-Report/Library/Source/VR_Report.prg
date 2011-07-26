/*
 * $Id$
 */

//-----------------------------------------------------------------------------------------------
// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!
//-----------------------------------------------------------------------------------------------

#include "debug.ch"
#include "vxh.ch"

//-----------------------------------------------------------------------------------------------
#define PIX_PER_INCH   1440

#define  acFileSaveDefault     -1
#define  acFileSaveAll          0
#define  acFileSaveView         1
#define  acFileSaveDesign       2

#define  acScaleVertical        2
#define  acCommandToolZoomIn           53541
#define  acCommandToolZoomOut          53542
#define  acCommandToolPageHome         53773

static nPageNumber

CLASS VrReport INHERIT VrObject
   DATA PrintHeader     EXPORTED  INIT .T.
   DATA PrintRepHeader  EXPORTED  INIT .T.
   DATA PrintFooter     EXPORTED  INIT .T.
   DATA PrintRepFooter  EXPORTED  INIT .T.
   DATA GroupBy         EXPORTED  INIT ""

   DATA ClsName         EXPORTED  INIT "Report"
   DATA Name            EXPORTED  INIT "Report"

   DATA nImage          EXPORTED  INIT 0
   DATA nText           EXPORTED  INIT 0
   DATA nLine           EXPORTED  INIT 0
   DATA nBox            EXPORTED  INIT 0

   DATA FileName        EXPORTED  INIT "Preview"
   DATA Orientation     EXPORTED
   DATA PaperSize       EXPORTED  INIT DMPAPER_LETTER

   DATA LeftMargin      EXPORTED  INIT 1000
   DATA TopMargin       EXPORTED  INIT 1000
   DATA RightMargin     EXPORTED  INIT 1000
   DATA BottomMargin    EXPORTED  INIT 1000

   DATA oPDF            EXPORTED
   DATA PreviewCaption  EXPORTED  INIT "Visual Report - Print Preview"
   DATA nPage           EXPORTED  INIT 0
   DATA nRow            EXPORTED  INIT 0

   DATA xDataSource     PROTECTED
   ACCESS DataSource    INLINE ::xDataSource
   ASSIGN DataSource(o) INLINE ::__SetDataSource(o)

   DATA Button          EXPORTED
   DATA lUI             EXPORTED  INIT .F.

   DATA RepHeaderHeight EXPORTED  INIT 0
   DATA RepFooterHeight EXPORTED  INIT 0
   DATA HeaderHeight    EXPORTED  INIT 0
   DATA FooterHeight    EXPORTED  INIT 0

   DATA aRepHeader      EXPORTED  INIT {}
   DATA aRepFooter      EXPORTED  INIT {}
   DATA aHeader         EXPORTED  INIT {}
   DATA aBody           EXPORTED  INIT {}
   DATA aFooter         EXPORTED  INIT {}
   DATA aExtraPage      EXPORTED  INIT {}
   DATA aComponents     EXPORTED  INIT {}

   DATA hProps          EXPORTED
   DATA hExtra          EXPORTED

   DATA nVirTop         EXPORTED  INIT 0

   ACCESS Application   INLINE __GetApplication()

   METHOD Init()       CONSTRUCTOR
   METHOD Create()
   METHOD Preview()
   METHOD End()
   METHOD StartPage()
   METHOD EndPage()
   METHOD Run()
   METHOD CreateControl()
   METHOD CreateRecord()
   METHOD CreateGroupHeaders()
   METHOD CreateGroupFooters()
   METHOD CreateHeader()
   METHOD CreateFooter()
   METHOD CreateRepHeader()
   METHOD CreateRepFooter()
   METHOD CreateExtraPage()
   METHOD PrepareArrays()
   METHOD Load()
   METHOD InitPDF()
   METHOD Save() INLINE ::oPDF:Save( ::FileName + ".pdf", acFileSaveView )
   METHOD __SetDataSource()
   METHOD ChangePage()
   error HANDLER OnError()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD OnError( ... ) CLASS VrReport
   LOCAL hRet, n, cMsg := __GetMessage()
   IF PCount() == 0
      IF ( n := ASCAN( ::aBody, {|h| UPPER(h:Name) == UPPER(cMsg) } ) ) > 0
         hRet := VAL( ::aBody[n]:Text )
      ENDIF
   ENDIF
RETURN hRet

//-----------------------------------------------------------------------------------------------
METHOD Init() CLASS VrReport
   ::aProperties := {}
   ::Orientation := __GetSystem():PageSetup:Portrait
   AADD( ::aProperties, { "Name",           "Object"  } )
   AADD( ::aProperties, { "DataSource",     "Data"    } )
   AADD( ::aProperties, { "PrintHeader",    "Print"   } )
   AADD( ::aProperties, { "PrintRepHeader", "Print"   } )
   AADD( ::aProperties, { "PrintFooter",    "Print"   } )
   AADD( ::aProperties, { "PrintRepFooter", "Print"   } )
   AADD( ::aProperties, { "GroupBy",        "General" } )
   ::InitPDF()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD InitPDF() CLASS VrReport
   LOCAL oForm
   IF ::oPDF == NIL
      #ifdef VRDLL
         oForm := WinForm():SetInstance( "VReport" ):Init( GetActiveWindow(), {} )
         oForm:Create()
      #else
         oForm := ::Application:MainForm
      #endif
      ::oPDF := ActiveX( oForm )
      ::oPDF:SetChildren := .F.

      ::oPDF:ProgID := "PDFCreactiveX.PDFCreactiveX"
      ::oPDF:Width  := 0
      ::oPDF:Height := 0
      ::oPDF:Create()

      IF ::oPDF == NIL
         MessageBox( 0, "Error loading report generator" )
         RETURN NIL
      ENDIF

      ::oPDF:SetLicenseKey( "WinFakt", "07EFCDAB010001008C5BD0102426F725C273B3A7C1B30B61521A8890359D83AE6FD68732DDAE4AC7E85003CDB8ED4F70678BF1EDF05F" )
      ::oPDF:ObjectAttributeSTR( "Document", "UseSystemFonts", "1" )
      ::oPDF:ObjectAttributeSTR( "Document", "UnicodeFonts"  , "0" )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS VrReport
   ::InitPDF()
   ::nPage := 0
   FERASE( GetTempPath() + "\vr.tmp" )
   ::oPDF:StartSave( GetTempPath() + "\vr.tmp", acFileSaveView )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD End() CLASS VrReport
   ::oPDF:EndSave()
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD StartPage() CLASS VrReport
   ::nRow := 0
   IF ::nPage > 0
      ::oPDF:AddPage( ::nPage )
   ENDIF
   ::nPage++
   ::oPDF:ObjectAttribute( "Pages["+ALLTRIM(STR(::nPage))+"]", "PaperSize", ::PaperSize )
   ::oPDF:ObjectAttribute( "Pages["+ALLTRIM(STR(::nPage))+"]", "Landscape", ::Orientation == __GetSystem():PageSetup:Landscape )
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD EndPage() CLASS VrReport
   ::oPDF:SavePage( ::oPDF:CurrentPage )
   ::oPDF:ClearPage( ::oPDF:CurrentPage )
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD Preview() CLASS VrReport
   LOCAL oPv := VrPreview( Self )
   oPv:Create()
   ::oPDF:Destroy()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateControl( hCtrl, nHeight, oPanel, hDC, nVal, nVirTop, nTop, hTotal ) CLASS VrReport
   LOCAL aCtrl, oControl, x := 0, y := 0, n, cProp, xValue, xVar, oParent

   DEFAULT nVirTop TO 0
   DEFAULT nTop    TO 0

   IF HGetPos( hCtrl, "Left" ) > 0 
      x := VAL( hCtrl:Left )
   ENDIF
   IF HGetPos( hCtrl, "Top" ) > 0 
      y := VAL( hCtrl:Top )-nVirTop
      y += nTop
   ENDIF

   IF oPanel == NIL
      oControl := hb_ExecFromArray( hCtrl:ClsName, {,.F.} )
      oControl:Parent := Self
      oControl:Left := x
      oControl:Top  := y

      FOR EACH cProp IN hCtrl:Keys
          IF ! UPPER( cProp ) IN { "FONT","LEFT","TOP","CPARENT" }
             xVar := __objSendMsg( oControl, cProp )
             xValue := hCtrl[ cProp ]
             IF VALTYPE( xVar ) != VALTYPE( xValue )
                DO CASE
                   CASE VALTYPE( xVar ) == "N"
                        xValue := VAL( xValue )

                   CASE VALTYPE( xVar ) == "D"
                        xValue := DTOC( xValue )

                   CASE VALTYPE( xVar ) == "L"
                        xValue := xValue == "True"
                ENDCASE
             ENDIF
             __objSendMsg( oControl, "_" + cProp, xValue )
          ENDIF
      NEXT

    ELSE

      IF hCtrl:ParName != NIL
         IF ( n := ASCAN( oPanel:Objects, {|o| o:Name == hCtrl:ParName} ) ) > 0
            oParent := oPanel:Objects[n]
         ENDIF
      ENDIF
      oControl := oPanel:CreateControl( hCtrl, x, y, oParent )
   ENDIF

   IF ASCAN( oControl:aProperties, {|a| a[1]=="DataSource"} ) > 0
      IF VALTYPE( oControl:DataSource ) == "C"
         IF ( n := ASCAN( ::Application:Props:CompObjects, {|o| o:Name==oControl:DataSource} ) ) > 0
            oControl:DataSource := ::Application:Props:CompObjects[n]
         ENDIF
      ENDIF
   ENDIF
      
   IF HGetPos( hCtrl, "Font" ) > 0 
      DEFAULT oControl:Font TO Font()
      oControl:Font:FaceName  := hCtrl:Font:FaceName
      oControl:Font:PointSize := VAL( hCtrl:Font:PointSize )
      oControl:Font:Italic    := hCtrl:Font:Italic == "True"
      oControl:Font:Underline := hCtrl:Font:Underline == "True"
      oControl:Font:Weight    := VAL( hCtrl:Font:Weight )
   ENDIF
   IF ! Empty( nVal )
      oControl:Caption := ALLTRIM( STR( nVal ) )
   ENDIF

   IF oPanel == NIL
      IF UPPER( hCtrl:cParent ) == "BODY"
         ::ChangePage( hDC, 0 )
      ENDIF
      oControl:Draw( hDC, hTotal, hCtrl )
      TRY
         IF oControl:ClsName != "Image" .OR. ! oControl:OnePerPage

            y  := ( ::nPixPerInch / GetDeviceCaps( hDC, LOGPIXELSY ) ) * y

            nHeight := MAX( y+oControl:PDFCtrl:Attribute( 'Bottom' )-oControl:PDFCtrl:Attribute( 'Top' ), nHeight )
         ENDIF
      CATCH
      END

    ELSE
      oControl:Configure()
   ENDIF
RETURN oControl

//-----------------------------------------------------------------------------------------------
METHOD __SetDataSource( oData ) CLASS VrReport
   LOCAL n
   IF VALTYPE( oData ) == "C"
      IF ( n := ASCAN( ::Application:Props:CompObjects, {|o| o:Name==oData} ) ) > 0
          oData := ::Application:Props:CompObjects[n]
      ENDIF
   ENDIF
   ::xDataSource := oData
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateGroupHeaders( hDC ) CLASS VrReport
   LOCAL nTop, n, hCtrl, nHeight := 0
   ::nVirTop := 0
   FOR EACH hCtrl IN ::aBody
       IF hCtrl:ClsName == "VRGROUPHEADER"
          ::nVirTop += VAL( hCtrl:Height )
       ENDIF
       IF HGetPos( hCtrl, "ParCls" ) > 0 .AND. hCtrl:ParCls == "VRGROUPHEADER"
          IF ( n := ASCAN( ::aBody, {|h| h:Name == hCtrl:ParName} ) ) > 0
             nTop := VAL( ::aBody[n]:Top )
          ENDIF
          ::CreateControl( hCtrl, @nHeight,, hDC,,, nTop )
       ENDIF
   NEXT
   n  := ( ::nPixPerInch / GetDeviceCaps( hDC, LOGPIXELSY ) ) * ::nVirTop
   ::nRow += n
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateGroupFooters( hDC ) CLASS VrReport
   LOCAL nTop, n, hCtrl, hTotal, nHeight := 0
   ::nVirTop := 0
   FOR EACH hCtrl IN ::aBody
       IF HGetPos( hCtrl, "ParCls" ) > 0 .AND. hCtrl:ParCls == "VRGROUPFOOTER"
          IF hCtrl:ClsName=="VRTOTAL"
             IF VALTYPE( hCtrl:Value ) == "N"
                hCtrl:Text := XSTR( hCtrl:Value )
                hCtrl:Value := ""
              ELSE
                IF !EMPTY( hCtrl:Value )
                   hCtrl:Text := &(hCtrl:Value)
                ENDIF
             ENDIF
          ENDIF

          IF ( n := ASCAN( ::aFooter, {|h| h:ClsName=="VRTOTAL" .AND. h:Column==hCtrl:Name} ) ) > 0
             hTotal := ::aFooter[n]
          ENDIF

          ::CreateControl( hCtrl, @nHeight,, hDC,,,, hTotal )
       ENDIF
   NEXT
   n  := ( ::nPixPerInch / GetDeviceCaps( hDC, LOGPIXELSY ) ) * ::nVirTop
   ::nRow += n
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateRecord( hDC ) CLASS VrReport
   LOCAL hCtrl, nTop, nHeight := 0, oCtrl, n, hTotal
   FOR EACH hCtrl IN ::aBody
       IF ( HGetPos( hCtrl, "ParCls" ) == 0 .OR. ! (hCtrl:ParCls IN {"VRGROUPHEADER","VRGROUPFOOTER"}) ) .AND. ! (hCtrl:ClsName IN {"VRGROUPHEADER","VRGROUPFOOTER"})
          hTotal := NIL
          IF ( n := ASCAN( ::aBody, {|h| h:ClsName=="VRTOTAL" .AND. h:Column==hCtrl:Name} ) ) > 0
             hTotal := ::aBody[n]
          ENDIF
          oCtrl := ::CreateControl( hCtrl, @nHeight,, hDC,, ::nVirTop,, hTotal )
       ENDIF
   NEXT
   ::nRow += nHeight
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateRepHeader( hDC ) CLASS VrReport
   LOCAL hCtrl, nHeight := 0
   IF ::PrintRepHeader
      FOR EACH hCtrl IN ::aRepHeader
          ::CreateControl( hCtrl, @nHeight,, hDC )
      NEXT
      ::nRow += ::RepHeaderHeight
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateHeader( hDC ) CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   IF ::PrintHeader
      FOR EACH aCtrl IN ::aHeader
          ::CreateControl( aCtrl, @nHeight,, hDC )
      NEXT
      ::nRow += ::HeaderHeight
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateRepFooter( hDC ) CLASS VrReport
   LOCAL hCtrl, nHeight := 0
   IF ::PrintRepFooter
      FOR EACH hCtrl IN ::aRepFooter
          IF hCtrl:ClsName=="VRTOTAL"
             IF VALTYPE( hCtrl:Value ) == "N"
                hCtrl:Text := XSTR( hCtrl:Value )
                hCtrl:Value := ""
              ELSE
                IF !EMPTY( hCtrl:Value )
                   hCtrl:Text := &(hCtrl:Value)
                ENDIF
             ENDIF
          ENDIF

          ::CreateControl( hCtrl, @nHeight,, hDC )
      NEXT
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateFooter( hDC ) CLASS VrReport
   LOCAL hCtrl, nHeight := 0
   IF ::PrintFooter
      ::nRow := ::oPDF:PageLength - ::FooterHeight
      FOR EACH hCtrl IN ::aFooter

          IF hCtrl:ClsName=="VRTOTAL"
             IF VALTYPE( hCtrl:Value ) == "N"
                hCtrl:Text := XSTR( hCtrl:Value )
                hCtrl:Value := ""
              ELSE
                IF !EMPTY( hCtrl:Value )
                   hCtrl:Text := &(hCtrl:Value)
                ENDIF
             ENDIF
          ENDIF

          ::CreateControl( hCtrl, @nHeight,, hDC )
      NEXT
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateExtraPage( hDC ) CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   ::nRow := 0
   FOR EACH aCtrl IN ::aExtraPage
       ::CreateControl( aCtrl, @nHeight,, hDC )
   NEXT
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD PrepareArrays( oDoc ) CLASS VrReport
   LOCAL oNode, cData, n, cParent, hDC, hControl, cParName, cParCls

   ::hProps := {=>}
   ::hExtra := {=>}
   HSetCaseMatch( ::hProps, .F. )
   HSetCaseMatch( ::hExtra, .F. )

   oNode := oDoc:FindFirstRegEx( "Report" )

   WHILE oNode != NIL
      DO CASE
         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              DEFAULT oNode:cData TO ""
              ::hProps[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "ExtraPage" .AND. oNode:oParent:oParent:cName == "Report" .AND. oNode:cName != "Control"
              DEFAULT oNode:cData TO ""
              ::hExtra[ oNode:cName ] := oNode:cData



         CASE oNode:cName == "Control" 
              IF !EMPTY( hControl )
                 hControl:cParent := SUBSTR( cParent, 2 )
                 AADD( ::&cParent, hControl )
              ENDIF
              hControl := {=>}
              HSetCaseMatch( hControl, .F. )
              hControl[ "ParName" ] := NIL
              IF UPPER( oNode:oParent:cName ) != "CONTROL"
                 cParent := "a" + oNode:oParent:cName
               ELSE
                 hControl[ "ParName" ] := cParName
                 hControl[ "ParCls"  ] := cParCls
              ENDIF

         CASE oNode:cName == "Font" 
              hControl[ oNode:cName ] := {=>}
              HSetCaseMatch( hControl[ oNode:cName ], .F. )

         CASE oNode:oParent:cName == "Control"
              DEFAULT oNode:cData TO ""
              hControl[ oNode:cName ] := oNode:cData
              IF oNode:cName == "Name" .AND. hControl[ "ClsName" ] IN {"VRGROUPHEADER","VRGROUPFOOTER"}
                 cParName := oNode:cData
              ENDIF
              IF oNode:cName == "ClsName" .AND. oNode:cData IN {"VRGROUPHEADER","VRGROUPFOOTER"}
                 cParCls := oNode:cData
              ENDIF

         CASE oNode:oParent:cName == "Font"
              DEFAULT oNode:cData TO ""
              hControl[ oNode:oParent:cName ][ oNode:cName ] := oNode:cData

      ENDCASE
      oNode := oDoc:Next()
   ENDDO
   IF !EMPTY( hControl )
      hControl:cParent := SUBSTR( cParent, 2 )
      AADD( ::&cParent, hControl )
   ENDIF
#ifndef VRDLL
   TRY
      n := ::Application:Props[ "RepHeader" ]:Height - ::Application:Props[ "RepHeader" ]:ClientHeight
           ::Application:Props[ "RepHeader" ]:Height := VAL( ::hProps:RepHeaderHeight )+n

      n := ::Application:Props[ "RepFooter" ]:Height - ::Application:Props[ "RepFooter" ]:ClientHeight
           ::Application:Props[ "RepFooter" ]:Height := VAL( ::hProps:RepFooterHeight )+n
   CATCH
   END
   n := ::Application:Props[ "Header" ]:Height - ::Application:Props[ "Header" ]:ClientHeight
        ::Application:Props[ "Header" ]:Height := VAL( ::hProps:HeaderHeight )+n

   n := ::Application:Props[ "Footer" ]:Height - ::Application:Props[ "Footer" ]:ClientHeight
        ::Application:Props[ "Footer" ]:Height := VAL( ::hProps:FooterHeight )+n

        ::Application:Props[ "Footer" ]:Dockit()
        ::Application:Props[ "Body" ]:Dockit()
#endif
   TRY
      ::Orientation  := VAL( ::hProps:Orientation )
   CATCH
   END
   TRY
      ::PaperSize   := VAL( ::hProps:PaperSize )

      ::LeftMargin  := VAL( ::hProps:LeftMargin )
      ::TopMargin   := VAL( ::hProps:TopMargin )
      ::RightMargin := VAL( ::hProps:RightMargin )
      ::BottomMargin:= VAL( ::hProps:BottomMargin )
   CATCH
   END

   hDC := GetDC(0)
   ::HeaderHeight := VAL( ::hProps:HeaderHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
   ::FooterHeight := VAL( ::hProps:FooterHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
   TRY
      ::RepHeaderHeight := VAL( ::hProps:RepHeaderHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
      ::RepFooterHeight := VAL( ::hProps:RepFooterHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
   CATCH
   END
   ReleaseDC(0, hDC)
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Load( cReport ) CLASS VrReport
   LOCAL n, hCtrl, oCtrl, oDoc := TXmlDocument():New( cReport )

   ::PrepareArrays( oDoc )
   
   FOR EACH hCtrl IN ::aComponents
       oCtrl := ::CreateControl( hCtrl,, ::Application:Props:Body )
       IF !EMPTY( ::hProps:DataSource ) .AND. hCtrl:Name == ::hProps:DataSource
          ::DataSource := oCtrl
       ENDIF
   NEXT

   ::PrintHeader    := ::hProps:PrintHeader    == "1"
   ::PrintRepHeader := ::hProps:PrintRepHeader == "1"
   ::PrintFooter    := ::hProps:PrintFooter    == "1"
   ::PrintRepFooter := ::hProps:PrintRepFooter == "1"
   ::GroupBy        := ::hProps:GroupBy
   
   FOR EACH hCtrl IN ::aRepHeader
       ::CreateControl( hCtrl,, ::Application:Props:RepHeader )
   NEXT
   FOR EACH hCtrl IN ::aRepFooter
       ::CreateControl( hCtrl,, ::Application:Props:RepFooter )
   NEXT
   FOR EACH hCtrl IN ::aHeader
       ::CreateControl( hCtrl,, ::Application:Props:Header )
   NEXT
   FOR EACH hCtrl IN ::aBody
       ::CreateControl( hCtrl,, ::Application:Props:Body )
   NEXT
   FOR EACH hCtrl IN ::aFooter
       ::CreateControl( hCtrl,, ::Application:Props:Footer )
   NEXT
   TRY
      ::Application:Props:ExtraPage:PagePosition := VAL( ::hExtra:PagePosition )
   CATCH
   END
   FOR EACH hCtrl IN ::aExtraPage
       ::CreateControl( hCtrl,, ::Application:Props:ExtraPage )
   NEXT
RETURN oDoc

//-----------------------------------------------------------------------------------------------
METHOD Run( oDoc, oWait ) CLASS VrReport
   LOCAL nHeight, hDC, nSubHeight, nTotHeight, nCount, nPer, nPos, nRow, oData, hCtrl, hData := {=>}
   LOCAL xValue, cData, oIni, cEntry

   ::Create()

   IF oDoc != NIL
      ::PrepareArrays( oDoc )
   ENDIF

   ::PrintHeader    := ::hProps:PrintHeader    == "1"
   ::PrintRepHeader := ::hProps:PrintRepHeader == "1"
   ::PrintFooter    := ::hProps:PrintFooter    == "1"
   ::PrintRepFooter := ::hProps:PrintRepFooter == "1"
   ::GroupBy        := ::hProps:GroupBy

   FOR EACH hCtrl IN ::aComponents
       IF hCtrl:ClsName == "VRDATATABLE"
          oData := DataTable( NIL )
          oData:Driver           := hCtrl:Driver
          oData:FileName         := hCtrl:FileName
          
          IF hCtrl:Driver != "SQLRDD"
             IF !EMPTY( hCtrl:Alias )
                oData:Alias := hCtrl:Alias
             ENDIF
             oData:Create()
             IF ! EMPTY( hCtrl:Filter )
                hCtrl:Filter := STRTRAN( hCtrl:Filter, "@TODAY", 'CTOD("'+DTOC(DATE())+'")' )
                oData:SetFilter( &(hCtrl:Filter) )
             ENDIF
             IF ! EMPTY( hCtrl:Order )
                oData:OrdSetFocus( hCtrl:Order )
             ENDIF
           
           ELSEIF !EMPTY( hCtrl:ConnectionFile )
             oData:DataConnector        := SqlConnector( NIL )
             oData:DataConnector:Server := hCtrl:Server
             IF FILE( hCtrl:ConnectionFile )
                oIni   := IniFile( hCtrl:ConnectionFile )
                cEntry := oIni:ReadString( "SQL", "OPEN" )
                oData:DataConnector:ConnectionString := oIni:ReadString( "SQL", cEntry )
              ELSE
                oData:DataConnector:ConnectionString := hCtrl:ConnectionFile
             ENDIF
             oData:DataConnector:Create()
             oData:Create()
          ENDIF
          hData[ hCtrl:Name ] := oData

          IF !EMPTY( ::hProps:DataSource ) .AND. hCtrl:Name == ::hProps:DataSource
             ::DataSource := oData
          ENDIF

       ENDIF
   NEXT

   FOR EACH hCtrl IN ::aComponents
       IF hCtrl:ClsName == "VRDATATABLE"
          cData := hCtrl:Name
          IF ! EMPTY( hCtrl:RelationTable )
             hData[cData]:SetRelation( hData[ hCtrl:RelationTable ], hCtrl:RelationExp )
          ENDIF
       ENDIF
   NEXT

   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      TRY
         ::DataSource:Select()
         ::DataSource:GoTop()
      CATCH
         MessageBox(0, "Database error" )
         ::End()
         hb_gcall(.t.)
         HEVAL( hData, {|cKey,o| IIF( o:IsOpen, o:Close(),)} )
         RETURN .F.
      END
   ENDIF

   ::StartPage()
   hDC := GetDC(0)
#ifndef VRDLL
   IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == -1
      ::CreateExtraPage( hDC )
      ::EndPage()
      ::StartPage()
   ENDIF
#endif
   nPageNumber := 1

   ::CreateRepHeader( hDC )
   ::CreateHeader( hDC )

   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      ::DataSource:Select()
      ::DataSource:GoTop()
      nCount := ::DataSource:OrdkeyCount()
      nPos := 0

      nHeight := ::CreateGroupHeaders( hDC )
      IF !EMPTY(::GroupBy)
         xValue := ::DataSource:Fields:&(::GroupBy)
      ENDIF
      WHILE ! ::DataSource:Eof()
         nHeight := ::CreateRecord( hDC )
         IF xValue != NIL .AND. ::DataSource:Fields:&(::GroupBy) != xValue
            xValue  := ::DataSource:Fields:&(::GroupBy)
            ::nRow += 100
            nHeight := ::CreateGroupFooters( hDC )
            ::nRow += 500
            nHeight := ::CreateGroupHeaders( hDC )
         ENDIF
         IF oWait != NIL
            oWait:Position := Int( (nPos/nCount)*100 )
         ENDIF
         nPos ++
         ::DataSource:Skip()
      ENDDO
   ENDIF

   ::CreateRepFooter( hDC )
   ::CreateFooter( hDC )

#ifndef VRDLL
   IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == 0
      ::CreateExtraPage( hDC )
   ENDIF
#endif

   ReleaseDC(0, hDC)

   ::EndPage()
   ::End()
   hb_gcall(.t.)
   HEVAL( hData, {|cKey,o| IIF( o:IsOpen, o:Close(),)} )
RETURN .T.

METHOD ChangePage( hDC, nHeight )
   IF ::nRow + nHeight + IIF( ::PrintFooter, ::FooterHeight, 0 ) > ::oPDF:PageLength
#ifndef VRDLL
      IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == 0
         ::CreateExtraPage( hDC )
      ENDIF
#endif
      ::CreateFooter( hDC )
      ::EndPage()
      ::StartPage()
      nPageNumber ++
      ::CreateHeader( hDC )
      RETURN .T.
   ENDIF
RETURN .F.


FUNCTION S2R( hDC, cSize ); RETURN VAL(cSize)*PIX_PER_INCH/GetDeviceCaps( hDC, LOGPIXELSY )
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS VrPreview INHERIT Dialog
   DATA Report EXPORTED
   DATA oPDF   EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oReport ) CLASS VrPreview
   ::Report := oReport
   ::Super:Init( __GetApplication():MainForm )
   ::Modal      := .T.
   ::Top        := 300
   ::Width      := 800
   ::Height     := 900
//   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::DlgModalFrame := .T.
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS VrPreview
   LOCAL oItem, oSub, nZoom
   ::Caption := ::Report:PreviewCaption

   WITH OBJECT ToolStrip( Self )
      :ShowChevron := .F.
      :ShowGrip    := .F.
      :ImageList   := ImageList( :this, 32, 32 ):Create()
      :Height      := 38
      :ImageList:AddImage( "ICO_ZOOMIN" )
      :ImageList:AddImage( "ICO_ZOOMOUT" )
      :ImageList:AddImage( "ICO_PRINT" )
      :Create()
      WITH OBJECT ToolStripButton( :this )
         :Caption           := "Zoom-In"
         :ImageIndex        := 1
         :Action            := {|o| ::Report:oPDF:DoCommandTool( acCommandToolZoomIn )}
         :Create()
      END
      WITH OBJECT ToolStripButton( :this )
         :Caption           := "Zoom-Out"
         :ImageIndex        := 2
         :Action            := {|o| ::Report:oPDF:DoCommandTool( acCommandToolZoomOut )}
         :Create()
      END
      WITH OBJECT ToolStripButton( :this )
         :Caption           := "Print"
         :Begingroup        := .T.
         :ImageIndex        := 3
         :Action            := <|o| 
                                TRY
                                  ::Report:oPDF:Print( "", .T. )
                                CATCH
                                END
                                >
         :Create()
      END
   END
   
   WITH OBJECT StatusBar( Self )
      StatusBarPanel( ::StatusBar1, , 120 )
      StatusBarPanel( ::StatusBar1, ,  -1 )
      StatusBarPanel( ::StatusBar1, , 250 )
      :Create()
      :DockIt()
   END

   WITH OBJECT ::Report:oPDF
      :SetParent( Self )
      :Dock:Left   := Self
      :Dock:Top    := ::ToolStrip1
      :Dock:Right  := Self
      :Dock:Bottom := ::StatusBar1
      :DockIt()
      :Width       := 300
      :Height      := 300
      :RulerSize   := 0
      :MinimumGap  := 5
      :DoCommandTool( acCommandToolPageHome )
      nZoom        := ::Application:IniFile:Read( "Preview", "ZoomFactor", 0 )
      IF nZoom > 0
         :ZoomFactor := nZoom
       ELSE
         :ScaleToWindow( acScaleVertical )
      ENDIF
      :Show()
   END

   ::CenterWindow( .T. )
RETURN Self

#ifdef VRDLL

CLASS __VReport INHERIT Application
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oParent, aParameters ) CLASS __VReport
   ::Super:Init( oParent, aParameters )
   ::Create()
RETURN Self

#endif

FUNCTION PageNumber(); RETURN nPageNumber