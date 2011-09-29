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

static nPageNumber

//-----------------------------------------------------------------------------------------------
#define FC_EQUALTO    "Equals to"
#define FC_NOTEQUALTO "Is not equal to"
#define FC_GREATEREQU "Greater than or equal"
#define FC_LESSEQUAL  "Less than or equal"
#define FC_BETWEEN    "Between"
#define FC_INTHERANGE "Is in the range"
#define FC_CONTAINS   "Contains"
#define FC_NOTCONTAIN "Does not contain"
#define FC_BEGWITH    "Begins with"
#define FC_NOTBEGWITH "Does not begin with"
#define FC_ISEMPTY    "Is empty"
#define FC_NOTEMPTY   "Is not empty"
#define FC_PERQUARTER "Per quarter"
#define FC_INLAST     "Is in the last"
#define FC_NOTINLAST  "Is not in the last"
#define FC_TRUE       "True"
#define FC_FALSE      "False"

#define PIX_PER_INCH   1440

#define  acFileSaveDefault     -1
#define  acFileSaveAll          0
#define  acFileSaveView         1
#define  acFileSaveDesign       2

#define  acScaleVertical        2
#define  acCommandToolZoomIn           53541
#define  acCommandToolZoomOut          53542
#define  acCommandToolPageHome         53773

CLASS VrReport INHERIT VrObject
   DATA oForm           EXPORTED
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
         ::oForm := WinForm():SetInstance( "VReport" ):Init( GetActiveWindow(), {} )
         ::oForm:Create()
      #else
         ::oForm := ::Application:MainForm
      #endif
      ::oPDF := ActiveX( ::oForm )
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

         CASE oNode:cName IN { "Font", "Filter" }
              hControl[ oNode:cName ] := {=>}
              HSetCaseMatch( hControl[ oNode:cName ], .F. )

         CASE oNode:cName == "AskMeLater"
              ATAIL( hControl:Filter:Expressions ):AskMeLater := {=>}
              HSetCaseMatch( ATAIL( hControl:Filter:Expressions ):AskMeLater, .F. )

         CASE LEFT(oNode:cName,10) == "Expression" 
              IF HGetPos( hControl[ "Filter" ], "Expressions" ) == 0
                 hControl[ "Filter" ][ "Expressions" ] := {}
              ENDIF
              AADD( hControl[ "Filter" ][ "Expressions" ], {=>} )
              HSetCaseMatch( ATAIL( hControl[ "Filter" ][ "Expressions" ]), .F. )

         CASE oNode:oParent:cName == "Control"
              DEFAULT oNode:cData TO ""
              hControl[ oNode:cName ] := oNode:cData
              IF oNode:cName == "Name" .AND. hControl[ "ClsName" ] IN {"VRGROUPHEADER","VRGROUPFOOTER"}
                 cParName := oNode:cData
              ENDIF
              IF oNode:cName == "ClsName" .AND. oNode:cData IN {"VRGROUPHEADER","VRGROUPFOOTER"}
                 cParCls := oNode:cData
              ENDIF

         CASE oNode:oParent:cName IN { "Font", "Filter" } 
              DEFAULT oNode:cData TO ""
              hControl[ oNode:oParent:cName ][ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "AskMeLater"
              DEFAULT oNode:cData TO ""
              ATAIL( hControl:Filter:Expressions ):AskMeLater[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName != NIL .AND. LEFT( oNode:oParent:cName, 10 ) == "Expression" 
              DEFAULT oNode:cData TO ""
              IF oNode:cName == "AndOr" .AND. EMPTY( oNode:cData )
                 oNode:cData := NIL
              ENDIF
              IF oNode:cName == "FieldSel"
                 oNode:cData := VAL( oNode:cData )
              ENDIF
              IF oNode:cName == "ExpSel"
                 oNode:cData := VAL( oNode:cData )
              ENDIF
              ATAIL( hControl[ "Filter" ][ "Expressions" ] )[ oNode:cName ] := oNode:cData

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
   LOCAL xValue, cFilter, cData, oIni, cEntry, aRelation, aRelations, cRelation
   
   ::Create()

   IF oDoc != NIL .AND. ::hProps == NIL
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
          oData:Driver   := hCtrl:Driver
          oData:FileName := hCtrl:FileName
          oData:xName    := hCtrl:Name
          IF hCtrl:Driver != "SQLRDD"
             IF !EMPTY( hCtrl:Alias )
                oData:Alias := hCtrl:Alias
             ENDIF
             oData:Create()
             IF HGetPos( hCtrl, "Filter" ) > 0  .AND. ! EMPTY( hCtrl:Filter )
                cFilter := BuildFilterExp( hCtrl:Filter )
                IF cFilter == NIL
                   IF oWait != NIL
                      oWait:Destroy()
                   ENDIF
                   MessageBox( GetActiveWindow(), "Report generation cancelled", "Ask me later" )
                   hb_gcall(.t.)
                   HEVAL( hData, {|cKey,o| IIF( o:IsOpen, o:Close(),)} )
                   ::End()
                   return .f.
                ENDIF
                IF !EMPTY( cFilter )
                   oData:SetFilter( &("{||"+cFilter+"}") )
                ENDIF
             ENDIF
             IF ! EMPTY( hCtrl:Order )
                oData:OrdSetFocus( hCtrl:Order )
             ENDIF
           
           ELSEIF !EMPTY( hCtrl:ConnectionFile )
             oData:BindingSource        := SqlConnector( NIL )
             oData:BindingSource:Server := hCtrl:Server
             IF FILE( hCtrl:ConnectionFile )
                oIni   := IniFile( hCtrl:ConnectionFile )
                cEntry := oIni:ReadString( "SQL", "OPEN" )
                oData:BindingSource:ConnectionString := oIni:ReadString( "SQL", cEntry )
              ELSE
                oData:BindingSource:ConnectionString := hCtrl:ConnectionFile
             ENDIF
             
             TRY
                oData:BindingSource:Create()
             CATCH
                IF oWait != NIL
                   oWait:Destroy()
                ENDIF
                MessageBox( GetActiveWindow(), "Error connecting to SQL server", "VR" )
                hb_gcall(.t.)
                HEVAL( hData, {|cKey,o| IIF( o:IsOpen, o:Close(),)} )
                ::End()
                return .f.
             END
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
          IF ! EMPTY( hCtrl:Relation )
             aRelations := hb_aTokens( hCtrl:Relation, "," )
             FOR EACH cRelation IN aRelations
                 aRelation := hb_aTokens( cRelation, "|" )
                 IF LEN( aRelation ) == 2
                    (hData[cData]:Alias)->( dbSetRelation( ALLTRIM(aRelation[2]), &("{||"+ALLTRIM(aRelation[1])+"}") ) )
                 ENDIF
             NEXT
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
   IF ::Application:Props:ExtraPage:PagePosition > -2
      ::CreateExtraPage( hDC )
      IF ::Application:Props:ExtraPage:PagePosition == -1
         ::EndPage()
         ::StartPage()
      ENDIF
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

   ReleaseDC(0, hDC)

   ::EndPage()
   ::End()
   hb_gcall(.t.)
   HEVAL( hData, {|cKey,o| IIF( o:IsOpen, o:Close(),)} )
RETURN .T.

METHOD ChangePage( hDC, nHeight )
   IF ::nRow + nHeight + IIF( ::PrintFooter, ::FooterHeight, 0 ) > ::oPDF:PageLength
      ::CreateFooter( hDC )
      ::EndPage()
      ::StartPage()
#ifndef VRDLL
      IF ::Application:Props:ExtraPage:PagePosition == 0
         ::CreateExtraPage( hDC )
      ENDIF
#endif
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
   ::Super:Init( oReport:oForm )
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

//------------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------------

FUNCTION AskLater( cField, cType, nCond, hExp )
RETURN VrAskLater( NIL, cField, cType, nCond, hExp ):cResult

CLASS VrAskLater INHERIT Dialog
   DATA cResult     EXPORTED
   DATA cField      EXPORTED INIT ""
   DATA cType       EXPORTED
   DATA oCond       EXPORTED
   DATA cEdit       EXPORTED
   DATA nCond       EXPORTED
   DATA hExp        EXPORTED
   DATA oGet1, oGet2
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD SetDateEdit()
   METHOD OK_OnClick()
   METHOD ComboBox1_OnCBNSelEndOk()
   METHOD BrowseF3_OnClick()
ENDCLASS

METHOD Init( oParent, cField, cType, nCond, hExp ) CLASS VrAskLater
   ::Super:Init( oParent )
   ::Modal       := .T.
   ::Left        := 11
   ::Top         := 10
   ::Width       := 434
   ::Height      := 213
   ::Center      := .T.
   ::Caption     := "Please enter value"
   ::TopMost     := .T.
   ::MaximizeBox := .F.
   ::MinimizeBox := .F.
   ::nCond       := nCond
   ::hExp        := hExp
   ::Caption     := hExp:AskMeLater:Title
   ::Icon        := "AVR"
   ::cField      := cField
   ::cType       := cType
   ::cEdit := "EditBox"
   IF ::cType == "D"
      ::cEdit := "DateTimePicker"
   ENDIF
   ::oCond       := Conditions( NIL )
   ::Create()
RETURN Self

METHOD OnInitDialog() CLASS VrAskLater
   LOCAL cType := ::cType
   WITH OBJECT ( PictureBox( Self ) )
      :Name             := "PictureBox1"
      WITH OBJECT :Dock
         :Left          := Self
         :Right         := Self
         :Bottom        := Self
         :Margins       := "0,0,0,0"
      END
      :Left             := 0
      :Top              := 125
      :Width            := 418
      :Height           := 50
      :ImageName        := "_BOTTOMRIBBOJPG"
      :Stretch          := .T.
      :Create()

      WITH OBJECT ( Button( :this ) )
         :Dock:Right    := :Parent
         :Dock:Margins  := "0,0,20,0"
         :Left          := 311
         :Top           := 13
         :Width         := 80
         :Height        := 25
         :Caption       := "OK"
         :EventHandler[ "OnClick" ] := "OK_OnClick"
         :Create()
      END

      WITH OBJECT ( Button( :this ) )
         :Dock:Left     := :Parent
         :Dock:Margins  := "20,0,0,0"
         :Left          := 14
         :Top           := 13
         :Width         := 80
         :Height        := 25
         :Caption       := "Help"
         :Create()
      END

   END

   WITH OBJECT ( GroupBox( Self ) )
      :Caption      := IIF( EMPTY( ::hExp:AskMeLater:GroupText ), ::cField, ::hExp:AskMeLater:GroupText )
      :Dock:Margins := "20,15,20,70"
      :Left         := 20
      :Top          := 15
      :Width        := 379
      :Height       := 88
      :ForeColor    := 0
      :Create()
      :DockToParent()
      WITH OBJECT ( ComboBox( :this ) )
         :Name            := "ComboBox1"
         :Left            := 20
         :Top             := 37
         :Width           := 150
         :Height          := 200
         :SelectionHeight := 17
         :ItemHeight      := 17
         :EventHandler[ "OnCBNSelEndOk" ] := "ComboBox1_OnCBNSelEndOk"
         :Create()
         IF cType != NIL
            AEVAL( ::oCond:aCond_&cType, {|a| :AddItem(a[1]) } )
         ENDIF
         :SetCurSel( ::nCond )
      END
      
      WITH OBJECT EditBox( :this )
         :Caption      := ::hExp:Exp1
         :Name         := "EditBox1"
         :Left         := 180
         :Top          := 37
         :Width        := 160
         :Height       := 22
         :AutoHScroll  := .T.
         :Visible      := .F.
         :Create()
      END

      WITH OBJECT EditBox( :this )
         :Caption      := ::hExp:Exp2
         :Name         := "EditBox2"
         :Left         := 270
         :Top          := 37
         :Width        := 78
         :Height       := 22
         :AutoHScroll  := .T.
         :Visible      := .F.
         :Create()
      END

      WITH OBJECT DateTimePicker( :this )
         :Caption      := ::hExp:Exp1
         :Name         := "DateTimePicker1"
         :Left         := 180
         :Top          := 37
         :Width        := 160
         :Height       := 22
         :Visible      := .F.
         :Create()
      END

      WITH OBJECT DateTimePicker( :this )
         :Caption      := ::hExp:Exp2
         :Name         := "DateTimePicker2"
         :Left         := 270
         :Top          := 37
         :Width        := 78
         :Height       := 22
         :Visible      := .F.
         :Create()
      END
      WITH OBJECT ( Button( :this ) )
         :Name         := "BrowseF3"
         :Left         := 344
         :Top          := 35
         :Width        := 30
         :Height       := 25
         :Caption      := "..."
         :Visible      := ! EMPTY( ::hExp:AskMeLater:Search )
         :EventHandler[ "OnClick" ] := "BrowseF3_OnClick"
         :Create()
      END

   END
   ::SetDateEdit( cType )
RETURN Self

METHOD BrowseF3_OnClick() CLASS VrAskLater
   LOCAL aTemp := HB_ATOKENS( ::hExp:AskMeLater:Search, CRLF )
   ::oGet1:Caption  := wf_BrowseF3(aTemp, .T., ALIAS(), .F. )
RETURN Self

METHOD OK_OnClick() CLASS VrAskLater
   LOCAL cExp1, cExp2, bExp, cType, nSel, oGet1, oGet2, aExp, nNum, cExpSel, cField
   IF ::cField != NIL
      cExp1   := ::oGet1:Caption
      cExp2   := ::oGet2:Caption
      cType   := ::cType
      cExpSel := ::ComboBox1:GetSelString()
      cField  := ::cField

      IF cType == "A"
         cField := "TRIM("+::cField+"[1])"
         cExp1 := ValToPrg( cExp1 )
         cExp2 := ValToPrg( cExp2 )

       ELSEIF cType $ "CM"
         cField := "TRIM("+::cField+")"
         cExp1 := ValToPrg( cExp1 )
         cExp2 := ValToPrg( cExp2 )

       ELSEIF cType == "N"
         cExp1 := ValToPrg( VAL( cExp1 ) )
         cExp2 := ValToPrg( VAL( cExp2 ) )

       ELSEIF cType == "D" .AND. ! ( cExpSel == FC_INTHERANGE )
         IF cExpSel IN {FC_INLAST, FC_NOTINLAST}
            aExp  := hb_aTokens( ::oGet1:Caption )
            cExp1 := "@TODAY-"
            nNum  := VAL( aExp[1] )
            IF aExp[2] == "days"
               cExp1 += aExp[1]
             ELSEIF aExp[2] == "weeks"
               cExp1 += AllTrim( Str( nNum*7 ) )
             ELSEIF aExp[2] == "months"
               cExp1 += AllTrim( Str( nNum*30 ) )
            ENDIF
          ELSEIF cExpSel IN {FC_PERQUARTER}
            aExp  := hb_aTokens( ::oGet1:Caption )
            cExp1 := 'MONTH('+cField+')>='+aExp[2]+'.AND.MONTH('+cField+')<='+aExp[4]
          ELSE
            cExp1 := 'STOD( "' + DTOS(::oGet1:Date) + '" )'
            cExp2 := 'STOD( "' + DTOS(::oGet2:Date) + '" )'
         ENDIF
      ENDIF

      nSel  := ::ComboBox1:GetCurSel()
      bExp  := ::oCond:aCond_&cType[nSel][2]
      ::cResult := EVAL( bExp, cField, cExp1, cExp2 )
   ENDIF
   ::Close()
RETURN Self

METHOD ComboBox1_OnCBNSelEndOk( Sender ) CLASS VrAskLater
   LOCAL cSel, oDlg, oPanel := Sender:Parent

   cSel := Sender:GetSelString()
   ::SetDateEdit( ::cType )

   ::oGet1:Enabled := .T.

   IF cSel == FC_BETWEEN
      ::oGet1:Width := 78
      ::oGet2:Visible := .T.

    ELSEIF cSel IN {FC_PERQUARTER}
      ::SetDateEdit( "C" )
      ::oGet1:Enabled := .F.
      oDlg := FilterPerQuarter( Self, ::oGet1 )
      IF oDlg:Result == IDOK
         ::oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
      ENDIF
      ::oGet1:Enabled := .F.

    ELSEIF cSel IN {FC_INLAST, FC_NOTINLAST}
      ::SetDateEdit( "C" )
      ::oGet1:Enabled := .F.
      oDlg := IsInTheLast( Self, cSel, {"days", "weeks", "months"}  )
      IF oDlg:Result == IDOK
         ::oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
      ENDIF
      ::oGet1:Enabled := .F.

    ELSEIF cSel IN {FC_ISEMPTY, FC_NOTEMPTY}
      ::SetDateEdit( "C" )
      ::oGet1:Caption := ""
      ::oGet1:Enabled := .F.

   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD SetDateEdit( cType ) CLASS VrAskLater
   DEFAULT cType TO ::cType
   IF ::oGet1 != NIL
      ::oGet1:Visible := .F.
      ::oGet2:Visible := .F.
   ENDIF
   IF cType == "D" .AND. ! ( ::ComboBox1:GetSelString() == FC_INTHERANGE )
      ::oGet1 := ::DateTimePicker1
      ::oGet2 := ::DateTimePicker2
    ELSE
      ::oGet1 := ::EditBox1
      ::oGet2 := ::EditBox2
   ENDIF
   ::oGet1:Visible := .T.
   ::oGet2:Visible := ::ComboBox1:GetSelString() == FC_BETWEEN
   IF !::oGet2:Visible
      ::oGet1:Width := 160
      ::oGet2:Caption := ""
    ELSE
      ::oGet1:Width := 80
   ENDIF
   ::oGet1:SetFocus()
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS Conditions
   DATA aCond_C      EXPORTED INIT {}
   DATA aCond_N      EXPORTED INIT {}
   DATA aCond_D      EXPORTED INIT {}
   DATA aCond_L      EXPORTED INIT {}
   DATA aCond_M      EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
ENDCLASS


METHOD Init( oDataTable ) CLASS Conditions
   ::aCond_N := {  { FC_EQUALTO,          {|cField,cExp,cExp2| cField + "==" + cExp} },;
                   { FC_NOTEQUALTO,       {|cField,cExp,cExp2| "!(" + cField + "==" + cExp + ")" } },;
                   { FC_GREATEREQU,       {|cField,cExp,cExp2| cField + ">=" + cExp} },;
                   { FC_LESSEQUAL,        {|cField,cExp,cExp2| cField + "<=" + cExp} },;
                   { FC_BETWEEN,          {|cField,cExp,cExp2| "(" + cField + ">= " + cExp + ".AND." + cField +"<=" + cExp2 + ")"} },;
                   { FC_INTHERANGE,       {|cField,cExp,cExp2| cField + "="  + cExp } } }

   ::aCond_C := {  { FC_CONTAINS,         {|cField,cExp,cExp2| cExp + " $ " + cField} },;
                   { FC_NOTCONTAIN,       {|cField,cExp,cExp2| "!(" + cExp + " $ " + cField + ")"} },;
                   { FC_BEGWITH,          {|cField,cExp,cExp2| cField + "=" + cExp} },;
                   { FC_NOTBEGWITH,       {|cField,cExp,cExp2| cField + "!=" + cExp} },;
                   { FC_ISEMPTY,          {|cField,cExp,cExp2| "EMPTY(" + cField + ")"} },;
                   { FC_NOTEMPTY,         {|cField,cExp,cExp2| "! EMPTY(" + cField + ")"} },;
                   { FC_INTHERANGE,       {|cField,cExp,cExp2| cField + "="  + cExp} } }

   ::aCond_D := {  { FC_EQUALTO,          {|cField,cExp,cExp2| cField + "==" + cExp} },;
                   { FC_NOTEQUALTO,       {|cField,cExp,cExp2| cField + "<>" + cExp} },;
                   { FC_GREATEREQU,       {|cField,cExp,cExp2| cField + ">=" + cExp} },;
                   { FC_LESSEQUAL,        {|cField,cExp,cExp2| cField + "<=" + cExp} },;
                   { FC_BETWEEN,          {|cField,cExp,cExp2| "(" + cField + ">= " + cExp + ".AND." + cField +"<=" + cExp2 + ")"} },;
                   { FC_PERQUARTER,       {|cField,cExp,cExp2| cExp } },;
                   { FC_INLAST,           {|cField,cExp,cExp2| cField + ">=" + cExp } },;
                   { FC_NOTINLAST,        {|cField,cExp,cExp2| cField + "<"  + cExp } },;
                   { FC_INTHERANGE,       {|cField,cExp,cExp2| cField + "="  + cExp } } }

   ::aCond_L := {  { FC_TRUE,  {|cField,cExp,cExp2| cField} },;
                   { FC_FALSE, {|cField,cExp,cExp2| "!"+cField} } }

   ::aCond_M := ACLONE( ::aCond_C )
RETURN Self

FUNCTION BuildFilterExp( hFilter )
   LOCAL cType, cExp1, cExp2, nSel2, cValue, cField, cAndOr, hExp, n, cFilter := ""
   LOCAL cExpSel, bExp, nNum, aExp, oCond := Conditions( NIL )
   
   cAndOr := IIF( hFilter:ANDRadio == "1", " .AND. ", " .OR. " )
   
   IF HGetPos( hFilter, "Expressions" ) == 0 
      RETURN "" // returning NIL forces the report to cancel
   ENDIF
   
   FOR n := 1 TO LEN( hFilter:Expressions )
       hExp    := hFilter:Expressions[n]

       cField  := hExp:Field
       nSel2   := hExp:ExpSel
       cExp1   := '"'+hExp:Exp1+'"'
       cExp2   := '"'+hExp:Exp2+'"'
       cType   := hExp:FieldType
       cExpSel := oCond:aCond_&cType[nSel2][1]

       IF cType == "A"
          cField := ALLTRIM(hExp:Field)+"[1]"
          cType  := VALTYPE(cField)
       ENDIF
       cValue  := ""
       IF hExp:AndOr != NIL
          cAndOr := IIF( hExp:AndOr == 1, " .AND. ", " .OR. " )
        ELSE
          IF n > 1
             cFilter += cAndOr
          ENDIF

          DO CASE
             CASE cType == "N"
                  cExp1 := hExp:Exp1
                  cExp2 := hExp:Exp2

             CASE cType == "D" .AND. ! ( cExpSel == FC_INTHERANGE )
                  IF cExpSel IN {FC_INLAST, FC_NOTINLAST}
                     aExp  := hb_aTokens( cExp1, "/" )
                     cExp1 := "@TODAY-"
                     nNum  := VAL( aExp[1] )
                     IF aExp[2] == "days"
                        cExp1 += aExp[1]
                      ELSEIF aExp[2] == "weeks"
                        cExp1 += AllTrim( Str( nNum*7 ) )
                      ELSEIF aExp[2] == "months"
                        cExp1 += AllTrim( Str( nNum*30 ) )
                     ENDIF

                   ELSEIF cExpSel IN {FC_PERQUARTER}
                     aExp  := hb_aTokens( cExp1, "/" )
                     cExp1 := 'MONTH('+cField+')>='+aExp[2]+'.AND.MONTH('+cField+')<='+aExp[4]

                   ELSE
                     cExp1 := 'CTOD( "' + DTOC( STOD( hExp:Exp1 ) ) + '" )'
                     cExp2 := 'CTOD( "' + DTOC( STOD( hExp:Exp2 ) ) + '" )'
                  ENDIF
          ENDCASE

          IF HGetPos( hExp, "AskMeLater" ) > 0 .AND. hExp:AskMeLater != NIL
             cValue := AskLater( cField, cType, nSel2, hExp )
             IF cValue == NIL
                RETURN NIL
             ENDIF
           ELSE
             bExp := oCond:aCond_&cType[nSel2][2]
             cValue := EVAL( bExp, cField, cExp1, cExp2 )
          ENDIF
          cFilter += cValue
       ENDIF
       hFilter:Expressions[n]:Value := cValue
   NEXT
   cFilter := STRTRAN( cFilter, "@TODAY", 'CTOD("'+DTOC(DATE())+'")' )
RETURN cFilter

//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------

CLASS FilterPerQuarter INHERIT Dialog
   DATA oEdit EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()

   METHOD FilterPerQuarter_OnLoad()
   METHOD RadioButton1_OnClick()
   METHOD RadioButton2_OnClick()
   METHOD RadioButton3_OnClick()
   METHOD RadioButton4_OnClick()
   METHOD RadioButton5_OnClick()
   METHOD Button1_OnClick()
   METHOD EditBox1_OnVertScroll()
   METHOD EditBox1_OnChar()
ENDCLASS

METHOD Init( oParent, oEdit ) CLASS FilterPerQuarter
   ::oEdit := oEdit
   ::Super:Init( oParent )

   ::EventHandler[ "OnLoad" ]  := "FilterPerQuarter_OnLoad"

   ::Name            := "FilterPerQuarter"
   ::VertScrollSize  := 262
   ::HorzScrollSize  := 284
   ::Modal           := .T.
   ::Left            := 10
   ::Top             := 10
   ::Width           := 454
   ::Height          := 253
   ::Center          := .T.
   ::Caption         := "WinFakt! Per quarter"
   ::TopMost         := .T.
   ::MaximizeBox     := .F.
   ::MinimizeBox     := .F.
   ::Create()
RETURN Self

METHOD OnInitDialog() CLASS FilterPerQuarter
   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "GroupBox1"
      WITH OBJECT :Dock
         :Left                 := "FilterPerQuarter"
         :Top                  := "FilterPerQuarter"
         :Bottom               := "FilterPerQuarter"
         :Margins              := "20,15,0,70"
      END

      :Left                 := 20
      :Top                  := 15
      :Width                := 156
      :Height               := 129
      :Caption              := "Month"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( LABEL( :this ) )
         :Name                 := "Label1"
         :Left                 := 23
         :Top                  := 26
         :Width                := 50
         :Height               := 16
         :Caption              := "From:"
         :Rightalign           := .T.
         :Create()
      END //LABEL

      WITH OBJECT ( EDITBOX( :this ) )
         :Name                 := "EditBox1"
         :Caption              := "1"
         :Left                 := 76
         :Top                  := 22
         :Width                := 50
         :Height               := 22
         :Alignment            := 3
         :VertScroll           := .T.
         :EventHandler[ "OnVertScroll" ] := "EditBox1_OnVertScroll"
         :EventHandler[ "OnChar" ]       := "EditBox1_OnChar"
         :Number               := .T.
         :Create()
      END

      WITH OBJECT ( LABEL( :this ) )
         :Name                 := "Label2"
         :Left                 := 23
         :Top                  := 61
         :Width                := 50
         :Height               := 16
         :Caption              := "To:"
         :Rightalign           := .T.
         :Create()
      END //LABEL

      WITH OBJECT ( EDITBOX( :this ) )
         :Name                 := "EditBox2"
         :Caption              := "3"
         :Left                 := 76
         :Top                  := 57
         :Width                := 50
         :Height               := 22
         :Alignment            := 3
         :VertScroll           := .T.
         :EventHandler[ "OnVertScroll" ] := "EditBox1_OnVertScroll"
         :EventHandler[ "OnChar" ]       := "EditBox1_OnChar"
         :Number               := .T.
         :Create()
      END

   END

   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "GroupBox2"
      WITH OBJECT :Dock
         :Top                  := "FilterPerQuarter"
         :Right                := "FilterPerQuarter"
         :Bottom               := "FilterPerQuarter"
         :Margins              := "0,15,20,70"
      END

      :Left                 := 193
      :Top                  := 15
      :Width                := 223
      :Height               := 129
      :Caption              := "Quarter"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton1"
         :Left                 := 26
         :Top                  := 25
         :Width                := 80
         :Height               := 15
         :InitialState         := BST_CHECKED
         :Caption              := "I. First"
         :EventHandler[ "OnClick" ] := "RadioButton1_OnClick"
         :Create()
      END

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton2"
         :Left                 := 26
         :Top                  := 60
         :Width                := 80
         :Height               := 15
         :Caption              := "II. Second"
         :EventHandler[ "OnClick" ] := "RadioButton2_OnClick"
         :Create()
      END

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name          := "RadioButton3"
         :Left          := 124
         :Top           := 25
         :Width         := 80
         :Height        := 15
         :Caption       := "III. Third"
         :EventHandler[ "OnClick" ] := "RadioButton3_OnClick"
         :Create()
      END

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name          := "RadioButton4"
         :Left          := 124
         :Top           := 60
         :Width         := 80
         :Height        := 15
         :Caption       := "IV. Fourth"
         :EventHandler[ "OnClick" ] := "RadioButton4_OnClick"
         :Create()
      END

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name          := "RadioButton5"
         :Left          := 26
         :Top           := 93
         :Width         := 80
         :Height        := 15
         :Caption       := "All quarters"
         :EventHandler[ "OnClick" ] := "RadioButton5_OnClick"
         :Create()
      END
   END

   WITH OBJECT ( PICTUREBOX( Self ) )
      WITH OBJECT :Dock
         :Left          := Self
         :Right         := Self
         :Bottom        := Self
         :Margins       := "0,0,0,0"
      END
      :Left             := 0
      :Top              := 165
      :Width            := 437
      :Height           := 50
      :ImageName        := "_BOTTOMRIBBOJPG"
      :Stretch          := .T.
      :Create()
      WITH OBJECT ( BUTTON( :this ) )
         :Name                 := "Button1"
         WITH OBJECT :Dock
            :Right      := "PictureBox1"
            :Margins    := "0,0,12,0"
         END
         :Left          := 340
         :Top           := 12
         :Width         := 80
         :Height        := 25
         :Caption       := "Save"
         :DefaultButton := .T.
         :EventHandler[ "OnClick" ] := "Button1_OnClick"
         :Create()
      END
      WITH OBJECT ( BUTTON( :this ) )
         :Name          := "Button2"
         :Left          := 12
         :Top           := 12
         :Width         := 80
         :Height        := 25
         :Caption       := "Help"
         :Create()
      END
   END
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD EditBox1_OnChar( Sender ) CLASS FilterPerQuarter
RETURN 0

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD EditBox1_OnVertScroll( Sender ) CLASS FilterPerQuarter
   LOCAL nAdd := 1
   IF LoWord( Sender:wParam ) == 1 .AND. VAL( Sender:Caption ) > 1
      Sender:Caption := XSTR( VAL( Sender:Caption )-1 )
   ELSEIF LoWord( Sender:wParam ) <= 1 .AND. VAL( Sender:Caption ) < 12
      Sender:Caption := XSTR( VAL( Sender:Caption )+1 )
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD FilterPerQuarter_OnLoad( Sender ) CLASS FilterPerQuarter
   
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RadioButton1_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "1"
   ::EditBox2:Caption := "3"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RadioButton2_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "4"
   ::EditBox2:Caption := "6"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RadioButton3_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "7"
   ::EditBox2:Caption := "9"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RadioButton4_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "10"
   ::EditBox2:Caption := "12"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RadioButton5_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "1"
   ::EditBox2:Caption := "12"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD Button1_OnClick( Sender ) CLASS FilterPerQuarter
   LOCAL nMonFrom, nMonTo, mFromDt, mToDt, nYear, i, cTemp
   nMonFrom := VAL(::EditBox1:Caption)
   nMonTo   := VAL(::EditBox2:Caption)
   IF nMonFrom < 1 .OR. nMonFrom > 12 .OR. nMonTo > 12
      ::MessageBox( "Please enter valid month.", "Filter" )
      RETURN Self
   ENDIF
   IF nMonFrom > nMonTo
      ::MessageBox( "'From' month cannot be greater than 'To' month.", "Filter" )
      RETURN Self
   ENDIF
   ::oEdit:Caption := "From " + XSTR( nMonFrom ) + " To " + XSTR( nMonTo )
   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------

CLASS IsInTheLast INHERIT Dialog
   DATA Text     EXPORTED
   DATA aOptions EXPORTED
   DATA nNum     EXPORTED
   DATA cSel     EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OK_OnClick()
   METHOD Help_OnClick()
ENDCLASS

METHOD Init( oParent, cText, aOptions ) CLASS IsInTheLast
   Super:Init( oParent )

   ::Width      := 300
   ::Height     := 200
   ::Caption    := "VR Filter"
   ::Modal      := .T.
   ::Center     := .T.
   ::AutoClose  := .T.
   ::Text       := cText
   ::TopMost    := .T.
   ::aOptions   := aOptions
   ::Icon       := "AVR"
   ::MaximizeBox:= .F.
   ::MinimizeBox:= .F.
   ::Create()

RETURN Self

METHOD OnInitDialog() CLASS IsInTheLast
   WITH OBJECT ( PictureBox( Self ) )
      :Name      := "BottomRibbon"
      WITH OBJECT :Dock
         :Left   := Self
         :Right  := Self
         :Bottom := Self
      END
      :Left      := 0
      :Top       := 415
      :Width     := 844
      :Height    := 50
      :ImageName := "_BOTTOMRIBBOJPG"
      :Stretch   := .T.
      :Create()
      WITH OBJECT ( Button( :this ) )
         :Caption   := "Help"
         :ID        := IDOK
         :Left      := 10
         :Top       := 12
         :Width     := 80
         :Height    := 25
         :Action    := {||::Help_OnClick()}
         :Create()
      END
      WITH OBJECT ( Button( :this ) )
         :Caption   := "OK"
         :Left      := :Parent:Width - 85
         :Top       := 12
         :Width     := 80
         :Height    := 25
         :DefaultButton := .T.
         :Action    := {||::OK_OnClick()}
         :Create()
      END
   END
   
   WITH OBJECT ( GroupBox( Self ) )
      :Caption   := ::Text    
      :Left      := 15
      :Top       := 15
      :Width     := ::ClientWidth-30
      :Height    := 90
      :Create()
      WITH OBJECT ( EditBox( :this ) )
         :Caption   := "1"
         :Number    := .T.
         :Left      := 15
         :Top       := 40
         :Width     := 100
         :Alignment :=  3
         :Create()
      END
      WITH OBJECT ( ComboBox( :this ) )
         :Left      := 120
         :Top       := 40
         :Width     := 130
         :Create()
         AEVAL( ::aOptions, {|c| :AddItem(c) } )
         :SetCurSel(1)
      END
   END
   ::EditBox1:SetFocus()
RETURN 0

METHOD OK_OnClick() CLASS IsInTheLast
   ::nNum := ::EditBox1:Caption
   ::cSel := ::ComboBox1:GetSelString()
   ::Close( IDOK )
RETURN NIL

METHOD Help_OnClick() CLASS IsInTheLast
RETURN NIL

