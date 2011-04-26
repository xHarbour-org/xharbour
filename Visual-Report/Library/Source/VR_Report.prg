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

CLASS VrReport INHERIT VrObject
   DATA PrintHeader    EXPORTED  INIT .T.
   DATA PrintRepHeader EXPORTED  INIT .T.
   DATA PrintFooter    EXPORTED  INIT .T.
   DATA PrintRepFooter EXPORTED  INIT .T.

   DATA ClsName        EXPORTED  INIT "Report"
   DATA Name           EXPORTED  INIT "Report"

   DATA nImage         EXPORTED  INIT 0
   DATA nText          EXPORTED  INIT 0
   DATA nLine          EXPORTED  INIT 0
   DATA nBox           EXPORTED  INIT 0
   
   DATA FileName       EXPORTED  INIT "Preview"
   DATA Orientation    EXPORTED
   DATA PaperSize      EXPORTED  INIT DMPAPER_LETTER
   
   DATA LeftMargin     EXPORTED  INIT 1000
   DATA TopMargin      EXPORTED  INIT 1000
   DATA RightMargin    EXPORTED  INIT 1000
   DATA BottomMargin   EXPORTED  INIT 1000
   
   DATA oPDF           EXPORTED
   DATA PreviewCaption EXPORTED  INIT "Visual Report - Print Preview"
   DATA nPage          EXPORTED  INIT 0
   DATA nRow           EXPORTED  INIT 0
   DATA DataSource     EXPORTED
   DATA Button         EXPORTED
   DATA lUI            EXPORTED  INIT .F.

   DATA HeaderHeight   EXPORTED  INIT 0
   DATA FooterHeight   EXPORTED  INIT 0

   DATA aHeader        EXPORTED  INIT {}
   DATA aBody          EXPORTED  INIT {}
   DATA aFooter        EXPORTED  INIT {}
   DATA aExtraPage     EXPORTED  INIT {}
   DATA aData          EXPORTED
   DATA aProps         EXPORTED
   DATA aExtra         EXPORTED
   DATA aSubtotals     EXPORTED  INIT {}
   DATA aTotals        EXPORTED  INIT {}
   DATA aFormulas      EXPORTED  INIT {}

   ACCESS Application  INLINE __GetApplication()

   METHOD Init()       CONSTRUCTOR
   METHOD Create()
   METHOD Preview()
   METHOD End()
   METHOD StartPage()
   METHOD EndPage()
   METHOD Run()
   METHOD CreateControl()
   METHOD CreateBody()
   METHOD CreateHeader()
   METHOD CreateFooter()
   METHOD CreateExtraPage()
   METHOD CreateSubtotals()
   METHOD PrepareArrays()
   METHOD Load()
   METHOD InitPDF()
   METHOD Save() INLINE ::oPDF:Save( ::FileName + ".pdf", acFileSaveView )
   METHOD GetSubtotalHeight()
   METHOD GetTotalHeight()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init() CLASS VrReport
   ::aProperties := {}
   ::Orientation := __GetSystem():PageSetup:Portrait
   AADD( ::aProperties, { "Name",           "Object" } )
   AADD( ::aProperties, { "DataSource",     "Data"   } )
   AADD( ::aProperties, { "PrintHeader",    "Print"  } )
   AADD( ::aProperties, { "PrintRepHeader", "Print"  } )
   AADD( ::aProperties, { "PrintFooter",    "Print"  } )
   AADD( ::aProperties, { "PrintRepFooter", "Print"  } )

   ::InitPDF()
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD InitPDF() CLASS VrReport
   IF ::oPDF == NIL
      ::oPDF := ActiveX( ::Application:MainForm )
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
METHOD CreateControl( aCtrl, nHeight, oPanel, hDC, nVal ) CLASS VrReport
   LOCAL oControl, x := 0, y := 0, n
   IF ( x := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "LEFT"} ) ) > 0
      x := VAL( aCtrl[x][2] )
   ENDIF
   IF ( y := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TOP"} ) ) > 0
      y := VAL( aCtrl[y][2] )
   ENDIF
   IF oPanel == NIL
      oControl := hb_ExecFromArray( aCtrl[1][2], {,.F.} )
      oControl:Parent := Self
      oControl:Left := x
      oControl:Top  := y
    ELSE
      oControl := oPanel:CreateControl( aCtrl[1][2], x, y )
   ENDIF
   //------------------------------------------------------------------------------------------
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "NAME"} ) ) > 0
      oControl:Name := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FILENAME"} ) ) > 0
      oControl:FileName := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "ONLABEL"} ) ) > 0
      oControl:OnLabel := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TEXT"} ) ) > 0
      oControl:Text      := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FORMULA"} ) ) > 0
      oControl:Formula := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "VALUE"} ) ) > 0
      oControl:Value := aCtrl[n][2]
   ENDIF
   //------------------------------------------------------------------------------------------
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "AUTORESIZE"} ) ) > 0
      oControl:AutoResize := VAL( aCtrl[n][2] ) == 1
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "ALIGNMENT"} ) ) > 0
      oControl:Alignment := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "WIDTH"} ) ) > 0
      oControl:Width     := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "HEIGHT"} ) ) > 0
      oControl:Height    := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FORECOLOR"} ) ) > 0
      oControl:ForeColor := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "BACKCOLOR"} ) ) > 0
      oControl:BackColor := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FONT"} ) ) > 0
      DEFAULT oControl:Font TO Font()
      oControl:Font:FaceName  := aCtrl[n][2][1][2]
      oControl:Font:PointSize := VAL( aCtrl[n][2][2][2] )
      oControl:Font:Italic    := IIF( aCtrl[n][2][3][2]=="True", .T., .F. )
      oControl:Font:Underline := IIF( aCtrl[n][2][4][2]=="True", .T., .F. )
      oControl:Font:Weight    := VAL( aCtrl[n][2][5][2] )
   ENDIF
   IF ! Empty( nVal )
      oControl:Caption := ALLTRIM( STR( nVal ) )
   ENDIF
   IF oPanel == NIL
      oControl:Draw( hDC )
      TRY
         IF oControl:ClsName != "Image" .OR. ! oControl:OnePerPage
            nHeight := MAX( oControl:PDFCtrl:Attribute( 'Bottom' )-oControl:PDFCtrl:Attribute( 'Top' ), nHeight )
         ENDIF
      CATCH
      END
    ELSE
      oControl:Configure()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD GetTotalHeight( hDC ) CLASS VrReport
   LOCAL i, n, nPt, cClass, nHeight := 0, aCtrl, aBody := ACLONE( ::aBody )
   ::aTotals    := {}
   FOR EACH aCtrl IN aBody
       IF UPPER( aCtrl[1][2] ) == "VRTOTAL"
          IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "ONLABEL"} ) ) > 0
             AADD( ::aTotals, {aCtrl[n][2],0} )
          ENDIF
          IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FONT"} ) ) > 0
             nHeight := MAX( nHeight, VAL( aCtrl[n][2][2][2] ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY ) )
          ENDIF
       ENDIF
   NEXT
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD GetSubtotalHeight( hDC ) CLASS VrReport
   LOCAL i, n, nPt, cClass, nHeight := 0, aCtrl, aBody := ACLONE( ::aBody )
   ::aFormulas  := {}
   FOR EACH aCtrl IN aBody
       IF UPPER( aCtrl[1][2] ) == "VRSUBTOTAL"
          AADD( ::aSubtotals, aCtrl )
          IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FONT"} ) ) > 0
             nHeight := MAX( nHeight, VAL( aCtrl[n][2][2][2] ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY ) )
          ENDIF
       ELSEIF UPPER( aCtrl[1][2] ) == "VRFORMULA"
          AADD( ::aFormulas, {aCtrl[2][2],aCtrl[3][2],0} )
       ENDIF
   NEXT
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateSubtotals( hDC, cField ) CLASS VrReport
   LOCAL aSubtotal, cArray, x, y, i, n, nFormula, nSub, nHeight := 0, aCtrl, aBody := ACLONE( ::aBody ), aFormula, cText

   FOR EACH aCtrl IN aBody
//       IF nFormula > 0 .AND. ( n := ASCAN( aBody, {|a| a[2][2]==::aFormulas[nFormula][2]} ) > 0
          
//       ENDIF
       
/*

          IF ( i := ASCAN( aBody, {|a| UPPER(a[1][2])=="VR"+cField} ) ) > 0

             cText := aBody[i][4][2]


             IF nForm > 0 .AND. ( n := ASCAN( ::aFormulas, {|a| UPPER(a[1]) == UPPER(aBody[i][nForm][2]) } ) ) > 0
                aBody[i][4][2] := &(::aFormulas[n][2])
              ELSE
                aBody[i][4][2] := xStr(::a&cArray[nSub][2])
             ENDIF




             IF ( x := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "LEFT"} ) ) > 0
                x := aCtrl[x][2]
             ENDIF
             IF ( y := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TOP"} ) ) > 0
                y := aCtrl[y][2]
             ENDIF

             IF ( n := ASCAN( aBody[i], {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "LEFT"} ) ) > 0
                aBody[i][n][2] := x
             ENDIF
             IF ( n := ASCAN( aBody[i], {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TOP"} ) ) > 0
                aBody[i][n][2] := y
             ENDIF

             ::CreateControl( aBody[i], @nHeight,, hDC )
             ::a&cArray[nSub][2] := 0
             
             aBody[i][4][2] := cText
          ENDIF

       ENDIF
*/
   NEXT
   ::nRow += nHeight
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateBody( hDC ) CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   FOR EACH aCtrl IN ::aBody
       IF !( UPPER( aCtrl[1][2] ) IN { "VRSUBTOTAL", "VRTOTAL" } )
          ::CreateControl( aCtrl, @nHeight,, hDC )
       ENDIF
   NEXT
   ::nRow += nHeight
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateHeader( hDC ) CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   IF ::PrintHeader
      FOR EACH aCtrl IN ::aHeader
          ::CreateControl( aCtrl, @nHeight,, hDC )
      NEXT
      ::nRow := ::HeaderHeight
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateFooter( hDC ) CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   IF ::PrintFooter
      ::nRow := ::oPDF:PageLength - ::FooterHeight
      FOR EACH aCtrl IN ::aFooter
          ::CreateControl( aCtrl, @nHeight,, hDC )
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
   LOCAL oPrev, oNode, cData, n, aControl, cParent, hDC

   ::aData  := {=>}
   ::aProps := {=>}
   ::aExtra := {=>}
   HSetCaseMatch( ::aData, .F. )
   HSetCaseMatch( ::aProps, .F. )
   HSetCaseMatch( ::aExtra, .F. )

   oNode := oDoc:FindFirstRegEx( "Report" )

   WHILE oNode != NIL
      DO CASE
         CASE oNode:oParent:cName == "DataSource" .AND. oNode:oParent:oParent:cName == "Report"
              DEFAULT oNode:cData TO ""
              ::aData[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              DEFAULT oNode:cData TO ""
              ::aProps[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "ExtraPage" .AND. oNode:oParent:oParent:cName == "Report" .AND. oNode:cName != "Control"
              DEFAULT oNode:cData TO ""
              ::aExtra[ oNode:cName ] := oNode:cData

         CASE oNode:cName == "Control" 
              IF !EMPTY( aControl )
                 AADD( ::&cParent, aControl )
              ENDIF
              cParent := "a" + oNode:oParent:cName
              aControl := {}

         CASE oNode:cName == "Font" 
              AADD( aControl, { oNode:cName, {} } )

         CASE oNode:oParent:cName == "Control"
              DEFAULT oNode:cData TO ""
              AADD( aControl, { oNode:cName, oNode:cData } )

         CASE oNode:oParent:cName == "Font"
              DEFAULT oNode:cData TO ""
              AADD( aTail(aControl)[2], { oNode:cName, oNode:cData } )
      ENDCASE
      oNode := oDoc:Next()
   ENDDO
   IF !EMPTY( aControl )
      AADD( ::&cParent, aControl )
   ENDIF
   n := ::Application:Props[ "Header" ]:Height - ::Application:Props[ "Header" ]:ClientHeight
        ::Application:Props[ "Header" ]:Height := VAL( ::aProps:HeaderHeight )+n
   n := ::Application:Props[ "Footer" ]:Height - ::Application:Props[ "Footer" ]:ClientHeight
        ::Application:Props[ "Footer" ]:Height := VAL( ::aProps:FooterHeight )+n
        ::Application:Props[ "Footer" ]:Dockit()
        ::Application:Props[ "Body" ]:Dockit()
   TRY
      ::Orientation  := VAL( ::aProps:Orientation )
   CATCH
   END
   TRY
      ::PaperSize   := VAL( ::aProps:PaperSize )

      ::LeftMargin  := VAL( ::aProps:LeftMargin )
      ::TopMargin   := VAL( ::aProps:TopMargin )
      ::RightMargin := VAL( ::aProps:RightMargin )
      ::BottomMargin:= VAL( ::aProps:BottomMargin )
   CATCH
   END

   hDC := GetDC(0)
   ::HeaderHeight := VAL( ::aProps:HeaderHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
   ::FooterHeight := VAL( ::aProps:FooterHeight ) * PIX_PER_INCH / GetDeviceCaps( hDC, LOGPIXELSY )
   ReleaseDC(0, hDC)
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Load( cReport ) CLASS VrReport
   LOCAL aCtrl, oDoc := TXmlDocument():New( cReport )

   ::PrepareArrays( oDoc )
   
   IF !EMPTY( ::aData ) .AND. !EMPTY( ::aData:FileName )
      WITH OBJECT ::DataSource := ::Application:Props[ "Body" ]:CreateControl( ::aData:ClsName )
         :FileName := ::aData:FileName
         :Alias    := ::aData:Alias
         :bFilter  := ::aData:bFilter
      END
      ::Application:Props:PropEditor:ResetProperties( {{ ::DataSource }} )
   ENDIF
   ::PrintHeader    := ::aProps:PrintHeader == "1"
   ::PrintRepHeader := ::aProps:PrintRepHeader == "1"
   ::PrintFooter    := ::aProps:PrintFooter == "1"
   ::PrintRepFooter := ::aProps:PrintRepFooter == "1"

   FOR EACH aCtrl IN ::aHeader
       ::CreateControl( aCtrl,, ::Application:Props[ "Header" ] )
   NEXT
   FOR EACH aCtrl IN ::aBody
       ::CreateControl( aCtrl,, ::Application:Props[ "Body" ] )
   NEXT
   FOR EACH aCtrl IN ::aFooter
       ::CreateControl( aCtrl,, ::Application:Props[ "Footer" ] )
   NEXT
   TRY
      ::Application:Props:ExtraPage:PagePosition := VAL( ::aExtra:PagePosition )
   CATCH
   END
   FOR EACH aCtrl IN ::aExtraPage
       ::CreateControl( aCtrl,, ::Application:Props[ "ExtraPage" ] )
   NEXT
RETURN oDoc

//-----------------------------------------------------------------------------------------------
METHOD Run( oDoc, oWait ) CLASS VrReport
   LOCAL nHeight, hDC, nSubHeight, nTotHeight, nCount, nPer, nPos

   ::Create()

   IF oDoc != NIL
      ::PrepareArrays( oDoc )
   ENDIF

   ::PrintHeader    := ::aProps:PrintHeader == "1"
   ::PrintRepHeader := ::aProps:PrintRepHeader == "1"
   ::PrintFooter    := ::aProps:PrintFooter == "1"
   ::PrintRepFooter := ::aProps:PrintRepFooter == "1"

   ::StartPage()
   hDC := GetDC(0)

   IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == -1
      ::CreateExtraPage( hDC )
      ::EndPage()
      ::StartPage()
   ENDIF
   
   IF !EMPTY( ::aData ) .AND. !EMPTY( ::aData:FileName )
      ::DataSource := hb_ExecFromArray( ::aData:ClsName )
      ::DataSource:FileName := ::aData:FileName
      ::DataSource:Alias    := ::aData:Alias
      ::DataSource:bFilter  := ::aData:bFilter
      ::DataSource:Create()
      nCount := ::DataSource:EditCtrl:RecCount()
   ENDIF

   ::CreateHeader( hDC )
   
   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      ::DataSource:EditCtrl:Select()
      ::DataSource:EditCtrl:GoTop()
      AEVAL( ::aSubtotals, {|a| a[2] := 0} )
      AEVAL( ::aTotals,    {|a| a[2] := 0} )
      
      nSubHeight := ::GetSubtotalHeight( hDC )
      nTotHeight := ::GetTotalHeight( hDC )
      nPos := 0
      WHILE ! ::DataSource:EditCtrl:Eof()
         nHeight := ::CreateBody( hDC )
         IF ::nRow >= ( ::oPDF:PageLength - IIF( ::PrintFooter, ::FooterHeight, 0 ) - nHeight - nSubHeight )
            ::CreateSubtotals( hDC )
            IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == 0
               ::CreateExtraPage( hDC )
            ENDIF
            ::CreateFooter( hDC )
            ::EndPage()
            ::StartPage()
            ::CreateHeader( hDC )
         ENDIF
         oWait:Position := Int( (nPos/nCount)*100 )
         nPos ++
         ::DataSource:EditCtrl:Skip()
      ENDDO
      ::CreateSubtotals( hDC )
      IF ::nRow >= ( ::oPDF:PageLength - IIF( ::PrintFooter, ::FooterHeight, 0 ) - nHeight - nTotHeight )
         ::CreateFooter( hDC )
         ::EndPage()
         ::StartPage()
         ::CreateHeader( hDC )
      ENDIF
      //::CreateTotals( hDC )
    ELSE
      ::CreateBody( hDC )
   ENDIF
   ::CreateFooter( hDC )
   IF ::Application:Props:ExtraPage:PagePosition != NIL .AND. ::Application:Props:ExtraPage:PagePosition == 0
      ::CreateExtraPage( hDC )
   ENDIF

   ReleaseDC(0, hDC)

   ::EndPage()
   ::End()
   hb_gcall(.t.)
   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      IF ::DataSource:EditCtrl:IsOpen
         ::DataSource:EditCtrl:Close()
      ENDIF
   ENDIF
RETURN Self

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

