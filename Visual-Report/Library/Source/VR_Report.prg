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
   DATA nImage         EXPORTED  INIT 0
   DATA nText          EXPORTED  INIT 0
   DATA nLine          EXPORTED  INIT 0
   DATA nBox           EXPORTED  INIT 0
   
   DATA FileName       EXPORTED  INIT "Preview"
   DATA LandScape      EXPORTED  INIT .F.
   DATA PaperSize      EXPORTED  INIT 1
   DATA oPDF           EXPORTED
   DATA PreviewCaption EXPORTED  INIT "Visual Report - Print Preview"
   DATA nPage          EXPORTED  INIT 0
   DATA nRow           EXPORTED  INIT 0
   DATA HeaderHeight   EXPORTED  INIT 0
   DATA FooterHeight   EXPORTED  INIT 0
   DATA DataSource     EXPORTED
   DATA Button         EXPORTED
   DATA ClsName        EXPORTED  INIT "Report"
   DATA Name           EXPORTED  INIT "Report"
   DATA lUI            EXPORTED  INIT .F.
   DATA hReport        EXPORTED  INIT {=>}

   DATA aHeader        EXPORTED  INIT {}
   DATA aBody          EXPORTED  INIT {}
   DATA aFooter        EXPORTED  INIT {}
   DATA aData          EXPORTED
   DATA aProps         EXPORTED

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
   METHOD PrepareArrays()
   METHOD Load()
   METHOD Save() INLINE ::oPDF:Save( ::FileName + ".pdf", acFileSaveView )
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init() CLASS VrReport
   ::aProperties := {}
   AADD( ::aProperties, { "Name",       "Object"  } )
   AADD( ::aProperties, { "DataSource", "Data"  } )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS VrReport
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

   ::nPage := 0
   ::oPDF:SetLicenseKey( "WinFakt", "07EFCDAB010001008C5BD0102426F725C273B3A7C1B30B61521A8890359D83AE6FD68732DDAE4AC7E85003CDB8ED4F70678BF1EDF05F" )

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
   ::oPDF:ObjectAttribute( "Pages["+ALLTRIM(STR(::nPage))+"]", "Landscape", ::LandScape )
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
METHOD CreateControl( aCtrl, nHeight, oPanel ) CLASS VrReport
   LOCAL oControl, x := 0, y := 0, n
   IF ( x := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "LEFT"} ) ) > 0
      x := VAL( aCtrl[x][2] )
   ENDIF
   IF ( y := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TOP"} ) ) > 0
      y := VAL( aCtrl[y][2] )
   ENDIF
   IF oPanel == NIL
      //IF ::nRow + ( ( PIX_PER_INCH / 72 ) * y ) + nHeight > ( ::oPDF:PageLength - ::FooterHeight )
      //   RETURN NIL
      //ENDIF
      oControl := hb_ExecFromArray( aCtrl[1][2], {,.F.} )
      oControl:Parent := Self
      oControl:Left := x
      oControl:Top  := y
    ELSE
      oControl := oPanel:CreateControl( aCtrl[1][2], x, y )
   ENDIF

   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "WIDTH"} ) ) > 0
      oControl:Width  := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "HEIGHT"} ) ) > 0
      oControl:Height  := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "TEXT"} ) ) > 0
      oControl:Text   := aCtrl[n][2]
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FORECOLOR"} ) ) > 0
      oControl:ForeColor := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "BACKCOLOR"} ) ) > 0
      oControl:BackColor := VAL( aCtrl[n][2] )
   ENDIF
   IF ( n := ASCAN( aCtrl, {|a| Valtype(a[1])=="C" .AND. Upper(a[1]) == "FONT"} ) ) > 0
      oControl:Font:FaceName  := aCtrl[n][2][1][2]
      oControl:Font:PointSize := VAL( aCtrl[n][2][2][2] )
      oControl:Font:Italic    := IIF( aCtrl[n][2][3][2]=="True", .T., .F. )
      oControl:Font:Underline := IIF( aCtrl[n][2][4][2]=="True", .T., .F. )
      oControl:Font:Weight    := VAL( aCtrl[n][2][5][2] )
   ENDIF
   
   IF oPanel == NIL
      oControl:Draw()
      TRY
         nHeight := MAX( oControl:PDFCtrl:Attribute( 'Bottom' )-oControl:PDFCtrl:Attribute( 'Top' ), nHeight )
      CATCH
      END
    ELSE
      oControl:Configure()
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateBody() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   FOR EACH aCtrl IN ::aBody
       ::CreateControl( aCtrl, @nHeight )
   NEXT
   ::nRow += nHeight
RETURN nHeight

//-----------------------------------------------------------------------------------------------
METHOD CreateHeader() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   FOR EACH aCtrl IN ::aHeader
       ::CreateControl( aCtrl, @nHeight )
   NEXT
   ::nRow := ::HeaderHeight - 450
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD CreateFooter() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   ::nRow := ::oPDF:PageLength - ::FooterHeight
   FOR EACH aCtrl IN ::aFooter
       ::CreateControl( aCtrl, @nHeight )
   NEXT
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD PrepareArrays( oDoc ) CLASS VrReport
   LOCAL oPrev, oNode, cData, n, aControl, cParent

   ::aData  := {=>}
   ::aProps := {=>}
   HSetCaseMatch( ::aData, .F. )
   HSetCaseMatch( ::aProps, .F. )

   oNode := oDoc:FindFirstRegEx( "Report" )

   WHILE oNode != NIL
      DO CASE
         CASE oNode:oParent:cName == "DataSource" .AND. oNode:oParent:oParent:cName == "Report"
              DEFAULT oNode:cData TO ""
              ::aData[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              DEFAULT oNode:cData TO ""
              ::aProps[ oNode:cName ] := oNode:cData

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
   ::Application:Props[ "Header" ]:Height := VAL( ::aProps:HeaderHeight )
   ::Application:Props[ "Footer" ]:Height := VAL( ::aProps:FooterHeight )
   ::Application:Props[ "Footer" ]:Dockit()
   ::Application:Props[ "Body" ]:Dockit()
   ::HeaderHeight := VAL( ::aProps:HeaderHeight ) * ( PIX_PER_INCH / 72 )
   ::FooterHeight := VAL( ::aProps:FooterHeight ) * ( PIX_PER_INCH / 72 )
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

   FOR EACH aCtrl IN ::aHeader
       ::CreateControl( aCtrl,, ::Application:Props[ "Header" ] )
   NEXT
   FOR EACH aCtrl IN ::aBody
       ::CreateControl( aCtrl,, ::Application:Props[ "Body" ] )
   NEXT
   FOR EACH aCtrl IN ::aFooter
       ::CreateControl( aCtrl,, ::Application:Props[ "Footer" ] )
   NEXT
RETURN oDoc

//-----------------------------------------------------------------------------------------------
METHOD Run( oDoc ) CLASS VrReport
   LOCAL nHeight

   ::Create()

   IF oDoc != NIL
      ::PrepareArrays( oDoc )
   ENDIF

   ::StartPage()

   IF !EMPTY( ::aData ) .AND. !EMPTY( ::aData:FileName )
      ::DataSource := hb_ExecFromArray( ::aData:ClsName )
      ::DataSource:FileName := ::aData:FileName
      ::DataSource:Alias    := ::aData:Alias
      ::DataSource:bFilter  := ::aData:bFilter
      ::DataSource:Create()
   ENDIF

   ::CreateHeader()

   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      ::DataSource:EditCtrl:Select()
      ::DataSource:EditCtrl:GoTop()
      WHILE ! ::DataSource:EditCtrl:Eof()
         nHeight := ::CreateBody()
         IF ::nRow >= ( ::oPDF:PageLength - ::FooterHeight - nHeight )
            ::CreateFooter()
            ::EndPage()
            ::StartPage()
            ::CreateHeader()
         ENDIF
         ::DataSource:EditCtrl:Skip()
      ENDDO
    ELSE
      ::CreateBody()
   ENDIF

   ::CreateFooter()
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

