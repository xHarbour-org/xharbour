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
#define  acCommandToolPageHome  53773

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
   DATA nPage          PROTECTED INIT 0
   DATA nRow           EXPORTED  INIT 0
   DATA HeaderHeight   EXPORTED  INIT 0
   DATA FooterHeight   EXPORTED  INIT 0
   DATA DataSource     EXPORTED
   DATA Button         EXPORTED
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
   METHOD SetPortrait()
   METHOD SetLandscape()
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
   ::oPDF:ObjectAttribute( "Pages[1]", "PaperSize", ::PaperSize )
   ::oPDF:ObjectAttribute( "Pages[1]", "Landscape", ::LandScape )
   
   FERASE( ::FileName + ".pdf" )
   ::oPDF:StartSave( ::FileName + ".pdf", acFileSaveView )
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
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD EndPage() CLASS VrReport
   ::oPDF:SavePage( ::oPDF:CurrentPage )
   ::oPDF:ClearPage( ::oPDF:CurrentPage )
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD SetLandscape() CLASS VrReport
   LOCAL nPage
   ::LandScape := .T.
   FOR nPage := 1 TO ::oPDF:PageCount
      ::oPDF:ObjectAttribute( "Pages["+Alltrim(Str(nPage))+"]", "Landscape", ::LandScape )
   NEXT
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetPortrait() CLASS VrReport
   LOCAL nPage
   ::LandScape := .F.
   FOR nPage := 1 TO ::oPDF:PageCount
      ::oPDF:ObjectAttribute( "Pages["+Alltrim(Str(nPage))+"]", "Landscape", ::LandScape )
   NEXT
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Preview() CLASS VrReport
   LOCAL oPv := VrPreview( Self )
   oPv:Create()
   ::oPDF:Destroy()
RETURN Self

METHOD CreateControl( aCtrl, nHeight, oPanel ) CLASS VrReport
   LOCAL hPointer, oControl
   IF oPanel == NIL
      hPointer := HB_FuncPtr( aCtrl[1][2] )
      IF hPointer != NIL
         oControl := HB_Exec( hPointer,, , .F. )
         oControl:Parent := Self
      ENDIF
      oControl:Left := VAL( aCtrl[6][2] )
      oControl:Top  := VAL( aCtrl[7][2] )
    ELSE
      oControl := oPanel:CreateControl( aCtrl[1][2], VAL( aCtrl[6][2] ), VAL( aCtrl[7][2] ) )
   ENDIF

   oControl:Text      := aCtrl[3][2]
   oControl:ForeColor := VAL( aCtrl[4][2] )
   oControl:BackColor := VAL( aCtrl[5][2] )
   oControl:Font:FaceName  := aCtrl[8][2][1][2]
   oControl:Font:PointSize := VAL( aCtrl[8][2][2][2] )
   oControl:Font:Italic    := IIF( aCtrl[8][2][3][2]=="True", .T., .F. )
   oControl:Font:Underline := IIF( aCtrl[8][2][4][2]=="True", .T., .F. )
   oControl:Font:Weight    := VAL( aCtrl[8][2][5][2] )

   IF oPanel == NIL
      oControl:Create()
      nHeight := MAX( oControl:PDFCtrl:Attribute( 'Bottom' )-oControl:PDFCtrl:Attribute( 'Top' ), nHeight )
    ELSE
      oControl:Configure()
   ENDIF

RETURN Self

METHOD CreateBody() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   FOR EACH aCtrl IN ::aBody
       ::CreateControl( aCtrl, @nHeight )
   NEXT
   ::nRow += nHeight
RETURN nHeight

METHOD CreateHeader() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   FOR EACH aCtrl IN ::aHeader
       ::CreateControl( aCtrl, @nHeight )
   NEXT
   ::nRow := ::HeaderHeight
RETURN Self

METHOD CreateFooter() CLASS VrReport
   LOCAL aCtrl, nHeight := 0
   ::nRow := ::oPDF:PageLength - ( ::FooterHeight )
   FOR EACH aCtrl IN ::aFooter
       ::CreateControl( aCtrl, @nHeight )
   NEXT
RETURN Self

METHOD PrepareArrays( oDoc ) CLASS VrReport
   LOCAL aFont, oPrev, oNode, hPointer, cData, n, aControl, cParent

   ::aData  := {=>}
   ::aProps := {=>}
   HSetCaseMatch( ::aData, .F. )
   HSetCaseMatch( ::aProps, .F. )

   oNode := oDoc:FindFirstRegEx( "Report" )

   WHILE oNode != NIL
      DO CASE
         CASE oNode:oParent:cName == "DataSource" .AND. oNode:oParent:oParent:cName == "Report"
              ::aData[ oNode:cName ] := oNode:cData

         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              ::aProps[ oNode:cName ] := oNode:cData

         CASE oNode:cName == "Control" 
              aControl := {}
              cParent := "a" + oNode:oParent:cName
              oNode := oDoc:Next()
              WHILE oNode:oParent:cName == "Control"
                 IF oNode:cName != "Font"
                    AADD( aControl, { oNode:cName, oNode:cData } )
                    oNode := oDoc:Next()
                  ELSE
                    AADD( aControl, { oNode:cName, {} } )
                    aFont := {}
                    FOR n := 1 TO 5
                        oNode := oDoc:Next()
                        AADD( aTail(aControl)[2], { oNode:cName, oNode:cData } )
                    NEXT
                 ENDIF
              ENDDO
              AADD( ::&cParent, aControl )
      ENDCASE
      oNode := oDoc:Next()
   ENDDO

   ::HeaderHeight := VAL( ::aProps:HeaderHeight ) * ( PIX_PER_INCH / 72 )
   ::FooterHeight := VAL( ::aProps:FooterHeight ) * ( PIX_PER_INCH / 72 )
RETURN Self

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

RETURN Self

METHOD Run( oDoc ) CLASS VrReport
   LOCAL hPointer, oNode, nHeight
   
   ::Create()
   
   IF oDoc != NIL
      ::PrepareArrays( oDoc )
   ENDIF

   ::StartPage()

   IF !EMPTY( ::aData ) .AND. !EMPTY( ::aData:FileName )
      hPointer := HB_FuncPtr( ::aData:ClsName )
      IF hPointer != NIL
         WITH OBJECT ::DataSource := HB_Exec( hPointer )
            :FileName := ::aData:FileName
            :Alias    := ::aData:Alias
            :bFilter  := ::aData:bFilter
            :Create()
         END
      ENDIF
   ENDIF

   ::CreateHeader()

   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
      ::DataSource:EditCtrl:Select()
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
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
//   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::DlgModalFrame := .T.
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS VrPreview
   LOCAL oItem, oSub, nZoom
   ::Caption := ::Report:PreviewCaption
   
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
      :Dock:Top    := Self
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



FUNCTION DecToHexa(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%16)
      cNewString:=SubStr('0123456789ABCDEF',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/16)
   ENDDO
RETURN(cNewString)
