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

#define  acFileSaveDefault     -1
#define  acFileSaveAll          0
#define  acFileSaveView         1
#define  acFileSaveDesign       2

#define  acScaleVertical        2
#define  acCommandToolPageHome  53773

CLASS VrReport INHERIT VrObject
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

   DATA aProps         EXPORTED  INIT {}
   DATA aHeader        EXPORTED  INIT {}
   DATA aBody          EXPORTED  INIT {}
   DATA aFooter        EXPORTED  INIT {}

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
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init() CLASS VrReport
   ::aProperties := {}
   AADD( ::aProperties, { "Name",       "Object"  } )
   AADD( ::aProperties, { "DataSource", "Data"  } )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS VrReport
   ::oPDF := ActiveX( __GetApplication():MainForm )
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

   ::oPDF:ObjectAttributeSTR( "Document", "DefaultFont", "Courier New,12,400,0,0" )

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
   VrPreview( Self )
RETURN Self

METHOD CreateControl( aCtrl ) CLASS VrReport
   LOCAL hPointer, oControl
   hPointer := HB_FuncPtr( aCtrl[1][2] )
   IF hPointer != NIL
      oControl := HB_Exec( hPointer,, , .F. )
      oControl:Parent    := Self
      oControl:Text      := aCtrl[2][2]
      oControl:ForeColor := VAL( aCtrl[3][2] )
      oControl:BackColor := VAL( aCtrl[4][2] )
      oControl:Left      := VAL( aCtrl[5][2] )
      oControl:Top       := VAL( aCtrl[6][2] )
      oControl:Font:FaceName  := aCtrl[7][2][1][2]
      oControl:Font:PointSize := VAL( aCtrl[7][2][2][2] )
      oControl:Font:Italic    := IIF( aCtrl[7][2][3][2]=="True", .T., .F. )
      oControl:Font:Underline := IIF( aCtrl[7][2][4][2]=="True", .T., .F. )
      oControl:Font:Weight    := VAL( aCtrl[7][2][5][2] )
      oControl:Create()
   ENDIF
RETURN Self

METHOD CreateBody( oNode ) CLASS VrReport
   LOCAL aCtrl
   ::nRow := ::HeaderHeight
   FOR EACH aCtrl IN ::aBody
       ::CreateControl( aCtrl )
   NEXT
RETURN Self

METHOD CreateHeader() CLASS VrReport
   LOCAL aCtrl
   ::nRow := 0
   FOR EACH aCtrl IN ::aHeader
       ::CreateControl( aCtrl )
   NEXT
RETURN Self

METHOD CreateFooter() CLASS VrReport
   LOCAL aCtrl
   ::nRow := 0
   FOR EACH aCtrl IN ::aFooter
       ::CreateControl( aCtrl )
   NEXT
RETURN Self

METHOD PrepareArrays( oDoc ) CLASS VrReport
   LOCAL aKeys, aFont, oPrev, oNode, hPointer, cData, oCurControl, n, oControl, aControl, cParent
   oNode := oDoc:FindFirstRegEx( "Report" )
   WHILE oNode != NIL
      DO CASE
         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              IF oNode:cName IN {"HeaderHeight", "FooterHeight" }
                 AADD( ::aProps, { oNode:cName, VAL( oNode:cData ) } )
              ENDIF

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
RETURN Self

METHOD Run( oDoc ) CLASS VrReport
   LOCAL aKeys, aFont, oNode, hPointer, cData, oCurControl, n, cProp, oControl, aControl, cParent
   IF ::oPDF != NIL
      ::oPDF:Destroy()
   ENDIF
   ::Create()
   ::nRow := 0

   ::PrepareArrays( oDoc )

   ::CreateHeader()
   ::CreateBody()
   ::CreateFooter()
//              ::nRow += ::HeaderHeight
/*              
              IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
                 ::DataSource:Create()
                 ::DataSource:EditCtrl:Select()
                 WHILE ! oRep:DataSource:EditCtrl:Eof()
                    ::PrintBody( oNode )
                    IF ::nRow >= ( ::oPDF:PageLength - ::FooterHeight )
                       ::PrintFooter( oNode )
                       ::EndPage()
                       ::StartPage()
                       ::PrintHeader( oNode )
                    ENDIF
                    ::DataSource:EditCtrl:Skip()
                 ENDDO
              ENDIF
*/
/*
              cProp := oNode:cName
              IF cProp == "ClassName"
                 IF oCurControl != NIL
                    oCurControl:Create()
                    oCurControl := NIL
                 ENDIF

                 hPointer := HB_FuncPtr( oNode:cData )
                 IF hPointer != NIL
                    oCurControl := HB_Exec( hPointer,, , .F. )
                    oCurControl:Parent := Self
                 ENDIF
               ELSEIF cProp != "Font"
                 cData := oNode:cData
                 IF cProp IN {"Left", "Top", "Width", "Height", "ForeColor", "BackColor" }
                    cData := VAL( cData )
                 ENDIF
                 __objSendMsg( oCurControl, "_"+cProp, cData )
              ENDIF
*/                            
//         CASE oNode:oParent:cName == "Font" .AND. oNode:oParent:oParent:cName == "Control"
//              cData := oNode:cData
//              IF cProp IN {"PointSize", "Weight"}
//                 cData := VAL( cData )
//              ENDIF
//              oCurControl:Font:&cProp := cData
              
   ::EndPage()
   ::End()
//   IF ::DataSource != NIL .AND. ! EMPTY( ::DataSource:FileName )
//      IF ::DataSource:EditCtrl:IsOpen
//         ::DataSource:EditCtrl:Close()
//      ENDIF
//   ENDIF
   ::Preview()
RETURN Self
/*
//---------------------------------------------------------------------------------------------------------   
METHOD PrintHeader() CLASS VrReport
   local nHeight := 0
   aCtrl := ::hReport:Header:Objects
   FOR n := 1 TO LEN( aCtrl )
       IF aCtrl[n]:lUI
          ::Generate( aCtrl[n], @cBuffer )
       ENDIF
   NEXT
   cBuffer += "   oRep:nRow += nHeight"+ CRLF
   cBuffer += "RETURN NIL" + CRLF + CRLF

METHOD RunReport()
   IF ::hReport:DataSource != NIL .AND. ! EMPTY( ::hReport:DataSource:FileName )
      WITH OBJECT ::DataSource := ::hReport:DataSource:ClassName( NIL )
         :FileName := ::hReport:DataSource:FileName
         :Alias    := ::hReport:DataSource:Alias
         :bFilter  := ::hReport:DataSource:bFilter
         :Create()
      END
   ENDIF
   ::HeaderHeight := ::hReport:HeaderHeight * 20
   ::FooterHeight := ::hReport:FooterHeight * 20
   ::StartPage()

   ::PrintHeader()

   cBuffer += "   // Generate body area"                                                     + CRLF
   cBuffer += "   IF oRep:DataSource != NIL"                                                 + CRLF
   cBuffer += "      oRep:DataSource:EditCtrl:Select()"                                      + CRLF
   cBuffer += "   ENDIF"                                                                     + CRLF
   cBuffer += "   oRep:nRow := oRep:HeaderHeight"                                            + CRLF
   cBuffer += "   WHILE .T."                                                                 + CRLF
   cBuffer += "      " + ::GetName(,.F.) + "_PrintBody( hDC )"                               + CRLF 
   cBuffer += "      IF oRep:nRow >= ( oRep:oPDF:PageLength - oRep:FooterHeight )"           + CRLF
   cBuffer += "         " + ::GetName(,.F.) + "_PrintFooter( hDC )"                          + CRLF
   cBuffer += "         oRep:EndPage()"                                                      + CRLF
   cBuffer += "         oRep:StartPage()"                                                    + CRLF
   cBuffer += "         " + ::GetName(,.F.) + "_PrintHeader( hDC )"                          + CRLF
   cBuffer += "      ENDIF"                                                                  + CRLF
   cBuffer += "      IF oRep:DataSource == NIL .OR. oRep:DataSource:EditCtrl:Eof()"          + CRLF
   cBuffer += "         EXIT"                                                                + CRLF
   cBuffer += "       ELSEIF oRep:DataSource != NIL"                                         + CRLF
   cBuffer += "         oRep:DataSource:EditCtrl:Skip()"                                     + CRLF
   cBuffer += "      ENDIF"                                                                  + CRLF
   cBuffer += "   ENDDO"                                                                     + CRLF
//---------------------------------------------------------------------------------------------------------   

   cBuffer += "   oRep:End()"                                                                + CRLF
   cBuffer += "   IF oRep:DataSource != NIL"                                                 + CRLF
   cBuffer += "      oRep:DataSource:EditCtrl:Close()"                                       + CRLF
   cBuffer += "   ENDIF"                                                                     + CRLF
   //cBuffer += "   ReleaseDC(0, hDC)"                                                         + CRLF 
   cBuffer += "RETURN NIL" + CRLF + CRLF


//---------------------------------------------------------------------------------------------------------   
   cBuffer += "STATIC FUNCTION " + ::GetName(,.F.) + "_PrintFooter( hDC )" + CRLF
   cBuffer += "   local nHeight := 0" + CRLF
   aCtrl := oApp:Props:Footer:Objects
   FOR n := 1 TO LEN( aCtrl )
       IF aCtrl[n]:lUI
          ::Generate( aCtrl[n], @cBuffer )
       ENDIF
   NEXT
   cBuffer += "   oRep:nRow += nHeight" + CRLF
   cBuffer += "RETURN NIL" + CRLF + CRLF

//---------------------------------------------------------------------------------------------------------   

   cBuffer += "STATIC FUNCTION " + ::GetName(,.F.) + "_PrintBody( hDC )" + CRLF
   cBuffer += "   local nHeight := 0" + CRLF
   aCtrl := oApp:Props:Body:Objects
   FOR n := 1 TO LEN( aCtrl )
       IF aCtrl[n]:lUI
          ::Generate( aCtrl[n], @cBuffer )
       ENDIF
   NEXT
   cBuffer += "   oRep:nRow += nHeight" + CRLF
   cBuffer += "RETURN oCtrl" + CRLF
*/


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
   ::Create()
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
RETURN NIL



FUNCTION DecToHexa(nNumber)
   local cNewString:=''
   local nTemp:=0
   WHILE(nNumber > 0)
      nTemp:=(nNumber%16)
      cNewString:=SubStr('0123456789ABCDEF',(nTemp+1),1)+cNewString
      nNumber:=Int((nNumber-nTemp)/16)
   ENDDO
RETURN(cNewString)
