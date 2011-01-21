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
   
   METHOD Init()       CONSTRUCTOR
   METHOD Create()
   METHOD SetPortrait()
   METHOD SetLandscape()
   METHOD Preview()
   METHOD End()
   METHOD StartPage()
   METHOD EndPage()
   METHOD Run()
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

METHOD Run( oDoc ) CLASS VrReport
   LOCAL oNode, hPointer, cData, oCurControl, n, cProp, oControl
   IF ::oPDF != NIL
      ::oPDF:Destroy()
   ENDIF
   ::Create()
   
   oNode := oDoc:FindFirstRegex( "Report" )
   WHILE oNode != NIL
      DO CASE
         CASE oNode:cName == "Body"
              ::nRow += ::HeaderHeight

         CASE oNode:oParent:cName == "Properties" .AND. oNode:oParent:oParent:cName == "Report"
              cData := oNode:cData
              IF cProp IN {"HeaderHeight", "FooterHeight" }
                 cData := VAL( cData )
                 ::&cProp := cData
              ENDIF

              // Load Report properties

         CASE oNode:oParent:cName == "Control" //.AND. oNode:oParent:oParent:cName == "Header"
              // Load Header controls

              cProp := oNode:cName
              IF cProp == "ClassName"
                 IF oCurControl != NIL
                    VIEW oCurControl:Text
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

         CASE oNode:oParent:cName == "Font" .AND. oNode:oParent:oParent:cName == "Control"
              cData := oNode:cData
              IF cProp IN {"PointSize", "Weight"}
                 cData := VAL( cData )
              ENDIF
              oCurControl:Font:&cProp := cData
              
      ENDCASE
      oNode := oDoc:Next()
   ENDDO
   IF oCurControl != NIL
      oCurControl:Create()
      oCurControl := NIL
   ENDIF
   ::Preview()
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
