/*
 * $Id$
 */

//-----------------------------------------------------------------------------------------------
// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!
//-----------------------------------------------------------------------------------------------

#include "vxh.ch"

//-----------------------------------------------------------------------------------------------

#define  acFileSaveDefault     -1
#define  acFileSaveAll          0
#define  acFileSaveView         1
#define  acFileSaveDesign       2

CLASS VrReport INHERIT VrObject
   DATA hDC            EXPORTED
   DATA FileName       EXPORTED  INIT "Preview"
   DATA LandScape      EXPORTED  INIT .F.
   DATA PaperSize      EXPORTED  INIT 1
   DATA oPDF           EXPORTED
   DATA PreviewCaption EXPORTED  INIT "Visual Report - Print Preview"
   DATA nPage          PROTECTED INIT 0
   DATA nRow           EXPORTED  INIT 0
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
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init() CLASS VrReport
   ::aProperties := {}
   AADD( ::aProperties, { "Name",       "Object"  } )
   AADD( ::aProperties, { "DataSource", "Data"  } )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS VrReport
   TRY
      ::oPDF := GetActiveObject( "PDFCreactiveX.PDFCreactiveX" )
   CATCH
      TRY
         ::oPDF := CreateObject( "PDFCreactiveX.PDFCreactiveX" )
      CATCH
      END
   END
   IF ::oPDF == NIL
      MessageBox( 0, "Error loading report generator" )
      RETURN NIL
   ENDIF

   ::hDC   := GetDC(0)
   ::nPage := 0

   ::oPDF:SetLicenseKey( "WinFakt", "07EFCDAB010001008C5BD0102426F725C273B3A7C1B30B61521A8890359D83AE6FD68732DDAE4AC7E85003CDB8ED4F70678BF1EDF05F" )
   ::oPDF:ObjectAttribute( "Pages[1]", "PaperSize", ::PaperSize )
   ::oPDF:ObjectAttribute( "Pages[1]", "Landscape", ::LandScape )

   ::oPDF:ObjectAttributeSTR( "Document", "DefaultFont", "Courier New,12,400,0,0" )
   //::oPDF:StartSave( ::FileName + ".pdf", acFileSaveView )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD End() CLASS VrReport
   //::oPDF:EndSave()
   //IF ::nPage > 0
   //   ::Preview()
   // ELSE
   //   ::oPDF:End()
   //ENDIF
   ::oPDF:Save( ::FileName + ".pdf", acFileSaveAll )
   ReleaseDC(0, ::hDC )
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
   //::oPDF:SavePage( ::oPDF:CurrentPage )
   //::oPDF:ClearPage( ::oPDF:CurrentPage )
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

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS VrPreview INHERIT Dialog
   DATA Report EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oReport ) CLASS VrPreview
   ::Report := oReport
   ::Super:Init( NIL )
   ::Modal      := .T.
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::DlgModalFrame := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS VrPreview
   LOCAL oItem, oSub
   ::Caption    := ::Report:PreviewCaption
   
   WITH OBJECT StatusBar( Self )
      StatusBarPanel( ::StatusBar1, , 120 )
      StatusBarPanel( ::StatusBar1, ,  -1 )
      StatusBarPanel( ::StatusBar1, , 250 )
      :Create()
      :DockIt()
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
