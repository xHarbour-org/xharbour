/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HPrinter class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HPrinter INHERIT HObject

   DATA hDCPrn INIT 0
   DATA hDC
   DATA aMeta
   DATA lPreview
   DATA cMetaName

   METHOD New( cPrinter )
   METHOD StartDoc( lPreview,cMetaName )
   METHOD EndDoc()
   METHOD StartPage()
   METHOD EndPage()
   METHOD ReleaseMeta()
   METHOD PlayMeta( nPage, oWnd, x1, y1, x2, y2 )
   METHOD PrintMeta( nPage )
   METHOD Preview( cTitle )
   METHOD End()
   METHOD Box( x1,y1,x2,y2,oPen ) INLINE ;
                  Iif(oPen!=Nil,SelectObject(::hDC,oPen:handle),.F.), ;
                  Rectangle( ::hDC,x1,y1,x2,y2 )
   METHOD Line( x1,y1,x2,y2,oPen ) INLINE ;
                  Iif(oPen!=Nil,SelectObject(::hDC,oPen:handle),.F.), ;
                  DrawLine( ::hDC,x1,y1,x2,y2 )
   METHOD Say( cString,x1,y1,x2,y2,nOpt,oFont ) INLINE ;
                  Iif(oFont!=Nil,SelectObject(::hDC,oFont:handle),.F.), ;
                  DrawText( ::hDC,cString,x1,y1,x2,y2,Iif(nOpt==Nil,DT_LEFT,nOpt) )
   METHOD Bitmap( x1,y1,x2,y2,nOpt,hBitmap )  INLINE ;
                  DrawBitmap( ::hDC,hBitmap,Iif(nOpt==Nil,SRCAND,nOpt),x1,y1,x2-x1+1,y2-y1+1 )

ENDCLASS

METHOD New( cPrinter ) CLASS HPrinter
   IF cPrinter == Nil
      ::hDCPrn := PrintSetup()
   ELSEIF Empty( cPrinter )
      ::hDCPrn := Hwg_OpenDefaultPrinter()
   ELSE
      ::hDCPrn := Hwg_OpenPrinter( cPrinter )
   ENDIF
Return Self

METHOD End() CLASS HPrinter
   IF ::hDCPrn != 0
      DeleteDC( ::hDCPrn )
      ::hDCPrn := 0
   ENDIF
   ::ReleaseMeta()
Return Nil

METHOD StartDoc( lPreview,cMetaName ) CLASS HPrinter

   IF lPreview != Nil .AND. lPreview
      ::lPreview := .T.
      ::ReleaseMeta()
      ::aMeta := {}
      ::cMetaName := cMetaName
   ELSE
      ::lPreview := .F.
      ::hDC := ::hDCPrn
      Hwg_StartDoc( ::hDC )
   ENDIF

Return Nil

METHOD EndDoc() CLASS HPrinter

   IF !::lPreview
      Hwg_EndDoc( ::hDC )
   ENDIF
Return Nil

METHOD StartPage() CLASS HPrinter
Local fname

   IF ::lPreview
      fname := Iif( ::cMetaName!=Nil, ::cMetaName + Ltrim(Str(Len(::aMeta)+1)) + ".emf", Nil )
      // Aadd( ::aMeta, CreateEnhMetaFile( HWindow():GetMain():handle,fname ) )
      Aadd( ::aMeta, CreateMetaFile( ::hDCPrn,fname ) )
      ::hDC := Atail( ::aMeta )
   ELSE
      Hwg_StartPage( ::hDC )
   ENDIF

Return Nil

METHOD EndPage() CLASS HPrinter
Local nLen

   IF ::lPreview
     nLen := Len( ::aMeta )
     ::aMeta[nLen] := CloseEnhMetaFile( ::aMeta[nLen] )
     ::hDC := 0
   ELSE
     Hwg_EndPage( ::hDC )
   ENDIF
Return Nil

METHOD ReleaseMeta() CLASS HPrinter
Local i, nLen

   IF ::aMeta == Nil .OR. Empty( ::aMeta )
      Return Nil
   ENDIF

   nLen := Len( ::aMeta )
   FOR i := 1 TO nLen
      DeleteEnhMetaFile( ::aMeta[i] )
   NEXT
   ::aMeta := Nil

Return Nil

METHOD Preview( cTitle ) CLASS HPrinter
Local oDlg, oToolBar, oSayPage
Local oFont := HFont():Add( "Times New Roman",0,-13 )
Memvar x1, y1, x2, y2, nCurrPage
Private x1, y1, x2, y2, nCurrPage := 1

   IF cTitle == Nil; cTitle := "Print preview"; ENDIF

   INIT DIALOG oDlg TITLE cTitle                  ;
     AT 40,10 SIZE 600,440                        ;
     STYLE WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_SIZEBOX+WS_MAXIMIZEBOX ;
     ON PAINT {||::PlayMeta(nCurrPage,oDlg,x1,y1,x2,y2 )} ;
     ON SIZE {|o,x,y|ResizePreviewDlg(o,x,y,Self)}

   oDlg:brush := HBrush():Add( 0 )

   @ 0,0 PANEL oToolBar SIZE 36,oDlg:nHeight

   @ 0,2 SAY oSayPage CAPTION "1:"+Ltrim(Str(Len(::aMeta))) OF oToolBar ;
        SIZE 36,22 STYLE WS_BORDER+SS_CENTER FONT oFont BACKCOLOR 12507070

   @ 1,26 OWNERBUTTON OF oToolBar ON CLICK {||::PrintMeta()} ;
        SIZE 33,24 TEXT "Print" FONT oFont FLAT   ;
        TOOLTIP "Print file"
   @ 1,50 OWNERBUTTON OF oToolBar ON CLICK {||ChangePage(oDlg,oSayPage,Self,1)} ;
        SIZE 33,24 TEXT ">>" FONT oFont FLAT                 ;
        TOOLTIP "Next page"
   @ 1,74 OWNERBUTTON OF oToolBar ON CLICK {||ChangePage(oDlg,oSayPage,Self,-1)} ;
        SIZE 33,24 TEXT "<<" FONT oFont FLAT   ;
        TOOLTIP "Previous page"
   @ 1,98 OWNERBUTTON OF oToolBar ON CLICK {||EndDialog()} ;
        SIZE 33,24 TEXT "Exit" FONT oFont FLAT     ;
        TOOLTIP "Exit Preview"

   ResizePreviewDlg( oDlg, 600, 440, Self )

   oDlg:Activate()
   oDlg:brush:Release()

Return Nil

Static Function ChangePage( oDlg,oSayPage,oPrinter,n )
Memvar nCurrPage

   IF n > 0 .AND. nCurrPage < Len(oPrinter:aMeta)
      nCurrPage ++
   ELSEIF n < 0 .AND. nCurrPage > 1
      nCurrPage --
   ENDIF
   oSayPage:SetValue( Ltrim(Str(nCurrPage))+":"+Ltrim(Str(Len(oPrinter:aMeta))) )
   RedrawWindow( oDlg:handle, RDW_ERASE + RDW_INVALIDATE )
Return Nil

Static Function ResizePreviewDlg( oDlg, x, y, oPrinter )
Local aPrnCoors := GetDeviceArea( oPrinter:hDCPrn )
Local nWidth, nHeight
Memvar x1, y1, x2, y2

   y -= 10
   IF aPrnCoors[1] > aPrnCoors[2]
      nWidth := x - 20
      nHeight := Round( nWidth * aPrnCoors[2] / aPrnCoors[1], 0 )
      IF nHeight > y - 20
         nHeight := y - 20
         nWidth := Round( nHeight * aPrnCoors[1] / aPrnCoors[2], 0 )
      ENDIF
   ELSE
      nHeight := y - 10
      nWidth := Round( nHeight * aPrnCoors[1] / aPrnCoors[2], 0 )
      IF nWidth > x - 20
         nWidth := x - 20
         nHeight := Round( nWidth * aPrnCoors[2] / aPrnCoors[1], 0 )
      ENDIF
   ENDIF

   x1 := Round( (x-nWidth)/2,0 )
   x2 := x1 + nWidth - 1
   y1 := Round( (y-nHeight)/2+10,0 )
   y2 := y1 + nHeight - 1

   RedrawWindow( oDlg:handle, RDW_ERASE + RDW_INVALIDATE )
   // InvalidateRect( oDlg:handle, 0 )
   // SendMessage( oDlg:handle, WM_PAINT, 0, 0 )

Return Nil

METHOD PlayMeta( nPage, oWnd, x1, y1, x2, y2 ) CLASS HPrinter
Local pps := DefinePaintStru(), hDC := BeginPaint( oWnd:handle, pps )

   FillRect( hDC, x1, y1, x2, y2, COLOR_3DHILIGHT+1 )
   PlayEnhMetafile( hDC, ::aMeta[nPage], x1, y1, x2, y2 )
   EndPaint( oWnd:handle, pps )
Return Nil

METHOD PrintMeta( nPage ) CLASS HPrinter

   IF ::lPreview

      ::StartDoc()
      IF nPage == Nil
         FOR nPage := 1 TO Len( ::aMeta )
            PrintEnhMetafile( ::hDCPrn,::aMeta[nPage] )
         NEXT
      ELSE
         PrintEnhMetafile( ::hDCPrn,::aMeta[nPage] )
      ENDIF
      ::EndDoc()
      ::lPreview := .T.
   ENDIF
Return Nil
