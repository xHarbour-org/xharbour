/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Main prg level functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"

Function InitControls( oWnd,lNoActivate )
Local i, pArray := oWnd:aControls

   lNoActivate := Iif( lNoActivate==Nil,.F.,lNoActivate )
   IF pArray != Nil
      FOR i := 1 TO Len( pArray )
         // writelog( "InitControl1"+str(pArray[i]:handle)+"/"+pArray[i]:classname )
         IF pArray[i]:handle == 0 .AND. !lNoActivate
            pArray[i]:Activate()
         ENDIF
         IF pArray[i]:handle <= 0          
            pArray[i]:handle := GetDlgItem( oWnd:handle, pArray[i]:id )
            // writelog( "InitControl2"+str(pArray[i]:handle)+"/"+pArray[i]:classname )
         ENDIF
         IF !Empty( pArray[i]:aControls )
            InitControls( pArray[i] )
         ENDIF
         pArray[i]:Init()
      NEXT
   ENDIF

Return .T.

Function FindParent( hCtrl,nLevel )
Local i, oParent, hParent := GetParent( hCtrl )
   IF hParent > 0
      IF ( i := Ascan( HDialog():aModalDialogs,{|o|o:handle==hParent} ) ) != 0
         Return HDialog():aModalDialogs[i]
      ELSEIF ( oParent := HDialog():FindDialog(hParent) ) != Nil
         Return oParent
      ELSEIF ( oParent := HWindow():FindWindow(hParent) ) != Nil
         Return oParent
      ENDIF
   ENDIF
   IF nLevel == Nil; nLevel := 0; ENDIF
   IF nLevel < 2
      IF ( oParent := FindParent( hParent,nLevel+1 ) ) != Nil
         Return oParent:FindControl( ,hParent )
      ENDIF
   ENDIF
Return Nil

Function FindSelf( hCtrl )
Local oParent
   oParent := FindParent( hCtrl )
   IF oParent != Nil
      Return oParent:FindControl( ,hCtrl )
   ENDIF
Return Nil

Function WriteStatus( oWnd, nPart, cText, lRedraw )
Local aControls, i
   aControls := oWnd:aControls
   IF ( i := Ascan( aControls, {|o|o:ClassName()=="HSTATUS"} ) ) > 0
      WriteStatusWindow( aControls[i]:handle,nPart-1,cText )
      IF lRedraw != Nil .AND. lRedraw
         RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
   ENDIF
Return Nil

Function VColor( cColor )
Local i,res := 0, n := 1, iValue
   cColor := Trim(cColor)
   for i := 1 to Len( cColor )
      iValue := Asc( Substr( cColor,Len(cColor)-i+1,1 ) )
      if iValue < 58 .and. iValue > 47
         iValue -= 48
      elseif iValue >= 65 .and. iValue <= 70
         iValue -= 55
      elseif iValue >= 97 .and. iValue <= 102
         iValue -= 87
      else
        Return 0
      endif
      res += iValue * n
      n *= 16
   next
Return res

Function MsgGet( cTitle, cText, nStyle, x, y, nDlgStyle )
Local oModDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cRes := ""

   nStyle := Iif( nStyle == Nil, 0, nStyle )
   x := Iif( x == Nil, 210, x )
   y := Iif( y == Nil, 10, y )
   nDlgStyle := Iif( nDlgStyle == Nil, 0, nDlgStyle )

   INIT DIALOG oModDlg TITLE cTitle AT x,y SIZE 300,140 ;
        FONT oFont CLIPPER STYLE WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_SIZEBOX+nDlgStyle

   @ 20,10 SAY cText SIZE 260,22
   @ 20,35 GET cres  SIZE 260,26 STYLE WS_DLGFRAME + WS_TABSTOP + nStyle

   @ 20,95 BUTTON "Ok" ID IDOK SIZE 100,32
   @ 180,95 BUTTON "Cancel" ID IDCANCEL SIZE 100,32

   ACTIVATE DIALOG oModDlg

   oFont:Release()
   IF oModDlg:lResult
      Return Trim( cRes )
   ELSE
      cRes := ""
   ENDIF

Return cRes

Function WChoice( arr, cTitle, nLeft, nTop, oFont, clrT, clrB, clrTSel, clrBSel )
Local oDlg, oBrw
Local nChoice := 0, i, aLen := Len( arr ), nLen := 0, addX := 20, addY := 30
Local hDC, aMetr, width, height, aArea, aRect

   IF cTitle == Nil; cTitle := ""; ENDIF
   IF nLeft == Nil; nLeft := 10; ENDIF
   IF nTop == Nil; nTop := 10; ENDIF
   IF oFont == Nil; oFont := HFont():Add( "MS Sans Serif",0,-13 ); ENDIF

   IF Valtype( arr[1] ) == "A"
      FOR i := 1 TO aLen
         nLen := Max( nLen,Len(arr[i,1]) )
      NEXT
   ELSE
      FOR i := 1 TO aLen
         nLen := Max( nLen,Len(arr[i]) )
      NEXT
   ENDIF

   hDC := GetDC( GetActiveWindow() )
   SelectObject( hDC, ofont:handle )
   aMetr := GetTextMetric( hDC )
   aArea := GetDeviceArea( hDC )
   aRect := GetWindowRect( GetActiveWindow() )
   ReleaseDC( GetActiveWindow(),hDC )
   height := (aMetr[1]+1)*aLen+4+addY
   IF height > aArea[2]-aRect[2]-nTop-30
      height := aArea[2]-aRect[2]-nTop-30
      addX := addY := 0
   ENDIF
   width := ( Round( (aMetr[3]+aMetr[2]) / 2,0 ) + 3 ) * nLen + addX

   INIT DIALOG oDlg TITLE cTitle ;
         AT nLeft,nTop           ;
         SIZE width,height  ;
         FONT oFont              ;
         ON INIT {|o|ResetWindowPos(o:handle)}

   @ 0,0 BROWSE oBrw ARRAY          ;
       FONT oFont                   ;
       STYLE WS_BORDER              ;
       ON SIZE {|o,x,y|MoveWindow(o:handle,addX/2,addY/4,x-addX,y-addY)} ;
       ON CLICK {|o|nChoice:=o:tekzp,EndDialog(o:oParent:handle)}

   IF Valtype( arr[1] ) == "A"
      oBrw:AddColumn( HColumn():New( ,{|value,o|o:msrec[o:tekzp,1]},"C",nLen ) )
   ELSE
      oBrw:AddColumn( HColumn():New( ,{|value,o|o:msrec[o:tekzp]},"C",nLen ) )
   ENDIF
   CreateArList( oBrw, arr )
   oBrw:lDispHead := .F.
   IF clrT != Nil
      oBrw:tcolor := clrT
   ENDIF
   IF clrB != Nil
      oBrw:bcolor := clrB
   ENDIF
   IF clrTSel != Nil
      oBrw:tcolorSel := clrTSel
   ENDIF
   IF clrBSel != Nil
      oBrw:bcolorSel := clrBSel
   ENDIF

   oDlg:Activate()
   oFont:Release()

Return nChoice

Function ShowProgress( nStep,maxPos,nRange,cTitle,oWnd,x1,y1,width,height )
Local nStyle := WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_SIZEBOX
Static oDlg, hPBar, iCou, nLimit

   IF nStep == 0
      nLimit := Iif( nRange != Nil,Int( nRange/maxPos ),1 )
      iCou := 0
      x1 := Iif( x1==Nil,0,x1 )
      y1 := Iif( x1==Nil,0,y1 )
      width := Iif( width==Nil,220,width )
      height := Iif( height==Nil,55,height )
      IF x1 == 0
         nStyle += DS_CENTER
      ENDIF
      IF oWnd != Nil
         oDlg := Nil
         hPBar := CreateProgressBar( oWnd:handle, maxPos,20,25,width-40,20 )
      ELSE
         INIT DIALOG oDlg TITLE cTitle   ;
              AT x1,y1 SIZE width,height ;
              STYLE nStyle               ;
              ON INIT {|o|hPBar:=CreateProgressBar(o:handle, maxPos,20,25,width-40,20)}
         ACTIVATE DIALOG oDlg NOMODAL
      ENDIF
   ELSEIF nStep == 1
      iCou ++
      IF iCou == nLimit
         iCou := 0
         UpdateProgressBar( hPBar )
      ENDIF
   ELSEIF nStep == 2
      UpdateProgressBar( hPBar )
   ELSEIF nStep == 3
      SetWindowText( oDlg:handle,cTitle )
      IF maxPos != Nil
         SetProgressBar( hPBar,maxPos )
      ENDIF
   ELSE
      DestroyWindow( hPBar )
      IF oDlg != Nil
         EndDialog( oDlg:handle )
      ENDIF
   ENDIF
           
Return Nil

Function EndWindow()
   IF HWindow():GetMain() != Nil
      SendMessage( Hwg_GetWindowHandle(1), WM_SYSCOMMAND, SC_CLOSE, 0 )
   ENDIF
Return Nil

FUNCTION HdSerial( cDrive )

   Local n       :=  HDGETSERIAL( cDrive )
   Local cHex    :=  HB_NUMTOHEX(n)
   Local cResult := ""
   cResult := Substr( cHex,1, 4 ) + '-' + Substr( cHex,5, 4 )

return cResult


