/*
 *$Id: hdialog.prg,v 1.3 2004/02/07 15:24:22 lculik Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * HDialog class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

Static aSheet := Nil
Static aMessModalDlg := { ;
         { WM_COMMAND,{|o,w,l|DlgCommand(o,w,l)} },         ;
         { WM_NOTIFY,{|o,w,l|DlgNotify(o,w,l)} },           ;
         { WM_PAINT,{|o,w,l|DlgPaint(o,w,l)} },             ;
         { WM_DRAWITEM,{|o,w,l|DlgDrawItem(o,w,l)} },       ;
         { WM_CTLCOLORSTATIC,{|o,w,l|DlgCtlColor(o,w,l)} }, ;
         { WM_CTLCOLOREDIT,{|o,w,l|DlgCtlColor(o,w,l)} },   ;
         { WM_CTLCOLORBTN,{|o,w,l|DlgCtlColor(o,w,l)} },    ;
         { WM_SIZE,{|o,w,l|DlgSize(o,w,l)} },               ;
         { WM_INITDIALOG,{|o,w,l,h|InitModalDlg(o,w,l,h)} },;
         { WM_ERASEBKGND,{|o,w|DlgEraseBk(o,w)} },          ;
         { WM_DESTROY,{|o|DlgDestroy(o)} },                 ;
         { WM_ENTERIDLE,{|o,w,l|DlgEnterIdle(o,w,l)} },     ;
         { WM_ACTIVATE,{|o,w,l|DlgActivate(o,w,l)} }        ;
      }


Static aMessDlg := { ;
         { WM_COMMAND,{|o,w,l|DlgCommand(o,w,l)} },         ;
         { WM_NOTIFY,{|o,w,l|DlgNotify(o,w,l)} },           ;
         { WM_PAINT,{|o,w,l|DlgPaint(o,w,l)} },             ;
         { WM_DRAWITEM,{|o,w,l|DlgDrawItem(o,w,l)} },       ;
         { WM_CTLCOLORSTATIC,{|o,w,l|DlgCtlColor(o,w,l)} }, ;
         { WM_CTLCOLOREDIT,{|o,w,l|DlgCtlColor(o,w,l)} },   ;
         { WM_CTLCOLORBTN,{|o,w,l|DlgCtlColor(o,w,l)} },    ;
         { WM_SIZE,{|o,w,l|DlgSize(o,w,l)} },               ;
         { WM_DESTROY,{|o|DlgDestroy(o)} },                 ;
         { WM_ERASEBKGND,{|o,w|DlgEraseBk(o,w)} },          ;
         { WM_ENTERIDLE,{|o,w,l|DlgEnterIdle(o,w,l)} },     ;
         { WM_NCACTIVATE,{|o,w,l|DlgActivate(o,w,l)} }      ;
      }

Static aMessPsp := { ;
         { WM_COMMAND,{|o,w,l|DlgCommand(o,w,l)} },         ;
         { WM_PAINT,{|o,w,l|DlgPaint(o,w,l)} },             ;
         { WM_DRAWITEM,{|o,w,l|DlgDrawItem(o,w,l)} },       ;
         { WM_CTLCOLORSTATIC,{|o,w,l|DlgCtlColor(o,w,l)} }, ;
         { WM_CTLCOLOREDIT,{|o,w,l|DlgCtlColor(o,w,l)} },   ;
         { WM_CTLCOLORBTN,{|o,w,l|DlgCtlColor(o,w,l)} },    ;
         { WM_SIZE,{|o,w,l|DlgSize(o,w,l)} },               ;
         { WM_DESTROY,{|o|DlgDestroy(o)} },                 ;
         { WM_NOTIFY,{|o,w,l|PspNotify(o,w,l)} }            ;
      }

// ------------------------------------
// Class HDialog

CLASS HDialog INHERIT HCustomWindow

   CLASS VAR aDialogs   INIT {}
   CLASS VAR aModalDialogs  INIT {}

   DATA oPopup                // Context menu for a dialog
   DATA lResult  INIT .F.     // Becomes TRUE if the OK button is pressed
   DATA lUpdated INIT .F.     // TRUE, if any GET is changed
   DATA lClipper INIT .F.     // Set it to TRUE for moving between GETs with ENTER key
   DATA GetList  INIT {}      // The array of GET items in the dialog
   DATA KeyList  INIT {}      // The array of keys ( as Clipper's SET KEY )
   DATA lExitOnEnter INIT .F. // Set it to True, if dialog shouldn't be ended after pressing ENTER key,
                              // Added by Sandro Freire 
   DATA nLastKey INIT 0
   DATA oIcon, oBmp
   DATA bActivate
   DATA lActivated INIT .F.

   METHOD New( lType,nStyle,x,y,width,height,cTitle,oFont,bInit,bExit,bSize, ;
                  bPaint,bGfocus,bLfocus,bOther,lClipper,oBmp,oIcon,lExitOnEnter )
   METHOD Activate( lNoModal )
   METHOD AddItem( oWnd,lModal )
   METHOD DelItem( oWnd,lModal )
   METHOD FindDialog( hWnd )
   METHOD Close()	INLINE EndDialog()
ENDCLASS

METHOD NEW( lType,nStyle,x,y,width,height,cTitle,oFont,bInit,bExit,bSize, ;
                  bPaint,bGfocus,bLfocus,bOther,lClipper,oBmp,oIcon,lExitOnEnter ) CLASS HDialog

   ::oDefaultParent := Self
   ::type     := lType
   ::title    := cTitle
   ::style    := Iif( nStyle==Nil,WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_SIZEBOX,nStyle )
   ::oBmp     := oBmp
   ::oIcon    := oIcon
   ::nTop     := Iif( y==Nil,0,y )
   ::nLeft    := Iif( x==Nil,0,x )
   ::nWidth   := Iif( width==Nil,0,width )
   ::nHeight  := Iif( height==Nil,0,height )
   ::oFont    := oFont
   ::bInit    := bInit
   ::bDestroy := bExit
   ::bSize    := bSize
   ::bPaint   := bPaint
   ::bGetFocus  := bGFocus
   ::bLostFocus := bLFocus
   ::bOther     := bOther
   ::lClipper   := Iif( lClipper==Nil,.F.,lClipper )
   ::lExitOnEnter:=Iif( lExitOnEnter==Nil,.F.,lExitOnEnter )

RETURN Self

METHOD Activate( lNoModal ) CLASS HDialog

   CreateGetList( Self )
   IF ::type == WND_DLG_RESOURCE
      IF lNoModal == Nil .OR. !lNoModal
         ::AddItem( Self,.T. )
         // Hwg_DialogBox( HWindow():GetMain():handle,Self )
         Hwg_DialogBox( GetActiveWindow(),Self )
      ELSE
         ::handle  := 0
         ::lResult := .F.
         ::AddItem( Self,.F. )
         Hwg_CreateDialog( HWindow():GetMain():handle, Self )
         IF ::oIcon != Nil
            SendMessage( ::handle,WM_SETICON,1,::oIcon:handle )
         ENDIF
      ENDIF
   ELSEIF ::type == WND_DLG_NORESOURCE
      IF lNoModal == Nil .OR. !lNoModal
         ::AddItem( Self,.T. )
         // Hwg_DlgBoxIndirect( HWindow():GetMain():handle,Self,::nLeft,::nTop,::nWidth,::nHeight,::style )
         Hwg_DlgBoxIndirect( GetActiveWindow(),Self,::nLeft,::nTop,::nWidth,::nHeight,::style )
      ELSE
         ::handle  := 0
         ::lResult := .F.
         ::AddItem( Self,.F. )
         Hwg_CreateDlgIndirect( HWindow():GetMain():handle,Self,::nLeft,::nTop,::nWidth,::nHeight,::style )
         IF ::oIcon != Nil
            SendMessage( ::handle,WM_SETICON,1,::oIcon:handle )
         ENDIF
      ENDIF
   ENDIF

RETURN Nil

METHOD AddItem( oWnd,lModal ) CLASS HDialog
   Aadd( Iif( lModal,::aModalDialogs,::aDialogs ), oWnd )
RETURN Nil

METHOD DelItem( oWnd,lModal ) CLASS HDialog
Local i, h := oWnd:handle
   IF lModal
      IF ( i := Ascan( ::aModalDialogs,{|o|o:handle==h} ) ) > 0
         Adel( ::aModalDialogs,i )
         Asize( ::aModalDialogs, Len(::aModalDialogs)-1 )
      ENDIF
   ELSE
      IF ( i := Ascan( ::aDialogs,{|o|o:handle==h} ) ) > 0
         Adel( ::aDialogs,i )
         Asize( ::aDialogs, Len(::aDialogs)-1 )
      ENDIF
   ENDIF
RETURN Nil

METHOD FindDialog( hWnd ) CLASS HDialog
Local i := Ascan( ::aDialogs, {|o|o:handle==hWnd} )
Return Iif( i == 0, Nil, ::aDialogs[i] )

// End of class
// ------------------------------------

Function DefModalDlgProc( hDlg, msg, wParam, lParam )
Local oModalDlg, i

   // WriteLog( Str(hDlg,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   /*
   IF ( oModalDlg := Atail( HDialog():aModalDialogs ) ) == Nil .OR. ;
         ( oModalDlg:handle != Nil .AND. oModalDlg:handle != hDlg )
      Return 0
   ENDIF
   */
   IF ( i := Ascan( HDialog():aModalDialogs, {|o|o:handle==hDlg} ) ) == 0 .AND. ;
      ( ( oModalDlg := Atail( HDialog():aModalDialogs ) ) == Nil .OR. oModalDlg:handle == Nil )
      Return 0
   ENDIF
   IF i > 0
      oModalDlg := HDialog():aModalDialogs[ i ]
   ENDIF
   IF ( i := Ascan( aMessModalDlg, {|a|a[1]==msg} ) ) != 0
      Return Eval( aMessModalDlg[i,2], oModalDlg, wParam, lParam, hDlg )
   ELSE
      IF msg == WM_MOUSEMOVE
         DlgMouseMove()
      ENDIF
      IF oModalDlg:bOther != Nil
         Return Eval( oModalDlg:bOther, oModalDlg, msg, wParam, lParam )
      ENDIF
   ENDIF

Return 0

Static Function InitModalDlg( oDlg,wParam,lParam,hDlg )
Local iCont

   oDlg:handle := hDlg
   InitControls( oDlg,.T. )
   IF oDlg:oIcon != Nil
      SendMessage( oDlg:handle,WM_SETICON,1,oDlg:oIcon:handle )
   ENDIF
   IF oDlg:bInit != Nil
      Eval( oDlg:bInit, oDlg )
   ENDIF

Return 1

Function DlgEnterIdle( oDlg, wParam, lParam )
Local oItem

   IF wParam == 0 .AND. ( oItem := Atail( HDialog():aModalDialogs ) ) != Nil ;
         .AND. oItem:handle == lParam .AND. !oItem:lActivated
      oItem:lActivated := .T.
      IF oItem:bActivate != Nil
         Eval( oItem:bActivate, oItem )
      ENDIF
   ENDIF
Return 0

Static Function DlgEraseBk( oDlg,hDC )
Local aCoors

   IF __ObjHasMsg( oDlg,"OBMP") 
      IF oDlg:oBmp != Nil
         SpreadBitmap( hDC, oDlg:handle, oDlg:oBmp:handle )
         Return 1
      ELSE
        aCoors := GetClientRect( oDlg:handle )
        IF oDlg:brush != Nil
           FillRect( hDC, aCoors[1],aCoors[2],aCoors[3]+1,aCoors[4]+1,oDlg:brush:handle )
        ELSE
           FillRect( hDC, aCoors[1],aCoors[2],aCoors[3]+1,aCoors[4]+1,COLOR_3DFACE+1 )
        ENDIF
        Return 1
      ENDIF
   ENDIF
Return 0

Static Function DlgCommand( oDlg,wParam,lParam )
Local iParHigh := HiWord( wParam ), iParLow := LoWord( wParam )
Local aMenu, i

   IF iParHigh == 0 
      IF iParLow == IDOK
         FOR i := Len(oDlg:GetList) TO 1 STEP -1
            IF !oDlg:GetList[i]:lHide
               EXIT
            ENDIF
         NEXT
         IF i != 0 .AND. oDlg:GetList[i]:handle == GetFocus()
            IF __ObjHasMsg(oDlg:GetList[i],"BVALID") .AND. ;
                   Eval( oDlg:GetList[i]:bValid,oDlg:GetList[i] )
               IF !oDlg:lExitOnEnter
                  oDlg:lResult := .T.
                  EndDialog( oDlg:handle )
               ENDIF
               Return 1
            ENDIF
         ENDIF
         IF oDlg:lClipper
            IF !GetSkip( oDlg,GetFocus(),1 )
               IF !oDlg:lExitOnEnter
                  oDlg:lResult := .T.
                  EndDialog( oDlg:handle )
               ENDIF
            ENDIF
            Return 1
         ENDIF
      ELSEIF iParLow == IDCANCEL
         oDlg:nLastKey := 27
      ENDIF
   ENDIF

   IF oDlg:aEvents != Nil .AND. ;
      ( i := Ascan( oDlg:aEvents, {|a|a[1]==iParHigh.and.a[2]==iParLow} ) ) > 0
      Eval( oDlg:aEvents[ i,3 ],oDlg,iParLow )
   ELSEIF iParHigh == 0 
      IF ( iParLow == IDOK .AND. oDlg:FindControl(IDOK) != Nil ) .OR. ;
              iParLow == IDCANCEL
         IF iParLow == IDOK
            oDlg:lResult := .T.
         ENDIF
         EndDialog( oDlg:handle )
      ENDIF
   ELSEIF __ObjHasMsg(oDlg,"OPOPUP") .AND. oDlg:oPopup != Nil .AND. ;
         ( aMenu := Hwg_FindMenuItem( oDlg:oPopup:aMenu,wParam,@i ) ) != Nil ;
         .AND. aMenu[ 1,i,1 ] != Nil
         Eval( aMenu[ 1,i,1 ] )
   ENDIF

Return 1

Static Function DlgPaint( oDlg,wParam,lParam )

   IF oDlg:bPaint != Nil
      Eval( oDlg:bPaint,oDlg,wParam )
   ENDIF

Return 0

Function DlgDrawItem( oDlg,wParam,lParam )
Local oCtrl

   IF wParam != 0
      IF ( oCtrl  := oDlg:FindControl( wParam ) ) != Nil
         IF oCtrl:bPaint != Nil
            Eval( oCtrl:bPaint, oCtrl,lParam )
            Return 1
         ENDIF
      ENDIF
   ENDIF

Return 0

Function DlgMouseMove()
Local oBtn := SetOwnBtnSelected()

   IF oBtn != Nil .AND. !oBtn:lPress
      oBtn:state := OBTN_NORMAL
      InvalidateRect( oBtn:handle, 0 )
      PostMessage( oBtn:handle, WM_PAINT, 0, 0 )
      SetOwnBtnSelected( Nil )
   ENDIF

Return 0

Function DlgCtlColor( oDlg,wParam,lParam )
Local oCtrl  := oDlg:FindControl(,lParam)

   IF oCtrl != Nil
      IF oCtrl:tcolor != Nil
         SetTextColor( wParam, oCtrl:tcolor )
      ENDIF
      IF oCtrl:bcolor != Nil
         SetBkColor( wParam, oCtrl:bcolor )
         Return oCtrl:brush:handle
      ENDIF
      // Return -1
   ENDIF

Return -1

Static Function DlgSize( oDlg,wParam,lParam )
Local aControls, iCont

   IF oDlg:bSize != Nil .AND. ;
       ( oDlg:oParent == Nil .OR. !__ObjHasMsg( oDlg:oParent,"ACONTROLS" ) )
      Eval( oDlg:bSize, oDlg, LoWord( lParam ), HiWord( lParam ) )
   ENDIF
   aControls := oDlg:aControls
   IF aControls != Nil
      FOR iCont := 1 TO Len( aControls )
         IF aControls[iCont]:bSize != Nil
            Eval( aControls[iCont]:bSize, ;
             aControls[iCont], LoWord( lParam ), HiWord( lParam ) )
         ENDIF
      NEXT
   ENDIF

Return 0

Function DlgNotify( oDlg,wParam,lParam )
Local iItem, oCtrl := oDlg:FindControl( wParam ), nCode, res, handle, oItem

   IF oCtrl != Nil
      IF oCtrl:ClassName() == "HTAB"
         IF ( nCode := GetNotifyCode( lParam ) ) == TCN_SELCHANGE
            IF oCtrl != Nil .AND. oCtrl:bChange != Nil
               Eval( oCtrl:bChange, oCtrl, GetCurrentTab( oCtrl:handle ) )
            ENDIF
         ENDIF
      ELSEIF oCtrl:ClassName() == "HQHTM"
         Return oCtrl:Notify( oDlg,lParam )
      ELSEIF oCtrl:ClassName() == "HTREE"
         Return TreeNotify( oCtrl,lParam )
      ELSE
         nCode := GetNotifyCode( lParam )
         IF oDlg:aNotify != Nil .AND. ;
            ( iItem := Ascan( oDlg:aNotify, {|a|a[1]==nCode.and.a[2]==wParam} ) ) > 0
            IF ( res := Eval( oDlg:aNotify[ iItem,3 ],oDlg,wParam ) ) != Nil
               Return res
            ENDIF
         ENDIF
      ENDIF
   ENDIF

Return -1

Function DlgDestroy( oDlg )
Local aControls := oDlg:aControls
Local i, nLen := Len( aControls )
   FOR i := 1 TO nLen
      IF __ObjHasMsg( aControls[i],"END" )
         aControls[i]:End()
      ENDIF
   NEXT
   HDialog():DelItem( oDlg,.T. )
Return 1

Function DefDlgProc( hDlg, msg, wParam, lParam )
Local oDlg, i, aControls

   // WriteLog( Str(hDlg,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   IF msg == WM_INITDIALOG
      IF (i := Ascan( HDialog():aDialogs, {|o|o:handle==0} ) ) == 0
         MsgStop( "WM_INITDIALOG: wrong window handle "+Str( hDlg ),"Error!" )
         QUIT
      ENDIF
      oDlg := HDialog():aDialogs[ i ]
      oDlg:handle := hDlg
      InitControls( oDlg,.T. )
      aControls := oDlg:aControls
      IF aControls != Nil
         GetSkip( oDlg,,1 )
      ENDIF
      IF oDlg:bInit != Nil
         Eval( oDlg:bInit,oDlg )
      ENDIF
      IF oDlg:oFont != Nil
         SendMessage( oDlg:handle, WM_SETFONT, oDlg:oFont:handle, 0 )
      ENDIF
      Return 1
   ENDIF

   IF ( oDlg := HDialog():FindDialog(hDlg) ) == Nil
      Return 0
   ENDIF
   IF ( i := Ascan( aMessDlg, {|a|a[1]==msg} ) ) != 0
      Return Eval( aMessDlg[i,2], oDlg, wParam, lParam, hDlg )
   ELSE
      IF msg == WM_MOUSEMOVE
         DlgMouseMove()
      ENDIF
      IF oDlg:bOther != Nil
         Eval( oDlg:bOther, oDlg, msg, wParam, lParam )
      ENDIF
   ENDIF

Return 0

Function DefProc( o, msg, wParam, lParam )
Local i

   // WriteLog( Str(o:handle,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   IF ( i := Ascan( aMessDlg, {|a|a[1]==msg} ) ) != 0
      Return Eval( aMessDlg[i,2], o, wParam, lParam )
   ELSE
      IF msg == WM_MOUSEMOVE
         DlgMouseMove()
      ENDIF
      IF o:bOther != Nil
         Eval( o:bOther, o, msg, wParam, lParam )
      ENDIF
   ENDIF

Return 0

Static Function DlgActivate( oDlg,wParam,lParam )
Local iParLow := LoWord( wParam )

   if iParLow == 1 .AND. oDlg:bGetFocus != Nil
      Eval( oDlg:bGetFocus, oDlg )
   elseif iParLow == 0 .AND. oDlg:bLostFocus != Nil
      Eval( oDlg:bLostFocus, oDlg )
   endif

Return 0

Function DefPSPProc( hDlg, msg, wParam, lParam )
Local oDlg, i, aControls

   // WriteLog( Str(hDlg,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   IF msg == WM_INITDIALOG
      IF (i := Ascan( aSheet, {|a|a[1]==lParam} ) ) == 0
         MsgStop( "WM_INITDIALOG: wrong page handle"+Str( hDlg ),"Error!" )
      ELSE
         oDlg := aSheet[ i,2 ]
         HDialog():AddItem( oDlg,.F. )
         oDlg:handle := hDlg
         /*
         writelog( "DefPsp: "+Str(Len(HBrowse():aItemsList))+" "+oDlg:title )
         for i := 1 to Len(HBrowse():aItemsList)
            writelog( Str(HBrowse():aItemsList[i]:handle)+"  "+HBrowse():aItemsList[i]:oParent:title )
         next
         writelog( "======" )
         */
         InitControls( oDlg,.T. )
         IF oDlg:oIcon != Nil
            SendMessage( oDlg:handle,WM_SETICON,0,oDlg:oIcon:handle )
         ENDIF
         IF oDlg:bInit != Nil
            Eval( oDlg:bInit, oDlg )
         ENDIF
         /*
         aControls := oDlg:aControls
         IF aControls != Nil
            FOR i := 1 TO Len( aControls )
               IF aControls[i]:bInit != Nil
                  Eval( aControls[i]:bInit, aControls[i] )
               ENDIF
            NEXT
         ENDIF
         */
         Return 1
      ENDIF
   ENDIF
   IF ( oDlg := HDialog():FindDialog(hDlg) ) == Nil
      Return 0
   ENDIF
   IF ( i := Ascan( aMessPsp, {|a|a[1]==msg} ) ) != 0
      Return Eval( aMessPsp[i,2], oDlg, wParam, lParam, hDlg )
   ENDIF

Return 0

Static Function PspNotify( oDlg,wParam,lParam )
Local nCode := GetNotifyCode( lParam ), res := .T.
   IF nCode == PSN_SETACTIVE 
      IF oDlg:bGetFocus != Nil
         res := Eval( oDlg:bGetFocus, oDlg )
      ENDIF
      // 'res' should be 0(Ok) or -1
      Hwg_SetDlgResult( oDlg:handle,Iif(res,0,-1) )
      Return 1
   ELSEIF nCode == PSN_KILLACTIVE 
      IF oDlg:bLostFocus != Nil
         res := Eval( oDlg:bLostFocus, oDlg )
      ENDIF
      // 'res' should be 0(Ok) or 1
      Hwg_SetDlgResult( oDlg:handle,Iif(res,0,1) )
      Return 1
   ELSEIF nCode == PSN_RESET
   ELSEIF nCode == PSN_APPLY
      IF oDlg:bDestroy != Nil
         res := Eval( oDlg:bDestroy, oDlg )
      ENDIF
      // 'res' should be 0(Ok) or 2
      Hwg_SetDlgResult( oDlg:handle,Iif(res,0,2) )
      IF res
         oDlg:lResult := .T.
      ENDIF
      Return 1
   ELSE
      IF oDlg:bOther != Nil
         res := Eval( oDlg:bOther, oDlg, WM_NOTIFY, 0, lParam )
         Hwg_SetDlgResult( oDlg:handle,Iif(res,0,1) )
         return 1
      ENDIF
   ENDIF
Return 0

Function PropertySheet( hParentWindow, aPages, cTitle, x1, y1, width, height, ;
                           lModeless, lNoApply, lWizard )
Local hSheet, i, aHandles := Array( Len( aPages ) ), aTemplates := Array( Len( aPages ) )

   aSheet := Array( Len( aPages ) )
   FOR i := 1 TO Len( aPages )
      IF aPages[i]:type == WND_DLG_RESOURCE
         aHandles[i] := _CreatePropertySheetPage( aPages[i] )
      ELSE
         aTemplates[i] := CreateDlgTemplate( aPages[i],x1,y1,width,height,WS_CHILD+WS_VISIBLE+WS_BORDER )
         aHandles[i] := _CreatePropertySheetPage( aPages[i],aTemplates[i] )
      ENDIF
      aSheet[i] := { aHandles[i], aPages[i] }
   NEXT
   hSheet := _PropertySheet( hParentWindow, aHandles, Len( aHandles ), cTitle, ;
                        lModeless, lNoApply, lWizard )
   FOR i := 1 TO Len( aPages )
      ReleaseDlgTemplate( aTemplates[i] )
   NEXT

Return hSheet

Function GetModalDlg
Local i := Len( HDialog():aModalDialogs )
Return Iif( i>0, HDialog():aModalDialogs[i], 0 )

Function GetModalHandle
Local i := Len( HDialog():aModalDialogs )
Return Iif( i>0, HDialog():aModalDialogs[i]:handle, 0 )

Function EndDialog( handle )
Local oDlg
   IF handle == Nil
      IF ( oDlg := Atail( HDialog():aModalDialogs ) ) == Nil
         Return Nil
      ENDIF
   ELSE
      IF ( ( oDlg := Atail( HDialog():aModalDialogs ) ) == Nil .OR. ;
            oDlg:handle != handle ) .AND. ;
         ( oDlg := HDialog():FindDialog(handle) ) == Nil
         Return Nil
      ENDIF
   ENDIF
   IF oDlg:bDestroy != Nil
      IF Eval( oDlg:bDestroy, oDlg )
         Return Hwg_EndDialog( oDlg:handle )
      ELSE
         Return Nil
      ENDIF
   ENDIF
Return  Hwg_EndDialog( oDlg:handle )

Function SetDlgKey( oDlg, nctrl, nkey, block )
Local i, aKeys

   IF oDlg == Nil ; oDlg := HCustomWindow():oDefaultParent ; ENDIF
   IF nctrl == Nil ; nctrl := 0 ; ENDIF

   IF !__ObjHasMsg( oDlg,"KEYLIST" )
      Return .F.
   ENDIF
   aKeys := oDlg:KeyList
   IF block == Nil

      IF ( i := Ascan( aKeys,{|a|a[1]==nctrl.AND.a[2]==nkey} ) ) == 0
         Return .F.
      ELSE
         Adel( oDlg:KeyList, i )
         Asize( oDlg:KeyList, Len(oDlg:KeyList)-1 )
      ENDIF
   ELSE
      IF ( i := Ascan( aKeys,{|a|a[1]==nctrl.AND.a[2]==nkey} ) ) == 0
         Aadd( aKeys, { nctrl,nkey,block } )
      ELSE
         aKeys[i,3] := block
      ENDIF
   ENDIF

Return .T.
