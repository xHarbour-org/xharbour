/*
 *$Id: hedit.prg,v 1.3 2003/11/15 08:34:32 alkresin Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * HEdit class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "hblang.ch"
#include "guilib.ch"


CLASS HEdit INHERIT HControl

   CLASS VAR winclass   INIT "EDIT"
   DATA lMultiLine   INIT .F.
   DATA cType INIT "C"
   DATA bSetGet
   DATA bValid
   DATA cPicFunc, cPicMask
   DATA lPicComplex INIT .F.
   DATA lFirst      INIT .T.
   DATA lChanged    INIT .F.

   METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
         oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt,tcolor,bcolor,cPicture,lNoBorder )
   METHOD Activate()
   METHOD Redefine( oWnd,nId,vari,bSetGet,oFont,bInit,bSize,bDraw,bGfocus, ;
             bLfocus,ctoolt,tcolor,bcolor,cPicture )
   METHOD Init()
   METHOD SetGet(value) INLINE Eval( ::bSetGet,value )
   METHOD Refresh() 

ENDCLASS

METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt, ;
                  tcolor,bcolor,cPicture,lNoBorder ) CLASS HEdit

   // ::classname:= "HEDIT"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   IF vari != Nil
      ::cType   := Valtype( vari )
   ENDIF
   IF bSetGet == Nil
      ::title := vari
   ENDIF
   ::bSetGet := bSetGet
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), ;
       WS_CHILD+WS_VISIBLE+WS_TABSTOP+Iif(lNoBorder==Nil.OR.!lNoBorder,WS_BORDER,0) )
   IF Hwg_BitAnd( ::style,ES_MULTILINE ) != 0
      ::style := Hwg_BitOr( ::style,ES_WANTRETURN )
      ::lMultiLine := .T.
   ENDIF
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ParsePict( Self, cPicture, vari )
   ::SetColor( tcolor,Iif( bcolor==Nil,GetSysColor( COLOR_BTNHIGHLIGHT ),bcolor ) )

   ::Activate()
   ::oParent:AddControl( Self )

   IF bSetGet != Nil
      ::bGetFocus := bGFocus
      ::bLostFocus := bLFocus
      ::oParent:AddEvent( EN_SETFOCUS,::id,{|o,id|__When(o:FindControl(id))} )
      ::oParent:AddEvent( EN_KILLFOCUS,::id,{|o,id|__Valid(o:FindControl(id))} )
      ::bValid := {|o|__Valid(o)}
   ELSE
      IF bGfocus != Nil
         ::oParent:AddEvent( EN_SETFOCUS,::id,bGfocus )
      ENDIF
      IF bLfocus != Nil
         ::oParent:AddEvent( EN_KILLFOCUS,::id,bLfocus )
      ENDIF
   ENDIF

Return Self

METHOD Activate CLASS HEdit
   IF ::oParent:handle != 0
      ::handle := CreateEdit( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Redefine( oWndParent,nId,vari,bSetGet,oFont,bInit,bSize,bPaint, ;
          bGfocus,bLfocus,ctoolt,tcolor,bcolor,cPicture )  CLASS HEdit
   // ::classname:= "HEDIT"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   IF vari != Nil
      ::cType   := Valtype( vari )
      // ::title   := Iif(::cType=="D",Dtoc(vari),Iif(::cType=="N",Str(vari),Iif(::cType=="C",vari,"")))
   ENDIF
   ::bSetGet := bSetGet
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ParsePict( Self, cPicture, vari )
   ::SetColor( tcolor,Iif( bcolor==Nil,GetSysColor( COLOR_BTNHIGHLIGHT ),bcolor ) )

   ::oParent:AddControl( Self )
   IF bSetGet != Nil
      ::bGetFocus := bGFocus
      ::bLostFocus := bLFocus
      ::oParent:AddEvent( EN_SETFOCUS,::id,{|o,id|__When(o:FindControl(id))} )
      ::oParent:AddEvent( EN_KILLFOCUS,::id,{|o,id|__Valid(o:FindControl(id))} )
      ::bValid := {|o|__Valid(o)}
   ELSE
      IF bGfocus != Nil
         ::oParent:AddEvent( EN_SETFOCUS,::id,bGfocus )
      ENDIF
      IF bLfocus != Nil
         ::oParent:AddEvent( EN_KILLFOCUS,::id,bLfocus )
      ENDIF
   ENDIF
Return Self

METHOD Init()  CLASS HEdit
   IF !::lInit
      Super:Init()
      IF ::bSetGet != Nil
         IF !::lMultiLine
            Hwg_InitEditProc( ::handle )
         ENDIF
         ::Refresh()
      ENDIF
   ENDIF
Return Nil

METHOD Refresh()  CLASS HEdit
Local vari

   IF ::bSetGet != Nil
      vari := Eval( ::bSetGet )
      IF !Empty( ::cPicFunc ) .OR. !Empty( ::cPicMask )
         vari := Transform( vari, ::cPicFunc + Iif(Empty(::cPicFunc),""," ") + ::cPicMask )
      ELSE
         vari := Iif(::cType=="D",Dtoc(vari),Iif(::cType=="N",Str(vari),Iif(::cType=="C",vari,"")))
      ENDIF
      IF vari != ::title
         ::title := vari
      ENDIF
      SetDlgItemText( ::oParent:handle,::id,vari )
   ELSE
      SetDlgItemText( ::oParent:handle,::id,::title )
   ENDIF

Return Nil

Function DefEditProc( hEdit, msg, wParam, lParam )
Local oEdit, oParent, nPos, nctrl, cKeyb
   // writelog( "EditProc: " + Str(hEdit,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   IF msg == WM_CHAR

      oEdit := FindSelf( hEdit )
      IF wParam == 8
         oEdit:lFirst := .F.
         SetGetUpdated( oEdit )
         IF oEdit:lPicComplex
            DeleteChar( oEdit,.T. )
            Return 0
         ENDIF
         Return -1
      ELSEIF wParam == 13
         Return -1
      ENDIF
      IF oEdit:bSetGet != Nil
         // ------- Change by NightWalker - Check HiBit -------
         If (wParam <129).or.!Empty( oEdit:cPicFunc ).OR.!Empty( oEdit:cPicMask )
            Return GetApplyKey( oEdit,Chr(wParam) )
         Endif
      ENDIF

   ELSEIF msg == WM_KEYDOWN 

      IF ( oParent := FindParent( hEdit ) ) != Nil
         oEdit := oParent:FindControl(,hEdit)
         IF wParam == 40     // KeyDown
            IF ReadExit() .AND. !IsCtrlShift()
               GetSkip( oParent,hEdit,1 )
               Return 0
            ENDIF
         ELSEIF wParam == 38     // KeyUp
            IF ReadExit() .AND. !IsCtrlShift()
               GetSkip( oParent,hEdit,-1 )
               Return 0
            ENDIF
         ELSEIF wParam == 39     // KeyRight
            IF !IsCtrlShift()
               oEdit:lFirst := .F.
               Return KeyRight( oParent:FindControl(,hEdit) )
            ENDIF
         ELSEIF wParam == 37     // KeyLeft
               oEdit:lFirst := .F.
               Return KeyLeft( oParent:FindControl(,hEdit) )
         ELSEIF wParam == 35     // End
               oEdit:lFirst := .F.
               IF oEdit:cType == "C"
                  nPos := Len( Trim( oEdit:title ) )
                  SendMessage( oEdit:handle, EM_SETSEL, nPos, nPos )
                  Return 0
               ENDIF
         ELSEIF wParam == 45     // Insert
            IF !IsCtrlShift()
               Set( _SET_INSERT, ! Set( _SET_INSERT ) )
            ENDIF
         ELSEIF wParam == 46     // Del
            oEdit:lFirst := .F.
            SetGetUpdated( oEdit )
            IF oEdit:lPicComplex
               DeleteChar( oEdit,.F. )
               Return 0
            ENDIF
         ENDIF
      ENDIF

   ELSEIF msg == WM_KEYUP

      IF wParam != 16 .AND. wParam != 17 .AND. wParam != 18
         IF ( oParent := FindParent( hEdit ) ) != Nil
            DO WHILE oParent != Nil .AND. !__ObjHasMsg( oParent,"GETLIST" )
               oParent := oParent:oParent
            ENDDO
            IF oParent != Nil
               cKeyb := GetKeyboardState()
               /*
               nctrl := ""
               for i := 16 to 32
                  nctrl += Str(Asc(Substr(cKeyb,i,1)),4)
               next
               writelog(nctrl)
               */
               nctrl := Iif( Asc(Substr(cKeyb,VK_CONTROL+1,1))>=128,FCONTROL,Iif( Asc(Substr(cKeyb,VK_SHIFT+1,1))>=128,FSHIFT,0 ) )
               IF ( nPos := Ascan( oParent:KeyList,{|a|a[1]==nctrl.AND.a[2]==wParam} ) ) > 0
                  Eval( oParent:KeyList[ nPos,3 ] )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ELSEIF msg == WM_LBUTTONUP
      oEdit := FindSelf( hEdit )
      IF Empty( GetEditText( oEdit:oParent:handle, oEdit:id ) )
         SendMessage( oEdit:handle, EM_SETSEL, 0, 0 )
      ENDIF
      // writelog( str(HiWord(SendMessage(oEdit:handle,EM_GETSEL,0,0))) )
   ENDIF
Return -1

Static Function IsCtrlShift()
Local cKeyb := GetKeyboardState()
Return Asc(Substr(cKeyb,VK_CONTROL+1,1)) >= 128 .OR. Asc(Substr(cKeyb,VK_SHIFT+1,1)) >= 128

Static Function ParsePict( oEdit,cPicture,vari )
Local nAt, i, masklen, cChar

   IF oEdit:bSetGet == Nil
      Return Nil
   ENDIF
   oEdit:cPicFunc := oEdit:cPicMask := ""
   IF cPicture != Nil
      IF Left( cPicture, 1 ) == "@"
         nAt := At( " ", cPicture )
         IF nAt == 0
            oEdit:cPicFunc := Upper( cPicture )
            oEdit:cPicMask := ""
         ELSE
            oEdit:cPicFunc := Upper( SubStr( cPicture, 1, nAt - 1 ) )
            oEdit:cPicMask := SubStr( cPicture, nAt + 1 )
         ENDIF
         IF oEdit:cPicFunc == "@"
            oEdit:cPicFunc := ""
         ENDIF
      ELSE
         oEdit:cPicFunc   := ""
         oEdit:cPicMask   := cPicture
      ENDIF
   ENDIF

   IF Empty( oEdit:cPicMask ) 
      IF oEdit:cType == "D"
         oEdit:cPicMask := StrTran( Dtoc( Ctod( Space(8) ) ),' ','9' )
      ELSEIF oEdit:cType == "N"
         vari := Str( vari )
         IF ( nAt := At( ".", vari ) ) > 0
            oEdit:cPicMask := Replicate( '9', nAt - 1 ) + "." + ;
                  Replicate( '9', Len( vari ) - nAt )
         ELSE
            oEdit:cPicMask := Replicate( '9', Len( vari ) )
         ENDIF
      ENDIF
   ENDIF

   IF !Empty( oEdit:cPicMask )
      masklen := Len( oEdit:cPicMask )
      FOR i := 1 TO masklen
         cChar := SubStr( oEdit:cPicMask, i, 1 )
         IF !cChar $ "!ANX9#"
            oEdit:lPicComplex := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

Return Nil

Static Function IsEditable( oEdit,nPos )
Local cChar

   IF Empty( oEdit:cPicMask )
      Return .T.
   ENDIF
   IF nPos > Len( oEdit:cPicMask )
      Return .F.
   ENDIF

   cChar := SubStr( oEdit:cPicMask, nPos, 1 )

   IF oEdit:cType == "C"
      return cChar $ "!ANX9#"
   ELSEIF oEdit:cType == "N"
      Return cChar $ "9#$*"
   ELSEIF oEdit:cType == "D"
      Return cChar == "9"
   ELSEIF oEdit:cType == "L"
      Return cChar $ "TFYN"
   ENDIF

Return .F.

Static Function KeyRight( oEdit,nPos )
Local i, masklen
   IF oEdit == Nil
      Return -1
   ENDIF
   IF nPos == Nil
      nPos := HiWord( SendMessage( oEdit:handle, EM_GETSEL, 0, 0 ) ) + 1
   ENDIF
   IF oEdit:cPicMask == Nil .OR. Empty( oEdit:cPicMask )
      SendMessage( oEdit:handle, EM_SETSEL, nPos, nPos )
   ELSE
      masklen := Len( oEdit:cPicMask )
      DO WHILE nPos <= masklen
         IF IsEditable( oEdit,++nPos )
            // writelog( "KeyRight-2 "+str(nPos) )
            SendMessage( oEdit:handle, EM_SETSEL, nPos-1, nPos-1 )
            EXIT
         ENDIF
      ENDDO
   ENDIF
Return 0

Static Function KeyLeft( oEdit,nPos )
Local i
   IF oEdit == Nil
      Return -1
   ENDIF
   IF nPos == Nil
      nPos := HiWord( SendMessage( oEdit:handle, EM_GETSEL, 0, 0 ) ) + 1
   ENDIF
   IF oEdit:cPicMask == Nil .OR. Empty( oEdit:cPicMask )
      SendMessage( oEdit:handle, EM_SETSEL, nPos-2, nPos-2 )
   ELSE
      DO WHILE nPos >= 1
         IF IsEditable( oEdit,--nPos )
            SendMessage( oEdit:handle, EM_SETSEL, nPos-1, nPos-1 )
            EXIT
         ENDIF
      ENDDO
   ENDIF
Return 0

Static Function DeleteChar( oEdit,lBack )
Local nPos := HiWord( SendMessage( oEdit:handle, EM_GETSEL, 0, 0 ) ) + Iif( !lBack,1,0 )
Local nGetLen := Len( oEdit:cPicMask ), nLen

   FOR nLen := 0 TO nGetLen
      IF !IsEditable( oEdit,nPos+nLen )
         Exit
      ENDIF
   NEXT
   IF nLen == 0
      DO WHILE nPos >= 1
         nPos --
         nLen ++
         IF IsEditable( oEdit,nPos )
            EXIT
         ENDIF
      ENDDO
   ENDIF
   IF nPos > 0
      oEdit:title := PadR( Left( oEdit:title, nPos-1 ) + ;
                  SubStr( oEdit:title, nPos+1, nLen-1 ) + " " + ;
                  SubStr( oEdit:title, nPos+nLen ), nGetLen )
      SetDlgItemText( oEdit:oParent:handle, oEdit:id, oEdit:title )
      SendMessage( oEdit:handle, EM_SETSEL, nPos-1, nPos-1 )
   ENDIF
   
Return Nil

Static Function Input( oEdit,cChar,nPos )
Local cPic

   IF !Empty( oEdit:cPicMask ) .AND. nPos > Len( oEdit:cPicMask )
      Return Nil
   ENDIF
   IF oEdit:cType == "N"
      IF cChar == "-"
         IF nPos != 1
            Return Nil
         ENDIF
         // ::minus := .t.
      ELSEIF !( cChar $ "0123456789" )
         Return Nil
      ENDIF

   ELSEIF oEdit:cType == "D"

      IF !( cChar $ "0123456789" )
         Return Nil
      ENDIF

   ELSEIF oEdit:cType == "L"

      IF !( Upper( cChar ) $ "YNTF" )
         Return Nil
      ENDIF

   ENDIF

   IF !Empty( oEdit:cPicFunc )
      cChar := Transform( cChar, oEdit:cPicFunc )
   ENDIF

   IF !Empty( oEdit:cPicMask )
      cPic  := Substr( oEdit:cPicMask, nPos, 1 )

      cChar := Transform( cChar, cPic )
      IF cPic == "A"
         if ! IsAlpha( cChar )
            cChar := Nil
         endif
      ELSEIF cPic == "N"
         IF ! IsAlpha( cChar ) .and. ! IsDigit( cChar )
            cChar := Nil
         ENDIF
      ELSEIF cPic == "9"
         IF ! IsDigit( cChar ) .and. cChar != "-"
            cChar := Nil
         ENDIF
      ELSEIF cPic == "#"
         IF ! IsDigit( cChar ) .and. !( cChar == " " ) .and. !( cChar $ "+-" )
            cChar := Nil
         ENDIF
      ENDIF
   ENDIF

Return cChar

Static Function GetApplyKey( oEdit,cKey )
Local nPos, nGetLen, nLen, vari

   // writelog( "GetApplyKey "+str(asc(ckey)) )
   oEdit:title := GetEditText( oEdit:oParent:handle, oEdit:id )
   IF oEdit:cType == "N" .and. cKey == "." .AND. ;
                     ( nPos := At( ".",oEdit:cPicMask ) ) != 0
      IF oEdit:lFirst
         vari := 0
      ELSE
         vari := Val( oEdit:title )
      ENDIF
      IF !Empty( oEdit:cPicFunc ) .OR. !Empty( oEdit:cPicMask )
         oEdit:title := Transform( vari, oEdit:cPicFunc + Iif(Empty(oEdit:cPicFunc),""," ") + oEdit:cPicMask )
      ENDIF
      SetDlgItemText( oEdit:oParent:handle, oEdit:id, oEdit:title )
      KeyRight( oEdit,nPos-1 )
   ELSE
      IF oEdit:cType == "N" .AND. oEdit:lFirst
         // SetDlgItemText( oEdit:oParent:handle, oEdit:id, "" )
         nGetLen := Len( oEdit:cPicMask )
         IF ( nPos := At( ".",oEdit:cPicMask ) ) == 0
            oEdit:title := Space( nGetLen )
         ELSE
            oEdit:title := Space( nPos-1 ) + "." + Space( nGetLen-nPos )
         ENDIF
         nPos := 1
      ELSE
         nPos := HiWord( SendMessage( oEdit:handle, EM_GETSEL, 0, 0 ) ) + 1
      ENDIF
      cKey := Input( oEdit,cKey,nPos )
      IF cKey != Nil
         SetGetUpdated( oEdit )
         IF Set( _SET_INSERT )
            IF oEdit:lPicComplex
               nGetLen := Len( oEdit:cPicMask )
               FOR nLen := 0 TO nGetLen
                  IF !IsEditable( oEdit,nPos+nLen )
                     Exit
                  ENDIF
               NEXT
               oEdit:title := Left( oEdit:title,nPos-1 ) + cKey + ;
                  Substr( oEdit:title,nPos,nLen-1 ) + Substr( oEdit:title,nPos+nLen )
            ELSE
               oEdit:title := Left( oEdit:title,nPos-1 ) + cKey + ;
                  Substr( oEdit:title,nPos )
            ENDIF

            IF !Empty( oEdit:cPicMask ) .AND. Len( oEdit:cPicMask ) < Len( oEdit:title )
               oEdit:title := Left( oEdit:title,nGetLen )
            ENDIF
         ELSE
            oEdit:title := Left( oEdit:title,nPos-1 ) + cKey + SubStr( oEdit:title,nPos+1 )
         ENDIF
         SetDlgItemText( oEdit:oParent:handle, oEdit:id, oEdit:title )
         // writelog( "GetApplyKey "+oEdit:title+str(nPos-1) )
         KeyRight( oEdit,nPos )
      ENDIF
   ENDIF
   oEdit:lFirst := .F.

Return 0

Static Function __When( oCtrl )
Local res

   oCtrl:Refresh()
   oCtrl:lFirst := .T.
   IF oCtrl:bGetFocus != Nil 
      res := Eval( oCtrl:bGetFocus, Eval( oCtrl:bSetGet ), oCtrl )
      IF !res
         GetSkip( oCtrl:oParent,oCtrl:handle,1 )
      ENDIF
      Return res
   ENDIF

Return .T.

Static Function __Valid( oCtrl )
Local vari, oDlg

   IF oCtrl:bSetGet != Nil
      oDlg := ParentGetDialog( oCtrl )
      IF oDlg:nLastKey != 27
         vari := UnTransform( oCtrl,GetEditText( oCtrl:oParent:handle, oCtrl:id ) )
         oCtrl:title := vari
         IF oCtrl:cType == "D"
            IF IsBadDate( vari )
               SetFocus( oCtrl:handle )
               Return .F.
            ENDIF
            vari := Ctod( vari )
         ELSEIF oCtrl:cType == "N"
            vari := Val( Ltrim( vari ) )
            oCtrl:title := Transform( vari, oCtrl:cPicFunc + Iif(Empty(oCtrl:cPicFunc),""," ") + oCtrl:cPicMask )
            SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
         ENDIF
         Eval( oCtrl:bSetGet,vari )
         oDlg:nLastKey := 27
         IF oCtrl:bLostFocus != Nil .AND. !Eval( oCtrl:bLostFocus, vari, oCtrl )
            SetFocus( oCtrl:handle )
            oDlg:nLastKey := 0
            Return .F.
         ENDIF
         oDlg:nLastKey := 0
      ENDIF
   ENDIF

Return .T.

Static function Untransform( oEdit,cBuffer )
Local xValue, cChar, nFor, minus

   IF oEdit:cType == "C"

      IF "R" $ oEdit:cPicFunc
         FOR nFor := 1 to Len( oEdit:cPicMask )
            cChar := SubStr( oEdit:cPicMask, nFor, 1 )
            IF !cChar $ "ANX9#!"
               cBuffer := SubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + SubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
         cBuffer := StrTran( cBuffer, Chr( 1 ), "" )
      endif

      xValue := cBuffer

   ELSEIF oEdit:cType == "N"
      minus := ( Left( Ltrim( cBuffer ),1 ) == "-" )
      cBuffer := Space( FirstEditable(oEdit) - 1 ) + SubStr( cBuffer, FirstEditable(oEdit), LastEditable(oEdit) - FirstEditable(oEdit) + 1 )

      IF "D" $ oEdit:cPicFunc
         FOR nFor := FirstEditable( oEdit ) to LastEditable( oEdit )
            IF !IsEditable( oEdit,nFor )
               cBuffer = Left( cBuffer, nFor-1 ) + Chr( 1 ) + SubStr( cBuffer, nFor+1 )
            ENDIF
         NEXT
      ELSE
         IF "E" $ oEdit:cPicFunc
            cBuffer := Left( cBuffer, FirstEditable(oEdit) - 1 ) +           ;
                        StrTran( SubStr( cBuffer, FirstEditable(oEdit),      ;
                           LastEditable(oEdit) - FirstEditable(oEdit) + 1 ), ;
                           ".", " " ) + SubStr( cBuffer, LastEditable(oEdit) + 1 )
            cBuffer := Left( cBuffer, FirstEditable(oEdit) - 1 ) +           ;
                        StrTran( SubStr( cBuffer, FirstEditable(oEdit),      ;
                           LastEditable(oEdit) - FirstEditable(oEdit) + 1 ), ;
                           ",", "." ) + SubStr( cBuffer, LastEditable(oEdit) + 1 )
         ELSE
            cBuffer := Left( cBuffer, FirstEditable(oEdit) - 1 ) +        ;
                        StrTran( SubStr( cBuffer, FirstEditable(oEdit),   ;
                        LastEditable(oEdit) - FirstEditable(oEdit) + 1 ), ;
                         ",", " " ) + SubStr( cBuffer, LastEditable(oEdit) + 1 )
         ENDIF

         FOR nFor := FirstEditable(oEdit) to LastEditable(oEdit)
            IF !IsEditable( oEdit,nFor ) .and. SubStr( cBuffer, nFor, 1 ) != "."
               cBuffer = Left( cBuffer, nFor-1 ) + Chr( 1 ) + SubStr( cBuffer, nFor+1 )
            ENDIF
         NEXT
      ENDIF

      cBuffer := StrTran( cBuffer, Chr( 1 ), "" )

      cBuffer := StrTran( cBuffer, "$", " " )
      cBuffer := StrTran( cBuffer, "*", " " )
      cBuffer := StrTran( cBuffer, "-", " " )
      cBuffer := StrTran( cBuffer, "(", " " )
      cBuffer := StrTran( cBuffer, ")", " " )

      cBuffer := PadL( StrTran( cBuffer, " ", "" ), Len( cBuffer ) )

      IF minus
         FOR nFor := 1 to Len( cBuffer )
            IF IsDigit( SubStr( cBuffer, nFor, 1 ) )
               exit
            ENDIF
         NEXT
         nFor--
         IF nFor > 0
            cBuffer := Left( cBuffer, nFor-1 ) + "-" + SubStr( cBuffer, nFor+1 )
         ELSE
            cBuffer := "-" + cBuffer
         ENDIF
      ENDIF

      xValue := cBuffer

   ELSEIF oEdit:cType == "L"

      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .or. "Y" $ cBuffer .or. hb_langmessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer

   ELSEIF oEdit:cType == "D"

      IF "E" $ oEdit:cPicFunc
         cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
      ENDIF
      xValue := cBuffer

   ENDIF

Return xValue

Static Function FirstEditable( oEdit )
Local nFor, nMaxLen := Len( oEdit:cPicMask )

   IF IsEditable( oEdit,1 )
      Return 1
   ENDIF

   FOR nFor := 2 to nMaxLen
      IF IsEditable( oEdit,nFor )
         Return nFor
      ENDIF
   NEXT

Return 0

Static Function  LastEditable( oEdit )
Local nFor, nMaxLen := Len( oEdit:cPicMask )

   FOR nFor := nMaxLen to 1 step -1
      IF IsEditable( oEdit,nFor )
         Return nFor
      ENDIF
   NEXT

Return 0

Static Function IsBadDate( cBuffer )
Local i, nLen

   IF !Empty( Ctod( cBuffer ) )
      Return .F.
   ENDIF
   nLen := len( cBuffer )
   FOR i := 1 to nLen
      If IsDigit( Substr( cBuffer,i,1 ) )
         Return .T.
      ENDIF
   NEXT
Return .F.

Function CreateGetList( oDlg )
Local i, j, aLen1 := Len( oDlg:aControls ), aLen2

   FOR i := 1 TO aLen1
      IF __ObjHasMsg( oDlg:aControls[i],"BSETGET" ) .AND. oDlg:aControls[i]:bSetGet != Nil
         Aadd( oDlg:GetList,oDlg:aControls[i] )
      ELSEIF !Empty(oDlg:aControls[i]:aControls)
         aLen2 := Len(oDlg:aControls[i]:aControls)
         FOR j := 1 TO aLen2
            IF __ObjHasMsg( oDlg:aControls[i]:aControls[j],"BSETGET" ) .AND. oDlg:aControls[i]:aControls[j]:bSetGet != Nil
               Aadd( oDlg:GetList,oDlg:aControls[i]:aControls[j] )
            ENDIF
         NEXT
      ENDIF
   NEXT
Return Nil

Function GetSkip( oParent,hCtrl,nSkip )
Local i, aLen

   DO WHILE oParent != Nil .AND. !__ObjHasMsg( oParent,"GETLIST" )
      oParent := oParent:oParent
   ENDDO
   IF oParent == Nil
      Return .F.
   ENDIF
   IF hCtrl == Nil
      i := 0
   ENDIF
   IF hCtrl == Nil .OR. ( i := Ascan( oParent:Getlist,{|o|o:handle==hCtrl} ) ) != 0
      IF nSkip > 0
         aLen := Len( oParent:Getlist )
         DO WHILE ( i := i+nSkip ) <= aLen
            IF !oParent:Getlist[i]:lHide .AND. oParent:Getlist[i]:ClassName() == "HEDIT"
               SetFocus( oParent:Getlist[i]:handle )
               Return .T.
            ENDIF
         ENDDO
      ELSE
         DO WHILE ( i := i+nSkip ) > 0
            IF !oParent:Getlist[i]:lHide .AND. oParent:Getlist[i]:ClassName() == "HEDIT"
               SetFocus( oParent:Getlist[i]:handle )
               Return .T.
            ENDIF
         ENDDO
      ENDIF
   ENDIF

Return .F.

Function SetGetUpdated( o )

   o:lChanged := .T.
   IF ( o := ParentGetDialog( o ) ) != Nil
      o:lUpdated := .T.
   ENDIF

Return Nil

Function ParentGetDialog( o )
   DO WHILE .T.
      o := o:oParent
      IF o == Nil
         EXIT
      ELSE
         IF __ObjHasMsg( o,"GETLIST" )
            EXIT
         ENDIF
      ENDIF
   ENDDO
Return o
