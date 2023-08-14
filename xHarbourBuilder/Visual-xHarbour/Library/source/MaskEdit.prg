/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MaskEdit.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include 'inkey.ch'
#Include 'vxh.ch'


#Define WM_CARET   WM_USER + 2551

CLASS MaskEdit INHERIT EditBox
   PROPERTY Picture      SET ::SetGetProp( "Picture", @v )
   PROPERTY Text         GET IIF( ::oGet != NIL .AND. ::oGet:VarGet() != NIL, (::oget:assign(),::oGet:VarGet()), ::xText );
                         SET IIF( ::oGet != NIL, ( ::oGet:VarPut(v), ::oGet:updatebuffer(), ::SetWindowText( ::oGet:Buffer ) ),  )

   DATA ReadOnly         EXPORTED INIT .F.
   DATA lInValid         EXPORTED INIT .F.
   DATA NoEdit           EXPORTED INIT .F.
   DATA NoOverStrike     EXPORTED INIT .F.
   DATA oGet             EXPORTED

   DATA Number           EXPORTED INIT .F.
   DATA CueBanner        PROTECTED

   DATA EnterNext        INIT .F.
   DATA Case             INIT 1

   ACCESS PreBlock       INLINE ::oGet:preblock
   ASSIGN PreBlock(b)    INLINE ::oGet:preblock  := b

   ACCESS PostBlock      INLINE ::oGet:postblock
   ASSIGN PostBlock(b)   INLINE ::oGet:postblock  := b

   METHOD SetGetProp()

   ACCESS VarGet         INLINE ::oGet:VarGet()

   ACCESS xCaption       INLINE ::xText
   ASSIGN xCaption(c)    INLINE ::xText := c

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnGetDlgCode()
   METHOD OnUserMsg()
   METHOD OnUndo()
   METHOD OnKeyDown()
   METHOD OnChar()
   METHOD OnSetFocus()
   METHOD OnKillFocus()
   METHOD OnPaste()
   METHOD OnCopy()
   METHOD OnClear( nwParam, nlParam )  INLINE ::OnCut( nwParam, nlParam )
   METHOD OnCut()
   METHOD VarPut( xVal ) INLINE Eval( ::oGet:Block, xVal ), ::oGet:updateBuffer()
   METHOD OnLButtonDown()

   METHOD __GoToNextControl()
   METHOD __Validate()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS MaskEdit
   ::__xCtrlName := "MaskEdit"
   ::Super:Init( oParent )
   ::WantReturn := .T.
   IF ::Parent:DesignMode
      AINS( ::Events, 1, {"Get System", {;
                                         { "Valid"   , "", "" },;
                                         { "When"    , "", "" } } }, .T. )
   ENDIF
   ::oGet := VXHGet()
   ::oGet:New( - 1, - 1, { | x | If( x == NIL, ::oGet:cargo, ::oGet:cargo := x ) } , '', ::Picture )
   ::bSetValue := {|xValue| ::Text := xValue, IIF( ValType(::bOnSetValue)=="B", Eval( ::bOnSetValue, Self, xValue ),) }
   ::bGetValue := {|| ::Text }
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS MaskEdit
   LOCAL aTextExt := ::Drawing:GetTextExtentPoint32( 'X' )

   DEFAULT ::xText TO SPACE( ::Width / aTextExt[1] )

   ::oGet:cargo   := ::Text
   ::oGet:Picture := ::Picture
   ::oGet:SetFocus()
   ::oGet:Clear   := .T.

   IF ::Picture == NIL .OR. ! ( '@K' $ ::Picture )
      ::oGet:Clear := .F.
   ENDIF

   Super:Create()

   ::NoEdit := (::GetWindowLong( GWL_STYLE ) & ES_READONLY) != 0

   ::SendMessage( EM_LIMITTEXT, MAX( LEN( TRANSFORM( ::Text, ::oGet:Picture ) ), LEN( ::oGet:buffer ) ) , 0 )

   IF ! ::DesignMode
      SetWindowText( ::hWnd, ::oGet:buffer )
   ENDIF
   IF ValType( ::oGet:preblock ) == 'B' .AND. ! eval( ::oGet:preblock, Self, .F. )
      EnableWindow( ::hWnd, .F. )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnGetDlgCode( msg ) CLASS MaskEdit
   LOCAL n, nRet
   IF ::InDataGrid .AND. msg != NIL .AND. msg:hwnd == ::hWnd .AND. msg:message == WM_KEYDOWN .AND. (msg:wParam IN {VK_RETURN,VK_TAB,VK_ESCAPE})
      RETURN ::Super:OnGetDlgCode( msg )
   ENDIF
   IF msg != NIL .AND. msg:hwnd == ::hWnd .AND. msg:message == WM_KEYDOWN
      ::LastKey := msg:wParam
      IF (msg:wParam IN {VK_RETURN,VK_UP,VK_DOWN})
         ::oGet:assign()
         ::oGet:updatebuffer()
         IF ::oGet:baddate()
            MessageBeep( MB_OK )
            ::oGet:buffer := dtoc(CTOD( "" ))
            ::oget:baddate(.F.)
         ENDIF
         SetWindowText( ::hWnd, ::oGet:buffer )
         ::__Validate()
         // default button
         IF msg:wParam == VK_RETURN .AND. ::IsValid
            IF ( n := HSCAN( ::Form:__hObjects, {|a,o| (a), o:__xCtrlName == "Button" .AND. o:IsWindowVisible() .AND. o:DefaultButton } ) ) > 0
               nRet := ExecuteEvent( "OnClick", HGetValueAt( ::Form:__hObjects, n ) )
               RETURN NIL
            ENDIF
         ENDIF

         IF ::IsValid
            ::__GoToNextControl( msg:wParam )
          ELSE
            SetFocus( ::hWnd )
         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   ::LastKey := 0

   IF ::wParam == VK_RETURN
      IF ! ::InDataGrid
         ::PostMessage( WM_KEYDOWN, VK_TAB, ::lParam )
      ENDIF
      RETURN DLGC_WANTALLKEYS
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnLButtonDown( nwParam, nlParam ) CLASS MaskEdit
   LOCAL oCtrl := ObjFromHandle( GetFocus() )
   Super:OnLButtonDown()
   IF oCtrl != NIL .AND. oCtrl:__xCtrlName == "MaskEdit" .AND. oCtrl:hWnd != ::hWnd
      oCtrl:__Validate()
      IF ! oCtrl:IsValid
         RETURN 0
       ELSE
         ::PostMessage( WM_LBUTTONUP, nwParam, nlParam )
      ENDIF
   ENDIF
RETURN NIL

METHOD __Validate() CLASS MaskEdit
   LOCAL lCaret
   IF ! ::lInValid
      ::lInValid := .T.
      IF ValType( ::oGet:postblock ) == "B" .OR. HGetPos( ::EventHandler, "Valid" ) > 0
         IF ( lCaret := IsCaret() )
            ::HideCaret()
         ENDIF
         ::Validating := TRUE
         ::IsValid := .T.
         IF ValType( ::oGet:postblock ) == "B"
            ::IsValid := eval( ::oGet:postblock, Self )
         ENDIF
         IF HGetPos( ::EventHandler, "Valid" ) > 0
            ::IsValid := ExecuteEvent( "Valid", Self )
            IF ValType(::IsValid) != "L"
               ::IsValid := .T.
            ENDIF
         ENDIF
         ::Validating := FALSE

         IF ! ::IsValid
            SetFocus( ::hWnd )
         ENDIF

         IF lCaret
            ::ShowCaret()
         ENDIF

         IF ::IsValid .AND. ::InDataGrid
            ::SendMessage( WM_KILLFOCUS )
            ::Parent:SetFocus()
         ENDIF

      ENDIF
      ::lInValid := .F.
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD SetGetProp( cProp, xVal ) CLASS MaskEdit
   IF cProp == "Picture" .AND. ::oGet != NIL
      ::oGet:Picture := xVal
   ENDIF
RETURN xVal

//-----------------------------------------------------------------------------------------------
METHOD OnUserMsg( hWnd, nMsg, nwParam ) CLASS MaskEdit
   DO CASE
      CASE nMsg == WM_CARET
           Resetcaret( Self, Set( _SET_INSERT ) .OR. ::NoOverStrike )
   ENDCASE
RETURN Super:OnUserMsg( hWnd, nMsg, nwParam )

//-----------------------------------------------------------------------------------------------
METHOD OnUndo() CLASS MaskEdit
   ::oGet:Undo()
   ::oGet:changed := FALSE
   ::oGet:UpdateBuffer()
   SetWindowText( ::hWnd, ::oGet:buffer )
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnKeyDown( nwParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, i, lShift, lCtrl
   IF ::ReadOnly
      RETURN(0)
   ENDIF

   IF nwParam == VK_DOWN .AND. ::DropCalendar
      RETURN Super:OnKeyDown( nwParam )
   ENDIF
//   IF ValType( ::OnWMKeyDown ) == 'B'
//      nRet := Eval( ::OnWMKeyDown, ::oGet, nwParam, nlParam )
//      IF nRet != NIL
//         RETURN nRet
//      ENDIF
//   ENDIF

   IF CHR( nwParam )$"UuZz" .AND. CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
      RETURN ::OnUndo( 0, 0 )
   ENDIF
   IF CHR( nwParam )$"Xx" .AND. CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
      RETURN ::OnCut( 0, 0 )
   ENDIF
   IF CHR( nwParam )$"Cc" .AND. CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
      RETURN ::OnCopy( 0, 0 )
   ENDIF
   IF CHR( nwParam )$"Vv" .AND. CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
      RETURN 0//::OnPaste( 0, 0 )
   ENDIF

   DO CASE
      CASE nwParam == 27
           IF ::InDataGrid
              ::lInValid := .T.
              ::IsValid := .F.
              ::Parent:SetFocus()
              RETURN 0
           ENDIF
           IF ::Parent:Modal // GetDlgItem( ::Parent:hWnd, IDCANCEL ) # 0
              ::Parent:PostMessage( WM_COMMAND, IDCANCEL, 0 )
              RETURN 0
           ENDIF

      CASE nwParam == 110 .OR. nwParam == 190
           RETURN 0

      CASE ( nwParam == K_INS .OR. nwParam == VK_INSERT )
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
           lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
           IF ! lShift .AND. ! lCtrl
              Set( _SET_INSERT, ! Set( _SET_INSERT ) )
              ResetCaret( Self, Set( _SET_INSERT ) .OR. ::NoOverStrike )
            ELSE
              RETURN ::SendMessage( WM_PASTE, 0, 0 )
           ENDIF
           RETURN 0

      CASE nwParam == VK_END
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
           lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
           IF lCtrl
              RETURN 0
           ENDIF
           nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) )
           nEnd   := Len( Trim( ::oGet:Buffer ) )
           IF !lShift
              nEnd  -= IIF( ::oGet:Type == "N", 1, 0 )
              nStart := nEnd
           ENDIF
           ::SendMessage( EM_SETSEL, nStart, nEnd  )
           ::ScrollCaret()
           RETURN 0

      CASE nwParam == VK_LEFT
           nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
           nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )

           IF nStart != nEnd .AND. !lShift
              ::oget:pos := nStart
              ::SendMessage( EM_SETSEL, ( ::oGet:pos ) , ( ::oGet:pos )  )
           ENDIF

      CASE nwParam == VK_RIGHT
           nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
           nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
           lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )

           IF nStart != nEnd .AND. !lShift
              ::oget:pos := nEnd-1
              ::SendMessage( EM_SETSEL, ( ::oGet:pos -1) , ( ::oGet:pos-1 )  )
           ENDIF

      CASE nwParam == VK_DELETE
           IF ::IsWindowEnabled() .AND.  !::NoEdit
              nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
              nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
              ::oGet:pos := nEnd

              IF nStart # nEnd
                 IF nEnd > Len( ::oGet:buffer )
                    ::oGet:delete()
                 ENDIF

                 FOR i := nStart To nEnd
                    IF ::oGet:pos > nStart
                       ::oGet:backSpace()
                     ELSE
                       EXIT
                    ENDIF
                 NEXT
               ELSE
                 IF ::oGet:Editable( ::oGet:Pos )
                    ::oGet:delete()
                  ELSE
                    ::oGet:Right()
                 ENDIF
              ENDIF
              SetWindowText( ::hWnd, ::oGet:buffer )
              //::Text := ::oGet:buffer

              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()
              RETURN 0
           ENDIF
   ENDCASE
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD __GoToNextControl( nKey ) CLASS MaskEdit
   LOCAL hNext
   IF (nKey IN { VK_UP, VK_DOWN, VK_RETURN })
      IF ::Form:Modal
         PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, IIF( nKey == VK_UP, 1, 0 ), 0 )
       ELSEIF ( hNext := GetNextDlgTabItem( ::Form:hWnd, ::hWnd, nKey == VK_UP ) ) > 0
         SetFocus( hNext )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnChar( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, i, bChanged

   IF nwParam==27 .OR. ::Validating
      RETURN 0
   ENDIF
   IF ::ReadOnly
      RETURN 0
   ENDIF

   IF ::IsWindowEnabled()
      nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      ::oGet:pos := nEnd


      DO CASE
         CASE ( nwParam == VK_BACK .AND. nlParam # 0 )
              IF nEnd > nStart
                 IF nEnd > Len( ::oGet:Buffer )
                    ::oGet:delete()
                 ENDIF
                 For i := nStart To nEnd
                     IF ::oGet:pos > nStart
                        ::oGet:backSpace()
                      ELSE
                        EXIT
                     ENDIF
                 NEXT
               ELSE
                 ::oGet:backspace()
              ENDIF
              SetWindowText( ::hWnd, ::oGet:buffer )
              //::Text := ::oGet:buffer
              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()

              IF ! ::ReadOnly
                 IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
                    bChanged := ::Parent:bChanged
                 ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
                    bChanged := ::Form:bChanged
                 ENDIF
                 IF bChanged != NIL
                    ::CallWindowProc()
                    Eval( bChanged, Self )
                 ENDIF
              ENDIF
              RETURN 0

         CASE nwParam >= 32 .AND. nwParam < 256
              IF ( ::oGet:type == "N" .OR. ::oGet:type == "D" ) .AND. !UPPER( CHR( nwParam ) )$"0123456789-."
                 RETURN 0
              ENDIF
              IF !::oGet:changed .AND. ::oGet:type == "N"
                 IF ::oGet:pos == 1
                    nEnd := LEN( ::oGet:Buffer ) + 1
                    ::oGet:pos := nEnd
                 ENDIF
              ENDIF

              IF nStart # nEnd
                 IF nEnd > LEN( ::oGet:Buffer )
                    ::oGet:delete()
                 ENDIF
                 FOR i := nStart To nEnd // clear selection by backspacing
                     IF ::oGet:pos > nStart
                        ::oGet:backSpace()
                      ELSE
                        EXIT
                     ENDIF
                 NEXT
              ENDIF

              IF ::oGet:pos == 1
                 IF !::oGet:changed .AND. ( Set( _SET_INSERT ) .OR. ::oGet:type == "N" )

                    IF ::oGet:type == "N" .OR. AT("K", UPPER( ::oGet:picture ) ) > 0
                       DO CASE
                          CASE ::oGet:type == "N"
                               ::oGet:buffer := ""
                               ::oGet:minus  := .F.

                          CASE ::oGet:Type == "C"
                               ::oGet:buffer := SPACE( LEN( ::oGet:buffer ) )

                          CASE ::oGet:Type == "D"
                               //::oGet:buffer := CTOD( "" )
                       ENDCASE
                       ::oGet:Assign()
                       ::oGet:UpdateBuffer()
                    ENDIF
                 ENDIF
                 ::oGet:home()
              ENDIF

              IF ::oGet:type == "N" .AND. nwParam == 46
                 ::oGet:toDecPos()
               ELSE
                 IF Set( _SET_INSERT ) .OR. ::NoOverStrike
                    ::oGet:insert( chr( nwParam ) )
                  ELSE
                    ::oGet:overstrike( chr( nwParam ) )
                 ENDIF
                 IF ::oGet:Rejected
                    MessageBeep( MB_OK )
                 ENDIF
              ENDIF

              SetWindowText( ::hWnd, ::oGet:buffer )
              //::Text := ::oGet:buffer
              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()

              IF ! ::ReadOnly
                 IF ::Parent:HasMessage( "bChanged" ) .AND. ::Parent:bChanged != NIL
                    bChanged := ::Parent:bChanged
                 ELSEIF ::Form != NIL .AND. ::Form:HasMessage( "bChanged" ) .AND. ::Form:bChanged != NIL
                    bChanged := ::Form:bChanged
                 ENDIF
                 IF bChanged != NIL
                    ::CallWindowProc()
                    Eval( bChanged, Self )
                 ENDIF
              ENDIF
              RETURN 0

      ENDCASE
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnSetFocus() CLASS MaskEdit
   LOCAL lShift, h, nStart, nEnd, coldbuff

   ::NoEdit := (::GetWindowLong( GWL_STYLE ) & ES_READONLY) != 0

   IF HGetPos( ::EventHandler, "When" ) != 0 .AND. !::NoEdit
      ::NoEdit := !ExecuteEvent( "When", Self )
   ENDIF

   IF !::IsWindowEnabled() .OR. ::NoEdit

      lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
      IF ( h := GetNextDlgTabItem( ::Parent:hWnd, ::hWnd, lShift ) ) # 0
         SetFocus( h )
         RETURN 0
      ENDIF

   ENDIF


   nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) )
   nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) )

   nStart := min( nStart, Len( Trim( ::oGet:buffer ) ) )
   nEnd   := min( nEnd,   Len( Trim( ::oGet:buffer ) ) )

   coldbuff := ::oGet:buffer

   IF !( coldbuff == ::oGet:buffer )
      IF ::oGet:BadDate()
         ::Text := CTOD( "" )
         ::oget:baddate(.F.)
      Else
         SetWindowText( ::hWnd, ::oGet:buffer )
         //::Text := ::oGet:buffer
      ENDIF
   ENDIF

   ::InvalidateRect()
   ::PostMessage( WM_CARET, 0, 0 )
   ::oGet:pos      := 1
   ::oGet:changed  := .F.
   ::oGet:original := ::oGet:VarGet()
   ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 )  )
   ::SendMessage( WM_MOUSEMOVE, 1, 1 )
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnKillFocus( nwParam, nlParam ) CLASS MaskEdit
   ::oget:assign()
   ::oget:updatebuffer()

   IF ! ::Validating .AND. nwParam <> 0 .AND. ! ::InDataGrid .AND. ( ::Application:MaskEditKillFocusValid .OR. nlParam == NIL .OR. ::LastKey == VK_TAB )
      IF ::oGet:Type == "N" .AND. ::oGet:VarGet() == 0
         ::oget:VarPut( 0 )
      ENDIF
      IF ::oget:baddate()
         ::oget:baddate(.F.)
         MessageBeep( MB_OK )
         ::oGet:buffer := DTOC(CTOD( "" ))
         ::oget:assign()
         ::oget:updatebuffer()
      ENDIF
      SetWindowText( ::hWnd, ::oGet:buffer )
      ::InvalidateRect()
      ::SendMessage( WM_MOUSEMOVE, 1, 1 )

      ::LastKey := 0

      IF ::DropCalendar .AND. ::CalendarWindow != NIL .AND. ::CalendarWindow:IsWindow()
         RETURN NIL
      ENDIF

      ::__Validate()

      IF ! ::IsValid
         ::SetFocus()
         RETURN 0
      ENDIF
   ENDIF
RETURN Super:OnKillFocus( nwParam, nlParam )

//-----------------------------------------------------------------------------------------------

METHOD OnPaste( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, cText, i, cChar
   (nwParam, nlParam)
   IF ::IsWindowEnabled() .AND. !::NoEdit .AND. !::ReadOnly
      nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      IF ::OpenClipboard()
         IF ( cText := GetClipboardData( CF_TEXT ) ) <> NIL
            cText := STRTRAN( cText, CHR(0) )
            IF nStart # nEnd
               ::oget:pos := nEnd
               FOR i := nStart To nEnd // clear selection by backspacing
                  IF ::oget:pos > nStart
                     ::oget:backSpace()
                   ELSE
                     EXIT
                  ENDIF
               NEXT
            ENDIF

            FOR i := 1 To LEN( cText )
               cChar := SubStr( cText, i, 1 )
               IF ::oget:type == "N"

                  IF cChar == "," .AND. AT( "E", ::oget:picture ) == 0
                     LOOP
                  ENDIF
                  IF cChar == "." .AND. AT( "E", ::oget:picture ) > 0
                     LOOP
                  ENDIF

                  IF cChar == "." .AND. AT( "E", ::oget:picture ) == 0
                     ::oget:toDecPos()
                  ENDIF
                  IF cChar == "," .AND. AT( "E", ::oget:picture ) > 0
                     ::oget:toDecPos()
                  ENDIF

               ENDIF

               IF Set( _SET_INSERT ) .OR. ::NoOverStrike
                  ::oget:insert( cChar )
                ELSE
                  ::oget:overstrike( cChar )
               ENDIF
               IF ::oget:Rejected
                  MessageBeep( MB_OK )
               ENDIF
            NEXT

            SetWindowText( ::hWnd, ::oGet:buffer )
            ::SendMessage( EM_SETSEL, ( ::oget:pos - 1 ) , ( ::oget:pos - 1 ) )

         ENDIF
         CloseClipboard()
      ENDIF
   ENDIF
RETURN 0

//-----------------------------------------------------------------------------------------------

METHOD OnCopy( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, ctempbuff
   IF ::IsWindowEnabled() .AND. !::NoEdit .AND. !::ReadOnly
      nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      ::oget:pos := nEnd

      ctempbuff := ::oget:buffer
      CallWindowProc( ::__nproc, ::hWnd, WM_COPY, nwParam, nlParam )
    ELSE
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnCut( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, i, ctempbuff, retval
   IF ::IsWindowEnabled() .AND. !::NoEdit .AND. !::ReadOnly
      nStart := LoWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      nEnd   := HiWord( ::SendMessage( EM_GETSEL, 0, 0 ) ) + 1
      ::oget:pos := nEnd

      IF nStart # nEnd
         IF nEnd > Len( ::oget:buffer )
            ::oget:delete()
         ENDIF

         FOR i := nStart To nEnd
            IF ::oget:pos > nStart
               ::oget:backSpace()
             ELSE
               EXIT
            ENDIF
         NEXT
       ELSE
         ::oget:delete()
      ENDIF
      ctempbuff := ::oget:buffer
      retval := CallWindowProc( ::__nproc, ::hWnd, WM_CUT, nwParam, nlParam )
      ::Text := ctempbuff
      ::SendMessage( EM_SETSEL, ( ::oget:pos - 1 ) , ( ::oget:pos - 1 ) )
    ELSE
      RETURN 0
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

FUNCTION ResetCaret( oWnd, lInsert )
   LOCAL cText := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   oWnd:GetClientRect()
   oWnd:HideCaret()
   oWnd:DestroyCaret()

   oWnd:CreateCaret( 0, IIF( lInsert, 1, 7 ), oWnd:Drawing:GetTextExtentPoint32( cText )[2] )
   oWnd:ShowCaret()
RETURN NIL

//-----------------------------------------------------------------------------------------------

CLASS VXHGet INHERIT Get
   METHOD Editable( nPos ) INLINE ::IsEditable( nPos )
ENDCLASS
