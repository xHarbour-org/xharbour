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


#Define WM_INVALID WM_USER + 2550
#Define WM_CARET   WM_USER + 2551

CLASS MaskEdit INHERIT EditBox

   DATA OnCharacter  EXPORTED
   DATA OnKey        EXPORTED
   DATA ReadOnly     EXPORTED  INIT .F.
   DATA lInValid     EXPORTED  INIT .F.
   DATA NoEdit       EXPORTED  INIT .F.
   DATA NoOverStrike EXPORTED  INIT .F.
   DATA oGet EXPORTED
   DATA  __Validate  EXPORTED INIT .F.

   ACCESS PreBlock       INLINE ::oGet:preblock
   ASSIGN PreBlock(b)    INLINE ::oGet:preblock  := b

   ACCESS PostBlock       INLINE ::oGet:postblock
   ASSIGN PostBlock(b)    INLINE ::oGet:postblock  := b

   ACCESS Picture    INLINE ::oGet:picture
   ASSIGN Picture(c) INLINE ::oGet:picture   := c


   PROPERTY Picture         INDEX "Picture"             READ xPicture         WRITE SetGetProp     DEFAULT NIL PROTECTED

   METHOD SetGetProp()

   ACCESS VarGet     INLINE ::oGet:VarGet()

   ACCESS Caption    INLINE IIF( ::oGet != NIL, ::oGet:VarGet(), ::xCaption ) PERSISTENT
   ASSIGN Caption(c) INLINE IIF( ::oGet != NIL, ( ::oGet:VarPut(c), ::oGet:updatebuffer(), ::SetWindowText( ::oGet:Buffer ) ), ::xCaption := c )

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
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS MaskEdit
   ::__xCtrlName := "MaskEdit"
   ::Super:Init( oParent )
   ::WantReturn := .T.
   IF ::Parent:__ClassInst != NIL
      AINS( ::Events, 1, {"Get System", {;
                                         { "Valid"   , "", "" },;
                                         { "When"    , "", "" } } }, .T. )
   ENDIF
   ::oGet := VXHGet()
   ::oGet:New( - 1, - 1, { | x | If( x == NIL, ::oGet:cargo, ::oGet:cargo := x ) } , '', ::Picture )
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetGetProp( cProp, xVal ) CLASS MaskEdit
   IF cProp == "Picture" .AND. ::oGet != NIL
      ::oGet:Picture := xVal
   ENDIF
RETURN xVal

//-----------------------------------------------------------------------------------------------

METHOD Create() CLASS MaskEdit
   LOCAL aTextExt := ::Drawing:GetTextExtentPoint32( 'X' )

   IF ::Caption == NIL
      ::Caption := SPACE( ::Width / aTextExt[1] )
   ENDIF
   ::oGet:cargo   := ::Caption
   ::oGet:Picture := ::Picture
   ::oGet:SetFocus()
   ::oGet:Clear   := .T.

   IF ::Picture == NIL .OR. ! ( '@K' $ ::Picture )
      ::oGet:Clear := .F.
   ENDIF

   Super:Create()

   ::NoEdit := ::GetWindowLong( GWL_STYLE ) & ES_READONLY != 0

   ::SendMessage( EM_LIMITTEXT, MAX( LEN( TRANSFORM( ::Caption, ::oGet:Picture ) ), LEN( ::oGet:buffer ) ) , 0 )

   IF ::__ClassInst == NIL
      SetWindowText( ::hWnd, ::oGet:buffer )
      //::Caption := ::oGet:buffer
   ENDIF
   IF ValType( ::oGet:preblock ) == 'B' .AND. ! eval( ::oGet:preblock, Self, .F. )
      EnableWindow( ::hWnd, .F. )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnGetDlgCode() CLASS MaskEdit
   LOCAL nRet
//   IF ::wParam > 0
      nRet := DLGC_WANTMESSAGE | DLGC_WANTALLKEYS
//   ENDIF
RETURN nRet


//-----------------------------------------------------------------------------------------------
METHOD OnUserMsg( hWnd, nMsg, nwParam, nlParam ) CLASS MaskEdit
   LOCAL coldbuff, h
   DO CASE
      CASE nMsg == WM_CARET
           Resetcaret( Self, Set( _SET_INSERT ) .OR. ::NoOverStrike )

      CASE nMsg == WM_INVALID
           IF isChildOfActiveWindow( hWnd ) .AND. GetDlgCtrlID( nwParam ) <> IDCANCEL
              IF !::lInValid .AND. ::__Validate
                 ::lInValid := .T.
                 IF ::__Validate
                    ::__Validate := .F.
                 ENDIF
                 IF ValType( ::oGet:postblock ) == "B" .OR. HGetPos( ::EventHandler, "Valid" ) != 0
                    coldbuff := ::oGet:buffer
                    h:=GetFocus()
                    HideCaret(h)
                    ::Validating := TRUE
                    ::IsValid := .T.
                    IF ValType( ::oGet:postblock ) == "B"
                       ::IsValid := eval( ::oGet:postblock, Self )
                    ENDIF
                    IF HGetPos( ::EventHandler, "Valid" ) != 0
                       ::IsValid := ExecuteEvent( "Valid", Self )
                    ENDIF
                    ::Validating := FALSE


                    IF !::IsValid
                       SetFocus( ::hWnd )
                     ELSE
                       ShowCaret(h)
                    ENDIF
                    IF !( coldbuff == ::oGet:buffer )
                       SetWindowText( ::hWnd, ::oGet:buffer )
                       //::Caption := ::oGet:buffer
                    ENDIF

                    IF !::IsValid
                       ShowCaret(h)
                    ENDIF

                    IF ::IsValid.AND. ::Parent:ClsName == "DataGrid"
                       ::SendMessage( WM_KILLFOCUS )
                       ::Parent:SetFocus()
                    ENDIF

                 ENDIF
                 ::lInValid := .F.
              ENDIF
           ENDIF

   ENDCASE
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnUndo( nwParam, nlParam ) CLASS MaskEdit
   ::oGet:Undo()
   ::oGet:changed := FALSE
   ::oGet:UpdateBuffer()
   SetWindowText( ::hWnd, ::oGet:buffer )
   //::Caption := ::oGet:buffer
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnKeyDown( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, i, lShift, lCtrl, cOldBuff, h, oChild, nCur, nPos
   IF ::ReadOnly
      RETURN(0)
   ENDIF

   IF ValType( ::OnKey ) == 'B'
      IF !eval( ::OnKey, ::oGet, ::hWnd, nwParam, nlParam )
         RETURN 0
      ENDIF
   ENDIF

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
      CASE ( nwParam == 40 .OR. nwParam == 38 )
           IF ValType( ::oGet:postblock ) == "B" .OR. HGetPos( ::EventHandler, "Valid" ) != 0
              IF !::lInValid
                 ::oGet:assign()
                 ::oGet:updatebuffer()
                 IF ::oGet:baddate()
                    MessageBeep( MB_OK )
                    ::oGet:buffer:=dtoc(ctod( "" ))
                 ENDIF
                 SetWindowText( ::hWnd, ::oGet:buffer )
                 //::Caption := ::oGet:buffer
                 cOldBuff := ::oGet:buffer
                 h:=GetFocus()
                 HideCaret(h)
                 ::Validating := .T.
                 ::IsValid := .T.
                 IF ValType( ::oGet:postblock ) == "B"
                    ::IsValid := eval( ::oGet:postblock, Self )
                 ENDIF
                 IF HGetPos( ::EventHandler, "Valid" ) != 0
                    ::IsValid := ExecuteEvent( "Valid", Self )
                 ENDIF
                 ::Validating := .F.
                 IF !::IsValid
                    ::SetFocus()
                  ELSE
                    ShowCaret(h)
                 ENDIF
                 IF !( cOldBuff == ::oGet:buffer )
                    SetWindowText( ::hWnd, ::oGet:buffer )
                    //::Caption := ::oGet:buffer
                 ENDIF
                 ::InvalidateRect()
                 IF !::IsValid
                    ShowCaret(h)
                    RETURN 0
                 ENDIF
                 IF ::Parent:ClsName == "DataGrid"
                    ::lInvalid := .T.
                    ::SendMessage( WM_KILLFOCUS )
                    ::Parent:SetFocus()
                 ENDIF
              ENDIF
           ENDIF

           nCur := ASCAN( ::Parent:Children, {|o|o:hWnd == ::hWnd} ) + IIF( nwParam == 38, -1, 1 )
           FOR nPos := nCur TO IIF( nwParam == 38, 1, LEN( ::Parent:Children ) ) STEP IIF( nwParam == 38, -1, 1 )
               IF ::Parent:Children[nPos]:__xCtrlName == "MaskEdit" .AND. ::Parent:Children[nPos]:IsWindowEnabled() .AND. !::Parent:Children[nPos]:NoEdit .AND. ::Parent:Children[nPos]:IsWindowVisible()
                  ::Parent:Children[nPos]:SetFocus()
                  RETURN NIL
               ENDIF
           NEXT
           IF nwParam == 40
              IF !::Parent:ClsName == "DataGrid"
                 IF ( nPos := ASCAN( ::Parent:Children, {|o|o:ClsName == "MaskEdit" .AND. o:oGet != NIL } ) ) > 0
                    ::Parent:Children[nPos]:SetFocus()
                 ENDIF
               ELSE
                 ::Parent:OnKeyDown(VK_DOWN)
              ENDIF
            ELSE
              IF nwParam == 38 .AND. ::Parent:ClsName == "DataGrid"
                 ::Parent:OnKeyDown(VK_UP)
              ENDIF

              nCur := ASCAN( ::Parent:Children, {|o|o:hWnd == ::hWnd} )
              FOR nPos := LEN( ::Parent:Children ) TO nCur + 1 STEP -1
                  IF ::Parent:Children[nPos]:__xCtrlName == "MaskEdit" .AND.;
                     Valtype( ::Parent:Children[nPos]:oGet ) == "O" .AND.;
                     ::Parent:Children[nPos]:IsWindowEnabled() .AND.;
                     !::Parent:Children[nPos]:NoEdit .AND.;
                     ::Parent:Children[nPos]:IsWindowVisible()
                     ::Parent:Children[nPos]:SetFocus()
                     RETURN NIL
                  ENDIF
              NEXT
           ENDIF

      CASE ( nwParam == 13 .OR. nwParam == 9 )
           IF ValType( ::oGet:postblock ) == 'B' .OR. HGetPos( ::EventHandler, "Valid" ) != 0
              IF !::lInValid
                 ::oGet:assign()
                 ::oGet:updatebuffer()
                 IF ::oGet:baddate()
                    MessageBeep( MB_OK )
                    ::oGet:buffer := dtoc(CTOD( "" ))
                 ENDIF
                 SetWindowText( ::hWnd, ::oGet:buffer )
                 //::Caption := ::oGet:buffer
                 ::__Validate := .T.
                 ::SendMessage( WM_INVALID, nwParam, 0 )
                 ::InvalidateRect()
              ENDIF
            ELSEIF ::Parent:ClsName == "DataGrid"
              ::Parent:SetFocus()
           ENDIF
           RETURN 0

      CASE nwParam == 27
           IF ::Parent:ClsName == "DataGrid"
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
              //::Caption := ::oGet:buffer

              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()
              RETURN 0
           ENDIF
   ENDCASE

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnChar( nwParam, nlParam ) CLASS MaskEdit
   LOCAL lShift, h, nStart, nEnd, i, nLen

   IF nwParam==27
      RETURN 0
   ENDIF
   IF ::ReadOnly
      RETURN 0
   ENDIF

   IF ValType( ::OnCharacter ) == 'B'
      IF ! eval( ::OnCharacter, ::oGet, ::hWnd, nwParam, nlParam )
         RETURN 0
      ENDIF
   ENDIF

   IF ( nwParam==13 .or. nwParam==9 ) .AND. ::IsValid
      lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
      IF ( h := GetNextDlgTabItem( ::Form:hWnd, ::hWnd, lShift ) ) # 0
         IF ::Form:Modal
            PostMessage( ::Form:hWnd, WM_NEXTDLGCTL, h, MAKELPARAM( 1, 0 ) )
          ELSE
            SetFocus(h)
         ENDIF
         RETURN 0
      ENDIF

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
              //::Caption := ::oGet:buffer
              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()
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
                               ::oGet:buffer := CTOD( "" )
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
              //::Caption := ::oGet:buffer
              ::SendMessage( EM_SETSEL, ( ::oGet:pos - 1 ) , ( ::oGet:pos - 1 ) )
              ::ScrollCaret()
              RETURN 0

      ENDCASE
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnSetFocus() CLASS MaskEdit
   LOCAL lShift, h, nStart, nEnd, i, nLen, coldbuff, nCur, nPos, nLas

   ::NoEdit := ::GetWindowLong( GWL_STYLE ) & ES_READONLY != 0

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
         ::Caption := CTOD( "" )
      Else
         SetWindowText( ::hWnd, ::oGet:buffer )
         //::Caption := ::oGet:buffer
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

METHOD OnKillFocus( nwParam ) CLASS MaskEdit
   IF ::oGet:Type == "N" .AND. ::oGet:VarGet() == 0
      ::oget:VarPut( 0 )
   ENDIF

   ::oget:assign()
   ::oget:updatebuffer()

   IF ::oget:baddate()
      MessageBeep( MB_OK )
      ::oget:buffer:=dtoc(ctod( "" ))
   ENDIF
   SetWindowText( ::hWnd, ::oGet:buffer )
   //::Caption := ::oget:buffer
   IF ::__Validate
      IF ( ValType( ::oget:postblock ) == 'B' .OR. HGetPos( ::EventHandler, "Valid" ) != 0 ) .AND. !::lInValid .AND. ::TabValidate
         ::PostMessage( WM_INVALID, nwParam, 0 )
      ENDIF
    ELSE
      ::__Validate := .F.
   ENDIF
   ::InvalidateRect()
   ::SendMessage( WM_MOUSEMOVE, 1, 1 )

RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD OnPaste( nwParam, nlParam ) CLASS MaskEdit
   LOCAL nStart, nEnd, cText, i, cChar
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
   LOCAL nStart, nEnd, i, ctempbuff, retval
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
      ::Caption := ctempbuff
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
