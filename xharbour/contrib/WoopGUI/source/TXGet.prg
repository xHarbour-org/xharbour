/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "winuser.ch"

#include "getexit.ch"
#include "inkey.ch"

#define	NEW_STR(c)	((c) + "")

#define SYNC_EDIT

// Windows definitions
CLASS TXGet FROM TEdit
    // Base

    DATA oGet            AS OBJECT  HIDDEN

    DATA nCurrentProc           AS NUMERIC HIDDEN
    DATA nPreviousProc   AS NUMERIC HIDDEN
    DATA nMsg            AS NUMERIC HIDDEN
    DATA nwParam         AS NUMERIC HIDDEN
    DATA nlParam         AS NUMERIC HIDDEN

    DATA cExtendedType   AS STRING
    DATA bDisplay        AS CODEBLOCK
    DATA bMeaning        AS CODEBLOCK
    DATA bHelp           AS CODEBLOCK
    // DATA bAction         AS CODEBLOCK   // Come from window parent class
    DATA cMessage        AS STRING
    DATA oField          AS OBJECT
    DATA aData           AS ARRAY
    DATA cDefault        AS STRING
    DATA nKey            AS NUMERIC
    DATA cLetter         AS STRING
    DATA nLetterPos      AS NUMERIC
    DATA lAutoSize       AS LOGICAL INIT FALSE

    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD Init()

    METHOD Cancel()                            ;
           INLINE ; // iif( Set(_SET_ESCAPE), ..., ...)
                  ::oGet:Undo(),                   ;
                  ::oGet:ExitState := GE_ESCAPE,           ;
                  .t.


    // -------------------------- From Get class ------------------------------------
    ACCESS BadDate                      INLINE ::oGet:BadDate
    ACCESS Buffer                       INLINE ::oGet:Buffer
    ACCESS Cargo                        INLINE ::oGet:Cargo
    ACCESS Changed                      INLINE ::oGet:Changed
    ACCESS Clear                        INLINE ::oGet:Clear
    ACCESS Col                          INLINE ::oGet:Col
    ACCESS DecPos                       INLINE ::oGet:DecPos
    ACCESS ExitState                    INLINE ::oGet:ExitState
    ACCESS HasFocus                     INLINE ::oGet:HasFocus
    ACCESS Minus                        INLINE ::oGet:Minus
    ACCESS Name                         INLINE ::oGet:Name
    ACCESS Original                     INLINE ::oGet:Original
    ACCESS Pos                          INLINE ::oGet:Pos
    ACCESS PostBlock                    INLINE ::oGet:PostBlock
    ACCESS PreBlock                     INLINE ::oGet:PreBlock
    ACCESS Reader                       INLINE ::oGet:Reader
    ACCESS Rejected                     INLINE ::oGet:Rejected
    ACCESS Row                          INLINE ::oGet:Row
    ACCESS SubScript                    INLINE ::oGet:SubScript
    ACCESS Type                         INLINE ::oGet:Type
    ACCESS TypeOut                      INLINE ::oGet:TypeOut
    #ifdef HB_COMPAT_C53
    ACCESS Control                      INLINE ::oGet:Control
    ACCESS Message                      INLINE ::oGet:Message
    #endif

    ASSIGN BadDate( x )                 INLINE ::oGet:BadDate    := x
    ASSIGN Buffer( x )                  INLINE ::oGet:Buffer     := x
    ASSIGN Cargo( x )                   INLINE ::oGet:Cargo      := x
    ASSIGN Changed( x )                 INLINE ::oGet:Changed    := x
    ASSIGN Clear( x )                   INLINE ::oGet:Clear      := x
    ASSIGN Col( x )                     INLINE ::oGet:Col        := x
    ASSIGN DecPos( x )                  INLINE ::oGet:DecPos     := x
    ASSIGN ExitState( x )               INLINE ::oGet:ExitState  := x
    ASSIGN HasFocus( x )                INLINE ::oGet:HasFocus   := x
    ASSIGN Minus( x )                   INLINE ::oGet:Minus      := x
    ASSIGN Name( x )                    INLINE ::oGet:Name       := x
    ASSIGN Original( x )                INLINE ::oGet:Original   := x
    ASSIGN Pos( x )                     INLINE ::oGet:Pos        := x
    ASSIGN PostBlock( x )               INLINE ::oGet:PostBlock  := x
    ASSIGN PreBlock( x )                INLINE ::oGet:PreBlock   := x
    ASSIGN Reader( x )                  INLINE ::oGet:Reader     := x
    ASSIGN Rejected( x )                INLINE ::oGet:Rejected   := x
    ASSIGN Row( x )                     INLINE ::oGet:Row        := x
    ASSIGN SubScript( x )               INLINE ::oGet:SubScript  := x
    ASSIGN Type( x )                    INLINE ::oGet:Type       := x
    ASSIGN TypeOut( x )                 INLINE ::oGet:TypeOut    := x
    #ifdef HB_COMPAT_C53
    ASSIGN Control( x )                 INLINE ::oGet:Control    := x
    ASSIGN Message( x )                 INLINE ::oGet:Message    := x
    #endif


    METHOD Display()
    METHOD ColorDisp( cColorSpec )     INLINE ::oGet:ColorDisp( cColorSpec ), Self
    METHOD ParsePict( cPicture )       INLINE ::oGet:ParsePict( cPicture )
    METHOD Assign()                    INLINE ::oGet:Assign(), Self
    METHOD End()                       INLINE ::oGet:End(), Self
    METHOD Home()                      INLINE ::oGet:Home(), Self
    METHOD Reset()                     INLINE ::oGet:Reset(), Self
    METHOD Undo()                      INLINE ::oGet:Undo(), Self
    METHOD SetFocus()                  INLINE ::oGet:SetFocus(), Self
    METHOD KillFocus()                 INLINE ::oGet:KillFocus(), Self
    METHOD VarPut( xValue, lReFormat ) INLINE ::oGet:VarPut( xValue, lReFormat )
    METHOD VarGet()                    INLINE ::oGet:VarGet()
    METHOD Untransform( cBuffer )      INLINE ::oGet:Untransform( cBuffer )
    METHOD UpdateBuffer()              INLINE ::oGet:UpdateBuffer(), Self
    METHOD overstrike( cChar )         INLINE ::oGet:overstrike( cChar ), Self
    METHOD Insert( cChar )             INLINE ::oGet:Insert( cChar ), Self
    METHOD Right( lDisplay )           INLINE ::oGet:Right( lDisplay ), Self
    METHOD Left( lDisplay )            INLINE ::oGet:Left( lDisplay ), Self
    METHOD WordLeft()                  INLINE ::oGet:WordLeft(), Self
    METHOD WordRight()                 INLINE ::oGet:WordRight(), Self
    METHOD ToDecPos()                  INLINE ::oGet:ToDecPos(), Self
    METHOD IsEditable( nPos )          INLINE ::oGet:IsEditable( nPos )
    METHOD Input( cChar )              INLINE ::oGet:Input( cChar )
    METHOD PutMask( xValue, lEdit )    INLINE ::oGet:PutMask( xValue, lEdit )
    METHOD BackSpace( lDisplay )       INLINE ::oGet:BackSpace( lDisplay ), Self
    METHOD Delete( lDisplay )          INLINE ::oGet:Delete( lDisplay ), Self
    METHOD DeleteAll()                 INLINE ::oGet:DeleteAll(), Self
    METHOD DelEnd()                    INLINE ::oGet:DelEnd(), Self
    METHOD DelLeft()                   INLINE ::oGet:DelLeft(), Self
    METHOD DelRight()                  INLINE ::oGet:DelRight(), Self
    METHOD DelWordLeft()               INLINE ::oGet:DelWordLeft(), Self
    METHOD ColorSpec( cColorSpec )     INLINE ::oGet:ColorSpec( cColorSpec )
    METHOD Picture( cPicture )         INLINE ::oGet:Picture( cPicture )
    METHOD Block( bBlock )             INLINE ::oGet:Block( bBlock )
    METHOD HitTest(mrow,mcol)          INLINE ::oGet:HitTest(mrow,mcol)
    METHOD FirstEditable()             INLINE ::oGet:FirstEditable()
    METHOD LastEditable()              INLINE ::oGet:LastEditable()
    METHOD HasScroll()                 INLINE ::oGet:HasScroll()
    // --------------------------- End from get class ------------------------------------

    METHOD GetValue()                  INLINE ::VarGet()
    METHOD AdjustWidth()

    METHOD WindowProc()
    METHOD CallWindowProc()
    METHOD ApplyKey()
    METHOD GetApplyKey()
    METHOD SetCaretType()
    METHOD OnCommand()
    METHOD OnChar()
    METHOD OnKeyDown()
    METHOD OnPaint()
    METHOD OnSetFocus()
    METHOD OnKillFocus()

    METHOD SetWindowFocus()           INLINE ::Super:SetFocus()
    METHOD KillWindowFocus()          INLINE ::Super:KillFocus()

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New( nTop, nLeft, bVarBlock, cVarName, cPicture, cColorSpec, ;
            nWidth, nHeight, oParent, cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword, lAutoSize ) CLASS TXGet

    ::oGet := Get():New( nTop, nLeft, bVarBlock, cVarName, cPicture, cColorSpec )
    ::Super:New( cVarName,, nTop, nLeft, nWidth, nHeight, oParent, , cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )

    ASSIGN ::lAutoSize WITH lAutosize DEFAULT FALSE

    WG_DebugTrace( "TXGet:New()", "Self", Self )

RETURN Self

//---------------------------------------------------------------------------//

METHOD NewExtended( nTop, nLeft, bVarBlock, cVarName, cPicture, cColorSpec, ;
                    nWidth, nHeight, oParent, cToolTip,;
                    cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword, lAutoSize, ;
                    oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TXGet

    ::New( nTop, nLeft, bVarBlock, cVarName, cPicture, cColorSpec, ;
            nWidth, nHeight, oParent, cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword, lAutoSize )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

    WG_DebugTrace( "TXGet:NewExtended()", "Self", Self )

RETURN Self

//---------------------------------------------------------------------------//

METHOD Init() CLASS TXGet

    WG_DebugTrace( "TXGet:Init()" )

    // Set default colors
    //IF ::oFgColor == NIL THEN ::SetForeGroundColor( TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
    //IF ::oBgColor == NIL THEN ::SetBackGroundColor( TSystemSetting():GetColor(COLOR_BTNHIGHLIGHT) )
    WG_DebugTrace( "TXGET:Init() - before ::Super:Init()", "Object", Self:ClassName + "(" + Self:cName + ")", "nHandle", ::nHandle )

    ::Super:Init()

    IF ::lAutoSize THEN ::AdjustWidth()

    WG_DebugTrace( "TXGET:Init() - after ::Super:Init()" )
    //IF ::nLimitText <> NIL THEN ::SetLimitText( ::nLimitText )
    //IF ValType( ::bVarBlock ) == "B"
    //   Eval( ::bVarBlock, ::GetValue() )
    //ENDIF
    WG_DebugTrace( "TXGET:Init() - activate subclassing", "::nHandle", ::nHandle )
    ::Display( TRUE )
    ::nPreviousProc := WG_InitEditProc( ::nHandle )
    ::nCurrentProc  := GetWindowLong( ::nHandle, GWL_WNDPROC )
    WG_DebugTrace( "TXGET:Init() - subclassing activated", "::nPreviousProc", ::nPreviousProc, "::nCurrentProc", ::nCurrentProc )

    //SendMessage ( ::nHandle, EM_LIMITTEXT, 10, 0 )
    //IF ::cInitValue <> NIL THEN ::SetValue( ::cInitValue )
    //::DisplayData()
RETURN Self

//---------------------------------------------------------------------------//

METHOD AdjustWidth() CLASS TXGet
   LOCAL nW
   WG_DebugTrace( "TXGet:AdjustWidth()", "::oGet:nDispLen", ::oGet:nDispLen, "::oFont:nHandle", ::oFont:nHandle )
   nW := UnMapDialogRect( Replicate( "X", ::oGet:nDispLen ), ::oFont:nHandle ) [ 1 ]
   WG_DebugTrace( "TXGet:AdjustWidth()", "nW", nW, "::lPixel", ::lPixel, "::oGet:Picture", ::oGet:Picture, "::oGet:nMaxLen", ::oGet:nMaxLen )
   WG_DebugTrace( "TXGet:AdjustWidth()", "::nWidth", ::nWidth, "::nHeight", ::nHeight, "::nTop", ::nTop, "::nLeft", ::nLeft )
   ::nWidth := Int( nW )
   ::SetSize()
RETURN Self

//---------------------------------------------------------------------------//

METHOD WindowProc( nMsg, wParam, lParam ) CLASS TXGet
   LOCAL nRet := -1
   LOCAL wmId, wmEvent, wmHandle
   LOCAL oWin

   // Check if there is a user event handler
   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
      // Evaluate User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
   ENDIF
   IF nRet == -1
      // Class event handler
      ::nMsg    := nMsg
      ::nwParam := wParam
      ::nlParam := lParam

      DO CASE
         CASE nMsg == WM_GETDLGCODE
              WG_DebugTrace( "TXGet:WindowProc() - WM_GETDLGCODE" )
              nRet := DLGC_HASSETSEL + DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS
      ENDCASE
   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF
RETURN nRet


//---------------------------------------------------------------------------//

METHOD GetApplyKey(oGet, nKey) CLASS TXGet
   WG_DebugTrace( "TXGET:GetApplyKey()", "nKey", nKey )
   ::oParent:oGetList:oGet := oGet
   ::oParent:oGetList:GetApplyKey(nKey)
RETURN Self

//---------------------------------------------------------------------------//

METHOD OnCommand( wParam, lParam ) CLASS TXGet
   LOCAL nRet   := 0
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )

   LOCAL xVal, lValid

   //MessageBox(,"Passato da BN_ command")
   DO CASE

      CASE nEvent == EN_SETFOCUS
    //       HideCaret(::nHandle)
    //       ::oParent:oGetList:GetActive( ::oGet )
    //       lValid := ::oParent:oGetList:GetPreValidate()
           WG_DebugTrace( "XGET - EN_SETFOCUS", "wParam", wParam, "lParam", lParam )
           nRet := ::OnSetFocus( lParam )
    //
    //       //lValid := TRUE
    //       //IF ValType( ::oGet:PreBlock ) == "B"
    //       //   lValid := Eval( ::oGet:PreBlock, ::oGet )
    //       //ENDIF
    //       IF lValid
    //          WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_SETFOCUS", wParam, lParam )
    //          ::Super:OnSetFocus()
    //          //nRet := ::CallWindowProc( WM_COMMAND, wParam, lParam )
    //          //WG_DebugTrace("TXGet:OnCommand()", "::CallWindowProc()", nRet )
    //          WG_DebugTrace("TXGet:OnCommand()", "::GetSelection()[1]", ::GetSelection()[1], "::GetSelection()[2]", ::GetSelection()[2] )
    //          WG_DebugTrace("TXGet:OnCommand()", "xVal", xVal, "ValType(xVal)", ValType(xVal), "::oGet:Type", ::Type, "::oGet:Original", ::oGet:Original, "ValType( ::oGet:Original )",ValType( ::oGet:Original ) )
    //          ::oGet:SetFocus()
    //          ::Display( TRUE )
    //          WG_DebugTrace("TXGet:OnCommand()", "xVal", xVal, "ValType(xVal)", ValType(xVal), "::oGet:Type", ::Type, "::oGet:Original", ::oGet:Original, "ValType( ::oGet:Original )",ValType( ::oGet:Original ) )
    //          WG_DebugTrace("TXGet:OnCommand()", "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet() )
    //          WG_DebugTrace("TXGet:OnCommand()", "::oGet:Pos()", ::oGet:Pos, "::GetInsertionPoint()", ::GetInsertionPoint() )
    //          ::SetInsertionPoint( ::oGet:Pos )
    //          ::SetCaretType()
    //       ELSE
    //          ::oParent:NextControl()
    //       ENDIF
    //
      CASE nEvent == EN_KILLFOCUS
    //       lValid := TRUE
    //       HideCaret(::nHandle)
    //
           WG_DebugTrace( "XGET - EN_KILLFOCUS", "wParam", wParam, "lParam", lParam )
           nRet := ::OnKillFocus( lParam )
    //       WG_DebugTrace("TXGet:OnCommand()", "xVal", xVal, "ValType(xVal)", ValType(xVal), "::oGet:Type", ::Type, "::oGet:Original", ::oGet:Original, "ValType( ::oGet:Original )",ValType( ::oGet:Original ) )
    //       WG_DebugTrace("TXGet:OnCommand()", "::oGet:Untransform()", ::oGet:Untransform() )
    //       WG_DebugTrace("TXGet:OnCommand()", "::oGet:BadDate", ::oGet:BadDate, "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet() )
    //
    //       WG_DebugTrace("TXGet:OnCommand() - KILLFOCUS", "PostBlock", IIF( ValType( ::oGet:PostBlock ) == "B", Eval( ::oGet:PostBlock, ::oGet ), NIL ) )
    //
    //       lValid := ::oParent:oGetList:GetPostValidate()
    //       WG_DebugTrace("TXGet:OnCommand() - KILLFOCUS", "lValid", lValid )
    //
    //       /*
    //       IF ::oGet:type == "D" .AND. ::oGet:BadDate
    //          //WG_DebugTrace("TXGet:OnCommand() - BAD DATE", "::oGet:HasFocus", ::oGet:HasFocus, "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet() )
    //
    //          // FSG - IF WE WANT TO FREE THIS GET WE CAN PUT A BLANK VALUE
    //          //       by now leave as clipper default
    //          // Clear bad date
    //          //::oGet:VarPut( CTOD("") )
    //          // Set as correct
    //          //::oGet:BadDate := FALSE
    //          // Update
    //          //::oGet:Reset()
    //
    //
    //          ::oGet:KillFocus()
    //          ::Display( TRUE )
    //          //nRet := 0
    //          lValid := FALSE
    //       ELSE
    //          IF ValType( ::oGet:PostBlock ) == "B"
    //             lValid := Eval( ::oGet:PostBlock, ::oGet )
    //          ENDIF
    //          IF lValid
    //             ::oGet:Home()
    //             ::oGet:KillFocus()
    //             ::Display( TRUE )
    //             WG_DebugTrace("TXGet:OnCommand()", "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet(), "::oGet:Untransform()", ::oGet:Untransform() )
    //          ENDIF
    //       ENDIF
    //       */
    //       IF lValid
    //          nRet = ::Super:OnKillFocus()
    //          DestroyCaret()
    //       ELSE
    //          ::Super:SetFocus()
    //          ::SetCaretType()
    //       ENDIF

      CASE nEvent == EN_CHANGE
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_CHANGE", wParam, lParam )

      CASE nEvent == EN_UPDATE
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_UPDATE", wParam, lParam )
           ::UpdateVar()

      CASE nEvent == EN_MAXTEXT
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_MAXTEXT", wParam, lParam )

      CASE nEvent == EN_HSCROLL
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_HSCROLL", wParam, lParam )

      CASE nEvent == EN_VSCROLL
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_VSCROLL", wParam, lParam )

      OTHERWISE
           WG_DebugTrace( "XGET:OnCommand() - otherwise nEvent" )
      //     IF ::HasAction()
      //        ::ExecAction( Self )
      //        nRet := 0  // Something exec, stop handling
      //     ENDIF
   ENDCASE

RETURN nRet

//---------------------------------------------------------------------------//

METHOD OnSetFocus( nHandle ) CLASS TXGet
   LOCAL nRet   := -1
   LOCAL xVal, lValid

           WG_DebugTrace( "XGET:OnSetFocus()", "nHandle", nHandle )

           HideCaret(::nHandle)
           ::oParent:oGetList:GetActive( ::oGet )
           ::oParent:oGetList:oGet := ::oGet
           lValid := ::oParent:oGetList:GetPreValidate()
           WG_DebugTrace("TXGet:OnSetFocus() - SETFOCUS", "::oGet:PreBlock", ::oGet:PreBlock, "::oGet:Name", ::oGet:Name, "lValid", lValid )
           WG_DebugTrace("TXGet:OnSetFocus() - SETFOCUS", "::oGet:PreBlock", IIF( ::oGet:PreBlock <> nil, EVAL( ::oGet:PreBlock ), NIL ), "::oGet:Name", ::oGet:Name, "lValid", lValid )

           IF lValid
              ::Super:OnSetFocus()
              //nRet := ::CallWindowProc( WM_COMMAND, wParam, lParam )
              //WG_DebugTrace("TXGet:OnCommand()", "::CallWindowProc()", nRet )
              WG_DebugTrace("TXGet:OnCommand()", "::GetSelection()[1]", ::GetSelection()[1], "::GetSelection()[2]", ::GetSelection()[2] )
              WG_DebugTrace("TXGet:OnCommand()", "xVal", xVal, "ValType(xVal)", ValType(xVal), "::oGet:Type", ::Type, "::oGet:Original", ::oGet:Original, "ValType( ::oGet:Original )",ValType( ::oGet:Original ) )
              ::oGet:SetFocus()
              ::Display( TRUE )
              WG_DebugTrace("TXGet:OnCommand()", "xVal", xVal, "ValType(xVal)", ValType(xVal), "::oGet:Type", ::Type, "::oGet:Original", ::oGet:Original, "ValType( ::oGet:Original )",ValType( ::oGet:Original ) )
              WG_DebugTrace("TXGet:OnCommand()", "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet() )
              WG_DebugTrace("TXGet:OnCommand()", "::oGet:Pos()", ::oGet:Pos, "::GetInsertionPoint()", ::GetInsertionPoint() )
              ::SetInsertionPoint( ::oGet:Pos )
              ::SetCaretType()
           ELSE
              WG_DebugTrace("TXGet:OnSetFocus() - SETFOCUS - Call ::oParent:NextControl()", "::oGet:ExitState", ::oGet:ExitState, "::oParent:oGetlist:nLastExitState", ::oParent:oGetlist:nLastExitState, "::oParent:oGetlist:nPos", ::oParent:oGetlist:nPos, "::oParent:oGetlist:nNextGet", ::oParent:oGetlist:nNextGet, "::oParent:oGetlist:nLastPos", ::oParent:oGetlist:nLastPos )
              ::oParent:oGetlist:nLastExitState := 0
              ::oParent:oGetlist:nNextGet := ::oParent:oGetlist:nPos + 1
              WG_DebugTrace("TXGet:OnSetFocus() - SETFOCUS - Call ::oParent:NextControl()", "::oGet:ExitState", ::oGet:ExitState, "::oParent:oGetlist:nLastExitState", ::oParent:oGetlist:nLastExitState, "::oParent:oGetlist:nPos", ::oParent:oGetlist:nPos, "::oParent:oGetlist:nNextGet", ::oParent:oGetlist:nNextGet, "::oParent:oGetlist:nLastPos", ::oParent:oGetlist:nLastPos )
              ::oParent:NextControl()
           ENDIF


RETURN nRet

//---------------------------------------------------------------------------//

METHOD OnKillFocus( nHandle ) CLASS TXGet
   LOCAL nRet   := -1

   LOCAL xVal, lValid

   WG_DebugTrace( "XGET:OnKillFocus()", "nHandle", nHandle )

           HideCaret(::nHandle)

           lValid := ::oParent:oGetList:GetPostValidate()
           WG_DebugTrace("TXGet:OnKillFocus() - KILLFOCUS", "::oGet:PostBlock", ::oGet:PostBlock, "::oGet:Name", ::oGet:Name, "lValid", lValid )

           /*
           IF ::oGet:type == "D" .AND. ::oGet:BadDate
              //WG_DebugTrace("TXGet:OnCommand() - BAD DATE", "::oGet:HasFocus", ::oGet:HasFocus, "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet() )

              // FSG - IF WE WANT TO FREE THIS GET WE CAN PUT A BLANK VALUE
              //       by now leave as clipper default
              // Clear bad date
              //::oGet:VarPut( CTOD("") )
              // Set as correct
              //::oGet:BadDate := FALSE
              // Update
              //::oGet:Reset()


              ::oGet:KillFocus()
              ::Display( TRUE )
              //nRet := 0
              lValid := FALSE
           ELSE
              IF ValType( ::oGet:PostBlock ) == "B"
                 lValid := Eval( ::oGet:PostBlock, ::oGet )
              ENDIF
              IF lValid
                 ::oGet:Home()
                 ::oGet:KillFocus()
                 ::Display( TRUE )
                 WG_DebugTrace("TXGet:OnCommand()", "::oGet:Buffer", ::oGet:buffer, "::oGet:varGet()", ::oGet:varGet(), "::oGet:Untransform()", ::oGet:Untransform() )
              ENDIF
           ENDIF
           */
           IF lValid
              ::oGet:Home()
              ::oGet:KillFocus()
              ::Display( TRUE )
              nRet = ::Super:OnKillFocus()
              DestroyCaret()
              //::oParent:NextControl()
           ELSE
              ::Super:SetFocus()
              ::SetCaretType()
           ENDIF

RETURN nRet

//---------------------------------------------------------------------------//

METHOD OnPaint() CLASS TXGet
   LOCAL nRet   := -1
   WG_DebugTrace("TXGet:OnPaint()" )
   ::Display( TRUE )
   nRet := 0

RETURN nRet

//---------------------------------------------------------------------------//


// Method Display modified from Get class to display under windows

METHOD Display( lForced ) CLASS TXGet

   //local nOldCursor := SetCursor( SC_NONE )
   WG_DebugTrace("TXGet:Display", "Called from " + PROCNAME(1) + "(" + cStr( PROCLINE(1), TRUE ) + ")" )
   WG_DebugTrace("TXGet:Display", "lForced", lForced )

   DEFAULT lForced TO TRUE

   WG_DebugTrace("TXGet:Display", "::oGet:HasScroll()", ::oGet:HasScroll(), "::oGet:Pos", ::oGet:Pos )
   if ::oGet:HasScroll() .and. ::oGet:Pos != NIL
      if ::oGet:nDispLen > 8
         ::oGet:nDispPos := Max( 1, Min( ::oGet:Pos - ::oGet:nDispLen + 4, ::oGet:nMaxLen - ::oGet:nDispLen + 1 ) )
      else
         ::oGet:nDispPos := Max( 1, Min( ::oGet:Pos - int( ::oGet:nDispLen / 2 ), ::oGet:nMaxLen - ::oGet:nDispLen + 1 ) )
      endif
   endif

   WG_DebugTrace("TXGet:Display", "oGet:Buffer", ::oGet:Buffer, "::GetText()", ::GetText(), "lForced", lForced, "::oGet:nDispPos != ::oGet:nOldPos", ::oGet:nDispPos != ::oGet:nOldPos )
   if ::oGet:buffer != NIL .and. ( lForced .or. ( ::oGet:nDispPos != ::oGet:nOldPos ) )

      WG_DebugTrace("TXGet:Display", "::oGet:cDelimit", ::oGet:cDelimit, "::oGet:nDispPos", ::oGet:nDispPos , "::oGet:nOldPos", ::oGet:nOldPos, "::oGet:nDispLen", ::oGet:nDispLen )
      SetPos( ::nTop, ::nLeft )
      if !(::oGet:cDelimit == NIL)
         ::SetText( Substr( ::oGet:cDelimit, 1, 1) + Pad( Substr( ::oGet:buffer, ::oGet:nDispPos, ::oGet:nDispLen ), ::oGet:nDispLen ) + Substr( ::oGet:cDelimit, 2, 1) )
      else
         ::SetText( Substr( ::oGet:buffer, ::oGet:nDispPos, ::oGet:nDispLen ) )
      endif
      WG_DebugTrace("TXGet:Display", "oGet:Buffer", ::oGet:Buffer, "::GetText()", ::GetText() )
      ::Redraw()
      //::SetForeGroundColor( IIF( ::oGet:HasFocus, ::oGet:ColorSpec, NIL ) ) // FSG - Must be set a standard color for control

      /*
      DispOutAt( ::oGet:Row, ::oGet:Col + if( ::oGet:cDelimit == NIL, 0, 1 ),;
                 Substr( ::oGet:buffer, ::oGet:nDispPos, ::oGet:nDispLen ), ;
                 hb_ColorIndex( ::oGet:ColorSpec, iif( ::oGet:HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      if !(::oGet:cDelimit == NIL)
         DispOutAt( ::oGet:Row, ::oGet:Col, Substr( ::oGet:cDelimit, 1, 1), hb_ColorIndex( ::oGet:ColorSpec, iif( ::oGet:HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
         DispOutAt( ::oGet:Row, ::oGet:Col + ::oGet:nDispLen + 1, Substr( ::oGet:cDelimit, 2, 1), hb_ColorIndex( ::oGet:ColorSpec, iif( ::oGet:HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      endif
      */
   endif

   ::oGet:nOldPos := ::oGet:nDispPos

   if ::oGet:Pos != NIL
      ::SetInsertionPoint( ::oGet:Pos - ::oGet:nDispPos + if( ::oGet:cDelimit == NIL, 0, 1 ) )
      SetPos( ::nTop, ::nLeft + ::oGet:Pos - ::oGet:nDispPos + if( ::oGet:cDelimit == NIL, 0, 1 ) )
   endif

   //SetCursor( nOldCursor )

return Self

// ------------------------------------------------------------------------ //

METHOD ApplyKey(nVK, nKey) CLASS TXGet
   local   oGet  := ::oGet, hWnd := ::nHandle
   *local  nGPos := oGet:Pos
   local   aESel := ::GetSelection()
   local   nEStart := aESel[1], nEEnd := aESel[2]
   local   cGBuf := NEW_STR(oGet:Buffer)
   local   cEBuf := ::GetText()
   local   lGSame, lESame, i, nRet := 0, cStr

   WG_DebugTrace("TXGet:ApplyKey", "nVK", nVK, "nKey", nKey)
   WG_DebugTrace("TXGet:ApplyKey", "nEStart", nEStart, "nEEnd", nEEnd, "oGet:Pos", oGet:Pos )
   WG_DebugTrace("TXGet:ApplyKey", "CanUndo", ::CanUndo())
   //$ "nEStart, nEEnd, oGet:Pos", nEStart, nEEnd, oGet:Pos
   //$ "CanUndo", ::CanUndo()

   //nRet = ::CallWindowProc() - 1
   //$ "  back from ::CallWindowProc()"
   //WG_DebugTrace("TXGet:ApplyKey - back from", "::CallWindowProc()", nRet )
   lESame := ::IsModified() //(SendMessage(hWnd, EM_GETMODIFY) == 0) // was Edit unchanged
   //lESame = (::GetText() == cEBuf)    // was Edit unchanged

    WG_DebugTrace("TXGet:ApplyKey", "cGBuf", cGBuf)

   if nVK == 26    // Ctrl+Z == undo
       aESel   := ::GetSelection()
       nEStart := aESel[1]
       nEEnd   := aESel[2]
       ::Undo()
       //$ "UNDO: new nEStart, nEEnd, lESame", nEStart, nEEnd, lESame
       WG_DebugTrace("TXGet:ApplyKey - UNDO: new ", "nEStart", nEStart, "nEEnd", nEEnd, "lESame", lESame )
   else
       WG_DebugTrace("TXGet:ApplyKey - sync edit " )
       if nEStart != nEEnd
           ::GetApplyKey(oGet, nKey)
       elseif nEStart > len(oGet:Buffer) // .and. oGet:Pos == len(oGet:Buffer)
           // a position a GET doesn't have (i.e. off the end)
           if ( (nVK >= 32 .and. nVK < 256)              ;
                .OR. nVK == VK_DELETE ) .AND. !( nVk == VK_HOME )
               //$ "ignored"
               WG_DebugTrace("TXGet:ApplyKey - ignored" )

           elseif nVK == VK_BACK
               ::GetApplyKey(oGet, K_DEL)
           else
               ::GetApplyKey(oGet, nKey)
           endif
           if len( ::GetText() ) > len( cGBuf )
               //$ "Truncating!"
               WG_DebugTrace("TXGet:ApplyKey - Truncating " )
               aESel = ::GetSelection()
               ::SetSelection( len(cGBuf) + 1, -1 )
               // sadly, loses CANUNDO status
               ::Replace( "" )
               ::SetSelection( aESel )
               //$ "new length", len(::GetText())
               WG_DebugTrace("TXGet:ApplyKey - New lenght = ", len(::GetText()) )
           endif
       else
           ::GetApplyKey(oGet, nKey)
       endif
   endif

   lGSame := (oGet:Buffer == cGBuf)
   WG_DebugTrace("TXGet:ApplyKey", "oGet:Buffer", oGet:Buffer)
   WG_DebugTrace("TXGet:ApplyKey", "lESame", lESame, "lGSame", lGSame)
   WG_DebugTrace("TXGet:ApplyKey", "::GetInsertionPoint()", ::GetInsertionPoint(), "oGet:Pos", oGet:Pos )

   i := oGet:Pos // i := oGet:Pos - 1

   if ::IsReadOnly() //ReadOnly(self)
       // read-only, so don't touch the Clipper GET
   elseif (lESame .and. lGSame) .or. (WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"),;
           oGet:Buffer == left(::GetText(), len(oGet:Buffer)))
   *   if lGSame
           // buffers didn't change, but maybe the position did
           WG_DebugTrace("TXGet:ApplyKey", "just syncing Position")
           aESel := ::GetSelection()
           //$ "aESel", aESel[1], aESel[2], i + 1
           WG_DebugTrace("TXGet:ApplyKey", "aESel[1]", aESel[1], "aESel[2]", aESel[2], "i", i )
           if aESel[1] == aESel[2]
               //oGet:Pos = aESel[1]
               //$ "moved to", oGet:Pos
               WG_DebugTrace("TXGet:ApplyKey", "moved to pos", oGet:Pos )
               if oGet:Pos != aESel[1] .and. oGet:Pos != len(oGet:Buffer)
                   //$ "not editable", aESel[1]
                   //$ "sync with GET after all"
                   WG_DebugTrace("TXGet:ApplyKey", "not editable aESel[1]", aESel[1] )
                   WG_DebugTrace("TXGet:ApplyKey", "sync with GET after all" )
                   //if i < aESel[1]
                   //    oGet:Left()
                   //// else already did oGet:Right() internally
                   //endif
                   i := oGet:Pos
                   //$ "moved to", i + 1
                   WG_DebugTrace("TXGet:ApplyKey", "moved to i", i )
                   ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
                   nRet := 0
               elseif oGet:Pos != aESel[1] .and. oGet:Pos == len(oGet:Buffer)
                   ::SetInsertionPoint( i )
                   nRet := 0
               endif

           // else keep the Edit's selection
           endif

   elseif lESame .and. !lGSame
   *   else
           // only the GET buffer changed - update the Edit too
           WG_DebugTrace("TXGet:ApplyKey", "lESame .and. !lGSame", lESame .and. !lGSame, "lESame", lESame, "lGSame", lGSame)
           WG_DebugTrace("TXGet:ApplyKey", "Edit := Get")
           WG_DebugTrace("TXGet:ApplyKey", "i", i)
           ::SetText( oGet:Buffer ) //SendMessage(hWnd, WM_SETTEXT, 0, oGet:Buffer)
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
   *   endif
   elseif nEStart == nEEnd .and. nVK != 26  // Ctrl+Z == Undo
       // Edit changed
       // and there was no existing selection that could have been deleted
   //  if nKey >= 32 .and. nKey <= 255
           // sync with the Get
           WG_DebugTrace("TXGet:ApplyKey - Edit := Get")
           //$ cGBuf, oGet:Buffer
           WG_DebugTrace("TXGet:ApplyKey", "cGBuf", oGet:Buffer )
           WG_DebugTrace("TXGet:ApplyKey", "i", i)
           ::SetText( oGet:Buffer ) //SendMessage(hWnd, WM_SETTEXT, 0, oGet:Buffer)
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
           ::Redraw()
   else
       // Edit changed, and there was an existing selection

       // sync with the Edit
       WG_DebugTrace("TXGet:ApplyKey", "was a selection, so Get := Edit")
       WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'" )
       //$ "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"
       // ::PutText()  we can't do :assign(), because numeric wouldn't work correct
       ::SetModified( TRUE ) //SET_CHANGED(oGet)


       /*
        *  Try to do to the GET what the edit control did, but allow for the
        *  GET's behaviour (PICTURE, non-editable positions, length, etc).
        *
        *  In the worst case, this is impossible!
        */

       // restore GET, then delete/insert according to changes in the edit
       //$ "before: '" + cGBuf + "' cEBuf: '" + cEBuf + "'"
       WG_DebugTrace("TXGet:ApplyKey", "before: '" + cGBuf + "' cEBuf: '" + cEBuf + "'" )
       //oGet:Buffer = NEW_STR(cGBuf)
       oGet:VarPut( NEW_STR(cGBuf) )
       cStr = ::GetText()
       cStr += right(cGBuf, len(cGBuf) - len(cStr))    // may pad with previous
       //$ "cStr: '" + cStr + "'"
       WG_DebugTrace("TXGet:ApplyKey", "cStr: '" + cStr + "'" )
       i = 0
       do while .t.
           ++i
           if oGet:Pos != i
               oGet:Pos = i
           endif
           //$ "    ", i, oGet:Pos
           WG_DebugTrace("TXGet:ApplyKey", "i", i, "oGet:Pos", oGet:Pos )
           if substr(cStr, i, 1) == substr(cEBuf, i, 1)
               if substr(cStr, i, 1) == ""
                   exit    // no more to check
               endif
               if substr(cStr, i, 1) == substr(oGet:Buffer, i, 1)
                   loop    // all are the same
               endif

               // the edit control didn't change but != GET
               if oGet:Pos != i
                   loop    // not an editable position
               endif
               //$ "    Overstrike1:", substr(cStr, i, 1)
               WG_DebugTrace("TXGet:ApplyKey", "    Overstrike1:", substr(cStr, i, 1) )
               oGet:Overstrike(substr(cStr, i, 1))
               // can't think what to do if oGet:Rejected
               //$ "    '" + oGet:Buffer + "'"
               WG_DebugTrace("TXGet:ApplyKey", "    '" + oGet:Buffer + "'" )
               loop
           endif
           // edit control is different at this point; re-sync'ing is
           // impossible but worth a try
           if substr(cStr, i, 1) == substr(oGet:Buffer, i, 1)
               loop    // these look OK
           endif
           if oGet:Pos != i
               loop    // not an editable position
           endif
           //$ "    Overstrike2:", substr(cStr, i, 1)
           WG_DebugTrace("TXGet:ApplyKey", "    Overstrike2:", substr(cStr, i, 1) )
           oGet:Overstrike(substr(cStr, i, 1))
           // can't think what to do if oGet:Rejected
           //$ "    '" + oGet:Buffer + "'"
           WG_DebugTrace("TXGet:ApplyKey", "    '" + oGet:Buffer + "'" )
       enddo
       //$ "before: '" + cGBuf + "' after: '" + oGet:Buffer + "'"
       WG_DebugTrace("TXGet:ApplyKey", "before: '" + cGBuf + "' after: '" + oGet:Buffer + "'" )


       //$ "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"
       WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'" )
       oGet:Pos := nEStart := ::GetInsertionPoint()
       // be careful that the Get accepted it
       if !(rtrim(oGet:Buffer) == rtrim(::GetText()))
           // may warn if not just a difference in case
           if !(lower(rtrim(oGet:Buffer)) == lower(rtrim(::GetText())))
               WG_DebugTrace("TXGet:ApplyKey", "! Something rejected !")
   #ifdef  WARN_REJECT
               ::Warning()
   #endif
           endif
           ::SetText( oGet:Buffer )
           i := oGet:Pos - 1
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
       endif
       if oGet:Pos != nEStart
           WG_DebugTrace("TXGet:ApplyKey", "! Get:Pos != Edit:Position !")
           i := oGet:Pos - 1
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
       endif
   endif

   ::SetModified( FALSE )
   //$ "CanUndo", ::CanUndo()
   WG_DebugTrace("TXGet:ApplyKey", "CanUndo", ::CanUndo() )

return nRet

//---------------------------------------------------------------------------//

METHOD OnChar( wParam, lParam ) CLASS TXGet
   LOCAL nRet := -1  // = TRUE  if process message must return 0
   LOCAL nVK         := wParam
   LOCAL nStatus     := lParam

   WG_DebugTrace("OnChar", "wParam", wParam, "lParam", lParam )

   //MessageBox(,"OnChar")

   if nVK != K_INS   ; // Ctrl+V changes _SET_INSERT (see ::OnKeyDown()) !!!
     .and. !(nVK == VK_BACK .and. nStatus == 0)  // MLE sends this after DEL in a mid-position
     ::ApplyKey(nVK, nVK)
     //::Display()
     nRet := 0
   else
     nRet := -1
   endif

   do case
      case ::oGet:ExitState == GE_NOEXIT
           WG_DebugTrace("TXGet:oGet:ExitState", "GE_NOEXIT")
      case ::oGet:ExitState == GE_ENTER
           WG_DebugTrace("TXGet:oGet:ExitState", "GE_ENTER")
      case ::oGet:ExitState == GE_UP
           WG_DebugTrace("TXGet:oGet:ExitState", "GE_UP")
      case ::oGet:ExitState == GE_DOWN
           WG_DebugTrace("TXGet:oGet:ExitState", "GE_DOWN")
      case ::oGet:ExitState == GE_ESCAPE
           WG_DebugTrace("TXGet:oGet:ExitState", "GE_ESCAPE")
      otherwise
           WG_DebugTrace("TXGet:oGet:ExitState", ::oGet:ExitState)
   endcase

return nRet

//---------------------------------------------------------------------------//

#define SHIFTED 0x8000

METHOD OnKeyDown( wParam, lParam ) CLASS TXGet
   LOCAL nRet := -1  // = TRUE  if process message must return 0
   LOCAL nVK     := wParam
   LOCAL nStatus := lParam
   local nKey, lCtrl, lShift, aSel
   LOCAL nPos

   WG_DebugTrace("TXGet:OnKeyDown", "wParam", wParam, "lParam", lParam )

   WG_DebugTrace("TXGet:OnKeyDown", "nVK", nVK)

   lCtrl := And( GetKeyState( VK_CONTROL ), SHIFTED ) != 0  // .t. if pressed

   // only handle the special keys  (see accel.ch)

   IF nVK < 48 .AND. nVK != VK_BACK

       lShift := And( GetKeyState( VK_SHIFT ), SHIFTED ) != 0

       DO CASE
       CASE nVK == VK_SHIFT
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_SHIFT")
          nKey := 0   // ignore
       CASE nVK == VK_CONTROL
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_CONTROL")
          nKey := 0   // ignore
       CASE nVK == VK_CLEAR
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_CLEAR")
          // maybe something ?
       CASE nVK == VK_PRIOR
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_PRIOR")
          nKey := K_PGUP
       CASE nVK == VK_NEXT
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_NEXT")
       //   nKey := K_PGDN              // can do this if you wish
          nKey := 0                     // ignore - best to use an OK button
       CASE nVK == VK_END
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_END")
          nKey := iif(lCtrl, 0, K_END)  // ignore Ctrl-End: best to use an OK button
       CASE nVK == VK_HOME
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_HOME")
          nKey := iif(lCtrl, K_CTRL_HOME, K_HOME)
       CASE nVK == VK_LEFT
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_LEFT")
          nKey := iif(lCtrl, K_CTRL_LEFT, K_LEFT)
       CASE nVK == VK_UP
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_UP")
          nKey := K_UP
       CASE nVK == VK_RIGHT
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_RIGHT")
          nKey := iif(lCtrl, K_CTRL_RIGHT, K_RIGHT)
       CASE nVK == VK_DOWN
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_DOWN")
          nKey := K_DOWN
       CASE nVK == VK_INSERT
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_INSERT")
          // was: nKey := K_INS
          if ! lShift .and. ! lCtrl   // ::OnChar() would change _SET_INSERT
             Set( _SET_INSERT, ! Set( _SET_INSERT ) )    // also on Ctrl+V and
             ::SetCaretType()
             nKey := NIL                                 // Shift+Ins,
          //else                                          // so make it here!!!
          //   nRet := ::CallWindowProc()
          endif
       CASE nVK == VK_DELETE
          WG_DebugTrace("TXGet:OnKeyDown", "nVK", "VK_DELETE")
          // was: nKey := K_DEL
          aSel := ::GetSelection()
          if aSel[1] != aSel[2]   // Deletes selection
             //nRet := ::CallWindowProc()
             //$ "DEL aSel", aSel[1], aSel[2] - 1
             WG_DebugTrace("TXGet:OnKeyDown - DEL aSel", "aSel[1]", aSel[1], "aSel[2] - 1", aSel[2] - 1 )
             WG_DebugTrace("TXGet:OnKeyDown - before", "::oGet:Buffer", ::oGet:Buffer, "::oGet:Pos", ::oGet:Pos )
             // delete right to left to allow for picture chars

             // find firt editable
             do while ( nPos := --aSel[2] ) >= aSel[1]
                if ::oGet:IsEditable( nPos )
                   exit
                endif
             enddo
             ::oGet:Pos := nPos
             // delete until aSel[1] except this, because i'm going left
             do while ::oGet:Pos > aSel[1]
                ::oGet:Delete()
                ::oGet:Left()
                WG_DebugTrace("TXGet:OnKeyDown - deleting", "::oGet:Buffer", ::oGet:Buffer, "::oGet:Pos", ::oGet:Pos )
             enddo
             // delete this too
             if ::oGet:Pos == aSel[1]
                ::oGet:Delete()
                WG_DebugTrace("TXGet:OnKeyDown - deleting", "::oGet:Buffer", ::oGet:Buffer, "::oGet:Pos", ::oGet:Pos )
             endif
             ::SetText( ::oGet:Buffer )
             ::SetInsertionPoint( ::oGet:Pos )
             WG_DebugTrace("TXGet:OnKeyDown - after", "::oGet:Buffer", ::oGet:Buffer, "::oGet:Pos", ::oGet:Pos )
             nKey := NIL
             nRet := 0
          else
             nKey := K_DEL
          endif
       ENDCASE

       WG_DebugTrace("TXGet:OnKeyDown", "nKey", nKey)
       IF nKey != nil .and. nKey != 0
             nRet := ::ApplyKey(nVK, nKey)
             //nRet := 0
       ELSE
          nRet := 0
       ENDIF
   else
       WG_DebugTrace("TXGet:OnKeyDown", "to ::nPreviousProc")
       // was: nRet = ::CallWindowProc()
       if ( nVK == 86 .and. lCtrl )    // Ctrl+V -> paste: see above !!!
           ::Paste() //SendMessage(::hWnd, WM_PASTE, 0, 0)
           nRet := 0
       //else
       //    WG_DebugTrace("TXGet:OnKeyDown", "to ::nPreviousProc")
       //    nRet = ::CallWindowProc() - 1
       endif
   ENDIF

   do case
      case ::oGet:ExitState == GE_NOEXIT
          WG_DebugTrace("TXGet:oGet:ExitState", "GE_NOEXIT")
      case ::oGet:ExitState == GE_ENTER
          WG_DebugTrace("TXGet:oGet:ExitState", "GE_ENTER")
      case ::oGet:ExitState == GE_UP
          WG_DebugTrace("TXGet:oGet:ExitState", "GE_UP")
      case ::oGet:ExitState == GE_DOWN
          WG_DebugTrace("TXGet:oGet:ExitState", "GE_DOWN")
      case ::oGet:ExitState == GE_ESCAPE
          WG_DebugTrace("TXGet:oGet:ExitState", "GE_ESCAPE")
      otherwise
          WG_DebugTrace("TXGet:oGet:ExitState", ::oGet:ExitState)
   endcase

return nRet

//---------------------------------------------------------------------------//

METHOD CallWindowProc( nMsg, wParam, lParam ) CLASS TXGet
  DEFAULT nMsg   TO ::nMsg
  DEFAULT wParam TO ::nwParam
  DEFAULT lParam TO ::nlParam
RETURN CallWindowProc( ::nPreviousProc, ::nHandle, nMsg, wParam, lParam )

//---------------------------------------------------------------------------//

METHOD SetCaretType( lInsert ) CLASS TXGet

   Local hDC := GetDC( ::nHandle )
   local nX, nY, nWidth, nHeight, aClientRect := GetClientRect( ::nHandle )

   DEFAULT lInsert TO Set( _SET_INSERT )

   nX  := GetSystemMetrics( SM_CXBORDER )
   nY  := GetSystemMetrics( SM_CYBORDER )

   nWidth  := IIF( lInsert, 2, 6 ) * nX
   nHeight := ( aClientRect[4] - aClientRect[2] - 1 ) * nY

   WG_DebugTrace("TXGet:SetCaretType", "nWidth", nWidth, "nHeight", nHeight )

   ReleaseDC( ::nHandle, hDC )
   HideCaret( ::nHandle )
   DestroyCaret()
   CreateCaret( ::nHandle, NIL, nWidth, nHeight )
   ShowCaret( ::nHandle )

RETURN Self


Function UnMapDialogRect(cText,hfont)

  Local nX,nY,nW,nH

  Local hDC := GetDC(0)
  Local hOldFont:=SelectObject(hDC,hFont)
  Local aTextExt:=GetTextExtentPoint32(hDC,;
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')

  Local arect:={0,0,100,100}

  nW:=aTextExt[1]
  nH:=aTextExt[2]

  // Looks like this is what it should be (?)

  nW:=Int((Int(nW / 26) + 1)/2)
  nX:=GetTextExtentPoint32(hDC,cText)[1]
  nY:=nH

  SelectObject(hDC,hOldFont)
  ReleaseDC(0, hDC)

  Return({Ceiling(nX*4/nW),Ceiling(nY*8/nY)})

// returns ceiling of a number

Function Ceiling( x )

   Return( If( x - Int( x ) > 0, Int( x ) + 1, x ) )
                                                    