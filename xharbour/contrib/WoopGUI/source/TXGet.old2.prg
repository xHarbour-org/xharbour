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
#include "windows.ch"

#include "getexit.ch"
#include "inkey.ch"

#define	NEW_STR(c)	((c) + "")

#define SYNC_EDIT

// Windows definitions
CLASS WG_TXGet FROM WG_TEdit
    // Base

    DATA oGet            HIDDEN

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

    METHOD  New()         CONSTRUCTOR
    METHOD  NewExtended() CONSTRUCTOR
    METHOD  Init()

    METHOD  Cancel()                            ;
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


    METHOD  Display()
    METHOD  ColorDisp( cColorSpec )     INLINE ::oGet:ColorDisp( cColorSpec ), Self
    METHOD  ParsePict( cPicture )       INLINE ::oGet:ParsePict( cPicture )
    METHOD  Assign()                    INLINE ::oGet:Assign(), Self
    METHOD  End()                       INLINE ::oGet:End(), Self
    METHOD  Home()                      INLINE ::oGet:Home(), Self
    METHOD  Reset()                     INLINE ::oGet:Reset(), Self
    METHOD  Undo()                      INLINE ::oGet:Undo(), Self
    METHOD  SetFocus()                  INLINE ::oGet:SetFocus(), Self
    METHOD  KillFocus()                 INLINE ::oGet:KillFocus(), Self
    METHOD  VarPut( xValue, lReFormat ) INLINE ::oGet:VarPut( xValue, lReFormat )
    METHOD  VarGet()                    INLINE ::oGet:VarGet()
    METHOD  Untransform( cBuffer )      INLINE ::oGet:Untransform( cBuffer )
    METHOD  UpdateBuffer()              INLINE ::oGet:UpdateBuffer(), Self
    METHOD  overstrike( cChar )         INLINE ::oGet:overstrike( cChar ), Self
    METHOD  Insert( cChar )             INLINE ::oGet:Insert( cChar ), Self
    METHOD  Right( lDisplay )           INLINE ::oGet:Right( lDisplay ), Self
    METHOD  Left( lDisplay )            INLINE ::oGet:Left( lDisplay ), Self
    METHOD  WordLeft()                  INLINE ::oGet:WordLeft(), Self
    METHOD  WordRight()                 INLINE ::oGet:WordRight(), Self
    METHOD  ToDecPos()                  INLINE ::oGet:ToDecPos(), Self
    METHOD  IsEditable( nPos )          INLINE ::oGet:IsEditable( nPos )
    METHOD  Input( cChar )              INLINE ::oGet:Input( cChar )
    METHOD  PutMask( xValue, lEdit )    INLINE ::oGet:PutMask( xValue, lEdit )
    METHOD  BackSpace( lDisplay )       INLINE ::oGet:BackSpace( lDisplay ), Self
    METHOD  Delete( lDisplay )          INLINE ::oGet:Delete( lDisplay ), Self
    METHOD  DeleteAll()                 INLINE ::oGet:DeleteAll(), Self
    METHOD  DelEnd()                    INLINE ::oGet:DelEnd(), Self
    METHOD  DelLeft()                   INLINE ::oGet:DelLeft(), Self
    METHOD  DelRight()                  INLINE ::oGet:DelRight(), Self
    METHOD  DelWordLeft()               INLINE ::oGet:DelWordLeft(), Self
    METHOD  ColorSpec( cColorSpec )     INLINE ::oGet:ColorSpec( cColorSpec )
    METHOD  Picture( cPicture )         INLINE ::oGet:Picture( cPicture )
    METHOD  Block( bBlock )             INLINE ::oGet:Block( bBlock )
    METHOD  HitTest(mrow,mcol)          INLINE ::oGet:HitTest(mrow,mcol)
    METHOD  FirstEditable()             INLINE ::oGet:FirstEditable()
    METHOD  LastEditable()              INLINE ::oGet:LastEditable()
    METHOD  HasScroll()                 INLINE ::oGet:HasScroll()
    // --------------------------- End from get class ------------------------------------

    METHOD  ApplyKey()
    METHOD  GetApplyKey()
    METHOD  OnCommand()
    METHOD  OnChar()
    METHOD  OnKeyDown()
    METHOD  OnPaint()

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec, ;
            nWidth, nHeight, oParent, cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )

    WG_DebugTrace( "TXGet:New()" )

    ::oGet := Get():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )
    ::Super:New( cVarName,, nRow, nCol, nWidth, nHeight, oParent, , cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )

RETURN Self

//---------------------------------------------------------------------------//

METHOD NewExtended( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec, ;
                    nWidth, nHeight, oParent, cToolTip,;
                    cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword, ;
                    oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

    WG_DebugTrace( "TXGet:NewExtended()" )

    ::New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec, ;
            nWidth, nHeight, oParent, cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )
    ::Extend( bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

RETURN Self

//---------------------------------------------------------------------------//

METHOD Init()

    WG_DebugTrace( "TXGet:Init()" )

    // Set default colors
    //IF ::oFgColor == NIL THEN ::SetForeGroundColor( WG_TSystemSetting():GetColor(COLOR_WINDOWTEXT) )
    //IF ::oBgColor == NIL THEN ::SetBackGroundColor( WG_TSystemSetting():GetColor(COLOR_BTNHIGHLIGHT) )
    WG_DebugTrace( "TXGET:Init() - before ::Super:Init()", "Object", Self:ClassName + "(" + Self:cName + ")", "nHandle", ::nHandle )
    ::Super:Init()
    WG_DebugTrace( "TXGET:Init() - after ::Super:Init()" )
    //IF ::nLimitText <> NIL THEN ::SetLimitText( ::nLimitText )
    //IF ValType( ::bVarBlock ) == "B"
    //   Eval( ::bVarBlock, ::GetValue() )
    //ENDIF
    WG_DebugTrace( "TXGET:Init() - activate subclassing", "nHandle", ::nHandle )
    ::Display( TRUE )
    WG_InitEditProc( ::nHandle )
    //SendMessage ( ::nHandle, EM_LIMITTEXT, 10, 0 )
    //IF ::cInitValue <> NIL THEN ::SetValue( ::cInitValue )
    //::DisplayData()
RETURN Self


//---------------------------------------------------------------------------//

METHOD GetApplyKey(oGet, nKey)
   WG_DebugTrace( "TXGET:GetApplyKey()" )
   ::oParent:oGetList:oGet := oGet
   ::oParent:oGetList:GetApplyKey(nKey)
RETURN Self

//---------------------------------------------------------------------------//

METHOD OnCommand( wParam, lParam )
   LOCAL nRet   := -1
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )

   //MessageBox(,"Passato da BN_ command")
   DO CASE

      CASE nEvent == EN_SETFOCUS
           //MessageBox(,"Passato da EN_SETFOCUS")
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_SETFOCUS", wParam, lParam )
           ::oGet:SetFocus()
           ::oGet:Pos := ::GetSelection()[1]
           //::SetInsertionPoint( ::oGet:Pos - 1 )
           ::Display( TRUE )
           nRet = ::Super:OnSetFocus()

      CASE nEvent == EN_KILLFOCUS
           //MessageBox(,"Passato da BN_KILLFOCUS")
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_KILLFOCUS", wParam, lParam )
           ::oGet:KillFocus()
           nRet = ::Super:OnKillFocus()

      CASE nEvent == EN_CHANGE
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_CHANGE", wParam, lParam )

      CASE nEvent == EN_UPDATE
           WG_ApplObj():EventsWrite( "XGET", ::nHandle, "EN_UPDATE", wParam, lParam )
           ::UpdateVar()
           //::Display()

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

METHOD OnPaint()
   LOCAL nRet   := -1
   WG_DebugTrace("TXGet:OnPaint()" )
   ::Display( TRUE )
   nRet := 0

RETURN nRet

//---------------------------------------------------------------------------//


METHOD Display( lForced )

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

      if !(::oGet:cDelimit == NIL)
         ::SetText( Substr( ::oGet:cDelimit, 1, 1) + Pad( Substr( ::oGet:buffer, ::oGet:nDispPos, ::oGet:nDispLen ), ::oGet:nDispLen ) + Substr( ::oGet:cDelimit, 2, 1) )
      else
         ::SetText( Substr( ::oGet:buffer, ::oGet:nDispPos, ::oGet:nDispLen ) )
      endif
      WG_DebugTrace("TXGet:Display", "oGet:Buffer", ::oGet:Buffer, "::GetText()", ::GetText() )
      ::Redraw()
      ::SetForeGroundColor( IIF( ::oGet:HasFocus, ::oGet:ColorSpec, NIL ) ) // FSG - Must be set a standard color for control

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
      // SetPos( ::oGet:Row, ::oGet:Col + ::oGet:Pos - ::oGet:nDispPos + if( ::oGet:cDelimit == NIL, 0, 1 ) )
   endif

   //SetCursor( nOldCursor )

return Self

// ------------------------------------------------------------------------ //

METHOD ApplyKey(nVK, nKey)
   local   oGet  := ::oGet, hWnd := ::nHandle
   *local  nGPos := oGet:Pos
   local   aESel := ::GetSelection()
   local   nEStart := aESel[1], nEEnd := aESel[2]
   local   cGBuf := NEW_STR(oGet:Buffer)
   local   cEBuf := ::GetText()
   local   lGSame, lESame, i, nRet, cStr

   WG_DebugTrace("TXGet:ApplyKey", "nVK", nVK, "nKey", nKey)
   WG_DebugTrace("TXGet:ApplyKey", "nEStart", nEStart, "nEEnd", nEEnd, "oGet:Pos", oGet:Pos )
   WG_DebugTrace("TXGet:ApplyKey", "CanUndo", ::CanUndo())
   //$ "nEStart, nEEnd, oGet:Pos", nEStart, nEEnd, oGet:Pos
   //$ "CanUndo", ::CanUndo()

   //nRet = ::CallWindowProc()
   //$ "  back from ::CallWindowProc()"
   lESame := ::IsModified() //(SendMessage(hWnd, EM_GETMODIFY) == 0) // was Edit unchanged
   //lESame = (::GetText() == cEBuf)    // was Edit unchanged

   WG_DebugTrace("TXGet:ApplyKey", "cGBuf", cGBuf)

   #ifdef  NO_UNDO
   ::GetApplyKey(oGet, nKey)
   #else   // !NO_UNDO
   if nVK == 26    // Ctrl+Z == undo
       aESel   := ::GetSelection()
       nEStart := aESel[1]
       nEEnd   := aESel[2]
       //$ "UNDO: new nEStart, nEEnd, lESame", nEStart, nEEnd, lESame
       WG_DebugTrace("TXGet:ApplyKey - UNDO: new ", "nEStart", nEStart, "nEEnd", nEEnd, "lESame", lESame )
   else
   #ifdef  SYNC_EDIT
       WG_DebugTrace("TXGet:ApplyKey - sync edit " )
       if nEStart != nEEnd
           ::GetApplyKey(oGet, nKey)
       elseif nEStart > len(oGet:Buffer) // .and. oGet:Pos == len(oGet:Buffer)
           // a position a GET doesn't have (i.e. off the end)
           if (nVK >= 32 .and. nVK < 256)              ;
              .OR. nVK == VK_DELETE
               //$ "ignored"
               WG_DebugTrace("TXGet:ApplyKey - VK_DELETE ignored" )

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
   /*  elseif (aESel := ::GetSelection(),                   ;
               WG_DebugTrace("new sel start, end", aESel[1], aESel[2]), ;
               aESel[1] > len(oGet:Buffer) .and. aESel[1] == aESel[2]) // .and. oGet:Pos == len(oGet:Buffer)
           // a position a GET doesn't have (i.e. off the end)
           if (nVK >= 32 .and. nVK < 256)              ;
           .or. nVK == VK_DELETE
               // ignore it
           elseif nVK == VK_BACK
               GetApplyKey(oGet, K_DEL)
           else
               GetApplyKey(oGet, nKey)
           endif
   */
       else
           ::GetApplyKey(oGet, nKey)
       endif
   #else   // !SYNC_EDIT
       ::GetApplyKey(oGet, nKey)
   #endif  // !SYNC_EDIT
   endif
   #endif  // !NO_UNDO

   lGSame := (oGet:Buffer == cGBuf)
   WG_DebugTrace("TXGet:ApplyKey", "oGet:Buffer", oGet:Buffer)
   WG_DebugTrace("TXGet:ApplyKey", "lESame", lESame, "lGSame", lGSame)
   WG_DebugTrace("TXGet:ApplyKey", "oGet:Pos", oGet:Pos)

   i := oGet:Pos - 1

   #ifdef  CHECK_SEL       // for KN
   aESel := ::GetSelection()
   if aESel[1] != aESel[2]
       // don't mess with whatever is selected
   elseif ::IsReadOnly() //ReadOnly(self)
   #else   // !CHECK_SEL
   if ::IsReadOnly() //ReadOnly(self)
   #endif  // !CHECK_SEL
       // read-only, so don't touch the Clipper GET
   elseif (lESame .and. lGSame) .or. (WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"),;
           oGet:Buffer == left(::GetText(), len(oGet:Buffer)))
   *   if lGSame
           // buffers didn't change, but maybe the position did
           WG_DebugTrace("TXGet:ApplyKey", "just syncing Position")
   #ifdef  REMOVE_SEL
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
   #else   // !REMOVE_SEL
           aESel := ::GetSelection()
           //$ "aESel", aESel[1], aESel[2], i + 1
           WG_DebugTrace("TXGet:ApplyKey", "aESel[1]", aESel[1], "aESel[2]", aESel[2], "i + 1", i + 1 )
           if aESel[1] == aESel[2]
   #ifdef  SYNC_EDIT
               oGet:Pos = aESel[1]
               //$ "moved to", oGet:Pos
               WG_DebugTrace("TXGet:ApplyKey", "moved to pos", oGet:Pos )
               if oGet:Pos != aESel[1] .and. oGet:Pos != len(oGet:Buffer)
                   //$ "not editable", aESel[1]
                   //$ "sync with GET after all"
                   WG_DebugTrace("TXGet:ApplyKey", "not editable aESel[1]", aESel[1] )
                   WG_DebugTrace("TXGet:ApplyKey", "sync with GET after all" )
                   if i < aESel[1]
                       oGet:Left()
                   // else already did oGet:Right() internally
                   endif
                   i = oGet:Pos - 1
                   //$ "moved to", i + 1
                   WG_DebugTrace("TXGet:ApplyKey", "moved to", i + 1 )
                   ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
               endif
   #else   // !SYNC_EDIT
               // nothing selected, sync with GET
               ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
   #endif  // !SYNC_EDIT
           // else keep the Edit's selection
           endif
   #endif  // !REMOVE_SEL
   elseif lESame .and. !lGSame
   *   else
           // only the GET buffer changed - update the Edit too
           WG_DebugTrace("TXGet:ApplyKey", "Edit := Get")
           ::SetText( oGet:Buffer ) //SendMessage(hWnd, WM_SETTEXT, 0, oGet:Buffer)
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
   *   endif
   #ifdef  NO_UNDO
   elseif nEStart == nEEnd
   #else   // !NO_UNDO
   elseif nEStart == nEEnd .and. nVK != 26
   #endif  // !NO_UNDO
       // Edit changed
       // and there was no existing selection that could have been deleted
   //  if nKey >= 32 .and. nKey <= 255
           // sync with the Get
           WG_DebugTrace("TXGet:ApplyKey - Edit := Get")
           //$ cGBuf, oGet:Buffer
           WG_DebugTrace("TXGet:ApplyKey", "cGBuf", oGet:Buffer )
           ::SetText( oGet:Buffer ) //SendMessage(hWnd, WM_SETTEXT, 0, oGet:Buffer)
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
           ::Redraw()
   /*  else
           // may be a Windows Paste or some such - sync with the Edit
           WG_DebugTrace("TXGet:ApplyKey", "paste?  Get := Edit")
           oGet:Buffer := NEW_STR(cEBuf := ::GetText())
           oGet:Pos := nEStart := ::GetInsertionPoint()
           // be careful that the Get accepted it
           if !(oGet:Buffer == cEBuf)
               WG_DebugTrace("TXGet:ApplyKey", "! Something rejected !")
               ::Warning()
               ::SetText( oGet:Buffer )
           endif
           if oGet:Pos != nEStart
               WG_DebugTrace("TXGet:ApplyKey", "! Get:Pos != Edit:Position !")
               i = oGet:Pos - 1
               ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
           endif
       endif
   */
   else
       // Edit changed, and there was an existing selection

       // sync with the Edit
       WG_DebugTrace("TXGet:ApplyKey", "was a selection, so Get := Edit")
       WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'" )
       //$ "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"
       // ::PutText()  we can't do :assign(), because numeric wouldn't work correct
       ::SetModified( TRUE ) //SET_CHANGED(oGet)

   #ifdef  NO_OBEY_PIC

   #ifdef  NO_PICBUF
       if oGet:type == "D"
           // maybe this is the best of bad solutions?
           oGet:Buffer := NEW_STR(cEBuf := DtoC(CtoD(::GetText())))
       else
           oGet:Buffer := NEW_STR(cEBuf := Pad(::GetText(), Len(oGet:Buffer))) // important Len() !!!
       endif
   #else
       ::PictureBuffer(::GetText())
   #endif
       // was: oGet:Buffer := cEBuf := ::GetText()

   #else   // !NO_OBEY_PIC

       /*
        *  Try to do to the GET what the edit control did, but allow for the
        *  GET's behaviour (PICTURE, non-editable positions, length, etc).
        *
        *  In the worst case, this is impossible!
        */

       // restore GET, then delete/insert according to changes in the edit
       //$ "before: '" + cGBuf + "' cEBuf: '" + cEBuf + "'"
       WG_DebugTrace("TXGet:ApplyKey", "before: '" + cGBuf + "' cEBuf: '" + cEBuf + "'" )
       oGet:Buffer = NEW_STR(cGBuf)
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

   #endif  // !NO_OBEY_PIC

       //$ "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'"
       WG_DebugTrace("TXGet:ApplyKey", "buf, text '" + oGet:Buffer + "' '" + ::GetText() + "'" )
       oGet:Pos := nEStart := ::GetInsertionPoint()
       // be careful that the Get accepted it
   #ifdef  NO_CHECK_AFTER
       if !(oGet:Buffer == cEBuf)
   #else
   #ifdef  NO_OBEY_PIC
       if !(oGet:Buffer == ::GetText())
   #else   // !NO_OBEY_PIC
       if !(rtrim(oGet:Buffer) == rtrim(::GetText()))
   #endif  // !NO_OBEY_PIC
   #endif
           // may warn if not just a difference in case
           if !(lower(rtrim(oGet:Buffer)) == lower(rtrim(::GetText())))
               WG_DebugTrace("TXGet:ApplyKey", "! Something rejected !")
   #ifdef  WARN_REJECT
               ::Warning()
   #endif
           endif
           ::SetText( oGet:Buffer )
           i = oGet:Pos - 1
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
       endif
       if oGet:Pos != nEStart
           WG_DebugTrace("TXGet:ApplyKey", "! Get:Pos != Edit:Position !")
           i = oGet:Pos - 1
           ::SetInsertionPoint( i ) //SendMessage(hWnd, EM_SETSEL, 0, MAKELPARAM(i, i))
       endif
   endif

   //SendMessage(hWnd, EM_SETMODIFY, 0, 0)
   ::SetModified( FALSE )
   //$ "CanUndo", ::CanUndo()
   WG_DebugTrace("TXGet:ApplyKey", "CanUndo", ::CanUndo() )

return nRet

//---------------------------------------------------------------------------//

METHOD OnChar( wParam, lParam )
   LOCAL nRet := -1  // = TRUE  if process message must return 0
   LOCAL nVK         := wParam
   LOCAL nStatus     := lParam

   WG_DebugTrace("OnChar", "wParam", wParam, "lParam", lParam )

   //MessageBox(,"OnChar")

   if nVK != K_INS   ; // Ctrl+V changes _SET_INSERT (see ::OnKeyDown()) !!!
     .and. !(nVK == VK_BACK .and. nStatus == 0)  // MLE sends this after DEL in a mid-position
     ::ApplyKey(nVK, nVK)
     ::Display( TRUE )
     nRet := 0
   endif

   do case
      case ::oGet:ExitState == GE_NOEXIT
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

METHOD OnKeyDown( wParam, lParam )
   LOCAL nRet := -1  // = TRUE  if process message must return 0
   LOCAL nVK     := wParam
   LOCAL nStatus := lParam
   local nKey, lCtrl, lShift, aSel

   WG_DebugTrace("TXGet:OnKeyDown", "wParam", wParam, "lParam", lParam )

   WG_DebugTrace("TXGet:OnKeyDown", "nVK", nVK)

   lCtrl := And( GetKeyState( VK_CONTROL ), SHIFTED ) != 0  // .t. if pressed

   // only handle the special keys  (see accel.ch)

   IF nVK < 48 .AND. nVK != VK_BACK

       lShift := And( GetKeyState( VK_SHIFT ), SHIFTED ) != 0

       DO CASE
       CASE nVK == VK_SHIFT
          nKey := 0   // ignore
       CASE nVK == VK_CONTROL
          nKey := 0   // ignore
       CASE nVK == VK_CLEAR
          // maybe something ?
       CASE nVK == VK_PRIOR
          nKey := K_PGUP
       CASE nVK == VK_NEXT
       //   nKey := K_PGDN              // can do this if you wish
          nKey := 0                     // ignore - best to use an OK button
       CASE nVK == VK_END
          nKey := iif(lCtrl, 0, K_END)  // ignore Ctrl-End: best to use an OK button
       CASE nVK == VK_HOME
          nKey := iif(lCtrl, K_CTRL_HOME, K_HOME)
       CASE nVK == VK_LEFT
          nKey := iif(lCtrl, K_CTRL_LEFT, K_LEFT)
       CASE nVK == VK_UP
          nKey := K_UP
       CASE nVK == VK_RIGHT
          nKey := iif(lCtrl, K_CTRL_RIGHT, K_RIGHT)
       CASE nVK == VK_DOWN
          nKey := K_DOWN
       CASE nVK == VK_INSERT
          // was: nKey := K_INS
          if ! lShift .and. ! lCtrl   // ::OnChar() would change _SET_INSERT
             Set( _SET_INSERT, ! Set( _SET_INSERT ) )    // also on Ctrl+V and
             nKey := NIL                                 // Shift+Ins,
          //else                                          // so make it here!!!
          //   nRet := ::CallWindowProc()
          endif
       CASE nVK == VK_DELETE
          // was: nKey := K_DEL
          aSel := ::GetSelection()
          if aSel[1] != aSel[2]   // Deletes selection
             //nRet := ::CallWindowProc()
   #ifdef  NO_OBEY_PIC
             if ::oGet:type == "C"
                ::PutText()
             else
                if ::oGet:type == "N"
                   ::oGet:varPut(Val(::GetText()))
                else
                   ::oGet:varPut(Ctod(LTrim(::GetText())))
                endif
                ::SetInsertionPoint( ::oGet:Pos := 1 )
                ::UpdateBuffer()
             endif
             nKey := NIL
   #else   // !NO_OBEY_PIC
             //$ "DEL aSel", aSel[1], aSel[2] - 1
             WG_DebugTrace("TXGet:OnKeyDown - DEL aSel", "aSel[1]", aSel[1], "aSel[2] - 1", aSel[2] - 1 )
             // delete right to left to allow for picture chars
             do while .t.
                ::oGet:Pos = --aSel[2]
                if aSel[2] < aSel[1]
                   exit
                endif
                if ::oGet:Pos == aSel[2]
                   ::oGet:Delete()
                endif
             enddo
             ::oGet:Pos = ::GetInsertionPoint()  // aSel[1]
             //$ "after: '" + ::oGet:Buffer + "'", ::oGet:Pos
             WG_DebugTrace("TXGet:OnKeyDown - after", "::oGet:Buffer", ::oGet:Buffer, "::oGet:Pos", ::oGet:Pos )
   *         if !(::oGet:Buffer == ::GetText())
             if !(rtrim(::oGet:Buffer) == rtrim(::GetText()))
                // may warn if not just a difference in case
                if !(lower(rtrim(::oGet:Buffer)) == lower(rtrim(::GetText())))
                   WG_DebugTrace("TXGet:OnKeyDown", "! Something rejected !")
   #ifdef  WARN_REJECT
                   ::Warning()
   #endif
                endif
                ::SetText( ::oGet:Buffer )
                ::SetInsertionPoint( ::oGet:Pos - 1 ) //SendMessage(::hWnd, EM_SETSEL, 0, MAKELPARAM(::oGet:Pos - 1, ::oGet:Pos - 1))
             endif
             if ::oGet:Pos != aSel[1]
                WG_DebugTrace("TXGet:OnKeyDown", "! Get:Pos != Edit:Position !")
                ::SetInsertionPoint( ::oGet:Pos - 1 ) //SendMessage(::hWnd, EM_SETSEL, 0, MAKELPARAM(::oGet:Pos - 1, ::oGet:Pos - 1))
             endif
             nKey := NIL
   #endif  // !NO_OBEY_PIC
          else
             nKey := K_DEL
          endif
       ENDCASE

       IF nKey != nil .and. nKey != 0
             nRet := ::ApplyKey(nVK, nKey)
             nRet := -1
       ENDIF
   else
       WG_DebugTrace("TXGet:OnKeyDown", "to ::nProc")
       // was: nRet = ::CallWindowProc()
       if ( nVK == 86 .and. lCtrl )    // Ctrl+V -> paste: see above !!!
           ::Paste() //SendMessage(::hWnd, WM_PASTE, 0, 0)
           nRet := 0
       //else
       //    WG_DebugTrace("TXGet:OnKeyDown", "to ::nProc")
       //    nRet = ::CallWindowProc()
       endif
   ENDIF

   do case
   case ::oGet:ExitState == GE_NOEXIT
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

