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

// Menu definitions
CLASS WG_TMenuItem FROM WG_TObject
    // Base
    DATA nFlags
    DATA nIDItem       // nIDItem o oSubMenu
    DATA nPos         AS NUMERIC
    DATA cItem        AS STRING
    DATA bAction      AS CODEBLOCK
    DATA cMessage     AS STRING
    DATA oParentMenu  AS OBJECT

    DATA lChecked  AS LOGICAL INIT FALSE
    DATA lEnabled  AS LOGICAL INIT TRUE
    DATA lGrayed   AS LOGICAL INIT FALSE

    // METODI
    METHOD New() CONSTRUCTOR
    METHOD Check( lChecked )    INLINE IIF( lChecked == NIL, lChecked := TRUE, NIL ),;
                                       ::lChecked := lChecked ,;
                                       CheckMenuItem( ::oParentMenu:nHandle, ::oParentMenu:GetPosition( ::nIDItem )-1, ;
                                                       MF_BYPOSITION + IIF( ::lChecked, MF_CHECKED, MF_UNCHECKED ) )
    METHOD Disable()            INLINE ::Enable( FALSE )
    METHOD Enable( lEnabled )   INLINE IIF( lEnabled == NIL, lEnabled := TRUE, NIL ),;
                                       ::lEnabled := lEnabled, ;
                                       EnableMenuItem( ::oParentMenu:nHandle, ::oParentMenu:GetPosition( ::nIDItem )-1, ;
                                                      MF_BYPOSITION + IIF( ::lEnabled, MF_ENABLED, MF_DISABLED + MF_GRAYED ) )
    METHOD ExecAction()         BLOCK  {|Self, oWin| Eval( ::bAction, oWin ) }
    METHOD GetLabel()           INLINE StrTran( ::cItem, "&", "" )
    METHOD GetState()           INLINE GetMenuState(::oParentMenu:nHandle, ::oParentMenu:GetPosition( ::nIDItem )-1, ;
                                                    MF_BYPOSITION )
    METHOD Gray( lGrayed )      INLINE IIF( lGrayed == NIL, lGrayed := TRUE, NIL ),;
                                       ::lGrayed := lGrayed,;
                                       EnableMenuItem( ::oParentMenu:nHandle, ::oParentMenu:GetPosition( ::nIDItem )-1, ;
                                                       MF_BYPOSITION + IIF( ::lGrayed, MF_GRAYED, 0 ) + ;
                                                       IIF( ::lEnabled, MF_ENABLED, MF_DISABLED ) )
    METHOD Hilite( lHilite )    INLINE IIF( lHilite == NIL, lHilite := TRUE, NIL ),;
                                       ::lHilite := lHilite ,;
                                       HiliteMenuItem( ::oParentMenu:nHandle, ::oParentMenu:GetPosition( ::nIDItem )-1, ;
                                                       MF_BYPOSITION + IIF( ::lHilite, MF_HILITE, MF_UNHILITE ) )
    METHOD IsChecked()          INLINE AND( ::GetState(), MF_CHECKED ) != 0
    METHOD IsEnabled()          INLINE AND( ::GetState(), MF_DISABLED ) == 0
    METHOD IsGrayed()           INLINE AND( ::GetState(), MF_GRAYED ) != 0
    METHOD IsHilite()           INLINE AND( ::GetState(), MF_HILITE ) != 0
    METHOD IsPopup()            INLINE AND( ::GetState(), MF_POPUP ) != 0
    METHOD HasAction()          INLINE ( ValType( ::bAction ) == "B" )
    METHOD SetAction( bAction ) INLINE LOCAL bOld,;
                                       bOld := ::bAction ,;
                                       ::bAction := bAction,;
                                       bOld
    METHOD SetID()
    METHOD SetParent( oMenu )   INLINE LOCAL oOld,;
                                       oOld := ::oParentMenu,;
                                       ::oParentMenu := oMenu,;
                                       oOld
    METHOD Separator()          INLINE ::New( MF_SEPARATOR )
    METHOD UnCheck()            INLINE ::Check( FALSE )
    METHOD UnGray()             INLINE ::Gray( FALSE )
    METHOD UnHilite()           INLINE ::Hilite( FALSE )


    METHOD OnHilite()
ENDCLASS

METHOD New( nFlags, nIDItem, cItem, bAction, lChecked, lEnabled, lGrayed, cMsg ) CLASS WG_TMenuItem

    DEFAULT nFlags TO 0
    ::nFlags := MF_ENABLED + MF_STRING + nFlags

    IF lChecked <> NIL
       ASSIGN ::lChecked WITH lChecked
       ::nFlags += IIF( ::lChecked, MF_CHECKED, 0 )
    ENDIF
    IF lEnabled <> NIL
       ASSIGN ::lEnabled WITH lEnabled
       ::nFlags += IIF( ::lEnabled, 0, MF_DISABLED + MF_GRAYED )
    ENDIF
    IF lGrayed <> NIL
       ASSIGN ::lGrayed WITH lGrayed
       ::nFlags += IIF( ::lGrayed, MF_GRAYED, 0 )
    ENDIF

    ASSIGN ::nIDItem  WITH nIDItem DEFAULT 100
    ASSIGN ::cItem    WITH cItem   DEFAULT "Menu_1"
    ASSIGN ::bAction  WITH bAction
    ASSIGN ::cMessage WITH cMsg    DEFAULT ""

RETURN Self

METHOD SetID( nIDItem ) CLASS WG_TMenuItem
   LOCAL nOldIDItem := ::nIDItem
   ASSIGN ::nIDItem WITH nIDItem
RETURN nOldIDItem

METHOD OnHilite( oWndParent ) CLASS WG_TMenuItem

   WG_DebugTrace( "TMenuItem:OnHilite()" )

   IF ::cMessage <> NIL .AND. ;
      oWndParent <> NIL .AND. ;
      oWndParent:oStatusBar <> NIL
      //MessageBox( ,"Menu Message = " + cStr( oItem:cMessage ) )
      oWndParent:oStatusBar:SetValue( ::cMessage )
   ENDIF

RETURN Self


/*
METHOD Check( lChecked AS LOGICAL ) CLASS WG_TMenuItem
   LOCAL lOldChecked := ::lChecked
   ASSIGN ::lChecked WITH lChecked DEFAULT FALSE

RETURN lOldChecked
*/
    