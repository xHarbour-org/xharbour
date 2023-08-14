/*
 * $Id$
 */
#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

#define PBS_NORMAL     1
#define PBS_HOT        2
#define PBS_PRESSED    3
#define PBS_DISABLED   4
#define PBS_DEFAULTED  5
#define BP_PUSHBUTTON  1

#define DG_ADDCONTROL      1
#define DG_PROPERTYCHANGED 3
#define DG_FONTCHANGED     5
#define DG_DELCOMPONENT    6

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS DebugTab INHERIT TabStrip
   METHOD OnSysCommand(n) INLINE IIF( n==SC_CLOSE, (::Hide(),0),)
   METHOD Close() INLINE ::Hide()
ENDCLASS

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS DebugBuild INHERIT ListBox
   METHOD Init() CONSTRUCTOR
   METHOD Create()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS DebugBuild

   DEFAULT ::__xCtrlName  TO "DebugBuild"

   #ifdef DLL
      Application := GetApplication()
   #endif

   ::Super:Init( oParent )
   ::HorzScroll   := .T.
   ::VertScroll   := .T.
   ::ClipChildren := .F.
   ::ClipSiblings := .F.
   ::HasStrings   := .T.
   ::Border       := 0
RETURN Self

//------------------------------------------------------------------------------------------
METHOD Create() CLASS DebugBuild
   ::Super:Create()
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ErrorListView INHERIT ListView
   METHOD OnParentNotify()
   METHOD ProcessErrors()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD ProcessErrors( aErrors ) CLASS ErrorListView
   LOCAL n
   ::ResetContent()

   FOR n := 1 TO LEN( aErrors )
       ::InsertItem( aErrors[n][1],, n )
       ::SetItemText( n, 1, aErrors[n][2] )
       ::SetItemText( n, 2, aErrors[n][3] )
       ::SetItemText( n, 3, aErrors[n][4] )
   NEXT
   ::EnsureVisible( 0, .F. )
   ::EnsureVisible( 1, .F. )
   ::EnsureVisible( 2, .F. )
   ::EnsureVisible( 3, .F. )
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnParentNotify() CLASS ErrorListView
   LOCAL aSel, oEditor

   IF ::Parent:hdr:code == NM_DBLCLK
      aSel := ::GetSelection()
      IF aSel != NIL
         FOR EACH oEditor IN ::Application:SourceEditor:aDocs
            IF Upper( oEditor:File ) == Upper( aSel[1] )
               ::Application:EditorPage:Select()
               oEditor:Select()
               oEditor:TreeItem:Select()
               oEditor:GoToLine( Val(aSel[2])-1 )
               ::Application:SourceEditor:SetFocus()
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
RETURN 0

//------------------------------------------------------------------------------------------
CLASS FindInFilesListView INHERIT ListView
   METHOD OnParentNotify()
  // METHOD OnNCCalcSize( nwParam, nlParam )  INLINE Super:OnNCCalcSize( nwParam, nlParam ), NIL
ENDCLASS

METHOD OnParentNotify() CLASS FindInFilesListView
   LOCAL aSel, oEditor

   IF ::Parent:hdr:code == NM_DBLCLK
      aSel := ::GetSelection()

      FOR EACH oEditor IN ::Application:SourceEditor:aDocs
         IF Upper( oEditor:FileName ) == Upper( aSel[1] )
            ::Application:EditorPage:Select()
            oEditor:Select()
            oEditor:TreeItem:Select()
            oEditor:GotoPosition( oEditor:PositionFromLine( Val(aSel[2])-1 ) + aSel[4]-1 )
            oEditor:SetSelection( oEditor:PositionFromLine( Val(aSel[2])-1 ) + aSel[4]-1, oEditor:PositionFromLine( Val(aSel[2])-1 ) + aSel[4]-1 + Len( ::Application:Project:__cFindText ) )

            ::Application:SourceEditor:SetFocus()
            EXIT
         ENDIF
      NEXT
   ENDIF
RETURN 0

