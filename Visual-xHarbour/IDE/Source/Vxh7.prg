/*
 * $Id$
 */

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS MenuEditor INHERIT Dialog
   DATA MenuObject       EXPORTED
   DATA ItemManager      EXPORTED
   DATA ItemEventManager EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD TabSelection()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oParent, oMenu ) CLASS MenuEditor

   ::MenuObject   := oMenu
   
   DEFAULT ::__xCtrlName  TO "MenuEditor"
   ::Super:Init( oParent )
   ::Caption    := "Menu Editor"
   ::Template   := "MENUEDIT"
   ::Modal      := .F.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS MenuEditor
   LOCAL oItem, oSub
   ::ThickFrame := .T.
   ::CenterWindow()
   CoolBar( Self )
   WITH OBJECT ::CoolBar1
      :Create()

      CoolBarBand( :this )
      WITH OBJECT :Band1
         :MinWidth  := 140
         :MinHeight := 22
         :Break()
         :AllowUndock := .T.

         ToolBar( :this )
         WITH OBJECT :ToolBar1
            :Create()

            :AddBitmap( -1, IDB_STD_SMALL_COLOR )

            :DrawArrows()

            ToolButton( :this )
            WITH OBJECT :ToolButton1
               :ImageIndex := ::System:StdIcons:FileNew
               :ToolTip    := "New Popup Item"
               :Action     := {|o| o:Parent:Parent:TreeView1:NewPopupItem() }
               :Create()
            END

         END

         :SetChevron()
         :SetChild( :ToolBar1 )
      END

   END

   StatusBar( Self )
   WITH OBJECT ::StatusBar1
      StatusBarPanel( :Parent:StatusBar1, , 120 )
      StatusBarPanel( :Parent:StatusBar1, ,  -1 )
      StatusBarPanel( :Parent:StatusBar1, , 250 )
      :Create()
      :DockIt()
   END

   Panel( Self )
   WITH OBJECT ::Panel1
      :Width         := 252
      :Dock:Margin   := 4
      :Dock:Top      := :Parent:CoolBar1
      :Dock:Bottom   := :Parent:StatusBar1
      :Dock:Right    := :Parent
      :Create()
      :DockIt()
   
      TabControl( :this )
      WITH OBJECT :TabControl1
         :Width         := 252
         :Height        := 22
         :BoldSelection := .T.
         :Flat          := TRUE
         :Frame         := FALSE
//         :Dock:Margin   := 4
         :Dock:Top      := :Parent
         :Dock:Left     := :Parent
         :Dock:Right    := :Parent

         :Create()
         :InsertTab( "  Properties ", 0 )
         :InsertTab( "  Events ", 1 )
         :OnSelChanged := {|o,x,y|o:Parent:Parent:TabSelection( x,y ) }
         :DockIt()
      END

      ::ItemManager := ItemManager( :this )
      WITH OBJECT :ObjManager1
         :Width         := 252
         :Height        := 500
         :Dock:TopMargin:= 2
         :Caption       := NIL
         :Dock:Top      := :Parent:TabControl1
         :Dock:Bottom   := :Parent
         :Dock:Left     := :Parent
         :Dock:Right    := :Parent

         :FullRowSelect := .T.

         :NoHScroll     := .T.
         :HasButtons    := .T.
         :LinesAtRoot   := .T.
         :ShowSelAlways := .T.

         :Columns := { {100,C_LIGHTYELLOW}, {120,C_WHITE} }
         :Create()
         :DockIt()
         :BackColor := GetSysColor( COLOR_BTNFACE )
         :ExpandAll()
      END

      ::ItemEventManager := ItemEventManager( :this )
      WITH OBJECT :EvtManager1
         :Width         := 252
         :Height        := 500
         :Dock:TopMargin:= 2
         :Caption       := NIL
         :Dock:Top      := :Parent:TabControl1
         :Dock:Bottom   := :Parent
         :Dock:Left     := :Parent
         :Dock:Right    := :Parent

         :FullRowSelect := .T.

         :NoHScroll     := .T.
         :HasButtons    := .T.
         :LinesAtRoot   := .T.
         :ShowSelAlways := .T.

         :Columns := { {100,C_LIGHTYELLOW}, {120,C_WHITE} }
         :Create()
         :DockIt()
         :BackColor := GetSysColor( COLOR_BTNFACE )
         :ExpandAll()
         :Hide()
      END
   END
   MenuManager( Self )
   WITH OBJECT ::TreeView1
      :Width       := 500
      :Height      := 500
      :Dock:Margin := 4
      :Dock:Left   := :Parent
      :Dock:Top    := :Parent:CoolBar1
      :Dock:Right  := :Parent:Panel1
      :Dock:Bottom := :Parent:StatusBar1
      :AutoWidth   := .T.
      :AutoDock    := .F.
      :Create()
      :DockIt()
      
      :ResetTree()
      :Parent:ItemManager:ResetContent()

   END
   ::MoveWindow()
   
RETURN NIL

//------------------------------------------------------------------------------------------

METHOD TabSelection( nPrev, nCur ) CLASS MenuEditor
   IF nCur == 1
      ::ItemEventManager:Hide()
      ::ItemManager:Show()
    ELSE
      ::ItemEventManager:Show()
      ::ItemManager:Hide()
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ItemManager INHERIT ObjManager
   METHOD SetValue( x ) INLINE ::Super:SetValue( x ), ::Parent:Parent:Parent:TreeView1:ResetTree()
   METHOD ResetProperties()
ENDCLASS

METHOD ResetProperties( aSel ) CLASS ItemManager
   ::Super:ResetProperties( aSel )
RETURN Self

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ItemEventManager INHERIT EventManager
ENDCLASS

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS MenuManager INHERIT TreeView
   METHOD NewPopupItem()
   METHOD ResetTree()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD NewPopupItem() CLASS MenuManager
    LOCAL oItem
    oItem := CoolMenuItem( ::Parent:MenuObject )
    WITH OBJECT oItem
       :Create()
    END
    ::ResetTree()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD ResetTree() CLASS MenuManager
   LOCAL oItem, oSub
   ::ResetContent()
   FOR EACH oItem IN ::Parent:MenuObject:Children
       oSub := ::AddItem( oItem:Caption )
       oSub:Cargo := oItem
       oSub:Action := {|o|o:Parent:Parent:ItemManager:ResetProperties( {{ o:Cargo }} ),;
                          o:Parent:Parent:ItemEventManager:ResetEvents( {{ o:Cargo }} ) }
   NEXT
RETURN Self

//------------------------------------------------------------------------------------------

