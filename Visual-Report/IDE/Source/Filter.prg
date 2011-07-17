/*
 * $Id$
 */

#include "vxh.ch"
#include "debug.ch"
#define FILTERCTRLPERLINE      10

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS FilterUI INHERIT Dialog
   DATA cFilter    EXPORTED INIT ""
   DATA aFilter    EXPORTED INIT {}
   DATA oDataTable EXPORTED
   DATA aCondStr   EXPORTED INIT {}
   DATA aCondVal   EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OnOk()

   METHOD ConditionComboBox_OnCBNSelEndOk()
   METHOD RemoveConditionButton_OnClick()
   METHOD AddConditionButton_OnClick()
   METHOD MoreConditionButton_OnClick()
   METHOD FilterBrowse_OnClick()
   METHOD LoadFieldList()
   METHOD AddButtons()
   METHOD BuildFilterExp()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oDataTable ) CLASS FilterUI
   LOCAL lProp
   ::oDataTable  := oDataTable
   DEFAULT ::__xCtrlName  TO "FilterUI"

   ::Super:Init( ::Application:MainForm )

   ::aCondStr := { "equals to",;
                   "is not equal to",;
                   "greater than",;
                   "less than",;
                   "between",;
                   "begins with",;
                   "does not begin with",;
                   "contains",;
                   "does not contain",;
                   "is empty",;
                   "is not empty",;
                   "is in the range" }

   ::Modal      := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS FilterUI

   ::Left   := 190
   ::Top    := 20
   ::Width  := 634
   ::Height := 375

   WITH OBJECT ( GROUPBOX( Self ) )
      WITH OBJECT :Dock
         :Left                 := Self
         :Top                  := Self
         :Right                := Self
         :Margins              := "20,15,20,0"
      END
      :Left                 := 20
      :Top                  := 15
      :Width                := 590
      :Height               := 62
      :Caption              := "Setting"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "ANDRadioButton"
         :Left                 := 24
         :Top                  := 27
         :Width                := 289
         :Height               := 15
         :Caption              := "Match ALL of the conditions"
         :InitialState         := 1
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "ORRadioButton"
         :Left                 := 325
         :Top                  := 27
         :Width                := 259
         :Height               := 15
         :Caption              := "Match ANY of the conditions"
         :Create()
      END
   END

   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "ConditionGroupBox"
      :Dock:Margins         := "20,86,20,40"
      :Left                 := 20
      :Top                  := 86
      :Width                := 590
      :Height               := 224
      :Caption              := "Conditions"
      :ForeColor            := 0
      :Create()
      :DockToParent()
      WITH OBJECT ( PANEL( :this ) )
         :Name                 := "ConditionPanel"
         :Dock:Margins         := "2,14,2,2"
         :VertScroll           := .T.
         :Create()
         :DockToParent()
      END
      //---------------------------
      ::AddConditionButton_OnClick()
      //---------------------------
   END

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "FilterBrowse"
      WITH OBJECT :Dock
         :Right                := Self
         :Bottom               := Self
         :Margins              := "0,0,20,10"
      END
      :Left                 := 530
      :Top                  := 315
      :Width                := 80
      :Height               := 25
      :Caption              := "Browse"
      :EventHandler[ "OnClick" ] := "FilterBrowse_OnClick"
      :Create()
   END

   ::CenterWindow()
RETURN NIL

METHOD OnOk() CLASS FilterUI
   LOCAL n
   ImageListDestroy( ::ImageList:Handle )

   FOR n := 1 TO LEN( ::aDeleted )
       ::Application:Project:RemoveImage( ::aDeleted[n], ::ImageList )
   NEXT
   
   ::ImageList:Images := ACLONE( ::DataGrid1:ImageList:Images )
   IF ::DataGrid1:ImageList:MaskColor != NIL
      ::ImageList:MaskColor := ::DataGrid1:ImageList:MaskColor
   ENDIF
   ::ImageList:Create()
   ::Application:Project:Modified := .T.
   ::Close( IDOK )
RETURN NIL

//----------------------------------------------------------------------------------------------------//
METHOD AddConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )

      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .F.
   ENDIF
   WITH OBJECT ::ConditionPanel
      WITH OBJECT ( PANEL( :this ) )
         :Left           := 0
         :Top            := 0
         :Width          := 150
         :Height         := 30
         :Dock:Left      := :Parent
         :Dock:Right     := :Parent
         :Dock:Top       := IIF( LEN( ::ConditionPanel:Children ) > 0, ATAIL( ::ConditionPanel:Children ), :Parent )
         :Dock:TopMargin := 4
         :Create()
         :SetRedraw( .F. )

         WITH OBJECT ( COMBOBOX( :this ) )
            :ToolTip:Text         := "Select field"
            :VertScroll           := .T.
            :Left                 := 1
            :Top                  := 0
            :Width                := 150
            :Height               := 200
            :Create()

            ::LoadFieldList( :This )

            :Parent:Height := :SelectionHeight() + 7
         END
         :Parent:VertScrollSize := (:Height+4)*LEN( ::ConditionPanel:Children )

         WITH OBJECT ( COMBOBOX( :this ) )
            :ToolTip:Text         := "Select condition"
            :VertScroll           := .T.
            :Left                 := 165
            :Top                  := 0
            :Width                := 150
            :Height               := 200
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionComboBox_OnCBNSelEndOk"
            :Create()
            :AddItem( "equals to" )
            :AddItem( "is not equal to" )
            :AddItem( "greater than" )
            :AddItem( "less than" )
            :AddItem( "between" )
            :AddItem( "begins with" )
            :AddItem( "does not begin with" )
            :AddItem( "contains" )
            :AddItem( "does not contain" )
            :AddItem( "is empty" )
            :AddItem( "is not empty" )
            :AddItem( "is in the range" )
            :SetCurSel(1)
         END

         WITH OBJECT ( EDITBOX( :this ) )
            :Left                 := 320
            :Top                  := 0
            :Width                := 150
            :Height               := 22
            :AutoHScroll          := .T.
            :Case                 := 2
            :Create()
         END

         WITH OBJECT ( EDITBOX( :this ) )
            :Left                 := 394
            :Top                  := 0
            :Width                := 72
            :Height               := 22
            :Visible              := .F.
            :AutoHScroll          := .T.
            :Create()
         END //EDITBOX

         ::AddButtons( :this )
         
         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD AddButtons( oParent ) CLASS FilterUI
   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text         := "Remove condition"
      :Left                 := 475
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "-"
      :EventHandler[ "OnClick" ] := "RemoveConditionButton_OnClick"
      :Create()
   END

   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text         := "Add more condition"
      :Left                 := 500
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "+"
      :EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
      :Create()
   END

   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text         := "More..."
      :Left                 := 525
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "..."
      :EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
      :Create()
   END

RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
   LOCAL oPanel := Sender:Parent

   oPanel:Children[3]:Enabled := .T.
   IF Sender:CurSel == 5
      IF !oPanel:Children[4]:Visible
         oPanel:Children[3]:Width := 72
         oPanel:Children[4]:Visible := .T.
      ENDIF
    ELSE
      IF oPanel:Children[4]:Visible
         oPanel:Children[3]:Width := 150
         oPanel:Children[4]:Caption := ""
         oPanel:Children[4]:Visible := .F.
      ENDIF
      IF Sender:CurSel == 10 .OR. Sender:CurSel == 11
         oPanel:Children[3]:Caption := ""
         oPanel:Children[3]:Enabled := .F.
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RemoveConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL oDock, n, oLastPanel
   IF LEN( ::ConditionPanel:Children ) > 1
      oDock := Sender:Parent:Dock:Top
      n := ASCAN( ::ConditionPanel:Children, {|o| o:hWnd == Sender:Parent:hWnd} )
      IF n > 0 .AND. LEN( ::ConditionPanel:Children ) >= n+1
         ::ConditionPanel:Children[n+1]:Dock:Top := oDock
         ::ConditionPanel:Children[n+1]:DockIt()
      ENDIF
      Sender:Parent:Destroy()
      IF LEN( ::ConditionPanel:Children ) > 0
         oLastPanel := ATAIL( ::ConditionPanel:Children )
         oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .T.
         oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .T.
      ENDIF
   ENDIF
   ::ConditionPanel:VertScrollSize := (ATAIL( ::ConditionPanel:Children ):Height+4)*LEN( ::ConditionPanel:Children )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MoreConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )
      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .F.
   ENDIF
   WITH OBJECT ::ConditionPanel
      WITH OBJECT ( PANEL( :this ) )
         :Left           := 0
         :Top            := 0
         :Width          := 150
         :Height         := 30
         :Dock:Left      := :Parent
         :Dock:Right     := :Parent
         :Dock:Top       := IIF( LEN( ::ConditionPanel:Children ) > 0, ATAIL( ::ConditionPanel:Children ), :Parent )
         :Dock:TopMargin := 4
         :Create()
         :SetRedraw( .F. )

         WITH OBJECT ( COMBOBOX( :this ) )
            :VertScroll           := .T.
            :Left                 := 1
            :Top                  := 0
            :Width                := 250
            :Height               := 100
            :Create()

            :AddItem( "Match ALL of the following conditions" )
            :AddItem( "Match ANY of the following conditions" )
            :SetCurSel(1)

            :Parent:Height := :SelectionHeight() + 7
         END
         :Parent:VertScrollSize := (:Height+4)*LEN( ::ConditionPanel:Children )

         ::AddButtons( :this )

         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD FilterBrowse_OnClick( Sender ) CLASS FilterUI
   ::BuildFilterExp()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD BuildFilterExp() CLASS FilterUI
   ::cFilter := ""
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD LoadFieldList( oComboBox ) CLASS FilterUI
   LOCAL n, i, aFields
   oComboBox:ResetContent()
   aFields := ::oDataTable:EditCtrl:Struct()
   FOR n := 1 TO LEN( aFields )
       oComboBox:AddItem( aFields[n][1] )
   NEXT
   oComboBox:SetCurSel(1)
RETURN NIL

