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
   DATA nRow       EXPORTED INIT 0
   DATA cFilter    EXPORTED INIT ""
   DATA aFilter    EXPORTED INIT {}
   DATA oDataTable EXPORTED
   DATA lPrefChanged
   DATA lAskLaterConfirmed
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OnOk()

   METHOD ANDRadioButton_OnClick()
   METHOD ORRadioButton_OnClick()
   METHOD ConditionPanel_OnClick()
   METHOD ConditionFieldComboBox_OnCBNSelEndOk()
   METHOD ConditionComboBox_OnCBNSelEndOk()
   METHOD ConditionValueEditBox1_OnChar()
   METHOD ConditionValueEditBoxSec1_OnChar()
   METHOD RemoveConditionButton_OnClick()
   METHOD AddConditionButton_OnClick()
   METHOD MoreConditionButton_OnClick()
   METHOD AskLaterCheckBox_OnClick()
   METHOD cmdFilterBrowse_OnClick()
   METHOD cmdFilterHelp_OnClick()
   METHOD CompleteComboboxList()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oDataTable ) CLASS FilterUI
   LOCAL lProp
   ::oDataTable  := oDataTable
   DEFAULT ::__xCtrlName  TO "FilterUI"

   ::Super:Init( ::Application:MainForm )

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
      :Name                 := "GroupBox7"
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
         :EventHandler[ "OnClick" ] := "ANDRadioButton_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "ORRadioButton"
         :Left                 := 325
         :Top                  := 27
         :Width                := 259
         :Height               := 15
         :Caption              := "Match ANY of the conditions"
         :EventHandler[ "OnClick" ] := "ORRadioButton_OnClick"
         :Create()
      END //RADIOBUTTON

   END //GROUPBOX

   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "ConditionGroupBox"
      WITH OBJECT :Dock
         :Left                 := Self
         :Top                  := Self
         :Right                := Self
         :Bottom               := Self
         :Margins              := "20,86,20,40"
      END

      :Left                 := 20
      :Top                  := 86
      :Width                := 590
      :Height               := 224
      :Caption              := "Conditions"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( PANEL( :this ) )
         :Name                 := "ConditionPanel"
         WITH OBJECT :Dock
            :Left                 := "ConditionGroupBox"
            :Top                  := "ConditionGroupBox"
            :Right                := "ConditionGroupBox"
            :Bottom               := "ConditionGroupBox"
            :Margins              := "2,14,2,2"
         END
         :VertScroll           := .T.
         :EventHandler[ "OnClick" ] := "ConditionPanel_OnClick"
         :Create()
         :DockIt()
      END
      //---------------------------
      ::AddConditionButton_OnClick()
      //---------------------------

   END //GROUPBOX

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "cmdFilterBrowse"
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
      :EventHandler[ "OnClick" ] := "cmdFilterBrowse_OnClick"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "cmdFilterHelp"
      WITH OBJECT :Dock
         :Left                 := Self
         :Bottom               := Self
         :Margins              := "20,0,0,10"
      END

      :Left                 := 20
      :Top                  := 315
      :Width                := 24
      :Height               := 25
      :Caption              := "?"
      :EventHandler[ "OnClick" ] := "cmdFilterHelp_OnClick"
      :Create()
   END //BUTTON
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
METHOD ANDRadioButton_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ORRadioButton_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionPanel_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionFieldComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD AddConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName
   WITH OBJECT ::ConditionPanel
      WITH OBJECT ( PANEL( :this ) )
         cName       := "PanelHolder"
         :Left       := 0
         :Top        := 0
         :Width      := 150
         :Height     := 30
         :Name       := cName + XSTR( ::nRow+1 )
         :Dock:Left  := :Parent
         :Dock:Right := :Parent
         IF ::nRow > 0
            :Dock:Top       := cName + XSTR( ::nRow )
            :Dock:TopMargin := 4
         ENDIF
         :EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
         :Create()

         WITH OBJECT ( COMBOBOX( :this ) )
            :ToolTip:Text         := "Select field"
            :Name                 := "ConditionFieldComboBox" + XSTR( ::nRow+1 )
            :VertScroll           := .T.
            :Left                 := 1
            :Top                  := 0
            :Width                := 150
            :Height               := 200
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionFieldComboBox_OnCBNSelEndOk"
            :Create()

            ::CompleteComboboxList( :This )

            :Parent:Height := :SelectionHeight() + 7
         END
         :Parent:VertScrollSize := (:Height+4)*(::nRow+1)

         WITH OBJECT ( COMBOBOX( :this ) )
            :ToolTip:Text         := "Select condition"
            :Name                 := "ConditionComboBox" + XSTR( ::nRow+1 )
            :VertScroll           := .T.
            :Left                 := 165
            :Top                  := 0
            :Width                := 150
            :Height               := 200
            :Enabled              := .F.
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionComboBox_OnCBNSelEndOk"
            :Create()
         END

         WITH OBJECT ( EDITBOX( :this ) )
            :Name                 := "ConditionValueEditBox" + XSTR( ::nRow+1 )
            :Left                 := 320
            :Top                  := 0
            :Width                := 150
            :Height               := 22
            :Enabled              := .F.
            :AutoHScroll          := .T.
            :Case                 := 2
            :EventHandler[ "OnChar" ] := "ConditionValueEditBox1_OnChar"
            :Create()
         END

         WITH OBJECT ( LABEL( :this ) )
            :Name                 := "ConditionValueLabel" + XSTR( ::nRow+1 )
            :ToolTip:Text         := "Condition value"
            :Left                 := 320
            :Top                  := 3
            :Width                := 0
            :Height               := 16
            :Create()
         END

         WITH OBJECT ( BUTTON( :this ) )
            :ToolTip:Text         := "Remove condition"
            :Name                 := "RemoveConditionButton" + XSTR( ::nRow+1 )
            :Left                 := 475
            :Top                  := 0
            :Width                := 20
            :Height               := 22
            :Caption              := "-"
            :EventHandler[ "OnClick" ] := "RemoveConditionButton_OnClick"
            :Create()
         END

         WITH OBJECT ( BUTTON( :this ) )
            :ToolTip:Text         := "Add more condition"
            :Name                 := "AddConditionButton" + XSTR( ::nRow+1 )
            :Left                 := 500
            :Top                  := 0
            :Width                := 20
            :Height               := 22
            :Caption              := "+"
            :EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
            :Create()
         END

         WITH OBJECT ( BUTTON( :this ) )
            :Name                 := "MoreConditionButton" + XSTR( ::nRow+1 )
            :ToolTip:Text         := "More..."
            :Left                 := 525
            :Top                  := 0
            :Width                := 20
            :Height               := 22
            :Caption              := "..."
            :EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
            :Create()
         END

         WITH OBJECT ( CHECKBOX( :this ) )
            :Name                 := "AskLaterCheckBox" + XSTR( ::nRow+1 )
            :ToolTip:Text         := "Ask me later"
            :Left                 := 553
            :Top                  := 4
            :Width                := 15
            :Height               := 15
            :EventHandler[ "OnClick" ] := "AskLaterCheckBox_OnClick"
            :Create()
         END

      END

   END
   ::nRow ++
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MoreConditionButton_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RemoveConditionButton_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD AskLaterCheckBox_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD cmdFilterBrowse_OnClick( Sender ) CLASS FilterUI
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD cmdFilterHelp_OnClick( Sender ) CLASS FilterUI
RETURN Self

METHOD ConditionValueEditBox1_OnChar() CLASS FilterUI
RETURN Self

METHOD ConditionValueEditBoxSec1_OnChar() CLASS FilterUI
RETURN Self


METHOD CompleteComboboxList( oComboBox ) CLASS FilterUI
   LOCAL n, i, aFields
   oComboBox:ResetContent()
   aFields := ::oDataTable:EditCtrl:Struct()
   FOR n := 1 TO LEN( aFields )
       oComboBox:AddItem( aFields[n][1] )
   NEXT
RETURN NIL

