/*
 * $Id$
 */

#include "vxh.ch"
#include "debug.ch"

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS FilterUI INHERIT Dialog
   DATA oReport EXPORTED
   
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OnOk()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oReport ) CLASS FilterUI
   LOCAL lProp
   ::oReport  := oReport
   DEFAULT ::__xCtrlName  TO "FilterUI"

   ::Super:Init( ::Application:MainForm )

   ::Modal      := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS FilterUI
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
         //:EventHandler[ "OnClick" ] := "ANDRadioButton_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "ORRadioButton"
         :Left                 := 325
         :Top                  := 27
         :Width                := 259
         :Height               := 15
         :Caption              := "Match ANY of the conditions"
         //:EventHandler[ "OnClick" ] := "ORRadioButton_OnClick"
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

         :Left                 := 2
         :Top                  := 14
         :Width                := 586
         :Height               := 208
         //:EventHandler[ "OnClick" ] := "ConditionPanel_OnClick"
         :Create()
         WITH OBJECT ( COMBOBOX( :this ) )
            :Name                 := "ConditionFieldComboBox1"
            WITH OBJECT :ToolTip
               :Text                 := "Select field"
            END

            :VertScroll           := .T.
            :Left                 := 10
            :Top                  := 10
            :Width                := 150
            :Height               := 200
            :SelectionHeight      := 17
            :ItemHeight           := 17
            //:EventHandler[ "OnCBNSelEndOk" ] := "ConditionFieldComboBox_OnCBNSelEndOk"
            :Create()
         END //COMBOBOX

         WITH OBJECT ( COMBOBOX( :this ) )
            :Name                 := "ConditionComboBox1"
            WITH OBJECT :ToolTip
               :Text                 := "Select condition"
            END

            :VertScroll           := .T.
            :Left                 := 165
            :Top                  := 10
            :Width                := 150
            :Height               := 200
            :Enabled              := .F.
            :SelectionHeight      := 17
            :ItemHeight           := 17
            //:EventHandler[ "OnCBNSelEndOk" ] := "ConditionComboBox_OnCBNSelEndOk"
            :Create()
         END //COMBOBOX

         WITH OBJECT ( EDITBOX( :this ) )
            :Name                 := "ConditionValueEditBox1"
            :Left                 := 320
            :Top                  := 10
            :Width                := 150
            :Height               := 22
            :Enabled              := .F.
            :AutoHScroll          := .T.
            :Case                 := 2
            //:EventHandler[ "OnChar" ] := "ConditionValueEditBox1_OnChar"
            :Create()
         END //EDITBOX

         WITH OBJECT ( EDITBOX( :this ) )
            :Name                 := "ConditionValueEditBoxSec1"
            :Left                 := 480
            :Top                  := 10
            :Width                := 0
            :Height               := 22
            :AutoHScroll          := .T.
            //:EventHandler[ "OnChar" ] := "ConditionValueEditBoxSec1_OnChar"
            :Create()
         END //EDITBOX

         WITH OBJECT ( LABEL( :this ) )
            :Name                 := "ConditionValueLabel1"
            WITH OBJECT :ToolTip
               :Text                 := "Condition value"
            END

            :Left                 := 320
            :Top                  := 14
            :Width                := 0
            :Height               := 16
            :Create()
         END //LABEL

         WITH OBJECT ( BUTTON( :this ) )
            :Name                 := "RemoveConditionButton1"
            WITH OBJECT :ToolTip
               :Text                 := "Remove condition"
            END

            WITH OBJECT :Dock
               :Margins              := "0,0,0,0"
            END

            :Left                 := 475
            :Top                  := 10
            :Width                := 20
            :Height               := 22
            :Caption              := "-"
            //:EventHandler[ "OnClick" ] := "RemoveConditionButton_OnClick"
            :Create()
         END //BUTTON

         WITH OBJECT ( BUTTON( :this ) )
            :Name                 := "AddConditionButton1"
            WITH OBJECT :ToolTip
               :Text                 := "Add more condition"
            END

            WITH OBJECT :Dock
               :Margins              := "0,0,0,0"
            END

            :Left                 := 500
            :Top                  := 10
            :Width                := 20
            :Height               := 22
            :Caption              := "+"
            //:EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
            :Create()
         END //BUTTON

         WITH OBJECT ( BUTTON( :this ) )
            :Name                 := "MoreConditionButton1"
            WITH OBJECT :ToolTip
               :Text                 := "More..."
            END

            :Left                 := 525
            :Top                  := 10
            :Width                := 20
            :Height               := 22
            :Caption              := "..."
            //:EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
            :Create()
         END //BUTTON

         WITH OBJECT ( COMBOBOX( :this ) )
            :Name                 := "AndOrComboBox1"
            :Left                 := 13
            :Top                  := 53
            :Width                := 250
            :Height               := 100
            :Visible              := .F.
            :SelectionHeight      := 17
            :ItemHeight           := 17
            :Create()
         END //COMBOBOX

         WITH OBJECT ( CHECKBOX( :this ) )
            :Name                 := "AskLaterCheckBox1"
            WITH OBJECT :ToolTip
               :Text                 := "Ask me later"
            END

            :Left                 := 553
            :Top                  := 14
            :Width                := 15
            :Height               := 15
            //:EventHandler[ "OnClick" ] := "AskLaterCheckBox_OnClick"
            :Create()
         END //CHECKBOX

      END //PANEL

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
      //:EventHandler[ "OnClick" ] := "cmdFilterBrowse_OnClick"
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
      //:EventHandler[ "OnClick" ] := "cmdFilterHelp_OnClick"
      :Create()
   END //BUTTON
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

