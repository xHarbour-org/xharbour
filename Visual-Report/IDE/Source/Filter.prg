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
   DATA oReport EXPORTED

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

         :Left                 := 2
         :Top                  := 14
         :Width                := 586
         :Height               := 208
         :EventHandler[ "OnClick" ] := "ConditionPanel_OnClick"
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
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionFieldComboBox_OnCBNSelEndOk"
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
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionComboBox_OnCBNSelEndOk"
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
            :EventHandler[ "OnChar" ] := "ConditionValueEditBox1_OnChar"
            :Create()
         END //EDITBOX

         WITH OBJECT ( EDITBOX( :this ) )
            :Name                 := "ConditionValueEditBoxSec1"
            :Left                 := 480
            :Top                  := 10
            :Width                := 0
            :Height               := 22
            :AutoHScroll          := .T.
            :EventHandler[ "OnChar" ] := "ConditionValueEditBoxSec1_OnChar"
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
            :EventHandler[ "OnClick" ] := "RemoveConditionButton_OnClick"
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
            :EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
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
            :EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
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
            :EventHandler[ "OnClick" ] := "AskLaterCheckBox_OnClick"
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
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ORRadioButton_OnClick( Sender ) CLASS FilterUI
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionPanel_OnClick( Sender ) CLASS FilterUI

RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionFieldComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI

   wf_Filter_ConditionFieldChanged("wf_Print", ::this, Sender, "wf_Print_Alias1", "wf_Print_Alias2")

   wf_AskLaterSetup(::this, Val(wf_OnlyDigit(Sender:Name)))
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD AddConditionButton_OnClick( Sender ) CLASS FilterUI
   //Maak een reeks nieuwe filter objecten aan
   //Make a series of new filter objects

   LOCAL nTemp

   nTemp:= Val(wf_OnlyDigit(Sender:Name))

   wf_Filter_AddCondition("wf_Print", ::this, nTemp)

   AEVAL( ::ConditionGroupBox:Children, {|o| o:DockIt()} )
   IF !Sender:Parent:VertScroll .AND. LEN(Sender:Parent:Children)/FILTERCTRLPERLINE > 5
      Sender:Parent:VertScroll := .T.
   ENDIF
   ::Application:Yield()

   ::&("ConditionComboBox" + V(nTemp + 1)):Visible:= .T.
   ::&("ConditionFieldComboBox" + V(nTemp + 1)):Visible:= .T.
   ::&("ConditionValueEditBox" + V(nTemp + 1)):Visible:= .T.
   Sender:Enabled:= .F.
   ::&("MoreConditionButton" + V(nTemp)):Enabled := .F.
   ::&("AskLaterCheckBox" + V(nTemp + 1)):Enabled := .T.

   wf_AskLaterSetup( ::This, nTemp )

   wf_Print_SetPrefChanged()

   RETURN Self

RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MoreConditionButton_OnClick( Sender ) CLASS FilterUI
   //Maak een reeks nieuwe filter objecten aan
   //Make a series of new filter objects

   LOCAL nTemp

   nTemp:= Val(wf_OnlyDigit(Sender:Name))

   wf_Filter_AddCondition("wf_Print", ::This, nTemp)
   AEVAL( ::ConditionGroupBox:Children, {|o| o:DockIt()} )
   IF !Sender:Parent:VertScroll .AND. LEN(Sender:Parent:Children)/FILTERCTRLPERLINE > 5
      Sender:Parent:VertScroll := .T.
   ENDIF
   ::Application:Yield()

   ::&( "AndOrComboBox"  + V(nTemp + 1)):ResetContent()
   ::&( "AndOrComboBox"  + V(nTemp + 1)):AddItem(wfl("Match ALL of the following conditions"))
   ::&( "AndOrComboBox"  + V(nTemp + 1)):AddItem(wfl("Match ANY of the following conditions"))
   ::&( "AndOrComboBox"  + V(nTemp + 1)):SetCurSel(1)
   ::&( "AndOrComboBox"  + V(nTemp + 1)):Visible := .T.

   ::&("ConditionComboBox" + V(nTemp + 1)):Visible:= .F.
   ::&("ConditionFieldComboBox" + V(nTemp + 1)):Visible:= .F.
   ::&("ConditionValueEditBox" + V(nTemp + 1)):Visible:= .F.

   Sender:Enabled:= .F.
   ::&("AddConditionButton" + V(nTemp)):Enabled := .F.
   ::&("AskLaterCheckBox" + V(nTemp + 1)):Enabled := .F.


   //::AddConditionButton_OnClick(::&("AddConditionButton" + V(nTemp+1)))

   wf_AskLaterSetup( ::This, nTemp )
   wf_Print_SetPrefChanged()
   RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RemoveConditionButton_OnClick( Sender ) CLASS FilterUI

   wf_Filter_RemoveCondition(::this, Sender)
   wf_RemoveAskLaterSetup(::this, Val(wf_OnlyDigit(Sender:Name)))
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
   wf_Filter_ConditionChanged("wf_Print", ::This, Sender)
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD AskLaterCheckBox_OnClick( Sender ) CLASS FilterUI
   var:lPrefChanged := .T.
   wf_Filter_AskLaterCheckBoxClicked(::This, Sender)
   wf_AskLaterSetup(::this, Val(wf_OnlyDigit(Sender:Name)))
   wf_Print_SetPrefChanged()
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD cmdFilterBrowse_OnClick( Sender ) CLASS FilterUI
   LOCAL cFilter

   IF wf_dbSelect("WF_PRINT_ALIAS1") == 0
      RETURN SELF
   ENDIF

   wfDB SELECT "WF_PRINT_ALIAS1"

   cFilter := wf_Filter_GetFilterString("wf_Print", ::this, "wf_Print_Alias1", "wf_Print_Alias2")
   IF var:lAskLaterConfirmed
      wf_FilterBrowse("wf_Print_Alias1", cFilter, ::Height, ::Width)
   ENDIF

RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD cmdFilterHelp_OnClick( Sender ) CLASS FilterUI
RETURN Self



