/*
 * $Id$
 */

#include "vxh.ch"
#include "debug.ch"

#define FC_EQUALTO    "Equals to"
#define FC_NOTEQUALTO "Is not equal to"
#define FC_GREATEREQU "Greater than or equal"
#define FC_LESSEQUAL  "Less than or equal"
#define FC_BETWEEN    "Between"
#define FC_INTHERANGE "Is in the range"
#define FC_CONTAINS   "Contains"
#define FC_NOTCONTAIN "Does not contain"
#define FC_BEGWITH    "Begins with"
#define FC_NOTBEGWITH "Does not begin with"
#define FC_ISEMPTY    "Is empty"
#define FC_NOTEMPTY   "Is not empty"
#define FC_PERQUARTER "Per quarter"
#define FC_INLAST     "Is in the last"
#define FC_NOTINLAST  "Is not in the last"
#define FC_TRUE       "True"
#define FC_FALSE      "False"

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS FilterUI INHERIT Dialog
   DATA cFilter      EXPORTED INIT ""
   DATA BuildFilter  EXPORTED
   DATA oDataTable   EXPORTED
   DATA oCond        EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()

   METHOD ConditionComboBox_OnCBNSelEndOk()
   METHOD FieldComboBox_OnCBNSelEndOk()
   METHOD RemoveConditionButton_OnClick()
   METHOD AddConditionButton_OnClick()
   METHOD MoreConditionButton_OnClick()
   METHOD FilterBrowse_OnClick()
   METHOD LoadFieldList()
   METHOD AddButtons()
   METHOD OK_OnClick()
   METHOD Cancel_OnClick()
   METHOD SetDateEdit()
   METHOD CheckBox_OnClick()
   METHOD GetFilterExp()
   METHOD AskSettings_OnClick()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oDataTable ) CLASS FilterUI
   LOCAL lProp
   IF EMPTY( oDataTable:Alias )
      ::Application:MainForm:MessageBox( "The Alias property cannot be empty", "DataTable" )
      RETURN NIL
   ENDIF
   ::oDataTable  := oDataTable
   DEFAULT ::__xCtrlName  TO "FilterUI"

   ::Super:Init( ::Application:MainForm )
   ::oCond := Conditions( NIL )
   ::Modal      := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS FilterUI
   LOCAL hFilter, aExps, n, i, aExp, hExp

   ::Left    := 190
   ::Top     := 20
   ::Width   := 750
   ::Height  := 375
   ::Caption := "Create Filter Expression"

   WITH OBJECT ( GROUPBOX( Self ) )
      :Dock:Left            := :Parent
      :Dock:Top             := :Parent
      :Dock:Right           := :Parent
      :Dock:Margins         := "20,15,20,0"
      :Left                 := 20
      :Top                  := 15
      :Width                := 590
      :Height               := 62
      :Caption              := "Setting"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name              := "ANDRadio"
         :Left              := 24
         :Top               := 27
         :Width             := 289
         :Height            := 15
         :Caption           := "Match ALL of the conditions"
         :InitialState      := 1
         :Create()
      END

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name              := "ORRadio"
         :Left              := 325
         :Top               := 27
         :Width             := 259
         :Height            := 15
         :Caption           := "Match ANY of the conditions"
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
         :Name              := "ConditionPanel"
         :Dock:Margins      := "2,14,2,2"
         :VertScroll        := .T.
         //:BackColor         := RGB(255,255,255)
         :Create()
         :DockToParent()
      END
   END
   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "FilterBrowse"
      :Dock:Left            := :Parent
      :Dock:Bottom          := :Parent
      :Dock:Margins         := "20,0,20,10"
      :Left                 := 530
      :Top                  := 315
      :Width                := 80
      :Height               := 25
      :Enabled              := .F.
      :Caption              := "Test Filter"
      :EventHandler[ "OnClick" ] := "FilterBrowse_OnClick"
      :Create()
   END

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Cancel"
      :Dock:Right           := :Parent
      :Dock:Bottom          := :Parent
      :Dock:Margins         := "0,0,20,10"
      :Left                 := 530
      :Top                  := 315
      :Width                := 80
      :Height               := 25
      :Caption              := "Cancel"
      :EventHandler[ "OnClick" ] := "Cancel_OnClick"
      :Create()
   END
   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "OK"
      :Dock:Right           := "Cancel"
      :Dock:Bottom          := :Parent
      :Dock:Margins         := "0,0,5,10"
      :Left                 := 530
      :Top                  := 315
      :Width                := 80
      :Height               := 25
      :Caption              := "OK"
      :EventHandler[ "OnClick" ] := "OK_OnClick"
      :Create()
   END
   ::AddConditionButton_OnClick()
   ::CenterWindow()
   
   hFilter := ::oDataTable:Filter

   IF VALTYPE( hFilter ) == "H"
      IF hFilter:ANDRadio == "1"
         ::ANDRadio:SetState( BST_CHECKED )
         ::ORRadio:SetState( BST_UNCHECKED )
       ELSE
         ::ANDRadio:SetState( BST_UNCHECKED )
         ::ORRadio:SetState( BST_CHECKED )
      ENDIF
      aExps := hFilter:Expressions

      FOR i := 1 TO LEN( aExps )
          ::ConditionPanel:Children[i]:Children[1]:Enabled := .T.

          ::ConditionPanel:Children[i]:Children[1]:SetCurSel( aExps[i]:FieldSel )
          ::FieldComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i]:Children[1] )

          ::ConditionPanel:Children[i]:Children[2]:SetCurSel( aExps[i]:ExpSel )
          ::ConditionComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i]:Children[2], aExps[i] )

          IF i < LEN( aExps )
             ::AddConditionButton_OnClick()
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD AddConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )
      IF LEN( oLastPanel:Children ) == 4
         oLastPanel:Children[-1]:Enabled := .F.
         oLastPanel:Children[-2]:Enabled := .F.
      ENDIF
      oLastPanel:Children[-3]:Enabled := .F.
   ENDIF
   ::FilterBrowse:Enabled := .F.
   ::OK:Enabled := .F.
   WITH OBJECT ::ConditionPanel
      WITH OBJECT ( ContCondPanel( :this ) )
         :Left           := 0
         :Top            := 0
         :Width          := 150
         :Height         := 30
         :Dock:Left      := :Parent
         :Dock:Right     := :Parent
         :Dock:Top       := IIF( LEN( ::ConditionPanel:Children ) > 0, ATAIL( ::ConditionPanel:Children ), :Parent )
         :Dock:TopMargin := 4
         //:BackColor      := RGB(255,255,255)
         :Create()
         :SetRedraw( .F. )

         WITH OBJECT ( COMBOBOX( :this ) )
            :ToolTip:Text         := "Select field"
            :VertScroll           := .T.
            :Left                 := 10
            :Top                  := 0
            :Width                := 150
            :Height               := 200
            :EventHandler[ "OnCBNSelEndOk" ] := "FieldComboBox_OnCBNSelEndOk"
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
            :Enabled              := .F.
            :EventHandler[ "OnCBNSelEndOk" ] := "ConditionComboBox_OnCBNSelEndOk"
            :Create()
         END

         WITH OBJECT ( :oGet1 := EDITBOX( :this ) )
            :Left                 := 320
            :Top                  := 0
            :Width                := 160
            :Height               := 22
            :AutoHScroll          := .T.
            :Create()
         END

         WITH OBJECT ( :oGet2 := EDITBOX( :this ) )
            :Left                 := 402
            :Top                  := 0
            :Width                := 78
            :Height               := 22
            :Visible              := .F.
            :AutoHScroll          := .T.
            :Create()
         END


         WITH OBJECT ( DateTimePicker( :this ) )
            :Left                 := 320
            :Top                  := 0
            :Width                := 160
            :Height               := 22
            :Visible              := .F.
            :Create()
         END

         WITH OBJECT ( DateTimePicker( :this ) )
            :Left                 := 402
            :Top                  := 0
            :Width                := 78
            :Height               := 22
            :Visible              := .F.
            :Create()
         END

         ::AddButtons( :this, .F. )
         
         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
      :PostMessage( WM_VSCROLL, MAKELONG( SB_PAGEDOWN, 0) )
   END
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD AddButtons( oParent, lEnabled ) CLASS FilterUI
   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text         := "Remove condition"
      :Left                 := 485
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "-"
      :EventHandler[ "OnClick" ] := "RemoveConditionButton_OnClick"
      :Create()
   END

   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text         := "Add more condition"
      :Left                 := 510
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "+"
      :Enabled              := lEnabled
      :EventHandler[ "OnClick" ] := "AddConditionButton_OnClick"
      :Create()
   END

   WITH OBJECT ( BUTTON( oParent ) )
      :ToolTip:Text     := "More..."
      :Left             := 535
      :Top              := 0
      :Width            := 20
      :Height           := 22
      :Caption          := "..."
      :Enabled          := lEnabled
      :EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
      :Create()
   END

   IF ! lEnabled
      WITH OBJECT ( CheckBox( oParent ) )
         :ToolTip:Text  := "Ask Me Later"
         :Left          := 560
         :Top           := 4
         :Caption       := ""
         :Width         := 16
         :Enabled       := .T.
         :EventHandler[ "OnClick" ] := "CheckBox_OnClick"
         :Create()
      END
      WITH OBJECT ( BUTTON( oParent ) )
         :Left            := 580
         :Top             := 0
         :Width           := 80
         :Height          := 22
         :Caption         := "Ask Settings"
         :Enabled         := .F.
         :EventHandler[ "OnClick" ] := "AskSettings_OnClick"
         :Cargo           := {=>}
         :Cargo:Title     := ""
         :Cargo:GroupText := ""
         :Cargo:Search    := ""
         :Create()
      END
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD FieldComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
   LOCAL cType := ::oDataTable:EditCtrl:FieldType( Sender:GetCurSel() )
   IF !Sender:Parent:Children[2]:Enabled
      Sender:Parent:Children[2]:Enabled := .T.

      Sender:Parent:Children[-2]:Enabled := .T. // checkbox
      Sender:Parent:Children[-3]:Enabled := .T. // ...
      Sender:Parent:Children[-4]:Enabled := .T. // +
   ENDIF
   Sender:Parent:Children[2]:ResetContent()
   AEVAL( ::oCond:aCond_&cType, {|a| Sender:Parent:Children[2]:AddItem(a[1]) } )
   Sender:Parent:Children[2]:SetCurSel(1)
   ::FilterBrowse:Enabled := .T.
   ::OK:Enabled := .T.

   ::SetDateEdit( Sender, cType )
   Sender:Parent:Children[3]:Enabled := .T.
   Sender:Parent:Children[4]:Enabled := .T.
   Sender:Parent:Children[5]:Enabled := .T.
   Sender:Parent:Children[6]:Enabled := .T.

   Sender:Parent:Children[3]:Caption := ""
   Sender:Parent:Children[4]:Caption := ""
   Sender:Parent:Children[5]:Caption := ""
   Sender:Parent:Children[6]:Caption := ""
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD CheckBox_OnClick( Sender ) CLASS FilterUI
   LOCAL oPanel := Sender:Parent
   //oPanel:Children[-3]:Enabled := .T.
   oPanel:Children[-2]:Enabled := .T.
   oPanel:Children[-1]:Enabled := Sender:checked
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD SetDateEdit( Sender, cType ) CLASS FilterUI
   DEFAULT cType TO ::oDataTable:EditCtrl:FieldType( Sender:Parent:Children[1]:GetCurSel() )
   Sender:Parent:oGet1:Visible := .F.
   Sender:Parent:oGet2:Visible := .F.
   IF cType == "D" .AND. ! ( Sender:Parent:Children[2]:GetSelString() == FC_INTHERANGE )
      Sender:Parent:oGet1 := Sender:Parent:Children[5]
      Sender:Parent:oGet2 := Sender:Parent:Children[6]
    ELSE
      Sender:Parent:oGet1 := Sender:Parent:Children[3]
      Sender:Parent:oGet2 := Sender:Parent:Children[4]
   ENDIF
   Sender:Parent:oGet1:Visible := .T.
   Sender:Parent:oGet2:Visible := Sender:Parent:Children[2]:GetSelString() == FC_BETWEEN
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD ConditionComboBox_OnCBNSelEndOk( Sender, hExp ) CLASS FilterUI
   LOCAL cSel, oDlg, oPanel := Sender:Parent

   cSel := Sender:GetSelString()
   ::SetDateEdit( Sender )

   oPanel:oGet1:Enabled := .T.

   IF cSel == FC_BETWEEN
      oPanel:oGet1:Width := 77
      oPanel:oGet2:Visible := .T.

    ELSEIF cSel IN {FC_PERQUARTER}
      oPanel:oGet1:Enabled := .F.
      ::SetDateEdit( Sender, "C" )
      IF hExp == NIL
         oDlg := FilterPerQuarter( Self, oPanel:oGet1 )
         IF oDlg:Result == IDOK
            oPanel:oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
         ENDIF
       ELSE
         oPanel:oGet1:Caption := hExp:Exp1
      ENDIF
      oPanel:oGet1:Enabled := .F.

    ELSEIF cSel IN {FC_INLAST, FC_NOTINLAST}
      oPanel:oGet1:Enabled := .F.
      ::SetDateEdit( Sender, "C" )
      IF hExp == NIL
         oDlg := IsInTheLast( Self, cSel, {"days", "weeks", "months"}  )
         IF oDlg:Result == IDOK
            oPanel:oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
         ENDIF
       ELSE
         oPanel:oGet1:Caption := hExp:Exp1
      ENDIF
      oPanel:oGet1:Enabled := .F.

    ELSEIF cSel IN {FC_ISEMPTY, FC_NOTEMPTY}
      oPanel:oGet1:Caption := ""
      oPanel:oGet1:Enabled := .F.
    ELSE
      oPanel:oGet1:Width := 160
      oPanel:oGet2:Caption := ""
      oPanel:oGet2:Visible := .F.
   ENDIF

   IF hExp != NIL
      IF hExp:FieldType == "D" .AND. oPanel:oGet1:ClsName == DATETIMEPICK_CLASS
         oPanel:oGet1:Date := STOD( hExp:Exp1 )
       ELSE
         oPanel:oGet1:Caption := hExp:Exp1
      ENDIF

      IF oPanel:oGet2:Visible
         IF hExp:FieldType == "D" .AND. oPanel:oGet1:ClsName == DATETIMEPICK_CLASS
            oPanel:oGet2:Date := STOD( hExp:Exp2 )
          ELSE
            oPanel:oGet2:Caption := hExp:Exp2
         ENDIF
      ENDIF
      IF HGetPos( hExp, "AskMeLater" ) > 0 .AND. hExp:AskMeLater != NIL
         oPanel:Children[-2]:Check()
         oPanel:Children[-1]:Enabled := .T.
         oPanel:Children[-1]:Cargo:Title     := hExp:AskMeLater:Title
         oPanel:Children[-1]:Cargo:GroupText := hExp:AskMeLater:GroupText
         oPanel:Children[-1]:Cargo:Search    := hExp:AskMeLater:Search
      ENDIF
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD RemoveConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL oDock, n, oLastPanel
   IF LEN( ::ConditionPanel:Children ) > 1
      oDock := Sender:Parent:Dock:Top
      n := ASCAN( ::ConditionPanel:Children, {|o| o:hWnd == Sender:Parent:hWnd} )
      IF n > 0 .AND. LEN( ::ConditionPanel:Children ) >= n+1
         ::ConditionPanel:Children[n+1]:Dock:Top := oDock
      ENDIF
      Sender:Parent:Destroy()
      IF LEN( ::ConditionPanel:Children ) > 0
         oLastPanel := ATAIL( ::ConditionPanel:Children )
         oLastPanel:Children[-3]:Enabled := .T.
         oLastPanel:Children[-2]:Enabled := .T.
      ENDIF
    ELSE
      WITH OBJECT ::ConditionPanel:Children[1]
         :Children[1]:SetCurSel(0)
         :Children[2]:SetCurSel(0)
         :Children[3]:Width   := 160
         :Children[3]:Caption := ""
         :Children[3]:Enabled := .F.
         :Children[4]:Caption := ""
         :Children[4]:Visible := .F.
      END
   ENDIF
   ::ConditionPanel:VertScrollSize := (ATAIL( ::ConditionPanel:Children ):Height+4)*LEN( ::ConditionPanel:Children )
   ::FilterBrowse:Enabled := .T.
   ::OK:Enabled := .T.
   ::ConditionPanel:PostMessage( WM_VSCROLL, MAKELONG( SB_PAGEDOWN, 0) )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD MoreConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )
      oLastPanel:Children[-3]:Enabled := .F.
   ENDIF
   ::FilterBrowse:Enabled := .F.
   ::OK:Enabled := .F.
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
            :Left                 := 10
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

         ::AddButtons( :this, .T. )

         :Children[-1]:Enabled := .F.

         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
   END
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD FilterBrowse_OnClick( Sender ) CLASS FilterUI
   LOCAL oDlg
   ::GetFilterExp()

   ::cFilter := BuildFilterExp( ::BuildFilter )
   IF ::cFilter != NIL
      WITH OBJECT oDlg := TestFilter( Self )
         :Caption := "Test DataTable Filter"
         :Width   := 600
         :Height  := 400
         :Center  := .T.
         :Create()
      END
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD OK_OnClick() CLASS FilterUI
   ::GetFilterExp()
   ::Close( IDOK )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD Cancel_OnClick() CLASS FilterUI
   ::Close( IDCANCEL )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------

METHOD GetFilterExp() CLASS FilterUI
   LOCAL cAndOr, nNum, cExpSel, cField, cExp1, cExp2, nSel1, nSel2, oPanel, n, cType, aExp, hExp

   ::BuildFilter := {=>}
   HSetCaseMatch( ::BuildFilter, .F. )

   ::BuildFilter:ANDRadio := IIF( ::ANDRadio:Checked, "1", "2" )
   ::BuildFilter:Expressions := {}
   FOR n := 1 TO LEN( ::ConditionPanel:Children )
       oPanel := ::ConditionPanel:Children[n]
       cField := oPanel:Children[1]:GetSelString()

       IF !EMPTY( cField )
          hExp := {=>}
          HSetCaseMatch( hExp, .F. )
          
          hExp:AndOr  := NIL
          IF LEN( oPanel:Children ) == 4
             hExp:AndOr  := oPanel:Children[1]:GetCurSel()
           ELSE
             hExp:Field      := oPanel:Children[1]:GetSelString()
             hExp:FieldSel   := oPanel:Children[1]:GetCurSel()
             hExp:ExpSel     := oPanel:Children[2]:GetCurSel()
             hExp:FieldType  := ::oDataTable:EditCtrl:FieldType( hExp:FieldSel )
             hExp:Exp1       := oPanel:oGet1:Caption
             hExp:Exp2       := oPanel:oGet2:Caption

             hExp:AskMeLater := NIL

             IF oPanel:Children[-2]:Checked
                hExp:AskMeLater := {=>}
                HSetCaseMatch( hExp:AskMeLater, .F. )

                hExp:AskMeLater:Title     := oPanel:Children[-1]:Cargo:Title
                hExp:AskMeLater:GroupText := oPanel:Children[-1]:Cargo:GroupText
                hExp:AskMeLater:Search    := oPanel:Children[-1]:Cargo:Search
             ENDIF

          ENDIF
          AADD( ::BuildFilter:Expressions, hExp )
       ENDIF
   NEXT
RETURN Self


//------------------------------------------------------------------------------------------------------------------------------------------
METHOD LoadFieldList( oComboBox ) CLASS FilterUI
   LOCAL n, i, aFields
   oComboBox:ResetContent()
   aFields := ::oDataTable:EditCtrl:Struct()
   FOR n := 1 TO LEN( aFields )
       oComboBox:AddItem( aFields[n][1] )
   NEXT
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD AskSettings_OnClick( Sender ) CLASS FilterUI
   WITH OBJECT AskSettings( Self )
      :Owner         := Sender:Cargo
      :ThickFrame    := .F.
      :DlgModalFrame := .T.
      :Caption       := "Ask Me Later Settings"
      :Width         := 390
      :Height        := 295
      :Center        := .T.
      :Create()
   END
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------

CLASS TestFilter INHERIT Dialog
   METHOD OnInitDialog()
ENDCLASS

METHOD OnInitDialog() CLASS TestFilter
   LOCAL cFilter, oGrid, oEdit, oData, oTable := ::Parent:oDataTable
   
   WITH OBJECT oData := DataTable( Self )
      :xFileName := oTable:FileName
      :Driver   := oTable:Driver
      
      IF oTable:Driver != "SQLRDD"
         :Alias := oTable:Alias
         :Create()
         cFilter := ::Parent:cFilter
         IF !EMPTY( cFilter )
            :SetFilter( &("{||"+cFilter+"}") )
         ENDIF
         IF ! EMPTY( oTable:Order )
            :OrdSetFocus( oTable:Order )
         ENDIF
         :GoTop()
      ENDIF
   END

   WITH OBJECT oEdit := Edit( Self )
      :Left         := 0
      :Top          := 0
      :Width        := 300
      :ReadOnly     := .T.
      :Caption      := cFilter
      :Dock:Left    := Self
      :Dock:Bottom  := Self
      :Dock:Right   := Self
      :Create()
   END

   WITH OBJECT oGrid := DataGrid( Self )
      :Left         := 0
      :Top          := 0
      :Width        := 300
      :Height       := 200
      :DataSource   := oData
      :Dock:Left    := Self
      :Dock:Top     := Self
      :Dock:Right   := Self
      :Dock:Bottom  := oEdit
      :Dock:BottomMargin := 2
      :Create()
      :AutoAddColumns()
   END
RETURN Self

CLASS ContCondPanel INHERIT Panel
   DATA oGet1      EXPORTED
   DATA oGet2      EXPORTED
ENDCLASS

//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------------

CLASS AskSettings INHERIT Dialog
   DATA Owner EXPORTED
   METHOD OnInitDialog()
   METHOD OK_OnClick()
   METHOD Test_OnClick()
ENDCLASS

METHOD OK_OnClick() CLASS AskSettings
   ::Owner:Title     := ::Title:Caption
   ::Owner:GroupText := ::GroupText:Caption
   ::Owner:Search    := ::Search:Caption
   ::Close( IDOK )
RETURN Self

METHOD Test_OnClick() CLASS AskSettings
   LOCAL hAsk := {=>}
   hAsk:Exp1 := ""
   hAsk:Exp2 := ""
   hAsk:AskMeLater := {=>}
   hAsk:AskMeLater:Title     := ::Title:Caption
   hAsk:AskMeLater:GroupText := ::GroupText:Caption
   hAsk:AskMeLater:Search    := ::Search:Caption
   AskLater( , , 1, hAsk )
RETURN Self


METHOD OnInitDialog() CLASS AskSettings
   WITH OBJECT ( PictureBox( Self ) )
      :Name             := "PictureBox1"
      WITH OBJECT :Dock
         :Left          := Self
         :Right         := Self
         :Bottom        := Self
         :Margins       := "0,0,0,0"
      END
      :Left             := 0
      :Top              := 125
      :Width            := 418
      :Height           := 50
      :Type             := "JPG"
      :ImageName        := "_BOTTOMRIBBOJPG"
      :Stretch          := .T.
      :Create()

      WITH OBJECT ( Button( :this ) )
         :Dock:Right    := :Parent
         :Dock:Margins  := "0,0,20,0"
         :Left          := 311
         :Top           := 13
         :Width         := 80
         :Height        := 25
         :Caption       := "OK"
         :EventHandler[ "OnClick" ] := "OK_OnClick"
         :Create()
      END

      WITH OBJECT ( Button( :this ) )
         :Dock:Left     := :Parent
         :Dock:Margins  := "20,0,0,0"
         :Left          := 14
         :Top           := 13
         :Width         := 80
         :Height        := 25
         :Caption       := "Help"
         :Create()
      END
   END

   WITH OBJECT Label( Self )
      :Caption    := "Window Title"
      :Width      := 80
      :Left       := 10
      :Top        := 17
      :Create()
   END
   WITH OBJECT EditBox( Self )
      :Name       := "Title"
      :Caption    := ""
      :Left       :=  90
      :Width      := 280
      :Top        :=  15
      :Caption    := ::Owner:Title
      :Create()
   END

   WITH OBJECT Label( Self )
      :Caption    := "Group Text"
      :Width      := 70
      :Left       := 10
      :Top        := 41
      :Create()
   END
   WITH OBJECT EditBox( Self )
      :Name       := "GroupText"
      :Caption    := ""
      :Left       :=  90
      :Width      := 280
      :Top        :=  39
      :Caption    := ::Owner:GroupText
      :Create()
   END
   WITH OBJECT Label( Self )
      :Caption := "Search browse settings:"
      :Width   := 200
      :Left    :=  10
      :Top     :=  80
      :Create()
   END
   WITH OBJECT Button( Self )
      :Caption      := "Test"
      :Width        := 70
      :Left         := 300
      :Top          := 76
      :Height       := 22
      :EventHandler[ "OnClick" ] := "Test_OnClick"
      :Create()
   END
   WITH OBJECT EditBox( Self )
      :Name         := "Search"
      :Left         :=  10
      :Width        := 360
      :Height       := 100
      :Top          := 100
      :MultiLine    := .T.
      :VertScroll   := .T.
      :WantReturn   := .T.
      :Caption      := ::Owner:Search
      :Create()
   END

RETURN Self

