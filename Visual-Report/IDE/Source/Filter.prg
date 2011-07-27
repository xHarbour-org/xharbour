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
   DATA cFilter      EXPORTED INIT ""
   DATA BuildFilter  EXPORTED INIT {}
   DATA oDataTable   EXPORTED
   DATA aCond_C      EXPORTED INIT {}
   DATA aCond_N      EXPORTED INIT {}
   DATA aCond_D      EXPORTED INIT {}
   DATA aCond_L      EXPORTED INIT {}
   DATA aCond_M      EXPORTED INIT {}
   DATA aCondVal     EXPORTED INIT {}

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
   METHOD BuildFilterExp()
   METHOD OK_OnClick()
   METHOD Cancel_OnClick()
   METHOD SetDateEdit()
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


   ::aCond_N := {  { "Equals to",             {|cField,cExp,cExp2| cField + "==" + cExp} },;
                   { "Is not equal to",       {|cField,cExp,cExp2| "!(" + cField + "==" + cExp + ")" } },;
                   { "Greater than or equal", {|cField,cExp,cExp2| cField + ">=" + cExp} },;
                   { "Less than or equal",    {|cField,cExp,cExp2| cField + "<=" + cExp} },;
                   { "Between",               {|cField,cExp,cExp2| "(" + cField + ">= " + cExp + ".AND." + cField +"<=" + cExp2 + ")"} },;
                   { "Is in the range",       {|cField,cExp,cExp2| cField } } }

   ::aCond_C := {  { "Contains",              {|cField,cExp,cExp2| cExp + " $ " + cField} },;
                   { "Does not contain",      {|cField,cExp,cExp2| "!(" + cExp + " $ " + cField + ")"} },;
                   { "Begins with",           {|cField,cExp,cExp2| cField + "=" + cExp} },;
                   { "Does not begin with",   {|cField,cExp,cExp2| cField + "!=" + cExp} },;
                   { "Is empty",              {|cField,cExp,cExp2| "EMPTY(" + cField + ")"} },;
                   { "Is not empty",          {|cField,cExp,cExp2| "! EMPTY(" + cField + ")"} },;
                   { "Is in the range",       {|cField,cExp,cExp2| cField} } }

   ::aCond_D := {  { "Equals",                    {|cField,cExp,cExp2| cField + "==" + cExp} },;
                   { "Is not equal",              {|cField,cExp,cExp2| cField + "<>" + cExp} },;
                   { "Is greater or the same as", {|cField,cExp,cExp2| cField + ">=" + cExp} },;
                   { "Is less or the same as",    {|cField,cExp,cExp2| cField + "<=" + cExp} },;
                   { "Between",                   {|cField,cExp,cExp2| "(" + cField + ">= " + cExp + ".AND." + cField +"<=" + cExp2 + ")"} },;
                   { "Per quarter",               {|cField,cExp,cExp2| cField} },;
                   { "Is in the last",            {|cField,cExp,cExp2| cField + ">=" + cExp } },;
                   { "Is not in the last",        {|cField,cExp,cExp2| cField} },;
                   { "Is in the range",           {|cField,cExp,cExp2| cField} } }

   ::aCond_L := {  { "True",  {|cField,cExp,cExp2| cField} },;
                   { "False", {|cField,cExp,cExp2| "!"+cField} } }

   ::aCond_M := ACLONE( ::aCond_C )
   
   ::Modal      := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS FilterUI
   LOCAL aExps, n, i, cExp, aExp
   ::Left    := 190
   ::Top     := 20
   ::Width   := 634
   ::Height  := 375
   ::Caption := "Create Filter Expression"
   WITH OBJECT ( GROUPBOX( Self ) )
      WITH OBJECT :Dock
         :Left              := Self
         :Top               := Self
         :Right             := Self
         :Margins           := "20,15,20,0"
      END
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
      END //RADIOBUTTON

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
         :Create()
         :DockToParent()
      END
   END
   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "FilterBrowse"
      WITH OBJECT :Dock
         :Left              := Self
         :Bottom            := Self
         :Margins           := "20,0,20,10"
      END
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
      WITH OBJECT :Dock
         :Right                := Self
         :Bottom               := Self
         :Margins              := "0,0,20,10"
      END
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
      WITH OBJECT :Dock
         :Right                := "Cancel"
         :Bottom               := Self
         :Margins              := "0,0,5,10"
      END
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
   
   aExps := hb_aTokens( ::oDataTable:BuildFilter, "~" )
   IF !EMPTY( aExps )
      IF aExps[1] == "1"
         ::ANDRadio:SetState( BST_CHECKED )
         ::ORRadio:SetState( BST_UNCHECKED )
       ELSE
         ::ANDRadio:SetState( BST_UNCHECKED )
         ::ORRadio:SetState( BST_CHECKED )
      ENDIF
      FOR i := 2 TO LEN( aExps )
          aExp := hb_aTokens( aExps[i], "|" )
          IF LEN( aExp ) > 1
             ::ConditionPanel:Children[i-1]:Children[1]:Enabled := .T.
             ::ConditionPanel:Children[i-1]:Children[1]:SetCurSel( VAL( aExp[1] ) )
             ::FieldComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i-1]:Children[1] )
             
             ::ConditionPanel:Children[i-1]:Children[2]:SetCurSel( VAL( aExp[2] ) )
             ::ConditionComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i-1]:Children[2], aExp[3], IIF( LEN( aExp ) >= 4, aExp[4],), IIF( LEN( aExp ) >= 5, aExp[5],) )
             
          ENDIF
          IF i < LEN( aExps )
             ::AddConditionButton_OnClick()
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------//
METHOD AddConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )

      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .F.
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
         END //EDITBOX


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
         END //EDITBOX

         ::AddButtons( :this, .F. )
         
         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
      :PostMessage( WM_VSCROLL, MAKELONG( SB_PAGEDOWN, 0) )
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
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
      :ToolTip:Text         := "More..."
      :Left                 := 535
      :Top                  := 0
      :Width                := 20
      :Height               := 22
      :Caption              := "..."
      :Enabled              := lEnabled
      :EventHandler[ "OnClick" ] := "MoreConditionButton_OnClick"
      :Create()
   END

RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD FieldComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
   LOCAL cType := ::oDataTable:EditCtrl:FieldType( Sender:GetCurSel() )
   IF !Sender:Parent:Children[2]:Enabled
      Sender:Parent:Children[2]:Enabled := .T.
      Sender:Parent:Children[ LEN(Sender:Parent:Children)-1 ]:Enabled := .T.
      Sender:Parent:Children[ LEN(Sender:Parent:Children)-0 ]:Enabled := .T.
   ENDIF
   Sender:Parent:Children[2]:ResetContent()
   AEVAL( ::aCond_&cType, {|a| Sender:Parent:Children[2]:AddItem(a[1]) } )
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


//----------------------------------------------------------------------------------------------------//
METHOD SetDateEdit( Sender, cType ) CLASS FilterUI
   DEFAULT cType TO ::oDataTable:EditCtrl:FieldType( Sender:Parent:Children[1]:GetCurSel() )
   Sender:Parent:oGet1:Visible := .F.
   Sender:Parent:oGet2:Visible := .F.
   IF cType == "D"
      Sender:Parent:oGet1 := Sender:Parent:Children[5]
      Sender:Parent:oGet2 := Sender:Parent:Children[6]
    ELSE
      Sender:Parent:oGet1 := Sender:Parent:Children[3]
      Sender:Parent:oGet2 := Sender:Parent:Children[4]
   ENDIF
   Sender:Parent:oGet1:Visible := .T.
   Sender:Parent:oGet2:Visible := Sender:Parent:Children[2]:GetSelString() == "Between"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD ConditionComboBox_OnCBNSelEndOk( Sender, cType, cValue, cValue2 ) CLASS FilterUI
   LOCAL cSel, oDlg, oPanel := Sender:Parent

   cSel := Sender:GetSelString()
   ::SetDateEdit( Sender )

   oPanel:oGet1:Enabled := .T.

   IF cSel == "Between"
      oPanel:oGet1:Width := 77
      oPanel:oGet2:Visible := .T.

    ELSEIF cSel IN {"Per quarter"}
      oPanel:oGet1:Enabled := .F.
      ::SetDateEdit( Sender, "C" )
      IF cValue == NIL
         oDlg := FilterPerQuarter( Self, {"days", "weeks", "months"}  )
         IF oDlg:Result == IDOK
            oPanel:oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
         ENDIF
       ELSE
         oPanel:oGet1:Caption := cValue
      ENDIF
      oPanel:oGet1:Enabled := .F.
      
    ELSEIF cSel IN {"Is in the last", "Is not in the last"}
      oPanel:oGet1:Enabled := .F.
      ::SetDateEdit( Sender, "C" )
      IF cValue == NIL
         oDlg := IsInTheLast( Self, cSel, {"days", "weeks", "months"}  )
         IF oDlg:Result == IDOK
            oPanel:oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
         ENDIF
       ELSE
         oPanel:oGet1:Caption := cValue
      ENDIF
      oPanel:oGet1:Enabled := .F.
    ELSE
      oPanel:oGet1:Width := 160
      oPanel:oGet2:Caption := ""
      oPanel:oGet2:Visible := .F.
   ENDIF
   IF cValue != NIL
      IF cType == "D"
         oPanel:oGet1:Date := STOD( cValue )
       ELSE
         oPanel:oGet1:Caption := cValue
      ENDIF
   ENDIF
   IF cValue2 != NIL .AND. oPanel:oGet2:Visible
      IF cType == "D"
         oPanel:oGet2:Date := STOD( cValue2 )
       ELSE
         oPanel:oGet2:Caption := cValue2
      ENDIF
   ENDIF
   IF cSel IN {"Is empty", "Is not empty"}
      oPanel:oGet1:Caption := ""
      oPanel:oGet1:Enabled := .F.
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
      ENDIF
      Sender:Parent:Destroy()
      IF LEN( ::ConditionPanel:Children ) > 0
         oLastPanel := ATAIL( ::ConditionPanel:Children )
         oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .T.
         oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .T.
      ENDIF
   ENDIF
   ::ConditionPanel:VertScrollSize := (ATAIL( ::ConditionPanel:Children ):Height+4)*LEN( ::ConditionPanel:Children )
   ::FilterBrowse:Enabled := .T.
   ::OK:Enabled := .T.
   ::ConditionPanel:PostMessage( WM_VSCROLL, MAKELONG( SB_PAGEDOWN, 0) )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD MoreConditionButton_OnClick( Sender ) CLASS FilterUI
   LOCAL cName, n, oLastPanel
   
   IF LEN( ::ConditionPanel:Children ) > 0
      oLastPanel := ATAIL( ::ConditionPanel:Children )
      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-0 ]:Enabled := .F.
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

         :SetRedraw( .T. )
         :RedrawWindow( , , RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT | RDW_ALLCHILDREN )
         :UpdateWindow()
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD FilterBrowse_OnClick( Sender ) CLASS FilterUI
   LOCAL oDlg
   ::BuildFilterExp()
   WITH OBJECT oDlg := TestFilter( Self )
      :Caption := "Test DataTable Filter"
      :Width   := 600
      :Height  := 400
      :Center  := .T.
      :Create()
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD OK_OnClick() CLASS FilterUI
   ::BuildFilterExp()
   ::Close( IDOK )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Cancel_OnClick() CLASS FilterUI
   ::Close( IDCANCEL )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD BuildFilterExp() CLASS FilterUI
   LOCAL cAndOr, nNum, cFldSel, nDays, cExpSel, cField, cExp, cExp2, nSel1, nSel2, oPanel, n, cType,  bExp, aExp, xAnd
   ::cFilter := ""
   cAndOr := IIF( ::ANDRadio:Checked, " .AND. ", " .OR. " )
   ::BuildFilter := IIF( ::ANDRadio:Checked, "1", "2" )
   FOR n := 1 TO LEN( ::ConditionPanel:Children )
       oPanel := ::ConditionPanel:Children[n]
       IF LEN( oPanel:Children ) == 4
          cAndOr := IIF( oPanel:Children[1]:GetCurSel() == 1, " .AND. ", " .OR. " )
          ::BuildFilter += "~"+xStr( oPanel:Children[1]:GetCurSel() )
        ELSE
          cFldSel := oPanel:Children[1]:GetSelString()
          cExpSel := oPanel:Children[2]:GetSelString()

          nSel1   := oPanel:Children[1]:GetCurSel()
          nSel2   := oPanel:Children[2]:GetCurSel()
          cType   := ::oDataTable:EditCtrl:FieldType( nSel1 )
          cExp    := oPanel:oGet1:Caption //oPanel:Children[3]:Caption
          cExp2   := oPanel:oGet2:Caption //oPanel:Children[4]:Caption

          cField := ::oDataTable:Alias + "->" + cFldSel

          ::BuildFilter += "~"+xStr(nSel1)+"|"+xStr(nSel2)+"|"+cType

          IF cType $ "CM"
             
             ::BuildFilter += "|"+cExp+"|"+cExp2
             
             cField := "TRIM("+cField+")"
             cExp  := ValToPrg( cExp  )
             cExp2 := ValToPrg( cExp2 )
           ELSEIF cType == "N"
             ::BuildFilter += "|"+cExp+"|"+cExp2

             cExp   := ValToPrg( VAL( cExp ) )
             cExp2  := ValToPrg( VAL( cExp2 ) )
           ELSEIF cType == "D"
             IF cExpSel IN {"Is in the last", "Is not in the last"}

                ::BuildFilter += "|"+cExp

                aExp  := hb_aTokens( oPanel:oGet1:Caption )
                cExp  := "@TODAY-"
                nNum  := VAL( aExp[1] )
                IF aExp[2] == "days"
                   cExp += aExp[1]
                 ELSEIF aExp[2] == "weeks"
                   cExp += AllTrim( Str( nNum*7 ) )
                 ELSEIF aExp[2] == "months"
                   cExp += AllTrim( Str( nNum*30 ) )
                ENDIF
              ELSE
                ::BuildFilter += "|"+DTOS(oPanel:oGet1:Date)
                IF oPanel:oGet2:Visible
                   ::BuildFilter += "|"+DTOS(oPanel:oGet2:Date)
                ENDIF
                cExp  := 'STOD( "' + DTOS(oPanel:oGet1:Date) + '" )'
                cExp2 := 'STOD( "' + DTOS(oPanel:oGet2:Date) + '" )'
             ENDIF
          ENDIF

          IF n > 1
             ::cFilter += cAndOr
          ENDIF
          
          bExp := ::aCond_&cType[nSel2][2]

          ::cFilter += EVAL( bExp, cField, cExp, cExp2 )

       ENDIF
   NEXT
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD LoadFieldList( oComboBox ) CLASS FilterUI
   LOCAL n, i, aFields
   oComboBox:ResetContent()
   aFields := ::oDataTable:EditCtrl:Struct()
   FOR n := 1 TO LEN( aFields )
       oComboBox:AddItem( aFields[n][1] )
   NEXT
RETURN NIL

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
         cFilter := STRTRAN( cFilter, "@TODAY", 'CTOD("'+DTOC(DATE())+'")' )
         TRY
            :SetFilter( &("{||"+cFilter+"}") )
         CATCH
            ::MessageBox( "The selected filter has caused an error. Please rebuild the filter expression and try again", "VR" )
         END
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

//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------

CLASS IsInTheLast INHERIT Dialog
   DATA Text     EXPORTED
   DATA aOptions EXPORTED
   DATA nNum     EXPORTED
   DATA cSel     EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OK_OnClick()
   METHOD Help_OnClick()
ENDCLASS

METHOD Init( oParent, cText, aOptions ) CLASS IsInTheLast
   Super:Init( oParent )

   ::Width      := 300
   ::Height     := 200
   ::Caption    := "VR Filter"
   ::Modal      := .T.
   ::Center     := .T.
// ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU
// ::Resizable  := .F.
   ::AutoClose  := .T.
   ::Text       := cText
   ::TopMost    := .T.
   ::aOptions   := aOptions
   ::Icon       := "AVR"
   ::MaximizeBox:= .F.
   ::MinimizeBox:= .F.
   ::Create()

RETURN Self

METHOD OnInitDialog() CLASS IsInTheLast
   WITH OBJECT ( PictureBox( Self ) )
      :Name      := "BottomRibbon"
      WITH OBJECT :Dock
         :Left   := Self
         :Right  := Self
         :Bottom := Self
      END
      :Left      := 0
      :Top       := 415
      :Width     := 844
      :Height    := 50
      :Type      := "JPG"
      :ImageName := "BTRIBBON"
      :Stretch   := .T.
      :Create()
      WITH OBJECT ( Button( :this ) )
         :Caption   := "Help"
         :ID        := IDOK
         :Left      := 10
         :Top       := 12
         :Width     := 80
         :Height    := 25
         :Action    := {||::Help_OnClick()}
         :Create()
      END
      WITH OBJECT ( Button( :this ) )
         :Caption   := "OK"
         :Left      := :Parent:Width - 85
         :Top       := 12
         :Width     := 80
         :Height    := 25
         :DefaultButton := .T.
         :Action    := {||::OK_OnClick()}
         :Create()
      END
   END
   
   WITH OBJECT ( GroupBox( Self ) )
      :Caption   := ::Text    
      :Left      := 15
      :Top       := 15
      :Width     := ::ClientWidth-30
      :Height    := 90
      :Create()
      WITH OBJECT ( EditBox( :this ) )
         :Caption   := "1"
         :Number    := .T.
         :Left      := 15
         :Top       := 40
         :Width     := 100
         :Alignment :=  3
         :Create()
      END
      WITH OBJECT ( ComboBox( :this ) )
         :Left      := 120
         :Top       := 40
         :Width     := 130
         :Create()
         AEVAL( ::aOptions, {|c| :AddItem(c) } )
         :SetCurSel(1)
      END
   END
   ::EditBox1:SetFocus()
RETURN 0

METHOD OK_OnClick() CLASS IsInTheLast
   ::nNum := ::EditBox1:Caption
   ::cSel := ::ComboBox1:GetSelString()
   ::Close( IDOK )
RETURN NIL

METHOD Help_OnClick() CLASS IsInTheLast
RETURN NIL

//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------------

CLASS FilterPerQuarter INHERIT Dialog
   VAR nCondNum, oMainWin, lReturn,lNum
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()

   // Event declaration
   METHOD FilterPerQuarter_OnClose()
   METHOD FilterPerQuarter_OnLoad()
   METHOD RadioButton1_OnClick()
   METHOD RadioButton2_OnClick()
   METHOD RadioButton3_OnClick()
   METHOD RadioButton4_OnClick()
   METHOD RadioButton5_OnClick()
   METHOD Button1_OnClick()

ENDCLASS

METHOD Init( oParent, aParameters ) CLASS FilterPerQuarter
   ::Super:Init( oParent, aParameters )

   ::EventHandler[ "OnClose" ] := "FilterPerQuarter_OnClose"
   ::EventHandler[ "OnLoad" ]  := "FilterPerQuarter_OnLoad"

   ::Name                 := "FilterPerQuarter"
   ::VertScrollSize       := 262
   ::HorzScrollSize       := 284
   ::Modal                := .T.
   ::Left                 := 10
   ::Top                  := 10
   ::Width                := 454
   ::Height               := 253
   ::Center               := .T.
   ::Caption              := "WinFakt! Per quarter"
   ::TopMost              := .T.
   ::MaximizeBox          := .F.
   ::MinimizeBox          := .F.
   ::Create()

   // Populate Children
RETURN Self

METHOD OnInitDialog() CLASS FilterPerQuarter
   // Properties declaration

   // Populate Children
   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "GroupBox1"
      WITH OBJECT :Dock
         :Left                 := "FilterPerQuarter"
         :Top                  := "FilterPerQuarter"
         :Bottom               := "FilterPerQuarter"
         :Margins              := "20,15,0,70"
      END

      :Left                 := 20
      :Top                  := 15
      :Width                := 156
      :Height               := 129
      :Caption              := "Month"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( LABEL( :this ) )
         :Name                 := "Label1"
         :Left                 := 23
         :Top                  := 26
         :Width                := 50
         :Height               := 16
         :Caption              := "From :"
         :Rightalign           := .T.
         :Create()
      END //LABEL

      WITH OBJECT ( EDITBOX( :this ) )
         :Name                 := "EditBox1"
         :Left                 := 76
         :Top                  := 22
         :Width                := 50
         :Height               := 22
         :Alignment            := 3
         :AutoHScroll          := .T.
         :Number               := .T.
         :Create()
      END //EDITBOX

      WITH OBJECT ( LABEL( :this ) )
         :Name                 := "Label2"
         :Left                 := 23
         :Top                  := 61
         :Width                := 50
         :Height               := 16
         :Caption              := "To :"
         :Rightalign           := .T.
         :Create()
      END //LABEL

      WITH OBJECT ( EDITBOX( :this ) )
         :Name                 := "EditBox2"
         :Left                 := 76
         :Top                  := 57
         :Width                := 50
         :Height               := 22
         :Alignment            := 3
         :AutoHScroll          := .T.
         :Number               := .T.
         :Create()
      END //EDITBOX

   END //GROUPBOX

   WITH OBJECT ( GROUPBOX( Self ) )
      :Name                 := "GroupBox2"
      WITH OBJECT :Dock
         :Top                  := "FilterPerQuarter"
         :Right                := "FilterPerQuarter"
         :Bottom               := "FilterPerQuarter"
         :Margins              := "0,15,20,70"
      END

      :Left                 := 193
      :Top                  := 15
      :Width                := 223
      :Height               := 129
      :Caption              := "Quarter"
      :ForeColor            := 0
      :Create()
      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton1"
         :Left                 := 26
         :Top                  := 25
         :Width                := 80
         :Height               := 15
         :Caption              := "I. First"
         :EventHandler[ "OnClick" ] := "RadioButton1_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton2"
         :Left                 := 26
         :Top                  := 60
         :Width                := 80
         :Height               := 15
         :Caption              := "II. Second"
         :EventHandler[ "OnClick" ] := "RadioButton2_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton3"
         :Left                 := 124
         :Top                  := 25
         :Width                := 80
         :Height               := 15
         :Caption              := "III. Third"
         :EventHandler[ "OnClick" ] := "RadioButton3_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton4"
         :Left                 := 124
         :Top                  := 60
         :Width                := 80
         :Height               := 15
         :Caption              := "IV. Fourth"
         :EventHandler[ "OnClick" ] := "RadioButton4_OnClick"
         :Create()
      END //RADIOBUTTON

      WITH OBJECT ( RADIOBUTTON( :this ) )
         :Name                 := "RadioButton5"
         :Left                 := 26
         :Top                  := 93
         :Width                := 80
         :Height               := 15
         :Caption              := "All quarters"
         :EventHandler[ "OnClick" ] := "RadioButton5_OnClick"
         :Create()
      END //RADIOBUTTON

   END //GROUPBOX

   WITH OBJECT ( PICTUREBOX( Self ) )
      WITH OBJECT :Dock
         :Left                 := Self
         :Right                := Self
         :Bottom               := Self
         :Margins              := "0,0,0,0"
      END
      :Left                 := 0
      :Top                  := 165
      :Width                := 437
      :Height               := 50
      :Type                 := "JPG"
      :ImageName            := "BTRIBBON"
      :Stretch              := .T.
      :Create()
      WITH OBJECT ( BUTTON( :this ) )
         :Name                 := "Button1"
         WITH OBJECT :Dock
            :Right                := "PictureBox1"
            :Margins              := "0,0,12,0"
         END

         :Left                 := 340
         :Top                  := 12
         :Width                := 80
         :Height               := 25
         :Caption              := "Save"
         :EventHandler[ "OnClick" ] := "Button1_OnClick"
         :Create()
      END //BUTTON

      WITH OBJECT ( BUTTON( :this ) )
         :Name                 := "Button2"
         :Left                 := 12
         :Top                  := 12
         :Width                := 80
         :Height               := 25
         :Caption              := "Help"
         :Create()
      END //BUTTON

   END //PICTUREBOX

RETURN Self


METHOD FilterPerQuarter_OnLoad( Sender ) CLASS FilterPerQuarter
/*
   LOCAL i, nMonFrom := 0, nMonTo := 0, nQtr, aTemp := {}
   
   ::nCondNum := ::Params[1]
   ::oMainWin := ::Params[2]
   ::lNum     := ::Params[3]
   
   ::lReturn := .F.
   
   IF ::lNum
      aTemp := HB_ATOKENS(::oMainWin:&("ConditionValueEditBox"+V(::nCondNum)):Caption, ";")
      IF LEN(aTemp) > 0
         nMonFrom := VAL(aTemp[1])
         nMonTo   := VAL(aTemp[LEN(aTemp)])
      ENDIF
   ELSE
      nMonFrom := MONTH(CTOD(::oMainWin:&("ConditionValueEditBox"+V(::nCondNum)):Caption))
      nMonTo   := MONTH(CTOD(::oMainWin:&("ConditionValueEditBoxSec"+V(::nCondNum)):Caption))
   ENDIF
   
   IF nMonFrom = 0 .AND. nMonTo = 0
      nMonFrom := 1
      nMonTo   := 12
   ENDIF
   
   nMonFrom := IF(nMonFrom < 1, 1, IF(nMonFrom > 12, 12, nMonFrom))
   nMonTo   := IF(nMonTo < 1, 1, IF(nMonTo > 12, 12, nMonTo))
   
   ::EditBox1:Caption := V(nMonFrom)
   ::EditBox2:Caption := V(nMonTo)
   
   DO CASE
      CASE nMonFrom = 1 .AND. nMonTo = 3
         ::RadioButton1:SetState(1)
      CASE nMonFrom = 4 .AND. nMonTo = 6
         ::RadioButton2:SetState(1)
      CASE nMonFrom = 7 .AND. nMonTo = 9
         ::RadioButton3:SetState(1)
      CASE nMonFrom = 10 .AND. nMonTo = 12
         ::RadioButton4:SetState(1)
      CASE nMonFrom = 1 .AND. nMonTo = 12
         ::RadioButton5:SetState(1)
   ENDCASE
   
   ::EditBox1:SetFocus()
*/
RETURN Self



//----------------------------------------------------------------------------------------------------//
METHOD RadioButton1_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "1"
   ::EditBox2:Caption := "3"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RadioButton2_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "4"
   ::EditBox2:Caption := "6"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RadioButton3_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "7"
   ::EditBox2:Caption := "9"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RadioButton4_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "10"
   ::EditBox2:Caption := "12"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD RadioButton5_OnClick( Sender ) CLASS FilterPerQuarter
   ::EditBox1:Caption := "1"
   ::EditBox2:Caption := "12"
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS FilterPerQuarter
   LOCAL nMonFrom, nMonTo, mFromDt, mToDt, nYear, i, cTemp
   
   nMonFrom := VAL(::EditBox1:Caption)
   nMonTo   := VAL(::EditBox2:Caption)
   
   IF nMonFrom < 1 .OR. nMonFrom > 12 .OR. nMonTo > 12
      ::MessageBox( "Please enter valid month.", "Filter" )
      RETURN Self
   ENDIF
   
   IF nMonFrom > nMonTo
      ::MessageBox( "'From' month cannot be greater than 'To' month.", "Filter" )
      RETURN Self
   ENDIF
   
   IF ::lNum
      cTemp := ""
      FOR i := nMonFrom TO nMonTo
         cTemp += IF(EMPTY(cTemp), "", ";")+V(i)
      NEXT
      ::oMainWin:&("ConditionValueEditBox"+V(::nCondNum)):Caption := cTemp
   ELSE
      nYear := IF(::oMainWin:cboFiscalYear:GetCurSel() > 0, VAL(::oMainWin:cboFiscalYear:GetString()), YEAR(DATE()))
      
      mFromDt := STOD(V(nYear)+PADL(V(nMonFrom), 2, "0")+"01")
      mToDt   := STOD(V(nYear)+PADL(V(nMonTo), 2, "0")+"01")
      
      mFromDt := BOM(mFromDt)
      mToDt   := EOM(mToDt)
      
      ::oMainWin:&("ConditionValueEditBox"+V(::nCondNum)):Caption    := DTOC(mFromDt)
      ::oMainWin:&("ConditionValueEditBoxSec"+V(::nCondNum)):Caption := DTOC(mToDt)

   ENDIF
   
   ::lReturn := .T.
   
   ::Close()
   
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD FilterPerQuarter_OnClose( Sender ) CLASS FilterPerQuarter
   LOCAL oWin := ::oMainWin, cTemp := V(::nCondNum)
   IF !::lNum
      oWin:&("ConditionValueLabel"+cTemp):Caption := "From "+V(MONTH(CTOD(oWin:&("ConditionValueEditBox"+cTemp):Caption)))+ " to " +;
         V(MONTH(CTOD(oWin:&("ConditionValueEditBoxSec"+cTemp):Caption)))
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------//
