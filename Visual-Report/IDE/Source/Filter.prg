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
   DATA BuildFilter  EXPORTED INIT {}
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
   METHOD BuildFilterExp()
   METHOD OK_OnClick()
   METHOD Cancel_OnClick()
   METHOD SetDateEdit()
   METHOD CheckBox_OnClick()
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
   LOCAL aExps, n, i, cExp, aExp
   ::Left    := 190
   ::Top     := 20
   ::Width   := 634
   ::Height  := 375
   ::Caption := "Create Filter Expression"
   WITH OBJECT ( TabStrip( Self ) )
      :Multiline   := .F.
      :Dock:Margin := 1
      :Dock:Left   := :Parent
      :Dock:Top    := :Parent
      :Dock:Right  := :Parent
      :Dock:Bottom := :Parent
      :Create()
      WITH OBJECT ( Tabpage( :this ) )
         :Caption   := "Conditions"
         :Create()
         WITH OBJECT ( GROUPBOX( :this ) )
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

         WITH OBJECT ( GROUPBOX( :this ) )
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
               :BackColor         := RGB(255,255,255)
               :Create()
               :DockToParent()
            END
         END
         WITH OBJECT ( BUTTON( :this ) )
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

         WITH OBJECT ( BUTTON( :this ) )
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
         WITH OBJECT ( BUTTON( :this ) )
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
      END
      WITH OBJECT ( Tabpage( :this ) )
         :Caption   := "AskMeLater"
         :Create()
         WITH OBJECT ListBox( :this )
            :Left   := 15
            :Top    := 15
            :Width  := 200
            :Height := 200
            :Create()
         END
      END
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
             ::ConditionPanel:Children[i-1]:Children[1]:SetCurSel( VAL( aExp[2] ) )
             ::FieldComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i-1]:Children[1] )
             
             ::ConditionPanel:Children[i-1]:Children[2]:SetCurSel( VAL( aExp[3] ) )
             IF LEN( aExp ) > 3
                ::ConditionComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i-1]:Children[2], aExp[4], aExp[5], IIF(LEN(aExp)>=6,aExp[6],), aExp[1] )
              ELSE
                ::ConditionComboBox_OnCBNSelEndOk( ::ConditionPanel:Children[i-1]:Children[2], aExp[4] )
             ENDIF
          ENDIF
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

      oLastPanel:Children[ LEN(oLastPanel:Children)-2 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
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
         :BackColor      := RGB(255,255,255)
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

   IF ! lEnabled
      WITH OBJECT ( CheckBox( oParent ) )
         :ToolTip:Text         := "Ask Later"
         :Left                 := 560
         :Top                  := 5
         :EventHandler[ "OnClick" ] := "CheckBox_OnClick"
         :Create()
      END
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD FieldComboBox_OnCBNSelEndOk( Sender ) CLASS FilterUI
   LOCAL cType := ::oDataTable:EditCtrl:FieldType( Sender:GetCurSel() )
   IF !Sender:Parent:Children[2]:Enabled
      Sender:Parent:Children[2]:Enabled := .T.
      Sender:Parent:Children[ LEN(Sender:Parent:Children)-2 ]:Enabled := .T.
      Sender:Parent:Children[ LEN(Sender:Parent:Children)-1 ]:Enabled := .T.
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
   oPanel:Children[ LEN(oPanel:Children)-2 ]:Enabled := .T.
   oPanel:Children[ LEN(oPanel:Children)-1 ]:Enabled := .T.
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
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
   Sender:Parent:oGet2:Visible := Sender:Parent:Children[2]:GetSelString() == FC_BETWEEN
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD ConditionComboBox_OnCBNSelEndOk( Sender, cType, cValue, cValue2, cAskLater ) CLASS FilterUI
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
      IF cValue == NIL
         oDlg := FilterPerQuarter( Self, oPanel:oGet1 )
         IF oDlg:Result == IDOK
            oPanel:oGet1:Caption := oDlg:nNum + " " + oDlg:cSel
         ENDIF
       ELSE
         oPanel:oGet1:Caption := cValue
      ENDIF
      oPanel:oGet1:Enabled := .F.

    ELSEIF cSel IN {FC_INLAST, FC_NOTINLAST}
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

    ELSEIF cSel IN {FC_ISEMPTY, FC_NOTEMPTY}
      oPanel:oGet1:Caption := ""
      oPanel:oGet1:Enabled := .F.
    ELSE
      oPanel:oGet1:Width := 160
      oPanel:oGet2:Caption := ""
      oPanel:oGet2:Visible := .F.
   ENDIF

   IF cValue != NIL
      IF cType == "D" .AND. oPanel:oGet1:ClsName == DATETIMEPICK_CLASS
         oPanel:oGet1:Date := STOD( cValue )
       ELSE
         oPanel:oGet1:Caption := cValue
      ENDIF
   ENDIF
   IF cValue2 != NIL .AND. oPanel:oGet2:Visible
      IF cType == "D" .AND. oPanel:oGet1:ClsName == DATETIMEPICK_CLASS
         oPanel:oGet2:Date := STOD( cValue2 )
       ELSE
         oPanel:oGet2:Caption := cValue2
      ENDIF
   ENDIF
   IF cAskLater != NIL .AND. cAskLater == "1"
      ATAIL(oPanel:Children):Check()
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
         oLastPanel:Children[ LEN(oLastPanel:Children)-2 ]:Enabled := .T.
         oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .T.
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
      oLastPanel:Children[ LEN(oLastPanel:Children)-2 ]:Enabled := .F.
      oLastPanel:Children[ LEN(oLastPanel:Children)-1 ]:Enabled := .F.
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

//------------------------------------------------------------------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD OK_OnClick() CLASS FilterUI
   ::BuildFilterExp()
   ::Close( IDOK )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD Cancel_OnClick() CLASS FilterUI
   ::Close( IDCANCEL )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------------
METHOD BuildFilterExp() CLASS FilterUI
   LOCAL cAndOr, nNum, cFldSel, nDays, cExpSel, cField, cExp, cExp2, nSel1, nSel2, oPanel, n, cType,  bExp, aExp, xAnd, cAsk
   ::cFilter := ""
   cAndOr := IIF( ::ANDRadio:Checked, " .AND. ", " .OR. " )
   ::BuildFilter := IIF( ::ANDRadio:Checked, "1", "2" )
   FOR n := 1 TO LEN( ::ConditionPanel:Children )
       oPanel := ::ConditionPanel:Children[n]
       cFldSel := oPanel:Children[1]:GetSelString()
       nSel1   := oPanel:Children[1]:GetCurSel()
       IF !EMPTY( cFldSel )
          IF LEN( oPanel:Children ) == 4
             cAndOr := IIF( nSel1 == 1, " .AND. ", " .OR. " )
             ::BuildFilter += "~"+xStr( nSel1 )
           ELSE
             cExpSel := oPanel:Children[2]:GetSelString()
             nSel2   := oPanel:Children[2]:GetCurSel()
             cType   := ::oDataTable:EditCtrl:FieldType( nSel1 )
             cExp    := oPanel:oGet1:Caption
             cExp2   := oPanel:oGet2:Caption

             cField := ::oDataTable:Alias + "->" + cFldSel

             IF ATAIL( oPanel:Children ):Checked
                cAsk := "1"
              ELSE
                cAsk := "0"
             ENDIF

             ::BuildFilter += "~"+cAsk+"|"+xStr(nSel1)+"|"+xStr(nSel2)+"|"+cType

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
                IF cExpSel IN {FC_INLAST, FC_NOTINLAST}

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
                 ELSEIF cExpSel IN {FC_PERQUARTER}
                   ::BuildFilter += "|"+cExp
                   aExp  := hb_aTokens( oPanel:oGet1:Caption )
                   cExp := 'MONTH('+cField+')>='+aExp[2]+'.AND.MONTH('+cField+')<='+aExp[4]
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

             IF ATAIL( oPanel:Children ):Checked
                ::cFilter += '~AskLater( "'+cFldSel+'","'+cType+'",'+xStr(nSel2)+')~'
              ELSE
                bExp := ::oCond:aCond_&cType[nSel2][2]
                ::cFilter += EVAL( bExp, cField, cExp, cExp2 )
             ENDIF
          ENDIF
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
         cFilter := CleanFilter( ::Parent:cFilter )
         :SetFilter( &("{||"+cFilter+"}") )
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

