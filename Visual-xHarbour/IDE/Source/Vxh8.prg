/*
 * $Id$
 */
static s_oSave

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

#define DG_ADDCONTROL      1
#define DG_DELCONTROL      2

EXIT PROCEDURE __CleanVxh8()
   s_oSave := NIL
RETURN


//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ColumnManager INHERIT Dialog
   DATA Grid             EXPORTED
   DATA ItemManager      EXPORTED
   DATA ItemEventManager EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD TabSelection()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oGrid ) CLASS ColumnManager
   LOCAL lProp
   ::Grid  := oGrid
   DEFAULT ::__xCtrlName  TO "ColumnManager"

   ::Super:Init( ::Application:MainForm )

   ::Modal      := .T.
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::DlgModalFrame := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS ColumnManager
   LOCAL oItem, oSub
   ::Caption    := "DataGrid Column Manager"
   
   WITH OBJECT Image( Self )
      :Height  := 77
      :BackColor     := C_WHITE
      :ImageName     := "Banner"
      :Dock:Margin   := 0
      :Dock:Left     := :Parent
      :Dock:Top      := :Parent
      :Dock:Right    := :Parent
      :Create()
   END
   
   WITH OBJECT ToolBar( Self )
      :Create()
      :AddBitmap( -1, IDB_STD_SMALL_COLOR )
      :DrawArrows()

      WITH OBJECT ToolButton( :this )
         :ImageIndex := ::System:StdIcons:FileNew
         :ToolTip    := "New"
         :Action     := {|o| o:Form:ListBox1:NewColumn() }
         :Create()
      END
      
      WITH OBJECT ToolButton( :this )
         :ImageIndex := ::System:StdIcons:Delete
         :ToolTip    := "Delete"
         :Action     := {|o| o:Form:ListBox1:DelColumn() }
         :Create()
      END
   END

   WITH OBJECT CoolBar( Self )
      :Dock:TopMargin := 0
      :Dock:Top := ::Image1
      :Create()

      CoolBarBand( :this )
      WITH OBJECT ::Band1
         :MinWidth  := 140
         :MinHeight := 22
         :Break     := .T.
         :Chevron   := .T.
         :BandChild := ::ToolBar1
         :Create()
      END
   END

   WITH OBJECT StatusBar( Self )
      StatusBarPanel( ::StatusBar1, , 120 )
      StatusBarPanel( ::StatusBar1, ,  -1 )
      StatusBarPanel( ::StatusBar1, , 250 )
      :Create()
      :DockIt()
   END

   WITH OBJECT Panel( Self )
      :Width         := 332
      :Dock:Margin   := 2
      :Dock:Top      := ::CoolBar1
      :Dock:Bottom   := ::StatusBar1
      :Dock:Right    := :Parent
      :Create()
      :DockIt()
   
      WITH OBJECT TabControl( :this )
         :Width         := 252
         :Height        := 22
         :BoldSelection := .T.
         :Flat          := TRUE
         :Frame         := FALSE
         :Dock:Top      := :Parent
         :Dock:Left     := :Parent
         :Dock:Right    := :Parent

         :Create()
         :InsertTab( "  Properties ", 0 )
         :InsertTab( "  Events ", 1 )
         :OnSelChanged := {|o,x,y|o:Parent:Parent:TabSelection( x,y ) }
         :DockIt()
      END

      WITH OBJECT ( ::ItemManager := ColObjManager( :this ) )
         :Width         := 332
         :Height        := 500
         :Dock:TopMargin:= 2
         :Caption       := NIL
         :Dock:Left     := :Parent
         :Dock:Top      := ::TabControl1
         :Dock:Bottom   := :Parent
         :Dock:Right    := :Parent

         :FullRowSelect := .T.

         :NoHScroll     := .T.
         :HasButtons    := .T.
         :LinesAtRoot   := .T.
         :ShowSelAlways := .T.

         :Columns := { {100,C_WHITE}, {200,C_WHITE} }
         :Create()
         //:DockIt()
         :BackColor := GetSysColor( COLOR_BTNFACE )
      END

      WITH OBJECT ( ::ItemEventManager := EventManager( :this ) )
         :Width         := 252
         :Height        := 500
         :Dock:TopMargin:= 2
         :Caption       := NIL
         :Dock:Top      := ::TabControl1
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
         //:DockIt()
         :BackColor := GetSysColor( COLOR_BTNFACE )
         :ExpandAll()
         :Hide()
      END
   END

   WITH OBJECT ColManager( Self )
      :Width         := 500
      :Height        := 500
      :Dock:Margin   := 4
      :Dock:Left     := :Parent
      :Dock:Top      := ::CoolBar1
      :Dock:Right    := ::Panel1
      :Dock:Bottom   := ::StatusBar1
      :VertScroll    := .T.
      :OwnerDraw     := ::System:ListBox:OwnerDrawVariable
      :Create()
      :ResetList()

      :SetFocus()
   END
   ::CenterWindow( .T. )

RETURN NIL


//------------------------------------------------------------------------------------------

METHOD TabSelection( nPrev, nCur ) CLASS ColumnManager
   IF nCur == 1
      ::ItemEventManager:Hide()
      ::ItemManager:Show()
    ELSE
      ::ItemEventManager:Show()
      ::ItemManager:Hide()
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------

CLASS ColObjManager INHERIT ObjManager
   METHOD SetValue()
ENDCLASS

METHOD SetValue( xValue, cCaption ) CLASS ColObjManager
   LOCAL oItem := FindTreeItem( ::Items, TVGetSelected( ::hWnd ) )
   Super:SetValue( xValue, cCaption, oItem )
   IF oItem:Caption == "Caption"
      ::Form:ListBox1:ResetList(.F.)
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------


CLASS ColManager INHERIT ListBox
   DATA nMove  INIT -1
   DATA nPos
   METHOD NewColumn()
   METHOD DelColumn()
   METHOD ResetList()
   METHOD OnParentDrawItem()
   METHOD OnParentMeasureItem()
   METHOD OnMouseMove()
   METHOD OnLButtonDown()
   METHOD OnLButtonUp()
   METHOD OnSelChange()
ENDCLASS

METHOD OnLButtonDown( nwParam, x, y ) CLASS ColManager
   ::nMove := ::GetTopIndex() + Int( y / ::GetItemHeight( 0 ) )
RETURN NIL
   
METHOD OnLButtonUp( nwParam, x, y ) CLASS ColManager
   ::Cursor := ::System:Cursor:Arrow
   ::nMove := -1
RETURN NIL

METHOD OnMouseMove( nwParam, x, y ) CLASS ColManager
   LOCAL nSel, cText, oCol
   IF ::nMove != -1

      IF ::Cursor == NIL .OR. ::Cursor == ::System:Cursor:Arrow
         ::Cursor := ::System:Cursor:SizeNS
      ENDIF

      nSel := ::GetTopIndex() + Int( y / ::GetItemHeight( 0 ) )
      IF nSel <> ::nMove .AND. nSel <> - 1 .AND. nSel < ::GetCount()
         cText := ::GetItemText( ::nMove )
         
         SendMessage( ::hWnd, LB_DELETESTRING, ::nMove, 0 )
         SendMessage( ::hWnd, LB_INSERTSTRING, nSel, cText )

         ::Parent:Grid:Children[::nMove+1]:Position := nSel + 1
         ::nMove := nSel
      ENDIF
   ENDIF   
RETURN NIL

METHOD OnParentMeasureItem( nwParam, nlParam, mis ) CLASS ColManager
   mis:itemHeight := 20
   mis:CopyTo( nlParam )
RETURN NIL

METHOD OnParentDrawItem() CLASS ColManager
   LOCAL lSelected, cText, hBrush

   lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED == ODS_SELECTED    

   IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE != 0 .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT != 0

      hBrush := GetSysColorBrush( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW ) )
      SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor( IIF( lselected, COLOR_HIGHLIGHTTEXT, COLOR_WINDOWTEXT ) ) )
      SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor( IIF( lselected, COLOR_HIGHLIGHT, COLOR_WINDOW ) ) )
      
      FillRect( ::Parent:DrawItemStruct:hDC, ::Parent:DrawItemStruct:rcItem, hBrush )
      
      cText := ::GetItemText( ::Parent:DrawItemStruct:ItemID )
      ::Parent:DrawItemStruct:rcItem:Left += 5
      DrawText( ::Parent:DrawItemStruct:hDC, cText, ::Parent:DrawItemStruct:rcItem, DT_SINGLELINE | DT_VCENTER )

   ENDIF
RETURN Self

METHOD OnSelChange() CLASS ColManager
   LOCAL nSel := ::CurSel
   IF nSel <= 0
      nSel := ::GetCaretIndex()
   ENDIF
   IF nSel > 0
      ::Parent:ItemManager:ResetProperties( {{ ::Parent:Grid:Children[::CurSel]  }} )
      ::Parent:ItemEventManager:ResetEvents( {{ ::Parent:Grid:Children[::CurSel]  }} )
   ENDIF
RETURN 0


//------------------------------------------------------------------------------------------

METHOD NewColumn() CLASS ColManager
    ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., ::Parent:Grid, "GridColumn",,,1, {}, } }, ::Application:Project:aUndo )
    ::Parent:Grid:Update()

    ::AddItem( ATAIL( ::Parent:Grid:Children ):Name )
    ::SetCurSel( ::GetCount() )
    
    
    ::Parent:ItemManager:ResetProperties( {{ ::Parent:Grid:Children[ MAX( ::CurSel, 1 ) ]  }} )
    ::Parent:ItemEventManager:ResetEvents( {{ ::Parent:Grid:Children[ MAX( ::CurSel, 1 ) ]  }} )
    //::Parent:ItemManager:EditCaption()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD DelColumn() CLASS ColManager
    LOCAL oItem
    IF LEN( ::Parent:Grid:Children ) > 0 .AND. ::CurSel > 0
       oItem := ::Parent:Grid:Children[ MAX( ::CurSel, 1 ) ]

       ::Application:Project:SetAction( { { DG_DELCONTROL, NIL, oItem:Left,;
                                                  oItem:Top,;
                                                  ::Form,;
                                                  oItem:Parent,; //oParent,;
                                                  oItem:__xCtrlName,;
                                                  oItem,,;
                                                  1,,, } }, ::Application:Project:aUndo )
       
       //::Parent:Grid:DeleteColumn( ::CurSel, .T. )
       
       ::ResetList()
    ENDIF
    IF ::GetCount() == 0
       ::Parent:ItemManager:ResetContent()
       ::Parent:ItemEventManager:ResetContent()
    ENDIF
RETURN Self

//------------------------------------------------------------------------------------------

METHOD ResetList( lReset ) CLASS ColManager
   LOCAL oItem, oSub, n
   DEFAULT lReset TO .T.
   ::SetRedraw(.F.)
   ::ResetContent()
   FOR EACH oItem IN ::Parent:Grid:Children
       ::AddItem( oItem:Caption )
   NEXT
   ::SetCurSel(1)
   n := MIN( ::GetCurSel(), LEN( ::Parent:Grid:Children ) )
   ::SetRedraw(.T.)
   ::UpdateWindow()

   IF lReset .AND. n > 0
      ::Parent:ItemManager:ResetProperties(  {{ IIF( EMPTY(::Parent:Grid:Children), NIL, ::Parent:Grid:Children[ MAX( n, 1 ) ] ) }} )
      ::Parent:ItemEventManager:ResetEvents( {{ IIF( EMPTY(::Parent:Grid:Children), NIL, ::Parent:Grid:Children[ MAX( n, 1 ) ] ) }} )
   ENDIF
   ::SetCurSel( MAX( n, 1 ) )
RETURN Self
















//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------

CLASS StructEditor INHERIT Dialog
   DATA DataSource  EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD Save()
   METHOD OnCancel()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oData ) CLASS StructEditor
   LOCAL lProp
   ::DataSource  := oData
   DEFAULT ::__xCtrlName  TO "StructEditor"
   
   ::Super:Init( ::Application:MainForm )

   ::Modal      := .T.
   ::DlgModalFrame := .T.
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnInitDialog() CLASS StructEditor
   LOCAL aField
   ::Caption    := "DataSource Structure Editor"
   
   PictureBox( Self )
   WITH OBJECT ::PictureBox1
      :Height          := 77
      :BackColor       := C_WHITE
      :Type            := "BMP"
      :ImageName       := "Banner"
      :KeepAspectRatio := .T.
      :Dock:Margin     := 0
      :Dock:Left       := :Parent
      :Dock:Top        := :Parent
      :Dock:Right      := :Parent
      :Create()
   END
   
   StatusBar( Self )
   WITH OBJECT ::StatusBar1
      :Create()
      :DockIt()
   END
   
   s_oSave := Button( Self )
   WITH OBJECT ::Button1
      :Caption := "&Save Changes"
      :Left    := 6
      :Top     := 510
      :Width   := 109
      :Height  := 30
      :Action  := {|o| o:Parent:Save(o)}
      :Enabled := .F.
      :Create()
   END
   
   Button( Self )
   WITH OBJECT ::Button2
      :Caption := "&Close"
      :Left    := 115
      :Top     := 510
      :Width   := 109
      :Height  := 30
      :id      := IDCANCEL
      :Create()
   END

   Label( Self )
   
   WITH OBJECT ::Label1
      :Caption := "Use the navigation keys"+CRLF+"DOWN appends blank at EOF"
      :Left    := 309
      :Top     := 510
      :Width   := 178
      :Height  := 30
      :Font:Bold := .T.
      :Create()
   END

   StrEditor( Self )
   ::CenterWindow( .T. )
RETURN 0

METHOD Save() CLASS StructEditor
   LOCAL cFile, hFile, nArea, nRec, n, lDel, nPos, xData, cType, x, cOrig, aDeleted, aTable, aStruct
   LOCAL cField, oFile, aFilter := { "dbf", "adt" }
   
   IF UPPER( RIGHT( ::DataSource:__xCtrlName, 9 ) ) IN {"DATATABLE","MEMORYDATATABLE"} .AND. !::DataSource:__lMemory

      IF ::DataSource:IsOpen
         FOR n := 1 TO LEN( ::DataSource:Structure )
             cField := ::DataSource:Structure[n][1]
             IF ASCAN( ::DataGrid1:DataSource:Table, {|a|a[1]==cField} ) == 0
                IF x == NIL
                   x := ::MessageBox( "Field " + cField + " has been removed/renamed," + CHR(13)+;
                                      "such field might be attached to several controls using this DataSource file," + CHR(13)+;
                                      "saving the structure might result in strange behaviour," + CHR(13) + CHR(13)+;
                                      "PROCEED ?", ::DataSource:Name, MB_YESNOCANCEL | MB_ICONEXCLAMATION )
                   IF x == IDCANCEL
                      RETURN .F.
                    ELSEIF x == IDNO
                      RETURN .T.
                   ENDIF
                ENDIF
             ENDIF
         NEXT
      ENDIF

      GetTempFileName( GetTempPath(), "vxh", 0, @cFile )
      
      n := AdsSetServerType(1)
      
      dbCreate( cFile, ::DataGrid1:DataSource:Table, ::DataSource:Driver )
      dbUseArea( .T., ::DataSource:Driver, cFile, "TEMPDATA", .F., .F. )
      
      IF ::DataSource:IsOpen
         nArea := Select()
         nRec  := ::DataSource:Recno()
         lDel  := Set( _SET_DELETED )
         SET DELETED OFF

         ::DataSource:GoTop()
         nPos := 1

         WHILE !::DataSource:Eof()
            (nArea)->( dbAppend() )
            FOR n := 1 TO LEN( ::DataSource:Structure )
                xData := (::DataSource:Alias)->( FieldGet( n ) )
                TRY
                   (nArea)->( fieldput( n, xData ) )
                CATCH
                END
            NEXT
            ::DataSource:Skip()
         ENDDO
         (nArea)->( dbCloseArea() )

         Set( _SET_DELETED, lDel )
         ::DataSource:Close()
         cOrig := ::DataSource:FileName
         IF !EMPTY( ::DataSource:Path )
            cOrig := ::DataSource:Path + "\" + ::DataSource:FileName
         ENDIF
         FERASE( cOrig )
         MoveFile( cFile, cOrig )
         
       ELSE
         nRec := 1
         oFile := CFile()
         oFile:Flags := OFN_EXPLORER | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST
         oFile:AddFilter( "DataTable File (*.dbf)", "*.dbf" )
         oFile:AddFilter( "Advantage (*.adt)", "*.adt" )
         oFile:SaveDialog()
         IF oFile:Result == IDOK
            IF AT( ".", oFile:Name ) == 0 // !( oFile:Name[-4] == "." )
               oFile:Name += "." + aFilter[ oFile:FilterIndex ]
            ENDIF
            CLOSE TEMPDATA
            MoveFile( cFile, oFile:Path + "\" + oFile:Name )
            ::DataSource:xFileName := oFile:Name
            ::DataSource:Path      := oFile:Path
          ELSE
            CLOSE TEMPDATA
            FERASE( cFile )
            RETURN .F.
         ENDIF
      ENDIF
      
      TRY
         AdsSetServerType( ::DataSource:ServerType )
      CATCH
      END
      ::DataSource:Create()
      ::DataSource:Goto( nRec )
    ELSE
      aTable  := ACLONE( ::DataSource:Table )
      aStruct := ACLONE( ::DataSource:Structure )
      ::DataSource:Structure := ACLONE( ::DataGrid1:DataSource:Table )
      ::DataSource:Table     := {}
      ::DataSource:Create()

      FOR n := 1 TO LEN( aTable )
          ::DataSource:Append()

          FOR x := 1 TO LEN( aTable[n] )
              TRY
                ::DataSource:Table[n][x] := aTable[n][x]
               catch
              END
          NEXT x
      NEXT n

   ENDIF
   s_oSave:Enabled := .F.
   ::Application:Project:Modified := .T.
RETURN .T.

METHOD OnCancel() CLASS StructEditor
   LOCAL nRet
   IF s_oSave:Enabled
      nRet := s_oSave:Parent:MessageBox( "Structure is changed, Save before closing?", "Structure Editor", MB_YESNOCANCEL | MB_ICONQUESTION )
      SWITCH nRet
         CASE IDYES
            IF !s_oSave:Parent:Save()
               RETURN 0
            ENDIF
            EXIT
         CASE IDCANCEL
            RETURN 0
      END
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS StrEditor INHERIT DataGrid
   METHOD Init() CONSTRUCTOR
   METHOD OnKeyDown()
   METHOD DataSave()
   METHOD OnChar()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS StrEditor
   LOCAL lEnter := .F.
   Super:Init( oParent )
   ::Height        := 425
   ::BackColor     := C_LIGHTYELLOW
   ::AutoVertScroll:= .T.
   ::Font:Bold     := .T.
   ::Dock:Margin   := 0
   ::Dock:Left     := ::Parent
   ::Dock:Top      := ::Parent:PictureBox1
   ::Dock:Right    := ::Parent
//   ::Dock:Bottom   := ::Parent:StatusBar1
   
   IF !::Parent:DataSource:IsOpen
      ::Parent:DataSource:Create()
   ENDIF
   ::DataSource := MemoryTable( ::Parent )
   
   WITH OBJECT ::DataSource
      :Structure := { {"NAME", "C", 100 }, {"TYPE", "C", 100 }, {"SIZE", "N", 3 }, {"DECIMALS", "N", 2 } }
      :Table     := ACLONE( ::Parent:DataSource:Structure )
      :Create()

      IF :Reccount() == 0
         lEnter := .T.
         :Append()
         :Fields:Name     := "FIELD_"+XSTR( :Reccount() )
         :Fields:Type     := "C"
         :Fields:Size     := 10
         :Fields:Decimals := 0
      ENDIF

   END

   WITH OBJECT GridColumn( Self )
      :Caption := "Field Name"
      :Data             := "hb_QSelf():DataSource:Fields:Name"
      :Width            := 265
      :Control          := {|o, n, oCtrl| oCtrl := EditBox( o ),;
                                          oCtrl:OnWMChar := {|o,nKey|ChkNameChar(o,nKey)},;
                                          oCtrl:OnWMKeyDown := {|o,nKey|ChkNameKey(o,nKey)},;
                                          oCtrl:OnWMDestroy := {|o|ChkNameDestroy(o)},;
                                          oCtrl:Case := 2,;
                                          oCtrl }
      :ControlAccessKey := GRID_CHAR
      :OnSave           := {|oCol, oGrid, xData| oGrid:DataSave( "Name", xData, {VK_RIGHT} ) }
      :Create()
   END

   WITH OBJECT GridColumn( Self )
      :Caption          := "Data Type"
//      :Data             := "hb_QSelf():DataSource:Fields:Type"
      :Data             := "GetDataType(hb_QSelf():DataSource:Fields:Type)"
      :Width            := 70
      :Control          := {|o, n, oCtrl| oCtrl := EditBox( o ),;
                                          oCtrl:OnWMChar := {|o,nKey|ChkTypeChar(o,nKey)},;
                                          oCtrl:Case := 2,;
                                          oCtrl }
      :ControlAccessKey := GRID_CHAR
      :Alignment        := 3
      :OnSave           := {|oCol, oGrid, xData| oGrid:DataSave( "Type", xData, {VK_RIGHT,VK_RETURN} ) }
      :Create()
   END

   WITH OBJECT GridColumn( Self )
      :Caption          := "Field Size"
      :Data             := "hb_QSelf():DataSource:Fields:Size"
      :Width            := 70
      :Alignment        := 2
      :Control          := {|o, n, oCtrl| IIF( !(o:DataSource:Fields:Type $"DML"), ( oCtrl := EditBox( o ),;
                                                                                  oCtrl:OnWMKeyDown := {|o,nKey| __SetEditKey( o, nKey )},;
                                                                                  oCtrl:OnWMChar := {|o,nKey|ChkNumberChar( nKey, o, 255, 1 )},;
                                                                                  oCtrl:Alignment := 3,;
                                                                                  oCtrl ),) }
      :ControlAccessKey := GRID_CHAR
      :OnSave           := {|oCol, oGrid, xData| oGrid:DataSave( "Size", MAX( VAL(xData), 1 ), {VK_RIGHT,VK_RETURN} ) }
      :Create()
   END

   WITH OBJECT GridColumn( Self )
      :Caption          := "Decimals"
      :Data             := "hb_QSelf():DataSource:Fields:Decimals"
      :Width            := 70
      :Alignment        := 2

      :Control          := {|o, n, oCtrl| IIF( o:DataSource:Fields:Type == "N", ( oCtrl := EditBox( o ),;
                                                                                  oCtrl:OnWMKeyDown := {|o,nKey| __SetEditKey( o, nKey )},;
                                                                                  oCtrl:OnWMChar := {|o,nKey|ChkNumberChar( nKey, o, 15, 0 )},;
                                                                                  oCtrl:Alignment := 3,;
                                                                                  oCtrl ),) }
      :ControlAccessKey := GRID_CHAR
      :OnSave           := {|oCol, oGrid, xData| oGrid:DataSave( "Decimals", MAX( VAL(xData), 0 ), {VK_LEFT,VK_LEFT,VK_LEFT,VK_DOWN,VK_RETURN} ) }
      :Create()
   END
   ::Create()
   ::SetFocus()
   IF lEnter
      ::PostMessage( WM_KEYDOWN, VK_RETURN )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnChar( nKey ) CLASS StrEditor
   IF ::ColPos == 2 .AND. ( nKey == VK_RETURN .OR. !UPPER( CHR( nKey ) ) $ "CNLMD"  )
      RETURN 0
   ENDIF
   Super:OnChar( nKey )
RETURN NIL

//------------------------------------------------------------------------------------------
METHOD OnKeyDown( nwParam, nlParam ) CLASS StrEditor
   IF ( nwParam == VK_DOWN .AND. ::DataSource:Recno() >= ::DataSource:RecCount() ) .OR. nwParam == VK_INSERT
      IF nwParam == VK_DOWN
         ::DataSource:Append()
       ELSE
         ::DataSource:Insert()
      ENDIF
      ::DataSource:Fields:Name     := "FIELD_"+XSTR( ::DataSource:Reccount() )
      ::DataSource:Fields:Type     := "C"
      ::DataSource:Fields:Size     := 10
      ::DataSource:Fields:Decimals := 0
      ::Update()
      ::ColPos := 1
      IF nwParam == VK_DOWN
         ::End()
      ENDIF
      ::PostMessage( WM_KEYDOWN, VK_RETURN )
      RETURN 0
      
    ELSEIF nwParam == VK_DELETE
      ::DataSource:Delete()
      ::Update()
      s_oSave:Enabled := .T.
      RETURN 0
      
    ELSEIF nwParam == VK_RETURN .AND. ::ColPos == 2
      RETURN Super:OnKeyDown( VK_RIGHT, 0 )
   ENDIF
   Super:OnKeyDown( nwParam, nlParam )
RETURN NIL


//------------------------------------------------------------------------------------------
METHOD DataSave( cField, xData, aKeys ) CLASS StrEditor
   IF !EMPTY( XSTR( xData ) )
      IF cField == "Type"
         xData := LEFT( xData, 1 )
      ENDIF
      ::DataSource:Fields:Put( xData, cField )
      IF EMPTY( ::DataSource:Fields:Name )
         RETURN .F.
      ENDIF
      s_oSave:Enabled := .T.
      AEVAL( aKeys, {|n| ::PostMessage( WM_KEYDOWN, n )} )
   ENDIF
RETURN .T.

//------------------------------------------------------------------------------------------

FUNCTION ChkNameDestroy( oEdit )
   LOCAL oGrid := oEdit:Parent
   oGrid:Form:StatusBar1:Caption := ""
   IF EMPTY( oGrid:DataSource:Fields:Name )
      oGrid:DataSource:Delete()
      oGrid:Update()
      IF oGrid:DataSource:Recno() >= oGrid:DataSource:RecCount()
         oGrid:End()
      ENDIF
   ENDIF
RETURN NIL

FUNCTION ChkNameChar( oEdit, nKey )
   LOCAL aExep := {32}
   IF !CHR( nKey ) $ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_" .AND. nKey >= 35 .AND. nKey <= 40 //ASCAN( aExep, nKey ) > 0
      oEdit:Form:StatusBar1:Caption := "Invalid Character"
      RETURN 0
   ENDIF
   oEdit:Form:StatusBar1:Caption := ""
   IF nKey == 27 .AND. EMPTY( oEdit:Parent:DataSource:Fields:Name )
      oEdit:Parent:DataSource:Delete()
      oEdit:Parent:Update()
   ENDIF
   IF !oEdit:Modified
      oEdit:Modified := .T.
      IF nKey >= 32 .AND. nKey < 255 .AND. nKey != VK_LEFT .AND. nKey != VK_RIGHT 
         oEdit:Caption := ""
      ENDIF
   ENDIF
RETURN NIL

FUNCTION ChkNameKey( oEdit, nKey )
   LOCAL n, oGrid := oEdit:Parent
   oGrid:Form:StatusBar1:Caption := ""
   IF nKey == 27
      oEdit:Destroy()
      oGrid:SetFocus()
    ELSEIF nKey == 13
      IF ( n := ASCAN( oEdit:Parent:DataSource:Table, {|a| a[1]==oEdit:Caption } ) ) > 0
         IF n != oEdit:Parent:DataSource:Recno()
            oEdit:Form:StatusBar1:Caption := "Field " + oEdit:Caption + " already exists"
            RETURN 0
         ENDIF
      ENDIF
      oGrid:__ControlSaveData()
      oEdit:Destroy()
    ELSEIF !oEdit:Modified
      oEdit:Modified := .T.
      IF nKey >= 32 .AND. nKey < 255 .AND. nKey != VK_LEFT .AND. nKey != VK_RIGHT 
         oEdit:Caption := ""
      ENDIF
   ENDIF
RETURN NIL

FUNCTION ChkTypeChar( oEdit, nKey )
   oEdit:LimitText(1)

   IF ( nKey >= 35 .AND. nKey <= 40 )
      oEdit:Form:StatusBar1:Caption := ""
      RETURN NIL
   ENDIF
   
   IF UPPER( CHR( nKey ) ) $ "CNLMD" 
      oEdit:Form:StatusBar1:Caption := ""
      IF UPPER( CHR( nKey ) ) == "D"
         oEdit:Parent:DataSource:Fields:SIZE := 8
      ENDIF
      IF UPPER( CHR( nKey ) ) == "L"
         oEdit:Parent:DataSource:Fields:SIZE := 1
      ENDIF
      oEdit:Caption := UPPER( CHR( nKey ) )
      oEdit:PostMessage( WM_KEYDOWN, VK_RETURN )
      RETURN NIL
   ENDIF
   oEdit:Form:StatusBar1:Caption := "Cannot set "+CHR(nKey)+" range is C N L M D"
RETURN 0

FUNCTION ChkNumberChar( nKey, oEdit, nMax, nMin )
   LOCAL nStart, nEnd, cCaption
   IF !CHR(nKey) $ "0123456789"
      IF ! ( nKey >= 35 .AND. nKey <= 40 ) .AND. nKey != VK_BACK .AND. nKey != VK_DELETE
         RETURN 0
      ENDIF
   ENDIF

   nStart := LoWord( oEdit:SendMessage( EM_GETSEL, 0, 0 ) ) + 1
   nEnd   := HiWord( oEdit:SendMessage( EM_GETSEL, 0, 0 ) ) + 1

   cCaption := oEdit:Caption+CHR(nKey)

   IF nEnd > nStart
      cCaption := IIF( nStart > 1, SUBSTR( cCaption, 1, nStart ), "" ) + SUBSTR( cCaption, nEnd )
   ENDIF

   IF nMax != NIL .AND. VAL( cCaption ) > nMax
      oEdit:Form:StatusBar1:Caption := "Cannot set "+cCaption+" maximum value is "+ XSTR(nMax)
      RETURN 0
   ENDIF

   IF nMin != NIL .AND. VAL( cCaption ) < nMin
      oEdit:Form:StatusBar1:Caption := "Cannot set "+cCaption+" minimum value is "+ XSTR(nMin)
      RETURN 0
   ENDIF
   
   oEdit:Form:StatusBar1:Caption := ""
   IF !oEdit:Modified
      oEdit:Modified := .T.
      IF nKey >= 32 .AND. nKey < 255 .AND. nKey != VK_LEFT .AND. nKey != VK_RIGHT 
         oEdit:Caption := ""
      ENDIF
   ENDIF
RETURN NIL

FUNCTION GetDataType( cType )
   DO CASE
      CASE cType == "N"
         RETURN "Numeric"
      CASE cType == "C"
         RETURN "Character"
      CASE cType == "D"
         RETURN "Date"
      CASE cType == "L"
         RETURN "Logical"
      CASE cType == "M"
         RETURN "Memo"
   ENDCASE
RETURN cType

//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------

CLASS TableEditor INHERIT Dialog
   DATA DataSource  EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD Save()
   METHOD OnCancel()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oData ) CLASS TableEditor
   LOCAL lProp
   ::DataSource  := oData
   DEFAULT ::__xCtrlName  TO "TableEditor"
   ::Super:Init( ::Application:MainForm )
   
   ::Modal      := .T.
   ::DlgModalFrame := .T.
   ::Top        := 400
   ::Width      := 500
   ::Height     := 600
   ::Style      := WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnInitDialog() CLASS TableEditor
   LOCAL aField
   ::Caption    := "DataSource Editor"
   
   WITH OBJECT PictureBox( Self )
      :Height          := 77
      :BackColor       := C_WHITE
      :Type            := "BMP"
      :ImageName       := "Banner"
      :KeepAspectRatio := .T.
      :Dock:Margin     := 0
      :Dock:Left       := :Parent
      :Dock:Top        := :Parent
      :Dock:Right      := :Parent
      :Create()
   END
   
   WITH OBJECT StatusBar( Self )
      :Create()
      :DockIt()
   END
   
   WITH OBJECT s_oSave := Button( Self )
      :Caption := "&Save Changes"
      :Left    := 6
      :Top     := 510
      :Width   := 109
      :Height  := 30
      :Action  := {|o| o:Parent:Save(o)}
      :Enabled := .F.
      :Create()
   END
   
   WITH OBJECT Button( Self )
      :Caption := "&Close"
      :Left    := 115
      :Top     := 510
      :Width   := 109
      :Height  := 30
      :id      := IDCANCEL
      :Create()
   END

   WITH OBJECT Label( Self )
      :Caption := "Use the navigation keys"+CRLF+"DOWN appends blank at EOF"
      :Left    := 309
      :Top     := 510
      :Width   := 178
      :Height  := 30
      :Font:Bold := .T.
      :Create()
   END

   TblEditor( Self )
   ::CenterWindow( .T. )
RETURN 0

METHOD Save() CLASS TableEditor
   ::DataSource:Table := ACLONE( ::DataGrid1:DataSource:Table )
   ::DataSource:GoTop()
   ::Application:Project:Modified := .T.
   s_oSave:Enabled := .F.
RETURN Self

METHOD OnCancel() CLASS TableEditor
   LOCAL nRet
   IF s_oSave:Enabled
      nRet := s_oSave:Parent:MessageBox( "Table is changed, Save before closing?", "DataSource Editor", MB_YESNOCANCEL | MB_ICONQUESTION )
      SWITCH nRet
         CASE IDYES
            s_oSave:Parent:Save()
            EXIT
         CASE IDCANCEL
            RETURN 0
      END
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS TblEditor INHERIT DataGrid
   METHOD Init() CONSTRUCTOR
   METHOD DataSave()
   METHOD OnKeyDown()
ENDCLASS

//------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS TblEditor
   LOCAL aField, cField, oCol, lEnter := .F.
   Super:Init( oParent )
   ::Height        := 425
   ::BackColor     := C_LIGHTYELLOW
   ::Font:Bold     := .T.
   ::Dock:Margin   := 0
   ::Dock:Left     := ::Parent
   ::Dock:Top      := ::Parent:PictureBox1
   ::Dock:Right    := ::Parent
   
   ::DataSource := oParent:DataSource //MemoryTable( ::Parent )
   /*
   WITH OBJECT ::DataSource
      :Structure := ::Parent:DataSource:Structure
      :Table     := ACLONE( ::Parent:DataSource:Table )
      :Create()
      IF :Reccount() == 0
         lEnter := .T.
         :Append()
      ENDIF
   END
*/
   FOR EACH aField IN ::DataSource:Structure

       cField := "oGrid:DataSource:Fields:" + aField[1]

       oCol := GridColumn( Self )
       oCol:xCaption  := __Proper( aField[1] )
       oCol:Data      := "hb_QSelf():DataSource:Fields:" + aField[1]
       oCol:xWidth    := MAX( aField[3], LEN(oCol:Caption)+2 )*7
       oCol:AllowSize := .T.
       oCol:AllowDrag := .T.
       oCol:Create()

       oCol:ControlAccessKey := GRID_CHAR
       oCol:OnSave  := {|oCol, oGrid, xData| oGrid:DataSave( aField[1], xData ) }
       
       DO CASE
          CASE aField[2]=="C"
               oCol:Control := {|o, n, oCtrl| oCtrl := EditBox( o ),;
                                              oCtrl:OnWMChar := {|o,nKey|__SetEditKey( o, nKey )},;
                                              oCtrl:OnWMKeyDown := {|o,nKey| __SetEditKey( o, nKey )},;
                                              oCtrl }
          CASE aField[2]=="D"
               oCol:Alignment := 3
               oCol:Control := {|o, n, oCtrl| oCtrl := MaskEdit( o ),;
                                              oCtrl }

          CASE aField[2]=="L"
               oCol:Alignment := 3
               oCol:Width := MAX( 6, LEN(oCol:Caption)+2 )*7
               oCol:Control := {|o, n, oCtrl| oCtrl := MaskEdit( o ),;
                                              oCtrl }
          CASE aField[2]=="N"
               oCol:Alignment := 2
               oCol:Control := {|o, n, oCtrl| oCtrl := EditBox( o ),;
                                              oCtrl:OnWMKeyDown := {|o,nKey| __SetEditKey( o, nKey )},;
                                              oCtrl:OnWMChar := {|o,nKey|ChkNumberChar( nKey, o )},;
                                              oCtrl:Alignment := 3,;
                                              oCtrl }
       ENDCASE

   NEXT
   
   ::Create()
   ::SetFocus()
   IF lEnter
      ::PostMessage( WM_KEYDOWN, VK_RETURN )
   ENDIF
RETURN Self

FUNCTION __SetEditKey( o, nKey )
   SWITCH nKey
      CASE 27
           o:Destroy()
           RETURN NIL
      CASE 13
           o:Parent:__ControlSaveData()
           o:Destroy()
           o:Parent:DataSource:UnLock()
           RETURN NIL
   END
   IF !o:Modified
      o:Modified := .T.
      IF nKey >= 32 .AND. nKey < 255 .AND. nKey != VK_LEFT .AND. nKey != VK_RIGHT 
         o:Caption := ""
      ENDIF
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------
METHOD DataSave( cField, xData, aKeys ) CLASS TblEditor
   IF !EMPTY( XSTR( xData ) )
      ::DataSource:Fields:Put( xData, cField )
      s_oSave:Enabled := .T.
   ENDIF
RETURN .T.

//------------------------------------------------------------------------------------------
METHOD OnKeyDown( nwParam, nlParam ) CLASS TblEditor
   IF ( nwParam == VK_DOWN .AND. ::DataSource:Recno() >= ::DataSource:RecCount() ) .OR. nwParam == VK_INSERT .OR. ::DataSource:RecCount() == 0
      IF nwParam == VK_DOWN
         ::DataSource:Append()
       ELSE
         ::DataSource:Insert()
      ENDIF
      ::Update()
      IF nwParam == VK_DOWN
         ::End()
      ENDIF
      ::PostMessage( WM_KEYDOWN, VK_RETURN )
      RETURN 0
    ELSEIF nwParam == VK_DELETE
      IF ::DataSource:RecLock()
         ::DataSource:Delete()
         ::DataSource:Unlock()
      ENDIF
      ::Update()
      s_oSave:Enabled := .T.
      RETURN 0
   ENDIF
   Super:OnKeyDown( nwParam, nlParam )
RETURN NIL
