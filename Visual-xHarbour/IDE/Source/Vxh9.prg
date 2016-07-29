/*
 * $Id$
 */

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"

#define FILE_ATTRIBUTE_NORMAL    0x00000080
#define FILE_ATTRIBUTE_DIRECTORY 0x00000010

#define SHGFI_SMALLICON          0x000000001
#define SHGFI_SHELLICONSIZE      0x000000004
#define SHGFI_USEFILEATTRIBUTES  0x000000010
#define SHGFI_ICON               0x000000100
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------

CLASS ImageManager INHERIT Dialog
   DATA ImageList        EXPORTED
   DATA ItemManager      EXPORTED
   DATA ItemEventManager EXPORTED
   DATA aDeleted         EXPORTED INIT {}
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD AddImage()
   METHOD DeleteImage()
   METHOD OnOk()
   METHOD ImageListView_ColChanged()
   METHOD ImageList_RowChanged()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oList ) CLASS ImageManager
   ::ImageList  := oList
   DEFAULT ::__xCtrlName  TO "ImageManager"

   ::Super:Init( ::Application:MainForm )
   ::Width  := 400
   ::height := 500
   ::Modal      := .T.
   ::Create()
RETURN Self

//------------------------------------------------------------------------------------------
METHOD ImageListView_ColChanged() CLASS ImageManager
   ::DataGrid1:DataSource:Goto( ::DataGrid2:ColPos )
   ::DataGrid1:Update()
RETURN Self

//------------------------------------------------------------------------------------------
METHOD ImageList_RowChanged() CLASS ImageManager
   ::DataGrid2:ColPos := ::DataGrid1:DataSource:Recno()
   ::DataGrid2:Update()
RETURN Self

//------------------------------------------------------------------------------------------
METHOD OnInitDialog() CLASS ImageManager
   LOCAL n
   ToolBar( Self )
   WITH OBJECT ::ToolBar1
      :Create()
      :AddBitmap( -1, IDB_STD_SMALL_COLOR )
      :DrawArrows()

      WITH OBJECT ToolButton( :this )
         :ImageIndex := ::System:StdIcons:FileNew
         :ToolTip    := "New Image"
         :Action     := {|o| o:Parent:Parent:AddImage()}
         :Create()
      END

      WITH OBJECT ToolButton( :this )
         :ImageIndex := ::System:StdIcons:Delete
         :ToolTip    := "Delete Image"
         :Action     := {|o| o:Parent:Parent:DeleteImage()}
         :Create()
      END
   END

   WITH OBJECT CoolBar( Self )
      :DockToParent()
      :Dock:Bottom := NIL
      :Create()
      WITH OBJECT CoolBarBand( :this )
         :MinWidth    := 140
         :MinHeight   := 22
         :Break       := .T.
         :Chevron     := .T.
         :BandChild   := ::ToolBar1
         :Create()
      END
   END

   WITH OBJECT Button( Self )
      :Text        := "&Cancel"
      :ID          := IDCANCEL
      :Dock:Right  := Self
      :Dock:Bottom := Self
      :Create()
   END

   WITH OBJECT Button( Self )
      :Text        := "&OK"
      :ID          := IDOK
      :Dock:Right  := ::Button1
      :Dock:Bottom := Self
      :Create()
   END

   WITH OBJECT DataGrid( Self )
      :Left    := 5
      :Top     := 5
      :Width   := 250
      :Height  := 250
      :EventHandler[ "OnRowChanged" ] := "ImageList_RowChanged"

      WITH OBJECT :ImageList := ImageList( :this )
         :IconWidth  := ::ImageList:IconWidth
         :IconHeight := ::ImageList:IconHeight
         :Images     := ACLONE( ::ImageList:Images )
         :Create()
      END

      :ItemHeight    := ::ImageList:IconHeight+2
      :Dock:Margin   := 2
      :Dock:Left     := :Parent
      :Dock:Top      := :Parent:Coolbar1
      :Dock:Right    := :Parent

      WITH OBJECT :DataSource := MemoryTable( ::Parent )
         :Structure := { {"Resource", "C", 247 } }
         :Table     := {}
         FOR n := 1 TO LEN( ::ImageList:Images )
             AADD( :Table, { ::ImageList:Images[n][1] } )
         NEXT
         :Create()
      END

      WITH OBJECT GridColumn( :this )
         :Text       := "Resource Name"
         :Data       := "hb_QSelf():DataSource:Fields:Resource"
         :Width      := :Parent:Parent:ClientWidth-6
         :ImageIndex := {|o| o:Record }
         :AllowSize  := .F.
         :Create()
      END
      :AnchorColumn(1)
      :Create()
      :Home()
   END

   WITH OBJECT DataGrid( Self )
      :Text           := "ImageList View"
      :Left           := 5
      :Top            := 0
      :AutoHorzScroll := .F.
      :ShadowRow      := .F.
      :ShowHeaders    := .F.
      :ShowGrid       := .F.
      :HeaderHeight   := 0
      //:BackColor      := GetSysColor(COLOR_BTNFACE)
      :ItemHeight     := ::ImageList:IconHeight + 2

      :Height         := ::ImageList:IconHeight + 5 + :TitleHeight
      :Dock:Left      := :Parent
      :Dock:Right     := :Parent
      :Dock:Bottom    := ::Button1
      :EventHandler[ "OnColChanged" ] := "ImageListView_ColChanged"

      WITH OBJECT :ImageList := ImageList( :this )
         :IconWidth  := ::ImageList:IconWidth
         :IconHeight := ::ImageList:IconHeight
         :Images     := ACLONE( ::ImageList:Images )
         :Create()
      END

      WITH OBJECT :DataSource := MemoryTable( ::Parent )
         :Structure := {}
         :Table     := {}
         FOR n := 1 TO ::ImageList:Count
             AADD( :Structure, {"Image_"+XStr(n), "N", ::ImageList:IconWidth } )
         NEXT
         :Create()
         :Append()
         FOR n := 1 TO ::ImageList:Count
             :Table[1][n] := n //-1
         NEXT
      END
      :Create()
      :AutoAddColumns()
      FOR n := 1 TO ::ImageList:Count
          :Children[n]:Width      := :ImageList:IconWidth+20
          :Children[n]:Picture    := "9999"
          :Children[n]:ImageIndex := n
      NEXT
      :Update()
   END

   ::DataGrid1:Dock:Bottom := ::DataGrid2
   ::DataGrid1:DockIt()

   IF ::DataGrid1:DataSource:RecCount() == 0
      ::ToolButton2:Disable()
   ENDIF

   ::CenterWindow( .T. )
RETURN NIL

METHOD AddImage() CLASS ImageManager
   LOCAL hBmp, oData, nCount, nWidth, n, cFile, lCopy, oFile := CFile( "" )

   oFile:AddFilter( "Supported Files (*.bmp,*.ico)", "*.bmp;*.ico" )
   oFile:AddFilter( "Windows Bitmap (*.bmp)", "*.bmp" )
   oFile:AddFilter( "Icon Files (*.ico)", "*.ico" )

   oFile:AllowMultiSelect := .T.
   oFile:OpenDialog()
   IF oFile:Result != IDCANCEL .AND. oFile:Path != NIL .AND. oFile:Name != NIL

      IF ::DataGrid1:DataSource:RecCount() == 0
         ::ToolButton2:Enable()
      ENDIF
      IF ::ImageList:MaskColor == NIL .AND. LOWER( RIGHT( oFile:Name[1], 3 ) ) == "bmp"
         IF ::MessageBox( "Use the bitmap background color as the ImageList MaskColor?", "Resources", MB_ICONEXCLAMATION | MB_YESNO ) == IDYES
            hBmp := LoadImage( GetModuleHandle(), oFile:Path + "\" + oFile:Name[1], IMAGE_BITMAP,,, LR_LOADFROMFILE )
            ::DataGrid1:ImageList:MaskColor := __GetPixelFromBMP( hBmp )
            DeleteObject( hBmp )
         ENDIF
      ENDIF

      lCopy := .F.
      IF !( oFile:Path == ::Application:Project:Properties:path + "\" + ::Application:Project:Properties:Resource )
         IF ::MessageBox( "Selected files are not in the project path, would you like to copy them to the resource folder?", "Resources", MB_ICONEXCLAMATION | MB_YESNO ) == IDYES
            ::Application:Project:Save()
            lCopy := .T.
         ENDIF
      ENDIF

      WITH OBJECT ::DataGrid1
         FOR EACH cFile IN oFile:Name
             IF lCopy
                CopyFile( oFile:Path + "\" + cFile, ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Resource + "\" + cFile )
                :ImageList:AddImage( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Resource + "\" + cFile,,, LR_LOADFROMFILE, IIF( UPPER( RIGHT( cFile, 4 ) ) == ".ICO", IMAGE_ICON, IMAGE_BITMAP ) )
               ELSE
                :ImageList:AddImage( oFile:Path + "\" + cFile,,, LR_LOADFROMFILE, IIF( UPPER( RIGHT( cFile, 4 ) ) == ".ICO", IMAGE_ICON, IMAGE_BITMAP ) )
             ENDIF
             AADD( :DataSource:Table, { :ImageList:Images[ LEN( :ImageList:Images) ][1] } )
         NEXT
         :DataSource:GoBottom()
         :Update()
      END

      WITH OBJECT ::DataGrid2
         :ImageList  := ::DataGrid1:ImageList
         nCount := :ImageList:Count
         nWidth := :ImageList:IconWidth
         oData  := MemoryTable( Self )
         WITH OBJECT oData
            :Structure := {}
            :Table     := {}
            FOR n := 1 TO nCount
                AADD( :Structure, {"Image_"+XStr(n), "N", nWidth } )
            NEXT
            :Create()
            :Append()
            FOR n := 1 TO nCount
                :Table[1][n] := n
            NEXT
         END
         :DataSource:Destroy()
         :DataSource := oData
         :AutoAddColumns()
         FOR n := 1 TO nCount
             :Children[n]:Width      := :ImageList:IconWidth+20
             :Children[n]:ImageIndex := n
             :Children[n]:Picture    := "9999"
         NEXT
         :Update()
      END
   ENDIF
RETURN Self

METHOD DeleteImage() CLASS ImageManager
   LOCAL oData, nCount, nWidth, n

   AADD( ::aDeleted, ::DataGrid1:DataSource:Fields:Resource )

   ADEL( ::DataGrid1:ImageList:Images, ::DataGrid1:DataSource:Record, .T. )
   ImageListDestroy( ::DataGrid1:ImageList:Handle )
   ::DataGrid1:ImageList:Create()
   ::DataGrid1:DataSource:Delete()
   ::DataGrid1:Update()

   IF ::DataGrid1:DataSource:RecCount() == 0
      ::ToolButton2:Disable()
   ENDIF

   WITH OBJECT ::DataGrid2
      :ImageList  := ::DataGrid1:ImageList
      nCount := :ImageList:Count
      nWidth := :ImageList:IconWidth
      :DataSource:Destroy()
      oData  := MemoryTable( Self )
      WITH OBJECT oData
         :Structure := {}
         :Table     := {}
         FOR n := 1 TO nCount
             AADD( :Structure, {"Image_"+XStr(n), "N", nWidth } )
         NEXT
         :Create()
         :Append()
         FOR n := 1 TO nCount
             :Table[1][n] := n
         NEXT
      END
      :DataSource := oData
      :AutoAddColumns()
      FOR n := 1 TO nCount
          :Children[n]:Width      := :ImageList:IconWidth+20
          :Children[n]:ImageIndex := n
          :Children[n]:Picture    := "9999"
      NEXT
      :Update()
   END
RETURN Self

METHOD OnOk() CLASS ImageManager
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
   ::Application:Project:CurrentForm:__lModified := .T.
   ::Close( IDOK )
RETURN NIL

//-------------------------------------------------------------------------------------------------------

CLASS ObjectTreeView INHERIT TreeView
   DATA aImages  EXPORTED INIT {}
   DATA CurObj   EXPORTED
   DATA oList
   DATA oApp, oPrj
   DATA oDrag, nPrgImg, nCImg, nXfmImg
   METHOD InitProject()
   METHOD Set()
   METHOD OnKeyDown()
   METHOD OnSelChanged()
   METHOD GetImage()
   METHOD OnRightClick()
   METHOD OnBeginDrag()
   METHOD OnEndDrag()
   METHOD OnUserMsg()
   METHOD ResetContent()
ENDCLASS

METHOD OnBeginDrag( /*oDrag*/ ) CLASS ObjectTreeView
   //::oDrag := oDrag
RETURN Self

METHOD ResetContent() CLASS ObjectTreeView
   ::aImages := {}
   ::ImageList := NIL
   IF ::oList != NIL
      ::oList:Destroy()
   ENDIF
   IF ::Application:Project:Properties != NIL
      ::Application:Project:Properties:TreeItem := NIL
   ENDIF
   IF ::oApp != NIL
      ::oApp:Cargo := NIL
      ::oPrj:Cargo := NIL
      ::oPrj:Delete()
    ELSE
      Super:ResetContent()
   ENDIF
RETURN NIL

METHOD OnEndDrag( oTarget ) CLASS ObjectTreeView
   LOCAL nPos, oObj, nPre//, oItem
   IF .F. //::oDrag != NIL .AND. oTarget != NIL
      oObj := ::oDrag:Cargo

      nPre := ASCAN( ::oDrag:Cargo:Parent:Children, {|o| o==oObj} )
      nPos := ASCAN( oTarget:Cargo:Parent:Children, {|o| o==oTarget:Cargo} )
      IF nPos > 0
         IF ! ( ::oDrag:Cargo:Parent == oTarget:Cargo:Parent )
            //::oDrag:Cargo:SetParent( oTarget:Cargo:Parent )
            RETURN NIL
         ENDIF
         ::oDrag:Cargo:TabOrder := nPos
         //ADEL( oTarget:Cargo:Parent:Children, nPre, .T. )
         //AINS( oTarget:Cargo:Parent:Children, nPos, oObj, .T. )
         //oItem := ::MoveItem( ::oDrag, nPos, oTarget:Owner )
         //oItem:Select()
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnKeyDown( nKey ) CLASS ObjectTreeView
   IF nKey == VK_DELETE .AND. ::SelectedItem != NIL
      ::Application:Project:DelControl( ::SelectedItem:Cargo )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD InitProject() CLASS ObjectTreeView
   LOCAL cProject, shfi := (struct SHFILEINFO)

   ::oList := ImageList( Self, 16, 16 ):Create()
   ::oList:MaskColor := C_LIGHTCYAN
   ::oList:AddBitmap( "TREE" )

   SHGetFileInfo( ".prg", FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
   ::oList:AddIcon( shfi:hIcon )
   ::nPrgImg := ::oList:Count

   SHGetFileInfo( ".c", FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
   ::oList:AddIcon( shfi:hIcon )
   ::nCImg := ::oList:Count

   SHGetFileInfo( ".xfm", FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
   ::oList:AddIcon( shfi:hIcon )
   ::nXfmImg := ::oList:Count

   ::ImageList := ::oList

   cProject := ::Application:Project:Properties:Name
   IF EMPTY( cProject )
      cProject := "Untitled"
   ENDIF
   ::oPrj := ::AddItem( cProject, 6 )
   ::oPrj:Cargo := ::Application:Project:Properties
   ::Application:Project:Properties:TreeItem := ::oPrj

   IF ! Empty( ::Application:Project:Forms )
      ::oApp := ::oPrj:AddItem( "Application", 9 )
      ::oApp:Cargo := ::Application:Project:AppObject
      ::Application:Project:AppObject:TreeItem := ::oApp

      ::Application:DesignPage:TreeItem := ::oApp
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD GetImage( oObj, lChange ) CLASS ObjectTreeView
   LOCAL hIcon, n, cName := oObj:__xCtrlName
   DEFAULT lChange TO .F.

   n := ASCAN( ::aImages, {|a| a[1] == oObj:Name} )
   IF n > 0
      IF !EMPTY( oObj:__hIcon )
         ::oList:ReplaceImage( ::aImages[n][2]-1, oObj:__hIcon )
         RETURN ::aImages[n][2]

       ELSEIF oObj:ClsName == "VXH_FORM_IDE" .AND. ::oList:GetImage( ::aImages[n][2]-1 ) != NIL
         ADEL( ::aImages, ::aImages[n][2]-1 )
         RETURN -1

       ELSEIF lChange .AND. oObj:HasMessage( "ImageIndex" ) .AND. oObj:ImageIndex > 0 .AND. oObj:Parent:ImageList != NIL
         ::oList:AddIcon( oObj:Parent:ImageList:GetImage( oObj:ImageIndex ) )
         ::aImages[n][2] := ::oList:Count
         n := ::oList:Count
      ENDIF
   ENDIF

   n := ASCAN( ::aImages, {|a| a[1] == cName} )
   IF n > 0
      n := ::aImages[n][2]
    ELSEIF cName != NIL
      hIcon := ::oList:AddImage( "ICO_" + UPPER( cName ) )

      IF hIcon != NIL
         AADD( ::aImages, { cName, ::oList:Count } )
         n := ::oList:Count

       ELSEIF oObj:HasMessage( "ImageIndex" )
         TRY
            IF oObj:ImageIndex > 0
               ::oList:AddIcon( oObj:Parent:ImageList:GetImage( oObj:ImageIndex ) )
               AADD( ::aImages, { oObj:Name, ::oList:Count } )
               n := ::oList:Count
            ENDIF
          CATCH
         END
       ELSEIF !EMPTY( oObj:__hIcon )
         ::oList:AddIcon( oObj:__hIcon )
         AADD( ::aImages, { oObj:Name, ::oList:Count } )
         n := ::oList:Count

       ELSE
         n := 1
      ENDIF
   ENDIF
RETURN n

//-------------------------------------------------------------------------------------------------------
METHOD Set( oObj, nImg ) CLASS ObjectTreeView
   IF ! oObj:ClsName == "Application"
      IF oObj:TreeItem == NIL
         DEFAULT nImg TO ::GetImage( oObj )
         TRY
            oObj:TreeItem := oObj:Parent:TreeItem:AddItem( oObj:Name, nImg )
            oObj:TreeItem:Cargo := oObj
         CATCH
         END
       ELSE
         DEFAULT nImg TO ::GetImage( oObj, .T. )
         oObj:TreeItem:Text := oObj:Name
         oObj:TreeItem:ImageIndex := nImg
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnUserMsg( hWnd, nMsg ) CLASS ObjectTreeView
   (hWnd)
   IF nMsg == WM_USER + 555
      ::Application:SourceEditor:SetFocus()
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnSelChanged( oItem ) CLASS ObjectTreeView
   IF EMPTY( procname(10) ) .OR. procname(10) == "OBJECTTREEVIEW:ONLBUTTONDOWN"
      IF ASCAN( ::Application:Project:Forms, {|o| oItem:Cargo != NIL .AND. o == oItem:Cargo:Form} ) > 0
         IF oItem:Cargo:Form:hWnd != ::Application:Project:CurrentForm:hWnd
            ::Application:Project:SelectWindow( oItem:Cargo:Form, ::hWnd )
         ENDIF
      ENDIF
      IF oItem:Cargo:Parent != NIL .AND. oItem:Cargo:Parent:__xCtrlName == "TabPage"
         oItem:Cargo:Parent:Select()
      ENDIF
      IF oItem:Cargo != NIL .AND. oItem:Cargo:__xCtrlName == "TabPage"
         oItem:Cargo:Select()
      ENDIF
      IF ::Application:Project:CurrentForm != NIL
         ::Application:Project:CurrentForm:SelectControl( oItem:Cargo, .T. )
      ENDIF
   ENDIF
RETURN NIL

METHOD OnRightClick() CLASS ObjectTreeView
   LOCAL pt := (struct POINT), oItem, Item, oMenu, oSel, lGroup := .F.

   GetCursorPos( @pt )
   ScreenToClient( ::hWnd, @pt )
   oSel := ::HitTest( pt:x, pt:y )

   oMenu := ContextStrip( Self )
   oMenu:Create()
   oMenu:ImageList := ImageList( Self, 16, 16 ):Create()
   oMenu:ImageList:AddImage( IDB_STD_SMALL_COLOR )

   IF oSel != NIL .AND. __ObjHasMsg( oSel:Cargo, "__IdeContextMenuItems" ) .AND. !EMPTY( oSel:Cargo:__IdeContextMenuItems )
      FOR EACH Item IN oSel:Cargo:__IdeContextMenuItems
          oItem := MenuStripItem( oMenu )
          oItem:Text   := Item[1]
          oItem:Action := Item[2]
          oItem:Create()
      NEXT
      lGroup := .T.
   ENDIF

   oItem := MenuStripItem( oMenu )
   oItem:Text       := "&Delete"
   oItem:ImageIndex := ::System:StdIcons:Delete
   oItem:BeginGroup := lGroup
   oItem:Action     := {|| ::Application:Project:DelControl( oSel:Cargo ) }
   oItem:Create()

   GetCursorPos( @pt )
   oMenu:Show( pt:x, pt:y )
RETURN NIL

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
CLASS FileExplorer INHERIT TreeView
   DATA Project     EXPORTED
   DATA Main        EXPORTED
   DATA ExtSource   EXPORTED
   DATA ExtBinary   EXPORTED
   DATA aExt        EXPORTED INIT {".prg",".c",".lib",".obj"}
   DATA lSkipSelect EXPORTED INIT .F.

   METHOD Init() CONSTRUCTOR

   METHOD InitProject()

   METHOD AddSource()
   METHOD AddExtSource()
   METHOD AddExtBinary()

   METHOD OnSelChanged()
   METHOD OnDropFiles()
   METHOD OnUserMsg()
   METHOD OnRightClick()
   METHOD GetImage()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS FileExplorer
   LOCAL cExt, shfi := (struct SHFILEINFO)

   Super:Init( oParent )
   ::Border := 0
   ::ImageList := ImageList( Self, 16, 16 ):Create()

   SHGetFileInfo( GetModuleFileName(), FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
   ::ImageList:AddIcon( shfi:hIcon )

   SHGetFileInfo( GetWindowsDirectory(), FILE_ATTRIBUTE_DIRECTORY, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
   ::ImageList:AddIcon( shfi:hIcon )

   FOR EACH cExt IN ::aExt
       SHGetFileInfo( cExt, FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
       ::ImageList:AddIcon( shfi:hIcon )
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD ResetContent() CLASS FileExplorer
   ::aImages := {}
   ::ImageList := NIL
   IF ::oList != NIL
      ::oList:Destroy()
   ENDIF
   IF ::Application:Project:Properties != NIL
      ::Application:Project:Properties:TreeItem := NIL
   ENDIF
   IF ::oApp != NIL
      ::oApp:Cargo := NIL
      ::oPrj:Cargo := NIL
      ::oPrj:Delete()
    ELSE
      Super:ResetContent()
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD GetImage( cFile )  CLASS FileExplorer
   LOCAL shfi, n, cExt := RIGHT( cFile, LEN(cFile)-RAT( ".", cFile )+1 )

   n := ASCAN( ::aExt, {|c|lower(c)==lower(cExt)} )

   IF n == 0
      shfi := (struct SHFILEINFO)
      SHGetFileInfo( cExt, FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
      ::ImageList:AddIcon( shfi:hIcon )
      AADD( ::aExt, cExt )
      n := Len(::aExt)
   ENDIF

   IF n > 0
      n += 2
   ENDIF
RETURN n

//-------------------------------------------------------------------------------------------------------
METHOD OnUserMsg( hWnd, nMsg ) CLASS FileExplorer
   (hWnd)
   IF nMsg == WM_USER + 555
      ::Application:SourceEditor:SetFocus()
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnDropFiles( nwParam ) CLASS FileExplorer
   LOCAL cFile, oEditor, n, cExt, shfi := (struct SHFILEINFO)
   Super:OnDropFiles( nwParam )
   FOR EACH cFile IN ::DragDrop:Files
       IF RIGHT( lower( cFile ), 4 ) $ ".lib.obj"
          ::AddExtBinary( cFile )
        ELSE
          IF ( n := RAT( ".", cFile ) ) > 0
             cExt := SubStr( cFile, n )
             IF ASCAN( ::aExt, {|c|lower(c)==lower(cExt)} ) == 0
                SHGetFileInfo( cExt, FILE_ATTRIBUTE_NORMAL, @shfi, SHGFI_ICON|SHGFI_SMALLICON|SHGFI_USEFILEATTRIBUTES )
                ::ImageList:AddIcon( shfi:hIcon )
                AADD( ::aExt, cExt )
             ENDIF
          ENDIF

          oEditor := Source( ::Application:SourceEditor )
          oEditor:Open( cFile )
          ::AddExtSource( oEditor )
          ::Application:Project:SourceTabChanged( ::Application:SourceEditor:DocCount )
          ::Application:SourceEditor:Parent:Select()
       ENDIF
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD InitProject() CLASS FileExplorer
   LOCAL cName := ::Application:Project:Properties:Name

   ::ResetContent()

   ::Project := ::AddItem( cName, 1 )
   ::Project:Bold := .T.

   ::Main := ::Project:AddItem( cName + "_Main.prg" + IIF( ::Application:ProjectPrgEditor != NIL .AND. ::Application:ProjectPrgEditor:Modified, " *", ""), ::GetImage( cName + "_Main.prg" )  )
   ::Main:Cargo := ::Application:ProjectPrgEditor
   IF ::Application:ProjectPrgEditor != NIL
      ::Application:ProjectPrgEditor:TreeItem := ::Main
   ENDIF

   ::ExtSource := ::Project:AddItem( "External Source Files", 2 )
   ::ExtSource:Bold := .T.

   ::ExtBinary := ::Project:AddItem( "External Binary Files", 2 )
   ::ExtBinary:Bold := .T.
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD AddSource( oEditor ) CLASS FileExplorer
   LOCAL cFile := oEditor:FileName
   cFile += IIF( oEditor:Modified, " *", "" )
   oEditor:TreeItem := ::Project:AddItem( cFile, ::GetImage( oEditor:FileName ),, ::Project:Items[-3]:hItem  )
   oEditor:TreeItem:Cargo := oEditor
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD AddExtSource( oEditor ) CLASS FileExplorer
   LOCAL cFile := oEditor:FileName
   cFile += IIF( oEditor:Modified, " *", "" )
   oEditor:TreeItem := ::ExtSource:AddItem( cFile, ::GetImage( oEditor:FileName ) )
   oEditor:TreeItem:Cargo := oEditor
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD AddExtBinary( cFile ) CLASS FileExplorer
   LOCAL oFile := ProjectFile( cFile )
   oFile:TreeItem := ::ExtBinary:AddItem( cFile, ::GetImage( cFile ) )
   oFile:TreeItem:Cargo := oFile
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnSelChanged( oItem ) CLASS FileExplorer
   IF ! ::lSkipSelect .AND. oItem:Cargo != NIL
      IF oItem:Cargo:lSource
         ::Application:SourceEditor:Source := oItem:Cargo
         ::Application:Project:EditReset()
         IF ! CheckBit( GetKeyState( VK_UP ) ) .AND. ! CheckBit( GetKeyState( VK_DOWN ) )
            ::PostMessage( WM_USER + 555 )
         ENDIF
      ENDIF
   ENDIF
   ::lSkipSelect := .F.
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnRightClick() CLASS FileExplorer
   LOCAL pt := (struct POINT), oItem, oMenu, oSel, lGroup := .F.

   GetCursorPos( @pt )
   ScreenToClient( ::hWnd, @pt )
   oSel := ::HitTest( pt:x, pt:y )

   IF oSel != NIL
      oMenu := ContextStrip( Self )
      oMenu:Create()

      IF oSel == ::Project
         oItem            := MenuStripItem( oMenu )
         oItem:Text       := "&New Form"
         oItem:Action     := {|| ::Application:Project:AddWindow() }
         oItem:Enabled    := ::Application:Props[ "NewFormItemEnabled" ]
         oItem:Create()
         oItem            := MenuStripItem( oMenu )
         oItem:Text       := "&Add Existing Form"
         oItem:Create()
         oItem            := MenuStripItem( oMenu )
         oItem:BeginGroup := .T.
         oItem:Text       := "&New Custom Control"
         oItem:Enabled    := ::Application:Props[ "CustControlEnabled"]
         #ifdef VXH_ENTERPRISE
         oItem:Action     := {|o| IIF( o:Enabled, ::Application:Project:AddWindow(,,.T.),) }
         #else
         oItem:Action     := {|| MessageBox( , "Sorry, Custom Controls are available in the Enterprise edition only.", "Visual xHarbour", MB_OK | MB_ICONEXCLAMATION ) }
         #endif
         oItem:Create()

       ELSEIF oSel == ::ExtBinary
         oItem            := MenuStripItem( oMenu )
         oItem:Text       := "&Add existing file"
         oItem:Action     := {|| ::Application:Project:AddFile(,.T.) }
         oItem:Create()

       ELSEIF oSel == ::ExtSource
         oItem            := MenuStripItem( oMenu )
         oItem:Text       := "&New file"
         oItem:Action     := {|| ::Application:Project:NewSource() }
         oItem:Create()

         oItem := MenuStripItem( oMenu )
         oItem:BeginGroup := .T.
         oItem:Text       := "&Add existing file"
         oItem:Action     := {|| ::Application:Project:AddFile() }
         oItem:Create()
      ENDIF
      IF oSel:Cargo != NIL .AND. oSel:Cargo:Form == NIL

         oItem := MenuStripItem( oMenu )
         oItem:Text    := "&Remove"
         oItem:Action  := <||
                           IF oSel:Cargo:Close()
                              oSel:Delete()
                              ::Application:Project:Modified := .T.
                           ENDIF
                           RETURN NIL
                          >
         oItem:Create()
      ENDIF
      GetCursorPos( @pt )
      oMenu:Show( pt:x, pt:y )
   ENDIF
RETURN NIL
