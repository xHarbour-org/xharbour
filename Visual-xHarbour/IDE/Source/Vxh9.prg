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
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init( oList ) CLASS ImageManager
   ::ImageList  := oList
   DEFAULT ::__xCtrlName  TO "ImageManager"

   ::Super:Init( ::Application:MainForm )

   ::Template   := "IMGMAN"
   ::Modal      := .T.
   ::Create()
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

   ::Button1:Dock:Right  := Self
   ::Button2:Dock:Right  := ::Button1
   
   ::Button1:Dock:Bottom := Self
   ::Button2:Dock:Bottom := Self
   
   WITH OBJECT DataGrid( Self )
      :Left                 := 5
      :Top                  := 5
      :Width                := 250
      :Height               := 300

      WITH OBJECT :ImageList := ImageList( :this )
         :IconWidth  := ::ImageList:IconWidth
         :IconHeight := ::ImageList:IconHeight
         :Images     := ACLONE( ::ImageList:Images )
         :Create()
      END
      //:ImageList            := ::ImageList

      :ItemHeight           := ::ImageList:IconHeight+2

      :Dock:Margin          := 2
      :Dock:Left            := :Parent
      :Dock:Top             := :Parent:Coolbar1
      :Dock:Right           := :Parent
//      :Dock:Bottom          := :Parent:Button1

      :OnWMSize             := {|o| IIF( !EMPTY( o:Children ), o:Children[1]:Width := o:ClientWidth, ) }
      WITH OBJECT :DataSource := MemoryTable( ::Parent )
         :Structure := { {"Resource", "C", 247 } }
         :Table     := {}

         FOR n := 1 TO LEN( ::ImageList:Images )
             AADD( :Table, { ::ImageList:Images[n][1] } )
         NEXT

         :Create()
      END

      WITH OBJECT GridColumn( :this )
         :Caption    := "Resource Name"
         :Data       := "hb_QSelf():DataSource:Fields:Resource"
         :Width      := :Parent:Parent:ClientWidth-6
         :ImageIndex := {|o| o:Record }
         :AllowSize  := .T.
         :Create()
      END

      :Create()
      :Home()
   END

   WITH OBJECT DataGrid( Self )
      :Caption        := "ImageList View"
      :Left           := 5
      :Top            := 0
      :Width          := 250
      :AutoHorzScroll := .F.
      :ShadowRow      := .F.
      :ShowHeaders    := .F.
      :ShowGrid       := .F.
      :HeaderHeight   := 0
      :BackColor      := GetSysColor(COLOR_BTNFACE)
      :Height         := ::ImageList:IconHeight + 9 + :CaptionHeight
      :ItemHeight     := ::ImageList:IconHeight + 2
      :Dock:Left      := :Parent
      :Dock:Right     := :Parent
      :Dock:Bottom    := :Parent:Button1

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
             :Table[1][n] := n//-1
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
   /*
   ::Button1:Anchor:Right := .T.
   ::Button1:Anchor:Bottom := .T.
   ::Button2:Anchor:Right := .T.
   ::Button2:Anchor:Bottom := .T.
   ::Button1:Anchor:Left := .T.
   ::Button1:Anchor:Top := .T.
   ::Button2:Anchor:Left := .T.
   ::Button2:Anchor:Top := .T.
*/
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
      IF !( oFile:Path == ::Application:Project:Properties:Resource )
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
   ::Close( IDOK )
RETURN NIL

//-------------------------------------------------------------------------------------------------------

CLASS ObjectTreeView INHERIT TreeView
   DATA aImages  EXPORTED INIT {}
   DATA CurObj   EXPORTED
   DATA lSetting EXPORTED INIT .F.
   DATA oList
   METHOD InitProject()
   METHOD Set()
   METHOD OnKeyDown()
   METHOD OnSelChanged()
   METHOD GetImage()
   //METHOD OnParentNotify()
   METHOD ResetContent() INLINE Super:ResetContent(),;
                                ::aImages := {},;
                                ::ImageList := NIL,;
                                IIF( ::oList != NIL, ::oList:Destroy(), )
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD OnKeyDown( nKey ) CLASS ObjectTreeView
   IF nKey == VK_DELETE
      ::Application:Project:CurrentForm:MaskKeyDown(, VK_DELETE )
   ENDIF
RETURN Self

/*
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS ObjectTreeView
   LOCAL tvdi, nPos, hItem, oItem
   DO CASE
      CASE hdr:code == TVN_GETDISPINFO
           hItem := TvDispInfoGethItem( nlParam )
           oItem := FindTreeItem( ::Items, hItem )
           
           TvDispInfoImage( nlParam, 6 )

           //tvdi = (struct NMTVDISPINFO*) ::Parent:lParam 
           //tvdi:item:iImage := 8
           //tvdi:CopyTo( nlParam )

   ENDCASE   
RETURN NIL
*/

//-------------------------------------------------------------------------------------------------------
METHOD InitProject() CLASS ObjectTreeView
   LOCAL o, oItem, cProject
   
   ::oList := ImageList( Self, 16, 16 ):Create()
   ::oList:MaskColor := C_LIGHTCYAN
   ::oList:AddBitmap( "TREE" )

   ::ImageList := ::oList

   cProject := ::Application:Project:Properties:Name
   IF EMPTY( cProject )
      cProject := "Untitled"
   ENDIF
   oItem := ::AddItem( cProject, 6 )
   oItem:Cargo := ::Application:Project:Properties
   ::Application:Project:Properties:TreeItem := oItem


   o := oItem:AddItem( "Application", 9 )
   o:Cargo := ::Application:Project:AppObject
   ::Application:Project:AppObject:TreeItem := o

   ::Application:MainForm:FormEditor1:TreeItem := o

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD GetImage( oObj, lChange ) CLASS ObjectTreeView
   LOCAL hIcon, n, cName := oObj:__xCtrlName

   n := ASCAN( ::aImages, {|a| a[1] == oObj:Name} )
   IF n > 0
      IF !EMPTY( oObj:__hIcon )
         ::oList:ReplaceImage( ::aImages[n][2]-1, oObj:__hIcon )
         RETURN ::aImages[n][2]

       ELSEIF oObj:ClsName == "VXH_FORM_IDE" .AND. ::oList:GetImage( ::aImages[n][2]-1 ) != NIL
         //::oList:RemoveImage( ::aImages[n][2]-1 )
         ADEL( ::aImages, ::aImages[n][2]-1 )
         RETURN -1
       
       ELSEIF lChange .AND. oObj:HasMessage( "ImageIndex" ) .AND. oObj:ImageIndex > 0
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
METHOD Set( oObj, lSelect, lChange ) CLASS ObjectTreeView
   LOCAL n, cTab
   DEFAULT lChange TO .F.
   DEFAULT lSelect TO .T.
   TRY
      IF !::lSetting .AND. oObj != NIL .AND. oObj:Caption != "[ Add New Item ]" //.AND. !oObj:__CustomOwner
         IF oObj:TreeItem == NIL
            IF !__clsParent( oObj:ClassH, "COMPONENT" )

               DEFAULT oObj:Parent TO ::Application:Project:CurrentForm
               DEFAULT oObj:Parent:TreeItem TO ::Set( oObj:Parent )

               n := ::GetImage( oObj, lChange )
               cTab := ""
               //IF oObj:HasMessage( "TabOrder" )
               //   cTab := IIF( oObj:TabOrder != NIL .AND. oObj:Style & WS_TABSTOP != 0, " #", "" )
               //ENDIF
               oObj:TreeItem := oObj:Parent:TreeItem:AddItem( oObj:Name + cTab, n )
               IF oObj:HasMessage( "Components" )
                  FOR n := 1 TO LEN( oObj:Components )
                      ::Set( oObj:Components[n], .F. )
                  NEXT
               ENDIF
               FOR n := 1 TO LEN( oObj:Children )
                   ::Set( oObj:Children[n], .T. )
               NEXT
             ELSE
               n := ::GetImage( oObj, lChange )
               oObj:TreeItem := oObj:Form:TreeItem:AddItem( oObj:Name, n )
            ENDIF
            oObj:TreeItem:Cargo := oObj
            IF oObj:Children != NIL
               FOR n := 1 TO LEN( oObj:Children )
                   ::Set( oObj:Children[n], .F. )
               NEXT
            ENDIF
          ELSE
            IF !EMPTY( oObj:Name )
               cTab := ""
               //IF oObj:HasMessage( "TabOrder" )
               //   cTab := IIF( oObj:TabOrder != NIL .AND. oObj:Style & WS_TABSTOP != 0, " #", "" )
               //ENDIF
               oObj:TreeItem:Caption := oObj:Name + cTab
            ENDIF
            oObj:TreeItem:ImageIndex := ::GetImage( oObj, lChange )
            IF lChange
               IF oObj:HasMessage( "Components" )
                  FOR n := 1 TO LEN( oObj:Components )
                      ::Set( oObj:Components[n], .F. )
                  NEXT
               ENDIF
               FOR n := 1 TO LEN( oObj:Children )
                   ::Set( oObj:Children[n], .T. )
               NEXT
            ENDIF
         ENDIF
         IF lSelect
            oObj:TreeItem:Select()
         ENDIF
      ENDIF
   CATCH
   END
RETURN oObj:TreeItem

//-------------------------------------------------------------------------------------------------------
METHOD OnSelChanged( oItem ) CLASS ObjectTreeView
   IF EMPTY( procname(10) )
      ::lSetting := .T.
      
      TRY
         IF ::PreviousItem:Cargo:Form:hWnd != oItem:Cargo:Form:hWnd
            ::Application:Project:SelectWindow( oItem:Cargo:Form, ::hWnd )
         ENDIF
         
         IF oItem:Cargo:Parent:__xCtrlName == "TabPage"
            oItem:Cargo:Parent:Select()
         ENDIF
         IF oItem:Cargo:__xCtrlName == "TabPage"
            oItem:Cargo:Select()
         ENDIF

      CATCH
      END
      TRY
         ::Application:Project:CurrentForm:SelectControl( oItem:Cargo, .F. )
      CATCH
         ::Application:ObjectManager:ResetProperties( {{oItem:Cargo}} )
         ::Application:EventManager:ResetEvents( {{oItem:Cargo}} )
      END
      ::lSetting := .F.
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
CLASS FileTreeView INHERIT TreeView
   DATA Changing  EXPORTED INIT .F.
   METHOD UpdateView()
   METHOD OnSelChanged()
   METHOD OnDropFiles()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD OnDropFiles() CLASS FileTreeView
   LOCAL cFile
   FOR EACH cFile IN ::FilesDroped
       IF RIGHT( lower( cFile ), 4 ) $ ".lib.obj"
          ::Application:Project:AddBinary( cFile )
        ELSE
          ::Application:Project:AddSource( cFile )
      ENDIF
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD UpdateView() CLASS FileTreeView
   LOCAL Form, MainItem, SubItem, Item, cFile := ::Application:Project:Properties:Name
   DEFAULT cFile TO "Untitled"
   ::ResetContent()

   MainItem := ::AddItem( cFile+".vxh", 6 )
   MainItem:Bold := .T.
   MainItem:AddItem( cFile+"_Main.prg", 16 )
   FOR EACH Form IN ::Application:Project:Forms
       SubItem := MainItem:AddItem( Form:Name + ".prg", 16 )
       SubItem:Cargo := Form:Editor
       //SubItem:Action := {|o| o:Parent:SelectControl( o:Cargo ) }
   NEXT
   IF !EMPTY( ::Application:Project:Properties:Sources )
      Item := MainItem:AddItem( "External Source Files", 20 )
      Item:Bold := .T.
      FOR EACH cFile IN ::Application:Project:Properties:Sources
          SubItem := Item:AddItem( cFile, 20 )
          //SubItem:Cargo := Form:Editor
          //Item:Action := {|o| o:Parent:SelectControl( o:Cargo ) }
      NEXT
   ENDIF
   IF !EMPTY( ::Application:Project:Properties:Binaries )
      Item := MainItem:AddItem( "External Binary Files", 19 )
      Item:Bold := .T.
      FOR EACH cFile IN ::Application:Project:Properties:Binaries
          SubItem := Item:AddItem( cFile, 19 )
          //SubItem:Cargo := Form:Editor
          //Item:Action := {|o| o:Parent:SelectControl( o:Cargo ) }
      NEXT
   ENDIF
//   ::ExpandAll()
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OnSelChanged( oItem ) CLASS FileTreeView
   ::Application:Project:cFileRemove   := NIL
   ::Application:Project:cSourceRemove := NIL
   ::Application:RemoveSourceMenu:Disable()
   IF oItem:Owner != NIL
      IF oItem:Owner:Caption == "External Binary Files" 
         ::Application:Project:cFileRemove := oItem:Caption
         ::Application:RemoveSourceMenu:Enable()
       ELSEIF oItem:Owner:Caption == "External Source Files" 
         ::Application:Project:cSourceRemove := oItem:Caption
         ::Application:RemoveSourceMenu:Enable()
      ENDIF
   ENDIF   
RETURN Self
