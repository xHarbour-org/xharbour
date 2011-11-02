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

CLASS ResourceManager INHERIT Dialog
   DATA ItemManager      EXPORTED
   DATA ItemEventManager EXPORTED
   DATA lChanged         EXPORTED INIT .F.
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnInitDialog()
   METHOD AddResource()
   METHOD DeleteResource()
   METHOD OnOk()
   METHOD OnCancel()
   METHOD Save()
ENDCLASS

//------------------------------------------------------------------------------------------

METHOD Init() CLASS ResourceManager
   DEFAULT ::__xCtrlName  TO "ResourceManager"
   ::Super:Init( ::Application:MainForm )
   ::Create()
RETURN Self

METHOD Create() CLASS ResourceManager
   ::Caption := "Resource Manager"
   ::Left    := 0
   ::Top     := 0
   ::Width   := 500
   ::Height  := 400
   ::Modal   := .T.
   ::xName   := "ResourceManager"
   ::DlgModalFrame := .T.
   Super:Create()
RETURN Self

//------------------------------------------------------------------------------------------

METHOD OnInitDialog() CLASS ResourceManager
   LOCAL n, cResName, cType, aFile
   ::CenterWindow( .T. )
   ::RestoreLayout(, "WindowPosition")
   WITH OBJECT ToolStrip( Self )
      :ImageList := ImageList( :this, 16, 16 ):Create()
      :ImageList:AddImage( IDB_STD_SMALL_COLOR )
      :Create()

      WITH OBJECT ToolStripButton( :this )
         :ImageIndex   := ::System:StdIcons:FileNew
         :ToolTip:Text := "New Resource"
         :Action       := {|o| o:Parent:Parent:AddResource()}
         :Create()
      END

      WITH OBJECT ToolStripButton( :this )
         :ImageIndex   := ::System:StdIcons:Delete
         :ToolTip:Text := "Delete Resource"
         :Action       := {|o| o:Parent:Parent:DeleteResource()}
         :Create()
      END

   END
   
   WITH OBJECT Button( Self )
      :Caption     := "Cancel"
      :Dock:Right  := Self
      :Dock:Bottom := Self
      :ID          := IDCANCEL
      :Create()
   END

   WITH OBJECT Button( Self )
      :Caption     := "OK"
      :Dock:Right  := ::Button1
      :Dock:Bottom := Self
      :ID          := IDOK
      :Create()
   END
   
   WITH OBJECT DataGrid( Self )
      :xName        := "DataGrid"
      :Left         := 5
      :Top          := 5
      :Width        := 250
      :Height       := 300
      :FullRowSelect:= .T.
      :Dock:Margin  := 2
      :Dock:Left    := :Parent
      :Dock:Top     := :Parent:ToolStrip1
      :Dock:Right   := :Parent
      :Dock:Bottom  := :Parent:Button1

      :OnWMSize     := {|o| IIF( !EMPTY( o:Children ), o:Children[3]:Width := MAX( 1000, o:ClientWidth-150 ), ) }

      WITH OBJECT :DataSource := MemoryTable( ::Parent )
         :Structure := { {"ResourceName", "C", 100 },;
                         {"ResourceType", "C", 50 },;
                         {"ResourceFile", "C", 500 } }
         :Table     := {}
         :Create()

         FOR n := 1 TO LEN( ::Application:Project:Properties:Resources )

             aFile := SplitFile( ::Application:Project:Properties:Resources[n] )

             cResName := STRTRAN( aFile[2], " " )
             cResName := "_"+UPPER(STRTRAN( cResName, "." ))

             cType := UPPER( SUBSTR( aFile[2], RAT( ".", aFile[2] )+1 ) )
             IF cType == "ICO"
                cType := "ICON"
              ELSEIF cType == "BMP"
                cType := "BITMAP"
             ENDIF
             :Append()
             :Fields:ResourceName := cResName
             :Fields:ResourceType := cType
             :Fields:ResourceFile := aFile[1] + "\" + aFile[2]

             //AADD( :Table, { ::Application:Project:Properties:Resources[n] } )
         NEXT

      END

      WITH OBJECT GridColumn( :this )
         :Name       := "ResourceName"
         :Caption    := "Name"
         :Data       := "hb_QSelf():DataSource:Fields:ResourceName"
         :Width      := 100
         :AllowSize  := .T.
         :BackColor  := {|o| IIF( !o:DataSource:Eof() .AND. !FILE( o:DataSource:Fields:ResourceFile ), ::System:Color:Red, NIL ) }
         :Create()
      END
      WITH OBJECT GridColumn( :this )
         :Name       := "ResourceType"
         :Caption    := "Type"
         :Data       := "hb_QSelf():DataSource:Fields:ResourceType"
         :Width      := 50
         :AllowSize  := .T.
         :BackColor  := {|o| IIF( !o:DataSource:Eof() .AND. !FILE( o:DataSource:Fields:ResourceFile ), ::System:Color:Red, NIL ) }
         :Create()
      END
      WITH OBJECT GridColumn( :this )
         :Name       := "ResourceFile"
         :Caption    := "Resource File"
         :Data       := "hb_QSelf():DataSource:Fields:ResourceFile"
         :Width      := MAX( 1000, :Parent:ClientWidth-150 )
         :AllowSize  := .T.
         :BackColor  := {|o| IIF( !o:DataSource:Eof() .AND. !FILE( o:DataSource:Fields:ResourceFile ), ::System:Color:Red, NIL ) }
         :Create()
      END
      :Create()
      :RestoreLayout( , "WindowPosition" )
      :Home()
   END

   ::ToolStripButton2:Enabled := ::DataGrid1:DataSource:RecCount() > 0
RETURN NIL

METHOD AddResource() CLASS ResourceManager
   LOCAL cFile, lCopy, cResName, cType, oFile := CFile( "" )

   oFile:AllowMultiSelect := .T.
   oFile:OpenDialog()
   IF oFile:Result != IDCANCEL .AND. oFile:Path != NIL .AND. oFile:Name != NIL

      lCopy := .F.
      IF !( oFile:Path == ::Application:Project:Properties:Resource )
         IF ::MessageBox( "Selected files are not in the project path, would you like to copy them to the resource folder?", "Resources", MB_ICONEXCLAMATION | MB_YESNO ) == IDYES
            ::Application:Project:Save()
            lCopy := .T.
         ENDIF
      ENDIF
      
      ::DataGrid1:DataSource:Tag := "ResourceFile"
      
      WITH OBJECT ::DataGrid1
         FOR EACH cFile IN oFile:Name
             IF lCopy
                CopyFile( oFile:Path + "\" + cFile, ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Resource + "\" + cFile )
                oFile:Path := ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Resource
             ENDIF
             WITH OBJECT ::DataGrid1:DataSource
                IF ! :Seek( oFile:Path + "\" + cFile )
                   cResName := STRTRAN( cFile, " " )
                   cResName := "_"+UPPER(STRTRAN( cResName, "." ))

                   cType := UPPER( SUBSTR( cFile, RAT( ".", cFile )+1 ) )
                   IF cType == "ICO"
                      cType := "ICON"
                    ELSEIF cType == "BMP"
                      cType := "BITMAP"
                   ENDIF
                   :Append()
                   :Fields:ResourceName := cResName
                   :Fields:ResourceType := cType
                   :Fields:ResourceFile := oFile:Path + "\" + cFile
                   ::lChanged := .T.
                ENDIF
             END
         NEXT
         :DataSource:GoBottom()
         :Update()
      END

      ::ToolStripButton2:Enabled := ::DataGrid1:DataSource:RecCount() > 0

   ENDIF
RETURN Self

METHOD DeleteResource() CLASS ResourceManager
   ::DataGrid1:DataSource:Delete()
   ::DataGrid1:Update()

   ::ToolStripButton2:Enabled := ::DataGrid1:DataSource:RecCount() > 0
   ::lChanged := .T.
RETURN Self

METHOD OnOk() CLASS ResourceManager
   IF ::lChanged
      ::Save()
   ENDIF
   ::SaveLayout(,"WindowPosition",.T.)
   ::DataGrid1:SaveLayout(,"WindowPosition")
   ::Close( IDOK )
RETURN NIL

METHOD Save() CLASS ResourceManager
   ::DataGrid1:DataSource:GoTop()
   ::Application:Project:Properties:Resources := {}
   WHILE !::DataGrid1:DataSource:Eof()
      AADD( ::Application:Project:Properties:Resources, ::DataGrid1:DataSource:Fields:ResourceFile )
      ::DataGrid1:DataSource:Skip()
   END
   ::Application:Project:Modified := .T.
RETURN NIL

METHOD OnCancel() CLASS ResourceManager
   LOCAL n
   ::SaveLayout(,"WindowPosition",.T.)
   ::DataGrid1:SaveLayout(,"WindowPosition")
   IF ::lChanged
      n := ::MessageBox( "Save changes resources?", "Resource Manager", MB_ICONEXCLAMATION | MB_YESNOCANCEL )
      IF n == IDYES
         ::Save()
       ELSEIF n == IDCANCEL
         RETURN 0
      ENDIF
   ENDIF
RETURN NIL
  
//-------------------------------------------------------------------------------------------------------

