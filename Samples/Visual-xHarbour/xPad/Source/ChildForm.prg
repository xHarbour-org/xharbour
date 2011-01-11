GLOBAL EXTERNAL AppCaption, cTextFile

#include "vxh.ch"
#include "ChildForm.xfm"

//---------------------------------------- End of system code ----------------------------------------//
METHOD MainBar_MainBar_Open_OnClick( Sender ) CLASS ChildForm   
   local cFile
   with object ::Application:MainForm:MyOpen
      :Title:="Open Text File..."
      :FileName:=""
      :DefaultExt:="txt"
      :Filter:="Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
      :Show()
      cFile:=:FileName 
   end

   IF empty( cFile )
      RETURN NIL
   ELSE
      ::FileContent:Caption := MEMOREAD( cFile )
      ::Caption := cFile
      ::RedrawWindow()
   ENDIF   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MainBar_MainBar_Close_OnClick( Sender ) CLASS ChildForm
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MainBar_MainBar_Save_OnClick( Sender ) CLASS ChildForm
   LOCAL cFile   

   with object ::Application:MainForm:MySave
      :Title:="Save Text File..."
      :FileName:=::Caption
      :DefaultExt:="txt"
      :Filter:="Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
      :Show()
      cFile:=:FileName
   end
   
   IF empty( cFile )
      RETURN Self
   ELSE
      MEMOWRIT( cFile, ::FileContent:Caption )
      ::Caption := cFile
      ::doc_initial_save := 1
      ::doc_saved := 1
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MainBar_MainBar_SaveAs_OnClick( Sender ) CLASS ChildForm
   LOCAL cFile

   IF ::doc_initial_save = 0
      with object ::Application:MainForm:MySave
         :Title:="Save Text File As..."
         :FileName:=""
         :DefaultExt:="txt"
         :Filter:="Text Files (*.txt)|*.txt|All Files (*.*)|*.*"
         :Show()
         cFile:=:FileName
      end
      
      IF empty( cFile )
         RETURN Self
      ELSE
         MEMOWRIT( cFile, ::FileContent:Caption )
         ::Caption := cFile
         ::doc_initial_save := 1
         ::doc_saved := 1
      END
   ELSE
      IF MEMOWRIT(::Caption, ::FileContent:Caption) == .F.
         ::MessageBox( "An error occured while trying to save the document.", AppCaption )
      else
         ::doc_saved:=1
      END
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MainBar_MainBar_Font_OnClick( Sender ) CLASS ChildForm
   ::MessageBox( "Fonts not available in current version.", ::Application:Name+" "+::Application:Version)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FileContent_OnChar( Sender ) CLASS ChildForm
   ::doc_saved := 0
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form_OnClose( Sender ) CLASS ChildForm
   IF ::doc_saved == 0
      IF ::MessageBox( "Close this unsaved document?", AppCaption, MB_ICONQUESTION|MB_YESNO ) <> 6
         RETURN .F.
      END
   END
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ChildForm_OnLoad( Sender ) CLASS ChildForm
   ::doc_saved:=0
   ::doc_initial_save:=0
   if cTextFile <> NIL
      if Sender:MainBar_MainBar_Open_OnClick( Sender ) == NIL
         ::doc_saved:=1
         ::doc_initial_save:=1
         ::Close()
      endif
   endif
RETURN Self