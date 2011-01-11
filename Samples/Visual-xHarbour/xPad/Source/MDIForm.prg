GLOBAL AppCaption, cTextFile

#include "vxh.ch"
#include "MDIForm.xfm"

#translate xCRLF => CHR(13) + CHR(10)

//---------------------------------------- End of system code ----------------------------------------//
METHOD File_New_OnClick( Sender ) CLASS MDIForm
   cTextFile:=NIL
   ChildForm( ::this )
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD File_Open_OnClick( Sender ) CLASS MDIForm
   cTextFile:=""
   ChildForm(::this)
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD File_Close_OnClick( Sender ) CLASS MDIForm
   LOCAL tmp_child
   tmp_child := ::MdiGetActive()
   if valtype( tmp_child )=="U"
      ::MessageBox( "There is no textfile opened", AppCaption, MB_ICONINFORMATION|MB_OK )
      return Self
   endif
   tmp_child:MdiClose()
   tmp_child := NIL
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD File_CloseAll_OnClick( Sender ) CLASS MDIForm
   LOCAL tmp_child

   DO WHILE (tmp_child := ::MdiGetActive()) <> NIL
      tmp_child:MdiClose()
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD File_Exit_OnClick( Sender ) CLASS MDIForm
   IF ::MessageBox( "Exit xPad?", AppCaption, MB_ICONQUESTION|MB_YESNO ) == 6
       ::Close()
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD About_Info_OnClick( Sender ) CLASS MDIForm
   ::MessageBox( "This version of xPad was developed by xHarbour.com Inc." + xCRLF + xCRLF + "The project files can be downloaded at http://www.xHarbour.com/TrainingCenter." , AppCaption + " - About")
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD About_VisitOnline_OnClick( Sender ) CLASS MDIForm
   ShellExecute( GetActiveWindow(), "open", "http://www.xHarbour.com", "", , SW_SHOW)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MDIForm_OnCreate( Sender ) CLASS MDIForm
   AppCaption:=::Application:Name + ::Application:Version
RETURN Self