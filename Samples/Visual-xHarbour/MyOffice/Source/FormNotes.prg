#include "vxh.ch"
#include "FormNotes.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormNotes_OnLoad( Sender ) CLASS FormNotes
   ::Application:MainForm:oNotes:=Sender
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormNotes_OnDestroy( Sender ) CLASS FormNotes
   ::Application:MainForm:oNotes:=NIL   
RETURN Self