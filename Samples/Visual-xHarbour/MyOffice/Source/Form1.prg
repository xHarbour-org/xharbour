GLOBAL oIni:=NIL, hModel:={ => }

#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   local i, cPrice, a
   oIni:=IniFile( ::Application:Path + "\myoffice.ini" )
   a:=oIni:GetEntries( "models" )
   for i:=1 to len( a )
      if hGetPos( hModel, a[i] ) > 0
         loop
      endif
      cPrice:=oIni:Read( "models", a[i], "" )
      hModel[ a[i] ]:=val( cPrice )
   next
   
   ::oNotes:=NIL
   ::oEmployees:=NIL
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonProjects_OnClick( Sender ) CLASS Form1
   FormProjects( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNotes_OnClick( Sender ) CLASS Form1
   if ::oNotes == NIL
      FormNotes( ::this )
   else
      ::oNotes:SetFocus()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuEmployees_OnClick( Sender ) CLASS Form1
     FormEmployees( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MenuScheduler_OnClick( Sender ) CLASS Form1
     FormScheduler( ::this )
RETURN Self