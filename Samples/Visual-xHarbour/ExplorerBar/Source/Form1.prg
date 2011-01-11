#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD PictureBox1_OnClick( Sender ) CLASS Form1
   ::TestLabel:Caption:="Tables pressed"
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD PictureBox2_OnClick( Sender ) CLASS Form1
      ::TestLabel:Caption:="Columns pressed"
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form1
   ::TestLabel:Caption:="Connect pressed"   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   ::TestLabel:Caption:=""
RETURN Self