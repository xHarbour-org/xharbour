#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   ::StatusBarPanel1:Caption:=Dtoc( Date() )
   ::StatusBarPanel2:Caption:=AmPm( Time() )
   ::Timer1:Start()
   ::oProgress:=Form2(NIL)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS Form1
   ::StatusBarPanel2:Caption:=AmPm( Time() )
   if ::oProgress <> NIL
      ::oProgress:ProgressBar1:STepIt()
   endif   
RETURN Self