GLOBAL Application
GLOBAL EXTERNAL Form1

#include "xide.ch"

//----------------------------------- Project1.prg -------------------------------------//

Procedure Main()

   Application := Application():Initialize()

   //Application:CreateFrame( 'MainFrame', TFrame() )

   Application:CreateForm( @Form1, TForm1(), Application )

   Application:Run()

Return
