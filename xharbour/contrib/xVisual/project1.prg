GLOBAL Application
GLOBAL EXTERNAL Form1

#include "xide.ch"

//----------------------------------- Project1.prg -------------------------------------//

Procedure Main()

   Application := Application():Initialize()

   Application:CreateForm( TForm1(), @Form1 )

   Application:Run()

Return
