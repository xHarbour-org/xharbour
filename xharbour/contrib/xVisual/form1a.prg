GLOBAL Form1

#include "hbclass.ch"
#include "xide.ch"

// ! AUTO_GENERATED !
CLASS TForm1 FROM TForm

      DATA BIMAXIMIZE INIT .T.
      DATA BIMINIMIZE INIT .T.
      DATA BISYSTEMMENU INIT .T.
      DATA CAPTION INIT "TForm1"
      DATA HEIGHT INIT        400
      DATA LEFT INIT        200
      DATA NAME INIT "TFORMEDIT"
      DATA TOP INIT        125
      DATA WIDTH INIT        600

   CONTROL ListBox1 FROM TLISTBOX
      :ID :=        506

      OBJECT ITEMS IS TSTRINGS
         :TEXT := { "Option1", "Option2" }
      END OBJECT

      :LEFT :=        335
      :TOP :=         57
   END CONTROL

   CONTROL Button1 FROM TBUTTON
      :CAPTION := "Button1"
      :ID :=        500
      :LEFT :=        140
      :TOP :=         83
   END CONTROL

END CLASS 
