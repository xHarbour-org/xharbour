GLOBAL Form1

#include "hbclass.ch"
#include "xide.ch"

// ! AUTO_GENERATED !
CLASS TForm1 FROM TForm

      VAR BIMAXIMIZE INIT .T.
      VAR BIMINIMIZE INIT .T.
      VAR BISYSTEMMENU INIT .T.
      VAR CAPTION INIT "TForm1"
      VAR HEIGHT INIT        400
      VAR LEFT INIT        200
      VAR NAME INIT "TFORMEDIT"
      VAR TOP INIT        125
      VAR WIDTH INIT        600

   CONTROL ListBox1 FROM TLISTBOX
      :ID :=        506

      OBJECT ITEMS IS TSTRINGS
         :TEXT := {}
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
