GLOBAL Form1

#include "hbclass.ch"
#include "xide.ch"

// ! AUTO_GENERATED !
CLASS TForm1 FROM TForm

   DATA CAPTION INIT "MyForm1"
   DATA HEIGHT INIT        400
   DATA LEFT INIT        200
   DATA STYLE INIT   13565952
   DATA TOP INIT        125
   DATA WIDTH INIT        500

   CONTROL Button1 FROM TBUTTON
      :CAPTION := "MyButton"
      :HEIGHT :=         24
      :ID :=        500
      :LEFT :=        163
      :STYLE :=  1342242816
      :TOP :=         96
      :WIDTH :=         80

      :OnClick := { |Button1| Button1:Parent:Button1Click( HB_QSelf() ) }
   END CONTROL

   // Generated only for On... Events that were specied in the Designer.
   DATA OnClick INIT {|Self| TraceLog(), Self:Click() }

   METHOD Click()

   // Methods...
   METHOD Button1Click( Sender )
   //...

END CLASS 

METHOD Click() CLASS TForm1

    /*
       Code specified by developer in IDE for overridden event.
       May refer to the form as either Self, ::, or Form1.
       Self or :: are faster.
    */
    MessageBox( 0, ::Caption + " was clicked.", "Project1", 0 )

Return Self

// Controls:
METHOD Button1Click( Sender /* Button1 class TButton() */ ) CLASS TForm1

   /*
      Creating a fast reference to the initiating control.
      This way we can use : for the child control initating
      the Event (we also have :: for the container Form).
   */
   WITH OBJECT Sender

   // Self is the container not the initiating control so:

   // Using the standard WITH shortcut.
   :Caption := "New Caption"          // Fastest.
   // Or:
   Sender:Caption := "New Caption"    // Still very fast.
   // Or:
   Self:Button1:Caption := "New Caption" // Slower, but might be needed when referring other controls on the form.

   // Now self:
   ::Caption := "My Form"     // Faster.

   END WITH // Sender

Return Self
