
PROCEDURE CalendarTest( oWnd )
   local oDlg
   local oCal, dVar
   
   DEFINE DIALOG oDlg ;
          MODAL ;
          AT 30, 30 SIZE 210, 150 ;
          TITLE "Example Dialog with Edit Test" ;
          OF oWnd

   SET CURRENT WINDOW oDlg
   
   @   10,10 CALENDAR oCal VAR dVar ;
             CAPTION "Default value" ;
             TOOLTIP "Calendar control" ;
             STATUS "Please select a date from calendar control" ;
             COLOR "rb"
             
   @  10,160 PUSHBUTTON "&Get values" ;
             TOOLTIP "Show control values";
             ACTION ShowControlValues( oDlg ) ;
             STATUS "Display all controls with values"

   @ 120,160 PUSHBUTTON "&Values" ;
             TOOLTIP "Values" ;
             ACTION oDlg:MessageBox( "ValType of Calendar control = "+ ValType( dVar ) + CRLF +;
                                     "Value Of Calendar Control = " + cStr( dVar ) )

   @ 120, 10 PUSHBUTTON "&Close" ;
             DEFAULT ;
             TOOLTIP "Close Dialog Window" ;
             STATUS "Close this dialog window" ;
             ID IDOK
             
   @ 120, 60 PUSHBUTTON "C&ancel" ;
             TOOLTIP "Cancel Dialog Window" ;
             STATUS "Cancel this dialog window" ;
             ID IDCANCEL 


   DIALOG oDlg CREATE

RETURN
