*  $Id$
*
*  This file is a demo for text mode screen designer
*
*  How to use:
*  1. Enter a file name to edit/create
*  2. Use combination of:
*     CTRL_RIGHT, CTRL_LEFT, CTRL_UP, CTRL_DOWN
*     ALT_RIGHT, ALT_LEFT, ALT_UP, ALT_DOWN
*     RIGHT, LEFT, UP, DOWN
*     To resize/move the dialog box
*  3. Press F1 to POP-UP object selection
*  4. Select an object
*  5. Use arrow keys to move the object within dialog
*  6. Press ENTER when finished moving the object
*  7. Press F6 to save file
*
*  Requires design.lib to link
*
*  Andi Jahja
*

#include "inkey.ch"
#ifndef __CLIPPER__
request HB_NOMOUSE
#endif
********************************************************************************
PROCEDURE MAIN()
********************************************************************************

   LOCAL scr     := scrpack()
   LOCAL cfile
   LOCAL fl      := space(50)
   LOCAL ncursor := setcursor(0)
   LOCAL odlg    := window():new(09,14,15,65,,,,,,.f.)

   IF empty( cfile := getfname( "File Name",,.F., @Fl,.F.,NIL,)[1] )
      odlg:display:edit:hide()
   ELSE
      odlg:display( cfile ):edit:hide()
   ENDIF
   IF lastkey() <> 27
      odlg:write(3)
   ENDIF
   setcursor( ncursor )
   scrunpack(scr)

   return
