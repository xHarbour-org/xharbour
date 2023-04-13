*************************************************************************************
* Clipboard test
*
* $Id$
*
* Test for clipboard functions:
* hb_gtInfo( GTI_CLIPBOARDDATA, <x> ) - set the clipboard value
* hb_gtInfo( GTI_CLIPBOARDDATA ) - get the current clipboard value
* GTPasteClipboard - paste contents of the clipboard into the inkey buffer.
*
* This functions are available in ALL the gts; where it is possible, the
* OS clipboard capabilities are used.
*
* (C) 2004 Giancarlo Niccolai
*

#include "inkey.ch"
#include "hbgtinfo.ch"

PROCEDURE Main()
   LOCAL GetList := {}
   PUBLIC cData1, cData2, cData3, cData4

   SET COLOR TO w+/b
   CLEAR SCREEN
   @1,0 SAY Padc( "X H A R B O U R - Clipboard test", MaxCol() )
   @3,0 SAY Padc( ;
      "Use fields to copy and paste tests. If available, OS clipboard will be used.", MaxCol() )
   @4,5 SAY "CTRL + INS  = COPY the contents of the current field"
   @5,5 SAY "CTRL + DEL = CUT the contents of the current field"
   @6,5 SAY "CTRL + ENTER = Insert the clipboard in the field"
   @7,5 SAY "CTRL + B = Change the contents of the field with the clipboard"

   m->cData1 := space(50)
   m->cData2 := space(50)
   m->cData3 := space(50)
   m->cData4 := space(50)

   SetKey( K_CTRL_INS, { |cProc, nLine, cData | DoCopy(cProc, nLine, cData, &cData)} )
   SetKey( K_CTRL_DEL, { |cProc, nLine, cData | DoCut(cProc, nLine, cData, &cData)} )
   SetKey( K_CTRL_B, { |cProc, nLine, cData | DoPaste(cProc, nLine, cData, &cData)} )
   SetKey( K_CTRL_ENTER, { |cProc, nLine, cData | DoFill(cProc, nLine, cData, &cData)} )

   DO WHILE LastKey() != K_ESC
      @10,5 SAY "Field 1: " GET m->cData1
      @12,5 SAY "Field 2: " GET m->cData2
      @14,5 SAY "Field 3: " GET m->cData3
      @16,5 SAY "Field 4: " GET m->cData4
      READ
   ENDDO

RETURN

PROCEDURE DoCopy(cProc, nLine, cData, cValue)
   hb_gtInfo( GTI_CLIPBOARDDATA, Alltrim( cValue ) )
   @20,5 SAY "Clipboard: " + hb_gtInfo( GTI_CLIPBOARDDATA ) + Space(50)
RETURN

PROCEDURE DoCut(cProc, nLine, cData, cValue)
   hb_gtInfo( GTI_CLIPBOARDDATA, Alltrim( cValue ) )
   // Cdata is a public memvar, so it should work
   &cData := Space( Len( cValue ) )
   @20,5 SAY "Clipboard: " + hb_gtInfo( GTI_CLIPBOARDDATA ) + Space(50)
RETURN

PROCEDURE DoPaste(cProc, nLine, cData, cValue)
   &cData := PadR( hb_gtInfo( GTI_CLIPBOARDDATA ), Len( cValue ) )
RETURN

PROCEDURE DoFill()
   hb_gtInfo( GTI_CLIPBOARDPAST )
RETURN
