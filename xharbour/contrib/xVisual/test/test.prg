/*
 * $Id: test.prg,v 1.4 2002/10/18 21:02:42 fsgiudice Exp $
 */

//#include "windows.ch"
//#include "wingdi.ch"
//#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
//#include "what32.ch"
//#Include "toolbar.ch"
//#Include "winlview.ch"
//#include "wintypes.ch"
//#include "cstruct.ch"

#define CRLF CHR(13)+CHR(10)
#translate Display <x,...> => QOut( <x> ) ; view <x>

PROCEDURE Main()

  LOCAL FC
  LOCAL SC

  clear screen
  BEGIN SEQUENCE

    FC := FirstClass():New()
    SC := SecondClass():New()

    Display "Original Data from FirstClass"
    FC:Print()

    ?

    Display "Modified Data internally from FirstClass"
    FC:Modify()
    FC:Print()

    ?
    wait
    ?
    Display "Modified FirstClass Data externally"

    FC:data_published := "AA"
    //FC:data_protected := "BB"
    //FC:data_hidden    := "CC"
    FC:data_exported  := "DD"
    //FC:data_RO        := "EE"

    ?
    Display "Display FirstClass Data internally"
    FC:Print()
    ?
    wait
    Display "Display FirstClass Data externally"

    Print( FC )
    wait

    ?
    Display "Original Data from SecondClass"
    SC:Print()
    ?

    wait
    ?
    Display "Modified Data internally from SecondClass"
    SC:Modify()
    SC:Print()

    ?

    Display "Modified SecondClass Data externally"

    SC:data_published := "AA"
    //SC:data_protected := "BB"
    //SC:data_hidden    := "CC"
    SC:data_exported  := "DD"
    //SC:data_RO        := "EE"

    ?
    Display "Display FirstClass Data internally"
    SC:Print()
    ?
    wait
    Display "Display SecondClass Data externally"

    Print( SC )
    wait
    ?
  RECOVER
     ? "Scoping Error"
  END SEQUENCE
RETURN

STATIC PROCEDURE Print( oObj )
   ? DisplayData( oObj )
   /*
   ? "ClassName      = ", oObj:ClassName      ; view oObj:ClassName
   ? "data_published = ", oObj:data_published ; view oObj:data_published
   ? "data_protected = ", oObj:data_protected ; view oObj:data_protected
   ? "data_hidden    = ", oObj:data_hidden    ; view oObj:data_hidden
   ? "data_exported  = ", oObj:data_exported  ; view oObj:data_exported
   ? "data_RO        = ", oObj:data_RO        ; VIEW oObj:data_RO
   */
RETURN

FUNCTION DisplayData( oObj AS OBJECT )
   local cString := "", i, aData
   local oB := oObj

      cString += "Object Name: " + oB:ClassName + CRLF

      aData := __objGetValueList( oB )
      FOR i = 1 to len ( aData )
          cString += "DATA name: " + Pad( aData[ i, HB_OO_DATA_SYMBOL ], 25 )
          cString += " - type: " + ValType( aData[ i, HB_OO_DATA_VALUE  ] )
          cString += " - value: " + Pad( cStr( aData[ i, HB_OO_DATA_VALUE  ] ), 30 )
          cString += CRLF
      NEXT

RETURN cString
