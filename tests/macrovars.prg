************************************************************
* macrovars.prg
* $Id$
*
* Test of SET MACRO BLOCK VARS [ON|OFF|XHARBOUR|CLIPPER]
* This SET set the behavior of codeblock to share the PRIVATE memvars level
* with caller in creation time of codeblock.
*
* (C) Walter Negro
*

Function Main()
Local cBlock := "{|| M->MYVAR := 1}"
Local bBlockOn, bBlockOff, bBlockOther, bBlockStatic
Local n := 1

// Macro-compilation with SET MACRO BLOCK VARS OFF, Clipper compatible
//SET MACRO BLOCK VARS OFF  // Default OFF
bBlockOff := &cBlock

// Macro-compilation with SET MACRO BLOCK VARS ON, xHarbour extension
SET MACRO BLOCK VARS ON  // or SET MACRO BLOCK VARS XHARBOUR
bBlockOn := &cBlock

Eval(bBlockOff)
TRY
   ? M->MYVAR
   ? "Error, M->MyVar should not be exist."
CATCH
   ? "Ok, M->MyVar does not exist."
END

Eval(bBlockOn)
TRY
   ? M->MYVAR
CATCH
   ? "Error, M->MyVar should be exist."
END

// Macro-compilation with SET MACRO BLOCK VARS ON and setting OFF for this codeblock.
bBlockOther := &("{|| M->OTHERVAR := 1 }")
Set( _SET_MACROBLOCKVARS, .F., bBlockOther )
Eval(bBlockOther)

TRY
   ? M->OTHERVAR
CATCH
   ? "Ok, M->OtherVar does not exist."
END

// Can not be change the SET in static codeblock.
bBlockStatic := {|| M->FROMSTATIC := 1 }

TRY
   Set( _SET_MACROBLOCKVARS, .T., bBlockStatic )
   ? "Error, is not possible use _SET_MACROBLOCKVARS with static codeblock"
CATCH
   ? "Ok, is not possible use _SET_MACROBLOCKVARS with static codeblock"
END
Eval(bBlockStatic)

TRY
   ? M->FROMSTATIC
CATCH
   ? "Ok, M->FromStatic does not exist."
END
