/*
 * $Id$
 */
/*
   test9.prg
   testing date settings
   Options:
      "AMERICAN"
      "ANSI"
      "BRITISH"
      "FRENCH"
      "GERMAN"
      "ITALIAN"
      "SPANISH"
      "MM/DD/YY"
      "YY.MM.DD"
      "DD/MM/YY"
      "DD.MM.YY"
      "DD-MM-YY"
      "MM/DD/YYYY"
      "YYYY.MM.DD"
      "DD/MM/YYYY"
      "DD.MM.YYYY"
      "DD-MM-YYYY"
*/
#include "sixapi.ch"

#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
 
PROCEDURE MAIN

LOCAL e

SET CENTURY ON
? date()

SET DATE AMERICAN
? date()

SET DATE BRITISH
? date()

? "Before :",sx_SetDateFormat("AMERICAN"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("ANSI"       ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("BRITISH"    ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("FRENCH"     ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("GERMAN"     ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("ITALIAN"    ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("SPANISH"    ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("MM/DD/YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("YY.MM.DD"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD/MM/YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD.MM.YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD-MM-YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("MM/DD/YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("YYYY.MM.DD" ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD/MM/YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD.MM.YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
? "Before :",sx_SetDateFormat("DD-MM-YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =",date()
TRY
? sx_SetDateFormat("YYYY-MM-DD"), date()
CATCH e
? "This is Invalid =>", e:SubSystem, padl(e:SubCode,4), e:Operation, e:Description, ValToPrg( e:Args )
END

TRY
? sx_SetDateFormat("AFRIKAANS"), date()
CATCH e
? "This is Invalid =>", e:SubSystem, padl(e:SubCode,4), e:Operation, e:Description, ValToPrg( e:Args )
END
