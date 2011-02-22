/*
 * $Id$
 * Test Program FOr xHarbour pCode Using Deferred Functions
 * Andi Jahja
 * TO DO: To include Callable Dumped C Codes
 */

#pragma DEFERRED

FUNCTION MYNUM( n )

   ? "PROCNAME(0)=" + PROCNAME(0) +"()"
   ? "PARAM FROM EXE=" + LTRIM(STR(N)) + " VALTYPE=" + VALTYPE( N )
   RETURN "RETURNED FROM " + PROCNAME(0) + "() -> " + LTRIM(STR(3 * n))

FUNCTION MYTEST( c )

   IF !EMPTY( C )
      ? "PROCNAME(0)=" + PROCNAME(0) +"()"
      ? CALLINGEXE("EXECUTE CALLINGEXE() FROM PCODE.DLL")
      RETURN PROCNAME(0) + "() RETURNS: -> " + C
   ENDIF
   RETURN PROCNAME(0) + "() RETURNS: -> " + "Hello"
