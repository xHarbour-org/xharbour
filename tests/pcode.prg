/*
 * $Id$
 * Test Program FOr xHarbour pCode Using Deferred Functions
 * Andi Jahja
 * TO DO: To include Callable Dumped C Codes
 */

#pragma DEFERRED

FUNCTION MYNUM( n )

   ? __FILE__ + ": PROCNAME(0)=" + PROCNAME(0) +"()"
   ? __FILE__ + ": PARAM FROM EXE=" + LTRIM(STR(N)) + " VALTYPE=" + VALTYPE( N )
   RETURN __FILE__ + ": RETURNED FROM " + PROCNAME(0) + "() -> " + LTRIM(STR(3 * n))

FUNCTION MYTEST( c )

   IF !EMPTY( C )
      ? __FILE__ + ": PROCNAME(0)=" + PROCNAME(0) +"()"
      ? __FILE__ + " " + CALLINGEXE(__FILE__ + ": EXECUTE CALLINGEXE() FROM PCODE.DLL")
      RETURN __FILE__ + ": " + PROCNAME(0) + "() RETURNS: -> " + C
   ENDIF
   RETURN __FILE__ + ": " + PROCNAME(0) + "() RETURNS: -> " + "Hello"
