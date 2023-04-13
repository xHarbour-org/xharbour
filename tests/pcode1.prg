/*
 * $Id$
 * Test Program FOr xHarbour pCode Using Deferred Functions
 * Andi Jahja
 * TO DO: To include Callable Dumped C Codes
 */

#pragma DEFERRED

FUNCTION MYNUM_1( n )

   ? __FILE__ + ": PROCNAME(0)=" + PROCNAME(0) +"()"
   ? __FILE__ + ": PARAM FROM EXE=" + LTRIM(STR(N)) + " VALTYPE=" + VALTYPE( N )

   // CALLING FUNCTION IN OTHER DLL
   ? HB_LIBDO("MYTEST" ,"FROM PCODE1.DLL")
   ? HB_LIBDO("MYTEST", "HARBOUR POWER (FROM PCODE1.DLL)" )
   ? HB_LIBDO("MYNUM", 10 )

   RETURN __FILE__ + ": RETURNED FROM " + PROCNAME(0) +"() -> " + LTRIM(STR(3 * n))

FUNCTION MYTEST_1( c )

   IF !EMPTY( C )
      ? __FILE__ + ": PROCNAME(0)=" + PROCNAME(0) +"()"
      ? __FILE__ + " " + CALLINGEXE( "EXECUTE CALLINGEXE() FROM DLL" )
      RETURN __FILE__ + ": " + PROCNAME(0) + "() RETURNS: -> " + C
   ENDIF
   RETURN __FILE__ + ": " + PROCNAME(0) + "() RETURNS: -> " + "Hello"
