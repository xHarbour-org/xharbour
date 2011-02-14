/*
 * $Id$
 * Test Program FOr xHarbour pCode Using Deferred Functions
 * Andi Jahja
 * TO DO: To include Callable Dumped C Codes
 */

FUNCTION MYNUM_1( n )

   ? "PROCNAME(0)=" + PROCNAME(0) +"()"
   ? "PARAM FROM EXE=" + LTRIM(STR(N)) + " VALTYPE=" + VALTYPE( N )

   // CALLING FUNCTION IN OTHER DLL
   ? HB_LIBDO("MYTEST" ,"FROM PCODE1.DLL")
   ? HB_LIBDO("MYTEST", "HARBOUR POWER (FROM PCODE1.DLL)" )
   ? HB_LIBDO("MYNUM", 10 )

   RETURN "RETURNED FROM " + PROCNAME(0) +"() -> " + LTRIM(STR(3 * n))

FUNCTION MYTEST_1( c )

   IF !EMPTY( C )
      ? "PROCNAME(0)=" + PROCNAME(0) +"()"
      ? CALLINGEXE("EXECUTE CALLINGEXE() FROM DLL")
      RETURN PROCNAME(0) + "() RETURNS: -> " + C
   ENDIF
   RETURN PROCNAME(0) + "() RETURNS: -> " + "Hello"
