/*
 * $Id$
 * Test Program FOr xHarbour pCode Using Deferred Functions
 * Andi Jahja
 */

REQUEST HB_GT_WIN_DEFAULT

FUNCTION MAIN()

  LOCAL H  := HB_LIBLOAD("pcode.dll")
  LOCAL H1 := HB_LIBLOAD("pcode1.dll")

  ? HB_LIBDO("MYTEST")
  ? HB_LIBDO("MYTEST", "HARBOUR POWER" )
  ? HB_LIBDO("MYNUM", 10 )

  ? HB_LIBDO("MYTEST_1")
  ? HB_LIBDO("MYTEST_1", "HARBOUR POWER" )
  ? HB_LIBDO("MYNUM_1", 10 )

  HB_LIBFREE( H )
  HB_LIBFREE( H1 )

  RETURN NIL

FUNCTION CALLINGEXE( C )

   ? __FILE__ + " " + C
   RETURN __FILE__ + ": PROCNAME(0)=" + PROCNAME(0) + " -> " + C
