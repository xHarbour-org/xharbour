//#include "rp_run.ch"

#ifdef __HARBOUR__
   #include "hbclass.ch"
#else
   #include "clpclass.ch"
#endif

PROCEDURE Main()
    LOCAL o := TTest():New()

    o:Increment()
    Alert( o:nVar )
Return NIL

CLASS TTest
   VAR nVar
   METHOD New CONSTRUCTOR
   METHOD Increment()
ENDCLASS

METHOD New //CLASS TTest /* CLASS clause now optional */
   ::nVar := 0
return Self

METHOD Increment() CLASS TTest // () No longer required
Return (++::nVar)