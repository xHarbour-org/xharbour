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
RETURN

CLASS TTest
   VAR nVar
   METHOD New CONSTRUCTOR
   METHOD Increment()
ENDCLASS

METHOD New //CLASS TTest /* CLASS clause now optional */
   ::nVar := 0
RETURN Self

METHOD Increment() CLASS TTest // () No longer required
RETURN (++::nVar)
