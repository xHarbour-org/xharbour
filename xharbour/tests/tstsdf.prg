#define CRLF chr(13)+chr(10)

REQUEST DBFCDX

Function Main

   local c, s

   RddSetDefault( "DBFCDX" )

   s := { {"C1","C", 20, 0}, {"C2","D", 8, 0}, {"C3","L", 1, 0}, {"C4","L", 1, 0}, {"C5","N", 5, 2} }
   c := "1234567890123456789020031231TF12.34continuebutdoesnotimport"+ chr(13) + chr(10)


   MemoWrit( "TSTSDF.TXT", replicate( c, 2000 ) )
   dbCreate( "TSTSDF.DBF", s )

   USE TSTSDF
   APPEND FROM TSTSDF.TXT SDF

   dbGoTop()
   browse()

return
