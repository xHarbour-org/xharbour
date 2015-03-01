request DBFCDX,DBFFPT,DBFNTX
#define RECORDS_IN_TEST                   1500
func main
Local xHora,xTimes,xdata
Local nMonth
   local aStruct := {{"CODE_ID","C",8,0 },;
                     {"CARDID","C",1,0},;
                     {"DESCR","C",50,0},;
                     {"PERCENT","N",10,2},;
                     {"DAYS","N",8,0},;
                     {"DATE_LIM","D",8,0},;
                     {"DATET_LIM","T",8,0},;
                     {"HORA","T",4,0},;
                     {"ENABLE","L",1,0},;
                     {"OBS","M",10,0},;
                     {"TIMES","@",8,0},;
                     {"VALUE","N",18,6}}

rddsetdefault("DBFCDX")
if !file("testtipo.dbf")
dbcreate("testtipo.dbf",aStruct,"DBFCDX")
endif
use testtipo.dbf new  alias tst

   For i = 1 to RECORDS_IN_TEST
      Append Blank
      Replace CODE_ID  with strZero( i     , 8 )
      Replace DESCR    with dtoc( date() ) + " - " + time()
      Replace DAYS     with (RECORDS_IN_TEST - i )
      Replace DATE_LIM with date()
      replace DATET_LIM with datetime()+i
      Replace ENABLE   with .T.
      replace percent  with i/RECORDS_IN_TEST 
      Replace OBS      with "This is a memo field. Seconds since midnight : " + alltrim(str(seconds()))
      
      replace times with  datetime()-i
      if i%2==0
         nMonth := HB_RANDOMINT(1,12)
         if nMonth == 2
            replace hora with  datetime(HB_RANDOMINT(0,23),nMonth,HB_RANDOMINT(1,28))
         elseif nMonth == 2 .or. nMonth == 4 .or. nMonth == 6 .or. nMonth == 9 .or. nMonth == 11 
            replace hora with  datetime(HB_RANDOMINT(0,23),nMonth,HB_RANDOMINT(1,30))
         else
            replace hora with  datetime(HB_RANDOMINT(0,23),nMonth,HB_RANDOMINT(1,31))
         endif
         
      elseif i%3 ==0
         replace hora with datetime(,,,HB_RANDOMINT(0,23),HB_RANDOMINT(0,59),HB_RANDOMINT(0,59),)   
      else
         replace hora with  datetime()-(i*3)
      endif
   Next

   go top

   browse()
   use

return nil                     