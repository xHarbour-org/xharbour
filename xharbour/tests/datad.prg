#include "ads.ch"
REQUEST _ADS
FUNCTION MAIN  
   LOCAL n
   LOCAL aStru := {{ "ID","A",1,0},{"Name","C",50,0},{"address","C",50,0},{"city","C",30,0},{"Age","n",3,0}}


   RddRegister("ads",1)
   RddSetDefault("Ads")
   AdsSetServerType ( 7 )
   SET Filetype to ADT

   //now Create an Data dictionary and the files if not exist
   IF !File("xharbour.add")
      ADSDDCREATE("xharbour.add",,"Xharbour Ads demo data dictionary")
      // now create two table with same stucture
      DbCreate("Table1",aStru)
      DbCreate("Table2",aStru)
      //now create an index
      USE table1 new
      INDEX on id tag codigo
      USE
   
      USE table2 new
      INDEX on id tag codigo
      USE
   ENDIF

   // now the magic
   IF adsConnect60("xharbour.add",7/* All types of conection*/,"ADSSYS","",)
      // Add one user
       AdsDDCreateUser(,"Luiz","papael","This is luiz User") 
      // Add the tables
       AdsDDaddTable("Table1","table1.adt","table1.adi")
       AdsDDaddTable("Custumer Data","table2.adt","table2.adi")
   ENDIF
   AdsDisconnect(AdsGetConnectionHandle())
   // now open the tables and put some data

   IF AdsConnect60("xharbour.add",7/* All types of conection*/,"Luiz","papael",)

      FOR n := 1 TO  100
         IF AdsCreateSqlStatement("Data2",3)
            AdsExecuteSqlDirect(" insert into Table1( name,address,city,age) VALUES( '" + strzero(n)+"','"+strzero(n)+"','"+strzero(n)+"'," +str(n)+ ")" )
            USE
         ENDIF
      NEXT



      FOR n := 1 TO  100
         IF AdsCreateSqlStatement("Data1",3)
            AdsExecuteSqlDirect(" insert into " +'"Custumer Data"'+"( name,address,city,age) VALUES( '"+ strzero(n)+"','"+strzero(n)+"','"+strzero(n)+"'," +str(n)+")" )
            USE
         ENDIF                 
      NEXT
   

      AdsUseDictionary(.t.)
      DbUseAreaD(.t.,,"Custumer Data","custum",.t.,.f.)
      Browse()
      USE
      USE table1 new
      Browse()
      USE
      AdsUseDictionary(.f.)
   ENDIF

   AdsDisconnect(AdsGetConnectionHandle())
RETURN NIL
    	
    
    
     
    	

	
