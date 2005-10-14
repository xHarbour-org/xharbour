
/*
    Demo of ADS Connection handling and Data Dictionaries
*/


#include "ads.ch"
REQUEST ADS

FUNCTION MAIN
   local n
   local cErr
   local aStru := {{ "ID", "A", 1, 0}, {"Name", "C", 50, 0}, {"address", "C", 50, 0}, {"city", "C", 30, 0}, {"Age", "n", 3, 0}}
   local hConnection1, hConnection2
   local lIsDict := .f.

   CLS

   RddRegister("ads",1)
   RddSetDefault("Ads")
   AdsSetServerType ( 7 )
   SET Filetype to ADT

   ? "Default connection is 0:", adsConnection()

   fErase("xharbour.add")
   fErase("xharbour.ai")
   fErase("xharbour.am")
   fErase("Table1.adt")
   fErase("Table1.adi")
   fErase("Table2.adt")
   fErase("Table2.adi")

   // now Create a Data dictionary and the files if not exist
   IF !File("xharbour.add")
      ADSDDCREATE("xharbour.add",, "Xharbour ADS demo for data dictionary")
      // This also creates an Administrative Handle that is set as the default
      ? "Default connection is now this admin ID and true handle:", adsConnection(), adsConnectionToHandle()
      AdsDisconnect()   // disconnect current default.
                        // if you wanted to retain this connection for later, you could use
                        // hAdminCon := adsConnection(0)
                        // This get/set call would return the current connection, then set it to 0

      ? "Default connection is now this ID and true handle (zero):", adsConnection(), adsConnectionToHandle()

      // now create two free tables with same structure
      DbCreate("Table1", aStru)
      DbCreate("Table2", aStru)
      //now create an index
      USE table1 new
      INDEX ON id TAG codigo
      USE

      USE table2 new
      INDEX ON id TAG codigo
      USE
   ENDIF

   // now the magic
   IF adsConnect60("xharbour.add", 7/* All types of connection*/, "ADSSYS", "", , @hConnection1 )
      // The connection identifier to xharbour.add is now stored in hConnection1,
      // and this is now the default connection
      ? "Default connection is now this ID and true handle:", adsConnection(), adsConnectionToHandle(hConnection1, @lIsDict)
      ? "Is a Data Dict connection? (Yes, admin is also DD):", lIsDict

      // Add one user
      AdsDDCreateUser(, "Luiz", "papael", "This is user Luiz")

      ? "Add the tables"
      AdsDDaddTable("Table1", "table1.adt", "table1.adi")
      ?
      IF ! AdsDDaddTable("Customer Data", "table2.adt", "table2.adi")
         // notice the "long table name" for file Table2.adt.  Later open it with "Customer Data" as the table name
          ? "Error adding table:", adsGetLastError(@cErr), cErr
      ENDIF
   ELSE
      ? "Error connecting to xharbour.add!"
   ENDIF
   AdsDisconnect(hConnection1)
   hConnection1 := nil     // you should always reset a variable holding a handle that is no longer valid

   ? "Default connection is back to 0:", adsConnection(), adsConnectionToHandle( nil, @lIsDict)
   ? "Is a Data Dict connection? (No):", lIsDict

   // now open the tables and put some data

   IF AdsConnect60("xharbour.add", 7/* All types of connection*/, "Luiz", "papael", , @hConnection1)
      ? "Default connection is now this ID and true handle:", adsConnection(), adsConnectionToHandle(hConnection1, @lIsDict)
      ? "Is a Data Dict connection?", lIsDict

      FOR n := 1 TO  100
         IF AdsCreateSqlStatement("Data2", 3)
            AdsExecuteSqlDirect(" insert into Table1( name,address,city,age) VALUES( '" + strzero(n)+"','"+strzero(n)+"','"+strzero(n)+"'," +str(n)+ ")" )
            USE
         ENDIF
      NEXT

      FOR n := 1 TO 100
         IF AdsCreateSqlStatement("Data1", 3)
            AdsExecuteSqlDirect(" insert into " +'"Customer Data"'+"( name,address,city,age) VALUES( '"+ strzero(n)+"','"+strzero(n)+"','"+strzero(n)+"'," +str(n)+")" )
            USE
         ENDIF
      NEXT


      // AdsUseDictionary(.t.)  this function no longer is needed; the system knows if it's using a Data Dictionary connection

      // Open the "long table name" for Table2
      DbUseArea(.t.,, "Customer Data", "custom", .t., .f.)
      ? "Press a key to browse", alias()
      inkey(0)
      Browse()
      USE
      USE table1 new
      Browse()
      USE
   ENDIF

   AdsDisconnect(hConnection1)
   @ 24, 0 say ""

RETURN NIL







