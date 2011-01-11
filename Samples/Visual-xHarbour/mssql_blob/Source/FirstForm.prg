GLOBAL nHandConn:=NIL, oSQL:=NIL
GLOBAL aGrid:={{}}, bGrid:={{}}

#include "vxh.ch"
#include "FirstForm.xfm"
#include "sqlrdd.ch"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FirstForm_OnDestroy( Sender ) CLASS FirstForm
   if nHandConn <> NIL
      SR_EndConnection( nHandConn )
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolStripButton1_OnClick( Sender ) CLASS FirstForm
// Connect   
   local oError:=NIL
   local cCon:="DSN=;UID=;PWD=;DTB="
   ::Disable()
   try
      nHandConn:=SR_AddConnection( CONNECT_ODBC, cCon )
      ::Enable()
      if nHandConn > 0
         oSQL:=SR_GetConnection( nHandConn )
         ::MessageBox( "Connection OK", "" )
      else
         nHandConn:=NIL
         ::MessageBox( "Connection Failure","Error" )
      endif
   catch oError
      nHandConn:=NIL
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Connection failure" )      
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolStripButton2_OnClick( Sender ) CLASS FirstForm
// Create tables
   local i, j, nMaxT:=4, cName:="", cCode:="", cAddr:=""
   local oError:=NIL, cFile:="", uZone:=NIL
   local aStruA:={ { "CODE", "C",  6, 0 }, ;
                   { "NAME", "C", 10, 0 }, ;
                   { "ADDR", "C", 30, 0 }   ;
                 }
   local aStruB:={ { "CODE",  "C",  6, 0 }, ;
                   { "MVAL1", "M", 10, 0 }  ;
                 }

   ::Disable()

   try
      dbcreate( "TEST_A", aStruA, "SQLRDD", NIL )
      dbcreate( "TEST_B", aStruB, "SQLRDD", NIL )
   catch oError
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Creating tables" )
      dbcloseall()
      return Self
   end

   try
      dbusearea( .T., "SQLRDD", "TEST_A", "test_a", .F., .F. )
      dbusearea( .T., "SQLRDD", "TEST_B", "test_b", .F., .F. )
   catch oError
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Opening tables" )
      dbcloseall()             
      return Self
   end
   
   try           
      
      for i:=1 to nMaxT
         dbselectarea("test_a")     
         dbappend()
         cCode:=strzero(i,6)
         cName:=""
         for j:=1 to 10
            cName:=cName+chr(hb_randomint(65,90))
         next
         cAddr:=""
         for j:=1 to 30
            cAddr:=cAddr+chr(hb_randomint(65,90))
         next
         replace code with cCode, ;
                 name with cName, ;
                 addr with cAddr
         dbselectarea("test_b")     
         dbappend()
         cFile:=::Application:Path+"\u"+str(i,1)+".jpg"
         uZone:=MemoRead(cFile)
         replace code with cCode, ;
                 mval1 with uZone
      next
      
   catch oError
      
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Writing in new table" )
      dbcloseall()
      return Self
      
   end

   try
      dbselectarea("test_a")     
      ordcreate( , "CODE", "CODE", , .F. )   
      ordcreate( , "NAME", "NAME", , .F. )   
      dbselectarea("test_b")     
      ordcreate( , "CODE", "CODE", , .F. )   
   catch oError
   ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Indexing new tables" )
   end
   
   dbcloseall()
   ::Enable()
   ::MessageBox( "New table creation process terminated normally","" )
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolStripButton3_OnClick( Sender ) CLASS FirstForm
// Query tables
   local nMaxL:=1000, oError:=NIL
   local phrase:="select code, name, addr from test_a"
   
   ::Disable()
   try
      oSQL:Exec( phrase, .t., .t., aGrid, , , nMaxL )
   catch oError
      ::Enable()
      ::MessageBox( oError:SubSystem+str(oError:SubCode,7)+" "+oError:Operation+" "+oError:Description, "Query data" )
      return Self
   end
      
   ::Enable()
   if len( aGrid ) > 0
      SecondForm( ::this )
   else
      ::MessageBox( "Result table empty", "" )
   endif   
   
RETURN Self