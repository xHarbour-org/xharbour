PROCEDURE Main()

   LOCAL cConStr := "DBQ=grocertogo.mdb;Driver={Microsoft Access Driver (*.mdb)}"
   LOCAL dsNames := TODBC():New( cConStr )

   set COLOR TO "W+/B"
   CLS

   @ 00, 00 SAY padc( "þ TODBC Demonstration þ", 80 ) COLOR "B/W"

   dsNames:SetSQL( "SELECT * FROM Products" )
   dsNames:Open()

   BrowseODBC( 2, 2, 22, 78, dsNames )

   dsNames:Close()

   dsNames:Destroy()

RETURN
