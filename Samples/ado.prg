PROCEDURE Main()
   LOCAL cn, rs, oErr
   
   TRY
      cn := CreateObject( "ADODB.Connection" )

      cn:ConnectionString := "DRIVER={Microsoft Access Driver (*.mdb)};DBQ=ado.mdb"

      cn:Open()

      rs := CreateObject( "ADODB.Recordset" )
   
      rs:Open( "Names", cn, 0, 1, 2 )
   
      Do While ! rs:Eof()
        Alert( rs:Fields( "First" ):Value )
        rs:MoveNext()
      ENDDO
   
      rs:Close()
      rs := Nil
   
      cn:Close()
      cn := Nil
   CATCH oErr
      Alert( "Error: " + oErr:Operation + " -> " + oErr:Description )
END
RETURN   