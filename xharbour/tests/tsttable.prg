#include 'ttable.ch'

request Dbfcdx
FUNCTION main

   LOCAL x      := 1
   LOCAL oTable
   
   RDDSETDEFAULT( 'dbfcdx')

   doData()
   /*Open An Table With Table Class */
   DEFINE TABLE oTable file test new
   
   /*Adding an Index to This Table */
      Define order on key "nome" tag _1 in oTable
   /* Force the index Creating*/
      oTable:reindex()

   WHILE x <= 100

      oTable:readblank()
      oTable:nome     := STR( x, 20 )
      oTable:cidade   := STR( x + 1, 20 )
      oTable:endereco := STR( x + 2, 20 )
      oTable:codigo   := x
      otable:Append()
      oTable:write()

      x ++

   ENDDO

   USE
RETURN nil

FUNCTION dodata()

   LOCAL oTable

   IF !FILE( 'test.dbf' )
   /* Create An Table using Table Classe Syntax */
      Create database otable FILE test.dbf
      FIELD name nome type Character len 40 Dec 0 of oTable
      FIELD name endereco type Character len 40 Dec 0 of oTable
      FIELD name cidade type Character len 40 Dec 0 of oTable
      FIELD name codigo type Numeric len 5 Dec 0 of oTable
      build table o
   ENDIF
RETURN nil
