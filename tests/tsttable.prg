#include 'ttable.ch'

request Dbfcdx

PROCEDURE main

   LOCAL x      := 1
   LOCAL oTable := nil
   
   RDDSETDEFAULT( 'dbfcdx')

   doData()

   /*Open An Table With Table Class */
   DEFINE TABLE oTable FILE tsttable NEW
   
   /*Adding an Index to This Table */
   DEFINE ORDER ON KEY "nome" TAG _1 IN oTable

   /* Force the index Creating*/
   oTable:Reindex()

   WHILE x <= 100

      oTable:ReadBlank()

      oTable:name   := Str( x, 20 )
      oTable:street := Str( x + 1, 20 )
      oTable:city   := Str( x + 2, 20 )
      oTable:code   := x
      oTable:today  := Date()
      oTable:pay    := ( x % 2 ) == 0

      oTable:Append()

      oTable:Write()

      x ++

   ENDDO

   USE

RETURN

PROCEDURE dodata()

   LOCAL oTable

   IF !FILE( 'tsttable.dbf' )

      /* Create An Table using Table Classe Syntax */

      CREATE DATABASE oTable FILE tsttable.dbf

      FIELD NAME name   TYPE CHARACTER LEN 40 DEC 0 OF oTable
      FIELD NAME street TYPE CHARACTER LEN 40 DEC 0 OF oTable
      FIELD NAME city   TYPE CHARACTER LEN 40 DEC 0 OF oTable
      FIELD NAME code   TYPE NUMERIC   LEN  5 DEC 0 OF oTable
      FIELD NAME today  TYPE DATE      LEN  8 DEC 0 OF oTable
      FIELD NAME pay    TYPE LOGICAL   LEN  1 DEC 0 OF oTable

      BUILD TABLE oTable

   ENDIF

RETURN

