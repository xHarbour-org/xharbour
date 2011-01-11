
#include "mysql.ch" 
#include "sqlrdd.ch"

REQUEST SQLRDD
REQUEST SR_MYSQL
REQUEST DBFCDX

PROCEDURE MAIN()
   LOCAL cConnString := "HST=mssql.bekz.net;UID=xHarbour_xh_mir;PWD=ncj45djc;DTB=xharbour_xhdn_mirror"
   LOCAL aTables := {"language_misc", "language_classes", "language_class_properties", "language_class_methods", "language_articles"}
   //LOCAL cConnString := "HST=shop.yorcom.nl;UID=yorcom_shop;PWD=S0mLf9igo7;DTB=yorcom_shop"
   //LOCAL aTables := {"klanten"}
   LOCAL nIndex

   FOR nIndex := 1 TO LEN(aTables)
      CopyDBStructure(cConnString, aTables[nIndex])
      CopyDBRecords(cConnString, aTables[nIndex])
   NEXT

   WAIT
RETURN

FUNCTION CopyDBStructure(cConnString, cTable)
   LOCAL nDBConn

   TRY
      nDBConn := SR_AddConnection(3, cConnString)

      IF nDBConn == 1
         SR_SetActiveConnection(nDBConn)
         SR_RecnoName("ID")
      END
   CATCH oErr
      ? oErr:Description
   END

   IF nDBConn == 1
      USE &(cTable) NEW VIA "SQLRDD" ALIAS dbSource
      COPY STRUCTURE TO &(cTable)

      CLOSE dbSource
      SR_EndConnection(nDBConn)
   END
RETURN NIL

FUNCTION CopyDBRecords(cConnString, cTable)
   LOCAL nDBConn
   LOCAL nField := 0

   TRY
      nDBConn := SR_AddConnection(3, cConnString)

      IF nDBConn == 1
         SR_SetActiveConnection(nDBConn)
         SR_RecnoName("ID")
      END
   CATCH oErr
      ? oErr:Description
   END

   IF nDBConn == 1
      USE &(cTable) NEW VIA "SQLRDD" ALIAS dbSource
      GO TOP

      USE &(cTable) NEW VIA "DBFCDX" ALIAS dbTarget

      WHILE dbSource->(EOF()) == .F.
         dbTarget->(DBAppend())
         FOR nField := 0 TO dbSource->(FCOUNT())
            dbTarget->(FIELDPUT(nField, dbSource->(FIELDGET(nField))))
         NEXT

         dbSource->(DBSKIP())
      END

      CLOSE dbSource
      CLOSE dbTarget

      SR_EndConnection(nDBConn)
   END
RETURN NIL