/*
* SQLRDD dbf2sql
* Sample application to upload dbf files to SQL databases
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "directry.ch"
#include "sqlrdd.ch"       // SQLRDD Main include

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

REQUEST DBFNTX
REQUEST DBFCDX
REQUEST DBFFPT
REQUEST DBFDBT

/*------------------------------------------------------------------------*/

Function Main( cRDD, cDSN )

   local nCnn, nDrv, cDriver

   clear screen

   ? ""
   ? "dbf2sql.exe"
   ? ""
   ? "Sample tool to upload dbf files to SQL databases"
   ? "(c) 2004 - Marcelo Lombardo"
   ? ""

   If !Connect(@cRDD, cDSN)
      Return
   EndIf

   nDrv := Alert( "Select source RDD", { "DBFCDX", "DBFNTX" } )

   Do Case
   Case nDrv == 1
      cDriver := "DBFCDX"
   Case nDrv == 2
      cDriver := "DBFNTX"
   EndCase
   RddSetDefault( cDriver )

   SET DELETED ON

   ? "RDD in use          :", cRDD

   upload( ".\", "", cDriver, cRDD )

Return NIL

/*------------------------------------------------------------------------*/

Function upload( cBaseDir, cPrefix, cDriver, cRDD )

   local aFiles, aStruct, aFile, cFile

   /* upload files */

   aFiles := directory( cBaseDir + "*.dbf" )

   For each aFile in aFiles
      cFile := strtran(lower( alltrim( cPrefix + aFile[ F_NAME ] ) ),".dbf","")
      dbUseArea( .T., cDriver, cBaseDir + aFile[ F_NAME ], "ORIG" )
      ? "   Uploading...", cFile, "(" + alltrim(str(ORIG->( lastrec() ) ) ), "records)"
      aStruct := ORIG->( dbStruct() )
      ORIG->( dbCloseArea() )

      dbCreate( cFile, aStruct, cRDD )
      dbUseArea( .T., cRDD, cFile, "DEST", .F. )
      Append from (cBaseDir + aFile[ F_NAME ]) VIA cDriver

      dbUseArea( .T., cDriver, cBaseDir + aFile[ F_NAME ], "ORIG" )

      If !empty( ordname(1) )
         ? "   Creating indexes:", cFile
      EndIf

      n:=1
      while .t.
         if empty( ordname(n) )
            exit
         endif
        ? "      =>", ordname(n),",", ordkey(n),",", ordfor(n)
        DEST->(ordCondSet(orig->(ordfor(n)),,.t.,,,, nil, nil, nil, nil,, nil, .F., .F., .F., .F.))
        DEST->(dbGoTop())
        DEST->(ordCreate(,orig->(OrdName(n)), orig->(ordKey(n)), &("{||"+orig->(OrdKey(n))+"}") ))
        ++n
      enddo
      ORIG->( dbCloseArea() )
      DEST->( dbCloseArea() )
   Next

   /* recursive directories scan */

   aFiles := directory( cBaseDir + "*.*", "D" )

   For each aFile in aFiles
      if left( aFile[ F_NAME ], 1 ) != "." .and. "D" $ aFile[ F_ATTR ]
        cFile := cBaseDir + aFile[ F_NAME ] + HB_OsPathSeparator()
         ? "   Subdir......", cFile
         upload( cFile, cPrefix + lower(alltrim(aFile[ F_NAME ])) + "_", cDriver )
      endif
   Next

Return

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
