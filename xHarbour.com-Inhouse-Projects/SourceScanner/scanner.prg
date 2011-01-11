/*
* xHarbour.com's source code scanner
* Create documentation database from source code
* Copyright (c) 2005 - Marcelo Lombardo  <lombardo@uol.com.br>
*/

#include "directry.ch"
#include "common.ch"
#include "sqlrdd.ch"

#command REPLIFEMPTY <f1> WITH <v1> [, <fN> WITH <vN> ] ;
         => _FIELD-><f1> := if(empty(_FIELD-><f1>), <v1>, _FIELD-><f1>)

#define CATEGORY_FUNC         1
#define CATEGORY_COMMAND      2
#define CATEGORY_CLASS        3
#define CATEGORY_METHOD       4
#define CATEGORY_DATA         5
#define CATEGORY_OPERATOR     6
#define CATEGORY_STATEMENT    7

#define CRLF      chr(13)+chr(10)

Static cCurrentClassName := ""


Function Main(cFileSpec, cUplMemo)
   LOCAL cUploadMemo := "Uploaded with SourceScanner 2.0 on " + DTOC(DATE()) + " " + TIME() + " by " + NetName(.T.)
   LOCAL lRecursive := .F.

   // Connect to the database (based on the sqlrdd.ini file)
   Connect()
   RDDSetDefault("SQLRDD")

   // Open the tables in the database
   DBCloseAll()
   USE tbl_function NEW
   USE tbl_command NEW
   USE tbl_class NEW
   USE tbl_class_property NEW
   USE tbl_class_method NEW
   USE tbl_operator NEW
   USE tbl_statement NEW

   If !Empty(cUplMemo)
      cUploadMemo := cUplMemo
   EndIf

   If alert( "Search sub directories ?", { "Yes", "No" } ) == 1
      lRecursive := .T.
   EndIf

   FErase( "scanerror.log" )
   
   If !Empty(cFileSpec)
      If File(cFileSpec) .or. "*" $ cFileSpec .or. "?" $ cFileSpec
         Parse(cFileSpec, lRecursive, cUploadMemo)
      Else
         If right(cFileSpec,1) != HB_OsPathSeparator()
            cFileSpec += HB_OsPathSeparator()
         EndIf
         Parse( cFileSpec + "*.prg", lRecursive, cUploadMemo )
         Parse( cFileSpec + "*.c", lRecursive, cUploadMemo )
         Parse( cFileSpec + "*.txt", lRecursive, cUploadMemo )
      EndIf
   Else
      Parse( "*.prg", lRecursive, cUploadMemo )
      Parse( "*.c", lRecursive, cUploadMemo )
      Parse( "*.txt", lRecursive, cUploadMemo )
   EndIf

Return NIL


Static Function Parse( cFileSpec, lRecurse, cUploadMemo )
   
   Local aFiles, cFile, cLine, i, n, aFile, lAdd := .F.
   Local cName, cPar, aPar, cList, cList2, aPar2
   Local oSql, apCode, cPath := "", lClassContext := .F., cClass
   Local lConstruct, hFile, cCat, cFilesFrom, lMakeFile := .F.
   Local cCatOld := "", cFilesFromOld := ""
   Local lInBlock := .F., cTxt := "", nRet
   
   cPath := left( cFileSpec, rat( HB_OsPathSeparator(), cFileSpec ) )

   oSql   := SR_GetConnection()
   apCode := SR_SqlParse( "DELETE FROM ? WHERE FILENAME = ?" )
   aFiles := directory( cFileSpec )

   If file (cPath+"Makefile")
      cFile := memoread( cPath+"Makefile" )
      cCatOld := SubStr(cFile, at( chr(10) + "LIBNAME=", cFile ) + 9, 50 )
      cCatOld := alltrim(memoline(cCatOld,, 1 ))
      If !empty( cCat )
         lMakeFile := .T.
         cFilesFromOld := cCatOld + ".lib"
      EndIf
   EndIf

   For each aFile in aFiles
      ? "Processing file", cPath + aFile[F_NAME], ""

      hFile := fOpen( cPath + aFile[F_NAME] )

      i := 0
      cLine := Space(1024)

      cCat       := cCatOld
      cFilesFrom := cFilesFromOld
      cTxt       := ""
      lInBlock   := .F.

      While .T.
         nRet := hb_fReadLine( hFile, @cLine, { CRLF, chr(10) } )

         i ++
         cLine := AllTrim(StrTran(cLine, Chr(9), " "))

         IF i == 1      // Check if category and destination file name are present in file
            IF Left(cLine, 2) $ "//* " .and. Upper(Right(aFile[F_NAME], 4)) != ".TXT"
               cLine := LTrim(SubStr(cLine, 3))
               IF At("*\", cLine) > 0
                  cLine := AllTrim(SubStr(cLine, 1, At("*/", cLine) -1))
               ENDIF
               aPar := hb_aTokens(cLine, "$")

               // We expect to see in first source line:
               // $CATEGORY$SQLRDD$FILES$sql.lib$

               FOR n = 1 to len(aPar)
                  IF Upper(aPar[n]) == "CATEGORY" .and. Len(aPar) > n
                     cCat := aPar[n+1]
                  ELSEIF Upper(aPar[n]) == "FILES" .and. Len(aPar) > n
                     cFilesFrom := aPar[n+1]
                  ENDIF
               NEXT

               IF cCat == "HIDE" .or. cFilesFrom == "HIDE"
                  Exit              // We do not want to documnt this file
               ENDIF
            ENDIF
         ENDIF

         IF Empty(cCat)
            IF Empty(cPath)
               cCat := CurDir()
            ELSE
               cCat := Left(cPath, Len(cPath) -1)
            ENDIF
            cCat := Right(cCat, Len(cCat) - Rat(HB_OsPathSeparator(), cCat))
         ENDIF

         IF Empty(cFilesFrom)
            cFilesFrom := cCat + ".lib"
         ENDIF

         IF Upper(Right(aFile[F_NAME], 2)) == ".C"
            // CURRENT FILE IS A .C FILE
            IF upper(left(cLine, 8)) == "HB_FUNC(" .or. upper(left(cLine, 9)) == "HB_FUNC ("
               n := at( "(", cLine )
               cName := Upper(AllTrim(SubStr(cLine, n + 1, At( ")", cLine ) -1 -n)))

               lClassContext := .F.

               SELECT tbl_function
               Set Order to 2
               SEEK PadR(cName, 128)
               IF !Found()
                  Append Blank
                  Replace FUNCTION     with cName
                  Replace TYPE         with "HB_FUNC function"
                  //Replace SCOPE        with ""
                  Replace CATEGORY     with cCat
                  Replace SYNTAX       with cName + "() ==> NIL"
                  //Replace ARGUMENTS    with ""
                  //Replace RETURNS      with ""
                  //Replace ONELINER     with ""
                  //Replace DESCRIPTION  with ""
                  //Replace EXAMPLES     with ""
                  //Replace TESTS        with ""
                  Replace STATUS       with "Ready"
                  Replace COMPLIANCE   with "Extension"
                  Replace PLATFORMS    with "All"
                  Replace FILES        with cFilesFrom + ", source is " + alltrim(aFile[F_NAME])
                  //Replace EXTENSION    with ""
                  //Replace SEEALSO      with ""
                  Replace PUBLISH      with 1
                  Replace SOURCEFILE   with aFile[F_NAME]
                  Replace UPLOADMEMO   with cUploadMemo
               ELSE
                  IF ! "source is" $ FILES
                     Replace FILES with IF(Empty(FILES), "", ", ") + "source is " + AllTrim(aFile[F_NAME])
                  ENDIF
               ENDIF
               DBCommit()
            ENDIF

         ELSEIF Upper(Right(aFile[F_NAME], 4)) == ".PRG"
            // CURRENT FILE IS A .PRG FILE
            DO CASE
            CASE Right(cLine, 1) == ";"
               cLine := SubStr(cLine, 1, Len(cLine) - 1)
               lAdd  := .T.
               LOOP

            CASE Upper(Left(cLine, 8)) == "HB_FUNC(" .or. Upper(Left(cLine, 9)) == "HB_FUNC ("
               n     := at( "(", cLine )
               cName := upper(alltrim( SubStr( cLine, n + 1, at( ")", cLine ) -1 -n ) )) 

               lClassContext := .F.

               SELECT tbl_function
               Set Order to 2
               SEEK PadR(cName, 128)
               If !Found()
                  Append Blank
                  Replace FUNCTION     with cName
                  Replace TYPE         with "HB_FUNC function"
                  //Replace SCOPE        with ""
                  Replace CATEGORY     with cCat
                  Replace SYNTAX       with cName + "() ==> NIL"
                  //Replace ARGUMENTS    with ""
                  //Replace RETURNS      with ""
                  //Replace ONELINER     with ""
                  //Replace DESCRIPTION  with ""
                  //Replace EXAMPLES     with ""
                  //Replace TESTS        with ""
                  Replace STATUS       with "Ready"
                  Replace COMPLIANCE   with "Extension"
                  Replace PLATFORMS    with "All"
                  Replace FILES        with cFilesFrom + ", source is " + alltrim(aFile[F_NAME])
                  //Replace EXTENSION    with ""
                  //Replace SEEALSO      with ""
                  Replace PUBLISH      with 1
                  Replace SOURCEFILE   with aFile[F_NAME]
                  Replace UPLOADMEMO   with cUploadMemo                  
               ELSE
                  IF ! "source is" $ FILES
                     Replace FILES        with  if(empty(FILES), "", ", ") + "source is " + alltrim(aFile[F_NAME])
                  ENDIF
               ENDIF
               DBCommit()

            Case upper(left(cLine, 8)) == "FUNCTION"
               cLine := ltrim( SubStr( cLine, 10 ) )
               n     := at( "(", cLine )
               If n == 0
                  n     := at( " ", cLine )
               EndIf
               If n == 0
                  n := len(cLine)+1
               EndIf
               cName := Upper(left( cLine, n - 1 )) 
               cPar  := SubStr( cLine, n + 1 )
               n := at( ")", cPar )
               If n == 0
                  cPar := ""
               Else
                  cPar  := SubStr( cPar, 1, at( ")", cPar ))
               EndIf

               If Empty( cName )
                  ? "Error reading function (" + alltrim(str(i)) + "):", cLine
                  wait
               Else
                  cPar  := alltrim(SubStr( cPar, 1, len(cPar)-1 ) )
                  aPar  := hb_atokens( cPar, "," )
                  cList := ""
                  cList2:= ""

                  For n = 1 to len( aPar )
                     If n > 1
                        cList += ", "
                        cList2 += CRLF
                     EndIf
                     cList += "<" + aPar[n] + ">"
                     cList2 += "<" + aPar[n] + "> "
                  Next

                  SELECT tbl_function
                  Set Order to 2
                  SEEK PadR( cName, 128 )
                  If !Found()
                     Append Blank
                     Replace FUNCTION      with cName
                  EndIf
                  ReplIfEmpty TYPE         with ""
                  //ReplIfEmpty SCOPE        with ""
                  ReplIfEmpty CATEGORY     with cCat
                  ReplIfEmpty SYNTAX       with cName + "() ==> NIL"
                  //ReplIfEmpty ARGUMENTS    with ""
                  //ReplIfEmpty RETURNS      with ""
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty EXAMPLES     with ""
                  //ReplIfEmpty TESTS        with ""
                  ReplIfEmpty STATUS       with "Ready"
                  ReplIfEmpty COMPLIANCE   with "Extension"
                  ReplIfEmpty PLATFORMS    with "All"
                  ReplIfEmpty FILES        with cFilesFrom + ", source is " + alltrim(aFile[F_NAME])
                  //Replace EXTENSION    with ""
                  //ReplIfEmpty SEEALSO      with ""
                  ReplIfEmpty PUBLISH      with 1
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo                  

                  DBCommit()
               ENDIF

            CASE Upper(left(cLine, 9)) == "PROCEDURE"
               cLine := ltrim( SubStr( cLine, 10 ) )
               n     := at( "(", cLine )
               If n == 0
                  n     := at( " ", cLine )
               EndIf
               If n == 0
                  n := len(cLine)+1
               EndIf
               cName := Upper(left( cLine, n - 1 )) 
               cPar  := SubStr( cLine, n + 1 )
               n := at( ")", cPar )
               If n == 0
                  cPar := ""
               Else
                  cPar  := SubStr( cPar, 1, at( ")", cPar ))
               EndIf

               If Empty( cName )
                  ? "Error reading procedure (" + alltrim(str(i)) + "):", cLine
                  wait
               Else
                  cPar  := alltrim(SubStr( cPar, 1, len(cPar)-1 ) )
                  aPar  := hb_atokens( cPar, "," )
                  cList := ""
                  cList2:= ""

                  For n = 1 to len( aPar )
                     If n > 1
                        cList += ", "
                        cList2 += CRLF
                     EndIf
                     cList += "<" + aPar[n] + ">"
                     cList2 += "<" + aPar[n] + "> "
                  Next

                  SELECT tbl_function
                  Set Order to 2
                  SEEK PadR( cName, 128 )
                  If !Found()
                     Append Blank
                     Replace FUNCTION      with cName
                  EndIf
                  ReplIfEmpty TYPE         with "HB_FUNC function"
                  //ReplIfEmpty SCOPE        with ""
                  ReplIfEmpty CATEGORY     with cCat
                  ReplIfEmpty SYNTAX       with cName + "() ==> NIL"
                  //ReplIfEmpty ARGUMENTS    with ""
                  //ReplIfEmpty RETURNS      with ""
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty EXAMPLES     with ""
                  //ReplIfEmpty TESTS        with ""
                  ReplIfEmpty STATUS       with "Ready"
                  ReplIfEmpty COMPLIANCE   with "Extension"
                  ReplIfEmpty PLATFORMS    with "All"
                  ReplIfEmpty FILES        with cFilesFrom + ", source is " + alltrim(aFile[F_NAME])
                  //Replace EXTENSION    with ""
                  //ReplIfEmpty SEEALSO      with ""
                  ReplIfEmpty PUBLISH      with 1
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo                  

                  DBCommit()
               EndIf

            Case upper(left(cLine, 8)) == "ENDCLASS"
               lClassContext := .F.

            Case ( upper(left(cLine, 7)) == "METHOD " .or. upper(left(cLine, 7)) == "MESSAGE" .or. upper(left(cLine, 7)) == "ASSIGN " ) .and. lClassContext
               cLine := ltrim( SubStr( cLine, 8 ) )

               If "CONSTRUCTOR" $ upper( cLine )
                  cLine := alltrim(SubStr( cLine, 1, at( "CONSTRUCTOR", upper(cLine) )-1 ))
                  lConstruct := .T.
               Else
                  lConstruct := .F.
               EndIf

               n     := at( "(", cLine )
               If n == 0
                  n     := at( " ", cLine )     // Method without "()"
               EndIf
               If n == 0
                  n := len(cLine)+1
               EndIf
               cName := left( cLine, n - 1 )
               //cName := Upper(left( cLine, n - 1 ))  
               cPar  := SubStr( cLine, n + 1 )

               n     := at( ")", cPar )
               If n != 0
                  cPar  := SubStr( cPar, 1, n)
               Else
                  cPar  := ""
               EndIf

               If Empty( cName )
                 ? "Error reading method (" + alltrim(str(i)) + "):", cLine
                  wait
               Else
                  cPar  := alltrim(SubStr( cPar, 1, len(cPar)-1 ) )
                  aPar  := hb_atokens( cPar, "," )
                  cList := ""
                  cList2:= ""

                  For n = 1 to len( aPar )
                     If n > 1
                        cList += ", "
                        cList2 += CRLF
                     EndIf
                     cList += "<" + aPar[n] + ">"
                     cList2 += "<" + aPar[n] + "> "
                  Next

                  SELECT tbl_class_method
                  Set Order to 2
                  SEEK PadR( cClass, 64 ) + PadR( cName, 64 )
                  If !Found()
                     Append Blank
                     Replace CLASS        with cClass
                     Replace METHOD       with cName
                  EndIf
                  //ReplIfEmpty TYPE         with ""
                  //ReplIfEmpty SCOPE        with ""
                  ReplIfEmpty CATEGORY     with cCat
                  //ReplIfEmpty ONELINER     with ""
                  ReplIfEmpty SYNTAX       with cName + "( " + cList + " ) ==> NIL"
                  ReplIfEmpty ARGUMENTS    with cList2
                  //ReplIfEmpty RETURNS      with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty EXAMPLES     with ""
                  ReplIfEmpty STATUS       with "Ready"
                  //ReplIfEmpty SEEALSO      with ""
                  IF IsLower(cName)
                     ReplIfEmpty PUBLISH   with 0
                  ELSE
                     ReplIfEmpty PUBLISH   with 1
                  ENDIF
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo
                  DBCommit()

                  If lConstruct
                     SELECT tbl_class
                     Set Order to 2
                     Seek cClass

                     If Found()
                        ReplIfEmpty CONSTRUCTOR  with CONSTRUCTOR + if(Empty(CONSTRUCTOR),"", CRLF+CRLF) + cName + "( " + cList + " ) ==> Self"
                        ReplIfEmpty SEEALSO      with cName + "() Method;"
                     Else
                        ? "Error: Class not found is tbl_class (" + alltrim(str(i)) + "):", cClass
                        wait
                     EndIf
                  EndIf

               EndIf

            CASE upper(left(cLine, 6)) == "CLASS "
               aPar  := hb_atokens( cLine, " " )

               IF len( aPar ) < 2
                  ? "Error reading class (" + alltrim(str(i)) + "):", cLine
                  wait
               ELSE
                  lClassContext := .T.
                  cClass := Upper(aPar[2]) 

                  SELECT tbl_class
                  Set Order to 2
                  SEEK PadR( cClass, 64 )
                  If !Found()
                     Append Blank
                     Replace CLASS        with cClass
                  EndIf
                  //ReplIfEmpty TYPE         with ""
                  ReplIfEmpty INHERITS     with Upper(if( len(aPar) > 3, aPar[4], "" ))
                  //ReplIfEmpty SCOPE        with ""
                  ReplIfEmpty CATEGORY     with cCat
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty CONSTRUCTOR  with ""
                  ReplIfEmpty SYNTAX       with cClass + "( ) ==> Self"
                  //ReplIfEmpty ARGUMENTS    with ""
                  //ReplIfEmpty RETURNS      with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty EXAMPLES     with ""
                  ReplIfEmpty STATUS       with "Ready"
                  ReplIfEmpty COMPLIANCE   with "Extension"
                  ReplIfEmpty PLATFORMS    with "All"
                  ReplIfEmpty FILES        with cFilesFrom + ", source is " + alltrim(aFile[F_NAME])
                  //ReplIfEmpty SEEALSO      with ""
                  //ReplIfEmpty EXTENSION    with ""
                  IF IsLower(cName)
                     ReplIfEmpty PUBLISH   with 0
                  ELSE
                     ReplIfEmpty PUBLISH   with 1
                  ENDIF
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo
                  DBCommit()
               ENDIF

            Case upper(left(cLine, 5)) == "DATA "
               cLine := alltrim( SubStr( cLine, 6 ) )
               aPar  := hb_atokens( cLine, "," )

               For n = 1 to len( aPar )
                  aPar[n] := StrTran( aPar[n], "    ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar2 := hb_atokens( alltrim(aPar[n]), " " )

                  SELECT tbl_class_property
                  Set Order to 2
                  SEEK PadR( cClass, 64 ) + PadR( alltrim(aPar2[1]), 64 )
                  //SEEK PadR( cClass, 64 ) + PadR( Upper(alltrim(aPar2[1])), 64 ) 
                  If !Found()
                     Append Blank
                     Replace CLASS        with cClass
                     Replace PROPERTY     with alltrim(aPar2[1])
                  EndIf
                  //ReplIfEmpty TYPE         with ""
                  ReplIfEmpty SCOPE        with "Published" + if(len(aPar2)>2.and.upper(aPar2[2])=="AS"," " + aPar2[3], "" )
                  ReplIfEmpty CATEGORY     with cCat
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty SEEALSO      with ""
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo
                  IF IsLower(cName)
                     ReplIfEmpty PUBLISH   with 0
                  ELSE
                     ReplIfEmpty PUBLISH   with 1
                  ENDIF
                  DBCommit()

                  If aScan( aPar2, { |x| upper(x) == "INIT" } ) > 0
                     Exit
                  EndIf

               Next

            Case upper(left(cLine, 9)) == "PROPERTY " .or. upper(left(cLine, 7)) == "ACCESS "
               cLine := alltrim( SubStr( cLine, at( " ", cLine ) + 1 ) )
               aPar  := hb_atokens( cLine, "," )

               For n = 1 to len( aPar )

                  aPar[n] := StrTran( aPar[n], "    ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar2 := hb_atokens( alltrim(aPar[n]), " " )

                  SELECT tbl_class_property
                  Set Order to 2
                  SEEK PadR( cClass, 64 ) + PadR( alltrim(aPar2[1]), 64 )
                  //SEEK PadR( cClass, 64 ) + PadR( Upper(alltrim(aPar2[1])), 64 ) 
                  If !Found()
                     Append Blank
                     Replace CLASS        with cClass
                     Replace PROPERTY     with alltrim(aPar2[1])
                  EndIf
                  //ReplIfEmpty TYPE         with ""
                  ReplIfEmpty SCOPE        with "Published"
                  ReplIfEmpty CATEGORY     with cCat
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty SEEALSO      with ""
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo
                  IF IsLower(cName)
                     ReplIfEmpty PUBLISH   with 0
                  ELSE
                     ReplIfEmpty PUBLISH   with 1
                  ENDIF
                  dbCommit()

               Next

            Case upper(left(cLine, 10)) == "CLASSDATA "
               cLine := alltrim( SubStr( cLine, 11 ) )
               aPar  := hb_atokens( cLine, "," )

               For n = 1 to len( aPar )

                  aPar[n] := StrTran( aPar[n], "    ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "   ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar[n] := StrTran( aPar[n], "  ", " " )
                  aPar2 := hb_atokens( alltrim(aPar[n]), " " )

                  SELECT tbl_class_property
                  Set Order to 2
                  SEEK PadR( cClass, 64 ) + PadR( alltrim(aPar2[1]), 64 )
                  //SEEK PadR( cClass, 64 ) + PadR( upper( alltrim(aPar2[1])), 64 ) 
                  If !Found()
                     Append Blank
                     Replace CLASS        with cClass
                     Replace PROPERTY     with alltrim(aPar2[1])
                  EndIf
                  //ReplIfEmpty TYPE         with ""
                  ReplIfEmpty SCOPE        with "Static Published" + if(len(aPar2)>2.and.upper(aPar2[2])=="AS"," " + aPar2[3], "" )
                  ReplIfEmpty CATEGORY     with cCat
                  //ReplIfEmpty ONELINER     with ""
                  //ReplIfEmpty DESCRIPTION  with ""
                  //ReplIfEmpty SEEALSO      with ""
                  ReplIfEmpty SOURCEFILE   with aFile[F_NAME]
                  ReplIfEmpty UPLOADMEMO   with cUploadMemo
                  IF IsLower(cName)
                     ReplIfEmpty PUBLISH   with 0
                  ELSE
                     ReplIfEmpty PUBLISH   with 1
                  ENDIF
                  DBCommit()

                  If aScan( aPar2, { |x| upper(x) == "INIT" } ) > 0
                     Exit
                  EndIf

               Next

            EndCase

         ElseIf upper(right( aFile[F_NAME], 4 )) == ".TXT"     // Incremental load of old TXT HBDOC's file
            If lInBlock
              cTxt += UnComment( cLine )
              If right( rtrim(cLine), 2 ) == "*/"
                  ProcessBlock( cTxt, aFile[F_NAME], cUploadMemo )
                  //Replace UPLOADMEMO with cUploadMemo
                  cTxt := ""
                  lInBlock := .F.
               EndIf
            Else
               If left( ltrim(cLine), 2 ) == "/*"
                  cTxt += UnComment( cLine )
                  lInBlock := .T.
               EndIf
            EndIf
            
         EndIf

         If nRet != 0
            Exit
         EndIf

      EndDo

      fClose( hFile )

   Next

   If lRecurse

      /* recursive directories scan */

      aFiles := directory( cPath + "*.*", "D" )

      For each aFile in aFiles
         if left( aFile[ F_NAME ], 1 ) != "." .and. "D" $ aFile[ F_ATTR ]
            cFile := cPath + aFile[ F_NAME ] + HB_OsPathSeparator()
            ? "   Subdir......", cFile + SubStr( cFileSpec, len( cFile ) + 1 )
            Parse( cFile + SubStr( cFileSpec, len( cFile ) + 1 ) + Right(cFileSpec, Len(cFileSpec) - Len(cPath)), lRecurse, cUploadMemo )
            
         endif
      Next

   EndIf

Return

/*------------------------------------------------------------------------*/

Function UnComment( cLine )

   If left(cLine,2) == "/*"
      cLine := alltrim(SubStr( cLine, 3 ))
   EndIf
   If right(cLine,2) == "*/"
      cLine := SubStr( cLine, 1, len(cLine)-2 )
   EndIf
   If left(cLine,1) == "*"
      cLine := rtrim(SubStr( cLine, 2 ))
   EndIf

Return cLine + CRLF

/*------------------------------------------------------------------------*/

Procedure ProcessBlock( cTxt, cFileName, cUploadMemo )

   Local nLines, nLine, cLine, lToken, cWrite := "", cLastToken := "", cThisToken, cOldToken
   Local nCat := 0, lAppended := .F., nFld, cLine1, lFixed := .F., cOrigToken, cLastOrigToken

   nLines := mlCount( cTxt, 512, 4, .T., .T. )
   For nLine = 1 to nLines
      cLine1 := memoLine( cTxt, 512, nLine, 4, .T., .T. )
      cLine  := alltrim(cLine1)
      If Upper(cLine) == "<FIXED>"
         lFixed := .T.
         Loop
      EndIf
      If Upper(cLine) == "</FIXED>"
         lFixed := .F.
         Loop
      EndIf
      If Empty( cLine )
         cWrite += CRLF
         Loop
      EndIf
      lToken := right(cLine,1) == "$" .and. left( ltrim(cLine), 1 ) == "$" .and. len(alltrim(cLine)) > 2
      If lToken
         cThisToken := SubStr(SubStr(cLine,2),1,len(cLine)-2)
         cOrigToken := cThisToken

         Do Case
         Case cThisToken == "DATATYPE" .or. cThisToken == "PROPERTYTYPE"
//            SR_LogFile( "scanerror.log", { "Token changed (from/to)", cThisToken, "SCOPE", cFileName  } )
            cThisToken := "SCOPE"
         Case cThisToken == "COMMAND"
//            SR_LogFile( "scanerror.log", { "Token changed (from/to)", cThisToken, "COMMANDNAME", cFileName  } )
            cThisToken := "COMMANDNAME"
         Case cThisToken == "PROPERTY"
//            SR_LogFile( "scanerror.log", { "Token changed (from/to)", cThisToken, "DATA", cFileName  } )
            cThisToken := "DATA"
         EndCase

         If (!Empty( cLastToken )) .and. (!Empty( cWrite )) .and. nCat != 0
            DO CASE
            CASE cLastToken = "FUNCNAME"
               nFld := FieldPos("FUNCTION")
            CASE cLastToken = "COMMANDNAME"
               nFld := FieldPos("COMMAND")
            CASE cLastToken = "DATA"
               nFld := FieldPos("PROPERTY")
            OTHERWISE
               nFld := FieldPos( cLastToken )
            ENDCASE
            
            //nFld := FieldPos( cLastToken )
            If nFld > 0
               If Empty( FieldGet( nFld ) )
                  FieldPut( nFld, cWrite )
               EndIf
            Else
               SR_LogFile( "scanerror.log", { "Invalid Token", cLastToken, cFileName  } )
            EndIf
            cLastToken := cThisToken
            cWrite     := ""
         ElseIf (cLastToken == "DOC" .or. cLastToken == "CLASSDOC" ) .and. nCat = 0
            cOldToken  := cLastToken
            cLastToken := cThisToken
            Do Case
            Case cLastToken == "FUNCNAME"
               nCat := CATEGORY_FUNC
               SELECT tbl_function
               SET ORDER TO 2
            Case cLastToken == "COMMANDNAME"
               nCat := CATEGORY_COMMAND
               SELECT tbl_command
               SET ORDER TO 2
            Case cLastToken == "OPERATOR"
               nCat := CATEGORY_OPERATOR
               SELECT tbl_operator
               SET ORDER TO 2
            Case cLastToken == "STATEMENT"
               nCat := CATEGORY_STATEMENT
               SELECT tbl_statement
               SET ORDER TO 2
            Case cLastToken == "CLASS"
               nCat := CATEGORY_CLASS
               SELECT tbl_class
               SET ORDER TO 2
            Case cLastToken == "METHOD"
               nCat := CATEGORY_METHOD
               SELECT tbl_class_method
               SET ORDER TO 2
            Case cLastToken == "DATA"
               nCat := CATEGORY_DATA
               SELECT tbl_class_property
               SET ORDER TO 2
            OtherWise
               cLastToken := cOldToken
               SR_LogFile( "scanerror.log", { "Invalid Token after $DOC$ or $CLASSDOC$", cLastToken, cFileName, nLine  } )
               Loop
            EndCase
            //SET ORDER TO 2

         ElseIf (!Empty( cLastToken )) .and. (!Empty( cWrite )) .and. nCat != 0 .and. (nFld := FieldPos( cLastToken )) == 0
            SR_LogFile( "scanerror.log", { "Invalid Token", cLastToken, cFileName } )
         Else
            cLastToken := cThisToken
            cWrite     := ""
         EndIf
      Else
         If cLastToken == "EXAMPLES" .or. lFixed
            cWrite += rtrim(cLine1)           // Should not break identation in example
         Else
            cWrite += cLine
         EndIf

         If (cLastToken == "FUNCNAME" .or. cLastToken == "METHOD") .and. at( "(", cWrite ) > 0
            cWrite := SubStr( cWrite, 1, at( "(", cWrite ) -1 )
            //cWrite := Upper(SubStr( cWrite, 1, at( "(", cWrite ) -1 ))  
         EndIf

         Do Case
         Case cLastToken == "FUNCNAME"
            cWrite := Upper( cWrite ) 
            SEEK PadR( cWrite, 128 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
            EndIf
         Case cLastToken == "COMMANDNAME"
            SEEK PadR( cWrite, 128 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "OPERATOR"
            SEEK PadR( cWrite, 128 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "STATEMENT"
            SEEK PadR( cWrite, 128 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "CLASS"
            cWrite := Upper( cWrite ) 
            cCurrentClassName := cWrite
            SEEK PadR( cWrite, 64 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "INHERITS"
            cWrite := Upper( cWrite ) 
         Case cLastToken == "METHOD"
            cWrite := cWrite
            //cWrite := Upper( cWrite )   
            SEEK PadR( cCurrentClassName, 64 ) + PadR( cWrite, 64 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace CLASS      with cCurrentClassName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "DATA"
            cWrite := cWrite
            //cWrite := Upper( cWrite )  
            SEEK PadR( cCurrentClassName, 64 ) + PadR( cWrite, 64 )
            If !Found()
               Append Blank
               Replace SOURCEFILE with cFileName
               Replace CLASS      with cCurrentClassName
               Replace UPLOADMEMO with cUploadMemo
            EndIf
         Case cLastToken == "EXAMPLES" .or. lFixed
            cWrite += CRLF
         EndCase

      EndIf
   Next
Return

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/