

PROCEDURE FilterTree(oTree, cFolder, nFilter, cCondition, lLevel)
   LOCAL nTreeIndex, aTreeFiles
   LOCAL cFileName := "", cFileType := ""

   aTreeFiles := Directory(cFolder, "D")
   ASort(aTreeFiles, , , { |x, y| LOWER(x[1]) < LOWER(y[1]) })

   FOR nTreeIndex := 1 TO LEN(aTreeFiles)
      cFileName := aTreeFiles[nTreeIndex][1]
      cFileType := aTreeFiles[nTreeIndex][5]

      IF (cFileType == "DX" .OR. cFileType == "D") .AND. UPPER(cFileName) <> "CVS" .AND. cFileName <> "." .AND. cFileName <> ".."
         FilterTree(oTree:AddItem(cFileName), cFolder + cFileName + "\", nFilter, cCondition, lLevel)
      END

      IF cFileName <> "." .AND. cFileName <> ".." .AND. RIGHT(cFileName, 4) == ".txt" .AND. LOWER(cFileName) <> "makefile.en.txt"
         IF LEN(cCondition) > 0
            IF lLevel == .T.
               IF AT(LOWER(cCondition), LOWER(MEMOREAD(cFolder + cFileName))) > 0
                  IF nFilter == 2 .AND. GetLatestStatus(cFolder + cFileName) == "New file"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 3 .AND. GetLatestCRC(cFolder + cFileName) <> CalculateCRC(MemoRead(cFolder + cFileName))
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 4 .AND. GetLatestStatus(cFolder + cFileName) == "Approved"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 5 .AND. GetLatestStatus(cFolder + cFileName) == "Disapproved"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 1
                     oTree:Additem(cFileName)
                  END
               END
            ELSE
               IF AT(LOWER(cCondition), LOWER(cFileName)) > 0
                  IF nFilter == 2 .AND. GetLatestStatus(cFolder + cFileName) == "New file"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 3 .AND. GetLatestCRC(cFolder + cFileName) <> CalculateCRC(MemoRead(cFolder + cFileName))
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 4 .AND. GetLatestStatus(cFolder + cFileName) == "Approved"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 5 .AND. GetLatestStatus(cFolder + cFileName) == "Disapproved"
                     oTree:AddItem(cFileName)
                  ELSEIF nFilter == 1
                     oTree:Additem(cFileName)
                  END
               END
            END
         ELSE
            IF nFilter == 2 .AND. GetLatestStatus(cFolder + cFileName) == "New file"
               oTree:AddItem(cFileName)
            ELSEIF nFilter == 3 .AND. GetLatestCRC(cFolder + cFileName) <> CalculateCRC(MemoRead(cFolder + cFileName))
               oTree:AddItem(cFileName)
            ELSEIF nFilter == 4 .AND. GetLatestStatus(cFolder + cFileName) == "Approved"
               oTree:AddItem(cFileName)
            ELSEIF nFilter == 5 .AND. GetLatestStatus(cFolder + cFileName) == "Disapproved"
               oTree:AddItem(cFileName)
            ELSEIF nFilter == 1
               oTree:Additem(cFileName)
            END
         END
      END
   NEXT
RETURN


FUNCTION GetFullPathFromTree(oTree)
   LOCAL cPath := oTree:GetSelected():Caption
   LOCAL oTreeItem := oTree:GetSelected()
   LOCAL aOwnerFolders := {}

   DO WHILE HB_ISNIL(oTreeItem:Owner) == .F.
      cPath := oTreeItem:Owner:Caption + "\" + cPath
      oTreeItem := oTreeItem:Owner
   END

   cPath := AppFileLocation + cPath
RETURN cPath


FUNCTION GetPathsFromTree(oTree)
   LOCAL cPath := ""
   LOCAL nIndex := 1

   IF LEN(oTree:Items) > 0
      FOR nIndex := 1 TO LEN(oTree:Items)
         GetPathsFromTree(oTree:Items[nIndex])
      NEXT
   ELSE
      cPath := oTree:Caption
      DO WHILE HB_ISNIL(oTree:Owner) == .F.
         cPath := oTree:Owner:Caption + "\" + cPath
         oTree := oTree:Owner
      END
      AADD(aTemporaryArray, cPath := AppFileLocation + cPath)
   END
RETURN NIL


FUNCTION GetLatestStatus(cFile)
   LOCAL aApproveHistory := ReadApproveHistory(cFile)
   LOCAL cStatus := ""

   IF LEN(aApproveHistory) > 0
      cStatus := aApproveHistory[LEN(aApproveHistory)][4]
   ELSE
      cStatus := "New file"
   END
RETURN cStatus


FUNCTION GetLatestCRC(cFile)
   LOCAL aApproveHistory := ReadApproveHistory(cFile)
   LOCAL nCRC := 0

   IF LEN(aApproveHistory) > 0
      nCRC := VAL(aApproveHistory[LEN(aApproveHistory)][3])
   ELSE
      nCRC := 0
   END
RETURN nCRC


FUNCTION ReadApproveHistory(cFile)
   LOCAL nFileHandle, cFileLine
   LOCAL aApprovals := {}
   LOCAL cName, cDate, cCRC, cStatus

   nFileHandle := FOpen(cFile, 0)

   IF nFileHandle >= 0
      HB_FReadLine(nFileHandle, @cFileLine)

      DO WHILE UPPER(LEFT(cFileLine, 13)) == "!APPROVED BY:" .OR. UPPER(LEFT(cFileLine, 16)) == "!DISAPPROVED BY:"
         IF (UPPER(LEFT(cFileLine, 13)) == "!APPROVED BY:" .AND. LEN(ALLTRIM(cFileLine)) > 13) .OR. (UPPER(LEFT(cFileLine, 16)) == "!DISAPPROVED BY:" .AND. LEN(ALLTRIM(cFileLine)) > 16)
            IF UPPER(LEFT(cFileLine, 13)) == "!APPROVED BY:"
               cStatus := "Approved"
               cFileLine := ALLTRIM(RemLeftCR(RemRightCR(SUBSTR(cFileLine, 14))))
            ELSE
               cStatus := "Disapproved"
               cFileLine := ALLTRIM(RemLeftCR(RemRightCR(SUBSTR(cFileLine, 17))))
            END

            cName := LEFT(cFileLine, RAT(" on ", cFileLine) - 1)
            cDate := SUBSTR(cFileLine, RAT(" on ", cFileLine) + 4, (RAT(" (", cFileLine)) - (RAT(" on ", cFileLine) + 4))
            cCRC := SUBSTR(cFileLine, RAT(" (",cFileLine) + 2, LEN(cFileLine) - (RAT(" (",cFileLine) + 2))

            AADD(aApprovals, {cName, cDate, cCRC, cStatus})
         END
         HB_FReadLine(nFileHandle, @cFileLine)
      END

      FClose(nFileHandle)
   END
RETURN aApprovals


FUNCTION CalculateCRC(cText)
   LOCAL aTempArray := {}, cTempString := "", i := 1
   LOCAL nCRC := 0

   IF LEN(cText) > 0
      aTempArray := HB_ATOKENS(cText, xCRLF)

      FOR i := 1 TO LEN(aTempArray)
         IF UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 13)) <> "!APPROVED BY:" .AND. UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 16)) <> "!DISAPPROVED BY:"
            cTempString := cTempString + RemRightCR(RemLeftCR(aTempArray[i])) + xCRLF
         END
      NEXT
      cTempString := RemRightCR(RemLeftCR(cTempString))

      nCRC := HB_CRC32(cTempString)
   ELSE
      nCRC := 0
   END
RETURN nCRC


FUNCTION RemLeftCR(cLine)
   LOCAL cNewLine := cLine
   LOCAL nCharPointer := 1

   DO WHILE nCharPointer <= LEN(cNewLine) .AND. (ASC(cNewLine[nCharPointer]) == 10 .OR. ASC(cNewLine[nCharPointer]) == 13)
      cNewLine := SUBSTR(cNewLine, 2, LEN(cNewLine) - 1)
      // nCharPointer := nCharPointer + 1
   END
RETURN cNewLine


FUNCTION RemRightCR(cLine)
   LOCAL cNewLine := cLine
   LOCAL nCharPointer := LEN(cLine)

   DO WHILE nCharPointer > 0 .AND. (ASC(cNewLine[nCharPointer]) == 10 .OR. ASC(cNewLine[nCharPointer]) == 13)
      cNewLine := SUBSTR(cNewLine, 1, nCharPointer - 1)
      nCharPointer := nCharPointer - 1
   END
RETURN cNewLine


FUNCTION RemLeft(cLine, cSign)
   LOCAL cNewLine := cLine
   LOCAL nCharPointer := 1

   DO WHILE UPPER(cNewLine[nCharPointer]) == UPPER(cSign)
      cNewLine := SUBSTR(cNewLine, 2, LEN(cNewLine) - 1)
      // nCharPointer := nCharPointer + 1
   END
RETURN cNewLine


FUNCTION RemRight(cLine, cSign)
   LOCAL cNewLine := cLine
   LOCAL nCharPointer := LEN(cLine)

   DO WHILE UPPER(cNewLine[nCharPointer]) == UPPER(cSign)
      cNewLine := SUBSTR(cNewLine, 1, LEN(cNewLine) - 1)
      nCharPointer := nCharPointer - 1
   END
RETURN cNewLine


FUNCTION CreateTimestamp()
   LOCAL cTimestamp := ""

   cTimestamp := cTimestamp + ALLTRIM(STR(YEAR(DATE()))) + "-" + ALLTRIM(STR(MONTH(DATE()))) + "-" + ALLTRIM(STR(DAY(DATE()))) + " "
   cTimestamp := cTimestamp + SUBSTR(TIME(), 1, 2) + ":"
   cTimestamp := cTimestamp + SUBSTR(TIME(), 4, 2) + ":"
   cTimestamp := cTimestamp + SUBSTR(TIME(), 7, 2)
RETURN cTimestamp


FUNCTION ApproveFile(cFile)
   LOCAL aApproveHistory := {}
   LOCAL cFileHeader := "", cFileBody := ""
   LOCAL aTempArray := {}, i := 1

   IF VALTYPE(cFile) == "A"
      FOR i := 1 TO LEN(cFile)
         ApproveFile(cFile[i])
      NEXT
   ELSEIF VALTYPE(cFile) == "C"
      IF FILE(cFile)
         aApproveHistory := ReadApproveHistory(cFile)

         FOR i := 1 TO LEN(aApproveHistory)
            cFileHeader := cFileHeader + "!" + UPPER(aApproveHistory[i][4]) + " BY: " + aApproveHistory[i][1] + " on " + aApproveHistory[i][2] + " (" + aApproveHistory[i][3] + ")" + xCRLF
         NEXT
         cFileHeader := cFileHeader + "!APPROVED BY: " + oINI:ReadString("GENERAL", "USERNAME") + " on " + CreateTimestamp() + " ("

         aTempArray := HB_ATOKENS(MEMOREAD(cFile), xCRLF)
         FOR i := 1 TO LEN(aTempArray)
            IF UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 13)) <> "!APPROVED BY:" .AND. UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 16)) <> "!DISAPPROVED BY:"
              cFileBody := cFileBody + RemRightCR(RemLeftCR(aTempArray[i])) + xCRLF
            END
         NEXT
         cFileBody := RemLeftCR(RemRightCR(cFileBody))

         cFileHeader := cFileHeader + ALLTRIM(STR(HB_CRC32(cFileBody))) + ")"

         IF LEN(aApproveHistory) == 0 .OR. HB_CRC32(cFileBody) <> VAL(aApproveHistory[LEN(aApproveHistory)][3]) .OR. aApproveHistory[LEN(aApproveHistory)][4] == "Disapproved"
            MEMOWRIT(cFile, cFileHeader + xCRLF + xCRLF + cFileBody)
         ELSE
            RETURN -1
         END         
      ELSE
         RETURN 0
      END
   ELSE
      RETURN 0
   END
RETURN 1


FUNCTION DisapproveFile(cFile)
   LOCAL aApproveHistory := {}
   LOCAL cFileHeader := "", cFileBody := ""
   LOCAL aTempArray := {}, i := 1

   IF VALTYPE(cFile) == "A"
      FOR i := 1 TO LEN(cFile)
         DisapproveFile(cFile[i])
      NEXT
   ELSEIF VALTYPE(cFile) == "C"
      IF FILE(cFile)
         aApproveHistory := ReadApproveHistory(cFile)

         FOR i := 1 TO LEN(aApproveHistory)
            cFileHeader := cFileHeader + "!" + UPPER(aApproveHistory[i][4]) + " BY: " + aApproveHistory[i][1] + " on " + aApproveHistory[i][2] + " (" + aApproveHistory[i][3] + ")" + xCRLF
         NEXT
         cFileHeader := cFileHeader + "!DISAPPROVED BY: " + oINI:ReadString("GENERAL", "USERNAME") + " on " + CreateTimestamp() + " ("

         aTempArray := HB_ATOKENS(MEMOREAD(cFile), xCRLF)
         FOR i := 1 TO LEN(aTempArray)
            IF UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 13)) <> "!APPROVED BY:" .AND. UPPER(LEFT(RemRightCR(RemLeftCR(aTempArray[i])), 16)) <> "!DISAPPROVED BY:"
              cFileBody := cFileBody + RemRightCR(RemLeftCR(aTempArray[i])) + xCRLF
            END
         NEXT
         cFileBody := RemLeftCR(RemRightCR(cFileBody))

         cFileHeader := cFileHeader + ALLTRIM(STR(HB_CRC32(cFileBody))) + ")"

         IF LEN(aApproveHistory) == 0 .OR. HB_CRC32(cFileBody) <> VAL(aApproveHistory[LEN(aApproveHistory)][3]) .OR. aApproveHistory[LEN(aApproveHistory)][4] == "Approved"
            MEMOWRIT(cFile, cFileHeader + xCRLF + xCRLF + cFileBody)
         ELSE
            RETURN -1
         END         
      ELSE
         RETURN 0
      END
   ELSE
      RETURN 0
   END
RETURN 1


FUNCTION UploadFile(cFile)
   LOCAL nDBConn, cDBConn := oIni:ReadString("DATABASE", "CONNSTRING")
   LOCAL aFiles:= {}, nIndex
   LOCAL cFileUrl, cFilename
   LOCAL oError
   LOCAL nRecordID := 0, aCategories, nIndex2, nRecordCategory

   IF VALTYPE(cFile) == "C"
      AADD(aFiles, cFile)
   ELSEIF VALTYPE(cFile) == "A"
      ACOPY(cFile, aFiles, 1, LEN(cFile))
   END

   FOR nIndex := 1 TO LEN(aFiles)
      TRY
         nDBConn := SR_AddConnection(3, cDBConn)

         SR_SetActiveConnection(nDBConn)
         SR_RecnoName("ID")

         cFileUrl := cFile
         cFilename := SUBSTR(cFile, RAT("\", cFile) + 1)

         USE REFERENCE_LANGUAGE NEW VIA "SQLRDD" ALIAS RS

         SET FILTER TO &("ALLTRIM(ASCII_FILENAME) = '" + cFilename + "'")
         RS->(DBGOTOP())

         ReadSource(cFileUrl)

         IF HB_ISNIL(GetType()) == .F.
            IF RS->(EOF())
               RS->(DBAPPEND())
               REPLACE ASCII_CRC WITH "0"
            END
            
            IF RS->ASCII_CRC <> ALLTRIM(STR(GetLatestCRC(cFileUrl)))
               REPLACE TIMESTAMP WITH CreateTimestamp()
               REPLACE ITEM WITH GetItem()
               REPLACE ONELINER WITH GetOneliner()
               REPLACE TYPE WITH GetType()
               REPLACE SYNTAX WITH GetSyntax()
               REPLACE ARGUMENTS WITH GetArguments()
               REPLACE RETURNS WITH GetReturn()
               REPLACE DESCRIPTION WITH GetDescription()
               REPLACE EXAMPLES WITH GetExamples()
               REPLACE FILE_SOURCE WITH GetFileSource()
               REPLACE FILE_MISC WITH GetFileMisc()
               REPLACE FILE_LIB WITH GetFileLib()
               REPLACE FILE_DLL WITH GetFileDll()
               REPLACE LINKED_TOPICS WITH GetLinkedTopics()
               REPLACE FLAG_EXTENSION WITH GetExtension()
               REPLACE FLAG_STATUS WITH GetFlag()
               REPLACE ASCII_FILENAME WITH cFilename
               REPLACE ASCII_CRC WITH ALLTRIM(STR(GetLatestCRC(cFileUrl)))
            
               RS->(DBCOMMIT())
               RS->(DBGOBOTTOM())
               nRecordID := RS->(RECNO())
            
               aCategories := GetCategory()
               FOR nIndex2 := 1 TO LEN(aCategories)
                  USE reference_categories NEW VIA "SQLRDD" ALIAS RS_CAT
                  SET FILTER TO RS_CAT->NAME = aCategories[nIndex2]
                  RS_CAT->(DBGOTOP())
            
                  IF RS_CAT->(EOF())
                     RS_CAT->(DBAPPEND())
            
                     REPLACE TIMESTAMP WITH CreateTimestamp()
                     REPLACE NAME WITH aCategories[nIndex2]
            
                     RS_CAT->(DBCOMMIT())
                     RS_CAT->(DBGOBOTTOM())
                     nRecordCategory := RS_CAT->(RECNO())
                  ELSE
                     nRecordCategory := RS_CAT->(RECNO())
                  END
            
                  USE reference_language_category NEW VIA "SQLRDD" ALIAS RS_LAN_CAT
            
                  RS_LAN_CAT->(DBAPPEND())
                  REPLACE TIMESTAMP WITH CreateTimestamp()
                  REPLACE LANGUAGE_ID WITH nRecordID
                  REPLACE CATEGORY_ID WITH nRecordCategory
            
                  CLOSE RS_LAN_CAT
                  CLOSE RS_CAT
               NEXT            
            END
         END
      CATCH oError
         
      FINALLY
         CLOSE ALL
      END
   NEXT
RETURN 1


FUNCTION TranslateTags(cRawValue)
   LOCAL cReturn := cRawValue
   LOCAL aTextlines := HB_ATOKENS(cReturn, xCRLF)
   LOCAL aTableItems := {}
   LOCAL nIndex := 0, nTempIndex := 0

   nIndex := ASCAN(aTextlines, {|x| "!P[NOTE=" $ UPPER(x) })
   DO WHILE nIndex <> 0
      aTextlines[nIndex] := "!DIV_NOTE " + SUBSTR(aTextlines[nIndex], 10, RAT("]", UPPER(aTextlines[nIndex])) - 10) + " !DIV_NOTEEND"
      nIndex := ASCAN(aTextlines, {|x| "!P[NOTE=" $ UPPER(x) }, nIndex + 1)
   END

   nIndex := ASCAN(aTextlines, {|x| "!P[INDENT=3" $ UPPER(x) })
   DO WHILE nIndex <> 0
      aTextlines[nIndex] := "!DIV_INDENT3 " + SUBSTR(aTextlines[nIndex], RAT("]", UPPER(aTextlines[nIndex])) + 1) + " !DIV_INDENT3END"
      nIndex := ASCAN(aTextlines, {|x| "!P[INDENT=3" $ UPPER(x) }, nIndex + 1)
   END
   
   nIndex := ASCAN(aTextlines, {|x| "!P[TITLE" $ UPPER(x) })
   DO WHILE nIndex <> 0
      aTextlines[nIndex] := "!DIV_TITLE " + SUBSTR(aTextlines[nIndex], RAT("]", UPPER(aTextlines[nIndex])) + 1) + " !DIV_TITLEEND"
      nIndex := ASCAN(aTextlines, {|x| "!P[TITLE" $ UPPER(x) }, nIndex + 1)
   END
   
   nIndex := ASCAN(aTextlines, {|x| "!TABLE" $ UPPER(x) })
   DO WHILE nIndex <> 0
      aTextlines[nIndex] := "!DIV_TABLE " + SUBSTR(aTextlines[nIndex], RAT("]", UPPER(aTextlines[nIndex])) + 1) + " !DIV_TABLEEND" + xCRLF + "!TABLE"
      nIndex := nIndex + 1

      DO WHILE AT("!END", UPPER(aTextlines[nIndex])) == 0
         // TODO: split cells into cells (now only one big cell)
         //aTableItems := HB_ATOKENS(aTextlines[nIndex], "    ")

         //FOR nTempIndex := 1 TO LEN(aTableItems)
            //messagebox(, aTableItems[nTempIndex])
         //NEXT
         
         aTextlines[nIndex] := "!TR !TDL " + aTextlines[nIndex] + " !TDEND !TREND"
         nIndex := nIndex + 1
      END
      aTextlines[nIndex] := "!TABLEEND" + SUBSTR(aTextlines[nIndex], 6)

      nIndex := ASCAN(aTextlines, {|x| "!TABLE" $ UPPER(x) }, nIndex + 1)
   END   

   cReturn := ""
   FOR nIndex := 1 TO LEN(aTextlines)
      cReturn := cReturn + RemLeftCR(aTextlines[nIndex]) + xCRLF
   NEXT
RETURN cReturn


FUNCTION CheckNewTag(cLine)
   LOCAL aTags, i
   aTags := {"!ABOUT", "!SYNTAX", "!ARG", "!DESCRIPTION", "!EXAMPLE", "!INFO", "!RETURN", "!GROUP Instance variables", "!GROUP State changing methods"}

   FOR i := 1 TO LEN(aTags)
      IF AT(UPPER(aTags[i]), UPPER(cLine)) > 0
         RETURN .T.
      END
   NEXT
RETURN .F.