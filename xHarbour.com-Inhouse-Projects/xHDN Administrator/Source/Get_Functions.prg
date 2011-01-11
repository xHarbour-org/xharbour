
#translate xCRLF => CHR(13) + CHR(10)

PROCEDURE ReadSource(cFile)
   LOCAL nIndex := 1

   // READ THE FILE
   aFilelines := HB_ATOKENS(MemoRead(cFile), CHR(13) + CHR(10))

   // REMOVE TRAILING ENTERS
   FOR nIndex := 1 TO LEN(aFilelines)
      aFilelines[nIndex] := RemLeftCR(aFilelines[nIndex])
   NEXT
RETURN

//-- GET ITEM NAME -----------------------------------------------------------------------------------//
FUNCTION GetItem()
   LOCAL cReturn := ""
   LOCAL cType := GetType()
   LOCAL nTempIndex := 0

   // FIND ITEM NAME
   nTempIndex := ASCAN(aFilelines, {|x| AT("!" + UPPER(cType), UPPER(x)) > 0})
   IF nTempIndex != 0
      // APPEND UNTIL A NEW TAG IS ENCOUNTERED
      DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines)
         cReturn := cReturn + aFilelines[nTempIndex] + " "
         nTempIndex := nTempIndex + 1
      END

      // RETRIEVE THE ITEM NAME
      IF RAT("]", cReturn) != 0
         cReturn := SUBSTR(cReturn, AT("]", cReturn) + 1, LEN(cReturn) - (AT("]", cReturn) + 1))
      ELSE
         cReturn := RemLeft(STRTRAN(cReturn, "!" + UPPER(cType), ""), " ")
      END

      cReturn := LTRIM(cReturn)
   END
RETURN cReturn

//-- GET ITEM ONELINER -------------------------------------------------------------------------------//
FUNCTION GetOneliner()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!ABOUT", UPPER(x)) > 0})
   IF nTempIndex != 0
      nTempIndex := nTempIndex + 1
      DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines)
         cReturn := cReturn + aFilelines[nTempIndex] + " "
         nTempIndex := nTempIndex + 1
      END

      cReturn := RemRightCR(cReturn)
   END
RETURN cReturn

//-- GET ITEM TYPE -----------------------------------------------------------------------------------//
FUNCTION GetType()
   IF ASCAN(aFilelines, { |x| AT("!FUNCTION", UPPER(x)) > 0 } ) > 0
      RETURN "Function"
   END
   IF ASCAN(aFilelines, { |x| AT("!COMMAND", UPPER(x)) > 0 } ) > 0
      RETURN "Command"
   END
   IF ASCAN(aFilelines, { |x| AT("!DIRECTIVE", UPPER(x)) > 0 } ) > 0
      RETURN "Directive"
   END
   IF ASCAN(aFilelines, { |x| AT("!OPERATOR", UPPER(x)) > 0 } ) > 0
      RETURN "Operator"
   END
   IF ASCAN(aFilelines, { |x| AT("!STATEMENT", UPPER(x)) > 0 } ) > 0
      RETURN "Statement"
   END
RETURN NIL

//-- GET ITEM SYNTAX ---------------------------------------------------------------------------------//
FUNCTION GetSyntax()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!SYNTAX", UPPER(x)) > 0})
   IF nTempIndex != 0
      nTempIndex := nTempIndex + 1
      DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines) .AND. aFilelines[nTempIndex] <> "!END"
         cReturn := cReturn + aFilelines[nTempIndex] + xCRLF
         nTempIndex := nTempIndex + 1
      END
   END
RETURN cReturn

//-- GET ITEM ARGUMENTS ------------------------------------------------------------------------------//
FUNCTION GetArguments()
   LOCAL cReturn := "", cTempReturn := ""
   LOCAL nTempIndexOne := 0, nTempIndexTwo := 0

   nTempIndexOne := ASCAN(aFilelines, {|x| AT("!ARG", UPPER(x)) > 0})
   nTempIndexTwo := ASCAN(aFilelines, {|x| AT("!GROUP", UPPER(x)) > 0})

   IF nTempIndexTwo <= 0
      nTempIndexTwo := LEN(aFilelines)
   END

   IF nTempIndexOne != 0
      DO WHILE nTempIndexOne != 0 .AND. nTempIndexOne <= nTempIndexTwo
         cTempReturn := RemLeft(aFilelines[nTempIndexOne], "!ARG")
         IF AT("[xhb=extension]", LOWER(cReturn)) > 0
            cTempReturn := "EXT" + ALLTRIM(STRTRAN(cTempReturn, "[xhb=extension]", "")) + "!ARGEND" + xCRLF
         ELSE
            cTempReturn := cTempReturn + "!ARGEND" + xCRLF
         END

         nTempIndexOne := nTempIndexOne + 1
         DO WHILE CheckNewTag(aFilelines[nTempIndexOne]) == .F. .AND. nTempIndexOne <= LEN(aFilelines)
            cTempReturn := cTempReturn + aFileLines[nTempIndexOne] + xCRLF
            nTempIndexOne := nTempIndexOne + 1
         END

         cReturn := RemRightCR(cReturn + cTempReturn) + xCRLF + "!END" + xCRLF + xCRLF
         nTempIndexOne := ASCAN(aFilelines, {|x| AT("!ARG", UPPER(x)) > 0}, nTempIndexOne)
      END

      cReturn := RemRightCR(RemLeftCR(cReturn))
      cReturn := TranslateTags(cReturn)
   END
RETURN cReturn

//-- GET ITEM RETURN ---------------------------------------------------------------------------------//
FUNCTION GetReturn()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!RETURN", UPPER(x)) > 0})
   IF nTempIndex != 0
      nTempIndex := nTempIndex + 1
      DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines)
         cReturn := cReturn + aFilelines[nTempIndex] + xCRLF

         nTempIndex := nTempIndex + 1
      END
      cReturn := RemRightCR(cReturn)
   END
RETURN cReturn

//-- GET ITEM DESCRIPTION ----------------------------------------------------------------------------//
FUNCTION GetDescription()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!DESCRIPTION", UPPER(x)) > 0})
   IF nTempIndex != 0
      nTempIndex := nTempIndex + 1
      DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines)
         cReturn := cReturn + aFilelines[nTempIndex] + xCRLF

         nTempIndex := nTempIndex + 1
      END
      cReturn := RemRightCR(cReturn)

      cReturn := TranslateTags(cReturn)
   END
RETURN cReturn


//-- GET ITEM EXAMPLES -------------------------------------------------------------------------------//
FUNCTION GetExamples()
   LOCAL cReturn := "", cTempReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!EXAMPLE", UPPER(x)) > 0})
   IF nTempIndex != 0
      DO WHILE nTempIndex <> 0
         cTempReturn := RemLeft(STRTRAN(aFilelines[nTempIndex], "!EXAMPLE", ""), " ") + xCRLF
         nTempIndex := nTempIndex + 1
         DO WHILE CheckNewTag(aFilelines[nTempIndex]) == .F. .AND. nTempIndex <= LEN(aFilelines) .AND. aFilelines[nTempIndex] <> "!END"
            cTempReturn := cTempReturn + aFilelines[nTempIndex] + xCRLF
            nTempIndex := nTempIndex + 1
         END
         cReturn := cReturn + cTempReturn + xCRLF + xCRLF
         nTempIndex := ASCAN(aFilelines, {|x| AT("!EXAMPLE", UPPER(x)) > 0}, nTempIndex)
      END
      cReturn := RemRightCR(RemLeftCR(cReturn))
   END
RETURN cReturn


//-- GET ITEM SOURCE FILE ----------------------------------------------------------------------------//
FUNCTION GetFileSource()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("SRC = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "src = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("SRC=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "src="))
         END
      END
   END
RETURN cReturn


//-- GET ITEM MISC FILES -----------------------------------------------------------------------------//
FUNCTION GetFileMisc()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("HDR = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "hdr = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("HDR=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "hdr="))
         END
      END

      IF cReturn == "<!-- #include files -->"
         cReturn := ""
      END
   END
RETURN cReturn


//-- GET ITEM LIBRARY FILE ---------------------------------------------------------------------------//
FUNCTION GetFileLib()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("LIB = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "lib = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("LIB=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "lib="))
         END
      END

      IF cReturn == "<!-- libs required -->"
         cReturn := ""
      END
   END
RETURN cReturn


//-- GET ITEM DLL FILE -------------------------------------------------------------------------------//
FUNCTION GetFileDll()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("DLL = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "dll = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("DLL=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "dll="))
         END
      END

      IF cReturn == "<!-- dlls required -->"
         cReturn := ""
      END
   END
RETURN cReturn


//-- GET ITEM LINKED TOPICS --------------------------------------------------------------------------//
FUNCTION GetLinkedTopics()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("SEE = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "see = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("SEE=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "see="))
         END
      END
   END
RETURN cReturn


//-- GET ITEM EXTENSION SWITCH -----------------------------------------------------------------------//
FUNCTION GetExtension()
   LOCAL cReturn := .F.
RETURN cReturn


//-- GET ITEM FLAG -----------------------------------------------------------------------------------//
FUNCTION GetFlag()
   LOCAL cReturn := "Live"
RETURN cReturn


//-- GET ITEM CATEGORY -------------------------------------------------------------------------------//
FUNCTION GetCategory()
   LOCAL cReturn := ""
   LOCAL nTempIndex := 0
   LOCAL aCategories := {}

   nTempIndex := ASCAN(aFilelines, {|x| AT("!INFO", UPPER(x)) > 0})
   IF nTempIndex <> 0
      nTempIndex := ASCAN(aFilelines, {|x| AT("GRP = ", UPPER(x)) > 0}, nTempIndex)
      IF nTempIndex <> 0
         cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "grp = ", ""))
      ELSE
         nTempIndex := ASCAN(aFilelines, {|x| AT("GRP=", UPPER(x)) > 0}, nTempIndex)
         IF nTempIndex <> 0
            cReturn := LTRIM(STRTRAN(aFilelines[nTempIndex], "grp="))
         END
      END

      aCategories := HB_ATOKENS(cReturn + ",", ",")

      FOR nTempIndex := 1 TO LEN(aCategories)
         aCategories[nTempIndex] := ALLTRIM(RemLeft(aCategories[nTempIndex], ","))
      NEXT
   END
RETURN aCategories