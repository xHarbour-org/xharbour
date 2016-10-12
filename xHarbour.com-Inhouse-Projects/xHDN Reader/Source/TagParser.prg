FUNCTION DocTag(cDataToTranslate)
   LOCAL cData := cDataToTranslate
   LOCAL cTag := ""

   //' CHECK IF THE ARGUMENT HAS AN EMPTY VALUE
   IF HB_ISNIL(cData)
      RETURN ""
   ELSEIF LEN(ALLTRIM(cData)) == 0
      RETURN ""
   END

   //' REPLACE SPECIAL CHARACTERS
   cData := STRTRAN(cData, "&", "&amp;")
   cData := STRTRAN(cData, "<", "&lt;")
   cData := STRTRAN(cData, ">", "&gt;")
   cData := STRTRAN(cData, "!EQL", "=")
   cData := STRTRAN(cData, "!HSH", "#")
   cData := STRTRAN(cData, "!GT", "&gt;")
   cData := STRTRAN(cData, "!LT", "&lt;")

   //' RESTORE ORIGINAL COMMENT TAGS
   cData := STRTRAN(cData, "&lt;!--", "<!--")
   cData := STRTRAN(cData, "--&gt;", "-->")

   //' FILTER OUT THE LINK-TAGS
   cData := STRTRAN(cData, "!LINK ", "")
   cData := STRTRAN(cData, "!ELINK ", "")
   cData := STRTRAN(cData, "!LINK", "")
   cData := STRTRAN(cData, "!ELINK", "")

   //' TRANSLATE ITALIC TEXT
   cTag := "!I"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EI", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<i>" + cTagText + "</i>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   //' TRANSLATE BOLD TEXT
   cTag := "!B"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EB", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<strong>" + cTagText + "</strong>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   cData := STRTRAN(cData, CHR(13) + CHR(10), "<br />")
RETURN ALLTRIM(cData)


FUNCTION DocTagCode(cDataToTranslate)
   LOCAL cData := cDataToTranslate
   LOCAL cTag := ""

   //' CHECK IF THE ARGUMENT HAS AN EMPTY VALUE
   IF HB_ISNIL(cData)
      RETURN ""
   ELSEIF LEN(ALLTRIM(cData)) == 0
      RETURN ""
   END

   //' REPLACE SPECIAL CHARACTERS
   cData := STRTRAN(cData, "&", "&amp;")
   cData := STRTRAN(cData, "<", "&lt;")
   cData := STRTRAN(cData, ">", "&gt;")
   cData := STRTRAN(cData, "!EQL", "=")
   cData := STRTRAN(cData, "!HSH", "#")
   cData := STRTRAN(cData, "!GT", "&gt;")
   cData := STRTRAN(cData, "!LT", "&lt;")

   //' RESTORE ORIGINAL COMMENT TAGS
   cData := STRTRAN(cData, "&lt;!--", "<!--")
   cData := STRTRAN(cData, "--&gt;", "-->")

   //' FILTER OUT THE LINK-TAGS
   cData := STRTRAN(cData, "!LINK ", "")
   cData := STRTRAN(cData, "!ELINK ", "")
   cData := STRTRAN(cData, "!LINK", "")
   cData := STRTRAN(cData, "!ELINK", "")

   //' TRANSLATE ITALIC TEXT
   cTag := "!I"
   FOR i := 1 TO COUNTCHARS(cData, cTag)
      nTagStart := AT(cTag, cData) + LEN(cTag)
      nTagStop := AT_EXT("!EI", cData, nTagStart)
      cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
      cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<i>" + cTagText + "</i>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
   NEXT

   //' TRANSLATE BOLD TEXT
   cTag := "!B"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EB", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<strong>" + cTagText + "</strong>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   cData := STRTRAN(cData, " ", "&nbsp;")
   cData := STRTRAN(cData, CHR(13) + CHR(10), "<br />")
RETURN ALLTRIM(cData)


FUNCTION DocTagArguments(cDataToTranslate)
   LOCAL cData := cDataToTranslate
   LOCAL cTag := "", nTagCount := 0
   LOCAL i := 1, j := 1, nExtension := 0
   LOCAL cDataLine := "", cDataLines := "", cDataPreviousLine := ""
   LOCAL lExtension := .F., lInArgument := .F., lInTable := .F., lInCode := .F., lInDiv := .F.
   LOCAL nTagStart := 1, nTagStop := 1, cTagText := "", cTempString := ""
   LOCAL cTableAlign := ""

   //' CHECK IF THE ARGUMENT HAS AN EMPTY VALUE
   IF HB_ISNIL(cData)
      RETURN ""
   ELSEIF LEN(ALLTRIM(cData)) == 0
      RETURN ""
   ELSE
      //' RETURN cData
   END

   //' REPLACE SPECIAL CHARACTERS
   cData := STRTRAN(cData, "&", "&amp;")
   cData := STRTRAN(cData, "<", "&lt;")
   cData := STRTRAN(cData, ">", "&gt;")
   cData := STRTRAN(cData, "!EQL", "=")
   cData := STRTRAN(cData, "!HSH", "#")
   cData := STRTRAN(cData, "!GT", "&gt;")
   cData := STRTRAN(cData, "!LT", "&lt;")

   //' RESTORE ORIGINAL COMMENT TAGS
   cData := STRTRAN(cData, "&lt;!--", "<!--")
   cData := STRTRAN(cData, "--&gt;", "-->")

   //' FILTER OUT THE LINK-TAGS
   cData := STRTRAN(cData, "!LINK ", "")
   cData := STRTRAN(cData, "!ELINK ", "")
   cData := STRTRAN(cData, "!LINK", "")
   cData := STRTRAN(cData, "!ELINK", "")

   //' TRANSLATE ITALIC TEXT
   cTag := "!I"
   FOR i := 1 TO COUNTCHARS(cData, cTag)
      nTagStart := AT(cTag, cData) + LEN(cTag)
      nTagStop := AT_EXT("!EI", cData, nTagStart)
      cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
      cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<i>" + cTagText + "</i>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
   NEXT
   
   //' TRANSLATE BOLD TEXT
   cTag := "!B"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EB", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<strong>" + cTagText + "</strong>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   nTagStart := 1
   nTagStop := 1
   //' LOOP THROUGH ARGUMENT ITEMS
   FOR i := 1 TO (COUNTCHARS(cData, CHR(13) + CHR(10)) + 1)
      nTagStart := nTagStop
      nTagStop := AT_EXT(CHR(13) + CHR(10), cData, nTagStart)
      IF nTagStop == 0
         nTagStop := LEN(cData) + 1
      END
      cDataLine := SUBSTR(cData, nTagStart, nTagStop - nTagStart)

      IF LEFT(ALLTRIM(cDataLine), 2) == "!!"
         nTagStop := nTagStop + 2
         LOOP
      END

      IF LEFT(cDataLine, 6) == "!TABLE"
         lInTable := .T.
      ELSEIF lInTable == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</table>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInTable := .F.
      END

      IF LEFT(cDataLine, 5) == "!CODE"
         lInCode := .T.
      ELSEIF lInCode == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</div>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInCode := .F.
      END

      IF LEFT(cDataLine, 2) == "!P"
         lInDiv := .T.

         IF LEFT(cDataLine, 8) == "!P[note="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<i>" + SUBSTR(cDataLine, 9, AT("]", cDataLine) - 9) + "</i><br />"
         ELSEIF LEFT(cDataLine, 9) == "!P[title]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong style='font-size:13px;'>" + SUBSTR(cDataLine, 10) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 8) == "!P[bold]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong>" + SUBSTR(cDataLine, 9) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 10) == "!P[indent="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;margin-left:" + ALLTRIM(STR(VAL(SUBSTR(cDataLine, 11, AT("]", cDataLine) - 11)) * 4)) + "px;'>"
            IF LEN(ALLTRIM(cDataLine)) > (AT("]", cDataLine) + 1)
               cDataLines := cDataLines + SUBSTR(cDataLine, AT("]", cDataLine) + 1) + "<br />"
            END

            nTagStop := nTagStop + 2
            LOOP
         ELSE
            //cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
         END
      ELSEIF lInDiv == .T. .AND. ALLTRIM(cDataLine) == ""
         cDataLines := cDataLines + "</div><br />"
         lInDiv := .F.

         nTagStop := nTagStop + 2
         LOOP
      END

      IF LEFT(cDataLine, 4) == "!ARG"
         //' BEGIN OF AN ARGUMENT
         lInArgument := .T.

         IF LOWER(RIGHT(ALLTRIM(cDataLine), 15)) == "[xhb=extension]"
            lExtension := .T.
            cDataLine := STRTRAN(cDataLine, "[xhb=extension]", "")
         ELSE
            lExtension := .F.
         END

         cDataLines := cDataLines + ALLTRIM(STRTRAN(cDataLine, "!ARG", "")) + "<br />"
      ELSEIF LEFT(cDataLine, 4) == "!END" .AND. (lInTable == .F. .AND. lInCode == .F.)
         //' END OF AN ARGUMENT
         lInArgument := .F.

         IF lExtension == .T.
             cDataLines := cDataLines + "<div style='margin-top:2px;margin-bottom:0px;font-size:11px;'><i>This argument is an <a href='javascript:popup_ext();' style='font-size:11px;'>xHarbour Extension</a>.</i></div><br />"
             nExtension := .F.
         END
      ELSE
         //' INSIDE OR OUTSIDE ARGUMENT SECTION
         IF lInArgument == .T.
            IF lInTable == .T.
               IF LEFT(cDataLine, 6) == "!TABLE"
                  IF AT("]", cDataLine) > 0
                     cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("]", cDataLine) + 1)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
                     cTableAlign := ALLTRIM(STRTRAN(SUBSTR(cDataLine, AT("[", cDataLine) + 1, AT("]", cDataLine) - (AT("[", cDataLine) + 1)), "align=", ""))
                  ELSE
                     cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("!TABLE", cDataLine) + 6)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
                     cTableAlign := "L"
                  END
               ELSE
                  cDataLines := cDataLines + "<tr>"

                  FOR j := 1 TO LEN(cTableAlign)
                     IF cTableAlign[j] == "R"
                        cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:right;'>"
                     ELSEIF cTableAlign[j] == "C"
                        cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:center;'>"
                     ELSE
                        cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:left;'>"
                     END

                     IF AT("  ", cDataLine) > 0
                        cDataLines := cDataLines + SUBSTR(cDataLine, 1, AT("  ", cDataLine) - 1)
                        cDataLine := ALLTRIM(SUBSTR(cDataLine, AT("  ", cDataLine)))
                     ELSE
                        cDataLines := cDataLines + ALLTRIM(cDataLine)
                     END
                     cDataLines := cDataLines + "</td>"
                  NEXT

                  cDataLines := cDataLines + "</tr>"
               END
            ELSEIF lInCode == .T.
               IF LEFT(cDataLine, 5) == "!CODE"
                  cDataLines := cDataLines + "<div style='margin-top:12px;margin-bottom:12px;font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>"
               ELSE
                  cDataLines := cDataLines + " " + STRTRAN(cDataLine, " ", "&nbsp;") + "<br />"
               END
            ELSE
               cDataLines := cDataLines + " " + cDataLine
            END
         ELSE
            IF RIGHT(ALLTRIM(cDataLines), 12) <> "<br /><br />" .AND. RIGHT(ALLTRIM(cDataLines), 12) <> "</div><br />"
               cDataLines := cDataLines + "<br /><br />"
            END
         END
      END

      nTagStop := nTagStop + 2
   NEXT

   cDataLines := RemRight(cDataLines, "<br />")

   IF lInDiv == .T.
      cDataLines := cDataLines + "</div>"
   END

   cData := cDataLines
RETURN ALLTRIM(cData)


FUNCTION DocTagArticle(cDataToTranslate)
   LOCAL cData := cDataToTranslate
   LOCAL cTag := "", nTagCount := 0
   LOCAL i := 1, j := 1, nExtension := 0
   LOCAL cDataLine := "", cDataLines := "", cDataPreviousLine := ""
   LOCAL lExtension := .F., lInArgument := .F., lInTable := .F., lInCode := .F., lInDiv := .F.
   LOCAL nTagStart := 1, nTagStop := 1, cTagText := "", cTempString := ""
   LOCAL cTableAlign := ""

   //' CHECK IF THE ARGUMENT HAS AN EMPTY VALUE
   IF HB_ISNIL(cData)
      RETURN ""
   ELSEIF LEN(ALLTRIM(cData)) == 0
      RETURN ""
   END

   //' REPLACE SPECIAL CHARACTERS
   cData := STRTRAN(cData, "&", "&amp;")
   cData := STRTRAN(cData, "<", "&lt;")
   cData := STRTRAN(cData, ">", "&gt;")
   cData := STRTRAN(cData, "!EQL", "=")
   cData := STRTRAN(cData, "!HSH", "#")
   cData := STRTRAN(cData, "!GT", "&gt;")
   cData := STRTRAN(cData, "!LT", "&lt;")

   //' RESTORE ORIGINAL COMMENT TAGS
   cData := STRTRAN(cData, "&lt;!--", "<!--")
   cData := STRTRAN(cData, "--&gt;", "-->")

   //' FILTER OUT THE LINK-TAGS
   cData := STRTRAN(cData, "!LINK ", "")
   cData := STRTRAN(cData, "!ELINK ", "")
   cData := STRTRAN(cData, "!LINK", "")
   cData := STRTRAN(cData, "!ELINK", "")

   //' TRANSLATE ITALIC TEXT
   cTag := "!I"
   FOR i := 1 TO COUNTCHARS(cData, cTag)
      nTagStart := AT(cTag, cData) + LEN(cTag)
      nTagStop := AT_EXT("!EI", cData, nTagStart)
      cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
      cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<i>" + cTagText + "</i>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
   NEXT

   //' TRANSLATE BOLD TEXT
   cTag := "!B"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EB", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<strong>" + cTagText + "</strong>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   nTagStart := 1
   nTagStop := 1
   //' LOOP THROUGH ARGUMENT ITEMS
   FOR i := 1 TO (COUNTCHARS(cData, CHR(13) + CHR(10)) + 1)
      nTagStart := nTagStop
      nTagStop := AT_EXT(CHR(13) + CHR(10), cData, nTagStart)
      IF nTagStop == 0
         nTagStop := LEN(cData) + 1
      END
      cDataLine := SUBSTR(cData, nTagStart, nTagStop - nTagStart)

      IF LEFT(ALLTRIM(cDataLine), 2) == "!!"
         nTagStop := nTagStop + 2
         LOOP
      END

      IF LEFT(cDataLine, 6) == "!TABLE"
         lInTable := .T.
      ELSEIF lInTable == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</table>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInTable := .F.
      END

      IF LEFT(cDataLine, 5) == "!CODE"
         lInCode := .T.
      ELSEIF lInCode == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</div>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInCode := .F.
      END

      IF LEFT(cDataLine, 2) == "!P"
         lInDiv := .T.

         IF LEFT(cDataLine, 8) == "!P[note="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<i>" + SUBSTR(cDataLine, 9, AT("]", cDataLine) - 9) + "</i><br />"
         ELSEIF LEFT(cDataLine, 9) == "!P[title]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong style='font-size:13px;'>" + SUBSTR(cDataLine, 10) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 8) == "!P[bold]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong>" + SUBSTR(cDataLine, 9) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 10) == "!P[indent="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;margin-left:" + ALLTRIM(STR(VAL(SUBSTR(cDataLine, 11, AT("]", cDataLine) - 11)) * 4)) + "px;'>"
            IF LEN(ALLTRIM(cDataLine)) > (AT("]", cDataLine) + 1)
               cDataLines := cDataLines + SUBSTR(cDataLine, AT("]", cDataLine) + 1) + "<br />"
            END

            nTagStop := nTagStop + 2
            LOOP
         ELSE
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
         END
      ELSEIF lInDiv == .T. .AND. ALLTRIM(cDataLine) == ""
         cDataLines := cDataLines + "</div><br />"
         lInDiv := .F.

         nTagStop := nTagStop + 2
         LOOP
      END

      IF UPPER(LEFT(cDataLine, 9)) == "!HEADING1"
         cDataLine := "<div style='font-size:16px;font-weight:bold;margin-bottom:6px;'>" + ALLTRIM(SUBSTR(cDataLine, 11)) + "</div>"
      ELSEIF UPPER(LEFT(cDataLine, 9)) == "!HEADING2"
         cDataLine := "<div style='font-size:14px;font-weight:bold;margin-bottom:4px;'>" + ALLTRIM(SUBSTR(cDataLine, 11)) + "</div>"
      ELSEIF UPPER(LEFT(cDataLine, 9)) == "!HEADING3"
         cDataLine := "<div style='font-size:12px;font-weight:bold;margin-bottom:2px;'>" + ALLTRIM(SUBSTR(cDataLine, 11)) + "</div>"
      END

      IF lInTable == .T.
         IF LEFT(cDataLine, 6) == "!TABLE"
            IF AT("]", cDataLine) > 0
               cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("]", cDataLine) + 1)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
               cTableAlign := ALLTRIM(STRTRAN(SUBSTR(cDataLine, AT("[", cDataLine) + 1, AT("]", cDataLine) - (AT("[", cDataLine) + 1)), "align=", ""))
            ELSE
               cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("!TABLE", cDataLine) + 6)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
               cTableAlign := "L"
            END
         ELSE
            cDataLines := cDataLines + "<tr>"

            FOR j := 1 TO LEN(cTableAlign)
               IF cTableAlign[j] == "R"
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:right;'>"
               ELSEIF cTableAlign[j] == "C"
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:center;'>"
               ELSE
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:left;'>"
               END

               IF AT("  ", cDataLine) > 0
                  cDataLines := cDataLines + SUBSTR(cDataLine, 1, AT("  ", cDataLine) - 1)
                  cDataLine := ALLTRIM(SUBSTR(cDataLine, AT("  ", cDataLine)))
               ELSE
                  cDataLines := cDataLines + ALLTRIM(cDataLine)
               END
               cDataLines := cDataLines + "</td>"
            NEXT

            cDataLines := cDataLines + "</tr>"
         END
      ELSEIF lInCode == .T.
         IF LEFT(cDataLine, 5) == "!CODE"
            cDataLines := cDataLines + "<div style='margin-top:12px;margin-bottom:12px;font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>"
         ELSE
            cDataLines := cDataLines + " " + STRTRAN(cDataLine, " ", "&nbsp;") + "<br />"
         END
      ELSE
         IF LEN(ALLTRIM(cDataLine)) == 0
            IF RIGHT(ALLTRIM(cDataLines), 12) <> "<br /><br />" .AND. RIGHT(ALLTRIM(cDataLines), 12) <> "</div><br />"
               cDataLines := cDataLines + "<br /><br />"
            END
         ELSE
            cDataLines := cDataLines + " " + cDataLine
         END
      END

      nTagStop := nTagStop + 2
   NEXT

   cData := RemRight(cDataLines, "<br />")
RETURN ALLTRIM(cData)


FUNCTION DocTagDescription(cDataToTranslate)
   LOCAL cData := cDataToTranslate
   LOCAL cTag := "", nTagCount := 0
   LOCAL i := 1, j := 1, nExtension := 0
   LOCAL cDataLine := "", cDataLines := "", cDataPreviousLine := ""
   LOCAL lExtension := .F., lInArgument := .F., lInTable := .F., lInCode := .F., lInDiv := .F.
   LOCAL nTagStart := 1, nTagStop := 1, cTagText := "", cTempString := ""
   LOCAL cTableAlign := ""

   //' CHECK IF THE ARGUMENT HAS AN EMPTY VALUE
   IF HB_ISNIL(cData)
      RETURN ""
   ELSEIF LEN(ALLTRIM(cData)) == 0
      RETURN ""
   ELSE
      //' RETURN cData
   END

   // cData := DocTagGeneral(cData)

   //' REPLACE SPECIAL CHARACTERS
   cData := STRTRAN(cData, "&", "&amp;")
   cData := STRTRAN(cData, "<", "&lt;")
   cData := STRTRAN(cData, ">", "&gt;")
   cData := STRTRAN(cData, "!EQL", "=")
   cData := STRTRAN(cData, "!HSH", "#")
   cData := STRTRAN(cData, "!GT", "&gt;")
   cData := STRTRAN(cData, "!LT", "&lt;")

   //' RESTORE ORIGINAL COMMENT TAGS
   cData := STRTRAN(cData, "&lt;!--", "<!--")
   cData := STRTRAN(cData, "--&gt;", "-->")

   //' FILTER OUT THE LINK-TAGS
   cData := STRTRAN(cData, "!LINK ", "")
   cData := STRTRAN(cData, "!ELINK ", "")
   cData := STRTRAN(cData, "!LINK", "")
   cData := STRTRAN(cData, "!ELINK", "")

   //' TRANSLATE ITALIC TEXT
   cTag := "!I"
   FOR i := 1 TO COUNTCHARS(cData, cTag)
      nTagStart := AT(cTag, cData) + LEN(cTag)
      nTagStop := AT_EXT("!EI", cData, nTagStart)
      cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
      cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<i>" + cTagText + "</i>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
   NEXT

   //' TRANSLATE BOLD TEXT
   cTag := "!B"
   IF COUNTCHARS(cData, cTag) > 0
      FOR i := 1 TO COUNTCHARS(cData, cTag)
         nTagStart := AT(cTag, cData) + LEN(cTag)
         nTagStop := AT_EXT("!EB", cData, nTagStart)
         cTagText := SUBSTR(cData, nTagStart, nTagStop - nTagStart)
         cData := LEFT(cData, nTagStart - (LEN(cTag) + 1)) + "<strong>" + cTagText + "</strong>" + RIGHT(cData, LEN(cData) - nTagStop - 2)
      NEXT
   END

   nTagStart := 1
   nTagStop := 1
   //' LOOP THROUGH ARGUMENT ITEMS
   FOR i := 1 TO (COUNTCHARS(cData, CHR(13) + CHR(10)) + 1)
      nTagStart := nTagStop
      nTagStop := AT_EXT(CHR(13) + CHR(10), cData, nTagStart)
      IF nTagStop == 0
         nTagStop := LEN(cData) + 1
      END
      cDataLine := SUBSTR(cData, nTagStart, nTagStop - nTagStart)

      IF LEFT(ALLTRIM(cDataLine), 2) == "!!"
         nTagStop := nTagStop + 2
         LOOP
      END

      IF LEFT(cDataLine, 6) == "!TABLE"
         lInTable := .T.
      ELSEIF lInTable == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</table>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInTable := .F.
      END

      IF LEFT(cDataLine, 5) == "!CODE"
         lInCode := .T.
      ELSEIF lInCode == .T. .AND. LEFT(cDataLine, 4) == "!END"
         cDataLines := cDataLines + "</div>"
         cDataLine := ALLTRIM(STRTRAN(cDataLine, "!END", ""))
         lInCode := .F.
      END

      IF LEFT(cDataLine, 2) == "!P"
         lInDiv := .T.

         IF LEFT(cDataLine, 8) == "!P[note="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<i>" + SUBSTR(cDataLine, 9, AT("]", cDataLine) - 9) + "</i><br />"
         ELSEIF LEFT(cDataLine, 9) == "!P[title]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong style='font-size:13px;'>" + SUBSTR(cDataLine, 10) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 8) == "!P[bold]"
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
            cDataLine := "<strong>" + SUBSTR(cDataLine, 9) + "</strong><br />"
         ELSEIF LEFT(cDataLine, 10) == "!P[indent="
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;margin-left:" + ALLTRIM(STR(VAL(SUBSTR(cDataLine, 11, AT("]", cDataLine) - 11)) * 4)) + "px;'>"
            IF LEN(ALLTRIM(cDataLine)) > (AT("]", cDataLine) + 1)
               cDataLines := cDataLines + SUBSTR(cDataLine, AT("]", cDataLine) + 1) + "<br />"
            END

            nTagStop := nTagStop + 2
            LOOP
         ELSE
            cDataLines := cDataLines + "<div style='margin-top:0px;margin-bottom:0px;'>"
         END
      ELSEIF lInDiv == .T. .AND. ALLTRIM(cDataLine) == ""
         cDataLines := cDataLines + "</div><br />"
         lInDiv := .F.

         nTagStop := nTagStop + 2
         LOOP
      END

      IF lInTable == .T.
         IF LEFT(cDataLine, 6) == "!TABLE"
            IF AT("]", cDataLine) > 0
               cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("]", cDataLine) + 1)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
               cTableAlign := ALLTRIM(STRTRAN(SUBSTR(cDataLine, AT("[", cDataLine) + 1, AT("]", cDataLine) - (AT("[", cDataLine) + 1)), "align=", ""))
            ELSE
               cDataLines := cDataLines + "<div style='margin-top:8px;margin-bottom:2px;'><i><strong>Table: </strong>" + ALLTRIM(SUBSTR(cDataLine, AT("!TABLE", cDataLine) + 6)) + "</i></div><table border='0' cellpadding='2' cellspacing='0' style='border-top:1px solid #c0c0c0;border-bottom:1px solid #c0c0c0;' width='100%'>"
               cTableAlign := "L"
            END
         ELSE
            cDataLines := cDataLines + "<tr>"

            FOR j := 1 TO LEN(cTableAlign)
               IF cTableAlign[j] == "R"
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:right;'>"
               ELSEIF cTableAlign[j] == "C"
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:center;'>"
               ELSE
                  cDataLines := cDataLines + "<td style='font-family:verdana;font-size:11px;text-align:left;'>"
               END

               IF AT("  ", cDataLine) > 0
                  cDataLines := cDataLines + SUBSTR(cDataLine, 1, AT("  ", cDataLine) - 1)
                  cDataLine := ALLTRIM(SUBSTR(cDataLine, AT("  ", cDataLine)))
               ELSE
                  cDataLines := cDataLines + ALLTRIM(cDataLine)
               END
               cDataLines := cDataLines + "</td>"
            NEXT

            cDataLines := cDataLines + "</tr>"
         END
      ELSEIF lInCode == .T.
         IF LEFT(cDataLine, 5) == "!CODE"
            cDataLines := cDataLines + "<div style='margin-top:12px;margin-bottom:12px;font-family:courier new;font-size:12px;border:1px solid #b0b0b0;background-color:#e0e0e0;padding:3px;'>"
         ELSE
            cDataLines := cDataLines + " " + STRTRAN(cDataLine, " ", "&nbsp;") + "<br />"
         END
      ELSE
         IF LEN(cDataLine) == 0
            IF RIGHT(ALLTRIM(cDataLines), 12) <> "<br /><br />" .AND. RIGHT(ALLTRIM(cDataLines), 12) <> "</div><br />"
               cDataLines := cDataLines + "<br /><br />"
            ELSE
               cDataLines := cDataLines + " " + cDataLine
            END
         ELSE
            cDataLines := cDataLines + " " + cDataLine
         END
      END

      nTagStop := nTagStop + 2
   NEXT

   IF lInDiv == .T.
      cDataLines := cDataLines + "</div>"
   END

   cData := cDataLines
RETURN ALLTRIM(cData)