/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   Utility functions for HTML LIB
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "common.ch"
#include "error.ch"

GLOBAL oCGI

/*
  SplitUrl( cUrl ) --> hUrl
  (C) 2006 Francesco Saverio Giudice

  Splits a valid URL into simple components and return them in a hash
  it works like parse_url() PHP function

  a URL string is something like this:
  http://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

  Parameters:
  cUrl     -   Valid URL string

  Returns:
  hUrl     -   Hash containing these keys:
               SCHEME   - protocol name
               HOST     - hostname
               PORT     - protocol port number
               USER     - username
               PASS     - password
               PATH     - path to directory and/or file
               QUERY    - part after question mark ?
               FRAGMENT - part after hashmark #

*/
FUNCTION SplitUrl( cUrl )
   LOCAL hUrl := Hash()
   LOCAL nPos, cTemp, cUserNamePassword, cHostnamePort
   LOCAL cProto, cHost, cPort, nPort, cUser, cPass, cPath, cQuery, cFragment

   // Prevents case matching
   HSetCaseMatch( hUrl, FALSE )

   cTemp := cUrl

   // Starting with
   // http://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read protocol
   nPos := At( "://", cTemp )
   IF nPos > 0
      cProto := Left( cTemp, nPos - 1 )
      // delete protocol from temp string
      cTemp := SubStr( cTemp, nPos + 3 )
   ELSE
      cProto := ""
   ENDIF

   // Now we have:
   // [username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read username and password
   nPos := At( "@", cTemp )
   IF nPos > 0
      cUserNamePassword := Left( cTemp, nPos - 1 )
      // delete Username and Password from temp string
      cTemp := SubStr( cTemp, nPos + 1 )
      // Split username and password
      nPos := At( ":", cUserNamePassword )
      IF nPos > 0
         cUser := Left( cUserNamePassword, nPos - 1 )
         cPass := SubStr( cUserNamePassword, nPos + 1 )
      ELSE
         cUser := cUserNamePassword
         cPass := ""
      ENDIF
   ELSE
      cUser := ""
      cPass := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Search for anchor using # char from right
   nPos := RAt( "#", cTemp )
   IF nPos > 0
      cFragment := SubStr( cTemp, nPos + 1 )

      // delete anchor from temp string
      cTemp := SubStr( cTemp, 1, nPos - 1 )

   ELSE
      cFragment := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]]]

   // Search for Query part using ? char from right
   nPos := RAt( "?", cTemp )
   IF nPos > 0
      cQuery := SubStr( cTemp, nPos + 1 )

      // delete query from temp string
      cTemp := SubStr( cTemp, 1, nPos - 1 )

   ELSE
      cQuery := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]]

   // Search for Path part using / char from right
   nPos := RAt( "/", cTemp )
   IF nPos > 0
      cPath := SubStr( cTemp, nPos )

      // delete path from temp string
      cTemp := SubStr( cTemp, 1, nPos - 1 )

   ELSE
      cPath := "/"
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]]

   cHostnamePort := cTemp

   // Searching port number
   nPos := At( ":", cHostnamePort )
   IF nPos > 0
      cHost := Left( cHostnamePort, nPos - 1 )
      cPort := SubStr( cHostnamePort, nPos + 1 )
      nPort := Val( cPort )
      IF nPort <= 0
         nPort := -1
      ENDIF
   ELSE
      cHost := cHostnamePort
      cPort := ""
      nPort := -1
   ENDIF

   // Assemble hash
   WITH OBJECT hUrl
        :SCHEME   := cProto
        :HOST     := cHost
        :PORT     := nPort
        :USER     := cUser
        :PASS     := cPass
        :PATH     := cPath
        :QUERY    := cQuery
        :FRAGMENT := cFragment
   END

   // Prevents externals to add something else to this Hash
   HSetAutoAdd( hUrl, FALSE )
RETURN hUrl


/*
  SplitString( cString ) --> aLines
  (C) 2006 Francesco Saverio Giudice

  Splits a string into simple components and return them in an array

  Parameters:
  cString     -   Initial string
  cDelim      -   Delimiter - default CRLF
  lRemDelim   -   Remove delimiter from return values - default TRUE

  Returns:
  aLines      -   Array with lines / fields for each element

  Sample:
  SplitString( "this=is=a=line=with=equals", "=" ) -> { "this", "is", "a", "line", "with", "equals" }

*/
FUNCTION SplitString( cString, cDelim, lRemDelim )
   LOCAL nEOLPos
   LOCAL cBuffer := cString
   LOCAL aLines  := {}, cLine

   DEFAULT cDelim    TO ( CHR(13) + CHR(10) )
   DEFAULT lRemDelim TO TRUE

   DO WHILE ( nEOLPos := AT( cDelim, cBuffer ) ) > 0
      IF lRemDelim
         cLine := LEFT( cBuffer, nEOLPos - 1 )
      ELSE
         cLine := LEFT( cBuffer, ( nEOLPos + LEN( cDelim ) ) - 1 )
      ENDIF
      //TraceLog( "cBuffer, cDelim, nEOLPos, cLine", cBuffer, cDelim, nEOLPos, cLine )
      aAdd( aLines, cLine )
      cBuffer := SubStr( cBuffer, nEOLPos + LEN( cDelim ) )
   ENDDO

   // Check last line
   IF Len( cBuffer ) > 0
      aAdd( aLines, cBuffer )
   ENDIF

RETURN aLines

************************************************************
* Decoding URL
* Can return both a string or a number
*
FUNCTION URLDecode( cStr )
   LOCAL cRet := "", i, cCar
   LOCAL lNumeric := .T.

   FOR i := 1 TO Len( cStr )
      cCar := cStr[i]
      DO CASE

         CASE cCar == "+"
            cRet += " "

         CASE cCar == "%"
            i ++
            cRet += Chr( HB_HexToNum( SubStr( cStr, i, 2 ) ) )
            i ++

         OTHERWISE
            cRet += cCar

      ENDCASE

      IF (cRet[i] > "9" .or. cRet[i] < "0") .and. cRet[i] != "."
         lNumeric := .F.
      ENDIF
   NEXT

   *IF lNumeric
   *   cRet := Val( cRet )
   *ENDIF

RETURN cRet


************************************************************
* Encoding URL
* Can return both a string or a number
*
FUNCTION URLEncode( cStr )
   LOCAL cRet := "", i, nVal, cCar

   FOR i := 1 TO Len( cStr )
      cCar := cStr[i]
      DO CASE

         CASE cCar == " "
              cRet += "+"

         CASE cCar >= "A" .and. cCar <= "Z"
            cRet += cCar

         CASE cCar >= "a" .and. cCar <= "z"
            cRet += cCar

         CASE cCar >= "0" .and. cCar <= "9"
            cRet += cCar

         OTHERWISE
            nVal := Asc( cCar )
            cRet += "%" + HB_NumToHex( nVal )
      ENDCASE
   NEXT

RETURN cRet

/*
 * DateToGMT( dDate, cTime, nDayToAdd ) --> cGMTDate
 *
 * dDate     : default DATE()
 * cTime     : default "00:00:00"
 * nDayToAdd : default 0 - may be a negative number
 *
 * cGMTDate  : The string return in form of "Saturday, 31-Oct-03 00:00:00 GMT"
 */

FUNCTION DateToGMT( dDate, cTime, nDayToAdd, nSecsToAdd )
  LOCAL cStr := ""
  LOCAL cOldDateFormat := Set( _SET_DATEFORMAT, "dd-mm-yy" )
  LOCAL nDay, nMonth, nYear, nDoW
  LOCAL aDays   := { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" }
  LOCAL aMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

  DEFAULT dDate      TO DATE()
  DEFAULT cTime      TO TIME()
  DEFAULT nDayToAdd  TO 0
  DEFAULT nSecsToAdd TO 0

  //Tracelog( "DateToGMT - StartingValue", dDate, cTime, nDayToAdd, nSecsToAdd )

  cTime := AddSecondsToTime( cTime, nSecsToAdd, @nDayToAdd )
  dDate += nDayToAdd

  nDay   := Day( dDate )
  nMonth := Month( dDate )
  nYear  := Year( dDate)
  nDoW   := Dow( dDate )

  cStr := aDays[ nDow ] + ", " + StrZero( nDay, 2 ) + "-" + aMonths[ nMonth ] + "-" + ;
          Right( StrZero( nYear, 4 ), 2 ) + " " + cTime + " GMT"

  //Tracelog( "DateToGMT", cStr )

  Set( _SET_DATEFORMAT, cOldDateFormat )

RETURN cStr

/*
 * AddSecondsToTime( cTime, nSecsToAdd, @nDaysAdded ) --> cNewTime
 *
 * cTime      : default "00:00:00"
 * nSecsToAdd : default 0 - may be a negative number
 * nDaysAdded : (out) return how many days add (or subtract) to actual date if numbers seconds is
 *                    more than 86400 seconds (1 day)
 *
 * cNewTime   : The new time string
 *
 * Rules: time is converted to seconds from midnight, then added of nSecsToAdd. Divided of 1 day and
 *        then reverted to Time string
 */

FUNCTION AddSecondsToTime( cTime, nSecsToAdd, nDaysAdded )
  LOCAL nOneDaySeconds := 86400  // 24 * 60 * 60
  LOCAL cNewTime, nSecs

  DEFAULT cTime      TO TIME()
  DEFAULT nSecsToAdd TO 0
  DEFAULT nDaysAdded TO 0      // nDaysAdded can be already valued, so below i add to this value

  IF nSecsToAdd <> 0
     nSecs      := TimeAsSeconds( cTime ) + nSecsToAdd
     nDaysAdded += Int( nSecs / nOneDaySeconds )  // Attention! nDaysAdded can be already filled
     nSecs      := nSecs - nDaysAdded
     cNewTime   := TimeAsString( nSecs )
  ELSE
     cNewTime := cTime
  ENDIF

RETURN cNewTime

FUNCTION TimeDiffAsSeconds( dDateStart, dDateEnd, cTimeStart, cTimeEnd )
  LOCAL aRetVal

  DEFAULT dDateEnd     TO DATE()
  DEFAULT cTimeEnd     TO TIME()

  aRetVal := FT_ELAPSED( dDateStart, dDateEnd, cTimeStart, cTimeEnd )

RETURN aRetVal[ 4, 2 ]

FUNCTION OutputString( cString, aTranslate, lProtected )
  LOCAL cHtml := ""
  DEFAULT lProtected TO FALSE
  DEFAULT aTranslate TO { { '"', '&quot;' }, { ' ', '&nbsp;' } }

  //TraceLog( "OutputString( cString, aTranslate, lProtected )", cString, aTranslate, lProtected )
  IF lProtected
     cHtml := HtmlSpecialChars( cString )
  ELSE
     cHtml := TranslateStrings( cString, aTranslate )
  ENDIF
  //TraceLog( "OutputString() = cHtml", cHtml )

RETURN cHtml

FUNCTION HtmlSpecialChars( cString, cQuote_style )
  LOCAL aTranslations := { ;
                           { '&', '&amp;' } ,;
                           { '<', '&lt;'  } ,;
                           { '>', '&gt;'  }  ;
                         }
RETURN HtmlConvertChars( cString, cQuote_style, aTranslations )

FUNCTION HtmlConvertChars( cString, cQuote_style, aTranslations )
  DEFAULT cQuote_style TO "ENT_COMPAT"
  DO CASE
     CASE cQuote_style == "ENT_COMPAT"
          aAdd( aTranslations, { '"', '&quot;'  } )
     CASE cQuote_style == "ENT_QUOTES"
          aAdd( aTranslations, { '"', '&quot;'  } )
          aAdd( aTranslations, { "'", '&#039;'  } )
     CASE cQuote_style == "ENT_NOQUOTES"
  ENDCASE
RETURN TranslateStrings( cString, aTranslations )

FUNCTION CRLF2BR( cString )
  LOCAL aTranslations := { ;
                           { CRLF(), '<br />' } ;
                         }
RETURN TranslateStrings( cString, aTranslations )

FUNCTION TranslateStrings( cString, aTranslate )
  LOCAL aTran
  FOR EACH aTran IN aTranslate
      IF aTran[1] IN cString
         cString := StrTran( cString, aTran[1], aTran[2] )
      ENDIF
  NEXT
RETURN cString

FUNCTION StrStr( cString, cSearch )
  LOCAL nPos := AT( cSearch, cString )
  LOCAL cVal := IIF( nPos > 0, SubStr( cString, nPos ), NIL )
RETURN cVal

FUNCTION StrIStr( cString, cSearch )
RETURN StrStr( Upper( cSearch ), Upper( cString ) )

FUNCTION HtmlEntities( cString, cQuote_style )
//  LOCAL aTranslations := { ;  // ATTENTION, this chars are visible only with OEM font
//                           { ' ', '&#160;' } ,; // &nbsp;   Nonbreaking space
//                           { '≠', '&#161;' } ,; // &iexcl;  Inverted exclamation
//                           { 'Ω', '&#162;' } ,; // &cent;   Cent sign
//                           { 'ú', '&#163;' } ,; // &pound;  Pound sterling
//                           { 'œ', '&#164;' } ,; // &curren; General currency sign
//                           { 'æ', '&#165;' } ,; // &yen;    Yen sign
//                           { '›', '&#166;' } ,; // &brvbar; or &brkbar; Broken vertical bar
//                           { 'ı', '&#167;' } ,; // &sect;   Section sign
//                           { '˘', '&#168;' } ,; // &uml; or &die; Diëresis / Umlaut
//                           { '∏', '&#169;' } ,; // &copy;   Copyright
//                           { '¶', '&#170;' } ,; // &ordf;   Feminine ordinal
//                           { 'Æ', '&#171;' } ,; // &laquo;  Left angle quote, guillemet left
//                           { '™', '&#172;' } ,; // &not     Not sign
//                           { '', '&#173;' } ,; // &shy;    Soft hyphen
//                           { '©', '&#174;' } ,; // &reg;    Registered trademark
//                           { 'Ó', '&#175;' } ,; // &macr; or &hibar; Macron accent
//                           { '¯', '&#176;' } ,; // &deg;    Degree sign
//                           { 'Ò', '&#177;' } ,; // &plusmn; Plus or minus
//                           { '˝', '&#178;' } ,; // &sup2;   Superscript two
//                           { '¸', '&#179;' } ,; // &sup3;   Superscript three
//                           { 'Ô', '&#180;' } ,; // &acute;  Acute accent
//                           { 'Ê', '&#181;' } ,; // &micro;  Micro sign
//                           { 'Ù', '&#182;' } ,; // &para;   Paragraph sign
//                           { '˙', '&#183;' } ,; // &middot; Middle dot
//                           { '˜', '&#184;' } ,; // &cedil;  Cedilla
//                           { '˚', '&#185;' } ,; // &sup1;   Superscript one
//                           { 'ß', '&#186;' } ,; // &ordm;   Masculine ordinal
//                           { 'Ø', '&#187;' } ,; // &raquo;  Right angle quote, guillemet right
//                           { '¨', '&#188;' } ,; // &frac14; Fraction one-fourth
//                           { '´', '&#189;' } ,; // &frac12; Fraction one-half
//                           { 'Û', '&#190;' } ,; // &frac34; Fraction three-fourths
//                           { '®', '&#191;' } ,; // &iquest; Inverted question mark
//                           { '∑', '&#192;' } ,; // &Agrave; Capital A, grave accent
//                           { 'µ', '&#193;' } ,; // &Aacute; Capital A, acute accent
//                           { '∂', '&#194;' } ,; // &Acirc;  Capital A, circumflex
//                           { '«', '&#195;' } ,; // &Atilde; Capital A, tilde
//                           { 'é', '&#196;' } ,; // &Auml;   Capital A, diëresis / umlaut
//                           { 'è', '&#197;' } ,; // &Aring;  Capital A, ring
//                           { 'í', '&#198;' } ,; // &AElig;  Capital AE ligature
//                           { 'Ä', '&#199;' } ,; // &Ccedil; Capital C, cedilla
//                           { '‘', '&#200;' } ,; // &Egrave; Capital E, grave accent
//                           { 'ê', '&#201;' } ,; // &Eacute; Capital E, acute accent
//                           { '“', '&#202;' } ,; // &Ecirc;  Capital E, circumflex
//                           { '”', '&#203;' } ,; // &Euml;   Capital E, diëresis / umlaut
//                           { 'ﬁ', '&#204;' } ,; // &Igrave; Capital I, grave accent
//                           { '÷', '&#205;' } ,; // &Iacute; Capital I, acute accent
//                           { '◊', '&#206;' } ,; // &Icirc;  Capital I, circumflex
//                           { 'ÿ', '&#207;' } ,; // &Iuml;   Capital I, diëresis / umlaut
//                           { '—', '&#208;' } ,; // &ETH;    Capital Eth, Icelandic
//                           { '•', '&#209;' } ,; // &Ntilde; Capital N, tilde
//                           { '„', '&#210;' } ,; // &Ograve; Capital O, grave accent
//                           { '‡', '&#211;' } ,; // &Oacute; Capital O, acute accent
//                           { '‚', '&#212;' } ,; // &Ocirc;  Capital O, circumflex
//                           { 'Â', '&#213;' } ,; // &Otilde; Capital O, tilde
//                           { 'ô', '&#214;' } ,; // &Ouml;   Capital O, diëresis / umlaut
//                           { 'û', '&#215;' } ,; // &times;  Multiply sign
//                           { 'ù', '&#216;' } ,; // &Oslash; Capital O, slash
//                           { 'Î', '&#217;' } ,; // &Ugrave; Capital U, grave accent
//                           { 'È', '&#218;' } ,; // &Uacute; Capital U, acute accent
//                           { 'Í', '&#219;' } ,; // &Ucirc;  Capital U, circumflex
//                           { 'ö', '&#220;' } ,; // &Uuml;   Capital U, diëresis / umlaut
//                           { 'Ì', '&#221;' } ,; // &Yacute; Capital Y, acute accent
//                           { 'Ë', '&#222;' } ,; // &THORN;  Capital Thorn, Icelandic
//                           { '·', '&#223;' } ,; // &szlig;  Small sharp s, German sz
//                           { 'Ö', '&#224;' } ,; // &agrave; Small a, grave accent
//                           { '†', '&#225;' } ,; // &aacute; Small a, acute accent
//                           { 'É', '&#226;' } ,; // &acirc;  Small a, circumflex
//                           { '∆', '&#227;' } ,; // &atilde; Small a, tilde
//                           { 'Ñ', '&#228;' } ,; // &auml;   Small a, diëresis / umlaut
//                           { 'Ü', '&#229;' } ,; // &aring;  Small a, ring
//                           { 'ë', '&#230;' } ,; // &aelig;  Small ae ligature
//                           { 'á', '&#231;' } ,; // &ccedil; Small c, cedilla
//                           { 'ä', '&#232;' } ,; // &egrave; Small e, grave accent
//                           { 'Ç', '&#233;' } ,; // &eacute; Small e, acute accent
//                           { 'à', '&#234;' } ,; // &ecirc;  Small e, circumflex
//                           { 'â', '&#235;' } ,; // &euml;   Small e, diëresis / umlaut
//                           { 'ç', '&#236;' } ,; // &igrave; Small i, grave accent
//                           { '°', '&#237;' } ,; // &iacute; Small i, acute accent
//                           { 'å', '&#238;' } ,; // &icirc;  Small i, circumflex
//                           { 'ã', '&#239;' } ,; // &iuml;   Small i, diëresis / umlaut
//                           { '–', '&#240;' } ,; // &eth;    Small eth, Icelandic
//                           { '§', '&#241;' } ,; // &ntilde; Small n, tilde
//                           { 'ï', '&#242;' } ,; // &ograve; Small o, grave accent
//                           { '¢', '&#243;' } ,; // &oacute; Small o, acute accent
//                           { 'ì', '&#244;' } ,; // &ocirc;  Small o, circumflex
//                           { '‰', '&#245;' } ,; // &otilde; Small o, tilde
//                           { 'î', '&#246;' } ,; // &ouml;   Small o, diëresis / umlaut
//                           { 'ˆ', '&#247;' } ,; // &divide; Division sign
//                           { 'õ', '&#248;' } ,; // &oslash; Small o, slash
//                           { 'ó', '&#249;' } ,; // &ugrave; Small u, grave accent
//                           { '£', '&#250;' } ,; // &uacute; Small u, acute accent
//                           { 'ñ', '&#251;' } ,; // &ucirc;  Small u, circumflex
//                           { 'Å', '&#252;' } ,; // &uuml;   Small u, diëresis / umlaut
//                           { 'Ï', '&#253;' } ,; // &yacute; Small y, acute accent
//                           { 'Á', '&#254;' } ,; // &thorn;  Small thorn, Icelandic
//                           { 'ò', '&#255;' }  ; // &yuml;   Small y, diëresis / umlaut
//                         }

  LOCAL aTranslations := {}
  LOCAL i
  FOR i := 160 TO 255
      aAdd( aTranslations, { Chr( i ), "&#" + Str( i, 3 ) + ";" } )
  NEXT

RETURN HtmlConvertChars( cString, cQuote_style, aTranslations )

PROCEDURE Die( cError )
  LOCAL oErr
  IF cError <> NIL //THEN OutStd( cError )
     //__OutDebug( "cError: ", cError )
     IF !oCGI:HeaderSent()
        oCGI:WriteLN( CRLF2BR( cError ), CRLF2BR( CRLF() ) )
        //oCGI:WriteLN( CRLF2BR( hb_dumpVar(TConfigure():hConfig) ) )
     ENDIF
     // Generate Error
     oErr := ErrorNew()
     oErr:severity    := ES_ERROR
     oErr:genCode     := EG_LIMIT
     oErr:subSystem   := "HBHTMLLIB"
     oErr:subCode     := 0
     oErr:description := cError
     oErr:canRetry    := .F.
     oErr:canDefault  := .F.
     oErr:fileName    := ""
     oErr:osCode      := 0
     Eval(ErrorBlock(), oErr)
     //QUIT
  ELSE
     QUIT
  ENDIF
RETURN

FUNCTION HTMLSpace( n )
RETURN replicate( "&nbsp;", n )  //"&#32;"
