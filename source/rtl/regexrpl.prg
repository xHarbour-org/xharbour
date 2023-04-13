/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * hb_RegexReplace( cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch ) --> cReturn
 *
 * Copyright 2006 Francesco Saverio Giudice <info/at/fsgiudice.com>
 * www - http://www.xharbour.org
 *
 * HB_REGEXSTRTRAN()
 * Copyright 2010 Adam L. <niechcespamu/domena.pl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * <text>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * </text>
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 * <text>
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 * </text>
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//--------------------------------------------------------------//

#define MATCH_STRING 1
#define MATCH_START  2
#define MATCH_END    3

FUNCTION hb_RegexReplace( cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch )

   LOCAL pRegex
   LOCAL aMatches, aMatch
   LOCAL cReturn
   LOCAL nOffSet := 0
   LOCAL cSearch, nStart, nLenSearch, nLenReplace
// LOCAL nEnd

   IF HB_ISREGEXSTRING( cRegex )
      pRegex := cRegEx
   ELSE
      pRegex := hb_regexComp( cRegEx, lCaseSensitive, lNewLine )
   ENDIF

   cReturn := cString

// lCaseSensitive and lNewLine already defined by HB_RegExComp()!
   aMatches := hb_regexAll( pRegEx, cString, /* lCaseSensitive */, /*lNewLine*/, nMaxMatches, nGetMatch, .F. )

   IF ! ( aMatches == NIL )
      FOR EACH aMatch IN aMatches
         //TraceLog( "ValToPrg( aMatch ), cReturn", ValToPrg( aMatch ), cReturn )
         IF Len( aMatch ) == 1 .AND. Len( aMatch[1] ) == 3 // if regex matches I must have an array of 3 elements
            cSearch := aMatch[1][ MATCH_STRING ]
            nStart  := aMatch[1][ MATCH_START ]
            //nEnd    := aMatch[1][ MATCH_END ]

            nLenSearch  := Len( cSearch ) //nEnd - nStart + 1
            nLenReplace := Len( cReplace )
            //TraceLog( "SubStr( cString, nStart, nLenSearch )", ;
            //          SubStr( cString, nStart - nOffSet, nLenSearch ) )

            cReturn := Stuff( cReturn, nStart - nOffSet, nLenSearch, cReplace )
            nOffSet += nLenSearch - nLenReplace
            //TraceLog( "cSearch, nStart, nEnd, nLenSearch, nLenReplace, nOffSet, cReturn",;
            //          cSearch, nStart, nEnd, nLenSearch, nLenReplace, nOffSet, cReturn )
         ENDIF
      NEXT
   ENDIF

   RETURN cReturn

//--------------------------------------------------------------//

/**************************************************************************
*  Regular expression version of function STRTRAN()
*  --------------------------------------------------
*  hb_RegExStrTran(<cString>,<cpSearch>,[cReplace],[nStart],[nCount],[lCase],[lNewLine])  --> cReturn
*  <cString>  The main string to search
*  <cpSearch> The string/regexp to locate in the main string
*  <cReplace> The string to replace
*  <nStart>   The first occurence to be replaced  (defaut 1)
*  <nCount>   Number of occurence to replace      (default ALL)
*  <lCase> ,<lNewLine> Options for regular expression
*  DESCRIPTION:
*  In <cReplace> sign '$' is extra token: for iclude backreferences
*   '$&' or '$0' - insert whole regex match
*   '$1'..'$99'  - insert group 1 .. 99  (if group exist)
*   '$10'..'$99' but not exist group > 9 - try group 1..9 + 'digit'
*   '$1'..'$9' but group not exist  -  insert as empty string (remove '$n')
*   '$$' - as single '$' (sample to insert '$2', use '$$2' )
*   '$x' where x is not digit or '&' -  insert as is
* SAMPLE:
* hb_RegExStrTran("xxxA1yyyB2zzz",".\d","QQ") --> "xxxQQyyyQQzzz"
* hb_RegExStrTran("xxxA1yyyB2zzz",".\d","Q$&Q") --> "xxxQA1QyyyQB2Qzzz"
* hb_RegExStrTran("xxxA1yyyB2zzz",".(\d)","Q$1") --> "xxxQ1yyyQ2zzz"
* hb_RegExStrTran("xxxA1yyyB2zzz",".\d","$$0") --> "xxx$0yyy$0zzz"
* hb_RegExStrTran("xxxA1yyyB2zzz",".(\d)","Q$3") --> "xxxQyyyQzzz"
* hb_RegExStrTran("xxxA1yyyB2zzz",".\d","Q$Q") --> "xxxQ$QyyyQ$Qzzz"
*
* Adam
* PS.
* Please correct the descriptions and comments in "English" :)
*************************************************************************/

FUNCTION hb_RegExStrTran( cString, cpSearch, cReplace, nStart, nCount, lCase, lNewLine )

   LOCAL aMatch, nFind := 0, cRet := ""
   LOCAL cRep, cRep0, pos, xG1, xG2, lAll

   IF !ValType( cString ) $ "CM"
      // do error ???
      RETURN nil  // or ""
   ENDIF

   IF !ValType( cpSearch ) $ "CM"
      // IF !HB_ISREGEX(cpSearch)
      IF !HB_ISREGEXSTRING( cpSearch )
         // do error ???
         RETURN nil // or ""
      ENDIF
   ENDIF

   IF !ValType( cReplace ) $ "CM"
      cReplace := ""
   ENDIF

   IF !HB_ISNUMERIC( nStart )
      nStart := 1
   ENDIF

   IF !HB_ISNUMERIC( nCount )
      nCount := 0
      lAll   := .T.
   ELSE
      lAll   := .F.
   ENDIF

//  StrTran() work this way:
   IF !lAll .AND. nCount == 0
      RETURN ""
   ENDIF
   IF nCount < 0
      RETURN cString
   ENDIF
   IF nStart < 1
      RETURN cString
   ENDIF

// START SEARCH
   DO WHILE lAll .OR. nCount > 0
      aMatch := hb_regexAtX( cpSearch, cString, lCase, lNewLine )
      //aMatch: { {Find,Start,End} [,{FindGr1,StartGr1,EndGr1},...] }
      IF Empty( aMatch )   //not found
         EXIT
      ENDIF
      nFind++
      IF nFind >= nStart
         // now change in cReplace "$..."
         cRep0 := cReplace
         cRep  := ""
         DO WHILE ( pos := At( "$", cRep0 ) ) > 0
            xG1 := SubStr( cRep0, pos + 1, 1 )
            xG2 := SubStr( cRep0, pos + 2, 1 )
            IF xG1 == "$"                         // '$$' -> '$'
               cRep  += Left( cRep0, pos )
               cRep0 := SubStr( cRep0, pos + 2 )
            ELSEIF xG1 $ "&0"                     // all found text
               cRep  += Left( cRep0, pos - 1 ) + aMatch[ 1, 1 ]
               cRep0 := SubStr( cRep0, pos + 2 )
            ELSEIF xG1 $ "123456789"             // $1 .. $9
               IF xG2 $ "0123456789"             // test $10 .. $99
                  IF ( xG2  := Val( xG1 + xG2 ) + 1 ) <= Len( aMatch )   //try group > 9
                     cRep  += Left( cRep0, pos - 1 ) + aMatch[ xG2, 1 ]
                     cRep0 := SubStr( cRep0, pos + 3 )
                  ELSEIF ( xG1 := Val( xG1 ) + 1 ) <= Len( aMatch )   //try '$xy'- >GroupX+'y'
                     cRep  += Left( cRep0, pos - 1 ) + aMatch[ xG1, 1 ]
                     cRep0 := SubStr( cRep0, pos + 2 )
                  ELSE                           //group not exist ->remove
                     cRep  += Left( cRep0, pos - 1 )
                     cRep0 := SubStr( cRep0, pos + 2 )
                     //  *** OR as is ???  ***
                     // cRep  += LEFT( cRep0, pos + 1 )
                     // cRep0 := SUBSTR( cRep0, pos + 2 )
                  ENDIF
               ELSE                            // not $10..$99
                  IF ( xG1 := Val( xG1 ) + 1 ) <= Len( aMatch )   //gropu exist
                     cRep  += Left( cRep0, pos - 1 ) + aMatch[ xG1, 1 ]
                     cRep0 := SubStr( cRep0, pos + 2 )
                  ELSE                         //group not exist-> remove
                     cRep  += Left( cRep0, pos - 1 )
                     cRep0 := SubStr( cRep0, pos + 2 )
                     //  *** OR as is ???  ***
                     // cRep  += LEFT( cRep0, pos + 1 )
                     // cRep0 := SUBSTR( cRep0, pos + 2 )
                  ENDIF
               ENDIF
            ELSE                          // '$x' -> copy as is
               cRep  += Left( cRep0, pos + 1 )
               cRep0 := SubStr( cRep0, pos + 2 )
            ENDIF
         ENDDO
         cRep    += cRep0
         cRet    += Left( cString, aMatch[ 1, 2 ] - 1 ) + cRep
         cString := SubStr( cString, aMatch[ 1, 3 ] + 1 )
         nCount--
      ENDIF
   ENDDO
   cRet += cString

   RETURN cRet

// ****** END *********
