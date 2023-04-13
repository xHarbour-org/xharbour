/*
 * $Id$
 */
/*
 * File......: NTOW.PRG
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:54   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   09 Jun 1991 00:26:56   GLENN
 * Initial revision.
 *
 */




/*  $DOC$
 *  $FUNCNAME$
 *     FT_NTOW()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Translate numeric value to words
 *  $SYNTAX$
 *     FT_NTOW( <nNumber> ) -> cWords
 *  $ARGUMENTS$
 *     <nNumber>  An integer to translate
 *  $RETURNS$
 *     A text string representing <nNumber>
 *  $DESCRIPTION$
 *      Translates numeric input to a text string.
 *
 *      FT_NTOW is intended to be used with integers only.  Since I don't
 *      know what your application will be, I can't assume the type of
 *      fraction you want returned (ninety nine cents, 99/100, .99, etc).
 *      If you want the fraction in words, just pass it as an integer.
 *
 *      Do not pass a negative number!  Handle negative numbers any way
 *      you need to in your code.  (ie: CR, DB, Negative, Minus, etc.)
 *
 *      Also, numeric 0 is returned as a null string.  You will need to
 *      make a decision how to output it (zero dollars, no dollars, etc).
 *  $EXAMPLES$
 *  ? FT_NTOW( 999 )  -> Nine Hundred Ninety Nine
 *
 *  ? FT_NTOW( 1000 )  -> One Thousand
 *
 *  ? FT_NTOW( 23 ) + " Dollars and " + FT_NTOW( 99 ) + " Cents"
 *   -> Twenty Three Dollars and Ninety Nine Cents
 *
 *  ? FT_NTOW( 23 ) + " Dollars and " + "99/100"
 *   -> Twenty Three Dollars and 99/100
 *
 *    x      := -23.99
 *    cents  := str( (x - int( x )) * 100, 2, 0 ) + "/100"
 *  x      := int( x )
 *    string := iif( x < 0, "Credit of ", "Debit of " )
 *  ? string + FT_NTOW( abs(x) ) + " Dollars and " + "99/100"
 *       -> Credit of Twenty Three Dollars and 99/100
 *  $END$
 */



STATIC ones  := { "",     " One",   " Two",   " Three", " Four", " Five",  ;
      " Six", " Seven", " Eight", " Nine"                      ;
      }

STATIC teens := { " Ten",      " Eleven",    " Twelve",   ;
      " Thirteen", " Fourteen",  " Fifteen",  ;
      " Sixteen",  " Seventeen", " Eighteen", ;
      " Nineteen"                             ;
      }

STATIC tens  :=  { "", "", " Twenty", " Thirty", " Forty", " Fifty", ;
      " Sixty", " Seventy", " Eighty", " Ninety"  }

STATIC qualifiers := { "", " Thousand", " Million", " Billion", " Trillion" }


#ifdef FT_TEST

FUNCTION main( cNum )

   RETURN QOut( ft_ntow( Val( cNum ) ) )

#endif

FUNCTION ft_ntow( nAmount )

   LOCAL nTemp, sResult := " ", nQualNo
   LOCAL nDiv := 10 ^ ( Int( sol10(nAmount ) / 3 ) * 3 )

   nTemp   := Int( nAmount % nDiv )
   nAmount := Int( nAmount / nDiv )
   nQualNo := Int( sol10( nDiv ) / 3 ) + 1
   sResult += grp_to_words( nAmount, qualifiers[ nQualNo ] )

   IF nTemp > ( nDiv /= 1000 ) .AND. ( nDiv > 1 )
      sResult += ft_ntow( nTemp, nDiv )
   ELSE
      sResult += grp_to_words( nTemp, "" )
   ENDIF

   RETURN( LTrim( sResult ) )

STATIC FUNCTION grp_to_words( nGrp, sQual )

   LOCAL sResult := "", nTemp

   nTemp   := Int( nGrp % 100 )
   nGrp    := Int( nGrp / 100 )
   sResult += ones[ nGrp + 1 ] + iif( nGrp > 0, " Hundred", "" )

   DO CASE
   CASE nTemp > 19
      sResult += tens[ int( nTemp / 10 ) + 1 ]
      sResult += ones[ int( nTemp % 10 ) + 1 ]
   CASE nTemp < 20 .AND. nTemp > 9
      sResult += teens[ int( nTemp % 10 ) + 1 ]
   CASE nTemp < 10 .AND. nTemp > 0
      sResult += ones[ int( nTemp) + 1 ]
   ENDCASE

   RETURN( sResult + sQual )

STATIC FUNCTION sol10( nNumber )

   LOCAL sTemp

   sTemp := LTrim( Str( Int(nNumber ), 0 ) )

   RETURN( Len( sTemp ) - 1 )
