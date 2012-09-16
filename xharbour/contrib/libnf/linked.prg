/*
 * $Id$
 */
/*
 * File......: Linked.PRG
 * Author....: Brian Loesgen
 * CIS ID....: 74326,1174
 *
 * This is an original work by Brian Loesgen and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:52   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:08   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   13 Jun 1991 15:21:26   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_LINKED()
 *  $CATEGORY$
 *     Environment
 *  $ONELINER$
 *     Determine if a function was linked in
 *  $SYNTAX$
 *     FT_LINKED( <cString> ) -> lResult
 *  $ARGUMENTS$
 *     <cString> is a character string containing one or more function
 *               calls
 *  $RETURNS$
 *     .T. if all functions within the string are currently linked into
 *     the application, .F. if one or more aren't.  See below for a
 *     definition of "function."
 *  $DESCRIPTION$
 *
 *     This function would be used in data driven application to determine
 *     whether or not a macro compiled function was linked in.
 *
 *     Several functions can be passed, and nested, in <cString>.
 *
 *     Caveat: Some function calls are converted by the preprocessor
 *     into other function calls. You cannot have these types of
 *     functions in a macro compiled string as they never exist at
 *     runtime. FT_LINKED will correctly tell you that they are invalid.
 *
 *     For instance: there is no function called SORT() in any of the
 *     Nantucket LIBraries, but it is a valid CLIPPER command because the
 *     preprocessor will convert it to other function calls.
 *
 *
 *  $EXAMPLES$
 *
 *     cString := "FT_GoodFunc(BadFunc(3,2))"
 *     IF FT_LINKED(cString)
 *        EVAL( &("{||"+cString+"}") )
 *     ELSE
 *        ALERT("Error: "+cString+" was not linked in. Called by FT_LINKED()")
 *     ENDIF
 *
 *
 *  $END$
 */


#ifdef FT_TEST

FUNCTION Main

   LOCAL cString
   LOCAL aString := { "TRIM('abc ')",                                     ;
      "NotARealFunc()",                                   ;
      "FT_DispMsg()",                                     ;
      'TRIM(cVar+"abc"+LEFT(cString)), FOUND()',          ;
      "IsItLinked()",                                     ;
      "lRetVal := FOUND()",                               ;
      "!EOF() .AND. MONTH(DATE())=12 .AND. YeeHa()",      ;
      "!EOF() .AND. MONTH(DATE())=12",                    ;
      "!EOF() .AND. MONTH(DATE(YeeHa()))=12",             ;
      "LEFT(SUBSTR(nNum,4,VAL(cChar+ASC(c))))",           ;
      "EOF(>> Note: Syntax IS NOT checked! <<)"           ;
      }

   CLS
   @1, 0 SAY "String Tested                               Result"
   @2, 0 TO 2, MaxCol()
   AEval( aString, {|ele, num| QOut( ele, Space(45 - Len(ele ) ), FT_Linked(ele ) ) } )
   @MaxRow() - 2, 0

   RETURN NIL

#endif

//------------------------------------------------

FUNCTION FT_Linked( cFuncs )

// A function is detected by the left parenthesis, "(", and it begins
// at the space, comma or start-of-string preceeding the "("

// Returns: .T. if all functions are available,
//          .F. if not

   LOCAL aFuncArray := {}, nSpace, nComma, nFEnd, lRetVal := .F.

   IF At( "(", cFuncs ) = 0
      // No functions in string
      Alert( "Warning: Expected function(s) in FT_Linked(), but none were found" )
   ELSE
      DO WHILE ( nFEnd := At( "(",cFuncs ) ) > 0
         // Add the current function to the array of functions
         AAdd( aFuncArray, Left( cFuncs,nFEnd ) + ")" )
         // Remove the current function from the string
         cFuncs := SubStr( cFuncs, nFEnd + 1 )
         nSpace := At( " ", cFuncs ) ; nComma := At( ",", cFuncs )
         DO WHILE  ( nComma > 0 .AND. nComma < nFEnd ) .OR. ;
               ( nSpace > 0 .AND. nSpace < nFEnd )
            // We have extra parameters or spaces prior to the start
            // of the function. Strip them out.
            IF nComma > 0
               cFuncs := SubStr( cFuncs, nComma + 1 )
            ELSEIF nSpace > 0
               cFuncs := SubStr( cFuncs, nSpace + 1 )
            ENDIF
            nSpace := At( " ", cFuncs ) ; nComma := At( ",", cFuncs )
         ENDDO
      ENDDO
      // Scan through the array of functions, stop after the first occurence
      // of a function which returns a TYPE() of "U" (hence is not linked in)
      lRetVal := AScan( aFuncArray, {|element| Type( element ) == "U" } ) = 0
   ENDIF

   RETURN( lRetVal )
