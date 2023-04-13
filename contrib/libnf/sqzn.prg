/*
 * $Id$
 */
/*
 * File......: SQZN.PRG
 * Author....: Joseph D. Booth, Sr.
 * CIS ID....: 72040,2112
 *
 * This is an original work by Joseph D. Booth Sr. and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   13 Jun 1991 15:21:36   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SQZN()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Compress a numeric value into a character string
 *  $SYNTAX$
 *     FT_SQZN( <nValue> [, <nSize> [, <nDecimals> ] ] ) -> cCompressed
 *  $ARGUMENTS$
 *     nValue       - The numeric value to be compressed
 *     nSize        - Optional size of numeric field, defaults to 10
 *     nDecimals    - Optional number of decimal places, defaults to 0
 *  $RETURNS$
 *     cCompressed  - Compressed string, 50% the size of nSize
 *  $DESCRIPTION$
 *
 *    The FT_SQZN function allows a numeric value to be compressed when
 *    stored in the database.  The compression is 50% the storage space
 *    of the original number.  The companion function, FT_UNSQZN returns
 *    the original number from the compressed string.
 *
 *  $EXAMPLES$
 *
 *  replace TRANS->cust_id with FT_SQZN(mcust_id,8),;
 *          TRANS->amount  with FT_SQZN(mamount,12,2)
 *
 *  $SEEALSO$
 *      FT_UNSQZN()
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION ft_sqzn( nValue, nSize, nDecimals )

   LOCAL tmpstr, cCompressed, k

   nSize       := if( nSize    == NIL, 10, nSize )
   nDecimals   := if( nDecimals == NIL, 0, nDecimals )
   nValue      := nValue * ( 10 ** nDecimals )
   nSize       := if( nSize/2 <> Int( nSize/2 ), nSize + 1, nSize )
   tmpstr      := Str( Abs( nValue ), nSize )
   tmpstr      := StrTran( tmpstr, " ", "0" )
   cCompressed := Chr( Val( SubStr(tmpstr,1,2 ) ) + if( nValue < 0,128,0 ) )

   FOR k := 3 TO Len( tmpstr ) STEP 2
      cCompressed += Chr( Val( SubStr(tmpstr,k,2 ) ) )
   NEXT

   RETURN cCompressed



/*  $DOC$
 *  $FUNCNAME$
 *     FT_UNSQZN()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Uncompress a numeric compressed by FT_SQZN()
 *  $SYNTAX$
 *     FT_UNSQZN( <cCompressed>, <nSize> [, <nDecimals> ] ) -> nValue
 *  $ARGUMENTS$
 *     <cCompressed>  - Compressed string, obtained from FT_SQZN()
 *
 *     <nSize>        - Size of numeric field
 *
 *     <nDecimals>    - Optional number of decimal places
 *  $RETURNS$
 *     nValue       - Uncompressed numeric value
 *  $DESCRIPTION$
 *
 *    The FT_UNSQZN function returns the numeric value from the compressed
 *    string.  The compression is 50% the storage space of the original
 *    number.  The original number must have been compressed using the
 *    FT_SQZN() function.
 *
 *    This function, along with FT_SQZN() can be used to reduce disk storage
 *    requirements for numeric fields in a database file.
 *
 *  $EXAMPLES$
 *
 *    mcust_id := FT_UNSQZN(TRANS->cust_id,8),;
 *    mamount  := FT_UNSQZN(TRANS->amount,12,2)
 *
 *  $SEEALSO$
 *    FT_SQZN()
 *  $INCLUDE$
 *
 *  $END$
 */

FUNCTION ft_unsqzn( cCompressed, nSize, nDecimals )

   LOCAL tmp, k, cValue, multi := 1

   nSize       := if( nSize    == NIL, 10, nSize )
   nDecimals   := if( nDecimals == NIL, 0, nDecimals )
   cCompressed := if( multi    == - 1, SubStr( cCompressed,2 ), cCompressed )
   nSize       := if( nSize/2 <> Int( nSize/2 ), nSize + 1, nSize )
   IF Asc( cCompressed ) > 127
      tmp         := Str( Asc( cCompressed ) - 128, 2 )
      multi       := - 1
   ELSE
      tmp         := Str( Asc( cCompressed ), 2 )
   ENDIF

   FOR k := 2 TO Len( cCompressed )
      tmp += Str( Asc( SubStr(cCompressed,k,1 ) ), 2 )
   NEXT

   tmp    := StrTran( tmp, " ", "0" )
   cValue := SubStr( tmp, 1, nSize - nDecimals ) + "." + SubStr( tmp, nSize - nDecimals + 1 )

   RETURN Val( cValue ) * multi
