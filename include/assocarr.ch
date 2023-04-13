/*
 * $Id$
 */
/* native support in parsers for Hash()
#TRANSLATE { <Key1> \=> <Val1> [, <KeyN> \=> <ValN> ] } => ( HB_SetWith( TAssociativeArray( { { <Key1>, <Val1> } [,{ <KeyN>, <ValN> } ] } ) ), __ClsSetModule( HB_QWith():ClassH ), HB_SetWith() )
 */

#warning TAssociativeArray() has been replaced by Hash()

#translate TAssociativeArray() => Hash()
