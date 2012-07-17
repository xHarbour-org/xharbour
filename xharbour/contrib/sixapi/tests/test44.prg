/*
 * $Id$
 */
/*
  test44.prg
  Test sx_Encrypt()/sx_Decrypt()
*/
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
#include "sixapi.ch"
 
PROCEDURE MAIN

   LOCAL cname := "Harbour Power"
   LOCAL c

   ?
   ? 'local cname := "Harbour Power"'
   ? 'local c'
   ?
   ? 'sx_encrypt( cname, "password" ) = ', c := sx_encrypt( cname, "password" )
   ? 'sx_decrypt( c, "password" )     = ', sx_decrypt( c, "password" )
   ?
   ? 'sx_decrypt( c, "wrongpass" )    = ', sx_decrypt( c, "wrongpass" )
