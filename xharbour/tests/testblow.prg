/*
* $Id$
* Test program for blowfish routines
*/

#include "common.ch"

PROCEDURE MAIN()

   LOCAL cData := "Harbour Power"
   LOCAL cKey := hb_blowfishKey( "password" )
   LOCAL cCrypted := hb_blowfishEncrypt( cKey, cData )
   LOCAL cDecrypted

   ? cCrypted
   ? cDecrypted := hb_blowfishDecrypt( cKey, cCrypted )
   ? cDecrypted == cData
   ? LEN( cDecrypted ) == LEN( cData )

   RETURN
