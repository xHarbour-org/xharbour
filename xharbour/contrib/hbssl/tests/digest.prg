/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 */

#message "/ ----------------------------------------------------\"
#message "| This program requires:                              |"
#message "|   hbssl.lib                                         |"
#message "|   libeay32*.lib                                     |"
#message "|   ssleay32*.lib                                     |"
#message "| URL: http://slproweb.com/products/Win32OpenSSL.html |"
#message "\-----------------------------------------------------/ "

#include "hbssl.ch"

PROCEDURE Main()

   LOCAL ctx
   LOCAL digest

   LOCAL key
   LOCAL iv

   SSL_init()

   OpenSSL_add_all_digests()
   OpenSSL_add_all_ciphers()

   ? "Version built against:", NumToHex( OPENSSL_VERSION() )
   ? "Version loaded:", NumToHex( SSLeay() )

   ctx := EVP_MD_CTX_create()
   EVP_MD_CTX_init( ctx )

   EVP_DigestInit_ex( ctx, "SHA256" )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "SHA256", ">" + StrToHex( digest ) + "<"

   EVP_DigestInit_ex( ctx, HB_EVP_MD_SHA256 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "SHA256", ">" + StrToHex( digest ) + "<"

   EVP_MD_CTX_cleanup( ctx )

   EVP_DigestInit_ex( ctx, HB_EVP_MD_RIPEMD160 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "RIPEMD160", ">" + StrToHex( digest ) + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( HB_EVP_CIPHER_AES_192_OFB, HB_EVP_MD_SHA256, "salt1234", "data", 2, @key, @iv )
   ? "KEY", StrToHex( key )
   ? ">" + key + "<"
   ? "IV", StrToHex( iv )
   ? ">" + iv + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( "AES-192-OFB", "SHA256", "salt1234", "data", 2, @key, @iv )
   ? "KEY", StrToHex( key )
   ? ">" + key + "<"
   ? "IV", StrToHex( iv )
   ? ">" + iv + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( "AES-192-OFB", "SHA256",, "data", 2, @key, @iv )
   ? "KEY", StrToHex( key )
   ? ">" + key + "<"
   ? "IV", StrToHex( iv )
   ? ">" + iv + "<"

   EVP_cleanup()

   RETURN
