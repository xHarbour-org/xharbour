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

   LOCAL bio

   SSL_init()

   ? bio := BIO_new_fd( 1, HB_BIO_NOCLOSE )
   ? "BIO_WRITE", BIO_write( bio, "Hello world!" + hb_osnewline() )
   ? "BIO_FLUSH", BIO_flush( bio )
   ? "BIO_FREE", BIO_free( bio )

   ? bio := BIO_new_file( "bio_test.txt", "a+" )
   ? "BIO_WRITE", BIO_write( bio, "Hello world!" + hb_osnewline() )
   ? "BIO_FLUSH", BIO_flush( bio )
   ? "BIO_FREE", BIO_free( bio )

   RETURN
