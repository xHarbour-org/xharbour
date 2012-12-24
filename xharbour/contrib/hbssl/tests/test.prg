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

#define CRLF Chr( 13 ) + Chr( 10 )

PROCEDURE Main()

   LOCAL ssl_ctx
   LOCAL ssl
   LOCAL cipher

   LOCAL socket
   LOCAL buffer

   LOCAL bits
   LOCAL tmp

   //

   inetInit()

   ? "-------"

   socket := inetCreate()
   ? "INETTIMEOUT", inetSetTimeout( socket, 500 )
   ? "INETCONN", inetConnect( "www.fortify.net", 80, socket )
   ? "INETERR", inetErrorCode( socket )
   ? "INETFD", inetFD( socket )
   ? "INETSEND", inetSend( socket, "GET / http/1.1" + CRLF + "Host: " + "syenar.net" + CRLF + CRLF )
   ? "INETERR", inetErrorCode( socket )
   buffer := Space( 1024 )
   ? "INETRECVALL", inetRecvAll( socket, @buffer, Len( buffer ) )
   ? "BUFFER", ">" + AllTrim( buffer ) + "<"
   ? "INETCLOSE", inetClose( socket )

   ? "-------"

   socket := inetCreate()
   ? inetSetTimeout( socket, 2500 )
   ? inetConnect( "www.fortify.net", 443, socket )
   ? inetErrorCode( socket )

   //

   SSL_init()

   ? SSLeay_version()
   ? SSLeay_version( HB_SSLEAY_VERSION  )
   ? SSLeay_version( HB_SSLEAY_CFLAGS   )
   ? SSLeay_version( HB_SSLEAY_BUILT_ON )
   ? SSLeay_version( HB_SSLEAY_PLATFORM )
   ? SSLeay_version( HB_SSLEAY_DIR      )

   ? "RAND_SEED", RAND_seed( "some entropy" )

   ? "SSL_CTX_NEW", ssl_ctx := SSL_CTX_new()

   ? "SSL_NEW", ssl := SSL_new( ssl_ctx )

   ? "SSL_VERSION", SSL_version( ssl )
   ? "SSL_GET_VERSION", SSL_get_version( ssl )

   ? "INET FD", inetFD( socket )

   ? "SSL_SET_FD", SSL_set_fd( ssl, inetFD( socket ) )
   ? "SSL_CONNECT", tmp := SSL_connect( ssl )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )

   tmp := SSL_get_ciphers( ssl )
   FOR EACH cipher IN tmp
      ? "SSL_CIPHER_GET_NAME", SSL_CIPHER_get_name( cipher )
      ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_get_version( cipher )
      ? "SSL_CIPHER_GET_BITS", SSL_CIPHER_get_bits( cipher, @bits ), bits
      ? "SSL_CIPHER_DESCRIPTION", ">" + SSL_CIPHER_description( cipher ) + "<"
      ? "- - - - - - - - - - - - - - -"
   NEXT

   ? "SSL_GET_CIPHER_BITS", SSL_get_cipher_bits( ssl, @bits ), bits
   ? "SSL_GET_CIPHER_LIST", SSL_get_cipher_list( ssl )
   ? "SSL_GET_CIPHER_NAME", SSL_get_cipher_name( ssl )
   ? "SSL_GET_CIPHER_VERSION", SSL_get_cipher_version( ssl )

   ? "SSL_GET_CURRENT_CIPHER", cipher := SSL_get_current_cipher( ssl )
   ? "SSL_CIPHER_GET_NAME", SSL_CIPHER_get_name( cipher )
   ? "SSL_CIPHER_GET_VERSION", SSL_CIPHER_get_version( cipher )
   ? "SSL_CIPHER_GET_BITS", SSL_CIPHER_get_bits( cipher, @bits ), bits
   ? "SSL_CIPHER_DESCRIPTION", SSL_CIPHER_description( cipher )

   ? "SSL_WRITE", tmp := SSL_write( ssl, "GET / http/1.1" + CRLF + "Host: " + "www.fortify.net" + CRLF + CRLF )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )
   buffer := Space( 1024 )
   ? "SSL_READ", tmp := SSL_read( ssl, @buffer )
   ? "SSL_GET_ERROR", SSL_get_error( ssl, tmp )
   ? buffer

   ? inetClose( socket )

   RETURN
