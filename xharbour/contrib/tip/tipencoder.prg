/**********************************************
* tIPEncoder.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipclient.prg,v 1.5 2003/11/28 16:05:40 jonnymind Exp $
************************************************/
#include "hbclass.ch"
#include "fileio.ch"
#include "tip.ch"

CLASS TIPEncoder
   DATA cName

   METHOD New( cModel ) 
   METHOD Encode( cData )
   METHOD Decode( cData )
ENDCLASS


METHOD New( cModel ) class TIPEncoder
   DO CASE
      CASE Lower( cModel ) == "text"
         ::cName := "text"
         RETURN Self

      CASE Lower(cModel) == "base64"
         RETURN TIPEncoderBase64():New()

   ENDCASE
RETURN NIL

METHOD Encode( cData ) class TIPEncoder
RETURN cData

METHOD Decode( cData ) class TIPEncoder
RETURN cData

