/**********************************************
* tIPEncoder.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipencoder.prg,v 1.1 2003/11/30 14:41:50 jonnymind Exp $
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

   cModel := Lower( cModel )

   DO CASE
      CASE cModel == "text" .or. cModel == "plain"
         ::cName := "text"
         RETURN Self

      CASE cModel == "base64"
         RETURN TIPEncoderBase64():New()

      CASE cModel == "quoted-printable"
         RETURN TIPEncoderQP():New()

      CASE cModel == "url" .or. cModel == "urlencoded"
         RETURN TIPEncoderURL():New()

   ENDCASE
RETURN NIL

METHOD Encode( cData ) class TIPEncoder
RETURN cData

METHOD Decode( cData ) class TIPEncoder
RETURN cData

