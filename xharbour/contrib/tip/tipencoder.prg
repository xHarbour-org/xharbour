/**********************************************
* tIPEncoder.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipencoder.prg,v 1.2 2003/12/01 00:19:39 jonnymind Exp $
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
   IF cModel == "base64"
         RETURN TIPEncoderBase64():New()

   ELSEIF cModel == "quoted-printable"
      RETURN TIPEncoderQP():New()

   ELSEIF cModel == "url" .or. cModel == "urlencoded"
      RETURN TIPEncoderURL():New()

   ELSEIF cModel == "text" .or. cModel == "plain";
             .or. cModel == "text/plain" .or. cModel == "as-is";
             .or. cModel == "7-bit" .or. cModel == "8-bit"
      ::cName := "as-is"
      RETURN Self
   ENDIF

RETURN NIL

METHOD Encode( cData ) class TIPEncoder
RETURN cData

METHOD Decode( cData ) class TIPEncoder
RETURN cData

