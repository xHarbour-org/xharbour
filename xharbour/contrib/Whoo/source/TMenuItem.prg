
// WHOO.LIB

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"

CLASS TMenuItem

   DATA Text    AS CHARACTER
   DATA oMenu   AS OBJECT
   DATA Id
   DATA Action  AS CODEBLOCK
   METHOD New() CONSTRUCTOR

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( cText, nId, bAction, oMenu ) CLASS TMenuItem

   ::Text   := cText
   ::Id     := nId
   ::Action := bAction
   ::oMenu  := oMenu
   aAdd( ::oMenu:oMenu:aItems, self )
   AppendMenu( oMenu:handle, MF_ENABLED + MF_STRING, nId, cText)

   return(self)

*-----------------------------------------------------------------------------*