
// WHOO.LIB

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "debug.ch"

CLASS TMenuItem

   DATA Text    AS CHARACTER
   DATA oMenu   AS OBJECT
   DATA Id
   DATA Action  AS CODEBLOCK
   DATA Checked
   DATA Enabled

   ACCESS Checked     INLINE ::MenuSet( MF_CHECKED, MF_UNCHECKED)
   ACCESS Disabled    INLINE ::MenuSet( MF_DISABLED, MF_ENABLED)
   ACCESS Enabled     INLINE ::MenuSet( MF_ENABLED, MF_DISABLED)
   ACCESS Grayed      INLINE ::MenuSet( MF_GRAYED, MF_ENABLED)

   METHOD New() CONSTRUCTOR
   METHOD MenuSet()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( cText, nId, bAction, oMenu ) CLASS TMenuItem

   ::Text   := cText
   ::Id     := nId
   ::Action := bAction
   ::oMenu  := oMenu
   aAdd( ::oMenu:oMenu:aItems, self )
   IF cText==NIL.AND.nId==NIL.AND.bAction==NIL
      AppendMenu( oMenu:handle, MF_SEPARATOR)
     else
      AppendMenu( oMenu:handle, MF_ENABLED + MF_STRING, nId, cText)
   endif

   return(self)

*-----------------------------------------------------------------------------*

METHOD MenuSet( nTest, nOther) CLASS TMenuItem
   local nMask := Or(nTest, nOther)
return AND(GetMenuState(::oMenu:handle, ::Id, MF_BYCOMMAND), nMask) == nTest
