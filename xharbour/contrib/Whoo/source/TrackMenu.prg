// Augusto Infante
// Whoo.lib

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"

CLASS TTrackPopupMenu

   DATA handle
   METHOD New() CONSTRUCTOR
   METHOD Create()
   METHOD AddItem()
   METHOD AddSeparator() INLINE ::AddItem()
   METHOD Destroy()
ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New() CLASS TTrackPopupMenu
   ::handle := CreatePopupMenu()
return( self )

*-----------------------------------------------------------------------------*

METHOD AddItem( cText, nId ) CLASS TTrackPopupMenu
   IF cText==NIL.AND.nId==NIL==NIL
      AppendMenu( ::handle, MF_SEPARATOR)
     else
      AppendMenu( ::handle, MF_ENABLED + MF_STRING, nId, cText)
   endif
return(self)

*-----------------------------------------------------------------------------*

METHOD Create( hWnd, x, y ) CLASS TTrackPopupMenu
   TrackPopupMenu( ::handle, TPM_TOPALIGN + TPM_HORIZONTAL, x, y, 0, hWnd )
return(self)

METHOD Destroy() CLASS TTrackPopupMenu
   DestroyMenu(::handle)
return(self)
