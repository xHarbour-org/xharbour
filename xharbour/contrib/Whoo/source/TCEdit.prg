// Augusto Infante
// Whoo.lib

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TEdit FROM TControl

   METHOD New() CONSTRUCTOR

ENDCLASS

*------------------------------------------------------------------------------*

METHOD New( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight ) CLASS TEdit

   ::id        := nId
   ::lRegister := .F.
   ::lControl  := .T.
   ::Msgs      := IFNIL( ::Msgs, {WM_DESTROY,WM_SIZE,WM_MOVE}, ::Msgs )
   ::WndProc   := IFNIL( ::WndProc, 'FormProc', ::WndProc )
   ::Caption   := cCaption
   ::Left      := nLeft
   ::Top       := nTop
   ::Width     := IFNIL( nWidth , IFNIL( ::Width , 80, ::Width ), nWidth )
   ::Height    := IFNIL( nHeight, IFNIL( ::height, 20, ::height), nHeight)
   ::Name      := 'edit'
   ::ExStyle   := WS_EX_CLIENTEDGE
   ::Style     := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + ES_AUTOHSCROLL + ;
                  ES_AUTOVSCROLL + ES_LEFT + ES_WANTRETURN + ES_MULTILINE

   RETURN( super:new( oParent ) )

*------------------------------------------------------------------------------*
