/*
 * HWGUI - Harbour Win32 GUI library source code:
 * TVideo component
 *
 * Copyright 2003 Luiz Rafael Culik Guimaraes <culikr@brtrubo.com>
 * www - http://sites.uol.com.br/culikr/
*/
#include "HBClass.ch"
#include "windows.ch"
#include "guilib.ch"

#include "common.ch"


//----------------------------------------------------------------------------//

CLASS TVideo FROM hControl


   DATA   oMci
   DATA   cAviFile

   METHOD New( nRow, nCol, nWidth, nHeight, cFileName, oWnd,;
               bWhen, bValid, lNoBorder ,nid) CONSTRUCTOR

   METHOD ReDefine( nId, cFileName, oDlg, bWhen, bValid ) CONSTRUCTOR

   METHOD Initiate( )

   METHOD Play( nFrom, nTo ) INLINE  ::oMci:Play( nFrom, nTo, ::oparent:handle )

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( nRow, nCol, nWidth, nHeight, cFileName, oWnd, lNoBorder,nid ) CLASS TVideo

   DEFAULT nWidth to 200, nHeight to 200, cFileName to "",;
           lNoBorder to .f.

   ::nTop      := nRow *  VID_CHARPIX_H  // 8
   ::nLeft     := nCol * VID_CHARPIX_W   // 14
   ::nHeight   := ::nTop  + nHeight - 1
   ::nwidth    := ::nLeft + nWidth + 1
   ::Style     := nOR( WS_CHILD, WS_VISIBLE, WS_TABSTOP, If( ! lNoBorder, WS_BORDER, 0 ) )

   ::oParent   := Iif( oWnd==Nil, ::oDefaultParent, oWnd )
   ::id        := Iif( nId==Nil,::NewId(), nId )
   ::cAviFile  := cFileName
   tracelog('antes mci')
   ::oMci      := TMci():New( "avivideo", cFileName )
   ::Initiate()

   if !Empty( ::oparent:handle)
        ::oMci:lOpen()
        ::oMci:SetWindow( Self )
   else
      ::oparent:AddControl( Self )
   endif

return Self

//----------------------------------------------------------------------------//

METHOD ReDefine( nId, cFileName, oDlg, bWhen, bValid ) CLASS TVideo

   ::nId      = nId
   ::cAviFile = cFileName
   ::bWhen    = bWhen
   ::bValid   = bValid
   ::oWnd     = oDlg
   ::oMci     = TMci():New( "avivideo", cFileName )

   oDlg:AddControl( Self )

return Self

//----------------------------------------------------------------------------//

METHOD Initiate( ) CLASS TVideo

   Super:Init(  )
   ::oMci:lOpen()
   ::oMci:SetWindow( Self )

return nil

//----------------------------------------------------------------------------//
