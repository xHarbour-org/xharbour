************************************************************
* Test for Xwt Browse
*
* Giancarlo Niccolai et al.:
*
* $Id: xwt_test.prg,v 1.24 2003/10/09 23:18:34 jonnymind Exp $
*

#include "xwt.ch"
#include "xwtcmd.ch"

PROCEDURE Main()
   LOCAL oBrowse, oWindow, oCol
   LOCAL i
   LOCAL aData1 := {"first a", "first b", "first c", "first d"}
   LOCAL aData2 := {"second a", "second b", "second c", "second d"}

   i:= Eval( { |n| Str(n,3)}, 1 )
   XWTInit()

   DEFINE WINDOW oWindow TITLE "XWT BrowseTest"

   oBrowse := XWTBrowse():New(Len(aData2))
   oCol := XWTbColumn():New( "Index",, { |n| Str(n+3,3)} )
   oBrowse:AddColumn( oCol )
   oBrowse:AddColumn( XWTbColumn():New( "First field", aData1 ) )
   oBrowse:AddColumn( XWTbColumn():New( "A new column", aData2, { @GetElem() } ) )
   oBrowse:GetColumn(1):bEditable := .F.
   oCol:cColor    := "red"
   oCol:cBackground := "#100060"
   oCol:cBackground := "#100060"

   oBrowse:Stabilize()

   oWindow:Add( oBrowse )
   oWindow:AddEventListener( XWT_E_DESTROY_REQ, { || .not. XwtQuit()} )
   oWindow:Show()

   oBrowse:AddEventListener( XWT_E_UPDATED, @BrowseUpdated() )
   /*** Main LOOP ***/
   XwtMainLoop()

   /*** Going to terminate */
   //oWindow:Destroy()

   XWTQuit()
RETURN

Function GetElem( nRow, oBrowse, oColumn )
   ? "Doing:", nRow
Return oColumn:xCargo[ nRow ]


FUNCTION BrowseUpdated( oEvent )
   oCol := oEvent:oSender:GetColumn( oEvent:aParams[1] )
   oCol:xCargo[ oEvent:aParams[2] ] := oEvent:aParams[3]
RETURN

