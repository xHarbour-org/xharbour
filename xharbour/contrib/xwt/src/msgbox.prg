/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id$

   Message Box
   
   Every widget system has a message box facility; most of them
   are object oriented, but the procedure abstraction is more
   functional for the uses that are generally meant for the
   message box.
   
*/

#include "xwt.ch"

Function XWT_MsgBox( cMessage, nButtonType, nMsgType, oParent, cCaption )
RETURN XWT_drv_msgbox( oParent, cMessage, nButtonType, nMsgType, cCaption )

Function XWT_MsgBoxParent( oParent, cMessage, nButtonType, nMsgType )
RETURN XWT_drv_msgbox( oParent:oRawWidget, cMessage, nButtonType, nMsgType )
