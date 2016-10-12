/*
 Include this source file in your project if you'd like to use 
 WVT.lib as your Graphic Terminal system, instead of the default
 terminal. This will allow you to produce a Windows executable 
 using the standrd Clipper syntax, and incremenatly modify your
 app to use native GUI controls, when desired.
*/

#include "wvtwin.ch"

REQUEST HB_GT_WVT
REQUEST Wvt_GetScreenWidth
REQUEST WVT_SetFont
REQUEST WVT_SetCodePage

PROCEDURE GTSYS


RETURN

