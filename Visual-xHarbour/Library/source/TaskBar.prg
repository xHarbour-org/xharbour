/*
 * $Id$
 */
#include "vxh.ch"
#include "debug.ch"

// structures


// taskbar specific messages (should go info a header file)

#define ABM_GETTASKBARPOS     0x00000005
#define ABN_POSCHANGED        0x0000001
#define ABM_WINDOWPOSCHANGED  0x0000009


//--------------------------------------------------------------------------------------------------

CLASS TaskBar FROM Window

   DATA BarData

   METHOD Init() CONSTRUCTOR
   METHOD OnWindowPosChanged()

ENDCLASS

//--------------------------------------------------------------------------------------------------

METHOD Init() CLASS TaskBar

   SHAppBarMessage( ABM_GETTASKBARPOS, @::BarData )
   ::hWnd    := ::BarData:hWnd

RETURN Self

//--------------------------------------------------------------------------------------------------

METHOD OnWindowPosChanged() CLASS TaskBar

   LOCAL cAbd

   SHAppBarMessage( ABM_GETTASKBARPOS, @::BarData )

   ::Left   := ::BarData:rc:Left
   ::Top    := ::BarData:rc:Top
   ::Width  := ::BarData:rc:right  - ::BarData:rc:Left
   ::Height := ::BarData:rc:bottom - ::BarData:rc:Top

RETURN NIL

//--------------------------------------------------------------------------------------------------
