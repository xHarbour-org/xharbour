// Augusto Infante
// WhatPlus.lib

#Include "windows.ch"
#Include "hbclass.ch"
#include "what32.ch"
#Include "debug.ch"
#INCLUDE "WinGdi.ch"
#INCLUDE "progbar.ch"

CLASS TProgressBar FROM TControl

   DATA Caption  INIT  "%"
   DATA Left     INIT   0
   DATA Top      INIT   0
   DATA Width    INIT  80
   DATA Height   INIT  24
   DATA Style    INIT  WS_CHILD+WS_VISIBLE
   DATA ExStyle  INIT  0

   DATA range    INIT  100
   DATA percent  INIT  0

   DATA bkColor  INIT GetSysColor(COLOR_BTNFACE)
   DATA BarColor INIT GetSysColor(COLOR_HIGHLIGHT)
   
   DATA lRegister PROTECTED INIT .F.
   DATA lControl  PROTECTED INIT .T.
   DATA Msgs      PROTECTED INIT {WM_DESTROY,WM_SIZE,WM_MOVE}
   DATA WndProc   PROTECTED INIT "ControlProc"
   DATA Name      PROTECTED INIT PROGRESS_CLASS

   METHOD New() CONSTRUCTOR
   METHOD Update()
   METHOD DrawText()
   METHOD SetBkColor()
   METHOD SetBarColor()
   METHOD OnCreate() INLINE ::SendMessage( PBM_SETRANGE, 0, ::range ),;
                            ::SendMessage( PBM_SETSTEP, 1, 0),;
                            ::Update(),nil

ENDCLASS

METHOD New( oParent, nLeft, nTop, nWidth, nHeight, nRange )
   ::Left    := IFNIL( nLeft,   ::Left,   nLeft   )
   ::Top     := IFNIL( nTop,    ::Top,    nTop    )
   ::Width   := IFNIL( nWidth , ::Width,  nWidth  )
   ::Height  := IFNIL( nHeight, ::height, nHeight )
   ::range   := IFNIL( nRange,  ::range,  nRange  )
   ::percent := 50
return(super:new(oParent))

METHOD Update()
   SetProgressBar(::handle,::percent)
   IF ::Caption!="".and.AND(GetWindowLong(::handle,GWL_STYLE),PBS_SMOOTH)>0
      ::DrawText()
   END
RETURN(self)

METHOD DrawText()
   local hDC,aMetrics,nTMWidth,nTMHeight,nX,nY,aClip,nWidth,aRect,hBrush
   aRect:=GetClientRect(::handle)
   hDC:=GetDC(::handle)
   SelectObject(hDC,SendMessage(::Parent:handle,WM_GETFONT))
   aMetrics :=GetTextExtentPoint32(hDC,::Caption)
   nTMWidth :=aMetrics[1]
   nTMHeight:=aMetrics[2]
   nX = (aRect[3] - nTMWidth )/ 2
   nY =((aRect[4] - nTMHeight)/ 2)+(aRect[2]/2)
   nWidth:=aRect[1]+(((aRect[3]-aRect[1])* ::percent )/100)
   aClip:={nX,aRect[2],nX+nTMWidth,aRect[4]}
   SetTextColor(hDC, ::barColor)
   SetBkColor(hDC, ::bkColor)
   ExtTextOut(hDC, nX, nY, ETO_CLIPPED , aClip, ::Caption)
   IF nX<=nWidth
      SetTextColor(hDC, ::bkColor)
      SetBkColor(hDC, ::barColor)
      aClip:={nX,aRect[2],nWidth,aRect[4]}
      ExtTextOut(hDC, nX, nY, ETO_CLIPPED , aClip, ::Caption)
   END
   ReleaseDC(::handle,hDC)
return(self)

METHOD SetBkColor(nColor)
::bkColor:=nColor
SendMessage(::handle,PBM_SETBKCOLOR,0,nColor)
return(self)

METHOD SetBarColor(nColor)
::BarColor:=nColor
SendMessage(::handle,PBM_SETBARCOLOR,0,nColor)
return(self)
