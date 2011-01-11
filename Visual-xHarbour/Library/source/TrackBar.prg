/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// TrackBar.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"


//-----------------------------------------------------------------------------------------------

CLASS TrackBar INHERIT Control
   METHOD Init()  CONSTRUCTOR
   METHOD OnParentNotify()
   METHOD OnEraseBkGnd() INLINE 1
   DATA OnNMReleasedCapture EXPORTED
ENDCLASS

METHOD Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle, lCreate ) CLASS TrackBar
   DEFAULT ::__xCtrlName TO "TrackBar"
   DEFAULT nLeft   TO 0
   DEFAULT nTop    TO 0
   DEFAULT nWidth  TO 150
   DEFAULT nHeight TO 30
   ::ClsName  := "msctls_trackbar32" //TRACKBAR_CLASS
   ::Super:Init( oParent, cCaption, nId, nLeft, nTop, nWidth, nHeight, nStyle, lCreate )
   ::Style     := WS_CHILD + WS_VISIBLE + WS_TABSTOP + TBS_AUTOTICKS + TBS_ENABLESELRANGE
RETURN Self


METHOD OnParentNotify( nwParam, nlParam ) CLASS TrackBar
   LOCAL cd
   ::Super:OnParentNotify( nwParam, nlParam )
   IF ::BkBrush != NIL
      DO CASE
         CASE ::Parent:hdr:code == NM_CUSTOMDRAW 
              cd := (struct NMCUSTOMDRAW*) nlParam
              DO CASE
                 CASE cd:dwDrawStage == CDDS_PREPAINT
                      SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_NOTIFYITEMDRAW )
                      RETURN CDRF_NOTIFYITEMDRAW

                 CASE cd:dwDrawStage == CDDS_ITEMPREPAINT
                      DO CASE
                         CASE cd:dwItemSpec == TBCD_CHANNEL
                              _FillRect( cd:hdc, {0,0,::ClientWidth,::ClientHeight}, ::BkBrush )
                      ENDCASE
                      SetWindowLong( ::Parent:hWnd, DWL_MSGRESULT, CDRF_DODEFAULT )
                      RETURN CDRF_DODEFAULT
              ENDCASE

         CASE ::Parent:hdr:code == NM_RELEASEDCAPTURE
              __Evaluate( ::OnNMReleasedCapture, Self )
      ENDCASE
   ENDIF
RETURN NIL
