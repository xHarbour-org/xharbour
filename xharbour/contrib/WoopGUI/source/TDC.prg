/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

CLASS TDC FROM TObject

    DATA   nHWnd AS NUMERIC
    DATA   nDC   AS NUMERIC

    METHOD New() CONSTRUCTOR

    //METHOD Create( cDriver, cDevice ) INLINE ::nDC := CreateDC( cDriver, cDevice, NULL, NULL )
    METHOD BeginDrawing()             VIRTUAL
    METHOD Blit()                     VIRTUAL
    METHOD CalcBoundingBox()          VIRTUAL
    METHOD Clear()                    VIRTUAL
    METHOD CrossHair()                VIRTUAL
    METHOD DestroyClippingRegion()    VIRTUAL
    METHOD DeviceToLogicalX()         VIRTUAL
    METHOD DeviceToLogicalXRel()      VIRTUAL
    METHOD DeviceToLogicalY()         VIRTUAL
    METHOD DeviceToLogicalYRel()      VIRTUAL
    METHOD DrawArc()                  VIRTUAL
    METHOD DrawBitmap()               VIRTUAL
    METHOD DrawCheckMark()            VIRTUAL
    METHOD DrawEllipse()              VIRTUAL
    METHOD DrawEllipticArc()          VIRTUAL
    METHOD DrawIcon()                 VIRTUAL
    METHOD DrawLine()                 VIRTUAL
    METHOD DrawLines()                VIRTUAL
    METHOD DrawPolygon()              VIRTUAL
    METHOD DrawPoint()                VIRTUAL
    METHOD DrawRectangle()            VIRTUAL
    METHOD DrawRotatedText()          VIRTUAL
    METHOD DrawRoundedRectangle()     VIRTUAL
    METHOD DrawSpline()               VIRTUAL
    METHOD DrawText()                 VIRTUAL
    METHOD EndDoc()                   VIRTUAL
    METHOD EndDrawing()               VIRTUAL
    METHOD EndPage()                  VIRTUAL
    METHOD FloodFill()                VIRTUAL
    METHOD GetBackground()            VIRTUAL
    METHOD GetBackgroundMode()        VIRTUAL
    METHOD GetBrush()                 VIRTUAL
    METHOD GetCharHeight()            VIRTUAL
    METHOD GetCharWidth()             VIRTUAL
    METHOD GetClippingBox()           VIRTUAL
    METHOD GetFont()                  VIRTUAL
    METHOD GetLogicalFunction()       VIRTUAL
    METHOD GetMapMode()               VIRTUAL
    METHOD GetOptimization()          VIRTUAL
    METHOD GetPen()                   VIRTUAL
    METHOD GetPixel()                 VIRTUAL
    METHOD GetSize()                  VIRTUAL
    METHOD GetTextBackground()        VIRTUAL
    METHOD GetTextExtent()            VIRTUAL
    METHOD GetTextForeground()        VIRTUAL
    METHOD GetUserScale()             VIRTUAL
    METHOD LogicalToDeviceX()         VIRTUAL
    METHOD LogicalToDeviceXRel()      VIRTUAL
    METHOD LogicalToDeviceY()         VIRTUAL
    METHOD LogicalToDeviceYRel()      VIRTUAL
    METHOD MaxX()                     VIRTUAL
    METHOD MaxY()                     VIRTUAL
    METHOD MinX()                     VIRTUAL
    METHOD MinY()                     VIRTUAL
    METHOD Ok()                       VIRTUAL
    METHOD ResetBoundingBox()         VIRTUAL
    METHOD SetDeviceOrigin()          VIRTUAL
    METHOD SetBackground()            VIRTUAL
    METHOD SetBackgroundMode()        VIRTUAL
    METHOD SetClippingRegion()        VIRTUAL
    METHOD SetPalette()               VIRTUAL
    METHOD SetBrush()                 VIRTUAL
    METHOD SetFont()                  VIRTUAL
    METHOD SetLogicalFunction()       VIRTUAL
    METHOD SetMapMode()               VIRTUAL
    METHOD SetOptimization()          VIRTUAL
    METHOD SetPen()                   VIRTUAL
    METHOD SetTextBackground()        VIRTUAL
    METHOD SetTextForeground()        VIRTUAL
    METHOD SetUserScale()             VIRTUAL
    METHOD StartDoc()                 VIRTUAL
    METHOD StartPage()                VIRTUAL

    METHOD Get()                      INLINE ::nDC := GetDC( ::nHWnd )
    METHOD Release()                  INLINE ( ReleaseDC( ::nHWnd, ::nDC ) == 1 )

ENDCLASS

METHOD New( nHWnd ) CLASS TDC
   ASSIGN ::nHWnd WITH nHWnd
RETURN Self
