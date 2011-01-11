/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// NotifyIcon.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

static nMess
#include "vxh.ch"
#include "debug.ch"

#define NIF_MESSAGE     1
#define NIF_ICON        2
#define NIF_TIP         4

#define NIM_ADD         0
#define NIM_MODIFY      1
#define NIM_DELETE      2

#define NIF_INFO        0x00000010

#define NIIF_NONE       0x00000000

#define NIIF_INFO       0x00000001
#define NIIF_WARNING    0x00000002
#define NIIF_ERROR      0x00000003

#define NOTIFYICONDATA_V2_SIZE 488

//    GUID guidItem;

//-------------------------------------------------------------------------------------------------------
CLASS NotifyIcon INHERIT Component
   DATA BalloonTipText  PUBLISHED
   DATA BalloonTipTitle PUBLISHED
   DATA Text            PUBLISHED
   PROPERTY BalloonTipIcon READ xBalloonTipIcon WRITE SetBalloonTipIcon DEFAULT 0
   PROPERTY Visible        READ xVisible        WRITE SetVisible        DEFAULT .T.
   PROPERTY Icon           READ xIcon           WRITE __SetIcon         INVERT
   PROPERTY Text           READ xText           WRITE __SetText         INVERT
   PROPERTY ContextMenu    GET __ChkComponent( Self, ::xContextMenu )

   DATA Balloon_Icons   EXPORTED  INIT { "None", "Info", "Warning", "Error" }
   DATA ID              EXPORTED
   DATA Flags           EXPORTED
   DATA Message         EXPORTED
   DATA Parent          EXPORTED
   DATA Events          EXPORTED  INIT {;
                                       {"Mouse", {;
                                                 { "OnLButtonDown"     , "", "" },;
                                                 { "OnLButtonUp"       , "", "" },;
                                                 { "OnRButtonDown"     , "", "" },;
                                                 { "OnRButtonUp"       , "", "" };
                                                 } } }
   
   METHOD Init() CONSTRUCTOR
   METHOD SetVisible()
   METHOD SetBalloonTipIcon()
   METHOD __SetIcon()
   METHOD __SetText()
   METHOD Destroy() INLINE IIF( ::__ClassInst == NIL, ::Visible := .F.,), Super:Destroy()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS NotifyIcon
   ::__xCtrlName   := "NotifyIcon"
   ::ClsName       := "NotifyIcon"
   ::ComponentType := "NotifyIcon"
   ::Super:Init( oOwner )
   ::Flags         := NIF_MESSAGE | NIF_ICON | NIF_TIP | NIF_INFO
   DEFAULT nMess TO 1
   ::Message       := WM_USER + nMess
   ::hWnd          := ::Owner:hWnd
   ::Parent        := ::Owner
   nMess++
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD SetVisible( lVisible ) CLASS NotifyIcon
   LOCAL lOpen, tray := (struct NOTIFYICONDATA)
   IF ::__ClassInst == NIL
      DEFAULT ::Id   TO ::Owner:GetNextControlId()
      tray:cbSize           := NOTIFYICONDATA_V2_SIZE
      tray:hWnd             := ::Owner:hWnd
      tray:uID              := ::Id
      tray:uFlags           := ::Flags
      tray:uCallbackMessage := ::Message
      lOpen := .F.
      IF lVisible
         IF EMPTY( ::Icon )
            ::Icon := ::Owner:Icon
         ENDIF
         IF !EMPTY( ::Icon )
            SWITCH VALTYPE( ::Icon )
               CASE "A"
                    IF ::__ClassInst == NIL .OR. EMPTY( ::Icon[1] )
                       ::__hIcon := LoadIcon( ::AppInstance, ::Icon[2] )
                       ::xIcon := ::Icon[2]
                     ELSE
                       ::__hIcon := LoadImage( ::AppInstance, ::Icon[1], IMAGE_ICON,,, LR_LOADFROMFILE )
                       ::xIcon := ::Icon[1]
                    ENDIF
                    EXIT

               CASE "C"
                    ::__hIcon := LoadIcon( ::AppInstance, ::Icon )
                    EXIT

               CASE "N"
                    ::__hIcon := ::Icon
                    EXIT
            END
            tray:hIcon := ::__hIcon
         ENDIF
         IF !EMPTY( ::Text )
            tray:szTip:Buffer( ::Text )
            lOpen := .T.
         ENDIF
         IF !EMPTY( ::BalloonTipText )
            tray:szInfo:Buffer( ::BalloonTipText )
            lOpen := .T.
         ENDIF
         IF !EMPTY( ::BalloonTipTitle )
            tray:szInfoTitle:Buffer( ::BalloonTipTitle )
         ENDIF
         tray:dwInfoFlags      := ::xBalloonTipIcon
      ENDIF
      lOpen := .T.
      IF lOpen .OR. !lVisible
         Shell_NotifyIcon( IIF( lVisible, NIM_ADD, NIM_DELETE ), tray )
      ENDIF
   ENDIF
RETURN Self

METHOD __SetText( cText ) CLASS NotifyIcon
   LOCAL tray
   IF ::__ClassInst == NIL .AND. ::Owner != NIL .AND. ::Owner:hWnd != NIL
      tray := (struct NOTIFYICONDATA)
      tray:cbSize := NOTIFYICONDATA_V2_SIZE
      tray:hWnd   := ::Owner:hWnd
      tray:uID    := ::Id
      tray:uFlags := NIF_TIP | NIF_INFO
      tray:szTip:Buffer( cText )
      Shell_NotifyIcon( NIM_MODIFY, tray )
   ENDIF
RETURN Self

METHOD __SetIcon( cIcon ) CLASS NotifyIcon
   LOCAL tray
   IF ::__hIcon != NIL
      DestroyIcon( ::__hIcon )
      ::__hIcon := NIL
   ENDIF

   SWITCH VALTYPE( cIcon )
      CASE "A"
           IF ::__ClassInst == NIL .OR. EMPTY( cIcon[1] )
              ::__hIcon := LoadIcon( ::AppInstance, cIcon[2] )
              cIcon := cIcon[2]
            ELSE
              ::__hIcon := LoadImage( ::AppInstance, cIcon[1], IMAGE_ICON,,, LR_LOADFROMFILE )
              cIcon := cIcon[1]
           ENDIF
           EXIT
           
      CASE "C"
           //::__hIcon := LoadImage( ::AppInstance, cIcon, IMAGE_ICON,,, LR_LOADFROMFILE )
           ::__hIcon := LoadImage( ::AppInstance, cIcon, IMAGE_ICON,,, IIF( AT( ".ico", LOWER(cIcon) )>0,LR_LOADFROMFILE,) )
           EXIT

      CASE "N"
           ::__hIcon := cIcon
           EXIT
   END
   IF ::__ClassInst == NIL .AND. ( ::__hIcon != NIL .OR. ::Owner:__hIcon != NIL )
      tray := (struct NOTIFYICONDATA)
      tray:cbSize           := NOTIFYICONDATA_V2_SIZE
      tray:hIcon            := IIF( ::__hIcon != NIL, ::__hIcon, ::Owner:__hIcon )
      tray:hWnd             := ::Owner:hWnd
      tray:uID              := ::Id
      tray:uFlags           := ::Flags
      tray:uCallbackMessage := ::Message
      Shell_NotifyIcon( NIM_MODIFY, tray )
   ENDIF
   
   IF ::__ClassInst != NIL 
      IF !EMPTY( ::xIcon ) .AND. EMPTY( cIcon )
         ::Application:Project:RemoveImage( ::xIcon, Self )
      ENDIF
      IF !EMPTY( cIcon )
         ::Application:Project:AddImage( cIcon, IMAGE_ICON, Self )
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD SetBalloonTipIcon(n) CLASS NotifyIcon
   ::xBalloonTipIcon := n
RETURN Self
         