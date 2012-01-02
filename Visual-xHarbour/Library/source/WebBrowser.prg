/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// WebBrowser.prg                                                                                       *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "ole.ch"
#include "debug.ch"

#define HKEY_CLASSES_ROOT           0x80000000
#define HKEY_CURRENT_USER           0x80000001
#define HKEY_LOCAL_MACHINE          0x80000002
#define HKEY_USERS                  0x80000003
#define HKEY_PERFORMANCE_DATA       0x80000004
#define HKEY_CURRENT_CONFIG         0x80000005
#define HKEY_DYN_DATA               0x80000006

#define KEY_ALL_ACCESS              (0xF003F)
#define REG_SZ                      1

CLASS WebBrowser INHERIT ActiveX
   DATA ProgID        EXPORTED
   DATA ClsID         EXPORTED

   PROPERTY Url READ xUrl WRITE WebNavigate PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD WebNavigate()
   METHOD OnGetDlgCode() INLINE DLGC_WANTALLKEYS
ENDCLASS

METHOD Init( oParent ) CLASS WebBrowser
   DEFAULT ::__xCtrlName TO "WebBrowser"
   Super:Init( oParent )
   ::ProgID  := "Shell.Explorer.2"
RETURN Self

METHOD Create() CLASS WebBrowser
   Super:Create()
   IF !EMPTY( ::Url )
      ::WebNavigate( ::Url )
   ENDIF
RETURN Self

METHOD WebNavigate( Url ) CLASS WebBrowser
   IF ::__ClassInst == NIL .AND. ::hObj != NIL
      ::Navigate( Url )
   ENDIF
RETURN Self

#endif