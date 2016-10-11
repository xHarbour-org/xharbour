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

#define EMULATION_IE_07             0x1B58  //Webpages containing standards-based !DOCTYPE directives are displayed in IE7 Standards mode. Default value for applications hosting the WebBrowser Control.
#define EMULATION_IE_08_STD         0x22B8  //Webpages are displayed in IE8 Standards mode, regardless of the !DOCTYPE directive.
#define EMULATION_IE_08             0x1F40  //Webpages containing standards-based !DOCTYPE directives are displayed in IE8 mode. Default value for Internet Explorer 8

#define EMULATION_IE_09_STD         0x270F  //Windows Internet Explorer 9. Webpages are displayed in IE9 Standards mode, regardless of the !DOCTYPE directive.
#define EMULATION_IE_09             0x2328  //Internet Explorer 9. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode. Default value for Internet Explorer 9.

#define EMULATION_IE_10_STD         0x2711  //Internet Explorer 10. Webpages are displayed in IE10 Standards mode, regardless of the !DOCTYPE directive.
#define EMULATION_IE_10             0x02710 //Internet Explorer 10. Webpages containing standards-based !DOCTYPE directives are displayed in IE10 Standards mode. Default value for Internet Explorer 10.

#define EMULATION_IE_11_EDG         0x2AF9  //Internet Explorer 11. Webpages are displayed in IE11 edge mode, regardless of the !DOCTYPE directive.
#define EMULATION_IE_11             0x2AF8  //IE11. Webpages containing standards-based !DOCTYPE directives are displayed in IE11 edge mode. Default value for IE11.

CLASS WebBrowser INHERIT ActiveX
   PROPERTY Url SET ::WebNavigate(v)
   PROPERTY BrowserEmulation  DEFAULT EMULATION_IE_07

   DATA EnumBrowserEmulation EXPORTED INIT { {"None",;
                                              "IE 8 Standards",;
                                              "IE 8 Default",;
                                              "IE 9 Standards",;
                                              "IE 9 Default",;
                                              "IE 10 Standards",;
                                              "IE 10 Default",;
                                              "IE 11 Edge",;
                                              "IE 11 Default"}, {EMULATION_IE_07,;
                                                                 EMULATION_IE_08_STD,;
                                                                 EMULATION_IE_08,;
                                                                 EMULATION_IE_09_STD,;
                                                                 EMULATION_IE_09,;
                                                                 EMULATION_IE_10_STD,;
                                                                 EMULATION_IE_10,;
                                                                 EMULATION_IE_11_EDG,;
                                                                 EMULATION_IE_11} }
   DATA ProgID        EXPORTED
   DATA ClsID         EXPORTED

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
   LOCAL oReg
   IF ! ::DesignMode
      oReg := Registry( HKEY_CURRENT_USER, "Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION" )
      IF ::BrowserEmulation <> EMULATION_IE_07
         IF oReg:Create()
            oReg:SetValue( ::Application:Name + ".exe", ::BrowserEmulation )
            oReg:Close()
         ENDIF
       ELSE
         IF oReg:Open()
            oReg:Delete( ::Application:Name + ".exe" )
            oReg:Close()
         ENDIF
      ENDIF
   ENDIF
   Super:Create()
   IF ! EMPTY( ::Url )
      ::WebNavigate( ::Url )
   ENDIF
RETURN Self

METHOD WebNavigate( Url ) CLASS WebBrowser
   IF ! ::DesignMode .AND. ::hObj != NIL
      ::Navigate( Url )
   ENDIF
RETURN Self


/*
32 bit:

    HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Internet Explorer\MAIN\FeatureControl\FEATURE_BROWSER_EMULATION

    Value Key: yourapplication.exe

64 bit:

    HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Internet Explorer\MAIN\FeatureControl\FEATURE_BROWSER_EMULATION

    Value Key: yourapplication.exe

The value to set this key to is (taken from MSDN here) as decimal values:

9999 (0x270F)
Internet Explorer 9. Webpages are displayed in IE9 Standards mode, regardless of the !DOCTYPE directive.

9000 (0x2328)
Internet Explorer 9. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode.

8888 (0x22B8)
Webpages are displayed in IE8 Standards mode, regardless of the !DOCTYPE directive.

8000 (0x1F40)
Webpages containing standards-based !DOCTYPE directives are displayed in IE8 mode.

7000 (0x1B58)
Webpages containing standards-based !DOCTYPE directives are displayed in IE7 Standards mode.
*/