/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// System.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

static oSystem

#include "vxh.ch"
#include "Structures.ch"
#include "ole.ch"
#include "commdlg.ch"
#include "debug.ch"

#define LVGA_HEADER_LEFT        0x00000001
#define LVGA_HEADER_CENTER      0x00000002
#define LVGA_HEADER_RIGHT       0x00000004

#define CSIDL_MYMUSIC           0x000d
#define CSIDL_MYVIDEO           0x000e

#define SHGFP_TYPE_CURRENT      0

#define DEF_SCHEME "Classic"

INIT PROCEDURE __InitSystem
   oSystem := System( NIL )
RETURN

EXIT PROCEDURE __SystemCleanup
   oSystem:ImageList[ "StdSmall" ]:Destroy()
RETURN

FUNCTION __GetSystem(); RETURN oSystem

CLASS System
   DATA OsVersion              EXPORTED
   DATA StdIcons               EXPORTED
   DATA ListBox                EXPORTED
   DATA Cursor                 EXPORTED
   DATA ImageList              EXPORTED
   DATA ExplorerBar            EXPORTED
   DATA DateTimeFormat         EXPORTED
   DATA TextAlignment          EXPORTED
   DATA ListViewGroupAlign     EXPORTED
   DATA DropDownStyle          EXPORTED
   DATA DataDrivers            EXPORTED
   DATA AdsDataDrivers         EXPORTED
   DATA Folders                EXPORTED
   DATA OleVerb                EXPORTED
   DATA Color                  EXPORTED
   DATA Colors                 EXPORTED
   DATA SockProtocol           EXPORTED
   DATA HandShake              EXPORTED
   DATA Parity                 EXPORTED
   DATA StopBits               EXPORTED
   DATA ProfessionalColorTable EXPORTED
   DATA KeyboardList           EXPORTED
   DATA WindowAnimation        EXPORTED
   DATA FreeImageFormats       EXPORTED
   DATA __ToolStripFlags       EXPORTED
   DATA CurrentScheme          EXPORTED
   DATA Time                   EXPORTED
   DATA xLocalTime             PROTECTED
   DATA PageSetup              EXPORTED
   DATA PaperSize              EXPORTED
   
   ACCESS LocalTime     INLINE ::GetLocalTime()
   ACCESS RootFolders   INLINE ::Folders
   ACCESS LastError     INLINE STRTRAN( FormatMessage( , , GetLastError() ), CRLF )
   
   METHOD Init() CONSTRUCTOR
   METHOD Update()
   
   METHOD GetPathFromFolder()
   METHOD GetLocalTime()
   METHOD GetFocus()
   METHOD GetRunningProcs()
   METHOD IsProcRunning()
   METHOD UpdateColorSchemes() INLINE ::CurrentScheme:Load()
   ACCESS Services             INLINE __ENUMSERVICES()

ENDCLASS

METHOD GetPathFromFolder( nId, lCreate ) CLASS System
   LOCAL cPath := ""
   DEFAULT lCreate TO .F.
   If lCreate
      nId := nId | CSIDL_FLAG_CREATE
   Endif
   SHGetFolderPath( 0, nId, 0, SHGFP_TYPE_CURRENT, @cPath )
RETURN cPath


METHOD GetLocalTime() CLASS System
   LOCAL st
   GetLocalTime( @st )
   ::xLocalTime[ "Year"         ] := st:wYear
   ::xLocalTime[ "Month"        ] := st:wMonth
   ::xLocalTime[ "DayOfWeek"    ] := st:wDayOfWeek
   ::xLocalTime[ "Day"          ] := st:wDay
   ::xLocalTime[ "Hour"         ] := st:wHour
   ::xLocalTime[ "Minute"       ] := st:wMinute
   ::xLocalTime[ "Second"       ] := st:wSecond
   ::xLocalTime[ "Milliseconds" ] := st:wMilliseconds
RETURN ::xLocalTime

METHOD GetFocus() CLASS System
   LOCAL pPtr, hWnd := GetFocus()
   IF IsWindow( hWnd )
      pPtr := GetProp( hWnd, "PROP_CLASSOBJECT" )
      IF pPtr != NIL .AND. pPtr != 0
         RETURN ArrayFromPointer( pPtr )
      ENDIF
   ENDIF
RETURN NIL

METHOD Init() CLASS System
   LOCAL cRdd, aList, hSmall, hLarge, cBuffer := ""
   LOCAL cSupp := ""

   ::FreeImageFormats := {;
                   { "Windows or OS/2 Bitmap (*.bmp)",                   "*.bmp;" },;
                   { "Dr. Halo (*.cut)",                                 "*.cut;" },;
                   { "DirectX Sur face (*.dds)",                         "*.dds;" },;
                   { "ILM OpenEXR (*.exr)",                              "*.exr;" },;
                   { "Raw fax format CCITT G.3 (*.g3)",                  "*.g3;" },;
                   { "Graphics Interchange Format (*.gif)",              "*.gif;" },;
                   { "High Dynamic Range (*.hdr)",                       "*.hdr;" },;
                   { "Windows Icon (*.ico)",                             "*.ico;" },;
                   { "IFF Interleaved Bitmap (*.iff,*.lbm)",             "*.iff;*.lbm;" },;
                   { "JPEG-2000 codestream (*.j2k,*.j2c)",               "*.j2k;*.j2c;" },;
                   { "JPEG Network Graphics (*.jng)",                    "*.jng;" },;
                   { "JPEG-2000 File Format (*.jp2)",                    "*.jp2;" },;
                   { "JPEG - JFIF Compliant (*.jpg,*.jif,*.jpeg,*.jpe)", "*.jpg;*.jif;*.jpeg;*.jpe;" },;
                   { "C64 Koala Graphics(*.k,*.oa)",                     "*.k;*.oa;" },;
                   { "Multiple Network Graphics (*.mng)",                "*.mng;" },;
                   { "Portable Bitmap (ASCII) (*.pbm)",                  "*.pbm;" },;
                   { "Portable Bitmap (RAW) (*.pbm)",                    "*.pbm;" },;
                   { "Kodak PhotoCD (*.pcd)",                            "*.pcd;" },;
                   { "Zsoft Paintbrush (*.pcx)",                         "*.pcx;" },;
                   { "Portable Floatmap (*.pfm)",                        "*.pfm;" },;
                   { "Portable Greymap (ASCII) (*.pgm)",                 "*.pgm;" },;
                   { "Portable Greymap (RAW) (*.pgm)",                   "*.pgm;" },;
                   { "Portable Network Graphics (*.png)",                "*.png;" },;
                   { "Portable Pixelmap (ASCII) (*.ppm)",                "*.ppm;" },;
                   { "Portable Pixelmap (RAW) (*.ppm)",                  "*.ppm;" },;
                   { "Adobe Photoshop (*.psd)",                          "*.psd;" },;
                   { "Sun Raster Image (*.ras)",                         "*.ras;" },;
                   { "Silicon Graphics SGI image format (*.sgi)",        "*.sgi;" },;
                   { "Truevision Targa (*.tga,*.targa)",                 "*.tga;*.targa;" },;
                   { "Tagged Image File Format (*.tif,*.tiff)",          "*.tif;*.tiff;" },;
                   { "Wireles s Bitmap (*.wap,*.wbmp,*.wbm)",            "*.wap;*.wbmp;*.wbm;" },;
                   { "X11 Bitmap Format (*.xbm)",                        "*.xbm;" },;
                   { "X11 Pixmap Format (*.xpm)",                        "*.xpm"  } }

   AEVAL( ::FreeImageFormats, {|a| cSupp += a[2]} )
   AINS( ::FreeImageFormats, 1, { "All Supported Graphics", cSupp }, .T. )

   ::OsVersion        := (struct OSVERSIONINFOEX)
   GetVersionEx( @::OsVersion )

   ::CurrentScheme := ProfessionalColorTable( NIL )
   ::CurrentScheme:Load()

   ::__ToolStripFlags := {=>}
   HSetCaseMatch( ::__ToolStripFlags, .F. )
   ::__ToolStripFlags[ "s_lExecuting" ]    := .F.
   ::__ToolStripFlags[ "s_CurrFocus" ]     := NIL
   ::__ToolStripFlags[ "s_CurrentObject" ] := NIL
   ::__ToolStripFlags[ "s_lKey" ]          := .F.
   ::__ToolStripFlags[ "s_lOpenMenu" ]     := .T.
   ::__ToolStripFlags[ "s_hKeyMenuHook" ]  := NIL


   ::WindowAnimation := {=>}
   HSetCaseMatch( ::WindowAnimation, .F. )
   ::WindowAnimation[ "None"               ] := 0
   ::WindowAnimation[ "Blend"              ] := AW_BLEND
   ::WindowAnimation[ "Center"             ] := AW_CENTER
   ::WindowAnimation[ "SlideHorzPositive"  ] := AW_SLIDE | AW_HOR_POSITIVE
   ::WindowAnimation[ "SlideHorzNegative"  ] := AW_SLIDE | AW_HOR_NEGATIVE
   ::WindowAnimation[ "SlideVertPositive"  ] := AW_SLIDE | AW_VER_POSITIVE
   ::WindowAnimation[ "SlideVertNegative"  ] := AW_SLIDE | AW_VER_NEGATIVE

   ::KeyboardList := {=>}
   HSetCaseMatch( ::KeyboardList, .F. )
   
   ::KeyboardList["A"] := 65
   ::KeyboardList["B"] := 65
   ::KeyboardList["Backspace"] := 32
   ::KeyboardList["C"] := 65
   ::KeyboardList["D"] := 65
   ::Time := __SysTime()
   
   ::xLocalTime := {=>}
   HSetCaseMatch( ::xLocalTime, .F. )
   ::GetLocalTime()


   ::StopBits := {=>}
   HSetCaseMatch( ::StopBits, .F. )
   ::StopBits[ "None"         ] := -1
   ::StopBits[ "One"          ] := 0
   ::StopBits[ "Two"          ] := 2
   ::StopBits[ "OnePointFive" ] := 1

   ::Parity := {=>}
   HSetCaseMatch( ::Parity, .F. )
   ::Parity[ "None"       ] := 0
   ::Parity[ "Odd"        ] := 1
   ::Parity[ "Even"       ] := 2
   ::Parity[ "Mark"       ] := 3
   ::Parity[ "Space"      ] := 4

   ::HandShake := {=>}
   HSetCaseMatch( ::HandShake, .F. )
   ::HandShake[ "None"                ] := 1
   ::HandShake[ "XOnOff"              ] := 2
   ::HandShake[ "RequestToSend"       ] := 3
   ::HandShake[ "RequestToSendXOnOff" ] := 4

   
   ::SockProtocol := {=>}
   HSetCaseMatch( ::SockProtocol, .F. )
   ::SockProtocol[ "TCP"           ]   := 1
   ::SockProtocol[ "UDP"           ]   := 2
   
   ::Colors := __SystemColors()

   ::Color := {=>}
   HSetCaseMatch( ::Color, .F. )
   ::Color[ "AliceBlue"            ]   := CHGCOLORREF( 0xF0F8FF )
   ::Color[ "AntiqueWhite"         ]   := CHGCOLORREF( 0xFAEBD7 )
   ::Color[ "Aqua"                 ]   := CHGCOLORREF( 0x00FFFF )
   ::Color[ "Aquamarine"           ]   := CHGCOLORREF( 0x7FFFD4 )
   ::Color[ "Azure"                ]   := CHGCOLORREF( 0xF0FFFF )
   ::Color[ "Beige"                ]   := CHGCOLORREF( 0xF5F5DC )
   ::Color[ "Bisque"               ]   := CHGCOLORREF( 0xFFE4C4 )
   ::Color[ "Black"                ]   := CHGCOLORREF( 0x000000 )
   ::Color[ "BlanchedAlmond"       ]   := CHGCOLORREF( 0xFFEBCD )
   ::Color[ "Blue"                 ]   := CHGCOLORREF( 0x0000FF )
   ::Color[ "BlueViolet"           ]   := CHGCOLORREF( 0x8A2BE2 )
   ::Color[ "Brown"                ]   := CHGCOLORREF( 0xA52A2A )
   ::Color[ "BurlyWood"            ]   := CHGCOLORREF( 0xDEB887 )
   ::Color[ "CadetBlue"            ]   := CHGCOLORREF( 0x5F9EA0 )
   ::Color[ "Chartreuse"           ]   := CHGCOLORREF( 0x7FFF00 )
   ::Color[ "Chocolate"            ]   := CHGCOLORREF( 0xD2691E )
   ::Color[ "Coral"                ]   := CHGCOLORREF( 0xFF7F50 )
   ::Color[ "CornflowerBlue"       ]   := CHGCOLORREF( 0x6495ED )
   ::Color[ "Cornsilk"             ]   := CHGCOLORREF( 0xFFF8DC )
   ::Color[ "Crimson"              ]   := CHGCOLORREF( 0xDC143C )
   ::Color[ "Cyan"                 ]   := CHGCOLORREF( 0x00FFFF )
   ::Color[ "DarkBlue"             ]   := CHGCOLORREF( 0x00008B )
   ::Color[ "DarkCyan"             ]   := CHGCOLORREF( 0x008B8B )
   ::Color[ "DarkGoldenrod"        ]   := CHGCOLORREF( 0xB8860B )
   ::Color[ "DarkGray"             ]   := CHGCOLORREF( 0xA9A9A9 )
   ::Color[ "DarkGreen"            ]   := CHGCOLORREF( 0x006400 )
   ::Color[ "DarkGrey"             ]   := CHGCOLORREF( 0xA9A9A9 )
   ::Color[ "DarkKhaki"            ]   := CHGCOLORREF( 0xBDB76B )
   ::Color[ "DarkMagenta"          ]   := CHGCOLORREF( 0x8B008B )
   ::Color[ "DarkOliveGreen"       ]   := CHGCOLORREF( 0x556B2F )
   ::Color[ "DarkOrange"           ]   := CHGCOLORREF( 0xFF8C00 )
   ::Color[ "DarkOrchid"           ]   := CHGCOLORREF( 0x9932CC )
   ::Color[ "DarkRed"              ]   := CHGCOLORREF( 0x8B0000 )
   ::Color[ "DarkSalmon"           ]   := CHGCOLORREF( 0xE9967A )
   ::Color[ "DarkSeaGreen"         ]   := CHGCOLORREF( 0x8FBC8F )
   ::Color[ "DarkSlateBlue"        ]   := CHGCOLORREF( 0x483D8B )
   ::Color[ "DarkSlateGray"        ]   := CHGCOLORREF( 0x2F4F4F )
   ::Color[ "DarkTurquoise"        ]   := CHGCOLORREF( 0x00CED1 )
   ::Color[ "DarkViolet"           ]   := CHGCOLORREF( 0x9400D3 )
   ::Color[ "DeepPink"             ]   := CHGCOLORREF( 0xFF1493 )
   ::Color[ "DeepSkyBlue"          ]   := CHGCOLORREF( 0x00BFFF )
   ::Color[ "DimGray"              ]   := CHGCOLORREF( 0x696969 )
   ::Color[ "DkBlue"               ]   := CHGCOLORREF( 0x00008B )
   ::Color[ "DkCyan"               ]   := CHGCOLORREF( 0x008B8B )
   ::Color[ "DkGoldenrod"          ]   := CHGCOLORREF( 0xB8860B )
   ::Color[ "DkGray"               ]   := CHGCOLORREF( 0xA9A9A9 )
   ::Color[ "DkGreen"              ]   := CHGCOLORREF( 0x006400 )
   ::Color[ "DkGrey"               ]   := CHGCOLORREF( 0xA9A9A9 )
   ::Color[ "DkKhaki"              ]   := CHGCOLORREF( 0xBDB76B )
   ::Color[ "DkMagenta"            ]   := CHGCOLORREF( 0x8B008B )
   ::Color[ "DkOliveGreen"         ]   := CHGCOLORREF( 0x556B2F )
   ::Color[ "DkOrange"             ]   := CHGCOLORREF( 0xFF8C00 )
   ::Color[ "DkOrchid"             ]   := CHGCOLORREF( 0x9932CC )
   ::Color[ "DkRed"                ]   := CHGCOLORREF( 0x8B0000 )
   ::Color[ "DkSalmon"             ]   := CHGCOLORREF( 0xE9967A )
   ::Color[ "DkSeaGreen"           ]   := CHGCOLORREF( 0x8FBC8F )
   ::Color[ "DkSlateBlue"          ]   := CHGCOLORREF( 0x483D8B )
   ::Color[ "DkSlateGray"          ]   := CHGCOLORREF( 0x2F4F4F )
   ::Color[ "DkTurquoise"          ]   := CHGCOLORREF( 0x00CED1 )
   ::Color[ "DkViolet"             ]   := CHGCOLORREF( 0x9400D3 )
   ::Color[ "DodgerBlue"           ]   := CHGCOLORREF( 0x1E90FF )
   ::Color[ "FireBrick"            ]   := CHGCOLORREF( 0xB22222 )
   ::Color[ "FloralWhite"          ]   := CHGCOLORREF( 0xFFFAF0 )
   ::Color[ "ForestGreen"          ]   := CHGCOLORREF( 0x228B22 )
   ::Color[ "Fuchsia"              ]   := CHGCOLORREF( 0xFF00FF )
   ::Color[ "Gainsboro"            ]   := CHGCOLORREF( 0xDCDCDC )
   ::Color[ "GhostWhite"           ]   := CHGCOLORREF( 0xF8F8FF )
   ::Color[ "Gold"                 ]   := CHGCOLORREF( 0xFFD700 )
   ::Color[ "Goldenrod"            ]   := CHGCOLORREF( 0xDAA520 )
   ::Color[ "Gray"                 ]   := CHGCOLORREF( 0x808080 )
   ::Color[ "Green"                ]   := CHGCOLORREF( 0x008000 )
   ::Color[ "GreenYellow"          ]   := CHGCOLORREF( 0xADFF2F )
   ::Color[ "Grey"                 ]   := CHGCOLORREF( 0x808080 )
   ::Color[ "Honeydew"             ]   := CHGCOLORREF( 0xF0FFF0 )
   ::Color[ "HotPink"              ]   := CHGCOLORREF( 0xFF69B4 )
   ::Color[ "IndianRed"            ]   := CHGCOLORREF( 0xCD5C5C )
   ::Color[ "Indigo"               ]   := CHGCOLORREF( 0x4B0082 )
   ::Color[ "Ivory"                ]   := CHGCOLORREF( 0xFFFFF0 )
   ::Color[ "Khaki"                ]   := CHGCOLORREF( 0xF0E68C )
   ::Color[ "Lavender"             ]   := CHGCOLORREF( 0xE6E6FA )
   ::Color[ "LavenderBlush"        ]   := CHGCOLORREF( 0xFFF0F5 )
   ::Color[ "LawnGreen"            ]   := CHGCOLORREF( 0x7CFC00 )
   ::Color[ "LemonChiffon"         ]   := CHGCOLORREF( 0xFFFACD )
   ::Color[ "LightBlue"            ]   := CHGCOLORREF( 0xADD8E6 )
   ::Color[ "LightCoral"           ]   := CHGCOLORREF( 0xF08080 )
   ::Color[ "LightCyan"            ]   := CHGCOLORREF( 0xE0FFFF )
   ::Color[ "LightGoldenrodYellow" ]   := CHGCOLORREF( 0xFAFAD2 )
   ::Color[ "LightGray"            ]   := CHGCOLORREF( 0xD3D3D3 )
   ::Color[ "LightGreen"           ]   := CHGCOLORREF( 0x90EE90 )
   ::Color[ "LightGrey"            ]   := CHGCOLORREF( 0xD3D3D3 )
   ::Color[ "LightPink"            ]   := CHGCOLORREF( 0xFFB6C1 )
   ::Color[ "LightSalmon"          ]   := CHGCOLORREF( 0xFFA07A )
   ::Color[ "LightSeaGreen"        ]   := CHGCOLORREF( 0x20B2AA )
   ::Color[ "LightSkyBlue"         ]   := CHGCOLORREF( 0x87CEFA )
   ::Color[ "LightSlateGray"       ]   := CHGCOLORREF( 0x778899 )
   ::Color[ "LightSteelBlue"       ]   := CHGCOLORREF( 0xB0C4DE )
   ::Color[ "LightYellow"          ]   := CHGCOLORREF( 0xFFFFE0 )
   ::Color[ "Lime"                 ]   := CHGCOLORREF( 0x00FF00 )
   ::Color[ "LimeGreen"            ]   := CHGCOLORREF( 0x32CD32 )
   ::Color[ "Linen"                ]   := CHGCOLORREF( 0xFAF0E6 )
   ::Color[ "LtBlue"               ]   := CHGCOLORREF( 0xADD8E6 )
   ::Color[ "LtCoral"              ]   := CHGCOLORREF( 0xF08080 )
   ::Color[ "LtCyan"               ]   := CHGCOLORREF( 0xE0FFFF )
   ::Color[ "LtGoldenrodYellow"    ]   := CHGCOLORREF( 0xFAFAD2 )
   ::Color[ "LtGray"               ]   := CHGCOLORREF( 0xD3D3D3 )
   ::Color[ "LtGreen"              ]   := CHGCOLORREF( 0x90EE90 )
   ::Color[ "LtGrey"               ]   := CHGCOLORREF( 0xD3D3D3 )
   ::Color[ "LtPink"               ]   := CHGCOLORREF( 0xFFB6C1 )
   ::Color[ "LtSalmon"             ]   := CHGCOLORREF( 0xFFA07A )
   ::Color[ "LtSeaGreen"           ]   := CHGCOLORREF( 0x20B2AA )
   ::Color[ "LtSkyBlue"            ]   := CHGCOLORREF( 0x87CEFA )
   ::Color[ "LtSlateGray"          ]   := CHGCOLORREF( 0x778899 )
   ::Color[ "LtSteelBlue"          ]   := CHGCOLORREF( 0xB0C4DE )
   ::Color[ "LtYellow"             ]   := CHGCOLORREF( 0xFFFFE0 )
   ::Color[ "Magenta"              ]   := CHGCOLORREF( 0xFF00FF )
   ::Color[ "Maroon"               ]   := CHGCOLORREF( 0x800000 )
   ::Color[ "MediumAquamarine"     ]   := CHGCOLORREF( 0x66CDAA )
   ::Color[ "MediumBlue"           ]   := CHGCOLORREF( 0x0000CD )
   ::Color[ "MediumOrchid"         ]   := CHGCOLORREF( 0xBA55D3 )
   ::Color[ "MediumPurple"         ]   := CHGCOLORREF( 0x9370DB )
   ::Color[ "MediumSeaGreen"       ]   := CHGCOLORREF( 0x3CB371 )
   ::Color[ "MediumSlateBlue"      ]   := CHGCOLORREF( 0x7B68EE )
   ::Color[ "MediumSpringGreen"    ]   := CHGCOLORREF( 0x00FA9A )
   ::Color[ "MediumTurquoise"      ]   := CHGCOLORREF( 0x48D1CC )
   ::Color[ "MediumVioletRed"      ]   := CHGCOLORREF( 0xC71585 )
   ::Color[ "MidnightBlue"         ]   := CHGCOLORREF( 0x191970 )
   ::Color[ "MintCream"            ]   := CHGCOLORREF( 0xF5FFFA )
   ::Color[ "MistyRose"            ]   := CHGCOLORREF( 0xFFE4E1 )
   ::Color[ "Moccasin"             ]   := CHGCOLORREF( 0xFFE4B5 )
   ::Color[ "NavajoWhite"          ]   := CHGCOLORREF( 0xFFDEAD )
   ::Color[ "Navy"                 ]   := CHGCOLORREF( 0x000080 )
   ::Color[ "OldLace"              ]   := CHGCOLORREF( 0xFDF5E6 )
   ::Color[ "Olive"                ]   := CHGCOLORREF( 0x808000 )
   ::Color[ "OliveDrab"            ]   := CHGCOLORREF( 0x6B8E23 )
   ::Color[ "Orange"               ]   := CHGCOLORREF( 0xFFA500 )
   ::Color[ "OrangeRed"            ]   := CHGCOLORREF( 0xFF4500 )
   ::Color[ "Orchid"               ]   := CHGCOLORREF( 0xDA70D6 )
   ::Color[ "PaleGoldenrod"        ]   := CHGCOLORREF( 0xEEE8AA )
   ::Color[ "PaleGreen"            ]   := CHGCOLORREF( 0x98FB98 )
   ::Color[ "PaleTurquoise"        ]   := CHGCOLORREF( 0xAFEEEE )
   ::Color[ "PaleVioletRed"        ]   := CHGCOLORREF( 0xDB7093 )
   ::Color[ "PapayaWhip"           ]   := CHGCOLORREF( 0xFFEFD5 )
   ::Color[ "PeachPuff"            ]   := CHGCOLORREF( 0xFFDAB9 )
   ::Color[ "Peru"                 ]   := CHGCOLORREF( 0xCD853F )
   ::Color[ "Pink"                 ]   := CHGCOLORREF( 0xFFC0CB )
   ::Color[ "Plum"                 ]   := CHGCOLORREF( 0xDDA0DD )
   ::Color[ "PowderBlue"           ]   := CHGCOLORREF( 0xB0E0E6 )
   ::Color[ "Purple"               ]   := CHGCOLORREF( 0x800080 )
   ::Color[ "Red"                  ]   := CHGCOLORREF( 0xFF0000 )
   ::Color[ "RosyBrown"            ]   := CHGCOLORREF( 0xBC8F8F )
   ::Color[ "RoyalBlue"            ]   := CHGCOLORREF( 0x4169E1 )
   ::Color[ "SaddleBrown"          ]   := CHGCOLORREF( 0x8B4513 )
   ::Color[ "Salmon"               ]   := CHGCOLORREF( 0xFA8072 )
   ::Color[ "SandyBrown"           ]   := CHGCOLORREF( 0xF4A460 )
   ::Color[ "SeaGreen"             ]   := CHGCOLORREF( 0x2E8B57 )
   ::Color[ "Seashell"             ]   := CHGCOLORREF( 0xFFF5EE )
   ::Color[ "Sienna"               ]   := CHGCOLORREF( 0xA0522D )
   ::Color[ "Silver"               ]   := CHGCOLORREF( 0xC0C0C0 )
   ::Color[ "SkyBlue"              ]   := CHGCOLORREF( 0x87CEEB )
   ::Color[ "SlateBlue"            ]   := CHGCOLORREF( 0x6A5ACD )
   ::Color[ "SlateGray"            ]   := CHGCOLORREF( 0x708090 )
   ::Color[ "Snow"                 ]   := CHGCOLORREF( 0xFFFAFA )
   ::Color[ "SpringGreen"          ]   := CHGCOLORREF( 0x00FF7F )
   ::Color[ "SteelBlue"            ]   := CHGCOLORREF( 0x4682B4 )
   ::Color[ "Tan"                  ]   := CHGCOLORREF( 0xD2B48C )
   ::Color[ "Teal"                 ]   := CHGCOLORREF( 0x008080 )
   ::Color[ "Thistle"              ]   := CHGCOLORREF( 0xD8BFD8 )
   ::Color[ "Tomato"               ]   := CHGCOLORREF( 0xFF6347 )
   ::Color[ "Turquoise"            ]   := CHGCOLORREF( 0x40E0D0 )
   ::Color[ "Violet"               ]   := CHGCOLORREF( 0xEE82EE )
   ::Color[ "Wheat"                ]   := CHGCOLORREF( 0xF5DEB3 )
   ::Color[ "White"                ]   := CHGCOLORREF( 0xFFFFFF )
   ::Color[ "WhiteSmoke"           ]   := CHGCOLORREF( 0xF5F5F5 )
   ::Color[ "Yellow"               ]   := CHGCOLORREF( 0xFFFF00 )
   ::Color[ "YellowGreen"          ]   := CHGCOLORREF( 0x9ACD32 )


   ::OleVerb := {=>}
   HSetCaseMatch( ::OleVerb, .F. )
   ::OleVerb["Hide"]                      := OLEIVERB_HIDE
   ::OleVerb["InPlaceActivate"]           := OLEIVERB_INPLACEACTIVATE
   ::OleVerb["Open"]                      := OLEIVERB_OPEN
   ::OleVerb["Primary"]                   := OLEIVERB_PRIMARY
   ::OleVerb["Show"]                      := OLEIVERB_SHOW
   ::OleVerb["UIActivate"]                := OLEIVERB_UIACTIVATE
   ::OleVerb["DiscardUndoState"]          := OLEIVERB_DISCARDUNDOSTATE

   ::Folders := {=>}
   HSetCaseMatch( ::Folders, .F. )
   ::Folders["ApplicationData"]       := CSIDL_APPDATA
   ::Folders["ControlPanel"]          := CSIDL_CONTROLS
   ::Folders["CommonApplicationData"] := CSIDL_COMMON_APPDATA
   ::Folders["CommonProgramFiles"]    := CSIDL_PROGRAM_FILES_COMMON
   ::Folders["Cookies"]               := CSIDL_COOKIES
   ::Folders["Desktop"]               := CSIDL_DESKTOP
   ::Folders["DesktopDirectory"]      := CSIDL_DESKTOPDIRECTORY
   ::Folders["Favorites"]             := CSIDL_FAVORITES
   ::Folders["History"]               := CSIDL_HISTORY
   ::Folders["InternetCache"]         := CSIDL_INTERNET_CACHE
   ::Folders["LocalApplicationData"]  := CSIDL_LOCAL_APPDATA
   ::Folders["MyComputer"]            := CSIDL_DRIVES
   ::Folders["MyMusic"]               := CSIDL_MYMUSIC
   ::Folders["MyPictures"]            := CSIDL_MYPICTURES
   ::Folders["MyDocuments"]           := CSIDL_PERSONAL
   ::Folders["MyVideos"]              := CSIDL_MYVIDEO
   ::Folders["MyNetworkPlaces"]       := CSIDL_NETHOOD
   ::Folders["Network"]               := CSIDL_NETHOOD
   ::Folders["ProgramFiles"]          := CSIDL_PROGRAM_FILES
   ::Folders["Programs"]              := CSIDL_PROGRAMS
   ::Folders["Recent"]                := CSIDL_RECENT
   ::Folders["RecycleBin"]            := CSIDL_BITBUCKET
   ::Folders["SendTo"]                := CSIDL_SENDTO
   ::Folders["StartMenu"]             := CSIDL_STARTMENU
   ::Folders["Startup"]               := CSIDL_STARTUP
   ::Folders["System"]                := CSIDL_SYSTEM
   ::Folders["Templates"]             := CSIDL_TEMPLATES
   ::Folders["Windows"]               := CSIDL_WINDOWS

   aList := RDDLIST( 1 )
   ::DataDrivers := {=>}
   HSetCaseMatch( ::DataDrivers, .F. )
   ::AdsDataDrivers := {=>}
   HSetCaseMatch( ::DataDrivers, .F. )
   FOR EACH cRdd IN aList
       IF LEFT( cRdd, 3 ) != "ADS" .AND. LEFT( cRdd, 3 ) != "ADT"
          ::DataDrivers[ cRdd ] := cRdd
        ELSE
          ::AdsDataDrivers[ cRdd ] := cRdd
       ENDIF
   NEXT

   ::DropDownStyle := Hash()
   HSetCaseMatch( ::DropDownStyle, .F. )
   ::DropDownStyle[ "DropDown" ]     := CBS_DROPDOWN
   ::DropDownStyle[ "DropDownList" ] := CBS_DROPDOWNLIST
   ::DropDownStyle[ "Simple" ]       := CBS_SIMPLE

   ::TextAlignment := Hash()
   HSetCaseMatch( ::TextAlignment, .F. )
   ::TextAlignment[ "Center" ]       := DT_CENTER
   ::TextAlignment[ "Left" ]         := DT_LEFT
   ::TextAlignment[ "Right" ]        := DT_RIGHT

   ::ListViewGroupAlign := Hash()
   HSetCaseMatch( ::ListViewGroupAlign, .F. )
   ::ListViewGroupAlign[ "Center" ]  := LVGA_HEADER_CENTER
   ::ListViewGroupAlign[ "Left" ]    := LVGA_HEADER_LEFT
   ::ListViewGroupAlign[ "Right" ]   := LVGA_HEADER_RIGHT

   ::DateTimeFormat := Hash()
   HSetCaseMatch( ::DateTimeFormat, .F. )
   ::DateTimeFormat[ "Short" ]       := DTS_SHORTDATEFORMAT
   ::DateTimeFormat[ "Long" ]        := DTS_LONGDATEFORMAT
   ::DateTimeFormat[ "Time" ]        := DTS_TIMEFORMAT
   ::DateTimeFormat[ "Custom" ]      := 20

   ::StdIcons := Hash()
   HSetCaseMatch( ::StdIcons, .F. )
   ::StdIcons[ "Cut" ]               := STD_CUT        + 1
   ::StdIcons[ "Copy" ]              := STD_COPY       + 1
   ::StdIcons[ "Paste" ]             := STD_PASTE      + 1
   ::StdIcons[ "Undo" ]              := STD_UNDO       + 1
   ::StdIcons[ "Redo" ]              := STD_REDOW      + 1
   ::StdIcons[ "Delete" ]            := STD_DELETE     + 1
   ::StdIcons[ "FileNew" ]           := STD_FILENEW    + 1
   ::StdIcons[ "FileOpen" ]          := STD_FILEOPEN   + 1
   ::StdIcons[ "FileSave" ]          := STD_FILESAVE   + 1
   ::StdIcons[ "PrintPreview" ]      := STD_PRINTPRE   + 1
   ::StdIcons[ "Properties" ]        := STD_PROPERTIES + 1
   ::StdIcons[ "Help" ]              := STD_HELP       + 1
   ::StdIcons[ "Find" ]              := STD_FIND       + 1
   ::StdIcons[ "Replace" ]           := STD_REPLACE    + 1
   ::StdIcons[ "Print" ]             := STD_PRINT      + 1

   ::ListBox := Hash()
   HSetCaseMatch( ::ListBox, .F. )
   ::ListBox[ "OwnerDrawFixed" ]     := 2
   ::ListBox[ "OwnerDrawVariable" ]  := 3

   ::PageSetup := Hash()
   HSetCaseMatch( ::PageSetup, .F. )
   ::PageSetup[ "Portrait" ]   := DMORIENT_PORTRAIT
   ::PageSetup[ "Landscape" ]  := DMORIENT_LANDSCAPE

   ::PaperSize := {}
   AADD( ::PaperSize, { "US Letter 8 1/2 x 11 in",                       DMPAPER_LETTER                       } )
   AADD( ::PaperSize, { "US Letter Small 8 1/2 x 11 in",                 DMPAPER_LETTERSMALL                  } )
   AADD( ::PaperSize, { "US Tabloid 11 x 17 in",                         DMPAPER_TABLOID                      } )
   AADD( ::PaperSize, { "US Ledger 17 x 11 in",                          DMPAPER_LEDGER                       } )
   AADD( ::PaperSize, { "US Legal 8 1/2 x 14 in",                        DMPAPER_LEGAL                        } )
   AADD( ::PaperSize, { "US Statement 5 1/2 x 8 1/2 in",                 DMPAPER_STATEMENT                    } )
   AADD( ::PaperSize, { "US Executive 7 1/4 x 10 1/2 in",                DMPAPER_EXECUTIVE                    } )
   AADD( ::PaperSize, { "A3 297 x 420 mm",                               DMPAPER_A3                           } )
   AADD( ::PaperSize, { "A4 210 x 297 mm",                               DMPAPER_A4                           } )
   AADD( ::PaperSize, { "A4 Small 210 x 297 mm",                         DMPAPER_A4SMALL                      } )
   AADD( ::PaperSize, { "A5 148 x 210 mm",                               DMPAPER_A5                           } )
   AADD( ::PaperSize, { "B4 (JIS) 257 x 364 mm",                         DMPAPER_B4                           } )
   AADD( ::PaperSize, { "B5 (JIS) 182 x 257 mm",                         DMPAPER_B5                           } )
   AADD( ::PaperSize, { "Folio 8 1/2 x 13 in",                           DMPAPER_FOLIO                        } )
   AADD( ::PaperSize, { "Quarto 215 x 275 mm",                           DMPAPER_QUARTO                       } )
   AADD( ::PaperSize, { "10 x 14 in",                                    DMPAPER_10X14                        } )
   AADD( ::PaperSize, { "11 x 17 in",                                    DMPAPER_11X17                        } )
   AADD( ::PaperSize, { "US Note 8 1/2 x 11 in",                         DMPAPER_NOTE                         } )
   AADD( ::PaperSize, { "US Envelope #9 3 7/8 x 8 7/8",                  DMPAPER_ENV_9                        } )
   AADD( ::PaperSize, { "US Envelope #10 4 1/8 x 9 1/2",                 DMPAPER_ENV_10                       } )
   AADD( ::PaperSize, { "US Envelope #11 4 1/2 x 10 3/8",                DMPAPER_ENV_11                       } )
   AADD( ::PaperSize, { "US Envelope #12 4 3/4 x 11 in",                 DMPAPER_ENV_12                       } )
   AADD( ::PaperSize, { "US Envelope #14 5 x 11 1/2",                    DMPAPER_ENV_14                       } )
   AADD( ::PaperSize, { "C size sheet",                                  DMPAPER_CSHEET                       } )
   AADD( ::PaperSize, { "D size sheet",                                  DMPAPER_DSHEET                       } )
   AADD( ::PaperSize, { "E size sheet",                                  DMPAPER_ESHEET                       } )
   AADD( ::PaperSize, { "Envelope DL 110 x 220 mm",                      DMPAPER_ENV_DL                       } )
   AADD( ::PaperSize, { "Envelope C5 162 x 229 mm",                      DMPAPER_ENV_C5                       } )
   AADD( ::PaperSize, { "Envelope C3 324 x 458 mm",                      DMPAPER_ENV_C3                       } )
   AADD( ::PaperSize, { "Envelope C4 229 x 324 mm",                      DMPAPER_ENV_C4                       } )
   AADD( ::PaperSize, { "Envelope C6 114 x 162 mm",                      DMPAPER_ENV_C6                       } )
   AADD( ::PaperSize, { "Envelope C65 114 x 229 mm",                     DMPAPER_ENV_C65                      } )
   AADD( ::PaperSize, { "Envelope B4 250 x 353 mm",                      DMPAPER_ENV_B4                       } )
   AADD( ::PaperSize, { "Envelope B5 176 x 250 mm",                      DMPAPER_ENV_B5                       } )
   AADD( ::PaperSize, { "Envelope B6 176 x 125 mm",                      DMPAPER_ENV_B6                       } )
   AADD( ::PaperSize, { "Envelope 110 x 230 mm",                         DMPAPER_ENV_ITALY                    } )
   AADD( ::PaperSize, { "US Envelope Monarch 3.875 x 7.5 in",            DMPAPER_ENV_MONARCH                  } )
   AADD( ::PaperSize, { "6 3/4 US Envelope 3 5/8 x 6 1/2 in",            DMPAPER_ENV_PERSONAL                 } )
   AADD( ::PaperSize, { "US Std Fanfold 14 7/8 x 11 in",                 DMPAPER_FANFOLD_US                   } )
   AADD( ::PaperSize, { "German Std Fanfold 8 1/2 x 12 in",              DMPAPER_FANFOLD_STD_GERMAN           } )
   AADD( ::PaperSize, { "German Legal Fanfold 8 1/2 x 13 in",            DMPAPER_FANFOLD_LGL_GERMAN           } )
   AADD( ::PaperSize, { "B4 (ISO) 250 x 353 mm",                         DMPAPER_ISO_B4                       } )
   AADD( ::PaperSize, { "Japanese Postcard 100 x 148 mm",                DMPAPER_JAPANESE_POSTCARD            } )
   AADD( ::PaperSize, { "9 x 11 in",                                     DMPAPER_9X11                         } )
   AADD( ::PaperSize, { "10 x 11 in",                                    DMPAPER_10X11                        } )
   AADD( ::PaperSize, { "15 x 11 in",                                    DMPAPER_15X11                        } )
   AADD( ::PaperSize, { "Envelope Invite 220 x 220 mm",                  DMPAPER_ENV_INVITE                   } )
   AADD( ::PaperSize, { "RESERVED--DO NOT USE",                          DMPAPER_RESERVED_48                  } )
   AADD( ::PaperSize, { "RESERVED--DO NOT USE",                          DMPAPER_RESERVED_49                  } )
   AADD( ::PaperSize, { "US Letter Extra 9 1/2 x 12 in",                 DMPAPER_LETTER_EXTRA                 } )
   AADD( ::PaperSize, { "US Legal Extra 9 1/2 x 15 in",                  DMPAPER_LEGAL_EXTRA                  } )
   AADD( ::PaperSize, { "US Tabloid Extra 11.69 x 18 in",                DMPAPER_TABLOID_EXTRA                } )
   AADD( ::PaperSize, { "A4 Extra 9.27 x 12.69 in",                      DMPAPER_A4_EXTRA                     } )
   AADD( ::PaperSize, { "Letter Transverse 8 1/2 x 11 in",               DMPAPER_LETTER_TRANSVERSE            } )
   AADD( ::PaperSize, { "A4 Transverse 210 x 297 mm",                    DMPAPER_A4_TRANSVERSE                } )
   AADD( ::PaperSize, { "Letter Extra Transverse 9 1/2 x 12 in",         DMPAPER_LETTER_EXTRA_TRANSVERSE      } )
   AADD( ::PaperSize, { "SuperA/SuperA/A4 227 x 356 mm",                 DMPAPER_A_PLUS                       } )
   AADD( ::PaperSize, { "SuperB/SuperB/A3 305 x 487 mm",                 DMPAPER_B_PLUS                       } )
   AADD( ::PaperSize, { "US Letter Plus 8.5 x 12.69 in",                 DMPAPER_LETTER_PLUS                  } )
   AADD( ::PaperSize, { "A4 Plus 210 x 330 mm",                          DMPAPER_A4_PLUS                      } )
   AADD( ::PaperSize, { "A5 Transverse 148 x 210 mm",                    DMPAPER_A5_TRANSVERSE                } )
   AADD( ::PaperSize, { "B5 (JIS) Transverse 182 x 257 mm",              DMPAPER_B5_TRANSVERSE                } )
   AADD( ::PaperSize, { "A3 Extra 322 x 445 mm",                         DMPAPER_A3_EXTRA                     } )
   AADD( ::PaperSize, { "A5 Extra 174 x 235 mm",                         DMPAPER_A5_EXTRA                     } )
   AADD( ::PaperSize, { "B5 (ISO) Extra 201 x 276 mm",                   DMPAPER_B5_EXTRA                     } )
   AADD( ::PaperSize, { "A2 420 x 594 mm",                               DMPAPER_A2                           } )
   AADD( ::PaperSize, { "A3 Transverse 297 x 420 mm",                    DMPAPER_A3_TRANSVERSE                } )
   AADD( ::PaperSize, { "A3 Extra Transverse 322 x 445 mm",              DMPAPER_A3_EXTRA_TRANSVERSE          } )
   AADD( ::PaperSize, { "Japanese Double Postcard 200 x 148 mm",         DMPAPER_DBL_JAPANESE_POSTCARD        } )
   AADD( ::PaperSize, { "A6 105 x 148 mm",                               DMPAPER_A6                           } )
   AADD( ::PaperSize, { "Japanese Envelope Kaku #2",                     DMPAPER_JENV_KAKU2                   } )
   AADD( ::PaperSize, { "Japanese Envelope Kaku #3",                     DMPAPER_JENV_KAKU3                   } )
   AADD( ::PaperSize, { "Japanese Envelope Chou #3",                     DMPAPER_JENV_CHOU3                   } )
   AADD( ::PaperSize, { "Japanese Envelope Chou #4",                     DMPAPER_JENV_CHOU4                   } )
   AADD( ::PaperSize, { "Letter Rotated 11 x 8 1/2 11 in",               DMPAPER_LETTER_ROTATED               } )
   AADD( ::PaperSize, { "A3 Rotated 420 x 297 mm",                       DMPAPER_A3_ROTATED                   } )
   AADD( ::PaperSize, { "A4 Rotated 297 x 210 mm",                       DMPAPER_A4_ROTATED                   } )
   AADD( ::PaperSize, { "A5 Rotated 210 x 148 mm",                       DMPAPER_A5_ROTATED                   } )
   AADD( ::PaperSize, { "B4 (JIS) Rotated 364 x 257 mm",                 DMPAPER_B4_JIS_ROTATED               } )
   AADD( ::PaperSize, { "B5 (JIS) Rotated 257 x 182 mm",                 DMPAPER_B5_JIS_ROTATED               } )
   AADD( ::PaperSize, { "Japanese Postcard Rotated 148 x 100 mm",        DMPAPER_JAPANESE_POSTCARD_ROTATED    } )
   AADD( ::PaperSize, { "Double Japanese Postcard Rotated 148 x 200 mm", DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED} )
   AADD( ::PaperSize, { "A6 Rotated 148 x 105 mm",                       DMPAPER_A6_ROTATED                   } )
   AADD( ::PaperSize, { "Japanese Envelope Kaku #2 Rotated",             DMPAPER_JENV_KAKU2_ROTATED           } )
   AADD( ::PaperSize, { "Japanese Envelope Kaku #3 Rotated",             DMPAPER_JENV_KAKU3_ROTATED           } )
   AADD( ::PaperSize, { "Japanese Envelope Chou #3 Rotated",             DMPAPER_JENV_CHOU3_ROTATED           } )
   AADD( ::PaperSize, { "Japanese Envelope Chou #4 Rotated",             DMPAPER_JENV_CHOU4_ROTATED           } )
   AADD( ::PaperSize, { "B6 (JIS) 128 x 182 mm",                         DMPAPER_B6_JIS                       } )
   AADD( ::PaperSize, { "B6 (JIS) Rotated 182 x 128 mm",                 DMPAPER_B6_JIS_ROTATED               } )
   AADD( ::PaperSize, { "12 x 11 in",                                    DMPAPER_12X11                        } )
   AADD( ::PaperSize, { "Japanese Envelope You #4",                      DMPAPER_JENV_YOU4                    } )
   AADD( ::PaperSize, { "Japanese Envelope You #4 Rotated",              DMPAPER_JENV_YOU4_ROTATED            } )
   AADD( ::PaperSize, { "PRC 16K 146 x 215 mm",                          DMPAPER_P16K                         } )
   AADD( ::PaperSize, { "PRC 32K 97 x 151 mm",                           DMPAPER_P32K                         } )
   AADD( ::PaperSize, { "PRC 32K(Big) 97 x 151 mm",                      DMPAPER_P32KBIG                      } )
   AADD( ::PaperSize, { "PRC Envelope #1 102 x 165 mm",                  DMPAPER_PENV_1                       } )
   AADD( ::PaperSize, { "PRC Envelope #2 102 x 176 mm",                  DMPAPER_PENV_2                       } )
   AADD( ::PaperSize, { "PRC Envelope #3 125 x 176 mm",                  DMPAPER_PENV_3                       } )
   AADD( ::PaperSize, { "PRC Envelope #4 110 x 208 mm",                  DMPAPER_PENV_4                       } )
   AADD( ::PaperSize, { "PRC Envelope #5 110 x 220 mm",                  DMPAPER_PENV_5                       } )
   AADD( ::PaperSize, { "PRC Envelope #6 120 x 230 mm",                  DMPAPER_PENV_6                       } )
   AADD( ::PaperSize, { "PRC Envelope #7 160 x 230 mm",                  DMPAPER_PENV_7                       } )
   AADD( ::PaperSize, { "PRC Envelope #8 120 x 309 mm",                  DMPAPER_PENV_8                       } )
   AADD( ::PaperSize, { "PRC Envelope #9 229 x 324 mm",                  DMPAPER_PENV_9                       } )
   AADD( ::PaperSize, { "PRC Envelope #10 324 x 458 mm",                 DMPAPER_PENV_10                      } )
   AADD( ::PaperSize, { "PRC 16K Rotated",                               DMPAPER_P16K_ROTATED                 } )
   AADD( ::PaperSize, { "PRC 32K Rotated",                               DMPAPER_P32K_ROTATED                 } )
   AADD( ::PaperSize, { "PRC 32K(Big) Rotated",                          DMPAPER_P32KBIG_ROTATED              } )
   AADD( ::PaperSize, { "PRC Envelope #1 Rotated 165 x 102 mm",          DMPAPER_PENV_1_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #2 Rotated 176 x 102 mm",          DMPAPER_PENV_2_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #3 Rotated 176 x 125 mm",          DMPAPER_PENV_3_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #4 Rotated 208 x 110 mm",          DMPAPER_PENV_4_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #5 Rotated 220 x 110 mm",          DMPAPER_PENV_5_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #6 Rotated 230 x 120 mm",          DMPAPER_PENV_6_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #7 Rotated 230 x 160 mm",          DMPAPER_PENV_7_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #8 Rotated 309 x 120 mm",          DMPAPER_PENV_8_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #9 Rotated 324 x 229 mm",          DMPAPER_PENV_9_ROTATED               } )
   AADD( ::PaperSize, { "PRC Envelope #10 Rotated 458 x 324 mm",         DMPAPER_PENV_10_ROTATED              } )

   ::Cursor := Hash()
   HSetCaseMatch( ::Cursor, .F. )
   HSetAACompatibility( ::Cursor, .T. )

   ::Cursor[ "Arrow" ]       := LoadCursor(, IDC_ARROW       )
   ::Cursor[ "Help" ]        := LoadCursor(, IDC_HELP        )
   ::Cursor[ "Working" ]     := LoadCursor(, IDC_APPSTARTING )
   ::Cursor[ "Busy" ]        := LoadCursor(, IDC_WAIT        )
   ::Cursor[ "Cross" ]       := LoadCursor(, IDC_CROSS       )
   ::Cursor[ "TextSelect" ]  := LoadCursor(, IDC_IBEAM       )
   ::Cursor[ "Unavailable" ] := LoadCursor(, IDC_NO          )
   ::Cursor[ "SizeNS" ]      := LoadCursor(, IDC_SIZENS      )
   ::Cursor[ "SizeWE" ]      := LoadCursor(, IDC_SIZEWE      )
   ::Cursor[ "SizeNESW" ]    := LoadCursor(, IDC_SIZENESW    )
   ::Cursor[ "SizeNWSE" ]    := LoadCursor(, IDC_SIZENWSE    )
   ::Cursor[ "SizeAll" ]     := LoadCursor(, IDC_SIZEALL     )
   ::Cursor[ "UpArrow" ]     := LoadCursor(, IDC_UPARROW     )
   ::Cursor[ "LinkSelect" ]  := LoadCursor(, IDC_HAND        )

   ::ImageList := Hash()
   HSetCaseMatch( ::ImageList, .F. )

   ::ImageList[ "Cursors" ] := ImageList()
   WITH OBJECT ::ImageList[ "Cursors" ]
      :Palette := ILC_COLOR8
      :xName      := "::System:ImageList:Cursors"
      :IconWidth  := 32
      :IconHeight := 32
      :Create()
      ImageListAddIcon( :Handle, ::Cursor[ "Arrow" ]       )
      ImageListAddIcon( :Handle, ::Cursor[ "Help" ]        )
      ImageListAddIcon( :Handle, ::Cursor[ "Working" ]     )
      ImageListAddIcon( :Handle, ::Cursor[ "Busy" ]        )
      ImageListAddIcon( :Handle, ::Cursor[ "Cross" ]       )
      ImageListAddIcon( :Handle, ::Cursor[ "TextSelect" ]  )
      ImageListAddIcon( :Handle, ::Cursor[ "Unavailable" ] )
      ImageListAddIcon( :Handle, ::Cursor[ "SizeNS" ]      )
      ImageListAddIcon( :Handle, ::Cursor[ "SizeWE" ]      )
      ImageListAddIcon( :Handle, ::Cursor[ "SizeNESW" ]    )
      ImageListAddIcon( :Handle, ::Cursor[ "SizeNWSE" ]    )
      ImageListAddIcon( :Handle, ::Cursor[ "SizeAll" ]     )
      ImageListAddIcon( :Handle, ::Cursor[ "UpArrow" ]     )
      ImageListAddIcon( :Handle, ::Cursor[ "LinkSelect" ]  )
   END

   SysGetImageList( @hLarge, @hSmall )
   ::ImageList[ "Small" ] := ImageList()
   ::ImageList[ "Small" ]:Handle := hSmall
   ::ImageList[ "Small" ]:xName  := "::System:ImageList:Small"

   ::ImageList[ "Large" ] := ImageList()
   ::ImageList[ "Large" ]:Handle      := hLarge
   ::ImageList[ "Large" ]:xIconWidth  := 32
   ::ImageList[ "Large" ]:xIconHeight := 32
   ::ImageList[ "Large" ]:xName       := "::System:ImageList:Large"

   ::ImageList[ "StdSmall" ] := ImageList( NIL, 16, 16 ):Create()
   ::ImageList[ "StdSmall" ]:AddImage( IDB_STD_SMALL_COLOR )
   ::ImageList[ "StdSmall" ]:xName := "::System:ImageList:StdSmall"

   ::ExplorerBar := (struct EXPBARINFO)
   cBuffer := ::ExplorerBar:Value()
   ExplorerBarInfo( @cBuffer )
   ::ExplorerBar:Buffer( cBuffer )
RETURN Self

METHOD GetRunningProcs() CLASS System
   LOCAL oLocator, oProcess, aProcessList, oWMIService, aProcess := {}
   TRY
      oLocator := GetActiveObject("WbemScripting.SWbemLocator") 
    CATCH
      oLocator := CreateObject("WbemScripting.SWbemLocator") 
   END
   TRY
      oWMIService  := oLocator:ConnectServer( , "root\CIMV2", , , "MS_409", ) 
      aProcessList := oWMIService:ExecQuery("SELECT * FROM Win32_Process")
      
      FOR EACH oProcess IN aProcessList
          AADD( aProcess, { oProcess:Name, oProcess:CommandLine, IIF( !EMPTY(oProcess:CreationDate), STOD( LEFT( oProcess:CreationDate, 8 ) ),"") } )
      NEXT
   CATCH
   END
RETURN aProcess

METHOD IsProcRunning( cProcName, lTerminate ) CLASS System
   LOCAL oLocator, oProcess, aProcessList, oWMIService
   DEFAULT lTerminate TO .F.
   TRY
      oLocator := GetActiveObject("WbemScripting.SWbemLocator") 
    CATCH
      oLocator := CreateObject("WbemScripting.SWbemLocator") 
   END
   oWMIService  := oLocator:ConnectServer( , "root\CIMV2", , , "MS_409", ) 
   aProcessList := oWMIService:ExecQuery("SELECT * FROM Win32_Process WHERE Name = '"+cProcName+"'")
      
   IF lTerminate
      FOR EACH oProcess IN aProcessList
          oProcess:Terminate()
      NEXT
   ENDIF
RETURN aProcessList:Count > 0

FUNCTION GC2RGB( p_nColor )
   LOCAL l_nRed, l_nGreen, l_nBlue 

   l_nRed   := MOD(p_nColor, 256) 
   l_nGreen := MOD(INT(p_nColor/256), 256) 
   l_nBlue  := MOD(INT(p_nColor/(256*256)), 256) 

RETURN ALLTRIM(STR(l_nRed))+","+ALLTRIM(STR(l_nGreen))+","+ALLTRIM(STR(l_nBlue)) 


METHOD Update() CLASS System
   LOCAL cBuffer
   
   FreeExplorerBarInfo()
   GetExplorerBarInfo()
   cBuffer := ::ExplorerBar:Value()
   ExplorerBarInfo( @cBuffer )
   ::ExplorerBar:Buffer( cBuffer )
RETURN Self

CLASS __SysTime
   ACCESS Year          INLINE ::__GetSysTime(1)
   ACCESS Month         INLINE ::__GetSysTime(2)
   ACCESS DayOfWeek     INLINE ::__GetSysTime(3)
   ACCESS Day           INLINE ::__GetSysTime(4)
   ACCESS Hour          INLINE ::__GetSysTime(5)
   ACCESS Minute        INLINE ::__GetSysTime(6)
   ACCESS Second        INLINE ::__GetSysTime(7)
   ACCESS Milliseconds  INLINE ::__GetSysTime(8)

   ASSIGN Year(x)          INLINE ::__SetSysTime( 1, x )
   ASSIGN Month(x)         INLINE ::__SetSysTime( 2, x )
   ASSIGN DayOfWeek(x)     INLINE ::__SetSysTime( 3, x )
   ASSIGN Day(x)           INLINE ::__SetSysTime( 4, x )
   ASSIGN Hour(x)          INLINE ::__SetSysTime( 5, x )
   ASSIGN Minute(x)        INLINE ::__SetSysTime( 6, x )
   ASSIGN Second(x)        INLINE ::__SetSysTime( 7, x )
   ASSIGN Milliseconds(x)  INLINE ::__SetSysTime( 8, x )
   METHOD __GetSysTime()
   METHOD __SetSysTime()
ENDCLASS

METHOD __GetSysTime(n) CLASS __SysTime
   LOCAL st
   GetSystemTime( @st )
RETURN st:Array[n]

METHOD __SetSysTime(n,x) CLASS __SysTime
   LOCAL st, st1
   GetSystemTime( @st1 )
   SWITCH n
      CASE 1
           st1:wYear := x
           EXIT
      CASE 2
           st1:wMonth := x
           EXIT
      CASE 3
           st1:wDayOfWeek := x
           EXIT
      CASE 4
           st1:wDay := x
           EXIT
      CASE 5
           st1:wHour := x
           EXIT
      CASE 6
           st1:wMinute := x
           EXIT
      CASE 7
           st1:wSecond := x
           EXIT
      CASE 8
           st1:wMilliseconds := x
           EXIT
   END
   st := (struct SYSTEMTIME)

   st:wYear         := st1:wYear
   st:wMonth        := st1:wMonth
   st:wDayOfWeek    := st1:wDayOfWeek
   st:wDay          := st1:wDay
   st:wHour         := st1:wHour
   st:wMinute       := st1:wMinute
   st:wSecond       := st1:wSecond
   st:wMilliseconds := st1:wMilliseconds

   SetSystemTime( st )
RETURN st:Array[n]
