/*
 * Harbour Project source code:
 * Header file for the WVT*Classes
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
//-------------------------------------------------------------------//
//
//                         GTWVT Specific
//
//-------------------------------------------------------------------//
//
//   Wvt_DrawButton() constants
//
#define WVT_BTN_FORMAT_RAISED         0   // Default
#define WVT_BTN_FORMAT_RECESSED       1
#define WVT_BTN_FORMAT_FLAT           2
#define WVT_BTN_FORMAT_OUTLINED       3

#define WVT_BTN_IMAGE_TOP             0   // Default
#define WVT_BTN_IMAGE_LEFT            1
#define WVT_BTN_IMAGE_BOTTOM          2
#define WVT_BTN_IMAGE_RIGHT           3

//-------------------------------------------------------------------//
//
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat,;
//                 nAlign, nStyle, nThick, nColor )
//
//   nOrient
#define WVT_LINE_HORZ                 0   // Default
#define WVT_LINE_VERT                 1

//   nFormat
#define WVT_LINE_RAISED               0   // Default
#define WVT_LINE_RECESSED             1
#define WVT_LINE_PLAIN                2

//   nAlign
#define WVT_LINE_CENTER               0   // Default
#define WVT_LINE_TOP                  1
#define WVT_LINE_BOTTOM               2
#define WVT_LINE_LEFT                 3
#define WVT_LINE_RIGHT                4

//   nStyle
#define WVT_LINE_SOLID                0   // Default
#define WVT_LINE_DASH                 1
#define WVT_LINE_DOT                  2
#define WVT_LINE_DASHDOT              3
#define WVT_LINE_DASHDOTDOT           4

//-------------------------------------------------------------------//
//
//              Standard Mouse Pointer Shape Constants
//
#define WVT_IDC_ARROW                 1
#define WVT_IDC_IBEAM                 2
#define WVT_IDC_WAIT                  3
#define WVT_IDC_CROSS                 4
#define WVT_IDC_UPARROW               5
#define WVT_IDC_SIZE                  6
#define WVT_IDC_ICON                  7
#define WVT_IDC_SIZENWSE              8
#define WVT_IDC_SIZENESW              9
#define WVT_IDC_SIZEWE               10
#define WVT_IDC_SIZENS               11
#define WVT_IDC_SIZEALL              12
#define WVT_IDC_NO                   13
#define WVT_IDC_HAND                 14
#define WVT_IDC_APPSTARTING          15
#define WVT_IDC_HELP                 16

//-------------------------------------------------------------------//

#define WVT_BLOCK_IMAGE               1
#define WVT_BLOCK_BOX                 2
#define WVT_BLOCK_LABEL               3
#define WVT_BLOCK_GRID_V              4
#define WVT_BLOCK_GRID_H              5
#define WVT_BLOCK_BUTTON              6
#define WVT_BLOCK_GETS                7
#define WVT_BLOCK_LINE                8
#define WVT_BLOCK_STATUSBAR           9
#define WVT_BLOCK_TOOLBAR            10
#define WVT_BLOCK_STATIC             11

//-------------------------------------------------------------------//

#define DLG_OBJ_BROWSE                1
#define DLG_OBJ_PICTURE               2
#define DLG_OBJ_LINE                  3
#define DLG_OBJ_RECT                  4
#define DLG_OBJ_GETS                  5
#define DLG_OBJ_BUTTON                6
#define DLG_OBJ_STATUSBAR             7
#define DLG_OBJ_PANEL                 8
#define DLG_OBJ_LABEL                 9
#define DLG_OBJ_STATIC               10
#define DLG_OBJ_TOOLBAR              11
#define DLG_OBJ_IMAGE                12
#define DLG_OBJ_PUSHBUTTON           13
#define DLG_OBJ_CONSOLE              14
#define DLG_OBJ_SCROLLBAR            15
#define DLG_OBJ_BANNER               16
#define DLG_OBJ_TEXTBOX              17

//-------------------------------------------------------------------//

#define TLB_BUTTON_TYPE_IMAGE         0
#define TLB_BUTTON_TYPE_SEPARATOR     1
#define TLB_BUTTON_TYPE_TEXT          2

//-------------------------------------------------------------------//

#define WVT_STATIC_LINE               1
#define WVT_STATIC_BOXRAISED          2
#define WVT_STATIC_BOXRECESSED        3
#define WVT_STATIC_BOXGROUP           4
#define WVT_STATIC_BOXGROUPRAISED     5
#define WVT_STATIC_RECTANGLE          6
#define WVT_STATIC_ROUNDRECT          7
#define WVT_STATIC_FOCUSRECT          8
#define WVT_STATIC_OUTLINE            9
#define WVT_STATIC_ELLIPSE           10
#define WVT_STATIC_SHADEDRECT        11

#define WVT_SCROLLBAR_VERT            1
#define WVT_SCROLLBAR_HORZ            2

#define WVT_SCROLLBUTTON_TOP          1
#define WVT_SCROLLBUTTON_LEFT         2
#define WVT_SCROLLBUTTON_BOTTOM       3
#define WVT_SCROLLBUTTON_RIGHT        4
#define WVT_SCROLL_THUMB              5

//-------------------------------------------------------------------//
//
// wvtmenu defines  .  Peter Rees
//
#define WVT_MENU_TYPE                 1
#define WVT_MENU_IDENTIFIER           2
#define WVT_MENU_CAPTION              3
#define WVT_MENU_ACTION               4
#define WVT_MENU_MENUOBJ              4

//-------------------------------------------------------------------//

#define ISBLOCK( x )      valtype( x ) == 'B'

#define RGB( nR,nG,nB )   ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

//-------------------------------------------------------------------//
//
//                          Windows Specific
//
//-------------------------------------------------------------------//
//
//                   Menu Manipulation Constants
//
#define MF_INSERT                     0
#define MF_CHANGE                   128
#define MF_APPEND                   256
#define MF_DELETE                   512
#define MF_REMOVE                  4096

#define MF_BYCOMMAND                  0
#define MF_BYPOSITION              1024

#define MF_SEPARATOR               2048

#define MF_ENABLED                    0
#define MF_GRAYED                     1
#define MF_DISABLED                   2

#define MF_UNCHECKED                  0
#define MF_CHECKED                    8
#define MF_USECHECKBITMAPS          512

#define MF_STRING                     0
#define MF_BITMAP                     4
#define MF_OWNERDRAW                256

#define MF_POPUP                     16
#define MF_MENUBARBREAK              32
#define MF_MENUBREAK                 64

#define MF_UNHILITE                   0
#define MF_HILITE                   128

//-------------------------------------------------------------------//

#define TPM_LEFTBUTTON           0x0000
#define TPM_RIGHTBUTTON          0x0002

#define TPM_LEFTALIGN            0x0000
#define TPM_CENTERALIGN          0x0004
#define TPM_RIGHTALIGN           0x0008

#define TPM_TOPALIGN             0x0000
#define TPM_VCENTERALIGN         0x0010
#define TPM_BOTTOMALIGN          0x0020

#define TPM_HORIZONTAL           0x0000     /* Horz alignment matters more */
#define TPM_VERTICAL             0x0040     /* Vert alignment matters more */
#define TPM_NONOTIFY             0x0080     /* Don't send any notification msgs */
#define TPM_RETURNCMD            0x0100

//-------------------------------------------------------------------//

