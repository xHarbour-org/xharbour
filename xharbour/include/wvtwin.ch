/*
 * Harbour Project source code:
 * Header file for the WVT commands
 *
 * Copyright 2004 Francesco Saverio Giudice <info@fsgiudice.com>
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

