/*
 * $Id: hbgtwvt.h,v 1.1 2003/12/22 01:21:36 peterrees Exp $
 */

/*
 * xHarbour Project source code:
 * Video subsystem grwvt exported functions header file
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

extern void HB_EXPORT hb_wvt_gtSetCloseEvent(int iEvent);
extern void HB_EXPORT hb_wvt_gtSetShutdownEvent(int iEvent);
extern void HB_EXPORT hb_wvt_gtSetFont(char * fontface, int height, int width, int Bold, int Quality );
extern void HB_EXPORT hb_wvt_gtSetWindowTitle(char * title);
extern int HB_EXPORT hb_wvt_gtGetWindowTitle(char *title, int length);
extern DWORD HB_EXPORT hb_wvt_gtSetWindowIcon(int icon);
extern HWND HB_EXPORT hb_wvt_gtGetWindowHandle(void);
extern void HB_EXPORT hb_wvt_gtPostMessage(int message);
extern BOOL HB_EXPORT hb_wvt_gtSetWindowPos(int left, int top);
extern int HB_EXPORT hb_wvt_gtGetLastMenuEvent(void);
extern void HB_EXPORT hb_wvt_gtResetWindow(void);
extern BOOL HB_EXPORT hb_wvt_gtSetCentreWindow(BOOL bCentre, BOOL bPaint);
extern int HB_EXPORT hb_wvt_gtSetMenuKeyEvent(int iMenuKeyEvent);  // Set Key number to return when for windows menu selection
extern int HB_EXPORT hb_wvt_gtSetCodePage(int iCodePage);
extern BOOL HB_EXPORT hb_wvt_gtSetAltF4Close( BOOL bCanClose);
extern void HB_EXPORT hb_wvt_gtDoProcessMessages(void);
extern BOOL HB_EXPORT hb_wvt_gtSetMouseMove( BOOL bHandleEvent);
