/*
 * $Id: gtinfo.ch,v 1.3 2004/02/16 16:11:19 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the GTINFO API
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
 * www - http://www.harbour-project.org
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

/* 
   Minimal informations:
   
   READ doc/gtinfo.txt
*/
    
#ifndef HB_GTINFO_CH_
#define HB_GTINFO_CH_

#define GTI_ISGRAPHIC      0  /* 1 if GT has graphic support / pixel oriented */
#define GTI_SCREENWIDTH    1  /* Get/set width of application window in pixels */
#define GTI_SCREENHEIGHT   2  /* Get/set height of appl. window in pixels */
#define GTI_SCREENDEPTH    3  /* Amount of bits used for colors in the application */
#define GTI_FONTSIZE       4  /* Get/set height of application font in pixels */
#define GTI_FONTWIDTH      5  /* Get/set width of application font characters */
#define GTI_DESKTOPWIDTH   6  /* Get width of desktop in pixels */
#define GTI_DESKTOPHEIGHT  7  /* Get height of desktop in pixels */
#define GTI_DESKTOPDEPTH   8  /* Amount of bits used for colors in system */
#define GTI_DESKTOPROWS   20  /* Get Size of desktop in character rows */
#define GTI_DESKTOPCOLS   21  /* Get Size of desktop in character cols */
#define GTI_FONTWEIGHT    22  /* Get/set the weight of the font used in application */
#define GTI_FONTQUALITY   23  /* Get/set quality of font rendering in the appl. */

#define GTI_INPUTFD       30  /* Get Standard input steream of application/GT */
#define GTI_OUTPUTFD      31  /* Get Standard output steream of application/GT */
#define GTI_ERRORFD       32  /* Get Standard error steream of application/GT */

#define GTI_ESCDELAY      33  /* Get/Set escape key delay */

/* Font weights */
#define GTI_FONTW_THIN     1
#define GTI_FONTW_NORMAL   2
#define GTI_FONTW_BOLD     3

/* Font sizes */
#define GTI_FONTQ_DRAFT    1
#define GTI_FONTQ_NORMAL   2
#define GTI_FONTQ_HIGH     3

#endif /* HB_GTINFO_CH_ */
