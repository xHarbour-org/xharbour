/*
 * $Id: hbcc.ch,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Header File For Unicode Support
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
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
//charset definitions
   //Invalid charset
#define HB_CSINVALID 0
   //User charset upper limit
#define HB_CSPRESET 4096
   //Preset: UCS2 low Endian
#define HB_CSUNICODE 4097
#define HB_CSUCS2 4097
#define HB_CSUCS2LE 4097
   //Preset: UCS2 big Endian
#define HB_CSUCS2BE 4098
   //Preset: UTF-8
#define HB_CSUTF8 4099
   //Preset: UTF-7
#define HB_CSUTF7 4100

//Error values
   //No error
#define HB_CSERR_OK 0
   //Bad (wrong type) argument used
#define HB_CSERR_BADARG 1
   //Bad (inactive or unavailable) charset
#define HB_CSERR_BADCS 2
   //Bad (impossible for chosen source charset) character value
   //(not implemented yet)
#define HB_CSERR_BADCHAR 3
   //Charsets number limit (4096) exceeded
#define HB_CSERR_LIMIT 4
   //Unspecified error
#define HB_CSERR_ERROR 99
