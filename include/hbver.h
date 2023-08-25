/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for version information
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#ifndef HB_VER_H_
#define HB_VER_H_

#define HB_VER_LEX "SimpLex"
#define HB_VER_MAJOR    1       /* Major version number */
#define HB_VER_MINOR    3       /* Minor version number */
#define HB_VER_REVISION 0       /* Revision number */

#if defined( __XHARBOUR__ )
   #undef __XHARBOUR__
#endif
#define __XHARBOUR__    0x0130

#if defined( __HARBOUR__ )
   #undef __HARBOUR__
#endif
#define __HARBOUR__     0x0130

/* TOFIX: Ideally these should be generated dynamically, until
          then, they should be updated by the builder. [vszakats] */
/* Automatic update implemented upon clean build. [AJ:2008-05-05] */

/* The following constants are no longer significant as they are now overridden
   in hbverbld.h [AJ:2008-05-05] */
#define HB_VER_LENTRY   "2009-03-26 10:41 UTC-0430 Ron Pinkas <ron.pinkas/at/xharbour.com>"
#define HB_VER_CHLCVS   "ChangeLog,v 1.6406"

#define HB_VER_C_USR    ""
#define HB_VER_L_USR    ""
#define HB_VER_PRG_USR  ""

#include "hbver.ch"
/* #include "hb_ver_.h" */

#endif /* HB_VER_H_ */
