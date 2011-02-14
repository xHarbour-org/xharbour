/*
 * $Id$
 */

/*
 * xharbour Project source code:
 * The session API
 *
 * Copyright 2009 Miguel Angel Marchuet Frutos <miguelangel@marchuet.net>
 * www - http://www.xharbour.org
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
 
#ifndef HB_APISESSION_H_
#define HB_APISESSION_H_

#include "hbapi.h"
#include "hbinit.h"

HB_EXTERN_BEGIN

/* This hack is needed to force preprocessing if id is also a macro */
#define HB_SESSION_REQUEST()      HB_SESSION_REQUEST_()
#define HB_SESSION_REQUEST_()     HB_FUNC_EXTERN( HB_SESSION ); \
                                  void hb_session_ForceLink( void ) \
                                  { \
                                    HB_FUNC_EXEC( HB_SESSION ); \
                                  }
#define HB_SESSION_ANNOUNCE()     HB_FUNC( HB_SESSION ) {}

#define HB_SESSION_INIT()         HB_SESSION_ANNOUNCE() \
                                  HB_CALL_ON_STARTUP_BEGIN( hb_session_Init ) \
                                  hb_sessionRegister( &s_session ); \
                                  HB_CALL_ON_STARTUP_END( hb_session_Init )

typedef struct _HB_SESSION
{
   int            id;
   const char *   username;
   HB_FHANDLE     hTransFile;      /* transaction  file handle */
} HB_SESSION, * PHB_SESSION;

extern HB_EXPORT BOOL         hb_sessionRegister( PHB_SESSION );
extern HB_EXPORT int          hb_sessionSelectID( const int );
extern HB_EXPORT int          hb_sessionID( void );
extern HB_EXPORT PHB_SESSION  hb_sessionSelect( PHB_SESSION );
extern HB_EXPORT PHB_SESSION  hb_sessionFind( const int );
extern HB_EXPORT void         hb_sessionReleaseAll( void );
extern HB_EXPORT PHB_SESSION  hb_session( void );

HB_EXTERN_END

#endif
