/*
 * $Id: terror.prg,v 1.8 2003/05/25 17:03:18 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * Error Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* Error Class. We are keeping Clipper compatibility here, instead of using
   TError():New() style and also avoiding hungarian notation. */

#include "error.ch"

#ifndef HB_THREAD_SUPPORT
   static s_aErrHandlers := {}
#endif

FUNCTION ErrorNew( SubSystem, SubCode, Operation, Description, Args, ModuleName )

   STATIC s_oClass
   LOCAL oErr

   IF s_oClass == NIL
      s_oClass := HBClass():New( "ERROR" )

      s_oClass:AddData( "Args"         ,  )
      s_oClass:AddData( "CanDefault"   , .F. )
      s_oClass:AddData( "CanRetry"     , .F. )
      s_oClass:AddData( "CanSubstitute", .F. )
      s_oClass:AddData( "Cargo" )
      s_oClass:AddData( "Description"  , "" )
      s_oClass:AddData( "FileName"     , "" )
      s_oClass:AddData( "GenCode"      , 0 )
      s_oClass:AddData( "Operation"    , "" )
      s_oClass:AddData( "OsCode"       , 0 )
      s_oClass:AddData( "Severity"     , ES_ERROR )
      s_oClass:AddData( "SubCode"      , 0 )
      s_oClass:AddData( "SubSystem"    , "" )
      s_oClass:AddData( "Tries"        , 0 )

      s_oClass:AddData( "ProcName"     , Procname(1) )
      s_oClass:AddData( "ProcLine"     , Procline(1) )

      #ifdef HB_THREAD_SUPPORT
         s_oClass:AddData( "RunningThreads" , HB_ThreadCountStacks() )
         s_oClass:AddData( "OsThreadId"     , ThreadGetCurrent() )
         s_oClass:AddData( "VMThreadId"     , ThreadGetCurrentInternal() )
      #endif

      s_oClass:AddData( "ModuleName"   , "" )

      s_oClass:Create()
   ENDIF

   oErr := s_oClass:Instance()

   IF SubSystem != NIL
      oErr:SubSystem := SubSystem
   ENDIF
   IF SubCode != NIL
      oErr:SubCode := SubCode
   ENDIF
   IF Operation != NIL
      oErr:Operation := Operation
   ENDIF
   IF Description != NIL
      oErr:Description := Description
   ENDIF
   IF Args != NIL
      oErr:Args := Args
   ENDIF
   IF ModuleName != NIL
      oErr:ModuleName := ModuleName
   ENDIF

RETURN oErr

#ifndef HB_THREAD_SUPPORT

PROCEDURE HB_SetTry()

   aAdd( s_aErrHandlers, ErrorBlock( {|e| Break(e) } ) )

RETURN

PROCEDURE HB_ResetTry()

   ErrorBlock( s_aErrHandlers[-1] )
   aSize( s_aErrHandlers, Len( s_aErrHandlers ) - 1 )

RETURN

#else

PROCEDURE HB_SetTry()

   aAdd( HB_threadGetTryErrorArray(), ErrorBlock( {|e| Break(e) } ) )

RETURN

PROCEDURE HB_ResetTry()
   LOCAL aTryErrHandlers := HB_threadGetTryErrorArray()

   ErrorBlock( aTryErrHandlers[-1] )
   aSize( aTryErrHandlers, Len( aTryErrHandlers ) - 1 )

RETURN

#endif
