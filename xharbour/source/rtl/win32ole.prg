/*
 * $Id: win32ole.prg,v 1.27 2003/06/21 03:17:57 ronpinkas Exp $
 */

/*
 * Copyright 2002  José F. Giménez (JFG) - <jfgimenez@wanadoo.es>
 *                 Ron Pinkas            - <ron@ronpinkas.com>
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

#ifndef __PLATFORM__Windows
  Function CreateObject()
  Return NIL

  FUNCTION GetActiveObject( cString )
  Return NIL
#else

Static bOleInitialized := .F.

#include "hbclass.ch"
#include "error.ch"

#define EG_OLEEXECPTION 1001

//----------------------------------------------------------------------------//

FUNCTION CreateObject( cString )

RETURN TOleAuto():New( cString )

//----------------------------------------------------------------------------//

FUNCTION GetActiveObject( cString )

RETURN TOleAuto():GetActiveObject( cString )

//----------------------------------------------------------------------------//

#pragma BEGINDUMP

   #ifndef __STDC__
      #define __STDC__ 1
   #endif

   #ifndef CINTERFACE
      #define CINTERFACE 1
   #endif

   #ifndef __FLAT__
     #define __FLAT__ 1
   #endif

   #define NONAMELESSUNION

   #include "hbapi.h"
   #include "hbstack.h"
   #include "hbapierr.h"
   #include "hbapiitm.h"
   #include "hbvm.h"
   #include "hbdate.h"
   #include "hboo.ch"
   #include "hbfast.h"

   #include <ctype.h>

   #include <Windows.h>
   #include <Ole2.h>
   #include <OleAuto.h>

   #ifndef __MINGW32__
      // Missing in Mingw V 2.
      //#include <OleDB.h>
   #endif

   #include <ShlObj.h>

   #ifdef __MINGW32__
      // Missing in oleauto.h
      WINOLEAUTAPI VarR8FromDec(DECIMAL *pdecIn, DOUBLE *pdblOut);
   #endif

   static HRESULT s_nOleError = 0;
   static HB_ITEM  OleAuto;

   static PHB_DYNS s_pSym_OleAuto = NULL;
   static PHB_DYNS s_pSym_hObj    = NULL;
   static PHB_DYNS s_pSym_New     = NULL;

   static char *s_OleRefFlags = NULL;

   HB_FUNC_STATIC( OLE_INITIALIZE )
   {
      s_nOleError = OleInitialize( NULL );

      s_pSym_OleAuto = hb_dynsymFindName( "TOLEAUTO" );
      s_pSym_New  = hb_dynsymFindName( "NEW" );
   }

   HB_FUNC_STATIC( OLE_UNINITIALIZE )
   {
      OleUninitialize();
   }

   HB_FUNC( SETOLEREFFLAGS )
   {
      if( s_OleRefFlags )
      {
         hb_xfree( s_OleRefFlags );
         s_OleRefFlags = NULL;
      }

      if( hb_pcount() )
      {
         s_OleRefFlags = hb_strdup( hb_stackItemFromBase( 1 )->item.asString.value );
      }
   }

#pragma ENDDUMP


//----------------------------------------------------------------------------//

INIT PROCEDURE Initialize_Ole
   IF ! bOleInitialized
      bOleInitialized := .T.
      Ole_Initialize()
   ENDIF
RETURN

EXIT PROCEDURE UnInitialize_Ole
   IF bOleInitialized
      bOleInitialized := .F.
      Ole_UnInitialize()
   ENDIF
RETURN

CLASS TOleAuto

   DATA hObj
   DATA cClassName
   DATA bShowException INIT .T.

   METHOD New( uObj, cClass ) CONSTRUCTOR
   METHOD GetActiveObject( cClass ) CONSTRUCTOR

   METHOD Invoke( cMember, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )

   ERROR HANDLER OnError()

ENDCLASS

//--------------------------------------------------------------------

METHOD New( uObj, cClass ) CLASS TOleAuto

   IF ValType( uObj ) = 'C'
      ::hObj := CreateOleObject( uObj )
      ::cClassName := uObj
   ELSEIF ValType( uObj ) = 'N'
      ::hObj := uObj
      IF ValType( cClass ) == 'C'
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
   ELSE
      MessageBox( 0, "Invalid parameter type to constructor TOleAuto():New()!", "OLE Interface", 0 )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------

METHOD GetActiveObject( cClass ) CLASS TOleAuto

   IF ValType( cClass ) = 'C'
      ::hObj := GetOleObject( cClass )
      ::cClassName := cClass
   ELSE
      MessageBox( 0, "Invalid parameter type to constructor TOleAuto():GetActiveObject()!", "OLE Interface", 0 )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------

METHOD Invoke( cMethod, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6, uParam7, uParam8, uParam9 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount(), Counter
   LOCAL OleRefFlags := Space( nParams - 1 )
   LOCAL oErr

   //TraceLog( cMethod, nParams )

   IF ProcName( 1 ) != "TOLEAUTO:" + cMethod

      IF nParams >= 10
         IF HB_ISBYREF( @uParam9 )
            OleRefFlags[9] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 9
         IF HB_ISBYREF( @uParam8 )
            OleRefFlags[8] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 8
         IF HB_ISBYREF( @uParam7 )
            OleRefFlags[7] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 7
         IF HB_ISBYREF( @uParam6 )
            OleRefFlags[6] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 6
         IF HB_ISBYREF( @uParam5 )
            OleRefFlags[5] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 5
         IF HB_ISBYREF( @uParam4 )
            OleRefFlags[4] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 4
         IF HB_ISBYREF( @uParam3 )
            OleRefFlags[3] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 3
         IF HB_ISBYREF( @uParam2 )
            OleRefFlags[2] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 2
         IF HB_ISBYREF( @uParam1 )
            OleRefFlags[1] = 'Y'
         ENDIF

         SetOleRefFlags( OleRefFlags )
      ENDIF
   ENDIF

   IF nParams == 10
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 )
   ELSEIF nParams == 9
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8 )
   ELSEIF nParams == 8
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7 )
   ELSEIF nParams == 7
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1 )
   ELSEIF nParams == 1
      uObj := OLEInvoke( ::hObj, cMethod )
   ELSE
      MessageBox( 0, "TOleAuto:Invoke() - Unsupported number of parameter!", "OLE Interface", 0 )
      RETURN NIL
   ENDIF

   SetOleRefFlags()

   IF ::bShowException
      IF Ole2TxtError() == "DISP_E_EXCEPTION"
         oErr := ErrorNew()
         oErr:Args          := { Self, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9  }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := OLEExceptionDescription()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cMethod
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := OLEExceptionSource()

         RETURN Eval( ErrorBlock(), oErr )

      ELSEIF OleError() != 0
         oErr := ErrorNew()
         oErr:Args          := { Self, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9  }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := Ole2TxtError()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cMethod
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := ::cClassName

         RETURN Eval( ErrorBlock(), oErr )
      ENDIF

      IF ValType( uObj ) == 'O'
         uObj:cClassName := Self:cClassName + ':' + cMethod
         //TraceLog( HB_ArrayId( uObj ), "Returning: " + uObj:cClassName )
      ENDIF
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6, uParam7, uParam8, uParam9 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount()
   LOCAL oErr

   //TraceLog( cProperty, nParams )

   IF nParams == 10
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 )
   ELSEIF nParams == 9
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8 )
   ELSEIF nParams == 8
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7 )
   ELSEIF nParams == 7
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      OLESetProperty( ::hObj, cProperty, @uParam1 )
   ELSE
      MessageBox( 0, "TOleAuto:Set() - Unsupported number of parameter!", "OLE Interface", 0 )
      RETURN NIL
   ENDIF

   SetOleRefFlags()

   IF ::bShowException .AND. Ole2TxtError() == "DISP_E_EXCEPTION"
      oErr := ErrorNew()
      oErr:Args          := { Self, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := OLEExceptionDescription()
      oErr:GenCode       := EG_OLEEXECPTION
      oErr:Operation     := ::cClassName + ":" + cProperty
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := OLEExceptionSource()

      RETURN Eval( ErrorBlock(), oErr )
   ELSEIF ::bShowException .AND. OleError() != 0
      oErr := ErrorNew()
      oErr:Args          := { Self, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := Ole2TxtError()
      oErr:GenCode       := EG_OLEEXECPTION
      oErr:Operation     := ::cClassName + ":" + cProperty
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := ::cClassName

      RETURN Eval( ErrorBlock(), oErr )
   ENDIF

RETURN nil

//--------------------------------------------------------------------

METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount()
   LOCAL oErr

   //TraceLog( cProperty, nParams )

   IF nParams == 7
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1 )
   ELSEIF nParams == 1
      uObj := OLEGetProperty( ::hObj, cProperty )
   ELSE
      MessageBox( 0, "TOleAuto:Get() - Unsupported number of parameter!", "OLE Interface", 0 )
      RETURN NIL
   ENDIF

   IF ::bShowException
      IF Ole2TxtError() == "DISP_E_EXCEPTION"
         oErr := ErrorNew()
         oErr:Args          := { Self, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := OLEExceptionDescription()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cProperty
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := OLEExceptionSource()

         RETURN Eval( ErrorBlock(), oErr )
      ELSEIF OleError() != 0
         oErr := ErrorNew()
         oErr:Args          := { Self, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := Ole2TxtError()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cProperty
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := ::cClassName

         RETURN Eval( ErrorBlock(), oErr )
      ENDIF

      IF ValType( uObj ) == 'O'
         uObj:cClassName := Self:cClassName + ':' + cProperty
      ENDIF
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD OnError( uParam1, uParam2, uParam3, uParam4, uParam5, uParam6, uParam7, uParam8, uParam9 ) CLASS TOleAuto

   LOCAL cMsg := __GetMessage()
   LOCAL bPresetShowException := ::bShowException
   LOCAL uObj
   LOCAL cError
   LOCAL nParams := PCount(), OleRefFlags := Space( nParams )
   LOCAL oErr

   IF nParams >= 9
      IF HB_ISBYREF( @uParam9 )
         OleRefFlags[9] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 8
      IF HB_ISBYREF( @uParam8 )
         OleRefFlags[8] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 7
      IF HB_ISBYREF( @uParam7 )
         OleRefFlags[7] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 6
      IF HB_ISBYREF( @uParam6 )
         OleRefFlags[6] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 5
      IF HB_ISBYREF( @uParam5 )
         OleRefFlags[5] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 4
      IF HB_ISBYREF( @uParam4 )
         OleRefFlags[4] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 3
      IF HB_ISBYREF( @uParam3 )
         OleRefFlags[3] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 2
      IF HB_ISBYREF( @uParam2 )
         OleRefFlags[2] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 1
      IF HB_ISBYREF( @uParam1 )
         OleRefFlags[1] = 'Y'
      ENDIF

      SetOleRefFlags( OleRefFlags )
   ENDIF

   ::bShowException := .F.

   IF LEFT( cMsg, 1 ) == '_'
      cMsg := SubStr( cMsg, 2 )

      IF nParams == 9
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 )
      ELSEIF nParams == 8
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8 )
      ELSEIF nParams == 7
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7 )
      ELSEIF nParams == 6
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
      ELSEIF nParams == 5
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
      ELSEIF nParams == 4
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
      ELSEIF nParams == 3
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3 )
      ELSEIF nParams == 2
         uObj := ::Set( cMsg, @uParam1, @uParam2 )
      ELSEIF nParams == 1
         uObj := ::Set( cMsg, @uParam1 )
      ELSE
         uObj := ::Set( cMsg )
      ENDIF

      // Reset in ::Set()
      //SetOleRefFlags()
   ELSE
      IF nParams == 9
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 )
      ELSEIF nParams == 8
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8 )
      ELSEIF nParams == 7
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7 )
      ELSEIF nParams == 6
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
      ELSEIF nParams == 5
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
      ELSEIF nParams == 4
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
      ELSEIF nParams == 3
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3 )
      ELSEIF nParams == 2
         uObj := ::Invoke( cMsg, @uParam1, @uParam2 )
      ELSEIF nParams == 1
         uObj := ::Invoke( cMsg, @uParam1 )
      ELSE
         //TraceLog( ::cClassName )
         uObj := ::Invoke( cMsg )
         //TraceLog( ::cClassName )
      ENDIF

      // Reset in ::Invoke()
      //SetOleRefFlags()

      IF Ole2TxtError() != "S_OK"
         //TraceLog( cMsg )

         IF nParams == 9
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 )
         ELSEIF nParams == 8
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8 )
         ELSEIF nParams == 7
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7 )
         ELSEIF nParams == 6
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
         ELSEIF nParams == 5
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
         ELSEIF nParams == 4
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
         ELSEIF nParams == 3
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3 )
         ELSEIF nParams == 2
            uObj := ::Get( cMsg, @uParam1, @uParam2 )
         ELSEIF nParams == 1
            uObj := ::Get( cMsg, @uParam1 )
         ELSE
            uObj := ::Get( cMsg )
         ENDIF

        // Reset in ::Get()
        //SetOleRefFlags()
      ENDIF
   ENDIF

   ::bShowException := bPresetShowException

   IF ::bShowException
      IF Ole2TxtError() == "DISP_E_EXCEPTION"
         oErr := ErrorNew()
         oErr:Args          := { Self, cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := OLEExceptionDescription()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cMsg
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := OLEExceptionSource()

         RETURN Eval( ErrorBlock(), oErr )
      ELSEIF ( cError := Ole2TxtError() ) != "S_OK"
         IF cMsg == "END"
            RETURN OleRelease( ::hObj )
         ENDIF

         oErr := ErrorNew()
         oErr:Args          := { Self, cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6, @uParam7, @uParam8, @uParam9 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := cError
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ::cClassName + ":" + cMsg
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := ::cClassName

         RETURN Eval( ErrorBlock(), oErr )
      ENDIF

      IF ValType( uObj ) == 'O'
         uObj:cClassName := Self:cClassName + ':' + cMsg
         //TraceLog( HB_ArrayId( uObj ), "*Returning: " + uObj:cClassName )
      ENDIF
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

#pragma BEGINDUMP

  // -----------------------------------------------------------------------

  static VARIANTARG RetVal;

  static EXCEPINFO excep;

  static PHB_ITEM *aPrgParams = NULL;

  //---------------------------------------------------------------------------//

  static double DateToDbl( LPSTR cDate )
  {
     double nDate;

     nDate = hb_dateEncStr( cDate ) - 0x0024d9abL;

     return ( nDate );
  }

  //---------------------------------------------------------------------------//

  static LPSTR DblToDate( double nDate )
  {
     static char *cDate = "00000000";

     hb_dateDecStr( cDate, (long) nDate + 0x0024d9abL );

     return ( cDate );
  }

  //---------------------------------------------------------------------------//

  static BSTR AnsiToWide( LPSTR cString )
  {
     UINT uLen;
     BSTR wString;

     uLen  = strlen( cString ) + 1;

     if( uLen > 1 )
     {
        wString = ( BSTR ) hb_xgrab( uLen * 2 );
        MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, uLen, wString, uLen );
     }
     else
     {
        // *** This is a speculation about L"" - need to be verified.
        wString = (BSTR) hb_xgrab( 2 );
        wString[0] = L'\0';
     }

     //printf( "\nAnsi: '%s'\n", cString );
     //wprintf( L"\nWide: '%s'\n", wString );

     return wString;
  }

  //---------------------------------------------------------------------------//

  static LPSTR WideToAnsi( BSTR wString )
  {
     UINT uLen;
     char *cString;

     uLen = SysStringLen( wString ) + 1;

     if( uLen > 1 )
     {
        cString = (char *) hb_xgrab( uLen );
        WideCharToMultiByte( CP_ACP, 0, wString, uLen, cString, uLen, NULL, NULL );
     }
     else
     {
        cString = (char *) hb_xgrab( 1 );
        cString[0] = '\0';
     }

     //wprintf( L"\nWide: '%s'\n", wString );
     //printf( "\nAnsi: '%s'\n", cString );

     return cString;
  }

  //---------------------------------------------------------------------------//

  static void GetParams(DISPPARAMS * dParams)
  {
     VARIANTARG * pArgs = NULL;
     PHB_ITEM uParam;
     int n, nArgs, nArg, nParam;
     BSTR wString;
     BOOL bByRef;

     nArgs = hb_pcount() - 2;

     if( nArgs > 0 )
     {
        pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * nArgs );
        aPrgParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * nArgs );

        //printf( "Args: %i\n", nArgs );

        for( n = 0; n < nArgs; n++ )
        {
           // Pramateres processed in reveresed order.
           nArg = nArgs + 2 - n;
           nParam = nArgs - n;

           bByRef = s_OleRefFlags && s_OleRefFlags[ nParam - 1 ] == 'Y';

           //printf( "N: %i Arg: %i Type: %i ByRef: %i\n", n, nArg, hb_stackItemFromBase( nArg  )->type, bByRef );

           VariantInit( &( pArgs[ n ] ) );

           uParam = hb_param( nArg, HB_IT_ANY );

           aPrgParams[ n ] = uParam;

           switch( uParam->type )
           {
              case HB_IT_NIL:
                pArgs[ n ].n1.n2.vt   = VT_EMPTY;
                break;

              case HB_IT_STRING:
              case HB_IT_MEMO:
                if( bByRef )
                {
                   wString = AnsiToWide( hb_parc( nArg ) );
                   hb_itemReleaseString( uParam );
                   uParam->item.asString.value = (char *) SysAllocString( wString );
                   hb_xfree( wString );
                   pArgs[ n ].n1.n2.vt   = VT_BYREF | VT_BSTR;
                   pArgs[ n ].n1.n2.n3.pbstrVal = (BSTR *) &( uParam->item.asString.value );
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_BSTR;
                   wString = AnsiToWide( hb_parc( nArg ) );
                   pArgs[ n ].n1.n2.n3.bstrVal = SysAllocString( wString );
                   hb_xfree( wString );
                }
                break;

              case HB_IT_LOGICAL:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_BOOL;
                   pArgs[ n ].n1.n2.n3.pboolVal = (short *) &( uParam->item.asLogical.value ) ;
                   uParam->type = HB_IT_LONG;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_BOOL;
                   pArgs[ n ].n1.n2.n3.boolVal = hb_parl( nArg );
                }
                break;

              case HB_IT_INTEGER:
              case HB_IT_LONG:
              case HB_IT_NUMERIC:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I4;
                   pArgs[ n ].n1.n2.n3.plVal = &( uParam->item.asLong.value ) ;
                   uParam->type = HB_IT_LONG;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I4;
                   pArgs[ n ].n1.n2.n3.lVal = hb_parnl( nArg );
                }
                break;

              case HB_IT_DOUBLE:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_R8;
                   pArgs[ n ].n1.n2.n3.pdblVal = &( uParam->item.asDouble.value ) ;
                   uParam->type = HB_IT_DOUBLE;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_R8;
                   pArgs[ n ].n1.n2.n3.dblVal = hb_parnd( nArg );
                }
                break;

              case HB_IT_DATE:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_DATE;
                   uParam->item.asDouble.value = DateToDbl( hb_pards( nArg ) );
                   pArgs[ n ].n1.n2.n3.pdblVal = &( uParam->item.asDouble.value ) ;
                   uParam->type = HB_IT_DOUBLE;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_DATE;
                   pArgs[ n ].n1.n2.n3.dblVal = DateToDbl( hb_pards( nArg ) );
                }
                break;

              case HB_IT_OBJECT:
              {
                 pArgs[ n ].n1.n2.vt = VT_EMPTY;

                 if( strcmp( hb_objGetClsName( uParam ), "TOLEAUTO" ) == 0 )
                 {
                    if( s_pSym_hObj == NULL )
                    {
                      s_pSym_hObj = hb_dynsymFindName( "HOBJ" );
                    }

                    if( s_pSym_hObj )
                    {
                       hb_vmPushSymbol( s_pSym_hObj->pSymbol );
                       hb_vmPush( uParam );
                       hb_vmSend( 0 );
                       //TraceLog( NULL, "\n#%i Dispatch: %ld\n", n, hb_parnl( -1 ) );
                       pArgs[ n ].n1.n2.vt = VT_DISPATCH;
                       pArgs[ n ].n1.n2.n3.pdispVal = ( IDispatch * ) hb_parnl( -1 );
                       //printf( "\nDispatch: %p\n", pArgs[ n ].n1.n2.n3.pdispVal );
                    }
                 }
                 else
                 {
                    TraceLog( NULL, "Class: '%s' not suported!\n", hb_objGetClsName( uParam ) );
                 }
              }
              break;
           }
        }
     }

     dParams->rgvarg            = pArgs;
     dParams->cArgs             = nArgs;
     dParams->rgdispidNamedArgs = 0;
     dParams->cNamedArgs        = 0;
  }

  //---------------------------------------------------------------------------//

  static void FreeParams(DISPPARAMS * dParams)
  {
     int n, nParam;
     char *sString;

     if( dParams->cArgs > 0 )
     {
        for( n = 0; n < ( int ) dParams->cArgs; n++ )
        {
           nParam = dParams->cArgs - n;

           //TraceLog( NULL, "*** N: %i, Param: %i Type: %i\n", n, nParam, dParams->rgvarg[ n ].n1.n2.vt );

           if( s_OleRefFlags && s_OleRefFlags[ nParam - 1 ] == 'Y' )
           {
              switch( dParams->rgvarg[ n ].n1.n2.vt )
              {
                 case VT_BYREF | VT_BSTR:
                   //printf( "String\n" );
                   sString = WideToAnsi( *( dParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );
                   SysFreeString( *( dParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );

                   // The item should NOT be cleared because we released the value above.
                   aPrgParams[ n ]->type = HB_IT_NIL;

                   // The Arg should NOT be cleared because we released the value above.
                   dParams->rgvarg[ n ].n1.n2.vt = VT_EMPTY;

                   hb_itemPutCPtr( aPrgParams[ n ], sString, strlen( sString ) );
                   break;

                 // Already using the PHB_ITEM allocated value
                 /*
                 case VT_BYREF | VT_BOOL:
                   //printf( "Logical\n" );
                   ( aPrgParams[ n ] )->type = HB_IT_LOGICAL;
                   ( aPrgParams[ n ] )->item.asLogical.value = dParams->rgvarg[ n ].n1.n2.n3.boolVal ;
                   break;
                 */

                 case VT_DISPATCH:
                 case VT_BYREF | VT_DISPATCH:
                   //TraceLog( NULL, "Dispatch %p\n", dParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                   if( dParams->rgvarg[ n ].n1.n2.n3.pdispVal == NULL )
                   {
                      hb_itemClear( aPrgParams[ n ] );
                      break;
                   }

                   OleAuto.type = HB_IT_NIL;

                   if( s_pSym_OleAuto )
                   {
                      hb_vmPushSymbol( s_pSym_OleAuto->pSymbol );
                      hb_vmPushNil();
                      hb_vmDo( 0 );

                      hb_itemForwardValue( &OleAuto, &hb_stack.Return );
                   }

                   if( s_pSym_New && OleAuto.type )
                   {
                      //TOleAuto():New( nDispatch )
                      hb_vmPushSymbol( s_pSym_New->pSymbol );
                      hb_itemPushForward( &OleAuto );
                      hb_vmPushLong( ( LONG ) dParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                      hb_vmSend( 1 );

                      hb_itemForwardValue( aPrgParams[ n ], &hb_stack.Return );
                   }
                   // Can't CLEAR this Variant
                   continue;

                 /*
                 case VT_BYREF | VT_I2:
                   //printf( "Int %i\n", dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   hb_itemPutNI( aPrgParams[ n ], ( int ) dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_I4:
                   //printf( "Long %ld\n", dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   hb_itemPutNL( aPrgParams[ n ], ( LONG ) dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_R8:
                   //printf( "Double\n" );
                   hb_itemPutND( aPrgParams[ n ],  dParams->rgvarg[ n ].n1.n2.n3.dblVal );
                   break;
                 */

                 case VT_BYREF | VT_DATE:
                   //printf( "Date\n" );
                   hb_itemPutDS( aPrgParams[ n ], DblToDate( *( dParams->rgvarg[ n ].n1.n2.n3.pdblVal ) ) );
                   break;

                 /*
                 case VT_BYREF | VT_EMPTY:
                   //printf( "Nil\n" );
                   hb_itemClear( aPrgParams[ n ] );
                   break;
                 */

                 default:
                   TraceLog( NULL, "*** Unexpected Type: %i [%p]***\n", dParams->rgvarg[ n ].n1.n2.vt, dParams->rgvarg[ n ].n1.n2.vt );
              }
           }
           else
           {
              switch( dParams->rgvarg[ n ].n1.n2.vt )
              {
                 case VT_DISPATCH:
                   //TraceLog( NULL, "***NOT REF*** Dispatch %p\n", dParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                   // Can'r CLEAR this Variant.
                   continue;
              }
           }

           VariantClear( &(dParams->rgvarg[ n ] ) );
        }

        hb_xfree( ( LPVOID ) dParams->rgvarg );

        if( aPrgParams )
        {
           hb_xfree( ( LPVOID ) aPrgParams );
           aPrgParams = NULL;
        }
     }
  }

  //---------------------------------------------------------------------------//

  static void RetValue( void )
  {
     LPSTR cString;

     /*
     printf( "Type: %i\n", RetVal.n1.n2.vt );
     fflush( stdout );
     getchar();
     */

     switch( RetVal.n1.n2.vt )
     {
        case VT_BSTR:
          //printf( "String\n" );
          cString = WideToAnsi( RetVal.n1.n2.n3.bstrVal );
          //printf( "cString %s\n", cString );
          hb_retcAdopt( cString );
          //printf( "Adopted\n" );
          break;

        case VT_BOOL:
          hb_retl( RetVal.n1.n2.n3.boolVal );
          break;

        case VT_DISPATCH:
          if( RetVal.n1.n2.n3.pdispVal == NULL )
          {
             hb_ret();
             break;
          }

          OleAuto.type = HB_IT_NIL;

          if( s_pSym_OleAuto )
          {
             hb_vmPushSymbol( s_pSym_OleAuto->pSymbol );
             hb_vmPushNil();
             hb_vmDo( 0 );

             hb_itemForwardValue( &OleAuto, &hb_stack.Return );
          }

          if( s_pSym_New && OleAuto.type )
          {
             //TOleAuto():New( nDispatch )
             hb_vmPushSymbol( s_pSym_New->pSymbol );
             hb_itemPushForward( &OleAuto );
             hb_vmPushLong( ( LONG ) RetVal.n1.n2.n3.pdispVal );
             hb_vmSend( 1 );
             //printf( "Dispatch: %ld %ld\n", ( LONG ) RetVal.n1.n2.n3.pdispVal, (LONG) hb_stack.Return.item.asArray.value );
          }
          break;

        case VT_I1:     // Byte
        case VT_UI1:
          hb_retni( ( short ) RetVal.n1.n2.n3.bVal );
          break;

        case VT_I2:     // Short (2 bytes)
        case VT_UI2:
          hb_retni( ( short ) RetVal.n1.n2.n3.iVal );
          break;

        case VT_I4:     // Long (4 bytes)
        case VT_UI4:
        case VT_INT:
        case VT_UINT:
          hb_retnl( ( LONG ) RetVal.n1.n2.n3.lVal );
          break;

        case VT_R4:     // Single
          hb_retnd( RetVal.n1.n2.n3.fltVal );
          break;

        case VT_R8:     // Double
          hb_retnd( RetVal.n1.n2.n3.dblVal );
          break;

        case VT_CY:     // Currency
        {
          double tmp = 0;
          VarR8FromCy( RetVal.n1.n2.n3.cyVal, &tmp );
          hb_retnd( tmp );
        }
          break;

        case VT_DECIMAL: // Decimal
          {
          double tmp = 0;
          VarR8FromDec( &RetVal.n1.decVal, &tmp );
          hb_retnd( tmp );
          }
          break;

        case VT_DATE:
          hb_retds( DblToDate( RetVal.n1.n2.n3.dblVal ) );
          break;

        case VT_EMPTY:
        case VT_NULL:
          hb_ret();
          break;

        default:
          //printf( "Default %i!\n", RetVal.n1.n2.vt );
          if( s_nOleError == S_OK )
          {
             (LONG) s_nOleError = -1;
          }

          hb_ret();
          break;
     }

     if( RetVal.n1.n2.vt == VT_DISPATCH && RetVal.n1.n2.n3.pdispVal )
     {
        //printf( "Dispatch: %ld\n", ( LONG ) RetVal.n1.n2.n3.pdispVal );
     }
     else
     {
        VariantClear( &RetVal );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( CREATEOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
  {
     BSTR wCLSID;
     IID ClassID, iid;
     LPIID riid = (LPIID) &IID_IDispatch;
     IDispatch * pDisp = NULL;

     s_nOleError = S_OK;

     if ( ( s_nOleError == S_OK ) || ( s_nOleError == (HRESULT) S_FALSE) )
     {
        wCLSID = AnsiToWide( hb_parc( 1 ) );

        if ( hb_parc( 1 )[ 0 ] == '{' )
        {
           s_nOleError = CLSIDFromString( wCLSID, (LPCLSID) &ClassID );
        }
        else
        {
           s_nOleError = CLSIDFromProgID( wCLSID, (LPCLSID) &ClassID );
        }

        hb_xfree( wCLSID );

        if ( hb_pcount() == 2 )
        {
           if ( hb_parc( 2 )[ 0 ] == '{' )
           {
              wCLSID = AnsiToWide( hb_parc( 2 ) );
              s_nOleError = CLSIDFromString( wCLSID, &iid );
              hb_xfree( wCLSID );
           }
           else
           {
              memcpy( ( LPVOID ) &iid, hb_parc( 2 ), sizeof( iid ) );
           }

           riid = &iid;
        }

        if ( s_nOleError == S_OK )
        {
           s_nOleError = CoCreateInstance( (REFCLSID) &ClassID, NULL, CLSCTX_SERVER, riid, (void **) &pDisp );
        }
     }

     hb_retnl( ( LONG ) pDisp );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( GETOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
  {
     BSTR wCLSID;
     IID ClassID, iid;
     LPIID riid = (LPIID) &IID_IDispatch;
     IDispatch *pDisp = NULL;
     IUnknown *pUnk = NULL;
     //LPOLESTR pOleStr = NULL;

     s_nOleError = S_OK;

     if ( ( s_nOleError == S_OK ) || ( s_nOleError == (HRESULT) S_FALSE) )
     {
        wCLSID = AnsiToWide( hb_parc( 1 ) );

        if ( hb_parc( 1 )[ 0 ] == '{' )
        {
           s_nOleError = CLSIDFromString( wCLSID, (LPCLSID) &ClassID );
        }
        else
        {
           s_nOleError = CLSIDFromProgID( wCLSID, (LPCLSID) &ClassID );
        }

        //s_nOleError = ProgIDFromCLSID( &ClassID, &pOleStr );
        //wprintf( L"Result %i ProgID: '%s'\n", s_nOleError, pOleStr );

        hb_xfree( wCLSID );

        if ( hb_pcount() == 2 )
        {
           if ( hb_parc( 2 )[ 0 ] == '{' )
           {
              wCLSID = AnsiToWide( hb_parc( 2 ) );
              s_nOleError = CLSIDFromString( wCLSID, &iid );
              hb_xfree( wCLSID );
           }
           else
           {
              memcpy( ( LPVOID ) &iid, hb_parc( 2 ), sizeof( iid ) );
           }

           riid = &iid;
        }

        if ( s_nOleError == S_OK )
        {
           s_nOleError = GetActiveObject( &ClassID, NULL, &pUnk );

           if ( s_nOleError == S_OK )
           {
              s_nOleError = pUnk->lpVtbl->QueryInterface( pUnk, riid, (void **) &pDisp );
           }
        }
     }

     hb_retnl( ( LONG ) pDisp );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLESHOWEXCEPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source, description;

        source = WideToAnsi( excep.bstrSource );
        description = WideToAnsi( excep.bstrDescription );
        MessageBox( NULL, description, source, MB_ICONHAND );
        hb_xfree( source );
        hb_xfree( description );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEEXCEPTIONSOURCE )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source;

        source = WideToAnsi( excep.bstrSource );
        hb_retcAdopt( source );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEEXCEPTIONDESCRIPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR description;

        description = WideToAnsi( excep.bstrDescription );
        hb_retcAdopt( description );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEINVOKE ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID;
     DISPPARAMS dParams;
     UINT uArgErr;

     VariantInit( &RetVal );
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     //printf( "1\n" );

     wMember = AnsiToWide( hb_parc( 2 ) );
     s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     //printf( "2\n" );

     if( s_nOleError == S_OK )
     {
        GetParams( &dParams );

        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                             lDispID,
                                             &IID_NULL,
                                             LOCALE_USER_DEFAULT,
                                             DISPATCH_METHOD,
                                             &dParams,
                                             &RetVal,
                                             &excep,
                                             &uArgErr ) ;

        //TraceLog( NULL, "Invoke '%s' Result: %p\n", hb_parc(2), s_nOleError );

        FreeParams( &dParams );

        RetValue();
     }
     else
     {
        //TraceLog( NULL, "Invoke GetIDsOfNames '%s' Error: %p\n", hb_parc(2), s_nOleError );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLESETPROPERTY ) // (hOleObject, cPropName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID, lPropPut = DISPID_PROPERTYPUT;
     WORD wFlags;
     DISPPARAMS dParams;
     UINT uArgErr;

     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     wMember = AnsiToWide( hb_parc( 2 ) );
     s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     if( s_nOleError == S_OK )
     {
        GetParams( &dParams );
        dParams.rgdispidNamedArgs = &lPropPut;
        dParams.cNamedArgs = 1;

        if( dParams.rgvarg[0].n1.n2.vt == VT_DISPATCH )
        {
           wFlags = DISPATCH_PROPERTYPUTREF | DISPATCH_METHOD;
           //TraceLog( NULL, "SetProperty '%s' BYREF\n", hb_parc(2) );
        }
        else
        {
           wFlags = DISPATCH_PROPERTYPUT | DISPATCH_METHOD;
        }

        //TraceLog( NULL, "SetProperty '%s' Args: %i\n", hb_parc(2), dParams.cArgs );

        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                           lDispID,
                                           &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           wFlags,
                                           &dParams,
                                           NULL,    // No return value
                                           &excep,
                                           &uArgErr );

        //TraceLog( NULL, "SetProperty '%s' Result: %p\n", hb_parc(2), s_nOleError );

        FreeParams( &dParams );

        hb_ret();
     }
     else
     {
        //TraceLog( NULL, "SetProperty GetIDsOfNames '%s' Error: %p\n", hb_parc(2), s_nOleError );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEGETPROPERTY )  // (hOleObject, cPropName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID;
     DISPPARAMS dParams;
     UINT uArgErr;

     VariantInit( &RetVal );

     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     wMember = AnsiToWide( hb_parc( 2 ) );
     s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     if( s_nOleError == S_OK )
     {
        GetParams( &dParams );

        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                           lDispID,
                                           &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           DISPATCH_PROPERTYGET,
                                           &dParams,
                                           &RetVal,
                                           &excep,
                                           &uArgErr );

        //TraceLog( NULL, "GetProperty '%s' Result: %p\n", hb_parc(2), s_nOleError );

        FreeParams( &dParams );

        RetValue();
     }
     else
     {
        //TraceLog( NULL, "GetProperty GetIDsOfNames '%s' Error: %p\n", hb_parc(2), s_nOleError );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEQUERYINTERFACE )  // (hOleObject, cIID ) -> ppvObject
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );
     IUnknown * ppvObject = NULL;
     GUID iid;
     BSTR wiid;

     s_nOleError = S_OK;

     if( hb_parc( 2 )[ 0 ] == '{' )
     {
        wiid = AnsiToWide( hb_parc( 2 ) );
        s_nOleError = CLSIDFromString( wiid, &iid );
        hb_xfree( wiid );
     }
     else
     {
        memcpy( ( LPVOID ) &iid, hb_parc( 2 ), sizeof( iid ) );
     }

     if( s_nOleError == S_OK )
     {
        s_nOleError = pUnk ->lpVtbl->QueryInterface( pUnk, (const struct _GUID *const) &iid, (void **) &ppvObject );
     }

     hb_retnl( ( LONG ) ppvObject );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEADDREF )  // ( hOleObject )
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );

     hb_retnl( pUnk -> lpVtbl -> AddRef( pUnk ) );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLERELEASE )  // ( hOleObject )
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );

     hb_retnl( pUnk -> lpVtbl -> Release( pUnk ) );
  }

  //---------------------------------------------------------------------------//

  #if 0
  HB_FUNC( COMFUNCTION )  // ( hOleObject, nFunc, uParams... )
  {
     typedef HRESULT ( STDMETHODCALLTYPE * COMFunc ) ( IUnknown * pUnk );

     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );
     COMFunc pFunc;
     COMFunc *vTbl;
     int i, iParams = hb_pcount();
     double doubles[16];
     LPVOID ptros[16];

     vTbl = ( COMFunc * ) &( pUnk -> lpVtbl -> QueryInterface );
     vTbl += hb_parni( 2 ) + 2;
     pFunc = *vTbl;

     for( i = iParams; i > 2; i-- )
     {
        char *sString;
        int iInt;
        double dDouble;

        switch ( ( hb_parinfo( i ) & ~HB_IT_BYREF) )
        {
           case HB_IT_STRING:
           case HB_IT_MEMO:
             sString = hb_parc( i );
             __asm push sString
             break;

           case HB_IT_LOGICAL:
             if ( ISBYREF( i ) )
             {
                ptros[ i ] = (LPVOID) hb_parl( i );
                sString = ( char * ) &ptros[ i ];
                __asm push sString
             }
             else
             {
                iInt = hb_parl( i );
                __asm push iInt
             }
             break;

           case HB_IT_INTEGER:
           case HB_IT_LONG:
             if ( ISBYREF( i ) )
             {
                ptros[ i ] = (LPVOID) hb_parnl( i );
                sString = ( char * ) &ptros[ i ];
                __asm push sString
             }
             else
             {
                iInt = hb_parnl( i );
                __asm push iInt
             }
             break;

           case HB_IT_DOUBLE:
             if ( ISBYREF( i ) )
             {
                doubles[ i ] = hb_parnd( i );
                sString = ( char * ) &doubles[ i ];
                __asm push sString
             }
             else
             {
                dDouble = hb_parl( i );
                __asm push dDouble
             }
             break;

           case HB_IT_DATE:
             if ( ISBYREF( i ) )
             {
                doubles[ i ] = DateToDbl( hb_pards( i ) );
                sString = ( char * ) &doubles[ i ];
                __asm push sString
             }
             else
             {
                dDouble = DateToDbl( hb_pards( i ) );
                __asm push dDouble
             }
             break;

           default:
             sString = ( char * ) NULL;
             __asm push sString
             break;
        }
     }

     s_nOleError = pFunc( pUnk );

     for ( i = 3; i <= iParams; i++ )
     {
        if ( ISBYREF( i ) )
        {
           switch ( (hb_parinfo( i ) & ~HB_IT_BYREF) )
           {
              case HB_IT_STRING:
              case HB_IT_MEMO:
                hb_storc( (char *) hb_parc( i ), i );
                break;

              case HB_IT_LOGICAL:
                hb_storl( (long) ptros[ i ], i );
                break;

              case HB_IT_INTEGER:
              case HB_IT_LONG:
                hb_stornl( (long) ptros[ i ], i );
                break;

              case HB_IT_DOUBLE:
                hb_stornd( doubles[ i ], i );
                break;

              case HB_IT_DATE:
                hb_stords( DblToDate( doubles[ i ] ), i );
                break;
           }
        }
     }

     hb_ret();
  }
  #endif

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEERROR )
  {
     hb_retnl( (LONG) s_nOleError );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLE2TXTERROR )
  {
     switch ( (LONG) s_nOleError)
     {
        case S_OK:
           hb_retc( "S_OK" );
           break;

        case CO_E_CLASSSTRING:
           hb_retc( "CO_E_CLASSSTRING" );
           break;

        case OLE_E_WRONGCOMPOBJ:
           hb_retc( "OLE_E_WRONGCOMPOBJ" );
           break;

        case REGDB_E_CLASSNOTREG:
           hb_retc( "REGDB_E_CLASSNOTREG" );
           break;

        case REGDB_E_WRITEREGDB:
           hb_retc( "REGDB_E_WRITEREGDB" );
           break;

        case E_OUTOFMEMORY:
           hb_retc( "E_OUTOFMEMORY" );
           break;

        case E_INVALIDARG:
           hb_retc( "E_INVALIDARG" );
           break;

        case E_UNEXPECTED:
           hb_retc( "E_UNEXPECTED" );
           break;

        case DISP_E_UNKNOWNNAME:
           hb_retc( "DISP_E_UNKNOWNNAME" );
           break;

        case DISP_E_UNKNOWNLCID:
           hb_retc( "DISP_E_UNKNOWNLCID" );
           break;

        case DISP_E_BADPARAMCOUNT:
           hb_retc( "DISP_E_BADPARAMCOUNT" );
           break;

        case DISP_E_BADVARTYPE:
           hb_retc( "DISP_E_BADVARTYPE" );
           break;

        case DISP_E_EXCEPTION:
           hb_retc( "DISP_E_EXCEPTION" );
           break;

        case DISP_E_MEMBERNOTFOUND:
           hb_retc( "DISP_E_MEMBERNOTFOUND" );
           break;

        case DISP_E_NONAMEDARGS:
           hb_retc( "DISP_E_NONAMEDARGS" );
           break;

        case DISP_E_OVERFLOW:
           hb_retc( "DISP_E_OVERFLOW" );
           break;

        case DISP_E_PARAMNOTFOUND:
           hb_retc( "DISP_E_PARAMNOTFOUND" );
           break;

        case DISP_E_TYPEMISMATCH:
           hb_retc( "DISP_E_TYPEMISMATCH" );
           break;

        case DISP_E_UNKNOWNINTERFACE:
           hb_retc( "DISP_E_UNKNOWNINTERFACE" );
           break;

        case DISP_E_PARAMNOTOPTIONAL:
           hb_retc( "DISP_E_PARAMNOTOPTIONAL" );
           break;

        default:
           hb_retc( "Unknown error" );
           break;
     };
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( ANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
  {
     UINT uLen;
     BSTR wString;
     char *cString = hb_parc( 1 );

     if( cString == NULL )
     {
        hb_ret();
        return;
     }

     uLen = strlen( cString ) + 1;

     wString = ( BSTR ) hb_xgrab( uLen * 2 );
     MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, uLen, wString, uLen );

     hb_retclenAdopt( (char *) wString, uLen * 2 - 1 );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( WIDETOANSI )  // ( cWideStr, nLen ) -> cAnsiStr
  {
     UINT uLen;
     BSTR wString = ( BSTR ) hb_parc( 1 );
     char *cString;

     uLen = SysStringLen( wString ) + 1;

     cString = ( char * ) hb_xgrab( uLen );

     WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, wString, uLen, cString, uLen, NULL, NULL );

     hb_retclenAdopt( cString, uLen - 1 );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( MESSAGEBOX )
  {
     hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parc( 2 ), hb_parc( 3 ), hb_parni( 4 ) ) );
  }

  //---------------------------------------------------------------------------//

#pragma ENDDUMP

#endif
