/*
 * $Id: errorapi.c,v 1.18 2003/08/20 20:06:06 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * The Error API
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    DOSERROR()
 *    __ERRINHANDLER()
 *    __ERRRT_BASE()
 *    __ERRRT_SBASE()
 *    hb_errLaunch()
 *    hb_errLaunchSubst()
 *    hb_errGetFlags()
 *    hb_errPutFlags()
 *    hb_errRT_New()
 *    hb_errRT_New_Subst()
 *    hb_errRT_BASE()
 *    hb_errRT_BASE_Ext1()
 *    hb_errRT_BASE_Subst()
 *    hb_errInternal()
 *
 * See doc/license.txt for licensing terms.
 *
 */
/*JC1: say we are going to optimze MT stack */
#define HB_THREAD_OPTIMIZE_STACK

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbset.h"

#ifdef HB_OS_WIN_32
   #define HB_OS_WIN_32_USED
   #include "windows.h"
#endif

/* This is added to be able to detect a recursive error, and not let Harbour
   go into an infinite loop, this is an emulated version of the Clipper
   "Unrecoverable error 650: Processor stack fault" internal error, but
   better shows what is really the problem. [vszakats] */
#define HB_ERROR_LAUNCH_MAX hb_set.HB_SET_ERRORLOOP

/* In MT, this data is held in the stack */
#ifndef HB_THREAD_SUPPORT
   static HB_ERROR_INFO_PTR s_errorHandler = NULL;
   static HB_ITEM_PTR s_errorBlock;
   static int     s_iLaunchCount = 0;
   static USHORT  s_uiErrorDOS = 0; /* The value of DOSERROR() */
#else
   #define s_errorHandler      (HB_VM_STACK.errorHandler)
   #define s_errorBlock        (HB_VM_STACK.errorBlock)
   #define s_iLaunchCount      (HB_VM_STACK.iLaunchCount)
   #define s_uiErrorDOS        (HB_VM_STACK.uiErrorDOS)
#endif

extern HB_SET_STRUCT hb_set;

extern HB_FUNC( ERRORNEW );

PHB_ITEM HB_EXPORT hb_errPutModuleName( PHB_ITEM pError, char * szModuleName );

/* NOTE: This is called via its symbol name, so we should make sure
         that it gets linked. WARNING ! DON'T make this function static.
         [vszakats] */
void hb_errForceLink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errForceLink()"));

   HB_FUNCNAME( ERRORNEW )();
}

/* There's a similar undocumented, internal functions in CA-Cl*pper named
   ErrorInHandler(). [vszakats] */

HB_FUNC( __ERRINHANDLER )
{
   hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
}

HB_FUNC( ERRORBLOCK )
{
   HB_THREAD_STUB

   HB_ITEM oldError;
   PHB_ITEM pNewErrorBlock = hb_param( 1, HB_IT_BLOCK );

   /* initialize an item
    * NOTE: hb_itemClear() cannot be used to initialize an item because
    * memory occupied by the item can contain garbage bits
   */

   ( &oldError )->type = HB_IT_NIL;
   hb_itemCopy( &oldError, s_errorBlock );

   if( pNewErrorBlock )
   {
      hb_itemCopy( s_errorBlock, pNewErrorBlock );
   }

   hb_itemReturn( &oldError );
}

/* set new low-level error launcher (C function) and return
 * handler currently active
 */
HB_ERROR_INFO_PTR HB_EXPORT hb_errorHandler( HB_ERROR_INFO_PTR pNewHandler )
{
   HB_THREAD_STUB
   HB_ERROR_INFO_PTR pOld = s_errorHandler;

   if( pNewHandler )
      pNewHandler->Previous = s_errorHandler;
   s_errorHandler = pNewHandler;

   return pOld;
}

/* TOFIX: Make it Clipper compatible. [vszakats] */

HB_FUNC( DOSERROR )
{
   HB_THREAD_STUB

   hb_retni( s_uiErrorDOS );

   if( ISNUM( 1 ) )
         s_uiErrorDOS = ( USHORT ) hb_parni( 1 );
}

void HB_EXPORT hb_errInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errInit()"));

   /* initialize an item
    * NOTE: hb_itemClear() cannot be used to initialize an item because
    * memory occupied by the item can contain garbage bits
   */
   #ifndef HB_THREAD_SUPPORT
      s_errorBlock = hb_itemNew( NULL );
   #endif
}

void HB_EXPORT hb_errExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errExit()"));

   #ifndef HB_THREAD_SUPPORT
      if( s_errorBlock && s_errorBlock->type )
      {
         hb_itemRelease( s_errorBlock );
      }
   #endif
}

PHB_ITEM HB_EXPORT hb_errNew( void )
{
   HB_THREAD_STUB

   static PHB_DYNS pDyn;
   PHB_ITEM pError;
   char *szModuleName;

   HB_TRACE(HB_TR_DEBUG, ("hb_errNew()"));

   if( pDyn == NULL )
   {
      pDyn = hb_dynsymGet( "ERRORNEW" );

      if( pDyn == NULL )
      {
         hb_errInternal( HB_EI_ERRUNRECOV, "Couldn't locate ErrorNew() symbol in hb_errNew()", NULL, NULL );
      }
   }

   if( (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym && (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym->pModuleSymbols )
   {
      szModuleName = (* HB_VM_STACK.pBase)->item.asSymbol.value->pDynSym->pModuleSymbols->szModuleName;
   }
   else
   {
      szModuleName = NULL;
   }

   pError = hb_itemNew( NULL );

   hb_vmPushSymbol( pDyn->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   hb_itemForwardValue( pError, &(HB_VM_STACK.Return) );

   if( ! HB_IS_OBJECT( pError ) )
   {
      hb_errInternal( HB_EI_ERRUNRECOV, "Couldn't create Error object in hb_errNew()", NULL, NULL );
   }

   if( szModuleName )
   {
      hb_errPutModuleName( pError, szModuleName );
   }

   return pError;
}

USHORT HB_EXPORT hb_errLaunch( PHB_ITEM pError )
{
   HB_THREAD_STUB

   USHORT uiAction = E_DEFAULT; /* Needed to avoid GCC -O2 warning */
   USHORT usRequest;

   HB_TRACE(HB_TR_DEBUG, ("hb_errLaunch(%p)", pError));

   if( pError )
   {
      PHB_ITEM pResult;

      /* Check if we have a valid error handler */

      if( hb_itemType( s_errorBlock ) != HB_IT_BLOCK )
      {
         hb_errInternal( HB_EI_ERRNOBLOCK, NULL, NULL, NULL );
      }

      /* Check if the error launcher was called too many times recursively */
      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
      {
         hb_errInternal( HB_EI_ERRTOOMANY, NULL, NULL, NULL );
      }

      /* Launch the error handler: "lResult := EVAL( ErrorBlock(), oError )" */
      s_iLaunchCount++;

      if( s_errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Harbour-level one
          */
         s_errorHandler->Error = pError;
         s_errorHandler->ErrorBlock = s_errorBlock;
         pResult = (s_errorHandler->Func)( s_errorHandler );
         s_errorHandler->Error = NULL;
      }
      else
      {
         pResult = hb_itemDo( s_errorBlock, 1, pError );
      }

      s_iLaunchCount--;

      /* Check results */

      usRequest = hb_vmRequestQuery();

      if( usRequest == HB_QUIT_REQUESTED )
      {
         if( pResult )
         {
             hb_itemRelease( pResult );
         }

         hb_errRelease( pError );
         hb_vmQuit();
      }
      else if( usRequest == HB_BREAK_REQUESTED || usRequest == HB_ENDPROC_REQUESTED )
      {
         if( pResult )
         {
             hb_itemRelease( pResult );
         }

         uiAction = E_BREAK;
      }
      else if( pResult )
      {
         BOOL bFailure = FALSE;
         USHORT uiFlags = hb_errGetFlags( pError );

         /* If the error block didn't return a logical value, */
         /* or the canSubstitute flag has been set, consider it as a failure */

         if( hb_itemType( pResult ) != HB_IT_LOGICAL || ( uiFlags & EF_CANSUBSTITUTE ) )
         {
            bFailure = TRUE;
         }
         else
         {
            uiAction = hb_itemGetL( pResult ) ? E_RETRY : E_DEFAULT;

            if( ( uiAction == E_DEFAULT && !( uiFlags & EF_CANDEFAULT ) ) ||
                ( uiAction == E_RETRY   && !( uiFlags & EF_CANRETRY   ) ) )
            {
               bFailure = TRUE;
            }
         }

         hb_itemRelease( pResult );

         if( bFailure )
         {
            hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
         }

         /* Add one try to the counter. */

         if( uiAction == E_RETRY )
         {
            hb_errPutTries( pError, hb_errGetTries( pError ) + 1 );
         }
      }
      else
      {
         hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
      }
   }
   else
   {
      uiAction = E_RETRY; /* Clipper does this, undocumented */
   }

   return uiAction;
}

/* This error launcher should be used in those situations, where the error
   handler is expected to return a value to be substituted as the result of
   a failed operation. [vszakats] */

/* NOTE: This should only be called when the EF_CANSUBSTITUE flag was set
         Since in this case the error handler will return the value
         to be substituted. [vszakats] */

/* NOTE: The item pointer returned should be hb_itemRelease()-d by the
         caller if it was not NULL. [vszakats] */

PHB_ITEM HB_EXPORT hb_errLaunchSubst( PHB_ITEM pError )
{
   HB_THREAD_STUB

   PHB_ITEM pResult;
   USHORT usRequest;

   HB_TRACE(HB_TR_DEBUG, ("hb_errLaunchSubst(%p)", pError));

   if( pError )
   {
      /* Check if we have a valid error handler */

      if( hb_itemType( s_errorBlock ) != HB_IT_BLOCK )
      {
         hb_errInternal( HB_EI_ERRNOBLOCK, NULL, NULL, NULL );
      }

      /* Check if the error launcher was called too many times recursively */
      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
      {
         hb_errInternal( HB_EI_ERRTOOMANY, NULL, NULL, NULL );
      }

      /* Launch the error handler: "xResult := EVAL( ErrorBlock(), oError )" */
      s_iLaunchCount++;

      if( s_errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Harbour-level one
          */
         s_errorHandler->Error = pError;
         s_errorHandler->ErrorBlock = s_errorBlock;
         pResult = ( s_errorHandler->Func )( s_errorHandler );
         s_errorHandler->Error = NULL;
      }
      else
      {
         pResult = hb_itemDo( s_errorBlock, 1, pError );
      }

      s_iLaunchCount--;

      /* Check results */
      usRequest = hb_vmRequestQuery();

      if( usRequest == HB_QUIT_REQUESTED )
      {
         if( pResult )
         {
            hb_itemRelease( pResult );
         }

         hb_errRelease( pError );
         hb_vmQuit();
      }
      else if( usRequest == HB_BREAK_REQUESTED || usRequest == HB_ENDPROC_REQUESTED )
      {
         if( pResult )
         {
            hb_itemRelease( pResult );
         }

         pResult = NULL;
      }
      else
      {
         /* If the canSubstitute flag has not been set,
            consider it as a failure. */

         if( ! ( hb_errGetFlags( pError ) & EF_CANSUBSTITUTE ) )
         {
            hb_errInternal( HB_EI_ERRRECFAILURE, NULL, NULL, NULL );
         }
      }
   }
   else
   {
      pResult = hb_itemNew( NULL );
   }

   return pResult;
}

void HB_EXPORT hb_errRelease( PHB_ITEM pError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errRelease(%p)", pError));

   /* NOTE: NULL pointer is checked by hb_itemRelease() [vszakats] */
   hb_itemRelease( pError );
}

char HB_EXPORT * hb_errGetDescription( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetDescription(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "DESCRIPTION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetCPtr( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutDescription( PHB_ITEM pError, char * szDescription )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutDescription(%p, %s)", pError, szDescription));

   if( szDescription )
   {
      hb_vmPushSymbol( hb_dynsymGet( "_DESCRIPTION" )->pSymbol );
      hb_vmPush( pError );
      hb_vmPushString( szDescription, strlen( szDescription ) );

      hb_vmSend( 1 );
   }

   return pError;
}

char HB_EXPORT * hb_errGetFileName( PHB_ITEM pError )
{
   HB_THREAD_STUB
   HB_TRACE(HB_TR_DEBUG, ("hb_errGetFileName(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "FILENAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetCPtr( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutFileName( PHB_ITEM pError, char * szFileName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutFileName(%p, %s)", pError, szFileName));

   hb_vmPushSymbol( hb_dynsymGet( "_FILENAME" )->pSymbol );
   hb_vmPush( pError );

   if( szFileName )
   {
      hb_vmPushString( szFileName, strlen( szFileName ) );
   }
   else
   {
      hb_itemPushStaticString( "", 0 );
   }

   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetGenCode( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetGenCode(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "GENCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutGenCode( PHB_ITEM pError, USHORT uiGenCode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutGenCode(%p, %hu)", pError, uiGenCode));

   hb_vmPushSymbol( hb_dynsymGet( "_GENCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiGenCode );
   hb_vmSend( 1 );

   return pError;
}

char HB_EXPORT * hb_errGetOperation( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetOperation(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "OPERATION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetCPtr( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutOperation( PHB_ITEM pError, char * szOperation )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutOperation(%p, %s)", pError, szOperation));

   hb_vmPushSymbol( hb_dynsymGet( "_OPERATION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szOperation, strlen( szOperation ) );
   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetOsCode( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetOsCode(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "OSCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutOsCode( PHB_ITEM pError, USHORT uiOsCode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutOsCode(%p, %hu)", pError, uiOsCode));

   hb_vmPushSymbol( hb_dynsymGet( "_OSCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiOsCode );
   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetSeverity( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetSeverity(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "SEVERITY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutSeverity(%p, %hu)", pError, uiSeverity));

   hb_vmPushSymbol( hb_dynsymGet( "_SEVERITY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiSeverity );
   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetSubCode( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetSubCode(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "SUBCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutSubCode( PHB_ITEM pError, USHORT uiSubCode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutSubCode(%p, %hu)", pError, uiSubCode));

   hb_vmPushSymbol( hb_dynsymGet( "_SUBCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiSubCode );
   hb_vmSend( 1 );

   return pError;
}

char HB_EXPORT * hb_errGetSubSystem( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetSubSytem(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "SUBSYSTEM" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetCPtr( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutSubSystem( PHB_ITEM pError, char * szSubSystem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutSubSytem(%p, %s)", pError, szSubSystem));

   hb_vmPushSymbol( hb_dynsymGet( "_SUBSYSTEM" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szSubSystem, strlen( szSubSystem ) );
   hb_vmSend( 1 );

   return pError;
}

char HB_EXPORT * hb_errGetProcName( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetProcName(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "PROCNAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetCPtr( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutProcName( PHB_ITEM pError, char * szProcName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutProcName(%p, %s)", pError, szProcName));

   hb_vmPushSymbol( hb_dynsymGet( "_PROCNAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szProcName, strlen( szProcName ) );
   hb_vmSend( 1 );

   return pError;
}


USHORT hb_errGetProcLine( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetProcLine(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "PROCLINE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return (USHORT) hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutProcLine( PHB_ITEM pError, USHORT uiLine )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutProcLINE(%p, %d)", pError, uiLine));

   hb_vmPushSymbol( hb_dynsymGet( "_PROCLINE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( (int) uiLine );
   hb_vmSend( 1 );

   return pError;
}

#ifdef HB_THREAD_SUPPORT

HB_THREAD_T hb_errGetThreadId( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetThreadId(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "OSTHREADID" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return (HB_THREAD_T) hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutThreadId( PHB_ITEM pError, HB_THREAD_T thId)
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutThreadId(%p, %X)", pError, (int)thId));

   hb_vmPushSymbol( hb_dynsymGet( "_RUNNINGTHREADS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( (HB_THREAD_T) thId  );
   hb_vmSend( 1 );

   return pError;
}


UINT hb_errGetRunningThreads( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetRunningThreads(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "RUNNINGTHREADS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return (UINT) hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutRunningThreads( PHB_ITEM pError, UINT uiCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutRunningThreads(%p, %d)", pError, uiCount));

   hb_vmPushSymbol( hb_dynsymGet( "_RUNNINGTHREADS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( (int) uiCount );
   hb_vmSend( 1 );

   return pError;
}

UINT hb_errGetVmThreadId( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetVmThreadId(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "VMTHREADID" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return (UINT) hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutVmThreadId( PHB_ITEM pError, UINT uiThid )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutRunningThreads(%p, %d)", pError, uiThid));

   hb_vmPushSymbol( hb_dynsymGet( "_VMTHREADID" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( (int) uiThid );
   hb_vmSend( 1 );

   return pError;
}

#endif

PHB_ITEM HB_EXPORT hb_errPutModuleName( PHB_ITEM pError, char * szModuleName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutModuleName(%p, %s)", pError, szModuleName));

   hb_vmPushSymbol( hb_dynsymGet( "_MODULENAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szModuleName, strlen( szModuleName ) );
   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetTries( PHB_ITEM pError )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetTries(%p)", pError));

   hb_vmPushSymbol( hb_dynsymGet( "TRIES" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   return hb_itemGetNI( &(HB_VM_STACK.Return) );
}

PHB_ITEM HB_EXPORT hb_errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutTries(%p, %hu)", pError, uiTries));

   hb_vmPushSymbol( hb_dynsymGet( "_TRIES" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiTries );
   hb_vmSend( 1 );

   return pError;
}

USHORT HB_EXPORT hb_errGetFlags( PHB_ITEM pError )
{
   HB_THREAD_STUB

   USHORT uiFlags = EF_NONE;

   HB_TRACE(HB_TR_DEBUG, ("hb_errGetFlags(%p)", pError));

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANRETRY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   if( hb_itemGetL( &(HB_VM_STACK.Return) ) )
      uiFlags |= EF_CANRETRY;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   if( hb_itemGetL( &(HB_VM_STACK.Return) ) )
      uiFlags |= EF_CANSUBSTITUTE;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmSend( 0 );

   if( hb_itemGetL( &(HB_VM_STACK.Return) ) )
      uiFlags |= EF_CANDEFAULT;

   /* ; */

   return uiFlags;
}

PHB_ITEM HB_EXPORT hb_errPutFlags( PHB_ITEM pError, USHORT uiFlags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_errPutFlags(%p, %hu)", pError, uiFlags));

   hb_vmPushSymbol( hb_dynsymGet( "_CANRETRY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANRETRY ) ? TRUE : FALSE );
   hb_vmSend( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANSUBSTITUTE ) ? TRUE : FALSE );
   hb_vmSend( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANDEFAULT ) ? TRUE : FALSE );
   hb_vmSend( 1 );

   /* ; */

   return pError;
}

PHB_ITEM HB_EXPORT hb_errPutArgs( PHB_ITEM pError, ULONG ulArgCount, ... )
{
   PHB_ITEM pArray;
   ULONG ulArgPos;
   va_list va;

   HB_TRACE(HB_TR_DEBUG, ("hb_errPutArgs(%p, %hu, ...)", pError, ulArgCount));

   pArray = hb_itemArrayNew( ulArgCount );

   /* Build the array from the passed arguments. */

   va_start( va, ulArgCount );
   for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
   {
      PHB_ITEM pVaItem = va_arg( va, PHB_ITEM );

      if ( pVaItem == NULL )
      {
         pVaItem = hb_itemNew( NULL );
      }
      hb_arraySet( pArray, ulArgPos, pVaItem );
   }
   va_end( va );

   /* Assign the new array to the object data item. */

   hb_vmPushSymbol( hb_dynsymGet( "_ARGS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPush( pArray );
   hb_vmSend( 1 );

   hb_itemRelease( pArray );

   return pError;
}

/* Wrappers for hb_errLaunch() */

PHB_ITEM HB_EXPORT hb_errRT_New(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags )
{
   HB_THREAD_STUB

   PHB_ITEM pError = hb_errNew();
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   USHORT uLine;

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ( USHORT ) ulGenCode );
   hb_errPutSubCode( pError, ( USHORT ) ulSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRDESC + ulGenCode ) );
   hb_errPutOperation( pError, szOperation ? szOperation : "" );
   hb_errPutOsCode( pError, uiOsCode );
   hb_errPutFlags( pError, uiFlags );

   hb_errPutProcName( pError, hb_procinfo( 0, szName, &uLine, NULL ) );
   hb_errPutProcLine( pError, uLine );

   #ifdef HB_THREAD_SUPPORT
      hb_errPutThreadId( pError, HB_CURRENT_THREAD() );
      hb_errPutVmThreadId( pError, HB_VM_STACK.th_vm_id );
      hb_errPutRunningThreads( pError, hb_threadCountStacks() );
   #endif

   return pError;
}

PHB_ITEM HB_EXPORT hb_errRT_New_Subst(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags )
{
   HB_THREAD_STUB

   PHB_ITEM pError = hb_errNew();
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   USHORT uLine;

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ( USHORT ) ulGenCode );
   hb_errPutSubCode( pError, ( USHORT ) ulSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRDESC + ulGenCode ) );
   hb_errPutOperation( pError, szOperation ? szOperation : "" );
   hb_errPutOsCode( pError, uiOsCode );
   hb_errPutFlags( pError, uiFlags | EF_CANSUBSTITUTE );

   hb_errPutProcName( pError, hb_procinfo( 0, szName, &uLine, NULL ) );
   hb_errPutProcLine( pError, uLine );

   #ifdef HB_THREAD_SUPPORT
      hb_errPutThreadId( pError, HB_CURRENT_THREAD() );
      hb_errPutVmThreadId( pError, HB_VM_STACK.th_vm_id );
      hb_errPutRunningThreads( pError, hb_threadCountStacks() );
   #endif

   return( pError );
}

HB_FUNC( __ERRRT_BASE )
{
   hb_errRT_BASE( ( ULONG ) hb_parnl( 1 ),
                  ( ULONG ) hb_parnl( 2 ),
                  hb_parc( 3 ),
                  hb_parc( 4 ),
                  ( USHORT ) hb_parni( 5 ),
                  hb_param( 6, HB_IT_ANY ) );
}

HB_FUNC( __ERRRT_SBASE )
{
   hb_errRT_BASE_SubstR( ( ULONG ) hb_parnl( 1 ),
                         ( ULONG ) hb_parnl( 2 ),
                         hb_parc( 3 ),
                         hb_parc( 4 ),
                         ( USHORT ) hb_parni( 5 ),
                         hb_param( 6, HB_IT_ANY ) );
}

USHORT HB_EXPORT hb_errRT_BASE( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... )
{
   USHORT uiAction;
   PHB_ITEM pError;

   PHB_ITEM pArray, pArg;
   va_list va;
   ULONG ulArgPos;
   BOOL bRelease = TRUE;

   /* Build the array from the passed arguments. */
   va_start( va, ulArgCount );
   if( ( ulSubCode == 1001 || ulSubCode == 1004 || ulSubCode == 1005 ) && ulArgCount == 1 )
   {
      pArray = va_arg( va, PHB_ITEM );

      if( HB_IS_ARRAY( pArray ) )
      {
         bRelease = FALSE;
      }
      else
      {
         pArg = pArray;
         pArray = hb_itemArrayNew( 1 );
         hb_arraySet( pArray, 1, pArg );
      }
   }
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pVaItem = va_arg( va, PHB_ITEM );

         if ( pVaItem == NULL )
         {
            pVaItem = hb_itemNew( NULL );
         }
         hb_arraySet( pArray, ulArgPos, pVaItem );
      }
   }
   va_end( va );

   pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Assign the new array to the object data item. */
   hb_vmPushSymbol( hb_dynsymGet( "_ARGS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPush( pArray );
   hb_vmSend( 1 );

   /* Release the Array. */
   if( bRelease )
   {
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   uiAction = hb_errLaunch( pError );

   /* Release. */
   hb_errRelease( pError );

   return uiAction;
}

USHORT HB_EXPORT hb_errRT_BASE_Ext1( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOsCode, USHORT uiFlags, ULONG ulArgCount, ... )
{
   USHORT uiAction;
   PHB_ITEM pError;

   PHB_ITEM pArray;
   va_list va;
   ULONG ulArgPos;

   pArray = hb_itemArrayNew( ulArgCount );

   /* Build the array from the passed arguments. */
   va_start( va, ulArgCount );
   for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
   {
      PHB_ITEM pVaItem = va_arg( va, PHB_ITEM );

      if ( pVaItem == NULL )
      {
         pVaItem = hb_itemNew( NULL );
      }
      hb_arraySet( pArray, ulArgPos, pVaItem );
   }

   va_end( va );

   pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, uiOsCode, uiFlags );

   /* Assign the new array to the object data item. */
   hb_vmPushSymbol( hb_dynsymGet( "_ARGS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPush( pArray );
   hb_vmSend( 1 );

   /* Release the Array. */
   hb_itemRelease( pArray );

   /* Ok, launch... */
   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

PHB_ITEM HB_EXPORT hb_errRT_BASE_Subst( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... )
{
   PHB_ITEM pRetVal;
   PHB_ITEM pError;

   PHB_ITEM pArray;
   va_list va;
   ULONG ulArgPos;

   HB_TRACE_STEALTH( HB_TR_DEBUG, ( "hb_errRT_BASE_Subst()") );

   pArray = hb_itemArrayNew( ulArgCount );

   /* Build the array from the passed arguments. */
   va_start( va, ulArgCount );
   for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
   {
       PHB_ITEM pVaItem = va_arg( va, PHB_ITEM );

      if ( pVaItem == NULL )
      {
         pVaItem = hb_itemNew( NULL );
      }
      hb_arraySet( pArray, ulArgPos, pVaItem );
   }
   va_end( va );

   pError = hb_errRT_New_Subst( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Assign the new array to the object data item. */
   hb_vmPushSymbol( hb_dynsymGet( "_ARGS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPush( pArray );
   hb_vmSend( 1 );

   /* Release the Array. */
   hb_itemRelease( pArray );

   /* Ok, launch... */
   pRetVal = hb_errLaunchSubst( pError );

   hb_errRelease( pError );

   return pRetVal;
}

void HB_EXPORT hb_errRT_BASE_SubstR( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, ULONG ulArgCount, ... )
{
   PHB_ITEM pError;

   PHB_ITEM pArray, pArg;
   va_list va;
   ULONG ulArgPos;
   BOOL bRelease = TRUE;

   /* Build the array from the passed arguments. */
   va_start( va, ulArgCount );
   if( ( ulSubCode == 1001 || ulSubCode == 1004 || ulSubCode == 1005 ) && ulArgCount == 1 )
   {
      pArray = va_arg( va, PHB_ITEM );

      if( HB_IS_ARRAY( pArray ) )
      {
         bRelease = FALSE;
      }
      else
      {
         pArg = pArray;
         pArray = hb_itemArrayNew( 1 );
         hb_arraySet( pArray, 1, pArg );
      }
   }
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         PHB_ITEM pVaItem = va_arg( va, PHB_ITEM );

         if ( pVaItem == NULL )
         {
            pVaItem = hb_itemNew( NULL );
         }
         hb_arraySet( pArray, ulArgPos, pVaItem );
      }
   }
   va_end( va );

   pError = hb_errRT_New_Subst( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Assign the new array to the object data item. */
   hb_vmPushSymbol( hb_dynsymGet( "_ARGS" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPush( pArray );
   hb_vmSend( 1 );

   /* Release the Array. */
   if( bRelease )
   {
      hb_itemRelease( pArray );
   }

   /* Ok, launch... */
   hb_itemRelease( hb_itemReturn( hb_errLaunchSubst( pError ) ) );
   hb_errRelease( pError );
}

USHORT HB_EXPORT hb_errRT_TERM( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOSCode, USHORT uiFlags )
{
   USHORT uiAction;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_TERMINAL, ulGenCode, ulSubCode, szDescription, szOperation, uiOSCode, uiFlags );

   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

USHORT HB_EXPORT hb_errRT_DBCMD( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   USHORT uiAction;
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, HB_ERR_SS_DBCMD, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

USHORT HB_EXPORT hb_errRT_TOOLS( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   USHORT uiAction;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   uiAction = hb_errLaunch( pError );

   hb_errRelease( pError );

   return uiAction;
}

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */

void HB_EXPORT hb_errInternal( ULONG ulIntCode, char * szText, char * szPar1, char * szPar2 )
{
   char title[64], buffer[ 256 ];
   FILE *fpError;
   BOOL bLang;

   HB_TRACE(HB_TR_DEBUG, ("hb_errInternal(%lu, %s, %s, %s)", ulIntCode, szText, szPar1, szPar2));

   bLang = ( hb_langID() != NULL );

   if( szText )
   {
      fpError = fopen( "error.log", "w" );

      if( fpError )
      {
         fclose( fpError );
         TraceLog( "error.log", szText );
      }
   }

   hb_conOutErr( hb_conNewLine(), 0 );
   sprintf( title, bLang ?
                      ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR ) :
                      "Unrecoverable error %lu: ", ulIntCode );
   hb_conOutErr( title, 0 );
   if( szText != NULL )
      sprintf( buffer, szText, szPar1, szPar2 );
   else if (bLang)
      sprintf( buffer, ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR + ulIntCode - 9000 ), szPar1, szPar2 );
   hb_conOutErr( buffer, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   hb_stackDispCall();

   #ifndef HB_RUN_AS_SERVICE
      #ifdef HB_OS_WIN_32
         MessageBox( NULL, buffer, title, MB_ICONSTOP );
      #endif
   #endif

   /* release console settings */
   hb_conRelease();

   exit( EXIT_FAILURE );
}
