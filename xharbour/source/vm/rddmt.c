/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * The MT support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * if you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 *
 */

/* source/rdd/wacore.c */

void hb_rdd_wacore_rddWaInit( PHB_STACKRDD pRddInfo )
{
#ifdef HB_THREAD_SUPPORT
   pRddInfo->fMtLockInit   = FALSE;
   pRddInfo->ulCounter     = 1;
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_hb_atomic_inc( PHB_STACKRDD pRddInfo )
{
#ifdef HB_THREAD_SUPPORT
   HB_ATOMIC_INC( pRddInfo->ulCounter );
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_Lock( PHB_STACKRDD pRddInfo )
{
#if defined( HB_THREAD_SUPPORT )
   if( pRddInfo->fMtLockInit )
      HB_CRITICAL_LOCK( pRddInfo->mtxWorkArea );
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_Unlock( PHB_STACKRDD pRddInfo )
{
#if defined( HB_THREAD_SUPPORT )
   if( pRddInfo->fMtLockInit )
      HB_CRITICAL_UNLOCK( pRddInfo->mtxWorkArea );
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_LockInit( PHB_STACKRDD pRddInfo )
{
#if defined( HB_THREAD_SUPPORT )
   if( ! pRddInfo->fMtLockInit )
   {
      HB_CRITICAL_INIT( pRddInfo->mtxWorkArea );
      pRddInfo->fMtLockInit = TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_LockDestroy( PHB_STACKRDD pRddInfo )
{
#if defined( HB_THREAD_SUPPORT )
   if( pRddInfo->fMtLockInit )
   {
      HB_CRITICAL_DESTROY( pRddInfo->mtxWorkArea );
      pRddInfo->fMtLockInit = FALSE;
   }
#else
   HB_SYMBOL_UNUSED( pRddInfo );
#endif
}

void hb_rdd_wacore_rddWaShutDown( PHB_STACKRDD pRddInfo )
{
#ifdef HB_THREAD_SUPPORT
   if( ! hb_setGetWorkareasShared() )
      hb_rddCloseAll();

   if( HB_ATOMIC_DEC( pRddInfo->ulCounter ) == 0 )
   {
      if( pRddInfo->fMtLockInit )
         hb_rdd_wacore_LockDestroy( pRddInfo );

      hb_xfree( pRddInfo );
   }
#else
   hb_xfree( pRddInfo );
#endif
}

BOOL hb_rdd_wacore_rddChangeSetWorkareasShared( BOOL bPrev, BOOL bSet, PHB_STACKRDD s_pRddInfo )
{
   BOOL bOk = TRUE;

#ifdef HB_THREAD_SUPPORT
   if( bPrev == bSet )
      return TRUE;

   hb_threadWaitForIdle();
   if( ! bSet )
   {
      /* We must create HB_STACKRDD structures for each thread and save them in their respective stacks
       * And we must destroy the LOCK_AREA and set fMtLockInit to FALSE */

      PHB_STACK      p = hb_ht_stack->next; /* Start from the second thread */
      PHB_STACKRDD   pRddInfo;

      while( p )
      {
         pRddInfo                         = ( PHB_STACKRDD ) hb_xgrab( sizeof( HB_STACKRDD ) );

         pRddInfo->szDefaultRDD           = NULL;
         pRddInfo->waList                 = NULL;
         pRddInfo->waNums                 = NULL;
         pRddInfo->uiWaMax                = 0;
         pRddInfo->uiWaSpace              = 0;
         pRddInfo->uiWaNumMax             = 0;
         pRddInfo->fMtLockInit            = FALSE;
         pRddInfo->ulCounter              = 1;

         p->rdd                           = pRddInfo;
         p->set.HB_SET_WORKAREAS_SHARED   = bSet;

         p                                = p->next;
      }

      pRddInfo             = s_pRddInfo;
      pRddInfo->ulCounter  = 1;

      if( pRddInfo->fMtLockInit )
         hb_rdd_wacore_LockDestroy( pRddInfo );
   }
   else
   {
      /* We must verify that there are no open areas in any thread except the main one.
       * We must destroy HB_STACKRDD structures of each thread except the main one and
       * set each stack's HB_STACKRDD pointer to the main thread's HB_STACKRDD.
       * In the main thread, if there are open areas, create the LOCK_AREA and set fMtLockInit to TRUE. */

      HB_STACK *     p = hb_ht_stack->next; /* Start from the second thread */
      PHB_STACKRDD   pRddInfo;

      while( p )
      {
         if( p->rdd->uiWaMax > 0 )
         {
            bOk = FALSE;
            break;
         }

         p = p->next;
      }

      if( bOk )
      {
         p        = hb_ht_stack; /* Start from the first thread */
         pRddInfo = p->rdd;

         if( pRddInfo->uiWaMax > 0 )
            hb_rdd_wacore_LockInit( pRddInfo );

         p = p->next;

         while( p )
         {
            hb_xfree( p->rdd );
            p->rdd                           = pRddInfo;
            p->set.HB_SET_WORKAREAS_SHARED   = bSet;
            p                                = p->next;
         }
      }
   }
   hb_threadIdleEnd();
#else
   HB_SYMBOL_UNUSED( s_pRddInfo );
   HB_SYMBOL_UNUSED( bPrev );
   HB_SYMBOL_UNUSED( bSet );
#endif
   return bOk;
}

/* source/rdd/wafunc.c */

#define HB_GET_AREA_HANDLE( pDyn ) \
   ( pDyn ) ? ( int ) hb_dynsymAreaHandle( ( pDyn ) ) : 0

#if defined ( HB_THREAD_SUPPORT )
static PHB_DYNS s_rddAliasThGet( const char * szName, HB_STACK * pstack )
{
   /* Can NOT use HB_VM_STACK here!!!
    */
   if( pstack == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0 )
      return hb_dynsymGet( szName );
   else
   {
      char szNewName[ 270 ];
      hb_snprintf( szNewName, sizeof( szNewName ), ":TH:%d:%s", pstack->th_vm_id, szName );
      return hb_dynsymGet( szNewName );
   }
}

static PHB_DYNS s_rddAliasThFind( const char * szName, HB_STACK * pstack )
{
   /* Can NOT use HB_VM_STACK here!!!
    */
   if( pstack == &hb_stackMT || strncmp( szName, ":TH:", 4 ) == 0 )
      return hb_dynsymFindName( szName );
   else
   {
      char szNewName[ 270 ];
      hb_snprintf( szNewName, sizeof( szNewName ), ":TH:%d:%s", pstack->th_vm_id, szName );
      return hb_dynsymFindName( szNewName );
   }
}

#endif /* defined ( HB_THREAD_SUPPORT ) */

const char * hb_rddGetAliasNameTH( PHB_DYNS pSymAlias )
{
   const char * szName = hb_dynsymName( pSymAlias );

#ifdef HB_THREAD_SUPPORT
   if( strncmp( szName, ":TH:", 4 ) == 0 )
   {
      szName += 4;
      while( *szName++ != ':' )
      {
         ;
      }
   }
#endif
   return szName;
}

int hb_get_Area_Handle_From_Sym( PHB_SYMB pSymAlias )
{
#ifndef HB_THREAD_SUPPORT
   int      iArea = HB_GET_AREA_HANDLE( pSymAlias->pDynSym );
#else
   HB_THREAD_STUB
   PHB_DYNS pDyn  = hb_setGetWorkareasShared() ? pSymAlias->pDynSym : s_rddAliasThFind( ( pSymAlias )->szName, &HB_VM_STACK );
   int      iArea = HB_GET_AREA_HANDLE( pDyn );
#endif
   return iArea;
}

void hb_get_Area_Handle_From_Name( int * iArea, const char * szAlias )
{
#ifndef HB_THREAD_SUPPORT
   PHB_DYNS pDyn  = hb_dynsymFindName( szAlias );
#else
   HB_THREAD_STUB
   PHB_DYNS pDyn  = hb_setGetWorkareasShared() ? hb_dynsymFindName( szAlias ) : s_rddAliasThFind( szAlias, &HB_VM_STACK );
#endif
   *iArea = HB_GET_AREA_HANDLE( pDyn );
}

PHB_DYNS hb_get_Area_Sym( const char * szAlias )
{
   PHB_DYNS pSymAlias;

#ifdef HB_THREAD_SUPPORT
   HB_THREAD_STUB
   if( ! hb_setGetWorkareasShared() )
      pSymAlias = s_rddAliasThGet( szAlias, &HB_VM_STACK );
   else
#endif
   pSymAlias = hb_dynsymGet( szAlias );

   return pSymAlias;
}
