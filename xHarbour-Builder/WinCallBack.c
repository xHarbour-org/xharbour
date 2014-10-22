/*
 * $Id$
 */
#include <windows.h>

#define HB_NO_DEFAULT_API_MACROS

#include "hbapi.h"
#include "hbvm.h"
#include "classes.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"

//#define VirtualAlloc( p, size, request, rights ) alloc( size )
//#define VirtualFree( p, size, request ) free( p )
//#define VirtualProtect( p, size, rights, old ) TRUE

/* Patch an address of the dynamic function */
void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
{
   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );
}

/* Intel specific ?? Patch an address relative to the next instruction */
void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

   ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );
}

void * WinCallBackPointer( PHB_SYMB pPrgFunc )
{

   BYTE *bCode = (BYTE *) VirtualAlloc( NULL, 93, MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE );
   DWORD dOldProtection;

   //OutputDebugValues( "Virtual allocated: %p\n", bCode );

   //_MainWindowProc@16:
   bCode[  0] = 0x55;//              push      ebp

   bCode[  1] = 0x89;
   bCode[  2] = 0xE5;//              mov       ebp,esp

   //    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
   bCode[  3] = 0x68;
   bCode[  4] = 0x00;
   bCode[  5] = 0x00;
   bCode[  6] = 0x00;
   bCode[  7] = 0x00;//              push      00000000

   bCode[  8] = 0xE8;
   bCode[  9] = 0x00;
   bCode[ 10] = 0x00;
   bCode[ 11] = 0x00;
   bCode[ 12] = 0x00;//              call      _hb_vmPushSymbol

   bCode[ 13] = 0x83;
   bCode[ 14] = 0xC4;
   bCode[ 15] = 0x04;//              add       esp,+4

   //    hb_vmPushNil();
   bCode[ 16] = 0xE8;
   bCode[ 17] = 0x00;
   bCode[ 18] = 0x00;
   bCode[ 19] = 0x00;
   bCode[ 20] = 0x00;//              call      _hb_vmPushNil

   //    hb_vmPushLong( (LONG) hWnd );
   bCode[ 21] = 0x8B;
   bCode[ 22] = 0x45;
   bCode[ 23] = 0x08;//              mov       eax,dword ptr [ebp+8]
   bCode[ 24] = 0x50;//              push      eax

   bCode[ 25] = 0xE8;
   bCode[ 26] = 0x00;
   bCode[ 27] = 0x00;
   bCode[ 28] = 0x00;
   bCode[ 29] = 0x00;//              call      _hb_vmPushLong

   bCode[ 30] = 0x83;
   bCode[ 31] = 0xC4;
   bCode[ 32] = 0x04;//              add       esp,+4

   //   hb_vmPushLong( (LONG) message );
   bCode[ 33] = 0x8B;
   bCode[ 34] = 0x45;
   bCode[ 35] = 0x0C;//              mov       eax,dword ptr [ebp+C]

   bCode[ 36] = 0x50;//              push      eax

   bCode[ 37] = 0xE8;
   bCode[ 38] = 0x00;
   bCode[ 39] = 0x00;
   bCode[ 40] = 0x00;
   bCode[ 41] = 0x00;//              call      _hb_vmPushLong

   bCode[ 42] = 0x83;
   bCode[ 43] = 0xC4;
   bCode[ 44] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) wParam );
   bCode[ 45] = 0x8B;
   bCode[ 46] = 0x45;
   bCode[ 47] = 0x10;//              mov       eax,dword ptr [ebp+10]

   bCode[ 48] = 0x50;//              push      eax

   bCode[ 49] = 0xE8;
   bCode[ 50] = 0x00;
   bCode[ 51] = 0x00;
   bCode[ 52] = 0x00;
   bCode[ 53] = 0x00;//              call      _hb_vmPushLong

   bCode[ 54] = 0x83;
   bCode[ 55] = 0xC4;
   bCode[ 56] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) lParam );
   bCode[ 57] = 0x8B;
   bCode[ 58] = 0x45;
   bCode[ 59] = 0x14;//              mov       eax,dword ptr [ebp+14]

   bCode[ 60] = 0x50;//              push      eax

   bCode[ 61] = 0xE8;
   bCode[ 62] = 0x00;
   bCode[ 63] = 0x00;
   bCode[ 64] = 0x00;
   bCode[ 65] = 0x00;//              call      _hb_vmPushLong

   bCode[ 66] = 0x83;
   bCode[ 67] = 0xC4;
   bCode[ 68] = 0x04;//              add       esp,+4

   //    hb_vmDo( 4 );
   bCode[ 69] = 0x6A;
   bCode[ 70] = 0x04;//              push      +4

   bCode[ 71] = 0xE8;
   bCode[ 72] = 0x00;
   bCode[ 73] = 0x00;
   bCode[ 74] = 0x00;
   bCode[ 75] = 0x00;//              call      _hb_vmDo

   bCode[ 76] = 0x83;
   bCode[ 77] = 0xC4;
   bCode[ 78] = 0x04;//              add       esp,+4

   //    return hb_parnl( -1 ) );
   bCode[ 79] = 0x6A;
   bCode[ 80] = 0xFF;//              push      -FF (-1)

   bCode[ 81] = 0xE8;
   bCode[ 82] = 0x00;
   bCode[ 83] = 0x00;
   bCode[ 84] = 0x00;
   bCode[ 85] = 0x00;//              call      _hb_parnl

   bCode[ 86] = 0x83;
   bCode[ 87] = 0xC4;
   bCode[ 88] = 0x04;//              add       esp,+4

   bCode[ 89] = 0x5D;//              pop       ebp

   bCode[ 90] = 0xC2;
   bCode[ 91] = 0x10;
   bCode[ 92] = 0x00;//              ret       10

   hb_hrbAsmPatch( bCode, 4, pPrgFunc );

   hb_hrbAsmPatchRelative( bCode, 9, hb_vmPushSymbol, 13 );
   hb_hrbAsmPatchRelative( bCode, 17, hb_vmPushNil, 21 );
   hb_hrbAsmPatchRelative( bCode, 26, hb_vmPushLong, 30 );
   hb_hrbAsmPatchRelative( bCode, 38, hb_vmPushLong, 42 );
   hb_hrbAsmPatchRelative( bCode, 50, hb_vmPushLong, 54 );
   hb_hrbAsmPatchRelative( bCode, 62, hb_vmPushLong, 66 );
   hb_hrbAsmPatchRelative( bCode, 72, hb_vmDo, 76 );
   hb_hrbAsmPatchRelative( bCode, 82, hb_parnl, 86 );

   if( VirtualProtect( (LPVOID) bCode, 93, PAGE_EXECUTE, &dOldProtection ) )
   {
      return (void *) bCode;
   }
   else
   {
      return NULL;
   }
}

void * WinCallBackMethodPointer( PHB_SYMB pPrgFunc, PHB_ITEM pSelf )
{
   BYTE *bCode = (BYTE*) VirtualAlloc( NULL, 101, MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE );
   DWORD dOldProtection;

   //OutputDebugValues( "Virtual allocated: %p\n", bCode );

   //_MainWindowProc@16:
   bCode[  0] = 0x55;//              push      ebp

   bCode[  1] = 0x89;
   bCode[  2] = 0xE5;//              mov       ebp,esp

   //    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
   bCode[  3] = 0x68;
   bCode[  4] = 0x00;
   bCode[  5] = 0x00;
   bCode[  6] = 0x00;
   bCode[  7] = 0x00;//              push      00000000

   bCode[  8] = 0xE8;
   bCode[  9] = 0x00;
   bCode[ 10] = 0x00;
   bCode[ 11] = 0x00;
   bCode[ 12] = 0x00;//              call      _hb_vmPushSymbol

   bCode[ 13] = 0x83;
   bCode[ 14] = 0xC4;
   bCode[ 15] = 0x04;//              add       esp,+4

   //    hb_vmPushBasearray( 0x00000000 );
   bCode[ 16] = 0x68;
   bCode[ 17] = 0x00;
   bCode[ 18] = 0x00;
   bCode[ 19] = 0x00;
   bCode[ 20] = 0x00;//              push      00000000

   bCode[ 21] = 0xE8;
   bCode[ 22] = 0x00;
   bCode[ 23] = 0x00;
   bCode[ 24] = 0x00;
   bCode[ 25] = 0x00;//              call      _hb_vmPushBaseArray

   bCode[ 26] = 0x83;
   bCode[ 27] = 0xC4;
   bCode[ 28] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) hWnd );
   bCode[ 29] = 0x8B;
   bCode[ 30] = 0x45;
   bCode[ 31] = 0x08;//              mov       eax,dword ptr [ebp+8]

   bCode[ 32] = 0x50;//              push      eax

   bCode[ 33] = 0xE8;
   bCode[ 34] = 0x00;
   bCode[ 35] = 0x00;
   bCode[ 36] = 0x00;
   bCode[ 37] = 0x00;//              call      _hb_vmPushLong

   bCode[ 38] = 0x83;
   bCode[ 39] = 0xC4;
   bCode[ 40] = 0x04;//              add       esp,+4

   //   hb_vmPushLong( (LONG) message );
   bCode[ 41] = 0x8B;
   bCode[ 42] = 0x45;
   bCode[ 43] = 0x0C;//              mov       eax,dword ptr [ebp+C]
   bCode[ 44] = 0x50;//              push      eax

   bCode[ 45] = 0xE8;
   bCode[ 46] = 0x00;
   bCode[ 47] = 0x00;
   bCode[ 48] = 0x00;
   bCode[ 49] = 0x00;//              call      _hb_vmPushLong

   bCode[ 50] = 0x83;
   bCode[ 51] = 0xC4;
   bCode[ 52] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) wParam );
   bCode[ 53] = 0x8B;
   bCode[ 54] = 0x45;
   bCode[ 55] = 0x10;//              mov       eax,dword ptr [ebp+10]
   bCode[ 56] = 0x50;//              push      eax

   bCode[ 57] = 0xE8;
   bCode[ 58] = 0x00;
   bCode[ 59] = 0x00;
   bCode[ 60] = 0x00;
   bCode[ 61] = 0x00;//              call      _hb_vmPushLong

   bCode[ 62] = 0x83;
   bCode[ 63] = 0xC4;
   bCode[ 64] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) lParam );
   bCode[ 65] = 0x8B;
   bCode[ 66] = 0x45;
   bCode[ 67] = 0x14;//              mov       eax,dword ptr [ebp+14]
   bCode[ 68] = 0x50;//              push      eax

   bCode[ 69] = 0xE8;
   bCode[ 70] = 0x00;
   bCode[ 71] = 0x00;
   bCode[ 72] = 0x00;
   bCode[ 73] = 0x00;//              call      _hb_vmPushLong

   bCode[ 74] = 0x83;
   bCode[ 75] = 0xC4;
   bCode[ 76] = 0x04;//              add       esp,+4

   //    hb_vmDo( 4 );
   bCode[ 77] = 0x6A;
   bCode[ 78] = 0x04;//              push      +4

   bCode[ 79] = 0xE8;
   bCode[ 80] = 0x00;
   bCode[ 81] = 0x00;
   bCode[ 82] = 0x00;
   bCode[ 83] = 0x00;//              call      _hb_vmDo

   bCode[ 84] = 0x83;
   bCode[ 85] = 0xC4;
   bCode[ 86] = 0x04;//              add       esp,+4

   //    return hb_parnl( -1 ) );
   bCode[ 87] = 0x6A;
   bCode[ 88] = 0xFF;//              push      -FF (-1)

   bCode[ 89] = 0xE8;
   bCode[ 90] = 0x00;
   bCode[ 91] = 0x00;
   bCode[ 92] = 0x00;
   bCode[ 93] = 0x00;//              call      _hb_parnl

   bCode[ 94] = 0x83;
   bCode[ 95] = 0xC4;
   bCode[ 96] = 0x04;//              add       esp,+4

   bCode[ 97] = 0x5D;//              pop       ebp

   bCode[ 98] = 0xC2;
   bCode[ 99] = 0x10;
   bCode[100] = 0x00;//              ret       10

   hb_hrbAsmPatch( bCode, 4, pPrgFunc );

   hb_hrbAsmPatch( bCode, 17, pSelf->item.asArray.value );

   hb_hrbAsmPatchRelative( bCode,  9, hb_vmPushSymbol, 13 );
   hb_hrbAsmPatchRelative( bCode, 22, hb_vmPushBaseArray, 26 );
   hb_hrbAsmPatchRelative( bCode, 34, hb_vmPushLong, 38 );
   hb_hrbAsmPatchRelative( bCode, 46, hb_vmPushLong, 50 );
   hb_hrbAsmPatchRelative( bCode, 58, hb_vmPushLong, 62 );
   hb_hrbAsmPatchRelative( bCode, 70, hb_vmPushLong, 74 );
   hb_hrbAsmPatchRelative( bCode, 80, hb_vmSend, 84 );
   hb_hrbAsmPatchRelative( bCode, 90, hb_parnl, 94 );

   if( VirtualProtect( (LPVOID) bCode, 101, PAGE_EXECUTE, &dOldProtection ) )
   {
      return (void *) bCode;
   }
   else
   {
      return NULL;
   }
}

HB_FUNC( WINCALLBACKPOINTER )
{
   PHB_SYMB pPrgFunc = (PHB_SYMB) hb_parptr( 1 );
   PHB_ITEM pSelf = hb_param( 2, HB_IT_ANY );

   if( pSelf && HB_IS_OBJECT( pSelf ) )
   {
      //TraceLog( NULL, "Class: %s Base: %p\n", hb_objGetClsName( pSelf ), pSelf->item.asArray.value );
      //hb_gcLock( pSelf->item.asArray.value );
      hb_retnl( (LONG) WinCallBackMethodPointer( pPrgFunc, pSelf ) );
   }
   else
   {
      hb_retnl( (LONG) WinCallBackPointer( pPrgFunc ) );
   }
}

HB_FUNC( FREECALLBACKPOINTER )
{
   void *pCallback = (void *) hb_parnl( 1 );

   if( pCallback )
   {
      PHB_ITEM pSelf = hb_param( 2, HB_IT_ANY );

      VirtualFree( (LPVOID) pCallback, 0, MEM_DECOMMIT | MEM_RELEASE );
      hb_retl( TRUE );


      if( pSelf && HB_IS_OBJECT( pSelf ) )
      {
         //hb_gcUnlock( pSelf->item.asArray.value );
      }

      return;
   }

   hb_retl( FALSE );
}

HOOKPROC WinHookPointer( PHB_SYMB pPrgFunc )
{
   BYTE *bCode = (BYTE*) VirtualAlloc( NULL, 81, MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE );
   DWORD dOldProtection;

   //_MainHookProc@12:
   bCode[  0] = 0x55;//              push      ebp

   bCode[  1] = 0x89;
   bCode[  2] = 0xE5;//              mov       ebp,esp

   //    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
   bCode[  3] = 0x68;
   bCode[  4] = 0x00;
   bCode[  5] = 0x00;
   bCode[  6] = 0x00;
   bCode[  7] = 0x00;//              push      00000000

   bCode[  8] = 0xE8;
   bCode[  9] = 0x00;
   bCode[ 10] = 0x00;
   bCode[ 11] = 0x00;
   bCode[ 12] = 0x00;//              call      _hb_vmPushSymbol

   bCode[ 13] = 0x83;
   bCode[ 14] = 0xC4;
   bCode[ 15] = 0x04;//              add       esp,+4

   //    hb_vmPushNil();
   bCode[ 16] = 0xE8;
   bCode[ 17] = 0x00;
   bCode[ 18] = 0x00;
   bCode[ 19] = 0x00;
   bCode[ 20] = 0x00;//              call      _hb_vmPushNil

   //    hb_vmPushInteger( nCode );
   bCode[ 21] = 0x8B;
   bCode[ 22] = 0x45;
   bCode[ 23] = 0x08;//              mov       eax,dword ptr [ebp+8]
   bCode[ 24] = 0x50;//              push      eax

   bCode[ 25] = 0xE8;
   bCode[ 26] = 0x00;
   bCode[ 27] = 0x00;
   bCode[ 28] = 0x00;
   bCode[ 29] = 0x00;//              call      _hb_vmPushInteger

   bCode[ 30] = 0x83;
   bCode[ 31] = 0xC4;
   bCode[ 32] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) wParam );
   bCode[ 33] = 0x8B;
   bCode[ 34] = 0x45;
   bCode[ 35] = 0x0C;//              mov       eax,dword ptr [ebp+C]

   bCode[ 36] = 0x50;//              push      eax

   bCode[ 37] = 0xE8;
   bCode[ 38] = 0x00;
   bCode[ 39] = 0x00;
   bCode[ 40] = 0x00;
   bCode[ 41] = 0x00;//              call      _hb_vmPushLong

   bCode[ 42] = 0x83;
   bCode[ 43] = 0xC4;
   bCode[ 44] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) lParam );
   bCode[ 45] = 0x8B;
   bCode[ 46] = 0x45;
   bCode[ 47] = 0x10;//              mov       eax,dword ptr [ebp+10]

   bCode[ 48] = 0x50;//              push      eax

   bCode[ 49] = 0xE8;
   bCode[ 50] = 0x00;
   bCode[ 51] = 0x00;
   bCode[ 52] = 0x00;
   bCode[ 53] = 0x00;//              call      _hb_vmPushLong

   bCode[ 54] = 0x83;
   bCode[ 55] = 0xC4;
   bCode[ 56] = 0x04;//              add       esp,+4

   //    hb_vmDo( 3 );
   bCode[ 57] = 0x6A;
   bCode[ 58] = 0x03;//              push      +3

   bCode[ 59] = 0xE8;
   bCode[ 60] = 0x00;
   bCode[ 61] = 0x00;
   bCode[ 62] = 0x00;
   bCode[ 63] = 0x00;//              call      _hb_vmDo

   bCode[ 64] = 0x83;
   bCode[ 65] = 0xC4;
   bCode[ 66] = 0x04;//              add       esp,+4

   //    return hb_parnl( -1 ) );
   bCode[ 67] = 0x6A;
   bCode[ 68] = 0xFF;//              push      -FF (-1)

   bCode[ 69] = 0xE8;
   bCode[ 70] = 0x00;
   bCode[ 71] = 0x00;
   bCode[ 72] = 0x00;
   bCode[ 73] = 0x00;//              call      _hb_parnl

   bCode[ 74] = 0x83;
   bCode[ 75] = 0xC4;
   bCode[ 76] = 0x04;//              add       esp,+4

   bCode[ 77] = 0x5D;//              pop       ebp

   bCode[ 78] = 0xC2;
   bCode[ 79] = 0x0C;
   bCode[ 80] = 0x00;//              ret       C

   hb_hrbAsmPatch( bCode, 4, pPrgFunc );

   hb_hrbAsmPatchRelative( bCode,  9, hb_vmPushSymbol, 13 );
   hb_hrbAsmPatchRelative( bCode, 17, hb_vmPushNil, 21 );
   hb_hrbAsmPatchRelative( bCode, 26, hb_vmPushLong, 30 );
   hb_hrbAsmPatchRelative( bCode, 38, hb_vmPushLong, 42 );
   hb_hrbAsmPatchRelative( bCode, 50, hb_vmPushLong, 54 );
   hb_hrbAsmPatchRelative( bCode, 60, hb_vmDo, 64 );
   hb_hrbAsmPatchRelative( bCode, 70, hb_parnl, 74 );

   if( VirtualProtect( (LPVOID) bCode, 81, PAGE_EXECUTE, &dOldProtection ) )
   {
      return (HOOKPROC) bCode;
   }
   else
   {
      return (HOOKPROC) 0;
   }
}

HOOKPROC WinHookMethodPointer( PHB_SYMB pPrgFunc, PHB_ITEM pSelf )
{
   BYTE *bCode = (BYTE*) VirtualAlloc( NULL, 99, MEM_RESERVE | MEM_COMMIT, PAGE_EXECUTE_READWRITE );
   DWORD dOldProtection;

   //_MainWindowProc@16:
   bCode[  0] = 0x55;//              push      ebp

   bCode[  1] = 0x89;
   bCode[  2] = 0xE5;//              mov       ebp,esp

   //    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
   bCode[  3] = 0x68;
   bCode[  4] = 0x00;
   bCode[  5] = 0x00;
   bCode[  6] = 0x00;
   bCode[  7] = 0x00;//              push      00000000

   bCode[  8] = 0xE8;
   bCode[  9] = 0x00;
   bCode[ 10] = 0x00;
   bCode[ 11] = 0x00;
   bCode[ 12] = 0x00;//              call      _hb_vmPushSymbol

   bCode[ 13] = 0x83;
   bCode[ 14] = 0xC4;
   bCode[ 15] = 0x04;//              add       esp,+4

   //    hb_vmPushBasearray( 0x00000000 );
   bCode[ 16] = 0x68;
   bCode[ 17] = 0x00;
   bCode[ 18] = 0x00;
   bCode[ 19] = 0x00;
   bCode[ 20] = 0x00;//              push      00000000

   bCode[ 21] = 0xE8;
   bCode[ 22] = 0x00;
   bCode[ 23] = 0x00;
   bCode[ 24] = 0x00;
   bCode[ 25] = 0x00;//              call      _hb_vmPushBaseArray

   bCode[ 26] = 0x83;
   bCode[ 27] = 0xC4;
   bCode[ 28] = 0x04;//              add       esp,+4

   //    hb_vmPushInteger( nCode );
   bCode[ 29] = 0x8B;
   bCode[ 30] = 0x45;
   bCode[ 31] = 0x08;//              mov       eax,dword ptr [ebp+8]
   bCode[ 32] = 0x50;//              push      eax

   bCode[ 33] = 0xE8;
   bCode[ 34] = 0x00;
   bCode[ 35] = 0x00;
   bCode[ 36] = 0x00;
   bCode[ 37] = 0x00;//              call      _hb_vmPushInteger

   bCode[ 38] = 0x83;
   bCode[ 39] = 0xC4;
   bCode[ 40] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) wParam );
   bCode[ 41] = 0x8B;
   bCode[ 42] = 0x45;
   bCode[ 43] = 0x0C;//              mov       eax,dword ptr [ebp+C]
   bCode[ 44] = 0x50;//              push      eax

   bCode[ 45] = 0xE8;
   bCode[ 46] = 0x00;
   bCode[ 47] = 0x00;
   bCode[ 48] = 0x00;
   bCode[ 49] = 0x00;//              call      _hb_vmPushLong

   bCode[ 50] = 0x83;
   bCode[ 51] = 0xC4;
   bCode[ 52] = 0x04;//              add       esp,+4

   //    hb_vmPushLong( (LONG) lParam );
   bCode[ 53] = 0x8B;
   bCode[ 54] = 0x45;
   bCode[ 55] = 0x10;//              mov       eax,dword ptr [ebp+10]
   bCode[ 56] = 0x50;//              push      eax

   bCode[ 57] = 0xE8;
   bCode[ 58] = 0x00;
   bCode[ 59] = 0x00;
   bCode[ 60] = 0x00;
   bCode[ 61] = 0x00;//              call      _hb_vmPushLong

   bCode[ 62] = 0x83;
   bCode[ 63] = 0xC4;
   bCode[ 64] = 0x04;//              add       esp,+4

   //    hb_vmSend( 3 );
   bCode[ 65] = 0x6A;
   bCode[ 66] = 0x03;//              push      +3

   bCode[ 67] = 0xE8;
   bCode[ 68] = 0x00;
   bCode[ 69] = 0x00;
   bCode[ 70] = 0x00;
   bCode[ 71] = 0x00;//              call      _hb_vmSend

   bCode[ 72] = 0x83;
   bCode[ 73] = 0xC4;
   bCode[ 74] = 0x04;//              add       esp,+4

   //    return hb_parnl( -1 ) );
   bCode[ 75] = 0x6A;
   bCode[ 76] = 0xFF;//              push      -FF (-1)

   bCode[ 77] = 0xE8;
   bCode[ 78] = 0x00;
   bCode[ 79] = 0x00;
   bCode[ 80] = 0x00;
   bCode[ 81] = 0x00;//              call      _hb_parnl

   bCode[ 82] = 0x83;
   bCode[ 83] = 0xC4;
   bCode[ 84] = 0x04;//              add       esp,+4

   bCode[ 85] = 0x5D;//              pop       ebp

   bCode[ 86] = 0xC2;
   bCode[ 87] = 0x0C;
   bCode[ 88] = 0x00;//              ret       C

   hb_hrbAsmPatch( bCode, 4, pPrgFunc );

   hb_hrbAsmPatch( bCode, 17, pSelf->item.asArray.value );

   hb_hrbAsmPatchRelative( bCode,  9, hb_vmPushSymbol, 13 );
   hb_hrbAsmPatchRelative( bCode, 22, hb_vmPushBaseArray, 26 );
   hb_hrbAsmPatchRelative( bCode, 34, hb_vmPushInteger, 38 );
   hb_hrbAsmPatchRelative( bCode, 46, hb_vmPushLong, 50 );
   hb_hrbAsmPatchRelative( bCode, 58, hb_vmPushLong, 62 );
   hb_hrbAsmPatchRelative( bCode, 68, hb_vmSend, 72 );
   hb_hrbAsmPatchRelative( bCode, 78, hb_parnl, 82 );

   if( VirtualProtect( (LPVOID) bCode, 99, PAGE_EXECUTE, &dOldProtection ) )
   {
      return (HOOKPROC) bCode;
   }
   else
   {
      return (HOOKPROC) 0;
   }
}

/*
HHOOK SetWindowsHookEx(          int idHook,
    HOOKPROC lpfn,
    HINSTANCE hMod,
    DWORD dwThreadId
);
*/

HB_FUNC( SETWINDOWSHOOKEX )
{
   PHB_SYMB pPrgFunc = (PHB_SYMB) hb_parptr( 2 );
   PHB_ITEM pSelf = hb_param( 5, HB_IT_ANY );

   if( pSelf && HB_IS_OBJECT( pSelf ) )
   {
      hb_retnl( (LONG) SetWindowsHookEx( hb_parni( 1 ), WinHookMethodPointer( pPrgFunc, pSelf ), (HINSTANCE) hb_parnl( 3 ), (DWORD) hb_parnl( 4 ) ) );
   }
   else
   {
      hb_retnl( (LONG) SetWindowsHookEx( hb_parni( 1 ), WinHookPointer( pPrgFunc ), (HINSTANCE) hb_parnl( 3 ), (DWORD) hb_parnl( 4 ) ) );
   }
}

HB_FUNC( UNHOOKWINDOWSHOOKEX )
{
   HHOOK pHook = (HHOOK) hb_parnl( 1 );

   if( pHook )
   {
      PHB_ITEM pSelf = hb_param( 2, HB_IT_ANY );

      if( pSelf && HB_IS_OBJECT( pSelf ) )
      {
      }

      // TODO, free OUR callback pointer!
      hb_retl( UnhookWindowsHookEx( pHook ) );
   }

   hb_retl( FALSE );
}
