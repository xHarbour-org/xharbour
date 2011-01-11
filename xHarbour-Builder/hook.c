#include <windows.h>

#include "hbapi.h"
#include "hbvm.h"

LRESULT CALLBACK HookProc( int nCode, WPARAM wParam, LPARAM lParam )
{
   hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
   hb_vmPushNil();
   hb_vmPushInteger( nCode );
   hb_vmPushLong( (LONG) wParam );
   hb_vmPushLong( (LONG) lParam );

   hb_vmDo( 3 );

   return hb_parnl( -1 );
}

LRESULT CALLBACK HookProcMethod( int nCode, WPARAM wParam, LPARAM lParam )
{
   hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
   hb_vmPushBaseArray( (PHB_BASEARRAY) 0x40000000);
   hb_vmPushInteger( nCode );
   hb_vmPushLong( (LONG) wParam );
   hb_vmPushLong( (LONG) lParam );

   hb_vmSend( 3 );

   return hb_parnl( -1 );
}
