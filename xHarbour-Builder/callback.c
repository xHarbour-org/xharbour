#include <windows.h>

#include "hbapi.h"
#include "hbvm.h"

LRESULT CALLBACK WinCallBackPointer( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
   hb_vmPushNil();
   hb_vmPushLong( (LONG) hWnd );
   hb_vmPushLong( (LONG) message );
   hb_vmPushLong( (LONG) wParam );
   hb_vmPushLong( (LONG) lParam );

   hb_vmDo( 4 );

   return hb_parnl( -1 );
}

LRESULT CALLBACK WinCallBackMethodPointer( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
   hb_vmPushBaseArray( (PHB_BASEARRAY) 0x40000000);
   hb_vmPushLong( (LONG) hWnd );
   hb_vmPushLong( (LONG) message );
   hb_vmPushLong( (LONG) wParam );
   hb_vmPushLong( (LONG) lParam );

   hb_vmSend( 4 );

   return hb_parnl( -1 );
}
