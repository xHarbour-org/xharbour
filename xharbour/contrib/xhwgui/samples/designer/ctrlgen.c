/*
 * HWIDE
 * C level windows functions
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

LRESULT APIENTRY ButtonSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
LRESULT APIENTRY CheckSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
LRESULT APIENTRY RadioSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
LRESULT APIENTRY EditSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
LRESULT APIENTRY DateSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
LRESULT APIENTRY ComboSubProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );

static WNDPROC wpOrigButtonProc;
static WNDPROC wpOrigCheckProc;
static WNDPROC wpOrigRadioProc;
static WNDPROC wpOrigEditProc;
static WNDPROC wpOrigDateProc;
static WNDPROC wpOrigComboProc;

HB_FUNC ( HWGI_INITBUTTONPROC )
{
   wpOrigButtonProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) ButtonSubProc );
}

HB_FUNC ( HWGI_INITCHECKPROC )
{
   wpOrigCheckProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) CheckSubProc );
}

HB_FUNC ( HWGI_INITRADIOPROC )
{
   wpOrigRadioProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) RadioSubProc );
}


HB_FUNC ( HWGI_INITEDITPROC )
{
   wpOrigEditProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) EditSubProc );
}

HB_FUNC ( HWGI_INITDATEPROC )
{
   wpOrigDateProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) DateSubProc );
}

HB_FUNC ( HWGI_INITCOMBOPROC )
{
   wpOrigComboProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) ComboSubProc );
}

LRESULT APIENTRY ButtonSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigButtonProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigButtonProc, hWnd, msg, wParam, lParam );
}

LRESULT APIENTRY CheckSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigCheckProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigCheckProc, hWnd, msg, wParam, lParam );
}

LRESULT APIENTRY RadioSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigRadioProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigRadioProc, hWnd, msg, wParam, lParam );
}

LRESULT APIENTRY EditSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigEditProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigEditProc, hWnd, msg, wParam, lParam );
}

LRESULT APIENTRY DateSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigDateProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigDateProc, hWnd, msg, wParam, lParam );
}

LRESULT APIENTRY ComboSubProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   long int res;
   PHB_DYNS pSymTest;

   if( ( pSymTest = hb_dynsymFind( "DEFBUTTONGENPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) msg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return CallWindowProc( wpOrigComboProc, hWnd, msg, wParam, lParam );
      else
         return res;
   }
   else
      return CallWindowProc( wpOrigComboProc, hWnd, msg, wParam, lParam );
}
