
/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

/*------------------------------------------------------------------------------
* Low Level C Generic Routines
*------------------------------------------------------------------------------*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

// WINUSERAPI int WINAPI CopyAcceleratorTableA( IN HACCEL hAccelSrc, OUT LPACCEL lpAccelDst, IN int cAccelEntries);

// SYNTAX
// CopyAcceleratorTable(hAccel,aAccel) -> nEntries, or nCopied

HB_FUNC ( COPYACCELERATORTABLE )
{
   LPACCEL lpAccelDst    ;
   int iCount = 0;
   int iRet ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;
   PHB_ITEM item ;
   int i ;

   if ( ISARRAY(2) && ((iCount=hb_parinfa(2,0)) > 0 ) )
      lpAccelDst = (LPACCEL) hb_xgrab( iCount * sizeof(ACCEL) ) ;

   iRet = CopyAcceleratorTable( (HACCEL) hb_parnl( 1 ) ,
                                   (iCount==0 ? NULL : lpAccelDst ) ,
                                   iCount
                              ) ;

   if ( ( iCount > 0 ) && (iRet > 0 ) )
   {
      // read accelerator table elements into a subarrays
      // and store them into the original array elements

      aParam = hb_param( 2, HB_IT_ARRAY ) ;
      aSub = hb_itemArrayNew( 3 ) ;
      item = hb_itemNew( NULL ) ;
      for ( i = 0 ; i < iCount ; i++ )
      { 
         hb_arraySet( aSub, 1, hb_itemPutNI( item, lpAccelDst->fVirt ) );
         hb_arraySet( aSub, 2, hb_itemPutNI( item, lpAccelDst->key   ) );
         hb_arraySet( aSub, 3, hb_itemPutNI( item, lpAccelDst->cmd   ) );
         hb_arraySet( aParam, i+1, hb_arrayClone(aSub, NULL ) ) ;
      }
      hb_itemRelease(item) ;
      hb_itemRelease(aSub) ;
   }

   if ( iCount > 0 )
       hb_xfree( lpAccelDst ) ;
   hb_retni( iRet ) ;
}




