/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Graph functions
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "item.api"

typedef struct
{
  double * aValue;
  int      nlen;
  HDC      hDC;
  int      x1,y1,x2,y2;
} GRAPHINFO, DBORDERCONDINFO, *LPGRAPHINFO;

HB_FUNC( HWG_PAINTGRAPH )
{
   GRAPHINFO graph;
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   DRAWITEMSTRUCT * lpdis = (DRAWITEMSTRUCT*)hb_parnl(2);
   int i;

   graph.nlen = pArray->item.asArray.value->ulLen;
   graph.aValue = (double*) hb_xgrab( sizeof(double) * graph.nlen );

   for( i=0; i<graph.nlen; i++ )
      graph.aValue[i] = hb_itemGetND( (PHB_ITEM) (pArray->item.asArray.value->pItems + i) );

   graph.hDC = lpdis->hDC;
   graph.x1  = lpdis->rcItem->left;
   graph.y1  = lpdis->rcItem->top;
   graph.x2  = lpdis->rcItem->right;
   graph.y2  = lpdis->rcItem->bottom;

   PaintGraph( &graph );
   hb_xfree( graph.aValue );
}

void PaintGraph( LPGRAPHINFO lpGraph )
{
   double xmax = graph.aValue[1][1], ymax = graph.aValue[1][2],
          xmin = graph.aValue[1][1], ymin = graph.aValue[1][2];
   int i;

   for( i=0; i<graph.nlen; i++ )
   {
      if( xmax <
   }
}
