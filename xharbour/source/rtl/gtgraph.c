/*
 * $Id: gtgraph.c,v 1.4 2004/02/10 13:16:18 andijahja Exp $
 */

/*
 * XHarbour Project source code:
 * GT - Graphical GT enabled functions
 *
 * Copyright 2004-x Giancarlo Niccolai <antispam at niccolai dot ws>
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

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbapierr.h"

static HB_GT_GCOLOR *s_paramToColor( PHB_ITEM pColor, char *funcname )
{
   HB_GT_COLDEF  *color; // = NULL;
   static HB_GT_GCOLOR lcolor;

   if ( HB_IS_STRING( pColor ) )
   {
      color = hb_gt_gcolorFromString( hb_itemGetCPtr( pColor ) );
      if ( color == NULL )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong color code", funcname, 0 );
      }
   }
#ifndef HB_LONG_LONG_OFF
   else if ( HB_IS_LONGLONG( pColor ) )
   {
      ULONGLONG col  = (ULONGLONG) hb_itemGetNLL( pColor );
#else
   else if ( HB_IS_LONG( pColor ) )
   {
      ULONG col  = (ULONG) hb_itemGetNL( pColor );
#endif
      lcolor.usAlpha = ( (col & 0xFFFF000000000000) >> 48 );
      lcolor.usRed   = ( (col & 0x0000FFFF00000000) >> 32 );
      lcolor.usGreen = ( (col & 0x00000000FFFF0000) >> 16 );
      lcolor.usBlue  = ( (col & 0x000000000000FFFF) );
      return &lcolor;
   }
   else
   {
      color = hb_gt_gcolorFromString( "W" );
   }

   return &(color->color);
}

/**********************************************************************
* Utility functions
***********************************************************************/

HB_FUNC( GTRGB )
{
   PHB_ITEM pRed   = hb_param(1, HB_IT_NUMERIC );
   PHB_ITEM pGreen = hb_param(2, HB_IT_NUMERIC );
   PHB_ITEM pBlue  = hb_param(3, HB_IT_NUMERIC );
   PHB_ITEM pAlpha = hb_param(4, HB_IT_NUMERIC );
#ifndef HB_LONG_LONG_OFF
   ULONGLONG color = (ULONGLONG) 0;
#else
   ULONG color = (ULONG) 0;
#endif
   BOOL lCorrect = TRUE;

   if ( pRed )
   {
      if ( HB_IS_DOUBLE( pRed ) )
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF * hb_itemGetND( pRed  ) ) ) << 32;
#else
         color |= ((ULONG)(0xFFFF * hb_itemGetND( pRed  ) ) ) << 32;
#endif
      }
      else
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF & hb_itemGetNL( pRed ) ) ) << 32;
#else
         color |= ((ULONG)(0xFFFF & hb_itemGetNL( pRed ) ) ) << 32;
#endif
      }
   }
   else
   {
      lCorrect = FALSE;
   }

   if ( lCorrect && pGreen )
   {
     if ( HB_IS_DOUBLE( pGreen ) )
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF * hb_itemGetND( pGreen  ) ) ) << 16;
#else
         color |= ((ULONG)(0xFFFF * hb_itemGetND( pGreen  ) ) ) << 16;
#endif
      }
      else
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF & hb_itemGetNL( pGreen ) ) ) << 16;
#else
         color |= ((ULONG)(0xFFFF & hb_itemGetNL( pGreen ) ) ) << 16;
#endif
      }
   }
   else
   {
      lCorrect = FALSE;
   }

   if ( lCorrect && pBlue )
   {
     if ( HB_IS_DOUBLE( pGreen ) )
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF * hb_itemGetND( pBlue  ) ) );
#else
         color |= ((ULONG)(0xFFFF * hb_itemGetND( pBlue  ) ) );
#endif
      }
      else
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF & hb_itemGetNL( pBlue ) ) );
#else
         color |= ((ULONG)(0xFFFF & hb_itemGetNL( pBlue ) ) );
#endif
      }
   }
   else
   {
      lCorrect = FALSE;
   }

   if ( lCorrect && pAlpha )
   {
     if ( HB_IS_DOUBLE( pAlpha ) )
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF * hb_itemGetND( pBlue  ) ) ) << 48;
#else
         color |= ((ULONG)(0xFFFF * hb_itemGetND( pBlue  ) ) ) << 48;
#endif
      }
      else
      {
#ifndef HB_LONG_LONG_OFF
         color |= ((ULONGLONG)(0xFFFF & hb_itemGetNL( pBlue ) ) ) << 48;
#else
         color |= ((ULONG)(0xFFFF & hb_itemGetNL( pBlue ) ) ) << 48;
#endif
      }
   }
   else
   {
#ifndef HB_LONG_LONG_OFF
      color |= ((ULONGLONG)0xFFFF) << 48;
#else
      color |= ((ULONG)0xFFFF) << 48;
#endif
   }

#ifndef HB_LONG_LONG_OFF
   hb_retnll( color );
#else
   hb_retnl( color );
#endif
}

/**********************************************************************
* Graphic functions
***********************************************************************/

HB_FUNC( GTPOINT )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 3, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTPOINT", 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTPOINT" );

      if ( color != NULL )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_POINT;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         hb_gtAddGobject( gobj );
      }

   }
}

HB_FUNC( GTLINE )
{
   PHB_ITEM pX1 = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY1 = hb_param( 2, HB_IT_NUMERIC );
   /* For lines, width and height represent X2, Y2 */
   PHB_ITEM pX2 = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pY2 = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 5, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX1 == NULL || pY1 == NULL || pX2 == NULL || pY2 == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTLINE", 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTLINE" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_LINE;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX1 );
         gobj->y = hb_itemGetNI( pY1 );
         gobj->width = hb_itemGetNI( pX2 );
         gobj->height = hb_itemGetNI( pY2 );
         hb_gtAddGobject( gobj );
      }
   }
}

HB_FUNC( GTSQUARE )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pWidth = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pHeight = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 5, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL || pWidth == NULL || pHeight == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTSQUARE", 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTSQUARE" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_SQUARE;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         gobj->width = hb_itemGetNI( pWidth );
         gobj->height = hb_itemGetNI( pHeight );
         hb_gtAddGobject( gobj );
      }
   }
}

HB_FUNC( GTRECTANGLE )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pWidth = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pHeight = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 5, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL || pWidth == NULL || pHeight == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTRECTANGLE", 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTRECTANGLE" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_RECTANGLE;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         gobj->width = hb_itemGetNI( pWidth );
         gobj->height = hb_itemGetNI( pHeight );
         hb_gtAddGobject( gobj );
      }
   }
}


HB_FUNC( GTCIRCLE )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pWidth = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pHeight = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 5, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL || pWidth == NULL || pHeight == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTCIRCLE", 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTCIRCLE" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_CIRCLE;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         gobj->width = hb_itemGetNI( pWidth );
         gobj->height = hb_itemGetNI( pHeight );
         hb_gtAddGobject( gobj );
      }
   }
}

HB_FUNC( GTDISK )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pWidth = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pHeight = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pColor = hb_param( 5, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL || pWidth == NULL || pHeight == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTDISK", 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTDISK" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_DISK;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         gobj->width = hb_itemGetNI( pWidth );
         gobj->height = hb_itemGetNI( pHeight );
         hb_gtAddGobject( gobj );
      }
   }
}

HB_FUNC( GTTEXT )
{
   PHB_ITEM pX = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pY = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pText = hb_param( 3, HB_IT_STRING );
   PHB_ITEM pColor = hb_param( 4, HB_IT_ANY );
   HB_GT_GOBJECT *gobj;

   if ( pX == NULL || pY == NULL || pText == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "GTTEXT", 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ) );
   }
   else
   {
      HB_GT_GCOLOR *color = s_paramToColor( pColor, "GTTEXT" );

      if ( color )
      {
         gobj = (HB_GT_GOBJECT *) hb_xgrab( sizeof( HB_GT_GOBJECT ) );
         gobj->type = GTO_TEXT;
         gobj->color = *color;
         gobj->x = hb_itemGetNI( pX );
         gobj->y = hb_itemGetNI( pY );
         gobj->data_len = (USHORT) hb_itemGetCLen( pText );
         gobj->data = (char*) hb_xgrab( gobj->data_len );
         memcpy( gobj->data, hb_itemGetCPtr( pText ), gobj->data_len );
         hb_gtAddGobject( gobj );
      }
   }
}

