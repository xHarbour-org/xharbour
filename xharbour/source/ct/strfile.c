/*
 * $Id: strfile.c,v 1.1 2004/11/28 17:59:27 ptsarenko Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Functions:
 * SETFCREATE(), CSETSAFETY(), STRFILE(), FILESTR(), SCREENFILE()
 * SCREENFILE(), FILESCREEN()
 *                                             
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
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
#include "hbapifs.h"
#include "hbapigt.h"


static int s_iFileAttr = HB_FA_NORMAL;
static BOOL s_bSafety = 0;

void ct_setfcreate (int iFileAttr)
{
   HB_TRACE(HB_TR_DEBUG, ("ct_setfcreate(%i)", iFileAttr));
   s_iFileAttr = iFileAttr;
}

int ct_getfcreate (void)
{
   HB_TRACE(HB_TR_DEBUG, ("ct_getfcreate()"));
   return (s_iFileAttr);
}

HB_FUNC (SETFCREATE)
{

   hb_retni (ct_getfcreate());

   if (ISNUM (1))
   {
      ct_setfcreate(hb_parni (1));
   }

   return;

}

void ct_setsafety (BOOL bSafety)
{
   HB_TRACE(HB_TR_DEBUG, ("ct_setsafety(%i)", bSafety));
   s_bSafety = bSafety;
}

BOOL ct_getsafety (void)
{
   HB_TRACE(HB_TR_DEBUG, ("ct_getsafety()"));
   return (s_bSafety);
}

HB_FUNC (CSETSAFETY)
{

   hb_retni (ct_getsafety());

   if (ISLOG (1))
   {
      ct_setsafety(hb_parnl (1));
   }

   return;

}

LONG ct_StrFile( BYTE *pFileName, BYTE *pcStr, LONG sLen, BOOL bOverwrite, LONG lOffset, BOOL bTrunc)
{
   FHANDLE hFile;
   BOOL bOpen;
   BOOL bFile = hb_fsFile(pFileName);
   LONG lWrite = 0;

   if( bFile && bOverwrite)
   {
      hFile = hb_fsOpen(pFileName, FO_READWRITE);
      bOpen = 1;
   }
   else if ( ! bFile || ! ct_getsafety() )
   {
      hFile = hb_fsCreate(pFileName, ct_getfcreate() );
      bOpen = 0;
   }
   else
   {
      hFile = FS_ERROR;
   }

   if( hFile != FS_ERROR )
   {
      if ( lOffset )
      {
         hb_fsSeek(hFile, lOffset, FS_SET);
      }
      else if (bOpen) 
      {
         hb_fsSeek(hFile, 0, FS_END);
      }

      lWrite = hb_fsWrite(hFile, pcStr, sLen);
      if( (lWrite == sLen) && bOpen && bTrunc)
      {         
         hb_fsWrite(hFile, NULL, 0);
      }

      hb_fsClose( hFile );
   }
   return( lWrite );
}

HB_FUNC (STRFILE)
{

   if ( ISCHAR(1) && ISCHAR(2) )
   {
      hb_retnl( ct_StrFile((BYTE *) hb_parc(2), (BYTE *) hb_parc (1), hb_parclen (1), (ISLOG(3) ? hb_parl(3) : 0 ),
         (ISNUM(4) ? hb_parnl(4) : 0), (ISLOG(5) ? hb_parl(5) : 0 ) ) );
   }
   else
   {
      hb_retnl(0);
   }

}

HB_FUNC (FILESTR)
{

   if ( ISCHAR(1) )
   {
      FHANDLE hFile = hb_fsOpen((BYTE *) hb_parc(1), FO_READ);

      if( hFile != FS_ERROR )
      {
         LONG lFileSize = hb_fsSeek(hFile, 0, FS_END);
         LONG lPos = hb_fsSeek(hFile, (ISNUM(3) ? hb_parnl(3) : 0), FS_SET);
         LONG lLength = ISNUM(2) ? HB_MIN(hb_parnl(2), lFileSize - lPos) : lFileSize - lPos;
         char * pcResult = (char *) hb_xgrab(lLength);
         BOOL bCtrlZ = (ISLOG(4) ? hb_parl(4) : 0 );
         char * pCtrlZ;

         lLength = hb_fsRead(hFile, (BYTE *) pcResult, lLength);

         if( bCtrlZ )
         {
            pCtrlZ = (char *) memchr(pcResult, 26, lLength);
            if( pCtrlZ )
            {
               lLength = pCtrlZ - pcResult;
            }
         }

         hb_fsClose( hFile );
         hb_retclen(pcResult, lLength);
         hb_xfree(pcResult);
      }
      else
      {
         hb_retc("");
      }
   }
   else
   {
      hb_retc("");
   }

}

HB_FUNC( SCREENFILE )
{
   if ( ISCHAR(1) )
   {
      char *pBuffer;
      UINT uiSize;

      hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &uiSize );
      pBuffer = (char *) hb_xgrab( uiSize + 1 );

      hb_gtSave( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

      hb_retnl( ct_StrFile((BYTE *) hb_parc(1), (BYTE *) pBuffer, (LONG) uiSize, (ISLOG(2) ? hb_parl(2) : 0 ),
         (ISNUM(3) ? hb_parnl(3) : 0), (ISLOG(4) ? hb_parl(4) : 0 ) ) );
      hb_xfree( pBuffer );

   }
   else
   {
      hb_retnl(0);
   }
   
}

HB_FUNC( FILESCREEN )
{
   if ( ISCHAR(1) )
   {
      FHANDLE hFile = hb_fsOpen((BYTE *) hb_parc(1), FO_READ);

      if( hFile != FS_ERROR )
      {
         char *pBuffer;
         UINT uiSize;
         LONG lLength;

         if( ISNUM(2) )
         {
            hb_fsSeek(hFile, hb_parnl(2), FS_SET);
         }
         hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &uiSize );
         pBuffer = (char *) hb_xgrab( uiSize );

         lLength = hb_fsRead(hFile, (BYTE *) pBuffer, uiSize);
         hb_gtRest( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

         hb_fsClose( hFile );
         hb_retnl(lLength);
         hb_xfree(pBuffer);
      }
      else
      {
         hb_retnl(0);
      }
   }
   else
   {
      hb_retnl(0);
   }
}
