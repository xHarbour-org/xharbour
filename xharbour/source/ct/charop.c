/*
 * $Id: charop.c,v 1.2 2005/03/09 21:08:59 ptsarenko Exp $
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHARADD()
 *     - CHARSUB()  (NEW)
 *     - CHARAND()
 *     - CHARNOT()
 *     - CHAROR()
 *     - CHARXOR()  
 *     - CHARSHL()  (NEW)
 *     - CHARSHR()  (NEW)
 *     - CHARRLL()  (NEW)
 *     - CHARRLR()  (NEW)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "ct.h"


#define DO_CHAROP_CHARADD      0
#define DO_CHAROP_CHARSUB      1  /* new: character subtraction */
#define DO_CHAROP_CHARAND      2
#define DO_CHAROP_CHARNOT      3
#define DO_CHAROP_CHAROR       4
#define DO_CHAROP_CHARXOR      5
#define DO_CHAROP_CHARSHL      6  /* new: shift left */
#define DO_CHAROP_CHARSHR      7  /* new: shift right */
#define DO_CHAROP_CHARRLL      8  /* new: left rotation */
#define DO_CHAROP_CHARRLR      9  /* new: right rotation */

/* helper function */
static void do_charop (int iSwitch)
{

  int iNoRet;

  /* suppressing return value ? */
  iNoRet = ct_getref() && ISBYREF( 1 );

  if (ISCHAR (1))
  {
    
    size_t sStrLen = hb_parclen (1);
    size_t sPos;
    unsigned char *pucString = ( unsigned char * ) hb_parc (1);
    unsigned char *pucResult;
    
    if ( sStrLen == 0 )
    {
       if (iNoRet)
       {
          hb_ret();
       }
       else
       {
          hb_retc( "" );
       }
       return;
    }

    pucResult = ( unsigned char * ) hb_xgrab (sStrLen);

    switch (iSwitch)
    {
      /* NOT */
      case DO_CHAROP_CHARNOT:
      {

        for (sPos = 0; sPos < sStrLen; sPos++)
          pucResult[sPos] = ~pucString[sPos];

      }; break;

      /* SHL */
      case DO_CHAROP_CHARSHL:
      {
        
        int iSHL = (hb_parni (2))%8;  /* defaults to 0 */

        if (iSHL == 0)
          hb_xmemcpy (pucResult, pucString, sStrLen);
        else
          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = pucString[sPos] << iSHL;

      }; break;

      /* SHR */
      case DO_CHAROP_CHARSHR:      
      {
        
        int iSHR = (hb_parni (2))%8;  /* defaults to 0 */

        if (iSHR == 0)
          hb_xmemcpy (pucResult, pucString, sStrLen);
        else
          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = pucString[sPos] >> iSHR;

      }; break;

      /* RLL */
      case DO_CHAROP_CHARRLL:      
      {
        
        int iRLL = (hb_parni (2))%8;  /* defaults to 0 */

        hb_xmemcpy (pucResult, pucString, sStrLen);
        
        if (iRLL != 0)
          for (sPos = 0; sPos < sStrLen; sPos++)
          {
            int iRLLCnt;

            for (iRLLCnt = 0; iRLLCnt < iRLL; iRLLCnt++)
              if (pucResult[sPos]&0x80)  /* most left bit set -> roll over */
              {
                pucResult[sPos] <<= 1;
                pucResult[sPos] |= 0x01;
              }
              else
              {
                pucResult[sPos] <<= 1;
              }
          }

      }; break;

      /* RLR */
      case DO_CHAROP_CHARRLR:      
      {
        
        int iRLR = (hb_parni (2))%8;  /* defaults to 0 */

        hb_xmemcpy (pucResult, pucString, sStrLen);
        
        if (iRLR != 0)
          for (sPos = 0; sPos < sStrLen; sPos++)
          {
            int iRLRCnt;

            for (iRLRCnt = 0; iRLRCnt < iRLR; iRLRCnt++)
              if (pucResult[sPos]&0x01)  /* most right bit set -> roll over */
              {
                pucResult[sPos] >>= 1;
                pucResult[sPos] |= 0x80;
              }
              else
              {
                pucResult[sPos] >>= 1;
              }
          }

      }; break;

      /* ADD */
      case DO_CHAROP_CHARADD: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos]+pucString2[sPos%sStrLen2]);

        }
        else
        {
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARADD,
                      NULL, "CHARADD", 0, EF_CANDEFAULT, 2,
                      hb_paramError (1), hb_paramError (2));
          }
          hb_xmemcpy (pucResult, pucString, sStrLen);
        }
      }; break;

      /* SUB */
      case DO_CHAROP_CHARSUB: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos]-pucString2[sPos%sStrLen2]);

        }
        else
        {
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSUB,
                      NULL, "CHARSUB", 0, EF_CANDEFAULT, 2,
                      hb_paramError (1), hb_paramError (2));
          }
          hb_xmemcpy (pucResult, pucString, sStrLen);
        }
      }; break;

      /* AND */
      case DO_CHAROP_CHARAND: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] & pucString2[sPos%sStrLen2]);

        }
        else
        {
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARAND,
                      NULL, "CHARAND", 0, EF_CANDEFAULT, 2,
                      hb_paramError (1), hb_paramError (2));
          }
          hb_xmemcpy (pucResult, pucString, sStrLen);
        }
      }; break;

      /* OR */
      case DO_CHAROP_CHAROR:  
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] | pucString2[sPos%sStrLen2]);

        }
        else
        {
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHAROR,
                      NULL, "CHAROR", 0, EF_CANDEFAULT, 2,
                      hb_paramError (1), hb_paramError (2));
          }
          hb_xmemcpy (pucResult, pucString, sStrLen);
        }
      }; break;

      /* XOR */
      case DO_CHAROP_CHARXOR: 
      {
        if (ISCHAR (2))
        {
          char *pucString2 = hb_parc (2);
          size_t sStrLen2 = hb_parclen (2);

          for (sPos = 0; sPos < sStrLen; sPos++)
            pucResult[sPos] = (char)(pucString[sPos] ^ pucString2[sPos%sStrLen2]);

        }
        else
        {
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARXOR,
                      NULL, "CHARXOR", 0, EF_CANDEFAULT, 2,
                      hb_paramError (1), hb_paramError (2));
          }
          hb_xmemcpy (pucResult, pucString, sStrLen);
        }
      }; break;

    }; /* endswitch (iSwitch) */

    if (ISBYREF (1))
      hb_storclen (( char * ) pucResult, sStrLen, 1);

    if (!iNoRet)
      hb_retclen (( char * ) pucResult, sStrLen);

    hb_xfree (pucResult);

  }
  else   /* if (ISCHAR (1)) */
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      switch (iSwitch)
      {
        case DO_CHAROP_CHARADD:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARADD,
                                   NULL, "CHARADD", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHAROP_CHARSUB:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSUB,
                                   NULL, "CHARSUB", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHAROP_CHARAND:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARAND,
                                   NULL, "CHARAND", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHAROP_CHARNOT:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARNOT,
                                   NULL, "CHARNOT", 0, EF_CANSUBSTITUTE, 1,
                                   hb_paramError (1));
        }; break;

        case DO_CHAROP_CHAROR:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHAROR,
                                   NULL, "CHAROR", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHAROP_CHARXOR:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARXOR,
                                   NULL, "CHARXOR", 0, EF_CANSUBSTITUTE, 2,
                                   hb_paramError (1), hb_paramError (2));
        }; break;

        case DO_CHAROP_CHARSHL:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSHL,
                                   NULL, "CHARSHL", 0, EF_CANSUBSTITUTE, 1,
                                   hb_paramError (1));
        }; break;

        case DO_CHAROP_CHARSHR:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSHR,
                                   NULL, "CHARSHR", 0, EF_CANSUBSTITUTE, 1,
                                   hb_paramError (1));
        }; break;

        case DO_CHAROP_CHARRLL:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARRLL,
                                   NULL, "CHARRLL", 0, EF_CANSUBSTITUTE, 1,
                                   hb_paramError (1));
        }; break;

        case DO_CHAROP_CHARRLR:
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARRLR,
                                   NULL, "CHARRLR", 0, EF_CANSUBSTITUTE, 1,
                                   hb_paramError (1));
        }; break;

      }
    }

    if (pSubst != NULL)
    {
      hb_itemRelease( hb_itemReturnForward( pSubst ) );
    }
    else
    {
      hb_ret();
    }
  }

  return;

}



HB_FUNC (CHARADD)
{

  do_charop (DO_CHAROP_CHARADD);
  return;

}



HB_FUNC (CHARSUB)
{

  do_charop (DO_CHAROP_CHARSUB);
  return;

}



HB_FUNC (CHARAND)
{

  do_charop (DO_CHAROP_CHARAND);
  return;

}



HB_FUNC (CHARNOT)
{

  do_charop (DO_CHAROP_CHARNOT);
  return;

}



HB_FUNC (CHAROR)
{

  do_charop (DO_CHAROP_CHAROR);
  return;

}



HB_FUNC (CHARXOR)
{

  do_charop (DO_CHAROP_CHARXOR);
  return;

}



HB_FUNC (CHARSHL)
{

  do_charop (DO_CHAROP_CHARSHL);
  return;

}



HB_FUNC (CHARSHR)
{

  do_charop (DO_CHAROP_CHARSHR);
  return;

}



HB_FUNC (CHARRLL)
{

  do_charop (DO_CHAROP_CHARRLL);
  return;

}



HB_FUNC (CHARRLR)
{

  do_charop (DO_CHAROP_CHARRLR);
  return;

}



