/*
 * $Id: cgiread.c,v 1.1 2004/01/14 06:14:03 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * File read/write subsystem for GTCGI
 *
 * Source codes for functions:
 *    CGIREAD()
 *    CGIWRITE()
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
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

#include "hbapi.h"
#include "hbapiitm.h"
#ifdef HB_OS_WIN_32
#include <windows.h>
#endif

HB_FUNC(CGIREAD)
{
   ULONG cgilen;
   int n;
   char *cgistr, *temp;
   temp = getenv("REQUEST_METHOD");
   if (temp)
   {
      if ( strcmp(temp,"GET") == 0 )
      {
         temp = getenv("QUERY_STRING");
         if (temp)
         {
            cgilen = strlen(temp);
            cgistr = (char *) hb_xgrab(cgilen);
            strncpy(cgistr,temp,cgilen);
         }
         else
            hb_retc("");
         return;
      }
      else if ( strcmp(temp,"POST") == 0 )
      {
        sscanf(getenv("CONTENT_LENGTH"),"%d",&n);
        cgistr = (char *) hb_xgrab(n);
#ifdef HB_OS_WIN_32 //(WINDOWS.H must be included)
        ReadFile(GetStdHandle(STD_INPUT_HANDLE),cgistr,n,&cgilen,NULL);
#else
        cgilen=fread(cgistr,sizeof(char),n,stdin);
#endif
      }
      else /*Not get, not post...*/
      {
         hb_retc("");
         return;
      }
      hb_xfree(cgistr);
      hb_retclen(cgistr,cgilen);
   }
   else
      hb_retc("");
}

HB_FUNC(CGIWRITE)
{
   ULONG len;
   PHB_ITEM phbstr = hb_param(1,HB_IT_STRING);

#ifdef HB_OS_WIN_32
   if (phbstr)
      WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),hb_itemGetCPtr(phbstr),hb_itemGetCLen(phbstr),&len,NULL);
#else
   if (phbstr)
      fwrite(hb_itemGetCPtr(phbstr),sizeof(char),hb_itemGetCLen(phbstr),stdout);
#endif
}

