/*
 * $Id: bit1.c,v 1.5 2004/05/12 02:25:08 druzus Exp $
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - NUMAND()
 *                                              - NUMOR()
 *                                              - NUMXOR()
 *                                              - NUMNOT()
 *                                              - NUMROL()
 *                                              - NUMMIRR()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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
#include "ct.h"

static USHORT __hex2int( char *cNum1, int iLenHex );
static USHORT __getparam( int iParam );
static USHORT __numand( USHORT wNum1, USHORT wNum2 );
static USHORT __numor ( USHORT wNum1, USHORT wNum2 );
static USHORT __numxor( USHORT wNum1, USHORT wNum2 );
static USHORT __numnot( USHORT wNum1, USHORT wNum2 );
static LONG __numfun( int iPCount, USHORT (*operation)(USHORT wNum1, USHORT wNum2));



HB_FUNC( NUMAND )
{
  int iPCount;

  iPCount = hb_pcount();

  hb_retnl( __numfun( iPCount, (USHORT (*)(USHORT wNum1, USHORT wNum2))(__numand) ) );
}



HB_FUNC( NUMOR )
{
  int  iPCount;

  iPCount = hb_pcount();

  hb_retnl( __numfun( iPCount, (USHORT (*)(USHORT wNum1, USHORT wNum2))(__numor) ) );
}



HB_FUNC( NUMXOR )
{
  int  iPCount;

/*  iPCount = hb_pcount(); */

  iPCount = 2;

  hb_retnl( __numfun( iPCount, (USHORT (*)(USHORT wNum1, USHORT wNum2))(__numxor) ) );
}



HB_FUNC( NUMNOT )
{
  int  iPCount;

/*  iPCount = hb_pcount(); */

  iPCount = 1;

  hb_retnl( __numfun( iPCount, (USHORT (*)(USHORT wNum1, USHORT wNum2))(__numnot) ) );
}



HB_FUNC( NUMROL )
{
  USHORT   usNum1, usNum2, usNumBak, usPattern, usTestRol;
  USHORT   usBytes, usFor;

  usNum1 = (USHORT) __getparam( 1 );  /* Number to do ROL */
  usNum2 = (USHORT) __getparam( 2 );  /* Iterations       */

  if ( ISLOG( 3 ) )                   /* if 3th parameter is LOGICAL */
  {
    if ( hb_parl( 3 ) )
       usBytes = 8;
    else
       usBytes = 16;
  }
  else
    usBytes = 16;

  usNum2 = usNum2 % usBytes;          /* Set usNum2 < usBytes  */

  usPattern = (-1) << usBytes;

  usTestRol = 1 << ( usBytes - 1 );   /* Pattern to test the MSB */

  usNumBak = usNum1 & usPattern;      /* usNumBak contain the section
                                         to doesn't ROL               */

  for (usFor = 1; usFor <= usNum2; usFor++)
  {
     if ( usNum1 & usTestRol )  /* Test if MSB is ON */
     {
        usNum1 = usNum1 << 1;
        usNum1 = usNum1 | 1;    /* Simulate that the MSB move to LSB */
     }
     else
        usNum1 = usNum1 << 1;
   }
                              /* Set the section not ROLed */
  usNum1 = ( usNum1 & (~usPattern) ) | usNumBak;

  hb_retnl( usNum1 );
}



HB_FUNC ( NUMMIRR )
{
  USHORT  usNum1, usBytes, usFor, usPattern = 0, usNumBak, usMirror = 0;

  usNum1 = (USHORT) __getparam( 1 );

  if ( ISLOG( 2 ) )                   /* if 3th parameter is LOGICAL */
  {
    if ( hb_parl( 2 ) )
    {
       usBytes = 8;
       usPattern = 0xFF00;
    }
    else
    {
       usBytes = 16;
       usPattern = 0;
    }
  }
  else
    usBytes = 16;

  usNumBak = usNum1 & usPattern;

  for ( usFor = 1; usFor <= usBytes; usFor++ )
  {
    if ( usNum1 & 1 )
    {

       usMirror = usMirror << 1;  /* if the LSB of usNum1 == 1 then */
       usMirror = usMirror | 1;   /* set the LSB of usMirror = 1    */
    }
    else
       usMirror = usMirror << 1;

    usNum1 = usNum1 >> 1;

  }
  usMirror = ( usMirror & (~usPattern) ) | usNumBak;

  hb_retnl( usMirror );

}

/*
 * Function to test, transform Hexadecimal numbers to decimals numbers
 * It's more quicker than CTON() function

HB_FUNC ( HEX2NUM )
{
  hb_retnl( __hex2int( hb_parcx( 1 ), hb_parclen( 1 ) ) );
}
*/

static USHORT __hex2int( char *cNum1, int iLenHex )
{
  int  i;
  int  iNum;
  USHORT uiHexNum = 0;


  i = ( iLenHex - 1 );
  while (( i >= 0 ) && ( iLenHex-i <= 4 ))
  {
     iNum = ((int) cNum1[i]) - 0x30;

     if (iNum > 9)
       iNum -= 7;

     if ((iNum < 0) || (iNum > 0x0F))
       break;

     uiHexNum += (USHORT) iNum * (1 << (4 * ( iLenHex - i - 1 )));
     i--;
  }
  return uiHexNum;
}


static USHORT __getparam( int iParam )
{

  if ( ISCHAR( iParam ) )
     return  __hex2int( hb_parcx( iParam ), hb_parclen( iParam ) );
  else
     return (USHORT) hb_parnl( iParam );

}


static USHORT __numand( USHORT uiNum1, USHORT uiNum2 )
{
    return uiNum1 & uiNum2;
}


static USHORT __numor( USHORT uiNum1, USHORT uiNum2 )
{
    return uiNum1 | uiNum2;
}


static USHORT __numxor( USHORT uiNum1, USHORT uiNum2 )
{
    return uiNum1 ^ uiNum2;
}


static USHORT __numnot( USHORT uiNum1, USHORT uiNum2 )
{
    HB_SYMBOL_UNUSED (uiNum2);
    return ~uiNum1;
}


static LONG __numfun( int iPCount, USHORT (*operation)(USHORT wNum1, USHORT wNum2))
{
  USHORT uiNumOp = 0;
  USHORT uiNum1, uiNum2;
  int  iFor;

  if ( ISNUM(1) || ISCHAR(1) )
  {
     uiNum1 = __getparam( 1 );

     if ( iPCount == 1 )

  /*  If unary operation: NOT                           */
        uiNumOp = (*operation)( uiNum1, 0 );

     else
     {

        for ( iFor=2; iFor <= iPCount; iFor++)
        {
           if ( ISNUM( iFor ) || ISCHAR( iFor ) )
           {
              uiNum2 = __getparam( iFor );


  /*  Call to operation: AND, OR, XOR                   */
              uiNumOp = (*operation)( uiNum1, uiNum2 );

           }
           else
  /*  If error in parameter then return -1              */
              return (-1);

  /*  Copy result to first parameter if multi operation */
           uiNum1 = uiNumOp;
        }

     }

  }
  else

  /*  If error in parameter then return -1              */
     return (-1);


  /*  Return result of operation */
  return uiNumOp;

}

