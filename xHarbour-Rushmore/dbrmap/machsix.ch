/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    Mach SIx compatible library
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
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

#include "dbinfo.ch"

/* SIX m6_FiltJoin actions */
#define JOIN_UNION        1   /* logical OR on records in two filter */
#define JOIN_INTERSECT    2   /* logical AND on records in two filter */
#define JOIN_DIFFERENCE   3   /* logical XOR on records in two filter */

/* SIX m6_FiltInfo array elements */
#define INFO_EXPR       1  /* Complete filter expression */
#define INFO_NONEXPR    2  /* Non-indexed expression */
#define INFO_OPTLVL     3  /* Optimization level */
#define INFO_COUNT      4  /* Number of records in filter */
#define INFO_SIZE       5  /* Maximum valid record no. */
#define INFO_OWNER      6  /* Filter Owner, 1=System, 2=User */
#define INFO_POS        7  /* Current position in filter */
#define INFO_ASIZE      7  /* Size of return array */

/* SIX filter owners */
#define OWN_SYSTEM      1  /* filter owner is sytem (bound with WA) */
#define OWN_USER        2  /* filter oner is user */

/* SIX m6_IsOptimize values, this are the same values */
#define OPT_NONE        DBOI_OPTIMIZED_NONE
#define OPT_PARTIAL     DBOI_OPTIMIZED_PART
#define OPT_FULL        DBOI_OPTIMIZED_FULL

/*
 * undocumented m6_set() functions parameters, be careful this values
 * had to be changed to be compatible with CL5.3 and xHarbour extensions
 */
#define _SET_TYPECHECK  1  /* only for compatibility - will be ignored */
/*#define _SET_OPTIMIZE   2*/  /* we already have it with this name (44) */
#define _SET_RECHECK    _SET_FORCEOPT  /* we already have it */

/* SIX m6_Error codes */
#define M6ERR_OK           0  /* no error */
#define M6ERR_NOTABLE   2001  /* No database in use */
#define M6ERR_NOTSUPP   2003  /* Driver not supported */
#define M6ERR_NOIDX     2005  /* No index(es) in use */
#define M6ERR_TOOBIG    2007  /* Database contains over 524 million records */
#define M6ERR_NOMATCH   2009  /* No matching index expression(s) found */
#define M6ERR_INDETERM  2011  /* Indeterminate */
#define M6ERR_QUOTE     2013  /* Missing quote ( [, ], ', " ) */
#define M6ERR_PARENTH   2015  /* Missing parenthesis */
#define M6ERR_SYNTAX    2017  /* Syntax error */
#define M6ERR_TYPE      2019  /* Data type error */
#define M6ERR_NOFILTER  2021  /* No filter active */
#define M6ERR_DSTACKOFL 2023  /* Deferred stack overflow */
#define M6ERR_LOWMEM    2025  /* Low memory */
#define M6ERR_AUTOPTOFF 2027  /* Automatic optimization off */
#define M6ERR_JOINTYPE  2029  /* Invalid filter join type */
#define M6ERR_DIFFSIZE  2031  /* Can't join filters of different lengths */
#define M6ERR_CANTOPT   2033  /* Non-optimizable .OR.'ed condition */
#define M6ERR_SCPRANGE  2035  /* High/Low parameters of m6_AddScoped() reversed */
#define M6ERR_ARRAYSIZE 2101  /* Maximum number of Clipper array elements(4096) exceeded */
#define M6ERR_BADOPER   2103  /* Unknown operator */
#define M6ERR_STACKOVFL 2105  /* Stack overflow */
#define M6ERR_STACKUVFL 2107  /* Stack underflow */
#define M6ERR_BADRMTYPE 2109  /* Unknown map type */
#define M6ERR_BADHANDLE 2207  /* Bad filter handle */
#define M6ERR_RECRANGE  2209  /* Record number out of filter range */
#define M6ERR_FWRITE    2211  /* file write error */
#define M6ERR_FREAD     2213  /* file read error */
#define M6ERR_COMPILE   2219  /* Internal compile error */
#define M6ERR_EVAL      2221  /* Internal evaluation error */
#define M6ERR_FCREATE   2223  /* file create error */
#define M6ERR_FOPEN     2224  /* file open error */
