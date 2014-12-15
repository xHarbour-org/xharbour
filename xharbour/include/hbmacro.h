/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Macro compiler
 *
 * Copyright 1999 Ryszard Glab
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

#ifndef HB_MACRO_H_
#define HB_MACRO_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <time.h>

/* Standard parameters passed to macro aware functions
 */
#define PHB_BISON       void *
#define HB_MACRO_PARAM  pMacro
#define HB_MACRO_DECL   PHB_BISON HB_MACRO_PARAM

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbexprop.h"
#include "hbpcode.h"
#include "hbmacro.ch"

HB_EXTERN_BEGIN

/* flags for compilation process
 */
#define HB_MACRO_GEN_PUSH        1 /* generate PUSH pcodes */
#define HB_MACRO_GEN_POP         2 /* generate POP pcodes */
#define HB_MACRO_GEN_ALIASED     4 /* force aliased variable */
#define HB_MACRO_GEN_TYPE        8 /* check the type of expression (from TYPE() function) */
#define HB_MACRO_GEN_PARE       16 /* generate parentesized list */
#define HB_MACRO_GEN_LIST       32 /* generate push operation for every comma separated expressions */
#define HB_MACRO_DEALLOCATE    128 /* macro structure is allocated on the heap */
#define HB_MACRO_GEN_STATEMENT 256 /* generate STATEMENT pcodes */

/* values returned from compilation process
 */
#define HB_MACRO_OK           0    /* macro compiled successfully */
#define HB_MACRO_FAILURE      1    /* syntax error */

/* additional status of compilation
 */
#define HB_MACRO_CONT         1    /* everything is OK so far */
#define HB_MACRO_TOO_COMPLEX  2    /* compiled expression is too complex */
#define HB_MACRO_UDF          4    /* code uses UDF function (info used by TYPE function) */
#define HB_MACRO_UNKN_SYM     8    /* requested symbol was not found in runtime symbol table */
#define HB_MACRO_UNKN_VAR     16   /* requested variable doesn't exist */

/* Global functions
 */
extern void hb_macroError( int iError, PHB_BISON pMacro );
extern int hb_macroYYParse( PHB_MACRO pMacro );
extern HB_SIZE hb_macroSetMacro( BOOL bSet, HB_SIZE ulFlag );
extern HB_SIZE hb_macroAutoSetMacro( HB_SIZE ulFlag );

extern void hb_compGenPCode1( BYTE byte, PHB_BISON pMacro );
extern void hb_compGenPData1( BYTE byte, PHB_BISON pMacro );
extern void hb_compGenPCode2( BYTE byte1, BYTE byte2, PHB_BISON pMacro );
extern void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3, PHB_BISON pMacro );
extern void hb_compGenPCode4( BYTE byte1, BYTE byte2, BYTE byte3, BYTE byte4, PHB_BISON pMacro );
extern void hb_compGenPCodeN( BYTE * pBuffer, HB_SIZE ulSize, PHB_BISON pMacro );

/* Size of pcode buffer incrementation
 */
#define HB_PCODE_SIZE  512

/* Bison requires (void *) pointer  - some code needs PHB_MACRO pointer
 */
#define HB_MACRO_DATA     ( (PHB_MACRO) HB_MACRO_PARAM )
#define HB_PCODE_DATA     ( HB_MACRO_DATA->pCodeInfo )

/* Declarations for functions macro.c */

extern int hb_compLocalVarGetPos( char * szVarName, PHB_BISON pMacro );
extern HB_SIZE hb_compGenJump( LONG lOffset, PHB_BISON pMacro );
extern HB_SIZE hb_compGenJumpFalse( LONG lOffset, PHB_BISON pMacro );
extern void hb_compGenJumpThere( HB_SIZE ulFrom, HB_SIZE ulTo, PHB_BISON pMacro );
extern void hb_compGenJumpHere( HB_SIZE ulOffset, PHB_BISON pMacro );
extern HB_SIZE hb_compGenJumpTrue( LONG lOffset, PHB_BISON pMacro );
extern void hb_compMemvarGenPCode( BYTE bPCode, char * szVarName, PHB_BISON pMacro );
extern void hb_compGenPushSymbol( char * szSymbolName, char * szNamespace, BOOL bAlias, PHB_BISON pMacro );
extern void hb_compGenPushLong( HB_LONG lNumber, PHB_BISON pMacro );
extern void hb_compGenPushDate( LONG lDate, LONG lTime, USHORT uType, PHB_BISON pMacro );
extern void hb_compGenMessage( char * szMsgName, PHB_BISON pMacro );
extern void hb_compGenMessageData( char * szMsg, PHB_BISON pMacro );
extern void hb_compGenPopVar( char * szVarName, PHB_BISON pMacro );
extern void hb_compGenPopAliasedVar( char * szVarName,
                                     BOOL bPushAliasValue,
                                     char * szAlias,
                                     LONG lWorkarea, PHB_BISON pMacro );
extern void hb_compGenPushVar( char * szVarName, PHB_BISON pMacro );
extern void hb_compGenPushVarRef( char * szVarName, PHB_BISON pMacro );
extern void hb_compGenPushMemVarRef( char * szVarName, PHB_BISON pMacro );
extern void hb_compGenPushAliasedVar( char * szVarName,
                                      BOOL bPushAliasValue,
                                      char * szAlias,
                                      LONG lWorkarea, PHB_BISON pMacro );
extern void hb_compGenPushLogical( int iTrueFalse, PHB_BISON pMacro );
extern void hb_compGenPushDouble( double dNumber, BYTE bWidth, BYTE bDec, PHB_BISON pMacro );
extern void hb_compGenPushFunCall( char * szFunName, char * szNamespace, PHB_BISON pMacro );
extern void hb_compGenPushString( char * szText, HB_SIZE ulStrLen, PHB_BISON pMacro );
extern void hb_compCodeBlockStart( PHB_BISON pMacro );
extern void hb_compCodeBlockEnd( PHB_BISON pMacro );

HB_EXTERN_END

#endif /* HB_MACRO_H_ */
