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

#include "machsix.ch"

static s_lTypeCheck := .t.
static s_cTmpPath := ""

function m6_Set( nRequest, xParam )
   local xRet

   if valType( nRequest ) == "N"
      if nRequest == _SET_TYPECHECK .or. nRequest == 1
         xRet := s_lTypeCheck
         if valtype( xParam ) == "L"
            s_lTypeCheck := xParam
         elseif valtype( xParam ) == "C"
            s_lTypeCheck := upper( alltrim( xParam ) ) == "ON"
         endif
      elseif nRequest == _SET_OPTIMIZE .or. nRequest == 2
         xRet := set( _SET_OPTIMIZE, xParam )
      elseif nRequest == _SET_RECHECK .or. nRequest == 4
         xRet := set( _SET_FORCEOPT, xParam )
      endif
   endif
return xRet

function m6_RefreshFilter()
   local cFilter, nFilter

   cFilter := dbfilter()
   if !empty( cFilter )
      nFilter := m6_NewFilter( cFilter )
      if !empty( nFilter )
         m6_SetAreaFilter( nFilter )
         if set( _SET_FORCEOPT )
            rlDoLinear()
         endif
      endif
   endif
return nil

function m6_SetFilter( bFilter, cFilter, lNoOptimize, lLinear )
return dbSetFilter( bFilter, cFilter, lNoOptimize, lLinear )

function m6_dbEval( bEvalBlk, cForExpr, bForBlk )
   local nNewFilter, nOldFilter

   if empty( cForExpr ) .or. m6_IsOptimize( cForExpr ) == OPT_NONE
      dbEval( bEvalBlk, bForBlk )
   else
      if !valtype( bForBlk ) == "B"
         bForBlk := &( "{||" + cForExpr + "}" )
      endif
      nNewFilter := m6_NewFilter( cForExpr )
      if empty( nNewFilter )
         dbEval( bEvalBlk, bForBlk )
      else
         nOldFilter := m6_GetAreaFilter()
         if !empty( nOldFilter )
            m6_FiltJoin( nNewFilter, nOldFilter, JOIN_INTERSECT )
            m6_ChgOwner( nOldFilter )
         endif
         m6_SetAreaFilter( nNewFilter )
         dbEval( bEvalBlk, bForBlk )
         if !empty( nOldFilter )
            m6_SetAreaFilter( nOldFilter )
         else
            nNewFilter := m6_GetAreaFilter()
            m6_ChgOwner( nNewFilter )
            m6_FreeFilter( nNewFilter )
         endif
      endif
   endif
return nil

function m6_SetTemp( cPath )
   local cRet := s_cTmpPath
   if valtype( cPath ) == "C"
      s_cTmpPath := cPath
   endif
return cRet
