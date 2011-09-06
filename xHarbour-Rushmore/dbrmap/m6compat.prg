/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    Mach SIx compatible library
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
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
