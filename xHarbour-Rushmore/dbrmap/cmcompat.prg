/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    ClipMore/COMIX compatible function set
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
 *
 */

/* ************************************************************************ */
/* cm*() functions */
/* ************************************************************************ */

#include "ord.ch"

#define SAVEFILTSIG     1339732
#define isValidCSF(saveFilt)  (valType(saveFilt)=="C".and.bin2l(saveFilt)==SAVEFILTSIG)

function cmSetLinear( lVal )
return set( _SET_FORCEOPT, lVal )

function cmGetLinear()
return set( _SET_FORCEOPT )

function cmDoLinear()
   rlDoLinear()
return nil

function cmSmartFilter( lVal )
return set( _SET_OPTIMIZE, lVal )


function cmVersion( nDetail )
   local cVer := "3.00"
   if valType( nDetail ) == "N"
      if nDetail == 1
         cVer += ".00"
      elseif nDetail == 2
         cVer += ".00 (HbMore/5.2)"
      endif
   endif
return cVer

/* convert any value to string expresion */
function cm2Str( xValue )
   local cType := valType( xValue ), cRet, n

   if cType $ "CM"
      cRet := '"' + strtran( xValue, '"',  '"+'+"'"+'"'+"'"+'+"' ) + '"'
//      cRet := '"' + strtran( xValue, '"', ["+'"'+"] ) + '"'
   elseif cType $ "D"
      cRet := "stod('" + dtos(xValue) + "')"
   elseif cType $ "L"
      cRet := iif( xValue, ".T.", ".F." )
   elseif cType $ "N"
      cRet := str( xValue, 41, 20 )
      n := len( cRet )
      while substr( cRet, n, 1 ) == '0'
         --n
      enddo
      if substr( cRet, n, 1 ) == '.'
         --n
      endif
      cRet := ltrim( substr( cRet, 1, n ) )
   endif
return cRet

function cmClrFilter()
   if used()
      dbClearFilter()
   endif
return nil

function cmFilter( cCond )
   if valType(cCond) != "C" .or. empty(cCond)
      dbClearFilter()
   else
      dbSetFilter( &( "{||" + cCond + "}" ), cCond )
      if cmGetLinear()
         cmDoLinear()
      endif
   endif
return nil

function cmReFilter( cCond )
   local rlOld, rlNew, cNewCond

   if rlGetFilter() == 0
      return cmFilter( cCond )
   endif
   if valType(cCond) == "C" .and. !empty(cCond)
      rlOld := rlExFilter()
      cNewCond := "(" + alltrim( dbfilter() ) + ").and.(" + alltrim( cCond ) + ")"
      dbSetFilter( &( "{||" + cNewCond + "}" ), cNewCond )
      rlNew := rlExFilter()
      if rlNew == 0
         rlNew := rlOld
      else
         rlNew := rlAnd( rlNew, rlOld )
      endif
      rlSetFilter( rlNew )
      if cmGetLinear()
         cmDoLinear()
      endif
   endif
return nil

function cmSaveFilter()
   local rl := rlGetFilter()

   if rl != 0
      rl := rlNewDup( rl )
   endif
return l2bin( SAVEFILTSIG ) + l2bin( rl ) + dbFilter()

function cmRestFilter( cSaveFilt )
   local rl, cFilter, n := len( l2bin( 0 ) )

   if isValidCSF( cSaveFilt )
      rl := bin2l( substr( cSaveFilt, n + 1, n ) )
      cFilter := substr( cSaveFilt, n + n + 1 )
      if !empty(cFilter)
         dbSetFilter( &( "{||" + cFilter + "}" ), cFilter )
      else
         dbClearFilter()
      endif
      rlSetFilter(rl)
   endif
return nil

function cmDestroyFilter( cSaveFilt )
   local rl, n := len( l2bin( 0 ) )

   if isValidCSF( cSaveFilt )
      rl := bin2l( substr( cSaveFilt, n + 1, n ) )
      if rl != 0
         rlDestroy( rl )
      endif
   endif
return nil

function cmFiltCount()
   local rl := rlGetFilter(), nRet

   if rl != 0
      rlDoLinear()
      nRet := rlCount( rl )
   else
      nRet := recCount()
   endif
return nRet

function cmKeyCount( xTag, cBag )
return dbOrderInfo( DBOI_KEYCOUNT, cBag, xTag )

function cmKeySkip( nCount )
   if valType( nCount ) == "N"
      dbskip( nCount )
   endif
return nil

function cmKeyGoto( nPos )
if valType( nPos ) == "N"
   dbOrderInfo( DBOI_KEYGOTO,,, nPos )
endif
return nil

function cmKeyNo( xTag, cBag )
return dbOrderInfo( DBOI_KEYNO, cBag, xTag )

/* ************************************************************************ */
/* cmx*() functions */
/* ************************************************************************ */

function cmxSys( nRequest, xParam )
   local xRet

   if valType( nRequest ) == "N"
      if nRequest == 1001
         xRet := set( _SET_STRICTREAD, xParam )
      elseif nRequest == 1002
         xRet := set( _SET_HARDCOMMIT, xParam )
      endif
   endif
return xRet

function cmxVersion( nDetail )
   local cVer := "3.00"
   if valType( nDetail ) == "N"
      if nDetail == 1
         cVer += ".00"
      elseif nDetail == 2
         cVer += ".00 (HbComixCompat/5.2)"
      endif
   endif
return cVer

function cmxAutoOpen( lVal )
return set( _SET_AUTOPEN, lVal )

function cmxAutoOrder( lVal )
return set( _SET_AUTORDER, lVal )

function cmxAutoShare( lVal )
return set( _SET_AUTOSHARE, lVal )

function cmxClrScope( nScope )
return ordScope( nScope, nil )

function cmxCount( cBag )
return ordCount( cBag )

function cmxCustom( xTag, cBag, lSet )
return ordCustom( xTag, cBag, lSet )

function cmxDescend( xTag, cBag, lSet )
return ordDescend( xTag, cBag, lSet )

function cmxFor( xTag, cBag, cCond )
return ordFor( xTag, cBag, cCond )

function cmxIndexInfo()
   local aInfo, nOrds := ordCount(), i

   aInfo := array( 6, nOrds )
   for i := 1 to nOrds
      aInfo[ 1, i ] := ordName( i )
      aInfo[ 2, i ] := ordKey( i )
      aInfo[ 3, i ] := ordFor( i )
      aInfo[ 4, i ] := ordIsUnique( i )
      aInfo[ 5, i ] := ordDescend( i )
      aInfo[ 6, i ] := ordCustom( i )
   next
return aInfo

function cmxKeyAdd( xTag, cBag )
return ordKeyAdd( xTag, cBag )

function cmxKeyCount( xTag, cBag )
return dbOrderInfo( DBOI_KEYCOUNTRAW, cBag, xTag )

function cmxKeyDel( xTag, cBag )
return ordKeyDel( xTag, cBag )

function cmxKeyGoTo( nPos )
if valType( nPos ) == "N"
   dbOrderInfo( DBOI_KEYGOTORAW,,, nPos )
endif
return nil

function cmxKeyNo( xTag, cBag )
return dbOrderInfo( DBOI_KEYNORAW, cBag, xTag )

function cmxKeysIncluded()
return dbOrderInfo( DBOI_KEYSINCLUDED )

function cmxKeyVal()
return ordKeyVal()

function cmxKeySkip( nRec )
return ordSkipRaw( nRec )

function cmxMemoBlock( nSize )
return set( _SET_MBLOCKSIZE, nSize )

function cmxMemoExt()
return set( _SET_MFILEEXT )

function cmxRollback()
return dbinfo(DBI_ROLLBACK)

function cmxSeekLast( xVal, lSoft )
return dbseek( xVal, lSoft, .t. )

function cmxSetRelation( xArea, bKey, cKey )
return ordSetRelation( xArea, bKey, cKey )

function cmxSetScope( nScope, xVal )
return iif( PCount() > 1, ordScope( nScope, xVal ), ;
            dbOrderInfo( iif( valtype( nScope ) == "N" .and. nScope == 1, ;
                           DBOI_SCOPEBOTTOM, DBOI_SCOPETOP ) ) )

function cmxShared( lSet )
return dbInfo( DBI_SHARED, lSet )

function cmxSkipUnique( nDir )
return ordSkipUnique( nDir )

function cmxUnique( xTag, cBag )
return ordIsUnique( xTag, cBag )
