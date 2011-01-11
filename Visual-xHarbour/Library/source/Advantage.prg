/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Advantage.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
       
#ifdef VXH_ADS

#include "vxh.ch"
#include "ord.ch"

//-------------------------------------------------------------------------------------------------------

CLASS AdsDataTable INHERIT DataTable

   DATA ServerType       PUBLISHED INIT 1
   DATA xDriver          PROTECTED INIT "ADSCDX"
   DATA __ServerTypes    EXPORTED  INIT { "Local", "Remote", "Either" }
   DATA __ExplorerFilter EXPORTED  INIT { { "DataTable / Advantage (*.dbf,*.adt)", "*.dbf;*.adt" } }
   DATA __xCtrlName      EXPORTED  INIT "AdsDataTable"

   METHOD File2Blob( cFile, cField ) INLINE (::Area)->( AdsFile2Blob( cFile, cField ) )
   METHOD Blob2File( cFile, cField ) INLINE (::Area)->( AdsBlob2File( cFile, cField ) )
   METHOD AdsSetServerType(n)        INLINE AdsSetServerType(n)
ENDCLASS

//-------------------------------------------------------------------------------------------------------

#pragma BEGINDUMP
   #pragma comment( lib, "ads.lib" )
   #pragma comment( lib, "ace32.lib" )
#pragma ENDDUMP

#endif