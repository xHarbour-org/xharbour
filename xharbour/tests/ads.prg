#include "..\contrib\rdd_ads\ads.ch"

PROCEDURE TestAds( cDBF )

  LOCAL cField

  IF cDbf == NIL
     cDbf := "test"
  ENDIF

  #ifdef __HARBOUR__
     RddRegister( "ADS", 1 )
     RddSetDefault( "ADS" )
     AdsSetServerType( 1 )
     AdsSetFileType( 2 )
  #endif

  USE ( cDBF ) EXCLUSIVE

  IF ! File( "ADSTEST.DBF" )
    COPY STRUCTURE TO ADSTEST
  ENDIF

  USE ADSTEST

  DO WHILE RecCount() < 200000
     ? "Copying..."
     APPEND ALL FROM TEST
  ENDDO

  cField := FieldName(1)

  #ifdef __HARBOUR__
     Register_CallBack( {|n| QOut(n) } )
     INDEX ON &cField TO (cField)
     UnRegister_CallBack()
  #else
     INDEX ON FIELD->&(cField) TO (cField)
  #endif

RETURN

#ifdef __HARBOUR__

#pragma BEGINDUMP

#include <windows.h>

#define WIN32

#include "..\contrib\rdd_ads\ace.h"

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"

unsigned long __stdcall ShowPercentage( UNSIGNED16 usPercentDone );

static PHB_ITEM pBlock;

HB_FUNC( REGISTER_CALLBACK )
{
   pBlock = hb_itemParam( 1 );

   AdsRegisterProgressCallback( ShowPercentage );
}

HB_FUNC( UNREGISTER_CALLBACK )
{
   AdsClearProgressCallback();

   if( pBlock )
   {
      hb_itemRelease( pBlock );
   }
}

unsigned long __stdcall ShowPercentage( UNSIGNED16 usPercentDone )
{
   PHB_ITEM pPercent = hb_itemPutNI( NULL, usPercentDone );

   if( pPercent )
   {
      hb_vmEvalBlockV( pBlock, 1, pPercent );
      hb_itemRelease( pPercent );
   }

   return 0;
}

#pragma ENDDUMP

#endif
