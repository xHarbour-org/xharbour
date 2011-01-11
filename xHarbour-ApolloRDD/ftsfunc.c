/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 */

#include "rddsix.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "hbstack.h"

SIXAREAP getCurrentArea( void );

#define FTS_SHARE    0x0     /* SHARE */
#define FTS_EXCL     0x1     /* EXCLUSIVE */
#define FTS_RDONLY   0x2     /* READ-ONLY */

typedef  long FTSHANDLE;

long      __stdcall  FtsAdd      ( FTSHANDLE, unsigned char * );
short     __stdcall  FtsClose    ( FTSHANDLE );
FTSHANDLE __stdcall  FtsCreate   ( char *, short, short, char, short );
short     __stdcall  FtsDelete   ( FTSHANDLE, long );
int       __stdcall  FtsSet      ( FTSHANDLE, unsigned char * );
short     __stdcall  FtsIsDelete ( FTSHANDLE, long );
FTSHANDLE __stdcall  FtsOpen     ( char *, short, short );
long      __stdcall  FtsNextRec  ( FTSHANDLE );
long      __stdcall  FtsNumRecs  ( FTSHANDLE );
short     __stdcall  FtsReplace  ( FTSHANDLE, unsigned char *, long );
short     __stdcall  FtsUnDelete ( FTSHANDLE, long );
short     __stdcall  FtsVerify   ( FTSHANDLE, unsigned char *, unsigned char *, short );
char *    __stdcall  FtsVersion ( void );


HB_FUNC( HS_ADD )
{
   PHB_ITEM pItem;

   if( ( pItem = hb_param( 2, HB_IT_STRING ) ) != NULL )
      hb_retni( FtsAdd( (FTSHANDLE) hb_parni(1), (unsigned char*) hb_itemGetCPtr( pItem ) ) );
   else if( ( pItem = hb_param( 2, HB_IT_BLOCK ) ) != NULL )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      hb_vmSend( 0 );
      hb_retni( FtsAdd( (FTSHANDLE) hb_parni(1), (unsigned char *) hb_itemGetCPtr( hb_stackReturnItem() ) ) );
   }
   else
      hb_retni( -16 );
}

HB_FUNC( HS_CLOSE )
{
   hb_retni( FtsClose( (FTSHANDLE) hb_parni(1) ) );
}

HB_FUNC( HS_CREATE )
{
   hb_retni(
       FtsCreate( (char *) hb_parc(1), (hb_pcount()<2 || ISNIL(2))? 10:hb_parni(2),
                          (hb_pcount()<3 || ISNIL(3))? 2:hb_parni(3),
                          (hb_pcount()<4 || ISNIL(4))? 1:hb_parl(4),
                          (hb_pcount()<5 || ISNIL(5))? 1:hb_parni(5) ) );
}

HB_FUNC( HS_DELETE )
{
   hb_retni( FtsDelete( (FTSHANDLE) hb_parni(1), hb_parnl(2) ) );
}

HB_FUNC( HS_IFDEL )
{
   hb_retni( FtsIsDelete( (FTSHANDLE) hb_parni(1), hb_parnl(2) ) );
}

HB_FUNC( HS_INDEX )
{
   SIXAREAP pArea = getCurrentArea();

   if( pArea )
   {
      FTSHANDLE handle;
      LONG ulRecno;
      SHORT usOrder;
      char * szString = (char *) hb_parc(2);
      PHB_ITEM pExpr;

      if( !szString || SELF_COMPILE( ( AREAP ) pArea, szString ) != SUCCESS )
      {
         hb_retni( -16 );
         return;
      }
      pExpr = pArea->area.valResult;
      pArea->area.valResult = NULL;

      handle = FtsCreate( (char *) hb_parc(1),
                          (hb_pcount()<3 || ISNIL(3))? 16:hb_parni(3),
                          (hb_pcount()<5 || ISNIL(5))? 2:hb_parni(5),
                          (hb_pcount()<6 || ISNIL(6))? 1:hb_parl(6),
                          (hb_pcount()<7 || ISNIL(7))? 1:hb_parni(7) );

      if( handle >= 0 )
      {
         usOrder = sx_IndexOrd();
         if( usOrder )
            sx_SetOrder( 0 );
         ulRecno = sx_RecNo();
         sx_GoTop();
         while( !sx_Eof() )
         {
            FtsAdd( (FTSHANDLE) hb_parni(1), (unsigned char *)
                    hb_itemGetCPtr( hb_vmEvalBlockOrMacro( pExpr ) ) );
            sx_Skip( 1 );
         }
         if( usOrder )
            sx_SetOrder( usOrder );
         sx_Go( ulRecno );
      }
      hb_vmDestroyBlockOrMacro( pExpr );
      hb_retni( handle );
   }
   else
      hb_retni( -16 );

}

HB_FUNC( HS_KEYCOUNT )
{
   hb_retnl( FtsNumRecs( (FTSHANDLE) hb_parni(1) ) );
}

HB_FUNC( HS_NEXT )
{
   hb_retnl( FtsNextRec( (FTSHANDLE) hb_parni(1) ) );
}

HB_FUNC( HS_OPEN )
{
   int   nMode = hb_parni(2);

   hb_retni( FtsOpen( (char *) hb_parc(1),
                      (hb_pcount()<2 || ISNIL(2))? 10:hb_parni(2),
                      ( ( nMode<2 )? 0 : FTS_RDONLY ) | ( ( nMode%2 )? FTS_EXCL : FTS_SHARE ) ) );
}

HB_FUNC( HS_REPLACE )
{
   hb_retni( FtsReplace( (FTSHANDLE) hb_parni(1), (unsigned char *) hb_parc(2), hb_parnl(3) ) );
}

HB_FUNC( HS_SET )
{
   hb_retni( FtsSet( (FTSHANDLE) hb_parni(1), (unsigned char *) hb_parc(2) ) );
}

HB_FUNC( HS_UNDELETE )
{
   hb_retni( FtsUnDelete( (FTSHANDLE) hb_parni(1), hb_parnl(2) ) );
}

HB_FUNC( HS_VERIFY )
{
   hb_retc( (char*) FtsVersion() );
}

HB_FUNC( HS_VERSION )
{
   hb_retc( (char*) FtsVersion() );
}
