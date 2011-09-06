/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    Record Map filter - header file
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
 *
 */

#ifndef HB_RDDRM_H_
#define HB_RDDRM_H_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#ifndef HB_CDP_SUPPORT_OFF
#include "hbapicdp.h"
#endif

#include "machsix.ch"

HB_EXTERN_BEGIN

#ifdef __XHARBOUR__

#define __PRG_SOURCE__  __FILE__
#undef  HB_PRG_PCODE_VER
#define HB_PRG_PCODE_VER HB_PCODE_VER

#define HB_FALSE        0
#define HB_TRUE         (!0)

#define HB_LONG         LONG
#define HB_ULONG        ULONG

typedef unsigned char   HB_BYTE;
typedef int             HB_BOOL;
typedef unsigned short  HB_USHORT;
typedef ULONG           HB_SIZE;

#define hb_rddIsDerivedFrom( rddID, superID )   ( (rddID) == (superID) )

#define hb_rddInheritEx( pTable, pSubTable, pSuperTable, szDrvName, puiSuperRddId ) \
          hb_rddInherit( pTable, pSubTable, pSuperTable, szDrvName )

#endif /* __XHARBOUR__ */

#define HB_RMITEM_BITSIZE     15
#define HB_RMITEM_SIZE        (1<<HB_RMITEM_BITSIZE)
#define HB_RMITEM_DUMMY       0xFFFFFFFFL

#define RM_OPT_NONE           DBOI_OPTIMIZED_NONE
#define RM_OPT_PART           DBOI_OPTIMIZED_PART
#define RM_OPT_FULL           DBOI_OPTIMIZED_FULL
/*#define RM_OPT_EMPTY        0x03*/
/*#define RM_OPT_FILL         0x04*/

#define M6FILE_SIGNATURE      4560
#define RMFILE_SIGNATURE      0x4D524248L /* "HBRM" */
#define RMFILE_MAXEXPSIZE     0xFFFF


#define HB_RM_ITEM_NO(rec)    ((rec)>>(3+HB_RMITEM_BITSIZE))
#define HB_RM_ITEM_POS(rec)   (((rec)>>3) & (HB_RMITEM_SIZE-1))

#define RM_TYPE_MAYBE   0x01
#define RM_TYPE_EXACT   0x02
#define RM_TYPE_COMPLEX 0x03

#define RM_RDD_MAX      8
#define HB_RMLST_ALLOC  32

#define SELF_RMFILTER(p)      ((PHB_RMFILTER) ((p)->dbfi.lpvCargo))
#define SELF_RMSET(p, f)      do { \
                                 if( (f) != NULL ) \
                                 { \
                                    ((PHB_RMFILTER)(f))->iArea = (p)->uiArea; \
                                 } \
                                 (p)->dbfi.lpvCargo = ( void * )(f); \
                                 (p)->dbfi.fFilter = (f) != NULL || (p)->dbfi.itmCobExpr != NULL; \
                              } while( 0 )
#define SELF_RMRELEASE(p)     do { \
                                 PHB_RMFILTER _pRM = SELF_RMFILTER(p); \
                                 if( _pRM ) \
                                 { \
                                    _pRM->iArea = 0; \
                                    hb_rmDestroy( _pRM ); \
                                    (p)->dbfi.lpvCargo = NULL; \
                                    (p)->dbfi.fFilter = (p)->dbfi.itmCobExpr != NULL; \
                                 } \
                              } while( 0 )
#define SELF_RMDETACH(p)      do { \
                                 PHB_RMFILTER _pRM = SELF_RMFILTER(p); \
                                 if( _pRM ) \
                                 { \
                                    _pRM->iArea = 0; \
                                    (p)->dbfi.lpvCargo = NULL; \
                                    (p)->dbfi.fFilter = (p)->dbfi.itmCobExpr != NULL; \
                                 } \
                              } while( 0 )

typedef struct _HB_RMITEM
{
   HB_ULONG    ulTmpBlock; /* number of block in temporary file with record map */
   HB_BYTE *   pRecMap;    /* pointer to current record map */
} HB_RMITEM;
typedef HB_RMITEM * PHB_RMITEM;

typedef struct _HB_RMFILTER
{
   int         iHandle;   /* RM filter handle */
   HB_FHANDLE  hFile;     /* file handle to record map buffer */
   int         iItems;    /* number of record map items */
   HB_ULONG    ulRecords; /* maximum number of records in the reocrd map */
   HB_ULONG    ulPos;     /* the position in filter for SIX m6_*() emulation */
   HB_BOOL     fLocked;   /* lock the filter against destroy in AND/OR/XOR operations */
   int         iArea;     /* workarea number */
   int         iType;     /* filter status RM_TYPE_* */
   int         iOptLvl;   /* optimization level */
   PHB_ITEM    pExpr;     /* filter expression */
   PHB_ITEM    pNonExpr;  /* Non-index part of filter expression */
   PHB_RMITEM  *pRMItems; /* array of record map items with good records */
   PHB_RMITEM  *pRMmaybe; /* array of record map items with maybe's records */
} HB_RMFILTER;
typedef HB_RMFILTER * PHB_RMFILTER;

typedef struct
{
   int   iSize;
   int   iCount;
   PHB_RMFILTER *pRMFilters;
} HB_RMBAG;
typedef HB_RMBAG * PHB_RMBAG;

typedef struct _HB_RMFILE
{
   HB_BYTE  hdrSig[4];     /* M6 filter file signature */
   HB_BYTE  recNum[4];     /* number of records in filter */
   HB_BYTE  currPos[4];    /* current possition in filter */
   HB_BYTE  expSize[4];    /* size of expression */
   HB_BYTE  nExpSize[4];   /* size of non opt. expression */
   HB_BYTE  optLvl[1];     /* optimization level 0, 1 or 2 */
   HB_BYTE  hasMB[1];      /* has maybe records 0, 1 */
   HB_BYTE  filler[2];     /* unused */
} HB_RMFILE;
typedef HB_RMFILE * PHB_RMFILE;

/* API functions */
int hb_rmGetError( void );
void hb_rmSetError( int iError );
HB_ULONG hb_rmGetRecords( void );
void hb_rmSetRecords( HB_ULONG ulRecords );
HB_BOOL hb_rmIsFilter( int iHandle );
PHB_RMFILTER hb_rmGetFilterPtr( int iHandle );
PHB_RMFILTER hb_rmCreate( HB_ULONG ulRecords );
void hb_rmDestroy( PHB_RMFILTER pRM );
void hb_rmDestroyAll( void );
void hb_rmFill( PHB_RMFILTER pRM );
void hb_rmClear( PHB_RMFILTER pRM );
void hb_rmFillMB( PHB_RMFILTER pRM );
void hb_rmClearMB( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmMakeMB( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmDup( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmReSize( PHB_RMFILTER pRM, HB_ULONG ulRecords );
PHB_RMFILTER hb_rmOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmAND( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmXOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmNOT( PHB_RMFILTER pRM );
void hb_rmSetRecord( PHB_RMFILTER pRM, HB_ULONG ulRec );
void hb_rmClearRecord( PHB_RMFILTER pRM, HB_ULONG ulRec );
HB_BOOL hb_rmTestRecord( PHB_RMFILTER pRM, HB_ULONG ulRec );
HB_BOOL hb_rmHasMB( PHB_RMFILTER pRM );
HB_ULONG hb_rmNextRecordMB( PHB_RMFILTER pRM, HB_ULONG ulRec );
HB_ULONG hb_rmNextRecord( PHB_RMFILTER pRM, HB_ULONG ulRec );
HB_ULONG hb_rmPrevRecord( PHB_RMFILTER pRM, HB_ULONG ulRec );
HB_ULONG hb_rmCountRecords( PHB_RMFILTER pRM );
HB_ULONG hb_rmRecordPos( PHB_RMFILTER pRM, HB_ULONG ulRecord );

PHB_RMFILTER hb_rmRestore( const char * szFile );
HB_BOOL hb_rmSave( PHB_RMFILTER pRM, const char * szFile );

/*
 * WorkAREA methods - IMHO they should be a part of lprfsHost so we can
 * create common RM filters code for different RDD
 */

void hb_rmDetach( PHB_RMFILTER pRM );

void hb_rmSetRddID( HB_USHORT uiRddId );
void hb_rmDelRddID( HB_USHORT uiRddId );
void * hb_rmGetRMAreaPointer( void );

PHB_RMFILTER hb_rmNewQuery( AREAP pArea, PHB_ITEM pFilterText );

PHB_RMFILTER hb_rmGetAreaFilter( void );
PHB_RMFILTER hb_rmReplaceAreaFilter( PHB_RMFILTER pRM );
HB_BOOL hb_rmSetAreaFilter( PHB_RMFILTER pRM );

HB_ULONG hb_rmSetLoHi( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pItmLo, PHB_ITEM pItmHi, PHB_ITEM pTag, PHB_ITEM pBag );
HB_ULONG hb_rmMaybeEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock );
HB_ULONG hb_rmDbEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock );
void hb_rmDoLinear( AREAP pArea );

int hb_rmqOptLevel( AREAP pArea, PHB_ITEM pQuery );
PHB_ITEM hb_rmqNonOptExpression( AREAP pArea, PHB_ITEM pQuery );
PHB_RMFILTER hb_rmqBuildQRM( AREAP pArea, PHB_ITEM pQuery );

#ifndef RMAREAP
#define RMAREAP AREAP
#endif

#define SUPERTABLE                         ( &rmSuper )


HB_EXTERN_END

#endif /* HB_RDDRM_H_ */
