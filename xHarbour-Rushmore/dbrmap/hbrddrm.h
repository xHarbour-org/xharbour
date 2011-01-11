/*
 * $Id$
 */

/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Przemyslaw Czerpak Przemek@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
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

#define HB_RMITEM_BITSIZE     15
#define HB_RMITEM_SIZE        (1<<HB_RMITEM_BITSIZE)
#define HB_RMITEM_DUMMY       0xFFFFFFFFL

#define RM_OPT_NONE	      DBOI_OPTIMIZED_NONE
#define RM_OPT_PART	      DBOI_OPTIMIZED_PART
#define RM_OPT_FULL	      DBOI_OPTIMIZED_FULL
/*#define RM_OPT_EMPTY	      0x03*/
/*#define RM_OPT_FILL	      0x04*/

#define M6FILE_SIGNATURE      4560
#define RMFILE_SIGNATURE      0x4D524248L /* "HBRM" */
#define RMFILE_MAXEXPSIZE     0xFFFF


#define HB_RM_ITEM_NO(rec)    ((rec)>>(3+HB_RMITEM_BITSIZE))
#define HB_RM_ITEM_POS(rec)   (((rec)>>3) & (HB_RMITEM_SIZE-1))

#define RM_TYPE_MAYBE	0x01
#define RM_TYPE_EXACT   0x02
#define RM_TYPE_COMPLEX 0x03

#define HB_RMLST_ALLOC  32

#define SELF_RMFILTER(p)      ((PHB_RMFILTER) ((p)->dbfi.lpvCargo))
#define SELF_RMSET(p, f)      do { \
                                 if( (f) != NULL ) \
                                 { \
                                    ((PHB_RMFILTER)(f))->iArea = (p)->uiArea; \
                                 } \
                                 (p)->dbfi.lpvCargo = ( void * )(f); \
                                 (p)->dbfi.fFilter = (f) != NULL || pArea->dbfi.itmCobExpr != NULL; \
                              } while( 0 )
#define SELF_RMRELEASE(p)     do { \
                                 PHB_RMFILTER pRM = SELF_RMFILTER(p); \
                                 if( pRM ) \
                                 { \
                                    pRM->iArea = 0; \
                                    hb_rmDestroy( pRM ); \
                                    (p)->dbfi.lpvCargo = NULL; \
                                    (p)->dbfi.fFilter = pArea->dbfi.itmCobExpr != NULL; \
                                 } \
                              } while( 0 )
#define SELF_RMDETACH(p)      do { \
                                 PHB_RMFILTER pRM = SELF_RMFILTER(p); \
                                 if( pRM ) \
                                 { \
                                    pRM->iArea = 0; \
                                    (p)->dbfi.lpvCargo = NULL; \
                                    (p)->dbfi.fFilter = pArea->dbfi.itmCobExpr != NULL; \
                                 } \
                              } while( 0 )

typedef struct _HB_RMITEM
{
   ULONG    ulTmpBlock; /* number of block in temporary file with record map */
   BYTE *   pRecMap;    /* pointer to current record map */
} HB_RMITEM;
typedef HB_RMITEM * PHB_RMITEM;

typedef struct _HB_RMFILTER
{
   int         iHandle;   /* RM filter handle */
   FHANDLE     hFile;     /* file handle to record map buffer */
   int         iItems;    /* number of record map items */
   ULONG       ulRecords; /* maximum number of records in the reocrd map */
   ULONG       ulPos;     /* the position in filter for SIX m6_*() emulation */
   BOOL        fLocked;   /* lock the filter against destroy in AND/OR/XOR operations */
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
   BYTE     hdrSig[4];        /* M6 filter file signature */
   BYTE     recNum[4];        /* number of records in filter */
   BYTE     currPos[4];       /* current possition in filter */
   BYTE     expSize[4];       /* size of expression */
   BYTE     nExpSize[4];      /* size of non opt. expression */
   BYTE     optLvl[1];        /* optimization level 0, 1 or 2 */
   BYTE     hasMB[1];         /* has maybe records 0, 1 */
   BYTE     filler[2];        /* unused */
} HB_RMFILE;
typedef HB_RMFILE * PHB_RMFILE;

/* API functions */
int hb_rmGetError( void );
void hb_rmSetError( int iError );
BOOL hb_rmIsFilter( int iHandle );
PHB_RMFILTER hb_rmGetFilterPtr( int iHandle );
PHB_RMFILTER hb_rmCreate( ULONG ulRecords );
void hb_rmDestroy( PHB_RMFILTER pRM );
void hb_rmDestroyAll( void );
void hb_rmFill( PHB_RMFILTER pRM );
void hb_rmClear( PHB_RMFILTER pRM );
void hb_rmFillMB( PHB_RMFILTER pRM );
void hb_rmClearMB( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmMakeMB( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmDup( PHB_RMFILTER pRM );
PHB_RMFILTER hb_rmReSize( PHB_RMFILTER pRM, ULONG ulRecords );
PHB_RMFILTER hb_rmOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmAND( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmXOR( PHB_RMFILTER pRM1, PHB_RMFILTER pRM2 );
PHB_RMFILTER hb_rmNOT( PHB_RMFILTER pRM );
void hb_rmSetRecord( PHB_RMFILTER pRM, ULONG ulRec );
void hb_rmClearRecord( PHB_RMFILTER pRM, ULONG ulRec );
BOOL hb_rmTestRecord( PHB_RMFILTER pRM, ULONG ulRec );
BOOL hb_rmHasMB( PHB_RMFILTER pRM );
ULONG hb_rmNextRecordMB( PHB_RMFILTER pRM, ULONG ulRec );
ULONG hb_rmNextRecord( PHB_RMFILTER pRM, ULONG ulRec );
ULONG hb_rmPrevRecord( PHB_RMFILTER pRM, ULONG ulRec );
ULONG hb_rmCountRecords( PHB_RMFILTER pRM );
ULONG hb_rmRecordPos( PHB_RMFILTER pRM, ULONG ulRecord );

PHB_RMFILTER hb_rmRestore( const char * szFile );
BOOL hb_rmSave( PHB_RMFILTER pRM, const char * szFile );

/*
 * WorkAREA methods - IMHO they should be a part of lprfsHost so we can
 * create common RM filters code for different RDD
 */

void hb_rmDetach( PHB_RMFILTER pRM );

void * hb_rmGetRMAreaPointer( void );

PHB_RMFILTER hb_rmNewQuery( AREAP pArea, PHB_ITEM pFilterText );

PHB_RMFILTER hb_rmGetAreaFilter( void );
PHB_RMFILTER hb_rmReplaceAreaFilter( PHB_RMFILTER pRM );
BOOL hb_rmSetAreaFilter( PHB_RMFILTER pRM );

ULONG hb_rmSetLoHi( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pItmLo, PHB_ITEM pItmHi, PHB_ITEM pTag, PHB_ITEM pBag );
ULONG hb_rmMaybeEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock );
ULONG hb_rmDbEval( AREAP pArea, PHB_RMFILTER pRM, PHB_ITEM pCodeBlock );
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
