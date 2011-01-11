/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 */

#include <windows.h>
#include "Sde7.h"
#include "hbapirdd.h"

# if !defined( EVAL_DATE)
#define  EVAL_DATE 4
#endif
#define RDD_VERSION   "ApolloRDD v1.10 for xHarbour Builder"

#define SIX_MAX_FIELD_NAME      10
#define SIX_MAX_TAG_NAME        10
#define SIX_MAX_KEY_LENGTH     256

#define EVENT_PREUSE        1
#define EVENT_POSTUSE       2
#define EVENT_UPDATE	    3
#define EVENT_APPEND        4
#define EVENT_DELETE        5
#define EVENT_RECALL        6
#define EVENT_PACK          7
#define EVENT_ZAP           8
#define EVENT_PUT           9
#define EVENT_GET          10
#define EVENT_PRECLOSE     11
#define EVENT_POSTCLOSE    12

#define SIX_FLOAT           1
#define SIX_INTEGER         2
#define SIX_SHORTDATE       3
#define SIX_VARCHAR         4

/*
*  SIX WORKAREA
*  --------
*  The Workarea Structure of Apollo RDD
*
*/


typedef struct _SIXAREA_
{

   AREA area;

   /*
   *  SIX's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   USHORT uiSxArea;              /* Internal area number */
   char * szDataFileName;        /* Name of data file */
   USHORT uiRecordLen;           /* Size of record */
   USHORT * pFieldOffset;        /* Pointer to field offset array */
   BYTE * pRecord;               /* Buffer of record data */
   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */
   BOOL fShared;                 /* Shared file */
   BOOL fReadonly;               /* Read only file */
   BOOL fFLocked;                /* TRUE if file is locked */
   BOOL fTurbo;
   BOOL fFltOptimized;
   USHORT iOrdCurrent;
   USHORT iFltCurrent;
   int iFileType;                /* ntx/cdx/nsx */
   BOOL fMemoBinary;
   BOOL fTriggerActive;
   char cTriggerName[20];
} SIXAREA;

typedef SIXAREA * SIXAREAP;
