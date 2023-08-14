/*
* SQLRDD C Header
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#ifndef SQLRDD_H

#define SQLRDD_H

#include "compat.h"

#include "hbsetup.h"
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "sqlrdd.ch"

#define SUPERTABLE                  ( &sqlrddSuper )

/*
*  SQL WORKAREA
*/

typedef struct _SQLAREA
{
   AREA area;

   /*
   *  SQLRDD's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below
   */
   PHB_CODEPAGE cdPageCnv; /* Area's codepage convert pointer */
   char * szDataFileName;  /* file name */
   LONG hOrdCurrent;       /* current index order */
   BOOL shared;
   BOOL readonly;          /* only SELECT allowed */
   BOOL creating;          /* TRUE when creating table */
   BOOL firstinteract;     /* TRUE when workarea was not used yet */
   BOOL isam;              /* ISAM Simulator ? */
   BOOL wasdel;
   BOOL initialized;       /* Workarea Initialization done */
   BOOL sqlfilter;         /* SET FILTER converted to SQL */

   PHB_ITEM oWorkArea;      /* SQL Workarea object */
   PHB_ITEM aInfo;          /* Status array */
   PHB_ITEM aBuffer;        /* Record buffer */
   PHB_ITEM aOrders;        /* Indexes */
   PHB_ITEM aStruct;        /* Table xBase structure */
   PHB_ITEM aLocked;        /* Locked lines */
   PHB_ITEM aCreate;        /* Structure received by dbCreate() */
   PHB_ITEM aCache;         /* Workarea recordset cache */
   PHB_ITEM aOldBuffer;     /* Last workarea buffer */
   PHB_ITEM aEmptyBuff;     /* Empty buffer to be in eof()+1 */
   PHB_ITEM aSelectList;

   HB_ULONG ulhRecno;          /* Recno position in field list */
   HB_ULONG ulhDeleted;        /* Deleted position in field list */

   int * uiBufferIndex;     /* Field offset in fields array */
   int * uiFieldList;       /* Keeps a field list for SELECT statements */
   int iFieldListStatus;    /* field list status - see sqlprototypes.h */

   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */
   char editMask[MAX_FIELDS];    /* Flags if a column was updated - must be cleared on every GO_COLD - USED BY ODBCRDD */

} SQLAREA;

typedef SQLAREA * LPSQLAREA;

#ifndef SQLAREAP
#define SQLAREAP LPSQLAREA
#endif

/* prototypes */

void commonError( AREAP ThisDb, USHORT uiGenCode, USHORT uiSubCode, char* filename );

#endif
