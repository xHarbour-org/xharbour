
extern void WG_error(LPCTSTR cError, LPCTSTR cWhere);
extern void WG_warning(LPCTSTR cError, LPCTSTR cWhere);

extern PHB_ITEM WG_GetObjectData( PHB_ITEM pObj, char* cData);
extern double WG_GetObjectDataDouble( PHB_ITEM pObj, char* cData );
extern int WG_GetObjectDataInteger( PHB_ITEM pObj, char* cData );
extern int WG_GetObjectDataLogical( PHB_ITEM pObj, char* cData );
extern long WG_GetObjectDataLong( PHB_ITEM pObj, char* cData );
extern char * WG_GetObjectDataString( PHB_ITEM pObj, char* cData );
extern char * WG_GetObjectDataStringPtr( PHB_ITEM pObj, char* cData );

extern void WG_SetObjectDataDate( PHB_ITEM pObj, char* cData, char * szDate );
extern void WG_SetObjectDataDouble( PHB_ITEM pObj, char* cData, double dNew );
extern void WG_SetObjectDataInteger( PHB_ITEM pObj, char* cData, int iNew );
extern void WG_SetObjectDataLogical( PHB_ITEM pObj, char* cData, BOOL lNew );
extern void WG_SetObjectDataLong( PHB_ITEM pObj, char* cData, long nNew );
extern void WG_SetObjectDataString( PHB_ITEM pObj, char* cData, char *cNew );
extern void WG_SetObjectData( PHB_ITEM pObj, char* cData, PHB_ITEM pNew );

#define WIN_CENTER_VERTICAL     2
#define WIN_CENTER_HORIZONTAL   4
