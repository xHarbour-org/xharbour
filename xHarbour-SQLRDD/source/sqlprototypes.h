/*
* SQLRDD Connection Classes C Internal header
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* Not to be distributed
* All Rights Reserved
*/

HB_FUNC_EXTERN( SR_DESERIALIZE );

PHB_ITEM sr_escapeNumber( char *FromBuffer, HB_SIZE len, HB_SIZE dec, PHB_ITEM pRet );
PHB_ITEM sr_getBaseLang( PHB_ITEM );
PHB_ITEM sr_getSecondLang( PHB_ITEM );
PHB_ITEM sr_getRootLang( PHB_ITEM );
BOOL HB_EXPORT sr_lSerializedAsString( void );
BOOL HB_EXPORT sr_lHideRecno( void );
BOOL HB_EXPORT sr_lHideHistoric( void );
BOOL HB_EXPORT sr_isMultilang( void );
BOOL HB_EXPORT sr_isShutdownProcess( void );
BOOL HB_EXPORT sr_UseDeleteds( void );
BOOL HB_EXPORT sr_lSerializeArrayAsJson( void );
BOOL HB_EXPORT sr_lsql2008newTypes( void );
BOOL HB_EXPORT sr_iOldPgsBehavior( void ) ;
BOOL HB_EXPORT sr_fShortasNum( void );
#ifdef SQLRDD_COMPAT_PRE_1_1
   BOOL hb_arraySetNL( PHB_ITEM pArray, ULONG ulIndex, LONG ulVal );
   BOOL hb_arraySetL( PHB_ITEM pArray, ULONG ulIndex, BOOL lVal );
#endif
BOOL iTemCompEqual( PHB_ITEM pItem1, PHB_ITEM pItem2 );
BOOL hb_itemEmpty( PHB_ITEM pItem );
BOOL sr_GoTopOnScope( void );

// SOme commom defines to ALL SQL RDDs

#define FIELD_LIST_LEARNING         0
#define FIELD_LIST_STABLE           1
#define FIELD_LIST_CHANGED          2
#define FIELD_LIST_NEW_VALUE_READ   3

