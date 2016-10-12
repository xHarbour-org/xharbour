#ifndef MAP_DEFINED
#define MAP_DEFINED

#ifdef __cplusplus
extern "C"
{
#endif
   
   #define MAP_KEEP_STATS
   
   // Private
   
   typedef struct
   {
      const char *sKey;
      const void *pValue;
   } PAIR;
   
   typedef struct
   {
      unsigned short uiPairs;
      PAIR *pPairs;
   } BUCKET;
   
   typedef struct _MAP
   {
      unsigned int uiBuckets;
      BUCKET *pBuckets;
      
      #ifdef MAP_KEEP_STATS
        unsigned int uiEntries;
        unsigned int uiUsedBuckets;
        unsigned short uiGlobalCollisions;
        unsigned short uiHighestBucketCollisions;
      #endif
   } MAP;
   
   static inline PAIR * GetBucketPair( BUCKET *pBucket, const char *sKey );
   
   static inline unsigned long CalcKeyHash( const char *sKey );

   // Public Interface
   
   #define MAP_ALLOC( size )               malloc( size )
   #define MAP_REALLOC( pointer, size )    realloc( pointer, size )
   #define MAP_FREE( pointer )             free( pointer )
   
   // Default to no copy
   #define MAP_COPY_KEY_ON_PUT( sKey )     sKey
   #define MAP_COPY_VALUE_ON_PUT( pValue ) pValue

   #define MAP_COPY_KEY_ON_GET( sKey )     sKey
   #define MAP_COPY_VALUE_ON_GET( pValue ) pValue

   #define MAP_FREE_KEY_COPY( sKey )
   #define MAP_FREE_VALUE_COPY( pValue )   
   
   MAP * NewMap( unsigned int uiSize );
   void  DeleteMap( MAP *pMap );

   void * AddMapValue( MAP *pMap, const char *sKey, const void *pValue );
   void * GetMapValue( const MAP *pMap, const char *sKey );

   // Optional utilities.
   typedef void( *MAP_INSPECTOR )( const char *sKey, const void *pValue, void *pCargo );
   void IterateMap( const MAP *pMap, MAP_INSPECTOR pMapInspector, void *pCargoj );
   unsigned long CountMapEntries( const MAP *pMap );
   void ListMapEntries( const MAP *pMap );

#ifdef __cplusplus
}
#endif

#endif