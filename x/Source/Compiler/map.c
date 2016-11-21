#include "common.h"

static inline unsigned long CalcKeyHash( const char *sKey )
{
	unsigned long ulHash = 5381;
   
	while ( *sKey )
   {
		ulHash = ( ( ulHash << 5 ) + ulHash ) + *sKey; // ulHash *= 33 + Char
      ++sKey;
	}
   
	return ulHash;
}

static inline PAIR * GetBucketPair( BUCKET *pBucket, const char *sKey )
{
	unsigned int uiPair, uiPairs;
	PAIR *pPair;
   
   assert( pBucket );
   
	uiPairs = pBucket->uiPairs;
	
   if ( uiPairs == 0 )
   {
		return NULL;
	}
   
	pPair = pBucket->pPairs;
	uiPair = 0;
	
   while ( uiPair < uiPairs )
   {
      assert( pPair );
      //assert( pPair->sKey );
      
      if ( strcmp( pPair->sKey, sKey ) == 0 )
      {
         return pPair;
      }
   
      pPair++;
      uiPair++;
   }
   
   return NULL;
}

MAP * NewMap( unsigned int uiBuckets )
{
	MAP *pMap;
	
	pMap = MAP_ALLOC( sizeof( MAP ) );
   
   assert( pMap );
   
	pMap->uiBuckets = uiBuckets;

	pMap->pBuckets  = MAP_ALLOC( uiBuckets * sizeof( BUCKET ) );
   assert( pMap->pBuckets );

   #ifdef MAP_KEEP_STATS
      pMap->uiEntries = 0;
      pMap->uiUsedBuckets = 0;
      pMap->uiGlobalCollisions = 0;
      pMap->uiHighestBucketCollisions = 0;
   #endif
   
	memset( pMap->pBuckets, 0, uiBuckets * sizeof( BUCKET ) );
   
	return pMap;
}

void DeleteMap( MAP *pMap )
{
	unsigned int uiBucket, uiBuckets, uiPair, uiPairs;
	BUCKET *pBucket;
	PAIR *pPair;

   assert( pMap );
   
   #ifdef MAP_KEEP_STATS
   printf( "Map: %p, used %li Bytes, had %i Entries, in %i Buckets (%i used), with %i Collisions (%i in the busiest Bucket) \n", pMap, ( pMap->uiBuckets + pMap->uiGlobalCollisions ) * sizeof( BUCKET ), pMap->uiEntries, pMap->uiBuckets, pMap->uiUsedBuckets, pMap->uiGlobalCollisions, pMap->uiHighestBucketCollisions );
   #endif
   
	uiBuckets = pMap->uiBuckets;
	pBucket   = pMap->pBuckets;
	uiBucket  = 0;
	
   while ( uiBucket < uiBuckets )
   {
		uiPairs = pBucket->uiPairs;
		pPair   = pBucket->pPairs;
		uiPair  = 0;
      
		while ( uiPair < uiPairs )
      {
			MAP_FREE_KEY_COPY( pPair->sKey );
			MAP_FREE_VALUE_COPY( pPair->pValue );
         
			pPair++;
			uiPair++;
		}
      
		MAP_FREE( pBucket->pPairs );
      
		pBucket++;
		uiBucket++;
	}
   
	MAP_FREE( pMap->pBuckets );
	MAP_FREE( pMap );
}

void * AddMapValue( MAP *pMap, const char *sKey, const void *pValue )
{
	unsigned int uiBucket;
	BUCKET *pBucket;
	PAIR *pPair;
	const void *pDroppedValue = NULL;
   
   assert( pMap );
   assert( sKey );
   assert( pValue );
      
	uiBucket = CalcKeyHash( sKey ) % pMap->uiBuckets;
	pBucket  = &( pMap->pBuckets[ uiBucket ] );
 
   #ifdef MAP_KEEP_STATS
      ++pMap->uiEntries;
   #endif
   
	if ( ( pPair = GetBucketPair( pBucket, sKey ) ) != NULL )
   {
      pDroppedValue = pPair->pValue;
      
      MAP_FREE_VALUE_COPY( pDropedValue );
      
      #ifdef MAP_KEEP_STATS
         --pMap->uiEntries;
      #endif
	}
   else
   {
	   if ( pBucket->uiPairs == 0 )
      {
		   pBucket->pPairs = MAP_ALLOC( sizeof( PAIR ) );
         assert( pBucket->pPairs );
      
		   pBucket->uiPairs = 1;
         pPair = &( pBucket->pPairs[ 0 ] );

         #ifdef MAP_KEEP_STATS
            ++( pMap->uiUsedBuckets );
         #endif
	   }
	   else
      {
		   pBucket->pPairs = MAP_REALLOC( pBucket->pPairs, ++( pBucket->uiPairs ) * sizeof( PAIR ) );
         assert( pBucket->pPairs );
      
         pPair = &( pBucket->pPairs[ pBucket->uiPairs - 1 ] );

         #ifdef MAP_KEEP_STATS
            ++( pMap->uiGlobalCollisions );
      
            if( pBucket->uiPairs - 1 > pMap->uiHighestBucketCollisions )
            {
               pMap->uiHighestBucketCollisions = pBucket->uiPairs - 1;
            }
         #endif
      }      
 	}
   
	pPair->sKey   = MAP_COPY_KEY_ON_PUT( sKey );
	pPair->pValue = MAP_COPY_VALUE_ON_PUT( pValue );
   
	return (void *) pDroppedValue;
}

void * GetMapValue( const MAP *pMap, const char *sKey )
{
	unsigned int uiBucket;
	BUCKET *pBucket;
	PAIR *pPair;

   assert( pMap );
   assert( sKey );
   
	uiBucket = CalcKeyHash( sKey ) % pMap->uiBuckets;
	pBucket  = &( pMap->pBuckets[ uiBucket ] );
	pPair    = GetBucketPair( pBucket, sKey );
   
	if ( pPair == NULL )
   {
		return NULL;
	}
   
   return (void *) MAP_COPY_VALUE_ON_GET( pPair->pValue );
}

void IterateMap( const MAP *pMap, MAP_INSPECTOR pMapInspector, void *pCargo )
{
	unsigned int uiBucket, uiBuckets, uiPair, uiPairs;
	BUCKET *pBucket;
	PAIR *pPair;

   assert( pMap );
   assert( pMapInspector );
   
	pBucket   = pMap->pBuckets;
	uiBuckets = pMap->uiBuckets;
	uiBucket  = 0;

	while ( uiBucket < uiBuckets )
   {
		pPair   = pBucket->pPairs;
		uiPairs = pBucket->uiPairs;
		uiPair  = 0;
      
		while ( uiPair < uiPairs )
      {
			pMapInspector( pPair->sKey, pPair->pValue, pCargo );
         
			pPair++;
			uiPair++;
		}
      
		pBucket++;
		uiBucket++;
	}
   
	return;
}

static void CountInspector( const char *sKey, const void *pValue, void *pCargo )
{
   (void) sKey;
   (void) pValue;
   
   ++( *( (unsigned long *) pCargo ) );
}

unsigned long CountMapEntries( const MAP *pMap )
{
   unsigned long ulCount = 0;
   
   IterateMap( pMap, CountInspector, (void *) &ulCount );
   
   return ulCount;
}

static void ListInspector( const char *sKey, const void *pValue, void *pCargo )
{
   (void) pCargo;
   printf( "ID: '%s' Value %p\n", sKey, pValue );
}

void ListMapEntries( const MAP *pMap )
{
   IterateMap( pMap, ListInspector, NULL );
}

