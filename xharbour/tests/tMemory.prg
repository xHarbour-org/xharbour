Function Main()

   Alert( Memory(1) )

RETURN NIL


//------------------------------------------------------------------------
#ifndef __HARBOUR__
   #include "Fivewin.ch"
   #include "struct.ch"
   #include "DLL.CH"
#endif

#DEFINE MEM_TotalPhys            1
#DEFINE MEM_AvailPhys            2
#DEFINE MEM_TotalPageFile        3
#DEFINE MEM_AvailPageFile        4
#DEFINE MEM_TotalVirtual         5
#DEFINE MEM_AvailVirtual         6

//--------------------------------------------------------------------
Function Memory( n )

   LOCAL nRetu

   #ifdef __HARBOUR__
      #pragma BEGINDUMP
         #include "windows.h"
      #pragma STOPDUMP

      nRetu := HB_INLINE( n )
      {
         MEMORYSTATUS mst;
         long n = hb_parnl(1);

         mst.dwLength = sizeof( MEMORYSTATUS );
         GlobalMemoryStatus( &mst );

         switch( n )
         {
            case 1:  hb_retnl( mst.dwTotalPhys / (1024*1024)    ) ; break;
            case 2:  hb_retnl( mst.dwTotalPageFile / (1024*1024) ) ; break;
            case 3:  hb_retnl( mst.dwAvailPageFile / (1024*1024) ) ; break;
            default: hb_retnl( 0 )                   ;
         }
      }

   #else

       LOCAL oMemory

       STRUCT oMemory
          MEMBER m1 AS LONG  // nSize
          MEMBER m2 AS LONG  // Memory Load
          MEMBER m3 AS LONG  // Total Physical
          MEMBER m4 AS LONG  // Available Physical
          MEMBER m5 AS LONG  // Total Page File
          MEMBER m6 AS LONG  // Available Page File
          MEMBER m7 AS LONG  // Total Virtual
          MEMBER m8 AS LONG  // Available Virtual
       ENDSTRUCT

       oMemory:m1 = oMemory:SizeOf()
       MemStat( oMemory:cBuffer )

       DO CASE
          CASE n=1 ; nRetu:=Round( oMemory:m3 / (1024*1024) ,0 )
          CASE n=2 ; nRetu:=Round( oMemory:m4 / (1024*1024) ,0 )
          CASE n=3 ; nRetu:=Round( oMemory:m5 / (1024*1024) ,0 )
          CASE n=4 ; nRetu:=Round( oMemory:m6 / (1024*1024) ,0 )
          CASE n=5 ; nRetu:=Round( oMemory:m7 / (1024*1024) ,0 )
          CASE n=6 ; nRetu:=Round( oMemory:m8 / (1024*1024) ,0 )
          OTHERWISE; nRetu:=0
       ENDCASE

   #endif

RETURN nRetu

#ifndef __HARBOUR__
   DLL32 STATIC FUNCTION MemStat( pMEMORY AS LPSTR ) AS VOID PASCAL FROM "GlobalMemoryStatus" LIB "KERNEL32.DLL"
#endif
