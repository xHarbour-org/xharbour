#include <stdio.h>
#include <windows.h>
#include <errno.h>

#include "hbapi.h"

unsigned long _doserrno = 0;

#if 0
   typedef union
   {
      BYTE *   pAsmData;                           /* The assembler bytes      */
      PHB_FUNC pFunPtr;                            /* The (dynamic) harbour
                                                      function                 */
   } ASM_CALL, * PASM_CALL;

   static BYTE prgFunction[] =
   {
      0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset Globals             */
      0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset symbols             */
      0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset pcode               */
      0xE8, 0x00, 0x00, 0x00, 0x00,  /* call near relative hb_vmExecute */
      0x83, 0xC4, 0x0C,              /* add esp, 12                     */
      0xC3                           /* ret near                        */
   };

   PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode ); /* Create a dynamic function*/
   void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address );
   void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext );
#endif

struct errentry {
        unsigned long oscode;           /* OS return value */
        int errnocode;  /* System V error code */
};

static struct errentry errtable[] = {
        {  ERROR_INVALID_FUNCTION,       EINVAL    },  /* 1 */
        {  ERROR_FILE_NOT_FOUND,         ENOENT    },  /* 2 */
        {  ERROR_PATH_NOT_FOUND,         ENOENT    },  /* 3 */
        {  ERROR_TOO_MANY_OPEN_FILES,    EMFILE    },  /* 4 */
        {  ERROR_ACCESS_DENIED,          EACCES    },  /* 5 */
        {  ERROR_INVALID_HANDLE,         EBADF     },  /* 6 */
        {  ERROR_ARENA_TRASHED,          ENOMEM    },  /* 7 */
        {  ERROR_NOT_ENOUGH_MEMORY,      ENOMEM    },  /* 8 */
        {  ERROR_INVALID_BLOCK,          ENOMEM    },  /* 9 */
        {  ERROR_BAD_ENVIRONMENT,        E2BIG     },  /* 10 */
        {  ERROR_BAD_FORMAT,             ENOEXEC   },  /* 11 */
        {  ERROR_INVALID_ACCESS,         EINVAL    },  /* 12 */
        {  ERROR_INVALID_DATA,           EINVAL    },  /* 13 */
        {  ERROR_INVALID_DRIVE,          ENOENT    },  /* 15 */
        {  ERROR_CURRENT_DIRECTORY,      EACCES    },  /* 16 */
        {  ERROR_NOT_SAME_DEVICE,        EXDEV     },  /* 17 */
        {  ERROR_NO_MORE_FILES,          ENOENT    },  /* 18 */
        {  ERROR_LOCK_VIOLATION,         EACCES    },  /* 33 */
        {  ERROR_BAD_NETPATH,            ENOENT    },  /* 53 */
        {  ERROR_NETWORK_ACCESS_DENIED,  EACCES    },  /* 65 */
        {  ERROR_BAD_NET_NAME,           ENOENT    },  /* 67 */
        {  ERROR_FILE_EXISTS,            EEXIST    },  /* 80 */
        {  ERROR_CANNOT_MAKE,            EACCES    },  /* 82 */
        {  ERROR_FAIL_I24,               EACCES    },  /* 83 */
        {  ERROR_INVALID_PARAMETER,      EINVAL    },  /* 87 */
        {  ERROR_NO_PROC_SLOTS,          EAGAIN    },  /* 89 */
        {  ERROR_DRIVE_LOCKED,           EACCES    },  /* 108 */
        {  ERROR_BROKEN_PIPE,            EPIPE     },  /* 109 */
        {  ERROR_DISK_FULL,              ENOSPC    },  /* 112 */
        {  ERROR_INVALID_TARGET_HANDLE,  EBADF     },  /* 114 */
        {  ERROR_INVALID_HANDLE,         EINVAL    },  /* 124 */
        {  ERROR_WAIT_NO_CHILDREN,       ECHILD    },  /* 128 */
        {  ERROR_CHILD_NOT_COMPLETE,     ECHILD    },  /* 129 */
        {  ERROR_DIRECT_ACCESS_HANDLE,   EBADF     },  /* 130 */
        {  ERROR_NEGATIVE_SEEK,          EINVAL    },  /* 131 */
        {  ERROR_SEEK_ON_DEVICE,         EACCES    },  /* 132 */
        {  ERROR_DIR_NOT_EMPTY,          ENOTEMPTY },  /* 145 */
        {  ERROR_NOT_LOCKED,             EACCES    },  /* 158 */
        {  ERROR_BAD_PATHNAME,           ENOENT    },  /* 161 */
        {  ERROR_MAX_THRDS_REACHED,      EAGAIN    },  /* 164 */
        {  ERROR_LOCK_FAILED,            EACCES    },  /* 167 */
        {  ERROR_ALREADY_EXISTS,         EEXIST    },  /* 183 */
        {  ERROR_FILENAME_EXCED_RANGE,   ENOENT    },  /* 206 */
        {  ERROR_NESTING_NOT_ALLOWED,    EAGAIN    },  /* 215 */
        {  ERROR_NOT_ENOUGH_QUOTA,       ENOMEM    }    /* 1816 */
};

/* size of the table */
#define ERRTABLESIZE (sizeof(errtable)/sizeof(errtable[0]))

/* The following two constants must be the minimum and maximum
   values in the (contiguous) range of Exec Failure errors. */
#define MIN_EXEC_ERROR ERROR_INVALID_STARTING_CODESEG
#define MAX_EXEC_ERROR ERROR_INFLOOP_IN_RELOC_CHAIN

/* These are the low and high value in the range of errors that are
   access violations */
#define MIN_EACCES_RANGE ERROR_WRITE_PROTECT
#define MAX_EACCES_RANGE ERROR_SHARING_BUFFER_EXCEEDED

#ifdef DEMO
   extern void hb_vmAtInit( void (*pFunc)(void), void *cargo );

   void xHB_DemoInit( void )
   {
      OutputDebugString( "Demo Startup\n" );

      if( hb_dynsymFind( "WINAPIVER" ) == NULL )
      {
         MessageBox( 0, (LPCSTR) "This application was build with the demo version of xHarbour Builder. \n \n Copyright (c) 2007 xHarbour.com Inc. \n http://www.xHarbour.com" , (LPCSTR) "xHarbour Builder Demo", MB_OK | MB_ICONINFORMATION ) ;
      }
   }

   void xHbSetInit( void )
   {
     hb_vmAtInit( xHB_DemoInit, NULL );
   }

   #pragma startup xHbSetInit

   //#pragma startup xHbStartDemo
   //#pragma exit    xHbEndDemo
#endif

unsigned int _inp(unsigned int port)
{
   __asm {
            mov edx,port    /* unsigned int = 32 bits -> mov edx,port;  unsigned short = 16 bits -> mov dx,port */
            in al,dx
            movzx eax,al    /* movzx eax,al -> return unsigned int;  movzx ax,al -> return unsigned short */
         }

  /* return value already in eax */
}

void _outp(unsigned int port, unsigned char value)
{
   __asm {
            mov edx,port
            mov al,value
            out dx,al
         }
}

/*
 * Wrap Microsoft _ftol() to Pelles C __ftol().
 */
extern int __cdecl __ftol(void);
int __declspec(naked) _ftol(void) { __asm jmp __ftol }

/*
 * Wrap Microsoft _allshr() to Pelles C.
 */
__int64 __declspec(naked) _allshr(void)
{
    /* input EDX:EAX (64 bit value) and ECX (shift amount) */
    __asm {
                cmp     ecx,64
                jae     retsign

    /* Handle shifts of between 0 and 31 bits */

                cmp     cl,32
                jae     over32
                shrd    eax,edx,cl
                sar     edx,cl
                ret

    /* Handle shifts of between 32 and 63 bits */
over32:         mov     eax,edx
                sar     edx,31
                and     cl,31
                sar     eax,cl
                ret

    /* Return double precision 0 or -1, depending on the sign of edx */

retsign:        sar     edx,31
                mov     eax,edx
                ret
    };
}
#undef isascii
int isascii( int c )
{
   return (unsigned int) c < 128;
}

void __cdecl _dosmaperr( unsigned long oserrno )
{
   int i;

   _doserrno = oserrno;        /* set _doserrno */

   /* check the table for the OS error code */
   for( i = 0; i < ERRTABLESIZE; ++i)
   {
      if( oserrno == errtable[i].oscode)
      {
         errno = errtable[i].errnocode;
         return;
      }
   }

   /* The error code wasn't in the table.  We check for a range of */
   /* EACCES errors or exec failure errors (ENOEXEC).  Otherwise   */
   /* EINVAL is returned.                                          */

   if( oserrno >= MIN_EACCES_RANGE && oserrno <= MAX_EACCES_RANGE )
   {
      errno = EACCES;
   }
   else if( oserrno >= MIN_EXEC_ERROR && oserrno <= MAX_EXEC_ERROR )
   {
      errno = ENOEXEC;
   }
   else
   {
      errno = EINVAL;
   }
}

#undef GetLargestConsoleWindowSize

typedef struct tagXCCCOORD
{
   union
   {
     DWORD dw;
     COORD co;
   };
} XCCCOORD;

WINBASEAPI DWORD WINAPI GetLargestConsoleWindowSize(HANDLE);

COORD xCCGetLargestConsoleWindowSize( HANDLE h )
{
   XCCCOORD coBuf;

   coBuf.dw = GetLargestConsoleWindowSize( h );

   return coBuf.co;
}

#if 0
   PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode )
   {
      PASM_CALL asmRet;

      asmRet = ( PASM_CALL ) hb_xgrab( sizeof( ASM_CALL ) );
      asmRet->pAsmData = ( BYTE * ) hb_xgrab( sizeof( prgFunction ) );
      memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                                 /* Copy new assembler code in */
   /* #if INTEL32 */

      //hb_hrbAsmPatch( asmRet->pAsmData, 1, NULL );       /* Insert pointer to globals */
      hb_hrbAsmPatch( asmRet->pAsmData, 6, pSymbols );   /* Insert pointer to symbols */
      hb_hrbAsmPatch( asmRet->pAsmData, 11, pCode );      /* Insert pointer to pcode */
      hb_hrbAsmPatchRelative( asmRet->pAsmData, 16, ( void * ) hb_vmExecute, 20 );
                                         /* Insert pointer to hb_vmExecute() */

   /* #elseif INTEL16 */
   /* #elseif MOTOROLA */
   /* #elseif ... */
   /* #endif */
      return asmRet;
   }
#endif

/* Patch an address of the dynamic function */
void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
{
/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

/* #if 32 bits and low byte first */
   ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}
