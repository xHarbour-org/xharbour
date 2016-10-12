/*
  FTS32.H - Fast Text Search (FTS) 32-bit Header File 
  ©1999 Vista Software - www.VistaSoftware.com 
  Last Updated: 09-14-99 
*/

/* Control special calling conventions and addressing modes */
#define FAR 	_far
#define FTSCALL __stdcall

#define TRUE               1     /* truth */
#define FALSE              0     /* falsehood */

/* Verify Types */
#define  VBEG  1                 /* verify at begining of string only */
#define  VEND  2                 /* verify at end of string only */
#define  VAND  3                 /* verify all tokens in target */
#define  VONE  4                 /* don't tokenize target */

/* FTS File Open Modes */
#define FTS_SHARE    0x0     /* SHARE */
#define FTS_EXCL     0x1     /* EXCLUSIVE */
#define FTS_RDONLY   0x2     /* READ-ONLY */

#define FTSifdelete  FTSisdelete /* permit old name */

/* FTS Error Codes */
#define FTS_CREATEFAIL    -1
#define FTS_MEMERR        -2
#define FTS_NULLPTR       -3
#define FTS_BADSEEK       -4
#define FTS_BADREAD       -5
#define FTS_BADWRITE      -6
#define FTS_RECBOUND      -7
#define FTS_ISDELETED     -8
#define FTS_NOTDELETED    -9
#define FTS_OPENERR       -10
#define FTS_INTERR        -11
#define FTS_NORECS        -13
#define FTS_BADPARMS      -16
#define FTS_NOMOREHANDLES -17
#define FTS_BADHANDLE     -18
#define FTS_BADIHANDLE    -19
#define FTS_LOCKFAILED    -20
#define FTS_NOMORELOCKS   -21
#define FTS_CANNOTUNLOCK  -22
#define FTS_BADCOMMIT     -23


/* TYPES */
typedef  long FTSHANDLE;

/* function prototypes */
#ifdef __cplusplus
extern "C" {
#endif

long      FTSCALL  FtsAdd      ( FTSHANDLE, unsigned char FAR * );
short     FTSCALL  FtsClose    ( FTSHANDLE );
FTSHANDLE FTSCALL  FtsCreate   ( char FAR *, short, short, char, short );
short     FTSCALL  FtsDelete   ( FTSHANDLE, long );
int       FTSCALL  FtsSet      ( FTSHANDLE, unsigned char FAR * );
short     FTSCALL  FtsIsDelete ( FTSHANDLE, long );
FTSHANDLE FTSCALL  FtsOpen     ( char FAR *, short, short );
long      FTSCALL  FtsNextRec  ( FTSHANDLE );
long      FTSCALL  FtsNumRecs  ( FTSHANDLE );
short     FTSCALL  FtsReplace  ( FTSHANDLE, unsigned char FAR *, long );
short     FTSCALL  FtsUnDelete ( FTSHANDLE, long );
short     FTSCALL  FtsVerify   ( FTSHANDLE, unsigned char FAR *, unsigned char FAR *, short );
char FAR * FTSCALL  FtsVersion ( void );

#ifdef __cplusplus
}
#endif

