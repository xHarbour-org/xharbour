/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_COMP_H_
#define HB_COMP_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "hberrors.h"
#include "hbpp.h"
#include "hbver.h"
#include "hbexprop.h"
#include "hbpcode.h"
#include "hbhash.h"

HB_EXTERN_BEGIN

/* compiler related declarations */

/* Output types */
typedef enum
{
   LANG_C,                      /* C language (by default) <file.c> */
   LANG_OBJ32,                  /* DOS/Windows 32 bits <file.obj> */
   LANG_PORT_OBJ,               /* Portable objects <file.hrb> */
   LANG_OBJ_MODULE              /* Platform dependant object module <file.obj> */
} LANGUAGES;                    /* supported Harbour output languages */

/* #include support */
typedef struct
{
   FILE * handle;               /* handle of the opened file */
   void * pBuffer;              /* file buffer */
   int    iBuffer;              /* current position in file buffer */
   int    lenBuffer;            /* current length of data in file buffer */
   char * szFileName;           /* name of the file */
   void * pPrev;                /* pointer to the previous opened file */
   void * pNext;                /* pointer to the next opened file */
   int    iLine;                /* currently processed line number */
} _FILE, * PFILE;               /* structure to hold an opened PRG or CH */

/* structure to control several opened PRGs and CHs */
typedef struct
{
   PFILE pLast;                 /* pointer to the last opened file */
   int   iFiles;                /* number of files currently opened */
} FILES;

struct _COMCLASS;    /* forward declaration */

/* Declared Function/Method support structure */
typedef struct _COMDECLARED
{
   char                * szName;              /* the name of the symbol */
   BYTE                  cType;
   USHORT                iParamCount;
   BYTE                * cParamTypes;
   union
   {
      struct _COMCLASS *pClass;
      struct _ENUMDEF  *pEnum;
   } Extended;
   struct _COMCLASS    * ( * pParamClasses );
   struct _COMDECLARED * pNext;               /* pointer to the next declared function */
   BOOL bFree;
} COMDECLARED, * PCOMDECLARED;

/* Declared Class support structure */
typedef struct _COMCLASS
{
   char             * szName;
   PCOMDECLARED       pMethod;
   PCOMDECLARED       pLastMethod;
   struct _COMCLASS * pNext;
} COMCLASS, * PCOMCLASS;

typedef struct _ENUMDEF
{
   char            * szName;                  /* Set name */
   ULONG             lMembers;
   char           ** pMembers;
   struct _ENUMDEF * pNext;
} ENUMDEF, *PENUMDEF;

/* locals, static, public variables support */
typedef struct _VAR
{
   char *    szName;               /* variable name */
   char *    szAlias;              /* variable alias namespace (NULL for GLOBALS, "" for EXTERNGLOBALS) */
   int       iUsed;                /* number of times used */
   int       iDeclLine;            /* declaration line number */
   BYTE      cType;                /* optional strong typing */
   union
   {
      PCOMCLASS pClass;
      PENUMDEF  pEnum;
   } Extended;
   struct _VAR * pNext;            /* pointer to next defined variable */
} VAR, * PVAR;

/* pcode chunks bytes size */
#define HB_PCODE_CHUNK   100

#define HB_MAX_SWITCHES 16

struct __NAMESPACE;

/* structure to hold a Clipper defined function */
typedef struct __FUNC
{
   char *         szName;                 /* name of a defined Clipper function */
   struct __NAMESPACE *pNamespace;
   HB_SYMBOLSCOPE cScope;                 /* scope of a defined Clipper function */
   BYTE           bFlags;                 /* some flags we may need */
   USHORT         wParamCount;            /* number of declared parameters */
   USHORT         wParamNum;              /* current parameter number */
   PVAR           pLocals;                /* pointer to local variables list */
   PVAR           pStatics;               /* pointer to static variables list */
   PVAR           pFields;                /* pointer to fields variables list */
   PVAR           pMemvars;               /* pointer to memvar variables list */
   PVAR           pPrivates;              /* pointer to private variables list */
   PENUMDEF       pEnums;                 /* pointer to Enumeration definitions list */
   BYTE *         pCode;                  /* pointer to a memory block where pcode is stored */
   HB_SIZE        lPCodeSize;             /* total memory size for pcode */
   HB_SIZE        lPCodePos;              /* actual pcode offset */
   int            iStaticsBase;           /* base for this function statics */
   HB_SIZE *      pNOOPs;                 /* pointer to the NOOP array */
   HB_SIZE *      pJumps;                 /* pointer to the Jumps array */
   HB_SIZE        iNOOPs;                 /* NOOPs Counter */
   HB_SIZE        iJumps;                 /* Jumps Counter */
   BYTE *         pStack;                 /* Compile Time Stack */
   USHORT         iStackSize;             /* Compile Time Stack size */
   int            iStackIndex;            /* Compile Time Stack index */
   PCOMDECLARED   pStackFunctions[ 8 ];   /* Declared Functions on the Compile Time Stack */
   int            iStackFunctions;        /* Index into DEclared Functions on Compile Time Stack */
   PCOMCLASS      pStackClasses[ 8 ];     /* Declared Classes on the Compile Time Stack */
   int            iStackClasses;          /* Index into Declared Classes on Compile Time Stack */
   struct __FUNC * pOwner;                /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;                 /* pointer to the next defined function */
} _FUNC, * PFUNCTION;

typedef struct __FUNCALL
{
   char *             szName;
   void *             Namespace;
   int                iFlags;
   struct __FUNCALL * pNext;
} _FUNCALL, * PFUNCALL;

/* structure to hold an INLINE block of source */
typedef struct __PINLINE
{
   char *       szName;         /* name of a inline function */
   BYTE *       pCode;          /* pointer to a memory block where pcode is stored */
   HB_SIZE      lPCodeSize;     /* total memory size for pcode */
   char *       szFileName;     /* Source file name */
   int          iLine;          /* Source line number */
   struct __PINLINE * pNext;    /* pointer to the next defined inline */
} __INLINE, * PINLINE;

/* structure to control all Clipper defined functions */
typedef struct
{
   PFUNCTION pFirst;            /* pointer to the first defined funtion */
   PFUNCTION pLast;             /* pointer to the last defined function */
   int       iCount;            /* number of defined functions */
} FUNCTIONS;

typedef struct
{
   PFUNCALL pFirst;
   PFUNCALL pLast;
   int      iCount;
} FUNCALLS;

/* structure to control all Clipper defined functions */
typedef struct
{
   PINLINE pFirst;            /* pointer to the first defined inline */
   PINLINE pLast;             /* pointer to the last defined inline */
   int     iCount;            /* number of defined inlines */
} INLINES;

/* compiler symbol support structure */
typedef struct _COMSYMBOL
{
   char *         szName;     /* the name of the symbol */
   HB_SYMBOLSCOPE cScope;     /* the scope of the symbol */
   BYTE           cType;
   void *         Namespace;      /* is it a function name or memvar (NULL) */
   int            iFlags;
   union
   {
      PCOMCLASS pClass;
      PENUMDEF  pEnum;
   } Extended;
   struct _COMSYMBOL * pNext;   /* pointer to the next defined symbol */
} COMSYMBOL, * PCOMSYMBOL;

/* symbol table support structures */
typedef struct
{
   PCOMSYMBOL pFirst;           /* pointer to the first defined symbol */
   PCOMSYMBOL pLast;            /* pointer to the last defined symbol */
   int        iCount;           /* number of defined symbols */
} COMPSYMBOLS;

typedef struct __EXTERN
{
   char * szName;
   char * szNamespace;
   HB_SYMBOLSCOPE cScope;
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

typedef struct _AUTOOPEN
{
   char * szName;
   struct _AUTOOPEN * pNext;
} AUTOOPEN, * PAUTOOPEN;      /* support structure for extern symbols */

typedef struct _HB_DEBUGINFO
{
   char *   pszModuleName;
   HB_SIZE  ulFirstLine;
   HB_SIZE  ulLastLine;
   HB_SIZE  ulAllocated;
   BYTE *   pLineMap;
   struct _HB_DEBUGINFO * pNext;
} HB_DEBUGINFO, * PHB_DEBUGINFO;       /* support structure for line number info */

typedef struct _HB_LABEL_INFO
{
   FILE *   yyc;
   BOOL     fVerbose;
   BOOL     fSetSeqBegin;
   BOOL     fCondJump;
   BOOL     fEndProc;
   int      iNestedBlock;
   int      iFinally;
   ULONG *  pulLabels;
} HB_LABEL_INFO, * PHB_LABEL_INFO;

typedef struct _HB_EXPR_LIST
{
   HB_EXPR Expr;
   struct _HB_EXPR_LIST * pPrev;
   struct _HB_EXPR_LIST * pNext;
} HB_EXPR_LIST, * PHB_EXPR_LIST;

typedef struct
{
   char * __INITLINES__;
   char * __INITSTATICS__;
   char * __CLSSETMODULE;
   char * __DBGENTRY;
   char * __DBLIST;
   char * __GET;
   char * __GETA;
   char * __MVPRIVATE;
   char * __MVPUBLIC;
   char * _1;
   char * ARRAY;
   char * AT;
   char * ATAIL;
   char * BREAK_;
   char * CHR;
   char * CTOD;
   char * EVAL;
   char * FIELD_;
   char * GLOBAL_;
   char * HASH;
   char * HB_ENUMINDEX;
   char * HB_QWITH;
   char * HB_SETWITH;
   char * I18N;
   char * LEFT;
   char * LEN;
   char * RIGHT;
   char * STOD;
   char * STR;
   char * SUBSTR;
   char * TYPE;
   char * UPPER;
   char * WHILE_;
} HB_COMP_IDS;

/* definitions for hb_compPCodeEval() support */
typedef void * PHB_VOID;
#define HB_PCODE_FUNC( func, type ) HB_SIZE func( PFUNCTION pFunc, HB_SIZE lPCodePos, type cargo )

typedef  HB_PCODE_FUNC( HB_PCODE_FUNC_, PHB_VOID );
typedef  HB_PCODE_FUNC_ * PHB_PCODE_FUNC;

extern HB_SIZE hb_compPCodeSize( PFUNCTION, HB_SIZE );
extern void hb_compPCodeEval( PFUNCTION, const PHB_PCODE_FUNC *, void * );
extern void hb_compPCodeTrace( PFUNCTION, const PHB_PCODE_FUNC *, void * );

extern void hb_compGenLabelTable( PFUNCTION pFunc, PHB_LABEL_INFO label_info );
extern PHB_DEBUGINFO hb_compGetDebugInfo( void );

extern void hb_compPCodeStat( PHB_FNAME pFileName );

#define VS_NONE           0
#define VS_LOCAL          1
#define VS_STATIC         2
#define VS_FIELD          4
#define VS_PARAMETER      8
#define VS_GLOBAL        16
#define VS_EXTERNGLOBAL  32
#define VS_PRIVATE       64
#define VS_PUBLIC       128
#define VS_MEMVAR       ( VS_PUBLIC | VS_PRIVATE )

#define VU_NOT_USED    0
#define VU_INITIALIZED 1
#define VU_USED        2

#define VT_OFFSET_BYREF             60
#define VT_OFFSET_VARIANT           90
#define VT_OFFSET_OPTIONAL          90

/*
 * flags for bFlags member
*/
#define FUN_STATEMENTS        1   /* Function have at least one executable statement */
#define FUN_USES_STATICS      2   /* Function uses static variables */
#define FUN_PROCEDURE         4   /* This is a procedure that shouldn't return value */
#define FUN_BREAK_CODE        8   /* last statement breaks execution flow */
#define FUN_USES_LOCAL_PARAMS 16  /* parameters are declared using () */
#define FUN_WITH_RETURN       32  /* there was RETURN statement in previous line */
#define FUN_SEALED            64  /* there was RETURN statement in previous line */

#define SYMF_PUBLIC      0x0001 // HB_FS_PUBLIC
#define SYMF_STATIC      0x0002 //HB_FS_STATIC

#define SYMF_FUNCALL     0x1000

#define SYMF_NS_EXPLICITPATH ( SYMF_FUNCALL | 0x0010 )
#define SYMF_NS_EXPLICITPTR  ( SYMF_FUNCALL | 0x0020 )
#define SYMF_NS_RESOLVE      ( SYMF_FUNCALL | 0x0040 )
#define SYMF_NS_MEMBER       ( SYMF_FUNCALL | 0x0080 )

#define SYMF_NS_DEFER        ( SYMF_FUNCALL | SYMF_NS_EXPLICITPATH | 0x0100 )

#define NSF_NONE         SYMF_FUNCALL

#define NSF_RUNTIME      SYMF_NS_RUNTIME
#define NSF_EXPLICITPATH SYMF_NS_EXPLICITPATH
#define NSF_EXPLICITPTR  SYMF_NS_EXPLICITPTR
#define NSF_RESOLVE      SYMF_NS_RESOLVE
#define NSF_MEMBER       SYMF_NS_MEMBER
#define NSF_DEFER        SYMF_NS_DEFER

#if ! defined(  HB_MACRO_SUPPORT )

    #define NSTYPE_SPACE       0x0001
    #define NSTYPE_MEMBER      0x0002
    #define NSTYPE_END         0x0004

    #define NSTYPE_STEALTH     ( 0x0008 | NSTYPE_SPACE )

    #define NSTYPE_STATIC      0x0100
    #define NSTYPE_TERMINATOR  ( 0x0200 | NSTYPE_END )

    #define NSTYPE_RUNTIME     ( 0x0800 | NSTYPE_SPACE )

    #define NSTYPE_OPTIONAL    ( 0x1000 | NSTYPE_SPACE )
    #define NSTYPE_EXTERNAL    0x2000
    #define NSTYPE_IMPLEMENTS  ( 0x4000 | NSTYPE_SPACE )
    #define NSTYPE_USED        ( 0x8000 | NSTYPE_SPACE )

    #define NSENUM_SPACEADVISE 0x0010
    #define NSENUM_SPACEBEGIN  0x0020
    #define NSENUM_SPACEEND    0x0040

    typedef struct __NAMESPACE
    {
      int               type;
      char              *szName;
      char              *szFullPath;
      int               iID;
      struct __NAMESPACE *pOuter;
      PFUNCTION           pFunc;
      struct __NAMESPACE *pNext;
    } _NAMESPACE, *PNAMESPACE;

    typedef struct
    {
       PNAMESPACE pFirst;
       PNAMESPACE pLast;
       PNAMESPACE pCurrent;
    } NAMESPACES;

    extern NAMESPACES hb_comp_Namespaces;
    extern NAMESPACES hb_comp_UsedNamespaces;

    typedef PNAMESPACE (*PHB_COMP_NS_ENUMFUN)( PNAMESPACE, void ** );

    extern PNAMESPACE hb_compNamespaceEnumSkipMembers( PNAMESPACE pNamespace );
    extern PNAMESPACE hb_compNamespaceEnumDeep       ( PNAMESPACE pNamespace, PHB_COMP_NS_ENUMFUN pEnumFunc, void **pCargo );
    extern void       hb_compNamespaceEnumSpaces     ( PNAMESPACE pStart, PHB_COMP_NS_ENUMFUN pEnumFunc, void **pCargo );
    extern void       hb_compNamespaceEnumerate      ( PHB_COMP_NS_ENUMFUN pEnumFunc, void **pCargo );
    extern PNAMESPACE hb_compNamespaceNew            ( char *szName, int iType );
    extern void       hb_compNamespaceEnd            ( void );
    extern PNAMESPACE hb_compNamespaceLast           ( void );
    extern PNAMESPACE hb_compUsedNamespaceNew        ( char *szName, int iType );
    extern void       hb_compUsedNamespaceEnd        ( void );
    extern PNAMESPACE hb_compNamespaceFind           ( PNAMESPACE pNamespace, char *szName, int type );
    extern PNAMESPACE hb_compNamespaceFindMember     ( PNAMESPACE pNamespace, char *szName, int type );

    extern PFUNCTION  hb_compFunctionResolve ( char * szFunctionName, PNAMESPACE pNamespace, PCOMSYMBOL pSym );
    extern char *     hb_compFunctionResolveUsed( char *szFunName );

    extern void       hb_compFunctionAdd   ( char * szFunName, HB_SYMBOLSCOPE cScope, int iType ); /* starts a new Clipper language function definition */
    extern PFUNCTION  hb_compFunctionFind  ( char * szFunName, void *Namespace, int iFlags );      /* locates a previously defined function */
    extern PINLINE    hb_compInlineFind    ( char * szFunName );
    extern USHORT     hb_compFunctionGetPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */
    extern PFUNCTION  hb_compFunctionKill  ( PFUNCTION );           /* releases all memory allocated by function and returns the next one */
    extern void       hb_compAnnounce      ( char * );
    extern PINLINE    hb_compInlineAdd     ( char * szFunName );

#endif

extern PFUNCALL hb_compFunCallAdd  ( char * szFuntionName, void *Namespace, int iFlags );
extern PFUNCALL hb_compFunCallFind ( char * szFunName, void *Namespace, int iFlags ); /* locates a previously defined called function */
extern void     hb_compFunCallCheck( char *, int );

extern void   hb_compVariableAdd      ( char * szVarName, BYTE cType ); /* add a new param, local, static variable to a function definition or a public or private */
extern PVAR   hb_compVariableFind     ( PVAR pVars, int wOrder );       /* returns a variable if defined or zero */
extern PVAR   hb_compLocalVariableFind( PFUNCTION pFunc, int wVar );
extern USHORT hb_compVariableGetPos   ( PVAR pVars, char * szVarName ); /* returns the order + 1 of a variable if defined or zero */
extern int    hb_compLocalGetPos      ( char * szVarName );             /* returns the order + 1 of a local variable */
extern int    hb_compStaticGetPos     ( char *, PFUNCTION );            /* return if passed name is a static variable */
extern int    hb_compFieldGetPos      ( char *, PFUNCTION );            /* return if passed name is a field variable */
extern int    hb_compMemvarGetPos     ( char *, PFUNCTION );            /* return if passed name is a memvar variable */

extern PCOMSYMBOL hb_compSymbolAdd   ( char *szName, USHORT *pwPos, void *Namespace, int iFlags);
extern PCOMSYMBOL hb_compSymbolKill  ( PCOMSYMBOL );    /* releases all memory allocated by symbol and returns the next one */
extern PCOMSYMBOL hb_compSymbolFind  ( char *szName, USHORT *pwPos, void *Namespace, int iFlags ); /* returns a symbol pointer from the symbol table */
extern PCOMSYMBOL hb_compSymbolGetPos( USHORT );        /* returns a symbol based on its index on the symbol table */

extern PCOMDECLARED hb_compDeclaredAdd ( char * );
extern PCOMDECLARED hb_compDeclaredFind( char * );

extern PCOMCLASS    hb_compClassAdd  ( char * );
extern PCOMCLASS    hb_compClassFind ( char * );
extern PCOMDECLARED hb_compMethodAdd ( PCOMCLASS pClass, char *, BOOL );
extern PCOMDECLARED hb_compMethodFind( PCOMCLASS pClass, char * );
extern void         hb_compDeclaredParameterAdd( char * szVarName, BYTE cValueType );

extern void hb_compGenError  ( const char * szErrors[], char cPrefix, int iError, const char * szError1, const char * szError2 );        /* generic parsing error management function */
extern void hb_compGenWarning( const char * szWarnings[], char cPrefix, int iWarning, const char * szWarning1, const char * szWarning2); /* generic parsing warning management function */

extern void hb_compGenBreak        ( void );              /* generate code for BREAK statement */
extern void hb_compGenWithObject   ( PHB_EXPR pObject );  /* generate code for WITH OBJECT <obj> statement */
extern void hb_compGenEndWithObject( void );              /* generate code for END //WITH OBJECT <obj> statement */

extern void hb_compExternGen( void );                     /* generates the symbols for the EXTERN names */
extern void hb_compExternAdd( char * szExternName, char *szNamespace, HB_SYMBOLSCOPE cScope ); /* defines a new extern name */

extern void hb_compAutoOpenAdd( char * szName );

extern void     hb_compEnumAdd( char * szName );
extern PENUMDEF hb_compEnumFind( char * szName );
extern void     hb_compEnumMemberAdd( char * szName );

#ifdef HB_MACRO_SUPPORT

#define hb_compErrorType( p )    hb_macroError( EG_ARG,     HB_MACRO_PARAM )
#define hb_compErrorIndex( p )   hb_macroError( EG_BOUND,   HB_MACRO_PARAM )
#define hb_compErrorSyntax( p )  hb_macroError( EG_SYNTAX,  HB_MACRO_PARAM )
#define hb_compErrorLValue( p )  hb_macroError( EG_SYNTAX,  HB_MACRO_PARAM )
#define hb_compErrorBound( p )   hb_macroError( EG_BOUND,   HB_MACRO_PARAM )
#define hb_compErrorAlias( p )   hb_macroError( EG_NOALIAS, HB_MACRO_PARAM )
#define hb_compErrorDuplVar( c ) hb_macroError( EG_SYNTAX,  HB_MACRO_PARAM )
#define hb_compWarnMeaningless( p )

extern void * hb_compFlexNew( PHB_MACRO );
extern void   hb_compFlexDelete( void * );

#else /* HB_MACRO_SUPPORT */

extern void hb_compPrepareOptimize( void );

extern BOOL hb_compVariableMacroCheck( char * ); /* checks if passed variable can be used in macro */

extern HB_SIZE hb_compGenJump     ( HB_LONG );            /* generates the pcode to jump to a specific offset */
extern HB_SIZE hb_compGenJumpFalse( HB_LONG );            /* generates the pcode to jump if false */
extern HB_SIZE hb_compGenJumpTrue ( HB_LONG );            /* generates the pcode to jump if true */
extern void    hb_compGenJumpHere ( HB_ULONG  );          /* returns the pcode pos where to set a jump offset */
extern void    hb_compGenJumpThere( HB_ULONG, HB_ULONG ); /* sets a jump offset */


extern void hb_compLinePush          ( void ); /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfDebugger( void ); /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfInside  ( void ); /* generates the pcode with the currently compiled source code line */

extern void hb_compGenMessage       ( char * szMsgName );                                 /* sends a message to an object */
extern void hb_compGenMessageData   ( char * szMsg );                                     /* generates an underscore-symbol name for a data assignment */
extern void hb_compGenPopVar        ( char * szVarName );                                 /* generates the pcode to pop a value from the virtual machine stack onto a variable */
extern void hb_compGenPushDouble    ( double dNumber, BYTE bWidth, BYTE bDec );           /* Pushes a number on the virtual machine stack */
extern void hb_compGenPushFunCall   ( char *, char * );                                   /* generates the pcode to push function's call */
extern void hb_compGenPushVar       ( char * szVarName );                                 /* generates the pcode to push a variable value to the virtual machine stack */
extern void hb_compGenPushVarRef    ( char * szVarName );                                 /* generates the pcode to push a variable by reference to the virtual machine stack */
extern void hb_compGenPushMemVarRef ( char * szVarName );                                 /* generates the pcode to push a memvar variable by reference to the virtual machine stack */
extern void hb_compGenPushInteger   ( int iNumber );                                      /* Pushes a integer number on the virtual machine stack */
extern void hb_compGenPushLogical   ( int iTrueFalse );                                   /* pushes a logical value on the virtual machine stack */
extern void hb_compGenPushLong      ( HB_LONG lNumber );                                  /* Pushes an integer number on the virtual machine stack */
extern void hb_compGenPushDate      ( long lDate, long lTime, USHORT uType );             /* Pushes a date on the virtual machine stack */
extern void hb_compGenPushNil       ( void );                                             /* Pushes nil on the virtual machine stack */
extern void hb_compGenPushString    ( char * szText, HB_SIZE ulLen );                     /* Pushes a string on the virtual machine stack */
extern void hb_compGenPushSymbol    ( char * szSymbolName, void *Namespace, int iFlags ); /* Pushes a symbol on to the Virtual machine stack */
extern void hb_compGenPushAliasedVar( char *, BOOL, char *, long );
extern void hb_compGenPopAliasedVar ( char *, BOOL, char *, long );
extern void hb_compGenPushFunRef    ( char * );
extern void hb_compGenSwitchCase    ( long );                                             /* generates the pcode to push switchcase value on the virtual machine stack */

extern HB_SIZE hb_compSequenceBegin ( void );  /* Generate the opcode to open BEGIN/END sequence */
extern HB_SIZE hb_compSequenceEnd   ( void );  /* Generate the opcode to close BEGIN/END sequence */
extern HB_SIZE hb_compTryBegin      ( void );  /* Generate the opcode to open TRY/END tryuence */
extern HB_SIZE hb_compTryEnd        ( void );  /* Generate the opcode to close TRY/END tryuence */
extern void    hb_compSequenceFinish( HB_SIZE, int );

extern void hb_compGenPCode1( BYTE );                                 /* generates 1 byte of pcode */
extern void hb_compGenPData1( BYTE );                                 /* generates 1 byte of pcode argument */
extern void hb_compGenPCode2( BYTE, BYTE, BOOL );                     /* generates 2 bytes of pcode + flag for optional StrongType(). */
extern void hb_compGenPCode3( BYTE, BYTE, BYTE, BOOL );               /* generates 3 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCode4( BYTE, BYTE, BYTE, BYTE, BOOL );         /* generates 4 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCodeN( BYTE * pBuffer, HB_SIZE ulSize, BOOL ); /* copy bytes to a pcode buffer + flag for optional StrongType() */

#if defined(HB_COMP_STRONG_TYPES)
extern void hb_compStrongType( int iSize );
#endif


/* Codeblocks */
extern void     hb_compCodeBlockStart( void );        /* starts a codeblock creation */
extern PHB_EXPR hb_compCodeBlockEnd  ( BOOL );        /* end of codeblock creation */

/* support for FIELD declaration */
extern void hb_compFieldSetAlias( char *, int );
extern int  hb_compFieldsCount  ( void );

/* Static variables */
extern void hb_compStaticDefStart( void );
extern void hb_compStaticDefEnd  ( void );
extern void hb_compGenStaticName ( char * );

/* Global variables */
extern void     hb_compGlobalsDefStart  ( void );
extern void     hb_compGlobalsDefEnd    ( void );
extern void     hb_compGenGlobalName    ( char * );

extern void     hb_compGenModuleName    ( const char *szFile, const char *szFunc );

extern PHB_EXPR hb_compErrorStatic      ( char *, PHB_EXPR );
extern PHB_EXPR hb_compErrorType        ( PHB_EXPR );
extern PHB_EXPR hb_compErrorIndex       ( PHB_EXPR );
extern PHB_EXPR hb_compErrorSyntax      ( PHB_EXPR );
extern PHB_EXPR hb_compErrorLValue      ( PHB_EXPR );
extern PHB_EXPR hb_compErrorBound       ( PHB_EXPR );
extern PHB_EXPR hb_compErrorAlias       ( PHB_EXPR );
extern void     hb_compErrorDuplVar     ( char * );
extern PHB_EXPR hb_compWarnMeaningless  ( PHB_EXPR );

extern void     hb_compChkCompilerSwitch( int, char * Args[] );
extern void     hb_compChkEnvironVar    ( char * );
extern void     hb_compChkPaths         ( void );
extern void     hb_compChkDefines       ( int iArg, char * Args[] );

extern void     hb_compPrintUsage       ( char * );
extern void     hb_compPrintCredits     ( void );
extern void     hb_compFileInfo         ( void );
extern void     hb_compPrintLogo        ( void );
extern void     hb_compPrintModes       ( void );

extern void     hb_compInitPP           ( int argc, char * argv[] );

#endif    /* HB_MACRO_SUPPORT */

/* Main compiler functions */
extern int hb_compMain( int argc, char * argv[] );

/* Misc functions defined in harbour.c */
extern void hb_compSetDeferredFlagOn( void );
extern void hb_compSetCOutput( int iOutput );
extern void hb_compFinalizeFunction( void ); /* fixes all last defined function returns jumps offsets */
extern void hb_compNOOPadd( PFUNCTION pFunc, HB_SIZE ulPos );
extern void hb_compNOOPfill( PFUNCTION pFunc, HB_SIZE ulFrom, int iCount, BOOL fPop, BOOL fCheck );
extern BOOL hb_compIsJump( PFUNCTION pFunc, HB_SIZE ulPos );
/* internationalization */
extern void hb_compAddI18nString( char *szString );

/* Misc functions defined in hbfix.c */
extern void hb_compFixFuncPCode( PFUNCTION );
/* Misc functions defined in hbstripl.c */
extern void hb_compStripFuncLines( PFUNCTION pFunc );
/* Misc functions defined in hbdead.c */
extern void hb_compCodeTraceMarkDead( PFUNCTION pFunc );

/* Misc functions defined in harbour.y */
extern int hb_compYACCMain( char * szName );
extern BOOL hb_compInclude( char * szFileName, HB_PATHNAMES * pSearchPath );  /* end #include support */

extern char * hb_comp_buffer; /* yacc input buffer */

/* output related functions defined in gen*.c */
extern void hb_compGenCCode  ( PHB_FNAME, const char *); /* generates the C language output */
extern void hb_compGenPortObj( PHB_FNAME );              /* generates the portable objects */

/* REMOVED: Non-functional */
// extern void hb_compGenObj32( PHB_FNAME );           /* generates OBJ 32 bits */

extern void hb_compGenCObj( PHB_FNAME, const char *);  /* generates platform dependant object module */

/* hbident.c   */
extern char * hb_compIdentifierNew  ( char * szName, BOOL bCopy ); /* create the reusable identifier */
extern void   hb_compIdentifierOpen ( void );       /* prepare the table of identifiers */
extern void   hb_compIdentifierClose( void );       /* release the table of identifiers */

/* User defined function for Harbour compiler output */
typedef void ( * HB_OUTSTDFUNC ) ( void *, const char* );
typedef void ( * HB_OUTERRFUNC ) ( void *, const char* );

/* variable used by compiler
 */
extern PHB_PP_STATE   hb_comp_PP;
extern int            hb_comp_iLine;
extern FUNCTIONS      hb_comp_functions;
extern FUNCALLS       hb_comp_funcalls;
extern COMPSYMBOLS    hb_comp_symbols;
extern PCOMDECLARED   hb_comp_pFirstDeclared;
extern PCOMDECLARED   hb_comp_pLastDeclared;
extern PCOMDECLARED   hb_comp_pReleaseDeclared;
extern PCOMCLASS      hb_comp_pFirstClass;
extern PCOMCLASS      hb_comp_pLastClass;
extern PCOMCLASS      hb_comp_pReleaseClass;
extern char *         hb_comp_szFromClass;
extern PCOMDECLARED   hb_comp_pLastMethod;

extern char *         hb_comp_szFromEnum;

extern HB_PATHNAMES * hb_comp_pIncludePath;
extern char *         hb_comp_PrgFileName;
extern PFUNCTION      hb_comp_pInitFunc;
extern PFUNCTION      hb_comp_pGlobalsFunc;
extern PFUNCTION      hb_comp_pLineNumberFunc;
extern PHB_FNAME      hb_comp_pFileName;
extern BOOL           hb_comp_bPPO;
extern FILE *         hb_comp_yyppo;
extern BOOL           hb_comp_bStartProc;
extern BOOL           hb_comp_bExplicitStartProc;
extern BOOL           hb_comp_bLineNumbers;
extern BOOL           hb_comp_bQuiet;
extern BOOL           hb_comp_bShortCuts;
extern int            hb_comp_iWarnings;
extern BOOL           hb_comp_bAnyWarning;
extern BOOL           hb_comp_bAutoMemvarAssume;
extern BOOL           hb_comp_bForceMemvars;
extern BOOL           hb_comp_bDebugInfo;
extern char           hb_comp_szPrefix[ 21 ];
extern int            hb_comp_iGenCOutput;
extern BOOL           hb_comp_bNoStartUp;
extern int            hb_comp_iExitLevel;
extern int            hb_comp_iFunctionCnt;
extern char           hb_comp_cVarType;
extern char           hb_comp_cDataListType;
extern char           hb_comp_cCastType;
extern int            hb_comp_iVarScope;
extern BOOL           hb_comp_bDontGenLineNum;
extern int            hb_comp_iStaticCnt;
extern int            hb_comp_iErrorCount;
extern PHB_EXPR_LIST  hb_comp_exprs;

extern char *         hb_comp_szAnnounce;

extern PHB_FNAME      hb_comp_pOutPath;
extern PHB_FNAME      hb_comp_ppo_pOutPath;
extern BOOL           hb_comp_bCredits;
extern BOOL           hb_comp_bBuildInfo;

extern BOOL           hb_comp_iGenVarList;

/* Giancarlo Niccolai */
extern BOOL           hb_comp_bI18n;
extern char *         hb_comp_szHILout;

extern BOOL           hb_comp_bLogo;
extern BOOL           hb_comp_bSyntaxCheckOnly;
extern int            hb_comp_iLanguage;

extern USHORT         hb_comp_wSeqCounter;
extern USHORT         hb_comp_wForCounter;
extern USHORT         hb_comp_wIfCounter;
extern USHORT         hb_comp_wWhileCounter;
extern USHORT         hb_comp_wCaseCounter;
extern USHORT         hb_comp_wWithObjCounter;

extern char *         hb_comp_szDeclaredFun;
extern char *         hb_Command_Line;

extern char *         hb_comp_szLastMethod;

extern const char *   hb_comp_szErrors[];
extern const char *   hb_comp_szWarnings[];
extern char *         hb_pp_STD_CH;
extern int            hb_pp_STD_CH_ADDITIVE;

extern BOOL           hb_comp_bAutoOpen;
extern BOOL           hb_comp_bError;
extern USHORT         hb_comp_cInlineID;

extern INLINES        hb_comp_inlines;
extern int            hb_comp_iLineINLINE;
extern int            hb_comp_iLinePRG;

extern HB_SIZE        hb_comp_Supported;

extern PVAR           hb_comp_pGlobals;
extern short          hb_comp_iGlobals;

/* PreProcessor Tracing support. */
extern BOOL           hb_comp_bTracePP;
extern FILE *         hb_comp_PPTrace;

/* table with PCODEs' length */
extern const BYTE     hb_comp_pcode_len[];

/* error messages output */
/* extern FILE *      hb_comp_errFile; */

/* how many pcode used in a module */
extern ULONG          hb_comp_upCodeTotal;

/* Force cpp output (default is c )*/
extern BOOL           hb_comp_OutputIsCpp;

/* auto convert external function to dynamic when -vd is turned-on */
extern BOOL           hb_comp_autoDeferred;

extern char *         hb_comp_szNamespace;

/* procude list of public function in a module */
extern BOOL           hb_comp_createExternList;

/* force using PP reserved words */
extern BOOL           hb_comp_bUsePPReservedWord;

extern BOOL hb_comp_bWarnUnUsedLocals;
extern BOOL hb_comp_bWarnUnUsedStatics;
extern BOOL hb_comp_bWarnUnUsedGlobals;
extern BOOL hb_comp_bWarnUnUsedMemvars;
extern BOOL hb_comp_bWarnUnUsedFields;
extern BOOL hb_comp_bWarnUnUsedBlockParams;

extern char          hb_comp_szMsgBuf[ HB_PATH_MAX * 2 + 80 ];
extern void*         hb_compHandle;
extern HB_OUTSTDFUNC hb_outStdFunc;
extern HB_OUTERRFUNC hb_outErrFunc;

#define SIZE_OF_SZMSGBUF sizeof( hb_comp_szMsgBuf )

/* /GC command line setting types */
#define HB_COMPGENC_COMPACT        0
#define HB_COMPGENC_NORMAL         1
#define HB_COMPGENC_VERBOSE        2
#define HB_COMPGENC_REALCODE       3

/* /ES command line setting types */
#define HB_EXITLEVEL_DEFAULT       0
#define HB_EXITLEVEL_SETEXIT       1
#define HB_EXITLEVEL_DELTARGET     2

/* /kx command line setting types - compatibility modes
 * (turn on a bit in HB_SIZE word)
*/
#define HB_COMPFLAG_HARBOUR        1    /* -kh */
#define HB_COMPFLAG_XBASE          2    /* -kx */
#define HB_COMPFLAG_HB_INLINE      4    /* -ki */
#define HB_COMPFLAG_OPTJUMP       16    /* -kj turn off jump optimalization */
#define HB_COMPFLAG_RT_MACRO      64    /* -kr */

#ifdef HB_MACRO_SUPPORT
  #define HB_COMP_ISSUPPORTED(flag)    ( HB_MACRO_DATA->supported & (flag) )
#else
  #define HB_COMP_ISSUPPORTED(flag)    ( hb_comp_Supported & (flag) )
#endif

/* Hide Strings */
extern int    hb_comp_iHidden;
extern BYTE * hb_compHideString( int iType, char * szText, HB_SIZE ulStrLen, HB_SIZE * ulBufferLen );

/* Date and DateTime support */
extern void   hb_comp_datetimeEncode( long *plDate, long *plTime, int iYear, int iMonth, int iDay, int iHour, int iMinute, double dSeconds, int iAmPm, int * piOk );

/* Free memory upon exit */
extern void   hb_compCleanUp( void );

/* Checking if variable name is reserved by PP */
extern BOOL   hb_compReservedPPName( char * szName );

/* Writing Harbour compiler output */
extern void   hb_compOutStd( char * szMessage );
extern void   hb_compOutErr( char * szMessage );

/* Alternate memory tracer */
extern void * hb_xgrabEx   ( HB_SIZE ulSize, const char* szSourceFile, int iLine, const char* szFuncName );
extern void * hb_xreallocEx( void * pMem, HB_SIZE ulSize, const char* szSourceFile, int iLine, const char* szFuncName );
extern void   hb_xfreeEx   ( void * pMem, const char* szSourceFile, int iLine, const char* szFuncName );
extern void   hb_xexitEx   ( void );

#if defined( __HB_COMPILER__ )

#if defined( __BORLANDC__ )
#   define __HB_FUNC_NAME__     __FUNC__
#elif defined( __POCC__ )
#   define __HB_FUNC_NAME__     __func__
#elif defined( _MSC_VER )
#   if ( _MSC_VER >= 1300 )
#      define __HB_FUNC_NAME__  __FUNCTION__
#   else
       /* MSVS 6.0 is not C99 compliance yet. TODO: create an alternative */
#      define __HB_FUNC_NAME__  "N/A"
#  endif
#else
#   define __HB_FUNC_NAME__     __func__
#endif

#undef  hb_xgrab
#define hb_xgrab( p )       hb_xgrabEx( p, __FILE__, __LINE__, __HB_FUNC_NAME__ )

#undef  hb_xrealloc
#define hb_xrealloc( p, x ) hb_xreallocEx( p, x, __FILE__, __LINE__, __HB_FUNC_NAME__ )

#undef  hb_xfree
#define hb_xfree( p )       hb_xfreeEx( p, __FILE__, __LINE__, __HB_FUNC_NAME__ )

#undef  hb_xexit
#define hb_xexit            hb_xexitEx

#endif /* __HB_COMPILER__ */

HB_EXTERN_END

#endif /* HB_COMP_H_ */
