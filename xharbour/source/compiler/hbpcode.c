/*
 * $Id: hbpcode.c,v 1.31 2004/05/30 20:44:11 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Compiler PCode generation functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 RonPinkas <Ron@Profit-Master.com>
 *    hb_compStrongType()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <assert.h>

#include "hbcomp.h"

/* pCode List File */
typedef struct _CODELIST
{
   char *    szName;
   int       iUsed;
   struct _CODELIST * pNext;
} CODELIST, * PCODELIST;

static char *pCodeList[] =
{
    "HB_P_AND",                   /*   0 peforms the logical AND of two latest stack values, removes them and places result */
    "HB_P_ARRAYPUSH",             /*   1 places on the virtual machine stack an array element */
    "HB_P_ARRAYPOP",              /*   2 pops a value from the eval stack into an array element */
    "HB_P_ARRAYDIM",              /*   3 instructs the virtual machine to build an array with some specific dimensions */
    "HB_P_ARRAYGEN",              /*   4 instructs the virtual machine to build an array and load elemnst from the stack */
    "HB_P_EQUAL",                 /*   5 check if the latest two values on the stack are equal, removing them and leaving there the result */
    "HB_P_ENDBLOCK",              /*   6 end of a codeblock definition */
    "HB_P_ENDPROC",               /*   7 instructs the virtual machine to end execution */
    "HB_P_EXACTLYEQUAL",          /*   8 check if the latest two values on the stack are exactly equal, removing them and leaving there the result */
    "HB_P_FALSE",                 /*   9 pushes false on the virtual machine stack */
    "HB_P_FORTEST",               /*  10 For STEP. If step > 1 less. If step < 1 greater. */
    "HB_P_FUNCTION",              /*  11 instructs the virtual machine to execute a function saving its result */
    "HB_P_FUNCTIONSHORT",         /*  12 instructs the virtual machine to execute a function saving its result */
    "HB_P_FRAME",                 /*  13 instructs the virtual machine about how many parameters and locals a function uses */
    "HB_P_FUNCPTR",               /*  14 returns a function address pointer */
    "HB_P_GREATER",               /*  15 checks if the second latest value on the stack is greater that the lastest one */
    "HB_P_GREATEREQUAL",          /*  16 checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
    "HB_P_DEC",                   /*  17 decrements the latest value on the virtual machine stack */
    "HB_P_DIVIDE",                /*  18 divides the latest two values on the stack, removing them and leaving there the result */
    "HB_P_DO",                    /*  19 instructs the virtual machine to execute a function discarding its result */
    "HB_P_DOSHORT",               /*  20 instructs the virtual machine to execute a function discarding its result */
    "HB_P_DUPLICATE",             /*  21 places a copy of the latest virtual machine stack value on to the stack */
    "HB_P_DUPLTWO",               /*  22 places a copy of the latest two virtual machine stack value on to the stack */
    "HB_P_INC",                   /*  23 increments the latest value on the virtual machine stack */
    "HB_P_INSTRING",              /*  24 checks if the second latest value on the stack is a substring of the latest one */
    "HB_P_JUMPNEAR",              /*  25 jumps to a relative offset 1 Byte */
    "HB_P_JUMP",                  /*  26 jumps to a relative offset 2 Bytes */
    "HB_P_JUMPFAR",               /*  27 jumps to a relative offset 3 Bytes */
    "HB_P_JUMPFALSENEAR",         /*  28 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_JUMPFALSE",             /*  29 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_JUMPFALSEFAR",          /*  30 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_JUMPTRUENEAR",          /*  31 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_JUMPTRUE",              /*  32 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_JUMPTRUEFAR",           /*  33 checks a logic expression of the stack and jumps to a relative offset */
    "HB_P_LESSEQUAL",             /*  34 checks if the second latest value on the stack is less equal that the latest one, leaves the result only */
    "HB_P_LESS",                  /*  35 checks if the second latest value on the stack is less that the lastest one */
    "HB_P_LINE",                  /*  36 currently compiled source code line number */
    "HB_P_LOCALNAME",             /*  37 sets the name of local variable */
    "HB_P_MACROPOP",              /*  38 compile and run - pop a value from the stack */
    "HB_P_MACROPOPALIASED",       /*  39 compile and run - pop a field value from the stack */
    "HB_P_MACROPUSH",             /*  40 compile and run - leave the result on the stack */
    "HB_P_MACROPUSHARG",          /*  41 compile and run - leave the result on the stack */
    "HB_P_MACROPUSHLIST",         /*  42 compile and run - leave the result on the stack */
    "HB_P_MACROPUSHINDEX",        /*  43 compile and run - leave the result on the stack */
    "HB_P_MACROPUSHPARE",         /*  44 compile and run - leave the result on the stack */
    "HB_P_MACROPUSHALIASED",      /*  45 compile and run - leave the field value on the stack */
    "HB_P_MACROSYMBOL",           /*  46 compile into a symbol name (used in function calls) */
    "HB_P_MACROTEXT",             /*  47 macro text substitution */
    "HB_P_MESSAGE",               /*  48 sends a message to an object */
    "HB_P_MINUS",                 /*  49 subs the latest two values on the stack, removing them and leaving there the result */
    "HB_P_MODULUS",               /*  50 calculates the modulus of the two values on the stack, removing them and leaving there the result */
    "HB_P_MODULENAME",            /*  51 sets the name of debugged module */
    "HB_P_MMESSAGE",              /*  52 */
    "HB_P_MPOPALIASEDFIELD",      /*  53 */
    "HB_P_MPOPALIASEDVAR",        /*  54 */
    "HB_P_MPOPFIELD",             /*  55 */
    "HB_P_MPOPMEMVAR",            /*  56 */
    "HB_P_MPUSHALIASEDFIELD",     /*  57 */
    "HB_P_MPUSHALIASEDVAR",       /*  58 */
    "HB_P_MPUSHBLOCK",            /*  59 */
    "HB_P_MPUSHFIELD",            /*  60 */
    "HB_P_MPUSHMEMVAR",           /*  61 */
    "HB_P_MPUSHMEMVARREF",        /*  62 */
    "HB_P_MPUSHSYM",              /*  63 */
    "HB_P_MPUSHVARIABLE",         /*  64 */  // Note also 132 HB_P_MPUSHSTR
    "HB_P_MULT",                  /*  65 multiplies the latest two values on the stack, removing them and leaving there the result */
    "HB_P_NEGATE",                /*  66 numerically negates the latest value on the stack */
    "HB_P_NOOP",                  /*  67 no operation */
    "HB_P_NOT",                   /*  68 logically negates the latest value on the stack */
    "HB_P_NOTEQUAL",              /*  69 checks if the latest two stack values are equal, leaves just the result */
    "HB_P_OR",                    /*  70 peforms the logical OR of two latest stack values, removes them and places result */
    "HB_P_PARAMETER",             /*  71 creates PRIVATE variables and assigns values to functions paramaters */
    "HB_P_PLUS",                  /*  72 adds the latest two values on the stack, removing them and leaving there the result */
    "HB_P_POP",                   /*  73 removes the latest value from the stack */
    "HB_P_POPALIAS",              /*  74 pops the item from the eval stack and selects the current workarea */
    "HB_P_POPALIASEDFIELD",       /*  75 pops aliased field */
    "HB_P_POPALIASEDFIELDNEAR",   /*  76 pops aliased field */
    "HB_P_POPALIASEDVAR",         /*  77 pops aliased variable (either a field or a memvar) */
    "HB_P_POPFIELD",              /*  78 pops unaliased field */
    "HB_P_POPLOCAL",              /*  79 pops the contains of the virtual machine stack onto a local variable */
    "HB_P_POPLOCALNEAR",          /*  80 pops the contains of the virtual machine stack onto a local variable */
    "HB_P_POPMEMVAR",             /*  81 pops the contains of a memvar variable to the virtual machine stack */
    "HB_P_POPSTATIC",             /*  82 pops the contains of the virtual machine stack onto a static variable */
    "HB_P_POPVARIABLE",           /*  83 pops the contains of an undeclared variable from the virtual machine stack */
    "HB_P_POWER",                 /*  84 calculates the power of the two values on the stack, removing them and leaving there the result */
    "HB_P_PUSHALIAS",             /*  85 saves the current workarea number on the eval stack */
    "HB_P_PUSHALIASEDFIELD",      /*  86 pushes aliased field */
    "HB_P_PUSHALIASEDFIELDNEAR",  /*  87 pushes aliased field */
    "HB_P_PUSHALIASEDVAR",        /*  88 pushes aliased variable (either a field or a memvar) */
    "HB_P_PUSHBLOCK",             /*  89 start of a codeblock definition */
    "HB_P_PUSHBLOCKSHORT",        /*  90 start of a codeblock definition */
    "HB_P_PUSHFIELD",             /*  91 pushes unaliased field */
    "HB_P_PUSHBYTE",              /*  92 places a 1 byte integer number on the virtual machine stack */
    "HB_P_PUSHINT",               /*  93 places an integer number on the virtual machine stack */
    "HB_P_PUSHLOCAL",             /*  94 pushes the contains of a local variable to the virtual machine stack */
    "HB_P_PUSHLOCALNEAR",         /*  95 pushes the contains of a local variable to the virtual machine stack */
    "HB_P_PUSHLOCALREF",          /*  96 pushes a local variable by reference to the virtual machine stack */
    "HB_P_PUSHLONG",              /*  97 places an integer number on the virtual machine stack */
    "HB_P_PUSHMEMVAR",            /*  98 pushes the contains of a memvar variable to the virtual machine stack */
    "HB_P_PUSHMEMVARREF",         /*  99 pushes the a memvar variable by reference to the virtual machine stack */
    "HB_P_PUSHNIL",               /* 100 places a nil on the virtual machine stack */
    "HB_P_PUSHDOUBLE",            /* 101 places a double number on the virtual machine stack */
    "HB_P_PUSHSELF",              /* 102 pushes Self for the current processed method */
    "HB_P_PUSHSTATIC",            /* 103 pushes the contains of a static variable to the virtual machine stack */
    "HB_P_PUSHSTATICREF",         /* 104 pushes the a static variable by reference to the virtual machine stack */
    "HB_P_PUSHSTR",               /* 105 places a string on the virtual machine stack */
    "HB_P_PUSHSTRSHORT",          /* 106 places a string on the virtual machine stack */
    "HB_P_PUSHSYM",               /* 107 places a symbol on the virtual machine stack */
    "HB_P_PUSHSYMNEAR",           /* 108 places a symbol on the virtual machine stack */
    "HB_P_PUSHVARIABLE",          /* 109 pushes the contains of an undeclared variable to the virtual machine stack */
    "HB_P_RETVALUE",              /* 110 instructs the virtual machine to return the latest stack value */
    "HB_P_SEND",                  /* 111 send operator */
    "HB_P_SENDSHORT",             /* 112 send operator */
    "HB_P_SEQBEGIN",              /* 113 BEGIN SEQUENCE */
    "HB_P_SEQEND",                /* 114 END SEQUENCE */
    "HB_P_SEQRECOVER",            /* 115 RECOVER statement */
    "HB_P_SFRAME",                /* 116 sets the statics frame for a function */
    "HB_P_STATICS",               /* 117 defines the number of statics variables for a PRG */
    "HB_P_STATICNAME",            /* 118 sets the name of static variable */
    "HB_P_SWAPALIAS",             /* 119 restores the current workarea number from the eval stack */
    "HB_P_TRUE",                  /* 120 pushes true on the virtual machine stack */
    "HB_P_ZERO",                  /* 121 places a ZERO on the virtual machine stack */
    "HB_P_ONE",                   /* 122 places a ONE on the virtual machine stack */
    "HB_P_MACROLIST",             /* 123 HB_P_MACROPUSHLIST envelope start. */
    "HB_P_MACROLISTEND",          /* 124 HB_P_MACROPUSHLIST envelope end. */
    "HB_P_LOCALNEARADDINT",       /* 125 Add/Subtract specified int into specified local without using the stack. */
    "HB_P_LOCALNEARSETINT",       /* 126 Set specified int into specified local without using the stack.*/
    "HB_P_LOCALNEARSETSTR",       /* 127 Set specified string into specified local without using the stack.*/
    "HB_P_ADDINT",                /* 128 Add/Subtract specified int onto Stack Top value without extra push/pop. */
    "HB_P_LEFT",                  /* 129 Optimized Left(). */
    "HB_P_RIGHT",                 /* 130 Optimized Right(). */
    "HB_P_SUBSTR",                /* 131 Optimized SubStr() */
    "HB_P_MPUSHSTR",              /* 132 Macro compiled Pushed String. */
    "HB_P_BASELINE",              /* 133 Line Number Offset. */
    "HB_P_LINEOFFSET",            /* 134 Line Number Offset. */
    "HB_P_WITHOBJECT",            /* 135 */
    "HB_P_SENDWITH",              /* 136 */
    "HB_P_SENDWITHSHORT",         /* 137 */
    "HB_P_ENDWITHOBJECT",         /* 138 */
    "HB_P_FOREACH",               /* 139 */
    "HB_P_ENUMERATE",             /* 140 */
    "HB_P_ENDENUMERATE",          /* 141 */
    "HB_P_PUSHGLOBAL",            /* 142 */
    "HB_P_POPGLOBAL",             /* 143 */
    "HB_P_PUSHGLOBALREF",         /* 144 */
    "HB_P_ENUMINDEX",             /* 145 */
    "HB_P_SWITCHCASE",            /* 146 */
    "HB_P_LIKE",                  /* 147 */
    "HB_P_MATCH",                 /* 148 */
    "HB_P_PUSHMACROREF",          /* 149 */
    "HB_P_IVARREF",               /* 150 */
    "HB_P_CLASSSETMODULE",        /* 151 */
    "HB_P_BITAND",                /* 152 */
    "HB_P_BITOR",                 /* 153 */
    "HB_P_BITXOR",                /* 154 */
    "HB_P_BITSHIFTR",             /* 155 */
    "HB_P_BITSHIFTL",             /* 156 */
    "HB_P_LARGEFRAME",            /* 157 */
    "HB_P_PUSHWITH",              /* 158 pushes QWith for the current procedure/method */
    "HB_P_LAST_PCODE"             /* 159 this defines the number of defined pcodes */
};

static BYTE s_pcode_len[] = {
   1,        /* HB_P_AND,                  */
   1,        /* HB_P_ARRAYPUSH,            */
   1,        /* HB_P_ARRAYPOP,             */
   3,        /* HB_P_ARRAYDIM,             */
   3,        /* HB_P_ARRAYGEN,             */
   1,        /* HB_P_EQUAL,                */
   1,        /* HB_P_ENDBLOCK,             */
   1,        /* HB_P_ENDPROC,              */
   1,        /* HB_P_EXACTLYEQUAL,         */
   1,        /* HB_P_FALSE,                */
   1,        /* HB_P_FORTEST,              */
   3,        /* HB_P_FUNCTION,             */
   2,        /* HB_P_FUNCTIONSHORT,        */
   3,        /* HB_P_FRAME,                */
   1,        /* HB_P_FUNCPTR,              */
   1,        /* HB_P_GREATER,              */
   1,        /* HB_P_GREATEREQUAL,         */
   1,        /* HB_P_DEC,                  */
   1,        /* HB_P_DIVIDE,               */
   3,        /* HB_P_DO,                   */
   2,        /* HB_P_DOSHORT,              */
   1,        /* HB_P_DUPLICATE,            */
   1,        /* HB_P_DUPLTWO,              */
   1,        /* HB_P_INC,                  */
   1,        /* HB_P_INSTRING,             */
   2,        /* HB_P_JUMPNEAR,             */
   3,        /* HB_P_JUMP,                 */
   4,        /* HB_P_JUMPFAR,              */
   2,        /* HB_P_JUMPFALSENEAR,        */
   3,        /* HB_P_JUMPFALSE,            */
   4,        /* HB_P_JUMPFALSEFAR,         */
   2,        /* HB_P_JUMPTRUENEAR,         */
   3,        /* HB_P_JUMPTRUE,             */
   4,        /* HB_P_JUMPTRUEFAR,          */
   1,        /* HB_P_LESSEQUAL,            */
   1,        /* HB_P_LESS,                 */
   3,        /* HB_P_LINE,                 */
   0,        /* HB_P_LOCALNAME,            */
   2,        /* HB_P_MACROPOP,             */
   2,        /* HB_P_MACROPOPALIASED,      */
   2,        /* HB_P_MACROPUSH,            */
   2,        /* HB_P_MACROPUSHARG,         */
   2,        /* HB_P_MACROPUSHLIST,        */
   2,        /* HB_P_MACROPUSHINDEX,       */
   2,        /* HB_P_MACROPUSHPARE,        */
   2,        /* HB_P_MACROPUSHALIASED,     */
   1,        /* HB_P_MACROSYMBOL,          */
   1,        /* HB_P_MACROTEXT,            */
   3,        /* HB_P_MESSAGE,              */
   1,        /* HB_P_MINUS,                */
   1,        /* HB_P_MODULUS,              */
   0,        /* HB_P_MODULENAME,           */
             /* start: pcodes generated by macro compiler */
   3,        /* HB_P_MMESSAGE,             */
   3,        /* HB_P_MPOPALIASEDFIELD,     */
   3,        /* HB_P_MPOPALIASEDVAR,       */
   3,        /* HB_P_MPOPFIELD,            */
   3,        /* HB_P_MPOPMEMVAR,           */
   3,        /* HB_P_MPUSHALIASEDFIELD,    */
   3,        /* HB_P_MPUSHALIASEDVAR,      */
   0,        /* HB_P_MPUSHBLOCK,           */
   3,        /* HB_P_MPUSHFIELD,           */
   3,        /* HB_P_MPUSHMEMVAR,          */
   3,        /* HB_P_MPUSHMEMVARREF,       */
   3,        /* HB_P_MPUSHSYM,             */
   3,        /* HB_P_MPUSHVARIABLE,        */
             /* end: */
   1,        /* HB_P_MULT,                 */
   1,        /* HB_P_NEGATE,               */
   1,        /* HB_P_NOOP,                 */
   1,        /* HB_P_NOT,                  */
   1,        /* HB_P_NOTEQUAL,             */
   1,        /* HB_P_OR,                   */
   4,        /* HB_P_PARAMETER,            */
   1,        /* HB_P_PLUS,                 */
   1,        /* HB_P_POP,                  */
   1,        /* HB_P_POPALIAS,             */
   3,        /* HB_P_POPALIASEDFIELD,      */
   2,        /* HB_P_POPALIASEDFIELDNEAR,  */
   3,        /* HB_P_POPALIASEDVAR,        */
   3,        /* HB_P_POPFIELD,             */
   3,        /* HB_P_POPLOCAL,             */
   2,        /* HB_P_POPLOCALNEAR,         */
   3,        /* HB_P_POPMEMVAR,            */
   3,        /* HB_P_POPSTATIC,            */
   3,        /* HB_P_POPVARIABLE,          */
   1,        /* HB_P_POWER,                */
   1,        /* HB_P_PUSHALIAS,            */
   3,        /* HB_P_PUSHALIASEDFIELD,     */
   2,        /* HB_P_PUSHALIASEDFIELDNEAR, */
   3,        /* HB_P_PUSHALIASEDVAR,       */
   0,        /* HB_P_PUSHBLOCK,            */
   0,        /* HB_P_PUSHBLOCKSHORT,       */
   3,        /* HB_P_PUSHFIELD,            */
   2,        /* HB_P_PUSHBYTE,             */
   3,        /* HB_P_PUSHINT,              */
   3,        /* HB_P_PUSHLOCAL,            */
   2,        /* HB_P_PUSHLOCALNEAR,        */
   3,        /* HB_P_PUSHLOCALREF,         */
   1 + sizeof( LONG ),        /* HB_P_PUSHLONG,             */
   3,        /* HB_P_PUSHMEMVAR,           */
   3,        /* HB_P_PUSHMEMVARREF,        */
   1,        /* HB_P_PUSHNIL,              */
   1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ),        /* HB_P_PUSHDOUBLE,           */
   1,        /* HB_P_PUSHSELF,             */
   3,        /* HB_P_PUSHSTATIC,           */
   3,        /* HB_P_PUSHSTATICREF,        */
   0,        /* HB_P_PUSHSTR,              */
   0,        /* HB_P_PUSHSTRSHORT,         */
   3,        /* HB_P_PUSHSYM,              */
   2,        /* HB_P_PUSHSYMNEAR,          */
   3,        /* HB_P_PUSHVARIABLE,         */
   1,        /* HB_P_RETVALUE,             */
   3,        /* HB_P_SEND,                 */
   2,        /* HB_P_SENDSHORT,            */
   4,        /* HB_P_SEQBEGIN,             */
   4,        /* HB_P_SEQEND,               */
   1,        /* HB_P_SEQRECOVER,           */
   3,        /* HB_P_SFRAME,               */
   5,        /* HB_P_STATICS,              */
   0,        /* HB_P_STATICNAME,           */
   1,        /* HB_P_SWAPALIAS,            */
   1,        /* HB_P_TRUE,                 */
   1,        /* HB_P_ZERO,                 */
   1,        /* HB_P_ONE,                  */
   1,        /* HB_P_MACROLIST,            */
   1,        /* HB_P_MACROLISTEND,         */
   4,        /* HB_P_LOCALNEARADDINT,      */
   4,        /* HB_P_LOCALNEARSETINT,      */
   0,        /* HB_P_LOCALNEARSETSTR,      */
   3,        /* HB_P_ADDINT,               */
   3,        /* HB_P_LEFT,                 */
   3,        /* HB_P_RIGHT,                */
   5,        /* HB_P_SUBSTR,               */
   0,        /* HB_P_MPUSHSTR,             */
   3,        /* HB_P_BASELINE,             */
   2,        /* HB_P_LINEOFFSET,           */
   1,        /* HB_P_WITHOBJECT            */
   3,        /* HB_P_SENDWITH,             */
   2,        /* HB_P_SENDWITHSHORT,        */
   1,        /* HB_P_ENDWITHOBJECT,        */
   1,        /* HB_P_FOREACH,              */
   1,        /* HB_P_ENUMERATE,            */
   1,        /* HB_P_ENDENUMERATE,         */
   2,        /* HB_P_PUSHGLOBAL,           */
   2,        /* HB_P_POPGLOBAL,            */
   2,        /* HB_P_PUSHGLOBALREF,        */
   1,        /* HB_P_ENUMINDEX,            */
   1 + sizeof( LONG ), /* HB_P_SWITCHCASE, */
   1,        /* HB_P_LIKE,                 */
   1,        /* HB_P_MATCH,                */
   1,        /* HB_P_PUSHMACROREF,         */
   1,        /* HB_P_IVARREF,              */
   1,        /* HB_P_CLASSSETMODULE,       */
   1,        /* HB_P_BITAND,               */
   1,        /* HB_P_BITOR,                */
   1,        /* HB_P_BITXOR,               */
   1,        /* HB_P_BITSHIFTR,            */
   1,        /* HB_P_BITSHIFTL,            */
   4,        /* HB_P_LARGEFRAME            */
   1         /* HB_P_PUSHWITH,             */
};

extern BOOL hb_comp_iGenVarList;
extern FILE *hb_comp_pCodeList;
static PCODELIST pCodeFirst = NULL;

static PCODELIST hb_compPCodeList( PCODELIST pCodeCurrent, char* pCodeName )
{
   PCODELIST pCheck = pCodeFirst;
   BOOL bFound = FALSE;

   while( pCheck )
   {
      if( strcmp( pCheck->szName, pCodeName ) )
      {
         pCheck = pCheck->pNext;
      }
      else
      {
         pCheck->iUsed ++;
         bFound = TRUE;
         break;
      }
   }

   if ( ! bFound )
   {
      PCODELIST pCodeLast = (PCODELIST) hb_xgrab( sizeof( CODELIST ) );

      pCodeLast->szName = pCodeName;
      pCodeLast->iUsed = 1;
      pCodeLast->pNext = NULL;

      if( pCodeCurrent )
      {
         pCodeCurrent->pNext = pCodeLast;
      }

      pCodeCurrent = pCodeLast;

   }
   return pCodeCurrent;
}
#if 0
static PVAR hb_compPrivateFind( char * szPrivateName )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;
   PVAR pPrivate = NULL;

   if( pFunc )
   {
      pPrivate = pFunc->pPrivates;
   }

   while ( pPrivate )
   {
      if( ! strcmp( pPrivate->szName, szPrivateName ) )
      {
         return pPrivate;
      }
      pPrivate = pPrivate->pNext;

   }
   return NULL;
}
#endif

void hb_compPCodeEval( PFUNCTION pFunc, HB_PCODE_FUNC_PTR * pFunctions, void * cargo, BOOL bWriteList )
{
   ULONG ulPos = 0;
   USHORT usSkip;
   BYTE opcode;
   HB_PCODE_FUNC_PTR pCall;

   USHORT ipCodeCount = 0;
   USHORT ipCodeUsed = 0;
   PCODELIST pCodeCurrent = NULL;

   /* Make sure that table is correct */
   assert( sizeof( s_pcode_len ) == HB_P_LAST_PCODE );

   while( ulPos < pFunc->lPCodePos )
   {
      opcode = pFunc->pCode[ ulPos ];

      if( opcode < HB_P_LAST_PCODE )
      {
         if( bWriteList )
         {
            pCodeCurrent = hb_compPCodeList( pCodeCurrent, pCodeList[opcode] );

            if( !pCodeFirst )
            {
               pCodeFirst = pCodeCurrent;
            }
         }

         usSkip = s_pcode_len[ opcode ];
         pCall = pFunctions[ opcode ];

         if( pCall )
         {
            if( usSkip )
            {
               pCall( pFunc, ulPos, cargo );
            }
            else
            {
               usSkip = pCall( pFunc, ulPos, cargo );
            }
         }
         ulPos += (ULONG) usSkip;
      }
      else
      {
         printf( "--- Invalid opcode %i in hb_compPCodeEval() ---\n", opcode );
         ++ulPos;
      }
   }

   if( bWriteList )
   {
      PCODELIST pCodeTemp;

      pCodeTemp = pCodeFirst;
      while( pCodeTemp )
      {
         ipCodeCount ++;
         ipCodeUsed += pCodeTemp->iUsed ;
         fprintf( hb_comp_pCodeList, "%s=%i\n", pCodeTemp->szName, pCodeTemp->iUsed );
         pCodeTemp = pCodeTemp->pNext;
         hb_xfree( ( void * ) pCodeFirst );
         pCodeFirst = pCodeTemp;
      }
      fprintf( hb_comp_pCodeList, "TYPES=%i\nCALLS=%i\n\n", ipCodeCount, ipCodeUsed );
   }
}

void hb_compStrongType( int iSize )
{
   HB_SYMBOL_UNUSED( iSize );
#if 0
   PFUNCTION pFunc = hb_comp_functions.pLast, pTmp;
   PVAR pVar = NULL;
   PCOMSYMBOL pSym = NULL;
   PCOMDECLARED pDeclared;
   ULONG ulPos = pFunc->lPCodePos - iSize;
   SHORT wVar = 0;
   BYTE szType0[64], szType1[64], szType2[64], cType, cSubType0 = 0, cSubType1 = 0, cSubType2 = 0;
   BYTE bLast1, bLast2;
   static int s_aiPreCondStack[ 16 ], s_iCondIndex = 0;

   /*
   printf( "\nProcessing: %i Stack: %i\n", pFunc->pCode[ ulPos ], pFunc->iStackSize );
   */

   /* Make sure we have enough stack space. */
   if( ! pFunc->pStack )
   {
      pFunc->pStack = ( BYTE * ) hb_xgrab( pFunc->iStackSize += 16 );
   }
   else if( pFunc->iStackSize - pFunc->iStackIndex < 4 )
   {
      pFunc->pStack = ( BYTE * ) hb_xrealloc( pFunc->pStack, pFunc->iStackSize += 16 );
   }

   /* TODO: Split under conitions for the different matching possible iSize. */

   /* TODO: Subject to Operator Overloading! */

   switch ( pFunc->pCode[ ulPos ] )
   {
     /*-----------------4/26/00 0:16AM-------------------
      * Push values on stack.
      * --------------------------------------------------*/

     case HB_P_SWAPALIAS :
        /* TODO check affect on stack. */
        break;

     case HB_P_RETVALUE :
       if( pFunc->iStackIndex < 1 )
       {
          break;
       }

       pFunc->iStackIndex--;

       pSym = hb_compSymbolFind( pFunc->szName, NULL );

       if( pSym && pSym->szName )
       {
          char cType1, cType2;

          pDeclared = hb_compDeclaredFind( pSym->szName );

          if( pDeclared )
          {
             if( hb_comp_cCastType == ' ' )
             {
                /* No casting - do nothing. */
             }
             else if( toupper( hb_comp_cCastType ) == 'S' )
             {
                PCOMCLASS pClass = hb_compClassFind( hb_comp_szFromClass );

                if( pClass )
                {
                   if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
                   {
                      pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] = pClass;
                   }
                   else
                   {
                      pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pClass;
                   }
                   pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
                }
                else
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, pDeclared->szName );
                   pFunc->pStack[ pFunc->iStackIndex ] = ( isupper(  ( int ) hb_comp_cCastType ) ? 'O' : 'o' );
                }

                hb_comp_cCastType = ' ';
             }
             else
             {
                pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
                hb_comp_cCastType = ' ';
             }

             /* Variant as SubType. */
             if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
             {
                cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
             }
             else
             {
                cType1 = pFunc->pStack[ pFunc->iStackIndex ];
             }

             if( cSubType1 )
             {
                if( cSubType1 == 'S' && pFunc->iStackClasses )
                {
                   sprintf( ( char * ) szType1, "AnyType.SubType[%s]", pFunc->pStackClasses[ --pFunc->iStackClasses ]->szName );
                }
                else if( cSubType1 == 's' && pFunc->iStackClasses )
                {
                   sprintf( ( char * ) szType1, "AnyType.SubType[ARRAY OF %s]", pFunc->pStackClasses[ --pFunc->iStackClasses ]->szName );
                }
                else if( cSubType1 == '-' )
                {
                   strcpy( ( char * ) szType1,  "AnyType.SubType[NIL]" );
                }
                else
                {
                   sprintf( ( char * ) szType1, "AnyType.SubType[%c]", cSubType1 );
                }
             }
             else
             {
                if( cType1 == 'S' && pFunc->iStackClasses )
                {
                   sprintf( ( char * ) szType1, "%s", pFunc->pStackClasses[ --pFunc->iStackClasses ]->szName );
                }
                else if( cType1 == 's' && pFunc->iStackClasses )
                {
                   sprintf( ( char * ) szType1, "ARRAY OF %s", pFunc->pStackClasses[ --pFunc->iStackClasses ]->szName );
                }
                else if( cType1 == '-' )
                {
                   strcpy( ( char * ) szType1,  "NIL" );
                }
                else
                {
                   sprintf( ( char * ) szType1, "%c", cType1 );
                }
             }

             cType2 = pDeclared->cType;
             if( cType2 == 'S' )
             {
                sprintf( ( char * ) szType2, "%s", pDeclared->Extended.pClass->szName );
             }
             else if( cType2 == 's' )
             {
                sprintf( ( char * ) szType2, "ARRAY OF %s", pDeclared->Extended.pClass->szName );
             }
             else if( cType2 == '-' )
             {
                sprintf( ( char * ) szType2, "NIL" );
             }
             else
             {
                sprintf( ( char * ) szType2, "%c", cType2 );
             }

             if( pDeclared->cType != ' ' && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
             {
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_RETURN_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
             }
             else if( pDeclared->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'O' )
             {
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_RETURN_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
             }
             else if( pDeclared->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 'o' )
             {
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_RETURN_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
             }
             else if( pDeclared->cType != ' ' && pDeclared->cType != pFunc->pStack[ pFunc->iStackIndex ] )
             {
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_RETURN_TYPE, ( char * ) szType1, ( char * ) szType2 );
             }
          }
       }
       break;

     case HB_P_DO :
     case HB_P_FUNCTION :
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );
       /* DON't put break; Has to fall through */
     case HB_P_DOSHORT :
     case HB_P_FUNCTIONSHORT :
       if( wVar == 0 )
       {
          wVar = pFunc->pCode[ ulPos + 1 ];
       }

       if( pFunc->iStackIndex < ( wVar + 2 ) )
       {
          /*
          printf( "\nNeeded %i values, found %i!\n", wVar + 1, pFunc->iStackIndex - 1 );
          */
          pFunc->iStackIndex = 1;
          pFunc->pStack[ 0 ] = ' ';
          break;
       }

       if( pFunc->iStackFunctions > 0 && pFunc->pStackFunctions[ --pFunc->iStackFunctions ] )
       {
          int hb_comp_iParamCount, iParamCount, iOptionals = 0;
          BYTE * hb_comp_cParamTypes;

          hb_comp_cParamTypes   = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->cParamTypes;
          hb_comp_iParamCount   = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->iParamCount;

          iParamCount = hb_comp_iParamCount;
          /* First, find how many optionals. */
          while ( --iParamCount >= 0 )
          {
             if( hb_comp_cParamTypes[ iParamCount ] == ( ' ' + VT_OFFSET_OPTIONAL ) || hb_comp_cParamTypes[ iParamCount ] >= ( 'A' + VT_OFFSET_OPTIONAL ) )
             {
                iOptionals++;
             }
             else
             {
                break;
             }
          }

          /*
          printf( "\nOptionals: %i\n", iOptionals );
          printf( "\nExec Function: %s, wVar: %i Parameters: %i Optionals: %i\n", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, wVar, hb_comp_iParamCount, iOptionals );
          */

          /* Now, check the types. */
          if( wVar >= ( hb_comp_iParamCount - iOptionals ) && wVar <= hb_comp_iParamCount )
          {
             BYTE iParamBase = pFunc->iStackIndex - wVar, cFormalType, cParamType;
             int iOffset = wVar;

             while ( --iOffset >= 0 )
             {
                BOOL bByRef = FALSE;

                cParamType = pFunc->pStack[ iParamBase + iOffset ];
                if( ( cParamType == '-' + VT_OFFSET_VARIANT ) || cParamType >= ( 'A' + VT_OFFSET_VARIANT ) )
                {
                   cParamType -= VT_OFFSET_VARIANT;
                }

                cFormalType = hb_comp_cParamTypes[ iOffset ];

                if( cFormalType == ( ' ' + VT_OFFSET_OPTIONAL ) || cFormalType >= ( 'A' + VT_OFFSET_OPTIONAL ) )
                {
                   cFormalType -= VT_OFFSET_OPTIONAL;
                }

                if( cFormalType == ' ' + VT_OFFSET_BYREF || cFormalType >= ( 'A' + VT_OFFSET_BYREF ) )
                {
                   bByRef = TRUE;
                   cFormalType -= VT_OFFSET_BYREF;
                }

                /* --- */

                if( cFormalType == ' ' && ! bByRef )
                {
                   /* Declared is Variant, accept anything. */
                }
                else if( cFormalType == 'S' && cParamType == 'S' && pFunc->iStackClasses )
                {
                   PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                   PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];

                   if( hb_comp_pFormalClass != hb_comp_pParamClass )
                   {
                      sprintf( ( char * ) szType1, "%s %i", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, iOffset + 1 );
                      sprintf( ( char * ) szType2, "%s", hb_comp_pFormalClass->szName );
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                   }
                }
                else if( cFormalType == 's' && cParamType == 's' && pFunc->iStackClasses )
                {
                   PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                   PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];

                   if( hb_comp_pFormalClass != hb_comp_pParamClass )
                   {
                      sprintf( ( char * ) szType1, "%s %i", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, iOffset + 1 );
                      sprintf( ( char * ) szType2, "ARRAY OF %s", hb_comp_pFormalClass->szName );
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                   }
                }
                else if( cFormalType != cParamType )
                {
                   if( cFormalType == 'S' )
                   {
                      PCOMCLASS hb_comp_pParamClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                      sprintf( ( char * ) szType2, "%s", hb_comp_pParamClass->szName );
                   }
                   else if( cFormalType == 's' )
                   {
                      PCOMCLASS hb_comp_pParamClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                      sprintf( ( char * ) szType2, "ARRAY OF %s", hb_comp_pParamClass->szName );
                   }
                   else
                   {
                      /* Cleanup. */
                      if( toupper( cParamType ) == 'S' && pFunc->iStackClasses )
                      {
                         --pFunc->iStackClasses;
                      }

                      if( bByRef )
                      {
                         sprintf( ( char * ) szType2, "@%c", cFormalType );
                      }
                      else if( islower( cFormalType ) )
                      {
                         sprintf( ( char * ) szType2, "ARRAY OF %c", toupper( cFormalType ) );
                      }
                      else
                      {
                         sprintf( ( char * ) szType2, "%c", cFormalType );
                      }
                   }

                   sprintf( ( char * ) szType1, "%s #%i", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, iOffset + 1 );

                   if( cParamType == ' ' || cParamType == '-' || cParamType == 'U' )
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
                   }
                   else if( bByRef && ( cParamType == ( ' ' + VT_OFFSET_BYREF ) || cParamType >= ( 'A' + VT_OFFSET_BYREF )  ) )
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                   }
                }
             }
          }
          else
          {
             sprintf( ( char * ) szType1, "%s got %i", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, wVar );

             if( iOptionals )
             {
                sprintf( ( char * ) szType2, "%i-%i", hb_comp_iParamCount - iOptionals, hb_comp_iParamCount );
             }
             else
             {
                sprintf( ( char * ) szType2, "%i", hb_comp_iParamCount - iOptionals );
             }
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_COUNT, ( char * ) szType1, ( char * ) szType2 );
          }
       }
       else
       {
          /*
          printf( "\nExec Non Declared Function - Stack: %i Functions: %i\n", pFunc->iStackIndex, pFunc->iStackFunctions );
          */
       }

       #if 0
       {
          int i;
          for ( i = 0; i < pFunc->iStackIndex; i++ )
          {
             printf( "\nStack: %i Type: %c", i, pFunc->pStack[ i ] );
          }
       }
       printf( "\Removing %i parameters\n", wVar );

       #endif

       /* Removing all the parameters. Return type already pushed just prior to parameters */
       pFunc->iStackIndex -= wVar;

       /* Removing the NIL */
       pFunc->iStackIndex--;

       if( pFunc->pCode[ ulPos ] == HB_P_DO || pFunc->pCode[ ulPos ] == HB_P_DOSHORT )
       {
          /* No return value. */
          pFunc->iStackIndex--;
          /*
          printf( "\nNo Type for Procedure. - Stack: %i\n", pFunc->iStackIndex );
          */
       }
       else
       {
          #if 0
          /* Declared result already on stack. */
          cType = pFunc->pStack[ pFunc->iStackIndex - 1 ];
          if( ( cType == '-' + VT_OFFSET_VARIANT ) || cType >= ( 'A' + VT_OFFSET_VARIANT ) )
          {
             cType -= VT_OFFSET_VARIANT;
          }
          printf( "\nType of Function \'%c\' - Stack: %i\n", cType, pFunc->iStackIndex );
          #endif
       }
       break;

     case HB_P_MESSAGE :
       if( pFunc->iStackIndex < 1 )
       {
          break;
       }

       cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ];

       if( ( cSubType1 == '-' + VT_OFFSET_VARIANT ) || cSubType1 >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 -= VT_OFFSET_VARIANT;
          if( cSubType1 == 'S' )
          {
             sprintf( ( char * ) szType1, "AnyType.SubType[Object]" );
          }
          else if( cSubType1 == 's' )
          {
             sprintf( ( char * ) szType1, "AnyType.SubType[ARRAY OF Object]" );
          }
          else if( islower( cSubType1 ) )
          {
             sprintf( ( char * ) szType1, "AnyType.SubType[ARRAY OF %c]", cSubType1 );
          }
          else if( cSubType1 == '-' )
          {
             strcpy( ( char * ) szType1,  "AnyType.SubType[NIL]" );
          }
          else
          {
             sprintf( ( char * ) szType1, "AnyType.SubType[%c]", cSubType1 );
          }
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", cSubType1 );
       }

       if( cSubType1 == 'O' )
          ;/* The Object is not declared. */
       else if( cSubType1 == 'S' )
       {
          pSym = hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ ulPos + 1 ] ) ) );

          if( pSym && pSym->szName && pFunc->iStackClasses && pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
          {
             if( pFunc->iStackFunctions < 8 )
             {
                pFunc->pStackFunctions[ pFunc->iStackFunctions++ ] = hb_compMethodFind( pFunc->pStackClasses[ pFunc->iStackClasses - 1 ], pSym->szName );

                /*printf( "\nMethod: %s of Class: %s Parameters: %i\n", pSym->szName, pFunc->pStackClasses[ pFunc->iStackClasses - 1 ]->szName, pFunc->pStackFunctions[ pFunc->iStackFunctions - 1 ]->iParamCount );*/

                if( pFunc->pStackFunctions[ pFunc->iStackFunctions - 1 ] == NULL )
                {
                   if( pSym->szName[0] == '_' )
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_MESSAGE_NOT_FOUND, &( pSym->szName[1] ), pFunc->pStackClasses[ pFunc->iStackClasses - 1 ]->szName );
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_MESSAGE_NOT_FOUND, pSym->szName, pFunc->pStackClasses[ pFunc->iStackClasses - 1 ]->szName );
                   }
                }
             }
          }
          else /* The method is not declared. */
          {
             if( pFunc->iStackFunctions < 8 )
             {
                pFunc->pStackFunctions[ pFunc->iStackFunctions++ ] = NULL;
             }
          }
       }
       else if( cSubType1 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "O", NULL );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "O" );
       }

       /* Result will be pushed by HB_P_SEND*/
       break;

     /* Also handled by HB_P_MESSAGE. */
     case HB_P_SEND :
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );

       /* Fall Through - don't add break !!! */

     case HB_P_SENDSHORT :
        if( wVar == 0 )
        {
          wVar = ( SHORT ) pFunc->pCode[ ulPos + 1 ];
        }

        if( pFunc->iStackIndex < ( wVar + 1 ) )
        {
          break;
        }

        cType = pFunc->pStack[ pFunc->iStackIndex - ( wVar + 1 ) ];
        if( ( cType == '-' + VT_OFFSET_VARIANT ) || cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
        {
           cType -= VT_OFFSET_VARIANT;
        }

        if( cType == 'S' &&  pFunc->iStackFunctions > 0 && pFunc->pStackFunctions[ pFunc->iStackFunctions - 1 ] )
        {
           int hb_comp_iParamCount, iParamCount, iOptionals = 0;
           BYTE * hb_comp_cParamTypes;

           hb_comp_cParamTypes = pFunc->pStackFunctions[ --pFunc->iStackFunctions ]->cParamTypes;
           hb_comp_iParamCount = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->iParamCount;

           iParamCount = hb_comp_iParamCount;

           /*
           printf( "\nExec Method: %s of Class: %s Parameters: %i\n", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, pFunc->pStackClasses[ pFunc->iStackClasses - 1 ]->szName, pFunc->pStackFunctions[ pFunc->iStackFunctions ]->iParamCount );
           */

           /* First, find how many optionals. */
           while ( --iParamCount >= 0 )
           {
              if( hb_comp_cParamTypes[ iParamCount ] == ( ' ' + VT_OFFSET_OPTIONAL ) || hb_comp_cParamTypes[ iParamCount ] >= ( 'A' + VT_OFFSET_OPTIONAL ) )
              {
                 iOptionals++;
              }
              else
              {
                 break;
              }
           }

           /*printf( "\nOptionals: %i\n", iOptionals );*/

           /*printf( "Method: %s, wVar: %i Parameters: %i Optionals: %i\n", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, wVar, hb_comp_iParamCount, iOptionals );*/

           /* Now, check the types. */
           if( wVar >= ( hb_comp_iParamCount - iOptionals ) && wVar <= hb_comp_iParamCount )
           {
              BYTE iParamBase = pFunc->iStackIndex - wVar, cFormalType;
              int iOffset = wVar;

              while ( --iOffset >= 0 )
              {
                 cFormalType = hb_comp_cParamTypes[ iOffset ];

                 /*printf( "\nFormal # %i Type: %c\n", iOffset, cFormalType );*/

                 if( cFormalType == ( ' ' + VT_OFFSET_OPTIONAL ) || cFormalType >= ( 'A' + VT_OFFSET_OPTIONAL ) )
                 {
                    cFormalType -= VT_OFFSET_OPTIONAL;
                 }

                 if( cFormalType == ' ' + VT_OFFSET_BYREF )
                 {
                    cFormalType = '@';
                 }

                 if( cFormalType == ' ' )
                   ; /* Formal is Variant, accept anything. */
                 else if( pFunc->pStack[ iParamBase + iOffset ] == '-' )
                   ; /* Parameter is NIL, always accepted. */
                 else if( cFormalType == '@' && pFunc->pStack[ iParamBase + iOffset ] >= ( 'A' + VT_OFFSET_BYREF ) )
                   ; /* Formal is ANY REFERENCE, and Parameter is SOME REFERENCE. */
                 else if( cFormalType == pFunc->pStack[ iParamBase + iOffset ] - VT_OFFSET_VARIANT )
                   ; /* Prameter is VARIANT.SubType of same as Formal. */
                 else if( cFormalType == 'S' && pFunc->pStack[ iParamBase + iOffset ] == 'S' && pFunc->iStackClasses )
                 {
                    PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                    PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];

                    if( hb_comp_pFormalClass != hb_comp_pParamClass )
                    {
                       sprintf( ( char * ) szType1, "%i", iOffset + 1 );
                       sprintf( ( char * ) szType2, "%s", hb_comp_pFormalClass->szName );
                       hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                    }
                 }
                 else if( cFormalType == 's' && pFunc->pStack[ iParamBase + iOffset ] == 's' && pFunc->iStackClasses )
                 {
                    PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                    PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];

                    if( hb_comp_pFormalClass != hb_comp_pParamClass )
                    {
                       sprintf( ( char * ) szType1, "%i", iOffset + 1 );
                       sprintf( ( char * ) szType2, "ARRAY OF %s", hb_comp_pFormalClass->szName );
                       hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                    }
                 }
                 else if( islower( cFormalType ) && pFunc->pStack[ iParamBase + iOffset ] == 'A' )
                 {
                    if( cFormalType == 'S' )
                    {
                       PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                       sprintf( ( char * ) szType2, "%s", hb_comp_pFormalClass->szName );
                    }
                    else if( cFormalType == 's' )
                    {
                       PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                       sprintf( ( char * ) szType2, "ARRAY OF %s", hb_comp_pFormalClass->szName );
                    }
                    else
                    {
                       if( cFormalType == ( '-' + VT_OFFSET_BYREF ) || cFormalType >= ( 'A' + VT_OFFSET_BYREF ) )
                       {
                          sprintf( ( char * ) szType2, "@%c", cFormalType - VT_OFFSET_BYREF );
                       }
                       else if( islower( cFormalType ) )
                       {
                          sprintf( ( char * ) szType2, "ARRAY OF %c", toupper( cFormalType ) );
                       }
                       else
                       {
                          sprintf( ( char * ) szType2, "%c", cFormalType );
                       }
                    }

                    if( pFunc->pStack[ iParamBase + iOffset ] == 'S' )
                    {
                       PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];
                       sprintf( ( char * ) szType1, "(%i) %s", iOffset + 1, hb_comp_pParamClass->szName );
                    }
                    else if( pFunc->pStack[ iParamBase + iOffset ] == 's' )
                    {
                       PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];
                       sprintf( ( char * ) szType1, "(%i) ARRAY OF %s", iOffset + 1, hb_comp_pParamClass->szName );
                    }
                    else
                    {
                       if( pFunc->pStack[ iParamBase + iOffset ] == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ iParamBase + iOffset ] >= ( 'A' + VT_OFFSET_VARIANT ) )
                       {
                          sprintf( ( char * ) szType1, "(%i) AnyType.SubType[%c]", iOffset + 1, pFunc->pStack[ iParamBase + iOffset ] - VT_OFFSET_VARIANT );
                       }
                       else if( islower( pFunc->pStack[ iParamBase + iOffset ] ) )
                       {
                          sprintf( ( char * ) szType1, "(%i) ARRAY OF %c", iOffset + 1, toupper( pFunc->pStack[ iParamBase + iOffset ] ) );
                       }
                       else
                       {
                          sprintf( ( char * ) szType1, "(%i) %c", iOffset + 1, pFunc->pStack[ iParamBase + iOffset ] );
                       }
                    }

                    hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_SUSPECT, ( char * ) szType1, ( char * ) szType2 );
                 }
                 else if( cFormalType != pFunc->pStack[ iParamBase + iOffset ] )
                 {
                    if( cFormalType == 'S' )
                    {
                       PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                       sprintf( ( char * ) szType2, "%s", hb_comp_pFormalClass->szName );
                    }
                    else if( cFormalType == 's' )
                    {
                       PCOMCLASS hb_comp_pFormalClass = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->pParamClasses[ iOffset ];
                       sprintf( ( char * ) szType2, "ARRAY OF %s", hb_comp_pFormalClass->szName );
                    }
                    else
                    {
                       if( cFormalType == ( '-' + VT_OFFSET_BYREF ) || cFormalType >= ( 'A' + VT_OFFSET_BYREF ) )
                       {
                          sprintf( ( char * ) szType2, "@%c", cFormalType - VT_OFFSET_BYREF );
                       }
                       else if( islower( cFormalType ) )
                       {
                          sprintf( ( char * ) szType2, "ARRAY OF %c", toupper( cFormalType ) );
                       }
                       else
                       {
                          sprintf( ( char * ) szType2, "%c", cFormalType );
                       }
                    }

                    if( pFunc->pStack[ iParamBase + iOffset ] == 'S' )
                    {
                       PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];
                       sprintf( ( char * ) szType1, "(%i) %s", iOffset + 1, hb_comp_pParamClass->szName );
                    }
                    else if( pFunc->pStack[ iParamBase + iOffset ] == 's' )
                    {
                       PCOMCLASS hb_comp_pParamClass = pFunc->pStackClasses[ --pFunc->iStackClasses ];
                       sprintf( ( char * ) szType1, "(%i) ARRAY OF %s", iOffset + 1, hb_comp_pParamClass->szName );
                    }
                    else
                    {
                       if( pFunc->pStack[ iParamBase + iOffset ] == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ iParamBase + iOffset ] >= ( 'A' + VT_OFFSET_VARIANT ) )
                       {
                          sprintf( ( char * ) szType1, "(%i) AnyType.SubType[%c]", iOffset + 1, pFunc->pStack[ iParamBase + iOffset ] - VT_OFFSET_VARIANT );
                       }
                       else if( islower( pFunc->pStack[ iParamBase + iOffset ] ) )
                       {
                          sprintf( ( char * ) szType1, "(%i) ARRAY OF %c", iOffset + 1, toupper( pFunc->pStack[ iParamBase + iOffset ] ) );
                       }
                       else
                       {
                          sprintf( ( char * ) szType1, "(%i) %c", iOffset + 1, pFunc->pStack[ iParamBase + iOffset ] );
                       }
                    }

                    hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE, ( char * ) szType1, ( char * ) szType2 );
                 }
              }
           }
           else
           {
              /*printf( "Method: %s, wVar: %i Parameters: %i Optionals: %i\n", pFunc->pStackFunctions[ pFunc->iStackFunctions ]->szName, wVar, hb_comp_iParamCount, iOptionals );*/
              sprintf( ( char * ) szType1, "%i", wVar );
              sprintf( ( char * ) szType2, "%i", hb_comp_iParamCount - iOptionals );
              hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_COUNT, ( char * ) szType1, ( char * ) szType2 );
           }
        }
        else if( pFunc->iStackFunctions )
        {
           --pFunc->iStackFunctions;
        }

        /* Removing all the parameters.*/
        pFunc->iStackIndex -= wVar;

        if( cType == 'S' && pFunc->pStackFunctions[ pFunc->iStackFunctions ] )
        {
           pFunc->pStack[ pFunc->iStackIndex - 1 ] = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->cType;

           /*
           printf( "\nDeclared Method!!! Stack: %i Type: %c\n", pFunc->iStackIndex, pFunc->pStack[ pFunc->iStackIndex - 1 ] );
           */

           if( toupper( pFunc->pStack[ pFunc->iStackIndex - 1 ] ) == 'S' && pFunc->iStackClasses < 8 )
           {
              /*
              printf( "\nNested CLASS!!! Stack: %i Type: %c Class: %s\n", pFunc->iStackIndex, pFunc->pStack[ pFunc->iStackIndex - 1 ], pFunc->pStackFunctions[ pFunc->iStackFunctions ]->Extended.pClass->szName );
              */
              pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pFunc->pStackFunctions[ pFunc->iStackFunctions ]->Extended.pClass;
           }
        }
        else
        {
           pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
        }

        break;

     case HB_P_DEC :
     case HB_P_INC :
       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == 'N' )
       {
          /* Ok. */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "N", NULL );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "N" );
       }
       break;

     case HB_P_JUMPNEAR :
     case HB_P_JUMP :
     case HB_P_JUMPFAR :
       /* Restoring Stack depth. */
       if( s_iCondIndex )
       {
          pFunc->iStackIndex = s_aiPreCondStack[ --s_iCondIndex ];
       }
       /*
       printf( "\nAfter Cond: %i\n", pFunc->iStackIndex );
       */
       break;

     case HB_P_JUMPFALSENEAR :
     case HB_P_JUMPFALSE :
     case HB_P_JUMPFALSEFAR :
     case HB_P_JUMPTRUENEAR :
     case HB_P_JUMPTRUE :
     case HB_P_JUMPTRUEFAR :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       /*
       printf( "\nBefore Cond: %i\n", pFunc->iStackIndex );
       */

       /* Saving Stack depth befor Jump. */
       /* TODO: Remove Hard coded limitation. */
       if( s_iCondIndex < 16 )
       {
          s_aiPreCondStack[ s_iCondIndex++ ] = pFunc->iStackIndex;
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       if( pFunc->pStack[ pFunc->iStackIndex ] == 'L' )
       {
          /* Ok. */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "L", NULL );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "L" );
       }
       break;

     case HB_P_INSTRING :
       if( pFunc->iStackIndex < 2 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' &&  pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          /* Ok. */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == 'C' &&  pFunc->pStack[ pFunc->iStackIndex - 1 ] == 'C' )
       {
          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'L';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "C", NULL  );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] != 'C' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "C" );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "C", NULL );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] != 'C' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType2, "C" );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }

       break;

     /* May be subject to Operator Overloading - don't restrict to Numeric! */
     case HB_P_DIVIDE :
     case HB_P_PLUS :
     case HB_P_MINUS :
     case HB_P_NEGATE :
     case HB_P_MULT :
     case HB_P_POWER :
       if( pFunc->iStackIndex < 2 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       /*printf( "\nTop: %c Bottom: %c Typ-SubType: %c Bottom->SubType: %c\n", pFunc->pStack[ pFunc->iStackIndex], pFunc->pStack[ pFunc->iStackIndex - 1 ], pFunc->pStack[ pFunc->iStackIndex] - 100, pFunc->pStack[ pFunc->iStackIndex - 1 ] - 100 );*/

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ] - VT_OFFSET_VARIANT;
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = pFunc->pStack[ pFunc->iStackIndex ] - VT_OFFSET_VARIANT;
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", cSubType1 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", cSubType2 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       if( ! cSubType1 )
       {
          cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ];
       }

       if( ! cSubType2 )
       {
          cSubType2 = pFunc->pStack[ pFunc->iStackIndex ];
       }

       if( cSubType1 == ' ' &&  cSubType2 == ' ' )
       {
          /* Override the last item with the new result type which is already there */
       }
       else if( cSubType1 == 'N' && cSubType2 == 'N' )
       {
          /* Override the last item with the new result type wich is already there */
       }
       else if( ( pFunc->pCode[ ulPos ] == HB_P_PLUS || pFunc->pCode[ ulPos ] == HB_P_MINUS ) &&
                  cSubType1 == cSubType2 )
       {
          /* Override the last item with the new result type wich is already there */
       }
       else if( cSubType1 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType1, ( char * ) szType2 );

          /* Override the last item with the new result type. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = cSubType2;
       }
       else if( cSubType2 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType2, ( char * ) szType1 );

          /* Override the last item with the new result type. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = cSubType1;
       }
       else if( cSubType1 == '-' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, (char *) szType1, ( char * ) szType2 );

          /* Override the last item with the new result type. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       else if( cSubType2 == '-' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, (char *) szType2, ( char * ) szType1 );

          /* Override the last item with the new result type. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERANDS_INCOMPATIBLE, ( char * ) szType1, ( char * ) szType2 );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       break;

     case HB_P_FORTEST :

       if( pFunc->iStackIndex < 3 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       /* --- */

       if( ( pFunc->pStack[ pFunc->iStackIndex - 2 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 2 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType0 = pFunc->pStack[ pFunc->iStackIndex - 2 ] - VT_OFFSET_VARIANT;
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ] - VT_OFFSET_VARIANT;
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = pFunc->pStack[ pFunc->iStackIndex ] - VT_OFFSET_VARIANT;
       }

       /* --- */

       if( cSubType0 && cSubType0 == '-' )
       {
          strcpy( ( char * ) szType0, "AnyType.SubType[NIL]" );
       }
       else if( cSubType0 )
       {
          sprintf( ( char * ) szType0, "AnyType.SubType[%c]", cSubType0 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 2 ] == '-' )
       {
          strcpy( ( char * ) szType0, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType0, "%c", pFunc->pStack[ pFunc->iStackIndex - 2 ] );
       }

       /* --- */

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", cSubType1 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       /* --- */

       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", cSubType2 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       /* --- */

       if( ! cSubType0 )
       {
          cSubType0 = pFunc->pStack[ pFunc->iStackIndex - 2 ];
       }

       if( ! cSubType1 )
       {
          cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ];
       }

       if( ! cSubType2 )
       {
          cSubType2 = pFunc->pStack[ pFunc->iStackIndex ];
       }

       /* --- */

       if( cSubType0 == 'N' )
       {
           /* Ok. */
       }
       else if ( cSubType0 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "UnKnown", "N" );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType0, "N" );
       }

       /* --- */

       if( cSubType1 == 'N' )
       {
          /* Ok. */
       }
       else if ( cSubType1 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "UnKnown", "N" );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "N" );
       }

       /* --- */

       if( cSubType2 == 'N' )
       {
           /* Ok. */
       }
       else if ( cSubType2 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "UnKnown", "N" );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType2, "N" );
       }

       /* Remove Step. */
       pFunc->iStackIndex--;

       /* Override the last item with Logical */
       pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'L';

       break;

     case HB_P_GREATER :
     case HB_P_GREATEREQUAL :
     case HB_P_LESSEQUAL :
     case HB_P_LESS :
       if( pFunc->iStackIndex < 2 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       if( pFunc->pStack[ pFunc->iStackIndex ] == '-' || pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-'  )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERANDS_INCOMPATIBLE, ( char * ) szType1, ( char * ) szType2 );

          /* Override the last item with the new result type wich is already there */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' &&  pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          /* Override the last item with the new result type which is already there */
          ;
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == pFunc->pStack[ pFunc->iStackIndex - 1 ] )
       {
          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'L';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType1, NULL );

          /* Override the last item with the new result type wich is already there */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType2, NULL );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERANDS_INCOMPATIBLE, ( char * ) szType1, ( char * ) szType2 );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }

       break;

     case HB_P_EQUAL :
     case HB_P_EXACTLYEQUAL :
     case HB_P_NOTEQUAL :
       if( pFunc->iStackIndex < 2 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }
       if( cSubType1 && cSubType1 == '-' )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", cSubType1 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       /*---*/

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
       }
       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", cSubType2 );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       /*---*/

       if( ! cSubType1 )
       {
          cSubType1 = pFunc->pStack[ pFunc->iStackIndex - 1 ];
       }

       if( ! cSubType2 )
       {
          cSubType2 = pFunc->pStack[ pFunc->iStackIndex ];
       }

       if( cSubType1 == '-' || cSubType2 == '-' )
       {
          /* Override the last item with the new result type */
       }
       else if( cSubType1 == cSubType2 )
       {
          /* Override the last item with the new result type */
       }
       else if( cSubType1 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType2, NULL );
          /* Override the last item with the new result type wich is already there */
       }
       else if( cSubType2 == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, ( char * ) szType1, NULL );
          /* Override the last item with the new result type */
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERANDS_INCOMPATIBLE, ( char * ) szType1, ( char * ) szType2 );
          /* Override the last item with the new result type */
       }

       /* Override the last item with the new result type */
       pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'L';

       break;

     case HB_P_NOT :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          strcpy( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL" );
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "L", NULL );

          /* Override the last item with the new result type which is already there */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] != 'L' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "L" );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }

       break;

     case HB_P_AND :
     case HB_P_OR :
     {
       if( pFunc->iStackIndex < 2 )
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
     }

       pFunc->iStackIndex--;

       if( ( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex - 1 ] >= 'A' + VT_OFFSET_VARIANT  )
       {
          cSubType1 = ( pFunc->pStack[ pFunc->iStackIndex - 1 ] -= VT_OFFSET_VARIANT );
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cSubType2 = ( pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT );
       }

       if( cSubType1 && cSubType1 == '-' )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[NIL]" );
       }
       else if( cSubType1 )
       {
          sprintf( ( char * ) szType1, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == '-' )
       {
          strcpy( ( char * ) szType1, "NIL");
       }
       else
       {
          sprintf( ( char * ) szType1, "%c", pFunc->pStack[ pFunc->iStackIndex - 1 ] );
       }

       if( cSubType2 && cSubType2 == '-' )
       {
          strcpy( ( char * ) szType2, "AnyType.SubType[NIL]" );
       }
       else if( cSubType2 )
       {
          sprintf( ( char * ) szType2, "AnyType.SubType[%c]", pFunc->pStack[ pFunc->iStackIndex ] );
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
       {
          strcpy( ( char * ) szType2, "NIL" );
       }
       else
       {
         sprintf( ( char * ) szType2, "%c", pFunc->pStack[ pFunc->iStackIndex ] );
       }

       if( ( pFunc->pStack[ pFunc->iStackIndex ] == ' ' &&  pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' ) ||
            ( pFunc->pStack[ pFunc->iStackIndex ] == 'L' &&  pFunc->pStack[ pFunc->iStackIndex - 1 ] == 'L' ) )
       {
          /* Override the last item with the new result type which is already there */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "L", NULL  );

          /* Override the last item with the new result type which is already there */
       }
       else if( pFunc->pStack[ pFunc->iStackIndex - 1 ] != 'L' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType1, "L" );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }
       else if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "L", NULL );

          /* Override the last item with the new result type which is already there */
       }

       else if( pFunc->pStack[ pFunc->iStackIndex ] != 'L' )
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_TYPE, ( char * ) szType2, "L" );

          /* Override the last item with the new result type */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'U';
       }

       break;

     case HB_P_DUPLICATE :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       bLast1 = pFunc->pStack[ pFunc->iStackIndex - 1 ];
       pFunc->pStack[ pFunc->iStackIndex++ ] = bLast1;
       break;

     case HB_P_DUPLTWO :
       if( pFunc->iStackIndex < 2 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       bLast1 = pFunc->pStack[ pFunc->iStackIndex - 2 ];
       bLast2 = pFunc->pStack[ pFunc->iStackIndex - 1 ];
       pFunc->pStack[ pFunc->iStackIndex++ ] = bLast1;
       pFunc->pStack[ pFunc->iStackIndex++ ] = bLast2;
       break;

     /* Explicit Types. */

     /* Objects */
     case HB_P_PUSHSELF :
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'O';

       /* Todo find Self's Class. */
       break;

     case HB_P_PUSHWITH :
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'O';

       /* Todo find With's Class. */
       break;

   #if 0
     /* Blcoks */
     case HB_P_PUSHBLOCKSHORT :
     case HB_P_PUSHBLOCK :
           break;
   #endif

     case HB_P_ENDBLOCK :
       /* Override the last value of the block left on the stack. */
           /* The last value was actualy generated on the pBlock stack, not in parrent. */
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'B';
       break;

     /* Undefined */
     case HB_P_PUSHNIL :
       pFunc->pStack[ pFunc->iStackIndex++ ] = '-';
       break;

     /* Logicals */
     case HB_P_TRUE :
     case HB_P_FALSE :
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'L';
       break;

     /* Numerics */
     case HB_P_PUSHDOUBLE :
     case HB_P_PUSHLONG :
     case HB_P_PUSHINT :
     case HB_P_PUSHBYTE :
     case HB_P_ZERO :
     case HB_P_ONE :
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'N';
       break;

     /* Charcters */
     case HB_P_PUSHSTRSHORT :
     case HB_P_PUSHSTR :
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'C';
       break;

     case HB_P_PUSHSYMNEAR :
     case HB_P_PUSHSYM :
     case HB_P_MPUSHSYM :
       /* In Private or Public statement can't be a declared function */
       if( ( hb_comp_iVarScope == VS_PRIVATE || hb_comp_iVarScope == VS_PUBLIC ) )
       {
          pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
       }
       else
       {
          if( pFunc->pCode[ ulPos ] == HB_P_PUSHSYMNEAR )
          {
             pSym = hb_compSymbolGetPos( pFunc->pCode[ ulPos + 1 ] );
          }
          else
          {
             pSym = hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ ulPos + 1 ] ) ) );
          }

          /*printf( "\nSymbol: %s\n", pSym->szName );*/

          if( pSym && pSym->szName )
          {
             pDeclared = hb_compDeclaredFind( pSym->szName );

             if( pDeclared )
             {
                pFunc->pStack[ pFunc->iStackIndex++ ] = pDeclared->cType;

                if( toupper( pDeclared->cType ) == 'S' && pFunc->iStackClasses < 8 )
                {
                   pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pDeclared->Extended.pClass;
                }
             }
             else
             {
                pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
             }

             /* Storing, will be checked by HB_P_FUNCTION, OK to store NULL */
             /* TODO don't use hard coded size */
             if( pFunc->iStackFunctions < 8 )
             {
                pFunc->pStackFunctions[ pFunc->iStackFunctions++ ] = pDeclared;
             }

             /* QUESTION: Add other "safe" functions, or remove adaptive type checking support for memvars? */
             if( strcmp( pSym->szName, "QOUT" ) == 1 )
             {
                /*printf( "\nRestting privates affected by: %s\n", pSym->szName );*/

                /* All Private Variants Subtype will be unknown after function call. */
                pVar = pFunc->pMemvars;
                while ( pVar )
                {
                   if( pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
                   {
                      pVar->cType = ' ';
                   }

                   pVar = pVar->pNext;
                }
             }

             /*
             printf( "\nPushed: %s() Type: %c Stack: %i\n", pSym->szName, pFunc->pStack[ pFunc->iStackIndex - 1], pFunc->iStackIndex - 1 );
             */
          }
          else
          {
             /* Storing, will be checked by FUNCTION, OK to store NULL */
             /* TODO don't use hard coded size */
             if( pFunc->iStackFunctions < 8 )
             {
                pFunc->pStackFunctions[ pFunc->iStackFunctions++ ] = NULL;
             }

             pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
          }
       }
       break;

     case HB_P_FUNCPTR :
        /* Previous symbol pushed no longer used as function call. */
        pFunc->pStackFunctions[ --pFunc->iStackFunctions ] = NULL;
        pFunc->pStack[ pFunc->iStackIndex - 1 ] = 'F';
        break;

     case HB_P_PUSHLOCALNEAR :
     case HB_P_PUSHLOCALREF :
     case HB_P_PUSHLOCAL :
       if( pFunc->pCode[ ulPos ] == HB_P_PUSHLOCALNEAR )
       {
          wVar = ( signed char ) pFunc->pCode[ ulPos + 1 ];
       }
       else
       {
          wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );
       }

       /* we are accesing variables within a codeblock */
       if( wVar < 0 )
       {
          /* Finding the Function owning the block. */
          pTmp = pFunc->pOwner;

          /* Might be a nested block. */
          while ( pTmp->pOwner )
          {
             pTmp = pTmp->pOwner;
          }

          pVar = pFunc->pStatics;
          while( ++wVar < 0 && pVar )
          {
             pVar = pVar->pNext;
          }

          if ( pVar )
          {
             wVar = hb_compVariableGetPos( pTmp->pLocals, pVar->szName );
             pVar = hb_compVariableFind( pTmp->pLocals, wVar );
          }
       }
       else
       {
          pVar = hb_compVariableFind( pFunc->pLocals, wVar );
       }

       if( pVar )
       {
          if( pFunc->pCode[ ulPos ] == HB_P_PUSHLOCALREF )
          {
             pVar->iUsed |= VU_INITIALIZED;
          }
          else if( ! ( pVar->iUsed & VU_INITIALIZED ) )
          {
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
          }

          /*
          printf( "\nUsed: %s\n", pVar->szName );
          */

          /* Mark as used */
          pVar->iUsed |= VU_USED;

          if( pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType >= ( 'A' + VT_OFFSET_VARIANT ) )
          {
             cType = pVar->cType - VT_OFFSET_VARIANT;
          }
          else
          {
             cType = pVar->cType;
          }

          if( toupper( cType ) == 'S' && pFunc->iStackClasses < 8 )
          {
            /* Object of declared class */
            pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pVar->Extended.pClass;
          }

          if( pFunc->pCode[ ulPos ] == HB_P_PUSHLOCALREF )
          {
             pFunc->pStack[ pFunc->iStackIndex++ ] = pVar->cType + VT_OFFSET_BYREF;
          }
          else
          {
             pFunc->pStack[ pFunc->iStackIndex++ ] = pVar->cType;
          }
       }
       else
       {
          /*
          printf( "\nCould not find Local %i in: $s\n", wVar, pFunc->szName );
          */

          if( pFunc->pCode[ ulPos ] == HB_P_PUSHLOCALREF )
          {
             pFunc->pStack[ pFunc->iStackIndex++ ] = '@';
          }
          else
          {
             pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
          }
       }
       break;

     case HB_P_PUSHSTATICREF :
     case HB_P_PUSHSTATIC :
       pTmp = hb_comp_functions.pFirst;
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );

       while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
       {
          pTmp = pTmp->pNext;
       }

       pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );

       /* Will be pushed shortly. */
       pFunc->iStackIndex++;

       if( pVar )
       {
          /*printf( "\nStatic: %s Type: %c Function: %s Found in: %s\n", pVar->szName, pVar->cType, pFunc->szName, pTmp->szName );*/

          /* Only if "private" static, since global static may be intialized elsewhere. */
          /* May have been initialized in previous execution of the function.
          if( pTmp == pFunc )
            if( ! ( pVar->iUsed & VU_INITIALIZED ) )
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
          */

          /* Mark as used */
          pVar->iUsed |= VU_USED;

          if( pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType >= ( 'A' + VT_OFFSET_VARIANT ) )
          {
             cType = pVar->cType - VT_OFFSET_VARIANT;
          }
          else
          {
             cType = pVar->cType;
          }

          if( toupper( cType ) == 'S' && pFunc->iStackClasses < 8 )
          {
             /* Object of declared class */
             pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pVar->Extended.pClass;
          }

          if( pFunc->pCode[ ulPos ] == HB_P_PUSHSTATICREF )
          {
             pFunc->pStack[ pFunc->iStackIndex - 1 ] = pVar->cType + VT_OFFSET_BYREF;
          }
          else
          {
             pFunc->pStack[ pFunc->iStackIndex - 1 ] = pVar->cType;
          }
       }
       else
       {
          if( pFunc->pCode[ ulPos ] == HB_P_PUSHSTATICREF )
          {
             pFunc->pStack[ pFunc->iStackIndex - 1 ] = '@';
          }
          else
          {
             pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
          }
       }
       break;

     case HB_P_PUSHVARIABLE :
       /* Type can not be detrmined at compile time. */
       pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
       break;

     case HB_P_PUSHALIASEDVAR :
       /* TODO check what is aliased var. */
       pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
       break;

     case HB_P_PUSHALIASEDFIELDNEAR :
       pSym = hb_compSymbolGetPos( pFunc->pCode[ ulPos + 1 ] );
       /* Fall through - don't add break */

     case HB_P_PUSHALIASEDFIELD :
     case HB_P_PUSHFIELD :
       if( ! pSym )
       {
          pSym = hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ ulPos + 1 ] ) ) );
       }

       if( pSym && pSym->szName && pFunc->pFields )
       {
         wVar = hb_compVariableGetPos( pFunc->pFields, pSym->szName );
         if( wVar )
         {
           pVar = hb_compVariableFind( pFunc->pFields, wVar );
         }
       }

       /* Fall through - don't add break */

     case HB_P_PUSHMEMVARREF :
     case HB_P_PUSHMEMVAR :
       if( ! pSym )
       {
          pSym = hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ ulPos + 1 ] ) ) );
       }

       if( pSym )
       {
          if( pFunc->pCode[ ulPos ] == HB_P_PUSHMEMVAR && pSym->szName )
          {
             if( pFunc->pMemvars )
             {
                wVar = hb_compVariableGetPos( pFunc->pMemvars, pSym->szName );
             }

             if( wVar )
             {
                pVar = hb_compVariableFind( pFunc->pMemvars, wVar );
             }

             if( ! pVar )
             {
                pVar = hb_compPrivateFind( pSym->szName );
             }

             if( ( ! pVar ) && hb_comp_functions.pFirst->pMemvars )
             {
                wVar = hb_compVariableGetPos( hb_comp_functions.pFirst->pMemvars, pSym->szName );
                if( wVar )
                {
                   pVar = hb_compVariableFind( hb_comp_functions.pFirst->pMemvars, wVar );
                   /* May have been initialized in any other function - can't check. */
                   pVar->iUsed |= VU_INITIALIZED;
                }
             }
          }

          if( pVar )
          {
             cType = pVar->cType;

             /*printf( "\nPushed: %s Type: %c SubType: %c\n", pVar->szName, pVar->cType, pVar->cType - 100 );*/

             if( pFunc->pCode[ ulPos ] == HB_P_PUSHMEMVARREF )
             {
                pVar->iUsed |= VU_INITIALIZED;
             }
             else if( ! ( pVar->iUsed & VU_INITIALIZED ) )
             {
                hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
             }

             if( cType  == ( '-' + VT_OFFSET_VARIANT ) || cType >= ( 'A' + VT_OFFSET_VARIANT ) )
             {
                cType -= VT_OFFSET_VARIANT;
             }

             /* Mark as used */
             pVar->iUsed |= VU_USED;

             if( pFunc->pCode[ ulPos ] == HB_P_PUSHMEMVARREF )
             {
                pFunc->pStack[ pFunc->iStackIndex - 1 ] = pVar->cType + VT_OFFSET_BYREF;
             }
             else if( toupper( cType ) == 'S' && pFunc->iStackClasses < 8 )
             {
                /* Object of declared class */
                pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pVar->Extended.pClass;
                pFunc->pStack[ pFunc->iStackIndex++ ] = pVar->cType;
             }
             else
             {
                pFunc->pStack[ pFunc->iStackIndex - 1 ] = pVar->cType;
             }
          }
          else
          {
             cType = pSym->cType;

             /*printf( "\nPushed Symbol: %s Type: %c SubType: %c\n", pSym->szName, pSym->cType, pSym->cType - 100 );*/

             if( cType  == ( '-' + VT_OFFSET_VARIANT ) || cType >= ( 'A' + VT_OFFSET_VARIANT ) )
             {
                cType -= VT_OFFSET_VARIANT;
             }

             if( pFunc->pCode[ ulPos ] == HB_P_PUSHMEMVARREF )
             {
                pFunc->pStack[ pFunc->iStackIndex - 1 ] = pSym->cType + VT_OFFSET_BYREF;
             }
             else if( toupper( cType ) == 'S' && pFunc->iStackClasses < 8 )
             {
                /* Object of declared class */
                pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pSym->Extended.pClass;
                pFunc->pStack[ pFunc->iStackIndex++ ] = pSym->cType;
             }
             else
             {
                pFunc->pStack[ pFunc->iStackIndex - 1 ] = pSym->cType;
             }
          }
       }
       else
       {
         if( pFunc->pCode[ ulPos ] == HB_P_PUSHMEMVARREF )
         {
            pFunc->pStack[ pFunc->iStackIndex++ ] = '@';
         }
         else
         {
            pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
         }
       }

       break;

     /* Arrays. */

     case HB_P_ARRAYDIM :
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );

       if( pFunc->iStackIndex < wVar )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       /* Removing the dimensions sizes. */
       pFunc->iStackIndex -= wVar;

       /* Push type array of NILs (empty array). */
       pFunc->pStack[ pFunc->iStackIndex++ ] = '-' + 100;

       /*printf( "\nPushed array at: %i\n", pFunc->iStackIndex - 1 );*/
       break;

     case HB_P_ARRAYGEN :
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );

       /* TODO Error Message after finalizing all possible pcodes. */
       if( pFunc->iStackIndex < wVar )
       {
          pFunc->iStackIndex = 1;
          pFunc->pStack[ 0 ] = 'A';
          break;
       }

           /* Pop the Elements. */
       pFunc->iStackIndex -= wVar;

           /* Push the array. */
       pFunc->pStack[ pFunc->iStackIndex++ ] = 'A';
       break;

     case HB_P_ARRAYPUSH :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       /* Poping the Array Index. */
       pFunc->iStackIndex--;

       cType = pFunc->pStack[ pFunc->iStackIndex - 1 ];

       /*printf( "\n Base Type: %c\n", cType );*/

       if( cType  == ( '-' + VT_OFFSET_VARIANT ) || cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
       {
          cType -= VT_OFFSET_VARIANT;
       }

       if( cType == ' ' )
       {
          /* Type unknown. */
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "A", NULL );
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       }
       else if( cType == 'A' )
       {
          /* Variant Array Element - Type unknown. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       }
       else if( cType == 'a' )
       {
          /* Variant Array Element - Type unknown. */
          pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       }
       else if( islower( cType ) )
       {
         /* Now we have the declared array element on the stack.*/
         pFunc->pStack[ pFunc->iStackIndex - 1 ] = toupper( cType );
       }
       else
       {
          hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_ARRAY, NULL, NULL );
       }
       break;

     case HB_P_ARRAYPOP :
       if( pFunc->iStackIndex < 3 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       /* Poping the Array Index. */
       pFunc->iStackIndex--;

       {
          BYTE cElementType = pFunc->pStack[ pFunc->iStackIndex - 2 ];

          cType = pFunc->pStack[ pFunc->iStackIndex - 1 ];

          if( cType  == ( '-' + VT_OFFSET_VARIANT ) || cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
          {
             cType -= VT_OFFSET_VARIANT;
          }

          if( cElementType  == ( '-' + VT_OFFSET_VARIANT ) || cElementType  >= ( 'A' + VT_OFFSET_VARIANT ) )
          {
             cElementType -= VT_OFFSET_VARIANT;
          }

          if( cType == ' ' )
          {
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_OPERAND_SUSPECT, "A", NULL );
          }
          else if( cType == 'A' )
          {
             /* Array of variant can hold any value. */
          }
          else if( cType == 'a' )
          {
             /* Array of variant can hold any value. */
          }
          else if( islower( cType ) && cElementType == ' ' )
          {
             /* Array Of explicit type. */
             char szType[2];

             sprintf( ( char * ) szType, "%c", toupper( cType ) );
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
          }
          else if( islower( cType ) && toupper( cType ) != cElementType && cElementType != '-' )
          {
             /* Array Of explicit type. */
             char szType[2];

             sprintf( ( char * ) szType, "%c", toupper( cType ) );
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, ( char * ) szType, NULL );
          }
          else
          {
             hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_ARRAY, NULL, NULL );
          }
       }

       /* Poping the Assigned Value. */
       pFunc->iStackIndex--;

       break;

     /* Macros type unknown */
     case HB_P_MPUSHALIASEDFIELD :
     case HB_P_MPUSHALIASEDVAR :
     case HB_P_MPUSHFIELD :
     case HB_P_MPUSHMEMVAR :
     case HB_P_MPUSHMEMVARREF :
     case HB_P_MPUSHVARIABLE :
     case HB_P_MACROPUSHALIASED :
     case HB_P_MACROPUSH :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       /* Replace the value of the macro expression with unknown result of expanded macro. */
       pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       break;

     case HB_P_MACROSYMBOL :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       /* Replace Macro Variable Symbol Name type with unknown type of expanded macro Function Call */
       pFunc->pStack[ pFunc->iStackIndex - 1 ] = ' ';
       break;

     case HB_P_MACROTEXT :
       /* Stack already has type C. */
       /*pFunc->pStack[ pFunc->iStackIndex ] = 'C';*/
       break;

     /*-----------------4/26/00 0:15AM-------------------
      *  Begin POP Check and Remove from Stack.
      * --------------------------------------------------*/

     case HB_P_POP :
     case HB_P_POPALIAS :
       pFunc->iStackIndex--;
       break;

     case HB_P_POPVARIABLE :
       /* Ambigious Variable, will be determined only in Run Time! */
       pFunc->iStackIndex--;

       if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
       {
          --pFunc->iStackClasses;
       }

       break;

     case HB_P_POPALIASEDVAR :
       /* Ambigious Variable, will be determined only in Run Time! */
       pFunc->iStackIndex--;

       if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
       {
          --pFunc->iStackClasses;
       }

       break;

     case HB_P_POPALIASEDFIELDNEAR :
       if( pFunc->pCode[ ulPos ] == HB_P_POPALIASEDFIELDNEAR )
       {
          wVar = ( SHORT ) pFunc->pCode[ ulPos + 1 ];
          pSym = hb_compSymbolGetPos( wVar );
       }

       /* Fall through, don't put break!!!*/

     case HB_P_POPALIASEDFIELD :
     case HB_P_POPFIELD :
       if( pFunc->pCode[ ulPos ] == HB_P_POPFIELD || pFunc->pCode[ ulPos ] == HB_P_POPALIASEDFIELD )
       {
          wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );
          pSym = hb_compSymbolGetPos( wVar );
       }

       /*printf( "\nField: %s Pos: %i", pSym->szName, wVar );*/

       /* For fall through as well */
       if( pSym && pSym->szName && pFunc->pFields )
       {
          wVar = hb_compVariableGetPos( pFunc->pFields, pSym->szName );
          if( wVar )
          {
             pVar = hb_compVariableFind( pFunc->pFields, wVar );
          }
       }

       if( ( ( ! wVar ) || ( ! pVar ) ) && pSym && pSym->szName && hb_comp_functions.pFirst->pFields )
       {
          wVar = hb_compVariableGetPos( hb_comp_functions.pFirst->pFields, pSym->szName );
          pVar = hb_compVariableFind( hb_comp_functions.pFirst->pFields, wVar );
       }

       /* Fall through, don't put break!!!*/

     case HB_P_POPMEMVAR :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       if( pFunc->pCode[ ulPos ] == HB_P_POPMEMVAR )
       {
          wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );
       }

       if( ! pSym )
       {
          pSym = hb_compSymbolGetPos( wVar );
       }

       /*
       if( pFunc->pMemvars )
       {
          printf( "\nSymbol: %s #%li Function: %s which HAS memvars\n", pSym->szName, wVar, pFunc->szName );
       }

       if( pFunc->pPrivates )
       {
          printf( "\nSymbol: %s #%li Function: %s which HAS privates\n", pSym->szName, wVar, pFunc->szName );
       }
       */

       if( pSym )
       {
          if( pFunc->pCode[ ulPos ] == HB_P_POPMEMVAR && pSym->szName )
          {
            if( pFunc->pMemvars )
            {
               wVar = hb_compVariableGetPos( pFunc->pMemvars, pSym->szName );
            }

            if( wVar )
            {
               pVar = hb_compVariableFind( pFunc->pMemvars, wVar );
            }

            if( ! pVar )
            {
               pVar = hb_compPrivateFind( pSym->szName );
            }

            if( ( ! pVar ) && hb_comp_functions.pFirst->pMemvars )
            {
              wVar = hb_compVariableGetPos( hb_comp_functions.pFirst->pMemvars, pSym->szName );
              if( wVar )
              {
                 pVar = hb_compVariableFind( hb_comp_functions.pFirst->pMemvars, wVar );
              }
            }
          }

          if( pVar )
          {
             pVar->iUsed |= VU_INITIALIZED;

             /*printf( "\nSymbol: %s Variable: %s Type: %c #%i Function: %s\n", pSym->szName, pVar->szName, pVar->cType, wVar, pFunc->szName );*/

             /* Allow any type into a Variant, and record the subtype */
             if( pVar->cType == ' ' || pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
             {
                if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   pVar->cType = ' ';
                }
                else if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
                {
                   pVar->cType = pFunc->pStack[ pFunc->iStackIndex ];
                }
                else
                {
                   pVar->cType = pFunc->pStack[ pFunc->iStackIndex ] + VT_OFFSET_VARIANT;
                }

                /* Will need the Class Handle. */
                cType = pVar->cType;
                if( cType == ( '-' + VT_OFFSET_VARIANT  ) || cType >= ( 'A' + VT_OFFSET_VARIANT  ) )
                {
                   cType -= VT_OFFSET_VARIANT;
                }

                if( toupper( cType ) == 'S' && pFunc->iStackClasses )
                {
                   pVar->Extended.pClass = pFunc->pStackClasses[ pFunc->iStackClasses - 1 ];
                }
                else
                {
                   pVar->Extended.pClass = NULL;
                }

                /*
                printf( "\nSymbol: %s Variable: %s Assigned Type: \'%c\' SubType: %c #%i Stack: %i\n", pSym->szName, pVar->szName, pVar->cType, pVar->cType - 100, wVar, pFunc->iStackIndex );
                */
             }
             else
             {
                char szType[2];
                if( pVar->cType == 'S' )
                {
                   sprintf( ( char * ) szType, "%s", pVar->Extended.pClass->szName );
                }
                else if( pVar->cType == 's' )
                {
                   sprintf( ( char * ) szType, "ARRAY OF %s", pVar->Extended.pClass->szName );
                }
                else if( islower( pVar->cType ) )
                {
                   sprintf( ( char * ) szType, "ARRAY OF %c", toupper( pVar->cType ) );
                }
                else
                {
                   sprintf( ( char * ) szType, "%c", pVar->cType );
                }

                /*printf( "Variable: %s Type: \'%c\' SubType: %c Comparing: %c Recorded: %s\n", pSym->szName, pVar->cType, pVar->cType - 100, pFunc->pStack[ pFunc->iStackIndex ], ( char * ) szType );*/

                if( hb_comp_cCastType == ' ' )
                   ; /* No casting - do nothing. */
                else if( toupper( hb_comp_cCastType ) == 'S' )
                {
                   PCOMCLASS pClass = hb_compClassFind( hb_comp_szFromClass );

                   if( pClass )
                   {
                      if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
                      {
                         pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] = pClass;
                      }
                      else
                      {
                         pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pClass;
                      }
                      pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, pVar->szName );
                      pFunc->pStack[ pFunc->iStackIndex ] = ( isupper(  ( int ) hb_comp_cCastType ) ? 'O' : 'o' );
                   }

                   hb_comp_cCastType = ' ';
                }
                else
                {
                   pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
                   hb_comp_cCastType = ' ';
                }

                if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
                {
                   pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT;
                }

                if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
                {
                   /* NIL allowed into all types */
                }
                else if( pVar->cType == 'a' && islower( pFunc->pStack[ pFunc->iStackIndex ] ) )
                {
                   /* Array Of ANYTYPE may accept any Array  */
                }
                else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
                }
                else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == 'A' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
                }
                else if( islower( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, ( char * ) szType, NULL );
                }
                else if( pVar->cType != ' ' && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
                }
                else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'O' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
                }
                else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 'o' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
                }
                else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'S' )
                {
                   if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
                   {
                    /* Same class */
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
                   }
                }
                else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 's' )
                {
                   if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
                   {
                    /* Same class */
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
                   }
                }
                else if( isupper( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
                }
             }
          }
          else
          {
             /* Allow any type into a Variant, and record the subtype */
             if( pSym->cType == ' ' || pSym->cType  == ( '-' + VT_OFFSET_VARIANT ) || pSym->cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
             {
                if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   pSym->cType = ' ';
                }
                else if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
                {
                   pSym->cType = pFunc->pStack[ pFunc->iStackIndex ];
                }
                else
                {
                   pSym->cType = pFunc->pStack[ pFunc->iStackIndex ] + VT_OFFSET_VARIANT;
                }

                /* Will need the Class Handle. */
                if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
                {
                   pSym->Extended.pClass = pFunc->pStackClasses[ pFunc->iStackClasses - 1 ];
                }
             }
             else
             {
                char szType[2];

                if( pSym->cType == 'S' )
                {
                   sprintf( ( char * ) szType, "%s", pSym->Extended.pClass->szName );
                }
                else if( pSym->cType == 's' )
                {
                   sprintf( ( char * ) szType, "ARRAY OF %s", pSym->Extended.pClass->szName );
                }
                else if( islower( pSym->cType ) )
                {
                   sprintf( ( char * ) szType, "ARRAY OF %c", toupper( pSym->cType ) );
                }
                else
                {
                   sprintf( ( char * ) szType, "%c", pSym->cType );
                }

                if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
                {
                   pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT;
                }

                if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
                {
                   /* NIL allowed into all types */
                }
                else if( pSym->cType == 'a' && islower( pFunc->pStack[ pFunc->iStackIndex ] ) )
                {
                   /* Array Of ANYTYPE may accept any Array  */
                }
                else if( islower( pSym->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
                }
                else if( islower( pSym->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == 'A' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
                }
                else if( islower( pSym->cType ) && pSym->cType != pFunc->pStack[ pFunc->iStackIndex ] )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, ( char * ) szType, NULL );
                }
                else if( pSym->cType != ' ' && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pSym->szName, ( char * ) szType );
                }
                else if( pSym->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'O' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pSym->szName, ( char * ) szType );
                }
                else if( pSym->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 'o' )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, pSym->szName, ( char * ) szType );
                }
                else if( pSym->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'S' )
                {
                   if( pFunc->iStackClasses && pSym->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
                   {
                      /* Same class */
                   }
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pSym->szName, ( char * ) szType );
                   }
                }
                else if( pSym->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 's' )
                {
                   if( pFunc->iStackClasses && pSym->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
                      ; /* Same class */
                   else
                   {
                      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, pSym->szName, ( char * ) szType );
                   }
                }
                else if( isupper( pSym->cType ) && pSym->cType != pFunc->pStack[ pFunc->iStackIndex ] )
                {
                   hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pSym->szName, ( char * ) szType );
                }
             }
          }
       }

       /* Resetting */
       if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
       {
          --pFunc->iStackClasses;
       }

       break;

     case HB_P_POPLOCALNEAR :
     case HB_P_POPLOCAL :
       /* TODO Error Message after finalizing all possible pcodes. */
       if( pFunc->iStackIndex < 1 )
       {
          printf( "Oops - Stack: %i\n", pFunc->iStackIndex );
          break;
       }

       pFunc->iStackIndex--;

       if( pFunc->pCode[ ulPos ] == HB_P_POPLOCAL )
       {
          wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );
       }
       else
       {
          wVar = ( SHORT ) pFunc->pCode[ ulPos + 1 ];
       }

       /* we are accesing variables within a codeblock */
       if( wVar < 0 )
       {
          /* Finding the Function owning the block. */
          pTmp = pFunc->pOwner;

          /* Might be a nested block. */
          while ( pTmp->pOwner )
          {
             pTmp = pTmp->pOwner;
          }

          pVar = pFunc->pStatics;
          while( ++wVar < 0 && pVar )
          {
             pVar = pVar->pNext;
          }

          if ( pVar )
          {
             wVar = hb_compVariableGetPos( pTmp->pLocals, pVar->szName );
             pVar = hb_compVariableFind( pTmp->pLocals, wVar );
          }
       }
       else
       {
          pVar = hb_compVariableFind( pFunc->pLocals, wVar );
       }

       if( pVar )
       {
         pVar->iUsed |= VU_INITIALIZED;

         /* Allow any type into a Variant, and record the subtype */
         if( pVar->cType == ' ' || pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
         {
            if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               pVar->cType = ' ';
            }
            else if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
            {
               pVar->cType = pFunc->pStack[ pFunc->iStackIndex ];
            }
            else
            {
               pVar->cType = pFunc->pStack[ pFunc->iStackIndex ] + VT_OFFSET_VARIANT;
            }

            cType = pVar->cType;
            if( cType == ( '-' + VT_OFFSET_VARIANT ) || cType >= ( 'A' + VT_OFFSET_VARIANT ) )
            {
               cType -= VT_OFFSET_VARIANT;
            }

            if( toupper( cType ) == 'S' && pFunc->iStackClasses )
            {
               pVar->Extended.pClass = pFunc->pStackClasses[ pFunc->iStackClasses - 1 ];
            }
            else
            {
               pVar->Extended.pClass = NULL;
            }
         }
         else
         {
            char szType[2];

            if( pVar->cType == 'S' )
            {
               sprintf( ( char * ) szType, "%s", pVar->Extended.pClass->szName );
            }
            else if( pVar->cType == 's' )
            {
               sprintf( ( char * ) szType, "ARRAY OF %s", pVar->Extended.pClass->szName );
            }
            else if( islower( pVar->cType ) )
            {
               sprintf( ( char * ) szType, "ARRAY OF %c", toupper( pVar->cType ) );
            }
            else
            {
               sprintf( ( char * ) szType, "%c", pVar->cType );
            }

            if( hb_comp_cCastType == ' ' )
               ; /* No casting - do nothing. */
            else if( toupper( hb_comp_cCastType ) == 'S' )
            {
               PCOMCLASS pClass = hb_compClassFind( hb_comp_szFromClass );

               if( pClass )
               {
                  if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
                  {
                     pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] = pClass;
                  }
                  else
                  {
                     pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pClass;
                  }
                  pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
               }
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, pVar->szName );
                  pFunc->pStack[ pFunc->iStackIndex ] = ( isupper(  ( int ) hb_comp_cCastType ) ? 'O' : 'o' );
               }

               hb_comp_cCastType = ' ';
            }
            else
            {
               pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
               hb_comp_cCastType = ' ';
            }

            if( pFunc->pStack[ pFunc->iStackIndex ] == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ] >= ( 'A' + VT_OFFSET_VARIANT ) )
            {
               pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT;
            }

            if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
            {
               /* NIL allowed into all types */
            }
            else if( pVar->cType == 'a' && islower( pFunc->pStack[ pFunc->iStackIndex ] ) )
            {
               /* Array Of ANYTYPE may accept any Array  */
            }
            else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
            }
            else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == 'A' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
            }
            else if( islower( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, ( char * ) szType, NULL );
            }
            else if( pVar->cType != ' ' && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'O' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 'o' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'S' )
            {
               if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
               {
                  /* Same class */
               }
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
               }
            }
            else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 's' )
            {
               if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
                 ; /* Same class */
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
               }
            }
            else if( isupper( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
            }
         }
       }

       /* Resetting */
       if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
       {
          --pFunc->iStackClasses;
       }

       break;

     case HB_P_POPSTATIC :
       if( pFunc->iStackIndex < 1 )
       {
          /* TODO Error Message after finalizing all possible pcodes. */
          break;
       }

       pFunc->iStackIndex--;

       pTmp = hb_comp_functions.pFirst;
       wVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ ulPos + 1 ] ) );

       while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
       {
          pTmp = pTmp->pNext;
       }

       pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );

       if( pVar )
       {
          pVar->iUsed |= VU_INITIALIZED;

         /* Allow any type into a Variant, and record the subtype */
         if( pVar->cType == ' ' || pVar->cType  == ( '-' + VT_OFFSET_VARIANT ) || pVar->cType  >= ( 'A' + VT_OFFSET_VARIANT ) )
         {
            if( pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               pVar->cType = ' ';
            }
            else if( pFunc->pStack[ pFunc->iStackIndex ]  == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ]  >= ( 'A' + VT_OFFSET_VARIANT ) )
            {
               pVar->cType = pFunc->pStack[ pFunc->iStackIndex ];
            }
            else
            {
               pVar->cType = pFunc->pStack[ pFunc->iStackIndex ] + VT_OFFSET_VARIANT;
            }

            cType = pVar->cType;
            if( cType == ( '-' + VT_OFFSET_VARIANT  ) || cType >= ( 'A' + VT_OFFSET_VARIANT  ) )
            {
               cType -= VT_OFFSET_VARIANT;
            }
            if( toupper( cType ) == 'S' && pFunc->iStackClasses )
            {
               pVar->Extended.pClass = pFunc->pStackClasses[ pFunc->iStackClasses - 1 ];
            }
            else
            {
               pVar->Extended.pClass = NULL;
            }

            /*
            printf( "\nStack: %c Asc: %i, Var: %c Asc: %i\n", pFunc->pStack[ pFunc->iStackIndex ], pFunc->pStack[ pFunc->iStackIndex ], pVar->cType, pVar->cType );
            */
         }
         else
         {
            char szType[2];
            if( pVar->cType == 'S' )
            {
               sprintf( ( char * ) szType, "%s", pVar->Extended.pClass->szName );
            }
            else if( pVar->cType == 's' )
            {
               sprintf( ( char * ) szType, "ARRAY OF %s", pVar->Extended.pClass->szName );
            }
            else if( islower( pVar->cType ) )
            {
               sprintf( ( char * ) szType, "ARRAY OF %c", toupper( pVar->cType ) );
            }
            else
            {
               sprintf( ( char * ) szType, "%c", pVar->cType );
            }

            if( hb_comp_cCastType == ' ' )
               ; /* No casting - do nothing. */
            else if( toupper( hb_comp_cCastType ) == 'S' )
            {
               PCOMCLASS pClass = hb_compClassFind( hb_comp_szFromClass );

               if( pClass )
               {
                  if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
                  {
                     pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] = pClass;
                  }
                  else
                  {
                     pFunc->pStackClasses[ pFunc->iStackClasses++ ] = pClass;
                  }
                  pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
               }
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, pVar->szName );
                  pFunc->pStack[ pFunc->iStackIndex ] = ( isupper(  ( int ) hb_comp_cCastType ) ? 'O' : 'o' );
               }

               hb_comp_cCastType = ' ';
            }
            else
            {
               pFunc->pStack[ pFunc->iStackIndex ] = hb_comp_cCastType;
               hb_comp_cCastType = ' ';
            }

            if( pFunc->pStack[ pFunc->iStackIndex ] == ( '-' + VT_OFFSET_VARIANT ) || pFunc->pStack[ pFunc->iStackIndex ] >= ( 'A' + VT_OFFSET_VARIANT ) )
            {
               pFunc->pStack[ pFunc->iStackIndex ] -= VT_OFFSET_VARIANT;
            }

            if( pFunc->pStack[ pFunc->iStackIndex ] == '-' )
            {
               /* NIL allowed into all types */
            }
            else if( pVar->cType == 'a' && islower( pFunc->pStack[ pFunc->iStackIndex ] ) )
            {
               /* Array Of ANYTYPE may accept any Array  */
            }
            else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
            }
            else if( islower( pVar->cType ) && pFunc->pStack[ pFunc->iStackIndex ] == 'A' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, ( char * ) szType, NULL );
            }
            else if( islower( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, ( char * ) szType, NULL );
            }
            else if( pVar->cType != ' ' && pFunc->pStack[ pFunc->iStackIndex ] == ' ' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'O' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 'o' )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_SUSPECT, pVar->szName, ( char * ) szType );
            }
            else if( pVar->cType == 'S' && pFunc->pStack[ pFunc->iStackIndex ] == 'S' )
            {
               if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
               {
                  /* Same class */
               }
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
               }
            }
            else if( pVar->cType == 's' && pFunc->pStack[ pFunc->iStackIndex ] == 's' )
            {
               if( pFunc->iStackClasses && pVar->Extended.pClass == pFunc->pStackClasses[ pFunc->iStackClasses - 1 ] )
               {
                  /* Same class */
               }
               else
               {
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ARRAY_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
               }
            }
            else if( isupper( pVar->cType ) && pVar->cType != pFunc->pStack[ pFunc->iStackIndex ] )
            {
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGN_TYPE, pVar->szName, ( char * ) szType );
            }
         }
       }

       /* Resetting */
       if( toupper( pFunc->pStack[ pFunc->iStackIndex ] ) == 'S' && pFunc->iStackClasses )
       {
         --pFunc->iStackClasses;
       }
       break;

     /* Macros Undefined Types */

     case HB_P_MPOPALIASEDFIELD :
     case HB_P_MPOPALIASEDVAR :
     case HB_P_MPOPFIELD :
     case HB_P_MPOPMEMVAR :
     case HB_P_MACROPOP :
     case HB_P_MACROPOPALIASED :
       pFunc->iStackIndex--;
       break;

     case HB_P_SEQRECOVER :
       /* TODO: find type of BREAK() */
       pFunc->pStack[ pFunc->iStackIndex++ ] = ' ';
       break;
   }

   /* TODO Error or trace messages when completed. */
   if( pFunc->iStackIndex < 0 )
   {
      printf( "\nStrongType Stack underflow!\n" );
      pFunc->iStackIndex = 0;
   }
#endif
}

void hb_compGenPCode1( BYTE byte )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
   {
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );
   }

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;

   if( hb_comp_iWarnings >= 3 )
   {
      hb_compStrongType( 1 );
   }
}

void hb_compGenPData1( BYTE byte )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
   {
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );
   }

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;
}

void hb_compGenPCode2( BYTE byte1, BYTE byte2, BOOL bStackAffected )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 2 )
   {
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );
   }

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;

   if( hb_comp_iWarnings >= 3 && bStackAffected )
   {
      hb_compStrongType( 2 );
   }
}

void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3, BOOL bStackAffected )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 3 )
   {
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );
   }

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;

   if( hb_comp_iWarnings >= 3 && bStackAffected  )
   {
      hb_compStrongType( 3 );
   }
}

void hb_compGenPCode4( BYTE byte1, BYTE byte2, BYTE byte3, BYTE byte4, BOOL bStackAffected )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 4 )
   {
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );
   }

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte4;

   if( hb_comp_iWarnings >= 3 && bStackAffected  )
   {
      hb_compStrongType( 4 );
   }
}

void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize, BOOL bStackAffected )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->lPCodeSize = ( ( ulSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK;
      pFunc->pCode      = ( BYTE * ) hb_xgrab( pFunc->lPCodeSize );
      pFunc->lPCodePos  = 0;
   }
   else if( pFunc->lPCodePos + ulSize > pFunc->lPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->lPCodeSize += ( ( ( ulSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK );
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->lPCodePos, pBuffer, ulSize );
   pFunc->lPCodePos += ulSize;

   if( hb_comp_iWarnings >= 3 && bStackAffected  )
   {
      hb_compStrongType( ulSize );
   }
}

