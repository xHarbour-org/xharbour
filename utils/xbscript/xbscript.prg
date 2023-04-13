/*
 * xbScript Project source code:
 * Script Interpreter/ Dot Prompt Console / Pre-Processor
 *
 * Copyright 2000-2005 Ron Pinkas <ron@xharbour.com>
 * www - http://www.xbScript.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 * Please note: You are NOT allowed to include any portions of the source
 * files (xbscript.prg, xbs_harb.ch, rp_run.src, and rp_dot.src) into
 * any propriatary (non GPL) application, except if you purchase an alternate
 * Typical Retail License (TRL).
 *
 * For any Licensing questions please contact the author at <ron@xharbour.com>
 *
 */

#pragma w2

#include "error.ch"

#ifdef PP_QUIET
   #COMMAND @ Row(), 0 SAY <nLine> =>
#else
   #COMMAND @ Row(), 0 SAY <nLine> => IIF( <nLine> % 50 == 0, ( DevPos( Row(), 0 ), DevOut( <nLine> ) ), )
#endif

#DEFINE MAX_CICLES 256
#DEFINE PP_BUFFER_SIZE 8192 //16384

//#define PP_OP_NOOP      0
#define PP_OP_JUMP        1
#define PP_OP_TRY         2
#define PP_OP_ENDTRY      3
#define PP_OP_BEGIN       4
#define PP_OP_ENDBEGIN    5
#define PP_OP_FOREACH     6
#define PP_OP_LOOPFOREACH 7
#define PP_OP_ENDFOREACH  8

#ifdef __CLIP__
   #ifdef __LINUX__
      #define __PLATFORM__UNIX
   #endif
#endif

#ifdef __PLATFORM__UNIX
   #define EOL Chr(10)

   #ifndef OS_PATH_DELIMITER
      #define OS_PATH_DELIMITER '/'
   #endif
   #ifndef OS_PATH_LIST_SEPARATOR
      #define OS_PATH_LIST_SEPARATOR ':'
   #endif
   #ifndef OS_PATH_DELIMITER_LIST
      #define OS_PATH_DELIMITER_LIST "/"
   #endif
#else
   #define EOL Chr(13) + Chr(10)

   #ifndef OS_PATH_DELIMITER
      #define OS_PATH_DELIMITER '\'
   #endif
   #ifndef OS_PATH_LIST_SEPARATOR
      #define OS_PATH_LIST_SEPARATOR ';'
   #endif
   #ifndef OS_PATH_DELIMITER_LIST
      #define OS_PATH_DELIMITER_LIST "\/:"
   #endif
#endif

#ifdef __HARBOUR__

   REQUEST dbClearRel
   REQUEST DBFFPT

   #includ "hbmacro.ch"

   // Enable extended syntax.
   #ifdef __XHARBOUR__
      #ifndef NODYN
         #ifndef DYN
            #define DYN
         #endif
      #endif

      #ifndef NOMACRO
         #define __MACRO_COMPILE__
      #endif

      #ifndef NOCONCILEPCODE
         #define __CONCILE_PCODE__
      #endif

      #ifdef __MACRO_COMPILE__
        #define HB_MACRO_GEN_POP         2
        #define HB_MACRO_GEN_LIST       32
        #define HB_MACRO_GEN_STATEMENT 256
        #define PP_CONTEXT_STATEMENT   HB_MACRO_GEN_STATEMENT //| HB_MACRO_GEN_LIST
      #endif

      #ifdef __CONCILE_PCODE__
        #ifndef __MACRO_COMPILE__
           #define __MACRO_COMPILE__
        #endif

        #ifndef DYN
           #define DYN
        #endif

        #define HB_P_ENDPROC         7
        #define HB_P_FORTEST        10
        #define HB_P_GREATEREQUAL   16
        #define HB_P_JUMP           26
        #define HB_P_JUMPFALSE      29
        #define HB_P_LESSEQUAL      34

        #define HB_P_LINE           36

        #define HB_P_NOOP           67
        #define HB_P_POP            73

        #define HB_P_RETVALUE      110

        #define HB_P_SEQBEGIN      113
        #define HB_P_SEQEND        114
        #define HB_P_SEQRECOVER    115

        #define HB_P_BASELINE      133
        #define HB_P_LINEOFFSET    134

        #define HB_P_WITHOBJECT    135
        #define HB_P_ENDWITHOBJECT 138

        #define HB_P_FOREACH       139
        #define HB_P_ENUMERATE     140
        #define HB_P_ENDENUMERATE  141

        #define HB_P_SWITCHCASE    146

        #define HB_P_TRYBEGIN      162
        #define HB_P_TRYEND        163
        #define HB_P_TRYRECOVER    164
        #define HB_P_FINALLY       165
        #define HB_P_ENDFINALLY    166

        #define VAR_LOCALPARAM 1
        #define VAR_PARAM      2
        #define VAR_LOCAL      3
        #define VAR_STATIC     4
        #define VAR_PRIVATE    5
        #define VAR_PUBLIC     6

        #define LOBYTE(l)  ( (l) & 0xFF )
        #define HIBYTE(l)  ( (l) >> 8 )

        #define BYTE1(l)  ( ( (l) & 0x000000FF ) )
        #define BYTE2(l)  ( ( (l) & 0x0000FF00 ) >> 8 )
        #define BYTE3(l)  ( ( (l) & 0x00FF0000 ) >> 16 )
        #define BYTE4(l)  (   (l)                >> 24 )
      #endif

      #define __FOR_EACH__
      #define __WITH_
      #define __STRING_INDEX__

      REQUEST ErrorSys

      REQUEST CreateObject
      REQUEST GetActiveObject

      REQUEST SIN, COS

      #ifdef ADS
         // All ADS specific externs will be requested by hbextern.ch
      #endif

      #ifdef GD
         #include "gdexternal.ch"

         #pragma BEGINDUMP
            #ifdef _MSC_VER
               #pragma comment( lib, "\\xHarbour\\lib\\GDlib.lib" )
               #pragma comment( lib, "\\xHarbour\\lib\\BGD.lib" )
            #endif
         #pragma ENDDUMP
      #endif

      #ifdef SQL
         REQUEST SQLRDD
         #include "sqlextern.ch"
      #endif

      #ifdef ZIP
         REQUEST ZipCreate
         REQUEST RMDBFCDX
      #endif
   #endif

   #ifndef NO_BOOST
      #define USE_C_BOOST
   #endif

   #ifndef NO_CT3
      #define CT3
   #endif

   #ifndef HB_CDP_SUPPORT_ON
      #define HB_CDP_SUPPORT_OFF
   #endif
   #INCLUDE "hbextern.ch"

   #DEFINE  CRLF HB_OsNewLine()

   #ifdef FW
      #ifdef REQUEST_FWALL
         #INCLUDE "fwextern.ch"
      #endif
   #else
      #ifdef WIN
         #COMMAND Alert( <x> ) => MessageBox( 0, CStr( <x> ), "PP for Windows", 0 )
         REQUEST MessageBox
      #endif
   #endif

#else

   #DEFINE __CLIPPER__

   #translate At( <find>, <where>, <from> ) => IIF( ( M->__AT__  := At( <find>, SubStr( <where>, <from> ) ) ) == 0, 0, <from> + M->__AT__ - 1 )

   #xtranslate ErrorNew( <init,...> ) => ErrorNewX( <init> )

   #ifndef CRLF
      #DEFINE  CRLF Chr(13) + Chr(10)
   #endif

   EXTERNAL BROWSE

   EXTERNAL ARRAY,ASIZE,ATAIL,AINS,ADEL,AFILL,ASCAN,AEVAL,ACOPY,ACLONE,ADIR
   EXTERNAL ASORT

   EXTERNAL ERRORLEVEL

   EXTERNAL __QQPUB,__MCLEAR,__MRELEASE,__MXRELEASE,__MSAVE,__MRESTORE ;

   EXTERNAL PROCNAME,PROCLINE,PROCFILE

   EXTERNAL BIN2W,BIN2I,BIN2L,I2BIN,L2BIN

   EXTERNAL OUTSTD,OUTERR,QQOUT,QOUT,DISPOUT,DISPOUTAT,__EJECT, ;
            SETPRC,DISPBOX,DISPBEGIN,DISPEND,DISPCOUNT,ISCOLOR, ;
            NOSNOW,DBGSHADOW,SAVESCREEN,RESTSCREEN,SETCURSOR,SETBLINK,SETMODE,__ACCEPT, ;
            __ACCEPTSTR

   EXTERNAL __COPYFILE

   EXTERNAL DESCEND,DIRECTORY

   EXTERNAL VERSION,GETENV,__RUN

   EXTERNAL ERRORNEW,DOSERROR

   EXTERNAL FERASE,FRENAME,FILE,FREADSTR,CURDIR,DISKSPACE

   EXTERNAL __KEYBOARD,NEXTKEY,LASTKEY,FKLABEL,FKMAX

   EXTERNAL ISPRINTER

   EXTERNAL MOD

   EXTERNAL MEMOREAD,MEMOWRIT,MEMOLINE,MLCOUNT,MLPOS,MEMOTRAN

   EXTERNAL NETNAME

   EXTERNAL __BOX,__BOXD,__BOXS

   EXTERNAL AMPM,DAYS,ELAPTIME,LENNUM,SECS,TSTRING

   EXTERNAL SETCANCEL,__SETCENTURY,DEFPATH,__DEFPATH

   EXTERNAL SETCOLOR,COLORSELECT

   EXTERNAL SOUNDEX

   EXTERNAL ISALPHA,ISDIGIT,ISUPPER,ISLOWER,ALLTRIM,PADR,PAD,PADL,PADC, ;
            STUFF,STRZERO

   EXTERNAL TONE

   EXTERNAL TRANSFORM

   EXTERNAL __XHELP

   EXTERNAL ACHOICE

   EXTERNAL __NONOALERT

   EXTERNAL TBROWSEDB,DBEDIT

   EXTERNAL DEVOUTPICT

   EXTERNAL __DIR

   EXTERNAL DBSETRELATION,DBCLEARREL,MEMOEDIT,MLCTOPOS,MPOSTOLC,__DBAPP,__DBCOPY, ;
            __DBDELIM,__DBJOIN,__DBLIST,__DBSDF,__DBSORT,__DBTOTAL,__DBUPDATE,__DBARRANGE,__DBFLIST, ;
            __DBOPENSDF,__DBTRANS,__DBTRANSREC

   EXTERNAL FIELDBLOCK,FIELDWBLOCK

   EXTERNAL __INPUT

   EXTERNAL MEMVARBLOCK

   EXTERNAL __ATPROMPT,__MENUTO

   EXTERNAL READKEY

   EXTERNAL SETKEY

   EXTERNAL SETTYPEAHEAD

   EXTERNAL TBCOLUMNNEW,TBROWSENEW

   EXTERNAL __TEXTSAVE,__TEXTRESTORE

   EXTERNAL __GET,__GETA

   EXTERNAL __LABELFORM, __REPORTFORM

   EXTERNAL __TYPEFILE

   EXTERNAL __WAIT

   EXTERNAL __XSAVESCREEN,__XRESTSCREEN

   /*
   EXTERNAL RDDSYS,AFIELDS,DBEVAL,DBCLEARFILTER,DBCLOSEALL, ;
            DBCOMMIT,__DBCONTINUE,DBCREATE,DBDELETE,DBFILTER,DBGOBOTTOM,DBGOTO, ;
            DBGOTOP,__DBLOCATE,__DBSETLOCATE,__DBPACK,DBRECALL,DBRLOCK,DBRLOCKLIST,DBRUNLOCK,DBSEEK, ;
            DBSELECTAREA,__DBSETFOUND,DBSKIP,DBSETFILTER,DBSTRUCT,DBTABLEEXT,DBUNLOCK,DBUNLOCKALL,DBUSEAREA, ;
            __DBZAP,DELETED,EOF,FCOUNT,FIELDGET,FIELDNAME,FIELDPOS,FIELDPUT,FLOCK,FOUND,HEADER,INDEXORD, ;
            LASTREC,LOCK,LUPDATE,NETERR,ORDBAGEXT,ORDBAGNAME,ORDCONDSET,ORDCREATE,ORDDESTROY,ORDFOR,ORDKEY, ;
            ORDLISTADD,ORDLISTCLEAR,ORDLISTREBUILD,ORDNAME,ORDNUMBER,ORDSETFOCUS,RDDLIST,RDDNAME,RDDREGISTER, ;
            RECCOUNT,RECNO,RECSIZE,RLOCK,SELECT,USED,RDDSETDEFAULT,RDDSETDEFAULT,DBSETDRIVER
   */

   EXTERNAL __DBPACK,__DBZAP,DBCLOSEALL,DBGOBOTTOM,DBGOTO,DBGOTOP

   EXTERNAL DBREINDEX,DBCREATEINDEX,DBCLEARINDEX,DBSETINDEX,DBSETORDER

   EXTERNAL __DBCOPYSTRUCT,__DBCOPYXSTRUCT,__DBCREATE,__FLEDIT

   EXTERNAL INDEXEXT,INDEXKEY

   STATIC s_abBlocks := {}, nBlockId := 0

#endif

#ifdef __CONCILE_PCODE__
   STATIC s_hDynFuncLists := Hash()
#else
   STATIC s_aProc
#endif

STATIC aDefRules     := {}, aDefResults   := {}
STATIC aTransRules   := {}, aTransResults := {}
STATIC aCommRules    := {}, aCommResults  := {}

STATIC bDbgMatch := .F., bDbgExp := .F., bDbgPPO := .F., bLoadRules := .T., ;
       bCount := .T., bCCH := .F., bCompile := .T., bStrict := .T.

STATIC nIfDef := 0, abIfDef := {}, nIf := 0, abIf := {}

STATIC hPP

STATIC s_asPaths := {}

STATIC s_bArrayPrefix := .F.

STATIC s_sFile := "", s_sIncludeFile

STATIC s_nRow, s_nCol

STATIC s_nProcId := 0, s_aProcedures := {}, s_xRet, s_nIfLevel := 0, ;
       s_aProcStack := {}, s_nProcStack := 0

STATIC s_asPrivates := {}, s_asPublics := {}, s_asLocals := {}, ;
       s_aStatics, s_aParams := {}

STATIC s_sModule := "", s_aInitExit := { {}, {} }

STATIC s_nCompIf := 0,  s_nCompLoop := 0, s_aIfJumps := {}, s_aLoopJumps := {}
STATIC s_acFlowType := {},  s_nFlowId := 0

STATIC s_lRunLoaded := .F., s_lDotLoaded := .F., s_lClsLoaded := .F., s_lFWLoaded := .F.
STATIC s_aSwitchDefs := {}

STATIC s_sPending

STATIC s_lTrying := .F.
STATIC s_lReturnRequested

STATIC s_bExternalRecovery
STATIC s_anEnumIndex := {}, s_nForEachIndex := 0
STATIC s_aEnumerations := {}, s_anEnumerator := {}, s_anForEachStartingBlock := {}

STATIC s_bDefRTEBlock := {|e| DefRTEHandler( e ) }, s_bRTEBlock
STATIC s_bInterceptRTEBlock := {|oErr| RP_Run_Err( oErr, s_aProcedures ) }

STATIC s_anRecover := {}, s_acRecover := {}, s_aSequence := {}

// TODO: Finish translation of all context STATICs to the context array.
#define PP_CONTEXT_aDefRules               1
#define PP_CONTEXT_aDefResults             2
#define PP_CONTEXT_aTransRules             3
#define PP_CONTEXT_aTransResults           4
#define PP_CONTEXT_aCommRules              5
#define PP_CONTEXT_aCommResults            6
#define PP_CONTEXT_bLoadRules              7
#define PP_CONTEXT_bCount                  8
#define PP_CONTEXT_bCCH                    9
#define PP_CONTEXT_bCompile               10
#define PP_CONTEXT_bStrict                11
#define PP_CONTEXT_nIfDef                 12
#define PP_CONTEXT_abIfDef                13
#define PP_CONTEXT_nIf                    14
#define PP_CONTEXT_abIf                   15
#define PP_CONTEXT_hPP                    16
#define PP_CONTEXT_asPaths                17
#define PP_CONTEXT_bArrayPrefix           18
#define PP_CONTEXT_sFile                  19
#define PP_CONTEXT_sIncludeFile           20
#define PP_CONTEXT_nRow                   21
#define PP_CONTEXT_nCol                   22
#define PP_CONTEXT_nProcId                23
#define PP_CONTEXT_aProcedures            24
#define PP_CONTEXT_xRet                   25
#define PP_CONTEXT_nIfLevel               26
#define PP_CONTEXT_aProcStack             27
#define PP_CONTEXT_nProcStack             28
#define PP_CONTEXT_asPrivates             29
#define PP_CONTEXT_asPublics              30
#define PP_CONTEXT_asLocals               31
#define PP_CONTEXT_aStatics               32
#define PP_CONTEXT_aParams                33
#define PP_CONTEXT_sModule                34
#define PP_CONTEXT_aInitExit              35
#define PP_CONTEXT_nCompIf                36
#define PP_CONTEXT_nCompLoop              37
#define PP_CONTEXT_aIfJumps               38
#define PP_CONTEXT_aLoopJumps             39
#define PP_CONTEXT_acFlowType             40
#define PP_CONTEXT_nFlowId                41
#define PP_CONTEXT_lRunLoaded             42
#define PP_CONTEXT_lDotLoaded             43
#define PP_CONTEXT_lClsLoaded             44
#define PP_CONTEXT_lFWLoaded              45
#define PP_CONTEXT_aSwitchDefs            46
#define PP_CONTEXT_sPending               47
#define PP_CONTEXT_lTrying                48
#define PP_CONTEXT_lReturnRequested       49
#define PP_CONTEXT_bExternalRecovery      50
#define PP_CONTEXT_anEnumIndex            51
#define PP_CONTEXT_nForEachIndex          52
#define PP_CONTEXT_aEnumerations          53
#define PP_CONTEXT_anEnumerator           54
#define PP_CONTEXT_anForEachStartingBlock 55
#define PP_CONTEXT_bDefRTEBlock           56
#define PP_CONTEXT_bRTEBlock              57
#define PP_CONTEXT_bInterceptRTEBlock     58
#define PP_CONTEXT_anRecover              59
#define PP_CONTEXT_acRecover              60
#define PP_CONTEXT_aSequence              61
#define PP_CONTEXT_bDbgMatch              62
#define PP_CONTEXT_bDbgExp                63
#define PP_CONTEXT_bDbgPPO                64

#ifdef __CONCILE_PCODE__
   #define PP_CONTEXT_hDynFuncLists          65

   #define PP_CONTEXT_SIZE                   65
#else
   #define PP_CONTEXT_aProc                  65

   #define PP_CONTEXT_SIZE                   65
#endif


STATIC s_aPPContext

#ifdef PP_RECURSIVE
   STATIC s_bRecursive := .F.
#endif

STATIC s_cVer := "2.0 RC4"

#ifdef __HARBOUR__
   STATIC s_sAppPath
#endif

//--------------------------------------------------------------//

#ifdef _USE_APPMAIN_
PROCEDURE _AppMain( sSource, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
#else
PROCEDURE PP_Main( sSource, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
#endif

   LOCAL sIncludePath, nNext, sPath, sSwitch := ""
   LOCAL nAt, sParams, sPPOExt, aParams := {}
   LOCAL sDefine, sCH, lStayInDotPrompt := .f.

   IF p1 != NIL
      sSwitch += p1
   ENDIF
   IF p2 != NIL
      sSwitch += p2
   ENDIF
   IF p3 != NIL
      sSwitch += p3
   ENDIF
   IF p4 != NIL
      sSwitch += p4
   ENDIF
   IF p5 != NIL
      sSwitch += p5
   ENDIF
   IF p6 != NIL
      sSwitch += p6
   ENDIF
   IF p7 != NIL
      sSwitch += p7
   ENDIF
   IF p8 != NIL
      sSwitch += p8
   ENDIF
   IF p9 != NIL
      sSwitch += p9
   ENDIF

   IF sSource != NIL .AND. ( Upper( sSource ) == "-H" .OR. Upper( sSource ) == "--HELP" )
      sSwitch := "   XBSCRIPT filename[.ext] [-CCH] [-D<id>] [-D:E] [-D:M] [-D:P] [-I<path>] [-P] [-R]" + CRLF
      sSwitch += "                     [-S] [-FIX] [-U[ch-file]]" + CRLF + CRLF

      sSwitch += [    -CCH     = Generate a .cch file (compiled command header).] + CRLF
      sSwitch += [    -D<id>   = #define <id>.] + CRLF
      sSwitch += [    -D:E     = Show tracing information into the Expression Scanner.] + CRLF
      sSwitch += [    -D:M     = Show tracing information into the Match Engine.] + CRLF
      sSwitch += [    -D:P     = Show tracing information into the Output Generator.] + CRLF
      sSwitch += [    -FIX     = Do not clone Clipper PreProcessor bugs.] + CRLF
      sSwitch += [    -H       = Syntax and command line switches description.] + CRLF
      sSwitch += [    --help   = Syntax and command line switches description.] + CRLF
      sSwitch += [    -I<path> = #include file search path(s) ('] + OS_PATH_LIST_SEPARATOR + [' seperated).] + CRLF
      sSwitch += [    -Q       = Quiet. ] + CRLF
      sSwitch += [    -P       = Generate .pp$ pre-processed output file.] + CRLF
      sSwitch += [    -R       = Run filename as a script.] + CRLF
      sSwitch += [    -S       = Stay in dot prompt mode after running source file.] + CRLF
      sSwitch += [    -U       = Use command definitions set in <ch-file> (or none).] + CRLF

        ? sSwitch
      ?
      QUIT
   ENDIF

   #ifdef __PLATFORM__UNIX
      IF right( hb_argv( 0 ), 6 ) == "/pprun"
         bCount := .F.
         bCompile := .T.
         sSwitch := ""
         aParams := { p1, p2, p3, p4, p5, p6, p7, p8, p9 }
         aSize( aParams, PCount() - 1 )
      ENDIF
   #endif

   #ifdef _DEFAULT_INC_DIR
      sPath := _DEFAULT_INC_DIR
      IF ! ( Right( sPath, 1 ) $ OS_PATH_DELIMITER_LIST )
         sPath += OS_PATH_DELIMITER
      ENDIF
      aAdd( s_asPaths, sPath )
   #endif

   sIncludePath := GetE( "INCLUDE" )

   WHILE ( nNext := At( OS_PATH_LIST_SEPARATOR, sIncludePath ) ) > 0
      sPath := Left( sIncludePath, nNext - 1 )
      IF ! ( Right( sPath, 1 ) $ OS_PATH_DELIMITER_LIST )
         sPath += OS_PATH_DELIMITER
      ENDIF
      aAdd( s_asPaths, sPath )
      sIncludePath := SubStr( sIncludePath, nNext + 1 )
   ENDDO
   IF ! ( sIncludePath == '' )
      IF ! ( Right( sIncludePath, 1 ) $ OS_PATH_DELIMITER_LIST )
         sIncludePath += OS_PATH_DELIMITER
      ENDIF
      aAdd( s_asPaths, sIncludePath )
   ENDIF

#ifdef __CLIP__
   sIncludePath := StartPath()
   nAt := AtR( '/', sIncludePath )

   IF nAt <= 0
      nAt := AtR( "\", sIncludePath )
   ENDIF
   //? nAt,sIncludePath

   IF nAt != 0
      sIncludePath := Left( sIncludePath, nAt - 1 )

      IF ! ( Right( sIncludePath, 1 ) $ OS_PATH_DELIMITER_LIST )
         sIncludePath += OS_PATH_DELIMITER
      ENDIF

      aAdd( s_asPaths, sIncludePath )
   ENDIF

   IF Empty( GetEnv( "CLIPROOT" ) )
      aAdd( s_asPaths, ClipRoot() + OS_PATH_DELIMITER + "include" + OS_PATH_DELIMITER )
   ELSE
      aAdd( s_asPaths, GetEnv( "CLIPROOT" ) + OS_PATH_DELIMITER + "include" + OS_PATH_DELIMITER )
   ENDIF
#endif

   IF ! Empty( sSwitch )
      sSwitch := Upper( sSwitch )

      /* Generate compiled header. */
      IF "-CCH" $ sSwitch
         bCCH := .T.
         bCompile := .F.
      ENDIF

      /* Quiet Mode. */
      IF "-Q" $ sSwitch
         bCount := .F.
      ENDIF

      /* Run Source but then run Dot prompt. Source can set up complex Views and relations. */
      IF "-S" $ sSwitch
         lStayInDotPrompt := .T.
      ENDIF

      /* Debug tracing options. */
      IF "-D:E" $ sSwitch
         bDbgExp := .T.
         sSwitch := StrTran( sSwitch, "-D:E", "" )
      ENDIF
      IF "-D:M" $ sSwitch
         bDbgMatch := .T.
         sSwitch := StrTran( sSwitch, "-D:M", "" )
      ENDIF
      IF "-D:P" $ sSwitch
         bDbgPPO := .T.
         sSwitch := StrTran( sSwitch, "-D:P", "" )
      ENDIF

      /* Process command line defines. */
      WHILE ( nAt := At( "-D", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF

         sDefine := SubStr( sSwitch, nAt + 2, nNext - 1 )
         sSwitch := Left( sSwitch, nAt - 1 ) + SubStr( sSwitch, nAt + 1 + nNext )
         //CompileDefine( sDefine )
         aAdd( s_aSwitchDefs, sDefine )
      ENDDO

      /* Process command line include paths. */
      IF ( nAt := At( "-I", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF
         sIncludePath := SubStr( sSwitch, nAt + 2, nNext - 1 )

         WHILE ( nNext := At( OS_PATH_LIST_SEPARATOR, sIncludePath ) ) > 0
            sPath := Left( sIncludePath, nNext - 1 )
            IF ! ( Right( sPath, 1 ) $ OS_PATH_DELIMITER_LIST )
               sPath += OS_PATH_DELIMITER
            ENDIF
            aAdd( s_asPaths, sPath )
            sIncludePath := SubStr( sIncludePath, nNext + 1 )
         ENDDO
         IF ! ( sIncludePath == '' )
            IF ! ( Right( sIncludePath, 1 ) $ OS_PATH_DELIMITER_LIST )
               sIncludePath += OS_PATH_DELIMITER
            ENDIF
            aAdd( s_asPaths, sIncludePath )
         ENDIF
      ENDIF

      /* Generate .pp$ pre-processed output file. */
      IF "-P" $ sSwitch
         sPPOExt := ".pp$"
         bCompile := .F.
      ENDIF

      /* Run file as a script. */
      IF "-R" $ sSwitch
         bCompile := .T.
      ENDIF

      /* Clone Clipper PreProcessor bugs. */
      IF "-FIX" $ sSwitch
         bStrict := .F.
      ENDIF

      /* Use alternate command defintions file, or none. */
      WHILE ( nAt := At( "-U", sSwitch ) ) > 0
         nNext := At( "-", SubStr( sSwitch, nAt + 2 ) )
         IF nNext == 0
            nNext := 256
         ENDIF

         sCH := SubStr( sSwitch, nAt + 2, nNext - 1 )
         sSwitch := Left( sSwitch, nAt - 1 ) + SubStr( sSwitch, nAt + 1 + nNext )

         IF( ! sCH == "" )
            ? [Loading standard definitions from: '] + sCH + "'"
            ?

            CompileDefine( "__PP__" )
            #ifdef __HARBOUR__
               CompileDefine( "__HARBOUR__" )
            #endif

            #ifdef __XHARBOUR__
               CompileDefine( "__XHARBOUR__" )
            #endif

            PP_PreProFile( sCH, NIL, .F., .T. ) // Process ONLY #Directives!

            /* Reset.*/
            hPP := NIL
         ENDIF

         /* Don't load standard definitions. */
         bLoadRules := .F.
      ENDDO

      /* End of command line arguments processing. */
   ENDIF

   IF bLoadRules
      bLoadRules := .F.

      PP_InitStd()

      IF Len( aDefRules ) != Len( aDefResults )
         Eval( s_bRTEBlock, ErrorNew( [PP], 0, 1003, [Pre-Processing], [#DEFINE Rules size mismatch], { aDefRules, aDefResults } ) )
         // Safety
         BREAK
      ENDIF

      IF Len( aTransRules ) != Len( aTransResults )
         Eval( s_bRTEBlock, ErrorNew( [PP], 0, 1003, [Pre-Processing], [#TRANSLATE Rules size mismatch], { aTransRules, aTransResults } ) )
         // Safety
         BREAK
      ENDIF

      IF Len( aCommRules ) != Len( aCommResults )
         Eval( s_bRTEBlock, ErrorNew( [PP], 0, 1003, [Pre-Processing], [#COMMAND Rules size mismatch], { aCommRules, aCommResults } ) )
         // Safety
         BREAK
      ENDIF
   ELSE
      IF sCH == NIL
         PP_Warning( [Not using standard rules.] )
      ENDIF
   ENDIF

   // Command line defines.
   #ifdef __XHARBOUR__
       FOR EACH sDefine IN s_aSwitchDefs
          CompileDefine( sDefine )
       NEXT
   #else
       FOR nAt := 1 TO Len( s_aSwitchDefs )
          CompileDefine( s_aSwitchDefs[ nAt ] )
       NEXT
   #endif

   IF sSource == NIL
      s_nRow := 2
      s_nCol := 0

      RP_Dot()
   ELSE
      s_nRow := Row()
      s_nCol := Col()

      IF bCompile
         // Populate possible Command-line Parameters
         IF ( nAt := At( " ", sSource ) ) > 0
            sParams := LTrim( SubStr( sSource, nAt + 1 ) )
            sSource := Left( sSource, nAt - 1 )

            WHILE ( nAt := At( " ", sParams ) ) > 0
               aAdd( aParams, Left( sParams, nAt - 1 ) )
               sParams := LTrim( SubStr( sParams, nAt + 1 ) )
            ENDDO
            IF ! sParams == ""
               aAdd( aParams, sParams )
            ENDIF
         ENDIF

         PP_Run( sSource, aParams, sPPOExt )
         IF lStayInDotPrompt
            RP_Dot()
         ENDIF
      ELSE
         PP_PreProFile( sSource, sPPOExt )
      ENDIF
   ENDIF

   //DevPos( s_nRow, s_nCol )

RETURN

//------------------------------- *** RP DOT and Interpreter Functions *** -------------------------------//

STATIC FUNCTION PP_InitContext()

   LOCAL aPPContext[ PP_CONTEXT_SIZE ]

   aPPContext[ PP_CONTEXT_aDefRules ] := {}
   aPPContext[ PP_CONTEXT_aDefResults ] := {}
   aPPContext[ PP_CONTEXT_aTransRules ] := {}
   aPPContext[ PP_CONTEXT_aTransResults ] := {}
   aPPContext[ PP_CONTEXT_aCommRules ] := {}
   aPPContext[ PP_CONTEXT_aCommResults ] := {}

   aPPContext[ PP_CONTEXT_bLoadRules ] := .T.
   aPPContext[ PP_CONTEXT_bCount ] := .T.
   aPPContext[ PP_CONTEXT_bCCH ] := .F.
   aPPContext[ PP_CONTEXT_bCompile ] := .T.
   aPPContext[ PP_CONTEXT_bStrict ] := .T.

   aPPContext[ PP_CONTEXT_nIfDef ] := 0
   aPPContext[ PP_CONTEXT_abIfDef ] := {}
   aPPContext[ PP_CONTEXT_nIf ] := 0
   aPPContext[ PP_CONTEXT_abIf ] := {}

   aPPContext[ PP_CONTEXT_hPP ] := NIL

   aPPContext[ PP_CONTEXT_asPaths ] := {}

   aPPContext[ PP_CONTEXT_bArrayPrefix ] := .F.

   aPPContext[ PP_CONTEXT_sFile ] := ""
   aPPContext[ PP_CONTEXT_sIncludeFile ] := NIL

   aPPContext[ PP_CONTEXT_nRow ] := NIL
   aPPContext[ PP_CONTEXT_nCol ] := NIL

   aPPContext[ PP_CONTEXT_nProcId ] := 0
   aPPContext[ PP_CONTEXT_aProcedures ] := {}
   aPPContext[ PP_CONTEXT_xRet ] := NIL
   aPPContext[ PP_CONTEXT_nIfLevel ] := 0
   aPPContext[ PP_CONTEXT_aProcStack ] := {}
   aPPContext[ PP_CONTEXT_nProcStack ] := 0

   aPPContext[ PP_CONTEXT_asPrivates ] := {}
   aPPContext[ PP_CONTEXT_asPublics ] := {}
   aPPContext[ PP_CONTEXT_asLocals ] := {}
   aPPContext[ PP_CONTEXT_aStatics ] := NIL
   aPPContext[ PP_CONTEXT_aParams ] := {}

   aPPContext[ PP_CONTEXT_sModule ] := ""
   aPPContext[ PP_CONTEXT_aInitExit ] := { {}, {} }

   aPPContext[ PP_CONTEXT_nCompIf ] := 0
   aPPContext[ PP_CONTEXT_nCompLoop ] := 0
   aPPContext[ PP_CONTEXT_aIfJumps ] := {}
   aPPContext[ PP_CONTEXT_aLoopJumps ] := {}
   aPPContext[ PP_CONTEXT_acFlowType ] := {}
   aPPContext[ PP_CONTEXT_nFlowId ] := 0

   aPPContext[ PP_CONTEXT_lRunLoaded ] := .F.
   aPPContext[ PP_CONTEXT_lDotLoaded ] := .F.
   aPPContext[ PP_CONTEXT_lClsLoaded ] := .F.
   aPPContext[ PP_CONTEXT_lFWLoaded ] := .F.

   aPPContext[ PP_CONTEXT_aSwitchDefs ] := {}

   aPPContext[ PP_CONTEXT_sPending ] := NIL

   aPPContext[ PP_CONTEXT_lTrying ] := .F.

   aPPContext[ PP_CONTEXT_lReturnRequested ] := NIL

   aPPContext[ PP_CONTEXT_bExternalRecovery ] := NIL

   aPPContext[ PP_CONTEXT_anEnumIndex ] := {}
   aPPContext[ PP_CONTEXT_nForEachIndex ] := 0
   aPPContext[ PP_CONTEXT_aEnumerations ] := {}
   aPPContext[ PP_CONTEXT_anEnumerator ] := {}
   aPPContext[ PP_CONTEXT_anForEachStartingBlock ] := {}

   aPPContext[ PP_CONTEXT_bDefRTEBlock ] := {|e| DefRTEHandler( e ) }
   aPPContext[ PP_CONTEXT_bRTEBlock ] := aPPContext[ PP_CONTEXT_bDefRTEBlock ]
   aPPContext[ PP_CONTEXT_bInterceptRTEBlock ] := {|oErr| RP_Run_Err( oErr, aPPContext[ PP_CONTEXT_aProcedures ] ) }

   aPPContext[ PP_CONTEXT_anRecover ] := {}
   aPPContext[ PP_CONTEXT_acRecover ] := {}
   aPPContext[ PP_CONTEXT_aSequence ] := {}

   aPPContext[ PP_CONTEXT_bDbgMatch ] := .F.
   aPPContext[ PP_CONTEXT_bDbgExp ] := .F.
   aPPContext[ PP_CONTEXT_bDbgPPO ] := .F.

   #ifdef __CONCILE_PCODE__
      aPPContext[ PP_CONTEXT_hDynFuncLists ] := Hash()
   #else
      aPPContext[ PP_CONTEXT_aProc ] := NIL
   #endif

RETURN aPPContext

//--------------------------------------------------------------//
PROCEDURE PP_Break( xVal )

  Break( xVal )

//RETURN

PROCEDURE PP_ResetStack( aProcedures )

   LOCAL aProc

   #ifndef __XHARBOUR__
      LOCAL nProc, nProcedures := Len( aProcedures )
   #endif

   IF ! Empty( aProcedures )
     #ifdef __XHARBOUR__
      FOR EACH aProc IN aProcedures
     #else
      FOR nProc := 1 TO nProcedures
         aProc := aProcedures[ nProc ]
     #endif
         // Reset Stack Pointer
         aProc[8] := NIL
      NEXT
   ENDIF

   aSize( s_aProcStack, 0 )

RETURN

//------------------------------------------------------------------//
#ifdef __CONCILE_PCODE__

  //--------------------------------------------------------------//

  PROCEDURE ValidateProcedure( aProcedure )

     //#define DEBUG_PCODE
     #ifdef DEBUG_PCODE
        LOCAL c
     #endif

     IF Empty( aProcedure )
        RETURN
     ENDIF

     //TraceLog( aProcedure[1] )

     //Alert( ProcName() + "->" + aProcedure[1] )

     IF ! Empty( aProcedure[3] )
        Throw( ErrorNew( [PP], 0, 2099, [Parse], [Unclosd control structure, line: ] + Str( aProcedure[3][-1][2] ), { aProcedure[3] } ) )
     ENDIF

     #ifdef DEBUG_PCODE
        TraceLog( aProcedure[1], aProcedure[2] )

        FOR EACH c IN aProcedure[2]
          TraceLog( HB_EnumIndex(), c, Str( Asc( c ), 3 ) )
        NEXT
     #endif

  RETURN

  //--------------------------------------------------------------//

  FUNCTION ConcileProcedures( aProcedures, nDynOffset, pDynFunctions )

     STATIC s_aProcsContainer := {}

     LOCAL aProcedure
     LOCAL cID := CStr( HB_ArrayID( aProcedures ) )

     // Last processed not validfated.
     ValidateProcedure( aProcedures[-1] )

     FOR EACH aProcedure IN aProcedures
        //TraceLog( aProcedure[1] )
        aAdd( s_aProcsContainer, aProcedure )

        aProcedure[2] += Chr( HB_P_ENDPROC )
     NEXT

     IF hScan( s_hDynFuncLists, cID ) == 0
        s_hDynFuncLists[ cID ] := pDynFunctions
     ENDIF

     pDynFunctions := s_hDynFuncLists[ cID ]

     IF Empty( nDynOffset )
        nDynOffset := 1
     ENDIF

     //Alert( "Generating" )
     PP_GenDynProcedures( aProcedures, nDynOffset, @pDynFunctions )

     s_hDynFuncLists[ cID ] := pDynFunctions

     //TraceLog( s_hDynFuncLists[ cID ], cID, pDynFunctions )

  RETURN pDynFunctions

  //--------------------------------------------------------------//

  PROCEDURE ParseLinePCode( sLine, nLine, nProcID, aProcedures, aInitExit )

     LOCAL sSymbol
     LOCAL nAt
     LOCAL aProcedure, aFlow
     LOCAL sIf, sElseIf
     LOCAL sCase
     LOCAL sCounter, sStart, sEnd, sStep
     LOCAL sEnumerator, sEnumeration
     LOCAL sWhile
     LOCAL sWith
     LOCAL sUsing
     LOCAL sCatcher
     LOCAL cPCode
     LOCAL sReturn
     LOCAL cVars, aVars, cVar
     LOCAL sSwitch

     STATIC s_InlineMethodID := 0

     // Debug only!
     #ifdef _DEBUG
        LOCAL oErr
     #endif

     //TraceLog( sLine, nLine, nProcID, aProcedures, aInitExit )

     IF sLine = "PP_PROC"
        sSymbol := Upper( LTrim( SubStr( sLine, At( ' ', sLine ) ) ) )

        IF sLine = "PP_PROC_PRG"
        ELSEIF sLine = "PP_PROC_INIT"
           aAdd( aInitExit[1], nProcId + 1 )
        ELSEIF sLine = "PP_PROC_EXIT"
           aAdd( aInitExit[2], nProcId + 1 )
        ENDIF

        IF s_InlineMethodID == 0
           IF ! Empty( aProcedures )
              ValidateProcedure( aProcedures[nProcID] )
           ENDIF
        ELSE
           SetProcedure( aProcedures, nLine, sSymbol )

           RETURN
        ENDIF

        SetProcedure( aProcedures, nLine, sSymbol )
        nProcID := Len( aProcedures )
        aProcedure := aProcedures[-1]
     ELSE
        // No procedure declaration.
        IF nProcId == 0
           sSymbol := "Implied_Main"

           SetProcedure( aProcedures, nLine, sSymbol )
           nProcID := Len( aProcedures )
           aProcedure := aProcedures[-1]
        ELSE
           IF s_InlineMethodID == 0
              aProcedure := aProcedures[nProcID]
           ELSE
              aProcedure := aProcedures[s_InlineMethodID]
           ENDIF
        ENDIF

        IF sLine = "PP__"

           aFlow := aProcedure[3]

         // Debug only!
         #ifdef _DEBUG
          TRY
         #endif

           IF sLine = "PP__LocalParams"

              IF Len( aProcedure[5][VAR_LOCALPARAM] ) > 0
                 Throw( ErrorNew( [PP], 0, 2034, [Parse], [Formal parameters already declared], { nLine, sLine } ) )
              ENDIF

              sLine := SubStr( sLine, 17 )
              cVars := Left( sLine, Len( sLine ) - 1 )

              aVars := &( cVars )

              FOR EACH cVar IN aVars
                 SetParam( aProcedure, nLine, VAR_LOCALPARAM, cVar )
              NEXT

           ELSEIF sLine = "PP__Params"

              IF Len( aProcedure[5][VAR_LOCALPARAM] ) > 0
                 Throw( ErrorNew( [PP], 0, 2034, [Parse], [Formal parameters already declared], { nLine, sLine } ) )
              ENDIF

              sLine := SubStr( sLine, 12 )
              cVars := Left( sLine, Len( sLine ) - 1 )

              aVars := &( cVars )

              FOR EACH cVar IN aVars
                 SetParam( aProcedure, nLine, VAR_PARAM, cVar )
              NEXT

           ELSEIF sLine = "PP__Locals"

              sLine := SubStr( sLine, 12 )
              cVars := Left( sLine, Len( sLine ) - 1 )

              aVars := &( cVars )

              FOR EACH cVar IN aVars
                 SetVar( aProcedure, nLine, VAR_LOCAL, cVar )
              NEXT

           ELSEIF sLine = "PP__Statics"

              sLine := SubStr( sLine, 13 )
              cVars := Left( sLine, Len( sLine ) - 1 )

              aVars := &( cVars )

              FOR EACH cVar IN aVars
                 SetVar( aProcedure, nLine, VAR_STATIC, cVar )
              NEXT

           ELSE

              aProcedure[6] := .T.

              IF sLine = "PP__Privates"

                 sLine := SubStr( sLine, 14 )
                 cVars := Left( sLine, Len( sLine ) - 1 )

                 aVars := &( cVars )

                 FOR EACH cVar IN aVars
                    SetVar( aProcedure, nLine, VAR_PRIVATE, cVar )
                 NEXT

              ELSEIF sLine = "PP__Publics"

                 sLine := SubStr( sLine, 13 )
                 cVars := Left( sLine, Len( sLine ) - 1 )

                 aVars := &( cVars )

                 FOR EACH cVar IN aVars
                    SetVar( aProcedure, nLine, VAR_PUBLIC, cVar )
                 NEXT

              ELSEIF sLine = "PP__IF"

                 sIf := SubStr( sLine, 8 )

                 SetFlow( aProcedure, nLine, 'I', sIf )

              ELSEIF sLine = "PP__ELSEIF"

                 IF aFlow[-1][1] == "I" .AND. aFlow[-1][4] == .F.
                    sElseIf := SubStr( sLine, 12 )
                    SetFlowCondition( aProcedure, nLine, sElseIf )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2026, [Parse], [ELSEIF does not match IF], { nLine, sLine } ) )
                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__ELSE"

                 IF aFlow[-1][1] == "I" .AND. aFlow[-1][4] == .F.
                    SetFlowDefault( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2026, [Parse], [ELSE does not match IF], { nLine, sLine } ) )
                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__ENDIF"

                 IF aFlow[-1][1] == "I"
                    FinalizeFlow( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2026, [Parse], [ENDIF does not match IF], { nLine, sLine } ) )
                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__DOCASE"

                 aAdd( aFlow, { "C", nLine, -Len( aProcedure[2] ), .F., {} } )

              ELSEIF sLine = "PP__SWITCH"

                 sSwitch := SubStr( sLine, 12 )

                 SetFlowSwitch( aProcedure, nLine, sSwitch )

              ELSEIF sLine = "PP__CASE"

                 IF aFlow[-1][1] $ "CS"

                    sCase := SubStr( sLine, 10 )
                    SetFlowCondition( aProcedure, nLine, sCase )

                 ELSE

                    Throw( ErrorNew( [PP], 0, 2031, [Parse], [CASE does not match DO CASE], { nLine, sLine } ) )

                    // Safety
                    BREAK

                 ENDIF

              ELSEIF sLine = "PP__OTHERWISE"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "C"
                    SetFlowDefault( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2031, [Parse], [OTHERWISE does not match DO CASE], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__DEFAULT"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "S"
                    SetFlowDefault( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2031, [Parse], [DEFAULT does not match SWITCH], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__ENDCASE"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "C"
                    FinalizeFlow( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2030, [Parse], [ENDCASE with no DO CASE in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__WHILE"

                 sWhile := SubStr( sLine, 11 )

                 SetFlow( aProcedure, nLine, 'W', sWhile )

              ELSEIF sLine = "PP__FOR "

                 sLine    := SubStr( sLine, 9 )

                 sCounter := Left( sLine, ( nAt := AT( ":=", sLine ) ) - 1 )
                 sLine    := SubStr( sLine, nAt + 2 )

                 sStart   := Left( sLine, ( nAt := At( "~TO~", sLine ) ) - 1 )
                 sLine    := SubStr( sLine, nAt + 4 )

                 sEnd     := Left( sLine, ( nAt := At( "~STEP~", sLine ) ) - 1 )

                 sStep    := SubStr( sLine, nAt + 6 )

                 IF Empty( sStep )
                    sStep := "1"
                 ENDIF

                 SetFlowFor( aProcedure, nLine, sCounter, sStart, sEnd, sStep )

              ELSEIF sLine = "PP__FOREACH"

                 sLine := SubStr( sLine, 13 )

                 sEnumerator := Left( sLine, ( nAt := AT( "~$~", sLine ) ) - 1 )
                 sEnumeration := SubStr( sLine, nAt + 3 )

                 SetFlowForEach( aProcedure, nLine, sEnumerator, sEnumeration )

              ELSEIF sLine = "PP__LOOP"

                 IF aScan( aFlow, {|_1| _1[1] $ "WFE" } ) > 0
                    SetFlowLoop( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2012, [Parse], [LOOP with no loop in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__EXIT"
                 IF aScan( aFlow, {|_1| _1[1] $ "WFES" } ) > 0
                    SetFlowExit( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2011, [Parse], [EXIT with no loop in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__NEXT"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "F"
                    FinalizeFor( aProcedure, nLine )
                 ELSEIF aFlow[-1][1] == "E"
                    FinalizeForEach( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2029, [Parse], [NEXT does not match FOR], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__ENDDO"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "W"
                    FinalizeWhile( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2028, [Parse], [ENDDO does not match WHILE], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__WITHOBJECT"

                 sWith := SubStr( sLine, 16 )

                 SetWithObject( aProcedure, nLine, sWith )

              ELSEIF sLine = "PP__BEGIN"

                 SetBegin( aProcedure, nLine )

              ELSEIF sLine = "PP__RECOVER" //PP_RECOVER USING ...

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "B"
                    sUsing  := LTrim( SubStr( sLine, 13 ) )

                    SetRecover( aProcedure, nLine, sUsing )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2046, [Parse], [RECOVER with no BEGIN SEQUENCE in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__TRY"

                 SetTry( aProcedure, nLine )

              ELSEIF sLine = "PP__CATCH"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "T"
                    sCatcher := LTrim( SubStr( sLine, 11 ) )

                    SetCatch( aProcedure, nLine, sCatcher )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2046, [Parse], [CATCH with no TRY in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__FINALLY"

                 IF aFlow[-1] != NIL .AND. aFlow[-1][1] == "T"
                    SetFinally( aProcedure, nLine )
                 ELSE
                    Throw( ErrorNew( [PP], 0, 2046, [Parse], [FINALLY with no TRY in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ENDIF

              ELSEIF sLine = "PP__Return"

                 IF aScan( aFlow, {|_1| ( _1[1] == "B" .AND. _1[4] == 0 ) .OR. ( _1[1] == "T" .AND. _1[4] == 0 .AND. _1[6] == .F. )  } ) > 0
                    Throw( ErrorNew( [PP], 0, 2086, [Parse], [RETURN violates enclosing SEQUENCE/TRY], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ELSE
                    // NO Space: PP__RETURN( ... ) - drop Opener (
                    sLine := AllTrim( SubStr( sLine, 12 ) )
                    sReturn := Left( sLine, Len( sLine ) - 1 )

                    SetReturn( aProcedure, nLine, sReturn )
                 ENDIF

              ELSEIF sLine = "PP__ENDMETHOD"

                  ValidateProcedure( aProcedures[ s_InlineMethodID ] )

                  // Restore Class Function context
                  s_InlineMethodID := 0

              ELSEIF sLine = "PP__END"

                 IF Empty( aFlow )
                    Throw( ErrorNew( [PP], 0, 2027, [Parse], [END with no Flow-Control structure in sight!], { nLine, sLine } ) )

                    // Safety
                    BREAK
                 ELSE
                    IF aFlow[-1][1] $ "ICS"

                       FinalizeFlow( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "W"

                       FinalizeWhile( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "F"

                       FinalizeFor( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "E"

                       FinalizeForEach( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "O"

                       FinalizeWithObject( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "B"

                       FinalizeBegin( aProcedure, nLine )

                    ELSEIF aFlow[-1][1] == "T"

                       FinalizeTry( aProcedure, nLine )

                    ELSE

                        alert( "Unexpected case in " + ProcName() )

                    ENDIF

                 ENDIF

              ELSEIF sLine = "PP__INLINEMETHOD"

                 s_InlineMethodID := Len( aProcedures ) + 1

              ELSE

                 TraceLog( "Unexpected case!", sLine )

              ENDIF

           ENDIF

         // Debug only!
         #ifdef _DEBUG
          CATCH oErr

             #ifdef __XHARBOUR__
                Alert( oErr:ProcName + "(" + str( oErr:ProcLine, 5 ) + ")" )
             #endif

             TraceLog( oErr:Operation, oErr:Description )
             #ifdef __XHARBOUR__
                TraceLog oErr:ProcName, oErr:ProcLine )
             #endif

             Throw( oErr )

          END
         #endif

        ELSE

           //OutputDebugString( aProcedure[1] )

           aProcedure[6] := .T.

           sLine := ParseAssign( sLine )

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           SetLine( aProcedure, nLine )

           //TraceLog( nLine, sLine )

           cPCode := HB_MacroCompile( sLine, PP_CONTEXT_STATEMENT )
           cPCode[-1] := HB_P_NOOP

           aProcedure[2] += cPCode

        ENDIF

     ENDIF

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PCodeProlog( aProcedure )

     LOCAL cVar

     aProcedure[8] := s_aProcStack
     aAdd( s_aProcStack, aProcedure )

     IF Len( s_aProcStack ) > 1
        aProcedure := s_aProcStack[-2]

        aSize( aProcedure[9], Len( aProcedure[5][VAR_LOCAL] ) )

        FOR EACH cVar IN aProcedure[5][VAR_STATIC]
           // Might have been called in LOCAL declarations, before this STATIC was created.
           IF __MVExist( cVar )
              // Save existing value
              aProcedure[7][HB_EnumIndex()] := __MVGet( cVar )
           ENDIF

           // Hide it from the new procedure
           //__MVRelease( cVar )
           //__MVPut( cVar, NIL )
        NEXT

        FOR EACH cVar IN aProcedure[5][VAR_LOCAL]

           // Might have been called in LOCAL declarations, before this LOCAL was created.
           IF __MVExist( cVar )
              // Save existing value
              aProcedure[9][HB_EnumIndex()] := __MVGet( cVar )
           ENDIF

           // Hide it from the new procedure
           //__MVRelease( cVar )
           //__MVPut( cVar, NIL )
        NEXT

     ENDIF

     //TraceLog( aProcedure[1] )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetProcedure( aProcedures, nLine, sSymbol )

     LOCAL aProcedure, cPCode

     //OutputDebugString( sSymbol )

     (nLine)

     //#define OPTIMIZE_SETLINE
     #ifdef OPTIMIZE_SETLINE
        aProcedure := { sSymbol, Chr( HB_P_NOOP ), {}, 0, { {}, {}, {}, {}, {}, {} }, .F., {}, NIL, {} }
     #else
        aProcedure := { sSymbol, "", {}, 0, { {}, {}, {}, {}, {}, {} }, .F., {}, NIL, {} }
     #endif

     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( "PCodeProlog(HB_ThisArray(0x" + CStr( HB_ArrayID( aProcedure ) ) + "))", PP_CONTEXT_STATEMENT )
     cPCode[-1] := HB_P_NOOP

     aProcedure[2] := cPCode

     aAdd( aProcedures, aProcedure )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PCodeEpilog( aProcedure )

     LOCAL cStatic, aStack := aProcedure[8], cVar

     FOR EACH cStatic IN aProcedure[5][VAR_STATIC]
        //TraceLog( HB_EnumIndex(), cStatic, __MVGet( cStatic ) )
        aProcedure[7][HB_enumIndex()] := __MVGet( cStatic )
     NEXT

     aSize( aStack, Len( aStack ) - 1 )

     IF Len( aStack ) > 0
        aProcedure := aStack[-1]

        FOR EACH cVar IN aProcedure[5][VAR_STATIC]
           // Restore value
           __MVPut( cVar, aProcedure[7][HB_EnumIndex()] )
        NEXT

        FOR EACH cVar IN aProcedure[5][VAR_LOCAL]
           // Restore value
           __MVPut( cVar, aProcedure[9][HB_EnumIndex()] )
        NEXT
     ENDIF

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetReturn( aProcedure, nLine, sReturn )

     LOCAL cPCode

     IF Empty( sReturn )
        IF Len( aProcedure[3] ) > 0 .AND. aProcedure[3][-1][1] $ "CS" .AND. aProcedure[3][-1][3] < 0
           Throw( ErrorNew( [PP], 0, 2099, [Parse], [Code placed before first case handler, line: ] + Str( aProcedure[3][-1][2] ), { nLine } ) )
        ENDIF
     ELSE
        SetLine( aProcedure, nLine )
        cPCode := aProcedure[2]

        cPCode += HB_MacroCompile( sReturn )
        cPCode[-1] := HB_P_NOOP

        aProcedure[2] := cPCode
     ENDIF

     cPCode := aProcedure[2]
     cPCode += HB_MacroCompile( "PCodeEpilog(HB_ThisArray(0x" + CStr( HB_ArrayID( aProcedure ) ) + "))", PP_CONTEXT_STATEMENT )

     // Retvalue above.
     cPCode[-1] := HB_P_RETVALUE

     cPCode += Chr( HB_P_ENDPROC )

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetLine( aProcedure, nLine, lCase )

     LOCAL cPCode := aProcedure[2]

     //RETURN
     //TraceLog( nLine )

     IF lCase != .T. .AND. Len( aProcedure[3] ) > 0 .AND. aProcedure[3][-1][1] $ "CS" .AND. aProcedure[3][-1][3] < 0
        Throw( ErrorNew( [PP], 0, 2099, [Parse], [Code placed before first case handler, line: ] + Str( aProcedure[3][-1][2] ), { nLine } ) )
     ENDIF

    #ifdef OPTIMIZE_SETLINE
     IF cPCode[-1] == Chr( HB_P_NOOP )
        IF aProcedure[4] == 0 .OR. ( nLine - aProcedure[4] ) > 255
           IF aProcedure[4] == 0
              aProcedure[4] := nLine
              cPCode[-1] := Chr( HB_P_BASELINE )
           ELSE
              cPCode[-1] := Chr( HB_P_LINE )
           ENDIF

           cPCode += Chr( BYTE1( nLine ) ) + Chr( BYTE2( nLine ) )
        ELSEIF nLine != aProcedure[4]
           cPCode[-1] := Chr( HB_P_LINEOFFSET )
           cPCode += Chr( nLine - aProcedure[4] )
        ENDIF
     ELSE
    #endif
        IF aProcedure[4] == 0
           aProcedure[4] := nLine
           cPCode += Chr( HB_P_BASELINE ) + Chr( BYTE1( nLine ) ) + Chr( BYTE2( nLine ) )
        ELSEIF ( nLine - aProcedure[4] ) > 255
           cPCode += Chr( HB_P_LINE ) + Chr( BYTE1( nLine ) ) + Chr( BYTE2( nLine ) )
        ELSE//IF nLine != aProcedure[4]
           cPCode += Chr( HB_P_LINEOFFSET ) + Chr( nLine - aProcedure[4] )
        ENDIF
    #ifdef OPTIMIZE_SETLINE
     ENDIF
    #endif

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetParam( aProcedure, nLine, nKind, cPureVar )

     LOCAL cPCode
     LOCAL aVars

     //TraceLog( aProcedure[1] )

     IF Len( aProcedure[3] ) > 0 .AND. aProcedure[3][-1][1] $ "CS" .AND. aProcedure[3][-1][3] < 0
        Throw( ErrorNew( [PP], 0, 2099, [Parse], [Code placed before first case handler, line: ] + Str( aProcedure[3][-1][2] ), { nLine } ) )
     ENDIF

     cPureVar := Upper( cPureVar )

     IF nKind == VAR_LOCALPARAM .AND. aProcedure[6]
        Throw( ErrorNew( [PP], 0, 2051, cPureVar, [Declaration follows executable code], { cPureVar } ) )
        // Safety
        BREAK
     ENDIF

     FOR EACH aVars IN aProcedure[5]
        IF aScan( aVars, cPureVar, , , .T. ) > 0
           Throw( ErrorNew( [PP], 0, 2016, cPureVar, [Variable redeclaration], { cPureVar } ) )
           // Safety
           BREAK
        ENDIF
     NEXT

     aAdd( aProcedure[5][nKind], cPureVar )

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( "__MVPrivate('" + cPureVar + "')", PP_CONTEXT_STATEMENT )
     cPCode[-1] := HB_P_NOOP

     cPCode += HB_MacroCompile( cPureVar + ":=PValue(" + Str( Len( aProcedure[5][nKind] ) ) + ")" , PP_CONTEXT_STATEMENT )
     cPCode[-1] := HB_P_NOOP

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetVar( aProcedure, nLine, nKind, cVar )

     LOCAL cPCode
     LOCAL nAt, cPureVar
     LOCAL aVars
     LOCAL cID, cInit, cArraySize
     LOCAL nVarID

     IF Len( aProcedure[3] ) > 0 .AND. aProcedure[3][-1][1] $ "CS" .AND. aProcedure[3][-1][3] < 0
        Throw( ErrorNew( [PP], 0, 2099, [Parse], [Code placed before first case handler, line: ] + Str( aProcedure[3][-1][2] ), { nLine } ) )
     ENDIF

     IF ( nAt := At( ":=", cVar ) ) > 0
        cPureVar := Upper( Left( cVar, nAt - 1 ) )
        cInit := SubStr( cVar, nAt + 2 )
     ELSEIF ( nAt := At( "[", cVar ) ) > 0
        cPureVar := Upper( Left( cVar, nAt - 1 ) )
        cArraySize := SubStr( cVar, nAt + 1 )
        cArraySize := Left( cArraySize, Len( cArraySize ) - 1 )
        cInit := "Array(" + cArraySize + ")"
     ELSE
        cPureVar := Upper( cVar )
     ENDIF

     cPureVar := RTrim( cPureVar )

     IF nKind < VAR_PRIVATE .AND. aProcedure[6]
        Throw( ErrorNew( [PP], 0, 2051, cPureVar, [Declaration follows executable code], { cPureVar } ) )
        // Safety
        BREAK
     ENDIF

     FOR EACH aVars IN aProcedure[5]
        IF aScan( aVars, cPureVar, , , .T. ) > 0
           Throw( ErrorNew( [PP], 0, 2016, cPureVar, [Variable redeclaration], { cPureVar } ) )
           // Safety
           BREAK
        ENDIF
     NEXT

     aAdd( aProcedure[5][nKind], cPureVar )

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     IF nKind == VAR_PUBLIC
        cPCode += HB_MacroCompile( "__MVPublic('" + cPureVar + "')", PP_CONTEXT_STATEMENT )
        cPCode[-1] := HB_P_NOOP

        IF cInit != NIL
           cPCode += HB_MacroCompile( "__MVPut('" + cPureVar + "'," + cInit + ")", PP_CONTEXT_STATEMENT )
           cPCode[-1] := HB_P_NOOP
        ENDIF
     ELSEIF nKind == VAR_STATIC
        cID := CStr( HB_ArrayID( aProcedure ) )

        IF cInit == NIL
           cInit := "NIL"
        ENDIF

        cPCode += HB_MacroCompile( "__MVPrivate('" + cPureVar + "')", PP_CONTEXT_STATEMENT )
        cPCode[-1] := HB_P_NOOP

        nVarID := Len( aProcedure[5][VAR_STATIC] )

        //TraceLog( nVarID, aProcedure[5][VAR_STATIC], aProcedure[7] )

        TRY
           aAdd( aProcedure[7], &cInit )
           //TraceLog( cID, aProcedure[7][nVarID] )
        CATCH
           Throw( ErrorNew( [PP], 0, 2015, [Parse], [Illegal static initializer.], { nLine, cPureVar, cInit } ) )
        END

        cPCode += HB_MacroCompile( "__MVPut('" + cPureVar + "',HB_ThisArray(0x" + cID + ")[7][" + Str( nVarID ) + "])", PP_CONTEXT_STATEMENT )
        cPCode[-1] := HB_P_NOOP
     ELSE
        cPCode += HB_MacroCompile( "__MVPrivate('" + cPureVar + "')", PP_CONTEXT_STATEMENT )
        cPCode[-1] := HB_P_NOOP

        IF cInit == NIL
           cInit := "NIL"
        ENDIF

        //IF cInit != NIL
           cPCode += HB_MacroCompile( "__MVPut('" + cPureVar + "'," + cInit + ")", PP_CONTEXT_STATEMENT )
           cPCode[-1] := HB_P_NOOP
        //ENDIF
     ENDIF

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlow( aProcedure, nLine, cKind, sCondition )

     LOCAL aFlow := { cKind, nLine, 0, .F., {}, 0, {}, {} }
     LOCAL cPCode

     // Top
     IF cKind == 'W'
        #ifdef OPTIMIZE_SETLINE
          IF aProcedure[2][-1] == Chr( HB_P_NOOP )
             aFlow[6] := Len( aProcedure[2] )
          ELSE
        #endif
             aFlow[6] := Len( aProcedure[2] ) + 1
        #ifdef OPTIMIZE_SETLINE
          ENDIF
        #endif
     ENDIF

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     // Condition
     cPCode += HB_MacroCompile( sCondition )

     // Next Branch or after END
     cPCode[-1] := HB_P_JUMPFALSE

     // Bookmark Jump
     aFlow[3] := Len( cPCode )
     cPCode += Chr(0) + Chr(0)

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aFlow )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlowSwitch( aProcedure, nLine, sSwitch )

     LOCAL aFlow := { "S", nLine, -Len( aProcedure[2] ), .F., {}, 0 }
     LOCAL cPCode

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( sSwitch )
     cPCode[-1] := HB_P_NOOP

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aFlow )

  RETURN

  //--------------------------------------------------------------//
  // Serves ElseIf/Case(Do/Switch)
  PROCEDURE SetFlowCondition( aProcedure, nLine, sCondition )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode
     LOCAL nOffset
     LOCAL nConstant

     // Might be first CASE
     IF aFlow[3] > 0
        cPCode := aProcedure[2]

        // Bookmark Jump
        IF aFlow[1] == 'S'
           // Explicit EXIT, no need to jenerate a fall through jump
           IF Len( aFlow[5] ) > 0 .AND. aFlow[5][-1] == Len( cPCode ) - 2
              aFlow[6] := 0
           ELSE
              // Unconditional SKIP to body of next SWITCH branch.
              cPCode += Chr( HB_P_JUMP )
              aFlow[6] := Len( cPCode )
              cPCode += Chr(0) + Chr(0)
           ENDIF
        ELSE
           // Unconditional SKIP to after END
           cPCode += Chr( HB_P_JUMP )
           aAdd( aFlow[5], Len( cPCode ) )
           cPCode += Chr(0) + Chr(0)
        ENDIF

        // Patch failure Jump
       #ifdef OPTIMIZE_SETLINE
        IF cPCode[-1] == Chr( HB_P_NOOP )
           nOffset := Len( cPCode ) - aFlow[3]
        ELSE
       #endif
           nOffset := ( Len( cPCode ) + 1 ) - aFlow[3]
      #ifdef OPTIMIZE_SETLINE
        ENDIF
      #endif

        cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

        aProcedure[2] := cPCode
     ENDIF

     SetLine( aProcedure, nLine, aFlow[1] $ "CS" )
     cPCode := aProcedure[2]

     IF aFlow[1] == 'S'
        nConstant := Val( sCondition )

        cPCode += Chr( HB_P_SWITCHCASE )
        cPCode += Chr( BYTE1( nConstant ) ) + Chr( BYTE2( nConstant ) ) + Chr( BYTE3( nConstant ) ) + Chr( BYTE4( nConstant ) )
        cPCode += Chr( HB_P_JUMPFALSE )

        IF aFlow[6] > 0
           // Patch fall through of last SWITCh branch.
           nOffset := ( Len( cPCode ) + 3 ) - aFlow[6]
           cPCode[ aFlow[6] + 1 ] := BYTE1( nOffset )
           cPCode[ aFlow[6] + 2 ] := BYTE2( nOffset )
        ENDIF
     ELSE
        cPCode += HB_MacroCompile( sCondition )
        cPCode[-1] := HB_P_JUMPFALSE
     ENDIF

     // Bookmark Jump
     aFlow[3] := Len( cPCode )

     cPCode += Chr(0) + Chr(0)

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//
  // Serves Else/Otherwise/Default
  PROCEDURE SetFlowDefault( aProcedure, nLine, sCondition )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     IF aFlow[3] < 0
        Throw( ErrorNew( [PP], 0, 2099, [Parse], [Missing case handler, line: ] + Str( aFlow[2] ), { nLine } ) )
     ENDIF

     (sCondition)

     IF aFlow[1] == 'S'
        // Patch fall through of last SWITCh branch.
        //No need to JUMP/PATCH at all!
     ELSE
        // Unconditional SKIP to after END
        cPCode += Chr( HB_P_JUMP )

        // Bookmark Jump
        aAdd( aFlow[5], Len( cPCode ) )

        cPCode += Chr(0) + Chr(0)
     ENDIF

     // Patch failure Jump of last branch
     nOffset := ( Len( cPCode ) + 1 ) - aFlow[3]

     cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
     cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

     // Elsed!
     aFlow[4] := .T.

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeFlow( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset
     LOCAL nPos

     IF aFlow[3] < 0
        IF -aFlow[3] != Len( cPCode )
           Throw( ErrorNew( [PP], 0, 2099, [Parse], [Empty case structure, line: ] + Str( aFlow[2] ), { nLine } ) )
        ENDIF
     ENDIF

     // No default
     IF aFlow[4] == .F.
        nOffset := ( Len( cPCode ) + 1 ) - aFlow[3]
        //TraceLog( nOffset )
        cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )
     ENDIF

     // Patch Jumps to END
     FOR EACH nPos IN aFlow[5]
        nOffset := ( Len( cPCode ) + 1 ) - nPos
        //TraceLog( nOffset )

        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     IF aFlow[1] == 'S'
        cPCode += Chr( HB_P_POP )
     ENDIF

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeWhile( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset
     LOCAL nPos

     (nLine)

     cPCode += Chr( HB_P_JUMP )

     // Go to TOP
     nOffset := aFlow[6] - Len( cPCode )

     cPCode += Chr( BYTE1( nOffset ) )
     cPCode += Chr( BYTE2( nOffset ) )

     // Patch LOOP jumps
     FOR EACH nPos IN aFlow[7]
        nOffset := aFlow[6] - nPos

        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     // Patch jump to END on FALSE
     nOffset := ( Len( cPCode ) + 1 ) - aFlow[3]

     cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
     cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

     // Patch EXIT jumps
     FOR EACH nPos IN aFlow[8]
        nOffset := ( Len( cPCode ) + 1 ) - nPos

        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlowFor( aProcedure, nLine, sCounter, sFrom, sTo, sStep )

     LOCAL aForFlow := { 'F', nLine, sCounter, sStep, 0, 0, {}, {} }
     LOCAL cPCode

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( sCounter + ":=" + sFrom, PP_CONTEXT_STATEMENT )
     cPCode[-1] := HB_P_NOOP

     // Bookmark TOP of Loop
     aForFlow[5] := Len( cPCode ) + 1

     // Push Counter
     cPCode += HB_MacroCompile( sCounter )
     cPCode[-1] := HB_P_NOOP

     // Push Limit
     cPCode += HB_MacroCompile( sTo )

     IF Val( sStep ) > 0
        cPCode[-1] := Chr( HB_P_LESSEQUAL )
     ELSEIF Val( sStep ) < 0
        cPCode[-1] := Chr( HB_P_GREATEREQUAL )
     ELSE
        cPCode[-1] := HB_P_NOOP

        // Push Step
        cPCode += HB_MacroCompile( sStep )

        // FORTEST
        cPCode[-1] := HB_P_FORTEST
     ENDIF

     cPCode += Chr( HB_P_JUMPFALSE )

     // Bookmark jump to AFTER Loop
     aForFlow[6] := Len( cPCode )

     cPCode += Chr(0) + Chr(0)

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aForFlow )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlowLoop( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][ raScan( aProcedure[3], {|_1| _1[1] $ "WFES" } ) ]
     LOCAL cPCode := aProcedure[2]

     (nLine)

     cPCode += Chr( HB_P_JUMP )

     IF aFlow[1] $ "FW"
        aAdd( aFlow[7], Len( cPCode ) )
     ELSE //E
        aAdd( aFlow[4], Len( cPCode ) )
     ENDIF

     cPCode += Chr(0)
     cPCode += Chr(0)

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlowExit( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][ raScan( aProcedure[3], {|_1| _1[1] $ "WFES" } ) ]
     LOCAL cPCode := aProcedure[2]

     (nLine)

     cPCode += Chr( HB_P_JUMP )

     IF aFlow[1] $ "FW"
        aAdd( aFlow[8], Len( cPCode ) )
     ELSE //ES
        aAdd( aFlow[5], Len( cPCode ) )
     ENDIF

     cPCode += Chr(0)
     cPCode += Chr(0)

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeFor( aProcedure, nLine )

     LOCAL aForFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset
     LOCAL nPos

     (nLine)

     // Patch LOOP Jumps
     FOR EACH nPos IN aForFlow[7]
        nOffset := ( Len( cPCode ) + 1 ) - nPos
        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     // Step logic
     IF aForFlow[4] == '1'
        cPCode += HB_MacroCompile( aForFlow[3] + "++", PP_CONTEXT_STATEMENT )
     ELSE
        cPCode += HB_MacroCompile( aForFlow[3] + "+=" + aForFlow[4], PP_CONTEXT_STATEMENT )
     ENDIF

     cPCode[-1] := HB_P_JUMP

     nOffset := aForFlow[5] - Len( cPCode )

     // Go to TOP ( Push/Push/Push FORTEST)
     cPCode += Chr( BYTE1( nOffset ) )
     cPCode += Chr( BYTE2( nOffset ) )

     nOffset := ( Len( cPCode ) + 1 ) - aForFlow[6]

     cPCode[ aForFlow[6] + 1 ] := BYTE1( nOffset )
     cPCode[ aForFlow[6] + 2 ] := BYTE2( nOffset )

     FOR EACH nPos IN aForFlow[8]
        nOffset := ( Len( cPCode ) + 1 ) - nPos
        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFlowForEach( aProcedure, nLine, sEnumerator, sEnumeration )

     LOCAL aForEachFlow := { 'E', nLine, 0, {}, {} }
     LOCAL cPCode

     //TraceLog( sEnumerator, sEnumeration )

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( '@' + sEnumerator )
     cPCode[-1] := HB_P_NOOP

     cPCode += HB_MacroCompile( sEnumeration )

     cPCode[-1] := HB_P_FOREACH

     cPCode += Chr( HB_P_ENUMERATE )

     // Bookmark TOP as well as JUMP (+1) to after loop
     aForEachFlow[3] := Len( cPCode )

     cPCode += Chr( HB_P_JUMPFALSE ) + Chr(0) + Chr(0)

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aForEachFlow )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeForEach( aProcedure, nLine )

     LOCAL aForEachFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset
     LOCAL nPos

     (nLine)

     cPCode += Chr( HB_P_JUMP )

     nOffset := aForEachFlow[3] - Len( cPCode )

     // Go TOP of loop
     cPCode += Chr( BYTE1( nOffset ) )
     cPCode += Chr( BYTE2( nOffset ) )

     FOR EACH nPos IN aForEachFlow[4]
        nOffset := aForEachFlow[3] - nPos
        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     cPCode += Chr( HB_P_ENDENUMERATE )

     nOffset := Len( cPCode ) - ( aForEachFlow[3] + 1 )

     // Go AFTER loop
     cPCode[ ( aForEachFlow[3] + 1 ) + 1 ] := BYTE1( nOffset )
     cPCode[ ( aForEachFlow[3] + 1 ) + 2 ] := BYTE2( nOffset )

     FOR EACH nPos IN aForEachFlow[5]
        nOffset := Len( cPCode ) - nPos
        cPCode[ nPos + 1 ] := BYTE1( nOffset )
        cPCode[ nPos + 2 ] := BYTE2( nOffset )
     NEXT

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetBegin( aProcedure, nLine )

     LOCAL aFlow := { 'B', nLine, 0, 0 }
     LOCAL cPCode := aProcedure[2]

     cPCode += Chr( HB_P_SEQBEGIN )

     aFlow[3] := Len( cPCode )

     cPCode += Chr(0) + Chr( 0 ) + Chr( 0 )

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aFlow )

  RETURN
  //--------------------------------------------------------------//

  PROCEDURE SetRecover( aProcedure, nLine, sUsing )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     cPCode += Chr( HB_P_SEQEND )

     // Bookmark Jump to after END
     aFlow[4] := Len( cPCode )

     cPCode += Chr(0) + Chr( 0 ) + Chr( 0 )

     cPCode += Chr( HB_P_SEQRECOVER )

     // Patch HB_P_SEQBEGIN
     nOffset := Len( cPCode ) - aFlow[3]

     cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
     cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

     IF ! Empty( sUsing )
        aProcedure[2] := cPCode
        SetLine( aProcedure, nLine )
        cPCode := aProcedure[2]

        cPCode += HB_MacroCompile( sUsing, HB_MACRO_GEN_POP )
        cPCode[-1] := HB_P_NOOP
     ENDIF

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeBegin( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     (nLine)

     //TraceLog( nLine )

     IF aFlow[4] == 0
        cPCode += Chr( HB_P_SEQEND )

        // Patch HB_P_SEQBEGIN
        nOffset := Len( cPCode ) - aFlow[3]

        cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

        cPCode += Chr(4) + Chr( 0 ) + Chr( 0 )
     ELSE
        // Patch HB_P_SEQRECOVER
        nOffset := Len( cPCode ) - aFlow[4]

        cPCode[ aFlow[4] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[4] + 2 ] := BYTE2( nOffset )
     ENDIF

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetTry( aProcedure, nLine )

     LOCAL aFlow := { 'T', nLine, 0, 0, 0, .F. }
     LOCAL cPCode := aProcedure[2]

     cPCode += Chr( HB_P_TRYBEGIN )

     aFlow[3] := Len( cPCode )

     cPCode += Chr(0) + Chr( 0 ) + Chr( 0 )

     aProcedure[2] := cPCode

     aAdd( aProcedure[3], aFlow )

  RETURN
  //--------------------------------------------------------------//

  PROCEDURE SetCatch( aProcedure, nLine, sCatcher )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     cPCode += Chr( HB_P_TRYEND )

     // Bookmark Jump to after END
     aFlow[4] := Len( cPCode )

     cPCode += Chr(0) + Chr( 0 ) + Chr( 0 )

     cPCode += Chr( HB_P_TRYRECOVER )

     // Bookmark jump to FINALLY
     aFlow[5] := Len( cPCode )

     cPCode += Chr(0) + Chr( 0 ) + Chr( 0 )

     // Patch HB_P_TRYBEGIN
     nOffset := aFlow[5] - aFlow[3]

     cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
     cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

     IF ! Empty( sCatcher )
        aProcedure[2] := cPCode
        SetLine( aProcedure, nLine )
        cPCode := aProcedure[2]

        cPCode += HB_MacroCompile( sCatcher, HB_MACRO_GEN_POP )
        cPCode[-1] := HB_P_NOOP
     ELSE
        cPCode += Chr( HB_P_POP )
     ENDIF

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetFinally( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     (nLine)

     IF aFlow[4] == 0
        cPCode += Chr( HB_P_TRYEND )

        aFlow[4] := Len( cPCode )

        cPCode += Chr(4) + Chr( 0 ) + Chr( 0 )

        cPCode += Chr( HB_P_FINALLY )

        // Patch HB_P_TRYBEGIN
        nOffset := Len( cPCode ) - aFlow[3]

        cPCode[ aFlow[3] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[3] + 2 ] := BYTE2( nOffset )

     ELSE
        cPCode += Chr( HB_P_FINALLY )

        // Patch TRYEND
        nOffset := Len( cPCode ) - aFlow[4]

        cPCode[ aFlow[4] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[4] + 2 ] := BYTE2( nOffset )
     ENDIF

     IF aFlow[5] > 0
        // Patch TRYRECOVER
        nOffset := Len( cPCode ) - aFlow[5]

        cPCode[ aFlow[5] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[5] + 2 ] := BYTE2( nOffset )
     ENDIF

     aFlow[6] := .T.
     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeTry( aProcedure, nLine )

     LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]
     LOCAL nOffset

     //TraceLog( aFlow[6] )

     IF aFlow[6]
        cPCode += Chr( HB_P_ENDFINALLY )
     ELSEIF aFlow[4] == 0
        aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )
        Throw( ErrorNew( [PP], 0, 2027, [Parse], [TRY section requires a CATCH or FINALLY handler!], { nLine } ) )
     ELSE
        // Patch HB_P_TRYEND
        nOffset := ( Len( cPCode ) + 1 ) - aFlow[4]

        cPCode[ aFlow[4] + 1 ] := BYTE1( nOffset )
        cPCode[ aFlow[4] + 2 ] := BYTE2( nOffset )
     ENDIF

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE SetWithObject( aProcedure, nLine, sWith )

     LOCAL aFlow := aProcedure[3]
     LOCAL cPCode

     aAdd( aFlow, { 'O', nLine } )

     SetLine( aProcedure, nLine )
     cPCode := aProcedure[2]

     cPCode += HB_MacroCompile( sWith )
     cPCode[-1] := Chr( HB_P_WITHOBJECT )

     aProcedure[2] := cPCode

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE FinalizeWithObject( aProcedure, nLine )

     //LOCAL aFlow := aProcedure[3][-1]
     LOCAL cPCode := aProcedure[2]

     (nLine)

     IF cPCode[-1] == HB_P_NOOP
        cPCode[-1] := HB_P_ENDWITHOBJECT
     ELSE
        cPCode += Chr( HB_P_ENDWITHOBJECT )
     ENDIF

     aProcedure[2] := cPCode

     aSize( aProcedure[3], Len( aProcedure[3] ) - 1 )

  RETURN

#else

   //--------------------------------------------------------------//

    FUNCTION Throw( xVal )

       IF Empty( s_aProc )
          Eval( ErrorBlock(), xVal )
          // Safety
          Break( xVal )
       ENDIF

       // Script error
       IF s_lTrying
          Break( xVal )
       ENDIF

       BEGIN SEQUENCE
          s_xRet := Eval( s_bRTEBlock, xVal )
       RECOVER
          Break( xVal )
       END

    RETURN s_xRet


  //--------------------------------------------------------------//

  FUNCTION PP_ExecMethod( sProcName, p1, p2, p3, p4, p5, p6, p7, p8, p9 )

     LOCAL i, sProc, nProc, nParams

     sProcName := Upper( sProcName )

     sProc := s_sModule + sProcName
     nProc := aScan( s_aProcedures, {|aProc| aProc[1] == sProc } )

     IF nProc == 0
        sProc := sProcName
        nProc := aScan( s_aProcedures, {|aProc| aProc[1] == sProc } )
     ENDIF

     #ifdef __XHARBOUR__
        s_aParams := HB_aParams()
        aDel( s_aParams, 1, .T. )
     #else
        nParams := PCount()
        s_aParams := {}

        #ifdef __CLIP__
           FOR i := 2 TO nParams
              aAdd( s_aParams, Param( i ) )
           NEXT
        #else
           nParams--

           DO CASE
              CASE nParams == 1
                 aAdd( s_aParams, p1 )
              CASE nParams == 2
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
              CASE nParams == 3
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
              CASE nParams == 4
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
              CASE nParams == 5
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
                 aAdd( s_aParams, p5 )
              CASE nParams == 6
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
                 aAdd( s_aParams, p5 )
                 aAdd( s_aParams, p6 )
              CASE nParams == 7
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
                 aAdd( s_aParams, p5 )
                 aAdd( s_aParams, p6 )
                 aAdd( s_aParams, p7 )
              CASE nParams == 8
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
                 aAdd( s_aParams, p5 )
                 aAdd( s_aParams, p6 )
                 aAdd( s_aParams, p7 )
                 aAdd( s_aParams, p8 )
              CASE nParams == 9
                 aAdd( s_aParams, p1 )
                 aAdd( s_aParams, p2 )
                 aAdd( s_aParams, p3 )
                 aAdd( s_aParams, p4 )
                 aAdd( s_aParams, p5 )
                 aAdd( s_aParams, p6 )
                 aAdd( s_aParams, p7 )
                 aAdd( s_aParams, p8 )
                 aAdd( s_aParams, p9 )
           ENDCASE
        #endif
     #endif

     IF nProc > 0
        //? p1, p2, p3, p3, p4, p5, p6, p7, p8, p9
        //? "METHOD:", sProcName, s_aParams[1], s_aParams[2], s_aParams[3]
        //TraceLog()
        //Inkey(0)
        PP_ExecProcedure( s_aProcedures, nProc )
     ELSE
        Eval( s_bRTEBlock, ErrorNew( [PP], 0, 1004, sProcName, [Missing Method: ], s_aParams ) )
        // Safety
        BREAK
     ENDIF

  RETURN s_xRet

  //--------------------------------------------------------------//

  FUNCTION PP_ExecProcedure( aProcedures, nProc, aParams )

     LOCAL nBlock, nBlocks, xErr
     LOCAL aLastSequence, lRecover
     LOCAL aBlocks, aCode, Code1, OpCode
     LOCAL nForEachIndex
     LOCAL aProc
     LOCAL aPresetProc
     LOCAL aTopProcStack, aProcStack
     LOCAL nLocalRecover
     LOCAL nSequence

     // Debug only
     #ifdef _DEBUG
        LOCAL c
     #endif

     IF aProcedures == NIL
        aProcedures := s_aProcedures
     ENDIF

     aProc := aProcedures[nProc]

     aPresetProc := ScriptProlog( aProc, aParams )

     // Reset
     lRecover      := .F.
     nForEachIndex := s_nForEachIndex
     nLocalRecover := Len( s_anRecover )
     nSequence     := 0

     aProcStack := s_aProcStack[s_nProcStack]
     s_lReturnRequested := .F.

     #ifdef __XHARBOUR__
        SET( _SET_ERRORLOOP, SET( _SET_ERRORLOOP ) + 8 )
     #endif

     aBlocks := aProc[2]

     aTopProcStack := s_aProcStack[ Len( s_aProcStack ) ]

     // Do NOT optimize, can NOT use FOR EACH!!!
     nBlocks := Len( aBlocks )

     FOR nBlock := 1 TO nBlocks
        aCode := aBlocks[nBlock]
        Code1 := aCode[1]
        OpCode := aCode[2]

        //TraceLog( nBlock, Code1, OpCode )
        //OutputDebugString( "Block: " + Str( nBlock ) + "Op: " + CStr( OpCode ) )

      #ifdef __MACRO_COMPILE__
        IF HB_ISSTRING( OpCode )
      #else
        IF HB_ISBLOCK( OpCode )
      #endif
           aProcStack[2] := aCode[3] // Line No.

           BEGIN SEQUENCE

              //TraceLog( "Line: " + Str( aCode[3], 3 ) )

              IF Code1 == 0
                 //? aCode[3]
                 #ifdef __MACRO_COMPILE__
                    HB_vmExecute( opCode )
                 #else
                    Eval( OpCode )
                 #endif
              ELSE
                #ifdef __MACRO_COMPILE__
                 IF ! HB_vmExecute( OpCode ) // Jump if FALSE.
                #else
                 IF ! Eval( OpCode ) // Jump if FALSE.
                #endif
                    nBlock := Code1
                    //TraceLog( "Jump: " + Str( aCode[3], 3 ) )
                 ENDIF
              ENDIF

           RECOVER USING xErr

              //TraceLog( "Recovering!!!", xErr, s_anRecover, s_lReturnRequested, s_lTrying )

              IF s_lReturnRequested
                 //TraceLog( "Return" )
                 // Return from this procedure requested by PP__Return()
                 s_lReturnRequested := .F.
                 EXIT
              ELSE
                 IF s_lTrying
                    // Removal of envelope will take place once the END is encountered.
                    s_lTrying := .F.

                    s_nForEachIndex := aLastSequence[1]
                    s_lTrying := aLastSequence[2]

                    #ifdef __XHARBOUR__
                       WHILE HB_WithObjectCounter() > aLastSequence[3]
                          HB_SetWith()
                       END
                    #endif

                    aSize( s_aEnumerations, s_nForEachIndex )
                    aSize( s_anEnumIndex, s_nForEachIndex )
                    aSize( s_anEnumerator, s_nForEachIndex )
                    aSize( s_anForEachStartingBlock, s_nForEachIndex )
                 ENDIF

                 IF nSequence > nLocalRecover
                    IF ! Empty( s_acRecover[ nSequence ] )
                       //TraceLog( s_acRecover[ Len( s_acRecover ) ] )
                       M->&( s_acRecover[ nSequence ] ) := xErr
                    ENDIF

                    nBlock := s_anRecover[ nSequence ]
                    nSequence--
                 ELSEIF ! Empty( s_anRecover )
                    //TraceLog( "Return to RECOVER of parrent", PP_ProcName(), PP_ProcLine() )
                    lRecover := .T.
                    EXIT
                 ELSE
                    Break( xErr )
                 ENDIF
              ENDIF

           END SEQUENCE

        ELSEIF OpCode == PP_OP_JUMP

           nBlock := Code1

        ELSEIF OpCode == PP_OP_TRY

           aAdd( s_acRecover, Code1[1] ) // Catcher Var
           aAdd( s_anRecover, Code1[2] ) // Recovery Address
           #ifdef __XHARBOUR__
              aAdd( s_aSequence, { s_nForEachIndex, s_lTrying, HB_WithObjectCounter() } )
           #else
              aAdd( s_aSequence, { s_nForEachIndex, s_lTrying, 0 } )
           #endif
           aLastSequence := s_aSequence[ nSequence := Len( s_aSequence ) ]

           s_lTrying := .T.

           //TraceLog( s_acRecover, s_anRecover, s_aSequence )

        ELSEIF OpCode == PP_OP_ENDTRY

           s_lTrying := aLastSequence[2]
           aSize( s_acRecover, Len( s_acRecover ) - 1 )
           aSize( s_anRecover, Len( s_acRecover ) )
           aSize( s_aSequence, Len( s_acRecover ) )

           IF ( nSequence := Len( s_aSequence ) ) > 0
              aLastSequence := s_aSequence[ nSequence ]
           ELSE
              aLastSequence := NIL
           ENDIF

           //TraceLog( s_acRecover, s_anRecover, s_aSequence )

        ELSEIF OpCode == PP_OP_BEGIN

           aAdd( s_acRecover, Code1[1] ) // Catcher Var
           aAdd( s_anRecover, Code1[2] ) // Recovery Address
           #ifdef __XHARBOUR__
              aAdd( s_aSequence, { s_nForEachIndex, s_lTrying, HB_WithObjectCounter() } )
           #else
              aAdd( s_aSequence, { s_nForEachIndex, s_lTrying, 0 } )
           #endif
           aLastSequence := s_aSequence[ nSequence := Len( s_aSequence ) ]
           s_lTrying := .F.

           //TraceLog( s_acRecover, s_anRecover, s_aSequence )

        ELSEIF OpCode == PP_OP_ENDBEGIN

           s_lTrying := aLastSequence[2]
           aSize( s_acRecover, Len( s_acRecover ) - 1 )
           aSize( s_anRecover, Len( s_acRecover ) )
           aSize( s_aSequence, Len( s_acRecover ) )

           IF ( nSequence := Len( s_aSequence ) ) > 0
              aLastSequence := s_aSequence[ nSequence ]
           ELSE
              aLastSequence := NIL
           ENDIF

           //TraceLog( s_acRecover, s_anRecover, s_aSequence )

        ELSEIF OpCode == PP_OP_FOREACH

           s_nForEachIndex++

           aAdd( s_aEnumerations, &( Code1[1] ) )
           aAdd( s_anEnumIndex, 1 )
           aAdd( s_anEnumerator, Code1[2] )
           aAdd( s_anForEachStartingBlock, nBlock )

           IF Len( s_aEnumerations[ s_nForEachIndex ] ) > 1
              &( s_anEnumerator[ s_nForEachIndex ] ) := s_aEnumerations[ s_nForEachIndex ][1]
           ELSE
              nBlock := Code1[3]
           ENDIF

        ELSEIF OpCode == PP_OP_ENDFOREACH .OR. OpCode == PP_OP_LOOPFOREACH

           s_anEnumIndex[ s_nForEachIndex ] := s_anEnumIndex[ s_nForEachIndex ] + 1

           IF Len( s_aEnumerations[ s_nForEachIndex ] ) >= s_anEnumIndex[ s_nForEachIndex ]
              &( s_anEnumerator[ s_nForEachIndex ] ) := s_aEnumerations[ s_nForEachIndex ][ s_anEnumIndex[ s_nForEachIndex ] ]
              // Loop back.
              nBlock := s_anForEachStartingBlock[ s_nForEachIndex ]
           ELSE
              s_nForEachIndex--
              aSize( s_aEnumerations, s_nForEachIndex )
              aSize( s_anEnumIndex, s_nForEachIndex )
              aSize( s_anEnumerator, s_nForEachIndex )
              aSize( s_anForEachStartingBlock, s_nForEachIndex )
           ENDIF

        //ELSEIF OpCode == PP_OP_NOOP

           // Do nothing.
        ELSE

            Eval( ErrorBlock(), ErrorNew( [PP], 0, 1005, ProcName(), [Unsupported OPCode.], { s_aProc, OpCode, nBlock } ) )
            // Safety
            BREAK

        ENDIF
     NEXT

     //TraceLog( "DONE: " + aProc[1], ValToPrg( s_aStatics ) )

     // Reset
     aSize( s_acRecover, nLocalRecover )
     aSize( s_anRecover, nLocalRecover )
     aSize( s_aSequence, nLocalRecover )
     IF nLocalRecover > 0
        s_lTrying := s_aSequence[ nLocalRecover ][2]
     ENDIF

     // Reset
     s_nForEachIndex := nForEachIndex
     aSize( s_aEnumerations, s_nForEachIndex )
     aSize( s_anEnumIndex, s_nForEachIndex )
     aSize( s_anEnumerator, s_nForEachIndex )
     aSize( s_anForEachStartingBlock, s_nForEachIndex )

     #ifdef __XHARBOUR__
        SET( _SET_ERRORLOOP, SET( _SET_ERRORLOOP ) - 8 )
     #endif

     ScriptEpilog( aPresetProc )

     IF lRecover
        Break( xErr )
     ENDIF

  RETURN s_xRet

  //--------------------------------------------------------------//

  PROCEDURE PP__LocalParams( aVars )

     LOCAL nVar, nVars := Len( aVars ), xInit, nParams, cVar

     //TraceLog( ValToPrg( s_aParams ) )

     FOR nVar := 1 TO nVars
        IF ( nParams := Len( s_aParams ) ) > 0
           xInit := s_aParams[1]
           aDel( s_aParams, 1 )
           aSize( s_aParams, nParams - 1 )
        ELSE
           xInit := NIL
        ENDIF

        //? nVar, aVars[nVar], xInit
        //Inkey(0)

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0
      #endif
           __QQPub( cVar )

           #ifdef __HARBOUR__
              __MVPUT( cVar, xInit )
           #else
              &( cVar ) := xInit
           #endif

           aAdd( s_asLocals, cVar )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2034, cVar, [ Declared Parameter redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PP__Params( aVars )

     LOCAL nVar, nVars := Len( aVars ), xInit, nParams, cVar

     FOR nVar := 1 TO nVars
        IF ( nParams := Len( s_aParams ) ) > 0
           xInit := s_aParams[1]
           aDel( s_aParams, 1 )
           aSize( s_aParams, nParams - 1 )
        ELSE
           xInit := NIL
        ENDIF

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, cVar, , , .T. ) == 0 .AND. aScan( s_asPublics, cVar, , , .T. ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, {|cPrivate| cPrivate == cVar } ) == 0 .AND. aScan( s_asPublics, {|sPublic| sPublic == cVar } ) == 0
      #endif
           __QQPub( cVar )

           #ifdef __HARBOUR__
              __MVPUT( cVar, xInit )
           #else
              &( cVar ) := xInit
           #endif

           aAdd( s_asPrivates, cVar )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2034, cVar, [ Declared Parameter redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PP__Privates( aVars )

     LOCAL nVar, nVars := Len( aVars ), nAt, cInit, cVar

     FOR nVar := 1 TO nVars
        IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
           cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
           aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
        ELSE
           cInit := "NIL"
        ENDIF

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, cVar, , , .T. ) == 0 .AND. aScan( s_asPublics, cVar, , , .T. ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, {|cPrivate| cPrivate == cVar } ) == 0 .AND. aScan( s_asPublics, {|sPublic| sPublic == cVar } ) == 0
      #endif
           __QQPub( cVar )

           #ifdef __HARBOUR__
              __MVPut( cVar, &( cInit ) )
           #else
              &( cVar ) := &( cInit )
           #endif

           aAdd( s_asPrivates, cVar )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2016, cVar, [ Private redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PP__Locals( aVars )

     LOCAL nVar, nVars := Len( aVars ), nAt, xInit, cVar

     FOR nVar := 1 TO nVars
        IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
           //Alert( LTrim( SubStr( aVars[nVar], nAt + 2 ) ) )
           xInit := &( LTrim( SubStr( aVars[nVar], nAt + 2 ) ) )
           aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
        ENDIF

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, {|cPrivate| cPrivate == cVar } ) == 0 .AND. aScan( s_asPublics, {|sPublic| sPublic == cVar } ) == 0
      #endif
           __QQPub( cVar )

           #ifdef __HARBOUR__
              TraceLog( cVar, xInit )
              __MVPut( cVar, xInit )
           #else
              &( cVar ) := xInit
           #endif

           aAdd( s_asLocals, cVar )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2016, cVar, [ Local redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PP__Publics( aVars )

     LOCAL nVar, nVars := Len( aVars ), nAt, cInit, cVar

     FOR nVar := 1 TO nVars
        IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
           cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
           aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
        ELSE
           cInit := ".F."
        ENDIF

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, cVar, , , .T. ) == 0 .AND. aScan( s_asPublics, cVar, , , .T. ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0 .AND. aScan( s_asPrivates, {|cPrivate| cPrivate == cVar } ) == 0 .AND. aScan( s_asPublics, {|sPublic| sPublic == cVar } ) == 0
      #endif
           __QQPub( cVar )

           #ifdef __HARBOUR__
              __MVPut( cVar, &( cInit ) )
           #else
              &( cVar ) := &( cInit )
           #endif

           aAdd( s_asPublics, cVar )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2016, cVar, [ Public redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE PP__Statics( aVars )

     LOCAL nVar, nVars := Len( aVars ), nAt, cInit, cVar

     //TraceLog( s_aProc[1], ValToPrg( s_aStatics ) )

     IF s_aStatics == NIL
        s_aStatics := {}
     ENDIF

     FOR nVar := 1 TO nVars
        IF ( nAt := At( ":=", aVars[nVar] ) ) > 0
           cInit := LTrim( SubStr( aVars[nVar], nAt + 2 ) )
           aVars[nVar] := RTrim( Left( aVars[nVar], nAt - 1 ) )
        ELSE
           cInit := "NIL"
        ENDIF

        cVar := Upper( aVars[nVar] )

      #ifdef __XHARBOUR__
        IF aScan( s_asLocals, cVar, , , .T. ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0
      #else
        IF aScan( s_asLocals, {|cLocal| cLocal == cVar } ) == 0 .AND. aScan( s_aStatics, {|aStatic| aStatic[1] == cVar } ) == 0
      #endif
           //Alert( [Creating static: ] + cVar + [ in ] + s_aProc[1] )
           __QQPub( cVar )

           #ifdef __HARBOUR__
              __MVPut( cVar, &( cInit ) )
           #else
              &( cVar ) := &( cInit )
           #endif

           aAdd( s_aStatics, { cVar, NIL } )
        ELSE
           Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2034, cVar, [ Declared Static redeclaration: ], aVars ) )
           // Safety
           BREAK
        ENDIF
     NEXT

     IF s_nProcStack > 0
        IF Len( s_aProc ) == 2
           aSize( s_aProc, 3 )
        ENDIF

        s_aProc[ 3 ] := s_aStatics
     ENDIF

  RETURN

  //--------------------------------------------------------------//

  PROCEDURE ParseLine( sLine, nLine, nProcID, aProcedures, aInitExit )

     LOCAL sSymbol
     LOCAL nAt, nPos, cChr
     LOCAL nJumps, nJump
     LOCAL sCounter, sStart, sEnd, sStep
     LOCAL sEnumerator, sEnumeration
     LOCAL cPCode

     IF sLine = "PP_PROC"

        sSymbol := Upper( LTrim( SubStr( sLine, At( ' ', sLine ) ) ) )
        aSize( aProcedures, ++nProcId )

        IF sLine = "PP_PROC_PRG"
           sSymbol := s_sModule + sSymbol
        ELSEIF sLine = "PP_PROC_INIT"
           aAdd( aInitExit[1], nProcId )
        ELSEIF sLine = "PP_PROC_EXIT"
           aAdd( aInitExit[2], nProcId )
        ENDIF

        //TraceLog( sSymbol , nProcId )
        aProcedures[nProcId] := { sSymbol, {} }

     ELSE

        // No procedure declaration.
        IF nProcId == 0
           sSymbol := "Implied_Main"
           aSize( aProcedures, ++nProcId )
           aProcedures[nProcId] := { sSymbol, {} }
        ENDIF

        IF sLine = "PP__"

           IF sLine = "PP__FOREACH"

              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "E", nLine }

              sLine := SubStr( sLine, 13 )
              sEnumerator := Left( sLine, ( nAt := AT( "~$~", sLine ) ) - 1 )
              sEnumeration := SubStr( sLine, nAt + 3 )

              aAdd( aProcedures[ nProcId ][2], { { sEnumeration, sEnumerator, 0 }, PP_OP_FOREACH, nLine } ) // Loop back

              s_nCompLoop++
              aSize( s_aLoopJumps, s_nCompLoop )
              s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ), {}, "E" } // Address of line to later place conditional Jump instruction into.

              RETURN

           ELSEIF sLine = "PP__FOR"

              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "F", nLine }

              sLine := SubStr( sLine, 9 )
              sCounter := Left( sLine, ( nAt := AT( ":=", sLine ) ) - 1 )
              sLine   := SubStr( sLine, nAt + 2 )
              sStart   := Left( sLine, ( nAt := At( "~TO~", sLine ) ) - 1 )
              sLine   := SubStr( sLine, nAt + 4 )
              sEnd     := Left( sLine, ( nAt := At( "~STEP~", sLine ) ) - 1 )
              sStep    := SubStr( sLine, nAt + 6 )
              IF sStep == ""
                 sStep := "1"
              ENDIF

              #ifdef __MACRO_COMPILE__
                 aAdd( aProcedures[ nProcId ][2], { 0, HB_MacroCompile( sCounter + ":=" + sStart, PP_CONTEXT_STATEMENT ), nLine } ) // Loop back
              #else
                 aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sCounter + ":=" + sStart + "}" ), nLine } ) // Loop back
              #endif

              IF Val( sStep ) < 0
                 sLine := sCounter + ">=" + sEnd
              ELSE
                 sLine := sCounter + "<=" + sEnd
              ENDIF

              s_nCompLoop++
              aSize( s_aLoopJumps, s_nCompLoop )

              #ifdef __MACRO_COMPILE__
                  s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "F", HB_MacroCompile( sCounter + ":=" + sCounter + "+" + sStep, PP_CONTEXT_STATEMENT ) } // Address of line to later place conditional Jump instruction into.
              #else
                  s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "F", &( "{||" + sCounter + ":=" + sCounter + "+" + sStep + "}" ) } // Address of line to later place conditional Jump instruction into.
              #endif

           ELSEIF sLine = "PP__NEXT"

              IF s_nFlowId > 0 .AND. s_acFlowType[ s_nFlowId ][1] == "E"
                 aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_ENDFOREACH, nLine } ) // Register Recovery Address and Catch Var.
              ELSEIF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "F"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2029, [Parse], [NEXT does not match FOR], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                 aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, PP_OP_JUMP, nLine } ) // Loop back
                 aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
              ENDIF

              nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
              FOR nJump := 1 TO nJumps
                 aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
              NEXT

              s_nCompLoop--
              s_nFlowId--

              RETURN

           ELSEIF sLine = "PP__TRY"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "T", 0 }

              aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_TRY, nLine } ) // Register Recovery Address and Catch Var.

              s_nCompLoop++
              aSize( s_aLoopJumps, s_nCompLoop )
              s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ), {}, "T" } // Address of line to later place conditional Jump instruction into.

              RETURN

           ELSEIF sLine = "PP__BEGIN"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "B", nLine }

              aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_BEGIN, nLine } ) // Register Recovery Address and Catch Var.

              s_nCompLoop++
              aSize( s_aLoopJumps, s_nCompLoop )
              s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ), {}, "B" } // Address of line to later place conditional Jump instruction into.

              RETURN

           ELSEIF sLine = "PP__CATCH"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "T"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2046, [Parse], [CATCH with no TRY in sight!], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_JUMP, nLine } ) // Unconditional Jump to END

                 aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := { SubStr( sLine, 11 ), Len( aProcedures[ nProcId ][2] ) }

                 s_aLoopJumps[ s_nCompLoop ][1] := Len( aProcedures[ nProcId ][2] )

                 RETURN
              ENDIF

           ELSEIF sLine = "PP__RECOVER"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "B"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2046, [Parse], [RECOVER with no BEGIN SEQUENCE in sight!], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_JUMP, nLine } ) // Unconditional Jump to END

                 aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := { SubStr( sLine, 13 ), Len( aProcedures[ nProcId ][2] ) }

                 s_aLoopJumps[ s_nCompLoop ][1] := Len( aProcedures[ nProcId ][2] )

                 RETURN
              ENDIF

           ELSEIF sLine = "PP__WHILE"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "W", nLine }

              sLine := SubStr( sLine, 11 )
              s_nCompLoop++
              aSize( s_aLoopJumps, s_nCompLoop )
              s_aLoopJumps[ s_nCompLoop ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "W" } // Address of line to later place conditional Jump instruction into.

           ELSEIF sLine = "PP__WITHOBJECT"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "O", nLine }

              sLine := "HB_SetWith(" + SubStr( sLine, 16 ) + ")"

           ELSEIF sLine = "PP__LOOP"

              IF s_nCompLoop == 0
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2012, [Parse], [LOOP with no loop in sight!], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 IF s_aLoopJumps[ s_nCompLoop ][3] == "F"
                    aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } )
                 ELSEIF s_aLoopJumps[ s_nCompLoop ][3] == "E"
                    aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_LOOPFOREACH, nLine } ) // Place holder for unconditional Jump to END.
                    RETURN
                 ENDIF

                 aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, PP_OP_JUMP, nLine } ) // Loop back
              ENDIF

              RETURN

           ELSEIF sLine = "PP__EXIT"

              IF s_nCompLoop == 0
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2011, [Parse], [EXIT with no loop in sight!], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_JUMP, nLine } ) // Place holder for unconditional Jump to END.
                 aAdd( s_aLoopJumps[ s_nCompLoop ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
              ENDIF

              RETURN

           ELSEIF sLine = "PP__ENDDO"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "W"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2028, [Parse], [ENDDO does not match WHILE], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, PP_OP_JUMP, nLine } ) // Loop back
                 aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                 nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )
                 FOR nJump := 1 TO nJumps
                    aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                 NEXT

                 s_nCompLoop--
                 s_nFlowId--
              ENDIF

              RETURN

           ELSEIF sLine = "PP__DOCASE"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "C", nLine }

              s_nCompIf++
              aSize( s_aIfJumps, s_nCompIf )
              s_aIfJumps[ s_nCompIf ] := { 0, {}, "C", .F. } // Address of line to later place conditional Jump instruction into.

              RETURN

           ELSEIF sLine = "PP__CASE"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "C"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2031, [Parse], [CASE does not match DO CASE], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 IF s_aIfJumps[ s_nCompIf ][1] > 0
                    aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_JUMP, nLine } ) // Place holder for unconditional Jump to END.
                    aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                 ENDIF

                 sLine := SubStr( sLine, 10 )
                 s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place conditional Jump instruction into.
              ENDIF

           ELSEIF sLine = "PP__OTHERWISE"

              IF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "C" .OR. s_aIfJumps[ s_nCompIf ][4]
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2031, [Parse], [OTHERWISE does not match DO CASE], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 s_aIfJumps[ s_nCompIf ][4] := .T.

                 aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_JUMP, nLine } ) // Place holder for unconditional Jump to END.

                 IF s_aIfJumps[ s_nCompIf ][1] > 0
                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                    s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) // Address of line to later place Jump instruction into.
                 ENDIF
              ENDIF

              RETURN

           ELSEIF sLine = "PP__ENDCASE"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "C"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2030, [Parse], [ENDCASE with no DO CASE in sight!], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 IF s_aIfJumps[ s_nCompIf ][1] > 0
                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                    nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                    FOR nJump := 1 TO nJumps
                       aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                    NEXT
                 ENDIF

                 s_nCompIf--
                 s_nFlowId--
              ENDIF

              RETURN

           ELSEIF sLine = "PP__IF"
              s_nFlowId++
              aSize( s_acFlowType, s_nFlowId )
              s_acFlowType[ s_nFlowId ] := { "I", nLine }

              sLine := SubStr( sLine, 8 )
              s_nCompIf++
              aSize( s_aIfJumps, s_nCompIf )
              s_aIfJumps[ s_nCompIf ] := { Len( aProcedures[ nProcId ][2] ) + 1, {}, "I", .F. } // Address of line to later place conditional Jump instruction into.

           ELSEIF sLine = "PP__ELSEIF"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "I"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2026, [Parse], [ELSEIF does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSEIF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2026, [Parse], [ELSEIF does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 IF s_aIfJumps[ s_nCompIf ][1] > 0
                    aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_JUMP, nLine } ) // Place holder for unconditional Jump to END.
                    aAdd( s_aIfJumps[ s_nCompIf ][2], Len( aProcedures[ nProcId ][2] ) ) // Address of line to later place unconditional Jump instruction into.
                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                 ENDIF

                 sLine := SubStr( sLine, 12 )
                 s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) + 1 // Address of line to later place Jump instruction into.
              ENDIF

           ELSEIF sLine = "PP__ELSE"

              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "I"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2026, [Parse], [ELSE does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSEIF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I" .OR. s_aIfJumps[ s_nCompIf ][4]
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2025, [Parse], [ELSE does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 s_aIfJumps[ s_nCompIf ][4] := .T.

                 aAdd( aProcedures[ nProcId ][2], { 0, PP_OP_JUMP, nLine } ) // Place holder for unconditional Jump to END.

                 aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the prebvious conditional Jump Instruction
                 s_aIfJumps[ s_nCompIf ][1] := Len( aProcedures[ nProcId ][2] ) // Address of line to later place Jump instruction into.
              ENDIF

              RETURN

           ELSEIF sLine = "PP__ENDIF"
              IF s_nFlowId == 0 .OR. s_acFlowType[ s_nFlowId ][1] != "I"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2026, [Parse], [ENDIF does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSEIF s_nCompIf == 0 .OR. s_aIfJumps[ s_nCompIf ][3] != "I"
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2027, [Parse], [ENDIF does not match IF], { sLine } ) )
                 // Safety
                 BREAK
              ELSE
                 aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                 nJumps := Len( s_aIfJumps[s_nCompIf][2] )
                 FOR nJump := 1 TO nJumps
                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                 NEXT

                 s_nCompIf--
                 s_nFlowId--
              ENDIF

              RETURN

           ELSEIF sLine = "PP__END"

              IF s_nFlowId == 0
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2027, [Parse], [END with no Flow-Control structure in sight!], { sLine } ) )

                 // Safety
                 BREAK
              ELSE
                 IF s_acFlowType[ s_nFlowId ][1] $ "FW"

                    IF s_acFlowType[ s_nFlowId ][1] == "F"
                       aAdd( aProcedures[ nProcId ][2], { 0, s_aLoopJumps[ s_nCompLoop ][4], nLine } ) // STEP
                    ENDIF

                    aAdd( aProcedures[ nProcId ][2], { s_aLoopJumps[ s_nCompLoop ][1] - 1, PP_OP_JUMP, nLine } ) // Loop back

                    aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                    nJumps := Len( s_aLoopJumps[s_nCompLoop][2] )

                    FOR nJump := 1 TO nJumps
                       aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                    NEXT

                    s_nCompLoop--
                    //aSize( s_aLoopJumps, s_nCompLoop )

                 ELSEIF s_acFlowType[ s_nFlowId ][1] == "T"

                    IF aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][2] == PP_OP_JUMP
                       aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                    ELSE
                       aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := { NIL , Len( aProcedures[ nProcId ][2] ) } // Patching the previous conditional Jump Instruction
                    ENDIF
                    s_nCompLoop--

                    aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_ENDTRY, nLine } ) // Register Recovery Address and Catch Var.

                 ELSEIF s_acFlowType[ s_nFlowId ][1] == "B"

                    IF aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][2] == PP_OP_JUMP
                       aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction
                    ELSE
                       aProcedures[ nProcId ][2][ s_aLoopJumps[s_nCompLoop][1] ][1] := { NIL , Len( aProcedures[ nProcId ][2] ) } // Patching the previous conditional Jump Instruction
                    ENDIF
                    s_nCompLoop--

                    aAdd( aProcedures[ nProcId ][2], { NIL, PP_OP_ENDBEGIN, nLine } ) // Register Recovery Address and Catch Var.

                 ELSEIF s_acFlowType[ s_nFlowId ][1] == "O"

                    #ifdef __MACRO_COMPILE__
                       aAdd( aProcedures[ nProcId ][2], { 0, HB_MacroCompile( "HB_SetWith()", PP_CONTEXT_STATEMENT ), nLine } ) // Reset
                    #else
                       aAdd( aProcedures[ nProcId ][2], { 0, &( "{|| HB_SetWith() }" ), nLine } ) // Reset
                    #endif
                 ELSE

                    aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][1] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the previous conditional Jump Instruction

                    nJumps := Len( s_aIfJumps[s_nCompIf][2] )

                    FOR nJump := 1 TO nJumps
                       aProcedures[ nProcId ][2][ s_aIfJumps[s_nCompIf][2][nJump] ][1] := Len( aProcedures[ nProcId ][2] ) // Patching the unconditional Jump Instruction
                    NEXT

                    s_nCompIf--

                 ENDIF

              ENDIF

              s_nFlowId--

              RETURN

           ENDIF
        ELSE
           sLine := ParseAssign( sLine )

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif
        ENDIF

        //TraceLog( sLine, sSymbol )

        #ifdef __MACRO_COMPILE__
           aAdd( aProcedures[ nProcId ][2], { 0, HB_MacroCompile( sLine ), nLine } )
        #else
           aAdd( aProcedures[ nProcId ][2], { 0, &( "{||" + sLine + "}" ), nLine } )
        #endif
     ENDIF

  RETURN

  //--------------------------------------------------------------//

  FUNCTION ScriptProlog( aProc, aParams )

     LOCAL aPresetProc := s_aProc
     LOCAL aProcStack
     LOCAL nVar, nVars

     IF aParams == NIL
        s_aParams := {}
     ELSE
        s_aParams := aParams
     ENDIF

     s_xRet := NIL
     s_aProc := aProc

     //TraceLog( "DOING: " + aProc[1] )

     IF s_nProcStack > 0
        aProcStack := s_aProcStack[s_nProcStack]

        /* Saving and Releasing Statics of upper level. */
        IF s_aStatics != NIL
           nVars := Len( s_aStatics )

           FOR nVar := 1 TO nVars
              s_aStatics[nVar][2] := M->&( s_aStatics[nVar][1] )
              __MXRelease( s_aStatics[nVar][1] )
              //Alert( [Released upper static: ] + s_aStatics[nVar][1] + [ in ] + aProcStack[1] )
              //TraceLog( [Saved and Released upper static: ] + s_aStatics[nVar][1] + [ in ] + aProc[1], s_aStatics[nVar][2] )
           NEXT

           s_aStatics := NIL
        ENDIF

        /* Saving Privates of upper level. */
        nVars := Len( s_asPrivates )
        aAdd( aProcStack, Array( nVars, 2 ) )

        FOR nVar := 1 TO nVars
           aProcStack[3][nVar][1] := s_asPrivates[nVar]

           #ifdef __HARBOUR__
              aProcStack[3][nVar][2] := __MVGET( s_asPrivates[nVar] )
           #else
              aProcStack[3][nVar][2] := &( s_asPrivates[nVar] )
           #endif
           //Alert( [Saved upper Private: ] + s_asPrivates[nVar] + [ in ] + aProcStack[1] )
        NEXT

        aSize( s_asPrivates, 0 )

        /* Saving and Releasing Locals of upper level. */
        nVars := Len( s_asLocals )
        aAdd( aProcStack, Array( nVars, 2 ) )

        FOR nVar := 1 TO nVars
           aProcStack[4][nVar][1] := s_asLocals[nVar]

           #ifdef __HARBOUR__
              aProcStack[4][nVar][2] := __MVGet( s_asLocals[nVar] )
           #else
              aProcStack[4][nVar][2] := &( s_asLocals[nVar] )
           #endif

           __MXRelease( s_asLocals[nVar] )
           //Alert( [Released upper local: ] + s_asLocals[nVar] + [ in ] + aProc[1] )
           //TraceLog( [Released upper local: ] + s_asLocals[nVar] + [ in ] + aProc[1] )
        NEXT

        aSize( s_asLocals, 0 )
     ENDIF

     /* Reinstating Current Statics if any. */
     IF Len( aProc ) >= 3
        s_aStatics := aProc[3]
        nVars := Len( s_aStatics )

        FOR nVar := 1 TO nVars
           #ifdef __HARBOUR__
              __QQPub( s_aStatics[nVar][1] )

              __MVPut( s_aStatics[nVar][1], s_aStatics[nVar][2] )

              //Alert( [ReInstated static: ] + s_aStatics[nVar][1] + [ for ] + aProc[1] )
              //TraceLog( [ReInstated static: ] + s_aStatics[nVar][1] + [ for ] + aProc[1], s_aStatics[nVar][2] )
           #else
              __QQPub( s_aStatics[nVar][1] )
              &( s_aStatics[nVar][1] ) := s_aStatics[nVar][2]
           #endif
        NEXT
     ENDIF

     aAdd( s_aProcStack, { aProc[1], 0 } )
     s_nProcStack++

  RETURN aPresetProc

  //--------------------------------------------------------------//

  PROCEDURE ScriptEpilog( aPresetProc )

     LOCAL aProcStack
     LOCAL nVar, nVars

     //TraceLog( "DONE: " + s_aProcStack[-1][1] )

     /* Saving and Releasing Statics created by the Procedure. */
     IF s_aStatics != NIL
        nVars := Len( s_aStatics )

        FOR nVar := 1 TO nVars
           s_aStatics[nVar][2] := M->&( s_aStatics[nVar][1] )
           __MXRelease( s_aStatics[nVar][1] )
           //Alert( [Released static: ] + s_aStatics[nVar] + [ after ] + aProc[1] )
           //TraceLog( [Released static: ] + s_aStatics[nVar][1] + [ after ] + aProc[1], s_aStatics[nVar][1] )
        NEXT

        s_aStatics := NIL
     ENDIF

     /* Releasing Privates created by the Procedure */
     nVars := Len( s_asPrivates )
     FOR nVar := 1 TO nVars
        __MXRelease( s_asPrivates[nVar] )
        //Alert( [Released private: ] + s_asPrivates[nVar] + [ in ] + aProcStack[1] )
     NEXT
     aSize( s_asPrivates, 0 )

     /* Releasing Locals created by the Procedure */
     nVars := Len( s_asLocals )
     FOR nVar := 1 TO nVars
        __MXRelease( s_asLocals[nVar] )
        //Alert( [Released local: ] + s_asLocals[nVar] + [ in ] + aProcStack[1] )
     NEXT
     aSize( s_asLocals, 0 )

     s_nProcStack--
     aSize( s_aProcStack, s_nProcStack )

     IF s_nProcStack > 0
        aProcStack := s_aProcStack[s_nProcStack]
        s_aProc := aPresetProc

        IF Len( aPresetProc ) > 2
           s_aStatics := aPresetProc[3]

           /* Reinstating Outer Statics. */
           s_aStatics := aPresetProc[3]
           nVars := Len( s_aStatics )

           FOR nVar := 1 TO nVars
              #ifdef __HARBOUR__
                 __QQPub( s_aStatics[nVar][1] )
                 __MVPut( s_aStatics[nVar][1], s_aStatics[nVar][2] )
                 //Alert( [ReInstated static: ] + s_aStatics[nVar][1] + [ for ] + aProc[1] )
                 //TraceLog( [ReInstated static: ] + s_aStatics[nVar][1] + [ for ] + aProc[1], s_aStatics[nVar][2] )
              #else
                 __QQPub( s_aStatics[nVar][1] )
                 &( s_aStatics[nVar][1] ) := s_aStatics[nVar][2]
              #endif
           NEXT
        ELSE
           s_aStatics := NIL
        ENDIF

        /* Restoring Privates of parrent. */
        nVars := Len( aProcStack[3] )

        FOR nVar := 1 TO nVars
           aAdd( s_asPrivates, aProcStack[3][nVar][1] )
           #ifdef __HARBOUR__
              __QQPub( aProcStack[3][nVar][1] )
              __MVPut( aProcStack[3][nVar][1], aProcStack[3][nVar][2] )
           #else
              __QQPub( aProcStack[3][nVar][1] )
              &( aProcStack[3][nVar][1] ) := aProcStack[3][nVar][2]
           #endif
        NEXT

        /* Restoring Locals of parrent. */
        nVars := Len( aProcStack[4] )
        FOR nVar := 1 TO nVars
           aAdd( s_asLocals, aProcStack[4][nVar][1] )

           #ifdef __HARBOUR__
              __QQPub( aProcStack[4][nVar][1] )
              __MVPut( aProcStack[4][nVar][1], aProcStack[4][nVar][2] )
           #else
              __QQPub( aProcStack[4][nVar][1] )
              &( aProcStack[4][nVar][1] ) := aProcStack[4][nVar][2]
           #endif
        NEXT

        aSize( aProcStack, 2 )
     ELSE
        s_aProc := NIL
     ENDIF

  RETURN

  //--------------------------------------------------------------//

#endif

//--------------------------------------------------------------//

FUNCTION PP_ProcName( nLevel )

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   //TraceLog( s_nProcStack, nLevel )

   IF nLevel >= 0 .AND. nLevel < s_nProcStack
      RETURN s_aProcStack[ s_nProcStack - nLevel ][1]
   ENDIF

RETURN ""

//--------------------------------------------------------------//

FUNCTION PP_ProcLine( nLevel )

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   IF nLevel >= 0 .AND. nLevel < s_nProcStack
      RETURN s_aProcStack[ s_nProcStack - nLevel ][2]
   ENDIF

RETURN 0

//--------------------------------------------------------------//

PROCEDURE RP_Dot()

   LOCAL GetList := {}, sLine := Space(256)

   LOCAL aCpyDefRules, aCpyDefResults
   LOCAL aCpyCommRules, aCpyCommResults
   LOCAL aCpyTranRules, aCpyTranResults

   LOCAL aKBCommands := Array( 16 ), nKBCommand := 1, nTemp, bKey5, bKey24

   LOCAL bErrHandler
   //LOCAL oErr

   #ifdef FW
       Alert( [DOT mode (no filename parameter) is Not ready for GUI yet.] + [;;Please try Interpreter mode, using the -R switch...] )
       RETURN
   #endif

   IF bLoadRules
      bLoadRules := .F.
      PP_InitStd()
   ENDIF

   bCount := .F.

   IF File( "rp_dot.ch" )
      PP_PreProFile( "rp_dot.ch" )
   ELSE
      PP_LoadDot()
   ENDIF

   #ifdef WIN
      PP_PreProLine( '#COMMAND Alert( <x> ) => MessageBox( 0, CStr( <x> ), "TInterpreter for Windows", 0 )' )
   #endif

   #ifndef DONT_CLONE_RULES
      aCpyDefRules     := aClone( aDefRules )
      aCpyDefResults   := aClone( aDefResults )

      aCpyCommRules    := aClone( aCommRules )
      aCpyCommResults  := aClone( aCommResults )

      aCpyTranRules   := aClone( aTransRules )
      aCpyTranResults := aClone( aTransResults )
   #endif

   bErrHandler := ErrorBlock( {|oErr| RP_Dot_Err( oErr ) } )

   CLEAR SCREEN
   SET SCOREBOARD OFF

   @ 0,0 SAY "PP: "
   @ 0,4 SAY Space( 76 ) COLOR "N/R"
   @ 1,0 SAY PadR( "RDD: " + Space( 6 ) + " | Area: " + Space( 2 ) + " | Dbf: " + Space( 10 ) + ;
             " | Index: " + Space( 8 ) + " | # " + Space( 7 ) + "/" + Space( 7 ), MaxCol() + 1 )  ;
              COLOR "N/BG"

   DevPos( 02, 00 )

   aFill( aKBCommands, sLine )

   bCompile := .F.

   DO WHILE .T.
      sLine := aKBCommands[ nKBCommand ]

      @ MaxRow(), 00 SAY '.'
      @ MaxRow(), 01 GET sLine PICTURE '@KS79'
      SET CURSOR ON

      bKey5  := SetKey(  5, { || IIF( nKBCommand >  1, sLine := aKBCommands[ --nKBCommand ], ) } )
      bKey24 := SetKey( 24, { || IIF( nKBCommand < 16, sLine := aKBCommands[ ++nKBCommand ], ) } )

      READ

      SetKey(  5, bKey5  )
      SetKey( 24, bKey24 )

      IF ! sLine == aKBCommands[ nKBCommand ]
         IF ( nTemp := aScan( aKBCommands, sLine ) ) == 0
            aKBCommands[ nKBCommand ] := sLine
         ENDIF
      ENDIF

      IF LastKey() == 27
         aKBCommands[ nKBCommand ] := Space( 256 )
         LOOP
      ELSEIF LastKey() == 13 .OR. LastKey() == 24 .OR. LastKey() == 9
         nKBCommand++
         IF nKBCommand > 16
            aDel( aKBCommands, 1 )
            aKBCommands[16] := Space( 256 )
            nKBCommand := 16
         ENDIF
      ELSEIF LastKey() == 5 .OR. LastKey() == 271
         nKBCommand--
      ENDIF

      sLine := StrTran( sLine,  Chr(9), " " )

      Scroll( MaxRow(), 0, MaxRow(), MaxCol(), 1 )

      BEGIN SEQUENCE
         ExecuteLine( PP_PreProLine( RTrim( sLine ), 1, '' ) )
      //RECOVER USING oErr
      END SEQUENCE

      //TraceLog( Len( aDefRules ), Len( aCommRules ), Len( aTransRules ) )

      IF s_nRow >= MaxRow()
         Scroll( 2, 0, MaxRow(), MaxCol(), 1 )
         s_nRow := MaxRow() - 1
      ENDIF

      #ifndef DONT_CLONE_RULES
         IF s_lRunLoaded
            aDefRules     := aClone( aCpyDefRules )
            aDefResults   := aClone( aCpyDefResults )

            aCommRules    := aClone( aCpyCommRules )
            aCommResults  := aClone( aCpyCommResults )

            aTransRules   := aClone( aCpyTranRules )
            aTransResults := aClone( aCpyTranResults )

            s_lRunLoaded := .F.
            s_lClsLoaded := .F.
            s_lFWLoaded  := .F.
         ENDIF
      #endif

   ENDDO

   CLEAR SCREEN

   ErrorBlock( bErrHandler )

RETURN

//--------------------------------------------------------------//

STATIC PROCEDURE ExecuteLine( sPPed )

   LOCAL nNext, sBlock, sTemp
   LOCAL sTemp2, nLen, sLeft, sSymbol, nNextAssign

   ExtractLeadingWS( @sPPed )
   DropTrailingWS( @sPPed )
   sTemp := sPPed

   @ 0,0 SAY "PP: "
   @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"
   DevPos( s_nRow, s_nCol )

   BEGIN SEQUENCE

      WHILE ! Empty( sTemp )
         nNext := AtSkipStrings( ';', sTemp )

         IF nNext > 0
            sBlock := Left( sTemp, nNext - 1 )
            sTemp  := AllTrim( SubStr( sTemp, nNext + 1 ) )
         ELSE
            sBlock := sTemp
            sTemp := ""
         ENDIF

         sBlock := ParseAssign( AllTRim( sBlock ) )
         #ifdef USE_C_BOOST
            SetArrayPrefix( .F. )
         #else
            s_bArrayPrefix := .F.
         #endif

         sTemp2 := sBlock
         WHILE ( nNextAssign := At( ":=", sTemp2 ) ) > 0
            sLeft  := Left( sTemp2, nNextAssign - 1 )
            sTemp2 := SubStr( sTemp2, nNextAssign + 2 )

            DropTrailingWS( @sLeft )
            nLen := Len( sLeft )
            WHILE nLen > 0
               IF SubStr( sLeft, nLen, 1 ) $ " (,=><*+-\^%&@["
                  EXIT
               ENDIF
               nLen--
            ENDDO
            IF nLen == 0
               sSymbol := sLeft
            ELSE
               sSymbol := SubStr( sLeft, nLen + 1 )
            ENDIF
            IF Type( sSymbol ) == 'U'
               __QQPub( sSymbol )
            ENDIF
         ENDDO

         IF sBlock == "__"
            sSymbol := Upper( SubStr( sBlock, 3, 12 ) ) // Len( "SetOtherwise" )
         ELSE
            sSymbol := ""
         ENDIF

         IF nIf == 0 .OR. ;
            sSymbol = "SETIF" .OR. sSymbol = "SETELSE" .OR. sSymbol = "SETELSEIF" .OR. sSymbol = "SETEND" .OR. ;
            sSymbol = "SETDOCASE" .OR. sSymbol = "SETCASE" .OR. sSymbol = "SETOTHERWISE" .OR. sSymbol = "SETENDCASE" .OR. ;
            abIf[ nIf ]

            @ 0,0 SAY "PP: "
            @ 0,4 SAY Pad( sBlock, 76 ) COLOR "N/R"
            DevPos( s_nRow, s_nCol )

            sBlock := "{|| " + sBlock + " }"
            #ifdef __CLIPPER__
               /* Clipper Macro Compiler can't compile nested blocks! */
               CompileNestedBlocks( sBlock, @sBlock )
            #endif

            Eval( &sBlock )

            s_nRow := Row()
            s_nCol := Col()

            #ifdef __CLIPPER__
               nBlockID := 0
               aSize( s_abBlocks, 0 )
            #endif
         ENDIF
      ENDDO

      s_nRow := Row()
      s_nCol := Col()

      @ 0,0 SAY "PP: "
      @ 0,4 SAY Pad( sPPed, 76 ) COLOR "N/R"

      IF Empty( Alias() )
         @ 1,0 SAY PadR( "RDD: " + Space( 6 ) + " | Area: " + Space( 2 ) + " | Dbf: " + Space( 10 ) + ;
             " | Index: " + Space( 8 ) + " | # " + Space( 7 ) + "/" + Space( 7 ), MaxCol() + 1 )  ;
              COLOR "N/BG"
      ELSE
         //@ 1,0 CLEAR TO 1, MaxCol()
         @ 1,0 SAY PadR( "RDD: " + RddName() + " | Area: " + Str( Select(), 2 ) + " | Dbf: " + PadR( Alias(), 10 ) + ;
                         " | Index: " + PadR( OrdName( IndexOrd() ), 8 ) + " | # " + Str( RecNo(), 7 ) + ;
                         "/"  + Str( RecCount(), 7 ), MaxCol() + 1 );
                   COLOR "N/BG"
      ENDIF
   END SEQUENCE

   #ifdef __CLIPPER__
      nBlockID := 0
      aSize( s_abBlocks, 0 )
   #endif

RETURN

//--------------------------------------------------------------//

FUNCTION PP_CompileLine( sPPed, nLine, aProcedures, aInitExit, nProcId )

   LOCAL nNext, sBlock, sTemp
   LOCAL Dummy
   LOCAL oError

   ExtractLeadingWS( @sPPed )
   DropTrailingWS( @sPPed )

   //TraceLog( s_sModule, sPPed, nLine )

   BEGIN SEQUENCE

      IF sPPed = "_HB_CLASS"
         BREAK
      ENDIF

      sTemp := sPPed
      WHILE ( nNext := AtSkipStrings( ';', sTemp ) ) > 0 .OR. ! Empty( sTemp )
         IF nNext > 0
            sBlock := Left( sTemp, nNext - 1 )

            sTemp  := RTrim( SubStr( sTemp, nNext + 1 ) )
            ExtractLeadingWS( @sTemp )
         ELSE
            sBlock := sTemp
            sTemp  := ""
         ENDIF

         //TraceLog( sBlock )

         IF ! Empty( sBlock )
            sBlock := AllTrim( sBlock )

            IF sBlock = "#line"
               LOOP
            ENDIF

            #ifdef __CLIPPER__
                /* Clipper Macro Compiler can't compile nested blocks! */
                CompileNestedBlocks( sBlock, @sBlock )
            #endif

            IF ( nProcId == 0 .AND. sBlock = "PP__Statics" )
               Dummy := &( sBlock )
               LOOP
            ENDIF

            #ifdef __CONCILE_PCODE__
               ParseLinePCode( @sBlock, @nLine, @nProcID, @aProcedures, @aInitExit )
            #else
               ParseLine( @sBlock, @nLine, @nProcID, @aProcedures, @aInitExit )
            #endif

         ENDIF

      ENDDO

   RECOVER USING oError

      IF( oError:ClassName == "ERROR" )
         TraceLog( oError:SubSystem, oError:Operation, oError:Description, oError:Args, sBlock )

         IF oError:Operation == '&'
            oError:SubSystem := "PP"
            oError:Operation := "Parse"
            oError:Description := "Syntax Error;Script line: " + Str( nLine, 5 )

            #ifdef __XHARBOUR__
               oError:Description += ";Engine line: " + Str( oError:ProcLine, 5 )

               oError:Args := { sBlock }

               oError:ProcLine := nLine
               IF nProcID > 0
                  oError:ProcName := aProcedures[ nProcID ]
               ENDIF
               oError:ModuleName := s_sFile
            #endif
         ELSEIF oError:SubSystem == "PP"
            oError:Description += ";Script line: " + Str( nLine, 5 )

            #ifdef __XHARBOUR__
               oError:Description += ";Engine line: " + Str( oError:ProcLine, 5 )

               oError:ProcLine := nLine
               IF nProcID > 0
                  oError:ProcName := aProcedures[ nProcID ]
               ENDIF
               oError:ModuleName := s_sFile
            #endif
         ELSE
            #ifdef __XHARBOUR__
               oError:Description += ";Engine line: " + oError:ProcName + "(" + Str( oError:ProcLine, 5 ) + ")"
            #endif
         ENDIF

         Break( oError )
      ELSE
         TraceLog( "UNEXPEXTED CASE!", oError )
      ENDIF

   END SEQUENCE

RETURN aProcedures

//--------------------------------------------------------------//

FUNCTION ParseAssign( sBlock )

   LOCAL cID
   LOCAL cLValue := ""
   LOCAL cOp     := ""
   LOCAL nAt
   LOCAL cPad

   //TraceLog( sBlock )

   WHILE ! Empty( cID := NextToken( @sBlock ) )
      IF IsAlpha( cID ) .OR. Left( cID, 1 ) == '_'
         cLValue += cID
         cOp := DropTrailingWS( NextToken( @sBlock ), @cPad )

         IF cOp == '='
            RETURN cLValue + ':=' + cPad + sBlock
         ELSEIF cOp == "->"
            cLValue += ( cOp + cPad )
         ELSE
            EXIT
         ENDIF
      ELSE
         RETURN cLValue + cID + sBlock
      ENDIF
   END

   WHILE cOp == "[" .OR. cOp == ':'
      IF cOp == '['
         nAt := AtSkipStrings( ']', sBlock )

         IF nAt > 0
            cOp += ( cPad + Left( sBlock, nAt ) )
            sBlock := SubStr( sBlock, nAt + 1 )
            cLValue += cOp

            #ifdef USE_C_BOOST
               SetArrayPrefix( .T. )
            #else
               s_bArrayPrefix := .T.
            #endif

            cOp := DropTrailingWS( NextToken( @sBlock ), @cPad )
         ELSE
            RETURN cLValue + cOp + cPad + sBlock
         ENDIF
      ELSE
         cLValue += ( cOp + cPad )
         cOp := DropTrailingWS( NextToken( @sBlock ), @cPad )
      ENDIF
   END

   IF cOp == '='
      RETURN cLValue + ':=' + cPad + sBlock
   ENDIF

RETURN cLValue + cOp + cPad + sBlock

//--------------------------------------------------------------//

FUNCTION PP_Run( cFile, aParams, sPPOExt, bBlanks )

   LOCAL nBaseProc := s_nProcId, sPresetModule := s_sModule
   LOCAL bErrorHandler
   LOCAL aProcedures
   LOCAL x

   IF bBlanks == NIL
      bBlanks := .T.
   ENDIF

   IF ! s_lRunLoaded
      s_lRunLoaded := .T.

      IF File( "rp_run.ch" )
         PP_PreProFile( "rp_run.ch" )
      ELSE
         InitRunRules()
         InitRunResults()

         IF Len( aDefRules ) != Len( aDefResults )
            Eval( ErrorBlock(), ErrorNew( [PP], 0, 9001, [Rules], [#DEFINE Rules size mismatch], { aDefRules, aDefResults } ) )
            // Safety
            BREAK
         ENDIF

         IF Len( aTransRules ) != Len( aTransResults )
            Eval( ErrorBlock(), ErrorNew( [PP], 0, 9001, [Rules], [#TRANSLATE Rules size mismatch], { aTransRules, aTransResults } ) )
            // Safety
            BREAK
         ENDIF

         IF Len( aCommRules ) != Len( aCommResults )
            Eval( ErrorBlock(), ErrorNew( [PP], 0, 9001, [Rules], [#COMMAND Rules size mismatch], { aCommRules, aCommResults } ) )
            // Safety
            BREAK
         ENDIF
      ENDIF
   ENDIF

   //TraceLog( cFile, s_sModule, s_aProcedures, s_aInitExit, s_nProcId, aParams )

   //IF s_sModule == cFile
      //TraceLog( s_aProcedures, s_aInitExit, s_nProcId, aParams )
   //ELSE
      s_nProcId := 0; s_aProcedures := {}; s_aInitExit := { {}, {} }
      s_asPrivates := {}; s_asPublics := {}; s_asLocals := {}; s_aStatics := NIL; s_aParams := {}

      s_sModule := cFile
      bCompile  := .T.
      bErrorHandler := ErrorBlock( {|e| Break(e) } )

      BEGIN SEQUENCE
         PP_PreProFile( cFile, sPPOExt, bBlanks )
      RECOVER USING x
         //IF x:ClassName == "ERROR"
         //   Eval( s_bRTEBlock, x )
         //ELSE
            Break( x )
         //ENDIF
      END SEQUENCE

      ErrorBlock( bErrorHandler )
      bCompile  := .F.
   //ENDIF

   //UGLY hack.
   aProcedures := s_aProcedures
   s_aProcedures := NIL

   s_xRet := PP_Exec( aProcedures, s_aInitExit, s_nProcId, aParams )

   s_sModule := sPresetModule

RETURN s_xRet

//--------------------------------------------------------------//

PROCEDURE PP__Return( xRet )

   s_xRet := xRet

   //TraceLog( xRet )

   s_lReturnRequested := .T.
   BREAK .T.

//RETURN

//--------------------------------------------------------------//

PROCEDURE RP_Dot_Err( oErr )

   LOCAL Counter, xArg, sArgs := ";"

   IF HB_ISARRAY( oErr:Args ) .AND. Len( oErr:Args ) > 0
      sArgs := ";Arguments: "

      FOR Counter := 1 TO Len( oErr:Args )
         xArg := oErr:Args[Counter]

         SWITCH ValType( xArg )
            CASE "U"
               sArgs += "NIL; "
               exit
            CASE "A"
               sArgs += "{}; "
               exit
            CASE "B"
               sArgs += "{|| }; "
               exit
            CASE "C"
               sArgs += '"' + xArg + '"; '
               exit
            CASE "D"
               sArgs +=  dtoc( xArg ) + "; "
               exit
            CASE "L"
               sArgs += IIF( xArg, ".T.; ", ".F.; " )
               exit
            CASE "N"
               sArgs +=  Str( xArg ) + "; "
               exit
            CASE "O"
               sArgs +=  "{o}"
               exit
            DEFAULT
               sArgs +=  '[' + ValType( xArg ) + "]; "
               exit
         END SWITCH
      NEXT
   ENDIF

   Alert( [Sorry, could not execute: ] + oErr:Description + sArgs + " " + ProcName(2) + '[' + Str( ProcLine(2) ) + ']')

   BREAK

//RETURN // Unreacable code

//--------------------------------------------------------------//

FUNCTION RP_Run_Err( oErr, aProcedures )

   LOCAL nProc, sProc
   LOCAL oRecover, lSuccess

   #ifdef __XHARBOUR__
      //TraceLog( s_anRecover, oErr:Description, oErr:Operation, oErr:ProcName, oErr:ProcLine, ValToPrg( oErr:Args ) )
   #else
      //TraceLog( oErr:Description, oErr:Operation )
   #endif

   oRecover := oErr

   IF oErr:SubCode == 1001 .AND. oErr:SubSystem == "BASE"
      IF s_sModule != NIL
         sProc := s_sModule + oErr:Operation
         nProc := aScan( aProcedures, {|aProc| aProc[1] == sProc } )
      ELSE
         nProc := 0
      ENDIF

      IF nProc == 0
         sProc := oErr:Operation
         nProc := aScan( aProcedures, {|aProc| aProc[1] == sProc } )
      ENDIF

      IF nProc > 0
         lSuccess := .T.

         BEGIN SEQUENCE
            //TraceLog( sProc )

            #ifdef __CONCILE_PCODE__
               SWITCH Len( oErr:Args )
                  CASE 0
                     s_xRet := &( aProcedures[nProc][1] )()

                  CASE 1
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1] )

                  CASE 2
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2] )

                  CASE 3
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3] )

                  CASE 4
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4] )

                  CASE 5
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4], oErr:Args[5] )

                  CASE 6
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4], oErr:Args[5], oErr:Args[6] )

                  CASE 7
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4], oErr:Args[5], oErr:Args[6], oErr:Args[7] )

                  CASE 8
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4], oErr:Args[5], oErr:Args[6], oErr:Args[7], oErr:Args[8] )

                  CASE 9
                     s_xRet := &( aProcedures[nProc][1] )( oErr:Args[1], oErr:Args[2], oErr:Args[3], oErr:Args[4], oErr:Args[5], oErr:Args[6], oErr:Args[7], oErr:Args[8], oErr:Args[9] )

                  DEFAULT
                     BREAK oErr
               END
            #else
               s_xRet := PP_ExecProcedure( aProcedures, nProc, oErr:Args )
            #endif

         RECOVER USING oRecover
            lSuccess := .F.
            IF oRecover:ClassName == "ERROR"
               oRecover:Cargo := oErr
            ELSE
               // Request to Break again
               Break( oRecover )
            ENDIF
         END

         IF lSuccess
            RETURN s_xRet
         ENDIF
      ELSE
        IF s_bExternalRecovery != NIL
           //TraceLog( "Resolve: " + oRecover:Operation )
           RETURN Eval( s_bExternalRecovery, oRecover )
        ENDIF
      ENDIF
   ENDIF

   #ifdef __XHARBOUR__
      oRecover:ProcName   := PP_ProcName()
      oRecover:ProcLine   := PP_ProcLine()
      oRecover:ModuleName := s_sFile
   #endif

   // Script Error within a TRY block.
   IF s_lTrying
      //Alert( "Break within Script TRY!" )
      Break( oRecover )
   ENDIF

   BEGIN SEQUENCE
      s_xRet := Eval( s_bRTEBlock, oRecover )
      //Alert( "Script Error returned:" + ValType( s_xRet ) )

      IF ! HB_ISLOGICAL( s_xRet ) .AND. ( ! oRecover:CanSubStitute )
         Alert( "SCRIPT - Error Recovery Failure!" )
         Break( oRecover )
      ENDIF
   RECOVER
      //Alert( "Recovered after RTE Handler!" )
      Break( oRecover )
   END

RETURN s_xRet

//--------------------------------------------------------------//

FUNCTION __SetIf( bExp )

   IF nIf > 0 .AND. ! abIf[nIf]
      bExp := .F.
   ENDIF

   nIf++
   aSize( abIf, nIf )
   abIf[nIf] := bExp

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetElseIf( bExp )

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

   IF abIf[nIf]
      abIf[nIf] := bExp
   ENDIF

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetElse()

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetEnd()

   IF nIf > 0
      nIf--
   ELSE
      Eval( s_bRTEBlock, ErrorNew( [PP], 0, 9002, [Flow], [END with no IF in sight!] ) )
      // Safety
      BREAK
   ENDIF

RETURN nIf

//--------------------------------------------------------------//

FUNCTION __SetDoCase()

   nIf++
   aSize( abIf, nIf )
   abIf[nIf] := .F.

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetCase( bExp )

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

   IF abIf[nIf]
      abIf[nIf] := bExp
   ENDIF

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetOtherwise()

   IF nIf > 1 .AND.  ! abIf[nIf - 1]
      RETURN .F.
   ENDIF

   abIf[nIf] := ! abIf[nIf]

RETURN abIf[nIf]

//--------------------------------------------------------------//

FUNCTION __SetEndCase()

   IF nIf > 0
      nIf--
   ELSE
      Eval( s_bRTEBlock, ErrorNew( [PP], 0, 9002, [Flow], [ENDCAE with no DOCASE in sight!] ) )
      // Safety
      BREAK
   ENDIF

RETURN nIf

//--------------------------------------------------------------//

#ifdef __CLIPPER__

   //--------------------------------------------------------------//

   STATIC FUNCTION CompileNestedBlocks( sTemp, sMain )

      LOCAL asBlocks, nBlocks, Counter, aReplace

      asBlocks := asBlocks(sTemp )
      nBlocks  := Len( asBlocks )

      FOR Counter := 1 TO nBlocks
         aReplace := CompileNestedBlocks( SubStr( asBlocks[Counter], 2 ), @sMain )
      NEXT

      IF ProcName(1) == ProcName(0) // .AND. nBlocks == 0
         IF aReplace != NIL
            sTemp := StrTran( sTemp, aReplace[1], aReplace[2] )
         ELSE
            aReplace := Array(2)
         ENDIF

         aReplace[1] :=  '{' + sTemp
         aReplace[2] := "PP_Block(" + LTrim( Str( ++nBlockId, 3, 0 ) ) + ')'
         aAdd( s_abBlocks, &( aReplace[1]) )

         sMain := StrTran( sMain, aReplace[1], aReplace[2] )

         RETURN aReplace
      ENDIF

   RETURN NIL

   //--------------------------------------------------------------//

   FUNCTION asBlocks( sBlock, asBlocks )

      LOCAL nStart := 1, nEnd := Len( sBlock ), nPosition, sNested, nOpen, ;
            lBlock := .F., cChar

      IF asBlocks == NIL
         asBlocks := {}
      ENDIF

      WHILE ( nStart := AtSkipStrings( '{', sBlock, nStart ) ) > 0
         FOR nPosition := nStart + 1 TO nEnd
            IF SubStr( sBlock, nPosition, 1 ) != ' '
               EXIT
            ENDIF
         NEXT
         IF SubStr( sBlock, nPosition, 1 ) != '|'
            nStart++
            LOOP
         ENDIF

         nPosition++
         nOpen := 1

         DO WHILE nOpen > 0 .AND. nPosition <= nEnd
            cChar := SubStr( sBlock, nPosition, 1 )

            IF cChar == '"'
               DO WHILE nPosition <= nEnd
                  nPosition++
                  IF SubStr( sBlock, nPosition, 1 ) == '"'
                     EXIT
                  ENDIF
               ENDDO
            ELSEIF cChar == "'"
               DO WHILE nPosition <= nEnd
                  nPosition++
                  IF SubStr( sBlock, nPosition, 1 ) == "'"
                     EXIT
                  ENDIF
               ENDDO
            ELSEIF cChar == '{'
               nOpen++
            ELSEIF cChar == '}'
               nOpen--
            ENDIF

            nPosition++
         ENDDO

         sNested := SubStr( sBlock, nStart, ( nPosition - nStart ) )
         //TraceLog( asBlocks, sNested )

         aAdd( asBlocks, sNested )
         asBlocks( SubStr( sBlock, nPosition + 1 ), asBlocks )

         nStart := nPosition
      ENDDO

   RETURN asBlocks

   //--------------------------------------------------------------//

   FUNCTION PP_Block( nId )

   RETURN s_abBlocks[nId]

   //--------------------------------------------------------------//
#endif

//------------------------------- *** END - RP DOT Functions *** -------------------------------//

FUNCTION PP_PreProFile( sSource, sPPOExt, bBlanks, bDirectivesOnly, aPendingLines )

   LOCAL hSource, sBuffer, sLine, nPosition, sExt, cPrev
   LOCAL nLen, nMaxPos, cChar := '', nClose, nNext, nLine := 0
   LOCAL sRight, nPath := 0, nPaths := Len( s_asPaths ), nNewLine
   LOCAL sPath := "", oError, sPrevFile := s_sFile
   LOCAL sTmp
   LOCAL lMaintainPending

   s_bRTEBlock := s_bDefRTEBlock

   #ifdef __HARBOUR__
      IF Empty( s_sAppPath )
         s_sAppPath := HB_ArgV(0)
         s_sAppPath := Left( s_sAppPath, RAt( OS_PATH_DELIMITER, s_sAppPath ) )
         aAdd( s_asPaths, s_sAppPath )
         nPaths++
      ENDIF
   #endif

   IF At( '.', sSource ) == 0
     sSource += ".prg"
   ENDIF

   IF aPendingLines == NIL
      lMaintainPending := .F.
   ELSE
      lMaintainPending := .T.
   ENDIF

   s_sFile := sSource

   hSource := FOpen( sSource, 64 )
   IF hSource == -1
      nPath := 1
      WHILE hSource == -1 .AND. nPath <= nPaths
          hSource := FOpen( s_asPaths[nPath] + sSource, 64 )
          nPath++
      ENDDO
   ENDIF

   IF hSource == -1
      Break( ErrorNew( [PP], 0, 3007, sSource, [ERROR! opening: ] + Str( FError(), 2 ), { sSource } ) )
   ENDIF

   IF nPath > 1
      sPath := s_asPaths[ nPath - 1 ]
   ENDIF

   IF hPP == NIL .AND. aPendingLines == NIL
      IF bBlanks == NIL
         bBlanks := .T.
      ENDIF

      IF sPPOExt == NIL
         // *** Intentionally invalid handle - saves us from performing an IF on each of the FWrite( hPP, ... )
         hPP := -1
      ELSE
         sExt := SubStr( sSource, RAt( '.', sSource ) )
         IF ! ( sExt == '' )
            hPP := FCreate( StrTran( sSource, sExt, sPPOExt ) )
         ELSE
            hPP := FCreate( sSource + sPPOExt )
         ENDIF
         IF hPP == -1
            Break( ErrorNew( [PP], 0, 3006, sSource + sPPOExt, [Can't create preprocessed output file: ] + Str( FError(), 2 ), { sSource, sPPOExt } ) )
         ENDIF
      ENDIF
   ELSE
      sLine := '#line 1 "' + sPath + Upper( sSource ) + '"'
      FWrite( hPP, sLine + CRLF )
      IF lMaintainPending
         aAdd( aPendingLines, sLine )
      ENDIF
      bBlanks := .F.
   ENDIF

   IF bDirectivesOnly == NIL
      bDirectivesOnly := .F.
   ENDIF

   sBuffer   := Space( PP_BUFFER_SIZE )
   sLine     := ''

   BEGIN SEQUENCE

      WHILE ( nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE ) ) > 2
         sBuffer := Left( sBuffer, nLen )
         nPosition := 1
         nMaxPos   := nLen - 1

         //TraceLog( "***", nLen, nMaxPos, Left( sBuffer, nLen ) )

         WHILE nPosition < nMaxPos .OR. SubStr( sBuffer, nPosition, 1 ) == Chr(10)

             cPrev := cChar
             cChar := SubStr( sBuffer, nPosition, 1 )

             //TraceLog( nPosition, nMaxPos, Left( sBuffer, nLen ), cChar, cPrev )

             DO CASE
                CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '*' )
                   nPosition++

                   sTmp := NIL
                   WHILE .T.
                      nClose := At( "*/", sBuffer, nPosition + 1 )

                      IF nClose == 0
                         nNext := nPosition + 1
                         WHILE ( nNext := At( Chr(10), sBuffer, nNext ) ) > 0
                            nLine++
                            IF bCount
                               @ Row(), 0 SAY nLine
                            ENDIF

                            IF bBlanks
                               FWrite( hPP, CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, "" )
                               ENDIF
                            ENDIF

                            nNext++
                            nPosition := nNext
                         ENDDO

                         sTmp := SubStr( sBuffer, nPosition )
                         nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         IF nLen < 2
                            Break( ErrorNew( [PP], 0, 2083, [Pre-Process], [Unterminated /* */ comment], { sLine, nLine } ) )
                         ENDIF

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nNext := nPosition + 1
                         nClose -= nPosition

                         WHILE ( nNext := At( Chr(10), sBuffer, nNext ) ) > 0 .AND. ( nNext - nPosition ) <= nClose + 1
                            nLine++
                            IF bCount
                               @ Row(), 0 SAY nLine
                            ENDIF
                            IF bBlanks
                               FWrite( hPP, CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, "" )
                               ENDIF
                            ENDIF
                            nNext++
                         ENDDO

                         nPosition += ( nClose + 1 )
                         cChar := ''
                         EXIT
                      ENDIF
                   ENDDO

                CASE ( cChar == '/' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '/' )
                   nPosition++
                   WHILE .T.
                      nClose := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nClose == 0
                         sTmp := SubStr( sBuffer, nPosition )
                         nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         IF nLen < 2
                            BREAK "//"
                         ENDIF

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose -= nPosition
                         nLine++

                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF

                         DropTrailingWS( @sLine, @sRight )

                         IF Right( sLine, 1 ) == ';'
                            nLen  := Len( sLine )
                            sLine := /*DropTrailingWS(*/ Left( sLine, nLen - 1 )/*, @sRight )*/

                            IF bBlanks
                               FWrite( hPP, CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, "" )
                               ENDIF
                            ENDIF

                            /* Right after the NL */
                            nPosition += ( nClose + 1 )

                            /* Skip leading spaces in continued next line. */
                            WHILE SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                               nPosition++
                            ENDDO
                            nPosition--

                            cChar := ' '
                            EXIT
                         ELSE
                            IF LTrim( sLine ) == ''
                               IF bBlanks
                                  FWrite( hPP, CRLF )
                                  IF lMaintainPending
                                     aAdd( aPendingLines, "" )
                                  ENDIF
                               ENDIF
                            ELSE
                               IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                                  sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                                  IF bBlanks .OR. ! ( sLine == '' )
                                     FWrite( hPP, sLine + CRLF )
                                     IF lMaintainPending
                                        aAdd( aPendingLines, sLine )
                                     ENDIF
                                  ENDIF
                               ENDIF
                            ENDIF

                            nPosition += nClose
                            sLine := ''
                            cChar := ''

                            EXIT
                         ENDIF
                      ENDIF
                   ENDDO

                CASE ( cChar == '&' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '&' )
                   nPosition++
                   WHILE .T.
                      nClose := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nClose == 0
                         sTmp := SubStr( sBuffer, nPosition )
                         nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         IF nLen < 2
                            BREAK "&&"
                         ENDIF

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose -= nPosition
                         nLine++

                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF

                         IF LTrim( sLine ) == ''
                            IF bBlanks
                               FWrite( hPP, CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, "" )
                               ENDIF
                            ENDIF
                         ELSE
                            IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                               sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                               IF bBlanks .OR. ! ( sLine == '' )
                                  FWrite( hPP, sLine + CRLF )
                                  IF lMaintainPending
                                     aAdd( aPendingLines, sLine )
                                  ENDIF
                               ENDIF
                            ENDIF
                         ENDIF

                         nPosition += nClose
                         sLine := ''
                         cChar := ''

                         EXIT
                      ENDIF
                   ENDDO

                CASE ( cChar == '*' )
                   IF LTrim( sLine ) == ''
                      sTmp := NIL
                      WHILE .T.
                         nClose := At( Chr(10), sBuffer, nPosition + 1 )

                         IF nClose == 0
                            sTmp := SubStr( sBuffer, nPosition + 1 )
                            nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                            sBuffer := Left( sBuffer, nLen )

                            IF nLen < 2
                               BREAK "*"
                            ENDIF

                            sBuffer   := sTmp + sBuffer
                            nLen      += Len( sTmp )
                            nMaxPos   := nLen - 1
                            nPosition := Len( sTmp )

                            LOOP
                         ELSE
                            nClose -= nPosition
                            nLine++

                            IF bCount
                               @ Row(), 0 SAY nLine
                            ENDIF

                            IF bBlanks
                               FWrite( hPP, CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, "" )
                               ENDIF
                            ENDIF

                            /*
                            IF sTmp == NIL
                               sLine += SubStr( sBuffer, nPosition, nClose - 1 )
                            ELSE
                               sLine += Left( sBuffer, nClose - 1 )
                            ENDIF
                            TraceLog( "Dropped: '" + sLine + "'" )
                            */

                            nPosition += nClose
                            sLine := ''
                            cChar := ''

                            EXIT
                         ENDIF
                      ENDDO
                   ENDIF

             #ifdef __PLATFORM__UNIX
                CASE ( nLine == 0 .AND. nPosition == 1 .AND. cChar == '#' .AND. SubStr( sBuffer, nPosition + 1, 1 ) == '!' )
                   WHILE .T.
                      nClose := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nClose == 0
                         sTmp := SubStr( sBuffer, nPosition )
                         nLen := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         IF nLen < 2
                            BREAK "#!"
                         ENDIF

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose -= nPosition
                         nLine++

                         IF bCount
                            @ Row(), 0 SAY nLine
                         ENDIF

                         IF bBlanks
                            FWrite( hPP, CRLF )
                            IF lMaintainPending
                               aAdd( aPendingLines, "" )
                            ENDIF
                         ENDIF

                         nPosition += nClose
                         sLine := ''
                         cChar := ''

                         EXIT
                      ENDIF
                   ENDDO
             #endif

              CASE ( cChar == '"' )
                   sTmp := NIL
                   WHILE .T.
                      nClose := At( '"', sBuffer, nPosition + 1 )
                      nNewLine := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                         EXIT
                      ENDIF

                      IF nClose == 0
                         sTmp      := SubStr( sBuffer, nPosition )
                         nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer   := Left( sBuffer, nLen )

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose -= nPosition
                         IF sTmp == NIL
                            sLine += SubStr( sBuffer, nPosition, nClose )
                         ELSE
                            sLine += Left( sBuffer, nClose )
                         ENDIF
                         nPosition += nClose

                         EXIT
                      ENDIF
                   ENDDO

              CASE ( cChar == "'" )
                   sTmp := NIL
                   WHILE .T.
                      nClose   := At( "'", sBuffer, nPosition + 1 )
                      nNewLine := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                         EXIT
                      ENDIF

                      IF nClose == 0
                         sTmp      := SubStr( sBuffer, nPosition )
                         nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose    -= nPosition
                         IF sTmp == NIL
                            sLine += SubStr( sBuffer, nPosition, nClose )
                         ELSE
                            sLine += Left( sBuffer, nClose )
                         ENDIF
                         nPosition += nClose

                         EXIT
                      ENDIF
                   ENDDO

                CASE ( cChar == '[' )
                   IF LTrim( sLine ) = "#" .OR. ( IsAlpha( cPrev ) .OR. IsDigit( cPrev ) .OR. cPrev $ "])}._" )
                      sLine += cChar
                      nPosition++
                      LOOP
                   ENDIF

                   sTmp := NIL
                   WHILE .T.
                      nClose   := At( ']', sBuffer, nPosition + 1 )
                      nNewLine := At( Chr(10), sBuffer, nPosition + 1 )

                      IF nNewLine > 0 .AND. ( nClose == 0 .OR. nClose > nNewLine )
                         EXIT
                      ENDIF

                      IF nClose == 0
                         sTmp      := SubStr( sBuffer, nPosition )
                         nLen      := FRead( hSource, @sBuffer, PP_BUFFER_SIZE )
                         sBuffer := Left( sBuffer, nLen )

                         sBuffer   := sTmp + sBuffer
                         nLen      += Len( sTmp )
                         nMaxPos   := nLen - 1
                         nPosition := Len( sTmp )

                         LOOP
                      ELSE
                         nClose -= nPosition
                         IF sTmp == NIL
                            sLine += SubStr( sBuffer, nPosition, nClose )
                         ELSE
                            sLine += Left( sBuffer, nClose )
                         ENDIF
                         nPosition += nClose

                         EXIT
                      ENDIF
                   ENDDO

                   IF nClose > 0 .AND. nClose < nNewLine
                      cChar := ']'
                   ENDIF

                CASE cChar == Chr(9)
                   sLine += "    "
                   cChar := ''

                CASE cChar == Chr(10)
                   DropTrailingWS( @sLine, @sRight )

                   nLine++
                   IF bCount
                      @ Row(), 0 SAY nLine
                   ENDIF

                   IF Right( sLine, 1 ) == ';'
                      nLen  := Len( sLine )
                      sLine := /*DropTrailingWS(*/ Left( sLine, nLen - 1 )/*, @sRight )*/

                      IF bBlanks
                         FWrite( hPP, CRLF )
                         IF lMaintainPending
                            aAdd( aPendingLines, "" )
                         ENDIF
                      ENDIF

                      /* Skip leading spaces in continued next line. */
                      nPosition++
                      IF SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                          // White space if found in continued lines is replaced with a single space and rest is ignored!
                          sLine += ' '
                          nPosition++

                         WHILE SubStr( sBuffer, nPosition, 1 ) $ ' ' + Chr(9)
                            nPosition++
                         ENDDO
                      ENDIF

                      LOOP
                   ELSE
                      IF LTrim( sLine ) == ''
                         IF bBlanks
                            FWrite( hPP, CRLF )
                            IF lMaintainPending
                               aAdd( aPendingLines, "" )
                            ENDIF
                         ENDIF
                      ELSE
                         //sLine += sRight
                         IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                            sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                            IF bBlanks .OR. ! ( sLine == '' )
                               FWrite( hPP, sLine + CRLF )
                               IF lMaintainPending
                                  aAdd( aPendingLines, sLine )
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDIF

                      sLine := ''
                      cChar := ''
                   ENDIF

                CASE cChar == Chr(13)
                   nPosition++
                   LOOP

                CASE cChar == Chr(26)
                   nLine++
                   IF bCount
                      @ Row(), 0 SAY nLine
                   ENDIF
                   IF LTrim( sLine ) == ''
                      IF bBlanks
                         FWrite( hPP, CRLF )
                         IF lMaintainPending
                            aAdd( aPendingLines, "" )
                         ENDIF
                      ENDIF
                   ELSE
                      IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
                         sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
                         IF bBlanks .OR. ! ( sLine == '' )
                            FWrite( hPP, sLine + CRLF )
                            IF lMaintainPending
                               aAdd( aPendingLines, sLine )
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDIF
                   sLine := ''
                   cChar := ''
             ENDCASE

             sLine += cChar
             nPosition++

         ENDDO

         FSeek( hSource, -2 + ( nPosition - nMaxPos ), 1 )

      ENDDO

      //? "Closing: " + sSource

      FClose( hSource )

      //? Asc( cChar ), "'" + sLine + "'"
      //? sSource, nPosition, nMaxPos, nLen, SubStr( sLine, nPosition, 40 )
      //WAIT

      IF nPosition == NIL
         sLine := sBuffer
      ELSE
         sLine += SubStr( sBuffer, nPosition, Max( 0, ( nMaxPos + 2 ) - nPosition ) )
      ENDIF

      sLine := StrTran( sLine, Chr(09), "   " )
      DropTrailingWS( @sLine )

      sLine := StrTran( sLine, Chr(10), '' )
      sLine := StrTran( sLine, Chr(13), '' )
      sLine := StrTran( sLine, Chr(26), '' )

      /*
      ? '=>"' + sLine + '"<=', Asc( Right( sLine, 1 ) ), Asc( Left( sLine, 1 ) )
      FOR Counter := 1 TO Len( RTrim( Ltrim( sLine ) ) )
         ? Asc( SubStr( sLine, Counter, 1 ) )
      NEXT
      WAIT
      */

      //? "Finished: " + sSource

      nLine++
      IF bCount
         @ Row(), 0 SAY nLine
      ENDIF

      IF LTrim( sLine ) == ''
         IF bBlanks
            FWrite( hPP, sLine )
            IF lMaintainPending
               aAdd( aPendingLines, sLine )
            ENDIF
         ENDIF
      ELSE
         IF bDirectivesOnly == .F. .OR. Left( sLine, 1 ) == '#'
            sLine := PP_PreProLine( sLine, nLine, sPath + sSource )
            IF bBlanks .OR. ! ( sLine == '' )
               FWrite( hPP, sLine + CRLF )
               IF lMaintainPending
                  aAdd( aPendingLines, sLine )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ProcName(1) == "PP_MAIN" .OR. ProcName(1) == "PP_RUN"
         FClose( hPP )

         #ifdef __CONCILE_PCODE__
            ConcileProcedures( s_aProcedures )
         #else
           #ifdef DYN
               PP_GenDynProcedures( s_aProcedures )
           #endif
         #endif

         IF bCCH
            CompileToCCH( sSource )
         ENDIF
      ENDIF

      //? "Done: " + sSource
      //WAIT

      IF ! Empty( sPrevFile )
         s_sFile := sPrevFile
      ENDIF

   RECOVER USING oError

      IF HB_ISSTRING( oError )
         //TraceLog( "No EOL after: ", cError )
         //Alert( [No EOL after: ] + cError )
         oError := ErrorNew( [PP], 0, 1002, [Pre-Process], [Missing EOL], { sLine, oError } )
      ENDIF

      IF HB_ISOBJECT( oError )
         //oError:Description += ";Script line: " + Str( nLine ) + ";Source: " + sLine

         Eval( s_bRTEBlock, oError )
         // Safety
         BREAK
      ELSE
         TraceLog( "UNEXPECTED CASE!", oError )
      ENDIF

      nPosition := nMaxPos + 2
      sLine := ""
   END SEQUENCE

   //TraceLog( ValToPrg( aPendingLines ) )

RETURN .T.

//--------------------------------------------------------------//

STATIC PROCEDURE RecomposeRuleLine( sLine, aPendingLines, nPendingLines )

   #ifdef __XHARBOUR__
      LOCAL sSubLine, nNextLine, sSubSubLine

      FOR EACH sSubLine IN aPendingLines
         IF sSubLine != NIL
            nNextLine := 0

            sSubSubLine := LTrim( sSubLine )

            IF sSubSubLine[1] == '#'
               //IF Upper( RTrim( NextToken( LTrim( SubStr( sSubSubLine, 2 ) ) ) ) ) == "PROCESS"
                  //TraceLog( sLine )
                  RETURN
               //ENDIF
            ENDIF

            WHILE ( nNextLine := AtSkipStrings( ';', sSubLine, nNextLine + 1, .T. ) ) > 0
               sSubSubLine := LTrim( SubStr( sSubLine, nNextLine + 1 ) )
               //TraceLog( sLine, sSubLine, sSubsubLine, nNextLine )

               IF sSubSubLine[1] == '#'
                  //IF Upper( RTrim( NextToken( LTrim( SubStr( sSubSubLine, 2 ) ) ) ) ) == "PROCESS"
                     IF Len( sLine ) == 0
                        sLine := Left( sSubLine, nNextLine )
                     ELSE
                        sLine += '; ' + Left( sSubLine, nNextLine )
                     ENDIF

                     // We consumed left already - FOR EACH makes it a BYREF!
                     sSubLine := SubStr( sSubLine, nNextLine + 1 )

                     //TraceLog( sLine )
                     RETURN
                  //ENDIF
               ENDIF
            ENDDO

            sSubSubLine := LTrim( sSubLine )

            IF sSubSubLine[1] == '#'
               //IF Upper( RTrim( NextToken( LTrim( SubStr( sSubSubLine, 2 ) ) ) ) ) == "PROCESS"
                  RETURN
               //ENDIF
            ENDIF

            IF Len( sLine ) == 0
               sLine := sSubLine
            ELSE
               sLine += '; ' + sSubLine
            ENDIF

            nPendingLines--

            // FOR EACH makes it a BYREF!
            sSubLine := NIL
         ELSE
            RETURN
         ENDIF
      NEXT

   #else

      LOCAL nPendLine, sSubLine, nNextLine, sSubSubLine

      FOR nPendLine := 1 TO nPendingLines
         sSubLine := aPendingLines[ nPendLine ]

         IF sSubLine != NIL
            nNextLine := 0

            WHILE ( nNextLine := AtSkipStrings( ';', sSubLine, nNextLine + 1, .T. ) ) > 0
               sSubSubLine := LTrim( SubStr( sSubLine, nNextLine + 1 ) )

               IF sSubSubLine[1] == '#'
                  //IF Upper( RTrim( NextToken( LTrim( SubStr( sSubSubLine, 2 ) ) ) ) ) == "PROCESS"
                     IF Len( sLine ) == 0
                        sLine := Left( sSubLine, nNextLine - 1 )
                     ELSE
                        sLine += '; ' + Left( sSubLine, nNextLine - 1 )
                     ENDIF

                     // We consumed left already
                     aPendingLines[ nPendLine ] := SubStr( sSubLine, nNextLine + 1 )

                     RETURN
                  //ENDIF
               ENDIF
            ENDDO

            IF Len( sLine ) == 0
               sLine := aPendingLines[ nPendLine ]
            ELSE
               sLine += '; ' + aPendingLines[ nPendLine ]
            ENDIF

            nPendingLines--
            aPendingLines[ nPendLine ] := NIL
         ELSE
            EXIT
         ENDIF
      NEXT
   #endif

   //TraceLog( sLine )

RETURN

//--------------------------------------------------------------//

STATIC PROCEDURE DeferPendingLines( sLine, aPendingLines, nPendingLines )

   LOCAL nNewLineAt

   IF ( nNewLineAt := AtSkipStrings( ';', sLine ) ) > 0
      nPendingLines++

      IF nPendingLines > Len( aPendingLines )
         aSize( aPendingLines, nPendingLines )
      ENDIF

      aIns( aPendingLines, 1 )
      aPendingLines[ 1 ] := SubStr( sLine, nNewLineAt + 1 )
      sLine := Left( sLine, nNewLineAt - 1 )
   ENDIF

RETURN

//--------------------------------------------------------------//

FUNCTION PP_PreProLine( sLine, nLine, sSource )

   LOCAL nPendingLines := 0, aPendingLines  := {}

   LOCAL sDirective, bX, sToken, nRule
   LOCAL nNewLineAt, nLines, Counter
   LOCAL sLeft, sPassed, asOutLines := {}, sOut := ''
   LOCAL nLen, nCycles := 0, aDefined := {}, aTranslated := {}, aCommanded := {}
   //LOCAL nIdAt, sRight
   LOCAL oError
   LOCAL sBackupLine
   LOCAL sSkipped
   LOCAL bArrayPrefix
   LOCAL bPresetCompile
   LOCAL sRight, nAt, sConstant

   //TraceLog( sLine, nLine )

   s_bRTEBlock := s_bDefRTEBlock

   IF Left( LTrim( sLine ), 1 ) != '#'
      IF ( nNewLineAt := AtSkipStrings( ';', sLine ) ) > 0
         nPendingLines := 1
         aPendingLines := { SubStr( sLine, nNewLineAt  + 1 ) }

         //TraceLog( "Pending #", nPendingLines,  Left( sLine, nNewLineAt - 1 ), aPendingLines[ 1 ] )
         sLine := Left( sLine, nNewLineAt - 1 )
      ENDIF
   ENDIF

   BEGIN SEQUENCE

      WHILE .T.
         //? "Processing: '" + sLine + "'"
         //? nPendingLines, nIfDef, IIF( nIfDef > 0, abIfDef[nIfDef] , )
         //WAIT

         IF nCycles < MAX_CICLES
            nCycles++
         ELSE
            //TraceLog( "Circularity!", sLine )
            //Alert( [ERROR! Circularity detected ]+"[" + sSource + "(" + LTrim( Str( nLine ) ) + ")]" )
            //? sLine
            Break( ErrorNew( [PP], 0, 2083, [Pre-Process], [Ciruclarity Detected], { sLine, nLine, sSource } ) )
         ENDIF

         IF sLine == NIL
            sLine := ''
            sLeft := ''
         ELSE
            sLeft := ExtractLeadingWS( @sLine )
         ENDIF

         IF sLine == ''
            IF nPendingLines > 0
               sLine := aPendingLines[1]
               aDel( aPendingLines, 1 )
               nPendingLines--

               DeferPendingLines( @sLine, aPendingLines, @nPendingLines )

               LOOP
            ENDIF

            EXIT
         ENDIF

         IF s_sIncludeFile != NIL
            IF ! Empty( sSource )
               aAdd( asOutLines, "#line " + LTrim( Str( nLine ) ) )
            ENDIF
            s_sIncludeFile := NIL
         ENDIF

         //TraceLog( "Processing: '" + sLine +"'" )
         //? "Processing: '" + sLine +"'"
         //WAIT

         IF Left( sLine, 1 ) == '#'

            sLine := LTrim( SubStr( sLine, 2 ) )
            sDirective := RTrim( Upper( NextToken( @sLine ) ) )

            IF ( nLen := Len( sDirective ) ) < 4 .AND. sDirective != "IF"
               Break( ErrorNew( [PP], 0, 3010, [Pre-Process], [Unknown directive: ] + sDirective, { sLine, nLine, sSource } ) )
            ENDIF

            IF sDirective == "IF" .AND. nIfDef > 0 .AND. ! abIfDef[ nIfDef ]

               nIfDef++
               aSize( abIfDef, nIfDef )
               abIfDef[ nIfDef ] := .F.

               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "IFDEF", nLen ) .AND. nIfDef > 0 .AND. ! abIfDef[ nIfDef ]

               nIfDef++
               aSize( abIfDef, nIfDef )
               abIfDef[ nIfDef ] := .F.

               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "IFNDEF", nLen ) .AND. nIfDef > 0 .AND. ! abIfDef[ nIfDef ]

               nIfDef++
               aSize( abIfDef, nIfDef )
               abIfDef[ nIfDef ] := .F.

               sLine := ''
               LOOP

            ELSEIF sDirective == "ELSE" .AND. nIfDef > 1 .AND. ! abIfDef[ nIfDef - 1 ]

               sLine := ''
               LOOP

            ELSEIF sDirective == "ELSE"

               abIfDef[ nIfDef ] := ! abIfDef[ nIfDef ]

               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "ENDIF", nLen )

               IF nIfDef > 0
                  nIfDef--
               ELSE
                  Break( ErrorNew( [PP], 0, 2069, [Pre-Process], [#endif with no #ifdef in sight], { sDirective, nLine, sSource } ) )
               ENDIF

               sLine := ''
               LOOP

            ELSEIF sDirective == "LINE"

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               aAdd( asOutLines, "#line " + sLine )
               sLine := ''
               LOOP

            ENDIF

            IF nIfDef > 0 .AND. ! abIfDef[nIfDef]
               //? "Ignored: " + sLine
               sLine := ''
               LOOP
            ENDIF

            ExtractLeadingWS( @sLine )
            //TraceLog( sLine )

            #ifdef OUTPUT_DIRECTIVES
               aAdd( asOutLines, "/* #" + sDirective + " " + sLine + " */" )
            #endif

            IF sDirective == Left( "DEFINE", nLen )

               RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines, .T. )

               CompileDefine( sLine )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "ERROR", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               Break( ErrorNew( [PP], 0, 2069, [#error], sLine, { sLine, nLine, sSource } ) )

            ELSEIF sDirective == Left( "PROCESS", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               // Force early processing of #process directives!
               PP_PreProLine( sLine, nLine, sSource )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "UNDEF", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               RemoveDefine( sLine )
               sLine := ''
               LOOP

            ELSEIF sDirective == "IF"

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               sConstant := AllTrim( sLine )
               sConstant := StrTran( sConstant, ".T.", "1" )
               sConstant := StrTran( sConstant, ".F.", "0" )

               sConstant := StrTran( sConstant, ".AND.", "!= 0 .AND. 0 !=" )
               sConstant := StrTran( sConstant, ".OR.", "!= 0 .OR. 0 !=" )

               sConstant := StrTran( sConstant, "&&", "!= 0 .AND. 0 !=" )
               sConstant := StrTran( sConstant, "||", "!= 0 .OR. 0 !=" )

               WHILE ( nAt := At( "defined", sConstant ) ) > 0
                  sLeft := Left( sConstant, nAt - 1 )
                  sRight := LTrim( SubStr( sConstant, nAt + 7 ) )

                  IF Left( sRight, 1 ) == '('
                     sRight := LTrim( SubStr( sRight, 2 ) )
                     sToken := NextToken( @sRight )
                     sRight := LTrim( sRight )

                     IF Left( sRight, 1 ) == ')'
                        sRight := SubStr( sRight, 2 )
                        sConstant := sLeft + IIF( aScan( aDefRules, {|aDefine| aDefine[1] == sToken } ) > 0, "1", "0" ) + sRight
                     ELSE
                        Break( ErrorNew( [PP], 0, 26, [Pre-Processing], [Invalid constant expression], { sLine, nLine, sSource } ) )
                     ENDIF
                  ELSE
                     Break( ErrorNew( [PP], 0, 26, [Pre-Processing], [Invalid constant expression], { sLine, nLine, sSource } ) )
                  ENDIF
               END

               IF Type( sConstant ) $ "NL"
                  //TraceLog( sConstant, &(sConstant) )
                  nIfDef++
                  aSize( abIfDef, nIfDef )
                  abIfDef[nIfDef] := ! Empty( &( sConstant ) )
               ELSE
                  //TraceLog( sConstant )
                  Break( ErrorNew( [PP], 0, 26, [Pre-Processing], [Invalid constant expression], { sLine, nLine, sSource } ) )
               ENDIF

               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "IFDEF", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               SetIfDef( sLine, .T. )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "IFNDEF", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               SetIfDef( sLine, .F. )
               sLine := ''
               LOOP

            ELSEIF sDirective == Left( "INCLUDE", nLen )

               //RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

               ExtractLeadingWS( @sLine )
               DropTrailingWS( @sLine )

               // Added Oct-16-2004 to support #defines and translates.
               bPresetCompile := bCompile
               bCompile := .F.
               sLine := PP_PreProLine( sLine, nLine, sSource )
               bCompile := bPresetCompile

               // Strip the ""
               sLine := SubStr( sLine, 2, Len( sLine ) - 2 )

               IF Upper( sLine ) == "RP_RUN.CH" .AND. s_lRunLoaded
                  // already loaded.
               ELSEIF Upper( sLine ) == "HBCLASS.CH" .AND. ! ( sSource == "xbsclass.ch" )
                  IF ! s_lClsLoaded
                     s_lClsLoaded := .T.
                     InitClsRules()
                     InitClsResults()
                     IF Len( aDefRules ) != Len( aDefResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #DEFINE Rules size mismatch], { aDefRules, aDefResults } ) )
                     ENDIF
                     IF Len( aTransRules ) != Len( aTransResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #TRANSLATE Rules size mismatch], { aTransRules, aTransResults } ) )
                     ENDIF
                     IF Len( aCommRules ) != Len( aCommResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #COMMAND Rules size mismatch], { aCommRules, aCommResults } ) )
                     ENDIF
                  ENDIF

             #ifdef FW
               ELSEIF Upper( sLine ) == "FIVEWIN.CH"
                  IF ! s_lFWLoaded
                     s_lFWLoaded := .T.
                     IF ! s_lClsLoaded
                        s_lClsLoaded := .T.
                        InitClsRules()
                        InitClsResults()
                        IF Len( aDefRules ) != Len( aDefResults )
                           Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #DEFINE Rules size mismatch], { aDefRules, aDefResults } ) )
                        ENDIF
                        IF Len( aTransRules ) != Len( aTransResults )
                           Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #TRANSLATE Rules size mismatch], { aTransRules, aTransResults } ) )
                        ENDIF
                        IF Len( aCommRules ) != Len( aCommResults )
                           Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [CLASS #COMMAND Rules size mismatch], { aCommRules, aCommResults } ) )
                        ENDIF
                     ENDIF

                     InitFWRules()
                     InitFWResults()

                     IF Len( aDefRules ) != Len( aDefResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [FW #DEFINE Rules size mismatch], { aDefRules, aDefResults } ) )
                     ENDIF
                     IF Len( aTransRules ) != Len( aTransResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [FW #TRANSLATE Rules size mismatch], { aTransRules, aTransResults } ) )
                     ENDIF
                     IF Len( aCommRules ) != Len( aCommResults )
                        Break( ErrorNew( [PP], 0, 1003, [Pre-Processing], [FW #COMMAND Rules size mismatch], { aCommRules, aCommResults } ) )
                     ENDIF
                  ENDIF
             #endif

               ELSE
                  IF bCompile .OR. ( hPP != NIL .AND. hPP > 0 )
                     PP_PreProFile( sLine ) // Intentionally not using s_sIncludeFile
                  ELSE
                     PP_PreProFile( sLine, NIL, .T., .F., aPendingLines ) // Intentionally not using s_sIncludeFile
                     aAdd( aPendingLines, "#line " + LTrim( Str( nLine ) ) + " " + sSource )
                     nPendingLines := Len( aPendingLines )
                  ENDIF

                  /* Recursion safety - don't use the Static might be modified. */
                  s_sIncludeFile := sLine
               ENDIF

               //TraceLog( sLine )
               sLine := ''
               LOOP

            ELSE

               IF Left( sDirective, 1 ) == 'X'
                  bX := .T.
                  sDirective := SubStr( sDirective, 2 )
                  nLen--
               ELSE
                  bX := .F.
               ENDIF

               IF sDirective == Left( 'TRANSLATE', nLen )

                  RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

                  CompileRule( sLine, aTransRules, aTransResults, bX, .F. )
                  sLine := ''
                  LOOP

               ELSEIF sDirective == Left( 'COMMAND', nLen )

                  RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

                  CompileRule( sLine, aCommRules, aCommResults, bX, .F. )
                  sLine := ''
                  LOOP

               ELSEIF sDirective == Left( 'UNTRANSLATE', nLen )

                  RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

                  CompileRule( sLine, aTransRules, aTransResults, bX, .T. )
                  sLine := ''
                  LOOP

               ELSEIF sDirective == Left( 'UNCOMMAND', nLen )

                  RecomposeRuleLine( @sLine, aPendingLines, @nPendingLines )

                  CompileRule( sLine, aCommRules, aCommResults, bX, .T. )
                  sLine := ''
                  LOOP

               ELSE

                  Break( ErrorNew( [PP], 0, 3010, [Pre-Process], [Unknown directive: ] + sDirective, { sLine, nLine, sSource } ) )

               ENDIF

            ENDIF

         ENDIF

         #ifdef PP_RECURSIVE
            s_bRecursive := .T.
         #endif

         BEGIN SEQUENCE

            IF nIfDef > 0 .AND. ! abIfDef[nIfDef]
               //? "Ignored: " + sLine
               sLine := ''
               BREAK
            ENDIF

            //TraceLog( sLine )
            s_sPending := ""
            aEval( aPendingLines, {|s| IIF( s != NIL, s_sPending += '; ' + s, ) } )

            sBackupLine := sLine
            sPassed     := ""
            DO WHILE ( sToken := NextIdentifier( @sLine, @sSkipped ) ) != NIL
               //? "Token = '"  + sToken + "'"
               //WAIT

               sPassed += sSkipped

               // Save incase MatchRule fails.
               #ifdef USE_C_BOOST
                  bArrayPrefix := GetArrayPrefix()
               #else
                  bArrayPrefix := s_bArrayPrefix
               #endif

               IF ( nRule := MatchRule( sToken, @sLine, aDefRules, aDefResults, .F., .F. ) ) > 0
                  //? "DEFINED: " + sLine
                  //WAIT

                  aAdd( aDefined, nRule )

                  DeferPendingLines( @sLine, aPendingLines, @nPendingLines )
                  sLine := sLeft + sPassed + sLine

                  // Re-Reprocess the line ...
                  BREAK
               ELSE
                  // Restore since MatchRule() faild.
                  #ifdef USE_C_BOOST
                     SetArrayPrefix( bArrayPrefix )
                  #else
                     s_bArrayPrefix := bArrayPrefix
                  #endif
               ENDIF

               sPassed += sToken
            ENDDO

            // Now process Translates...
            //? "After Defines:", sLine

            sLine := sBackupLine

            // Reset at new line.
            #ifdef USE_C_BOOST
               SetArrayPrefix( .F. )
            #else
               s_bArrayPrefix := .F.
            #endif

            sPassed := ""
            DO WHILE ( sToken := NextToken( @sLine ) ) != NIL
               //? "Token = '"  + sToken + "'"
               //WAIT

               // Save incase MatchRule fails.
               #ifdef USE_C_BOOST
                  bArrayPrefix := GetArrayPrefix()
               #else
                  bArrayPrefix := s_bArrayPrefix
               #endif

               IF ( nRule := MatchRule( sToken, @sLine, aTransRules, aTransResults, .F., .T. ) ) > 0
                  //? "TRANSLATED: " + sLine
                  //WAIT

                  IF sPassed == "" .AND. aScan( aTranslated, nRule ) > 64
                     Break( ErrorNew( "PP", 0, 2079, [Pre-Process], [Ciruclarity Detected], { sLine, nLine, sSource, sToken } ) )
                  ELSE
                     aAdd( aTranslated, nRule )
                  ENDIF

                  IF Empty( s_sPending )
                     // Because NextExp() might have CONSUMED it all upon an <**> Marker!
                     nPendingLines := 0
                     //aSize( aPendingLines, 0 )
                     aFill( aPendingLines, NIL )
                  ENDIF

                  DeferPendingLines( @sLine, aPendingLines, @nPendingLines )
                  sLine := sLeft + sPassed + sLine

                  BREAK
               ELSE
                  // Restore since MatchRule() faild.
                  #ifdef USE_C_BOOST
                     SetArrayPrefix( bArrayPrefix )
                  #else
                     s_bArrayPrefix := bArrayPrefix
                  #endif
               ENDIF

               sPassed += sToken
            ENDDO

            sLine := sPassed //sBackupLine

            sToken := NextToken( @sLine )

            IF sToken != NIL .AND. ( nRule := MatchRule( sToken, @sLine, aCommRules, aCommResults, .T., .T. ) ) > 0
               //? "COMMANDED: " + sLine
               //? '"' + sLeft +'"', '"' + sPassed + '"'
               //WAIT

               /*
               IF aScan( aCommanded, nRule ) > 0
                  Alert( [Cyclic directive: #command ] + sToken )
               BREAK
               ELSE
                  aAdd( aCommanded, nRule )
               ENDIF
               */

               IF Empty( s_sPending )
                  // Because NextExp() might have CONSUMED it all upon an <**> Marker!
                  nPendingLines := 0
                  //aSize( aPendingLines, 0 )
                  aFill( aPendingLines, NIL )
               ENDIF

               DeferPendingLines( @sLine, aPendingLines, @nPendingLines )
               sLine := sLeft + sLine

               BREAK
            ENDIF

            aAdd( asOutLines, sLeft + sPassed )
            sLine := ""

         RECOVER USING oError

           IF HB_ISOBJECT( oError )
              //Alert( oError )
              Break( oError ) // We have another wrapper outside.
           ENDIF

           LOOP

         END SEQUENCE


      ENDDO

   RECOVER USING oError

     IF( oError:ClassName == "ERROR" )
        //TraceLog( nLine, oError:SubSystem, oError:Operation, oError:Description, oError:Args )

        IF oError:SubSystem == "PP"
           oError:Description += ";Script line: " + CStr( nLine )
           #ifdef __XHARBOUR__
              oError:Description += ";Engine line: " + CStr( oError:ProcLine )

              oError:ProcLine := nLine
              IF s_nProcID > 0
                 oError:ProcName := s_aProcedures[ s_nProcID ]
              ENDIF
              oError:ModuleName := s_sFile
           #endif
        ELSE
           #ifdef __XHARBOUR__
              oError:Description += ";Engine line: " + oError:ProcName + "(" + CStr( oError:ProcLine ) + ")"
           #endif
        ENDIF

        Break( oError )
     ELSE
        TraceLog( "UNEXPEXTED CASE!", oError )
     ENDIF

   END SEQUENCE

   #ifdef PP_RECURSIVE
      s_bRecursive := .F.
   #endif

   sOut   := ""
   nLines := Len( asOutLines )

   //? nLines
   //WAIT

   FOR Counter := 1 TO nLines
      //? Counter, asOutLines[Counter]
      //WAIT
      sOut += asOutLines[Counter]
      IF Counter < nLines
         sOut += ' ;'
      ENDIF
   NEXT

   /*
   IF ! Empty( sOut )
      ? "Returning: " + sOut
      WAIT
      TraceLog( sOut )
   ENDIF
   */

   IF bCompile
      PP_CompileLine( sOut, nLine, s_aProcedures, s_aInitExit, @s_nProcId )
   ENDIF

RETURN sOut

//--------------------------------------------------------------//

STATIC FUNCTION MatchRule( sKey, sLine, aRules, aResults, bStatement, bUpper )

   LOCAL Counter, nRules, nRule, aMarkers, xMarker
   LOCAL aMP, nOptional := 0, sAnchor, cType, aList, nMarkerId, nKeyLen
   LOCAL sToken, sWorkLine, sNextAnchor, nMatch, nMatches
   LOCAL sPad, asRevert := {}, bNext, sPreMatch, nLen
   LOCAL sPrimaryStopper, sPreStoppers, sStopper, sMultiStopper, nStopper, nStoppers
   LOCAL nSpaceAt, sStopLine, sNextStopper, nTemp
   LOCAL bRepeatableMatched
   LOCAL aaRevertMarkers := {}
   LOCAL nBackup, nLevel, bTestDependant
   LOCAL sPPO
   LOCAL aRule, aMatchers

   nRules   := Len( aRules )

   IF nRules == 0 .OR. sKey == NIL .OR. sKey == ""
      RETURN 0
   ENDIF

   nRule    := nRules + 1
   sPad     := ''

   DropTrailingWS( @sKey, @sPad )
   IF bUpper
      sKey  := Upper( sKey )
   ENDIF

   IF bDbgMatch
      ? "Matching Key: '" + sKey + "' Line: " + sLine
      WAIT
   ENDIF

   nKeyLen := Max( Len( sKey ), 4 )

   WHILE .T.

      nRule--

      FOR Counter := nRule TO 1 STEP -1
         IF aRules[Counter][3]
            IF aRules[ Counter ][1] == sKey
               EXIT
            ENDIF
         ELSE
            IF Left( aRules[ Counter ][1], nKeyLen ) == sKey
               EXIT
            ENDIF
         ENDIF
      NEXT

      IF Counter == 0
         IF bDbgMatch
            ? "No Prospects, returning: " + sLine
            WAIT
         ENDIF

         RETURN 0
      ELSE
         nRule := Counter
      ENDIF

      aRule     := aRules[nRule]
      aMatchers := aRule[2]
      sWorkLine := sLine

      IF bDbgMatch
         ? "KEY: " + sKey + " Matching Rule: " + Str( nRule, 3 ) + " with: " + sWorkLine
         WAIT
      ENDIF

      IF aMatchers == NIL
         nMatches := 0
      ELSE
         nMatches := Len( aMatchers )
      ENDIF

      IF nMatches == 0
         IF bStatement .AND. ! Empty( sWorkLine )
            IF bDbgMatch
               ? "***1 Unmatched remainder: >", sWorkLine, "<"
               ? "Statement failed"
               WAIT
            ENDIF

            LOOP
         ELSEIF bStatement
            sWorkLine := ""
         ENDIF

         sPPO := PPOut( aResults[nRule], aMarkers, aMatchers )

         IF bDbgMatch
            IF HB_ISSTRING( sLine )
               ? "TRANSLATED to:", sPPO
            ELSE
               ? "Output failed! Continue with next rule."
            ENDIF

            WAIT
         ENDIF

         IF HB_ISSTRING( sPPO )
            sLine := sPPO + sPad + sWorkLine
            RETURN nRule
         ELSE
            // Continue with next rule.
            LOOP
         ENDIF
      ENDIF

      aMarkers  := aResults[nRule][3]
      IF aMarkers != NIL
         aFill( aMarkers, NIL )
      ENDIF

      nMatch    := 1
      aMP       := aMatchers[1]
      nOptional := 0
      bNext     := .F.

      DO WHILE .T. //! ( sWorkLine == '' )

         aMP       := aMatchers[nMatch]

         nMarkerId := aMP[1]
         sAnchor   := aMP[3]
         cType     := aMP[4]
         aList     := aMP[5]

         /* Might be needed - added 5-27-2001 when debugging oddity in FW CheckBox rule ???
         IF aMP[2] == 0
            nOptional := 0
            aSize( asRevert, 0 )
         ENDIF
         */

         /* "Used" non repeatable! */
         IF nMarkerID > 0 .AND. nMarkerID < 1000
            IF aMarkers != NIL .AND. aMarkers[nMarkerID] != NIL
               IF bDbgMatch
                  ? "Used:", nMatch, nMarkerId, aMarkers[nMarkerId], nOptional, aMP[2]
                  WAIT
               ENDIF

               IF nOptional <> 0 .AND. aMP[2] < 0
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "* Reverted: " + asRevert[Abs(nOptional)]
                     WAIT
                  ENDIF
               ENDIF

               IF aMP[2] > 0 .AND. nMatch < nMatches
                  /* Skip all same level optionals to next group. */
                  nOptional := Abs( aMP[2] )
                  nMatch++
                  WHILE nMatch <= nMatches
                     aMP := aMatchers[nMatch]
                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= nOptional )
                        EXIT
                     ENDIF
                     nMatch++
                  ENDDO
                  IF bDbgMatch
                     ? "Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                  ENDIF
                  IF nMatch <= nMatches
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ELSE
                  IF nMatch < nMatches
                     nMatch++
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF nMarkerId >= 1000
            nMarkerId -= 1000
         ENDIF

         /* Do we have to look for a stopper? */
         IF cType != ':' .AND. sAnchor == NIL .AND. ValType( aList ) == 'A'

            sPreStoppers := sWorkLine
            sPrimaryStopper := NextToken( @sWorkLine )

            IF sPrimaryStopper == NIL

               //? "No primary", sPrimaryStopper
               sWorkLine := sPreStoppers

            ELSE
               sPrimaryStopper := Upper( RTrim( sPrimaryStopper ) )

               /* Is it a stopper (the anchor of another acceptable match) ? */
               IF bDbgMatch
                  ? "Stopper?: '" + sPrimaryStopper +"'"
               ENDIF

               nStoppers := Len( aList )
               FOR nStopper := 1 TO nStoppers

                  sStopLine := sWorkLine
                  sToken    := sPrimaryStopper
                  sStopper  := aList[ nStopper ]

                  sMultiStopper := ''
                  WHILE ( nSpaceAt := At( ' ', sStopper ) ) > 0
                     sNextStopper := Left( sStopper, nSpaceAt - 1 )

                     IF aRule[3]
                        nLen := 64
                     ELSE
                        nLen := Max( 4, Len( sToken ) )
                     ENDIF

                     //? "Next Stopper: " + sNextStopper, sToken
                     IF Left( sNextStopper, nLen ) == sToken
                        sMultiStopper += sNextStopper
                        sStopper      := SubStr( sStopper, nSpaceAt )
                        sMultiStopper += ExtractLeadingWS( @sStopper )
                        sToken        := NextToken( @sStopLine )
                        sToken        := Upper( RTrim( sToken ) )
                     ELSE
                        EXIT
                     ENDIF
                  ENDDO

                  IF aRule[3]
                     nLen := 64
                  ELSE
                     nLen := Max( 4, Len( sToken ) )
                  ENDIF

                  IF Left( sStopper, nLen ) == sToken
                     sMultiStopper += sStopper
                     EXIT
                  ENDIF
               NEXT

               IF nStopper <= nStoppers

                  IF bDbgMatch
                     ? "Found stopper: " + sMultiStopper
                  ENDIF

                  sWorkLine := sStopLine

                  /* Current level */
                  nOptional := Abs( aMP[2] )

                  /* Commented out 07-21-2001 Seems unneeded. */
                  #ifdef WHY_REWIND
                      /* Rewind to beging of same level and then search for the stopper match */
                      WHILE nMatch > 1
                         nMatch--
                         IF Abs( aMatchers[nMatch][2] ) < nOPtional
                            nMatch++
                            EXIT
                         ENDIF
                      ENDDO

                      // Added June-1-2003 (yes I know it's a commented section.)
                      IF nMatch == 1 .AND. aMatchers[nMatch][2] == 0
                         nMatch++
                      ENDIF
                  #endif

                  /* Now search for the stopper. */
                  WHILE nMatch < nMatches
                     nMatch++
                     aMP := aMatchers[nMatch]

                     IF aMP[3] == NIL .AND. aMP[4] == ':'
                      #ifdef __XHARBOUR__
                        IF aScan( aMP[5], sMultiStopper, , , .T. ) > 0
                      #else
                        IF aScan( aMP[5], {|sWord| sWord == sMultiStopper } ) > 0
                      #endif
                           EXIT
                        ENDIF
                     ELSE
                        IF aMP[2] >= 0 .AND. aMP[2] <= nOptional .AND. aMP[3] == sMultiStopper
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO

                  nOptional := 0
                  LOOP

               ELSE

                  sWorkLine     := sPreStoppers
                  sMultiStopper := NIL

                  IF bDbgMatch
                     ? sToken, "Not a stopper."
                     ? "Reverted: ", sWorkLine
                  ENDIF

               ENDIF

               IF bDbgMatch
                  WAIT
               ENDIF

            ENDIF

         ENDIF

         sNextAnchor := NIL
         nTemp       := 1
         WHILE nMatch + nTemp <= nMatches
            IF aRules[Counter][2][nMatch + nTemp][2] <= 0 // Non NEW Optional ONLY!
               sNextAnchor := aRules[Counter][2][nMatch + nTemp][3]
               EXIT
            ENDIF
            nTemp++
         ENDDO

         IF bDbgMatch
            IF sAnchor == NIL
               ? nMatch, 'of', nMatches, "NO Anchore!", nMarkerId, nOptional, aMP[2], sMultiStopper, sNextAnchor
            ELSE
               ? nMatch, 'of', nMatches, "Searching for Anchore: '" + sAnchor + "'", nMarkerId, nOptional, aMP[2], sMultiStopper, sNextAnchor
            ENDIF
            WAIT
         ENDIF

         sToken    := NIL
         xMarker   := NIL
         sPreMatch := sWorkLine

         IF ( sAnchor == NIL .OR. sMultiStopper != NIL .OR. ;
              ( ( ( sToken := NextToken( @sWorkLine ) ) != NIL  .AND. ( DropTrailingWS( @sToken, @sPad ), nLen := Max( 4, Len( sToken ) ), Upper( sToken ) == Left( sAnchor, nLen ) ) ) ) ) ;
            .AND. ( nMarkerId == 0 .OR. ( sAnchor == NIL .AND. sMultiStopper != NIL ) .OR. ( ( xMarker := NextExp( @sWorkLine, cType, aList, sNextAnchor, aRule[3] ) ) != NIL ) )

            IF sMultiStopper != NIL
               IF sAnchor == NIL
                  xMarker := sMultiStopper
               ELSE
                  sToken  := sMultiStopper
               ENDIF
               IF bDbgMatch
                  ? "Using MultiStopper: " + sMultiStopper
               ENDIF
               sMultiStopper := NIL
            ENDIF

            IF bDbgMatch
               ? "sKey =", sKey, "Anchor =", sAnchor, "nMarkerId =", nMarkerId, "sToken =", sToken,
               #ifdef __XHARBOUR__
                  ?? "xMarker =", ValToPrg( xMarker ), "<="
               #endif
            ENDIF

            IF HB_ISSTRING( xMarker )
               DropTrailingWS( @xMarker )
            ENDIF

            IF aMP[2] > 0 .AND. nOptional < 0

               nOptional := aMP[2]

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ELSEIF aMP[2] > 0 .AND. nOptional >= 0 .AND. aMP[2] >= nOptional

               nOptional := aMP[2]

               /* Save. */
               aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            // Group started with nested optional, this is the 1st element in current level.
            ELSEIF aMP[2] < 0 .AND. Len( asRevert ) >= - aMP[2] .AND. asRevert[ - aMP[2] ] == NIL

               nOptional := - aMP[2]

               /* Save. */
               //aSize( asRevert, nOptional )
               asRevert[nOptional] := sPreMatch
               //aSize( aaRevertMarkers, nOptional )
               aaRevertMarkers[nOptional] := aClone( aMarkers )

               IF bDbgMatch
                  ? "*** Saved: " + asRevert[nOptional]
                  WAIT
               ENDIF

            ENDIF

            IF nMarkerId > 0
               /* Repeatable. */
               IF aMP[1] > 1000
                  IF aMarkers[nMarkerId] == NIL
                     aMarkers[nMarkerId] := {}
                  ENDIF
                  aAdd( aMarkers[nMarkerId], xMarker )

                  IF bDbgMatch
                     ? nMarkerId, "Repetable added: ", xMarker, Len( aMarkers[nMarkerId] )
                  ENDIF
               ELSE
                  IF ValType( aMarkers ) != 'A' .OR. nMarkerId > Len( aMarkers )
                     //TraceLog( "Oops", nRule, sKey, nMarkerId, ValType( aMarkers ), IIF( ValType( aMarkers ) == 'A', Len( aMarkers ) , "No array" ) )
                     Eval( ErrorBlock(), ErrorNew( [PP], 0, 3010, [Pre-Process], [Unexpected case], { nRule, sKey, nMarkerId, aMarkers } ) )
                  ELSE
                     aMarkers[nMarkerId] := xMarker
                  ENDIF
               ENDIF
            ENDIF

            // *** This complete IF section added 2002-May-11, the bug this solved might be fixed otherwise ***
            // EOL - Rule will match if rest is OPTIONAL.
            IF Empty( sWorkLine )
               IF bDbgMatch
                  ? "End of Input."
               ENDIF

               // Remainder may be optional.
               nBackup := nMatch
               bTestDependant := .T.
               nTemp := Abs( aMP[2] )
               WHILE ++nMatch <= nMatches
                  nLevel := aMatchers[nMatch][2]

                  // Non Optional.
                  IF nLevel == 0
                     EXIT
                  ELSEIF nLevel > 0 .AND. nLevel <= nTemp
                     // Head of New Adjucent or Outer group - Adjust stop condition...
                     nTemp := nLevel
                     bTestDependant := .F.
                     LOOP
                  ELSEIF nLevel < 0 .AND. ( ( ( - nLevel ) < nTemp ) .OR. ( bTestDependant .AND. ( ( - nLevel ) == nTemp ) ) )
                     EXIT
                  ENDIF
               ENDDO

               IF bDbgMatch
                  ? "Skipped optionals to:", nMatch, "of:", nMatches
                  WAIT
               ENDIF

               IF nMatch > nMatches
                  //TraceLog( ValToPrg( aMatchers ), ValToPrg( aMarkers ) )
                  sPPO := PPOut( aResults[nRule], aMarkers, aMatchers )

                  IF bDbgMatch
                     IF HB_ISSTRING( sLine )
                        ? "Skipped optionals and TRANSLATED to:", sPPO
                     ELSE
                        ? "Output failed! Continue with next rule."
                     ENDIF

                     WAIT
                  ENDIF

                  IF HB_ISSTRING( sPPO )
                     sLine := sPPO + sPad + sWorkLine
                     RETURN nRule
                  ELSE
                     // Continue with next rule.
                     bNext := .T.
                     EXIT
                  ENDIF
               ELSE
                  // Must consider this match a failure because End of input but NOT end of rule - REVERT if OPTIONAL.
                  IF aMP[2] <> 0
                     nMatch := nBackup

                     /* Skip all same level optionals to next group. */
                     nTemp := Abs( aMP[2] )
                     WHILE nMatch < nMatches
                        nMatch++
                        aMP := aMatchers[nMatch]
                        IF ( aMP[2] < 0 ) .AND. ( Abs( aMP[2] ) < nTemp )
                           EXIT
                        ENDIF
                        IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= nTemp )
                           EXIT
                        ENDIF
                     ENDDO

                     IF bDbgMatch
                        ? "Skipped same level to:", nMatch
                     ENDIF

                     // Because will LOOP
                     //nMatch--

                     /* Revert. */
                     IF nOptional <> 0 /*.AND. aMP[2] < 0*/ .AND. asRevert[Abs(nOptional)] != NIL
                        sWorkLine := asRevert[Abs(nOptional)]
                        aMarkers  := aaRevertMarkers[Abs(nOptional)]

                        IF bDbgMatch
                           ? "* Reverted: " + asRevert[Abs(nOptional)]
                           WAIT
                        ENDIF
                     ELSE
                        sWorkLine := sPreMatch
                        IF bDbgMatch
                           ? "*** Reclaimed token/marker: " + sWorkLine
                           WAIT
                        ENDIF
                     ENDIF

                     LOOP
                  ELSE
                     IF bDbgMatch
                        ? "*** Match Failed - Not Revertable and Not End of Rule, but End of Input ***"
                        WAIT
                     ENDIF

                     bNext := .T.
                     EXIT
                  ENDIF

               ENDIF

            ENDIF

            IF aMP[2] <> 0
               IF bDbgMatch
                  ? "Optional"
               ENDIF

               /* We reached the end of current optional group - Rewind, to 1st optional at same level. */
               IF nMatch == nMatches .OR. ( aMatchers[nMatch + 1][2] >= 0 .AND. aMatchers[nMatch + 1][2] <= Abs( aMP[2] ) ) .OR. ;
                                          ( aMatchers[nMatch + 1][2] < 0 .AND. abs( aMatchers[nMatch + 1][2] ) < Abs( aMP[2] ) )

                  /* Current level */
                  nOptional := Abs( aMP[2] )

                  IF Len( asRevert ) >= nOptional
                     asRevert[ nOptional ] := NIL
                  ENDIF

                  IF nMatch > 1
                     // Now rewind.
                     WHILE nMatch > 1
                        nMatch--
                        IF Abs( aMatchers[nMatch][2] ) < nOPtional
                           nMatch++
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  // Added June-1-2003
                  IF nMatch == 1 .AND. aMatchers[nMatch][2] == 0
                     nMatch++
                  ENDIF

                  nOptional := 0

                  IF bDbgMatch
                     ? "Rewinded to:", nMatch
                  ENDIF

                  LOOP

               ENDIF

            ENDIF

            IF bDbgMatch
               WAIT
            ENDIF

            IF nMatch == nMatches
               IF bStatement .AND. ! Empty( sWorkLine )
                  bNext := .T.

                  IF bDbgMatch
                     ? "Key: >", sKey, "< ***2 Unmatched remainder: >", sWorkLine, "<"
                     ? "Statement failed, try next rule...'"
                     WAIT
                  ENDIF

                  sWorkLine := ""
                  EXIT
               ELSEIF bStatement
                  sWorkLine := ""
               ENDIF

               sPPO := PPOut( aResults[nRule], aMarkers, aMatchers )

               IF bDbgMatch
                  IF HB_ISSTRING( sLine )
                     ? "TRANSLATED to:", sPPO
                  ELSE
                     ? "Output failed! Continue with next rule."
                  ENDIF

                  WAIT
               ENDIF

               IF HB_ISSTRING( sPPO )
                  sLine := sPPO + sPad + sWorkLine
                  RETURN nRule
               ELSE
                  // Continue with next rule.
                  bNext := .T.
                  EXIT
               ENDIF
            ELSE
               IF bDbgMatch
                  ? "Accepted:", sToken, xMarker
               ENDIF

               nMatch++
               LOOP
            ENDIF

         ELSE /* Match failed. */

            IF bDbgMatch
               ? "NO MATCH:", nMatch, "of", nMatches, sAnchor, sToken, nMarkerId, xMarker, nOptional, aMP[2]
            ENDIF

            // Optional
            IF aMP[2] <> 0
               /* Revert. */
               IF nOptional <> 0 .AND. aMP[2] < 0 .AND. asRevert[Abs(nOptional)] != NIL
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "* Reverted: " + asRevert[Abs(nOptional)]
                     WAIT
                  ENDIF
               ELSE
                  sWorkLine := sPreMatch
                  IF bDbgMatch
                     ? "*** Reclaimed token/marker: " + sWorkLine
                     WAIT
                  ENDIF

                  /* Commented out 07-21-2001 - Seems wrong !
                  IF aMP[1] > 1000 .AND. xMarker != NIL
                     IF bDbgMatch
                        ? "Removed repeatble: " + aTail( aMarkers[ aMP[1] - 1000 ] )
                        WAIT
                     ENDIF
                     aSize( aMarkers[ aMP[1] - 1000 ], Len( aMarkers[ aMP[1] - 1000 ] ) - 1 )
                  ENDIF
                  */
               ENDIF

               // TODO: rethink this! Should only match if failure becuase we ran out of input and OPTIONAL. ???

               /* Optional (last) didn't match - Rule can still match. */
               IF nMatch == nMatches
                  IF bStatement .AND. ! Empty( sWorkLine )
                     /* Top of nested optional. */
                     IF aMP[2] > 1
                        /* Upper level optional should be accepted - rewind to top of parent group. */
                        //nOptional--
                        WHILE nMatch > 1
                           nMatch--
                           IF Abs( aMatchers[nMatch][2] ) < nOPtional
                              nMatch++
                              EXIT
                           ENDIF
                        ENDDO
                        // Added June-1-2003
                        IF nMatch == 1 .AND. aMatchers[nMatch][2] == 0
                           nMatch++
                        ENDIF

                        nOptional := 0

                        IF bDbgMatch
                           ? "1 - Nested last optional, Rewinded to:", nMatch
                        ENDIF

                        LOOP
                     ENDIF

                     bNext := .T.

                     IF bDbgMatch
                        ? "***3 Unmatched remainder: >", sWorkLine, "<"
                        ? "Statement failed, try next rule..."
                        WAIT
                     ENDIF

                     EXIT
                  ELSEIF bStatement
                     sWorkLine := ""
                  ENDIF

                  sPPO := PPOut( aResults[nRule], aMarkers, aMatchers )

                  IF bDbgMatch
                     IF HB_ISSTRING( sLine )
                        ? "TRANSLATED to:", sPPO
                     ELSE
                        ? "Output failed! Continue with next rule."
                     ENDIF

                     WAIT
                  ENDIF

                  IF HB_ISSTRING( sPPO )
                     sLine := sPPO + sPad + sWorkLine
                     RETURN nRule
                  ELSE
                     // Continue with next rule.
                     bNext := .T.
                     EXIT
                  ENDIF
               ELSE
                  /* Top of Nested optional, maybe last in its parrent group. */
                  IF aMP[2] > 1
                     /* Skip dependents and nested optionals, if any. */
                     nTemp := aMP[2]
                     nMatch++
                     WHILE ( nMatch <= nMatches ) .AND. ( Abs( aMatchers[nMatch][2] ) >= nTemp )
                        nMatch++
                     ENDDO

                     // End of rule or reached end of parrent group.
                     IF nMatch > nMatches .OR. aMatchers[nMatch][2] >= 0 .OR. nTemp + aMatchers[nMatch][2] > 1
                        /* Upper level optional should be accepted - rewind to top of parent group. */
                        //nOptional--
                        WHILE nMatch > 1
                           nMatch--
                           IF Abs( aMatchers[nMatch][2] ) < nOPtional
                              nMatch++
                              EXIT
                           ENDIF
                        ENDDO
                        // Added June-1-2003
                        IF nMatch == 1 .AND. aMatchers[nMatch][2] == 0
                           nMatch++
                        ENDIF

                        nOptional := 0

                        IF bDbgMatch
                           ? "2 - Nested last optional, Rewinded to:", nMatch
                        ENDIF

                        LOOP
                     ELSEIF aMatchers[nMatch][2] < 0
                        // More optionals of the upper level - try to continue matching.
                        IF bDbgMatch
                           ? "Resuming optionals of upper group at match:", nMatch
                        ENDIF

                        LOOP
                     ELSE
                        // Will proceed below (skip to next group) ...
                     ENDIF

                  ENDIF

                  /* Skip all same level optionals to next group. */
                  nOptional          := Abs( aMP[2] )
                  bRepeatableMatched := aMP[1] > 1000 .AND. aMarkers[ aMP[1] - 1000 ] != NIL //.AND. Len( aMarkers[ aMP[1] - 1000 ] ) > 0
                  WHILE nMatch < nMatches
                     nMatch++
                     aMP := aMatchers[nMatch]
                     IF ( aMP[2] < 0 ) .AND. ( Abs( aMP[2] ) < nOptional )
                        EXIT
                     ENDIF
                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= nOptional )
                        EXIT
                     ENDIF
                  ENDDO

                  // We should NOT consider this a failure, continue matching...
                  IF bRepeatableMatched
                     IF bDbgMatch
                        ? "Repeatable previously Matched - Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                     ENDIF

                     nOptional := aMP[2]
                     LOOP
                  ELSE
                     IF bDbgMatch
                        ? "Partial not allowed - Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                     ENDIF
                  ENDIF

                  IF nMatch == nMatches

                     IF ( aMP[2] >= 0 ) .AND. ( aMP[2] <= Abs( nOptional ) )
                        /* Ok. */
                     ELSEIF ( aMP[2] < 0 ) .AND. ( Abs( aMP[2] ) < Abs( nOptional ) ) // Added 07-21-2001 ???
                        /* Ok. */
                     ELSE
                        IF bDbgMatch
                           ? "Reached End of Rule"
                        ENDIF
                        EXIT
                     ENDIF
                  ENDIF

                  IF bDbgMatch
                     ? "Skipped to", nMatch, "of", nMatches, aMP[2], aMP[3], nOptional
                  ENDIF

                  nOptional := aMP[2]
                  LOOP
               ENDIF
            ELSE
               IF bDbgMatch
                  ? "Match failed, try next rule..."
                  WAIT
               ENDIF

               bNext := .T.
               EXIT
            ENDIF
         ENDIF

      ENDDO

      IF bNext
         IF bDbgMatch
            ? "NEXT Rule requested."
         ENDIF

         LOOP
      ELSE
         IF bStatement .AND. ! Empty( sWorkLine )
            IF bDbgMatch
               ? "***4 Unmatched remainder: >", sWorkLine, "<"
               ? "Statement failed, try next rule..."
               WAIT
            ENDIF

            LOOP
         ENDIF
      ENDIF

      IF bDbgMatch
         ? "EOL."
         WAIT
      ENDIF

      IF nMatch < nMatches

         IF bDbgMatch
            ? nMatch, "of:", nMatches, "Checking if Rule remainder is optional."
         ENDIF

         /* Current and remainder of MP NOT optional. */
         IF aMP[2] == 0
            IF bDbgMatch
               ? "NON Optional failed, Statement failed, try next rule..."
               WAIT
            ENDIF
            LOOP
         ELSE
            // Failed match is OPTIONAL.

            // defed out 2001-08-15 appear un-needed beacuse optional would have been ignored in favor of stopper.
            #ifdef NOT_NEEDED

               IF nOptional <> 0 .AND. aMP[2] < 0
                  sWorkLine := asRevert[Abs(nOptional)]
                  aMarkers  := aaRevertMarkers[Abs(nOptional)]

                  IF bDbgMatch
                     ? "*** Reverted: " + asRevert[nOptional]
                  ENDIF
               ELSE
                  sWorkLine := sPreMatch

                  IF bDbgMatch
                     ? "*** Reclaimed token/marker: " + sWorkLine
                  ENDIF

                  /* Commented out 07-21-2001 - Seems wrong !
                  IF aMP[1] > 1000 .AND. xMarker != NIL
                     IF bDbgMatch
                        ? "Removed repeatble: " + aTail( aMarkers[ aMP[1] - 1000 ] )
                        WAIT
                     ENDIF
                     aSize( aMarkers[ aMP[1] - 1000 ], Len( aMarkers[ aMP[1] - 1000 ] ) - 1 )
                  ENDIF
                  */
               ENDIF

            #endif

            IF bDbgMatch
               WAIT
            ENDIF

            //nOptional := aMP[2] // Commented 2001-08-15
            WHILE nMatch < nMatches
               nMatch++
               aMP := aMatchers[nMatch]
               IF ( aMP[2] == 0 )
                  EXIT
               ENDIF

               IF bDbgMatch
                  ? "Skipped:", nMatch, aMP[2], aMP[3]
               ENDIF
            ENDDO

            IF ( aMP[2] == 0 )
               IF bDbgMatch
                  ? "Statement failed, try next rule..."
                  WAIT
               ENDIF
               LOOP
            ENDIF
         ENDIF
      ENDIF

      sPPO := PPOut( aResults[nRule], aMarkers, aMatchers )

      IF bDbgMatch
         IF HB_ISSTRING( sLine )
            ? "TRANSLATED to:", sPPO
         ELSE
            ? "Output failed! Continue with next rule."
         ENDIF

         WAIT
      ENDIF

      IF HB_ISSTRING( sPPO )
         sLine := sPPO
         RETURN nRule
      ELSE
         // Continue with next rule.
         //LOOP
      ENDIF
   ENDDO

   Eval( ErrorBlock(), ErrorNew( [PP], 0, 3010, [Match-Rule], [Logic Failure], ) )

RETURN 0

//--------------------------------------------------------------//

STATIC FUNCTION NextExp( sLine, cType, aWords, sNextAnchor, bX )

  LOCAL sExp, Counter, sToken, sList
  LOCAL sNextLine, sNextToken, sLastToken, sJustToken, sJustNext, cLastChar
  LOCAL s1, s2, s4, s5, sNext1, sNext2, sNext3, sNext4, sNext5, nLen, nNextLen
  LOCAL sWorkLine, sPrimaryStopper, nStoppers, nStopper, sStopLine, sStopper
  LOCAL sMultiStopper, nSpaceAt, sNextStopper, cChar
  LOCAL aExp
  LOCAL nAt
  LOCAL nOpen

  IF  cType == '*'
      IF Empty( s_sPending ) .OR. AllTrim( s_sPending ) == ';'
      ELSE
         s_sPending := PP_PreProLine( s_sPending, 0, "" )
         sLine += ";" + s_sPending
         s_sPending := ""
      ENDIF
  ENDIF

  IF Empty( sLine )
     RETURN NIL
  ENDIF

  //TraceLog( "*** Start", cType, sLine, sNextAnchor, bX )

  DO CASE
     CASE cType == '<'
        /* No prep needed */

     CASE cType == 'A'
        aExp := {}

     CASE cType == ','
        sList := ""

     CASE cType == ':'
        sWorkLine       := sLine
        sPrimaryStopper := NextToken( @sWorkLine )

        IF sPrimaryStopper == NIL
           //? "No primary", sPrimaryStopper
           RETURN NIL
        ELSE
           sPrimaryStopper := Upper( RTrim( sPrimaryStopper ) )

           /* Is it a stopper (the anchor of another acceptable match) ? */
           IF bDbgExp
              ? "Stopper?: '" + sPrimaryStopper +"'"
           ENDIF

           nStoppers := Len( aWords )
           FOR nStopper := 1 TO nStoppers

              sStopLine := sWorkLine
              sToken    := sPrimaryStopper
              sStopper  := aWords[ nStopper ]

              sMultiStopper := ""
              WHILE ( nSpaceAt := At( ' ', sStopper ) ) > 0
                 sNextStopper := Left( sStopper, nSpaceAt - 1 )

                 IF bX
                    nLen := 64
                 ELSE
                    nLen := Max( 4, Len( sToken ) )
                 ENDIF

                 //? "Next Stopper: " + sNextStopper, sToken
                 IF Left( sNextStopper, nLen ) == sToken
                    sMultiStopper += sNextStopper
                    sStopper      := SubStr( sStopper, nSpaceAt )
                    sMultiStopper += ExtractLeadingWS( @sStopper )
                    sToken        := NextToken( @sStopLine )
                    sToken        := Upper( RTrim( sToken ) )
                 ELSE
                    EXIT
                 ENDIF
              ENDDO

              IF bX
                 nLen := 64
              ELSE
                 nLen := Max( 4, Len( sToken ) )
              ENDIF

              IF Left( sStopper, nLen ) == sToken
                 sMultiStopper += sStopper
                 EXIT
              ENDIF
           NEXT

           IF nStopper <= nStoppers
              sLine := sStopLine
              //TraceLog( sMultiStopper, sStopLine )
              RETURN sMultiStopper
           ELSE
              sLine := sWorkLine
              RETURN NIL
           ENDIF
        ENDIF

     CASE cType == '*'
        sExp  := sLine
        sLine := ""

        //? "EXP <*>: " + sExp
        RETURN sExp

     CASE cType == '('
        s1 := Left( sLine, 1 )

        IF s1 $ "=:"
           RETURN NIL
        ELSEIF s1 == '('
           // Continue with normal Matcher
        ELSE
            nAt := nAtAnyCharSkipStr( " ,", sLine )

            IF nAt == 0
               sExp  := sLine
               sLine := ""
            ELSEIF nAt > 1
               sExp  := Left( sLine, nAt - 1 )
               sLine := SubStr( sLine, nAt )
               sExp  += ExtractLeadingWS( @sLine )
            ELSE
               sExp := NIL
            ENDIF

            //? "EXP <(>: " + sExp
            RETURN sExp
        ENDIF

     CASE cType == '!'
        IF IsAlpha( cChar := Left( sLine, 1 ) ) .OR. cChar == '_'
           RETURN NextToken( @sLine )
        ELSE
           RETURN NIL
        ENDIF

     CASE cType == NIL
        RETURN "-"
  ENDCASE

  sExp := ""
  DO WHILE .T.
     sToken := NextToken( @sLine )

     IF sToken == NIL
        EXIT
     ENDIF

     //TraceLog( sToken )

     sJustToken := RTrim( sToken )

     IF sNextAnchor != NIL  .AND. sJustToken == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor $ ',' .AND. cType $ ",A" )
           //TraceLog( "Anchor: '" + sNextAnchor + "' found!" )
           sLine := sToken + sLine
           EXIT
        ENDIF
     ENDIF

     nLen := Len( sJustToken )
     s1 := Left( sJustToken, 1 )
     s2 := s4 := s5 := ""
     IF nLen == 2
        s2 := sJustToken
     ELSEIF nLen == 4
        s4 := Upper( sJustToken )
     ELSEIF nLen == 5
        s5 := Upper( sJustToken )
     ENDIF

     IF Empty( sLine )
        sNextToken := ""
        sJustNext  := ""
        sNext1     := ""
     ELSE
        sNextLine := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
           sJustNext  := ""
           sNext1     := ""
        ELSE
           sJustNext := RTrim( sNextToken )
           sNext1    := Left( sJustNext, 1 )
        ENDIF
     ENDIF

     // ------------------
     // 1st. Level.
     // ------------------

     IF bDbgExp
        ? "1st. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     //TraceLog( "Token: '" + sToken + "' Len: " + Str( nLen ) + " Next: '" + sNextToken + "'", sLine )

     IF nLen == 1

        // *** Very ODD Clipper considers '|', '.', '*' '/', '\', '^', '%', '>', '<' '#', '$' '?' a valid startup/continuation token !!!
        IF s1 $ "-+!:@|.*/^%><#$?"
           sExp += sToken
           LOOP

        ELSEIF s1 == "&" .AND. ( s1 == sToken .OR. sNext1 == '(' )// No white space, or parenthesized.
           sExp += sToken

           IF sNext1 == '('
              LOOP
           ELSE
              IF IsAlpha( sNext1 ) .OR. sNext1 == '_'
                 sExp           += sNextToken
                 sLastToken     := sJustNext
                 sLine          := sNextLine

                 #ifdef USE_C_BOOST
                    SetArrayPrefix( .T. )
                 #else
                    s_bArrayPrefix := .T.
                 #endif

                 sNextToken     := NextToken( @sNextLine, .T. )

                 IF sNextToken != NIL .AND. Left( sNextToken, 1 ) == '.'
                    // Get the macro terminator.
                    sExp           += sNextToken
                    sLastToken     := "."
                    sLine          := sNextLine

                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif

                    IF sNextToken == '.' //(Last Token) No space after Macro terminator, so get the suffix.
                       sNextToken := NextToken( @sNextLine, .T. )
                       IF sNextToken != NIL
                          sNext1 := Left( sNextToken, 1 )
                          IF IsAlpha( sNext1 ) .OR. IsDigit( sNext1 ) .OR. sNext1 == '_'
                             // Get the macro sufix.
                             sExp           += sNextToken
                             sLastToken     := RTrim( sNextToken )
                             sLine          := sNextLine

                             #ifdef USE_C_BOOST
                                SetArrayPrefix( .T. )
                             #else
                                s_bArrayPrefix := .T.
                             #endif
                          ENDIF
                       ENDIF
                    ELSEIF sNextToken == '&' //(Last Token) Get the next Macro suffix.
                       sExp += NextExp( @sLine, cType, aWords, sNextAnchor, bX )
                    ENDIF
                 ENDIF
              ELSE
                 Eval( s_bRTEBlock, ErrorNew( [PP], 0, 3010, [Next-Token], [Invalid &], { sExp, sNextToken } ) )
                 // Safety
                 BREAK
              ENDIF
           ENDIF

           sLastToken := RTrim( sLastToken )

           IF Left( sLine, 1 ) == '.'
              LOOP
           ENDIF

           // Continue  2nd level checks below.

        ELSEIF s1 == "&" // Followed by white space.
           sExp += sToken
           LOOP

        ELSEIF s1 == '('
           sExp += sToken
           nOpen := 1

           WHILE .T.
              sToken := NextToken( @sLine )

              IF sToken == NIL
                 EXIT
              ELSE
                 sExp += sToken
                 s1 := Left( sToken, 1 )

                 IF s1 == ')'
                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif

                    IF --nOpen == 0
                       EXIT
                    ENDIF
                 ELSEIF s1 == '('
                    nOpen++
                 ENDIF
              ENDIF
           ENDDO

           IF sToken != NIL
              sLastToken := RTrim( sToken )
           ENDIF

           // Continue  2nd level checks below.
        ELSEIF s1 == '{'
           sExp += sToken
           nOpen := 1

           WHILE .T.
              sToken := NextToken( @sLine )

              IF sToken == NIL
                 EXIT
              ELSE
                 sExp += sToken
                 s1 := Left( sToken, 1 )

                 IF s1 == '}'
                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif

                    IF --nOpen == 0
                       EXIT
                    ENDIF
                 ELSEIF s1 == '{'
                    nOpen++
                 ENDIF
              ENDIF
           ENDDO

           sLastToken := RTrim( sToken )

           // Continue  2nd level checks below.
        ELSEIF s1 == "["
           sExp += sToken
           nOpen := 1

           WHILE .T.
              sToken := NextToken( @sLine )

              IF sToken == NIL
                 EXIT
              ELSE
                 sExp += sToken
                 s1 := Left( sToken, 1 )

                 IF s1 == ']'
                    #ifdef USE_C_BOOST
                       SetArrayPrefix( .T. )
                    #else
                       s_bArrayPrefix := .T.
                    #endif

                    IF --nOpen == 0
                       EXIT
                    ENDIF
                 ELSEIF s1 == '['
                    nOpen++
                 ENDIF
              ENDIF
           ENDDO

           sLastToken := RTrim( sToken )

           // Continue  2nd level checks below.
        ELSEIF s1 $ "=)}]"
           sLine := sToken + sLine
           EXIT
        ELSEIF s1 == ","
           IF cType == ","
              sList += ( sExp + sToken )
              sExp  := ""
              LOOP
           ELSEIF cType == "A"
              aAdd( aExp, sExp )
              sExp  := ""
              LOOP
           ELSE
              //? "DONT CONTINUE: " + sLine
              sLine := sToken + sLine
              EXIT
           ENDIF
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 2

        // Odd, Clipper considers '->', '**', '!=', '<>', '>=', '<=' as valid startup tokens!!!
        IF s2 $ '++;--;->;**;!=;<>;>=;<='
           sExp += sToken
           LOOP
        ELSEIF s2 $ "==;:=;+=;-=;*=;^=;/=;%="
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 4

        IF s4 == '.OR.'
           sLine := sToken + sLine
           EXIT
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSEIF nLen == 5

        IF s5 == '.AND.'
           sLine := sToken + sLine
           EXIT
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF s5 == ".NOT."
           sExp       += sToken
           LOOP
        */
        ELSE
           sExp       += sToken
           sLastToken := sJustToken
        ENDIF

     ELSE

        sExp       += sToken
        sLastToken := sJustToken

     ENDIF

     // ------------------
     // 2nd. Level.
     // ------------------

     //TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )

     IF sLastToken == NIL .OR. Right( sLastToken, 1 ) == ' '
        //TraceLog( sExp, sLastToken, sLine, nLen, sToken, sNextToken )
        //Alert( "??? " + sExp )
        EXIT
     ENDIF

     nLen := Len( sLastToken )
     cLastChar := Right( sLastToken, 1 )

     IF Empty( sLine )
        EXIT
     ELSE
        sNextLine  := sLine
        sNextToken := NextToken( @sNextLine, .T. )
        IF sNextToken == NIL
           sNextToken := ""
        ENDIF
     ENDIF

     sJustNext := RTrim( sNextToken )
     nNextLen := Len( sJustNext )

     sNext1 := Left( sJustNext, 1 )
     sNext2 := sNExt3 := sNext4 := sNext5 := ""

     IF nNextLen == 2
        sNext2 := sJustNext
     ELSEIF nNextLen == 3
        sNext3 := Upper( sJustNext )
     ELSEIF nNextLen == 4
        sNext4 := Upper( sJustNext )
     ELSEIF nNextLen == 5
        sNext5 := sJustNext
     ENDIF

     IF bDbgExp
        ? "2nd. Level - Token: '" + sToken + "' Next: '" + sNextToken + "'"
        WAIT
     ENDIF

     IF sNextAnchor != NIL  .AND. sJustNext == sNextAnchor
        // Clipper give preference to ',' in list expression.
        IF ! ( sNextAnchor == ',' .AND. cType $ ",A" )
           EXIT
        ENDIF
     ENDIF

     //TraceLog( sExp, sToken, sJustToken, nLen, sNextToken, sJustNext, nNextLen, sLastToken, cLastChar, sNextAnchor )

     IF nNextLen == 1

        IF sNext1 == '(' .AND. ( IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_." )
           LOOP
        ELSEIF sNext1 == '[' // No need to check prefix because NextToken() already has the logic.
           LOOP
        ELSEIF sNext1 $ "+-*/:=^!><!$%#|." // *** Very ODD Clipper consider '|' and '.' a continuation token !!!
           sExp           += sNextToken
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           LOOP
        ELSEIF sNext1 == "&" .AND. Len( sNextToken ) > 1 // & folowed bt white space.
           LOOP
        ENDIF

     ELSEIF nNextLen == 2

        IF sNext2 $ "--\++"
           IF IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "_.]"
              sExp  += sNextToken
              sLine := sNextLine

              #ifdef USE_C_BOOST
                 SetArrayPrefix( .F. )
              #else
                 s_bArrayPrefix := .F.
              #endif
           ENDIF
        ELSEIF sNext2 $ "->\:=\==\!=\<>\>=\<=\+=\-=\*=\/=\^=\**\%=\IN\=>\^^\>>\<<"
           sExp           += sNextToken
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .T. )
           #else
              s_bArrayPrefix := .T.
           #endif

           LOOP
        ENDIF

     ELSEIF nNextLen == 3

        IF sNext3 == "HAS"
           sExp           += sNextToken
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           LOOP
        ENDIF

     ELSEIF nNextLen == 4

        IF sNext4 == ".OR." .OR. sNext4 == "LIKE"
           sExp           += sNextToken
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           LOOP
        ENDIF

     ELSEIF nNextLen == 5

        IF sNext5 == ".AND."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           LOOP
        /* .NOT. is being translated to ! at NextToken() !!!
        ELSEIF sNext5 == ".NOT."
           sExp           += sNextToken
           sLine          := sNextLine
           s_bArrayPrefix := .F.
           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif */
        ENDIF

     ENDIF

     // ------------------
     // 3rd. Level.
     // ------------------

     IF sNext1 == ','
        IF cType == ","
           sList          += ( sExp + sNextToken )
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           sExp           := ""
        ELSEIF cType == "A"
           aAdd( aExp, sExp )
           sLine          := sNextLine

           #ifdef USE_C_BOOST
              SetArrayPrefix( .F. )
           #else
              s_bArrayPrefix := .F.
           #endif

           sExp           := ""
        ELSE
           //? "DONT CONTINUE: " + sLine
           EXIT
        ENDIF
     ELSE
        //? "DONT CONTINUE: " + sLine
        EXIT
     ENDIF
  ENDDO

  IF cType == 'A'
     IF sExp == ""
        IF Len( aExp ) == 0
           aExp := NIL
        ENDIF
     ELSE
        aAdd( aExp, sExp )
     ENDIF

     IF bDbgExp
        IF ! ( ProcName(1) == "NEXTEXP" )
           ? "List Exp: " + '{'
           FOR Counter := 1 TO Len( aExp )
              ?? aExp[Counter]
              IF Counter < Len( aExp )
                 ?? ','
              ENDIF
           NEXT
           ?? '}'
        ENDIF
     ENDIF
  ELSEIF cType == ','
     IF sExp == ""
        IF sList == ""
           sExp := NIL
        ELSE
           sExp := sList
        ENDIF
     ELSE
        sExp := ( sList + sExp )
     ENDIF

     IF bDbgExp
        ? "List =", sExp, " Next:", sLine
     ENDIF
  ELSE
     IF sExp == ""
        sExp := NIL
     ENDIF
     IF bDbgExp
        ? "Exp =", sExp, " Next:", sLine
     ENDIF
  ENDIF

  IF bDbgExp
     WAIT
  ENDIF

  //TraceLog( "*** Finish", cType, aExp, sExp, sLine, sNextAnchor )

RETURN IIF( cType == 'A', aExp, sExp )

//--------------------------------------------------------------//

STATIC FUNCTION PPOut( aResults, aMarkers, aMatchers )

  LOCAL Counter, nResults, sResult := "", nMarker, nMatches, nMatch
  LOCAL xValue, nRepeats := 0, nDependee, nGroupStart, sDumb, aBackUp := aClone( aMarkers )
  LOCAL nMarkers, anMarkers, bBuildList
  LOCAL nGroupIterator
  LOCAL lMacro, lComplexMacro

  IF aResults[1] == NIL
     nResults := 0
  ELSE
     nResults := Len( aResults[1] )
  ENDIF

  FOR Counter := 1 TO nResults

     IF bDbgPPO
        ? sResult
        ? Counter, "of:", nResults, nGroupStart, nRepeats
        WAIT
     ENDIF

     /* Normal mode. */
     IF nRepeats == 0

        nDependee := aResults[1][Counter][1]

        IF nDependee > 0
           nGroupStart := Counter

           nGroupIterator := Counter
           nRepeats := 0
           WHILE nGroupIterator <= nResults .AND. aResults[1][nGroupIterator][1] == nDependee
              IF ValType( aResults[1][nGroupIterator][2] ) == 'N'
                 IF ValType( aMarkers[ aResults[1][nGroupIterator][2] ] ) == 'A'
                    nRepeats := Max( nRepeats, Len( aMarkers[ aResults[1][nGroupIterator][2] ] ) )
                 ELSEIF ! Empty( aMarkers[ aResults[1][nGroupIterator][2] ] )
                    nRepeats := Max( nRepeats, 1 )
                 ENDIF
              ENDIF
              nGroupIterator++
           ENDDO
           IF nRepeats > 0
              anMarkers := {}
              bBuildList := .T.
           ENDIF

           IF bDbgPPO
              ? Counter, nDependee, aMarkers, ValType( aMarkers ), nRepeats
              WAIT
           ENDIF

           IF nRepeats > 0
              IF ValType( aResults[1][Counter][2] ) == 'N'
                 IF bBuildList .AND. aScan( anMarkers, nDependee ) == 0
                    aAdd( anMarkers, nDependee )
                 ENDIF

                 // For group head nDependee and nMaker _must_ be identical.
                 IF ValType( aMarkers[ nDependee ] ) == 'A'
                    xValue := aMarkers[ nDependee ][1]
                 ELSE
                    xValue := aMarkers[ nDependee ]
                 ENDIF
              ELSE
                 sResult += aResults[1][Counter][2]
                 LOOP
              ENDIF
           ELSE
              IF bDbgPPO
                 ? "Skipping other dependants"
                 WAIT
              ENDIF

              /* Skip all other dependants. */
              Counter++
              WHILE Counter < nResults .AND. aResults[1][Counter][1] == nDependee
                 Counter++
              ENDDO
              Counter-- // LOOP will increased.
              LOOP
           ENDIF

        ELSE // IF nDependee > 0

           IF ValType( aResults[1][Counter][2] ) == 'N'
              xValue := aMarkers[ aResults[1][Counter][2] ]

              IF aMatchers[ aResults[1][Counter][2] ][4] != 'A' .AND. ValType( xValue ) == 'A' .AND. Len( xValue ) > 1
                 // Case: ?? 1, 2 => QQOut( 1, 2 )
                 IF aMatchers[ aResults[1][Counter][2] ][4] != NIL
                    IF bDbgPPO
                       //TraceLog( ValToPrg( aResults ), aResults[1][Counter][2], aMatchers[aResults[1][Counter][2]][4], "Too many values for non repeatable result pattern!", Counter )
                       ? "Too many values for non repeatable result pattern!", Counter
                    ENDIF
                    RETURN .F.
                 ENDIF
              ENDIF
           ELSE
              sResult += aResults[1][Counter][2]
              LOOP
           ENDIF

        ENDIF // IF nDependee > 0

     ELSE /* Repeat mode. */

        /* Still in repeat group? */
        IF aResults[1][Counter][1] == nDependee

           IF ValType( aResults[1][Counter][2] ) == 'N'
              //IF aMarkers[ aResults[1][Counter][2] ] != NIL
                 IF bBuildList .AND. aScan( anMarkers, aResults[1][Counter][2] ) == 0
                    aAdd( anMarkers, aResults[1][Counter][2] )
                 ENDIF
                 IF aMarkers[ aResults[1][Counter][2] ] == NIL .OR. Len( aMarkers[ aResults[1][Counter][2] ] ) == 0
                    xValue := NIL
                 ELSEIF ValType( aMarkers[ aResults[1][Counter][2] ] ) == 'A'
                    xValue := aMarkers[ aResults[1][Counter][2] ][1]
                 ELSE
                    xValue := aMarkers[ aResults[1][Counter][2] ]
                 ENDIF
                 //aDel( aMarkers[ aResults[1][Counter][2] ], 1 )
                 //aSize( aMarkers[ aResults[1][Counter][2] ], nRepeats - 1 )
              //ELSE
              //   xValue := ""
              //ENDIF
           ELSE
              sResult += aResults[1][Counter][2]

              IF nRepeats > 1 .AND. Counter == nResults
                 nRepeats--
                 Counter := nGroupStart - 1

                 bBuildList := .F.

                 nMarkers := Len( anMarkers )
                 FOR nMarker := 1 TO nMarkers
                    // Clipper does not remove optional nested repeatable which only has single value if main repeatable has more values.
                    IF ValType( aMarkers[ anMarkers[nMarker] ] ) == 'A' .AND. ( Len( aBackup[ anMarkers[1] ] ) == 1 .OR. Len( aMarkers[ anMarkers[nMarker] ] ) > 1 )
                       IF bDbgPPO
                          ? nMarker, "- Removing Repeatable", aMarkers[ anMarkers[nMarker] ][1]
                          WAIT
                       ENDIF
                       aDel( aMarkers[ anMarkers[nMarker] ], 1 )
                       aSize( aMarkers[ anMarkers[nMarker] ], nRepeats )
                    ELSE
                       IF bDbgPPO
                          ? nMarker, Len( aBackup[ anMarkers[1] ] ), Len( aMarkers[ anMarkers[nMarker] ] ),"Removed Repeatable"
                          WAIT
                       ENDIF
                    ENDIF
                 NEXT

                 IF bDbgPPO
                    ? "END - Looping: ", Counter, nMarker, nGroupStart, nRepeats
                    WAIT
                 ENDIF
              ENDIF

              IF bDbgPPO
                 ? "Bottom: ", Counter, nMarker, nGroupStart, nRepeats
                 WAIT
              ENDIF

              LOOP
           ENDIF
        ELSE
           nRepeats--
           bBuildList := .F.

           nMarkers := Len( anMarkers )
           FOR nMarker := 1 TO nMarkers
              // Clipper does not remove optional nested repeatable which only has single value if main repeatable has more values.
              IF ValType( aMarkers[ anMarkers[nMarker] ] ) == 'A' .AND. Len( aMarkers[ anMarkers[nMarker] ] ) > 1
                 IF bDbgPPO
                    ? nMarker, "+ Removing Repeatable", aMarkers[ anMarkers[nMarker] ][1]
                    WAIT
                 ENDIF
                 aDel( aMarkers[ anMarkers[nMarker] ], 1 )
                 aSize( aMarkers[ anMarkers[nMarker] ], nRepeats )
              ELSE
                 IF bDbgPPO
                    ? nMarker, "Removed Repeatable skipped"
                    WAIT
                 ENDIF
              ENDIF
           NEXT

           IF nRepeats > 0
              IF bDbgPPO
                 ? "Looping to: ", nGroupStart, nRepeats
                 WAIT
              ENDIF

              Counter := nGroupStart - 1 // LOOP will increase
              LOOP
           ELSE
              IF bDbgPPO
                 ? "Repeats Finished: "
                 WAIT
              ENDIF

              // Restore for possible re-use.
              aMarkers := aClone( aBackup )

              /* Recheck this item in "normal" mode. */
              Counter--
              LOOP
           ENDIF
        ENDIF

     ENDIF

     nMarker := aResults[1][Counter][2]

     IF bDbgPPO
        ? "Outputing:", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

     DO CASE
        /* <-x-> Ommit. */
        CASE aResults[2][Counter] == 0

        /* <x> Regular */
        CASE aResults[2][Counter] == 1
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += xValue[nMatch]
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 sResult += xValue
              ENDIF
           ENDIF

        /* #<x> Dumb Stringify */
        CASE aResults[2][Counter] == 2
           IF ValType( xValue ) == 'A'
              sDumb := ""
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sDumb += xValue[nMatch]
                 IF nMatch < nMatches
                    sDumb += ", "
                 ENDIF
              NEXT
              IF '"' $ sDumb .AND. "'" $ sDumb .AND. ']' $ sDumb .AND. Left( sDumb, 1 ) != '['
                 sResult += '[[' + sDumb + ']]'
              ELSEIF '"' $ sDumb .AND. "'" $ sDumb
                 sResult += '[' + sDumb + "]"
              ELSEIF '"' $ sDumb
                 sResult += "'" + sDumb + "'"
              ELSE
                 sResult += '"' + sDumb + '"'
              ENDIF
           ELSE
              IF xValue == NIL
                 sResult += '""'
              ELSE
                 IF '"' $ xValue .AND. "'" $ xValue .AND. ']' $ xValue .AND. Left( xValue, 1 ) != '['
                    sResult += "[[" + xValue + "]]"
                 ELSEIF '"' $ xValue .AND. "'" $ xValue
                    sResult += '[' + xValue + "]"
                 ELSEIF '"' $ xValue
                    sResult += "'" + xValue + "'"
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <"x"> Normal Stringify */
        CASE aResults[2][Counter] == 3
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 /*
                    Clipper bug does NOT check operator existence if the Experssion begins with a Parentesized Macro - apparantly because
                    it then becomes somewhat complex to find such operator only after the end of the Parentesized Macro.
                    Emulating this bug due to same "complexity". :-)
                  */
                 IF Left( xValue[nMatch], 1 ) == '&' .AND. ( SubStr( xValue[nMatch], 2, 1 ) == '(' .OR. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue[nMatch] ) == 0 )
                    lMacro := .T.

                    IF SubStr( xValue[nMatch], 2, 1 ) != '(' .AND. '.' $ SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                       lComplexMacro := .T.
                    ELSE
                       lComplexMacro := .F.
                    ENDIF
                 ELSE
                    lMacro := .F.
                 ENDIF

                 IF lMacro .AND. ! lComplexMacro
                    IF Right( xValue[nMatch], 1 ) == '.'
                       sResult += SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                    ELSE
                       sResult += SubStr( xValue[nMatch], 2 )
                    ENDIF
                 ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch] .AND. ']' $ xValue[nMatch] .AND. Left( xValue[nMatch], 1 ) != '['
                    sResult += "[[" + RTrim( xValue[nMatch] ) + "]]"
                 ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch]
                    sResult += '[' + RTrim( xValue[nMatch] ) + "]"
                 ELSEIF '"' $ xValue[nMatch]
                    sResult += "'" + RTrim( xValue[nMatch] ) + "'"
                 ELSE
                    sResult += '"' + RTrim( xValue[nMatch] ) + '"'
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF ! ( xValue == NIL )
                 /*
                    Clipper bug does NOT check operator existence if the Experssion begins with a Parentesized Macro - apparantly because
                    it then becomes somewhat complex to find such operator only after the end of the Parentesized Macro.
                    Emulating this bug due to same complexity. :-)
                  */
                 IF Left( xValue, 1 ) == '&' .AND. ( SubStr( xValue, 2, 1 ) == '(' .OR. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue ) == 0 )
                    lMacro := .T.

                    IF SubStr( xValue, 2, 1 ) != '(' .AND. '.' $ SubStr( xValue, 2, Len( xValue ) - 2 )
                       lComplexMacro := .T.
                    ELSE
                       lComplexMacro := .F.
                    ENDIF
                 ELSE
                    lMacro := .F.
                 ENDIF

                 IF lMacro .AND. ! lComplexMacro
                    IF Right( xValue, 1 ) == '.'
                       sResult += SubStr( xValue, 2, Len( xValue ) - 2 )
                    ELSE
                       sResult += SubStr( xValue, 2 )
                    ENDIF
                 ELSEIF '"' $ xValue .AND. "'" $ xValue .AND. ']' $ xValue .AND. Left( xValue, 1 ) != '['
                    sResult += "[[" + xValue + "]]"
                 ELSEIF '"' $ xValue .AND. "'" $ xValue
                    sResult += '[' + xValue + ']'
                 ELSEIF '"' $ xValue
                    sResult += "'" + xValue + "'"
                 ELSE
                    sResult += '"' + xValue + '"'
                 ENDIF
              ENDIF
           ENDIF

        /* <(x)> Smart Stringify */
        CASE aResults[2][Counter] == 4
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 IF Left( xValue[nMatch], 1 ) == '(' .OR. ( Left( xValue[nMatch], 1 ) $ "'[" + '"' .AND. Left( xValue[nMatch], 1 ) == Right( xValue[nMatch], 1 ) .AND. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue[nMatch] ) == 0 )
                    sResult += xValue[nMatch]
                 ELSE
                    /*
                       Clipper bug does NOT check operator existence if the Experssion begins with a Parentesized Macro - apparantly because
                       it then becomes somewhat complex to find such operator only after the end of the Parentesized Macro.
                       Emulating this bug due to same complexity. :-)
                    */
                    IF Left( xValue[nMatch], 1 ) == '&' .AND. ( SubStr( xValue[nMatch], 2, 1 ) == '(' .OR. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue[nMatch] ) == 0 )
                       lMacro := .T.

                       IF SubStr( xValue[nMatch], 2, 1 ) != '(' .AND. '.' $ SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                          lComplexMacro := .T.
                       ELSE
                          lComplexMacro := .F.
                       ENDIF
                    ELSE
                       lMacro := .F.
                    ENDIF

                    IF lMacro .AND. ! lComplexMacro
                       IF Right( xValue[nMatch], 1 ) == '.'
                          sResult += SubStr( xValue[nMatch], 2, Len( xValue[nMatch] ) - 2 )
                       ELSE
                          sResult += SubStr( xValue[nMatch], 2 )
                       ENDIF
                    ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch] .AND. ']' $ xValue[nMatch] .AND. Left( xValue[nMatch], 1 ) != '['
                       sResult += "[[" + RTrim( xValue[nMatch] ) + "]]"
                    ELSEIF '"' $ xValue[nMatch] .AND. "'" $ xValue[nMatch]
                       sResult += '[' + RTrim( xValue[nMatch] ) + "]"
                    ELSEIF '"' $ xValue[nMatch]
                       sResult += "'" + RTrim( xValue[nMatch] ) + "'"
                    ELSE
                       sResult += '"' + RTrim( xValue[nMatch] ) + '"'
                    ENDIF
                 ENDIF

                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF xValue != NIL
                 IF Left( xValue, 1 ) == '(' .OR. ( Left( xValue, 1 ) $ "('[" + '"' .AND. Left( xValue, 1 ) == Right( xValue, 1 ) .AND. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue ) == 0 )
                    sResult += xValue
                 ELSE
                    /*
                       Clipper bug does NOT check operator existence if the Experssion begins with a Parentesized Macro - apparantly because
                       it then becomes somewhat complex to find such operator only after the end of the Parentesized Macro.
                       Emulating this bug due to same complexity. :-)
                     */
                    IF Left( xValue, 1 ) == '&' .AND. ( SubStr( xValue, 2, 1 ) == '(' .OR. nAtAnyCharSkipStr( "+-*/^$=!#<>|", xValue ) == 0 )
                       lMacro := .T.

                       IF SubStr( xValue, 2, 1 ) != '(' .AND. '.' $ SubStr( xValue, 2, Len( xValue ) - 2 )
                          lComplexMacro := .T.
                       ELSE
                          lComplexMacro := .F.
                       ENDIF
                    ELSE
                       lMacro := .F.
                    ENDIF

                    IF lMacro .AND. ! lComplexMacro
                       IF Right( xValue, 1 ) == '.'
                          sResult += SubStr( xValue, 2, Len( xValue ) - 2 )
                       ELSE
                          sResult += SubStr( xValue, 2 )
                       ENDIF
                    ELSEIF '"' $ xValue .AND. "'" $ xValue .AND. ']' $ xValue .AND. Left( xValue, 1 ) != '['
                       sResult += "[[" + xValue + "]]"
                    ELSEIF '"' $ xValue .AND. "'" $ xValue
                       sResult += '[' + xValue + ']'
                    ELSEIF '"' $ xValue
                       sResult += "'" + xValue + "'"
                    ELSE
                       sResult += '"' + xValue + '"'
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF

        /* <{x}> Blockify */
        CASE aResults[2][Counter] == 5
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )

              IF bStrict .AND. Left( xValue[1], 1 ) == '{' .AND. Left( LTrim( SubStr( xValue[1], 2 ) ), 1 ) == '|'
                 FOR nMatch := 1 TO nMatches
                    sResult += xValue[nMatch]
                    IF nMatch < nMatches
                       sResult += ', '
                    ENDIF
                 NEXT
              ELSE
                 FOR nMatch := 1 TO nMatches
                    IF Left( xValue[nMatch], 1 ) == '{' .AND. Left( LTrim( SubStr( xValue[nMatch], 2 ) ), 1 ) == '|'
                       sResult += xValue[nMatch]
                    ELSE
                       sResult += "{|| " + xValue[nMatch] + '}'
                    ENDIF
                    IF nMatch < nMatches
                       sResult += ', '
                    ENDIF
                 NEXT
              ENDIF
           ELSE
              IF xValue != NIL
                 IF Left( xValue, 1 ) == '{' .AND. Left( LTrim( SubStr( xValue, 2 ) ), 1 ) == '|'
                    sResult += xValue
                 ELSE
                    sResult += "{|| " + xValue + '}'
                 ENDIF
              ENDIF
           ENDIF

        /* <.x.> Logify */
        CASE aResults[2][Counter] == 6
           IF ValType( xValue ) == 'A'
              nMatches := Len( xValue )
              FOR nMatch := 1 TO nMatches
                 sResult += ".T."
                 IF nMatch < nMatches
                    sResult += ', '
                 ENDIF
              NEXT
           ELSE
              IF Empty( xValue )
                 sResult += ".F."
              ELSE
                 sResult += ".T."
              ENDIF
           ENDIF

     ENDCASE

     IF nRepeats > 1 .AND. Counter == nResults
        nRepeats--
        Counter := nGroupStart - 1

        bBuildList := .F.

        nMarkers := Len( anMarkers )
        FOR nMarker := 1 TO nMarkers
           // Clipper does not remove optional nested repeatable which only has single value if main repeatable has more values.
           IF ValType( aMarkers[ anMarkers[nMarker] ] ) == 'A' .AND. ( Len( aBackup[ anMarkers[1] ] ) == 1 .OR. Len( aMarkers[ anMarkers[nMarker] ] ) > 1 )
              aDel( aMarkers[ anMarkers[nMarker] ], 1 )
              aSize( aMarkers[ anMarkers[nMarker] ], nRepeats )
              IF bDbgPPO
                 ? nMarker, "Removed Repeatable"
                 WAIT
              ENDIF
           ELSE
              IF bDbgPPO
                 ? nMarker, Len( aBackup[ anMarkers[1] ] ), Len( aMarkers[ anMarkers[nMarker] ] ),"Removed Repeatable"
                 WAIT
              ENDIF
           ENDIF
        NEXT

        IF bDbgPPO
           ? "END - Looping: ", Counter, nMarker, nGroupStart, nRepeats
           WAIT
        ENDIF
     ENDIF

     IF bDbgPPO
        ? "Bottom: ", Counter, nMarker, nGroupStart, nRepeats
        WAIT
     ENDIF

  NEXT

  IF bDbgPPO
     ? "*** OUT: " + sResult
     WAIT
  ENDIF

  //TraceLog( sResult )

RETURN sResult

//--------------------------------------------------------------//

STATIC PROCEDURE CompileRule( sRule, aRules, aResults, bX, bDelete )

   LOCAL nNext, sKey, sAnchor, nOptional := 0, cType, nId := 0, aRule, aMatch, aWords
   LOCAL nOptionalAt, nMarkerAt, aMarkers := {}, Counter, nType, aResult := {}, sTemp, aModifiers
   LOCAL aRP, nAt, sResult, nCloseAt, sMarker, nCloseOptionalAt, sPad, nResults, nMarker, nMP, nMatches
   LOCAL nWord, nWords, cChar
   LOCAL sRuleCopy := sRule
   LOCAL nLastOptional, nPending
   LOCAL sDots
   LOCAL nMarkerID
   LOCAL nTempMP
   LOCAL nTokenLen
   LOCAL aMatchRule, cRuleExp
   LOCAL sWord
   LOCAL nLen, sToken
   LOCAL nRule, nRules

   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   aNext
   */

   //? "=>" + sRule + "<="

   //TraceLog( sRule, bX, bDelete )

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule )
   IF Left( sKey, 1 ) == '\'
      sKey := SubStr( sKey, 2, 1 )
   ENDIF

   DropTrailingWS( @sKey )

   sKey := Upper( sKey )

   //? "KEY: '" + sKey + "'"

   aRule := { sKey, {}, bX }

   nNext := 0
   DO WHILE ( nNext := AtInRules( "=", sRule, nNext + 1 ) ) > 0
      IF Left( LTrim( SubStr( sRule, nNext + 1 ) ), 1 ) == '>'
         nTokenLen := 1
         WHILE SubStr( sRule, nNext + nTokenLen, 1 ) != '>'
            nTokenLen++
         END
         nTokenLen++

         EXIT
      ENDIF
   ENDDO

   IF nNext > 0
      sResult := SubStr( sRule, nNext + nTokenLen )
      ExtractLeadingWS( @sResult )
      sRule   := Left( sRule, nNext - 1 )
   ELSEIF  bDelete
      //
   ELSE
      Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Missing => in #directive], { sRule } ) )
      // Safety
      BREAK
   ENDIF

   //TraceLog( sRule, sResult )

   DO WHILE ! Empty( sRule )
      //? "Scaning: " + sRule
      nMarkerAt   := AtInRules( '<', sRule )
      nOptionalAt := AtInRules( '[', sRule )

      IF nOptional != 0
         nCloseOptionalAt := AtInRules( ']', sRule )
      ELSE
         nCloseOptionalAt := 0
      ENDIF

      nAt := Max( Max( nMarkerAt, nOptionalAt ), nCloseOptionalAt )

      IF nMarkerAt > 0
         IF nMarkerAt <= nAt
            nAt := nMarkerAt
         ELSE
            nMarkerAt := 0
         ENDIF
      ENDIF

      IF nOptionalAt > 0
         IF nOptionalAt <= nAt
            nAt         := nOptionalAt
            nMarkerAt   := 0
         ELSE
            nOptionalAt := 0
         ENDIF
      ENDIF

      IF nCloseOptionalAt > 0
         IF nCloseOptionalAt <= nAt
            nAt         := nCloseOptionalAt
            nMarkerAt   := 0
            nOptionalAt := 0
         ELSE
            nCloseOptionalAt := 0
         ENDIF
      ENDIF

      //TraceLog( sRule, nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, nOptional )

      IF nAt == 0
         sTemp := sRule
         sRule := ""
      ELSE
         sTemp := Left( sRule, nAt - 1 )
         sRule := SubStr( sRule, nAt + 1 )
         /* Skip trailing spaces...*/
         ExtractLeadingWS( @sRule )
      ENDIF

      IF Empty( sTemp )
         sAnchor := NIL
      ELSE
         nLen := Len( sTemp )

         // Remove redundant Escape characters.
         FOR nAt := 1 TO nLen
            cChar := SubStr( sTemp, nAt, 1 )

            IF cChar $ ['"]
               WHILE ( nAt < nLen ) .AND. SubStr( sTemp, ++nAt, 1 ) != cChar
               END
            ELSEIF cChar == '\'
               sTemp := Left( sTemp, nAt - 1 ) + SubStr( sTemp, nAt + 1 )
               nLen--

               // Skip the next Escape if any, because we did not --nAt!
            ENDIF
         NEXT

         //TraceLog( sTemp )

         WHILE ( sToken := Upper( NextToken( @sTemp ) ), ! Empty( sTemp ) )
            //TraceLog( sToken )
            aMatch := { 0, nOptional, RTrim( sToken ), NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         END

         sAnchor := RTrim( sToken )

         IF nMarkerAt == 0
            aMatch := { 0, nOptional, sAnchor, NIL, NIL }
            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         ENDIF
      ENDIF

      IF nOptionalAt > 0
         nOptional := Abs( nOptional )
         nOptional++
         //? "Optional:", nOptional
      ELSEIF nCloseOptionalAt > 0
         IF nOptional > 0
            nOptional--
            nOptional := (-nOptional)
         ELSE
            nOptional++
         ENDIF
      ELSEIF nMarkerAt > 0
         //nId++
         nId := Len( aMarkers ) + 1

         DO CASE
            CASE SubStr( sRule, 1, 1 ) == '*'
               cType := '*'

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := AtInRules( '*', sRule )

               IF nNext > 1
                  sMarker := RTrim( Left( sRule, nNext - 1 ) )
                #ifdef __XHARBOUR__
                  IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
                #else
                  IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
                #endif
                     nId := nMarkerID
                  ELSE
                     aAdd( aMarkers, sMarker )
                  ENDIF

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<*'], { sRule } ) )
                     // Safety
                     BREAK
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  LOOP
               ELSE
                  Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<*'], { sRule } ) )
                  // Safety
                  BREAK
               ENDIF

            CASE SubStr( sRule, 1, 1 ) == '('
               cType := '('

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := AtInRules( ')', sRule )
               IF nNext > 1
                  sMarker := RTrim( Left( sRule, nNext - 1 ) )

                #ifdef __XHARBOUR__
                  IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
                #else
                  IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
                #endif
                     nId := nMarkerID
                  ELSE
                     aAdd( aMarkers, sMarker )
                  ENDIF

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<('], { sRule } ) )
                     // Safety
                     BREAK
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  LOOP
               ELSE
                  Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<('], { sRule } ) )
                  // Safety
                  BREAK
               ENDIF

            CASE SubStr( sRule, 1, 1 ) == '!'
               cType := '!'

               sRule := SubStr( sRule, 2 )
               ExtractLeadingWS( @sRule )

               nNext := AtInRules( '!', sRule )
               IF nNext > 1
                  sMarker := RTrim( Left( sRule, nNext - 1 ) )

                #ifdef __XHARBOUR__
                  IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
                #else
                  IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
                #endif
                     nId := nMarkerID
                  ELSE
                     aAdd( aMarkers, sMarker )
                  ENDIF

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )

                  IF Left( sRule, 1 ) == '>'
                     sRule := SubStr( sRule, 2 )
                     ExtractLeadingWS( @sRule )
                  ELSE
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<!'], { sRule } ) )
                     // Safety
                     BREAK
                  ENDIF

                  aMatch := { nId, nOptional, sAnchor, cType, NIL }
                  //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
                  aAdd( aRule[2], aMatch )

                  /* Next dependant optional will be marked as trailing. */
                  IF nOptional > 0
                     nOptional := ( -nOptional )
                  ENDIF

                  LOOP
               ELSE
                  Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<!'], { sRule } ) )
                  // Safety
                  BREAK
               ENDIF

            OTHERWISE
               cType := NIL // Reset - not known yet.
         ENDCASE

         nCloseAt := AtInRules( '>', sRule )
         nNext    := AtInRules( ',', sRule )

         IF nNext > 1 .AND. nNext < nCloseAt
            sDots := LTrim( SubStr( sRule, nNext + 1 ) )
            IF Left( sDots, 1 ) == '.'
               sDots := LTrim( SubStr( sDots, 2 ) )
               IF Left( sDots, 1 ) == '.'
                  sDots := LTrim( SubStr( sDots, 2 ) )
                  IF Left( sDots, 1 ) == '.'
                     sDots := LTrim( SubStr( sDots, 2 ) )
                     IF Left( sDots, 1 ) != '>'
                        nNext := 0
                     ENDIF
                  ELSE
                     nNext := 0
                  ENDIF
               ELSE
                  nNext := 0
               ENDIF
            ELSE
               nNext := 0
            ENDIF
         ENDIF

         IF nNext > 1 .AND. nNext < nCloseAt
            //? "Extended: '" + sRule + "'"
            cType := 'A'

            sMarker := RTrim( Left( sRule, nNext - 1 ) )
            ExtractLeadingWS( @sMarker )

          #ifdef __XHARBOUR__
            IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
          #else
            IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
          #endif
               nId := nMarkerID
            ELSE
               aAdd( aMarkers, sMarker )
            ENDIF

            sRule := sDots

            nNext    := 0
            nCloseAt := 1
         ELSE
            nNext := AtInRules( ':', sRule )
         ENDIF

         IF nNext > 0 .AND. nNext < nCloseAt
            cType := ':'

            //? "LIST"
            sMarker := RTrim( Left( sRule, nNext - 1 ) )
            ExtractLeadingWS( @sMarker )

          #ifdef __XHARBOUR__
            IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
          #else
            IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
          #endif
               nId := nMarkerID
            ELSE
               aAdd( aMarkers, sMarker )
            ENDIF

            sRule := SubStr( sRule, nNext + 1 )
            ExtractLeadingWS( @sRule )

            aWords := {}
            DO WHILE ! ( Left( sRule, 1 ) == '>' )
               nNext := AtInRules( ',', sRule )

               IF nNext > 0 .AND. nNext < AtInRules( '>', sRule )
                  sWord := Upper( RTrim( Left( sRule, nNext - 1 ) ) )

                  sWord := StrTran( sWord, '\\', '' )
                  sWord := StrTran( sWord, '\', '' )
                  sWord := StrTran( sWord, '', '\' )

                  //? "Added: " + sTemp
                  aAdd( aWords, sWord )

                  sRule := SubStr( sRule, nNext + 1 )
                  ExtractLeadingWS( @sRule )
                  LOOP
               ELSE
                  nCloseAt := AtInRules( '>', sRule )
                  IF nCloseAt > 0
                     //? "Last: " + Left( sRule, nCloseAt - 1 )
                     aAdd( aWords, Upper( RTrim( Left( sRule, nCloseAt - 1 ) ) ) )
                     EXIT
                  ELSE
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<:'], { sRule } ) )
                     // Safety
                     BREAK
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF nCloseAt > 0
            IF cType == NIL
               cType := '<'
            ENDIF

            IF Len( aMarkers ) < nId
               sMarker := RTrim( Left( sRule, nCloseAt - 1 ) )
               ExtractLeadingWS( @sMarker )

             #ifdef __XHARBOUR__
               IF ( nMarkerID := aScan( aMarkers, sMarker, , , .T. ) ) > 0
             #else
               IF ( nMarkerID := aScan( aMarkers, {|s| s == sMarker } ) ) > 0
             #endif
                  nId := nMarkerID
               ELSE
                  aAdd( aMarkers, sMarker )
               ENDIF
            ENDIF

            sRule := SubStr( sRule, nCloseAt + 1 )
            ExtractLeadingWS( @sRule )

            aMatch := { nId, nOptional, sAnchor, cType, aWords }


            aWords := NIL // Reset.

            //? aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5]
            aAdd( aRule[2], aMatch )

            /* Next dependant optional will be marked as trailing. */
            IF nOptional > 0
               nOptional := ( -nOptional )
            ENDIF
         ELSE
            Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unblanced MP: '<'], { sRule } ) )
            // Safety
            BREAK
         ENDIF
      ELSE
         //
      ENDIF

   ENDDO

   IF nOptional <> 0
      //TraceLog( "ERROR Unclose Optional group, nOptional = " + Str( nOptional, 3 ), aMatch[1], aMatch[2], aMatch[3], aMatch[4], aMatch[5] )
      Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unclosed Optional group], { sRule, nOptional } ) )
      // Safety
      BREAK
   ENDIF

   IF bDelete
     #ifdef __XHARBOUR__
      IF bX
         // Discard the bX flag (should not be included in the match logic)
         cRuleExp := ValToPrgExp( aSize( aClone( aRule ), Len( aRule ) - 1 ) )
      ENDIF

      nRules := Len( aRules )

      FOR nRule := nRules TO 1 STEP -1
         aMatchRule := aRules[ nRule ]

         IF aMatchRule[1] == sKey
            // Discard the bX flag (should not be included in the match logic)
            IF ( ! bX ) .OR. ValToPrgExp( aSize( aClone( aMatchRule ), Len( aMatchRule ) - 1 ) ) == cRuleExp
               aDel( aRules, nRule, .T. )
               aDel( aResults, nRule, .T. )
               RETURN
            ENDIF
         ENDIF
      NEXT

      RETURN
     #endif
   ELSE
      aAdd( aRules, aRule )
   ENDIF

   /*
   nMarkerID
   nOPTIONAL
   sAnchor
   cTYPE
   aLIST
   */

   // *** Processing STOP Words below!

   /*
   ? ''
   FOR Counter := 1 TO nId
      ?? aMarkers[Counter]
      IF Counter < nId
         ?? ' , '
      ENDIF
   NEXT

   nMatches := Len( aRule[2] )
   FOR Counter := 1 TO nMatches
      ? aRule[2][Counter][1], aRule[2][Counter][2], aRule[2][Counter][3], aRule[2][Counter][4], aRule[2][Counter][5]
   NEXT
   WAIT
   */

   /* --------------------------------------------------------------- */

   //? [RP: "] + sResult + '"'

   //TraceLog( sResult )

   nOptional     := 0
   aModifiers    := {}
   nId           := 0
   sPad          := ''

   DO WHILE ! ( sResult == '' )
      nOptionalAt := AtInRules( '[', sResult )

      nMarkerAt := AtInRules( '<', sResult )

      IF nOptionalAt != 0
         IF nMarkerAt > 0
            IF nMarkerAt > nOptionalAt
               nMarkerAt := 0
            ELSE
               nOptionalAt := 0
            ENDIF
         ELSE
            TraceLog( "Warning, no markers in repeatable group: " + SubStr( sResult, nOptionalAt ) )

            nCloseOptionalAt := AtInRules( ']', sResult, nOptionalAt )

            IF nCloseOptionalAt > 0
               //TraceLog( "Skipped: " + SubStr( sResult, nOptionalAt, nCloseOptionalAt - nOptionalAt + 1 ) )
               sResult := Left( sResult, nOptionalAt - 1 ) + SubStr( sResult, nCloseOptionalAt + 1 )
               LOOP
            ELSE
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unclosed repeatable group], { sResult, nOptionalAt } ) )
               // Safety
               BREAK
            ENDIF
         ENDIF
      ENDIF

      nAt := IIF( nOptionalAt == 0, nMarkerAt, nOptionalAt )

      IF nOptional == 0
         nCloseOptionalAt := 0
      ELSE
         IF nAt == 0
            nCloseOptionalAt := AtInRules( ']', sResult )

            IF nCloseOptionalAt == 0
               //TraceLog( "RP Scan:", nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, sResult )
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unclosed repeatable group], { sResult, nOptionalAt } ) )
               // Safety
               BREAK
            ENDIF
         ELSE
            nCloseOptionalAt := AtInRules( ']', sResult )

            IF nCloseOptionalAt > 0
               IF nCloseOptionalAt > nAt
                  nCloseOptionalAt := 0
               ELSE
                  nAt         := 0
                  nOptionalAt := 0
                  nMarkerAt   := 0
               ENDIF
            ELSE
               //TraceLog( "RP Scan:", nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, sResult )
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unclosed repeatable group], { sResult, nOptionalAt } ) )
               // Safety
               BREAK
            ENDIF
         ENDIF
      ENDIF

      //? "RP Scan:", nAt, nMarkerAt, nOptionalAt, nCloseOptionalAt, sResult
      //WAIT

      IF nCloseOptionalAt > 0
         IF nCloseOptionalAt > 1
            sTemp := Left( sResult, nCloseOptionalAt - 1 )
            aRP := { nOptional, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         nOptional := 0 //--
         sResult := SubStr( sResult, nCloseOptionalAt + 1 )
         ExtractLeadingWS( @sResult, @sPad )
         LOOP
      ENDIF

      IF nOptionalAt > 0

         IF nOptional <> 0
            Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Nested repeatable group], { sResult } ) )
            // Safety
            BREAK
         ELSE
            nOptional := -1
         ENDIF

         /* The text preceding this new repeatable group. */
         IF nOptionalAt > 1
            sTemp := Left( sResult, nOptionalAt - 1 )
            aRP := { 0, sPad + sTemp }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ELSE
            aRP := { 0, "" }
            aAdd( aResult, aRP )
            aAdd( aModifiers, -1 )
         ENDIF

         sResult := SubStr( sResult, nOptionalAt + 1 )
         ExtractLeadingWS( @sResult, @sPad )
         LOOP

      ELSEIF nMarkerAt > 0

         /* Resetting. */
         nType := 0

         IF nMarkerAt == 1 .OR. ( nMarkerAt == 2 .AND. Left( sResult, 1 ) == '#' )
            /* I consider this a Clipper bug - it produces .ppo without the padding if none suplied,
               but treats it as if padding existed! - so at least we will generate the space. */

            //IF /*Len( aResult ) > 0 .AND. ( ValType( aTail( aResult )[2] ) == 'N' .OR. aTail( aResult )[2] == "" ) .AND.*/ Len( sPad ) > 0
            IF bStrict
               aRP := { nOptional, " " }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ELSEIF Len( sPad ) > 0
               aRP := { nOptional, sPad }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ENDIF
         ENDIF

         IF nMarkerAt > 1
            sTemp := RTrim( Left( sResult, nMarkerAt - 1 ) )
            IF Right( sTemp, 1 ) == '#'
               nType := 2
               IF nMarkerAt > 2
                  sTemp := Left( sTemp, Len( sTemp ) - 1 )
                  aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, -1 )
               ENDIF
            ELSE
               sTemp := Left( sResult, nMarkerAt - 1 )
               aRP := { nOptional, sPad + DropExtraTrailingWS( sTemp ) }
               aAdd( aResult, aRP )
               aAdd( aModifiers, -1 )
            ENDIF
         ENDIF

         sResult := SubStr( sResult, nMarkerAt + 1 )
         ExtractLeadingWS( @sResult )

         /* <-x-> Ommit */
         IF SubStr( sResult, 1, 1 ) == '-'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := AtInRules( ">", sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<-'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '-'
                  nType := 0
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )

                #ifdef __XHARBOUR__
                  nId := aScan( aMarkers, sTemp, , , .T. )
                #else
                  nId := aScan( aMarkers, {|s| s == sTemp } )
                #endif

                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecognized RP '<-'], { sResult, sTemp } ) )
                     // Safety
                     BREAK
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* #<x> Dumb */
         ELSEIF nType == 2

            nNext := AtInRules( '>', sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '#<'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               /*nType := 2*/
               sTemp := RTrim( Left( sResult, nNext - 1 ) )

             #ifdef __XHARBOUR__
               nId := aScan( aMarkers, sTemp, , , .T. )
             #else
               nId := aScan( aMarkers, {|s| s == sTemp } )
             #endif

               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecognized RP '<-'], { sResult, sTemp } ) )
                  // Safety
                  BREAK
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         /* <"x"> Normal */
         ELSEIF SubStr( sResult, 1, 1 ) == '"'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := At( ">", sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<"'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '"'
                  nType := 3
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )

                #ifdef __XHARBOUR__
                  nId := aScan( aMarkers, sTemp, , , .T. )
                #else
                  nId := aScan( aMarkers, {|s| s == sTemp } )
                #endif

                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecgnized RP '<"'], { sResult, sTemp } ) )
                     // Safety
                     BREAK
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <(x)> Smart */
         ELSEIF SubStr( sResult, 1, 1 ) == '('

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := AtInRules( ">", sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<('], { sResult } ) )
               // Safety
               BREAK
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == ')'
                  nType := 4
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )

                #ifdef __XHARBOUR__
                  nId := aScan( aMarkers, sTemp, , , .T. )
                #else
                  nId := aScan( aMarkers, {|s| s == sTemp } )
                #endif

                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecognized RP '<('], { sResult, sTemp } ) )
                     // Safety
                     BREAK
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <{x}> Blockify */
         ELSEIF SubStr( sResult, 1, 1 ) == '{'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := AtInRules( ">", sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<{'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '}'
                  nType := 5
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )

                #ifdef __XHARBOUR__
                  nId := aScan( aMarkers, sTemp, , , .T. )
                #else
                  nId := aScan( aMarkers, {|s| s == sTemp } )
                #endif

                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unreconized RP '<{'], { sResult, sTemp } ) )
                     // Safety
                     BREAK
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         /* <.x.> Logify */
         ELSEIF SubStr( sResult, 1, 1 ) == '.'

            sResult := SubStr( sResult, 2 )
            ExtractLeadingWS( @sResult )

            nNext := AtInRules( ">", sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<.'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               sTemp := RTrim( Left( sResult, nNext - 1 ) )
               IF Right( sTemp, 1 ) == '.'
                  nType := 6
                  sTemp := RTrim( Left( sTemp, Len( sTemp ) - 1 ) )

                #ifdef __XHARBOUR__
                  nId := aScan( aMarkers, sTemp, , , .T. )
                #else
                  nId := aScan( aMarkers, {|s| s == sTemp } )
                #endif

                  sResult := SubStr( sResult, nNext + 1 )
                  ExtractLeadingWS( @sResult, @sPad )
                  IF nId == 0
                     Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecognized RP '<.'], { sResult, sTemp } ) )
                     // Safety
                     BREAK
                  ELSE
                     aRP := { nOptional, nId }
                     aAdd( aResult, aRP )
                     aAdd( aModifiers, nType )
                  ENDIF
               ENDIF
            ENDIF

         ELSE

            nNext := AtInRules( '>', sResult )
            IF nNext == 0
               Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unbalanced RP '<'], { sResult } ) )
               // Safety
               BREAK
            ELSE
               /* <x> Regular */
               nType := 1
               sTemp := Left( sResult, nNext - 1 )

             #ifdef __XHARBOUR__
               nId := aScan( aMarkers, sTemp, , , .T. )
             #else
               nId := aScan( aMarkers, {|s| s == sTemp } )
             #endif

               sResult := SubStr( sResult, nNext + 1 )
               ExtractLeadingWS( @sResult, @sPad )
               IF nId == 0
                  //aEval( aMarkers, {|sMarker| TraceLog( sResult, sTemp, sMarker ) } )
                  Eval( s_bRTEBlock, ErrorNew( [PP], 0, 2059, [Compile-Rule], [Unrecognized RP '<'], { sResult, sTemp } ) )
                  // Safety
                  BREAK
               ELSE
                  aRP := { nOptional, nId }
                  aAdd( aResult, aRP )
                  aAdd( aModifiers, nType )
               ENDIF
            ENDIF

         ENDIF

         //? "RP #", nID, "Optional:", nOptional

         IF nOptional < 0
            nOptional := nID
            aRP[1]    := nOptional
         ENDIF

      ELSE

         aRP := { 0, sPad + sResult }
         aAdd( aResult, aRP )
         aAdd( aModifiers, -1 )
         sResult := ''

      ENDIF

   ENDDO

   IF nOptional <> 0
      //TraceLog( "ERROR! Internal logic failure, nOptional = " + Str( nOptional, 3 ) + " [" + Str( ProcLine(0), 4 ) + "]", aRP[1], aRP[2] )
      Eval( ErrorBlock(), ErrorNew( [PP], 0, 9003, [Compile-Rule], [Internal logic failure], { sResult, nOptional } ) )
      BREAK
   ENDIF


   /* Processing Repeatable Flag of Match Markers. */
   /* Note additional correction done in subsequent processing of STOP Words, below... */
   nResults := Len( aResult )
   FOR Counter := nResults TO 1 STEP -1
      aRP := aResult[Counter]

      /* Correcting the ID of the Marker this result depends upon. */
      IF aRP[1] > 0
         nOptional := aRP[1]
         nMarker   := aRP[2]

         //? "Repeatable: ", nMarker, "Root: ", nOptional

         IF ValType( nMarker ) == 'N'
            nTempMP := 0

            WHILE ( nTempMP := aScan( aRule[2], {|aMP| aMP[1] == nMarker .OR. aMP[1] - 1000 == nMarker }, nTempMP + 1 ) ) > 0
               nMP := nTempMP

               // Clipper compatible - only repeatable if explictly used as repeatable in result, or is missing in result!
               #ifdef _0_
                  WHILE aRule[2][nMP][2] < 0
                     IF aRule[2][nMP][1] >= 0

                        /* Mark as Repeatable. */
                        IF aRule[2][nMP][1] < 1000
                           aRule[2][nMP][1] += 1000
                           //? "   Flagged:", nMP, "As:", aRule[2][nMP][1]
                        ENDIF
                     ENDIF

                     nMP--
                  ENDDO
               #endif

               IF aRule[2][nMP][2] == 0
                  TraceLog( "Warning - Result #" + Str( Counter ) + " marked REPEATABLE but root #" + Str( nMarker ) + " is not OPTIONAL!", sRuleCopy )
                  //aRule[2][nMP][2] := 1
               ELSEIF aRule[2][nMP][1] < 1000
                  aRule[2][nMP][1] += 1000
                  //? "Flagged:", nMP, "As:", aRule[2][nMP][1]
               ENDIF
            ENDDO
            //WAIT
         ENDIF
      ELSEIF aRP[1] < 0
         aRP[1] := nOptional
      ENDIF

      IF HB_ISSTRING( aRP[2] )
         aRP[2] := StrTran( aRP[2], '\\', '' )
         aRP[2] := StrTran( aRP[2], '\', '' )
         aRP[2] := StrTran( aRP[2], '', '\' )
         //? "RP #", Counter, aRP[1], '"' + aRP[2] + '"'
      ELSE
         //? "RP #", Counter, aRP[1], aRP[2]
      ENDIF
   NEXT

   //WAIT

   // Processing STOP words for NON Anchored optionals.
   nLastOptional := 0
   nPending      := 0
   nMatches      := Len( aRule[2] )

   FOR Counter := 1 TO nMatches
      aMatch := aRule[2][Counter]

      /* If optional, which is *not* used as a result, Clipper makes it repeatable. */
      IF aMatch[1] < 1000 .AND. aMatch[1] > 0 .AND. aMatch[2] > 0
         IF aScan( aResult, { |aRP| ValType( aRP[2] ) == 'N' .AND. aRP[2] == aMatch[1] } ) == 0
            TraceLog( "Warning - Marker #" + Str( aMatch[1] ) + " not utilized in Result Rule", sRuleCopy )
            aMatch[1] += 1000
         ENDIF
      ENDIF

      /* Optional group start (marker), no anchor, and not a restricted pattern - have to build stop words list! */
      IF aMatch[1] > 0 .AND. aMatch[2] > 0 .AND. aMatch[3] == NIL .AND. aMatch[4] != ':'

         aWords        := {}
         nOptional     := aMatch[2]
         nLastOptional :=  nOptional

         /*
         nMP := Counter - 1
         WHILE nMP > 0
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP--
         ENDDO
         */

         nMP := Counter + 1
         WHILE nMP <= nMatches
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP++
         ENDDO

         IF Len( aWords ) > 0
            aRule[2][Counter][5] := aWords
         ENDIF

      ELSEIF aMatch[2] > 0 .AND. aMatch[2] - nLastOptional == 2 // Head of new group missing because nested optional is first element.
         nPending := - ( aMatch[2] - 1 )

      ELSEIF nPending != 0 .AND. aMatch[1] > 0 .AND. aMatch[2] == nPending .AND. aMatch[3] == NIL .AND. aMatch[4] != ':'
         nPending      := 0
         aWords        := {}
         nOptional     := -aMatch[2]
         nLastOptional :=  nOptional

         nMP := Counter - 1
         WHILE nMP > 0
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP--
         ENDDO

         nMP := Counter + 1
         WHILE nMP <= nMatches
            aMatch := aRule[2][nMP]
            IF aMatch[2] >= 0 .AND. aMatch[2] < nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
               EXIT
            ENDIF
            IF aMatch[2] > 0 .AND. aMatch[2] == nOptional
               IF aMatch[3] != NIL
                  aAdd( aWords, Upper( aMatch[3] ) )
               ELSEIF aMatch[4] == ':'
                  nWords := Len( aMatch[5] )
                  FOR nWord := 1 TO nWords
                     aAdd( aWords, aMatch[5][nWord] )
                  NEXT
               ENDIF
            ENDIF
            nMP++
         ENDDO

         IF Len( aWords ) > 0
            aRule[2][Counter][5] := aWords
         ENDIF

      ENDIF

      //? aRule[1], aRule[2][Counter][1], aRule[2][Counter][2], aRule[2][Counter][3], aRule[2][Counter][4], aRule[2][Counter][5]
   NEXT
   //WAIT

   aAdd( aResults, { aResult, aModifiers, Array( Len( aMarkers ) ) } )

   //TraceLog( "Finished" )

RETURN

//--------------------------------------------------------------//

STATIC FUNCTION RemoveDefine( sDefine )

   LOCAL nId, nLen

   sDefine := AllTrim( sDefine )

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } ) ) > 0
      aDel( aDefRules, nId )
      aSize( aDefRules, ( nLen := Len( aDefRules ) - 1 ) )
      aDel( aDefResults, nId )
      aSize( aDefResults, nLen )
   ENDIF

RETURN nId

//--------------------------------------------------------------//

STATIC FUNCTION CompileDefine( sRule )

   LOCAL sKey, sResult, aRule, nCloseAt, nId, sMarker, nCommaAt, aMP
   LOCAL sToken, aRPs, sAnchor, aMarkers := {}, aResult, sPad, sText := ""

   ExtractLeadingWS( @sRule )

   sKey := NextToken( @sRule )

   // TraceLog( sKey, sRule )
   // ? "KEY: '" + sKey + "'"
   // ? "Rest: '" + sRule + "'"

   DropTrailingWS( @sKey, @sPad )

   IF ( nId := aScan( aDefRules, {|aDefine| aDefine[1] == sKey } ) ) > 0
      PP_Warning( [Redefinition of ] + "'" + sKey + "'" + [ in file: ] + s_sFile )

      aRule      := aDefRules[nId]
      //aRule[1]   := sKey
      aRule[2]   := {}
      aResult    := aDefResults[nId]
      aResult[1] := {} // aMPs
      aResult[2] := {} // aModifiers
      aResult[3] := {} // Markers place holders
   ELSE
      aRule   := { sKey, {}, .T. }
      aAdd( aDefRules, aRule )
      aResult := { {}, {}, {} } //1=MPs, 2=Modifiers 3=Marker place holders
      aAdd( aDefResults, aResult )
   ENDIF

   IF sPad == "" .AND. Left( sRule, 1 ) == '(' .AND. ( nCloseAt := AtInRules( ')', sRule ) ) > 0

      /*Pseudo Function. */
      sResult := SubStr( sRule, nCloseAt + 1 )
      sRule   := SubStr( sRule, 2, nCloseAt - 2 )
      ExtractLeadingWS( @sRule )
      DropTrailingWS( @sRule )
      ExtractLeadingWS( @sResult )

      /* No paramaets */
      IF sRule == ''

//? "Added: '" + aRule[1] + "'"
//WAIT
         aAdd( aRule[2], { 0, 0 , '(', NIL, NIL } )
         aAdd( aRule[2], { 0, 0 , ')', NIL, NIL } )

         IF sResult == ''
            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := NIL
         ELSE
            aResult[1] := { { 0, sResult } }
         ENDIF
      ELSE

//? "***'" + sRule + "'"
//WAIT
         nId      := 1
         sAnchor  := '('

         WHILE ( nCommaAt := AtInRules( ',', sRule ) ) > 0
            sMarker := Left( sRule, nCommaAt - 1 )
            sRule   := SubStr( sRule, nCommaAt + 1 )
            ExtractLeadingWS( @sRule )
            DropTrailingWS( @sMarker )

//? nId, "Marker: '" + sMarker + "'"
//WAIT
            aAdd( aMarkers, sMarker )
            aMP := { nId, 0, sAnchor, '<', NIL }
            aAdd( aRule[2], aMP )

            sAnchor := ','
            nId++
         ENDDO

         aAdd( aMarkers, sRule )
         aMP := { nId, 0, sAnchor, '<', NIL }
         aAdd( aRule[2], aMP )

         aMP := { 0, 0, ')', NIL, NIL }
         aAdd( aRule[2], aMP )

         /*----------------------------------------- */

         aRPs := {}

         IF sResult == ''

            aResult[1] := NIL
            aResult[2] := NIL
            aResult[3] := Array( Len( aMarkers ) )

         ELSE

            WHILE ( sToken := NextToken( @sResult ) ) != NIL
               DropTrailingWS( @sToken, @sPad )

//? "Token: '" + sToken + "'"
            #ifdef __XHARBOUR__
               IF ( nId := aScan( aMarkers, sToken, , , .T. ) ) > 0
            #else
               IF ( nId := aScan( aMarkers, {|sMarker| sMarker == sToken } ) ) > 0
            #endif
                  IF ! ( sText == "" )
                     aAdd( aRPs, { 0, sText } )
                     aAdd( aResult[2], -1 )
                     sText := sPad
                  ENDIF

                  aAdd( aRPs, { 0, nId } )
                  aAdd( aResult[2], 1 )
               ELSE
                  sText += sToken + sPad
               ENDIF

//? "ID:", nID
//WAIT
            ENDDO

            IF ! ( sText == "" )
               aAdd( aRPs, { 0, sText } )
               aAdd( aResult[2], -1 )
            ENDIF

            aResult[1] := aRPs
            aSize( aResult[3], Len( aMarkers ) )
            aFill( aResult[3], NIL )

         ENDIF

      ENDIF

   ELSE

      /* Plain. */

      sResult := sRule

      IF sResult == ''
         aResult[1] := NIL
         aResult[2] := NIL
         aResult[3] := NIL
      ELSE
         aResult[1] := { { 0, sResult } }
         aResult[2] := { -1 }
         aResult[3] := NIL
      ENDIF

   ENDIF

//? "Defines: ", Len( aDefRules )
//? "Results: ", Len( aDefResults )
//WAIT

RETURN Len( aDefRules )

//--------------------------------------------------------------//

#ifndef USE_C_BOOST

   //--------------------------------------------------------------//

   STATIC FUNCTION NextToken( sLine, lDontRecord )

      LOCAL sReturn, Counter, nLen, nClose
      LOCAL s1, s2, s3
      LOCAL sDigits
      LOCAL sToken

      //TraceLog( sLine, lDontRecord )

      IF Empty( sLine )
         RETURN NIL
      ENDIF

      IF Empty( lDontRecord )
         lDontRecord := .F.
      ENDIF

      // *** To be removed after final testing !!!
      sLine := LTrim( sLine )

      nLen := Len( sLine )
      s1 := Left( sLine, 1 )

      BEGIN SEQUENCE

         IF nLen >= 2

            s2 := Left( sLine, 2 )

            IF s2 $ "++;--;->;:=;==;!=;<>;>=;<=;+=;-=;*=;^=;**;/=;%=;=>;^^;<<;>>"

               sReturn := s2

               BREAK

            ELSEIF s2 == "[["

               nClose := AT( ']]', sLine )
               IF nClose == 0
                  //Alert( "ERROR! [NextToken()] Unterminated '[[' at: " + sLine + "[" + Str( ProcLine() ) + "]" )
                  sReturn := "["  // Clipper does NOT consider '[[' a single token
               ELSE
                  sReturn := Left( sLine, nClose + 2 )
               ENDIF

               BREAK

            ELSEIF s2 $ "0x\0X"
               sReturn := s2

               FOR Counter := 3 TO nLen
                  s1 := SubStr( sLine, Counter, 1 )
                  IF ! ( IsDigit( s1 ) .OR. s1 $ "abcdefABCDEF" )
                     EXIT
                  ENDIF
                  sReturn += s1
               NEXT

               BREAK

            ENDIF

         ENDIF

         IF IsAlpha( s1 ) .OR. s1 == '_'

            sReturn := s1
            FOR Counter := 2 TO nLen
               s1 := SubStr( sLine, Counter, 1 )
               IF ! ( IsAlpha( s1 ) .OR. IsDigit( s1 ) .OR. s1 == "_" )
                  EXIT
               ENDIF
               sReturn += s1
            NEXT

            BREAK

         ELSEIF IsDigit( s1 )

            sReturn := s1
            FOR Counter := 2 TO nLen
               s1 := SubStr( sLine, Counter, 1 )
               IF ! ( IsDigit( s1 ) )
                  EXIT
               ENDIF
               sReturn += s1
            NEXT

            // Consume the point (and subsequent digits) only if digits follow...
            IF s1 == '.'
               sDigits := ""
               DO WHILE IsDigit( ( s1 := SubStr( sLine, ++Counter, 1 ) ) )
                  sDigits += s1
               ENDDO

               IF ! ( sDigits == "" )
                  sReturn += ( '.' + sDigits )
               ENDIF
            ENDIF

            // Either way we are done.
            BREAK

         ELSEIF s1 == '.'

            sDigits := ""
            FOR Counter := 2 TO nLen
               s1 := SubStr( sLine, Counter, 1 )
               IF ! ( IsDigit( s1 ) )
                  EXIT
               ENDIF

               sDigits += s1
            NEXT

            // Must have accumulated decimal digits.
            IF ! ( sDigits == "" )
               sReturn := '.' + sDigits

               BREAK
            ENDIF

            IF nLen >= 5 .AND. SubStr( sLine, 5, 1 ) == '.'

               s3 := Upper( SubStr( sLine, 2, 3 ) )
               IF s3 == 'AND'

                  sReturn := ".AND."

                  BREAK

               ELSEIF s3 == 'NOT'

                  sReturn := "!"
                  /* Skip the unaccounted letters ( .NOT. <-> ! ) */
                  sLine := SubStr( sLine, 5 )

                  BREAK

               ENDIF

            ENDIF

            IF nLen >= 4 .AND. SubStr( sLine, 4, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 2 ) ) == 'OR'

               sReturn := ".OR."

               BREAK

            ENDIF

            IF nLen >= 3 .AND. SubStr( sLine, 3, 1 ) == '.' .AND. Upper( SubStr( sLine, 2, 1 ) ) $ "TF"

               sReturn := Upper( Left( sLine, 3 ) )

               BREAK

            ENDIF

            sReturn := '.'

            BREAK

         ELSEIF s1 == '"'

            nClose := AT( '"', sLine, 2 )
            IF nClose == 0
               //Alert( 'ERROR! [NextToken()] Unterminated ["] at: ' + sLine )
               sReturn := '"'
            ELSE
               sReturn := Left( sLine, nClose )
            ENDIF

            BREAK

         ELSEIF s1 == "'"

            nClose := AT( "'", sLine, 2 )
            IF nClose == 0
               //Alert( "ERROR! [NextToken()] Unterminated ['] at: " + sLine )
               sReturn := "'"
            ELSE
               sReturn := SubStr( sLine, 2, nClose - 2 )
               IF ! ( '"' $ sReturn )
                  sReturn := '"' + sReturn + '"'
               ELSE
                  sReturn := "'" + sReturn + "'"
               ENDIF
            ENDIF

            BREAK

         ELSEIF s1 == '['

            IF s_bArrayPrefix
               sReturn := '['
            ELSE
               nClose := AT( ']', sLine )
               IF nClose == 0
                  //Alert( "ERROR! [NextToken()] Unterminated '[' at: " + sLine + "[" + Str( ProcLine() ) + "]" )
                  sReturn := '['
               ELSE
                  sReturn := SubStr( sLine, 2, nClose - 2 )
                  IF ! ( '"' $ sReturn )
                     sReturn := '"' + sReturn + '"'
                  ELSEIF ! ( "'" $ sReturn )
                     sReturn := "'" + sReturn + "'"
                  ELSE
                     sReturn := '[' + sReturn + ']'
                  ENDIF
               ENDIF
            ENDIF

            BREAK

         ELSEIF s1 $ "+-*/:=^!&()[]{}@,|<>#%?$~\"

            sReturn := s1

            BREAK

         ELSE

            TraceLog( "Unexpected case: ", sLine )
            Eval( ErrorBlock(), ErrorNew( [PP], 0, 3010, [Next-Token], [Unexpected case], { sLine } ) )
            sReturn := sLine

         ENDIF

      END SEQUENCE

      sLine := SubStr( sLine, Len( sReturn ) + 1 )

      IF lDontRecord == .F.
         IF Left( sReturn, 1 ) == '.' .AND. Len( sReturn ) > 1 .AND. Right( sReturn, 1 ) == '.'
            s_bArrayPrefix := .F.
         ELSE
            s1             := Right( sReturn, 1 )

            IF Upper( s1 ) == 'R'
               sToken := Upper( sReturn )
               IF sToken == "RETU" .OR. sToken == "RETUR" .OR. sToken == "RETURN"
                  s_bArrayPrefix := .F.
               ELSE
                  s_bArrayPrefix := .T.
               ENDIF
            ELSE
               s_bArrayPrefix := ( IsAlpha( s1 ) .OR. IsDigit( s1 ) .OR. s1 $ "])}._" )
            ENDIF
         ENDIF
      ENDIF

      sReturn += ExtractLeadingWS( @sLine )

      #ifdef PP_RECURSIVE

         IF s_bRecursive
            s1 := Left( sReturn, 1 )
            IF ( IsAlpha( s1 ) .OR. s1 == '_' ) .AND. MatchRule( sReturn, @sLine, aDefRules, aDefResults, .F., .F. ) > 0
               RETURN NextToken( @sLine, .T. )
            ENDIF

            IF MatchRule( sReturn, @sLine, aTransRules, aTransResults, .F., .T. ) > 0
               //? '>', sLine, '<'
               RETURN NextToken( @sLine, .T. )
            ENDIF

            //? sReturn, "not defined/translated."
            //WAIT
         ENDIF

      #endif

      //TraceLog( "TOKEN = >" + sReturn + "<", sLine, s_bArrayPrefix )

   RETURN sReturn

   //--------------------------------------------------------------//

   STATIC FUNCTION NextIdentifier( sLine, sSkipped )

      LOCAL nAt, nLen := Len( sLine ), cChar, cLastChar := '0', nStart, sIdentifier, sTmp

      FOR nAt := 1 TO nLen
          cChar := SubStr( sLine, nAt, 1 )

          IF cChar $ ' ,([{|^*/+-%=!#<>:&$'
             IF nStart != NIL
                EXIT
             ENDIF
             LOOP // No need to record cLastChar
          ELSEIF cChar $ ')]}'
             IF nStart != NIL
                EXIT
             ENDIF
          ELSEIF cChar $ '"'+"'"
             DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
             ENDDO
             LOOP // No need to record cLastChar
          ELSEIF cChar == '['
             IF ! ( IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "])}_." )
                DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != ']'
                ENDDO
             ENDIF
             cLastChar := ']'
             LOOP // Recorded cLastChar
          ELSEIF cChar == '.'
             IF cLastChar == '_' .OR. IsAlpha( cLastChar )
                EXIT
             ENDIF

             sTmp := Upper( SubStr( sLine, nAt + 1, 4 ) )
             IF sTmp = "T."
                nAt += 2
                LOOP
             ELSEIF sTmp = "F."
                nAt += 2
                LOOP
             ELSEIF sTmp = "OR."
                nAt += 3
                LOOP
             ELSEIF sTmp = "AND."
                nAt += 4
                LOOP
             ELSEIF sTmp = "NOT."
                nAt += 4
                LOOP
             ENDIF
          ELSEIF nStart == NIL .AND. ( IsAlpha( cChar ) .OR. cChar == '_' )
             nStart := nAt
          ENDIF

          cLastChar := cChar
       NEXT

       IF nStart != NIL
          sIdentifier := SubStr( sLine, nStart, nAt - nStart )
          sSkipped    := Left( sLine, nStart - 1 )
          sLine       := SubStr( sLine, nAt )
       ENDIF

       //TraceLog( sIdentifier, sLine, sSkipped, cChar, cLastChar, nStart, nAt, nLen )

   RETURN sIdentifier

   //--------------------------------------------------------------//

   STATIC FUNCTION ExtractLeadingWS( sLine, sWS )

      LOCAL Counter, cChar, nLen := Len( sLine )

      //? "Removing Leading: '" + sLine + "'"

      sWS := ''
      FOR Counter := 1 TO nLen
         cChar := SubStr( sLine, Counter, 1 )
         IF cChar == ' ' //$ ( ' ' + Chr(9) ) // Tabs converted to spaces
            sWS += cChar
         ELSE
            EXIT
         ENDIF
      NEXT

      IF Counter > 1
         sLine := SubStr( sLine, Counter )
      ENDIF

      //? "Removed: '" + sWs + "' sLine: " + sLine

   RETURN sWS

   //--------------------------------------------------------------//

   STATIC FUNCTION DropTrailingWS( sLine, sWS )

      LOCAL nLenSource, nLen := Len( sLine ), cChar

      nLenSource := nLen

      //? "Before Drop: '" + sLine + "'"

      /* Tabs are converted to spaces at PP_PreProFile() */

      WHILE nLen > 0 .AND. ( cChar := SubStr( sLine, nLen, 1 ) ) == ' ' //$ ( ' ' + Chr(9) ) // Tabs converted to spaces
         nLen--
      ENDDO

      sLine := Left( sLine, nLen )
      sWS   := Space( nLenSource - nLen )

      //? "After Drop: '" + sLine + "'"

   RETURN sLine

   //--------------------------------------------------------------//

   STATIC FUNCTION DropExtraTrailingWS( sLine )

      LOCAL nLen := Len( sLine )
      /* Tabs are converted to spaces at PP_PreProFile() */

      //? "Before Extra: '" + sLine + "'"

      WHILE nLen > 2 .AND. ( SubStr( sLine, nLen, 1 ) == ' ' /* $ ( ' ' + Chr(9) ) */ ) .AND. ;
                           ( SubStr( sLine, nLen - 1, 1 ) == ' ' ) //$ ( ' ' + Chr(9) ) )
         nLen--
      ENDDO

      sLine := Left( sLine, nLen )

   RETURN sLine

   //--------------------------------------------------------------//

#endif

//--------------------------------------------------------------//

STATIC FUNCTION SetIfDef( sDefine, bExist )

   LOCAL nId

   nIfDef++
   aSize( abIfDef, nIfDef )

   DropTrailingWS( @sDefine )

   nId := aScan( aDefRules, {|aDefine| aDefine[1] == sDefine } )
   IF bExist
      abIfDef[nIfDef] := ( nId > 0 )
   ELSE
      abIfDef[nIfDef] := ( nId == 0 )
   ENDIF

   //? nIfDef, nId, sDefine, abIfDef[nIfDef]

RETURN nIfDef

//--------------------------------------------------------------//

STATIC FUNCTION CompileToCCH( sSource )

   LOCAL hCCH, Counter, aRules, nRules, nRule, aRule, nMatches, nMatch, aMatch, nWords, nWord, aWords
   LOCAL aResults, nResults, nResult, aResult, nRPs, nRP, aRP, nIDs, nID, nModifier
   LOCAL sRulesArray, sResultsArray, sExt

   sExt := SubStr( sSource, RAt( '.', sSource ) )
   IF ! ( sExt == '' )
      hCCH   := FCreate( StrTran( sSource, sExt, ".cch" ) )
   ELSE
      hCCH   := FCreate( sSource + ".cch" )
   ENDIF

   FWrite( hCCH, "FUNCTION InitRules()" + CRLF )

   FOR Counter := 1 TO 3
      IF Counter == 1
         aRules      := aDefRules
         sRulesArray := "aDefRules"
         FWrite( hCCH, CRLF + "/* Defines */" + CRLF + "aDefRules := {}" + CRLF )
      ELSEIF Counter == 2
         aRules      := aTransRules
         sRulesArray := "aTransRules"
         FWrite( hCCH, CRLF + "/* Translates */" + CRLF + "aTransRules := {}" + CRLF )
      ELSE
         aRules      := aCommRules
         sRulesArray := "aCommRules"
         FWrite( hCCH, CRLF + "/* Commands */" + CRLF + "aCommRules := {}" + CRLF )
      ENDIF

      nRules := Len( aRules )

      FOR nRule := 1 TO nRules
         aRule := aRules[nRule]

         FWrite( hCCH, "aAdd( " + sRulesArray + ", { '" + aRule[1] + "' " )

         IF aRule[2] == NIL
            nMatches := 0
         ELSE
            nMatches := Len( aRule[2] )
         ENDIF

         IF nMatches == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )

            FOR nMatch := 1 TO nMatches
               aMatch := aRule[2][nMatch] //{ nId, nOptional, sAnchor, cType, aWords }
               FWrite( hCCH, "{ " + Str( aMatch[1], 4) + ", " + Str(aMatch[2],3) + ", " + IF( aMatch[3] == NIL, "NIL", "'" + aMatch[3] + "'" ) + ", " + IF( aMatch[4] == NIL, "NIL", "'" + aMatch[4] + "'" ) + ", " )

               IF aMatch[5] == NIL
                  FWrite( hCCH, "NIL" )
               ELSE
                  aWords := aMatch[5]
                  nWords := Len( aWords )
                  FWrite( hCCH, "{ " )

                  FOR nWord := 1 TO nWords
                     FWrite( hCCH, "'" + aWords[nWord] + "'" )
                     IF nWord < nWords
                        FWrite( hCCH, ", " )
                     ENDIF
                  NEXT

                  FWrite( hCCH, " }" )
               ENDIF

               FWrite( hCCH, " }" )

               IF nMatch < nMatches
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT

            FWrite( hCCH, " }" )
         ENDIF

         IF aRule[3]
            FWrite( hCCH, " , .T." )
         ELSE
            FWrite( hCCH, " , .F." )
         ENDIF

         FWrite( hCCH, " } )" + CRLF )
      NEXT
   NEXT

   FWrite( hCCH, CRLF + "RETURN .T." + CRLF )

   FWrite( hCCH, CRLF + "FUNCTION InitResults()" + CRLF )

   FOR Counter := 1 TO 3
      IF Counter == 1
         aResults      := aDefResults
         sResultsArray := "aDefResults"
         FWrite( hCCH, CRLF + "/* Defines Results*/" + CRLF + "aDefResults := {}" + CRLF )
      ELSEIF Counter == 2
         aResults      := aTransResults
         sResultsArray := "aTransResults"
         FWrite( hCCH, CRLF + "/* Translates Results*/" + CRLF + "aTransResults := {}" + CRLF )
      ELSE
         aResults      := aCommResults
         sResultsArray := "aCommResults"
         FWrite( hCCH, CRLF + "/* Commands Results*/" + CRLF + "aCommResults := {}" + CRLF )
      ENDIF

      nResults := Len( aResults )

      FOR nResult := 1 TO nResults
         aResult := aResults[nResult]

         FWrite( hCCH, "aAdd( " + sResultsArray + ", { " )

         IF aResult[1] == NIL
            nRPs := 0
         ELSE
            nRPs := Len( aResult[1] )
         ENDIF
         IF aResult[3] == NIL
            nIDs := 0
         ELSE
            nIDs := Len( aResult[3] )
         ENDIF

         IF nRPs == 0
            /*FWrite( hCCH, "NIL " )*/
         ELSE
            FWrite( hCCH, "{ " )
            FOR nRP := 1 TO nRPs
               aRP := aResult[1][nRP] //{ nLevel, xVal }

               FWrite( hCCH, "{ " + Str( aRP[1], 3) + ", " )
               IF HB_ISSTRING( aRP[2] )
                  FWrite( hCCH, "'" + aRP[2] + "' }" )
               ELSE
                  FWrite( hCCH, Str( aRP[2], 3 ) + " }" )
               ENDIF

               IF nRP < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, " }" )
         ENDIF

         IF nRPs == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nModifier := 1 TO nRPs
               FWrite( hCCH, Str( aResult[2][nModifier], 2 ) )
               IF nModifier < nRPs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, "} " )
         ENDIF

         IF nIDs == 0
            FWrite( hCCH, ", " )
         ELSE
            FWrite( hCCH, ", { " )
            FOR nID := 1 TO nIDs
               FWrite( hCCH, "NIL" )
               IF nID < nIDs
                  FWrite( hCCH, ", " )
               ENDIF
            NEXT
            FWrite( hCCH, " } " )
         ENDIF
         FWrite( hCCH, " } )" + CRLF )
      NEXT

   NEXT

   FWrite( hCCH, CRLF + "RETURN .T. " + CRLF )

   FClose( hCCH )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitRules()

  /* Defines */
  aDefRules := {}
  aAdd( aDefRules, { '_SET_EXACT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_FIXED' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DECIMALS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DATEFORMAT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EPOCH' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PATH' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEFAULT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXCLUSIVE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SOFTSEEK' ,  , .T. } )
  aAdd( aDefRules, { '_SET_UNIQUE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELETED' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CANCEL' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEBUG' ,  , .T. } )
  aAdd( aDefRules, { '_SET_TYPEAHEAD' ,  , .T. } )
  aAdd( aDefRules, { '_SET_COLOR' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CURSOR' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CONSOLE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ALTERNATE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ALTFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DEVICE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXTRA' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXTRAFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PRINTER' ,  , .T. } )
  aAdd( aDefRules, { '_SET_PRINTFILE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MARGIN' ,  , .T. } )
  aAdd( aDefRules, { '_SET_BELL' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CONFIRM' ,  , .T. } )
  aAdd( aDefRules, { '_SET_ESCAPE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_INSERT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_EXIT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_INTENSITY' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SCOREBOARD' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELIMITERS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_DELIMCHARS' ,  , .T. } )
  aAdd( aDefRules, { '_SET_WRAP' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MESSAGE' ,  , .T. } )
  aAdd( aDefRules, { '_SET_MCENTER' ,  , .T. } )
  aAdd( aDefRules, { '_SET_SCROLLBREAK' ,  , .T. } )
  aAdd( aDefRules, { '_SET_COUNT' ,  , .T. } )
  aAdd( aDefRules, { '_SET_CH' ,  , .T. } )
  aAdd( aDefRules, { '_DFSET' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

  /* Translates */
  aTransRules := {}

  /* Commands */
  aCommRules := {}
  aAdd( aCommRules, { 'DO' , { {    1,   0, 'WHILE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'END' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'END' , { {    0,   0, 'SEQUENCE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDSEQUENCE' ,  , .F. } )

/*
  aAdd( aCommRules, { 'ENDDO' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDIF' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDCASE' , { {    1,   0, NIL, '*', NIL } } , .F. } )

  Clipper rules are buggy in that they will HIDE lines following END* in lines ':' seperated
  multi-lins, such as:

     IF x; y(); ENDIF; z() // z() will be sent to lalaland

  This is extermly problematic with #directives generating multiple lines such as hbclass.ch

  The corrected rules are:

  // Using sTrangE cApitalizatioN to signify transformation when debugging PPO files.
  #command ENDDO   <any,...> [<anymore,...>] => eNddO
  #command ENDIF   <any,...> [<anymore,...>] => eNdiF
  #command ENDCASE <any,...> [<anymore,...>] => eNdcasE
*/

  aAdd( aCommRules, { 'ENDDO' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDIF' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'ENDCASE' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )

  aAdd( aCommRules, { 'ENDFOR' , { {    1,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'NEXT' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'STEP', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'CALL' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, { 1002,   1, 'WITH', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'STORE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'TO', '<', NIL }, { 1003,   1, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ECHO', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'HEADING', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'MENU', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'STATUS', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'STEP', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SAFETY', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'TALK', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PROCEDURE', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXACT', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXACT', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'FIXED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FIXED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DECIMALS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    1,   0, 'TO', '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PATH', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    1,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEFAULT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CENTURY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CENTURY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EPOCH', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   0, 'FORMAT', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'AMERICAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ANSI', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'BRITISH', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'FRENCH', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'GERMAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'ITALIAN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'JAPANESE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DATE', NIL, NIL }, {    0,   1, 'TO', NIL, NIL }, {    0,   0, 'USA', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ALTERNATE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ALTERNATE', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONSOLE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONSOLE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MARGIN', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'PRINTER', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'PRINTER', NIL, NIL }, {    1,   0, 'TO', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DEVICE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'COLOUR', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CURSOR', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CURSOR', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '?' , { {    1,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '?' , { {    0,   0, '?', NIL, NIL }, {    1,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'EJECT' ,  , .F. } )
  aAdd( aCommRules, { 'TEXT' ,  , .F. } )
  aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TEXT' , { {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'PRINTER', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLS' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CLEAR', NIL, NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, ',', '<', NIL }, {    4,   0, ',', '<', NIL }, {    5,   0, 'BOX', '<', NIL }, { 1006,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, {    0,   1, 'DOUBLE', NIL, NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   0, ',', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, { 1005,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, 'COLOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'BELL', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'BELL', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'CONFIRM', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'CONFIRM', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'ESCAPE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ESCAPE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'INTENSITY', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'INTENSITY', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SCOREBOARD', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'SCOREBOARD', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELIMITERS', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    0,   0, 'DEFAULT', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELIMITERS', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   0, '.', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FORMAT', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, {    5,   1, 'VALID', '<', NIL }, {    6,   1, 'WHEN', '<', NIL }, { 1007,   1, 'SEND', '<', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'SAY', '<', NIL }, { 1004,   1, NIL, 'A', { 'GET' } }, {    5,   0, 'GET', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'RANGE' } }, {    5,   0, 'RANGE', '<', NIL }, {    6,   0, ',', '<', NIL }, { 1007,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'GET', '<', NIL }, { 1004,   1, NIL, 'A', { 'COLOR' } }, {    5,   0, 'COLOR', '<', NIL }, { 1006,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'READ' , { {    0,   0, 'SAVE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'READ' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'GETS', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { { 1001,   1, NIL, 'A', { 'COLOUR' } }, {    0,   0, 'COLOUR', NIL, NIL }, { 1002,   1, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'WRAP', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'WRAP', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, {    2,   1, NIL, ':', { 'CENTER', 'CENTRE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'PROMPT', '<', NIL }, {    4,   1, 'MESSAGE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'MENU' , { {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   0, 'FROM', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'WAIT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'ACCEPT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'INPUT' , { {    1,   1, NIL, '<', { 'TO' } }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'KEYBOARD' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'TYPEAHEAD', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'TYPEAHEAD', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', '<', NIL }, {    0,   0, '(', NIL, NIL }, {    3,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    2,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'KEY', '<', NIL }, {    0,   1, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'FUNCTION', '<', NIL }, {    0,   1, 'TO', NIL, NIL }, {    2,   1, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'MEMORY', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RELEASE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RESTORE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'LIKE', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'LIKE', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'ALL', NIL, NIL }, {    1,   0, 'EXCEPT', '<', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   0, 'TO', '(', NIL }, {    0,   0, 'ALL', NIL, NIL }, {    2,   0, 'EXCEPT', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SAVE' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'ERASE' , { {    1,   0, NIL, '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'RENAME' , { {    1,   0, NIL, '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   0, 'FILE', '(', NIL }, {    2,   0, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'DIR' , { {    1,   1, NIL, '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TYPE' , { { 1,   1, NIL, '(', { 'TO PRINTER', 'TO' } }, {    2,   1, NIL, ':', { 'TO PRINTER' } }, { 1000,   1, 'TO', NIL, NIL }, { 1003,  -1, 'FILE', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'TYPE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, NIL, ':', { 'TO PRINTER' } } } , .F. } )
  aAdd( aCommRules, { 'REQUEST' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'CANCEL' ,  , .F. } )
  aAdd( aCommRules, { 'QUIT' ,  , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { '!' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, '=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'RUN' , { {    1,   0, ':=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'EXCLUSIVE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'EXCLUSIVE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'SOFTSEEK', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'SOFTSEEK', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'UNIQUE', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'UNIQUE', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    1,   0, 'DELETED', ':', { 'ON', 'OFF', '&' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'DELETED', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SELECT' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, {    2,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'USE' ,  , .F. } )
  aAdd( aCommRules, { 'USE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'VIA', '<', NIL }, {    3,   1, 'ALIAS', '<', NIL }, {    4,   1, NIL, ':', { 'NEW' } }, {    5,   1, NIL, ':', { 'EXCLUSIVE' } }, {    6,   1, NIL, ':', { 'SHARED' } }, {    7,   1, NIL, ':', { 'READONLY' } }, { 1008,   1, 'INDEX', '(', NIL }, { 1009,   2, ',', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    0,   0, 'BLANK', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'PACK' ,  , .F. } )
  aAdd( aCommRules, { 'ZAP' ,  , .F. } )
  aAdd( aCommRules, { 'UNLOCK' ,  , .F. } )
  aAdd( aCommRules, { 'UNLOCK' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COMMIT' ,  , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    0,   0, 'TOP', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GOTO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'GO' , { {    0,   0, 'BOTTOM', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' ,  , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, 'ALIAS', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SKIP' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'ALIAS', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SEEK' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'SOFTSEEK' } } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, NIL, '*', NIL } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, ':=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'FIND' , { {    1,   0, '=', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CONTINUE' ,  , .F. } )
  aAdd( aCommRules, { 'LOCATE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'RELATION', NIL, NIL }, {    1,   1, NIL, ':', { 'ADDITIVE' } }, {    2,   1, 'TO', '<', NIL }, {    3,  -1, 'INTO', '(', NIL }, {    0,   2, ',', NIL, NIL }, { 1000,   3, 'TO', NIL, NIL }, { 1004,  -2, NIL, '<', NIL }, { 1005,  -2, 'INTO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'FILTER', NIL, NIL }, {    1,   0, 'TO', ':', { '&' } } } , .F. } )
  aAdd( aCommRules, { 'REPLACE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,  -1, 'WITH', '<', NIL }, { 1003,   2, ',', '<', NIL }, { 1004,  -2, 'WITH', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'REPLACE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'WITH', '<', NIL }, { 1003,   1, ',', '<', NIL }, { 1004,  -1, 'WITH', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'RECALL' , { {    1,   1, 'FOR', '<', NIL }, {    2,   1, 'WHILE', '<', NIL }, {    3,   1, 'NEXT', '<', NIL }, {    4,   1, 'RECORD', '<', NIL }, {    5,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'DELETE' ,  , .F. } )
  aAdd( aCommRules, { 'RECALL' ,  , .F. } )
  aAdd( aCommRules, { 'CREATE' , { {    1,   0, NIL, '(', NIL }, {    2,   1, 'FROM', '(', NIL }, {    3,   1, 'VIA', '<', NIL }, {    4,   1, 'ALIAS', '<', NIL }, {    5,   1, NIL, ':', { 'NEW' } } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    0,   1, 'EXTENDED', NIL, NIL }, {    1,   1, 'TO', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    0,   1, 'STRUCTURE', NIL, NIL }, {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'COPY' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'DELIMITED', NIL, NIL }, {    2,   2, 'WITH', '*', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    0,   1, 'SDF', NIL, NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'APPEND' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'FIELDS', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    8,   1, 'VIA', '<', NIL }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SORT' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', 'A', NIL }, {    3,   1, 'FOR', '<', NIL }, {    4,   1, 'WHILE', '<', NIL }, {    5,   1, 'NEXT', '<', NIL }, {    6,   1, 'RECORD', '<', NIL }, {    7,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'TOTAL' , { {    1,   1, 'TO', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL }, {    5,   1, 'WHILE', '<', NIL }, {    6,   1, 'NEXT', '<', NIL }, {    7,   1, 'RECORD', '<', NIL }, {    8,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'UPDATE' , { {    1,   1, 'FROM', '(', NIL }, {    2,   1, 'ON', '<', NIL }, {    3,   1, 'REPLACE', '<', NIL }, {    4,  -1, 'WITH', '<', NIL }, { 1005,   2, ',', '<', NIL }, { 1006,  -2, 'WITH', '<', NIL }, {    7,   1, NIL, ':', { 'RANDOM' } } } , .F. } )
  aAdd( aCommRules, { 'JOIN' , { {    1,   1, 'WITH', '(', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, 'FIELDS', 'A', NIL }, {    4,   1, 'FOR', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'COUNT' , { {    1,   1, 'TO', '<', NIL }, {    2,   1, 'FOR', '<', NIL }, {    3,   1, 'WHILE', '<', NIL }, {    4,   1, 'NEXT', '<', NIL }, {    5,   1, 'RECORD', '<', NIL }, {    6,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'SUM' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'AVERAGE' , { {    1,   1, NIL, '<', { 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, { 1002,   2, ',', '<', NIL }, {    3,  -1, 'TO', '<', NIL }, { 1004,   2, ',', '<', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'LIST' , { {    1,   1, NIL, 'A', { 'OFF', 'TO PRINTER', 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'DISPLAY' , { {    1,   1, NIL, 'A', { 'OFF', 'TO PRINTER', 'TO', 'FOR', 'WHILE', 'NEXT', 'RECORD', 'REST', 'ALL' } }, {    2,   1, NIL, ':', { 'OFF' } }, {    3,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    4,  -1, 'FILE', '(', NIL }, {    5,   1, 'FOR', '<', NIL }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, {    9,   1, NIL, ':', { 'REST' } }, {   10,   1, NIL, ':', { 'ALL' } } } , .F. } )
  aAdd( aCommRules, { 'REPORT' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, 'HEADING', '<', NIL }, {    3,   1, NIL, ':', { 'PLAIN' } }, {    4,   1, NIL, ':', { 'NOEJECT' } }, {    5,   1, NIL, ':', { 'SUMMARY' } }, {    6,   1, NIL, ':', { 'NOCONSOLE' } }, {    7,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    8,  -1, 'FILE', '(', NIL }, {    9,   1, 'FOR', '<', NIL }, {   10,   1, 'WHILE', '<', NIL }, {   11,   1, 'NEXT', '<', NIL }, {   12,   1, 'RECORD', '<', NIL }, {   13,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'LABEL' , { {    1,   0, 'FORM', '<', NIL }, {    2,   1, NIL, ':', { 'SAMPLE' } }, {    3,   1, NIL, ':', { 'NOCONSOLE' } }, {    4,   1, NIL, ':', { 'TO PRINTER' } }, {    0,   1, 'TO', NIL, NIL }, {    5,  -1, 'FILE', '(', NIL }, {    6,   1, 'FOR', '<', NIL }, {    7,   1, 'WHILE', '<', NIL }, {    8,   1, 'NEXT', '<', NIL }, {    9,   1, 'RECORD', '<', NIL }, {   10,   1, NIL, ':', { 'REST' } }, {    0,   1, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' ,  , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'DATABASES', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALTERNATE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'FORMAT', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'INDEXES', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'PROCEDURE', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLOSE' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'CLEAR' ,  , .F. } )
  aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'ALL', NIL, NIL } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   1, 'TAG', '(', NIL }, {    3,   0, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TAG', '(', NIL }, {    3,   1, 'TO', '(', NIL }, {    4,   1, 'FOR', '<', NIL }, { 1005,   1, NIL, ':', { 'ALL' } }, {    6,   1, 'WHILE', '<', NIL }, {    7,   1, 'NEXT', '<', NIL }, {    8,   1, 'RECORD', '<', NIL }, { 1009,   1, NIL, ':', { 'REST' } }, {   10,   1, 'EVAL', '<', NIL }, {   11,   1, 'EVERY', '<', NIL }, { 1012,   1, NIL, ':', { 'UNIQUE' } }, {   13,   1, NIL, ':', { 'ASCENDING' } }, { 1014,   1, NIL, ':', { 'DESCENDING' } } } , .F. } )
  aAdd( aCommRules, { 'INDEX' , { {    1,   0, 'ON', '<', NIL }, {    2,   0, 'TO', '(', NIL }, {    3,   1, NIL, ':', { 'UNIQUE' } } } , .F. } )
  aAdd( aCommRules, { 'DELETE' , { {    1,   0, 'TAG', '(', NIL }, {    2,   1, 'IN', '(', NIL }, { 1003,   1, ',', '(', NIL }, { 1004,   2, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'REINDEX' , { {    1,   1, 'EVAL', '<', NIL }, {    2,   1, 'EVERY', '<', NIL } } , .F. } )
  aAdd( aCommRules, { 'REINDEX' ,  , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'INDEX', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, { 1001,   1, NIL, '(', { 'ADDITIVE' } }, { 1002,   2, ',', '(', NIL }, {    3,   1, NIL, ':', { 'ADDITIVE' } } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL }, {    1,   0, 'TAG', '(', NIL }, { 1002,   1, 'IN', '(', NIL } } , .F. } )
  aAdd( aCommRules, { 'SET' , { {    0,   0, 'ORDER', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .F. } )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitResults()

  /* Defines Results*/
  aDefResults := {}
  aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '10' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '11' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '12' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '13' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '14' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '15' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '17' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '18' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '19' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '20' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '21' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '22' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '23' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '24' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '25' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '26' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '27' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '28' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '29' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '30' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '31' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '32' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '33' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '34' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '35' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '36' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '37' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '38' } }, { -1} ,  } )
  aAdd( aDefResults, { { {   0, '38' } }, { -1} ,  } )
  aAdd( aDefResults, { , ,  } )
  aAdd( aDefResults, { { {   0, 'Set' }, {   0, '(' }, {   0, '_SET_DATEFORMAT' }, {   0, ',' }, {   0, 'if' }, {   0, '(' }, {   0, '__SetCentury' }, {   0, '(' }, {   0, ')' }, {   0, ',' }, {   0,   1 }, {   0, ',' }, {   0,   2 }, {   0, ')' }, {   0, ')' } }, { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  1, -1,  1, -1, -1} , { NIL, NIL }  } )

  /* Translates Results*/
  aTransResults := {}

  /* Commands Results*/
  aCommResults := {}
  aAdd( aCommResults, { { {   0, 'while ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'end' } }, { -1} ,  } )

/*
  aAdd( aCommResults, { { {   0, 'enddo' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'endif' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'endcase' } }, { -1} , { NIL }  } )
*/

  aAdd( aCommResults, { { {   0, 'eNddO' } }, { -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'eNdiF' } }, { -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'eNdcasE' } }, { -1} , { NIL, NIL }  } )

  aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'next' } }, { -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'do ' }, {   0,   1 }, {   0, '' }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'call ' }, {   0,   1 }, {   0, '' }, {   2, ' WITH ' }, {   2,   2 } }, { -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := ' }, {   3, ' ' }, {   3,   3 }, {   3, ' := ' }, {   0, ' ' }, {   0,   1 } }, { -1,  1, -1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , , { NIL }  } )
  aAdd( aCommResults, { , ,  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXACT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_FIXED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DECIMALS, 0 )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PATH, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEFAULT, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetCentury( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EPOCH, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DATEFORMAT, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "mm/dd/yyyy", "mm/dd/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "yyyy.mm.dd", "yy.mm.dd" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd/mm/yyyy", "dd/mm/yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd.mm.yyyy", "dd.mm.yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "dd-mm-yyyy", "dd-mm-yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "yyyy/mm/dd", "yy/mm/dd" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_DFSET( "mm-dd-yyyy", "mm-dd-yy" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTERNATE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ALTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONSOLE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MARGIN, 0 )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTER, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, "" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_PRINTFILE, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "SCREEN" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DEVICE, "PRINTER" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetColor( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SET COLOR TO ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetCursor( if(Upper(' }, {   0,   1 }, {   0, ') == "ON", 1, 0) )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetCursor( if(' }, {   0,   1 }, {   0, ', 1, 0) )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'QOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'QQOut( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Eject()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'text QOut, QQOut' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__TextSave( ' }, {   0,   1 }, {   0, ' ) ; text QOut, __TextRestore' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__TextSave("PRINTER") ; text QOut, __TextRestore' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Scroll() ; SetPos(0,0)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLS' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Scroll( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' ) ; SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, '' }, {   6, ', ' }, {   6,   6 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 2 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DispBox( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', 1 ' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOutPict( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, '' }, {   5, ', ' }, {   5,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DevPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; DevOut( ' }, {   0,   3 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_BELL, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_CONFIRM, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_ESCAPE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_INTENSITY, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SCOREBOARD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMITERS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELIMCHARS, "::" )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + ".FMT" ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '_ProcReq_( ' }, {   0,   1 }, {   0, ' + "." + ' }, {   0,   2 }, {   0, ' ) ; __SetFormat( {|| ' }, {   0,   1 }, {   0, '()} )' } }, { -1,  4, -1,  4, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;   SET FORMAT TO ; else ;   __SetFormat( &("{||" + ' }, {   0,   1 }, {   0, ' + "()}") ) ; end' } }, { -1,  4, -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetFormat()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'SetPos( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ; AAdd( GetList, _GET_( ' }, {   0,   3 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' ):display() ) ' }, {   7, '; ATail(GetList):' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1, -1,  3, -1,  1, -1,  5, -1,  5, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' SAY ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' ; @ Row(), Col()+1 GET ' }, {   0,   5 }, {   0, '' }, {   6, ' ' }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' VALID {|_1| RangeCheck(_1,, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ')} ' }, {   7, ' ' }, {   7,   7 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' GET ' }, {   0,   3 }, {   0, '' }, {   4, ' ' }, {   4,   4 }, {   0, ' SEND colorDisp(' }, {   0,   5 }, {   0, ') ' }, {   6, ' ' }, {   6,   6 } }, { -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1, -1,  1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ReadModal(GetList)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ReadModal(GetList) ; GetList := {}' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ReadKill(.T.) ; GetList := {}' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '@ ' }, {   1, ' ' }, {   1,   1 }, {   0, ' COLOR ' }, {   2, ' ' }, {   2,   2 } }, { -1, -1,  1, -1, -1,  1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_WRAP, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, ' }, {   0,   1 }, {   0, ' ) ; Set( _SET_MCENTER, ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .f. )' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__AtPrompt( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' , ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := __MenuTo( {|_1| if(PCount() == 0, ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' := _1)}, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__XSaveScreen()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__XRestScreen()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := SaveScreen( 0, 0, Maxrow(), Maxcol() )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'RestScreen( 0, 0, Maxrow(), Maxcol(), ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Wait( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := __Wait( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' := __Accept( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( !Empty(__Accept(' }, {   0,   1 }, {   0, ')) ) ; ' }, {   0,   2 }, {   0, ' := &( __AcceptStr() ) ; end' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__Keyboard( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Keyboard()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_TYPEAHEAD, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'SET KEY ' }, {   0,   1 }, {   0, ' TO ' }, {   0,   2 } }, { -1,  1, -1,  1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   2 }, {   0, ') ) ;   SetKey( ' }, {   0,   1 }, {   0, ', NIL ) ; else ;   SetKey( ' }, {   0,   1 }, {   0, ', {|p, l, v| ' }, {   0,   2 }, {   0, '(p, l, v)} ) ; end' } }, { -1,  4, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'SetKey( ' }, {   0,   1 }, {   0, ', NIL )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__SetFunction( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MClear()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__MXRelease( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  3, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRelease("*", .t.)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRelease( ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__MRestore( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .t. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', .f. )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__MSave( ' }, {   0,   1 }, {   0, ', "*", .t. )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FErase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'FRename( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__CopyFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__Dir( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   1, '; COPY FILE ' }, {   1,   1 }, {   1, ' TO ' }, {   1,   3 } }, { -1,  4, -1,  6, -1, -1,  4, -1,  4} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__TypeFile( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'EXTERNAL ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__Quit()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  2, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__Run( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'RUN ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( run := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_EXCLUSIVE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_SOFTSEEK, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_UNIQUE, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'Set( _SET_DELETED, ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSelectArea( ' }, {   0,   1 }, {   0, '(' }, {   0,   2 }, {   0, ') )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUseArea( ' }, {   0,   4 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   3 }, {   0, ', if(' }, {   0,   6 }, {   0, ' .or. ' }, {   0,   5 }, {   0, ', !' }, {   0,   5 }, {   0, ', NIL), ' }, {   0,   7 }, {   0, ' ) ' }, {   8, '; dbSetIndex( ' }, {   8,   8 }, {   8, ' )' }, {   0, '' }, {   9, '; dbSetIndex( ' }, {   9,   9 }, {   9, ' )' } }, { -1,  6, -1,  1, -1,  4, -1,  4, -1,  6, -1,  6, -1,  6, -1,  6, -1, -1,  4, -1, -1, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbAppend()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbPack()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbZap()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUnlock()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbUnlockAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbCommitAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbGoto(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoTop()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbGoBottom()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSkip(1)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSkip( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' -> ( dbSkip(1) )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   2 }, {   0, ' -> ( dbSkip(' }, {   0,   1 }, {   0, ') )' } }, { -1,  1, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   2 }, {   0, ', .T., NIL ) )' } }, { -1,  1, -1,  6, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbSeek( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '( find := ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbContinue()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbLocate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbClearRel()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'if ( !' }, {   0,   1 }, {   0, ' ) ;    dbClearRel() ; end ; dbSetRelation( ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   5, '; dbSetRelation( ' }, {   5,   5 }, {   5, ', ' }, {   5,   4 }, {   5, ', ' }, {   5,   4 }, {   5, ' )' } }, { -1,  6, -1,  4, -1,  5, -1,  3, -1, -1,  4, -1,  5, -1,  3, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbClearFilter(NIL)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' )' } }, { -1,  5, -1,  3, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'if ( Empty(' }, {   0,   1 }, {   0, ') ) ;    dbClearFilter() ; else     ;    dbSetFilter( ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; end' } }, { -1,  4, -1,  5, -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| _FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   0, '' }, {   3, ', _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '_FIELD->' }, {   0,   1 }, {   0, ' := ' }, {   0,   2 }, {   0, '' }, {   3, '; _FIELD->' }, {   3,   3 }, {   3, ' := ' }, {   3,   4 } }, { -1,  1, -1,  1, -1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| dbDelete()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'DBEval( {|| dbRecall()}, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbDelete()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbRecall()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__dbCreate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  1, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopyXStruct( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopyStruct( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  4, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbDelim( .T., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSDF( .T., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbCopy( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbDelim( .F., ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSDF( .F., ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbApp( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbSort( ' }, {   0,   1 }, {   0, ', { ' }, {   0,   2 }, {   0, ' }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbTotal( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ' )' } }, { -1,  4, -1,  5, -1,  4, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbUpdate( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   7 }, {   0, ', {|| _FIELD->' }, {   0,   3 }, {   0, ' := ' }, {   0,   4 }, {   0, '' }, {   5, ', _FIELD->' }, {   5,   5 }, {   5, ' := ' }, {   5,   6 }, {   0, '} )' } }, { -1,  4, -1,  5, -1,  6, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbJoin( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   4 }, {   0, ' )' } }, { -1,  4, -1,  4, -1,  4, -1,  5, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' := 0 ; DBEval( {|| ' }, {   0,   1 }, {   0, ' := ' }, {   0,   1 }, {   0, ' + 1}, ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   3 }, {   0, ' := ' }, {   4, ' ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'M->__Avg := ' }, {   0,   3 }, {   0, ' := ' }, {   4, ' ' }, {   4,   4 }, {   4, ' := ' }, {   0, ' 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' + ' }, {   0,   1 }, {   0, '' }, {   4, ', ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' + ' }, {   4,   2 }, {   0, ' }, ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' ) ; ' }, {   0,   3 }, {   0, ' := ' }, {   0,   3 }, {   0, ' / M->__Avg ' }, {   4, '; ' }, {   4,   4 }, {   4, ' := ' }, {   4,   4 }, {   4, ' / M->__Avg ' } }, { -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__dbList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, .t., ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__DBList( ' }, {   0,   2 }, {   0, ', { ' }, {   0,   1 }, {   0, ' }, ' }, {   0,  10 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  6, -1,  5, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  4, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__ReportForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, '__LabelForm( ' }, {   0,   1 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   2 }, {   0, ' )' } }, { -1,  4, -1,  6, -1,  4, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, '->( dbCloseArea() )' } }, { -1,  1, -1} , { NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCloseArea()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbCloseAll()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'Set(_SET_ALTFILE, "")' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, '__SetFormat(NIL)' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'dbClearIndex()' } }, { -1} ,  } )
  aAdd( aCommResults, { , ,  } )
  aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLEAR SCREEN ; CLEAR GETS' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'CLOSE DATABASES ; CLOSE FORMAT ; CLEAR MEMORY ; CLEAR GETS ; SET ALTERNATE OFF ; SET ALTERNATE TO' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5, ' ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9, ' ' }, {   9,   9 }, {   0, ', ' }, {  14, ' ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12, ' ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet( ' }, {   0,   4 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5, ' ' }, {   5,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ',    RECNO(), ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9, ' ' }, {   9,   9 }, {   0, ', ' }, {  14, ' ' }, {  14,  14 }, {   0, ' ) ;  ordCreate(' }, {   0,   3 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {  12, ' ' }, {  12,  12 }, {   0, '    )' } }, { -1,  3, -1,  5, -1, -1,  6, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1, -1,  6, -1,  4, -1,  4, -1,  3, -1,  5, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'dbCreateIndex( ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   1 }, {   0, ', if( ' }, {   0,   3 }, {   0, ', .t., NIL ) )' } }, { -1,  4, -1,  3, -1,  5, -1,  6, -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordDestroy( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ' ) ' }, {   3, '; ordDestroy( ' }, {   3,   3 }, {   3, ', ' }, {   3,   4 }, {   3, ' ) ' } }, { -1,  4, -1,  4, -1, -1,  4, -1,  4, -1} , { NIL, NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordCondSet(,,,, ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ',,,,,,,) ;  ordListRebuild()' } }, { -1,  5, -1,  1, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordListRebuild()' } }, { -1} ,  } )
  aAdd( aCommResults, { { {   0, 'if !' }, {   0,   3 }, {   0, ' ; ordListClear() ; end ' }, {   1, '; ordListAdd( ' }, {   1,   1 }, {   1, ' )' }, {   0, '' }, {   2, '; ordListAdd( ' }, {   2,   2 }, {   2, ' )' } }, { -1,  6, -1, -1,  4, -1, -1, -1,  4, -1} , { NIL, NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   0, '' }, {   2, ', ' }, {   2,   2 }, {   0, ' )' } }, { -1,  1, -1, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus( ' }, {   0,   1 }, {   0, '' }, {   2, ', ' }, {   2,   2 }, {   0, ' )' } }, { -1,  4, -1, -1,  4, -1} , { NIL, NIL }  } )
  aAdd( aCommResults, { { {   0, 'ordSetFocus(0)' } }, { -1} ,  } )

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitClsRules()

 #ifdef __HARBOUR__

      /* Defines */
      aAdd( aDefRules, { 'HB_CONSTRUCTOR_NO_DIVERT' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLASS_CH_' ,  , .T. } )
      aAdd( aDefRules, { 'HB_SETUP_CH_' ,  , .T. } )
      aAdd( aDefRules, { 'HB_EXTENSION' ,  , .T. } )
      aAdd( aDefRules, { 'HB_C52_UNDOC' ,  , .T. } )
      aAdd( aDefRules, { 'HB_COMPAT_C53' ,  , .T. } )
      aAdd( aDefRules, { 'HB_COMPAT_XPP' ,  , .T. } )
      aAdd( aDefRules, { 'HB_COMPAT_FLAGSHIP' ,  , .T. } )
      aAdd( aDefRules, { 'HB_COMPAT_FOXPRO' ,  , .T. } )
      aAdd( aDefRules, { 'HB_EXT_INKEY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CH_' ,  , .T. } )
      aAdd( aDefRules, { 'HB_MSGLISTALL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_MSGLISTCLASS' ,  , .T. } )
      aAdd( aDefRules, { 'HB_MSGLISTPURE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_EXPORTED' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_PUBLISHED' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_PROTECTED' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_HIDDEN' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_CTOR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_READONLY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_SHARED' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_CLASS' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_SUPER' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_CLASSCTOR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_SYNC' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_SYMBOL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSTP_PFUNC' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_INSTANCED' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_CLASSCTOR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_ONERROR_SYMB' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_DESTRUC_SYMB' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_REALLOCINIT' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLS_ONERROR_SUPER' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_METHOD' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_DATA' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_CLASSDATA' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_INLINE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_VIRTUAL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_SUPER' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_ONERROR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_DESTRUCTOR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_PROPERTY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_PROPERTY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_CLASSPROPERTY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MSG_DELEGATE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_SYMBOL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_VALUE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_SYMBOL_PTR' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_TYPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_SCOPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_DATA_PERSISTENT' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSD_SYMBOL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSD_VALUE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSD_TYPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSD_SCOPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_SYMBOL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_PFUNCTION' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_DELEGNAME' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_SCOPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_PERSISTENT' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MTHD_DELEGOBJ' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSM_SYMBOL' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSM_PFUNCTION' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_CLSM_SCOPE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MCLSCTOR_INSTANCE' ,  , .T. } )
      aAdd( aDefRules, { 'HB_OO_MCLSCTOR_CLONE' ,  , .T. } )
      aAdd( aDefRules, { 'FOREACH_BEGIN' ,  , .T. } )
      aAdd( aDefRules, { 'FOREACH_ENUMERATE' ,  , .T. } )
      aAdd( aDefRules, { 'FOREACH_END' ,  , .T. } )
      aAdd( aDefRules, { '__HB_CLS_NOINI' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLS_FWO' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLS_CSY' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLS_VO' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLS_TOP' ,  , .T. } )
      aAdd( aDefRules, { 'HB_CLS_XB' ,  , .T. } )
      aAdd( aDefRules, { '_AsName_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

      /* Translates */
      aAdd( aTransRules, { '(' , { {    0,   0, '.', NIL, NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
      aAdd( aTransRules, { 'AS' , { {    0,   0, 'INTEGER', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '__ERR' , { {    0,   0, '(', NIL, NIL }, { 1001,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_INHERITFROM_' , { {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_INHERITFROM_' , { {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASFUNC_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASFUNC_' , { {    1,   0, '(', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASNAME_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASNAME_' , { {    1,   0, '(', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASSTR_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASSTR_' , { {    1,   0, '(', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASSTRLST_' , { {    1,   0, '(', '<', NIL }, { 1002,   1, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASNAMEFROM_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_ASNAMEFROM_' , { {    1,   0, '(', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '__OPT__' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '__OPT__' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    0,   0, '(', NIL, NIL }, {    0,   0, '.T.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    0,   0, '(', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.T.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    0,   0, '(', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.T.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    0,   0, '(', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.T.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HBCLSCHOICE' , { {    0,   0, '(', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '.F.', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'CREATE' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_HB_MEMBER' , { {    0,   0, '{', NIL, NIL }, {    0,   0, 'AS', NIL, NIL }, {    0,   0, 'NUM', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '_HB_MEMBER' , { {    0,   0, '{', NIL, NIL }, {    0,   0, 'AS', NIL, NIL }, {    0,   0, 'CHAR', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'DECLMETH' , { {    1,   0, NIL, '<', NIL }, {    2,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aTransRules, { ':' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ':' , { {    0,   0, 'CLASS', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '(' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '=' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ',' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ':=' , { {    1,   0, NIL, '!', NIL }, {    0,   0, '{', NIL, NIL }, {    2,   1, NIL, 'A', { '}' } }, {    0,   0, '}', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'END' , { {    0,   0, 'CLASS', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { ':' , { {    0,   0, 'SUPER', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

      /* Commands */
      aAdd( aCommRules, { 'OVERRIDE' , { {    1,   0, 'METHOD', '!', NIL }, {    0,   1, 'IN', NIL, NIL }, {    2,   0, 'CLASS', '!', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    0,   1, 'METHOD', NIL, NIL }, {    3,   0, NIL, '!', NIL }, {    4,   1, 'SCOPE', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', '!', NIL }, {    2,   0, 'WITH', ':', { 'DATA', 'VAR' } }, {    3,   0, NIL, '<', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'METHOD', '!', NIL }, {    3,   1, 'SCOPE', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT' } }, {    5,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'METHOD', '!', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    4,   0, 'INLINE', 'A', NIL }, {    5,   1, 'SCOPE', '<', NIL }, {    6,   1, NIL, ':', { 'PERSISTENT' } }, {    7,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'OVERRIDE' , { {    1,   0, 'METHOD', '!', NIL }, {    0,   1, 'IN', NIL, NIL }, {    2,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    0,   1, 'METHOD', NIL, NIL }, {    3,   0, NIL, '!', NIL }, {    4,   1, 'SCOPE', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    2,   0, 'WITH', ':', { 'DATA', 'VAR' } }, {    3,   0, NIL, '<', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'METHOD', '!', NIL }, {    3,   1, 'SCOPE', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT' } }, {    5,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'METHOD', '!', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    4,   0, 'INLINE', 'A', NIL }, {    5,   1, 'SCOPE', '<', NIL }, {    6,   1, NIL, ':', { 'PERSISTENT' } }, {    7,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    0,   1, 'TYPE', NIL, NIL }, {    1,   0, NIL, ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'METHOD', '!', NIL }, {    3,   1, 'SCOPE', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT' } }, {    5,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    0,   1, 'TYPE', NIL, NIL }, {    1,   0, NIL, ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'METHOD', '!', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    0,   1, 'TYPE', NIL, NIL }, {    1,   0, NIL, ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, {    4,   1, 'SCOPE', '<', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT' } }, {    6,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'EXTEND' , { {    0,   1, 'TYPE', NIL, NIL }, {    1,   0, NIL, ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'MESSAGE', '<', NIL }, {    3,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    4,   0, 'INLINE', 'A', NIL }, {    5,   1, 'SCOPE', '<', NIL }, {    6,   1, NIL, ':', { 'PERSISTENT' } }, {    7,   1, NIL, ':', { 'NOUPPER' } } } , .T. } )
      aAdd( aCommRules, { 'ENABLE' , { {    0,   0, 'TYPE', NIL, NIL }, {    1,   0, 'CLASS', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, { 1002,   1, ',', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } } } , .T. } )
      aAdd( aCommRules, { 'ENABLE' , { {    0,   0, 'TYPE', NIL, NIL }, {    0,   0, 'CLASS', NIL, NIL }, {    0,   0, 'ALL', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ASSOCIATE' , { {    1,   0, 'CLASS', '<', NIL }, {    0,   0, 'WITH', NIL, NIL }, {    2,   0, 'TYPE', ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } } } , .T. } )
      aAdd( aCommRules, { 'EXTERNAL' , { {    1,   0, NIL, ':', { 'ARRAY', 'BLOCK', 'CHARACTER', 'DATE', 'LOGICAL', 'NIL', 'NUMERIC', 'POINTER', 'HASH' } }, { 1002,   1, ',', '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'CLASS' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'METACLASS', '<', NIL }, { 1003,   1, NIL, ':', { 'FROM', 'INHERIT' } }, { 1004,  -1, NIL, '<', NIL }, { 1005,   2, ',', '<', NIL }, {    6,   1, NIL, ':', { 'STATIC' } }, { 1007,   1, 'FUNCTION', '<', NIL }, {    0,   1, 'IMPLEMENTS', NIL, NIL }, { 1008,  -1, 'NAMESPACE', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLSUPER' ,  , .T. } )
      aAdd( aCommRules, { 'DECLSUPER' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, ',', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLSUPER' , { {    1,   0, NIL, '<', NIL }, {    2,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL }, { 1003,   1, ',', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLSUPERN' , { {    1,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLSUPERN' , { {    1,   0, NIL, '<', NIL }, {    2,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IN', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'IN', '<', NIL }, {    5,   1, NIL, ':', { 'PUBLISHED' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    9,   1, NIL, ':', { 'READONLY', 'RO' } }, {   10,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'VAR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'TO', '<', NIL }, {    5,   1, NIL, ':', { 'PUBLISHED' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    9,   1, NIL, ':', { 'READONLY', 'RO' } }, {   10,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'CLASS' , { {    1,   0, 'VAR', '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'CLASS' , { {    1,   0, 'METHOD', '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'DEFERRED', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'EXPORT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'EXPORT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'PROTECT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'PROTECT' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'HIDDEN' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'HIDDEN' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'READONLY', 'RO' } }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'CLASSVAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'TYPE', '<', NIL }, {    3,   1, 'ASSIGN', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'SHARED' } } } , .T. } )
      aAdd( aCommRules, { 'CLASSVAR' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'SHARED' } } } , .T. } )
      aAdd( aCommRules, { 'DATA' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'CLASSDATA' , { {    1,   0, NIL, 'A', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, 'INIT', '<', NIL }, {    4,   1, NIL, ':', { 'PUBLISHED' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'READONLY', 'RO' } }, { 1009,   1, NIL, ':', { 'SHARED' } } } , .T. } )
      aAdd( aCommRules, { 'SYNC' , { {    1,   0, 'METHOD', '<', NIL }, { 1002,   1, NIL, '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'CLASSMETHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    7,   1, NIL, ':', { 'SHARED' } } } , .T. } )
      aAdd( aCommRules, { 'CONSTRUCTOR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, NIL, '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'CONSTRUCTOR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'INLINE', 'A', NIL }, { 1003,   1, NIL, '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    7,   1, NIL, ':', { 'SYNC' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1009,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1004,   1, 'AS', '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1010,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'CLASSMETHOD' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    7,   1, NIL, ':', { 'SHARED' } }, {    8,   1, NIL, ':', { 'SYNC' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'CLASSMETHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, {    3,   1, NIL, ':', { 'CONSTRUCTOR' } }, { 1004,   1, 'AS', '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SHARED' } }, {    9,   1, NIL, ':', { 'SYNC' } }, {    0,   1, '_CLASS_DECLARATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'BLOCK', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1010,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'EXTERN', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1010,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1003,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    4,   0, NIL, 'A', NIL }, { 1005,   1, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1004,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    5,   0, NIL, 'A', NIL }, { 1006,   1, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } }, {    9,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1010,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'INLINE', 'A', NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    9,   1, NIL, ':', { 'SYNC' } }, {   10,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, { 1011,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'INLINE' , { {    1,   0, 'METHOD', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'INLINE' , { {    1,   0, 'METHOD', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ENDMETHOD' ,  , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, { 1003,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    0,   0, 'VIRTUAL', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, { 1003,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    0,   0, 'DYNAMIC', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'OPERATOR', '<', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    7,   1, NIL, ':', { 'SYNC' } }, { 1008,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'OPERATOR', '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } }, { 1009,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'OPERATOR' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'ARG', '<', NIL }, {    3,   0, 'INLINE', 'A', NIL }, {    4,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    5,   1, NIL, ':', { 'PROTECTED' } }, {    6,   1, NIL, ':', { 'HIDDEN', 'PRIVARE' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'METHOD', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'METHOD', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1004,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    9,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'METHOD', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1005,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1006,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    7,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    8,   1, NIL, ':', { 'PROTECTED' } }, {    9,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {   10,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'PROCEDURE', '<', NIL }, { 1004,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    8,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'PROCEDURE', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1004,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1005,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {    9,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   0, 'PROCEDURE', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1005,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1006,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    7,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    8,   1, NIL, ':', { 'PROTECTED' } }, {    9,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, {   10,   1, NIL, ':', { 'SYNC' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, { 1008,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, { 1008,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IN', '<', NIL }, {    4,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, { 1008,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'TO', '<', NIL }, {    5,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, { 1009,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'MESSAGE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, 'IS', '<', NIL }, {    4,   0, 'IN', '<', NIL }, {    5,   1, NIL, ':', { 'CONSTRUCTOR' } }, {    6,   1, NIL, ':', { 'EXPORTED', 'VISIBLE', 'PUBLIC' } }, {    7,   1, NIL, ':', { 'PROTECTED' } }, {    8,   1, NIL, ':', { 'HIDDEN', 'PRIVATE' } }, { 1009,   1, NIL, ':', { 'OVERRIDE' } } } , .T. } )
      aAdd( aCommRules, { 'DELEGATE' , { {    1,   0, NIL, '*', NIL } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, NIL, ':', { 'METHOD', 'IS' } }, {    4,   0, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   0, NIL, ':', { 'METHOD', 'IS' } }, {    4,   0, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'EXPORTED', 'VISIBLE' } }, {    6,   1, NIL, ':', { 'PROTECTED' } }, {    7,   1, NIL, ':', { 'HIDDEN' } }, {    8,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, {    0,   0, 'SETGET', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } }, {    0,   0, 'SETGET', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    3,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    4,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1003,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    4,   0, NIL, 'A', NIL }, {    5,   1, NIL, ':', { 'PERSISTENT', 'PROPERTY' } } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL }, {    0,   0, 'DEFERRED', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL }, { 1003,   1, 'AS', '<', NIL }, {    0,   0, 'INLINE', NIL, NIL }, { 1004,   1, 'LOCAL', '<', NIL }, {    0,  -1, ',', NIL, NIL }, {    5,   0, NIL, 'A', NIL } } , .T. } )
      aAdd( aCommRules, { 'ON' , { {    1,   0, 'ERROR', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ERROR' , { {    1,   0, 'HANDLER', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ERROR' , { {    1,   0, 'HANDLER', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1002,   1, NIL, 'A', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'DESTRUCTOR' , { {    1,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DESTRUCTOR' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ENDCLASS' ,  , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    0,   1, 'CLASS', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PROCEDURE' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'PROCEDURE' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PROCEDURE' , { {    0,   1, 'FUNCTION', NIL, NIL }, {    0,   1, 'PROCEDURE', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    0,   1, 'CLASS', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    0,   0, '_CLASS_IMPLEMENTATION_', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLARED' , { {    1,   0, 'METHOD', '<', NIL }, {    2,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'DECLARED' , { {    1,   0, 'PROCEDURE', '<', NIL }, {    2,   0, NIL, '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ACCESS' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'ASSIGN' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'CLASS', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'METHOD' , { {    1,   0, NIL, '!', NIL }, {    2,   0, ':', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'FRIEND' , { {    1,   0, 'CLASS', '<', NIL }, { 1002,   1, ',', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'FRIEND' , { {    1,   0, 'FUNCTION', '<', NIL }, { 1002,   1, ',', '<', NIL } } , .T. } )
      aAdd( aCommRules, { 'EXPORTED' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'EXPORT' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'VISIBLE' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PUBLIC' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'HIDDEN' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PRIVATE' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PROTECTED' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'PUBLISHED' , { {    0,   0, ':', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'INLINE' , { {    1,   0, 'METHOD', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'INLINE' , { {    1,   0, 'METHOD', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aCommRules, { 'ENDMETHOD' ,  , .F. } )

 #endif

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitClsResults()

 /*
    Cliper can not comile even if protected by an #ifdef,
    we are forced to use combination of #ifdef/#include
  */
 #ifdef __HARBOUR__
    #include "clsresults.ch"
 #endif

RETURN .T.

//--------------------------------------------------------------//
FUNCTION PP_QSelf( o )

   STATIC s_oSelf
   LOCAL  oPreset := s_oSelf

   IF ValType( o ) == 'O'
      s_oSelf := o
      RETURN oPreset
   ENDIF

RETURN s_oSelf

//--------------------------------------------------------------//
FUNCTION AtInRules( cFind, sLine, nStart )

   LOCAL nAt, nLen := Len( sLine ), cChar

   IF nStart == NIL
      nStart := 1
   ENDIF

   FOR nAt := nStart TO nLen
      cChar := SubStr( sLine, nAt, 1 )

      IF cChar $ '"'+"'"
         DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
         ENDDO
      ELSEIF cChar == '\'
         // Skip next [Escaped] char
         nAt++
      ELSEIF cChar == cFind
         IF cFind == "<" .AND. SubStr( sLine, nAt + 1, 1 ) $ "=>"
            // Clipper sees it as a double char token "<=" or "<>" and does not accept it!
         ELSE
            RETURN nAt
         ENDIF
      ENDIF
   NEXT

RETURN 0

//--------------------------------------------------------------//
#ifndef __XHARBOUR__
FUNCTION AtSkipStrings( sFind, sLine, nStart, bRule )

   LOCAL nAt, nLen := Len( sLine ), cChar, cLastChar := ' ', nLenFind := Len( sFind )

   IF nStart == NIL
      nStart := 1
   ENDIF

   IF bRule == NIL
      bRule := .F.
   ENDIF

   FOR nAt := nStart TO nLen
       IF SubStr( sLine, nAt, nLenFind ) == sFind
          RETURN nAt
       ENDIF

       cChar := SubStr( sLine, nAt, 1 )

       IF cChar $ '"'+"'"
          DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
          ENDDO
          LOOP // No need to record cLastChar
       ELSEIF cChar == '['
          IF ! ( bRule .OR. IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "])}_." )
             DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != ']'
             ENDDO
             cLastChar := ']'
             LOOP // Recorded cLastChar
          ENDIF
       ENDIF

       cLastChar := cChar
    NEXT

RETURN 0
#endif

//--------------------------------------------------------------//
FUNCTION nAtAnyCharSkipStr( sChars, sLine, nStart )

   LOCAL nAt, nLen := Len( sLine ), cChar, cLastChar := ' '
   LOCAL lRule

   IF nStart == NIL
      nStart := 1
   ENDIF

   IF ProcName( 1 ) == "COMPILERULE"
      lRule := .T.
   ELSE
      lRule := .F.
   ENDIF

   FOR nAt := nStart TO nLen
       IF SubStr( sLine, nAt, 1 ) $ sChars
          RETURN nAt
       ENDIF

       cChar := SubStr( sLine, nAt, 1 )

       IF cChar $ '"'+"'"
          DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != cChar
          ENDDO
          LOOP // No need to record cLastChar
       ELSEIF lRule == .F. .AND. cChar == '['
          IF ! ( IsAlpha( cLastChar ) .OR. IsDigit( cLastChar ) .OR. cLastChar $ "])}_." )
             DO WHILE ( nAt < nLen ) .AND. SubStr( sLine, ++nAt, 1 ) != ']'
             ENDDO
          ENDIF
          cLastChar := ']'
          LOOP // Recorded cLastChar
       ENDIF

       cLastChar := cChar
    NEXT

RETURN 0

//--------------------------------------------------------------//

FUNCTION PP_ModuleName( sNewModule )

   LOCAL sModule := s_sModule

   IF PCount() > 0
      s_sModule := sNewModule
   ENDIF

RETURN sModule

//--------------------------------------------------------------//

STATIC FUNCTION InitRunRules()

   /* Defines */

   /* Translates */
   aAdd( aTransRules, { '.' , { {    0,   0, '.', NIL, NIL }, {    0,   0, '.', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    1,   0, NIL, ':', { 'ANYTYPE', 'ARRAY', 'CHARACTER', 'CODEBLOCK', 'DATE', 'LOGICAL', 'NUMERIC', 'OBJECT', 'STRING', 'USUAL' } } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    0,   0, 'ARRAY', NIL, NIL }, {    1,   0, 'OF', '<', NIL } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    1,   0, 'CLASS', '!', NIL } } , .F. } )
   aAdd( aTransRules, { 'AS' , { {    1,   0, 'CLASS', '!', NIL }, {    0,   0, ':=', NIL, NIL } } , .F. } )

   #ifndef __CONCILE_PCODE__
      aAdd( aTransRules, { 'QSELF' , { {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'ADDMETHOD' , { {    1,   0, '(', '<', NIL }, {    0,   0, ',', NIL, NIL }, {    2,   0, '@', '!', NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL }, {    3,   0, ',', '<', NIL }, {    4,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   #endif

   aAdd( aTransRules, { ':' , { {    0,   0, ':', NIL, NIL } } , .F. } )

   aAdd( aTransRules, { '_GET_' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ',', NIL, NIL }, {    3,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    4,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    5,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )
   #ifndef __HARBOUR__
      aAdd( aTransRules, { '__GET' , { {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL }, {    0,   0, 'DISPLAY', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   #endif

   #ifndef __CONCILE_PCODE__
      aAdd( aTransRules, { 'PROCNAME' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'PROCLINE' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'HB_ENUMINDEX' , { {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'ERRORBLOCK' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { 'THROW' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
      aAdd( aTransRules, { '__QUIT' , { {    0,   0, '(', NIL, NIL }, {    1,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .T. } )
   #endif

   aAdd( aTransRules, { 'IN' , { {    1,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aTransRules, { 'IF' , { {    1,   0, '(', '<', NIL }, {    0,   0, ',', NIL, NIL }, {    2,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    3,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )

   #ifdef AX
      aAdd( aTransRules, { 'RESPONSE' , { {    0,   0, '.', NIL, NIL } } , .T. } )
   #endif

   /* Commands */
   aAdd( aCommRules, { '_HB_CLASS' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { '_HB_MEMBER' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'MEMVAR' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'EXTERNAL' , { {    1,   0, NIL, '!', NIL }, { 1002,   1, ',', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '!', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '!', NIL }, {    2,   0, 'WITH', '<', NIL }, { 1003,   1, ',', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'IF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSEIF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSE' ,  , .F. } )
   aAdd( aCommRules, { 'ENDIF' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'END' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    0,   0, 'CASE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'SWITCH' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'CASE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'OTHERWISE' ,  , .F. } )
   aAdd( aCommRules, { 'DEFAULT' ,  , .F. } )
   aAdd( aCommRules, { 'ENDCASE' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'FOR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ':=', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, 'STEP', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'FOR' , { {    1,   0, NIL, '<', NIL }, {    2,   0, '=', '<', NIL }, {    3,   0, 'TO', '<', NIL }, {    4,   1, 'STEP', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'FOR' , { {    1,   0, 'EACH', '<', NIL }, {    2,   0, '$', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ENDFOR' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'LOOP' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'EXIT' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'NEXT' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, 'WHILE', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'WHILE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ENDDO' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'TRY' ,  , .F. } )
   aAdd( aCommRules, { 'CATCH' , { {    1,   1, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'FINALLY' ,  , .F. } )
   aAdd( aCommRules, { 'BEGIN' , { {    0,   0, 'SEQUENCE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'BREAK' , { {    1,   1, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'RECOVER' , { {    1,   1, 'USING', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'WITH' , { {    1,   0, 'OBJECT', '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL }, {    2,   0, 'WITH', 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'INIT' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'EXIT' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'FUNCTION', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, 'FUNCTION', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'PROCEDURE' , { {    1,   0, NIL, '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'PROCEDURE' , { {    1,   0, NIL, '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'FUNCTION' , { {    1,   0, NIL, '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'FUNCTION' , { {    1,   0, NIL, '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    0,   0, 'STATIC', NIL, NIL }, {    1,   0, 'PROCEDURE', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    0,   0, 'STATIC', NIL, NIL }, {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    0,   0, 'STATIC', NIL, NIL }, {    1,   0, 'FUNCTION', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    0,   0, 'STATIC', NIL, NIL }, {    1,   0, 'FUNCTION', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    1,   0, 'PROCEDURE', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    1,   0, 'FUNCTION', '!', NIL }, {    2,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'UTILITY' , { {    1,   0, 'FUNCTION', '!', NIL }, {    0,   1, '(', NIL, NIL }, {    0,  -1, ')', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'RETURN' , { {    1,   1, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'PARAMETERS' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'PRIVATE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DECLARE' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'PUBLIC' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'LOCAL' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'STATIC' , { {    1,   0, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'FIELD' , { {    1,   0, NIL, '!', NIL }, {    2,   1, ',', 'A', NIL } } , .F. } )

   IF aScan( aDefRules, {|aDefine| aDefine[1] == "WIN" } ) > 0
      aAdd( aCommRules, { 'ALERT' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   ENDIF

RETURN .T.

//--------------------------------------------------------------//

STATIC FUNCTION InitRunResults()

   /* Defines Results*/

   /* Translates Results*/
   aAdd( aTransResults, { , ,  } )
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { , , { NIL }  } )
   aAdd( aTransResults, { { {   0, ':=' } }, { -1} , { NIL }  } )

   #ifndef __CONCILE_PCODE__
      aAdd( aTransResults, { { {   0, 'PP_Qself()' } }, { -1} ,  } )
      aAdd( aTransResults, { { {   0, 'AddInLine( ' }, {   0,   1 }, {   0, ', {|Self,p1,p2,p3,p4,p5,p6,p7,p8,p9| Eval( {|s| HB_SetWith( PP_QSelf(s) ) }, Self ), PP_ExecMethod( ' }, {   0,   2 }, {   0, ', p1,p2,p3,p4,p5,p6,p7,p8,p9 ), Eval( {|| PP_QSelf( HB_SetWith() ) } ) }, ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, { -1,  1, -1,  3, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   #endif

   aAdd( aTransResults, { { {   0, 'Self:' } }, { -1} ,  } )

   aAdd( aTransResults, { { {   0, '__GET( MEMVARBLOCK(' }, {   0,   2 }, {   0, '), ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   #ifndef __HARBOUR__
      aAdd( aTransResults, { { {   0, '__GET(' }, {   0,   1 }, {   0, ')' } }, { -1,  1, -1} , { NIL }  } )
   #endif

   #ifndef __CONCILE_PCODE__
      aAdd( aTransResults, { { {   0, 'PP_ProcName( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
      aAdd( aTransResults, { { {   0, 'PP_ProcLine( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
      aAdd( aTransResults, { { {   0, 'PP_EnumIndex()' } }, { -1} ,  } )
      aAdd( aTransResults, { { {   0, 'PP_ErrorBlock( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
      aAdd( aTransResults, { { {   0, 'PP__THROW( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
      aAdd( aTransResults, { { {   0, 'Break( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   #endif

   aAdd( aTransResults, { { {   0, '$ ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, 'IIF( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )

   #ifdef AX
      aAdd( aTransResults, { { {   0, 'ReSpOnSe:' } }, { -1} ,  } )
   #endif

   /* Commands Results*/
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, '()' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, '( @' }, {   0,   2 }, {   0, '' }, {   3, ', @' }, {   3,   3 }, {   0, ' )' } }, { -1,  1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__IF ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ELSEIF ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ELSE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDIF ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__END ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__DOCASE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__SWITCH ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__CASE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__OTHERWISE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__DEFAULT' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDCASE ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FOR ' }, {   0,   1 }, {   0, ':=' }, {   0,   2 }, {   0, '~TO~' }, {   0,   3 }, {   0, '~STEP~' }, {   0,   4 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FOR ' }, {   0,   1 }, {   0, ':=' }, {   0,   2 }, {   0, '~TO~' }, {   0,   3 }, {   0, '~STEP~' }, {   0,   4 } }, { -1,  1, -1,  1, -1,  1, -1,  1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FOREACH ' }, {   0,   1 }, {   0, '~$~' }, {   0,   2 } }, { -1,  1, -1,  1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__NEXT ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__LOOP ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__EXIT ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__NEXT ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__WHILE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__WHILE ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__ENDDO ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__TRY' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__CATCH ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__FINALLY' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP__BEGIN' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PP_Break( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__RECOVER ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__WITHOBJECT ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Run( ' }, {   0,   1 }, {   0, ' + ".prg" )' } }, { -1,  2, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Run( ' }, {   0,   1 }, {   0, ' + ".prg", {' }, {   0,   2 }, {   0, '} )' } }, { -1,  2, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_INIT ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_EXIT ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC_PRG ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 }, {   0, ' ; PP__LocalParams( { ' }, {   0,   2 }, {   0, ' } )' } }, { -1,  1, -1,  3, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_PROC ' }, {   0,   1 } }, { -1,  1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Return( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Params( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Privates( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Privates( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Publics( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Locals( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP__Statics( { ' }, {   0,   1 }, {   0, ' } )' } }, { -1,  3, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, ' ' }, {   0,   1 }, {   0, ' ' }, {   0,   2 } }, { -1,  0, -1,  0} , { NIL, NIL }  } )

   IF aScan( aDefRules, {|aDefine| aDefine[1] == "WIN" } ) > 0
      aAdd( aCommResults, { { {   0, 'MessageBox( 0, CStr( ' }, {   0,   1 }, {   0, ' ), "xBaseScript for Windows", 0 )' } }, { -1,  1, -1} , { NIL }  } )
   ENDIF

RETURN .T.

//--------------------------------------------------------------//
STATIC FUNCTION InitDotRules()

   /* Defines */

   /* Translates */
   aAdd( aTransRules, { '_GET_' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, ',', NIL, NIL }, {    3,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    4,   1, NIL, '<', { ',' } }, {    0,   0, ',', NIL, NIL }, {    5,   1, NIL, '<', { ')' } }, {    0,   0, ')', NIL, NIL } } , .F. } )

   #ifndef __HARBOUR__
      aAdd( aTransRules, { '__GET' , { {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ':', NIL, NIL }, {    0,   0, 'DISPLAY', NIL, NIL }, {    0,   0, '(', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
      aAdd( aTransRules, { 'AADD' , { {    0,   0, '(', NIL, NIL }, {    0,   0, 'GETLIST', NIL, NIL }, {    0,   0, ',', NIL, NIL }, {    0,   0, '__GET', NIL, NIL }, {    1,   0, '(', 'A', NIL }, {    0,   0, ')', NIL, NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   #endif

   /* Commands */
   aAdd( aCommRules, { 'CLS' ,  , .F. } )
   aAdd( aCommRules, { 'BROWSE' ,  , .F. } )
   aAdd( aCommRules, { 'LIST' , { {    0,   0, 'STRUCTURE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'DISPLAY' , { {    0,   0, 'STRUCTURE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'MODIFY' , { {    0,   0, 'STRUCTURE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'EXIT' ,  , .F. } )
   aAdd( aCommRules, { 'QUIT' ,  , .F. } )
   aAdd( aCommRules, { 'IF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSEIF' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'ELSE' ,  , .F. } )
   aAdd( aCommRules, { 'ENDIF' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'END' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    0,   0, 'CASE', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'CASE' , { {    1,   0, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { 'OTHERWISE' ,  , .F. } )
   aAdd( aCommRules, { 'ENDCASE' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL } } , .F. } )
   aAdd( aCommRules, { 'DO' , { {    1,   0, NIL, '<', NIL }, {    0,   0, '.', NIL, NIL }, {    0,   0, 'PRG', NIL, NIL }, {    2,   0, 'WITH', 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'CD' , { {    1,   0, NIL, '(', NIL } } , .F. } )

RETURN .T.

//--------------------------------------------------------------//
STATIC FUNCTION InitDotResults()

   /* Defines Results*/

   /* Translates Results*/
   aAdd( aTransResults, { { {   0, '__GET( MEMVARBLOCK(' }, {   0,   2 }, {   0, '), ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, { -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )

   #ifndef __HARBOUR__
      aAdd( aTransResults, { { {   0, '__GET( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
      aAdd( aTransResults, { { {   0, '__oGet := __GET(' }, {   0,   1 }, {   0, ') ; aAdd( GetList, __oGet ) ; __oGet:Display()' } }, { -1,  1, -1} , { NIL }  } )
   #endif

   /* Commands Results*/
   aAdd( aCommResults, { { {   0, 'Scroll( 2, 0, MaxRow() - 1, MaxCol() )                          ; SetPos( 2, 0 )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'Browse( 1, 0, MaxRow() - 1, MaxCol() )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'DISPLAY STRUCTURE' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'M->__nArea := Select() ; COPY STRUCTURE EXTENDED TO _$struct$_ ; USE _$struct$_ ALIAS NewStructure READONLY NEW ; Browse() ; CLOSE ; SELECT ( M->__nArea )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'M->__nArea := Select() ; M->__cFile := dbInfo( 10 ) ; CLOSE ; dbModifyStructure( M->__cFile ) ; SELECT ( M->__nArea ) ; USE ( M->__cFile )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__QUIT()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__QUIT()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__SetIf( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '__SetElseIf( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '__SetElse()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__SetEnd() ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '__SetEnd() ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '__SetDoCase()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__SetCase( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, '__SetOtherwise()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, '__SetEndCase() ' }, {   1, ' ' }, {   1,   1 } }, { -1, -1,  0} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Run( ' }, {   0,   1 }, {   0, ' + ".prg" )' } }, { -1,  2, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'PP_Run( ' }, {   0,   1 }, {   0, ' + ".prg", {' }, {   0,   2 }, {   0, '} )' } }, { -1,  2, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'DirChange( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  4, -1} , { NIL }  } )

RETURN .T.

//--------------------------------------------------------------//
PROCEDURE PP_RunInit( aProcedures, aInitExit, nLine )

   IF ValType( aProcedures ) != 'A' .OR. ValType( aInitExit ) != 'A'
      Eval( ErrorBlock(), ErrorNew( [PP], 0, 9004, [Run-Init], [Invalid parameters], { aProcedures, aInitExit, nLine } ) )
   ELSE
      aSize( aProcedures, 0 )

      aSize( aInitExit, 2 )
      aInitExit[1] := {}
      aInitExit[2] := {}
   ENDIF

   PP_InitStd()
   PP_LoadRun()

RETURN

//--------------------------------------------------------------//
FUNCTION PP_PreProText( sLines, asLines, bBlanks, bAutoCompile, nStartLine, sSource )

   LOCAL nOpen, nClose, sTemp := "", nLine, nLines

   //TraceLog( sLines )

   IF bBlanks == NIL
      bBlanks := .T.
   ENDIF

   IF bAutoCompile == NIL
      bAutoCompile := bCompile
   ELSE
      bCompile := bAutoCompile
   ENDIF

   IF asLines == NIL
      asLines := {}
   ENDIF

   IF nStartLine == NIL
      nStartLine := 0
   ENDIF

   IF sSource == NIL
      sSource := ""
   ENDIF

   sLines := StrTran( sLines, Chr(13), " " )
   sLines := StrTran( sLines, Chr(9), " " )

   WHILE ( nOpen := AtSkipStrings( "/*", sLines ) ) > 0
      sTemp += Left( sLines, nOpen - 1 )
      nClose := AtSkipStrings( "*/", sLines, nOpen + 2 )
      WHILE ( nOpen := AtSkipStrings( Chr(10), sLines, nOpen + 1 ) ) > 0 .AND. nOpen < nClose
         sTemp += Chr(10)
      ENDDO
      sLines := SubStr( sLines, nClose + 2 )
   ENDDO
   sLines := ( sTemp + sLines )

   nOpen  := 0
   nClose := 0

   WHILE ( nOpen := At( Chr(10), sLines, nOpen + 1 ) ) > 0 .AND. nOpen > nClose
      //TraceLog( sLines, Len( sLines ), nOpen, nClose )
      aAdd( asLines, RTrim( LTrim( SubStr( sLines, nClose + 1, nOpen - ( nClose + 1 ) ) ) ) )
      //TraceLog( Len( asLines ), aTail( asLines ) )
      nClose := nOpen
   ENDDO
   IF Len( sLines ) > nClose
      aAdd( asLines, RTrim( LTrim( SubStr( sLines, nClose + 1 ) ) ) )
      //TraceLog( Len( asLines) , aTail( asLines ) )
   ENDIF

   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      sTemp := asLines[nLine]
      //TraceLog( sTemp )

      DO WHILE Empty( sTemp ) .OR. Left( sTemp, 1 ) == '*'
         //TraceLog( nLine, nLines, sTemp )
         IF bBlanks
            asLines[nLine] := NIL
            nLine++
            IF nLine > nLines
               EXIT
            ENDIF
            sTemp := asLines[nLine]
         ELSE
            aDel( asLines, nLine )
            nLines--
            aSize( asLines, nLines )
            IF nLine > nLines
               EXIT
            ENDIF
         ENDIF
      ENDDO

         IF nLine > nLines
             EXIT
         ENDIF

      //TraceLog( nLine, nLines, sTemp )

      nOpen := AtSkipStrings( "&&", sTemp )
      IF nOpen > 0
         IF nOpen == 1
           IF bBlanks
              asLines[nLine] := NIL
              LOOP
           ELSE
              aDel( asLines, nLine )
              nLine--
              nLines--
              aSize( asLines, nLines )
              LOOP
            ENDIF
         ENDIF
         sTemp := Left( asLines[nLine], nOpen - 1 )
      ELSE
         sTemp := asLines[nLine]
      ENDIF

      nOpen := AtSkipStrings( "//", sTemp )
      IF nOpen > 0
         IF nOpen == 1
            IF bBlanks
               asLines[nLine] := NIL
               //TraceLog( "LOOP" )
               LOOP
            ELSE
               aDel( asLines, nLine )
               nLine--
               nLines--
               aSize( asLines, nLines )
               LOOP
            ENDIF
         ENDIF
         sTemp := Left( sTemp, nOpen - 1 )
      ENDIF

      asLines[nLine] := sTemp
   NEXT

   sLines := ""

   IF nLines == 0
      RETURN ""
   ENDIF

   //TraceLog( nLines )

   // Don't process the last line for [;].
   nLines--
   FOR nLine := 1 TO nLines
      sTemp := asLines[nLine]
      //TraceLog( sTemp )
      IF sTemp == NIL
         LOOP
      ENDIF

      DO WHILE Right( sTemp, 1 ) == ';'
         IF bBlanks
            #ifdef __STR_INDEX__
               sTemp[-1] := ' '
            #else
               sTemp := Left( sTemp, Len( sTemp ) - 1 ) + ' '
            #endif
            asLines[nLine] := NIL
            nLine++
            IF asLines[nLine] == NIL
               asLines[nLine] := sTemp
            ELSE
               asLines[nLine] := sTemp + asLines[nLine]
            ENDIF
            sTemp := asLines[nLine]
         ELSE
            aDel( asLines, nLine )
            nLines--
            aSize( asLines, nLines )
            // nLine now points to the next line.
            sTemp := Left( sTemp, Len( sTemp ) - 1 ) + ' ' + asLines[nLine]
         ENDIF
      ENDDO

      sTemp := PP_PreProLine( sTemp, nStartLine + nLine, sSource )

      sLines += sTemp
      sLines += ";"

      IF sTemp == ""
         asLines[nLine] := NIL
      ELSE
         asLines[nLine] := sTemp
      ENDIF
      //TraceLog( nLine, sTemp )
   NEXT

   sTemp := asLines[nLine]

   IF sTemp != NIL
      sTemp := PP_PreProLine( sTemp, nStartLine + nLine, sSource )
      sLines += sTemp
   ENDIF

   IF sTemp == ""
      asLines[nLine] := NIL
   ELSE
      asLines[nLine] := sTemp
   ENDIF

   //TraceLog( nLine, sTemp, sLines )

RETURN sLines

//--------------------------------------------------------------//
FUNCTION PP_RunText( sLines, bPP, aParams )

   LOCAL aProcedures := {}, aInitExit := { {}, {} }, nProcId := 0, ;
         nLine, nLines, xRet, asLines := {}, nOpen, nClose, sLine

   s_bRTEBlock := s_bDefRTEBlock

   IF bPP == NIL
      bPP := .T.
   ENDIF

   IF bPP
      PP_PreProText( sLines, asLines )
   ELSE
      sLines := StrTran( sLines, Chr(13), "" )
      sLines := StrTran( sLines, Chr(9), " " )
      sLines := StrTran( sLines, ';', Chr(10) )
      nOpen  := 0
      nClose := 0
      WHILE ( nOpen := AtSkipStrings( Chr(10), sLines, nOpen + 1 ) ) > 0
         aAdd( asLines, SubStr( sLines, nClose + 1, nOpen - ( nClose + 1 ) ) )
         nClose := nOpen
      ENDDO
      IF Len( sLines ) > nClose
         aAdd( asLines, SubStr( sLines, nClose + 1 ) )
      ENDIF
   ENDIF

   PP_RunInit( aProcedures, aInitExit, @nLine )

   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      sLine := asLines[nLine]
      IF sLine != NIL
         PP_CompileLine( sLine, nLine, aProcedures, aInitExit, @nProcId )
      ENDIF
   NEXT

   xRet := PP_Exec( aProcedures, aInitExit, nProcId, aParams )

RETURN xRet

//--------------------------------------------------------------//
FUNCTION PP_RunArray( asLines, aParams )

   LOCAL aProcedures := {}, aInitExit := { {}, {} }, nProcId := 0, ;
         nLine, nLines

   LOCAL xRet

   s_bRTEBlock := s_bDefRTEBlock

   PP_RunInit( aProcedures, aInitExit, @nLine )

   nLines := Len( asLines )
   FOR nLine := 1 TO nLines
      IF asLines[nLine] != NIL
         PP_CompileLine( asLines[nLine], nLine, aProcedures, aInitExit, @nProcId )
      ENDIF
   NEXT

   xRet := PP_Exec( aProcedures, aInitExit, nProcId, aParams )

RETURN xRet

#ifdef __XHARBOUR__

  //--------------------------------------------------------------//
  FUNCTION PP_Eval( cExp, aParams, aProcedures, nLine, bScriptProc )

     LOCAL bErrHandler, oError, xRet
     LOCAL aPresetProcedures
     LOCAL nProc

     IF nLine == NIL
        nLine := 0
     ENDIF

     aPresetProcedures := s_aProcedures
     s_aProcedures     := aProcedures

     IF bScriptProc .AND. ( nProc := aScan( aProcedures, {|aProc| aProc[1] == cExp } ) ) > 0
        #ifdef __CONCILE_PCODE__
           &( s_aProcedures[ nProc ][1] )()
        #else
           PP_ExecProcedure( s_aProcedures, nProc )
        #endif

        s_aProcedures := aPresetProcedures

        RETURN s_xRet
     ElSE
        ErrorBlock( s_bInterceptRTEBlock )
     ENDIF

     TRY
        IF HB_IsArray( aParams )
           SWITCH Len( aParams )
              CASE 0
                 xRet := &cExp()
                 EXIT

              CASE 1
                 xRet := &cExp( aParams[1] )
                 EXIT

              CASE 2
                 xRet := &cExp( aParams[1], aParams[2] )
                 EXIT

              CASE 3
                 xRet := &cExp( aParams[1], aParams[2], aParams[3] )
                 EXIT

              CASE 4
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4] )
                 EXIT

              CASE 5
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5] )
                 EXIT

              CASE 6
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6] )
                 EXIT

              CASE 7
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6], aParams[7] )
                 EXIT

              CASE 8
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6], aParams[7], aParams[8] )
                 EXIT

              DEFAULT
                 xRet := &cExp( aParams[1], aParams[2], aParams[3], aParams[4], aParams[5], aParams[6], aParams[7], aParams[8], aParams[9] )
           END
        ELSE
           xRet := &cExp
        ENDIF
     CATCH oError
     END

     s_aProcedures := aPresetProcedures

     ErrorBlock( bErrHandler )

     IF oError != NIL
        Break( oError )
     ENDIF

  RETURN xRet

#endif

//--------------------------------------------------------------//

FUNCTION PP_Exec( aProcedures, aInitExit, nScriptProcs, aParams, nStartup )

   LOCAL nProc, nProcs, xRet
   LOCAL bErrHandler, oError
   LOCAL bPreset, aPresetProcedures
   LOCAL aStack

   #ifdef __CONCILE_PCODE__
      LOCAL cParamList, xParam
   #endif

   #ifdef DEBUG_PCODE
      LOCAL c
   #endif

   IF ValType( aParams ) == 'A'
      s_aParams := aParams
   ELSE
      s_aParams := {}
   ENDIF

   IF nStartup == NIL
      nStartup := 1
   ENDIF

   IF aProcedures == s_aProcedures
      bPreset := .F.
   ELSE
      bPreset := .T.

      aPresetProcedures := s_aProcedures
      s_aProcedures := aProcedures
   ENDIF

   bErrHandler := ErrorBlock( s_bInterceptRTEBlock )

   BEGIN SEQUENCE
      nProcs := Len( aInitExit[1] )

      FOR nProc := 1 TO nProcs
         #ifdef __CONCILE_PCODE__
            //TraceLog( aInitExit[1][nProc], aProcedures[ aInitExit[1][nProc] ][1] )
            &( aProcedures[ aInitExit[1][nProc] ][1] )()
         #else
            PP_ExecProcedure( aProcedures, aInitExit[1][nProc] )
         #endif
      NEXT

      FOR nProc := nStartup TO nScriptProcs
         IF aScan( aInitExit[1], nProc ) == 0 .AND. aScan( aInitExit[2], nProc ) == 0
            #ifdef __CONCILE_PCODE__
               IF ! Empty( s_aParams )
                  cParamList := ""

                  FOR EACH xParam IN s_aParams
                     IF HB_ISSTRING( xParam )
                        IF ! xParam[1] IN '"'+ "'\["
                           xParam := '"' + xParam + '"'
                        ENDIF
                     ELSE
                        xParam := ValToPrgExp( xParam )
                     ENDIF

                     cParamList += xParam + ','
                  NEXT

                  cParamList[-1] := ' '

                  xRet := &( aProcedures[nProc][1] + "(" +  cParamList + ")" )
               ELSE
                  xRet := &( aProcedures[nProc][1] )()
               ENDIF

            #else
               xRet := PP_ExecProcedure( aProcedures, nProc, s_aParams )
            #endif
            EXIT
         ENDIF
      NEXT

      nProcs := Len( aInitExit[2] )
      FOR nProc := 1 TO nProcs
         #ifdef __CONCILE_PCODE__
            //TraceLog( aInitExit[2][nProc], aProcedures[ aInitExit[1][nProc] ][1] )
            &( aProcedures[ aInitExit[2][nProc] ][1] )()
         #else
            PP_ExecProcedure( aProcedures, aInitExit[2][nProc] )
         #endif
      NEXT

   RECOVER USING oError

      //TraceLog( oError, IIF( oError:ClassName == "ERROR", ValToPrg( { oError:Description, oError:Operation, oError:ProcName, oError:ProcLine, oError:aaStack } ), "Not Error!" ) )

      #ifdef __XHARBOUR__
        IF oError:ClassName == "ERROR"
           FOR EACH aStack IN oError:aaStack
              IF aScan( aProcedures, {|__aProc| /*TraceLog( __aProc[1], aStack[2] ),*/ Upper( __aProc[1] ) == aStack[2] } ) > 0
                 oError:ModuleName := s_sFile
                 oError:ProcName := aStack[2]
                 oError:ProcLine := aStack[3]

                 //OutputDebugString( "File: '" + oError:ModuleName + "' Proc: '" + oError:ProcName + "' Line: " + Str( oError:ProcLine ) )
                 EXIT
              ENDIF
           NEXT
        ENDIF
      #endif

      #ifdef DEBUG_PCODE
         TraceLog( aProcedures[nProc][1], aProcedures[nProc][2] )

         FOR EACH c IN aProcedures[nProc][2]
           TraceLog( HB_EnumIndex(), c, Str( Asc( c ), 3 ) )
         NEXT
      #endif

   END SEQUENCE

   ErrorBlock( bErrHandler )

   s_aProcedures := aPresetProcedures

   IF oError != NIL
      Break( oError )
   ENDIF

RETURN xRet

//--------------------------------------------------------------//
PROCEDURE PP_ResetRules()

   aDefRules     := {}; aDefResults   := {}
   aTransRules   := {}; aTransResults := {}
   aCommRules    := {}; aCommResults  := {}

   s_lRunLoaded := .F.
   s_lClsLoaded := .F.
   s_lFWLoaded  := .F.

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_InitStd()

   InitRules()
   InitResults()

   CompileDefine( "__PP__" )
   #ifdef __HARBOUR__
      CompileDefine( "__HARBOUR__" )
   #endif
   #ifdef __XHARBOUR__
      CompileDefine( "__XHARBOUR__" )
   #endif

   s_lRunLoaded := .F.
   s_lClsLoaded := .F.
   s_lFWLoaded  := .F.

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_LoadRun()

   IF ! s_lRunLoaded
      s_lRunLoaded := .T.

      InitRunRules()
      InitRunResults()
   ENDIF

RETURN

//--------------------------------------------------------------//
PROCEDURE PP_LoadDot()

   IF ! s_lDotLoaded
      s_lDotLoaded := .T.

      InitDotRules()
      InitDotResults()
   ENDIF

RETURN

//--------------------------------------------------------------//
FUNCTION PP_RecoveryBlock( bRetryRecovery )

   LOCAL bPresetBlock

   IF PCount() > 0
      bPresetBlock := s_bExternalRecovery
      s_bExternalRecovery := bRetryRecovery
      RETURN bPresetBlock
   ENDIF

RETURN s_bExternalRecovery

//--------------------------------------------------------------//
PROCEDURE PP_Warning( cMsg )
   ? cMsg
RETURN

//--------------------------------------------------------------//
FUNCTION PP_Version()

   #ifdef SQL
      RETURN s_cVer + " Enterprise"
   #else
      #ifdef ZIP
         RETURN s_cVer + " Professional"
      #else
         RETURN s_cVer
      #endif
   #endif

//--------------------------------------------------------------//
FUNCTION PP_EnumIndex()

RETURN s_anEnumIndex[ s_nForEachIndex ]

//--------------------------------------------------------------//
FUNCTION PP_DefaultErrorBlock( bNewBlock )

   LOCAL bRet := s_bDefRTEBlock

   IF PCount() > 0
      s_bDefRTEBlock := bNewBlock
   ENDIF

RETURN bRet

//--------------------------------------------------------------//
FUNCTION PP_ErrorBlock( bNewBlock )

   LOCAL bRet := s_bRTEBlock

   IF PCount() > 0
      s_bRTEBlock := bNewBlock

      IF s_lTrying
         s_lTrying := .F.
         s_aSequence[ Len( s_aSequence ) ][2] := .F.
      ENDIF
   ENDIF

RETURN bRet

//--------------------------------------------------------------//
FUNCTION PP_InterceptRTEBlock( bNewBlock )

   LOCAL bRet := s_bInterceptRTEBlock

   IF PCount() > 0
      s_bInterceptRTEBlock := bNewBlock
   ENDIF

RETURN bRet

//--------------------------------------------------------------//
STATIC FUNCTION DefRTEHandler( e )

   LOCAL cMessage, aOptions, nChoice, nProc, bPrevHandler

   IF e:genCode == EG_ZERODIV .AND. e:CanSubStitute
     RETURN 0
   ENDIF

   IF e:genCode == EG_OPEN .and. e:osCode == 32 .and. e:CanDefault
     NetErr( .T. )
     RETURN .F.
   ENDIF

   IF e:genCode == EG_APPENDLOCK .and. e:CanDefault
     NetErr( .T. )
     RETURN .F.
   ENDIF

   nProc := 0
   DO WHILE ! Empty( ProcName( ++nProc ) )
      IF ProcName( nProc ) == ProcName()
         TraceLog( "*** Nest Error! ***" )
         Alert( "Nested Error at: " + ProcName(nProc) + "(" + Str( ProcLine( nProc ) ) + ")" )
         ErrorLevel(2)
         Break( e )
      ENDIF
   END

   bPrevHandler := ErrorBlock( s_bDefRTEBlock )

   TraceLog( "*** Error! ***" )
   cMessage := PP_ErrorMessage( e )
   TraceLog( cMessage )

   aOptions := { "Quit" }

   IF e:CanRetry
      aAdd( aOptions, "Retry" )
   ENDIF

   IF e:CanDefault
      aAdd( aOptions, "Default" )
   ENDIF

   nChoice := 0
   WHILE nChoice == 0
      nChoice := Alert( cMessage, aOptions )

      IF nChoice == NIL
         nChoice := 0
      ENDIF
   ENDDO

   ErrorBlock( bPrevHandler )

   IF nChoice > 0
      IF aOptions[nChoice] == "Retry"
         RETURN .T.
      ELSEIF aOptions[ nChoice ] == "Default"
         RETURN .F.
      ENDIF
   ENDIF

   OutErr( EOL + StrTran( cMessage, ";", EOL ) )

   ErrorLevel(1)
   Break( e )

#ifndef __XHARBOUR__
RETURN .F.
#endif

//--------------------------------------------------------------//
FUNCTION PP_ErrorMessage( e )

  LOCAL cMessage, nArg, nArgs, nPad, nLevel

  IF e:ClassName != "ERROR"
     //TraceLog( e )
     RETURN ProcName() + ": Argument is not an Error object. From: " + ProcName(1) + "(" + Str( ProcLine(1) ) + ")"
  ENDIF

  IF e:severity > ES_WARNING
     cMessage := "Error "
  ELSE
     cMessage := "Warning "
  ENDIF

  IF HB_ISSTRING( e:subsystem )
    cMessage += e:subsystem()
  ELSE
    cMessage += "???"
  ENDIF

  IF ValType( e:subCode ) == 'N'
    cMessage += "/" + LTrim( Str( e:subCode ) )
  ELSE
    cMessage += "/???"
  ENDIF

  IF HB_ISSTRING( e:description )
    cMessage += (";Description: " + e:description)
  ENDIF

  IF HB_ISSTRING( e:filename ) .AND. ! Empty( e:filename )
    cMessage += ";Offending file: " + e:filename
  ENDIF

  IF HB_ISSTRING( e:operation )
    cMessage += ";Operation: " + e:operation
  ENDIF

  IF ValType( e:osCode ) == 'N' .AND. ! Empty( e:osCode )
     cMessage += ";DOS Error: " + LTrim( Str( e:osCode ) )
  ENDIF

  nPad := Len( cMessage ) + 1
  IF ! Empty( e:args )
     nArgs := Len( e:args )
     cMessage += ";Arguments:"

     FOR nArg := 1 TO nArgs
        cMessage += Pad( ";#" + LTrim( Str( nArg ) ) + " Type: " + ValType( e:args[ nArg ] ) + " -> " + CStr( e:args[ nArg] ), nPad )
     NEXT
  ENDIF

  #ifdef __XHARBOUR__
     IF ! HB_ISSTRING( e:ModuleName )
        e:ModuleName := "Unspecified module"
     ENDIF

     IF ! HB_ISSTRING( e:ProcName )
        e:ProcName := "No explicit Procedure"
     ENDIF

     IF ValType( e:ProcLine ) != 'N'
        e:ProcLine := 0
     ENDIF

     cMessage += Pad( ";Error at: " + e:ModuleName + "->" + e:ProcName + "(" + LTrim( Str( e:ProcLine ) ) + ")", nPad )
  #endif

  FOR nLevel := 0 TO s_nProcStack - 1
     cMessage += Pad( ";Called from " + s_aProcStack[ s_nProcStack - nLevel ][1] + "(" + LTrim( Str( s_aProcStack[ s_nProcStack - nLevel ][2] ) ) + ")", nPad )
  NEXT

  nLevel := 2
  WHILE ! Empty( ProcName( nLevel ) )
     cMessage += Pad( ";*Called from " + ProcName( nLevel ) + "(" + LTrim( Str( ProcLine( nLevel ) ) ) + ")", nPad )
     nLevel++
  ENDDO

RETURN cMessage

//--------------------------------------------------------------//
#ifdef __CLIPPER__

FUNCTION ErrorNewX(  SubSystem, GenCode, SubCode, Operation, Description, Args, ModuleName )

   LOCAL oError := ErrorNew()

   TraceLog( SubSystem, GenCode, SubCode, Operation, Description, Args, ModuleName )

   oError:GenCode := GenCode
   oError:SubSystem := SubSystem
   oError:SubCode := SubCode
   oError:Operation := Operation
   oError:Description := Description
   oError:Args := Args
   oError:Severity := ES_ERROR

RETURN oError

#endif

//--------------------------------------------------------------//
#ifdef __HARBOUR__
   #include "xbs_harb.ch"

   //--------------------------------------------------------------//
   INIT PROCEDURE PPInit

      LOCAL nScreenWidth

      s_cVer += " Compiled: " + __DATE__
      s_cVer += " " + __TIME__

      /*
      HB_SetMacro( HB_SM_HARBOUR,   .T. )
      HB_SetMacro( HB_SM_XBASE,     .T. )
      HB_SetMacro( HB_SM_SHORTCUTS, .T. )
      */

      #ifdef __XHARBOUR__
        ASSOCIATE CLASS StringOle WITH TYPE CHARACTER


        IF Type( "HB_GT_WVT()" ) == "UI"
           nScreenWidth := &( "Wvt_GetScreenWidth()" )

           DO CASE
              CASE nScreenWidth >= 1024
                 &( "Wvt_SetFont" )( 'Terminal', 20, 10 )
              CASE nScreenWidth >= 800
                 &( "Wvt_SetFont" )( 'System', 16, 8, 600, 2 )
              OTHERWISE
                 &(" Wvt_SetFont" )( 'Terminal', 12, 6 )
           ENDCASE

           &( "Wvt_SetCodePage" )( 255 )  // #define OEM_CHARSET 255 - from wingdi.h
           //SetMode( 25, 80 )
        ELSE
           #include "hbgtinfo.ch"

           IF MaxRow() != HB_GtInfo( HB_GTI_VIEWPORTHEIGHT ) .OR. MaxCol() != HB_GtInfo( HB_GTI_VIEWPORTWIDTH )
              SetMode( HB_GtInfo( HB_GTI_VIEWPORTHEIGHT ) + 1, HB_GtInfo( HB_GTI_VIEWPORTWIDTH ) + 1 )
           ENDIF
        ENDIF
      #endif

      s_aPPContext := PP_InitContext()

   RETURN
   //--------------------------------------------------------------//

#else

   //--------------------------------------------------------------//
   INIT PROCEDURE PPInit

      LOCAL FileHandle

      FileHandle := FCreate('Trace.Log')
      FClose(FileHandle)

      s_aPPContext := PP_InitContext()

   RETURN

   //--------------------------------------------------------------//
   FUNCTION TraceLog(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15 )

      LOCAL FileHandle, ProcName, Counter := 1, aEntries

      aEntries := {p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15}

      FileHandle := FOpen( 'Trace.Log', 1 )

      FSeek(FileHandle, 0, 2)

      FWrite( FileHandle, '[' + ProcName(1) + '] (' + Str( Procline(1), 5 ) + ') Called from: '  + CRLF )

      DO WHILE ! ( ( ProcName := ProcName( ++Counter ) ) == '' )
         FWrite( FileHandle, space(30) + ProcName + '(' + Str( Procline( Counter), 5 ) + ')' + CRLF )
      ENDDO

      IF ! ( PP_ProcName(0) == "" )
         FWrite( FileHandle, "Interpreter:"  + CRLF )
         Counter := -1
         DO WHILE ! ( ( ProcName := PP_ProcName( ++Counter ) ) == "" )
            FWrite( FileHandle, space(30) + ProcName + '(' + Str( PP_Procline( Counter), 5 ) + ')' + CRLF )
         ENDDO
      ENDIF

      FOR Counter := 1 to PCount()
         FWrite( FileHandle, '>>>' + CStr( aEntries[Counter] ) + '<<<' + CRLF )
      NEXT

      FWrite( FileHandle, CRLF )

      FClose(FileHandle)

   RETURN .T.

   //--------------------------------------------------------------//
   FUNCTION CStr( xExp )

      SWITCH ValType( xExp )
         CASE "C"
            RETURN xExp

         CASE "D"
            RETURN dToc( xExp )

         CASE "L"
            RETURN IIF( xExp, '.T.', '.F.' )

         CASE "N"
            RETURN Str( xExp )

         CASE "M"
            RETURN xExp

         CASE "A"
            RETURN "{ Array of " +  LTrim( Str( Len( xExp ) ) ) + " Items }"

         CASE "B"
            RETURN '{|| Block }'

         CASE "O"
            RETURN "{ " + xExp:ClassName() + " Object }"

         CASE "U"
            RETURN 'NIL'

         DEFAULT
            RETURN "Type: " + ValType( xExp )
      ENDSWITCH

   RETURN ""

   //--------------------------------------------------------------//

#endif

//--------------------------------------------------------------//
#ifdef ADS
   INIT PROCEDURE ADSInit
      rddRegister( "ADS", 1 )
      //AdsSetFileType( ADS_ADT )
   RETURN
#endif

//--------------------------------------------------------------//
/*
Function Alert( cMsg )

   //? ProcName(1), ProcLine(1), cMsg
   TraceLog( cMsg )

return NIL
*/
