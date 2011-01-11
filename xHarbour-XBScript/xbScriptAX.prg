#include "hbclass.ch"

#define CRLF Chr(13) + Chr(10)

REQUEST GetRef

//--------------------------------------------------------------//
INIT PROCEDURE Init_xbScriptAX

   ErrorBlock( {|e| TraceLog( e ), Break( e ) } )
   PP_DefaultErrorBlock( {|e| Break( e ) } )
   //PP_RecoveryBlock( {|oErr| ResolveSiteGlobals( @oErr, oErr:Args ) } )

RETURN

//--------------------------------------------------------------//
FUNCTION __ResolveSiteGlobals( oErr )

   LOCAL xArg

   #if 0
      FOR EACH xArg IN oErr:Args
         TraceLog( HB_EnumIndex(), xArg:ClassName )
      NEXT

      TraceLog( oErr:SubSystem, oErr:Operation, oErr:Description, oErr:Args, oErr:ProcName, oErr:ProcLine )
   #endif

   IF oErr:SubCode == 1001 .AND. oErr:SubSystem == "BASE"
      RETURN ResolveSiteGlobals( @oErr, oErr:Args )
   ENDIF

RETURN Break( oErr )

#if 0
//--------------------------------------------------------------//
FUNCTION TraceLog( ... )

   LOCAL nLevel := SET( _SET_TRACESTACK ), ProcName, xParam

   IF ! SET( _SET_TRACE )
      RETURN .T.
   ENDIF

   IF nLevel > 0
      OutputDebugString( '[' + ProcFile(1) + "->" + ProcName( 1 ) + '] (' + LTrim( Str( Procline(1) ) ) + ')' )
   ENDIF

   IF nLevel > 1 .AND. ! ( ProcName( 2 ) == '' )
      OutputDebugString( ' Called from: '  + CRLF )
      nLevel := 1
      DO WHILE ! ( ( ProcName := ProcName( ++nLevel ) ) == '' )
         OutputDebugString( space(30) + ProcFile( nLevel ) + "->" + ProcName + '(' + LTrim( Str( Procline( nLevel ) ) ) + ')' + CRLF )
      ENDDO
   ELSE
      OutputDebugString( CRLF )
   ENDIF

   FOR EACH xParam IN HB_aParams()
      OutputDebugString( 'Type: ' + ValType( xParam ) + ' >>>' + CStr( xParam ) + '<<<' + CRLF )
   NEXT

   OutputDebugString( CRLF )

RETURN .T.

#endif

//--------------------------------------------------------------//
CLASS xbScriptAX FROM TInterpreter

   CLASSDATA nNextDynProc INIT 1

   DATA bInterceptRTEBlock INIT {|oErr| __ResolveSiteGlobals( @oErr ) }

   DATA aScriptHostGlobals   INIT {}
   DATA oGlobalInterpreter
   //DATA pDynList

   METHOD Compile( aAdoptCompiledProcs )

   METHOD ScriptSiteAddGlobal( cName, pDisp )

   METHOD ScriptSiteResetGlobals()

   METHOD Activate() INLINE IIF( ::nNextDynProc > Len( ::aCompiledProcs ), , ( ::nNextDynProc := Len( ::aCompiledProcs ) + 1, PP_GenDynProcedures( ::aCompiledProcs, ::nNextDynProc, @::pDynList ) ) )

   METHOD Reset()

ENDCLASS

//----------------------------------------------------------------------------//
METHOD Compile( oGlobalInterpreter ) CLASS xbScriptAX

   LOCAL xRet

   IF ( ! Empty( oGlobalInterpreter ) ) .AND. ! ( Self == oGlobalInterpreter )
      ::oGlobalInterpreter := oGlobalInterpreter

      ::nNextStartProc := Len( oGlobalInterpreter:aCompiledProcs ) + 1

      oGlobalInterpreter:AddText( ::cText, ::nStartLine )

      // Attach.
      ::aCompiledProcs := oGlobalInterpreter:aCompiledProcs

      // Simulate compilation.
      ::cText := ""

      xRet := oGlobalInterpreter:Super:Compile()
      //PP_GenDynProcedures( oGlobalInterpreter:aCompiledProcs, ::nNextStartProc, @oGlobalInterpreter:pDynList )
   ELSE
      xRet := ::Super:Compile()
      //PP_GenDynProcedures( ::aCompiledProcs, ::nNextStartProc, @::pDynList )
   ENDIF

   //OutputDebugString( ::cName + "!!!!!!!!!!!!!!!!!!" + Str( Len( ::aCompiledProcs ) ) )

RETURN xRet

//----------------------------------------------------------------------------//
METHOD ScriptSiteAddGlobal( cName, pDisp ) CLASS xbScriptAX

   LOCAL oGlobal := TOleAuto():New( pDisp, cName )

   aAdd( ::aScriptHostGlobals, { cName, pDisp, oGlobal } )

   __QQPub( cName )
   __MVPUT( cName, oGlobal )

   //TraceLog( cName, pDisp, oGlobal, Type( cName ) )

RETURN Self

//----------------------------------------------------------------------------//
METHOD ScriptSiteResetGlobals() CLASS xbScriptAX

   LOCAL aGlobal

   FOR EACH aGlobal IN ::aScriptHostGlobals
      __MVXRELEASE( aGlobal[1] )
   NEXT

   aSize( ::aScriptHostGlobals, 0 )

RETURN .T.

//----------------------------------------------------------------------------//
METHOD Reset() CLASS xbScriptAX

   LOCAL aProcedure

   //TraceLog( ::nId, ::cName )

   ::ScriptSiteResetGlobals()

   IF ! Empty( ::aCompiledProcs )
      PP_ResetStack( ::aCompiledProcs )
   ENDIF

   IF ::pDynList != NIL
      PP_ReleaseDynProcedures( 0, ::pDynList )
      ::pDynList := NIL
   ENDIF

   ::nNextDynProc := ::nNextStartProc

RETURN Self

#if 0
//----------------------------------------------------------------------------//
#pragma BEGINDUMP

	#include "hbapi.h"
	#include "hbvm.h"
	#include "classes.h"
	#include "hbapiitm.h"
	#include "hbapierr.h"
	#include "hbstack.h"

  #include <windows.h>

  #include "..\..\xHarbour-OleServer\source\oleserver.h"

  typedef struct
  {
     PHB_PCODEFUNC pDynFunc;
     PHB_DYNS pDynSym;
     PHB_FUNC pPresetFunc;
     HB_SYMBOLSCOPE cPresetScope;
  } DYN_PROC;

  typedef struct
  {
     int iProcs;
     DYN_PROC *pProcsArray;
  } DYN_PROCS_LIST;

  static DYN_PROCS_LIST *s_pDynList = NULL;

		/* Patch an address of the dynamic function */
	void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
	{
		HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatch(%p, %lu, %p)", pCode, ulOffset, Address));

		pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
		pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
		pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
		pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );
	}


	/* Intel specific ?? Patch an address relative to the next instruction */
	void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext )
	{
		ULONG ulBase;
		ULONG ulRelative;

		HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatchRelative(%p, %lu, %p, %lu)", pCode, ulOffset, Address, ulNext));

		ulBase = ( ULONG ) pCode + ulNext;
										/* Relative to next instruction */
		ulRelative = ( ULONG ) Address - ulBase;

		pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
		pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
		pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
		pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );
	}

   #if 0
   //---------------------------------------------------------------------------//
   HB_FUNC_STATIC( _GENDYNPROCEDURES )
   {
      static PHB_SYMB pSym_PP_ExecProcedure = NULL, pSym_HB_aParams = NULL;

      PHB_ITEM pProcedures = hb_param( 1, HB_IT_ARRAY );
      PHB_ITEM pxList;

      int iProcedures, iProcedure, iBase, iIndex, iPos;

      PHB_PCODEFUNC pDynFunc;
      PHB_DYNS pDynSym;
      DYN_PROCS_LIST *pDynList;

      if( pSym_PP_ExecProcedure == NULL )
      {
          pSym_PP_ExecProcedure = hb_dynsymFind( "PP_EXECPROCEDURE" )->pSymbol;
          pSym_HB_aParams       = hb_dynsymFind( "HB_APARAMS" )->pSymbol;
      }

      if( pProcedures )
      {
         iProcedures = (int) pProcedures->item.asArray.value->ulLen;
      }
      else
      {
         #ifdef _DEBUG
            OutputDebugValues( "*** EMPTY *** _GENDYNPROCEDURES()\n" );
         #endif
         hb_retnl( 0 );
         return;
      }

      iIndex = hb_parnl( 2 );

      if( iIndex )
      {
         iProcedure = iIndex - 1;
      }
      else
      {
         iProcedure = 0;
      }

      if( iProcedures - iProcedure == 0 )
      {
         #ifdef _DEBUG
            OutputDebugValues( "*** Nothing to process *** _GENDYNPROCEDURES()\n" );
         #endif
         hb_retnl( 0 );
         return;
      }

      pxList = hb_param( 3, HB_IT_BYREF );

      if( pxList )
      {
         if( HB_IS_POINTER( pxList ) )
         {
            pDynList = (DYN_PROCS_LIST *) pxList->item.asPointer.value;
         }
         else
         {
            pDynList = NULL;
         }
      }
      else
      {
         pDynList = s_pDynList;
      }

      if( pDynList )
      {
         iBase = pDynList->iProcs;
         pDynList->iProcs += iProcedures - iProcedure;
         pDynList = (DYN_PROCS_LIST *) hb_xrealloc( pDynList, sizeof(int) + ( sizeof( DYN_PROC ) * pDynList->iProcs ) );
      }
      else
      {
         iBase = 0;
         pDynList = (DYN_PROCS_LIST *) hb_xgrab( sizeof(int) + ( sizeof( DYN_PROC ) * ( iProcedures - iProcedure ) ) );
         pDynList->iProcs = iProcedures - iProcedure;

         if( pxList == NULL )
         {
            s_pDynList = pDynList;
         }
      }

      for( iPos = 0; iProcedure < iProcedures; iProcedure++, iPos++ )
      {
         char *sFunctionName = hb_arrayGetCPtr( pProcedures->item.asArray.value->pItems + iProcedure, 1 );
         BYTE *bCode = (BYTE *) hb_xgrab( 87 );

         iIndex = iProcedure + 1;

         #ifdef _DEBUG
            OutputDebugValues( "_GENDYNPROCEDURE: '%s' Pos: %i Index: %i\n", sFunctionName, iBase + iPos, iIndex );
         #endif

         /*
            PP_ExecProcedure( pProcedures, iIndex, HB_aParams() )

            ->

            hb_vmPushSymbol( pSym_ExecProcedure );

            hb_vmPushNil();

            hb_vmPushBaseArray( pProcedures->item.asArray.value );

            hb_vmPushLong( iIndex );

                hb_vmPushSymbol( p_Sym_HB_aParams );
                hb_vmPushNil();
                hb_vmFunction( 0 );

            hb_vmDo( 3 );
        */

        /*
		_DynProcedure:
		[00000000] 55                   push      ebp
		[00000001] 89E5                 mov       ebp,esp

		6:    hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
		[00000003] 6800000040           push      40000000
		[00000008] E800000000           call      _hb_vmPushSymbol
		[0000000D] 83C404               add       esp,+4

		8:    hb_vmPushNil();
		[00000010] E800000000           call      _hb_vmPushNil
        */

		bCode[  0] = 0x55;//              push      ebp
		bCode[  1] = 0x89;
		bCode[  2] = 0xE5;//              mov       ebp,esp

		//    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
		bCode[  3] = 0x68;
		bCode[  4] = 0x00;
		bCode[  5] = 0x00;
		bCode[  6] = 0x00;
		bCode[  7] = 0x00;//              push      00000000

		bCode[  8] = 0xE8;
		bCode[  9] = 0x00;
		bCode[ 10] = 0x00;
		bCode[ 11] = 0x00;
		bCode[ 12] = 0x00;//              call      _hb_vmPushSymbol

		bCode[ 13] = 0x83;
		bCode[ 14] = 0xC4;
		bCode[ 15] = 0x04;//              add       esp,+4

		//    hb_vmPushNil();
		bCode[ 16] = 0xE8;
		bCode[ 17] = 0x00;
		bCode[ 18] = 0x00;
		bCode[ 19] = 0x00;
		bCode[ 20] = 0x00;//              call      _hb_vmPushNil

        /*
		10:    hb_vmPushBaseArray( (PHB_BASEARRAY) 0x40000000 );
		[00000015] 6800000040           push      40000000
		[0000001A] E800000000           call      _hb_vmPushBaseArray
		[0000001F] 83C404               add       esp,+4
		*/

		//    hb_vmPushBasearray( 0x00000000 );
		bCode[ 21] = 0x68;
		bCode[ 22] = 0x00;
		bCode[ 23] = 0x00;
		bCode[ 24] = 0x00;
		bCode[ 25] = 0x00;//              push      00000000

		bCode[ 26] = 0xE8;
		bCode[ 27] = 0x00;
		bCode[ 28] = 0x00;
		bCode[ 29] = 0x00;
		bCode[ 30] = 0x00;//              call      _hb_vmPushBaseArray

		bCode[ 31] = 0x83;
		bCode[ 32] = 0xC4;
		bCode[ 33] = 0x04;//              add       esp,+4

        /*
		12:    hb_vmPushLong( 0x40000000 );
		[00000022] 6800000040           push      40000000
		[00000027] E800000000           call      _hb_vmPushLong
		[0000002C] 83C404               add       esp,+4
		*/

		//    hb_vmPushLong( iIndex );
		bCode[ 34] = 0x68;
		bCode[ 35] = 0x00;
		bCode[ 36] = 0x00;
		bCode[ 37] = 0x00;
		bCode[ 38] = 0x00;//              push      00000000

		bCode[ 39] = 0xE8;
		bCode[ 40] = 0x00;
		bCode[ 41] = 0x00;
		bCode[ 42] = 0x00;
		bCode[ 43] = 0x00;//              call      _hb_vmPushLong

		bCode[ 44] = 0x83;
		bCode[ 45] = 0xC4;
		bCode[ 46] = 0x04;//              add       esp,+4

        /*
		14:        hb_vmPushSymbol( (PHB_SYMB) 0x40000000 );
		[0000002F] 6800000040           push      40000000
		[00000034] E800000000           call      _hb_vmPushSymbol
		[00000039] 83C404               add       esp,+4

		15:        hb_vmPushNil();
		[0000003C] E800000000           call      _hb_vmPushNil

		16:        hb_vmFunction( 0 );
		[00000041] 6A00                 push      +0
		[00000043] E800000000           call      _hb_vmFunction
		[00000048] 83C404               add       esp,+4
		*/

		//    hb_vmPushSymbol( (PHB_SYMB) 0x00000000 );
		bCode[ 47] = 0x68;
		bCode[ 48] = 0x00;
		bCode[ 49] = 0x00;
		bCode[ 50] = 0x00;
		bCode[ 51] = 0x00;//              push      00000000

		bCode[ 52] = 0xE8;
		bCode[ 53] = 0x00;
		bCode[ 54] = 0x00;
		bCode[ 55] = 0x00;
		bCode[ 56] = 0x00;//              call      _hb_vmPushSymbol

		bCode[ 57] = 0x83;
		bCode[ 58] = 0xC4;
		bCode[ 59] = 0x04;//              add       esp,+4

		//    hb_vmPushNil();
		bCode[ 60] = 0xE8;
		bCode[ 61] = 0x00;
		bCode[ 62] = 0x00;
		bCode[ 63] = 0x00;
		bCode[ 64] = 0x00;//              call      _hb_vmPushNil

		//    hb_vmFunction( 0 );
		bCode[ 65] = 0x6A;
		bCode[ 66] = 0x00;//              push      +0

		bCode[ 67] = 0xE8;
		bCode[ 68] = 0x00;
		bCode[ 69] = 0x00;
		bCode[ 70] = 0x00;
		bCode[ 71] = 0x00;//              call      _hb_vmfunction

		bCode[ 72] = 0x83;
		bCode[ 73] = 0xC4;
		bCode[ 74] = 0x04;//              add       esp,+4

        /*
		18:    hb_vmDo( 3 );
		[0000004B] 6A03                 push      +3
		[0000004D] E800000000           call      _hb_vmDo
		[00000052] 83C404               add       esp,+4
        */

		//    hb_vmDo( 3 );
		bCode[ 75] = 0x6A;
		bCode[ 76] = 0x03;//              push      +3

		bCode[ 77] = 0xE8;
		bCode[ 78] = 0x00;
		bCode[ 79] = 0x00;
		bCode[ 80] = 0x00;
		bCode[ 81] = 0x00;//              call      _hb_vmDo

		bCode[ 82] = 0x83;
		bCode[ 83] = 0xC4;
		bCode[ 84] = 0x04;//              add       esp,+4

        /*
		@9:
		19: }
		[00000055] 5D                   pop       ebp
		[00000056] C3                   ret
        */

        bCode[ 85] = 0x5D;//              pop       ebp
        bCode[ 86] = 0xC3;//              ret

        /*
			Relocations for section #4

			offset    type        symbol
			00000009  REL32       _hb_vmPushSymbol
			00000011  REL32       _hb_vmPushNil
			0000001B  REL32       _hb_vmPushBaseArray
			00000028  REL32       _hb_vmPushLong
			00000035  REL32       _hb_vmPushSymbol
			0000003D  REL32       _hb_vmPushNil
			00000044  REL32       _hb_vmFunction
			0000004E  REL32       _hb_vmDo
        */

        hb_hrbAsmPatch( bCode, 4, pSym_PP_ExecProcedure );
        hb_hrbAsmPatchRelative( bCode, 9, hb_vmPushSymbol, 13 );

        hb_hrbAsmPatchRelative( bCode, 17, hb_vmPushNil, 21 );

        hb_hrbAsmPatch( bCode, 22, pProcedures->item.asArray.value );
        hb_hrbAsmPatchRelative( bCode, 27, hb_vmPushBaseArray, 31 );

        hb_hrbAsmPatch( bCode, 35, (void *) iIndex );
        hb_hrbAsmPatchRelative( bCode, 40, hb_vmPushLong, 44 );

        hb_hrbAsmPatch( bCode, 48, pSym_HB_aParams );
        hb_hrbAsmPatchRelative( bCode, 53, hb_vmPushSymbol, 57 );

        hb_hrbAsmPatchRelative( bCode, 61, hb_vmPushNil, 65 );
        hb_hrbAsmPatchRelative( bCode, 68, hb_vmFunction, 72 );
        hb_hrbAsmPatchRelative( bCode, 78, hb_vmDo, 82 );

        pDynSym = hb_dynsymGet( sFunctionName );

        //TraceLog( NULL, "Dyn: %p %s\n", pDynSym, sFunctionName );

        pDynFunc = (PHB_PCODEFUNC) hb_xgrab( sizeof( HB_PCODEFUNC ) );

        pDynFunc->pCode = bCode;
        pDynFunc->pSymbols = symbols;
        pDynFunc->pGlobals = NULL;

        pDynList->pProcsArray[ iBase + iPos ].pDynFunc     = pDynFunc;
        pDynList->pProcsArray[ iBase + iPos ].pDynSym      = pDynSym;
        pDynList->pProcsArray[ iBase + iPos ].pPresetFunc  = pDynSym->pSymbol->value.pFunPtr;
        pDynList->pProcsArray[ iBase + iPos ].cPresetScope = pDynSym->pSymbol->scope.value;

        pDynSym->pSymbol->value.pFunPtr = (PHB_FUNC) bCode;
        pDynSym->pSymbol->scope.value = HB_FS_PUBLIC;
      }

      //hb_retptr( (void *) pDynList );
      hb_retnl( iBase );

      if( hb_param( 3, HB_IT_BYREF ) )
      {
         hb_storptr( pDynList, 3 );
      }

      //TraceLog( NULL, "Base: %i, New: %i\n", iBase, pDynList->iProcs );
   }

   //---------------------------------------------------------------------------//
   HB_FUNC_STATIC( _RELEASEDYNPROCEDURES )
   {
      int i, iProcedures, iBase;
      DYN_PROCS_LIST *pDynList;

      PHB_ITEM pxList = hb_param( 2, HB_IT_POINTER );

      if( pxList )
      {
         pDynList = (DYN_PROCS_LIST *) pxList->item.asPointer.value;
      }
      else
      {
         pDynList = s_pDynList;
      }

      if( pDynList == NULL )
      {
         //TraceLog( NULL, "*** EMPTY List! ***\n" );
         return;
      }

      iBase = hb_parnl( 1 );

      iProcedures = pDynList->iProcs;

      if( iProcedures == iBase )
      {
         //TraceLog( NULL, "*** Nothing to release ***\n" );
         return;
      }

      for( i = iProcedures - 1; i >= iBase; i-- )
      {
         if( pDynList->pProcsArray[i].pDynSym->pSymbol->value.pFunPtr == (PHB_FUNC) pDynList->pProcsArray[i].pDynFunc )
         {
            pDynList->pProcsArray[i].pDynSym->pSymbol->value.pFunPtr = pDynList->pProcsArray[i].pPresetFunc ;
            pDynList->pProcsArray[i].pDynSym->pSymbol->scope.value   = pDynList->pProcsArray[i].cPresetScope ;
         }
         else
         {
            TraceLog( NULL, "*** FUNCTION MISMATCH ***\n" );
         }

         hb_xfree( (void *) ( pDynList->pProcsArray[i].pDynFunc->pCode ) );
         hb_xfree( (void *) ( pDynList->pProcsArray[i].pDynFunc ) );
      }

      if( iBase )
      {
         pDynList = (DYN_PROCS_LIST *) hb_xrealloc( (void *) pDynList, sizeof( int ) + ( sizeof( DYN_PROC ) * iBase ) );
         pDynList->iProcs = iBase;
      }
      else
      {
         hb_xfree( (void *) pDynList );
         pDynList = NULL;
      }

      if( ! pxList )
      {
         s_pDynList = pDynList;
      }
   }
   #endif
   //---------------------------------------------------------------------------//

#pragma ENDDUMP

#endif
