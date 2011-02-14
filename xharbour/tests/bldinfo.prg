/*
* $Id$
* Test program for HB_BUILDINDO( [ nInfo ] )
* Retrieve information on xHarbour Build Info
* Please refer to include/hbver.ch for more information on nInfo
*/

#include "hbver.ch"
//------------------------------------------------------------------------------
proc main()

   LOCAL aInfo := HB_BUILDINFO()
   LOCAL j := 0, uEl, cMember, i := 0

   OutStd( "HB_BUILDINFO() Without Parameter ..." + hb_osnewline() )
   OutStd( "Returns : " + ltrim(str(len(aInfo))) + "-element single-dimensional-array" + hb_osnewline()        )
   OutStd( 'Element types are varying: "L", "C", "N" or "A"' + hb_osnewline()      )
   OutStd( "Press any key ..." + hb_osnewline()    )
   Inkey(0)

   FOR EACH uEl IN aInfo
      ++ i
      IF ValType( uEl ) == "A"
         FOR EACH cMember IN uEl
            OutStd( "[" + PADL(ltrim(str(i)),2) +"]" )
            OutStd( "[" + PADL(ltrim(str(++j)),2) + "]")
            OutStd( " = " )
            OutStd( cMember )
            OutStd( hb_osnewline() )
         NEXT
      ELSE
         OutStd( "[" )
         OutStd( PADL(ltrim(str(i)),2) )
         OutStd( "] = " )
         OutStd( uEl )
         OutStd( hb_osnewline() )
      ENDIF
   NEXT
   OutStd( hb_osnewline())

   OutStd( "HB_BUILDINFO( nInfo ) -> With Parameter ..." + hb_osnewline()  )
   OutStd( "nInfo = information item number. Please refer to hbver.ch" + hb_osnewline()    )
   OutStd( "Press any key ..." + hb_osnewline() )
   Inkey(0)

   _BldInfo ( "_HB_VER_MAJOR"              , _HB_VER_MAJOR              )
   _BldInfo ( "_HB_VER_MINOR"              , _HB_VER_MINOR              )
   _BldInfo ( "_HB_VER_REVISION"           , _HB_VER_REVISION           )
   _BldInfo ( "_HB_VER_LEX"                , _HB_VER_LEX                )
   _BldInfo ( "_HB_VER_AS_STRING"          , _HB_VER_AS_STRING          )
   _BldInfo ( "_HB_PCODE_VER"              , _HB_PCODE_VER              )
   _BldInfo ( "_HB_VER_COMPILER"           , _HB_VER_COMPILER           )
   _BldInfo ( "_HB_VER_PLATFORM"           , _HB_VER_PLATFORM           )
   _BldInfo ( "_HB_VER_BUILD_DATE"         , _HB_VER_BUILD_DATE         )
   _BldInfo ( "_HB_VER_BUILD_TIME"         , _HB_VER_BUILD_TIME         )
   _BldInfo ( "_HB_VER_LENTRY"             , _HB_VER_LENTRY             )
   _BldInfo ( "_HB_VER_CHLCVS"             , _HB_VER_CHLCVS             )
   _BldInfo ( "_HB_VER_C_USR"              , _HB_VER_C_USR              )
   _BldInfo ( "_HB_VER_L_USR"              , _HB_VER_L_USR              )
   _BldInfo ( "_HB_VER_PRG_USR"            , _HB_VER_PRG_USR            )
   _BldInfo ( "_HB_EXTENSION"              , _HB_EXTENSION              )
   _BldInfo ( "_HB_C52_UNDOC"              , _HB_C52_UNDOC              )
   _BldInfo ( "_HB_C52_STRICT"             , _HB_C52_STRICT             )
   _BldInfo ( "_HB_COMPAT_C53"             , _HB_COMPAT_C53             )
   _BldInfo ( "_HB_COMPAT_XPP"             , _HB_COMPAT_XPP             )
   _BldInfo ( "_HB_COMPAT_VO"              , _HB_COMPAT_VO              )
   _BldInfo ( "_HB_COMPAT_FLAGSHIP"        , _HB_COMPAT_FLAGSHIP        )
   _BldInfo ( "_HB_COMPAT_FOXPRO"          , _HB_COMPAT_FOXPRO          )
   _BldInfo ( "_HB_COMPAT_DBASE"           , _HB_COMPAT_DBASE           )
   _BldInfo ( "_HB_HARBOUR_OBJ_GENERATION" , _HB_HARBOUR_OBJ_GENERATION )
   _BldInfo ( "_HB_HARBOUR_STRICT_ANSI_C"  , _HB_HARBOUR_STRICT_ANSI_C  )
   _BldInfo ( "_HB_CPLUSPLUS"              , _HB_CPLUSPLUS              )
   _BldInfo ( "_HB_HARBOUR_YYDEBUG"        , _HB_HARBOUR_YYDEBUG        )
   _BldInfo ( "_HB_SYMBOL_NAME_LEN"        , _HB_SYMBOL_NAME_LEN        )
   _BldInfo ( "_HB_MULTITHREAD"            , _HB_MULTITHREAD            )
   _BldInfo ( "_HB_VM_OPTIMIZATION"        , _HB_VM_OPTIMIZATION        )
   _BldInfo ( "_HB_LANG_ID"                , _HB_LANG_ID                )
   _BldInfo ( "_HB_ARRAY_MODE"             , _HB_ARRAY_MODE             )
   _BldInfo ( "_HB_CREDITS"                , _HB_CREDITS                )

   RETURN

//------------------------------------------------------------------------------
STATIC FUNCTION _BldInfo( cInfo, nInfo )

   LOCAL uInfo := HB_BUILDINFO( nInfo )
   LOCAL cValType := ValType( uInfo )

   cInfo := PADR( cInfo, 26 )

   SWITCH ( cValType )
   CASE "L"
      OutStd( cInfo + " : (LOGICAL) " + if( uInfo, ".T.", ".F.") + hb_osnewline() )
      Exit
   CASE "C"
      OutStd( cInfo + " : (STRING)  " + uInfo + hb_osnewline() )
      Exit
   CASE "N"
      OutStd( cInfo + " : (INTEGER) " + LTRIM(STR(uInfo)) + hb_osnewline() )
      Exit
   CASE "A"
      OutStd( cInfo + " : (ARRAY)   " + hb_osnewline() )
      AEval( uInfo, { |e| OutStd( space(3) + e + hb_osnewline() ) })
      Exit
   END

   RETURN NIL
