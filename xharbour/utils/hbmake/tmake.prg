
#include "hbclass.ch"
#include "common.ch"

#define EOL hb_osnewline()
#define CRLF hb_osnewline()
Static  lEof          :=  .F.
Class ThbMake
export:
Data  lPrint         Init  .F.
Data  nHandle  
Data  aDefines       Init  {}
Data  aBuildOrder    Init  {}
Data  aCommands      Init  {}
Data  aMacros        Init  {}
Data  aPrgs          Init  {}
Data  aCs            Init  {}
Data  aObjs          Init  {}
Data  aObjsc         Init  {}

Data  aRes           Init  {}
Data  nLinkHandle 
Data  cLinker        Init  "makefile.tmp"
Data  cLinkcomm      Init  ''
DATA lCompress       init .F.
Data  lBcc           Init  .T.
Data  lGcc           Init  .F.
Data  lVcc           Init  .F.
Data  lForce         Init  .F.
Data  lLinux         Init  .F.
Data  szProject      Init  ""
Data  lLibrary       Init  .F.
Data  lIgnoreErrors  Init  .F.
Data  lExtended      Init  .T.
Data  lOs2           Init  .F.
Data  lRecurse       Init  .F.
Data  aDir
Data  lEditMode      Init  .F.
Data  aDir
Data  aLangMessages  init  {}
Data  cDefLang
data  lFwh           init .F.
data  lCw            init .F.
Data  lmini          init .F.
Data  lHwgui         init .F.
data  lRddAds        init .F.
DAta  lMediator      init .F.
Data  cMakefile      init ""
Data  lExternalLib   init .F.
Data  cObj           init ""
Data  cUserdef       init ""
Data  cUserInclude   init ""
Data  lGenppo        init .f.
Data  lCompMod       init .f.
Data  lAutomemvar    init .f.
Data  lvarismemvar   init .f.
Data  ldebug         init .f.
Data  lSupressline   init .f.
Data  StartPath      init ""
Data  cFMC           init ""
DAta  cMedpath       init ""
Data  cAppLibName    init ""
Data  cOs            init ""
Data  cTopfile       init ""
Data  aOut           init {}
Method New()
method Reset()
Method WriteMakeFileHeader()
Method SetMakeCommands()
Method WriteMakeFile()
Method WriteLibMakeFile()
Method ReadMakefile()
Method Replacemacros( cMacros )
METHOD Findmacro( cMacro, cRead ) 
endclass

Method New() Class tHbmake

   ::cObj           := "obj" + Space( 40 )
   ::cUserdef       := Space( 200 )
   ::cUserInclude   := Space( 200 )
   ::cFMC           := Space( 200 )
   ::cAppLibName    := Space( 20 )
return self

method Reset() class THbmake

   ::aDefines       :=  {}
   ::aBuildOrder    :=  {}
   ::aCommands      :=  {}
   ::aMacros        :=  {}
   ::aPrgs          :=  {}
   ::aCs            :=  {}
   ::aObjs          :=  {}
   ::aObjsc         :=  {}

return self

Method WriteMakeFileHeader() Class THbmake

    Fwrite( ::nLinkHandle, "#BCC" + CRLF )
    Fwrite( ::nLinkHandle, "VERSION=BCB.01" + CRLF )
    Fwrite( ::nLinkHandle, "!ifndef BCB" + CRLF )
    Fwrite( ::nLinkHandle, "BCB = $(MAKEDIR)" + CRLF )
    Fwrite( ::nLinkHandle, "!endif" + CRLF )
    Fwrite( ::nLinkHandle, CRLF )
    Fwrite( ::nLinkHandle, "!ifndef BHC" + CRLF )
    Fwrite( ::nLinkHandle, "BHC = $(HMAKEDIR)" + CRLF )
    Fwrite( ::nLinkHandle, "!endif" + CRLF )
    Fwrite( ::nLinkHandle, " " + CRLF )
    Fwrite( ::nLinkHandle, "RECURSE ="+if(::lRecurse," YES "," NO ") + CRLF )
    Fwrite( ::nLinkHandle, "LIBRARY ="+if(::lLibrary," YES "," NO ") + CRLF )    
    Fwrite( ::nLinkHandle, " " + CRLF )
    IF ::lFwh

        Fwrite( ::nLinkHandle, "FWH = " + ::cFMC + CRLF )

    ELSEIF ::lCw

        Fwrite( ::nLinkHandle, "C4W =" + ::cFMC + CRLF )

    ELSEIF ::lMini

        Fwrite( ::nLinkHandle, "MINIGUI =" + ::cFMC + CRLF )

    ELSEIF ::lHwgui
        Fwrite( ::nLinkHandle, "HWGUI =" + ::cFMC + CRLF )
    ENDIF


RETURN Self

METHOD SetMakeCommands() CLASS tHbmake

    IF ::lBcc

        Aadd( ::aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )
        Aadd( ::aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

        Aadd( ::aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS)" + if(::lFwh," -I$(FWH)\include" ,if(::lMini," -I$(MINIGUI)\include" , "" )) +" -o$* $**" } )

        Aadd( ::aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

    ELSEIF ::lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. ::cOs == "Linux"

            Aadd( ::aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( ::aCommands, { ".c.o:", "gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )

            Aadd( ::aCommands, { ".prg.o:", "harbour -n  -go -I$(HB_INC_INSTALL) -I.  -o$* $**" } )

        ELSE

            Aadd( ::aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( ::aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

            Aadd( ::aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )

        ENDIF

    ELSEIF ::lVcc

        Aadd( ::aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )
        Aadd( ::aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

        Aadd( ::aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -go  -I$(C4W)\include -o$* $**" } )

        Aadd( ::aCommands, { ".rc.res:", "$(BCB)\rc $(RFLAGS) $<" } )

    ENDIF

return Self

Method WriteMakeFile() CLASS THBMAKE

   Local x
    LOCAL citem          := ""
    LOCAL cExt           := ""
    LOCAL cDrive         := ""
    LOCAL cPath          := ""
    LOCAL cTest          := ""
    Local cObjDir        := alltrim(::cObj)
    LOCAL nWriteFiles  := 0
    Local cResname       := ""
    LOCAL cDefBccLibs    := "lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib gtwin.lib"
    LOCAL cDefGccLibs    := "-lvm -lrtl -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
    LOCAL cgcclibsos2    := "-lvm -lrtl -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
    LOCAL cDeflibGccLibs := "-lvm -lrtl -lgtsln -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon -lslang -lm"
    LOCAL cDefHarOpts  := ""
    Local nPos

    IF ::lAutomemvar

        cDefHarOpts += " -a "

    ENDIF

    IF ::lvarismemvar

        cDefHarOpts += " -v "

    ENDIF

    IF ::ldebug

        cDefHarOpts    += " -b "
        cDefBccLibs    += " debug.lib "
        cDefGccLibs    += " -ldebug "
        cgcclibsos2    += " -ldebug "
        cDeflibGccLibs += " -ldebug "

    ENDIF

    IF ::lSupressline

        cDefHarOpts += " -l "

    ENDIF

    IF ::lGenppo

        cDefHarOpts += " -p "

    ENDIF

    IF ::lCompmod

        cDefHarOpts += " -m "

    ENDIF


    ::nLinkHandle := Fcreate( ::cMakeFile )
    ::WriteMakeFileHeader()

    FOR x := 1 TO Len( ::aMacros )

        IF !Empty( ::aMacros[ x, 2 ] )

            cItem := ::aMacros[ x, 2 ]
            nPos  := Ascan( ::aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( ::aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aPrgs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + ')\' ), ) } )

                IF !::aMacros[ x, 3 ]

                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    ::aMacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( ::aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                IF !::aMacros[ x, 3 ]

                    Aeval( ::aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aCs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )
                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    ::aMacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( ::aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                IF !Empty( cObjDir )

                    Aeval( ::aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aObjs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )
                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )

                ENDIF

            ENDIF

            IF ::lExtended

                nPos := Ascan( ::aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

                IF nPos > 0

                    IF !Empty( cObjDir )

                        Aeval( ::aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aObjsc[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )

                    ENDIF

                ENDIF

            ENDIF

        ENDIF

    NEXT

    IF ::lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. ::cOs == "Linux"

            hb_FNAMESPLIT( ::cTopFile, @cPath, @cTest, @cExt, @cDrive )
            cExt := Substr( cExt, 2 )
            Fwrite( ::nLinkHandle, "PROJECT = " + If( Isupper( cExt ), Strtran( ::cTopFile, ".PRG", "" ), Strtran( ::cTopFile, ".prg", "" ) ) + " $(PR) " + CRLF )
            cExt := lower(cExt)
        ELSE

            hb_FNAMESPLIT( ::cTopFile, @cPath, @cTest, @cExt, @cDrive )
            cExt := Substr( cExt, 2 )
            cExt := lower(cExt)
            Fwrite( ::nLinkHandle, "PROJECT = " + If( Isupper( cExt ), cTest + "." + Strtran( cExt, "PRG", "EXE" ), cTest + "." + Strtran( cExt, "prg", "exe" ) ) + " $(PR) " + CRLF )

        ENDIF

    ELSE

        hb_FNAMESPLIT( ::cTopFile, @cPath, @cTest, @cExt, @cDrive )
        cExt := Substr( cExt, 2 )
        Fwrite( ::nLinkHandle, "PROJECT = " + alltrim(::cAppLibName)+".exe $(PR) " + CRLF )

    ENDIF

    hb_FNAMESPLIT( ::cTopFile, @cPath, @cTest, @cExt, @cDrive )
    cExt := Substr( cExt, 2 )


        nWriteFiles := 0
        Fwrite( ::nLinkHandle, "OBJFILES =" )

        IF Len( ::aObjs ) < 1

            Fwrite( ::nLinkHandle, + " $(OB) " + CRLF )

        ELSE
                                                                                                                                                                
            Aeval( ::aObjs, {   | x, i | nWriteFiles ++, If( ( i <> Len( ::aObjs ) /*.and. x <> ::cTopFile */), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( ::nLinkHandle, "PRGFILES =" )

        IF Len( ::aPrgs ) < 1

            Fwrite( ::nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( ::aPrgs, { | x, i | nWriteFiles ++, If( i <> Len( ::aPrgs ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( ::nLinkHandle, "OBJCFILES =" )

        IF Len( ::aObjsc ) < 1

            Fwrite( ::nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( ::aObjsc, { | x, i | nWriteFiles ++, If( i <> Len( ::aObjsc ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( ::nLinkHandle, "CFILES =" )

        IF Len( ::aCs ) < 1

            Fwrite( ::nLinkHandle, + " $(CF)" + CRLF )

        ELSE

            Aeval( ::aCs, { | x, i | nWriteFiles ++, If( i <> Len( ::aCs ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

    
    
    aeval(::aRes,{|cItem| cResName += cItem+" "})
    CResName :=lower(CResName ) 
    Fwrite( ::nLinkHandle, "RESFILES = "+ CResName  + CRLF )
    Fwrite( ::nLinkHandle, "RESDEPEN = "+ strtran(CResName,".rc",".res")  + CRLF )

    IF ::lRddads

        cDefBccLibs += " rddads.lib ace32.lib"

    ENDIF

/*    IF Len( alibsout ) > 0 .and. lExternalLib

        IF ::lVcc .or. ::lBcc

            cOldLib := cDefBccLibs
            nPos    := Ascan( aLibsout, { | z | At( "html", Lower( z ) ) > 0 } )

            IF npos > 0

                cHtmlLib += aLibsout[ npos ]
                Adel( alibsout, nPos )
                Asize( alibsout, Len( alibsout ) - 1 )

            ENDIF

            Aeval( alibsout, { | cLib | cLibs += " " + cLib } )

            cDefBccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

        ENDIF

        IF ::lGcc

            nPos := Ascan( aLibsout, { | z | At( "html", Lower( z ) ) > 0 } )

            IF npos > 0

                cHtmlLib += "-l" + Strtran( aLibsout[ npos ], '.a', "" )
                Adel( alibsout, nPos )
                Asize( alibsout, Len( alibsout ) - 1 )

            ENDIF

            Aeval( alibsout, { | cLib | cLibs += " -l" + Strtran( cLib, '.a', "" ) } )

            IF ::cOs == "Linux"

                cOldLib        := " " + cDeflibGccLibs
                cDeflibGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

            ELSEIF ::cOs == "OS/2"

                cOldLib     := " " + cgcclibsos2
                cgcclibsos2 := cHtmlLib + " " + cOldLib + " " + cLibs

            ELSE

                cOldLib     := " " + cDefGccLibs
                cDefGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

            ENDIF

        ENDIF

    ENDIF
*/
    IF ::lBcc .or. ::lVcc

        IF ::lFwh

            Fwrite( ::nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib " + cDefBccLibs + CRLF )

        ELSEIF ::lMini

            Fwrite( ::nLinkHandle, "LIBFILES = Minigui.lib " + cDefBccLibs + CRLF )

        ELSEIF ::lHwgui

            Fwrite( ::nLinkHandle, "LIBFILES = hwgui.lib promisc.lib hwg_ghtm.lib " + cDefBccLibs + CRLF )


        ELSEIF ::lCw

            Fwrite( ::nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib" + cDefBccLibs + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "LIBFILES = " + cDefBccLibs + CRLF )

        ENDIF

    ELSEIF ::lGcc

        IF ::cOs == "Linux"

            Fwrite( ::nLinkHandle, "LIBFILES = -Wl,--start-group " + cDeflibGccLibs + " -Wl,--end-group " + CRLF )

        ELSEIF ::cOs == "OS/2"

            Fwrite( ::nLinkHandle, "LIBFILES = " + cgcclibsos2 + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "LIBFILES = " + cDefgccLibs + CRLF )

        ENDIF

    ENDIF

    Fwrite( ::nLinkHandle, "DEFFILE = " + CRLF )
    Fwrite( ::nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

    IF ::lBcc

        Fwrite( ::nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + CRLF )

        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = -L$(BCB)\lib\obj;$(BCB)\lib;$(BHC)\lib;$(FWH)\lib -Gn -M -m -s -Tpe" + If( ::lFwh, " -aa", IF( ::lMini , " -aa" , IF( ::lHwgui , " -aa", " -ap" ))) + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LINKER = ilink32" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ = " + If( ::lFwh, "c0w32.obj", "c0x32.obj" ) + " $(OBJFILES)" + If( ::lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = $(LIBFILES) import32.lib cw32.lib" + CRLF )
        Fwrite( ::nLinkHandle, ".autodepend" + CRLF )

    ELSEIF ::lVcc

        Fwrite( ::nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 =  -c" + CRLF )
        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = /LIBPATH:$(BCB)\lib;$(BHC)\lib;$(C4W)\lib /SUBSYSTEM:CONSOLE" + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LINKER = link" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ = " + If( ::lCw, "$(C4W)\initc.obj", "" ) + "$(OBJFILES)" + If( ::lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = $(LIBFILES) comdlg32.lib shell32.lib user32.lib gdi32.lib" + CRLF )

    ELSEIF ::lGcc

        Fwrite( ::nLinkHandle, "CFLAG1 = " + If( At( "Linux", ::cOs ) > 0, "-I$(HB_INC_INSTALL)", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 = " + If( At( "Linux", ::cOs ) > 0, "-L $(HB_LIB_INSTALL)", " -L $(BHC)/lib" ) + CRLF )
        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = $(CFLAG2)" + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LINKER = gcc" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ = $(OBJFILES) " + If( ::lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESFILES) " + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
        Fwrite( ::nLinkHandle, ".autodepend" + CRLF )

    ENDIF

    Fwrite( ::nLinkHandle, " " + CRLF )
    Fwrite( ::nLinkHandle, "#COMMANDS" + CRLF )

    Aeval( ::aCommands, { | xItem | Fwrite( ::nLinkHandle, xitem[ 1 ] + CRLF ), Fwrite( ::nLinkHandle, xitem[ 2 ] + CRLF ), Fwrite( ::nLinkHandle, " " + CRLF ) } )

    IF ::lBcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( ::nLinkHandle, "    $(LFLAGS) +" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ), +" + CRLF )
        Fwrite( ::nLinkHandle, "    $(PROJECT),, +" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLLIB), +" + CRLF )
        Fwrite( ::nLinkHandle, "    $(DEFFILE), +" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLRES) " + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ELSEIF ::lVcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, "" + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( ::nLinkHandle, "    $(LFLAGS)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ) " + CRLF )
        Fwrite( ::nLinkHandle, "    $(PROJECT)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(PROJECTMAP)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLLIB) " + CRLF )
        Fwrite( ::nLinkHandle, "    $(DEFFILE) " + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLRES) " + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ELSEIF ::lGcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )

        IF At( 'Linux', ::cOs ) > 0

            Fwrite( ::nLinkHandle, "    $(LINKER) @&&!" + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )

        ENDIF

        Fwrite( ::nLinkHandle, "    $(PROJECT) " + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ)  " + CRLF )
        Fwrite( ::nLinkHandle, "    $(LFLAGS)  " + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLLIB)  " + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ENDIF
      fClose(::nLinkHandle)
RETURN nil

Method WriteLIBMakeFile() CLASS THBMAKE
Local x
    LOCAL citem          := ""
    LOCAL cExt           := ""
    LOCAL cDrive         := ""
    LOCAL cPath          := ""
    LOCAL cTest          := ""
    LOCAL nPos
    LOCAL nWriteFiles  := 0
    LOCAL cDefHarOpts := ""
    Local cObjDir        := alltrim(::cObj)

    IF !Empty( ::cUserDef )

        cDefHarOpts += " -D" + Alltrim( ::cUserDef ) + " "

    ENDIF

    IF !Empty( ::cUserInclude )

        cDefHarOpts += " -I" + Alltrim( ::cUserInclude ) + " "

    ENDIF

    IF ::lAutomemvar
     
        cDefHarOpts += " -a "

    ENDIF

    IF ::lvarismemvar

        cDefHarOpts += " -v "

    ENDIF

    IF ::ldebug

        cDefHarOpts += " -b "

    ENDIF

    IF ::lSupressline

        cDefHarOpts += " -l "

    ENDIF

    IF ::lGenppo

        cDefHarOpts += " -p "

    ENDIF

    IF ::lCompmod

        cDefHarOpts += " -m "

    ENDIF


    ::nLinkHandle := Fcreate( ::cMakeFile )
    ::WriteMakeFileHeader()

    FOR x := 1 TO Len( ::aMacros )

        IF !Empty( ::aMacros[ x, 2 ] )

            cItem := ::aMacros[ x, 2 ]
            
            nPos := Ascan( ::aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( ::aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aPrgs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + ')\' ), ) } )

                IF !::aMacros[ x, 3 ]

                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    ::aMacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( ::aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( ::aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aCs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )

                IF !::aMacros[ x, 3 ]

                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    ::aMacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( ::aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0
                
                IF !Empty( cObjDir )

                    Aeval( ::aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aObjs[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )
                    Fwrite( ::nLinkHandle, ::aMacros[ x, 1 ] + ' = ' + Left( ::aMacros[ x, 2 ], Len( ::aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )

                ENDIF

            ENDIF

            IF ::lExtended

                nPos := Ascan( ::aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

                IF nPos > 0

                    IF !Empty( cObjDir )
                                                                    
                        Aeval( ::aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, ::aObjsc[ b ] := Strtran( a, cpath, "$(" + ::aMacros[ x, 1 ] + If( ::lGcc, ")/", ')\' ) ), ) } )

                    ENDIF

                ENDIF

            ENDIF

        ENDIF

    NEXT

    IF ::lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. ::cOs == "Linux"

            Fwrite( ::nLinkHandle, "PROJECT = " + Alltrim( ::cAppLibName ) + ".a " + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "PROJECT = " + Alltrim( Lower( ::cAppLibName ) ) + ".a " + CRLF )

        ENDIF
    ELSE

        Fwrite( ::nLinkHandle, "PROJECT = " + Alltrim( Lower( ::cAppLibName ) ) + ".lib " + CRLF )

    ENDIF


        Fwrite( ::nLinkHandle, "OBJFILES =" )
        nWriteFiles := 0

        IF Len( ::aObjs ) < 1

            Fwrite( ::nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( ::aObjs, { | x, i | nWriteFiles ++, If( i <> Len( ::aObjs ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        Fwrite( ::nLinkHandle, "PRGFILES =" )
        nWriteFiles := 0

        IF Len( ::aPrgs ) < 1

            Fwrite( ::nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( ::aPrgs, { | x, i | nWriteFiles ++, If( i <> Len( ::aPrgs ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0

        IF Len( ::aObjsc ) > 0

            Fwrite( ::nLinkHandle, "OBJCFILES =" )

            IF Len( ::aObjsc ) < 1

                Fwrite( ::nLinkHandle, + " $(OB) " + CRLF )

            ELSE

                Aeval( ::aObjsc, { | x, i | nWriteFiles ++, If( i <> Len( ::aObjsc ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

            ENDIF

        ENDIF

        nWriteFiles := 0

        IF Len( ::aCs ) > 0

            Fwrite( ::nLinkHandle, "CFILES =" )

            IF Len( ::aCs ) < 1

                Fwrite( ::nLinkHandle, + " $(CF)" + CRLF )

            ELSE

                Aeval( ::aCs, { | x, i | nWriteFiles ++, If( i <> Len( ::aCs ), Fwrite( ::nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( ::nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

            ENDIF

        ENDIF



    Fwrite( ::nLinkHandle, "RESFILES =" + CRLF )
    Fwrite( ::nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
    Fwrite( ::nLinkHandle, "DEFFILE = " + CRLF )
    Fwrite( ::nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

    IF ::lBcc

        Fwrite( ::nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + CRLF )
        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = /P32 /0" + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LINKER = tlib $(LFLAGS) $(PROJECT)" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = " + CRLF )
        Fwrite( ::nLinkHandle, ".autodepend" + CRLF )

    ELSEIF ::lVcc

        Fwrite( ::nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 =  -c" + CRLF )
        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LINKER = lib $(PROJECT)" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = " + CRLF )

    ELSEIF ::lGcc

        Fwrite( ::nLinkHandle, "CFLAG1 = " + If( At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0, "-I$(HB_INC_INSTALL)", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
        Fwrite( ::nLinkHandle, "CFLAG2 = " + If( At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0, "-L $(HB_LIB_INSTALL)", " -L $(BHC)/lib" ) + CRLF )
        Fwrite( ::nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "LFLAGS = " + CRLF )
        Fwrite( ::nLinkHandle, "IFLAGS = " + CRLF )

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. ::cOs == "Linux" .or. At( "linux", Lower( Os() ) ) > 0

            Fwrite( ::nLinkHandle, "LINKER = ar -M " + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "LINKER = $(BCB)\ar -M " + CRLF )

        ENDIF

        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
        Fwrite( ::nLinkHandle, "ALLRES = $(RESFILES) " + CRLF )
        Fwrite( ::nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
        Fwrite( ::nLinkHandle, ".autodepend" + CRLF )

    ENDIF

    Fwrite( ::nLinkHandle, " " + CRLF )
    Fwrite( ::nLinkHandle, "#COMMANDS" + CRLF )
    Aeval( ::aCommands, { | xItem | Fwrite( ::nLinkHandle, xitem[ 1 ] + CRLF ), Fwrite( ::nLinkHandle, xitem[ 2 ] + CRLF ), Fwrite( ::nLinkHandle, " " + CRLF ) } )

    IF ::lBcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ)" + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ELSEIF ::lVcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, "" + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
        Fwrite( ::nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ) " + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ELSEIF ::lGcc

        Fwrite( ::nLinkHandle, "#BUILD" + CRLF )
        Fwrite( ::nLinkHandle, " " + CRLF )
        Fwrite( ::nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) " + CRLF )

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. ::cOs == "Linux"

            Fwrite( ::nLinkHandle, "    $(LINKER) @&&!" + CRLF )

        ELSE

            Fwrite( ::nLinkHandle, "    $(BCB)\$(LINKER) @&&!" + CRLF )

        ENDIF

        Fwrite( ::nLinkHandle, "    $(PROJECT) " + CRLF )
        Fwrite( ::nLinkHandle, "    $(ALLOBJ)  " + CRLF )
        Fwrite( ::nLinkHandle, "!" + CRLF )

    ENDIF
   fClose( ::nLinkHandle)
RETURN nil

Method ReadMakefile(cFile) //class thbmake

    LOCAL nPos
    LOCAL cBuffer     := {}
    LOCAL cMacro      := "#BCC"
    LOCAL cDep        := "#DEPENDS"
    LOCAL cOpt        := "#OPTS"
    LOCAL cCom        := "#COMMANDS"
    LOCAL cBuild      := "#BUILD"
    LOCAL cTemp       := ""
    LOCAL cTemp1      := ''
    LOCAL aTemp       := {}
    LOCAL lMacrosec   := .f.
    LOCAL lBuildSec   := .f.
    LOCAL lComSec     := .f.
    LOCAL aTemp1      := {}
    LOCAL cCfg        := ""
    LOCAL lCfgFound   := .F.
    LOCAL aTempCFiles := {}
    Local nHandle
    Local cObjitem
    LOCAL lLinux      := At( 'linux', Lower( Os() ) ) > 0
    Local lExtended:=.t.,szProject

    nHandle := FT_FUSE( cFile )
    IF nHandle < 0
        RETURN self
    ENDIF
    cBuffer := Trim( Substr( ReadLN( @lEof ), 1 ) )
   ::lLibrary :=.f.

    WHILE !leof

        IF At( cMacro, cBuffer ) > 0

            lMacroSec := .T.
            lBuildSec := .f.
            lComSec   := .f.

        ELSEIF At( cBuild, cBuffer ) > 0

            lMacroSec := .f.
            lBuildSec := .T.
            lComSec   := .f.

        ELSEIF At( cCom, cBuffer ) > 0

            lBuildSec := .f.
            lComSec   := .t.
            lMacroSec := .f.

        ELSE

            ? "Invalid Make File"
            Fclose( nHandle )
            RETURN Nil

        ENDIF

        cTemp := Trim( Substr( ReadLN( @lEof ), 1 ) )

        IF At( "//", ctemp ) > 0

            WHILE At( "//", ctemp ) > 0

                ctemp := Strtran( ctemp, " //", "" )
                cTemp += Trim( Substr( ReadLN( @lEof ), 1 ) )

            ENDDO

            ctemp := Strtran( ctemp, " //", "" )

        ENDIF       
        aTemp := Listasarray2( Alltrim( cTemp ), "=" )

        IF lmacrosec

            IF Alltrim( Left( ctemp, 7 ) ) <> '!ifndef' .and. Alltrim( Left( ctemp, 6 ) ) <> "!endif" .and. Alltrim( Left( ctemp, 7 ) ) <> '!iffile' .and. Alltrim( Left( ctemp, 7 ) ) <> '!stdout' .and. Alltrim( Left( ctemp, 6 ) ) <> '!ifdef'

                IF Len( atemp ) > 1

                    IF At( "$", atemp[ 2 ] ) > 0

/*                        IF lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and. aTemp[ 1 ] = "CFLAG2"

                            Aadd( ::aMacros, { aTemp[ 1 ], Strtran( ::replacemacros( atemp[ 2 ] ), "\", "/" ) } )

                        ELSE*/

                            Aadd( ::aMacros, { aTemp[ 1 ], ::replacemacros( atemp[ 2 ] ) } )

/*                        ENDIF */

                    ELSE

/*                        IF lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and. aTemp[ 1 ] = "CFLAG2"

                            Aadd( ::aMacros, { aTemp[ 1 ], Strtran( atemp[ 2 ], "\", "/" ) } )

                        ELSE*/

                            Aadd( ::aMacros, { aTemp[ 1 ], atemp[ 2 ] } )
tracelog(aTemp[ 1 ], atemp[ 2 ])
/*                        ENDIF*/

                    ENDIF

                ENDIF

                IF aTemp[ 1 ] == "PROJECT"

                    ::cAppLibName :=atemp[ 2 ]
                    ::cAppLibName :=strtran(::cAppLibName ,"$(PR)","")
                    ::cAppLibName :=strtran(::cAppLibName ,".exe","")
                    ::cAppLibName :=strtran(::cAppLibName ,".lib","")
                ENDIF

                IF aTemp[ 1 ] == "C4W"
                ::cFMC:= aTemp[2]
                ::lCw :=.t.
                endif
                IF aTemp[ 1 ] == "FWH"
                ::cFMC:= aTemp[2]
                ::lFwh           :=.t.
                endif
                IF aTemp[ 1 ] == "MINIGUI"
                ::cFMC:= aTemp[2]
                ::lmini :=.t.
                endif
                IF aTemp[ 1 ] == "HWGUI"
                ::cFMC:= aTemp[2]
                ::lHwGui :=.t.
                endif

                IF aTemp[ 1 ] == "MEDIATOR"
                ::cMedpath:= aTemp[2]
                ::lmEDIATOR :=.t.
                endif
                IF aTemp[ 1 ] == "COMPRESS"
                   ::lCompress := "YES" IN aTemp[ 2 ]
                endif


                IF aTemp[ 1 ] == "OBJFILES"
                     cObjitem := substr( atemp[ 2 ],1,at(")",atemp[ 2 ]))
                     tracelog( cObjitem ) 
                    ::cObj := ::replacemacros(cObjItem)
                     tracelog( ::cObj ) 
                    ::aObjs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                ENDIF

                IF aTemp[ 1 ] == "OBJCFILES"

                    aTemp1 := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                    IF Len( atemp1 ) == 1

                        IF !Empty( atemp[ 1 ] )

                            ::aObjsC := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                        ENDIF
                    ELSE
                        ::aObjsC := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                    ENDIF

                ENDIF

                IF aTemp[ 1 ] == "PRGFILES"

                    ::aPrgs     := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
                    lExtended := .T.


                ENDIF

                IF aTemp[ 1 ] == "PRGFILE"

                    ::aPrgs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                ENDIF

                IF atemp[ 1 ] == "CFILES"

                    IF lExtended

                        aTempCFiles := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                        IF ( Len( aTempCFiles ) == 1 )

                            IF !Empty( aTempCFiles[ 1 ] )

                                ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                            ENDIF

                        ELSE

                            ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                        ENDIF

                    ELSE

                        ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                    ENDIF

                ENDIF

                IF atemp[ 1 ] == "RESFILES"

                    ::aRes := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                ENDIF

                IF aTemp[ 1 ] == "RECURSE"

                   ::lRecurse := AT( "YES" , aTemp[ 2 ] ) > 0

                ENDIF

                IF aTemp[ 1 ] == "LIBRARY"

                   ::lLibrary := AT( "YES", aTemp[ 2 ] ) > 0

                ENDIF  

                IF aTemp[ 1 ] ==  "HARBOURFLAGS"

                     ::lGenppo        := AT( "-p" , aTemp[ 2 ] ) > 0
                     ::lCompMod       := AT( "-m" , aTemp[ 2 ] ) > 0
                     ::lAutomemvar    := AT( "-a" , aTemp[ 2 ] ) > 0
                     ::lvarismemvar   := AT( "-v" , aTemp[ 2 ] ) > 0
                     ::ldebug         := AT( "-b" , aTemp[ 2 ] ) > 0
                     ::lSupressline   := AT( "-l" , aTemp[ 2 ] ) > 0
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-p","")
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-m","")
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-a","")
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-v","")
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-b","")
                     aTemp[ 2 ] := strtran(aTemp[ 2 ],"-l","")
                     aTemp[ 2 ] := Alltrim( aTemp[ 2 ] )
                     ::cUserdef:=substr(aTemp[ 2 ],1,at("-I",aTemp[ 2 ])-1)
                     ::cUserdef:=strtran(::cUserdef,"-D","")
                     ::cUserInclude:=substr(aTemp[ 2 ],at("-I",aTemp[ 2 ]))
                     ::cUserInclude:=strtran(::cUserInclude,"-I","")


                endif
                  

            

/*                IF At( '!ifndef', cTemp ) > 0

                    Checkdefine( cTemp )

                ELSEIF At( '!ifdef', ctemp ) > 0

                    CheckifDef( cTemp )

                ELSEIF At( '!iffile', cTemp ) > 0

                    checkiffile( cTemp )

                ELSEIF At( '!stdout', cTemp ) > 0

                    checkstdout( cTemp )

                ENDIF*/

            ENDIF

        ENDIF

        IF lbuildSec

            szProject   := cTemp
            ::aBuildOrder := Listasarray2( ctemp, ":" )


        ENDIF

/*        IF lComSec

            IF !Empty( ctemp )

                Setcommands( cTemp )

            ENDIF

        ENDIF*/
        IF cTemp = "#BUILD"

            cBuffer := cTEmp

        ELSEIF cTemp == "#COMMANDS"

            cbuffer := ctemp

        ENDIF

    ENDDO

/*    IF lExtended .and. !lCfgFound

        IF lBcc

            BuildBorCfgFile()

        ELSEIF lVcc

            BuildMSCCfgFile()

        ELSEIF lGcc .and. !lLinux

            BuildGccCfgFile()

        ELSEIF lGcc .and. lLinux

            BuildGccCfgFilel()

        ENDIF

    ENDIF*/
qout( nhandle)
            Fclose( nHandle )
RETURN self

Method Replacemacros( cMacros ) CLass THBMAKE

    LOCAL nPos
    LOCAL nCount       := 0
    LOCAL aTempMacros  := {}
    LOCAL aLocalMacros := {}

    aTempMacros := Listasarray2( cMacros, " " )
    Aeval( aTempMacros, { | xMacro | If( At( "$", xMacro ) > 0, ;
                          IF( At( ";", xMacro ) > 0, ( aLocalMacros := Listasarray2( xMacro, ";" ), ;
                          Aeval( aLocalMacros, { | x | ::Findmacro( x, @cMacros ) } ) ), ;
                          ::Findmacro( xMacro, @cMacros ) ), ) } )

RETURN cmacros

METHOD Findmacro( cMacro, cRead ) CLASS THBMAKE

    LOCAL nPos
    LOCAL cTemp
    LOCAL aLocalMacros := {}

    cMacro := Substr( cMacro, 1, At( ")", cMacro ) )

    IF At( "-", cMacro ) > 0

        cMacro := Substr( cMacro, 3 )

    ENDIF

    IF At( ";", cMacro ) > 0

        cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )

    ENDIF

    nPos := Ascan( ::aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

    IF nPos = 0

        cTemp := Strtran( cmacro, "$(", "" )
        cTemp := Strtran( ctemp, ")", "" )

        IF !Empty( cTemp )

            cRead := Alltrim( Strtran( cRead, cmacro, Gete( cTemp ) ) )

        ENDIF

    ELSE

        cRead := Alltrim( Strtran( cRead, cmacro, ::amacros[ npos, 2 ] ) )

    ENDIF

RETURN cRead
/*Function ReadLN( leof )

     Local cBuffer := ""
     cBuffer := FT_FREADLN()
     cBuffer := Strtran( cBuffer, Chr( 13 ), '' )
     cBuffer := Strtran( cBuffer, Chr( 10 ), '' )
     FT_FSKIP( 1 )
     leof := ft_FEOF()
Return cBuffer
*/
