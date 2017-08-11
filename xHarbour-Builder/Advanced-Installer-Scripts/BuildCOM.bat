    SET VXH_Edition=VXH_ENTERPRISE

    SET ROBOCOPY_FROM=C:\xHarbour.com
    SET ROBOCOPY_XF=*.exe *.obj *.bak *.log *.pdb *.idb *.map *.exp *.ilk
    SET ROBOCOPY_XD=GIT CVS CVSROOT Obj Free xHarbour-Documentation xHarbour-VRW xHarbour-XBScript Samples UpdateServer X xHarbour.com*
    SET ROBOCOPY_INCL=*.*
    ROBOCOPY "%ROBOCOPY_FROM%" C:\xHarbour.com *.* /XO /NS /NC /NP /S /E /PURGE /XF %ROBOCOPY_XF% /XD %ROBOCOPY_XD%
  
    SET ROBOCOPY_FROM=C:\xHb
    SET ROBOCOPY_XF=*.obj *.bak *.log *.pdb *.idb *.map *.exp *.ilk *.ppo Unwise.exe vxh-patrick*.*
    SET ROBOCOPY_XD=CVS CVSROOT Doc Samples BC5 VC8 Personal Professional Demo DLL
    SET ROBOCOPY_INCL=*.*
    ROBOCOPY "%ROBOCOPY_FROM%" C:\xHb %ROBOCOPY_INCL% /XO /NS /NC /NP /S /E /XF %ROBOCOPY_XF% /XD %ROBOCOPY_XD%

    ROBOCOPY C:\OpenSSL C:\OpenSSL %ROBOCOPY_INCL% /XO /NS /NC /NP /S /E /PURGE /XF %ROBOCOPY_XF%

    SET ROBOCOPY_XD=Help
    SET ROBOCOPY_XF=uninst.exe
    ROBOCOPY "%ProgramFiles%\PellesC" "%ProgramFiles%\PellesC" *.* /XO /NS /NC /NP /S /E /PURGE /XF %ROBOCOPY_XF% /XD %ROBOCOPY_XD%

    ROBOCOPY "%ProgramFiles%\Advantage 11.10\acesdk" "%ProgramFiles%\Advantage 11.10\acesdk" *.* /XO /NS /NC /NP /S /E /PURGE /XF

    C:
    
    CALL \xHarbour.com\xHarbour-Builder\Bat\SetXHB.bat

    SET XBUILD_XCC=YES

    SET XCC_DEMO=YES
    SET XCC_PERSONAL=YES

REM SET XCC_VXH_AS=NONE
REM SET XCC_VXHDLL=NO

    GOTO :Standard

REM == XCC =================
    SET XCC_XHB.EXE=NO
    SET XCC_XHB.LIB=NO
    SET XCC_XHB.DLL=NO
    SET XCC_CORELIBS=NO
    SET XCC_XBUILD=NO
    SET XCC_CONTRIB=NO
    SET XCC_DMAIN.LIB=NO
    SET XCC_MT=NO
    SET XCC_DEMO=NO
    SET XCC_PERSONAL=NO
    SET XCC_VXHDLL=NO
    SET XCC_SQLRDD=NO
    SET XCC_HBZLIB=NO
    SET XCC_CT3COMM=NO
    SET XCC_APOLLORDD=NO  

    SET XCC_XBUILDW_AS=NONE
    SET XCC_VXH_AS=NONE
    SET XCC_XPROMPT_AS=NONE
    SET XCC_XEDITW_AS=NONE
    SET XCC_XDEBUGW_AS=NONE
REM ========================

    :Standard

    CALL make_vc.bat ?
    ECHO ON

    ATTRIB +r \xHarbour.com\FreeImage\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\IEGui\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\Visual-xHarbour\IDE\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\Visual-xHarbour\Library\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\Visual-xHarbour\xEdit\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-ApolloRDD\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-ActiveX\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-Builder\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-ct3comm\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-DebugClient\vxhdebug\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-DebugServer\server\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-HBZlib\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-OLEServer\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-Rushmore\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-SQLRDD\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-xBuild\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-XDO\xcc\xbuild.windows.ini
    ATTRIB +r \xHarbour.com\xHarbour-xHBComm\xcc\xbuild.windows.ini

    CD \xHarbour.com\xHarbour-Builder\
    CALL xbldfull.bat %1

    COPY \vxh.ini \xhb\bin /Y

    SET ROBOCOPY_XF=*.obj *.bak *.log *.pdb *.idb *.map *.exp *.ilk          
    SET ROBOCOPY_XD=CVS CVSROOT Doc Samples
    ROBOCOPY C:\xHB C:\xHB *.* /XO /NS /NC /NP /S /E /PURGE /XF %ROBOCOPY_XF% /XD %ROBOCOPY_XD%
