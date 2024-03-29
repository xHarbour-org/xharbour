ECHO -----------
ECHO BC5 VERSION
ECHO -----------

SET _XB_Compiler=bc5
SET _XB_NonDebug=-NoXbp
SET _XB_Debug=-NoXbp -Debug
SET _XB_Exe=%_XB_NonDebug%

SET _BUILD_MT=YES

SET _BUILD_DEMO=YES
SET _BUILD_PERSONAL=NO
SET _BUILD_XHB.EXE=NO
SET _BUILD_XHB.LIB=NO
SET _BUILD_XHB.DLL=NO
SET _BUILD_CORE=NO
SET _BUILD_XBUILD=NO
SET _BUILD_CONTRIB=NO
SET _BUILD_DMAIN.LIB=NO
SET _BUILD_VXHDLL=NO
SET _BUILD_HBZLIB=NO
SET _BUILD_XHBCOMM=NO
SET _BUILD_ADS=NO
SET _BUILD_XDO.DLL=NO
SET _BUILD_IEGUI.LIB=NO
SET _BUILD_DBG_CLIENT=NO

SET _BUILD_TPROJECT.LIB=NO
SET _BUILD_XEDIT.LIB=NO

SET _BUILD_APOLLORDD=YES
SET _BUILD_WINAPI.LIB=YES
SET _BUILD_XBSCRIPT.LIB=YES
SET _BUILD_CT3COMM=YES
SET _BUILD_ACTIVEX=YES
SET _BUILD_RMDBFCDX=YES
SET _BUILD_WINCORE=YES
SET _BUILD_OLE=YES
SET _BUILD_SQLRDD=YES

SET _BUILD_XBUILDW_AS=NONE
SET _BUILD_VXH_AS=NONE
SET _BUILD_XEDITW_AS=NONE
SET _BUILD_XPROMPT_AS=NONE
SET _BUILD_XDEBUGW_AS=NONE


IF "%BC5_MT%"=="YES"           SET _BUILD_MT=YES
IF "%BC5_MT%"=="NO"            SET _BUILD_MT=NO

IF "%BC5_DEMO%"=="YES"         SET _BUILD_DEMO=YES
IF "%BC5_DEMO%"=="NO"          SET _BUILD_DEMO=NO

IF "%BC5_PERSONAL%"=="YES"     SET _BUILD_PERSONAL=YES
IF "%BC5_PERSONAL%"=="NO"      SET _BUILD_PERSONAL=NO

IF "%BC5_XHB.EXE%"=="YES"      SET _BUILD_XHB.EXE=YES
IF "%BC5_XHB.EXE%"=="NO"       SET _BUILD_XHB.EXE=NO

IF "%BC5_XHB.LIB%"=="YES"      SET _BUILD_XHB.LIB=YES
IF "%BC5_XHB.LIB%"=="NO"       SET _BUILD_XHB.LIB=NO

IF "%BC5_XHB.DLL%"=="YES"      SET _BUILD_XHB.DLL=YES
IF "%BC5_XHB.DLL%"=="NO"       SET _BUILD_XHB.DLL=NO

IF "%BC5_CORELIBS%"=="YES"     SET _BUILD_CORE=YES
IF "%BC5_CORELIBS%"=="NO"      SET _BUILD_CORE=NO

IF "%BC5_XBUILD%"=="YES"       SET _BUILD_XBUILD=YES
IF "%BC5_XBUILD%"=="NO"        SET _BUILD_XBUILD=NO

IF "%BC5_CONTRIB%"=="YES"      SET _BUILD_CONTRIB=YES
IF "%BC5_CONTRIB%"=="NO"       SET _BUILD_CONTRIB=NO

IF "%BC5_DMAIN.LIB%"=="YES"    SET _BUILD_DMAIN.LIB=YES
IF "%BC5_DMAIN.LIB%"=="NO"     SET _BUILD_DMAIN.LIB=NO

IF "%BC5_VXHDLL%"=="YES"       SET _BUILD_VXHDLL=YES
IF "%BC5_VXHDLL%"=="NO"        SET _BUILD_VXHDLL=NO

IF "%BC5_SQLRDD%"=="YES"       SET _BUILD_SQLRDD=YES
IF "%BC5_SQLRDD%"=="NO"        SET _BUILD_SQLRDD=NO

IF "%BC5_HBZLIB%"=="YES"       SET _BUILD_HBZLIB=YES
IF "%BC5_HBZLIB%"=="NO"        SET _BUILD_HBZLIB=NO

IF "%BC5_CT3COMM%"=="YES"      SET _BUILD_CT3COMM=YES
IF "%BC5_CT3COMM%"=="NO"       SET _BUILD_CT3COMM=NO

IF "%BC5_XHBCOMM%"=="YES"      SET _BUILD_XHBCOMM=YES
IF "%BC5_XHBCOMM%"=="NO"       SET _BUILD_XHBCOMM=NO

IF "%BC5_APOLLORDD%"=="YES"    SET _BUILD_APOLLORDD=YES
IF "%BC5_APOLLORDD%"=="NO"     SET _BUILD_APOLLORDD=NO

IF "%BC5_WINAPI.LIB%"=="YES"   SET _BUILD_WINAPI.LIB=YES
IF "%BC5_WINAPI.LIB%"=="NO"    SET _BUILD_WINAPI.LIB=NO

IF "%BC5_TPROJECT.LIB%"=="YES" SET _BUILD_TPROJECT.LIB=YES
IF "%BC5_TPROJECT.LIB%"=="NO"  SET _BUILD_TPROJECT.LIB=NO

IF "%BC5_XEDIT.LIB%"=="YES"    SET _BUILD_XEDIT.LIB=YES
IF "%BC5_XEDIT.LIB%"=="NO"     SET _BUILD_XEDIT.LIB=NO

IF "%BC5_XDO.DLL%"=="YES"      SET _BUILD_XDO.DLL=YES
IF "%BC5_XDO.DLL%"=="NO"       SET _BUILD_XDO.DLL=NO

IF "%BC5_IEGUI.LIB%"=="YES"    SET _BUILD_IEGUI.LIB=YES
IF "%BC5_IEGUI.LIB%"=="NO"     SET _BUILD_IEGUI.LIB=NO

IF "%BC5_DBG_CLIENT%"=="YES"   SET _BUILD_DBG_CLIENT=YES
IF "%BC5_DBG_CLIENT%"=="NO"    SET _BUILD_DBG_CLIENT=NO

IF "%BC5_DEBUG%"=="YES"        SET _XB_Exe=%_XB_Debug%
IF "%BC5_DEBUG%"=="NO"         SET _XB_Exe=%_XB_NonDebug%


IF "%BC5_XBUILDW_AS%"=="DLL"   SET _BUILD_XBUILDW_AS=DLL
IF "%BC5_XBUILDW_AS%"=="EXE"   SET _BUILD_XBUILDW_AS=EXE
IF "%BC5_XBUILDW_AS%"=="NONE"  SET _BUILD_XBUILDW_AS=NONE

IF "%BC5_VXH_AS%"=="DLL"       SET _BUILD_VXH_AS=DLL
IF "%BC5_VXH_AS%"=="EXE"       SET _BUILD_VXH_AS=EXE
IF "%BC5_VXH_AS%"=="NONE"      SET _BUILD_VXH_AS=NONE

IF "%BC5_XEDITW_AS%"=="DLL"    SET _BUILD_XEDITW_AS=DLL
IF "%BC5_XEDITW_AS%"=="EXE"    SET _BUILD_XEDITW_AS=EXE
IF "%BC5_XEDITW_AS%"=="NONE"   SET _BUILD_XEDITW_AS=NONE

IF "%BC5_XDEBUGW_AS%"=="DLL"   SET _BUILD_XDEBUGW_AS=DLL
IF "%BC5_XDEBUGW_AS%"=="EXE"   SET _BUILD_XDEBUGW_AS=EXE
IF "%BC5_XDEBUGW_AS%"=="NONE"  SET _BUILD_XDEBUGW_AS=NONE

IF "%BC5_XPROMPT_AS%"=="DLL"   SET _BUILD_XPROMPT_AS=DLL
IF "%BC5_XPROMPT_AS%"=="EXE"   SET _BUILD_XPROMPT_AS=EXE
IF "%BC5_XPROMPT_AS%"=="NONE"  SET _BUILD_XPROMPT_AS=NONE


SET _XHB_LIB=\xhb\lib\%_XB_Compiler%
SET _XHB_BIN=\xhb\bin\%_XB_Compiler%
SET _XHB_DLL=\xhb\dll\%_XB_Compiler%

SET _XHARBOUR_ROOT=\xharbour
SET _XHARBOUR_LIB=\xharbour\lib\b32
SET _XHARBOUR_BIN=\xharbour\bin\b32
SET _XHARBOUR_XBP=\xbp\bc5
SET _XHARBOUR_XBP_DEMO=\xbp\bc5\demo
SET _XHARBOUR_XBP_PERS=\xbp\bc5\personal

ECHO ON
