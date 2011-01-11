DEL XBScript.cab
NotePad Setup.inf
XCOPY ..\files\XBScript.dll /d
CALL SIGN "XBScript Personal Edition" XBScript.dll
..\cabsdk\bin\cabarc n XBScript.cab XBScript.dll setup.inf
..\cabsdk\bin\cabarc l XBScript.cab
CALL SIGN "XBScript Personal Edition" XBScript.cab