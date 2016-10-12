DEL XBScriptPro.cab
NotePad Setup.inf
XCOPY ..\files\XBScriptPro.dll /d
CALL SIGN "XBScript Professional Edition" XBScriptPro.dll
..\cabsdk\bin\cabarc n XBScriptPro.cab XBScriptPro.dll setup.inf
..\cabsdk\bin\cabarc l XBScriptPro.cab
CALL SIGN "XBScript Professional Edition" XBScriptPro.cab