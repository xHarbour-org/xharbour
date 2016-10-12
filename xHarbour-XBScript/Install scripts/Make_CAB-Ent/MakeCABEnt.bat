DEL XBScriptEnt.cab
NotePad Setup.inf
XCOPY ..\files\XBScriptEnt.dll /d
CALL SIGN "XBScript Enterprise Edition" XBScriptEnt.dll
..\cabsdk\bin\cabarc n XBScriptEnt.cab XBScriptEnt.dll setup.inf
..\cabsdk\bin\cabarc l XBScriptEnt.cab
CALL SIGN "XBScript Enterprise Edition" XBScriptEnt.cab