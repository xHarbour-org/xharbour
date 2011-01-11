DEL C:\*.aip /Q

CD C:\
CALL "1.UpdateCVS.bat"

CD C:\
CALL "2.Build-xH.org.bat" %1

CD C:\
CALL "3.CopyFiles.bat"

CD C:\
CALL "4.Build-Setup.bat"

CD C:\
CALL "5.Build-html.bat"

CD C:\

PAUSE