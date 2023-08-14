CALL Build_1-Free.bat
CALL Build_1-Personal.bat
CALL Build_2-Professional.bat
CALL Build_3-Enterprise.bat

IF "%1" == "" GOTO END

Build_9-Rename.bat %1

:END
