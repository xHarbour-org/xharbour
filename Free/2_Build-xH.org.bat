@ECHO OFF
SET HB_DIR_ADS=C:\Program Files\Advantage 9.10\acesdk

SET HB_DIR_OPENSSL=C:\OPENSSL
SET TIPSSL=1

C:
CD \xHarbour

IF "%2" == "all"  call make_%1%.bat clean build
IF "%2" == "ALL"  call make_%1%.bat clean build
IF "%2" == "All"  call make_%1%.bat clean build
IF "%2" == "-all" call make_%1%.bat clean build
IF "%2" == "-ALL" call make_%1%.bat clean build
IF "%2" == "-All" call make_%1%.bat clean build

IF "%2" == "" call make_%1%.bat all -DTIPSSL=1

