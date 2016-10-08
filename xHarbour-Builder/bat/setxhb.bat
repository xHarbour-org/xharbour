SET PATH=\xHarbour\bin;%PATH%;

SET CFLAGS=/Od /EHsc /RTC1 /MTd /Gs /GS /Gy /GR /Zi /DHB_FM_STATISTICS /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE /DHB_USE_PROFILER -D"HAVE_STDINT_H=0" -D"HAVE_INTTYPES_H=0"
SET LFLAGS=-DEBUG -DEBUGTYPE:CV
SET __MSC__=14

IF EXIST "\Program Files\Advantage 11.10\acesdk" SET HB_DIR_ADS=\Program Files\Advantage 11.10\acesdk
IF EXIST "\Program Files (x86)\Advantage 11.10\acesdk" SET HB_DIR_ADS=\Program Files (x86)\Advantage 11.10\acesdk

SET HB_DIR_OPENSSL=\OpenSSL-Win32

CD \xharbour
