SET XBUILD_XCC=YES
SET XCC_DEMO=NO
SET XCC_PERSONAL=NO

SET XHB_VC8=\xharbour
SET XBUILD_VC8=YES
SET VC8_DEBUG=YES
SET VC8_DEMO=NO
SET VC8_PERSONAL=NO

ATTRIB +R \xHarbour.com\xbuild*.ini /S

CALL \xharbour.com\xharbour-builder\xbldfull.bat %1

CD \xharbour