/*
 * $Id$
 * Notes on building xHarbour
*/

The following are some environment set-ups which are required to build
custom xHarbour:

SET HB_NO_BACKGROUND=
When set to 1, xHarbour will not use background task routines in ST mode

SET HB_GUI=
When set to 1, xHarbour will disregard keyboard interrupt wiich is not very
useful in GUI mode

SET HB_NO_VM_ALL=
When set, VM modules will not be amalgamated.

SET HB_NO_FM_DL_ALLOC=
When set, xHarbour will use compiler's native memory manager

SET HB_DIR_POSTGRESQL=<install dir>
Required to build libhbpg.lib and sddpg.lib
Download URL: http://postgresql.org/download/

SET HB_DIR_OCILIB=<install dir>
Required to build sddoci.lib
Download URL: http://orclib.sourceforge.net/

SET HB_DIR_MYSQL=<install dir>
Required to build mysql.lib and sddmy.lib
Download URL: http://mysql.com/

SET HB_DIR_FIREBIRD=<install dir>
Required to build firebird.lib and sddfb.lib
Download URL: http://firebirdsql.org/ 

SET HB_DIR_CAIRO=<install dir>
Required to build hbcairo.lib
Download URL: http://cairographics.org/ 

SET HB_DIR_CURL=<install dir>
Required to build hbcurl.lib
Download URL: http://curl.haxx.se/

SET HB_DIR_OPENSSL=<install dir>
Required to build tipssl.lib
Download URL: http://slproweb.com/products/Win32OpenSSL.html

SET HB_DIR_ALLEGRO=<install dir>
Required to build gtalleg.lib
Download URL: http://alleg.sourceforge.net/

SET HB_DIR_MAGIC=<install dir>
Required to build hbmagic.lib
Download URL: ftp://ftp.astron.com/pub/file/file-5.04.tar.gz

SET HB_DIR_ADS=<install dir>
Required to build rdads.lib
Download URL: http://devzone.advantagedatabase.com/dz/content.aspx?Key=20
