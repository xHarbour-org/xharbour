/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Cofig CLASS for HTML LIB
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "cgidefs.ch"
#include "common.ch"
#include "hbclass.ch"


#define HB_IS_FUNCTION( x ) ( __dynsGetIndex( x ) > 0 )

FUNCTION Configure( cKey, xNewValue )
  STATIC oConf
  LOCAL xValue

  //TraceLog( "oConf == " + cStr( IIF( oConf == NIL, "NIL", "!NIL" ) ) )

  // check if a configuratio already exists
  IF oConf == NIL
     oConf := CGI_Config():New()

     // Check if I have defined a local configuration
     IF HB_IS_FUNCTION( "CGI_LOCALCONFIG"  )

        // and that it is this function that call itself recursively
        IF ProcName( -1 ) <> "CGI_LOCALCONFIG"

           // Run CGI_LocalConfig with HB_ExecFromArray() so I not get error if
           // a local config function it is not linked
           HB_ExecFromArray( "CGI_LocalConfig", { oConf:hConfig } )

        ENDIF

     ENDIF
  ENDIF

  // Read key value
  xValue := IIF( cKey <> NIL, oConf:hConfig[ cKey ], NIL )

  // Set a value if it is passed as parameter
  IF PCount() > 1 .AND. cKey <> NIL
     oConf:hConfig[ cKey ] := xNewValue
  ENDIF

RETURN xValue

FUNCTION GetSQLServer()
RETURN CGI_Config():oDBServer

//----------------------------------------------------------------------------------------------//

CLASS CGI_Config

    CLASSDATA hConfig      INIT Hash()
    CLASSDATA oDBServer

    METHOD New() CONSTRUCTOR
    METHOD Start()
    METHOD Stop()

ENDCLASS

// Setting default values, use Local config function to change base values
METHOD New() CLASS CGI_Config
   LOCAL hIni, cSection, hSection

   WITH OBJECT ::hConfig
     :APPLICATION_NAME            := "APPLICATIONNAME"
     :APPLICATION_VERSION         := "0.00"

     //__OutDebug( "hb_ArgC(0), hb_ArgV(0)", hb_ArgC(0), hb_ArgV(0) )
     //IF OS() = "Windows"
     //   cBaseFS := hb_ArgV(0)
     //   hFile   := FileNameSplit( cBaseFS )
     //   IF hFile <> NIL
     //      :DIR_FS_BASE              := hFile[ "drive" ] + hFile[ "path" ]
     //   ELSE
     //      :DIR_FS_BASE              := "./"
     //   ENDIF
     //ELSE
     :DIR_FS_BASE                 := "."
     :DIR_FS_DATA                 := :DIR_FS_BASE + "/data"
     :DIR_FS_MODULES              := :DIR_FS_BASE + "/modules"
     //ENDIF
     IF OS() = "Windows"
        :INI_NAME                    := Lower( :APPLICATION_NAME ) + "_win.ini"
     ELSE
        :INI_NAME                    := Lower( :APPLICATION_NAME ) + "_linux.ini"
     ENDIF
     :INI_FILE                    := :DIR_FS_DATA + "/" + :INI_NAME
     :LOG_FILE                    := :DIR_FS_DATA + "/access.log"

     :HTTP_SERVER                 := "http://localhost:80"
     :HTTPS_SERVER                := "https://localhost:443"
     :ENABLE_SSL                  := TRUE
     :HTTP_COOKIE_DOMAIN          := "localhost"
     :HTTPS_COOKIE_DOMAIN         := "localhost"
     :HTTP_COOKIE_PATH            := "/"
     :HTTPS_COOKIE_PATH           := "/"
     :DIR_HTTP                    := "/cgi-bin"
     :DIR_HTTPS                   := "/cgi-bin"

     :DIR_DATA                    := "/" + Lower( :APPLICATION_NAME )
     :DIR_IMAGES                  := :DIR_DATA + "/images"
     :SEARCH_ENGINE_FRIENDLY_URLS := FALSE

     :UPLOAD_FILE_EXT             := ".upl"
     :UPLOAD_FILE_MAX_SIZE        := 20000

     :SITE_LOGO_NAME              := "My Web Site"
     :SITE_LOGO_FILE_NORMAL       := :DIR_IMAGES + "/mylogo_normal.jpg"
     :SITE_LOGO_FILE_SMALL        := :DIR_IMAGES + "/mylogo_small.jpg"
     :SITE_LOGO_EXTERNAL_URL      := "http://www.xharbour.org"
     :SITE_LOGO_ALT_NAME          := "My Web Site url"

     IF OS() = "Windows"
        :DB_SERVER                   := "localhost"
        :DB_SERVER_USERNAME          := "root"
        :DB_SERVER_PASSWORD          := "root"
        :DB_DATABASE                 := "dbname"

     ELSE

        :DB_SERVER                   := "localhost"
        :DB_SERVER_USERNAME          := "root"
        :DB_SERVER_PASSWORD          := "root"
        :DB_DATABASE                 := "dbname"

     ENDIF

     :THEMA                       := "2"
     :DIR_THEMA                   := :DIR_DATA   + "/thema/" + :THEMA
     :DIR_HTML_EDITOR             := :DIR_DATA   + "/htmlarea"
     :DIR_PRODUCT_IMAGES          := :DIR_IMAGES + "/products"
     :DIR_PRODUCT_MANUFACTURER    := :DIR_IMAGES + "/manufacturer"
     :DIR_BANNERS                 := :DIR_IMAGES + "/banners"

     // Filesystem paths

     :DIR_FS_UPLOAD               := "/tmp/upload"
     :DIR_FS_TEMP                 := "/tmp"
     :DIR_FS_IMAGES               := :DIR_FS_DATA + "/images"
     :DIR_FS_PRODUCT_IMAGES       := :DIR_FS_IMAGES + "/products"
     :DIR_FS_PRODUCT_MANUFACTURER := :DIR_FS_IMAGES + "/manufacturer"
     :DIR_FS_BANNERS              := :DIR_FS_IMAGES + "/banners"

     :DISPLAY_COLUMN_LEFT         := TRUE
     :DISPLAY_COLUMN_RIGHT        := TRUE
     :APPLICATION_TIMEOUT         := 30 * 1000    // 60 secondi

     // News
     :NEWS_NUM_DISPLAY            := 5
     :NEWS_DAYS_DISPLAY           := 7

     // Visual values
     :BOX_WIDTH                   := 130

     :BOX_PRODUCTS_WIDTH          := "50%"
     :BOX_PRODUCTS_HEIGHT         := 150
     :BOX_PRODUCTS_IMAGE_WIDTH    := 160

     // Sessioni
     :USERS_ON_LINE_FROM_SECONDS  := 2 * 60  // 1 minuto

     // Editor
     //:EDITOR_WIDTH                :=

   END

   // INI file support
   IF File( ::hConfig:INI_FILE )

      hIni := HB_ReadIni( ::hConfig:INI_FILE )
      FOR EACH cSection IN hIni:Keys
          hSection := hIni[ cSection ]
          cSection := Upper( cSection )

          // TODO: lock important keys. Otherwise anyone can change anything !!!!
          DO CASE
             CASE cSection == "MAIN"
             CASE cSection == "DATABASE"
             CASE cSection == "PATH"
          ENDCASE

          hEval( hSection, {|cKey, xVal| ::hConfig[ cKey ] := xVal } )

      NEXT
   ENDIF

   IF HGetValue( ::hConfig, "DEBUG_ON" ) <> NIL .AND. ;
      ValType( ::hConfig:DEBUG_ON ) == "C" .AND. ;
      Lower( ::hConfig:DEBUG_ON ) == "yes"
      ::hConfig:DEBUG := TRUE
   ELSE
      ::hConfig:DEBUG := FALSE
   ENDIF

RETURN Self

METHOD Start() CLASS CGI_Config

   IF ::hConfig == NIL
      ::New()
   ENDIF
   // Open Database
   ::oDBServer := INFO_DBConnect()

RETURN NIL

METHOD Stop() CLASS CGI_Config

   // Close Database
   INFO_DBClose( ::oDBServer )

RETURN NIL

