/*
 * $Id: hblog.ch,v 1.1 2003/07/10 10:48:58 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * Header file with for logging system
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://www.harbour-project.org
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
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_LOG_CH
#define HB_LOG_CH

#include "hblogdef.ch"

// a pretty high number  that should log any debug level
#define HB_LOG_ALL         9999

#define HB_LOG_ST_DATE     1
#define HB_LOG_ST_TIME     2
#define HB_LOG_ST_SECS     4
#define HB_LOG_ST_LEVEL    8
#define HB_LOG_ST_ISODATE  16

#xcommand INIT LOG [ON] <data,...> [NAME <cName>] => ;
    #xtranslate CONSOLE => HB_LogConsole():New(, <(cName)> );;
    #xtranslate CONSOLE( <nPrio> )=> HB_LogConsole():New( <nPrio>, <(cName)> );;
    #xtranslate SYSLOG => HB_LogSyslog():New(, <(cName)> );;
    #xtranslate SYSLOG( <nPrio>[, <nId>] )=> HB_LogSyslog():New( <nPrio>, <(cName)>, <nId> );;
    #xtranslate FILE( <cFname> [, <nPrio>[,<nMaxSize>[, <nBackup>]]] )=> ;
         HB_LogFile():New( <nPrio>, <cName>, <cFname>, <nMaxSize>, <nBackup> );;
    #xtranslate EMAIL( <nPrio>, <cHelo>, <cServer>, <cDest> [, <cSubject> [,<cFrom>]] ) =>;
         HB_LogEmail():New( <nPrio>, <cName>, <cServer>, <cDest>,;
                <cFrom>, <cSubject>, <cHelo> ) ;;
    #xtranslate MONITOR => HB_LogInetPort():New(, <(cName)> );;
    #xtranslate MONITOR( <nPrio> [,<nPort>] )=>;
             HB_LogInetPort():New( <nPrio>, <(cName)>, <nPort> );;
    HB_InitStandardLog( <data> );;

#xcommand SET LOG STYLE <nStyle> => HB_SetStandardLogStyle( <nStyle> )

#xcommand LOG <data,...> => HB_StandardLog( HB_BldLogMsg( <data> ) )
#xcommand LOG <data,...> PRIO[RITY] <prio> => ;
      HB_StandardLog( HB_BldLogMsg( <data> ), <prio> )

#xcommand CLOSE LOG =>  HB_CloseStandardLog()

#endif /* HB_LOGDEF_CH */

