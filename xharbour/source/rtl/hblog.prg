/*
* $Id: hblog.prg,v 1.4 2003/07/17 19:19:41 jonnymind Exp $
*/

/*
* xHarbour Project source code:
* Versatile logging system.
*
* Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
* www - http://www.xharbour.org
*
* this program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* this program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
* GNU General public License for more details.
*
* You should have received a copy of the GNU General public License
* along with this software; see the file COPYING.  if not, write to
* the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
* Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* this exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General public License.
*
* this exception applies only to the code released with this xHarbour
* explicit exception.  if you add/copy code from other sources,
* as the General public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* if you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* if you do not wish that, delete this exception notice.
*
*/

#include "hbclass.ch"
#include "hblog.ch"
#include "fileio.ch"


/*************************************************************
* Static standard logger access
*/

#ifdef HB_THREAD_SUPPORT
   STATIC StdLogMutex
#endif

STATIC StdLogger

PROCEDURE HB_InitStandardLog( ... )
   LOCAL nCount

   StdLogger := HB_Logger():New()
   #ifdef HB_THREAD_SUPPORT
      StdLogMutex := CreateMutex()
   #endif

   FOR nCount := 1 TO PCount()
      #ifdef HB_THREAD_SUPPORT
         MutexLock( StdLogMutex )
      #Endif

      StdLogger:AddChannel( HB_Pvalue( nCount ) )

      #ifdef HB_THREAD_SUPPORT
         MutexUnlock( StdLogMutex )
      #Endif
   NEXT

   #ifdef HB_THREAD_SUPPORT
      MutexLock( StdLogMutex )
   #Endif

   StdLogger:SetStyle( HB_LOG_ST_DATE + HB_LOG_ST_ISODATE+;
         HB_LOG_ST_TIME + HB_LOG_ST_LEVEL )

   #ifdef HB_THREAD_SUPPORT
      MutexUnlock( StdLogMutex )
   #endif


   // This avoid using the LOG mutex to open the logs, as each
   // log channel could have his own mutexes, and could be subject
   // of cross locking.

   FOR nCount := 1 TO PCount()
      HB_Pvalue( nCount ):Open()
   NEXT

RETURN


PROCEDURE HB_StandardLogAdd( oChannel )

   IF StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         MutexLock( StdLogMutex )
      #Endif

      StdLogger:AddChannel( oChannel )

      #ifdef HB_THREAD_SUPPORT
         MutexUnlock( StdLogMutex )
      #endif

      oChannel:Open()

   ENDIF

RETURN

PROCEDURE HB_CloseStandardLog()

   // If the logger is NIL also the mutex is NIL
   IF StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         MutexLock( StdLogMutex )
      #Endif

      StdLogger:Close()

      #ifdef HB_THREAD_SUPPORT
         MutexUnlock( StdLogMutex )
         DestroyMutex( StdLogMutex )
      #Endif

   ENDIF
RETURN


PROCEDURE HB_SetStandardLogStyle( nStyle )
   IF StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         MutexLock( StdLogMutex )
      #Endif

      StdLogger:SetStyle( nStyle )

      #ifdef HB_THREAD_SUPPORT
         MutexUnlock( StdLogMutex )
      #Endif
   ENDIF
RETURN


PROCEDURE HB_StandardLog( cMsg, nPrio )
   IF StdLogger != NIL
      #ifdef HB_THREAD_SUPPORT
         MutexLock( StdLogMutex )
      #Endif

      StdLogger:Log( cMsg, nPrio )

      #ifdef HB_THREAD_SUPPORT
         MutexUnlock( StdLogMutex )
      #Endif
   ENDIF
RETURN

FUNCTION HB_BldLogMsg( ... )
   LOCAL nCount
   LOCAL cMsg := ""

   FOR nCount := 1 TO PCount()
      cMsg += CStr( HB_PValue( nCount ) )
      IF nCount < PCount()
         cMsg += " "
      ENDIF
   NEXT

RETURN cMsg


FUNCTION HB_LogDateStamp()
   LOCAL dToday := Date()
RETURN  Str(Year( dToday ), 4 ) +"-"+ Padl( Month( dToday ) , 2, "0" ) +;
         "-" + Padl( Day( dToday ), 2, "0" )

/**********************************************
* Logger class
***********************************************/

CLASS HB_Logger
   DATA aLogToChannel                  INIT  {}
   DATA nStyle                         INIT  -1
   DATA nDefaultPriority               INIT  HB_LOG_INFO

   METHOD New()
   METHOD AddChannel( oChannel )       INLINE Aadd( ::aLogToChannel, oChannel )

   METHOD SetStyle( nStyle )           INLINE ::nStyle := nStyle

   METHOD Open()
   METHOD Close()

   METHOD Log( cMessage, nPriority )

ENDCLASS

/**
* Builds a new logger object.
* Call with ::New( ch1, ch2... chN ) where ch are the channels
* where to log.
* Channels can be called at a second time.
*/

METHOD New() CLASS HB_Logger
   LOCAL nCount

   FOR nCount := 1 TO PCount()
      ::AddChannel( HB_PValue( nCount ) )
   NEXT
RETURN Self

/**
* Open all the channels calling their ::Open() method
*/

METHOD Open() CLASS HB_Logger
   LOCAL oChannel

   FOR EACH oChannel IN ::aLogToChannel
      oChannel:Open()
   NEXT

RETURN NIL

/**
* Close all the channels calling their ::Close() method
*/

METHOD Close() CLASS HB_Logger
   LOCAL oChannel

   FOR EACH oChannel IN ::aLogToChannel
      oChannel:Close()
   NEXT

RETURN NIL

/**
* Send a log message to all the channels
*/

METHOD Log( cMessage, nPriority ) CLASS HB_Logger
   LOCAL oChannel
   LOCAL cPrefix := ""

   IF nPriority == NIL
      nPriority := ::nDefaultPriority
   ENDIF

   FOR EACH oChannel IN ::aLogToChannel
      /* Channels may want to have something to say about the format,
         so message formatting is done by the channels */
      oChannel:Log( ::nStyle, cMessage, nPriority )
   NEXT

RETURN NIL

/**********************************************
* Logger Channel class (mostly VIRTUAL)
***********************************************/

CLASS HB_LogChannel
   DATA cProgramName
   DATA lOpened                  INIT .F.

   METHOD New( nLevel, cName )   CONSTRUCTOR
   METHOD Open()                 VIRTUAL
   METHOD Close()                VIRTUAL

   METHOD Log( cMessage, nPriority )
   METHOD SetActive( lAct )      INLINE   ::lActive := lAct

   METHOD Format( nStyle, cMessage, nPriority )

PROTECTED:
   METHOD Send( nStyle, cMessage, nPriority )       VIRTUAL

HIDDEN:
   DATA nLevel
   DATA lActive                  INIT .T.

ENDCLASS

/**
*  Creates a new channel. nLeven can be nil ( and will log all ),
*  cName is the "program name" and must be given
*/
METHOD New( nLevel, cName ) CLASS HB_LogChannel

   IF nLevel == NIL
      // log everything by default
      nLevel := HB_LOG_ALL
   ENDIF

   ::nLevel := nLevel
   ::cProgramName := cName

RETURN Self

/**
* Log the message: it send a request to the subclass "send" method
* if the log level is higher or equal than the channel setting
*/

METHOD Log( nStyle, cMessage, nPriority ) CLASS HB_LogChannel
   IF nPriority <= ::nLevel .and. ::lActive
      ::Send( nStyle, cMessage, nPriority )
   ENDIF
RETURN NIL

/**
* This is an utility functions for subclasses, used to
* have a standard formatting for the message. Subclasses
* may or may not call it.
*/

METHOD Format( nStyle, cMessage, nPriority ) CLASS HB_LogChannel
   LOCAL cPrefix := ""

   IF HB_BitAnd( nStyle, HB_LOG_ST_DATE ) > 0
      IF HB_BitAnd( nStyle, HB_LOG_ST_ISODATE ) > 0
         cPrefix += HB_LogDateStamp()
      ELSE
         cPrefix += DtoC( Date() )
      ENDIF
      cPrefix += " "
   ENDIF

   IF HB_BitAnd( nStyle, HB_LOG_ST_TIME ) > 0
      cPrefix += Time() + " "
   ENDIF

   IF HB_BitAnd( nStyle, HB_LOG_ST_LEVEL ) > 0
      SWITCH nPriority
         CASE HB_LOG_CRITICAL
            cPrefix += "CRITICAL: "
         EXIT

         CASE HB_LOG_ERROR
            cPrefix += "ERROR: "
         EXIT

         CASE HB_LOG_WARNING
            cPrefix += "WARNING: "
         EXIT

         CASE HB_LOG_INFO
            cPrefix += "INFO: "
         EXIT

         CASE HB_LOG_DEBUG
            cPrefix += "DEBUG: "
         EXIT

         DEFAULT
            cPrefix += "DEBUG" + Alltrim( Str(nPriority - HB_LOG_DEBUG) )+  ": "
      END
   ENDIF

RETURN cPrefix + cMessage

/**********************************************
* Console channel
***********************************************/

CLASS HB_LogConsole FROM HB_LogChannel
   METHOD New( cName, nLevel )          INLINE ::Super:New( cName, nLevel )
   METHOD Open()
   METHOD Close()

PROTECTED:
   METHOD Send( nStyle, cMessage, nPriority )

ENDCLASS

METHOD Open() CLASS HB_LogConsole
   IF ::lOpened
      RETURN .F.
   ENDIF

   IF ::cProgramName == NIL
      HB_FnameSplit( hb_argv(0),,@::cProgramName )
   ENDIF

   OutStd( HB_LogDateStamp(), Time(), "--", ::cProgramName, "start --", HB_OsNewLine() )
   ::lOpened := .T.
RETURN .T.

METHOD Close() CLASS HB_LogConsole
   IF .not. ::lOpened
      RETURN .F.
   ENDIF

   OutStd( HB_LogDateStamp(), Time(), "--", ::cProgramName, "end --", HB_OsNewLine() )
   ::lOpened := .F.
RETURN .T.

METHOD Send( nStyle, cMessage, nPriority )
   OutStd( ::Format( nStyle, cMessage, nPriority ) , HB_OsNewLine() )
RETURN NIL


/**********************************************
* Console channel - to file
***********************************************/

CLASS HB_LogFile FROM HB_LogChannel
   DATA cFileName
   DATA nFileHandle
   DATA nFileLimit         INIT -1
   DATA nBackup            INIT 5

   METHOD New( nLevel, cName, cFile, nMaxSize, nBackup )
   METHOD Open()
   METHOD Close()

PROTECTED:
   METHOD Send( nStyle, cMessage, nPriority )

ENDCLASS


METHOD New( nLevel, cProgName, cFilename, nMaxSize, nBackup ) CLASS HB_LogFile
   ::Super:New( nLevel, cProgName )
   ::cFileName := cFileName

   IF nMaxSize != NIL
      ::nFileLimit := nMaxSize
   ENDIF

   IF nBackup != NIL
      ::nBackup := nBackup
   ENDIF
RETURN Self


METHOD Open() CLASS HB_LogFile
   IF ::lOpened
      RETURN .F.
   ENDIF

   IF File( ::cFileName )
      ::nFileHandle := FOpen( ::cFileName, FO_READWRITE )
      IF ::nFileHandle > 0
         Fseek( ::nFileHandle, 0 ,FS_END )
      END
   ELSE
      ::nFileHandle := FCreate( ::cFileName )
   ENDIF

   IF ::nFileHandle < 0
      RETURN .F.
   ENDIF

   IF ::cProgramName == NIL
      HB_FnameSplit( hb_argv(0),,@::cProgramName )
   ENDIF

   Fwrite( ::nFileHandle,;
      HB_BldLogMsg( HB_LogDateStamp(), Time(), "--", ::cProgramName, ;
         "start --", HB_OsNewLine() ) )

   ::lOpened := .T.
RETURN .T.

METHOD Close() CLASS HB_LogFile
   IF .not. ::lOpened
      RETURN .F.
   ENDIF

   Fwrite( ::nFileHandle,;
      HB_BldLogMsg( HB_LogDateStamp(), Time(), "--", ::cProgramName, ;
         "end --", HB_OsNewLine() ) )

   FClose( ::nFileHandle )
   ::nFileHandle := -1

   ::lOpened := .F.
RETURN .T.

METHOD Send( nStyle, cMessage, nPrio ) CLASS HB_LogFile
   LOCAL cExt, nCount

   FWrite( ::nFileHandle, ::Format( nStyle, cMessage, nPrio ) + HB_OsNewLine() )

   // see file limit and eventually swap file.
   IF ::nFileLimit > 0 .and. FError() == 0
      IF FSeek( ::nFileHandle, 0, FS_RELATIVE ) > ::nFileLimit * 1024
          Fwrite( ::nFileHandle,;
            HB_BldLogMsg( HB_LogDateStamp(), Time(), ;
            "LogFile: Closing file due to size limit breaking", HB_OsNewLine() ) )
         FClose( ::nFileHandle )

         IF ::nBackup > 1
            FOR nCount := ::nBackup -1 TO 1 STEP -1
               FRename( ::cFileName +"." + Padl( nCount-1, 3,"0" ), ;
                   ::cFileName + "." + Padl( nCount, 3,"0" ) )
            NEXT
         ENDIF

         IF FRename( ::cFileName, ::cFileName + ".000" ) == 0
            ::nFileHandle := FCreate( ::cFileName )
            Fwrite( ::nFileHandle,;
               HB_BldLogMsg( HB_LogDateStamp(), Time(), ;
               "LogFile: Reopening file due to size limit breaking", HB_OsNewLine() ) )
         ENDIF
      ENDIF
   ENDIF

RETURN Ferror() == 0


/**********************************************
* Syslog channel - a wrapper for the low level
* C interface to syslog/ event log system
***********************************************/

CLASS HB_LogSyslog FROM HB_LogChannel
   DATA nId

   METHOD New( nLevel, cName, nId )
   METHOD Open()
   METHOD Close()

PROTECTED:
   METHOD Send( cMessage, nPrio )

ENDCLASS

METHOD New( nLevel, cName, nId ) CLASS HB_LogSyslog
   ::Super:New( nLevel, cName )
   ::nId := nId
RETURN SELF

METHOD Open() CLASS HB_LogSyslog
   IF ::lOpened
      RETURN .F.
   ENDIF

   IF HB_SyslogOpen( ::cProgramName )
      ::lOpened := .T.
      RETURN .T.
   ENDIF
RETURN .F.

METHOD Close() CLASS HB_LogSyslog
   IF .not. ::lOpened
      RETURN .F.
   ENDIF

   IF HB_SyslogClose( ::cProgramName )
      ::lOpened := .F.
      RETURN .T.
   ENDIF
RETURN .F.

METHOD Send( nType, cMessage, nPrio ) CLASS HB_LogSyslog
// Syslog does not need timestamp, nor priority
RETURN HB_SyslogMessage( ::Format( HB_LOG_ST_LEVEL, cMessage, nPrio ), nPrio, ::nId )


