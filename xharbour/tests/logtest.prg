************************************************************
* logtest.prg
* $Id: logtest.prg,v 1.1 2003/07/10 10:49:00 jonnymind Exp $
*
* Demonstrates the standard log system
*
* (C) Giancarlo Niccolai
*

#include "hblog.ch"
#include "inkey.ch"

Procedure MAIN()
   LOCAL oLog
   LOCAL nChoice
   LOCAL cMessage
   LOCAL GetList := {}

   // We'll use this to allow users to change level.
   LOCAL aLevelNames := { ;
      "Critical",;
      "Error", ;
      "Warning", ;
      "Information", ;
      "Debug", ;
      "Debug (lower)", ;
      "Debug (still lower)";
   }
   // mapping level names to real HB_ log levels
   LOCAL aLevels := { ;
      HB_LOG_CRITICAL,;
   HB_LOG_ERROR, ;
   HB_LOG_WARNING, ;
   HB_LOG_INFO, ;
   HB_LOG_DEBUG, ;
   HB_LOG_DEBUG+1, ;
   HB_LOG_DEBUG+2 ;
   }

   SET COLOR TO w+/b
   CLEAR SCREEN
   @1,15 SAY "X H A R B O U R - Log test "
   @3,5 SAY "Select a log priority and then write a string in the field."
   @4,5 SAY "The string will be reported to the log with this predecence:"
   @5,10 SAY "To 'console': all messages"
   @6,10 SAY "To the file logtest.log: only INFO or above"
   @7,10 SAY "To the system log: only error or critical."
   @8,5 SAY "Press ESC to select another priority; Press it again to exit"
   @9,5 SAY "*Notice: to demonstrate self log file rolling feature, log file"
   @10,6 SAY "limit is set to 2K"

   // Log can be initialized in any moment
   // Other than the HB_Logger class, there is a "standard log" that is
   // a static HB_Logger instantation, that is accessed with some functions.
   // The following instructions are preprocessor wrappers.

   INIT LOG ON ;
      File( "logtest.log", HB_LOG_INFO, 2, 5 ), ;
      SYSLOG( HB_LOG_ERROR,  0x3ffaaffaa ), ;
      CONSOLE ;
      NAME "Log test program"

   //The above creates the default HB_Logger and automatically adds
   // HB_LogFile channel to logtest.log, INFO and above, 2k, max 5 backups
   // HB_LogSyslog, ERROR and above, application code 0x3ffaaffaa
   // HB_LogConsole, any level.
   // Also, the application name for logging is set to "Log Test Program

   // preparing a "virual console"
   @22,0 SAY "Virtual console  ----------------------------------------"
   @23,0 SAY Space( 80 )
   @23,0

   // a demo of how the log works from within a program
   // LOG can accept also a set of strings or variables separated by commas
   // PRIO[RITY] is optional, If omitted, will log as INFO
   LOG "LogTest", "Hello", "debug" PRIO HB_LOG_DEBUG

   @13,30 SAY "Insert message to log below:"
   cMessage := Space(45)

   nChoice := 1
   DO WHILE nChoice > 0
      MakeBox( 11,2, 19, 25 )
      @11,5 SAY  "Select Level: "
      nChoice := Achoice(12, 4, 18, 24 , aLevelNames)

      IF nChoice > 0
         DO While LastKey() != K_ESC
            @14,30 GET cMessage
            READ
            @10, nChoice say "HERE"
            IF LastKey() != K_ESC
               // Preparing virtual console
               @23,0 SAY Space(80)
               @23,0

               LOG cMessage PRIO aLevels[ nChoice ]
               cMessage := Space( 45 )
            ENDIF
         ENDDO
      ENDIF
   ENDDO

   // Log closing creates a log INFO message
   @23,0 SAY Space(80)
   @23,0
   // closing the log
   CLOSE LOG

   @24,0

RETURN


PROCEDURE MakeBox( nRow, nCol, nRowTo, nColTo )
   @nRow, nCol, nRowTo, nColTo ;
      BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) +;
      Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
RETURN

