/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The default error handler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 *    TraceLog()
 *    CStr()
 * Copyright 2002 Luiz Rafael Culik <culikr@uol.com.br>
 *    StrValue()
 *    FWriteLine()
 *    LogError()
 */

#include "common.ch"
#include "error.ch"
#include "fileio.ch"
#include "set.ch"

REQUEST Select,Alias,RecNo,DbFilter,DbRelation,IndexOrd,IndexKey

PROCEDURE ErrorSys

     Errorblock( { | oError | DefError( oError ) } )

Return

STATIC FUNCTION DefError( oError )
   LOCAL cMessage
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n

   n := 0
   WHILE ! Empty( ProcName( ++n ) )
      IF ProcName( n ) == ProcName()
         TraceLog( "Error system failure!", oError:ProcName, oError:ProcLine(), oError:ModuleName, oError:description )
         Alert( "Error system failure!;Please correct error handler:;" + oError:ProcName + "(" + LTrim( Str( oError:ProcLine() ) ) +  ") in module: " + oError:ModuleName )
         ErrorLevel( 1 )
         QUIT
      ENDIF
   ENDDO

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oError:genCode == EG_LOCK .AND. ;
      oError:canRetry
      // oError:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Making sure we display the error info!
   DO WHILE DispCount() > 0
      DispEnd()
   ENDDO

   cMessage := ErrorMessage( oError )
   If !Empty( oError:osCode )
      cDOSError := "(DOS Error " + Ltrim( Str( oError:osCode ) ) + ")"
   Endif


   If ValType( oError:Args ) == "A"
     cMessage += " Arguments: (" + Arguments( oError ) + ")"
   Endif

   // Build buttons

   IF MaxCol() > 0
       aOptions := {}

       // AAdd( aOptions, "Break" )
       Aadd( aOptions, "Quit" )

       If oError:canRetry
          Aadd( aOptions, "Retry" )
       Endif

       If oError:canDefault
          Aadd( aOptions, "Default" )
       Endif

       // Show alert box
       //TraceLog( cMessage )

       nChoice := 0
       While nChoice == 0

          If Empty( oError:osCode )
             nChoice := Alert( cMessage, aOptions )
          Else
             nChoice := Alert( cMessage + ";" + cDOSError, aOptions )
          Endif

       Enddo

       IF ! Empty( nChoice )
          DO CASE
          CASE aOptions[ nChoice ] == "Break"
             Break( oError )
          CASE aOptions[ nChoice ] == "Retry"
             RETURN .T.
          CASE aOptions[ nChoice ] == "Default"
             RETURN .F.
          ENDCASE
       ENDIF
   ELSE
      IF Empty( oError:osCode )
         nChoice := Alert( cMessage + ";" + oError:ProcName + "(" + LTrim( Str( oError:ProcLine() ) ) +  ") in module: " + oError:ModuleName )
      ELSE
         nChoice := Alert( cMessage + ";" + cDOSError + ";" + oError:ProcName + "(" + LTrim( Str( oError:ProcLine() ) ) +  ") in module: " + oError:ModuleName )
      ENDIF
   ENDIF

   // "Quit" selected

   IF ! Empty( oError:osCode )
      cMessage += " " + cDOSError
   ENDIF

   ? cMessage

   ?
   ? "Error at ...:", oError:ProcName + "(" + LTrim( Str( oError:ProcLine ) ) + ") in Module:", oError:ModuleName
   n := 2
   WHILE ! Empty( ProcName( ++n ) )
      ? "Called from :", ProcName( n ) + ;
      "(" + LTrim( Str( ProcLine( n ) ) ) + ") in Module:", ProcFile( n )
   ENDDO

/// For some strange reason, the DOS prompt gets written on the first line
/// *of* the message instead of on the first line *after* the message after
/// the program quits, unless the screen has scrolled. - dgh
   LogError( oError )

   ErrorLevel( 1 )
   ?
   QUIT

RETURN .F.

// [vszakats]

STATIC FUNCTION ErrorMessage( oError )

     LOCAL cMessage

     // start error message
     cMessage := Iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

     // add subsystem name if available
     If Ischaracter( oError:subsystem )
        cMessage += oError:subsystem()
     Else
        cMessage += "???"
     Endif

     // add subsystem's error code if available
     If Isnumber( oError:subCode )
        cMessage += "/" + Ltrim( Str( oError:subCode ) )
     Else
        cMessage += "/???"
     Endif

     // add error description if available
     If Ischaracter( oError:description )
        cMessage += "  " + oError:description
     Endif

     // add either filename or operation
     Do Case
         Case !Empty( oError:filename )
             cMessage += ": " + oError:filename
         Case !Empty( oError:operation )
             cMessage += ": " + oError:operation
     Endcase

Return cMessage

STATIC FUNCTION LogError( oerr )

     LOCAL cScreen
     LOCAL aLogFile    := SET( _SET_ERRORLOG )
     LOCAL cLogFile    := aLogFile[1]  // error log file name
     LOCAL lAppendLog  := aLogFile[2]  // .f. = create a new error log (default) .t. = append to a existing one.
     LOCAL nStart      := 1
     LOCAL nCellSize
     LOCAL nRange
     LOCAL nCols
     LOCAL nRows

     LOCAL nCount

     LOCAL nForLoop
     LOCAL cOutString
     LOCAL cSubString

     LOCAL nHandle
     LOCAL nBytes

     LOCAL nHandle2   := -1
     LOCAL cLogFile2  := "_error.log"
     LOCAL cBuff      := ""
     LOCAL nRead      := 0


     nCols := MaxCol()
     IF nCols > 0
        nRows := MaxRow()
        cScreen := Savescreen()
     ENDIF
     //Alert( 'An error occured, Information will be ;written to error.log' )

     If !lAppendLog
        nHandle := FCreate( cLogFile, FC_NORMAL )
     Else
        If !File( cLogFile )
           nHandle := FCreate( cLogFile, FC_NORMAL )
        Else
           nHandle  := FCreate( cLogFile2, FC_NORMAL )
           nHandle2 := FOpen( cLogFile, FO_READ )
        Endif
     Endif


     If nHandle < 3 .and. lower( cLogFile ) != 'error.log'
        // Force creating error.log in case supplied log file cannot
        // be created for any reason
        cLogFile := 'error.log'
        nHandle := Fcreate( cLogFile, FC_NORMAL )
     Endif

     If nHandle < 3
     Else

        FWriteLine( nHandle, Padc( ' xHarbour Error Log ' , 79, '-' ) )
        FWriteLine( nHandle, '' )

        FWriteLine( nHandle, 'Date...............: ' + dtoc( date() )  )
        FWriteLine( nHandle, 'Time...............: ' + time()          )

        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Application name...: ' + hb_cmdargargv() )
        FWriteLine( nHandle, 'Workstation name...: ' + netname() )
        FWriteLine( nHandle, 'Available memory...: ' + strvalue( Memory(0) )  )
        FWriteLine( nHandle, 'Current disk.......: ' + diskname() )
        FWriteLine( nHandle, 'Current directory..: ' + curdir() )
        FWriteLine( nHandle, 'Free disk space....: ' + strvalue( DiskSpace() ) )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Operating system...: ' + os() )
        FWriteLine( nHandle, 'xHarbour version...: ' + version() )
        FWriteLine( nHandle, 'xHarbour built on..: ' + hb_builddate() )
        FWriteLine( nHandle, 'C/C++ compiler.....: ' + hb_compiler() )

        FWriteLine( nHandle, 'Multi Threading....: ' + If( Hb_MultiThread(),"YES","NO" ) )
        FWriteLine( nHandle, 'VM Optimization....: ' + strvalue( Hb_VmMode() ) )

        IF Type( "Select()" ) == "UI" .or. Type( "Select()" )  == "N"
        
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Current Area ......:' + strvalue( &("Select()") ) )
        ENDIF

        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, Padc( ' Environmental Information ', 79, '-' ) )
        FWriteLine( nHandle, '' )

        FWriteLine( nHandle, "SET ALTERNATE......: " + strvalue( Set( _SET_ALTERNATE  ), .T. ) )
        FWriteLine( nHandle, "SET ALTFILE........: " + strvalue( Set( _SET_ALTFILE  )      ) )
        FWriteLine( nHandle, "SET AUTOPEN........: " + strvalue( Set( _SET_AUTOPEN  ), .T. ) )
        FWriteLine( nHandle, "SET AUTORDER.......: " + strvalue( Set( _SET_AUTORDER )      ) )
        FWriteLine( nHandle, "SET AUTOSHARE......: " + strvalue( Set( _SET_AUTOSHARE )      ) )

        FWriteLine( nHandle, "SET BACKGROUNDTASKS: " + strvalue( Set( _SET_BACKGROUNDTASKS ), .T. ) )
        FWriteLine( nHandle, "SET BACKGROUNDTICK.: " + strvalue( Set( _SET_BACKGROUNDTICK ), .T. ) )
        FWriteLine( nHandle, "SET BELL...........: " + strvalue( Set( _SET_BELL  ), .T. ) )
        FWriteLine( nHandle, "SET BLINK..........: " + strvalue( SetBlink()      ) )

        FWriteLine( nHandle, "SET CANCEL.........: " + strvalue( Set( _SET_CANCEL  ), .T. ) )
        FWriteLine( nHandle, "SET CENTURY........: " + strvalue( __SetCentury(), .T. ) )
        FWriteLine( nHandle, "SET COLOR..........: " + strvalue( Set( _SET_COLOR  )      ) )
        FWriteLine( nHandle, "SET CONFIRM........: " + strvalue( Set( _SET_CONFIRM  ), .T. ) )
        FWriteLine( nHandle, "SET CONSOLE........: " + strvalue( Set( _SET_CONSOLE  ), .T. ) )
        FWriteLine( nHandle, "SET COUNT..........: " + strvalue( Set( _SET_COUNT  )      ) )
        FWriteLine( nHandle, "SET CURSOR.........: " + strvalue( Set( _SET_CURSOR  )      ) )

        FWriteLine( nHandle, "SET DATE FORMAT....: " + strvalue( Set( _SET_DATEFORMAT )      ) )
        FWriteLine( nHandle, "SET DBFLOCKSCHEME..: " + strvalue( Set( _SET_DBFLOCKSCHEME )      ) )
        FWriteLine( nHandle, "SET DEBUG..........: " + strvalue( Set( _SET_DEBUG ), .T. ) )
        FWriteLine( nHandle, "SET DECIMALS.......: " + strvalue( Set( _SET_DECIMALS )      ) )
        FWriteLine( nHandle, "SET DEFAULT........: " + strvalue( Set( _SET_DEFAULT )      ) )
        FWriteLine( nHandle, "SET DEFEXTENSIONS..: " + strvalue( Set( _SET_DEFEXTENSIONS ), .T. ) )
        FWriteLine( nHandle, "SET DELETED........: " + strvalue( Set( _SET_DELETED ), .T. ) )
        FWriteLine( nHandle, "SET DELIMCHARS.....: " + strvalue( Set( _SET_DELIMCHARS )      ) )
        FWriteLine( nHandle, "SET DELIMETERS.....: " + strvalue( Set( _SET_DELIMITERS ), .T. ) )
        FWriteLine( nHandle, "SET DEVICE.........: " + strvalue( Set( _SET_DEVICE )      ) )
        FWriteLine( nHandle, "SET DIRCASE........: " + strvalue( Set( _SET_DIRCASE )      ) )
        FWriteLine( nHandle, "SET DIRSEPARATOR...: " + strvalue( Set( _SET_DIRSEPARATOR )      ) )

        FWriteLine( nHandle, "SET EOL............: " + strvalue( Asc( Set( _SET_EOL ) ) )  )
        FWriteLine( nHandle, "SET EPOCH..........: " + strvalue( Set( _SET_EPOCH )      ) )
        FWriteLine( nHandle, "SET ERRORLOG.......: " + if(!Empty(aLogFile), strvalue( aLogFile[1] )+","+strvalue( aLogFile[2] ), "") )
        FWriteLine( nHandle, "SET ERRORLOOP......: " + strvalue( Set( _SET_ERRORLOOP )      ) )
        FWriteLine( nHandle, "SET ESCAPE.........: " + strvalue( Set( _SET_ESCAPE ), .T. ) )
        FWriteLine( nHandle, "SET EVENTMASK......: " + strvalue( Set( _SET_EVENTMASK )      ) )
        FWriteLine( nHandle, "SET EXACT..........: " + strvalue( Set( _SET_EXACT ), .T. ) )
        FWriteLine( nHandle, "SET EXCLUSIVE......: " + strvalue( Set( _SET_EXCLUSIVE ), .T. ) )
        FWriteLine( nHandle, "SET EXIT...........: " + strvalue( Set( _SET_EXIT ), .T. ) )
        FWriteLine( nHandle, "SET EXTRA..........: " + strvalue( Set( _SET_EXTRA ), .T. ) )
        FWriteLine( nHandle, "SET EXTRAFILE......: " + strvalue( Set( _SET_EXTRAFILE )      ) )

        FWriteLine( nHandle, "SET FILECASE.......: " + strvalue( Set( _SET_FILECASE )      ) )
        FWriteLine( nHandle, "SET FIXED..........: " + strvalue( Set( _SET_FIXED ), .T. ) )
        FWriteLine( nHandle, "SET FORCEOPT.......: " + strvalue( Set( _SET_FORCEOPT ), .T. ) )

        FWriteLine( nHandle, "SET HARDCOMMIT.....: " + strvalue( Set( _SET_HARDCOMMIT ), .T. ) )

        FWriteLine( nHandle, "SET IDLEREPEAT.....: " + strvalue( Set( _SET_IDLEREPEAT ), .T. ) )
        FWriteLine( nHandle, "SET INSERT.........: " + strvalue( Set( _SET_INSERT ), .T. ) )
        FWriteLine( nHandle, "SET INTENSITY......: " + strvalue( Set( _SET_INTENSITY ), .T. ) )

        FWriteLine( nHandle, "SET LANGUAGE.......: " + strvalue( Set( _SET_LANGUAGE )      ) )

        FWriteLine( nHandle, "SET MARGIN.........: " + strvalue( Set( _SET_MARGIN )      ) )
        FWriteLine( nHandle, "SET MBLOCKSIZE.....: " + strvalue( Set( _SET_MBLOCKSIZE )      ) )
        FWriteLine( nHandle, "SET MCENTER........: " + strvalue( Set( _SET_MCENTER ), .T. ) )
        FWriteLine( nHandle, "SET MESSAGE........: " + strvalue( Set( _SET_MESSAGE )      ) )
        FWriteLine( nHandle, "SET MFILEEXT.......: " + strvalue( Set( _SET_MFILEEXT )      ) )

        FWriteLine( nHandle, "SET OPTIMIZE.......: " + strvalue( Set( _SET_OPTIMIZE ), .T. ) )
        FWriteLine( nHandle, "SET OUTPUTSAFETY...: " + strvalue( Set( _SET_OUTPUTSAFETY ), .T. ) )

        FWriteLine( nHandle, "SET PATH...........: " + strvalue( Set( _SET_PATH )      ) )
        FWriteLine( nHandle, "SET PRINTER........: " + strvalue( Set( _SET_PRINTER ), .T. ) )
        FWriteLine( nHandle, "SET PRINTERJOB.....: " + strvalue( Set( _SET_PRINTERJOB )      ) )
        FWriteLine( nHandle, "SET PRINTFILE......: " + strvalue( Set( _SET_PRINTFILE )      ) )

        FWriteLine( nHandle, "SET SCOREBOARD.....: " + strvalue( Set( _SET_SCOREBOARD ), .T. ) )
        FWriteLine( nHandle, "SET SCROLLBREAK....: " + strvalue( Set( _SET_SCROLLBREAK ), .T. ) )
        FWriteLine( nHandle, "SET SOFTSEEK.......: " + strvalue( Set( _SET_SOFTSEEK ), .T. ) )
        FWriteLine( nHandle, "SET STRICTREAD.....: " + strvalue( Set( _SET_STRICTREAD ), .T. ) )

        FWriteLine( nHandle, "SET TRACE..........: " + strvalue( Set( _SET_TRACE ), .T. ) )
        FWriteLine( nHandle, "SET TRACEFILE......: " + strvalue( Set( _SET_TRACEFILE )      ) )
        FWriteLine( nHandle, "SET TRACESTACK.....: " + strvalue( Set( _SET_TRACESTACK )      ) )
        FWriteLine( nHandle, "SET TRIMFILENAME...: " + strvalue( Set( _SET_TRIMFILENAME )      ) )

        FWriteLine( nHandle, "SET TYPEAHEAD......: " + strvalue( Set( _SET_TYPEAHEAD )      ) )

        FWriteLine( nHandle, "SET UNIQUE.........: " + strvalue( Set( _SET_UNIQUE ), .T. ) )

        FWriteLine( nHandle, "SET VIDEOMODE......: " + strvalue( Set( _SET_VIDEOMODE )      ) )

        FWriteLine( nHandle, "SET WRAP...........: " + strvalue( Set( _SET_WRAP ), .T. ) )


        FWriteLine( nHandle, "" )

        IF nCols > 0
            FWriteLine( nHandle, Padc( 'Detailed Work Area Items', nCols, '-' ) )
        ELSE
            FWriteLine( nHandle, 'Detailed Work Area Items ' )
        ENDIF
        FWriteLine( nHandle, "" )

        IF Type( "Select()" ) == "UI" .or. Type( "Select()" )  == "N"
           For nCount := 1 To 600
              If !Empty( ( nCount )->( &("Alias()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Work Area No ......: " + strvalue( &("Select()") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Alias .............: " + &("Alias()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Current Recno .....: " + strvalue( &("RecNo()") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Current Filter ....: " + &("DbFilter()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Relation Exp. .....: " + &("DbRelation()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Index Order .......: " + strvalue( &("IndexOrd(0)") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Active Key ........: " + strvalue( &("IndexKey(0)") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "" ) )
              Endif
           Next
        ENDIF

        FWriteLine( nHandle, "" )
        IF nCols > 0
            FWriteLine( nHandle, Padc( " Internal Error Handling Information  ", nCols, "-" ) )
        ELSE
            FWriteLine( nHandle, " Internal Error Handling Information  " )
        ENDIF
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "Subsystem Call ....: " + oErr:subsystem() )
        FWriteLine( nHandle, "System Code .......: " + strvalue( oErr:suBcode() ) )
        FWriteLine( nHandle, "Default Status ....: " + strvalue( oerr:candefault() ) )
        FWriteLine( nHandle, "Description .......: " + oErr:description() )
        FWriteLine( nHandle, "Operation .........: " + oErr:operation() )
        FWriteLine( nHandle, "Arguments .........: " + Arguments( oErr ) )
        FWriteLine( nHandle, "Involved File .....: " + oErr:filename() )
        FWriteLine( nHandle, "Dos Error Code ....: " + strvalue( oErr:oscode() ) )

        hb_Error_MT( nHandle, oErr, { |o| strvalue( o ) } )

        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, " Trace Through:" )
        FWriteLine( nHandle, "----------------" )

        FWriteLine( nHandle, Padr( oErr:ProcName, 21 ) + " : " + Transform( oErr:ProcLine, "999,999" ) + " in Module: " + oErr:ModuleName )
        nCount := 3
        While !Empty( Procname( ++ nCount ) )
          FWriteLine( nHandle, Padr( Procname( nCount ), 21 ) + ' : ' + Transform( Procline( nCount ), "999,999" ) + " in Module: " + ProcFile( nCount ) )
        Enddo

        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )

        IF valtype( cScreen ) == "C"
            FWriteLine( nHandle, Padc( " Video Screen Dump ", nCols, "#" ) )
            FWriteLine( nHandle, "" )
            //FWriteLine( nHandle, "" )
            FWriteLine( nHandle, "+" + Replicate( '-', nCols + 1 ) + "+" )
            //FWriteLine( nHandle, "" )
            nCellSize := len( Savescreen( 0, 0, 0, 0 ) )
            nRange := ( nCols + 1 ) * nCellSize
            For nCount := 1 To nRows + 1
               cOutString := ''
               cSubString := Substr( cScreen, nStart, nRange )
               For nForLoop := 1 To nRange step nCellSize
                  cOutString += Substr( cSubString, nForLoop, 1 )
               Next
               FWriteLine( nHandle, "|" + cOutString + "|" )
               nStart += nRange
            Next
            FWriteLine( nHandle, "+" + Replicate( '-', nCols + 1 ) + "+" )
            FWriteLine( nHandle, "" )
            FWriteLine( nHandle, "" )
        ELSE
            FWriteLine( nHandle, " Video Screen Dump not available" )
        ENDIF


    /*
     *  FWriteLine( nHandle, padc(" Available Memory Variables ",nCols,'+') )
     *  FWriteLine( nHandle, "" )
     *  Save All Like * To errormem
     *  nMemHandle := Fopen( 'errormem.mem', FO_READWRITE )
     *  nMemLength := Fseek( nMemHandle, 0, 2 )
     *  Fseek( nMemHandle, 0 )
     *  nCount := 1
     *  While Fseek( nMemHandle, 0, 1 ) + 1 < nMemLength
     *    nMemWidth := Space( 18 )
     *    Fread( nMemHandle, @nMemWidth, 18 )
     *    cVarName  := Left( nMemWidth, At( Chr( 0 ), nMemWidth ) - 1 )
     *    cVarType  := Substr( nMemWidth, 12, 1 )
     *    cVarRec   := Bin2w( Right( nMemWidth, 2 ) )
     *    nMemCount := If( cVarType IN Chr( 195 ) + Chr( 204 ), 14 + cVarRec, 22 )
     *    Fseek( nMemHandle, nMemCount, 1 )
     *    cTemp  := Left( cVarName + Space( 10 ), 10 )
     *    cTemp  += " TYPE " + Type( cVarName )
     *    cTemp  += " " + If( Type( cVarName ) == "C", '"' + &cVarName + '"', strvalue( &cVarName ) )
     *    nBytes := 0
     *    Switch ValType( cVarName )
     *        Case "C"
     *            nBytes += ( nLenTemp := Len( &cVarName ) )
     *            exit
     *        Case "N"
     *            nBytes += ( nLenTemp := 9 )
     *            exit
     *        Case 'L'
     *            nBytes += ( nLenTemp := 2 )
     *            exit
     *        Case "D"
     *            nBytes += ( nLenTemp := 9 )
     *            exit
     *    End
     *    Fwrite( nFhandle, "            " + Transform( nLenTemp, '999999' ) + 'bytes -> ' )
     *    FWriteLine( nHandle, "      " + cTemp )
     *  Enddo
     *  Fclose( nMemHandle )
     *  Ferase( 'errormem.mem' )
     */
        if lAppendLog .and. nHandle2 != -1

           nBytes := FSeek( nHandle2, 0, FS_END )

           cBuff := space(10)
           FSeek( nHandle2, 0, FS_SET )

           while nBytes > 0
             nRead := FRead( nHandle2, @cBuff, 10 )
             FWrite( nHandle, cBuff, nRead )
             nBytes -= nRead
             cBuff := space( 10 )
           enddo

           FClose( nHandle2 )
           FClose( nHandle )

           FErase( cLogFile )
           FRename( cLogFile2, cLogFile )
        else
           FClose( nHandle )
        endif

     Endif

Return .f.

STATIC FUNCTION strvalue( c, l )

     LOCAL cr := ''
     Default l To .f.
     Switch ValType( c )
         Case "C"
             cr := c
             exit
         Case "N"
             cr := Alltrim( Str( c ) )
             exit
         Case "M"
             cr := c
             exit
         Case "D"
             cr := Dtoc( c )
             exit
         Case "L"
//             cr := If( l, If( c, "On", "Off" ), If( c, "True", "False" ) )
             cr := If( l, If( c, "On", "Off" ), If( c, ".t.", ".f." ) )
             exit
     End
Return Upper( cr )

STATIC FUNCTION FWriteLine( nh, c )

   Fwrite( nh, c + HB_OsNewLine() )
   //HB_OutDebug( c + HB_OsNewLine() )
Return nil

STATIC FUNCTION Arguments( oErr )

   LOCAL xArg, cArguments := ""

   IF ValType( oErr:Args ) == "A"
      FOR EACH xArg IN oErr:Args
         cArguments += " [" + Str( HB_EnumIndex(), 2 ) + "] = Type: " + ValType( xArg )

         IF xArg != NIL
            cArguments +=  " Val: " + CStr( xArg )
         ENDIF
      NEXT
   ENDIF

RETURN cArguments

#ifdef __PLATFORM__Windows
#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbvmpub.h"
#include "hbfast.h"
#include "hbstack.h"
#include "thread.h"

#include <windows.h>

static PHB_SYMB s_xHbFunc = NULL;

LONG WINAPI PRGUnhandledExceptionFilter( EXCEPTION_POINTERS *ExceptionInfo )
{
   if( s_xHbFunc )
   {
      HB_ITEM Exception;
      PHB_DYNS pDyn = hb_dynsymFind( "HB_CSTRUCTURE" );

      Exception.type = HB_IT_NIL;

      //TraceLog( NULL, "%s(%p)\n", pExecSym->pSymbol->szName, ExceptionInfo );

      if( pDyn )
      {
         hb_vmPushSymbol( pDyn->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "EXCEPTION_POINTERS", 18 );
         hb_vmPushLong( 8 );
         hb_vmDo( 2 );

         if( hb_stackReturnItem()->type == HB_IT_OBJECT )
         {
            HB_ITEM_NEW( Buffer );
            HB_ITEM Adopt;

            hb_itemForwardValue( &Exception, hb_stackReturnItem() );

            hb_itemPutCLStatic( &Buffer, (char *) ExceptionInfo, sizeof( EXCEPTION_POINTERS ) );

            Adopt.type = HB_IT_LOGICAL;
            Adopt.item.asLogical.value = FALSE;

            hb_objSendMsg( &Exception, "Buffer", 2, &Buffer, &Adopt );
         }
      }

      hb_vmPushSymbol( s_xHbFunc );
      hb_vmPushNil();
      hb_itemPushForward( &Exception );
      hb_vmDo( 1 );

      //TraceLog( NULL, "Done\n" );
   }

   return hb_itemGetNL( hb_stackReturnItem() );
}

HB_FUNC( SETERRORMODE )
{
   hb_retni( SetErrorMode( hb_parni( 1 ) ) ) ;
}

HB_FUNC( SETUNHANDLEDEXCEPTIONFILTER )
{
   LPTOP_LEVEL_EXCEPTION_FILTER pDefaultHandler;

   s_xHbFunc = (PHB_SYMB) hb_parptr( 1 );

   pDefaultHandler = SetUnhandledExceptionFilter( PRGUnhandledExceptionFilter );
   //TraceLog( NULL, "Default: %p\n", pDefaultHandler );

   hb_retnl( (LONG) pDefaultHandler );
}

#pragma ENDDUMP

#endif
*+ EOF: ERRORSYS.PRG
