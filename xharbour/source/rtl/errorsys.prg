/*
 * $Id: errorsys.prg,v 1.12 2002/05/22 15:24:55 ronpinkas Exp $
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
#define  CRLF HB_OsNewLine()

PROCEDURE ErrorSys

     Errorblock( { | oError | DefError( oError ) } )

Return

STATIC FUNCTION DefError( oError )

     LOCAL cMessage
     LOCAL cDOSError

     LOCAL aOptions
     LOCAL nChoice

     LOCAL n

     // By default, division by zero results in zero
     If oError:genCode == EG_ZERODIV
        Return 0
     Endif

     // Set NetErr() of there was a database open error
     If oError:genCode == EG_OPEN .and. ;
                oError:osCode == 32 .and. ;
                oError:canDefault
        Neterr( .T. )
        Return .F.
     Endif

     // Set NetErr() if there was a lock error on dbAppend()
     If oError:genCode == EG_APPENDLOCK .and. ;
                oError:canDefault
        Neterr( .T. )
        Return .F.
     Endif

     cMessage := ProcName(1) + "(" + Str( ProcLine(1) ) + ") "
     cMessage += ErrorMessage( oError )
     If !Empty( oError:osCode )
        cDOSError := "(DOS Error " + Ltrim( Str( oError:osCode ) ) + ")"
     Endif

     cMessage += " Arguments: (" + Arguments( oError ) + ")"

     // Build buttons

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

     nChoice := 0
     While nChoice == 0

       If Empty( oError:osCode )
          nChoice := Alert( cMessage, aOptions )
       Else
          nChoice := Alert( cMessage + ";" + cDOSError, aOptions )
       Endif

     Enddo

     If !Empty( nChoice )
        Do Case
            Case aOptions[ nChoice ] == "Break"
                Break( oError )
            Case aOptions[ nChoice ] == "Retry"
                Return .T.
            Case aOptions[ nChoice ] == "Default"
                Return .F.
        Endcase
     Endif

     // "Quit" selected

     If !Empty( oError:osCode )
        cMessage += " " + cDOSError
     Endif

     Qout()         /// dgh - Temporary to keep DOS prompt from overwriting message.
     Qout( cMessage )

     n := 2
     While !Empty( Procname( n ) )
       Qout( "Called from " + Procname( n ) + ;
             "(" + Alltrim( Str( Procline( n ++ ) ) ) + ")" )
     Enddo
     /// For some strange reason, the DOS prompt gets written on the first line
     /// *of* the message instead of on the first line *after* the message after
     /// the program quits, unless the screen has scrolled. - dgh
     LogError( oError )
     Quit

Return .F.

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

     LOCAL cScreen     := Savescreen()
     LOCAL cLogFile    := 'error.log'
     LOCAL nWorkArea   := Select()
     LOCAL nRange      := ( Maxcol() + 1 ) * 2
     LOCAL nStart      := 1
     LOCAL nFhandle
     LOCAL nCount
     LOCAL nMemHandle
     LOCAL nForLoop
     LOCAL nMemLength
     LOCAL nMemWidth
     LOCAL cOutString
     LOCAL cSubString
     LOCAL cVarName
     LOCAL cVarType
     LOCAL nLenTemp
     LOCAL cTemp
     LOCAL nBytesLocal
     LOCAL cVarRec
     LOCAL nHandle
     LOCAL nBytes
     LOCAL nMemCount

     //Alert( 'An error occured, Information will be ;written to error.log' )

     nHandle := Fcreate( cLogFile, FC_NORMAL )

     If nHandle < 3
     Else
        FWriteLine( nHandle, Padc( ' Error log file ', 79, '*' ) )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, '              Date: ' + Dtoc( Date() ) )
        FWriteLine( nHandle, '              Time: ' + Time() )
        FWriteLine( nHandle, ' Avaliavle Memory :' + strvalue( Memory( 0 ) ) )
        FWriteLine( nHandle, '     Current Area :' + strvalue( Select() ) )
        FWriteLine( nHandle, Padc( ' Enviromental Information ', 79, '-' ) )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, "Exact is :" + strvalue( Set( 1 ), .T. ) )
        FWriteLine( nHandle, "Fixed is :" + strvalue( Set( 2 ), .T. ) )
        FWriteLine( nHandle, "Decimals is at :" + strvalue( Set( 3 ) ) )
        FWriteLine( nHandle, "Date Format is :" + strvalue( Set( 4 ) ) )
        FWriteLine( nHandle, "Epoch is :" + strvalue( Set( 5 ) ) )
        FWriteLine( nHandle, "Path is :" + strvalue( Set( 6 ) ) )
        FWriteLine( nHandle, "Default is :" + strvalue( Set( 7 ) ) )
        FWriteLine( nHandle, "Exclusive is :" + strvalue( Set( 8 ), .T. ) )
        FWriteLine( nHandle, "SoftSeek is :" + strvalue( Set( 9 ), .T. ) )
        FWriteLine( nHandle, "Unique is :" + strvalue( Set( 10 ), .T. ) )
        FWriteLine( nHandle, "Deleted is :" + strvalue( Set( 11 ), .T. ) )
        FWriteLine( nHandle, "Cancel is :" + strvalue( Set( 12 ), .T. ) )
        FWriteLine( nHandle, "Debug is :" + strvalue( Set( 13 ) ) )
        FWriteLine( nHandle, "Color is :" + strvalue( Set( 15 ) ) )
        FWriteLine( nHandle, "Cursor is :" + strvalue( Set( 16 ) ) )
        FWriteLine( nHandle, "Console is :" + strvalue( Set( 17 ), .T. ) )
        FWriteLine( nHandle, "Alternate is :" + strvalue( Set( 18 ), .T. ) )
        FWriteLine( nHandle, "AltFile is :" + strvalue( Set( 19 ) ) )
        FWriteLine( nHandle, "Device is :" + strvalue( Set( 20 ) ) )
        FWriteLine( nHandle, "Printer is :" + strvalue( Set( 23 ) ) )
        FWriteLine( nHandle, "PrintFile is :" + strvalue( Set( 24 ) ) )
        FWriteLine( nHandle, "Margin is :" + strvalue( Set( 25 ) ) )
        FWriteLine( nHandle, "Bell is :" + strvalue( Set( 26 ), .T. ) )
        FWriteLine( nHandle, "Confirm is :" + strvalue( Set( 27 ), .T. ) )
        FWriteLine( nHandle, "Escape is :" + strvalue( Set( 28 ), .T. ) )
        FWriteLine( nHandle, "Insert is :" + strvalue( Set( 29 ), .T. ) )
        FWriteLine( nHandle, "Intensity is :" + strvalue( Set( 31 ), .T. ) )
        FWriteLine( nHandle, "Scoreboard is :" + strvalue( Set( 32 ), .T. ) )
        FWriteLine( nHandle, "Delimeters is :" + strvalue( Set( 33 ), .T. ) )
        FWriteLine( nHandle, "Delimchars em :" + strvalue( Set( 34 ) ) )
        FWriteLine( nHandle, "Wrap is :" + strvalue( Set( 35 ), .T. ) )
        FWriteLine( nHandle, "Message is :" + strvalue( Set( 36 ) ) )
        FWriteLine( nHandle, "MCenter is :" + strvalue( Set( 37 ), .T. ) )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, Padc( 'Detalaid Work Area Items', Maxcol(), "=" ) )
        FWriteLine( nHandle, "" )
        For nCount := 1 To 600
           If !Empty( ( nCount )->( Alias() ) )
              ( nCount )->( FWriteLine( nHandle, "   Work Area No.: " + strvalue( Select() ) ) )
              ( nCount )->( FWriteLine( nHandle, "   Alias Alias  : " + Alias() ) )
              ( nCount )->( FWriteLine( nHandle, "  Current Recno : " + strvalue( Recno() ) ) )
              ( nCount )->( FWriteLine( nHandle, "  Current Filter: " + Dbfilter() ) )
              ( nCount )->( FWriteLine( nHandle, "   Relation Exp.: " + Dbrelation() ) )
              ( nCount )->( FWriteLine( nHandle, "     Index Order: " + strvalue( Indexord() ) ) )
              ( nCount )->( FWriteLine( nHandle, "      Ative Key : " + strvalue( Indexord() ) ) )
              ( nCount )->( FWriteLine( nHandle, "" ) )
           Endif
        Next

        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, Padc( " Internal Error Handling Information  ", Maxcol(), "+" ) )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "Subsystem Call: " + oErr:subsystem() )
        FWriteLine( nHandle, "   System Code: " + strvalue( oErr:suBcode() ) )
        FWriteLine( nHandle, "Default Status: " + strvalue( oerr:candefault() ) )
        FWriteLine( nHandle, "   Description: " + oErr:description() )
        FWriteLine( nHandle, "     Operation: " + Oerr:operation() )
        FWriteLine( nHandle, "     Arguments:"  + Arguments( oErr ) )
        FWriteLine( nHandle, " Involved File: " + oErr:filename() )
        FWriteLine( nHandle, "Dos Error Code: " + strvalue( oErr:oscode() ) )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, " Trace Through" )
        nCount := 1
        While !Empty( Procname( ++ nCount ) )
          FWriteLine( nHandle, Padr( Procname( nCount ), 21 ) + ': ' + Padr( Procline( nCount ), 20 ) )
        Enddo
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )

        FWriteLine( nHandle, " Video Screen Dump " )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "+" + Replicate( '-', Maxcol() + 1 ) + "+" )
        FWriteLine( nHandle, "" )
        For nCount := 1 To Maxrow()
           cOutString := ''
           cSubString := Substr( cScreen, nStart, nRange )
           For nForLoop := 1 To nRange step 2
              cOutString += Substr( cSubString, nForLoop, 1 )
           Next
           FWriteLine( nHandle, "|" + cOutString + "|" )
           nStart += nRange
        Next
        FWriteLine( nHandle, "+" + Replicate( '-', Maxcol() + 1 ) + "+" )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )

        /*
        FWriteLine( nHandle, padc(" Avaliable Memory Variables ",maxcol(),'+') )
        FWriteLine( nHandle, "" )
        Save All Like * To errormem
        nMemHandle := Fopen( 'errormem.mem', FO_READWRITE )
        nMemLength := Fseek( nMemHandle, 0, 2 )
        Fseek( nMemHandle, 0 )
        nCount := 1
        While Fseek( nMemHandle, 0, 1 ) + 1 < nMemLength
          nMemWidth := Space( 18 )
          Fread( nMemHandle, @nMemWidth, 18 )
          cVarName  := Left( nMemWidth, At( Chr( 0 ), nMemWidth ) - 1 )
          cVarType  := Substr( nMemWidth, 12, 1 )
          cVarRec   := Bin2w( Right( nMemWidth, 2 ) )
          nMemCount := If( cVarType IN Chr( 195 ) + Chr( 204 ), 14 + cVarRec, 22 )
          Fseek( nMemHandle, nMemCount, 1 )
          cTemp  := Left( cVarName + Space( 10 ), 10 )
          cTemp  += " TYPE " + Type( cVarName )
          cTemp  += " " + If( Type( cVarName ) == "C", '"' + &cVarName + '"', strvalue( &cVarName ) )
          nBytes := 0
          Do Case
              Case Type( cVarName ) == "C"
                  nBytes += ( nLenTemp := Len( &cVarName ) )
              Case Type( cVarName ) == "N"
                  nBytes += ( nLenTemp := 9 )
              Case Type( cVarName ) == 'L'
                  nBytes += ( nLenTemp := 2 )
              Case Type( cVarName ) == "D"
                  nBytes += ( nLenTemp := 9 )
          Endcase
          Fwrite( nFhandle, "            " + Transform( nLenTemp, '999999' ) + 'bytes -> ' )
          FWriteLine( nHandle, "      " + cTemp )
        Enddo
        Fclose( nMemHandle )
        Ferase( 'errormem.mem' )
        */
        Fclose( nFhandle )
     Endif
     Errorlevel( 1 )
     Cls
     Close All

Return .f.

STATIC FUNCTION strvalue( c, l )

     LOCAL cr := ''
     Default l To .f.
     Do Case
         Case Valtype( c ) == "C"
             cr := c
         Case Valtype( c ) == "N"
             cr := Alltrim( Str( c ) )
         Case Valtype( c ) == "M"
             cr := c
         Case Valtype( c ) == "D"
             cr := Dtoc( c )
         Case Valtype( c ) == "L"
             cr := If( l, If( c, "On", "Off" ), If( c, "True", "False" ) )
     Endcase
Return cr

STATIC FUNCTION FWriteLine( nh, c )

     Fwrite( nh, c )
     Fwrite( nh, Chr( 13 ) )
     Fwrite( nh, Chr( 10 ) )
Return nil

STATIC FUNCTION Arguments( oErr )

   LOCAL nIndex := 0, xArg, cArguments := ""

   IF ValType( oErr:Args ) == "A"
      FOR EACH xArg IN oErr:Args
         nIndex++
         cArguments += " [" + Str( nIndex, 2 ) + "] = Type: " + ValType( xArg ) + " Val: " + CStr( xArg )
      NEXT
   ENDIF

RETURN cArguments
*+ EOF: ERRORSYS.PRG
