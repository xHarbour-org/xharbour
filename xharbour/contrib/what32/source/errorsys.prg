
// WHAT32 ErrorSys
// A.J. Wos 08/06/2002 ( scaled down and adapted for Harbour + What32.Lib )


#Define TRUE .T.
#Define FALSE .F.

#Define WIN_WANT_ALL
#Include "windows.ch"
#Include "error.ch"
//#define ES_READONLY            2048
#xTranslate NTRIM( < n > ) = > lTrim( Str( < n > ) )
#Define CRLF chr( 13 ) + chr( 10 )
#Define CR chr( 13 )

#Define LOGFILE error.log // don't use quotes

*-----------------------------------------------------------------------------*

Procedure ErrorSys( )

   ErrorBlock( { | e | DefError( e ) } )
   //OutputDebugString( "What32 Errorsys in use" + CRLF )

   Return

*-----------------------------------------------------------------------------*

Static Function DefError( e )

   Local cMessage, aOptions, nChoice
   Local hCursor, hOldcursor, cErr, SourceLine
   Local cProcStack := ''
   Local i

   If e:genCode == EG_PRINT
      Return PrintError( )
   EndIf
   If ( e:genCode == EG_ZERODIV )
      Return ( 0 )
   EndIf
   If ( e:genCode == EG_OPEN .AND. e:osCode == 32 .AND. e:canDefault )
      NetErr( .T. )
      Return ( .F. ) // NOTE
   EndIf
   If ( e:genCode == EG_APPENDLOCK .AND. e:canDefault )
      NetErr( .T. )
      Return ( .F. ) // NOTE
   EndIf

   //SourceLine := Trim( ProcName( 2 ) ) + "(" + NTRIM( ProcLine( 2 ) ) + ")" + CRLF + Trim( ProcName( 3 ) ) + "(" + NTRIM( ProcLine( 3 ) ) + ")"
   OutputDebugString( e:description + CHR( 13 ) + "Procedure Stack Depth:" + ntrim( getprocstack( ) ) + CRLF )

   i := 2
   Do While ( ! Empty( ProcName( i ) ) )
      cProcStack += ( CRLF + Trim( ProcName( i ) ) + "(" + NTRIM( ProcLine( i ++ ) ) + ")" )
      If ProcName( i ) == 'DEFERROR' // Oops, recursive arror, cannot continue !
         OutputDebugString( "Recursive error" + CRLF )
         PostQuitMessage( 0 )
         Errorlevel( 1 )
         OutputDebugString( cProcStack + CRLF )
         // quit
         Return( .F. )
      EndIf
   EndDo

   OutputDebugString( cProcStack + CRLF )

   cErr := LogError( e, cProcStack )
   cMessage := ErrorMessage( e )

   aOptions := { "Quit" }
   If ( e:canRetry )
      aAdd( aOptions, "Retry" )
   End
   If ( e:canDefault )
      aAdd( aOptions, "Default" )
   End

   nChoice := 0

   hCursor := LoadCursor( , IDC_WAIT )
   hOldcursor := SetCursor( hCursor )
   SetCursor( hOldcursor )

   If ( Empty( e:osCode ) )
      nChoice := eAlert( cMessage, aOptions, cErr )
   Else
      nChoice := eAlert( cMessage + ;
                         ";(OS Error " + NTRIM( e:osCode ) + ")", ;
                         aOptions, cErr )
   End

   If ( ! Empty( nChoice ) )

      // do as instructed
      If ( aOptions[ nChoice ] == "Break" )
         Set DELETED On
         Break( e )
         Return( .F. )
      ElseIf ( aOptions[ nChoice ] == "Retry" )
         Return ( .T. )
      ElseIf ( aOptions[ nChoice ] == "Default" )
         Set DELETED On
         Return ( .F. )
      End

   End

   PostQuitMessage( 0 )
   ErrorLevel( 1 )
   Quit
   //CLOSE ALL
   //PGMEXIT()

   Return ( .F. )


*-----------------------------------------------------------------------------*

Static Function ErrorMessage( e )

   Local cMessage

   // start error message
   cMessage := If( e:severity > ES_WARNING, "Error", "Warning" )

   // add error description if available
   If ( ValType( e:description ) == "C" )
      cMessage += ';' + e:description
   End

   // add either filename or operation
   If ( ! Empty( e:filename ) )
      cMessage += ( ';' + e:filename )
   ElseIf ( ! Empty( e:operation ) )
      cMessage += ( ';' + e:operation )
   End

   // add subsystem name if available
   If ( ValType( e:subsystem ) == "C" )
      cMessage += ';ERROR: ' + e:subsystem( ) + ' '
   Else
      cMessage += ";ERROR: ??? "
   End

   // add subsystem's error code if available
   If ( ValType( e:subCode ) == "N" )
      cMessage += ( NTRIM( e:subCode ) )
   End
   cMessage += ';Called from ' + procname( 3 ) + ' (' + AllTrim( str( procline( 3 ) ) ) + '),  ' + ;
   + procname( 4 ) + ' (' + AllTrim( str( procline( 4 ) ) ) + ')'
   cMessage += ';Error logged in file ERROR.LOG'

   Return ( cMessage )


*-----------------------------------------------------------------------------*

Static Function LogError( e, cProcStack )

   Local r, c
   Local h
   Local args := convertargs( e:args )
   Local i := 3
   Local cErr := ''
   Local dVer

   cErr += 'SYSTEM'
   cErr += ( CRLF + '------' )
   cErr += ( CRLF + 'Error date:' + dtoc( date( ) ) + ' time:' + time( ) )
   cErr += ( CRLF + 'Application: ' + GetModuleFileName( ) )
   cErr += ( CRLF + 'What32.Lib ver.' + WhatVersion( @dVer ) + ", " + DTOC( dVer ) )

   // T.B.D.:
   // add here  Windows version, memory info, diskspace info, free resources info
   // add computer name and operator name

   cErr += ( CRLF )
   cErr += ( CRLF + "ERROR INFORMATION" )
   cErr += ( CRLF + "-----------------" )
   cErr += ( CRLF + "Arguments     " + args )
   cErr += ( CRLF + "Description   " + e:description )
   cErr += ( CRLF + "Filename      " + IfEmpty( e:filename ) )
   cErr += ( CRLF + "GenCode       " + gencodetext( e:genCode ) )
   cErr += ( CRLF + "Operation     " + IfEmpty( e:operation ) )
   cErr += ( CRLF + "Severity      " + NTRIM( e:severity ) )
   cErr += ( CRLF + "SubCode       " + NTRIM( e:subCode ) )
   cErr += ( CRLF + "SubSystem     " + e:subSystem )
   cErr += ( CRLF + "Tries         " + NTRIM( e:tries ) )
   cErr += ( CRLF + "Alias()       " + IfEmpty( ALIAS( ) ) )
   cErr += ( CRLF + "Open DBFs     " + ntrim( GetAliasCount( ) ) )
   cErr += ( CRLF + "DOS Error     " + DosErrCode( e ) )
   cErr += ( CRLF + "Windows Error " + NTRIM( GetLastError( ) ) )
   cErr += ( CRLF )
   cErr += ( CRLF )
   cErr += ( CRLF + "PROCEDURE STACK" )
   cErr += ( CRLF + "---------------" )

   cErr += cProcStack

   Set PRINTER To error.LOG ADDITIVE
   Set console Off
   Set printer On


   ? '        Please mail or fax this error report to:'
  /*
  ? '             +---------------------------+'
  ? '             |  YOUR BUSINESS NAME HERE  |'
  ? '             |        P.O.Box 123        |'
  ? '             |Some Prestigeous Town, 1234|'
  ? '             |    Fax: (01) 1234 1234    |'
  */
   ? '             +---------------------------+'
   ? cErr

   ? replicate( "=", 70 )

   eject
   Set printer Off
   Set printer To
   Set console On

   Return cErr


*-----------------------------------------------------------------------------*

Static Function IfEmpty( msg )

   Local ret_val := "<none>"

   If ! Empty( msg )
      ret_val := Left( msg, 68 )
   EndIf

   Return ret_val


*-----------------------------------------------------------------------------*

Static Function PrintError

   Break

   Return ( .F. )


*-----------------------------------------------------------------------------*

Static Function convertargs( a )

   Local ret_val
   Local x, ctype
   local numargs:=IF(VALTYPE(a)=="A",Len(a),IF( VALTYPE(a)=="C",(a:={a},1),0))

   If numargs > 0
      ret_val := '{ '
      For x := 1 To numargs
         ctype := ValType( a[ x ] )
         Do Case
         Case ctype == "C"
            ret_val += a[ x ]
         Case ctype == "N"
            ret_val += NTRIM( a[ x ] )
         Case ctype == "D"
            ret_val += dtoc( a[ x ] )
         Case ctype == "L"
            ret_val += If( a[ x ] , ".T.", ".F." )
         Case ctype == "O"
            ret_val += a[ x ] :className + " Object"
         Case ctype == "U"
            ret_val += "NIL"
         EndCase
         //ÄÄÄÄÄ Next block added 1/8/92 To separate arguments
         If x < numargs
            ret_val += ', '
         EndIf
      Next
      ret_val += ' }'
   EndIf

   Return ifempty( ret_val )


*------------------------------------------------------------------------------*

Static Function GetAliasCount( )

   Local Counter := 0
   Local nCounter := 0

   For Counter := 1 To 255
      If ! Empty( alias( Counter ) )
         nCounter ++
      EndIf
   Next

   Return( nCounter )


*----------------------------------------------------------------------------*

Static Function getprocstack( )

   Local i := 2

   Do While ! Empty( procname( i ) )
      i ++
   EndDo

   Return( i - 3 )


*-----------------------------------------------------------------------------*

Static Function DosErrCode( e )

   Local msg

   If e:osCode > 0
      msg := NTRIM( e:osCode ) + ": " + Left( DosErrText( e:osCode ) , 37 )
   Else
      msg := "(not an operating system error)"
   EndIf

   Return msg


*-----------------------------------------------------------------------------*

/*
 Function: DosErrText( )
 Author: Craig Yellick
 Purpose: Provide full description of DOS error code ( see table D - 1
 in the Clipper 5.0 "Programming & Utilities Guide" )
 Returns: character string
*/
Static Function DosErrText( n )
   Local desc_ := { "Invalid function number", ; // 1
   "File not found", ; // 2
   "Path not found", ; // 3
   "Too many files open (no handles left)", ; // 4
   "Access denied", ; // 5
   "Invalid handle", ; // 6
   "Memory control blocks destroyed (oh, my)", ; // 7
   "Insufficient memory", ; // 8
   "Invalid memory block address", ; // 9
   "Invalid environment", ; // 10
   "Invalid format", ; // 11
   "Invalid access code", ; // 12
   "Invalid data", ; // 13
   , ; // 14
   "Invalid drive was specified", ; // 15
   "Attempt to remove the current directory", ; // 16
   "Not same device", ; // 17
   "No more files", ; // 18
   "Attempt to write on write-protected diskette", ; // 19
   "Unknown unit", ; // 20
   "Drive not ready", ; // 21
   "Unknown command", ; // 22
   "Data error (CRC)", ; // 23
   "Bad request structure length", ; // 24
   "Seek error", ; // 25
   "Unknown media type", ; // 26
   "Sector not found", ; // 27
   "Printer out of paper", ; // 28
   "Write fault", ; // 29
   "Read fault", ; // 30
   "General failure", ; // 31
   "Sharing violation", ; // 32
   "Lock violation", ; // 33
   "Invalid disk change", ; // 34
   "FCB unavailable", ; // 35
   "Sharing buffer overflow", ; // 36
   , , , , , , , , , , , , , ; // 37-49
   "Network request not supported", ; // 50
   "Remote computer not listening", ; // 51
   "Duplicate name on network", ; // 52
   "Network name not found", ; // 53
   "Network busy", ; // 54
   "Network device no longer exists", ; // 55
   "Network BIOS command limit exceeded", ; // 56
   "Network adapter hardware error", ; // 57
   "Incorrect response from network", ; // 58
   "Unexpected network error", ; // 59
   "Incompatible remote adapter", ; // 60
   "Print queue full", ; // 61
   "Not enough space for print file", ; // 62
   "Print file deleted (not enough space)", ; // 63
   "Network name deleted", ; // 64
   "Access denied", ; // 65
   "Network device type incorrect", ; // 66
   "Network name not found", ; // 67
   "Network name limit exceeded", ; // 68
   "Network BIOS session limit exceeded", ; // 69
   "Temporarily paused", ; // 70
   "Network request not accepted", ; // 71
   "Print or disk redirection paused", ; // 72
   , , , , , , , ; // 73-79
   "File already exists", ; // 80
   , ; // 81
   "Cannot make directory entry", ; // 82
   "Fail on INT 24h", ; // 83
   "Too many redirections", ; // 84
   "Duplicate redirection", ; // 85
   "Invalid password", ; // 86
   "Invalid parameter", ; // 87
   "Network device fault", ; // 88
   ;
   "Undefined or reserved error code!" } // +1
/*
 Check that code number is within known upper limit,
 AND that a description is available For it.
*/
   If ( n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   EndIf

   Return desc_[ n ]


*-----------------------------------------------------------------------------*

Static Function GenCodeText( n )

   Local desc_ := { "EG_ARG", ; // 1
   "EG_BOUND", ; // 2
   "EG_STROVERFLOW", ; // 3
   "EG_NUMOVERFLOW", ; // 4
   "EG_ZERODIV", ; // 5
   "EG_NUMERR", ; // 6
   "EG_SYNTAX", ; // 7
   "EG_COMPLEXITY", ; // 8
   , , ; // 9-10
   "EG_MEM", ; // 11
   "EG_NOFUNC", ; // 12
   "EG_NOMETHOD", ; // 13
   "EG_NOVAR", ; // 14
   "EG_NOALIAS", ; // 15
   "EG_NOVARMETHOD", ; // 16
   "EG_BADALIAS", ; // 17 (new w/ 5.01a)
   "EG_DUPALIAS", ; // 18 (new w/ 5.01a)
   , ; // 19
   "EG_CREATE", ; // 20
   "EG_OPEN", ; // 21
   "EG_CLOSE", ; // 22
   "EG_READ", ; // 23
   "EG_WRITE", ; // 24
   "EG_PRINT", ; // 25
   , , , , ; // 26-29
   "EG_UNSUPPORTED", ; // 30
   "EG_LIMIT", ; // 31
   "EG_CORRUPTION", ; // 32
   "EG_DATATYPE", ; // 33
   "EG_DATAWIDTH", ; // 34
   "EG_NOTABLE", ; // 35
   "EG_NOORDER", ; // 36
   "EG_SHARED", ; // 37
   "EG_UNLOCKED", ; // 38
   "EG_READONLY", ; // 39
   "EG_APPENDLOCK", ; // 40
   ;
   "Unknown or reserved" } // +1
/*
 Check that code number is within known upper limit,
 AND that a description is available For it.
*/
   If ( n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   EndIf

   Return NTRIM( n ) + ": " + desc_[ n ]


*-----------------------------------------------------------------------------*

Static Function eAlert( cMsg, aChoices, cDetail )

   Local aDlg, i, j, n, aWid, aChoose, amSG
   Local hWnd, hDC
   Local lErr := .F., e, w , h, t := 0, cTitle, msgh, butwidth
   Local crpos := 0, txth := 0, atm := { }
   Local isDetail := .F.

   If ValType( cMsg ) != "C"
      cMsg = asString( cMsg )
   EndIf
   cTitle := 'Alert'

   If aChoices == NIL
      aChoices = { "&Ok" }
   EndIf

   aAdd( achoices, "&Details >>" )

   cMsg = StrTran( cMsg, ";", CR )

   If ( crpos := at( CR, cMsg ) ) > 0
      cTitle := Left( cMsg, crpos - 1 )
      cMsg := SubStr( cMsg, crpos + 1 )
   EndIf

   hWnd := GetDesktopWindow( ) // default parent
   hDC = GetDC( hWnd )

   //------------- total width without buttons

   w := GetTextExtentPoint32( hDC, AllTrim( cTitle ) ) [ 1 ]
   amSG := str2a( cMsg, CR )
   AEVAL( amSG, { | X, Y | w := Max( w, GetTextExtentPoint32( hDC, AllTrim( X ) ) [ 1 ] ) } )
   w += 20

   //--------- total width of choices, also add "&" to the choices (if needed)

   n := Len( aChoices )
   aChoose := array( n )

   txth := 8 //ATM[TM_Height]
   msgh := Len( amSG ) * txth
   For i = 1 To n
      butwidth := Max( 20, GetTextExtentpoint32( hDC, aChoices[ i ] ) [ 1 ] + 6 )
      t := Max( t, butwidth )
      aChoose[ i ] = If( at( "&", aChoices[ i ] ) == 0, "&" + aChoices[ i ] , aChoices[ i ] )
   Next i

   ReleaseDC( , hDC )

   butwidth := t / 2
   t *= ( n + 1 )
   w = Max( Max( w, t ) + 40, 500 ) // minimum dlg width
   h = msgh + 33
   w /= 2

   //----------- create dialog

   aDlg = MakeDlgTemplate( cTitle, ;
                           WS_CAPTION + DS_MODALFRAME + WS_VISIBLE + 4 + WS_POPUP + DS_SETFONT , ;
                           0, 0, w, h, 8, 'MS Sans Serif' )

   For i = 1 To n
      aDlg = AddDlgItem( aDlg, i, "BUTTON", ;
                         BS_PUSHBUTTON + WS_TABSTOP + WS_CHILD + WS_VISIBLE, ;
                         w - ( n - i ) * ( butwidth + 5 ) - butwidth - 5, h - 18, butwidth, 14, ;
                         aChoose[ i ] )
   Next i

   aDlg = AddDlgItem( aDlg, - 1, "STATIC", ;
                      SS_CENTER + WS_CHILD + WS_VISIBLE, ;
                      10, 8, w - 20, msgh, ;
                      cMsg )

   aDlg = AddDlgItem( aDlg, - 1, "BUTTON", ;
                      BS_GROUPBOX + WS_CHILD + WS_VISIBLE, ;
                      5, 1, w - 10, msgh + 10, ;
                      "" )

   aDlg = AddDlgItem( aDlg, 101, "EDIT", ;
                      WS_CHILD + WS_VISIBLE + WS_BORDER + ES_MULTILINE + ES_READONLY + WS_VSCROLL + WS_TABSTOP, ;
                      5, h + 1, w - 10, 115, ;
                      cDetail )

   MessageBeep( MB_ICONHAND )

   i = DialogBox( , aDlg, hWnd, { | hDlg, nMsg, nwParam, nlParam | ;
                                  eAlertProc( hDlg, nMsg, nwParam, nlParam, @isDetail, hWnd, n ) } )

   SetFocus( hWnd )

   Return i


*----------------------------------------------------------------------------*

Static Function eAlertProc( hDlg, nMsg, nwParam, nlParam, isDetail, hWnd, n )

   Local arect

   Do Case
   Case nMsg == WM_INITDIALOG
      CenterWindow( hDlg, , , hWnd )
      SetOnTop( hDlg, HWND_TOPMOST )
      Return( 1 )

   Case nMsg == WM_COMMAND
      If 'Detail' $ GetDlgItemText( hDlg, nwParam )
         arect := getwindowrect( hDlg )
         If isDetail
            SetDlgItemText( hDlg, nwParam, '&Detail >>' )
            MoveWindow( hDlg, arect[ 1 ] , arect[ 2 ] , arect[ 3 ] - arect[ 1 ] , arect[ 4 ] - arect[ 2 ] - 200, .T. )
            isDetail := .F.
         Else
            SetDlgItemText( hDlg, nwParam, '<< &Detail' )
            MoveWindow( hDlg, arect[ 1 ] , arect[ 2 ] , arect[ 3 ] - arect[ 1 ] , arect[ 4 ] - arect[ 2 ] + 200, .T. )
            isDetail := .T.
         EndIf
      Else
         If nwParam > 0 .AND. nwParam < n
            EndDialog( hDlg, nwParam )
         EndIf
      EndIf

   EndCase

   Return( 0 )



// eof.
