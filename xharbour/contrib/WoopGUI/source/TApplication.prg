/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
* PRG application CLASS & start functions
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"

#define APPNAME "Applicazione di prova"

STATIC oMyApplication

STATIC shInstance

// Environment application definitions
CLASS TApplication FROM TObject
    // Base
    PROTECTED:
    DATA cId         AS CHAR
    DATA cName       AS CHAR                 // Application name
    DATA nHandle     INIT GetModuleHandle()  // Application Handle. Must be used from all application abjects.

    DATA cIcon       AS CHAR                 // Application Icon
    DATA cCursor     AS CHAR                 // Application Handle to the class cursor. This member must be a handle to a cursor resource.
    DATA nBkColor    AS NUMERIC              // Application default Background color
    DATA cIconSm     AS CHAR                 // Application small icon

    DATA oFrmClass   AS OBJECT               // Window default class
    DATA oDlgClass   AS OBJECT               // Dialog default class

    DATA bInit       AS CODEBLOCK            // Code block to evaluate on Init
    DATA bQuit       AS CODEBLOCK            // Code block to evaluate on quitting

    //DATA aoWinClass   INIT {}  HIDDEN        // Array of application windows class definition
    DATA oCurrentWindow        HIDDEN        // Current Window

    DATA aLog          INIT {}            HIDDEN
    DATA lLogWrite     INIT FALSE         HIDDEN
    DATA lLogAppend INIT FALSE            HIDDEN
    DATA cLogFileName  INIT "events.log"  HIDDEN
    DATA oLogWindow    AS OBJECT          HIDDEN
    // Variabile per tavole
    // CLASSDATA Tables
    // Variabile per maint
    // CLASSDATA Maints

    // METODI
    METHOD New() CONSTRUCTOR
    METHOD Activate()                INLINE ::MainLoop(), ::OnQuit()
    METHOD Create()
    METHOD MainLoop()
    //METHOD  FindWinClassByName()
    METHOD OnInit()
    METHOD OnQuit()

    METHOD DisableLogWrite()         INLINE ::EnableLogWrite( FALSE )
    METHOD EnableLogWrite( lEnable ) INLINE IIF( lEnable == NIL, lEnable := TRUE, NIL ),;
                                             ::lLogWrite := lEnable
    METHOD EventsWrite()
    //METHOD  GetCurrentWindow()
    METHOD GetLogWindow()            INLINE ::oLogWindow
    METHOD HasLogWindow()            INLINE ::oLogWindow <> NIL
    METHOD LogWrite()
    METHOD MenuEventsWrite()
    //METHOD  SetCurrentWindow()
    METHOD SetLogWindow()
    METHOD SetLogWrite()
    //METHOD  UnsetCurrentWindow()

    // AddWindow viene usato direttamente dalla classe Window
    HIDDEN:
    //METHOD  AddWinClass() BLOCK {|Self, oWnd| aAdd( ::aoWinClass, oWnd ) }
    //METHOD  UnregisterWinClasses()

    //ON ERROR ErrorHandler()

ENDCLASS

FUNCTION WG_ApplObj()
RETURN oMyApplication

FUNCTION WG_ApplHandle()
RETURN oMyApplication:nHandle

METHOD New( cId, cName, cIcon, cCursor, nBkColor, cIconSm, bInit, bQuit ) CLASS TApplication
    LOCAL oWinDef
    LOCAL n, pIcon

    IF oMyApplication == NIL

       ASSIGN ::cId        WITH cId        DEFAULT "WoopGUI"
       ASSIGN ::cName      WITH cName      DEFAULT "Applicazione di prova"
       ASSIGN ::cIcon      WITH cIcon
       ASSIGN ::cCursor    WITH cCursor
       ASSIGN ::nBkColor   WITH nBkColor
       ASSIGN ::cIconSm    WITH cIconSm    DEFAULT ::cIcon
       ASSIGN ::bInit      WITH bInit
       ASSIGN ::bQuit      WITH bQuit

       WG_MyErrorSys()

       // Definisco l'ambiente e attivo la finestra principale
       //::aoWinClass := {}
       //::aoWindows  := {}

       oMyApplication := Self

       // Inizializzo i controlli
       InitCommonControlsEx()
    ELSE
       Self := oMyApplication
    ENDIF

    ::OnInit()

RETURN Self

METHOD Create() CLASS TApplication
   LOCAL oWinDef

      // Define Standard Application Class Data
      oWinDef := TWindowDef():New( ,, ::nHandle )
      oWinDef:cClassName := "WoopGUIFrmClass"

      IF ::cIcon   <> NIL THEN oWinDef:SetIconFromFile( ::cIcon )
      IF ::cCursor <> NIL THEN oWinDef:SetCursorFromFile( ::cCursor )
      IF ::cIconSm <> NIL
         IF ::cIconSm == ::cIcon
            oWinDef:hIconSm := oWinDef:hIcon
         ELSE
            oWinDef:SetIconSmFromFile( ::cIcon )
         ENDIF
      ENDIF

RETURN Self

METHOD MainLoop() CLASS TApplication
   LOCAL cMsg

   DO WHILE GetMessage( @cMsg, 0, 0, 0 )
      IF !IsDialogMessage( , cMsg )
         TranslateMessage( cMsg )
         DispatchMessage( cMsg )
      ENDIF
   ENDDO

RETURN 0

Function isDialogMessage( hDlg, cMsg )

   If hDlg == NIL
     Return ( aScan( WG_GetDialogArray(), {|h| _isDialogMessage( h, cMsg ) } ) > 0 )
   Endif

Return _isDialogMessage( hDlg, cMsg )


/*
METHOD FindWinClassByName( cClassName ) CLASS WG_TApplication
   LOCAL oWin
   LOCAL nPos
   cClassName := Upper( cClassName )
   nPos := aScan( ::aoWinClass, {|e| Upper( e:cClassName ) == cClassName } )
   IF nPos > 0 THEN oWin := ::aoWinClass[ nPos ]
RETURN oWin
*/

//METHOD GetCurrentWindow() CLASS WG_TApplication
//  IF ValType( ::oCurrentWindow ) <> "O" THEN MessageBox(0,"Default Windows Object not defined!","Error")
//RETURN ::oCurrentWindow
//
//METHOD SetCurrentWindow( oWnd ) CLASS WG_TApplication
//  LOCAL nOldWnd := ::oCurrentWindow
//  ::oCurrentWindow := oWnd
//RETURN nOldWnd

METHOD SetLogWrite( lWrite AS LOGICAL, cFileName AS STRING, lAppend AS LOGICAL, oWnd AS OBJECT ) CLASS TApplication
  ASSIGN ::lLogWrite     WITH lWrite
  ASSIGN ::cLogFileName  WITH cFileName DEFAULT "events.log"
  ASSIGN ::lLogAppend    WITH lAppend   DEFAULT FALSE
  ASSIGN ::oLogWindow    WITH oWnd
RETURN NIL

METHOD SetLogWindow( oWnd AS OBJECT ) CLASS TApplication
  LOCAL oOldWnd := ::oLogWindow
  ::oLogWindow := oWnd
RETURN oOldWnd

METHOD OnInit() CLASS TApplication
  WG_DebugTrace( "TApplication:OnInit()", "Self", Self )
  IF ValType( ::bInit ) == "B"
     Eval( ::bInit )
  ENDIF
RETURN Self

METHOD OnQuit() CLASS TApplication
  WG_DebugTrace( "TApplication:OnQuit()", "Self", Self )
  IF ValType( ::bQuit ) == "B"
     Eval( ::bQuit )
  ENDIF
  //IF !Empty( WG_TWindow():GetWindows() )
  //   AEval( WG_TWindow():GetWindows(), {|o| o:Destroy() } )
  //ENDIF
RETURN NIL

//METHOD UnsetCurrentWindow( oWnd ) CLASS WG_TApplication
//  ::oCurrentWindow := NIL
//RETURN NIL


METHOD EventsWrite( cFrom, hWnd, cnMsg, wParam, lParam ) CLASS TApplication
    LOCAL oWinF := TWindow():FindWindowByHandle( hWnd )
    LOCAL cMsg, nMsg
    STATIC lHeader := TRUE

    DEFAULT cFrom TO ""

    IF ValType( cnMsg ) == "N"
       cMsg := WG_DecodeMessageName( cnMsg )
       nMsg := cnMsg
    ELSE
       cMsg := cnMsg
       nMsg := 0
    ENDIF


    IF !::HasLogWindow() .OR. ;
       ( ::HasLogWindow() .AND. ;
         hWnd <> ::oLogWindow:nHandle .AND. ;
         ( ::oLogWindow:HasParent() .AND. hWnd <> ::oLogWindow:oParent:nHandle ) ;
       )

       IF lHeader
          //MessageBox(,"Passato da Header")
          ::LogWrite( "From| Win Handle |  Classname  |        Name        |Msg #|   Message Decode   | wParam      [ HIGH WORD  ][  LOW WORD  ]| lParam      [ HIGH WORD  ][  LOW WORD  ]" )
          ::LogWrite( "--- |------------|-------------|--------------------|-----|--------------------|-----------------------------------------|-----------------------------------------" )

          lHeader := FALSE
       ENDIF
       IF oWinF <> NIL
          ::LogWrite( PadR(cFrom,4)+"|"+Str(hWnd,12)+"|"+PadR(oWinF:ClassName,13)+"|"+PadR(oWinF:cName,20)+"|"+Str(nMsg,5)+"|"+Pad(cMsg,20)+;
                      "|"+Str(wParam,12)+" ["+PadL(cStr(HIWORD(wParam)),12)+"]["+PadL(cStr(LOWORD(wParam)),12)+"]"+;
                      "|"+Str(lParam,12)+" ["+PadL(cStr(HIWORD(lParam)),12)+"]["+PadL(cStr(LOWORD(lParam)),12)+"]";
                    )
       ELSE
          ::LogWrite( PadR(cFrom,4)+"|"+Str(hWnd,12)+"|"+PadR("no class win-",13)+"|"+PadR("---------",20)+"|"+Str(nMsg,5)+"|"+Pad( cMsg,20)+;
                      "|"+Str(wParam,12)+" ["+PadL(cStr(HIWORD(wParam)),12)+"]["+PadL(cStr(LOWORD(wParam)),12)+"]"+;
                      "|"+Str(lParam,12)+" ["+PadL(cStr(HIWORD(lParam)),12)+"]["+PadL(cStr(LOWORD(lParam)),12)+"]";
                    )
       ENDIF

    ENDIF

RETURN NIL

METHOD MenuEventsWrite( oItem AS OBJECT ) CLASS TApplication
    // STATIC lHeader

    // IF lHeader == NIL
    //    ::LogWrite( " Win Handle |  Classname  |        Name        |       Message      | wParam      [ HIGH WORD  ][  LOW WORD  ]| lParam      [ HIGH WORD  ][  LOW WORD  ]" )
    //    ::LogWrite( "------------|-------------|--------------------|--------------------|-----------------------------------------|-----------------------------------------" )
    //
    //    lHeader := FALSE
    // ENDIF
    IF ValType( oItem ) == "O"
       ::LogWrite( "Called Menu Item Command: '" + ;
                    oItem:GetLabel() + ;
                    "' - " + IIF( oItem:HasAction(), "HAS Action", "NO Action" ) )
    ENDIF

RETURN NIL

METHOD LogWrite( cText AS STRING, fname AS STRING) CLASS TApplication
  LOCAL nHand
  LOCAL n

  IF cText <> NIL .AND. Len( cText ) > 0

     IF ::lLogWrite

        IF fname <> NIL THEN ::cLogFileName := fname
        fname := ::cLogFileName

        if !File( fname ) .OR. !::lLogAppend
           nHand := Fcreate( fname )
           ::lLogAppend := TRUE
        else
           nHand := Fopen( fname,1 )
        endif
        Fseek( nHand,0,2 )

        IF !Empty( ::aLog )
           // flush the log array
           FOR n := 1 TO Len( ::aLog )
               Fwrite( nHand, ::aLog[n] + CRLF )
           NEXT
           ::aLog := {}
        ENDIF
        Fwrite( nHand, cText + CRLF )


        Fclose( nHand )

        IF ::HasLogWindow() THEN ;
              ::oLogWindow:AppendText( cText + CRLF )

     ELSE
        // Push in an array waiting permission to write
        // Add control to check array overflow if there are no log write
        IF Len( ::aLog ) < 3000
           aAdd( ::aLog, cText )
        ENDIF

     ENDIF
  ENDIF

RETURN NIL

FUNCTION WG_EventToLog(hWnd, nMsg, wParam, lParam )

   WG_ApplObj():LogWrite( "ERRORE ------------------------------" )
   WG_ApplObj():EventsWrite( hWnd, nMsg, wParam, lParam )

RETURN NIL

/*
METHOD ErrorHandler( x ) CLASS WG_TApplication
   // Gestione errori della classe

   if PCount() > 0
      W_MsgBox( x, "Errore" )
   endif

   MsgBox( __GetMessage(), "Errore" )  // Shows the message that was sent to the object

RETURN
*/
// ----------------------------------------------------
//
// Funzioni di servizio
//
// ----------------------------------------------------

#include "error.ch"
// Called directly from C
FUNCTION WG_GenMyError( cErrore )
   LOCAL oErr
   // Qui la gestione errori
   //MessageBox( , "Genero file di log (parametri"+str(PCOUNT())+")" )
   oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_UNSUPPORTED
      oErr:subSystem   := "WOOPGUI"
      oErr:SubCode     := 0
      oErr:Description := cErrore
      Eval( ErrorBlock(), oErr )

RETURN TRUE

// WG_ParamDisplay( Self, hb_aparams(), "TWindow" ) --> 0

FUNCTION WG_ParamDisplay( oB, aParam, cWhere )
   local cString := "", i, aData

      DEFAULT cWhere TO ""

      IF ValType( oB ) == "O" THEN cString += "Object Name: " + oB:ClassName + CRLF

      aData := aParam
      FOR i = 1 to len ( aData )
          cString += "Param: " + str(i,2)
          cString += " - Type: " + ValType( aData[i] )
          cString += " - value= " + cStr( aData[ i ] )
          cString += CRLF
      NEXT

   MessageBox(, cString, "Parameters Data "+cWhere )

RETURN 0

FUNCTION WG_DisplayData( oObj AS OBJECT, cText AS STRING, lMessageBox AS LOGICAL )
   local cString := "", i, aData
   local oB := oObj

      DEFAULT cText       TO ""
      DEFAULT lMessageBox TO TRUE

      cString += "Object Name: " + oB:ClassName + CRLF

      aData := __objGetValueList( oB )
      FOR i = 1 to len ( aData )
          cString += "DATA name: " + Pad( aData[ i, HB_OO_DATA_SYMBOL ], 25 )
          cString += " - type: " + ValType( aData[ i, HB_OO_DATA_VALUE  ] )
          cString += " - value: " + Pad( cStr( aData[ i, HB_OO_DATA_VALUE  ] ), 30 )
          cString += CRLF
      NEXT

   IF lMessageBox
      MessageBox(, cString, "Object Data" + ;
                            IIF( Len( cText ) > 0, " - " + cText, "" ) )
   ENDIF

RETURN cString

FUNCTION WG_DebugTrace( cString, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20,;
                                 v21, v22, v23, v24, v25, v26, v27, v28, v29, v30 )

  LOCAL cDebugString := cString, n
  LOCAL nCount := PCOUNT()

  IF nCount > 1
     FOR n := 2 TO nCount
         if ( n % 2 ) == 0
            //if ( hb_PValue( n + 1 ) <> NIL )
               cDebugString += " " + cStr( hb_PValue( n ), TRUE ) + " ="
            //else
            //   cDebugString += " " + cStr( hb_PValue( n ), TRUE )
            //endif
         else
            cDebugString += " " + cStr( hb_PValue( n ), TRUE )
         endif
     NEXT
  ENDIF


  WG_ApplObj():LogWrite( PadR( PROCNAME(1) + "(" + cStr( PROCLINE(1), TRUE ) + ")", 40 ) + " - " + cDebugString )


RETURN NIL

