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
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

#include "getexit.ch"
#include "inkey.ch"

#define	NEW_STR(c)	((c) + "")

// Windows definitions
CLASS WG_TXGet FROM Get
    // Base

    DATA cExtendedType   AS STRING
    DATA bDisplay        AS CODEBLOCK
    DATA bMeaning        AS CODEBLOCK
    DATA bHelp           AS CODEBLOCK
    DATA bAction         AS CODEBLOCK
    DATA cMessage        AS STRING
    DATA oField          AS OBJECT
    DATA aData           AS ARRAY
    DATA cDefault        AS STRING
    DATA cBackground     AS STRING
    DATA nKey            AS NUMERIC
    DATA cLetter         AS STRING
    DATA nLetterPos      AS NUMERIC

    METHOD  New()         CONSTRUCTOR

    METHOD  ListRead()
    METHOD  GetKey()
    METHOD  Pre()
    METHOD  Post()
    METHOD  ValidateGet()
    METHOD  Help()
    METHOD  Validate()
    METHOD  FindKey()
    METHOD  Full()


ENDCLASS

//---------------------------------------------------------------------------//

METHOD New( nRow, nCol, bVar, cVar, cPicture, ;
            nWidth, nHeight, oParent, cToolTip,;
            cStatusMsg, lPixel, nID, xDef, nLimit, cMsg, bHelp, bMeaning, bDisplay, ;
            nKey, cLetter, bExit, ;
            oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

    //::Super:New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )
    //::Super:New( cVarName,, nRow, nCol, nWidth, nHeight, oParent, , cToolTip, cStatusBar, lPixel, nID, cValue, nLimitText, lReadOnly, lPassword )

   LOCAL xg
   LOCAL f, bBlock, bPost
   LOCAL nPos

   //DEFAULT ncFgColor TO "N"  // FSG - ColorGetData()
   //DEFAULT ncBgColor TO "W+" // FSG - ColorGetData()
   DEFAULT bMeaning TO {|| NIL }
   DEFAULT bDisplay TO {|g| g:Display() }
   IF ValType( bVar ) == "B"
      bBlock := bVar
   //ELSE
   //   f:= bVar
   //   bBlock := FldGetSet( f )
   //   cVar   := FldName( f )
   //   cPic   := PictMerge( cPic, FldPicture( f ) )
   //   bPost  := {|fg| fg:Validate() }
   ENDIF
   //xg := ClassExtend( GetNew( nRow, nCol, bBlock, cVar, cPic, cColor ) )
   ::Super:New( nRow, nCol, bVar, cVar, cPicture) //, ncFgColor )
   IF bExit == NIL
      ::postBlock := bPost
   ELSE
      ::postBlock := {|g| Eval( bExit, g ), Eval( bPost, g ) }
   ENDIF
   ::cMessage    := cMsg
   ::bHelp       := bHelp
   ::bMeaning    := bMeaning
   ::oField      := f
   ::bDisplay    := bDisplay
   ::cBackground := ncBgColor
   ::nKey        := nKey
   ::cLetter     := cLetter
   IF cLetter != NIL THEN ::nLetterPos := AT( cLetter, ::varGet() )

RETURN Self

//---------------------------------------------------------------------------//

METHOD ListRead( aGetList, kl )
   LOCAL lExit := FALSE
   LOCAL nPos  := 0
   LOCAL nLastExit
   LOCAL lUpdated := FALSE
   // Requires GETSYSPP.PRG
   //CursorPush()
   //DEFAULT kl TO StdGetKeyList( MaintActive() )

   ReadModal( aGetList, {| xg | xg:GetKey( kl ) },;
                        {| xg | xg:Pre() },;
                        {| xg | xg:Post() },;
                        @nPos, ;
                        @nLastExit, @lUpdated )

   //CursorPop()
   RETURN !( nLastExit == GE_ESCAPE )

METHOD GetKey( kl )
   LOCAL nKey := 0
   //WHILE TRUE
   //   nKey := GetKey( 0 )                       // Get the keystroke
   //   EXIT IF kl == NIL                         // No active keylist
   //   EXIT IF kl:ListGo( nKey, Self ) == NIL      // Key wasn't processed
   //END
   RETURN nkey

METHOD Pre()
   //HelpF2Push( {|| ::Help() } )
   //InfoMark()
   //FYIPush( ::cMessage )
   RETURN Self

METHOD Post()
   //LOCAL nGet, i := 1
   //LOCAL nPos:= ReadPos()
   //FYIPop()
   //InfoRelease()
   //HelpF2Pop()
   //CondEval( ::bMeaning, Self )
   //IF ::exitState == GE_WRITE .OR.;
   //   ( ::exitState == GE_ENTER .OR. ::exitState == GE_WHEN ) .AND.;
   //     nPos == Len( ReadGetList() )
   //   // Validate all gets
   //   CursorPush( SC_NONE )
   //   ReadValidate( TRUE )
   //   nGet := AScan( ReadGetList(), {|xg| !xg:ValidateGet( i++ ) } )
   //   ReadValidate( FALSE )
   //   CursorPop()
   //   IF nGet == 0
   //      ReadPos( nPos )           // Reset
   //      ::exitState := GE_WRITE  // Allow exit
   //   ELSE
   //      ReadPos( nGet )           // Set to bad get
   //      ::exitState := GE_NOEXIT // No Exit
   //   ENDIF
   //ENDIF
   RETURN Self

// To be used by XGetPost()
// Sets environment stuff so a get can be validated
METHOD ValidateGet( nGet )
   LOCAL lRet := TRUE
   //DEFAULT nGet TO ReadPos()
   GetActive( Self )
   //ReadPos( nGet )
   IF ::preBlock == NIL
      IF ::postBlock != NIL
         lRet := Eval( ::postBlock, Self )
         Eval( ::bDisplay, Self )
      ENDIF
   ELSE
      IF Eval( ::preBlock, Self )
         IF ::postBlock != NIL
            lRet := Eval( ::postBlock, Self )
            Eval( ::bDisplay, Self )
         ENDIF
      ENDIF
   ENDIF
   RETURN lRet

METHOD Help()
   LOCAL lHelp := TRUE
   LOCAL f:= ::oField
   //FYIPush( "Attendere ... Richiamo aiuto del campo" )
   DO CASE
      //CASE f != NIL .AND. ValType( FldHelpBlk( f ) ) == "B"
      //   Eval( FldHelpBlk( f ), Self )
      CASE ValType( ::bHelpBlock ) == "B"
         Eval( ::bHelpBlock, Self )
      CASE ::type == "D"
         //::varPut( CalHelp( CToD( if( ::buffer == NIL, ::varGet(), ::buffer ) ) ) )
         ::updateBuffer()
         IF LastKey() == K_ENTER
            KEYBOARD Chr( K_ENTER )
         ENDIF
      OTHERWISE
         lHelp := FALSE
   ENDCASE
   //FYIPop()
   RETURN lHelp

METHOD Validate()
   LOCAL lOk := TRUE, cErrMsg := "Valore immesso non valido."
   LOCAL f, lHelp, lCallHelp := TRUE
   IF ( f:= ::oField ) <> NIL
      DO WHILE TRUE
         lOk := f:Validate( ::varGet(), @cErrMsg, @lCallHelp )
         IF !lOk .AND. ::HasFocus .AND. lCallHelp
            lHelp := ::Help()
            IF lHelp .AND. !Esc()
               KEYBOARD ""  // Svuoto il buffer dagli ultimi tasti premuti
               LOOP
            ENDIF
         ENDIF
         EXIT
      ENDDO
   ELSE
      lOk := ::ValidateGet()
   ENDIF
   //IF !lOk .AND. !Empty( cErrMsg )
   //   IF ::hasFocus .AND. InfoActive()
   //      InfoMsg( cErrMsg, BOX_CENTER )
   //   ELSE
   //      MsgBox( cErrMsg, 0, BOX_CENTER )
   //   ENDIF
   //ENDIF
   RETURN lOk

METHOD FindKey( nKey, GetList )
   DEFAULT GetList TO __GetListActive()// ReadGetList()
   RETURN AScan( GetList, {|g| g:nKey == nKey } )

METHOD Full()
   DispBegin()
   Eval( ::bDisplay, Self )
   CondEval( ::bMeaning, Self )
   DispEnd()
   RETURN Self

//---------------------------------------------------------------------------//

// EOF

