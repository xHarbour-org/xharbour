*****************************************************
* HB I18N dictionary editor
*
* $Id: hbdict.prg,v 1.6 2003/09/05 01:15:34 jonnymind Exp $
*
* Usage: hbdict <infile> <outfile>
*
* The outfile must be a HIT table file, while the input
* can be an HIL or a HIT.
* Input and output can be the same; in this case, the
* target dictionary will be overwritten on program
* exit.
*
* NOTICE: this program is currently a WIP and is
* being developed for testing purposes. This notice
* will be removed when the program will begin to assume
* more functionality.
*
#include "inkey.ch"
#include "hbclass.ch"


******************************************************
* Main Program
*

PROCEDURE Main( cInput, cOutput )
   LOCAL aInput, aOutput
   LOCAL recpos
   LOCAL nKey
   LOCAL lModified := .F.

   Save Screen
   SET COLOR TO W+/B
   CLEAR SCREEN

   @0,18 SAY i18n( "X H A R B O U R - Dictionary Editor (preview,2nd)" )

   DisplayMask()

   IF cInput == NIL .or. cOutput == NIL
      Popup( i18n( "Incorrect format" ) )
      @12,12 say i18n( "Usage:" )  + " hbdict <input> <output>"
      DoQuit()
   ENDIF

   aInput := HB_I18nLoadTable( cInput )

   IF aInput == NIL
      Popup( i18n( "Error in loading file" ) )
      @12,12 SAY i18n( "Can't load file " ) + cInput
      DoQuit()
   ENDIF

   // now we must merge duplicated strings
   MergeDuplicates( aInput[2] )
   // Sorting records
   IF SubStr( aInput[1][1], 2 ) == "HIL"
      ASort( aInput[2],,,{| x, y | AsciiOrder(x[1], y[1]) < 0} )
   ENDIF

   //then we try to load the output table
   if cInput != cOutput
      aOutput := HB_I18nLoadTable( cOutput )
      // If we exists, we merge it's content in the input
      IF aOutput != NIL
         MergeTables( aInput[2], aOutput[2] )
         // also, get header from output
         IF aInput[1][1] == chr(3) + "HIL"
            aInput[1] := aOutput[1]
            aInput[1][1] := chr(3) + "HIL"
         ENDIF
      ENDIF
   ENDIF


   // Informing user
   ShowHeader( aInput[1] )

   @6,3 SAY i18n("Saving to: ") + cOutput
   @3,50 SAY i18n("Press 'S' to save")
   @4,50 SAY i18n("Press 'E' to edit header")
   recpos := 1

   DO WHILE .T.
      DisplayMask()
      DrawPos( recpos, aInput[2] )
      ShowRecord( recpos, aInput[2] )
      nKey := Inkey(0)
      IF nKey == K_ESC
         IF lModified
            IF( Alert( i18n( "Dictionary modified ! Do you really want to exit?" ), { i18n( "Yes" ), i18n( "No" ) } ) == 1 )
            EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDIF

      SWITCH nKey

         CASE K_LEFT
         CASE K_UP
            IF recpos > 1
               recpos--
            ENDIF
         EXIT

         CASE K_CTRL_LEFT
         CASE K_CTRL_UP
            IF recpos > 10
               recpos-= 10
            ELSE
               recpos := 1
            ENDIF
         EXIT

         CASE K_ALT_LEFT
         CASE K_ALT_UP
            IF recpos > 50
               recpos-= 50
            ELSE
               recpos := 1
            ENDIF
         EXIT

         CASE K_RIGHT
         CASE K_DOWN
            IF recpos < Len ( aInput[2] )
               recpos++
            ENDIF
         EXIT

         CASE K_CTRL_RIGHT
         CASE K_CTRL_DOWN
            IF recpos + 10 < Len ( aInput[2] )
               recpos += 10
            ELSE
               recpos := Len( aInput[2] )
            ENDIF
         EXIT

         CASE K_ALT_RIGHT
         CASE K_ALT_DOWN
            IF recpos + 50 < Len ( aInput[2] )
               recpos += 50
            ELSE
               recpos := Len( aInput[2] )
            ENDIF
         EXIT

         CASE K_END
            recpos := Len( aInput[2] )
         EXIT

         CASE K_HOME
            recpos := 1
         EXIT

         CASE K_ENTER
            EditEntry( recpos, aInput[2] )
            lModified := .T.
         EXIT

         DEFAULT
            IF Upper( Chr( nKey ) ) == 'S'
               IF SaveTable( cOutput, aInput )
                  lModified := .F.
               ENDIF
            ELSEIF Upper( Chr( nKey ) ) == 'E'
               ReadHeader( aInput[1] )
               lModified := .T.
            ENDIF

      END

   ENDDO

   Restore screen
RETURN

PROCEDURE DoQuit()
   @24, 20 say i18n( "Program terminated. Press any key" )
   Inkey( 0 )
   CLEAR SCREEN
   QUIT
RETURN


PROCEDURE DisplayMask()
   MakeBox( 8,4, 13,76 )
   MakeBox( 15,4, 21,76 )
   @8,8 SAY " " + i18n("International string") + " "
   @15,8 SAY " " + i18n("Local language string") + " "
   @22,12 SAY i18n("Press ENTER to edit entry and then CTRL+W to save it")
   @23,12 SAY i18n("While editing, press F2 for 'autopadding' feature")
RETURN


PROCEDURE MakeBox( nRow, nCol, nRowTo, nColTo )
   @nRow, nCol, nRowTo, nColTo ;
      BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) +;
      Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
RETURN

PROCEDURE MakeBoxShadow( nRow, nCol, nRowTo, nColTo )
   SET COLOR TO W+/N
   @nRow+1,nCol +1 clear to nRowTo, nColTo
   SET COLOR TO W+/b
   @nRow, nCol clear to nRowTo-1, nColTo -1
   MakeBox( nRow, nCol, nRowTo-1, nColTo -1 )
RETURN

PROCEDURE PopUp( cMessage )
   MakeBoxShadow( 10,10, 16, 71 )
   @10,20 SAY " " +cMessage + " "
RETURN

PROCEDURE DrawPos( recpos, aTable )
   @7,60 SAY AllTrim( Str( recpos ) ) +" "+ I18n("of") + " " +  ;
      AllTrim( Str( Len(aTable ) ) )+"     "
RETURN

PROCEDURE ShowRecord( nPos, aTable )
   LOCAL cInter := aTable[nPos][1]
   LOCAL cLocal := aTable[nPos][2]

   IF cInter != NIL
      @9,5 SAY ">>"+cInter+"<<"
   ELSE
      @9,7 SAY i18n( "<Nothing>" )
   ENDIF

   IF cLocal != NIL
      @16,5 SAY ">>"+cLocal+"<<"
   ELSE
      @16,7 SAY i18n( "<Not Yet translated>" )
   ENDIF

RETURN


PROCEDURE EditEntry( nPos, aTable )
   LOCAL cInput := aTable[nPos][2]
   LOCAL oEditor

   IF cInput == NIL
      cInput := ""
   ENDIF

   oEditor := THBDictEdit():New( cInput, 16, 7, 20,73, aTable[ nPos ][1] )

   oEditor:Edit()
   IF oEditor:lSaved
      cInput := oEditor:GetText()
      IF Len( cInput ) > 0
         aTable[nPos][2] := cInput
      ENDIF
   ENDIF

RETURN

FUNCTION SaveTable( cFileName, aTable )
   LOCAL aHeader
   LOCAL lOk := .F.

   Popup( i18n( "Saving file" ) )
   @12,12 SAY i18n( "Saving file to " ) + cFileName

   // Saving the new header
   aHeader := Aclone( aTable[1] )
   // I can only save HIT types
   aHeader[1] := Chr( 3 ) + "HIT"
   aHeader[6] := Len( aTable[2] )

   ASort( aTable[2],,,{| x, y | AsciiOrder(x[1], y[1]) < 0} )
   IF HB_I18nSaveTable( cFileName, aHeader, aTable[2] )
      Popup( i18n( "Success" ) )
      @12,12 SAY i18n( "The file has been saved" )
      lOk := .T.
   ELSE
      Popup( i18n( "Failure" ) )
      @12,12 SAY i18n( "The File has not been saved" )
   ENDIF

   @13,13 SAY i18n( "(Press any key)" )
   Inkey(0)
   @10,10 clear to 16, 71
RETURN lOk

PROCEDURE ReadHeader( aHeader )
   LOCAL GetList := {}
   LOCAL cAuthor, cLanguage, cCode

   SAVE SCREEN
   cAuthor   := padr( aHeader[2], 40 )
   cLanguage := padr( aHeader[3], 40 )
   cCode := padr( aHeader[5], 5 )

   MakeBoxShadow( 10,10, 15, 70 )
   SET CONFIRM ON
   @10,14 SAY i18n("Change language header")
   @11,12 SAY i18n("Author:    ") GET cAuthor
   @12,12 SAY i18n("Language:  ") GET cLanguage
   @13,12 SAY i18n("Code:      ") GET cCode
   READ

   IF LastKey() != K_ESC
      aHeader[2] := AllTrim( cAuthor )
      aHeader[3] := AllTrim( cLanguage )
      aHeader[5] := AllTrim( cCode )
   ENDIF

   RESTORE SCREEN
   ShowHeader( aHeader )
RETURN

PROCEDURE ShowHeader( aHeader )
   @2,3 SAY i18n("File Type: ") + SubStr( aHeader[1], 2 )
   @3,3 SAY i18n("Author:    ") + padr( aHeader[2], 37 )
   @4,3 SAY i18n("Language:  ") + padr( aHeader[3], 37 )
   @5,3 SAY i18n("Code:      ") + padr( aHeader[5], 37 )
RETURN

PROCEDURE MergeDuplicates( aTable )
   LOCAL nPos, nFound

   // very, very unefficient, but it works.
   // Suggestions are welcome
   nPos := 1

   DO WHILE nPos < Len( aTable )
      nFound := aScan( aTable, { | x | x[1] == aTable[ nPos ][1] }, nPos +1 )
      IF nFound > 0
         aDel( aTable, nFound )
         aSize( aTable, Len( aTable ) -1)
      ELSE
         nPos ++
      ENDIF
   ENDDO
RETURN


PROCEDURE MergeTables( aInTable, aOutTable )
   LOCAL aElem, nFound
   LOCAL cEntry

   FOR EACH aElem in aInTable
      cEntry := aElem[1]
      nFound := AScan( aOutTable, { |x| x[1] == cEntry } )
      IF nFound > 0
         aElem[2] := aOutTable[ nFound ][2]
      ENDIF
   NEXT

RETURN

***************************************************
* Sorting input
*
FUNCTION AsciiOrder( cStr1, cStr2 )
   LOCAL nPos := 1

   DO WHILE nPos <= Len( cStr1 ) .and. nPos <= Len( cStr2 )
      IF asc( cStr1[nPos] ) < asc( cStr2[ nPos ] )
         RETURN -1
      ELSEIF asc( cStr1[nPos] ) > asc( cStr2[ nPos ] )
         RETURN 1
      ELSE
         nPos ++
      ENDIF
   ENDDO

   IF Len( cStr1 ) < Len( cStr2 )
      RETURN -1
   ELSEIF Len( cStr1 ) > Len( cStr2 )
      RETURN 1
   ENDIF

RETURN 0



***************************************************
* redefining the keyboard hook of xharbour teditor
*

CLASS THBdictEdit FROM HBEditor
   DATA cOrigin

   METHOD New( cString, nTop, nLeft, nBottom, nRight, cOrigin )

   METHOD KeyboardHook( nKey )
   METHOD IdleHook()
ENDCLASS

METHOD New( cString, nTop, nLeft, nBottom, nRight, cOrigin ) CLASS THBdictEdit
   ::cOrigin := cOrigin
   ::Super:New( cString, nTop, nLeft, nBottom, nRight )
RETURN Self

METHOD KeyboardHook( nKey ) CLASS THBdictEdit
   // Control enter has become a common way to exit multiline
   // editors
   IF nKey == K_CTRL_ENTER
      ::lSaved := .T.
      ::lExitEdit := .T.
   ENDIF

   IF nKey == K_F2
      IF ::naTextLen < Len( ::cOrigin )
         ::LoadText( padr( ::GetText(), Len( ::cOrigin) ) )
      ENDIF
      ::lSaved := .T.
      ::lExitEdit := .T.
   ENDIF
RETURN nKey

METHOD IdleHook() CLASS THBDictEdit
   ThreadSleep( 10 ) // release CPU
RETURN Self


