*****************************************************
* HB I18N dictionary editor
*
* $Id: hbdict.prg,v 1.10 2003/11/03 05:16:58 jonnymind Exp $
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
   LOCAL aInput, aOutput, aTable
   LOCAL recpos, nLength
   LOCAL nKey
   LOCAL lModified := .F.
   LOCAL oBrowse := TBrowseNew( 8, 2, 22, 75 )
   LOCAL lContinue := .T.
   LOCAL cLang, cProgPath

   SAVE SCREEN
   SET COLOR TO W+/B
   CLEAR SCREEN

   /* Load internationalization language if not already loaded */
   IF .not. HB_I18nInitialized()
      cLang := GetEnv( "LANG" )
      IF .not. Empty(cLang)
         HB_FnameSplit( hb_argv(0), @cProgPath )
         HB_I18nSetPath( cProgPath )
         cLang := "hbdict_" + cLang
         HB_I18nSetLanguage( cLang )
      ENDIF
   ENDIF

   @0,0 SAY PadC( ;
      i18n( "X H A R B O U R - Dictionary Editor (preview, 3rd)" ), MaxCol() )

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
   nLength := Len( aInput[2] )
   aTable := aInput[2]
   ShowHeader( aInput[1] )

   @6,3 SAY i18n("Saving to: ") + cOutput
   @2,52 SAY i18n("Press 'S' to save")
   @3,52 SAY i18n("Press 'E' to edit header")
   recpos := 1

   oBrowse:colorSpec := "W+/B, N/BG"
   oBrowse:ColSep := "Ё"
   oBrowse:HeadSep := "ям"
   oBrowse:FootSep := "ом"
   oBrowse:Border := Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) +;
      Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 )

   oBrowse:GoTopBlock    = { || recpos := 1 }
   oBrowse:GoBottomBlock = { || recpos := nLength }
   oBrowse:SkipBlock     = { | nSkip, nPos |;
      nPos := recpos,;
      recpos := If( nSkip > 0, Min( nLength, nPos + nSkip ),;
      Max( 1, nPos + nSkip )), ;
      recpos - nPos }

   oBrowse:AddColumn( TBColumnNew( i18n("Index"),  { || recpos } ) )

   oBrowse:AddColumn( TBColumnNew( i18n("Untraslated"),;
         {|| StringTrim( aTable[ recpos ][1], 30 )} ))

   oBrowse:AddColumn( TBColumnNew( i18n("Translated"),;
         {|| StringTrim( aTable[ recpos ][2], 30 )} ))

   oBrowse:GetColumn(1):Picture = '99999'
   oBrowse:GetColumn(2):width = 30
   oBrowse:GetColumn(3):width = 30

   oBrowse:Configure(3)
   @23,5 SAY Padc( i18n( "Press enter to edit an entry" ), 70)
   SET CURSOR OFF

   DO WHILE lContinue
      oBrowse:ForceStable()

      nKey := Inkey(0)

      SWITCH nKey
         CASE K_ESC
            IF .not. lModified .or.;
                  Alert( i18n( "Dictionary modified ! Do you really want to exit?" ), { i18n( "Yes" ), i18n( "No" ) } ) == 1
               lContinue := .F.
            ENDIF
         EXIT

         CASE K_ENTER
            oBrowse:Configure(3)
            SET CURSOR ON
            EditEntry( aTable, recpos )
            SET CURSOR OFF
         EXIT

         CASE 's'
         CASE 'S'
            SaveTable( cOutput, aInput )
         EXIT

         CASE 'e'
         CASE 'E'
            ReadHeader( aInput[1] )
         EXIT

         DEFAULT
            oBrowse:ApplyKey( nKey )
      END
   ENDDO

   SET CURSOR ON
   SET COLOR TO W/N
   CLEAR SCREEN
   @24,0
   RESTORE SCREEN
RETURN

PROCEDURE DoQuit()
   @24, 20 say i18n( "Program terminated. Press any key" )
   Inkey( 0 )
   CLEAR SCREEN
   QUIT
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

PROCEDURE EditEntry( aTable, nPos )
   LOCAL cInput := aTable[nPos][2]
   LOCAL oEditor, oEdit1
   LOCAL cScreen

   SAVE SCREEN TO cScreen
   MakeBoxShadow( 4, 10, 20, 71 )
   MakeBox( 10, 10, 16, 70 )
   @17,12 SAY i18n("Press ESC to exit without saving, and CTRL+W to save")
   @18,12 SAY i18n("Press F2 to save padding to the original text length")

   @4,12 SAY " " + i18n( "International String" ) + " "
   oEdit1 := HBEditor():New( ">>" + aTable[nPos][1] + "<<",;
      5, 11, 9 , 69, .F., 55 )
   oEdit1:RefreshWindow()

   @10,12 SAY " " + i18n( "Local String" ) + " "

   IF cInput == NIL
      cInput := ""
   ENDIF

   oEditor := THBDictEdit():New( cInput, 11, 11, 15, 69, aTable[ nPos ][1] )

   oEditor:Edit()
   IF oEditor:lSaved
      cInput := oEditor:GetText()
      IF Len( cInput ) > 0
         aTable[nPos][2] := cInput
      ENDIF
   ENDIF

   RESTORE SCREEN FROM cScreen
RETURN


FUNCTION SaveTable( cFileName, aTable )
   LOCAL aHeader, cScreen
   LOCAL lOk := .F.

   SAVE SCREEN TO cScreen
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
   RESTORE SCREEN FROM cScreen

RETURN lOk

PROCEDURE ReadHeader( aHeader )
   LOCAL GetList := {}
   LOCAL cAuthor, cLanguage, cCode
   LOCAL cScreen

   SAVE SCREEN TO cScreen
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

   RESTORE SCREEN FROM cScreen
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


FUNCTION StringTrim( cStr, nLen )
   IF cStr == NIL
      cStr := i18n( "<Not yet translated>")
   ENDIF
   IF Len( cStr ) < nLen
      RETURN Padr( cStr, nLen )
   ENDIF
RETURN Substr( cStr, 1, nLen - 3 ) + "..."

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


