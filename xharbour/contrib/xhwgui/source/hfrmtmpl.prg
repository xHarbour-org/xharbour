/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HFormTemplate Class
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "fileio.ch"
#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HFormTemplate

   DATA name
   DATA file
   DATA aControls
   DATA lGet INIT .T.
   DATA script1, script2

   METHOD Open( fname,formname )
   METHOD End()
   METHOD Execute()

ENDCLASS

METHOD Open( fname,formname ) CLASS HFormTemplate
LOCAL strbuf := Space(512), poz := 513, stroka, nMode := 0
Local han
Local aControls := {}, itemName, i
Local cScript1 := "", cScript2 := "", nError, nLineEr

   han := FOPEN( fname, FO_READ + FO_SHARED )
   IF han == - 1
      MsgStop( "Can't open "+fname )
      Return Nil
   ENDIF
   DO WHILE .T.
      stroka := RDSTR( han,@strbuf,@poz,512 )
      IF LEN( stroka ) = 0
         EXIT
      ENDIF
      stroka := Ltrim( stroka )
      IF nMode == 0
         IF Left( stroka,1 ) == "#"
            IF Upper( Substr( stroka,2,4 ) ) == "FORM"
               stroka := Ltrim( Substr( stroka,7 ) )
               itemName := NextItem( stroka,.T. )
               IF Upper( itemName ) == Upper( formname )
                  Aadd( aControls, "DIALOG;"+stroka )
                  nMode := 1
               ENDIF
            ENDIF
         ENDIF
      ELSEIF nMode == 1
         IF Left( stroka,1 ) == "#"
            IF Upper( Substr( stroka,2,7 ) ) == "ENDFORM"
               Exit
            ELSEIF Upper( Substr( stroka,2,6 ) ) == "SCRIPT"
               nMode := 2
            ELSEIF Upper( Substr( stroka,2,4 ) ) == "CODE"
               aControls[Len(aControls)] += Chr(1) + Ltrim( Substr(stroka,6) )
            ENDIF
         ELSE
            itemName := NextItem( stroka,.T. )
            Aadd( aControls, stroka )
         ENDIF
      ELSEIF nMode == 2
         IF Left( stroka,1 ) == "#" .AND. Upper( Substr( stroka,2,6 ) ) == "ENDSCR"
            nMode := 1
         ELSE
            IF Len( aControls ) == 1
               cScript1 += stroka+Chr(13)+chr(10)
            ELSE
               cScript2 += stroka+Chr(13)+chr(10)
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   Fclose( han )

   IF Empty( aControls )
      MsgStop( formname + " not found or empty!" )
      Return Nil
   ENDIF

   ::file := fname
   ::name := formname
   ::aControls := aControls
   IF !Empty( cScript1 )
      IF ( ::script1 := RdScript( ,cScript1 ) ) == Nil
         nError := CompileErr( @nLineEr )
         MsgStop( "Script error ("+Ltrim(Str(nError))+"), line "+Ltrim(Str(nLineEr)) )
      ENDIF
   ENDIF
   IF !Empty( cScript2 )
      IF ( ::script2 := RdScript( ,cScript2 ) ) == Nil
         nError := CompileErr( @nLineEr )
         MsgStop( "Script error ("+Ltrim(Str(nError))+"), line "+Ltrim(Str(nLineEr)) )
      ENDIF
   ENDIF

Return Self

METHOD End() CLASS HFormTemplate
Return Nil

METHOD Execute() CLASS HFormTemplate
Local oDlg, oFont, cFont, oCtrl
Local x, y, nWidth, nHeight, nStyle, lClipper
Local i, j, strCode, cObjName, cVarName, cInit, cValid
Local aNames[Len(::aControls)-1,3]
Local bOldError

   NextItem( ::aControls[1],.T. )
   NextItem( ::aControls[1] )
   x := Val( NextItem( ::aControls[1] ) )
   y := Val( NextItem( ::aControls[1] ) )
   nWidth := Val( NextItem( ::aControls[1] ) )
   nHeight := Val( NextItem( ::aControls[1] ) )
   nStyle := Val( NextItem( ::aControls[1] ) )
   ::lGet := ( Upper( NextItem( ::aControls[1] ) ) == "T" )
   lClipper := ( Upper( NextItem( ::aControls[1] ) ) == "T" )
   cFont := NextItem( ::aControls[1] )

   IF !Empty( cFont )
      oFont := HFont():Add( NextItem( cFont,.T.,"," ), ;
            Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
            Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
            Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
            Val(NextItem( cFont,,"," )) )
   ENDIF

   IF oFont == Nil
      oFont := HFont():Add( "MS Sans Serif",0,-13 )
   ENDIF

   INIT DIALOG oDlg TITLE ::name                 ;
          AT x,y  SIZE nWidth,nHeight            ;
          FONT oFont

   oDlg:lClipper := lClipper

   FOR i := 2 TO Len( ::aControls )
      IF ( j := At( Chr(1),::aControls[i] ) ) != 0
         strCode := Substr( ::aControls[i],j+1 )
         cObjName := NextItem(strCode,.T.)
         IF Upper( NextItem(strCode) ) == "T"
            __mvPrivate( cObjName )
            aNames[i-1,1] := cObjName
         ENDIF
         cVarName := NextItem(strCode)
         IF Upper( NextItem(strCode) ) == "T"
            __mvPrivate( cVarName )
            cInit := NextItem(strCode)
            IF !Empty( cInit )
               IF Left( cInit,1 ) == "'" .OR. Left( cInit,1 ) == '"'
                  __mvPut( cVarName,SubStr( cInit,2,Len(cInit)-2 ) )
               ELSEIF IsDigit( cInit )
                  __mvPut( cVarName,Val( cInit ) )
               ELSE
                  bOldError := ERRORBLOCK( { | e | MacroError(1,e,cInit) } )
                  BEGIN SEQUENCE
                     __mvPut( cVarName,&cInit )
                  RECOVER
                     ERRORBLOCK( bOldError )
                     RETURN .F.
                  END SEQUENCE
                  ERRORBLOCK( bOldError )
               ENDIF
            ENDIF
         ELSE
            NextItem(strCode)
         ENDIF
         aNames[i-1,2] := cVarName
         aNames[i-1,3] := NextItem(strCode,,Chr(1))
      ENDIF
   NEXT

   IF ::script1 != Nil
      DoScript( ::script1 )
   ENDIF

   FOR i := 2 TO Len( ::aControls )

      oCtrl := String2Ctrl( oDlg,::aControls[i],aNames[i-1,2] )
      IF !Empty( aNames[i-1,1] )
         __mvPut( aNames[i-1,1],oCtrl )
      ENDIF
   NEXT

   ACTIVATE DIALOG oDlg

   IF ::script2 != Nil
      DoScript( ::script2 )
   ENDIF

Return Nil

Static Function String2Ctrl( oDlg,stroka,cVarName )
Local oCtrl, aCtrl, oFont, i

   IF Empty( stroka )
      Return Nil
   ENDIF

   IF ( i := At( Chr(1),stroka ) ) != 0
      stroka  := Left( stroka,i-1 )
   ENDIF

   aCtrl := { NextItem( stroka,.T. ),    ;
              NextItem( stroka ),        ;
              Val( NextItem( stroka ) ), ;
              Val( NextItem( stroka ) ), ;
              Val( NextItem( stroka ) ), ;
              Val( NextItem( stroka ) ), ;
              Val( NextItem( stroka ) ), ;
              Val( NextItem( stroka ) ), ;
              NextItem( stroka ),        ;
              NextItem( stroka ),        ;
              NextItem( stroka )         ;
            }
   oFont := Iif( Empty(aCtrl[9]), Nil, ;
               HFont():Add( NextItem( aCtrl[9],.T.,"," ), ;
               Val(NextItem( aCtrl[9],,"," )),Val(NextItem( aCtrl[9],,"," )), ;
               Val(NextItem( aCtrl[9],,"," )),Val(NextItem( aCtrl[9],,"," )), ;
               Val(NextItem( aCtrl[9],,"," )),Val(NextItem( aCtrl[9],,"," )), ;
               Val(NextItem( aCtrl[9],,"," )) ) )

   IF aCtrl[8] == 0
      aCtrl[8] := Nil
   ENDIF
   IF aCtrl[3] == 0
      aCtrl[3] := Nil
   ENDIF
   IF aCtrl[1] == "STATIC"
      oCtrl := HStatic():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "BUTTON"
      oCtrl := HButton():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,,,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "GROUP"
      oCtrl := HGroup():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "EDITBOX"
      oCtrl := HEdit():New( oDlg,aCtrl[3],Iif(cVarName==Nil,aCtrl[2],&cVarName),     ;
          Iif(cVarName==Nil,Nil,&("{|v|Iif(v==Nil,"+&cVarName+","+&cVarName+":=v)}")),    ;
          aCtrl[8],aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],oFont,,,,,,,          ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "CHECKBOX"
      oCtrl := HCheckButton():New( oDlg,aCtrl[3],cVarName, ;
          Iif(cVarName==Nil,Nil,&("{|v|Iif(v==Nil,"+&cVarName+","+&cVarName+":=v)}")),    ;
          aCtrl[8],aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,,,,,;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "RADIOBUTTON"
      oCtrl := HRadioButton():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,,,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "DATEPICKER"
      oCtrl := HDatePicker():New( oDlg,aCtrl[3],cVarName, ;
          Iif(cVarName==Nil,Nil,&("{|v|Iif(v==Nil,"+&cVarName+","+&cVarName+":=v)}")),    ;
          aCtrl[8],aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],,oFont,,,,,        ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "UPDOWN"
      oCtrl := HUpDown():New( oDlg,aCtrl[3],cVarName, ;
          Iif(cVarName==Nil,Nil,&("{|v|Iif(v==Nil,"+&cVarName+","+&cVarName+":=v)}")),    ;
          aCtrl[8],aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],oFont,,,,,,,       ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
   ELSEIF aCtrl[1] == "COMBOBOX"
      oCtrl := HComboBox():New( oDlg,aCtrl[3],cVarName, ;
          Iif(cVarName==Nil,Nil,&("{|v|Iif(v==Nil,"+&cVarName+","+&cVarName+":=v)}")),    ;
          aCtrl[8],aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],,oFont )
   ELSEIF aCtrl[1] == "HLINE"
      oCtrl := hLine():New( oDlg,aCtrl[3],.F., ;
          aCtrl[4],aCtrl[5],aCtrl[6] )
   ELSEIF aCtrl[1] == "VLINE"
      oCtrl := hLine():New( oDlg,aCtrl[3],.T., ;
          aCtrl[4],aCtrl[5],aCtrl[7] )
   ELSEIF aCtrl[1] == "PANEL"
      oCtrl := HPanel():New( oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
   ELSEIF aCtrl[1] == "OWNERBUTTON"
      oCtrl := HOwnButton():New( oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
   ELSEIF aCtrl[1] == "BROWSE"
      oCtrl := HBrowse():New( ,oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
   ELSE
      MsgStop( "Wrong item name: " + aCtrl[1] )
   ENDIF

Return oCtrl

STATIC FUNCTION MacroError( nm, e, stroka )

   IF nm == 1
      MsgStop( ErrorMessage( e ) + Chr(10)+Chr(13) + "in" + Chr(10)+Chr(13) + ;
             AllTrim(stroka),"Variable initialization error" )
   ENDIF
   BREAK
RETURN .T.
