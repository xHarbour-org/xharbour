/*
 * HWGUI - Harbour Win32 GUI library source code:
 * RepExec - Loading and executing of reports, built with RepBuild
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"
#include "repmain.h"
#include "fileio.ch"

// #define __DEBUG__

Static aPaintRep := Nil

REQUEST DBUSEAREA
REQUEST RECNO
REQUEST DBSKIP
REQUEST DBGOTOP

Function ClonePaintRep( ar )
   aPaintRep := Aclone( ar )
Return Nil

Function SetPaintRep( ar )
   aPaintRep := ar
Return Nil

Function OpenReport( fname,repName )
LOCAL strbuf := Space(512), poz := 513, stroka, nMode := 0
Local han
Local itemName, aItem, res := .T., sFont
Local nFormWidth

   IF aPaintRep != Nil .AND. fname == aPaintRep[FORM_FILENAME] .AND. repName == aPaintRep[FORM_REPNAME]
      Return res
   ENDIF
   han := FOPEN( fname, FO_READ + FO_SHARED )
   IF han <> - 1
      DO WHILE .T.
         stroka := RDSTR( han,@strbuf,@poz,512 )
         IF LEN( stroka ) = 0
            EXIT
         ENDIF
         IF Left( stroka,1 ) == ";"
            LOOP
         ENDIF
         IF nMode == 0
            IF Left( stroka,1 ) == "#"
               IF Upper( Substr( stroka,2,6 ) ) == "REPORT"
                  stroka := Ltrim( Substr( stroka,9 ) )
                  IF Upper( stroka ) == Upper( repName )
                     nMode := 1
                     aPaintRep := { 0,0,0,0,0,{},fname,repName,.F.,0,Nil }
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF nMode == 1
            IF Left( stroka,1 ) == "#"
               IF Upper( Substr( stroka,2,6 ) ) == "ENDREP"
                  Exit
               ELSEIF Upper( Substr( stroka,2,6 ) ) == "SCRIPT"
                  nMode := 2
                  IF aItem != Nil
                     aItem[ITEM_SCRIPT] := ""
                  ELSE
                     aPaintRep[FORM_VARS] := ""
                  ENDIF
               ENDIF
            ELSE
               IF ( itemName := NextItem( stroka,.T. ) ) == "FORM"
                  aPaintRep[FORM_WIDTH] := Val( NextItem( stroka ) )
                  aPaintRep[FORM_HEIGHT] := Val( NextItem( stroka ) )
                  nFormWidth := Val( NextItem( stroka ) )
                  aPaintRep[FORM_XKOEF] := nFormWidth/aPaintRep[FORM_WIDTH]
               ELSEIF itemName == "TEXT"
                  Aadd( aPaintRep[FORM_ITEMS], { 1,NextItem(stroka),Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)), Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)),Val(NextItem(stroka)),0,NextItem(stroka), ;
                           Val(NextItem(stroka)),0,Nil,0 } )
                  aItem := Atail( aPaintRep[FORM_ITEMS] )
                  aItem[ITEM_FONT] := HFont():Add( NextItem( aItem[ITEM_FONT],.T.,"," ), ;
                    Val(NextItem( aItem[ITEM_FONT],,"," )),Val(NextItem( aItem[ITEM_FONT],,"," )), ;
                    Val(NextItem( aItem[ITEM_FONT],,"," )),Val(NextItem( aItem[ITEM_FONT],,"," )), ;
                    Val(NextItem( aItem[ITEM_FONT],,"," )),Val(NextItem( aItem[ITEM_FONT],,"," )), ;
                    Val(NextItem( aItem[ITEM_FONT],,"," )) )
                  IF aItem[ITEM_X1] == Nil .OR. aItem[ITEM_X1] == 0 .OR. ;
                     aItem[ITEM_Y1] == Nil .OR. aItem[ITEM_Y1] == 0 .OR. ;
                     aItem[ITEM_WIDTH] == Nil .OR. aItem[ITEM_WIDTH] == 0 .OR. ;
                     aItem[ITEM_HEIGHT] == Nil .OR. aItem[ITEM_HEIGHT] == 0
                     MsgStop( "Error: "+stroka )
                     res := .F.
                     EXIT
                  ENDIF
               ELSEIF itemName == "HLINE" .OR. itemName == "VLINE" .OR. itemName == "BOX"
                  Aadd( aPaintRep[FORM_ITEMS], { Iif(itemName=="HLINE",2,Iif(itemName=="VLINE",3,4)), ;
                           "",Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)), Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)),0,NextItem(stroka),0,0,0,Nil,0 } )
                  aItem := Atail( aPaintRep[FORM_ITEMS] )
                  aItem[ITEM_PEN] := HPen():Add( Val(NextItem( aItem[ITEM_PEN],.T.,"," )), ;
                          Val(NextItem( aItem[ITEM_PEN],,"," )),Val(NextItem( aItem[ITEM_PEN],,"," )) )
                  IF aItem[ITEM_X1] == Nil .OR. aItem[ITEM_X1] == 0 .OR. ;
                     aItem[ITEM_Y1] == Nil .OR. aItem[ITEM_Y1] == 0 .OR. ;
                     aItem[ITEM_WIDTH] == Nil .OR. aItem[ITEM_WIDTH] == 0 .OR. ;
                     aItem[ITEM_HEIGHT] == Nil .OR. aItem[ITEM_HEIGHT] == 0
                     MsgStop( "Error: "+stroka )
                     res := .F.
                     EXIT
                  ENDIF
               ELSEIF itemName == "BITMAP"
                  Aadd( aPaintRep[FORM_ITEMS], { 5, NextItem(stroka), ;
                           Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)), Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)),0,0,0,0,0,Nil,0 } )
                  aItem := Atail( aPaintRep[FORM_ITEMS] )
                  IF aItem[ITEM_X1] == Nil .OR. aItem[ITEM_X1] == 0 .OR. ;
                     aItem[ITEM_Y1] == Nil .OR. aItem[ITEM_Y1] == 0 .OR. ;
                     aItem[ITEM_WIDTH] == Nil .OR. aItem[ITEM_WIDTH] == 0 .OR. ;
                     aItem[ITEM_HEIGHT] == Nil .OR. aItem[ITEM_HEIGHT] == 0
                     MsgStop( "Error: "+stroka )
                     res := .F.
                     EXIT
                  ENDIF
               ELSEIF itemName == "MARKER"
                  Aadd( aPaintRep[FORM_ITEMS], { 6, NextItem(stroka),Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)), Val(NextItem(stroka)), ;
                           Val(NextItem(stroka)), Val(NextItem(stroka)), ;
                           0,0,0,0,Nil,0 } )
                  aItem := Atail( aPaintRep[FORM_ITEMS] )
               ENDIF
            ENDIF
         ELSEIF nMode == 2
            IF Left( stroka,1 ) == "#" .AND. Upper( Substr( stroka,2,6 ) ) == "ENDSCR"
               nMode := 1
            ELSE
               IF aItem != Nil
                  aItem[ITEM_SCRIPT] += stroka+Chr(13)+chr(10)
               ELSE
                  aPaintRep[FORM_VARS] += stroka+Chr(13)+chr(10)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      Fclose( han )
   ELSE
      MsgStop( "Can't open "+fname )
      Return .F.
   ENDIF
   IF Empty( aPaintRep[FORM_ITEMS] )
      MsgStop( repname+" not found or empty!" )
      res := .F.
   ELSE
      aPaintRep[FORM_ITEMS] := Asort( aPaintRep[FORM_ITEMS],,, {|z,y|z[ITEM_Y1]<y[ITEM_Y1].OR.(z[ITEM_Y1]==y[ITEM_Y1].AND.z[ITEM_X1]<y[ITEM_X1]).OR.(z[ITEM_Y1]==y[ITEM_Y1].AND.z[ITEM_X1]==y[ITEM_X1].AND.(z[ITEM_WIDTH]<y[ITEM_WIDTH].OR.z[ITEM_HEIGHT]<y[ITEM_HEIGHT]))} )
   ENDIF
Return res

Function RecalcForm( aPaintRep,nFormWidth )
Local hDC, aMetr, aItem, i
   hDC := GetDC( GetActiveWindow() )
   aMetr := GetDeviceArea( hDC )
   aPaintRep[FORM_XKOEF] := ( aMetr[1]-XINDENT )/aPaintRep[FORM_WIDTH]
   ReleaseDC( GetActiveWindow(),hDC )

   IF nFormWidth != aMetr[1]-XINDENT
      FOR i := 1 TO Len(aPaintRep[FORM_ITEMS])
         aItem := aPaintRep[FORM_ITEMS,i]
         aItem[ITEM_X1] := Round( aItem[ITEM_X1]*(aMetr[1]-XINDENT)/nFormWidth,0 )
         aItem[ITEM_Y1] := Round( aItem[ITEM_Y1]*(aMetr[1]-XINDENT)/nFormWidth,0 )
         aItem[ITEM_WIDTH] := Round( aItem[ITEM_WIDTH]*(aMetr[1]-XINDENT)/nFormWidth,0 )
         aItem[ITEM_HEIGHT] := Round( aItem[ITEM_HEIGHT]*(aMetr[1]-XINDENT)/nFormWidth,0 )
      NEXT
   ENDIF
Return Nil

Function PrintReport( printerName,oPrn,lPreview )
Local oPrinter := Iif( oPrn != Nil, oPrn, HPrinter():New( printerName ) )
Local hDCwindow
Local aPrnCoors, prnXCoef, prnYCoef
Local iItem, aItem, nLineStartY := 0, nLineHeight := 0, nPHStart := 0
Local iPH := 0, iSL := 0, iEL := 0, iPF := 0, iEPF := 0, iDF := 0
Local poz := 0, stroka, varName, varValue
Local i, aMetr, aTmetr, aPmetr, dKoef, pKoef
Local fontKoef, oFont
Local lAddMode := .F., nYadd := 0, nEndList := 0
memvar lFirst, lFinish, lLastCycle, oFontStandard
Private lFirst := .T., lFinish := .T., lLastCycle := .F.

   IF oPrinter:hDCPrn == Nil .OR. oPrinter:hDCPrn == 0
      Return .F.
   ENDIF

   aPrnCoors := GetDeviceArea( oPrinter:hDCPrn )
   prnXCoef := aPrnCoors[1]/aPaintRep[FORM_WIDTH]
   prnYCoef := aPrnCoors[2]/aPaintRep[FORM_HEIGHT]

   IF Type( "oFontStandard" ) = "U"
      Private oFontStandard := HFont():Add( "Arial",0,-13,400,204 )
   ENDIF

   hDCwindow := GetDC( Hwindow():GetMain():handle )
   aMetr := GetDeviceArea( hDCwindow )
   SelectObject( hDCwindow, oFontStandard:handle )
   aTmetr := GetTextMetric( hDCwindow )
   dKoef := ( aMetr[1]-XINDENT ) / aTmetr[2]
   ReleaseDC( Hwindow():GetMain():handle,hDCwindow )

   SelectObject( oPrinter:hDCPrn, oFontStandard:handle )
   aPmetr := GetTextMetric( oPrinter:hDCPrn )
   pKoef := aPrnCoors[1] / aPmetr[2]
   fontKoef := pKoef / dKoef
   FOR i := 1 TO Len( aPaintRep[FORM_ITEMS] )
      IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_TEXT
         oFont := aPaintRep[FORM_ITEMS,i,ITEM_FONT]
         aPaintRep[FORM_ITEMS,i,ITEM_STATE] := HFont():Add( oFont:name,   ;
              oFont:width,Round(oFont:height*fontKoef,0),oFont:weight, ;
              oFont:charset,oFont:italic )
      ENDIF
   NEXT

   IF Valtype(aPaintRep[FORM_VARS]) == "C"
      DO WHILE .T.
         stroka := RDSTR( , aPaintRep[FORM_VARS], @poz )
         IF LEN( stroka ) = 0
            EXIT
         ENDIF
         DO WHILE !EMPTY( varName := getNextVar( @stroka, @varValue ) )
            PRIVATE &varName
            IF varvalue != Nil
               &varName := &varValue
            ENDIF
         ENDDO
      ENDDO
   ENDIF

   FOR iItem := 1 TO Len( aPaintRep[FORM_ITEMS] )
      aItem := aPaintRep[FORM_ITEMS,iItem]
      IF aItem[ITEM_TYPE] == TYPE_MARKER
         aItem[ITEM_STATE] := 0
         IF aItem[ITEM_CAPTION] == "SL"
            nLineStartY := aItem[ITEM_Y1]
            aItem[ITEM_STATE] := 0
            iSL := iItem
         ELSEIF aItem[ITEM_CAPTION] == "EL"
            nLineHeight := aItem[ITEM_Y1] - nLineStartY
            iEL := iItem
         ELSEIF aItem[ITEM_CAPTION] == "PF"
            nEndList := aItem[ITEM_Y1]
            iPF := iItem
         ELSEIF aItem[ITEM_CAPTION] == "EPF"
            iEPF := iItem
         ELSEIF  aItem[ITEM_CAPTION] == "DF"
            iDF := iItem
            IF iPF == 0
               nEndList := aItem[ITEM_Y1]
            ENDIF
         ELSEIF aItem[ITEM_CAPTION] == "PH"
            iPH := iItem
            nPHStart := aItem[ITEM_Y1]
         ENDIF
      ENDIF
   NEXT
   IF iPH > 0 .AND. iSL == 0
      MsgStop( "'Start Line' marker is absent" )
      oPrinter:End()
      Return .F.
   ELSEIF iSL > 0 .AND. iEL == 0
      MsgStop( "'End Line' marker is absent" )
      oPrinter:End()
      Return .F.
   ELSEIF iPF > 0 .AND. iEPF == 0
      MsgStop( "'End of Page Footer' marker is absent" )
      oPrinter:End()
      Return .F.
   ELSEIF iSL > 0 .AND. iPF == 0 .AND. iDF == 0
      MsgStop( "'Page Footer' and 'Document Footer' markers are absent" )
      oPrinter:End()
      Return .F.
   ENDIF

#ifdef __DEBUG__
   oPrinter:End()
   Writelog( "Startdoc" )
   Writelog( "Startpage" )
#else
   oPrinter:StartDoc( lPreview )
   oPrinter:StartPage()
#endif

   DO WHILE .T.
      iItem := 1
      DO WHILE iItem <= Len( aPaintRep[FORM_ITEMS] )
         aItem := aPaintRep[FORM_ITEMS,iItem]
         // WriteLog( Str(iItem,3)+": "+Str(aItem[ITEM_TYPE]) )
         IF aItem[ITEM_TYPE] == TYPE_MARKER
            IF aItem[ITEM_CAPTION] == "PH"
               IF aItem[ITEM_STATE] == 0
                  aItem[ITEM_STATE] := 1
                  FOR i := 1 TO iPH-1
                     IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_BITMAP
                        PrintItem( oPrinter, aPaintRep, aPaintRep[FORM_ITEMS,i], prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
                     ENDIF
                  NEXT
               ENDIF
            ELSEIF aItem[ITEM_CAPTION] == "SL"
               IF aItem[ITEM_STATE] == 0
                  // IF iPH == 0
                     FOR i := 1 TO iSL-1
                        IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_BITMAP
                           PrintItem( oPrinter, aPaintRep, aPaintRep[FORM_ITEMS,i], prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
                        ENDIF
                     NEXT
                  // ENDIF
                  aItem[ITEM_STATE] := 1
                  IF !ScriptExecute( aItem )
#ifdef __DEBUG__
                     Writelog( "Endpage" )
                     Writelog( "Enddoc" )
#else
                     oPrinter:EndPage()
                     oPrinter:EndDoc()
                     oPrinter:End()
#endif
                     Return .F.
                  ENDIF
                  IF lLastCycle
                     iItem := iEL + 1
                     LOOP
                  ENDIF
               ENDIF
               lAddMode := .T.
            ELSEIF aItem[ITEM_CAPTION] == "EL"
               FOR i := iSL+1 TO iEL-1
                  IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_BITMAP
                     PrintItem( oPrinter, aPaintRep, aPaintRep[FORM_ITEMS,i], prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
                  ENDIF
               NEXT
               IF !ScriptExecute( aItem )
#ifdef __DEBUG__
                     Writelog( "Endpage" )
                     Writelog( "Enddoc" )
#else
                  oPrinter:EndPage()
                  oPrinter:EndDoc()
                  oPrinter:End()
#endif
                  Return .F.
               ENDIF
               IF !lLastCycle
                  nYadd += nLineHeight
                  // Writelog( Str(nLineStartY)+" "+Str(nYadd)+" "+Str(nEndList) )
                  IF nLineStartY+nYadd+nLineHeight >= nEndList
                     // Writelog("New Page")
                     IF iPF == 0
#ifdef __DEBUG__
                        Writelog( "Endpage" )
                        Writelog( "Startpage" )
#else
                        oPrinter:EndPage()
                        oPrinter:StartPage()
#endif
                        nYadd := 10 - Iif( nPHStart > 0,nPHStart,nLineStartY )
                        lAddMode := .T.
                        IF iPH == 0
                           iItem := iSL
                        ELSE
                           iItem := iPH
                        ENDIF
                     ELSE
                        lAddMode := .F.
                     ENDIF
                  ELSE
                     iItem := iSL
                  ENDIF
               ELSE
                  lAddMode := .F.
               ENDIF
            ELSEIF aItem[ITEM_CAPTION] == "EPF"
               FOR i := iPF+1 TO iEPF-1
                  IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_BITMAP
                     PrintItem( oPrinter, aPaintRep, aPaintRep[FORM_ITEMS,i], prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
                  ENDIF
               NEXT
               IF !lLastCycle
#ifdef __DEBUG__
                  Writelog( "Endpage" )
                  Writelog( "Startpage" )
#else
                  oPrinter:EndPage()
                  oPrinter:StartPage()
#endif
                  nYadd := 10 - Iif( nPHStart > 0,nPHStart,nLineStartY )
                  lAddMode := .T.
                  IF iPH == 0
                     iItem := iSL
                  ELSE
                     iItem := iPH
                  ENDIF
               ENDIF
            ELSEIF aItem[ITEM_CAPTION] == "DF"
               lAddMode := .F.
               IF aItem[ITEM_ALIGN] == 1
               ENDIF
            ENDIF
         ELSE
            IF aItem[ITEM_TYPE] == TYPE_TEXT
               IF !ScriptExecute( aItem )
#ifdef __DEBUG__
                  Writelog( "Endpage" )
                  Writelog( "Enddoc" )
#else
                  oPrinter:EndPage()
                  oPrinter:EndDoc()
#endif
                  oPrinter:End()
                  Return .F.
               ENDIF
            ENDIF
            IF aItem[ITEM_TYPE] != TYPE_BITMAP
               PrintItem( oPrinter, aPaintRep, aItem, prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
            ENDIF
         ENDIF
         iItem++
      ENDDO
      FOR i := Iif(iSL==0,1,Iif(iDF>0,iDF+1,Iif(iPF>0,iEPF+1,iEL+1))) TO Len( aPaintRep[FORM_ITEMS] )
         IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_BITMAP
            PrintItem( oPrinter, aPaintRep, aPaintRep[FORM_ITEMS,i], prnXCoef, prnYCoef, Iif(lAddMode,nYadd,0), .T. )
         ENDIF
      NEXT
      IF lFinish
         Exit
      ENDIF
   ENDDO

#ifdef __DEBUG__
   Writelog( "Endpage" )
   Writelog( "Enddoc" )
#else
   oPrinter:EndPage()
   oPrinter:EndDoc()
   IF lPreview != Nil .AND. lPreview
      oPrinter:Preview()
   ENDIF
#endif
   oPrinter:End()

   FOR i := 1 TO Len( aPaintRep[FORM_ITEMS] )
      IF aPaintRep[FORM_ITEMS,i,ITEM_TYPE] == TYPE_TEXT
         aPaintRep[FORM_ITEMS,i,ITEM_STATE]:Release()
         aPaintRep[FORM_ITEMS,i,ITEM_STATE] := Nil
      ENDIF
   NEXT

Return .T.

Function PrintItem( oPrinter, aPaintRep, aItem, prnXCoef, prnYCoef, nYadd, lCalc )
Local x1 := aItem[ITEM_X1], y1 := aItem[ITEM_Y1]+nYadd, x2, y2
Local hBitmap, stroka

   x2 := x1+aItem[ITEM_WIDTH]-1
   y2 := y1+aItem[ITEM_HEIGHT]-1
   // writelog( Str(aItem[ITEM_TYPE])+": "+Iif(aItem[ITEM_TYPE]==TYPE_TEXT,aItem[ITEM_CAPTION],"")+str(x1)+str(y1)+str(x2)+str(y2) )
   x1 := Round( x1*prnXCoef/aPaintRep[FORM_XKOEF],0 )
   y1 := Round( y1*prnYCoef/aPaintRep[FORM_XKOEF],0 )
   x2 := Round( x2*prnXCoef/aPaintRep[FORM_XKOEF],0 )
   y2 := Round( y2*prnYCoef/aPaintRep[FORM_XKOEF],0 )
   // writelog( "PrintItem-2: "+str(x1)+str(y1)+str(x2)+str(y2))

#ifdef __DEBUG__
   Writelog( Str(aItem[ITEM_TYPE])+": "+Str(x1)+" "+Str(y1)+" "+Str(x2)+" "+Str(y2)+" "+Iif(aItem[ITEM_TYPE] == TYPE_TEXT,aItem[ITEM_CAPTION]+Iif(aItem[ITEM_VAR]>0,"("+&( aItem[ITEM_CAPTION] )+")",""),"") )
#else
   // Writelog( Str(aItem[ITEM_TYPE])+": "+Str(x1)+" "+Str(y1)+" "+Str(x2)+" "+Str(y2)+" "+Iif(aItem[ITEM_TYPE] == TYPE_TEXT,aItem[ITEM_CAPTION]+Iif(aItem[ITEM_VAR]>0,"("+&( aItem[ITEM_CAPTION] )+")",""),"") )
   IF aItem[ITEM_TYPE] == TYPE_TEXT
      IF aItem[ITEM_VAR] > 0
         stroka := Iif( lCalc,&( aItem[ITEM_CAPTION] ),"" )
      ELSE
         stroka := aItem[ITEM_CAPTION]
      ENDIF
      IF !Empty( aItem[ITEM_CAPTION] )
         oPrinter:Say( stroka,x1,y1,x2,y2, ;
                 Iif(aItem[ITEM_ALIGN]==0,DT_LEFT,Iif(aItem[ITEM_ALIGN]==1,DT_RIGHT,DT_CENTER)), ;
                 aItem[ITEM_STATE] )
      ENDIF
   ELSEIF aItem[ITEM_TYPE] == TYPE_HLINE
      oPrinter:Line( x1,Round((y1+y2)/2,0),x2,Round((y1+y2)/2,0),aItem[ITEM_PEN] )
   ELSEIF aItem[ITEM_TYPE] == TYPE_VLINE
      oPrinter:Line( Round((x1+x2)/2,0),y1,Round((x1+x2)/2,0),y2,aItem[ITEM_PEN] )
   ELSEIF aItem[ITEM_TYPE] == TYPE_BOX
      oPrinter:Box( x1, y1, x2, y2, aItem[ITEM_PEN] )
   ELSEIF aItem[ITEM_TYPE] == TYPE_BITMAP
      hBitmap := OpenBitmap( aItem[ITEM_CAPTION], oPrinter:hDC )
      // writelog( "hBitmap: "+str(hBitmap) )
      oPrinter:Bitmap( x1, y1, x2, y2,, hBitmap )
      DeleteObject( hBitmap )
      // DrawBitmap( hDC, aItem[ITEM_BITMAP],SRCAND, x1, y1, x2-x1+1, y2-y1+1 )
   ENDIF
#endif
Return Nil

Static Function ScriptExecute( aItem )
Local nError, nLineEr
   IF aItem[ITEM_SCRIPT] != Nil .AND. !Empty( aItem[ITEM_SCRIPT] )
      IF Valtype( aItem[ITEM_SCRIPT] ) == "C"
         IF ( aItem[ITEM_SCRIPT] := RdScript( ,aItem[ITEM_SCRIPT] ) ) == Nil
            nError := CompileErr( @nLineEr )
            MsgStop( "Script error ("+Ltrim(Str(nError))+"), line "+Ltrim(Str(nLineEr)) )
            Return .F.
         ENDIF
      ENDIF
      Return DoScript( aItem[ITEM_SCRIPT] )
   ENDIF
Return .T.
