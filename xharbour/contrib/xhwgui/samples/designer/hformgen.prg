/*
 * HWIDE
 * HFormGen class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "fileio.ch"
#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define UDS_SETBUDDYINT     2
#define UDS_ALIGNRIGHT      4
#define  CONTROL_FIRST_ID   34000

Static aCtrlNames := { { "HSTATIC","STATIC" }, { "HBUTTONGEN","BUTTON" }, ;
    { "HCHECKBOXGEN","CHECKBOX" }, { "HRADIOBUTTONGEN","RADIOBUTTON" },   ;
    { "HEDITGEN","EDITBOX" }, { "HGROUP","GROUPBOX" }, { "HDATEGEN","DATEPICKER" }, ;
    { "HUPDOWNGEN","UPDOWN" }, { "HCOMBOGEN","COMBOBOX" }, { "HLINE","HLINE" }, ;
    { "HLINE","VLINE" }, { "HPANELGEN","PANEL" }, { "HOWNBUTGEN","OWNERBUTTON" }, ;
    { "HBROWSEGEN","BROWSE" } }

Static aRcNames := { { "CONTROL","" }, { "AUTO3STATE","CHECKBOX" },        ;
    { "AUTOCHECKBOX","CHECKBOX" }, { "AUTORADIOBUTTON","RADIOBUTTON" },    ;
    { "CHECKBOX","CHECKBOX" }, { "COMBOBOX","COMBOBOX" }, { "CTEXT","STATIC" }, ;
    { "DEFPUSHBUTTON","BUTTON" }, { "EDITTEXT","EDITBOX" }, { "GROUPBOX","GROUPBOX" },;
    { "ICON","" }, { "LISTBOX","" }, { "LTEXT","STATIC" }, { "PUSHBOX","BUTTON" }, ;
    { "PUSHBUTTON","BUTTON" }, { "RADIOBUTTON","RADIOBUTTON" }, { "RTEXT","STATIC" },;
    { "SCROLLBAR","" }, { "STATE3","CHECKBOX" } }

Static aStaticTypes := { { SS_LEFT,"SS_LEFT" }, { SS_CENTER,"SS_CENTER" }, ;
    { SS_RIGHT,"SS_RIGHT" }, { SS_BLACKFRAME,"SS_BLACKFRAME" },            ;
    { SS_GRAYFRAME,"SS_GRAYFRAME" }, { SS_WHITEFRAME,"SS_WHITEFRAME" },    ;
    { SS_BLACKRECT,"SS_BLACKRECT" }, { SS_GRAYRECT,"SS_GRAYRECT" },        ;
    { SS_WHITERECT,"SS_WHITERECT" }, { SS_ETCHEDFRAME,"SS_ETCHEDFRAME" },  ;
    { SS_ETCHEDHORZ,"SS_ETCHEDHORZ" }, { SS_ETCHEDVERT,"SS_ETCHEDVERT" },  ;
    { SS_OWNERDRAW,"SS_OWNERDRAW" } }

Static aStyles := { { WS_POPUP,"WS_POPUP" }, { WS_CHILD,"WS_CHILD" }, { WS_VISIBLE,"WS_VISIBLE" }, ;
    { WS_DISABLED,"WS_DISABLED" }, { WS_CLIPSIBLINGS,"WS_CLIPSIBLINGS" }, { WS_BORDER,"WS_BORDER" }, ;
    { WS_DLGFRAME,"WS_DLGFRAME" }, { WS_VSCROLL,"WS_VSCROLL" }, { WS_HSCROLL,"WS_HSCROLL" }, ;
    { WS_SYSMENU,"WS_SYSMENU" }, { WS_THICKFRAME,"WS_THICKFRAME" }, { WS_GROUP,"WS_GROUP" }, ;
    { WS_TABSTOP,"WS_TABSTOP" }, { BS_PUSHBUTTON,"BS_PUSHBUTTON" }, { BS_CHECKBOX,"BS_CHECKBOX" }, ;
    { BS_AUTORADIOBUTTON,"BS_AUTORADIOBUTTON" }, { ES_AUTOHSCROLL,"ES_AUTOHSCROLL" }, ;
    { ES_AUTOVSCROLL,"ES_AUTOVSCROLL" }, { ES_MULTILINE,"ES_MULTILINE" }, { BS_GROUPBOX,"BS_GROUPBOX" }, ;
    { CBS_DROPDOWNLIST,"CBS_DROPDOWNLIST" }, { SS_OWNERDRAW,"SS_OWNERDRAW" }  }

CLASS HFormGen INHERIT HObject

   CLASS VAR aForms INIT {}
   DATA oDlg
   DATA name
   DATA filename, path
   DATA type  INIT 1
   DATA lGet  INIT .T.
   DATA script1 INIT ""
   DATA script2 INIT ""
   DATA oCtrlSelected
   DATA lChanged  INIT .F.

   METHOD New() CONSTRUCTOR
   METHOD Open() CONSTRUCTOR
   METHOD AddForm()
   METHOD Save( lAs )
   METHOD End()

ENDCLASS

METHOD New() CLASS HFormGen
Local oDlg, i := 1, name

   DO WHILE .T.
      name := "Form"+Ltrim(Str(i))
      IF Ascan( ::aForms,{|o|o:name==name} ) == 0
        Exit
      ENDIF
      i ++
   ENDDO

   ::name     := name
   ::oDlg     := CreateDialog( Self )
   ::filename := ""

   ::AddForm( Self )

Return Self

METHOD Open()  CLASS HFormGen
Local aControls
   IF FileDlg( Self,.T. )
      IF ( aControls := ReadForm( Self ) ) != Nil
         ::oDlg     := CreateDialog( Self,aControls )
         ::AddForm( Self )
      ELSE
         Return Nil
      ENDIF
   ENDIF
RETURN Self

METHOD AddForm() CLASS HFormGen
   Aadd( ::aForms, Self )
RETURN Nil

METHOD End() CLASS HFormGen
Local i, name := ::name

   IF ::lChanged
      IF MsgYesNo( ::name + " was changed. Save it ?" )
         ::Save()
      ENDIF
   ENDIF
   IF ( i := Ascan( ::aForms,{|o|o:name==name} ) ) > 0
      Adel( ::aForms,i )
      Asize( ::aForms, Len(::aForms)-1 )
   ENDIF
   ShowCtrlPos()
   EndDialog( ::oDlg:handle )

RETURN .T.

METHOD Save( lAs ) CLASS HFormGen

   IF !::lChanged .AND. ( lAs==Nil .OR. !lAs )
      MsgStop( "Nothing to save" )
      Return Nil
   ENDIF

   IF Empty( ::filename ) .OR. ( lAs!=Nil .AND. lAs )
      IF FileDlg( Self,.F. )
         SaveRFile( Self )
      ENDIF
   ELSE
      SaveRFile( Self )
   ENDIF
   ::lChanged := .F.

RETURN Nil

Static Function FileDlg( oFrm,lOpen )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local oEdit1, oEdit2
Local nType := 1, fname := Iif( lOpen,"",oFrm:filename )
Local formname := Iif( lOpen,"",oFrm:name )
Local cExten

   IF !lOpen .AND. !Empty(fname)
      cExten := Lower(FilExten(fname))
      nType := Iif( cExten=="prg",3,Iif( cExten=="rc",2,1 ) )
   ENDIF

   INIT DIALOG oDlg TITLE Iif( lOpen,"Open form","Save form" ) ;
       AT 50, 100 SIZE 310,300 FONT oFont

   @ 10,15 GROUPBOX "" SIZE 130, 85

   GET RADIOGROUP nType
   @ 20,35 RADIOBUTTON "Form (*.frm)"     SIZE 110, 20 ON CLICK {||Iif(lOpen,.F.,(fname:=CutExten(fname)+".frm",oEdit1:Refresh()))}
   @ 20,55 RADIOBUTTON "Resource (*.rc)"  SIZE 110, 20 ON CLICK {||Iif(lOpen,.F.,(fname:=CutExten(fname)+".rc",oEdit1:Refresh()))}
   @ 20,75 RADIOBUTTON "Program (*.prg)"  SIZE 110, 20 ON CLICK {||Iif(lOpen,.F.,(fname:=CutExten(fname)+".prg",oEdit1:Refresh()))}
   END RADIOGROUP
   
   @ 10,110 GET oEdit1 VAR fname  ;
        STYLE ES_AUTOHSCROLL      ;
        SIZE 200, 26
 
   @ 210,110 BUTTON "Browse" SIZE 80, 26   ;
        ON CLICK {||BrowFile(lOpen,nType,oEdit1,oEdit2)}

   @ 10,150 SAY "Form name:" SIZE 80,22

   @ 10,175 GET oEdit2 VAR formname SIZE 140, 26

   @ 20,240 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 180,240 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   oDlg:Activate()

   IF oDlg:lResult
      oFrm:type := nType
      oFrm:filename := CutPath( fname )
      IF Empty( FilExten( oFrm:filename ) )
         oFrm:filename += Iif( nType==1,".frm",Iif( nType==2,".rc",".prg" ) )
      ENDIF
      oFrm:path := Iif( Empty( FilePath(fname) ), mypath, FilePath(fname) )
      oFrm:name := formname
      Return .T.
   ENDIF

Return .F.

Static Function BrowFile( lOpen,nType,oEdit1, oEdit2 )
Local fname, s1, s2
   IF nType == 1
      s1 := "Form files( *.frm )"
      s2 := "*.frm"
   ELSEIF nType == 2
      s1 := "Resource files( *.rc )"
      s2 := "*.rc"
   ELSE
      s1 := "Program files( *.prg )"
      s2 := "*.prg"
   ENDIF
   IF lOpen
      fname := SelectFile( s1, s2,mypath )
   ELSE
      fname := SaveFile( s2,s1,s2,mypath )
   ENDIF
   IF !Empty( fname )
      mypath := FilePath( fname )
      fname := CutPath( fname )
      // Eval( oEdit1:bSetGet,fname )
      oEdit1:SetGet( fname )
      oEdit1:Refresh()
      // SetDlgItemText( oEdit1:oParent:handle, oEdit1:id, fname )
      SetFocus( oEdit2:handle )
   ENDIF
Return Nil


Static Function CreateDialog( oFrm,aControls )
Local oDlg, i, oFont
Local x := aTermMetr[1]-500, y := 120, nWidth := 500, nHeight := 400, nStyle, lClipper := .F.
Local cFont

   IF aControls != Nil
      NextItem( aControls[1],.T. )
      NextItem( aControls[1] )
      x := Val( NextItem( aControls[1] ) )
      y := Val( NextItem( aControls[1] ) )
      nWidth := Val( NextItem( aControls[1] ) )
      nHeight := Val( NextItem( aControls[1] ) )
      nStyle := Val( NextItem( aControls[1] ) )
      oFrm:lGet := ( Upper( NextItem( aControls[1] ) ) == "T" )
      lClipper := ( Upper( NextItem( aControls[1] ) ) == "T" )
      cFont := NextItem( aControls[1] )

      IF !Empty( cFont )
         oFont := HFont():Add( NextItem( cFont,.T.,"," ), ;
               Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
               Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
               Val(NextItem( cFont,,"," )),Val(NextItem( cFont,,"," )), ;
               Val(NextItem( cFont,,"," )) )
      ENDIF
   ENDIF

   IF oFont == Nil
      oFont := HFont():Add( "MS Sans Serif",0,-13 )
   ENDIF

   INIT DIALOG oDlg TITLE oFrm:name              ;
          AT x,y  SIZE nWidth,nHeight            ;
          FONT oFont                             ;
          ON SIZE  {|o,h,w|oDlg:nWidth:=w,oDlg:nHeight:=h,oFrm:lChanged:=.T.} ;
          ON PAINT {|o|PaintDlg(o)}              ;
          ON EXIT  {|o|EndDlg(o)}                ;
          ON GETFOCUS {|o|oDlgCurrent:=o}        ;
          ON OTHER MESSAGES {|o,m,wp,lp|MessagesProc(o,m,wp,lp)}

   oDlg:lClipper := lClipper
   oDlg:oParent := oFrm

   IF aControls != Nil
      FOR i := 2 TO Len( aControls )
         String2Ctrl( oDlg,aControls[i] )
      NEXT
   ENDIF

   ACTIVATE DIALOG oDlg NOMODAL

   // MoveWindow( oDlg:handle,aTermMetr[1]-500,120,500,400 )

Return oDlg

Static Function EndDlg( oDlg )
   IF !Empty( HFormGen():aForms )
     oDlgCurrent := HFormGen():aForms[1]:oDlg
     SetFocus( oDlgCurrent:handle )
   ENDIF
Return .T.

Static Function SaveRFile( oFrm )
LOCAL strbuf := Space(512), poz := 513, stroka, nMode := 0
Local han, hanOut, isOut := .F., res := .F.
Local fname := oFrm:path + oFrm:filename, itemName
Local lPrg := ( Upper(FilExten(fname))=="PRG" )
Local lRc := ( Upper(FilExten(fname))=="RC" )

   IF File( fname )
      han := FOPEN( fname, FO_READWRITE + FO_EXCLUSIVE )
      IF han <> - 1
         hanOut := FCREATE( oFrm:path+"__frm.tmp" )
         IF hanOut <> - 1
            DO WHILE .T.
               stroka := RDSTR( han,@strbuf,@poz,512 )
               IF LEN( stroka ) = 0
                  EXIT
               ENDIF
               IF nMode == 0
                  IF ( lPrg .AND. Upper( Left( stroka,8 ) ) == "FUNCTION" ) ;
                        .OR. ( !lPrg .AND. Left( stroka,1 ) == "#" .AND.    ;
                             Upper( Substr( stroka,2,4 ) ) == "FORM" )      ;
                        .OR. ( lRc .AND. Upper(itemName:=NextItem(stroka,.T.," ")) == Upper(oFrm:name) ;
                                    .AND. NextItem(stroka,," ") == "DIALOG" )
                     stroka := Ltrim( Substr(stroka,7) )
                     IF lRc .OR. Upper( itemName := NextItem( stroka,.T. ) ) == Upper( oFrm:name )
                        nMode := 1
                        isOut := .T.
                        LOOP
                     ENDIF
                  ENDIF
                  Fwrite( hanOut,stroka+Iif(Asc(Right(stroka,1))<20,"",Chr(10)) )
               ELSEIF nMode == 1
                  IF ( lPrg .AND. Left( stroka,6 ) == "RETURN" )       ;
                      .OR. ( !lPrg .AND. Left( stroka,1 ) == "#" .AND. ;
                          Upper( Substr( stroka,2,7 ) ) == "ENDFORM" ) ;
                      .OR. ( lRc .AND. Left( stroka,1 ) == "}" )
                     nMode := 0
                     IF lPrg
                        WriteToPrg( oFrm,hanOut )
                     ELSEIF lRc
                        WriteToRc( oFrm,hanOut )
                     ELSE
                        WriteForm( oFrm,hanOut )
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            IF isOut
               Fclose( hanOut )
               Fclose( han )
               IF Ferase( fname ) == -1 .OR. Frename( oFrm:path+"__frm.tmp",fname ) == -1
                  MsgStop( "Can't rename __frm.tmp" )
               ELSE
                  res := .T.
               ENDIF
            ELSE
               Fclose( hanOut )
               Ferase( oFrm:path+"__frm.tmp" )
               Fseek( han,0,FS_END )
               Fwrite( han, Chr(10 ) )
               IF lPrg
                  WriteToPrg( oFrm,han )
               ELSEIF lRc
                  WriteToRc( oFrm,han )
               ELSE
                  WriteForm( oFrm,han )
               ENDIF
               Fclose( han )
               res := .T.
            ENDIF
         ELSE
            MsgStop( "Can't create __frm.tmp" )
            Fclose( han )
         ENDIF
      ELSE
         MsgStop( "Can't open "+fname )
      ENDIF
   ELSE
      han := Fcreate( fname )
      IF lPrg
         WriteToPrg( oFrm,han )
      ELSEIF lRc
         WriteToRc( oFrm,han )
      ELSE
         WriteForm( oFrm,han )
      ENDIF
      Fwrite( han, Chr(10 ) )
      Fclose( han )
      res := .T.
   ENDIF

Return res

Static Function String2Style( stroka,c )
Local nStyle := 0, item := NextItem( stroka,.T.,c )
Local i

   // writelog( "String2Style: " + stroka )
   DO WHILE !Empty(item)
      // writelog( ": /" + item + "/" )
      IF ( i := Ascan( aStyles,{|a|a[2]==item} ) ) != 0
         nStyle := Hwg_BitOr( nStyle,aStyles[i,1] )
      ELSEIF ( i := Ascan( aStaticTypes,{|a|a[2]==item} ) ) != 0
         nStyle := Hwg_BitOr( nStyle,aStaticTypes[i,1] )
      ENDIF
      item := NextItem( stroka,,c )
   ENDDO
Return nStyle

Static Function Style2String( o,lPrg )
Local lShort, stroka := "0", nStyle, i
Local c := Iif( lPrg,"+","|" )

   lShort := lPrg .AND. __ObjHasMsg( o,"ID" )
   IF Hwg_BitAnd( o:style,WS_POPUP ) != 0
      stroka += " "+c+" WS_POPUP"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_CHILD ) != 0
      IF !lShort
         stroka += " "+c+" WS_CHILD"
      ENDIF
   ENDIF
   IF Hwg_BitAnd( o:style,WS_VISIBLE ) != 0
      IF !lShort
         stroka += " "+c+" WS_VISIBLE"
      ENDIF
   ENDIF
   IF Hwg_BitAnd( o:style,WS_DISABLED ) != 0
      stroka += " "+c+" WS_DISABLED"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_CLIPSIBLINGS ) != 0
      stroka += " "+c+" WS_CLIPSIBLINGS"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_BORDER ) != 0
      IF !lShort .AND. !( "EDIT" $ o:classname() )
         stroka += " "+c+" WS_BORDER"
      ENDIF
   ENDIF
   IF Hwg_BitAnd( o:style,WS_DLGFRAME ) != 0
      stroka += " "+c+" WS_DLGFRAME"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_VSCROLL ) != 0
      stroka += " "+c+" WS_VSCROLL"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_HSCROLL ) != 0
      stroka += " "+c+" WS_HSCROLL"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_SYSMENU ) != 0
      stroka += " "+c+" WS_SYSMENU"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_THICKFRAME ) != 0
      stroka += " "+c+" WS_THICKFRAME"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_GROUP ) != 0
      stroka += " "+c+" WS_GROUP"
   ENDIF
   IF Hwg_BitAnd( o:style,WS_TABSTOP ) != 0
      stroka += " "+c+" WS_TABSTOP"
   ENDIF

   IF o:classname() == "HSTATIC"
      nStyle := Hwg_BitAnd( o:style,31 )
      IF ( i := Ascan( aStaticTypes,{|a|a[1]==nStyle} ) ) != 0 .AND. ;
         ( !lShort .OR. aStaticTypes[i,2] != "SS_LEFT" )
         stroka += " "+c+" " + aStaticTypes[i,2]
      ENDIF
   ELSEIF o:classname() == "HBUTTONGEN"
      IF !lShort
         stroka += " "+c+" BS_PUSHBUTTON"
      ENDIF
   ELSEIF o:classname() == "HCHECKBOXGEN"
      IF !lShort
         stroka += " "+c+" BS_CHECKBOX"
      ENDIF
   ELSEIF o:classname() == "HRADIOBUTTONGEN"
      IF !lShort
         stroka += " "+c+" BS_AUTORADIOBUTTON"
      ENDIF
   ELSEIF o:classname() == "HEDITGEN"
      IF Hwg_BitAnd( o:style,ES_AUTOHSCROLL ) != 0
         stroka += " "+c+" ES_AUTOHSCROLL"
      ENDIF
      IF Hwg_BitAnd( o:style,ES_AUTOVSCROLL ) != 0
         stroka += " "+c+" ES_AUTOVSCROLL"
      ENDIF
      IF Hwg_BitAnd( o:style,ES_MULTILINE ) != 0
         stroka += " "+c+" ES_MULTILINE"
      ENDIF
   ELSEIF o:classname() == "HGROUP"
      IF !lShort
         stroka += " "+c+" BS_GROUPBOX"
      ENDIF
   ELSEIF o:classname() == "HUPDOWNGEN"
      IF !lShort
         stroka += " "+c+" UDS_SETBUDDYINT | UDS_ALIGNRIGHT"
      ENDIF
   ELSEIF o:classname() == "HCOMBOGEN"
      IF !lShort
         stroka += " "+c+" CBS_DROPDOWNLIST"
      ENDIF
   ELSEIF o:classname() == "HLINE"
      IF !lShort
         stroka += " "+c+" SS_OWNERDRAW"
      ENDIF
   ENDIF

   IF !Empty( stroka )
      stroka := Substr( stroka,5 )
   ENDIF

Return stroka

Static Function Rc2String( cRc,baseX,baseY )
Local stroka, itemName := NextItem( cRc,.T.," " ), title, id
Local x1, y1, width, height, style, i

   // writelog( cRc + "/" + itemName + "/" )
   IF itemName == "CONTROL"
      title := NextItem( cRc,,"," )
      cRc := NextItem( cRc,,"?" )
      IF '"STATIC"' $ cRc
         stroka := "STATIC;"
      ELSEIF 'RADIOBUTTON' $ cRc
         stroka := "RADIOBUTTON;"
      ELSEIF 'CHECKBOX' $ cRc .OR. '3STATE' $ cRc
         stroka := "CHECKBOX;"
      ELSEIF '"EDIT"' $ cRc
         stroka := "EDITBOX;"
      ELSEIF '"PANEL"' $ cRc
         stroka := "PANEL;"
      ELSEIF '"BROWSE"' $ cRc
         stroka := "BROWSE;"
      ELSEIF '"OWNERBUTTON"' $ cRc
         stroka := "OWNERBUTTON;"
      ENDIF
      IF Left( title,1 ) == '"'
         id := NextItem( cRc,.T.,"," )
         itemName := NextItem( cRc,,"," )
         IF stroka == Nil
            stroka := itemName +";"
         ENDIF
         stroka += Substr( title,2,Len(title)-2 ) + ";"
      ELSE
         id := title
         itemName := NextItem( cRc,,"," )
         IF stroka == Nil
            stroka := itemName +";"
         ENDIF
         stroka += ";"
      ENDIF
      style := NextItem( cRc,,"," )
      x1 := NextItem( cRc,,"," )
      y1 := NextItem( cRc,,"," )
      width := NextItem( cRc,,"," )
      height := NextItem( cRc,,"," )
   ELSE
      i := Ascan( aRcNames, {|a|a[1]==itemName} )
      IF i == 0 .OR. Empty( aRcNames[i,2] )
         Return ""
      ENDIF
      stroka := aRcNames[i,2] + ";"

      title := NextItem( cRc,,"," )
      IF Left( title,1 ) == '"'
         stroka += Substr( title,2,Len(title)-2 ) + ";"
         id := NextItem( cRc,,"," )
      ELSE
         stroka += ";"
         id := title
      ENDIF
      x1 := NextItem( cRc,,"," )
      y1 := NextItem( cRc,,"," )
      width := NextItem( cRc,,"," )
      height := NextItem( cRc,,"," )
      style := NextItem( cRc,,"," )
   ENDIF
   id := Val( id )
   IF id == -1
      id := 0
   ENDIF
   x1 := Ltrim( Str( Round( (Val(x1)*baseX)/4,0 ) ) )
   y1 := Ltrim( Str( Round( (Val(y1)*baseY)/8,0 ) ) )
   width := Ltrim( Str( Round( (Val(width)*baseX)/4,0 ) ) )
   height := Ltrim( Str( Round( (Val(height)*baseY)/8,0 ) ) )
   // writelog( "Rc2:"+style+":" )
   stroka += Ltrim( Str(id) ) + ";" + x1 + ";" + y1 + ";" + width + ";" + height + ";" + ;
             Ltrim( Str( String2Style( style,"|" ) ) ) + ";;;"

   // writelog( stroka )
Return stroka

Static Function Ctrl2Rc( oCtrl,baseX,baseY )
Local cStyle := Style2String( oCtrl,.F. ), stroka, id := oCtrl:id
Local class := oCtrl:winclass

   IF "SS_LEFT" $ cStyle
      stroka := "LTEXT"
   ELSEIF "SS_RIGHT" $ cStyle
      stroka := "RTEXT"
   ELSEIF "SS_CENTER" $ cStyle
      stroka := "CTEXT"
   ELSEIF "BS_GROUPBOX" $ cStyle
      stroka := "GROUPBOX"
   ELSEIF "BS_DEFPUSHBUTTON" $ cStyle
      stroka := "DEFPUSHBUTTON"
   ELSEIF "BS_PUSHBUTTON" $ cStyle
      stroka := "PUSHBUTTON"
   ELSEIF "BS_CHECKBOX" $ cStyle
      stroka := "CHECKBOX"
   ELSEIF oCtrl:classname() == "HEDITGEN"
      stroka := "EDITTEXT"
   ENDIF
   IF id >= CONTROL_FIRST_ID .AND. class == "STATIC"
      id := -1
   ENDIF
   IF stroka != Nil
      Return stroka + ' "' + oCtrl:title + '", ' + Ltrim(Str(id)) + ", " + ;
           Ltrim( Str(Round((oCtrl:nLeft*4)/baseX,0)) )   + "," +   ;
           Ltrim( Str(Round((oCtrl:nTop*8)/baseY,0)) )    + "," +   ;
           Ltrim( Str(Round((oCtrl:nWidth*4)/baseX,0)) )  + "," +   ;
           Ltrim( Str(Round((oCtrl:nHeight*8)/baseY,0)) ) + "," + cstyle
             
   ENDIF
   IF oCtrl:classname() == "HPANELGEN"
      class := "PANEL"
   ELSEIF oCtrl:classname() == "HBROWSEGEN"
      class := "BROWSE"
   ELSEIF oCtrl:classname() == "HOWNBUTGEN"
      class := "OWNERBUTTON"
   ENDIF

Return 'CONTROL "' + oCtrl:title + '", ' + Ltrim(Str(oCtrl:id)) + ', "' ;
           + class + '", ' + cStyle + "," +   ;
           Ltrim( Str(Round((oCtrl:nLeft*4)/baseX,0)) )   + "," +   ;
           Ltrim( Str(Round((oCtrl:nTop*8)/baseY,0)) )    + "," +   ;
           Ltrim( Str(Round((oCtrl:nWidth*4)/baseX,0)) )  + "," +   ;
           Ltrim( Str(Round((oCtrl:nHeight*8)/baseY,0)) )

Static Function Ctrl2Prg( oCtrl )
Local stroka := "   @ ", classname, cStyle

   IF oCtrl == Nil
      Return ""
   ENDIF

   classname := oCtrl:classname()
   i := Ascan( aCtrlNames, {|a|a[1]==classname} )

   stroka += Ltrim( Str(oCtrl:nLeft) ) + "," + Ltrim( Str(oCtrl:nTop) ) + " "

   IF classname == "HPANELGEN" .OR. classname == "HOWNBUTGEN" .OR. ;
         classname == "HBROWSEGEN" .OR. classname == "HDATEGEN"
      stroka += aCtrlNames[i,2] + " "
   ELSEIF classname == "HUPDOWNGEN"
      stroka += 'UPDOWN 0 '
   ELSEIF classname == "HCOMBOGEN"
      stroka += 'COMBOBOX {} '
   ELSEIF classname == "HLINE"
      stroka += 'LINE LENGTH '
      IF oCtrl:lVert
         stroka += Ltrim(Str(oCtrl:nHeight)) + " VERTICAL "
      ELSE
         stroka += Ltrim(Str(oCtrl:nWidth)) + " "
      ENDIF
   ELSEIF classname == "HSTATIC"
      stroka += 'SAY "' + oCtrl:title + '" '
   ELSE
      stroka += aCtrlNames[i,2] + ' "' + oCtrl:title + '" '
   ENDIF

   stroka +=  "SIZE " + Ltrim( Str(oCtrl:nWidth) ) + "," + Ltrim( Str(oCtrl:nHeight) ) + " "
   cStyle := Style2String( oCtrl,.T. )
   IF !Empty( cStyle )
      stroka += "STYLE " + cStyle + " "
   ENDIF
   IF oCtrl:tcolor != Nil .AND. oCtrl:tcolor != 0
      stroka += Iif( Empty(cStyle),"",";" + Chr(10) + Space(8) ) + ;
           "COLOR " + Ltrim( Str(oCtrl:tcolor) ) + " "
   ENDIF
   IF oCtrl:bcolor != Nil
      stroka += "BACKCOLOR " + Ltrim( Str(oCtrl:bcolor) )
   ENDIF

Return stroka

Static Function WriteToRc( oFrm, han )
Local aControls := oFrm:oDlg:aControls
Local alen := Len( aControls ), i
Local baseX := LoWord(DialogBaseUnits()), baseY := HiWord(DialogBaseUnits())

   Fwrite( han, oFrm:name + " DIALOG " + ;
       Ltrim( Str(Round((oFrm:oDlg:nLeft*4)/baseX,0)) )   + "," + ;
       Ltrim( Str(Round((oFrm:oDlg:nTop*8)/baseY,0)) )    + "," + ;
       Ltrim( Str(Round((oFrm:oDlg:nWidth*4)/baseX,0)) )  + "," + ;
       Ltrim( Str(Round((oFrm:oDlg:nHeight*8)/baseY,0)) ) + Chr(10) )
   Fwrite( han, "STYLE " + Style2String( oFrm:oDlg,.F. ) + Chr(10) )
   Fwrite( han, 'CAPTION "' + oFrm:name + '"' + Chr(10) )
   Fwrite( han, "BEGIN"+ Chr(10) )

   FOR i := 1 TO alen
      Fwrite( han, " " + Ctrl2Rc( aControls[i],baseX,baseY ) + Chr(10) )
   NEXT
   Fwrite( han, "END"+ Chr(10) )

Return

Static Function WriteToPrg( oFrm, han )
Local aControls := oFrm:oDlg:aControls
Local alen := Len( aControls ), i

   Fwrite( han, "FUNCTION " + oFrm:name + Chr(10)  )
   Fwrite( han, "LOCAL oDlg" + Chr(10) + Chr(10) )
   Fwrite( han, "/* Designer: declaration of controls */" + Chr(10) )
   Fwrite( han, '   INIT DIALOG oDlg TITLE "' + oFrm:name + '" ;' + Chr(10) )
   Fwrite( han, Space(8) + "AT " + Ltrim( Str(oFrm:oDlg:nLeft) ) + "," ;
      + Ltrim( Str(oFrm:oDlg:nTop) ) + " SIZE " + ;
      Ltrim( Str(oFrm:oDlg:nWidth) ) + "," + Ltrim( Str(oFrm:oDlg:nHeight) ) + Chr(10) )

   FOR i := 1 TO alen
      Fwrite( han, Ctrl2Prg( aControls[i] ) + Chr(10) )
   NEXT

   Fwrite( han, "   ACTIVATE DIALOG oDlg" + Chr(10) )
   Fwrite( han, "RETURN" )

Return

Static Function WriteForm( oFrm, han )
Local aControls := oFrm:oDlg:aControls
Local alen := Len( aControls ), i

   Fwrite( han, "#FORM " + oFrm:name ;
       + ";" + Ltrim( Str(oFrm:oDlg:nLeft) )    ;
       + ";" + Ltrim( Str(oFrm:oDlg:nTop) )     ;
       + ";" + Ltrim( Str(oFrm:oDlg:nWidth) )   ;
       + ";" + Ltrim( Str(oFrm:oDlg:nHeight ) ) ;
       + ";" + Ltrim( Str(oFrm:oDlg:style) )    ;
       + ";" + Iif(oFrm:lGet,"T","F")           ;
       + ";" + Iif(oFrm:oDlg:lClipper,"T","F")  ;
       + ";" + Iif(oFrm:oDlg:oFont!=Nil,        ;
       oFrm:oDlg:oFont:name + "," + Ltrim(Str(oFrm:oDlg:oFont:width)) ;
       + "," + Ltrim(Str(oFrm:oDlg:oFont:height)) + "," + Ltrim(Str(oFrm:oDlg:oFont:weight)) ;
       + "," + Ltrim(Str(oFrm:oDlg:oFont:charset)) + "," + Ltrim(Str(oFrm:oDlg:oFont:italic));
       + "," + Ltrim(Str(oFrm:oDlg:oFont:underline)) + "," + Ltrim(Str(oFrm:oDlg:oFont:strikeout)) ;
       ,"")  ;
       + Chr(10) )
   WriteScript( han,oFrm:script1 )
   FOR i := 1 TO alen
      Fwrite( han, Ctrl2String( aControls[i] ) + Chr(10) )
      IF aControls[i]:Cargo[2] .OR. aControls[i]:Cargo[4]
         Fwrite( han, "  #CODE " + ;
           aControls[i]:Cargo[1] + ";" + Iif(aControls[i]:Cargo[2],"T;","F;") + ;
           aControls[i]:Cargo[3] + ";" + Iif(aControls[i]:Cargo[4],"T;","F;") + ;
           aControls[i]:Cargo[5] + ";" + ;
           aControls[i]:Cargo[6] + ";" + Chr(10) )
      ENDIF
   NEXT
   WriteScript( han,oFrm:script2 )
   Fwrite( han, "#ENDFORM " )

Return

Static Function WriteScript( han,cScript,lPrg )
Local poz := 0, stroka, i
Local lastC := Chr(10), cQuote, lFirst := .T.

   IF lPrg == Nil; lPrg := .F.; ENDIF
   IF cScript != Nil .AND. !Empty( cScript )
      IF !lPrg
         Fwrite( han,"#SCRIPT"+Chr(10) )
      ENDIF
      DO WHILE .T.
         stroka := RDSTR( , cScript, @poz )
         IF LEN( stroka ) = 0
            IF lPrg
               Fwrite( han,Chr(10) )
            ENDIF
            EXIT
         ENDIF
         IF Left( stroka,1 ) != Chr(10)
            IF lPrg
               cQuote := Iif(!( '"' $ stroka),'"', ;
                           Iif(!( "'" $ stroka),"'","["))
               Fwrite( han,Iif(lFirst,"",";"+Chr(10))+Space(5)+;
                     Iif(lFirst,"","+ ")+cQuote+stroka+cQuote+"+cEnd" )
               lFirst := .F.
            ELSE
               Fwrite( han,Iif( Asc(lastC)<20,"",Chr(10) )+stroka )
               lastC := Right( stroka,1 )
            ENDIF
         ENDIF
      ENDDO
      IF !lPrg
         Fwrite( han, Iif( Asc(lastC)<20,"",Chr(10) )+"#ENDSCRIPT"+Chr(10) )
      ENDIF
   ENDIF
Return Nil


Function Ctrl2String( oCtrl )
Local j
   IF oCtrl == Nil
      Return ""
   ENDIF
   j := Ascan( aCtrlNames, {|a|a[1]==oCtrl:classname()} )
   IF oCtrl:classname() == "HLINE" .AND. oCtrl:lVert
      j ++
   ENDIF
Return aCtrlNames[j,2] + ";" + Rtrim( oCtrl:title) ;
       + ";" + Ltrim( Str(Iif(oCtrl:id<34000,oCtrl:id,0)) ) ;
       + ";" + Ltrim( Str(oCtrl:nLeft) )    ;
       + ";" + Ltrim( Str(oCtrl:nTop) )     ;
       + ";" + Ltrim( Str(oCtrl:nWidth) )   ;
       + ";" + Ltrim( Str(oCtrl:nHeight ) ) ;
       + ";" + Ltrim( Str(oCtrl:style) )    ;
       + ";" + Iif(oCtrl:oFont!=Nil,        ;
       oCtrl:oFont:name + "," + Ltrim(Str(oCtrl:oFont:width));
       + "," + Ltrim(Str(oCtrl:oFont:height)) + "," + Ltrim(Str(oCtrl:oFont:weight)) ;
       + "," + Ltrim(Str(oCtrl:oFont:charset)) + "," + Ltrim(Str(oCtrl:oFont:italic));
       + "," + Ltrim(Str(oCtrl:oFont:underline)) + "," + Ltrim(Str(oCtrl:oFont:strikeout)) ;
       ,"")  ;
       + ";" + Iif(oCtrl:tcolor!=Nil.AND.oCtrl:tcolor!=0,Ltrim(Str(oCtrl:tcolor)),"") ;
       + ";" + Iif(oCtrl:bcolor!=Nil,Ltrim(Str(oCtrl:bcolor)),"")

Function String2Ctrl( oDlg, stroka, xPos, yPos )
Local oCtrl, aCtrl, oFont, strCode
Local cn, i, aControls, j

   IF Empty( stroka )
      Return Nil
   ENDIF

   IF ( i := At( Chr(1),stroka ) ) != 0
      strCode := Substr( stroka,i+1 )
      stroka  := Left( stroka,i-1 )
   ELSE
      strCode := ""
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

   // writelog( "--> " + stroka + " -- " + aCtrl[1] )
   IF xPos != Nil
      aCtrl[4] := xPos
      aCtrl[5] := yPos
   ENDIF
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
      cn := "Say"
   ELSEIF aCtrl[1] == "BUTTON"
      oCtrl := HButtonGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Btn"
   ELSEIF aCtrl[1] == "GROUPBOX"
      oCtrl := HGroup():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,,,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Group"
   ELSEIF aCtrl[1] == "EDITBOX"
      oCtrl := HEditGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Edit"
   ELSEIF aCtrl[1] == "CHECKBOX"
      oCtrl := HCheckBoxGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Check"
   ELSEIF aCtrl[1] == "RADIOBUTTON"
      oCtrl := HRadioButtonGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Radio"
   ELSEIF aCtrl[1] == "DATEPICKER"
      oCtrl := HDateGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],,oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Date"
   ELSEIF aCtrl[1] == "UPDOWN"
      oCtrl := HUpDownGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "UpDown"
   ELSEIF aCtrl[1] == "COMBOBOX"
      oCtrl := HComboGen():New( oDlg,aCtrl[3],aCtrl[8], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7],aCtrl[2],oFont,, ;
          Iif(Empty(aCtrl[10]),Nil,Val(aCtrl[10])),Iif(Empty(aCtrl[11]),Nil,Val(aCtrl[11]) ) )
      cn := "Combo"
   ELSEIF aCtrl[1] == "HLINE"
      oCtrl := HLine():New( oDlg,aCtrl[3],.F., ;
          aCtrl[4],aCtrl[5],aCtrl[6] )
      cn := "Hline"
   ELSEIF aCtrl[1] == "VLINE"
      oCtrl := HLine():New( oDlg,aCtrl[3],.T., ;
          aCtrl[4],aCtrl[5],aCtrl[7] )
      cn := "Vline"
   ELSEIF aCtrl[1] == "PANEL"
      oCtrl := HPanelGen():New( oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
      cn := "Panel"
   ELSEIF aCtrl[1] == "OWNERBUTTON"
      oCtrl := HOwnButGen():New( oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
      cn := "Ownbtn"
   ELSEIF aCtrl[1] == "BROWSE"
      oCtrl := HBrowseGen():New( oDlg,aCtrl[3], ;
          aCtrl[4],aCtrl[5],aCtrl[6],aCtrl[7] )
      cn := "Browse"
   ENDIF
   IF oCtrl != Nil
      IF Empty( strCode )
         aControls := oDlg:aControls
         j := 0
         FOR i := 1 TO Len( aControls ) - 1
            IF aControls[i]:classname() == oCtrl:classname()
               j ++
            ENDIF
         NEXT
         cn += Ltrim( Str(++j) )
         oCtrl:Cargo := { "o"+cn,.F.,"var"+cn,.F.,"","" }
      ELSE
         oCtrl:Cargo := { NextItem(strCode,.T.),(Upper(NextItem(strCode))=="T"), ;
            NextItem(strCode),(Upper(NextItem(strCode))=="T"), ;
            NextItem(strCode),NextItem(strCode) }
      ENDIF
   ENDIF

Return oCtrl

Static Function ReadForm( oFrm )
LOCAL strbuf := Space(512), poz := 513, stroka, nMode := 0
Local han := FOPEN( oFrm:path+oFrm:filename, FO_READ + FO_SHARED )
Local lPrg := ( Upper( FilExten(oFrm:filename) )=="PRG" )
Local lRc := ( Upper( FilExten(oFrm:filename) )=="RC" )
Local aControls := {}, itemName, i
Local baseX := LoWord(DialogBaseUnits()), baseY := HiWord(DialogBaseUnits())

   IF han == - 1
      MsgStop( "Can't open "+oFrm:path+oFrm:filename )
      Return Nil
   ENDIF
   DO WHILE .T.
      stroka := RDSTR( han,@strbuf,@poz,512 )
      IF LEN( stroka ) = 0
         EXIT
      ENDIF
      stroka := Ltrim( stroka )
      IF ( lRc .OR. lPrg ) .AND. Left( stroka,2 ) == "//"
         LOOP
      ENDIF
      IF lRc .AND. Left( stroka,1 ) == "#"
         IF Left( stroka,7 ) == "#define"
         ELSEIF Left( stroka,8 ) == "#include"
         ENDIF
      ENDIF
      IF nMode == 0
         IF lPrg
            IF Upper( Left( stroka,8 ) ) == "FUNCTION" .AND. ;
                Upper( Ltrim( Substr( stroka,10 ) ) ) == Upper( oFrm:name )
               nMode := 10
            ENDIF
         ELSEIF lRc
            IF Upper( itemName := NextItem( stroka,.T.," " ) ) == Upper( oFrm:name ) ;
               .AND. NextItem( stroka,," " ) == "DIALOG"
               Aadd( aControls,"DIALOG;" + itemName + ";" + ;
                 Ltrim( Str( Round( (Val(NextItem( stroka,,"," ))*baseX)/4,0 ) ) ) + ";" + ;
                 Ltrim( Str( Round( (Val(NextItem( stroka,,"," ))*baseY)/8,0 ) ) ) + ";" + ;
                 Ltrim( Str( Round( (Val(NextItem( stroka,,"," ))*baseX)/4,0 ) ) ) + ";" + ;
                 Ltrim( Str( Round( (Val(NextItem( stroka,,"," ))*baseY)/8,0 ) ) ) + ";" )
               nMode := 20
            ENDIF
         ELSE
            IF Left( stroka,1 ) == "#"
               IF Upper( Substr( stroka,2,4 ) ) == "FORM"
                  stroka := Ltrim( Substr( stroka,7 ) )
                  itemName := NextItem( stroka,.T. )
                  IF Empty( oFrm:name ) .OR. Upper( itemName ) == Upper( oFrm:name )
                     IF Empty( oFrm:name )
                        oFrm:name := itemName
                     ENDIF
                     Aadd( aControls, "DIALOG;"+stroka )
                     nMode := 1
                  ENDIF
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
            IF Ascan( aCtrlNames,{|a|a[2]==itemName} ) == 0
               MsgStop( "Wrong item name: " + itemName )
               Return Nil
            ENDIF
            Aadd( aControls, stroka )
         ENDIF
      ELSEIF nMode == 2
         IF Left( stroka,1 ) == "#" .AND. Upper( Substr( stroka,2,6 ) ) == "ENDSCR"
            nMode := 1
         ELSE
            IF Len( aControls ) == 1
               oFrm:script1 += stroka+Chr(13)+chr(10)
            ELSE
               oFrm:script2 += stroka+Chr(13)+chr(10)
            ENDIF
         ENDIF
      ELSEIF nMode == 10
      ELSEIF nMode == 20
   writelog( "Readform-2: "+stroka )
         IF Left(stroka,1) == "{" .OR. Left(stroka,5) == "BEGIN"
            nMode := 21
         ELSE
            itemName := NextItem( stroka,.T.," " )
            IF itemName == "STYLE"
   writelog( "Readform-2a" )
               aControls[1] += Ltrim( Str( String2Style( NextItem( stroka,,"?" ) ) ) )
   writelog( "Readform-2b" )
            ENDIF
         ENDIF
      ELSEIF nMode == 21
   writelog( "Readform-3: "+stroka )
         IF Left(stroka,1) == "}" .OR. Left(stroka,3) == "END"
            Exit
         ELSE
            itemName := NextItem( stroka,.T.," " )
            IF Ascan( aRcNames, {|a|a[1]==itemName} ) == 0
               aControls[Len(aControls)] += stroka
            ELSE
               Aadd( aControls, stroka )
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   Fclose( han )

   IF lRc
   writelog( "Readform-10" )
      FOR i := 2 TO Len( aControls )
         aControls[i] := Rc2String( aControls[i],baseX,baseY )
      NEXT
   ENDIF

   IF Empty( aControls )
      MsgStop( oFrm:name + " not found or empty!" )
      Return Nil
   ENDIF

Return aControls

Static Function PaintDlg( oDlg )
Local pps, hDC, aCoors, oCtrl := GetCtrlSelected( oDlg )

   pps := DefinePaintStru()
   hDC := BeginPaint( oDlg:handle, pps )

   // aCoors := GetClientRect( oDlg:handle )   
   // FillRect( hDC, aCoors[1], aCoors[2], aCoors[3], aCoors[4], oDlg:brush:handle )
   IF oCtrl != Nil

      Rectangle( hDC, oCtrl:nLeft-3, oCtrl:nTop-3, ;
                  oCtrl:nLeft+oCtrl:nWidth+2, oCtrl:nTop+oCtrl:nHeight+2 )
      Rectangle( hDC, oCtrl:nLeft-1, oCtrl:nTop-1, ;
                  oCtrl:nLeft+oCtrl:nWidth, oCtrl:nTop+oCtrl:nHeight )

   ENDIF

   EndPaint( oDlg:handle, pps )

Return Nil


Static Function MessagesProc( oDlg, msg, wParam, lParam )
Local oCtrl

   // writelog( str(msg)+str(wParam)+str(lParam) )
   IF msg == WM_MOUSEMOVE
      MouseMove( oDlg, wParam, LoWord( lParam ), HiWord( lParam ) )
      Return 1
   ELSEIF msg == WM_LBUTTONDOWN
      LButtonDown( oDlg, LoWord( lParam ), HiWord( lParam ) )
      Return 1
   ELSEIF msg == WM_LBUTTONUP
      LButtonUp( oDlg, LoWord( lParam ), HiWord( lParam ) )
      Return 1
   ELSEIF msg == WM_RBUTTONUP
      RButtonUp( oDlg, LoWord( lParam ), HiWord( lParam ) )
      Return 1
   ELSEIF msg == WM_LBUTTONDBLCLK
      SetStyle( oDlg )
      Return 1
   ELSEIF msg == WM_SYSCOMMAND
      IF LoWord( wParam ) == SC_CLOSE
         oDlg:oParent:End()
         Return 1
      ENDIF
   ELSEIF msg == WM_MOVE
      oDlg:nLeft := Loword(lParam)
      oDlg:nTop  := Hiword(lParam)
      oDlg:oParent:lChanged := .T.
   ELSEIF msg == WM_KEYDOWN
      IF wParam == 46    // Del
         DeleteCtrl()
      ENDIF
   ELSEIF msg == WM_KEYUP
      IF wParam == 40        // Down
         IF ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,0,1,.F. )
         ENDIF
      ELSEIF wParam == 38    // Up
         IF ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,0,-1,.F. )
         ENDIF
      ELSEIF wParam == 39    // Right
         IF ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,1,0,.F. )
         ENDIF
      ELSEIF wParam == 37    // Left
         IF ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,-1,0,.F. )
         ENDIF
      ENDIF
   ENDIF

Return -1

Static Function MouseMove( oDlg, wParam, xPos, yPos )
Local aBDown, oCtrl, resizeDirection

   IF !Empty( addItem )
      Hwg_SetCursor( crossCursor )
   ELSE
      aBDown := GetBDown()
      IF aBDown[1] != Nil
         IF aBDown[4] > 0
            IF aBDown[4] == 1 .OR. aBDown[4] == 3
               Hwg_SetCursor( horzCursor )
            ELSEIF aBDown[4] == 2 .OR. aBDown[4] == 4
               Hwg_SetCursor( vertCursor )
            ENDIF            
            CtrlResize( aBDown[1],xPos,yPos )
         ELSE
            CtrlMove( aBDown[1],xPos,yPos,.T. )
         ENDIF
      ELSE
         IF ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
            IF ( resizeDirection := CheckResize( oCtrl,xPos,yPos ) ) == 1 .OR. resizeDirection == 3
               Hwg_SetCursor( horzCursor )
            ELSEIF resizeDirection == 2 .OR. resizeDirection == 4
               Hwg_SetCursor( vertCursor )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

Return Nil

Static Function LButtonDown( oDlg, xPos, yPos )
Local i, alen := Len( oDlg:aControls ), aControls := oDlg:aControls
Local oCtrl := GetCtrlSelected( oDlg ), resizeDirection

   IF !Empty( addItem )
      Return Nil
   ENDIF
   IF oCtrl != Nil .AND. ;
        ( resizeDirection := CheckResize( oCtrl,xPos,yPos ) ) > 0
      SetBDown( oCtrl,xPos,yPos,resizeDirection )
      IF resizeDirection == 1 .OR. resizeDirection == 3
         Hwg_SetCursor( horzCursor )
      ELSEIF resizeDirection == 2 .OR. resizeDirection == 4
         Hwg_SetCursor( vertCursor )
      ENDIF            
   ELSE
      FOR i := 1 TO alen
        oCtrl := aControls[i]
        IF xPos >= oCtrl:nLeft .AND. ;
              xPos < ( oCtrl:nLeft+oCtrl:nWidth ) .AND. ;
              yPos >= oCtrl:nTop .AND. ;
              yPos < ( oCtrl:nTop+oCtrl:nHeight )
           SetBDown( oCtrl,xPos,yPos,0 )
           Exit
        ENDIF
      NEXT
      IF i > alen .AND. ( oCtrl := GetCtrlSelected( oDlg ) ) != Nil
         SetCtrlSelected( oDlg )
      ENDIF
   ENDIF

Return Nil

Static Function LButtonUp( oDlg, xPos, yPos )
Local aBDown, oCtrl

   IF Empty( addItem )
      aBDown := GetBDown()
      IF aBDown[1] != Nil
         IF aBDown[4] > 0
            CtrlResize( aBDown[1],xPos,yPos )
         ELSE
            CtrlMove( aBDown[1],xPos,yPos,.T. )
         ENDIF
         SetBDown( Nil,0,0,0 )
      ENDIF
   ELSE 
      IF addItem == "PASTE"
         oCtrl := String2Ctrl( oDlg,cClipbrd,xPos,yPos )
      ELSE
         oCtrl := String2Ctrl( oDlg,addItem,xPos,yPos )
      ENDIF

      SetCtrlSelected( oDlg,oCtrl )
      oDlg:oParent:lChanged := .T.
      IF oBtnPressed != Nil
         oBtnPressed:Release()
      ENDIF
   ENDIF
   addItem := ""

Return -1

Function RButtonUp( oDlg, xPos, yPos )
Local i, alen := Len( oDlg:aControls ), aControls := oDlg:aControls
Local oCtrl

   IF !Empty( addItem )
      Return Nil
   ENDIF
   FOR i := 1 TO alen
     oCtrl := aControls[i]
     IF xPos >= oCtrl:nLeft .AND. ;
           xPos < ( oCtrl:nLeft+oCtrl:nWidth ) .AND. ;
           yPos >= oCtrl:nTop .AND. ;
           yPos < ( oCtrl:nTop+oCtrl:nHeight )
           SetCtrlSelected( oDlg,oCtrl )
           oCtrlMenu:Show( oDlg,xPos,yPos,.T. )
        Exit
     ENDIF
   NEXT
   IF i > aLen
      oDlgMenu:Show( oDlg,xPos,yPos,.T. )
   ENDIF

Return Nil
