/*
 * DBCHW - DBC ( Harbour + HWGUI )
 * Main file
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"
#include "dbchw.h"
#include "error.ch"
#ifdef RDD_ADS
   #include "ads.ch"
#endif

REQUEST OWNBTNPROC

Function Main()
Local aMainWindow, aPanel
Public BrwFont := {"MS Sans Serif",0,-13}, oBrwFont := Nil
PUBLIC msfile[ 15 ], msmode[ 15, 5 ], msexp[ 15 ], lenmsf := 0, improc := 0, mypath := ""
PUBLIC dformat := "dd/mm/yy", memownd := .F., prrdonly := .F.
PUBLIC lWinChar := .F.
PUBLIC nServerType := ADS_LOCAL_SERVER
PUBLIC msdriv := { "ADS_CDX", "ADS_NTX","ADS_ADT" }, numdriv := 1
PUBLIC nQueryWndHandle := 0

#ifdef RDD_ADS
   REQUEST _ADS
   rddRegister("ADS",1)
   rddSetdefault("ADS")
#else
   REQUEST _DBFCDX
   rddRegister("DBFCDX",1)
#endif

   SET EXCLUSIVE ON
   SET EPOCH TO 1960
   SET DATE FORMAT dformat

   Rdini( "dbc.ini" )
   mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )

#ifdef RDD_ADS
   IF nServerType == ADS_REMOTE_SERVER
      IF !AdsConnect( mypath )
          nServerType := ADS_LOCAL_SERVER
          MsgInfo( "Can't establish connection" )
      ENDIF
   ENDIF
   AdsSetServerType( nServerType )
   AdsRightsCheck( .F. )
   SET CHARTYPE TO OEM
   AdsSetFileType( Iif( numdriv==1,2,Iif( numdriv==2,1,3 ) ) )
#endif

   INIT WINDOW aMainWindow MDI TITLE "Dbc" MENU "APPMENU" MENUPOS 8
   MENU FROM RESOURCE OF aMainWindow        ;
       ON IDM_ABOUT   ACTION  About()       ;
       ON IDM_NEW     ACTION  StruMan(.T.)  ;
       ON IDM_OPEN    ACTION  OpenDlg()     ;
       ON IDM_CLOSE   ACTION  ChildClose()  ;
       ON IDM_FONT    ACTION  oBrwFont:=HFont():Select() ;
       ON IDM_CONFIG  ACTION  OpenConfig()  ;
       ON IDM_INDSEL  ACTION  ListIndex()   ;
       ON IDM_INDNEW  ACTION  NewIndex()    ;
       ON IDM_INDOPEN ACTION  OpenIndex()   ;
       ON IDM_INDCLOSE ACTION CloseIndex()  ;
       ON IDM_FIELMOD  ACTION StruMan(.F.)  ;
       ON IDM_LOCATE  ACTION  Move(1)       ;
       ON IDM_SEEK    ACTION  Move(2)       ;
       ON IDM_FILTER  ACTION  Move(3)       ;
       ON IDM_GOTO    ACTION  Move(4)       ;
       ON IDM_REPLACE ACTION  C_Repl()      ;
       ON IDM_DELETE  ACTION  C_Dele(1)     ;
       ON IDM_RECALL  ACTION  C_Dele(2)     ;
       ON IDM_COUNT   ACTION  C_Dele(3)     ;
       ON IDM_SUM     ACTION  C_Sum()       ;
       ON IDM_APPFROM ACTION  C_Append()    ;
       ON IDM_REINDEX ACTION  C_RPZ(1)      ;
       ON IDM_PACK    ACTION  C_RPZ(2)      ;
       ON IDM_ZAP     ACTION  C_RPZ(3)      ;
       ON IDM_SCRIPT  ACTION  Scripts(1)    ;
       ON IDM_QUENEW  ACTION  Query(.F.)    ;
       ON IDM_QUEOPEN ACTION  OpenQuery()   ;
       ON IDM_QUEEDIT ACTION  Query(.T.)    ;
       ON IDM_CALCUL  ACTION  Calcul()      ;
       ON IDM_DSCRIPT ACTION  Scripts(2)    ;
       ON IDM_EXIT    ACTION  EndWindow()   ;
       ON IDM_TILE    ACTION  SendMessage(HWindow():GetMain():handle,WM_MDITILE,MDITILE_HORIZONTAL,0) ;
       ON IDM_CASCADE ACTION  SendMessage(HWindow():GetMain():handle,WM_MDICASCADE,0,0)

/*
    @ 0,0 PANEL oPanel  SIZE 0,28

    @ 2,3 OWNERBUTTON OF oPanel ON CLICK {||OpenDlg()} ;
        SIZE 22,22 FLAT ;
        BITMAP "BMP_OPEN" FROM RESOURCE COORDINATES 0,4,0,0
*/

   IF Valtype( BrwFont ) == "A"
      oBrwFont := HFont():Add( BrwFont[1], BrwFont[2], BrwFont[3] )
   ENDIF

   EnableMenuItem( ,1, .F., .F. )
   EnableMenuItem( ,2, .F., .F. )
   EnableMenuItem( ,3, .F., .F. )
   EnableMenuItem( ,4, .F., .F. )

   aMainWindow:Activate()

Return Nil

Function ChildClose
Local nHandle := SendMessage( HWindow():GetMain():handle, WM_MDIGETACTIVE,0,0 )
   if nHandle > 0
      SendMessage( HWindow():GetMain():handle, WM_MDIDESTROY, nHandle, 0 )
   endif
Return Nil

Function About
Local oModDlg, oFont

   PREPARE FONT oFont NAME "MS Sans Serif" WIDTH 0 HEIGHT -13 ITALIC UNDERLINE

   INIT DIALOG oModDlg FROM RESOURCE TITLE "ABOUTDLG"

   REDEFINE BITMAP "BITMAP_1" FROM RESOURCE ID IDC_BMP1
   REDEFINE OWNERBUTTON ID IDC_OWNB1  ;
            ON CLICK {|| EndDialog()} ;
            FLAT TEXT "Close" COLOR Vcolor("0000FF") FONT oFont

   oModDlg:Activate()

Return Nil


/* -----------------------  Options --------------------- */

Static Function OpenConfig
Local aModDlg, aDates := { "dd/mm/yy","mm/dd/yy" }

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DIALOG_1" ON INIT {|| InitConfig() }
   REDEFINE COMBOBOX aDates OF aModDlg ID IDC_COMBOBOX3
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndConfig()}   ;
        ON BN_CLICKED,IDC_RADIOBUTTON1 ACTION {|| ServerButton(0) } ;
        ON BN_CLICKED,IDC_RADIOBUTTON2 ACTION {|| ServerButton(1) }

   aModDlg:Activate()

Return Nil

Static Function InitConfig
Local hDlg := getmodalhandle()
Local st := Iif( nServerType == ADS_REMOTE_SERVER,IDC_RADIOBUTTON2,IDC_RADIOBUTTON1 )
Local nd := Iif( numdriv==1,IDC_RADIOBUTTON3,Iif( numdriv==2,IDC_RADIOBUTTON4,IDC_RADIOBUTTON5 ) )
   CheckRadioButton( hDlg,IDC_RADIOBUTTON3,IDC_RADIOBUTTON5,nd )
   CheckRadioButton( hDlg,IDC_RADIOBUTTON1,IDC_RADIOBUTTON2,st )
   CheckDlgButton( hDlg,IDC_CHECKBOX4,SET( _SET_EXCLUSIVE ) )
   CheckDlgButton( hDlg,IDC_CHECKBOX5,prrdonly )
   CheckDlgButton( hDlg,IDC_CHECKBOX6,AdsLocking() )
   ServerButton( nServerType - 1 )
Return .T.

Static Function EndConfig()
Local hDlg := getmodalhandle()
Local new_numdriv, new_servertype, serverPath
   new_numdriv := Iif( IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON3 ), 1, ;
                  Iif( IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON4 ), 2, 3 ) )
   IF new_numdriv != numdriv
      numdriv := new_numdriv
      AdsSetFileType( Iif( numdriv==1,2,Iif( numdriv==2,1,3 ) ) )
   ENDIF
   IF SET( _SET_EXCLUSIVE ) != IsDlgButtonChecked( hDlg,IDC_CHECKBOX4 )
      SET( _SET_EXCLUSIVE, .NOT. SET( _SET_EXCLUSIVE ) )
   ENDIF
   IF prrdonly != IsDlgButtonChecked( hDlg,IDC_CHECKBOX5 )
      prrdonly := .NOT. prrdonly
   ENDIF
   IF AdsLocking() != IsDlgButtonChecked( hDlg,IDC_CHECKBOX6 )
      AdsLocking( !AdsLocking() )
   ENDIF
   dformat := GetDlgItemText( hDlg, IDC_COMBOBOX3, 12 )
   SET DATE FORMAT dformat
   new_servertype := Iif( IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON1 ), ;
                       ADS_LOCAL_SERVER, ADS_REMOTE_SERVER )
   IF new_servertype != nServerType
      nServerType := new_servertype
      AdsSetServerType( nServerType )
      IF nServerType == ADS_REMOTE_SERVER
         serverPath := GetDlgItemText( hDlg, IDC_EDIT1, 60 )
         IF Right( serverPath ) != "/" .AND. Right( serverPath ) != "\"
            serverPath += "\"
         ENDIF
         SetDlgItemText( hDlg, IDC_TEXT1, "Waiting for connection ..." )
         IF Empty( serverPath ) .OR. !AdsConnect( serverPath )
             nServerType := 1
             AdsSetServerType( nServerType )
             CheckRadioButton( hDlg,IDC_RADIOBUTTON1,IDC_RADIOBUTTON2,IDC_RADIOBUTTON1 )
             ServerButton( 0 )
             SetDlgItemText( hDlg, IDC_TEXT1, "Cannot connect to "+serverPath )
             Return .F.
         ELSE
             mypath := serverPath
         ENDIF
      ENDIF
   ENDIF
   EndDialog( hDlg )
Return .T.

Static Function ServerButton( iEnable )
Local hEdit := GetDlgItem( getmodalhandle(),IDC_EDIT1 )
   SendMessage( hEdit, WM_ENABLE, iEnable, 0 )
Return .T.

/* -----------------------  Select Order --------------------- */

Static Function ListIndex
Local oModDlg, oBrw
Local msind := { { "0","None","","" } }, i, ordlen := 0

   i := 1
   DO WHILE .NOT. EMPTY( indname := ORDNAME( i ) )
      AADD( msind, { STR( i, 1 ), indname, ORDKEY( i ), ORDBAGNAME( i ) } )
      ordlen := Max( ordlen, Len( OrdKey( i-1 ) ) )
      i ++
   ENDDO
   INIT DIALOG oModDlg FROM RESOURCE TITLE "DLG_SEL_IND"
   REDEFINE BROWSE oBrw ARRAY OF oModDlg ID ID_BROWSE   ;
       ON INIT {|o|o:rowPos:=o:tekzp:=IndexOrd()+1}       ;
       ON CLICK {|o|SetIndex(o)}

   oBrw:msrec := msind
   oBrw:AddColumn( HColumn():New( ,{|value,o|o:msrec[o:tekzp,1] },"C",1,0  ) )
   oBrw:AddColumn( HColumn():New( "Tag",{|value,o|o:msrec[o:tekzp,2] },"C",8,0  ) )
   oBrw:AddColumn( HColumn():New( "Expression",{|value,o|o:msrec[o:tekzp,3] },"C",ordlen,0  ) )
   oBrw:AddColumn( HColumn():New( "File",{|value,o|o:msrec[o:tekzp,4] },"C",8,0  ) )
  
   oBrw:bColorSel    := VColor( "800080" )
   oBrw:ofont := oBrwFont

   oModDlg:Activate()
Return Nil

Static Function SetIndex( oBrw )
Local oWindow := HWindow():GetMdiActive(), aControls, i

   SET ORDER TO oBrw:tekzp - 1
   WriteStatus( oWindow,2,"Order: "+oBrw:msrec[oBrw:tekzp,2] )
   IF oWindow != Nil
      aControls := oWindow:aControls
      IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
         RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
   ENDIF
   EndDialog( getmodalhandle() )
Return Nil

/* -----------------------  Creating New Index --------------------- */

Function NewIndex
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DIALOG_2" ON INIT {|| InitNewIndex() }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndNewIndex()}   ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() ) }  ;
        ON BN_CLICKED,IDC_CHECKBOX1 ACTION {|| TagName() }
   aModDlg:Activate()

Return Nil

Static Function InitNewIndex
Local hDlg := getmodalhandle()
   SetDlgItemText( hDlg, IDC_EDIT2, CutExten( CutPath( msfile[ improc ] ) ) + INDEXEXT() )
   CheckDlgButton( hDlg,IDC_CHECKBOX1,.T. )
   SetFocus( GetDlgItem( hDlg, IDC_EDIT2 ) )
Return Nil

Static Function TagName
Local hDlg := getmodalhandle()
Local hEdit := GetDlgItem( getmodalhandle(),IDC_EDIT3 )
   IF IsDlgButtonChecked( hDlg,IDC_CHECKBOX1 )
      SendMessage( hEdit, WM_ENABLE, 1, 0 )
   ELSE
      SendMessage( hEdit, WM_ENABLE, 0, 0 )
   ENDIF
Return Nil

Static Function EndNewIndex()
Local hDlg := getmodalhandle()
Local indname, isMulti, isUniq, tagname, expkey, expfor
Local oWindow, aControls, i

   indname := GetDlgItemText( hDlg, IDC_EDIT2, 20 )
   IF Empty( indname )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT2 ) )
      Return Nil
   ENDIF
   isMulti := IsDlgButtonChecked( hDlg,IDC_CHECKBOX1 )
   IF isMulti
      tagname := GetDlgItemText( hDlg, IDC_EDIT3, 60 )
      IF Empty( tagname )
         SetFocus( GetDlgItem( hDlg, IDC_EDIT3 ) )
         Return Nil
      ENDIF
   ENDIF
   isUniq := IsDlgButtonChecked( hDlg,IDC_CHECKBOX2 )
   expkey := GetDlgItemText( hDlg, IDC_EDIT4, 60 )
   IF Empty( expkey )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT4 ) )
      Return Nil
   ENDIF
   expfor := GetDlgItemText( hDlg, IDC_EDIT5, 60 )
   indname := mypath + indname
   SetDlgItemText( hDlg, IDC_TEXT2, "Indexing ..." )
   IF numdriv = 1 .AND. isMulti
      IF EMPTY( expfor )
         ORDCREATE( RTRIM( indname ), RTRIM( tagname ), RTRIM( expkey ), &( "{||" + RTRIM( expkey ) + "}" ), Iif( isUniq,.T.,Nil ) )
      ELSE
         ordCondSet( RTRIM( expfor ), &( "{||" + RTRIM( expfor ) + "}" ),,,,, RECNO(),,,, )
         ORDCREATE( RTRIM( indname ), RTRIM( tagname ), RTRIM( expkey ), &( "{||" + RTRIM( expkey ) + "}" ), Iif( isUniq,.T.,Nil ) )
      ENDIF
   ELSE
      IF EMPTY( expfor )
         dbCreateIndex( RTRIM( indname ), RTRIM( expkey ), &( "{||" + RTRIM( expkey ) + "}" ), Iif( isUniq,.T.,Nil ) )
      ELSE
         ordCondSet( RTRIM( expfor ), &( "{||" + RTRIM( expfor ) + "}" ),,,,, RECNO(),,,, )
         ORDCREATE( RTRIM( indname ),, RTRIM( expkey ), &( "{||" + RTRIM( expkey ) + "}" ), Iif( isUniq,.T.,Nil ) )
      ENDIF
   ENDIF
   oWindow := HWindow():GetMdiActive()
   WriteStatus( oWindow,2,"Order: "+Iif( isMulti,tagname,CutPath(indname) ) )

   IF oWindow != Nil
      aControls := oWindow:aControls
      IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
         RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
   ENDIF

   EndDialog( hDlg )
Return Nil

/* -----------------------  Open Index file --------------------- */

Function OpenIndex()
Local mask := Iif( numdriv == 1,"*.cdx;*.idx",Iif( numdriv == 2,"*ntx","*.adi" ) )
Local fname
Local oWindow, aControls, i

   IF !Empty( fname := SelectFile( "Index files", mask, mypath ) )
      mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
      ORDLISTADD( fname )
      oWindow := HWindow():GetMdiActive()
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
            RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
         ENDIF
      ENDIF
   ENDIF
Return Nil

/* -----------------------  Close Index files --------------------- */

Function CloseIndex()
Local oldOrder := Indexord()
Local oWindow, aControls, i

   ORDLISTCLEAR()
   IF Indexord() != oldOrder
      oWindow := HWindow():GetMdiActive()
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
            RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
         ENDIF
      ENDIF
   ENDIF
Return Nil

/* -----------------------  Open Database file --------------------- */

Function OpenDlg()
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_OPEN" ON INIT {|| InitOpen() }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndOpen()}  ;
        ON BN_CLICKED,IDC_BUTTONBRW ACTION {||SetDlgItemText( getmodalhandle(), IDC_EDIT7, SelectFile( "xBase files( *.dbf )", "*.dbf", mypath ) ) }
   aModDlg:Activate()

Return Nil

Static Function InitOpen
Local hDlg := getmodalhandle()
Local nd := Iif( numdriv==1,IDC_RADIOBUTTON3,Iif( numdriv==2,IDC_RADIOBUTTON4,IDC_RADIOBUTTON5 ) )
   CheckRadioButton( hDlg,IDC_RADIOBUTTON3,IDC_RADIOBUTTON5,nd )
   CheckDlgButton( hDlg,IDC_CHECKBOX4,SET( _SET_EXCLUSIVE ) )
   CheckDlgButton( hDlg,IDC_CHECKBOX5,prrdonly )
   CheckDlgButton( hDlg,IDC_CHECKBOX6,AdsLocking() )
   SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
Return .T.

Static Function EndOpen()
Local hDlg := getmodalhandle()
Local new_numdriv, old_numdriv := numdriv, alsName, fname, pass
Local oldExcl := SET( _SET_EXCLUSIVE ), oldRdonly := prrdonly, oldLock := AdsLocking()

   fname := GetEditText( hDlg, IDC_EDIT7 )
   IF !Empty( fname )
      alsName := GetEditText( hDlg, IDC_EDIT3 )
      pass := GetEditText( hDlg, IDC_EDIT4 )
      new_numdriv := Iif( IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON3 ), 1, ;
                     Iif( IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON4 ), 2, 3 ) )
      IF new_numdriv != numdriv
         numdriv := new_numdriv
         AdsSetFileType( Iif( numdriv==1,2,Iif( numdriv==2,1,3 ) ) )
      ENDIF
      IF SET( _SET_EXCLUSIVE ) != IsDlgButtonChecked( hDlg,IDC_CHECKBOX4 )
         SET( _SET_EXCLUSIVE, .NOT. SET( _SET_EXCLUSIVE ) )
      ENDIF
      IF prrdonly != IsDlgButtonChecked( hDlg,IDC_CHECKBOX5 )
         prrdonly := .NOT. prrdonly
      ENDIF
      IF AdsLocking() != IsDlgButtonChecked( hDlg,IDC_CHECKBOX6 )
         AdsLocking( !AdsLocking() )
      ENDIF
      mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
      OpenDbf( fname, Iif(Empty(alsName),Nil,alsName),,Iif(Empty(pass),Nil,pass) )
      IF numdriv != old_numdriv
         numdriv := old_numdriv
         AdsSetFileType( Iif( numdriv==1,2,Iif( numdriv==2,1,3 ) ) )
      ENDIF
      prrdonly := oldRdonly
      AdsLocking( oldLock )
      EndDialog( hDlg )
   ELSE
      SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
   ENDIF
Return .T.

Function OpenDbf( fname, alsname, hChild, pass )
Local oWindow, aControls, oBrowse, i

   IF !FiOpen( fname, alsname,, pass )
      Return 0
   ENDIF

   IF GetChildWindowsNumber() == 0
      EnableMenuItem( ,1, .T., .F. )
      EnableMenuItem( ,2, .T., .F. )
      EnableMenuItem( ,3, .T., .F. )
      EnableMenuItem( ,4, .T., .F. )
   ENDIF
   IF hChild == Nil .OR. hChild == 0
      INIT WINDOW oWindow MDICHILD TITLE fname ;
           AT 0,0                              ;
           ON GETFOCUS {|o|ChildGetFocus(o)}   ;
           ON EXIT {|o|ChildKill(o)}

      ADD STATUS PARTS 180,200,0
      @ 0,0 BROWSE oBrowse DATABASE  ;
           ON SIZE {|o,x,y|ResizeBrwQ(o,x,y)}

      oBrowse:bcolorSel  := VColor( "800080" )
      oBrowse:ofont := oBrwFont
      oBrowse:cargo := improc
      CreateList( oBrowse,.T. )
      oBrowse:lAppable := .T.

      oWindow:Activate()
   ELSE
      oWindow := HWindow():FindWindow( hChild )
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
            oBrowse := aControls[ i ]
            oBrowse:InitBrw()
            oBrowse:bcolorSel  := VColor( "800080" )
            oBrowse:ofont := oBrwFont
            oBrowse:cargo := improc
            SendMessage( HWindow():GetMain():handle, WM_MDIACTIVATE, hChild, 0 )
            oBrowse:Refresh()
         ENDIF
      ENDIF
   ENDIF
   WriteStatus( oWindow,1,Ltrim(Str(Reccount(),10))+" records" )
   WriteStatus( oWindow,2,"Order: None",.T. )
Return oWindow:handle

/* -----------------------  Calculator  --------------------- */

Function Calcul()
Local oModDlg

   INIT DIALOG oModDlg FROM RESOURCE TITLE "DLG_CALC" ON INIT {|| InitCalc() }
   DIALOG ACTIONS OF oModDlg ;
        ON 0,IDOK         ACTION {|| EndCalc()}
   oModDlg:Activate()

Return Nil

Static Function InitCalc()
Local hDlg := getmodalhandle()
   SetFocus( GetDlgItem( hDlg, IDC_EDITCALC ) )
Return Nil

Static Function EndCalc()
Local hDlg := getmodalhandle()
Local cExpr, res

   cExpr := GetDlgItemText( hDlg, IDC_EDITCALC, 80 )
   IF Empty( cExpr )
      SetFocus( GetDlgItem( hDlg, IDC_EDITCALC ) )
      Return Nil
   ENDIF
   IF TYPE( TRIM( cExpr ) ) $ "UEUI"
      MsgStop( "Wrong expression" )
   ELSE
      res := &( TRIM( cExpr ) )
      SetDlgItemText( hDlg, IDC_TEXTMSG, TRANSFORM( res, "@B" ) )
   ENDIF

Return Nil


/* -----------------------  Scripts  --------------------- */

Function Scripts( nAct )
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_SCRI" ON INIT {||SetFocus(GetDlgItem(getmodalhandle(),IDC_EDIT8))}
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndScri(nAct)}   ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() ) }  ;
        ON BN_CLICKED,IDC_PUSHBUTTON1 ACTION {||SetDlgItemText( getmodalhandle(), IDC_EDIT8, SelectFile( "Script files( *.scr )", "*.scr", mypath ) ) }
   aModDlg:Activate()

Return Nil

Static Function EndScri( lOk, nAct )
Local hDlg := getmodalhandle()
Local fname, arScr, nError, nLineEr, obl

   fname := GetDlgItemText( hDlg, IDC_EDIT8, 80 )
   IF Empty( fname )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT8 ) )
      Return Nil
   ENDIF
   SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
   IF ( arScr := RdScript( fname ) ) <> Nil
      IF nAct == 1
         obl := SELECT()
         GO TOP
         DO WHILE .NOT. EOF()
            DoScript( arScr )
            SELECT( obl )
            SKIP
         ENDDO
      ELSE
         DoScript( arScr )
      ENDIF
      MsgInfo( "Script executed" )
   ELSE
      nError := CompileErr( @nLineEr )
      MsgStop( "Script error ("+Ltrim(Str(nError))+"), line "+Ltrim(Str(nLineEr)) )
   ENDIF
   EndDialog( hDlg )

Return Nil

Function ChildGetFocus( oWindow )
Local i, aControls, oBrw
   IF oWindow != Nil
      aControls := oWindow:aControls
      IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
         oBrw := aControls[i]
         IF Valtype( oBrw:cargo ) == "N"
            Select( oBrw:cargo )
            improc := oBrw:cargo
            SetFocus( oBrw:handle )
         ENDIF
      ENDIF
   ENDIF
Return Nil

Function ChildKill( oWindow )
Local i, aControls, oBrw
   IF oWindow != Nil
      aControls := oWindow:aControls
      IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
         oBrw := aControls[i]
         IF Valtype( oBrw:cargo ) == "N"
            Select( oBrw:cargo )
            improc := oBrw:cargo
            IF Alias() == "ADSSQL"
               nQueryWndHandle := 0
            ENDIF
            FiClose()
            IF GetChildWindowsNumber() == 1
               EnableMenuItem( ,1, .F., .F. )
               EnableMenuItem( ,2, .F., .F. )
               EnableMenuItem( ,3, .F., .F. )
               EnableMenuItem( ,4, .F., .F. )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
Return Nil

Function ResizeBrwQ( oBrw, nWidth, nHeight )
Local hWndStatus, aControls := oBrw:oParent:aControls
Local aRect, i, nHbusy := 0

   FOR i := 1 TO Len( aControls )
      IF aControls[i]:classname() == "HSTATUS"
         hWndStatus := aControls[i]:handle
         aRect := GetClientRect( hWndStatus )
         nHbusy += aRect[ 4 ]
      ENDIF
   NEXT
   MoveWindow( oBrw:handle, 0, 0, nWidth, nHeight-nHBusy )
Return Nil

FUNCTION Fiopen( fname, alsname, prend, pass )

LOCAL i, oldimp := improc, res := .T.
LOCAL strerr := "Can't open file " + Iif( fname != Nil, fname, Alias() )
LOCAL bOldError, oError
   IF fname != Nil
      prend := IIF( prend = Nil, .F., prend )
      IF prend
         improc := lenmsf + 1
      ELSE
         FOR i := 1 TO 15
            IF msfile[ i ] = Nil
               improc := i
               EXIT
            ENDIF
         NEXT
      ENDIF
      IF improc > 15
         improc := oldimp
         MsgStop( "Too many opened files!" )
         RETURN .F.
      ENDIF
      SELECT( improc )
      alsname := IIF( alsname = Nil, CutExten( CutPath( fname ) ), Trim( CutExten( CutPath( alsname ) ) ) )
      IF ( i := AT( '~', alsname ) ) <> 0
         alsname := Stufmy( alsname, i, 1, '_' )
      ENDIF
      bOldError := ERRORBLOCK( { | e | OpenError( e ) } )
      DO WHILE .T.
         BEGIN SEQUENCE
            DBUSEAREA(,, fname, alsname,, prrdonly )
         RECOVER USING oError
            IF oError:genCode == EG_BADALIAS .OR. oError:genCode == EG_DUPALIAS
               IF EMPTY( alsname := MsgGet( "","Bad alias name, input other:" ) )
                  res := .F.
               ELSE
                  LOOP
               ENDIF
            ELSE
               EVAL( bOldError, oError )
            ENDIF
         END SEQUENCE
         EXIT
      ENDDO
      ERRORBLOCK( bOldError )
      IF !res
         improc := oldimp
         RETURN .F.
      ENDIF
      IF NETERR()
         IF SET( _SET_EXCLUSIVE )
            SET( _SET_EXCLUSIVE, .F. )
            DBUSEAREA(,, fname, CutExten( IIF( alsname = Nil, fname, alsname ) ),, prrdonly )
            IF NETERR()
               MsgStop( strerr )
               improc := oldimp
               RETURN .F.
            ENDIF
         ELSE
            MsgStop( strerr )
            improc := oldimp
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF
   IF improc > lenmsf
      lenmsf := improc
   ENDIF
   IF pass != Nil
      AdsEnableEncryption( pass )
   ENDIF
   msfile[ improc ] := Iif( fname != Nil, UPPER( fname ), Alias() )
   msmode[ improc, 1 ] = SET( _SET_EXCLUSIVE )
   msmode[ improc, 2 ] = prrdonly
   msmode[ improc, 3 ] = numdriv
   msmode[ improc, 4 ] = pass
   msmode[ improc, 5 ] = alsname
RETURN .T.

STATIC FUNCTION OpenError( e )

   BREAK e
RETURN

FUNCTION FiClose

LOCAL i
   IF improc > 0
      SELECT( improc )
      USE
      msfile[ improc ] = Nil
      IF improc = lenmsf
         FOR i := lenmsf - 1 TO 1 STEP - 1
            IF msfile[ i ] <> Nil
               EXIT
            ENDIF
         NEXT
         lenmsf := i
      ENDIF
      improc := 0
/*
      FOR i := 1 TO lenmsf
         IF msfile[ i ] <> Nil
            EXIT
         ENDIF
      NEXT
      improc := IIF( i <= lenmsf, i, 0 )
      prkorf := .T.
*/
   ENDIF
RETURN Nil

FUNCTION Stufmy( stsou, pozs, ellen, elzn )
RETURN SUBSTR( stsou, 1, pozs - 1 ) + elzn + SUBSTR( stsou, pozs + ellen )

FUNCTION FileLock()
LOCAL fname := msfile[ improc ]
   IF .NOT. msmode[ improc, 1 ]
      USE &fname EXCLUSIVE
      IF NETERR()
         MsgStop( "File cannot be opened in exclusive mode" )
         USE &fname SHARED
         RETURN .F.
      ELSE
         msmode[ improc, 1 ] = .T.
      ENDIF
   ENDIF
RETURN .T.

Function WndOut()
Return Nil

Function MsgSay( cText )
   MsgStop( cText )
Return Nil
