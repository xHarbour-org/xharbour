/*
 * DBCHW - DBC ( Harbour + HWGUI )
 * Commands ( Replace, delete, ... )
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"
#include "dbchw.h"
// #include "ads.ch"

/* -----------------------  Replace --------------------- */

Function C_REPL
Local aModDlg
Local af := Array( Fcount() )
   Afields( af )

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_REPLACE" ON INIT {|| InitRepl() }

   REDEFINE COMBOBOX af OF aModDlg ID IDC_COMBOBOX1

   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndRepl()}   ;
        ON BN_CLICKED,IDC_RADIOBUTTON7 ACTION {|| RecNumberEdit() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON6 ACTION {|| RecNumberDisable() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON8 ACTION {|| RecNumberDisable() }

   aModDlg:Activate()

Return Nil

Static Function RecNumberEdit
Local hDlg := getmodalhandle()
Local hEdit := GetDlgItem( hDlg,IDC_EDITRECN )
   SendMessage( hEdit, WM_ENABLE, 1, 0 )
   SetDlgItemText( hDlg, IDC_EDITRECN, "1" )
   SetFocus( hEdit )
Return Nil

Static Function RecNumberDisable
Local hEdit := GetDlgItem( getmodalhandle(),IDC_EDITRECN )
   SendMessage( hEdit, WM_ENABLE, 0, 0 )
Return Nil

Static Function InitRepl()
Local hDlg := getmodalhandle()

   RecNumberDisable()
   CheckRadioButton( hDlg,IDC_RADIOBUTTON6,IDC_RADIOBUTTON8,IDC_RADIOBUTTON6 )
   SetFocus( GetDlgItem( hDlg, IDC_COMBOBOX1 ) )
Return Nil

Static Function EndRepl()
Local hDlg := getmodalhandle()
Local nrest, nrec
Local oWindow, aControls, i
Private finame, cValue, cFor

   oWindow := HWindow():GetMdiActive()

   finame := GetDlgItemText( hDlg, IDC_COMBOBOX1, 12 )
   IF Empty( finame )
      SetFocus( GetDlgItem( hDlg, IDC_COMBOBOX1 ) )
      Return Nil
   ENDIF
   cValue := GetDlgItemText( hDlg, IDC_EDIT7, 60 )
   IF Empty( cValue )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
      Return Nil
   ENDIF
   cFor := GetDlgItemText( hDlg, IDC_EDITFOR, 60 )
   IF .NOT. EMPTY( cFor ) .AND. TYPE( cFor ) <> "L"
      MsgStop( "Wrong expression!" )
   ELSE
      IF EMPTY( cFor )
         cFor := ".T."
      ENDIF
      nrec := Recno()
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
      IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
         REPLACE ALL &finame WITH &cValue FOR &cFor
      ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
         nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
         REPLACE NEXT nrest &finame WITH &cValue FOR &cFor
      ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
         REPLACE REST &finame WITH &cValue FOR &cFor
      ENDIF
      Go nrec
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Done !" )
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:ClassName()=="HBROWSE"} ) ) > 0
            aControls[i]:Refresh()
         ENDIF
      ENDIF
   ENDIF
Return Nil

/* -----------------------  Delete, recall, count --------------------- */

Function C_DELE( nAct )
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_DEL" ON INIT {|| InitDele(nAct) }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndDele(nAct)}   ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() )}  ;
        ON BN_CLICKED,IDC_RADIOBUTTON7 ACTION {|| RecNumberEdit() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON6 ACTION {|| RecNumberDisable() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON8 ACTION {|| RecNumberDisable() }
   aModDlg:Activate()

Return Nil

Static Function InitDele(nAct)
Local hDlg := getmodalhandle()
   IF nAct == 2
      SetWindowText( hDlg,"Recall")
   ELSEIF nAct == 3
      SetWindowText( hDlg,"Count")
   ENDIF
   RecNumberDisable()
   CheckRadioButton( hDlg,IDC_RADIOBUTTON6,IDC_RADIOBUTTON8,IDC_RADIOBUTTON6 )
   SetFocus( GetDlgItem( hDlg, IDC_EDITFOR ) )
Return Nil

Static Function EndDele( nAct )
Local hDlg := getmodalhandle()
Local nrest, nsum, nRec := Recno()
Local oWindow, aControls, i
Private cFor

   oWindow := HWindow():GetMdiActive()

   cFor := GetDlgItemText( hDlg, IDC_EDITFOR, 60 )
   IF .NOT. EMPTY( cFor ) .AND. TYPE( cFor ) <> "L"
      MsgStop( "Wrong expression!" )
   ELSE
      IF EMPTY( cFor )
         cFor := ".T."
      ENDIF
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
      IF nAct == 1
         IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
            DELETE ALL FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
            nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
            DELETE NEXT nrest FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
            DELETE REST FOR &cFor
         ENDIF
      ELSEIF nAct == 2
         IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
            RECALL ALL FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
            nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
            RECALL NEXT nrest FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
            RECALL REST FOR &cFor
         ENDIF
      ELSEIF nAct == 3
         IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
            COUNT TO nsum ALL FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
            nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
            COUNT TO nsum NEXT nrest FOR &cFor
         ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
            COUNT TO nsum REST FOR &cFor
         ENDIF
         SetDlgItemText( hDlg, IDC_TEXTMSG, "Result: "+Str( nsum ) )
         Go nrec
         Return Nil
      ENDIF
      Go nrec
      WriteStatus( oWindow,3,"Done" )
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:ClassName()=="HBROWSE"} ) ) > 0
            RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
         ENDIF
      ENDIF
   ENDIF

   EndDialog( hDlg )
Return Nil

/* -----------------------  Sum --------------------- */

Function C_SUM()
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_SUM" ON INIT {|| InitSum() }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndSum()}   ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() )}  ;
        ON BN_CLICKED,IDC_RADIOBUTTON7 ACTION {|| RecNumberEdit() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON6 ACTION {|| RecNumberDisable() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON8 ACTION {|| RecNumberDisable() }
   aModDlg:Activate()

Return Nil

Static Function InitSum()
Local hDlg := getmodalhandle()
   RecNumberDisable()
   CheckRadioButton( hDlg,IDC_RADIOBUTTON6,IDC_RADIOBUTTON8,IDC_RADIOBUTTON6 )
   SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
Return Nil

Static Function EndSum()
Local hDlg := getmodalhandle()
Local cSumf, cFor, nrest, blsum, blfor, nRec := Recno()
Private nsum := 0

   cSumf := GetDlgItemText( hDlg, IDC_EDIT7, 60 )
   IF EMPTY( cSumf )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
      Return Nil
   ENDIF

   cFor := GetDlgItemText( hDlg, IDC_EDITFOR, 60 )
   IF ( !EMPTY( cFor ) .AND. TYPE( cFor ) <> "L" ) .OR. TYPE( cSumf ) <> "N"
      MsgStop( "Wrong expression!" )
   ELSE
      IF EMPTY( cFor )
         cFor := ".T."
      ENDIF
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
      blsum := &( "{||nsum:=nsum+" + cSumf + "}" )
      blfor := &( "{||" + cFor + "}" )
      IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
         DBEVAL( blsum, blfor )
      ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
         nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
         DBEVAL( blsum, blfor,, nrest )
      ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
         DBEVAL( blsum, blfor,,,, .T. )
      ENDIF
      Go nrec
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Result: "+Str( nsum ) )
      Return Nil
   ENDIF

   EndDialog( hDlg )
Return Nil

/* -----------------------  Append from --------------------- */

Function C_APPEND()
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_APFROM" ON INIT {|| InitApp() }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndApp()}  ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() )}  ;
        ON BN_CLICKED,IDC_BUTTONBRW ACTION {||SetDlgItemText( getmodalhandle(), IDC_EDIT7, SelectFile( "xBase files( *.dbf )", "*.dbf", mypath ) ) } ;
        ON BN_CLICKED,IDC_RADIOBUTTON11 ACTION {|| DelimEdit() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON10 ACTION {|| DelimDisable() } ;
        ON BN_CLICKED,IDC_RADIOBUTTON9 ACTION {|| DelimDisable() }
   aModDlg:Activate()

Return Nil

Static Function DelimEdit
Local hDlg := getmodalhandle()
Local hEdit := GetDlgItem( hDlg,IDC_EDITDWITH )
   SendMessage( hEdit, WM_ENABLE, 1, 0 )
   SetDlgItemText( hDlg, IDC_EDITDWITH, " " )
   SetFocus( hEdit )
Return Nil

Static Function DelimDisable
Local hEdit := GetDlgItem( getmodalhandle(),IDC_EDITDWITH )
   SendMessage( hEdit, WM_ENABLE, 0, 0 )
Return Nil

Static Function InitApp()
Local hDlg := getmodalhandle()
   DelimDisable()
   CheckRadioButton( hDlg,IDC_RADIOBUTTON9,IDC_RADIOBUTTON9,IDC_RADIOBUTTON11 )
   SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
Return Nil

Static Function EndApp()
Local hDlg := getmodalhandle()
Local fname, nRec := Recno()

   fname := GetDlgItemText( hDlg, IDC_EDIT7, 60 )
   IF EMPTY( fname )
      SetFocus( GetDlgItem( hDlg, IDC_EDIT7 ) )
      Return Nil
   ENDIF

   SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
   IF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON6 )
      // DBEVAL( blsum, blfor )
   ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON7 )
      // nrest := Val( GetDlgItemText( hDlg, IDC_EDITRECN, 10 ) )
      // DBEVAL( blsum, blfor,, nrest )
   ELSEIF IsDlgButtonChecked( hDlg,IDC_RADIOBUTTON8 )
      // DBEVAL( blsum, blfor,,,, .T. )
   ENDIF
   Go nrec

   EndDialog( hDlg )
Return Nil

/* -----------------------  Reindex, pack, zap --------------------- */

Function C_RPZ( nAct )
Local aModDlg

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_OKCANCEL" ON INIT {|| InitRPZ(nAct) }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDOK         ACTION {|| EndRPZ(nAct)}   ;
        ON 0,IDCANCEL     ACTION {|| EndDialog( getmodalhandle() ) }
   aModDlg:Activate()

Return Nil

Static Function InitRPZ( nAct )
Local hDlg := getmodalhandle()
   SetDlgItemText( hDlg, IDC_TEXTHEAD, Iif( nAct==1,"Reindex ?", ;
                                       Iif( nAct==2,"Pack ?", "Zap ?" ) ) )
Return Nil

Static Function EndRPZ( nAct )
Local hDlg := getmodalhandle()
Local hWnd, oWindow, aControls, i

   IF .NOT. msmode[ improc, 1 ]
      IF .NOT. FileLock()
         EndDialog( hDlg )
         Return Nil
      ENDIF
   ENDIF
   SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
   IF nAct == 1
      Reindex
   ELSEIF nAct == 2
      Pack
   ELSEIF nAct == 3
      Zap
   ENDIF

   hWnd := SendMessage( HWindow():GetMain():handle, WM_MDIGETACTIVE,0,0 )
   oWindow := HWindow():FindWindow( hWnd )
   IF oWindow != Nil
      aControls := oWindow:aControls
      IF ( i := Ascan( aControls, {|o|o:ClassName()=="HBROWSE"} ) ) > 0
         RedrawWindow( aControls[i]:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
   ENDIF

   EndDialog( hDlg )
Return Nil
