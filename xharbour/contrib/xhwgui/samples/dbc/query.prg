/*
 * DBCHW - DBC ( Harbour + HWGUI )
 * SQL queries
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"
#include "dbchw.h"
#include "ads.ch"

Static cQuery := ""

Function OpenQuery
Local fname := SelectFile( "Query files( *.que )", "*.que", mypath )

   IF !Empty( fname )
      mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
      cQuery := MemoRead( fname )
      Query( .T. )
   ENDIF

Return Nil

Function Query( lEdit )
Local aModDlg

   IF !lEdit
      cQuery := ""
   ENDIF

   INIT DIALOG aModDlg FROM RESOURCE TITLE "DLG_QUERY" ON INIT {|| InitQuery() }
   DIALOG ACTIONS OF aModDlg ;
        ON 0,IDCANCEL     ACTION {|| EndQuery(.F.) }  ;
        ON BN_CLICKED,IDC_BTNEXEC ACTION {|| EndQuery(.T.) } ;
        ON BN_CLICKED,IDC_BTNSAVE ACTION {|| QuerySave() }
   aModDlg:Activate()

Return Nil

Static Function InitQuery()
Local hDlg := getmodalhandle()
   SetDlgItemText( hDlg, IDC_EDITQUERY, cQuery )
   SetFocus( GetDlgItem( hDlg, IDC_EDITQUERY ) )
Return Nil

Static Function EndQuery( lOk )
Local hDlg := getmodalhandle()
Local oldArea := Alias(), tmpdriv, tmprdonly
Local id1
Local aChildWnd, hChild

   IF lOk
      cQuery := GetEditText( hDlg, IDC_EDITQUERY )
      IF Empty( cQuery )
         SetFocus( GetDlgItem( hDlg, IDC_EDITQUERY ) )
         Return Nil
      ENDIF

      IF numdriv == 2
         MsgStop( "You shoud switch to ADS_CDX or ADS_ADT to run query" )
         Return .F.
      ENDIF
      IF AdsGetConnectionHandle() == 0
         IF Empty( mypath )
            AdsConnect( "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" ) )
         ELSE
            AdsConnect( mypath )
         ENDIF
      ENDIF
      IF Select( "ADSSQL" ) > 0
         Select ADSSQL
         USE
      ELSE
         SELECT 0
      ENDIF
      IF !AdsCreateSqlStatement( ,Iif( numdriv==1,2,3 ) )
         MsgStop( "Cannot create SQL statement" )
         IF !Empty( oldArea )
            Select( oldArea )
         ENDIF
         Return .F.
      ENDIF
      SetDlgItemText( hDlg, IDC_TEXTMSG, "Wait ..." )
      IF !AdsExecuteSqlDirect( cQuery )
         MsgStop( "SQL execution failed" )
         IF !Empty( oldArea )
            Select( oldArea )
         ENDIF
         Return .F.
      ELSE
         IF Alias() == "ADSSQL"
            improc := Select( "ADSSQL" )
            tmpdriv := numdriv; tmprdonly := prrdonly
            numdriv := 3; prrdonly := .T.
            // Fiopen()
            nQueryWndHandle := OpenDbf( ,"ADSSQL",nQueryWndHandle )
            numdriv := tmpdriv; prrdonly := tmprdonly
            /*
            SET CHARTYPE TO ANSI
            __dbCopy( mypath+"_dbc_que.dbf",,,,,, .F. )
            SET CHARTYPE TO OEM
            FiClose()
            nQueryWndHandle := OpenDbf( mypath+"_dbc_que.dbf","ADSSQL",nQueryWndHandle )
            */
         ELSE
            IF !Empty( oldArea )
               Select( oldArea )
            ENDIF
            MsgStop( "Statement doesn't returns cursor" )
            Return .F.
         ENDIF
      ENDIF
   ENDIF

   EndDialog( hDlg )
Return .T.

Function QuerySave
Local fname := SaveFile( "*.que","Query files( *.que )", "*.que", mypath )
   cQuery := GetDlgItemText( getmodalhandle(), IDC_EDITQUERY, 400 )
   IF !Empty( fname )
      MemoWrit( fname,cQuery )
   ENDIF
Return Nil