/*
 * DBCHW - DBC ( Harbour + HWGUI )
 * Database structure handling
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"
#include "dbchw.h"
#include "ads.ch"

Function StruMan( lNew )
Local oModDlg
Local at := { "Character", "Numeric", "Date", "Logical", "Memo" }
LOCAL af, oBrw

   IF lNew
      af := { {"","",0,0} }
   ELSE
      af := dbStruct()
   ENDIF

   INIT DIALOG oModDlg FROM RESOURCE TITLE "DLG_STRU"

   REDEFINE BROWSE oBrw ARRAY OF oModDlg ID ID_BROWSE  ;
       ON CLICK {|o|SetField(o)}
   REDEFINE COMBOBOX at OF oModDlg ID IDC_COMBOBOX2

   DIALOG ACTIONS OF oModDlg ;
          ON 0,IDOK        ACTION {|o| EndStru(o,lNew)}   ;
          ON BN_CLICKED,IDC_PUSHBUTTON2 ACTION {|| ModiStru(1) }  ;
          ON BN_CLICKED,IDC_PUSHBUTTON3 ACTION {|| ModiStru(2) }  ;
          ON BN_CLICKED,IDC_PUSHBUTTON4 ACTION {|| ModiStru(3) }  ;
          ON BN_CLICKED,IDC_PUSHBUTTON5 ACTION {|| ModiStru(4) }

   oBrw:msrec := af
   oBrw:AddColumn( HColumn():New( "Name",{|value,o|o:msrec[o:tekzp,1] },"C",10,0  ) )
   oBrw:AddColumn( HColumn():New( "Type",{|value,o|o:msrec[o:tekzp,2] },"C",4,0  ) )
   oBrw:AddColumn( HColumn():New( "Length",{|value,o|o:msrec[o:tekzp,3] },"N",4,0  ) )
   oBrw:AddColumn( HColumn():New( "Dec",{|value,o|o:msrec[o:tekzp,4] },"N",2,0  ) )
   oBrw:bcolorSel := VColor( "800080" )
   oBrw:ofont      := oBrwFont

   oModDlg:Activate()
Return Nil

Static Function SetField( oBrw )
Local hDlg := getmodalhandle(), i
   SetDlgItemText( hDlg, IDC_EDIT2, oBrw:msrec[oBrw:tekzp,1] )
   IF ( i := At( oBrw:msrec[oBrw:tekzp,2], "CNDLM" ) ) != 0
      ComboSetString( GetDlgItem( hDlg, IDC_COMBOBOX2 ), i )
   ENDIF
   SetDlgItemText( hDlg, IDC_EDIT3, Ltrim( Str( oBrw:msrec[oBrw:tekzp,3] ) ) )
   SetDlgItemText( hDlg, IDC_EDIT4, Ltrim( Str( oBrw:msrec[oBrw:tekzp,4] ) ) )
Return Nil

Static Function ModiStru( nOper )
Local oDlg := getmodalDlg(), hDlg := oDlg:handle
Local oBrowse := oDlg:FindControl( ID_BROWSE )
Local cName, cType, nLen, nDec := 0

   IF nOper < 4
      cName := GetDlgItemText( hDlg, IDC_EDIT2, 10 )
      IF Empty( cName )
         SetFocus( GetDlgItem( hDlg, IDC_EDIT2 ) )
         Return Nil
      ENDIF
      cType := Left( GetDlgItemText( hDlg, IDC_COMBOBOX2, 10 ), 1 )
      IF Empty( cType )
         SetFocus( GetDlgItem( hDlg, IDC_COMBOBOX2 ) )
         Return Nil
      ENDIF
      IF cType == "D" 
         nLen := 8
      ELSEIF cType == "L" 
         nLen := 1
      ELSEIF cType == "M" 
         nLen := 10
      ELSE
         nLen  := Val( GetDlgItemText( hDlg, IDC_EDIT3, 10 ) )
         IF nLen == 0
            SetFocus( GetDlgItem( hDlg, IDC_EDIT3 ) )
            Return Nil
         ENDIF
         IF cType == "N" 
            nDec  := Val( GetDlgItemText( hDlg, IDC_EDIT4, 10 ) )
         ENDIF
      ENDIF
      IF nOper == 3 .OR. ( oBrowse:kolz == 1 .AND. Empty( oBrowse:msrec[1,1] ) )
         oBrowse:msrec[ oBrowse:tekzp ] := { cName, cType, nLen, nDec }
      ELSEIF nOper == 1
         Aadd( oBrowse:msrec, { cName, cType, nLen, nDec } )
         oBrowse:kolz ++
      ELSEIF nOper == 2
         Aadd( oBrowse:msrec, Nil )
         Ains( oBrowse:msrec, oBrowse:tekzp )
         oBrowse:msrec[ oBrowse:tekzp ] := { cName, cType, nLen, nDec }
         oBrowse:kolz ++
      ENDIF
   ELSEIF nOper == 4
      Adel( oBrowse:msrec,oBrowse:tekzp )
      Asize( oBrowse:msrec,Len( oBrowse:msrec ) - 1 )
      oBrowse:kolz --
   ENDIF
   RedrawWindow( oBrowse:handle, RDW_ERASE + RDW_INVALIDATE )
Return Nil

Static Function EndStru( oDlg,lNew )
Local fname, alsname
Local A1,A2,A3,A4,B1,B2,B3,B4,C1,C2
Local fi1, kolf, i, j
Local oBrowse := oDlg:FindControl( ID_BROWSE )
Local oWindow, aControls
Local oPBar, nSch := 0

   IF lNew
      IF Empty( fname := SaveFile( "*.dbf","xBase files( *.dbf )", "*.dbf", mypath ) )
         Return Nil
      ENDIF
      mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
      dbCreate( fname, oBrowse:msrec )
      OpenDbf( fname )
   ELSE
      alsname := Alias()
      kolf := Fcount()
      A1   := ARRAY( kolf )
      A2   := ARRAY( kolf )
      A3   := ARRAY( kolf )
      A4   := ARRAY( kolf )
      AFIELDS( A1, A2, A3, A4 )
      SELECT 20
      fi1 := mypath + "a0_new"
      dbCreate( fi1, oBrowse:msrec )
      USE ( fi1 )
      kolf := Fcount()
      B1   := ARRAY( kolf )
      B2   := ARRAY( kolf )
      B3   := ARRAY( kolf )
      B4   := ARRAY( kolf )
      C1   := ARRAY( kolf )
      C2   := ARRAY( kolf )
      AFIELDS( B1, B2, B3, B4 )
      FOR i := 1 TO kolf
         j := ASCAN( A1, B1[ i ] )
         IF j > 0
            C2[ i ] = j
            IF B2[ i ] = A2[ j ] .AND. B3[ i ] = A3[ j ] .AND. B4[ i ] = A4[ j ]
               IF C1[ i ] = Nil
                  C1[ i ] := &( "{|param|param}" )
               ENDIF
            ELSE
               IF C1[ i ] = Nil
                  DO CASE
                  CASE A2[ j ] = "C" .AND. B2[ i ] = "N"
                     C1[ i ] := &( "{|param|VAL(param)}" )
                  CASE A2[ j ] = "N" .AND. B2[ i ] = "C"
                     C1[ i ] := &( "{|param|LTRIM(STR(param," + LTRIM( STR( A3[ j ], 2 ) ) + "," + LTRIM( STR( A4[ j ], 2 ) ) + "))}" )
                  CASE A2[ j ] = "C" .AND. B2[ i ] = "C"
                     C1[ i ] := &( "{|param|SUBSTR(param,1," + LTRIM( STR( A3[ j ], 4 ) ) + ")}" )
                  CASE A2[ j ] = "N" .AND. B2[ i ] = "N"
                     C1[ i ] := &( "{|param|param}" )
                  OTHERWISE
                     //           C1[i] := &("{|param|param}")
                  ENDCASE
               ENDIF
            ENDIF
         ENDIF
      NEXT
      SELECT( improc )
      oPBar := HProgressBar():NewBox( "Structure updating ...",,,,,10,RecCount() )
      GO TOP
      DO WHILE .NOT. EOF()
         SELECT 20
         APPEND BLANK
         FOR i := 1 TO kolf
            IF C1[ i ] <> Nil
               FIELDPUT( i, EVAL( C1[ i ], (alsname)->( FIELDGET( C2[ i ] ) ) ) )
            ENDIF
         NEXT
         SELECT( improc )
         SKIP
         oPBar:Step()
      ENDDO
      oPBar:End()
      SELECT( improc )
      USE
      SELECT 20
      USE
      fi1 := Cutexten( msfile[ improc ] )
      ERASE &(fi1+".bak")
      FRENAME( fi1 + ".dbf", fi1 + ".bak" )
      FRENAME( mypath + "a0_new.DBF", fi1 + ".dbf" )
      IF FILE( mypath + "a0_new.fpt" )
         FRENAME( mypath + "a0_new.fpt", fi1 + ".fpt" )
      ENDIF
      SELECT( improc )
      USE (fi1)

      oWindow := HWindow():GetMdiActive()
      IF oWindow != Nil
         aControls := oWindow:aControls
         IF ( i := Ascan( aControls, {|o|o:classname()=="HBROWSE"} ) ) > 0
            oBrowse := aControls[ i ]
            CreateList( oBrowse,.T. )
         ENDIF
      ENDIF
   ENDIF
   EndDialog( getmodalhandle() )
Return Nil

