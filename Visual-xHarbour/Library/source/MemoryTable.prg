#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MemoryTable.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_PROFESSIONAL

#include "debug.ch"
#include "vxh.ch"
#include "colors.ch"

#include "ord.ch"

#define EF_NONE                         0
#define EF_CANRETRY                     1
#define EF_CANSUBSTITUTE                2
#define EF_CANDEFAULT                   4


CLASS MemoryTable INHERIT Component
   DATA Structure          PUBLISHED
   DATA Table              PUBLISHED INIT {}
   DATA Alias              EXPORTED
   DATA Fields             EXPORTED
   DATA Parent             EXPORTED
   DATA Tag                EXPORTED
   DATA Record             EXPORTED INIT 1
   DATA bOnFileNameChanged EXPORTED
   DATA bOnFileClosed      EXPORTED

   DATA lEof               PROTECTED INIT .F.
   DATA lBof               PROTECTED INIT .F.

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Deleted()          INLINE .F.
   METHOD Eof()              INLINE ::lEof
   METHOD Bof()              INLINE ::lBof
   METHOD RecCount()         INLINE LEN( ::Table )
   METHOD RecNo()            INLINE MIN( ::Record, LEN( ::Table ) )
   
   METHOD OrdKeyRelPos()     INLINE ( ::Record / LEN( ::Table ) )
   METHOD ordKey()           INLINE ""
   METHOD GoTop()
   METHOD GoTo()
   METHOD GoBottom()
   METHOD Skip()
   METHOD Delete()
   
   METHOD Close()            INLINE ::Table := NIL
   METHOD Append()
   METHOD Insert()           INLINE ::Append( .T. )
   METHOD OrdSetFocus()
   METHOD Seek()
   METHOD Found()            INLINE !::Eof()
   METHOD Zap()              INLINE ::Table := {}, ::Record := 0
   METHOD OrdKeyGoTo( nPos ) INLINE ::Goto( nPos )
   METHOD OrdKeyNo()         INLINE ::Record
   METHOD OrdKeyCount()      INLINE ::RecCount()
   METHOD FieldPos()
   METHOD RecLock()          INLINE .T.
   METHOD UnLock()           INLINE .T.
   METHOD FileLock()         INLINE .T.
   METHOD FieldPut( nField, xVal ) INLINE ::Fields:FieldPut( nField, xVal )
   METHOD FieldGet( nField, xVal ) INLINE ::Fields:FieldGet( nField, xVal )
   ACCESS IsOpen             INLINE ::Structure != NIL //Select( ::Area ) > 0
ENDCLASS

METHOD Init( oOwner, aStruct, aData ) CLASS MemoryTable
   ::__xCtrlName   := "MemoryTable"
   ::ClsName       := "MemoryTable"
   ::ComponentType := "DataSource"
   ::Super:Init( oOwner )
   //::Fields     := MemData( Self )
   ::Table      := aData
   ::Structure  := aStruct
   ::lCreated   := .T.
RETURN Self

METHOD GoTop() CLASS MemoryTable
   ::Record := MIN( 1, ::RecCount() )
   ::lBof   := ::Record < 1
   ::lEof   := ::Record < 1
RETURN Self

METHOD GoBottom() CLASS MemoryTable
   ::Record := MAX( 0, ::RecCount() )
   ::lBof   := ::Record < 1
   ::lEof   := ::Record < 1
RETURN Self

METHOD GoTo( nRec ) CLASS MemoryTable
   IF nRec > 0
      IF ( ::lEof := ( nRec > ::RecCount() ) )
         nRec := ::RecCount()
      ENDIF
      ::Record := nRec
    ELSE
      ::GoBottom()
   ENDIF
RETURN Self

METHOD Skip( n ) CLASS MemoryTable
   DEFAULT n TO 1
   ::Record += n
   IF ::Record < 1
      ::Record := 1
      ::lBof   := .T.
      ::lEof   := ::RecCount() == 0
    ELSEIF ::Record > ::RecCount()
      ::Record := ::RecCount()
      ::lEof   := .T.
      ::lBof   := ::Record <= 0
    ELSE
      ::lEof   := ::RecCount() == 0
      ::lBof   := ::Record < 1
   ENDIF
RETURN Self

METHOD Delete() CLASS MemoryTable
   IF ::RecCount() > 0
      ADEL( ::Table, ::Record, .T. )
      ::Record := MIN( ::Record, ::RecCount() )
   ENDIF
   ::lBof   := ::Record < 1
   ::lEof   := ::Record < 1
RETURN Self

METHOD Create() CLASS MemoryTable

   LOCAL aField
   LOCAL hClass, cField, oErr
   STATIC s_nID := 0

   DEFAULT ::Table TO {}

   IF ! Empty( ::Structure )
      IF Empty( ::Alias )
         ::Alias := LTrim( Str( ++s_nID, 3, 0 ) )
      ENDIF

      TRY
         hClass := __ClsNew( "MEMDATA_" + ::Alias, 0, 0, { MemData():ClassH } )

         FOR EACH aField IN ::Structure
            aField[1] := AllTrim( Upper( aField[1] ) )
            cField := aField[1]

            __clsAddMsg( hClass,       cField, &( "{|Self| ::FieldGet( " + Str( HB_EnumIndex() ) + " ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
            __clsAddMsg( hClass, "_" + cField, &( "{|Self, xVal| ::FieldPut( " + Str( HB_EnumIndex() ) + ", xVal ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
         NEXT

         ::Fields := __clsInst( hClass ):Init( Self )
         
         ::lCreated := .T.
         //IF VALTYPE( ::Owner ) == "O" .AND. __ObjHasMsg( ::Owner, "__DataSourceNotify" )
         //   ::Owner:__DataSourceNotify()
         //ENDIF
         ::GoTop()
      CATCH oErr
         MessageBox( 0, ProcName(1) + "->" + ProcName() + " -> " + oErr:Operation + " <" + ::Alias + ">", oErr:Description, MB_ICONEXCLAMATION )
      END
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Seek( xKey, lSoft, lCaseSensitive, lPartial ) CLASS MemoryTable
   LOCAL bBlock := {|a| a[ ::FieldPos( ::Tag ) ] == xKey }
   (lSoft)
   DEFAULT lCaseSensitive TO .T., lPartial TO .f.  // only affect string values
   IF valtype( xKey ) == "C"
      IF lPartial
         bBlock := {|a, xVal | xVal := a[ ::FieldPos( ::Tag ) ], ;
                               if( lCaseSensitive, (xVal = xKey),;
                                   ( upper(xVal) = upper(xKey) ) ) }
      ELSEIF lCaseSensitive // default block handles this
      ELSE
         bBlock := {|a, xVal | xVal := a[ ::FieldPos( ::Tag ) ], ;
                                   ( upper(xVal) == upper(xKey) )  }
      ENDIF
   ENDIF
   ::lEof := ( ::Record := ASCAN( ::Table, bBlock ) ) == 0
   IF ::Record == 0
      ::Record := ::RecCount()+1
   ENDIF
RETURN !::lEof

//-------------------------------------------------------------------------------------------------------
METHOD FieldPos( cField ) CLASS MemoryTable
RETURN aScan( ::Structure, {|a| a[1] == UPPER( cField ) } )

//-------------------------------------------------------------------------------------------------------
METHOD OrdSetFocus( cOrder ) CLASS MemoryTable
   LOCAL cPrevTag := ::Tag
   ::Tag := cOrder
   DEFAULT cPrevTag TO ""
RETURN cPrevTag

//-------------------------------------------------------------------------------------------------------
METHOD Append( lIns ) CLASS MemoryTable
   LOCAL n, xVal
   IF lIns != NIL .AND. !::Record == 0
      AINS( ::Table, ::Record, Array( Len( ::Structure ) ), .T. )
    ELSE
      AADD( ::Table, Array( Len( ::Structure ) ) )
      ::Record := ::RecCount()
   ENDIF
   FOR n := 1 TO LEN( ::Structure )
       xVal := ""
       SWITCH ::Structure[ n ][ 2 ]
          CASE "C"
            xVal := ""
            EXIT
          CASE "N"
            xVal := 0
            EXIT
          CASE "L"
            xVal := .F.
            EXIT
          CASE "D"
            xVal := CTOD("")
            EXIT
       END
       ::Table[ ::Record ][ n ] := xVal
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS MemData INHERIT Data
   METHOD Put()
   METHOD FieldPut( nField, xVal )
   METHOD FieldGet( nField )
   METHOD FieldName( nField ) INLINE ::Parent:Structure[ nField ][1]
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Put( xVal, cName ) CLASS MemData
   LOCAL n
   IF xVal != NIL
      n := ::Parent:FieldPos( Upper( cName ) )
      IF LEN( ::Parent:Table[ ::Parent:Record ] ) < n
         AADD( ::Parent:Table[ ::Parent:Record ], )
      ENDIF
      RETURN ::Parent:Table[ ::Parent:Record ][ n ] := xVal
   ENDIF
RETURN ::Parent:Table[ ::Parent:Record ][ ::Parent:FieldPos( Upper( cName ) ) ]

//-------------------------------------------------------------------------------------------------------
METHOD FieldPut( nField, xVal ) CLASS MemData
RETURN ::Parent:Table[ ::Parent:Record ][ nField ] := xVal

//-------------------------------------------------------------------------------------------------------
METHOD FieldGet( nField ) CLASS MemData
RETURN ::Parent:Table[ ::Parent:Record ][ nField ]

//-------------------------------------------------------------------------------------------------------
//#ifdef VXH_PROFESSIONAL
FUNCTION Browse( nTop, nLeft, nBottom, nRight, aData, aHeaders, cCaption, bAction, oGrid )

   LOCAL oForm := WinForm( NIL )
   LOCAL oDataSource

   IF aData == NIL .AND. ! Used()
      RETURN .F.
   ENDIF

   IF nLeft == NIL
      nLeft   := 10
      nBottom := 410
      nTop    := 10
      nRight  := 610
   ENDIF

   IF cCaption == NIL
     IF aData == NIL
       cCaption := "Browse " + Alias()
     ELSE
       cCaption := ""
     ENDIF
   ENDIF

   oForm:Left                 := nLeft
   oForm:Height               := nBottom - nTop
   oForm:Top                  := 10
   oForm:Caption              := cCaption
   oForm:Width                := nRight - nLeft
   oForm:Name                 := "Form1"

   IF ValType( aData ) == 'A'
      WITH OBJECT ( MemoryTable( oForm ) )
         :Structure := {}

         IF Len( aData ) > 0
           IF ValType( aData[1] ) == 'A'
              aEval( aData[1], {|xField, nField| aAdd( :Structure, { IIf( aHeaders == NIL, "C" + LTrim( Str( nField, 3 ) ), aHeaders[ nField ] ), 'C', Len( ValToPrgExp( xField ) ), 0 } ) } )
           ELSE
              aAdd( :Structure, { "Value", 'C', Len( ValToPrgExp( aData[1] ) ), 0 } )
           ENDIF
         ELSE
           IF !( aHeaders == NIL )
             AEval( aHeaders, {|x| AAdd( :Structure, { x, 'C', 50 } ) } )
           ENDIF
         ENDIF

         :Create()

         #if 1
           :Table := aData
         #else
            FOR EACH Row IN aData
               :Append()

               IF ValType( Row ) == 'A'
                  aEval( Row, {|xField, nField| :Fields:FieldPut( nField, xField ) } )
               ELSE
                  :Fields:FieldPut( 1, Row )
               ENDIF
            NEXT
         #endif
         oDataSource := HB_QWith()
      END
   ELSE
      WITH OBJECT ( DataTable( oForm ) )
         :Shared               := .T.
         :CheckAlias()

         oDataSource := HB_QWith()
      END
   ENDIF

   oForm:Create()
   oDataSource:GoTop()
   
   WITH OBJECT ( oGrid := DATAGRID( oForm ) )
      :Left                 := 2
      :Width                := 607
      :Caption              := IIf( cCaption == NIL, "DataGrid1", cCaption )
      :Name                 := "DataGrid1"
      :AutoVertScroll       := .T.
      WITH OBJECT :Dock
         :Top                  := oForm
         :Left                 := oForm
         :Bottom               := oForm
         :Right                := oForm
      END

      :BackColor            := 16777215
      :Height               := 395
      :Top                  := 2
      :DataSource           := oDataSource

      :Action := bAction

      :AutoAddColumns( bAction == NIL )
      :Create()
   END

   oForm:Show()

RETURN .T.


//----------------------------------------------------------------------------------------------------//


FUNCTION BrowseArray( aArray, aStructure, oForm )
   LOCAL oGrid, aRect, n, i, aTable, hWin := GetDeskTopWindow()
   aRect := _GetWindowRect( hWin )
   
   IF Empty(oForm)
      // Create Form on the fly here
      oForm := WinForm( NIL )
      oForm:Create()
   ENDIF   
   
   WITH OBJECT oGrid := DataGrid( oForm )
   
      IF aStructure == NIL
         aStructure := {}
         IF VALTYPE( aArray[1] ) == "A" 
            FOR n := 1 TO LEN( aArray[1] )
                AADD( aStructure, {"COLUMN"+XSTR(n), VALTYPE( aArray[1][n] ), LEN(XSTR(aArray[1][n])), 0 } )
            NEXT
            FOR n := 1 TO LEN( aArray )
                FOR i := 1 TO LEN( aArray[1] )
                    aStructure[i][3] := MAX( aStructure[i][3], LEN(XSTR(aArray[n][i])) )
                NEXT
            NEXT
          ELSE                
            AADD( aStructure, {"COLUMN"+XSTR(n), VALTYPE( aArray[1] ), LEN(XSTR(aArray[1])), 0 } )
            aTable := {}
            FOR n := 1 TO LEN( aArray )
                aStructure[1][3] := MAX( aStructure[1][3], LEN(XSTR(aArray[n])) )
                AADD( aTable, {aArray[n]} )
            NEXT
            aArray := aTable
         ENDIF
         :ShowHeaders := .F.
         
      ENDIF
      
      :DataSource := MemoryTable( oForm )
      :DataSource:Structure := ACLONE( aStructure )
      :DataSource:Table     := ACLONE( aArray )
      :DataSource:Create()

      :Caption          := ""
      :FullRowSelect    := .T.
      :ExtVertScrollBar := .T.
      :DockToParent()
      :Create()
      :AutoAddColumns()

      oForm:xWidth := 0
      FOR i := 1 TO LEN( :Children )
          oForm:xWidth += :Children[i]:Width
      NEXT
      oForm:Width  := MAX( MIN( oForm:xWidth, aRect[3] ), 200 )
      oForm:Height := MAX( MIN( (:RowCount * :ItemHeight) + GetSystemMetrics( SM_CYCAPTION ) + (GetSystemMetrics( SM_CYBORDER )*2) + 10, aRect[4] ), 150 )
      
      oForm:CenterWindow()
      oForm:Show()
   END
RETURN oGrid

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS MemoryDataTable INHERIT DataTable
   DATA Structure          PUBLISHED
   DATA Shared             PUBLISHED INIT .T.

   DATA __lMemory          EXPORTED INIT .T.
   DATA xFileName          PROTECTED
   ACCESS FileName         INLINE ::xFileName
   ASSIGN FileName(c)      INLINE ::xFileName := c

   DATA ReadOnly           EXPORTED INIT .F.
   DATA AutoOpen           EXPORTED INIT .T.
   DATA SqlConnector       EXPORTED 
   DATA Path               EXPORTED INIT ""
   DATA CodePage           EXPORTED 
   METHOD Init() CONSTRUCTOR
   METHOD Destroy()      INLINE  ::Close(), DBDrop("mem:"+::name), Super:Destroy()
ENDCLASS

METHOD Init( oOwner ) CLASS MemoryDataTable
   DEFAULT ::__xCtrlName TO "MemoryDataTable"
   DEFAULT ::ClsName     TO "MemoryDataTable"
   ::ComponentType := "DataSource"
   ::Super:Init( oOwner )
RETURN Self

#endif
