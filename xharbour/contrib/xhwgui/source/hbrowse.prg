/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HBrowse class - browse databases and arrays
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

// Modificaciones y Agregados. 27.07.2002, WHT.de la Argentina ///////////////
// 1) En el metodo HColumn se agregaron las DATA: "nJusHead" y "nJustLin",  //
//    para poder justificar los encabezados de columnas y tambi‚n las       //
//    lineas. Por default es DT_LEFT                                        //
//    0-DT_LEFT, 1-DT_RIGHT y 2-DT_CENTER. 27.07.2002. WHT.                 //
// 2) Ahora la variable "cargo" del metodo Hbrowse si es codeblock          //
//    ejectuta el CB. 27.07.2002. WHT                                       //
// 3) Se agreg¢ el Metodo "ShowSizes". Para poder ver la "width" de cada    //
//    columna. 27.07.2002. WHT.                                             //
// Alex:                                                                    //
// El metodo "DoVScroll" no se ejecuta correctamente. ¨¨¨???                //
//////////////////////////////////////////////////////////////////////////////

#include "windows.ch"
#include "inkey.ch"
#include "dbstruct.ch"
#include "HBClass.ch"
#include "guilib.ch"

REQUEST DBGOTOP
REQUEST DBGOTO
REQUEST DBGOBOTTOM
REQUEST DBSKIP
REQUEST RECCOUNT
REQUEST RECNO
REQUEST EOF
REQUEST BOF

/*
 * Scroll Bar Constants
 */
#define SB_HORZ             0
#define SB_VERT             1
#define SB_CTL              2
#define SB_BOTH             3

#undef  HDM_GETITEMCOUNT
#define HDM_GETITEMCOUNT    4608

#undef  WM_MOUSEWHEEL
#define WM_MOUSEWHEEL  0x020A

static crossCursor := 0
static arrowCursor := 0
static vCursor     := 0
static oCursor     := 0
static xDrag

//----------------------------------------------------//
CLASS HColumn INHERIT HObject

   DATA block,heading,footing,width,type,length,dec,cargo
   DATA nJusHead, nJusLin        // Para poder Justificar los Encabezados
                                 // de las columnas y lineas.
                                 // WHT. 27.07.2002
   DATA tcolor,bcolor,brush
   DATA lEditable INIT .F.       // Is the column editable
   DATA aList                    // Array of possible values for a column -
                                 // combobox will be used while editing the cell
   DATA aBitmaps
   DATA bValid,bWhen             // When and Valid codeblocks for cell editing
   DATA bEdit                    // Codeblock, which performs cell editing, if defined
   DATA cGrid
   DATA lSpandHead INIT .F.
   DATA lSpandFoot INIT .F.
   DATA Picture

   METHOD New( cHeading,block,type,length,dec,lEditable,nJusHead,nJusLin,cPict,bValid,bWhen,aItem,oBmp )

ENDCLASS

//----------------------------------------------------//
METHOD New( cHeading,block,type,length, dec, lEditable, nJusHead, nJusLin, cPict, bValid, bWhen, aItem, oBmp ) CLASS HColumn

   ::heading   := iif( cHeading == nil,"",cHeading )
   ::block     := block
   ::type      := iif( type     == nil, "C", type )
   ::dec       := iif( dec      == nil,  0 , dec )
   ::lEditable := Iif( lEditable != Nil,lEditable,.F. )
   ::nJusHead  := iif( nJusHead == nil,  DT_LEFT , nJusHead )  // Por default
   ::nJusLin   := iif( nJusLin  == nil,  DT_LEFT , nJusLin  )  // Justif.Izquierda
   ::picture      := cPict
   ::bValid    := bValid
   ::bWhen     := bWhen
   ::aList     := aItem
   ::aBitmaps  := oBmp

   if length == nil 
        if cPict != nil
             ::length := len(transform(eval(block), cPict))
        else    
             ::length := 10             
        end                    
        ::length := max(::length, len(cHeading))
   else
        ::length := length
   end        
                

RETURN Self

//----------------------------------------------------//
CLASS HBrowse INHERIT HControl

   DATA winclass   INIT "BROWSE"
   DATA active     INIT .T.
   DATA lChanged   INIT .F.
   DATA lDispHead  INIT .T.                    // Should I display headers ?
   DATA lDispSep   INIT .T.                    // Should I display separators ?
   DATA aColumns                               // HColumn's array
   DATA rowCount                               // Number of visible data rows
   DATA rowPos                                 // Current row position
   DATA rowCurrCount INIT 0                    // Current number of rows
   DATA colPos                                 // Current column position
   DATA nColumns                               // Number of visible data columns
   DATA nLeftCol                               // Leftmost column
   DATA xpos
   DATA freeze                                 // Number of columns to freeze
   DATA kolz                                   // Number of records in browse
   DATA tekzp,msrec
   DATA recCurr INIT 0
   DATA headColor                              // Header text color
   DATA sepColor INIT 12632256                 // Separators color
   DATA tcolorSel,bcolorSel,brushSel
   DATA bSkip,bGoTo,bGoTop,bGoBot,bEof,bBof
   DATA bRcou,bRecno
   DATA bPosChanged, bLineOut
   DATA bEnter, bKeyDown, bUpdate
   DATA internal
   DATA alias                                  // Alias name of browsed database
   DATA x1,y1,x2,y2,width,height
   DATA minHeight INIT 0
   DATA lEditable INIT .F.
   DATA lAppable  INIT .F.
   DATA lAppMode  INIT .F.
   DATA lAutoEdit INIT .F.
   DATA lUpdated  INIT .F.
   DATA lAppended INIT .F.
   DATA lAdjRight INIT .T.                     // Adjust last column to right
   DATA nHeadRows INIT 1                       // Rows in header
   DATA nFootRows INIT 0                       // Rows in footer

   METHOD New( lType,oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,oFont, ;
                  bInit,bSize,bPaint,bEnter,bGfocus,bLfocus,lNoVScroll,lNoBorder,;
                  lAppend,lAutoedit,bUpdate,bKeyDown,bPosChg)
   METHOD InitBrw( nType )
   METHOD Rebuild()
   METHOD Activate()
   METHOD Redefine( lType,oWnd,nId,oFont,bInit,bSize,bDraw,bEnter,bGfocus,bLfocus )
   METHOD FindBrowse( nId )
   METHOD AddColumn( oColumn )
   METHOD InsColumn( oColumn,nPos )
   METHOD DelColumn( nPos )
   METHOD Paint()
   METHOD LineOut()
   METHOD HeaderOut( hDC )
   METHOD FooterOut( hDC )
   METHOD DoHScroll( wParam )
   METHOD DoVScroll( wParam )
   METHOD LineDown(lMouse)
   METHOD LineUp()
   METHOD PageUp()
   METHOD PageDown()
   METHOD Bottom(lPaint)
   METHOD Top()
   METHOD ButtonDown( lParam )
   METHOD ButtonUp( lParam )
   METHOD ButtonDbl( lParam )
   METHOD MouseMove( wParam, lParam )
   METHOD MouseWheel( nKeys, nDelta, nXPos, nYPos )
   METHOD Edit( wParam,lParam )
   METHOD Append() INLINE (::Bottom(.F.),::LineDown())
   METHOD RefreshLine()
   METHOD Refresh()
   METHOD ShowSizes()
   METHOD End()

ENDCLASS

//----------------------------------------------------//
METHOD New( lType,oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,oFont, ;
                  bInit,bSize,bPaint,bEnter,bGfocus,bLfocus,lNoVScroll,;
                  lNoBorder,lAppend,lAutoedit,bUpdate,bKeyDown,bPosChg ) CLASS HBrowse

   // ::classname:= "HBROWSE"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::type    := lType
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+ ;
                    Iif(lNoBorder=Nil.OR.!lNoBorder,WS_BORDER,0)+            ;
                    Iif(lNoVScroll=Nil.OR.!lNoVScroll,WS_VSCROLL,0) )
   ::oFont   := Iif( oFont==Nil,::oParent:oFont,oFont )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := Iif( nWidth==Nil,0,nWidth )
   ::nHeight := Iif( nHeight==Nil,0,nHeight )
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::bEnter  := bEnter
   ::bGetFocus   := bGFocus
   ::bLostFocus  := bLFocus
   
   ::lAppable    := lAppend
   ::lAutoEdit   := lAutoedit
   ::bUpdate     := bUpdate
   ::bKeyDown    := bKeyDown
   ::bPosChanged := bPosChg

   ::oParent:AddControl( Self )
   ::InitBrw()
   ::Activate()

RETURN Self

//----------------------------------------------------//
METHOD Activate CLASS HBrowse
   if ::oParent:handle != 0
      ::handle := CreateBrowse( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   endif
RETURN Nil

//----------------------------------------------------//
METHOD Redefine( lType,oWndParent,nId,oFont,bInit,bSize,bDraw,bEnter,bGfocus,bLfocus ) CLASS HBrowse
   // ::classname:= "HBROWSE"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::type    := lType
   ::id      := nId
   ::style   := ::nLeft := ::nTop := ::nWidth := 0
   ::oFont   := Iif( oFont==Nil,::oParent:oFont,oFont )
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bDraw
   ::bEnter  := bEnter
   ::bGetFocus  := bGFocus
   ::bLostFocus := bLFocus

   ::oParent:AddControl( Self )
   ::InitBrw()
RETURN Self

//----------------------------------------------------//
METHOD FindBrowse( nId ) CLASS HBrowse
   local i := Ascan( ::aItemsList,{|o|o:id==nId},1,::iItems )
RETURN Iif( i>0,::aItemsList[i],Nil )

//----------------------------------------------------//
METHOD AddColumn( oColumn ) CLASS HBrowse
   aadd( ::aColumns, oColumn )
   ::lChanged := .T.
RETURN oColumn

//----------------------------------------------------//
METHOD InsColumn( oColumn,nPos ) CLASS HBrowse
   aadd( ::aColumns,Nil )
   ains( ::aColumns,nPos )
   ::aColumns[ nPos ] := oColumn
   ::lChanged := .T.
RETURN oColumn

//----------------------------------------------------//
METHOD DelColumn( nPos ) CLASS HBrowse
   Adel( ::aColumns,nPos )
   Asize( ::aColumns,Len( ::aColumns ) - 1 )
   ::lChanged := .T.
RETURN Nil

//----------------------------------------------------//
METHOD End() CLASS HBrowse
   if ::brush != Nil
      ::brush:Release()
      ::brushSel:Release()
   endif
RETURN Nil

//----------------------------------------------------//
METHOD InitBrw( nType )  CLASS HBrowse

   if nType != Nil
      ::type := nType
   else
      ::aColumns := {}
      ::rowPos    := ::tekzp  := ::colpos := ::nLeftCol := 1
      ::freeze  := ::height := 0
      ::internal  := { 15,1 }
      ::msrec     := Nil

      if crossCursor == 0
         crossCursor := LoadCursor( IDC_SIZEWE )
         arrowCursor := LoadCursor( IDC_ARROW )
         vCursor := LoadCursor( IDC_SIZENS )
      endif
   endif

   if ::type == BRW_DATABASE
      ::alias   := Alias()
      ::bSKip   := &( "{|o, x|" + ::alias + "->(DBSKIP(x)) }" )
      ::bGoTop  := &( "{||" + ::alias + "->(DBGOTOP())}" )
      ::bGoBot  := &( "{||" + ::alias + "->(DBGOBOTTOM())}")
      ::bEof    := &( "{||" + ::alias + "->(EOF())}" )
      ::bBof    := &( "{||" + ::alias + "->(BOF())}" )
      ::bRcou   := &( "{||" + ::alias + "->(RECCOUNT())}" )
      ::bRecno  := &( "{||" + ::alias + "->(RECNO())}" )
      ::bGoTo   := &( "{|a,n|"  + ::alias + "->(DBGOTO(n))}" )
   elseif ::type == BRW_ARRAY
      ::bSKip   := { | o, x | ARSKIP( o, x ) }
      ::bGoTop  := { | o | o:tekzp := 1 }
      ::bGoBot  := { | o | o:tekzp := o:kolz }
      ::bEof    := { | o | o:tekzp > o:kolz }
      ::bBof    := { | o | o:tekzp == 0 }
      ::bRcou   := { | o | len( o:msrec ) }
      ::bRecno  := { | o | o:tekzp }
      ::bGoTo   := { | o, n | o:tekzp := n }
   endif
RETURN Nil

//----------------------------------------------------//
METHOD Rebuild( hDC ) CLASS HBrowse

   local i, j, oColumn, xSize, nColLen, nHdrLen, nCount

   IF ::brush != Nil
      ::brush:Release()
   ENDIF
   IF ::brushSel != Nil
      ::brushSel:Release()
   ENDIF
   IF ::bcolor != Nil
      ::brush     := HBrush():Add( ::bcolor )
      IF hDC != Nil
         SendMessage( ::handle, WM_ERASEBKGND, hDC, 0 )
      ENDIF
   ENDIF
   IF ::bcolorSel != Nil
      ::brushSel  := HBrush():Add( ::bcolorSel )
   ENDIF
   ::nLeftCol  := ::colPos := ::freeze + 1
   ::tekzp     := ::rowPos := 1
   ::lEditable := .F.

   ::minHeight := 0
   for i := 1 to len( ::aColumns )

      oColumn := ::aColumns[i]

      if oColumn:lEditable
         ::lEditable := .T.
      endif

      if oColumn:aBitmaps != Nil
         xSize := 0
         for j := 1 to len( oColumn:aBitmaps )
            xSize := max( xSize, oColumn:aBitmaps[j,2]:nWidth+2 )
            ::minHeight := max( ::minHeight,oColumn:aBitmaps[j,2]:nHeight )
         next
      else
         // xSize := round( (max( len( FldStr( Self,i ) ), len( oColumn:heading ) ) + 2 ) * 8, 0 )
         nColLen := oColumn:length
         if oColumn:heading != nil
            HdrToken( oColumn:heading, @nHdrLen, @nCount )
            if ! oColumn:lSpandHead
               nColLen := max( nColLen, nHdrLen )
            endif
            ::nHeadRows := Max(::nHeadRows, nCount)
         endif
         if oColumn:footing != nil
            HdrToken( oColumn:footing, @nHdrLen, @nCount )
            if ! oColumn:lSpandFoot
               nColLen := max( nColLen, nHdrLen )
            endif
            ::nFootRows := Max(::nFootRows, nCount)
         endif
         xSize := round( ( nColLen + 2 ) * 8, 0 )
      endif

      oColumn:width := xSize

   next

   ::lChanged := .F.

RETURN Nil

//----------------------------------------------------//
METHOD Paint()  CLASS HBrowse
Local aCoors, aMetr, i, oldAlias, tmp, nRows
Local pps, hDC
Local oldBkColor, oldTColor

   IF !::active .OR. Empty( ::aColumns )
      Return Nil
   ENDIF

   IF ::tcolor == Nil ; ::tcolor := 0 ; ENDIF
   IF ::bcolor == Nil ; ::bcolor := VColor( "FFFFFF" ) ; ENDIF
   IF ::tcolorSel == Nil ; ::tcolorSel := VColor( "FFFFFF" ) ; ENDIF
   IF ::bcolorSel == Nil ; ::bcolorSel := VColor( "808080" ) ; ENDIF

   pps := DefinePaintStru()
   hDC := BeginPaint( ::handle, pps )
   if ::ofont != Nil
      SelectObject( hDC, ::ofont:handle )
   endif
   IF ::brush == Nil .OR. ::lChanged
      ::Rebuild(hDC)
   ENDIF
   aCoors := GetClientRect( ::handle )
   aMetr := GetTextMetric( hDC )
   ::width := Round( ( aMetr[ 3 ] + aMetr[ 2 ] ) / 2 - 1,0 )
   ::height := Max( aMetr[ 1 ], ::minHeight )
   ::x1 := aCoors[ 1 ]
   ::y1 := aCoors[ 2 ] + Iif( ::lDispHead, ::height*::nHeadRows, 0 )
   ::x2 := aCoors[ 3 ]
   ::y2 := aCoors[ 4 ]

   ::kolz := eval( ::bRcou,Self )
   IF ::tekzp > ::kolz
      ::tekzp := ::kolz
   ENDIF

   ::nColumns := FLDCOUNT( Self, ::x1 + 2, ::x2 - 2, ::nLeftCol )
   ::rowCount := Int( (::y2-::y1) / (::height+1) ) - ::nFootRows
   nRows := Min( ::kolz,::rowCount )

   IF ::internal[1] == 0
      IF ::rowPos != ::internal[2] .AND. !::lAppMode
         EVAL( ::bSkip, Self, ::internal[2]-::rowPos )
      ENDIF
      ::LineOut( ::internal[2], 0, hDC, .F. )
      IF ::rowPos != ::internal[2] .AND. !::lAppMode
         EVAL( ::bSkip, Self, ::rowPos-::internal[2] )
      ENDIF
   ELSE
      IF EVAL( ::bEof,Self )
         EVAL( ::bGoTop, Self )
         ::rowPos := 1
      ENDIF
      IF ::rowPos > nRows .AND. nRows > 0
         ::rowPos := nRows
      ENDIF
      tmp := EVAL( ::bRecno,Self )
      IF ::rowPos > 1
         EVAL( ::bSkip, Self,-(::rowPos-1) )
      ENDIF
      i := 1
      DO WHILE .T.
         IF EVAL( ::bRecno,Self ) == tmp
            ::rowPos := i
         ENDIF
         IF i > nRows .OR. EVAL( ::bEof,Self )
            EXIT
         ENDIF
         ::LineOut( i, 0, hDC, .F. )
         i ++
         EVAL( ::bSkip, Self,1 )
      ENDDO
      ::rowCurrCount := i - 1

      IF ::rowPos >= i
         ::rowPos := Iif( i > 1,i - 1,1 )
      ENDIF
      DO WHILE i <= nRows
         ::LineOut( i, 0, hDC, .F.,.T. )
         i ++
      ENDDO

      EVAL( ::bGoTo, Self,tmp )
   ENDIF
   IF ::lAppMode
      ::LineOut( nRows+1, 0, hDC, .F.,.T. )
   ENDIF

   ::LineOut( ::rowPos, IIF( ::lEditable, ::colpos, 0 ), hDC, .T. )
   IF Checkbit( ::internal[1],1 ) .OR. ::lAppMode
      ::HeaderOut( hDC )
      if ::nFootRows > 0
         ::FooterOut( hDC )
      endif
   ENDIF

   EndPaint( ::handle, pps )
   ::internal[1] := 15
   ::internal[2] := ::rowPos
   tmp := eval( ::bRecno,Self )
   IF ::recCurr != tmp
      ::recCurr := tmp
      IF ::bPosChanged != Nil
         Eval( ::bPosChanged,Self )
      ENDIF
   ENDIF

   IF ::lAppMode
      ::Edit()
   ENDIF

   IF ( tmp := GetFocus() ) == ::oParent:handle .OR. ;
         ::oParent:FindControl(,tmp) != Nil
      SetFocus( ::handle )
   ENDIF
   ::lAppMode := .F.

RETURN Nil

//----------------------------------------------------//
METHOD HeaderOut( hDC ) CLASS HBrowse
Local i, x, oldc, fif, xSize
Local nRows := Min( ::kolz+Iif(::lAppMode,1,0),::rowCount )
Local oPen, oldBkColor := SetBkColor( hDC,GetSysColor(COLOR_3DFACE) )
Local oColumn, nLine, cStr, cNWSE, oPenHdr

   IF ::lDispSep
      oPen := HPen():Add( BS_SOLID,1,::sepColor )
      SelectObject( hDC, oPen:handle )
   ENDIF

   x := ::x1
   if ::headColor <> Nil
      oldc := SetTextColor( hDC,::headColor )
   endif
   fif := iif( ::freeze > 0, 1, ::nLeftCol )

   while x < ::x2 - 2
      oColumn := ::aColumns[fif]
      xSize := oColumn:width
      if ::lAdjRight .and. fif == Len( ::aColumns )
         xSize := Max( ::x2 - x, xSize )
      endif
      if ::lDispHead .AND. !::lAppMode
         if oColumn:cGrid == nil
            DrawButton( hDC, x-1,::y1-::height*::nHeadRows,x+xSize-1,::y1+1,1 )
         else
            DrawButton( hDC, x-1,::y1-::height*::nHeadRows,x+xSize-1,::y1+1,0 )
            if oPenHdr == nil
               oPenHdr := HPen():Add( BS_SOLID,1,0 )
            endif
            SelectObject( hDC, oPenHdr:handle )
            cStr := oColumn:cGrid + ';'
            for nLine := 1 to ::nHeadRows
               cNWSE := __StrToken(@cStr, nLine, ';')
               if At('S', cNWSE) != 0
                  DrawLine(hDC, x-1, ::y1-(::height)*(::nHeadRows-nLine), x+xSize-1, ::y1-(::height)*(::nHeadRows-nLine))
               endif
               if At('N', cNWSE) != 0
                  DrawLine(hDC, x-1, ::y1-(::height)*(::nHeadRows-nLine+1), x+xSize-1, ::y1-(::height)*(::nHeadRows-nLine+1))
               endif
               if At('E', cNWSE) != 0
                  DrawLine(hDC, x+xSize-2, ::y1-(::height)*(::nHeadRows-nLine+1)+1, x+xSize-2, ::y1-(::height)*(::nHeadRows-nLine))
               endif
               if At('W', cNWSE) != 0
                  DrawLine(hDC, x-1, ::y1-(::height)*(::nHeadRows-nLine+1)+1, x-1, ::y1-(::height)*(::nHeadRows-nLine))
               endif
            next
            SelectObject( hDC, oPen:handle )
         endif
         // Ahora Titulos Justificados !!!
         cStr := oColumn:heading + ';'
         for nLine := 1 to ::nHeadRows
            DrawText( hDC, __StrToken(@cStr, nLine, ';'), x, ::y1-(::height)*(::nHeadRows-nLine+1)+1, x+xSize-1,::y1-(::height)*(::nHeadRows-nLine),;
               oColumn:nJusHead  + if(oColumn:lSpandHead, DT_NOCLIP, 0) )
         next
      endif
      if ::lDispSep .AND. x > ::x1
         DrawLine( hDC, x-1, ::y1+1, x-1, ::y1+(::height+1)*nRows )
      endif
      x += xSize
      if ! ::lAdjRight .and. fif == Len( ::aColumns )
         DrawLine( hDC, x-1, ::y1-(::height*::nHeadRows), x-1, ::y1+(::height+1)*nRows )
      endif
      fif := IIF( fif = ::freeze, ::nLeftCol, fif + 1 )
      if fif > Len( ::aColumns )
         exit
      endif
   enddo

   IF ::lDispSep
      for i := 1 to nRows
         DrawLine( hDC, ::x1, ::y1+(::height+1)*i, iif(::lAdjRight, ::x2, x), ::y1+(::height+1)*i )
      next
   ENDIF

   SetBkColor( hDC,oldBkColor )
   if ::headColor <> Nil
      SetTextColor( hDC,oldc )
   ENDIF
   IF ::lDispSep
      oPen:Release()
      if oPenHdr != nil
         oPenHdr:Release()
      endif
   ENDIF

RETURN Nil

//----------------------------------------------------//
METHOD FooterOut( hDC ) CLASS HBrowse
Local i, x, fif, xSize, oPen, nLine, cStr
Local oColumn

   IF ::lDispSep
      oPen := HPen():Add( BS_SOLID,1,::sepColor )
      SelectObject( hDC, oPen:handle )
   ENDIF

   x := ::x1
   fif := iif( ::freeze > 0, 1, ::nLeftCol )

   while x < ::x2 - 2
      oColumn := ::aColumns[fif]
      xSize := oColumn:width
      if ::lAdjRight .and. fif == Len( ::aColumns )
         xSize := Max( ::x2 - x, xSize )
      endif
      if oColumn:footing <> nil
         cStr := oColumn:footing + ';'
         for nLine := 1 to ::nFootRows
            DrawText( hDC, __StrToken(@cStr, nLine, ';'),;
               x, ::y1+(::rowCount+nLine-1)*(::height+1)+1, x+xSize-1, ::y1+(::rowCount+nLine)*(::height+1),;
               oColumn:nJusLin + if(oColumn:lSpandFoot, DT_NOCLIP, 0) )
         next
      endif
      x += xSize
      fif := IIF( fif = ::freeze, ::nLeftCol, fif + 1 )
      if fif > Len( ::aColumns )
         exit
      endif
   enddo

   IF ::lDispSep
      DrawLine( hDC, ::x1, ::y1+(::rowCount)*(::height+1)+1, iif(::lAdjRight, ::x2, x), ::y1+(::rowCount)*(::height+1)+1 )
      oPen:Release()
   ENDIF

RETURN Nil

//----------------------------------------------------//
METHOD LineOut( nstroka, vybfld, hDC, lSelected, lClear ) CLASS HBrowse
Local x, dx, i := 1, shablon, sviv, fif, fldname, slen, xSize
Local j, ob, bw, bh, y1, hBReal
Local oldBkColor, oldTColor, oldBk1Color, oldT1Color
Local oLineBrush := Iif( lSelected, ::brushSel,::brush )

   ::xpos := x := ::x1
   IF lClear == Nil ; lClear := .F. ; ENDIF

   IF ::bLineOut != Nil
      Eval( ::bLineOut,Self,lSelected )
   ENDIF
   IF ::kolz > 0
      oldBkColor := SetBkColor( hDC, Iif( lSelected,::bcolorSel,::bcolor ) )
      oldTColor  := SetTextColor( hDC, Iif( lSelected,::tcolorSel,::tcolor ) )
      fldname := SPACE( 8 )
      fif     := IIF( ::freeze > 0, 1, ::nLeftCol )

      WHILE x < ::x2 - 2
         xSize := ::aColumns[fif]:width
         IF ::lAdjRight .and. fif == LEN( ::aColumns )
            xSize := Max( ::x2 - x, xSize )
         ENDIF
         IF i == ::colpos
            ::xpos := x
         ENDIF

         IF vybfld == 0 .OR. vybfld == i
            IF ::aColumns[fif]:bColor != Nil .AND. ::aColumns[fif]:brush == Nil
               ::aColumns[fif]:brush := HBrush():Add( ::aColumns[fif]:bColor )
            ENDIF
            hBReal := Iif( ::aColumns[fif]:brush != Nil, ;
                         ::aColumns[fif]:brush:handle,   ;
                         oLineBrush:handle )
            FillRect( hDC, x, ::y1+(::height+1)*(nstroka-1)+1, x+xSize-1,::y1+(::height+1)*nstroka, hBReal )
            IF !lClear
               IF ::aColumns[fif]:aBitmaps != Nil .AND. !Empty( ::aColumns[fif]:aBitmaps )
                  FOR j := 1 TO Len( ::aColumns[fif]:aBitmaps )
                     IF Eval( ::aColumns[fif]:aBitmaps[i,1],EVAL( ::aColumns[fif]:block,,Self,fif ),lSelected )
                        ob := ::aColumns[fif]:aBitmaps[i,2]
                        IF ob:nHeight > ::height
                           y1 := 0
                           bh := ::height
                           bw := Int( ob:nWidth * ( ob:nHeight / ::height ) )
                           DrawBitmap( hDC, ob:handle,, x, y1+::y1+(::height+1)*(nstroka-1)+1, bw, bh )
                        ELSE
                           y1 := Int( (::height-ob:nHeight)/2 )
                           bh := ob:nHeight
                           bw := ob:nWidth
                           DrawTransparentBitmap( hDC, ob:handle, x, y1+::y1+(::height+1)*(nstroka-1)+1 )
                        ENDIF
                        // DrawBitmap( hDC, ob:handle,, x, y1+::y1+(::height+1)*(nstroka-1)+1, bw, bh )
                        EXIT
                     ENDIF
                  NEXT
               ELSE
                  sviv := FLDSTR( Self,fif )
                  // Ahora lineas Justificadas !!
                  IF ::aColumns[fif]:tColor != Nil
                     oldT1Color := SetTextColor( hDC, ::aColumns[fif]:tColor )
                  ENDIF
                  IF ::aColumns[fif]:bColor != Nil
                     oldBk1Color := SetBkColor( hDC, ::aColumns[fif]:bColor )
                  ENDIF
                  DrawText( hDC, sviv, x, ::y1+(::height+1)*(nstroka-1)+1, x+xSize-2,::y1+(::height+1)*nstroka-1, ::aColumns[fif]:nJusLin )
                  IF ::aColumns[fif]:tColor != Nil
                     SetTextColor( hDC, oldT1Color )
                  ENDIF
                  IF ::aColumns[fif]:bColor != Nil
                     SetBkColor( hDC, oldBk1Color )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         x += xSize
         fif := IIF( fif = ::freeze, ::nLeftCol, fif + 1 )
         i ++
         IF ! ::lAdjRight .and. fif > LEN( ::aColumns )
            EXIT
         ENDIF
      ENDDO
      SetTextColor( hDC,oldTColor )
      SetBkColor( hDC,oldBkColor )
   ENDIF
RETURN Nil

//----------------------------------------------------//
METHOD DoHScroll( wParam ) CLASS HBrowse

   local nScrollCode := LoWord( wParam )
   local minPos, maxPos, nPos, oldLeft := ::nLeftCol, oldPos := ::colpos, fif

   GetScrollRange( ::handle, SB_HORZ, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_HORZ )

   IF nScrollCode == SB_LINELEFT
      LineLeft( Self )
   ELSEIF nScrollCode == SB_LINERIGHT
      LineRight( Self )
   ENDIF
   IF ::nLeftCol != oldLeft .OR. ::colpos != oldpos
      fif := Iif( ::lEditable, ::colpos+::nLeftCol-1, ::nLeftCol )
      nPos := Iif( fif==1, minPos, Iif( fif=Len(::aColumns), maxpos, ;
                   Int((maxPos-minPos+1)*fif/Len(::aColumns)) ) )
      SetScrollPos( ::handle, SB_HORZ, nPos )
      IF ::nLeftCol == oldLeft
         ::RefreshLine()
      ELSE
         RedrawWindow( ::handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
   ENDIF
   SetFocus( ::handle )

RETURN Nil

//----------------------------------------------------//
STATIC FUNCTION LINERIGHT( oBrw )
   locaL i

   if oBrw:lEditable
      IF oBrw:colpos < oBrw:nColumns
         oBrw:colpos ++
         RETURN Nil
      endif
   endif
   IF oBrw:nColumns + oBrw:nLeftCol - oBrw:freeze - 1 < Len(oBrw:aColumns)
      i := oBrw:nLeftCol + oBrw:nColumns
      DO WHILE oBrw:nColumns + oBrw:nLeftCol - oBrw:freeze - 1 < Len(oBrw:aColumns) .AND. oBrw:nLeftCol + oBrw:nColumns = i
         oBrw:nLeftCol ++
      ENDDO
      oBrw:colpos := i - oBrw:nLeftCol + 1
   ENDIF
   RETURN Nil

//----------------------------------------------------//
STATIC FUNCTION LINELEFT( oBrw )

   IF oBrw:lEditable
      oBrw:colpos --
   ENDIF
   IF oBrw:nLeftCol > oBrw:freeze + 1 .AND. ( !oBrw:lEditable .OR. oBrw:colpos < oBrw:freeze + 1 )
      oBrw:nLeftCol --
      IF ! oBrw:lEditable .OR. oBrw:colpos < oBrw:freeze + 1
         oBrw:colpos := oBrw:freeze + 1 
      ENDIF
   ENDIF
   IF oBrw:colpos < 1
      oBrw:colpos := 1
   ENDIF
RETURN Nil

//----------------------------------------------------//
METHOD DoVScroll( wParam ) CLASS HBrowse
Local nScrollCode := LoWord( wParam )

   if nScrollCode == SB_LINEDOWN
      ::LINEDOWN(.T.)
   elseif nScrollCode == SB_LINEUP
      ::LINEUP()
   elseif nScrollCode == SB_BOTTOM
      ::BOTTOM()
   elseif nScrollCode == SB_TOP
      ::TOP()

   elseif nScrollCode == SB_THUMBPOSITION

   elseif nScrollCode == SB_THUMBTRACK
   endif
RETURN 0

//----------------------------------------------------//
METHOD LINEDOWN(lMouse) CLASS HBrowse
Local minPos, maxPos, nPos

   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_VERT )

   Eval( ::bSkip, Self,1 )
   IF Eval( ::bEof,Self )
      Eval( ::bSkip, Self,- 1 )
      IF ::lAppable .AND. ( lMouse==Nil.OR.!lMouse )
         ::lAppMode := .T.
      ELSE
         SetFocus( ::handle )
         Return Nil
      ENDIF
   ENDIF
   ::rowPos ++
   IF ::rowPos > ::rowCount
      ::rowPos := ::rowCount
      InvalidateRect( ::handle, 0 )
   ELSE
      ::internal[1] := 0
      InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::internal[2]-::height, ::x2, ::y1+(::height+1)*(::rowPos+1) )
   ENDIF
   IF ::lAppMode 
      IF ::rowPos > 1
         ::rowPos --
      ENDIF
      ::colPos := ::nLeftCol := 1
   ENDIF
   IF !::lAppMode  .OR. ::nLeftCol == 1
      ::internal[1] := SetBit( ::internal[1], 1, 0 )
   ENDIF
   nPos += Int( (maxPos-minPos)/(::kolz-1) )
   SetScrollPos( ::handle, SB_VERT, nPos )
   PostMessage( ::handle, WM_PAINT, 0, 0 )
   SetFocus( ::handle )
RETURN Nil

//----------------------------------------------------//
METHOD LINEUP() CLASS HBrowse
   local minPos, maxPos, nPos
   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_VERT )
   EVAL( ::bSkip, Self,- 1 )
   IF EVAL( ::bBof,Self )
      EVAL( ::bGoTop,Self )
   ELSE
      ::rowPos --
      IF ::rowPos = 0
         ::rowPos := 1
         InvalidateRect( ::handle, 0 )
      ELSE
         ::internal[1] := 0
         InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::internal[2]-::height, ::x2, ::y1+(::height+1)*::internal[2] )
         InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::rowPos-::height, ::x2, ::y1+(::height+1)*::rowPos )
      ENDIF
      nPos -= Int( (maxPos-minPos)/(::kolz-1) )
      SetScrollPos( ::handle, SB_VERT, nPos )
      ::internal[1] := SetBit( ::internal[1], 1, 0 )
      PostMessage( ::handle, WM_PAINT, 0, 0 )
   ENDIF
   SetFocus( ::handle )
RETURN Nil

//----------------------------------------------------//
METHOD PAGEUP() CLASS HBrowse
   local minPos, maxPos, nPos, step
   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_VERT )
   IF ::rowPos > 1
      step := ( ::rowPos - 1 )
      EVAL( ::bSKip, Self,- step )
      ::rowPos := 1
      InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::internal[2]-::height, ::x2, ::y1+(::height+1)*::internal[2] )
      InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::rowPos-::height, ::x2, ::y1+(::height+1)*::rowPos )
   ELSE
      step := ::rowCurrCount    // Min( ::kolz,::rowCount )
      EVAL( ::bSkip, Self,- step )
      IF EVAL( ::bBof,Self )
         EVAL( ::bGoTop,Self )
      ENDIF
      InvalidateRect( ::handle, 0 )
   ENDIF
   nPos := Max( nPos - Int( (maxPos-minPos)*step/(::kolz-1) ), minPos )
   SetScrollPos( ::handle, SB_VERT, nPos )
   ::internal[1] := SetBit( ::internal[1], 1, 0 )
   PostMessage( ::handle, WM_PAINT, 0, 0 )
   SetFocus( ::handle )
RETURN Nil

//----------------------------------------------------//
METHOD PAGEDOWN() CLASS HBrowse
Local minPos, maxPos, nPos, nRows := ::rowCurrCount // Min( ::kolz,::rowCount )
Local step := Iif( nRows>::rowPos,nRows-::rowPos+1,nRows )
   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_VERT )
   EVAL( ::bSkip, Self, step )
   ::rowPos := Min( ::kolz, nRows )
   IF EVAL( ::bEof,Self )
      EVAL( ::bSkip, Self,- 1 )
      nPos := maxPos
   ELSE
      nPos := Min( nPos + Int( (maxPos-minPos)*step/(::kolz-1) ), maxPos )
   ENDIF
   // writelog( "PageDown: "+str(minpos,3)+"-"+str(maxpos,3)+","+str(npos,3)+"/"+str(step,3)+"/"+str(::kolz) )
   SetScrollPos( ::handle, SB_VERT, nPos )
   InvalidateRect( ::handle, 0 )
   ::internal[1] := SetBit( ::internal[1], 1, 0 )
   PostMessage( ::handle, WM_PAINT, 0, 0 )
   SetFocus( ::handle )
RETURN Nil

//----------------------------------------------------//
METHOD BOTTOM(lPaint) CLASS HBrowse
Local minPos, maxPos, nPos

   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )

   nPos := GetScrollPos( ::handle, SB_VERT )
   ::rowPos := lastrec()
   eval( ::bGoBot, Self )
   ::rowPos := min( ::kolz, ::rowCount )
   nPos := maxPos
   SetScrollPos( ::handle, SB_VERT, nPos )
   InvalidateRect( ::handle, 0 )

   ::internal[1] := SetBit( ::internal[1], 1, 0 )
   IF lPaint == Nil .OR. lPaint
      PostMessage( ::handle, WM_PAINT, 0, 0 )
      SetFocus( ::handle )
   ENDIF
RETURN Nil

//----------------------------------------------------//
METHOD TOP() CLASS HBrowse
Local minPos, maxPos, nPos

   GetScrollRange( ::handle, SB_VERT, @minPos, @maxPos )
   nPos := GetScrollPos( ::handle, SB_VERT )
   ::rowPos := 1
   EVAL( ::bGoTop,Self )
   nPos := minPos
   SetScrollPos( ::handle, SB_VERT, nPos )
   InvalidateRect( ::handle, 0 )
   ::internal[1] := SetBit( ::internal[1], 1, 0 )
   PostMessage( ::handle, WM_PAINT, 0, 0 )
   SetFocus( ::handle )

RETURN Nil

//----------------------------------------------------//
METHOD ButtonDown( lParam ) CLASS HBrowse
Local hBrw := ::handle
Local nLine := Int( HIWORD(lParam)/(::height+1) + Iif(::lDispHead,1-::nHeadRows,1) )
Local step := nLine - ::rowPos, res := .F., nrec
Local minPos, maxPos, nPos
Local xm := LOWORD(lParam), x1, fif

   IF nLine > 0 .AND. nLine <= ::rowCurrCount
      IF step != 0
         nrec := Recno()
         EVAL( ::bSkip, Self, step )
         IF !Eval( ::bEof,Self )
            GetScrollRange( hBrw, SB_VERT, @minPos, @maxPos )
            nPos := GetScrollPos( hBrw, SB_VERT )
            ::rowPos := nLine
            nPos := Min( nPos + Int( (maxPos-minPos)*step/(::kolz-1) ), maxPos )
            SetScrollPos( hBrw, SB_VERT, nPos )
            res := .T.
         ELSE
            Go nrec
         ENDIF
      ENDIF
      IF ::lEditable
         x1  := ::x1
         fif := IIF( ::freeze > 0, 1, ::nLeftCol )
         DO WHILE fif < (::nLeftCol+::nColumns) .AND. x1 + ::aColumns[fif]:width < xm
            x1 += ::aColumns[fif]:width
            fif := IIF( fif = ::freeze, ::nLeftCol, fif + 1 )
         ENDDO
         IF ::colpos != fif - ::nLeftCol + 1 + :: freeze
            ::colpos := fif - ::nLeftCol + 1 + :: freeze
            GetScrollRange( hBrw, SB_HORZ, @minPos, @maxPos )
            nPos := Iif( fif==1, minPos, Iif( fif=Len(::aColumns), maxpos, ;
                         Int((maxPos-minPos+1)*fif/Len(::aColumns)) ) )
            SetScrollPos( hBrw, SB_HORZ, nPos )
            res := .T.
         ENDIF
      ENDIF
      IF res
         InvalidateRect( hBrw, 0, ::x1, ::y1+(::height+1)*::internal[2]-::height, ::x2, ::y1+(::height+1)*::internal[2] )
         InvalidateRect( hBrw, 0, ::x1, ::y1+(::height+1)*::rowPos-::height, ::x2, ::y1+(::height+1)*::rowPos )
         ::internal[1] := SetBit( ::internal[1], 1, 0 )
         PostMessage( hBrw, WM_PAINT, 0, 0 )
      ENDIF
   ELSEIF nLine == 0
      IF oCursor == crossCursor
         oCursor := vCursor
         Hwg_SetCursor( oCursor )
         xDrag := LoWord( lParam )
      ENDIF
   ENDIF
RETURN Nil

//----------------------------------------------------//
METHOD ButtonUp( lParam ) CLASS HBrowse
   local hBrw := ::handle
   local xPos := LOWORD(lParam), x := ::x1, x1, i := ::nLeftCol
   IF oCursor == vCursor
      DO WHILE x < xDrag
         x += ::aColumns[i]:width
         IF Abs( x-xDrag ) < 10
            x1 := x - ::aColumns[i]:width
            EXIT
         ENDIF
         i++
      ENDDO
      IF xPos > x1
         ::aColumns[i]:width := xPos - x1
         Hwg_SetCursor( arrowCursor )
         oCursor := 0
         InvalidateRect( hBrw, 0 )
         PostMessage( hBrw, WM_PAINT, 0, 0 )
      ENDIF
   ENDIF
   SetFocus( ::handle )
RETURN Nil

//----------------------------------------------------//
METHOD ButtonDbl( lParam ) CLASS HBrowse
   local hBrw := ::handle
   local nLine := Int( HIWORD(lParam)/(::height+1) + Iif(::lDispHead,1-::nHeadRows,1) )

   // writelog( "ButtonDbl"+str(nLine)+ str(::rowCurrCount) )
   if nLine <= ::rowCurrCount
      ::ButtonDown( lParam )
      ::Edit()
   endif
RETURN Nil

//----------------------------------------------------//
METHOD MouseMove( wParam, lParam ) CLASS HBrowse
   local xPos := LoWord( lParam ), yPos := HiWord( lParam )
   local x := ::x1, i := ::nLeftCol, res := .F.

   DlgMouseMove()
   IF !::active .OR. Empty( ::aColumns ) .OR. ::height == Nil
      Return Nil
   ENDIF
   IF ::lDispSep .AND. yPos <= ::height+1
      IF wParam == 1 .AND. oCursor == vCursor
         Hwg_SetCursor( oCursor )
         res := .T.
      ELSE
         DO WHILE x < ::x2 - 2 .AND. i <= Len( ::aColumns )
            x += ::aColumns[i++]:width
            IF Abs( x - xPos ) < 8
                  IF oCursor != vCursor
                     oCursor := crossCursor
                  ENDIF
                  Hwg_SetCursor( oCursor )
               res := .T.
               EXIT
            ENDIF
         ENDDO
      ENDIF
      IF !res .AND. oCursor != 0
         Hwg_SetCursor( arrowCursor )
         oCursor := 0
      ENDIF
   ENDIF
RETURN Nil

//----------------------------------------------------------------------------//
METHOD MouseWheel( nKeys, nDelta, nXPos, nYPos ) CLASS HBrowse
   if Hwg_BitAnd( nKeys, MK_MBUTTON ) != 0
      if nDelta > 0
         ::PageUp()
      else
         ::PageDown()
      endif
   else
      if nDelta > 0
         ::LineUp()
      else
         ::LineDown()
      endif
   endif
return nil

//----------------------------------------------------//
METHOD Edit( wParam,lParam ) CLASS HBrowse
Local fipos,varbuf, x1, y1, fif, lReadExit, rowPos
Local oModDlg, oColumn, aCoors, nChoic, bInit, oGet

   IF ::bEnter != Nil
      Eval( ::bEnter, Self )
   ELSE
      IF ::lEditable
         fipos := ::colpos + ::nLeftCol - 1 - ::freeze
         oColumn := ::aColumns[fipos]
         IF oColumn:lEditable .AND. ;
              ( oColumn:bWhen = Nil .OR. EVAL( oColumn:bWhen ) )
            IF ::lAppMode
               varbuf := Iif( oColumn:type=="D",Ctod(Space(8)), ;
                           Iif( oColumn:type=="N",0,"" ) )
            ELSE
               varbuf := Eval( oColumn:block,,Self,fipos )
            ENDIF
         ELSE
            RETURN Nil
         ENDIF
         x1  := ::x1
         fif := IIF( ::freeze > 0, 1, ::nLeftCol )
         DO WHILE fif < fipos
            x1 += ::aColumns[fif]:width
            fif := IIF( fif = ::freeze, ::nLeftCol, fif + 1 )
         ENDDO
         rowPos := ::rowPos - 1
         IF ::lAppMode .AND. ::kolz != 0
            rowPos ++
         ENDIF
         y1 := ::y1+(::height+1)*rowPos

         // aCoors := GetWindowRect( ::handle )
         // x1 += aCoors[1]
         // y1 += aCoors[2]

         aCoors := ClientToScreen( ::handle,x1,y1 )
         x1 := aCoors[1]
         y1 := aCoors[2]

         lReadExit := ReadExit( .T. )
         bInit := Iif( wParam==Nil, {|o|MoveWindow(o:handle,x1,y1,oColumn:width,::height+1)}, ;
            {|o|MoveWindow(o:handle,x1,y1,oColumn:width,::height+1),PostMessage(o:aControls[1]:handle,WM_KEYDOWN,wParam,lParam)} )

         INIT DIALOG oModDlg STYLE WS_POPUP+1+WS_BORDER ;
            AT x1,y1                                    ;
            SIZE oColumn:width, ::height                ;
            ON INIT bInit

         IF oColumn:aList != Nil
            nChoic := Ascan( oColumn:aList,varbuf )
            @ 0,0 GET COMBOBOX nChoic           ;
               ITEMS oColumn:aList              ;
               SIZE oColumn:width, ::height+1   ;
               FONT ::oFont
         ELSE
            @ 0,0 GET oGet VAR varbuf           ;
               SIZE oColumn:width, ::height+1   ;
               NOBORDER                         ;
               STYLE ES_AUTOHSCROLL             ;
               FONT ::oFont                     ;
               PICTURE oColumn:picture          ;
               VALID oColumn:bValid
         ENDIF

         ACTIVATE DIALOG oModDlg

         IF oModDlg:lResult            
            IF oColumn:aList != Nil
               varbuf := oColumn:aList[nChoic]
            ENDIF
            IF ::lAppMode
               ::lAppMode := .F.
               IF ::type == BRW_DATABASE
                  APPEND BLANK
                  Eval( oColumn:block,varbuf,Self,fipos )
                  UNLOCK
               ELSE
                  IF Valtype(::msrec[1]) == "A"
                     Aadd( ::msrec,Array(Len(::msrec[1])) )
                     FOR fif := 2 TO Len((::msrec[1]))
                        ::msrec[Len(::msrec),fif] := ;
                              Iif( ::aColumns[fif]:type=="D",Ctod(Space(8)), ;
                                 Iif( ::aColumns[fif]:type=="N",0,"" ) )
                     NEXT
                  ELSE
                     Aadd( ::msrec,Nil )
                  ENDIF
                  ::tekzp := Len( ::msrec )
                  Eval( oColumn:block,varbuf,Self,fipos )
               ENDIF
               IF ::kolz > 0
                  ::rowPos ++
               ENDIF
               ::lAppended := .T.
               ::Refresh()
            ELSE
               Eval( oColumn:block,varbuf,Self,fipos )
               ::lUpdated := .T.
               InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*(::rowPos-2), ::x2, ::y1+(::height+1)*::rowPos )
               ::RefreshLine()
            ENDIF

            /* Execute block after changes are made */
            IF ::bUpdate != nil
                Eval( ::bUpdate,  Self, fipos )         
            END
   
         ELSEIF ::lAppMode
            ::lAppMode := .F.
            InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::rowPos, ::x2, ::y1+(::height+1)*(::rowPos+2) )
            ::RefreshLine()
         ENDIF
         SetFocus( ::handle )
         ReadExit( lReadExit )
         
      ENDIF
   ENDIF
RETURN Nil

//----------------------------------------------------//
METHOD RefreshLine() CLASS HBrowse
   ::internal[1] := 0
   InvalidateRect( ::handle, 0, ::x1, ::y1+(::height+1)*::rowPos-::height, ::x2, ::y1+(::height+1)*::rowPos )
   SendMessage( ::handle, WM_PAINT, 0, 0 )
RETURN Nil

//----------------------------------------------------//
METHOD Refresh() CLASS HBrowse
   ::internal[1] := 15
   // writelog( "Refresh - 1" )
   RedrawWindow( ::handle, RDW_ERASE + RDW_INVALIDATE + RDW_INTERNALPAINT + RDW_UPDATENOW )
   // InvalidateRect( ::handle, 0 )
   // SendMessage( ::handle, WM_PAINT, 0, 0 )
RETURN Nil

//----------------------------------------------------//
STATIC FUNCTION FldStr( oBrw,numf )

   local fldtype
   local rez
   local vartmp
   local nItem := numf
   local type
   local pict

   if numf <= len( oBrw:aColumns )

      type := (oBrw:aColumns[numf]):type
      pict := oBrw:aColumns[numf]:picture

      if pict != nil
         rez := transform(eval( oBrw:aColumns[numf]:block,,oBrw,numf ), pict) 
         
      else
         vartmp := eval( oBrw:aColumns[numf]:block,,oBrw,numf )

         if type == "C"
            rez := padr( vartmp, oBrw:aColumns[numf]:length )

         elseif type == "N"
            rez := PADL( STR( vartmp, oBrw:aColumns[numf]:length, ;
                   oBrw:aColumns[numf]:dec ),oBrw:aColumns[numf]:length )
         elseif type == "D"
            rez := PADR( DTOC( vartmp ),oBrw:aColumns[numf]:length )

         elseif type == "L"
            rez := PADR( IIF( vartmp, "T", "F" ),oBrw:aColumns[numf]:length )

         elseif type == "M" 
            rez := "<Memo>"
         endif
      endif
   endif

RETURN rez

//----------------------------------------------------//
STATIC FUNCTION FLDCOUNT( oBrw, xstrt, xend, fld1 )

   local klf := 0, i := IIF( oBrw:freeze > 0, 1, fld1 )

   while .T.
      // xstrt += ( MAX( oBrw:aColumns[i]:length, LEN( oBrw:aColumns[i]:heading ) ) - 1 ) * oBrw:width
      xstrt += oBrw:aColumns[i]:width
      IF xstrt > xend
         EXIT
      ENDIF
      klf ++
      i   := IIF( i = oBrw:freeze, fld1, i + 1 )
      // xstrt += 2 * oBrw:width
      IF i > Len(oBrw:aColumns)
         EXIT
      ENDIF
   ENDDO
RETURN IIF( klf = 0, 1, klf )

//----------------------------------------------------//
FUNCTION BrwProc( hBrw, msg, wParam, lParam )
Local oBrw, aCoors
Static keyCode := 0

   // WriteLog( "Brw: "+Str(hBrw,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   if msg != WM_CREATE
      if Ascan( { WM_MOUSEMOVE, WM_MOUSEWHEEL, WM_GETDLGCODE, WM_PAINT, WM_ERASEBKGND, WM_SETFOCUS, WM_KILLFOCUS,  WM_HSCROLL, WM_VSCROLL, WM_DESTROY, WM_KEYDOWN, WM_KEYUP, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK }, msg ) > 0

         if ( oBrw := FindSelf( hBrw ) ) == Nil
            // MsgStop( "WM: wrong browse handle "+Str( hBrw ),"Error!" )
            // EndWindow()
            return -1
         endif

         if oBrw:active .AND. !Empty( oBrw:aColumns )

            // Si la variable "cargo" es codeblock se ejecuta ed cb.
            // Sirve para ejecutar "algo" fuera del Browse. P/Ej.
            // Mostrar una variable en la "dialog". Etc.Etc.
            // 27.07.2002 - WHT.
            if Ascan( { WM_PAINT, WM_ERASEBKGND, WM_SETFOCUS, WM_KILLFOCUS, WM_HSCROLL, WM_VSCROLL, WM_KEYDOWN, WM_KEYUP, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK }, msg ) > 0
               if oBrw:bOther != Nil
                  eval( oBrw:bOther )
               endif
            endif

            if msg == WM_PAINT
               oBrw:Paint()

            elseif msg == WM_ERASEBKGND
               if oBrw:brush != Nil
                  aCoors := GetClientRect( oBrw:handle )
                  FillRect( wParam, aCoors[1], aCoors[2], aCoors[3]+1, aCoors[4]+1, oBrw:brush:handle )
                  return 1
               endif

            elseif msg == WM_SETFOCUS
               if oBrw:bGetFocus != Nil
                  eval( oBrw:bGetFocus, oBrw )
               endif

            elseif msg == WM_KILLFOCUS
               if oBrw:bLostFocus != Nil
                  eval( oBrw:bLostFocus, oBrw )
               endif

            elseif msg == WM_HSCROLL
               oBrw:DoHScroll( wParam )

            elseif msg == WM_VSCROLL
               oBrw:DoVScroll( wParam )

            elseif msg == WM_GETDLGCODE
               IF wParam != 0
                  keyCode := wParam
               ENDIF
               return 1

            elseif msg == WM_KEYUP
               IF wParam == 13 .AND. keyCode == 13
                  keyCode := 0
                  oBrw:Edit()
               ENDIF
               return 1

            elseif msg == WM_KEYDOWN
            // WriteLog( "Brw: "+Str(hBrw,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
               if oBrw:bKeyDown != Nil
                  if !Eval( oBrw:bKeyDown,oBrw,wParam )
                     return 1
                  endif
               endif
               // keyCode := wParam
               if wParam == 40        // Down
                  oBrw:LINEDOWN()
               elseif wParam == 38    // Up
                  oBrw:LINEUP()
               elseif wParam == 39    // Right
                  oBrw:DoHScroll( SB_LINERIGHT )
               elseif wParam == 37    // Left
                  oBrw:DoHScroll( SB_LINELEFT )
               elseif wParam == 36    // Home
                  oBrw:TOP()
               elseif wParam == 35    // End
                  oBrw:BOTTOM()
               elseif wParam == 34    // PageDown
                  oBrw:PageDown()
               elseif wParam == 33    // PageUp
                  oBrw:PageUp()
               elseif wParam == 13    // Enter
                  oBrw:Edit()
               elseif wParam >= 48 .and. wParam <= 90 .and. oBrw:lAutoEdit
                  oBrw:Edit( wParam,lParam )
               endif

               return 1

            elseif msg == WM_LBUTTONDOWN
               oBrw:ButtonDown( lParam )

            elseif msg == WM_LBUTTONUP
               oBrw:ButtonUp( lParam )

            elseif msg == WM_LBUTTONDBLCLK
               oBrw:ButtonDbl( lParam )

            elseif msg == WM_MOUSEMOVE
               oBrw:MouseMove( wParam, lParam )

            elseif msg == WM_MOUSEWHEEL

               oBrw:MouseWheel( LoWord( wParam ),;
                                If( HiWord( wParam ) > 32768,;
                                HiWord( wParam ) - 65535, HiWord( wParam ) ),;
                                LoWord( lParam ), HiWord( lParam ) )
            endif

         endif

         if msg == WM_DESTROY
            oBrw:End()
         endif

      endif
   endif
RETURN -1

//----------------------------------------------------//
FUNCTION CREATEARLIST( oBrw, arr )
   local i
   oBrw:type  := BRW_ARRAY
   oBrw:msrec := arr
   IF Len( oBrw:aColumns ) == 0
      // oBrw:aColumns := {}
      IF Valtype( arr[1] ) == "A"
         FOR i := 1 TO Len( arr[1] )
            oBrw:AddColumn( HColumn():New( ,ColumnArBlock() ) )
         NEXT
      ELSE
         oBrw:AddColumn( HColumn():New( ,{|value,o| o:msrec[ o:tekzp ] } ) )
      ENDIF
   ENDIF
   EVAL( oBrw:bGoTop,oBrw )
RETURN Nil

//----------------------------------------------------//
PROCEDURE ARSKIP( oBrw, kolskip )
Local tekzp1
   if oBrw:kolz != 0
      tekzp1   := oBrw:tekzp
      oBrw:tekzp += kolskip + IIF( tekzp1 = 0, 1, 0 )
      IF oBrw:tekzp < 1
         oBrw:tekzp := 0
      ELSEIF oBrw:tekzp > oBrw:kolz
         oBrw:tekzp := oBrw:kolz + 1
      ENDIF
   ENDIF
RETURN

//----------------------------------------------------//
FUNCTION CreateList( oBrw,lEditable )
Local i
Local nArea := select()
Local kolf := FCOUNT()

   oBrw:alias   := alias()

   oBrw:aColumns := {}
   for i := 1 TO kolf
      oBrw:AddColumn( HColumn():New( Fieldname(i),                      ;
                                     FieldWBlock( Fieldname(i),nArea ), ;
                                     dbFieldInfo( DBS_TYPE,i ),         ;
                                     dbFieldInfo( DBS_LEN,i ),          ;
                                     dbFieldInfo( DBS_DEC,i ),          ;
                                     lEditable ) )
   next

   oBrw:Refresh()

RETURN Nil

//----------------------------------------------------//
// Agregado x WHT. 27.07.02
// Locus metodus.
METHOD ShowSizes() CLASS HBrowse
   local cText := ""
   aeval( ::aColumns,;
          { | v,e | cText += ::aColumns[e]:heading + ": " + str( round(::aColumns[e]:width/8,0)-2  ) + chr(10)+chr(13) } )
   MsgInfo( cText )
RETURN nil

Function ColumnArBlock()
Return {|value,o,n| Iif( value==Nil,o:msrec[o:tekzp,n],o:msrec[o:tekzp,n]:=value ) }

Static function HdrToken(cStr, nMaxLen, nCount)
Local nL, nPos := 0

   nMaxLen := nCount := 0
   cStr += ';'
   while (nL := Len(__StrTkPtr(@cStr, @nPos, ";"))) != 0
      nMaxLen := Max( nMaxLen, nL )
      nCount ++
   enddo
RETURN nil
