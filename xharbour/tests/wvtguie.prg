//-------------------------------------------------------------------//
//
//                   GTWVT Console GUI Interface
//
//               Pritpal Bedi <pritpal@vouchcac.com>
//
//       I have tried to simulate the gui controls through GDI
//        functions and found a way to refresh those controls
//          through WM_PAINT message issued to the Window.
//                               and
//             I feel that IF this functionality is built
//                 into the GT itself, what a wonder!
//
//   This protocol opens up the the distinct possibilities and hope
//            you all will cooperate to enhance it further.
//
//           Thanks Peter Rees! You have laid the foundation!
//
//-------------------------------------------------------------------//
// 17/01/2004 - FSG - Implemented commands

#include    'inkey.ch'
#include   'common.ch'
#include   'wvtgui.ch'

//-------------------------------------------------------------------//
PROCEDURE Main()

   LOCAL aLastPaint, clr, scr
   LOCAL get_1   := ctod( '04/01/04' )
   LOCAL get_2   := Pad( 'Pritpal Bedi', 35 )
   LOCAL get_3   := Pad( '60, New Professor Colony', 35 )
   LOCAL get_4   := Pad( 'Ludhiana, INDIA', 35 )
   LOCAL get_5   := Pad( 'http://www.vouchcac.com', 35 )
   LOCAL get_6   := 20000
   LOCAL aBlocks := {}
   LOCAL nColGet := 8
   LOCAL GetList := {}
   LOCAL nTop    := 4
   LOCAL nLft    := 4
   LOCAL nBtm    := 20
   LOCAL nRgt    := 75
   LOCAL kf2     := SetKey( K_F2, {|| GuiGets_1() } )
   LOCAL kf3     := SetKey( K_F3, {|| Wvt_WindowExpand( 1 ) } )
   LOCAL kf4     := SetKey( K_F4, {|| Wvt_WindowExpand( -1 ) } )
   LOCAL kf5     := SetKey( K_F5, {|| VouBrowse() } )
   LOCAL nMaxRows:= MaxRow()
   LOCAL cLabel  := 'xHarbour simulated GUI.'

   SET DATE BRITISH

   SetColor( 'N/W' )
   CLS   // Pay attention, CLS clears graphic objects, so it must the first screen command

   //SET FONT 'Courier New' SIZE 18
   // Here use function instead the above command to permit font resize
   Wvt_SetFont( 'Courier New', 18 )

   SET WINDOW ICON TO 'vr_1.ico'
   SET WINDOW TITLE TO 'Vouch'
   @  1, 40 LABEL cLabel CENTER COLOR "N/BG" FONTNAME "Arial" FONTSIZE 26
   @ nTop, nLft TO nBtm, nRgt RAISED
   @  7, 61 TO 13, 70 RECESSED
   @ 15, 59 TO 18, 72 GROUPBOX
   @  5,  6 TO 19, 44 GROUPBOX RAISED
   @  8, 62 TO 12, 69 IMAGE 'vouch1.bmp'
   @  7, 48 TO 13, 55 RECESSED

   // Simulated buttons
   @ nMaxRows-2, 4 TO nMaxRows-2, 18 RAISED
   @ nMaxRows-2,20 TO nMaxRows-2, 30 RAISED
   @ nMaxRows-2,32 TO nMaxRows-2, 42 RAISED
   @ nMaxRows-2,44 TO nMaxRows-2, 54 RAISED

   SetColor( 'N/W,N/GR*,,,N/W*' )

   @ nMaxRows  , 0 SAY PadC( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'
   @ nMaxRows-2, 4 say ' F2 New Screen ' COLOR 'N/W'
   @ nMaxRows-2,20 say ' F3 Expand '     COLOR 'N/W'
   @ nMaxRows-2,32 say ' F4 Shrink '     COLOR 'N/W'
   @ nMaxRows-2,44 say ' F5 Browse '     COLOR 'N/W'

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33 ) + '>'
   @ 16, 61      SAY '< Salary >'

   @  7, nColGet GET get_1
   @ 10, nColGet GET get_2 VALID ( VouChoice() < 7 )
   @ 13, nColGet GET get_3
   @ 15, nColGet GET get_4
   @ 17, nColGet GET get_5
   @ 17, 61      GET get_6 PICTURE '@Z 9999999.99'

   READ

   //  Restore Environment
   //
   SetKey( K_F2, kf2 )
   SetKey( K_F3, kf3 )
   SetKey( K_F4, kf4 )
   SetKey( K_F5, kf5 )

RETURN

//-------------------------------------------------------------------//
PROCEDURE GuiGets_1()

   LOCAL aLastPaint, clr, scr
   LOCAL get_1      := ctod( '' )
   LOCAL get_2      := Space( 35 )
   LOCAL get_3      := Space( 35 )
   LOCAL get_4      := Space( 35 )
   LOCAL get_5      := Space( 35 )
   LOCAL get_6      := 0
   LOCAL aBlocks    := {}
   LOCAL nColGet    := 8
   LOCAL GetList    := {}
   LOCAL nTop       := 4
   LOCAL nLft       := 4
   LOCAL nBtm       := 20
   LOCAL nRgt       := 75
   LOCAL kf2        := SetKey( K_F2, {|| GuiGets_1() } )
   LOCAL kf3        := SetKey( K_F3, {|| Wvt_WindowExpand( 1 ) } )
   LOCAL kf4        := SetKey( K_F4, {|| Wvt_WindowExpand( -1 ) } )
   LOCAL cLabel     := 'VOUCH, that GROWS with you'
   LOCAL aPalette    := Wvt_GetPalette()
   LOCAL aNewPalette := aclone( aPalette )

   // Change the values of pallatte arbitrarily though yu can fine tune
   // these values with realistic values.
   //
   aNewPalette[ 8 ] := aNewPalette[ 8 ] + 100000

   Wvt_SetPalette( aNewPalette )

   SAVE WINDOW

   SetColor( 'N/W' )
   CLS

   //SET FONT "Courier New" SIZE 24
   SET WINDOW ICON TO 'DIA_EXCL.ico'
   SET WINDOW TITLE TO 'Vouch Gets 2'
   @ maxrow()-1,0 TO maxrow()-1,maxcol()
   @ maxrow()-3,0 TO maxrow()-3,maxcol() OUTLINE
   //SET BRUSH STYLE 0 COLOR rgb( 32,255,100 )
   SET BRUSH STYLE 0 COLOR "BG"
   DRAW ELLIPSE 6,50,10,58
   SET BRUSH STYLE 2 COLOR rgb( 255,255,100 ) HATCH 1
   //DRAW RECTANGLE 11, 50, 13, 58
   DRAW ROUND RECTANGLE 11, 50, 13, 58 WIDTH 30 HEIGHT 20

   SetColor( 'N/W,N/GR*,,,N/W*' )

   @ MaxRow(), 0 SAY PadC( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33) + '>'
   @ 16, 61      SAY '< Salary >'

   @  7, nColGet GET get_1
   @ 10, nColGet GET get_2
   @ 13, nColGet GET get_3
   @ 15, nColGet GET get_4
   @ 17, nColGet GET get_5
   @ 17, 61      GET get_6 PICTURE '@Z 9999999.99'

   READ

   // Restore Environment
   //
   Wvt_SetPalette( aPalette )
   RESTORE WINDOW

RETURN

//-------------------------------------------------------------------//
//      Wvt_SetFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_SETFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
FUNCTION Wvt_SetFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Gained!', 'r/w' )

   DevPos( nRow, nCol )

RETURN nil

//-------------------------------------------------------------------//
//      Wvt_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
FUNCTION Wvt_KillFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Lost...', 'B/w' )

   DevPos( nRow, nCol )

RETURN nil

//-------------------------------------------------------------------//
FUNCTION Wvt_WindowExpand( nUnits )

   STATIC sUnits := 18

   sUnits += nUnits

   Wvt_setFont( 'Courier New', sUnits )

RETURN .t.

//-------------------------------------------------------------------//
STATIC FUNCTION rgb( r,g,b )

RETURN ( r + ( g * 256 ) + ( b * 256 * 256 ) )

//-------------------------------------------------------------------//
FUNCTION VouChoice( aChoices )

   LOCAL scr, clr, nChoice

   DEFAULT aChoices TO { 'One','Two','Three','Four','Five','Six','Seven' }

   scr := SaveScreen( 7,48,13,55 )
   clr := SetColor( 'N/W*,GR+/B*,,,GR+/B' )

   nChoice := aChoice( 7, 48, 13, 55, aChoices )

   setColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

RETURN nChoice

//-------------------------------------------------------------------//
PROCEDURE VouBrowse()

   LOCAL nKey, bBlock, oBrowse , aLastPaint, i
   LOCAL lEnd    := .F.
   LOCAL aBlocks := {}
   LOCAL aStruct := {}
   LOCAL nTop    :=  3
   LOCAL nLeft   :=  3
   LOCAL nBottom := maxrow() - 2
   LOCAL nRight  := maxcol() - 3

   STATIC nStyle := 0

   SAVE WINDOW

   SetColor( 'N/W*,N/GR*,,,N/W* ' )
   setCursor( 0 )
   USE 'TEST' NEW
   IF NetErr()
      RETURN NIL
   ENDIF
   aStruct := DbStruct()

   oBrowse := TBrowseNew( nTop + 3, nLeft + 2, nBottom - 1, nRight - 2 )

   oBrowse:ColSep        = '  '
   oBrowse:HeadSep       = '__'
   oBrowse:GoTopBlock    = { || dbGoTop() }
   oBrowse:GoBottomBlock = { || dbGoBottom() }
   oBrowse:SkipBlock     = { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   FOR i := 1 TO Len( aStruct )
       bBlock := VouBlockField( i )
       oBrowse:AddColumn( TBColumnNew( aStruct[ i,1 ], bBlock ) )
   NEXT

   oBrowse:configure()

   IF nStyle > 5
      nStyle := 0
   ENDIF

   SET PEN STYLE nStyle WIDTH 0 COLOR rgb( 210,1210,210 )

   nStyle++

   // Use functions
   WVT_AddPaintList( {|| Wvt_SetIcon( 'DIA_EXCL.ico' ) } )
   WVT_AddPaintList( {|| Wvt_SetTitle( 'WVT Gui TBrowse()' ) } )
   WVT_AddPaintList( {|| Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight ) } )
   WVT_AddPaintList( {|| Wvt_DrawBoxRecessed( nTop+3, nLeft+2, nBottom-1, nRight-2 ) } )
   WVT_AddPaintList( {|| Wvt_DrawGridHorz( oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   WVT_AddPaintList( {|| Wvt_DrawGridVert( oBrowse:nTop, oBrowse:nBottom, oBrowse:aColumnsSep, len( oBrowse:aColumnsSep ) ) } )

   DispBox( 0, 0, maxrow(), maxcol(), '         ', 'N/W' )
   DispOutAt( nTop + 1, nleft, padc( CurDrive()+':\'+CurDir()+'\'+'Test.dbf', nRight - nLeft + 1 ), 'W+/W' )

    WHILE !lEnd
      oBrowse:ForceStable()

      nKey = InKey( 0 )

      DO CASE
      CASE nKey == K_ESC
           lEnd = .t.

      CASE nKey == K_ENTER
           lEnd := .t.

      CASE nKey == K_DOWN
           oBrowse:Down()

      CASE nKey == K_UP
           oBrowse:Up()

      CASE nKey == K_LEFT
           oBrowse:Left()

      CASE nKey == K_RIGHT
           oBrowse:Right()

      CASE nKey = K_PGDN
           oBrowse:pageDown()

      CASE nKey = K_PGUP
           oBrowse:pageUp()

      CASE nKey = K_CTRL_PGUP
           oBrowse:goTop()

      CASE nKey = K_CTRL_PGDN
           oBrowse:goBottom()

      CASE nKey = K_HOME
           oBrowse:home()

      CASE nKey = K_END
           oBrowse:end()

      CASE nKey = K_CTRL_LEFT
           oBrowse:panLeft()

      CASE nKey = K_CTRL_RIGHT
           oBrowse:panRight()

      CASE nKey = K_CTRL_HOME
           oBrowse:panHome()

      CASE nKey = K_CTRL_END
           oBrowse:panEnd()

      ENDCASE
   END

   SET PEN STYLE 0
   RESTORE WINDOW

   DBCloseArea()

RETURN
//-------------------------------------------------------------------//
STATIC FUNCTION DbSkipBlock( n, oTbr )

   LOCAL nSkipped := 0

   IF n = 0
      DBSkip( 0 )

   ELSEIF n > 0
      DO WHILE nSkipped != n .and. TBNext( oTbr )
         nSkipped++
      ENDDO
   ELSE
      DO WHILE nSkipped != n .and. TBPrev( oTbr )
         nSkipped--
      ENDDO
   ENDIF

RETURN nSkipped

//-------------------------------------------------------------------//
STATIC FUNCTION TBNext( oTbr )

   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   if Eof()
      lMoved := .F.
   else
      DBSkip( 1 )
      if Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      endif
   endif

RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION TBPrev( oTbr )
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION VouBlockField( i )

RETURN  { || fieldget( i ) }
//-------------------------------------------------------------------//

