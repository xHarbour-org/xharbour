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

#include    'inkey.ch'
#include   'common.ch'

//-------------------------------------------------------------------//
PROCEDURE Main()

   LOCAL aLastPaint, clr, scr
   LOCAL get_1   := ctod( '' )
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
   LOCAL cLabel  := 'xHarbour simulated GUI.'

   SET DATE BRITISH

   get_1 := ctod( '04/01/04' )

   Wvt_SetFont( 'Courier New', 18 )
   // aAdd( aBlocks, {|| Wvt_SetFont( 'Courier New', 18 ) } )

   aAdd( aBlocks, {|| Wvt_SetIcon( 'vr_1.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'Vouch' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLabel( 1,40, cLabel,6,, rgb(255,255,255), rgb(198,198,198), 'Arial', 26, , , , , .t., .t. ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) } )
   aAdd( aBlocks, {|| Wvt_DrawImage( 8,62,12,69, 'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   aLastPaint := Wvt_SetBlocks( aBlocks )

   scr := SaveScreen( 0,0,maxrow(),maxcol() )
   clr := SetColor( 'N/W' )

   CLS

   SetColor( 'N/W,N/GR*,,,N/W*' )

   @ MaxRow(), 0 SAY PadC( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'
   @ MaxRow()-2, 8 say 'F2 New Screen' COLOR 'GR+/B'
   @ MaxRow()-2,24 say 'F3 Expand'     COLOR 'GR+/BG'
   @ MaxRow()-2,34 say 'F4 Shrink'     COLOR 'GR+/G'

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
   Wvt_setBlocks( aLastPaint )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )
   SetKey( K_F2, kf2 )
   SetKey( K_F3, kf3 )
   SetKey( K_F4, kf4 )

RETURN

//-------------------------------------------------------------------//
PROCEDURE GuiGets_1()

   LOCAL aLastPaint, clr, scr
   LOCAL get_1   := ctod( '' )
   LOCAL get_2   := Space( 35 )
   LOCAL get_3   := Space( 35 )
   LOCAL get_4   := Space( 35 )
   LOCAL get_5   := Space( 35 )
   LOCAL get_6   := 0
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
   LOCAL cLabel  := 'VOUCH, that GROWS with you'

   aAdd( aBlocks, {|| Wvt_SetFont( 'Courier New', 24 ) } )
   aAdd( aBlocks, {|| Wvt_SetIcon( 'DIA_EXCL.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'Vouch Gets 2' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,0,maxrow()-1,maxcol() ) })
   aAdd( aBlocks, {|| Wvt_SetBrush( 0, rgb( 32,255,100 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawEllipse( 6,50,10,58 ) } )
   aAdd( aBlocks, {|| Wvt_SetBrush( 2, rgb( 255,255,100 ),1 ) } )
   aAdd( aBlocks, {|| Wvt_DrawRectangle( 11, 50, 13, 58 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroupRaised( 5, 6, 19, 72 ) } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   aLastPaint := Wvt_SetBlocks( aBlocks )
   scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   clr        := SetColor( 'N/W' )

   CLS

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
   Wvt_setBlocks( aLastPaint )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )

RETURN

//-------------------------------------------------------------------//
//      Wvt_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//-------------------------------------------------------------------//
FUNCTION Wvt_Paint( hWnd, msg, wParam, lParam )

   LOCAL aBlocks := Wvt_SetBlocks()

   aEval( aBlocks, {|e| eval( e ) } )

RETURN 0

//-------------------------------------------------------------------//
//  Wvt_SetBlocks() is a get/set FUNCTION to be used by Wvt_Paint()
//-------------------------------------------------------------------//
FUNCTION Wvt_SetBlocks( a_ )

   LOCAL o
   STATIC s := {}

   o := aClone( s )

   IF a_ <> nil
      s := aClone( a_ )
   ENDIF

RETURN o

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
FUNCTION Wvt_SetFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Gained!', 'r/w' )

   DevPos( nRow, nCol )

RETURN nil

//-------------------------------------------------------------------//
FUNCTION Wvt_KillFocus( hWnd )

   LOCAL nRow := row()
   LOCAL nCol := col()

   DispOutAt( 1,3, 'Focus Lost...', 'B/w' )

   DevPos( nRow, nCol )

RETURN nil
//-------------------------------------------------------------------//
