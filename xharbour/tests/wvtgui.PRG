//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
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
//             I feel that if this functionality is built
//                 into the GT itself, what a wonder!
//
//   This protocol opens up the the distinct possibilities and hope
//            you all will cooperate to enhance it further. 
//                       
//           Thanks Peter Rees! You have laid the foundation!
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include    'inkey.ch'
#include   'common.ch'

//-------------------------------------------------------------------//

function Main()

local aLastPaint, clr, scr
local get_1   := ctod( '' )
local get_2   := pad( 'Pritpal Bedi', 35 )
local get_3   := pad( '60, New Professor Colony', 35 )
local get_4   := pad( 'Ludhiana, INDIA', 35 )
local get_5   := pad( 'http://www.vouchcac.com', 35 )
local get_6   := 20000
local aBlocks := {}
local nColGet := 8
local GetList := {}
local nTop    := 4
local nLft    := 4
local nBtm    := 20
local nRgt    := 75
local kf2     := setKey( K_F2, {|| GuiGets_1() } )
local kf3     := setKey( K_F3, {|| Wvt_WindowExpand( 1 ) } )
local kf4     := setKey( K_F4, {|| Wvt_WindowExpand( -1 ) } )
local cLabel  := 'VOUCH, that GROWS with you'
local box_    := ;
      {{ 7,nColGet,8 },{10,nColGet,35},{13,nColGet,35},{15,nColGet,35},{17,nColGet,35},{17,61,10} }
local aGetsParam := ;
      {{ 7,nColGet,8 },{10,nColGet,35},{13,nColGet,35},{15,nColGet,35},{17,nColGet,35},{17,61,10} }

SET DATE BRITISH

get_1 := ctod( '04/01/04' )

Wvt_SetFont( 'Courier New', 18 )
// aadd( aBlocks, {|| Wvt_SetFont( 'Courier New', 18 ) } )

aadd( aBlocks, {|| Wvt_SetIcon( 'vr_1.ico' ) } )
aadd( aBlocks, {|| Wvt_SetTitle( 'Vouch' ) } )
aadd( aBlocks, {|| Wvt_DrawLabel( 1,40, cLabel,6,, rgb(255,255,255), ;
                      rgb(198,198,198), 'Arial', 26, , , , , .t., .t. ) } )
aadd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt ) } )
aadd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) } )
aadd( aBlocks, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) } )
aadd( aBlocks, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) } )
aadd( aBlocks, {|| Wvt_DrawImage( 8,62,12,69, 'vouch1.bmp' ) } )
aadd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) } )
aadd( aBlocks, {|| aeval( box_, {|o_| Wvt_DrawBoxGet( o_[ 1 ], o_[ 2 ], o_[ 3 ] ) } ) } )

aLastPaint := Wvt_SetBlocks( aBlocks )

scr := SaveScreen( 0,0,maxrow(),maxcol() )
clr := SetColor( 'N/W' )

CLS

SetColor( 'N/W,N/GR*,,,N/W*' )

@ maxrow(), 0 SAY padc( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'
@ maxrow()-2, 8 say 'F2 New Screen' COLOR 'GR+/B'
@ maxrow()-2,24 say 'F3 Expand'     COLOR 'GR+/BG'
@ maxrow()-2,34 say 'F4 Shrink'     COLOR 'GR+/G'

VouSay( { {  6, nColGet, '< Date >' },;
          {  9, nColGet, '<'+padc('Name'   ,33)+'>'},;
          { 12, nColGet, '<'+padc('Address',33)+'>'},;
          { 16,      61, '< Salary >' } } )

@  7, nColGet GET get_1
@ 10, nColGet GET get_2 VALID ( VouChoice() < 7 )
@ 13, nColGet GET get_3 
@ 15, nColGet GET get_4 
@ 17, nColGet GET get_5 
@ 17, 61      GET get_6 PICTURE '@Z 9999999.99'

Read

//  Restore Environment
//
Wvt_setBlocks( aLastPaint )
SetColor( clr )
RestScreen( 0,0,maxrow(),maxcol(), scr )
SetKey( K_F2, kf2 )
SetKey( K_F3, kf3 )
SetKey( K_F4, kf4 )

return nil

//-------------------------------------------------------------------//

function GuiGets_1()

local aLastPaint, clr, scr
local get_1   := ctod( '' )
local get_2   := space( 35 )
local get_3   := space( 35 )
local get_4   := space( 35 )
local get_5   := space( 35 )
local get_6   := 0
local aBlocks := {}
local nColGet := 8
local GetList := {}
local nTop    := 4
local nLft    := 4
local nBtm    := 20
local nRgt    := 75
local kf2     := setKey( K_F2, {|| GuiGets_1() } )
local kf3     := setKey( K_F3, {|| Wvt_WindowExpand( 1 ) } )
local kf4     := setKey( K_F4, {|| Wvt_WindowExpand( -1 ) } )
local cLabel  := 'VOUCH, that GROWS with you'
local box_    := ;
      {{ 7,nColGet,8 },{10,nColGet,35},{13,nColGet,35},{15,nColGet,35},{17,nColGet,35},{17,61,10} }
local aGetsParam := ;
      {{ 7,nColGet,8 },{10,nColGet,35},{13,nColGet,35},{15,nColGet,35},{17,nColGet,35},{17,61,10} }


aadd( aBlocks, {|| Wvt_SetFont( 'Courier New', 24 ) } )
aadd( aBlocks, {|| Wvt_SetIcon( 'DIA_EXCL.ico' ) } )
aadd( aBlocks, {|| Wvt_SetTitle( 'Vouch Gets 2' ) } )
aadd( aBlocks, {|| Wvt_DrawBoxGroupRaised( 5, 6, 19, 72 ) } )
aadd( aBlocks, {|| aeval( box_, {|o_| Wvt_DrawBoxGet( o_[ 1 ], o_[ 2 ], o_[ 3 ] ) } ) } )

aLastPaint := Wvt_SetBlocks( aBlocks ) 
scr        := SaveScreen( 0,0,maxrow(),maxcol() )
clr        := SetColor( 'N/W' )

CLS

SetColor( 'N/W,N/GR*,,,N/W*' )

@ maxrow(), 0 SAY padc( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'

VouSay( { {  6,nColGet,'< Date >' },;
          {  9,nColGet,'<'+padc('Name'   ,33)+'>'},;
          { 12,nColGet,'<'+padc('Address',33)+'>'},;
          { 16,61,'< Salary >' } } )

@  7, nColGet GET get_1
@ 10, nColGet GET get_2 VALID ( VouChoice() < 7 )
@ 13, nColGet GET get_3 
@ 15, nColGet GET get_4 
@ 17, nColGet GET get_5 
@ 17, 61      GET get_6 PICTURE '@Z 9999999.99'

Read

// Restore Environment
//
Wvt_setBlocks( aLastPaint )
SetColor( clr )
RestScreen( 0,0,maxrow(),maxcol(), scr )

return nil

//-------------------------------------------------------------------//
//
//      Wvt_Paint() must be a function in your application
//      as it is called when Window gets WM_PAINT message.
//
//-------------------------------------------------------------------//

function Wvt_Paint( hWnd, msg, wParam, lParam )
local aBlocks := Wvt_SetBlocks()

aeval( aBlocks, {|e| eval( e ) } )

return 0

//-------------------------------------------------------------------//
//
//  Wvt_SetBlocks() is a get/set function to be used by Wvt_Paint()
//
function Wvt_SetBlocks( a_ )
local o
static s := {}

o := aclone( s )

if a_ <> nil
	s := aclone( a_ )
endif

return o

//-------------------------------------------------------------------//
//
//                       Utility Functions 
//
//-------------------------------------------------------------------//

function Wvt_WindowExpand( nUnits )
static sUnits := 18

sUnits += nUnits

Wvt_setFont( 'Courier New', sUnits )

return .t.

//-------------------------------------------------------------------//

static function rgb( r,g,b )

return ( r + ( g * 256 ) + ( b * 256 * 256 ) )

//-------------------------------------------------------------------//

function VouGetsGui( box_ )

aeval( box_, {|o_| Wvt_DrawBoxGet( o_[ 1 ], o_[ 2 ], o_[ 3 ] ) } )

return .t.

//-------------------------------------------------------------------//

function vousay( a_ )
local i

for i := 1 to len( a_ )
	@ a_[ i,1 ], a_[ i,2 ] SAY a_[ i,3 ]
next

return .t.

//-------------------------------------------------------------------//

function VouChoice( aChoice_ )
local scr, clr, nChoice

DEFAULT aChoice_ TO { 'One','Two','Three','Four','Five','Six','Seven' }

scr := SaveScreen( 7,48,13,55 )
clr := SetColor( 'N/W*,GR+/B*,,,GR+/B' )

nChoice := achoice( 7, 48, 13, 55, aChoice_ )

setColor( clr )
RestScreen( 7, 48, 13, 55, scr )

return nChoice

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

