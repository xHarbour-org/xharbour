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
//
//   Wvt_DrawButton() constants
//
#define WVT_BTN_FORMAT_RAISED      0   // Default
#define WVT_BTN_FORMAT_RECESSED    1   
#define WVT_BTN_FORMAT_FLAT        2
#define WVT_BTN_FORMAT_OUTLINED    3

#define WVT_BTN_IMAGE_TOP          0   // Default
#define WVT_BTN_IMAGE_LEFT         1
#define WVT_BTN_IMAGE_BOTTOM       2
#define WVT_BTN_IMAGE_RIGHT        3

//-------------------------------------------------------------------//
//
//   WvtSetObjects() array structure
//
#define WVT_OBJ_TYPE               1
#define WVT_OBJ_ID                 2
#define WVT_OBJ_ROW                3
#define WVT_OBJ_COL                4
#define WVT_OBJ_ROWTO              5
#define WVT_OBJ_COLTO              6
#define WVT_OBJ_ONDISP             7
#define WVT_OBJ_ONMOUSEOVER        8
#define WVT_OBJ_ONBUTTONDOWN       9
#define WVT_OBJ_ONBUTTONUP        10
#define WVT_OBJ_TOOLTIP           11
#define WVT_OBJ_STATE             12
#define WVT_OBJ_DUMMY             13

#define WVT_OBJ_VRBLS             13

//   WVT_OBJ_TYPE  Constants
//
#define OBJ_TYPE_BUTTON            1

//   WVT_OBJ_STATE
//
#define OBJ_STATE_HIDE             0
#define OBJ_STATE_DISP             1
#define OBJ_STATE_MOUSEOVER        2 
#define OBJ_STATE_BUTTONDOWN       3
#define OBJ_STATE_BUTTONUP         4

//-------------------------------------------------------------------//
//
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat,;
//                 nAlign, nStyle, nThick, nColor ) 
//
//   nOrient
#define WVT_LINE_HORZ              0   // Default
#define WVT_LINE_VERT              1

//   nFormat
#define WVT_LINE_RAISED            0   // Default
#define WVT_LINE_RECESSED          1   
#define WVT_LINE_PLAIN             2

//   nAlign 
#define WVT_LINE_CENTER            0   // Default
#define WVT_LINE_TOP               1
#define WVT_LINE_BOTTOM            2
#define WVT_LINE_LEFT              3
#define WVT_LINE_RIGHT             4

//   nStyle
#define WVT_LINE_SOLID             0   // Default
#define WVT_LINE_DASH              1   
#define WVT_LINE_DOT               2
#define WVT_LINE_DASHDOT           3
#define WVT_LINE_DASHDOTDOT        4

//-------------------------------------------------------------------//

static wvtScreen := {}

//-------------------------------------------------------------------//
PROCEDURE Main()

   LOCAL aLastPaint, clr, scr, bWhen, bValid
   LOCAL dDate   := ctod( '' )
   LOCAL cName   := Pad( 'Pritpal Bedi', 35 )
   LOCAL cAdd1   := Pad( '60, New Professor Colony', 35 )
   LOCAL cAdd2   := Pad( 'Ludhiana, INDIA', 35 )
   LOCAL cAdd3   := Pad( 'http://www.vouchcac.com', 35 )
   LOCAL nSlry   := 20000
   LOCAL aBlocks := {}
   LOCAL nColGet := 8
   LOCAL GetList := {}
   LOCAL nTop    := 4
   LOCAL nLft    := 4
   LOCAL nBtm    := 20
   LOCAL nRgt    := 75
	LOCAL nMaxRows:= MaxRow()
   LOCAL nBtnRow := nMaxRows - 1
   LOCAL cLabel  := 'xHarbour simulated GUI.'
   LOCAL aObjects:= WvtSetObjects( {} )
   LOCAL aObj    := {}
   LOCAL kf2     := SetKey( K_F2, {|| WvtGets() } )
   LOCAL kf3     := SetKey( K_F3, {|| WvtWindowExpand( 1 ) } )
   LOCAL kf4     := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
   LOCAL kf5     := SetKey( K_F5, {|| WvtBrowse() } )
   LOCAL kf6     := SetKey( K_F6, {|| Wvt_Minimize() } )
   LOCAL kf7     := SetKey( K_F7, {|| WvtPartialScreen() } )
   LOCAL kf8     := SetKey( K_F8, {|| WvtLines() } )
   LOCAL kf9     := SetKey( K_F9, {|| Wvt_ChooseFont() } )
   LOCAL kf10    := SetKey( K_F10,{|| Wvt_ChooseColor() } )
            
   SET DATE BRITISH

   dDate := ctod( '04/01/04' )

   Wvt_SetFont( 'Courier New', 18, 0, 0 )
   Wvt_SetMouseMove( .t. )
   
   //  Force mouse pointer right below the xHarbour label
   //
	Wvt_SetMousePos( 2,40 )
	   
   aAdd( aBlocks, {|| Wvt_SetIcon( 'vr_1.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'Vouch' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLabel( 1,40, cLabel,6,, rgb(255,255,255), rgb(198,198,198), 'Arial', 26, , , , , .t., .t. ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 ) } )
   aAdd( aBlocks, {|| Wvt_DrawImage( 8,62,12,69, 'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-2,0,maxrow()-2,maxcol(),WVT_LINE_HORZ,WVT_LINE_RECESSED,WVT_LINE_BOTTOM ) } )

   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1, 4,maxrow(), 4,WVT_LINE_VERT,WVT_LINE_RECESSED,WVT_LINE_CENTER ) } )      
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,41,maxrow(),41,WVT_LINE_VERT,WVT_LINE_RECESSED,WVT_LINE_CENTER ) } )   

   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )
  
	#define btnFDisp   WVT_BTN_FORMAT_FLAT
	#define btnFMOver  WVT_BTN_FORMAT_RAISED
	#define btnFBDown  WVT_BTN_FORMAT_RECESSED
	#define btnFBUp    WVT_BTN_FORMAT_FLAT

   WvtSetObjects( { OBJ_TYPE_BUTTON, 1,  nBtnRow, 6,nBtnRow+1, 9, ;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,'vouch1.bmp',btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,'vouch1.bmp',btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,'vouch1.bmp',btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow, 6,nBtnRow+1, 9,  ,'vouch1.bmp',btnFBUp   )  ,;
              eval( SetKey( K_F2 ) ) } ;
                    } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 2,  nBtnRow,11,nBtnRow+1,14, ;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,'v_browse.ico', btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,'v_browse.ico', btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,'v_browse.ico', btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,11,nBtnRow+1,14,  ,'v_browse.ico', btnFBUp   )  ,;
              eval( SetKey( K_F5 ) ) } ;
                    } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 3,  nBtnRow,16,nBtnRow+1,19, ;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand','v_notes.ico',btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand','v_notes.ico',btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand','v_notes.ico',btnFBDown ) },;
       {|| Wvt_DrawButton( nBtnRow,16,nBtnRow+1,19,'Expand','v_notes.ico',btnFBUp   )  ,;
              eval( SetKey( K_F3 ) ) } ;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 4,  nBtnRow,21,nBtnRow+1,24, ;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFDisp , rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFMOver, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFBDown, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;       
       {|| Wvt_DrawButton( nBtnRow,21,nBtnRow+1,24,'Shrink',  , btnFBUp  , rgb( 100,22,241 ), rgb( 0,100,0 ) )  ,;
              eval( SetKey( K_F4 ) ) } ;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 5,  nBtnRow,26,nBtnRow+1,29, ;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize','v_tools.ico' , btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize','v_tools.ico' , btnFMOver ) },;       
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize','v_tools.ico' , btnFBDown ) },;       
       {|| Wvt_DrawButton( nBtnRow,26,nBtnRow+1,29,'Minimize','v_tools.ico' , btnFBUp   )  ,;       
              eval( SetKey( K_F6 ) ) },;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 6, nBtnRow,31,nBtnRow+1,34, ;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial','v_help.ico', btnFDisp  ) },;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial','v_help.ico', btnFMOver ) },;
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial','v_help.ico', btnFBDown ) },;       
       {|| Wvt_DrawButton( nBtnRow,31,nBtnRow+1,34,'Partial','v_help.ico', btnFBUp   )  ,;       
              eval( SetKey( K_F7 ) ) },;
                   } )

   WvtSetObjects( { OBJ_TYPE_BUTTON, 7,  nBtnRow,36,nBtnRow+1,39, ;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines','Vr_1.ico', btnFDisp , rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines','Vr_1.ico', btnFMOver, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines','Vr_1.ico', btnFBDown, rgb( 100,22,241 ), rgb( 0,100,0 ) ) },;       
       {|| Wvt_DrawButton( nBtnRow,36,nBtnRow+1,39,'Lines','Vr_1.ico', btnFBUp  , rgb( 100,22,241 ), rgb( 0,100,0 ) )  ,;
              eval( SetKey( K_F8 ) ) } ;
                   } )
                              
   aAdd( aBlocks, {|| Wvt_Mouse( -1000001 ) } )


   aLastPaint := WvtSetBlocks( aBlocks )

   scr := SaveScreen( 0,0,maxrow(),maxcol() )
   clr := SetColor( 'N/W' )

   CLS

   SetColor( 'N/W,N/GR*,,,N/W*' )

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33 ) + '>'
   @ 16, 61      SAY '< Salary >'

   @  7, nColGet GET dDate WHEN  DispStatusMsg( 'Date must be Valid' );
                           VALID ClearStatusMsg()
   @ 10, nColGet GET cName WHEN  DispStatusMsg( 'Must be one of the list!' );
                           VALID ( VouChoice() < 7 .and. ClearStatusMsg() )
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE '@Z 9999999.99'

   READ

   //  Restore Environment
   //
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )
   SetKey( K_F2, kf2 )
   SetKey( K_F3, kf3 )
   SetKey( K_F4, kf4 )
   SetKey( K_F5, kf5 )
   SetKey( K_F6, kf6 )
   SetKey( K_F7, kf7 )
   SetKey( K_F8, kf8 )      
RETURN

//-------------------------------------------------------------------//
PROCEDURE WvtGets()

   LOCAL aLastPaint, clr
   LOCAL dDate      := ctod( '' )
   LOCAL cName      := Space( 35 )
   LOCAL cAdd1      := Space( 35 )
   LOCAL cAdd2      := Space( 35 )
   LOCAL cAdd3      := Space( 35 )
   LOCAL nSlry      := 0
   LOCAL aBlocks    := {}
   LOCAL nColGet    := 8
   LOCAL GetList    := {}
   LOCAL nTop       := 4
   LOCAL nLft       := 4
   LOCAL nBtm       := 20
   LOCAL nRgt       := 75
   LOCAL kf2        := SetKey( K_F2, {|| WvtGets() } )
   LOCAL kf3        := SetKey( K_F3, {|| WvtWindowExpand( 1 ) } )
   LOCAL kf4        := SetKey( K_F4, {|| WvtWindowExpand( -1 ) } )
   LOCAL cLabel     := 'VOUCH, that GROWS with you'
   LOCAL aPalette   := Wvt_GetPalette()
   LOCAL aNewPalette:= aclone( aPalette )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL nRow       := Row()
   LOCAL nCol       := Col()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL wvtScr     := Wvt_SaveScreen( 0,0,maxrow(),maxcol() )         

   // Change the values of pallatte arbitrarily though yu can fine tune
   // these values with realistic values.
   //
   aNewPalette[ 8 ] := aNewPalette[ 8 ] + 100000

   Wvt_SetPalette( aNewPalette )

   aAdd( aBlocks, {|| Wvt_SetTitle( 'Wvt Gets 2nd Window with Different Palette' ) } )
   aAdd( aBlocks, {|| Wvt_DrawLine( maxrow()-1,0,maxrow()-1,maxcol() ) })
   aAdd( aBlocks, {|| Wvt_SetBrush( 0, rgb( 32,255,100 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawEllipse( 6,50,10,58 ) } )
   aAdd( aBlocks, {|| Wvt_SetBrush( 2, rgb( 255,255,100 ),1 ) } )
   aAdd( aBlocks, {|| Wvt_DrawRectangle( 11, 50, 13, 58 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxGroupRaised( 5, 6, 19, 72 ) } )
   aAdd( aBlocks, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )
   
   aAdd( aBlocks, {|| Wvt_DrawButton( 21, 6,22, 9,'New'   ,'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,11,22,14,'Browse','vouch1.bmp', 1, rgb( 255,255,255 ) ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,16,22,19, ,'vouch1.bmp' ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,21,22,24,'Data',, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )         
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,26,22,29,'Flat','vr_1.ico',2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawButton( 21,31,22,34,'Outline','vr_1.ico',3 ) } )   
   aAdd( aBlocks, {|| Wvt_DrawButton( 22,36,22,41,'Data',, 0, rgb( 100,22,241 ), rgb( 198,198,198 ) ) } )         

   aLastPaint := WvtSetBlocks( aBlocks )
   clr        := SetColor( 'N/W,N/GR*,,,N/W*' )

   CLS

   @ MaxRow(), 0 SAY PadC( 'xHarbour + WVT Console GUI Screen',80 ) COLOR 'R+/W'

   @  6, nColGet SAY '< Date >'
   @  9, nColGet SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12, nColGet SAY '<' + PadC( 'Address', 33) + '>'
   @ 16, 61      SAY '< Salary >'

   @  7, nColGet GET dDate
   @ 10, nColGet GET cName
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE '@Z 9999999.99'

   READ

   // Restore Environment
   //
   Wvt_SetPalette( aPalette )   
   WvtSetObjects( aObjects )
   WvtSetBlocks( aLastPaint )
   SetColor( clr )
   RestScreen( 0,0,maxrow(),maxcol(), scr )
   Wvt_RestScreen( wvtScr )
   SetPos( nRow, nCol )
RETURN

//-------------------------------------------------------------------//
//      Wvt_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//-------------------------------------------------------------------//
FUNCTION Wvt_Paint( hWnd, msg, wParam, lParam, nTop, nLeft, nBottom, nRight )

   LOCAL aBlocks := WvtSetBlocks()

   aEval( aBlocks, {|e| eval( e ) } )

RETURN 0

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
//
//      Wvt_Mouse() must be present if you want to catch and fire
//      mouse call back outside of the inkey() loop.
//
//-------------------------------------------------------------------//
FUNCTION Wvt_Mouse( nKey, nRow, nCol )

	LOCAL i, nLen, aObjects := WvtSetObjects()
   LOCAL nObj
   
   STATIC nLastObj := 0
   STATIC nLastKey := 0
      
	if ( nLen := len( aObjects ) ) == 0
		return nil
	endif
	
	if nKey == -1000001
		for nObj :=	1 to nLen
			DO CASE
			CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_DISP
				eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
			CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_MOUSEOVER	
				eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
			CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONDOWN
				eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
			CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_BUTTONUP
				eval( aObjects[ nObj, WVT_OBJ_ONDISP ] )
			CASE aObjects[ nObj, WVT_OBJ_STATE ] == OBJ_STATE_HIDE

			ENDCASE
		next
		return nil
	endif

   nObj := ascan( aObjects, {|e_| e_[ WVT_OBJ_ROW   ] <= nRow .and. ;
                                  e_[ WVT_OBJ_ROWTO ] >= nRow .and. ;
                                  e_[ WVT_OBJ_COL   ] <= nCol .and. ;
                                  e_[ WVT_OBJ_COLTO ] >= nCol     } )
	if nObj == 0
		if nLastObj > 0
		   aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
			eval( aObjects[ nLastObj, WVT_OBJ_ONDISP ] )
			nLastObj := 0
		endif
	   return nil
	endif

   if nLastObj == nObj .and. nLastKey == nKey
   	return nil
   endif
   
   nLastObj := nObj
	nLastKey := nKey
		   
	DO CASE
	CASE nKey == K_MOUSEMOVE
		if aObjects[ nLastObj, WVT_OBJ_STATE ] <> OBJ_STATE_MOUSEOVER
  			aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_MOUSEOVER
			if aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] <> nil
				eval( aObjects[ nObj, WVT_OBJ_ONMOUSEOVER ] )
			endif
		endif
			
	CASE nKey == K_LBUTTONDOWN
  		aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_BUTTONDOWN
  		if aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] <> nil
  			eval( aObjects[ nObj, WVT_OBJ_ONBUTTONDOWN ] )
   	endif
			
	CASE nKey == K_LBUTTONUP
  		aObjects[ nLastObj, WVT_OBJ_STATE ] := OBJ_STATE_DISP
  		if aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] <> nil
  			eval( aObjects[ nObj, WVT_OBJ_ONBUTTONUP ] )
   	endif
	
	ENDCASE
   
RETURN nil
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//  WvtSetBlocks() is a get/set FUNCTION to be used by Wvt_Paint()
//-------------------------------------------------------------------//
FUNCTION WvtSetBlocks( a_ )

   LOCAL o
   STATIC s := {}

   o := aClone( s )

   IF a_ <> nil
      s := aClone( a_ )
   ENDIF

RETURN o

//-------------------------------------------------------------------//
//  WvtSetObjects() is a get/set FUNCTION to be used by Wvt_Mouse()
//-------------------------------------------------------------------//
FUNCTION WvtSetObjects( aObject )

   LOCAL oObjects
   STATIC aObjects := {}
   
   oObjects := aclone( aObjects )
   
   if aObject <> nil
      if empty( aObject )
      	aObjects := {}
      else
         if valtype( aObject[ 1 ] ) == 'A'
				aeval( aObject, {|e_| aadd( aObjects, e_ ) } )
         else
	         aSize( aObject, WVT_OBJ_VRBLS )

	         DEFAULT aObject[ WVT_OBJ_STATE ] TO OBJ_STATE_DISP

		   	aadd( aObjects, aObject )
		   endif	
	   endif	
   endif

RETURN oObjects
//-------------------------------------------------------------------//
FUNCTION WvtWindowExpand( nUnits )

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
FUNCTION WvtBrowse()

   LOCAL nKey, bBlock, oBrowse , aLastPaint, i
   LOCAL lEnd    := .f.
	LOCAL aBlocks := {}
	LOCAL info_   := {}
	LOCAL nTop    :=  3
	LOCAL nLeft   :=  3
	LOCAL nBottom := maxrow() - 2
	LOCAL nRight  := maxcol() - 3
	LOCAL nCursor := setCursor( 0 )
	LOCAL nRow    := row()
	LOCAL nCol    := col()
   LOCAL cColor  := SetColor( 'N/W*,N/GR*,,,N/W* ' )
	LOCAL cScr    := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL aObjects:= WvtSetObjects( {} )
   
	STATIC nStyle := 0

	USE 'TEST' NEW
   if NetErr()
      return nil
   endif
   info_:= DbStruct()

	oBrowse := TBrowseNew( nTop + 3, nLeft + 2, nBottom - 1, nRight - 2 )

   oBrowse:ColSep        = '  '
   oBrowse:HeadSep       = '__'
   oBrowse:GoTopBlock    = { || dbGoTop() }
   oBrowse:GoBottomBlock = { || dbGoBottom() }
   oBrowse:SkipBlock     = { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

	for i := 1 to len( info_ )
		bBlock := VouBlockField( i )
	   oBrowse:AddColumn( TBColumnNew( info_[ i,1 ], bBlock ) )
	next

	oBrowse:configure()

   if nStyle > 5
      nStyle := 0
   endif

   Wvt_SetPen( nStyle, 0, rgb( 210,1210,210 ) )

   nStyle++

   aAdd( aBlocks, {|| Wvt_SetIcon( 'DIA_EXCL.ico' ) } )
   aAdd( aBlocks, {|| Wvt_SetTitle( 'WVT Gui TBrowse()' ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( nTop+3, nLeft+2, nBottom-1, nRight-2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridHorz( oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
	aAdd( aBlocks, {|| Wvt_DrawGridVert( oBrowse:nTop, oBrowse:nBottom, oBrowse:aColumnsSep, len( oBrowse:aColumnsSep ) ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

	DispBox( 0, 0, maxrow(), maxcol(), '         ', 'N/W' )
	DispOutAt( nTop + 1, nleft, padc( CurDrive()+':\'+CurDir()+'\'+'Test.dbf', nRight - nLeft + 1 ), 'W+/W' )

	While !lEnd
      oBrowse:ForceStable()

      nKey = InKey( 0 )

      do case
      case nKey == K_ESC
         lEnd = .t.

		case nKey == K_ENTER
			lEnd := .t.

      case nKey == K_DOWN
         oBrowse:Down()

      case nKey == K_UP
         oBrowse:Up()

      case nKey == K_LEFT
         oBrowse:Left()

      case nKey == K_RIGHT
         oBrowse:Right()

      case nKey = K_PGDN
         oBrowse:pageDown()

      case nKey = K_PGUP
         oBrowse:pageUp()

      case nKey = K_CTRL_PGUP
         oBrowse:goTop()

      case nKey = K_CTRL_PGDN
         oBrowse:goBottom()

      case nKey = K_HOME
         oBrowse:home()

      case nKey = K_END
         oBrowse:end()

      case nKey = K_CTRL_LEFT
         oBrowse:panLeft()

      case nKey = K_CTRL_RIGHT
         oBrowse:panRight()

      case nKey = K_CTRL_HOME
         oBrowse:panHome()

      case nKey = K_CTRL_END
         oBrowse:panEnd()

      endcase
   end

	Wvt_SetPen( 0 )
   WvtSetBlocks( aLastPaint )
	WvtSetObjects( aObjects )
	
   DevPos( nRow, nCol )
   SetColor( cColor )
   SetCursor( nCursor )

	DBCloseArea()
   RestScreen( 0,0, maxrow(),maxcol(), cScr )

RETURN nil
//-------------------------------------------------------------------//
STATIC FUNCTION DbSkipBlock( n, oTbr )

   LOCAL nSkipped := 0

   if n = 0
      DBSkip( 0 )

   elseif n > 0
      do while nSkipped != n .and. TBNext( oTbr )
         nSkipped++
      enddo
   else
      do while nSkipped != n .and. TBPrev( oTbr )
         nSkipped--
      enddo
   endif

RETURN  nSkipped

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

RETURN  {|| fieldget( i ) }
//-------------------------------------------------------------------//
FUNCTION WvtPartialScreen()
   LOCAL scr        := SaveScreen( 7,20,15,60 )
   LOCAL wvtScr     := Wvt_SaveScreen( 0, 0, MaxRow(), MaxCol() )         
   LOCAL wvtScr1 
   LOCAL aLastPaint 
   
   aLastPaint := WvtSetBlocks( {} )
   
   DispBox( 7, 20, 15, 60, '         ', 'W/GR*' )   
   @ 10,25 SAY 'Wvt_SaveScreen()' COLOR 'N/GR*'
   @ 11,25 SAY 'Wvt_RestScreen()' COLOR 'N/GR*'
   @ 13,25 SAY 'Press Esc '       COLOR 'N/GR*'   
   Wvt_DrawBoxRecessed( 8,22,14,58 )   

   wvtScr1 := Wvt_SaveScreen( 7,20,15,60 )
   
   do while inkey( 0 ) <> K_ESC
   enddo
   
   DispBox( 7, 20, 15, 60, '         ', 'W/B*' )   
   @ 10,25 SAY 'Wvt_SaveScreen()' COLOR 'N/B*'
   @ 11,25 SAY 'Wvt_RestScreen()' COLOR 'N/B*'
   @ 13,25 SAY 'Press Esc '       COLOR 'N/B*'   
   Wvt_DrawBoxRecessed( 8,22,14,58 )   

   do while inkey( 0 ) <> K_ESC
   enddo

   Wvt_RestScreen( 7,20,15,60, wvtScr1 )   
	
   do while inkey( 0 ) <> K_ESC
   enddo
  
   RestScreen( 7,20,15,60,scr )
   Wvt_RestScreen( 0, 0, MaxRow(), MaxCol(), wvtScr )
   WvtSetBlocks( aLastPaint )

RETURN NIL
//-------------------------------------------------------------------//

function WvtLines()
   LOCAL scr        := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL clr        := SetColor( 'N/W' )
   LOCAL nRows      := maxrow()
   LOCAL nCols      := maxcol()
   LOCAL aLastPaint := WvtSetBlocks( {} )
   LOCAL aObjects   := WvtSetObjects( {} )

   CLS
  
   Wvt_DrawLine( 0, 0, 0, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER )
   Wvt_DrawLine( 1, 0, 1, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_TOP )
   Wvt_DrawLine( 2, 0, 2, nCols, WVT_LINE_HORZ, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_SOLID, 4, Rgb( 255,255,255 ) )
   Wvt_DrawLine( 3, 0, 3, nCols, WVT_LINE_HORZ, WVT_LINE_RAISED  , WVT_LINE_CENTER, WVT_LINE_DASH , 0, Rgb( 255,0,0 ) )
   Wvt_DrawLine( 4, 0, 4, nCols, WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_BOTTOM )

   @ 0, 1 SAY 'Center Raised' 
   @ 1,11 say 'Top Recessed'
   @ 2,21 say 'Center Plain White 3 Pixels'
   @ 3,31 say 'Center Raised Dotted'
   @ 4,41 SAY 'Bottom Recessed'
   @ 5, 1 SAY 'Bottom Checked'

   @ nRows, 0 Say PadC( 'Press ESC to Quit', nCols+1 ) COLOR 'GR+/W'

   Wvt_DrawLine( 11, 5,nRows-2, 5, WVT_LINE_VERT, WVT_LINE_RAISED  , WVT_LINE_CENTER )
   Wvt_DrawLine( 11, 6,nRows-2, 6, WVT_LINE_VERT, WVT_LINE_RECESSED, WVT_LINE_CENTER )
   Wvt_DrawLine( 11, 7,nRows-2, 7, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_LEFT   )
   Wvt_DrawLine( 11, 8,nRows-2, 8, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER )
   Wvt_DrawLine( 11, 9,nRows-2, 9, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_RIGHT  )
   Wvt_DrawLine( 11,10,nRows-2,10, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DOT,     0, RGB( 0,0,255 ) )
   Wvt_DrawLine( 11,11,nRows-2,11, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASH,    0, RGB( 255,0,0 ) )
   Wvt_DrawLine( 11,12,nRows-2,12, WVT_LINE_VERT, WVT_LINE_PLAIN   , WVT_LINE_CENTER, WVT_LINE_DASHDOT, 0, RGB( 0,255,0 ) )

   @ 12,5 Say 'A'
   @ 13,6 Say 'B'
   @ 14,7 Say 'C'
   @ 15,8 Say 'D'
   @ 16,9 Say 'E'

	do while ( inkey(0) <> 27 )
	enddo

	//  Restore Environments
   //
   SetColor( clr )

   RestScreen( 0,0,maxrow(),maxcol(), scr )
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )

RETURN nil

//-------------------------------------------------------------------//

FUNCTION DispStatusMsg( cMsg )

Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, rgb(198,198,198), 'Arial', 18, , 900 )

RETURN .t.

//-------------------------------------------------------------------//

FUNCTION ClearStatusMsg()
   LOCAL nRow := Row()
   LOCAL nCol := Col()
   
   DispOutAt( MaxRow(), 42, space( 37 ), 'W/W' )
   
   SetPos( nRow, nCol )
   
RETURN .t.

//-------------------------------------------------------------------//

