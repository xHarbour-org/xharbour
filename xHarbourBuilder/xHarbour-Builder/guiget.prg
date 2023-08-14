#include "hbclass.ch"

#include "winapi.ch"


//#command READ => GUIRead( GetList )
REQUEST WVT_SETFOCUS

PROCEDURE Main()

   LOCAL cVar := Space(30)//"(818)345-0122"
   LOCAL cTargetType := ""

   SET COLOR TO N/W
   CLS

   @ 0, 0 SAY 'Left'
   @ 0, MaxCol() - 1 SAY Str( MaxCol(), 2 )

   Inkey()
   IF LastKey() == 27
   @ 2,5 SAY "Name:" GET cVar PICTURE "@K"//"(999)999-9999"
   ENDIF

   @ 3,5 SAY "Name:" GET cVar PICTURE "@K"//"(999)999-9999"
   @ 4,5 SAY "Name:" GET cVar PICTURE "@K!"//"(999)999-9999"

   @ 13, 20, 18, 30 GET cTargetType   LISTBOX { "exe", "lib", "dll" } ;
                                      WHEN cTargetType == "" ;
                                      CAPTION "&Type:" ;
                                      MESSAGE "Use [Up], [Down], or [Space] to select." ;
                                      DROPDOWN ;
                                      COLOR "B/W, B/W, N/W, W+/B, N/W, N/W, R/W, R/W"

   SET CURSOR OFF
   READ

RETURN

FUNCTION GetNew( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )
RETURN GUIGet():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

//---------------------------------------------------------------------------//

FUNCTION __GET( bSetGet, cVarName, cPicture, bValid, bWhen )

   LOCAL oGet

   IF bSetGet == NIL
      IF Left( cVarName, 3 ) == "M->"
         cVarName := SubStr( cVarName, 4 )
         bSetGet := {|_1| iif( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSEIF FieldPos( cVarName ) > 0
         bSetGet := &( "{|_1| IIF( _1 == NIL, FIELD->" + cVarName + ", FIELD->" + cVarName + " := _1 ) }" )
      ELSEIF __MVEXIST( cVarName )
         bSetGet := {|_1| iif( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSE
         bSetGet := &( "{|_1| IIF( _1 == NIL, " + cVarName + ", " + cVarName + " := _1 ) }" )
      ENDIF
   ENDIF

   oGet := GUIGet():New( , ,bSetGet, cVarName, cPicture )

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet

FUNCTION __GETA( bGetArray, cVarName, cPicture, bValid, bWhen, aIndex )

   LOCAL oGet
   LOCAL nDim := Len( aIndex )
   LOCAL bSetGet
   LOCAL aGetVar
   LOCAL nCounter

   IF bGetArray == NIL
      IF __MVEXIST( cVarName )
         aGetVar := __MVGET( cVarName )
      ELSE
         aGetVar := &cVarName
      ENDIF
   ELSE
      aGetVar := Eval( bGetArray )
   ENDIF

   FOR nCounter := 1 TO nDim - 1
      aGetVar := aGetVar[ aIndex[ nCounter ] ]
   NEXT
   bSetGet := {|_1| iif( _1 == NIL, aGetVar[ aIndex[ nCounter ] ], aGetVar[ aIndex[ nCounter ] ] := _1 ) }

   oGet := GUIGet():New(,, bSetGet, cVarName, cPicture )
   oGet:SubScript := aIndex

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet

CLASS GUIGet FROM GET
   DATA hWnd, hDC
   DATA TextMetric INIT HB_CStructure( "TEXTMETRIC" ):Init( {} )

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )
   METHOD Display( lForced )
   //METHOD KillFocus()
   //METHOD Reset()
   METHOD SetFocus()

ENDCLASS

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS GUIGet

   LOCAL xRet := ::Super:New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )
   LOCAL TextMetric

   ::PutMask( ::VarGet(), .f. )
   ::hWnd := CreateGUIGet( ::Row, ::Col, ::nDispLen )
   ::hDC := GetDC( ::hWnd )
   GetTextMetrics( ::hDC, @TextMetric )
   ::TextMetric := (struct TEXTMETRIC*) TextMetric

RETURN xRet

METHOD Display( lForced ) CLASS GUIGet

   LOCAL xVar, xBuffer
   //LOCAL CursorPos IS POINT
   LOCAL Size
   LOCAL nPos

   IF lForced == NIL
      lForced := .T.
   ENDIF

   IF ::buffer == NIL
      xVar      := ::VarGet() // In VarGet() is setting ::xVarGet needed to ::Picture.
      ::Picture := ::cPicture
      xBuffer   := ::PutMask( xVar, .f. )
   ELSE
      xBuffer   := ::buffer
   ENDIF

   IF ! ::lMinusPrinted .AND. ! Empty( ::DecPos ) .AND. ::minus .AND. SubStr( xBuffer, ::DecPos - 1, 1 ) == "0"
      xBuffer := SubStr( xBuffer, 1, ::DecPos - 2 ) + "-." + SubStr( xBuffer, ::DecPos + 1 )
   ENDIF

   IF ::HasScroll() .and. ::Pos != NIL
      IF ::nDispLen > 8
         ::nDispPos := Max( 1, Min( ::Pos - ::nDispLen + 4, ::nMaxLen - ::nDispLen + 1 ) )
      ELSE
         ::nDispPos := Max( 1, Min( ::Pos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      ENDIF
   ENDIF

   IF xBuffer != NIL .and. ( lForced .or. ( ::nDispPos != ::nOldPos ) )
      xBuffer := Substr( xBuffer, ::nDispPos, ::nDispLen )
   ENDIF

   SetWindowText( ::hWnd, xBuffer )

   IF ::Buffer != NIL
      nPos := ::Pos - ::nDispPos + IIF( ::cDelimit == NIL, 0, 1 )
      GetTextExtentPoint32( ::hDC, Left( xBuffer, nPos ), nPos, @Size )
      Size := (struct SIZE*) Size
      //GUISetCaretPos( ::Pos - ::nDispPos + IIF( ::cDelimit == NIL, 0, 1 ) )
      SetCaretPos( Size:cx - ::TextMetric:tmOverhang, 3 )
      //SendMessage( ::hWnd, EM_SETSEL, nPos, nPos )
   ENDIF

RETURN Self

/*
METHOD KillFocus() CLASS GUIGet

RETURN

METHOD Reset() CLASS GUIGet

RETURN
*/

METHOD SetFocus() CLASS GUIGet

   //LOCAL CursorPos IS POINT
   LOCAL Size

   ::Super:SetFocus()

   SET CURSOR OFF

   SetFocus( ::hWnd )

   GetTextExtentPoint32( ::hDC, ::Buffer, ::Pos - ::nDispPos + IIF( ::cDelimit == NIL, 0, 1 ), @Size )
   Size := (struct SIZE*) Size

   //GUISetCaretPos( ::Pos - ::nDispPos + IIF( ::cDelimit == NIL, 0, 1 ) )
   SetCaretPos( Size:cx - ::TextMetric:tmOverhang, 3 )
   //SendMessage( ::hWnd, EM_SETSEL, ::Pos - ::nDispPos, ::Pos - ::nDispPos )

RETURN Self

#pragma BEGINDUMP

#include <windows.h>
#include "hbgtwvt.h"

static WNDPROC s_EditProc;

static GLOBAL_DATA *_ps = NULL;

static HWND s_hGUIGet = 0;

static HFONT s_hFont;

extern HANDLE  hb_hInstance;

static LRESULT CALLBACK GUIGetProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   //return CallWindowProc( s_EditProc, hWnd, message, wParam, lParam );

   switch( message )
   {
      case WM_SETFOCUS:
         s_hGUIGet = hWnd;
         CreateCaret( hWnd, NULL, 2, (int) (_ps->PTEXTSIZE.y * 0.7) );
         ShowCaret( hWnd );
         return 0;

      case WM_KILLFOCUS:
        if( IsChild( _ps->hWnd, (HWND) wParam ) )
        {
            s_hGUIGet = 0;
            //HideCaret( hWnd );
            //DestroyCaret();
        }
        return CallWindowProc( s_EditProc, hWnd, message, wParam, lParam );

      case WM_KEYDOWN:
         PostMessage( _ps->hWnd, WM_KEYDOWN, wParam, lParam );
         return 0;

       case WM_KEYUP:
         PostMessage( _ps->hWnd, WM_KEYUP, wParam, lParam );
         return 0;

       case WM_CHAR:
         //TraceLog( NULL, "WM_CHAR\n" );
         //PostMessage( _ps->hWnd, WM_CHAR, wParam, lParam );
         return 0;

      default:
      ;
   }

   return CallWindowProc( s_EditProc, hWnd, message, wParam, lParam );
}

HB_FUNC( GUISETCARETPOS )
{
   SetCaretPos( hb_parnl( 1 ) * _ps->PTEXTSIZE.x, 3 );
}

HB_FUNC( CREATEGUIGET )
{
   int  nRow    = hb_parni(1);
   int  nCol    = hb_parni(2);
   int  nWidth  = hb_parni(3);
   HWND hWnd;

   if( _ps == NULL )
   {
      hb_wvt_gtSetFont( "Courier New", 22, 0, 0, DEFAULT_QUALITY );

      _ps = hb_wvt_gtGetGlobalData();

      s_hFont = CreateFont( -(int)(_ps->PTEXTSIZE.y * 0.7),                  // height of font
                            0,                   // average character width
                            0,                   // angle of escapement
                            0,                   // base-line orientation angle
                            0,                   // font weight
                            FALSE,               // italic attribute option
                            FALSE,               // underline attribute option
                            FALSE,               // strikeout attribute option
                            DEFAULT_CHARSET,     // character set identifier
                            OUT_DEFAULT_PRECIS,  // output precision
                            CLIP_DEFAULT_PRECIS, // clipping precision
                            DEFAULT_QUALITY,     // output quality
                            FF_DONTCARE,         // pitch and family
                            NULL      // typeface name
                       );
   }

   /*
   TraceLog( NULL, "CreateGUIGet( %i, %i, %i ) (%i, %i, %i) Height %i\n", nRow, nCol, nWidth,
            (int)(nCol * _ps->PTEXTSIZE.x), (int)(nRow * _ps->PTEXTSIZE.y), (int)(nWidth * _ps->PTEXTSIZE.x),
             _ps->PTEXTSIZE.y );
   */

   hWnd = CreateWindowEx( WS_EX_WINDOWEDGE, "EDIT", NULL, WS_CHILD | WS_BORDER | WS_VISIBLE | WS_DISABLED,
                               nCol * _ps->PTEXTSIZE.x,
                               nRow * _ps->PTEXTSIZE.y,
                               nWidth * _ps->PTEXTSIZE.x,
                               _ps->PTEXTSIZE.y - 1,
                               _ps->hWnd, NULL, hb_hInstance, NULL );

   SendMessage( hWnd, WM_SETFONT, (WPARAM) s_hFont, 0 );

   s_EditProc = (WNDPROC) SetWindowLong( hWnd, GWL_WNDPROC, (LONG) GUIGetProc );

   EnableWindow( hWnd, TRUE );

   hb_retnl( (LONG) hWnd );
}

HB_FUNC( WVT_PROCESSMESSAGES )
{
   hb_wvt_gtDoProcessMessages();
}

HB_FUNC( SETCURSOR )
{
   USHORT uiCursor;

   hb_gtGetCursor( &uiCursor );

   hb_retni( uiCursor );

   if( s_hGUIGet == 0 && ISNUM( 1 ) )
   {
      hb_gtSetCursor( hb_parni( 1 ) );
   }
}

HB_FUNC( WVT_SETFOCUS )
{
   if( s_hGUIGet )
   {
      SetFocus( s_hGUIGet );
   }
}
#pragma ENDDUMP
