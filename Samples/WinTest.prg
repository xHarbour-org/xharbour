#include "winapi.ch"
#include "wingdi.ch"
#include "structures.ch"

PROCEDURE Main()

   LOCAL hWnd
   LOCAL wc := (struct WNDCLASS)
   LOCAL msg

   wc:style         := CS_OWNDC | CS_DBLCLKS
   wc:lpfnWndProc   := WinCallBackPointer( @MainWndProc() )
   //wc:cbClsExtra    := 0
   //wc:cbWndExtra    := 0
   wc:hInstance     := GetModuleHandle()
   //wc:hIcon         := 0
   wc:hCursor       := LoadCursor( NIL, IDC_IBEAM )
   wc:hbrBackground := COLOR_WINDOW + 1
   wc:lpszClassName := "TestWindow"
   //wc:lpszMenuName  := NIL

   IF RegisterClass( wc ) > 0
      hWnd := CreateWindowEx( 0, "TestWindow", "Test", WS_OVERLAPPEDWINDOW, 100, 100, 400, 200, 0, 0, wc:hInstance, NIL )

      IF hWnd > 0
         ShowWindow( hWnd, SW_SHOW )

         WHILE GetMessage( @msg, 0, 0, 0 )
            TranslateMessage( msg )
            DispatchMessage( msg )
         END
      ELSE
         TraceLog( GetLastError(), FormatMessage() )
         Alert( "Failed to create window!" )
      ENDIF
   ELSE
      TraceLog( GetLastError(), FormatMessage() )
      Alert( "Failed to register class!" )
   ENDIF

   RETURN

FUNCTION MainWndProc( hWnd, message, wParam, lParam )

   LOCAL ps, hDC

   //TraceLog( hWnd, message, wParam, lParam )

   SWITCH message
      CASE WM_SETFOCUS
         RETURN 0

      case WM_PAINT
         hDC := BeginPaint( hWnd, @ps )
         TextOut( hDC, 20, 20, "Hello World", Len( "Hello World" ) )
         EndPaint( hWnd, ps )
         RETURN 0

      case WM_CREATE
         RETURN 0

      case WM_DESTROY
         PostQuitMessage( 0 )
         TraceLog( "Quit" )
         RETURN 0
   END

RETURN DefWindowProc( hWnd, message, wParam, lParam )
