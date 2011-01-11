STATIC s_oControl := NIL

#include "winapi.ch"
#include "wingdi.ch"
#include "structures.ch"

#include "commctrl.ch"
#include "commdlg.ch"

#include "ole.ch"

//#define VERB OLEIVERB_INPLACEACTIVATE
#define VERB OLEIVERB_UIACTIVATE


#define IDM_OLE_IE             5001
#define IDM_OLE_PDF            5002
#define IDM_OLE_WORD           5003
#define IDM_OLE_EXCEL          5004
#define IDM_OLE_CRYSTALREPORTS 5005

PROCEDURE Main()

   LOCAL hWnd
   LOCAL wc := (struct WNDCLASS)
   LOCAL msg
   LOCAL hMenu, hFileMenu

   wc:style         := CS_OWNDC | CS_DBLCLKS
   wc:lpfnWndProc   := WinCallBackPointer( @MainWndProc() )
   //wc:cbClsExtra    := 0
   //wc:cbWndExtra    := 0
   wc:hInstance     := GetModuleHandle()
   //wc:hIcon         := 0
   wc:hCursor       := LoadCursor( NIL, IDC_IBEAM )
   wc:hbrBackground := COLOR_WINDOW + 1
   wc:lpszClassName := "PRG Test Window"
   //wc:lpszMenuName  := NIL

   IF RegisterClass( wc ) > 0

      hWnd := CreateWindowEx( 0, "PRG Test Window", "ActiveX Test", WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, 10, 10, 780, 580, 0, 0, wc:hInstance, 0 )

      hFileMenu := CreatePopupMenu()
      AppendMenu( hFileMenu, MF_STRING, IDM_OLE_IE,             "&Internet Explorer" )
      AppendMenu( hFileMenu, MF_STRING, IDM_OLE_PDF,            "&Adobe Acrobat" )
      AppendMenu( hFileMenu, MF_STRING, IDM_OLE_WORD,           "Microsoft &Word" )
      AppendMenu( hFileMenu, MF_STRING, IDM_OLE_EXCEL,          "Microsoft &Excel" )
      AppendMenu( hFileMenu, MF_STRING, IDM_OLE_CRYSTALREPORTS, "&Crystal Reports" )

      hMenu := CreateMenu()
      AppendMenu( hMenu, MF_POPUP, hFileMenu, "&File" )

      SetMenu( hWnd, hMenu )

      IF hWnd > 0
         ShowWindow( hWnd, SW_SHOW )
         UpdateWindow( hWnd );

         WHILE GetMessage( @msg, 0, 0, 0 )
            TranslateMessage( msg )
            DispatchMessage( msg )
         END

         //s_oControl:Close( OLECLOSE_NOSAVE )
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
   LOCAL ofn, cFile
   LOCAL CRXApp, CRXReport

   //TraceLog( hWnd, message, wParam, lParam )

   SWITCH message

      CASE WM_SIZE
         IF s_oControl != NIL
            s_oControl:__OnSize()
         ENDIF

         RETURN 0

      CASE WM_COMMAND
         s_oControl := NIL

         SWITCH LOWORD( wParam )
            CASE IDM_OLE_IE
               s_oControl := CreateActiveX( hWnd, "Shell.Explorer", , VERB )
               s_oControl:Navigate( "http://www.xHarbour.com" )
               EXIT

            CASE IDM_OLE_PDF
               TRY
                  s_oControl := CreateActiveX( hWnd, "PDF.PdfCtrl.6", , VERB )
               CATCH
                  Alert( "Sorry, Adobe Acrobat not available." )
                  EXIT
               END

               ofn IS OPENFILENAME

               ofn:lStructSize     := 76
               ofn:hwndOwner       := hWnd
               ofn:hInstance       := GetModuleHandle()
               ofn:nMaxFile        := MAX_PATH + 1
               //ofn:lpstrInitialDir :=
               ofn:lpstrFile       := Space( MAX_PATH )
               ofn:lpstrFilter     := "Acrobat files" + Chr(0) + "*.pdf" + Chr(0)
               ofn:lpstrTitle      := "ActiveX Tester - Open File"
               ofn:Flags           := OFN_FILEMUSTEXIST

               IF ! GetOpenFileName( @ofn )
                  EXIT
               ENDIF

               cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

               IF Empty( cFile )
                  EXIT
               ENDIF

               TRY
                  s_oControl:LoadFile( cFile )
               CATCH
                  Alert( "Sorry, Adobe Acrobat failed to open: " + cFile )
               END
               EXIT

            CASE IDM_OLE_WORD
               TRY
                  s_oControl := CreateActiveX( hWnd, "Word.document", , VERB )
               CATCH
                  Alert( "Sorry, Microsoft Word not available." )
               END
               EXIT

            CASE IDM_OLE_EXCEL
               TRY
                  s_oControl := CreateActiveX( hWnd, "OWC.SpreadSheet", , VERB )
               CATCH
                  Alert( "Sorry, Microsoft Excel not available." )
               END
               EXIT

            CASE IDM_OLE_CRYSTALREPORTS
               TRY
                  CRXApp := CreateObject( "CrystalRuntime.Application" )
               CATCH
                  Alert( "Sorry, Crystal Report not available." )
                  EXIT
               END

               ofn IS OPENFILENAME

               ofn:lStructSize     := 76
               ofn:hwndOwner       := hWnd
               ofn:hInstance       := GetModuleHandle()
               ofn:nMaxFile        := MAX_PATH + 1
               //ofn:lpstrInitialDir :=
               ofn:lpstrFile       := Space( MAX_PATH )
               ofn:lpstrFilter     := "Report files" + Chr(0) + "*.rpt" + Chr(0)
               ofn:lpstrTitle      := "ActiveX Tester - Open File"
               ofn:Flags           := OFN_FILEMUSTEXIST

               IF ! GetOpenFileName( @ofn )
                  EXIT
               ENDIF

               cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

               IF Empty( cFile )
                  EXIT
               ENDIF

               TRY
                  CRXReport := CRXApp:OpenReport( cFile )
               CATCH
                  Alert( "Sorry, Crystal Reports failed to open: " + cFile )
                  EXIT
               END

               TRY
                  s_oControl := CreateActiveX( hWnd, "CrystalReports.ActiveXReportViewer", , VERB )
                  s_oControl:EnableExportButton := .T.
                  s_oControl:EnableProgressControl := .T.
                  s_oControl:ReportSource := CRXReport
                  s_oControl:EnableAnimationCtrl := .F.

                  s_oControl:ViewReport()
               CATCH
                  Alert( "Sorry, Report Viewer not available." )
               END
               EXIT

         END
         RETURN 0

      CASE WM_SETFOCUS
         RETURN 0

      case WM_CREATE
         RETURN 0

      case WM_DESTROY
         PostQuitMessage( 0 )
         TraceLog( "Quit" )
         RETURN 0
   END

RETURN DefWindowProc( hWnd, message, wParam, lParam )
