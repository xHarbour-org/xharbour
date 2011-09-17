/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Application.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*
static Application

static nWinError

static __lErrorInit := .F.

static __pCallBackPtr
static __aErrorOptions
static __cErrorProcStack
static __oError
static __aErrorStack
static __hErrorFont
static __hErrorFontBold
static __aParams
static __cGpfError
static __nGpfHeight
static nButWidth

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

//#include "hbclass.ch"
//#include "winapi.ch"
//#include "commctrl.ch"
#include "colors.ch"
#include "debug.ch"
//#include "wingdi.ch"
#include "error.ch"
#include "fileio.ch"
#include "hbexcept.ch"
#include "set.ch"
#include "vxh.ch"
//#include "Structures.ch"
//#define CR CHR(13)
#define SM_CXSHADOW 4
#ifndef SPI_GETDROPSHADOW
   #define SPI_GETDROPSHADOW   0x1024
#endif
#define ICC_LINK_CLASS 0x00008000

#define HKEY_LOCAL_MACHINE           (0x80000002)
#define KEY_ALL_ACCESS              (0xF003F)

#define STAP_ALLOW_NONCLIENT 0x00000001
#define STAP_ALLOW_CONTROLS 0x00000002
#define STAP_ALLOW_WEBCONTENT 0x00000004

//#define EXCEPTION_EXECUTE_HANDLER  1
#define EXCEPTION_CONTINUE_SEARCH  0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)

#define STANDARD_RIGHTS_REQUIRED  0x000F0000L
#define SYNCHRONIZE               0x00100000L
#define MUTANT_QUERY_STATE        0x0001

#define MUTANT_ALL_ACCESS  (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|MUTANT_QUERY_STATE)
#define MUTEX_ALL_ACCESS MUTANT_ALL_ACCESS


INIT PROCEDURE __SetAutoClassInit
   OleInitialize()
   InitCommonControls()
   InitCommonControlsEx()
   InitCommonControlsEx( ICC_BAR_CLASSES )
   InitCommonControlsEx( ICC_USEREX_CLASSES )
   InitCommonControlsEx( ICC_LINK_CLASS )
   InitCommonControlsEx( ICC_INTERNET_CLASSES )
   __SetClassAutoInit( .T. )
RETURN

EXIT PROCEDURE __CleanUp
   SetUnhandledExceptionFilter( NIL )
   EndUxTheme()
   OleUninitialize()
   Application := NIL
RETURN

FUNCTION __GetApplication();RETURN Application

INIT PROCEDURE __GPFCatch
   SetUnhandledExceptionFilter( @GPFCatch() )
   __aParams := hb_aParams()
RETURN

FUNCTION GPFCatch( oExceptionInfo )
   LOCAL err := errorNew()
   LOCAL __cGpfError := " " + Substr( GetModuleFileName(Application:DllInstance), Rat( "\", GetModuleFileName(Application:DllInstance) ) + 1 ) + ;
         " has encountered a technical problem, and needs to close." + CRLF + CRLF

   SWITCH oExceptionInfo:ExceptionRecord:ExceptionCode
      CASE EXCEPTION_ACCESS_VIOLATION
         __cGpfError += " The thread tried to read from or write to a virtual address for which it does not have the appropriate" + CRLF +;
                        " access."
         EXIT

      CASE EXCEPTION_ARRAY_BOUNDS_EXCEEDED
         __cGpfError += " The thread tried to access an array element that is out of bounds and the underlying hardware supports" + CRLF +;
                        " bounds checking."
         EXIT

      CASE EXCEPTION_BREAKPOINT
         __cGpfError += " A breakpoint was encountered."
         EXIT

      CASE EXCEPTION_DATATYPE_MISALIGNMENT
         __cGpfError += " The thread tried to read or write data that is misaligned on hardware that does not provide alignment." + CRLF + ;
                        " For example, 16-bit values must be aligned on 2-byte boundaries; 32-bit values on 4-byte boundaries," + CRLF +;
                        " and so on."
         EXIT

      CASE EXCEPTION_FLT_DENORMAL_OPERAND
         __cGpfError += " One of the operands in a floating-point operation is denormal." + CRLF + ;
                        " A denormal value is one that is too small to represent as a standard floating-point value."
         EXIT

      CASE EXCEPTION_FLT_DIVIDE_BY_ZERO
         __cGpfError += " The thread tried to divide a floating-point value by a floating-point divisor of zero."
         EXIT

      CASE EXCEPTION_FLT_INEXACT_RESULT
         __cGpfError += " The result of a floating-point operation cannot be represented exactly as a decimal fraction."
         EXIT

      CASE EXCEPTION_FLT_INVALID_OPERATION
         __cGpfError += " This exception represents any floating-point exception not included in this list."
         EXIT

      CASE EXCEPTION_FLT_OVERFLOW
         __cGpfError += " The exponent of a floating-point operation is greater than the magnitude allowed by the corresponding" + CRLF +;
                        " type."
         EXIT

      CASE EXCEPTION_FLT_STACK_CHECK
         __cGpfError += " The stack overflowed or underflowed as the result of a floating-point operation."
         EXIT

      CASE EXCEPTION_FLT_UNDERFLOW
         __cGpfError += " The exponent of a floating-point operation is less than the magnitude allowed by the corresponding type."
         EXIT

      CASE EXCEPTION_ILLEGAL_INSTRUCTION
         __cGpfError += " The thread tried to execute an invalid instruction."
         EXIT

      CASE EXCEPTION_IN_PAGE_ERROR
         __cGpfError += " The thread tried to access a page that was not present, and the system was unable to load the page." + CRLF + ;
                        " For example, this exception might occur if a network connection is lost while running a program over" + CRLF +;
                        " the network."
         EXIT

      CASE EXCEPTION_INT_DIVIDE_BY_ZERO
         __cGpfError += " The thread tried to divide an integer value by an integer divisor of zero."
         EXIT

      CASE EXCEPTION_INT_OVERFLOW
         __cGpfError += " The result of an integer operation caused a carry out of the most significant bit of the result."
         EXIT

      CASE EXCEPTION_INVALID_DISPOSITION
         __cGpfError += " An exception handler returned an invalid disposition to the exception dispatcher." + CRLF + ;
                        " Programmers using a high-level language such as C should never encounter this exception."
         EXIT

      CASE EXCEPTION_NONCONTINUABLE_EXCEPTION
         __cGpfError += " The thread tried to continue execution after a noncontinuable exception occurred."
         EXIT

      CASE EXCEPTION_PRIV_INSTRUCTION
         __cGpfError += " The thread tried to execute an instruction whose operation is not allowed in the current machine mode."
         EXIT

      CASE EXCEPTION_SINGLE_STEP
         __cGpfError += " A trace trap or other single-instruction mechanism signaled that one instruction has been executed"
         EXIT

      CASE EXCEPTION_STACK_OVERFLOW
         __cGpfError += " The thread used up its stack"
         EXIT

      CASE EXCEPTION_GUARD_PAGE
         __cGpfError += " Guard page"
         EXIT

      CASE EXCEPTION_INVALID_HANDLE
         __cGpfError += " Invalid handle"
         EXIT

      DEFAULT
         __cGpfError += " Unknown exception error " + XSTR( oExceptionInfo:ExceptionRecord:ExceptionCode )
   END

   err:cargo       := __cGpfError
   err:description := "GENERAL PROTECTION FAULT"
   err:genCode     := EG_UNSUPPORTED

   VXH_DefError( err, .T. )

RETURN EXCEPTION_EXECUTE_HANDLER

//------------------------------------------------------------------------------------------------

CLASS Application
   ACCESS System INLINE __GetSystem() 
   DATA Version       PUBLISHED INIT "1.0.0.0"
   DATA Company       PUBLISHED INIT ""
   DATA Copyright     PUBLISHED INIT ""
   DATA Description   PUBLISHED INIT ""

   DATA Resources     PUBLISHED INIT {}
   
   PROPERTY SetCentury         INDEX -1                   READ xSetCentury         WRITE Set PROTECTED DEFAULT .F.
   PROPERTY SetDeleted         INDEX _SET_DELETED         READ xSetDeleted         WRITE Set PROTECTED DEFAULT SET( _SET_DELETED         )
   PROPERTY SetDefault         INDEX _SET_DEFAULT         READ xSetDefault         WRITE Set PROTECTED DEFAULT SET( _SET_DEFAULT         )
   PROPERTY SetExact           INDEX _SET_EXACT           READ xSetExact           WRITE Set PROTECTED DEFAULT SET( _SET_EXACT           )
   PROPERTY SetFixed           INDEX _SET_FIXED           READ xSetFixed           WRITE Set PROTECTED DEFAULT SET( _SET_FIXED           )
   PROPERTY SetDecimals        INDEX _SET_DECIMALS        READ xSetDecimals        WRITE Set PROTECTED DEFAULT SET( _SET_DECIMALS        )
   PROPERTY SetDateFormat      INDEX _SET_DATEFORMAT      READ xSetDateFormat      WRITE Set PROTECTED DEFAULT SET( _SET_DATEFORMAT      )
   PROPERTY SetEpoch           INDEX _SET_EPOCH           READ xSetEpoch           WRITE Set PROTECTED DEFAULT SET( _SET_EPOCH           )
   PROPERTY SetPath            INDEX _SET_PATH            READ xSetPath            WRITE Set PROTECTED DEFAULT SET( _SET_PATH            )
   PROPERTY SetExclusive       INDEX _SET_EXCLUSIVE       READ xSetExclusive       WRITE Set PROTECTED DEFAULT SET( _SET_EXCLUSIVE       )
   PROPERTY SetSoftseek        INDEX _SET_SOFTSEEK        READ xSetSoftseek        WRITE Set PROTECTED DEFAULT SET( _SET_SOFTSEEK        )
   PROPERTY SetUnique          INDEX _SET_UNIQUE          READ xSetUnique          WRITE Set PROTECTED DEFAULT SET( _SET_UNIQUE          )
   PROPERTY SetCancel          INDEX _SET_CANCEL          READ xSetCancel          WRITE Set PROTECTED DEFAULT SET( _SET_CANCEL          )
   PROPERTY SetDebug           INDEX _SET_DEBUG           READ xSetDebug           WRITE Set PROTECTED DEFAULT SET( _SET_DEBUG           )
   PROPERTY SetTypeahead       INDEX _SET_TYPEAHEAD       READ xSetTypeahead       WRITE Set PROTECTED DEFAULT SET( _SET_TYPEAHEAD       )
   PROPERTY SetAlternate       INDEX _SET_ALTERNATE       READ xSetAlternate       WRITE Set PROTECTED DEFAULT SET( _SET_ALTERNATE       )
   PROPERTY SetAltfile         INDEX _SET_ALTFILE         READ xSetAltfile         WRITE Set PROTECTED DEFAULT SET( _SET_ALTFILE         )
   PROPERTY SetDevice          INDEX _SET_DEVICE          READ xSetDevice          WRITE Set PROTECTED DEFAULT SET( _SET_DEVICE          )
   PROPERTY SetExtra           INDEX _SET_EXTRA           READ xSetExtra           WRITE Set PROTECTED DEFAULT SET( _SET_EXTRA           )
   PROPERTY SetExtrafile       INDEX _SET_EXTRAFILE       READ xSetExtrafile       WRITE Set PROTECTED DEFAULT SET( _SET_EXTRAFILE       )
   PROPERTY SetPrintfile       INDEX _SET_PRINTFILE       READ xSetPrintfile       WRITE Set PROTECTED DEFAULT SET( _SET_PRINTFILE       )
   PROPERTY SetMargin          INDEX _SET_MARGIN          READ xSetMargin          WRITE Set PROTECTED DEFAULT SET( _SET_MARGIN          )
   PROPERTY SetBell            INDEX _SET_BELL            READ xSetBell            WRITE Set PROTECTED DEFAULT SET( _SET_BELL            )
   PROPERTY SetConfirm         INDEX _SET_CONFIRM         READ xSetConfirm         WRITE Set PROTECTED DEFAULT SET( _SET_CONFIRM         )
   PROPERTY SetEscape          INDEX _SET_ESCAPE          READ xSetEscape          WRITE Set PROTECTED DEFAULT SET( _SET_ESCAPE          )
   PROPERTY SetInsert          INDEX _SET_INSERT          READ xSetInsert          WRITE Set PROTECTED DEFAULT SET( _SET_INSERT          )
   PROPERTY SetExit            INDEX _SET_EXIT            READ xSetExit            WRITE Set PROTECTED DEFAULT SET( _SET_EXIT            )
   PROPERTY SetWrap            INDEX _SET_WRAP            READ xSetWrap            WRITE Set PROTECTED DEFAULT SET( _SET_WRAP            )
   PROPERTY SetMessage         INDEX _SET_MESSAGE         READ xSetMessage         WRITE Set PROTECTED DEFAULT SET( _SET_MESSAGE         )
   PROPERTY SetMcenter         INDEX _SET_MCENTER         READ xSetMcenter         WRITE Set PROTECTED DEFAULT SET( _SET_MCENTER         )
   PROPERTY SetScrollbreak     INDEX _SET_SCROLLBREAK     READ xSetScrollbreak     WRITE Set PROTECTED DEFAULT SET( _SET_SCROLLBREAK     )
   PROPERTY SetEventmask       INDEX _SET_EVENTMASK       READ xSetEventmask       WRITE Set PROTECTED DEFAULT SET( _SET_EVENTMASK       )
   PROPERTY SetVideomode       INDEX _SET_VIDEOMODE       READ xSetVideomode       WRITE Set PROTECTED DEFAULT SET( _SET_VIDEOMODE       )
   PROPERTY SetMblocksize      INDEX _SET_MBLOCKSIZE      READ xSetMblocksize      WRITE Set PROTECTED DEFAULT SET( _SET_MBLOCKSIZE      )
   PROPERTY SetMfileext        INDEX _SET_MFILEEXT        READ xSetMfileext        WRITE Set PROTECTED DEFAULT SET( _SET_MFILEEXT        )
   PROPERTY SetStrictread      INDEX _SET_STRICTREAD      READ xSetStrictread      WRITE Set PROTECTED DEFAULT SET( _SET_STRICTREAD      )
   PROPERTY SetOptimize        INDEX _SET_OPTIMIZE        READ xSetOptimize        WRITE Set PROTECTED DEFAULT SET( _SET_OPTIMIZE        )
   PROPERTY SetAutopen         INDEX _SET_AUTOPEN         READ xSetAutopen         WRITE Set PROTECTED DEFAULT SET( _SET_AUTOPEN         )
   PROPERTY SetAutorder        INDEX _SET_AUTORDER        READ xSetAutorder        WRITE Set PROTECTED DEFAULT SET( _SET_AUTORDER        )
   PROPERTY SetAutoshare       INDEX _SET_AUTOSHARE       READ xSetAutoshare       WRITE Set PROTECTED DEFAULT SET( _SET_AUTOSHARE       )
   PROPERTY SetCount           INDEX _SET_COUNT           READ xSetCount           WRITE Set PROTECTED DEFAULT SET( _SET_COUNT           )
   PROPERTY SetLanguage        INDEX _SET_LANGUAGE        READ xSetLanguage        WRITE Set PROTECTED DEFAULT SET( _SET_LANGUAGE        )
   PROPERTY SetIdlerepeat      INDEX _SET_IDLEREPEAT      READ xSetIdlerepeat      WRITE Set PROTECTED DEFAULT SET( _SET_IDLEREPEAT      )
   PROPERTY SetTrace           INDEX _SET_TRACE           READ xSetTrace           WRITE Set PROTECTED DEFAULT SET( _SET_TRACE           )
   PROPERTY SetTracefile       INDEX _SET_TRACEFILE       READ xSetTracefile       WRITE Set PROTECTED DEFAULT SET( _SET_TRACEFILE       )
   PROPERTY SetTracestack      INDEX _SET_TRACESTACK      READ xSetTracestack      WRITE Set PROTECTED DEFAULT SET( _SET_TRACESTACK      )
   PROPERTY SetFilecase        INDEX _SET_FILECASE        READ xSetFilecase        WRITE Set PROTECTED DEFAULT SET( _SET_FILECASE        )
   PROPERTY SetDircase         INDEX _SET_DIRCASE         READ xSetDircase         WRITE Set PROTECTED DEFAULT SET( _SET_DIRCASE         )
   PROPERTY SetDirseparator    INDEX _SET_DIRSEPARATOR    READ xSetDirseparator    WRITE Set PROTECTED DEFAULT SET( _SET_DIRSEPARATOR    )
   PROPERTY SetErrorloop       INDEX _SET_ERRORLOOP       READ xSetErrorloop       WRITE Set PROTECTED DEFAULT SET( _SET_ERRORLOOP       )
   PROPERTY SetOutputsafety    INDEX _SET_OUTPUTSAFETY    READ xSetOutputsafety    WRITE Set PROTECTED DEFAULT SET( _SET_OUTPUTSAFETY    )
   PROPERTY SetDbflockscheme   INDEX _SET_DBFLOCKSCHEME   READ xSetDbflockscheme   WRITE Set PROTECTED DEFAULT SET( _SET_DBFLOCKSCHEME   )
   PROPERTY SetTrimfilename    INDEX _SET_TRIMFILENAME    READ xSetTrimfilename    WRITE Set PROTECTED DEFAULT SET( _SET_TRIMFILENAME    )
   PROPERTY SetPrinterjob      INDEX _SET_PRINTERJOB      READ xSetPrinterjob      WRITE Set PROTECTED DEFAULT SET( _SET_PRINTERJOB      )
   PROPERTY SetHardcommit      INDEX _SET_HARDCOMMIT      READ xSetHardcommit      WRITE Set PROTECTED DEFAULT SET( _SET_HARDCOMMIT      )
   PROPERTY SetForceopt        INDEX _SET_FORCEOPT        READ xSetForceopt        WRITE Set PROTECTED DEFAULT SET( _SET_FORCEOPT        )
   PROPERTY SetEol             INDEX _SET_EOL             READ xSetEol             WRITE Set PROTECTED DEFAULT SET( _SET_EOL             )
   PROPERTY SetErrorlog        INDEX _SET_ERRORLOG        READ xSetErrorlog        WRITE Set PROTECTED DEFAULT SET( _SET_ERRORLOG        )

   DATA Caption                        EXPORTED INIT ""
   DATA DllInstance                    EXPORTED
   DATA hAccel                         EXPORTED
   DATA Path                           EXPORTED
   DATA Name                           EXPORTED
   DATA FileName                       EXPORTED
   DATA OsVersion                      EXPORTED
   DATA Instance                       EXPORTED
   DATA Msg                            EXPORTED
   DATA lExit                          PROTECTED INIT .F.
   DATA Parent                         EXPORTED
   DATA ThemeActive                    EXPORTED  INIT IsThemeActive()
   DATA MDIClient                      EXPORTED
   DATA ImageLists                     EXPORTED  INIT {}
   DATA IniFile                        EXPORTED
   DATA MainForm                       EXPORTED
   DATA bUserError                     EXPORTED
   // compatibility with previous versions
   ACCESS MainWindow                   INLINE ::MainForm
   ASSIGN MainWindow(o)                INLINE ::MainForm := o
   ACCESS IsThemedXP                   INLINE IsThemeActive() .AND. ::OsVersion:dwMajorVersion > 4
   ACCESS TempDir                      INLINE GetTempPath()
   ACCESS AppIniFile                   INLINE ::IniFile
   
   DATA Params                         EXPORTED
   DATA hRich20                        PROTECTED
   DATA IdeActive                      EXPORTED  INIT .F.
   DATA Running                        EXPORTED
   DATA ClsName                        EXPORTED  INIT "Application"
   DATA Form                           EXPORTED
   DATA OnFunctionKey                  EXPORTED
   DATA hWnd                           EXPORTED  INIT 0
   DATA Events                         EXPORTED  INIT {} //{  {"General", { { "OnInit"       , "", "" },;
                                                         //                 { "OnFinish"      , "", "" }} } }
   DATA OfficeXPLook                   EXPORTED  INIT .F.

   DATA TreeItem                       EXPORTED
   DATA oCurMenu                       EXPORTED
   DATA Components                     EXPORTED  INIT {}
   DATA Property                       EXPORTED
   DATA Children                       EXPORTED  INIT {}
   DATA UserVariables                  EXPORTED
   DATA Modal                          EXPORTED  INIT .F.
   DATA InsKey                         EXPORTED  INIT .T.
   DATA Cursor                         PUBLISHED
   DATA GenerateMembers                PUBLISHED INIT .T.
   DATA __Cursors                      EXPORTED  INIT { "None",;
                                                         "Arrow"       ,;
                                                         "Help"        ,;
                                                         "Working"     ,;
                                                         "Busy"        ,;
                                                         "Cross"       ,;
                                                         "TextSelect"  ,;
                                                         "Unavailable" ,;
                                                         "SizeNS"      ,;
                                                         "SizeWE"      ,;
                                                         "SizeNESW"    ,;
                                                         "SizeNWSE"    ,;
                                                         "SizeAll"     ,;
                                                         "UpArrow"    ,;
                                                         "LinkSelect"  }
   DATA __CursorValues                 EXPORTED  INIT { NIL,;
                                                         IDC_ARROW        ,;
                                                         IDC_HELP         ,;
                                                         IDC_APPSTARTING  ,;
                                                         IDC_WAIT         ,;
                                                         IDC_CROSS        ,;
                                                         IDC_IBEAM        ,;
                                                         IDC_NO           ,;
                                                         IDC_SIZENS       ,;
                                                         IDC_SIZEWE       ,;
                                                         IDC_SIZENESW     ,;
                                                         IDC_SIZENWSE     ,;
                                                         IDC_SIZEALL      ,;
                                                         IDC_UPARROW      ,;
                                                         IDC_HAND          }

   DATA __Accelerators                 EXPORTED  INIT {}
   DATA __InstMsg                      PROTECTED
   DATA __hMutex                       EXPORTED
   DATA __ClassInst                    EXPORTED
   DATA __CurCoolMenu                  EXPORTED
   DATA __hCursor                      EXPORTED
   DATA __IsControl                    EXPORTED  INIT .F.
   DATA __xCtrlName                    EXPORTED  INIT "Application"
   DATA __Vxh                          EXPORTED  INIT .F.
   DATA __SocketInit                   EXPORTED  INIT .F.
   DATA __hIcon                        EXPORTED
   DATA __lCopyCut                     EXPORTED  INIT .F.
   DATA __hMenuHook                    EXPORTED
   DATA __CustomOwner                  EXPORTED  INIT .F.
   DATA __PropFilter                   EXPORTED  INIT {}
   DATA __FileOkMsg                    EXPORTED
   //DATA RemoteSocket                   EXPORTED
   
   ACCESS aPath INLINE ::Path
   METHOD Init() CONSTRUCTOR
   METHOD Create() INLINE NIL
   METHOD Run()
   METHOD Close()                      INLINE PostQuitMessage(0)
   METHOD AddAccelerators()
   METHOD DelAccelerators()
   METHOD Exit()
   METHOD Yield()
   METHOD DoEvents()                   INLINE __DoEvents()
   METHOD OnExit()                     VIRTUAL
   METHOD LoadIcon( cIcon )            INLINE LoadIcon( ::Instance, cIcon )
   METHOD RestorePrevInstance(nNotify) INLINE SendMessage( HWND_BROADCAST, ::__InstMsg, nNotify, 0 )
   //----------------------------------------------------------------------------------------------------
   METHOD IsThemeActive()              INLINE IsThemeActive()
   METHOD TranslateAccelerator()
   METHOD Set( nIndex, lSet )          INLINE IIF( ::__ClassInst == NIL, IIF( nIndex == -1, __SetCentury(lSet), Set( nIndex, lSet ) ), )
   METHOD AxTranslate()
   METHOD MessageBox()
   METHOD HasMessage( cMsg )           INLINE __ObjHasMsg( Self, cMsg )

   METHOD GetRectangle()               INLINE {0,0,0,0}
   
   METHOD LoadResource( cName, cType ) INLINE __ResourceToString( Application:Instance, cName, cType )
   METHOD SaveResource()
ENDCLASS

METHOD Init( lIde, __hDllInstance ) CLASS Application
   LOCAL cName, hPrevInstance
   DEFAULT lIde TO .F.
   IF !lIde
      Application := Self
   ENDIF
   ::DllInstance := __hDllInstance
   ::Params      := __aParams

   ::Property    := Hash()
   HSetCaseMatch( ::Property, .F. )

   ::__InstMsg := RegisterWindowMessage( GetModuleFileName(::DllInstance) )

   IF !lIde
      
      //SetThemeAppProperties( STAP_ALLOW_NONCLIENT | STAP_ALLOW_CONTROLS | STAP_ALLOW_WEBCONTENT )

      __GetSystem():Update()

      ::Yield()
      ::Path      := Left( GetModuleFileName(::DllInstance), Rat("\" ,GetModuleFileName(::DllInstance) )-1 )
      ::FileName  := Substr( GetModuleFileName(::DllInstance), Len(::Path) +2 )

      ::Name      := Left(::FileName, Rat( IIF( __hDllInstance == NIL, ".EXE", ".DLL" ), Upper(::FileName ) )-1 )

      cName := STRTRAN( UPPER( ::FileName ), " ", "_" )
      cName := STRTRAN( cName, "." )
      
      hPrevInstance = OpenMutex( MUTEX_ALL_ACCESS, .F., cName )
      ::Running := hPrevInstance <> 0
      IF !::Running
         ::__hMutex := CreateMutex( NIL , .T., cName )
         ReleaseMutex( ::__hMutex )
      ENDIF
      IF hPrevInstance <> 0 
         CloseHandle( hPrevInstance )
      ENDIF
      
      ::hRich20   := LoadLibrary( "riched20.dll" )

      Set( _SET_INSERT, .T. )

      ::IniFile          := IniFile( ::Path + "\" + ::Name + ".ini" )
      ::Msg              := (struct MSG)
      ::OsVersion        := (struct OSVERSIONINFOEX)

      GetVersionEx( @::OsVersion )

      InitUxTheme()
   ENDIF
   ::Instance := GetModuleHandle( ::FileName )
RETURN Self

//------------------------------------------------------------------------------------------------
METHOD SaveResource( ncRes, cFileName ) CLASS Application
   LOCAL hFile, cData, n, hBmp, lRet := .F.

   IF VALTYPE( ncRes ) == "N"
      n := ncRes
    ELSE
      n := ASCAN( ::Resources, {|a| UPPER(a[1]) == UPPER(ncRes) } )
   ENDIF

   IF ::Resources[n][2] == "BMP"
      hBmp := LoadImage( ::Instance, ::Resources[n][1], IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR | LR_DEFAULTSIZE )
      CreateBMPFile( hBmp, cFileName )
      lRet := FILE( cFileName )
    ELSE
      //IF ::Resources[n][2] == "ICO"
      //   cData := __ResIconToString( ::Instance, ::Resources[n][1] )
      // ELSE  
         cData := __ResourceToString( ::Instance, ::Resources[n][1], ::Resources[n][2] )
      //ENDIF
      IF !EMPTY( cData ) .AND. ( hFile := fCreate( cFileName ) ) <> -1
         fWrite( hFile, cData, Len( cData ) )
         fClose( hFile )
         lRet := FILE( cFileName )
      ENDIF
   ENDIF
RETURN lRet

//------------------------------------------------------------------------------------------------

METHOD Exit() CLASS Application
   IF VALTYPE( ::MainForm ) == "O" .AND. IsWindow( ::MainForm:hWnd )
      ::MainForm:Destroy()
   ENDIF
   ::MainForm := NIL
   FreeLibrary( ::hRich20 )
   
   IF ::DllInstance == NIL
      ::lExit := .T.
      PostQuitMessage(0)
      #ifdef VXH_PROFESSIONAL
         FreeExplorerBarInfo()
      #endif
      AEVAL( ::ImageLists, {|o|o:Destroy()} )
      ::OnExit()
      IF ::__hMenuHook != NIL
         UnhookWindowsHookEx( ::__hMenuHook )
      ENDIF
      IF ::__SocketInit
         InetCleanUp()
      ENDIF
      QUIT
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------
METHOD AxTranslate( pMsg, cClass ) CLASS Application
   LOCAL hParent, pUnk, lRet := .F., hWnd := pMsg:hwnd

   DEFAULT cClass TO GetClassName( hWnd )

   IF pMsg:message == WM_KEYDOWN
      IF pMsg:wParam == VK_RETURN .OR. ( (pMsg:wParam == VK_TAB .OR. pMsg:wParam == VK_DELETE) .AND. cClass == "Internet Explorer_Server" )

         WHILE GetClassName( hWnd ) != "AtlAxWin"
            hParent := GetParent( hWnd )
            IF hParent == 0
               EXIT
            ENDIF
            hWnd := hParent
         ENDDO

         IF ( pUnk := __AXGETUNKNOWN( hWnd ) ) != 0
            lRet := __AxTranslateMessage( pUnk, pMsg:Value )
            IF !lRet .AND. cClass != "Edit"
               TranslateMessage( pMsg )
               pMsg:message := WM_NULL
               DispatchMessage( pMsg )
               lRet := .T.
            ENDIF
         ENDIF
      ENDIF

   ENDIF
RETURN lRet

//------------------------------------------------------------------------------------------------
METHOD Run( oWnd ) CLASS Application
   LOCAL msg, cClass

   IF oWnd != NIL
      ::MainForm := oWnd
   ENDIF
   ::MainForm:__InstMsg := ::__InstMsg
   
   IF !::MainForm:Modal
      DO WHILE GetMessage( @Msg, 0, 0, 0 ) .AND. !::lExit
         cClass := GetClassName( Msg:hWnd )
         IF !::AxTranslate( Msg, cClass )
            IF !::TranslateAccelerator( Msg )
               IF ::MDIClient == NIL .OR. TranslateMDISysAccel( ::MDIClient, Msg ) == 0
                  IF ! (cClass == "AfxFrameOrView42s") 
                     IF !IsDialogMessage( GetActiveWindow(), Msg )
                        TranslateMessage( Msg )
                        DispatchMessage( Msg )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

      ENDDO
   ENDIF
   PostQuitMessage(0)

   #ifdef VXH_PROFESSIONAL
      FreeExplorerBarInfo()
   #endif
   
   AEVAL( ::ImageLists, {|o|o:Destroy()} )
   ::OnExit()
   FreeLibrary( ::hRich20 )

   IF ::__hMenuHook != NIL
      UnhookWindowsHookEx( ::__hMenuHook )
   ENDIF
   IF ::__SocketInit
      InetCleanUp()
   ENDIF
   ::MainForm := NIL

RETURN 0

//------------------------------------------------------------------------------------------------

METHOD Yield() CLASS Application
   LOCAL msg
   IF PeekMessage( @Msg, NIL, 0, 0, PM_REMOVE)
      IF !::AxTranslate( Msg )
         IF !::TranslateAccelerator( Msg )
            IF ::MDIClient == NIL .OR. TranslateMDISysAccel( ::MDIClient, Msg ) == 0
               IF !IsDialogMessage( GetActiveWindow(), Msg )
                  TranslateMessage( Msg )
                  DispatchMessage( Msg )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------

METHOD AddAccelerators( hWnd, hAccelTable ) CLASS Application
   LOCAL n
   IF ASCAN( ::__Accelerators, { |a| a[1] == hWnd .AND. a[2] == hAccelTable } ) == 0
      IF ( n := ASCAN( ::__Accelerators, { |a| a[1] == 0 } ) ) == 0
         AADD( ::__Accelerators, { hWnd, hAccelTable } )
       ELSE
         ::__Accelerators[n] := { hWnd, hAccelTable }
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------

METHOD DelAccelerators( hWnd, hAccelTable ) CLASS Application
   LOCAL n, x := 1
   IF hWnd == NIL
      IF hAccelTable == NIL
         ::__Accelerators := {}
         RETURN .T.
       ELSE
         DO WHILE ( n := ASCAN( ::__Accelerators, { |a| a[2] == hAccelTable }, x ) ) > 0
            ::__Accelerators[n] := { 0, 0 }
            x := n + 1
         ENDDO
         RETURN .T.
      ENDIF

    ELSE

      IF hAccelTable == NIL
         DO WHILE ( n := ASCAN( ::__Accelerators, { |a| a[1] == hWnd }, x ) ) > 0
            ::__Accelerators[n] := { 0, 0 }
            x := n + 1
         ENDDO
         RETURN .T.
       ELSE
         DO WHILE ( n := ASCAN( ::__Accelerators, { |a| a[1] == hWnd .AND. a[2] == hAccelTable }, x ) ) > 0
            ::__Accelerators[n] := { 0, 0 }
            x := n + 1
         ENDDO
         RETURN .T.
      ENDIF
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------

METHOD TranslateAccelerator( Msg ) CLASS Application
   LOCAL lRet := .F., n, hWnd
   IF Msg:message == WM_KEYDOWN
      //lAlt   := CheckBit( GetKeyState( VK_MENU ) , 32768 )
      //lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
      //lCtrl  := CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
      
      //IF lAlt .OR. lShift .OR. lCtrl
         FOR n := 1 TO LEN( ::__Accelerators )
             IF ::__Accelerators[n][1] != 0 .AND. ::__Accelerators[n][2] != 0
                hWnd := GetActiveWindow()
                WHILE hWnd != 0 .AND. ! IsChild( ::__Accelerators[n][1], hWnd )
                   hWnd := GetParent( hWnd )
                ENDDO
                IF IsChild( ::__Accelerators[n][1], hWnd ) .OR. ::__Accelerators[n][1] == GetActiveWindow()
                   IF !TranslateAccelerator( ::__Accelerators[n][1], ::__Accelerators[n][2], Msg ) == 0
                      lRet := .T.
                      EXIT
                   ENDIF
                ENDIF
             ENDIF
         NEXT
      //ENDIF
   ENDIF
RETURN lRet

#define BTTNGAP 3

METHOD MessageBox( cMsg, cCaption, aChoices, nIcon, nDefault ) CLASS Application
   LOCAL o := __AlertDlg( cMsg, cCaption, aChoices, nIcon, nDefault )
RETURN o:Result

CLASS __AlertDlg INHERIT Dialog
   DATA aChoices  EXPORTED
   DATA MsgHeight EXPORTED
   DATA Message   EXPORTED
   DATA _Icon
   DATA _Default
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD OnCommand( nId ) INLINE ::Close( nId )
ENDCLASS

METHOD Init( cMsg, cCaption, aChoices, nIcon, nDefault ) CLASS __AlertDlg
   Local n, aMsg, hFont, hOldFont, i, x
   Local hWnd, hDC
   Local nWidth, nMsgHeight
   LOCAL aSize, oParent
   
   nButWidth := 0
   
   DEFAULT nDefault TO 1
   
   ::_Default   := nDefault
   ::_Icon      := nIcon
   
   IF VALTYPE( cMsg ) != "C"
      IF VALTYPE( cMsg)=="A"
         cMsg:=__a2str(cMsg,";")
       ELSE
         cMsg = __asstring( cMsg )
      ENDIF
   ENDIF

   DEFAULT aChoices TO { "&OK" }

   cMsg     := StrTran( cMsg, ";", CHR(13) )
   hFont    := __GetMessageFont( 700 )
   hDC      := GetDC( 0 )
   hOldFont := SelectObject( hDC, hFont )
   aSize    := _GetTextExtentPoint32( hDC, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" )
   nWidth   := 0
   aMsg     := __str2a( cMsg, CHR(13) )

   AEVAL( aMsg, {|x| nWidth := Max( nWidth, _GetTextExtentPoint32( hDC, AllTrim(x) )[1] ) } )

   n := Len( aChoices )
   nMsgHeight := Len( aMsg ) * aSize[2]

   FOR i = 1 To n
       nButWidth := MAX( nButWidth, _GetTextExtentPoint32( hDC, aChoices[i] )[1]+15+BTTNGAP )
   NEXT i
   
   IF nWidth > nButWidth
      x := ( nWidth - nButWidth ) / n
      FOR i = 1 To n
          nButWidth := MAX( nButWidth, _GetTextExtentPoint32( hDC, aChoices[i] )[1]+x+BTTNGAP )
      NEXT i
   ENDIF
   
   SelectObject( hDC, hOldFont )
   DeleteObject( hFont )
   ReleaseDC( 0, hDC )

   nWidth  := Max( nWidth, (nButWidth*n)+5  )
   hWnd    := GetFocus()
   oParent := NIL

   Super:Init( NIL )
   ::Style    := DS_MODALFRAME | WS_VISIBLE | WS_POPUP | DS_SETFONT | WS_CAPTION
   ::ExStyle  := IIF( cCaption == NIL, WS_EX_TOOLWINDOW, 0 )
   ::Caption  := cCaption
   ::Message  := cMsg
   ::MsgHeight:= nMsgHeight
   ::aChoices := aChoices
   ::Width    := nWidth + IIF( ::_Icon != NIL, 60, 20 )
   ::Height   := nMsgHeight + 100
   ::Create() 
 
   SetFocus( hWnd )
RETURN Self

METHOD OnInitDialog() CLASS __AlertDlg
   LOCAL n, i, o, nLeft, nTop, oButton
   n := 15
   IF ::_Icon != NIL
      o := Label( Self )
      o:Style   := SS_ICON | WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
      o:Left    := 15
      o:Top     := 15
      o:Width   := 40
      o:Height  := 40
      o:Create()
      o:SendMessage( STM_SETICON, LoadIcon( NIL, ::_Icon ) )
      n := 60
   ENDIF
   
   o := Label( Self )
   o:Style   := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   o:Caption := ::Message
   o:Left    := n
   o:Top     := 15
   o:Width   := ::ClientWidth - 4
   o:Height  := ::MsgHeight + 5
   o:Create()

   n := LEN( ::aChoices )
   nLeft  := ( ::ClientWidth - ( ( nButWidth + BTTNGAP ) *n ) ) / 2
   nTop   := ::ClientHeight - 34
   
   For i = 1 To n
       o := Button( Self )
       o:Caption := ::aChoices[i]
       o:Left    := nLeft
       o:Top     := nTop
       o:Width   := nButWidth - 1
       o:Id      := i
       o:Height  := 22
       IF i ==  ::_Default
          o:DefaultButton := .T.
          DEFAULT oButton TO o
       ENDIF
       o:Create()
       nLeft += nButWidth + BTTNGAP
   Next i
   ::CenterWindow()
   IF !::Modal
      ::height := oButton:Top + oButton:Height + 25
   ENDIF
   oButton:SetFocus()
RETURN .T.



//--------------------------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------------------------

#xtranslate NTRIM( < n > ) = > ALLTRIM( STR( < n > ) )
#define CR CHR(13)
//------------------------------------------------------------------------------------------------------

PROCEDURE errorsys
   ErrorBlock( {|e| VXH_DefError(e) } )
RETURN

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION VXH_DefError( e, lGpf )
   LOCAL aOptions, nChoice, aStack, c, cErr, cPath, nAt, i
   LOCAL cProcStack := ""
   
   DEFAULT lGpf TO .F.
   
   nWinError := GetLastError()

     // By default, division by zero results in zero
   IF e:genCode == EG_ZERODIV
      RETURN 0
   ENDIF
   IF e:genCode == EG_PRINT
      BREAK
   ENDIF
   IF ( e:genCode == EG_OPEN .AND. e:osCode == 32 .AND. e:canDefault )
      NETERR( .T. )
      RETURN .F.
   ENDIF
   IF ( e:genCode == EG_APPENDLOCK .AND. e:canDefault )
      NETERR( .T. )
      RETURN .F.
   ENDIF

   IF Application != NIL
      Application:Cursor := NIL
   ENDIF

//   TraceLog( e:Operation, e:Description, e:ProcName, e:ProcLine )

   aStack := {}
   i := 2
   DO WHILE ( ! EMPTY( PROCNAME( i ) ) )
      c := PROCFILE( i ) + IIF( !EMPTY( PROCFILE(i) ), " -> ", "" )   + TRIM( PROCNAME( i ) ) + "(" + NTRIM( PROCLINE( i ) ) + ")"
      i ++
      cProcStack += ( CRLF + c )
      AADD( aStack, c )

      IF PROCNAME( i ) == 'VXH_DEFERROR'
         OutputDebugString( "" )
         OutputDebugString( "RECURSIVE ERROR" + CRLF )
         OutputDebugString( e:description + CHR( 13 ) + "Procedure Stack Depth:" + ntrim( i ) + CRLF )
         OutputDebugString( cProcStack + CRLF )
         PostQuitMessage( 0 )
         ERRORLEVEL( 1 )
         RETURN( .F. )
      ENDIF
   ENDDO

   cErr := LogError( e, cProcStack )
   OutputDebugString( cErr )

   cPath := HB_Argv(0)
   nAt   := RAt( "\", cPath )
   cPath := Left( cPath, nAt )

   MemoWrit( cPath + "\error.log", cErr )

   aOptions := { "Copy to clipboard", "Quit" }
   IF ( e:canRetry )
      aAdd( aOptions, "Retry" )
   ENDIF
   IF ( e:canDefault )
      aAdd( aOptions, "Default" )
   ENDIF

   IF Application != NIL .AND. Application:__hMenuHook != NIL
      UnhookWindowsHookEx( Application:__hMenuHook )
   ENDIF

   nChoice := ErrDialog( e, aOptions, aStack, cErr )

   IF ( ! Empty( nChoice ) )
      DO CASE
         CASE aOptions[ nChoice ] == "Break"
              BREAK( e )

         CASE aOptions[ nChoice ] == "Retry"
              RETURN .T.

         CASE aOptions[ nChoice ] == "Default"
              RETURN .F.
      ENDCASE
   ENDIF

   IF lGpf
      // cannot process PostQuitMessage, Application:Exit() or QUIT while in 
      // Exception's callback. Must return to GPFCatch and let the OS
      // clean the application
      RETURN .F.
   ENDIF
   
   IF Application != NIL
      Application:Exit()
   ENDIF

   PostQuitMessage( 0 )

   ERRORLEVEL( 1 )
   QUIT
RETURN .F.

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION LogError( e, cProcStack )

   LOCAL Args := convertargs( e:args, 0 )
   LOCAL cErr

   cErr :=          "SYSTEM INFORMATION"

   cErr += ( CRLF + "--------------------------------------------------------------------" )
   cErr += ( CRLF + "OS             " + GetOSDisplayString() )
   cErr += ( CRLF + "Date/Time      " + dtoc( date() ) + " - " + time() )
   cErr += ( CRLF + "--------------------------------------------------------------------" )
   cErr += ( CRLF )
   IF e:cargo != NIL
      cErr += ( CRLF + LTRIM( STRTRAN( e:cargo, CRLF + " ", CRLF ) ) )
   ENDIF
   cErr += ( CRLF )
   cErr += ( CRLF + "PROGRAM INFORMATION" )
   cErr += ( CRLF + "--------------------------------------------------------------------" )
   IF Application != NIL
      cErr += ( CRLF + "Name           " + Application:Name )
      cErr += ( CRLF + "Path           " + Application:Path )
      cErr += ( CRLF + "Version        " + Application:Version )
   ENDIF
   cErr += ( CRLF + "Date/Time      " + DTOC( FileDate( GetModuleFileName(Application:DllInstance) ) ) + " - " + FileTime( GetModuleFileName(Application:DllInstance) ) )
   cErr += ( CRLF + "Size           " + XSTR( GetFileSize( GetModuleFileName(Application:DllInstance) ) ) )
   cErr += ( CRLF )
   
   IF Application != NIL .AND. Application:bUserError != NIL
      cErr += ( CRLF )
      cErr += ( CRLF + "--------------------------------------------------------------------" )
      cErr += EVAL( Application:bUserError )
      cErr += ( CRLF + "--------------------------------------------------------------------" )
      cErr += ( CRLF )
   ENDIF

   cErr += ( CRLF )
   cErr += ( CRLF + "ERROR INFORMATION" )
   cErr += ( CRLF + "--------------------------------------------------------------------" )
   cErr += ( CRLF + "Arguments      " + Args )
   cErr += ( CRLF + "Description    " + e:description )
   cErr += ( CRLF + "Filename       " + GetVal( e:filename ) )
   cErr += ( CRLF + "GenCode        " + gencodetext( e:genCode ) )
   cErr += ( CRLF + "Operation      " + GetVal( e:operation ) )
   cErr += ( CRLF + "Severity       " + NTRIM( e:severity ) )
   cErr += ( CRLF + "SubCode        " + NTRIM( e:subCode ) )
   cErr += ( CRLF + "SubSystem      " + e:subSystem )
   cErr += ( CRLF + "Tries          " + NTRIM( e:tries ) )
   cErr += ( CRLF + "Current Alias  " + GetVal( ALIAS( ) ) )
   cErr += ( CRLF + "Last DOS Error " + DosErrCode( e ) )
   cErr += ( CRLF + "Last Win Error " + NTRIM( nWinError ) + " - " + FormatMessage( , , nWinError  )  )
   cErr += ( CRLF + "--------------------------------------------------------------------" )
   cErr += ( CRLF )
   cErr += ( CRLF + "FUNCTION STACK" )
   cErr += ( CRLF + "--------------------------------------------------------------------" )

   cErr += cProcStack

RETURN cErr

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION GetVal( cVal )
   LOCAL cRetVal := "<none>"
   IF VALTYPE( cVal ) == "C" .AND. !EMPTY( cVal )
      cRetVal := LEFT( cVal, 68 )
   ENDIF
RETURN cRetVal

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION ConvertArgs( a, Level )
   LOCAL Ret_Val
   LOCAL x, cType
   LOCAL NumArgs := IF( ValType( a ) == "A", Len( a ) , IF( ValType( a ) == "C", ( a := { a } , 1 ) , 0 ) )
   IF Level == NIL
      Level := 0
   ENDIF

   IF NumArgs > 0
      Ret_Val := '{ '
      FOR x := 1 TO NumArgs
         cType := ValType( a[ x ] )
         DO CASE
         CASE cType == "C"
            Ret_Val += '"' + a[ x ] + '"'
         CASE cType == "N"
            Ret_Val += NTRIM( a[ x ] )
         CASE cType == "D"
            Ret_Val += dtoc( a[ x ] )
         CASE cType == "L"
            Ret_Val += IF( a[ x ] , ".T.", ".F." )
         CASE cType == "O"
            Ret_Val += a[ x ] :className + " Object"
         CASE cType == "U"
            Ret_Val += "NIL"
         CASE cType == "A"
            Ret_Val += ConvertArgs( x, Level + 1 )
         ENDCASE

         Ret_Val += "(" + cType + ")"
         IF x < NumArgs
            Ret_Val += ', '
         ENDIF
      NEXT
      Ret_Val += ' }'
   ELSE
      Ret_Val := "{}"
   ENDIF

RETURN Ret_Val

//------------------------------------------------------------------------------------------------------

FUNCTION GetProcStack( )
   LOCAL i := 2
   DO WHILE ! Empty( procname( i ) )
      i ++
   ENDDO
RETURN( i - 3 )

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION DosErrCode( e )
   LOCAL Msg := "0"
   IF e:osCode > 0
      Msg := NTRIM( e:osCode ) + ": " + Left( DosErrText( e:osCode ) , 37 )
   ELSE//IF e:osCode < 0
      Msg := "(NOT OS ERROR)"
   ENDIF
RETURN Msg

//------------------------------------------------------------------------------------------------------

STATIC FUNCTION DosErrText( n )

   LOCAL desc_ := { "Invalid function number"                 , ; // 1
         "File not found"                          , ; // 2
         "Path not found"                          , ; // 3
         "Too many files open (no handles left)"   , ; // 4
         "Access denied"                           , ; // 5
         "Invalid handle"                          , ; // 6
         "Memory control blocks destroyed (oh, my)", ; // 7
         "Insufficient memory"                     , ; // 8
         "Invalid memory block address"            , ; // 9
         "Invalid environment"                     , ; // 10
         "Invalid format"                          , ; // 11
         "Invalid access code"                     , ; // 12
         "Invalid data"                            , ; // 13
         , ; // 14
         "Invalid drive was specified"             , ; // 15
         "Attempt to remove the current directory" , ; // 16
         "Not same device"                         , ; // 17
         "No more files"                           , ; // 18
         "Attempt to write on write-protected diskette", ; // 19
         "Unknown unit"                            , ; // 20
         "Drive not ready"                         , ; // 21
         "Unknown command"                         , ; // 22
         "Data error (CRC)"                        , ; // 23
         "Bad request structure length"            , ; // 24
         "Seek error"                              , ; // 25
         "Unknown media type"                      , ; // 26
         "Sector not found"                        , ; // 27
         "Printer out of paper"                    , ; // 28
         "Write fault"                             , ; // 29
         "Read fault"                              , ; // 30
         "General failure"                         , ; // 31
         "Sharing violation"                       , ; // 32
         "Lock violation"                          , ; // 33
         "Invalid disk change"                     , ; // 34
         "FCB unavailable"                         , ; // 35
         "Sharing buffer overflow"                 , ; // 36
         , , , , , , , , , , , , ,                   ; // 37-49
         "Network request not supported"           , ; // 50
         "Remote computer not listening"           , ; // 51
         "Duplicate name on network"               , ; // 52
         "Network name not found"                  , ; // 53
         "Network busy"                            , ; // 54
         "Network device no longer exists"         , ; // 55
         "Network BIOS command limit exceeded"     , ; // 56
         "Network adapter hardware error"          , ; // 57
         "Incorrect response from network"         , ; // 58
         "Unexpected network error"                , ; // 59
         "Incompatible remote adapter"             , ; // 60
         "Print queue full"                        , ; // 61
         "Not enough space for print file"         , ; // 62
         "Print file deleted (not enough space)"   , ; // 63
         "Network name deleted"                    , ; // 64
         "Access denied"                           , ; // 65
         "Network device type incorrect"           , ; // 66
         "Network name not found"                  , ; // 67
         "Network name limit exceeded"             , ; // 68
         "Network BIOS session limit exceeded"     , ; // 69
         "Temporarily paused"                      , ; // 70
         "Network request not accepted"            , ; // 71
         "Print or disk redirection paused"        , ; // 72
         , , , , , , ,                               ; // 73-79
         "File already exists"                     , ; // 80
         , ; // 81
         "Cannot make directory entry"             , ; // 82
         "Fail on INT 24h"                         , ; // 83
         "Too many redirections"                   , ; // 84
         "Duplicate redirection"                   , ; // 85
         "Invalid password"                        , ; // 86
         "Invalid parameter"                       , ; // 87
         "Network device fault"                    , ; // 88
         ;
         "Undefined or reserved error code!"         ; // +1
       }

   IF ( ( n < 1 ) .OR. n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF

RETURN desc_[ n ]


*------------------------------------------------------------------------------*

STATIC FUNCTION GenCodeText( n )

   LOCAL desc_ := { "EG_ARG", ; // 1
         "EG_BOUND"               , ; // 2
         "EG_STROVERFLOW"         , ; // 3
         "EG_NUMOVERFLOW"         , ; // 4
         "EG_ZERODIV"             , ; // 5
         "EG_NUMERR"              , ; // 6
         "EG_SYNTAX"              , ; // 7
         "EG_COMPLEXITY"          , ; // 8
         , ,                        ; // 9-10
         "EG_MEM"                 , ; // 11
         "EG_NOFUNC"              , ; // 12
         "EG_NOMETHOD"            , ; // 13
         "EG_NOVAR"               , ; // 14
         "EG_NOALIAS"             , ; // 15
         "EG_NOVARMETHOD"         , ; // 16
         "EG_BADALIAS"            , ; // 17 (new w/ 5.01a)
         "EG_DUPALIAS"            , ; // 18 (new w/ 5.01a)
         ,                          ; // 19
         "EG_CREATE"              , ; // 20
         "EG_OPEN"                , ; // 21
         "EG_CLOSE"               , ; // 22
         "EG_READ"                , ; // 23
         "EG_WRITE"               , ; // 24
         "EG_PRINT"               , ; // 25
         , , , ,                    ; // 26-29
         "EG_UNSUPPORTED"         , ; // 30
         "EG_LIMIT"               , ; // 31
         "EG_CORRUPTION"          , ; // 32
         "EG_DATATYPE"            , ; // 33
         "EG_DATAWIDTH"           , ; // 34
         "EG_NOTABLE"             , ; // 35
         "EG_NOORDER"             , ; // 36
         "EG_SHARED"              , ; // 37
         "EG_UNLOCKED"            , ; // 38
         "EG_READONLY"            , ; // 39
         "EG_APPENDLOCK"          , ; // 40
         ;
         "Unknown or reserved"      ; // +1
       }

   IF ( ( n < 1 ) .OR. n > ( Len( desc_ ) - 1 ) ) .OR. ( desc_[ n ] == NIL )
      n := Len( desc_ )
   ENDIF

RETURN NTRIM( n ) + ": " + desc_[ n ]

//------------------------------------------------------------------------------------------------------

FUNCTION ErrDialog( e, aChoices, aStack, cProcStack )
   LOCAL n, ncm, dt, hDC, hFont, a := {0,0}

   IF !__lErrorInit
      HB_CStructureCSyntax("ERRDLGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
      __ClsSetModule(__ActiveStructure() )
      __lErrorInit := .T.
   END

   dt := (struct ERRDLGTEMPLATE)
   __pCallBackPtr := WinCallBackPointer( @__ErrorDlgProc() )

   __nGpfHeight := 0
   IF e:cargo != NIL
      hDC   := CreateCompatibleDC()
      hFont := SelectObject( hDC, __hErrorFont )
      a     := _GetTextExtentPoint32( hDC, e:cargo )
      __nGpfHeight := a[2] * LEN( __str2a( e:cargo, CRLF ) )
      SelectObject( hDC, hFont )
      DeleteDC( hDC )
   ENDIF
   __cGpfError := e:cargo

   dt:style           := WS_POPUP | DS_SETFONT | WS_CAPTION | DS_SYSMODAL
   dt:dwExtendedStyle := 0
   dt:x               := 0
   dt:y               := 0
   dt:cx              := Int( ( 600 * 4 )/LOWORD(GetDialogBaseUnits()) )
   dt:cy              := Int( ( (220+__nGpfHeight) * 4 )/LOWORD(GetDialogBaseUnits()) )

   __aErrorOptions    := aChoices
   __cErrorProcStack  := cProcStack
   __oError           := e
   __aErrorStack      := aStack

   
   //ncm := (struct NONCLIENTMETRICS)
   //SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 )

   SysNonClientMetrics( @ncm )
   __hErrorFont       := CreateFontIndirect( ncm:lfMessageFont )
   ncm:lfMessageFont:lfWeight := 700
   __hErrorFontBold   := CreateFontIndirect( ncm:lfMessageFont )
   n := DialogBoxIndirect( GetModuleHandle( Application:FileName ), dt, GetActiveWindow(), __pCallBackPtr )
   DeleteObject( __hErrorFont )
   DeleteObject( __hErrorFontBold )
   FreeCallBackPointer( __pCallBackPtr )
RETURN n

FUNCTION __ErrorDlgProc( hWnd, nMsg, nwParam, nlParam )
   LOCAL cText, hKey, rc, nWidth, nHeight, cCaption, hStk, hLst, aRect, hDC, nColor
   LOCAL nLeft, cOpt, aLabels, aCaptions, nTop, n, hCtrl, dis, lSelected, aPar, __nWidth
   static hGpf, hBrush
   
   SWITCH nMsg
      CASE WM_INITDIALOG
           SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE )

           cText := Substr( GetModuleFileName(Application:DllInstance), Rat( "\", GetModuleFileName(Application:DllInstance) ) + 1 )
           cText := Left( cText, Rat("." ,cText )-1 )
           
           SetWindowText( hWnd, cText )

           rc := (struct RECT)
           GetClientRect( hWnd, @rc )

           IF __cGpfError != NIL
              hGpf := CreateWindowEx( WS_EX_TRANSPARENT, "static", __cGpfError, WS_CHILD | WS_VISIBLE | SS_SUNKEN, 5, 5, rc:right - 10, __nGpfHeight - 2, hWnd, 10, Application:Instance )
              SendMessage( hGpf, WM_SETFONT, __hErrorFontBold, MAKELPARAM( 1, 0 ) )
           ENDIF
           
           hCtrl := CreateWindowEx( WS_EX_TRANSPARENT, "static", "", WS_CHILD | WS_VISIBLE | SS_NOTIFY | SS_SIMPLE | SS_SUNKEN, 5, 5+__nGpfHeight, rc:right - 10, rc:bottom - 55 - __nGpfHeight, hWnd, 101, Application:Instance )
           SendMessage( hCtrl, WM_SETFONT, __hErrorFont, MAKELPARAM( 1, 0 ) )

           cCaption := "Arguments"      + CRLF +;
                       "Description"    + CRLF +;
                       "Filename"       + CRLF +;
                       "GenCode"        + CRLF +;
                       "Operation"      + CRLF +;
                       "Severity"       + CRLF +;
                       "SubCode"        + CRLF +;
                       "SubSystem"      + CRLF +;
                       "Tries"          + CRLF +;
                       "Current Alias"  + CRLF +;
                       "DOS Error"      + CRLF +;
                       "Windows Error"
           hCtrl := CreateWindowEx( 0, "static", cCaption, WS_CHILD | WS_VISIBLE | SS_NOTIFY, 10, 8+__nGpfHeight, 100, rc:bottom - 60, hWnd, 102, Application:Instance )
           SendMessage( hCtrl, WM_SETFONT, __hErrorFontBold, MAKELPARAM( 1, 0 ) )


           aLabels := { "Arguments"      ,;
                        "Description"    ,;
                        "Filename"       ,;
                        "GenCode"        ,;
                        "Operation"      ,;
                        "Severity"       ,;
                        "SubCode"        ,;
                        "SubSystem"      ,;
                        "Tries"          ,;
                        "Current Alias"  ,;
                        "DOS Error"      ,;
                        "Windows Error" }

           aCaptions := { convertargs( __oError:args, 0 )  ,;
                          __oError:description             ,;
                          GetVal( __oError:filename )      ,;
                          gencodetext( __oError:genCode )  ,;
                          GetVal( __oError:operation )     ,;
                          NTRIM( __oError:severity )       ,;
                          NTRIM( __oError:subCode )        ,;
                          __oError:subSystem               ,;
                          NTRIM( __oError:tries )          ,;
                          GetVal( ALIAS() )               ,;
                          DosErrCode( __oError )           ,;
                          NTRIM( nWinError ) + " - " + FormatMessage( , , nWinError ) }


           nTop := 8+__nGpfHeight
           nWidth := rc:right - 112
           FOR n := 1 TO LEN( aLabels )
               hCtrl := CreateWindowEx( 0, "static", aCaptions[n], WS_CHILD | WS_VISIBLE | SS_NOTIFY, 112, nTop, nWidth, 13, hWnd, 102+n, Application:Instance )
               SendMessage( hCtrl, WM_SETFONT, __hErrorFont, MAKELPARAM( 1, 0 ) )
               nTop += 13
           NEXT

           nLeft  := 2
           nWidth := rc:right / LEN( __aErrorOptions )

           n := 1
           FOR EACH cOpt IN __aErrorOptions
               hCtrl := CreateWindowEx( 0, "button", cOpt, WS_CHILD | WS_VISIBLE | WS_TABSTOP, nLeft, rc:bottom - 45, nWidth - 4, 25, hWnd, n, Application:Instance )
               SendMessage( hCtrl, WM_SETFONT, __hErrorFont, MAKELPARAM( 1, 0 ) )
               nLeft += nWidth
               n ++
           NEXT

           hStk := CreateWindowEx( 0, "button", "Stack >>", WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW, 2, rc:bottom - 17,  rc:right - 4, 15, hWnd, 201, Application:Instance )
           SendMessage( hStk, WM_SETFONT, __hErrorFont, MAKELPARAM( 1, 0 ) )
           SetWindowTheme( hStk, NIL, NIL )

           nTop := rc:bottom - 17
           hLst := CreateWindowEx( WS_EX_CLIENTEDGE, "listbox", "", WS_CHILD | WS_TABSTOP | LBS_NOTIFY | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT | WS_VSCROLL | WS_HSCROLL, 5, nTop, rc:right - 10, 200, hWnd, 202, Application:Instance )
           SendMessage( hLst, WM_SETFONT, __hErrorFont, MAKELPARAM( 1, 0 ) )

           FOR EACH cText IN __aErrorStack
               SendMessage( hLst, LB_ADDSTRING, 0, cText )
           NEXT
           
           cOpt := NIL
           IF RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\Visual xHarbour\Settings", 0, KEY_ALL_ACCESS, @hKey ) == 0
              RegQueryValueEx( hKey, "ErrorShowStack",,,@cOpt )
              RegCloseKey( hKey )
           ENDIF

           IF cOpt != NIL .AND. cOpt == "1"
              aRect := _GetWindowRect( hWnd )
              nWidth  := aRect[3]-aRect[1]
              nHeight := aRect[4]-aRect[2] + 200
              MoveWindow( hWnd, aRect[1], aRect[2], nWidth, nHeight )
              SetDlgItemText( hWnd, 201, "<< Stack" )
              ShowWindow( GetDlgItem( hWnd, 202 ), SW_SHOW )

              hStk    := GetDlgItem( hWnd, 201 )
              aRect   := _GetWindowRect( hStk )
              nWidth  := aRect[3]-aRect[1]
              aRect   := _GetClientRect( hWnd )
              MoveWindow( hStk, 2, aRect[4]-17, nWidth, 17 )
           ENDIF

           aPar := _GetWindowRect( GetDeskTopWindow() )
           aRect := _GetWindowRect( hWnd )

           nLeft := ( ( aPar[3]-aPar[1] ) / 2 ) - ( (aRect[3]-aRect[1]) / 2 )
           nTop  := ( ( aPar[4]-aPar[2] ) / 2 ) - ( (aRect[4]-aRect[2]) / 2 )
           MoveWindow( hWnd, nLeft, nTop, aRect[3]-aRect[1], aRect[4]-aRect[2] )

           __nWidth := 0
           hDC := GetDC( hLst )
           FOR EACH cText IN __aErrorStack
               n := (_GetTextExtentPoint32( hDC, ALLTRIM( cText ) )[1]+3)
               IF n > __nWidth
                  SendMessage( hLst, LB_SETHORIZONTALEXTENT, n, 0 )
                  __nWidth := n
               ENDIF
           NEXT
           ReleaseDC( hLst, hDC )
           EXIT

      CASE WM_CTLCOLORSTATIC
           IF nlParam == hGpf
              nColor := RGB( 255, 0, 0 )
              IF hBrush != NIL
                 DeleteObject( hBrush )
              ENDIF
              hBrush := CreateSolidBrush( nColor )
              SetBkColor( nwParam, nColor )
              SetTextColor( nwParam, RGB( 255, 255, 255 ) )
              RETURN hBrush
           ENDIF
           EXIT
           
      CASE WM_DRAWITEM
           dis  := (struct DRAWITEMSTRUCT)
           dis:Pointer( nlParam )

           hStk := GetDlgItem( hWnd, 201 )

           IF dis:hwndItem == hStk
              GetDlgItemText( hWnd, 201, @cText )
              lSelected := dis:itemState & ODS_SELECTED != 0
              aRect     := { dis:rcItem:Left, dis:rcItem:Top, dis:rcItem:Right, dis:rcItem:Bottom }

              _FillRect( dis:hDC, aRect, GetSysColorBrush( COLOR_BTNFACE ) )
              _DrawEdge( dis:hDC, aRect, IF( lSelected, EDGE_SUNKEN, EDGE_RAISED ), BF_RECT + BF_SOFT)

              __DrawDnArrow( dis:hDC, aRect, cText == "<< Stack" )

           ENDIF
           EXIT

      CASE WM_COMMAND
           GetDlgItemText( hWnd, nwParam, @cText )
           IF cText == "Stack >>"
              aRect := _GetWindowRect( hWnd )
              nWidth  := aRect[3]-aRect[1]
              nHeight := aRect[4]-aRect[2] + 200
              MoveWindow( hWnd, aRect[1], aRect[2], nWidth, nHeight )
              SetDlgItemText( hWnd, nwParam, "<< Stack" )
              ShowWindow( GetDlgItem( hWnd, 202 ), SW_SHOW ) //::ListBox1:Show()

              hStk    := GetDlgItem( hWnd, 201 )
              aRect   := _GetWindowRect( hStk )
              nWidth  := aRect[3]-aRect[1]
              aRect   := _GetClientRect( hWnd )
              MoveWindow( hStk, 2, aRect[4]-17, nWidth, 17 )

              IF RegCreateKey( HKEY_LOCAL_MACHINE, "Software\Visual xHarbour\Settings", @hKey ) == 0
                 RegSetValueEx( hKey, "ErrorShowStack",, 1, "1" )
                 RegCloseKey( hKey )
              ENDIF
              RETURN 0
           ENDIF

           IF cText == "<< Stack"
              aRect := _GetWindowRect( hWnd )
              nWidth  := aRect[3]-aRect[1]
              nHeight := aRect[4]-aRect[2] - 200
              MoveWindow( hWnd, aRect[1], aRect[2], nWidth, nHeight )
              SetDlgItemText( hWnd, nwParam, "Stack >>" )
              ShowWindow( GetDlgItem( hWnd, 202 ), SW_HIDE ) //::ListBox1:Hide()

              hStk    := GetDlgItem( hWnd, 201 )
              aRect   := _GetWindowRect( hStk )
              nWidth  := aRect[3]-aRect[1]
              aRect   := _GetClientRect( hWnd )
              MoveWindow( hStk, 2, aRect[4]-17, nWidth, 17 )

              IF RegCreateKey( HKEY_LOCAL_MACHINE, "Software\Visual xHarbour\Settings", @hKey ) == 0
                 RegSetValueEx( hKey, "ErrorShowStack",, 1, "0" )
                 RegCloseKey( hKey )
              ENDIF
              RETURN 0
           ENDIF

           IF ( n := ASCAN( __aErrorOptions, {|c| c == cText } ) ) > 1
              EndDialog( hWnd, n )
              RETURN 0
             ELSEIF n == 1
              OpenClipboard( hWnd )
              EmptyClipboard()
              SetClipboardData( CF_TEXT, GlobalString( __cErrorProcStack ) )
              CloseClipboard()
           ENDIF
           EXIT

   END
RETURN 0

FUNCTION __DrawDnArrow( hDC, aRect, lUp )
   LOCAL nLeft,nTop,nRight,nBottom,hOld,nM,nL,nR,nT
   nLeft  :=aRect[1]
   nTop   :=aRect[2]
   nRight :=aRect[3]-1
   nBottom:=aRect[4]-1
   SelectObject(hDC,GetStockObject(BLACK_PEN))
   hOld:=SelectObject(hDC,GetStockObject(BLACK_BRUSH))
   nM:=((nRight+nLeft)/2)
   nL:=(nM-2)
   nR:=(nM+2)
   nT:=nTop+((nBottom-nTop)/2)
   IF lUp
      _Polygon(hDC,{ {nL-2,nT+2},{nR+2,nT+2},{nM,nT-2} })
    ELSE
      _Polygon(hDC,{ {nL-2,nT-2},{nR+2,nT-2},{nM,nT+2} })
   ENDIF
   SelectObject(hDC,hOld)
RETURN NIL

#ifndef VXH_PROFESSIONAL
   CLASS MemoryTable
   ENDCLASS
#endif

//----------------------------------------------------------------------------//
#pragma BEGINDUMP
HB_FUNC( FORCEGPF )
{
   char gpf[1];

   strcpy( gpf, "Intentionally attempting to force a gpf" );
}
#pragma ENDDUMP
//----------------------------------------------------------------------------//

