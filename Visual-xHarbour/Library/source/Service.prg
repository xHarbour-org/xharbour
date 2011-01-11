/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Service.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "debug.ch"
#include "colors.ch"
#include "Service.ch"

//-----------------------------------------------------------------------------------------------

CLASS Service
   PROPERTY ServiceName READ xServiceName WRITE __SetServiceName DEFAULT "" INVERT 
   DATA File                PUBLISHED INIT ""
   DATA DisplayName         PUBLISHED INIT ""
   DATA Description         PUBLISHED INIT ""
   
   DATA hServiceManager     EXPORTED
   DATA hService            EXPORTED
   DATA ServiceStatusStruct EXPORTED
   DATA ServiceStatusHandle EXPORTED
   DATA StopServiceEvent    EXPORTED
   DATA EventBlock          EXPORTED
   DATA TimeOut             EXPORTED INIT 5000
   DATA __pMainCallBackPtr  EXPORTED
   DATA __pProcCallBackPtr  EXPORTED

   DATA aStatus             PROTECTED INIT { "Stopped",;
                                             "Start pending",;
                                             "Stop pending",;
                                             "Running",;
                                             "Continue pending",;
                                             "Pause pending",;
                                             "Paused" }
   DESTRUCTOR __ExitService
   
   ACCESS Status INLINE ::aStatus[ ::GetStatus ]
   
   METHOD Install()
   METHOD Run()
   METHOD Start()       INLINE StartService( ::hService, 0 )
   METHOD Stop( nTime ) INLINE StopService( ::hServiceManager, ::hService, .T., IIF( nTime != NIL, nTime, 60000 ) ), Self
   METHOD Delete()      INLINE DeleteService( ::hService ),;
                               CloseServiceHandle( ::hService ),;
                               ::hService := NIL,;
                               Self
   METHOD __ServiceMain()
   METHOD __ServiceProc()
   METHOD __SetServiceName()
   METHOD GetStatus()
   METHOD QueryData()
ENDCLASS

METHOD Install() CLASS Service
   LOCAL aProcs := {}
   IF EMPTY( ::hServiceManager )
      ::hServiceManager := OpenSCManager( NIL, NIL, SC_MANAGER_ALL_ACCESS )
   ENDIF
   IF EMPTY( ::hService )
      ::hService := OpenService( ::hServiceManager, ::ServiceName,  SERVICE_ALL_ACCESS )
   ENDIF
   IF ::hService == 0
      ::hService := CreateService( ::hServiceManager,;
                                   ::ServiceName,;
                                   IIF( ::DisplayName != NIL, ::DisplayName, ::ServiceName ),;
                                   SC_MANAGER_ALL_ACCESS,;
                                   SERVICE_WIN32_OWN_PROCESS/* | SERVICE_INTERACTIVE_PROCESS*/,;
                                   SERVICE_AUTO_START,;
                                   SERVICE_ERROR_NORMAL,;
                                   ::File )
      ChangeServiceDescription( ::hService, ::Description )
      StartService( ::hService, 0 )
   ENDIF
RETURN Self

METHOD QueryData() CLASS Service
   LOCAL n, aServices, sd, lRet, qsc := (struct QUERY_SERVICE_CONFIG)
   IF ( lRet := QueryServiceConfig( ::hService, @qsc ) )
      ::File := qsc:lpBinaryPathName
      ::DisplayName  := qsc:lpDisplayName
   ENDIF
   aServices := __EnumServices()
   IF ( n := ASCAN( aServices, {|a|a[2]==::DisplayName} ) ) > 0
      ::xServiceName := aServices[n][1]
   ENDIF
   sd := (struct SERVICE_DESCRIPTION)
   IF ( lRet := QueryServiceConfig2( ::hService, 1, @sd ) )
      ::Description := sd:lpDescription
   ENDIF
RETURN lRet

METHOD __SetServiceName( cName ) CLASS Service
   IF ::xServiceName != cName
      IF EMPTY( ::hServiceManager )
         ::hServiceManager := OpenSCManager( NIL, NIL, SC_MANAGER_ALL_ACCESS )
      ENDIF
      IF !EMPTY( ::hService )
         CloseServiceHandle( ::hService )
      ENDIF
      ::hService := OpenService( ::hServiceManager, cName,  SERVICE_ALL_ACCESS )
   ENDIF
RETURN cName

METHOD GetStatus() CLASS Service
   LOCAL ss
   IF !EMPTY( ::hService )
      ss := (struct SERVICE_STATUS)
      QueryServiceStatus( ::hService, @ss )
      RETURN ss:dwCurrentState
   ENDIF
RETURN 0

METHOD Run() CLASS Service
   LOCAL nWinError
   ::ServiceStatusStruct := (struct SERVICE_STATUS)

   ::__pMainCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "__ServiceMain" ), Self )

   RunService( ::ServiceName, ::__pMainCallBackPtr )
RETURN Self

PROCEDURE __ExitService CLASS Service
   IF ::hService != NIL
      CloseServiceHandle( ::hService )
   ENDIF

   IF ::hServiceManager != NIL
      CloseServiceHandle( ::hServiceManager )
   ENDIF
   IF ::__pMainCallBackPtr != NIL
      FreeCallBackPointer( ::__pMainCallBackPtr )
   ENDIF
   IF ::__pProcCallBackPtr != NIL
      FreeCallBackPointer( ::__pProcCallBackPtr )
   ENDIF
RETURN

METHOD __ServiceMain() CLASS Service
   ::ServiceStatusStruct:dwServiceType             := SERVICE_WIN32
   ::ServiceStatusStruct:dwCurrentState            := SERVICE_STOPPED
   ::ServiceStatusStruct:dwControlsAccepted        := 0
   ::ServiceStatusStruct:dwWin32ExitCode           := NO_ERROR
   ::ServiceStatusStruct:dwServiceSpecificExitCode := NO_ERROR
   ::ServiceStatusStruct:dwCheckPoint              := 0
   ::ServiceStatusStruct:dwWaitHint                := 0

   ::__pProcCallBackPtr := WinCallbackPointer( HB_ObjMsgPtr( Self, "__ServiceProc" ), Self )
   ::ServiceStatusHandle := RegisterServiceCtrlHandler( ::ServiceName, ::__pProcCallBackPtr )

   IF ::ServiceStatusHandle != 0
      ::ServiceStatusStruct:dwCurrentState := SERVICE_START_PENDING
      SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )

      ::StopServiceEvent                 := CreateEvent( 0, .F., .F., 0 )

      ::ServiceStatusStruct:dwControlsAccepted := ::ServiceStatusStruct:dwControlsAccepted | SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN
      ::ServiceStatusStruct:dwCurrentState     := SERVICE_RUNNING
      SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )

      WHILE ( WaitForSingleObject( ::StopServiceEvent, ::TimeOut ) == WAIT_TIMEOUT ) .AND. ::ServiceStatusStruct:dwCurrentState == SERVICE_RUNNING
         IF VALTYPE( ::EventBlock ) == "B"
            EVAL( ::EventBlock, Self )
         ENDIF
      ENDDO

      ::ServiceStatusStruct:dwCurrentState := SERVICE_STOP_PENDING
      SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )
      CloseHandle( ::StopServiceEvent )
      ::StopServiceEvent := 0
      ::ServiceStatusStruct:dwControlsAccepted := ::ServiceStatusStruct:dwControlsAccepted & NOT(SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN)
      ::ServiceStatusStruct:dwCurrentState     := SERVICE_STOPPED
      SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )
   ENDIF
RETURN 0


METHOD __ServiceProc( nCode ) CLASS Service
   SWITCH nCode
      CASE SERVICE_CONTROL_INTERROGATE
           EXIT

      CASE SERVICE_CONTROL_STOP
           ::ServiceStatusStruct:dwCurrentState := SERVICE_STOP_PENDING
           SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )
           SetEvent( ::StopServiceEvent )
           RETURN 0

      CASE SERVICE_CONTROL_PAUSE
           EXIT

      CASE SERVICE_CONTROL_CONTINUE
           EXIT
   END
   SetServiceStatus( ::ServiceStatusHandle, ::ServiceStatus )
RETURN 0

CLASS ServiceController INHERIT Component, Service
   DATA File                EXPORTED  INIT ""
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Stop( nTime ) INLINE StopService( ::hServiceManager, ::hService, .T., IIF( nTime != NIL, nTime, 1000 ) ), Self
ENDCLASS

METHOD Init( oOwner ) CLASS ServiceController
   ::__xCtrlName := "ServiceController"
   ::ClsName     := "ServiceController"
   ::ComponentType := "ServiceController"
   ::Super:Init( oOwner )
   ::lCreated := .T.
RETURN Self

METHOD Create() CLASS ServiceController
   ::hServiceManager := OpenSCManager( NIL, NIL, SC_MANAGER_ALL_ACCESS )
   ::hService        := OpenService( ::hServiceManager, ::ServiceName,  SERVICE_ALL_ACCESS )
RETURN Self
#endif