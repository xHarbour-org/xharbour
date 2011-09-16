#include "vxh.ch"
#include "ole.ch"
#include "debug.ch"

#define OLEIVERB_PROPERTIES (-7)

INIT PROCEDURE LoadAtl()
   __LoadAtlAx()
RETURN

EXIT PROCEDURE UnLoadAtl()
   __UnLoadAtlAx()
RETURN


#define VT_EMPTY                  0
#define VT_NULL                   1
#define VT_I2                     2
#define VT_I4                     3
#define VT_R4                     4
#define VT_R8                     5
#define VT_CY                     6
#define VT_DATE                   7
#define VT_BSTR                   8
#define VT_DISPATCH               9
#define VT_ERROR                 10
#define VT_BOOL                  11
#define VT_VARIANT               12
#define VT_UNKNOWN               13
#define VT_DECIMAL               14
#define VT_I1                    16
#define VT_UI1                   17
#define VT_UI2                   18
#define VT_UI4                   19
#define VT_I8                    20
#define VT_UI8                   21
#define VT_INT                   22
#define VT_UINT                  23
#define VT_VOID                  24
#define VT_HRESULT               25
#define VT_PTR                   26
#define VT_SAFEARRAY             27
#define VT_CARRAY                28
#define VT_USERDEFINED           29
#define VT_LPSTR                 30
#define VT_LPWSTR                31
#define VT_RECORD                36
#define VT_FILETIME              64
#define VT_BLOB                  65
#define VT_STREAM                66
#define VT_STORAGE               67
#define VT_STREAMED_OBJECT       68
#define VT_STORED_OBJECT         69
#define VT_BLOB_OBJECT           70
#define VT_CF                    71
#define VT_CLSID                 72
#define VT_VERSIONED_STREAM      73
#define VT_BSTR_BLOB          0xfff
#define VT_VECTOR             0x1000
#define VT_ARRAY              0x2000
#define VT_BYREF              0x4000
#define VT_RESERVED           0x8000
#define VT_ILLEGAL            0xffff
#define VT_ILLEGALMASKED      0xfff
#define VT_TYPEMASK           0xfff

#define OLEMISC_RECOMPOSEONRESIZE            1
#define OLEMISC_ONLYICONIC                   2
#define OLEMISC_INSERTNOTREPLACE             4
#define OLEMISC_STATIC                       8
#define OLEMISC_CANTLINKINSIDE               16
#define OLEMISC_CANLINKBYOLE1                32
#define OLEMISC_ISLINKOBJECT                 64
#define OLEMISC_INSIDEOUT                    128
#define OLEMISC_ACTIVATEWHENVISIBLE          256
#define OLEMISC_RENDERINGISDEVICEINDEPENDENT 512
#define OLEMISC_INVISIBLEATRUNTIME           1024
#define OLEMISC_ALWAYSRUN                    2048
#define OLEMISC_ACTSLIKEBUTTON               4096
#define OLEMISC_ACTSLIKELABEL                8192
#define OLEMISC_NOUIACTIVATE                 16384
#define OLEMISC_ALIGNABLE                    32768
#define OLEMISC_SIMPLEFRAME                  65536
#define OLEMISC_SETCLIENTSITEFIRST           131072
#define OLEMISC_IMEMODE                      262144
#define OLEMISC_IGNOREACTIVATEWHENVISIBLE    524288
#define OLEMISC_WANTSTOMENUMERGE             1048576
#define OLEMISC_SUPPORTSMULTILEVELUNDO       2097152

#define DVASPECT_CONTENT                     1
#define DVASPECT_THUMBNAIL                   2
#define DVASPECT_ICON                        4
#define DVASPECT_DOCPRINT                    8

#DEFINE SECURITY_ACCESS_MASK 983103

#define HKEY_CLASSES_ROOT           0x80000000
#define HKEY_LOCAL_MACHINE          0x80000002
#define KEY_ALL_ACCESS              (0xF003F)


//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

CLASS ActiveX INHERIT ToleAuto, UserControl

   DATA BackColor     EXPORTED
   DATA ForeColor     EXPORTED
   DATA Font          EXPORTED
   DATA ToolTip       EXPORTED
   DATA Cursor        EXPORTED
   DATA ProgID        PUBLISHED
   DATA ClsID         PUBLISHED
   DATA OleVerb      PUBLISHED INIT __GetSystem():OleVerb:Show
   DATA hEventHandler PROTECTED
   DATA __IUnknown    EXPORTED
   DATA oTypeLib      EXPORTED
   DATA __OleVars     EXPORTED
   DATA __LoadEvents  EXPORTED INIT .T.
   DATA Constants     EXPORTED

   PROPERTY StaticEdge    INDEX WS_EX_STATICEDGE    READ xStaticEdge    WRITE SetExStyle DEFAULT .F.
   PROPERTY ClientEdge    INDEX WS_EX_CLIENTEDGE    READ xClientEdge    WRITE SetExStyle DEFAULT .F.
   PROPERTY ControlParent INDEX WS_EX_CONTROLPARENT READ xControlParent WRITE SetExStyle DEFAULT .F.

   PROPERTY Center                                  READ xCenter        WRITE CenterWindow DEFAULT .F.

   PROPERTY ClipChildren  INDEX WS_CLIPCHILDREN     READ xClipChildren  WRITE SetStyle   DEFAULT .T.
   PROPERTY ClipSiblings  INDEX WS_CLIPSIBLINGS     READ xClipSiblings  WRITE SetStyle   DEFAULT .T.

   PROPERTY SmallCaption  READ  xSmallCaption WRITE SetSmallCaption DEFAULT .F.

   DATA AllowUnDock       INIT FALSE
   DATA AllowClose        INIT FALSE
   DATA xCaption               EXPORTED  INIT ""
   ACCESS Caption              INLINE    ::xCaption
   ASSIGN Caption(c)           INLINE    ::xCaption := c

   DATA oServer PROTECTED

   //ACCESS hWnd INLINE IIF( __objHasMsg( ::TOleAuto, "HWND" ), ::TOleAuto:hWnd, ::hWnd )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD __OpExactEqual()
   METHOD OnDestroy()
   METHOD __GetEventList()
   METHOD ShowPropertiesDialog()
   METHOD SetStyle()
   METHOD Configure()
   METHOD __GetObjProp()
   METHOD AxSet( cName, xValue ) INLINE __AxSetProperty( ::hObj, cName, xValue )
   METHOD AxGet( cName )         INLINE __AxGetProperty( ::hObj, cName )
   METHOD Translate( pMsg )      INLINE __AxTranslateMessage( ::__IUnknown, pMsg:Value )
   METHOD Destroy()
   METHOD OnGetDlgCode()
   METHOD IsRegistered()
ENDCLASS

//----------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS ActiveX
   ::Parent       := oParent
   ::ClsName      := "AtlAxWin"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   IF ::Parent != NIL .AND. ::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance   := .T.
   ENDIF

   ::Dock         := __WindowDock( Self )
   ::Anchor       := __AnchorSet( Self )
   ::EventHandler := {=>}
   ::Constants    := {=>}
   DEFAULT ::xWidth  TO 32
   DEFAULT ::xHeight TO 32

RETURN Self

METHOD OnGetDlgCode( msg ) CLASS ActiveX
RETURN DLGC_WANTMESSAGE | DLGC_WANTALLKEYS

//----------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS ActiveX
   EXTERN CreateActiveX

   LOCAL nStatus, o, cId, n, oServer, cHandle, cEvent, oApp, ProgID, hEventHandler := {=>}

   DEFAULT ::ClsID TO ::ProgID

   //IF !::IsRegistered()
   //   ::Form:MessageBox( 'Object "' + ::ProgID + '" is not registered or one of its components failed to load', "COM Error", MB_ICONEXCLAMATION )
   //   RETURN Self
   //ENDIF

   TRY
      DEFAULT ::oTypeLib TO LoadTypeLib( ::ClsID, .F. )
      cId := ::oTypeLib:Objects[1]:Name
    CATCH
      cId := STRTRAN( ::ProgID, "." )
   END

   DO CASE
      CASE cId == "ChartSpace"
           ::Width  := 576
           ::Height := 384

      CASE cId == "Spreadsheet"
           ::Width  := 576
           ::Height := 288
   ENDCASE

   ::ClsName     := "AtlAxWin"

   DEFAULT ::__xCtrlName TO cId

   ::UserControl:Init( ::Parent )
   ExecuteEvent( "OnInit", Self )

   ::Caption    := ::ClsID
   ::UserControl:Create()

   ::__IUnknown := __AxGetUnknown( ::hWnd )
   ::hObj       := __AxGetDispatch( ::__IUnknown, ::hWnd, ::OleVerb )

   ::cClassName := ::ClsID

   IF ::__ClassInst != NIL
      ::__GetEventList(.T.)
   ENDIF
   IF ::__ClassInst == NIL
      FOR EACH cHandle IN ::EventHandler:Keys
          cEvent := ::EventHandler[ cHandle ]
          hEventHandler[ cHandle ] := {|p1, p2, p3, p4, p5, p6, p7, p8, p9| __objSendMsg( ::Form, UPPER( ::EventHandler[ cHandle ] ), Self, @p1, @p2, @p3, @p4, @p5, @p6, @p7, @p8, @p9 ) }
      NEXT

      IF ( oServer  := WrapTypeLib( ::oTypeLib, ::ProgID, "OleXWrapper", ::ClsID ) ) != NIL
         oServer:hObj := ::hObj
         oServer:hWnd := ::hWnd
         oServer:ConnectEvents( hEventHandler )
         ::oServer := oServer
      ENDIF

    ELSE
      ::__IdeContextMenuItems := { { "Properties", {|| ::ShowPropertiesDialog( GetActiveWindow() ) } } }
      __DeleteEvents( ::Events,{ "OnLoad" } )
   ENDIF

   IF ( nStatus := __AxGetMiscStatus( ::hObj, DVASPECT_CONTENT ) ) != NIL
      ::InvisibleAtRuntime := ( nStatus & OLEMISC_INVISIBLEATRUNTIME ) == OLEMISC_INVISIBLEATRUNTIME
   ENDIF

   IF ::InvisibleAtRuntime
      IF ::__ClassInst != NIL
         ::__lResizeable := {.F.,.F.,.F.,.F.,.F.,.F.,.F.,.F.}
       ELSE
         ::Hide()
      ENDIF
   ENDIF
   IF !::Visible .AND. ::__ClassInst == NIL
      ::Hide()
   ENDIF
   ::Border := ::xBorder
RETURN Self

METHOD OnDestroy() CLASS ActiveX
   LOCAL n

   IF ::oServer != NIL
      ::oServer:DisconnectEvents()
   ENDIF

   IF ::__IUnknown != NIL
      __AxReleaseUnknown( ::__IUnknown )
   ENDIF
RETURN NIL

METHOD Destroy() CLASS ActiveX
   LOCAL nRet
   DestroyWindow( ::hWnd )
   ::__WindowDestroy()

   nRet := ExecuteEvent( "OnNcDestroy", Self )
   DEFAULT nRet TO ::OnNcDestroy()

   __AxReleaseDispatch( ::hObj )
//   __AxReleaseUnknown( ::__IUnknown )

RETURN Self

METHOD Configure() CLASS ActiveX
   ::OnCreate()
   ExecuteEvent( "OnCreate", Self )
RETURN Self

METHOD __OpExactEqual( oObj ) CLASS ActiveX
   LOCAL lRet
   TRY
      lRet := oObj:hObj == ::hObj
    catch
      lRet := oObj:ClassH == ::ClassH
   END
RETURN lRet

METHOD __GetObjProp( oObj ) CLASS ActiveX
   LOCAL n, Interface, cArg, Arg, cHandle, Property, Enumeration, Object, Constant, xVal, cProp, o, xVal2, lReadOnly, xVal3
   LOCAL __OleVars := {=>}
   WITH OBJECT oObj
      FOR EACH Property IN :Properties
          cProp := Property:Name
          IF cProp != "Picture" .AND. cProp != "XMLData" .AND. cProp != "HTMLData" .AND. !__objHasMsg( ::UserControl, UPPER( cProp ) )
             xVal  := NIL
             xVal2 := NIL
             lReadOnly := Property:ReadOnly
             TRY
                SWITCH Property:VT
                   CASE VT_VOID
                        IF LEFT( Property:Arguments[1]:TypeDesc, 3 ) == "VT_"
                           cArg := Property:Arguments[1]:TypeDesc
                         ELSE
                           cArg := SUBSTR( Property:Arguments[1]:TypeDesc, 13 )
                        ENDIF
                        IF ( n := ASCAN( ::oTypeLib:Enumerations, {|o| o:Name == cArg } ) ) > 0
                           o := ::oTypeLib:Enumerations[n]
                           xVal  := o:Constants
                           xVal2 := ::AxGet( cProp ) //::&cProp
                         ELSE
                           IF Property:Arguments[1]:TypeDesc != "VT_UNKNOWN" //.AND. Property:Arguments[1]:TypeDesc != "VT_I4" .AND. Property:Arguments[1]:TypeDesc != "VT_I2"
                              xVal  := ::AxGet( cProp ) //::&cProp
                           ENDIF
                        ENDIF
                        EXIT

                   CASE VT_PTR
                        EXIT
                END
                IF Property:VT != VT_VOID
                   DEFAULT xVal TO ::AxGet( cProp ) //::&cProp
                ENDIF
             catch
                xVal := NIL
             END
             //xVal3 := NIL
             //IF VALTYPE( xVal ) == "O"
             //   xVal3 := ::__GetObjProp( xVal:hObj )
             //ENDIF
             __OleVars[ cProp ] := { xVal, xVal, lReadOnly, xVal2, Property:HelpString }
           ELSE
          ENDIF

      NEXT
   END
RETURN __OleVars

METHOD __GetEventList( lVars ) CLASS ActiveX
   LOCAL Event, n, Interface, cArg, Arg, cHandle, Property, Enumeration, Object, Constant, xVal, cProp, o, xVal2, lReadOnly
   IF ::__ClassInst != NIL

      DEFAULT ::ClsID TO ::ProgID
      TRY
         DEFAULT ::oTypeLib TO LoadTypeLib( ::ClsID, .T. )
       CATCH
      END
      IF lVars
         TRY
            ::__OleVars := ::__GetObjProp( ::oTypeLib:Objects[1]:Interfaces[1] )
          CATCH
         END
      ENDIF

      IF ::__LoadEvents
         ::Events := {;
            {"Interface",   {;
                            { "OnCreate"           , "", "" },;
                            { "OnInit"             , "", "" } } },;
            {"Mouse",       {;
                            { "OnLButtonDown"      , "", "" },;
                            { "OnLButtonUp"        , "", "" },;
                            { "OnRButtonDown"      , "", "" },;
                            { "OnRButtonUp"        , "", "" },;
                            { "OnMouseActivate"    , "", "" } } },;
            {"Menu",        {;
                            { "OnContextMenu"      , "", "" } } } }


         IF ::oTypeLib != NIL
            FOR EACH Interface IN ::oTypeLib:Interfaces
                IF !EMPTY( Interface:Events )
                   AADD( ::Events, { Interface:Name, {} } )
                   FOR EACH Event IN Interface:Events
                       cArg := ""
                       FOR EACH Arg IN Event:Arguments
                           IF !EMPTY( cArg )
                              cArg += ", "
                           ENDIF
                           cArg += Arg:Name
                       NEXT
                       AADD( ::Events[-1][2], { Event:Name, "", cArg } )
                   NEXT
                ENDIF
            NEXT
         ENDIF
      ENDIF

   ENDIF
RETURN Self

METHOD SetStyle( nStyle, lAdd ) CLASS ActiveX
   LOCAL nSt, cStyle := "", n
   DEFAULT lAdd TO .T.
   IF ::IsWindow()
      ::Style := ::GetWindowLong( GWL_STYLE )
   ENDIF
   IF nStyle == WS_DISABLED
      lAdd := !lAdd
   ENDIF
   IF lAdd
      ::Style := ::Style | nStyle
    ELSE
      ::Style := ::Style & NOT( nStyle )
   ENDIF
   IF ::IsWindow()
      SWITCH nStyle
         CASE WS_VISIBLE
              IF ::__ClassInst != NIL
                 RETURN NIL
              ENDIF
              IF lAdd
                 ShowWindow( ::hWnd, SW_SHOW )
               ELSE
                 ShowWindow( ::hWnd, SW_HIDE )
              ENDIF
              EXIT
         CASE WS_DISABLED
              EnableWindow( ::hWnd, lAdd )
              EXIT

      END
      ::SetWindowLong( GWL_STYLE, ::Style )
      ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
      ::RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
   ENDIF
RETURN self


METHOD ShowPropertiesDialog( hWnd, lShow ) CLASS ActiveX
   LOCAL n, cProp, oItem, lChanged := .F.
   LOCAL Property, IUnknown, IDispatch, oOle, hAtl, oServer, xVal, xValue
   DEFAULT lShow TO .T.

   hAtl            := CreateWindowEx( 0, "AtlAxWin", ::ClsID, WS_CHILD, 0, 0, 0, 0, hWnd, 101, ::AppInstance, NIL )
   IUnknown        := __AxGetUnknown( hAtl )
   IDispatch       := __AxGetDispatch( IUnknown )
   oOle            := TOleAuto():New( ::ClsID )
   oOle:hObj       := IDispatch
   oOle:cClassName := ::ClsID

   FOR EACH cProp IN ::__OleVars:Keys
       TRY
         IF VALTYPE( oOle:&cProp ) != "O"
            IF !::__OleVars[cProp][3]
               __objSendMsg( oOle, "_"+UPPER( cProp ), IIF( ::__OleVars[cProp][4] != NIL, ::__OleVars[cProp][4], ::__OleVars[cProp][1] ) )
            ENDIF
         ENDIF
        catch
       END
   NEXT

   IF lShow
      TRY
         __AxGetPropertiesDialog( hWnd, IUnknown )
       catch
      END
   ENDIF

   FOR EACH cProp IN ::__OleVars:Keys
       TRY
         xValue := __objSendMsg( oOle, cProp )
         IF VALTYPE( xValue ) != "O"
            IF lShow
               IF !lChanged
                  lChanged := IIF( ::__OleVars[cProp][4] != NIL, ::__OleVars[cProp][4], ::__OleVars[cProp][1] )  != xValue
               ENDIF
               IF lChanged
                  IF ::__OleVars[cProp][4] != NIL
                     ::__OleVars[cProp][4] := xValue
                   ELSE
                     ::__OleVars[cProp][1] := xValue
                  ENDIF
               ENDIF
             ELSE
               IF ::__OleVars[cProp][4] != NIL
                  ::__OleVars[cProp][4] := xValue
                ELSE
                  ::__OleVars[cProp][1] := xValue
                  ::__OleVars[cProp][2] := xValue
               ENDIF
            ENDIF
         ENDIF
        catch
       END
   NEXT

   DestroyWindow( hAtl )
   __AxReleaseUnknown( IUnknown )
   __AxReleaseDispatch( IDispatch )


   IF lChanged
      ::Application:ObjectManager:ActiveObject := ::Application:Project:CurrentForm
      ::Application:ObjectManager:ResetProperties( {{Self}} )

      oItem := ::Application:ObjectManager:SearchString( "COM Properties" )
      IF oItem != NIL
         oItem:EnsureVisible()
         oItem:Expand()
      ENDIF
      ::Application:Project:Modified := .T.
   ENDIF

RETURN Self

METHOD IsRegistered() CLASS ActiveX
   LOCAL hKey, cKey, lReg := .F.
   RegOpenKeyEx( HKEY_CLASSES_ROOT, ::ProgID + "\Clsid", 0, SECURITY_ACCESS_MASK, @hKey )
   RegQueryValueEx( hKey, NIL,,, @cKey )
   RegCloseKey( hKey )
   IF !EMPTY( cKey )
      RegOpenKeyEx( HKEY_CLASSES_ROOT, "CLSID\" + cKey + "\InprocServer32", 0, SECURITY_ACCESS_MASK, @hKey )
      RegQueryValueEx( hKey, NIL,,, @cKey )
      RegCloseKey( hKey )
      lReg := !EMPTY(cKey) .AND. FILE(cKey)
   ENDIF
RETURN lReg


FUNCTION GetRegOleBmp( cID )
   LOCAL n, hKey, cBmp, hBmp, aBmp
   cBmp := GetOleIcon( cID )
   IF Empty( cBmp )
      RETURN NIL
   ENDIF
   aBmp := hb_aTokens( cBmp, "," )
   IF LEN( aBmp ) >= 2
      hBmp := ExtractIcon( __GetApplication():Instance, aBmp[1], 0 )
   ENDIF
RETURN hBmp

FUNCTION GetRegOle()
   LOCAL lControl, i, n, hKey, aBmp, cProg, iPos, hSub, nPos, cKey, cSub, cDes, cBmp, hBmp, cVer, hVer, hDes, cPath, hPath, aOle := {}
   aOle := EnumRegDLL()
   aSort( aOle,,,{|x, y| UPPER(x[1]) < UPPER(y[1])})
RETURN aOle

FUNCTION IsDotNet( cFrameworkPath, cVersion )
   LOCAL hKey, nAt
   cVersion       := ""
/*
   IF RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\Microsoft\ASP.NET", 0, KEY_ALL_ACCESS, @hKey ) == 0
      RegQueryValueEx( hKey, "RootVer",,, @cVersion )
      RegCloseKey( hKey )
   ENDIF

   IF EMPTY( cVersion )
      RETURN .F.
   ENDIF

   IF ( nAt := AT( ".", cVersion, 3 ) ) > 0
      cVersion := SUBSTR(cVersion,1,nAt) + "0"
   ENDIF

   IF RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\Microsoft\ASP.NET\" + cVersion, 0, KEY_ALL_ACCESS, @hKey ) == 0
      RegQueryValueEx( hKey, "PATH",,, @cFrameworkPath )
      RegCloseKey( hKey )
   ENDIF
*/
   DEFAULT cFrameworkPath TO "c:\WINDOWS\Microsoft.NET\Framework\v2.0.50727"
RETURN .T.

FUNCTION RegisterDotNetComponent( cDotNetDLL, cProgId, cError )
   LOCAL cFrameworkPath, cVersion, lUnregister, cRun, lResult, cPath, cGAC := "C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0\Bin\gacutil.exe"

   IF cProgId == "UNREGISTER"
      lUnregister := .T.
      cProgId := ""
    ELSE
      lUnregister := .F.
   ENDIF

   IF !EMPTY( cProgId ) .AND. IsComObject( cProgId )
      cError := ""
      RETURN .T.
   ENDIF

   IF !IsDotNet(@cFrameworkPath)
      cError := "DotNet Framework not installed or path not found."
      RETURN .F.
   ENDIF

   cRun := cFrameworkPath + "\regasm.exe"

   IF !FILE( cRun )
      cError := "Couldn't find RegAsm.exe at:" + CHR(13) + cFrameworkPath + "\regasm.exe"
      RETURN .F.
   ENDIF

   ShellExecute( GetActiveWindow(), "open", cRun, cDotNetDll + " /tlb", , SW_HIDE )
   sleep( 2000 )

   ShellExecute( GetActiveWindow(), "open", cGAC, " /i " + cDotNetDll, , SW_HIDE )
   sleep( 2000 )


   IF EMPTY(cProgId)
      RETURN .T.
   ENDIF

   lResult :=  IsComObject(cProgId)
   IF !lResult
      cError := "Registration of " + cDotNetDLL + " failed. " + CHR(13) + CHR(13) + "The command line has been pasted into your ClipBoard"
   ENDIF
RETURN lResult

FUNCTION IsComObject( cProgId )
   LOCAL oObj
   TRY
      oObj := CreateObject( cProgId )
    CATCH
   END
RETURN oObj != NIL
/*
FUNCTION AtlForwardMessage ( _
   BYVAL hWnd  AS DWORD, _   ' handle of window
   BYREF uMsg  AS tagMSG _   ' message information
   ) AS LONG

   ' Default return value
   FUNCTION = %FALSE

   ' Retrieve the handle of the window that hosts the WebBrowser control
   LOCAL hCtrl AS DWORD
   hCtrl = GetDlgItem(hWnd, %IDC_IEWB)

   ' Retrieve the ancestor of the control that has the focus
   LOCAL hWndCtrl AS DWORD
   hWndCtrl = GetFocus
   DO
      IF ISFALSE GetParent(hWndCtrl) OR GetParent(hWndCtrl) = hWnd THEN EXIT DO
      hWndCtrl = GetParent(hWndCtrl)
   LOOP

   ' If the focus is in the WebBrowser, forward the message to it
   IF hCtrl = hWndCtrl THEN
      IF ISTRUE SendMessage(hCtrl, &H37F, 0, VARPTR(uMsg)) THEN FUNCTION = %TRUE
   END IF

END FUNCTION
*/
