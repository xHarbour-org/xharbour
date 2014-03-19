/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Object.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"
#include "error.ch"

static __aObjects := {}

CLASS Object
   PROPERTY GenerateMember ROOT "Object" DEFAULT .T.
   PROPERTY Name           ROOT "Object" SET ::__SetCtrlName(v)

   ACCESS Instance             INLINE   IIF( __GetApplication() != NIL, __GetApplication():Instance, GetModuleHandle() )

   DATA hWnd                   EXPORTED
   
   DATA Error                  EXPORTED
   DATA Parent                 EXPORTED
   DATA ThemeName              EXPORTED
   DATA hTheme                 EXPORTED

   DATA Action                 EXPORTED

   DATA __xCtrlName            EXPORTED
   DATA __lCreateAfterChildren EXPORTED INIT .F.

   DATA __ForceSysColor        EXPORTED INIT .F.
   DATA __ClassInst            EXPORTED
   DATA __lCopyCut             EXPORTED INIT .T.
   DATA __IsControl            EXPORTED INIT .F.
   DATA __IsStandard           EXPORTED INIT .F.
   DATA __IdeImageIndex        EXPORTED
   DATA __hIcon                EXPORTED
   DATA __DockChildren         EXPORTED
   DATA __CustomOwner          EXPORTED INIT .F.
   DATA __OriginalName         EXPORTED
   DATA __IdeContextMenuItems  EXPORTED INIT {}

   DATA InvisibleAtRuntime     EXPORTED INIT .F.

   DATA ClsName                EXPORTED
   DATA Children               EXPORTED INIT {}
   DATA ExtraChildren          EXPORTED INIT {}
   DATA Cargo                  EXPORTED
   DATA TreeItem               EXPORTED
   
   DATA Owner                  EXPORTED
   DATA Components             EXPORTED INIT {}
   DATA __hObjects             EXPORTED

   DATA EventHandler           EXPORTED
   DATA lComponent             EXPORTED INIT .F.
   DATA __OnInitCanceled       EXPORTED INIT .F.
   DATA __InstApp              EXPORTED
   DATA xName                  EXPORTED

   ACCESS ColorScheme          INLINE IIF( ::__ClassInst != NIL, __GetApplication():Project:AppObject:ColorTable, __GetApplication():ColorTable )

   ACCESS Application          INLINE IIF( ::__InstApp != NIL, ::__InstApp, __GetApplication() )
   ACCESS System               INLINE __GetSystem()

   DATA Events                 EXPORTED

   ACCESS Form                 INLINE IIF( ::ClsName == "VXH_FORM_IDE" .OR. ::Parent == NIL, Self, ::Parent:Form )
   ACCESS This                 INLINE Self

   ACCESS Siblings             INLINE ::Parent:Children
   PROPERTY TabOrder           SET ::SetTabOrder(v)

   METHOD HasMessage( cMsg )   INLINE __ObjHasMsg( Self, cMsg )
   METHOD HasProperty()
   METHOD __SetCtrlName()
   METHOD __ResetImageList()   VIRTUAL
   METHOD GetControlName()
   METHOD __CreateProperty()
   METHOD __SetAsProperty()

   METHOD SetWindowTheme( cSubAppName, cSubIdList ) INLINE SetWindowTheme( ::hWnd, cSubAppName, cSubIdList )
   METHOD RemoveWindowTheme()                       INLINE SetWindowTheme( ::hWnd, "", "" )

   METHOD Create()
   METHOD __InvalidMember()
   METHOD RemoveProperty()
   METHOD ObjFromHandle()
   METHOD SetTabOrder()

   error HANDLER OnError()

ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD OnError( ... ) CLASS Object
   LOCAL cMsg, uRet, aParams := HB_AParams()
   cMsg := __GetMessage()
   
   IF PCount() == 0 .AND. ::__hObjects != NIL
      IF hGetPos( ::__hObjects, UPPER( cMsg ) ) > 0
         uRet := ::__hObjects[ UPPER( cMsg ) ]
         //uRet := __aObjects[ ::__hObjects[ cMsg ] ]
       ELSE
         uRet := ::__InvalidMember( cMsg )
      ENDIF
    ELSEIF !EMPTY( cMsg )
      IF PCount() == 0

         IF ( __clsParent( ::ClassH, "CUSTOMCONTROL" ) .OR. __clsParent( ::Parent:ClassH, "CUSTOMCONTROL" ) )
            RETURN ::Form:&cMsg //::Form:__hObjects[ cMsg ]
         ENDIF
      ENDIF

      uRet := ::__InvalidMember( cMsg )
   ENDIF
RETURN uRet

METHOD HasProperty( cName ) 
RETURN ::__hObjects != NIL .AND. hGetPos( ::__hObjects, cName ) > 0

//-----------------------------------------------------------------------------------------------------------------------------
METHOD __InvalidMember( cMsg ) CLASS Object
   LOCAL uRet, oErr := ErrorNew()
   oErr:Args          := { Self, cMsg,  }
   oErr:CanDefault    := .F.
   oErr:CanRetry      := .F.
   oErr:CanSubstitute := .T.
   oErr:Description   := "Invalid Class Member"
   oErr:GenCode       := EG_NOVARMETHOD
   oErr:Operation     := cMsg
   oErr:Severity      := ES_ERROR
   oErr:SubCode       := -1
   oErr:SubSystem     := ::classname
   uRet := Eval( ErrorBlock(), oErr )
RETURN uRet

//-----------------------------------------------------------------------------------------------------------------------------
METHOD __SetCtrlName(c) CLASS Object
   IF !(::Name == c) .AND. ::Form != NIL
      ::Form:__SetAsProperty( c, Self )
      IF ::Form:hWnd == ::hWnd
         ::Application:__SetAsProperty( c, Self )
      ENDIF
      ::xName := c
   ENDIF
   IF ::__ClassInst != NIL
      ::Application:ObjectTree:Set( Self )
   ENDIF
RETURN c

//-----------------------------------------------------------------------------------------------------------------------------
METHOD __CreateProperty( cBaseName ) CLASS Object
   LOCAL n

   DEFAULT cBaseName TO ::__xCtrlName
   IF EMPTY( ::xName ) .AND. ::Form != NIL  .AND. ::__xCtrlName != "ToolTip" .AND. ::GenerateMember
      n := ::GetControlName( cBaseName )
      ::Form:__SetAsProperty( cBaseName + ALLTRIM( STR( n ) ), Self )
      IF ::Form:hWnd == ::hWnd
         ::Application:__SetAsProperty( cBaseName + ALLTRIM( STR( n ) ), Self )
      ENDIF
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------------------------------------
METHOD GetControlName( cName ) CLASS Object
   LOCAL cProp, n := 1, lComp := .T., oForm := ::Form
   IF ::Application:GenerateMembers
      WHILE ::Application != NIL .AND. oForm != NIL .AND. oForm:__hObjects != NIL
         cProp := cName + XSTR( n )
         IF hGetPos( oForm:__hObjects, cProp ) == 0
            EXIT
         ENDIF
         n ++
      ENDDO
   ENDIF
RETURN n

//-----------------------------------------------------------------------------------------------------------------------------
METHOD __SetAsProperty( cName, oObj ) CLASS Object
   LOCAL n

   IF oObj:ClsName == TOOLTIPS_CLASS .OR. ::__hObjects == NIL .OR. (::Application != NIL .AND. !::Application:GenerateMembers)
      RETURN Self
   ENDIF
   IF oObj:ClsName == "AtlAxWin" .AND. oObj:xName != NIL .AND. ! ( oObj:xName == cName ) .AND. procname(4) == "USERCONTROL:INIT"
      cName := oObj:xName
   ENDIF
   IF !( oObj == Self ) 
      IF !EMPTY( oObj:xName ) .AND. ( n := hGetPos( ::__hObjects, oObj:xName ) ) > 0
         HDelAt( ::__hObjects, n )
      ENDIF

      ::__hObjects[ cName ] := oObj

      //::__hObjects[ cName ] := ASCAN( __aObjects, oObj,,,.T. )
      //IF ::__hObjects[ cName ] == 0
      //   AADD( __aObjects, oObj )
      //   ::__hObjects[ cName ] := LEN( __aObjects )
      //ENDIF
   ENDIF
   oObj:xName := cName
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS Object
   LOCAL nRet := ExecuteEvent( "OnInit", Self )
   
   IF ::__ClassInst != NIL
      ::Application:ObjectTree:Set( Self )
   ENDIF
   IF VALTYPE( nRet ) == "N" .AND. nRet == 0
      ::__OnInitCanceled := .T.
   ENDIF

   IF !::GenerateMember .OR. ::__OnInitCanceled
      ::RemoveProperty()
   ENDIF
RETURN Self

METHOD RemoveProperty() CLASS Object
   LOCAL n
   IF !EMPTY( ::xName ) .AND. ( n := hGetPos( ::Form:__hObjects, ::xName ) ) > 0
      RETURN HDelAt( ::Form:__hObjects, n )
   ENDIF
RETURN NIL

METHOD SetTabOrder( nTabOrder ) CLASS Object
   LOCAL n, hAfter
   IF nTabOrder > 0 .AND. nTabOrder != ::xTabOrder
      TRY
         IF nTabOrder == 1
            hAfter := HWND_TOP
          ELSE
            hAfter := ::Parent:Children[ nTabOrder-1 ]:hWnd
         ENDIF
      CATCH
      END
      IF ::__ClassInst != NIL
         ::Application:ObjectTree:MoveItem( ::TreeItem, nTabOrder, ::Parent:TreeItem )

         ADEL( ::Parent:Children, ::xTabOrder, .T. )
         IF nTabOrder > LEN( ::Parent:Children )
            AADD( ::Parent:Children, Self )
          ELSE
            AINS( ::Parent:Children, nTabOrder, Self, .T. )
         ENDIF
         FOR n := 1 TO LEN( ::Parent:Children )
             ::Parent:Children[n]:xTabOrder := n
             IF ::Parent:Children[n]:__ClassInst != NIL
                ::Parent:Children[n]:__ClassInst:xTabOrder := n
             ENDIF
         NEXT
      ENDIF
      IF ::hWnd != NIL
         SetWindowPos( ::hWnd, hAfter, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE )
      ENDIF
   ENDIF
RETURN Self



//-----------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------

FUNCTION __GetObj( nPos )
RETURN __aObjects[nPos]

FUNCTION __SetObjPtr( oObj )
   LOCAL n := ASCAN( __aObjects, {|o|o==oObj} )
   IF n == 0
      AADD( __aObjects, oObj )
   ENDIF
RETURN NIL

FUNCTION __ObjFromClassH( hClass )
   LOCAL n := ASCAN( __aObjects, {|o|o:ClassH == hClass } )
   IF n > 0
      RETURN __aObjects[n]
   ENDIF
RETURN NIL

FUNCTION __ObjFromName( cName, oForm )
   LOCAL n := ASCAN( __aObjects, {|o|o:ClsName == cName .AND. o:Form == oForm } )
   IF n > 0
      RETURN __aObjects[n]
   ENDIF
RETURN NIL

FUNCTION ObjFromHandle( hWnd, lRemove )
   LOCAL n := ASCAN( __aObjects, {|o|o:hWnd==hWnd} )
   DEFAULT lRemove TO .F.
   IF n > 0
      IF lRemove
         ADEL( __aObjects, n, .T. )
       ELSE
         RETURN __aObjects[n]
      ENDIF
   ENDIF
RETURN NIL

FUNCTION TraceObj()
   LOCAL n
   FOR n := 1 TO LEN( __aObjects )
       VIEW __aObjects[n]:Name, __aObjects[n]:ClsName
   NEXT
RETURN NIL

METHOD ObjFromHandle( hWnd ) CLASS Object
   LOCAL n, oObj
   IF ::hWnd == hWnd
      RETURN Self
   ENDIF
   FOR n := 1 TO LEN( ::Children )
       IF ::Children[n]:hWnd == hWnd
          oObj := ::Children[n]
          EXIT
       ELSE
          oObj := ::Children[n]:ObjFromHandle( hWnd )
       ENDIF
   NEXT
RETURN oObj
