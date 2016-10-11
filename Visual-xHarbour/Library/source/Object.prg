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

#include "hbxml.ch"

#define MXML_STYLE_INDENT        1
#define MXML_STYLE_THREESPACES   4

static __aObjects := {}
static __aObjPtrs := {}

//EXIT PROCEDURE Clean__aObjPtrs
//   view len( __aObjPtrs )
//RETURN

CLASS Object
   PROPERTY GenerateMember ROOT "Object" DEFAULT .T.
   PROPERTY Name           ROOT "Object" SET ::__SetCtrlName(v)

   ACCESS Instance             INLINE   IIF( __GetApplication() != NIL, __GetApplication():Instance, GetModuleHandle() )
   ACCESS MainForm             INLINE   IIF( __GetApplication() != NIL, __GetApplication():MainForm, )
   DATA hWnd                   EXPORTED

   DATA Error                  EXPORTED
   DATA Parent                 EXPORTED
   DATA ThemeName              EXPORTED
   DATA hTheme                 EXPORTED

   DATA Action                 EXPORTED

   DATA __xCtrlName            EXPORTED
   DATA __lCreateAfterChildren EXPORTED INIT .F.
   DATA __aExcludeProperties   EXPORTED INIT {}
   DATA __ForceSysColor        EXPORTED INIT .F.
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

   //------------------------------------------------------
   DATA lDesignMode            EXPORTED INIT .F.
   ACCESS DesignMode           INLINE IIF( ::Form != NIL, ::Form:lDesignMode, .F. )
   ASSIGN DesignMode(l)        INLINE ::lDesignMode := l
   //------------------------------------------------------


   ACCESS ColorScheme          INLINE IIF( ::DesignMode .AND. __GetApplication():Project:AppObject != NIL, __GetApplication():Project:AppObject:ColorTable, __GetApplication():ColorTable )

   ACCESS Application          INLINE __GetApplication()
   ACCESS System               INLINE __GetSystem()

   DATA Events                 EXPORTED

   ACCESS Form                 INLINE IIF( ::ClsName == "VXH_FORM_IDE" .OR. ::Parent == NIL, Self, ::Parent:Form )
   ACCESS This                 INLINE Self

   ACCESS Siblings             INLINE ::Parent:Children
   PROPERTY TabOrder           SET ::SetTabOrder(v) DEFAULT 0

   METHOD HasMessage( cMsg )   INLINE __ObjHasMsg( Self, cMsg )
   METHOD HasProperty()
   METHOD __SetCtrlName()
   METHOD __ResetImageList()   VIRTUAL
   METHOD Reload()             VIRTUAL
   METHOD GetControlName()
   METHOD __CreateProperty()
   METHOD __SetAsProperty()

   METHOD SetWindowTheme( cSubAppName, cSubIdList ) INLINE SetWindowTheme( ::hWnd, cSubAppName, cSubIdList )
   METHOD RemoveWindowTheme()                       INLINE SetWindowTheme( ::hWnd, "", "" )

   METHOD Create()
   METHOD __InvalidMember()
   METHOD RemoveProperty()
   METHOD SetTabOrder()

   METHOD GetXML( oNode, aEvents ) INLINE Obj2XML( Self, oNode,, @aEvents )

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
      IF IsWindow(::hWnd) .AND. ::Form:hWnd == ::hWnd
         ::Application:__SetAsProperty( c, Self )
      ENDIF
      ::xName := c
   ENDIF
   IF ::DesignMode
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
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------------------------------------
METHOD GetControlName( cName ) CLASS Object
   LOCAL cProp, n := 1, lComp := .T., oForm := ::Form
   IF ::Application:GenerateMembers .OR. ::Form:GenerateMembers .OR. ::DesignMode
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

   IF oObj:ClsName == TOOLTIPS_CLASS .OR. ::__hObjects == NIL .OR. (::Application != NIL .AND. ( ! ::Application:GenerateMembers .OR. ! ::Form:GenerateMembers ) .AND. ! ::DesignMode )
      RETURN Self
   ENDIF
   IF oObj:ClsName == "AtlAxWin" .AND. oObj:xName != NIL .AND. ! ( oObj:xName == cName ) .AND. procname(4) == "USERCONTROL:INIT"
      cName := oObj:xName
   ENDIF
   IF ! ( oObj == Self )
      IF !EMPTY( oObj:xName ) .AND. ( n := hGetPos( ::__hObjects, oObj:xName ) ) > 0
         HDelAt( ::__hObjects, n )
      ENDIF
      ::__hObjects[ cName ] := IIF( ! ::DesignMode, oObj, oObj:hWnd )
   ENDIF
   oObj:xName := cName
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS Object
   LOCAL nRet := ExecuteEvent( "OnInit", Self )

   IF ::DesignMode
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
   IF ! EMPTY( ::xName ) .AND. ::Form:hWnd <> ::hWnd
      IF ( n := hGetPos( ::Form:__hObjects, ::xName ) ) > 0
         HDelAt( ::Form:__hObjects, n )
      ENDIF
      IF ( n := hGetPos( ::Application:__hObjects, ::xName ) ) > 0
         HDelAt( ::Application:__hObjects, n )
      ENDIF
   ENDIF
RETURN NIL

METHOD SetTabOrder( nTabOrder ) CLASS Object
   LOCAL n, hAfter
   DEFAULT nTabOrder TO 0
   IF nTabOrder > 0 .AND. nTabOrder != ::xTabOrder
      IF nTabOrder == 1
         hAfter := HWND_TOP
       ELSEIF nTabOrder >= LEN( ::Parent:Children )
         hAfter := HWND_BOTTOM
       ELSE
         hAfter := ::Parent:Children[ nTabOrder-1 ]:hWnd
      ENDIF
      IF ::DesignMode
         ADEL( ::Parent:Children, ::xTabOrder, .T. )
         IF nTabOrder > LEN( ::Parent:Children )
            AADD( ::Parent:Children, Self )
            nTabOrder := LEN( ::Parent:Children )
          ELSE
            AINS( ::Parent:Children, nTabOrder, Self, .T. )
         ENDIF

         //::TreeItem:SetPosition( nTabOrder )
         ::TreeItem := ::Application:ObjectTree:MoveItem( ::TreeItem, nTabOrder, ::Parent:TreeItem )

         FOR n := 1 TO LEN( ::Parent:Children )
             ::Parent:Children[n]:xTabOrder := n
             IF ::Parent:Children[n]:DesignMode
                __SetInitialValues( Self, "TabOrder", n )
             ENDIF
         NEXT
      ENDIF
      IF ::hWnd != NIL
         SetWindowPos( ::hWnd, hAfter, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE) )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------

FUNCTION __SetWindowObjPtr( oObj )
   LOCAL nPtr
   IF IsWindow( oObj:hWnd ) .AND. GetProp( oObj:hWnd, "WINOBJPTR" ) == 0
      IF ( nPtr := ArrayPointer( oObj ) ) <> 0
         SetProp( oObj:hWnd, "WINOBJPTR", nPtr )
      ENDIF
   ENDIF
RETURN NIL

FUNCTION ObjFromHandle( hWnd, lRemove )
   LOCAL oObj, nPtr
   DEFAULT lRemove TO .F.

   IF IsWindow( hWnd ) .AND. ( nPtr := GetProp( hWnd, "WINOBJPTR" ) ) <> 0
      IF ! lRemove
         oObj := ArrayFromPointer( nPtr )
       ELSE
         ReleaseArrayPointer( nPtr )
         RemoveProp( hWnd, "WINOBJPTR" )
      ENDIF
   ENDIF
RETURN oObj

//---------------------------------------------------------------------------
FUNCTION __ObjPtr( oObj )
   LOCAL nPtr := __GetObjID( oObj )
   LOCAL n := ASCAN( __aObjPtrs, {|a| a[2] == nPtr } )
   IF n == 0
      AADD( __aObjPtrs, { oObj, nPtr } )
   ENDIF
RETURN nPtr

FUNCTION __ObjFromPtr( nPtr )
   LOCAL n := ASCAN( __aObjPtrs, {|a| a[2] == nPtr} )
   IF n > 0
      RETURN __aObjPtrs[n][1]
   ENDIF
RETURN NIL

FUNCTION __ObjRelPtr( nPtr )
   LOCAL n := ASCAN( __aObjPtrs, {|a| a[2] == nPtr} )
   IF n > 0
      ADEL( __aObjPtrs, n, .T. )
   ENDIF
RETURN NIL

FUNCTION __ObjFromID( nID, hWnd )
   LOCAL n := ASCAN( __aObjPtrs, {|a| a[1]:Form:hWnd == hWnd .AND. a[1]:Id == nID} )
   IF n > 0
      RETURN __aObjPtrs[n][1]
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------
FUNCTION Obj2XML( oObj, oNode, cName, aEvents )
   LOCAL xInit, cProp, oProp, aProperty, xValue, aProperties, oXML, n := 0
   LOCAL oComponent, oComps, aEvent, aTopic, oEvents, oControls, oProperties, oCtrl

   oXML := TXmlNode():new( , IIF( ! Empty(cName), cName, oObj:__xCtrlName ) )
   aProperties := __ClsGetPropertiesAndValues( oObj )

   IF __objHasMsg( oObj, "Components" )
      FOR EACH oComponent IN oObj:Components
          DEFAULT oComps TO TXmlNode():new( , "Components" )
          Obj2XML( oComponent, oComps,, @aEvents )
      NEXT
      IF oComps != NIL
         oXML:addBelow( oComps )
      ENDIF
   ENDIF

   FOR EACH aProperty IN aProperties
       cProp  := lower( aProperty[1] )
       DEFAULT oProperties TO TXmlNode():new( , "Properties" )
       xValue := __objSendMsg( oObj, cProp )
       xInit  := NIL
       IF __objHasMsg( oObj, "__a_" + cProp )
          xInit := __objSendMsg( oObj, "__a_" + cProp )[4]
          cProp := __objSendMsg( oObj, "__a_" + cProp )[1]
       ENDIF
       IF ValType( xValue ) == "O"

          IF __objHasMsg( xValue, "lComponent" ) .AND. xValue:lComponent
             oProp := TXmlNode():new( HBXML_TYPE_TAG, cProp, NIL, ValToPrgExp(xValue:Name) )
             oProperties:addBelow( oProp )
             n++
           ELSE
             Obj2XML( xValue, oProperties, cProp, @aEvents )
          ENDIF

        ELSEIF Valtype( xValue ) $ "CNDL"

          IF xInit != xValue
             oProp := TXmlNode():new( HBXML_TYPE_TAG, cProp, NIL, ValToPrgExp(xValue) )
             oProperties:addBelow( oProp )
             n++
          ENDIF
       ENDIF
   NEXT

   IF oProperties != NIL
      oXML:addBelow( oProperties )
   ENDIF

   IF __objHasMsg( oObj, "Children" )
      FOR EACH oCtrl IN oObj:Children
          DEFAULT oControls TO TXmlNode():new( , "Controls" )
          Obj2XML( oCtrl, oControls,, @aEvents )
      NEXT
      IF oControls != NIL
         oXML:addBelow( oControls )
      ENDIF
   ENDIF

   IF __objHasMsg( oObj, "Events" ) .AND. oObj:Events != NIL
      FOR EACH aTopic IN oObj:Events
          FOR EACH aEvent IN aTopic[2]
              IF ! EMPTY( aEvent[2] )
                 DEFAULT oEvents TO TXmlNode():new( , "Events" )
                 oProp := TXmlNode():new( HBXML_TYPE_TAG, aEvent[1], NIL, ValToPrgExp( aEvent[2] ) )
                 oEvents:addBelow( oProp )
                 IF aEvents != NIL
                    AADD( aEvents, aEvent[2] )
                 ENDIF
                 n++
              ENDIF
          NEXT
      NEXT
      IF oEvents != NIL
         oXML:addBelow( oEvents )
      ENDIF
   ENDIF

   IF oNode != NIL .AND. n > 0
      oNode:addBelow( oXML )
   ENDIF

RETURN oXML


//FUNCTION Xml2Prg( cFileXML )
//   LOCAL ooDoc := TXmlDocument():New( cFileXML )
//   oNode := oDoc:FindFirstRegEx( "Report" )

