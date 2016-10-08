/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Component.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

static lLoading := .F.
//-----------------------------------------------------------------------------------------------

CLASS Component INHERIT Object
   DATA Theming                EXPORTED
   DATA ImageList              EXPORTED
   DATA Exists                 EXPORTED INIT .T.
   DATA Events                 EXPORTED INIT {}
   DATA Button                 EXPORTED
   DATA EventHandler           EXPORTED
   DATA ComponentType          EXPORTED
   DATA lCreated               EXPORTED INIT .F.

   DATA __lCreateAfterChildren EXPORTED INIT .F.
   DATA __IdeImageIndex        EXPORTED INIT 7
   DATA __PropFilter           EXPORTED INIT {}
   DATA __lMoveable            EXPORTED INIT .F.
   DATA Caption                EXPORTED INIT ""
   DATA Text                   EXPORTED INIT ""
   DATA lComponent             EXPORTED INIT .T.
   DATA Left, Top

   ACCESS Form                 INLINE IIF( ::Owner != NIL, ::Owner:Form, NIL )
   ACCESS AppInstance          INLINE ::__GetInstance()

   ACCESS Parent               INLINE ::Owner

   METHOD Init()               CONSTRUCTOR
   METHOD Destroy()
   METHOD __SetCtrlName()
   METHOD __GetInstance()
   METHOD RenameComponents()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD __GetInstance() CLASS Component
   LOCAL hInstance
   IF ::Owner != NIL
      IF ::Form:DllInstance != NIL
         hInstance := ::Form:DllInstance
      ELSE
         hInstance := ::Application:Instance
      ENDIF
    ELSE
      hInstance := GetModuleHandle()
   ENDIF
RETURN hInstance

//-----------------------------------------------------------------------------------------------

METHOD Init( oOwner ) CLASS Component
   ::__IsControl  := .F.
   ::__lCopyCut   := .F.
   DEFAULT ::__xCtrlName TO "Component"
   DEFAULT ::ClsName     TO "Component"
   IF oOwner != NIL .AND. oOwner:Form != NIL .AND. !oOwner:Modal
      oOwner := oOwner:Form
   ENDIF
   ::Owner        := oOwner
   ::EventHandler := Hash()
   HSetCaseMatch( ::EventHandler, .F. )

   IF oOwner != NIL
      IF ::ClsName == "SqlConnector" .AND. LEN( oOwner:Components ) > 0
         AINS( oOwner:Components, 1, Self )
       ELSE
         AADD( oOwner:Components, Self )
      ENDIF
   ENDIF

   IF ::Owner != NIL .AND. ::Owner:DesignMode
      __SetInitialValues( Self )
   ENDIF

   ::__CreateProperty()

   IF ::Owner != NIL .AND. ::Owner:DesignMode
      IF oOwner:TreeItem == NIL
         ::Application:ObjectTree:Set( oOwner )
      ENDIF
      ::Application:ObjectTree:Set( Self )
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD Destroy() CLASS Component
   LOCAL n, cCtrl, oObj, cType := ::ComponentType
   IF ::Name != NIL
      TRY
         HDel( ::Form:__hObjects, ::xName )
      CATCH
      END

      IF ::Owner != NIL .AND. __ObjHasMsg( ::Owner, "Components" ) .AND. ( n := ASCAN( ::Owner:Components, {|o|o:Name==::Name} ) ) > 0
         ADEL( ::Owner:Components, n, .T. )
      ENDIF
      IF ::DesignMode
         // Clear referenced property
         FOR EACH cCtrl IN ::Form:__hObjects:Keys
             TRY
                oObj := ::Form:__hObjects[ cCtrl ]
                IF VALTYPE( oObj ) == "O"
                   IF __ObjHasMsg( oObj, cType ) .AND. VALTYPE( oObj:&cType ) == "O" .AND. oObj:&cType == Self
                      __objSendMsg( oObj, "_" + cType, NIL )
                   ENDIF
                ENDIF
             CATCH
             END
         NEXT

      ENDIF
      ::Owner := NIL
      RETURN .T.
   ENDIF
RETURN .F.

METHOD RenameComponents( oForm, cName, cOldName )
   LOCAL oWait, oObj, cCtrl, cType := ::ComponentType
   DEFAULT oForm TO ::Owner

      IF oForm:Cargo != NIL
         oWait := ::Application:MainForm:MessageWait( "Loading form " + oForm:Cargo )
         ::Application:Project:LoadForm( oForm:Cargo,,, .T., oForm )
         oForm:Cargo := NIL
         oWait:Close()
      ENDIF

      IF __ObjHasMsg( oForm, cType ) .AND. VALTYPE( oForm:&cType ) == "C" .AND. oForm:&cType == cOldName
         __objSendMsg( oForm, "_" + cType, cName )
      ENDIF
      FOR EACH cCtrl IN oForm:__hObjects:Keys
          oObj := oForm:__hObjects[ cCtrl ]
          IF ValType(oObj) == "O" .AND. __ObjHasMsg( oObj, cType ) .AND. VALTYPE( oObj:&cType ) == "C" .AND. oObj:&cType == cOldName
             __objSendMsg( oObj, "_" + cType, cName )
          ENDIF
      NEXT
RETURN .T.


METHOD __SetCtrlName( cName ) CLASS Component
   LOCAL n, cOldName := ::xName

   Super:__SetCtrlName( cName )

   IF ! ::Owner:__lLoading
      IF ::DesignMode .AND. ! Empty( cOldName ) .AND. UPPER(cName) != UPPER(cOldName)
         ::RenameComponents( ::Owner, cName, cOldName )

         IF ::Owner == ::Application:Project:Forms[1]
            FOR n := 2 TO LEN( ::Application:Project:Forms )
                ::RenameComponents( ::Application:Project:Forms[n], cName, cOldName )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   IF !EMPTY( ::Button )
      ::Button:Parent:Reset()
   ENDIF
RETURN cName
