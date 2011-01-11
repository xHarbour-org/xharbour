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
   DATA Parent                 EXPORTED

   DATA __lCreateAfterChildren EXPORTED INIT .F.
   DATA __IdeImageIndex        EXPORTED INIT 7
   DATA __PropFilter           EXPORTED INIT {}
   DATA Caption                EXPORTED INIT ""
   
   ACCESS Form                 INLINE IIF( ::Owner != NIL, ::Owner:Form, NIL )
   ACCESS AppInstance            INLINE IIF( ::Form:DllInstance != NIL, ::Form:DllInstance, ::Application:Instance )

   METHOD Init()               CONSTRUCTOR
   METHOD Destroy()
   METHOD __SetCtrlName()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oOwner ) CLASS Component
   LOCAL n, aProp, cProp, nPos
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
   
   ::__CreateProperty()

   IF ::Owner != NIL .AND. ::Owner:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::Application:ObjectTree:Set( Self )
   ENDIF
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD Destroy() CLASS Component
   LOCAL n, cCtrl, oObj, cType := ::ComponentType
   IF ::Name != NIL
      TRY
         HDel( ::Form:Property, ::xName )
      CATCH
      END

      IF ::Owner != NIL .AND. __ObjHasMsg( ::Owner, "Components" ) .AND. ( n := ASCAN( ::Owner:Components, {|o|o:Name==::Name} ) ) > 0
         ADEL( ::Owner:Components, n, .T. )
      ENDIF
      IF ::__ClassInst != NIL
         // Clear referenced property
         FOR EACH cCtrl IN ::Form:Property:Keys
             TRY
                oObj := ::Form:Property[ cCtrl ]
                IF VALTYPE( oObj ) == "O" 
                   IF __ObjHasMsg( oObj, cType ) .AND. VALTYPE( oObj:&cType ) == "O" .AND. oObj:&cType == Self
                      __objSendMsg( oObj, "_" + cType, NIL )
                   ENDIF
                ENDIF
             CATCH
             END
         NEXT
         
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

METHOD __SetCtrlName(c) CLASS Component
   LOCAL cType
   Super:__SetCtrlName(c)
   IF !EMPTY( ::Button )
      ::Button:Parent:Reset()
   ENDIF
RETURN c
