/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
* Under Harbour GPL license - see license.txt
*-----------------------------------------------------------------------------
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
//#include "windows.ch"

CLASS TComponent FROM TPersistent

    DATA ComObject          AS OBJECT  READONLY
	METHOD ComponentCount              INLINE Len( ::Components )
	DATA ComponentIndex     AS NUMERIC
	DATA Components         AS ARRAY   READONLY
	DATA ComponentState     AS NUMERIC READONLY
	DATA ComponentStyle     AS NUMERIC READONLY
	DATA DesignInfo         AS NUMERIC
	DATA Name               AS STRING  PUBLISHED
	DATA Owner              AS OBJECT  READONLY
	DATA Tag                           PUBLISHED
	DATA VCLComObject



    METHOD New()                  CONSTRUCTOR

    METHOD _AddRef                VIRTUAL   // PROTECTED
	METHOD _Release               VIRTUAL   // PROTECTED
	METHOD BeforeDestruction      VIRTUAL
	METHOD ChangeName             VIRTUAL   // PROTECTED
	METHOD Create                 VIRTUAL
	METHOD DefineProperties       VIRTUAL   // PROTECTED
	METHOD Destroy                VIRTUAL
	METHOD DestroyComponents      VIRTUAL
	METHOD Destroying             VIRTUAL
	METHOD ExecuteAction          VIRTUAL
	METHOD FindComponent          VIRTUAL
	METHOD FreeNotification       VIRTUAL
	METHOD FreeOnRelease          VIRTUAL
	METHOD GetChildOwner          VIRTUAL   // PROTECTED
	METHOD GetChildParent         VIRTUAL   // PROTECTED
	METHOD GetChildren            VIRTUAL   // PROTECTED
	METHOD GetIDsOfNames          VIRTUAL   // PROTECTED
	METHOD GetNamePath            VIRTUAL
	METHOD GetOwner               VIRTUAL   // PROTECTED
	METHOD GetParentComponent     VIRTUAL
	METHOD GetTypeInfo            VIRTUAL   // PROTECTED
	METHOD GetTypeInfoCount       VIRTUAL   // PROTECTED
	METHOD HasParent              VIRTUAL
	METHOD InsertComponent        VIRTUAL
	METHOD Invoke                 VIRTUAL   // PROTECTED
	METHOD IsImplementorOf        VIRTUAL
	METHOD Loaded                 VIRTUAL   // PROTECTED
	METHOD Notification           VIRTUAL   // PROTECTED
	METHOD PaletteCreated         VIRTUAL   // PROTECTED
    METHOD QueryInterface         VIRTUAL   // PROTECTED
	METHOD ReadState              VIRTUAL   // PROTECTED
	METHOD ReferenceInterface     VIRTUAL
	METHOD RemoveComponent        VIRTUAL
	METHOD RemoveFreeNotification VIRTUAL
	METHOD SafeCallException      VIRTUAL
	METHOD SetAncestor            VIRTUAL   // PROTECTED
	METHOD SetChildOrder          VIRTUAL   // PROTECTED
	METHOD SetDesigning           VIRTUAL   // PROTECTED
	METHOD SetDesignInstance      VIRTUAL   // PROTECTED
	METHOD SetInline              VIRTUAL   // PROTECTED
	METHOD SetName                VIRTUAL   // PROTECTED
	METHOD SetParentComponent     VIRTUAL   // PROTECTED
	METHOD SetSubComponent        VIRTUAL
	METHOD UpdateAction           VIRTUAL
	METHOD Updated                VIRTUAL   // PROTECTED
	METHOD UpdateRegistry         VIRTUAL   // PROTECTED
	METHOD Updating               VIRTUAL   // PROTECTED
	METHOD ValidateContainer      VIRTUAL   // PROTECTED
	METHOD ValidateInsert         VIRTUAL   // PROTECTED
	METHOD ValidateRename         VIRTUAL   // PROTECTED
	METHOD WriteState             VIRTUAL   // PROTECTED

ENDCLASS

METHOD New() CLASS TPersistent
RETURN Self

