
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TComponent CLASS
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

CLASS TComponent FROM TPersistent

    DATA ComObject          AS OBJECT  READONLY
	METHOD ComponentCount              INLINE Len( ::Components )
	DATA ComponentIndex     AS NUMERIC
	DATA Components         AS ARRAY   READONLY
	DATA ComponentState     AS NUMERIC READONLY
	DATA ComponentStyle     AS NUMERIC READONLY
	DATA DesignInfo         AS NUMERIC
	DATA Name               AS STRING  EXPORTED
	DATA Owner              AS OBJECT  READONLY
	DATA Tag                           EXPORTED
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

METHOD New() CLASS TComponent
RETURN Self

