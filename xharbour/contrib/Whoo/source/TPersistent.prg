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

CLASS TPersistent FROM TObject

    METHOD New() CONSTRUCTOR

    METHOD Assign                   VIRTUAL
	METHOD AssignTo                 VIRTUAL   // PROTECTED
	METHOD DefineProperties         VIRTUAL   // PROTECTED
	METHOD Destroy                  VIRTUAL
	METHOD GetNamePath              VIRTUAL
	METHOD GetOwner                 INLINE NIL PROTECTED

ENDCLASS

METHOD New() CLASS TPersistent
RETURN Self

