/*
 * $Id: TObject.prg,v 1.3 2002/10/16 21:17:00 fsgiudice Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TObject CLASS
 *
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
 *
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

/*
* Base Class - All windows and controls derive from this class
* NOT USE THIS CLASS DIRECTLY !!!
*/

#include "winuser.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

CLASS TObject  // FROM HBObject

    DATA cargo                          // cargo as in Clipper

    METHOD New() CONSTRUCTOR

    /*
    // This data and methods came from standard [x]Harbour object class

    DATA    ClassName               // Class Name
    DATA    ClassH                  // Class
    DATA    ClassSel

    METHOD New()                   INLINE Self
    METHOD Init()                  INLINE Self

    METHOD GetMessageList( lDataMethod, nClassType ) ;
                                   INLINE __objGetMsgList( Self, lDataMethod, nClassType )
    METHOD GetMethodList()         INLINE __objGetMethodList( Self )
    METHOD GetValueList( aExcept ) INLINE __objGetValueList( Self, aExcept )
    METHOD SetValueList( aData )   INLINE __ObjSetValueList( Self, aData )
    METHOD AddMethod( cSymbol, nFuncPtr ) ;
                                   INLINE __objAddMethod( Self, cSymbol, nFuncPtr )
    METHOD AddInline( cSymbol, bInline );
                                   INLINE __objAddInline( Self, cSymbol, bInline )
    METHOD AddData( cSymbol)       INLINE __objAddData( Self, cSymbol )
    METHOD ModifyMethod( cSymbol, nFuncPtr ) ;
                                   INLINE __objModMethod( Self, cSymbol, nFuncPtr )
    METHOD ModifyInline( cSymbol, bInline ) ;
                                   INLINE __objModInline( Self, cSymbol, bInline )
    METHOD DeleteMethod( cSymbol ) INLINE __objDelMethod( Self, cSymbol )
    METHOD DeleteInline( cSymbol ) INLINE __objDelInline( Self, cSymbol )
    METHOD DeleteData( cSymbol )   INLINE __objDelData( Self, cSymbol )
    METHOD IsDerivedFrom( cGrandFather AS STRING ) ;
                                    INLINE __objDerivedFrom( Self, Upper( cGrandFather ) )
    */

    // FSG - as Delphi
    METHOD AfterConstruction()           VIRTUAL
   METHOD BeforeDestruction()           VIRTUAL
   METHOD ClassInfo()                   VIRTUAL  // FSG - to be implemented
   //METHOD ClassName                            // FSG - already have from harbour
   METHOD ClassNameIs( cName )          INLINE  Upper( cName ) == ::ClassName
   METHOD ClassParent()                 INLINE  ::Super
   METHOD ClassType                     VIRTUAL  // FSG - to be implemented
   METHOD CleanupInstance               VIRTUAL  // FSG - to be implemented
   METHOD Create                        VIRTUAL  // FSG - to be implemented
   METHOD DefaultHandler                VIRTUAL
   METHOD Destroy                       VIRTUAL
   METHOD Dispatch                      VIRTUAL
   METHOD FieldAddress                  VIRTUAL  // FSG - to be implemented
   METHOD Free                          INLINE  IIF( Self <> NIL, ::Destroy(), NIL ), Self := NIL
   METHOD FreeInstance                  VIRTUAL
   METHOD GetInterface                  VIRTUAL  // FSG - to be implemented
   METHOD GetInterfaceEntry             VIRTUAL  // FSG - to be implemented
    METHOD GetInterfaceTable             VIRTUAL  // FSG - to be implemented
   METHOD InheritsFrom( cGrandFather )  INLINE  ::IsDerivedFrom( cGrandFather AS STRING )
   METHOD InitInstance                  VIRTUAL  // FSG - to be implemented
   METHOD InstanceSize                  VIRTUAL  // FSG - to be implemented
   METHOD MethodAddress                 VIRTUAL  // FSG - to be implemented
   METHOD MethodName                    VIRTUAL  // FSG - to be implemented
   METHOD NewInstance                   VIRTUAL
   METHOD SafeCallException             VIRTUAL  // FSG - to be implemented


    // Mine
    //METHOD DisplayData()
    //METHOD DisplayMethods()
    //METHOD DisplayArray()

ENDCLASS

METHOD New() CLASS TObject

RETURN Self

// ---------------------------------------------------------------------------------------

//METHOD DisplayArray( aData AS ARRAY, lMessageBox AS LOGICAL ) CLASS TObject
//   local cString := "", i
//
//      DEFAULT lMessageBox TO TRUE
//
//      FOR i = 1 to len ( aData )
//          cString += Str( i ) + " - " + cStr( aData[i] )
//          cString += CRLF
//      NEXT
//
//   IF lMessageBox
//      MessageBox(, cString, "Array Data" )
//   ENDIF
//
//RETURN cString
//
//METHOD DisplayData( cText AS STRING, lMessageBox AS LOGICAL ) CLASS TObject
//   local cString := "", i, aData
//   local oB := Self
//
//      DEFAULT cText       TO ""
//      DEFAULT lMessageBox TO TRUE
//
//      cString += "Object Name: " + oB:ClassName + CRLF
//
//      aData := __objGetValueList( oB )
//      FOR i = 1 to len ( aData )
//          cString += "DATA name: " + Pad( aData[ i, HB_OO_DATA_SYMBOL ], 25 )
//          cString += " - type: " + ValType( aData[ i, HB_OO_DATA_VALUE  ] )
//          cString += " - value: " + Pad( cStr( aData[ i, HB_OO_DATA_VALUE  ] ), 30 )
//          cString += CRLF
//      NEXT
//
//   IF lMessageBox
//      MessageBox(, cString, "Object Data" + ;
//                            IIF( Len( cText ) > 0, " - " + cText, "" ) )
//   ENDIF
//
//RETURN cString
//
//METHOD DisplayMethods( cText AS STRING, lMessageBox AS LOGICAL ) CLASS TObject
//   local cString := "", i, aData
//   local oB := Self
//
//      DEFAULT cText       TO ""
//      DEFAULT lMessageBox TO TRUE
//
//      cString += "Object Name: " + oB:ClassName + CRLF
//
//      aData := __objGetMethodList( oB )
//      FOR i = 1 to len ( aData )
//          cString += "Method name: " + Pad( aData[ i ], 25 )
//          //cString += " - type: " + ValType( aData[ i, HB_OO_DATA_VALUE  ] )
//          //cString += " - value: " + Pad( cStr( aData[ i, HB_OO_DATA_VALUE  ] ), 30 )
//          cString += CRLF
//      NEXT
//
//   IF lMessageBox
//      MessageBox(, cString, "Object Methods" + ;
//                            IIF( Len( cText ) > 0, " - " + cText, "" ) )
//   ENDIF
//
//RETURN cString
//
