/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Class commands
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 ( ->07/2000 ) JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    Support for Class(y), TopClass and Visual Object compatibility
 *    Support for MI (multiple inheritance),

 * Copyright 2000-2001 ( 08/2000-> ) JF. Lefebvre <jfl@mafact.com>
 *    Scoping (Protect, Hidden and Readonly),
 *    Delegating, DATA Shared
 *    Support of 10 Chars limits
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_CLASS_CH_
#define HB_CLASS_CH_

#include "hbsetup.ch"
#include "hboo.ch"

/* You can actually define one or all the syntax, they do not collide each other */
/* There is some difference with their original form and I hope I will have enough */
/* time to document it <g> */
/* This is work in progress ... */
/* FWOBJECT AND CLASSY compatibility are the base of this work */
/* VO is just here as I like it's way of */
/* instantiating object but there is only a very few VO keywords here :-( */
/* TOPCLASS is better implemented because I like the way some Classy command */
/* are simplified */
/* There is also a big common block extending in fact each of the four base syntax */
/* it seem actually impossible to completely separate it without creating */
/* four different include files (what I would not see in fact ) */

/* There is also two compatibility define you can use */
/* HB_CLS_NOTOBJECT which IF DEFINED, disable the auto inherit of HBObject */
/* (which in fact also disable the classy compatibility :new(...) => :Init(...)  */
/* HB_CLS_NOAUTOINIT which disable the (VO like) AutoInit for Logical and Numeric */
/* when not specifically initiated */
/* These two are disabled by default */
/* So Each class _inherit_ of HBObject by default and */
/*    Each type logical or numerical is initiated to .F. and 0 by default */

/* #define HB_CLS_NOTOBJECT  */ /* Should be included in some compatibility include files as needed */
/* #define HB_CLS_NOAUTOINIT */ /* Idem */
/* #define HB_CLS_ALLOWCLASS */ /* Work in progress, don't define it now */
/* #define HB_CLS_ENFORCERO FLAG to disable Write access to RO VAR outside */
/*         of Constructors /!\ Could be related to some incompatibility */

// Harbour
#xtranslate AS INTEGER => AS NUMERIC

#ifndef HB_CONSTRUCTOR_NO_DIVERT
  #define HB_CONSTRUCTOR_USE_DIVERT
#endif

#ifdef HB_CONSTRUCTOR_USE_DIVERT
   DYNAMIC DivertConstructorCall
#endif

DECLARE HBClass ;
        New( cName AS String, OPTIONAL SuperParams ) AS CLASS HBClass ;
        Create() AS Object ;
        Instance() AS Object ;
        AddClsMethod( cName AS String, @MethodName(), nScope AS Numeric, n2 AS Numeric, n3 AS Numeric ) ;
        AddDelegate( cName AS String, cDelegate AS String, cObject AS String, nScope AS Numeric, lPersistent AS LOGICAL ) ;
        AddMultiClsData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String ) ;
        AddMultiData( cType AS String, uVal, nScope AS Numeric, aDatas AS Array OF String, x AS LOGICAL, lPer AS LOGICAL ) ;
        AddMethod( cName AS String, @MethodName(), nScope AS Numeric, lPersistent AS LOGICAL ) ;
        AddInLine( cName AS String, bBlock AS CodeBlock, nScope AS Numeric, lPersistent AS LOGICAL ) ;
        AddVirtual( cName AS String ) ;
        ModMethod( cName AS String, @MethodName(), nScope AS Numeric, lPersistent AS LOGICAL ) ;
        ModClsMethod( cName AS String, @MethodName(), nScope AS Numeric ) ;
        ModInline( cName AS String, bBlock AS CodeBlock, nScope AS Numeric, lPersistent AS LOGICAL ) ;
        SetOnError( @MethodName() )

#xtranslate __ERR([<msg,...>]) => #error [<msg>]

#xtranslate _InheritFrom_( <cSuperClasses,...> ) => {<cSuperClasses>}

#ifdef HB_CLS_NOTOBJECT
   #xtranslate _InheritFrom_() => nil
#else
   #xtranslate _InheritFrom_() => { HBObject():Classh }
#endif

//----------------------------------------------------------------------------//
// Traductores Genericos de <Func>[( [ <parms,...>] )]

#xtranslate _AsFunc_( <itm> )                   => <itm>()
#xtranslate _AsFunc_( <itm>( [<prm,...>] ) )    => <itm>( [<prm>] )

#xtranslate _AsName_( <itm> )                   => <itm>
#xtranslate _AsName_( <itm>( [<prm,...>] ) )    => <itm>

#xtranslate _AsStr_( <itm> )                    => <(itm)>
#xtranslate _AsStr_( <itm>( [<prm,...>] ) )     => #<itm>
#xtranslate _AsStrLst_( <Typ> [, <TypN> ] )     =>;
                            _AsStr_( <Typ> )[ , _AsStr_( <TypN> ) ]

#xtranslate _AsNameFrom_( <itm> ) => _AsName_( <itm> )
#xtranslate _AsNameFrom_( <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> ) ;
            =>;
            _<type>


//----------------------------------------------------------------------------//

#xtranslate __OPT__( <a>, <b> )  => <a>
#xtranslate __OPT__( <a> )       => <a>

//----------------------------------------------------------------------------//

#ifdef HB_CLS_NOAUTOINIT
 #define __HB_CLS_NOINI .T.
#else
 #define __HB_CLS_NOINI .F.
#endif

#ifndef HB_CLS_FWO
#ifndef HB_CLS_CSY
#ifndef HB_CLS_VO
#ifndef HB_CLS_TOP
#ifndef HB_CLS_XB

/* IF NOTHING DECIDED BY THE PROGRAMMER USE ALL */
#define HB_CLS_FWO
#define HB_CLS_CSY
#define HB_CLS_VO
#define HB_CLS_TOP
#define HB_CLS_XB

#endif
#endif
#endif
#endif
#endif

//#xtranslate HBCLSCHOICE( <publish> <export>, <protect>, <hidde> ) => IIF( <export>, HB_OO_CLSTP_EXPORTED , IIF( <export>, HB_OO_CLSTP_EXPORTED , IIF( <protect>, HB_OO_CLSTP_PROTECTED, IIF( <hidde>, HB_OO_CLSTP_HIDDEN, nScope) ) ) )
//#xtranslate HBCLSCHOICE( <x,...> ) => ;__ERR( Can not use multiple scope qualifiers! );;
#xtranslate HBCLSCHOICE( <x,...> ) => ) ;__ERR( Can not use multiple scope qualifiers! );#line

#xtranslate HBCLSCHOICE( .T., .F., .F., .F. ) => HB_OO_CLSTP_PUBLISHED
#xtranslate HBCLSCHOICE( .F., .F., .T., .F. ) => HB_OO_CLSTP_PROTECTED
#xtranslate HBCLSCHOICE( .F., .F., .F., .T. ) => HB_OO_CLSTP_HIDDEN
#xtranslate HBCLSCHOICE( .F., .T., .F., .F. ) => HB_OO_CLSTP_EXPORTED
#xtranslate HBCLSCHOICE( .F., .F., .F., .F. ) => nScope  // Default

/* CLASSY SYNTAX */
#ifdef HB_CLS_CSY
#xtranslate CREATE CLASS => CLASS
#xtranslate _HB_MEMBER {AS Num  => _HB_MEMBER {AS Numeric
#xtranslate _HB_MEMBER {AS Char => _HB_MEMBER {AS Character
#endif

// Extend Classes
#xcommand OVERRIDE METHOD <!Message!> [IN] CLASS <!Class!> WITH [METHOD] <!Method!> [SCOPE <Scope>] => ;
  <Class>(); __clsModMsg( __ClsGetHandleFromName( #<Class> ), #<Message>, @<Method>(), IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ) )

#xcommand EXTEND CLASS <!Class!> WITH <data: DATA, VAR> <Data> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  <Class>(); __clsAddMsg( __ClsGetHandleFromName( #<Class> ), <(Data)>, __cls_IncData( __ClsGetHandleFromName( #<Class> ) ), HB_OO_MSG_PROPERTY, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <!Class!> WITH METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  <Class>(); __clsAddMsg( __ClsGetHandleFromName( #<Class> ), #<Method>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <!Class!> WITH MESSAGE <Message> METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  <Class>(); __clsAddMsg( __ClsGetHandleFromName( #<Class> ), <(Message)>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <!Class!> WITH MESSAGE <Message> INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  <Class>(); __clsAddMsg( __ClsGetHandleFromName( #<Class> ), <(Message)>, {|Self| Self, <code> }, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <!Class!> WITH MESSAGE <Message>( <params,...> ) INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  <Class>(); __clsAddMsg( __ClsGetHandleFromName( #<Class> ), <(Message)>, {|Self, <params>| Self, <code> }, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

// EXTEND native type classes.
#xcommand OVERRIDE METHOD <!Message!> [IN] CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH [METHOD] <!Method!> [SCOPE <Scope>] => ;
  _<type>(); __clsModMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, @<Method>(), IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ) )

#xcommand EXTEND CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH <data: DATA, VAR> <Data> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  _<type>(); __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Data)>, __cls_IncData( __ClsGetHandleFromName( #<type> ) ), HB_OO_MSG_PROPERTY, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  _<type>(); __clsAddMsg( __ClsGetHandleFromName( #<type> ), #<Method>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH MESSAGE <Message> METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  _<type>(); __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER,  HASH> WITH MESSAGE <Message> INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  _<type>(); __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, {|Self| Self, <code> }, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH MESSAGE <Message>( <params,...> ) INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  _<type>(); __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, {|Self, <params>| Self, <code> }, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

// Extend native type (NOT using standard classes)
#xcommand EXTEND [TYPE] <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  __clsAddMsg( __ClsGetHandleFromName( #<type> ), #<Method>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND [TYPE] <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH MESSAGE <Message> METHOD <!Method!> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, @<Method>(), HB_OO_MSG_METHOD, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND [TYPE] <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH MESSAGE <Message> INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, {|Self| Self, <code>}, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

#xcommand EXTEND [TYPE] <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> WITH MESSAGE <Message>(<params,...>) INLINE <code,...> [SCOPE <Scope>] [<Persistent: PERSISTENT> ] [<Case: NOUPPER>] => ;
  __clsAddMsg( __ClsGetHandleFromName( #<type> ), <(Message)>, {|Self, <params>| <code>}, HB_OO_MSG_INLINE, NIL, IIF( <.Scope.>, <Scope>, HB_OO_CLSTP_EXPORTED ), <.Persistent.>, <.Case.> )

// ENABLE
#xcommand ENABLE TYPE CLASS <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> [, <typeN: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH>] => _<type>() [;_<typeN>()]
#xcommand ENABLE TYPE CLASS ALL => _Array(); _Block(); _Character(); _Date(); _Logical(); _Nil(); _Numeric(); _Pointer(); _Hash()

#xcommand ASSOCIATE CLASS <ClassName> WITH TYPE <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> => ;
  __clsAssocType( IIF( __ClsGetHandleFromName( <(ClassName)> ) == 0, <ClassName>():ClassH, __ClsGetHandleFromName( <(ClassName)> ) ), #<type> )

#xcommand EXTERNAL <type: ARRAY, BLOCK, CHARACTER, DATE, LOGICAL, NIL, NUMERIC, POINTER, HASH> [, <*rest*>] => _<type>() [; EXTERNAL <rest>]

#ifdef HB_CLS_ALLOWCLASS  /* DONT DECLARE IT ! WORK IN PROGRESS !!! */

#ifndef HB_SHORTNAMES
/*
#xtranslate DECLMETH <ClassName> <MethodName> => <ClassName>_<MethodName>

#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   _HB_CLASS <ClassName> ;;
   UTILITY <static> FUNCTION <ClassName>(...) ;;
      static s_oClass ;;
      local MetaClass,nScope ;;
      [ REQUEST <SuperClass1> ] [ ,<SuperClassN> ] ;;
      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope ) ;;
      if s_oClass == NIL ;;
         s_oClass  := IIF(<.metaClass.>, <(metaClass)>, HBClass():new( <(ClassName)> , __HB_CLS_PAR ( [ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ) ;;
         if ! <.metaClass.> ;;
          Metaclass := HBClass():new( <(ClassName)>+" class", __HB_CLS_PAR0 ( [ <SuperClass1>():class ] [ ,<SuperClassN>():class ] ) )  ;;
         end              ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #untranslate CLSMETH ;;
     #untranslate DECLCLASS ;;
     #xtranslate CLSMETH <ClassName> \<MethodName> => @<ClassName>_\<MethodName> ;;
     #xtranslate DECLCLASS <ClassName> => <ClassName> ;
     ; #xuntranslate Super() : ;
     ; #xuntranslate Super : ;
     ; #xuntranslate : Super : ;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ;
     ; #translate Super() : => ::<SuperClass1>: ;
     ; #translate Super : => ::<SuperClass1>: ;
     ; #translate : Super : => :<SuperClass1>:
*/
#else
/*
#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] => ;
   _HB_CLASS <ClassName> ;;
   UTILITY <static> FUNCTION <ClassName>(...) ;;
      static s_oClass  ;;
      local MetaClass,nScope ;;
      [ REQUEST <SuperClass1> ] [ ,<SuperClassN> ] ;;
      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope ) ;;
      if s_oClass == NIL ;;
         s_oClass  := IIF(<.metaClass.>, <(metaClass)>, HBClass():new( <(ClassName)> , __HB_CLS_PAR ( [ <(SuperClass1)> ] [ ,<(SuperClassN)> ] ) ) ) ;;
         if ! <.metaClass.> ;;
          Metaclass := HBClass():new( <(ClassName)>+" class", __HB_CLS_PAR0 ( [ <SuperClass1>():class ] [ ,<SuperClassN>():class ] ) )  ;;
         end              ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #untranslate CLSMETH ;;
     #translate CLSMETH <ClassName> \<MethodName>() => @\<MethodName> ;
     ; #xuntranslate Super() : ;
     ; #xuntranslate Super : ;
     ; #xuntranslate : Super : ;
     [ ; #translate Super( <SuperClassN> ) : => ::<SuperClassN>: ] ;
     ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ;
     ; #translate Super() : => ::<SuperClass1>: ;
     ; #translate Super : => ::<SuperClass1>: ;
     ; #translate : Super : => :<SuperClass1>:
*/
#endif /* HB_SHORTNAMES */

#else

#ifndef HB_SHORTNAMES

#xtranslate DECLMETH <ClassName> <MethodName> => <ClassName>_<MethodName>

#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN> ] ] [<static: STATIC>] [ FUNCTION <FuncName> ] [ IMPLEMENTS NAMESPACE <ns> ] => ;
   _HB_CLASS <ClassName> ;;
   UTILITY <static> FUNCTION __OPT__( [ _AsName_( <FuncName> ), ] _AsName_( <ClassName> ))(...) [ IMPLEMENTS NAMESPACE <ns> ];;
      static s_oClass ;;
      local oClassInstance ;;
      local nScope ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
      local __lInactive := s_oClass != NIL .and. !__clsIsActive(s_oClass:hClass) ;;
   #else ;;
      #undef __lInactive ;;
      #define __lInactive .F. ;;
   #endif ;;
      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope ) ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
      if s_oClass == NIL .or. __lInactive ;;
   #endif ;;
         if s_oClass == NIL ;;
            s_oClass  := IIF(<.metaClass.>, <(metaClass)>, HBClass():New( _AsStr_( <ClassName> ) , _InheritFrom_( [ _AsNameFrom_( <SuperClass1> )():classh ] [ , _AsNameFrom_( <SuperClassN> )():classh ] ) ) ) ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
         endif ;;
   #endif ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName>;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #untranslate CLSMETH ;;
     #untranslate DECLCLASS ;;
     #xtranslate CLSMETH <ClassName> \<MethodName> => @<ClassName>_\<MethodName> ;;
     #xtranslate DECLCLASS <ClassName> => <ClassName> ;
     ;DECLSUPER <SuperClass1> [, <SuperClassN> ]


#else

#xcommand CLASS <ClassName> [METACLASS <metaClass>] [ <frm: FROM, INHERIT> <SuperClass1> [,<SuperClassN>] ] [<static: STATIC>] [ FUNCTION <FuncName> ] [ IMPLEMENTS NAMESPACE <ns> ] => ;
   _HB_CLASS <ClassName> ;;
   UTILITY <static> FUNCTION __OPT__( [ _AsName_( <FuncName> ), ] _AsName_( <ClassName> ))(...) [ IMPLEMENTS NAMESPACE <ns> ] ;;
      static s_oClass  ;;
      local oClassInstance ;;
      local nScope ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
      local __lInactive := s_oClass != NIL .and. !__clsIsActive(s_oClass:hClass) ;;
   #else ;;
      #undef __lInactive ;;
      #define __lInactive .F. ;;
   #endif ;;
      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope ) ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
      if s_oClass == NIL .or. __lInactive ;;
   #endif ;;
         if s_oClass == NIL ;;
            s_oClass  := IIF(<.metaClass.>, <(metaClass)>, HBClass():New( _AsStr_( <ClassName> ) , _InheritFrom_( [ _AsNameFrom_( <SuperClass1> )():classh ] [ , _AsNameFrom_( <SuperClassN> )():classh ] ) ) ) ;;
   #if defined( __HRB__ ) || defined( __EXPORT__ ) ;;
         endif ;;
   #endif ;;
     #undef  _CLASS_NAME_ ;;
     #define _CLASS_NAME_ <ClassName> ;;
     #undef  _CLASS_MODE_ ;;
     #define _CLASS_MODE_ _CLASS_DECLARATION_ ;;
     #untranslate CLSMETH ;;
     #translate CLSMETH <ClassName> \<MethodName>() => @\<MethodName> ;
     ;DECLSUPER <SuperClass1> [, <SuperClassN> ]

#endif /* HB_SHORTNAMES */

#xcommand  DECLSUPER =>

#xcommand  DECLSUPER <SuperClass1> [, <SuperClassN> ] ;
      =>;
     ; #xuntranslate Super() : ;
     ; #xuntranslate Super : ;
     ; #xuntranslate : Super : ;
     ;;
     [ ; DECLSUPERN <SuperClassN> ] ;
     ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ;
     ; #translate Super() : => ::<SuperClass1>: ;
     ; #translate Super : => ::<SuperClass1>: ;
     ; #translate : Super : => :<SuperClass1>:

#xcommand  DECLSUPER <Func>( <SuperClass1> ) [, <SuperClassN> ] ;
      =>;
     ; #xuntranslate Super() : ;
     ; #xuntranslate Super : ;
     ; #xuntranslate : Super : ;
     ;;
     [ ; DECLSUPERN <SuperClassN> ] ;
     ; #translate Super( <SuperClass1> ) : => ::<SuperClass1>: ;
     ; #translate Super() : => ::<SuperClass1>: ;
     ; #translate Super : => ::<SuperClass1>: ;
     ; #translate : Super : => :<SuperClass1>:

#xcommand DECLSUPERN <SuperClass> ;
      =>;
      #translate Super( <SuperClass> ) : => ::<SuperClass>:

#xcommand DECLSUPERN <Func>( <SuperClass> ) ;
      =>;
      #translate Super( <SuperClass> ) : => ::<SuperClass>:

/* Disable the message :Class */
/* CLASSY SYNTAX */
#ifdef HB_CLS_CSY
#xtranslate  :CLASS  =>
#xtranslate  :CLASS: => :
#endif

#endif /* HB_CLS_ALLOWCLASS */


/* CLASSY SYNTAX */
#IFDEF HB_CLS_CSY

// VAR ...
#xcommand VAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ) , )

// VAR ...
#xcommand VAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ) , )

// VAR ... IN ...
#xcommand VAR <DataName> [ AS <type> ] IN <SuperClass> [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <DataName> ),, <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), ) ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( _<DataName> ),, <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), )

// VAR ... IS ... IN ...
#xcommand VAR <DataName> [ AS <type> ] IS <SprDataName> IN <SuperClass> [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <DataName> ), _AsStr_( <SprDataName> ), <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), ) ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( _<DataName> ), _AsStr_( _<SprDataName> ), <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), )

// VAR ... IS ...
#xcommand VAR <DataName> [ AS <type> ] IS <SprDataName> [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <DataName> ), _AsStr_( <SprDataName> ),, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), ) ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( _<DataName> ), _AsStr_( _<SprDataName> ),, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), )

// VAR ... TO ...
#xcommand VAR <DataName> [ AS <type> ] TO <SuperClass> [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <DataName> ),, <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), ) ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( _<DataName> ),, <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), )

// VAR ... IS ... TO ...
#xcommand VAR <DataName> [ AS <type> ] IS <SprDataName> TO <SuperClass> [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataName>} ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <DataName> ), _AsStr_( <SprDataName> ), <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), ) ;;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( _<DataName> ), _AsStr_( _<SprDataName> ), <(SuperClass)>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), <.persistent.> ), )

#xcommand CLASS VAR <*rest*> => CLASSVAR <rest>
#xcommand CLASS METHOD <*rest*> => CLASSMETHOD <rest>

#xcommand METHOD <MethodName> [ AS <type> ] DEFERRED => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddVirtual( _AsStr_( <MethodName> ) ), )

#endif /* HB_CLS_CSY */


/* VO SYNTAX */
#ifdef HB_CLS_VO

#xtranslate  ( <!name!>{ [<p,...>] }        =>  ( <name>():New( <p> )
#xtranslate  = <!name!>{ [<p,...>] }        =>  = <name>():New( <p> )
#xtranslate  , <!name!>{ [<p,...>] }        =>  , <name>():New( <p> )
#xtranslate  := <!name!>{ [<p,...>] }       =>  := <name>():New( <p> )

#xcommand EXPORT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#xcommand EXPORT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_EXPORTED + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#xcommand PROTECT <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#xcommand PROTECT <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_PROTECTED + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#xcommand HIDDEN <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#xcommand HIDDEN <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HB_OO_CLSTP_HIDDEN + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;

#ENDIF /* HB_CLS_VO */


#xcommand CLASSVAR <DataNames,...> [ TYPE <type> ] [ ASSIGN <uValue> ] [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI ), ) ;

#xcommand CLASSVAR <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI ), ) ;


/* FWOBJECT SYNTAX */
#ifdef HB_CLS_FWO

#xcommand DATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<publish: PUBLISHED>] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] ;
   [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiData( <(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ), { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI, <.persistent.> ), ) ;


#xcommand CLASSDATA <DataNames,...> [ AS <type> ] [ INIT <uValue> ] [<publish: PUBLISHED>] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ro: READONLY, RO>] [<share: SHARED>] => ;
   _HB_MEMBER {[AS <type>] <DataNames>} ;;
   IIF( !__lInactive, s_oClass:AddMultiClsData(<(type)>, <uValue>, HBCLSCHOICE( <.publish.>, <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + HB_OO_CLSTP_SHARED, { _AsStrLst_( <DataNames> ) }, __HB_CLS_NOINI ), ) ;

#endif /* HB_CLS_FWO */


/* XBASE SYNTAX */
#ifdef HB_CLS_XB

#xcommand SYNC METHOD <Name> [<*x*>] => METHOD <Name> SYNC [<x>]

#endif

#xcommand CLASSMETHOD <MethodName> [ <clsctor: CONSTRUCTOR> ] [ AS <type> ] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<share: SHARED>] => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) ) ), ;
   s_oClass:AddClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) ) );;
   #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>])

#xcommand CONSTRUCTOR <Name> [<*x*>] => METHOD <Name> CONSTRUCTOR [<x>]
#xcommand CONSTRUCTOR <Name> INLINE <Code,...> [<*x*>] => METHOD <Name> INLINE <Code> CONSTRUCTOR [<x>]

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ),;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ));;
    #xcommand METHOD <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
    #xcommand PROCEDURE <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>;;
    #xcommand METHOD <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>;;
    #xcommand PROCEDURE <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>

  #xcommand CLASSMETHOD <MethodName> [ <clsctod: CONSTRUCTOR> ] [ AS <type> ] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<share: SHARED>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctod.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ), ;
    s_oClass:AddClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctod.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ));;
    #xcommand METHOD <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
    #xcommand PROCEDURE <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>;;
    #xcommand METHOD <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>;;
    #xcommand PROCEDURE <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>
#else
  #xcommand METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ),;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ));;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>(\[ \<anyParams>])

  #xcommand METHOD <MethodName>([<params,...>]) [ <ctor: CONSTRUCTOR> ] [ AS <type> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [_CLASS_DECLARATION_] ;
    [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER <MethodName>([ <params>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ),;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ));;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>(\[ \<anyParams>])

  #xcommand CLASSMETHOD <MethodName> [ <clsctor: CONSTRUCTOR> ] [ AS <type> ] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<share: SHARED>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ), ;
    s_oClass:AddClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ));;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>(\[ \<anyParams>])

  #xcommand CLASSMETHOD <MethodName>([<params,...>]) [ <clsctor: CONSTRUCTOR> ] [ AS <type> ] [<export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<share: SHARED>] [<sync: SYNC>] [_CLASS_DECLARATION_] => ;
    _HB_MEMBER <MethodName>([ <params>]) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ), ;
    s_oClass:AddClsMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.share.>, HB_OO_CLSTP_SHARED, 0 ) + IIF( <.clsctor.>, HB_OO_CLSTP_CLASSCTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[ \<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[ \<anyParams>]);;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>(\[ \<anyParams>])

#endif /* STRICT_OO */

// METHOD ... BLOCK ...
#xcommand METHOD <MethodName> [ AS <type> ] BLOCK <CodeBlock> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] ;
   [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModInline( _AsStr_( <MethodName> ), <CodeBlock>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ),;
   s_oClass:AddInline( _AsStr_( <MethodName> ), <CodeBlock>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ) )

// METHOD ... EXTERN ...
#xcommand METHOD <MethodName> [ AS <type> ] EXTERN <FuncName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] ;
   [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), @_AsName_( <FuncName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ),;
   s_oClass:AddMethod( _AsStr_( <MethodName> ), @_AsName_( <FuncName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ) )

// METHOD ... INLINE LOCAL ...
#xcommand METHOD <MethodName> [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName> [ AS <type> ] BLOCK {|Self [,<v>] | Self, <Code> } [<other>]

// METHOD ...( <,...> ) INLINE LOCAL ...
/* Must have secondary version with params becuase params are used in the block */
#xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] INLINE [Local <v>,] <Code,...> [<other>] => ;
          METHOD <MethodName>( [<params>] ) [ AS <type> ] BLOCK {|Self [,<params>] [,<v>] | Self, <Code> } [<other>]

// METHOD ... INLINE ...
#xcommand METHOD <MethodName> [ AS <type> ] INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] ;
   [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModInline( _AsStr_( <MethodName> ), {|Self | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ), ;
   s_oClass:AddInline( _AsStr_( <MethodName> ), {|Self | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ) )

// METHOD ...( <,...> ) INLINE ...
/* Must have secondary version with params because params are used in the block */
#xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] INLINE <Code,...> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] ;
   [<persistent: PERSISTENT, PROPERTY>] [<ov: OVERRIDE>] => ;
   _HB_MEMBER <MethodName>([<params>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModInline( _AsStr_( <MethodName> ), {|Self [,<params>] | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ), ;
   s_oClass:AddInline( _AsStr_( <MethodName> ), {|Self [,<params>] | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), <.persistent.> ) )


#XCOMMAND INLINE METHOD <!Method!>[()] => WITH OBJECT \<|Self|; #undef __METHOD__; #define __METHOD__ <Method>
#XCOMMAND INLINE METHOD <!Method!>( <params,...> ) => WITH OBJECT \<|Self, <params>|; #undef __METHOD__; #define __METHOD__ <Method>
#XCOMMAND ENDMETHOD => \>; METHOD __METHOD__ BLOCK HB_QWith(); END

#xcommand METHOD <MethodName> [ AS <type> ] [ <ctor: CONSTRUCTOR> ] VIRTUAL => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddVirtual( _AsStr_( <MethodName> ) ), )

#xcommand METHOD <MethodName> [ AS <type> ] [ <ctor: CONSTRUCTOR> ] DYNAMIC => ;
   _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddVirtual( _AsStr_( <MethodName> ) ), )

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER _AsFunc_( <MethodName> )  [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
    s_oClass:AddMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName> ;;
    #xcommand METHOD <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>
#else
  #xcommand METHOD <MethodName> [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER _AsFunc_( <MethodName> )  [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
    s_oClass:AddMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

  #xcommand METHOD <MethodName>( [<params,...>] ) [ AS <type> ] OPERATOR <op> [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] [<ov: OVERRIDE>] => ;
    _HB_MEMBER <MethodName>([<params>])  [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
    s_oClass:AddMethod( <(op)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])
#endif /* STRICT_OO */

#xcommand OPERATOR <op> [ARG <xArg>] INLINE <Code,...> [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVARE>] => ;
if( __lInactive, s_oClass:ModInline( <(op)>, {|Self [, <xArg>] | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) ), ;
s_oClass:AddInline( <(op)>, {|Self [, <xArg>] | Self, <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) ) )

//command OPERATOR <op> ARG <xArg> INLINE [Local lx,...] <Code,...> [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] => ;
//oClass:AddInline( <(op)>, {|Self, <xArg> [,<lx>] | <Code> }, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) )

#ifdef STRICT_OO
  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER _AsFunc_( <MessageName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( <(MessageName)>, CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
     #xcommand METHOD <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
     #xcommand METHOD <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>
#else
  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER _AsFunc_( <MessageName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

  #xcommand MESSAGE <MessageName> [ AS <type> ] METHOD <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

  #xcommand MESSAGE <MessageName>([<MsgParams,...>]) [ AS <type> ] METHOD <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-MsgParams->] [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

#endif /* STRICT_OO */

#ifdef STRICT_OO
  #xcommand MESSAGE <MessageName> [ AS <type> ] PROCEDURE <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER _AsFunc_( <MessageName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
     #xcommand PROCEDURE <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>
#else
  #xcommand MESSAGE <MessageName> [ AS <type> ] PROCEDURE <MethodName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER _AsFunc_( <MessageName> ) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[\<anyParams>])

  #xcommand MESSAGE <MessageName> [ AS <type> ] PROCEDURE <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[\<anyParams>])

  #xcommand MESSAGE <MessageName>([<MsgParams,...>]) [ AS <type> ] PROCEDURE <MethodName>([<MtdParams,...>]) [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<sync: SYNC>] => ;
     _HB_MEMBER <MessageName>([<MtdParams>]) [<-MsgParams->] [<-ctor-> AS CLASS _CLASS_NAME_] [ AS <type> ];;
     IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ),;
     s_oClass:AddMethod( _AsStr_( <MessageName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) + IIF( <.sync.>, HB_OO_CLSTP_SYNC, 0 ) ) );;
    #xcommand PROCEDURE <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>(\[\<anyParams>])

#endif /* STRICT_OO */

// MESSAGE ... IS ...
#xcommand MESSAGE <MessageName> [ AS <type> ] IS <DelegateName> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ov: OVERRIDE>] =>;
   _HB_MEMBER _AsFunc_( <MessageName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <MessageName> ), _AsStr_( <DelegateName> ),, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) ), )

// MESSAGE ... TO ...
#xcommand MESSAGE <MessageName> [ AS <type> ] TO <oObject> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ov: OVERRIDE>] =>;
   _HB_MEMBER _AsFunc_( <MessageName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <MessageName> ),, <(oObject)>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) ), )

// MESSAGE ... IN ...
#xcommand MESSAGE <MessageName> [ AS <type> ] IN <oObject> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ov: OVERRIDE>] =>;
   _HB_MEMBER _AsFunc_( <MessageName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <MessageName> ),, <(oObject)>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) ), )

// MESSAGE ... IS ... TO
#xcommand MESSAGE <MessageName> [ AS <type> ] IS <DelegateName> TO <oObject> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ov: OVERRIDE>] =>;
   _HB_MEMBER _AsFunc_( <MessageName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <MessageName> ), _AsStr_( <DelegateName> ), <(oObject)>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) ), )

// MESSAGE ... IS ... IN
#xcommand MESSAGE <MessageName> [ AS <type> ] IS <DelegateName> IN <oObject> [ <ctor: CONSTRUCTOR> ] [ <export: EXPORTED, VISIBLE, PUBLIC>] [<protect: PROTECTED>] [<hidde: HIDDEN, PRIVATE>] [<ov: OVERRIDE>] =>;
   _HB_MEMBER _AsFunc_( <MessageName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddDelegate( _AsStr_( <MessageName> ), _AsStr_( <DelegateName> ), <(oObject)>, HBCLSCHOICE( .F., <.export.>, <.protect.>, <.hidde.> ) + IIF( <.ctor.>, HB_OO_CLSTP_CTOR, 0 ) ), )

#xcommand DELEGATE <*x*> => MESSAGE <x>

#xcommand ACCESS <MessageName> [ AS <type> ] <mth: METHOD, IS> <MethodName> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] => ;
   MESSAGE <MessageName> [ AS <type> ] <mth> <MethodName> <export> <protect> <hidde> <persistent>

#xcommand ASSIGN <MessageName> [ AS <type> ] <mth: METHOD, IS> <MethodName> [<export: EXPORTED, VISIBLE>] [<protect: PROTECTED>] [<hidde: HIDDEN>] [<persistent: PERSISTENT, PROPERTY>] => ;
   MESSAGE _<MessageName> [ AS <type> ] <mth> <MethodName> <export> <protect> <hidde> <persistent>

#ifdef STRICT_OO
  #xcommand METHOD <MethodName> [ AS <type> ]  [<persistent: PERSISTENT, PROPERTY>] SETGET => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
    _HB_MEMBER _AsFunc_( _<MethodName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ),;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ) );;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ),;
    s_oClass:AddMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) );;
    #xcommand METHOD <MethodName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName> ;;
    #xcommand METHOD <MethodName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>
#else
  #xcommand METHOD <MethodName> [ AS <type> ]  [<persistent: PERSISTENT, PROPERTY>] SETGET => ;
    _HB_MEMBER _AsFunc_( <MethodName> ) [ AS <type> ];;
    _HB_MEMBER _AsFunc_( _<MethodName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ), ;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ) );;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ), ;
    s_oClass:AddMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) );;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

  #xcommand METHOD <MethodName>([<params,...>]) [ AS <type> ]  [<persistent: PERSISTENT, PROPERTY>] SETGET => ;
    _HB_MEMBER <MethodName>([<params>]) [ AS <type> ];;
    _HB_MEMBER _<MethodName>([<params>]) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY, <.persistent.> ), ;
    s_oClass:AddMethod( _AsStr_( <MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY, <.persistent.> ) ) ;;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ), ;
    s_oClass:AddMethod( _AsStr_( _<MethodName> ), CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

#endif /* STRICT_OO */

#ifdef STRICT_OO
  #xcommand ACCESS <AccessName> [ AS <type> ] [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER _AsFunc_( <AccessName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ), ;
    s_oClass:AddMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ) ) ;;
    #xcommand METHOD <AccessName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>;;
    #xcommand METHOD <AccessName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AccessName>
#else
  #xcommand ACCESS <AccessName> [ AS <type> ] [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER _AsFunc_( <AccessName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ), ;
    s_oClass:AddMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ) );;
    #xcommand METHOD <AccessName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>(\[\<anyParams>]);;
    #xcommand METHOD <AccessName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AccessName>(\[\<anyParams>])

  #xcommand ACCESS <AccessName>([<params,...>]) [ AS <type> ] [<persistent: PERSISTENT, PROPERTY>] => ;
    _HB_MEMBER <AccessName>([<params>]) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ), ;
    s_oClass:AddMethod( _AsStr_( <AccessName> ), CLSMETH _CLASS_NAME_ _AsName_( <AccessName> )(), HB_OO_CLSTP_EXPORTED + HB_OO_CLSTP_READONLY , <.persistent.> ) ) ;;
    #xcommand METHOD <AccessName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AccessName>(\[\<anyParams>]);;
    #xcommand METHOD <AccessName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AccessName>(\[\<anyParams>])

#endif /* STRICT_OO */

#xcommand ACCESS <AccessName> [ AS <type> ] INLINE [Local <v>,] <code,...> [<persistent: PERSISTENT, PROPERTY>] => ;
   _HB_MEMBER _AsFunc_( <AccessName> ) [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModInline( _AsStr_( <AccessName> ), {|Self [,<v>] | Self, <code> }, HB_OO_CLSTP_EXPORTED, <.persistent.> ), ;
   s_oClass:AddInline( _AsStr_( <AccessName> ), {|Self [,<v>] | Self, <code> }, HB_OO_CLSTP_EXPORTED, <.persistent.> ) )

#xcommand ACCESS <AccessName> [ AS <type> ] DEFERRED => ;
   _HB_MEMBER _AsFunc_( <AccessName> ) [ AS <type> ];;
   IIF( !__lInactive, s_oClass:AddVirtual( _AsStr_( <AccessName> ) ) )

#ifdef STRICT_OO
  #xcommand ASSIGN <AssignName> [ AS <type> ] => ;
    _HB_MEMBER _AsFunc_( _<AssignName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ), ;
    s_oClass:AddMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ) ) ;;
    #xcommand METHOD <AssignName> \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>);;
    #xcommand METHOD <AssignName> \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AssignName>)
#else
  #xcommand ASSIGN <AssignName> [ AS <type> ] => ;
    _HB_MEMBER _AsFunc_( _<AssignName> ) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ), ;
    s_oClass:AddMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ) );;
    #xcommand METHOD <AssignName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>(\[\<anyParams>]);;
    #xcommand METHOD <AssignName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AssignName>(\[\<anyParams>])

  #xcommand ASSIGN <AssignName>([<params,...>]) [ AS <type> ] => ;
    _HB_MEMBER _<AssignName>([<params>]) [ AS <type> ];;
    IIF( __lInactive, s_oClass:ModMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ), ;
    s_oClass:AddMethod( _AsStr_( _<AssignName> ), CLSMETH _CLASS_NAME_ _AsName_( _<AssignName> )(), HB_OO_CLSTP_EXPORTED ) ) ;;
    #xcommand METHOD <AssignName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <AssignName>(\[\<anyParams>]);;
    #xcommand METHOD <AssignName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <AssignName>(\[\<anyParams>])

#endif /* STRICT_OO */

#xcommand ASSIGN <AssignName>( [<params,...>] ) [ AS <type> ] INLINE [Local <v>,] <Code,...> => ;
   _HB_MEMBER _<AssignName>([<params>]) [ AS <type> ];;
   IIF( __lInactive, s_oClass:ModInline( _AsStr_( _<AssignName> ), {|Self [,<params>] [,<v>] | Self, <Code> }, HB_OO_CLSTP_EXPORTED ), ;
   s_oClass:AddInline( _AsStr_( _<AssignName> ), {|Self [,<params>] [,<v>] | Self, <Code> }, HB_OO_CLSTP_EXPORTED ) )

#xcommand ON ERROR <MethodName> => ERROR HANDLER <MethodName>;

#ifdef STRICT_OO
   #xcommand ERROR HANDLER <MethodName> => ;
     _HB_MEMBER _AsFunc_( <MethodName> );;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ;;
     #xcommand METHOD <MethodName>                    \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>;;
     #xcommand METHOD <MethodName>                    \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>
#else
   #xcommand ERROR HANDLER <MethodName> => ;
     _HB_MEMBER _AsFunc_( <MethodName> );;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])

   #xcommand ERROR HANDLER <MethodName>([<params,...>]) => ;
     _HB_MEMBER <MethodName>([<params>]);;
     s_oClass:SetOnError( CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ;;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>(\[\<anyParams>]);;
    #xcommand METHOD <MethodName> \[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED METHOD \<ClassName> <MethodName>(\[\<anyParams>])
#endif /* STRICT_OO */

#xcommand DESTRUCTOR <MethodName> => ;
  _HB_MEMBER _AsFunc_( <MethodName> );;
  s_oClass:SetDestructor( CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ;;
 #xcommand PROCEDURE <MethodName>\[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>;;
 #xcommand PROCEDURE <MethodName>\[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>

#xcommand DESTRUCTOR <MethodName>() => ;
  _HB_MEMBER _AsFunc_( <MethodName> );;
  s_oClass:SetDestructor( CLSMETH _CLASS_NAME_ _AsName_( <MethodName> )() ) ;;
 #xcommand PROCEDURE <MethodName>\[(\[\<anyParams,...>])] \[DECLCLASS _CLASS_NAME_] _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>;;
 #xcommand PROCEDURE <MethodName>\[(\[\<anyParams,...>])] \<ClassName> _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE \<ClassName> <MethodName>

#xtranslate END CLASS => ENDCLASS

#ifdef HB_CLS_ALLOWCLASS
#xcommand ENDCLASS => ;;
                        IF __lInactive ;;
                           __clsActive(s_oClass:hClass) ;;
                           s_oClass:Refresh() ;;
                        ELSE ;;
                           s_oClass:Create(MetaClass) ;;
                        END ;;
                        oClassInstance := __clsInst( s_oClass:hClass ) ;;
                        IF __ObjHasMsg( oClassInstance, "InitClass" );;
                          oClassInstance:InitClass( hb_aParams() ) ;;
                        END ;;
                      ELSE ;;
                        oClassInstance := __clsInst( s_oClass:hClass ) ;;
                      END ;;
                      IF PCount() > 0 ;;
                       #ifdef HB_CONSTRUCTOR_USE_DIVERT ;;
                         DIVERT TO (@DivertConstructorCall()) OF s_oClass ;;
                       #else ;;
                         RETURN s_oClass:ConstructorCall( oClassInstance, hb_aparams() ) ;;
                       #endif ;;
                      END ;;
                      RETURN oClassInstance AS CLASS _CLASS_NAME_ ;;
                      #undef  _CLASS_MODE_ ;;
                      #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_
#else
#xcommand ENDCLASS => ;;
                        IF __lInactive ;;
                           __clsActive(s_oClass:hClass) ;;
                           s_oClass:Refresh() ;;
                        ELSE ;;
                           s_oClass:Create() ;;
                        END ;;
                        oClassInstance := __clsInst( s_oClass:hClass ) ;;
                        IF __ObjHasMsg( oClassInstance, "InitClass" );;
                          oClassInstance:InitClass( hb_aParams() ) ;;
                        END ;;
                      ELSE ;;
                        oClassInstance := __clsInst( s_oClass:hClass ) ;;
                      END ;;
                      IF PCount() > 0 ;;
                       #ifdef HB_CONSTRUCTOR_USE_DIVERT;;
                         DIVERT TO (@DivertConstructorCall()) OF s_oClass ;;
                       #else ;;
                         RETURN s_oClass:ConstructorCall( oClassInstance, hb_aparams() ) ;;
                       #endif ;;
                      END ;;
                      RETURN oClassInstance AS CLASS _CLASS_NAME_ ;;
                      #undef  _CLASS_MODE_ ;;
                      #define _CLASS_MODE_ _CLASS_IMPLEMENTATION_
#endif /* HB_CLS_ALLOWCLASS */

#xtranslate :Super( <SuperClass> ) : => :<SuperClass>:
#xtranslate :Super() : => :Super:
#xtranslate :Super() => :Super


#ifndef HB_SHORTNAMES

#xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName>  => METHOD <MethodName>  _CLASS_MODE_
#xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> CLASS <ClassName> => METHOD <MethodName>  CLASS <ClassName>  _CLASS_MODE_

//#define HB_CLS_NO_OO_ERR

#ifdef HB_CLS_NO_OO_ERR
   #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> CLASS <ClassName> => METHOD <MethodName>     CLASS <ClassName> _CLASS_IMPLEMENTATION_

   #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName>                      _CLASS_IMPLEMENTATION_ => DECLARED METHOD _CLASS_NAME_ <MethodName>
   #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> CLASS <ClassName>    _CLASS_IMPLEMENTATION_ => DECLARED METHOD <ClassName> <MethodName>

   #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName> CLASS <ClassName> => PROCEDURE <MethodName>     CLASS <ClassName> _CLASS_IMPLEMENTATION_

   // Do NOT uncomment!
   //#xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName>                      _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE _CLASS_NAME_ <MethodName>
   #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName> CLASS <ClassName>    _CLASS_IMPLEMENTATION_ => DECLARED PROCEDURE <ClassName> <MethodName>
#else
   #xcommand METHOD [FUNCTION] <MethodName> CLASS <ClassName> => METHOD <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_

   #ifdef STRICT_OO
      #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName>                   _CLASS_IMPLEMENTATION_ => __ERR(Method <"MethodName"> not declared or declaration mismatch in class: _CLASS_NAME_) ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
      #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> [CLASS] <ClassName> _CLASS_IMPLEMENTATION_ => #error Method <"MethodName"> not declared or declaration mismatch in class: <ClassName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
   #else
      #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName>                   _CLASS_IMPLEMENTATION_ => __ERR(Method <"MethodName"> not declared in class: _CLASS_NAME_) ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
      #xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> [CLASS] <ClassName> _CLASS_IMPLEMENTATION_ => #error Method <"MethodName"> not declared in class: <ClassName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
   #endif

   #xcommand PROCEDURE [FUNCTION] <MethodName> CLASS <ClassName> => PROCEDURE <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_

   #ifdef STRICT_OO
      #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName>                   _CLASS_IMPLEMENTATION_ => #error Procedure <"MethodName"> not declared or declaration mismatch in class: _CLASS_NAME_ ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
      #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName> [CLASS] <ClassName> _CLASS_IMPLEMENTATION_ => #error Procedure <"MethodName"> not declared or declaration mismatch in class: <ClassName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
   #else
      #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName>                   _CLASS_IMPLEMENTATION_ => #error Procedure <"MethodName"> not declared in class: _CLASS_NAME_ ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
      #xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName> [CLASS] <ClassName> _CLASS_IMPLEMENTATION_ => #error Procedure <"MethodName"> not declared in class: <ClassName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
   #endif
#endif /* HB_CLS_NO_OO_ERR */

//#xcommand METHOD [FUNCTION] [PROCEDURE] <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_ => #error Class <"ClassName"> not declared for method: <MethodName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()
//#xcommand PROCEDURE [FUNCTION] [PROCEDURE] <MethodName> DECLCLASS <ClassName> _CLASS_IMPLEMENTATION_ => #error Class <"ClassName"> not declared for procedure: <MethodName> ; UTILITY FUNCTION <MethodName> ; local self := QSelf()

#xcommand DECLARED METHOD <ClassName> <MethodName> => ;
          UTILITY STATIC function DECLMETH <ClassName> <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand DECLARED PROCEDURE <ClassName> <MethodName> => ;
          UTILITY STATIC PROCEDURE DECLMETH <ClassName> <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ACCESS <AccessName> CLASS <ClassName> => ;
          UTILITY STATIC FUNCTION <ClassName>_<AccessName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ASSIGN <AssignName> CLASS <ClassName> => ;
          UTILITY STATIC FUNCTION <ClassName>__<AssignName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>
#else

#xcommand DECLARED METHOD <ClassName> <MethodName>=> ;
          UTILITY STATIC FUNCTION <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand DECLARED PROCEDURE <ClassName> <MethodName>=> ;
          UTILITY STATIC PROCEDURE <MethodName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ACCESS <AccessName> CLASS <ClassName> => ;
          UTILITY STATIC FUNCTION <AccessName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#xcommand ASSIGN <AssignName> CLASS <ClassName> => ;
          UTILITY STATIC FUNCTION _<AssignName> ;;
          local Self AS CLASS <ClassName> := QSelf() AS CLASS <ClassName>

#endif /* HB_SHORTNAMES */

#xcommand METHOD <!ClassName!>:<MethodName> => METHOD <MethodName> CLASS <ClassName>

/* Friend function/class definitions */
#xcommand FRIEND CLASS <ClassName1> [, <ClassNameN> ] => ;
   s_oClass:AddFriends( <ClassName1> [, <ClassNameN> ] )

#xcommand FRIEND FUNCTION <FuncName1> [, <FuncNameN> ] => ;
   s_oClass:AddFriends( @<FuncName1>() [, @<FuncNameN>() ] )


#ifdef HB_CLS_CSY
   #xcommand    EXPORTED:       =>      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope )
   #xcommand    EXPORT:         =>      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope )
   #xcommand    VISIBLE:        =>      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope )
   #xcommand    PUBLIC:         =>      nScope := HB_OO_CLSTP_EXPORTED ; ( nScope )

   #xcommand    HIDDEN:         =>      nScope := HB_OO_CLSTP_HIDDEN ; ( nScope )
   #xcommand    PRIVATE:        =>      nScope := HB_OO_CLSTP_HIDDEN ; ( nScope )

   #xcommand    PROTECTED:      =>      nScope := HB_OO_CLSTP_PROTECTED ; ( nScope )
   #xcommand    PUBLISHED:      =>      nScope := HB_OO_CLSTP_PUBLISHED ; ( nScope )
#endif

#endif /* HB_CLASS_CH_ */
