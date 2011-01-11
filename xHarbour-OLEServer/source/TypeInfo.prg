GLOBAL g_ScanMemberID, g_MemberID, g_ScanInterfaceGUID, g_InterfaceGUID

#include "hbclass.ch"

CLASS TypeLib
   DATA Name
   DATA GUID         INIT ""
   DATA HelpString
   DATA HelpFile

   DATA Enumerations INIT {}
   DATA Objects      INIT {}
   DATA Interfaces   INIT {}

   DATA MajorVer
   DATA MinorVer

   //DATA Properties   INIT {}
   //DATA Methods      INIT {}
   //DATA Events       INIT {}
ENDCLASS

CLASS EnumTypeInfo
   DATA Name
   DATA HelpString
   DATA Constants    INIT {}
ENDCLASS

CLASS ConstantTypeInfo
   DATA Name
   DATA TypeDesc
   DATA VT
   DATA Value
   DATA HelpString
ENDCLASS

CLASS ObjectTypeInfo
   DATA Name
   DATA GUID         INIT ""
   DATA ProgID
   DATA Flags
   DATA HelpString
   DATA HelpFile

   DATA Enumerations INIT {}
   DATA Interfaces   INIT {}

   //DATA Properties   INIT {}
   //DATA Methods      INIT {}
   //DATA Events       INIT {}
ENDCLASS

CLASS InterfaceTypeInfo
   DATA Name
   DATA GUID         INIT ""
   DATA ProgID
   DATA HelpString
   DATA IID
   DATA Flags

   DATA Properties   INIT {}
   DATA Methods      INIT {}
   DATA Events       INIT {}
ENDCLASS

CLASS PropertyTypeInfo
   DATA Name
   DATA GUID         INIT ""
   DATA HelpString
   DATA MemberID
   DATA ReadOnly     INIT .T.
   DATA ByRef        INIT .F.
   DATA VtblOffset
   DATA TypeDesc
   DATA VT
   DATA Interface

   DATA Arguments    INIT {}
ENDCLASS

CLASS MethodTypeInfo
   DATA Name
   DATA GUID         INIT ""
   DATA HelpString
   DATA MemberID
   DATA VtblOffset
   DATA TypeDesc
   DATA VT
   DATA Interface

   DATA Arguments    INIT {}
ENDCLASS

CLASS ArgumentTypeInfo
   DATA Name
   DATA GUID         INIT ""
   DATA HelpString
   DATA TypeDesc
   DATA VT
   DATA Interface

   DATA ByRef        INIT .F.
   DATA Optional     INIT .F.
   DATA Flags
ENDCLASS

INIT PROCEDURE __INIT_TypeLib

   g_ScanMemberID    := {|Method| /*TraceLog( g_MemberID, Method:MemberID ),*/ Method:MemberID == g_MemberID }
   g_ScanInterfaceGUID := {|Interface| /*TraceLog( g_InterfaceGUID, Interface:GUID ),*/ Interface:GUID == g_InterfaceGUID }

RETURN

FUNCTION InterfaceByName( oTypeLib, cName )

   LOCAL nPos := aScan( oTypeLib:Interfaces, {|Interface| Interface:Name == cName } )

RETURN oTypeLib:Interfaces[ nPos ]

FUNCTION ObjectByName( oTypeLib, cName )

   LOCAL nPos := aScan( oTypeLib:Objects, {|Object| Object:Name == cName } )

RETURN oTypeLib:Objects[ nPos ]
