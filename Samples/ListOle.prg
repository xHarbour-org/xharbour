//#translate ? <x,...> => TraceLog( <x> )
#translate ? [<x,...>] => OutStd( Chr(13) + Chr(10) ); OutStd( <x> )

PROCEDURE Main( cID )

   LOCAL oTypeLib, lAllObjects

   lAllObjects := .T.// ( Lower( Right( cID, 4 ) ) IN ".exe.dll.ocx.tlb.olb" )

   TraceLog( Seconds() )
   oTypeLib := LoadTypeLib( cID, lAllObjects )
   TraceLog( Seconds() )

   DumpTypeLib( oTypeLib )

RETURN

PROCEDURE DumpTypeLib( oTypeLib )

   LOCAL Enumeration, Constant, Object, Interface, Property, Method, Event, Arg

   WITH OBJECT oTypeLib

      FOR EACH Enumeration IN :Enumerations
         ? Enumeration:Name

         FOR EACH Constant IN Enumeration:Constants
             ? "  ", Constant:Name, Constant:Value, Constant:VT, Constant:TypeDesc, Constant:HelpString
         NEXT

         ?
      NEXT

      ?

      FOR EACH Object IN :Objects
          ? "Object:", Object:Name, Object:ProgID, Object:GUID, Object:HelpFile, Object:HelpString

          FOR EACH Interface IN Object:Interfaces
             ? "  ", Object:Name, "Interface:", Interface:Name, Interface:GUID, Interface:HelpString
          NEXT

          ?
      NEXT

      FOR EACH Interface IN :Interfaces
          ? "Interface:", Interface:Name, Interface:GUID, Interface:Flags, Interface:HelpString

          FOR EACH Property IN Interface:Properties
             ? "      Property:", Property:Name, IIF( Property:ReadOnly, "*READONLY*", IIF( Property:ByRef, "*BYREF*", "" ) ), Property:MemberID, Property:VT, Property:TypeDesc, Property:GUID, Property:HelpString

             FOR EACH Arg IN Property:Arguments
                ? "         Arg:", Arg:Name, IIF( Arg:ByRef, "*BYREF*", "" ), IIF( Arg:Optional, "*OPTIONAL*", "" ), Arg:VT, Arg:TypeDesc, Arg:GUID, Arg:Interface
             NEXT
          NEXT

          ?

          FOR EACH Method IN Interface:Methods
             ? "      Method:", Method:Name, Method:MemberID, Method:VtblOffset, Method:VT, Method:TypeDesc, Method:GUID, Method:Interface, Method:HelpString

             FOR EACH Arg IN Method:Arguments
                ? "         Arg:", Arg:Name, IIF( Arg:ByRef, "*BYREF*", "" ), IIF( Arg:Optional, "*OPTIONAL*", "" ), Arg:VT, Arg:TypeDesc, Arg:GUID, Arg:Interface
             NEXT

             ?
          NEXT

          ?

          FOR EACH Event IN Interface:Events
             ? "      Event:", Event:Name, Event:MemberID, Event:VT, Event:TypeDesc, Event:HelpString

             FOR EACH Arg IN Event:Arguments
                ? "         Arg:", Arg:Name, IIF( Arg:ByRef, "*BYREF*", "" ), IIF( Arg:Optional, "*OPTIONAL*", "" ), Arg:VT, Arg:TypeDesc, Arg:GUID, Arg:Interface
             NEXT

             ?
          NEXT

          ?
      NEXT

   END

RETURN
