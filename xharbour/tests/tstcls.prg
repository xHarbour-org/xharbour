#include "hbclass.ch"

Procedure Main()

   LOCAL oParrent := TParrent():New( "X in Parrent" )
   LOCAL oChild   := TChild():New( "X in Child" )

   oChild:Create( "Test Child Create()" )

Return

Class TParrent
   Data X
   Method New( p ) CONSTRUCTOR
   Method Create( p )
End class

Method New( p ) Class TParrent

   ::X := p

Return Self

Method Create( p ) Class TParrent

    Alert( "Parrent Create() received: '" + p + "'" )

Return p

Class TChild FROM TParrent
    Method Create( p )
End class

Method Create( p ) Class TChild

   Alert( "Child Create() received: '" + p + "'" )

   Super:Create( ::X )

Return p

