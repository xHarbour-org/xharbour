#include "set.ch"

PROCEDURE Main()

   LOCAL dDate, cDate, dDateTime, nHour
   
   CLS
   @24,0
   
   OutStd( "Test of Date Constant - X H A R B O U R", HB_OsNewLine() )
   OutStd( HB_OsNewLine(), HB_OsNewLine() )
   
   SET TIME FORMAT "HH:MM:SS PM"
   
   //Set(_SET_TIMEFORMAT,"HH:MM:SS.CCC PM")
   
   
   dDate := {^2004/01/01}
   OutStd( "{^2004/01/01} -> Valtype:", Valtype( dDate ),"Expected: 01/01/04 Result:", dDate, HB_OsNewLine() )
   
   Set Century On
   Outstd( "{^2004/01/01} -> Valtype:", Valtype( dDate ),"Expected: 01/01/2004 Result:", dDate, HB_OsNewLine() )
   
   dDate := {^2004/01/01}+1
   OutStd( "{^2004/01/01}+1 -> Valtype:", Valtype( dDate ),"Expected: 01/02/2004 Result:", dDate, HB_OsNewLine() )
   
   dDate := {^2004/01/01}-1
   OutStd( "{^2004/01/01}-1 -> Valtype:", Valtype( dDate ),"Expected: 12/31/2003 Result:", dDate, HB_OsNewLine() )
   
   dDate := STOD( "20040101" )
   OutStd( 'STOD( "20040101" ) -> Valtype:', Valtype( dDate ),"Expected: 01/01/2004 Result:",dDate, HB_OsNewLine() )
   
   
   dDate := {^2004/01/01 01:59:59.999 PM}
   Outstd( "{^2004/01/01 01:59:59.999 PM} -> Valtype:", Valtype( dDate ),"Expected: 01/01/04 01:59:59 PM Result:", dDate, HB_OsNewLine() )
   
   cDate := "{^2004/01/01 01:59:59.99}"
   dDate := &cDate
   Outstd( "Macro compilacion '{^2004/01/01 01:59:59.99}'", dDate, HB_OsNewLine() )
   
   
   cDate := "{^2004/01/01 01:59}"
   dDate := &cDate
   Outstd( "Macro compilacion '{^2004/01/01 01:59}'", dDate, HB_OsNewLine() )
   
   cDate := "{^2004/01/01}"
   dDate := &cDate
   Outstd( "Macro compilacion '{^2004/01/01}'", dDate, HB_OsNewLine() )
   
   dDateTime := {^ 2004/01/01 01:59}
   Outstd( "{^ 2004/01/01 01:59} -> Valtype:", Valtype( dDateTime ),"Expected: 01/01/2004 01:59:00 AM Result:", dDateTime, HB_OsNewLine() )
   
   dDateTime := {^ 2004/01/01 01:59:59}
   Outstd( "{^ 2004/01/01 01:59:59} -> Valtype:", Valtype( dDateTime ),"Expected: 01/01/2004 01:59:59 AM Result:", dDateTime, HB_OsNewLine() )
   
   dDateTime := {^ 2004/01/01 12:59:59.15}
   Outstd( "{^ 2004/01/01 12:59:59.15} -> Valtype:", Valtype( dDateTime ),"Expected: 01/01/2004 12:59:59 PM Result:", dDateTime, HB_OsNewLine() )
   
   Outstd( "dDateTime-dDate  ({^ 2004/01/01 12:59:59.15} - {^2004/01/01})", dDateTime - dDate, HB_OsNewLine() )
   Outstd( "dDate + 1  ({^2004/01/01} + 1)", dDate + 1, HB_OsNewLine() )
   Outstd( "dDateTime + 1 ({^ 2004/01/01 12:59:59.15} + 1)", dDateTime + 1.0, HB_OsNewLine() )
   
   dDateTime := STOT( "20040101125959.15" )
   
   Outstd( 'dDateTime := STOT( "20040101125959.15" )', HB_OsNewLine() )
   
   Outstd( 'TTOC( dDateTime )=',TTOC( dDateTime ), HB_OsNewLine() )
   Outstd( 'TTOS( dDateTime )=',TTOS( dDateTime ), HB_OsNewLine() )
   Outstd( 'DateTime()=',DateTime(), HB_OsNewLine() )
   Outstd( 'Hour(DateTime())=',Hour(DateTime()),'Minute(DateTime())=',Minute(DateTime()),'Secs(DateTime())=',Secs(DateTime()), HB_OsNewLine() )
   
   Outstd( 'CTOT( TTOC( dDateTime ))=',CTOT( TTOC( dDateTime )), HB_OsNewLine() )
   Outstd( 'STOT( TTOS( dDateTime ))=',STOT( TTOS( dDateTime )), HB_OsNewLine() )
   
   Outstd( 'TTOC( dDateTime - (12/24) )=',TTOC( dDateTime - (12/24) ), HB_OsNewLine() )
   Outstd( 'CTOT( TTOC( dDateTime - (12/24) ))=',CTOT( TTOC( dDateTime - (12/24) )), HB_OsNewLine() )
   
   dDateTime := {^ 12:59:59.15}
   Outstd( "{^ 12:59:59.15} -> Valtype:", Valtype( dDateTime ),"Expected: 30/12/1899 12:59:59 PM Result:", dDateTime, HB_OsNewLine() )
   
   nHour := dDateTime - {^ 06:00}
   Outstd( "dDateTime - {^ 06:00}  ({^ 12:59:59.15} - {^ 06:00})", nHour, HB_OsNewLine() )
   
   OutStd( "Date() + nHour=", Date() + nHour, HB_OsNewLine() )
   OutStd( "TTOC( Date() + nHour, 2 )=", TTOC( Date() + nHour, 2 ), HB_OsNewLine() )
   
   OutStd( "CTOT( TTOC( Date() + nHour, 2 ), 2 )=", CTOT( TTOC( Date() + nHour, 2 ), 2 ), HB_OsNewLine() )
   
RETURN