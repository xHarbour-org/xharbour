#define DATETIME_YEAR         1
#define DATETIME_MONTH        2
#define DATETIME_DAYOFWEEK    3
#define DATETIME_DAY          4
#define DATETIME_HOUR         5
#define DATETIME_MINUTE       6
#define DATETIME_SECOND       7
#define DATETIME_MILLISECONDS 8
/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*----------------------------------------------------------------------------
*Parts of this project come from:
*"Harbour MiniGUI"
*                  Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                  http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "winuser.ch"

//#define DATETIME_YEAR         1
//#define DATETIME_MONTH        2
//#define DATETIME_DAYOFWEEK    3
//#define DATETIME_DAY          4
//#define DATETIME_HOUR         5
//#define DATETIME_MINUTE       6
//#define DATETIME_SECOND       7
//#define DATETIME_MILLISECONDS 8

// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

// some trivial ones
#define MONTHS_IN_YEAR             12
#define SEC_PER_MIN                60
#define MIN_PER_HOUR               60
#define HOURS_PER_DAY              24
#define SECONDS_PER_DAY         86400
#define DAYS_PER_WEEK               7
#define MILLISECONDS_PER_DAY 86400000

//// this is the integral part of JDN of the midnight of Jan 1, 1970
//// (i.e. JDN(Jan 1, 1970) = 2440587.5)
//static const long EPOCH_JDN = 2440587
//
//// the date of JDN -0.5 (as we don't work with fractional parts, this is the
//// reference date for us) is Nov 24, 4714BC
//static const int JDN_0_YEAR = -4713;
//static const int JDN_0_MONTH = wxDateTime::Nov;
//static const int JDN_0_DAY = 24;
//
//// the constants used for JDN calculations
//static const long JDN_OFFSET         = 32046l;
#define DAYS_PER_5_MONTHS     153
#define DAYS_PER_4_YEARS     1461
#define DAYS_PER_400_YEARS 146097

// this array contains the cumulated number of days in all previous months for
// normal and leap years
#define ARRAY_CUMULATEDDAYS  ;
{ ;
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 }, ;
    { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 }  ;
}

// the number of days in month in Julian/Gregorian calendar: the first line
// is for normal years, the second one is for the leap ones
#define ARRAY_DAYSINMONTH ;
{ ;
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }, ;
    { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }  ;
}

CLASS TDateTime FROM TObject

   DATA   aDateTime  AS ARRAY INIT { 0,0,0,0,0,0,0,0 } HIDDEN

   METHOD GetYear()                      INLINE ::aDateTime[ DATETIME_YEAR         ]
   METHOD GetMonth()                     INLINE ::aDateTime[ DATETIME_MONTH        ]
   METHOD GetDayOfWeek()                 INLINE ::aDateTime[ DATETIME_DAYOFWEEK    ]
   METHOD GetDay()                       INLINE ::aDateTime[ DATETIME_DAY          ]
   METHOD GetHour()                      INLINE ::aDateTime[ DATETIME_HOUR         ]
   METHOD GetMinute()                    INLINE ::aDateTime[ DATETIME_MINUTE       ]
   METHOD GetSecond()                    INLINE ::aDateTime[ DATETIME_SECOND       ]
   METHOD GetMilliseconds()              INLINE ::aDateTime[ DATETIME_MILLISECONDS ]

   METHOD SetYear( x )                   INLINE ::aDateTime[ DATETIME_YEAR         ] := x
   METHOD SetMonth( x )                  INLINE ::aDateTime[ DATETIME_MONTH        ] := x
   METHOD SetDayOfWeek( x )              INLINE ::aDateTime[ DATETIME_DAYOFWEEK    ] := x
   METHOD SetDay( x )                    INLINE ::aDateTime[ DATETIME_DAY          ] := x
   METHOD SetHour( x )                   INLINE ::aDateTime[ DATETIME_HOUR         ] := x
   METHOD SetMinute( x )                 INLINE ::aDateTime[ DATETIME_MINUTE       ] := x
   METHOD SetSecond( x )                 INLINE ::aDateTime[ DATETIME_SECOND       ] := x
   METHOD SetMilliseconds( x )           INLINE ::aDateTime[ DATETIME_MILLISECONDS ] := x

   METHOD New()
   //METHOD Create()

   METHOD Now()                          INLINE ::aDateTime := GetSystemTime()

   METHOD GetDate()
   METHOD GetDateTime()                  INLINE ::aDateTime
   METHOD GetDayName()                   INLINE CDoW( ::GetDate() )
   METHOD GetDayOfYear()
   METHOD GetDayOfMonth()
   METHOD GetLastDayOfYear()             INLINE EOY( ::GetDate() )
   METHOD GetLastDayOfMonth()            INLINE EOM( ::GetDate() )
   METHOD GetMonthName()                 INLINE CMonth( ::GetDate() )
   METHOD GetNumberOfDaysInMonth()
   METHOD GetNumberOfDaysInYear()
   METHOD GetSystemTime()
   METHOD GetTime()
   METHOD IsBetween( dDateBefore, dDateAfter ) ;
                                         INLINE dDateBefore <= ::GetDate() .AND. ;
                                                ::GetDate() <= dDateAfter
   METHOD IsEarlierThan( dDate )         INLINE dDate < ::GetDate()
   METHOD IsEqualTo( dDate )             INLINE dDate == ::GetDate()
   METHOD IsLaterThan( dDate )           INLINE dDate > ::GetDate()
   METHOD IsLeapYear()
   METHOD IsSameDate( dDate  )           INLINE dDate == ::GetDate()
   METHOD IsStrictlyBetween( dDateBefore, dDateAfter ) ;
                                         INLINE dDateBefore < ::GetDate() .AND. ;
                                                ::GetDate() < dDateAfter
   METHOD IsValid()
   METHOD ResetTime()                    INLINE ::SetHour := ::SetMinute := ::SetSecond := ;
                                                ::SetMilliSeconds := 0,;
                                                ::aDateTime

   METHOD SetDate( dDate )               INLINE ::DateToDateTime( dDate )
   METHOD SetDateTime( aDateTime )       INLINE ::aDateTime := aDateTime
   METHOD SetSystemTime()
   METHOD SetToCurrent()                 INLINE ::Now()
   METHOD SetValue()                     INLINE ::SetDate()
   METHOD Today()                        INLINE ::Now(), ::ResetTime()

   METHOD DateToDateTime()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( aDateTime ) CLASS TDateTime
   DEFAULT aDateTime TO GetSystemTime()

   IF ValType( aDateTime ) == "A"
      ::aDateTime := aDateTime
   ELSEIF ValType( aDateTime ) == "D"
      ::DateToDateTime( aDateTime )
   ENDIF

RETURN Self

METHOD GetDate( aDateTime ) CLASS TDateTime
   LOCAL dDate
   DEFAULT aDateTime TO ::aDateTime
   //::DisplayArray( aDateTime )
   dDate := hb_Stod( StrZero( aDateTime[ DATETIME_YEAR  ], 4 ) + ;
                  StrZero( aDateTime[ DATETIME_MONTH ], 2 ) + ;
                  StrZero( aDateTime[ DATETIME_DAY   ], 2 ) )

   //MessageBox(, "dDate := " + cStr( hb_Stod( StrZero( aDateTime[ DATETIME_YEAR  ], 4 ) + ;
   //               StrZero( aDateTime[ DATETIME_MONTH ], 2 ) + ;
   //               StrZero( aDateTime[ DATETIME_DAY   ], 2 ) ) ) )

RETURN dDate

METHOD GetDayOfYear() CLASS TDateTime
   LOCAL dDate := hb_Stod( Str( ::aDateTime[ DATETIME_YEAR  ], 4 ) + "0101" )
RETURN ::GetDate() - dDate + 1

METHOD GetDayOfMonth() CLASS TDateTime
   LOCAL dDate := hb_Stod( StrZero( ::aDateTime[ DATETIME_YEAR  ], 4 ) + ;
                        StrZero( ::aDateTime[ DATETIME_MONTH ], 2 ) + "01" )
RETURN ::GetDate() - dDate + 1

METHOD GetTime( aDateTime ) CLASS TDateTime
   LOCAL cTime

   DEFAULT aDateTime TO ::aDateTime
   cTime := StrZero( aDateTime[ DATETIME_HOUR   ], 2 ) + ":" +;
            StrZero( aDateTime[ DATETIME_MINUTE ], 2 ) + ":" +;
            StrZero( aDateTime[ DATETIME_SECOND ], 2 )

RETURN cTime

METHOD GetNumberOfDaysInMonth( nMonth, nYear ) CLASS TDateTime
    IF ( nMonth == ::Invalid_Month .OR. nMonth == NIL )
        nMonth := ::GetMonth()
    ENDIF
    IF ( nYear == ::Invalid_Year .OR. nYear == NIL )
        nYear := ::GetYear()
    ENDIF
RETURN ARRAY_DAYSINMONTH[ IIF( ::IsLeapYear( nYear ), 1, 2 ) ][ nMonth ]

METHOD GetNumberOfDaysInYear( nYear AS NUMERIC ) CLASS TDateTime

    IF ( nYear == ::Invalid_Year .OR. nYear == NIL )
        // take the current year if none given
        nYear = ::GetYear()
    ENDIF

RETURN IIF( ::IsLeapYear( nYear ), 366, 365 )

METHOD GetSystemTime() CLASS TDateTime
   ::aDateTime := GetSystemTime()
RETURN ::aDateTime

METHOD IsLeapYear( nYear AS NUMERIC ) CLASS TDateTime

    IF ( nYear == ::Invalid_Year .OR. nYear == NIL )
        nYear := ::GetYear()
    ENDIF

    // in Gregorian calendar leap years are those divisible by 4 except
    // those divisible by 100 unless they're also divisible by 400
    // (in some countries, like Russia and Greece, additional corrections
    // exist, but they won't manifest themselves until 2700)

RETURN ( nYear % 4 == 0 ) .AND. ( ( nYear % 100 != 0 ) .OR. ( nYear % 400 == 0 ) )

METHOD IsValid() CLASS TDateTime
 // we allow for the leap seconds, although we don't use them (yet)
RETURN ( ::GetYear() >= 1601 .AND. ::GetYear() <= 30827 ) .AND. ;
       ( ::GetMonth() >= 1 .AND. ::GetMont() <= 12 ) .AND.;
       ( ::GetMonthDay() <= ::GetNumOfDaysInMonth(::Year, ::Month ) ) .AND. ;
       ( ::GetHour() >= 0 .AND. ::GetHour() < 24 ) .AND. ;
       ( ::GetMinute() >= 0 .AND. ::GetMinute() < 60 ) .AND. ;
       ( ::GetSecond() >= 0 .AND. ::GetSecond() < 60 ) .AND. ;
       ( ::GetMilliSeconds >= 0 .AND. ::GetMilliSeconds() < 1000 )

METHOD SetSystemTime( aDateTime ) CLASS TDateTime
   DEFAULT aDateTime TO ::aDateTime
RETURN SetSystemTime( ::aDateTime )

METHOD DateToDateTime( dDate ) CLASS TDateTime
   DEFAULT dDate TO DATE()
   IF ValType( dDate ) == "D" .AND. dDate <> NIL
      ::aDateTime := { ;
                       YEAR( dDate )  ,;
                       MONTH( dDate ) ,;
                       DOW( dDate )   ,;
                       DAY( dDate )   ,;
                       0              ,;
                       0              ,;
                       0              ,;
                       0               ;
                     }
   ENDIF
RETURN ::aDateTime
   