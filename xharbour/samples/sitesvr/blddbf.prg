/*
 * $Id: blddbf.prg,v 1.1 2003/01/04 08:51:18 fsgiudice Exp $
 */

***********************************************************
* Web Server in xHarbour programming language!
* Copyright, Giancarlo Niccolai 2002
*
* LICENSE
* This software is released under GPL license 2.0 or any
* later version, at your option. This is free software,
* provided as is, without any warranty, even without the
* implied warranty of suitability for any particular needs.
* Commercial use of this software is forbidden.
*
* BuildDBF - create working data
* Copyright Francesco Saverio Giudice 2003 - <info@fsgiudice.com>
*

#ifndef CRLF
  #define CRLF  CHR(13)+CHR(10)
#endif

PROCEDURE BuildDBF()
   LOCAL aStruct

   // Check if exist database
   IF !File( "site.dbf" )

      aStruct := {                                ;
                   { "ID"       , "N", 10, 0 },   ;
                   { "NAME"     , "C", 50, 0 },   ;
                   { "PARENT"   , "N", 10, 0 },   ;
                   { "MIMETYPE" , "C", 30, 0 },   ;
                   { "CONTENT"  , "M", 10, 0 }    ;
                 }

      dbCreate( "site.dbf", aStruct )

      USE "site.dbf"
      INDEX ON field->ID TO "site01"
      INDEX ON STR( field->PARENT ) + "-" + field->NAME TO "site02"

      SET INDEX TO "site01", "site02"

      APPEND BLANK
      field->id       := 1
      field->name     := "ROOT"
      field->parent   := 0
      field->mimetype := "text/html"
      field->content  := [<HTML>                                                        ] + CRLF + ;
                         [<HEAD>                                                        ] + CRLF + ;
                         [<TITLE>xHarbour Server demo site</TITLE>                       ] + CRLF + ;
                         [<BODY>                                                        ] + CRLF + ;
                         [    <H1>xHarbour Server Demo Site</H1>                         ] + CRLF + ;
                         [    <H2>Index</H2>                                            ] + CRLF + ;
                         [    <UL>                                                      ] + CRLF + ;
                         [      <LI>An HTML text in this area: <a href="/test">CLICK</A>] + CRLF + ;
                         [      <LI>Area 1: <A href="/Area1/">CLICK</a>                 ] + CRLF + ;
                         [      <LI>Area 2: <A href="/Area2/">CLICK</a>                 ] + CRLF + ;
                         [    </UL>                                                     ] + CRLF + ;
                         [    <HR>                                                      ] + CRLF + ;
                         [</BODY>                                                       ] + CRLF + ;
                         [</HTML>                                                       ]


      APPEND BLANK
      field->id       := 2
      field->name     := "Area1"
      field->parent   := 1
      field->mimetype := "text/html"
      field->content  := [<HTML>                                                         ] + CRLF + ;
                         [<HEAD>                                                         ] + CRLF + ;
                         [<TITLE>AREA 1</TITLE>                                          ] + CRLF + ;
                         [</HEAD>                                                        ] + CRLF + ;
                         [<BODY>                                                         ] + CRLF + ;
                         [<H2>INDEX area 1</H2>                                          ] + CRLF + ;
                         [<P>A <a href="test">text file</a> in this area                 ] + CRLF + ;
                         [</BODY>                                                        ] + CRLF + ;
                         [</HTML>                                                        ]

      APPEND BLANK
      field->id       := 3
      field->name     := "Area2"
      field->parent   := 1
      field->mimetype := "text/html"
      field->content  := [<HTML>                                                         ] + CRLF + ;
                         [<HEAD>                                                         ] + CRLF + ;
                         [<TITLE>Area 2</TITLE>                                          ] + CRLF + ;
                         [<BODY>                                                         ] + CRLF + ;
                         [<H2>INDEX Area 2</h2>                                          ] + CRLF + ;
                         [<P>A <a href="test1">text file</a> in this area.               ] + CRLF + ;
                         [</BODY>                                                        ] + CRLF + ;
                         [</HTML>                                                        ]

      APPEND BLANK
      field->id       := 4
      field->name     := "test"
      field->parent   := 1
      field->mimetype := "text/html"
      field->content  := [<HTML>                                                         ] + CRLF + ;
                         [<HEAD>                                                         ] + CRLF + ;
                         [<TITLE>A demo HTML</TITLE>                                     ] + CRLF + ;
                         [</HEAD>                                                        ] + CRLF + ;
                         [                                                               ] + CRLF + ;
                         [<BODY>                                                         ] + CRLF + ;
                         [<H2>A demo HTML</H2>                                           ] + CRLF + ;
                         [<P> Test page under main site                                  ] + CRLF + ;
                         [</BODY>                                                        ] + CRLF + ;
                         [</HTML>                                                        ]

      APPEND BLANK
      field->id       := 6
      field->name     := "test1"
      field->parent   := 3
      field->mimetype := "text/plain"
      field->content  := [Test Plain text page under area 2]

      APPEND BLANK
      field->id       := 7
      field->name     := "test"
      field->parent   := 2
      field->mimetype := "text/html"
      field->content  := [<HTML>                                                         ] + CRLF + ;
                         [<HEAD>                                                         ] + CRLF + ;
                         [<TITLE>A created page</TITLE>                                  ] + CRLF + ;
                         [</HEAD>                                                        ] + CRLF + ;
                         [<BODY>                                                         ] + CRLF + ;
                         [<H1>A created page</H1>                                        ] + CRLF + ;
                         [<P>Here it is a new page, created from the web!                ] + CRLF + ;
                         [</BODY>                                                        ] + CRLF + ;
                         [</HTML>                                                        ]

   ENDIF

RETURN








