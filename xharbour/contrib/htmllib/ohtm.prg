/*
 * $Id: ohtm.prg,v 1.1 2003/02/23 23:15:17 lculik Exp $
 */

/*
 * Harbour Project source code:
 * Main HTML CLASS for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "default.ch"
#include "hbclass.ch"
#include "html.ch"

STATIC saGreek := {}
STATIC snHtm   := NIL                   //0
STATIC scForm  := 0
STATIC soPage  := 0

/****
*
*     Class HTML()
*
*
*     Constructors :
*
*     Html():New()          Creates a new HTML document
*
*     Html():CGINew()       Creates a new CGI-HTML document
*
*/

// --- INIT --- //
   // --- HEADER --- //
   // --- END --- //
   // --- FONTS --- //
   // --- OUTPUT --- //
   // --- COSMETICS --- //
/*                             nSize  := IIF( nSize == NIL, 3, nSize ),;
                             nWidth := IIF( nWidth == NIL, 90, nWidth ),;
                             FWrite( ::nH, '<P>'+CRLF()+'<HR SIZE = '+NUMTRIM(nSize)+' WIDTH = '+NUMTRIM(nWidth)+'%>')*/
*/
   // --- URLs --- //
   // --- TABLES --- //
   // --- LISTS --- //
   // --- FORMS --- //
   // --- JAVA SUPPORT --- //
   // standard output
CLASS Html

   DATA nH
   DATA FName, TITLE
   DATA FontFace INIT "Verdana"         // Note!
   DATA FontSize INIT 1
   DATA FontColor INIT "black"
   DATA aImages
   DATA BaseURL, BaseTarget
   DATA lFont INIT .F.

   METHOD New( cFile, cTitle, cLinkTitle, cCharSet, cScriptSRC, ;
   BGIMAGE, BGCOLOR, txtColor, cJavaCode, ;
   onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
   cStyle, aimages, baseURL, baseTarget, ;
   nRefresh, cRefreshURL, cStyleScr, lnocache )
   METHOD CGINew( cTitle, cLinkTitle, cCharSet, cScriptSRC, bgImage, bgColor, txtColor, cJavaCode, onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, cStyle, aImages, aServerSrc, baseURL, baseTarget, nRefresh, cRefreshURL, cStyleScr, lnocache, nof, nMarginTop, nMarginHeight, nMarginWidth, nMarginLeft )

   /* METHOD Debug()         INLINE __clsDebug( self ) NOSELF */

   METHOD SetPageColor( cColor, lBody ) INLINE Fwrite( ::nH, IIF( lBody, '<BODY BGCOLOR="' + cColor + '">', ' BGCOLOR="' + cColor + '" ' ) )

   METHOD SetTextColor( cColor, lBody ) INLINE Fwrite( ::nH, IIF( lBody, '<BODY TEXT="' + cColor + '">', ' TEXT="' + cColor + '" ' ) )

   METHOD SetBgImage( cImage, lBody ) INLINE Fwrite( ::nH, IIF( lBody, '<BODY BACKGROUND="' + cImage + '">', ' BACKGROUND="' + cImage + '" ' ) )

   METHOD CLOSE()

   METHOD CGIClose()

   METHOD SetCenter( lOn ) INLINE Fwrite( ::nH, IIF( lOn, "<CENTER>", "</CENTER>" ) )

   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )

   METHOD ENDFONT()

   METHOD SAY( str, font, size, type, color, style )

   METHOD Qqout( c ) INLINE DEFAULT( c, "" ),;
   Fwrite( ::nH, c )

   METHOD Qout( c ) INLINE DEFAULT( c, "" ),;
    Fwrite( ::nH, CRLF() + c + '<BR>' + CRLF() )

   METHOD Write( c ) INLINE DEFAULT( c, "" ),;
      Fwrite( ::nH, c )

   METHOD WriteLN( c ) INLINE DEFAULT( c, "" ), ;
          Fwrite( ::nH, CRLF() + c + '<BR>' + CRLF() )

   METHOD SayColor( t, c ) INLINE DEFAULT( t, "" ), DEFAULT( c, "black" ),;
          Fwrite( ::nH, '<FONT COLOR="' + c + '">' + t + '</FONT>' )

   METHOD Space( n ) INLINE DEFAULT( n, 1 ), Fwrite( ::nH, Replicate( "&nbsp;", n ) )

   METHOD PutImage( cImage, nBorder, nHeight, ;
   cOnclick, cOnMsOver, cOnMsOut, ;
   cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALING, HSPACE )

   METHOD TEXT( cText, nCols, lWrap ) INLINE DEFAULT( lWrap, .T. ),;
   DEFAULT( nCols, 80 ),;
   Fwrite( ::nH, "<PRE" + IIF( nCols != NIL, ' COLS="' + NUMTRIM( nCols ) + "'", "" ) + IIF( lWrap, " WRAP>", ">" ) + CRLF() + cText + CRLF() + "</PRE>" + CRLF() )

   METHOD MultiCol( txt, cols, gutter, width ) INLINE DEFAULT( txt, "" ),;
   DEFAULT( cols, 2 ),;
   DEFAULT( gutter, 5 ),;
   DEFAULT( width, 100 ),;
   Fwrite( ::nH, '<MULTICOL COLS="' + NUMTRIM( cols ) + '" GUTTER="' + NUMTRIM( gutter ) + '" WIDTH="' + NUMTRIM( width ) + '">' ),;
   Fwrite( ::nH, txt ),;
   Fwrite( ::nH, "</MULTICOL>" )

   METHOD PutHeading( cText, nWeight, lCentered )

   METHOD HLine( nSize, nWidth, lShade, cColor )

   METHOD PutParagraph() INLINE Fwrite( ::nH, "<P> </P>" + CRLF() )

   METHOD Paragraph( l, c, style )

   METHOD PutBreak() INLINE Fwrite( ::nH, "<BR>" + CRLF() )

   METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
   cAlign, nWidth, nHeight, cbgColor, ;
   cBehavior, cDirection, ;
   nScrollAmt, nScrollDelay, LOOP, ;
   onMsOver, onMsOut, onClick, onStart, onFinish )

   METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
   cAlign, nWidth, nHeight, cbgColor, ;
   cBehavior, cDirection, ;
   nScrollAmt, nScrollDelay, LOOP, ;
   onMsOver, onMsOut, onClick, onStart, onFinish )
   METHOD EndMarquee()

   METHOD PutTextUrl( cText, cUrl, cOnClick, cOmMsOver, cOnMsout, cTarget,  font, clr, size, style, bld, lbreak, cClass )

   METHOD PutImageUrl( cImage, nBorder, nHeight, nWidth, cUrl, ;
   cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ALING )

   METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
   l3d, lRuleCols, lRuleRows, ;
   cClrDark, cClrLight, ncellpadding, ncellspacing, ;
   cAling, lRules, BGIMAGE, cStyle, ID, NOF )

   METHOD TableHead( cHead, cColor, cAlign, ;
   cFont, nSize, cFntColor, nHeight, cBgPic )

   METHOD NewTableRow( cColor )

   METHOD EndTableRow()

   METHOD NewTableCell( cAlign, cColor, ;
   cFont, nSize, cFntColor, nHeight, ;
   cBgPic, ;
   nWidth, lWrap, ;
   nCSpan, nRSpan, cValing, clrdrk, clrlt, cBdrClr, cclass )

   METHOD EndTableCell( lFont )

   METHOD EndTable()

   METHOD newList() INLINE Fwrite( ::nH, "<UL>" + CRLF() )

   METHOD ListItem() INLINE Fwrite( ::nH, "<LI> " )

   METHOD EndList() INLINE Fwrite( ::nH, "</UL> " )

   METHOD NewForm( cMethod, cAction, cName )

   METHOD FormImage( cText, name, File )

   METHOD FormGet( cType, cName, xValue, nSize )

   METHOD FormReset( c )

   METHOD FormSubmit( c )

   METHOD FormQOut( c ) INLINE Fwrite( ::nH, c + '<BR>' + CRLF() )

   METHOD FormQQOut( c ) INLINE Fwrite( ::nH, c + CRLF() )

   METHOD EndForm() INLINE Fwrite( ::nH, CRLF() + "</FORM>" + CRLF() )

   METHOD Pushbutton( cName, cCaption, ;
   cCgiApp, ;
   cOnClick, ;
   cOnFocus, cOnBlur, ;
   cOnMsOver, cOnMsOut, ;
   style, ID )

   METHOD endButton()

   METHOD Button( cName, cCaption, ;
   cOnClick, cCGIApp, ;
   cOnMsOver, cOnMsOut, ;
   style, ID )

   METHOD iFrame( name, src, border, ;
   marginwidth, marginheight, ;
   scrolling, allign, ;
   WIDTH, HEIGHT )

   METHOD StartJava() INLINE ;
   Fwrite( ::nH, '<SCRIPT LANGUAGE="JavaScript">' + CRLF() + ;
   "<!--" + CRLF() )

   METHOD PutJavaSource( c ) INLINE Fwrite( ::nH, Space( 5 ) + 'SRC="' + c + '"' + CRLF() )

   METHOD PutJava( c ) INLINE Fwrite( ::nH, Space( 5 ) + c + CRLF() )

   METHOD EndJava() INLINE Fwrite( ::nH, "                  //-->" + CRLF() + ;
   "</SCRIPT>" + CRLF() )

   METHOD serverCode( c ) INLINE Fwrite( ::nH, "<SERVER>" + ;
   Space( 9 ) + c + CRLF() + ;
   "</SERVER>" + CRLF() )

   METHOD Fwrite( c ) INLINE ;
   Fwrite( ::nH, c )
   METHOD FWriteLN( c ) INLINE ;
   Fwrite( ::nH, c + CRLF() )

   METHOD Span( c, Style )

   /* NEW  COMMANDS */
   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
   cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )
   METHOD Comment( cText )

   METHOD ADDoBJECT( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight )

   METHOD ADDPARAM( cName, cValue )

   METHOD EndOBJect()

   METHOD PutLinkName( cName )

   /* MAP SUPPORT */

   METHOD ENDMAP() INLINE ;
   Fwrite( ::nH, "</MAP>" )
   METHOD NewMap( cName ) INLINE ;
   Fwrite( ::nH, "<MAP NAME=" + cName + ">" )

   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
   Fwrite( ::nH, "<AREA  shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + CRLF() )
ENDCLASS

   /****
*
*     Html():New()
*
*     Starts a new HTML disk file.
*/

METHOD New( cFile, cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
               BGIMAGE, BGCOLOR, txtColor, aJavaCode, ;
               onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
               cStyle, aImages, cBaseURL, cBaseTarget, ;
               nRefresh, cRefreshURL, cStyleScr, lnocache ) CLASS Html

   LOCAL i

   DEFAULT cFile := "file1.htm"
   DEFAULT cTitle := "test HTML page"
   DEFAULT cLinkTitle := cTitle
   DEFAULT cRefreshURL := ""
   DEFAULT cCharset := "windows-1253"

   ::nH    := Fcreate( cFile )
   ::Title := cTitle
   ::FName := cFile

   Fwrite( ::nH, '<HTML>' + CRLF() + ;
           '<HEAD>' + CRLF() + ;
           '   <TITLE>' + cTitle + ' </TITLE>' + CRLF() )

   IF cBaseURL != NIL

      Fwrite( ::nH, "<BASE HREF='" + cBaseURL + "'" )

      IF cBaseTarget != NIL
         Fwrite( ::nH, " TARGET='" + cBaseTarget + "'" )
      ENDIF

      Fwrite( ::nH, ">" + CRLF() )
   ENDIF

   IF cStyleScr != NIL
      Fwrite( ::nH, '   <LINK HREF="' + cStyleScr + '"' + "  rel='STYLESHEET' type='text/css'>" + CRLF() )
   ENDIF

   Fwrite( ::nH, '   <LINK TITLE="' + cLinkTitle + '"' + CRLF() + ;
           '                HREF="mailto:culik@sl.conex.net" >' + CRLF() + ;
           '   <META HTTP-EQUIV="Content-Type" content="text/html; charset=' + cCharset + '">' + CRLF() )

   IF nRefresh != NIL
      Fwrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="] + NUMTRIM( nRefresh ) + [; URL=] + cRefreshURL + [">] )
   ENDIF

   IF lnocache
      Fwrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ] )
   ENDIF

   IF aJavaCode != NIL
      Aeval( aJavaCode, { | e | JavaCMD( ::nH, e ) } )
   ENDIF

   IF aScriptSrc != NIL

      FOR i := 1 TO Len( aScriptSrc )
         Fwrite( ::nH, ;
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aScriptSrc[ i ] + '"></SCRIPT>' + CRLF() )
      NEXT

   ENDIF

   // preload images...
   IF aImages != NIL
      ::aImages := aImages
      Fwrite( ::nH, ;
              '<SCRIPT LANGUAGE="JavaScript">' + CRLF() )
      Fwrite( ::nH, '<!--' + CRLF() )
      Fwrite( ::nH, "if(document.images)" + CRLF() )
      Fwrite( ::nH, "{" + CRLF() )

      FOR i := 1 TO Len( aImages )
         Fwrite( ::nH, Space( 5 ) + aImages[ i, 1 ] + "=new Image(100,50);" + CRLF() )
         Fwrite( ::nH, Space( 5 ) + aImages[ i, 1 ] + '.src="' + aImages[ i, 2 ] + '";' + CRLF() )
      NEXT

      Fwrite( ::nH, "}" + CRLF() )

      Fwrite( ::nH, "" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + [// Function to 'activate' images.] + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imageOn(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgOn=eval(imgName + "on.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgOn;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, Space( 5 ) + "// Function to 'deactivate' images." + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imageOff(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgOff = eval(imgName + "off.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgOff;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, Space( 5 ) + "// Function for 'pressed' images." + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imagePress(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgPress = eval(imgName + "press.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgPress;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, '//-->' + CRLF() )
      Fwrite( ::nH, '</SCRIPT>' + CRLF() )

   ENDIF

   IF cStyle != NIL
      Fwrite( ::nH, "<STYLE> " + cStyle + " </STYLE>" + CRLF() )
   ENDIF

   Fwrite( ::nH, ;
           CRLF() + '</HEAD>' + ;
           CRLF() + '<BODY' )

   IF onLoad != NIL
      Fwrite( ::nH, '   onLoad="' + onLoad + '"' )
   ENDIF

   IF onUnLoad != NIL
      Fwrite( ::nH, ' onUnload="' + onUnLoad + '"' )
   ENDIF

   Fwrite( ::nH, '>' )
   Fwrite( ::nH, CRLF() )

   IF BGIMAGE != NIL
      ::SetBgImage( bgImage )
   ENDIF

   IF BGCOLOR != NIL
      ::SetPageColor( bgColor )
   ENDIF

   IF txtColor != NIL
      ::SetTextColor( txtColor )
   ENDIF

   snHtm := ::nH

   soPage := Self

RETURN self

/****
*
*     Html():CGINew()
*
*     Starts a new CGI-HTML stream file.
*/

METHOD CGINew( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
                  BGIMAGE, BGCOLOR, txtColor, aJavaCode, ;
                  onLoad, onUnload, ;
                  cLinkClr, cVLinkClr, cALinkClr, ;
                  cStyle, aImages, aServerSrc, ;
                  cBaseURL, cBaseTarget, ;
                  nRefresh, cRefreshURL, cStyleScr, ;
                  lNocache, NOF, nMarginTop, nMarginHeight, ;
                  nMarginWidth, nMarginLeft ) CLASS Html

   LOCAL i

   //DEFAULT lAuthenticate := .F.
   DEFAULT cTitle := "CGI HTML page"
   DEFAULT cLinkTitle := cTitle
   DEFAULT cRefreshURL := ""
   DEFAULT cCharset := "windows-1253"
   DEFAULT lNocache := .f.
   ::nH    := STD_OUT                   //FCreate( cFile )
   ::Title := cTitle
   ::FName := "CGIOUT.HTM"

   Fwrite( ::nH, 'Content-Type: text/html' + CRLF() + CRLF() )

   /*
IF lAuthenticate == .T. .and. ;
   ( EMPTY(GetEnv( "AUTH_USER" )) .OR. EMPTY( GetEnv( "AUTH_PASS" )) )
   FWRITE( ::nH,"<HTML><HEAD</HEAD><BODY>"+CRLF() )
   FWRITE( ::nH,"HTTP/1.0 401 Not Authorized"+CRLF() )
   FWRITE( ::nH,'WWW-Authenticate:Basic Realm="'+cTitle+'"'+CRLF() )
   FWRITE( ::nH,"</BODY></HTML>"+CRLF() )
   FClose( ::nH )
   RETURN Self
ENDIF
*/

   Fwrite( ::nH, '<HTML>' + CRLF() + ;
           '<HEAD>' + CRLF() + ;
           '   <TITLE>' + cTitle + ' </TITLE>' + CRLF() )

   IF cBaseURL != NIL
      Fwrite( ::nH, "<BASE HREF='" + cBaseURL + "'" )

      IF cBaseTarget != NIL
         Fwrite( ::nH, " TARGET='" + cBaseTarget + "'" )
      ENDIF

      Fwrite( ::nH, ">" + CRLF() )
   ENDIF

   Fwrite( ::nH, '   <LINK TITLE="' + cLinkTitle + '"' + CRLF() + ;
           '                HREF="mailto:culik@sl.conex.net" >' + CRLF() + ;
           '   <META HTTP-EQUIV="Content-Type" content="text/html; charset=' + cCharset + '">' + CRLF() )

   IF cStyleScr != NIL
      Fwrite( ::nH, '   <LINK HREF="' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css'>" + CRLF() )
   ENDIF

   IF nRefresh != NIL
      Fwrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="] + NUMTRIM( nRefresh ) + [; URL=] + cRefreshURL + [">] )
   ENDIF

   IF lnocache
      Fwrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ] )
   ENDIF

   IF aJavaCode != NIL
      Aeval( aJavaCode, { | e | JavaCMD( ::nH, e ) } )
   ENDIF

   IF aScriptSrc != NIL

      FOR i := 1 TO Len( aScriptSrc )
         Fwrite( ::nH, ;                // RUNAT=SERVER
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aScriptSrc[ i ] + '"></SCRIPT>' + CRLF() )
      NEXT

   ENDIF

   IF aServerSrc != NIL

      FOR i := 1 TO Len( aServerSrc )
         Fwrite( ::nH, ;                // RUNAT=SERVER
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aServerSrc[ i ] + '" RUNAT=SERVER></SCRIPT>' + CRLF() )
      NEXT

   ENDIF

   // preload images...
   IF aImages != NIL
      ::aImages := aImages
      Fwrite( ::nH, ;
              '<SCRIPT LANGUAGE="JavaScript">' + CRLF() )
      Fwrite( ::nH, '<!--' + CRLF() )
      Fwrite( ::nH, "if(document.images)" + CRLF() )
      Fwrite( ::nH, "{" + CRLF() )
      FOR i := 1 TO Len( aImages )
         Fwrite( ::nH, Space( 5 ) + aImages[ i, 1 ] + "=new Image(100,50);" + CRLF() )
         Fwrite( ::nH, Space( 5 ) + aImages[ i, 1 ] + '.src="' + aImages[ i, 2 ] + '";' + CRLF() )
      NEXT
      Fwrite( ::nH, "}" + CRLF() )

      Fwrite( ::nH, "" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + [// Function to 'activate' images.] + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imageOn(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgOn=eval(imgName + "on.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgOn;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, Space( 5 ) + "// Function to 'deactivate' images." + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imageOff(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgOff = eval(imgName + "off.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgOff;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, Space( 5 ) + "// Function for 'pressed' images." + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "function imagePress(imgName) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        if (document.images) {" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            imgPress = eval(imgName + "press.src");' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + '            document[imgName].src = imgPress;' + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "        }" + CRLF() )
      Fwrite( ::nH, Space( 5 ) + "}" + CRLF() )
      Fwrite( ::nH, CRLF() )
      Fwrite( ::nH, '//-->' + CRLF() )
      Fwrite( ::nH, '</SCRIPT>' + CRLF() )

   ENDIF

   IF cStyle != NIL
      Fwrite( ::nH, "<STYLE> " + cStyle + " </STYLE>" + CRLF() )
   ENDIF

   Fwrite( ::nH, ;
           '</HEAD>' + CRLF() + ;
           '<BODY' )

   IF onLoad != NIL
      Fwrite( ::nH, '   onLoad="' + onLoad + '"' )
   ENDIF

   IF NOF != NIL
      Fwrite( ::nH, '   NOF="' + nof + '"' )
   ENDIF

   IF onUnLoad != NIL
      Fwrite( ::nH, ' onUnload="' + onUnLoad + '"' )
   ENDIF

   IF cLinkClr != NIL
      Fwrite( ::nH, ' link="' + cLinkClr + '"' )
   ENDIF

   IF cVLinkClr != NIL
      Fwrite( ::nH, ' vlnk="' + cVLinkClr + '"' )
   ENDIF

   IF cALinkClr != NIL
      Fwrite( ::nH, ' alink="' + cALinkClr + '"' )
   ENDIF

   IF BGIMAGE != NIL
      ::SetBgImage( bgImage, .F. )
   ENDIF

   IF BGCOLOR != NIL
      ::SetPageColor( bgColor, .F. )
   ENDIF

   IF txtColor != NIL
      ::SetTextColor( txtColor, .F. )
   ENDIF

   IF nMarginTop != NIL
      Fwrite( ::nH, ' topMargin=' + NUMTRIM( nMarginTop ) )
   ENDIF

   IF nMarginLeft != NIL
      Fwrite( ::nH, ' LeftMargin=' + NUMTRIM( nMarginLeft ) )
   ENDIF

   IF nMarginHeight != NIL
      Fwrite( ::nH, ' MARGINHEIGHT=' + NUMTRIM( nMarginHeight ) )
   ENDIF

   IF nMarginWidth != NIL
      Fwrite( ::nH, ' MARGINWIDTH=' + NUMTRIM( nMarginWidth ) )
   ENDIF

   Fwrite( ::nH, '>' )

   Fwrite( ::nH, CRLF() )

   snHtm := ::nH

   soPage := Self

RETURN self

/****
*
*     Html():SetFont()
*
*     obvious...
*/

METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS Html

   LOCAL cStr := CRLF() + '<FONT'

   DEFAULT cFont := ::fontFace
   DEFAULT nSize := ::fontSize
   DEFAULT cColor := ::fontColor
   DEFAULT lset := IIF( cFont != NIL, .t., .f. )

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' COLOR= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF lBold != NIL
      IIF( lBold, cStr += '<B>', cStr += '</B>' )
   ENDIF

   IF lItalic != NIL
      IIF( lItalic, cStr += '<I>', cStr += '</I>' )
   ENDIF

   IF lULine != NIL
      IIF( lULine, cStr += '<U>', cStr += '</U>' )
   ENDIF

   cStr += '</FONT>'
   Fwrite( ::nH, cStr + CRLF() )
RETURN Self

/****
*
*     Html():StartFont()
*
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut ) CLASS Html

   LOCAL cStr := "<FONT "
   DEFAULT lSet := .t.
   DEFAULT lPut := .f.
   DEFAULT cFont := ::fontFace
   DEFAULT nSize := ::fontSize
   DEFAULT cColor := ::fontColor

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF lPut

      IF nSize != NIL
         cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

         IF lSet
            ::fontSize := nSize
         ENDIF

      ENDIF

      IF cColor != NIL
         cStr += ' COLOR= "' + cColor + '">'

         IF lSet
            ::fontColor := cColor
         ENDIF

      ELSE
         cStr += ">"
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF lBold != NIL
      IIF( lBold, cStr += '<B>', cStr += '</B>' )
   ENDIF

   IF lItalic != NIL
      IIF( lItalic, cStr += '<I>', cStr += '</I>' )
   ENDIF

   IF lULine != NIL
      IIF( lULine, cStr += '<U>', cStr += '</U>' )
   ENDIF

   Fwrite( ::nH, cStr + CRLF() )

RETURN Self

/****
*
*     Html():DefineFont()
*
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS Html

   LOCAL cStr := "<FONT "

   DEFAULT cFont := ::fontFace
   DEFAULT nSize := ::fontSize
   DEFAULT cColor := ::fontColor
   DEFAULT lset := IIF( cFont != NIL, .t., .f. )

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' COLOR= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF cType != NIL
      cStr += cType
   ENDIF

   Fwrite( ::nH, cStr + CRLF() )

RETURN Self

/****
*
*     Html():EndFont()
*
*     End a font definition
*/

METHOD ENDFONT() CLASS Html

   Fwrite( ::nH, '</font>' + CRLF() )

RETURN Self

/****
*
*     Html():say()
*
*
*
*/

METHOD SAY( str, font, size, type, color, style ) CLASS Html

   LOCAL cOut    := ""
   LOCAL lBold   := .F.
   LOCAL lItalic := .F.
   LOCAL lULine  := .F.
   LOCAL lEm     := .f.
   LOCAL lStrong := .f.
   LOCAL nSize   := Size
   DEFAULT str := ""
   DEFAULT FONT := ::FontFace
   DEFAULT size := ::FontSize
   DEFAULT COLOR := ::FontColor

   IF FONT != NIL .or. Size != NIL .or. COLOR != NIL
      cOut := '<FONT ' + IIF( font != NIL, 'FACE="' + font + '"', '' ) + IIF( color != NIL, ' COLOR=' + color, '' ) + IIF( nSize != NIL, ' SIZE=' + NUMTRIM( size ), "" )

      IF Style != NIL
         cOut += '" Style="' + style + '">'
      ELSE
         cOut += '>'
      ENDIF

   ENDIF

   IF Valtype( type ) == "C"

      IF "<" $ type

         IF "<B>" $ type
            lBold := .T.
            cOut  += "<B>"
         ENDIF

         IF "<I>" $ type
            lItalic := .T.
            cOut    += "<I>"
         ENDIF

         IF "<U>" $ type
            lULine := .T.
            cOut   += "<U>"
         ENDIF

         IF "<EM>" $ type
            lEm  := .T.
            cOut += "<EM>"
         ENDIF

         IF "<STRONG>" $ type
            lStrong := .T.
            cOut    += "<STRONG>"
         ENDIF

      ENDIF

   ENDIF

   cOut += str

   IF lBold
      cOut += "</B>"
   ENDIF

   IF lItalic
      cOut += "</I>"
   ENDIF

   IF lULine
      cOut += "</U>"
   ENDIF

   IF lStrong
      cOut += "</STRONG>"
   ENDIF

   IF lEm
      cOut += "</EM>"
   ENDIF

   IF FONT != NIL .or. Size != NIL .or. COLOR != NIL
      cOut += "</FONT>"
   ENDIF

   Fwrite( ::nH, cOut + CRLF() )

RETURN Self

/****
*
*     Html():paragraph()
*
*
*
*/

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS Html

   LOCAL cStr := "<P"

   DEFAULT( lStart, .T. )
   DEFAULT( cAlign, "LEFT" )

   IF lStart
      cStr := "<P ALIGN='" + cAlign + "'"

      IF cStyle != NIL
         cStr += ' STYLE="' + cStyle + '"'
      ENDIF

      cStr += ">"
   ELSE
      cStr := "</P>"
   ENDIF

   cStr += CRLF()
   Fwrite( ::nH, cStr )
RETURN Self

/****
*
*     Html():HLine()
*
*     Put a Horizontal line
*/

METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS Html

   DEFAULT nSize := 3
   DEFAULT nWidth := 100
   DEFAULT lShade := .T.

   IF lShade
      Fwrite( ::nH, CRLF() + ;
              '<HR SIZE = ' + NUMTRIM( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + NUMTRIM( nWidth ) + '%>' + ;
              CRLF() )
   ELSE
      Fwrite( ::nH, CRLF() + ;
              '<HR NOSHADE SIZE = ' + NUMTRIM( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + NUMTRIM( nWidth ) + '%>' + ;
              CRLF() )
   ENDIF

RETURN Self

/****
*
*     Html():PutHeading()
*
*     Put an HTML heading ( large text )
*/

METHOD PutHeading( cText, nWeight, lCentered ) CLASS Html

   DEFAULT nWeight := 3
   DEFAULT lCentered := .F.

   IF lCentered
      Fwrite( ::nH, "<CENTER>" )
   ENDIF

   Fwrite( ::nH, "<H" + NUMTRIM( nWeight ) + ">" + cText + "</H" + NUMTRIM( nWeight ) + ">" + CRLF() )

   IF lCentered
      Fwrite( ::nH, "</CENTER>" )
   ENDIF

RETURN Self

/****
*
*     Html():putTextURL()
*
*     Put a text link.
*/

METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass ) CLASS Html

   LOCAL cStr := ""
   DEFAULT cUrl := ""
   DEFAULT bld := .F.

   Fwrite( ::nH, ;
           '<A HREF="' + cUrl + '"' + crlf() )

   IF cOnClick != NIL
      Fwrite( ::nH, ;
              Space( 5 ) + 'onClick="' + cOnClick + '"' + CRLF() )
   ENDIF
   IF cOnMsOver != NIL
      Fwrite( ::nH, ;
              Space( 5 ) + 'onMouseOver="' + cOnMsOver + '"' + CRLF() )
   ENDIF
   IF cOnMsOut != NIL
      Fwrite( ::nH, ;
              Space( 5 ) + 'onMouseOut="' + cOnMsOut + '"' + CRLF() )
   ENDIF

   IF cTarget != NIL
      Fwrite( ::nH, ;
              Space( 5 ) + 'TARGET=' + cTarget + CRLF() )
   ENDIF

   IF cClass != NIL
      Fwrite( ::nH, ;
              Space( 5 ) + 'CLASS=' + cClass + CRLF() )
   ENDIF

   IF bld
      cStr += "<B>" + CRLF()
   ENDIF

   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      //    cStr +=" Font" +valtype(font)+"color"+valtype(clr)+"size"+valtype(size)+"style"+valtype(style)
      cStr += " <FONT " + CRLF()

      IF FONT != NIL
         cStr += ' face="' + FONT + '"'
      ENDIF

      IF clr != NIL
         cStr += ' color=' + clr
      ENDIF

      IF size != NIL
         cStr += ' size=' + NUMTRIM( size )
      ENDIF

      IF style != NIL
         cStr += ' style="' + style + '"'
      ENDIF

   ENDIF

   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      cStr += '>' + cText
   ELSE
      cStr += cText
   ENDIF

   Fwrite( ::nH, ;
           '>' + cStr )
   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      Fwrite( ::nH, ;
              '</font>' )
   ENDIF

   IF bld
      Fwrite( ::nH, ;
              '</B>' )
   ENDIF

   Fwrite( ::nH, ;
           '</A>' + IIF( lBreak, '<br>' + CRLF(), CRLF() ) )

RETURN Self

/****
*
*     Html():putImageURL()
*
*     Put an Image link.
*/

METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
                       cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
                       Id, hSpace, Aling ) CLASS Html
   LOCAL cStr := ""

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"' + CRLF()
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + IIF( Valtype( nBorder ) == "N", NUMTRIM( nBorder ), nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " height = " + NUMTRIM( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NUMTRIM( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " width = " + nWidth + " " + CRLF()
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"' + CRLF()
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF

   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF cTarget != NIL
      cStr += ' TARGET=' + cTarget + CRLF()
   ENDIF

   IF Id != NIL
      cstr += " id=" + Id
   ENDIF

   IF Aling != NIL
      cStr += ' align="' + Aling + '"'
   ENDIF

   IF hSpace != NIL
      cStr += " hSpace= " + NUMTRIM( hSpace ) + " "
   ENDIF

   Fwrite( ::nH, ;
           '<A HREF=' + cUrl + IIF( cClass != NIL, ' class="' + cClass + '"', "" ) + '><IMG SRC="' + cImage + '"' + ;
           cStr + '></A>' + IIF( lBreak, '<br>' + CRLF(), "" ) )

RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
                           cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS Html
   LOCAL cStr := ""

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + NUMTRIM( nBorder )
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " height = " + NUMTRIM( nHeight ) + " "
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " "
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NUMTRIM( nWidth ) + " "
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " width = " + nWidth + " "
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"'
   ENDIF

   IF cTarget != NIL
      cStr += ' TARGET=' + cTarget
   ENDIF

   Fwrite( ::nH, ;
           '<A HREF=' + cUrl + IIF( cClass != NIL, ' class="' + cClass + '"', "" ) + '>' + cText + '<IMG SRC="' + cImage + '"' + ;
           cStr + '></A>' + IIF( lBreak, '<br>' + CRLF(), "" ) )

RETURN Self

/****
*
*     Html():putImage()
*
*     Put an Image.
*/

METHOD PutImage( cImage, nBorder, nHeight, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, Id, Map, Aling, hSpace ) CLASS Html
   LOCAL cStr := ""

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL .and. Valtype( nBorder ) == "N"
      cStr += " BORDER = " + NUMTRIM( nBorder )
   ELSEIF nBorder != NIL .and. Valtype( nBorder ) == "C"
      cStr += " BORDER = " + '"' + nBorder + '"'
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " HEIGHT = " + NUMTRIM( nHeight ) + " "
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " HEIGHT = " + '"' + nHeight + '"'
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NUMTRIM( nWidth ) + " "
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " width = " + nWidth + " "
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"'
   ENDIF

   IF Map != NIL
      cStr += " usemap=" + Map
   ENDIF

   IF cTarget != NIL
      cStr += ' TARGET="' + cTarget + '"'
   ENDIF

   IF Id != NIL
      cstr += " id=" + Id
   ENDIF

   IF Aling != NIL
      cStr += ' align="' + Aling + '"'
   ENDIF

   IF hSpace != NIL
      cStr += " hSpace= " + NUMTRIM( hSpace ) + " "
   ENDIF

   Fwrite( ::nH, ;
           '<IMG SRC="' + cImage + '"' + ;
           cStr + '>' + IIF( lBreak, '<br>' + CRLF(), "" ) )

RETURN Self

/****
*
*     Html():Close()
*
*     Close an HTML disk file
*
*/

METHOD CLOSE() CLASS Html

   Fwrite( ::nH, "</body>" + CRLF() )
   Fwrite( ::nH, "</html>" + CRLF() )
   Fclose( ::nH )
RETURN Self

/****
*
*     Html():CGIClose()
*
*     Close a CGI-HTML stream file
*/

METHOD cgiClose() CLASS Html

   Fwrite( ::nH, "</body>" + CRLF() )
   Fwrite( ::nH, "</html>" + CRLF() )
   Fwrite( ::nH, CRLF() )
RETURN Self

/****
*
*     Html():defineTable()
*
*     Start an HTML table definition.
*
*
*/

METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
                       l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
                       nCellPadding, nCellSpacing, cAling, lRules, ;
                       bgImage, cStyle, Id, NOF ) CLASS Html

   LOCAL cStr  := /*"<!-- Table Definition -->"+*/ CRLF() + CRLF() + "<TABLE "
   LOCAL xCols := nCols
   /*DEFAULT ColorBG   := "#9196A0"  //CLR_WHITE
DEFAULT nCols     := 1
DEFAULT nWidth    := 100
DEFAULT nBorder   := 1
*/
   DEFAULT l3d := .T.
   DEFAULT lRuleCols := .F.
   DEFAULT lRuleRows := .F.

   IF ColorFore != NIL
      cStr += " bordercolor=" + ColorFore + ' '
   ENDIF

   IF Colorbg != NIL
      cStr += " bgcolor=" + ColorBG + ' '
   ENDIF

   cStr += IIF( nBorder = NIL, "border ", "border=" + NUMTRIM( nBorder ) + ' ' )
   // ??? --> cStr += "frame=ALL "
   IF ncellpadding != NIL
      cStr += ' CellPadding=' + NUMTRIM( nCellPadding )
   ENDIF

   IF nCellSpacing != NIL
      cStr += ' CellSpacing=' + NUMTRIM( nCellSpacing )
   ENDIF

   IF cAling != NIL
      cStr += ' aling=' + '"' + cAling + '"'
   ENDIF

   //cStr += "rowspan = 1 "    + CRLF()
   //cStr += "colspan = 1 "    + CRLF()
   cStr += IIF( xCols != NIL, " COLS=" + NUMTRIM( nCols ), "" )

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " WIDTH=" + NUMTRIM( nWidth )
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " HEIGHT=" + NUMTRIM( nHeight )
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " HEIGHT=" + '"' + nHeight + '"'
   ENDIF

   IF l3d
      cStr += ' bordercolorlight=#000000 ' + ;
         ' bordercolordark=#FFFFFF '
   ENDIF

   IF cClrDark != NIL
      cStr += ' bordercolordark=' + cClrDark
   ENDIF

   IF cClrLight != NIL
      cStr += ' bordercolorlight=' + cClrLight
   ENDIF

   IF cClrBorder != NIL
      cStr += ' bordercolor=' + cClrBorder
   ENDIF

   IF lRuleCols == .T.
      cStr += " RULES=COLS"
   ELSEIF lRuleRows == .T.
      cStr += " RULES=ROWS"
   ELSEIF lRules == .T.
      cStr += " RULES=ALL"
   ENDIF

   IF bgImage != NIL
      cStr += ' background="' + bgImage + '" '
   ENDIF
   IF cStyle != NIL
      cStr += ' style ="' + cStyle + '" '
   ENDIF

   IF Id != NIL
      cStr += ' id=' + Id
   ENDIF

   IF NOF != NIL
      cStr += ' NOF="' + NOF + '"'
   ENDIF

   cStr += ">" + CRLF()

   // rules=cols
   // rules=rows
   // rules=all

   Fwrite( ::nH, cStr + CRLF() )

RETURN Self

/****
*
*     Html():tableHead()
*
*     Define a table column Header.
*
*/

METHOD TableHead( cHead, cColor, cAlign, ;
                     cFont, nSize, cFntColor, nHeight, cBgPic ) CLASS Html

   LOCAL cStr := Space( 3 ) + "<TH"

   DEFAULT cFont := ::fontFace
   DEFAULT nSize := ::fontSize
   DEFAULT cFntColor := ::fontColor

   IF cColor != NIL
      cStr += " bgcolor=" + '"' + cColor + '"'
   ENDIF

   IF cAlign != NIL
      cStr += " align=" + '"' + cAlign + '"'
   ENDIF

   IF nHeight != NIL
      cStr += " height=" + '"' + NUMTRIM( nHeight ) + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   cStr += ">"

   IF cFont != NIL
      cStr += '<font face="' + cFont + '"'

      IF nSize != NIL
         cStr += ' size="' + NUMTRIM( nSize ) + '"'
      ENDIF

      IF cFntColor != NIL
         cStr += ' color="' + cFntColor + '"'
      ENDIF

      cStr += ">"
   ENDIF

   cStr += cHead + IIF( cFont != NIL, '</font>', "" ) + "</th>" + CRLF()
   //cStr += cHead + '</font>'+"</th>"+CRLF()

   Fwrite( ::nH, cStr )

RETURN Self

/****
*
*     Html():newTableRow()
*
*     Start a table row definition.
*
*/

METHOD NewTableRow( cColor, vAling, aLing ) CLASS Html

   LOCAL cStr := Space( 5 ) + "<TR"

   IF cColor != NIL
      cStr += " bgcolor=" + cColor
   ENDIF

   IF vAling != NIL
      cStr += " vAling=" + vAling
   ENDIF

   IF ALING != NIL
      cStr += " Aling=" + ALING
   ENDIF

   cStr += ">" + CRLF()
   Fwrite( ::nH, cStr )
RETURN Self

/****
*
*     Html():endTableRow()
*
*     End a table row definition.
*
*/

METHOD EndTableRow() CLASS Html

   Fwrite( ::nH, Space( 5 ) + "</TR>" + CRLF() )
RETURN Self

/****
*
*     Html():newTableCell()
*
*     Start a table cell definition.
*
*/

METHOD NewTableCell( cAlign, cColor, ;
                        cFont, nSize, cFntColor, nHeight, ;
                        cBgPic, nWidth, lWrap, ;
                        nColspan, nRowspan, cValign, clrdrk, clrlt, cBdrClr, cClass, lNoFont ) CLASS Html

   LOCAL cStr := Space( 10 ) + "<TD"
   LOCAL cAli := cAlign
   DEFAULT lNoFont := .T.
   DEFAULT cFont := ::fontFace
   DEFAULT nSize := ::fontSize
   DEFAULT cFntColor := ::fontColor
   DEFAULT cAlign := "LEFT"
   DEFAULT lWrap := .T.

   IF cBdrClr != NIL
      cStr += " BORDERCOLOR=" + cBdrClr
   ENDIF

   IF cColor != NIL
      cStr += " BGCOLOR=" + cColor
   ENDIF

   IF cAlign != NIL .and. caLi != NIL
      cStr += " ALIGN=" + cAlign
   ENDIF

   IF cValign != NIL
      cStr += " VALIGN=" + cValign
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) = "N"
      cStr += " HEIGHT=" + NUMTRIM( nHeight )
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) = "C"
      cStr += " HEIGHT=" + '"' + nHeight + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " BACKGROUND=" + '"' + cBgPic + '"'
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) = "N"
      cStr += " WIDTH=" + NUMTRIM( nWidth )
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) = "C"
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nColspan != NIL .and. Valtype( nColspan ) = "N"
      cStr += " COLSPAN=" + NUMTRIM( nColspan )
   ELSEIF nColspan != NIL .and. Valtype( nColspan ) = "C"
      cStr += " COLSPAN=" + '"' + nColspan + '"'
   ENDIF

   IF clrdrk != NIL
      cStr += " borderColorDark=" + clrdrk
   ENDIF

   IF clrlt != NIL
      cStr += " bordercolorlight=" + clrlt
   ENDIF

   IF cClass != NIL
      cStr += ' Class ="' + cClass + '" '
   ENDIF

   IF nRowspan != NIL .and. Valtype( nRowspan ) = "N"
      cStr += " ROWSPAN=" + NUMTRIM( nRowspan )
   ELSEIF nRowspan != NIL .and. Valtype( nRowspan ) = "C"
      cStr += " ROWSPAN=" + '"' + nRowspan + '"'
   ENDIF

   IF lWrap == .F.
      cStr += " NOWRAP"
   ENDIF

   cStr += ">"

   IF /*(cFont != NIL .or. nSize != NIL .or. cFntColor != NIL ) .and. */ !lNoFont
      cStr += '<FONT '

      IF nSize != NIL
         cStr += 'SIZE=' + NUMTRIM( nSize )
      ENDIF

      IF cFntColor != NIL
         cStr += ' COLOR=' + cFntColor
      ENDIF

      IF !Empty( cFont )
         cStr += ' FACE="' + cFont + '"' + ">"
      ELSE
         cStr += ">"
      ENDIF

      ::lFont := .t.
   ENDIF

   Fwrite( ::nH, cStr )
RETURN Self

/****
*
*     Html():endTableCell()
*
*     End a table cell definition.
*
*/

METHOD EndTableCell( lFont ) CLASS Html

   IF ::lFont
      Fwrite( ::nH, "</font></td>" + CRLF() )
   ELSE
      Fwrite( ::nH, "</td>" + CRLF() )
   ENDIF

   ::lFont := .f.
RETURN Self

/****
*
*     Html():endTable()
*
*     End a table definition.
*/

METHOD EndTable() CLASS Html

   Fwrite( ::nH, "</table>" + CRLF() )
   Fwrite( ::nH, CRLF() /*+"<!-- End of Table -->"*/ + CRLF() + CRLF() )
RETURN Self

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
//   FORMS...
//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

/****
*
*     Html():newForm()
*
*     Creates a new form
*
*/

METHOD NewForm( cMethod, cAction, cName ) CLASS Html

   DEFAULT cMethod := "POST"
   DEFAULT cName := "newForm"

   Fwrite( ::nH, CRLF() + "<FORM" )

   IF cMethod != NIL
      Fwrite( ::nH, ' METHOD="' + cMethod + '"' )
   ENDIF

   IF cName != NIL
      Fwrite( ::nH, ' NAME="' + cName + '"' )
   ENDIF

   IF cAction != NIL
      Fwrite( ::nH, ' ACTION="' + cAction + '"' )
   ENDIF

   Fwrite( ::nH, '>' + CRLF() )

   scForm := cName

RETURN Self

/****
*
*     Html():FormGet()
*
*     Adds a form edit field
*
*/

METHOD FormGet( cType, cName, xValue, nSize ) CLASS Html

   DEFAULT cType := "edit"

   Fwrite( ::nH, '<INPUT Type="' + cType + '"' )

   IF cName != NIL
      Fwrite( ::nH, ' Name="' + cName + '"' )
   ENDIF

   IF xValue != NIL
      Fwrite( ::nH, ' Value="' + ANY2STR( xValue ) + '"' )
   ENDIF

   IF nSize != NIL
      Fwrite( ::nH, ' Size="' + ANY2STR( nSize ) + '"' )
   ENDIF

   Fwrite( ::nH, ">" )
RETURN Self

/****
*
*     Html():FormSubmit()
*
*     Adds a form submit button
*
*/

METHOD FormSubmit( cText ) CLASS Html

   Fwrite( ::nH, '<INPUT Type="submit" Value="' + cText + '">' + CRLF() )
RETURN Self

/****
*
*     Html():FormImage()
*
*     Adds a form image button
*
*/

METHOD FormImage( cText, name, File ) CLASS Html

   Fwrite( ::nH, '<INPUT TYPE="IMAGE" NAME="' + name + '" SRC="' + file + '">' + CRLF() )
RETURN Self

/****
*
*     Html():FormReset()
*
*     Adds a reset button
*
*/

METHOD FormReset( cText ) CLASS Html

   Fwrite( ::nH, '<INPUT Type="Reset" Value="' + cText + '">' + CRLF() )

RETURN Self

/****
*
*     Html():pushButton()
*
*     Insert a standalone push button and assign an action to it
*     Either pass onClick or cCgiApp - not both
*/

METHOD Pushbutton( cName, cCaption, ;
                      cCgiApp, ;
                      cOnClick, ;
                      cOnFocus, cOnBlur, ;
                      cOnMsOver, cOnMsOut, ;
                      style, ID ) CLASS Html

   LOCAL cStr := CRLF() + "<INPUT TYPE=BUTTON " + CRLF()

   DEFAULT cOnMsOver := "window.status=this.name;"
   DEFAULT cOnMsOut := "window.status='';"

   IF cName != NIL
      cStr += "        NAME=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       VALUE=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       STYLE="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          ID="' + ID + '"'
   ENDIF

   IF cOnClick != NIL
      cStr += '     onClick="' + cOnClick + '"'
   ENDIF

   IF cOnFocus != NIL
      cStr += '     onFocus="' + cOnFocus + '"'
   ENDIF

   IF cOnBlur != NIL
      cStr += '      onBlur="' + cOnBlur + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsover + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += '  onMouseOut="' + cOnMsout + '"'
   ENDIF

   IF cCgiApp != NIL
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   Fwrite( ::nH, cStr + ">" )

RETURN Self

/****
*
*     Html():Button()
*
*     Insert a standalone <BUTTON> push button and assign an action to it
*
*/

METHOD Button( cName, cCaption, ;
                  cOnClick, ;
                  cCGIApp, ;
                  cOnMsOver, cOnMsOut, ;
                  Style, ID ) CLASS Html

   LOCAL cStr := CRLF() + "<BUTTON " + CRLF()

   DEFAULT cOnMsOver := "window.status=this.name;"
   DEFAULT cOnMsOut := "window.status='';"

   IF cName != NIL
      cStr += "        NAME=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       TITLE=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       STYLE="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          ID="' + ID + '"'
   ENDIF

   IF cOnClick != NIL
      cStr += '     onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsover + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += '  onMouseOut="' + cOnMsout + '"'
   ENDIF

   IF cCgiApp != NIL
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   Fwrite( ::nH, cStr + ">" + CRLF() )

RETURN Self

/****
*
*     Html():EndButton()
*
*     End a <BUTTON> definition
*
*/

METHOD EndButton() CLASS Html

   Fwrite( ::nH, CRLF() + CRLF() + "</BUTTON>" + CRLF() )
RETURN Self

/****
*
*     Html():Marquee()
*
*     Display a scrolling marquee effect
*
*/

METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
                   cAlign, nWidth, nHeight, cbgColor, ;
                   cBehavior, cDirection, ;
                   nScrollAmt, nScrollDelay, LOOP, ;
                   onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS Html

   LOCAL cStr := ""

   DEFAULT cFont := "Verdana"
   DEFAULT cFntColor := "white"
   DEFAULT nFntSize := 3
   DEFAULT cAlign := "middle"
   DEFAULT nWidth := 100
   DEFAULT cText := ""
   DEFAULT cBgColor := "black"
   DEFAULT cBehavior := "scroll"        // "slide" "alternate"
   DEFAULT cDirection := "left"         // "slide" "alternate"
   DEFAULT nScrollAmt := 5
   DEFAULT nScrolldelay := 2
   DEFAULT LOOP := 0

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   Fwrite( ::nH, '<MARQUEE align="' + cAlign + '" ' )
   Fwrite( ::nH, 'behavior="' + cBehavior + '" ' )
   Fwrite( ::nH, 'width="' + NUMTRIM( nWidth ) + '%" ' )
   Fwrite( ::nH, IIF( nHeight != NIL, 'height=' + NUMTRIM( nHeight ) + " ", "" ) )
   Fwrite( ::nH, 'bgColor="' + cBgColor + '" ' )
   Fwrite( ::nH, 'scrollamount="' + NUMTRIM( nScrollAmt ) + '" ' )
   Fwrite( ::nH, 'scrolldelay="' + NUMTRIM( nScrollDelay ) + '" ' )
   Fwrite( ::nH, 'loop=' + IIF( Valtype( loop ) == "N", NUMTRIM( loop ), loop ) + ' ' )
   Fwrite( ::nH, 'direction="' + cDirection + '" ' )
   Fwrite( ::nH, IIF( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" ) )
   Fwrite( ::nH, IIF( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" ) )
   Fwrite( ::nH, IIF( onClick != NIL, 'onClick="' + onClick + '" ', "" ) )
   Fwrite( ::nH, IIF( onStart != NIL, 'onStart="' + onStart + '" ', "" ) )
   Fwrite( ::nH, IIF( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" ) )
   Fwrite( ::nH, '>' )
   Fwrite( ::nH, cText )

   Fwrite( ::nH, "</MARQUEE>" + CRLF() )
   // FWrite( ::nH, cStr )
   ::EndFont()

RETURN Self

/****
*
*     Html():StartMarquee()
*
*     Start a scrolling marquee effect definition
*
*/

METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
                        cAlign, nWidth, nHeight, cbgColor, ;
                        cBehavior, cDirection, ;
                        nScrollAmt, nScrollDelay, LOOP, ;
                        onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS Html

   LOCAL cStr := ""

   DEFAULT cFont := "Verdana"
   DEFAULT cFntColor := "white"
   DEFAULT nFntSize := 3
   DEFAULT cAlign := "middle"
   DEFAULT nWidth := 100
   DEFAULT cBgColor := "black"
   DEFAULT cBehavior := "scroll"        // "slide" "alternate"
   DEFAULT cDirection := "left"         // "slide" "alternate"
   DEFAULT nScrollAmt := 5
   DEFAULT nScrolldelay := 2
   //DEFAULT loop         := -1

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   cStr += '<MARQUEE align="' + cAlign + '" ' + ;
      'behavior="' + cBehavior + '" ' + ;
      'width="' + NUMTRIM( nWidth ) + '%" ' + ;
      IIF( nHeight != NIL, 'height=' + NUMTRIM( nHeight ) + " ", "" ) + ;
      'bgColor="' + cBgColor + '" ' + ;
      'scrollamount="' + NUMTRIM( nScrollAmt ) + '" ' + ;
      'scrolldelay="' + NUMTRIM( nScrollDelay ) + '" ' + ;
      'loop=' + IIF( Valtype( loop ) == "N", NUMTRIM( loop ), loop ) + ' ' + ;
      'direction="' + cDirection + '" ' + ;
      IIF( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" ) + ;
      IIF( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" ) + ;
      IIF( onClick != NIL, 'onClick="' + onClick + '" ', "" ) + ;
      IIF( onStart != NIL, 'onStart="' + onStart + '" ', "" ) + ;
      IIF( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" ) + ;
      '>' + ;
      CRLF()

   Fwrite( ::nH, cStr )
   ::EndFont()

RETURN Self

/****
*
*     Html():endMarquee()
*
*
*
*/

METHOD EndMarquee() CLASS Html

   Fwrite( ::nH, "</MARQUEE>" + CRLF() )
RETURN Self

/****
*
*     Html():iFrame()
*
*     Define an inline frame.
*
*/

METHOD iFrame( name, src, border, marginwidth, marginheight, ;
                  scrolling, align, WIDTH, HEIGHT ) CLASS Html

   LOCAL cStr := "<IFRAME " + CRLF()

   DEFAULT BORDER := .T.
   DEFAULT name := "Frame01"
   //DEFAULT align  := "vertical"

   IF name != NIL
      cStr += Space( 5 ) + '        NAME="' + name + '"' + CRLF()
   ENDIF
   IF src != NIL
      cStr += Space( 5 ) + '         SRC="' + src + '"' + CRLF()
   ENDIF

   IF BORDER
      cStr += Space( 5 ) + " FRAMEBORDER='1'" + CRLF()
   ELSE
      cStr += Space( 5 ) + " FRAMEBORDER='0'" + CRLF()
   ENDIF

   IF scrolling
      cStr += Space( 5 ) + "   SCROLLING='yes'" + CRLF()
   ELSE
      cStr += Space( 5 ) + "   SCROLLING='no'" + CRLF()
   ENDIF

   IF marginwidth != NIL
      cStr += Space( 5 ) + " MARGINWIDTH='" + NUMTRIM( marginWidth ) + "'" + CRLF()
   ENDIF

   IF marginheight != NIL
      cStr += Space( 5 ) + "MARGINHEIGHT='" + NUMTRIM( marginheight ) + "'" + CRLF()
   ENDIF

   IF WIDTH != NIL
      cStr += Space( 5 ) + "       WIDTH='" + NUMTRIM( Width ) + "'" + CRLF()
   ENDIF

   IF HEIGHT != NIL
      cStr += Space( 5 ) + "      HEIGHT='" + NUMTRIM( height ) + "'" + CRLF()
   ENDIF

   IF align != NIL
      cStr += Space( 5 ) + "       ALIGN='" + align + "'" + CRLF()
   ENDIF

   cStr += ">" + CRLF()
   cStr += "</IFRAME>" + CRLF()

   Fwrite( ::nH, cStr )

RETURN Self
/*   New    Methods   */
METHOD Span( c, Style ) Class html

   LOCAL cStr := "<Span "
   IF style != NIL
      cStr += ' style ="' + Style + '"'
   ENDIF
   cStr += ">" + c + '</span>'
   Fwrite( ::nh, cStr )
RETURN Self

METHOD Comment( cText ) Class html

   LOCAL cStr := CRLF() + "<!-- "
   cStr += cText + " -->"
   Fwrite( ::nh, cStr )
RETURN Self

METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) Class HTML

   LOCAL cStr := "<Object "

   IF cType != NIL
      cStr += ' type="' + cType + '"' + CRLF()
   ENDIF

   IF cClassId != NIL
      cStr += ' classid="' + cClassId + '"' + CRLF()
   ENDIF

   IF cAling != NIL
      cStr += ' aling ="' + cAling + '"' + CRLF()
   ENDIF

   IF cCode != NIL
      cStr += ' code ="' + cCode + '"' + CRLF()
   ENDIF

   IF lDisable
      cStr += ' DISABLED ' + CRLF()
   ENDIF

   IF cCodebase != NIL
      cStr += ' codebase ="' + cCodebase + '"' + CRLF()
   ENDIF

   IF cName != NIL
      cStr += ' Name ="' + cName + '"' + CRLF()
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " height = " + NUMTRIM( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NUMTRIM( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " width = " + nWidth + " " + CRLF()
   ENDIF

   cStr += " >"
   Fwrite( ::nh, cStr + CRLF() )

RETURN Self

METHOD EndObject() Class HTML

   Fwrite( ::nh, "</OBJECT>" + CRLF() )
RETURN Self

METHOD ADDPARAM( cType, cValue ) Class HTML

   Fwrite( ::nh, '<param name="' + cType + '" value="' + cValue + '">' + CRLF() )
RETURN Self

METHOD PutLinkName( cName ) Class html

   LOCAL cStr := '<a name="' + cName + '"></a>'
   Fwrite( ::nh, cStr )
RETURN Self

/*
METHOD MultiCol( txt, cols, gutter, width ) Class Html
            DEFAULT( txt, "" )
            DEFAULT( cols, 2 )
            DEFAULT( gutter, 5 )
            DEFAULT( width, 100 )
            FWrite( ::nH, '<MULTICOL COLS="'+NUMTRIM(cols)+'" GUTTER="'+NUMTRIM(gutter)+'" WIDTH="'+NUMTRIM(width)+'">' )
            FWrite( ::nH, txt )
            FWrite( ::nH, "</MULTICOL>" )
Return Self

*/

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
// International Support...
//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

#define GREEK_ALPHABET  {;
"Á", "Â", "Ã", "Ä", "Å", "Æ", "Ç", "È", "É", "Ê", "Ë", "Ì", "Í", "Î", "Ï", "Ð", "Ñ", "Ó", "Ô", "Õ", "Ö", "×", "Ø", "Ù", ;                   // A-—
   "á", "â", "ã", "ä", "å", "æ", "ç", "è", "é", "ê", "ë", "ì", "í", "î", "ï", "ð", "ñ", "ó", "ò", "ô", "õ", "ö", "÷", "ø", ;                // ˜-¯
   "ù", "Ü", "Ý", "Þ", "ú", "ß", "ü", "ý", "û", "þ", "¢", "¸", "¹", "º", "¼", "¾", "¿", "Ú", "Û" ;  // à-ð
   }

/****
*
*     InitGreek()
*
*     Initializes the international languages support.
*
*     Uses GREEK_ALPHABET array as a match pattern. Replace with your
*     own character set.
*/

FUNCTION initGreek()

   LOCAL i
   LOCAL n
   LOCAL aGreek := GREEK_ALPHABET
   LOCAL aArr   := Array( 255 )

   FOR i := 1 TO 255
      aArr[ i ] := Chr( i )
   NEXT

   n := 1
   FOR i := 128 TO 175
      aArr[ i ] := aGreek[ n ]
      n ++
   NEXT

   FOR i := 224 TO 240
      aArr[ i ] := aGreek[ n ]
      n ++
   NEXT
   aArr[ 244 ] := aGreek[ n ]
   n ++
   aArr[ 245 ] := aGreek[ n ]

RETURN ( aArr )

/****
*
*     Greek2Html()
*
*     Converts International characters to HTML
*/

FUNCTION Greek2Html( cText )

   LOCAL i
   LOCAL cStr := ""

   IF Empty( saGreek )
      saGreek := initGreek()
   ENDIF
   FOR I := 1 TO Len( cText )
      cStr += saGreek[ Asc( Substr( cText, i, 1 ) ) ]
   NEXT

RETURN ( cStr )

/****
*
*     PageHandle()
*
*     Returns the current HTML page handle
*
*/

FUNCTION PageHandle()

RETURN snHtm

/****
*
*     CurrentForm()
*
*     Returns the current ( or last ) form name
*
*/

FUNCTION CurrentForm()

RETURN scForm

/****
*     oPage()
*
*     Return the current HTML() object.
*
*/

FUNCTION oPage()

RETURN soPage

/****
*
*     ParseCGIVar()
*
*     Separates elements of a CGI query environment variable
*
*/

FUNCTION ParseCGIVar( cEnvVar )

   cEnvVar := DecodeURL( cEnvVar )

   IF "=" $ cEnvVar .and. Len( cEnvVar ) > At( "=", cEnvVar )
      cEnvVar := Alltrim( Substr( cEnvVar, At( "=", cEnvVar ) + 1 ) )
   ELSE
      cEnvVar := ""
   ENDIF

RETURN cEnvVar

/****
*
*     DecodeURL()
*
*     Decodes a URL encoded string. Also handles international charsets.
*
*/

FUNCTION DecodeURL( cString )

   LOCAL i
   LOCAL aGreek := GREEK_CGI

   DO WHILE "%26" $ cString
      cString := Stuff( cString, At( "%26", cString ), 3, "&" )
   ENDDO

   DO WHILE "%2B" $ cString
      cString := Stuff( cString, At( "%2B", cString ), 3, "+" )
   ENDDO

   DO WHILE "%20" $ cString
      cString := Stuff( cString, At( "%20", cString ), 3, " " )
   ENDDO

   DO WHILE "%27" $ cString
      cString := Stuff( cString, At( "%27", cString ), 3, "'" )
   ENDDO

   DO WHILE "+" $ cString
      cString := Stuff( cString, At( "+", cString ), 1, " " )
   ENDDO

   DO WHILE "%2C" $ cString
      cString := Stuff( cString, At( "%2C", cString ), 3, "," )
   ENDDO

   DO WHILE "%21" $ cString
      cString := Stuff( cString, At( "%21", cString ), 3, "!" )
   ENDDO

   DO WHILE "%7E" $ cString
      cString := Stuff( cString, At( "%7E", cString ), 3, "~" )
   ENDDO

   DO WHILE "%23" $ cString
      cString := Stuff( cString, At( "%23", cString ), 3, "#" )
   ENDDO

   DO WHILE "%24" $ cString
      cString := Stuff( cString, At( "%24", cString ), 3, "!" )
   ENDDO

   DO WHILE "%25" $ cString
      cString := Stuff( cString, At( "%25", cString ), 3, "%" )
   ENDDO

   DO WHILE "%5E" $ cString
      cString := Stuff( cString, At( "%5E", cString ), 3, "^" )
   ENDDO

   DO WHILE "%28" $ cString
      cString := Stuff( cString, At( "%28", cString ), 3, "(" )
   ENDDO

   DO WHILE "%29" $ cString
      cString := Stuff( cString, At( "%29", cString ), 3, ")" )
   ENDDO

   DO WHILE "%60" $ cString
      cString := Stuff( cString, At( "%60", cString ), 3, "`" )
   ENDDO

   DO WHILE "%2F" $ cString
      cString := Stuff( cString, At( "%2F", cString ), 3, "/" )
   ENDDO

   FOR i := 1 TO Len( aGreek )
      DO WHILE aGreek[ i, 2 ] $ cString
         cString := Stuff( cString, At( aGreek[ i, 2 ], cString ), 3, aGreek[ i, 1 ] )
      ENDDO
   NEXT

RETURN cString

/****
*
*     JavaCMD()
*
*     Inserts inline Javascript source
*
*/

PROC JavaCMD( nH, cCmd )

   DEFAULT nH := pageHandle()
   DEFAULT cCmd := ""

   Fwrite( nH, '<SCRIPT LANGUAGE=JavaScript 1.2>' + CRLF() + ;
           "<!--" + CRLF() )
   Fwrite( nH, cCmd + CRLF() )
   Fwrite( nH, "//-->" + CRLF() + ;
           "</SCRIPT>" + CRLF() )

RETURN

/****
*
*     linkStyle()
*
*
*
*/

FUNCTION linkStyle( cHoverStyle, cHoverClr, cHoverBG, ;
                       cLinkStyle, cLinkClr, cLinkBG )

   LOCAL cStr := ""
   DEFAULT cHoverStyle := "normal"
   DEFAULT cLinkStyle := "normal"
   DEFAULT cHoverClr := "white"
   DEFAULT cHoverBg := "black"
   DEFAULT cLinkClr := "black"
   DEFAULT cLinkBg := "white"
   cStr := ;
      "<!-- A:hover {text-decoration:" + cHoverStyle + ";color:" + cHoverClr + ";background:" + cHoverBG + ;
      ";} A:link {text-decoration:" + cLinkStyle + ";color:" + cLinkClr + ";background:" + cLinkBG + ";}-->"

   // A:visited {font:8pt/11pt verdana; color:#4e4e4e;}

RETURN cStr

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION ANY2STR( xVal )

   LOCAL xRet := NIL

   IF Valtype( xVal ) == "C"
      xRet := IIF( Empty( xVal ), htmlSpace( 2 ), xVal )

   ELSEIF Valtype( xVal ) == "N"
      xRet := Alltrim( Str( xVal ) )

   ELSEIF Valtype( xVal ) == "O"
      xRet := "<" + xVal:CLASSNAME() + ">"

   ELSEIF Valtype( xVal ) == "D"
      xRet := Dtoc( xVal )

   ELSEIF Valtype( xVal ) == "L"
      xRet := LTOC( xVal )

   ELSEIF Valtype( xVal ) == "B"
      xRet := "{||...}"

   ELSEIF Valtype( xVal ) == NIL
      xRet := "NIL"

   ELSEIF Valtype( xVal ) == "U"
      xRet := "<Unknown Value>"

   ENDIF

RETURN ( xRet )

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION HTMLANY2STR( xVal )

   LOCAL xRet := NIL

   IF Valtype( xVal ) == "C"
      xRet := IIF( Empty( xVal ), ".", xVal )

   ELSEIF Valtype( xVal ) == "N"
      xRet := Alltrim( Str( xVal ) )

   ELSEIF Valtype( xVal ) == "O"
      xRet := "<" + xVal:CLASSNAME() + ">"

   ELSEIF Valtype( xVal ) == "D"
      xRet := Dtoc( xVal )

   ELSEIF Valtype( xVal ) == "L"
      xRet := LTOC( xVal )

   ELSEIF Valtype( xVal ) == "B"
      xRet := "{||...}"

   ELSEIF Valtype( xVal ) == NIL
      xRet := "NIL"

   ELSEIF Valtype( xVal ) == "U"
      xRet := "<Unknown Value>"

   ENDIF

RETURN ( xRet )

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION Listasarray( cList, cDelimiter )

   LOCAL nPos          // Position of cDelimiter in cList
   LOCAL aList := {}   // Define an empty array

   DEFAULT cDelimiter TO ","

   // Loop while there are more items to extract
   DO WHILE ( nPos := At( cDelimiter, cList ) ) != 0

      // Add the item to aList and remove it from cList
      Aadd( aList, Alltrim( Substr( cList, 1, nPos - 1 ) ) )
      cList := Substr( cList, nPos + 1 )

   ENDDO
   Aadd( aList, cList )                 // Add final element

RETURN ( aList )    // Return the array

//*** EOF ***//
