/*
 * $Id: thtm.prg,v 1.5 2005/12/03 18:23:05 lculik Exp $
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

#include "common.ch"
#include "hbclass.ch"
#include "cgi.ch"

STATIC snHtm   := NIL                 
STATIC scForm  := 0
STATIC soPage  := 0

/****
*
*     Class THtml()
*
*     Constructors :
*
*     THtml():New()          Creates a new HTML document
*
*     THtml():CGINew()       Creates a new CGI-HTML document
*
*/

CLASS THtml

   DATA nH
   DATA FName, TITLE
   DATA FontFace INIT "Verdana"
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

   METHOD CgiNew( cTitle, cLinkTitle, cCharSet, cScriptSRC, ;
                  bgImage, bgColor, txtColor, cJavaCode, ;
                  onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
                  cStyle, aImages, aServerSrc, baseURL, baseTarget, ;
                  nRefresh, cRefreshURL, cStyleScr, lnocache, ;
                  nof, nMarginTop, nMarginHeight, nMarginWidth, nMarginLeft )

   METHOD CGIClose()

   METHOD SetPageColor( cColor, lBody ) INLINE DEFAULT(lBody ,.T.),Fwrite( ::nH, IIF( lBody, '<BODY BGCOLOR="' + cColor + '">', ' BGCOLOR="' + cColor + '" ' ) )

   METHOD SetTextColor( cColor, lBody ) INLINE DEFAULT(lBody ,.T.),Fwrite( ::nH, IIF( lBody, '<BODY TEXT="' + cColor + '">', ' TEXT="' + cColor + '" ' ) )

   METHOD SetBgImage( cImage, lBody ) INLINE DEFAULT(lBody ,.T.),Fwrite( ::nH, IIF( lBody, '<BODY BACKGROUND="' + cImage + '">', ' BACKGROUND="' + cImage + '" ' ) )

   METHOD CLOSE()

   METHOD SetCenter( lOn ) INLINE Fwrite( ::nH, IIF( lOn, "<CENTER>", "</CENTER>" ) )

   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )

   METHOD ENDFONT()                    

   METHOD SAY( str, font, size, type, color, style )

   METHOD Qqout( c ) INLINE DEFAULT( c, "" ), Fwrite( ::nH, c )

   METHOD Qout( c ) INLINE DEFAULT( c, "" ), Fwrite( ::nH, CRLF() + c + '<BR>' + CRLF() )

   METHOD Write( c ) INLINE DEFAULT( c, "" ), Fwrite( ::nH, c )

   METHOD WriteLN( c ) INLINE DEFAULT( c, "" ), Fwrite( ::nH, CRLF() + c + '<BR>' + CRLF() )

   METHOD SayColor( t, c ) INLINE DEFAULT( t, "" ), DEFAULT( c, "black" ),;
                    Fwrite( ::nH, '<FONT COLOR="' + c + '">' + t + '</FONT>' )

   METHOD Space( n ) INLINE DEFAULT( n, 1 ), Fwrite( ::nH, Replicate( "&nbsp;", n ) )

   METHOD PutImage( cImage, nBorder, nHeight, cOnclick, cOnMsOver, cOnMsOut, ;
                    cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALING, HSPACE )

   METHOD TEXT( cText, nCols, lWrap ) INLINE DEFAULT( lWrap, .T. ), DEFAULT( nCols, 80 ),;
              Fwrite( ::nH, "<PRE" + IIF( nCols != NIL, ' COLS="' + NTRIM( nCols ) + "'", "" ) + IIF( lWrap, " WRAP>", ">" ) + CRLF() + cText + CRLF() + "</PRE>" + CRLF() )

   METHOD MultiCol( txt, cols, gutter, width ) INLINE DEFAULT( txt, "" ),;
                    DEFAULT( cols, 2 ),;
                    DEFAULT( gutter, 5 ),;
                    DEFAULT( width, 100 ),;
                    Fwrite( ::nH, '<MULTICOL COLS="' + NTRIM( cols ) + '" GUTTER="' + NTRIM( gutter ) + '" WIDTH="' + NTRIM( width ) + '">' ),;
                    Fwrite( ::nH, txt ),;
                    Fwrite( ::nH, "</MULTICOL>" )

   METHOD PutHeading( cText, nWeight, lCentered )

   METHOD HLine( nSize, nWidth, lShade, cColor )

   METHOD PutParagraph() INLINE Fwrite( ::nH, "<P> </P>" + CRLF() )

   METHOD Paragraph( l, c, style )

   METHOD PutBreak() INLINE Fwrite( ::nH, "<BR>" + CRLF() )

   METHOD Marquee( cText, cFont, cFntColor, nFntSize, cAlign, nWidth, nHeight, cbgColor, ;
                   cBehavior, cDirection, nScrollAmt, nScrollDelay, LOOP, ;
                   onMsOver, onMsOut, onClick, onStart, onFinish )

   METHOD StartMarquee( cFont, cFntColor, nFntSize, cAlign, nWidth, nHeight, cbgColor, ;
                        cBehavior, cDirection, nScrollAmt, nScrollDelay, LOOP, ;
                        onMsOver, onMsOut, onClick, onStart, onFinish )
                        METHOD EndMarquee()

   METHOD PutTextUrl( cText, cUrl, cOnClick, cOmMsOver, cOnMsout, cTarget,  font, clr, size, style, bld, lbreak, cClass )

   METHOD PutImageUrl( cImage, nBorder, nHeight, nWidth, cUrl, ;
                       cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ALING )

   METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, l3d, lRuleCols, lRuleRows, ;
                       cClrDark, cClrLight, ncellpadding, ncellspacing, ;
                       cAling, lRules, BGIMAGE, cStyle, ID, NOF )

   METHOD TableHead( cHead, cColor, cAlign, cFont, nSize, cFntColor, nHeight, cBgPic )

   METHOD NewTableRow( cColor )

   METHOD EndTableRow()

   METHOD NewTableCell( cAlign, cColor, cFont, nSize, cFntColor, nHeight, cBgPic, ;
                        nWidth, lWrap, nCSpan, nRSpan, cValing, clrdrk, clrlt, cBdrClr, cclass )

   METHOD EndTableCell( lFont )

   METHOD EndTable()

   METHOD NewList() INLINE Fwrite( ::nH, "<UL>" + CRLF() )

   METHOD ListItem() INLINE Fwrite( ::nH, "<LI> " )

   METHOD EndList() INLINE Fwrite( ::nH, "</UL> " )

   METHOD NewForm( cMethod, cAction, cName )

   METHOD FormImage( cText, name, File )

   METHOD FormEdit( cType, cName, xValue, nSize )

   METHOD FormReset( c )

   METHOD FormSubmit( c )

   METHOD FormQOut( c ) INLINE Fwrite( ::nH, c + '<BR>' + CRLF() )

   METHOD FormQQOut( c ) INLINE Fwrite( ::nH, c + CRLF() )

   METHOD EndForm() INLINE Fwrite( ::nH, CRLF() + "</FORM>" + CRLF() )

   METHOD Pushbutton( cName, cCaption, cCgiApp, cOnClick, cOnFocus, cOnBlur, cOnMsOver, cOnMsOut, style, ID )

   METHOD endButton()

   METHOD Button( cName, cCaption, cOnClick, cCgiApp, cOnMsOver, cOnMsOut, style, ID )

   METHOD iFrame( name, src, border, marginwidth, marginheight, scrolling, allign, WIDTH, HEIGHT )

   METHOD StartJava() INLINE Fwrite( ::nH, '<SCRIPT LANGUAGE="JavaScript">' + CRLF() + "<!--" + CRLF() )

   METHOD PutJavaSource( c ) INLINE Fwrite( ::nH, Space( 5 ) + 'SRC="' + c + '"' + CRLF() )

   METHOD PutJava( c ) INLINE Fwrite( ::nH, Space( 5 ) + c + CRLF() )

   METHOD EndJava() INLINE Fwrite( ::nH, "                  //-->" + CRLF() + "</SCRIPT>" + CRLF() )

   METHOD serverCode( c ) INLINE Fwrite( ::nH, "<SERVER>" + Space( 9 ) + c + CRLF() + "</SERVER>" + CRLF() )

   METHOD Fwrite( c ) INLINE Fwrite( ::nH, c )

   METHOD FWriteLN( c ) INLINE Fwrite( ::nH, c + CRLF() )

   METHOD Span( c, Style )

   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, cOnclick, ;
                           cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )

   METHOD Comment( cText )

   METHOD ADDoBJECT( cType, cClassid, cAling, cCode, lDisable, cCodeBase, ;
                     cName, nWidth, nHeight )

   METHOD ADDPARAM( cName, cValue )

   METHOD EndOBJect()

   METHOD PutLinkName( cName )

   METHOD NewMap( cName ) INLINE Fwrite( ::nH, "<MAP NAME=" + cName + ">" )


   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
          Fwrite( ::nH, "<AREA  shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + CRLF() )

   METHOD EndMap() INLINE Fwrite( ::nH, "</MAP>" )

ENDCLASS

/****
*
*     THtml():New()
*
*     Starts a new HTML disk file.
*/

METHOD New( cFile, cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
               BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
               onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
               cStyle, aImages, cBaseURL, cBaseTarget, ;
               nRefresh, cRefreshURL, cStyleScr, lnocache ) CLASS THtml

   LOCAL i

   DEFAULT cFile TO "file1.htm"
   DEFAULT cTitle TO "New HTML page"
   DEFAULT cLinkTitle TO cTitle
   DEFAULT cRefreshURL TO ""
   DEFAULT cCharset TO "windows-1253"
   tracelog(cFile)
   ::nH    := Fcreate( cFile )
   tracelog(::nh)
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
/* TOFIX: Luiz please review it
   Fwrite( ::nH, '   <LINK TITLE="' + cLinkTitle + '"' + CRLF() + ;
           '                HREF="mailto:culik@sl.conex.net" >' + CRLF() + ;
           '   <META HTTP-EQUIV="Content-Type" content="text/html; charset=' + cCharset + '">' + CRLF() )
*/
   IF nRefresh != NIL
      Fwrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="] + NTRIM( nRefresh ) + [; URL=] + cRefreshURL + [">] )
   ENDIF

   IF lnocache
      Fwrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ] )
   ENDIF

   IF aJsCode != NIL
      Aeval( aJsCode, { | e | HtmlJsCmd( ::nH, e ) } )
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
*     THtml():CGINew()
*
*     Starts a new CGI-HTML stream file.
*/

METHOD CgiNew( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
                  BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
                  onLoad, onUnload, ;
                  cLinkClr, cVLinkClr, cALinkClr, ;
                  cStyle, aImages, aServerSrc, ;
                  cBaseURL, cBaseTarget, ;
                  nRefresh, cRefreshURL, cStyleScr, ;
                  lNocache, NOF, nMarginTop, nMarginHeight, ;
                  nMarginWidth, nMarginLeft ) CLASS THtml

   LOCAL i

   DEFAULT cTitle TO "New CGI-HTML page"
   DEFAULT cLinkTitle TO cTitle
   DEFAULT cRefreshURL TO ""
   DEFAULT cCharset TO "windows-1253"
   DEFAULT lNocache TO .f.

   ::nH    := STD_OUT 
   ::Title := cTitle
   ::FName := "cgiout.htm"

   Fwrite( ::nH, 'Content-Type: text/html' + CRLF() + CRLF() )

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
/* TOFIX: Luiz please review it
   Fwrite( ::nH, '   <LINK TITLE="' + cLinkTitle + '"' + CRLF() + ;
           '                HREF="mailto:culik@sl.conex.net" >' + CRLF() + ;
           '   <META HTTP-EQUIV="Content-Type" content="text/html; charset=' + cCharset + '">' + CRLF() )
*/
   IF cStyleScr != NIL
      Fwrite( ::nH, '   <LINK HREF="' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css'>" + CRLF() )
   ENDIF

   IF nRefresh != NIL
      Fwrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="] + NTRIM( nRefresh ) + [; URL=] + cRefreshURL + [">] )
   ENDIF

   IF lnocache
      Fwrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ] )
   ENDIF

   IF aJsCode != NIL
      Aeval( aJsCode, { | e | HtmlJsCmd( ::nH, e ) } )
   ENDIF

   IF aScriptSrc != NIL

      FOR i := 1 TO Len( aScriptSrc )
         Fwrite( ::nH, ;                
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aScriptSrc[ i ] + '"></SCRIPT>' + CRLF() )
      NEXT

   ENDIF

   IF aServerSrc != NIL

      FOR i := 1 TO Len( aServerSrc )
         Fwrite( ::nH, ;                
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
      Fwrite( ::nH, ' topMargin=' + NTRIM( nMarginTop ) )
   ENDIF

   IF nMarginLeft != NIL
      Fwrite( ::nH, ' LeftMargin=' + NTRIM( nMarginLeft ) )
   ENDIF

   IF nMarginHeight != NIL
      Fwrite( ::nH, ' MARGINHEIGHT=' + NTRIM( nMarginHeight ) )
   ENDIF

   IF nMarginWidth != NIL
      Fwrite( ::nH, ' MARGINWIDTH=' + NTRIM( nMarginWidth ) )
   ENDIF

   Fwrite( ::nH, '>' )

   Fwrite( ::nH, CRLF() )

   snHtm := ::nH

   soPage := Self

   RETURN self

/****
*
*     THtml():SetFont()
*
*     obvious...
*/

METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := CRLF() + '<FONT'

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor
   DEFAULT lset TO IIF( cFont != NIL, .t., .f. )

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
*     THtml():StartFont()
*
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut ) CLASS THtml

   LOCAL cStr := "<FONT "
   DEFAULT lSet TO .t.
   DEFAULT lPut TO .f.
   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor

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
*     THtml():DefineFont()
*
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := "<FONT "

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor
   DEFAULT lset TO IIF( cFont != NIL, .t., .f. )

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
*     THtml():EndFont()
*
*     End a font definition
*/

METHOD ENDFONT() CLASS THtml

   Fwrite( ::nH, '</font>' + CRLF() )

   RETURN Self

/****
*
*     THtml():say()
*
*
*
*/

METHOD SAY( str, font, size, type, color, style ) CLASS THtml

   LOCAL cOut    := ""
   LOCAL lBold   := .F.
   LOCAL lItalic := .F.
   LOCAL lULine  := .F.
   LOCAL lEm     := .f.
   LOCAL lStrong := .f.
   LOCAL nSize   := Size
   DEFAULT str TO ""
   DEFAULT FONT TO ::FontFace
   DEFAULT size TO ::FontSize
   DEFAULT COLOR TO ::FontColor

   IF FONT != NIL .or. Size != NIL .or. COLOR != NIL
      cOut := '<FONT ' + IIF( font != NIL, 'FACE="' + font + '"', '' ) + IIF( color != NIL, ' COLOR=' + color, '' ) + IIF( nSize != NIL, ' SIZE=' + NTRIM( size ), "" )

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
*     THtml():paragraph()
*
*
*
*/

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS THtml

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
*     THtml():HLine()
*
*     Put a Horizontal line
*/

METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS THtml

   DEFAULT nSize TO 3
   DEFAULT nWidth TO 100
   DEFAULT lShade TO .T.

   IF lShade
      Fwrite( ::nH, CRLF() + ;
              '<HR SIZE = ' + NTRIM( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + NTRIM( nWidth ) + '%>' + ;
              CRLF() )
   ELSE
      Fwrite( ::nH, CRLF() + ;
              '<HR NOSHADE SIZE = ' + NTRIM( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + NTRIM( nWidth ) + '%>' + ;
              CRLF() )
   ENDIF

   RETURN Self

/****
*
*     THtml():PutHeading()
*
*     Put an HTML heading ( large text )
*/

METHOD PutHeading( cText, nWeight, lCentered ) CLASS THtml

   DEFAULT nWeight TO 3
   DEFAULT lCentered TO .F.

   IF lCentered
      Fwrite( ::nH, "<CENTER>" )
   ENDIF

   Fwrite( ::nH, "<H" + NTRIM( nWeight ) + ">" + cText + "</H" + NTRIM( nWeight ) + ">" + CRLF() )

   IF lCentered
      Fwrite( ::nH, "</CENTER>" )
   ENDIF

   RETURN Self

/****
*
*     THtml():putTextURL()
*
*     Put a text link.
*/

METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass ) CLASS THtml

   LOCAL cStr := ""
   DEFAULT cUrl TO ""
   DEFAULT bld TO .F.
   DEFAULT lBreak TO .f.

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
         cStr += ' size=' + NTRIM( size )
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
*     THtml():putImageURL()
*
*     Put an Image link.
*/

METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
                       cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
                       Id, hSpace, Aling ) CLASS THtml
   LOCAL cStr := ""
   default lbreak TO .F.

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"' + CRLF()
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + IIF( Valtype( nBorder ) == "N", NTRIM( nBorder ), nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " height = " + NTRIM( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NTRIM( nWidth ) + " " + CRLF()
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
      cStr += " hSpace= " + NTRIM( hSpace ) + " "
   ENDIF

   Fwrite( ::nH, ;
           '<A HREF=' + cUrl + IIF( cClass != NIL, ' class="' + cClass + '"', "" ) + '><IMG SRC="' + cImage + '"' + ;
           cStr + '></A>' + IIF( lBreak, '<br>' + CRLF(), "" ) )

   RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
                           cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS THtml
   LOCAL cStr := ""
   default lbreak TO .F.
   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + NTRIM( nBorder )
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " height = " + NTRIM( nHeight ) + " "
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " "
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NTRIM( nWidth ) + " "
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
*     THtml():putImage()
*
*     Put an Image.
*/

METHOD PutImage( cImage, nBorder, nHeight, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget,;
                    nWidth, lbreak, Id, Map, Aling, hSpace ) CLASS THtml
   LOCAL cStr := ""
   default lbreak TO .F.

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL .and. Valtype( nBorder ) == "N"
      cStr += " BORDER = " + NTRIM( nBorder )
   ELSEIF nBorder != NIL .and. Valtype( nBorder ) == "C"
      cStr += " BORDER = " + '"' + nBorder + '"'
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " HEIGHT = " + NTRIM( nHeight ) + " "
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " HEIGHT = " + '"' + nHeight + '"'
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NTRIM( nWidth ) + " "
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
      cStr += " hSpace= " + NTRIM( hSpace ) + " "
   ENDIF

   Fwrite( ::nH, ;
           '<IMG SRC="' + cImage + '"' + ;
           cStr + '>' + IIF( lBreak, "<br>" + CRLF(), "" ) )

   RETURN Self

/****
*
*     THtml():Close()
*
*     Close an HTML disk file
*
*/

METHOD CLOSE() CLASS THtml

   Fwrite( ::nH, "</body>" + CRLF() )
   Fwrite( ::nH, "</html>" + CRLF() )
   Fclose( ::nH )
   RETURN Self

/****
*
*     THtml():CGIClose()
*
*     Close a CGI-HTML stream file
*/

METHOD cgiClose() CLASS THtml

   Fwrite( ::nH, "</body>" + CRLF() )
   Fwrite( ::nH, "</html>" + CRLF() )
   Fwrite( ::nH, CRLF() )
   RETURN Self

/****
*
*     THtml():defineTable()
*
*     Start an HTML table definition.
*
*
*/

METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
                       l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
                       nCellPadding, nCellSpacing, cAling, lRules, ;
                       bgImage, cStyle, Id, NOF ) CLASS THtml

   LOCAL cStr  := CRLF() + CRLF() + "<TABLE "
   LOCAL xCols := nCols

   DEFAULT l3d TO .T.
   DEFAULT lRuleCols TO .F.
   DEFAULT lRuleRows TO .F.

   IF ColorFore != NIL
      cStr += " bordercolor=" + ColorFore + ' '
   ENDIF

   IF Colorbg != NIL
      cStr += " bgcolor=" + ColorBG + ' '
   ENDIF

   cStr += IIF( nBorder = NIL, "border ", "border=" + NTRIM( nBorder ) + ' ' )

   IF ncellpadding != NIL
      cStr += ' CellPadding=' + NTRIM( nCellPadding )
   ENDIF

   IF nCellSpacing != NIL
      cStr += ' CellSpacing=' + NTRIM( nCellSpacing )
   ENDIF

   IF cAling != NIL
      cStr += ' aling=' + '"' + cAling + '"'
   ENDIF

   cStr += IIF( xCols != NIL, " COLS=" + NTRIM( nCols ), "" )

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " WIDTH=" + NTRIM( nWidth )
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nHeight != NIL .and. Valtype( nHeight ) == "N"
      cStr += " HEIGHT=" + NTRIM( nHeight )
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

   Fwrite( ::nH, cStr + CRLF() )

   RETURN Self

/****
*
*     THtml():TableHead()
*
*     Define a table column Header.
*
*/

METHOD TableHead( cHead, cColor, cAlign, ;
                     cFont, nSize, cFntColor, nHeight, cBgPic ) CLASS THtml

   LOCAL cStr := Space( 3 ) + "<TH"

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cFntColor TO ::fontColor

   IF cColor != NIL
      cStr += " bgcolor=" + '"' + cColor + '"'
   ENDIF

   IF cAlign != NIL
      cStr += " align=" + '"' + cAlign + '"'
   ENDIF

   IF nHeight != NIL
      cStr += " height=" + '"' + NTRIM( nHeight ) + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   cStr += ">"

   IF cFont != NIL
      cStr += '<font face="' + cFont + '"'

      IF nSize != NIL
         cStr += ' size="' + NTRIM( nSize ) + '"'
      ENDIF

      IF cFntColor != NIL
         cStr += ' color="' + cFntColor + '"'
      ENDIF

      cStr += ">"
   ENDIF

   cStr += cHead + IIF( cFont != NIL, '</font>', "" ) + "</th>" + CRLF()

   Fwrite( ::nH, cStr )

   RETURN Self

/****
*
*     THtml():NewTableRow()
*
*     Start a table row definition.
*
*/

METHOD NewTableRow( cColor, vAling, aLing ) CLASS THtml

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
*     THtml():EndTableRow()
*
*     End a table row definition.
*
*/

METHOD EndTableRow() CLASS THtml

   Fwrite( ::nH, Space( 5 ) + "</TR>" + CRLF() )
   RETURN Self

/****
*
*     THtml():NewTableCell()
*
*     Start a table cell definition.
*
*/

METHOD NewTableCell( cAlign, cColor, ;
                        cFont, nSize, cFntColor, nHeight, ;
                        cBgPic, nWidth, lWrap, ;
                        nColspan, nRowspan, cValign, clrdrk, clrlt, cBdrClr, cClass, lNoFont ) CLASS THtml

   LOCAL cStr := Space( 10 ) + "<TD"
   LOCAL cAli := cAlign
   DEFAULT lNoFont TO .T.
   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cFntColor TO ::fontColor
   DEFAULT cAlign TO "LEFT"
   DEFAULT lWrap TO .T.

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
      cStr += " HEIGHT=" + NTRIM( nHeight )
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) = "C"
      cStr += " HEIGHT=" + '"' + nHeight + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " BACKGROUND=" + '"' + cBgPic + '"'
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) = "N"
      cStr += " WIDTH=" + NTRIM( nWidth )
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) = "C"
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nColspan != NIL .and. Valtype( nColspan ) = "N"
      cStr += " COLSPAN=" + NTRIM( nColspan )
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
      cStr += " ROWSPAN=" + NTRIM( nRowspan )
   ELSEIF nRowspan != NIL .and. Valtype( nRowspan ) = "C"
      cStr += " ROWSPAN=" + '"' + nRowspan + '"'
   ENDIF

   IF lWrap == .F.
      cStr += " NOWRAP"
   ENDIF

   cStr += ">"

   IF !lNoFont
      cStr += '<FONT '

      IF nSize != NIL
         cStr += 'SIZE=' + NTRIM( nSize )
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
*     THtml():EndTableCell()
*
*     End a table cell definition.
*
*/

METHOD EndTableCell( lFont ) CLASS THtml

   IF ::lFont
      Fwrite( ::nH, "</font></td>" + CRLF() )
   ELSE
      Fwrite( ::nH, "</td>" + CRLF() )
   ENDIF

   ::lFont := .f.
   RETURN Self

/****
*
*     THtml():EndTable()
*
*     End a table definition.
*/

METHOD EndTable() CLASS THtml

   Fwrite( ::nH, "</table>" + CRLF() )
   Fwrite( ::nH, CRLF() + CRLF() + CRLF() )
   RETURN Self


/****
*
*     THtml():NewForm()
*
*     Creates a new form
*
*/

METHOD NewForm( cMethod, cAction, cName ) CLASS THtml

   DEFAULT cMethod TO "POST"
   DEFAULT cName TO "newForm"

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
*     THtml():FormEdit()
*
*     Adds a form edit field
*
*/

METHOD FormEdit( cType, cName, xValue, nSize ) CLASS THtml

   DEFAULT cType TO "edit"

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
*     THtml():FormSubmit()
*
*     Adds a form submit button
*
*/

METHOD FormSubmit( cText ) CLASS THtml

   Fwrite( ::nH, '<INPUT Type="submit" Value="' + cText + '">' + CRLF() )

   RETURN Self

/****
*
*     THtml():FormImage()
*
*     Adds a form image button
*
*/

METHOD FormImage( cText, name, File ) CLASS THtml

   Fwrite( ::nH, '<INPUT TYPE="IMAGE" NAME="' + name + '" SRC="' + file + '">' + CRLF() )

   RETURN Self

/****
*
*     THtml():FormReset()
*
*     Adds a reset button
*
*/

METHOD FormReset( cText ) CLASS THtml

   Fwrite( ::nH, '<INPUT Type="Reset" Value="' + cText + '">' + CRLF() )

   RETURN Self

/****
*
*     THtml():pushButton()
*
*     Insert a standalone push button and assign an action to it
*     Either pass onClick or cCgiApp - not both
*/

METHOD Pushbutton( cName, cCaption, ;
                      cCgiApp, ;
                      cOnClick, ;
                      cOnFocus, cOnBlur, ;
                      cOnMsOver, cOnMsOut, ;
                      style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<INPUT TYPE=BUTTON " + CRLF()

   DEFAULT cOnMsOver TO "window.status=this.name;"
   DEFAULT cOnMsOut TO "window.status='';"

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
*     THtml():Button()
*
*     Insert a standalone <BUTTON> push button and assign an action to it
*
*/

METHOD Button( cName, cCaption, ;
                  cOnClick, ;
                  cCGIApp, ;
                  cOnMsOver, cOnMsOut, ;
                  Style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<BUTTON " + CRLF()

   DEFAULT cOnMsOver TO "window.status=this.name;"
   DEFAULT cOnMsOut TO "window.status='';"

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
*     THtml():EndButton()
*
*     End a <BUTTON> definition
*
*/

METHOD EndButton() CLASS THtml

   Fwrite( ::nH, CRLF() + CRLF() + "</BUTTON>" + CRLF() )
   RETURN Self

/****
*
*     THtml():Marquee()
*
*     Display a scrolling marquee effect
*
*/

METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
                   cAlign, nWidth, nHeight, cbgColor, ;
                   cBehavior, cDirection, ;
                   nScrollAmt, nScrollDelay, LOOP, ;
                   onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   LOCAL cStr := ""

   DEFAULT cFont TO "Verdana"
   DEFAULT cFntColor TO "white"
   DEFAULT nFntSize TO 3
   DEFAULT cAlign TO "middle"
   DEFAULT nWidth TO 100
   DEFAULT cText TO ""
   DEFAULT cBgColor TO "black"
   DEFAULT cBehavior TO "scroll"      
   DEFAULT cDirection TO "left"      
   DEFAULT nScrollAmt TO 5
   DEFAULT nScrolldelay TO 2
   DEFAULT LOOP TO 0

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   Fwrite( ::nH, '<MARQUEE align="' + cAlign + '" ' )
   Fwrite( ::nH, 'behavior="' + cBehavior + '" ' )
   Fwrite( ::nH, 'width="' + NTRIM( nWidth ) + '%" ' )
   Fwrite( ::nH, IIF( nHeight != NIL, 'height=' + NTRIM( nHeight ) + " ", "" ) )
   Fwrite( ::nH, 'bgColor="' + cBgColor + '" ' )
   Fwrite( ::nH, 'scrollamount="' + NTRIM( nScrollAmt ) + '" ' )
   Fwrite( ::nH, 'scrolldelay="' + NTRIM( nScrollDelay ) + '" ' )
   Fwrite( ::nH, 'loop=' + IIF( Valtype( loop ) == "N", NTRIM( loop ), loop ) + ' ' )
   Fwrite( ::nH, 'direction="' + cDirection + '" ' )
   Fwrite( ::nH, IIF( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" ) )
   Fwrite( ::nH, IIF( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" ) )
   Fwrite( ::nH, IIF( onClick != NIL, 'onClick="' + onClick + '" ', "" ) )
   Fwrite( ::nH, IIF( onStart != NIL, 'onStart="' + onStart + '" ', "" ) )
   Fwrite( ::nH, IIF( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" ) )
   Fwrite( ::nH, '>' )
   Fwrite( ::nH, cText )

   Fwrite( ::nH, "</MARQUEE>" + CRLF() )
   ::EndFont()

   RETURN Self

/****
*
*     THtml():StartMarquee()
*
*     Start a scrolling marquee effect definition
*
*/

METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
                        cAlign, nWidth, nHeight, cbgColor, ;
                        cBehavior, cDirection, ;
                        nScrollAmt, nScrollDelay, LOOP, ;
                        onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   LOCAL cStr := ""

   DEFAULT cFont TO "Verdana"
   DEFAULT cFntColor TO "white"
   DEFAULT nFntSize TO 3
   DEFAULT cAlign TO "middle"
   DEFAULT nWidth TO 100
   DEFAULT cBgColor TO "black"
   DEFAULT cBehavior TO "scroll" 
   DEFAULT cDirection TO "left" 
   DEFAULT nScrollAmt TO 5
   DEFAULT nScrolldelay TO 2

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   cStr += '<MARQUEE align="' + cAlign + '" ' + ;
      'behavior="' + cBehavior + '" ' + ;
      'width="' + NTRIM( nWidth ) + '%" ' + ;
      IIF( nHeight != NIL, 'height=' + NTRIM( nHeight ) + " ", "" ) + ;
      'bgColor="' + cBgColor + '" ' + ;
      'scrollamount="' + NTRIM( nScrollAmt ) + '" ' + ;
      'scrolldelay="' + NTRIM( nScrollDelay ) + '" ' + ;
      'loop=' + IIF( Valtype( loop ) == "N", NTRIM( loop ), loop ) + ' ' + ;
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
*     THtml():EndMarquee()
*
*/

METHOD EndMarquee() CLASS THtml

   Fwrite( ::nH, "</MARQUEE>" + CRLF() )
   RETURN Self

/****
*
*     THtml():iFrame()
*
*     Define an inline frame.
*
*/

METHOD iFrame( name, src, border, marginwidth, marginheight, ;
                  scrolling, align, WIDTH, HEIGHT ) CLASS THtml

   LOCAL cStr := "<IFRAME " + CRLF()

   DEFAULT BORDER TO .T.
   DEFAULT name TO "Frame01"
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
      cStr += Space( 5 ) + " MARGINWIDTH='" + NTRIM( marginWidth ) + "'" + CRLF()
   ENDIF

   IF marginheight != NIL
      cStr += Space( 5 ) + "MARGINHEIGHT='" + NTRIM( marginheight ) + "'" + CRLF()
   ENDIF

   IF WIDTH != NIL
      cStr += Space( 5 ) + "       WIDTH='" + NTRIM( Width ) + "'" + CRLF()
   ENDIF

   IF HEIGHT != NIL
      cStr += Space( 5 ) + "      HEIGHT='" + NTRIM( height ) + "'" + CRLF()
   ENDIF

   IF align != NIL
      cStr += Space( 5 ) + "       ALIGN='" + align + "'" + CRLF()
   ENDIF

   cStr += ">" + CRLF()
   cStr += "</IFRAME>" + CRLF()

   Fwrite( ::nH, cStr )

   RETURN Self
/*   New    Methods   */
METHOD Span( c, Style ) CLASS THtml

   LOCAL cStr := "<Span "
   IF style != NIL
      cStr += ' style ="' + Style + '"'
   ENDIF
   cStr += ">" + c + '</span>'
   Fwrite( ::nh, cStr )
   RETURN Self

METHOD Comment( cText ) CLASS THtml

   LOCAL cStr := CRLF() + "<!-- "
   cStr += cText + " -->"
   Fwrite( ::nh, cStr )
   RETURN Self

METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) CLASS THtml

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
      cStr += " height = " + NTRIM( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. Valtype( nHeight ) == "C"
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. Valtype( nWidth ) == "N"
      cStr += " width = " + NTRIM( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .and. Valtype( nWidth ) == "C"
      cStr += " width = " + nWidth + " " + CRLF()
   ENDIF

   cStr += " >"
   Fwrite( ::nh, cStr + CRLF() )

   RETURN Self

METHOD EndObject() CLASS THtml

   Fwrite( ::nh, "</OBJECT>" + CRLF() )

   RETURN Self

METHOD ADDPARAM( cType, cValue ) CLASS THtml

   Fwrite( ::nh, '<param name="' + cType + '" value="' + cValue + '">' + CRLF() )

   RETURN Self

METHOD PutLinkName( cName ) CLASS THtml

   LOCAL cStr := '<a name="' + cName + '"></a>'

   Fwrite( ::nh, cStr )

   RETURN Self

/****
*
*     HtmlPageHandle()
*
*     Returns the current HTML page handle
*
*/

FUNCTION HtmlPageHandle()

   RETURN snHtm

/****
*
*     HtmlFormName()
*
*     Returns the current ( or last ) form name
*
*/

FUNCTION HtmlFormName()

   RETURN scForm

/****
*     HtmlPageObject()
*
*     Return the current THtml() object.
*
*/

FUNCTION HtmlPageObject()

   RETURN soPage

/****
*
*     HtmlDecodeURL()
*
*     Decodes a URL encoded string. Also handles international charsets.
*
*/

FUNCTION HtmlDecodeURL( cString )

   LOCAL i

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

   RETURN cString

/****
*
*     HtmlJsCmd()
*
*     Inserts inline Javascript source
*
*/

PROCEDURE HtmlJsCmd( nH, cCmd )

   DEFAULT nH TO HtmlPageHandle()
   DEFAULT cCmd TO ""

   Fwrite( nH, '<SCRIPT LANGUAGE=JavaScript 1.2>' + CRLF() + ;
           "<!--" + CRLF() )
   Fwrite( nH, cCmd + CRLF() )
   Fwrite( nH, "//-->" + CRLF() + ;
           "</SCRIPT>" + CRLF() )

   RETURN

/****
*
*     HtmlLinkStyle()
*
*/

FUNCTION HtmlLinkStyle( cHoverStyle, cHoverClr, cHoverBG, ;
                       cLinkStyle, cLinkClr, cLinkBG )

   LOCAL cStr := ""
   DEFAULT cHoverStyle TO "normal"
   DEFAULT cLinkStyle TO "normal"
   DEFAULT cHoverClr TO "white"
   DEFAULT cHoverBg TO "black"
   DEFAULT cLinkClr TO "black"
   DEFAULT cLinkBg TO "white"
   cStr := ;
      "<!-- A:hover {text-decoration:" + cHoverStyle + ";color:" + cHoverClr + ";background:" + cHoverBG + ;
      ";} A:link {text-decoration:" + cLinkStyle + ";color:" + cLinkClr + ";background:" + cLinkBG + ";}-->"

   RETURN cStr

/****
*
*     HtmlPadL()
*
*/

FUNCTION HtmlPadL( cStr, n )

   LOCAL cRet    := ""
   LOCAL nStrLen
   LOCAL nSpaces

   IF n == NIL
      RETURN cStr
   ENDIF

   nStrLen := Len( cStr )
   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := Right( cStr, n )
   ELSE
      cRet := Replicate( _HTML_SPACE, nSpaces ) + cStr
   ENDIF

   RETURN cRet

/****
*
*     HtmlPadR()
*
*/

FUNCTION HtmlPadR( cStr, n )

   LOCAL cRet    := ""
   LOCAL nStrLen
   LOCAL nSpaces

   IF n == NIL
      RETURN cStr
   ENDIF

   nStrLen := Len( cStr )
   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := LEFT( cStr, n )
   ELSE
      cRet := cStr + Replicate( _HTML_SPACE, nSpaces )
   ENDIF

   RETURN cRet

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

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
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

