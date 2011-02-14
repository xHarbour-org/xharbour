/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Main HTML DocType CLASS for HTML LIB
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "common.ch"
#include "hbclass.ch"
#include "cgidefs.ch"

#xtranslate HTMLIndent( <x> )   => ( Space( ::nIndent ) + <x> )
#xtranslate HTMLIndentLN( <x> ) => ( Space( ::nIndent ) + <x> + CRLF() )

/****
*
*     Class THtmlDocument()
*
*/

CLASS THtmlDocument FROM THtmlBase

   METHOD New()            CONSTRUCTOR

   METHOD Info()

   METHOD PageBegin()
   METHOD PageEnd()

   METHOD Image()
   METHOD FlashObject()
   METHOD Anchor()
   METHOD AnchorBegin()
   METHOD AnchorEnd()
   METHOD Comment()
   METHOD CommentBegin()
   METHOD CommentEnd()
   METHOD Span()
   METHOD Text()
   METHOD Bold( cText )       INLINE ::OutPut( "<b>" + cText + "</b>" )
   METHOD Underline( cText )  INLINE ::OutPut( "<u>" + cText + "</u>" )
   METHOD Paragraph()
   METHOD ParagraphBegin()
   METHOD ParagraphEnd()

   METHOD Space( n )          INLINE DEFAULT( n, 1 ), IIF( ::lOutPut, ::Write( HtmlSpace( n ) ), NIL ), HtmlSpace( n )
   METHOD LineBreak()         INLINE IIF( ::lOutPut, ::Write( '<br>' ), NIL ), '<br>'
   METHOD LineHR()            INLINE IIF( ::lOutPut, ::WriteLN( '<hr>' ), NIL ), '<hr>'

   METHOD TableInline()
   METHOD TableBegin()
   METHOD TableEnd()
   METHOD TableRowBegin()
   METHOD TableRowEnd()
   METHOD TableCaption()
   METHOD TableHeader()
   METHOD TableColumn()
   METHOD TableColumnBegin()
   METHOD TableColumnEnd()

   METHOD FormBegin()
   METHOD FormEnd()
   METHOD FormFieldSetBegin()
   METHOD FormFieldSetEnd()
   METHOD FormLegend()
   METHOD FormLabel()
   METHOD FormInput()
   METHOD FormGet()
   METHOD FormSayGet()
   METHOD FormReset()
   METHOD FormButton()
   METHOD FormImageButton()
   METHOD FormHidden()
   METHOD FormPassword()
   METHOD FormCheckBox()
   METHOD FormRadio()
   METHOD FormFile()

   METHOD FormSelectBegin()
   METHOD FormSelectOption()
   METHOD FormSelectEnd()

   METHOD FormTextArea()
   METHOD FormTextAreaBegin()
   METHOD FormTextAreaEnd()

   METHOD JavaCMD()
   METHOD JavaWinOpen()

   METHOD ParseTime( lNow )     INLINE DEFAULT( lNow, FALSE ), IIF( lNow, ::nParseTimeStop := Seconds(), NIL ), ::nParseTimeStop - ::nParseTimeStart
   METHOD ParseTimeStart()      INLINE ::nParseTimeStart := Seconds()
   METHOD ParseTimeStop()       INLINE ::nParseTimeStop  := Seconds()
   METHOD ParseTimeShow( lNow ) INLINE ::Text( ::ParseTime( lNow ) )

   HIDDEN:

   DATA nParseTimeStart    INIT 0
   DATA nParseTimeStop     INIT 0

   METHOD DisplayVars()
   METHOD DisplayHash()

ENDCLASS

METHOD New( cSessionName, lCGI, cTitle, cFile ) CLASS THtmlDocument

   ::Super:New( cSessionName, lCGI, cTitle, cFile )

RETURN Self

METHOD Info() CLASS THtmlDocument
  ::DisplayVars( ::h_Server , "SERVER Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Get    , "GET Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Post   , "POST Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Cookie , "COOKIE Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Files  , "FILE Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Request, "REQUEST Vars" )
  ::WriteLN( "<br>" )
  ::DisplayVars( ::h_Session, "SESSION Vars" )
RETURN Self

METHOD DisplayVars( hHash, cTitle ) CLASS THtmlDocument
  LOCAL lOld := ::SetOutPut( TRUE )
  ::TableBegin( "90%",, "center", 1 )
  ::TableCaption( cTitle )
    ::TableRowBegin()
      ::TableHeader( "KEY"  , "20%" )
      ::TableHeader( "VALUE", "80%" )
    ::TableRowEnd()
    ::DisplayHash( hHash )
  ::TableEnd()
  ::SetOutPut( lOld )
RETURN NIL

METHOD DisplayHash( hHash ) CLASS THtmlDocument
  LOCAL cKey, cSubKey
  LOCAL cHTML := ""

  FOR EACH cKey IN hHash:Keys
     cHTML += ::TableRowBegin()
     IF hHash[ cKey ]:ClassName == "HASH"
        cHTML += ::TableColumn( cKey )
        cHTML += ::TableColumn( "-------" )
        FOR EACH cSubKey IN hHash[ cKey ]:Keys
           cHTML += ::TableRowBegin()
           cHTML += ::TableColumn( cSubKey )
           cHTML += ::TableColumn( hHash[ cKey ][ cSubKey ] )
           cHTML += ::TableRowEnd()
        NEXT
     ELSE
        cHTML += ::TableColumn( cKey )
        cHTML += ::TableColumn( hHash[ cKey ] )
     ENDIF
     cHTML += ::TableRowEnd()
  NEXT
RETURN cHTML

/****
*
*     JavaCMD()
*
*     Inserts inline Javascript source
*
*/

METHOD JavaCMD( cCmd ) CLASS THtmlDocument
   LOCAL cHTML
   DEFAULT cCmd TO ""
   ::Indent( 2 )
   cHTML := HTMLIndentLN( '<script language="JavaScript1.2">' + CRLF() + ;
                          "<!--" + CRLF() + ;
                          cCmd  + CRLF() + ;
                          "//-->" + CRLF() + ;
                          "</script>" + CRLF() )

   ::UnIndent( 2 )
RETURN ::OutPut( cHTML )

METHOD JavaWinOpen( cUrl, cParameters, cWinName, lFullScreen, lLocation, lToolBar, lDirectories, lMenuBar, nWidth, nHeight, ;
                    lResizable, lScrollBars, nLeft, nTop, cParams )
   LOCAL cHTML
   LOCAL cLink := ::HRef_Link_String( cUrl, cParameters )
   DEFAULT cWinName     TO "MyWin"
   DEFAULT lFullScreen  TO FALSE
   DEFAULT lLocation    TO FALSE
   DEFAULT lToolBar     TO FALSE
   DEFAULT lDirectories TO FALSE
   DEFAULT lMenuBar     TO FALSE
   DEFAULT nWidth       TO 400
   DEFAULT nHeight      TO 400
   DEFAULT lResizable   TO TRUE
   DEFAULT lScrollBars  TO TRUE
   // onClick="window.open('Shop-Articoli_Mostrafoto.asp?Immagine=1CBX094.jpg','IMMAGINE','toolbar=no,directories=no,menubar=no,width=300,height=300,resizable=yes,scrollbars=yes')"; onMouseOut="window.status=''; return true">
   cHTML := "javascript: mynewwin=window.open('" + cLink + "','" + cWinName + "','" +;
            "fullscreen="  + IIF( lFullScreen, "yes", "no" ) + "," + ;
            "location="    + IIF( lLocation, "yes", "no" ) + "," + ;
            "toolbar="     + IIF( lToolBar, "yes", "no" ) + "," + ;
            "directories=" + IIF( lDirectories, "yes", "no" ) + "," + ;
            "menubar="     + IIF( lMenuBar, "yes", "no" ) + "," + ;
            "width="       + AllTrim( Str( nWidth ) ) + "," + ;
            "height="      + AllTrim( Str( nHeight ) ) + "," + ;
            "resizable="   + IIF( lResizable, "yes", "no" ) + "," + ;
            "scrollbars="  + IIF( lScrollBars, "yes", "no" ) + ;
            IIF( nLeft <> NIL, "left=" + Alltrim( Str( nLeft ) ), "" ) + ;
            IIF( nTop  <> NIL, "top=" + Alltrim( Str( nTop ) ), "" ) + ;
            "'); mynewwin.focus(); " + ;
            IIF( cParams <> NIL, ' ' + cParams, '' )
            // onMouseOut="window.status=''; return true">] )
RETURN ::OutPut( cHTML )

// ------------------------------ ***************************** -----------------------------------

METHOD PageBegin( cTitle, cCharSet, cStyle ) CLASS THtmlDocument
  LOCAL cHTML := ""
  DEFAULT cCharSet TO "iso-8859-1"

  cHTML += HTMLIndentLN( "<html>" )
  cHTML += HTMLIndentLN( "<head>" )

  ::Indent( 2 )
  IF cTitle <> NIL
     cHTML += HTMLIndentLN( "<title>" + cTitle + "</title>" )
  ENDIF
  cHTML += HTMLIndentLN( '<meta http-equiv="Content-Type" content="text/html; charset="' + cCharSet + '">' )
  IF cStyle <> NIL
     cHTML += HTMLIndentLN( '<link rel="stylesheet" type="text/css" href="' + cStyle + '">' )
  ENDIF
  ::UnIndent( 2 )

  cHTML += HTMLIndentLN( "</head>" )
  cHTML += HTMLIndentLN( "<body>" )

  ::Indent( 2 )

RETURN ::OutPut( cHTML )

METHOD PageEnd() CLASS THtmlDocument
  LOCAL cHTML := ""

  ::UnIndent( 2 )
  cHTML += HTMLIndentLN( "</body>" )
  cHTML += HTMLIndentLN( "</html>" )

RETURN ::OutPut( cHTML )

METHOD Anchor( cText, chRef, cParameters, cTitle, cTarget, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML += ::AnchorBegin( chRef, cParameters, cTitle, cTarget, cClass, cParams )
  cHTML += HTMLIndent( cText )
  cHTML += ::AnchorEnd()
  ::nIndent := nIndent

RETURN ::OutPut( cHTML )

METHOD AnchorBegin( chRef, cParameters, cTitle, cTarget, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML += HTMLIndent( '<a' + ;
                         IIF( chRef   <> NIL, ' href="' + ::HRef_Link_String( chRef, cParameters ) + '"', ;
                         IIF( cParameters <> NIL, ' href="' + ::HRef_Link_String( "", cParameters ) + '"', '' ) ) + ;
                         IIF( cTitle  <> NIL, ' title="' + cTitle + '"', '' ) + ;
                         IIF( cTarget <> NIL, ' target="' + cTarget + '"', '' ) + ;
                         IIF( cClass  <> NIL, ' class="' + cClass + '"', '' ) + ;
                         IIF( cParams <> NIL, ' ' + cParams, '' ) + ;
                       '>' )
  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD AnchorEnd() CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML += HTMLIndent( '</a>' )
  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD Comment( cText ) CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML += ::CommentBegin()
  cHTML += HTMLIndent( cText )
  cHTML += ::CommentEnd()
  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD CommentBegin() CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML := HTMLIndent( '<!-- ' )
  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD CommentEnd() CLASS THtmlDocument
  LOCAL cHTML
  LOCAL nIndent := ::nIndent
  ::nIndent := 0
  cHTML := HTMLIndent( ' -->' )
  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD Text( cText, cParams ) CLASS THtmlDocument
  LOCAL cHTML := ""
  IF cParams <> NIL
     cHTML += "<div " + cParams + ">" + cText + "</div>"
  ELSE
     cHTML += cText
  ENDIF
RETURN ::OutPut( cHTML )

METHOD Paragraph( cText, cAlign, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<p' + ;
                           IIF( cAlign     <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' + cText + '</p>' )
RETURN ::OutPut( cHTML )

METHOD ParagraphBegin( cAlign, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<p' + ;
                           IIF( cAlign     <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
RETURN ::OutPut( cHTML )

METHOD ParagraphEnd() CLASS THtmlDocument
  LOCAL cHTML := HTMLIndentLN( '</p>' )
RETURN ::OutPut( cHTML )

METHOD Image( cUrl, cAlt, cAlign, nWidth, nHeight, nBorder, nHSpace, nVSpace, cLink, cTarget, cParams ) CLASS THtmlDocument
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent

  ::nIndent := 0
  DEFAULT nBorder TO 0

  IF cLink <> NIL
     cHTML += ::AnchorBegin( cLink,,, cTarget )
  ENDIF
  cHTML += HTMLIndent( '<img src="' + cUrl + '"' + ;
                         IIF( cAlt         <> NIL, ' alt="' + cAlt + '"', '' ) + ;
                         IIF( cAlign       <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                         IIF( nWidth       <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                         IIF( nHeight      <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                         IIF( nBorder      <> NIL, ' border="' + LTrim( cStr( nBorder ) ) + '"', '' ) + ;
                         IIF( nHSpace      <> NIL, ' hspace="' + LTrim( cStr( nHSpace ) ) + '"', '' ) + ;
                         IIF( nVSpace      <> NIL, ' vspace="' + LTrim( cStr( nVSpace ) ) + '"', '' ) + ;
                         IIF( cParams      <> NIL, ' ' + cParams, '' ) + ;
                       '>' )
  IF cLink <> NIL
     cHTML += ::AnchorEnd()
  ENDIF

  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD FlashObject( cFile, nWidth, nHeight, cQuality )
  LOCAL cHTML := ""
  LOCAL nIndent := ::nIndent

  ::nIndent += 2

  DEFAULT cQuality TO "high"

  cHTML += HTMLIndent( [<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,29,0" ] )
  cHTML += IIF( nWidth       <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
           IIF( nHeight      <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
           ">"

  cHTML += HTMLIndent( [<param name="movie" value="] + cFile + [">] )
  cHTML += HTMLIndent( [<param name="quality" value="] + cQuality + [">] )
  cHTML += HTMLIndent( [<embed src="] + cFile + [" quality="] + cQuality + [" pluginspage="http://www.macromedia.com/go/getflashplayer" type="application/x-shockwave-flash" ] )
  cHTML += IIF( nWidth       <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
           IIF( nHeight      <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
           ">"
  cHTML += HTMLIndent( [</embed>] )
  cHTML += HTMLIndent( [</object>] )

  ::nIndent -= 2

  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

METHOD Span( cText, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  LOCAL nIndent := ::nIndent

  ::nIndent := 0
  cHTML := HTMLIndent( '<span' + ;
                         IIF( cParams      <> NIL, ' ' + cParams, '' ) + ;
                       '>' + cText + '</span>' )

  ::nIndent := nIndent
RETURN ::OutPut( cHTML )

// ------------------------------ TABLE *********************** -----------------------------------

METHOD TableInline( cText, nWidth, nHeight, cAlign, nBorder, nCellSpacing, nCellPadding, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  DEFAULT nWidth TO "100%"
  cHTML := HTMLIndentLN( '<table' + ;
                          IIF( nWidth       <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                          IIF( nHeight      <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                          IIF( cAlign       <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                          IIF( nBorder      <> NIL, ' border="' + LTrim( cStr( nBorder ) ) + '"', '' ) + ;
                          IIF( nCellSpacing <> NIL, ' cellspacing="' + LTrim( cStr( nCellSpacing ) ) + '"', '' ) + ;
                          IIF( nCellPadding <> NIL, ' cellpadding="' + LTrim( cStr( nCellPadding ) ) + '"', '' ) + ;
                          IIF( cClass       <> NIL, ' class="' + cClass + '"', '' ) + ;
                          IIF( cParams      <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
    cHTML += HTMLIndentLN( '<tr>' )
    ::Indent( 2 )

       cHTML += HTMLIndentLN( '<td>' + IIF( Empty( cText ), HtmlSpace( 1 ), cStr( cText ) ) + '</td>' )

    ::UnIndent( 2 )
    cHTML += HTMLIndentLN( '</tr>' )

  ::UnIndent( 2 )
  cHTML += HTMLIndentLN( '</table>' )
RETURN ::OutPut( cHTML )

METHOD TableBegin( nWidth, nHeight, cAlign, nBorder, nCellSpacing, nCellPadding, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  DEFAULT nBorder      TO 0
  DEFAULT nCellSpacing TO 0
  DEFAULT nCellPadding TO 0
  cHTML := HTMLIndentLN( '<table' + ;
                          IIF( nWidth       <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                          IIF( nHeight      <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                          IIF( cAlign       <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                          IIF( nBorder      <> NIL, ' border="' + LTrim( cStr( nBorder ) ) + '"', '' ) + ;
                          IIF( nCellSpacing <> NIL, ' cellspacing="' + LTrim( cStr( nCellSpacing ) ) + '"', '' ) + ;
                          IIF( nCellPadding <> NIL, ' cellpadding="' + LTrim( cStr( nCellPadding ) ) + '"', '' ) + ;
                          IIF( cClass       <> NIL, ' class="' + cClass + '"', '' ) + ;
                          IIF( cParams      <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
RETURN ::OutPut( cHTML )

METHOD TableEnd() CLASS THtmlDocument
  LOCAL cHTML
  ::UnIndent( 2 )
  cHTML := HTMLIndentLN( '</table>' )
RETURN ::OutPut( cHTML )

METHOD TableCaption( cCaption, cAlign, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<caption' + ;
                          IIF( cAlign      <> NIL, ' valign="' + Lower( cAlign ) + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' + cCaption + '</caption>' )
RETURN ::OutPut( cHTML )

METHOD TableRowBegin( cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<tr' + ;
                          IIF( cClass      <> NIL, ' class="' + cClass + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
RETURN ::OutPut( cHTML )

METHOD TableRowEnd() CLASS THtmlDocument
  LOCAL cHTML
  ::UnIndent( 2 )
  cHTML := HTMLIndentLN( '</tr>' )
RETURN ::OutPut( cHTML )

METHOD TableHeader( xVal, nWidth, nHeight, cBgColor, cAlign, cVAlign, nRowSpan, nColSpan, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndent( '<th' + ;
                        IIF( nWidth   <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                        IIF( nHeight  <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                        IIF( cBgcolor <> NIL, ' bgcolor="' + cBgcolor + '"'    , '' ) + ;
                        IIF( cAlign   <> NIL, ' align="' + Lower( cAlign ) + '"'        , '' ) + ;
                        IIF( cVAlign  <> NIL, ' valign="' + Lower( cVAlign ) + '"'      , '' ) + ;
                        IIF( nRowSpan <> NIL, ' rowspan="' + LTrim( cStr( nRowSpan ) ) + '"', '' ) + ;
                        IIF( nColSpan <> NIL, ' colspan="' + LTrim( cStr( nColSpan ) ) + '"', '' ) + ;
                        IIF( cClass   <> NIL, ' class="' + cClass + '"', '' ) + ;
                        IIF( cParams  <> NIL, ' ' + cParams, '' ) + ;
                       '>' + cStr( xVal ) + '</th>' + CRLF() )
RETURN ::OutPut( cHTML )

METHOD TableColumn( xVal, nWidth, nHeight, cBgColor, cAlign, cVAlign, nRowSpan, nColSpan, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndent( '<td' + ;
                        IIF( nWidth   <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                        IIF( nHeight  <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                        IIF( cBgcolor <> NIL, ' bgcolor="' + cBgcolor + '"'    , '' ) + ;
                        IIF( cAlign   <> NIL, ' align="' + Lower( cAlign ) + '"'        , '' ) + ;
                        IIF( cVAlign  <> NIL, ' valign="' + Lower( cVAlign ) + '"'      , '' ) + ;
                        IIF( nRowSpan <> NIL, ' rowspan="' + LTrim( cStr( nRowSpan ) ) + '"', '' ) + ;
                        IIF( nColSpan <> NIL, ' colspan="' + LTrim( cStr( nColSpan ) ) + '"', '' ) + ;
                        IIF( cClass   <> NIL, ' class="' + cClass + '"', '' ) + ;
                        IIF( cParams  <> NIL, ' ' + cParams, '' ) + ;
                       '>' + IIF( Empty( xVal ), HtmlSpace( 1 ), cStr( xVal ) ) + '</td>' + CRLF() )
RETURN ::OutPut( cHTML )

METHOD TableColumnBegin( nWidth, nHeight, cBgColor, cAlign, cVAlign, nRowSpan, nColSpan, cClass, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<td' + ;
                          IIF( nWidth   <> NIL, ' width="' + LTrim( cStr( nWidth ) ) + '"', '' ) + ;
                          IIF( nHeight  <> NIL, ' height="' + LTrim( cStr( nHeight ) ) + '"', '' ) + ;
                          IIF( cBgcolor <> NIL, ' bgcolor="' + cBgcolor + '"'    , '' ) + ;
                          IIF( cAlign   <> NIL, ' align="' + Lower( cAlign ) + '"'        , '' ) + ;
                          IIF( cVAlign  <> NIL, ' valign="' + Lower( cVAlign ) + '"'      , '' ) + ;
                          IIF( nRowSpan <> NIL, ' rowspan="' + LTrim( cStr( nRowSpan ) ) + '"', '' ) + ;
                          IIF( nColSpan <> NIL, ' colspan="' + LTrim( cStr( nColSpan ) ) + '"', '' ) + ;
                          IIF( cClass   <> NIL, ' class="' + cClass + '"', '' ) + ;
                          IIF( cParams  <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
RETURN ::OutPut( cHTML )

METHOD TableColumnEnd() CLASS THtmlDocument
  LOCAL cHTML := HTMLIndentLN( '</td>' )
RETURN ::OutPut( cHTML )

// ------------------------------ FORM  *********************** -----------------------------------

METHOD FormBegin( cAction, cMethod, cEncType, cName, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  DEFAULT cMethod TO "POST"
  DEFAULT cAction TO ""
  cAction := ::HRef_Link_String( cAction )
  cHTML := HTMLIndentLN( '<form' + ;
                          IIF( cAction     <> NIL, ' action="' + cAction + '"', '' ) + ;
                          IIF( cMethod     <> NIL, ' method="' + cMethod + '"', '' ) + ;
                          IIF( cEncType    <> NIL, ' enctype="' + cEncType + '"', '' ) + ;
                          IIF( cName       <> NIL, ' name="' + cName + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
RETURN ::OutPut( cHTML )

METHOD FormEnd() CLASS THtmlDocument
  LOCAL cHTML
  ::UnIndent( 2 )
  cHTML := HTMLIndentLN( '</form>' )
RETURN ::OutPut( cHTML )

METHOD FormFieldSetBegin( cLegend, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML

  cHTML := HTMLIndentLN( '<fieldset' + ;
                          IIF( cID         <> NIL, ' id="' + Lower( cStr( cID ) ) + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
  IF cLegend <> NIL THEN cHTML += ::FormLegend( cLegend )
RETURN ::OutPut( cHTML )

METHOD FormFieldSetEnd() CLASS THtmlDocument
  LOCAL cHTML
  ::UnIndent( 2 )
  cHTML := HTMLIndentLN( '</fieldset>' )
RETURN ::OutPut( cHTML )

METHOD FormLegend( cValue, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<legend' + ;
                           IIF( cID        <> NIL, ' id="' + cID + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' + cStr( cValue ) + '</legend>' )
RETURN ::OutPut( cHTML )

METHOD FormLabel( cValue, cFor, cAccessKey, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<label' + ;
                           IIF( cFor       <> NIL, ' for="' + cFor + '"', '' ) + ;
                           IIF( cAccessKey <> NIL, ' accesskey=' + cAccessKey, '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' + cValue + '</label>' )
RETURN ::OutPut( cHTML )

METHOD FormInput( cType, cName, cValue, lChecked, nSize, nMaxLenght, cSrc, cAlign, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<input type="' + cType + '"' + ;
                           IIF( cName      <> NIL, ' name="' + cName + '"', '' ) + ;
                           IIF( !Empty( cValue ), ' value="' + Trim( cValue ) + '"', '' ) + ;
                           IIF( lChecked   <> NIL .AND. lChecked, ' checked', '' ) + ;
                           IIF( nSize      <> NIL, ' size="' + LTrim( cStr( nSize ) ) + '"', '' ) + ;
                           IIF( nMaxLenght <> NIL, ' maxlenght=' + LTrim( cStr( nMaxLenght ) ) + '', '' ) + ;
                           IIF( cSrc       <> NIL, ' src="' + cSrc + '"', '' ) + ;
                           IIF( cAlign     <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                           IIF( cID        <> NIL, ' id="' + Lower( cStr( DEFAULT( cID, cName ) ) ) + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
RETURN ::OutPut( cHTML )

METHOD FormGet( cName, cValue, cPicture, nSize, nMaxLenght, cAlign, cId, cParams ) CLASS THtmlDocument
  LOCAL xValue := IIF( cPicture <> NIL, Transform( cValue, cPicture ), cValue )
RETURN ::FormInput( "TEXT", cName, cStr( xValue ),, IIF( nSize == NIL, Len( cStr( xValue ) ), nSize ), nMaxLenght,, cAlign, cID, cParams )

METHOD FormSayGet( cLabel, cName, cValue, cPicture, nSize, nMaxLenght, cAlign, cId, cParams ) CLASS THtmlDocument
  LOCAL cHTML := ""
  cHTML += ::FormLabel( cLabel, cId )
  cHTML += ::FormGet( cName, cValue, cPicture, nSize, nMaxLenght, cAlign, cId, cParams )
RETURN cHTML

METHOD FormButton( cValue, cName, cAlign, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "SUBMIT", cName, cStr( cValue ),,,,, cAlign, cID, cParams )

METHOD FormReset( cValue, cName, cAlign, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "RESET", cName, cStr( cValue ),,,,, cAlign, cID, cParams )

METHOD FormImageButton( cSrc, cName, cAlign, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "IMAGE", cName,,,,, cSrc, cAlign, cID, cParams )

METHOD FormHidden( cName, cValue, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "HIDDEN", cName, cStr( cValue ),,,,,, cID, cParams )

METHOD FormPassword( cName, cValue, nSize, nMaxLenght, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "PASSWORD", cName, cStr( cValue ),, IIF( nSize == NIL, Len( cValue ), nSize ), nMaxLenght,,, cID, cParams )

METHOD FormCheckBox( cName, cValue, lChecked, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "CHECKBOX", cName, cStr( cValue ), lChecked,,,,, cID, cParams )

METHOD FormRadio( cName, cValue, lChecked, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "RADIO", cName, cStr( cValue ), lChecked,,,,, cID, cParams )

METHOD FormFile( cName, cValue, nSize, nMaxLenght, cID, cParams ) CLASS THtmlDocument
RETURN ::FormInput( "FILE", cName, cStr( cValue ),, IIF( nSize == NIL, Len( cValue ), nSize ), nMaxLenght,,, cID, cParams )

METHOD FormSelectBegin( cSize, cName, lMultiple, cID, ncTabIndex, cParams ) CLASS THtmlDocument
  LOCAL cHTML

  DEFAULT cSize TO "1"

  cHTML := HTMLIndentLN( '<select' + ;
                          IIF( cSize       <> NIL, ' size="' + AllTrim( cStr( cSize ) ) + '"', '' ) + ;
                          IIF( cName       <> NIL, ' name="' + cName + '"', '' ) + ;
                          IIF( lMultiple   <> NIL, IIF( lMultiple, ' multiple', '' ), '' ) + ;
                          IIF( cID         <> NIL, ' id="' + cID + '"', '' ) + ;
                          IIF( ncTabIndex  <> NIL, ' tabindex="' + AllTrim( cStr( ncTabIndex ) ) + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
  ::Indent( 2 )
RETURN ::OutPut( cHTML )

METHOD FormSelectOption( cText, cValue, lSelected, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML

  cHTML := HTMLIndentLN( '<option' + ;
                          IIF( lSelected   <> NIL, IIF( lSelected, ' selected', '' ), '' ) + ;
                          IIF( cValue      <> NIL, ' value="' + AllTrim( cStr( cValue ) ) + '"', '' ) + ;
                          IIF( cID         <> NIL, ' id="' + AllTrim( cStr( cID ) ) + '"', '' ) + ;
                          IIF( cParams     <> NIL, ' ' + cParams, '' ) + ;
                         '>' + ;
                          IIF( cText       <> NIL, cText, '' ) + ;
                         '</option>' )
RETURN ::OutPut( cHTML )

METHOD FormSelectEnd() CLASS THtmlDocument
  LOCAL cHTML
  ::UnIndent( 2 )
  cHTML := HTMLIndentLN( '</select>' )
RETURN ::OutPut( cHTML )


METHOD FormTextArea( cName, cValue, nRows, nCols, lDisabled, lReadOnly, cAccessKey, nTabIndex, cAlign, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<textarea ' + ;
                           IIF( cName      <> NIL, ' name="' + cName + '"', '' ) + ;
                           IIF( nRows      <> NIL, ' rows="' + LTrim( cStr( nRows ) ) + '"', '' ) + ;
                           IIF( nCols      <> NIL, ' cols="' + LTrim( cStr( nCols ) ) + '"', '' ) + ;
                           IIF( lDisabled  <> NIL .AND. lDisabled, ' disabled', '' ) + ;
                           IIF( lReadOnly  <> NIL .AND. lReadOnly, ' readonly', '' ) + ;
                           IIF( cAccessKey <> NIL, ' accesskey="' + cAccessKey + '"', '' ) + ;
                           IIF( nTabIndex  <> NIL, ' tabindex="' + LTrim( cStr( nTabIndex ) ) + '"', '' ) + ;
                           IIF( cAlign     <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                           IIF( cID        <> NIL, ' id="' + Lower( cStr( cID ) ) + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' + cValue + '</textarea>' )
RETURN ::OutPut( cHTML )

METHOD FormTextAreaBegin( cName, nRows, nCols, lDisabled, lReadOnly, cAccessKey, nTabIndex, cAlign, cID, cParams ) CLASS THtmlDocument
  LOCAL cHTML
  cHTML := HTMLIndentLN( '<textarea ' + ;
                           IIF( cName      <> NIL, ' name="' + cName + '"', '' ) + ;
                           IIF( nRows      <> NIL, ' rows="' + LTrim( cStr( nRows ) ) + '"', '' ) + ;
                           IIF( nCols      <> NIL, ' cols="' + LTrim( cStr( nCols ) ) + '"', '' ) + ;
                           IIF( lDisabled  <> NIL .AND. lDisabled, ' disabled', '' ) + ;
                           IIF( lReadOnly  <> NIL .AND. lReadOnly, ' readonly', '' ) + ;
                           IIF( cAccessKey <> NIL, ' accesskey="' + cAccessKey + '"', '' ) + ;
                           IIF( nTabIndex  <> NIL, ' tabindex="' + LTrim( cStr( nTabIndex ) ) + '"', '' ) + ;
                           IIF( cAlign     <> NIL, ' align="' + Lower( cAlign ) + '"', '' ) + ;
                           IIF( cID        <> NIL, ' id="' + Lower( cStr( cID ) ) + '"', '' ) + ;
                           IIF( cParams    <> NIL, ' ' + cParams, '' ) + ;
                         '>' )
RETURN ::OutPut( cHTML )

METHOD FormTextAreaEnd() CLASS THtmlDocument
  LOCAL cHTML := HTMLIndentLN( '</textarea>' )
RETURN ::OutPut( cHTML )

// ------------------------------ ***************************** -----------------------------------

