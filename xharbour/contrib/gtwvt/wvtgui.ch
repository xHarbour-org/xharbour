/*
 * $Id: wvtgui.ch,v 1.2 2004/04/25 07:17:35 vouchcac Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the WVT commands
 *
 * Copyright 2004 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
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

/***
*  Clear screen
*/

#command CLS                                                            ;
      => Scroll()                                                       ;
       ; SetPos(0,0)                                                    ;
       ; WVT_Cls()


/***
*  @..BOX
*/

#command @ <top>, <left> TO <bottom>, <right>                           ;
      => WVT_DispBox(                                                   ;
                      <top>, <left>, <bottom>, <right>                  ;
                    )

#command @ <top>, <left> TO <bottom>, <right> OUTLINE                   ;
      => WVT_AddPaintList( {|| Wvt_DrawOutLine( <top>, <left>, <bottom>, <right> ) } )

#command @ <top>, <left> TO <bottom>, <right> RAISED                    ;
      => WVT_DispBox(                                                   ;
                      <top>, <left>, <bottom>, <right>, .T.             ;
                    )

#command @ <top>, <left> TO <bottom>, <right> RECESSED                  ;
      => WVT_DispBox(                                                   ;
                      <top>, <left>, <bottom>, <right>, .F.             ;
                    )

#command @ <top>, <left> TO <bottom>, <right> GROUPBOX [<raised: RAISED>];
      => WVT_DispGroupBox(                                              ;
                      <top>, <left>, <bottom>, <right>, <.raised.>      ;
                    )
/*
#command @ <top>, <left> TO <bottom>, <right> [<raised: RAISED>]        ;
      => WVT_DispBox(                                                   ;
                      <top>, <left>, <bottom>, <right>, <.raised.>      ;
                    )
*/

/***
*  @..IMAGE
*/
#command @ <top>, <left> TO <bottom>, <right> IMAGE <image>             ;
      => WVT_DispImage(                                                 ;
                      <top>, <left>, <bottom>, <right>, <image>         ;
                    )


/***
*  @..LABEL
*/
#xcommand @ <nRow>, <nCol> LABEL <cLabel>                               ;
             [ <cHorAlign: LEFT, CENTER, CENTERED, RIGHT> ]             ;
             [ <cVerAlign: TOP, BASELINE, BOTTOM> ]                     ;
             [ COLOR <ncFgColor> ]                                      ;
             [ BACKCOLOR <ncBgColor> ]                                  ;
             [ FONTNAME <cFontName> ]                                   ;
             [ FONTSIZE <nFontSize> ]                                   ;
             [ <it: ITALIC> ]                                           ;
             [ <ul: UNDERLINE> ]                                        ;
             [ <st: STRIKEOUT> ]                                        ;
      => WVT_DispLabel(                                                 ;
                         <nRow>, <nCol>, <cLabel>,                      ;
                         [<(cHorAlign)>], [<(cVerAlign)>],              ;
                         <ncFgColor>, <ncBgColor>,                      ;
                         <cFontName>, <nFontSize>,                      ;
                         <.it.>, <.ul.>, <.st.>                         ;
                      )



/***
*  @..SAY
*/
/*
#command @ <row>, <col>  SAY <xpr>;
                             [PICTURE <pic>];
                             [COLOR <color>];
=>;
         WVT_SAY( <row>, <col>, <xpr>, <pic>, <color> )

*/

/***
*  @..GET
*/

#command @ <row>, <col> GET <var>                                       ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => WVT_Get( <row>, <col>, <var>, <(var)>, <pic>, <{valid}>, <{when}>, [@<msg>], GetList )


/***
*   @..SAY..GET
*/

#command @ <row>, <col> SAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        GET <var>                                       ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> SAY <sayxpr> [<sayClauses>]                     ;
       ; @ Row(), Col()+1 GET <var> [<getClauses>]


/***
*  READ
*/
/*
#command READ SAVE                                                      ;
       => WVT_READ()

#command READ                                                           ;
      => WVT_READ()                                                     ;
       ; GetList := {}

#command CLEAR GETS                                                     ;
      => __KillRead()                                                   ;
       ; GetList := {}

*/

/***
*  SET FONT
*/

#command SET FONT <face>                                                ;
                 [SIZE <size>]                                          ;
                 [WIDTH <width>]                                        ;
                 [WEIGHT <weight>]                                      ;
                 [QUALITY <quality>]                                    ;
                                                                        ;
      => WVT_AddPaintList( {|| WVT_SetFont( <face>, <size>, <width>, <weight>, <quality> ) } )

/***
*  SAVE / RESTORE WINDOW
*/

#command SAVE WINDOW                    => WVT_PushWindow()
#command RESTORE WINDOW                 => WVT_PopWindow()

#command SAVE WINDOW TO <var>                                           ;
      => <var> := WVT_SaveWindow( 0, 0, Maxrow(), Maxcol() )

#command RESTORE WINDOW FROM <a>                                        ;
      => WVT_RestWindow( 0, 0, Maxrow(), Maxcol(), <a> )


/***
*  PRIMITIVES
*/

#command SET WINDOW TITLE TO <title>    => WVT_AddPaintList( {|| WVT_SetTitle( <title> ) } )
#command SET WINDOW ICON TO <iconfile>  => WVT_AddPaintList( {|| WVT_SetIcon( <iconfile> ) } )

#command SET BRUSH STYLE <nStyle>                                       ;
                  [COLOR <ncColor>]                                     ;
                  [HATCH <nHatch>]                                      ;
      => WVT_AddPaintList( {|| WVT_SetBrush( <nStyle>, IIF( <.ncColor.>, WVT_Color( <ncColor> ), ), <nHatch> ) } )

#command SET PEN   STYLE <nStyle>                                       ;
                  [WIDTH <nWidth>]                                      ;
                  [COLOR <ncColor>]                                     ;
      => WVT_AddPaintList( {|| WVT_SetPen( <nStyle>, <nWidth>, IIF( <.ncColor.>, WVT_Color( <ncColor> ), ) ) } )

#command DRAW ELLIPSE <nTop>, <nLeft>, <nBottom>, <nRight>              ;
      => WVT_AddPaintList( {|| Wvt_DrawEllipse( <nTop>, <nLeft>, <nBottom>, <nRight> ) } )

#command DRAW RECTANGLE <nTop>, <nLeft>, <nBottom>, <nRight>            ;
      => WVT_AddPaintList( {|| Wvt_DrawRectangle( <nTop>, <nLeft>, <nBottom>, <nRight> ) } )

#command DRAW ROUND RECTANGLE <nTop>, <nLeft>, <nBottom>, <nRight>      ;
                   [WIDTH <nWidth>]                                     ;
                   [HEIGHT <nHeight>]                                   ;
      => WVT_AddPaintList( {|| Wvt_DrawRoundRect( <nTop>, <nLeft>, <nBottom>, <nRight>, <nHeight>, <nWidth> ) } )

//-------------------------------------------------------------------//

