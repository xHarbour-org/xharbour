/*
 * $Id: listbox.prg,v 1.14 2003/09/19 17:05:49 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Listbox class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modIFy
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
 * along with this software; see the file COPYING.  IF not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  IF you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modIFied files, you must delete
 * this exception notice from them.
 *
 * IF you write modIFications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modIFications.
 * IF you do not wish that, delete this exception notice.
 *
 */

#include 'hbclass.ch'
#include 'common.ch'
#include "box.ch"
#IFdef HB_COMPAT_C53
Class HBListBox

    Method New( nTop, nLeft, nBottom, nRigth, lDrop )

    MESSAGE Select( nPos ) Method SELECTS( nPos )
    Method AddItem( cText, xValue )
    Method Close()
    Method DelItem( nPos )
    Method Display()
    Method FindText( cText, nPos, lCaseSensitive, lExact )
    Method FindData( cText, nPos, lCaseSensitive, lExact )
    Method GetData( xItem )
    Method GetItem( nPos )
    Method GetText( nPos )
    Method HitTest( n, p )
    Method InsItem( nPos, cText, xVal )
    Method KillFocus()
    Method NextItem()
    Method Open()
    Method PrevItem()
    MESSAGE Scroll( n ) Method _Scroll( n )

    Method SetData( nPos, xValue )
    Method SetFocus()
    Method SetItem( nPos, aitem )
    Method SetText( nPos, xValue )
    DATA ClassName Init "LISTBOX"
    DATA Buffer
    DATA CapCol
    DATA CapRow
    DATA Cargo Init NIL
    DATA HasFocus Init .T.
    DATA ItemCount Init 0
    DATA Left Init 0
    DATA Message Init ''
    DATA TextValue Init ''
    DATA Style Init ""
    DATA sBlock Init NIL
    DAta fBlock Init Nil
    DATA Hotbox Init ""
    Data ColorSpec Init ""
    DATA ColdBox
    Data ISOPEN Init .f.
    Data aItems Init {}
    Data vScrolls

    DATA Value Init 0
    Data Top Init 0
    Data right Init 0
    data Bottom Init 0
    Data TopItem Init 1
    Data dropdown Init .f.
    ACCESS nTop inline ::SetTop()
    ASSIGN nTop( xData ) inline ::SetTop( xData )
    ACCESS vScroll inline ::vScrolls
    ASSIGN vScroll( xData ) inline ::SetScroll( xData )
    ACCESS NRight inline ::SetRight()
    ASSIGN nRight( xData ) inline ::SetRight( xData )
    ACCESS lDropDown inline ::SetDropDown()
    ASSIGN lDropDown( xData ) inline ::SetDropDown( xData )
    ACCESS caption inline ::SetCaption()
    ASSIGN Caption( xData ) inline ::SetCaption( xData )
    ACCESS nBottom inline ::SetBottom()
    ASSIGN nBottom( xData ) inline ::SetBottom( xData )
    ACCESS nTopItem inline ::SetTopItem()
    ASSIGN nTopItem( xTop ) inline ::SetTopItem( xTop )
    ACCESS TypeOut inline ::itemCount == 0
    ASSIGN TypeOut( x ) inline IIF( x != nil, x, ::itemCount == 0 )

  Hidden:

    Method SetScroll( xData )
    Data xTop Init 0
    Method SetTop( xData )
    Data xRight Init 0
    Method SetRight( xData )
    DATA xDropDown Init .f.
    Method SetDropDown( xData )
    Data cCaption Init ''
    Method SetCaption( xData )
    Data xBottom Init 0
    Method SetBottom( xData )
    Data aScreen Init NIL
    DATA nCursor Init 0
    DATA xtopItem Init 0
    Method SetTopItem( xTop )
Endclass

Method New( nTop, nLeft, nBottom, nRigth, lDrop )

     Local ccolor

     ::ClassName := 'LISTBOX'
     ::Bottom    := nBottom
     ::nBottom   := nBottom
     ::right     := nRigth
     ::nright    := nRigth
     ::Top       := nTop
     ::ntop      := nTop
     ::left      := nleft
     ::Buffer    := Nil
     ::Caption   := ""
     ::CapCol    := nleft
     ::CapRow    := nTop
     ::Cargo     := Nil
     ::ColdBox   := B_SINGLE
     IF ( Isdefcolor() )
        ::Colorspec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
     ELSE
        cColor      := Setcolor()
        ::Colorspec := __guicolor( cColor, 5 ) + "," + ;
                                   __guicolor( cColor, 5 ) + "," + __guicolor( cColor, 5 ) + ;
                                   "," + __guicolor( cColor, 2 ) + "," + __guicolor( cColor, ;
                                   3 ) + "," + __guicolor( cColor, 1 ) + "," + ;
                                   __guicolor( cColor, 4 )
     ENDIF
     ::isopen    := !lDrop
     ::aItems    := {}
     ::dropdown  := lDrop
     ::ldropdown := lDrop
     ::fBlock    := Nil
     ::hasfocus  := .F.

     ::hotbox    := B_DOUBLE
     ::itemCount := 0

     ::message := ""

     ::ascreen := Str( nTop + 1, 2 ) + Str( nleft, 2 ) + Str( nBottom, ;
                       2 ) + Str( nRigth, 2 ) + Savescreen( nTop + 1, nleft, nBottom, nRigth )



     ::sBlock    := Nil
     ::nCursor   := Nil
     ::Style     := Chr( 240 )
     ::TextValue := ""

     ::Topitem  := 0
     ::nTopItem := 0
     ::vScroll  := Nil
     ::Value    := 0

     RETURN SELF
/**** Get/Set Datas ****/

Method SetScroll( xData ) Class HBListBox

     IF ( ISOBJECT( xData ) ) /*.and. xData:Classname=="SCROLLBAR" .and. xData:orient==1)*/
        ::vScrolls  := xData
        xData:total := ::iTemCount
     ENDIF

RETURN ::vScrolls

Method SetTop( xData ) Class HBListBox

     Local nTop

     IF ( !( ISNIL( xData ) .and. ISNUMBER( xData ) ) .and. ISNUMBER( ( ::xTop := xData ) ) .and. ISOBJECT( ::vScroll ) )
        ::vScroll:start := xData + 1
     ENDIF

RETURN ::xTop

Method SetRight( xData ) Class HBListBox

     IF ( !( ISNIL( xData ) ) .and. ISOBJECT( ( ::xRight := xData, ::vScroll ) ) )
        ::vScroll:offset := xData
     ENDIF

RETURN ::xRight

Method SetDropDown( xData ) Class HBListBox

     IF ( !( ISNIL( xData ) ) ) .and. ISLOGICAL( xData )
        ::xDropDown := xData

        IF xData
        ELSEIF ( !::isOpen )
           ::isOpen := .T.
        ENDIF
       ::display()
     ENDIF

RETURN ::xDropDown

Method SetCaption( xData ) Class HBListBox

     IF ( ISCHARACTER( xData ) .and. ISNIL( ::Capcol ) )
        ::cCaption := xData
        ::Caprow   := ::top
        ::Capcol   := ::left - Len( xData )
     ENDIF

RETURN ::cCaption

Method SetBottom( xData ) Class HBListBox

     Local nBottom

     IF ( !( ISNIL( xData ) .and. ISNUMBER( xData ) ) .and. ISNUMBER( ( ::xBottom := xData ) ) .and. ISOBJECT( ( ::vScroll ) ) )
        nBottom       := ::xBottom
        ::vScroll:end := xData - 1
     ENDIF

RETURN ::xBottom

/*** Class Methods ***/

Method ADDITEM( cText, xValue ) Class HBListBox

     IF ( !( ISCHARACTER( cText ) ) )
     ELSEIF ( Valtype( xValue ) IN "CU" )
        Aadd( ::aItems, { cText, xValue } )
        ::iTemCount ++

        IF ( ::iTemCount == 1 .and. ISOBJECT( ( ::Topitem := 1, ::nTopItem := 1, ::vScroll ) ) )
           ::vScroll:total := ( ::iTemCount - ( ::bottom - ;
                                ::top - 2 ) )
        ENDIF

     ENDIF

RETURN SELF

Method Close() Class HBListBox

     Local Local1
     Local Local2
     Local Local3
     Local cColor
     Local Local5

     IF ( ::isOpen )

        Restscreen( Val( Substr( ::aScreen, 1, 2 ) ), ;
                    Val( Substr( ::aScreen, 3, 2 ) ), ;
                    Val( Substr( ::aScreen, 5, 2 ) ), ;
                    Val( Substr( ::aScreen, 7, 2 ) ), Substr( ::aScreen, ;
                    9 ) )
        ::isOpen  := .F.
        ::aScreen := Nil

     ENDIF

RETURN SELF

Method DELITEM( xitem )

     IF ( xitem < 1 )
     ELSEIF ( xitem <= ::iTemCount )
        Adel( ::aItems[ xitem ], .T. )
//        Asize( ::aItems, -- ::iTemCount )
        ::iTemCount--

        IF ( ::Value > ::iTemCount )
           ::Value := ::iTemCount

           IF ( ::Value == 0 )
              ::TextValue := ""
           ELSE
              ::TextValue := _Getdata( ::aItems[ ::iTemCount ] )
           ENDIF

           IF ( ISNIL( ::Buffer ) )
           ELSEIF ( ISNUMBER( ::Buffer ) )
              ::Buffer := ::iTemCount
           ELSEIF ( ::Value > 0 )
              ::Buffer := ::TextValue
           ENDIF

        ENDIF

        IF ( ::Topitem > ::iTemCount )
           ::Topitem  := ::iTemCount
           ::nTopitem := ::iTemCount
        ENDIF

        IF ( ISOBJECT( ::vScroll ) )
           ::vScroll:total := ::iTemCount - ( ::Bottom - ;
                                              ::top - 2 )
        ENDIF

     ENDIF

RETURN SELF

Method Getdata( xData ) Class HBListBox

     Local xRet := Nil

     IF ( xData < 1 )
     ELSEIF ( xData <= ::itemCount )
        xRet := ::aitems[ xData, 2 ]
     ENDIF

RETURN xRet

Method FindData( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

     Local nPosFound
     Local lOldExact
     Local nStart
     Local nEnd
     Local nSize

     IF ( ISLOGICAL( lExact ) )
        lOldExact := Set( _SET_EXACT, lExact )
     ENDIF

     nEnd := 1

     IF ( ISNUMBER( nPos ) )
        nEnd ++
     ELSE
        nPos := 1
     ENDIF

     nSize := Len( ::aitems ) - nPos + 1

     IF ( !( ISLOGICAL( lCaseSensitive ) ) )
        lCaseSensitive := .T.
     ENDIF

     FOR nStart := 1 TO nEnd

        IF ( lCaseSensitive )

           IF ( Set( _SET_EXACT ) )
              nPosFound := Ascan( ::aitems, { | _1 | _Getdata( _1 ) == cText ;
                      }, nPos, nSize )
           ELSE
              nPosFound := Ascan( ::aitems, { | _1 | _Getdata( _1 ) = cText ;
                      }, nPos, nSize )
           ENDIF

        ELSEIF ( Set( _SET_EXACT ) )
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _Getdata( _1 ) ) == ;
                   Lower( cText ) }, nPos, nSize )
        ELSE
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _Getdata( _1 ) ) = ;
                   Lower( cText ) }, nPos, nSize )
        ENDIF

        IF ( nPosFound > 0 )
           EXIT
        ENDIF

        nSize := nPos - 1
        nPos  := 1
     NEXT

     IF ( !( ISNIL( lOldExact ) ) )
        Set Exact ( lOldExact )
     ENDIF

RETURN nPosFound

Method FindText( cText, nPos, lCaseSensitive, lExact ) Class HBListBox

     Local nPosFound
     Local lOldExact
     Local nStart
     Local nEnd
     Local nSize

     IF ( ISLOGICAL( lExact ) )
        lOldExact := Set( _SET_EXACT, lExact )
     ENDIF

     nEnd := 1

     IF ( ISNUMBER( nPos ) )
        nEnd ++
     ELSE
        nPos := 1
     ENDIF

     nSize := Len( ::aitems ) - nPos + 1

     IF ( !( ISLOGICAL( lCaseSensitive ) ) )
        lCaseSensitive := .T.
     ENDIF

     FOR nStart := 1 TO nEnd
        IF ( lCaseSensitive )

           IF ( Set( _SET_EXACT ) )
              nPosFound := Ascan( ::aitems, { | _1 | _1[ 1 ] == cText ;
                      }, nPos, nSize )

           ELSE
              nPosFound := Ascan( ::aitems, { | _1 | _1[ 1 ] == cText ;
                      }, nPos, nSize )
           ENDIF

        ELSEIF ( Set( _SET_EXACT ) )
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _1[ 1 ] ) == ;
                   Lower( cText ) }, nPos, nSize )
        ELSE
           nPosFound := Ascan( ::aitems, { | _1 | Lower( _1[ 1 ] ) = ;
                   Lower( cText ) }, nPos, nSize )
        ENDIF

        IF ( nPosFound > 0 )
           EXIT
        ENDIF

        nSize := nPos - 1
        nPos  := 1
     NEXT

     IF ( !( ISNIL( lOldExact ) ) )
        Set Exact ( lOldExact )
     ENDIF

RETURN nPosFound

Method NEXTITEM() Class HBListBox

     Local nCurValue
     Local nValue

     IF ( !::hasfocus )
     ELSEIF ( ::itemCount > 0 )

        IF ( ( nCurValue := ::value ) == ::itemCount )
           nValue := nCurValue
        ELSE
           nValue := nCurValue + 1
        ENDIF

        changeitem( SELF, nCurValue, nValue )

     ENDIF

RETURN SELF

Method PREVITEM() Class HBListBox

     Local nCurValue
     Local nValue

     IF ( !::hasfocus )
     ELSEIF ( ::itemCount > 0 )

        IF ( ( nCurValue := ::value ) == 0 )
           nValue := 1
        ELSEIF ( nCurValue == 1 )
           nValue := nCurValue
        ELSE
           nValue := nCurValue - 1
        ENDIF

        changeitem( SELF, nCurValue, nValue )

     ENDIF

RETURN SELF

Method _SCROLL( nMethod ) Class HBListBox

     Local nPos
     Local nTopItem
     Local nCount
     Local nThumbPos
     Local nCurrent
     Local nBarLength
     Local nTotal
     Local nSize
     Local nMouRow
     Local nMouseRow
     Local nKey
     Local nStart

     Switch nMethod
         CASE -3074
             IF ( ::topitem > 1 )
                ::topitem --
                ::vScroll:current := lbadjustcu( SELF )
                SELF:display()
             ENDIF
             EXIT

         CASE -3075
             IF ( ( ::topitem + ::bottom - ::top ) <= ::itemCount + 1 )
                ::topitem ++
                ::vScroll:current( lbadjustcu( SELF ) )
                SELF:display()
             ENDIF
             EXIT

         CASE -3077
             nPos     := ::bottom - ::top - 1
             nCount   := ::itemCount
             nTopItem := ::topitem + nPos
             IF ( ::topitem < nCount - nPos + 1 )
                IF ( nTopItem + nPos - 1 > nCount )
                   nTopItem := nCount - nPos + 1
                ENDIF
                ::topitem  := nTopItem
                ::ntopitem := nTopItem
                ::vScroll:current( lbadjustcu( SELF ) )
                SELF:display()
             ENDIF
             EXIT

         CASE -3076
             nPos := ::bottom - ::top - IIF( ::bitmap, 2, ;
                     1 )
             nCount   := ::itemCount
             nTopItem := ::topitem - nPos
             IF ( ::topitem > 1 )
                IF ( nTopItem < 1 )
                   nTopItem := 1
                ENDIF
                ::topitem  := nTopItem
                ::ntopitem := nTopItem
                ::vScroll:current( lbadjustcu( SELF ) )
                SELF:display()
             ENDIF
             EXIT

         CASE -3073
             nMouseRow := Mrow()
             Do While ( ( nKey := Inkey( 0 ) ) != 1003 )
               IF ( nKey == 1001 )
                  nMouRow := Mrow()
                  IF ( nMouRow <=::vScroll:start() )
                     nMouRow :=::vScroll:start() + 1
                  ENDIF
                  IF ( nMouRow >=::vScroll:end() )
                     nMouRow :=::vScroll:end() - 1
                  ENDIF
                  IF ( nMouRow != nMouseRow )
                     nThumbPos :=::vScroll:thumbpos() + ( nMouRow - ;
                                                   nMouseRow )
                     nBarLength :=::vScroll:barlength()
                     nTotal     :=::vScroll:total()
                     nSize      := ( nThumbPos * ( nTotal - nBarLength - 2 ) + 2 * ;
                                     nBarLength + 1 - nTotal ) / ( nBarLength - 1 )
                     IF ( nSize < 1 )
                        nSize := 1
                     ENDIF
                     IF ( nSize > nTotal )
                        nSize := nTotal
                     ENDIF
                     nCurrent :=::vScroll:current()
                     IF ( nSize - nCurrent > 0 )
                        FOR nStart := 1 TO nSize - nCurrent
                           SELF:scroll( - 3075 )
                        NEXT
                     ELSE
                        FOR nStart := 1 TO nCurrent - nSize
                           SELF:scroll( - 3074 )
                        NEXT
                     ENDIF
                     nMouseRow := nMouRow
                  ENDIF
               ENDIF
             Enddo
             EXIT
     End
RETURN SELF

Method SELECTS( nPosition ) Class HBListBox

     Local nValue
     Local nPos
     Local xType := Valtype( nPosition )

     Do CASE
         CASE xType == "C"
             nPos := SELF:finddata( nPosition )
             IF ( !( Valtype( ::buffer ) IN "CU" ) )
                ::buffer := nPos
             ELSEIF ( ::value == 0 )
                ::buffer := nPosition
             ELSE
                ::buffer := _Getdata( ::aitems[ nPos ] )
             ENDIF
         CASE !( xType == "N" )
             RETURN ::value
         CASE nPosition < 1
             RETURN ::value
         CASE nPosition > ::itemCount
             RETURN ::value
         CASE nPosition == ::value
             RETURN ::value
         Otherwise
             nPos := nPosition
             IF ( Valtype( ::buffer ) IN "NU" )
                ::buffer := nPos
             ELSEIF ( nPos == 0 )
                ::buffer := ""
             ELSE
                ::buffer := _Getdata( ::aitems[ nPos ] )
             ENDIF
     ENDCASE
     ::value := nPos
     IF ( nPos == 0 )
        ::textvalue := ""
     ELSE
        ::textvalue := _Getdata( ::aitems[ nPos ] )
     ENDIF
     IF ( Empty( ::hotbox + ::coldbox ) )
        nPos := 0
     ELSE
        nPos := 2
     ENDIF
     nValue := ::value - ( ::bottom - ::top - nPos )
     IF ( ::topitem <= nValue )
        ::topitem  := nValue
        ::ntopitem := nValue
        IF ( ISOBJECT( ::vScroll ) )
           ::vScroll:current := lbadjustcu( SELF )
        ENDIF
     ELSEIF ( ::value == 0 )
     ELSEIF ( ::topitem > ::value .and. ISOBJECT( ( ;
                 ::topitem := ::value, ::ntopitem := ::value, ::vScroll ) ) )
        ::vScroll:current := lbadjustcu( SELF )
     ENDIF
     SELF:display()
     IF ( ISBLOCK( ::sBlock ) )
        Eval( ::sBlock )
     ENDIF
RETURN ::value

Method SetTOPITEM( xData ) Class HBListBox

     Local nSize
     Local nPos
     IF ( !( ISNIL( xData ) ) ) .and. xData > 0 .and. xData <= ::itemCount

        IF ( Empty( ::hotbox + ::coldbox ) )
           nPos := 0
        ELSE
           nPos := 2
        ENDIF
        nSize := ::itemCount - ( ::bottom - ::top - ;
                                 nPos )
        IF ( xData > nSize )
           xData := nSize
        ENDIF
        IF ( ::topitem != xData )
           ::xtopitem := xData
           IF ( ISOBJECT( ::vScroll ) )
              ::vScroll:current := lbadjustcu( SELF )
           ENDIF
           SELF:display()
        ENDIF
     ENDIF
RETURN ::xtopitem

Method Display() Class HBListBox

     Local nCurRow       := Row()
     Local nCurCol       := Col()
     Local cCurrentColor := Setcolor()
     Local nStart
     Local nEnd
     Local cColor4
     Local cColor3
     Local nTop          := ::top
     Local nLeft         := ::left
     Local nSize
     Local cHotBox
     Local cCaption
     Local nAmpPos
     Local cColorAny
     nSize := ::right - nLeft + 1

     IF ( ::hasfocus )
        cHotBox := ::hotbox
        cColor3 := __guicolor( ::colorspec, 3 )
        cColor4 := __guicolor( ::colorspec, 4 )

        IF ( ::isopen )
           cColorAny := __guicolor( ::colorspec, 2 )
        ELSE
           cColorAny := __guicolor( ::colorspec, 4 )
        ENDIF

     ELSE
        cHotBox   := ::coldbox
        cColor3   := __guicolor( ::colorspec, 1 )
        cColor4   := __guicolor( ::colorspec, 2 )
        cColorAny := __guicolor( ::colorspec, 2 )

     ENDIF

     Dispbegin()
     nEnd := ::topitem + ::bottom - ::top

     IF ( ::dropdown )
        SET COLOR TO (cColorAny)
        Setpos( nTop ++, nLeft )

        IF ( ::value == 0 )
           ?? Space( nSize - 1 )
        ELSE
           ?? Padr( ::aitems[ ::value, 1 ], nSize - 1 )
        ENDIF

        SET COLOR TO (__guicolor(::colorspec, 8))
        ?? Left( ::style, 1 )
        nEnd --

     ENDIF

     IF ( ::isopen )

        IF ( !Empty( cHotBox ) )

           SET COLOR TO (__guicolor(::colorspec, 5))
           @ nTop, nLeft clear TO ::bottom, ::right
           @ nTop, nLeft, ::bottom, ::right Box cHotBox

           IF ( ISOBJECT( ::vScroll ) )
              ::vScroll:display()
           ENDIF

           nTop ++
           nLeft ++
           nSize -= 2
           nEnd  -= 2

        ENDIF

        IF ( nEnd > ::itemCount )
           nEnd := ::itemCount
        ENDIF

        FOR nStart := ::topitem TO nEnd

           IF ( nStart == ::value )
              SET COLOR TO (cColor4)
           ELSE
              SET COLOR TO (cColor3)
           ENDIF

           Setpos( nTop ++, nLeft )
           ?? Padr( ::aitems[ nStart, 1 ], nSize )

        NEXT

     ENDIF

     IF ( !Empty( cCaption := ::caption ) )

        IF ( ( nAmpPos := At( "&", cCaption ) ) == 0 )
        ELSEIF ( nAmpPos == Len( cCaption ) )
           nAmpPos := 0
        ELSE
           cCaption := Stuff( cCaption, nAmpPos, 1, "" )
        ENDIF

        SET COLOR TO (__guicolor(::colorspec, 6))
        Setpos( ::caprow, ::capcol - 1 )
        ?? cCaption

        IF ( nAmpPos != 0 )
           SET COLOR TO (__guicolor(::colorspec, 7))
           Setpos( ::caprow, ::capcol + nAmpPos - 2 )
           ?? Substr( cCaption, nAmpPos, 1 )
        ENDIF

     ENDIF

     Dispend()

     SET COLOR TO (cCurrentColor)
     Setpos( nCurRow, nCurCol )

RETURN SELF

Method GETITEM( xItem ) Class HBListBox

     Local xRet := Nil

     IF ( xItem < 1 )
     ELSEIF ( xItem <= ::itemCount )
        xRet := ::aitems[ xItem ]
     ENDIF

RETURN xRet

Method GETTEXT( xItem ) Class HBListBox

     Local xRet := Nil

     IF ( xItem < 1 )
     ELSEIF ( xItem <= ::itemCount )
        xRet := ::aitems[ xItem, 1 ]
     ENDIF

RETURN xRet

Method INSITEM( nPosition, cText, xExp )

     IF ( !( ISCHARACTER( cText ) ) )
     ELSEIF ( !( ISNUMBER( nPosition ) ) )
     ELSEIF ( nPosition < ::itemCount )
//        Asize( ::aitems, ++ ::itemCount )
        ::itemCount++
        Ains( ::aitems, nPosition, { cText, xExp }, .T. )
//        ::aitems[ nPosition ] := { cText, xExp }

        IF ( ::itemCount == 1 )
           ::topitem  := 1
           ::ntopitem := 1
        ENDIF

        IF ( ISOBJECT( ::vScroll ) )
           ::vScroll:total := ::itemCount - ( ::bottom - ;
                                              ::top - 2 )
        ENDIF

     ENDIF
RETURN SELF

Method HITTEST( nMouseRow, nMouseCol ) Class HBListBox

     Local Local1
     Local Local2 := 0
     Local Local3
     Local cColor

     IF ( !::isopen )
     ELSEIF ( !( ISOBJECT( ::vScroll ) ) )
     ELSEIF ( ( Local2 :=::vScroll:hittest( nMouseRow, nMouseCol ) ) != 0 )
        RETURN Local2
     ENDIF

     IF ( !::isopen .or. Empty( ::hotbox + ::coldbox ) )
        Local1 := 0
     ELSE
        cColor := ::top
        IF ( ::DropDown )
           cColor ++
        ENDIF

        Do CASE
            CASE nMouseRow == cColor
                IF ( nMouseCol == ::left )
                   RETURN - 1
                ELSEIF ( nMouseCol == ::right )
                   RETURN - 3
                ELSEIF ( nMouseCol >= ::left .and. nMouseCol <= ::right )
                   RETURN - 2
                ENDIF
            CASE nMouseRow == ::bottom
                IF ( nMouseCol == ::left )
                   RETURN - 7
                ELSEIF ( nMouseCol == ::right )
                   RETURN - 5
                ELSEIF ( nMouseCol >= ::left .and. nMouseCol <= ::right )
                   RETURN - 6
                ENDIF
            CASE nMouseCol == ::left
                IF ( nMouseRow >= ::top .and. nMouseRow <= ::bottom )
                   RETURN - 8
                ELSE
                   RETURN 0
                ENDIF
            CASE nMouseCol == ::right
                IF ( nMouseRow >= ::top .and. nMouseRow <= ::bottom )
                   RETURN - 4
                ELSE
                   RETURN 0
                ENDIF
        ENDCASE
        Local1 := 1
     ENDIF

     Do CASE
         CASE !::isopen
         CASE nMouseRow < cColor + Local1
         CASE nMouseRow > ::bottom - Local1
         CASE nMouseCol < ::left + Local1
         CASE nMouseCol <= ::right - Local1
             RETURN ::topitem + nMouseRow - ( cColor + Local1 )
     ENDCASE

     Do CASE
         CASE !::dropdown
         CASE nMouseRow != ::top
         CASE nMouseCol < ::left
         CASE nMouseCol < ::right
             RETURN - 2049
         CASE nMouseCol == ::right
             RETURN - 4097
     ENDCASE

     Do CASE
         CASE Empty( ::caption )
         CASE nMouseRow != ::caprow
         CASE nMouseCol < ::capcol
         CASE nMouseCol < ::capcol + __CapLength( ::caption )
             RETURN - 1025
     ENDCASE

RETURN 0

Method KillFocus() Class HBListBox

     Local Local1

     IF ( ::hasfocus )
        ::hasfocus := .F.

        IF ( ISBLOCK( ::fblock ) )
           Eval( ::fblock )
        ENDIF

        Dispbegin()

        IF ( ::dropdown .and. ::isopen )
           ::close()
        ENDIF

        ::display()
        Dispend()

        Setcursor( ::nCursor )

     ENDIF

RETURN SELF

Method Open() Class HBListBox

     IF ( !::isopen )

        ::ascreen := Str( ::top + 1, 2 ) + ;
                          Str( ::left, 2 ) + Str( ::bottom, 2 ) + ;
                          Str( ::right, 2 ) + Savescreen( ::top + 1, ;
                          ::left, ::bottom, ::right )
        ::isopen := .T.
        SELF:display()

     ENDIF
RETURN SELF

Method SetText( nPos, cText ) Class HBListBox

     IF ( nPos < 1 )
     ELSEIF ( nPos <= ::itemCount )
        ::aitems[ nPos, 1 ] := cText
     ENDIF
RETURN SELF

Method SetItem( nPos, cText ) Class HBListBox

     Do CASE
         CASE nPos < 1
         CASE nPos > ::itemCount
         CASE Len( cText ) != 2
         CASE ISCHARACTER( cText[ 1 ] )
             ::aitems[ nPos ] := cText
     ENDCASE
RETURN SELF

Method SETFOCUS() Class HBListBox

     IF ( !::hasfocus )
        ::nCursor  := Setcursor( 0 )
        ::hasfocus := .T.
        Dispbegin()
           ::display()
        Dispend()

        IF ( ISBLOCK( ::fblock ) )
           Eval( ::fblock )
        ENDIF

     ENDIF

RETURN SELF

Method SetDAta( nPos, xData ) Class HBListBox

     IF ( !( nPos < 1 ) )
     ELSEIF ( nPos <= ::itemCount )
        ::aitems[ nPos, 2 ] := xData
     ENDIF

RETURN SELF

Static Function CHANGEITEM( oList, nPos, nItem )

     Local Local1
     Local Local2

     IF ( nPos != nItem )
        oList:value := nItem

        IF ( oList:value == 0 )
           oList:Textvalue := ""
        ELSE
           oList:Textvalue := _Getdata( oList:aItems[ oList:value ] )
        ENDIF

        IF ( ISNIL( oList:Buffer ) )
        ELSEIF ( ISNUMBER( oList:Buffer ) )
           oList:Buffer := oList:value
        ELSEIF ( oList:value > 0 )
           oList:Buffer := oList:Textvalue
        ENDIF

        IF ( Empty( oList:hotbox + oList:coldbox ) )
           Local2 := 0
        ELSE
           Local2 := 2
        ENDIF

        IF ( oList:Dropdown )
           Local2 ++
        ENDIF

        Local1 := oList:value - ( oList:Bottom - oList:top - Local2 )

        IF ( oList:Topitem > oList:value )
           oList:topitem := oList:value

           IF ( ISOBJECT( oList:vScroll ) )
              oList:vScroll:current := lbadjustcu( oList )
           ENDIF

        ELSEIF ( oList:topitem <= Local1 .and. ISOBJECT( ( oList:topitem := Local1, ;
                    oList:vScroll ) ) )
           oList:vScroll:current := lbadjustcu( oList )
        ENDIF

        oList:display()

        IF ( ISBLOCK( oList:sBlock ) )
           Eval( oList:sBlock )
        ENDIF

     ENDIF
RETURN oList

Static Function LBADJUSTCU( oList )

     Local nSize
     Local nCount
     Local nLength
     Local nTopItem
     Local nNewSize

     nSize    := oList:Bottom - oList:top - IIF( oList:dropdown, 2, 1 )
     nCount   := oList:itemCount
     nLength  := oList:vScroll:barlength
     nTopItem := oList:Topitem
     nNewSize := ( ( nCount - nLength ) * nTopItem + nLength - nSize ) / ( ;
                   nCount - nSize )
RETURN nNewSize

Function Listbox( nTop, nLeft, nBottom, nRigth, lDrop )

     IF !( ISNUMBER( nTop ) ) .or. !( ISNUMBER( nleft ) ) .or. !( ISNUMBER( nBottom ) ) .or. !( ISNUMBER( nRigth ) )
        RETURN nil
     ENDIF

RETURN HBListBox():New( nTop, nLeft, nBottom, nRigth, lDrop )

Static Function _Getdata( xItem )

     IF ( ISNIL( xItem[ 2 ] ) )
        RETURN xItem[ 1 ]
     ENDIF

RETURN xItem[ 2 ]

Function _LISTBOX_( Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, ;
                         Arg9, Arg10, Arg11, Arg12, Arg13 )

     Local oScroll
     Local nPos
     Local nLen
     Local nCurPos
     DEFAULT arg5 TO 1
     DEFAULT arg12 TO .f.
     DEFAULT arg13 TO .f.
     DEFAULT arg7 TO ''
     oScroll := Listbox( Arg1, Arg2, Arg3, Arg4, Arg12 )

     IF ( !( ISNIL( oScroll ) ) )

        IF ( ISCHARACTER( Arg7 ) )
           oScroll:capcol := NIL
           oScroll:caption := Arg7
           oScroll:capcol  := Arg2 - __CapLength( Arg7 )
        ENDIF

        IF arg9 != nil
           oScroll:colorspec := Arg9
        ENDIF

        oScroll:message := Arg8
        oScroll:fblock  := Arg10
        oScroll:sblock  := Arg11

        nLen            := Len( Arg6 )

        FOR nPos := 1 TO nLen
           nCurPos := Arg6[ nPos ]

           IF ( !( ISARRAY( nCurPos ) ) )
              oScroll:additem( nCurPos )
           ELSEIF ( Len( nCurPos ) == 1 )
              oScroll:additem( nCurPos[ 1 ] )
           ELSE
              oScroll:additem( nCurPos[ 1 ], nCurPos[ 2 ] )
           ENDIF

        NEXT

        IF ( !( ISNIL( Arg13 ) ) .and. Arg13 )

           IF ( !( ISLOGICAL( Arg12 ) ) )
           ELSEIF ( Arg12 )
              Arg1 ++
           ENDIF

           oScroll:vscroll := Scrollbar( Arg1 + 1, Arg3 - 1, Arg4,, 1 )

        ENDIF

        oScroll:select( Arg5 )

     ENDIF

RETURN oScroll

Function __CAPLENGTH( Arg1 )

     Local Local1 := Len( Arg1 )
     Local Local2

     IF ( ( Local2 := At( "&", Arg1 ) ) == 0 )
     ELSEIF ( Local2 < Local1 )
        Local1 --
     ENDIF

RETURN Local1

#ENDIF

*+ EOF: LISTBOX.PRG
