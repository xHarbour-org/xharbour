/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
* From this class descends application window
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

CLASS WG_TDialogItem

    DATA nHelpID     AS NUMERIC    INIT 0
    DATA nExStyle    AS NUMERIC    INIT 0
    DATA nStyle      AS NUMERIC
    DATA nRow
    DATA nCol
    DATA nWidth
    DATA nHeight
    DATA nId
    DATA cClassName  AS NUMERIC INIT 0
    DATA cTitle      AS STRING  INIT ""

    METHOD New()         CONSTRUCTOR

ENDCLASS

METHOD New( cTitle, nRow, nCol, nWidth, nHeight, cClassName, nStyle ) CLASS WG_TDialogItem

    ASSIGN ::nExStyle     WITH 0
    //ASSIGN ::cClassName   WITH "WoopGUIDlgClass"
    ASSIGN ::cTitle       WITH cTitle               DEFAULT "DialogItem_1"
    ASSIGN ::nStyle       WITH WS_TABSTOP + WS_POPUP
    ASSIGN ::nRow         WITH nRow                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nCol         WITH nCol                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth               DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight              DEFAULT CW_USEDEFAULT
    ASSIGN ::cClassName   WITH cClassName           DEFAULT "BUTTON"

    // 0x0080 Button
    // 0x0081 Edit
    // 0x0082 Static
    // 0x0083 List box
    // 0x0084 Scroll bar
    // 0x0085 Combo box

RETURN Self

