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

CLASS TDialogTemplate

    DATA nDlgVer     AS NUMERIC INIT 1
    DATA nSignature  AS NUMERIC INIT 0xFFFF        // Extended mode
    DATA nHelpID     AS LONG    INIT 0
    DATA nExStyle    AS LONG    INIT 0
    DATA nStyle      AS LONG
    ACCESS nDlgItems INLINE Len( ::aoItems )
    DATA nRow
    DATA nCol
    DATA nWidth
    DATA nHeight
    DATA nMenu       AS NUMERIC INIT 0
    DATA cClassName  AS STRING  INIT ""
    DATA cTitle      AS STRING  INIT ""

    DATA nPointSize  AS NUMERIC INIT 0
    DATA nWeight     AS NUMERIC INIT 0
    DATA lItalic     AS LOGICAL INIT FALSE
    DATA nCharSet    AS NUMERIC INIT 0
    DATA cFace       AS STRING  INIT ""

    PROTECTED:
    DATA aoItems     AS ARRAY   INIT {}

    EXPORT:
    // Public Methods
    METHOD New()         CONSTRUCTOR

ENDCLASS

METHOD New( cTitle, nRow, nCol, nWidth, nHeight ) CLASS TDialogTemplate

    ASSIGN ::nExStyle     WITH WS_EX_DLGMODALFRAME //+ WS_EX_CONTROLPARENT
    //ASSIGN ::cClassName   WITH "WoopGUIDlgClass"
    ASSIGN ::cTitle       WITH cTitle               DEFAULT "Dialog_1"
    ASSIGN ::nStyle       WITH WS_TABSTOP + WS_POPUP + WS_DLGFRAME + WS_BORDER
    ASSIGN ::nRow         WITH nRow                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nCol         WITH nCol                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth               DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight              DEFAULT CW_USEDEFAULT

RETURN Self

