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
#include "winuser.ch"

CLASS TFrameMDIParent FROM TFrame

    // Data of this class only

    DATA oMDIClienWindow    AS OBJECT
    DATA oMDICurrentChild   AS OBJECT
    DATA oMDIMenu           AS OBJECT
    DATA lParentFrameActive AS LOGICAL INIT TRUE


    DATA aoMdiChilds   AS ARRAY INIT {} HIDDEN   // Childs mdi windows

    // Public Methods
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    //METHOD AddMdiChild( oMdi )       INLINE aAdd( ::aoMdiChilds, oMdi )
    METHOD ActivateNext()            VIRTUAL // FSG - to be implemented
    METHOD ActivatePrevious()        VIRTUAL // FSG - to be implemented
    METHOD ArrangeIcons()            VIRTUAL // FSG - to be implemented
    METHOD Cascade()                 VIRTUAL // FSG - to be implemented
    METHOD Create()                  VIRTUAL // FSG - to be implemented
    METHOD GetActiveChild()          VIRTUAL // FSG - to be implemented
    METHOD GetClientSize()           VIRTUAL // FSG - to be implemented
    METHOD GetClientWindow()         VIRTUAL // FSG - to be implemented
    METHOD GetToolBar()              VIRTUAL // FSG - to be implemented
    METHOD GetWindowMenu()           VIRTUAL // FSG - to be implemented

    METHOD OnCreateClient()          VIRTUAL // FSG - to be implemented

    METHOD SetToolBar()              VIRTUAL // FSG - to be implemented
    METHOD SetWindowMenu()           VIRTUAL // FSG - to be implemented
    METHOD Tile()                    VIRTUAL // FSG - to be implemented

ENDCLASS

METHOD New( cName, nStyle, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar ) CLASS TFrameMDIParent

    ASSIGN ::nExStyle     WITH 0 //WS_EX_MDICHILD
    ASSIGN ::cClassName   WITH "WoopGUIFrmClass"
    ASSIGN ::cName        WITH cName                   DEFAULT "Frame_1"
    ASSIGN ::nStyle       WITH nStyle                  DEFAULT WS_OVERLAPPEDWINDOW
    ASSIGN ::nTop         WITH nTop                    DEFAULT CW_USEDEFAULT
    ASSIGN ::nLeft        WITH nLeft                   DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth                  DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight                 DEFAULT CW_USEDEFAULT
    //ASSIGN ::nChild       WITH NULL
    //ASSIGN ::nApplication WITH NULL // ApplObj()
    //ASSIGN ::pStruct      WITH NULL
    ASSIGN ::lStatusBar   WITH lStatusBar           DEFAULT FALSE

    ::oMDIClientWindow := TWindow():New()

    ::Super:New( ::cName, ::nStyle, ::nTop, ::nLeft, ::nWidth, ::nHeight, oParent, lStatusBar )

RETURN Self

METHOD NewExtended( cTitle, nStyle, nTop, nLeft, nWidth, nHeight,;
                    oMenu, oBrush, oIcon, oParent, lStatusBar, ;
                    lvScroll, lhScroll, nClrFore, nClrBack, oCursor,;
                    cBorder, lNoSysMenu, lNoCaption,;
                    lNoIconize, lNoMaximize, lPixel ) CLASS TFrameMDIParent

   ::New( cTitle, nStyle, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar )

   IF ValType( oMenu ) == "O" THEN ::SetMenu( oMenu )
   // IF ValType( oBrush ) == "O" THEN ::SetBrush( oBrush )
   // IF ValType( oIcon ) == "O" THEN ::SetIcon( oIcon )

RETURN Self

METHOD Create()

RETURN Self
