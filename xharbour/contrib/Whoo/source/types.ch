// TYPES

#ifndef _DEFINE_TYPES_

  #xcommand TYPEDEFINE <typename> = (<list,...>) => GLOBAL EXTERNAL <list>

  #xcommand TYPEDEFINE <typename> = (<list,...>) AS VALUES =>

#else

  #ifdef _TYPE_GLOBAL_DECLARATION_
     #xcommand TYPEDEFINE <typename> = (<list,...>) => GLOBAL <list>
     #xcommand TYPEDEFINE <typename> = (<list,...>) AS VALUES =>
  #else
     #xcommand TYPEDEFINE <typename> = (<list,...>) => hb_declareType( <"typename">, {<"list">} )
     #xcommand TYPEDEFINE <typename> = (<list,...>) AS VALUES => hb_declareTypeAsValues( <"typename">, {<list>} )
  #endif

#endif

// ------------------------------------------------------------------------

// TMenuItemAutoFlag = (maAutomatic, maManual, maParent);
// TMenuAutoFlag = maAutomatic..maManual;

TYPEDEFINE TMenuItemAutoFlag = (maAutomatic, maManual, maParent)
TYPEDEFINE TMenuAutoFlag = (maAutomatic, maManual) AS VALUES

// ------------------------------------------------------------------------

// TMenuBreak = (mbNone, mbBreak, mbBarBreak);

TYPEDEFINE TMenuBreak = (mbNone, mbBreak, mbBarBreak)

// ------------------------------------------------------------------------

// type TTrackButton = (tbRightButton, tbLeftButton);

TYPEDEFINE TTrackButton = (tbRightButton, tbLeftButton)

// ------------------------------------------------------------------------

// type TPopupAlignment = (paLeft, paRight, paCenter);

TYPEDEFINE TPopupAlignment = (paLeft, paRight, paCenter)

// ------------------------------------------------------------------------

// type TBiDiMode = (bdLeftToRight, bdRightToLeft, bdRightToLeftNoAlign, bdRightToLeftReadingOnly);

TYPEDEFINE TBiDiMode = (bdLeftToRight, bdRightToLeft, bdRightToLeftNoAlign, bdRightToLeftReadingOnly)

// ------------------------------------------------------------------------

//  type TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);

TYPEDEFINE TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent)

// ------------------------------------------------------------------------

//  type TImageType = (itImage, itMask);

TYPEDEFINE TImageType = (itImage, itMask)

// ------------------------------------------------------------------------

//  type TResType = (rtBitmap, rtCursor, rtIcon);

TYPEDEFINE TResType = (rtBitmap, rtCursor, rtIcon)

// ------------------------------------------------------------------------

//  type TOverlay = 0..3;

TYPEDEFINE TOverlay = ( 0, 1, 2, 3 ) AS VALUES

// ------------------------------------------------------------------------

//  TLoadResource = (lrDefaultColor, lrDefaultSize, lrFromFile, lrMap3DColors, lrTransparent, lrMonoChrome);
//  TLoadResources = set of TLoadResource;
//  TImageIndex = type Integer;

TYPEDEFINE TLoadResource = (lrDefaultColor, lrDefaultSize, lrFromFile, lrMap3DColors, lrTransparent, lrMonoChrome)

// ------------------------------------------------------------------------

TYPEDEFINE TFindItemKind = (fkCommand, fkHandle, fkShortCut)

// fkCommand    Command ID used by Windows WM_COMMAND message matches the Value parameter.
// fkHandle     Pop-up menu handle matches the Value parameter.
// fkShortCut   Menu shortcut key code matches the Value parameter.

// ------------------------------------------------------------------------

TYPEDEFINE TWindowState = (wsNormal, wsMinimized, wsMaximized)

// wsNormal     The form is in its normal state (that is, neither minimized nor maximized).
// wsMinimized  The form is minimized.
// wsMaximized  The form is maximized.

// ------------------------------------------------------------------------

TYPEDEFINE TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop)

// Use FormStyle to get or set the form's style. FormStyle is one of the following values:
//
// fsNormal     The form is neither an MDI parent window nor an MDI child window.
// fsMDIChild   The form is an MDI child window.
// fsMDIForm    The form is an MDI parent window.
// fsStayOnTop  This form remains on top of the desktop and of other forms in the project, except any others that also have FormStyle set to fsStayOnTop.If one fsStayOnTop form launches another, neither form will consistently remain on top.
//
// If the form is the main form of an MDI application, its FormStyle property must be set to fsMDIForm.

// ------------------------------------------------------------------------

TYPEDEFINE TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp)

// Use BorderIcons to get or set the icons that appear on the title bar of the form. BorderIcons can include any of the following TBorderIcons values:
//
// biSystemMenu The form has a Control menu (also known as a System menu).
// biMinimize   The form has a Minimize button
// biMaximize   The form has a Maximize button
// biHelp       If BorderStyle is bsDialog or biMinimize and biMaximize are excluded, a question mark appears in the form's title bar and when clicked, the cursor changes to crHelp; otherwise,no question mark appears.
//
// Note:    Certain combinations of the BorderIcons and BorderStyle properties are mutually exclusive. For example, BorderIcons biMax, biMin with BorderStyle of bsDialog are mutually exclusive.

// ------------------------------------------------------------------------

TYPEDEFINE TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, ;
                        poScreenCenter, poDesktopCenter, poMainFormCenter, ;
                        poOwnerFormCenter)

// Use Position to get or set the size and placement of the form. Position can have one of the following TPosition values:
//
// poDesigned          The form appears positioned on the screen and with the same height and width as it had at design time.
// poDefault           The form appears in a position on the screen and with a height and width determined by the operating system. Each time you run the application, the form moves slightly down and to the right. The right side of the form is always near the far right side of the screen, and the bottom of the form is always near the bottom of the screen, regardless of the screen's resolution.
// poDefaultPosOnly	The form displays with the size you created it at design time, but the operating system chooses its position on the screen. Each time you run the application, the form moves slightly down and to the right. When the form can no longer move down and to the right and keep the same size while remaining entirely visible on the screen, the form displays at the top-left corner of the screen.
// poDefaultSizeOnly	The form appears in the position you left it at design time, but the operating system chooses its size. The right side of the form is always near the far right side of the screen, and the bottom of the form is always near the bottom of the screen, regardless of the screen's resolution.
// poScreenCenter      The form remains the size you left it at design time, but is positioned in the center of the screen. In multi-monitor applications, the form may be moved from this center position so that it falls entirely on one monitor, as specified by the DefaultMonitor property.
// poDesktopCenter     The form remains the size you left it at design time, but is positioned in the center of the screen. No adjustments are made for multi-monitor applications.
// poMainFormCenter    The form remains the size you left it at design time, but is positioned in the center of the application's main form. No adjustments are made for multi-monitor applications. This position should only be used with secondary forms. If set for a main form, it acts like poScreenCenter.
// poOwnerFormCenter   The form remains the size you left it at design time, but is positioned in the center of the form specified by the Owner property. If the Owner property does not specify a form, this position acts like poMainFormCenter.

// ------------------------------------------------------------------------

TYPEDEFINE TDefaultMonitor = (dmDesktop, dmPrimary, dmMainForm, dmActiveForm)

// Use DefaultMonitor to associate a form with a particular monitor in a multi-monitor application. The following table lists the possible values:
//
// dmDesktop    No attempt is made to position the form on a specific monitor.
// dmPrimary    The form is positioned on the first monitor listed in the global screen object's Monitors property.
// dmMainForm   The form appears on the same monitor as the application's main form.
// dmActiveForm The form appears on the same monitor as the currently active form.
//
// Note:	DefaultMonitor has no effect if the application does not have a main form.
//

// ------------------------------------------------------------------------

TYPEDEFINE TPrintScale = (poNone, poProportional, poPrintToFit)

// Use PrintScale to get or set the proportions of the printed form. PrintScale can have one of the following TPrintScale values:
//
// poNone           No special scaling occurs; therefore, the printed form and how the form appears onscreen may appear squished or stretched.
// poProportional   The form is printed so that the printed image is approximately the same visible size as on the screen (WYSIWYG). The form image is scaled so that the printed image is approximately the same visible size as on the screen.
// poPrintToFit     The form is printed using the same screen proportions, but in a size that just fits the printed page.

// ------------------------------------------------------------------------

TYPEDEFINE TShowAction = (saIgnore, saRestore, saMinimize, saMaximize)

// ------------------------------------------------------------------------

TYPEDEFINE TTileMode = (tbHorizontal, tbVertical)

// ------------------------------------------------------------------------

TYPEDEFINE TCloseAction = (caNone, caHide, caFree, caMinimize)

// ------------------------------------------------------------------------

TYPEDEFINE TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin)
TYPEDEFINE TBorderStyle = (bsNone,bsSingle) AS VALUES

// These are the possible values of the TFormBorderStyle type:
//
// bsNone          No visible border. (not resizable)
// bsSingle        Single-line border. (not resizable)
// bsDialog        Standard dialog box border. (not resizable)
// bsSizeable      Standard resizable border.
// bsToolWindow    Like bsSingle but with a smaller caption.
// bsSizeToolWin   Like bsSizeable but with a smaller caption.
//
// Only the bsNone and bsSingle values are possible values of TBorderStyle.

// ------------------------------------------------------------------------

TYPEDEFINE  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase)
TYPEDEFINE  TControlStyle = (csAcceptsControls, csCaptureMouse,;
    csDesignInteractive, csClickEvents, csFramed, csSetCaption, csOpaque,;
    csDoubleClicks, csFixedWidth, csFixedHeight, csNoDesignVisible,;
    csReplicatable, csNoStdEvents, csDisplayDragImage, csReflector)
