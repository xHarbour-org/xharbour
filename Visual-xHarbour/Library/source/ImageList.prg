/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// ImageList.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include "commdlg.ch"
#Include "vxh.ch"

//----------------------------------------------------------------------------------------------------

CLASS ImageList INHERIT Component

   DATA Images      PUBLISHED INIT {}

   PROPERTY IconWidth   READ xIconWidth    WRITE __RefreshHandle DEFAULT 16
   PROPERTY IconHeight  READ xIconHeight   WRITE __RefreshHandle DEFAULT 16
   PROPERTY Palette     READ xPalette      WRITE __RefreshHandle DEFAULT ILC_COLOR32 //ILC_COLORDDB
   PROPERTY MaskColor   READ xMaskColor    WRITE __RefreshHandle

   DATA Handle      EXPORTED
   DATA Events      EXPORTED INIT {  {"General", { { "OnCreate"     , "", "" } } } }
   ACCESS Count INLINE ImageListGetImageCount( ::Handle )

   DATA __lAdd      PROTECTED INIT .F.

   METHOD Init()     CONSTRUCTOR
   METHOD Create()
   METHOD Destroy()
   METHOD Clean()
   METHOD AddBitmap()
   METHOD AddImage()
   METHOD AddIcon()
   METHOD Add( hImage, hMask )    INLINE ImageListAdd( ::Handle, hImage, hMask )
   METHOD RemoveImage(n)          INLINE ImageListRemove( ::Handle, n )
   METHOD ReplaceImage(n, hImage) INLINE ImageListReplaceIcon( ::Handle, n, hImage )
   METHOD DrawDisabled()
   METHOD DrawImage()
   METHOD FromToolBar( oBar ) INLINE ImageListDestroy( ::Handle ), ::Handle := oBar:SendMessage( TB_GETIMAGELIST, 0, 0 )

   METHOD GetImage( nIndex )  INLINE ImageListGetIcon( ::Handle, nIndex-1, ILD_NORMAL )

   METHOD __RefreshHandle()
   METHOD GetImages()
   
   METHOD BeginDrag(i,x,y)    INLINE ImageListBeginDrag( ::Handle, i, x, y )
   METHOD EndDrag()           INLINE ImageListEndDrag()
   METHOD DragMove(x,y)       INLINE ImageListDragMove( x, y )
   METHOD DragEnter(hWnd,x,y) INLINE ImageListDragEnter( hWnd, x, y )
   METHOD DragShowNolock(l)   INLINE ImageListDragShowNolock(l)
   METHOD RemoveAll()         INLINE ImageListRemoveAll( ::Handle )
   
   METHOD ComboBox( oParent ) INLINE __ImageListComboBox( oParent, Self )
   METHOD DrawIndirect()
   METHOD SaveImage(n, cFile) INLINE ImageListSaveImage( ::Handle, n-1, cFile )
ENDCLASS

//----------------------------------------------------------------------------------------------------

METHOD Init( oOwner, x, y, nPalette, lAdd ) CLASS ImageList
   ::__xCtrlName := "ImageList"
   ::ComponentType := "ImageList"
   ::ClsName := "ImageList"
   
   DEFAULT lAdd TO .T.
   IF lAdd
      ::Super:Init( oOwner )
    ELSE
      ::Owner := oOwner
   ENDIF
   DEFAULT nPalette TO ::Palette
   DEFAULT x        TO ::IconWidth
   DEFAULT y        TO ::IconHeight
   ::xIconWidth  := x
   ::xIconHeight := y
   ::xPalette    := nPalette
   ::__lAdd      := lAdd
RETURN SELF

//----------------------------------------------------------------------------------------------------

METHOD Create() CLASS ImageList
   LOCAL oComp, aImage, cEvent, nStyle := ::Palette | ILC_MASK

   IF VALTYPE( ::Form ) == "O"
      FOR EACH oComp IN ::Form:Components
          IF oComp:HasMessage( "ImageList" ) .AND. VALTYPE( oComp:ImageList ) == "C" .AND. UPPER( oComp:ImageList ) == UPPER( ::Name )
             oComp:ImageList := Self
          ENDIF
      NEXT
   ENDIF
   
   ::Handle := ImageListCreate( ::IconWidth, ::IconHeight, nStyle, 1, 0 )
   FOR EACH aImage IN ::Images
       ::AddImage( aImage[1], aImage[2], aImage[3], aImage[4], aImage[5], aImage[6], .F. )
   NEXT
   IF ::EventHandler != NIL .AND. HGetPos( ::EventHandler, "OnCreate" ) != 0
      cEvent := ::EventHandler[ "OnCreate" ]
      IF ::Form != NIL .AND. __objHasMsg( ::Form, cEvent )
         ::Form:&cEvent( Self )
      ENDIF
   ENDIF
RETURN Self

METHOD Destroy() CLASS ImageList
   ImageListDestroy( ::Handle )
   IF ::__lAdd
      ::Super:Destroy()
   ENDIF
RETURN NIL

//----------------------------------------------------------------------------------------------------

METHOD Clean( oControl ) CLASS ImageList
   LOCAL oChild
   TRY
      IF __objHasMsg( oControl, "IMAGELIST" )
         IF oControl:ImageList != NIL .AND. oControl:ImageList:Handle == ::Handle
            oControl:ImageList := NIL
         ENDIF
         FOR EACH oChild IN oControl:Children
             TRY
                oChild:ImageIndex := -1
              catch
             END
             ::Clean( oChild )
         NEXT
      ENDIF
    catch
  END
RETURN Self

//----------------------------------------------------------------------------------------------------

METHOD __RefreshHandle() CLASS ImageList
   IF ::Handle != NIL
      ImageListDestroy( ::Handle )
      ::Handle := NIL
      ::Create()
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------

METHOD AddImage( cImage, nMask, hInst, nLoad, nType, cFile, lAdd, lParser ) CLASS ImageList
   LOCAL hImage, hTool, tbb, nRet, hList, pImageInfo := (struct IMAGEINFO)
   DEFAULT lAdd TO .T.
   DEFAULT nType TO IMAGE_ICON
   DEFAULT lParser TO .F.

   hInst := ::AppInstance
   
   IF lParser
      cImage := cFile
      nLoad  := LR_LOADFROMFILE //| LR_CREATEDIBSECTION
   ENDIF

   IF ::__ClassInst != NIL 
      IF AT( "\", cImage ) == 0
         DirChange( ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Resource )
      ENDIF

      ::Application:Project:AddImage( cImage, nType, Self )
   ENDIF

   IF lAdd
      AADD( ::Images, { cImage, nMask, hInst, nLoad, nType, cFile } )
   ENDIF

   DEFAULT nMask TO ::MaskColor

   IF ::Handle != NIL
      IF VALTYPE( cImage ) == "N"

         //IF ::Form != NIL .AND. ::Form:hWnd == NIL
         //   ::Form:Create()
         //ENDIF

         hTool := CreateWindowEx( WS_EX_TOOLWINDOW, "ToolBarWindow32", "", WS_CHILD, 0, 0, 1500, ::IconHeight, IIF( ::Owner != NIL, IIF( ::Owner:Parent != NIL, ::Owner:Parent:hWnd, ::Owner:hWnd ), GetDesktopWindow()), 0, ::AppInstance )
         //hTool := CreateWindowEx( WS_EX_TOOLWINDOW, "ToolBarWindow32", "", WS_CHILD, 0, 0, 1500, ::IconHeight, ::Form:hWnd, 0, ::AppInstance )

         tbb := (struct TBBUTTON)
         SendMessage( hTool, TB_BUTTONSTRUCTSIZE, tbb:SizeOf(), 0 )
         SendMessage( hTool, TB_SETBITMAPSIZE, 0, MAKELPARAM( ::IconWidth, ::IconHeight ) )
         SendMessage( hTool, TB_LOADIMAGES, cImage, -1 )


         ShowWindow( hTool, SW_HIDE )

         hList := SendMessage( hTool, TB_GETIMAGELIST, 0, 0 )

         ImageListDestroy( ::Handle )
         ::Handle := hList

         DestroyWindow( hTool )

       ELSE
         IF ( hImage := LoadImage( hInst, cImage, nType,,, nLoad ) ) != 0
            nRet := hImage

            IF nType == IMAGE_ICON
               ImageListReplaceIcon(::Handle, -1, hImage )
               DestroyIcon( hImage )
             ELSE
               //IF nMask == NIL
               //   nMask := __GetPixelFromBMP( hImage )
               //ENDIF
               //::xMaskColor := nMask
               IF ::MaskColor != NIL
                  ImageListAddMasked( ::Handle, hImage, ::MaskColor )
                 ELSE
                  ImageListAdd( ::Handle, hImage, ::MaskColor )
               ENDIF
               DeleteObject( hImage )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
RETURN nRet

//----------------------------------------------------------------------------------------------------

METHOD AddBitmap( cImage, nMask, hInst, nLoad ) CLASS ImageList
   LOCAL hBmp
   hInst := ::AppInstance
   hBmp := LoadImage( hInst, cImage, IMAGE_BITMAP,,, nLoad )
   DEFAULT nMask TO ::MaskColor
   //IF nMask == NIL
   //   nMask := __GetPixelFromBMP( hBmp )
   //ENDIF
   //::xMaskColor := nMask
   IF ::MaskColor != NIL
      ImageListAddMasked( ::Handle, hBmp, ::MaskColor )
     ELSE
      ImageListAdd( ::Handle, hBmp, ::MaskColor )
   ENDIF
   DeleteObject( hBmp )
RETURN SELF

//----------------------------------------------------------------------------------------------------

METHOD AddIcon( hIcon, hInst ) CLASS ImageList
   IF VALTYPE( hIcon ) == "C"
      hInst := ::AppInstance
      hIcon := LoadIcon( hInst, hIcon )
      //hIcon := LoadImage( hInst, hIcon, IMAGE_ICON, 0, 0, LR_CREATEDIBSECTION | LR_LOADTRANSPARENT )
   ENDIF
   ImageListAddIcon(::Handle, hIcon)
RETURN SELF

//----------------------------------------------------------------------------------------------------

METHOD DrawDisabled( hDC, nIndex, x, y, hBrush ) CLASS ImageList
   LOCAL nStyle := DST_ICON, hIcon := ImageListGetIcon( ::Handle, nIndex-1, ILD_NORMAL )
   IF hBrush == NIL
      hBrush := 0
      nStyle := nStyle | DSS_DISABLED
    ELSE
      nStyle := nStyle | DSS_MONO
   ENDIF
   DrawState( hDC, hBrush, hIcon, 0, x, y, 0, 0, nStyle )
   DestroyIcon( hIcon )
RETURN SELF

//----------------------------------------------------------------------------------------------------
#define ILD_DPISCALE 0x4000 
#define ILS_ALPHA 0x00000008
#define ILS_SHADOW 0x00000002
#define ILS_SATURATE (0x00000004)

METHOD DrawIndirect( hDC, nIndex, x, y ) CLASS ImageList
   LOCAL ildp := (struct IMAGELISTDRAWPARAMS)
   ildp:cbSize := ildp:sizeof()
   ildp:i      := nIndex-1
   ildp:himl   := ::handle
   ildp:hdcDst := hDC
   ildp:x      := x
   ildp:y      := y
   ildp:fStyle := ILD_TRANSPARENT
   ImageListDrawIndirect( ildp )
RETURN SELF
   
//----------------------------------------------------------------------------------------------------

METHOD DrawImage( hDC, nIndex, x, y, nFlags, nColor ) CLASS ImageList
   DEFAULT nColor TO CLR_NONE
   ImageListDrawEx( ::Handle, nIndex-1, hDC, x, y, 0, 0, CLR_NONE, nColor, nFlags )
RETURN SELF

//----------------------------------------------------------------------------------------------------

METHOD GetImages() CLASS ImageList
   LOCAL n, hIcon, aImages := {}
   FOR n := 1 TO ImageListGetImageCount( ::Handle )
       hIcon := ::GetImage( n )
       AADD( aImages, hIcon )
   NEXT
RETURN SELF

CLASS __ImageListComboBox INHERIT ComboBox
   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList )

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnParentDrawItem()
ENDCLASS

METHOD Init( oParent, oImageList ) CLASS __ImageListComboBox
   ::ImageList := oImageList
   ::Super:Init( oParent )
   ::Style     := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | CBS_HASSTRINGS | CBS_OWNERDRAWFIXED | CBS_DROPDOWNLIST | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
RETURN Self

METHOD Create() CLASS __ImageListComboBox
   LOCAL n
   ::Super:Create()
   ::AddItem( "None" )
   FOR n := 1 TO ::ImageList:Count
       ::AddItem( XSTR(n) )
       ::SendMessage( CB_SETITEMHEIGHT, n-1, ::ImageList:IconHeight )
   NEXT
RETURN Self

METHOD OnParentDrawItem() CLASS __ImageListComboBox
   LOCAL n, y, lSelected, nLen, itemTxt, aSize
   IF ::Parent:DrawItemStruct:hwndItem == ::hWnd
      lSelected := ::Parent:DrawItemStruct:itemState & ODS_SELECTED != 0

      IF ::Parent:DrawItemStruct:itemAction & ODA_DRAWENTIRE == ODA_DRAWENTIRE .OR. ::Parent:DrawItemStruct:itemAction & ODA_SELECT == ODA_SELECT
         SetTextColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
         SetBkColor( ::Parent:DrawItemStruct:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )

         nLen    := SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXTLEN, ::Parent:DrawItemStruct:itemID, 0 )
         itemTxt := Space( nLen + 1 )
         SendMessage( ::Parent:DrawItemStruct:hwndItem, CB_GETLBTEXT, ::Parent:DrawItemStruct:itemID, @itemTxt )
         itemTxt := Left( itemTxt, nLen )
         n := ::Parent:DrawItemStruct:itemID +1

         aSize := _GetTextExtentPoint32( ::Parent:DrawItemStruct:hDC, itemTxt )
         y := ::Parent:DrawItemStruct:rcItem:Top + ((::Parent:DrawItemStruct:rcItem:Bottom-::Parent:DrawItemStruct:rcItem:Top)/2) - (aSize[2]/2)

         IF n > 0
            ExtTextOut( ::Parent:DrawItemStruct:hDC, ::ImageList:IconWidth+20, y, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem, itemTxt )
            IF ::Parent:DrawItemStruct:itemState & ODS_COMBOBOXEDIT == 0
               //::ImageList:DrawImage( ::Parent:DrawItemStruct:hDC, n-1, 3, ::Parent:DrawItemStruct:rcItem:Top, ILD_TRANSPARENT )
               DrawIconEx( ::Parent:DrawItemStruct:hDC, 3, ::Parent:DrawItemStruct:rcItem:Top, ::ImageList:GetImage(n), ::ImageList:IconWidth, ::ImageList:IconHeight, 0, NIL,  DI_NORMAL )
            ENDIF
          ELSE
            ExtTextOut( ::Parent:DrawItemStruct:hDC, ( ::Parent:DrawItemStruct:rcItem:right - aSize[1] )/2, y, ETO_OPAQUE + ETO_CLIPPED, ::Parent:DrawItemStruct:rcItem, itemTxt )
         ENDIF
      ENDIF
   ENDIF
RETURN 0
