
// Form class in edit mode

#include "windows.ch"
#include "wingdi.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#Include "cstruct.ch"
#include "debug.ch"

GLOBAL EXTERNAL hThisInstance

//----------------------------------------------------------------------------//

 CLASS TFormEdit FROM TForm

   DATA oMask
   DATA Editing
   DATA aCtrl
   DATA nCurrent
   DATA GridColor
   DATA xGrid
   DATA yGrid


   METHOD New( oParent )     INLINE ::Caption := 'SubForm1 from TForm', ;
                                    ::Editing:=.F.       ,;
                                    ::aCtrl:={}          ,;
                                    ::left:=100          ,;
                                    ::top:=100           ,;
                                    ::width:=400         ,;
                                    ::height:=300        ,;
                                    ::xGrid:=8           ,;
                                    ::yGrid:=8           ,;
                                    ::GridColor:=RGB(0,0,0),;
                                    super:New( oParent )

   METHOD Add()
   METHOD OnPaint( hDC )     INLINE DrawGrid(hDC, ::width, ::height, ::GridColor, ::xGrid, ::yGrid ), 0
   METHOD OnCreate()         INLINE ::CreateSub()
   METHOD OnSize()

   METHOD OnMenuCommand(oItem)
   METHOD CreateSub()
   METHOD GetUniquePropName()

 END CLASS

//----------------------------------------------------------------------------//

METHOD Add( cName, oObj )

   LOCAL oCtrl:=Super:Add( cName, oObj )

   aAdd( ::aCtrl, oCtrl)
   ::oMask:GetSiblings()
   ::oMask:SetFocus()

   RETURN(oCtrl)

//----------------------------------------------------------------------------//

METHOD OnSize(nFlag,Width,Height)

   LOCAL aRect

   IF ::oMask <> NIL
      aRect:=GetClientRect(::Handle)
      ::oMask:Move(aRect[1],aRect[2],aRect[3],aRect[4],.T.)
      ::oMask:Refresh()
   ENDIF

   RETURN(NIL)

//----------------------------------------------------------------------------//

METHOD OnMenuCommand(oItem)

   LOCAL nInd:=0
   LOCAL cName:=::GetUniquePropName(oItem:Text)

   DO CASE
   CASE oItem:Text=="Button"
      ::Add(cName, TButton():New(   self, cName, 500,  50, 10 ) )
   CASE oItem:Text=="Edit"
      ::Add(cName, TEdit():New(     self, cName, 501,  50, 40 ) )
   CASE oItem:Text=="Label"
      ::Add(cName, TStatic():New(   self, cName, 502,  50, 80 ) )
   CASE oItem:Text=="CheckBox"
      ::Add(cName, TCheck():New(    self, cName, 503,  50,120 ) )
   CASE oItem:Text=="RadioButton"
      ::Add(cName, TRadio():New(    self, cName, 504,  50,160 ) )
   CASE oItem:Text=="ComboBox"
      ::Add(cName, TComboBox():New( self,        505, 200, 10 ) )
   CASE oItem:Text=="ListBox"
      ::Add(cName, TListBox():New(  self,        506, 200, 40 ) )
   CASE oItem:Text=="StatusBar"
      IF !("2" $ cName) // only one status bar
        ::Add(cName, TStatusBar():New( self, cName, 507 ) )
      ENDIF
   ENDCASE

  RETURN(NIL)

//----------------------------------------------------------------------------//

METHOD GetUniquePropName(cName)

   LOCAL aProp:=__objGetValueList( self )
   LOCAL nInd:=1

   DO WHILE aScan(aProp, {|a| a[1]==UPPER(cName)+alltrim(str(nInd))}) > 0
      nInd++
   ENDDO

   RETURN(cName+alltrim(str(nInd)))


//----------------------------------------------------------------------------//

METHOD CreateSub()

   LOCAL oBtn
   LOCAL xRet


   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM EDITOR')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()


   xRet := super:OnCreate()

   ::oMask:=oCtrlMask():New( self )
   ::oMask:Create()
   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

   RETURN( xRet )



*------------------------------------------------------------------------------
* Low Level C Routines
*------------------------------------------------------------------------------
#pragma BEGINDUMP


#define _WIN32_WINNT   0x0400

#include <shlobj.h>
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"


// DrawGrid(hDC,nWidth,yWidth,nColor, xGran,yGran)

HB_FUNC( DRAWGRID )
{

   HDC hDC            = (HDC) hb_parnl( 1 );
   int width          = hb_parni( 2 );
   int height         = hb_parni( 3 );
   COLORREF nRGBColor = hb_parnl( 4 );
   int xGran          = hb_parni( 5 );
   int yGran          = hb_parni( 6 );
   int x, y ;

   for( y = 0 ; y < height ; y += yGran )
        for( x = 0 ; x < width ; x += xGran )
           SetPixel( hDC, x, y, nRGBColor ) ;

}


#pragma ENDDUMP





//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//




