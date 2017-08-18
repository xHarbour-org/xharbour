#include "vxh.ch"
#include "winuser.ch"

// Device Parameters for GetDeviceCaps()

STATIC cPrevFontName,nPrevFontSize,lPrevBold,lPrevItalic,lPrevUnderLine,lPrevStrikeOut,cForeColor,nPDFCharHeight,;
       nTopMargin,nLeftMargin,nPixelsPerInchX,nPixelsPerInchY,nLogiPixelsX,nLogiPixelsY


#include "PrintPreview_Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form1_OnLoad() CLASS PrintPreview_Form1

   LOCAL aPrnSetup,i,cAlias,nOldPDFRightMargin,lDefaultSize

   REQUEST DBFCDX, DBFFPT
   
   ::Params:={}
   
   FOR i=1 TO HB_ArgC()
      AAdd(::Params,HB_ArgV(i))
   NEXT

   IF Empty(::Params)

      MsgAlert("Please use as PrintPreview.exe <PreviewDataFile>")
      ::Application:Quit()
      RETURN Self

   ELSEIF File(::Params[1])

      cAlias:=GetNewAlias()
      dbUseArea(.T.,,::Params[1],cAlias,.F.)    
      aPrnSetup:={}
      (cAlias)->(DBEval({|| AAdd(aPrnSetup,{(cAlias)->PRNPROP,(cAlias)->PRNVAL})}))
      (cAlias)->(DbCloseArea())
      
   ELSEIF Len(::Params[2])>1 .AND. File(::Params[2])

      cAlias:=GetNewAlias()
      dbUseArea(.T.,,::Params[2],cAlias,.F.)    
      aPrnSetup:={}
      (cAlias)->(DBEval({|| AAdd(aPrnSetup,{(cAlias)->PRNPROP,(cAlias)->PRNVAL})}))
      (cAlias)->(DbCloseArea())

   ELSE
      MsgAlert("Invalid parameter")
      ::Application:Quit()
      RETURN Self

   ENDIF
   
   ::aPrnPage:={}


   i:=AScan(aPrnSetup,{|x| x[1]=="PRINTER"})
   IF i>0
      ::cPrinter:=aPrnSetup[i][2]
   ELSE
      ::cPrinter:=GetDefaultPrinter()
   ENDIF


   i:=AScan(aPrnSetup,{|x| x[1]=="COPY"})
   IF i>0
      ::nCopies:=aPrnSetup[i][2]
   ELSE
      ::nCopies:=1
   ENDIF

   i:=AScan(aPrnSetup,{|x| x[1]=="LANDSCAPE"})
   IF i>0
      ::lLandScape:=aPrnSetup[i][2]
   ELSE
      ::lLandScape:=.F.
   ENDIF

   i:=AScan(aPrnSetup,{|x| x[1]=="FORMTYPE"})
   IF i>0
      ::nFormType:=aPrnSetup[i][2]
   ELSE
      ::nFormType:=9
   ENDIF

   i:=AScan(aPrnSetup,{|x| x[1]=="PAPERWIDTH"})
   IF i>0
      ::nPaperWidth:=aPrnSetup[i][2]
   ELSE
      ::nPaperWidth:=210*10
   ENDIF

   i:=AScan(aPrnSetup,{|x| x[1]=="PAPERLENGTH"})
   IF i>0
      ::nPaperLength:=aPrnSetup[i][2]
   ELSE
      ::nPaperLength:=297*10
   ENDIF
   
   i:=AScan(aPrnSetup,{|x| x[1]="RIGHTMARGIN"})
   IF i>0
      nOldPDFRightMargin:=aPrnSetup[i][2]
   ELSE
      nOldPDFRightMargin:=10
   ENDIF

   i:=AScan(aPrnSetup,{|x| x[1]="EMFNAMES"})
   IF i>0
      ::aEmfName:=aPrnSetup[i][2]
   ELSE
      ::aEmfName:={}
   ENDIF

   IF Len(::aEmfName)>=1
      ::nPage:=1
   ELSE
   ::Close()
      RETURN Self
   ENDIF
   

   ::lAskPrinter:=.T.
   ::oPrinter := Win32Prn():New(::cPrinter)   
   ::oPrinter:Landscape:=::lLandscape

   IF ::nFormType=0
      ::oPrinter:FormType:=::nFormType
      ::oPrinter:PaperWidth:=::nPaperWidth
      ::oPrinter:PaperLength:=::nPaperLength
   ELSE
      ::oPrinter:FormType:=::nFormType
   ENDIF

   ::oPrinter:setPrintQuality(-4) // Highest printer quality

   IF !::oPrinter:Create()
      MsgAlert("Cannot create printer object")
      ::Close()
      RETURN Self
   ENDIF

   IF !::oPrinter:StartDoc("WinPrnPreview")
      MsgAlert("Cannot create document")
      ::oPrinter:Destroy()
      ::Close()
      RETURN Self
   ENDIF

   ::oPrinter:SetBkMode(1)
   ::oPrinter:SetPrc(0,0)

   ::prnDC:=::oPrinter:hPrinterDC

   lDefaultSize:=.T.
   
   IF File(GetExePath()+"\vxh_Preview.INI")
      TRY
       ::RestoreLayout( GetExePath()+"\vxh_Preview.INI", "Form" )
       IF ::Height < 600
          ::Top-=600-::Height
          IF ::Top<50
             ::Top:=50
          ENDIF
          ::Height := 600
       ENDIF
       IF ::Width < 450
          ::Left-=450-::Width
          IF ::Left<50
             ::Left:=50
          ENDIF
          ::Width := 450
       ENDIF
       IF ::ShowMode = 2
          ::ShowMode := 1
       ENDIF
       lDefaultSize:=.F.
      CATCH
      END
   ENDIF
   
   IF lDefaultSize
      ::Height:=600
      ::Width:=450
      ::CenterWindow(.T.)
   ENDIF

   IF Len(::aEmfName)<=1
   
      ::GotoPageToolStripButton:Destroy()
      ::FirstPageToolStripButton:Destroy()
      ::PrevPageToolStripButton:Destroy()
      ::NextPageToolStripButton:Destroy()
      ::LastPageToolStripButton:Destroy()

   ENDIF

   ::Panel1:Top:=::ToolStrip1:Height+20
   ::Panel1:Left:=20

   ::nWidth :=GetDeviceCaps(::prnDC,HORZRES)
   ::nHeight:=GetDeviceCaps(::prnDC,VERTRES)
   
   nLogiPixelsX:=GetDeviceCaps(::prnDC, LOGPIXELSX)   
   nLogiPixelsY:=GetDeviceCaps(::prnDC, LOGPIXELSY)   
   
   nLeftMargin   :=::oPrinter:LeftMargin
   nTopMargin    :=::oPrinter:TopMargin
   nPixelsPerInchX:=::oPrinter:PixelsPerInchX
   nPixelsPerInchY:=::oPrinter:PixelsPerInchY
   
   ::NeedsRedraw:=.T.
   ::nZoom:=0
   ::lMaxMinRestore:=.F.
   
   TRY
    ::oPrinter:Destroy()
   CATCH
   END   

   IF ::ShowMode=3
      ::lMaxMinRestore:=.T.
   ENDIF

   ResizePreviewDlg(Self)
   
   ::Panel1:SetFocus()
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form1_OnSysCommand() CLASS PrintPreview_Form1

   ::lMaxMinRestore:=.T.
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form1_OnExitSizeMove() CLASS PrintPreview_Form1

   IF ::nWidth=NIL
      RETURN Self
   ENDIF
   
   ::NeedsRedraw:=.T.
   ResizePreviewDlg( Self)
   ::Panel1:Refresh()
   ::Refresh()

RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form1_OnSize() CLASS PrintPreview_Form1

   IF ::nWidth=NIL
      RETURN Self
   ENDIF

   
   IF ::lMaxMinRestore
      
      ::NeedsRedraw:=.T.
      ResizePreviewDlg( Self)
      ::Panel1:Refresh()
      ::Refresh()
      ::lMaxMinRestore:=.F.
   ENDIF
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD PrintPreview_Form1_OnClose() CLASS PrintPreview_Form1

   TRY
    IF ::ShowMode != 2
       ::SaveLayout( GetExePath()+"\vxh_Preview.INI", "Form" )
    ENDIF
   CATCH
   END

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD ZoomInToolStripButton_OnClick() CLASS PrintPreview_Form1

   ::NeedsRedraw:=.T.
   ResizePreviewDlg( Self, 1 )
   ::Panel1:Refresh()
   ::Refresh()
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD ZoomOutToolStripButton_OnClick() CLASS PrintPreview_Form1

   ::NeedsRedraw:=.T.
   ResizePreviewDlg( Self, -1 )
   ::Panel1:Refresh()
   ::Refresh()
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD PrintToolStripButton_OnClick() CLASS PrintPreview_Form1

   LOCAL oPrinter, i, cText, cPrinter, nCopies, aPrnPage, lAllPage, lCurPage, aPageNo, aPageRange, nCopyCtr
   
   IF ::lAskPrinter
      cPrinter:=::cPrinter
      nCopies:=::nCopies
      aPrnPage:=IF(::aPrnPage==NIL,{},::aPrnPage)
      
      IF GetPrinterDialog(Self,@cPrinter,@nCopies,@aPrnPage)
         ::cPrinter:=cPrinter
         ::nCopies:=nCopies
         ::aPrnPage:=aPrnPage
      ELSE
         RETURN Self
      ENDIF
   ENDIF
   
   ::aPrnPage:=IF(::aPrnPage==NIL .OR. Empty(::aPrnPage), {.T.,.F.,""}, ::aPrnPage)
   lAllPage:=::aPrnPage[1]
   lCurPage:=::aPrnPage[2]
   aPageNo:={}
   aPageRange:={}
   
   IF !lAllPage .AND. !lCurPage
   
      aPageNo:=HB_Atokens(::aPrnPage[3],",")
      FOR i=1 TO Len(aPageNo)
         IF "-" $ aPageNo[i]
            AAdd(aPageRange, HB_ATokens(aPageNo[i],"-"))
            ADel(aPageNo,i,.T.)
            i--
         ELSE
            aPageNo[i]:=Val(aPageNo[i])
         ENDIF
      NEXT
      
      FOR i=1 TO Len(aPageRange)
         aPageRange[i]:={Val(aPageRange[i][1]),Val(aPageRange[i][2])}
      NEXT
      
   ENDIF
   
   oPrinter := Win32Prn():New(::cPrinter)
   IF oPrinter==NIL
      MsgAlert("Cannot initiated printer object")
      RETURN Self
   ENDIF
   
   WITH OBJECT oPrinter
      :Landscape:=::lLandscape
   
      IF ::nFormType=0
         :FormType:=::nFormType
         :PaperWidth:=::nPaperWidth
         :PaperLength:=::nPaperLength
      ELSE
         :FormType:=::nFormType
      ENDIF

      //:Copies:=::nCopies  // It didn't work!!!
      
      :setPrintQuality(-4) // Highest printer quality

      IF !:Create()
         TRY
            :Destroy()
         CATCH
         END
         MsgAlert("Cannot create printer object")
         RETURN Self
      ENDIF

      IF !:StartDoc("WinPrintPreview")
         :Destroy()
         MsgAlert("Cannot start document")
         RETURN Self
      ENDIF

      //:SetBkMode(1)
      //:SetPrc(0,0)

      :TextOutAt(0,0,"",.F.,.F.) // Dummy TextOut, otherwise PlayEnhMetaFile doesn't work!
   
      FOR nCopyCtr=1 TO ::nCopies
         FOR i=1 TO Len(::aEmfName)
            IF !lAllPage .AND. !(lCurPage .AND. i==::nPage)
               IF Ascan(aPageNo,{|x| x==i})==0 .AND. AScan(aPageRange,{|x| i>=x[1] .AND. i<=x[2]})==0
                  LOOP
               ENDIF
            ENDIF
            
            :StartPage()
            
            VXH_PlayEnhMetaFile(::aEmfName[i],:hPrinterDC,0,0,GetDeviceCaps(:hPrinterDC,HORZRES),GetDeviceCaps(:hPrinterDC,VERTRES))
                     
            :EndPage(.F.)
         NEXT
      NEXT
      
      :EndDoc()
      
      TRY
         :Destroy()
      CATCH
      END
   END
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD MenuPrinterSetting_OnClick() CLASS PrintPreview_Form1

    LOCAL cPrinter,nCopies,lPrintNow,aPrnPage

    cPrinter:=::cPrinter
    nCopies:=::nCopies
    aPrnPage:=IF(::aPrnPage==NIL,{},::aPrnPage)

    IF GetPrinterDialog(Self,@cPrinter,@nCopies,@aPrnPage,@lPrintNow)
       ::cPrinter:=cPrinter
       ::nCopies:=nCopies
       ::lAskPrinter:=.F.
       ::aPrnPage:=aPrnPage
       IF lPrintNow
          ::PrintToolStripButton_OnClick()
       ENDIF       
    ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD GotoPageToolStripButton_OnClick() CLASS PrintPreview_Form1

   LOCAL nPageNo, lOk

   nPageNo := ::nPage
   lOk := PrintPreview_GetPageNo(Self, @nPageNo, Len(::aEmfName))

   IF !lOk
      RETURN NIL
   ENDIF
   
   IF nPageNo > 0 .AND. nPageNo!=::nPage  .AND. nPageNo <= Len(::aEmfName)
      ::nPage:=nPageNo
      ::NeedsRedraw := .T.
      ::Panel1:Refresh()
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD FirstPageToolStripButton_OnClick() CLASS PrintPreview_Form1

   IF ::nPage>1
      ::nPage:=1
      ::NeedsRedraw := .T.
      ::Panel1:Refresh()
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD PrevPageToolStripButton_OnClick() CLASS PrintPreview_Form1

   IF ::nPage>1
      ::nPage--
      ::NeedsRedraw := .T.
      ::Panel1:Refresh()
   ENDIF

RETURN Self                  


//----------------------------------------------------------------------------------------------------//


METHOD NextPageToolStripButton_OnClick() CLASS PrintPreview_Form1

   IF ::nPage<Len(::aEmfName)
      ::nPage++
      ::NeedsRedraw := .T.
      ::Panel1:Refresh()
   ENDIF
   
RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD LastPageToolStripButton_OnClick() CLASS PrintPreview_Form1

   IF ::nPage!=Len(::aEmfName)
      ::nPage:=Len(::aEmfName)
      ::NeedsRedraw := .T.
      ::Panel1:Refresh()
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


STATIC FUNCTION rgb(r, g, b)

RETURN ( r + ( g * 256 ) + ( b * 256 * 256 ) )


//----------------------------------------------------------------------------------------------------//


#pragma BEGINDUMP

#include "cinterface.h"
#include <windows.h>
#include <unknwn.h>
#include <commdlg.h>
#include <shellapi.h>
#include <winreg.h>
#include <Rpc.h>
#include <ole2.h>
#include <wincrypt.h>
#include <winsock2.h>
#include <winspool.h>

#include "hbapiitm.h"


HB_FUNC_STATIC( GETDEVICECAPS )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC && ISNUM(2))
  {
    Result = (LONG) GetDeviceCaps( hDC, hb_parnl(2)) ;
  }
  hb_retnl( Result) ;
}


/* Meta file, hDC, y1, x1, y2, x2 */
HB_FUNC( VXH_PLAYENHMETAFILE )
{
   RECT rect;
   HENHMETAFILE hEmf = GetEnhMetaFile( hb_parc( 1 ) );

   SetRect( &rect,  hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl(5 ), hb_parnl( 6 ) );
   hb_retnl( ( LONG ) PlayEnhMetaFile( ( HDC ) hb_parnl( 2 ), hEmf, &rect ) );
   DeleteEnhMetaFile( hEmf );
}




//HB_FUNC( PRN_PLAYENHMETAFILE )
//{
//   HDC hDC = ( HDC ) hb_parnl( 1 );
//   RECT rc;
//
//   SetRect( &rc, 0, 0, GetDeviceCaps( hDC, HORZRES ), GetDeviceCaps( hDC,
//               VERTRES ) );
//
//   StartPage( hDC );
//   hb_retnl( ( LONG ) PlayEnhMetaFile( hDC,
//               ( HENHMETAFILE ) GetEnhMetaFile( hb_parc( 2 ) ), &rc ) );
//   EndPage( hDC );
//}


//HB_FUNC_STATIC( CREATEDC )
//{
//  LONG Result = 0 ;
//  if (ISCHAR(1))
//  {
//    Result = (LONG)  CreateDC("",hb_parc(1),NULL, NULL) ;
//  }
//  hb_retnl(Result) ;
//}


//HB_FUNC( STARTDOC )
//{
//   void *hText;
//   DOCINFO di;
//
//   di.cbSize = sizeof( DOCINFO );
//   di.lpszDocName = hb_parc( 2 );
//   di.lpszOutput = NULL;
//   di.lpszDatatype = NULL;
//   di.fwType = 0;
//
//   hb_retnl( ( LONG ) StartDoc( ( HDC ) hb_parnl( 1 ), &di ) );
//   hb_strfree( hText );
//}

//HB_FUNC( ENDDOC )
//{
//   hb_retnl( ( LONG ) EndDoc( ( HDC ) hb_parnl( 1 ) ) );
//}


//HB_FUNC( STARTPAGE )
//{
//   hb_retnl( ( LONG ) StartPage( ( HDC ) hb_parnl( 1 ) ) );
//}

//HB_FUNC( ENDPAGE )
//{
//   hb_retnl( ( LONG ) EndPage( ( HDC ) hb_parnl( 1 ) ) );
//}


//HB_FUNC( VXH_DRAWTEXT )
//{
//   void *hText;
//   HB_SIZE nLen = hb_parni( 3 );
//   LPCTSTR lpText = hb_parc( 2 );
//   RECT rc;
//   UINT uFormat = hb_parni( 8 );
//   //UINT uFormat = ( hb_pcount(  ) == 5 ? hb_parni( 5 ) : hb_parni( 8 ) );
//   // int uiPos = ( hb_pcount(  ) == 5 ? 3 : hb_parni( 9 ) );
//   int heigh;
//
//   rc.left = hb_parni( 4 );
//   rc.top = hb_parni( 4 );
//   rc.right = hb_parni( 6 );
//   rc.bottom = hb_parni( 7 );
//
//   heigh = DrawText( ( HDC ) hb_parnl( 1 ), // handle of device context
//         lpText,                // address of string
//         nLen,                  // number of characters in string
//         &rc, uFormat );
//   hb_strfree( hText );
//
//   hb_retni( heigh );
//}


//HB_FUNC_STATIC(VXH_TEXTOUT)
//{
//  
//   TextOut((HDC) hb_parnl(1), hb_parni(4), hb_parni(5), hb_parc( 2 ), hb_parni(3)) ;
//   hb_retnl(1) ;
//   
//}

#pragma ENDDUMP


//----------------------------------------------------------------------------------------------------//


FUNCTION ResizePreviewDlg( Self, nZoom)

   LOCAL nWidth, nHeight, x, y 

   x := ::Width-40
   
   IF ::VertScrollTopMargin=0 
      y := ::Height-::ToolStrip1:Height-40
   ELSE
      y := ::Height-::VertScrollTopMargin-40
   ENDIF

   IF nZoom != NIL
      ::nZoom += nZoom
      IF ::nZoom<-4
         ::nZoom:=-4
      ENDIF
      ::NeedsRedraw := .T.
   ENDIF
   
   IF ::nHeight>::nWidth
      nHeight:=y-40
      nWidth:=Round(nHeight * ::nWidth / ::nHeight, 0)
   ELSE
      nWidth:=x-40
      nHeight:=Round(nWidth * ::nHeight / ::nWidth, 0)
   ENDIF

   nWidth:=Round(nWidth*(1+::nZoom*20/100),0)
   nHeight:=Round(nWidth*::nHeight/::nWidth,0)
   
//   ::Panel1:Width:=nWidth
//   ::Panel1:Height:=nHeight

   IF nWidth+40<::Width
      ::Panel1:Left:=Round((::Width-nWidth)/2,0)
   ELSE
      ::Panel1:Left:=20
   ENDIF

   IF nHeight+40<y
      ::Panel1:Top:=Round((y-nHeight)/2,0)+IF(::VertScrollTopMargin=0,::ToolStrip1:Height,0)-20
   ELSE
      ::Panel1:Top:=IF(::VertScrollTopMargin=0,::ToolStrip1:Height,0)+20
   ENDIF

   IF ::VertScrollTopMargin>0
      ::Panel1:Left:=Max(::Panel1:Left,20)
      ::Panel1:Top:=Max(::Panel1:Top,20)
   ENDIF 

   ::Panel2:Left:=::Panel1:Left+3
   ::Panel2:Top:=::Panel1:Top+3

   ::Panel1:Width:=nWidth
   ::Panel1:Height:=nHeight

   ::Panel2:Width:=nWidth
   ::Panel2:Height:=nHeight

   ::Panel2:Refresh()      
   ::Panel1:Refresh()

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Panel1_OnPaint( Sender ) CLASS PrintPreview_Form1

   LOCAL aRect,hDC,hMemDC,hMemBitmap,hOldBitmap
   
   aRect:={0,0,Sender:xWidth,Sender:xHeight}
   hDC        := Sender:BeginPaint()
   
   hMemDC     := CreateCompatibleDC( hDC )
   hMemBitmap := CreateCompatibleBitmap( hDC, Sender:Width, Sender:Height )
   SelectObject( hMemDC, hMemBitmap)
   hOldBitmap := SelectObject( hMemDC, hMemBitmap)
                                               
    SetBkMode(hMemDC,OPAQUE)
//   _Fillrect( hMemDC, aRect, CreateSolidBrush( ::Backcolor ) )                                  // Backcolor
//   _Fillrect( hMemDC, {3, 3, aRect[3], aRect[4]}, CreateSolidBrush( Rgb( 117, 117, 117 ) ) )    // Shadow

   _Fillrect( hMemDC, aRect, CreateSolidBrush( Rgb( 255, 255, 255 ) ) )   // White

   SetBkMode(hMemDC,TRANSPARENT)
   // Draw the actual printer data

   IF !(::aEmfName=NIL .OR. Len(::aEmfName)=0)
      VXH_PlayEnhMetaFile(::aEmfName[::nPage],hMemDC,0,0,Sender:Width,Sender:Height)
   ENDIF

   ::NeedsRedraw:=.F.  


   BitBlt( hDC, 0, 0, Sender:Width, Sender:Height, hMemDC, 0, 0, SRCCOPY )

   SelectObject( hMemDC,  hOldBitmap )
   DeleteObject( hMemBitmap )
   DeleteDC( hMemDC )
   Sender:EndPaint()

RETURN Self


//----------------------------------------------------------------------------------------------------//


STATIC FUNCTION GetNewAlias()

   LOCAL n:=1
   LOCAL cPrefix:="A"
   n++
   WHILE n<100000
      IF Select(cPrefix+LTrim(Str(n,9,0)))=0
         EXIT
      ENDIF
      n++
   ENDDO

RETURN cPrefix+LTrim(Str(n,9,0))  
   
   
//----------------------------------------------------------------------------------------------------//


STATIC FUNCTION GetExePath()

   LOCAL cPathMask:=hb_argv(0) 
   LOCAL n:= RAt( "\", cPathMask )

   IF n>0   
      cPathMask:=Left( cPathMask, n-1 )
   ELSE
      cPathMask:=""
   ENDIF
   
RETURN cPathMask


//----------------------------------------------------------------------------------------------------//