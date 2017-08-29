#include "vxh.ch"

#ifndef CRLF
 #define CRLF CHR(13) + CHR(10)
#endif

#define MM_TO_INCH 25.4

#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  If( <uVar1> == nil, <uVar1> := <uVal1>, ) ;;
                [ If( <uVarN> == nil, <uVarN> := <uValN>, ); ]

STATIC aPaper:={ {"9","A4 210 x 297 mm"},;
                 {"8","A3 297 x 420 mm"},;
                 {"1","Letter 8 1/2 x 11 in"},;
                 {"5","Legal 8 1/2 x 14 in"},;
                 {"11","A5 148 x 210 mm"},;
                 {"T80","Ticket 80mm"},;
                 {"T72","Ticket 72mm"} }

#define DMPAPER_CUSTOM 0 //Custom form type

//FREE_IMAGE_FORMAT
#define FIF_UNKNOWN -1
#define FIF_BMP      0
#define FIF_ICO      1
#define FIF_JPEG     2
#define FIF_JNG      3
#define FIF_KOALA    4
#define FIF_LBM      5
#define FIF_IFF      FIF_LBM
#define FIF_MNG      6
#define FIF_PBM      7
#define FIF_PBMRAW   8
#define FIF_PCD      9
#define FIF_PCX     10
#define FIF_PGM     11
#define FIF_PGMRAW  12
#define FIF_PNG     13
#define FIF_PPM     14
#define FIF_PPMRAW  15
#define FIF_RAS     16
#define FIF_TARGA   17
#define FIF_TIFF    18
#define FIF_WBMP    19
#define FIF_PSD     20
#define FIF_CUT     21
#define FIF_XBM     22
#define FIF_XPM     23
#define FIF_DDS     24
#define FIF_GIF     25
#define FIF_HDR     26

// Load / Save flag constants -----------------------------------------------
#define BMP_DEFAULT         0
#define GIF_DEFAULT         0
#define ICO_DEFAULT         0
#define JPEG_DEFAULT        0
#define PNG_DEFAULT         0

//FREE_IMAGE_FILTER
#define FILTER_BOX         0  // Box, pulse, Fourier window, 1st order (constant) b-spline
#define FILTER_BICUBIC     1  // Mitchell & Netravali's two-param cubic filter
#define FILTER_BILINEAR    2  // Bilinear filter
#define FILTER_BSPLINE     3  // 4th order (cubic) b-spline
#define FILTER_CATMULLROM  4  // Catmull-Rom spline, Overhauser spline
#define FILTER_LANCZOS3    5  // Lanczos3 filter


// Device Parameters for GetDeviceCaps()

#define WFHORZSIZE      4     // Horizontal size in millimeters
#define WFVERTSIZE      6     // Vertical size in millimeters
#define WFHORZRES       8     // Horizontal width in pixels
#define WFVERTRES       10    // Vertical height in pixels
#define WFNUMBRUSHES    16    // Number of brushes the device has
#define WFNUMPENS       18    // Number of pens the device has
#define WFNUMFONTS      22    // Number of fonts the device has
#define WFNUMCOLORS     24    // Number of colors the device supports
#define WFRASTERCAPS    38    // Bitblt capabilities

#define WFLOGPIXELSX    88    // Logical pixels/inch in X
#define WFLOGPIXELSY    90    // Logical pixels/inch in Y

#define WFPHYSICALWIDTH   110 // Physical Width in device units
#define WFPHYSICALHEIGHT  111 // Physical Height in device units
#define WFPHYSICALOFFSETX 112 // Physical Printable Area x margin
#define WFPHYSICALOFFSETY 113 // Physical Printable Area y margin
#define WFSCALINGFACTORX  114 // Scaling factor x
#define WFSCALINGFACTORY  115 // Scaling factor y

#define WFTA_LEFT                      0
#define WFTA_RIGHT                     2
#define WFTA_CENTER                    6

#define WFTA_TOP                       0
#define WFTA_BOTTOM                    8
#define WFTA_BASELINE                  24


//----------------------------------------------------------------------------------------------------//

CLASS WinPrint

   DATA oPrinter,nPage,lPaperLength,lPreview,lBestQuality,lLatePrn,cAlias,cFile
   DATA cPrinter,nCharHeight,nCopies,cFormType,lAskProperties
   DATA nPageWidth,nPageHeight,lLandScape
   DATA nTopMargin,nLeftMargin,nRightMargin,nBottomMargin,nLineHeight,nFontSizeDiff,nDtlLineRowGap
   DATA cHexShadowColor,nMaxY,aPrn,nPenWidth,lError,lDuplex,nPrnPosX,nPrnPosY,lDtlLine,lNextToHeader,lNextPageSameNum
   DATA cPrevFontName,nPrevFontSize,lPrevBold,lPrevItalic,lPrevUnderLine,lPrevStrikeOut,nForeColor,lGrayScale
   DATA cEmfName,aEmfName,cEmfPrefix,hDC,hDCBak,nPrevColor,aPrevFont,aPrnData,lPrintRightOfLeftMargin

   METHOD New() CONSTRUCTOR

   METHOD Create()
   METHOD WinPrnSetFont()
   METHOD SetFont()
   METHOD SetBold()     // Compatibility method
   METHOD SetItalic()   // Compatibility method
   METHOD SetFontArray()
   METHOD Say()
   METHOD GetJustifiedString()
   METHOD Line()
   METHOD Box()
   METHOD Shadow()
   METHOD Image()
   METHOD PageBreak()
   METHOD GetTextWidthMM()
   METHOD GetTextArray()
   METHOD GetTextHeightMM()
   METHOD mm2actx()
   METHOD mm2acty()
   METHOD mm2x()
   METHOD mm2y()
   METHOD x2mm()
   METHOD y2mm()
   METHOD SetPenWidth()
   METHOD Preview()

   METHOD SetPos()
   METHOD SetPosX()
   METHOD SetPosY()
   METHOD GetPosX()
   METHOD GetPosY()
   METHOD GetPageHeight()
   METHOD GetPageWidth()
   METHOD TextOut()     // Compatibility method with Say()
   METHOD TextOutAt()   // Compatibility method with Say()
   METHOD GetDtlLine()  // Compatibility method
   METHOD SetDtlLine()  // Compatibility method
   METHOD GetNextToHeader()  // Compatibility method
   METHOD SetNextToHeader()  // Compatibility method
   METHOD GetCharWidth() INLINE (::X2Mm(::oPrinter:GetCharWidth()))   // Compatibility method
   METHOD GetCharHeight() // Compatibility method
   METHOD DecreaseFontSize()  // Compatibility method
   METHOD IncreaseFontSize()  // Compatibility method

   METHOD EmfPrnStart()
   METHOD EmfPrnEnd()
   METHOD EmfPrnPageStart()
   METHOD EmfPrnPageEnd()
   METHOD EmfPrint()
   
   METHOD Close()

ENDCLASS


//----------------------------------------------------------------------------------------------------//


METHOD New(lPreview) CLASS WinPrint

   DEFAULT lPreview := .F.

   REQUEST DBFCDX, DBFFPT

   ::lError:=.F.
   ::cPrinter:=GetDefaultPrinter()
   ::lLandScape:=.F.
   ::nCopies:=1
   ::cFormType:= "9"
   ::lAskProperties:=.F.
   ::lPaperLength:=.T.
   ::aPrn:={}
   ::cHexShadowColor:="DCDCDC"
   ::nMaxY:=0
   ::nPenWidth:=0.10
   ::lPreview:=lPreview
   ::nRightMargin:=10
   ::lGrayScale:=.F.
   ::lBestQuality:=.F.
   ::lDuplex:=.F.
   ::nPrnPosX:=0
   ::nPrnPosY:=0
   ::nLineHeight:=0
   ::nFontsizeDiff:=0
   ::lDtlLine:=.F.
   ::lNextToHeader:=.F.
   ::nDtlLineRowGap:=0.5
   ::nCharHeight:=5
   ::nTopMargin:=10
   ::nRightMargin:=10
   ::nBottomMargin:=10
   ::nLeftMargin:=10
   ::cEmfName:=""
   ::aEmfName:={}
   ::lLatePrn:=.F.
   ::lNextPageSameNum:=.F.
   ::lPrintRightOfLeftMargin:=.F.

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Create() CLASS WinPrint

   IF AScan(GetPrinters(),{|x| Upper(x)==Upper(::cPrinter)})==0
      ::cPrinter:=GetDefaultPrinter()
   ENDIF

   ::oPrinter := Win32Prn():New(IF(::lAskProperties,"",::cPrinter))

   IF ::oPrinter==NIL
      MsgAlert("Cannot open "+::cPrinter)
      ::Destroy()
      RETURN .F.
   ENDIF

   ::oPrinter:Landscape:=::lLandscape
   ::oPrinter:AskProperties:=::lAskProperties

   IF ::cFormType="T80"
      ::oPrinter:FormType:=0
      ::nPageWidth:=80
      ::oPrinter:PaperWidth:=::nPageWidth*10
      ::oPrinter:PaperLength:=2970
      ::lPaperLength:=.F.
   ELSEIF ::cFormType="T72"
      ::oPrinter:FormType:=0
      ::nPageWidth:=72
      ::oPrinter:PaperWidth:=::nPageWidth*10
      ::oPrinter:PaperLength:=2970
      ::lPaperLength:=.F.
   ELSEIF Val(::cFormType)=DMPAPER_CUSTOM
      ::oPrinter:FormType:=0
      ::oPrinter:PaperWidth:=::nPageWidth*10
      IF ::nPageHeight=0
         ::oPrinter:PaperLength:=2970
         ::lPaperLength:=.F.
      ELSE
         ::oPrinter:PaperLength:=::nPageHeight*10
      ENDIF
   ELSE
      ::oPrinter:FormType:=Val(::cFormType)
   ENDIF

   IF ::lBestQuality
      ::oPrinter:setPrintQuality(-4) // Highest printer quality
   ENDIF

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview .AND. ::lDuplex
      IF ::oPrinter:setDuplexType()=1
         ::oPrinter:setDuplexType(3) // Made it horizontally
      ENDIF
   ELSE
      ::oPrinter:SetDuplexType(1) // Made it simplex
   ENDIF

   IF !::oPrinter:Create()
      MsgAlert("Cannot create printer object")
      RETURN .F.
   ENDIF


   IF !::oPrinter:StartDoc()
      MsgAlert("Cannot create document")
      ::oPrinter:Destroy()
      RETURN .F.
   ENDIF

   ::nPageWidth:=GetDeviceCaps(::oPrinter:hPrinterDC,WFHORZRES)/::oPrinter:PixelsPerInchX*MM_TO_INCH
   ::nPageHeight:=GetDeviceCaps(::oPrinter:hPrinterDC,WFVERTRES)/::oPrinter:PixelsPerInchY*MM_TO_INCH


   ::nRightMargin-=  (GetDeviceCaps(::oPrinter:hPrinterDC , WFPHYSICALWIDTH) - ;
                     GetDeviceCaps(::oPrinter:hPrinterDC , WFHORZRES)) / ;
                     ::oPrinter:PixelsPerInchX*MM_TO_INCH


   ::cPrevFontName:=""
   ::nPrevFontSize:=0
   ::lPrevBold:=.F.
   ::lPrevItalic:=.F.
   ::lPrevUnderLine:=.F.
   ::lPrevStrikeOut:=.F.
   ::nForeColor:=""

   ::oPrinter:SetBkMode(1)
   ::oPrinter:SetPrc(0,0)

   ::nPage:=1

   IF !::lPaperLength .OR. ::lLatePrn .OR. ::nCopies>1 .OR. ::lPreview
      ::aPrn:={}

      IF !::lPaperLength .OR. ::lLatePrn
         ::cAlias:=GetNewAlias()
         ::cFile:=TempFileName()
         TRY
         REQUEST RMDBFCDX
         RddSetDefault( "RMDBFCDX" )
         dbCreate(::cFile,{{"PAGENO","N",5,0},{"PAGEDATA","M",10,0}})
         dbUseArea(.T.,,::cFile,::cAlias,.F.)
         CATCH
         ::lError:=.T.
         END
         IF ::lError
            MsgAlert("Cannot create file "+::cFile+" (Code:686)")
            ::Close()
            RETURN .F.
         ENDIF
      ELSE
         ::EmfPrnStart()
      ENDIF
   ENDIF

RETURN .T.


//----------------------------------------------------------------------------------------------------//


METHOD SetPenWidth(n) CLASS WinPrint

   IF ::lPaperLength .AND. !::lLatePrn
      ::oPrinter:SetPen(::oPrinter:PenStyle,Int(n*::oPrinter:PixelsPerInchY/MM_TO_INCH),::oPrinter:PenColor)
   ELSE
      IF ::lPreview
         AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,n,::oPrinter:PenColor})
         AAdd(::aPrn,{"SETLINEWIDTH",n})
      ELSE
         AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,Int(n*::oPrinter:PixelsPerInchY/MM_TO_INCH),::oPrinter:PenColor})
      ENDIF
   ENDIF

   ::nPenWidth:=n

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD SetFontArray(aFont) CLASS WinPrint

   ::SetFont(aFont[1],aFont[2],aFont[3],aFont[4],aFont[5],aFont[6],aFont[7])

RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD SetFont(cFontName,nFontSize,lBold,lItalic,lUnderLine,lStrikeOut,nForeColor,lForce) CLASS WinPrint

   (lStrikeOut)

   DEFAULT lForce:=.F.

   IF ValType(nForeColor)="C"
      nForeColor:=Val(nForeColor)
   ENDIF

   IF !lForce .AND. ::cPrevFontName==cFontName .AND. ::nPrevFontSize==nFontSize .AND. ::lPrevBold==lBold .AND. ::lPrevItalic==lItalic .AND. ;
      ::lPrevUnderLine==lUnderLine .AND. ::lPrevStrikeOut==lStrikeOut .AND. ::nForeColor==nForeColor
      RETURN SELF
   ENDIF

   ::cPrevFontName:=cFontName

   ::nFontSizeDiff:=(nFontSize - ::nPrevFontSize) / 72 * MM_TO_INCH

   ::nPrevFontSize:=nFontSize
   ::lPrevBold:=lBold
   ::lPrevItalic:=lItalic
   ::lPrevUnderLine:=lUnderLine

   ::lPrevStrikeOut:=lStrikeOut
   ::nForeColor:=nForeColor

   ::oPrinter:SetColor(IF(::lGrayScale,0,nForeColor))      
   ::WinPrnSetFont(cFontName, ;
      nFontSize, ;
      0, ;
      IF(lBold, 700, 100), ;
      lUnderLine, ;
      lItalic, ;
      lStrikeOut, ;
      IF(Upper(cFontName)$"WINGDINGSWEBDINGS",2,0))
      
   IF !::lPaperLength .OR. ::lLatePrn
      AAdd(::aPrn,{"SETCOLOR",IF(::lGrayScale,0,nForeColor)})
      AAdd(::aPrn,{"SETFONT", cFontName, ;
         nFontSize, ;
         0, ;
         IF(lBold, 700, 100), ;
         lUnderLine, ;
         lItalic, ;
         lStrikeOut, ;
         IF(Upper(cFontName)$"WINGDINGSWEBDINGS",2,0)})
   ENDIF
   ::nCharHeight:=Ceiling(::oPrinter:GetCharHeight()/::oPrinter:PixelsPerInchY*MM_TO_INCH)


RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD SetBold(lBold) CLASS WinPrint
RETURN ::SetFont(::cPrevFontName,::nPrevFontSize,lBold,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)


//----------------------------------------------------------------------------------------------------//


METHOD SetItalic(lItalic) CLASS WinPrint
RETURN ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,lItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)


//----------------------------------------------------------------------------------------------------//


METHOD IncreaseFontSize(n,lChangeY) CLASS WinPrint
   LOCAL nPrevCharHeight := ::GetCharHeight()
   DEFAULT lChangeY := .T.
   ::SetFont(::cPrevFontName,::nPrevFontSize+n,::lPrevBold,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
   IF lChangeY
      ::nPrnPosy-=(nPrevCharHeight-::GetCharHeight())
   ENDIF
RETURN SELF


//----------------------------------------------------------------------------------------------------//


METHOD DecreaseFontSize(n,lChangeY) CLASS WinPrint
   LOCAL nPrevCharHeight := ::GetCharHeight()
   DEFAULT lChangeY := .T.
   ::SetFont(::cPrevFontName,::nPrevFontSize-n,::lPrevBold,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
   IF lChangeY
      ::nPrnPosY-=(nPrevCharHeight-::GetCharHeight())
   ENDIF
RETURN SELF



//----------------------------------------------------------------------------------------------------//

// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array {nMul,nDiv} is used to get precise size such a the Dot Matric equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()
METHOD WinPrnSetFont(cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, lStrikeOut, nCharSet) CLASS WinPrint

   LOCAL cType,aFontWidth:={0,0},lSetFontOk

   IF lStrikeOut
      ::oPrinter:SetFont(cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet) // Calling it again for changing some
   ELSE
       // Not needed to call our own function
       RETURN ::oPrinter:SetFont(cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet)
   ENDIF

   IF nWidth != NIL
      cType:= VALTYPE(nWidth)
      IF cType='A'
         aFontWidth     := nWidth
      ELSEIF cType='N' .AND. !EMPTY(nWidth)
         aFontWidth     := {1,nWidth }
      ELSE
         aFontWidth     := {0, 0 }
      ENDIF
   ENDIF

   lSetFontOk:= CreateFont( ::oPrinter:hPrinterDC, cFontName, nPointSize, aFontWidth[1], aFontWidth[2], ;
         nBold, lUnderLine, lItalic, lStrikeOut, nCharSet)

   ::oPrinter:FontName:= GetPrinterFontName(::oPrinter:hPrinterDC)  // Get the font name that Windows actually used

RETURN(lSetFontOk)



//----------------------------------------------------------------------------------------------------//

METHOD TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nTextAlign, nWidth, lWrapText, nBorderWidth, lBackColor) CLASS WinPrint

   LOCAL nTmp,i

   DEFAULT nPosX := (::nPrnPosX) //+ ::X2mm(::oPrinter:LeftMargin)
   DEFAULT nPosY := (::nPrnPosY)
   DEFAULT nTextAlign := 1
   DEFAULT nWidth := (::nPageWidth - ::nLeftMargin - ::nRightMargin) // (::GetTextWidthMM(cString))
   DEFAULT lWrapText := .F.

   (lNewLine); (lUpdatePosX); (nBorderWidth); (lBackColor)

   IF (nTextAlign=1 .OR. nTextAlign=4) .AND. nPoSX+nWidth > (::nPageWidth - ::nRightMargin)
      nWidth-=(nPosX+nWidth - (::nPageWidth - ::nRightMargin))
   ELSEIF nTextAlign=2 .AND. nPoSX+nWidth/2 > (::nPageWidth - ::nRightMargin)
      nWidth-=2*(nPosX+nWidth/2 - (::nPageWidth - ::nRightMargin))
      IF nWidth<0
         RETURN SELF
      ENDIF
   ELSEIF nTextAlign=3 .AND. nPoSX > (::nPageWidth - ::nRightMargin)
      nTmp:=nWidth
      nWidth-=(nPoSX - (::nPageWidth - ::nRightMargin))
      nPosX-=(nPoSX - (::nPageWidth - ::nRightMargin))
      IF nWidth<0
         RETURN SELF
      ENDIF

      IF !lWrapText
         i:=1
         nTmp:=::mm2ActX(nTmp-nWidth)
         WHILE ::oPrinter:GetTextWidth(SubStr(cString,i))>nTmp
            i++
         ENDDO
         cString:=SubStr(cString,1,i)
      ENDIF

   ENDIF

   cString:=RTrim(cString) //,.T.) // Remove white space at right, if any
   ::Say(nPosX+If(nTextAlign=1,0,If(nTextAlign=2,nWidth/2,nWidth)),nPosY,cString,nTextAlign,IF(nWidth=NIL,::GetTextWidthMM(cString),nWidth),,lWrapText)
   ::nPrnPosX+=nWidth

RETURN Self



//----------------------------------------------------------------------------------------------------//

METHOD TextOut(cString, lNewLine, lUpdatePosX, nTextAlign, nWidth, lWrapText, nBorderWidth, lBackColor) CLASS WinPrint

RETURN ::TextOutAt(,, cString, lNewLine, lUpdatePosX, nTextAlign, nWidth, lWrapText, nBorderWidth, lBackColor)




//----------------------------------------------------------------------------------------------------//


METHOD Say(nLeft,nTop,cString,nAlign,nWidth,nHeight,lWrap) CLASS WinPrint

   LOCAL i,j,aString,n,cTmp,lTest:=.F.,lJustified, nNext,nTmp,lFnt,lFirst

   DEFAULT lWrap := .T.

   cString:=StrTran(AllTrim(cString),"~",CRLF)

   lJustified:=(nAlign==4)

   IF nAlign=NIL .OR. nAlign>3
      nAlign:=1
   ENDIF

   IF nAlign=1     // Left
      IF ::lPrintRightOfLeftMargin .AND. nLeft<::nLeftMargin
         nLeft:=::nLeftMargin
      ENDIF
      nAlign:=WFTA_LEFT
   ELSEIF nAlign=2 // Center
      nAlign:=WFTA_CENTER
   ELSEIF nAlign=3 // Right
      nAlign:=WFTA_RIGHT
   ENDIF

   IF nWidth=NIL .OR. Len(cString)<=1 //.OR. At(" ", cString)=0
      IF ::lPaperLength .AND. !::lLatePrn
         ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
      ELSE
         IF ::lPreview
            AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
         ELSE
            AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
         ENDIF
         ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
      ENDIF
      ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
   ELSE
      nWidth:=::Mm2ActX(nWidth)

      lFnt:=(nAlign= 0 .AND. ;
            ("<b>" $ Lower(cString) .OR. "</b>" $ Lower(cString) .OR. ;
            "<i>" $ Lower(cString) .OR. "</i>" $ Lower(cString) .OR. ;
            "<u>" $ Lower(cString) .OR. "</u>" $ Lower(cString) .OR. "<n>" $ Lower(cString)))

      IF lFnt
         cTmp:=StrTran(StrTran(StrTran(cString,"<b>",""),"<i>",""),"<u>","")
         cTmp:=StrTran(StrTran(StrTran(cTmp,"</b>",""),"</i>",""),"</u>","")
         cTmp:=StrTran(cTmp,CRLF," ")
         nTmp:=::oPrinter:GetTextWidth(StrTran(cTmp,"<n>",""))
      ELSE
         nTmp:=::oPrinter:GetTextWidth(cString)
      ENDIF

      IF nTmp<=nWidth .AND. (lFnt .OR. !lWrap)

         IF lFnt
            nLeft:=::mm2x(nLeft)
            cTmp:=""

            cString:=StrTran(cString,CRLF," ")

            i=1
            WHILE i<=Len(cString)
               IF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<n>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  cTmp:=""
                  ::SetFont(::cPrevFontName,::nPrevFontSize,.F.,.F.,.F.,::lPrevStrikeOut,::nForeColor)
                  i+=3
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<b>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,.T.,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</b>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,.F.,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<i>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,.T.,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</i>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,.F.,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<u>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,::lPrevItalic,.T.,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</u>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,::lPrevItalic,.F.,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !::lPaperLength .OR. ::lLatePrn
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
                  ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
               ELSE //Print

                  nNext:=0
                  nTmp:=At("<n>",Lower(cString),i+1)
                  IF nTmp>0
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("<b>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("</b>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("<i>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("</i>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("<u>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  nTmp:=At("</u>",Lower(cString),i+1)
                  IF nTmp>0 .AND. (nNext=0 .OR. nTmp<nNext)
                     nNext:=nTmp
                  ENDIF

                  IF nNext>0
                     nNext:=nNext-i
                  ELSE
                     nNext:=Len(cString)-i+1
                  ENDIF

                  IF ::lPaperLength .AND. !::lLatePrn
                     ::oPrinter:TextOutAt(nLeft,::mm2y(nTop),SubStr(cString,i,nNext),.F.,.F.,nAlign)
                  ELSE
                     IF ::lPreview
                        AAdd(::aPrn, {"TEXTOUTAT",::X2Mm(nLeft+::oPrinter:LeftMargin),nTop,SubStr(cString,i,nNext),.F.,.F.,nAlign})
                     ELSE
                        AAdd(::aPrn, {"TEXTOUTAT",nLeft,::mm2y(nTop),SubStr(cString,i,nNext),.F.,.F.,nAlign})
                     ENDIF
                  ENDIF
                  cTmp:=SubStr(cString,i,nNext)
                  i+=nNext
               ENDIF

           END

         ELSE
            IF ::lPaperLength .AND. !::lLatePrn
               ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
            ELSE
               IF ::lPreview
                  AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
               ELSE
                  AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
               ENDIF
              ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
            ENDIF
            ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
         ENDIF
      ELSEIF !lWrap
         cString:=RTrim(cString) //,.T.) // As it's single line printing, remove any right white space
         WHILE ::oPrinter:GetTextWidth(cString)>nWidth .AND. Len(cString)!=0
            cString:=Left(cString,Len(cString)-1)
         ENDDO
         IF ::lPaperLength .AND. !::lLatePrn
            ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
         ELSE
            IF ::lPreview
               AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
            ELSE
               AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
            ENDIF
           ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
         ENDIF
         ::nLineHeight:=Max(::nLineHeight,::nCharHeight)
      ELSE
         aString:=hb_ATokens(cString,CRLF)
         FOR i:=1 TO Len(aString)
            aString[i]:=aString[i]+"~"
         NEXT
         IF nHeight!=NIL
            // Change by SB on 07/03/2017
            nHeight:=::mm2acty(nHeight)
            // nHeight:=::mm2y(nHeight)
         ENDIF
         FOR i=1 TO Len(aString)
            FOR j=1 TO Len(aString[i])
               IF j=Len(aString[i]) .AND. aString[i][j]=="~"
                  LOOP
               ENDIF
               IF ::oPrinter:GetTextWidth(Left(aString[i],j))>nWidth
                  EXIT
               ENDIF
            NEXT

            IF j>Len(aString[i])
               LOOP
            ENDIF

            n:=j-1
            lFirst:=.T.
            WHILE n>1
               cTmp:=SubStr(aString[i],n,1)
               IF IsSpace(cTmp) .OR. cTmp$".,?-;"
                  cTmp:=SubStr(aString[i],n+1)
                  aString[i]:=Left(aString[i],n)
                  IF Len(aString)>i
                     AIns(aString,i+1,cTmp,.T.)
                  ELSEIF Len(cTmp)>0
                     AAdd(aString,cTmp)
                  ENDIF
                  lFirst:=.F.
                  EXIT
               ENDIF
               n--
            ENDDO

            IF lFirst
               n:=j-1
               cTmp:=SubStr(aString[i],n+1)
               aString[i]:=Left(aString[i],n)
               IF Len(aString)>i
                  AIns(aString,i+1,cTmp,.T.)
               ELSEIF Len(cTmp)>0
                  AAdd(aString,cTmp)
               ENDIF
            ENDIF

            IF n<2
               n:=j-1
               cTmp:=SubStr(aString[i],n+1)
               aString[i]:=Left(aString[i],n)
               IF Len(aString)>i
                  AIns(aString,i+1,cTmp,.T.)
               ELSEIF Len(cTmp)>0
                  AAdd(aString,cTmp)
               ENDIF

               IF ::oPrinter:GetTextWidth(cTmp)>=nWidth
                  i++
                  LOOP
               ENDIF

            ENDIF
         NEXT

         // n is the differential increment of y axis
         n:=0

         nTop:=::mm2y(nTop)
         FOR i=1 TO Len(aString)
            aString[i]:=AllTrim(aString[i])
            IF i>1
               n+=IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
            ENDIF
            IF Right(aString[i],1)=="~"
               aString[i]:=Left(aString[i],Len(aString[i])-1)
            ELSEIF lJustified .AND. i<Len(aString)
               aString[i]:=::GetJustifiedString(aString[i],nWidth)
            ENDIF
            IF nHeight=NIL .OR. nHeight>=n-IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
               IF ::lPaperLength .AND. !::lLatePrn
                  ::oPrinter:TextOutAt(::mm2x(nLeft),nTop+n,aString[i],.F.,.F.,nAlign)
               ELSE
                  IF ::lPreview
                     AAdd(::aPrn, {"TEXTOUTAT",nLeft,::y2mm(nTop+n+::oPrinter:TopMargin),aString[i],.F.,.F.,nAlign})
                  ELSE
                     AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),nTop+n,aString[i],.F.,.F.,nAlign})
                  ENDIF
                  ::nMaxY:=Max(::nMaxY,nTop+n+::nCharHeight)
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT
         ::nLineHeight:=Max(::nLineHeight, ::Y2mm(::oPrinter:GetCharHeight()+n))
      ENDIF
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD GetJustifiedString(cString,nWidth) CLASS WinPrint

   LOCAL i,cTemp,cPrevTemp

   cPrevTemp:=cString

   WHILE Rat(" ", AllTrim(cString))>0 .AND. ::oPrinter:GetTextWidth(cString)<nWidth

      i:=Len(cString)-1

      WHILE i>1
         IF IsSpace(cString[i])
            cTemp:=Left(cString,i-1)+" "+SubStr(cString,i)
            IF ::oPrinter:GetTextWidth(cTemp)>nWidth
               EXIT
            ELSE
               cString:=cTemp
            ENDIF
         ENDIF
         i--
      ENDDO

      IF cPrevTemp==cString // Nothing to be changed
         EXIT
      ELSEIF ::oPrinter:GetTextWidth(cString)<nWidth/3
         EXIT
      ENDIF

      cPrevTemp:=cString

   ENDDO

RETURN cString



//----------------------------------------------------------------------------------------------------//


METHOD PageBreak() CLASS WinPrint

   ::nPrnPosX:=0
   ::nPrnPosY:=0

   IF ::lPaperLength .AND. !::lLatePrn
      IF ::lPreview .OR. ::nCopies>1
         ::EmfPrnPageEnd()
         ::EmfPrnPageStart()
      ELSE
         ::oPrinter:EndPage(.F.)
         ::oPrinter:StartPage()
         ::oPrinter:SetBkMode(1)
         ::oPrinter:SetPrc(0,0)
      ENDIF
   ELSE
      (::cAlias)->(DbAppend())
      (::cAlias)->PAGENO:=::nPage
      (::cAlias)->PAGEDATA:=::aPrn
      ::aPrn:={}
   ENDIF

   ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,IF(::lGrayScale,0,::nForeColor),.T.)

   ::nPage++

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Line(t,l,b,r,w,c) CLASS WinPrint

   LOCAL nPrevColor

   IF w=NIL .AND. c=NIL
      IF ::lPaperLength .AND. !::lLatePrn
         ::oPrinter:Line(::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b))
      ELSE
         IF ::lPreview
            AAdd(::aPrn, {"LINE", l, t, r, b})
         ELSE
            AAdd(::aPrn, {"LINE", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
         ENDIF
         ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
      ENDIF
   ELSE
   
      DEFAULT w to ::nPenWidth
      DEFAULT c to NumToHex(::oPrinter:PenColor)

      nPrevColor:=::oPrinter:PenColor

      IF ::lPaperLength .AND. !::lLatePrn
         ::oPrinter:SetPen(::oPrinter:PenStyle,Int(w*::oPrinter:PixelsPerInchY/MM_TO_INCH),IF(::lGrayScale,0,HexToNum(c)))
         ::oPrinter:Line(::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b))
         ::oPrinter:SetPen(::oPrinter:PenStyle,Int(::nPenWidth*::oPrinter:PixelsPerInchY/MM_TO_INCH),nPrevColor)
      ELSE
         IF ::lPreview
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,w,IF(::lGrayScale,0,HexToNum(c))})
            AAdd(::aPrn,{"LINE", l, t, r, b})
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,::nPenWidth,IF(::lGrayScale,0,nPrevColor)})
         ELSE
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,Int(w*::oPrinter:PixelsPerInchY/MM_TO_INCH),IF(::lGrayScale,0,HexToNum(c))})
            AAdd(::aPrn,{"LINE", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,Int(::nPenWidth*::oPrinter:PixelsPerInchY/MM_TO_INCH),IF(::lGrayScale,0,nPrevColor)})
         ENDIF
         ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
      ENDIF
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Image(cImage,t,l,b,r) CLASS WinPrint

   LOCAL oBmp

   cImage:=If(Upper(Right(cImage,3))=="BMP" .AND. !::lGrayScale,cImage,WinPrint_ToBmp(cImage,::lGrayScale))
   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview .AND. !::lLatePrn
      oBMP:= Win32BMP():new()
      l:=::mm2x(l)
      t:=::mm2y(t)
      r:= Round(r * ::oPrinter:PixelsPerInchX / MM_TO_INCH, 0)
      b:= Round(b * ::oPrinter:PixelsPerInchY / MM_TO_INCH, 0)
      IF oBmp:loadFile( cImage )
         oBmp:Draw( ::oPrinter,  {l, t, r, b} )
      ENDIF
      oBMP:Destroy()
   ELSEIF ::lPaperLength .AND. !::lLatePrn
      IF ::lPreview 
         t-=::nCharHeight
      ENDIF
      Emf_DrawImage(::hDC,cImage,t,l,b,r,.T.,.F.)
   ELSE
      IF ::lPreview
         t-=::nCharHeight
      ENDIF
      AAdd(::aPrn, {"DRAW", cImage, l, t, r, b})
      ::nMaxY:=Max(::nMaxY, Max(t,b)+::nCharHeight)
   ENDIF


RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Box(t,l,b,r) CLASS WinPrint

   IF ::lPaperLength .AND. !::lLatePrn
      ::oPrinter:Box( ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) )
   ELSE
      IF ::lPreview
         AAdd(::aPrn, {"BOX", l, t, r, b})
      ELSE
         AAdd(::aPrn, {"BOX",::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
      ENDIF
      ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
      //IF ::lPreview
      //   AAdd(::aPrn, {"BOXPDF", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
      //ENDIF
   ENDIF

RETURN Self



//----------------------------------------------------------------------------------------------------//


METHOD Shadow(t,l,b,r,cHexShadowColor,nBoxType) CLASS WinPrint

   IF cHexShadowColor=NIL
      cHexShadowColor:=::cHexShadowColor
   ENDIF

   IF Upper(cHexShadowColor)=="FFFFFF"
      RETURN Self
   ENDIF

   IF ::lGrayScale
      cHexShadowColor:="DCDCDC"
   ENDIF

   DEFAULT nBoxType := 1

   IF ::lPaperLength .AND. !::lLatePrn
      IF nBoxType!=1
         ::oPrinter:box(::mm2x(l-0.25), ::mm2y(t-0.25), ::mm2x(r+0.25), ::mm2y(b+0.25) )
      ENDIF
      ::oPrinter:FillRect( ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) , HexToNum(cHexShadowColor) )
   ELSE
      IF nBoxType!=1
         IF ::lPreview
            AAdd(::aPrn, {"BOX", l-0.25, t-0.25, r+0.25, b+0.25} )
         ELSE
            AAdd(::aPrn, {"BOX", ::mm2x(l-0.25), ::mm2y(t-0.25), ::mm2x(r+0.25), ::mm2y(b+0.25)} )
         ENDIF
      ENDIF
      IF ::lPreview
         AAdd(::aPrn, {"FILLRECT", l, t, r, b, HexToNum(cHexShadowColor)})
      ELSE
         AAdd(::aPrn, {"FILLRECT", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) , HexToNum(cHexShadowColor)})
      ENDIF
      //IF ::lPreview
      //   AAdd(::aPrn, {"FILLRECTPDF", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) , cHexShadowColor, nBoxType})
      //ENDIF
      ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD GetTextWidthMM(cString) CLASS WinPrint

RETURN ::oPrinter:GetTextWidth(cString)/::oPrinter:PixelsPerInchX*MM_TO_INCH 


//----------------------------------------------------------------------------------------------------//


METHOD GetTextArray(cString,nWidth) CLASS WinPrint

   LOCAL i,j,aString,n,cTmp,lFirst

   nWidth:=::mm2ActX(nWidth)

   aString:=hb_ATokens(cString,CRLF)

   FOR i=1 TO Len(aString)
      FOR j=1 TO Len(aString[i])
         IF ::oPrinter:GetTextWidth(Left(aString[i],j))>nWidth
            EXIT
         ENDIF
      NEXT
      IF j>Len(aString[i])
         LOOP
      ENDIF

      n:=j-1
      lFirst:=.T.
      WHILE n>1
         cTmp:=SubStr(aString[i],n,1)
         IF IsSpace(cTmp) .OR. cTmp$".,?-;"
            cTmp:=SubStr(aString[i],n+1)
            aString[i]:=Left(aString[i],n)
            IF Len(aString)>i
               AIns(aString,i+1,cTmp,.T.)
            ELSEIF Len(cTmp)>0
               AAdd(aString,cTmp)
            ENDIF
            lFirst:=.F.
            EXIT
         ENDIF
         n--
      ENDDO


      IF lFirst
         n:=j-1
         cTmp:=SubStr(aString[i],n+1)
         aString[i]:=Left(aString[i],n)
         IF Len(aString)>i
            AIns(aString,i+1,cTmp,.T.)
         ELSEIF Len(cTmp)>0
            AAdd(aString,cTmp)
         ENDIF
      ENDIF

      IF n<2
         n:=j-1
         cTmp:=SubStr(aString[i],n+1)
         aString[i]:=Left(aString[i],n)
         IF Len(aString)>i
            AIns(aString,i+1,cTmp,.T.)
         ELSEIF Len(cTmp)>0
            AAdd(aString,cTmp)
         ENDIF
         IF ::oPrinter:GetTextWidth(cTmp)>=nWidth
            i++
            LOOP
         ENDIF

      ENDIF
   NEXT

RETURN aString

//----------------------------------------------------------------------------------------------------//

METHOD GetTextHeightMM(cString,nWidth) CLASS WinPrint

   LOCAL i,aString,n
   
   IF nWidth=NIL
      RETURN Ceiling(::oPrinter:GetTextHeight(cString)/::oPrinter:PixelsPerInchY*MM_TO_INCH)
   ENDIF

   aString:=::GetTextArray(cString,nWidth)

   n:=0
   FOR i=1 TO Len(aString)
      n+=IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
   NEXT

RETURN Ceiling(n/::oPrinter:PixelsPerInchY*MM_TO_INCH)




//----------------------------------------------------------------------------------------------------//


METHOD EmfPrnStart() CLASS WinPrint

   LOCAL nMaxY

   ::nPrevColor:=-1
   ::aPrevFont:={}

   IF !::lPaperLength
      nMaxY:=Int(::nMaxY/::oPrinter:PixelsPerInchY*MM_TO_INCH)+::nTopMargin
   ENDIF

   ::oPrinter:SetDuplexType(1)
   ::oPrinter:EndDoc(.T.)

   TRY
    ::oPrinter:Destroy()
   CATCH
   END

   ::oPrinter := Win32Prn():New(::cPrinter)
   ::oPrinter:Landscape:=::lLandscape

   IF ::lPaperLength
      IF Val(::cFormType)=DMPAPER_CUSTOM
         ::oPrinter:FormType:=0
         ::oPrinter:PaperWidth:=::nPageWidth*10
         ::oPrinter:PaperLength:=::nPageHeight*10
      ELSE
         ::oPrinter:FormType:=Val(::cFormType)
      ENDIF
   ELSE
      ::oPrinter:FormType:=0
      ::oPrinter:PaperWidth:=::nPageWidth*10
      ::oPrinter:PaperLength:=nMaxY*10
   ENDIF

   ::oPrinter:Create()

   ::oPrinter:SetBkMode(1)
   ::oPrinter:SetPrc(0,0)

   ::hDC:=::oPrinter:hPrinterDC
   ::hDCBak:=::hDC
   ::aEmfName:={}
   ::cEmfPrefix:="Preview_"+DtoS(Date())+"_"+NToC(Seconds())+"_"

   ::EmfPrnPageStart()

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD EmfPrnPageStart() CLASS WinPrint

   ::cEmfName:=TempFileName(,,"EMF")
   ::hDC := StartPage_Preview(::hDCBak,::cEmfName)
   ::oPrinter:hPrinterDC:=::hDC
   AAdd(::aEmfName,::cEmfName)
   ::oPrinter:SetBkMode(1)
   ::oPrinter:SetPrc(0,0)

RETURN NIL


//----------------------------------------------------------------------------------------------------//

METHOD EmfPrnPageEnd() CLASS WinPrint

   EndPage_Preview(::hDC)

   IF !File(::cEmfName)
      MsgAlert("Sorry, EMF "+::cEmfName+" could not be created (Code:1313)")
   ENDIF

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD EmfPrnEnd() CLASS WinPrint

   ::EmfPrnPageEnd()

   ::oPrinter:hPrinterDC:=::hDCBak

   ::aPrnData:={}

   AAdd(::aPrnData, {"PRINTER",::cPrinter})
   AAdd(::aPrnData, {"LANDSCAPE",::lLandScape})
   AAdd(::aPrnData, {"FORMTYPE",::oPrinter:FormType})
   AAdd(::aPrnData, {"PAPERWIDTH",::oPrinter:PaperWidth})

   IF ::lPaperLength
      AAdd(::aPrnData, {"PAPERLENGTH", ::oPrinter:PaperLength})
   ELSE
      AAdd(::aPrnData, {"PAPERLENGTH",Int((::nMaxY/::oPrinter:PixelsPerInchY*MM_TO_INCH)+::nTopMargin)*10})
   ENDIF

   AAdd(::aPrnData, {"RIGHTMARGIN", ::nRightMargin})
   AAdd(::aPrnData, {"EMFNAMES", ::aEmfName})
   AAdd(::aPrnData, {"GREYSCALE", ::lGrayScale})

   TRY
    ::oPrinter:Destroy()
   CATCH
   END

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD EmfPrint() CLASS WinPrint

   LOCAL oPrinter,nCopyCtr,i

   oPrinter := Win32Prn():New(::cPrinter)

   IF oPrinter==NIL
      MsgAlert("Cannot initiated printer object")
      RETURN NIL
   ENDIF

   WITH OBJECT oPrinter
      :Landscape:=::lLandscape

      IF Val(::cFormType)=0
         :FormType:=Val(::cFormType)
         :PaperWidth:=::nPaperWidth
         :PaperLength:=::nPaperLength
      ELSE
         :FormType:=Val(::cFormType)
      ENDIF

      IF ::lBestQuality
         :setPrintQuality(-4) // Highest printer quality
      ENDIF

      IF !:Create()
         TRY
            :Destroy()
         CATCH
         END
         MsgAlert("Cannot create printer object")
         RETURN NIL
      ENDIF

      IF !:StartDoc()
         :Destroy()
         MsgAlert("Cannot start document")
         RETURN NIL
      ENDIF

      :TextOutAt(0,0,"",.F.,.F.) // Dummy TextOut, otherwise PlayEnhMetaFile doesn't work!

      FOR nCopyCtr=1 TO ::nCopies
         FOR i=1 TO Len(::aEmfName)

            :StartPage()

            WinPrint_PlayEnhMetaFile(::aEmfName[i],:hPrinterDC,0,0,GetDeviceCaps(:hPrinterDC,WFHORZRES),GetDeviceCaps(:hPrinterDC,WFVERTRES))

            :EndPage(.F.)

         NEXT
      NEXT

      :EndDoc()

      TRY
         :Destroy()
      CATCH
      END

   END

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD Preview() CLASS WinPrint

   LOCAL j,cPath,cAlias2,cFile2,lUse
   
   IF !::lPaperLength .OR. ::lLatePrn

      IF Len(::aPrn)>0
         (::cAlias)->(DbAppend())
         (::cAlias)->PAGENO:=::nPage
         (::cAlias)->PAGEDATA:=::aPrn
         ::aPrn:={}
      ENDIF

      ::EmfPrnStart()

      IF ::nPrevColor!=-1
         ::oPrinter:SetColor(::nPrevColor)
      ENDIF

      IF Len(::aPrevFont)!=0
         ::wfSetFont(::aPrevfont[1],::aPrevFont[2],::aPrevFont[3],::aPrevFont[4],::aPrevFont[5],::aPrevFont[6],::aPrevFont[7],::aPrevFont[8])
      ENDIF
            
      (::cAlias)->(DbGoTop())

      WHILE !(::cAlias)->(Eof())

         ::aPrn:=(::cAlias)->PAGEDATA
         ::oPrinter:SetBkMode(1)
         ::oPrinter:SetPrc(0,0)

         IF ::nPrevColor!=-1
            ::oPrinter:SetColor(::nPrevColor)
         ENDIF

         IF Len(::aPrevFont)!=0
            ::wfSetFont(::aPrevfont[1],::aPrevFont[2],::aPrevFont[3],::aPrevFont[4],::aPrevFont[5],::aPrevFont[6],::aPrevFont[7],::aPrevFont[8])
         ENDIF


         FOR j=1 TO Len(::aPrn)
            IF ::aPrn[j][1]=="SETCOLOR"
               ::oPrinter:SetColor(::aPrn[j][2])
               ::nPrevColor:=::aPrn[j][2]
            ELSEIF ::aPrn[j][1]=="SETFONT"
               ::wfSetFont(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9])
               ::aPrevFont:={::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9]}

            ELSEIF ::aPrn[j][1]=="TEXTOUTAT"
               ::aPrn[j][4]:=StrTran(::aPrn[j][4],"@Pages",LTrim(Str( Ceiling((::cAlias)->(RecCount()) * IF(::lDuplex .AND. ::lNextPageSameNum, 1/2, 1)) )))
               ::aPrn[j][4]:=StrTran(::aPrn[j][4],"@Page",LTrim( Str( Ceiling((::cAlias)->(RecNo()) * IF(::lDuplex .AND. ::lNextPageSameNum, 1/2, 1)) )))
               ::oPrinter:TextOutAt(::mm2x(::aPrn[j][2]),::mm2Y(::aPrn[j][3]),::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7])
            ELSEIF ::aPrn[j][1]=="LINE"
               ::oPrinter:Line(::mm2x(::aPrn[j][2]),::mm2y(::aPrn[j][3]),::mm2x(::aPrn[j][4]),::mm2y(::aPrn[j][5]))
            ELSEIF ::aPrn[j][1]=="DRAW"
               Emf_DrawImage(::hDC,::aPrn[j][2],::aPrn[j][4],::aPrn[j][3],::aPrn[j][6],::aPrn[j][5],.T.,.F.)
            ELSEIF ::aPrn[j][1]=="BOX"
               ::oPrinter:Box(::mm2x(::aPrn[j][2]),;
                              ::mm2y(::aPrn[j][3]),;
                              ::mm2x(::aPrn[j][4]),;
                              ::mm2y(::aPrn[j][5]))
            ELSEIF ::aPrn[j][1]=="FILLRECT"
               ::oPrinter:FillRect(::mm2x(::aPrn[j][2]),;
                              ::mm2y(::aPrn[j][3]),;
                              ::mm2x(::aPrn[j][4]),;
                              ::mm2y(::aPrn[j][5]),;
                              ::aPrn[j][6])
            ELSEIF ::aPrn[j][1]=="SETPEN"
               ::oPrinter:SetPen(::aPrn[j][2],Int(::aPrn[j][3]*::oPrinter:PixelsPerInchY/MM_TO_INCH),::aPrn[j][4])
            ENDIF
         NEXT j

         (::cAlias)->(DbSkip())

         IF !(::cAlias)->(Eof())
            ::EmfPrnPageEnd()
            ::EmfPrnPageStart()
         ENDIF

      ENDDO

      TRY
         (::cAlias)->(DbCloseArea())
      CATCH
      END

   ENDIF
   
   ::EmfPrnEnd()  // For both ::lPaperLength .T./.F.


   // Create temporary for printer data
   lUse:=.T.
   cAlias2:=GetNewAlias()
   cFile2:=TempFileName()
   TRY
    REQUEST RMDBFCDX
    RddSetDefault( "RMDBFCDX" )    
    dbCreate(cFile2,{{"PRNPROP","M",10,0},{"PRNVAL","M",10,0}})
    dbUseArea(.T.,,cFile2,cAlias2,.F.)
   CATCH
    lUse:=.F.
   END
   IF !lUse
      MsgAlert("Cannot create file "+cFile2+" (Code:708)")
      RETURN {}
   ENDIF
   
   FOR j=1 TO Len(::aPrnData)
      (cAlias2)->(DbAppend())
      (cAlias2)->PRNPROP:=::aPrnData[j][1]
      (cAlias2)->PRNVAL:=::aPrnData[j][2]
   NEXT

   (cAlias2)->(DbCloseArea())

   cPath:=Left( GetModuleFileName(), Rat("\" ,GetModuleFileName() )-1 )
   
   IF File(cPath+"\PrintPreview.exe")
      TRY
         ShellExecute(0,"OPEN",cPath+"\PrintPreview.exe",cFile2,"",1)
      CATCH
         MsgAlert("Print preview data file corrupted")
      END
   ELSE
      MsgAlert("Cannot find "+cPath+"\PRINTPREVIEW.EXE")
   ENDIF


RETURN ::aPrnData


//----------------------------------------------------------------------------------------------------//

//---- ALL parameters in mm
STATIC FUNCTION Emf_DrawImage(hDC,cImage,nRow,nCol,nHeight,nWidth,lStretch,lTransparent )

   nRow:=Int(nRow*10000/254 )
   nCol:=Int(nCol*10000/254 )
   nWidth:=Int(nWidth*10000/254 )
   nHeight:=Int(nHeight*10000/254 )

   Emf_DrawImage_LowLevel(hDC,cImage,nRow,nCol,nHeight,nWidth,lStretch,lTransparent)

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD Close(lAbort) CLASS WinPrint

   LOCAL j,oBmp,nMaxY,nCopyCtr

   DEFAULT lAbort := .F.

   IF ::lError .OR. lAbort .OR. ::lPreview
      TRY
         IF ::lPaperLength
            ::oPrinter:SetDuplexType(1)
            ::oPrinter:EndDoc(.T.)
            ::oPrinter:Destroy()
         ELSE
            ::EmfPrnEnd()
         ENDIF
      CATCH
      END
      RETURN Self
   ENDIF

   IF ::lPaperLength .AND. !::lLatePrn .AND. ::nCopies>1

      ::EmfPrnEnd()
      ::EmfPrint()

   ELSEIF !::lPaperLength .OR. ::lLatePrn

      IF Len(::aPrn)>0
         (::cAlias)->(DbAppend())
         (::cAlias)->PAGENO:=::nPage
         (::cAlias)->PAGEDATA:=::aPrn
         ::aPrn:={}
      ENDIF


      nMaxY:=Int(::nMaxY/::oPrinter:PixelsPerInchY*MM_TO_INCH)+::nTopMargin
      ::oPrinter:SetDuplexType(1)
      ::oPrinter:EndDoc(.T.)
      TRY
       ::oPrinter:Destroy()
      CATCH
      END

      ::oPrinter := Win32Prn():New(::cPrinter)

      ::oPrinter:Landscape:=::lLandscape
      ::oPrinter:AskProperties:=::lAskProperties

      IF ::lPaperLength
         IF Val(::cFormType)=DMPAPER_CUSTOM
            ::oPrinter:FormType:=0
            ::oPrinter:PaperWidth:=::nPageWidth*10
            ::oPrinter:PaperLength:=::nPageHeight*10
         ELSE
           ::oPrinter:FormType:=Val(::cFormType)
         ENDIF
      ELSE
         ::oPrinter:FormType:=0
         ::oPrinter:PaperWidth:=::nPageWidth*10
         ::oPrinter:PaperLength:=nMaxY*10
      ENDIF

      IF ::lBestQuality
         ::oPrinter:setPrintQuality(-4) // Highest printer quality
      ENDIF

      IF ::lDuplex
         IF ::oPrinter:SetDuplexType()=1
            ::oPrinter:setDuplexType(3) // Horizontal Duplex
         ENDIF
      ELSE
         ::oPrinter:SetDuplexType(1) // Made it simplex
      ENDIF

      ::oPrinter:Create()
      ::oPrinter:StartDoc()

      ::oPrinter:SetBkMode(1)
      ::oPrinter:SetPrc(0,0)
      
      FOR nCopyCtr=1 TO ::nCopies

         (::cAlias)->(DbGoTop())

         WHILE !(::cAlias)->(Eof())

            ::aPrn:=(::cAlias)->PAGEDATA

            FOR j=1 TO Len(::aPrn)
               IF ::aPrn[j][1]=="SETCOLOR"
                  ::oPrinter:SetColor(::aPrn[j][2])
               ELSEIF ::aPrn[j][1]=="SETFONT"
                  ::wfSetFont(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9])
               ELSEIF ::aPrn[j][1]=="TEXTOUTAT"
                  ::aPrn[j][4]:=StrTran(::aPrn[j][4],"@Pages",LTrim(Str( Ceiling((::cAlias)->(RecCount()) * IF(::lDuplex .AND. ::lNextPageSameNum,1/2,1)) )))
                  ::aPrn[j][4]:=StrTran(::aPrn[j][4],"@Page",LTrim(Str( Ceiling((::cAlias)->(RecNo()) * IF(::lDuplex .AND. ::lNextPageSameNum,1/2,1)) )))
                  ::oPrinter:TextOutAt(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7])
               ELSEIF ::aPrn[j][1]=="LINE"
                  ::oPrinter:Line(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5])
               ELSEIF ::aPrn[j][1]=="DRAW"
                  oBMP:= Win32BMP():new()
                  IF oBmp:loadFile( ::aPrn[j][2] )
                     oBmp:Draw( ::oPrinter,  { ::mm2x(::aPrn[j][3]),::mm2y(::aPrn[j][4]),;
                        Round(::aPrn[j][5] * ::oPrinter:PixelsPerInchX / MM_TO_INCH, 0),;
                        Round(::aPrn[j][6] * ::oPrinter:PixelsPerInchY / MM_TO_INCH, 0) } )
                  ENDIF
                  oBMP:Destroy()
               ELSEIF ::aPrn[j][1]=="BOX"
                  ::oPrinter:Box(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5])
               ELSEIF ::aPrn[j][1]=="FILLRECT"
                  ::oPrinter:FillRect(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6])
               ELSEIF ::aPrn[j][1]=="SETPEN"
                  ::oPrinter:SetPen(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4])
               ENDIF
            NEXT j

            IF (::cAlias)->(RecNo())<(::cAlias)->(LastRec())
               ::oPrinter:EndPage(.F.)
               ::oPrinter:StartPage()
               ::oPrinter:SetBkMode(1)
               ::oPrinter:SetPrc(0,0)
            ENDIF

            (::cAlias)->(DbSkip())

         ENDDO

         IF nCopyCtr<::nCopies
            ::oPrinter:EndPage(.F.)
            ::oPrinter:StartPage()
            ::oPrinter:SetBkMode(1)
            ::oPrinter:SetPrc(0,0)
         ENDIF


      NEXT nCopyCtr

      ::oPrinter:SetDuplexType(1)
      ::oPrinter:EndDoc()

      TRY
         (::cAlias)->(DbCloseArea())
      CATCH
      END

   ELSE
      ::oPrinter:EndDoc()
   ENDIF

   ::oPrinter:=NIL

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD mm2actx(nMm) CLASS WinPrint        // mm to actual X axis

RETURN Int((nMM*::oPrinter:PixelsPerInchX)/MM_TO_INCH)


//----------------------------------------------------------------------------------------------------//


METHOD mm2acty(nMm)  CLASS WinPrint       // mm to actual Y axis

RETURN Int((nMM*::oPrinter:PixelsPerInchY)/MM_TO_INCH)



//----------------------------------------------------------------------------------------------------//


METHOD mm2x(nMm) CLASS WinPrint

RETURN ::oPrinter:MM_TO_POSX(nMM)
// RETURN ::oPrinter:MM_TO_POSX(nMM)+::oPrinter:LeftMargin


//----------------------------------------------------------------------------------------------------//


METHOD mm2y(nMm)  CLASS WinPrint

RETURN ::oPrinter:MM_TO_POSY(nMM)
// RETURN ::oPrinter:MM_TO_POSY(nMM)+::oPrinter:TopMargin



//----------------------------------------------------------------------------------------------------//


METHOD SetPosX(x) CLASS WinPrint

   ::nPrnPosX:=x

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD SetPosY(y) CLASS WinPrint

   ::nPrnPosY:=y

RETURN NIL


//----------------------------------------------------------------------------------------------------//


METHOD SetPos(x,y) CLASS WinPrint

   ::nPrnPosX:=x
   ::nPrnPosY:=y

RETURN NIL


//----------------------------------------------------------------------------------------------------//

METHOD GetPosX() CLASS WinPrint

RETURN ::nPrnPosX



//----------------------------------------------------------------------------------------------------//

METHOD GetPosY() CLASS WinPrint

RETURN ::nPrnPosY


//----------------------------------------------------------------------------------------------------//

METHOD GetPageHeight() CLASS WinPrint
RETURN ::nPageHeight

//----------------------------------------------------------------------------------------------------//

METHOD GetPageWidth() CLASS WinPrint
RETURN ::nPageWidth


//----------------------------------------------------------------------------------------------------//

METHOD GetCharHeight() CLASS WinPrint
RETURN ::nCharHeight// +IF(::lDtlLine,::nDtlLineRowGap,1)
//RETURN ::nCharHeight+IF(::lDtlLine,::nDtlLineRowGap,1)





//----------------------------------------------------------------------------------------------------//


METHOD X2mm(n) CLASS WinPrint
RETURN n*25.4/::oPrinter:PixelsPerInchX
//RETURN (n+::oPrinter:LeftMargin)*25.4/::oPrinter:PixelsPerInchX


//----------------------------------------------------------------------------------------------------//


METHOD Y2mm(n) CLASS WinPrint
RETURN n*25.4/::oPrinter:PixelsPerInchY
//RETURN (n+::oPrinter:TopMargin)*25.4/::oPrinter:PixelsPerInchY



//----------------------------------------------------------------------------------------------------//


METHOD GetDtlLine() CLASS WinPrint
RETURN ::lDtlLine


//----------------------------------------------------------------------------------------------------//


METHOD SetDtlLine(lDtlLine) CLASS WinPrint
   ::lDtlLine:=lDtlLine
RETURN ::lDtlLine


//----------------------------------------------------------------------------------------------------//


METHOD GetNextToHeader() CLASS WinPrint
RETURN ::lNextToHeader


//----------------------------------------------------------------------------------------------------//


METHOD SetNextToHeader(lNextToHeader) CLASS WinPrint
   ::lNextToHeader:=lNextToHeader
RETURN ::lNextToHeader



//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_ToBmp(cFile,lGrayScale)

  LOCAL im, im2, nFif, cTemp

  DEFAULT lGrayScale := .F.

  nFif := FreeImageGetFileType(cFile)
  IF nFif==FIF_BMP .AND. !lGrayScale
     RETURN cFile
  ENDIF

  im := FreeImageLoad(nFif,cFile,0)
  IF lGrayScale
     im2 := fi_Clone( im )
     FreeImageUnload( im )
     im := fi_ConvertToGreyScale( im2 )
     FreeImageUnload( im2 )
     IF nFif==FIF_BMP
        cTemp := TempFileName(,,"BMP")
        fi_Save( FIF_BMP, im, cTemp, BMP_DEFAULT )
        FreeImageUnload(im)
        RETURN cTemp
     ENDIF
  ENDIF
  im2 := fi_Clone( im )
  FreeImageUnload( im )
  cTemp := TempFileName(,,"BMP")
  fi_Save( FIF_BMP, im2, cTemp, BMP_DEFAULT )

  FreeImageUnload( im2 )
RETURN cTemp




//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_ToJpg(cFile,lGrayScale)

  LOCAL im, im2, nFif, cTemp

  DEFAULT lGrayScale := .F.

  nFif := FreeImageGetFileType(cFile)
  IF nFif==FIF_JPEG .AND. !lGrayScale
     RETURN cFile
  ENDIF

  im := FreeImageLoad(nFif,cFile,0)
  IF lGrayScale
     im2 := fi_Clone( im )
     FreeImageUnload( im )
     im := fi_ConvertToGreyScale( im2 )
     FreeImageUnload( im2 )
     IF nFif==FIF_JPEG
        cTemp := TempFileName(,,"JPG")
        fi_Save( FIF_JPEG, im, cTemp, JPEG_DEFAULT )
        FreeImageUnload(im)
        RETURN cTemp
     ENDIF
  ENDIF
  im2 := fi_Clone( im )
  FreeImageUnload( im )
  cTemp := TempFileName(,,"JPG")
  fi_Save( FIF_JPEG, im2, cTemp, JPEG_DEFAULT )

  FreeImageUnload( im2 )

RETURN cTemp


//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_ToPng(cFile,lGrayScale)

  LOCAL im, im2, nFif, cTemp

  DEFAULT lGrayScale := .F.

  nFif := FreeImageGetFileType(cFile)
  IF nFif==FIF_PNG .AND. !lGrayScale
     RETURN cFile
  ENDIF

  im := FreeImageLoad(nFif,cFile,0)
  IF lGrayScale
     im2 := fi_Clone( im )
     FreeImageUnload( im )
     im := fi_ConvertToGreyScale( im2 )
     FreeImageUnload( im2 )
     IF nFif==FIF_PNG
        cTemp := TempFileName(,,"PNG")
        fi_Save( FIF_PNG, im, cTemp, PNG_DEFAULT )
        FreeImageUnload(im)
        RETURN cTemp
     ENDIF
  ENDIF
  im2 := fi_Clone( im )
  FreeImageUnload( im )
  cTemp := TempFileName(,,"PNG")
  fi_Save( FIF_PNG, im2, cTemp, PNG_DEFAULT )

  FreeImageUnload( im2 )

RETURN cTemp



//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_ImageRescale(cFile,nWidth,nHeight)

   LOCAL nFif, im, im2, cExt, cTemp

   nFif := FreeImageGetFileType(cFile)

   cExt:=WinPrint_FileExt(cFile)

   im := FreeImageLoad(nFif, cFile, 0)
   im2 := fi_Rescale( im, nWidth,nHeight, FILTER_BICUBIC )
   cTemp := TempFileName(,,cExt)
   fi_Save( nFif, im2, cTemp, 0 )
   FreeImageUnload(im2)
   FreeImageUnload(im)

RETURN cTemp


//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_FileExt(cFile)
   LOCAL i
   
   i:=RAt(".",cFile)
   IF i>0
      RETURN SubStr(cFile, i+1)
   ENDIF
   
RETURN ""


//----------------------------------------------------------------------------------------------------//


STATIC FUNCTION TempFileName(cFolder,cPrefix,cExt)

   LOCAL cFile,n:=1

   DEFAULT cFolder:=GetEnv("Temp")
   DEFAULT cPrefix:="Temp"
   DEFAULT cExt:="tmp"

   cFile:=cFolder+"\"+cPrefix+LTrim(Str(n))+"."+cExt
   WHILE File(cFile) .OR. ;
         File(cFolder+"\"+cPrefix+LTrim(Str(n))+".SMT")  .OR. ;
         File(cFolder+"\"+cPrefix+LTrim(Str(n))+".CDX")
      n:=n+1
      cFile:=cFolder+"\"+cPrefix+LTrim(Str(n))+"."+cExt
   ENDDO

RETURN cFile


//----------------------------------------------------------------------------------------------------//


STATIC FUNCTION GetNewAlias()

   LOCAL n:=1

   WHILE Select("A"+LTrim(Str(n,4,0)))>0
      n++
   ENDDO

RETURN "A"+LTrim(Str(n,4,0))


//----------------------------------------------------------------------------------------------------//

// Didn't remove following function as we may need it if used generally for VXH
//STATIC FUNCTION GetExePath()
//
//   LOCAL cPathMask:=hb_argv(0),n:=RAt("\",cPathMask )
//
//   IF n>0
//      cPathMask:=Left( cPathMask, n-1 )
//   ELSE
//      cPathMask:=""
//   ENDIF
//
//RETURN cPathMask


//----------------------------------------------------------------------------------------------------//


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

#ifndef CINTERFACE
  #define CINTERFACE
#endif

#include "hbapi.h"
#include "FreeImage.h"
//#include <FreeImage\FreeImage.h>


static DWORD charset = DEFAULT_CHARSET;


HB_FUNC( PRINTER_GETPAGEHEIGHT )
{
   hb_retni( GetDeviceCaps( ( HDC ) hb_parnl( 1 ), VERTSIZE ) );
}


HB_FUNC( PRINTER_GETPAGEWIDTH )
{
   hb_retni( GetDeviceCaps( ( HDC ) hb_parnl( 1 ), HORZSIZE ) );
}


HB_FUNC( STARTPAGE_PREVIEW )
{

   HDC  tmpDC;
   RECT emfrect;

   SetRect( &emfrect, 0, 0, GetDeviceCaps( ( HDC ) hb_parnl( 1 ), HORZSIZE ) * 100, GetDeviceCaps( ( HDC ) hb_parnl( 1 ), VERTSIZE ) * 100 );

   tmpDC = CreateEnhMetaFile( ( HDC ) hb_parnl( 1 ), hb_parc( 2 ), &emfrect, "" );

   hb_retnl( ( LONG ) tmpDC );

}


HB_FUNC( ENDPAGE_PREVIEW )
{
   DeleteEnhMetaFile( CloseEnhMetaFile( ( HDC ) hb_parnl( 1 ) ) );
}




// Adopted from codes by Dr. Claudio Soto (June 2014)

HB_FUNC ( EMF_DRAWIMAGE_LOWLEVEL )
{

// 1: hDC
// 2: Image File
// 3: Row
// 4: Col
// 5: Height
// 6: Width
// 7: Stretch
// 8: lTransparent
// 9: aTransparentColor

   HDC   hdcPrint  = (HDC)     hb_parnl (1);
   char *  FileName  = ( char * ) hb_parc( 2 );
   HBITMAP hBitmap;
   HRGN hRgn;
   INT nWidth, nHeight;
   POINT Point;
   BITMAP Bmp;
   int r   = hb_parni (3);   // Row
   int c   = hb_parni (4);   // Col
   int odr = hb_parni (5);   // Height
   int odc = hb_parni (6);   // Width
   int dr ;
   int dc ;

   if ( hdcPrint != NULL )
   {
      c  = ( c   * GetDeviceCaps ( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps ( hdcPrint, PHYSICALOFFSETX );
      r  = ( r   * GetDeviceCaps ( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps ( hdcPrint, PHYSICALOFFSETY );
      dc = ( odc * GetDeviceCaps ( hdcPrint, LOGPIXELSX ) / 1000 ); 
      dr = ( odr * GetDeviceCaps ( hdcPrint, LOGPIXELSY ) / 1000 ); 

      hBitmap = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
      if( hBitmap == NULL )
         hBitmap = ( HBITMAP ) LoadImage( NULL, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
      if( hBitmap == NULL )
         return;
      
      GetObject (hBitmap, sizeof(BITMAP), &Bmp);
      nWidth  = Bmp.bmWidth;
      nHeight = Bmp.bmHeight;

      if ( ! hb_parl (7) ) // Scale
      {
         if ( odr * nHeight / nWidth <= odr )
            dr = odc * GetDeviceCaps ( hdcPrint, LOGPIXELSY ) / 1000 * nHeight / nWidth;
         else
            dc = odr * GetDeviceCaps ( hdcPrint, LOGPIXELSX ) / 1000 * nWidth / nHeight;
      }

      GetViewportOrgEx (hdcPrint, &Point);

      hRgn = CreateRectRgn ( c + Point.x,
                             r + Point.y,
                             c + dc + Point.x - 1,
                             r + dr + Point.y - 1);

      SelectClipRgn (hdcPrint, hRgn);

      GetBrushOrgEx (hdcPrint, &Point);
      SetStretchBltMode (hdcPrint, HALFTONE); 
      SetBrushOrgEx (hdcPrint, Point.x, Point.y, NULL);

      HDC memDC = CreateCompatibleDC (hdcPrint);
      SelectObject (memDC, hBitmap);

      BOOL Transparent      = (BOOL) hb_parl (8);
      int  TransparentColor = HB_ISARRAY (9) ? (int) RGB (hb_parnl(9,1), hb_parnl(9,2), hb_parnl(9,3)) : -1;
      COLORREF color_transp;

      if (( Transparent == TRUE ) || ( TransparentColor != -1 ))
      {  if (TransparentColor == -1)
            color_transp = GetPixel (memDC, 0, 0);
         else
            color_transp = (COLORREF) TransparentColor;
         TransparentBlt (hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, color_transp);
      }
      else
         StretchBlt (hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY);

      SelectClipRgn (hdcPrint, NULL);

      DeleteObject (hBitmap);
      DeleteDC (memDC);
   }
}





//HB_FUNC( EMF_DRAWIMAGE_LOWLEVEL )
//{
//   // 1: hDC
//   // 2: Image File
//   // 3: Row
//   // 4: Col
//   // 5: Height
//   // 6: Width
//   // 7: Stretch
//   // 8: Transparent
//
//   HDC     hdcPrint  = ( HDC ) hb_parnl( 1 );
//   char *  FileName  = ( char * ) hb_parc( 2 );
//   BOOL    bBmpImage = TRUE;
//   HBITMAP hBitmap;
//   HRGN    hRgn;
//   HDC     memDC;
//   INT     nWidth, nHeight;
//   POINT   Point;
//   BITMAP  Bmp;
//   int     r   = hb_parni( 3 ); // Row
//   int     c   = hb_parni( 4 ); // Col
//   int     odr = hb_parni( 5 ); // Height
//   int     odc = hb_parni( 6 ); // Width
//   int     dr;
//   int     dc;
//
//   if( hdcPrint != NULL )
//   {
//      c  = ( c * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
//      r  = ( r * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
//      dc = ( odc * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
//      dr = ( odr * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );
//
//      hBitmap = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
//      if( hBitmap == NULL )
//         hBitmap = ( HBITMAP ) LoadImage( NULL, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
//      if( hBitmap == NULL )
//         bBmpImage = FALSE;
////       Not needed here
////      hBitmap = LoadImage( FileName );
////      
//      if( hBitmap == NULL )
//         return;
//
//      GetObject( hBitmap, sizeof( BITMAP ), &Bmp );
//      nWidth  = Bmp.bmWidth;
//      nHeight = Bmp.bmHeight;
//
//      if( ! hb_parl( 7 ) ) // Scale
//      {
//         if( odr * nHeight / nWidth <= odr )
//            dr = odc * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 * nHeight / nWidth;
//         else
//            dc = odr * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 * nWidth / nHeight;
//      }
//
//      GetViewportOrgEx( hdcPrint, &Point );
//
//      hRgn = CreateRectRgn( c + Point.x,
//                            r + Point.y,
//                            c + dc + Point.x - 1,
//                            r + dr + Point.y - 1 );
//
//      SelectClipRgn( hdcPrint, hRgn );
//
//      if( ! bBmpImage )
//      {
//         if( hb_parl( 7 ) )             // Stretch
//            SetStretchBltMode( hdcPrint, COLORONCOLOR );
//         else
//         {
//            GetBrushOrgEx( hdcPrint, &Point );
//            SetStretchBltMode( hdcPrint, HALFTONE );
//            SetBrushOrgEx( hdcPrint, Point.x, Point.y, NULL );
//         }
//      }
//
//      memDC = CreateCompatibleDC( hdcPrint );
//      SelectObject( memDC, hBitmap );
//
//      if( hb_parl( 8 ) && ! bBmpImage ) // Transparent
//         TransparentBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, GetPixel( memDC, 0, 0 ) );
//      else
//         StretchBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY );
//
//      SelectClipRgn( hdcPrint, NULL );
//
//      DeleteObject( hBitmap );
//      DeleteDC( memDC );
//   }
//}



HB_FUNC_STATIC( CREATEFONT )
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  HFONT hFont, hOldFont ;
  char *pszFont = ( char * ) hb_parc(2) ;
  int iHeight = (int) hb_parnl(3) ;
  int iMul = (int) hb_parnl(4) ;
  int iDiv = (int) hb_parnl(5) ;
  int iWidth ;
  int iWeight = (int) hb_parnl(6) ;
  DWORD dwUnderLine = (DWORD) hb_parl(7) ;
  DWORD dwItalic    = (DWORD) hb_parl(8) ;
  DWORD dwStrikeOut = (DWORD) hb_parl(9) ;
  DWORD dwCharSet   = (DWORD) hb_parnl(10) ;
  iWeight = iWeight > 0 ? iWeight : FW_NORMAL ;
  iHeight = -MulDiv(iHeight, GetDeviceCaps(hDC, LOGPIXELSY), 72);
  if (iDiv )
  {
    iWidth = MulDiv(abs(iMul), GetDeviceCaps(hDC,LOGPIXELSX), abs(iDiv)) ;
  }
  else
  {
    iWidth = 0 ; // Use the default font width
  }

  hFont = CreateFont(iHeight, iWidth, 0, 0, iWeight, dwItalic, dwUnderLine, dwStrikeOut,
        dwCharSet, OUT_DEVICE_PRECIS, CLIP_DEFAULT_PRECIS, DRAFT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,  pszFont) ;
  if (hFont)
  {
    Result = TRUE;
    hOldFont = (HFONT) SelectObject(hDC, hFont) ;
    if ( hOldFont )
    {
      DeleteObject(hOldFont) ;
    }
  }
  hb_retl( Result ) ;
}


HB_FUNC_STATIC( GETPRINTERFONTNAME )
{
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    unsigned char cFont[128] ;
    GetTextFace(hDC, 127, (LPTSTR) cFont) ;
    hb_retc( (char*) cFont ) ;
  }
  else
  {
    hb_retc("") ;
  }
}


HB_FUNC_STATIC( FREEIMAGELOAD )
{
   FIBITMAP *dib = FreeImage_Load( (FREE_IMAGE_FORMAT) hb_parni(1), hb_parc(2), hb_parni(3) );
   if ( dib != NULL )
   {
      hb_retptr( dib );
   }
}


HB_FUNC_STATIC( FREEIMAGEGETFILETYPE )
{
   hb_retni( FreeImage_GetFileType( hb_parc(1), 0 ) );
}


HB_FUNC_STATIC( FREEIMAGEUNLOAD )
{
   FreeImage_Unload( (FIBITMAP *) hb_parptr(1) );
}

/* Meta file, hDC, y1, x1, y2, x2 */
HB_FUNC( WINPRINT_PLAYENHMETAFILE )
{
   RECT rect;
   HENHMETAFILE hEmf = GetEnhMetaFile( hb_parc( 1 ) );

   SetRect( &rect,  hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl(5 ), hb_parnl( 6 ) );
   PlayEnhMetaFile( ( HDC ) hb_parnl( 2 ), hEmf, &rect );
   DeleteEnhMetaFile( hEmf );
}

#pragma ENDDUMP
