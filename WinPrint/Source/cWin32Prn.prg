#include "vxh.ch"

#ifndef CRLF
 #define CRLF CHR(13) + CHR(10)
#endif


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


//----------------------------------------------------------------------------------------------------//

CLASS WinPrint

   DATA oPrinter,nPage,lPaperLength,lPreload,lPreview,memDC,lBestQuality
   DATA cPrinter,nCharHeight,nCopies,cFormType,lAskProperties
   DATA nPageWidth,nPageHeight,lLandScape
   DATA nTopMargin,nLeftMargin,nRightMargin,nBottomMargin
   DATA cHexShadowColor,nMaxY,aPrn,cFile,cAlias,nPenWidth,lError,lDuplex
   DATA cPrevFontName,nPrevFontSize,lPrevBold,lPrevItalic,lPrevUnderLine,lPrevStrikeOut,nForeColor,lGreyScale

   METHOD New() CONSTRUCTOR

   METHOD Create()
   METHOD WinPrnSetFont()
   METHOD SetFont()
   METHOD SetFontArray()
   METHOD Say()
   METHOD GetJustifiedString()
   METHOD Line()
   METHOD Box()
   METHOD Shadow()
   METHOD Image()
   METHOD PageBreak()
   METHOD GetTextWidthMM()
   METHOD GetTextHeightMM()
   METHOD mm2x()
   METHOD mm2y()
   METHOD x2mm()
   METHOD y2mm()
   METHOD SetPenWidth()
   METHOD Preview()

   METHOD Close()

ENDCLASS


//----------------------------------------------------------------------------------------------------//


METHOD New(lPreview,lPload) CLASS WinPrint

   DEFAULT lPreview := .F.
   DEFAULT lPload   := .F.

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
   ::lPreload:=lPload
   ::lPreview:=lPreview
   ::nRightMargin:=10
   ::lGreyScale:=.F.
   ::lBestQuality:=.T.
   ::lDuplex:=.F.

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Create() CLASS WinPrint

   IF ::lPreload
      ::nPageWidth:=200
      ::nPageHeight:=300
      ::nCharHeight:=5
      ::nPage:=1
      RETURN .T.
   ENDIF

   IF AScan(GetPrinters(),{|x| Upper(x)==Upper(::cPrinter)})==0
//   IF AScan(GetPrinters(),{|x| Upper(x[1])==Upper(::cPrinter)})==0
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
   Alert(str(::nPageHeight)+","+str(::nPageWidth))
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

   ::oPrinter:SetDuplexType(1) // Made it simplex

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview .AND. ::lDuplex
      ::oPrinter:setDuplexType(3)
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

   IF Val(::cFormType)>0 //!=DMPAPER_CUSTOM
      ::nPageWidth:=Int(::oPrinter:PageWidth/::oPrinter:PixelsPerInchX*25.4)
      ::nPageHeight:=Int(::oPrinter:PageHeight/::oPrinter:PixelsPerInchY*25.4)
   ENDIF

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

   IF !::lPaperLength .OR. ::nCopies>1 .OR. ::lPreview
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
     ::aPrn:={}
   ENDIF

RETURN .T.


//----------------------------------------------------------------------------------------------------//


METHOD SetPenWidth(n) CLASS WinPrint

   IF ::lPreload
      RETURN Self
   ENDIF

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
      ::oPrinter:SetPen(::oPrinter:PenStyle,INT(n*::oPrinter:PixelsPerInchY/25.4),::oPrinter:PenColor)
   ELSE
      IF ::lPreview
         AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,n,::oPrinter:PenColor})
      ELSE
         AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,INT(n*::oPrinter:PixelsPerInchY/25.4),::oPrinter:PenColor})
      ENDIF
      IF ::lPreview // For PDF output
         AAdd(::aPrn,{"SETLINEWIDTH",n})
      ENDIF
   ENDIF

   ::nPenWidth:=n

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD SetFontArray(aFont) CLASS WinPrint

   IF ::lPreload
      RETURN Self
   ENDIF

   ::SetFont(aFont[1],aFont[2],aFont[3],aFont[4],aFont[5],aFont[6],aFont[7])

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD SetFont(cFontName,nFontSize,lBold,lItalic,lUnderLine,lStrikeOut,nForeColor) CLASS WinPrint

   (lStrikeOut)

   IF ::lPreload
      RETURN Self
   ENDIF

   IF ValType(nForeColor)="C"
      nForeColor:=Val(nForeColor)
   ENDIF

   IF ::cPrevFontName==cFontName .AND. ::nPrevFontSize==nFontSize .AND. ::lPrevBold==lBold .AND. ::lPrevItalic==lItalic .AND. ;
      ::lPrevUnderLine==lUnderLine .AND. ::lPrevStrikeOut==lStrikeOut .AND. ::nForeColor==nForeColor
      RETURN SELF
   ENDIF

   ::cPrevFontName:=cFontName
   ::nPrevFontSize:=nFontSize
   ::lPrevBold:=lBold
   ::lPrevItalic:=lItalic
   ::lPrevUnderLine:=lUnderLine

   ::lPrevStrikeOut:=lStrikeOut
   ::nForeColor:=nForeColor

   ::oPrinter:SetColor(IF(::lGreyScale,0,nForeColor))
   ::WinPrnSetFont(cFontName, ;
      nFontSize, ;
      0, ;
      IF(lBold, 700, 100), ;
      lUnderLine, ;
      lItalic, ;
      lStrikeOut, ;
      IF(Upper(cFontName)$"WINGDINGSWEBDINGS",2,0))

   IF !::lPaperLength .OR. ::nCopies>1 .OR. ::lPreview
      AAdd(::aPrn,{"SETCOLOR",IF(::lGreyScale,0,nForeColor)})
      AAdd(::aPrn,{"SETFONT", cFontName, ;
         nFontSize, ;
         0, ;
         IF(lBold, 700, 100), ;
         lUnderLine, ;
         lItalic, ;
         lStrikeOut, ;
         IF(Upper(cFontName)$"WINGDINGSWEBDINGS",2,0)})
   ENDIF
   ::nCharHeight:=Ceiling(::oPrinter:GetCharHeight()/::oPrinter:PixelsPerInchY*25.4)
RETURN Self


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

//  IF cFontName !=NIL
//    ::oPrinter:FontName:= cFontName
//  ENDIF
//  IF nPointSize!=NIL
//    ::oPrinter:FontPointSize:= nPointSize
//  ENDIF
//  IF nWidth != NIL
//    cType:= VALTYPE(nWidth)
//    IF cType='A'
//      ::oPrinter:FontWidth     := nWidth
//    ELSEIF cType='N' .AND. !EMPTY(nWidth)
//      ::oPrinter:FontWidth     := {1,nWidth }
//    ELSE
//      ::oPrinter:FontWidth     := {0, 0 }
//    ENDIF
//  ENDIF
//  IF nBold != NIL
//    ::oPrinter:fBold := nBold
//  ENDIF
//  IF lUnderLine != NIL
//    ::oPrinter:fUnderline:= lUnderLine
//  ENDIF
//  IF lItalic != NIL
//    ::oPrinter:oPrinter:fItalic := lItalic
//  ENDIF
//  IF nCharSet != NIL
//    ::oPrinter:fCharSet := nCharSet
//  ENDIF

  IF (lSetFontOk:= CreateFont( ::oPrinter:hPrinterDC, cFontName, nPointSize, aFontWidth[1], aFontWidth[2], ;
        nBold, lUnderLine, lItalic, lStrikeOut, nCharSet))
//    ::oPrinter:fCharWidth        := ::GetCharWidth()
//    ::oPrinter:CharWidth:= ABS(::fCharWidth)
//    ::oPrinter:CharHeight:= ::GetCharHeight()
  ENDIF
  ::oPrinter:FontName:= GetPrinterFontName(::oPrinter:hPrinterDC)  // Get the font name that Windows actually used
RETURN(lSetFontOk)



//----------------------------------------------------------------------------------------------------//


METHOD Say(nLeft,nTop,cString,nAlign,nWidth,nHeight,lWrap) CLASS WinPrint

   LOCAL i,j,aString,n,cTmp,lTest:=.F.,lJustified, nNext,nTmp,lFnt

   DEFAULT lWrap := .T.

   IF ::lPreload
      RETURN Self
   ENDIF

   cString:=StrTran(cString,"~",CRLF)

   lJustified:=(nAlign==4)

   IF nAlign=NIL .OR. nAlign>3
      nAlign:=1
   ENDIF

   IF nAlign=1     // Left
      nAlign:=0
      cString:=LTrim(cString)
   ELSEIF nAlign=2 // Center
      nAlign:=6
      cString:=AllTrim(cString)
   ELSEIF nAlign=3 // Right
      nAlign:=2
      cString:=AllTrim(cString)
   ENDIF

   IF nWidth=NIL
      IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
         ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
      ELSE
         IF ::lPreview
            AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
         ELSE
            AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
         ENDIF
         ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
      ENDIF
   ELSE
      nWidth:=Int(( nWidth * ::oPrinter:PixelsPerInchX ) / 25.4)

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
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<b>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,.T.,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</b>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,.F.,::lPrevItalic,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<i>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,.T.,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</i>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,.F.,::lPrevUnderLine,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=3 .AND.*/ Lower(SubStr(cString,i,3))="<u>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,::lPrevItalic,.T.,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=3
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF
               ELSEIF /*Len(cString)-i-1>=4 .AND.*/ Lower(SubStr(cString,i,4))="</u>"
                  nLeft+=IF(cTmp=="",0,::oPrinter:GetTextWidth(cTmp))
                  ::SetFont(::cPrevFontName,::nPrevFontSize,::lPrevBold,::lPrevItalic,.F.,::lPrevStrikeOut,::nForeColor)
                  cTmp:=""
                  i+=4
                  IF !(::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview)
                     ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
                  ENDIF

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

                  IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
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
            IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
               ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
            ELSE
               IF ::lPreview
                  AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
               ELSE
                  AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
               ENDIF
              ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
            ENDIF
         ENDIF
      ELSEIF !lWrap
         WHILE ::oPrinter:GetTextWidth(cString)>nWidth .AND. Len(cString)!=0
            cString:=Left(cString,Len(cString)-1)
         ENDDO
         IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
            ::oPrinter:TextOutAt(::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign)
         ELSE
            IF ::lPreview
               AAdd(::aPrn, {"TEXTOUTAT",nLeft,nTop,cString,.F.,.F.,nAlign})
            ELSE
               AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),::mm2y(nTop),cString,.F.,.F.,nAlign})
            ENDIF
           ::nMaxY:=Max(::nMaxY,::mm2y(nTop)+::nCharHeight)
         ENDIF
      ELSE
         aString:=hb_ATokens(cString,CRLF)
         FOR i:=1 TO Len(aString)
            aString[i]:=aString[i]+"~"
         NEXT
         IF nHeight!=NIL
            nHeight:=::mm2y(nHeight)
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
            WHILE n>1
               cTmp:=SubStr(aString[i],n,1)
               IF IsSpace(cTmp) .OR. cTmp$".,?-;"
                  cTmp:=SubStr(aString[i],n+1)
                  aString[i]:=Left(aString[i],n)
                  IF Len(aString)>i
                     AIns(aString,i+1,cTmp,.T.)
                  ELSE
                     AAdd(aString,cTmp)
                  ENDIF
                  EXIT
               ENDIF
               n--
            ENDDO

            IF n<2
               n:=j-1
               cTmp:=SubStr(aString[i],n+1)
               aString[i]:=Left(aString[i],n)
               IF Len(aString)>i
                  AIns(aString,i+1,cTmp,.T.)
               ELSE
                  AAdd(aString,cTmp)
               ENDIF

               IF ::oPrinter:GetTextWidth(cTmp)>=nWidth
                  i++
                  LOOP
               ENDIF

//               cTmp:=SubStr(aString[i],n,1)
//               IF IsSpace(cTmp) .OR. cTmp$".,?-;"
//                  cTmp:=SubStr(aString[i],n+1)
//                  aString[i]:=Left(aString[i],n)
//                  AIns(aString,i+1,cTmp,.T.)
//                  LOOP
//               ENDIF

            ENDIF
         NEXT

         n:=0

         nTop:=::mm2y(nTop)
         FOR i=1 TO Len(aString)
            IF i>1
               n+=IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
            ENDIF
            IF Right(aString[i],1)=="~"
               aString[i]:=Left(aString[i],Len(aString[i])-1)
            ELSEIF lJustified .AND. i<Len(aString)
               aString[i]:=::GetJustifiedString(aString[i],nWidth)
            ENDIF
            IF nHeight=NIL .OR. nHeight>=n-IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
               IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
                  ::oPrinter:TextOutAt(::mm2x(nLeft),nTop+n,aString[i],.F.,.F.,nAlign)
               ELSE
                  IF ::lPreview
                     AAdd(::aPrn, {"TEXTOUTAT",nLeft,::y2mm(nTop+n),aString[i],.F.,.F.,nAlign})
                  ELSE
                     AAdd(::aPrn, {"TEXTOUTAT",::mm2x(nLeft),nTop+n,aString[i],.F.,.F.,nAlign})
                  ENDIF
                  ::nMaxY:=Max(::nMaxY,nTop+n+::nCharHeight)
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD GetJustifiedString(cString,nWidth)

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

   IF ::lPreload
      RETURN Self
   ENDIF

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
      ::oPrinter:EndPage(.F.)
      IF ::lDuplex
         ::oPrinter:setDuplexType(3)
      ENDIF
      ::oPrinter:StartPage()
      ::oPrinter:SetBkMode(1)
      ::oPrinter:SetPrc(0,0)
   ELSE
      (::cAlias)->(DbAppend())
      (::cAlias)->PAGENO:=::nPage
      (::cAlias)->PAGEDATA:=::aPrn
      ::aPrn:={}
   ENDIF

   ::nPage++

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Line(t,l,b,r,w,c) CLASS WinPrint

   LOCAL nPrevColor,nPrevWidth

   IF ::lPreload
      RETURN Self
   ENDIF

   IF w=NIL .OR. c=NIL
      IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
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

      nPrevWidth:=::oPrinter:PenWidth
      nPrevColor:=::oPrinter:PenColor

      IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
         ::oPrinter:SetPen(::oPrinter:PenStyle,INT(w*::oPrinter:PixelsPerInchY/25.4),IF(::lGreyScale,0,HexToNum(c)))
         ::oPrinter:Line(::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b))
         ::oPrinter:SetPen(::oPrinter:PenStyle,nPrevWidth,nPrevColor)
      ELSE
         IF ::lPreview
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,w,HexToNum(c)})
            AAdd(::aPrn,{"LINE", l, t, r, b})
         ELSE
            AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,INT(w*::oPrinter:PixelsPerInchY/25.4),IF(::lGreyScale,0,HexToNum(c))})
            AAdd(::aPrn,{"LINE", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
         ENDIF
//         IF ::lPreview // For PDF output
//            AAdd(::aPrn,{"SETLINEWIDTH",w})
//         ENDIF

//         AAdd(::aPrn,{"SETPEN",::oPrinter:PenStyle,IF(nPrevWidth=NIL,1,nPrevWidth),nPrevColor})
//         IF ::lPreview // For PDF output
//            AAdd(::aPrn,{"SETLINEWIDTH",IF(nPrevWidth=NIL,1,nPrevWidth)*25.4/::oPrinter:PixelsPerInchY})
//         ENDIF
         ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
      ENDIF
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Image(cImage,t,l,b,r) CLASS WinPrint

   LOCAL oBmp

   IF ::lPreload
      RETURN Self
   ENDIF

   IF !File(cImage)
      MsgAlert("Cannot find file : "+cImage)
      RETURN Self
   ENDIF
   
   oBMP:= Win32BMP():new()
   cImage:=If(Upper(Right(cImage,3))=="BMP" .AND. !::lGreyScale,cImage,WinPrint_ToBmp(cImage,::lGreyScale))
   IF !::lPreview
      l:=::mm2x(l)
      t:=::mm2y(t)
      r:= Round(r * ::oPrinter:PixelsPerInchX / 25.4, 0)
      b:= Round(b * ::oPrinter:PixelsPerInchY / 25.4, 0)
   ENDIF
   IF oBmp:loadFile( cImage )
      IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
         oBmp:Draw( ::oPrinter,  {l, t, r, b} )
      ELSE
         AAdd(::aPrn, {"DRAW", cImage, l, t, r, b})
         ::nMaxY:=Max(::nMaxY, Max(t,b)+::nCharHeight)
      ENDIF
   ENDIF
   oBMP:Destroy()

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Box(t,l,b,r) CLASS WinPrint

   IF ::lPreload
      RETURN Self
   ENDIF

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
      ::oPrinter:Box( ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) )
   ELSE
      IF ::lPreview
         AAdd(::aPrn, {"BOX", l, t, r, b})
      ELSE
         AAdd(::aPrn, {"BOX",::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
      ENDIF
      ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
      IF ::lPreview // Will be used for PDF!
         AAdd(::aPrn, {"BOXPDF", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b)})
      ENDIF
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD Shadow(t,l,b,r,cHexShadowColor,nBoxType) CLASS WinPrint

   IF ::lPreload
      RETURN Self
   ENDIF

   IF cHexShadowColor=NIL
      cHexShadowColor:=::cHexShadowColor
   ENDIF

   IF Upper(cHexShadowColor)=="FFFFFF"
      RETURN Self
   ENDIF

   IF ::lGreyScale
      cHexShadowColor:="DCDCDC"
   ENDIF

   DEFAULT nBoxType := 1

   IF ::lPaperLength .AND. ::nCopies=1 .AND. !::lPreview
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
      IF ::lPreview // Will be used for PDF!
         AAdd(::aPrn, {"FILLRECTPDF", ::mm2x(l), ::mm2y(t), ::mm2x(r), ::mm2y(b) , cHexShadowColor, nBoxType})
      ENDIF
      ::nMaxY:=Max(::nMaxY, Max(::mm2y(t),::mm2y(b))+::nCharHeight)
   ENDIF

RETURN Self


//----------------------------------------------------------------------------------------------------//


METHOD GetTextWidthMM(cString) CLASS WinPrint

   IF ::lPreload
      RETURN 1
   ENDIF

RETURN Ceiling(::oPrinter:GetTextWidth(cString)/::oPrinter:PixelsPerInchX*25.4)


//----------------------------------------------------------------------------------------------------//


METHOD GetTextHeightMM(cString,nWidth)

   LOCAL i,j,aString,n,cTmp

   IF ::lPreload
      RETURN 1
   ENDIF

   nWidth:=::mm2x(nWidth)

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
      WHILE n>1
         cTmp:=SubStr(aString[i],n,1)
         IF IsSpace(cTmp) .OR. cTmp$".,?-;"
            cTmp:=SubStr(aString[i],n+1)
            aString[i]:=Left(aString[i],n)
            IF Len(aString)>i
               AIns(aString,i+1,cTmp,.T.)
            ELSE
               AAdd(aString,cTmp)
            ENDIF
            EXIT
         ENDIF
         n--
      ENDDO

      IF n<2
         n:=j-1
         cTmp:=SubStr(aString[i],n+1)
         aString[i]:=Left(aString[i],n)
         IF Len(aString)>i
            AIns(aString,i+1,cTmp,.T.)
         ELSE
            AAdd(aString,cTmp)
         ENDIF
         IF ::oPrinter:GetTextWidth(cTmp)>=nWidth
            i++
            LOOP
         ENDIF

//         cTmp:=SubStr(aString[i],n,1)
//         IF IsSpace(cTmp) .OR. cTmp$".,?-;"
//            cTmp:=SubStr(aString[i],n+1)
//            aString[i]:=Left(aString[i],n)
//            AIns(aString,i+1,cTmp,.T.)
//            LOOP
//         ENDIF

      ENDIF
   NEXT

   n:=0
   FOR i=1 TO Len(aString)
      n+=IF(Empty(aString[i]),::oPrinter:GetTextHeight("A"),::oPrinter:GetTextHeight(aString[i]))
   NEXT

RETURN Ceiling(n/::oPrinter:PixelsPerInchY*25.4)


//----------------------------------------------------------------------------------------------------//


METHOD Preview(oParentForm) CLASS WinPrint

   LOCAL cAlias2,cFile2,lUse:=.T.,hDCBak,cEmfName,aEmfName,cEmfPrefix,hDC,j,nMaxY,nPrevColor,aPrevFont

   IF ::lPreload
      RETURN Self
   ENDIF

   IF Len(::aPrn)>0
     (::cAlias)->(DbAppend())
     (::cAlias)->PAGENO:=::nPage
     (::cAlias)->PAGEDATA:=::aPrn
     ::aPrn:={}
   ENDIF
   
   nPrevColor:=-1
   aPrevFont:={}

   nMaxY:=Int(::nMaxY/::oPrinter:PixelsPerInchY*25.4)+::nTopMargin
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

   hDC:=::oPrinter:hPrinterDC
   hDCBak:=hDC
   aEmfName:={}
   cEmfPrefix:="Preview_"+DtoS(Date())+"_"+NToC(Seconds())

   (::cAlias)->(DbGoTop())
   WHILE !(::cAlias)->(Eof())
      cEmfName:=TempFileName(,cEmfPrefix,"EMF")
      hDC := StartPage_Preview(hDCBak,cEmfName)
      ::oPrinter:hPrinterDC:=hDC


      ::aPrn:=(::cAlias)->PAGEDATA
      ::oPrinter:SetBkMode(1)
      ::oPrinter:SetPrc(0,0)

      IF nPrevColor!=-1
         ::oPrinter:SetColor(nPrevColor)
      ENDIF
      
      IF Len(aPrevFont)!=0
         ::WinPrnSetFont(aPrevfont[1],aPrevFont[2],aPrevFont[3],aPrevFont[4],aPrevFont[5],aPrevFont[6],aPrevFont[7],aPrevFont[7],aPrevFont[8])
      ENDIF

      FOR j=1 TO Len(::aPrn)
         IF ::aPrn[j][1]=="SETCOLOR"
            ::oPrinter:SetColor(::aPrn[j][2])
            nPrevColor:=::aPrn[j][2]
         ELSEIF ::aPrn[j][1]=="SETFONT"
            ::WinPrnSetFont(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9])
            aPrevFont:={::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9]}
         ELSEIF ::aPrn[j][1]=="TEXTOUTAT"
            ::oPrinter:TextOutAt(::mm2x(::aPrn[j][2]),::mm2Y(::aPrn[j][3]),::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7])
         ELSEIF ::aPrn[j][1]=="LINE"
            ::oPrinter:Line(::mm2x(::aPrn[j][2]),::mm2y(::aPrn[j][3]),::mm2x(::aPrn[j][4]),::mm2y(::aPrn[j][5]))
         ELSEIF ::aPrn[j][1]=="DRAW"
            //----- All parameters in mm (hDC,cImageFile,nRow,nCol,nWidth,nHeight,lStretch,lTransparent)
            Emf_DrawImage(hDC,::aPrn[j][2],::aPrn[j][4],::aPrn[j][3],::aPrn[j][6],::aPrn[j][5],.T.,.F.)
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
            ::oPrinter:SetPen(::aPrn[j][2],Int(::aPrn[j][3]*::oPrinter:PixelsPerInchY/25.4),::aPrn[j][4])
         ENDIF
      NEXT j

      EndPage_Preview(hDC)

      AAdd(aEmfName,cEmfName)

      (::cAlias)->(DbSkip())
   ENDDO

   ::oPrinter:hPrinterDC:=hDCBak


   IF Select(::cAlias)>0
      (::cAlias)->(DbCloseArea())
   ENDIF

   // Create temporary for printer data
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
      RETURN Self
   ENDIF

   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="PRINTER"    ; (cAlias2)->PRNVAL:=::cPrinter
   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="LANDSCAPE"  ; (cAlias2)->PRNVAL:=::lLandScape
   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="FORMTYPE"   ; (cAlias2)->PRNVAL:=::oPrinter:FormType
   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="PAPERWIDTH" ; (cAlias2)->PRNVAL:=::oPrinter:PaperWidth
   IF ::lPaperLength
      (cAlias2)->(DbAppend()); (cAlias2)->PRNPROP:="PAPERLENGTH"; (cAlias2)->PRNVAL:=::oPrinter:PaperLength
   ELSE
      (cAlias2)->(DbAppend()); (cAlias2)->PRNPROP:="PAPERLENGTH"; (cAlias2)->PRNVAL:=Int((::nMaxY/::oPrinter:PixelsPerInchY*25.4)+::nTopMargin)*10
   ENDIF

   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="RIGHTMARGIN" ; (cAlias2)->PRNVAL:=::nRightMargin

   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="EMFNAMES" ; (cAlias2)->PRNVAL:=aEmfName

   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="GREYSCALE" ; (cAlias2)->PRNVAL:=::lGreyScale
   
   (cAlias2)->(DbAppend());    (cAlias2)->PRNPROP:="COPY" ; (cAlias2)->PRNVAL:=::nCopies

   (cAlias2)->(DbCloseArea())

   TRY
    ::oPrinter:Destroy()
   CATCH
   END

   PrintPreview_Form1(oParentForm,{cFile2})
   
RETURN Self



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


METHOD Close() CLASS WinPrint

   LOCAL j,oBmp,nMaxY,nCopyCtr

   IF ::lError
      TRY
        TRY
           (::cAlias)->(DbCloseArea())
        CATCH
        END
        ::oPrinter:SetDuplexType(1)
        ::oPrinter:EndDoc(.T.)
        ::oPrinter:Destroy()
      CATCH
      END
      RETURN Self
   ENDIF

   IF ::lPreload
      RETURN Self
   ENDIF

   IF !::lPaperLength .OR. ::nCopies>1

      nMaxY:=Int(::nMaxY/::oPrinter:PixelsPerInchY*25.4)+::nTopMargin
      ::oPrinter:SetDuplexType(1)
      ::oPrinter:EndDoc(.T.)
      TRY
       ::oPrinter:Destroy()
      CATCH
      END

      IF !(::cAlias)->(Used())
         TRY
           ::oPrinter:SetDuplexType(1)
           ::oPrinter:EndDoc(.T.)
           ::oPrinter:Destroy()
         CATCH
         END
         RETURN Self
      ENDIF

      IF Len(::aPrn)>0
        (::cAlias)->(DbAppend())
        (::cAlias)->PAGENO:=::nPage
        (::cAlias)->PAGEDATA:=::aPrn
        ::aPrn:={}
      ENDIF

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
     ::oPrinter:SetDuplexType(1) // Made it simplex

     IF ::lDuplex
        ::oPrinter:setDuplexType(3)
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
                 ::WinPrnSetFont(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7],::aPrn[j][8],::aPrn[j][9])
              ELSEIF ::aPrn[j][1]=="TEXTOUTAT"
                 ::oPrinter:TextOutAt(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6],::aPrn[j][7])
              ELSEIF ::aPrn[j][1]=="LINE"
                 ::oPrinter:Line(::aPrn[j][2],::aPrn[j][3],::aPrn[j][4],::aPrn[j][5])
              ELSEIF ::aPrn[j][1]=="DRAW"
                 oBMP:= Win32BMP():new()
                 IF oBmp:loadFile( ::aPrn[j][2] )
                    oBmp:Draw( ::oPrinter,  { ::aPrn[j][3],::aPrn[j][4],::aPrn[j][5],::aPrn[j][6] } )
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
              IF ::lDuplex
                 ::oPrinter:setDuplexType(3)
              ENDIF
              ::oPrinter:StartPage()
              ::oPrinter:SetBkMode(1)
              ::oPrinter:SetPrc(0,0)
           ENDIF

           (::cAlias)->(DbSkip(1))

        ENDDO

        IF nCopyCtr<::nCopies
           ::oPrinter:EndPage(.F.)
           IF ::lDuplex
              ::oPrinter:setDuplexType(3)
           ENDIF
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


METHOD mm2x(nMm) CLASS WinPrint

   IF ::lPreload
      RETURN 1
   ENDIF

RETURN ::oPrinter:MM_TO_POSX(nMM)


//----------------------------------------------------------------------------------------------------//


METHOD mm2y(nMm)  CLASS WinPrint

   IF ::lPreload
      RETURN 1
   ENDIF

RETURN ::oPrinter:MM_TO_POSY(nMM)+::oPrinter:TopMargin


//----------------------------------------------------------------------------------------------------//


METHOD X2mm(n) CLASS WinPrint
RETURN n*25.4/::oPrinter:PixelsPerInchX
//RETURN (n+::oPrinter:LeftMargin)*25.4/::oPrinter:PixelsPerInchX


//----------------------------------------------------------------------------------------------------//


METHOD Y2mm(n) CLASS WinPrint
RETURN n*25.4/::oPrinter:PixelsPerInchY
//RETURN (n+::oPrinter:TopMargin)*25.4/::oPrinter:PixelsPerInchY



//----------------------------------------------------------------------------------------------------//


FUNCTION WinPrint_ToBmp(cFile,lGreyScale)

  LOCAL im, im2, nFif, cTemp

  DEFAULT lGreyScale := .F.

//  DO CASE
//     CASE Upper(Right(cFile, 4)) == ".JPG" .OR. upper(Right(cFile, 5)) == ".JPEG"
//        nFif := FIF_JPEG
//     CASE Upper(Right(cFile, 4)) == ".GIF"
//        nFif := FIF_GIF
//     CASE Upper(Right(cFile, 4)) == ".ICO"
//        nFif := FIF_ICO
//     CASE Upper(Right(cFile, 4)) == ".PNG"
//        nFif := FIF_PNG
//     CASE Upper(Right(cFile, 4)) == ".BMP" .AND. lGreyScale
//        nFif := FIF_BMP
//     OTHERWISE
//        RETURN ""
//  ENDCASE

  nFif := FreeImageGetFileType(cFile)
  IF nFif==FIF_BMP .AND. !lGreyScale
     RETURN cFile
  ENDIF

  im := FreeImageLoad(nFif,cFile,0)
  //im := fi_Load( nFif, cFile, BMP_DEFAULT )
  IF lGreyScale
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
  //fi_Unload( im )

RETURN cTemp


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






HB_FUNC( EMF_DRAWIMAGE_LOWLEVEL )
{
   // 1: hDC
   // 2: Image File
   // 3: Row
   // 4: Col
   // 5: Height
   // 6: Width
   // 7: Stretch
   // 8: Transparent

   HDC     hdcPrint  = ( HDC ) hb_parnl( 1 );
   char *  FileName  = ( char * ) hb_parc( 2 );
   BOOL    bBmpImage = TRUE;
   HBITMAP hBitmap;
   HRGN    hRgn;
   HDC     memDC;
   INT     nWidth, nHeight;
   POINT   Point;
   BITMAP  Bmp;
   int     r   = hb_parni( 3 ); // Row
   int     c   = hb_parni( 4 ); // Col
   int     odr = hb_parni( 5 ); // Height
   int     odc = hb_parni( 6 ); // Width
   int     dr;
   int     dc;

   if( hdcPrint != NULL )
   {
      c  = ( c * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      r  = ( r * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      dc = ( odc * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
      dr = ( odr * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );

      hBitmap = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
      if( hBitmap == NULL )
         hBitmap = ( HBITMAP ) LoadImage( NULL, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
      if( hBitmap == NULL )
         bBmpImage = FALSE;
      /* Not needed here
      hBitmap = LoadImage( FileName );
      */
      if( hBitmap == NULL )
         return;

      GetObject( hBitmap, sizeof( BITMAP ), &Bmp );
      nWidth  = Bmp.bmWidth;
      nHeight = Bmp.bmHeight;

      if( ! hb_parl( 7 ) ) // Scale
      {
         if( odr * nHeight / nWidth <= odr )
            dr = odc * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 * nHeight / nWidth;
         else
            dc = odr * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 * nWidth / nHeight;
      }

      GetViewportOrgEx( hdcPrint, &Point );

      hRgn = CreateRectRgn( c + Point.x,
                            r + Point.y,
                            c + dc + Point.x - 1,
                            r + dr + Point.y - 1 );

      SelectClipRgn( hdcPrint, hRgn );

      if( ! bBmpImage )
      {
         if( hb_parl( 7 ) )             // Stretch
            SetStretchBltMode( hdcPrint, COLORONCOLOR );
         else
         {
            GetBrushOrgEx( hdcPrint, &Point );
            SetStretchBltMode( hdcPrint, HALFTONE );
            SetBrushOrgEx( hdcPrint, Point.x, Point.y, NULL );
         }
      }

      memDC = CreateCompatibleDC( hdcPrint );
      SelectObject( memDC, hBitmap );

      if( hb_parl( 8 ) && ! bBmpImage ) // Transparent
         TransparentBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, GetPixel( memDC, 0, 0 ) );
      else
         StretchBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY );

      SelectClipRgn( hdcPrint, NULL );

      DeleteObject( hBitmap );
      DeleteDC( memDC );
   }
}


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


#pragma ENDDUMP
