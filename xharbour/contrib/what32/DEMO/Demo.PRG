static oStat,hToolBar,oProg

static isOpen:=.f.
static isSelected:=.f.


static title    :='Graph Demo'
static subtitle :='SubTitle Text'
static smalltxt :='Small print text'
static yaxis    :='Y Axis Text'
static labelsin :='<none>'
static aseries  :={}
static dbffile  :=''

static type   :=1
static steps  :=5
static stretch:=.f.
static Use3D  :=.f.
static trend  :=.f.
static ref    :=0
static UseRef :=.f.
static stacked:=.f.
static cluster:=.f.
static maxrec :=10
static tthick :=3
static rthick :=3
static tcolor
static rcolor
static tbrush
static rbrush
static lxLabels,lyLabels,lLegends,lCage,lxGrid,lyGrid,lTop3,lData,lReversed,lOutLine


#DEFINE SHOWDEBUG
#include "hbclass.ch"
#Include "windows.ch"
#include "wintypes.ch"
#Include "cstruct.ch"
#Include "commdlg.ch"
#Include "WinLView.ch"
#Include "debug.ch"
#Include "what32.ch"
#Include "toolbar.ch"
#Include "accel.ch"
#INCLUDE "WinGdi.ch"

#define C_RED         RGB(255,0,0)
#define C_DARKRED     RGB(128, 0, 0)
#define C_PINK        RGB(255, 0, 128)
#define C_BLUE        RGB(0,0,255)
#define C_DARKBLUE    RGB(0, 0, 128)
#define C_GREEN       RGB(0,255,0)
#define C_DARKGREEN   RGB(0,128,0)
#define C_MAGENTA     RGB(255,0,255)
#define C_DARKMAGENTA RGB(128,0,128)
#define C_BLACK       RGB(0,0,0)
#define C_WHITE       RGB(255,255,255)
#define C_GREY        RGB(192, 192, 192)
#define C_DARKGREY    RGB(128, 128, 128)
#define C_GREYDARK    RGB(100, 100, 100)
#define C_YELLOW      RGB(255, 255, 0)
#define C_LIGHTYELLOW RGB(255, 255, 185)
#define C_OCKER       RGB(128, 128, 0)
#define C_CYAN        RGB(0, 128, 128)
#define C_LIGHTCYAN   RGB(0, 255, 255)
#define C_PURPLE      RGB(128, 0, 128)

#define CLR_NONE                0xFFFFFFFFL

#define ILD_NORMAL              0x0000
#define ILD_TRANSPARENT         0x0001

#define SND_SYNC            0x0000  /* play synchronously (default) */
#define SND_ASYNC           0x0001  /* play asynchronously */
#define SND_NODEFAULT       0x0002  /* silence (!default) if sound not found */
#define SND_MEMORY          0x0004  /* pszSound points to a memory file */
#define SND_LOOP            0x0008  /* loop the sound until next sndPlaySound */
#define SND_NOSTOP          0x0010  /* don't stop any currently playing sound */

pragma pack(4)
typedef struct { ;
    UINT    style; 
    WNDPROC lpfnWndProc; 
    int     cbClsExtra; 
    int     cbWndExtra; 
    HANDLE  hInstance; 
    HICON   hIcon; 
    HCURSOR hCursor; 
    HBRUSH  hbrBackground; 
    LPCTSTR lpszMenuName; 
    LPCTSTR lpszClassName; 
} WNDCLASS

typedef struct {;
    UINT    mask; 
    int     cxy; 
    LPTSTR  pszText; 
    HBITMAP hbm; 
    int     cchTextMax; 
    int     fmt; 
    LPARAM  lParam; 
    int     iImage;
    int     iOrder;
} HDITEM, FAR * LPHDITEM

typedef struct { ;
    RECT    *prc;  
    WINDOWPOS *pwpos; 
} HDLAYOUT

typedef struct { ;// si
    UINT cbSize; 
    UINT fMask; 
    int  nMin; 
    int  nMax; 
    UINT nPage; 
    int  nPos; 
    int  nTrackPos; 
}   SCROLLINFO


typedef struct tagDEVNAMES {; // dvnm
    WORD wDriverOffset; 
    WORD wDeviceOffset; 
    WORD wOutputOffset; 
    WORD wDefault;     // driver, device, and port name strings follow wDefault
} DEVNAMES


typedef struct tagPOINT {;
    LONG x;
    LONG y;
} POINT

/* RECT */

typedef struct _RECT { ;
    LONG left; 
    LONG top; 
    LONG right; 
    LONG bottom; 
} RECT 



typedef struct tagPSD {;  // psd
    DWORD           lStructSize; 
    HWND            hwndOwner; 
    HGLOBAL         hDevMode; 
    HGLOBAL         hDevNames; 
    DWORD           Flags; 
    POINT           ptPaperSize; 
    RECT            rtMinMargin; 
    RECT            rtMargin; 
    HINSTANCE       hInstance; 
    LPARAM          lCustData; 
    LPPAGESETUPHOOK lpfnPageSetupHook; 
    LPPAGEPAINTHOOK lpfnPagePaintHook; 
    LPCTSTR         lpPageSetupTemplateName; 
    HGLOBAL         hPageSetupTemplate; 
} PAGESETUPDLG, * LPPAGESETUPDLG



typedef struct tagPD {;  // pd
    DWORD     lStructSize; 
    HWND      hwndOwner; 
    HANDLE    hDevMode; 
    HANDLE    hDevNames; 
    HDC       hDC; 
    DWORD     Flags; 
    WORD      nFromPage; 
    WORD      nToPage; 
    WORD      nMinPage; 
    WORD      nMaxPage; 
    WORD      nCopies; 
    HINSTANCE hInstance; 
    DWORD     lCustData; 
    LPPRINTHOOKPROC lpfnPrintHook; 
    LPSETUPHOOKPROC lpfnSetupHook; 
    LPCTSTR    lpPrintTemplateName; 
    LPCTSTR    lpSetupTemplateName; 
    HANDLE    hPrintTemplate; 
    HANDLE    hSetupTemplate; 
} PRINTDLG

typedef struct tagBITMAP {;  // bm
   LONG   bmType; 
   LONG   bmWidth; 
   LONG   bmHeight; 
   LONG   bmWidthBytes; 
   WORD   bmPlanes; 
   WORD   bmBitsPixel; 
   LPVOID bmBits; 
} BITMAP

typedef struct tagLOGFONT { ;
   LONG lfHeight; 
   LONG lfWidth; 
   LONG lfEscapement; 
   LONG lfOrientation; 
   LONG lfWeight; 
   BYTE lfItalic; 
   BYTE lfUnderline; 
   BYTE lfStrikeOut; 
   BYTE lfCharSet; 
   BYTE lfOutPrecision; 
   BYTE lfClipPrecision; 
   BYTE lfQuality; 
   BYTE lfPitchAndFamily; 
   TCHAR lfFaceName[LF_FACESIZE]; 
} LOGFONT

typedef struct { ;   // cf 
    DWORD        lStructSize; 
    HWND         hwndOwner; 
    HDC          hDC; 
    LOGFONT     *lpLogFont; 
    INT          iPointSize; 
    DWORD        Flags; 
    DWORD        rgbColors; 
    LPARAM       lCustData; 
    LPCFHOOKPROC lpfnHook; 
    LPCTSTR      lpTemplateName; 
    HINSTANCE    hInstance; 
    LPTSTR       lpszStyle; 
    WORD         nFontType; 
    WORD         ___MISSING_ALIGNMENT__; 
    INT          nSizeMin; 
    INT          nSizeMax; 
} CHOOSEFONT 


typedef struct tagCOLORSCHEME {;
    DWORD dwSize;
    COLORREF clrBtnHighlight;
    COLORREF clrBtnShadow;
} COLORSCHEME

*-----------------------------------------------------------------------------*

FUNCTION WinMain()
LOCAL cMsg
LOCAL hWnd,hBar
LOCAL hBrush
local hBmp
local nProc
local hMenu
local hIcon
LOCAL wcMain IS WNDCLASS
LOCAL wcText IS WNDCLASS
LOCAL wcTB   IS WNDCLASS
SET EPOCH TO 1980
SET CENTURY ON
request DBFCDX
request DBFNTX
RddSetDefault( "DBFCDX" )

AddFontResource("Digiface.TTF")

wcText:lpszClassName := "TextView"
RegisterClass( wcText)

wcTB:lpszClassName := "TB"
wcTB:hbrBackground := GetStockObject(LTGRAY_BRUSH)
RegisterClass( wcTB )

hIcon :=LoadIcon(_GetInstance(),"0DEMO")
hBmp  :=LoadBitmap(,"BLOCK")
hBrush:=CreatePatternBrush(hBmp)
wcMain:hIcon         := hIcon
wcMain:hbrBackground := hBrush
wcMain:lpszClassName := "Demo"
RegisterClass( wcMain,,{ | hWnd, nMsg, nwParam, nlParam | MainWndProc( hWnd, nMsg, nwParam, nlParam ) } , - 1 )
DeleteObject(hBmp)

hWnd := CreateWindow("Demo","WHAT32 (Windows & Harbour Advanced Technologies) - DEMO" + chr( 0 ),WS_OVERLAPPEDWINDOW+WS_CLIPCHILDREN,100, 100, 450, 300,0 )

InitCommonControls()

hIcon := LoadImage(hInstance(), "comp", IMAGE_ICON, 16, 16)

oStat:=StatusBar():Init(WS_CHILD + WS_VISIBLE ,, hWnd, 100)
oStat:SetPanels( { 150,380,480,580,-1 } )
oStat:SetPanelIcon( 0, hIcon )
oStat:SetPanelText( 0, "What32 API StatusBar" )
oStat:SetPanelText( 2, "Enjoy" )

oProg:=ProgressBar():Init( oStat,1 ):Create()

hMenu:=PrepareMenu()
SetMenu( hWnd, hMenu )

hToolBar:=AddToolBar(hWnd)

ShowWindow(hWnd, SW_SHOWMAXIMIZED)
UpdateWindow(hWnd)

SetTimer(hWnd,1,100)

//sndPlaySoundA('S_8_22.WAV',SND_ASYNC )

DO WHILE GetMessage( @cMsg, 0, 0, 0 )
   IF !IsDialogMessage( , cMsg )
      TranslateMessage( cMsg )
      DispatchMessage( cMsg )
   ENDIF
ENDDO
RETURN( NIL )


FUNCTION MainWndProc( hWnd,nMsg,nwParam,nlParam)
local hDC,aRect,lShow,cPaint,ClientCreate,cf,lf
static hBmp,hBmp1
DO CASE
   case nMsg == WM_TIMER
        //oProg:Text :='Please Wait '+alltrim(str(oProg:percent))+'%'
        oProg:update()
        oProg:percent++
        IF oProg:percent>100
           oProg:percent:=0
        END
   CASE nMsg == WM_PAINT
        aRect:=GetClientRect(hWnd)
        hDC:=BeginPaint(hWnd,@cPaint)
        DrawGlyph(hDC,(aRect[3]/2)-(238/2),(aRect[4]/2)-(64/2),238,64,hBmp,RGB(255,0,0),.F.)
        DrawGlyph(hDC,(aRect[3]/2)-(560/2),(aRect[4]/2)-(34/2)+70,560,34,hBmp1,RGB(255,0,0),.F.)
        EndPaint(hWnd,cPaint)
        RETURN(0)
   CASE nMsg == WM_CREATE
        hBmp :=LoadBitmap(,"WHAT")
        hBmp1:=LoadBitmap(,"WHAT1")
   CASE nMsg==WM_COMMAND
        DO CASE
           CASE nwParam==100
                DestroyWindow(hWnd)
           CASE nwParam==101
           //     cf IS CHOOSEFONT
           //     lf IS LOGFONT
           //     cf:lpLogFont:=lf 
           //     cf:hwndOwner:=hWnd
           //     cf:Flags    :=/*CF_FIXEDPITCHONLY+*/CF_SCREENFONTS+CF_INITTOLOGFONTSTRUCT+CF_EFFECTS
           //     ChooseFont(cF:value)
                DialogBox(,"TESTDLG",hWnd,{|hDlg,nMsg,nwParam,nlParam|ResDlgProc(hDlg,nMsg,nwParam,nlParam)})
           CASE nwParam==102
                TestDBEdit( hWnd )

           CASE nwParam==103
                DialogBox(,"DIARY",hWnd,{|hDlg,nMsg,nwParam,nlParam|DiaryProc(hDlg,nMsg,nwParam,nlParam)})

           CASE nwParam==105
                DialogBox(,"BROWSER",hWnd,{|hDlg,nMsg,nwParam,nlParam|BrowseDlgProc(hDlg,nMsg,nwParam,nlParam)})
           CASE nwParam==106
                DialogBox(,"QBROW",hWnd,{|hDlg,nMsg,nwParam,nlParam|QBrowseProc(hDlg,nMsg,nwParam,nlParam)})



           CASE nwParam==601
                ShellAbout( hWnd , "WHAT32" , "(c) WHAT32 Development Team 2002", LoadIcon(_GetInstance(),"0DEMO"))
           CASE nwParam==201
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx with TIMEOUT",MB_OK, ,.T.,,,10)
           CASE nwParam==207
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx with Flashing Caption",MB_OK,,.T.,,.T.)
           CASE nwParam==208
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx with Resource Icon",MB_OK,,.T.,,,,'DEMO')
           CASE nwParam==209
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx with CheckBox",MB_OK,,.T.,,,,,'Show result of this option',@lShow)
                IF lShow
                   MessageBox(hWnd,'CheckBox checked','MessageBoxEx')
                END
           CASE nwParam==210
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx with User Buttons",,{{'&Print',100},{'&View',200}})
           CASE nwParam==211
                MessageBoxEx(hWnd,"WHAT32 (Windows & Harbour Advanced Technologies) - 1st DEMO",;
                                  "What32 MessageBoxEx SHOWTIME",,;
                                  {{'&Print',100},{'&View',200}},.T.,MB_ICONQUESTION,.T.,10,'DEMO','Show result of this option',@lShow)
                IF lShow
                   MessageBox(hWnd,'CheckBox checked','MessageBoxEx')
                END
           
           CASE nwParam==202
                Alert( {'First element','second element;you may set anything in here;just separated with a CR'},{'&Ok','&Cancel','&Print','&Retry','&GO HOME'})
           CASE nwParam==203
                ShellExecute(hWnd,"open","mailto:august212@msn.com",,SW_SHOW)
           CASE nwParam==204
                ShellExecute(hWnd,"open","http://www.geocities.com/what32lib",,SW_SHOW)
           CASE nwParam==205
                TView( hWnd, "TextView", "demo.prg") 
           CASE nwParam==206
                QUESTION(hWnd)
           CASE nwParam==307
                Calc(hWnd,{|n|MessageBox(hWnd,str(n),'Calculator Value')})

           CASE nwParam==301
                DialogBox(,"PIE",hWnd,{|hDlg,nMsg,nwParam,nlParam|PieDlgProc(hDlg,nMsg,nwParam,nlParam)})
           CASE nwParam==302
                DialogBox(,"BARS",hWnd,{|hDlg,nMsg,nwParam,nlParam|BarsDlgProc(hDlg,nMsg,nwParam,nlParam,2,.t.,.f.,.f.)})
           CASE nwParam==303
                DialogBox(,"BARS",hWnd,{|hDlg,nMsg,nwParam,nlParam|BarsDlgProc(hDlg,nMsg,nwParam,nlParam,2,.t.,.t.,.f.)})
           CASE nwParam==304
                DialogBox(,"BARS",hWnd,{|hDlg,nMsg,nwParam,nlParam|BarsDlgProc(hDlg,nMsg,nwParam,nlParam,2,.t.,.f.,.t.)})
           CASE nwParam==305
                DialogBox(,"BARS",hWnd,{|hDlg,nMsg,nwParam,nlParam|BarsDlgProc(hDlg,nMsg,nwParam,nlParam,3,.t.,.f.,.t.)})
           CASE nwParam==306
                //DialogBox(,"GRAPHS",hWnd,{|hDlg,nMsg,nwParam,nlParam|GraphsProc(hDlg,nMsg,nwParam,nlParam)})
                DialogBox(,"settype",hWnd,{|hDlg,nMsg,nwParam,nlParam|TypesProc(hDlg,nMsg,nwParam,nlParam,hWnd)})
        ENDCASE
   CASE nMsg==WM_DESTROY
        //sndPlaySoundA('BYE.WAV' )
        DeleteObject(hBmp)
        DeleteObject(hBmp1)
        PostQuitMessage(0)
ENDCASE
return(DefWindowProc(hWnd, nMsg, nwParam, nlParam))


STATIC FUNCTION Compile(cStr)
RETURN(&("{||"+cStr+"}"))


FUNCTION PrepareMenu()
LOCAL hMenu
LOCAL hPopupMenu
hMenu = CreateMenu( )
hPopupMenu = CreatePopupMenu( )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 101, "&Resource Dialog - Tabs, WListView and Gets"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 102, "&Auto Form Demo"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 103, "&Diary"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 105, "&Browse and Get"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 106, "&Quick Browse"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 100, "E&xit"  )
AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu , "&Dialogs",  )
hPopupMenu:=CreatePopupMenu( )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,201, "MessageBoxEx with &TIME-OUT" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,207, "MessageBoxEx with &Flashing Caption" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,208, "MessageBoxEx with Resource &Icon" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,209, "MessageBoxEx with Check &Box" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,210, "MessageBoxEx with &User Buttons" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,211, "&MessageBoxEx SHOWTIME" )

AppendMenu( hPopupMenu, MF_SEPARATOR )

AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,202, "&Alert" )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,203, "&Send e-mail" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,204, "&Harbour home page" )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,205, "&TextView - Unlimited text size - demo source" )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,206, "&Questionaire" )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,307, "&Calculator" )
AppendMenu( hMenu     , MF_ENABLED + MF_POPUP, hPopupMenu, "&Utilities" )

hPopupMenu:=CreatePopupMenu( )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 301, "Graphics - &Pie"  )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 302, "Graphics - &Bars"  )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 303, "Graphics - &More Bars"  )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 304, "Graphics - &Even More Bars"  )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 305, "Graphics - &Areas"  )
AppendMenu( hPopupMenu, MF_SEPARATOR )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, 306, "Graphs !!!"  )

AppendMenu( hMenu     , MF_ENABLED + MF_POPUP, hPopupMenu, "&Graphics !" )

hPopupMenu:=CreatePopupMenu( )
AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING,601, "&About..." )
AppendMenu( hMenu     , MF_ENABLED + MF_POPUP, hPopupMenu, "&Help" )
RETURN(hMenu)

//------------------------------------------------------------------------------------------

FUNCTION ResDlgProc( hDlg, nMsg, nwParam, nlParam )
local hWnd,hDC,oTab,hIcon1,hIcon2,hIcon3,hImgSml,cPaint,c,i,c1,c2,c3,c4
static hBmp,hFont,hPic
DO CASE
   CASE nMsg == WM_DESTROY
        DeleteObject(hBmp)
        DeleteObject(hFont)
   CASE nMsg == WM_PAINT
        hDC:=BeginPaint(hDlg,@cPaint)
        DrawGlyph(hDC,410,80,143,100,hBmp,RGB(255,0,0),.F.)
        EndPaint(hDlg,cPaint)
        return(1)
   CASE nMsg == WM_INITDIALOG
        UpdateWindow(hToolBar)
        InitCommonControlsEx(ICC_DATE_CLASSES)
        i:=GetClassInfo("SysDateTimePicker32")
        hPic:=DateTime_Create(,WS_CHILD+WS_TABSTOP+WS_BORDER+WS_VISIBLE,400,280,100,25,hDlg,0)
        hFont:=FontCreate('Times New Roman',24,.T.)
        SendDlgItemMessage(hDlg,501,WM_SETFONT,hFont,0)

        hBmp :=LoadBitmap(,"COMP")


        hIcon1:=LoadIcon(_GetInstance(),'red')
        hIcon2:=LoadIcon(_GetInstance(),'gray')
        hIcon3:=LoadIcon(_GetInstance(),'blue')

        hImgSml := ImageList_Create(16, 16, ILC_COLORDDB+ILC_MASK, 1, 0)
        ImageList_AddIcon(hImgSml, hIcon1)
        ImageList_AddIcon(hImgSml, hIcon2)
        ImageList_AddIcon(hImgSml, hIcon3)

        oTab:=TabControl():New( hDlg, 10, 20, 350, 300, WS_CHILD+WS_VISIBLE+WS_TABSTOP)
        oTab:SetImageList(hImgSml)
        oTab:Add("Array LV",   'TAB1',{|hDlg,nMsg,nwParam,nlParam|ResTabProc1(hDlg,nMsg,nwParam,nlParam)},0)
        oTab:Add("DataBase LV",'TAB2',{|hDlg,nMsg,nwParam,nlParam|ResTabProc2(hDlg,nMsg,nwParam,nlParam)},1)
        oTab:Add("Gets",       'TAB3',{|hDlg,nMsg,nwParam,nlParam|ResTabProc3(hDlg,nMsg,nwParam,nlParam)},2)
        oTab:Configure()
        BubbleTip(hPic,;  // window parenting the balloon
                    {' SysDateTimePicker32 '},;   // multi-line string (with CRLF), or array of text lines
                    30,; // time out in seconds or 0 for no time out (default 5)
                    100,;// horizonal pos. of balloon in percentage of parent width (0)
                    50,;// vertical pos. of balloon in percentage of parent height  (0)
                    ,;  // colour for the text (default blue)
                    ,;  // background color (default pale yellow)
                    1,; // 1: tailpoInt(default), 2: 3 bubbles, 3: no tail
                    ,;  // font for the text ( default parent font)
                    2,; // amount of shadow in horz direction (left or right) (default 1)
                    2,; // amount of shadow in vert direction ( up or down ) (default 5)
                    ,;  // messagebeep beep number (default none)
                    10,;  // round rectangle horizontal radius
                    20)   // round rectangle vertival radius


        RETURN( 1 )

   CASE nMsg == WM_SYSCOMMAND
        IF nwParam == SC_CLOSE
           EndDialog( hDlg, IDOK )
        ENDIF
   CASE nMsg == WM_COMMAND
        IF nwParam == IDOK
           EndDialog( hDlg, IDOK )
           RETURN( 0 )
        ENDIF
        IF nwParam == IDCANCEL
           EndDialog( hDlg, IDCANCEL )
           RETURN( 1 )
        ENDIF
ENDCASE
RETURN( 0 )

FUNCTION ResTabProc1( hDlg, nMsg, nwParam, nlParam )
local oListView,hIcon1,hIcon2,hIcon3,n,hDC,hC,aRect,nPix
static hFont,hBrush
DO CASE
//-------------------------------- this is for XP compatibility ONLY
   CASE nMsg == WM_CTLCOLORDLG
        SetBrushOrgEx(nwParam, 11,11,NIL)
        return(GetStockObject(NULL_BRUSH))
   CASE nMsg == WM_CTLCOLORSTATIC
        SetBkMode(nwParam,1)
        return(GetStockObject(NULL_BRUSH))
//------------------------------------------------------------------

   CASE nMsg == WM_DESTROY
        deleteobject(hFont)
        DeleteObject(hBrush)
   CASE nMsg == WM_INITDIALOG
        hFont:=FontCreate('Times New Roman',24,.T.)
        SendDlgItemMessage(hDlg,100,WM_SETFONT,hFont,0)

        oListView:=WListView():New(hDlg,2000,5,5,330,200)
        oListView:SetFullRow(.t.)
        oListView:SetHeaderDrop(.t.)

        oListView:ArrayData:={ { str(100000000,10), 'testing arrays', str(500,5) },;
                               { str(   358000,10), 'arrays',         str(200,5) },;
                               { str(      880,10), 'testing',        str(500,5) },;
                               { str(100000000,10), 'testing arrays', str(500,5) },;
                               { str(   358000,10), 'arrays',         str(200,5) },;
                               { str(      880,10), 'testing',        str(500,5) },;
                               { str(100000000,10), 'testing arrays', str(500,5) },;
                               { str(   358000,10), 'arrays',         str(200,5) },;
                               { str(      880,10), 'testing',        str(500,5) },;
                               { str(100000000,10), 'testing arrays', str(500,5) },;
                               { str(   358000,10), 'arrays',         str(200,5) },;
                               { str(      880,10), 'testing',        str(500,5) },;
                               { str(100000000,10), 'testing arrays', str(500,5) },;
                               { str(   358000,10), 'arrays',         str(200,5) },;
                               { str(      880,10), 'testing',        str(500,5) } }
            
        oListView:AddColumn('Header 1', 100, 1 )
        oListView:AddColumn('Header 2', 130, 0 )
        oListView:AddColumn('Header 3',  80, 1 )
            
        oListView:nItems :=15

        hIcon1:=LoadIcon(_GetInstance(),'red')
        hIcon2:=LoadIcon(_GetInstance(),'gray')
        hIcon3:=LoadIcon(_GetInstance(),'blue')
        
        FOR n:=1 to 5
            oListView:AddIcon(hIcon1)
            oListView:AddIcon(hIcon2)
            oListView:AddIcon(hIcon3)
        NEXT
        oListView:Create()
        RETURN( 0 )
ENDCASE
RETURN( 0 )

FUNCTION ResTabProc2( hDlg, nMsg, nwParam, nlParam )
local hIcon1,hIcon2,hIcon3,n,aFilter,cFile,nAlign,bColumn,aStruct
STATIC oListView
STATIC lGrid:=.T.
DO CASE
//-------------------------------- this is for XP compatibility ONLY
   CASE nMsg == WM_CTLCOLORDLG
        return(GetStockObject(NULL_BRUSH))
   CASE nMsg == WM_CTLCOLORSTATIC
        SetBkMode(nwParam,1)
        return(GetStockObject(NULL_BRUSH))
//------------------------------------------------------------------

   CASE nMsg == WM_DESTROY
        IF ALIAS()='TMP'
           CLOSE TMP
        END
   CASE nMsg == WM_INITDIALOG
        oListView:=WListView():New(hDlg,2000,5,5,330,220)
        RETURN(0)
   CASE nMsg == WM_COMMAND
        DO CASE
           CASE nwParam==11
                aFilter = { {"DataBase Files (*.dbf)","*.DBF"} }
                cFile:=GetOpenFileName( hDlg,space(255), "DataBase Files", aFilter, OFN_EXPLORER+OFN_FILEMUSTEXIST, "" ,'DBF')
                IF EMPTY(cFile)
                   RETURN(0)
                END
                DestroyWindow(oListView:hWnd)
                IF ALIAS()='TMP'
                   CLOSE TMP
                END
                USE (cFile) ALIAS TMP NEW SHARED
                ORDSETFOCUS(1)
                GO TOP
                oListView:=WListView():New(hDlg,2000,5,5,330,220)
                oListView:SetFullRow(.t.)
                oListView:SetHeaderDrop(.t.)
                aStruct:=DBSTRUCT()
                FOR n:=1 TO LEN(aStruct)
                    DO CASE
                       CASE aStruct[n][2]=='C'
                            nAlign :=0
                            bColumn:=Compile(aStruct[n][1])
                       CASE aStruct[n][2]=='N'
                            nAlign :=1
                            bColumn:=Compile("STR("+aStruct[n][1]+")")
                       CASE aStruct[n][2]='L'
                            nAlign :=2
                            bColumn:=Compile("IIF("+aStruct[n][1]+",'.T.','.F.')")
                       CASE aStruct[n][2]='D'
                            nAlign :=2
                            bColumn:=Compile("DTOC("+aStruct[n][1]+")")
                       CASE aStruct[n][2]='M'
                            nAlign :=2
                            bColumn:=Compile("< memo >")
                    END
                    oListView:AddColumn(Proper(aStruct[n][1]), aStruct[n][3]*10, nAlign, bColumn )
                NEXT
                oListView:nItems :=RECCOUNT()
                oListView:Create()
           CASE nwParam==12
                oListView:SetGrid(lGrid)
                IF lGrid
                   lGrid:=.F.
                   SetDlgItemText(hDlg,12,'Grid OFF')
                  ELSE
                   lGrid:=.T.
                   SetDlgItemText(hDlg,12,'Grid ON')
                END
        ENDCASE
ENDCASE
RETURN( 0 )

FUNCTION ResTabProc3( hDlg, nMsg, nwParam, nlParam )
local hDC,aColors,  aRect, cText
local GetList:={}
local dDate:=DATE()
static hFont
DO CASE
//-------------------------------- this is for XP compatibility ONLY
   CASE nMsg == WM_CTLCOLORDLG
        return(GetStockObject(NULL_BRUSH))
   CASE nMsg == WM_CTLCOLORSTATIC
        SetBkMode(nwParam,1)
        return(GetStockObject(NULL_BRUSH))
//------------------------------------------------------------------
   CASE nMsg == WM_DESTROY
        deleteobject(hFont)
   CASE nMsg == WM_INITDIALOG
        hFont:=FontCreate('Times New Roman',24,.T.)
        SendDlgItemMessage(hDlg,100,WM_SETFONT,hFont,0)
        aColors:={ {,},{RGB(0,0,0),RGB(255, 255, 0)}}
        @ DIALOG hDlg ID 101 GET Space( 25 ) PICTURE '!!!99-99999999/A-(999999)' ;
                 VALID { | x, lRet | lRet := ! ( ' ' $ x:cargo ) , If( lRet, , MessageBox( hDlg, 'The part number may not contain spaces', 'Error', MB_OK ) ) , lRet } ;
                 COLOR aColors ;
                 OVERSTRIKE LIKEDOS
        aColors[1][1]:=RGB(128, 0, 0) 
        @ DIALOG hDlg ID 102 GET dDate COLOR aColors OVERSTRIKE LIKEDOS
        aColors[1][1]:=RGB(0, 0, 128)
        @ DIALOG hDlg ID 103 GET Space( 14 ) PICTURE '(999) 999-9999' ;
                 VALID { | x, lRet | lRet := ! Empty( SubStr( x:cargo, 2, 3 ) ) , ;
                       If( lRet, , MessageBox( hDlg, 'The phone area code must be entered', 'Error', MB_OK ) ) , lRet } ;
                 COLOR aColors LIKEDOS
        aColors[1][1]:=RGB(0,128,0)
        @ DIALOG hDlg ID 104 GET 12345.67890 PICTURE '99999.99999' ;
                 VALID { | x, lRet | lRet := x:cargo < 0, ;
                       If( lRet, , MessageBox( hDlg, 'The number must be less than zero', 'Error', MB_OK ) ) , lRet } ;
                 COLOR aColors LIKEDOS
        @ DIALOG hDlg ID 105 GET Space( 30 ) COLOR aColors OVERSTRIKE LIKEDOS
        RETURN(0)
ENDCASE
RETURN( 0 )



function question(hParent)
dialogbox(,"question",hparent,{|hDlg,nMsg,nwParam,nlParam| questionproc(hDlg,nMsg,nwParam,nlParam)})
return(NIL)

function QuestionProc(hDlg,nMsg,nwParam,nlparam)
local nProc
do case
   case nmsg==WM_INITDIALOG
        nProc := SetProcedure(GetDlgItem(hDlg,1),{|hWnd,nMsg,nwParam,nlParam| funproc(nProc,hWnd,nMsg,nwParam,nlParam)},WM_MOUSEMOVE)
        return(1)
   case nmsg==WM_SYSCOMMAND
        if nwparam==SC_CLOSE
           enddialog(hDlg,0)
        endif
endcase
return(0)

function funproc(nProc,hWnd,nmsg,nwParam,nlParam)
local hParent
local arect
local acrect
local pt
local aMouse
local xoff
local yoff
if nmsg==WM_MOUSEMOVE
   hParent:=GetParent(hWnd)
   arect:=getwindowrect(hWnd)
   pt:={arect[1],arect[2]}
   screentoclient(hParent,pt)
   arect[1]:=pt[1]
   arect[2]:=pt[2]
   pt[1]:=arect[3]
   pt[2]:=arect[4]
   screentoclient(hParent,pt)
   arect[3]:=pt[1]
   arect[4]:=pt[2]
   acrect:=GetClientRect(hParent)
   amouse:={LoWord(nlParam), Hiword(nlparam) }
   xOff:=0
   yOff:=0
   if ((arect[4]-arect[2])/2) < amouse[2]
      yOff:= 0 - ((arect[4]-arect[2])/2)
   endif
   if ((arect[4]-arect[2])/2) >= amouse[2]
      yOff:= ((arect[4]-arect[2])/2)
   endif
   if ((arect[3]-arect[1])/2) < amouse[1]
      xOff:= 0 - ((arect[3]-arect[1])/2)
   endif
   if ((arect[3]-arect[1])/2) >= amouse[1]
      xOff:= ((arect[3]-arect[1])/2)
   endif
   if (arect[1] + xOff)  <= 0
      xOff := (acrect[3] - (3*(arect[3] - arect[1])/2)) - (arect[1] )
   endif
   if (arect[1]+xOff) >= (acrect[3]-(arect[3]-arect[1]))
      xOff :=  ((arect[3]-arect[1])/2) - arect[1]
   endif
   if (arect[2]+yOff) <= (acrect[4]/2)
      yOff:=(acrect[4]-(3*(arect[4]-arect[2])/2)) - (arect[2] )
   endif
   if (arect[2]+yOff) >= (acrect[4]-(arect[4]-arect[2]))
      yOff:=(acrect[4]/2) - arect[2]
   endif
   movewindow(hWnd,arect[1]+xOff,arect[2]+yOff,arect[3]-arect[1],arect[4]-arect[2],.t.)
   return(0)
endif
return CallWindowProc(nProc, hWnd,nMsg, nwParam,nlParam)

FUNCTION ZeroInit(ostr)
  ostr:buffer(replicate(chr( 0 ),ostr : sizeof()))
  RETURN(NIL)


FUNCTION PieDlgProc( hDlg, nMsg, nwParam, nlParam )
local aRect
STATIC oPie,lLines
DO CASE
   CASE nMsg == WM_INITDIALOG
        lLines:=.F.
        oPie:=PieChart():Init(hDlg,.t.)
        oPie:colors:={}
        oPie:SetRect({0,0,1300,900})
        oPie:setdata( {20,30,20,10,20})
        oPie:setcolors( { C_RED,C_BLUE,C_GREEN,C_LIGHTCYAN,C_PURPLE} )
        oPie:settype(.t.)
        oPie:alegends:={'Label 1','Label 2','Label 3','Label 4','Label 5'}
        oPie:blacklines:=.f.
        oPie:Explode(1,10)
        oPie:Explode(2,10)
        oPie:Explode(3,10)
        oPie:Explode(4,10)
        oPie:Explode(5,10)
        RETURN(0)
   CASE nMsg == WM_COMMAND
        do case
           CASE nwParam == IDOK
                EndDialog( hDlg, IDOK )
                RETURN( 0 )
           CASE nwParam == IDCANCEL
                CLOSE DATA
                EndDialog( hDlg, IDCANCEL )
                RETURN( 0 )
           CASE nwParam==101
                IF lLines
                   lLines:=.F.
                   SetDlgItemText(hDlg,101,'Lines ON')
                  ELSE
                   lLines:=.T.
                   SetDlgItemText(hDlg,101,'Lines OFF')
                END
                oPie:Separators(lLines)
        end
ENDCASE
return(0)


FUNCTION BarsDlgProc( hDlg, nMsg, nwParam, nlParam,nStyle,l3d,lSpace,lStacked )
local aRect,cP,hDC
STATIC oBars,lLines
DO CASE
   CASE nMsg == WM_INITDIALOG
        lLines:=.F.
        oBars:=bargraph():Init(hDlg,.T.)
        oBars:setdata({{3,9,1},{15,2,4},{4,6,5},{9,1,4},{4,3,8},{1,2,9}})
        oBars:settype(nStyle,l3d,lSpace,lStacked)
        oBars:setrect({100,40,1450,800})
        oBars:alabels:={'January','February','March','April','May','June'}
        oBars:SetColors({C_DARKRED,C_DARKBLUE,C_DARKGREEN})
        oBars:alegends:={'A','B','C'}
        oBars:setScale( ,NIL, ,.t.,.t.)
        oBars:blacklines:=.F.
        RETURN(0)
   CASE nMsg == WM_COMMAND
        IF nwParam == IDOK
          // view SHBrowseForFolder(hDlg,"Installed Printers",BIF_BROWSEFORPRINTER,CSIDL_PRINTERS )

           EndDialog( hDlg, IDOK )
           RETURN( 0 )
        ENDIF
        IF nwParam==101
           PrintGraph(hDlg,oBars)

           IF lLines
              lLines:=.F.
              SetDlgItemText(hDlg,101,'Lines ON')
             ELSE
              lLines:=.T.
              SetDlgItemText(hDlg,101,'Lines OFF')
           END
           oBars:Separators(lLines)
        end
ENDCASE
return(0)

*-----------------------------------------------------------------------------*

FUNCTION PrintGraph(hParent,o)

  LOCAL pWidth
  LOCAL pHeight
  LOCAL hDC
  LOCAL nPtr

  LOCAL pd IS PRINTDLG
  LOCAL psd IS PAGESETUPDLG
  LOCAL dvnm IS DEVNAMES

  psd:Reset()
  psd:lStructSize:=psd:sizeof
  psd:hwndOwner:=hParent
  psd:flags:=0
  view psd:sizeof
  PageSetupDlg(psd:value)
  psd:devalue()

  nPtr:=GlobalLock(psd:hDevNames)

  // This is hData - get Ptr
  dvnm:Buffer(peek(nPtr,dvnm:sizeof))

  view GetLastError(),psd,dvnm

  view peek(nPtr+8)
  view peek(nPtr+dvnm:wDriverOffset)
  view peek(nPtr+dvnm:wDeviceOffset)
  view peek(nPtr+dvnm:wOutputOffset)
     // driver, device, and port name strings follow wDefault
  GlobalUnlock(psd:hDevNames)




  // PrintDialog

  /*

  pd:Reset()
  pd:lStructSize:=pd:sizeof
  pd:hwndOwner:=hParent
  pd:Flags:=PD_RETURNDC //PD_PAGENUMS+PD_PRINTSETUP+PD_RETURNDC
  view pd:sizeof

  view 'calling'
  if PrintDlg(pd:value)
     pd:Devalue()
     hDC:=pd:hDC

     pwidth :=getdevicecaps(hdc,HORZRES)    // pixels
     pheight:=getdevicecaps(hdc,VERTRES)

     setmapmode(hdc,7)
     setwindowextex(hdc,1500,1000)
     setviewportextex(hdc,pwidth,pheight)
     setwindoworgex(hdc,0,0)

     startdoc(hdc,'Graph')
     startpage(hdc)
     o:show(hdc,{0,10,1500,250})
     endpage(hdc)
     enddoc(hdc)
     deletedc(hdc)

  endif

  */


RETURN(NIL)

     // PrintDlgEx



//----------------------------------------------------------------------------------


FUNCTION BrowseDlgProc( hDlg, nMsg, nwParam, nlParam )
LOCAL hIcon1,hIcon2,hIcon3,hIcon4,hDC,cPaint
local GetList:={}
static hImgSml,oBrowse,oCol1,oCol2,oCol3,oCol4,oCol5,lIcons
DO CASE
   CASE nMsg == WM_INITDIALOG 
        lIcons:=.F.
        USE TEST ALIAS TEST NEW
        GO TOP
        hIcon1:=LoadIcon(_GetInstance(),'cdr')
        hIcon2:=LoadIcon(_GetInstance(),'dial')
        hIcon3:=LoadIcon(_GetInstance(),'expl')
        hIcon4:=LoadBitmap(_GetInstance(),'notes')// LoadIcon(_GetInstance(),'users')
        hImgSml := ImageList_Create(16, 16, ILC_COLORDDB+ILC_MASK, 1, 0)
        ImageList_AddIcon(hImgSml, hIcon1)
        ImageList_AddIcon(hImgSml, hIcon2)
        ImageList_AddIcon(hImgSml, hIcon3)
        ImageList_AddMasked(hImgSml, hIcon4,C_RED) //ImageList_AddIcon(hImgSml, hIcon4)
        DeleteObject(hIcon4)

        oBrowse:=whBrowse():Init('TEST',GetDlgItem(hDlg,101),hImgSml,{||SetDlgItemText(hDlg,102,TEST->FIRST),SetDlgItemText(hDlg,103,TEST->LAST),SetDlgItemText(hDlg,104,TEST->STREET) })
        oCol1:=whColumn():Init( 'First', {||TEST->FIRST} ,DT_LEFT,150,,,,,,,, ,,, DT_LEFT, hImgSml,2)
        oCol2:=whColumn():INIT( 'Last'  ,{||TEST->LAST}  ,DT_LEFT,150,,C_LIGHTYELLOW,,,,,, ,,, DT_LEFT, hImgSml,2)
        oCol3:=whColumn():INIT( 'Street',{||TEST->STREET},DT_LEFT,300,,,,,,,, ,,, DT_LEFT, hImgSml,2)
        oCol4:=whColumn():INIT( 'Status',{||IIF(TEST->MARRIED,'Married','Single')},DT_LEFT,100,,,,,,,,3,,, DT_LEFT, hImgSml,2)
        oCol5:=whColumn():INIT( 'Salary',{||STR(TEST->SALARY,10,2)},DT_RIGHT,150,,,,,,,, ,,1, DT_RIGHT,hImgSml,20)
        oCol4:bgColor:={||IIF(TEST->MARRIED,C_DARKRED,)}
        oCol4:fgColor:={||IIF(TEST->MARRIED,C_WHITE,)}
        oCol4:style:=TBC_MOVE + TBC_SIZE


        oBrowse:addColumn(oCol1)
        oBrowse:addColumn(oCol5)
        oBrowse:addColumn(oCol2)
        oBrowse:addColumn(oCol3)
        oBrowse:addColumn(oCol4)

        oBrowse:HeadFont  :=FontCreate("MS Sans Serif",8,.T.)
        oBrowse:Font      :=FontCreate("MS Sans Serif",8,.F.)
        oBrowse:HeadHeight:=25
        oBrowse:ItemHeight:=20
        oBrowse:bKillBlock:={|| DeleteObject(oBrowse:Font),DeleteObject(oBrowse:HeadFont)}
        oBrowse:configure()

        @ DIALOG hDlg ID 102 GET TEST->FIRST
        @ DIALOG hDlg ID 103 GET TEST->LAST
        @ DIALOG hDlg ID 104 GET TEST->STREET

        return(0)
   CASE nMsg == WM_COMMAND
        DO CASE
           CASE nwParam == IDCANCEL
                CLOSE DATA
                EndDialog( hDlg, IDCANCEL )
                RETURN( 0 )
           CASE nwParam==200
                IF lIcons
                   lIcons:=.F.
                   oCol1:OffSet:=2
                   oCol1:BmpIndex:=NIL
                   SetDlgItemText(hDlg,200,'Show some images')
                  ELSE
                   oCol1:OffSet:=20
                   oCol1:BmpIndex:={||IIF(TEST->MARRIED,3,)}
                   lIcons:=.T.
                   SetDlgItemText(hDlg,200,'Hide the images')
                END
                oBrowse:configure()
        end
ENDCASE
return(0)

FUNCTION QBrowseProc( hDlg, nMsg, nwParam, nlParam )
local aStruc,o,oBrowse,oCol1,oCol2,oCol3
DO CASE
   CASE nMsg == WM_INITDIALOG
        USE TEST ALIAS TEST NEW SHARED
        aStruc:=DBSTRUCT()
        GO TOP
        oBrowse:=whBrowse():Init('TEST',GetDlgItem(hDlg,100))
        oCol1:=whColumn():Init( 'First', {||{TEST->FIRST,'3 lines','Centered'}} ,DT_LEFT,150) //,,,,,,,,,, , DT_LEFT, hImgSml,2)
        oCol2:=whColumn():INIT( 'Last'  ,{||{TEST->LAST, '2 lines Centered'} }  ,DT_LEFT,150) //,,,,,,,,,, , DT_LEFT, hImgSml,2)
        oCol3:=whColumn():INIT( 'Street',{||TEST->STREET},DT_LEFT,300) //,,,,,,,,,, , DT_LEFT, hImgSml,2)
        oCol1:VertAlign   :=TA_TOP
        oCol2:VertAlign   :=TA_CENTER
        oCol3:VertAlign   :=TA_BOTTOM
        
        oBrowse:addColumn(oCol1)
        oBrowse:addColumn(oCol2)
        oBrowse:addColumn(oCol3)
        oBrowse:HeadFont  :=FontCreate("MS Sans Serif",8,.T.)
        oBrowse:Font      :=FontCreate("MS Sans Serif",8,.F.)
        oBrowse:HeadHeight:=20
        oBrowse:ItemHeight:=50
        oBrowse:bKillBlock:={|| DeleteObject(oBrowse:Font),DeleteObject(oBrowse:HeadFont)}
        oBrowse:configure()

        BROWSE aStruc AS o USE WINDOW GetDlgItem(hDlg,101)
           ADD COLUMN "Field" DATA {|oCol,oB,n| asString(oB:source[n,1]) } WIDTH 100
           ADD COLUMN "Type"  DATA {|oCol,oB,n| asString(oB:source[n,2]) } WIDTH 50
           ADD COLUMN "Len"   DATA {|oCol,oB,n| asString(oB:source[n,3]) } WIDTH 40
           ADD COLUMN "Dec"   DATA {|oCol,oB,n| asString(oB:source[n,4]) } WIDTH 40
           o:wantHScroll:=.F.
        END BROWSE
   CASE nMsg == WM_COMMAND
        DO CASE
           CASE nwParam == IDCANCEL
                CLOSE DATA
                EndDialog( hDlg, IDCANCEL )
                RETURN( 0 )
        ENDCASE
ENDCASE
return(0)

//--------------------------------------------------------------------------------------
 function sndPlaySoundA(pszSound,fuSound )
 static hLib
 STATIC nProcAddr
 IF nProcAddr==NIL
   hLib:=Loadlibrary("WINMM")
   nProcAddr:=GetProcAddress(hLib,"sndPlaySoundA")
 ENDIF
 return CallDLL(hLib,nProcAddr,,8,10,pszSound,-3,fuSound )


Function TestDBEdit( hParent )
 
   Local aDlg
   Local aEdit := { }
   Local i
   Local xVal
   Local aCtrl
   Local lHelp   := .T. // show help button
   Local lOnSide := .T. // put buttons on the side ?
   Local lCenter := .F. // center push buttons ?
   Local cAlias:="SOMEALIAS"
   Static cFile:=""
 
   cFile:=GetOpenFileName(hParent, ;
                             LEFT(cFile,RAT("\",cFile)-1),;
                             "Select DBF File To Open",;
                             {{"DBF Files","*.DBF"}},,;
                             LEFT(cFile,RAT("\",cFile)-1),;
                             "DBF")
 
  if ! empty(cFile)
 
   DBUSEAREA(.T.,,cFile,cAlias,.T.,.F.) // or any data base with reasonable number of fields
   IF USED().AND.!neterr()
 
   For i := 1 To MIN( (cAlias) -> ( FCOUNT( ) ), 12) // max 12 fields for this demo
      xVal := (cAlias) -> ( FieldGet( i ) )
      aAdd( aEdit, { Proper( (cAlias) -> ( FieldName( i ) ) ) , xVal , ;
                     If( ValType( xVal ) == "C" .AND. Len( xVal ) > 50, { 30, 3 } , ;
                         If( ValType( xVal ) == "N", Len( STR( xVal ) ) , ) ) } )
   Next
 
   aAdd( aEdit, { "Previous", , , CT_BUTTON } )
   aAdd( aEdit, { "Next", , , CT_BUTTON } )
 
   aDlg := AutoForm( cFile, @aEdit, "Print", "Close" , lHelp , lOnSide, lCenter, @aCtrl  )
 
   DialogBox( , aDlg, hParent, { | h, m, w, l | DlgDbProc( h, m, w, l, aCtrl, cAlias ) } )
 
   (cAlias) -> ( DBCLOSEAREA( ) )
 
   endif
  Endif
 
   Return( NIL )
 
*-----------------------------------------------------------------------------*
 
Function DlgDbProc( hDlg, nMsg, nwParam, nlParam, aCtrl, cAlias )
 
   Do Case
   Case nMsg == WM_INITDIALOG
      SetValues( hDlg, aCtrl, cAlias )
      ShowWindow( hDlg, SW_SHOW )
      Return( 1 )
 
   Case nMsg == WM_SYSCOMMAND
      If nwParam == SC_CLOSE
         // DestroyWindow(hDlg)
         EndDialog( hDlg, IDOK )
      EndIf
 
   Case nMsg == WM_COMMAND
 
      If nwParam == IDCANCEL
         EndDialog( hDlg, IDCANCEL )
         Return( 1 )
 
      ElseIf nwParam == IDOK
         MessageBox( hDlg, "That would be too much for the first demo !", "Print ?", MB_ICONHAND )
 
      ElseIf nwParam == 101
         MessageBox( hDlg, "If you have problems with this simple demo," + CHR( 13 ) + "Nothing will help you ay more", "Help? with what ?" )
 

      ElseIf nwParam == 100 + Len( aCtrl )
         ( cAlias ) -> ( DBSKIP( 1 ) )
         If ( cAlias ) -> ( EOF( ) )
            MessageBox( hDlg, "Hey, This is the last record", "Come on !", MB_ICONEXCLAMATION )
         Else
            SetValues( hDlg, aCtrl, cAlias )
         EndIf
 
      ElseIf nwParam == 100 + Len( aCtrl ) - 1
         ( cAlias ) -> ( DBSKIP( - 1 ) )
         If ( cAlias ) -> ( BOF( ) )
            MessageBox( hDlg, "You are back where you started", "Go slowly !", MB_ICONEXCLAMATION )
         Else
            SetValues( hDlg, aCtrl, cAlias )
         EndIf
 
      EndIf
   EndCase
 
   Return( 0 )
 

*----------------------------
 
Function SetValues( hDlg, aCtrl, cAlias )
 
   Local i
   Local xVar
 
   For i := 1 To Len( aCtrl ) - 2
      xVar := ( cAlias ) -> ( FIELDGET( i ) )
      Do Case
      Case ValType( xVar ) == "C"
         SetDlgItemText( hDlg, aCtrl[ i, 2 ] , xVar )
      Case ValType( xVar ) == "N"
         SetDlgItemText( hDlg, aCtrl[ i, 2 ] , STR( xVar ) )
      Case ValType( xVar ) == "D"
         SetDlgItemText( hDlg, aCtrl[ i, 2 ] , DTOC( xVar ) )
      Case ValType( xVar ) == "L"
         CheckDlgButton( hDlg, aCtrl[ i, 2 ] , If( xVar, 1, 0 ) )
      EndCase
   Next
 
   Return( NIL )
 
FUNCTION DiaryProc( hDlg, nMsg, nwParam, nlParam )
local oBrowse, dSdate,nYear,nMonth, aCal[6][7],o,hFont
DO CASE
   CASE nMsg == WM_INITDIALOG
        nYear  := YEAR( DATE() )
        nMonth := MONTH( DATE() )
        dsdate := STOD(   str(nYear,4) +   strzero(nMonth,2) +"01")
        SetMonth( dSdate, aCal )
        oBrowse:=whBrowse():Init(aCal,GetDlgItem(hDlg,100))
        o:=WHColumn():Init("Sun", {|oCol,oB,n|oB:source[n][1]}, DT_LEFT,50,,,,,,C_RED,DT_CENTER)
        o:fgColor :=C_RED
        o:style:=0
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Mon", {|oCol,oB,n|oB:source[n][2]}, DT_LEFT,50,,,,,,C_RED,DT_CENTER)
        o:style:=0
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Tue", {|oCol,oB,n|oB:source[n][3]}, DT_LEFT,50,,,,,,C_RED,DT_CENTER)
        o:style:=0
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Wed", {|oCol,oB,n|oB:source[n][4]}, DT_LEFT,50)
        o:style:=0
        o:HeadAlign:=TA_CENTER
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Thu", {|oCol,oB,n|oB:source[n][5]}, DT_LEFT,50,,,,,,C_RED,TA_CENTER)
        o:style:=0
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Fri", {|oCol,oB,n|oB:source[n][6]}, DT_LEFT,51,,,,,,C_RED,TA_CENTER)
        o:style:=0
        oBrowse:addColumn(o) 
        o:=WHColumn():Init("Sat", {|oCol,oB,n|oB:source[n][7]}, DT_LEFT,51,,,,,,C_RED,TA_CENTER)
        o:fgColor :=C_BLUE
        o:style:=0
        oBrowse:addColumn(o)

        oBrowse:HeadFont  :=FontCreate("MS Sans Serif",8,.T.)
        oBrowse:Font      :=FontCreate("MS Sans Serif",8,.t.)
        oBrowse:HeadHeight:=21
        oBrowse:ItemHeight:=50
        oBrowse:wantResize :=.F.
        oBrowse:wantHScroll:=.F.
        oBrowse:wantVScroll:=.F.
        oBrowse:bKillBlock:={|| DeleteObject(oBrowse:Font),DeleteObject(oBrowse:HeadFont),DeleteObject(hFont)}
        oBrowse:configure()
        GetToday(oBrowse,aCal)
        SetFocus(oBrowse:hWnd)

        BubbleTip(oBrowse:hWnd,'Using WhBrowse()',0,100,50,,,1,,2,2,,0,0)
        
        return(0)
   CASE nMsg == WM_COMMAND
        DO CASE
           CASE nwParam == IDCANCEL
                CLOSE DATA
                EndDialog( hDlg, IDCANCEL )
                RETURN( 0 )
        ENDCASE
ENDCASE
RETURN(0)

STATIC FUNCTION SetMonth( dSdate, aCal )
       LOCAL nCtr, nCtr2
       LOCAL nStart, nEnd, nDay1
       nDay1 := DOW( dSdate )
       DO CASE
          CASE MONTH( dSdate ) == MONTH( dSdate + 30 )
               nEnd := 31
          CASE MONTH( dSdate ) == MONTH( dSdate + 29 )
               nEnd := 30
          CASE MONTH( dSdate ) == MONTH( dSdate + 28 )
               nEnd := 29
          OTHERWISE
               nEnd := 28
       ENDCASE
       nStart := 1
       FOR nCtr := 1 TO 7
           IF nCtr < nDay1
              aCal[1][nCtr] := ""
           ELSE
              aCal[1][nCtr] := LTRIM(STR(nStart++))
           ENDIF
       NEXT
       FOR nCtr := 2 TO 6
           FOR nCtr2 := 1 TO 7
               IF nStart <= nEnd
                  aCal[nCtr][nCtr2] := LTRIM(STR(nStart++))
               ELSE
                  aCal[nCtr][nCtr2] := ""
               ENDIF
           NEXT
       NEXT
RETURN(NIL)


STATIC FUNCTION GetToday( oB, aCal )
       LOCAL nWeek, nDay, cToday
       cToday := ALLTRIM( STR( DAY( DATE() ) ) ) //+ CHR(13) + CHR(10)
       FOR nWeek := 1 TO 6
           FOR nDay := 1 TO 7
               IF ALLTRIM( aCal[ nWeek ][ nDay ] ) == cToday
                  oB:rowPos := nWeek
                  oB:colPos := nDay
                  EXIT
               ENDIF
           NEXT
       NEXT
RETURN( NIL )

FUNCTION AddToolbar(hParent)

  LOCAL rb:=rebar():init()
  LOCAL tb:=toolbar():init()
  LOCAL aRect,cs
  LOCAL hMenu,hWnd,hImg,hBmp


  hImg := ImageList_Create(24, 24, ILC_COLORDDB+ILC_MASK) //, LEN(ADIR), 1)
  hBmp:=LoadImage(hInstance(),"Faces",IMAGE_BITMAP,0,0,LR_LOADTRANSPARENT )
  ImageList_AddMasked(hImg, hBmp,rgb(255,0,0) )

  rb:create(hParent)
  /*
  ,WS_VISIBLE+WS_BORDER+WS_CHILD+WS_CLIPCHILDREN+;
                           WS_CLIPSIBLINGS+RBS_VARHEIGHT+RBS_BANDBORDERS+;
                           CCS_NODIVIDER+CCS_NOPARENTALIGN+CCS_TOP+CCS_VERT)

  SendMessage(rb:hWnd, RB_SETBKCOLOR, 0, GetSysColor(COLOR_BTNSHADOW))
*/

//  tb:addstring("New"+CHR(0))
//  tb:addstring("Open"+CHR(0))
//  tb:addstring("Save"+CHR(0))
//  tb:addstring("Copy"+CHR(0))
//  tb:addstring("Cut"+CHR(0))
//  tb:addstring("Paste"+CHR(0))

  tb:AddButton( 0, 101,,,, 0,,'oops')
  tb:AddButton( 1, 102,,,, 1 )
  tb:AddButton( 2, 103,,TBSTYLE_DROPDOWN,,2 )
  tb:AddButton( 0,   0,,TBSTYLE_SEP)
  tb:AddButton( 3, 201,,,, 3 )
  tb:AddButton( 4, 202,,,, 4 )
  tb:AddButton( 5, 203,,,, 5 )
  tb:AddButton( 6, 204,,,, 6 )
  tb:AddButton( 0,   0,,TBSTYLE_SEP)
  tb:AddButton( 7, 301,,,, 7)
  tb:AddButton( 8, 302,,,, 8)
  tb:AddButton( 9, 303,,,, 9)
  tb:AddButton(10, 304,,,,10)
  tb:AddButton(11, 305,,,,11)

  tb:create(rb:hWnd,TBSTYLE_FLAT+TBSTYLE_TOOLTIPS+WS_CLIPCHILDREN+WS_CLIPSIBLINGS+CCS_ADJUSTABLE+CCS_NODIVIDER+CCS_NORESIZE+WS_CHILD,,12,,,32,32,24,24 )
/*
  cs IS COLORSCHEME
  cs:dwSize:=cs:sizeof
  cs:clrBtnHighlight:=GetSysColor(COLOR_BTNFACE)
  cs:clrBtnShadow   :=GetSysColor(COLOR_3DDKSHADOW)
  SendMessage(tb:hWnd,TB_SETCOLORSCHEME,0,cs:value)
*/

  SendMessage(tb:hwnd,TB_SETIMAGELIST,0,hImg)
  SendMessage(tb:hwnd,TB_SETBUTTONSIZE,0,MAKELONG(50,40))
  
  rb:addband(,,tb:hwnd,100,40,,"",)
 
  RETURN(rb:hWnd)

//------------------------------------------------------------------------------------------


FUNCTION GraphsProc( hWnd, nMsg, nwParam, nlParam )
local ntemp,n
DO CASE
   CASE nMsg == WM_COMMAND
        DO CASE
           CASE nwParam==101
                IF DialogBox(,"settype",hWnd,{|hDlg,nMsg,nwParam,nlParam|TypesProc(hDlg,nMsg,nwParam,nlParam)})==IDOK
                   GraphIt(hWnd)
                end
           case nwParam==IDCANCEL
                enddialog(hWnd,IDCANCEL)
                
        ENDCASE
ENDCASE
return(0)

FUNCTION TypesProc( hDlg, nMsg, nwParam, nlParam,hWnd)
local ntemp,n
DO CASE
   CASE nMsg == WM_DESTROY
        DeleteObject(tbrush)
        DeleteObject(rbrush)
   CASE nMsg == WM_INITDIALOG
        IF tcolor==NIL
           tcolor:=RGB(255,0,0)
           rcolor:=RGB(0,0,255)
        END
        tbrush:=CreateSolidBrush(tcolor)
        rbrush:=CreateSolidBrush(rcolor)
        checkradiobutton(hdlg,101,104,100+type)
        checkdlgbutton(hdlg,105,Use3D)
        SetDlgItemText(hdlg,106,alltrim(str(steps)))
        checkdlgbutton(hdlg,107,stretch)
        checkdlgbutton(hdlg,108,trend)
        checkdlgbutton(hdlg,109,useref)
        SetDlgItemText(hdlg,110,alltrim(str(ref)))
        checkradiobutton(hdlg,111,112,if(stacked,111,112))
        checkdlgbutton(hdlg,113,cluster)
//        SetDlgItemText(hdlg,114,alltrim(str(maxrec)))
        SetDlgItemText(hdlg,128,alltrim(str(tthick)))
        SetDlgItemText(hdlg,132,alltrim(str(rthick)))


        checkdlgbutton(hdlg,118,lxLabels)
        checkdlgbutton(hdlg,119,lyLabels)
        checkdlgbutton(hdlg,120,lLegends)
        checkdlgbutton(hdlg,121,lCage)
        checkdlgbutton(hdlg,122,lxGrid)
        checkdlgbutton(hdlg,123,lyGrid)
        checkdlgbutton(hdlg,124,lReversed)
        checkdlgbutton(hdlg,125,lOutline)
        checkdlgbutton(hdlg,127,lTop3)
        checkdlgbutton(hdlg,126,lData)

        spinedit(hdlg,106)
        spinedit(hdlg,114)
        return(1)
  case nMsg==WM_CTLCOLOREDIT
       DO CASE
          CASE GetDlgCtrlId(nlParam)==131
               SetBkColor(nwParam,tcolor)
               return(tbrush)
          CASE GetDlgCtrlId(nlParam)==134
               SetBkColor(nwParam,rcolor)
               return(rbrush)
       END
  case nMsg==WM_COMMAND
       do case
          case nwParam==IDCANCEL
               enddialog(hdlg,IDCANCEL)

          case nwParam==IDOK
               type :=readradiobutton(hdlg,101,104)-100
               Use3D:=isDlgButtonChecked(hdlg,105)>0
               steps:=Int(val(getdlgitemtext(hdlg,106)))
               stretch:=isdlgbuttonchecked(hdlg,107)>0
               trend  :=isdlgbuttonchecked(hdlg,108)>0
               useref :=(isdlgbuttonchecked(hdlg,109)>0)
               ref:=Int(val(getdlgitemtext(hdlg,110)))
               stacked:=(readradiobutton(hdlg,111,112)==111)
               cluster:=(isdlgbuttonchecked(hdlg,113)>0)
               maxrec:=Int(val(getdlgitemtext(hdlg,114)))
               tthick:=Int(val(getdlgitemtext(hdlg,128)))
               rthick:=Int(val(getdlgitemtext(hdlg,132)))

               lxLabels :=isDlgButtonChecked(hdlg,118)>0
               lyLabels :=isDlgButtonChecked(hdlg,119)>0
               lLegends :=isDlgButtonChecked(hdlg,120)>0
               lCage    :=isDlgButtonChecked(hdlg,121)>0
               lxGrid   :=isDlgButtonChecked(hdlg,122)>0
               lyGrid   :=isDlgButtonChecked(hdlg,123)>0
               lReversed:=isDlgButtonChecked(hdlg,124)>0
               lOutLine :=isDlgButtonChecked(hdlg,125)>0
               lData    :=isDlgButtonChecked(hdlg,126)>0
               lTop3    :=isDlgButtonChecked(hdlg,127)>0

               GraphIt(hWnd)

          case nwParam==106 //steps
               do case
                  case hiword(nlParam)==SB_LINEUP
                       ntemp:=Int(val(getdlgitemtext(hdlg,106)))
                       ntemp:=max(0,ntemp)
                       ntemp:=min(ntemp+1,99)
                       SetDlgItemText(hdlg,106,alltrim(str(ntemp)))

                  case hiword(nlParam)==SB_LINEDOWN
                       ntemp:=Int(val(getdlgitemtext(hdlg,106)))
                       ntemp:=max(2,ntemp)
                       ntemp:=min(ntemp-1,99)
                       SetDlgItemText(hdlg,106,alltrim(str(ntemp)))

                  case hiword(nlParam)==EN_KILLFOCUS
                       ntemp:=Int(val(getdlgitemtext(hdlg,106)))
                       ntemp:=max(2,ntemp)
                       ntemp:=min(ntemp,99)
                       SetDlgItemText(hdlg,106,alltrim(str(ntemp)))

               endcase
          case nwParam==114 //max records
               do case
                  case hiword(nlParam)==SB_LINEUP
                       ntemp:=Int(val(getdlgitemtext(hdlg,114)))
                       ntemp:=max(0,ntemp)
                       ntemp:=min(ntemp+1,4096)
                       SetDlgItemText(hdlg,114,alltrim(str(ntemp)))

                  case hiword(nlParam)==SB_LINEDOWN
                       ntemp:=Int(val(getdlgitemtext(hdlg,114)))
                       ntemp:=max(2,ntemp)
                       ntemp:=min(ntemp-1,4096)
                       SetDlgItemText(hdlg,114,alltrim(str(ntemp)))
           
                  case hiword(nlParam)==EN_KILLFOCUS
                       ntemp:=Int(val(getdlgitemtext(hdlg,114)))
                       ntemp:=max(2,ntemp)
                       ntemp:=min(ntemp,4096)
                       SetDlgItemText(hdlg,114,alltrim(str(ntemp)))
              
               endcase
          case nwParam==110 // reference line value
               //quick and dirty, user unfriendly numerical input
               ntemp:=Int(val(getdlgitemtext(hdlg,110)))
               SetDlgItemText(hdlg,110,alltrim(str(ntemp)))

          case nwParam==109
               enablewindow(getdlgitem(hdlg,110),(isdlgbuttonchecked(hdlg,109)>0))

          case nwParam==111
               checkdlgbutton(hdlg,113,.F.)
               enablewindow(getdlgitem(hdlg,113),.F.)
 
          case nwParam==112
               enablewindow(getdlgitem(hdlg,113),.T.)

          case nwParam==129
               IF (n := ChooseColor( hDlg,tcolor,,))!=NIL
                  tcolor:=n
                  DeleteObject(tbrush)
                  tbrush:=CreateSolidBrush(tcolor)
                  InvalidateRect(GetDlgItem(hDlg,131))
               END
       endcase
endcase
return(0)

*----------------------------------------------------------------------------*
function Graphit(hparent)

local oGraph:=Graph()
local oTitle    :=Text()
local oSubtitle :=Text()
local oSmallText:=Text()
local oXaxis    :=Text()
local oYaxis    :=Text()
local oGraphObj
local adata
local alabels
local acolors

alabels:={'January','February','March','April','May','June'}
adata  :={{3,9,1},{15,2,4},{4,6,5},{9,1,4},{4,3,8},{1,2,9}}
aseries:={'A','B','C'}
acolors:={C_RED,C_BLUE,C_GREEN}

  if empty(aseries)
    messagebox(hparent,'No data series defined','Problem',MB_OK+MB_ICONSTOP)
  else
    if empty(adata)
      messagebox(hparent,'No data found','Problem',MB_OK+MB_ICONSTOP)
    else

      if type==4
         if len(adata) >= 16
           MessageBox(hParent,'Pie can only plot 15 data items','I')
           // the rest should be accumulated as "Other"
           return(NIL)
         endif
         adata  :={20,30,20,10,20}
         acolors:= { C_RED,C_BLUE,C_GREEN,C_LIGHTCYAN,C_PURPLE} 
         alabels:={'Label 1','Label 2','Label 3','Label 4','Label 5'}

         oGraphObj:=PieChart():Init(hParent)
         oGraphObj:SetRect({100,140,1450,900})
         oGraphObj:SetData(adata)
         oGraphObj:SetType(Use3D)
         oGraphObj:SetLegends(alabels)
         oGraphObj:setcolors(aColors)
         oGraphObj:blacklines:=.f.
         oGraphObj:Explode(1,10)
         oGraphObj:Explode(2,10)
            oGraphObj:Explode(3,10)
         oGraphObj:Explode(4,10)
         oGraphObj:Explode(5,10)

      else
         if len(adata[1]) > 16
           MessageBox(hparent,'There can be a maximum of 16 data series','I')
           return(NIL)
         endif
         oGraphObj:=BarGraph():Init(hparent)
         oGraphObj:SetRect({100,140,1450,900})
         oGraphObj:SetData(adata)
         oGraphObj:SetXLabels(alabels)
         oGraphObj:SetType(type,Use3D,cluster,stacked)
         oGraphObj:SetColors(acolors)
         if trend
           oGraphObj:SetRefLine(0,0,tthick,tcolor,'Trend')
         endif
         if UseRef
           oGraphObj:SetRefLine(ref,ref,rthick,rcolor,'Reference')
         endif
         oGraphObj:SetLegends(aseries)
         oGraphObj:SetScale(0,0,steps,.t.,stretch)

         oGraphObj:wantXLabels:=lxLabels
         oGraphObj:wantYLabels:=lyLabels
         oGraphObj:wantLegends:=lLegends
         oGraphObj:wantCage   :=lCage
         oGraphObj:wantXGrid  :=lxGrid  
         oGraphObj:wantYGrid  :=lyGrid  
         oGraphObj:Reversed   :=lReversed
         oGraphObj:ShowData   :=lData
         oGraphObj:Reversed   :=lReversed
         oGraphObj:BlackLines :=lOutline
         
//         lTop3    :=isDlgButtonChecked(hdlg,127)>0
      endif

      oGraph:New('Test Graph from '+dbffile)
      oGraph:addGraph(oGraphObj)

      oGraph:AddText(oTitle:New(title,{10,10,1490,65}))
      oTitle:setalign('C')
      oTitle:color:=C_CYAN
      oTitle:font[5]:=700
      oTitle:font[7]:=.T.
      
      oGraph:AddText(oSubTitle:New(subtitle,{10,65,1490,105}))
      oSubTitle:SetAlign('C')
      oSubtitle:color:=C_CYAN
      oTitle:font[5]:=700
      oTitle:SetFontSize(40)
       
      oGraph:AddText(oSmallText:New(SmallTxt,{10,110,1490,135}))
      oSmallText:SetAlign('C')
      oSmallText:color:=C_CYAN

      oGraph:AddText(oXaxis:New(labelsin,{10,960,1490,990}))
      oXAxis:SetAlign('C')
      oXAxis:SetOrient('H')
      oXAxis:color:=C_RED
      oXAxis:font[5]:=700
      oXAxis:setfontsize(40)

      oGraph:AddText(oYaxis:New(yAxis,{10,100,50,900}))
      oYAxis:SetAlign('C')
      oYAxis:SetOrient('V')
      oYAxis:color:=C_RED
      oYAxis:font[5]:=700
      oYAxis:setfontsize(40)
       
      oGraph:Show(hParent,100,100,600,400)

      endif
  endif

return(NIL)

*----------------------------------------------------------------------------*

function ReadRadioButton(hDlg,nStartId,nEndId)
local i
  for i:=nStartId to nEndId
      if IsDlgButtonChecked(hdlg,i) == 1
        exit
      endif
   next
return(i)

*----------------------------------------------------------------------------*

static Function spinedit(hDlg,id)
Local nproc:=SetProcedure(GetDlgItem(hDlg,id),;
     {|hWnd,nMsg,nwParam,nlParam| _spineditproc(nproc,hWnd,nMsg,nwParam,nlParam,hDlg,id)},;
      {WM_VSCROLL,WM_HSCROLL})
  SetScrollRange(GetDlgItem(hDlg,id),SB_CTL,1,100,0)
Return(NIL)

*----------------------------------------------------------------------------*

static Function _spineditproc(nproc,hWnd,nMsg,nwParam,nlParam,hDlg,id)
  If (nMsg==WM_VSCROLL .or. nMsg==WM_HSCROLL) .and. ;
    (nwParam==SB_LINEUP .or. nwParam==SB_LINEDOWN)
    SetFocus(hWnd)
    SendMessage(hDlg,WM_COMMAND,id,makelong(hWnd,nwParam))
  EndIf
Return(CallWindowProc(nproc,hWnd,nMsg,nwParam,nlParam))


*----------------------------------------------------------------------------*

