/*
 *$Id: windows.ch,v 1.4 2003/12/29 20:20:43 lculik Exp $
 */

#define WM_CREATE                       1
#define WM_DESTROY                      2
#define WM_MOVE                         3
#define WM_SIZE                         5
#define WM_ACTIVATE                     6
#define WM_SETFOCUS                     7
#define WM_KILLFOCUS                    8
#define WM_ENABLE                       10
#define WM_SETREDRAW                    11
#define WM_SETTEXT                      12
#define WM_GETTEXT                      13
#define WM_GETTEXTLENGTH                14
#define WM_PAINT                        15
#define WM_CLOSE                        16   // 0x0010

#define WM_ERASEBKGND                   20   // 0x0014
#define WM_ENDSESSION                   22   // 0x0016
#define WM_GETMINMAXINFO                36   // 0x0024
#define WM_DRAWITEM                     43   // 0x002B
#define WM_SETFONT                      48   // 0x0030

#define WM_WINDOWPOSCHANGING            70   // 0x0046

#define WM_NOTIFY                       78   // 0x004E

#define WM_SETICON                      128    // 0x0080

#define WM_NCCREATE                     129
#define WM_NCDESTROY                    130
#define WM_NCCALCSIZE                   131
#define WM_NCHITTEST                    132
#define WM_NCPAINT                      133
#define WM_NCACTIVATE                   134
#define WM_GETDLGCODE                   135

#define WM_KEYDOWN                      256    // 0x0100
#define WM_KEYUP                        257    // 0x0101
#define WM_CHAR                         258    // 0x0102

#define WM_INITDIALOG                   272
#define WM_COMMAND                      273
#define WM_SYSCOMMAND                   274
#define WM_TIMER                        275
#define WM_HSCROLL                      276
#define WM_VSCROLL                      277
#define WM_ENTERIDLE                    289

#define WM_CTLCOLORMSGBOX               306     // 0x0132
#define WM_CTLCOLOREDIT                 307     // 0x0133
#define WM_CTLCOLORLISTBOX              308     // 0x0134
#define WM_CTLCOLORBTN                  309     // 0x0135
#define WM_CTLCOLORDLG                  310     // 0x0136
#define WM_CTLCOLORSCROLLBAR            311     // 0x0137
#define WM_CTLCOLORSTATIC               312     // 0x0138

#define WM_MOUSEMOVE                    512    // 0x0200
#define WM_LBUTTONDOWN                  513    // 0x0201
#define WM_LBUTTONUP                    514    // 0x0202
#define WM_LBUTTONDBLCLK                515    // 0x0203
#define WM_RBUTTONDOWN                  516    // 0x0204
#define WM_RBUTTONUP                    517    // 0x0205

#define WM_MDICREATE                    544     // 0x0220
#define WM_MDIDESTROY                   545     // 0x0221
#define WM_MDIACTIVATE                  546     // 0x0222
#define WM_MDIRESTORE                   547     // 0x0223
#define WM_MDINEXT                      548     // 0x0224
#define WM_MDIMAXIMIZE                  549     // 0x0225
#define WM_MDITILE                      550     // 0x0226
#define WM_MDICASCADE                   551     // 0x0227
#define WM_MDIICONARRANGE               552     // 0x0228
#define WM_MDIGETACTIVE                 553     // 0x0229
#define WM_ENTERSIZEMOVE                561     // 0x0231

#define WM_CUT                          768     // 0x0300
#define WM_COPY                         769     // 0x0301
#define WM_PASTE                        770     // 0x0302
#define WM_CLEAR                        771     // 0x0303

#define WM_USER                        1024    // 0x0400

#define SC_MINIMIZE                   61472   // 0xF020
#define SC_MAXIMIZE                   61488   // 0xF030
#define SC_CLOSE                      61536   // 0xF060
#define SC_RESTORE                    61728   // 0xF120

/*
 * Dialog Box Command IDs
 */
#define IDOK                1
#define IDCANCEL            2
#define IDABORT             3
#define IDRETRY             4
#define IDIGNORE            5
#define IDYES               6
#define IDNO                7

#define DS_CENTER           2048     // 0x0800L

/*
 * User Button Notification Codes
 */
#define BN_CLICKED          0
#define BN_PAINT            1
#define BN_HILITE           2
#define BN_UNHILITE         3
#define BN_DISABLE          4
#define BN_DOUBLECLICKED    5
#define BN_PUSHED           BN_HILITE
#define BN_UNPUSHED         BN_UNHILITE
#define BN_DBLCLK           BN_DOUBLECLICKED
#define BN_SETFOCUS         6
#define BN_KILLFOCUS        7

/*
 * Edit Control Notification Codes
 */
#define EN_SETFOCUS         256    // 0x0100
#define EN_KILLFOCUS        512    // 0x0200
#define EN_CHANGE           768    // 0x0300
#define EN_UPDATE           1024   // 0x0400
#define EN_ERRSPACE         1280   // 0x0500
#define EN_MAXTEXT          1281   // 0x0501
#define EN_HSCROLL          1537   // 0x0601
#define EN_VSCROLL          1538   // 0x0602
#define EN_SELCHANGE        1794   // 0x0702

/*
 * Combo Box messages
 */
#define CB_GETEDITSEL               320
#define CB_LIMITTEXT                321
#define CB_SETEDITSEL               322
#define CB_ADDSTRING                323
#define CB_DELETESTRING             324
#define CB_DIR                      325
#define CB_GETCOUNT                 326
#define CB_GETCURSEL                327
#define CB_GETLBTEXT                328
#define CB_GETLBTEXTLEN             329
#define CB_INSERTSTRING             330
#define CB_RESETCONTENT             331
#define CB_FINDSTRING               332
#define CB_SELECTSTRING             333
#define CB_SETCURSEL                334

/* Brush Styles */
#define BS_SOLID            0
#define BS_NULL             1
#define BS_HOLLOW           BS_NULL
#define BS_HATCHED          2
#define BS_PATTERN          3
#define BS_INDEXED          4
#define BS_DIBPATTERN       5
#define BS_DIBPATTERNPT     6
#define BS_PATTERN8X8       7
#define BS_DIBPATTERN8X8    8
#define BS_MONOPATTERN      9

/* Pen Styles */
#define PS_SOLID            0
#define PS_DASH             1       /* -------  */
#define PS_DOT              2       /* .......  */
#define PS_DASHDOT          3       /* _._._._  */
#define PS_DASHDOTDOT       4       /* _.._.._  */
#define PS_NULL             5
#define PS_INSIDEFRAME      6
#define PS_USERSTYLE        7
#define PS_ALTERNATE        8
#define PS_STYLE_MASK       15

#define COLOR_SCROLLBAR         0
#define COLOR_BACKGROUND        1
#define COLOR_ACTIVECAPTION     2
#define COLOR_INACTIVECAPTION   3
#define COLOR_MENU              4
#define COLOR_WINDOW            5
#define COLOR_WINDOWFRAME       6
#define COLOR_MENUTEXT          7
#define COLOR_WINDOWTEXT        8
#define COLOR_CAPTIONTEXT       9
#define COLOR_ACTIVEBORDER      10
#define COLOR_INACTIVEBORDER    11
#define COLOR_APPWORKSPACE      12
#define COLOR_HIGHLIGHT         13
#define COLOR_HIGHLIGHTTEXT     14
#define COLOR_BTNFACE           15
#define COLOR_BTNSHADOW         16
#define COLOR_GRAYTEXT          17
#define COLOR_BTNTEXT           18
#define COLOR_INACTIVECAPTIONTEXT 19
#define COLOR_BTNHIGHLIGHT      20

#define COLOR_3DDKSHADOW        21
#define COLOR_3DLIGHT           22
#define COLOR_INFOTEXT          23
#define COLOR_INFOBK            24

#define COLOR_HOTLIGHT          26
#define COLOR_GRADIENTACTIVECAPTION 27
#define COLOR_GRADIENTINACTIVECAPTION 28

#define COLOR_DESKTOP           COLOR_BACKGROUND
#define COLOR_3DFACE            COLOR_BTNFACE
#define COLOR_3DSHADOW          COLOR_BTNSHADOW
#define COLOR_3DHIGHLIGHT       COLOR_BTNHIGHLIGHT
#define COLOR_3DHILIGHT         COLOR_BTNHIGHLIGHT
#define COLOR_BTNHILIGHT        COLOR_BTNHIGHLIGHT

/*
 * DrawText() Format Flags
 */
#define DT_TOP                      0
#define DT_LEFT                     0
#define DT_CENTER                   1
#define DT_RIGHT                    2
#define DT_VCENTER                  4
#define DT_BOTTOM                   8
#define DT_WORDBREAK                16
#define DT_SINGLELINE               32
#define DT_EXPANDTABS               64
#define DT_TABSTOP                  128
#define DT_NOCLIP                   256
#define DT_EXTERNALLEADING          512
#define DT_CALCRECT                 1024
#define DT_NOPREFIX                 2048
#define DT_INTERNAL                 4096

#define DT_EDITCONTROL              8192
#define DT_PATH_ELLIPSIS            16384
#define DT_END_ELLIPSIS             32768
#define DT_MODIFYSTRING             65536
#define DT_RTLREADING               131072
#define DT_WORD_ELLIPSIS            262144
#define DT_NOFULLWIDTHCHARBREAK     524288
#define DT_HIDEPREFIX               1048576
#define DT_PREFIXONLY               2097152

/*
 * Scroll Bar Commands
 */
#define SB_LINEUP           0
#define SB_LINELEFT         0
#define SB_LINEDOWN         1
#define SB_LINERIGHT        1
#define SB_PAGEUP           2
#define SB_PAGELEFT         2
#define SB_PAGEDOWN         3
#define SB_PAGERIGHT        3
#define SB_THUMBPOSITION    4
#define SB_THUMBTRACK       5
#define SB_TOP              6
#define SB_LEFT             6
#define SB_BOTTOM           7
#define SB_RIGHT            7
#define SB_ENDSCROLL        8

/*
 * Edit Control Styles
 */
#define ES_LEFT             0
#define ES_CENTER           1
#define ES_RIGHT            2
#define ES_MULTILINE        4
#define ES_UPPERCASE        8
#define ES_LOWERCASE        16
#define ES_PASSWORD         32
#define ES_AUTOVSCROLL      64
#define ES_AUTOHSCROLL      128
#define ES_NOHIDESEL        256
#define ES_OEMCONVERT       1024
#define ES_READONLY         2048       // 0x0800L
#define ES_WANTRETURN       4096       // 0x1000L
#define ES_NUMBER           8192       // 0x2000L

/*
 * Window Styles
 */
#define WS_OVERLAPPED       0
#define WS_POPUP            2147483648 // 0x80000000L
#define WS_CHILD            1073741824 // 0x40000000L
#define WS_MINIMIZE         536870912  // 0x20000000L
#define WS_VISIBLE          268435456  // 0x10000000L
#define WS_DISABLED         134217728  // 0x08000000L
#define WS_CLIPSIBLINGS     67108864   // 0x04000000L
#define WS_CAPTION          12582912   // 0x00C00000L
#define WS_BORDER           8388608    // 0x00800000L
#define WS_DLGFRAME         4194304    // 0x00400000L
#define WS_EX_STATICEDGE    131072     // 0x00020000L
#define WS_VSCROLL          2097152    // 0x00200000L
#define WS_HSCROLL          1048576    // 0x00100000L
#define WS_SYSMENU          524288     // 0x00080000L
#define WS_THICKFRAME       262144     // 0x00040000L
#define WS_GROUP            131072     // 0x00020000L
#define WS_TABSTOP          65536      // 0x00010000L
#define WS_MINIMIZEBOX      131072     // 0x00020000L
#define WS_MAXIMIZEBOX      65536      // 0x00010000L
#define WS_SIZEBOX          WS_THICKFRAME

#define WS_EX_DLGMODALFRAME     1      // 0x00000001L
#define WS_EX_NOPARENTNOTIFY    4      // 0x00000004L
#define WS_EX_TOPMOST           8      // 0x00000008L
#define WS_EX_ACCEPTFILES      16      // 0x00000010L
#define WS_EX_TRANSPARENT      32      // 0x00000020L


#define RDW_INVALIDATE          1      // 0x0001
#define RDW_INTERNALPAINT       2      // 0x0002
#define RDW_ERASE               4      // 0x0004
#define RDW_VALIDATE            8      // 0x0008
#define RDW_NOINTERNALPAINT     16     // 0x0010
#define RDW_NOERASE             32     // 0x0020
#define RDW_NOCHILDREN          64     // 0x0040
#define RDW_ALLCHILDREN         128    // 0x0080
#define RDW_UPDATENOW           256    // 0x0100
#define RDW_ERASENOW            512    // 0x0200

/*
 * Static Control Constants
 */
#define SS_LEFT                   0    // 0x00000000L
#define SS_CENTER                 1    // 0x00000001L
#define SS_RIGHT                  2    // 0x00000002L
#define SS_ICON                   3    // 0x00000003L
#define SS_BLACKRECT              4    // 0x00000004L
#define SS_GRAYRECT               5    // 0x00000005L
#define SS_WHITERECT              6    // 0x00000006L
#define SS_BLACKFRAME             7    // 0x00000007L
#define SS_GRAYFRAME              8    // 0x00000008L
#define SS_WHITEFRAME             9    // 0x00000009L
#define SS_USERITEM              10    // 0x0000000AL
#define SS_SIMPLE                11    // 0x0000000BL
#define SS_LEFTNOWORDWRAP        12    // 0x0000000CL
#define SS_OWNERDRAW             13    // 0x0000000DL
#define SS_BITMAP                14    // 0x0000000EL
#define SS_ENHMETAFILE           15    // 0x0000000FL
#define SS_ETCHEDHORZ            16    // 0x00000010L
#define SS_ETCHEDVERT            17    // 0x00000011L
#define SS_ETCHEDFRAME           18    // 0x00000012L
#define SS_TYPEMASK              31    // 0x0000001FL
#define SS_NOTIFY               256    // 0x00000100L
#define SS_CENTERIMAGE          512    // 0x00000200L
#define SS_RIGHTJUST           1024    // 0x00000400L
#define SS_REALSIZEIMAGE       2048    // 0x00000800L
#define SS_SUNKEN              4096    // 0x00001000L

/*
 * Button Control Styles
 */
#define BS_PUSHBUTTON       0       // 0x00000000L
#define BS_DEFPUSHBUTTON    1       // 0x00000001L
#define BS_CHECKBOX         2       // 0x00000002L
#define BS_AUTOCHECKBOX     3       // 0x00000003L
#define BS_RADIOBUTTON      4       // 0x00000004L
#define BS_3STATE           5       // 0x00000005L
#define BS_AUTO3STATE       6       // 0x00000006L
#define BS_GROUPBOX         7       // 0x00000007L
#define BS_USERBUTTON       8       // 0x00000008L
#define BS_AUTORADIOBUTTON  9       // 0x00000009L
#define BS_OWNERDRAW        11      // 0x0000000BL
#define BS_LEFTTEXT         32      // 0x00000020L

#define IDC_ARROW           32512
#define IDC_IBEAM           32513
#define IDC_WAIT            32514
#define IDC_CROSS           32515
#define IDC_SIZEWE          32644
#define IDC_SIZENS          32645
#define IDC_UPARROW         32516

/*
 * Key State Masks for Mouse Messages
 */
#define MK_LBUTTON          1       // 0x0001
#define MK_RBUTTON          2       // 0x0002
#define MK_SHIFT            4       // 0x0004
#define MK_CONTROL          8       // 0x0008
#define MK_MBUTTON          16      // 0x0010

/* Ternary raster operations */
#define SRCCOPY             13369376   /* 0x00CC0020  dest = source          */
#define SRCPAINT            0          /* 0x00EE0086  dest = source OR dest  */
#define SRCAND              8913094    /* 0x008800C6  dest = source AND dest */
// #define SRCINVERT           0          /* 0x00660046  dest = source XOR dest */
// #define SRCERASE            0x00440328 /* dest = source AND (NOT dest )   */
// #define NOTSRCCOPY          0x00330008 /* dest = (NOT source)             */
// #define NOTSRCERASE         0x001100A6 /* dest = (NOT src) AND (NOT dest) */
#define MERGECOPY           12583114      /* 0x00C000CA dest = (source AND pattern) */
#define MERGEPAINT          12255782      /* 0x00BB0226 dest = (NOT source) OR dest */
// #define PATCOPY             0x00F00021 /* dest = pattern                  */
// #define PATPAINT            0x00FB0A09 /* dest = DPSnoo                   */
// #define PATINVERT           0x005A0049 /* dest = pattern XOR dest         */
// #define DSTINVERT           0x00550009 /* dest = (NOT dest)               */
// #define BLACKNESS           0x00000042 /* dest = BLACK                    */
// #define WHITENESS           0x00FF0062 /* dest = WHITE                    */

#define PSN_SETACTIVE           -200   // (PSN_FIRST-0)
#define PSN_KILLACTIVE          -201   // (PSN_FIRST-1)
#define PSN_APPLY               -202   // (PSN_FIRST-2)
#define PSN_RESET               -203   // (PSN_FIRST-3)
#define PSN_HELP                -205   // (PSN_FIRST-5)
#define PSN_WIZBACK             -206   // (PSN_FIRST-6)
#define PSN_WIZNEXT             -207   // (PSN_FIRST-7)
#define PSN_WIZFINISH           -208   // (PSN_FIRST-8)
#define PSN_QUERYCANCEL         -209   // (PSN_FIRST-9)
/*
#define TCN_SELCHANGE           -551   // (TCN_FIRST - 1)
*/
/*
 * Combo Box styles
 */
#define CBS_SIMPLE            1        // 0x0001L
#define CBS_DROPDOWN          2        // 0x0002L
#define CBS_DROPDOWNLIST      3        // 0x0003L

/*
 * MessageBox() Flags
 */
#define MB_OK                 0        // 0x00000000L
#define MB_OKCANCEL           1        // 0x00000001L
#define MB_ABORTRETRYIGNORE   2        // 0x00000002L
#define MB_YESNOCANCEL        3        // 0x00000003L
#define MB_YESNO              4        // 0x00000004L
#define MB_RETRYCANCEL        5        // 0x00000005L
#define MB_ICONHAND           16       // 0x00000010L
#define MB_ICONQUESTION       32       // 0x00000020L
#define MB_ICONEXCLAMATION    48       // 0x00000030L
#define MB_ICONASTERISK       64       // 0x00000040L

#define MB_USERICON           128      // 0x00000080L
#define MB_NOFOCUS            32768    // 0x00008000L
#define MB_SETFOREGROUND      65536    // 0x00010000L
#define MB_DEFAULT_DESKTOP_ONLY  131072 // 0x00020000L

#define MB_TOPMOST            262144   // 0x00040000L
#define MB_RIGHT              524288   // 0x00080000L
#define MB_RTLREADING         1048576  // 0x00100000L


#define HKEY_CLASSES_ROOT     2147483648       // 0x80000000
#define HKEY_CURRENT_USER     2147483649       // 0x80000001
#define HKEY_LOCAL_MACHINE    2147483650       // 0x80000002
#define HKEY_USERS            2147483651       // 0x80000003
#define HKEY_PERFORMANCE_DATA 2147483652       // 0x80000004
#define HKEY_CURRENT_CONFIG   2147483653       // 0x80000005
#define HKEY_DYN_DATA         2147483654       // 0x80000006

#define MDITILE_VERTICAL       0
#define MDITILE_HORIZONTAL     1

/*
 * OEM Resource Ordinal Numbers
 */
#define OBM_CLOSE           32754
#define OBM_UPARROW         32753
#define OBM_DNARROW         32752
#define OBM_RGARROW         32751
#define OBM_LFARROW         32750
#define OBM_REDUCE          32749
#define OBM_ZOOM            32748
#define OBM_RESTORE         32747
#define OBM_REDUCED         32746
#define OBM_ZOOMD           32745
#define OBM_RESTORED        32744
#define OBM_UPARROWD        32743
#define OBM_DNARROWD        32742
#define OBM_RGARROWD        32741
#define OBM_LFARROWD        32740
#define OBM_MNARROW         32739
#define OBM_COMBO           32738
#define OBM_UPARROWI        32737
#define OBM_DNARROWI        32736
#define OBM_RGARROWI        32735
#define OBM_LFARROWI        32734

#define OBM_OLD_CLOSE       32767
#define OBM_SIZE            32766
#define OBM_OLD_UPARROW     32765
#define OBM_OLD_DNARROW     32764
#define OBM_OLD_RGARROW     32763
#define OBM_OLD_LFARROW     32762
#define OBM_BTSIZE          32761
#define OBM_CHECK           32760
#define OBM_CHECKBOXES      32759
#define OBM_BTNCORNERS      32758
#define OBM_OLD_REDUCE      32757
#define OBM_OLD_ZOOM        32756
#define OBM_OLD_RESTORE     32755

/*
#define TCS_SCROLLOPPOSITE      1       // 0x0001   // assumes multiline tab
#define TCS_BOTTOM              2       // 0x0002
#define TCS_RIGHT               2       // 0x0002
#define TCS_MULTISELECT         4       // 0x0004  // allow multi-select in button mode
#define TCS_FLATBUTTONS         8       // 0x0008
#define TCS_FORCEICONLEFT       16      // 0x0010
#define TCS_FORCELABELLEFT      32      // 0x0020
#define TCS_HOTTRACK            64      // 0x0040
#define TCS_VERTICAL            128     // 0x0080
#define TCS_TABS                0       // 0x0000
#define TCS_BUTTONS             256     // 0x0100
#define TCS_SINGLELINE          0       // 0x0000
#define TCS_MULTILINE           512     // 0x0200
#define TCS_RIGHTJUSTIFY        0       // 0x0000
#define TCS_FIXEDWIDTH          1024    // 0x0400
#define TCS_RAGGEDRIGHT         2048    // 0x0800
#define TCS_FOCUSONBUTTONDOWN   4096    // 0x1000
#define TCS_OWNERDRAWFIXED      8192    // 0x2000
#define TCS_TOOLTIPS            16384   // 0x4000
#define TCS_FOCUSNEVER          32768   // 0x8000
*/
#define EM_GETSEL               176     // 0x00B0
#define EM_SETSEL               177     // 0x00B1
#define EM_GETRECT              178     // 0x00B2
#define EM_SETRECT              179     // 0x00B3
#define EM_SETRECTNP            180     // 0x00B4
#define EM_SCROLL               181     // 0x00B5
#define EM_LINESCROLL           182     // 0x00B6
#define EM_SCROLLCARET          183     // 0x00B7
#define EM_GETMODIFY            184     // 0x00B8
#define EM_SETMODIFY            185     // 0x00B9
#define EM_GETLINECOUNT         186     // 0x00BA
#define EM_LINEINDEX            187     // 0x00BB
#define EM_SETHANDLE            188     // 0x00BC
#define EM_GETHANDLE            189     // 0x00BD
#define EM_GETTHUMB             190     // 0x00BE
#define EM_LINELENGTH           193     // 0x00C1
#define EM_REPLACESEL           194     // 0x00C2
#define EM_GETLINE              196     // 0x00C4
#define EM_LIMITTEXT            197     // 0x00C5
#define EM_CANUNDO              198     // 0x00C6
#define EM_UNDO                 199     // 0x00C7
#define EM_FMTLINES             200     // 0x00C8
#define EM_LINEFROMCHAR         201     // 0x00C9
#define EM_SETTABSTOPS          203     // 0x00CB
#define EM_SETPASSWORDCHAR      204     // 0x00CC
#define EM_EMPTYUNDOBUFFER      205     // 0x00CD
#define EM_GETFIRSTVISIBLELINE  206     // 0x00CE
#define EM_SETREADONLY          207     // 0x00CF
#define EM_SETWORDBREAKPROC     208     // 0x00D0
#define EM_GETWORDBREAKPROC     209     // 0x00D1
#define EM_GETPASSWORDCHAR      210     // 0x00D2
#define EM_SETMARGINS           211     // 0x00D3
#define EM_GETMARGINS           212     // 0x00D4
#define EM_SETLIMITTEXT         EM_LIMITTEXT
#define EM_GETLIMITTEXT         213     // 0x00D5
#define EM_POSFROMCHAR          214     // 0x00D6
#define EM_CHARFROMPOS          215     // 0x00D7
#define EM_SETBKGNDCOLOR       1091
#define EM_SETEVENTMASK        1093     // (WM_USER + 69)

#define ENM_CHANGE             1        // 0x00000001
#define ENM_SELCHANGE          524288   // 0x00080000

#define IMAGE_BITMAP        0
#define IMAGE_ICON          1
#define IMAGE_CURSOR        2

#define LR_DEFAULTCOLOR         0
#define LR_MONOCHROME           1
#define LR_COLOR                2
#define LR_COPYRETURNORG        4
#define LR_COPYDELETEORG        8
#define LR_LOADFROMFILE        16       // 0x0010
#define LR_LOADTRANSPARENT     32       // 0x0020
#define LR_DEFAULTSIZE         64       // 0x0040
#define LR_VGACOLOR           128       // 0x0080
#define LR_LOADMAP3DCOLORS   4096       // 0x1000
#define LR_CREATEDIBSECTION  8192       // 0x2000
#define LR_COPYFROMRESOURCE 16384       // 0x4000
#define LR_SHARED           32768       // 0x8000

/* Stock Logical Objects */
#define WHITE_BRUSH         0
#define LTGRAY_BRUSH        1
#define GRAY_BRUSH          2
#define DKGRAY_BRUSH        3
#define BLACK_BRUSH         4
#define NULL_BRUSH          5
#define WHITE_PEN           6
#define BLACK_PEN           7
#define NULL_PEN            8
#define OEM_FIXED_FONT      10
#define ANSI_FIXED_FONT     11
#define ANSI_VAR_FONT       12
#define SYSTEM_FONT         13
#define DEVICE_DEFAULT_FONT 14
#define DEFAULT_PALETTE     15
#define SYSTEM_FIXED_FONT   16
#define DEFAULT_GUI_FONT    17

/* 3D border styles */
#define BDR_RAISEDOUTER     1           // 0x0001
#define BDR_SUNKENOUTER     2           // 0x0002
#define BDR_RAISEDINNER     4           // 0x0004
#define BDR_SUNKENINNER     8           // 0x0008

#define BDR_OUTER       (BDR_RAISEDOUTER + BDR_SUNKENOUTER)
#define BDR_INNER       (BDR_RAISEDINNER + BDR_SUNKENINNER)
#define BDR_RAISED      (BDR_RAISEDOUTER + BDR_RAISEDINNER)
#define BDR_SUNKEN      (BDR_SUNKENOUTER + BDR_SUNKENINNER)


#define EDGE_RAISED     (BDR_RAISEDOUTER + BDR_RAISEDINNER)
#define EDGE_SUNKEN     (BDR_SUNKENOUTER + BDR_SUNKENINNER)
#define EDGE_ETCHED     (BDR_SUNKENOUTER + BDR_RAISEDINNER)
#define EDGE_BUMP       (BDR_RAISEDOUTER + BDR_SUNKENINNER)

/* Border flags */
#define BF_LEFT             1           // 0x0001
#define BF_TOP              2           // 0x0002
#define BF_RIGHT            4           // 0x0004
#define BF_BOTTOM           8           // 0x0008

#define BF_TOPLEFT      (BF_TOP + BF_LEFT)
#define BF_TOPRIGHT     (BF_TOP + BF_RIGHT)
#define BF_BOTTOMLEFT   (BF_BOTTOM + BF_LEFT)
#define BF_BOTTOMRIGHT  (BF_BOTTOM + BF_RIGHT)
#define BF_RECT         (BF_LEFT + BF_TOP + BF_RIGHT + BF_BOTTOM)

#define BF_DIAGONAL        16           // 0x0010

// For diagonal lines, the BF_RECT flags specify the end point of the
// vector bounded by the rectangle parameter.
#define BF_DIAGONAL_ENDTOPRIGHT     (BF_DIAGONAL + BF_TOP + BF_RIGHT)
#define BF_DIAGONAL_ENDTOPLEFT      (BF_DIAGONAL + BF_TOP + BF_LEFT)
#define BF_DIAGONAL_ENDBOTTOMLEFT   (BF_DIAGONAL + BF_BOTTOM + BF_LEFT)
#define BF_DIAGONAL_ENDBOTTOMRIGHT  (BF_DIAGONAL + BF_BOTTOM + BF_RIGHT)


#define BF_MIDDLE        2048           // 0x0800  /* Fill in the middle */
#define BF_SOFT          4096           // 0x1000  /* For softer buttons */
#define BF_ADJUST        8192           // 0x2000  /* Calculate the space left over */
#define BF_FLAT         16384           // 0x4000  /* For flat rather than 3D borders */
#define BF_MONO         32768           // 0x8000  /* For monochrome borders */


#define FSHIFT    4   // 0x04
#define FCONTROL  8   // 0x08
#define FALT     16   // 0x10

#define VK_SHIFT          0x10
#define VK_CONTROL        0x11
#define VK_MENU           0x12
#define VK_PAUSE          0x13
#define VK_CAPITAL        0x14

#define VK_SPACE          0x20
#define VK_PRIOR          0x21
#define VK_NEXT           0x22
#define VK_END            0x23
#define VK_HOME           0x24
#define VK_LEFT           0x25
#define VK_UP             0x26
#define VK_RIGHT          0x27
#define VK_DOWN           0x28
#define VK_SELECT         0x29
#define VK_PRINT          0x2A
#define VK_EXECUTE        0x2B
#define VK_SNAPSHOT       0x2C
#define VK_INSERT         0x2D
#define VK_DELETE         0x2E
#define VK_HELP           0x2F

/*
 * VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
 * 0x40 : unassigned
 * VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
 */

#define VK_LWIN           0x5B
#define VK_RWIN           0x5C
#define VK_APPS           0x5D

/*
 * 0x5E : reserved
 */

#define VK_SLEEP          0x5F

#define VK_NUMPAD0        0x60
#define VK_NUMPAD1        0x61
#define VK_NUMPAD2        0x62
#define VK_NUMPAD3        0x63
#define VK_NUMPAD4        0x64
#define VK_NUMPAD5        0x65
#define VK_NUMPAD6        0x66
#define VK_NUMPAD7        0x67
#define VK_NUMPAD8        0x68
#define VK_NUMPAD9        0x69
#define VK_MULTIPLY       0x6A
#define VK_ADD            0x6B
#define VK_SEPARATOR      0x6C
#define VK_SUBTRACT       0x6D
#define VK_DECIMAL        0x6E
#define VK_DIVIDE         0x6F
#define VK_F1             0x70
#define VK_F2             0x71
#define VK_F3             0x72
#define VK_F4             0x73
#define VK_F5             0x74
#define VK_F6             0x75
#define VK_F7             0x76
#define VK_F8             0x77
#define VK_F9             0x78
#define VK_F10            0x79
#define VK_F11            0x7A
#define VK_F12            0x7B
#define VK_F13            0x7C
#define VK_F14            0x7D
#define VK_F15            0x7E
#define VK_F16            0x7F
#define VK_F17            0x80
#define VK_F18            0x81
#define VK_F19            0x82
#define VK_F20            0x83
#define VK_F21            0x84
#define VK_F22            0x85
#define VK_F23            0x86
#define VK_F24            0x87

#define VK_NUMLOCK        0x90
#define VK_SCROLL         0x91

#define SW_HIDE             0
#define SW_SHOWNORMAL       1
#define SW_NORMAL           1
#define SW_SHOWMINIMIZED    2
#define SW_SHOWMAXIMIZED    3
#define SW_MAXIMIZE         3
#define SW_SHOWNOACTIVATE   4
#define SW_SHOW             5
#define SW_MINIMIZE         6
#define SW_SHOWMINNOACTIVE  7
#define SW_SHOWNA           8
#define SW_RESTORE          9
#define SW_SHOWDEFAULT      10
/*
#define TVHT_NOWHERE            1       // 0x0001
#define TVHT_ONITEMICON         2       // 0x0002
#define TVHT_ONITEMLABEL        4       // 0x0004
#define TVHT_ONITEM             (TVHT_ONITEMICON + TVHT_ONITEMLABEL + TVHT_ONITEMSTATEICON)
#define TVHT_ONITEMINDENT       8       // 0x0008
#define TVHT_ONITEMBUTTON       16      // 0x0010
#define TVHT_ONITEMRIGHT        32      // 0x0020
#define TVHT_ONITEMSTATEICON    64      // 0x0040

#define TVHT_ABOVE              256     // 0x0100
#define TVHT_BELOW              512     // 0x0200
#define TVHT_TORIGHT            1024    // 0x0400
#define TVHT_TOLEFT             2048    // 0x0800
*/
/* For video controls */
#define WIN_CHARPIX_H	16
#define WIN_CHARPIX_W    8
#define VID_CHARPIX_H	14
#define VID_CHARPIX_W    8
#define CS_VREDRAW                 1  // 0x0001
#define CS_HREDRAW                 2  // 0x0002

