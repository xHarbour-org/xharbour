#ifndef _RICHEDIT_VER
   #define _RICHEDIT_VER   0x0300
#endif

#define cchTextLimitDefault 32767

#define CERICHEDIT_CLASSA  "RichEditCEA"
#define CERICHEDIT_CLASSW  "RichEditCEW"

#define RICHEDIT_CLASSA    "RichEdit20A"
#define RICHEDIT_CLASS10A  "RICHEDIT"        // Richedit 1.0

#define RICHEDIT_CLASSW    "RichEdit20W"

#ifdef UNICODE
   #define RICHEDIT_CLASS     RICHEDIT_CLASSW
 #else
   #define RICHEDIT_CLASS     RICHEDIT_CLASSA
#endif // UNICODE
//#else
//#define RICHEDIT_CLASS     RICHEDIT_CLASS10A

/* RichEdit messages */

#ifndef WM_CONTEXTMENU
   #define WM_CONTEXTMENU        0x007B
#endif

#ifndef WM_PRINTCLIENT
   #define WM_PRINTCLIENT        0x0318
#endif

#ifndef EM_GETLIMITTEXT
   #define EM_GETLIMITTEXT       (WM_USER + 37)
#endif

#ifndef EM_POSFROMCHAR  
   #define EM_POSFROMCHAR        (WM_USER + 38)
   #define EM_CHARFROMPOS        (WM_USER + 39)
#endif

#ifndef EM_SCROLLCARET
   #define EM_SCROLLCARET        (WM_USER + 49)
#endif
#define EM_CANPASTE           (WM_USER + 50)
#define EM_DISPLAYBAND        (WM_USER + 51)
#define EM_EXGETSEL           (WM_USER + 52)
#define EM_EXLIMITTEXT        (WM_USER + 53)
#define EM_EXLINEFROMCHAR     (WM_USER + 54)
#define EM_EXSETSEL           (WM_USER + 55)
#define EM_FINDTEXT           (WM_USER + 56)
#define EM_FORMATRANGE        (WM_USER + 57)
#define EM_GETCHARFORMAT      (WM_USER + 58)
#define EM_GETEVENTMASK       (WM_USER + 59)
#define EM_GETOLEINTERFACE    (WM_USER + 60)
#define EM_GETPARAFORMAT      (WM_USER + 61)
#define EM_GETSELTEXT         (WM_USER + 62)
#define EM_HIDESELECTION      (WM_USER + 63)
#define EM_PASTESPECIAL       (WM_USER + 64)
#define EM_REQUESTRESIZE      (WM_USER + 65)
#define EM_SELECTIONTYPE      (WM_USER + 66)
#define EM_SETBKGNDCOLOR      (WM_USER + 67)
#define EM_SETCHARFORMAT      (WM_USER + 68)
#define EM_SETEVENTMASK       (WM_USER + 69)
#define EM_SETOLECALLBACK     (WM_USER + 70)
#define EM_SETPARAFORMAT      (WM_USER + 71)
#define EM_SETTARGETDEVICE    (WM_USER + 72)
#define EM_STREAMIN           (WM_USER + 73)
#define EM_STREAMOUT          (WM_USER + 74)
#define EM_GETTEXTRANGE       (WM_USER + 75)
#define EM_FINDWORDBREAK      (WM_USER + 76)
#define EM_SETOPTIONS         (WM_USER + 77)
#define EM_GETOPTIONS         (WM_USER + 78)
#define EM_FINDTEXTEX         (WM_USER + 79)
#ifdef _WIN32
   #define EM_GETWORDBREAKPROCEX (WM_USER + 80)
   #define EM_SETWORDBREAKPROCEX (WM_USER + 81)
#endif

/* RichEdit 2.0 messages */
#define  EM_SETUNDOLIMIT         (WM_USER + 82)
#define EM_REDO               (WM_USER + 84)
#define EM_CANREDO            (WM_USER + 85)
#define EM_GETUNDONAME        (WM_USER + 86)
#define EM_GETREDONAME        (WM_USER + 87)
#define EM_STOPGROUPTYPING    (WM_USER + 88)

#define EM_SETTEXTMODE        (WM_USER + 89)
#define EM_GETTEXTMODE        (WM_USER + 90)

#define EM_AUTOURLDETECT      (WM_USER + 91)
#define EM_GETAUTOURLDETECT      (WM_USER + 92)
#define EM_SETPALETTE         (WM_USER + 93)
#define EM_GETTEXTEX       (WM_USER + 94)
#define EM_GETTEXTLENGTHEX    (WM_USER + 95)
#define EM_SHOWSCROLLBAR      (WM_USER + 96)
#define EM_SETTEXTEX       (WM_USER + 97)

/* Far East specific messages */
#define EM_SETPUNCTUATION     (WM_USER + 100)
#define EM_GETPUNCTUATION     (WM_USER + 101)
#define EM_SETWORDWRAPMODE    (WM_USER + 102)
#define EM_GETWORDWRAPMODE    (WM_USER + 103)
#define EM_SETIMECOLOR        (WM_USER + 104)
#define EM_GETIMECOLOR        (WM_USER + 105)
#define EM_SETIMEOPTIONS      (WM_USER + 106)
#define EM_GETIMEOPTIONS      (WM_USER + 107)
#define EM_CONVPOSITION       (WM_USER + 108)

#define EM_SETLANGOPTIONS     (WM_USER + 120)
#define EM_GETLANGOPTIONS     (WM_USER + 121)
#define EM_GETIMECOMPMODE     (WM_USER + 122)

#define EM_FINDTEXTW       (WM_USER + 123)
#define EM_FINDTEXTEXW        (WM_USER + 124)

/* RE3.0 FE messages */
#define EM_RECONVERSION       (WM_USER + 125)
#define EM_SETIMEMODEBIAS     (WM_USER + 126)   
#define EM_GETIMEMODEBIAS     (WM_USER + 127)

/* BiDi specific messages */
#define EM_SETBIDIOPTIONS     (WM_USER + 200)
#define EM_GETBIDIOPTIONS     (WM_USER + 201)

#define EM_SETTYPOGRAPHYOPTIONS  (WM_USER + 202)
#define EM_GETTYPOGRAPHYOPTIONS  (WM_USER + 203)

/* Extended edit style specific messages */
#define EM_SETEDITSTYLE       (WM_USER + 204)
#define EM_GETEDITSTYLE       (WM_USER + 205)

/* Extended edit style masks */
#define  SES_EMULATESYSEDIT      1
#define SES_BEEPONMAXTEXT     2
#define  SES_EXTENDBACKCOLOR     4
#define SES_MAPCPS            8
#define SES_EMULATE10         16
#define  SES_USECRLF          32
#define SES_USEAIMM           64
#define SES_NOIME          128

#define SES_ALLOWBEEPS        256
#define SES_UPPERCASE         512
#define  SES_LOWERCASE        1024
#define SES_NOINPUTSEQUENCECHK   2048
#define SES_BIDI           4096
#define SES_SCROLLONKILLFOCUS 8192
#define  SES_XLTCRCRLFTOCR    16384


/* Options for EM_SETLANGOPTIONS and EM_GETLANGOPTIONS */
#define IMF_AUTOKEYBOARD      0x0001
#define IMF_AUTOFONT       0x0002
#define IMF_IMECANCELCOMPLETE 0x0004   // high completes comp string when aborting, low cancels.
#define IMF_IMEALWAYSSENDNOTIFY 0x0008
#define IMF_AUTOFONTSIZEADJUST   0x0010
#define IMF_UIFONTS           0x0020
#define IMF_DUALFONT       0x0080

/* Values for EM_GETIMECOMPMODE */
#define ICM_NOTOPEN           0x0000
#define ICM_LEVEL3            0x0001
#define ICM_LEVEL2            0x0002
#define ICM_LEVEL2_5       0x0003
#define ICM_LEVEL2_SUI        0x0004

/* Options for EM_SETTYPOGRAPHYOPTIONS */
#define  TO_ADVANCEDTYPOGRAPHY   1
#define  TO_SIMPLELINEBREAK      2

/* Pegasus outline mode messages (RE 3.0) */

// Outline mode message
#define EM_OUTLINE              (WM_USER + 220)
// Message for getting and restoring scroll pos
#define EM_GETSCROLLPOS         (WM_USER + 221)
#define EM_SETSCROLLPOS         (WM_USER + 222)
// Change fontsize in current selection by wParam
#define EM_SETFONTSIZE          (WM_USER + 223)
#define EM_GETZOOM            (WM_USER + 224)
#define EM_SETZOOM            (WM_USER + 225)

// Outline mode wparam values
#define EMO_EXIT                0       // enter normal mode,  lparam ignored
#define EMO_ENTER               1       // enter outline mode, lparam ignored
#define EMO_PROMOTE             2       // LOWORD(lparam) == 0 ==>
                                        // promote  to body-text
                                        // LOWORD(lparam) != 0 ==>
                                        // promote/demote current selection
                                        // by indicated number of levels
#define EMO_EXPAND              3       // HIWORD(lparam) = EMO_EXPANDSELECTION
                                        // -> expands selection to level
                                        // indicated in LOWORD(lparam)
                                        // LOWORD(lparam) = -1/+1 corresponds
                                        // to collapse/expand button presses
                                        // in winword (other values are
                                        // equivalent to having pressed these
                                        // buttons more than once)
                                        // HIWORD(lparam) = EMO_EXPANDDOCUMENT
                                        // -> expands whole document to
                                        // indicated level
#define EMO_MOVESELECTION       4       // LOWORD(lparam) != 0 -> move current
                                        // selection up/down by indicated
                                        // amount
#define EMO_GETVIEWMODE       5     // Returns VM_NORMAL or VM_OUTLINE

// EMO_EXPAND options
#define EMO_EXPANDSELECTION     0
#define EMO_EXPANDDOCUMENT      1
#define VM_NORMAL          4     // Agrees with RTF \viewkindN
#define VM_OUTLINE            2

/* New notifications */
#define EN_MSGFILTER       0x0700
#define EN_REQUESTRESIZE      0x0701
#define EN_SELCHANGE       0x0702
#define EN_DROPFILES       0x0703
#define EN_PROTECTED       0x0704
#define EN_CORRECTTEXT        0x0705         /* PenWin specific */
#define EN_STOPNOUNDO         0x0706
#define EN_IMECHANGE       0x0707         /* Far East specific */
#define EN_SAVECLIPBOARD      0x0708
#define EN_OLEOPFAILED        0x0709
#define EN_OBJECTPOSITIONS    0x070a
#define EN_LINK               0x070b
#define EN_DRAGDROPDONE       0x070c
#define EN_PARAGRAPHEXPANDED  0x070d

/* BiDi specific notifications */
#define EN_ALIGNLTR           0x0710
#define EN_ALIGNRTL           0x0711

/* Event notification masks */
#define ENM_NONE           0x00000000
#define ENM_CHANGE            0x00000001
#define ENM_UPDATE            0x00000002
#define ENM_SCROLL            0x00000004
#define ENM_KEYEVENTS         0x00010000
#define ENM_MOUSEEVENTS       0x00020000
#define ENM_REQUESTRESIZE     0x00040000
#define ENM_SELCHANGE         0x00080000
#define ENM_DROPFILES         0x00100000
#define ENM_PROTECTED         0x00200000
#define ENM_CORRECTTEXT       0x00400000     /* PenWin specific */
#define ENM_SCROLLEVENTS      0x00000008
#define ENM_DRAGDROPDONE      0x00000010
#define ENM_PARAGRAPHEXPANDED 0x00000020

/* Far East specific notification mask */
#define ENM_IMECHANGE         0x00800000     /* unused by RE2.0 */
#define ENM_LANGCHANGE        0x01000000
#define ENM_OBJECTPOSITIONS      0x02000000
#define ENM_LINK           0x04000000

/* New edit control styles */
#define ES_SAVESEL            0x00008000
#define ES_SUNKEN          0x00004000
#define ES_DISABLENOSCROLL    0x00002000
/* same as WS_MAXIMIZE, but that doesn't make sense so we re-use the value */
#define ES_SELECTIONBAR       0x01000000
/* same as ES_UPPERCASE, but re-used to completely disable OLE drag'n'drop */
#define ES_NOOLEDRAGDROP      0x00000008

/* Edit control extended style */
// #if (_WIN32_WINNT > 0x0400) || (WINVER > 0x0400)
// #define ES_EX_NOCALLOLEINIT      0x00000000     /* Not supported in RE 2.0/3.0 */
// #else
// #ifdef   _WIN32
#define ES_EX_NOCALLOLEINIT      0x01000000
// #endif   
//#endif

/* These flags are used in FE Windows */
#define ES_VERTICAL           0x00400000     /* Not supported in RE 2.0/3.0 */
#define  ES_NOIME          0x00080000
#define ES_SELFIME            0x00040000

/* Edit control options */
#define ECO_AUTOWORDSELECTION 0x00000001
#define ECO_AUTOVSCROLL       0x00000040
#define ECO_AUTOHSCROLL       0x00000080
#define ECO_NOHIDESEL         0x00000100
#define ECO_READONLY       0x00000800
#define ECO_WANTRETURN        0x00001000
#define ECO_SAVESEL           0x00008000
#define ECO_SELECTIONBAR      0x01000000
#define ECO_VERTICAL       0x00400000     /* FE specific */


/* ECO operations */
#define ECOOP_SET          0x0001
#define ECOOP_OR           0x0002
#define ECOOP_AND          0x0003
#define ECOOP_XOR          0x0004

/* New word break function actions */
#define WB_CLASSIFY        3
#define WB_MOVEWORDLEFT    4
#define WB_MOVEWORDRIGHT   5
#define WB_LEFTBREAK    6
#define WB_RIGHTBREAK      7

/* Far East specific flags */
#define WB_MOVEWORDPREV    4
#define WB_MOVEWORDNEXT    5
#define WB_PREVBREAK    6
#define WB_NEXTBREAK    7

#define PC_FOLLOWING    1
#define  PC_LEADING        2
#define  PC_OVERFLOW       3
#define  PC_DELIMITER      4
#define WBF_WORDWRAP    0x010
#define WBF_WORDBREAK      0x020
#define  WBF_OVERFLOW      0x040 
#define WBF_LEVEL1         0x080
#define  WBF_LEVEL2        0x100
#define  WBF_CUSTOM        0x200

/* Far East specific flags */
#define IMF_FORCENONE           0x0001
#define IMF_FORCEENABLE         0x0002
#define IMF_FORCEDISABLE        0x0004
#define IMF_CLOSESTATUSWINDOW   0x0008
#define IMF_VERTICAL            0x0020
#define IMF_FORCEACTIVE         0x0040
#define IMF_FORCEINACTIVE       0x0080
#define IMF_FORCEREMEMBER       0x0100
#define IMF_MULTIPLEEDIT        0x0400

/* Word break flags (used with WB_CLASSIFY) */
#define WBF_CLASS       ((BYTE) 0x0F)
#define WBF_ISWHITE        ((BYTE) 0x10)
#define WBF_BREAKLINE      ((BYTE) 0x20)
#define WBF_BREAKAFTER     ((BYTE) 0x40)


/* new data types */
//#else
//#define CHARFORMAT CHARFORMATA

/* CHARFORMAT masks */
#define CFM_BOLD     0x00000001
#define CFM_ITALIC      0x00000002
#define CFM_UNDERLINE   0x00000004
#define CFM_STRIKEOUT   0x00000008
#define CFM_PROTECTED   0x00000010
#define CFM_LINK     0x00000020     /* Exchange hyperlink extension */
#define CFM_SIZE     0x80000000
#define CFM_COLOR    0x40000000
#define CFM_FACE     0x20000000
#define CFM_OFFSET      0x10000000
#define CFM_CHARSET     0x08000000

/* CHARFORMAT effects */
#define CFE_BOLD     0x0001
#define CFE_ITALIC      0x0002
#define CFE_UNDERLINE   0x0004
#define CFE_STRIKEOUT   0x0008
#define CFE_PROTECTED   0x0010
#define CFE_LINK     0x0020
#define CFE_AUTOCOLOR   0x40000000     /* NOTE: this corresponds to */
                              /* CFM_COLOR, which controls it */
#define yHeightCharPtsMost 1638

/* EM_SETCHARFORMAT wParam masks */
#define SCF_SELECTION      0x0001
#define SCF_WORD        0x0002
#define SCF_DEFAULT        0x0000      // Set default charformat or paraformat
#define SCF_ALL            0x0004      // Not valid with SCF_SELECTION or SCF_WORD
#define SCF_USEUIRULES     0x0008      // Modifier for SCF_SELECTION; says that
                              //  format came from a toolbar, etc., and
                              //  hence UI formatting rules should be
                              //  used instead of literal formatting
#define SCF_ASSOCIATEFONT  0x0010      // Associate fontname with bCharSet (one
                              //  possible for each of Western, ME, FE,
                              //  Thai)
#define SCF_NOKBUPDATE     0x0020      // Do not update the KB layput for this change
                              // even if autokeyboard is on.
#ifdef UNICODE
   #define TEXTRANGE    TEXTRANGEW
 #else
   #define TEXTRANGE TEXTRANGEA
#endif /* UNICODE */


/* Stream formats. Flags are all in low word, since high word gives
   possible codepage choice. */

#define SF_TEXT         0x0001
#define SF_RTF       0x0002
#define SF_RTFNOOBJS 0x0003      /* Write only */
#define SF_TEXTIZED     0x0004      /* Write only */

#define SF_UNICODE      0x0010      /* Unicode file (UCS2 little endian) */
#define SF_USECODEPAGE  0x0020      /* CodePage given by high word */
#define SF_NCRFORNONASCII 0x40      /* Output /uN for nonASCII */

/* Flag telling stream operations to operate on selection only */
/* EM_STREAMIN  will replace current selection */
/* EM_STREAMOUT will stream out current selection */
#define SFF_SELECTION   0x8000

/* Flag telling stream operations to ignore some FE control words */
/* having to do with FE word breaking and horiz vs vertical text. */
/* Not used in RichEdit 2.0 and later  */
#define SFF_PLAINRTF 0x4000

/* Flag telling file stream output (SFF_SELECTION flag not set) to persist */
/* \viewscaleN control word. */
#define SFF_PERSISTVIEWSCALE 0x2000

/* Flag telling file stream input with SFF_SELECTION flag not set not to */
/* close the document */
#define SFF_KEEPDOCINFO 0x1000

/* Flag telling stream operations to output in Pocket Word format */
#define SFF_PWD         0x0800

/* 3-bit field specifying the value of N - 1 to use for \rtfN or \pwdN */
#define SF_RTFVAL    0x0700

#ifdef UNICODE
   #define FINDTEXT  FINDTEXTW
 #else
   #define FINDTEXT  FINDTEXTA
#endif /* UNICODE */
//#else
//#define FINDTEXT  FINDTEXTA

#ifdef UNICODE
   #define FINDTEXTEX   FINDTEXTEXW
 #else
   #define FINDTEXTEX   FINDTEXTEXA
#endif /* UNICODE */
//#else
//#define FINDTEXTEX   FINDTEXTEXA


/* All paragraph measurements are in twips */

#define MAX_TAB_STOPS 32
#define lDefaultTab 720

/* This is a hack to make PARAFORMAT compatible with RE 1.0 */
#define  wReserved   wEffects

/* PARAFORMAT mask values */
#define PFM_STARTINDENT       0x00000001
#define PFM_RIGHTINDENT       0x00000002
#define PFM_OFFSET            0x00000004
#define PFM_ALIGNMENT         0x00000008
#define PFM_TABSTOPS       0x00000010
#define PFM_NUMBERING         0x00000020
#define PFM_OFFSETINDENT      0x80000000

/* PARAFORMAT numbering options */
#define PFN_BULLET      0x0001

/* PARAFORMAT alignment options */
#define PFA_LEFT  0x0001
#define PFA_RIGHT 0x0002
#define PFA_CENTER   0x0003

#define CHARFORMATDELTA    (sizeof(CHARFORMAT2) - sizeof(CHARFORMAT))


/* CHARFORMAT and PARAFORMAT "ALL" masks
   CFM_COLOR mirrors CFE_AUTOCOLOR, a little hack to easily deal with autocolor*/

#define CFM_EFFECTS (CFM_BOLD + CFM_ITALIC + CFM_UNDERLINE + CFM_COLOR + CFM_STRIKEOUT + CFE_PROTECTED + CFM_LINK)
#define CFM_ALL (CFM_EFFECTS + CFM_SIZE + CFM_FACE + CFM_OFFSET + CFM_CHARSET)

#define  PFM_ALL (PFM_STARTINDENT + PFM_RIGHTINDENT + PFM_OFFSET  + PFM_ALIGNMENT + PFM_TABSTOPS + PFM_NUMBERING + PFM_OFFSETINDENT+ PFM_RTLPARA)

/* New masks and effects -- a parenthesized asterisk indicates that
   the data is stored by RichEdit 2.0/3.0, but not displayed */

#define CFM_SMALLCAPS      0x0040         /* (*)   */
#define  CFM_ALLCAPS       0x0080         /* Displayed by 3.0  */
#define  CFM_HIDDEN        0x0100         /* Hidden by 3.0 */
#define  CFM_OUTLINE       0x0200         /* (*)   */
#define  CFM_SHADOW        0x0400         /* (*)   */
#define  CFM_EMBOSS        0x0800         /* (*)   */
#define  CFM_IMPRINT       0x1000         /* (*)   */
#define CFM_DISABLED    0x2000
#define  CFM_REVISED       0x4000

#define CFM_BACKCOLOR      0x04000000
#define CFM_LCID        0x02000000
#define  CFM_UNDERLINETYPE 0x00800000     /* Many displayed by 3.0 */
#define  CFM_WEIGHT        0x00400000
#define CFM_SPACING        0x00200000     /* Displayed by 3.0  */
#define CFM_KERNING        0x00100000     /* (*)   */
#define CFM_STYLE       0x00080000     /* (*)   */
#define CFM_ANIMATION      0x00040000     /* (*)   */
#define CFM_REVAUTHOR      0x00008000

#define CFE_SUBSCRIPT      0x00010000     /* Superscript and subscript are */
#define CFE_SUPERSCRIPT    0x00020000     /*  mutually exclusive         */

#define CFM_SUBSCRIPT      CFE_SUBSCRIPT + CFE_SUPERSCRIPT
#define CFM_SUPERSCRIPT    CFM_SUBSCRIPT

#define  CFM_EFFECTS2 (CFM_EFFECTS + CFM_DISABLED + CFM_SMALLCAPS + CFM_ALLCAPS +;
               CFM_HIDDEN  + CFM_OUTLINE + CFM_SHADOW + CFM_EMBOSS +;
               CFM_IMPRINT + CFM_DISABLED + CFM_REVISED +;
               CFM_SUBSCRIPT + CFM_SUPERSCRIPT + CFM_BACKCOLOR)

#define CFM_ALL2   (CFM_ALL + CFM_EFFECTS2 + CFM_BACKCOLOR + CFM_LCID +;
               CFM_UNDERLINETYPE + CFM_WEIGHT + CFM_REVAUTHOR +;
               CFM_SPACING + CFM_KERNING + CFM_STYLE + CFM_ANIMATION)

#define  CFE_SMALLCAPS     CFM_SMALLCAPS
#define  CFE_ALLCAPS       CFM_ALLCAPS
#define  CFE_HIDDEN        CFM_HIDDEN
#define  CFE_OUTLINE       CFM_OUTLINE
#define  CFE_SHADOW        CFM_SHADOW
#define  CFE_EMBOSS        CFM_EMBOSS
#define  CFE_IMPRINT       CFM_IMPRINT
#define  CFE_DISABLED      CFM_DISABLED
#define  CFE_REVISED       CFM_REVISED

/* NOTE: CFE_AUTOCOLOR and CFE_AUTOBACKCOLOR correspond to CFM_COLOR and
   CFM_BACKCOLOR, respectively, which control them */
#define CFE_AUTOBACKCOLOR  CFM_BACKCOLOR

/* Underline types. RE 1.0 displays only CFU_UNDERLINE */
#define CFU_CF1UNDERLINE   0xFF  /* map charformat's bit underline to CF2.*/
#define CFU_INVERT         0xFE  /* For IME composition fake a selection.*/
#define CFU_UNDERLINEHAIRLINE 10 /* (*) displayed as ordinary underline */
#define CFU_UNDERLINETHICK    9
#define CFU_UNDERLINEWAVE     8
#define  CFU_UNDERLINEDASHDOTDOT 7
#define  CFU_UNDERLINEDASHDOT 6
#define  CFU_UNDERLINEDASH    5
#define  CFU_UNDERLINEDOTTED     4
#define  CFU_UNDERLINEDOUBLE     3  /* (*) displayed as ordinary underline */
#define CFU_UNDERLINEWORD     2  /* (*) displayed as ordinary underline */
#define CFU_UNDERLINE         1
#define CFU_UNDERLINENONE     0


/* PARAFORMAT 2.0 masks and effects */

#define PFM_SPACEBEFORE       0x00000040
#define PFM_SPACEAFTER        0x00000080
#define PFM_LINESPACING       0x00000100
#define  PFM_STYLE            0x00000400
#define PFM_BORDER            0x00000800  /* (*)   */
#define PFM_SHADING           0x00001000  /* (*)   */
#define PFM_NUMBERINGSTYLE    0x00002000  /* RE 3.0   */
#define PFM_NUMBERINGTAB      0x00004000  /* RE 3.0   */
#define PFM_NUMBERINGSTART    0x00008000  /* RE 3.0   */

#define PFM_RTLPARA           0x00010000
#define PFM_KEEP           0x00020000  /* (*)   */
#define PFM_KEEPNEXT       0x00040000  /* (*)   */
#define PFM_PAGEBREAKBEFORE      0x00080000  /* (*)   */
#define PFM_NOLINENUMBER      0x00100000  /* (*)   */
#define PFM_NOWIDOWCONTROL    0x00200000  /* (*)   */
#define PFM_DONOTHYPHEN       0x00400000  /* (*)   */
#define PFM_SIDEBYSIDE        0x00800000  /* (*)   */
#define PFM_TABLE          0x40000000  /* RE 3.0 */

// The following three properties are read only
#define PFM_COLLAPSED         0x01000000  /* RE 3.0 */
#define PFM_OUTLINELEVEL      0x02000000  /* RE 3.0 */
#define PFM_BOX               0x04000000  /* RE 3.0 */


/* Note: PARAFORMAT has no effects */
#define PFM_EFFECTS (PFM_RTLPARA + PFM_KEEP + PFM_KEEPNEXT + PFM_TABLE ;
               + PFM_PAGEBREAKBEFORE + PFM_NOLINENUMBER  ;
               + PFM_NOWIDOWCONTROL + PFM_DONOTHYPHEN + PFM_SIDEBYSIDE ;
               + PFM_TABLE)

#define PFM_ALL2  (PFM_ALL + PFM_EFFECTS + PFM_SPACEBEFORE + PFM_SPACEAFTER ;
               + PFM_LINESPACING + PFM_STYLE + PFM_SHADING + PFM_BORDER ;
               + PFM_NUMBERINGTAB + PFM_NUMBERINGSTART + PFM_NUMBERINGSTYLE)

#define PFE_RTLPARA           (PFM_RTLPARA       >> 16)
#define PFE_KEEP           (PFM_KEEP          >> 16)  /* (*)   */
#define PFE_KEEPNEXT       (PFM_KEEPNEXT      >> 16)  /* (*)   */
#define PFE_PAGEBREAKBEFORE      (PFM_PAGEBREAKBEFORE >> 16)   /* (*)   */
#define PFE_NOLINENUMBER      (PFM_NOLINENUMBER  >> 16)  /* (*)   */
#define PFE_NOWIDOWCONTROL    (PFM_NOWIDOWCONTROL   >> 16)  /* (*)   */
#define PFE_DONOTHYPHEN       (PFM_DONOTHYPHEN   >> 16)  /* (*)   */
#define PFE_SIDEBYSIDE        (PFM_SIDEBYSIDE       >> 16)  /* (*)   */

// The following four effects are read only
#define PFE_OUTLINELEVEL      (PFM_OUTLINELEVEL  >> 16)  /* (+)   */
#define PFE_COLLAPSED         (PFM_COLLAPSED     >> 16)  /* (+)   */
#define PFE_BOX               (PFM_BOX        >> 16)  /* (+)   */
#define PFE_TABLE          0x4000      /* Para is a table row. RE 3.0 */

/* PARAFORMAT2 wNumbering options (see also PFN_BULLET) */
#define PFN_ARABIC      2     /* tomListNumberAsArabic:   0, 1, 2,   ...*/
#define PFN_LCLETTER 3     /* tomListNumberAsLCLetter: a, b, c,   ...*/
#define  PFN_UCLETTER   4     /* tomListNumberAsUCLetter: A, B, C,   ...*/
#define  PFN_LCROMAN    5     /* tomListNumberAsLCRoman:  i, ii, iii,   ...*/
#define  PFN_UCROMAN    6     /* tomListNumberAsUCRoman:  I, II, III,   ...*/

/* PARAFORMAT2 wNumberingStyle options */
#define PFNS_PAREN      0x000 /* default, e.g.,            1)  */
#define  PFNS_PARENS    0x100 /* tomListParentheses/256, e.g., (1)   */
#define PFNS_PERIOD     0x200 /* tomListPeriod/256, e.g.,        1.  */
#define PFNS_PLAIN      0x300 /* tomListPlain/256, e.g.,      1      */
#define PFNS_NONUMBER   0x400 /* Used for continuation w/o number    */

#define  PFA_JUSTIFY        4 /* New paragraph-alignment option 2.0 (*) */
#define PFA_FULL_INTERWORD  4
#define PFA_FULL_INTERLETTER 5
#define PFA_FULL_SCALED     6
#define  PFA_FULL_GLYPHS       7
#define  PFA_SNAP_GRID      8

/* Notification structures */

#ifndef WM_NOTIFY
   #define WM_NOTIFY    0x004E
#endif
#define SEL_EMPTY    0x0000
#define SEL_TEXT     0x0001
#define SEL_OBJECT      0x0002
#define SEL_MULTICHAR   0x0004
#define SEL_MULTIOBJECT 0x0008

/* Used with IRichEditOleCallback::GetContextMenu, this flag will be
   passed as a "selection type".  It indicates that a context menu for
   a right-mouse drag drop should be generated.  The IOleObject parameter
   will really be the IDataObject for the drop
 */
#define GCM_RIGHTMOUSEDROP  0x8000
#define  OLEOP_DOVERB   1

/* Clipboard formats - use as parameter to RegisterClipboardFormat() */
#define CF_RTF          "Rich Text Format"
#define CF_RTFNOOBJS    "Rich Text Format Without Objects"
#define CF_RETEXTOBJ    "RichEdit Text and Objects"

/* Flags for the SETEXTEX data structure */
#define ST_DEFAULT      0
#define ST_KEEPUNDO     1
#define ST_SELECTION 2

/* Flags for the GETEXTEX data structure */
#define GT_DEFAULT      0
#define GT_USECRLF      1
#define GT_SELECTION 2

/* Flags for the GETTEXTLENGTHEX data structure                   */
#define GTL_DEFAULT     0  /* do the default (return # of chars)     */
#define GTL_USECRLF     1  /* compute answer using CRLFs for paragraphs*/
#define GTL_PRECISE     2  /* compute a precise answer               */
#define GTL_CLOSE    4  /* fast computation of a "close" answer      */
#define GTL_NUMCHARS 8  /* return the number of characters        */
#define GTL_NUMBYTES 16 /* return the number of _bytes_           */

/* BIDIOPTIONS masks */
#if (_RICHEDIT_VER == 0x0100)
   #define BOM_DEFPARADIR        0x0001   /* Default paragraph direction (implies alignment) (obsolete) */
   #define BOM_PLAINTEXT         0x0002   /* Use plain text layout (obsolete) */
#endif /* _RICHEDIT_VER == 0x0100 */
#define BOM_NEUTRALOVERRIDE      0x0004   /* Override neutral layout (obsolete) */
#define BOM_CONTEXTREADING    0x0008   /* Context reading order */
#define BOM_CONTEXTALIGNMENT  0x0010   /* Context alignment */

/* BIDIOPTIONS effects */
#define BOE_RTLDIR            0x0001   /* Default paragraph direction (implies alignment) (obsolete) */
#define BOE_PLAINTEXT         0x0002   /* Use plain text layout (obsolete) */

#define BOE_NEUTRALOVERRIDE      0x0004   /* Override neutral layout (obsolete) */
#define BOE_CONTEXTREADING    0x0008   /* Context reading order */
#define BOE_CONTEXTALIGNMENT  0x0010   /* Context alignment */

/* Additional EM_FINDTEXT[EX] flags */
#ifndef FR_MATCHDIAC
 #define FR_MATCHDIAC                    0x20000000
#endif
#ifndef FR_MATCHKASHIDA
 #define FR_MATCHKASHIDA                 0x40000000
#endif 
#ifndef FR_MATCHALEFHAMZA
 #define FR_MATCHALEFHAMZA               0x80000000
#endif
   
/* UNICODE embedding character */
#ifndef WCH_EMBEDDING
   #define WCH_EMBEDDING (WCHAR)0xFFFC
#endif /* WCH_EMBEDDING */
      
