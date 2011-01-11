/****************************************************************************
 *																			*
 * File    : addin.h														*
 *																			*
 * Purpose : Definitions for Pelles C add-in API 2.0.						*
 *																			*
 * History : Date	   Reason												*
 *			 04-01-28  Created												*
 *			 04-05-19  Added definitions for version 2.80					*
 *			 04-10-12  Added definitions for version 2.90					*
 *																			*
 ****************************************************************************/

#ifndef _ADDIN_H
#define _ADDIN_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define ADDINAPI __declspec(dllexport)

#ifndef SNDMSG
#ifdef __cplusplus
#define SNDMSG ::SendMessage
#else
#define SNDMSG SendMessage
#endif
#endif

/*
 * AddInMain() events.
 */
typedef enum tagADDIN_EVENT {
	AIE_APP_CREATE = 1,		// Main window created (hwnd = main window handle).
	AIE_APP_DESTROY = 2,	// Main window destroyed (hwnd = main window handle).
	AIE_PRJ_CREATE = 3,		// Project pane created (hwnd = project pane handle).
	AIE_PRJ_DESTROY = 4,	// Project pane destroyed (hwnd = project pane handle).
	AIE_PRJ_SAVE = 7,		// Project saved (2.90) (hwnd = project pane handle).
	AIE_PRJ_STARTBUILD = 8,	// Project build will start (2.90) (hwnd = project pane handle).
	AIE_PRJ_ENDBUILD = 9,	// Project build has ended (2.90) (hwnd = project pane handle).
	AIE_DOC_CREATE = 5,		// MDI child window created (hwnd = MDI child window handle).
	AIE_DOC_DESTROY = 6,	// MDI child window destroyed (hwnd = MDI child window handle).
	AIE_DOC_SAVE = 10,		// MDI child window saved (2.90) (hwnd = MDI child window handle).
} ADDIN_EVENT;

/*
 * IDE Window types.
 */
enum ADDIN_WINDOW_TYPES {
	AID_UNKNOWN = 0,
	AID_SOURCE,				// Source document.
	AID_RESOURCE,			// Resource script document.
	AID_BITMAP,				// Bitmap document.
	AID_ICON,				// Icon document.
	AID_CURSOR,				// Cursor document.
	AID_ANICURSOR,			// Animated cursor document.
	AID_VIDEO,				// Video document.
	AID_HEXDUMP,			// Hexdump document.
	AID_PROJECT,			// Project pane.
	/*AID_DIALOG,*/
	/*AID_MENU,*/
	/*AID_ACCELTABLE,*/
	/*AID_STRINGTABLE,*/
	/*AID_VERSION,*/
};

/*
 * IDE menu identifiers.
 */
enum ADDIN_MENU_IDENTIFIERS {
	AIM_MENU_NOTHING = 0,
	AIM_MENU_FILE = 1,					// File menu.
	AIM_MENU_EDIT = 2,					// Edit menu.
	AIM_MENU_VIEW = 3,					// View menu.
	AIM_MENU_PROJECT = 4,				// Project menu.
	AIM_MENU_PROJECT_CONTEXT = 5,		// Project context menu(s).
	AIM_MENU_HELP = 6,					// Help menu.
	AIM_MENU_SOURCE = 7,				// Source menu.
	AIM_MENU_SOURCE_CONTEXT = AIM_MENU_SOURCE,	// Source context menu.
	AIM_MENU_SOURCE_CONVERT = 9,		// Source convert menu.
	/*AIM_MENU_IMAGE = 10,*/				// Image menu.
	/*AIM_MENU_DIALOG = 11,*/			// Dialog menu.
	/*AIM_MENU_RESOURCE = 12,*/			// Resource menu.
	/*AIM_MENU_DEBUG = 13,*/			// Debug menu.
};

/*
 * IDE command identifiers - AddIn_SendIDECommand().
 */
enum ADDIN_COMMAND_IDENTIFIERS {
	/* File commands */
	AIC_FILE_NEWPROJECT = 7501,			// File - New project.
	AIC_FILE_NEWSOURCE = 7502,			// File - New source code.
	AIC_FILE_NEWRESOURCE = 7503,		// File - New resource script.
	AIC_FILE_NEWBITMAP = 7504,			// File - New bitmap.
	AIC_FILE_NEWICON = 7505,			// File - New icon.
	AIC_FILE_NEWCURSOR = 7506,			// File - New cursor.
	AIC_FILE_NEWANICURSOR = 7507,		// File - New animated cursor.
	AIC_FILE_NEWVIDEO = 7508,			// File - New video.
	AIC_FILE_NEWHEXDUMP = 7509,			// File - New binary.
	AIC_FILE_OPEN = 7510,				// File - Open.
	AIC_FILE_CLOSE = 7511,				// File - Close.
	AIC_FILE_SAVE = 7512,				// File - Save.
	AIC_FILE_SAVEAS = 7513,				// File - Save As.
	AIC_FILE_SAVEALL = 7514,			// File - Save All.
	AIC_FILE_PRINT = 7516,				// File - Print.
	AIC_FILE_PRINTSETUP = 7517,			// File - Page setup.
	AIC_FILE_SENDMAIL = 7518,			// File - Send.
	AIC_FILE_PROPERTIES = 7519,			// File - Properties.
	AIC_FILE_EXIT = 7520,				// File - Exit.
	/* Edit commands */
	AIC_EDIT_UNDO = 7521,				// Edit - Undo.
	AIC_EDIT_REDO = 7522,				// Edit - Redo.
	AIC_EDIT_CUT = 7523,				// Edit - Cut.
	AIC_EDIT_COPY = 7524,				// Edit - Copy.
	AIC_EDIT_PASTE = 7525,				// Edit - Paste.
	AIC_EDIT_INSERT = 7526,				// Insert (hidden).
	AIC_EDIT_DELETE = 7527,				// Edit - Delete.
	AIC_EDIT_DELETEWORD = 7650,			// Edit - Delete previous word (2.90).
	AIC_EDIT_SELECTALL = 7528,			// Edit - Select All.
	AIC_EDIT_FIND = 7529,				// Edit - Find.
	AIC_EDIT_FINDNEXT = 7530,			// Edit - Find next.
	AIC_EDIT_REPLACE = 7531,			// Edit - Replace.
	AIC_FILE_FILEFIND = 7515,			// Edit - Find in files.
	AIC_EDIT_GOTO = 7532,				// Edit - Go To.
	/* View commands */
	AIC_VIEW_LABELS = 7533,				// View - Resource symbols.
	AIC_VIEW_INCLUDES = 7534,			// View - Resource includes.
	AIC_VIEW_SHOWTOOLBAR = 7535,		// View - Toolbar.
	AIC_VIEW_SHOWSTATUSBAR = 7536,		// View - Statusbar.
	AIC_VIEW_SHOWDOCTABS = 7641,		// View - Document tabs (2.80).
	/* Project commands */
	AIC_PROJ_ADDFILE = 7537,			// Project - Add files to project.
	AIC_PROJ_SETTINGS = 7538,			// Project - Project options.
	AIC_PROJ_COMPILE = 7539,			// Project - compile.
	AIC_PROJ_BUILD = 7540,				// Project - Build.
	AIC_PROJ_BUILDALL = 7541,			// Project - Rebuild all files.
	AIC_PROJ_STOPBUILD = 7542,			// Project - Stop rebuild.
	AIC_PROJ_MAKEALLPREQS = 7543,		// Project - Update all dependencies.
	AIC_PROJ_EXECUTE = 7544,			// Project - Execute.
	AIC_PROJ_ZIP = 7545,				// Project - ZIP project files.
	AIC_PROJ_UNZIP = 7546,				// Project - UNZIP project files.
	/* Tools commands */
	AIC_TOOL_CUSTOMIZE = 7547,			// Tools - Customize.
	AIC_TOOL_SETTINGS = 7548,			// Tools - Options.
	AIC_TOOL_CUSTOMTOOL = 7549,			// Tools - Custom tool #1.
	AIC_TOOL_CUSTOMTOOL_1 = 7550,		// Tools - Custom tool #2.
	AIC_TOOL_CUSTOMTOOL_2 = 7551,		// Tools - Custom tool #3.
	AIC_TOOL_CUSTOMTOOL_3 = 7552,		// Tools - Custom tool #4.
	AIC_TOOL_CUSTOMTOOL_4 = 7553,		// Tools - Custom tool #5.
	AIC_TOOL_CUSTOMTOOL_5 = 7554,		// Tools - Custom tool #6.
	AIC_TOOL_CUSTOMTOOL_6 = 7555,		// Tools - Custom tool #7.
	AIC_TOOL_CUSTOMTOOL_7 = 7556,		// Tools - Custom tool #8.
	/* Window commands */
	AIC_WIN_NEW = 7557,					// Window - New window.
	AIC_WIN_CASCADE = 7558,				// Window - Cascade.
	AIC_WIN_TILEVERT = 7559,			// Window - Tile vertically.
	AIC_WIN_TILEHORZ = 7560,			// Window - Tile horizontally.
	AIC_WIN_CLOSEALL = 7561,			// Window - Close All.
	/* Help commands */
	AIC_HELP_CONTENTS = 7562,			// Help - Contents.
	AIC_HELP_ABOUT = 7563,				// Help - About.
	AIC_HELP_CUSTOMHELP = 7564,			// Help - Custom help #1.
	AIC_HELP_CUSTOMHELP_1 = 7565,		// Help - Custom help #2.
	AIC_HELP_CUSTOMHELP_2 = 7566,		// Help - Custom help #3.
	AIC_HELP_CUSTOMHELP_3 = 7567,		// Help - Custom help #4.
	AIC_HELP_CUSTOMHELP_4 = 7568,		// Help - Custom help #5.
	AIC_HELP_CUSTOMHELP_5 = 7569,		// Help - Custom help #6.
	AIC_HELP_CUSTOMHELP_6 = 7570,		// Help - Custom help #7.
	AIC_HELP_CUSTOMHELP_7 = 7571,		// Help - Custom help #8.
	/* Image commands */
	AIC_IMG_NEWIMAGE = 7572,
	AIC_IMG_DELETEIMAGE = 7573,
	AIC_IMG_LOADPALETTE = 7574,
	AIC_IMG_SAVEPALETTE = 7575,
	AIC_IMG_DEFAULTPALETTE = 7576,
	AIC_IMG_MIRROR_HORZ = 7577,
	AIC_IMG_MIRROR_VERT = 7578,
	AIC_IMG_GRAYSCALE = 7579,
	AIC_IMG_ROTATE = 7580,
	AIC_IMG_HSV = 7649,					// (2.90)
	AIC_IMG_EFFECT_OILPAINT = 7581,
	AIC_IMG_EFFECT_SPREAD = 7582,
	AIC_IMG_EFFECT_EMBOSS = 7583,
	AIC_IMG_EFFECT_FISHEYE = 7648,		// (2.90)
	AIC_IMG_EFFECT_BLUR = 7584,
	AIC_IMG_EFFECT_SHARPEN = 7585,
	/* Dialog commands */
	AIC_DLG_ALIGNLEFT = 7586,
	AIC_DLG_ALIGNRIGHT = 7587,
	AIC_DLG_ALIGNTOP = 7588,
	AIC_DLG_ALIGNBOTTOM = 7589,
	AIC_DLG_ALIGNVERT = 7590,
	AIC_DLG_ALIGNHORZ = 7591,
	AIC_DLG_CENTERVERT = 7592,
	AIC_DLG_CENTERHORZ = 7593,
	AIC_DLG_SPACEVERT = 7594,
	AIC_DLG_SPACEHORZ = 7595,
	AIC_DLG_ARRSIZEWIDTH = 7596,
	AIC_DLG_ARRSIZEHEIGHT = 7597,
	AIC_DLG_ARRSIZEBOTH = 7598,
	AIC_DLG_ARRPUSHBOTTOM = 7599,
	AIC_DLG_ARRPUSHRIGHT = 7600,
	AIC_DLG_SIZETOTEXT = 7601,
	AIC_DLG_NEWCUSTOM = 7602,
	AIC_DLG_OPENCUSTOM = 7603,
	AIC_DLG_REMOVECUSTOM = 7604,
	AIC_DLG_LOCKCONTROLS = 7640,		// 2.80
	AIC_DLG_ORDERCONTROLS = 7652,		// 2.90
	/* Resource commands */
	AIC_RSRC_NEWBITMAP = 7605,
	AIC_RSRC_NEWICON = 7606,
	AIC_RSRC_NEWCURSOR = 7607,
	AIC_RSRC_NEWANICURSOR = 7608,
	AIC_RSRC_NEWVIDEO = 7609,
	AIC_RSRC_NEWDIALOG = 7610,
	AIC_RSRC_NEWMENU = 7611,
	AIC_RSRC_NEWSTRINGTABLE = 7612,
	AIC_RSRC_NEWACCEL = 7613,
	AIC_RSRC_NEWVERSION = 7614,
	AIC_RSRC_NEWMANIFEST = 7615,
	AIC_RSRC_NEWCOPY = 7616,
	AIC_RSRC_IMPORT = 7617,
	AIC_RSRC_SORTNAMES = 7651,			// (2.90)
	/* Source commands */
	AIC_SRC_COMPLETEWORD = 7642,		// Source - Complete word (2.80).
	AIC_SRC_UCASE = 7618,				// Source - Convert to upper case.
	AIC_SRC_LCASE = 7619,				// Source - Convert to lower case.
	AIC_SRC_TABTOSPACE = 7620,			// Source - Convert to spaces (from tabs).
	AIC_SRC_SPACETOTAB = 7621,			// Source - Convert to tabs (from spaces).
	AIC_SRC_NEXTBOOKMARK = 7622,		// Source - Next bookmark.
	AIC_SRC_PREVBOOKMARK = 7645,		// Source - Previous bookmark.
	AIC_SRC_SETBOOKMARK = 7623,			// Source - Toggle bookmark.
	AIC_SRC_CLEARBOOKMARKS = 7624,		// Source - Remove all bookmarks.
	AIC_SRC_WHITESPACE = 7625,			// Source - View whitespace.
	AIC_SRC_LINEBREAKS = 7626,			// Source - View breaks.
	AIC_SRC_LINENUMBERS = 7627,			// Source - View linenumbers.
	AIC_SRC_FOLDINGS = 7644,			// Source - View foldings (2.90).
	AIC_SRC_FOLDALL = 7646,				// Source - Fold all (2.90).
	AIC_SRC_UNFOLDALL = 7647,			// Source - Unfold all (2.90).
	/* Debug commands */
	AIC_DBG_GO = 7628,
	AIC_DBG_RESTART = 7629,
	AIC_DBG_STOP = 7630,
	AIC_DBG_BREAK = 7631,
	AIC_DBG_STEPINTO = 7632,
	AIC_DBG_STEPOVER = 7633,
	AIC_DBG_SETBREAKPOINT = 7634,
	AIC_DBG_SHOWNEXTSTMT = 7635,
	AIC_DBG_ADDWATCH = 7643,			// 2.80
	/* Misc commands */
	AIC_MISC_OPEN = 7636,
	AIC_MISC_TEST = 7637,
	AIC_MISC_CHECKMNEMONICS = 7638,
	AIC_MISC_NEXTWINDOW = 7639,
};

/*
 * IDE message identifiers.
 */
enum ADDIN_MESSAGE_IDENTIFIERS {
	/* Application window messages */
	AIM_ADD_COMMAND = (WM_APP+1000),
	AIM_REMOVE_COMMAND = (WM_APP+1001),
	AIM_GET_ACTIVE_DOCUMENT = (WM_APP+1002),
	AIM_NEW_DOCUMENT = (WM_APP+1003),
	AIM_OPEN_DOCUMENT = (WM_APP+1004),
	AIM_ENUM_DOCUMENTS = (WM_APP+1046),  /* (2.90) */
	AIM_ADD_TAB_PAGE = (WM_APP+1005),
	AIM_REMOVE_TAB_PAGE = (WM_APP+1006),
	AIM_SELECT_TAB_PAGE = (WM_APP+1007),
	AIM_WRITE_OUTPUT = (WM_APP+1041),  /* (2.90) */
	AIM_ADD_FILE_TYPE = (WM_APP+1042),  /* (2.90) */
	/* Project window messages */
	AIM_ADD_PROJECT_FILE = (WM_APP+1008),
	AIM_DELETE_PROJECT_FILE = (WM_APP+1009),
	AIM_ENUM_PROJECT_FILES = (WM_APP+1043),  /* (2.90) */
	AIM_GET_PROJECT_SYMBOL = (WM_APP+1010),
	AIM_SET_PROJECT_SYMBOL = (WM_APP+1011),
	AIM_GET_PROJECT_SHELLS = (WM_APP+1012),
	AIM_SET_PROJECT_SHELLS = (WM_APP+1013),
	AIM_DID_PROJECT_BUILD_FAIL = (WM_APP+1045),  /* (2.90) */
	/* Document window messages */
	AIM_GET_DOCUMENT_TYPE = (WM_APP+1014),
	AIM_GET_DOCUMENT_INFO = (WM_APP+1015),
	AIM_SET_DOCUMENT_INFO = (WM_APP+1016),
	AIM_CAN_DOCUMENT_UNDO = (WM_APP+1017),
	AIM_DOCUMENT_UNDO = (WM_APP+1018),
	AIM_FORGET_DOCUMENT_UNDO = (WM_APP+1019),
	AIM_SET_BREAKPOINT = (WM_APP+1020),
	AIM_REMOVE_BREAKPOINTS = (WM_APP+1021),
	AIM_ENUM_BREAKPOINTS = (WM_APP+1044),	/* (2.90) */
	/* Source document window messages */
	AIM_GET_SOURCE_TEXT = (WM_APP+1022),
	AIM_GET_SOURCE_TEXT_LENGTH = (WM_APP+1023),
	AIM_SET_SOURCE_TEXT = (WM_APP+1024),
	AIM_GET_SOURCE_LINE = (WM_APP+1025),
	AIM_GET_SOURCE_LINE_LENGTH = (WM_APP+1026),
	AIM_GET_SOURCE_SELECTION_TEXT = (WM_APP+1027),
	AIM_REPLACE_SOURCE_SELECTION_TEXT = (WM_APP+1028),
	AIM_GET_SOURCE_SELECTION = (WM_APP+1029),
	AIM_SET_SOURCE_SELECTION = (WM_APP+1030),
	AIM_GET_SOURCE_LINE_COUNT = (WM_APP+1031),
	AIM_SOURCE_LINE_INDEX = (WM_APP+1032),
	AIM_FIRST_VISIBLE_SOURCE_LINE = (WM_APP+1033),
	AIM_SOURCE_LINE_FROM_CHAR = (WM_APP+1034),
	AIM_SOURCE_CHAR_FROM_POS = (WM_APP+1035),
	AIM_SOURCE_POS_FROM_CHAR = (WM_APP+1036),
	AIM_SCROLL_SOURCE_CARET = (WM_APP+1037),
	AIM_FIND_SOURCE_TEXT = (WM_APP+1038),
	AIM_GET_SOURCE_INFO = (WM_APP+1039),
	AIM_SET_SOURCE_INFO = (WM_APP+1040),
};

typedef struct tagADDIN_ADD_COMMAND {
	UINT cbSize;						// Size of this structure (version control).
	int id; 							// Command identifier.
	LPCTSTR pszText;					// Display name.
	HICON hIcon;						// Display icon.
	int idMenu;							// Menu id or AIM_MENU_NOTHING for button (enum ADDIN_MENU_IDENTIFIERS) (2.90).
} ADDIN_ADD_COMMAND, *PADDIN_ADD_COMMAND;

typedef struct tagADDIN_DOCUMENT_INFO {
	UINT cbSize;						// Size of this structure (version control).
	int nType;							// Document type (enum ADDIN_WINDOW_TYPES).
	BOOL fDirty;						// Is the document dirty?
	FILETIME ftLastWriteTime;			// When was the document last written?
	TCHAR szFilename[260];				// Document file name (empty string if no name).
} ADDIN_DOCUMENT_INFO, *PADDIN_DOCUMENT_INFO;

typedef struct tagADDIN_SOURCE_INFO {
	UINT cbSize;						// Size of the structure (version control).
	BOOL fReadOnly; 					// Is the document read-only?
	BOOL fInsertMode;					// Is the source editor in insert mode?
	BOOL fCRBreak;						// <CR> is part of line breaks?
	BOOL fLFBreak;						// <LF> is part of line breaks?
	BOOL fWhiteSpace;					// Show white-space (global setting)?
	BOOL fBreaks;						// Show line breaks (global setting)?
	BOOL fLineNumbers;					// Show line numbers (global setting)?
	BOOL fFoldings;						// Show foldings (global setting)? (2.90)
	BOOL fCallTips;						// Use call tips (global setting)? (2.90)
	BOOL fMemberList;					// Use member list (global setting)? (2.90)
	BOOL fDragDrop;						// Use drag-and-drop editing (global setting)? (2.90)
	BOOL fCtrlChars;					// Accept control characters when typing (global setting)? (2.90)
	BOOL fWrapFindQuery;				// Question before search wrap (global setting)? (2.90)
} ADDIN_SOURCE_INFO, *PADDIN_SOURCE_INFO;

typedef struct tagADDIN_RANGE {
	int iStartPos;						// Starting position.
	int iEndPos;						// Ending position.
} ADDIN_RANGE, *PADDIN_RANGE;

typedef struct tagADDIN_ENUM_DOCUMENTS {
	UINT cbSize;						// Size of this structure (version control).
	BOOL (CALLBACK *pfnCallback)(HWND hwndDoc, LPVOID pvData);  // Callback function.
	LPVOID pvData;						// User data for callback.
} ADDIN_ENUM_DOCUMENTS, *PADDIN_ENUM_DOCUMENTS;

typedef struct tagADDIN_ENUM_PROJECT_FILES {
	UINT cbSize;						// Size of this structure (version control).
	BOOL (CALLBACK *pfnCallback)(LPCTSTR pszFilename, LPVOID pvData);  // Callback function.
	enum {
		EPFF_TARGET_FILES = 0x0001,
		EPFF_DEPENDENT_FILES = 0x0002,
		EPFF_ALL_FILES = EPFF_TARGET_FILES|EPFF_DEPENDENT_FILES
	} fuFlags;							// Flags.
	LPVOID pvData;						// User data for callback.
} ADDIN_ENUM_PROJECT_FILES, *PADDIN_ENUM_PROJECT_FILES;

typedef struct tagADDIN_ENUM_BREAKPOINTS {
	UINT cbSize;						// Size of this structure (version control).
	BOOL (CALLBACK *pfnCallback)(int iLine, LPVOID pvData);  // Callback function.
	LPVOID pvData;						// User data for callback.
} ADDIN_ENUM_BREAKPOINTS, *PADDIN_ENUM_BREAKPOINTS;

typedef enum tagADDIN_PARSER_COLORS {
    ADDIN_COLOR_TEXT = 0,				// Normal text.
    ADDIN_COLOR_KEYWORD = 2,			// Keyword.
    ADDIN_COLOR_COMMENT = 3,			// Comment.
    ADDIN_COLOR_NUMBER = 4,				// Number.
    ADDIN_COLOR_STRING = 5,				// String literal.
    ADDIN_COLOR_PREPROCESSOR = 6,		// Preprocessor directive.
    ADDIN_COLOR_FUNCTION = 7,			// Function.
    ADDIN_COLOR_OPERATOR = 8,			// Operator.
} ADDIN_PARSER_COLORS;

typedef struct tagADDIN_PARSE_POINT {
    int iChar;							// Column position (from zero) where text changes...
    int iColor;							// ...to this color (enum ADDIN_PARSER_COLORS).
} ADDIN_PARSE_POINT;

typedef struct tagADDIN_ADD_FILE_TYPE {
	UINT cbSize;						// Size of this structure (version control).
	LPCTSTR pszDescription;				// File description.
	LPCTSTR pszExtension;				// File extension (without dot, like "BAS").
	USHORT (CALLBACK *pfnParser)(USHORT usCookie, LPCTSTR pszText, int cchText, ADDIN_PARSE_POINT aPoints[], PINT pcPoints);  // Source code parser callback, or NULL.
	DWORD reserved;						// (Reserved).
	LPVOID pvReserved;					// (Reserved).
} ADDIN_ADD_FILE_TYPE, *PADDIN_ADD_FILE_TYPE;

// Parser helper macros.
#define ADDIN_MAKE_COOKIE(flags,level)  MAKEWORD((flags),(level))
#define ADDIN_GET_COOKIE_LEVEL(cookie)  HIBYTE(cookie)
#define ADDIN_GET_COOKIE_FLAGS(cookie)  LOBYTE(cookie)

/****** Message API ********************************************************/

#define AddIn_SendIDECommand(hwnd,id)  (void)SNDMSG((hwnd),WM_COMMAND,MAKEWPARAM((UINT)(id),0),0)
#define AddIn_AddCommand(hwnd,pAddCmd)  (BOOL)SNDMSG((hwnd),AIM_ADD_COMMAND,0,(LPARAM)(PADDIN_ADD_COMMAND)(pAddCmd))
#define AddIn_RemoveCommand(hwnd,id)  (BOOL)SNDMSG((hwnd),AIM_REMOVE_COMMAND,(WPARAM)(id),0)
#define AddIn_GetActiveDocument(hwnd)  (HWND)SNDMSG((hwnd),AIM_GET_ACTIVE_DOCUMENT,0,0)
#define AddIn_NewDocument(hwnd,nType)  (HWND)SNDMSG((hwnd),AIM_NEW_DOCUMENT,(WPARAM)(nType),0)
#define AddIn_OpenDocument(hwnd,nType,pszFile)  (HWND)SNDMSG((hwnd),AIM_OPEN_DOCUMENT,(WPARAM)(nType),(LPARAM)(LPCTSTR)(pszFile))
#define AddIn_CloseDocument(hwnd)  (void)SNDMSG((hwnd),WM_CLOSE,0,0)
#define AddIn_EnumDocuments(hwnd,pEnum)  (BOOL)SNDMSG((hwnd),AIM_ENUM_DOCUMENTS,0,(LPARAM)(PADDIN_ENUM_DOCUMENTS)(pEnum))
#define AddIn_AddTabPage(hwnd,pszTitle,hwndPage)  (BOOL)SNDMSG((hwnd),AIM_ADD_TAB_PAGE,(WPARAM)(LPCTSTR)(pszTitle),(LPARAM)(HWND)(hwndPage))
#define AddIn_RemoveTabPage(hwnd,hwndPage)  (BOOL)SNDMSG((hwnd),AIM_REMOVE_TAB_PAGE,0,(LPARAM)(HWND)(hwndPage))
#define AddIn_SelectTabPage(hwnd,hwndPage)  (BOOL)SNDMSG((hwnd),AIM_SELECT_TAB_PAGE,0,(LPARAM)(hwndPage))
#define AddIn_ClearOutput(hwnd)  (BOOL)SNDMSG((hwnd),AIM_WRITE_OUTPUT,0,0)
#define AddIn_WriteOutput(hwnd,pszText)  (BOOL)SNDMSG((hwnd),AIM_WRITE_OUTPUT,0,(LPARAM)(LPCTSTR)(pszText))
#define AddIn_AddFileType(hwnd,pAddFile)  (BOOL)SNDMSG((hwnd),AIM_ADD_FILE_TYPE,0,(LPARAM)(PADDIN_ADD_FILE_TYPE)(pAddFile))
/**/
#define AddIn_AddCommand(hwnd,pAddCmd)  (BOOL)SNDMSG((hwnd),AIM_ADD_COMMAND,0,(LPARAM)(PADDIN_ADD_COMMAND)(pAddCmd))
#define AddIn_RemoveCommand(hwnd,id)  (BOOL)SNDMSG((hwnd),AIM_REMOVE_COMMAND,(WPARAM)(id),0)
#define AddIn_AddProjectFile(hwnd,pszFile)  (BOOL)SNDMSG((hwnd),AIM_ADD_PROJECT_FILE,0,(LPARAM)(LPCTSTR)(pszFile))
#define AddIn_DeleteProjectFile(hwnd,pszFile)  (BOOL)SNDMSG((hwnd),AIM_DELETE_PROJECT_FILE,0,(LPARAM)(LPCTSTR)(pszFile))
#define AddIn_GetProjectSymbol(hwnd,pszName,pch,cchMax)  ((*((int *)(pch))=(cchMax)), ((int)SNDMSG((hwnd),AIM_GET_PROJECT_SYMBOL,(WPARAM)(LPCTSTR)(pszName),(LPARAM)(LPTSTR)(pch))))
#define AddIn_SetProjectSymbol(hwnd,pszName,psz)  (BOOL)SNDMSG((hwnd),AIM_SET_PROJECT_SYMBOL,(WPARAM)(LPCTSTR)(pszName),(LPARAM)(LPCTSTR)(psz))
#define AddIn_GetProjectShells(hwnd,pszFile,pch,cchMax)  ((*((int *)(pch))=(cchMax)), ((int)SNDMSG((hwnd),AIM_GET_PROJECT_SHELLS,(WPARAM)(LPCTSTR)(pszFile),(LPARAM)(LPTSTR)(pch))))
#define AddIn_SetProjectShells(hwnd,pszFile,psz)  (BOOL)SNDMSG((hwnd),AIM_SET_PROJECT_SHELLS,(WPARAM)(LPCTSTR)(pszFile),(LPARAM)(LPCTSTR)(psz))
#define AddIn_EnumProjectFiles(hwnd,pEnum)  (BOOL)SNDMSG((hwnd),AIM_ENUM_PROJECT_FILES,0,(LPARAM)(PADDIN_ENUM_PROJECT_FILES)(pEnum))
#define AddIn_DidProjectBuildFail(hwnd)  (BOOL)SNDMSG((hwnd),AIM_DID_PROJECT_BUILD_FAIL,0,0)
/**/
#define AddIn_GetDocumentType(hwnd)  (int)SNDMSG((hwnd),AIM_GET_DOCUMENT_TYPE,0,0)
#define AddIn_GetDocumentInfo(hwnd,pDocInfo)  (BOOL)SNDMSG((hwnd),AIM_GET_DOCUMENT_INFO,0,(LPARAM)(PADDIN_DOCUMENT_INFO)(pDocInfo))
#define AddIn_SetDocumentInfo(hwnd,pDocInfo)  (BOOL)SNDMSG((hwnd),AIM_SET_DOCUMENT_INFO,0,(LPARAM)(PADDIN_DOCUMENT_INFO)(pDocInfo))
#define AddIn_CanDocumentUndo(hwnd)  (BOOL)SNDMSG((hwnd),AIM_CAN_DOCUMENT_UNDO,0,0)
#define AddIn_DocumentUndo(hwnd)  (void)SNDMSG((hwnd),AIM_DOCUMENT_UNDO,0,0)
#define AddIn_ForgetDocumentUndo(hwnd)  (void)SNDMSG((hwnd),AIM_FORGET_DOCUMENT_UNDO,0,0)
#define AddIn_SetBreakpoint(hwnd,line)  (BOOL)SNDMSG((hwnd),AIM_SET_BREAKPOINT,(WPARAM)(int)(line),0)
#define AddIn_RemoveBreakpoints(hwnd)  (void)SNDMSG((hwnd),AIM_REMOVE_BREAKPOINTS,0,0)
#define AddIn_EnumBreakpoints(hwnd,pEnum)  (BOOL)SNDMSG((hwnd),AIM_ENUM_BREAKPOINTS,0,(LPARAM)(PADDIN_ENUM_BREAKPOINTS)(pEnum))
/**/
#define AddIn_GetSourceText(hwnd,pch,cchMax)  (int)SNDMSG((hwnd),AIM_GET_SOURCE_TEXT,(WPARAM)(int)(cchMax),(LPARAM)(LPTSTR)(pch))
#define AddIn_GetSourceTextLength(hwnd)  (int)SNDMSG((hwnd),AIM_GET_SOURCE_TEXT_LENGTH,0,0)
#define AddIn_SetSourceText(hwnd,psz)  (BOOL)SNDMSG((hwnd),AIM_SET_SOURCE_TEXT,0,(LPARAM)(LPCTSTR)(psz))
#define AddIn_GetSourceLine(hwnd,line,pch,cchMax)  ((*((int *)(pch))=(cchMax)), ((int)SNDMSG((hwnd),AIM_GET_SOURCE_LINE,(WPARAM)(int)(line),(LPARAM)(LPTSTR)(pch))))
#define AddIn_GetSourceLineLength(hwnd,line)  (int)SNDMSG((hwnd),AIM_GET_SOURCE_LINE_LENGTH,(WPARAM)(int)(line),0)
#define AddIn_GetSourceSelText(hwnd,pch,cchMax)  (int)SNDMSG((hwnd),AIM_GET_SOURCE_SELECTION_TEXT,(WPARAM)(int)(cchMax),(LPARAM)(LPTSTR)(pch))
#define AddIn_ReplaceSourceSelText(hwnd,psz)  (void)SNDMSG((hwnd),AIM_REPLACE_SOURCE_SELECTION_TEXT,0,(LPARAM)(LPCTSTR)(psz))
#define AddIn_GetSourceSel(hwnd,pRange)  (BOOL)SNDMSG((hwnd),AIM_GET_SOURCE_SELECTION,0,(LPARAM)(PADDIN_RANGE)(pRange))
#define AddIn_SetSourceSel(hwnd,pRange)  (BOOL)SNDMSG((hwnd),AIM_SET_SOURCE_SELECTION,0,(LPARAM)(PADDIN_RANGE)(pRange))
#define AddIn_GetSourceLineCount(hwnd)  (int)SNDMSG((hwnd),AIM_GET_SOURCE_LINE_COUNT,0,0)
#define AddIn_SourceLineIndex(hwnd,line)  (int)SNDMSG((hwnd),AIM_SOURCE_LINE_INDEX,(WPARAM)(int)(line),0)
#define AddIn_FirstVisibleSourceLine(hwnd)  (int)SNDMSG((hwnd),AIM_FIRST_VISIBLE_SOURCE_LINE,0,0)
#define AddIn_SourceLineFromChar(hwnd,ich)  (int)SNDMSG((hwnd),AIM_SOURCE_LINE_FROM_CHAR,(WPARAM)(int)(ich),0)
#define AddIn_SourceCharFromPos(hwnd,ppt)  (int)SNDMSG((hwnd),AIM_SOURCE_CHAR_FROM_POS,0,(LPARAM)(POINT*)(ppt))
#define AddIn_SourcePosFromChar(hwnd,ich,ppt)  (void)SNDMSG((hwnd),AIM_SOURCE_POS_FROM_CHAR,(WPARAM)(int)(ich),(LPARAM)(POINT*)(ppt))
#define AddIn_ScrollSourceCaret(hwnd)  (void)SNDMSG((hwnd),AIM_SCROLL_SOURCE_CARET,0,0)
#define AddIn_FindSourceText(hwnd,fuFlags,pszText)  (int)SNDMSG((hwnd),AIM_FIND_SOURCE_TEXT,(WPARAM)(UINT)(fuFlags),(LPARAM)(LPCTSTR)(pszText))
#define AddIn_GetSourceInfo(hwnd,pSrcInfo)  (BOOL)SNDMSG((hwnd),AIM_GET_SOURCE_INFO,0,(LPARAM)(PADDIN_SOURCE_INFO)(pSrcInfo))
#define AddIn_SetSourceInfo(hwnd,pSrcInfo)  (BOOL)SNDMSG((hwnd),AIM_SET_SOURCE_INFO,0,(LPARAM)(PADDIN_SOURCE_INFO)(pSrcInfo))

/****** Function prototypes ************************************************/

ADDINAPI BOOL WINAPI AddInMain(HWND hwnd, ADDIN_EVENT eEvent);  /* required export */
ADDINAPI void WINAPI AddInCommand(int idCmd);  /* optional [OBSOLETE] */
ADDINAPI void WINAPI AddInCommandEx(int idCmd, LPCVOID pcvData);  /* optional (2.90) */
ADDINAPI void WINAPI AddInSetup(HWND hwnd, LPCVOID pcvData);  /* optional (2.90) */

#endif /* _ADDIN_H */
