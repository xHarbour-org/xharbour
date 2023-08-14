/****************************************************************************
 *                                                                          *
 * File    : getch.c                                                        *
 *                                                                          *
 * Purpose : console read functions -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <conio.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    unsigned char lead_char;
    unsigned char second_char;
} char_pair;

typedef struct {
    unsigned short scan_code;
    char_pair reg_chars;
    char_pair shift_chars;
    char_pair ctrl_chars;
    char_pair alt_chars;
} enh_key_vals;

typedef struct {
    char_pair reg_chars;
    char_pair shift_chars;
    char_pair ctrl_chars;
    char_pair alt_chars;
} norm_key_vals;

/*
 * Table of key values for enhanced keys.
 */
static enh_key_vals enhanced_keys[] = {
    { 28, {  13,  0 }, {  13,  0 }, {  10,   0 }, { 0, 166 } },
    { 53, {  47,  0 }, {  63,  0 }, {   0, 149 }, { 0, 164 } },
    { 71, { 224, 71 }, { 224, 71 }, { 224, 119 }, { 0, 151 } },
    { 72, { 224, 72 }, { 224, 72 }, { 224, 141 }, { 0, 152 } },
    { 73, { 224, 73 }, { 224, 73 }, { 224, 134 }, { 0, 153 } },
    { 75, { 224, 75 }, { 224, 75 }, { 224, 115 }, { 0, 155 } },
    { 77, { 224, 77 }, { 224, 77 }, { 224, 116 }, { 0, 157 } },
    { 79, { 224, 79 }, { 224, 79 }, { 224, 117 }, { 0, 159 } },
    { 80, { 224, 80 }, { 224, 80 }, { 224, 145 }, { 0, 160 } },
    { 81, { 224, 81 }, { 224, 81 }, { 224, 118 }, { 0, 161 } },
    { 82, { 224, 82 }, { 224, 82 }, { 224, 146 }, { 0, 162 } },
    { 83, { 224, 83 }, { 224, 83 }, { 224, 147 }, { 0, 163 } }
};

/*
 * Table of key values for normal keys. Note that the table is padded so
 * that the key scan code serves as an index into the table.
 */
static norm_key_vals normal_keys[] = {
    { /*  0 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /*  1 */ {  27,   0 }, {  27,   0 }, {  27,   0 }, {   0,   1 } },
    { /*  2 */ {  49,   0 }, {  33,   0 }, {   0,   0 }, {   0, 120 } },
    { /*  3 */ {  50,   0 }, {  64,   0 }, {   0,   3 }, {   0, 121 } },
    { /*  4 */ {  51,   0 }, {  35,   0 }, {   0,   0 }, {   0, 122 } },
    { /*  5 */ {  52,   0 }, {  36,   0 }, {   0,   0 }, {   0, 123 } },
    { /*  6 */ {  53,   0 }, {  37,   0 }, {   0,   0 }, {   0, 124 } },
    { /*  7 */ {  54,   0 }, {  94,   0 }, {  30,   0 }, {   0, 125 } },
    { /*  8 */ {  55,   0 }, {  38,   0 }, {   0,   0 }, {   0, 126 } },
    { /*  9 */ {  56,   0 }, {  42,   0 }, {   0,   0 }, {   0, 127 } },
    { /* 10 */ {  57,   0 }, {  40,   0 }, {   0,   0 }, {   0, 128 } },
    { /* 11 */ {  48,   0 }, {  41,   0 }, {   0,   0 }, {   0, 129 } },
    { /* 12 */ {  45,   0 }, {  95,   0 }, {  31,   0 }, {   0, 130 } },
    { /* 13 */ {  61,   0 }, {  43,   0 }, {   0,   0 }, {   0, 131 } },
    { /* 14 */ {   8,   0 }, {   8,   0 }, { 127,   0 }, {   0,  14 } },
    { /* 15 */ {   9,   0 }, {   0,  15 }, {   0, 148 }, {   0,  15 } },
    { /* 16 */ { 113,   0 }, {  81,   0 }, {  17,   0 }, {   0,  16 } },
    { /* 17 */ { 119,   0 }, {  87,   0 }, {  23,   0 }, {   0,  17 } },
    { /* 18 */ { 101,   0 }, {  69,   0 }, {   5,   0 }, {   0,  18 } },
    { /* 19 */ { 114,   0 }, {  82,   0 }, {  18,   0 }, {   0,  19 } },
    { /* 20 */ { 116,   0 }, {  84,   0 }, {  20,   0 }, {   0,  20 } },
    { /* 21 */ { 121,   0 }, {  89,   0 }, {  25,   0 }, {   0,  21 } },
    { /* 22 */ { 117,   0 }, {  85,   0 }, {  21,   0 }, {   0,  22 } },
    { /* 23 */ { 105,   0 }, {  73,   0 }, {   9,   0 }, {   0,  23 } },
    { /* 24 */ { 111,   0 }, {  79,   0 }, {  15,   0 }, {   0,  24 } },
    { /* 25 */ { 112,   0 }, {  80,   0 }, {  16,   0 }, {   0,  25 } },
    { /* 26 */ {  91,   0 }, { 123,   0 }, {  27,   0 }, {   0,  26 } },
    { /* 27 */ {  93,   0 }, { 125,   0 }, {  29,   0 }, {   0,  27 } },
    { /* 28 */ {  13,   0 }, {  13,   0 }, {  10,   0 }, {   0,  28 } },
    { /* 29 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 30 */ {  97,   0 }, {  65,   0 }, {   1,   0 }, {   0,  30 } },
    { /* 31 */ { 115,   0 }, {  83,   0 }, {  19,   0 }, {   0,  31 } },
    { /* 32 */ { 100,   0 }, {  68,   0 }, {   4,   0 }, {   0,  32 } },
    { /* 33 */ { 102,   0 }, {  70,   0 }, {   6,   0 }, {   0,  33 } },
    { /* 34 */ { 103,   0 }, {  71,   0 }, {   7,   0 }, {   0,  34 } },
    { /* 35 */ { 104,   0 }, {  72,   0 }, {   8,   0 }, {   0,  35 } },
    { /* 36 */ { 106,   0 }, {  74,   0 }, {  10,   0 }, {   0,  36 } },
    { /* 37 */ { 107,   0 }, {  75,   0 }, {  11,   0 }, {   0,  37 } },
    { /* 38 */ { 108,   0 }, {  76,   0 }, {  12,   0 }, {   0,  38 } },
    { /* 39 */ {  59,   0 }, {  58,   0 }, {   0,   0 }, {   0,  39 } },
    { /* 40 */ {  39,   0 }, {  34,   0 }, {   0,   0 }, {   0,  40 } },
    { /* 41 */ {  96,   0 }, { 126,   0 }, {   0,   0 }, {   0,  41 } },
    { /* 42 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 43 */ {  92,   0 }, { 124,   0 }, {  28,   0 }, {   0,   0 } },
    { /* 44 */ { 122,   0 }, {  90,   0 }, {  26,   0 }, {   0,  44 } },
    { /* 45 */ { 120,   0 }, {  88,   0 }, {  24,   0 }, {   0,  45 } },
    { /* 46 */ {  99,   0 }, {  67,   0 }, {   3,   0 }, {   0,  46 } },
    { /* 47 */ { 118,   0 }, {  86,   0 }, {  22,   0 }, {   0,  47 } },
    { /* 48 */ {  98,   0 }, {  66,   0 }, {   2,   0 }, {   0,  48 } },
    { /* 49 */ { 110,   0 }, {  78,   0 }, {  14,   0 }, {   0,  49 } },
    { /* 50 */ { 109,   0 }, {  77,   0 }, {  13,   0 }, {   0,  50 } },
    { /* 51 */ {  44,   0 }, {  60,   0 }, {   0,   0 }, {   0,  51 } },
    { /* 52 */ {  46,   0 }, {  62,   0 }, {   0,   0 }, {   0,  52 } },
    { /* 53 */ {  47,   0 }, {  63,   0 }, {   0,   0 }, {   0,  53 } },
    { /* 54 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 55 */ {  42,   0 }, {   0,   0 }, { 114,   0 }, {   0,   0 } },
    { /* 56 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 57 */ {  32,   0 }, {  32,   0 }, {  32,   0 }, {  32,   0 } },
    { /* 58 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 59 */ {   0,  59 }, {   0,  84 }, {   0,  94 }, {   0, 104 } },
    { /* 60 */ {   0,  60 }, {   0,  85 }, {   0,  95 }, {   0, 105 } },
    { /* 61 */ {   0,  61 }, {   0,  86 }, {   0,  96 }, {   0, 106 } },
    { /* 62 */ {   0,  62 }, {   0,  87 }, {   0,  97 }, {   0, 107 } },
    { /* 63 */ {   0,  63 }, {   0,  88 }, {   0,  98 }, {   0, 108 } },
    { /* 64 */ {   0,  64 }, {   0,  89 }, {   0,  99 }, {   0, 109 } },
    { /* 65 */ {   0,  65 }, {   0,  90 }, {   0, 100 }, {   0, 110 } },
    { /* 66 */ {   0,  66 }, {   0,  91 }, {   0, 101 }, {   0, 111 } },
    { /* 67 */ {   0,  67 }, {   0,  92 }, {   0, 102 }, {   0, 112 } },
    { /* 68 */ {   0,  68 }, {   0,  93 }, {   0, 103 }, {   0, 113 } },
    { /* 69 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 70 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 71 */ {   0,  71 }, {  55,   0 }, {   0, 119 }, {   0,   0 } },
    { /* 72 */ {   0,  72 }, {  56,   0 }, {   0, 141 }, {   0,   0 } },
    { /* 73 */ {   0,  73 }, {  57,   0 }, {   0, 132 }, {   0,   0 } },
    { /* 74 */ {   0,   0 }, {  45,   0 }, {   0,   0 }, {   0,   0 } },
    { /* 75 */ {   0,  75 }, {  52,   0 }, {   0, 115 }, {   0,   0 } },
    { /* 76 */ {   0,   0 }, {  53,   0 }, {   0,   0 }, {   0,   0 } },
    { /* 77 */ {   0,  77 }, {  54,   0 }, {   0, 116 }, {   0,   0 } },
    { /* 78 */ {   0,   0 }, {  43,   0 }, {   0,   0 }, {   0,   0 } },
    { /* 79 */ {   0,  79 }, {  49,   0 }, {   0, 117 }, {   0,   0 } },
    { /* 80 */ {   0,  80 }, {  50,   0 }, {   0, 145 }, {   0,   0 } },
    { /* 81 */ {   0,  81 }, {  51,   0 }, {   0, 118 }, {   0,   0 } },
    { /* 82 */ {   0,  82 }, {  48,   0 }, {   0, 146 }, {   0,   0 } },
    { /* 83 */ {   0,  83 }, {  46,   0 }, {   0, 147 }, {   0,   0 } },
    { /* 84 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 85 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 86 */ {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },  /* padding */
    { /* 87 */ { 224, 133 }, { 224, 135 }, { 224, 137 }, { 224, 139 } },
    { /* 88 */ { 224, 134 }, { 224, 136 }, { 224, 138 }, { 224, 140 } }
};

/*
 * This is the one character push-back buffer used by
 * _getch(), _getche() and _ungetch().
 */
static int chbuf = EOF;

/* declaration for console handle */
static int _coninpfh = -2;


/* one time initialization of console input */
static void init_conin(void)
{
    _coninpfh = (int)CreateFile("CONIN$", GENERIC_READ|GENERIC_WRITE,
        FILE_SHARE_READ|FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0);
}


/* look up the extended key code for a given event */
static char_pair *get_extended_key_code(KEY_EVENT_RECORD *event)
{
    unsigned long shift_state;
    char_pair *cp;
    int i;

    if ((shift_state = event->dwControlKeyState) & ENHANCED_KEY)
    {
        /*
         * Find the appropriate entry in enhanced_keys[]
         */
        for (cp = 0, i = 0; i < (sizeof(enhanced_keys) / sizeof(enhanced_keys[0])); i++)
        {
            if (enhanced_keys[i].scan_code == event->wVirtualScanCode)
            {
                if (shift_state & (LEFT_ALT_PRESSED|RIGHT_ALT_PRESSED))
                    cp = &enhanced_keys[i].alt_chars;
                else if (shift_state & (LEFT_CTRL_PRESSED|RIGHT_CTRL_PRESSED))
                    cp = &enhanced_keys[i].ctrl_chars;
                else if (shift_state & SHIFT_PRESSED)
                    cp = &enhanced_keys[i].shift_chars;
                else
                    cp = &enhanced_keys[i].reg_chars;
                break;
            }
        }
    }
    else
    {
        /*
         * Regular key or a keyboard event which shouldn't be recognized.
         * Determine which by getting the proper field of the proper
         * entry in normal_keys[], and examining the extended code.
         */
        if (shift_state & (LEFT_ALT_PRESSED|RIGHT_ALT_PRESSED))
            cp = &normal_keys[event->wVirtualScanCode].alt_chars;
        else if (shift_state & (LEFT_CTRL_PRESSED|RIGHT_CTRL_PRESSED))
            cp = &normal_keys[event->wVirtualScanCode].ctrl_chars;
        else if (shift_state & SHIFT_PRESSED)
            cp = &normal_keys[event->wVirtualScanCode].shift_chars;
        else
            cp = &normal_keys[event->wVirtualScanCode].reg_chars;

        if ((cp->lead_char != 0 && cp->lead_char != 224) || cp->second_char == 0)
            /* Must be a keyboard event which should not be recognized */
            cp = 0;
    }

    return cp;
}


/* direct console input without echo */
int __cdecl (_getch)(void)
{
    INPUT_RECORD conrec;
    unsigned long nread;
    unsigned long oldmode;
    char_pair *cp;
    int ch = 0;

    /* check pushback buffer */
    if (chbuf != EOF)
    {
        ch = (unsigned char)(chbuf & 0xFF);
        chbuf = EOF;
        return ch;
    }

    if (_coninpfh == -1)
        return EOF;

    if (_coninpfh == -2)
        init_conin();

    /* switch to raw mode */
    GetConsoleMode((HANDLE)_coninpfh, &oldmode);
    SetConsoleMode((HANDLE)_coninpfh, 0);

    for (;;)
    {
        /* get a console input event */
        if (!ReadConsoleInput((HANDLE)_coninpfh, &conrec, 1, &nread) || nread == 0)
        {
            ch = EOF;
            break;
        }

        /* look for, and decipher, key events */
        if (conrec.EventType == KEY_EVENT && conrec.Event.KeyEvent.bKeyDown)
        {
            /*
             * Easy case: if uChar.AsciiChar is non-zero, just stuff it
             * into ch and quit.
             */
            if ((ch = (unsigned char)conrec.Event.KeyEvent.uChar.AsciiChar) != 0)
                break;

            /*
             * Hard case: either an extended code or an event which should
             * not be recognized. let get_extended_key_code() do the work...
             */
            if ((cp = get_extended_key_code(&conrec.Event.KeyEvent)) != NULL)
            {
                ch = cp->lead_char;
                chbuf = cp->second_char;
                break;
            }
        }
    }

    /* restore previous console mode */
    SetConsoleMode((HANDLE)_coninpfh, oldmode);

    return ch;
}

/* direct console input with echo */
int __cdecl (_getche)(void)
{
    int ch;

    /* check pushback buffer */
    if (chbuf != EOF)
    {
        /* return without echoing */
        ch = (unsigned char)(chbuf & 0xFF);
        chbuf = EOF;
        return ch;
    }

    ch = _getch();

    if (ch != EOF)
    {
        if (_putch(ch) != EOF)
            return ch;
    }

    return EOF;
}


/* push back one character for _getch and _getche */
int __cdecl (_ungetch)(int c)
{
    /* fail if the char is EOF or the pushback buffer is non-empty */
    if (c == EOF || chbuf != EOF)
        return EOF;

    chbuf = (c & 0xFF);
    return chbuf;
}


/* check if a keystroke is waiting to be read */
int __cdecl (_kbhit)(void)
{
    INPUT_RECORD *ip;
    DWORD npending;
    DWORD npeeked;

    if (chbuf != -1)
        return 1;

    if (_coninpfh == -2)
        init_conin();

    /* peek all pending console events */
    if (_coninpfh == -1 || !GetNumberOfConsoleInputEvents((HANDLE)_coninpfh, &npending) ||
        npending == 0 || (ip = (INPUT_RECORD *)_alloca(npending * sizeof(INPUT_RECORD))) == 0)
    {
        return 0;
    }

    if (PeekConsoleInput((HANDLE)_coninpfh, ip, npending, &npeeked) && npeeked && npeeked <= npending)
    {
        /*
         * Scan all of the peeked events to determine if any is a key event
         * which should be recognized.
         */
        for (; npeeked > 0; npeeked--, ip++)
        {
            if (ip->EventType == KEY_EVENT && ip->Event.KeyEvent.bKeyDown &&
                (ip->Event.KeyEvent.uChar.AsciiChar || get_extended_key_code(&ip->Event.KeyEvent)))
            {
                /*
                 * Key event corresponding to an ASCII character or an
                 * extended code. In either case, success!
                 */
                return 1;
            }
        }
    }

    return 0;
}

