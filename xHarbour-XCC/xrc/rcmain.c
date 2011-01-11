/****************************************************************************
 *                                                                          *
 * File    : rcmain.c                                                       *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; main loop.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-06-22  Handling of generic resource types added.            *
 *           98-06-22  Special handling of WAVE resource type removed.      *
 *           98-11-08  Resource type HTML added.                            *
 *           00-01-23  Resource types PLUGPLAY and VXD added.               *
 *           01-05-24  Resource type MANIFEST added.                        *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

static STRINGTABLE stringtable = {0};

struct ctlclass
{
    ushort_t classid;
    char * const classname;
}

static ctlclass[] =
{
    RSRC_C_BUTTON,    "button",
    RSRC_C_EDIT,      "edit",
    RSRC_C_STATIC,    "static",
    RSRC_C_LISTBOX,   "listbox",
    RSRC_C_SCROLLBAR, "scrollbar",
    RSRC_C_COMBOBOX,  "combobox"
};

BOOL bWantsFile = FALSE;
BOOL bDialog    = FALSE;

#define SYMBOLUPPER(sym) \
    if ((sym)->tok == C_SYMBOL) ansiupper((sym)->str)

/* Static function prototypes */
static void rc_language(void);
static void rc_stringtable(void);
static void add_string(STRINGTABLE *, STRINGENTRY *);
static void add_table(RSRC_HDR_TAIL *, STRINGTABLE *);
static void rc_accelerators(SYMBOL *);
static void rc_accelopt(RSRC_ACCENT *, bool_t *);
static void add_accel(ACCELTABLE *, RSRC_ACCENT *);
static void free_accels(ACCELTABLE *);
static void rc_anicursor(SYMBOL *);
static void rc_aniicon(SYMBOL *);
static void rc_bitmap(SYMBOL *);
static void rc_cursor(SYMBOL *);
static void rc_html(SYMBOL *);
static void rc_icon(SYMBOL *);
static void rc_plugplay(SYMBOL *);
static void rc_vxd(SYMBOL *);
static void rc_manifest(SYMBOL *);
static void rc_dialog(SYMBOL *, bool_t);
static void rc_defcontrol(DIALOGHEADER *, ushort_t, ulong_t);
static void rc_control(DIALOGHEADER *, ulong_t);
static void rc_dlginclude(SYMBOL *);
static void rc_menu(SYMBOL *);
static void rc_menuitemopt(MENUTREE *);
static void rc_menuex(SYMBOL *);
static void add_menuitem(MENUTREE *, MENUTREE *);
static void free_menu(MENUTREE *);
static void rc_messagetable(SYMBOL *);
static void rc_rcdata(SYMBOL *);
static void rc_version(SYMBOL *);
static void rc_fixedverinfo(RSRC_VERFIX *);
static void rc_stringfileinfo(VERSTRINGTABLE **);
static void add_verstring(VERSTRINGTABLE *, VERSTRINGENTRY *);
static void free_verstrings(VERSTRINGTABLE *);
static void rc_varfileinfo(VERVARTABLE **);
static void add_vervalue(VERVARTABLE *, ulong_t);
static void free_vervalues(VERVARTABLE *);
static void rc_generic(SYMBOL *, SYMBOL *);
static ushort_t rc_memflags(ushort_t);
static void rc_options(RSRC_HDR_TAIL *, DIALOGHEADER *);
static void print_info(const char *, SYMBOL *, RSRC_HDR_TAIL *);

/****************************************************************************
 *                                                                          *
 * Function: rc_main                                                        *
 *                                                                          *
 * Purpose : Main function for compiling resource scripts.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-11-08  HTML resource type added.                            *
 *           00-01-23  PLUGPLAY and VXD resource types added.               *
 *           01-11-23  Catch disallowed addition of symbol with constant.   *
 *                                                                          *
 ****************************************************************************/

void rc_main(const char *rcfile, const char *resfile)
{
    enum rctoken tok;
    SYMBOL sym;
    SYMBOL restype;

    if (info_callback)
    {
        char buf[100];
        sprintmsg(buf, MSG_CODEPAGE_INFO, codepage);
        info_callback(buf);
    }

    pp_start(rcfile);
    res_start(resfile);

    for (tok = getsymbol(); tok != C_EOF; tok = s.sym.tok)
    {
        if (quitrun)
            longjmp(jumpbuf, 1);

        switch (tok)
        {
            case C_LINESYNC:
                if (getsymbol() == C_NUMBER)
                {
                    s.line = s.sym.num-1;  /* compensate */
                    if (getsymbol() ==  C_STRING)
                    {
                        strcpy(s.filename, s.sym.str);
                        getsymbol();
                    }
                }
                break;

            case C_LANGUAGE:
                getsymbol();
                rc_language();
                break;

            case C_STRINGTABLE:
                getsymbol();
                rc_stringtable();
                break;

            case C_NUMBER:
            case C_SYMBOL:
                sym = s.sym;
                SYMBOLUPPER(&sym);

                switch (getsymbol())
                {
                    case C_ACCELERATORS:
                        getsymbol();
                        rc_accelerators(&sym);
                        break;

                    case C_ANICURSOR:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_anicursor(&sym);
                        break;

                    case C_ANIICON:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_aniicon(&sym);
                        break;

                    case C_BITMAP:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_bitmap(&sym);
                        break;

                    case C_CURSOR:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_cursor(&sym);
                        break;

                    case C_DIALOG:
                        bDialog = TRUE;
                        getsymbol();
                        rc_dialog(&sym, FALSE);
                        bDialog = FALSE;
                        break;

                    case C_DIALOGEX:
                        bDialog = TRUE;
                        getsymbol();
                        rc_dialog(&sym, TRUE);
                        bDialog = FALSE;
                        break;

                    case C_DLGINCLUDE:
                        getsymbol();
                        rc_dlginclude(&sym);
                        break;

                    case C_FONT:
                        apperror(RCFATAL(ERROR_NOT_IMPLEMENTED));
                        break;

                    case C_HTML:
                        getsymbol();
                        rc_html(&sym);
                        break;

                    case C_ICON:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_icon(&sym);
                        break;

                    case C_MANIFEST:
                        bWantsFile = TRUE;
                        getsymbol();
                        bWantsFile = FALSE;
                        rc_manifest(&sym);
                        break;

                    case C_MENU:
                        getsymbol();
                        rc_menu(&sym);
                        break;

                    case C_MENUEX:
                        getsymbol();
                        rc_menuex(&sym);
                        break;

                    case C_MESSAGETABLE:
                        getsymbol();
                        rc_messagetable(&sym);
                        break;

                    case C_PLUGPLAY:
                        getsymbol();
                        rc_plugplay(&sym);
                        break;

                    case C_RCDATA:
                        getsymbol();
                        rc_rcdata(&sym);
                        break;

                    case C_VERSIONINFO:
                        getsymbol();
                        rc_version(&sym);
                        break;

                    case C_VXD:
                        getsymbol();
                        rc_vxd(&sym);
                        break;

                    case C_NUMBER:
                    case C_SYMBOL:
                        restype = s.sym;
                        SYMBOLUPPER(&restype);
                        getsymbol();
                        rc_generic(&sym, &restype);
                        break;

                    case C_PLUS:
                        /* catch the case "WM_USER+5 ICON "filename" */
                        apperror(RCERROR(ERROR_PLUS_NOT_ALLOWED));
                        break;

                    default:
                        break;
                }
                break;

            default:
                getsymbol();
                break;
        }
    }

    /* Don't report error with file/lineno */
    s.filename[0] = '\0'; s.line = 0;

    /* Finally, write the string table */
    apperror(write_stringtable(&stringtable));

    /* Close the output file */
    res_close();

    /* Close the input file */
    unset_source();
}

/****************************************************************************
 *                                                                          *
 * Function: rc_language                                                    *
 *                                                                          *
 * Purpose : Parse a LANGUAGE statement.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           01-11-21  Call to print_info() removed.                        *
 *                                                                          *
 ****************************************************************************/

static void rc_language(void)
{
    ushort_t lang;
    ushort_t sublang;

    lang = accept_ushort();
    sublang = accept_ushort();

    commontail.language = MAKELANGID(lang, sublang);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_stringtable                                                 *
 *                                                                          *
 * Purpose : Parse a STRINGTABLE statement.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_stringtable(void)
{
    RSRC_HDR_TAIL tail = commontail;
    STRINGTABLE table = {0};

    tail.memflags = rc_memflags(RSRC_F_DISCARDABLE|RSRC_F_PURE|RSRC_F_MOVEABLE);

    /* Parse optional statements */
    rc_options(&tail, NULL);

    if (info_callback)
        print_info("STRINGTABLE", NULL, &tail);

    accept_symbol(C_BEGIN);

    do
    {
        ushort_t id = accept_ushort();

        if (s.sym.tok == C_STRING)
        {
            STRINGENTRY *entry = my_alloc(sizeof(STRINGENTRY));
            memset(entry, 0, sizeof(*entry));

            entry->pos = (id & 0x000F);
            entry->block = (id >> 4) + 1;
            entry->string = my_alloc((strlen(s.sym.str)+2) * sizeof(wchar_t));
            ansi2uni(&entry->string[1], s.sym.str);
            entry->string[0] = strlen(s.sym.str);
            add_string(&table, entry);
        }
        accept_symbol(C_STRING);

    } while (!trysymbol(C_END));

    add_table(&tail, &table);
    my_free(table.strings);
}

/****************************************************************************
 *                                                                          *
 * Function: add_string                                                     *
 *                                                                          *
 * Purpose : Add a string to the dynamic array.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-08-22  Simplified logic.                                    *
 *                                                                          *
 ****************************************************************************/

static void add_string(STRINGTABLE *table, STRINGENTRY *entry)
{
    if (table->num == table->max)
    {
        table->max += 10;
        table->strings = (STRINGENTRY **)my_realloc(
            table->strings, table->max * sizeof(STRINGENTRY *));
    }

    table->strings[table->num++] = entry;
}

/****************************************************************************
 *                                                                          *
 * Function: add_table                                                      *
 *                                                                          *
 * Purpose : Add a string array to the global string table.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_table(RSRC_HDR_TAIL *tailp, STRINGTABLE *table)
{
    int i;

    for (i = 0; i < table->num; i++)
    {
        table->strings[i]->tail = *tailp;
        add_string(&stringtable, table->strings[i]);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_accelerators                                                *
 *                                                                          *
 * Purpose : Parse a ACCELERATORS statement.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_accelerators(SYMBOL *symp)
{
    RSRC_HDR_TAIL tail = commontail;
    ACCELTABLE table = {0};

    tail.memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    /* Parse optional statements */
    rc_options(&tail, NULL);

    if (info_callback)
        print_info("ACCELERATORS", symp, &tail);

    accept_symbol(C_BEGIN);

    do
    {
        RSRC_ACCENT *entry = my_alloc(sizeof(RSRC_ACCENT));
        bool_t needtype = FALSE;

        memset(entry, 0, sizeof(*entry));

        if (s.sym.tok == C_NUMBER)
        {
            entry->key = accept_ushort();
            needtype = TRUE;
        }
        else
        {
            if (s.sym.tok == C_STRING)
            {
                if (s.sym.str[0] == '^')
                {
                    if (isalpha(s.sym.str[1]) && s.sym.str[2] == 0)
                        entry->key = toupper(s.sym.str[1])-'A'+1;
                    else
                        apperror(RCERROR(ERROR_INVALID_CONTROL_KEY));
                }
                else
                {
                    wchar_t wsz[256];  /* plenty of room, please */
                    ansi2uni(wsz, s.sym.str);
                    entry->key = wsz[0];
                }
            }
            accept_symbol(C_STRING);
        }

        entry->id = accept_ushort();

        /* Parse attributes */
        rc_accelopt(entry, &needtype);

        if (needtype)  /* sometimes we need a type... */
            apperror(RCERROR(ERROR_ACCEL_TYPE_NEEDED));

        add_accel(&table, entry);

    } while (!trysymbol(C_END));

    if (symp->tok == C_NUMBER)
        apperror(numbered_accelerators((wchar_t)symp->num, &tail, &table));
    else
        apperror(named_accelerators(symp->str, &tail, &table));

    /* Free temporary memory */
    free_accels(&table);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_accelopt                                                    *
 *                                                                          *
 * Purpose : Parse the accelerator options.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_accelopt(RSRC_ACCENT *entry, bool_t *needtype)
{
    for (;;)
    {
        switch (s.sym.tok)
        {
            case C_VIRTKEY:
                getsymbol();
                entry->flags |= RSRC_A_VIRTKEY;
                *needtype = FALSE;  /* found a type */
                break;

            case C_ASCII:
                getsymbol();
                *needtype = FALSE;  /* found a type */
                break;

            case C_NOINVERT:
                getsymbol();
                entry->flags |= RSRC_A_NOINVERT;
                break;

            case C_ALT:
                getsymbol();
                entry->flags |= RSRC_A_ALT;
                break;

            case C_SHIFT:
                getsymbol();
                entry->flags |= RSRC_A_SHIFT;
                break;

            case C_CONTROL:
                getsymbol();
                entry->flags |= RSRC_A_CONTROL;
                break;

            default:
                return;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: add_accel                                                      *
 *                                                                          *
 * Purpose : Add a accelerator to the dynamic array.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-08-22  Simplified logic.                                    *
 *                                                                          *
 ****************************************************************************/

static void add_accel(ACCELTABLE *table, RSRC_ACCENT *entry)
{
    if (table->num == table->max)
    {
        table->max += 10;
        table->entries = (RSRC_ACCENT **)my_realloc(
            table->entries, table->max * sizeof(RSRC_ACCENT *));
    }

    table->entries[table->num++] = entry;
}

/****************************************************************************
 *                                                                          *
 * Function: free_accels                                                    *
 *                                                                          *
 * Purpose : Free a dynamic accelerator array.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_accels(ACCELTABLE *table)
{
    int i;

    for (i = 0; i < table->num; i++)
        my_free(table->entries[i]);

    my_free(table->entries);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_anicursor                                                   *
 *                                                                          *
 * Purpose : Parse a ANICURSOR statement.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_anicursor(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("ANICURSOR", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_anicursor((wchar_t)symp->num, filename, memflags);
        else
            err = named_anicursor(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_aniicon                                                     *
 *                                                                          *
 * Purpose : Parse a ANIICON statement.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_aniicon(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("ANIICON", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_aniicon((wchar_t)symp->num, filename, memflags);
        else
            err = named_aniicon(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_bitmap                                                      *
 *                                                                          *
 * Purpose : Parse a BITMAP statement.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           03-08-02  Support for Borland style literal bitmaps added.     *
 *                                                                          *
 ****************************************************************************/

#define BIG_ENOUGH_CHUNK  1024
static void rc_bitmap(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("BITMAP", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_bitmap((wchar_t)symp->num, filename, memflags);
        else
            err = named_bitmap(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else /* parse literal Borland bitmap */
    {
        uchar_t *datap;
        size_t nb;
        size_t ndata;

        accept_symbol(C_BEGIN);

        datap = NULL;
        ndata = 0;
        nb = 0;

        do
        {
            if (s.sym.tok == C_BORLAND)  // 'XX XX XX XX ...'
            {
                char *p = s.sym.str, *end;
                while (*p)
                {
                    long num = strtol(p, &end, 16);

                    if (p == end || (*end != ' ' && *end != '\0'))
                    {
                        apperror(RCERROR(ERROR_INVALID_SYNTAX));
                        break;
                    }
                    else p = end;

                    if (nb < (ndata + sizeof(uchar_t)))
                        datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                    datap[ndata++] = num;
                }
                getsymbol();
            }
            else
            {
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();
            }
        } while (!trysymbol(C_END));

        if (symp->tok == C_NUMBER)
            apperror(numbered_literal_bitmap((wchar_t)symp->num, datap, ndata, memflags));
        else
            apperror(named_literal_bitmap(symp->str, datap, ndata, memflags));

        my_free(datap);
    }
}
#undef BIG_ENOUGH_CHUNK

/****************************************************************************
 *                                                                          *
 * Function: rc_cursor                                                      *
 *                                                                          *
 * Purpose : Parse a CURSOR statement.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           03-09-03  Support for Borland style literal icons added.       *
 *                                                                          *
 ****************************************************************************/

#define BIG_ENOUGH_CHUNK  1024
static void rc_cursor(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("CURSOR", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_cursor((wchar_t)symp->num, filename, memflags);
        else
            err = named_cursor(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else /* parse literal Borland cursor */
    {
        uchar_t *datap;
        size_t nb;
        size_t ndata;

        accept_symbol(C_BEGIN);

        datap = NULL;
        ndata = 0;
        nb = 0;

        do
        {
            if (s.sym.tok == C_BORLAND)  // 'XX XX XX XX ...'
            {
                char *p = s.sym.str, *end;
                while (*p)
                {
                    long num = strtol(p, &end, 16);

                    if (p == end || (*end != ' ' && *end != '\0'))
                    {
                        apperror(RCERROR(ERROR_INVALID_SYNTAX));
                        break;
                    }
                    else p = end;

                    if (nb < (ndata + sizeof(uchar_t)))
                        datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                    datap[ndata++] = num;
                }
                getsymbol();
            }
            else
            {
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();
            }
        } while (!trysymbol(C_END));

        if (symp->tok == C_NUMBER)
            apperror(numbered_literal_cursor((wchar_t)symp->num, datap, ndata, memflags));
        else
            apperror(named_literal_cursor(symp->str, datap, ndata, memflags));

        my_free(datap);
    }
}
#undef BIG_ENOUGH_CHUNK

/****************************************************************************
 *                                                                          *
 * Function: rc_html                                                        *
 *                                                                          *
 * Purpose : Parse a HTML statement.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_html(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("HTML", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_html((wchar_t)symp->num, filename, memflags);
        else
            err = named_html(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_icon                                                        *
 *                                                                          *
 * Purpose : Parse a ICON statement.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           03-09-03  Support for Borland style literal icons added.       *
 *                                                                          *
 ****************************************************************************/

#define BIG_ENOUGH_CHUNK  1024
static void rc_icon(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("ICON", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_icon((wchar_t)symp->num, filename, memflags);
        else
            err = named_icon(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else /* parse literal Borland icon */
    {
        uchar_t *datap;
        size_t nb;
        size_t ndata;

        accept_symbol(C_BEGIN);

        datap = NULL;
        ndata = 0;
        nb = 0;

        do
        {
            if (s.sym.tok == C_BORLAND)  // 'XX XX XX XX ...'
            {
                char *p = s.sym.str, *end;
                while (*p)
                {
                    long num = strtol(p, &end, 16);

                    if (p == end || (*end != ' ' && *end != '\0'))
                    {
                        apperror(RCERROR(ERROR_INVALID_SYNTAX));
                        break;
                    }
                    else p = end;

                    if (nb < (ndata + sizeof(uchar_t)))
                        datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                    datap[ndata++] = num;
                }
                getsymbol();
            }
            else
            {
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();
            }
        } while (!trysymbol(C_END));

        if (symp->tok == C_NUMBER)
            apperror(numbered_literal_icon((wchar_t)symp->num, datap, ndata, memflags));
        else
            apperror(named_literal_icon(symp->str, datap, ndata, memflags));

        my_free(datap);
    }
}
#undef BIG_ENOUGH_CHUNK

/****************************************************************************
 *                                                                          *
 * Function: rc_plugplay                                                    *
 *                                                                          *
 * Purpose : Parse a PLUGPLAY statement.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_plugplay(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("PLUGPLAY", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_plugplay((wchar_t)symp->num, filename, memflags);
        else
            err = named_plugplay(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_vxd                                                         *
 *                                                                          *
 * Purpose : Parse a VXD statement.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_vxd(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("VXD", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_vxd((wchar_t)symp->num, filename, memflags);
        else
            err = named_vxd(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_manifest                                                    *
 *                                                                          *
 * Purpose : Parse a MANIFEST statement.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-24  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_manifest(SYMBOL *symp)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    if (info_callback)
        print_info("MANIFEST", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_manifest((wchar_t)symp->num, filename, memflags);
        else
            err = named_manifest(symp->str, filename, memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_dialog                                                      *
 *                                                                          *
 * Purpose : Parse a DIALOG statement.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           00-01-22  Free dynamically allocated strings.                  *
 *           00-02-12  Added check for C_RBRACE.                            *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *                                                                          *
 ****************************************************************************/

static void rc_dialog(SYMBOL *symp, bool_t extended)
{
    RSRC_HDR_TAIL tail = commontail;
    DIALOGHEADER dlg = {0};
    CONTROLENTRY *ctl, *ctlnext;

    tail.memflags = rc_memflags(RSRC_F_PURE|RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE);

    dlg.extended = extended;
    dlg.x = accept_short();
    dlg.y = accept_short();
    dlg.width = accept_short();
    dlg.height = accept_short();
    dlg.helpid = (extended && s.sym.tok == C_NUMBER) ? accept_ulong() : 0;

    /* Parse optional statements */
    rc_options(&tail, &dlg);

    if (info_callback)
        print_info(extended ? "DIALOGEX" : "DIALOG", symp, &tail);

    accept_symbol(C_BEGIN);

    do
    {
        switch (s.sym.tok)
        {
            case C_AUTO3STATE:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_AUTO3STATE);
                break;

            case C_AUTOCHECKBOX:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_AUTOCHECKBOX|WS_TABSTOP);
                break;

            case C_AUTORADIOBUTTON:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_AUTORADIOBUTTON);
                break;

            case C_CHECKBOX:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_CHECKBOX|WS_TABSTOP);
                break;

            case C_DEFPUSHBUTTON:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_DEFPUSHBUTTON|WS_TABSTOP);
                break;

            case C_PUSHBUTTON:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_PUSHBUTTON|WS_TABSTOP);
                break;

            case C_GROUPBOX:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_GROUPBOX);
                break;

            case C_RADIOBUTTON:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_RADIOBUTTON);
                break;

            case C_STATE3:
                rc_defcontrol(&dlg, RSRC_C_BUTTON, BS_3STATE);
                break;

            case C_COMBOBOX:
                rc_defcontrol(&dlg, RSRC_C_COMBOBOX, 0);
                break;

            case C_CTEXT:
                rc_defcontrol(&dlg, RSRC_C_STATIC, SS_CENTER|WS_GROUP);
                break;

            case C_LTEXT:
                rc_defcontrol(&dlg, RSRC_C_STATIC, SS_LEFT|WS_GROUP);
                break;

            case C_RTEXT:
                rc_defcontrol(&dlg, RSRC_C_STATIC, SS_RIGHT|WS_GROUP);
                break;

            case C_ICON:
                rc_defcontrol(&dlg, RSRC_C_STATIC, SS_ICON);
                break;

            case C_EDITTEXT:
                rc_defcontrol(&dlg, RSRC_C_EDIT, ES_LEFT|WS_BORDER|WS_TABSTOP);
                break;

            case C_LISTBOX:
                rc_defcontrol(&dlg, RSRC_C_LISTBOX, WS_BORDER|LBS_NOTIFY);
                break;

            case C_SCROLLBAR:
                rc_defcontrol(&dlg, RSRC_C_SCROLLBAR, 0);
                break;

            case C_CONTROL:
                rc_control(&dlg, WS_CHILD|WS_VISIBLE);
                break;

            default:
                if (s.sym.tok != C_END && s.sym.tok != C_RBRACE)
                {
                    apperror(RCERROR(ERROR_INVALID_SYNTAX));
                    getsymbol();
                }
                break;
        }

    } while (!trysymbol(C_END));

    if (symp->tok == C_NUMBER)
        apperror(numbered_dialog((wchar_t)symp->num, &tail, &dlg));
    else
        apperror(named_dialog(symp->str, &tail, &dlg));

    for (ctl = dlg.next; ctl != NULL; ctl = ctlnext)
    {
        ctlnext = ctl->next;
        my_free(ctl->caption);
        my_free(ctl);
    }

    my_free(dlg.font.typeface);
    my_free(dlg.menu);
    my_free(dlg.class);
    my_free(dlg.caption);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_defcontrol                                                  *
 *                                                                          *
 * Purpose : Parse a control statement, as part of a dialog.                *
 *                                                                          *
 * Comment : The syntax for the supported controls are as follows:          *
 *                                                                          *
 *           AUTO3STATE:      text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           AUTOCHECKBOX:    text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           AUTORADIOBUTTON: text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           CHECKBOX:        text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           DEFPUSHBUTTON:   text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           PUSHBUTTON:      text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           GROUPBOX:        text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           RADIOBUTTON:     text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           STATE3:          text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           COMBOBOX:        text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           CTEXT:           text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           LTEXT:           text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           RTEXT:           text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           ICON:            text, id, x, y [, cx, cy [,style [,exstyle]]] *
 *           EDITTEXT:        text, id, x, y, cx, cy [,style [,exstyle]]    *
 *           LISTBOX:               id, x, y, cx, cy [,style [,exstyle]]    *
 *           SCROLLBAR:      ?text, id, x, y, cx, cy [,style [,exstyle]]    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-07-01  Changed rc_style() to accept_ulong_expression().     *
 *           00-01-22  Changed to dynamic allocation of strings.            *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *           04-01-28  Also accept strings for icons (not just ordinal).    *
 *                                                                          *
 ****************************************************************************/

static void rc_defcontrol(DIALOGHEADER *dlg, ushort_t class, ulong_t style)
{
    CONTROLENTRY *ctl = my_alloc(sizeof(CONTROLENTRY));
    CONTROLENTRY **ctlp;

    memset(ctl, 0, sizeof(*ctl));

    /* Put the allocated control node in the linked list */
    for (ctlp = &dlg->next; *ctlp != NULL; ctlp = &(*ctlp)->next)
        ;

    *ctlp = ctl;

    ctl->style = style|WS_CHILD|WS_VISIBLE;

    ctl->class[0] = (wchar_t)-1;
    ctl->class[1] = (wchar_t)class;

    /* Get past the control type */
    getsymbol();

    if (class == RSRC_C_STATIC && style == SS_ICON && s.sym.tok != C_STRING)
    {
        ctl->caption = (wchar_t *)my_alloc(2*sizeof(wchar_t));
        ctl->caption[0] = (wchar_t)-1;
        ctl->caption[1] = (wchar_t)accept_short();  /* icon ordinal */
    }
    else if (s.sym.tok == C_STRING)
    {
        ctl->caption = my_alloc(unisize(s.sym.str));
        ansi2uni(ctl->caption, s.sym.str);
        getsymbol();
    }

    ctl->id = accept_short();  /* must accept -1 */
    ctl->x = accept_short();
    ctl->y = accept_short();
    ctl->width = accept_short();
    ctl->height = accept_short();

    /*
     * Pass style bits to accept_ulong_expression, so that "NOT" will
     * turn off bits properly (old code didn't handle this case).
     */
    if (s.sym.tok == C_NUMBER || s.sym.tok == C_NOT)
        ctl->style = accept_ulong_expression(ctl->style);

    if (s.sym.tok == C_NUMBER || s.sym.tok == C_NOT)
        ctl->exstyle = accept_ulong_expression(0);

    ctl->helpid = (dlg->extended && s.sym.tok == C_NUMBER) ? accept_ulong() : 0;
}

/****************************************************************************
 *                                                                          *
 * Function: rc_control                                                     *
 *                                                                          *
 * Purpose : Parse a generic control statement, as part of a dialog.        *
 *                                                                          *
 *           CONTROL: text, id, class, style, x, y, cx, cy [,exstyle]       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-05-17  Icon ordinal (assumption) added.                     *
 *           98-07-01  Changed rc_style() to accept_ulong_expression().     *
 *           00-01-22  Changed to dynamic allocation of strings.            *
 *                                                                          *
 ****************************************************************************/

static void rc_control(DIALOGHEADER *dlg, ulong_t style)
{
    CONTROLENTRY *ctl = my_alloc(sizeof(CONTROLENTRY));
    CONTROLENTRY **ctlp;

    memset(ctl, 0, sizeof(*ctl));

    /* Put the allocated control node in the linked list */
    for (ctlp = &dlg->next; *ctlp != NULL; ctlp = &(*ctlp)->next)
        ;

    *ctlp = ctl;

    ctl->style = style|WS_CHILD|WS_VISIBLE;

    /* Get past the control type */
    getsymbol();

    if (s.sym.tok == C_STRING)
    {
        ctl->caption = my_alloc(unisize(s.sym.str));
        ansi2uni(ctl->caption, s.sym.str);
        getsymbol();
    }
    else if (s.sym.tok == C_NUMBER)  /* assume icon! */
    {
        ctl->caption = (wchar_t *)my_alloc(2*sizeof(wchar_t));
        ctl->caption[0] = (wchar_t)-1;
        ctl->caption[1] = (wchar_t)accept_short();  /* icon ordinal */
    }

    ctl->id = accept_short();  /* must accept -1 */

    if (s.sym.tok == C_STRING)
    {
        int i;

        for (i = 0; i < NELEMS(ctlclass); i++)
        {
            if (_stricmp(s.sym.str, ctlclass[i].classname) == 0)
            {
                ctl->class[0] = (wchar_t)-1;
                ctl->class[1] = (wchar_t)ctlclass[i].classid;
                break;
            }
        }

        if (i == NELEMS(ctlclass))
            ansi2uni(ctl->class, s.sym.str);
    }
    accept_symbol(C_STRING);

    /*
     * Pass style bits to accept_ulong_expression, so that "NOT" will
     * turn off bits properly (old code didn't handle this case).
     */
    ctl->style = accept_ulong_expression(ctl->style);

    ctl->x = accept_short();
    ctl->y = accept_short();
    ctl->width = accept_short();
    ctl->height = accept_short();

    if (s.sym.tok == C_NUMBER || s.sym.tok == C_NOT)
        ctl->exstyle = accept_ulong_expression(0);

    ctl->helpid = (dlg->extended && s.sym.tok == C_NUMBER) ? accept_ulong() : 0;
}

/****************************************************************************
 *                                                                          *
 * Function: rc_dlginclude                                                  *
 *                                                                          *
 * Purpose : Parse a DLGINCLUDE statement.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_dlginclude(SYMBOL *symp)
{
    if (s.sym.tok == C_STRING)
    {
        if (symp->tok == C_NUMBER)
            apperror(numbered_dlginclude((wchar_t)symp->num, s.sym.str));
        else
            apperror(named_dlginclude(symp->str, s.sym.str));
    }
    accept_symbol(C_STRING);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_menu                                                        *
 *                                                                          *
 * Purpose : Parse a MENU statement.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           00-02-12  Added check for C_RBRACE.                            *
 *                                                                          *
 ****************************************************************************/

static void rc_menu(SYMBOL *symp)
{
    RSRC_HDR_TAIL tail = commontail;
    MENUTREE *menu[8] = {0};
    int level = 0;

    tail.memflags = rc_memflags(RSRC_F_PURE|RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE);

    /* Parse optional statements */
    rc_options(&tail, NULL);

    if (info_callback)
        print_info("MENU", symp, &tail);

    accept_symbol(C_BEGIN);

    /* Allocate the root menu struct */
    menu[0] = (MENUTREE *)my_alloc(sizeof(MENUTREE));
    memset(menu[0], 0, sizeof(*menu[0]));

    while (level >= 0)
    {
        MENUTREE *item;

        switch (s.sym.tok)
        {
            case C_MENUITEM:
                item = (MENUTREE *)my_alloc(sizeof(MENUTREE));
                memset(item, 0, sizeof(*item));

                if (getsymbol() == C_STRING)
                {
                    ansi2uni(item->title, s.sym.str);
                    accept_symbol(C_STRING);

                    item->id = accept_ushort();

                    /* Parse optional attributes */
                    rc_menuitemopt(item);
                }
                else
                {
                    accept_symbol(C_SEPARATOR);
                    item->flags = RSRC_M_SEPARATOR;
                }

                add_menuitem(menu[level], item);
                break;

            case C_POPUP:
                level++;

                menu[level] = (MENUTREE *)my_alloc(sizeof(MENUTREE));
                memset(menu[level], 0, sizeof(*menu[level]));
                menu[level]->flags = RSRC_M_POPUP;

                if (getsymbol() == C_STRING)
                    ansi2uni(menu[level]->title, s.sym.str);
                accept_symbol(C_STRING);

                /* Parse optional attributes */
                rc_menuitemopt(menu[level]);

                accept_symbol(C_BEGIN);
                break;

            case C_END:
            case C_RBRACE:
                if (menu[level]->num > 0)
                    menu[level]->items[menu[level]->num-1]->flags |= RSRC_M_ENDMENU;
                if (--level >= 0)
                    add_menuitem(menu[level], menu[level+1]);
                getsymbol();
                break;

            default:
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();
                break;
        }
    }

    if (symp->tok == C_NUMBER)
        apperror(numbered_menu((wchar_t)symp->num, &tail, menu[0]));
    else
        apperror(named_menu(symp->str, &tail, menu[0]));

    /* Free temporary memory */
    free_menu(menu[0]);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_menuitemopt                                                 *
 *                                                                          *
 * Purpose : Parse a optional menuitem definition.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_menuitemopt(MENUTREE *item)
{
    for (;;)
    {
        switch (s.sym.tok)
        {
            case C_CHECKED:
                getsymbol();
                item->flags |= RSRC_M_CHECKED;
                break;

            case C_GRAYED:
                getsymbol();
                item->flags |= RSRC_M_GRAYED;
                break;

            case C_HELP:
                getsymbol();
                item->flags |= RSRC_M_HELP;
                break;

            case C_INACTIVE:
                getsymbol();
                item->flags |= RSRC_M_INACTIVE;
                break;

            case C_MENUBARBREAK:
                getsymbol();
                item->flags |= RSRC_M_MENUBARBREAK;
                break;

            case C_MENUBREAK:
                getsymbol();
                item->flags |= RSRC_M_MENUBREAK;
                break;

            default:
                return;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_menuex                                                      *
 *                                                                          *
 * Purpose : Parse a MENUEX statement.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-06-21  Bugfix: use menu[level] not item for C_POPUP.        *
 *                                                                          *
 ****************************************************************************/

static void rc_menuex(SYMBOL *symp)
{
    RSRC_HDR_TAIL tail = commontail;
    MENUTREE *menu[8] = {0};
    int level = 0;

    tail.memflags = rc_memflags(RSRC_F_PURE|RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE);

    /* Parse optional statements */
    rc_options(&tail, NULL);

    if (info_callback)
        print_info("MENUEX", symp, &tail);

    accept_symbol(C_BEGIN);

    /* Allocate the root menu struct */
    menu[0] = (MENUTREE *)my_alloc(sizeof(MENUTREE));
    memset(menu[0], 0, sizeof(*menu[0]));

    while (level >= 0)
    {
        MENUTREE *item;

        switch (s.sym.tok)
        {
            case C_MENUITEM:
                item = (MENUTREE *)my_alloc(sizeof(MENUTREE));
                memset(item, 0, sizeof(*item));

                if (getsymbol() == C_STRING)
                    ansi2uni(item->title, s.sym.str);
                accept_symbol(C_STRING);

                if (s.sym.tok == C_NUMBER)
                    item->id = accept_ushort();

                if (s.sym.tok == C_NUMBER)
                    item->type = accept_ulong_expression(0);

                if (s.sym.tok == C_NUMBER)
                    item->state = accept_ulong_expression(0);

                add_menuitem(menu[level], item);
                break;

            case C_POPUP:
                level++;

                menu[level] = (MENUTREE *)my_alloc(sizeof(MENUTREE));
                memset(menu[level], 0, sizeof(*menu[level]));
                menu[level]->flags = RSRC_M_POPUP;

                if (getsymbol() == C_STRING)
                    ansi2uni(menu[level]->title, s.sym.str);
                accept_symbol(C_STRING);

                if (s.sym.tok == C_NUMBER)
                    menu[level]->id = accept_ushort();

                if (s.sym.tok == C_NUMBER)
                    menu[level]->type = accept_ulong_expression(0);

                if (s.sym.tok == C_NUMBER)
                    menu[level]->state = accept_ulong_expression(0);

                if (s.sym.tok == C_NUMBER)
                    menu[level]->helpid = accept_ulong();

                accept_symbol(C_BEGIN);
                break;

            case C_END:
            case C_RBRACE:
                if (menu[level]->num > 0)
                    menu[level]->items[menu[level]->num-1]->flags |= RSRC_M_ENDMENU;
                if (--level >= 0)
                    add_menuitem(menu[level], menu[level+1]);
                getsymbol();
                break;

            default:
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();
                break;
        }
    }

    if (symp->tok == C_NUMBER)
        apperror(numbered_menuex((wchar_t)symp->num, &tail, menu[0]));
    else
        apperror(named_menuex(symp->str, &tail, menu[0]));

    /* Free temporary memory */
    free_menu(menu[0]);
}

/****************************************************************************
 *                                                                          *
 * Function: add_menuitem                                                   *
 *                                                                          *
 * Purpose : Add the parsed menu item to the menu tree.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-08-22  Simplified logic.                                    *
 *                                                                          *
 ****************************************************************************/

static void add_menuitem(MENUTREE *menu, MENUTREE *item)
{
    if (menu->num == menu->max)
    {
        menu->max += 10;
        menu->items = (MENUTREE **)my_realloc(
            menu->items, menu->max * sizeof(MENUTREE *));
    }

    menu->items[menu->num++] = item;
}

/****************************************************************************
 *                                                                          *
 * Function: free_menu                                                      *
 *                                                                          *
 * Purpose : Free a temporary menu tree.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_menu(MENUTREE *menu)
{
    int i;

    for (i = 0; i < menu->num; i++)
    {
        if (menu->items[i]->flags & RSRC_M_POPUP)
            free_menu(menu->items[i]);
        else
            my_free(menu->items[i]);
    }

    my_free(menu->items);
    my_free(menu);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_messagetable                                                *
 *                                                                          *
 * Purpose : Parse a MESSAGETABLE statement.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_messagetable(SYMBOL *symp)
{
    if (info_callback)
        print_info("MESSAGETABLE", symp, &commontail);

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_msgtable((wchar_t)symp->num, filename);
        else
            err = named_msgtable(symp->str, filename);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_rcdata                                                      *
 *                                                                          *
 * Purpose : Parse a RCDATA statement.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           00-11-22  Changed to accept_ulong_expression().                *
 *           02-03-04  Accept raw data from a file.                         *
 *           02-07-02  Added getsymbol() after syntax error!                *
 *                                                                          *
 ****************************************************************************/

#define BIG_ENOUGH_CHUNK  1024
static void rc_rcdata(SYMBOL *symp)
{
    RSRC_HDR_TAIL tail = commontail;

    tail.memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);

    /* Parse optional statements */
    rc_options(&tail, NULL);

    if (info_callback)
        print_info("RCDATA", symp, &tail);

    /* parse raw data from file */
    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
            err = numbered_generic_numbered_type((wchar_t)symp->num, RSRC_T_RCDATA, filename, tail.memflags);
        else
            err = named_generic_numbered_type(symp->str, RSRC_T_RCDATA, filename, tail.memflags);

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else /* parse literal raw data */
    {
        uchar_t *datap;
        size_t nb;
        size_t ndata;

        accept_symbol(C_BEGIN);

        datap = NULL;
        ndata = 0;
        nb = 0;

        do
        {
            if (s.sym.tok == C_NUMBER || s.sym.tok == C_NOT)
            {
                if (s.sym.is_long)
                {
                    long num = accept_ulong_expression(0);

                    if (nb < (ndata + sizeof(long)))
                        datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                    datap[ndata++] = LOBYTE(LOWORD(num));
                    datap[ndata++] = HIBYTE(LOWORD(num));
                    datap[ndata++] = LOBYTE(HIWORD(num));
                    datap[ndata++] = HIBYTE(HIWORD(num));
                }
                else
                {
                    long num = accept_ulong_expression(0);

                    if (nb < (ndata + sizeof(short)))
                        datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                    datap[ndata++] = LOBYTE(LOWORD(num));
                    datap[ndata++] = HIBYTE(LOWORD(num));
                }
            }
            else if (s.sym.tok == C_STRING)
            {
                int i;

                if (nb < (ndata + s.sym.nchars))
                    datap = (uchar_t *)my_realloc(datap, nb += BIG_ENOUGH_CHUNK);

                for (i = 0; i < s.sym.nchars; i++)
                    datap[ndata++] = (uchar_t)s.sym.str[i];

                getsymbol();
            }
            else if (s.sym.tok == C_UNISTRING)
            {
                apperror(RCFATAL(ERROR_NOT_IMPLEMENTED));
            }
            else
            {
                apperror(RCERROR(ERROR_INVALID_SYNTAX));
                getsymbol();  /* 02-07-02 */
            }

        } while (!trysymbol(C_END));

        if (symp->tok == C_NUMBER)
            apperror(numbered_rcdata((wchar_t)symp->num, &tail, datap, ndata));
        else
            apperror(named_rcdata(symp->str, &tail, datap, ndata));

        my_free(datap);
    }
}
#undef BIG_ENOUGH_CHUNK

/****************************************************************************
 *                                                                          *
 * Function: rc_version                                                     *
 *                                                                          *
 * Purpose : Parse a VERSIONINFO statement.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_version(SYMBOL *symp)
{
    RSRC_VERFIX vsffi = {0};
    VERSTRINGTABLE *strtab = NULL;
    VERVARTABLE *vartab = NULL;

    if (info_callback)
        print_info("VERSIONINFO", symp, NULL);

    vsffi.signature = RSRC_VF_SIGNATURE;
    vsffi.structver = RSRC_VF_STRUCTVER;

    /* Parse fixed part of resource */
    rc_fixedverinfo(&vsffi);

    accept_symbol(C_BEGIN);
    while (trysymbol(C_BLOCK))
    {
        if (s.sym.tok == C_STRING)
        {
            if (strcmp(s.sym.str, "StringFileInfo") == 0)
            {
                accept_symbol(C_STRING);
                accept_symbol(C_BEGIN);

                while (!trysymbol(C_END))
                    rc_stringfileinfo(&strtab);
            }
            else if (strcmp(s.sym.str, "VarFileInfo") == 0)
            {
                accept_symbol(C_STRING);
                accept_symbol(C_BEGIN);

                while (!trysymbol(C_END))
                    rc_varfileinfo(&vartab);
            }
            else /* international garbage club */
            {
                accept_symbol(C_STRING);
            }
        }
    }
    accept_symbol(C_END);

    if (symp->tok == C_NUMBER)
        apperror(numbered_version((wchar_t)symp->num, &vsffi, strtab, vartab));
    else
        apperror(named_version(symp->str, &vsffi, strtab, vartab));

    /* Free temporary memory */
    free_verstrings(strtab);
    free_vervalues(vartab);
}

/****************************************************************************
 *                                                                          *
 * Function: rc_fixedverinfo                                                *
 *                                                                          *
 * Purpose : Parse the fixed size version info.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-05-17  Changed to accept_ulong_expression().                *
 *                                                                          *
 ****************************************************************************/

static void rc_fixedverinfo(RSRC_VERFIX *vsffi)
{
    for (;;)
    {
        switch (s.sym.tok)
        {
            case C_FILEVERSION:
                getsymbol();
                /**/
                vsffi->file_hi_majver = accept_ushort();
                vsffi->file_hi_minver = accept_ushort();
                vsffi->file_lo_majver = accept_ushort();
                vsffi->file_lo_minver = accept_ushort();
                break;

            case C_PRODUCTVERSION:
                getsymbol();
                /**/
                vsffi->prod_hi_majver = accept_ushort();
                vsffi->prod_hi_minver = accept_ushort();
                vsffi->prod_lo_majver = accept_ushort();
                vsffi->prod_lo_minver = accept_ushort();
                break;

            case C_FILEFLAGSMASK:
                getsymbol();
                /**/
                vsffi->flagsmask = accept_ulong_expression(0);
                break;

            case C_FILEFLAGS:
                getsymbol();
                /**/
                vsffi->flags = accept_ulong_expression(0);
                break;

            case C_FILEOS:
                getsymbol();
                /**/
                vsffi->os = accept_ulong_expression(0);
                break;

            case C_FILETYPE:
                getsymbol();
                /**/
                vsffi->type = accept_ulong_expression(0);
                break;

            case C_FILESUBTYPE:
                getsymbol();
                /**/
                vsffi->subtype = accept_ulong_expression(0);
                break;

            default:
                return;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_stringfileinfo                                              *
 *                                                                          *
 * Purpose : Parse a language block in the "StringFileInfo" version.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_stringfileinfo(VERSTRINGTABLE **strtabp)
{
    VERSTRINGTABLE *strtab = my_alloc(sizeof(VERSTRINGTABLE));
    memset(strtab, 0, sizeof(*strtab));

    if (!*strtabp)
    {
        *strtabp = strtab;
    }
    else
    {
        while ((*strtabp)->next)
            (*strtabp) = (*strtabp)->next;
        (*strtabp)->next = strtab;
    }

    accept_symbol(C_BLOCK);
    ansi2uni(strtab->lang_charset, s.sym.str);
    accept_symbol(C_STRING);
    accept_symbol(C_BEGIN);

    do
    {
        VERSTRINGENTRY *entry = my_alloc(sizeof(VERSTRINGENTRY));
        memset(entry, 0, sizeof(*entry));

        accept_symbol(C_VALUE);

        if (s.sym.tok == C_STRING)
        {
            entry->key = my_alloc(unisize(s.sym.str));
            ansi2uni(entry->key, s.sym.str);
        }
        accept_symbol(C_STRING);

        if (s.sym.tok == C_STRING)
        {
            entry->val = my_alloc(unisize(s.sym.str));
            ansi2uni(entry->val, s.sym.str);
        }
        accept_symbol(C_STRING);

        add_verstring(strtab, entry);

    } while (!trysymbol(C_END));
}

/****************************************************************************
 *                                                                          *
 * Function: add_verstring                                                  *
 *                                                                          *
 * Purpose : Add a version string to a dynamic array.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-08-22  Simplified logic.                                    *
 *                                                                          *
 ****************************************************************************/

static void add_verstring(VERSTRINGTABLE *table, VERSTRINGENTRY *entry)
{
    if (table->num == table->max)
    {
        table->max += 10;
        table->strings = (VERSTRINGENTRY **)my_realloc(
            table->strings, table->max * sizeof(VERSTRINGENTRY *));
    }

    table->strings[table->num++] = entry;
}

/****************************************************************************
 *                                                                          *
 * Function: free_verstrings                                                *
 *                                                                          *
 * Purpose : Free a dynamic version string array.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_verstrings(VERSTRINGTABLE *table)
{
    VERSTRINGTABLE *table_next;
    int i;

    for (; table != NULL; table = table_next)
    {
        for (i = 0; i < table->num; i++)
        {
            my_free(table->strings[i]->key);
            my_free(table->strings[i]->val);
            my_free(table->strings[i]);
        }

        table_next = table->next;  /* before free! */

        my_free(table->strings);
        my_free(table);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_varfileinfo                                                 *
 *                                                                          *
 * Purpose : Parse a block in the "VarFileInfo" version.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           00-02-12  Added check for C_RBRACE.                            *
 *                                                                          *
 ****************************************************************************/

static void rc_varfileinfo(VERVARTABLE **vartabp)
{
    VERVARTABLE *vartab;

    (*vartabp) = vartab = my_alloc(sizeof(VERVARTABLE));
    memset(vartab, 0, sizeof(*vartab));

    accept_symbol(C_VALUE);

    if (s.sym.tok == C_STRING)
    {
        vartab->key = my_alloc(unisize(s.sym.str));
        ansi2uni(vartab->key, s.sym.str);
    }
    accept_symbol(C_STRING);

    do
    {
        ushort_t lang;
        ushort_t charset;

        lang = accept_ushort();
        charset = accept_ushort();

        add_vervalue(vartab, MAKELONG(lang, charset));

    } while (s.sym.tok != C_END && s.sym.tok != C_RBRACE);
}

/****************************************************************************
 *                                                                          *
 * Function: add_vervalue                                                   *
 *                                                                          *
 * Purpose : Add a version value to a dynamic array.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-08-22  Simplified logic.                                    *
 *                                                                          *
 ****************************************************************************/

static void add_vervalue(VERVARTABLE *table, ulong_t val)
{
    if (table->num == table->max)
    {
        table->max += 10;
        table->val = (ulong_t *)my_realloc(
            table->val, table->max * sizeof(ulong_t));
    }

    table->val[table->num++] = val;
}

/****************************************************************************
 *                                                                          *
 * Function: free_vervalues                                                 *
 *                                                                          *
 * Purpose : Free a dynamic version value array.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_vervalues(VERVARTABLE *table)
{
    if (table != NULL)
    {
        my_free(table->key);
        my_free(table->val);
        my_free(table);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_generic                                                     *
 *                                                                          *
 * Purpose : Parse a generic statement.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_generic(SYMBOL *symp, SYMBOL *restype)
{
    ushort_t memflags = rc_memflags(RSRC_F_PURE|RSRC_F_MOVEABLE);
    char *sp;

    sp = strrchr(s.filename, '.');
    if (sp != NULL && _stricmp(sp, ".h") == 0)
        return;  /* ignore garbage in windows.h; same as MS RC */

    if (info_callback)
    {
        if (restype->tok == C_SYMBOL)
        {
            print_info(restype->str, symp, &commontail);
        }
        else
        {
            char buf[32];
            sprintf(buf, "0x%lx", restype->num);
            print_info(buf, symp, &commontail);
        }
    }

    if (s.sym.tok == C_STRING || s.sym.tok == C_SYMBOL)
    {
        char *filename;
        WINERR err;

        filename = search_include(s.sym.str, FALSE);

        if (symp->tok == C_NUMBER)
        {
            if (restype->tok == C_NUMBER)
                err = numbered_generic_numbered_type((wchar_t)symp->num, (wchar_t)restype->num, filename, memflags);
            else
                err = numbered_generic_named_type((wchar_t)symp->num, restype->str, filename, memflags);
        }
        else
        {
            if (restype->tok == C_NUMBER)
                err = named_generic_numbered_type(symp->str, (wchar_t)restype->num, filename, memflags);
            else
                err = named_generic_named_type(symp->str, restype->str, filename, memflags);
        }

        my_free(filename);

        apperror(MYOPENERROR(err), s.sym.str);
        getsymbol();
    }
    else
    {
        apperror(RCERROR(ERROR_FILE_NOT_FOUND2), "");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_memflags                                                    *
 *                                                                          *
 * Purpose : Parse the load-mem flags.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           02-02-27  Support all flags for easier compare with MS RC.     *
 *                                                                          *
 ****************************************************************************/

static ushort_t rc_memflags(ushort_t flags)
{
    for (;;)
    {
        switch (s.sym.tok)
        {
            case C_PRELOAD:
                flags |= RSRC_F_PRELOAD;
                getsymbol();
                break;

            case C_LOADONCALL:
                flags &= ~RSRC_F_PRELOAD;
                getsymbol();
                break;

            case C_MOVEABLE:
                flags |= RSRC_F_MOVEABLE;
                getsymbol();
                break;

            case C_FIXED:
                flags &= ~RSRC_F_MOVEABLE;
                getsymbol();
                break;

            case C_PURE:
                flags |= RSRC_F_PURE;
                getsymbol();
                break;

            case C_IMPURE:
                flags &= ~(RSRC_F_DISCARDABLE|RSRC_F_PURE);
                getsymbol();
                break;

            case C_DISCARDABLE:
                flags |= RSRC_F_DISCARDABLE|RSRC_F_PURE;
                getsymbol();
                break;

            default:
                return flags;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rc_options                                                     *
 *                                                                          *
 * Purpose : Parse optional statements.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           98-03-14  Save dialog class in class field (not caption)!      *
 *           98-07-01  Changed rc_style() to accept_ulong_expression().     *
 *           01-11-26  Accept ordinal in dialog CLASS and MENU.             *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *                                                                          *
 ****************************************************************************/

static void rc_options(RSRC_HDR_TAIL *tailp, DIALOGHEADER *dlg)
{
    for (;;)
    {
        switch (s.sym.tok)
        {
            ushort_t lang;
            ushort_t sublang;

            /*
             * Generic options.
             */
            case C_CHARACTERISTICS:
                getsymbol();
                tailp->characteristics = accept_ulong();
                break;

            case C_LANGUAGE:
                getsymbol();
                lang = accept_ushort();
                sublang = accept_ushort();
                tailp->language = MAKELANGID(lang, sublang);
                break;

            case C_VERSION:
                getsymbol();
                tailp->version = accept_ulong();
                break;

            /*
             * DIALOG-specific options (in header).
             */
            case C_CAPTION:
                if (!dlg) return;
                /**/
                if (getsymbol() == C_STRING)
                {
                    dlg->caption = my_alloc(unisize(s.sym.str));
                    ansi2uni(dlg->caption, s.sym.str);
                }
                accept_symbol(C_STRING);
                break;

            case C_CLASS:
                if (!dlg) return;
                /**/
                if (getsymbol() == C_NUMBER)
                {
                    dlg->class = my_alloc(2 * sizeof(wchar_t));
                    dlg->class[0] = (wchar_t)-1;
                    dlg->class[1] = accept_ushort();
                }
                else
                {
                    if (getsymbol() == C_STRING)
                    {
                        dlg->class = my_alloc(unisize(s.sym.str));
                        ansi2uni(dlg->class, s.sym.str);
                    }
                    accept_symbol(C_STRING);
                }
                break;

            case C_FONT:
                if (!dlg) return;
                /**/
                getsymbol();
                dlg->font.ptsize = accept_ushort();
                if (s.sym.tok == C_STRING)
                {
                    dlg->font.typeface = my_alloc(unisize(s.sym.str));
                    ansi2uni(dlg->font.typeface, s.sym.str);
                }
                accept_symbol(C_STRING);
                if (dlg->extended)
                {
                    dlg->font.weight = (s.sym.tok == C_NUMBER) ? accept_short() : FW_DONTCARE;
                    dlg->font.italic = (s.sym.tok == C_NUMBER) ? accept_short() : FALSE;
                    dlg->font.charset = (s.sym.tok == C_NUMBER) ? accept_short() : DEFAULT_CHARSET;
                }
                break;

            case C_MENU:
                if (!dlg) return;
                /**/
                if (getsymbol() == C_NUMBER)
                {
                    dlg->menu = my_alloc(2 * sizeof(wchar_t));
                    dlg->menu[0] = (wchar_t)-1;
                    dlg->menu[1] = accept_ushort();
                }
                else
                {
                    if (s.sym.tok == C_STRING)
                    {
                        dlg->menu = my_alloc(unisize(s.sym.str));
                        ansi2uni(dlg->menu, s.sym.str);
                    }
                    accept_symbol(C_STRING);
                }
                break;

            case C_STYLE:
                if (!dlg) return;
                /**/
                getsymbol();
                dlg->style = accept_ulong_expression(0);
                break;

            case C_EXSTYLE:
                if (!dlg) return;
                /**/
                getsymbol();
                dlg->exstyle = accept_ulong_expression(0);
                break;

            default:
                return;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: print_info                                                     *
 *                                                                          *
 * Purpose : Display information for verbose mode.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           01-11-09  Rewritten when compiler were converted to DLL.       *
 *                                                                          *
 ****************************************************************************/

static void print_info(const char *text, SYMBOL *symp, RSRC_HDR_TAIL *tailp)
{
    char buf[512], *bp = buf;

    bp += sprintmsg(bp, MSG_RESOURCE_INFO_1, text);

    if (symp != NULL)
    {
        if (symp->tok == C_NUMBER)
            bp += sprintmsg(bp, MSG_RESOURCE_INFO_2A, symp->num);
        else
            bp += sprintmsg(bp, MSG_RESOURCE_INFO_2B, symp->str);
    }

    if (tailp != NULL)
        bp += sprintmsg(bp, MSG_RESOURCE_INFO_3, tailp->language);

    if (hfres != NULL)
        bp += sprintmsg(bp, MSG_RESOURCE_INFO_4, my_tellfile(hfres));

    info_callback(buf);
}


