/****************************************************************************
 *                                                                          *
 * File    : include.c                                                      *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; include management.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           01-11-21  New function add_inclinfo() added.                   *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

#define PATH_DELIMITER ";"

/* Locals */
static INCLUDE *includelist = NULL;

/* Static function prototypes */
static void add_inclinfo(const char *);

/****************************************************************************
 *                                                                          *
 * Function: include                                                        *
 *                                                                          *
 * Purpose : Handle a #include directive.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void include(TOKENROW *trp)
{
    char fname[MAX_PATH];
    char *iname;
    bool_t angled;
    size_t len;
    HANDLE hf;
    WINERR err;

    trp->tp += 1;
    if (trp->tp >= trp->lp)
        goto syntax;

    if (trp->tp->type != STRING && trp->tp->type != LT)
    {
        len = trp->tp - trp->bp;
        expandrow(trp, "<include>");
        trp->tp = trp->bp+len;
    }

    if (trp->tp->type == STRING)
    {
        /*
         * Handle #include "name".
         */
        len = trp->tp->len-2;
        if (len > NELEMS(fname)-1)
            len = NELEMS(fname)-1;
        strncpy(fname, (char *)trp->tp->t+1, len);
        angled = FALSE;
    }
    else if (trp->tp->type == LT)
    {
        /*
         * Handle #include <name>.
         */
        len = 0;
        trp->tp++;
        while (trp->tp->type != GT)
        {
            if (trp->tp > trp->lp || len+trp->tp->len+2 >= NELEMS(fname))
                goto syntax;

            strncpy(fname+len, (char *)trp->tp->t, trp->tp->len);
            len += trp->tp->len;
            trp->tp++;
        }
        angled = TRUE;
    }
    else goto syntax;

    trp->tp += 2;
    if (trp->tp < trp->lp || len == 0)
        goto syntax;

    fname[len] = '\0';

    iname = search_include(fname, angled);
    err = my_openfile(iname, &hf);
    if (err == 0)
    {
        if (idemode && !angled && incdepth == 0)
            add_inclinfo(iname);

        if (++incdepth > 100)
            pp_error(RCFATAL(ERROR_INCLUDE_TOO_DEEPLY_NESTED));

        set_source(iname, hf, NULL);
        (void)genline();
    }
    else
    {
        trp->tp = trp->bp+2;
        pp_error(RCFATAL(ERROR_INCLUDE_FILE_NOT_FOUND), trp);
    }
    return;

syntax:
    pp_error(RCERROR(ERROR_INCLUDE_SYNTAX_ERROR));
    return;
}

/****************************************************************************
 *                                                                          *
 * Function: genline                                                        *
 *                                                                          *
 * Purpose : Generate a #line directive for cursource.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t genline(void)
{
    static TOKEN ta = { UNCLASS };
    static TOKENROW tr = { &ta, &ta, &ta+1, 1 };
    uchar_t *p;
    uchar_t *s;

    ta.t = p = (uchar_t *)outpp;
    p += strlen(strcpy((char *)p, "#line "));
    p = (uchar_t *)outnum((char *)p, cursource->line);

    *p++ = ' '; *p++ = '"';
    s = (uchar_t *)cursource->filename;
#if 1  /* zero for compiler */
    while (*s)
    {
        if ((*p++ = *s++) == '\\' && *s != '\\')
             *p++ = '\\';
    }
#else
    while (*s)
        *p++ = *s++;
#endif
    *p++ = '"'; *p++ = '\n';

    ta.len = (char *)p - outpp;
    outpp = (char *)p;
    tr.tp = tr.bp;
    return put_tokens(&tr);
}

/****************************************************************************
 *                                                                          *
 * Function: setup_include                                                  *
 *                                                                          *
 * Purpose : Add search paths for 'standard places'.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_include(void)
{
    char *p;

    /* Add current directory to the list */
    add_include(".", 0, FALSE);

    p = getenv("include");
    if (p && *p)
    {
        /* make a temporary copy and chop it up */
        char *s = tstrcpy(p);

        for (p = strtok(s, PATH_DELIMITER);
             p != NULL;
             p = strtok(NULL, PATH_DELIMITER))
        {
            add_include(p, INC_DEFAULT|INC_STDPLACE, TRUE);
        }

        my_free(s);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: add_include                                                    *
 *                                                                          *
 * Purpose : Add a new search path (possibly via -I option).                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           00-01-29  Rewritten. Changed static array to linked list.      *
 *                                                                          *
 ****************************************************************************/

void add_include(const char *path, uint_t flags, bool_t append)
{
    INCLUDE *inc;

    inc = (INCLUDE *)my_alloc(sizeof(INCLUDE));

    inc->next = NULL;
    inc->path = tstrcpy(path);
    inc->flags = flags;

    if (!includelist)
    {
        /* This is the first node. Start the list */
        includelist = inc;
    }
    else if (!append)
    {
        /* Insert the new node after "." but before any other */
        inc->next  = includelist->next;
        includelist->next = inc;
    }
    else
    {
        INCLUDE *incT;

        /* Append the new node to the list */
        for (incT = includelist; incT->next != NULL; incT = incT->next)
            ;

        inc->next = incT->next;
        incT->next = inc;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: delete_includes                                                *
 *                                                                          *
 * Purpose : Delete search paths for 'standard places' (-X option).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-10-22  Created                                              *
 *           00-01-29  Rewritten. Changed static array to linked list.      *
 *                                                                          *
 ****************************************************************************/

void delete_includes(void)
{
    INCLUDE *inc;
    INCLUDE *inc_prev;
    INCLUDE *inc_next;

    for (inc = includelist, inc_prev = NULL;
         inc != NULL;
         inc_prev = inc, inc = inc_next)
    {
        inc_next = inc->next;

        if (inc->flags & INC_DEFAULT)
        {
            if (inc_prev)
                inc_prev->next = inc_next;
            else
                includelist = inc_next;

            my_free(inc);
            inc = inc_prev;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: search_include                                                 *
 *                                                                          *
 * Purpose : Search for a file; mostly in the INCLUDE directories.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *search_include(const char *fname, bool_t angled)
{
    SOURCE *s = cursource;
    char tname[MAX_PATH];
    INCLUDE *inc;

    if (s && !angled)
    {
        char *dp = ".";
        char *fp;

        if ((fp = strrchr(s->filename, '\\')) != NULL)
        {
            size_t len = (fp - s->filename);
            dp = (char *)newstring((uchar_t *)s->filename, len, 0);
            dp[len] = '\0';
        }

        if (my_fullpath(tname, fname, dp))
        {
            if (my_isfile(tname))
                return tstrcpy(tname);
        }
    }

    /* Search through include directories */
    for (inc = includelist; inc != NULL; inc = inc->next)
    {
        if (angled && (inc->flags & INC_STDPLACE) == 0)
            continue;

        if (my_fullpath(tname, fname, inc->path))
        {
            if (my_isfile(tname))
                return tstrcpy(tname);
        }
    }

    /* Always return a safe copy of the string */
    return tstrcpy(fname);
}

/****************************************************************************
 *                                                                          *
 * Function: add_inclinfo                                                   *
 *                                                                          *
 * Purpose : Add a new include information node to the global list.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_inclinfo(const char *filename)
{
    INCLINFO *info;

    info = (INCLINFO *)my_alloc(sizeof(INCLINFO));
    info->next = NULL;
    info->filename = strcpy(my_alloc(strlen(filename)+1), filename);

    if (inclinfo_list == NULL)
    {
        /* This is the first node. Start the list */
        inclinfo_list = info;
    }
    else
    {
        INCLINFO *infoT;

        /* Append the new node to the list */
        for (infoT = inclinfo_list; infoT->next != NULL; infoT = infoT->next)
            ;
        infoT->next = info;
    }
}

