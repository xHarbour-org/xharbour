/****************************************************************************
 *                                                                          *
 * File    : include.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Include file management.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

#ifndef __POCC__
#include <share.h>
#endif

#define PATH_DELIMITER ";"

/* Locals */
static INCLUDE *includelist = NULL;

/****************************************************************************
 *                                                                          *
 * Function: include                                                        *
 *                                                                          *
 * Purpose : Handle #include directive.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-07-25  Bugfix: didn't correctly handle "..\dir\file.h".     *
 *           04-03-18  Bugfix: support forward slashes too.                 *
 *                                                                          *
 ****************************************************************************/

void include(TOKENROW *trp)
{
    char fname[MAXPATH];
    char iname[MAXPATH];
    INCLUDE *inc;
    bool_t angled;
    size_t len;
    int fd;

    trp->tp += 1;
    if (trp->tp >= trp->lp)
        goto syntax;

    if (trp->tp->type != STRING && trp->tp->type != LT)
    {
        len = trp->tp - trp->bp;
        expand_tokenrow(trp, "<include>");
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
         * Handle #include <name>  (i.e. standard place).
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

    fd = -1;
    if (cursource && !angled)
    {
        char *fp;

        /* rewritten 2004-03-18: check for both forward & backward slashes through basename() */
        if ((fp = (char *)basename(cursource->filename)) != cursource->filename)
        {
            len = (fp - cursource->filename);
            fp = my_strndup(cursource->filename, len);
            if (my_fullpath(iname, fname, fp))
                fd = _sopen(iname, _O_RDONLY|_O_SEQUENTIAL|_O_TEXT, _SH_DENYWR);
            my_free(fp);
        }
        else if (basename(fname) != fname)
        {
            if (my_fullpath(iname, fname, NULL))
                fd = _sopen(iname, _O_RDONLY|_O_SEQUENTIAL|_O_TEXT, _SH_DENYWR);
        }
    }

    for (inc = includelist; inc != NULL && fd == -1; inc = inc->next)
    {
        if (angled && (inc->flags & INC_STDPLACE) == 0)
            continue;

        if (my_fullpath(iname, fname, inc->path))
            fd = _sopen(iname, _O_RDONLY|_O_SEQUENTIAL|_O_TEXT, _SH_DENYWR);
    }

    if (fd != -1)
    {
        if (++incdepth > 100)
            pp_error(RCFATAL(ERROR_INCLUDE_TOO_DEEPLY_NESTED));

        if (incdepth == 16)
            apperror(RCWARNING2(ERROR_MORE_THAN_X_INCLUDES), 15);

        set_source(my_strdup(iname), fd, NULL);
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
 *           00-06-06  Created                                              *
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
    while (*s)
        *p++ = *s++;
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
 *           00-06-06  Created                                              *
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
        char *s = my_strdup(p);

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
 *           00-06-06  Created                                              *
 *           04-04-06  Bugfix: always build the full path (GLFW).           *
 *                                                                          *
 ****************************************************************************/

void add_include(const char *path, uint_t flags, bool_t append)
{
    char buf[MAXPATH];
    INCLUDE *inc;

    /* Build the full path */
    if (my_fullpath(buf, path, _getcwd(buf, MAXPATH)))
        path = buf;

    inc = my_alloc(sizeof(INCLUDE));
    inc->next = NULL;
    inc->path = my_strdup(path);
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
 *           00-06-06  Created                                              *
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

