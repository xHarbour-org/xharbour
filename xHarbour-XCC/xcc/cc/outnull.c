/****************************************************************************
 *                                                                          *
 * File    : outnull.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Null output driver.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

typedef struct _SCNENTRY {
    struct _SCNENTRY *next;
    long number;
    char *name;
} SCNENTRY;

static SCNENTRY *sects;

/****************************************************************************
 *                                                                          *
 * Function: null_filename                                                  *
 *                                                                          *
 * Purpose : Construct the output filename, if not given by the user.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void null_filename(char *outfile, const char *srcfile)
{
    if (*outfile == '\0')
    {
        strcpy(outfile, basename(srcfile));
        update_extension_in_file(outfile, ".asm");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: null_filebeg                                                   *
 *                                                                          *
 * Purpose : Prepare to write to the output file.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void null_filebeg(void)
{
    sects = NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: null_fileend                                                   *
 *                                                                          *
 * Purpose : Finish writing to the output file.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void null_fileend(bool_t abort)
{
    while (sects)
    {
        SCNENTRY *s = sects;
        sects = sects->next;
        my_free(s->name);
        /* s in permanent storage */
    }
}

/****************************************************************************
 *                                                                          *
 * Function: null_output                                                    *
 *                                                                          *
 * Purpose : Handle output of code or data.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void null_output(long segment, const void *data, ulong_t type, long segref)
{
}

/****************************************************************************
 *                                                                          *
 * Function: null_symbol                                                    *
 *                                                                          *
 * Purpose : Handle a label definition (output driver).                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long null_symbol(const char *name, long segment, long offset, int type)
{
    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: null_segment                                                   *
 *                                                                          *
 * Purpose : Handle a segment change in the source code.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long null_segment(const char *name, const char *attr, int pass, int *bits)
{
    long seg;

    if (!name && bits)
        *bits = 32;

    if (!name)
    {
        seg = seg_alloc();
    }
    else
    {
        int n = strcspn(name, " \t");
        char *sname = my_strndup(name, n);
        SCNENTRY *s;

        seg = NO_SEG;
        for (s = sects; s != NULL; s = s->next)
        {
            if (strcmp(s->name, sname) == 0)
            {
                seg = s->number;
                break;
            }
        }

        if (seg == NO_SEG)
        {
            s = memalloc(sizeof(*s), PERM);
            s->name = sname;
            s->number = seg = seg_alloc();
            s->next = sects;
            sects = s;
        }
    }

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: null_segbase                                                   *
 *                                                                          *
 * Purpose : Handle a segment base change.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long null_segbase(long segment)
{
    return segment;
}

/****************************************************************************
 *                                                                          *
 * Function: null_directive                                                 *
 *                                                                          *
 * Purpose : Handle any driver specific directives.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t null_directive(const char *keyword, const char *value, int pass)
{
    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: null_lineno                                                    *
 *                                                                          *
 * Purpose : Handle a linenumber definition.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void null_lineno(long segment, int lineno)
{
}

/****************************************************************************
 *                                                                          *
 * Function: writeasm                                                       *
 *                                                                          *
 * Purpose : Write textfile for an external assembler.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void writeasm(void)
{
    if (aslist)
    {
        LIST *lp = aslist;
        do
        {
            lp = lp->link;
            fputs(lp->data, fdo), fputc('\n', fdo);
        } while (lp != aslist);
    }
}

OUTFMT x86asmOF = {
    FALSE,
    x86_init,
    writeasm,
    null_filename,
    null_filebeg,
    null_fileend,
    null_output,
    null_symbol,
    null_segment,
    null_segbase,
    null_directive,
    null_lineno
};

OUTFMT armasmOF = {
    FALSE,
    arm_init,
    writeasm,
    null_filename,
    null_filebeg,
    null_fileend,
    null_output,
    null_symbol,
    null_segment,
    null_segbase,
    null_directive,
    null_lineno
};
