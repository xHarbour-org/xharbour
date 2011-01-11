/****************************************************************************
 *                                                                          *
 * File    : outdbg.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Debugging output driver.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#if defined(PODEBUG) || defined(PRERELEASE)

typedef struct _SCNENTRY {
    struct _SCNENTRY *next;
    long number;
    char *name;
} SCNENTRY;

static SCNENTRY *sects;

/****************************************************************************
 *                                                                          *
 * Function: dbg_filename                                                   *
 *                                                                          *
 * Purpose : Construct the output filename, if not given by the user.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dbg_filename(char *outfile, const char *srcfile)
{
    if (*outfile == '\0')
    {
        strcpy(outfile, basename(srcfile));
        update_extension_in_file(outfile, ".dbg");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_filebeg                                                    *
 *                                                                          *
 * Purpose : Prepare to write to the output file.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dbg_filebeg(void)
{
    sects = NULL;

    fprintf(fdo, "Output format debug dump\n");
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_fileend                                                    *
 *                                                                          *
 * Purpose : Finish writing to the output file.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dbg_fileend(bool_t abort)
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
 * Function: dbg_output                                                     *
 *                                                                          *
 * Purpose : Handle output of code or data.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dbg_output(long segment, const void *data, ulong_t type, long segref)
{
    long realbytes;
    long ldata;
    int id;

    realbytes = type & OUT_SIZMASK;
    type &= OUT_TYPMASK;

    fprintf(fdo, "output to segment %lx, len = %ld: ", segment, realbytes);

    switch(type)
    {
        case OUT_RESERVE:
            fprintf(fdo, "reserve bytes.\n");
            break;

        case OUT_RAWDATA:
            fprintf(fdo, "raw data = ");
            while (realbytes--)
            {
                id = *(uchar_t *)data;
                data = (char *)data + 1;
                fprintf(fdo, "%02x ", id);
            }
            fprintf(fdo, "\n");
            break;

        case OUT_ADDRESS:
            if (realbytes == 1)
                ldata = *(char *)data;
            else if (realbytes == 2)
                ldata = *(short *)data;
            else if (realbytes == 4)
                ldata = *(long *)data;
            else
                ldata = 0;
            fprintf(fdo, "address %08lx (segref %lx)\n", ldata, segref);
            break;

        case OUT_REL2ADR:
            fprintf(fdo, "rel2adr %04x (segref %lx)\n", (int)*(short *)data, segref);
            break;

        case OUT_REL4ADR:
            fprintf(fdo, "rel4adr %08lx (segref %lx)\n", *(long *)data, segref);
            break;

        default:
            fprintf(fdo,"unknown\n");
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_symbol                                                     *
 *                                                                          *
 * Purpose : Handle a label definition (output driver).                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long dbg_symbol(const char *name, long segment, long offset, int type)
{
    fprintf(fdo, "symbol %s := %lx:%08lx %s (%d)\n",
        name, segment, offset,
        (type == SYMDEF_EXTERN) ? "external" :
        (type == SYMDEF_COMMON) ? "common" :
        (type == SYMDEF_GLOBAL) ? "global" : "local",
        type);

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_segment                                                    *
 *                                                                          *
 * Purpose : Handle a segment change in the source code.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long dbg_segment(const char *name, const char *attr, int pass, int *bits)
{
    long seg;

    if (!name && bits)
        *bits = 32;

    if (!name)
    {
        fprintf(fdo, "initial segment = %lx\n", seg = seg_alloc());
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
            fprintf(fdo, "segment %s = %lx (pass %d)\n", name, seg, pass);
        }
    }

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_segbase                                                    *
 *                                                                          *
 * Purpose : Handle a segment base change.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long dbg_segbase(long segment)
{
    return segment;
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_directive                                                  *
 *                                                                          *
 * Purpose : Handle any driver specific directives.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t dbg_directive(const char *keyword, const char *value, int pass)
{
    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: dbg_lineno                                                     *
 *                                                                          *
 * Purpose : Handle a linenumber definition.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dbg_lineno(long segment, int lineno)
{
    fprintf(fdo, "srcline: segment = %lx, lnno = %d\n", segment, lineno);
}

OUTFMT x86debugOF = {
    FALSE,
    x86_init,
    assembler,
    dbg_filename,
    dbg_filebeg,
    dbg_fileend,
    dbg_output,
    dbg_symbol,
    dbg_segment,
    dbg_segbase,
    dbg_directive,
    dbg_lineno
};

OUTFMT armdebugOF = {
    FALSE,
    arm_init,
    assembler,
    dbg_filename,
    dbg_filebeg,
    dbg_fileend,
    dbg_output,
    dbg_symbol,
    dbg_segment,
    dbg_segbase,
    dbg_directive,
    dbg_lineno
};

#endif  /* PODEBUG || PRERELEASE */
