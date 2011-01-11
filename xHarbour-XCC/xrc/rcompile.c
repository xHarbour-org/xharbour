/****************************************************************************
 *                                                                          *
 * File    : rcompile.c                                                     *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; output file management.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           98-06-22  Handling of generic resource types added.            *
 *           98-06-22  Special handling of WAVE resource type removed.      *
 *           98-11-08  Resource type HTML added.                            *
 *           00-01-23  Resource types PLUGPLAY and VXD added.               *
 *           01-05-24  Resource type MANIFEST added.                        *
 *           01-11-11  New function add_fileinfo() added.                   *
 *           03-08-02  Support for Borland style literal bitmaps added.     *
 *           03-09-03  Support for Borland style literal icons added.       *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

static const struct
{
    RSRC_HDR_HEAD head;
    wchar_t resourceType[2];
    wchar_t resourceName[2];
    RSRC_HDR_TAIL tail;
}
resfile_head32 =
{
    { 0, 32 },                      /* head */
    { (wchar_t)-1, (wchar_t)0 },    /* resourceType */
    { (wchar_t)-1, (wchar_t)0 },    /* resourceName */
    { 0, 0, 0, 0, 0 }               /* tail */
};

/* Global language settings etc */
RSRC_HDR_TAIL commontail = { 0, 0, 0x409, 0, 0 };

/* Locals */
static int iconcursor_count = 0;

/* Static function prototypes */
static size_t stringsize(STRINGENTRY **);
static WINERR write_accelerators(wchar_t *, RSRC_HDR_TAIL *, ACCELTABLE *);
static WINERR write_anicursor(wchar_t *, const char *, ushort_t);
static WINERR write_aniicon(wchar_t *, const char *, ushort_t);
static WINERR write_bitmap(wchar_t *, const char *, ushort_t);
static WINERR write_literal_bitmap(wchar_t *, uchar_t *, size_t, ushort_t);
static WINERR write_cursor(wchar_t *, const char *, ushort_t);
static WINERR write_literal_cursor(wchar_t *, uchar_t *, size_t, ushort_t);
static WINERR write_html(wchar_t *, const char *, ushort_t);
static WINERR write_icon(wchar_t *, const char *, ushort_t);
static WINERR write_literal_icon(wchar_t *, uchar_t *, size_t, ushort_t);
static WINERR write_dialog(wchar_t *, RSRC_HDR_TAIL *, DIALOGHEADER *);
static size_t dialogsize(DIALOGHEADER *);
static int ncontrols(DIALOGHEADER *);
static WINERR write_dlginclude(wchar_t *, const char *);
static WINERR write_manifest(wchar_t *, const char *, ushort_t);
static WINERR write_menu(wchar_t *, RSRC_HDR_TAIL *, MENUTREE *);
static WINERR write_menutree(MENUTREE *);
static size_t menusize(MENUTREE *);
static WINERR write_menuex(wchar_t *, RSRC_HDR_TAIL *, MENUTREE *);
static WINERR write_menuextree(MENUTREE *);
static size_t menuexsize(MENUTREE *);
static WINERR write_msgtable(wchar_t *, const char *);
static WINERR write_plugplay(wchar_t *, const char *, ushort_t);
static WINERR write_rcdata(wchar_t *, RSRC_HDR_TAIL *, uchar_t *, size_t);
static WINERR write_version(wchar_t *, RSRC_VERFIX *, VERSTRINGTABLE *, VERVARTABLE *);
static size_t verstringsize(VERSTRINGTABLE *, int);
static size_t vervarsize(VERVARTABLE *);
static WINERR write_vxd(wchar_t *, const char *, ushort_t);
static WINERR write_generic(wchar_t *, wchar_t *, const char *, ushort_t);
static WINERR write_resourceheader(HANDLE, RSRC_HDR_HEAD *, RSRC_HDR_TAIL *, wchar_t *, wchar_t *);
static WINERR write_alignment(HANDLE, long);
static WINERR write_nameord(HANDLE, wchar_t *);
static size_t nameordsize(wchar_t *);
static WINERR copy_anyfile(HANDLE, const char *);
static void add_fileinfo(wchar_t *, wchar_t *, const char *);
static wchar_t *nameorddup(wchar_t *);

/****************************************************************************
 *                                                                          *
 * Function: res_start                                                      *
 *                                                                          *
 * Purpose : Create the output file and write the WIN32 header.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void res_start(const char *filename)
{
    WINERR err;

    if (filename)
    {
        err = my_createfile(filename, &hfres);
        if (err) apperror(RCFATAL(err), filename);

        err = my_writefile(hfres, &resfile_head32, sizeof(resfile_head32));
        if (err) apperror(RCFATAL(err));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: res_close                                                      *
 *                                                                          *
 * Purpose : Close the output file.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

void res_close(void)
{
    if (hfres != NULL)
    {
        my_closefile(hfres);
        hfres = NULL;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_stringtable                                              *
 *                                                                          *
 * Purpose : Write the stringtable resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR write_stringtable(STRINGTABLE *table)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_STRING };
    RSRC_HDR_HEAD head;
    long pad = 0;
    int i;
    WINERR err;

    for (i = 0; i < table->num; i++)
    {
        if (table->strings[i] != NULL)
        {
            wchar_t resourceName[2];
            RSRC_HDR_TAIL tail = table->strings[i]->tail;
            STRINGENTRY *block[RSRC_STRINGS_PER_BLOCK] = {0};
            int j;

            resourceName[0] = (wchar_t)-1;
            resourceName[1] = (wchar_t)table->strings[i]->block;

            /* The current item has to be the first item in the loop */
            for (j = i; j < table->num; j++)
            {
                if (table->strings[j] != NULL)
                {
                    if (table->strings[j]->block == resourceName[1] &&
                        table->strings[j]->tail.language == tail.language)
                    {
                        if (block[table->strings[j]->pos] != NULL)
                            return RCFATAL(ERROR_DUPLICATE_STRING_ID);

                        block[table->strings[j]->pos] = table->strings[j];

                        /* Remove it from the table */
                        table->strings[j] = NULL;
                    }
                }
            }

            /* Write the general resource header */
            head.ressize = stringsize(block);

            err = write_resourceheader(hfres, &head, &tail, resourceType, resourceName);
            if (err) return RCFATAL(err);

            /* The header is already DWORD-aligned */
            for (j = 0; j < RSRC_STRINGS_PER_BLOCK; j++)
            {
                if (block[j] != NULL)
                {
                    wchar_t *string = block[j]->string;

                    err = my_writefile(hfres, string, (1 + string[0]) * sizeof(wchar_t));
                    if (err) return RCFATAL(err);
                }
                else
                {
                    /* Fill up the empty position */
                    err = my_writefile(hfres, &pad, sizeof(wchar_t));
                    if (err) return RCFATAL(err);
                }
            }

            err = write_alignment(hfres, head.ressize);
            if (err) return err;
        }
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: stringsize                                                     *
 *                                                                          *
 * Purpose : Calculate the size of a stringtable block.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t stringsize(STRINGENTRY **items)
{
    size_t size = 0;
    int i;

    for (i = 0; i < RSRC_STRINGS_PER_BLOCK; i++)
    {
        if (items[i] != NULL)
            size += nameordsize(&items[i]->string[1]);
        else
            size += sizeof(wchar_t);
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: named_accelerators                                             *
 *                                                                          *
 * Purpose : Write a named accelerator resource.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_accelerators(const char *name, RSRC_HDR_TAIL *tailp, ACCELTABLE *table)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_accelerators(resourceName, tailp, table);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_accelerators                                          *
 *                                                                          *
 * Purpose : Write a numbered accelerator resource.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_accelerators(wchar_t id, RSRC_HDR_TAIL *tailp, ACCELTABLE *table)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_accelerators(resourceName, tailp, table);
}

/****************************************************************************
 *                                                                          *
 * Function: write_accelerators                                             *
 *                                                                          *
 * Purpose : Write a accelerator resource.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_accelerators(wchar_t *name, RSRC_HDR_TAIL *tailp, ACCELTABLE *table)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_ACCELERATOR };
    RSRC_HDR_HEAD head;
    WINERR err;
    int i;

    head.ressize = sizeof(RSRC_ACCENT) * table->num;

    err = write_resourceheader(hfres, &head, tailp, resourceType, name);
    if (err) return RCFATAL(err);

    /* Mark the last item as...eer...last? */
    table->entries[table->num-1]->flags |= RSRC_A_LAST;

    for (i = 0; i < table->num; i++)
    {
        err = my_writefile(hfres, table->entries[i], sizeof(RSRC_ACCENT));
        if (err) return RCFATAL(err);

        /* RSRC_ACCENT is DWORD aligned. */
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_anicursor                                                *
 *                                                                          *
 * Purpose : Write a named animated cursor resource.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_anicursor(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_anicursor(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_anicursor                                             *
 *                                                                          *
 * Purpose : Write a numbered animated cursor resource.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_anicursor(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_anicursor(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_anicursor                                                *
 *                                                                          *
 * Purpose : Write a animated cursor resource.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_anicursor(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_ANICURSOR };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_aniicon                                                  *
 *                                                                          *
 * Purpose : Write a named animated icon resource.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_aniicon(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_aniicon(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_aniicon                                               *
 *                                                                          *
 * Purpose : Write a numbered animated icon resource.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_aniicon(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_aniicon(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_aniicon                                                  *
 *                                                                          *
 * Purpose : Write a animated icon resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_aniicon(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_ANIICON };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_bitmap                                                   *
 *                                                                          *
 * Purpose : Write a named bitmap resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_bitmap(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_bitmap(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_bitmap                                                *
 *                                                                          *
 * Purpose : Write a numbered bitmap resource.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_bitmap(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_bitmap(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_bitmap                                                   *
 *                                                                          *
 * Purpose : Write a bitmap resource.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_bitmap(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_BITMAP };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    BITMAPFILEHEADER filhdr;
    WINERR err;
    HANDLE hf;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_openfile(filename, &hf);
    if (err) return RCERROR(err);

    err = my_readfile(hf, &filhdr, sizeof(filhdr));
    if (err)
    {
        my_closefile(hf);
        return RCERROR(err);
    }

    if (filhdr.bfType != 0x4D42)  /* "BM" */
    {
        my_closefile(hf);
        return RCERROR(ERROR_INVALID_FILE);
    }

    head.ressize = GetFileSize(hf, NULL) - sizeof(filhdr);
    tail.memflags = memflags;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err)
    {
        my_closefile(hf);
        return RCFATAL(err);
    }

    /* Copy the bitmap bits */
    err = my_copystream(hfres, hf);
    if (err)
    {
        my_closefile(hf);
        return RCFATAL(err);
    }

    my_closefile(hf);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_literal_bitmap                                           *
 *                                                                          *
 * Purpose : Write a named literal bitmap resource (Borland).               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_literal_bitmap(const char *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_literal_bitmap(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_literal_bitmap                                        *
 *                                                                          *
 * Purpose : Write a numbered literal bitmap resource (Borland).            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_literal_bitmap(wchar_t id, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_literal_bitmap(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_literal_bitmap                                           *
 *                                                                          *
 * Purpose : Write a literal bitmap resource.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_literal_bitmap(wchar_t *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_BITMAP };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    BITMAPFILEHEADER *filhdrp;
    WINERR err;

    filhdrp = (BITMAPFILEHEADER *)datap;
    datap = datap + sizeof(BITMAPFILEHEADER);
    ndata -= sizeof(BITMAPFILEHEADER);

    if (filhdrp->bfType != 0x4D42)  /* "BM" */
        return RCERROR(ERROR_INVALID_FILE);

    head.ressize = ndata;
    tail.memflags = memflags;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    /* Copy the bitmap */
    err = my_writefile(hfres, datap, ndata);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_cursor                                                   *
 *                                                                          *
 * Purpose : Write a named cursor resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_cursor(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_cursor(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_cursor                                                *
 *                                                                          *
 * Purpose : Write a numbered cursor resource.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_cursor(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_cursor(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_cursor                                                   *
 *                                                                          *
 * Purpose : Write a cursor resource.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_cursor(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_CURSOR };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    RSRC_NEWHDR filhdr;
    CURHDR *filent;
    RSRC_NEWHDR grphdr;
    RSRC_ICOCUR *grpent;
    RSRC_HOTSPOT hotspot;
    WINERR err;
    HANDLE hf;
    int start_cursor = iconcursor_count+1;
    int i;

    if (idemode) add_fileinfo(resourceType, name, filename);

    tail.memflags = memflags;

    err = my_openfile(filename, &hf);
    if (err) return RCERROR(err);

    /*
     * Read the RSRC_NEWHDR structure.
     */
    err = my_readfile(hf, &filhdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_closefile(hf);
        return RCERROR(err);
    }

    if (filhdr.type != 2)
    {
        my_closefile(hf);
        return RCERROR(ERROR_INVALID_FILE);
    }

    filent = my_alloc(sizeof(CURHDR) * filhdr.nentries);

    /*
     * Read the component header entries.
     */
    for (i = 0; i < filhdr.nentries; i++)
    {
        err = my_readfile(hf, &filent[i], sizeof(CURHDR));
        if (err)
        {
            my_free(filent);
            my_closefile(hf);
            return RCERROR(err);
        }
    }

    /*
     * Write the component resources to the .RES file.
     */
    for (i = 0; i < filhdr.nentries; i++)
    {
        wchar_t resourceName[2] = { (wchar_t)-1, 0 };
        uchar_t *buffer;

        head.ressize = filent[i].ressize;
        buffer = (uchar_t *)my_alloc(head.ressize);

        iconcursor_count++;
        resourceName[1] = (wchar_t)iconcursor_count;

        hotspot.x = filent[i].hotspot.x;
        hotspot.y = filent[i].hotspot.y;

        err = my_seekfile(hf, filent[i].offset, FILE_BEGIN);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCERROR(ERROR_HANDLE_EOF);
        }

        /* Read the CURSOR bitmap data */
        err = my_readfile(hf, buffer, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCERROR(err);
        }

        /* ncolors is zero in most files; not very useful */
        filent[i].ncolors = (1 << ((BITMAPINFOHEADER *)buffer)->biBitCount);
        filent[i].width = (uchar_t)((BITMAPINFOHEADER *)buffer)->biWidth;
        filent[i].height = (uchar_t)((BITMAPINFOHEADER *)buffer)->biHeight / 2;

        head.ressize += sizeof(RSRC_HOTSPOT);

        /* Write the resource header for CURSOR component */
        err = write_resourceheader(hfres, &head, &tail,
            resourceType, resourceName);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        head.ressize -= sizeof(RSRC_HOTSPOT);

        err = my_writefile(hfres, &hotspot, sizeof(RSRC_HOTSPOT));
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        err = my_writefile(hfres, buffer, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        err = write_alignment(hfres, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        my_free(buffer);
    }

    /*
     * Write the cursor-group resource.
     */
    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)RSRC_T_GROUP_CURSOR;
    grpent = my_alloc(sizeof(RSRC_ICOCUR) * filhdr.nentries);

    grphdr.type = 2;
    grphdr.reserved = 0;
    grphdr.nentries = filhdr.nentries;

    for (i = 0; i < filhdr.nentries; i++)
    {
        grpent[i].u.cur.width = filent[i].width;
        grpent[i].u.cur.height = filent[i].height * 2;
        grpent[i].nplanes = 1;
        grpent[i].nbitspixel = bitcount(filent[i].ncolors);
        grpent[i].ressize = filent[i].ressize + sizeof(RSRC_HOTSPOT);
        grpent[i].id = i + start_cursor;
    }

    head.ressize = sizeof(RSRC_ICOCUR) * filhdr.nentries + sizeof(RSRC_NEWHDR);
    /* tail.memflags = memflags | RSRC_F_PURE; */
    tail.memflags = RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE|RSRC_F_PURE;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err)
    {
        my_free(grpent);
        my_free(filent);
        my_closefile(hf);
        return RCFATAL(err);
    }

    /* Write the icon header block */
    err = my_writefile(hfres, &grphdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_free(grpent);
        my_free(filent);
        my_closefile(hf);
        return RCFATAL(err);
    }

    /* Write the group component blocks */
    for (i = 0; i < filhdr.nentries; i++)
    {
        err = my_writefile(hfres, &grpent[i], sizeof(RSRC_ICOCUR));
        if (err)
        {
            my_free(grpent);
            my_free(filent);
            my_closefile(hf);
            return RCFATAL(err);
        }
    }

    my_free(grpent);
    my_free(filent);
    my_closefile(hf);

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_literal_cursor                                           *
 *                                                                          *
 * Purpose : Write a named literal cursor resource (Borland).               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_literal_cursor(const char *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_literal_cursor(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_literal_cursor                                        *
 *                                                                          *
 * Purpose : Write a numbered literal cursor resource (Borland).            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_literal_cursor(wchar_t id, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_literal_cursor(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_literal_cursor                                           *
 *                                                                          *
 * Purpose : Write a literal cursor resource.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_literal_cursor(wchar_t *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_CURSOR };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    RSRC_NEWHDR *filhdrp;
    CURHDR *filent;
    RSRC_NEWHDR grphdr;
    RSRC_ICOCUR *grpent;
    RSRC_HOTSPOT hotspot;
    WINERR err;
    int start_cursor = iconcursor_count+1;
    int i;

    UNREFERENCED_PARAMETER(ndata);

    tail.memflags = memflags;

    /*
     * Get the RSRC_NEWHDR structure.
     */
    filhdrp = (RSRC_NEWHDR *)datap;
    if (filhdrp->type != 2)
        return RCERROR(ERROR_INVALID_FILE);

    /*
     * Get the component header entries.
     */
    filent = (CURHDR *)(datap + sizeof(RSRC_NEWHDR));

    /*
     * Write the component resources to the .RES file.
     */
    for (i = 0; i < filhdrp->nentries; i++)
    {
        wchar_t resourceName[2] = { (wchar_t)-1, 0 };
        BITMAPINFOHEADER *bmhdrp;

        head.ressize = filent[i].ressize;

        iconcursor_count++;
        resourceName[1] = (wchar_t)iconcursor_count;

        hotspot.x = filent[i].hotspot.x;
        hotspot.y = filent[i].hotspot.y;

        /* ncolors is zero in most files; not very useful */
        bmhdrp = (BITMAPINFOHEADER *)((uchar_t *)filhdrp + filent[i].offset);
        filent[i].ncolors = (1 << bmhdrp->biBitCount);
        filent[i].width = (uchar_t)bmhdrp->biWidth;
        filent[i].height = (uchar_t)bmhdrp->biHeight / 2;

        head.ressize += sizeof(RSRC_HOTSPOT);

        /* Write the resource header for CURSOR component */
        err = write_resourceheader(hfres, &head, &tail, resourceType, resourceName);
        if (err) return RCFATAL(err);

        head.ressize -= sizeof(RSRC_HOTSPOT);

        /* Write the hotspot information */
        err = my_writefile(hfres, &hotspot, sizeof(RSRC_HOTSPOT));
        if (err) return RCFATAL(err);

        /* Write the bitmap data for CURSOR component */
        err = my_writefile(hfres, (uchar_t *)filhdrp + filent[i].offset, head.ressize);
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, head.ressize);
        if (err) return RCFATAL(err);
    }

    /*
     * Write the cursor-group resource.
     */
    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)RSRC_T_GROUP_CURSOR;
    grpent = my_alloc(sizeof(RSRC_ICOCUR) * filhdrp->nentries);

    grphdr.type = 2;
    grphdr.reserved = 0;
    grphdr.nentries = filhdrp->nentries;

    for (i = 0; i < filhdrp->nentries; i++)
    {
        grpent[i].u.cur.width = filent[i].width;
        grpent[i].u.cur.height = filent[i].height * 2;
        grpent[i].nplanes = 1;
        grpent[i].nbitspixel = bitcount(filent[i].ncolors);
        grpent[i].ressize = filent[i].ressize + sizeof(RSRC_HOTSPOT);
        grpent[i].id = i + start_cursor;
    }

    head.ressize = sizeof(RSRC_ICOCUR) * filhdrp->nentries + sizeof(RSRC_NEWHDR);
    /* tail.memflags = memflags | RSRC_F_PURE; */
    tail.memflags = RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE|RSRC_F_PURE;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err)
    {
        my_free(grpent);
        return RCFATAL(err);
    }

    /* Write the icon header block */
    err = my_writefile(hfres, &grphdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_free(grpent);
        return RCFATAL(err);
    }

    /* Write the group component blocks */
    for (i = 0; i < filhdrp->nentries; i++)
    {
        err = my_writefile(hfres, &grpent[i], sizeof(RSRC_ICOCUR));
        if (err)
        {
            my_free(grpent);
            return RCFATAL(err);
        }
    }

    my_free(grpent);

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_html                                                     *
 *                                                                          *
 * Purpose : Write a named HTML resource.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_html(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_html(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_html                                                  *
 *                                                                          *
 * Purpose : Write a numbered HTML resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_html(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_html(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_html                                                     *
 *                                                                          *
 * Purpose : Write a HTML resource.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_html(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_HTML };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_icon                                                     *
 *                                                                          *
 * Purpose : Write a named icon resource.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_icon(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_icon(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_icon                                                  *
 *                                                                          *
 * Purpose : Write a numbered icon resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_icon(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_icon(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_icon                                                     *
 *                                                                          *
 * Purpose : Write a icon resource.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_icon(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_ICON };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    RSRC_NEWHDR filhdr;
    ICOHDR *filent;
    RSRC_NEWHDR grphdr;
    RSRC_ICOCUR *grpent;
    WINERR err;
    HANDLE hf;
    int start_icon = iconcursor_count+1;
    int i;

    if (idemode) add_fileinfo(resourceType, name, filename);

    tail.memflags = memflags;

    err = my_openfile(filename, &hf);
    if (err) return RCERROR(err);

    /*
     * Read the RSRC_NEWHDR structure.
     */
    err = my_readfile(hf, &filhdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_closefile(hf);
        return RCERROR(err);
    }

    if (filhdr.type != 1)
    {
        my_closefile(hf);
        return RCERROR(ERROR_INVALID_FILE);
    }

    filent = my_alloc(sizeof(ICOHDR) * filhdr.nentries);

    /*
     * Read the component header entries.
     */
    for (i = 0; i < filhdr.nentries; i++)
    {
        err = my_readfile(hf, &filent[i], sizeof(ICOHDR));
        if (err)
        {
            my_free(filent);
            my_closefile(hf);
            return RCERROR(err);
        }
    }

    /*
     * Write the component resources to the .RES file.
     */
    for (i = 0; i < filhdr.nentries; i++)
    {
        wchar_t resourceName[2] = { (wchar_t)-1, 0 };
        uchar_t *buffer;

        head.ressize = filent[i].ressize;
        buffer = (uchar_t *)my_alloc(head.ressize);

        iconcursor_count++;
        resourceName[1] = (wchar_t)iconcursor_count;

        err = my_seekfile(hf, filent[i].offset, FILE_BEGIN);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCERROR(ERROR_HANDLE_EOF);
        }

        /* Read the ICON bitmap data */
        err = my_readfile(hf, buffer, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCERROR(err);
        }

        /* Write the resource header for ICON component */
        err = write_resourceheader(hfres, &head, &tail,
            resourceType, resourceName);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        /* Write the bitmap data for ICON component */
        err = my_writefile(hfres, buffer, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        err = write_alignment(hfres, head.ressize);
        if (err)
        {
            my_free(filent);
            my_free(buffer);
            my_closefile(hf);
            return RCFATAL(err);
        }

        my_free(buffer);
    }

    /*
     * Write the icon-group resource.
     */
    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)RSRC_T_GROUP_ICON;
    grpent = my_alloc(sizeof(RSRC_ICOCUR) * filhdr.nentries);

    grphdr.type = 1;
    grphdr.reserved = 0;
    grphdr.nentries = filhdr.nentries;

    for (i = 0; i < filhdr.nentries; i++)
    {
        grpent[i].u.ico.width = filent[i].width;
        grpent[i].u.ico.height = filent[i].height;
        grpent[i].u.ico.ncolors = filent[i].ncolors;
        grpent[i].nplanes = 1;
        grpent[i].nbitspixel = bitcount(filent[i].ncolors);
        grpent[i].ressize = filent[i].ressize;
        grpent[i].id = i + start_icon;
    }

    head.ressize = sizeof(RSRC_ICOCUR) * filhdr.nentries + sizeof(RSRC_NEWHDR);
    /* tail.memflags = memflags | RSRC_F_PURE; */
    tail.memflags = RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE|RSRC_F_PURE;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err)
    {
        my_free(filent);
        my_free(grpent);
        my_closefile(hf);
        return RCFATAL(err);
    }

    /* Write the icon header block */
    err = my_writefile(hfres, &grphdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_free(filent);
        my_free(grpent);
        my_closefile(hf);
        return RCFATAL(err);
    }

    /* Write the group component blocks */
    for (i = 0; i < filhdr.nentries; i++)
    {
        err = my_writefile(hfres, &grpent[i], sizeof(RSRC_ICOCUR));
        if (err)
        {
            my_free(filent);
            my_free(grpent);
            my_closefile(hf);
            return RCFATAL(err);
        }
    }

    my_free(grpent);
    my_free(filent);
    my_closefile(hf);

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_literal_icon                                             *
 *                                                                          *
 * Purpose : Write a named literal icon resource (Borland).                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_literal_icon(const char *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_literal_icon(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_literal_icon                                          *
 *                                                                          *
 * Purpose : Write a numbered literal icon resource (Borland).              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_literal_icon(wchar_t id, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_literal_icon(resourceName, datap, ndata, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_literal_icon                                             *
 *                                                                          *
 * Purpose : Write a literal icon resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-03  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_literal_icon(wchar_t *name, uchar_t *datap, size_t ndata, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_ICON };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    RSRC_NEWHDR *filhdrp;
    ICOHDR *filent;
    RSRC_NEWHDR grphdr;
    RSRC_ICOCUR *grpent;
    WINERR err;
    int start_icon = iconcursor_count+1;
    int i;

    UNREFERENCED_PARAMETER(ndata);

    tail.memflags = memflags;

    /*
     * Get the RSRC_NEWHDR structure.
     */
    filhdrp = (RSRC_NEWHDR *)datap;
    if (filhdrp->type != 1)
        return RCERROR(ERROR_INVALID_FILE);

    /*
     * Get the component header entries.
     */
    filent = (ICOHDR *)(datap + sizeof(RSRC_NEWHDR));

    /*
     * Write the component resources to the .RES file.
     */
    for (i = 0; i < filhdrp->nentries; i++)
    {
        wchar_t resourceName[2] = { (wchar_t)-1, 0 };

        head.ressize = filent[i].ressize;

        iconcursor_count++;
        resourceName[1] = (wchar_t)iconcursor_count;

        /* Write the resource header for ICON component */
        err = write_resourceheader(hfres, &head, &tail, resourceType, resourceName);
        if (err) return RCFATAL(err);

        /* Write the bitmap data for ICON component */
        err = my_writefile(hfres, (uchar_t *)filhdrp + filent[i].offset, head.ressize);
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, head.ressize);
        if (err) return RCFATAL(err);
    }

    /*
     * Write the icon-group resource.
     */
    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)RSRC_T_GROUP_ICON;
    grpent = my_alloc(sizeof(RSRC_ICOCUR) * filhdrp->nentries);

    grphdr.type = 1;
    grphdr.reserved = 0;
    grphdr.nentries = filhdrp->nentries;

    for (i = 0; i < filhdrp->nentries; i++)
    {
        grpent[i].u.ico.width = filent[i].width;
        grpent[i].u.ico.height = filent[i].height;
        grpent[i].u.ico.ncolors = filent[i].ncolors;
        grpent[i].nplanes = 1;
        grpent[i].nbitspixel = bitcount(filent[i].ncolors);
        grpent[i].ressize = filent[i].ressize;
        grpent[i].id = i + start_icon;
    }

    head.ressize = sizeof(RSRC_ICOCUR) * filhdrp->nentries + sizeof(RSRC_NEWHDR);
    /* tail.memflags = memflags | RSRC_F_PURE; */
    tail.memflags = RSRC_F_DISCARDABLE|RSRC_F_MOVEABLE|RSRC_F_PURE;

    /* Write the resource header */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err)
    {
        my_free(grpent);
        return RCFATAL(err);
    }

    /* Write the icon header block */
    err = my_writefile(hfres, &grphdr, sizeof(RSRC_NEWHDR));
    if (err)
    {
        my_free(grpent);
        return RCFATAL(err);
    }

    /* Write the group component blocks */
    for (i = 0; i < filhdrp->nentries; i++)
    {
        err = my_writefile(hfres, &grpent[i], sizeof(RSRC_ICOCUR));
        if (err)
        {
            my_free(grpent);
            return RCFATAL(err);
        }
    }

    my_free(grpent);

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_dialog                                                   *
 *                                                                          *
 * Purpose : Write a named dialog resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_dialog(const char *name, RSRC_HDR_TAIL *tailp, DIALOGHEADER *dlg)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_dialog(resourceName, tailp, dlg);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_dialog                                                *
 *                                                                          *
 * Purpose : Write a numbered dialog resource.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_dialog(wchar_t id, RSRC_HDR_TAIL *tailp, DIALOGHEADER *dlg)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_dialog(resourceName, tailp, dlg);
}

/****************************************************************************
 *                                                                          *
 * Function: write_dialog                                                   *
 *                                                                          *
 * Purpose : Write a dialog resource.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *                                                                          *
 ****************************************************************************/

static WINERR write_dialog(wchar_t *name, RSRC_HDR_TAIL *tailp, DIALOGHEADER *dlg)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_DIALOG };
    RSRC_HDR_HEAD head;
    CONTROLENTRY *ctl;
    short extra = 0;
    WINERR err;

    head.ressize = dialogsize(dlg);

    err = write_resourceheader(hfres, &head, tailp, resourceType, name);
    if (err) return RCFATAL(err);

    /*
     * Write the dialog header.
     */
    if (dlg->extended)
    {
        RSRC_DLGEXHDR item = {0};

        item.version = 1;
        item.signature = -1;
        item.helpid = dlg->helpid;
        item.exstyle = dlg->exstyle;
        item.style = dlg->style;
        item.nitems = ncontrols(dlg);
        item.x = dlg->x;
        item.y = dlg->y;
        item.width = dlg->width;
        item.height = dlg->height;

        if (item.style == 0)
            item.style = WS_POPUP|WS_BORDER|WS_SYSMENU;

        if (dlg->font.typeface != NULL)
            item.style |= DS_SETFONT;

        err = my_writefile(hfres, &item, sizeof(item));
        if (err) return RCFATAL(err);
    }
    else
    {
        RSRC_DLGHDR item = {0};

        item.style = dlg->style;
        item.exstyle = dlg->exstyle;
        item.nitems = ncontrols(dlg);
        item.x = dlg->x;
        item.y = dlg->y;
        item.width = dlg->width;
        item.height = dlg->height;

        if (item.style == 0)
            item.style = WS_POPUP|WS_BORDER|WS_SYSMENU;

        if (dlg->font.typeface != NULL)
            item.style |= DS_SETFONT;

        err = my_writefile(hfres, &item, sizeof(item));
        if (err) return RCFATAL(err);
    }

    err = write_nameord(hfres, dlg->menu ? dlg->menu : L"");
    if (err) return RCFATAL(err);

    err = write_nameord(hfres, dlg->class ? dlg->class : L"");
    if (err) return RCFATAL(err);

    err = write_nameord(hfres, dlg->caption ? dlg->caption : L"");
    if (err) return RCFATAL(err);

    if (dlg->font.typeface != NULL)
    {
        err = my_writefile(hfres, &dlg->font.ptsize, sizeof(dlg->font.ptsize));
        if (err) return RCFATAL(err);

        if (dlg->extended)
        {
            err = my_writefile(hfres, &dlg->font.weight, sizeof(dlg->font.weight));
            if (err) return RCFATAL(err);

            err = my_writefile(hfres, &dlg->font.italic, sizeof(dlg->font.italic));
            if (err) return RCFATAL(err);

            err = my_writefile(hfres, &dlg->font.charset, sizeof(dlg->font.charset));
            if (err) return RCFATAL(err);
        }

        err = write_nameord(hfres, dlg->font.typeface);
        if (err) return RCFATAL(err);
    }

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    /*
     * Write all controls in the dialog.
     */
    for (ctl = dlg->next; ctl != NULL; ctl = ctl->next)
    {
        if (dlg->extended)
        {
            RSRC_DLGEXENT item = {0};

            item.helpid = ctl->helpid;
            item.exstyle = ctl->exstyle;
            item.style = ctl->style;
            item.x = ctl->x;
            item.y = ctl->y;
            item.width = ctl->width;
            item.height = ctl->height;
            item.id = ctl->id;
            item.padding = 0;  /* MS writes garbage! */

            err = my_writefile(hfres, &item, sizeof(item));
            if (err) return RCFATAL(err);
        }
        else
        {
            RSRC_DLGENT item = {0};

            item.style = ctl->style;
            item.exstyle = ctl->exstyle;
            item.x = ctl->x;
            item.y = ctl->y;
            item.width = ctl->width;
            item.height = ctl->height;
            item.id = ctl->id;

            err = my_writefile(hfres, &item, sizeof(item));
            if (err) return RCFATAL(err);
        }

        err = write_nameord(hfres, ctl->class);
        if (err) return RCFATAL(err);

        err = write_nameord(hfres, ctl->caption ? ctl->caption : L"");
        if (err) return RCFATAL(err);

        err = my_writefile(hfres, &extra, sizeof(extra));
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, my_tellfile(hfres));
        if (err) return RCFATAL(err);
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: dialogsize                                                     *
 *                                                                          *
 * Purpose : Calculate the total dialog size.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           02-02-27  Added support for DIALOGEX resources.                *
 *                                                                          *
 ****************************************************************************/

static size_t dialogsize(DIALOGHEADER *dlg)
{
    CONTROLENTRY *ctl;
    size_t size = 0;

    size += (dlg->extended) ? sizeof(RSRC_DLGEXHDR) : sizeof(RSRC_DLGHDR);
    size += nameordsize(dlg->menu ? dlg->menu : L"");
    size += nameordsize(dlg->class ? dlg->class : L"");
    size += nameordsize(dlg->caption ? dlg->caption : L"");

    if (dlg->font.typeface != NULL)
    {
        size += sizeof(dlg->font.ptsize);
        size += nameordsize(dlg->font.typeface);
        if (dlg->extended)
        {
            size += sizeof(dlg->font.weight);
            size += sizeof(dlg->font.italic);
            size += sizeof(dlg->font.charset);
        }
    }

    for (ctl = dlg->next; ctl != NULL; ctl = ctl->next)
    {
        if (size % 4) size += 2;
        size += (dlg->extended) ? sizeof(RSRC_DLGEXENT) : sizeof(RSRC_DLGENT);
        size += nameordsize(ctl->class);
        size += nameordsize(ctl->caption ? ctl->caption : L"");
        size += sizeof(short);
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: ncontrols                                                      *
 *                                                                          *
 * Purpose : Calculate the total number of controls.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int ncontrols(DIALOGHEADER *dlg)
{
    CONTROLENTRY *ctl;
    int nctl = 0;

    for (ctl = dlg->next; ctl != NULL; ctl = ctl->next)
        nctl++;

    return nctl;
}

/****************************************************************************
 *                                                                          *
 * Function: named_dlginclude                                               *
 *                                                                          *
 * Purpose : Write a named dialog-include resource.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_dlginclude(const char *name, const char *include)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_dlginclude(resourceName, include);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_dlginclude                                            *
 *                                                                          *
 * Purpose : Write a numbered dialog-include resource.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_dlginclude(wchar_t id, const char *include)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_dlginclude(resourceName, include);
}

/****************************************************************************
 *                                                                          *
 * Function: write_dlginclude                                               *
 *                                                                          *
 * Purpose : Write a dialog-include resource.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_dlginclude(wchar_t *name, const char *include)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_DLGINCLUDE };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    head.ressize = strlen(include)+1;
    tail.memflags = RSRC_F_DISCARDABLE|RSRC_F_PURE|RSRC_F_MOVEABLE;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, include, strlen(include)+1);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_manifest                                                 *
 *                                                                          *
 * Purpose : Write a named MANIFEST resource.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-24  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_manifest(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_manifest(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_manifest                                              *
 *                                                                          *
 * Purpose : Write a numbered MANIFEST resource.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-24  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_manifest(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_manifest(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_manifest                                                 *
 *                                                                          *
 * Purpose : Write a MANIFEST resource.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-24  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_manifest(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_MANIFEST };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_menu                                                     *
 *                                                                          *
 * Purpose : Write a named menu resource.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_menu(const char *name, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_menu(resourceName, tailp, menu);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_menu                                                  *
 *                                                                          *
 * Purpose : Write a numbered menu resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_menu(wchar_t id, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_menu(resourceName, tailp, menu);
}

/****************************************************************************
 *                                                                          *
 * Function: write_menu                                                     *
 *                                                                          *
 * Purpose : Write a menu resource.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_menu(wchar_t *name, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_MENU };
    RSRC_HDR_HEAD head;
    RSRC_MENUHDR header;
    WINERR err;

    head.ressize = menusize(menu) + sizeof(header);

    header.version = 0;
    header.hdrsize = 0;

    err = write_resourceheader(hfres, &head, tailp, resourceType, name);
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, &header, sizeof(header));
    if (err) return RCFATAL(err);

    err = write_menutree(menu);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: write_menutree                                                 *
 *                                                                          *
 * Purpose : Convert the menu tree to the resource format.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_menutree(MENUTREE *menu)
{
    wchar_t null[4] = { 0, 0, 0, 0 };
    WINERR err;
    int i;

    for (i = 0; i < menu->num; i++)
    {
        if (menu->items[i]->flags & RSRC_M_POPUP)
        {
            RSRC_MENUPOPUP item;

            item.flags = menu->items[i]->flags;

            /* Write a popup menu item */
            err = my_writefile(hfres, &item, offsetof(RSRC_MENUPOPUP, text));
            if (err) return err;

            err = write_nameord(hfres, menu->items[i]->title);
            if (err) return err;

            /* Recurse inte submenus */
            err = write_menutree(menu->items[i]);
            if (err) return err;
        }
        else if (menu->items[i]->flags & RSRC_M_SEPARATOR)
        {
            /* Write a separator item */
            err = my_writefile(hfres, &null, 3 * sizeof(wchar_t));
            if (err) return err;
        }
        else
        {
            RSRC_MENUITEM item;

            item.flags = menu->items[i]->flags;
            item.id = menu->items[i]->id;

            /* Write a normal menu item */
            err = my_writefile(hfres, &item, offsetof(RSRC_MENUITEM, text));
            if (err) return err;

            err = write_nameord(hfres, menu->items[i]->title);
            if (err) return err;
        }
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: menusize                                                       *
 *                                                                          *
 * Purpose : Calculate the total menu size.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t menusize(MENUTREE *menu)
{
    size_t size = 0;
    int i;

    for (i = 0; i < menu->num; i++)
    {
        if (menu->items[i]->flags & RSRC_M_POPUP)
        {
            size += offsetof(RSRC_MENUPOPUP, text);
            size += nameordsize(menu->items[i]->title);
            size += menusize(menu->items[i]);
        }
        else
        {
            size += offsetof(RSRC_MENUITEM, text);
            size += nameordsize(menu->items[i]->title);
        }
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: named_menuex                                                   *
 *                                                                          *
 * Purpose : Write a named extended menu resource.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_menuex(const char *name, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_menuex(resourceName, tailp, menu);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_menuex                                                *
 *                                                                          *
 * Purpose : Write a numbered extended menu resource.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_menuex(wchar_t id, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_menuex(resourceName, tailp, menu);
}

/****************************************************************************
 *                                                                          *
 * Function: write_menuex                                                   *
 *                                                                          *
 * Purpose : Write a extended menu resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_menuex(wchar_t *name, RSRC_HDR_TAIL *tailp, MENUTREE *menu)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_MENU };
    RSRC_HDR_HEAD head;
    RSRC_MENUEXHDR header;
    WINERR err;

    head.ressize = menuexsize(menu) + sizeof(header);

    header.version = 1;
    header.hdrsize = sizeof(long);
    header.helpid = 0;

    err = write_resourceheader(hfres, &head, tailp, resourceType, name);
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, &header, sizeof(header));
    if (err) return RCFATAL(err);

    err = write_menuextree(menu);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: write_menuextree                                               *
 *                                                                          *
 * Purpose : Convert the menu tree to the resource format.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           98-06-21  Bugfix: didn't use RSRC_M_ENDMENU on popups.         *
 *                                                                          *
 ****************************************************************************/

static WINERR write_menuextree(MENUTREE *menu)
{
    WINERR err;
    int i;

    for (i = 0; i < menu->num; i++)
    {
        RSRC_MENUEXITEM item = {0};

        item.type = menu->items[i]->type;
        item.state = menu->items[i]->state;
        item.id = menu->items[i]->id;
        item.flags = (menu->items[i]->flags & RSRC_M_ENDMENU) ? 0x80 : 0;

        if (menu->items[i]->flags & RSRC_M_POPUP)
        {
            item.flags |= 0x01;  /* Popup follows */

            /* Write a popup menu item */
            err = my_writefile(hfres, &item, offsetof(RSRC_MENUEXITEM, text));
            if (err) return err;

            err = write_nameord(hfres, menu->items[i]->title);
            if (err) return err;

            err = write_alignment(hfres, my_tellfile(hfres));
            if (err) return err;

            err = my_writefile(hfres, &menu->items[i]->helpid, sizeof(long));
            if (err) return err;

            /* Recurse inte submenus */
            err = write_menuextree(menu->items[i]);
            if (err) return err;
        }
        else
        {
            /* Write a normal menu item */
            err = my_writefile(hfres, &item, offsetof(RSRC_MENUEXITEM, text));
            if (err) return err;

            err = write_nameord(hfres, menu->items[i]->title);
            if (err) return err;

            err = write_alignment(hfres, my_tellfile(hfres));
            if (err) return err;
        }
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: menuexsize                                                     *
 *                                                                          *
 * Purpose : Calculate the total menu size.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t menuexsize(MENUTREE *menu)
{
    size_t size = 0;
    int i;

    for (i = 0; i < menu->num; i++)
    {
        if (menu->items[i]->flags & RSRC_M_POPUP)
        {
            size += offsetof(RSRC_MENUEXITEM, text) + /*helpid*/ sizeof(long);
            size += nameordsize(menu->items[i]->title);
            size += menuexsize(menu->items[i]);
            if (size % 4) size += 2;
        }
        else
        {
            size += offsetof(RSRC_MENUEXITEM, text);
            size += nameordsize(menu->items[i]->title);
            if (size % 4) size += 2;
        }
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: named_msgtable                                                 *
 *                                                                          *
 * Purpose : Write a named messagetable resource.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_msgtable(const char *name, const char *filename)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_msgtable(resourceName, filename);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_msgtable                                              *
 *                                                                          *
 * Purpose : Write a numbered messagetable resource.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_msgtable(wchar_t id, const char *filename)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_msgtable(resourceName, filename);
}

/****************************************************************************
 *                                                                          *
 * Function: write_msgtable                                                 *
 *                                                                          *
 * Purpose : Write a (binary) messagetable resource.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_msgtable(wchar_t *name, const char *filename)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_MESSAGETABLE };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = RSRC_F_MOVEABLE|RSRC_F_PURE;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_plugplay                                                 *
 *                                                                          *
 * Purpose : Write a named PLUGPLAY resource.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_plugplay(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_plugplay(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_plugplay                                              *
 *                                                                          *
 * Purpose : Write a numbered PLUGPLAY resource.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_plugplay(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_plugplay(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_plugplay                                                 *
 *                                                                          *
 * Purpose : Write a PLUGPLAY resource.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_plugplay(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_PLUGPLAY };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_rcdata                                                   *
 *                                                                          *
 * Purpose : Write a named RCDATA resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_rcdata(const char *name, RSRC_HDR_TAIL *tailp, uchar_t *datap, size_t ndata)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_rcdata(resourceName, tailp, datap, ndata);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_rcdata                                                *
 *                                                                          *
 * Purpose : Write a numbered RCDATA resource.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_rcdata(wchar_t id, RSRC_HDR_TAIL *tailp, uchar_t *datap, size_t ndata)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_rcdata(resourceName, tailp, datap, ndata);
}

/****************************************************************************
 *                                                                          *
 * Function: write_rcdata                                                   *
 *                                                                          *
 * Purpose : Write a RCDATA resource.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_rcdata(wchar_t *name, RSRC_HDR_TAIL *tailp, uchar_t *datap, size_t ndata)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_RCDATA };
    RSRC_HDR_HEAD head;
    WINERR err;

    head.ressize = ndata;

    err = write_resourceheader(hfres, &head, tailp, resourceType, name);
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, datap, ndata);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_version                                                  *
 *                                                                          *
 * Purpose : Write a named version resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_version(const char *name, RSRC_VERFIX *vsffi, VERSTRINGTABLE *strtab, VERVARTABLE *vartab)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_version(resourceName, vsffi, strtab, vartab);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_version                                               *
 *                                                                          *
 * Purpose : Write a numbered version resource.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_version(wchar_t id, RSRC_VERFIX *vsffi, VERSTRINGTABLE *strtab, VERVARTABLE *vartab)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_version(resourceName, vsffi, strtab, vartab);
}

/****************************************************************************
 *                                                                          *
 * Function: write_version                                                  *
 *                                                                          *
 * Purpose : Write a version resource (by far the most complicated).        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_version(wchar_t *name, RSRC_VERFIX *vsffi, VERSTRINGTABLE *strtab, VERVARTABLE *vartab)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_VERSION };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    RSRC_VERHDR vsvi;
    ushort_t size;
    WINERR err;
    int i;

    size = sizeof(RSRC_VERHDR) +
        wstrsize(L"VS_VERSION_INFO") + 2 + sizeof(RSRC_VERFIX) +
        verstringsize(strtab, 1) +
        vervarsize(vartab);

    if ((size % 4) && vervarsize(vartab)) size += 2;

    vsvi.size = size;
    vsvi.versize = sizeof(RSRC_VERFIX);
    vsvi.type = RSRC_V_BINARY;

    head.ressize = size;
    tail.memflags = RSRC_F_MOVEABLE|RSRC_F_PURE;

    /*
     * Write the RSRC_VERFIX structure.
     */
    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, &vsvi, sizeof(vsvi));
    if (err) return RCFATAL(err);

    err = write_nameord(hfres, L"VS_VERSION_INFO");
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, my_tellfile(hfres));
    if (err) return RCFATAL(err);

    err = my_writefile(hfres, vsffi, sizeof(*vsffi));
    if (err) return RCFATAL(err);

    /*
     * Write the "StringFileInfo" structures.
     */
    if (strtab != NULL)
    {
        vsvi.size = verstringsize(strtab, 1);
        vsvi.versize = 0;
        vsvi.type = RSRC_V_TEXT;

        err = my_writefile(hfres, &vsvi, sizeof(vsvi));
        if (err) return RCFATAL(err);

        err = write_nameord(hfres, L"StringFileInfo");
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, my_tellfile(hfres));
        if (err) return RCFATAL(err);

        for (; strtab != NULL; strtab = strtab->next)
        {
            vsvi.size = verstringsize(strtab, 0);
            vsvi.versize = 0;
            vsvi.type = RSRC_V_TEXT;

            err = my_writefile(hfres, &vsvi, sizeof(vsvi));
            if (err) return RCFATAL(err);

            err = write_nameord(hfres, strtab->lang_charset);
            if (err) return RCFATAL(err);

            for (i = 0; i < strtab->num; i++)
            {
                err = write_alignment(hfres, my_tellfile(hfres));
                if (err) return RCFATAL(err);

                vsvi.size = sizeof(RSRC_VERHDR);
                vsvi.size += wstrsize(strtab->strings[i]->key);
                if (vsvi.size % 4) vsvi.size += 2;
                vsvi.size += wstrsize(strtab->strings[i]->val);
                vsvi.versize = wstrlen(strtab->strings[i]->val) + 1;
                vsvi.type = RSRC_V_TEXT;

                err = my_writefile(hfres, &vsvi, sizeof(vsvi));
                if (err) return RCFATAL(err);

                err = write_nameord(hfres, strtab->strings[i]->key);
                if (err) return RCFATAL(err);

                err = write_alignment(hfres, my_tellfile(hfres));
                if (err) return RCFATAL(err);

                err = write_nameord(hfres, strtab->strings[i]->val);
                if (err) return RCFATAL(err);

                err = write_alignment(hfres, my_tellfile(hfres));
                if (err) return RCFATAL(err);
            }
        }
    }

    /*
     * Write the "VarFileInfo" structures.
     */
    if (vartab != NULL)
    {
        vsvi.size = vervarsize(vartab);

        vsvi.versize = 0;
        vsvi.type = RSRC_V_TEXT;

        err = my_writefile(hfres, &vsvi, sizeof(vsvi));
        if (err) return RCFATAL(err);

        err = write_nameord(hfres, L"VarFileInfo");
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, my_tellfile(hfres));
        if (err) return RCFATAL(err);

        vsvi.size = sizeof(RSRC_VERHDR) + wstrsize(vartab->key);
        if (vsvi.size % 4) vsvi.size += 2;
        vsvi.size += vartab->num * sizeof(long);
        vsvi.versize = vartab->num * sizeof(long);
        vsvi.type = RSRC_V_BINARY;

        err = my_writefile(hfres, &vsvi, sizeof(vsvi));
        if (err) return RCFATAL(err);

        err = write_nameord(hfres, vartab->key);
        if (err) return RCFATAL(err);

        err = write_alignment(hfres, my_tellfile(hfres));
        if (err) return RCFATAL(err);

        vsvi.size = sizeof(RSRC_VERHDR) + vartab->num * sizeof(long);
        vsvi.versize = vartab->num * sizeof(long);
        vsvi.type = RSRC_V_TEXT;

        err = my_writefile(hfres, vartab->val, vartab->num * sizeof(long));
        if (err) return RCFATAL(err);
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: verstringsize                                                  *
 *                                                                          *
 * Purpose : Calculate the size of the "StringFileInfo" part.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t verstringsize(VERSTRINGTABLE *strtab, int walk)
{
    size_t size = 0;
    int i;

    if (strtab && walk)
    {
        size += sizeof(RSRC_VERHDR);
        size += wstrsize(L"StringFileInfo");
        if (size % 4) size += 2;
    }

    for (; strtab != NULL; strtab = (walk) ? strtab->next : NULL)
    {
        size += sizeof(RSRC_VERHDR);
        size += wstrsize(strtab->lang_charset);

        for (i = 0; i < strtab->num; i++)
        {
            if (size % 4) size += 2;
            size += sizeof(RSRC_VERHDR);
            size += wstrsize(strtab->strings[i]->key);
            if (size % 4) size += 2;
            size += wstrsize(strtab->strings[i]->val);
        }
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: vervarsize                                                     *
 *                                                                          *
 * Purpose : Calculate the size of the "VarFileInfo" part.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t vervarsize(VERVARTABLE *vartab)
{
    size_t size = 0;

    if (vartab != NULL)
    {
        size += sizeof(RSRC_VERHDR);
        size += wstrsize(L"VarFileInfo");
        if (size % 4) size += 2;

        size += sizeof(RSRC_VERHDR);
        size += wstrsize(vartab->key);
        if (size % 4) size += 2;

        size += vartab->num * sizeof(long);
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: named_vxd                                                      *
 *                                                                          *
 * Purpose : Write a named VXD resource.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_vxd(const char *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_vxd(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_vxd                                                   *
 *                                                                          *
 * Purpose : Write a numbered VXD resource.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_vxd(wchar_t id, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    return write_vxd(resourceName, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_vxd                                                      *
 *                                                                          *
 * Purpose : Write a VXD resource.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-01-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_vxd(wchar_t *name, const char *filename, ushort_t memflags)
{
    wchar_t resourceType[2] = { (wchar_t)-1, RSRC_T_VXD };
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: named_generic_named_type                                       *
 *                                                                          *
 * Purpose : Write a named generic resource with named type.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_generic_named_type(const char *name, const char *type, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];
    wchar_t resourceType[50];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    if (!ansi2uni(resourceType, type))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_generic(resourceName, resourceType, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: named_generic_numbered_type                                    *
 *                                                                          *
 * Purpose : Write a named generic resource with numbered type.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR named_generic_numbered_type(const char *name, wchar_t type, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[50];
    wchar_t resourceType[2];

    if (!ansi2uni(resourceName, name))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)type;

    return write_generic(resourceName, resourceType, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_generic_named_type                                    *
 *                                                                          *
 * Purpose : Write a numbered generic resource with named type.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_generic_named_type(wchar_t id, const char *type, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];
    wchar_t resourceType[50];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    if (!ansi2uni(resourceType, type))
        apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);

    return write_generic(resourceName, resourceType, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: numbered_generic_numbered_type                                 *
 *                                                                          *
 * Purpose : Write a numbered generic resource with numbered type.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR numbered_generic_numbered_type(wchar_t id, wchar_t type, const char *filename, ushort_t memflags)
{
    wchar_t resourceName[2];
    wchar_t resourceType[2];

    resourceName[0] = (wchar_t)-1;
    resourceName[1] = (wchar_t)id;

    resourceType[0] = (wchar_t)-1;
    resourceType[1] = (wchar_t)type;

    return write_generic(resourceName, resourceType, filename, memflags);
}

/****************************************************************************
 *                                                                          *
 * Function: write_generic                                                  *
 *                                                                          *
 * Purpose : Write a generic resource.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-06-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_generic(wchar_t *name, wchar_t *resourceType, const char *filename, ushort_t memflags)
{
    RSRC_HDR_HEAD head;
    RSRC_HDR_TAIL tail = commontail;
    WINERR err;

    if (idemode) add_fileinfo(resourceType, name, filename);

    err = my_filesize(filename, (ulong_t *)&head.ressize);
    if (err) return RCERROR(err);

    tail.memflags = memflags;

    err = write_resourceheader(hfres, &head, &tail, resourceType, name);
    if (err) return RCFATAL(err);

    err = copy_anyfile(hfres, filename);
    if (err) return RCFATAL(err);

    err = write_alignment(hfres, head.ressize);
    if (err) return RCFATAL(err);

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: write_resourceheader                                           *
 *                                                                          *
 * Purpose : Write header before the actual resource.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_resourceheader(HANDLE hf, RSRC_HDR_HEAD *headp,
    RSRC_HDR_TAIL *tailp, wchar_t *type, wchar_t *name)
{
    long padsize = 0;
    WINERR err;

    headp->hdrsize =
        sizeof(RSRC_HDR_HEAD) +
        sizeof(RSRC_HDR_TAIL) +
        nameordsize(type) +
        nameordsize(name);

    if (headp->hdrsize % 4)
    {
        headp->hdrsize += 2;
        padsize = 2;
    }

    err = my_writefile(hf, headp, sizeof(*headp));
    if (err) return err;

    err = write_nameord(hf, type);
    if (err) return err;

    err = write_nameord(hf, name);
    if (err) return err;

    err = write_alignment(hf, headp->hdrsize - padsize);
    if (err) return err;

    err = my_writefile(hf, tailp, sizeof(*tailp));
    if (err) return err;

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: write_alignment                                                *
 *                                                                          *
 * Purpose : Write alignment, if needed.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           98-06-22  Bugfix: bad assumption about WORD alignment.         *
 *                                                                          *
 ****************************************************************************/

static WINERR write_alignment(HANDLE hf, long ressize)
{
    if (ressize % 4)
    {
        long pad = 0;
        return my_writefile(hf, &pad, 4 - (ressize % 4));
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: write_nameord                                                  *
 *                                                                          *
 * Purpose : Write a name or ordinal number.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR write_nameord(HANDLE hf, wchar_t *nameord)
{
    if (nameord[0] == (wchar_t)-1)
    {
        /* write the integer value */
        return my_writefile(hf, nameord, 2 * sizeof(wchar_t));
    }
    else
    {
        /* write the UNICODE string and trailing 0 */
        return my_writefile(hf, nameord, wstrsize(nameord));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: nameordsize                                                    *
 *                                                                          *
 * Purpose : Return the size of the name or ordinal number.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t nameordsize(wchar_t *nameord)
{
    if (nameord[0] == (wchar_t)-1)
    {
        /* integer value */
        return 2 * sizeof(wchar_t);
    }
    else
    {
        /* UNICODE string */
        return wstrsize(nameord);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: copy_anyfile                                                   *
 *                                                                          *
 * Purpose : Copy a file to the output file.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static WINERR copy_anyfile(HANDLE hfd, const char *filename)
{
    HANDLE hfs;
    WINERR err;

    err = my_openfile(filename, &hfs);
    if (err) return err;

    err = my_copystream(hfd, hfs);
    my_closefile(hfs);

    return err;
}

/****************************************************************************
 *                                                                          *
 * Function: add_fileinfo                                                   *
 *                                                                          *
 * Purpose : Add a new file information node to the global list.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-11  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_fileinfo(wchar_t *type, wchar_t *name, const char *filename)
{
    FILEINFO *info;

    info = (FILEINFO *)my_alloc(sizeof(FILEINFO));
    info->next = NULL;
    info->type = nameorddup(type);
    info->name = nameorddup(name);
    info->language = commontail.language;
    info->filename = strcpy(my_alloc(strlen(filename)+1), filename);

    if (fileinfo_list == NULL)
    {
        /* This is the first node. Start the list */
        fileinfo_list = info;
    }
    else
    {
        FILEINFO *infoT;

        /* Append the new node to the list */
        for (infoT = fileinfo_list; infoT->next != NULL; infoT = infoT->next)
            ;
        infoT->next = info;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: nameorddup                                                     *
 *                                                                          *
 * Purpose : Make a copy of the name or ordinal number.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-11  Created                                              *
 *                                                                          *
 ****************************************************************************/

static wchar_t *nameorddup(wchar_t *nameord)
{
    if (nameord[0] == (wchar_t)-1)
    {
        /* copy the integer value */
        return memcpy(my_alloc(2 * sizeof(wchar_t)), nameord, 2 * sizeof(wchar_t));
    }
    else
    {
        /* copy the UNICODE string and trailing 0 */
        return memcpy(my_alloc(wstrsize(nameord)), nameord, wstrsize(nameord));
    }
}




