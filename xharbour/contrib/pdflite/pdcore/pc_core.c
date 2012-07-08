/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2006 Thomas Merz and PDFlib GmbH. All rights reserved. |
 +---------------------------------------------------------------------------+
 |                                                                           |
 |    This software is subject to the PDFlib license. It is NOT in the       |
 |    public domain. Extended versions and commercial licenses are           |
 |    available, please check http://www.pdflib.com.                         |
 |                                                                           |
 *---------------------------------------------------------------------------*/

/* $Id$
 *
 * PDFlib core services
 *
 */

#include "pc_util.h"
#include "pc_string.h"
#include "pc_ctype.h"

#define PDF_UnknownError  12

#if defined(__ia64__) && defined (__linux__)
#define PDC_ALIGN16
#endif

/* TODO: how to make this dynamic?
** exception during pdc_core_init():
**	- out of memory in pdc_bs_new()
*/
#define PDC_ERRPARM_SIZE	2048
#define PDC_ERRBUF_SIZE		(5 * PDC_ERRPARM_SIZE)
#define PDC_XSTACK_INISIZE	10

#define PDC_CLASSLIST_SIZE      32

#define N_ERRTABS		(PDC_ET_LAST / 1000)

/* temporary free store.
*/
typedef struct
{
    void  *		mem;
    pdc_destructor	destr;
    void  *		opaque;
} pdc_tmpmem;

typedef struct
{
    pdc_tmpmem *	tmpmem;
    int			capacity;
    int			size;
} pdc_tmpmem_list;


/* exception handling frame.
*/
typedef struct
{
    pdc_jmpbuf		jbuf;
} pdc_xframe;

typedef struct
{
    const pdc_error_info *	ei;
    int				n_entries;
} error_table;


/* ------------------------ the core private structure ---------------------- */

struct pdc_core_priv_s
{
    /* ------------ try/catch ------------ */
    pdc_xframe *	x_stack;
#ifdef PDC_ALIGN16
    char *		x_alias;
#endif
    int			x_ssize;
    int			x_sp;		/* exception stack pointer	*/
    int			x_sp0;		/* exception stack pointer at	*/
					/* the time of pdc_enter_api()	*/

    /* ------------ error handling ------------ */
    pdc_bool            in_error;       /* while initializing pdcore or */
                                        /* while creating error resp.   */
    char *              premsg;
    char                errbuf[PDC_ERRBUF_SIZE];
    char		errparms[4][PDC_ERRPARM_SIZE];
    int			epcount;
    int			errnum;
    pdc_bool		x_thrown;	/* exception thrown and not caught */
    char 	        apiname[32];
    pdc_error_fp	errorhandler;	/* client error handler		*/
    void *		opaque;		/* client specific, opaque data */

    error_table		err_tables[N_ERRTABS];

#ifdef	PDC_DEBUG
    pdc_bool		hexdump;	/* hexdump feature enabled? */
#endif /* PDC_DEBUG */

    /* ------------ memory management ------------ */
    pdc_alloc_fp	allocproc;
    pdc_realloc_fp	reallocproc;
    pdc_free_fp		freeproc;
    pdc_tmpmem_list	tm_list;
};


/* ----------- default memory management & error handling ----------- */

static void *
default_malloc(void *opaque, size_t size, const char *caller)
{
    (void) opaque;
    (void) caller;

    return malloc(size);
}

static void *
default_realloc(void *opaque, void *mem, size_t size, const char *caller)
{
    (void) opaque;
    (void) caller;

    return realloc(mem, size);
}

static void
default_free(void *opaque, void *mem)
{
    (void) opaque;

    free(mem);
}

static void
default_errorhandler(void *opaque, int errnum, const char *msg)
{
    (void) opaque;
    (void) errnum;

    fprintf(stderr, "fatal exception: %s\n", msg);
    exit(99);
}

pdc_bool
pdc_enter_api(pdc_core *pdc, const char *apiname)
{
    char *name = NULL;

    if (pdc->pr->in_error)
	return pdc_false;

    if (pdc->objorient)
        name = (char *) strchr(apiname, '_');
    if (name)
        name++;
    else
        name = (char *) apiname;
    if (name[0] == '\n')
        name++;

    strcpy(pdc->pr->apiname, name);

    if (pdc->binding != NULL)
    {
        size_t len = strlen(pdc->pr->apiname);
        len--;
        if (len && pdc->pr->apiname[len] == '2')
            pdc->pr->apiname[len] = 0;
    }

    pdc->pr->errnum = 0;
    pdc->pr->x_sp0 = pdc->pr->x_sp;
    return pdc_true;
}

pdc_bool
pdc_in_error(pdc_core *pdc)
{
    return pdc->pr->in_error;
}


/* --------------------- error table management --------------------- */

static pdc_error_info	core_errors[] =
{
#define		pdc_genInfo	1
#include	"pc_generr.h"
};

#define N_CORE_ERRORS	(sizeof core_errors / sizeof (pdc_error_info))


static void
pdc_panic(pdc_core *pdc, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);

#if defined (PDC_NO_VSNPRINTF)
    vsprintf(pdc->pr->errbuf, fmt, ap);
#else
#if defined(WIN32)
    _vsnprintf(pdc->pr->errbuf, PDC_ERRPARM_SIZE, fmt, ap);
#else
    vsnprintf(pdc->pr->errbuf, PDC_ERRPARM_SIZE, fmt, ap);
#endif
#endif

    va_end(ap);

    (*pdc->pr->errorhandler)(pdc->pr->opaque, PDF_UnknownError,
                             pdc->pr->errbuf);
} /* pdc_panic */


static void
check_parms(pdc_core *pdc, const pdc_error_info *ei)
{
    const char *msg = ei->errmsg;
    const char *dollar;

    while ((dollar = strchr(msg, '$')) != (char *) 0)
    {
	if (pdc_isdigit(dollar[1]))
	{
	    int n = dollar[1] - '0';

	    if (ei->nparms < n || n < 1)
		pdc_panic(pdc, "illegal parameter '$%d' in error message %d",
				    n, ei->errnum);
	}
	else if (dollar[1] != '$')
	{
	    pdc_panic(pdc,
		"illegal '$' in error message %d", ei->errnum);
	}

	msg = dollar + 1;
    }
} /* check_parms */


void
pdc_register_errtab(
    pdc_core *pdc,
    int et,
    const pdc_error_info *ei,
    int n_entries)
{
    int i;
    int n = (et / 1000) - 1;

    if (n < 0 || N_ERRTABS <= n || et % 1000 != 0)
	pdc_panic(pdc, "tried to register unknown error table %d", et);

    /* ignore multiple registrations of the same table.
    */
    if (pdc->pr->err_tables[n].ei != (pdc_error_info *) 0)
	return;

    pdc->pr->err_tables[n].ei = ei;
    pdc->pr->err_tables[n].n_entries = n_entries;

    check_parms(pdc, &ei[0]);

    for (i = 1; i < n_entries; ++i)
    {
	if (ei[i].errnum <= ei[i-1].errnum)
	{
	    pdc_panic(pdc,
		"duplicate or misplaced error number %d", ei[i].errnum);
	}

	/* an error table may span several blocks.
	*/
	if ((ei[i].errnum / 1000) - 1 > n)
	{
            pdc->pr->err_tables[n].n_entries = i; /* correct old block size */

	    n = (ei[i].errnum / 1000) - 1;	/* new block number */

	    if (N_ERRTABS <= n)
		pdc_panic(pdc, "invalid error number %d", ei[i].errnum);

	    ei += i;				/* start of new block */
	    n_entries -= i;			/* size of new block */
	    i = 0;
            pdc->pr->err_tables[n].ei = ei;
            pdc->pr->err_tables[n].n_entries = n_entries;
	}

	check_parms(pdc, &ei[i]);
    }
} /* pdc_register_errtab */


/* pdc_new_core() never throws exceptions.
** it returns NULL if there's not enough memory.
*/
pdc_core *
pdc_new_core(
    pdc_error_fp errorhandler,
    pdc_alloc_fp allocproc,
    pdc_realloc_fp reallocproc,
    pdc_free_fp freeproc,
    void *opaque,
    const char *prodname,
    const char *version)
{
    static const char fn[] = "pdc_new_core";

    pdc_core_priv *pdc_pr;
    pdc_core *pdc;
    int i;

    /* if allocproc is NULL, we use pdc's default memory handling.
    */
    if (allocproc == (pdc_alloc_fp) 0)
    {
	allocproc	= default_malloc;
	reallocproc	= default_realloc;
	freeproc	= default_free;
    }

    if (errorhandler == (pdc_error_fp) 0)
	errorhandler = default_errorhandler;

    pdc_pr = (pdc_core_priv *)
                (*allocproc)(opaque, sizeof (pdc_core_priv), fn);

    if (pdc_pr == (pdc_core_priv *) 0)
        return (pdc_core *) 0;

    pdc = (pdc_core *)
                (*allocproc)(opaque, sizeof (pdc_core), fn);

    if (pdc == (pdc_core *) 0)
        return (pdc_core *) 0;

    pdc->pr = pdc_pr;

    /* initialize client members
    */
    pdc->reslist = NULL;
    pdc->filesystem = NULL;
    pdc->logg = NULL;
    pdc->loggenv = pdc_false;
    pdc->encstack = NULL;
    pdc->pglyphtab = NULL;
    pdc->bstr_pool = NULL;
    pdc->ustr_pool = NULL;
    pdc->last_rand = 1;
    pdc->prodname = prodname;
    pdc->version = version;
    pdc->binding = NULL;
    pdc->unicaplang = pdc_false;
    pdc->objorient = pdc_false;
    pdc->hastobepos = pdc_false;
    pdc->ptfrun = pdc_false;
    pdc->smokerun = pdc_false;
    pdc->charref = pdc_false;
    pdc->escapesequ = pdc_false;
    pdc->honorlang = pdc_false;
    pdc->compatibility = PDC_X_X_LAST;
    pdc->floatdigits = 4;
    pdc->uniqueno = 0;


#ifdef PDC_DEBUG
    pdc->pr->hexdump = pdc_true;
#endif

    /* set diverse handlers
    */
    pdc->pr->errorhandler = errorhandler;
    pdc->pr->allocproc = allocproc;
    pdc->pr->reallocproc = reallocproc;
    pdc->pr->freeproc = freeproc;
    pdc->pr->opaque = opaque;

    /* initialize error & exception handling.
    */
    pdc->pr->in_error = pdc_true;  /* disable error messages */
    pdc->pr->x_thrown = pdc_false;
    pdc->pr->epcount = 0;
    pdc->pr->errnum = 0;
    pdc->pr->premsg = NULL;
    pdc->pr->errbuf[0] = 0;
    pdc->pr->apiname[0] = 0;
    pdc->pr->x_sp = -1;
    pdc->pr->x_ssize = PDC_XSTACK_INISIZE;

#ifdef PDC_ALIGN16
    pdc->pr->x_alias = (char *)
        (*allocproc)(opaque, 16 + pdc->pr->x_ssize * sizeof (pdc_xframe), fn);

    if (pdc->pr->x_alias == (char *) 0)
        pdc->pr->x_stack = (pdc_xframe *) 0;
    else
        pdc->pr->x_stack = (pdc_xframe *)
            (((unsigned long) pdc->pr->x_alias + 16) & 0xFFFFFFFFFFFFFFF0);
#else
    pdc->pr->x_stack = (pdc_xframe *)
        (*allocproc)(opaque, pdc->pr->x_ssize * sizeof (pdc_xframe), fn);
#endif

    if (pdc->pr->x_stack == (pdc_xframe *) 0)
    {
	(*freeproc)(opaque, pdc);
	return (pdc_core *) 0;
    }

    pdc_tmlist_init(pdc);

    /* initialize error tables.
    */
    for (i = 0; i < N_ERRTABS; ++i)
        pdc->pr->err_tables[i].ei = (pdc_error_info *) 0;

    pdc_register_errtab(pdc, PDC_ET_CORE, core_errors, N_CORE_ERRORS);

    /* initialize mempool for strings
    */
    pdc_init_strings(pdc);
    if (pdc->bstr_pool == NULL || pdc->ustr_pool == NULL)
    {
        (*freeproc)(opaque, pdc);
        return (pdc_core *) 0;
    }

    /* enable error messages
    */
    pdc->pr->in_error = pdc_false;


    return pdc;
}

void
pdc_delete_core(pdc_core *pdc)
{
    pdc_free_fp freeproc = pdc->pr->freeproc;
    void *opaque = pdc->pr->opaque;
    pdc_time    ltime;

    pdc_localtime(&ltime);
    pdc_logg(pdc, "[%04d-%02d-%02d %02d:%02d:%02d]\n",
        ltime.year + 1900, ltime.month + 1, ltime.mday,
        ltime.hour, ltime.minute, ltime.second);

    pdc_delete_reslist(pdc);
    pdc_delete_filesystem(pdc);
    pdc_delete_encodingstack(pdc);
    pdc_delete_pglyphtab(pdc);

    pdc_cleanup_strings(pdc);

    if (pdc->binding)
        pdc_free(pdc, pdc->binding);

    pdc_pop_errmsg(pdc);

    pdc_tmlist_cleanup(pdc);

    if (pdc->pr->tm_list.capacity != 0)
        pdc_free(pdc, pdc->pr->tm_list.tmpmem);

#ifdef PDC_ALIGN16
    pdc_free(pdc, pdc->pr->x_alias);
#else
    pdc_free(pdc, pdc->pr->x_stack);
#endif

    pdc_delete_logg(pdc);

    (*freeproc)(opaque, pdc->pr);
    (*freeproc)(opaque, pdc);
}

/* --------------------------- memory management --------------------------- */

void *
pdc_malloc(pdc_core *pdc, size_t size, const char *caller)
{
    void *ret;
    pdc_bool logg1 = pdc_logg_is_enabled(pdc, 1, trc_memory);

    if (logg1)
        pdc_logg(pdc, "\ttry to malloc %ld bytes\n", size);


    /* the behavior of malloc(0) is undefined in ANSI C, and may
     * result in a NULL pointer return value which makes PDFlib bail out.
     */
    if (size == (size_t) 0 || (long) size < 0L)
    {
	size = (size_t) 1;
	pdc_error(pdc, PDC_E_INT_ALLOC0, caller, 0, 0, 0);
    }

    if ((ret = (*pdc->pr->allocproc)(pdc->pr->opaque, size, caller)) ==
        (void *) 0)
    {
	pdc_error(pdc, PDC_E_MEM_OUT, caller, 0, 0, 0);
    }

    if (logg1)
        pdc_logg(pdc, "\t%p malloced, size=%ld, called from \"%s\"\n",
                       ret, size, caller);

    return ret;
}

/* We cook up our own calloc routine, using the caller-supplied
 * malloc and memset.
 */
void *
pdc_calloc(pdc_core *pdc, size_t size, const char *caller)
{
    void *ret;
    pdc_bool logg1 = pdc_logg_is_enabled(pdc, 1, trc_memory);

    if (logg1)
        pdc_logg(pdc, "\ttry to calloc %ld bytes\n", size);

    if (size == (size_t) 0 || (long) size < 0L)
    {
	size = (size_t) 1;
	pdc_error(pdc, PDC_E_INT_ALLOC0, caller, 0, 0, 0);
    }

    if ((ret = (*pdc->pr->allocproc)(pdc->pr->opaque, size, caller)) ==
        (void *) 0)
    {
	pdc_error(pdc, PDC_E_MEM_OUT, caller, 0, 0, 0);
    }

    if (logg1)
        pdc_logg(pdc, "\t%p calloced, size=%ld, called from \"%s\"\n",
                      ret, size, caller);

    memset(ret, 0, size);
    return ret;
}

void *
pdc_realloc(pdc_core *pdc, void *mem, size_t size, const char *caller)
{
    void *ret;
    pdc_bool logg1 = pdc_logg_is_enabled(pdc, 1, trc_memory);

    if (logg1)
        pdc_logg(pdc, "\ttry to realloc %p to %ld bytes\n", mem, size);

    if (size == (size_t) 0 || (long) size < 0L)
    {
        size = (size_t) 1;
        pdc_error(pdc, PDC_E_INT_ALLOC0, caller, 0, 0, 0);
    }

    ret = (mem == (void *) 0) ?
	(*pdc->pr->allocproc)(pdc->pr->opaque, size, caller) :
	(*pdc->pr->reallocproc)(pdc->pr->opaque, mem, size, caller);

    if (ret == (void *) 0)
	pdc_error(pdc, PDC_E_MEM_OUT, caller, 0, 0, 0);

    pdc_logg_cond(pdc, 1, trc_memory,
                       "\t%p realloced to\n"
                       "\t%p new, size=%ld, called from \"%s\"\n",
                       mem, ret, size, caller);

    return ret;
}

void
pdc_free(pdc_core *pdc, void *mem)
{
    pdc_logg_cond(pdc, 1, trc_memory, "\t%p freed\n", mem);

    /* just in case the freeproc() isn't that ANSI compatible...
    */
    if (mem != NULL)
        (*pdc->pr->freeproc)(pdc->pr->opaque, mem);
}

/* -------------------- temporary free store management -------------------- */

void
pdc_tmlist_init(pdc_core *pdc)
{
    pdc->pr->tm_list.size = pdc->pr->tm_list.capacity = 0;
}

static void
pdc_tmlist_grow(pdc_core *pdc)
{
    static const char	fn[] = "pdc_tmlist_grow";
    pdc_tmpmem_list *tm_list = &pdc->pr->tm_list;
    static const int	chunksize = 20;

    if (tm_list->capacity == 0)
    {
	tm_list->capacity = chunksize;
	tm_list->tmpmem = (pdc_tmpmem *) pdc_malloc(pdc,
	    (size_t) (tm_list->capacity * sizeof (pdc_tmpmem)), fn);
    }
    else
    {
	tm_list->capacity += chunksize;
	tm_list->tmpmem = (pdc_tmpmem *) pdc_realloc(pdc, tm_list->tmpmem,
	    (size_t) (tm_list->capacity * sizeof (pdc_tmpmem)), fn);
    }
}

void
pdc_tmlist_cleanup(pdc_core *pdc)
{
    pdc_tmpmem_list *tm_list = &pdc->pr->tm_list;
    int i;

    for (i = 0; i < tm_list->size; ++i)
    {
	if (tm_list->tmpmem[i].destr)
            tm_list->tmpmem[i].destr(tm_list->tmpmem[i].opaque,
                                     tm_list->tmpmem[i].mem);

	pdc_free(pdc, tm_list->tmpmem[i].mem);
    }

    tm_list->size = 0;
}

void
pdc_insert_mem_tmp(
    pdc_core *          pdc,
    void *              memory,
    void *              opaque,
    pdc_destructor      destr)
{
    pdc_tmpmem_list *tm_list = &pdc->pr->tm_list;

    if (tm_list->size == tm_list->capacity)
        pdc_tmlist_grow(pdc);

    pdc_logg_cond(pdc, 2, trc_memory,
        "\tTemporary memory %p was created\n", memory);

    tm_list->tmpmem[tm_list->size].mem = memory;
    tm_list->tmpmem[tm_list->size].destr = destr;
    tm_list->tmpmem[tm_list->size].opaque = opaque;
    ++tm_list->size;
}

void *
pdc_malloc_tmp(
    pdc_core *          pdc,
    size_t              size,
    const char *        caller,
    void *              opaque,
    pdc_destructor      destr)
{
    void *memory = pdc_malloc(pdc, size, caller);

    pdc_insert_mem_tmp(pdc, memory, opaque, destr);

    return memory;
}

void *
pdc_calloc_tmp(
    pdc_core *		pdc,
    size_t		size,
    const char *	caller,
    void *		opaque,
    pdc_destructor	destr)
{
    void *memory = pdc_calloc(pdc, size, caller);

    pdc_insert_mem_tmp(pdc, memory, opaque, destr);

    return memory;
}

void *
pdc_realloc_tmp(pdc_core *pdc, void *mem, size_t size, const char *caller)
{
    pdc_tmpmem_list *tm_list = &pdc->pr->tm_list;
    int i;

    for (i = tm_list->size - 1; 0 <= i; --i)
	if (tm_list->tmpmem[i].mem == mem)
	    return tm_list->tmpmem[i].mem = pdc_realloc(pdc, mem, size, caller);

    pdc_error(pdc, PDC_E_INT_REALLOC_TMP, caller, 0, 0, 0);
    return (void *) 0;
}

void
pdc_free_tmp(pdc_core *pdc, void *mem)
{
    pdc_tmpmem_list *tm_list = &pdc->pr->tm_list;
    int i, j;

    pdc_logg_cond(pdc, 2, trc_memory,
                       "\tTemporary memory %p to be freed\n", mem);

    /* we search the list backwards since chances are good
    ** that the most recently allocated items are freed first.
    */
    for (i = tm_list->size - 1; 0 <= i; --i)
    {
	if (tm_list->tmpmem[i].mem == mem)
	{
	    if (tm_list->tmpmem[i].destr)
		tm_list->tmpmem[i].destr(
		    tm_list->tmpmem[i].opaque, tm_list->tmpmem[i].mem);

	    pdc_free(pdc, tm_list->tmpmem[i].mem);
	    tm_list->tmpmem[i].mem = (void *) 0;

            --tm_list->size;
            for (j = i; j < tm_list->size; j++)
                tm_list->tmpmem[j] = tm_list->tmpmem[j + 1];

	    return;
	}
    }

    pdc_error(pdc, PDC_E_INT_FREE_TMP, 0, 0, 0, 0);
}


/* --------------------------- exception handling --------------------------- */

const char *pdc_errprintf(pdc_core *pdc, const char *fmt, ...)
{
    va_list ap;

    if (pdc->pr->epcount < 0 || pdc->pr->epcount > 3)
        pdc->pr->epcount = 0;

    va_start(ap, fmt);
    pdc_vsnprintf(pdc, pdc->pr->errparms[pdc->pr->epcount], PDC_ERRPARM_SIZE,
                  fmt, ap);
    va_end(ap);

    return pdc->pr->errparms[pdc->pr->epcount++];
}

static const pdc_error_info *
get_error_info(pdc_core *pdc, int errnum)
{
    int n = (errnum / 1000) - 1;

    if (0 <= n && n < N_ERRTABS && pdc->pr->err_tables[n].ei != 0)
    {
        error_table *etab = &pdc->pr->err_tables[n];
	int i;

	/* LATER: binary search. */
	for (i = 0; i < etab->n_entries; ++i)
	{
	    if (etab->ei[i].errnum == errnum)
		return &etab->ei[i];
	}
    }

    pdc_panic(pdc, "Internal error: unknown error number %d", errnum);

    return (pdc_error_info *) 0;	/* for the compiler */
} /* get_error_info */


static void
make_errmsg(
    pdc_core *		pdc,
    const pdc_error_info *ei,
    const char *	parm1,
    const char *	parm2,
    const char *	parm3,
    const char *	parm4,
    pdc_bool            popmsg)
{
    const char *src = ei->ce_msg ? ei->ce_msg : ei->errmsg;
    char *      dst = pdc->pr->errbuf;
    const char *dollar;

    if (pdc->pr->premsg != NULL)
    {
        strcpy(dst, pdc->pr->premsg);
        dst += strlen(pdc->pr->premsg);
        if (popmsg)
            pdc_pop_errmsg(pdc);
    }

    pdc->pr->epcount = 0;

    /* copy *src to *dst, replacing "$N" with *parmN.
    */
    while ((dollar = strchr(src, '$')) != (char *) 0)
    {
	const char *parm = (const char *) 0;

	memcpy(dst, src, (size_t) (dollar - src));
	dst += dollar - src;
	src = dollar + 1;

	switch (*src)
	{
	    case '1':	parm = (parm1 ? parm1 : "?");	break;
	    case '2':	parm = (parm2 ? parm2 : "?");	break;
	    case '3':	parm = (parm3 ? parm3 : "?");	break;
	    case '4':	parm = (parm4 ? parm4 : "?");	break;

	    case 0:	break;

	    default:	*(dst++) = *(src++);
			break;
	}

	if (parm != (const char *) 0)
	{
	    ++src;
	    strcpy(dst, parm);
	    dst += strlen(parm);
	}
    }

    strcpy(dst, src);

} /* make_errmsg */

void
pdc_pop_errmsg(pdc_core *pdc)
{
    if (pdc->pr->premsg)
    {
        pdc_free(pdc, pdc->pr->premsg);
        pdc->pr->premsg = NULL;
    }
} /* pdc_pop_errmsg */

void
pdc_push_errmsg(
    pdc_core *  pdc,
    int         errnum,
    const char *parm1,
    const char *parm2,
    const char *parm3,
    const char *parm4)
{
    static const char fn[] = "pdc_push_errmsg";
    const pdc_error_info *ei = get_error_info(pdc, errnum);

    pdc_pop_errmsg(pdc);
    pdc->pr->errnum = 0;

    make_errmsg(pdc, ei, parm1, parm2, parm3, parm4, pdc_false);

    pdc->pr->premsg = pdc_strdup_ext(pdc, pdc->pr->errbuf, 0, fn);

} /* pdc_push_errmsg */

void
pdc_set_errmsg(
    pdc_core *  pdc,
    int         errnum,
    const char *parm1,
    const char *parm2,
    const char *parm3,
    const char *parm4)
{
    if (errnum != 0)
    {
        const pdc_error_info *ei = get_error_info(pdc, errnum);

        make_errmsg(pdc, ei, parm1, parm2, parm3, parm4, pdc_false);
    }

    pdc->pr->errnum = errnum;

    if (errnum)
        pdc_logg_cond(pdc, 2, trc_warning,
                "[Reason for error message %d: \"%s\"]\n",
                pdc->pr->errnum, pdc->pr->errbuf);

} /* pdc_set_errmsg */

void
pdc_set_warnmsg(
    pdc_core *  pdc,
    int         errnum,
    const char *parm1,
    const char *parm2,
    const char *parm3,
    const char *parm4)
{
    char errbuf[PDC_ERRBUF_SIZE];

    strcpy(errbuf, pdc->pr->errbuf);

    if (errnum != -1)
    {
        const pdc_error_info *ei = get_error_info(pdc, errnum);

        make_errmsg(pdc, ei, parm1, parm2, parm3, parm4, pdc_false);
    }

    pdc_logg_cond(pdc, 1, trc_warning,
                "\n[Warning message %d: \"%s\"]\n",
                errnum, pdc->pr->errbuf);

    strcpy(pdc->pr->errbuf, errbuf);

} /* pdc_set_warnmsg */


void
pdc_error(
    pdc_core *	pdc,
    int		errnum,
    const char *parm1,
    const char *parm2,
    const char *parm3,
    const char *parm4)
{
    const char *logmsg = NULL;

    /* avoid recursive errors, but allow rethrow.
    */
    if (errnum != -1 && pdc->pr->in_error)
	return;

    pdc->pr->in_error = pdc_true;
    pdc->pr->x_thrown = pdc_true;

    if (errnum != -1)
    {
	const pdc_error_info *ei = get_error_info(pdc, errnum);

        make_errmsg(pdc, ei, parm1, parm2, parm3, parm4, pdc_true);
        pdc->pr->errnum = errnum;
    }

    if (pdc->pr->x_sp > pdc->pr->x_sp0)
    {
        if (pdc_logg_is_enabled(pdc, 2, trc_warning))
	    logmsg = "[Nested exception %d in %s]";
    }
    else
    {
	logmsg = "\n[Last exception %d in %s]";
    }

    if (logmsg != NULL)
    {
        pdc_logg(pdc, logmsg, pdc->pr->errnum,
	    (pdc->pr->errnum == 0 || !pdc->pr->apiname) ? "" : pdc->pr->apiname,
	    pdc->pr->x_sp0 + 1, pdc->pr->x_sp - pdc->pr->x_sp0);

        pdc_logg(pdc, "[\"%s\"]\n\n", pdc->pr->errbuf);
    }

    if (pdc->pr->x_sp == -1)
    {
	char errbuf[PDC_ERRBUF_SIZE];
        const char *apiname = pdc_get_apiname(pdc);
        const char *errmsg = pdc->pr->errbuf;

        if (strlen(apiname))
        {
            sprintf(errbuf, "[%d] %s: %s", pdc->pr->errnum, apiname, errmsg);
            errmsg = errbuf;
        }

        (*pdc->pr->errorhandler)(pdc->pr->opaque, PDF_UnknownError, errmsg);

	/*
	 * The error handler must never return. If it does, it is severely
	 * broken. We cannot remedy this, so we exit.
	 */
	 exit(99);

    }
    else
    {
        longjmp(pdc->pr->x_stack[pdc->pr->x_sp].jbuf.jbuf, 1);
    }

} /* pdc_error */

pdc_jmpbuf *
pdc_jbuf(pdc_core *pdc)
{
    static const char fn[] = "pdc_jbuf";

    pdc_logg_cond(pdc, 3, trc_api,
                  "[TRY to level %d]\n", pdc->pr->x_sp + 1);

    if (++pdc->pr->x_sp == pdc->pr->x_ssize)
    {
	pdc_xframe *aux;

#ifdef PDC_ALIGN16
        char *cp = (char *) (*pdc->pr->allocproc)(pdc->pr->opaque,
                        16 + 2 * pdc->pr->x_ssize * sizeof (pdc_xframe), fn);

	if (cp == (char *) 0)
	{
	    aux = (pdc_xframe *) 0;
	}
	else
	{
            /* remember the pointer in order to free it only after the memcpy
             * below, as pdc->pr->x_stack points into the memory allocated
             * to pdc->pr->x_alias
             */
            char *free_me_later = pdc->pr->x_alias;
            pdc->pr->x_alias = cp;
	    aux = (pdc_xframe *)
		(((unsigned long) cp + 16) & 0xFFFFFFFFFFFFFFF0);

            memcpy(aux, pdc->pr->x_stack,
                   pdc->pr->x_ssize * sizeof (pdc_xframe));
            pdc_free(pdc, free_me_later);
	}
#else
        aux = (pdc_xframe *) (*pdc->pr->reallocproc)(
                        pdc->pr->opaque, pdc->pr->x_stack,
                        2 * pdc->pr->x_ssize * sizeof (pdc_xframe), fn);
#endif

	if (aux == (pdc_xframe *) 0)
	{
            --pdc->pr->x_sp;
            pdc->pr->x_thrown = pdc_true;
            pdc->pr->in_error = pdc_true;

            pdc->pr->errnum = PDC_E_MEM_OUT;
            pdc->pr->apiname[0] = 0;
	    sprintf(pdc->pr->errbuf,
                    "Out of memory in TRY function (nesting level: %d)",
                    pdc->pr->x_sp + 1);

	    longjmp(pdc->pr->x_stack[pdc->pr->x_sp].jbuf.jbuf, 1);
	}

        pdc->pr->x_stack = aux;
        pdc->pr->x_ssize *= 2;
    }

    pdc->pr->x_thrown = pdc_false;
    return &pdc->pr->x_stack[pdc->pr->x_sp].jbuf;
} /* pdc_jbuf */

void
pdc_exit_try(pdc_core *pdc)
{
    pdc_logg_cond(pdc, 3, trc_api,
                  "[EXIT_TRY at level %d]\n", pdc->pr->x_sp);

    if (pdc->pr->x_sp == -1)
    {
        strcpy(pdc->pr->errbuf, "exception stack underflow");
        pdc->pr->errnum = PDC_E_INT_XSTACK;
        (*pdc->pr->errorhandler)(pdc->pr->opaque, PDF_UnknownError,
                                 pdc->pr->errbuf);
    }
    else
        --pdc->pr->x_sp;
} /* pdc_exit_try */

int
pdc_catch_intern(pdc_core *pdc)
{
    pdc_bool result;

    pdc_logg_cond(pdc, 3, trc_api,
                  "[CATCH intern at level %d]\n", pdc->pr->x_sp);

    if (pdc->pr->x_sp == -1)
    {
        strcpy(pdc->pr->errbuf, "exception stack underflow");
        pdc->pr->errnum = PDC_E_INT_XSTACK;
        (*pdc->pr->errorhandler)(pdc->pr->opaque, PDF_UnknownError,
                                 pdc->pr->errbuf);
    }
    else
        --pdc->pr->x_sp;

    result = pdc->pr->x_thrown;
    pdc->pr->in_error = pdc_false;
    pdc->pr->x_thrown = pdc_false;

    return result;
} /* pdc_catch_intern */

int
pdc_catch_extern(pdc_core *pdc)
{
    pdc_bool result;

    pdc_logg_cond(pdc, 3, trc_api,
                  "[CATCH at level %d]\n", pdc->pr->x_sp);

    if (pdc->pr->x_sp == -1)
    {
        strcpy(pdc->pr->errbuf, "exception stack underflow");
        pdc->pr->errnum = PDC_E_INT_XSTACK;
        (*pdc->pr->errorhandler)(pdc->pr->opaque, PDF_UnknownError,
                                 pdc->pr->errbuf);
    }
    else
        --pdc->pr->x_sp;

    result = pdc->pr->x_thrown;
    pdc->pr->x_thrown = pdc_false;

    return result;
} /* pdc_catch_extern */

void
pdc_rethrow(pdc_core *pdc)
{
    pdc_error(pdc, -1, 0, 0, 0, 0);
} /* pdc_rethrow */


/* this function should be called in the PDC_CATCH branch of
** a function before it returns -1.
*/
void
pdc_check_rethrow(pdc_core *pdc)
{
    if (pdc->pr->errnum == PDC_E_MEM_OUT)
	pdc_error(pdc, -1, 0, 0, 0, 0);
} /* pdc_check_rethrow */


int
pdc_get_errnum(pdc_core *pdc)
{
    return pdc->pr->errnum;
}

const char *
pdc_get_errmsg(pdc_core *pdc)
{
    return (pdc->pr->errnum == 0) ? "" : pdc->pr->errbuf;
}

const char *
pdc_get_apiname(pdc_core *pdc)
{
    return pdc->pr->apiname;
}

const char *
pdc_get_errpref(pdc_core *pdc)
{
    return pdc->pr->premsg;
}


#ifdef	PDC_DEBUG

/* --------------------------- debug hexdump  --------------------------- */
void
pdc_enable_hexdump(pdc_core *pdc)
{
    pdc->pr->hexdump = pdc_true;
}

void
pdc_disable_hexdump(pdc_core *pdc)
{
    pdc->pr->hexdump = pdc_false;
}

void
pdc_hexdump(pdc_core *pdc, const char *msg, const char *text, int tlen)
{
    if (pdc->pr->hexdump)
    {
	int i, k;

	if (tlen == 1)
	{
	    printf("%s: %02X '%c'\n", msg,
			(unsigned char) text[0],
			pdc_isprint(text[0]) ? text[0] : '.');
	}
	else
	{
	    printf("%s:\n", msg);

	    for (i = 0; i < tlen; i += 16)
	    {
		for (k = 0; k < 16; ++k)
		    if (i + k < tlen)
			printf("%02X ", (unsigned char) text[i + k]);
		    else
			printf("   ");

		printf(" ");
		for (k = 0; k < 16; ++k)
		    if (i + k < tlen)
		    {
			printf("%c",
			    pdc_isprint(text[i + k]) ? text[i + k] : '.');
		    }
		    else
			printf("   ");

		printf("\n");
	    }
	}
    }
}

#endif /* PDC_DEBUG */
