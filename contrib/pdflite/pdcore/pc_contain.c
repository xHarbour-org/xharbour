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
 * PDFlib generic container classes
 *
 */

#include "config.h"
#include "pc_util.h"
#include "pc_contain.h"


/**************************** avl tree class ****************************/

typedef struct avl_node_s avl_node;

struct avl_node_s
{
    const char *name;
    int		balance;
    avl_node *	left;
    avl_node *	right;
};

/*************************** bit vector class ***************************/

struct pdc_bvtr_s
{
    pdc_core *	pdc;

    char **	ctab;		/* chunk table				*/
    int		ctab_size;	/* current # of slots			*/
    int		ctab_incr;
    int		chunk_size;	/* # of bytes per chunk			*/
    int		size;		/* current # of bytes total		*/
    char	init_char;	/* 0x00 or 0xFF				*/
};


static const pdc_bvtr_parms bvtr_dflt_parms =
{
    0,				/* initial number of bits		*/
    pdc_false,			/* initial bit value			*/
    1000,			/* number of bytes per chunk		*/
    10				/* chunk table increment		*/
};


void pdc_bvtr_dflt_parms(pdc_bvtr_parms *vp)
{
    *vp = bvtr_dflt_parms;
}


pdc_bvtr *pdc_bvtr_new(pdc_core *pdc, const pdc_bvtr_parms *parms)
{
    static const char fn[] = "pdc_bvtr_new";

    pdc_bvtr *v = (pdc_bvtr *) pdc_malloc(pdc, sizeof (pdc_bvtr), fn);

    if (!parms)
	parms = &bvtr_dflt_parms;

    v->pdc = pdc;

    v->ctab = (char **) 0;
    v->ctab_size = 0;
    v->ctab_incr = parms->ctab_incr;
    v->chunk_size = parms->chunk_size;
    v->size = 0;
    v->init_char = parms->init_value ? 0xFF : 0x00;

    if (parms->init_n_bits != 0)
    {
	PDC_TRY (pdc)
	{
	    pdc_bvtr_resize(v, parms->init_n_bits);
	}
	PDC_CATCH (pdc)
	{
	    pdc_bvtr_delete(v);
	    PDC_RETHROW(pdc);
	}
    }

    return v;
} /* pdc_bvtr_new */


void pdc_bvtr_delete(pdc_bvtr *v)
{
    int i;

    for (i = 0; i < v->ctab_size && v->ctab[i]; ++i)
    {
	pdc_free(v->pdc, v->ctab[i]);
    }

    if (v->ctab)
	pdc_free(v->pdc, v->ctab);

    pdc_free(v->pdc, v);
} /* pdc_bvtr_delete */


void
pdc_bvtr_resize(pdc_bvtr *v, int n_bits)
{
    static const char fn[] = "pdc_bvtr_resize";

    int cs = v->chunk_size;
    int new_size = (n_bits + 7) / 8;
    int new_ctsize = (new_size + cs - 1) / cs;	/* TODO: ctab_incr? */
    int i;

    PDC_ASSERT(v->pdc, 0 <= n_bits);

    if (new_size < v->size)
    {
	if (new_ctsize < v->ctab_size)
	{
	    for (i = new_ctsize; i < v->ctab_size; ++i)
	    {
		pdc_free(v->pdc, v->ctab[i]);
	    }
	}

	v->ctab_size = new_ctsize;
	v->size = new_ctsize * cs;
    }
    else if (new_size > v->size)
    {
	v->ctab = (char **) pdc_realloc(v->pdc, v->ctab,
				(size_t) (new_ctsize * sizeof (char *)), fn);

	for (i = v->size / cs; i < new_ctsize; ++i)
	{
	    int k;

	    v->ctab[i] = (char *) pdc_malloc(v->pdc, (size_t) cs, fn);

	    for (k = 0; k < cs; ++k)
	    {
		v->ctab[i][k] = v->init_char;
	    }
	}

	v->ctab_size = new_ctsize;
	v->size = new_ctsize * cs;
    }
} /* pdc_vtr_resize */


pdc_bool
pdc_bvtr_getbit(const pdc_bvtr *v, int n)
{
    static const char fn[] = "pdc_bvtr_getbit";

    int cs = v->chunk_size;
    int idx = n / 8;
    int bit = 1 << (n % 8);

    if (idx < 0 || v->size <= idx)
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", n), fn, 0, 0);

    return (v->ctab[idx / cs][idx % cs] & bit) != 0;
} /* pdc_bvtr_getbit */


void pdc_bvtr_setbit(const pdc_bvtr *v, int n)
{
    static const char fn[] = "pdc_bvtr_setbit";

    int cs = v->chunk_size;
    int idx = n / 8;
    int bit = 1 << (n % 8);

    if (idx < 0 || v->size <= idx)
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", n), fn, 0, 0);

    v->ctab[idx / cs][idx % cs] |= bit;
} /* pdc_bvtr_setbit */


void pdc_bvtr_clrbit(const pdc_bvtr *v, int n)
{
    static const char fn[] = "pdc_bvtr_clrbit";

    int cs = v->chunk_size;
    int idx = n / 8;
    int bit = 1 << (n % 8);

    if (idx < 0 || v->size <= idx)
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", n), fn, 0, 0);

    v->ctab[idx / cs][idx % cs] &= ~bit;
} /* pdc_bvtr_clrbit */


/*********************** stack type vector class ************************/

struct pdc_vtr_s
{
    pdc_core *	pdc;

    pdc_ced	ced;		/* container entry descriptor		*/
    void *	context;	/* client context			*/

    char **	ctab;		/* chunk table				*/
    int		ctab_size;	/* current # of slots			*/
    int		ctab_incr;
    int		chunk_size;	/* # of items per chunk			*/
    int		size;		/* current # of items total		*/
};


static const pdc_vtr_parms vtr_dflt_parms =
{
    0,				/* init_size				*/
    100,			/* chunk_size				*/
    10,				/* ctab_incr				*/
};

void
pdc_vtr_dflt_parms(pdc_vtr_parms *vp)
{
    *vp = vtr_dflt_parms;
}


static void
pdc_vtr_grow_ctab(pdc_vtr *v, int new_size)
{
    static const char fn[] = "pdc_vtr_grow_ctab";

    int i;

    v->ctab = (char **)
	pdc_realloc(v->pdc, v->ctab, (size_t) (new_size * sizeof (char *)), fn);

    for (i = v->ctab_size; i < new_size; ++i)
	v->ctab[i] = (char *) 0;

    v->ctab_size = new_size;
} /* pdc_vtr_grow_ctab */


pdc_vtr *
pdc_vtr_new(
    pdc_core *pdc,
    const pdc_ced *ced,
    void *context,
    const pdc_vtr_parms *parms)
{
    static const char fn[] = "pdc_vtr_new";

    pdc_vtr *v = (pdc_vtr *) pdc_malloc(pdc, sizeof (pdc_vtr), fn);

    if (!parms)
	parms = &vtr_dflt_parms;

    v->pdc = pdc;
    v->ced = *ced;
    v->context = context ? context : pdc;

    v->ctab = (char **) 0;
    v->ctab_size = 0;
    v->ctab_incr = parms->ctab_incr;
    v->chunk_size = parms->chunk_size;
    v->size = 0;

    if (parms->init_size != 0)
    {
	PDC_TRY (pdc)
	{
	    pdc_vtr_resize(v, parms->init_size);
	}
	PDC_CATCH (pdc)
	{
	    pdc_vtr_delete(v);
	    PDC_RETHROW(pdc);
	}
    }

    return v;
} /* pdc_vtr_new */


void
pdc_vtr_delete(pdc_vtr *v)
{
    int cs = v->chunk_size;
    int i;

    if (v->size != 0 && v->ced.release)
    {
	for (i = 0; i < v->size; ++i)
	{
	    v->ced.release(v->context, (void *)
		&v->ctab[i / cs][(i % cs) * v->ced.size]);
	}
    }

    for (i = 0; i < v->ctab_size && v->ctab[i] != (char *) 0; ++i)
    {
	pdc_free(v->pdc, v->ctab[i]);
    }

    if (v->ctab)
	pdc_free(v->pdc, v->ctab);

    pdc_free(v->pdc, v);
} /* pdc_vtr_delete */


int
pdc_vtr_size(const pdc_vtr *v)
{
    return (int) v->size;
} /* pdc_vtr_size */


void
pdc_vtr_resize(pdc_vtr *v, int new_size)
{
    static const char fn[] = "pdc_vtr_resize";

    int cs = v->chunk_size;

    PDC_ASSERT(v->pdc, 0 <= new_size);

    if (new_size < v->size)
    {
	if (!v->ced.release)
	{
	    v->size = new_size;
	}
	else
	{
	    do
	    {
		--v->size;

		v->ced.release(v->context, (void *)
		    &v->ctab[v->size / cs][(v->size % cs) * v->ced.size]);
	    } while (new_size < v->size);
	}

	/* TODO: free chunks if possible? */
    }
    else if (new_size > v->size)
    {
	int curr_slot = v->size / cs;
	int new_ctsize = (new_size + cs - 1) / cs;
	int i;

	if (v->ctab_size < new_ctsize)
	    pdc_vtr_grow_ctab(v, new_ctsize);

	for (i = curr_slot; i < new_ctsize; ++i)
	{
	    if (v->ctab[i] == (char *) 0)
	    {
		v->ctab[i] = (char *)
		    pdc_malloc(v->pdc, (size_t) (cs * v->ced.size), fn);
	    }
	}

	if (v->ced.reclaim)
	{
	    for (i = v->size; i < new_size; ++i)
	    {
		v->ced.reclaim((void *) &v->ctab[i/cs][(i%cs) * v->ced.size]);
	    }
	}

	v->size = new_size;
    }
} /* pdc_vtr_resize */


void *
pdc__vtr_at(const pdc_vtr *v, int idx)
{
    static const char fn[] = "pdc__vtr_at";

    int cs = v->chunk_size;

    if (idx < 0 || v->size <= idx)
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", idx), fn, 0, 0);

    return (void *) (&v->ctab[idx / cs][(idx % cs) * v->ced.size]);
} /* pdc__vtr_at */


#if 0
const void *
pdc__vtr_at_c(const pdc_vtr *v, int idx)
{
    static const char fn[] = "pdc__vtr_at_c";

    int cs = v->chunk_size;

    if (idx < 0 || v->size <= idx)
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", idx), fn, 0, 0);

    return (const void *) (&v->ctab[idx / cs][(idx % cs) * v->ced.size]);
} /* pdc__vtr_at_c */
#endif


void *
pdc__vtr_top(const pdc_vtr *v)
{
    int cs = v->chunk_size;
    int idx;

    if (v->size == 0)
	return (void *) 0;

    idx = v->size - 1;
    return (void *) (&v->ctab[idx / cs][(idx % cs) * v->ced.size]);
} /* pdc__vtr_top */


#if 0
const void *
pdc__vtr_top_c(const pdc_vtr *v)
{
    int cs = v->chunk_size;
    int idx;

    if (v->size == 0)
	return (void *) 0;

    idx = v->size - 1;
    return (const void *) (&v->ctab[idx / cs][(idx % cs) * v->ced.size]);
} /* pdc__vtr_top_c */
#endif


void *
pdc__vtr_push(pdc_vtr *v)
{
    static char fn[] = "pdc__vtr_push";

    int cs = v->chunk_size;
    int idx = v->size;
    int slot = idx / cs;
    char *target;

    if (v->ctab_size <= slot)
	pdc_vtr_grow_ctab(v, v->ctab_size + v->ctab_incr);

    if (v->ctab[slot] == (char *) 0)
    {
	v->ctab[slot] = (char *)
	    pdc_malloc(v->pdc, (size_t) (cs * v->ced.size), fn);
    }

    ++v->size;
    target = &v->ctab[slot][(idx % cs) * v->ced.size];

    if (v->ced.reclaim)
    {
	v->ced.reclaim((void *) target);
    }

    return (void *) target;
} /* pdc__vtr_push */


void
pdc_vtr_pop(pdc_vtr *v)
{
    static char fn[] = "pdc_vtr_pop";

    int cs = v->chunk_size;

    if (v->size == 0)
	pdc_error(v->pdc, PDC_E_INT_STACK_UNDER, fn, 0, 0, 0);

    --v->size;

    if (v->ced.release)
    {
	v->ced.release(v->context, (void *)
	    &v->ctab[v->size / cs][(v->size % cs) * v->ced.size]);
    }
} /* pdc_vtr_pop */


/************************ heap type vector class ************************/

typedef struct pdc_link_s	pdc_link;
typedef struct pdc_chunk_s	pdc_chunk;

struct pdc_link_s		/* for doubly linked free items list	*/
{
    int		idx;
    pdc_link *	prev;		/* previous item in free list		*/
    pdc_link *	next;		/* next item in free list		*/
};


struct pdc_chunk_s
{
    char *	data;		/* the items in this chunk		*/
    int		n_items;	/* number of used items in this chunk	*/

    pdc_chunk *	next;		/* next chunk in free list		*/
};


struct pdc_hvtr_s
{
    pdc_core *	pdc;

    pdc_ced	ced;		/* container entry descriptor		*/
    void *	context;	/* client context			*/

    pdc_chunk *	ctab;		/* chunk table				*/
    int		ctab_size;	/* current # of slots			*/
    int		ctab_incr;
    int		chunk_size;	/* # of items per chunk			*/
    int		size;		/* current # of items total		*/

    pdc_link *	free_items;	/* first item in free items list	*/
    pdc_link	end_items;	/* sentinel				*/
    pdc_chunk *	free_chunks;	/* first chunk in free chunks list	*/
    pdc_chunk	end_chunks;	/* sentinel				*/

    pdc_bvtr *	free_mask;	/* bit mask of free items		*/
};


static const pdc_hvtr_parms hvtr_dflt_parms =
{
    100,			/* chunk_size				*/
    10,				/* ctab_incr				*/
};

void
pdc_hvtr_dflt_parms(pdc_hvtr_parms *vp)
{
    *vp = hvtr_dflt_parms;
}


pdc_hvtr *
pdc_hvtr_new(
    pdc_core *pdc,
    const pdc_ced *ced,
    void *context,
    const pdc_hvtr_parms *parms)
{
    static const char fn[] = "pdc_hvtr_new";

    pdc_hvtr *v = (pdc_hvtr *) pdc_malloc(pdc, sizeof (pdc_hvtr), fn);

    if (!parms)
	parms = &hvtr_dflt_parms;

    v->pdc = pdc;
    v->ced = *ced;
    v->context = context ? context : pdc;

    if (v->ced.size < sizeof (pdc_link))
    {
	v->ced.size = sizeof (pdc_link);
    }

    v->ctab = (pdc_chunk *) 0;
    v->ctab_size = 0;
    v->ctab_incr = parms->ctab_incr;
    v->chunk_size = parms->chunk_size;
    v->size = 0;

    v->free_items = &v->end_items;
    v->end_items.next = v->end_items.prev = &v->end_items;
    v->free_chunks = &v->end_chunks;
    v->free_mask = 0;

    PDC_TRY (pdc)
    {
	pdc_bvtr_parms bvp;

	pdc_bvtr_dflt_parms(&bvp);
	bvp.init_value = pdc_true;
	v->free_mask = pdc_bvtr_new(pdc, &bvp);
    }
    PDC_CATCH (pdc)
    {
	pdc_hvtr_delete(v);
	PDC_RETHROW(pdc);
    }

    return v;
} /* pdc_hvtr_new */


void
pdc_hvtr_delete(pdc_hvtr *v)
{
    int cs = v->chunk_size;
    int i;

    if (v->size != 0 && v->ced.release)
    {
	for (i = 0; i < v->size; ++i)
	{
	    if (!pdc_bvtr_getbit(v->free_mask, i))
	    {
		v->ced.release(v->context, (void *)
		    &v->ctab[i / cs].data[(i % cs) * v->ced.size]);
	    }
	}
    }

    if (v->ctab)
    {
	for (i = 0; i < v->ctab_size && v->ctab[i].data != (char *) 0; ++i)
	{
	    pdc_free(v->pdc, v->ctab[i].data);
	}

	pdc_free(v->pdc, v->ctab);
    }

    if (v->free_mask)
    {
	pdc_bvtr_delete(v->free_mask);
    }

    pdc_free(v->pdc, v);
} /* pdc_hvtr_delete */


void
pdc_hvtr_release_item(pdc_hvtr *v, int idx)
{
    static const char fn[] = "pdc_hvtr_release_item";

    const int	cs = v->chunk_size;
    pdc_chunk *	chunk = &v->ctab[idx / cs];
    void *	item;
    pdc_link *	link;

    if (idx < 0 || v->size <= idx || pdc_bvtr_getbit(v->free_mask, idx))
    {
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", idx), fn, 0, 0);
    }

    item = &chunk->data[(idx % cs) * v->ced.size];

    if (v->ced.release)
    {
	v->ced.release(v->context, item);
    }

    pdc_bvtr_setbit(v->free_mask, idx);

    link = (pdc_link *) item;
    link->idx = idx;
    link->next = v->free_items;
    link->prev = &v->end_items;
    link->next->prev = link->prev->next = link;
    v->free_items = link;

    if (--chunk->n_items == 0)
    {
	for (idx = 0; idx < cs; ++idx)
	{
	    link = (pdc_link *) &chunk->data[idx * v->ced.size];

	    link->prev->next = link->next;
	    link->next->prev = link->prev;
	}

	pdc_free(v->pdc, chunk->data);
	chunk->data = 0;
	chunk->next = v->free_chunks;
	v->free_chunks = chunk;
    }
} /* pdc_hvtr_release_item */


int
pdc_hvtr_reclaim_item(pdc_hvtr *v)
{
    static const char fn[] = "pdc_hvtr_reclaim_item";

    pdc_link *	new_item;
    int		idx;

    if (v->free_items != &v->end_items)
    {
	new_item = v->free_items;
	new_item->prev->next = new_item->next;
	new_item->next->prev = new_item->prev;
	v->free_items = new_item->next;
    }
    else
    {
	/* install new chunk.
	*/
	const int	cs = v->chunk_size;
	const int	es = (const int) v->ced.size;
	pdc_chunk *	new_chunk;
	pdc_link *	link;
	int		base;

	if (v->free_chunks != &v->end_chunks)
	{
	    new_chunk = v->free_chunks;
	    v->free_chunks = new_chunk->next;
	}
	else
	{
	    int new_size = v->ctab_size + v->ctab_incr;

	    v->ctab = (pdc_chunk *) pdc_realloc(v->pdc, v->ctab,
				(size_t) (new_size * sizeof (pdc_chunk)), fn);

	    for (idx = v->ctab_size; idx < new_size; ++idx)
	    {
		v->ctab[idx].data = (char *) 0;
		v->ctab[idx].n_items = 0;
		v->ctab[idx].next = &v->ctab[idx + 1];
	    }

	    v->ctab[new_size - 1].next = &v->end_chunks;
	    v->free_chunks = &v->ctab[v->ctab_size + 1];
	    new_chunk = &v->ctab[v->ctab_size];
	    v->ctab_size = new_size;
	    v->size += cs * v->ctab_incr;
	    pdc_bvtr_resize(v->free_mask, v->size);
	}

	new_chunk->data = (char *)pdc_malloc(v->pdc, cs * es, fn);
	base = ( int ) ( cs * (new_chunk - &v->ctab[0]) );

	for (idx = 1; idx < cs; ++idx)
	{
	    link = (pdc_link *) &new_chunk->data[idx * es];

	    link->idx = base + idx;
	    link->prev = (pdc_link *) &new_chunk->data[(idx - 1) * es];
	    link->next = (pdc_link *) &new_chunk->data[(idx + 1) * es];
	}

	/* end of new chain:
	*/
	link = (pdc_link *) &new_chunk->data[(cs - 1) * es];
	link->next = v->free_items;
	link->next->prev = link;

	/* start of new chain:
	*/
	link = (pdc_link *) &new_chunk->data[1 * es];
	link->prev = &v->end_items;
	v->free_items = v->end_items.next = link;

	new_item = (pdc_link *) &new_chunk->data[0];
	new_item->idx = base;
    }

    idx = new_item->idx;
    pdc_bvtr_clrbit(v->free_mask, idx);

    if (v->ced.reclaim)
    {
	v->ced.reclaim((void *) new_item);
    }

    return idx;
} /* pdc_hvtr_reclaim_item */


pdc_bool
pdc_hvtr_check_idx(const pdc_hvtr *v, int idx)
{
    return 0 <= idx && idx < v->size && !pdc_bvtr_getbit(v->free_mask, idx);
} /* pdc__hvtr_at */


void *
pdc__hvtr_at(const pdc_hvtr *v, int idx)
{
    static const char fn[] = "pdc__hvtr_at";

    int cs = v->chunk_size;

    if (idx < 0 || v->size <= idx || pdc_bvtr_getbit(v->free_mask, idx))
    {
	pdc_error(v->pdc, PDC_E_INT_ARRIDX,
	    pdc_errprintf(v->pdc, "%d", idx), fn, 0, 0);
    }

    return (void *) &v->ctab[idx / cs].data[(idx % cs) * v->ced.size];
} /* pdc__hvtr_at */
