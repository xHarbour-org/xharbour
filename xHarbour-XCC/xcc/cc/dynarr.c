/****************************************************************************
 *                                                                          *
 * File    : dynarr.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Dynamic array management.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define LEAFSIZ     (sizeof(RAA) - sizeof(RAA_UNION) + sizeof(RAA_LEAF))
#define BRANCHSIZ   (sizeof(RAA) - sizeof(RAA_UNION) + sizeof(RAA_BRANCH))
#define LAYERSIZ(r) (((r)->layers == 0) ? RAA_BLKSIZE : RAA_LAYERSIZE)

/****************************************************************************
 *                                                                          *
 * Function: real_raa_init                                                  *
 *                                                                          *
 * Purpose : Random Access Array; initialization.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static RAA *real_raa_init(int layers)
{
    RAA *r;

    if (layers == 0)
    {
        r = my_alloc(LEAFSIZ);
        memset(r->u.l.data, 0, sizeof(r->u.l.data));
        r->layers = 0;
        r->stepsize = 1L;
    }
    else
    {
        r = my_alloc(BRANCHSIZ);
        memset(r->u.b.data, 0, sizeof(r->u.b.data));
        r->layers = layers;
        r->stepsize = RAA_BLKSIZE;

        while (--layers)
            r->stepsize *= RAA_LAYERSIZE;
    }

    return r;
}

/****************************************************************************
 *                                                                          *
 * Function: raa_init                                                       *
 *                                                                          *
 * Purpose : Random Access Array; global initialization.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

RAA *raa_init(void)
{
    return real_raa_init(0);
}

/****************************************************************************
 *                                                                          *
 * Function: raa_free                                                       *
 *                                                                          *
 * Purpose : Random Access Array; free a leaf or a branch.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void raa_free(RAA *r)
{
    if (r)
    {
        if (r->layers == 0)
        {
            my_free(r);
        }
        else
        {
            RAA **p;

            for (p = r->u.b.data; p - r->u.b.data < RAA_LAYERSIZE; p++)
                if (*p) raa_free(*p);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: raa_read                                                       *
 *                                                                          *
 * Purpose : Random Access Array; read value at given position.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

long raa_read(RAA *r, long posn)
{
    if (posn >= r->stepsize * LAYERSIZ(r))  /* bugfix GT -> GE 04-12-13 */
        return 0L;

    while (r->layers > 0)
    {
        ldiv_t l;

        l = ldiv(posn, r->stepsize);
        r = r->u.b.data[l.quot];
        posn = l.rem;

        if (!r)         /* better check this */
            return 0L;
    }

    return r->u.l.data[posn];
}

/****************************************************************************
 *                                                                          *
 * Function: raa_write                                                      *
 *                                                                          *
 * Purpose : Random Access Array; write value at given position.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

RAA *raa_write(RAA *r, long posn, long value)
{
    RAA *result;

    if (posn < 0)
        apperror(RCFATAL(ERROR_INTERNAL), "raa_write()");

    while (r->stepsize * LAYERSIZ(r) <= posn)  /* bugfix LT -> LE 04-12-13 */
    {
        /*
         * Must go up a layer.
         */
        RAA *s;

        s = my_alloc(BRANCHSIZ);
        memset(s->u.b.data, 0, sizeof(r->u.b.data));
        s->layers = r->layers + 1;
        s->stepsize = RAA_LAYERSIZE * r->stepsize;
        s->u.b.data[0] = r;
        r = s;
    }

    result = r;

    while (r->layers > 0)
    {
        ldiv_t l;
        RAA **s;

        l = ldiv(posn, r->stepsize);
        s = &r->u.b.data[l.quot];
        if (!*s) *s = real_raa_init(r->layers - 1);
        r = *s;
        posn = l.rem;
    }

    r->u.l.data[posn] = value;

    return result;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_init                                                       *
 *                                                                          *
 * Purpose : Sequential Access Array; initialization.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define SAA_MAXLEN 8192

SAA *saa_init(long elem_len)
{
    SAA *s;

    if (elem_len > SAA_MAXLEN)
        apperror(RCFATAL(ERROR_INTERNAL), "saa_init()");

    s = my_alloc(sizeof(SAA));
    s->posn = s->start = 0L;
    s->elem_len = elem_len;
    s->length = SAA_MAXLEN - (SAA_MAXLEN % elem_len);
    s->data = my_alloc(s->length);
    s->next = NULL;
    s->end = s;

    return s;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_free                                                       *
 *                                                                          *
 * Purpose : Sequential Access Array; free the given array.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_free(SAA *s)
{
    while (s)
    {
        SAA *t = s->next;
        my_free(s->data);
        my_free(s);
        s = t;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: saa_wstruct                                                    *
 *                                                                          *
 * Purpose : Sequential Access Array; reserve a block.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *saa_wstruct(SAA *s)
{
    void *p;

    if (s->end->length - s->end->posn < s->elem_len)
    {
        s->end->next = my_alloc(sizeof(SAA));
        s->end->next->start = s->end->start + s->end->posn;
        s->end = s->end->next;
        s->end->length = s->length;
        s->end->next = NULL;
        s->end->posn = 0L;
        s->end->data = my_alloc(s->length);
    }

    p = s->end->data + s->end->posn;
    s->end->posn += s->elem_len;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_wbytes                                                     *
 *                                                                          *
 * Purpose : Sequential Access Array; write the given data.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_wbytes(SAA *s, const void *data, long len)
{
    const char *d = data;

    while (len > 0)
    {
        long l = s->end->length - s->end->posn;

        if (l > len) l = len;

        if (l > 0)
        {
            if (d)
            {
                memcpy(s->end->data + s->end->posn, d, l);
                d += l;
            }
            else
            {
                memset(s->end->data + s->end->posn, 0, l);
            }

            s->end->posn += l;
            len -= l;
        }

        if (len > 0)
        {
            s->end->next = my_alloc(sizeof(SAA));
            s->end->next->start = s->end->start + s->end->posn;
            s->end = s->end->next;
            s->end->length = s->length;
            s->end->next = NULL;
            s->end->posn = 0L;
            s->end->data = my_alloc(s->length);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: saa_rewind                                                     *
 *                                                                          *
 * Purpose : Rewind pointers to the beginning of the array.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_rewind(SAA *s)
{
    s->rptr = s;
    s->rpos = 0L;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_rstruct                                                    *
 *                                                                          *
 * Purpose : Sequential Access Array; read a block.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *saa_rstruct(SAA *s)
{
    void *p;

    if (!s->rptr)
        return NULL;

    if (s->rptr->posn - s->rpos < s->elem_len)
    {
        s->rptr = s->rptr->next;
        if (!s->rptr)
            return NULL;               /* end of array */
        s->rpos = 0L;
    }

    p = s->rptr->data + s->rpos;
    s->rpos += s->elem_len;
    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_rbytes                                                     *
 *                                                                          *
 * Purpose : Sequential Access Array; read one element.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *saa_rbytes(SAA *s, long *len)
{
    void *p;

    if (!s->rptr)
        return NULL;

    p = s->rptr->data + s->rpos;
    *len = s->rptr->posn - s->rpos;
    s->rptr = s->rptr->next;
    s->rpos = 0L;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_rnbytes                                                    *
 *                                                                          *
 * Purpose : Sequential Access Array; read data from current position.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_rnbytes(SAA *s, void *data, long len)
{
    char *d = data;

    while (len > 0)
    {
        long l;

        if (!s->rptr)
            return;

        l = s->rptr->posn - s->rpos;
        if (l > len) l = len;
        if (l > 0)
        {
            memcpy(d, s->rptr->data + s->rpos, l);
            d += l;
            s->rpos += l;
            len -= l;
        }
        if (len > 0)
        {
            s->rptr = s->rptr->next;
            s->rpos = 0L;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: saa_fread                                                      *
 *                                                                          *
 * Purpose : Sequential Access Array; read data from given position.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_fread(SAA *s, long posn, void *data, long len)
{
    SAA *p;
    long pos;
    char *cdata = data;

    if (!s->rptr || posn < s->rptr->start)
        saa_rewind(s);

    p = s->rptr;
    while (posn >= p->start + p->posn)
    {
        p = p->next;
        if (!p) return;  /* what else can we do?! */
    }

    pos = posn - p->start;
    while (len)
    {
        long l = p->posn - pos;
        if (l > len) l = len;
        memcpy(cdata, p->data+pos, l);
        len -= l;
        cdata += l;
        p = p->next;
        if (!p) return;
        pos = 0L;
    }
    s->rptr = p;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_fwrite                                                     *
 *                                                                          *
 * Purpose : Sequential Access Array; write data to given position.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_fwrite(SAA *s, long posn, const void *data, long len)
{
    SAA *p;
    long pos;
    const char *cdata = data;

    if (!s->rptr || posn < s->rptr->start)
        saa_rewind(s);

    p = s->rptr;
    while (posn >= p->start + p->posn)
    {
        p = p->next;
        if (!p) return;  /* what else can we do?! */
    }

    pos = posn - p->start;
    while (len)
    {
        long l = p->posn - pos;
        if (l > len) l = len;
        memcpy(p->data + pos, cdata, l);
        len -= l;
        cdata += l;
        p = p->next;
        if (!p) return;
        pos = 0L;
    }
    s->rptr = p;
}

/****************************************************************************
 *                                                                          *
 * Function: saa_fpwrite                                                    *
 *                                                                          *
 * Purpose : Sequential Access Array; write a whole array to file.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void saa_fpwrite(SAA *s, FILE *fp)
{
    char *data;
    long len;

    saa_rewind(s);

    while ((data = saa_rbytes(s, &len)) != NULL)
        fwrite(data, 1, len, fp);
}

