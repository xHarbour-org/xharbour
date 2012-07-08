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
 * Various geometry routines
 *
 */

#include "pc_util.h"
#include "pc_geom.h"


/* ---------------------- matrix functions ----------------------------- */

pdc_bool
pdc_is_identity_matrix(pdc_matrix *m)
{
    return PDC_FLOAT_ISNULL(m->a - 1) &&
           PDC_FLOAT_ISNULL(m->b) &&
           PDC_FLOAT_ISNULL(m->c) &&
           PDC_FLOAT_ISNULL(m->d - 1) &&
           PDC_FLOAT_ISNULL(m->e) &&
           PDC_FLOAT_ISNULL(m->f);
}

/* identity matrix */
void
pdc_identity_matrix(pdc_matrix *M)
{
    M->a = 1;
    M->b = 0;
    M->c = 0;
    M->d = 1;
    M->e = 0;
    M->f = 0;
}

/* translation matrix */
void
pdc_translation_matrix(pdc_scalar tx, pdc_scalar ty, pdc_matrix *M)
{
    M->a = 1;
    M->b = 0;
    M->c = 0;
    M->d = 1;
    M->e = tx;
    M->f = ty;
}

/* scale matrix */
void
pdc_scale_matrix(pdc_scalar sx, pdc_scalar sy, pdc_matrix *M)
{
    M->a = sx;
    M->b = 0;
    M->c = 0;
    M->d = sy;
    M->e = 0;
    M->f = 0;
}

/* rotation matrix */
void
pdc_rotation_matrix(pdc_scalar alpha, pdc_matrix *M)
{
    pdc_scalar phi, c, s;

    phi = alpha * PDC_DEG2RAD;
    c = cos(phi);
    s = sin(phi);

    M->a = c;
    M->b = s;
    M->c = -s;
    M->d = c;
    M->e = 0;
    M->f = 0;
}

/* skew matrix */
void
pdc_skew_matrix(pdc_scalar alpha, pdc_scalar beta, pdc_matrix *M)
{
    M->a = 1;
    M->b = tan(alpha * PDC_DEG2RAD);
    M->c = tan(beta * PDC_DEG2RAD);
    M->d = 1;
    M->e = 0;
    M->f = 0;
}

/* N = M * N */
void
pdc_multiply_matrix(const pdc_matrix *M, pdc_matrix *N)
{
    pdc_matrix result;

    result.a = M->a * N->a + M->b * N->c;
    result.b = M->a * N->b + M->b * N->d;
    result.c = M->c * N->a + M->d * N->c;
    result.d = M->c * N->b + M->d * N->d;

    result.e = M->e * N->a + M->f * N->c + N->e;
    result.f = M->e * N->b + M->f * N->d + N->f;

    *N = result;
}

/* L = M * N */
void
pdc_multiply_matrix3(pdc_matrix *L, const pdc_matrix *M, const pdc_matrix *N)
{
    L->a = M->a * N->a + M->b * N->c;
    L->b = M->a * N->b + M->b * N->d;
    L->c = M->c * N->a + M->d * N->c;
    L->d = M->c * N->b + M->d * N->d;
    L->e = M->e * N->a + M->f * N->c + N->e;
    L->f = M->e * N->b + M->f * N->d + N->f;
}

/* M = [a b c d e f] * M; */
void
pdc_multiply_6s_matrix(pdc_matrix *M, pdc_scalar a, pdc_scalar b, pdc_scalar c,
                                      pdc_scalar d, pdc_scalar e, pdc_scalar f)
{
    pdc_matrix result;

    result.a = a * M->a + b * M->c;
    result.b = a * M->b + b * M->d;
    result.c = c * M->a + d * M->c;
    result.d = c * M->b + d * M->d;

    result.e = e * M->a + f * M->c + M->e;
    result.f = e * M->b + f * M->d + M->f;

    *M = result;
}


/* invert M and store the result in N */
void
pdc_invert_matrix(pdc_core *pdc, pdc_matrix *N, pdc_matrix *M)
{
    pdc_scalar det = M->a * M->d - M->b * M->c;

    if (fabs(det) < PDC_SMALLREAL * PDC_SMALLREAL)
        pdc_error(pdc, PDC_E_INT_INVMATRIX,
            pdc_errprintf(pdc, "%f %f %f %f %f %f",
            M->a, M->b, M->c, M->d, M->e, M->f),
            0, 0, 0);

    N->a = M->d/det;
    N->b = -M->b/det;
    N->c = -M->c/det;
    N->d = M->a/det;
    N->e = -(M->e * N->a + M->f * N->c);
    N->f = -(M->e * N->b + M->f * N->d);
}

/* debug print */
void
pdc_print_matrix(const char *name, const pdc_matrix *M)
{
    printf("%s: a=%g, b=%g, c=%g, d=%g, e=%g, f=%g\n",
           name, M->a, M->b, M->c, M->d, M->e, M->f);
}

/* transform scalar */
pdc_scalar
pdc_transform_scalar(const pdc_matrix *M, pdc_scalar s)
{
    pdc_scalar det = M->a * M->d - M->b * M->c;

    return sqrt(fabs(det)) * s;
}

/* transform point */
void
pdc_transform_point(const pdc_matrix *M, pdc_scalar x, pdc_scalar y,
                    pdc_scalar *tx, pdc_scalar *ty)
{
    *tx = M->a * x + M->c * y + M->e;
    *ty = M->b * x + M->d * y + M->f;
}

/* transform vector */
void
pdc_transform_vector(const pdc_matrix *M, pdc_vector *v, pdc_vector *tv)
{
    pdc_scalar tx = M->a * v->x + M->c * v->y + M->e;
    pdc_scalar ty = M->b * v->x + M->d * v->y + M->f;
    if (tv)
    {
        tv->x = tx;
        tv->y = ty;
    }
    else
    {
        v->x = tx;
        v->y = ty;
    }
}

/* transform relative vector */
void
pdc_transform_rvector(const pdc_matrix *M, pdc_vector *v, pdc_vector *tv)
{
    pdc_scalar tx = M->a * v->x + M->c * v->y;
    pdc_scalar ty = M->b * v->x + M->d * v->y;
    if (tv)
    {
        tv->x = tx;
        tv->y = ty;
    }
    else
    {
        v->x = tx;
        v->y = ty;
    }
}

/* get length of vector */
pdc_scalar
pdc_get_vector_length(pdc_vector *start, pdc_vector *end)
{
    pdc_scalar dx = end->x - start->x;
    pdc_scalar dy = end->y - start->y;

    return sqrt(dx * dx + dy * dy);
}


/* ---------------------- utility functions ----------------------------- */

void
pdc_place_element(pdc_fitmethod method, pdc_scalar minfscale,
                  const pdc_box *fitbox, const pdc_vector *fitrelpos,
                  const pdc_vector *elemsize, const pdc_vector *elemrelpos,
                  pdc_box *elembox, pdc_vector *scale)
{
    pdc_vector refpos;
    pdc_scalar width, height, det, fscale = 1.0;
    pdc_bool xscaling = pdc_false;

    /* reference position in fitbox */
    width = fitbox->ur.x - fitbox->ll.x;
    height = fitbox->ur.y - fitbox->ll.y;
    refpos.x = fitbox->ll.x + fitrelpos->x * width;
    refpos.y = fitbox->ll.y + fitrelpos->y * height;

    /* first check */
    switch (method)
    {
        case pdc_entire:
        case pdc_slice:
        case pdc_meet:
        case pdc_tauto:
        if (fabs(width) > PDC_FLOAT_PREC && fabs(height) > PDC_FLOAT_PREC)
        {
            if (method != pdc_entire)
            {
                det = elemsize->x * height - elemsize->y * width;
                xscaling = (method == pdc_slice && det <= 0) ||
                            ((method == pdc_meet || method == pdc_tauto) &&
                             det > 0) ? pdc_true : pdc_false;
                if (xscaling)
                    fscale = width / elemsize->x;
                else
                    fscale = height / elemsize->y;
            }

            if (method == pdc_tauto)
            {
                if(fscale >= 1.0)
                {
                    method = pdc_nofit;
                }
                else if (fscale < minfscale)
                {
                    method = pdc_meet;
                }
            }
        }
        else
        {
            method = pdc_nofit;
        }
        break;

        default:
        break;
    }

    /* calculation */
    switch (method)
    {
        /* entire box is covered by entire element */
        case pdc_entire:
        *elembox = *fitbox;
        scale->x = width / elemsize->x;
        scale->y = height / elemsize->y;
        return;

        /* fit into and preserve aspect ratio */
        case pdc_slice:
        case pdc_meet:
        if (xscaling)
            height = fscale * elemsize->y;
        else
            width = fscale * elemsize->x;
        scale->x = fscale;
        scale->y = fscale;
        break;

        /* fit into and doesn't preserve aspect ratio */
        case pdc_tauto:
        if (xscaling)
        {
            height = elemsize->y;
            scale->x = fscale;
            scale->y = 1.0;
        }
        else
        {
            width = elemsize->x;
            scale->x = 1.0;
            scale->y = fscale;
        }
        break;

        /* only positioning */
        case pdc_nofit:
        case pdc_clip:
        width = elemsize->x;
        height = elemsize->y;
        scale->x = 1.0;
        scale->y = 1.0;
        break;
    }

    /* placed element box */
    elembox->ll.x = refpos.x - elemrelpos->x * width;
    elembox->ll.y = refpos.y - elemrelpos->y * height;
    elembox->ur.x = refpos.x + (1.0 - elemrelpos->x) * width;
    elembox->ur.y = refpos.y + (1.0 - elemrelpos->y) * height;
}


void
pdc_box2polyline(const pdc_matrix *M, const pdc_box *box, pdc_vector *polyline)
{
    pdc_scalar x[4], y[4];

    /* counterclockwise order */
    if (M != NULL)
    {
        pdc_transform_point(M, box->ll.x, box->ll.y, &x[0], &y[0]);
        pdc_transform_point(M, box->ur.x, box->ll.y, &x[1], &y[1]);
        pdc_transform_point(M, box->ur.x, box->ur.y, &x[2], &y[2]);
        pdc_transform_point(M, box->ll.x, box->ur.y, &x[3], &y[3]);

        polyline[0].x = x[0];
        polyline[0].y = y[0];
        polyline[1].x = x[1];
        polyline[1].y = y[1];
        polyline[2].x = x[2];
        polyline[2].y = y[2];
        polyline[3].x = x[3];
        polyline[3].y = y[3];
        polyline[4] = polyline[0];
    }
    else
    {
        polyline[0].x = box->ll.x;
        polyline[0].y = box->ll.y;
        polyline[1].x = box->ur.x;
        polyline[1].y = box->ll.y;
        polyline[2].x = box->ur.x;
        polyline[2].y = box->ur.y;
        polyline[3].x = box->ll.x;
        polyline[3].y = box->ur.y;
        polyline[4] = polyline[0];
    }
}

void *
pdc_delete_polylinelist(pdc_core *pdc, pdc_polyline *polylinelist, int nplines)
{
    int i;

    if (polylinelist != NULL)
    {
        for (i = 0; i < nplines; i++)
            pdc_free(pdc, polylinelist[i].p);
        pdc_free(pdc, polylinelist);
    }

    return NULL;
}

void
pdc_init_box(pdc_box *box)
{
    box->ll.x = PDC_FLOAT_MAX;
    box->ll.y = PDC_FLOAT_MAX;
    box->ur.x = PDC_FLOAT_MIN;
    box->ur.y = PDC_FLOAT_MIN;
}

void
pdc_adapt_box(pdc_box *box, const pdc_vector *v)
{
    if (v->x < box->ll.x)
        box->ll.x = v->x;
    if (v->y < box->ll.y)
        box->ll.y = v->y;

    if (v->x > box->ur.x)
        box->ur.x = v->x;
    if (v->y > box->ur.y)
        box->ur.y = v->y;
}

void
pdc_normalize_box(pdc_box *box, pdc_scalar ydir)
{
    pdc_scalar sxy;

    if (box->ll.x > box->ur.x)
    {
        sxy = box->ll.x;
        box->ll.x = box->ur.x;
        box->ur.x = sxy;
    }

    if (ydir * box->ll.y > ydir * box->ur.y)
    {
        sxy = box->ll.y;
        box->ll.y = box->ur.y;
        box->ur.y = sxy;
    }
}

void
pdc_transform_box(const pdc_matrix *M, pdc_box *box, pdc_box *tbox)
{
    pdc_vector polyline[5];
    pdc_box tmpbox;
    int i;

    pdc_box2polyline(NULL, box, polyline);

    pdc_init_box(&tmpbox);

    for (i = 0; i < 4; i++)
    {
        pdc_transform_vector(M, &polyline[i], NULL);
        pdc_adapt_box(&tmpbox, &polyline[i]);
    }

    if (tbox)
        *tbox = tmpbox;
    else
        *box = tmpbox;
}

/* --------------------------- rectangles  --------------------------- */
pdc_bool
pdc_rect_isnull(const pdc_rectangle *r)
{
    if (!r)
	return pdc_true;

    return
        (r->llx == 0 && r->lly == 0 &&
         r->urx == 0 && r->ury == 0);
}

pdc_bool
pdc_rect_contains(const pdc_rectangle *r1, const pdc_rectangle *r2)
{
    return
        (r1->llx <= r2->llx && r1->lly <= r2->lly &&
         r1->urx >= r2->urx && r1->ury >= r2->ury);
}

void
pdc_rect_copy(pdc_rectangle *r1, const pdc_rectangle *r2)
{
    r1->llx = r2->llx;
    r1->lly = r2->lly;
    r1->urx = r2->urx;
    r1->ury = r2->ury;
}

void
pdc_rect_init(pdc_rectangle *r, pdc_scalar llx, pdc_scalar lly,
                                pdc_scalar urx, pdc_scalar ury)
{
    r->llx = llx;
    r->lly = lly;
    r->urx = urx;
    r->ury = ury;
}

pdc_bool
pdc_rect_intersect(
    pdc_rectangle *result,
    const pdc_rectangle *r1,
    const pdc_rectangle *r2)
{
    if (r1->urx <= r2->llx ||
	r2->urx <= r1->llx ||
	r1->ury <= r2->lly ||
	r2->ury <= r1->lly)
    {
	if (result)
	{
	    result->llx = result->lly = result->urx = result->ury = 0;
	}

	return pdc_false;
    }

    if (result)
    {
	result->llx = MAX(r1->llx, r2->llx);
	result->urx = MIN(r1->urx, r2->urx);
	result->lly = MAX(r1->lly, r2->lly);
	result->ury = MIN(r1->ury, r2->ury);
    }

    return pdc_true;
}

void
pdc_rect_transform(const pdc_matrix *M, const pdc_rectangle *r1,
                   pdc_rectangle *r2)
{
    pdc_scalar x[4], y[4];
    int i;

    pdc_transform_point(M, r1->llx, r1->lly, &x[0], &y[0]);
    pdc_transform_point(M, r1->urx, r1->lly, &x[1], &y[1]);
    pdc_transform_point(M, r1->urx, r1->ury, &x[2], &y[2]);
    pdc_transform_point(M, r1->llx, r1->ury, &x[3], &y[3]);

    pdc_rect_init(r2, PDC_FLOAT_MAX, PDC_FLOAT_MAX,
                      PDC_FLOAT_MIN, PDC_FLOAT_MIN);

    for (i = 0; i < 4; i++)
    {
        if (x[i] < r2->llx)
            r2->llx = x[i];
        if (y[i] < r2->lly)
            r2->lly = y[i];

        if (x[i] > r2->urx)
            r2->urx = x[i];
        if (y[i] > r2->ury)
            r2->ury = y[i];
    }
}

void pdc_rect_normalize(pdc_rectangle *r)
{
    double aux;

    if (r->urx < r->llx)
    {
	aux = r->llx; r->llx = r->urx; r->urx = aux;
    }

    if (r->ury < r->lly)
    {
	aux = r->lly; r->lly = r->ury; r->ury = aux;
    }
}

void pdc_rect_normalize2(pdc_rectangle *dst, const pdc_rectangle *src)
{
    if (src->llx < src->urx)
    {
	dst->llx = src->llx;
	dst->urx = src->urx;
    }
    else
    {
	dst->llx = src->urx;
	dst->urx = src->llx;
    }

    if (src->lly < src->ury)
    {
	dst->lly = src->lly;
	dst->ury = src->ury;
    }
    else
    {
	dst->lly = src->ury;
	dst->ury = src->lly;
    }
}

void
pdc_polyline2rect(const pdc_vector *polyline, int np, pdc_rectangle *r)
{
    int i;

    pdc_rect_init(r, PDC_FLOAT_MAX, PDC_FLOAT_MAX,
                     PDC_FLOAT_MIN, PDC_FLOAT_MIN);

    for (i = 0; i < np; i++)
    {
        if (polyline[i].x < r->llx)
            r->llx = polyline[i].x;
        if (polyline[i].y < r->lly)
            r->lly = polyline[i].y;

        if (polyline[i].x > r->urx)
            r->urx = polyline[i].x;
        if (polyline[i].y > r->ury)
            r->ury = polyline[i].y;
    }
}

void
pdc_rect2polyline(const pdc_matrix *M, const pdc_rectangle *r,
                  pdc_vector *polyline)
{
    pdc_scalar x[4], y[4];

    /* counterclockwise order */
    if (M != NULL)
    {
        pdc_transform_point(M, r->llx, r->lly, &x[0], &y[0]);
        pdc_transform_point(M, r->urx, r->lly, &x[1], &y[1]);
        pdc_transform_point(M, r->urx, r->ury, &x[2], &y[2]);
        pdc_transform_point(M, r->llx, r->ury, &x[3], &y[3]);

        polyline[0].x = x[0];
        polyline[0].y = y[0];
        polyline[1].x = x[1];
        polyline[1].y = y[1];
        polyline[2].x = x[2];
        polyline[2].y = y[2];
        polyline[3].x = x[3];
        polyline[3].y = y[3];
        polyline[4] = polyline[0];
    }
    else
    {
        polyline[0].x = r->llx;
        polyline[0].y = r->lly;
        polyline[1].x = r->urx;
        polyline[1].y = r->lly;
        polyline[2].x = r->urx;
        polyline[2].y = r->ury;
        polyline[3].x = r->llx;
        polyline[3].y = r->ury;
        polyline[4] = polyline[0];
    }
}

/* debug print */
void
pdc_print_rectangle(const char *name, const pdc_rectangle *r)
{
    printf("%s: llx=%g, lly=%g, urx=%g, ury=%g\n",
           name, r->llx, r->lly, r->urx, r->ury);
}
