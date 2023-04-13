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
 * PDFlib core geometry utilities
 *
 */

#ifndef PC_GEOM_H
#define PC_GEOM_H

/* Unfortunately M_PI causes porting woes, so we use a private name */
#define PDC_M_PI                3.14159265358979323846          /* pi */
#define PDC_DEG2RAD             0.0174532925199433
#define PDC_RAD2DEG            57.2957795130823070

/* Conversion factors */
#define PDC_INCH2METER          0.0254
#define PDC_METER2INCH         39.3701

/* same as PDF_SMALLREAL */
#define PDC_SMALLREAL   (0.000015)

typedef double pdc_scalar;
typedef struct { pdc_scalar x, y; } pdc_vector;
typedef struct { pdc_vector ll, ur; } pdc_box;
typedef struct { pdc_scalar llx, lly, urx, ury; } pdc_rectangle;
typedef struct { int np; pdc_vector *p; } pdc_polyline;
typedef struct { pdc_scalar a, b, c, d, e, f; } pdc_matrix;

/* methods for fitting rectangle elements into a box */
typedef enum
{
    pdc_nofit = 0,      /* no fit, only positioning */
    pdc_clip,           /* no fit, only positioning with following condition:
                         * - the parts of element beyond the bounds of box
                         *   are clipped */
    pdc_slice,          /* fit into the box with following conditions:
                         * - aspect ratio of element is preserved
                         * - entire box is covered by the element
                         * - the parts of element beyond the bounds of box
                         *   are clipped */
    pdc_meet,           /* fit into the box with following conditions:
                         * - aspect ratio of element is preserved
                         * - entire element is visible in the box */
    pdc_entire,         /* fit into the box with following conditions:
                         * - entire box is covered by the element
                         * - entire element is visible in the box */
    pdc_tauto           /* automatic fitting. If element extends fit box in
                         * length, then element is shrinked, if shrink
                         * factor is greater than a specified value. Otherwise
                         * pdc_meet is applied. */
}
pdc_fitmethod;

pdc_bool pdc_is_identity_matrix(pdc_matrix *m);
void pdc_identity_matrix(pdc_matrix *M);
void pdc_translation_matrix(pdc_scalar tx, pdc_scalar ty, pdc_matrix *M);
void pdc_scale_matrix(pdc_scalar sx, pdc_scalar sy, pdc_matrix *M);
void pdc_rotation_matrix(pdc_scalar angle, pdc_matrix *M);
void pdc_skew_matrix(pdc_scalar alpha, pdc_scalar beta, pdc_matrix *M);
void pdc_multiply_matrix(const pdc_matrix *M, pdc_matrix *N);
void pdc_multiply_matrix3(pdc_matrix *L, const pdc_matrix *M,
        const pdc_matrix *N);
void pdc_multiply_6s_matrix(pdc_matrix *M, pdc_scalar a, pdc_scalar b,
        pdc_scalar c, pdc_scalar d, pdc_scalar e, pdc_scalar f);
void pdc_invert_matrix(pdc_core *pdc, pdc_matrix *N, pdc_matrix *M);
void pdc_print_matrix(const char *name, const pdc_matrix *M);
pdc_scalar pdc_transform_scalar(const pdc_matrix *M, pdc_scalar s);
void pdc_transform_point(const pdc_matrix *M,
        pdc_scalar x, pdc_scalar y, pdc_scalar *tx, pdc_scalar *ty);
void pdc_transform_vector(const pdc_matrix *M, pdc_vector *v, pdc_vector *tv);
void pdc_transform_rvector(const pdc_matrix *M, pdc_vector *v, pdc_vector *tv);
pdc_scalar pdc_get_vector_length(pdc_vector *start, pdc_vector *end);

void pdc_place_element(pdc_fitmethod method, pdc_scalar minfscale,
        const pdc_box *fitbox, const pdc_vector *fitrelpos,
        const pdc_vector *elemsize, const pdc_vector *elemrelpos,
        pdc_box *elembox, pdc_vector *scale);
void pdc_box2polyline(const pdc_matrix *M, const pdc_box *box,
        pdc_vector *polyline);
void *pdc_delete_polylinelist(pdc_core *pdc, pdc_polyline *polylinelist,
        int nplines);
void pdc_init_box(pdc_box *box);
void pdc_adapt_box(pdc_box *box, const pdc_vector *v);
void pdc_normalize_box(pdc_box *box, pdc_scalar ydir);
void pdc_transform_box(const pdc_matrix *M, pdc_box *box, pdc_box *tbox);

pdc_bool pdc_rect_isnull(const pdc_rectangle *r);
pdc_bool pdc_rect_contains(const pdc_rectangle *r1, const pdc_rectangle *r2);
void pdc_rect_copy(pdc_rectangle *r1, const pdc_rectangle *r2);
void pdc_rect_init(pdc_rectangle *r,
        pdc_scalar llx, pdc_scalar lly, pdc_scalar urx, pdc_scalar ury);
pdc_bool pdc_rect_intersect(pdc_rectangle *result,
	const pdc_rectangle *r1, const pdc_rectangle *r2);
void pdc_rect_transform(const pdc_matrix *M,
        const pdc_rectangle *r1, pdc_rectangle *r2);
void pdc_rect_normalize(pdc_rectangle *r);
void pdc_rect_normalize2(pdc_rectangle *dst, const pdc_rectangle *src);
void pdc_rect2polyline(const pdc_matrix *M, const pdc_rectangle *r,
        pdc_vector *polyline);
void pdc_polyline2rect(const pdc_vector *polyline, int np, pdc_rectangle *r);
void pdc_print_rectangle(const char *name, const pdc_rectangle *r);

#endif  /* PC_GEOM_H */

