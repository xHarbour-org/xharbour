/****************************************************************************
 *                                                                          *
 * File    : cc.h                                                           *
 *                                                                          *
 * Purpose : ISO C Compiler Driver; constants and definitions.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef _CCL_H
#define _CCL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "msg.h"

#define tstrcpy(s)  (strlen(s) ? strcpy(malloc(strlen(s)+1), (s)) : NULL)

#define UNREFERENCED_PARAMETER(P) (P)

#define SWAP(type,a,b)  { type t; t = (a); (a) = (b); (b) = t; }
#define NELEMS(arr)     (sizeof(arr) / sizeof(arr[0]))
#define ROUNDUP(n,m)    ((((n) + (m) - 1) / (m)) * (m))

/* Display name for this program */
#define PROGRAM_NAME  "CC"

struct options
{
    int compile_only;
};

/* main.c */
void printmsg(int, ...);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _CCL_H */
