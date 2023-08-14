/*
* SQLRDD PRG Header (Oracle DEFINES)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "sqlodbc.ch"

#define OCI_SUCCESS 0                      /* maps to SQL_SUCCESS of SAG CLI */
#define OCI_SUCCESS_WITH_INFO 1             /* maps to SQL_SUCCESS_WITH_INFO */
#define OCI_RESERVED_FOR_INT_USE 200                            /* reserved */
#define OCI_NO_DATA 100                               /* maps to SQL_NO_DATA */
#define OCI_ERROR -1                                    /* maps to SQL_ERROR */
#define OCI_INVALID_HANDLE -2                  /* maps to SQL_INVALID_HANDLE */
#define OCI_NEED_DATA 99                            /* maps to SQL_NEED_DATA */
#define OCI_STILL_EXECUTING -3123                   /* OCI would block error */
#define OCI_CONTINUE -24200    /* Continue with the body of the OCI function */

#define SQLT_CHR  1                        /* (ORANET TYPE) character string */
#define SQLT_NUM  2                          /* (ORANET TYPE) oracle numeric */
#define SQLT_INT  3                                 /* (ORANET TYPE) integer */
#define SQLT_FLT  4                   /* (ORANET TYPE) Floating point number */
#define SQLT_STR  5                                /* zero terminated string */
#define SQLT_VNU  6                        /* NUM with preceding length byte */
#define SQLT_PDN  7                  /* (ORANET TYPE) Packed Decimal Numeric */
#define SQLT_LNG  8                                                  /* long */
#define SQLT_VCS  9                             /* Variable character string */
#define SQLT_NON  10                      /* Null/empty PCC Descriptor entry */
#define SQLT_RID  11                                                /* rowid */
#define SQLT_DAT  12                                /* date in oracle format */
#define SQLT_VBI  15                                 /* binary in VCS format */
#define SQLT_BIN  23                                  /* binary data(DTYBIN) */
#define SQLT_LBI  24                                          /* long binary */
#define SQLT_UIN  68                                     /* unsigned integer */
#define SQLT_SLS  91                        /* Display sign leading separate */
#define SQLT_LVC  94                                  /* Longer longs (char) */
#define SQLT_LVB  95                                  /* Longer long binary */
#define SQLT_AFC  96                                      /* Ansi fixed char */
#define SQLT_AVC  97                                        /* Ansi Var char */
#define SQLT_CUR  102                                        /* cursor  type */
#define SQLT_RDD  104                                    /* rowid descriptor */
#define SQLT_LAB  105                                          /* label type */
#define SQLT_OSL  106                                        /* oslabel type */

#define SQLT_NTY  108                                   /* named object type */
#define SQLT_REF  110                                            /* ref type */
#define SQLT_CLOB 112                                       /* character lob */
#define SQLT_BLOB 113                                          /* binary lob */
#define SQLT_BFILEE 114                                    /* binary file lob */
#define SQLT_CFILEE 115                                 /* character file lob */
#define SQLT_RSET 116                                     /* result set type */
#define SQLT_NCO  122      /* named collection type (varray or nested table) */
#define SQLT_VST  155                                      /* OCIString type */
#define SQLT_ODT  156                                        /* OCIDate type */

/* datetimes and intervals */
#define SQLT_DATE                      184                      /* ANSI Date */
#define SQLT_TIME                      185                           /* TIME */
#define SQLT_TIME_TZ                   186            /* TIME WITH TIME ZONE */
#define SQLT_TIMESTAMP                 187                      /* TIMESTAMP */
#define SQLT_TIMESTAMP_TZ              188       /* TIMESTAMP WITH TIME ZONE */
#define SQLT_INTERVAL_YM               189         /* INTERVAL YEAR TO MONTH */
#define SQLT_INTERVAL_DS               190         /* INTERVAL DAY TO SECOND */
#define SQLT_TIMESTAMP_LTZ             232        /* TIMESTAMP WITH LOCAL TZ */

#define SQLT_PNTY   241              /* pl/sql representation of named types */
