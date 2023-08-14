
#ifndef ORACLE_CH

#define ORACLE_CH

#define OCI_SUCCESS 0                      /* maps to SQL_SUCCESS of SAG CLI */
#define OCI_SUCCESS_WITH_INFO 1             /* maps to SQL_SUCCESS_WITH_INFO */
#define OCI_RESERVED_FOR_INT_USE 200                            /* reserved */ 
#define OCI_NO_DATA 100                               /* maps to SQL_NO_DATA */
#define OCI_ERROR -1                                    /* maps to SQL_ERROR */
#define OCI_INVALID_HANDLE -2                  /* maps to SQL_INVALID_HANDLE */
#define OCI_NEED_DATA 99                            /* maps to SQL_NEED_DATA */
#define OCI_STILL_EXECUTING -3123                   /* OCI would block error */
#define OCI_CONTINUE -24200    /* Continue with the body of the OCI function */

#define SQL_ERROR                           OCI_ERROR
#define SQL_INVALID_HANDLE                  OCI_INVALID_HANDLE
#define SQL_NEED_DATA                       OCI_NEED_DATA
#define SQL_NO_DATA_FOUND                   OCI_NO_DATA
#define SQL_SUCCESS                         OCI_SUCCESS
#define SQL_SUCCESS_WITH_INFO               OCI_SUCCESS_WITH_INFO
#define SQL_DROP                            OCI_SUCCESS_WITH_INFO

#endif
