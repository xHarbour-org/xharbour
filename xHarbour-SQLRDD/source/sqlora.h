#ifndef __SQLORA8_H_LOADED
#define __SQLORA8_H_LOADED
/* $Id$ */
/**
 * @file sqlora.h
 * libsqlora8 - Easy C API to Oracle using OCI.
 *
 * @version 2.3
 *
 * @author Kai Poitschke
 *
 * Copyright (c) 1991-2004 Kai Poitschke (kai[_at_]poitschke.de)
 *
 * This file is part of the libsqlora8 package which can be found
 * at http://www.poitschke.de/libsqlora8/
 */
/*
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose with or without fee is hereby granted, provided that
 * the above copyright notice and this permission notice appear in all
 * copies.
 *
 *    THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 *    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *    IN NO EVENT SHALL THE AUTHORS AND COPYRIGHT HOLDERS AND THEIR
 *    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *    USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 *    OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *    SUCH DAMAGE.
 *
 */


/**
 * @defgroup env Environment Variables
 * @{
 * The library evaluates the following enviroment variables during
 * initialization:
 * <ul>
 * <li>SQLORA_PREFETCH_ROWS (1 .. ; initial value 100)
 * <li>SQLORA_TRACE_LEVEL
 *   <ul>
 *   <li>0 : Trace disabled (default)
 *   <li>1 : Print errors to tracefile
 *   <li>2 : Print function calls to tracefile
 *   <li>3 : Print more detailed information to tracefile
 *   <li>4 : Print also malloc/realloc operations.
 *   </ul>
 *
 * <li>SQLORA_TRACE_FILE (The trace file; default sqlora8.trc)
 * <li>SQLORA_LONGSIZE   (Max size of a fetched long; default 64K).
 * </ul>
 * During login, the library reads ORACLE_SID, if no tnsname was specified in
 * the connect string.
 * @}
 */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#include <stdio.h>
#ifndef LIBSQLORA8_CONFIG_H
#define LIBSQLORA8_CONFIG_H

#define LIBSQLORA8_MAJOR_VERSION 2
#define LIBSQLORA8_MINOR_VERSION 3
#define LIBSQLORA8_MICRO_VERSION 2
#define LIBSQLORA8_INTERFACE_AGE 0
#define LIBSQLORA8_BINARY_AGE    2

#define LIBSQLORA8_VERSION "2.3.2"

/* This is set to 1 if the library was compiled with a thread package */
#define LIBSQLORA8_THREADED 0

/* If LIBSQLORA8_THREADED is 1, this define tells you which package was used
 * It is either "posix" or "oracle"
 */
#define LIBSQLORA8_THREADS ""

#ifdef __MINGW32__
#define _int64 __int64
#endif

#endif
#endif /* DOXYGEN_SHOULD_SKIP_THIS */

/*
 * Definitions that allow this header file to be used either with or
 * without ANSI C features like function prototypes.
 */


/* @def __BEGIN_DECLS
 * __BEGIN_DECLS should be used at the beginning of your declarations,
 *  so that C++ compilers don't mangle their names.  Use @ref__END_DECLS at
 *  the end of C declarations.
 */

/* @def __END_DECLS
 * Opposite of @ref __BEGIN_DECLS
 */
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif



/*
 * @def __P
 * __P is a macro used to wrap function prototypes.
 *
 * Compilers that don't understand ANSI C prototypes still work, and ANSI C
 * compilers can issue warnings about type mismatches.
 * Use autoconf macro AC_C_PROTOTYPES to check for prototypes.
*/
#undef __P
#if (defined(PROTOTYPES) || defined(__STDC__) || defined(__cplusplus) )
# define __P(protos) protos
#else
# define __P(protos) ()
#endif



/*
 * @def CONST
 * Used to define constants.
 *
 * For ANSI C compilers this expands to const, for others its an empty definition.
 * @note AC_C_CONST defines const empty if it is not supported.
 */
#undef CONST

#ifndef const
#  if (defined(__STDC__) || defined(PROTOTYPES) || defined(__cplusplus))
#    define CONST const
#  else
#    define CONST
#  endif
#else
#  define CONST
#endif



/**
 * @defgroup constants Constants
 * @{
 */



/**
 * @enum sqlo_status_codes
 * Constants defining the return codes of the functions.
 *
 * These codes map basically to the OCI return codes.
 */
enum sqlo_status_codes {
  SQLO_SUCCESS             = 0,   /**< General success code (maps to OCI_SUCCESS) */
  SQLO_ERROR               = -1,      /**< General error code (maps to OCI_ERROR) */
  SQLO_INVALID_HANDLE      = -2,                  /**< Maps to OCI_INVALID_HANDLE */
  SQLO_STILL_EXECUTING     = -3123,              /**< Maps to OCI_STILL_EXECUTING */
  SQLO_CONTINUE            = -24200,                    /**< Maps to OCI_CONTINUE */
  SQLO_SUCCESS_WITH_INFO   = 1,                /**< Maps to OCI_SUCCESS_WITH_INFO */
  SQLO_NEED_DATA           = 99,                       /**< Maps to OCI_NEED_DATA */
  SQLO_NO_DATA             = 100                         /**< Maps to OCI_NO_DATA */
};



/**
 * @enum sqlo_error_codes
 * Constants defining error codes returned by the library
 *
 * All Error codes are < -30000 to be seperated from the oracle error space.
 */
enum sqlo_error_codes {
  SQLO_ERROR_BASE          = -30000,     /**< All our codes are below this value */
  SQLO_INVALID_DB_HANDLE   = (SQLO_ERROR_BASE - 1),      /**< Invalid dbh passed */
  SQLO_ERRMALLOC           = (SQLO_ERROR_BASE - 2),  /**< Cannot allocate memory */
  SQLO_INVALID_STMT_HANDLE = (SQLO_ERROR_BASE - 3), /**< Invalid statement handle passed*/
  SQLO_STMT_NOT_OPENED     = (SQLO_ERROR_BASE - 4), /**< Tried to reopen a not opened
                                                       cursor in @ref sqlo_reopen */
  SQLO_INVALID_STMT_TYPE   = (SQLO_ERROR_BASE - 5), /**< Tried to parse a PL/SQL block
                                                       with @ref sqlo_open */
  SQLO_STMT_NOT_PARSED     = (SQLO_ERROR_BASE - 6), /**< Tried to bind in/out variables
                                                       for a non-parsed statement */
  SQLO_INVALID_OCI_HANDLE_TYPE = (SQLO_ERROR_BASE - 7), /**< Passed a wrong handle type
                                                           to @ref sqlo_get_oci_handle */
  SQLO_MALFORMED_VERSION_STR = (SQLO_ERROR_BASE - 8), /**< Passed an invalid version
                                                         string to @ref sqlo_version */
  SQLO_WRONG_VERSION       = (SQLO_ERROR_BASE - 9), /**< The version of the library does
                                                      not match your request */
  SQLO_INVALID_COLPOS      = (SQLO_ERROR_BASE - 10), /**< Column position passed to a function is wrong */
  SQLO_INVALID_SQL         = (SQLO_ERROR_BASE -11), /**< A invalid sql statement was passed to @ref sqlo_open or @ref sqlo_open2 */
  SQLO_UNSUPPORTED_DATA_TYPE   = (SQLO_ERROR_BASE - 12) /**< Try to query a unsupported data type. */
};



/**
 * @enum sqlo_constants
 * Some constants used to pass to the functions.
 */
enum sqlo_constants {
  SQLO_OFF                 =  0, /**< use this to switch something off */
  SQLO_ON                  =  1, /**< use this to switch someting on */
  SQLO_NULL_IND            = -1, /**< NULL indicator */
  SQLO_NOT_NULL_IND        =  0, /**< NOT NULL indicator */
  SQLO_STH_INIT            = -1, /**< You must init the sth with this before the
                                    first call of @ref sqlo_open2 */

  /* constants for piece operations (lob writes). */
  SQLO_ONE_PIECE           = 0, /**< Piecewise operation code in
                                   @ref sqlo_lob_write_buffer  */
  SQLO_FIRST_PIECE         = 1, /**< Piecewise operation code in
                                   @ref sqlo_lob_write_buffer  */
  SQLO_NEXT_PIECE          = 2, /**< Piecewise operation code in
                                   @ref sqlo_lob_write_buffer  */
  SQLO_LAST_PIECE          = 3 /**< Piecewise operation code in
                                  @ref sqlo_lob_write_buffer  */

};



/**
 * @enum sqlo_data_types
 * The data types for bind variables
 * The oracle constants are copied from $ORACLE_HOME/rdbms/demo/ocidfn.h
 * @note Not all datatypes are implemented in this module (especially exotic
 * ones)
 * @see sqlo_bind_by_name sqlo_bind_by_pos sqlo_defined_by_pos
 */
enum sqlo_data_types {
  SQLOT_CHR  = 1,                      /**< (ORANET TYPE) character string */
  SQLOT_NUM  = 2,                        /**< (ORANET TYPE) oracle numeric */
  SQLOT_INT  = 3,                               /**< (ORANET TYPE) integer */
  SQLOT_FLT  = 4,                 /**< (ORANET TYPE) Floating point number */
  SQLOT_STR  = 5,                              /**< zero terminated string */
  SQLOT_VNU  = 6,                      /**< NUM with preceding length byte */
  SQLOT_PDN  = 7,                /**< (ORANET TYPE) Packed Decimal Numeric */
  SQLOT_LNG  = 8,                                                /**< long */
  SQLOT_VCS  = 9,                           /**< Variable character string */
  SQLOT_NON  = 10,                    /**< Null/empty PCC Descriptor entry */
  SQLOT_RID  = 11,                                              /**< rowid */
  SQLOT_DAT  = 12,                              /**< date in oracle format */
  SQLOT_VBI  = 15,                               /**< binary in VCS format */
  SQLOT_BIN  = 23,                                /**< binary data(DTYBIN) */
  SQLOT_LBI  = 24,                                        /**< long binary */
  SQLOT_UIN  = 68,                                   /**< unsigned integer */
  SQLOT_SLS  = 91,                      /**< Display sign leading separate */
  SQLOT_LVC  = 94,                                /**< Longer longs (char) */
  SQLOT_LVB  = 95,                                 /**< Longer long binary */
  SQLOT_AFC  = 96,                                    /**< Ansi fixed char */
  SQLOT_AVC  = 97,                                      /**< Ansi Var char */
  SQLOT_CUR  = 102,                                      /**< cursor  type */
  SQLOT_RDD  = 104,                                  /**< rowid descriptor */
  SQLOT_LAB  = 105,                                        /**< label type */
  SQLOT_OSL  = 106,                                      /**< oslabel type */
  SQLOT_NTY  = 108,                                 /**< named object type */
  SQLOT_REF  = 110,                                          /**< ref type */
  SQLOT_CLOB = 112,                                     /**< character lob */
  SQLOT_BLOB = 113,                                        /**< binary lob */
  SQLOT_BFILEE = 114,                                 /**< binary file lob */
  SQLOT_CFILEE = 115,                              /**< character file lob */
  SQLOT_RSET = 116,                                   /**< result set type */
  SQLOT_NCO  = 122,    /**< named collection type (varray or nested table) */
  SQLOT_VST  = 155,                                    /**< OCIString type */
  SQLOT_ODT  = 156,                                      /**< OCIDate type */

/* datetimes and intervals */
  SQLOT_DATE                   = 184,                      /**< ANSI Date */
  SQLOT_TIME                   = 185,                           /**< TIME */
  SQLOT_TIME_TZ                = 186,            /**< TIME WITH TIME ZONE */
  SQLOT_TIMESTAMP              = 187,                      /**< TIMESTAMP */
  SQLOT_TIMESTAMP_TZ           = 188,       /**< TIMESTAMP WITH TIME ZONE */
  SQLOT_INTERVAL_YM            = 189,         /**< INTERVAL YEAR TO MONTH */
  SQLOT_INTERVAL_DS            = 190,         /**< INTERVAL DAY TO SECOND */
  SQLOT_TIMESTAMP_LTZ          = 232         /**< TIMESTAMP WITH LOCAL TZ */
};



/* cxcheng: this has been added for backward compatibility -
   it needs to be here because ocidfn.h can get included ahead of sqldef.h */
#define SQLOT_FILE SQLOT_BFILEE                              /* binary file lob */
#define SQLOT_CFILE SQLOT_CFILEE
#define SQLOT_BFILE SQLOT_BFILEE



/**
 * @enum sqlo_statement_states
 * Possible statement states returned by @ref sqlo_get_stmt_state.
 */
enum sqlo_statement_states {
  SQLO_STMT_STATE_INITIALIZED = 1, /**< Statement initialized */
  SQLO_STMT_STATE_EXECUTED    = 2, /**< Statement executed */
  SQLO_STMT_STATE_END_OF_FETCH = 3 /**< Statement end of fetch reached */
};



/** @} */



/**
 * @defgroup typedefs Typedefs
 * @{
 */



/**
 * A database handle type.
 */
typedef int sqlo_db_handle_t;



/**
 * A statement handle type.
 * The statement handle stores internally the sqlo_db_handle in the upper
 * 16 bit
 */
typedef int sqlo_stmt_handle_t;



/**
 * Oracle OCI Handle types used by @ref sqlo_get_oci_handle
 */
typedef enum {
  SQLO_OCI_HTYPE_ENV     = 1,   /**< Environment handle */
  SQLO_OCI_HTYPE_ERROR   = 2,   /**< Error handle */
  SQLO_OCI_HTYPE_SVCCTX  = 3,   /**< Service context handle */
  SQLO_OCI_HTYPE_SERVER  = 4,   /**< Server handle */
  SQLO_OCI_HTYPE_SESSION = 5,   /**< Session handle */
  SQLO_OCI_HTYPE_STMT    = 6    /**< Statement handle  */
} sqlo_oci_handle_types_e;



/**
 * @typedef (void*) sqlo_lob_desc_t
 * LOB descriptor type
 * Hides the Oracle type OCILobLocator*
 */
typedef void * sqlo_lob_desc_t;



/**
 * The type of the signal handler function
 */
typedef void (*sqlo_signal_handler_t) __P((void));



/** @} */



/*-------------------------------------------------------------------------
 * Version Information
 *-----------------------------------------------------------------------*/

/**
 * @defgroup exvars  Exported Variables
 * @{
 */



/**
 * @var sqlo_major_version
 * The major version of the library
 */
//extern CONST unsigned sqlo_major_version;



/**
 * @var sqlo_minor_version
 * The minor version of the library
 */
//extern CONST unsigned sqlo_minor_version;



/**
 * @var sqlo_micro_version
 * The micro version of the library
 */
//extern CONST unsigned sqlo_micro_version;



/**
 * @var sqlo_interface_age
 * The interface age used by libtool
 */
//extern CONST unsigned sqlo_interface_age;



/**
 * @var sqlo_binary_age
 * The binary age used by libtool
 */
//extern CONST unsigned sqlo_binary_age;

/* to keep backward compatibility with <= 2.2, otherwise we break too much */
//extern CONST unsigned sqlora8_major_version;
//extern CONST unsigned sqlora8_minor_version;
//extern CONST unsigned sqlora8_micro_version;
//extern CONST unsigned sqlora8_interface_age;
//extern CONST unsigned sqlora8_binary_age;

#define SQLORA8_MAJOR_VERSION LIBSQLORA8_MAJOR_VERSION
#define SQLORA8_MINOR_VERSION LIBSQLORA8_MINOR_VERSION
#define SQLORA8_MICRO_VERSION LIBSQLORA8_MICRO_VERSION



/* @} */



/**
 * @def SQLORA8_CHECK_VERSION
 * A macro used during compile time to check the version
 * This macro is used during the configure process of your program to check
 * for the right version. Used in libsqlora8.m4
 */
#define SQLORA8_CHECK_VERSION(major,minor,micro)    \
    (LIBSQLORA8_MAJOR_VERSION > (major) || \
     (LIBSQLORA8_MAJOR_VERSION == (major) && LIBSQLORA8_MINOR_VERSION > (minor)) || \
     (LIBSQLORA8_MAJOR_VERSION == (major) && LIBSQLORA8_MINOR_VERSION == (minor) && \
      LIBSQLORA8_MICRO_VERSION >= (micro)))



/*-------------------------------------------------------------------------
 * FUNCTION DECLARATIONS
 *-----------------------------------------------------------------------*/
__BEGIN_DECLS

/**
 * @defgroup init Initialization
 * @{
 */



/**
 * Init the library.
 *
 * Reads the environment and sets up the global variables for trace-level/-file,
 * and arraysize. Initializes the OCI library.
 * This is the first function you should call, before you use the library. Make
 * sure you call it only once!
 *
 * @param threaded_mode  I - SQLO_ON threading enabled, SQLO_OFF threading disabled.
 * @param max_db         I - The maximum number of parallel connections (max. 32767)
 * @param max_cursors    I - The maximum number of open cursors per connection (max. 65535).
 *
 * @return <ul>
 * <li> SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 * @par Example:
 * @include examples.c
 */
int sqlo_init __P((int threaded_mode, unsigned int max_db, unsigned int max_cursors));



/**
 * Checks if the version is sufficient
 *
 * You can use this during runtime, to check if the dynamic linker did
 * a good job and linked you to the right version.
 *
 * @param version_str  I - The minimum version in the format major.minor.micro
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_WRONG_VERSION
 * <li>SQLO_MALFORMED_VERSION_STR
 * </ul>
 */
int sqlo_version __P((CONST char * version_str));

/** @} */



/**
 * @defgroup error Error handling functions
 * @{
 */



/**
 * Return last error string
 *
 * @param dbh I - A database handle
 *
 * @return A string containing the last error message for this dbh
 */
CONST char * sqlo_geterror __P(( sqlo_db_handle_t dbh ));



/**
 * Return the last error code
 *
 * @param dbh I - A database handle
 *
 * @return The last error code for this dbh.
 */
int sqlo_geterrcode __P(( sqlo_db_handle_t dbh ));

/** @} */



/**
 * @defgroup easy The easy interface
 * Functions in this group use basically bind variables passed as strings in
 * an argv. The query results are also converted to strings.
 *
 * @{ */



/**
 * Tests if a value exists in a table.
 *
 * Tests if a record exists in a table where field = value [AND where].
 *
 * @param dbh     I - A database handle
 * @param table   I - A table name
 * @param colname I - A column name
 * @param colval  I - A column value
 * @param where   I - More conditions (optional)
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_NO_DATA
 * <li> < 0 on error.
 * </ul>
 * @par Example:
 * @include ex1.c
 */
int sqlo_exists __P(( sqlo_db_handle_t dbh,
                      CONST char * table,
                      CONST char * colname,
                      CONST char * colval,
                      CONST char * where ));



/**
 * Counts the number of items in the table.
 *
 * Counts the number of items where field = value [AND where]
 *
 * @param dbh     I - A database handle
 * @param table   I - A table name
 * @param colname I - A column name
 * @param colval  I - A column value
 * @param where   I - More conditions (optional)
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_NO_DATA
 * <li> < 0 on error
 * </ul>
 * @par Example:
 * @include ex2.c
 */
int sqlo_count __P((sqlo_db_handle_t dbh,
                    CONST char * table,
                    CONST char * colname,
                    CONST char * colval,
                    CONST char * where ));



/**
 * Run a simple sql statements with parameters
 *
 * Like @ref sqlo_exec, but with bind parameters.
 * This is basically the same as calling @ref sqlo_open followed by @ref sqlo_fetch and
 * @ref sqlo_close.
 *
 * @param dbh   I - A database handle
 * @param stmt  I - A sql statement (non-query).
 * @param argc  I - Number of arguments in argv.
 * @param argv  I - The arguments
 *
 * @return <ul>
 * <li>The number of processed rows.
 * <li>SQLO_STILL_EXECUTING in non-blocking mode.
 * <li> < 0 on error.
 * </ul>
 * @par Example:
 * @include ex3.c
 */
int sqlo_run __P(( sqlo_db_handle_t dbh, CONST char * stmt, int argc, CONST char ** argv));



/**
 * Open a new cursor
 *
 * This function opens a new cursor for a query statement.
 *
 * If the stmt is a SELECT statement, the function sets the attribute
 * OCI_ATTR_PREFETCH rows to the max arraysize parameter of the library.
 * This is a kind of internal array fetch Oracle provides to speed up the fetching.
 * In case of an error, the allocated resources are freed. You don't have
 * to call sqlo_close for an invalid sth.
 *
 * @deprecated  For new developments please use @ref sqlo_open2
 * @param dbh  I - A database handle
 * @param stmt I - A sql statement
 * @param argc I - Number of arguments in argv
 * @param argv I - Arguments
 *
 * @return <ul>
 * <li>A statement handle
 * <li> < 0 on error
 * </ul>
 * @par Example:
 * @include ex4.c
 *
 * @see sqlo_open2, sqlo_fetch, sqlo_values, sqlo_close
 */
sqlo_stmt_handle_t sqlo_open __P((sqlo_db_handle_t dbh,
                                  CONST char * stmt,
                                  int argc,
                                  CONST char ** argv));

int sqlo_describecol __P(( sqlo_stmt_handle_t    sth,
                           int                   col,
                           unsigned short  *     dType,
                           char * *              name ,
                           int *                 namelen,
                           int *                 prec,
                           int *                 scale,
                           int *                 dbsize,
                           int *                 nullok ));

/**
 * Open a new cursor
 *
 * This function opens a new cursor for a query statement.
 * Use this function if your bind variables are all strings.
 * If you need native datatype support, use @ref sqlo_prepare
 *
 * If the stmt is a SELECT statement, the function sets the attribute
 * OCI_ATTR_PREFETCH rows to the max arraysize parameter of the library.
 * This is a kind of internal array fetch Oracle provides to speed up the fetching.
 * In case of an error, the allocated resources are freed. You don't have
 * to call sqlo_close for an invalid sth.
 *
 * @attention You have to init the passed statement handle with SQLO_STH_INIT.
 * This is required escpecially in non-blocking mode.
 *
 * @param sthp I/O - Returns the new sth in *sthp.
 * @param dbh  I - A database handle
 * @param stmt I - A sql statement
 * @param argc I - Number of arguments in argv
 * @param argv I - Arguments
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_STILL_EXECUTING in non-blocking mode
 * <li> < 0 on error
 * </ul>
 * @par Example:
 * @include ex5.c
 *
 * @see sqlo_fetch, sqlo_values, sqlo_close
 * @since Version 2.2
 */
int sqlo_open2 __P((sqlo_stmt_handle_t * sthp,
                    sqlo_db_handle_t dbh,
                    CONST char * stmt,
                    int argc,
                    CONST char ** argv));



/**
 * Reopens a already used cursor
 *
 * This function reopens an already used cursor with new bind variables.
 * Reopening cursors improve the speed, because no new parse is necessary.
 *
 * @param sth  I - The sth you want to rebind.
 * @param argc I - Number of arguments in argv
 * @param argv I - Arguments
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_STILL_EXECUTING in non-blocking mode
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * @include ex6.c
 *
 * @see sqlo_open2, sqlo_fetch, sqlo_values, sqlo_close
 */
int sqlo_reopen __P((sqlo_stmt_handle_t sth,
                     int argc,
                     CONST char ** argv));



/**
 * Fetches the data from an open cursor.
 *
 * This functions fetches data from an open cursor, if the sql was a query.
 * For non-queries, the statement is executed. Use @ref sqlo_values to get the
 * data.
 * @attention nrows must be 1 for cursors opened with @ref sqlo_open or @ref sqlo_open2.
 * For cursors where the output variables were defined manually with @ref sqlo_define_by_pos,
 * this can be for example the size of the array in which you are fetching.
 *
 * @param sth   I - A statement handle
 * @param nrows I - The number of rows to fetch.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_NO_DATA
 * <li>SQLO_SUCCESS_WITH_INFO
 * <li>< 0 on error
 * </ul>
 * SQLO_SUCCESS_WITH_INFO is returned for the following oracle errors
 * (from the Oracle documentation of OCIStmtFetch):
 * <ul>
 * <li>ORA-24344 Success with compilation error
 * <li>ORA-23445 A truncation or null fetch error occurred
 * <li>ORA-23447 Warning of a NULL column in an aggregate function
 * </ul>
 * @see sqlo_open2, sqlo_values, sqlo_close
 */
int sqlo_fetch __P((sqlo_stmt_handle_t sth, unsigned int nrows));



/**
 * Get one dataset
 *
 * Returns the data for one set of data fetched via @ref sqlo_fetch.
 *
 * @param sth I - A statement handle
 * @param num O - A destination where the function could write the size of the
 *                returned array (optional)
 * @param dostrip I - A flag indicating whether trailing blanks should be stripped
 *                    off (leading blanks in case of numbers).
 *
 * @return A pointer to an array of strings containing the data values
 *
 * @par Example:
 * @include ex7.c
 *
 * @see sqlo_fetch, sqlo_value_lens, sqlo_open, sqlo_close.
 */
CONST char **sqlo_values __P(( sqlo_stmt_handle_t sth, int * num, int dostrip ));



/**
 * Get the length of the returned values
 *
 * Returns the length in number of characters (bytes for non-unicode chars)
 * for a dataset fetched by sqlo_fetch.
 *
 * @param sth I - A statement handle
 * @param num O - A destination where the function can write the size of the returned
 * array (optional).
 *
 * @return A pointer to an array of unsigned shorts containing the lengths
 *
 * @see sqlo_fetch, sqlo_values, sqlo_open2, sqlo_close.
 */
CONST unsigned int * sqlo_value_lens __P(( sqlo_stmt_handle_t sth, int * num));

/**
 * Get the length of the returned lobs
 *
 * Returns the length in number of characters (bytes for non-unicode chars)
 * for a dataset fetched by sqlo_fetch.
 *
 * @param sth I - A statement handle
 * @param num O - A destination where the function can write the size of the returned
 * array (optional).
 *
 * @return A pointer to an array of unsigned shorts containing the lengths
 *
 * @see sqlo_fetch, sqlo_values, sqlo_open2, sqlo_close.
 */
CONST unsigned long * sqlo_lob_lens __P(( sqlo_stmt_handle_t sth, int * num));


/**
 * Get the select list columns
 *
 * Use this function to get the select list column names.
 * Most usefull for dynamic sql, where you don't know the sql statement at all.
 *
 * @deprecated  For new developments please use @ref sqlo_ocol_names2
 * @param sth I - A statement handle
 * @param num O - A destination where the function can write the size of the returned
 * array (optional).
 *
 * @return A pointer to an array of strings containing the column names,
 *         NULL on error
 *
 * @see sqlo_fetch, sqlo_values, sqlo_open2, sqlo_close, sqlo_ocol_name_lens.
 */
CONST char **sqlo_ocol_names __P(( sqlo_stmt_handle_t sth, int * num));



/**
 * Get the select list columns
 *
 * Use this function to get the select list column names.
 * Most usefull for dynamic sql, where you don't know the sql statement at all.
 *
 * @param sth I - A statement handle
 * @param num O - A destination where the function can write the size of the returned
 * @param ocol_names O - The address of a char ** which receives the array of ocol_names.
 * array.
 *
 * @return SQLO_SUCCESS or <0 on error.
 *
 */
int sqlo_ocol_names2 __P(( sqlo_stmt_handle_t sth, int * num, const char *** ocol_names));



/**
 * Get the select list columns name lengths
 *
 * Use this function to get the length of each select list column.
 * Call this when you need the length of the column, for formatting purposes etc.
 *
 * @param sth I - A statement handle
 * @param num O - A destination where the function can write the size of the returned
 * array.
 *
 * @return A pointer to an array of integers containing the lengths
 *
 * @see sqlo_ocol_names, sqlo_fetch, sqlo_open2, sqlo_close.
 */
CONST int *sqlo_ocol_name_lens __P(( sqlo_stmt_handle_t sth, int * num));



/**
 * Get the number of bind/select-list variables
 *
 * @param sth I - A statement handle
 * @param in  I - 1 returns the number of bind variables, 0 returns the number of
 *                select list columns.
 *
 * @return <ul>
 * <li>The number of columns
 * <li>SQLO_INVALID_STMT_HANDLE
 * </ul>
 */
int sqlo_ncols __P((sqlo_stmt_handle_t sth, int in));


/**
 * Fetch the next row from the resultset and return the columns.
 *
 * @param sth         I - A statement handle.
 * @param ncols       O - The number of ouput columns
 * @param values      O - The column values array
 * @param value_lens  O - The value lengths array (leave NULL if you are not interested in)
 * @param colnames    O - The column names array (leave NULL if you are not interested in)
 * @param colname_lens O - The column name lengths array (leave NULL if you are not interested in)
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_INVALID_STMT_TYPE if sth is not a query
 * <li>OCI status code
 * </ul>
 * @par Example:
 * @include ex20.c
 */
int
sqlo_query_result __P(( sqlo_stmt_handle_t    sth,
			unsigned int *        ncols,
			char ***              values,
			unsigned int **       value_lens,
			char ***              colnames,
			unsigned int **       colname_lens
			));

/**
 * Return the sql command
 *
 * @return The active sql statement of the sth.
 *
 * @param sth I - A statement handle
 */
CONST char *sqlo_command __P(( sqlo_stmt_handle_t sth ));



/**
 * Close the cursor
 *
 * Closes the cursor and releases the Oracle statement handle.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>< 0 on error
 * </ul>
 *
 * @see sqlo_open.
 *
 */
int sqlo_close __P(( sqlo_stmt_handle_t sth ));



/**
 * Execute a simple sql statement
 *
 * Use this to execute non-qeuery statements without bind variables.
 *
 * @param dbh A database handle
 * @param stmt A sql statement
 *
 * @return The number of processed rows (DML statements), 0 (non DML statements)
 *         or < 0 on error.
 *
 * @par Example:
 * @include ex8.c
 *
 * @see sqlo_run
 */
int sqlo_exec __P(( sqlo_db_handle_t dbh, CONST char * stmt, unsigned int  *rr ));



/**
 * Test if a cursor is open
 *
 * @param sth  I - A statement handle.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS  if the cursor is open
 * <li>1 if not (unused sth)
 * <li>SQLO_INVALID_STMT_HANDLE.
 * </ul>
 * @see sqlo_open2
 */
int sqlo_isopen __P((sqlo_stmt_handle_t sth));



/**
 * Return the number of processed rows by this statement
 *
 * @param sth I - A statement handle
 *
 * @return <ul>
 * <li>Number of processed rows
 * <li> 0 if it is not a dml statement
 * <li> < 0 on error
 * </ul>
 *
 */
int sqlo_prows __P(( sqlo_stmt_handle_t sth ));

/** @} */



/**
 * @defgroup loginout Functions to do login/logout to/from a database
 * @{
 */



/**
 * Connect to a database
 *
 * This is the short form of @ref sqlo_server_attach followed by @ref sqlo_session_begin
 *
 * @param dbhp        O - The database handle
 * @param cstr        I - A Oracle connect string.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * @include examples.c
 * @see sqlo_finish, sqlo_server_attach, sqlo_session_begin
 */
int sqlo_connect __P(( sqlo_db_handle_t * dbhp, CONST char * cstr ));



/**
 * Finish the session
 *
 * Finish the session with implicit commit.
 * This is the short form of @ref sqlo_session_end followed by @ref sqlo_server_detach.
 *
 * @param dbh I - A database handle
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_connect, sqlo_session_end, sqlo_server_detach, sqlo_server_attach
 */
int sqlo_finish __P((sqlo_db_handle_t dbh ));



/**
 * Split an Oracle connect string
 *
 * Splits an Oracle connect string of the form uid[[/pwd]\@tnsname] into its
 * components. If no tnsname is found in the cstr, we copy the value of the
 * env. variable ORACLE_SID into tnsname.
 *
 * @param cstr     I - A connect string to split
 * @param uid      O - The returned uid part.
 * @param pwd      O - The returned pwd part.
 * @param tnsname  O - The returned tnsname.
 * @param bufsize  I - The capacity of the output buffers.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> SQLO_ERROR (buffer too small)
 * </ul>
 * @since Version 2.2
 */
int sqlo_split_cstring __P((CONST char * cstr,
                            char * uid,
                            char * pwd,
                            char * tnsname,
                            unsigned int bufsize));



/**
 * Attach to a database server
 *
 * Attaches to a database without creating a session.
 * tnsname can be a database name or a connect string. The function extracts
 * the database name. If no database name is supplied, the function attaches
 * to the database given in the env. variable ORACLE_SID.
 *
 * @param dbhp    O - The database handle
 * @param tnsname O - The tnsname or the complete Oracle connect string.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_session_begin, sqlo_server_detach
 * @since Version 2.2
 */
int sqlo_server_attach __P((sqlo_db_handle_t * dbhp, CONST char * tnsname));



/**
 * Begin a session
 *
 * Do the login to an attached server.
 * You can either pass username and password seperatly, or pass the complete
 * connect string in username.
 *
 * @param dbh      I - A database handle
 * @param username I - The username for authentication, or a complete Oracle
 *                     connect string.
 * @param password I - The password for authentication
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_server_attach, sqlo_session_end
 * @since Version 2.2
 */
int sqlo_session_begin __P((sqlo_db_handle_t dbh, CONST char * username, CONST char * password));



/**
 * Detach from server.
 *
 * Closes all open sessions and detaches from the server.
 *
 *
 * @param dbh I - A database handle
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_server_attach
 * @since Version 2.2
 */
int sqlo_server_detach __P((sqlo_db_handle_t dbh));



/**
 * Free a server connection
 * This is your emergency exit when a connection to a database gets lost (end of file
 * on communication channel).
 * You cannot free the libsqlora8 resources in such a case by sqlo_session_end or
 * sqlo_server_detach, because the OCI statement OCISessionEnd crashes :-(
 * So, if you detect that a connection is broken and you want to clean up the situation
 * and reconnect, call sqlo_server_free to detach from the server and savely free the
 * resources allocated by libsqlora8
 *
 * @param dbh I - A database handle
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @since Version 2.3
 */
int sqlo_server_free __P((sqlo_db_handle_t dbh));



/**
 * End a session
 *
 * Does a logout, but does not detach from the server. It is possible to create a
 * new session via @ref sqlo_session_begin.
 *
 * @attention Closing a session this way, means also to close all the cursors.
 * Oracle is doing an implicit commit. This is maybe not be what you want.
 *
 * @param dbh I - A database handle
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_session_begin
 * @since Version 2.2
 */
int sqlo_session_end __P((sqlo_db_handle_t dbh));



/**
 * Returns the tnsname
 *
 * Returns the tnsname (or service name) of the given dbh.
 * @attention The function returns the database name passed to
 * @ref sqlo_connect or @ref sqlo_server_attach,
 * not the real SID, which can be different!
 *
 * @param dbh I - A database handle
 *
 * @return The tnsname or NULL in case of an invalid dbh
 */
CONST char * sqlo_getdatabase __P((sqlo_db_handle_t dbh ));

/** @} */



/**
 * @defgroup transactions Transaction control functions
 * @{
 */



/**
 * Commit
 *
 * Execute a commit on this database.
 *
 * @param dbh I - A database handle.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 */
int sqlo_commit __P((sqlo_db_handle_t dbh));



/**
 * Rollback
 *
 * Execute a rollback on this database.
 *
 * @param dbh I - A database handle.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 */
int sqlo_rollback __P((sqlo_db_handle_t dbh));


/**
 * sqlo_set_autocommit
 *
 * Set autocommit on the attached server <b>on</b> or <b>off</b>.
 *
 * <b>Autocommit is off by default.</b>
 * @note If you change the setting, it is valid
 * until you detach from the server (sqlo_server_detach or sqlo_finish).
 * If you stay attached and change the session via sqlo_session_end and
 * sqlo_session_begin, the autocommit setting is the same as before.
 *
 * @param dbh I - A database handle.
 * @param on  I - SQLO_ON or SQLO_OFF
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_INVALID_DB_HANDLE
 * </ul>
 * @see sqlo_autocommit
 * @since Version 2.3.2
 */
int sqlo_set_autocommit __P((sqlo_db_handle_t dbh, int on));


/**
 * @def sqlo_autocommit_on
 * A macro used to set autocommit <b>on</b>.
 * executes sqlo_set_autocommit( dbh, SQLO_ON )
 *
 * @param _dbh I - A database handle.
 *
 * @see sqlo_set_autocommit
 * @since Version 2.3.2
 */
#define sqlo_autocommit_on(_dbh) sqlo_set_autocommit(_dbh, SQLO_ON)


/**
 * @def sqlo_autocommit_off
 * A macro used to set autocommit <b>off</b>.
 * executes sqlo_set_autocommit( dbh, SQLO_OFF )
 *
 * @param _dbh I - A database handle.
 *
 * @see sqlo_set_autocommit
 * @since Version 2.3.2
 */
#define sqlo_autocommit_off(_dbh) sqlo_set_autocommit(_dbh, SQLO_OFF)



/**
 * sqlo_autocommit
 *
 * Returns the state of the autocommit flag. Either SQLO_ON or SQLO_OFF
 *
 * @param dbh I - A database handle.
 *
 * @return <ul>
 * <li>SQLO_ON
 * <li>SQLO_OFF
 * <li>SQLO_INVALID_DB_HANDLE
 * </ul>
 *
 * @see sqlo_set_autocommit
 * @since Version 2.3.2
 */
int sqlo_autocommit __P((sqlo_db_handle_t dbh));

/** @} */

/**
 * @defgroup complex The advanced interface.
 *
 * This functions offer more flexibility in terms of
 * datatypes, but they need (much) more parameters.
 * Use these functions if you want to execute PL/SQL or stored procedures.
 *
 * @{
 */



/**
 * Parse a statement
 *
 * This functions must be used to parse a statement if you want to bind manually
 * the parameters. By doing this you can use native datatypes.
 * This is the more complex form of @ref sqlo_open2.
 *
 * If the stmt is a SELECT statement, the function sets the attribute
 * OCI_ATTR_PREFETCH rows to the max arraysize parameter of the library.
 * This is a kind of internal array fetch Oracle provides to speed up the fetching.
 *
 * @param dbh  I - A database handle
 * @param stmt I - sql statement.

 * @return <ul>
 * <li>A new statement handle
 * <li> < 0 on error.
 * </ul>
 *
 * @see sqlo_bind_by_name, sqlo_bind_by_pos, sqlo_define_by_pos, sqlo_open2.
 */
int sqlo_prepare __P((sqlo_db_handle_t dbh, CONST char * stmt));



/**
 * Bind a variable by name
 *
 * Use this to bind a variable in a query or a stored procedure call.
 *
 * If is_array is 1, the parameters param_addr and ind_addr must point to arrays.
 * ind_addr is optional and can be passed a NULL.
 * The param_size is still the size of one array element, not the whole array size!
 *
 * @param sth        I - The statement handle.
 * @param name       I - The bind parameter name.
 * @param param_type I - The datatype of the bind parameter (see @ref sqlo_data_types).
 * @param param_addr I - The address of a variable or array.
 * @param param_size I - The size of the object at param_addr in bytes.
 * @param ind_addr   I - The pointer to the NULL indicator variable (optional).
 * @param is_array   I - 1 means param_addr points to an array, 0 means a single variable.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * @include ex9.c
 *
 * @see sqlo_prepare, sqlo_bind_by_pos, sqlo_define_by_pos
 */
int sqlo_bind_by_name __P((sqlo_stmt_handle_t sth,
                           CONST char * name,
                           int param_type,
                           CONST void * param_addr,
                           unsigned int param_size,
                           short * ind_addr,
                           int is_array));



/**
 * Bind a REF CURSOR
 *
 * Binds a ref cursor and returns a new sth, which you can use to retrieve
 * the data.
 * @note You can also use @ref sqlo_bind_by_name and supply as SQLOT_RSET as type
 * and the address of the new statement handle as param_addr. All other parameters
 * are ignored.
 *
 * @param sth         I - The statement handle
 * @param cursor_name I - The bind name of the cursor
 * @param sth2p       O - The new statement handle for the ref cursor.
 *
 * @par Example:
 * Example using sqlo_values to get the result:
 * @include ex17.c
 * Example using bind variables:
 * @include ex18.c
 */
int sqlo_bind_ref_cursor __P((sqlo_stmt_handle_t sth, CONST char * cursor_name, int * sth2p));



/**
 * Bind a variable by position
 *
 * If is_array is 1, the parameters param_addr and ind_addr must point to arrays.
 * ind_addr is optional and can be passed a NULL.
 * The param_size is still the size of one array element, not the whole array size!
 *
 * @param sth        I - The statement handle
 * @param position   I - The bind parameter position in the string. Starts with
 *                       1 for the first.
 * @param param_type I - The datatype of the bind parameter (see @ref sqlo_data_types).
 * @param param_addr I - The pointer to the parameter data.
 * @param param_size I - The size of the object at param_addr in bytes.
 * @param ind_addr   I - The pointer to the NULL indicator variable (optional).
 * @param is_array   I - 1 means param_addr points to an array, 0 means a single variable.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * @include ex10.c
 * @see sqlo_prepare, sqlo_bind_by_name, sqlo_define_by_pos
 */
int sqlo_bind_by_pos __P((sqlo_stmt_handle_t sth,
                          int position,
                          int param_type,
                          CONST void * param_addr,
                          unsigned int param_size,
                          short * ind_addr,
                          int is_array));



/**
 * Bind a variable by position
 *
 * Bind the input variables. This new version supports arrays of structures. Set
 * the skip_size to the size of the structure. rcode and ind must be part of
 * the structure.
 *
 * @param sth        I - The statement handle
 * @param position   I - The bind parameter position in the string. Starts with
 *                       1 for the first.
 * @param param_type I - The datatype of the bind parameter (@ref sqlo_data_types).
 * @param param_addr I - The pointer to the parameter data.
 * @param param_size I - The size of the object at param_addr in bytes.
 * @param ind_addr   I - The pointer to the NULL indicator variable (optional).
 * @param rcode_addr I - The pointer to the variable that should return the column level
 * @param skip_size  I - In case into an array of structures, set to sizeof(your_struct),
 *                       otherwise set it to 0.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_prepare, sqlo_bind_by_name, sqlo_define_by_pos
 * @since Version 2.2
 */
int sqlo_bind_by_pos2 __P((sqlo_stmt_handle_t sth,
                           int position,
                           int param_type,
                           CONST void * param_addr,
                           unsigned int param_size,
                           short * ind_addr,
                           unsigned short * rcode_addr,
                           unsigned int skip_size));



/**
 * Define a output variable of the select list
 *
 * Use this to define the output variables.
 *
 * If is_array is 1, the parameters value_addr, rlen_addr and ind_addr must point to
 * arrays.
 * ind_addr is optional and can be passed a NULL. Passing NULL is only usefull for
 * NOT NULL columns. If you ommit the indicator and a NULL is fetched, @ref sqlo_execute
 * will fail with an Oracle error (FETCHED COLUMN VALUE IS NULL).
 *
 * The value_size is still the size of one array element, not the whole array size!
 *
 * @param sth         I - The statement handle
 * @param value_pos   I - The bind parameter position in the string. Starts with
 *                        1 for the first.
 * @param value_type  I - The datatype of the bind parameter (@ref sqlo_data_types).
 * @param value_addr  I - The pointer to the parameter data.
 * @param value_size  I - The size of the object at param_addr in bytes.
 * @param ind_addr    I - The pointer to the NULL indicator variable (optional).
 * @param rlen_addr   I - The pointer where @ref sqlo_execute writes the actual returned
 *                        length.
 * @param is_array   I - 1 means param_addr points to an array, 0 means a single variable.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * @include ex11.c
 *
 * @see sqlo_prepare, sqlo_bind_by_name, sqlo_define_by_pos
 */
int sqlo_define_by_pos __P((sqlo_stmt_handle_t sth,
                            int value_pos,
                            int value_type,
                            CONST void * value_addr,
                            unsigned int value_size,
                            short * ind_addr,
                            unsigned int * rlen_addr,
                            int is_array));



/**
 * Define a output variable of the select list
 *
 * Use this to define where the result of the query should go.
 * This new version supports filling arrays of structures.
 * If skip_size is not 0, the parameter value_addr must point to an array of structures.
 * If used, the structure must contain variables for ind, rlen and rcode.
 *
 * The value_size is still the size of one array element, not the whole array size!
 *
 * @param sth        I - The statement handle
 * @param value_pos  I - The bind parameter position in the string.
 *                       Starts with 1 for the first.
 * @param value_type I - The datatype of the bind parameter (@ref sqlo_data_types).
 * @param value_addr I - The pointer to the parameter data.
 * @param value_size I - The size of the object at param_addr in bytes.
 * @param ind_addr   I - The pointer to the NULL indicator variable (optional).
 * @param rlen_addr  I - The pointer where library puts the actual return length.
 * @param rcode_addr I - The address where the library puts the return code for the column
 * @param skip_size  I - In case into an array of structures, set to sizeof(your_struct),
 *                       otherwise set it to 0.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @par Example:
 * This is an example of a fetch into an array of structure.
 * @include ex12.c
 *
 * @see sqlo_prepare, sqlo_bind_by_name, sqlo_define_by_pos, sqlo_execute
 * @since Version 2.2
 */
int sqlo_define_by_pos2 __P((sqlo_stmt_handle_t sth,
                             int value_pos,
                             int value_type,
                             CONST void * value_addr,
                             unsigned int value_size,
                             short * ind_addr,
                             unsigned int * rlen_addr,
                             unsigned short * rcode_addr,
                             unsigned int skip_size));



/**
 * Define a nested table
 * Please visit the example for details.
 * @note You can also use @ref sqlo_define_by_pos with type == SQLOT_RSET
 * and passing the address of the new sth as value_addr and set all other
 * parameters to 0.
 *
 * @param sth         I - The statement handle
 * @param pos         I - The define position of the nested table
 * @param sth2p       O - The new statement handle for the nested table.
 * @par Examples:
 * @include ex19.c
 */
int sqlo_define_ntable __P((sqlo_stmt_handle_t sth, unsigned int pos, int * sth2p));



/**
 * Execute a statement
 *
 * Execute a PL/SQL block or a statement after you prepared it with sqlo_prepare and bound input
 * and output variables.
 * If you are fetching into arrays, you can set iterations to the actual array
 * size. For PL/SQL blocks it must be set to 1.
 *
 * @param sth        I - A parsed statement handle
 * @param iterations I - How many times the statement should be exectuted.
 *                       Must be 1 if you execute a PL/SQL block
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_NO_DATA
 * <li>SQLO_STILL_EXECUTING (non-blocking)
 * <li> < 0 on error
 * </ul>
 *
 *
 * @see sqlo_prepare, sqlo_define_by_pos, sqlo_define_by_pos2, sqlo_bind_by_name
 */
int sqlo_execute __P((sqlo_stmt_handle_t sth, unsigned int iterations));
int sqlo_executeselect __P((sqlo_stmt_handle_t sth, unsigned int iterations));

/** @} */



/**
 * @defgroup lob Functions to insert/select LOBs
 *
 * @{
 */



/**
 * Allocate a lob descriptor
 *
 * @param dbh    I - A database handle
 * @param loblpp O - The lob locator
 *
 * @return SQLO_SUCCESS or < 0 on error
 * @since Version 2.2
 */
int sqlo_alloc_lob_desc __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t *loblpp));



/**
 * Free a lob descriptor
 *
 * Frees the descriptor and sets *loblp to NULL.
 *
 * @param dbh    I - A database handle
 * @param loblpp I/O - A address where we find the lob locator.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 * @since Version 2.2
 */
int sqlo_free_lob_desc __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t *loblpp));



/**
 * Write lob data from buffer into the lob column
 *
 * @param dbh     I - A database handle.
 * @param loblen  I - The length of the lob.
 * @param loblp   I - A lob locator.
 * @param bufp    I - A buffer of data.
 * @param bufl    I - The length of the buffer in terms of bytes.
 * @param piece   I - The piece indicator
 *                    <ul>
 *                    <li>SQLO_ONE_PIECE
 *                    <li>SQLO_FIRST_PIECE
 *                    <li>SQLO_NEXT_PIECE
 *                    <li>SQLO_LAST_PIECE
 *                    </ul>
 *
 * @return <ul>
 *         <li>SQLO_SUCCESS
 *         <li>SQLO_STILL_EXECUTING
 *         <li>SQLO_ERROR
 *         </ul>
 * @since Version 2.2
 * @par Example:
 * @include ex13.c
 * @include ex13b.c
 */
int sqlo_lob_write_buffer __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp, unsigned int loblen,
                               const void *bufp, unsigned int bufl, unsigned int piece));



/**
 * Append lob data from buffer to the lob column
 *
 * @param dbh     I - A database handle.
 * @param loblen  I - The length of the lob.
 * @param loblp   I - A lob locator.
 * @param bufp    I - A buffer of data.
 * @param bufl    I - The length of the buffer in terms of bytes.
 * @param piece   I - The piece indicator
 *                    <ul>
 *                    <li>SQLO_ONE_PIECE
 *                    <li>SQLO_FIRST_PIECE
 *                    <li>SQLO_NEXT_PIECE
 *                    <li>SQLO_LAST_PIECE
 *                    </ul>
 *
 * @return <ul>
 *         <li>SQLO_SUCCESS
 *         <li>SQLO_STILL_EXECUTING
 *         <li>SQLO_ERROR (always when OCILobWriteAppend is not available in your Oracle version)
 *         </ul>
 * @since Version 2.2 and Oracle version >= 8.1
 * @par Example:
 * @include ex13.c
 */
int sqlo_lob_append_buffer __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp,
                                unsigned int loblen,
                                void *bufp, unsigned int bufl,
                                unsigned int piece));



/**
 * Write lob data from a file into the lob column. This function reads the
 * data from the stream and writes it into the lob column via
 * @ref sqlo_lob_write_buffer.
 *
 * @attention The function does not close or rewind the fp.
 *
 * @param dbh     I - A database handle.
 * @param loblp   I - A lob locator.
 * @param filelen I - The size of the file (total lob length)
 * @param fp      I - The filepointer
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_ERROR
 * </ul>
 * @since Version 2.2
 * @par Example:
 * @include ex14.c
 *
 * @see sqlo_lob_write_buffer
 */
int sqlo_lob_write_stream __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp, unsigned int filelen, FILE * fp));



/**
 * Get the length of a lob
 *
 * @param dbh      I - Database handle
 * @param loblp    I - A lob descriptor
 * @param loblenp  O - The length of the lob
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_STILL_EXECUTING
 * <li>SQLO_ERROR
 * </ul>
 */
int sqlo_lob_get_length __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp, unsigned int * loblenp));



/**
 * Read lob data from lob column into a buffer
 *
 * Reads data from the lob and writes it into the supplied buffer.
 *
 * Use @ref sqlo_lob_get_length to get the loblen you have to use here.
 *
 * @param dbh     I - A database handle.
 * @param loblp   I - A lob locator.
 * @param loblen  I - The length of the lob
 * @param bufp    O - The output data.
 * @param bufl    I - The capacity of the buffer in terms of bytes.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_NEED_DATA
 * <li>SQLO_STILL_EXECUTING
 * <li>SQLO_ERROR
 * </ul>
 *
 * @since Version 2.2
 * @par Example:
 * @include ex15.c
 */
int sqlo_lob_read_buffer __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp, unsigned int loblen, void *bufp, unsigned int bufl));



/**
 * Read lob data from lob column into a stream
 *
 * Use @ref sqlo_lob_get_length to get the loblen you have to use here.
 *
 * @param dbh     I - A database handle.
 * @param loblp   I - A lob locator.
 * @param loblen  I - The length of the lob
 * @param fp      I - A filepointer
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_STILL_EXECUTING
 * <li>SQLO_ERROR
 * </ul>
 * @since Version 2.2
 *
 * @par Example:
 * @include ex16.c
 *
 * @see sqlo_lob_read_buffer
 */
int sqlo_lob_read_stream __P((sqlo_db_handle_t dbh, sqlo_lob_desc_t loblp, unsigned int loblen,
                              FILE *fp));
/** @} */



/**
 * @defgroup misc Miscellaneous functions
 * @{
 */



/**
 * Get the specified OCI handle
 *
 * You can get the oci handles here, if you want to call other OCI functions.
 *
 * @param sqloh I - Either a statement or a database handle depending on the handle type
 *                  you want to get.
 * @param ocihp O - The requested OCI handle.
 * @param type  I - The OCI handle type (see @ref sqlo_oci_handle_types_e)
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 *
 * @see sqlo_oci_handle_types_e
 */
int sqlo_get_oci_handle __P((int sqloh, void * ocihp, sqlo_oci_handle_types_e type));



/**
 * Return the database handle of a statement handle.
 *
 * @param sth  I - A statement handle
 * @return <ul>
 * <li>The database handle
 * <li> < 0 on error
 * </ul>
 */
int sqlo_get_db_handle __P((sqlo_stmt_handle_t sth));



/**
 * Set OCI blocking mode on/off.
 *
 * By default a database connection is in blocking mode. This means
 * the call does not return until the task is finished. With this
 * function you can change to non-blocking mode.
 * In this case some functions can return SQLO_STILL_EXECUTING.
 *
 * The functions are:
 * <ul>
 * <li>sqlo_open2 (when called for queries)
 * <li>sqlo_reopen (when called for queries)
 * <li>sqlo_fetch (when called for non-queries)
 * <li>sqlo_exec
 * <li>sqlo_execute
 * </ul>
 * @param dbh I - A database handle where the blocking should be changed.
 * @param on  I - SQLO_ON switches blocking mode on, SQLO_OFF switches to
 *                non-blocking mode
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_INVALID_DB_HANDLE
 * <li> < 0 on error
 * </ul>
 * @since Version 2.2
 */
int sqlo_set_blocking __P((sqlo_db_handle_t dbh, unsigned int on));



/**
 * Get OCI blocking mode
 *
 * Returns the the blocking mode.
 *
 * @param dbh      I - A database handle
 * @param blocking O - SQL_ON if in blocking mode (database default), or SQLO_OFF if
 *                     in non-blocking mode.
 * @return SQLO_SUCCESS or OCI status code.
 * @since Version 2.2
 */
int sqlo_get_blocking __P((sqlo_db_handle_t dbh, unsigned int * blocking));



/**
 * Abort all operations in non-blocking mode
 *
 * This call performs an immediate (asynchronous) abort of any
 * currently executing OCI function that is associated with a connection.
 *
 * If the connection is in blocking mode, SQLO_SUCCESS is returned without
 * doing an abort.
 *
 * The cursor in "SQLO_STILL_EXECUTING" status is closed.
 *
 * @param dbh A database handle
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_ERROR
 * <li>SQLO_INVALID_DB_HANDLE
 * </ul>
 * @since Version 2.2
 */
int sqlo_break __P((sqlo_db_handle_t dbh));



/**
 * Set the prefetch row attribute for a statement.
 * This functions sets the oci attribute OCI_ATTR_PREFETCH_ROWS to the
 * given value.
 * @note This does not affect the global setting passed by the enviroment
 * variable SQLORA_PREFETCH_ROWS.
 * @param sth    I - The statement handle
 * @param nrows  I - The value of the attribute.
 * @return SQLO_SUCCESS or < 0 on error
 * @since Version 2.2
 */
int sqlo_set_prefetch_rows __P((sqlo_stmt_handle_t sth, unsigned int nrows));



/**
 * Get the server version string.
 *
 * Returns the server version string which might look like this:
 @verbatim
   Oracle8i Enterprise Edition Release 8.1.5.0.0 - Production
   With the Partitioning and Java options
   PL/SQL Release 8.1.5.0.0 - Production.
  @endverbatim
    The buffer is null terminated.
  * @param dbh    I - The database handle
  * @param bufp   O - The version string
  * @param buflen I - Supply her the capacity of your buffer.
 * @return SQLO_SUCCESS or < 0 on error.
 * @since Version 2.2
 */
int sqlo_server_version __P((sqlo_db_handle_t dbh, char *bufp, unsigned int buflen));



/**
 * Get the state of the statement
 * @note Don't use this in Oracle versions \< 9.0!
 * @param sth I - A statement handle
 * @return The state (see @ref sqlo_statement_states) or \< 0 n error.
 * @since Version 2.2
 */
int sqlo_get_stmt_state __P((sqlo_stmt_handle_t sth));



/**
 * Get the sql statement text for the statement handle
 * @param sth  I - A statement handle
 * @return The sql text.
 * @since Version 2.2
 */
CONST char * sqlo_get_stmt __P((sqlo_stmt_handle_t sth));



/**
 * Get the datatype of a column in the select list
 * @param sth I - The statement handle
 * @param pos I - The column position (1 based).
 * @return <ul>
 * <li>The datatype (see @ref sqlo_data_types)
 * <li>SQLO_INVALID_STMT_HANDLE
 * <li>SQLO_INVALID_COLPOS
 * </ul>
 * @since Version 2.2
 */
int sqlo_get_ocol_dtype __P((sqlo_stmt_handle_t sth, unsigned int pos));



/**
 * Switches Oracle trace on/off
 *
 * Use this to switch the Oracle trace facility (tkprof) on or off.
 *
 * @param dbh I - A database handle
 * @param on  I - A flag indicating if we switch tracing on (SQLO_ON) or off (SQLO_OFF)
 *
 * @return <ul>
 * <li> SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 */
int sqlo_trace __P((sqlo_db_handle_t dbh, int on ));



/**
 * Print info about the statement to stdout
 *
 * @deprecated  This function will not be enhanced or maintained. This is a kind
 * of debuging code, but better use the builtin trace facility.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li> < 0 on error
 * </ul>
 * @param sth A statement handle
*/
int sqlo_print __P(( sqlo_stmt_handle_t sth ));



/**
 * Register a signal handler for interrupts.
 * Because oracle catches SIGINT, you can register here a handler, which is called by
 * oracle, when SIGINT is catched.
 * @note Don't do any database operation in this handler.
 * @param handle O - The created handle. Needed by @ref sqlo_clear_int_handler to
 *                   clear a signal handler
 * @param signal_handler I - The address of the signal handler to register.
 *
 * @return SQLO_SUCCESS or SQLO_ERROR on error.
 */
int sqlo_register_int_handler __P((int * handle, sqlo_signal_handler_t signal_handler));



/**
 * Clear an interrupt handler
 * @param handle I - The handle created by @ref sqlo_register_int_handler.
 *
 * @return SQLO_SUCCESS or SQLO_ERROR on error.
 */
int sqlo_clear_int_handler __P((int handle));




/**
 * }@
 */

/*-----------------------------------------------------------------------*/
/* Functions to keep backward compatibility                              */
/* Dont use this for new developments                                    */
/*-----------------------------------------------------------------------*/
#ifndef DOXYGEN_SHOULD_SKIP_THIS

int sql_init __P(( void ));

int sql_trace __P(( int on ));

CONST char * sql_geterror __P(( void ));

int sql_geterrcode __P(( void ));

int sql_exists __P((CONST char  * table, CONST char  * field,
                    CONST char * value, CONST char * where ));
int sql_run __P((CONST char * stmt, int argc, CONST char ** argv));

int sql_open __P((CONST char * stmt, int argc,
                                    CONST char ** argv));

int sql_reopen __P((int sth, int argc, CONST char ** argv));

int sql_fetch __P((int sth ));

CONST char **sql_values __P(( int sth, int * num, int dostrip ));

CONST char *sql_command __P(( int sth ));

int sql_close __P(( int sth ));

int sql_print __P(( int sth ));

int sql_finish __P((void));

CONST char * sql_getdatabase __P(( void ));

CONST char ** sql_cnam __P(( int sth, int in, int * num ));

CONST char ** sql_sclen __P(( int sth, int in, int * num ));

int sql_prows __P(( int sth ));

int sql_connect __P(( CONST char  * connect_str ));

int sql_commit __P(( void ));

int sql_rollback __P(( void ));

int sql_count __P((CONST char * table,
                   CONST char * field,
                   CONST char * value,
                   CONST char * where ));

int sql_exec __P(( CONST char * stmt ));

int sql_setparam __P(( int argc , CONST char ** argv));

char CONST ** sql_getparam __P(( int namec, CONST char ** name, int *numvalues ));
int sql_isopen __P((int sth));

int sql_prepare __P((CONST char * stmt));

int sql_bind_by_name __P((int sth, CONST char * name, int param_type,
                          CONST void * param_addr, unsigned int param_size,
                          short * ind_addr, int is_array));

int sql_bind_by_pos __P((int sth, int position, int param_type, CONST void * param_addr, unsigned int param_size, short * ind_addr, int is_array));

int sql_define_by_pos __P((int sth, int value_pos, int value_type,
                           CONST void * value_addr,
                           unsigned int value_size,
                           short * ind_addr,
                           short * rlen_addr,
                           int is_array));

int sql_execute __P((int sth, int iterations));

char CONST **sql_ocol_names __P((int sth, int * num));

int CONST *sql_ocol_name_lens __P((int sth, int * num));

unsigned short CONST * sql_value_lens __P((int sth, int * num));

int sql_ncols __P((int sth, int in));

int sql_getdbh __P((void));

void sqlo_freeall __P((void));

#define  SQLO_DEFDBH  (sql_getdbh())
#endif /* DOXYGEN_SHOULD_SKIP_THIS */

__END_DECLS


#endif

