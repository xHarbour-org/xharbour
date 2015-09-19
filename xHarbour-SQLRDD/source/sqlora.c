/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Oracle native routines
* CONVERTED and (very) EXTENDED from Kai Poitschke's libsql8
* to xHarbour by Marcelo Lombardo (marcelo@xharbour.com.br)
* (c) 2005 - Marcelo Lombardo (marcelo@xharbour.com.br)
*/

/**
 * @file sqlora.c
 * libsqlora8 Implementation
 *
 * @author Kai Poitschke
 *
 * Copyright (c) 1991-2004 Kai Poitschke (kai[_at_]poitschke.de)
 *
 *   This file is part of the libsqlora8 package which can be found
 *   at http://www.poitschke.de/libsqlora8/
 */

/*
 *     Permission to use, copy, modify, and distribute this software for
 *     any purpose with or without fee is hereby granted, provided that
 *     the above copyright notice and this permission notice appear in all
 *     copies.
 *
 *     THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 *     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *     IN NO EVENT SHALL THE AUTHORS AND COPYRIGHT HOLDERS AND THEIR
 *     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *     ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *     OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 *     OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *     SUCH DAMAGE.
 */

const char * _sqlo_sqloraID="$Id$";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "compat.h"

#include <limits.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H            /* for my windows friends  */
#    include <unistd.h>
#endif
#include <ctype.h>
#include <assert.h>

#if defined(ENABLE_WINTHREADS)
#include <windows.h>
#else
# if defined(HAVE_PTHREAD_H)
#include <pthread.h>
# endif
#endif

#if !defined(__GNUC__) && (defined(WIN32) || defined(__BORLANDC__))
#define inline __inline
#define __STDC__ 1
#endif

#include "oci.h"
#include "ociap.h"

/* If glib is enabled, we use the glib allocators */
#ifdef USE_GLIB_ALLOC
#  include "glib.h"
#  define MALLOC g_malloc
#  define REALLOC g_realloc
#  define FREE(_p) {void ** l_p= (void **)&(_p); if ( *l_p != NULL ) {g_free( *l_p); *l_p = NULL; } }
#else
#  define MALLOC malloc
#  define REALLOC realloc
#  define FREE(_p) {void ** l_p= (void**)&(_p); if ( *l_p != NULL ) {free( *l_p); *l_p = NULL; } }
#endif

#define NDEBUG
//#define DEBUG_XGRAB

#include "sqlora.h"
#define HAVE_OCISTMTFETCH2
#define HAVE_OCILOBWRITEAPPEND

#if defined (__STDC__) || defined (_AIX) || defined(PROTOTYPES) ||\
            (defined (__mips) && defined (_SYSTYPE_SVR4)) ||\
             defined(WIN32) || defined(__cplusplus)\

#  define AND ,
#  define DEFUN(name, arglist, args)      name(args)
#  define DEFUN_VOID(name)                name(void)
/*
 * Macro to use instead of "void" for arguments that must have
 * type "void *" in ANSI C;  maps them to type "char *" in
 * non-ANSI systems.
 */
#  define VOID void
#else
#  define AND ;
#  define DEFUN(name, arglist, args)      name arglist args;
#  define DEFUN_VOID(name)                name()
#  define VOID char
#endif

#ifndef NULL
#   define NULL 0
#endif

/*#define TEST_WITH_SIZEOF_RETURNING_ULONG*/

#ifdef TEST_WITH_SIZEOF_RETURNING_ULONG
#define sizeof(_a) (unsigned long)sizeof(_a)
#endif
#define LOGFILE               "oci.log"
/**
 * The macro below looks a bit "ugly" but I added it because libsql8
 * has some few erratic null pointer freeds. So I'll keep this here
 * until I fix this bug :-(
 * Marcelo Lombardo, 2005/04/21
 */

#ifdef DEBUG_XGRAB
#define XFREE(p,i) \
         do { \
            if(p) { TraceLog( LOGFILE, "Pointer %p freed in line %i\n", p, i ); hb_xfree(p); } \
            else \
            TraceLog(LOGFILE, "NULL pointer free at sqlora.c line %i \n", i); \
         } while( 0 )
#else
#define XFREE(p,i) \
         do { \
            if(p) hb_xfree(p); \
            else \
            TraceLog(LOGFILE, "NULL pointer free at sqlora.c line %i \n", i); \
         } while( 0 )
#endif

#if defined(ENABLE_PTHREADS) || defined(ENABLE_ORATHREADS) || defined(ENABLE_WINTHREADS)
/**
 * Defines if the library was compiled with --enable-pthreads
 */
enum {THREADS_ENABLED = 1};
#else
enum {THREADS_ENABLED = 0};
#endif



#ifdef ENABLE_PTHREADS
typedef pthread_t sqlo_thread_t;
typedef pthread_mutex_t sqlo_mutex_t;
#else
#  ifdef ENABLE_ORATHREADS
typedef OCIThreadMutex * sqlo_mutex_t;
typedef OCIThreadId * sqlo_thread_t;
#  else
#    ifdef ENABLE_WINTHREADS
typedef HANDLE sqlo_mutex_t;
typedef unsigned long sqlo_thread_t;
#    else
typedef unsigned long sqlo_mutex_t; /* dummy */
typedef unsigned long sqlo_thread_t; /* dummy */
#    endif
#  endif
#endif



/**
 * @def EXEC_WHEN_THREADING
 * Executes _cmd if library was compiled with threading and initialized in threaded mode
 * @param _cmd I - The code to execute
 */
#define EXEC_WHEN_THREADING(_cmd) \
     if (THREADS_ENABLED && OCI_THREADED == _oci_init_mode) { _cmd }

/**
 * @def UNLOCK_ALL
 * Release all mutex locks
 */
#define UNLOCK_ALL EXEC_WHEN_THREADING(_dbv_unlock(); _env_unlock(); _init_unlock(); )

#define ENCODE_STH(_sth, _dbh) ((int)(_dbh << (sizeof(sqlo_stmt_handle_t)/2 * 8) | _sth))

#define DECODE_STH(_sth) ((ub4) _sth & 0x0000ffff)
#define DECODE_DBH(_sth) ((ub4) (_sth >> (sizeof(sqlo_stmt_handle_t)/2 * 8)) & 0x007fff)

/*-------------------------------------------------------------------------
 * CONSTANTS
 *-----------------------------------------------------------------------*/
/**
 * @enum _sqlora_constants
 */
enum _sqlora_constants {

  SQLO_MAX_DB = 0x00007fff,           /**< max. number of allowed db connections */

  SQLO_MAX_CURSORS = 0x0000ffff,      /**< max. number of allowed cursors per db connection */

  /* changed the MIN_ sizes for better testing (to make sure we do some reallocations) */
  MIN_BINDP = 1 /*64*/,               /**< Initial number of bind pointers
                                 * that were allocated
                                 */

  MIN_DEFNP = 1 /*32*/,               /**< Initial number of define pointers
                                 * that were allocated
                                 */

  MIN_OBUF_SIZE = 1 /*32*/,           /**< Minimum size of the output buffer for columns
                                 * in the select list
                                 */

  MIN_COL_NAME_LEN = 1 /*31*/,        /**< Minimum size for a name or a column */

  MIN_STMT_SIZE =  10 /* 1024*/,        /**< Mininum allocation for sqlo_stmt_t.stmt */

  DEF_PREFETCH_ROWS = 100,      /**< The default number of prefetched rows */
//  DEF_PREFETCH_ROWS = 10,      /**< The default number of prefetched rows */

  SQLO_MAX_ERRMSG_LEN = 2047,   /**< The maximum length of the error message buffer */

  MAX_PATH_LEN = 512,           /**< for the trace file */

  INITIAL_LOB_ALLOC  = 512,

  MAX_VNAME_LEN = 255,          /**< Max variable name len read from the environment */

  MAX_LONG_SIZE = (1024*16),     /**< Max size for LONG output variables */
//#define LIBSQLORA8_TRACE_ENABLED
#ifdef LIBSQLORA8_TRACE_ENABLED
  TRACE_ENABLED = 1      /**< configured with enabled trace */
#else
  TRACE_ENABLED = 0      /**< configured with disabled trace */
#endif


};


/*-------------------------------------------------------------------------
 * MACROS
 *-----------------------------------------------------------------------*/
#undef TRUE
#undef FALSE

/**
 * Boolean TRUE
 */
#define TRUE ((1 == 1))

/**
 * Boolean FALSE
 */
#define FALSE (!TRUE)



/**
 * @def TRACE
 * Execute cmd when the trace level of the library is >= the trace level
 * passed to the macro.
 * @param p_trace_level   I - The min trace level where cmd should be executed
 * @param p_cmd           I - The commands to be executed in case of
 *                            _trace_level >= p_trace_level.
 *
 * @par Example:
 * TRACE(3, fprintf(g_ftp, "Calling foo()\n"););
 */
#define TRACE(p_trace_level, p_cmd)               \
   if ( TRACE_ENABLED &&                          \
       (NULL != _trace_fp) &&                     \
       (_trace_level >= p_trace_level) ) {        \
      { p_cmd }                                   \
      (void) fflush(_trace_fp);                   \
   }

/**
 * @def CHECK_DBHANDLE
 * Checks for a valid dbh and sets the dbp or returns with error.
 * If the db handle is invalid, the macro calls return _errval.
 * @param p_dbp    O - The pointer to the _sqlo_db_t structure for the _dbh.
 * @param p_dbh    I - The database handle to check.
 * @param p_func   I - The callers function name. Used for error message.
 * @param p_errval I - The value to return in case of an error.
 */
#define CHECK_DBHANDLE(p_dbp, p_dbh, p_func, p_errval)           \
{                                                                \
  int l_dbh = p_dbh;                                             \
  CONST char * l_func = p_func;                                  \
  if ( !VALID_DBH_RANGE(l_dbh) ||                                \
       !_dbv[ l_dbh ] ||                                         \
       !_dbv[ l_dbh ]->used) {                                   \
    TRACE(1, fprintf(_trace_fp,                                  \
         "Invalid Database handle %d in %s\n",                   \
          l_dbh, l_func););                                      \
    /* make sure we release all locks */                         \
    UNLOCK_ALL;                                                  \
    return (p_errval);                                           \
  }                                                              \
  p_dbp = _dbv[ l_dbh ];                                         \
}


/**
 * @def CHECK_STHANDLE
 * Checks for a valid sth and sets the stp or returns with error
 * If the st handle is invalid, the macro calls return _errval.
 * @param p_stp    O - The pointer to the _sqlo_stmt_t structure for the _sth.
 * @param p_sth    I - The statement handle to check.
 * @param p_func   I - The callers function name. Used for error message.
 * @param p_errval I - The value to return in case of an error.
 */
#define CHECK_STHANDLE(p_stp, p_sth, p_func, p_errval)          \
{                                                               \
  if ( NULL == (p_stp = _sth2stp( p_sth, p_func ) ) ||          \
       !p_stp->used ) {                                         \
    return p_errval;                                            \
  }                                                             \
}




/**
 * @def CHECK_OCI_STATUS
 * Checks and saves the OCI return status.
 * If the status is != OCI_SUCCESS, the function _save_oci_status is called to
 * save the error message in the database structure.
 *
 * @param p_dbp    I - The pointer to the database structure.
 * @param p_stat   I - The status to check.
 * @param p_action I - The action you executed causing this status
 * @param p_object I - The object on which you did _action.
 */
#define CHECK_OCI_STATUS(p_dbp, p_stat, p_action, p_object)      \
{                                                                \
  sqlo_db_struct_ptr_t l_dbp = p_dbp;                            \
  int l_stat = p_stat;                                           \
  (l_dbp)->status = p_stat;                                      \
  TRACE(4, fprintf(_get_trace_fp(l_dbp),                         \
                   "CHECK_OCI_STATUS[%u]: %d at %d\n",           \
                   l_dbp->dbh, l_stat, __LINE__););              \
  if (OCI_SUCCESS != l_stat &&                                   \
    OCI_STILL_EXECUTING != l_stat) {                             \
    _save_oci_status(l_dbp, p_action, p_object, __LINE__);       \
  }                                                              \
}



/**
 * @def CHECK_OCI_STATUS_RETURN
 * Checks and saves the OCI status and returns the status on failure.
 *
 * Calls CHECK_OCI_STATUS and returns the status if it is != OCI_SUCCESS.
 *
 * @param p_dbp  I - The pointer to the database structure.
 * @param p_stat I - The status to check.
 * @param p_action I - The action you executed causing this status
 * @param p_object I - The object on which you did _action.
 */
#define CHECK_OCI_STATUS_RETURN(p_dbp, p_stat, p_action, p_object) \
{                                                                  \
  sqlo_db_struct_ptr_t l_dbp2 = p_dbp;                             \
  int l_stat2 = p_stat;                                            \
  CHECK_OCI_STATUS(l_dbp2, l_stat2, p_action, p_object);           \
  if (OCI_SUCCESS != l_stat2) {                                    \
    UNLOCK_ALL;                                                    \
    return (l_stat2);                                              \
  }                                                                \
}



/**
 * @def VALID_DBH_RANGE
 * Expression resolves to true, if the passed dbh is in a valid range.
 *
 * @param _dbh  I - The dbh to be checked.
 */
#define VALID_DBH_RANGE(_dbh) ( _dbh >= 0 && _dbh < (int)_dbv_size )


/* If we have usleep we wait this amount of microseconds in a
 * OCI_STILL_EXECUTING loop
 */
#ifdef HAVE_USLEEP
#  define SQLO_USLEEP usleep(1000)
#else
#  define SQLO_USLEEP
#endif

/*-------------------------------------------------------------------------
 * TYPES
 *-----------------------------------------------------------------------*/

/**
 * Boolean type
 */
typedef unsigned char bool_t;



/**
 * Cursor types
 */
typedef enum {
  DEFAULT   = 0,                /**< standard cursor */
  REFCURSOR = 1,                /**< ref cursor type */
  NTABLE    = 2                 /**< nested table cursor */
} sqlo_cursor_type_e;



/**
 * Stores information about the database connection.
 */
typedef struct _sqlo_db_struct {
  struct _sqlo_stmt_struct *stmtv;  /**< The statements (cursors) for this connection */
  unsigned int stmtv_size;          /**< max size of stmtv[] */
  ub4 dbh;                          /**< The db handle used to address this entry */
  OCIServer * srvhp;                /**< OCI server handle */
  OCIError * errhp;                 /**< OCI error handle */
  OCISvcCtx * svchp;                /**< OCI service context handle */
  OCISession * authp;               /**< OCI authorization session handle */
  OCIEnv * envhp;                   /**< pointer to the OCI environment for the thread */
  char * tnsname;                   /**< The TNS name of the database */
  int status;                       /**< The last status code */
  char errmsg[SQLO_MAX_ERRMSG_LEN+1];   /**< The last error message */
  sb4 errcode;                      /**< The last oracle error code */
  OCIStmt * stmthp;                 /**< @ref sqlo_exec stores its stmthp here.
                                     This is not dangerous, because during a non-blocking
                                    OCI call, you cannot open other stmts
                                    */
  /* CONTROL FLAGS */
  bool_t used;                      /**< Flag, if this one is in use or not */
  bool_t attached;                  /**<  not zero, if we are attached to the server */
  bool_t session_created;           /**< not zero, if a session was created */
  FILE * trace_fp;                  /**< connection specific trace file */
  sqlo_thread_t thread_id;          /**< The thread id of the thread who opened this cursor */
  ub4 exec_flags;                   /**< mode flags passed to OCIStmtExecute to facilitate OCI_COMMIT_ON_SUCCESS (@see sqlo_set_autocommit) */
} sqlo_db_struct_t, * sqlo_db_struct_ptr_t;



/**
 * This pointer type defines a pointer to a const sqlo_db_struct_t.
 */
typedef const sqlo_db_struct_t * const_sqlo_db_struct_ptr_t;



/**
 * Stores information about a column in the select list.
 * This structure is not used if you bind the select list manually be
 * @ref sqlo_define_by_pos or @ref sqlo_define_by_pos2.
 */
typedef struct  _sqlo_col_struct {

  ub4 pos;                 /**< The position in the select list (0 based).
                                 * This variable can be used to address the right
                                 * data or ind element in the stmt structure. */
  ub2  dtype;                   /**< The datatype (@ref sqlo_data_types) */
  ub2  database_dtype;          /**< The column datatype in database */
  char * col_name;              /**< The column name */
  ub4 col_name_size;       /**< The max allocated length of col_name */
  ub2  dbsize;                  /**< The size in the database */
#if defined(HB_OS_HPUX) || defined( HB_OS_AIX)
  ub2  prec;                    /**< The precision */
  ub2  scale;                   /** The scale */

#else
  ub1  prec;                    /**< The precision */
  ub1  scale;                   /** The scale */
#endif  
  ub1  nullok;                  /**< Flag: Null allowed */
  sqlo_lob_desc_t loblp;        /**< The LOB descriptor - if column is MEMO */
  struct _sqlo_stmt_struct_t * stp;    /**< link to the stmt structure (see @ref sqlo_stmt_struct_t) */

} sqlo_col_struct_t, * sqlo_col_struct_ptr_t;



/**
 * Stores information about an sql statement.
 * This structure stores all OCI handles plus the data buffers used to
 * retrieve the data.
 */
typedef struct _sqlo_stmt_struct {
  ub4 sth;                      /**< The own handle that identifies this entry. This
                                     is the plain sth, not the encoded one. */
  sqlo_db_struct_ptr_t  dbp;    /**< The link to the database connection (see @ref sqlo_db_struct_t). */
  OCIStmt * stmthp;             /**< The OCI statement handle pointer. */
  char * stmt;                  /**< The sql statement. */
  ub4 stmt_size;           /**< The allocated size of stmt. */

  /* INPUT */
  OCIBind ** bindpv;            /**< The vector of input bind variables. */
  ub4 num_bindpv;          /**< The number of used entries in bindpv[]. */
  ub4 bindpv_size;         /**< The current capacity of size of bindpv[] and indp[]. */
  ub2 stype;                    /**< The OCI Statement type (see oci.h). */
  short * indpv;                /**< Indicator variable vector for input bind variables. */

  /* OUTPUT */
  sqlo_col_struct_t * ocolsv;   /**< The output columns of the select list. */
  OCIDefine ** defnpv;          /**< The OCI define pointers. */
  ub4 num_defnpv;          /**< The current number of output variables. */
  ub4 defnpv_size;         /**< The current size of the arrays defnpv, outv,
                                     outv_size,  oindv, rlenv, ocol_namev_size and
                                     ocol_namev_size. */
  char ** outv;                 /**< The output columns as a vector.
                                     See @ref sqlo_values. */
  ub4 * outv_size;         /**< The current size of an outv[i] element   */
  short * oindv;                /**< The indicator array for output variables */
  ub4 * rlenv;                  /**< The allocated buffer size to the column */
  char ** ocol_namev;           /**< Output column names.
                                     The actual number of filled entries is num_defnpv. */
  ub4 * ocol_namev_size;     /**< The sizes of a colum name  in ocol_namev[]. */
  sqlo_cursor_type_e cursor_type; /**< The cursor type see @ref sqlo_cursor_type_e */
  /* status flags */
  bool_t opened;                  /**< 1 if the cursor is open, 0 if not. */
  bool_t prepared;                /**< 1 if the statement is prepared, 0 if not. */
  bool_t used;              /**< 1 indicates that this structure is in use, 0 if not. */
  bool_t still_executing;   /**< Is 1 in non-blocking mode and a call
                                 to OCIStmtExecute returned OCI_STILL_EXECUTING */
  ub4 num_executions;  /**< number of times the statement was executed  */
  int stmtid;
} sqlo_stmt_struct_t, * sqlo_stmt_struct_ptr_t;



/**
 * This pointer type defines a pointer to a const sqlo_stmt_struct_t.
 */
typedef const sqlo_stmt_struct_t * const_sqlo_stmt_struct_ptr_t;



/**
 * Structure to store parameter name, pointer to the variable
 * and a optional trigger fct, that is executed when parameter is
 * changed.
 * @see g_params.
 */

typedef enum {INTEGER, STRING} vtyp_;

typedef struct {
  char * name;                       /**< parameter name */
  vtyp_ vtyp;                        /**< type of value */
  VOID * value;                      /**< The address of the value */
  int (*trigger_fct) __P((int));     /**< Function that handles this type */
} sqlora_param_t;


/*-------------------------------------------------------------------------
 * EXPORTED GLOBALS
 *-----------------------------------------------------------------------*/

/* define some variables where the user can check the version */
const ub4 sqlo_major_version = LIBSQLORA8_MAJOR_VERSION;
const ub4 sqlo_minor_version = LIBSQLORA8_MINOR_VERSION;
const ub4 sqlo_micro_version = LIBSQLORA8_MICRO_VERSION;
const ub4 sqlo_interface_age = LIBSQLORA8_INTERFACE_AGE;
const ub4 sqlo_binary_age    = LIBSQLORA8_BINARY_AGE;

/* for backward compatibility */
const ub4 sqlora8_major_version = LIBSQLORA8_MAJOR_VERSION;
const ub4 sqlora8_minor_version = LIBSQLORA8_MINOR_VERSION;
const ub4 sqlora8_micro_version = LIBSQLORA8_MICRO_VERSION;
const ub4 sqlora8_interface_age = LIBSQLORA8_INTERFACE_AGE;
const ub4 sqlora8_binary_age    = LIBSQLORA8_BINARY_AGE;


/*-------------------------------------------------------------------------
 * MODULE GLOBAL VARIABLES
 *-----------------------------------------------------------------------*/


/**
 * @var _oci_init_mode
 * The mode of the library.
 * Either OCI_DEFAULT or OCI_THREADED.
 */
static int _oci_init_mode = OCI_DEFAULT; /* the mode we initialize the OCI lib */

static int stmtidcounter = 0; /* How many stps allocated (ML) */


/**
 * @var _max_long_size
 *
 * The maxiumum length of a LONG result.
 * Can be changed by setting the environment variable SQLORA_LONGSIZE.
 * Default is @ref MAX_LONG_SIZE.
 */
static unsigned int _max_long_size = MAX_LONG_SIZE;



/**
 * @var _dbv
 * The array of @ref sqlo_db_struct_t pointers.
 * The array is allocated in @ref sqlo_init with the size of the passed max_db
 * parameter.
 * The array is proteced by the mutex @ref _dbv_mux when compiled with threading
 * enabled.
 */
static sqlo_db_struct_t ** _dbv = NULL; /* array for database connections */



/**
 * @var _dbv_size
 * The size of the @ref _dbv[].
 */
static unsigned int _dbv_size = 0;           /* number of available entries in _dbv[] */



/**
 * @var _max_cursors
 * The maximum number of cursors for one database connection
 */
static unsigned int _max_cursors;



/**
 * @var _env_mux
 * A mutex to protect the call to OCIEnvCreate
 */
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS
     static pthread_mutex_t _env_mux = PTHREAD_MUTEX_INITIALIZER;
#  else
#    ifdef ENABLE_ORATHREADS
       static OCIThreadMutex *_env_mux;
#    else
#      ifdef ENABLE_WINTHREADS
         static HANDLE _env_mux;
#      endif
#    endif
#  endif
#endif



/**
 * @var _dbv_mux
 * A mutex to protect @ref _dbv.
 * @see _dbv_lock, _dbv_unlock
 */
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS
     static pthread_mutex_t _dbv_mux = PTHREAD_MUTEX_INITIALIZER;
#  else
#    ifdef ENABLE_ORATHREADS
       static OCIThreadMutex *_dbv_mux;
#    else
#      ifdef ENABLE_WINTHREADS
         static HANDLE _dbv_mux;
#      endif
#    endif
#  endif
#endif


/**
 * @var _init_mux
 * A mutex to protect @ref _sqlo_init.
 */
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS
     static pthread_mutex_t _init_mux = PTHREAD_MUTEX_INITIALIZER;
#  else
#    ifdef ENABLE_ORATHREADS
       static OCIThreadMutex *_init_mux;
#    else
#      ifdef ENABLE_WINTHREADS
         static HANDLE _init_mux;
#      endif
#    endif
#  endif
#endif



/**
 * @var _init_mux_initialized
 * Flag indicating if the _init_mux was initialized
 */
#ifdef ENABLE_THREADS
static int _init_mux_initialized = 0;            /* Is set to 1 in _init_init_mux */
#endif


/**
 * @var _num_prefetch_rows
 * The number of rows Oracle should prefetch for us.
 * The variable is initialized with @ref DEF_PREFETCH_ROWS, but can
 * be changed by the environment variable SQLORA_PREFETCH_ROWS.
 */
static ub4 _num_prefetch_rows = DEF_PREFETCH_ROWS; /* out prefetch value */



/**
 * @var _sqlo_init
 * A flag indicating if the library was successfully initialized
 */
static int _sqlo_init = 0;                      /* Is set to 1 by sqlo_init */



/**
 * @var _trace_level
 *
 * The trace level of the library.
 * The level is initialized from the environment variable SQLORA_TRACE_LEVEL
 *
 * <ul>
 * <li>0 : Trace disabled (default)
 * <li>1 : Print errors to tracefile
 * <li>2 : Print function calls to tracefile
 * <li>3 : Print more detailed information to tracefile
 * <li>4 : Print also malloc/realloc operations.
 * </ul>
 */
static int _trace_level = 0;
//static int _trace_level = 10;


/**
 * @var _trace_file
 * The name of the trace file.
 * Default is sqlora8.trc, but can be changed by setting the environment
 * variable SQLORA_TRACE_FILE
 */
static char _trace_file[MAX_PATH_LEN+1] = "";



/* These handles are needed by the Oracle OCIThread package.
 * We cannot use the ones in _dbv, because this is the structure
 * we want to protect.
 */
#ifdef ENABLE_ORATHREADS
static OCIEnv *_oci_envhp;
static OCIError *_oci_errhp;
#endif



/**
 * @var DEFAULT_TRACE_FNAME
 * The default trace filename.
 */
static const char * DEFAULT_TRACE_FNAME = "sqlora.log";

static FILE * _trace_fp = NULL;     /**< The filepointer of the global tracefile  */

static unsigned int _session_count = 0; /**< The nubmer of created sessions. Used to name the trace file */

static char _errmsg[SQLO_MAX_ERRMSG_LEN+1]; /**< The variable for error messages when no dbp->errmsg is available */

/*---------------------------------------------------------------------------
 * PROTOTYPES
 *--------------------------------------------------------------------------*/

/* functions needed in threaded mode */
static int _init_mutexes __P((void));
static int _dbv_lock __P((void));
static int _dbv_unlock __P((void));
static int _env_lock __P((void));
static int _env_unlock __P((void));
static int _init_lock __P((void));
static int _init_unlock __P((void));


#ifdef ENABLE_WINTHREADS
static int _winmutex_lock __P((sqlo_mutex_t mp));
static int _winmutex_unlock __P((sqlo_mutex_t mp));
#endif

static int _init_init_mux __P((void));

static int _sqlo_getenv __P((void));

static int _save_oci_status __P((sqlo_db_struct_ptr_t dbp,
                                 const char *action,
                                 const char *object,
                                 int lineno));

static int _bind_argv __P(( sqlo_stmt_struct_ptr_t  stp, unsigned int argc, const char ** argv));

static int _bind_by_pos __P((sqlo_stmt_struct_ptr_t  stp, unsigned int param_pos, int param_type,
                            const void * param_addr, unsigned int  param_size,
                            short * ind_addr, int is_array));

static int _bind_by_pos2 __P((sqlo_stmt_struct_ptr_t  stp, unsigned int param_pos, int param_type,
                            const void * param_addr, unsigned int  param_size,
                            short * ind_addr, unsigned short * rcode_addr,
                               unsigned int skip_size));

static void _strip_string __P((char *s, unsigned int len));

static int _define_ocol_by_pos __P((sqlo_stmt_struct_ptr_t stp, sqlo_col_struct_t *colp,
                                    unsigned int pos));

static int _define_output __P((sqlo_stmt_struct_ptr_t stp));
static int _open_global_trace_file __P((void));

#if 0
static int _close_global_trace_file __P((void));
#endif

static int _open_session_trace_file __P((sqlo_db_struct_ptr_t dbp));
static int _close_session_trace_file __P((sqlo_db_struct_ptr_t dbp));

static sqlo_stmt_struct_ptr_t _get_stmt_ptr __P((const_sqlo_db_struct_ptr_t dbp));
static int _stmt_new __P((sqlo_db_struct_ptr_t dbp, const char * stmt, sqlo_stmt_struct_ptr_t *stpp));
static void _stmt_release __P((sqlo_stmt_struct_ptr_t  stp));
static void _bindpv_reset __P((sqlo_stmt_struct_ptr_t  stp));
static int _stmt_init __P((sqlo_stmt_struct_ptr_t  stp, sqlo_db_struct_ptr_t dbp, const char *stmt));

static const char * _get_stmt_type_str __P((int stype));

static int _is_query __P((sqlo_stmt_struct_ptr_t stp));
static int _is_plsql __P((sqlo_stmt_struct_ptr_t stp));
static int _is_prepared __P((sqlo_stmt_struct_ptr_t stp));
static int _is_opened __P((sqlo_stmt_struct_ptr_t stp));

static const char * _get_data_type_str __P((int dtype));
static sqlo_db_struct_ptr_t _db_add __P((void));
static void _db_release __P((sqlo_db_struct_ptr_t dbp));

static int _define_by_pos __P((sqlo_stmt_struct_ptr_t  stp, unsigned int value_pos,
                               int value_type, const void * value_addr,
                               unsigned int  value_size, short * ind_addr,
                               ub4 * rlen_addr, ub2 * rcode_addr, int is_array));

static int _define_by_pos2 __P((sqlo_stmt_struct_ptr_t  stp, unsigned int value_pos,
                                int value_type, const void * value_addr,
                                unsigned int  value_size, short * ind_addr,
                                ub4 * rlen_addr, ub2 * rcode_addr,
                                unsigned int skip_size));

static  int _calc_obuf_size __P(( unsigned int *bufsizep, unsigned int data_type,
                                  int prec, int scale, unsigned int dbsize));


static int _get_blocking_mode __P((sqlo_db_struct_ptr_t dbp, unsigned * blocking));

static int _get_errcode __P(( sqlo_db_struct_ptr_t dbp));

static int _set_prefetch_rows __P((sqlo_stmt_struct_ptr_t stp, unsigned int nrows));

static const char * _get_stmt_string __P((sqlo_stmt_struct_ptr_t stp));
static int _get_stmt_state __P((sqlo_stmt_struct_ptr_t stp));
static int _alloc_definep __P((sqlo_stmt_struct_ptr_t stp, unsigned int size));
static void _dealloc_definep  __P((sqlo_stmt_struct_ptr_t stp));
static int _alloc_bindp __P((sqlo_stmt_struct_ptr_t stp, unsigned int size));
static void _dealloc_bindp  __P((sqlo_stmt_struct_ptr_t stp));
static void _close_all_db_cursors __P((const_sqlo_db_struct_ptr_t dbp));
static void _close_all_executing_cursors __P((const_sqlo_db_struct_ptr_t dbp));
static FILE * _get_trace_fp __P((const_sqlo_db_struct_ptr_t dbp));
static int _prepare __P((sqlo_stmt_struct_ptr_t stp, const char * stmt, ub2 * stmt_type));
static sqlo_thread_t _get_thread_id __P((void));
static bool_t _thread_id_equal __P(( sqlo_thread_t id1, sqlo_thread_t id2));
static sqlo_stmt_struct_ptr_t _sth2stp __P((int sth, const char *func_name ));
static int _sqlo_reopen __P(( sqlo_stmt_struct_ptr_t stp, int argc,  const char ** argv));

static int _get_ocol_db_data_type __P((sqlo_stmt_struct_ptr_t stp, unsigned int pos, ub2 * dtypep));

static int _get_ocol_db_size __P((sqlo_stmt_struct_ptr_t stp, unsigned int pos, ub2 * sizep));

static int _get_ocol_db_prec __P((sqlo_stmt_struct_ptr_t stp, unsigned int pos, ub1 * precp));

static int _get_ocol_db_scale __P((sqlo_stmt_struct_ptr_t stp, unsigned int pos, ub1 * scalep));

static int _get_ocol_db_is_null __P((sqlo_stmt_struct_ptr_t stp, unsigned int pos, ub1 * is_nullp));

static int _set_ocol_name __P((sqlo_stmt_struct_ptr_t stp, sqlo_col_struct_t * colp, unsigned int pos));

static int _set_all_ocol_names __P((sqlo_stmt_struct_ptr_t stp));

static int _find_free_dbv_entry __P((unsigned int * free_idxp));

static int _db_alloc __P((unsigned int dbv_idx));

static int _stmtv_alloc __P((unsigned int dbv_idx));

static void _terminate_ocols __P((sqlo_stmt_struct_ptr_t stp,
                                  int do_strip_string));

/* Prototypes of functions not present in oracle header files */
int osnsui __P((int * handlep, sqlo_signal_handler_t signal_handler, char * ctx));
int osncui __P((int handle));

/*---------------------------------------------------------------------------
 * END PROTOTYPES
 *--------------------------------------------------------------------------*/

void * hb_xgrabDebug( int iline, HB_SIZE ulSize )
{
   void * pmem;
#ifndef DEBUG_XGRAB
   HB_SYMBOL_UNUSED( iline );
#endif
   pmem = hb_xgrab(ulSize );
#ifdef DEBUG_XGRAB
   TraceLog( LOGFILE, "Pointer %p allocating %" HB_PFS "u bytes in line %i\n", pmem, ulSize, iline );
#endif
   return (pmem);
}

void * hb_xreallocDebug( int iline, void * p, HB_SIZE ulSize )
{
   void * pmem;
#ifndef DEBUG_XGRAB
   HB_SYMBOL_UNUSED( iline );
#endif

#ifdef DEBUG_XGRAB
   TraceLog( LOGFILE, "Pointer %p realloc %" HB_PFS "u bytes in line %i\n", p, ulSize, iline );
#endif
   pmem = hb_xrealloc( p, ulSize );
#ifdef DEBUG_XGRAB
   TraceLog( LOGFILE, "   Pointer %p realloc ok - %i bytes to pointer %p in line %i\n", p, ulSize, pmem, iline );
#endif
   return (pmem);
}

#if defined(__GNUC__) || defined(_MSC_VER) || defined(__BORLANDC__)

/* Sometimes strdup is  not defined in strict ansi mode,
 * even if it is in libc
 */

#ifdef _MSC_VER
#define strdupx hb_strdup
#else
extern char * strdup __P((const char *s));
#define strdupx hb_strdup
#endif

#else

/**
 * strdup if not available on the target system
 * @param s  I - The string to duplicate
 * @return The newly allocated string.
 */
char * DEFUN(strdup, (s), const char * s)
{
  char *n;
  n = (char *) MALLOC(sizeof(char) * (strlen(s) + 1) );
  TRACE(4, fprintf(_trace_fp,"strdup: Allocated %d bytes\n", (strlen(s) + 1)););
  if (n)
    strcpy(n, s);
  return(n);
}

#endif

/*---------------------------------------------------------------------------
  Parameters (Can be changed by environment variables or sqlo_set)
  Naming conventions of environment variables:
  SQLORA_ || upper(<param_name>)
---------------------------------------------------------------------------*/
          /* Parameters, sort them by name */
static sqlora_param_t g_params[] = {
  {"PREFETCH_ROWS", INTEGER,       (VOID *)&_num_prefetch_rows,    NULL},
  {"LONGSIZE",      INTEGER,       (VOID *)&_max_long_size,    NULL},
  {"TRACE_FILE",    STRING,        _trace_file,          NULL},
  {"TRACE_LEVEL",   INTEGER,       &_trace_level,        NULL},
  {NULL,            INTEGER,       NULL,                NULL}
};

/*-------------------------------------------------------------------------
 * STATIC FUNCTIONS
 *-----------------------------------------------------------------------*/



/*-------------------------------------------------------------------------*/
/**
 * Initializes a mutex
 * @returns OCI status.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _mutex_init
#endif
static inline int
DEFUN(_mutex_init, (mutex), sqlo_mutex_t * mutex)
{
  int status = SQLO_SUCCESS;
#ifdef ENABLE_THREADS

#  ifdef ENABLE_PTHREADS

     pthread_mutex_init(mutex, NULL);

#  elif ENABLE_ORATHREADS

     status = OCIThreadMutexInit(_oci_envhp, _oci_errhp, mutex);

#  else
#    ifdef ENABLE_WINTHREADS

       if ( (*mutex = CreateMutex(NULL, FALSE, NULL)) != NULL)
          status = SQLO_SUCCESS;
       else
          status = SQLO_ERROR;

#    endif
#  endif
#else
( void )mutex;
#endif

  return status;
}



/*-------------------------------------------------------------------------*/
/**
 * Lock a mutex
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _mutex_lock
#endif
static inline int
DEFUN(_mutex_lock, (mutex), sqlo_mutex_t * mutex)
{
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS

     pthread_mutex_lock(mutex);

#  elif ENABLE_ORATHREADS

     return(OCIThreadMutexAcquire(_oci_envhp, _oci_errhp, *mutex));

#  elif ENABLE_WINTHREADS

     return (_winmutex_lock(*mutex));
#  endif
#else
( void )mutex;
#endif

  return OCI_SUCCESS;
}
/*-------------------------------------------------------------------------*/
/**
 * Unlock a mutex.
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _mutex_unlock
#endif
static inline int
DEFUN(_mutex_unlock, (mutex), sqlo_mutex_t * mutex)
{
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS

     pthread_mutex_unlock(mutex);

#  elif ENABLE_ORATHREADS

     return (OCIThreadMutexRelease(_oci_envhp, _oci_errhp, *mutex));

#  elif ENABLE_WINTHREADS

     return (_winmutex_unlock( *mutex ));

#  endif
#else
( void )mutex;
#endif

  return(OCI_SUCCESS);
}

/*-------------------------------------------------------------------------*/
/**
 * Inits all mutexes.
 * @returns OCI status.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _init_mutexes
#endif
static inline int
DEFUN_VOID(_init_mutexes)
{
  int status = SQLO_SUCCESS;
#ifdef ENABLE_THREADS

  status = _mutex_init(&_dbv_mux);

  if (OCI_SUCCESS == status )
    status = _mutex_init(&_env_mux);

#endif

  return status;
}

/*-------------------------------------------------------------------------*/
/**
 * Inits _init_mux.
 * @returns OCI status.
 */

#if CC_PRAGMA_INLINE
#pragma INLINE _init_init_mux
#endif
static inline int
DEFUN_VOID(_init_init_mux)
{
  int status = SQLO_SUCCESS;

#ifdef ENABLE_THREADS

  if (_init_mux_initialized)
    return status;


#  ifdef ENABLE_PTHREADS

     pthread_mutex_init(&_init_mux, NULL);

#  elif ENABLE_ORATHREADS

     status = (OCIThreadMutexInit(_oci_envhp, _oci_errhp, &_init_mux));

#  else
#    ifdef ENABLE_WINTHREADS

       if ((_init_mux = CreateMutex(NULL, FALSE, NULL)) != NULL)
         status = SQLO_SUCCESS;
       else
         status = SQLO_ERROR;
#    endif
#   endif

  if (status == SQLO_SUCCESS)
    _init_mux_initialized = 1;

#endif

  return status;
}

#ifdef ENABLE_WINTHREADS
#define MUTEX_WAIT_TIME 30000 /*30 sec*/

/*-------------------------------------------------------------------------*/
/**
 * Lock a windows mutex
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _winmutex_lock
#endif
static inline int
DEFUN(_winmutex_lock, (mp),
      sqlo_mutex_t mp)
{

  int stat;
  int locked = FALSE;

  do {
    stat = WaitForSingleObject(mp, MUTEX_WAIT_TIME);

    if (stat == WAIT_OBJECT_0)
      locked = TRUE;
    else if (stat == WAIT_ABANDONED)
      ;
    else
      locked = FALSE;

  } while (stat == WAIT_ABANDONED);

  return (locked == TRUE) ? OCI_SUCCESS : OCI_ERROR;

  return OCI_SUCCESS;
}
#endif


#ifdef ENABLE_WINTHREADS
/*-------------------------------------------------------------------------*/
/**
 * Unlock a windows mutex
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _winmutex_unlock
#endif
static inline int
DEFUN(_winmutex_unlock, (mp),
      sqlo_mutex_t mp)
{
  ReleaseMutex(mp);
  return OCI_SUCCESS;
}
#endif

/*-------------------------------------------------------------------------*/
/**
 * Lock @ref _env_mux
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _env_lock
#endif
static inline int
DEFUN_VOID(_env_lock)
{
#ifdef ENABLE_THREADS
  return ( _mutex_lock(&_env_mux) );
#else
  return (OCI_SUCCESS);
#endif
}

/*-------------------------------------------------------------------------*/
/**
 * Unlock @ref _env_mux.
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _env_unlock
#endif
static inline int
DEFUN_VOID(_env_unlock)
{

#ifdef ENABLE_THREADS
  return ( _mutex_unlock(&_env_mux) );
#else
  return OCI_SUCCESS;
#endif

}

/*-------------------------------------------------------------------------*/
/**
 * Lock @ref _dbv
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _dbv_lock
#endif
static inline int
DEFUN_VOID(_dbv_lock)
{
#ifdef ENABLE_THREADS
  return ( _mutex_lock(&_dbv_mux) );
#else
  return (OCI_SUCCESS);
#endif
}

/*-------------------------------------------------------------------------*/
/**
 * Unlock @ref _dbv.
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _dbv_unlock
#endif
static inline int
DEFUN_VOID(_dbv_unlock)
{
#ifdef ENABLE_THREADS
  return ( _mutex_unlock(&_dbv_mux) );
#else
  return (OCI_SUCCESS);
#endif
}
/*-------------------------------------------------------------------------*/
/**
 * Lock @ref _init_mux
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _init_lock
#endif
static inline int
DEFUN_VOID(_init_lock)
{
#ifdef ENABLE_THREADS
  if (!_init_mux_initialized)
        _init_init_mux();

  return (_mutex_lock(&_init_mux) );
#else
  return (OCI_SUCCESS);
#endif

}

/*-------------------------------------------------------------------------*/
/**
 * Unlock @ref _init_mux.
 * @return OCI status
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _init_unlock
#endif
static inline int
DEFUN_VOID(_init_unlock)
{
#ifdef ENABLE_THREADS
  return (_mutex_unlock(&_init_mux) );
#else
  return (OCI_SUCCESS);
#endif
}

/*---------------------------------------------------------------------------*/
/**
 * Returns the thread id of this thread.
 * @return The thread id
 */
static inline sqlo_thread_t
DEFUN_VOID(_get_thread_id)
{
#ifdef ENABLE_THREADS
#  ifdef ENABLE_PTHREADS

     return (pthread_self());

#  elif ENABLE_ORATHREADS

     sqlo_thread_t tid = NULL;
     OCIThreadIdInit(_oci_envhp, _oci_errhp, &tid);
     OCIThreadIdGet(_oci_envhp, _oci_errhp, tid);
     return (tid);

#  elif ENABLE_WINTHREADS

     return (0);
#  endif
#endif
  return (0);
}

/*-------------------------------------------------------------------------*/
/**
 * Sets the attribute PREFETCH_ROWS.
 * @param stp   I - statement pointer
 * @param nrows I - Number of rows to prefetch
 * @return OCI status code (dbp->status)
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _set_prefetch_rows
#endif
static inline int
DEFUN(_set_prefetch_rows, (stp, nrows),
      sqlo_stmt_struct_ptr_t  stp AND
      unsigned int nrows)
{
  unsigned int prefetch_rows = nrows;
  sqlo_db_struct_ptr_t dbp;

  assert( stp != NULL );
  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );
  dbp = stp->dbp;

  dbp->status = OCIAttrSet( (dvoid*)stp->stmthp,
                            (ub4)OCI_HTYPE_STMT,
                            &prefetch_rows,
                            (ub4) sizeof(prefetch_rows),
                            (ub4)OCI_ATTR_PREFETCH_ROWS,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_get_stmt_state", "OCIAttrGet");

  return ( dbp->status );
}

/*-------------------------------------------------------------------------*/
/**
 * Gets the statement text and puts it into stp->stmt.
 *
 * @param stp   I - statement pointer
 * @return OCI status code
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _get_stmt_string
#endif
static inline const char *
DEFUN(_get_stmt_string, (stp),
      sqlo_stmt_struct_ptr_t  stp
     )
{
  static const char *nostmt = "_get_stmt_string: No statement avalailable";

  if (!stp->stmt)
    return ( nostmt );

  return stp->stmt;
}


/*-------------------------------------------------------------------------*/
/**
 * Gets the statement state
 *
 * @param stp   I - statement pointer
 * @return The statement state or -1 on error.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _get_stmt_state
#endif
static inline int
DEFUN(_get_stmt_state, (stp),
      sqlo_stmt_struct_ptr_t  stp
     )
{
  ub4 st = 0;
  sqlo_db_struct_ptr_t dbp;

  assert( stp != NULL );
  assert( stp->dbp != NULL );

  dbp = stp->dbp;
  assert( dbp->stmthp != NULL );
  assert( dbp->errhp != NULL );


  /* OCI_ATTR_STMT_STATE is not defined in Oracle < 9i
   * I define this here, to make it compile with Oracle 8,8i,
   * but nobody should call this
   * function
   */
#ifndef OCI_ATTR_STMT_STATE
#define OCI_ATTR_STMT_STATE 182
#endif

  dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                            (ub4)OCI_HTYPE_STMT,
                            (dvoid *)&st,
                            (ub4 *) 0,
                            (ub4)OCI_ATTR_STMT_STATE,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_get_stmt_state", "OCIAttrGet");

  return ((int)st);

}

/*-------------------------------------------------------------------------*/
/**
 * Looks for a free entry in _dbv.
 * First we try to find an already allocated but unused one. If this
 * failes, free_idx points to the first free, but unallocated entry.
 * @param free_idxp  O - Returns the index where we can put in our new
 *                       db.
 * @return SQLO_SUCCESS if free_idx contains the new index, SQLO_ERROR if
 *         no free slots are available anymore.
 */
static int
DEFUN(_find_free_dbv_entry, (free_idx), unsigned int * free_idxp)
{
  register unsigned int dbv_idx;
  int first_null_idx;                       /* first null slot */
  bool_t found_unused;
  int status;

  assert(free_idxp);
  *free_idxp = 0;

  found_unused = FALSE;
  first_null_idx = -1;                /* -1 marks the variable as not set */

  /* Scan the _dbv array until we found an unused slot */
  for (dbv_idx = 0; dbv_idx < _dbv_size && found_unused == FALSE; ++dbv_idx) {

    if ((_dbv[ dbv_idx ]) && (! _dbv[ dbv_idx ]->used)) {  /* free slot found */
      *free_idxp = dbv_idx;
      found_unused = TRUE;

    } else if ( -1 == first_null_idx && !_dbv[ dbv_idx ]) {
      first_null_idx     = dbv_idx;
    }
  }

  /* either we found an unused slot (found_unused == TRUE), or
   * we have and empty slot (first_null_idx >= 0) or we found
   * nothing
   */
  if ( found_unused || first_null_idx >= 0 ) {
    status = SQLO_SUCCESS;

    /* return the index of the first null slot if no unused one was found */
    if ( FALSE == found_unused )
      *free_idxp = first_null_idx;
  } else {
    status = SQLO_ERROR;
  }

  TRACE(4, fprintf(_trace_fp, "_find_free_dbv_entry: status=%d, free_idx=%u "
                   "first_null_idx=%d\n",
                   status, *free_idxp,
                   first_null_idx););
  return status;

}

/*-------------------------------------------------------------------------*/
/** Allocates and initializes a new entry in _dbv[ dbv_idx ]
 * @param dbv_idx I - the index in _dbv where we allocate the new db structure
 * @return SQLO_SUCCESS or SQLO_ERROR on memory allocation error.
 */
static int
DEFUN(_db_alloc, (dbv_idx), unsigned int dbv_idx)
{
  /* allocate a new entry at free_idx position */

  TRACE(3, fprintf(_trace_fp,"_db_alloc: Add new handle at %u\n", dbv_idx););

  _dbv[ dbv_idx ] = (sqlo_db_struct_t *) hb_xgrabDebug( __LINE__, sizeof(sqlo_db_struct_t)) ;

  if (_dbv[ dbv_idx ]) {
    TRACE(4, fprintf(_trace_fp,"_db_add: Allocated %d bytes\n",
                     (int) sizeof(sqlo_db_struct_t)););

    /* initialize the db structure */
    memset(_dbv[ dbv_idx ], 0, sizeof(sqlo_db_struct_t)) ;

  }

  return _dbv[ dbv_idx ] != NULL ? SQLO_SUCCESS : SQLO_ERROR;
}

/*-------------------------------------------------------------------------*/
/** Allocates and initializes the stmt vector _dbv[ dbv_idx ]->stmtv
 *
 * @param dbv_idx   I - the index in _dbv where we allocate the
 *                      new db structure
 *
 * @return SQLO_SUCCESS or SQLO_ERRMALLOC on memory allocation error.
 */
static int
DEFUN(_stmtv_alloc, (dbv_idx), unsigned int dbv_idx)
{
  /* allocate the stmtv arrays in the _dbv */
  if ( NULL == _dbv[ dbv_idx ]->stmtv ) {
    TRACE(4, fprintf(_trace_fp,"_stmtv_alloc: Alloc stmtv for %d cursors at %u\n", _max_cursors, dbv_idx););

    _dbv[ dbv_idx ]->stmtv = (struct _sqlo_stmt_struct *) hb_xgrabDebug( __LINE__,  _max_cursors * sizeof(sqlo_stmt_struct_t) );
    _dbv[ dbv_idx ]->stmtv_size = _max_cursors;
    /* init the memory */
    memset( _dbv[ dbv_idx ]->stmtv, 0, _max_cursors * sizeof(sqlo_stmt_struct_t));
  }

  return SQLO_SUCCESS;
}

/*-------------------------------------------------------------------------*/
/**
 * Adds a new database entry to @ref _dbv[].
 * Alllocates a new entry entry if necessary, or reuses an old one.
 * @return The pointer to the entry. The entry is marked used.
 */
static sqlo_db_struct_ptr_t
DEFUN_VOID(_db_add)
{
  unsigned int free_idx;
  int status;

  TRACE(4, fprintf(_trace_fp, "_db_add starts _dbv_size=%u\n", _dbv_size););

  /* check for initialization */
  if ((! _sqlo_init) || (_dbv_size <= 0))
    return (NULL) ;

  EXEC_WHEN_THREADING(_dbv_lock(););  /* start of critical section */

  status = _find_free_dbv_entry(&free_idx);

  if (status != SQLO_SUCCESS) {        /* no more slots available? */
    EXEC_WHEN_THREADING(_dbv_unlock(););   /* end of critical section */
    return NULL;
  }

  /* If the slot is not allocated yet, we do it first */
  if ( !_dbv[ free_idx ] ) {
    if (SQLO_SUCCESS != _db_alloc(free_idx) ) {
      EXEC_WHEN_THREADING(_dbv_unlock(););   /* end of critical section */
      return NULL;
    }
  }

  /* Mark the slot as used */
  TRACE(3, fprintf(_trace_fp, "_db_add: Using db handle %u\n", free_idx););

  _dbv[ free_idx ]->dbh = free_idx;
  _dbv[ free_idx ]->used = TRUE ; /* set the in use flag *before* releasing the mutex */

  EXEC_WHEN_THREADING( _dbv_unlock(); ); /* end of critical section */

  EXEC_WHEN_THREADING( _dbv[ free_idx ]->thread_id = _get_thread_id(); );


  /* allocate the stmtv arrays in the _dbv */
  if ( NULL == _dbv[ free_idx ]->stmtv ) {
    if (SQLO_SUCCESS != _stmtv_alloc(free_idx)) {
      return (NULL);
    }
  }

  return (_dbv[ free_idx ]) ;
}

/*-------------------------------------------------------------------------*/
/**
 * Releases a statement.
 * Release is done by setting used to 0.
 */
#ifdef CC_PRAGMA_INLINE
#define PRAGMA INLINE _stmt_release
#endif
static inline void
DEFUN(_stmt_release, (stp), sqlo_stmt_struct_ptr_t  stp)
{

  if (stp) {
    stp->used      = FALSE;
    stp->opened    = FALSE;
    stp->prepared  = FALSE;
    stp->still_executing = FALSE;
    stp->num_executions = 0;
  }
}

/*-------------------------------------------------------------------------*/
/**
 * Release a database in _dbv[].
 * Marks it as unused, frees all allocated handles and marks all sths as free.
 *
 * @param dbp - I The pointer to the database structure
 */
static void
DEFUN(_db_release, (dbp), sqlo_db_struct_ptr_t dbp)
{
  unsigned int i;

  if (!dbp)
    return;

  TRACE(2, fprintf(_get_trace_fp(dbp), "_db_release[%u] starts\n", dbp->dbh); );

#ifdef ENABLE_ORATHREADS
  OCIThreadIdDestroy(dbp->envhp, dbp->errhp, &dbp->thread_id);
#endif

  if (dbp->srvhp)
  {
    (void)OCIHandleFree((dvoid *) dbp->srvhp, OCI_HTYPE_SERVER);
  }

  if (dbp->svchp)
  {
    (void)OCIHandleFree((dvoid *) dbp->svchp, OCI_HTYPE_SVCCTX);
  }

  if (dbp->errhp)
  {
    (void)OCIHandleFree((dvoid *) dbp->errhp, OCI_HTYPE_ERROR);
  }

  if (dbp->authp)
  {
    (void)OCIHandleFree((dvoid *) dbp->authp, OCI_HTYPE_SESSION);
  }

  if (dbp->envhp)
  {
    (void)OCIHandleFree((dvoid *) dbp->envhp, OCI_HTYPE_ENV);
  }

  if (dbp->tnsname)
  {
    XFREE(dbp->tnsname, __LINE__);
  }

  dbp->srvhp = NULL;
  dbp->svchp = NULL;
  dbp->errhp = NULL;
  dbp->authp = NULL;
  dbp->envhp = NULL;
  dbp->tnsname = NULL;

  /* close the trace file */
  if (TRACE_ENABLED && _trace_level > 0)
    _close_session_trace_file(dbp);


  dbp->thread_id = 0;
  dbp->errcode = 0;
  *(dbp->errmsg) = '\0';
  dbp->status = 0;

  /* release all sths */
  for (i = 0; i < dbp->stmtv_size; ++i)
  {
    _stmt_release( &(dbp->stmtv[ i ]) );
  }

  if (dbp->stmtv)
  {
     XFREE(dbp->stmtv,__LINE__);
  }
  dbp->stmtv = NULL;

  EXEC_WHEN_THREADING( _dbv_lock(); ); /* start of critical section */

  dbp->stmtv_size = 0;
  dbp->attached = FALSE;
  dbp->session_created = FALSE;
  dbp->used = FALSE;

  EXEC_WHEN_THREADING( _dbv_unlock(); ); /* end of critical section */

}

/*-------------------------------------------------------------------------*/
/**
 * Resets the number of bindpv entries.
 */
static void
DEFUN(_bindpv_reset, (stp), sqlo_stmt_struct_ptr_t  stp)
{
  unsigned int bindp_idx;

  if (stp) {
    for (bindp_idx = 0; bindp_idx < stp->num_bindpv; ++bindp_idx) {
      stp->bindpv[ bindp_idx ] = NULL;
    }

    stp->num_bindpv = 0;

  }

}

/*---------------------------------------------------------------------------*/
/**
 * Compares two thread ids and returns if they are equal
 * @return TRUE if they are equal, FALSE if not.
 */
static inline bool_t
DEFUN(_thread_id_equal, (id1, id2),
      sqlo_thread_t   id1      AND
      sqlo_thread_t   id2 )
{
#if ENABLE_ORATHREADS
  boolean result;
  OCIThreadIdSame(_oci_envhp, _oci_errhp, id1, id2, &result);
  return result ? TRUE : FALSE;
#else
  return id1 == id2 ? TRUE : FALSE;
#endif

}

/*-------------------------------------------------------------------------*/
/**
 * Returns a free stmt struct ptr from  @ref dbp->stmtv[].
 *
 * @param dbp I - The database ptr.
 * @return The new entry, marked as used, or NULL if all are in use
 */
static sqlo_stmt_struct_ptr_t
DEFUN(_get_stmt_ptr, (dbp), const_sqlo_db_struct_ptr_t dbp)
{
  register unsigned stmt_idx;
  bool_t found_free;

  register sqlo_stmt_struct_ptr_t stp;

  TRACE(4, fprintf(_get_trace_fp(dbp),
                   "_get_stmt_ptr: stmtv_size=%d\n", dbp->stmtv_size););

  /* check for initialization */
  if ((! _sqlo_init) || (dbp->stmtv_size <= 0))
  {
    return (NULL) ;
  }

  /* we need to check first if there is an unused allocated slot */
  found_free         = FALSE;

  for (stmt_idx = 0, stp = dbp->stmtv;
       stmt_idx < dbp->stmtv_size;
       ++stmt_idx, ++stp) {

    if ( !stp->used ) {  /* free entry found */
      found_free = TRUE;
      break ;
    }
  }

  TRACE(4, fprintf(_get_trace_fp(dbp),
                   "_get_stmt_ptr: found_free=%d, stmt_idx=%d\n",
                   found_free, stmt_idx););

  if (found_free)
  {

    TRACE(3, fprintf(_get_trace_fp(dbp),
                     "_get_stmt_ptr: Reusing handle %u\n", stmt_idx););
    stp->sth = stmt_idx ;
    stp->used = TRUE ;

  }
  else
  {
    stp = NULL;
  }

  return (stp) ;
}

/*-------------------------------------------------------------------------*/
/**
 * Initializes the stmt structure
 * Sets the dbp into stp and copies stmt into the structure.
 * @return SQLO_SUCCESS or SQLO_ERRMALLOC
 */
static int
DEFUN(_stmt_init, (stp, dbp, stmt),
       sqlo_stmt_struct_ptr_t     stp    AND
       sqlo_db_struct_ptr_t       dbp    AND
       const char *               stmt )
{
  unsigned int len = (unsigned int)strlen(stmt) + 1;

  stp->dbp       = dbp;
  stp->stmthp    = NULL;
  stp->stype     = 0;
  stp->opened    = FALSE;
  stp->prepared  = FALSE;
  stp->still_executing = FALSE;
  stp->num_executions  = 0;
  stp->cursor_type = DEFAULT;

  if( !stp->stmtid )
  {
    stp->stmtid = stmtidcounter ++;
  }

  if (!stp->stmt) {
    if ( MIN_STMT_SIZE > len )
      len = MIN_STMT_SIZE;

    stp->stmt = (char *) hb_xgrabDebug( __LINE__,  sizeof(char) * len );
    stp->stmt_size = len;

  } else if (stp->stmt_size < len ) {

    stp->stmt = (char *) hb_xreallocDebug( __LINE__, stp->stmt, sizeof(char) * len );
    stp->stmt_size = len;
  }

  strcpy(stp->stmt, stmt);

  return SQLO_SUCCESS;
}

/*-------------------------------------------------------------------------*/
/**
 * Creates a new stmt.
 * Allocates the stmt entry via @ref _get_stmt_ptr, allocates the statement handle and
 * initializes the structure.
 * The stmt structure is initialized.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_stmt_new, (dbp, stmt, stpp),
      sqlo_db_struct_ptr_t       dbp    AND
      const char *               stmt   AND
      sqlo_stmt_struct_ptr_t *   stpp )
{
  register sqlo_stmt_struct_ptr_t  stp;

  stp = _get_stmt_ptr(dbp);

  if (! stp) {
    sprintf(dbp->errmsg, "*** FATAL *** : allocation of statement failed") ;
    dbp->status = SQLO_ERRMALLOC;
    return (dbp->status);
  }

  /* init the structure */
  if (SQLO_SUCCESS != (dbp->status = _stmt_init(stp, dbp, stmt))) {
    _stmt_release(stp);
    return (dbp->status);
  }

  /*
   * Allocate the statement handle
   */
  dbp->status = OCIHandleAlloc( (dvoid *) dbp->envhp,
                           (dvoid **) &stp->stmthp,
                           OCI_HTYPE_STMT,
                           (size_t) 0,
                           (dvoid **) 0
                           );

  if (dbp->status != OCI_SUCCESS) {

    _stmt_release( stp );

  } else {

    *stpp = stp;

  }

  return ( dbp->status );
}

/*-------------------------------------------------------------------------*/
/**
 *
 * (Re)allocates space for stp->bindpv[] and stp->indpv[] if necessary.
 * Sets stp->bindpv_size to the new size.
 * Allocates always in steps of MIN_BINDP, to avoid too many reallocations.
 * Sets stp->dbp->status.
 *
 * @param stp  I - The statement pointer.
 * @param size I - The requested size.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS on error.
 * <li>SQLO_ERRMALLOC on failure.
 * </ul>
 */
static int
DEFUN(_alloc_bindp, (stp, size),
      sqlo_stmt_struct_ptr_t  stp    AND
      unsigned int            size )
{
  register int num_new;
  sqlo_db_struct_ptr_t dbp;

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  dbp->status = SQLO_SUCCESS;

  TRACE(4, fprintf(_get_trace_fp(dbp),
                   "_alloc_bindp: alloc sth: %u bindpv_size: %u, req. size: %u\n",
                   stp->sth, stp->bindpv_size, size););

  if (size > stp->bindpv_size) {

    /* allocate MIN_BINDP to avoid a lot of reallocations */
    if (size <= MIN_BINDP)
      size = MIN_BINDP;
    else
      size = 2 * size;

    if ( 0 == stp->bindpv_size ) {    /* complety empty ? */


      TRACE(4, fprintf(_get_trace_fp(dbp),
                       "_alloc_bindp: alloc sth: %u to %u elements\n",
                       stp->sth, size););

      stp->bindpv = (OCIBind **)hb_xgrabDebug( __LINE__,  sizeof(OCIBind*) * size );
      stp->indpv  = (short *)hb_xgrabDebug( __LINE__,  sizeof(short) * size );

    } else {

      TRACE(4, fprintf(_get_trace_fp(dbp),
                       "_alloc_bindpv: realloc sth: %u to %u elements\n",
                       stp->sth, size););

      stp->bindpv = (OCIBind **)hb_xreallocDebug( __LINE__, stp->bindpv, sizeof(OCIBind*) * size );
      stp->indpv  = (short *)hb_xreallocDebug( __LINE__, stp->indpv, sizeof(short) * size );

    }

    /* init the new elements */
    num_new = size - stp->bindpv_size; /* number of elements to be initialized */
    assert( num_new > 0 );

    memset(&stp->bindpv[ stp->bindpv_size ], 0, num_new * sizeof(OCIBind *));
    memset(&stp->indpv[ stp->bindpv_size ],  0, num_new * sizeof(short));
    /* set the new size */
    stp->bindpv_size = size;
  }

  /* checks */
  assert( stp->bindpv_size >= stp->num_bindpv );

  return (dbp->status);
}



/*-------------------------------------------------------------------------*/
/**
 *
 * Deallocates space for stp->bindpv[] and stp->indpv[]
 * Resets stp->bindpv_size and stp->numb_bindp to 0
 *
 * @param stp  I - The statement pointer.
 *
 */
static void
DEFUN(_dealloc_bindp, ( stp ),
      sqlo_stmt_struct_ptr_t  stp )
{
  sqlo_db_struct_ptr_t dbp;

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  TRACE(4, fprintf(_get_trace_fp(dbp),
                   "_dealloc_bindp: dealloc sth: %u bindpv_size: %u\n",
                   stp->sth, stp->bindpv_size););

  if ( stp->bindpv_size > 0 ) {
    assert( stp->bindpv != NULL );
    assert( stp->indpv != NULL );

    XFREE( stp->bindpv, __LINE__ );        /* bind pointer array */
    XFREE( stp->indpv, __LINE__ );                /* indicator var. pointer array */
  }

  stp->bindpv_size = 0;
  stp->num_bindpv = 0;

}



/*-------------------------------------------------------------------------*/
/**
 * (Re)allocates more space for output variables.
 * (Re)allocates more space for stp->ocolsv[], stp->outv,
 * stp->defnpv, stp->ocol_namev_size and stp->ocol_namev.
 * Sets stp->defnpv_size is set to the new size.
 * Allocates always in steps of MINUM_DEFNPV, to avoid too much memory free/allocs
 *
 * @param stp  I - The statement pointer
 * @param size I - The requested size.
 *
 * @return <ul>
 * <li>SQLO_SUCCESS on error.
 * <li>SQLO_ERRMALLOC on failure.
 * </ul>
 */
static int
DEFUN(_alloc_definep, (stp, size),
      sqlo_stmt_struct_ptr_t     stp    AND
      unsigned int               size )
{
  register int num_new;   /* number of new elements in the arrays */
  sqlo_db_struct_ptr_t dbp;

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  dbp->status = SQLO_SUCCESS;

  if (size > stp->defnpv_size) { /* not enough space? */

    /* allocate always a mininum of MIN_DEFNP elements.
     * Beyond this lower bound we take 2*size to avoid lots of reallocation
     * during sqlo_define_by_pos
     */
    if (size <= MIN_DEFNP)
      size = MIN_DEFNP;
    else
      size = 2 * size;

    /* complety empty ? --> MALLOC, else REALLOC*/
    if (0 == stp->defnpv_size )
    {
      TRACE(4, fprintf(_get_trace_fp(dbp),
                       "_alloc_definep: alloc sth: %u for %u columns\n",
                       stp->sth, size););

      stp->defnpv = (OCIDefine **) hb_xgrabDebug( __LINE__,  sizeof(OCIDefine *) * size );
      stp->ocolsv = (sqlo_col_struct_t *) hb_xgrabDebug( __LINE__,  sizeof(sqlo_col_struct_t) * size );
      stp->outv = (char **) hb_xgrabDebug( __LINE__,  sizeof(char *) * size );
      stp->outv_size = (ub4 *) hb_xgrabDebug( __LINE__,  sizeof(ub4) * size );
      stp->oindv = (short *) hb_xgrabDebug( __LINE__,  sizeof(short) * size );
      stp->rlenv = (ub4 *) hb_xgrabDebug( __LINE__,  sizeof(ub4) * size );
      stp->ocol_namev = (char **) hb_xgrabDebug( __LINE__,  sizeof(char *) * size );
      stp->ocol_namev_size = (unsigned int *) hb_xgrabDebug( __LINE__,  sizeof(ub4) * size );
    }
    else
    {
      TRACE(4, fprintf(_get_trace_fp(dbp),
                       "_alloc_definep: realloc sth: %u to %u elements\n",
                       stp->sth, size););

      stp->defnpv = (OCIDefine **) hb_xreallocDebug( __LINE__, stp->defnpv, sizeof(OCIDefine *) * size );
      stp->ocolsv = (sqlo_col_struct_t *) hb_xreallocDebug( __LINE__, stp->ocolsv, sizeof(sqlo_col_struct_t) * size );
      stp->outv = (char **) hb_xreallocDebug( __LINE__, stp->outv, sizeof(char *) * size );
      stp->outv_size = (ub4 *) hb_xreallocDebug( __LINE__, stp->outv_size, sizeof(ub4) * size );
      stp->oindv = (short *) hb_xreallocDebug( __LINE__, stp->oindv, sizeof(short) * size );
      stp->rlenv = (ub4 *) hb_xreallocDebug( __LINE__, stp->rlenv, sizeof(ub4) * size );
      stp->ocol_namev = (char **) hb_xreallocDebug( __LINE__, stp->ocol_namev, sizeof(char *) * size );
      stp->ocol_namev_size = (ub4 *) hb_xreallocDebug( __LINE__, stp->ocol_namev_size, sizeof(ub4) * size );
    }

    /* init the new elements */
    num_new = size - stp->defnpv_size;

    assert( num_new > 0 );

    memset(&stp->defnpv[ stp->defnpv_size ], 0,  sizeof(OCIDefine *) * num_new);
    memset(&stp->ocolsv[ stp->defnpv_size ], 0, sizeof(sqlo_col_struct_t) * num_new);
    memset(&stp->outv[ stp->defnpv_size ], 0, sizeof(char*) * num_new);
    memset(&stp->outv_size[ stp->defnpv_size ], 0, sizeof(ub4) * num_new);
    memset(&stp->oindv[ stp->defnpv_size ], 0, sizeof(short) * num_new);
    memset(&stp->rlenv[ stp->defnpv_size ], 0, sizeof(ub4) * num_new);
    memset(&stp->ocol_namev_size[ stp->defnpv_size ], 0, sizeof(ub4) * num_new);

    /* set new size */
    stp->defnpv_size = size;
  }

  assert( stp->defnpv_size >= stp->num_defnpv);

  return (dbp->status);

}



/*-------------------------------------------------------------------------*/
/**
 *
 * Deallocates space for stp->bindpv[] and stp->indpv[]
 * Resets stp->bindpv_size and stp->numb_bindp to 0
 *
 * @param stp  I - The statement pointer.
 *
 */
static void
DEFUN(_dealloc_definep, ( stp ),
      sqlo_stmt_struct_ptr_t  stp )
{
  sqlo_db_struct_ptr_t dbp;
  unsigned int col_idx;

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  TRACE(4, fprintf(_get_trace_fp(dbp),
                   "_dealloc_definep: dealloc sth: %u defnpv_size: %u\n",
                   stp->sth, stp->defnpv_size););

  if ( stp->defnpv_size > 0 ) {
    assert( stp->defnpv != NULL );
    assert( stp->ocolsv != NULL );
    assert( stp->outv != NULL );
    assert( stp->outv_size != NULL );
    assert( stp->oindv != NULL );
    assert( stp->rlenv != NULL );
    assert( stp->ocol_namev != NULL );
    assert( stp->ocol_namev_size != NULL );

    /* free all column names */
    for ( col_idx = 0; col_idx < stp->defnpv_size; ++col_idx )
    {
      if (stp->ocolsv[ col_idx ].col_name)
      {
         XFREE( stp->ocolsv[ col_idx ].col_name, __LINE__ );
      }

      if (stp->ocolsv[ col_idx ].loblp)
      {
        // TraceLog(LOGFILE, "col %i, OCIDescriptorFree 1 %p\n", col_idx, stp->ocolsv[ col_idx ].loblp );
        OCIDescriptorFree((dvoid **) &(stp->ocolsv[ col_idx ].loblp), (ub4) OCI_DTYPE_LOB);
        stp->ocolsv[ col_idx ].loblp = NULL;
      }

      if (stp->outv[ col_idx ])
      {
         XFREE( stp->outv[ col_idx ], __LINE__ );
         stp->outv[ col_idx ] = NULL;
         stp->outv_size[ col_idx ] = 0;
         stp->rlenv[ col_idx ]     = 0;
      }
    }

    XFREE( stp->defnpv, __LINE__ );
    XFREE( stp->ocolsv, __LINE__ );
    XFREE( stp->outv, __LINE__ );
    XFREE( stp->outv_size, __LINE__ );
    XFREE( stp->oindv, __LINE__ );
    XFREE( stp->rlenv, __LINE__ );
    XFREE( stp->ocol_namev, __LINE__ );
    XFREE( stp->ocol_namev_size, __LINE__ );

    stp->defnpv = NULL;
    stp->ocolsv = NULL;
    stp->outv = NULL;
    stp->outv_size = NULL;
    stp->oindv = NULL;
    stp->rlenv = NULL;
    stp->ocol_namev = NULL;
    stp->ocol_namev_size = NULL;

  }
  stp->defnpv_size = 0;
  stp->num_defnpv  = 0;
}



/*-------------------------------------------------------------------------*/
/**
 * Set our parameters from the environment.
 * @return <ul>
 * <li> SQLO_SUCCESS
 * <li> != 0 on error
 * </ul>
 */
static int
DEFUN_VOID(_sqlo_getenv)
{
  register int i;
  char * ep;
  char vname[MAX_VNAME_LEN+1];

  /* Preset trace file name */
  strcpy(_trace_file, DEFAULT_TRACE_FNAME);

  /*
   * Get the parameters from the environment
   */
  for ( i = 0; g_params[ i ].name ; i++) {
    sprintf(vname, "SQLORA_%s", g_params[ i ].name);

    ep = getenv(vname);

    if ( NULL != ep && strlen(ep) ) {

      switch (g_params[ i ].vtyp) {

      case INTEGER:
        if (g_params[ i ].value)
          *((int*) g_params[ i ].value) = atoi(ep);
        break;

      case STRING:
        if (g_params[ i ].value)
          strcpy( (char *) g_params[ i ].value, ep);
        break;

      default:
        break;
      }

      /* Call the trigger function, if one was defined */
      if (g_params[ i ].trigger_fct)
        if (SQLO_SUCCESS != g_params[ i ].trigger_fct( i ) )
          return SQLO_ERROR;
    }
  }
  return SQLO_SUCCESS;
}



/*-------------------------------------------------------------------------*/
/**
 * Open the global trace file @ref _trace_file.
 *
 * Stores the filepointer in the global variable @ref _trace_fp.
 * @return SQLO_SUCCESS or SQLO_ERROR.
 */
static int
DEFUN_VOID(_open_global_trace_file)
{
  if (_trace_fp)
    return SQLO_SUCCESS;

  if (NULL == (_trace_fp = fopen(_trace_file, "w"))) {
    fprintf(stderr,"Cannot open %s (errno=%d)\n", _trace_file, errno);
    return SQLO_ERROR;
  }

  fprintf(_trace_fp, "\n**** Starting new trace log ****\n");

  return SQLO_SUCCESS;
}



#if 0
/*-------------------------------------------------------------------------*/
/**
 * Close the global trace file @ref _trace_fp.
 * @return SQLO_SUCCESS or SQLO_ERROR.
 */
static int
DEFUN_VOID(_close_global_trace_file)
{
  int stat = SQLO_SUCCESS;
  if (_trace_fp) {
    stat = fclose(_trace_fp);
    _trace_fp = NULL;
  }
  return (0 == stat) ? SQLO_SUCCESS : SQLO_ERROR;
}
#endif



/*-------------------------------------------------------------------------*/
/**
 * Opens the trace file for a session.
 * The trace file is opened, if the @ref _trace_level is set.
 * The trace file name is made unique, by incrementing a @ref _session_count,
 * for each created session. The session count is different from the dbh,
 * because dbh's are recycled, but session counts are unique across the program
 * execution.
 * The trace filepointer is stored in the db structure.
 * @return SQLO_SUCCESS or SQLO_ERROR.
 */
static int
DEFUN(_open_session_trace_file, (dbp), sqlo_db_struct_ptr_t dbp)
{
  char trace_file[MAX_PATH_LEN+1];

  /* Note: we open a session specific trace file, not a dbh specific one, because
   * dbh's may be recycled.
   */
  _session_count++;              /* increase the session counter */

  /* construct the trace filename */
  sprintf(trace_file, "%s%u", _trace_file, _session_count);

  if (NULL == (dbp->trace_fp = fopen(trace_file, "w"))) {
    fprintf(stderr,"Cannot open %s (errno=%d)\n", trace_file, errno);
    return SQLO_ERROR;
  }

  fprintf(_trace_fp, "Starting new trace log for dbh=%u session=%u in file %s\n",
          dbp->dbh, _session_count, trace_file);

  fprintf(dbp->trace_fp,
          "\n**** Starting new trace log for dbh=%u session=%u ****\n",
          dbp->dbh, _session_count);

  return SQLO_SUCCESS;
}



/*-------------------------------------------------------------------------*/
/**
 * Close the session trace file db->trace_fp
 * @return SQLO_SUCCESS or SQLO_ERROR.
 */
static int
DEFUN(_close_session_trace_file, (dbp), sqlo_db_struct_ptr_t dbp)
{
  int stat = SQLO_SUCCESS;

  if (dbp->trace_fp) {
    stat = fclose(dbp->trace_fp);
    dbp->trace_fp = NULL;
  }

  return (0 == stat) ? SQLO_SUCCESS : SQLO_ERROR;
}



/*-------------------------------------------------------------------------*/
/**
 * Saves the error message for dbp->status.
 *
 * This function is called by @ref CHECK_OCI_STATUS_RETURN if dbp->status != 0.
 * If dbp->status is OCI_ERROR, dbp->errcode is set to the Oracle error code.
 * @param dbp    I - The database pointer
 * @param action I - A string identifying the action that lead to the status
 * @param object I - A string identifying the object where the action was executed on.
 * @param lineno I - The line in the source code, where the error occured.
 *
 * @return SQLO_SUCCESS
 */
static int
DEFUN(_save_oci_status, (dbp, action, object, lineno),
      sqlo_db_struct_ptr_t     dbp      AND
      const char *             action   AND
      const char *             object   AND
      int                      lineno )
{
  char errbuf[SQLO_MAX_ERRMSG_LEN+1];
  unsigned int len;

  if (!dbp)
    return 0;

  if (!object)
    object = "";

  *errbuf = '\0';
  dbp->errcode = dbp->status;   /* preset with something usefull */
  TRACE(3,
        if (dbp->status != OCI_SUCCESS) {
          fprintf(_get_trace_fp(dbp),"_save_oci_status: %d\n", dbp->status);
        }
        );

  switch (dbp->status) {

  case OCI_SUCCESS:
    break;

  case OCI_SUCCESS_WITH_INFO:
    (void) OCIErrorGet(dbp->errhp, (ub4) 1, (text *) NULL,
                       &dbp->errcode,
                       (text *)errbuf, (ub4) sizeof(errbuf),
                       OCI_HTYPE_ERROR);
#ifndef NDEBUG
    sprintf(dbp->errmsg,
            "%s\n(line: %d)\n", errbuf, lineno);
#else
    strcpy(dbp->errmsg, errbuf);
#endif

    break;

  case OCI_NEED_DATA:
    sprintf(dbp->errmsg,
            "ERROR: OCI_NEED_DATA\n(line: %d)\n", lineno);
    break;

  case OCI_NO_DATA:
    sprintf(dbp->errmsg,
            "ERROR: OCI_NO_DATA\n(line: %d)\n", lineno);
    break;

  case OCI_ERROR:
    (void)OCIErrorGet(dbp->errhp, (ub4) 1, (text *) NULL, &dbp->errcode,
                       (text *)errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
#ifndef NDEBUG
    sprintf(dbp->errmsg,
            "%s\n(line: %d)\n", errbuf, lineno);
#else
    strcpy(dbp->errmsg, errbuf);
#endif

    break;

  case OCI_INVALID_HANDLE:
    sprintf(dbp->errmsg,
            "ERROR: OCI_INVALID_HANDLE\n(line: %d)\n", lineno);
    break;

  case OCI_STILL_EXECUTING:
    sprintf(dbp->errmsg,
            "ERROR: OCI_STILL_EXECUTING\n(line: %d)\n", lineno);
    break;

  case OCI_CONTINUE:
    sprintf(dbp->errmsg,
            "ERROR: OCI_CONTINUE\n(line: %d)\n", lineno);
    break;

  case SQLO_INVALID_DB_HANDLE:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Invalid database handle.\n(line: %d)\n",
            dbp->status, lineno);
    break;

  case SQLO_INVALID_STMT_HANDLE:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Invalid statement handle.\n(line: %d)\n",
            dbp->status, lineno);
    break;

  case SQLO_STMT_NOT_OPENED:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Cursor is not open.\n(line: %d)\n",
            dbp->status, lineno);
    break;

  case SQLO_STMT_NOT_PARSED:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Stmt is not prepared.\n(line: %d)\n",
            dbp->status, lineno);
    break;

  case SQLO_INVALID_STMT_TYPE:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Sorry, this function cannot handle your type of statement.\n"
            "(line: %d)\n", dbp->status, lineno);
    break;

  case SQLO_INVALID_SQL:
    sprintf(dbp->errmsg,
            "ERROR: %05d: Invalid SQL parsed.\n"
            "(line: %d)\n", dbp->status, lineno);
    break;

  case SQLO_ERRMALLOC:
    /* concatenate the error message. */
    sprintf(&dbp->errmsg[strlen(dbp->errmsg)],
            "ERROR: %05d: Memory allocation error.\n(line: %d)\n", dbp->status,
            lineno);
    break;

  case SQLO_UNSUPPORTED_DATA_TYPE:
    sprintf(&dbp->errmsg[strlen(dbp->errmsg)],
            "ERROR: %05d: Unsupported database data type.\n(line: %d)\n", dbp->status,
            lineno);
    break;

  default:
    sprintf(dbp->errmsg,
            "ERROR: - 00000: Unknown status %d\n(line: %d)\n", dbp->status,
            lineno);
    break;
  }

#ifndef NDEBUG
  if ((len = strlen(dbp->errmsg) + strlen(action) + strlen(object) + 40)
      > SQLO_MAX_ERRMSG_LEN) {
      len = SQLO_MAX_ERRMSG_LEN - strlen(dbp->errmsg) - strlen(action) - 40;
  } else {
    len = (unsigned int) strlen(object);
  }

  if (strlen(action) ) {
    sprintf(&dbp->errmsg[ strlen(dbp->errmsg) ], "\nSQL error while doing %s", action);

    if (len) {
      sprintf(&dbp->errmsg[ strlen(dbp->errmsg) ], " on:\n\"%*.*s\"", (int) len,
               (int) len, object);
    }
  }

  strcat(dbp->errmsg,"\n");
#else
  /* just to keep lint happy */
  {
      char c=0;
      len = 0;
      if ( len > 0) {
        c = *action;
        c = *object;
      }
  }
#endif

  TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););

  return (SQLO_SUCCESS);

}



/*---------------------------------------------------------------------------*/
/**
 * Returns the statement type as a string.
 * @param stype The statement type
 * @return A constant string telling you the kind of statement.
 */
static const char *
DEFUN(_get_stmt_type_str, (stype), int stype)
{
  switch (stype) {

  case OCI_STMT_SELECT:
    return "SELECT";

  case OCI_STMT_UPDATE:
    return "UPDATE";

  case OCI_STMT_DELETE:
    return "DELETE";

  case OCI_STMT_INSERT:
    return "INSERT";

  case OCI_STMT_CREATE:
    return "CREATE";

  case OCI_STMT_DROP:
    return "DROP";

  case OCI_STMT_ALTER:
    return "ALTER";

  case OCI_STMT_BEGIN:
    return "BEGIN";

  case OCI_STMT_DECLARE:
    return "DECLARE";

  default:
    return "UNKNOWN";
  }
}



/*---------------------------------------------------------------------------*/
/**
 * Returns whether the stmt is a query or not, by looking at stp->stype
 * @param stp The statement handle pointer
 * @return 1 if this is a query, 0 if not.
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _is_query
#endif
static inline int
DEFUN(_is_query, (stp), sqlo_stmt_struct_ptr_t stp)
{
  assert(stp);
  return (stp->stype == OCI_STMT_SELECT ? 1 : 0);
}



/*---------------------------------------------------------------------------*/
/**
 * Returns whether the stmt is a PL/SQL block or not, by looking at stp->stype
 * @param stp The statement handle pointer
 * @return 1 if this is a PL/SQL block, 0 if not.
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _is_plsql
#endif
static inline int
DEFUN(_is_plsql, (stp), sqlo_stmt_struct_ptr_t stp)
{
  assert(stp);

  return (stp->stype == OCI_STMT_DECLARE ||
          stp->stype == OCI_STMT_BEGIN ?
          1 : 0);
}



/*---------------------------------------------------------------------------*/
/**
 * Returns whether the stmt was prepared or not
 * @param stp The statement handle pointer
 * @return 1 if it is prepared 0 if not
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _is_prepared
#endif
static inline int
DEFUN(_is_prepared, (stp), sqlo_stmt_struct_ptr_t stp)
{
  assert(stp);

  return (stp->prepared ? 1 : 0);

}



/*---------------------------------------------------------------------------*/
/**
 * Returns whether the stmt was opened or not
 * @param stp The statement handle pointer
 * @return 1 if it is opened 0 if not
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _is_opened
#endif
static inline int
DEFUN(_is_opened, (stp), sqlo_stmt_struct_ptr_t stp)
{
  assert(stp);

  return (stp->opened ? 1 : 0);

}



/*---------------------------------------------------------------------------*/
/**
 * Returns the data type as string
 * @param dtype The data type
 * @return A constant string telling you the data type.
 */
static const char *
DEFUN(_get_data_type_str, (dtype), int dtype)
{
  /* the constants are defined in ocidfn.h */
  switch (dtype)
  {
  case SQLT_CHR: return "character string";
  case SQLT_NUM: return "oracle numeric";
  case SQLT_INT: return "integer";
  case SQLT_FLT: return "floating point number";
  case SQLT_STR: return "zero terminated string";
  case SQLT_VNU: return "num with preceding length byte";
  case SQLT_PDN: return "packed decimal numeric";
  case SQLT_LNG: return "long";
  case SQLT_VCS: return "variable character string";
  case SQLT_NON: return "Null/empty PCC Descriptor entry ";
  case SQLT_RID: return "rowid";
  case SQLT_DAT: return "date in oracle format";
  case SQLT_DATE: return "ANSI Date";
  case SQLT_TIME: return "Time";
  case SQLT_TIME_TZ: return "Time with timezone";
  case SQLT_TIMESTAMP: return "Timestamp";
  case SQLT_TIMESTAMP_TZ: return "Timestamp with timezone";
  case SQLT_TIMESTAMP_LTZ: return "Timestamp with local timezone";
  case SQLT_INTERVAL_YM: return "Interval year to month";
  case SQLT_INTERVAL_DS: return "Interval day to second";
  case SQLT_VBI: return "binary in VCS format";
  case SQLT_BIN: return "binary data(DTYBIN)";
  case SQLT_LBI: return "long binary";
  case SQLT_UIN: return "unsigned integer";
  case SQLT_SLS: return "dispay sign leading separate";
  case SQLT_LVC: return "longer longs (char)";
  case SQLT_LVB: return "longer longs (binary)";
  case SQLT_AFC: return "ansi fixed char";
  case SQLT_AVC: return "ansi var char";
  case SQLT_CUR: return "cursor type";
  case SQLT_RDD: return "rowid descriptor";
  case SQLT_LAB: return "label type";
  case SQLT_OSL: return "oslabel type";
  case SQLT_NTY: return "named object type";
  case SQLT_REF: return "ref type";
  case SQLT_CLOB: return "character lob";
  case SQLT_BLOB: return "binary lob";
  case SQLT_BFILEE: return "binary file lob";
  case SQLT_CFILEE: return "character file lob";
  case SQLT_RSET: return "result set type";
  case SQLT_NCO: return "named collection type";
  case SQLT_VST: return "OCIString type";
  case SQLT_ODT: return "OCIDate type";
  }
  return "UNKNOWN";
}



/*---------------------------------------------------------------------------*/
/**
 * Strips all trailing blanks in s.
 * @param s   I - The string to strip
 * @param len I - The current string length.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _strip_string
#endif
static inline void
DEFUN(_strip_string, (s, len),
      char *        s   AND
      unsigned int  len )
{
  register char *p;

  for ( p = &s[len - 1]; len > 0 && ' ' == *p ; --len)
     *p = '\0';
}



/*---------------------------------------------------------------------------*/
/**
 * Returns the last error code.
 * Saves it also in dbp->errcode
 *
 * @param dbp I - The database pointer.
 * @return The Oracle error code.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _get_errcode
#endif
static inline int
DEFUN(_get_errcode, (dbp), sqlo_db_struct_ptr_t  dbp)
{
  assert( (dbp != NULL) );

  (void)OCIErrorGet(dbp->errhp,
                    (ub4) 1,
                    (text *) NULL,
                    &dbp->errcode,
                    NULL,
                    (ub4) 0,
                    OCI_HTYPE_ERROR);

  return dbp->errcode;
}



/*---------------------------------------------------------------------------*/
/**
 * Binds the input variables.
 * This is the same as @ref sqlo_bind_by_pos, except that the stmt pointer is passed
 * instead of the stmt handle.
 *
 * @return SQLO_SUCCESS or SQLO_ERROR.
 * @see sqlo_bind_by_pos
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _bind_by_pos
#endif
static inline int
DEFUN(_bind_by_pos,
       (stp, param_pos, param_type, param_addr, param_size, ind_addr, is_array),
      sqlo_stmt_struct_ptr_t      stp          AND
      unsigned int                param_pos    AND
      int                         param_type   AND
      const void *                param_addr   AND
      unsigned int                param_size   AND
      short *                     ind_addr     AND
      int                         is_array )
{
  sqlo_db_struct_ptr_t dbp;
  register OCIBind ** bindp_addr;

  TRACE(3,
        fprintf( _get_trace_fp(stp->dbp),
                 "_bind_by_pos pos: %u type: %d (%s), size: %u\n",
                 param_pos, param_type, _get_data_type_str(param_type),
                 param_size);
        );

  assert( param_pos > 0 );
  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL);

  dbp = stp->dbp;

  if ( _is_prepared(stp) ) {
    /* make sure we have enough memory */
    if ( param_pos > stp->bindpv_size) {
      dbp->status = _alloc_bindp(stp, param_pos);

      CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                              "_bind_by_pos. alloc error", "");
    }

    bindp_addr = &stp->bindpv[ param_pos - 1 ];

    dbp->status = OCIBindByPos( stp->stmthp,
                                bindp_addr,
                                dbp->errhp,
                                (ub4) param_pos,
                                (dvoid *) param_addr,
                                (sword) param_size,
                                param_type,
                                (dvoid *) ind_addr,
                                (ub2 *) 0,
                                (ub2) 0,
                                (ub4) 0,
                                (ub4 *) 0,
                                OCI_DEFAULT
                                );

    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_bind_by_pos. Cannot bind", NULL);
    if ( is_array ) {
      dbp->status = OCIBindArrayOfStruct( *bindp_addr,
                                          dbp->errhp,
                                          param_size,
                                          ind_addr ? sizeof(short) : 0,
                                          0,
                                          0
                                          );

      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_bind_by_pos. BindArrayOfStruct", "");
    }

    if (param_pos > stp->num_bindpv)
      stp->num_bindpv = param_pos;

    assert(stp->num_bindpv <= stp->bindpv_size);

  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
  }
  return ( dbp->status );
}



/*---------------------------------------------------------------------------*/
/**
 * Bind input variables.
 * This is the same as @ref sqlo_bind_by_pos2, except that the statement pointer
 * is passed here instead of the statement handle.
 * @return <ul>
 * <li> SQLO_SUCCESS
 * <li> < 0 on error.
 * </ul>
 * @see sqlo_bind_by_pos2
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _bind_by_pos2
#endif
static inline int
DEFUN(_bind_by_pos2,
      (stp, param_pos, param_type, param_addr, param_size, ind_addr, rcode_addr, skip_size),
      sqlo_stmt_struct_ptr_t        stp         AND
      unsigned                      param_pos   AND
      int                           param_type  AND
      const void *                  param_addr  AND
      unsigned int                  param_size  AND
      short *                       ind_addr    AND
      unsigned short *              rcode_addr  AND
      unsigned int                  skip_size )
{
  register OCIBind ** bindp_addr;
  sqlo_db_struct_ptr_t dbp;

  TRACE(3,
        fprintf(_get_trace_fp(stp->dbp),
                "_bind_by_pos pos2: %u type: %d (%s), size: %u\n",
                param_pos, param_type, _get_data_type_str(param_type),
                param_size););

  assert( param_pos > 0 );
  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL);

  dbp = stp->dbp;

  if ( _is_prepared(stp) ) {
    /* make sure we have enough memory */
    if ( param_pos > stp->bindpv_size) {
      dbp->status = _alloc_bindp(stp, param_pos);

      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_bind_by_pos2. alloc error", "");
    }

    bindp_addr = &stp->bindpv[ param_pos - 1 ];

    dbp->status = OCIBindByPos( stp->stmthp,
                                bindp_addr,
                                dbp->errhp,
                                (ub4) param_pos,
                                (dvoid *) param_addr,
                                (sword) param_size,
                                (ub2) param_type,
                                (dvoid *) ind_addr,
                                (ub2 *) 0,
                                (ub2 *) rcode_addr,
                                (ub4) 0,
                                (ub4 *) 0,
                                OCI_DEFAULT
                                );

    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_bind_by_pos. Cannot bind", NULL);

    if ( skip_size > 0 ) {
      dbp->status = OCIBindArrayOfStruct( *bindp_addr,
                                          dbp->errhp,
                                          skip_size,
                                          ind_addr ? skip_size : 0,
                                          0,
                                          rcode_addr ? skip_size : 0);

      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_bind_by_pos2. BindArrayOfStruct", "");
    }

    if (param_pos > stp->num_bindpv)
      stp->num_bindpv = param_pos;

    assert(stp->num_bindpv <= stp->bindpv_size);

  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
  }
  return ( dbp->status );
}



/*---------------------------------------------------------------------------*/
/**
 * Defines an output variable.
 * The same as @ref sqlo_define_by_pos2, except that the stmt pointer is passed
 * here instead of the statement handle.
 * @see sqlo_define_by_pos2
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _define_by_pos2
#endif
static inline int
DEFUN(_define_by_pos2,
       (stp, value_pos, value_type, value_addr, value_size, ind_addr,
        rlen_addr, rcode_addr, skip_size),
       sqlo_stmt_struct_ptr_t       stp         AND
       unsigned int                 value_pos   AND
       int                          value_type  AND
       const void *                 value_addr  AND
       unsigned int                 value_size  AND
       short *                      ind_addr    AND
       ub4 *                        rlen_addr   AND
       ub2 *                        rcode_addr  AND
       unsigned int                 skip_size )
{
  sqlo_db_struct_ptr_t dbp;

  TRACE(3,
        fprintf(_get_trace_fp(stp->dbp),
                 "_define_by_pos2 pos: %u type: %d (%s), size: %u, skip_size: %u, num_defnpv=%d\n",
                 value_pos, value_type, _get_data_type_str(value_type),
                value_size, skip_size, stp->num_defnpv););

  assert( value_pos > 0 );
  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL);

  dbp = stp->dbp;

  if ( _is_prepared(stp)) {

    _alloc_definep(stp, value_pos);
    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_define_by_pos2. alloc error", "");

    /* save the datatype */
    stp->ocolsv[ value_pos - 1 ].dtype = (ub2)value_type;

    dbp->status = OCIDefineByPos( stp->stmthp,
                                  &stp->defnpv[ value_pos - 1 ],
                                  dbp->errhp,
                                  (ub4) value_pos,
                                  (ub1 *) value_addr,
                                  (sword) value_size,
                                  value_type,
                                  (dvoid *) ind_addr,
                                  (ub2 *) rlen_addr,
                                  (ub2 *) rcode_addr,
                                  OCI_DEFAULT
                                  );

    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                            "_define_by_pos2: Cannot define", NULL);

    if (skip_size)
    {
      dbp->status = OCIDefineArrayOfStruct( stp->defnpv[ value_pos - 1 ],
                                            dbp->errhp,
                                            skip_size,
                                            ind_addr ? skip_size : 0,
                                            rlen_addr ? skip_size : 0,
                                            rcode_addr ? skip_size : 0
                                            );

      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_define_by_pos2 BindArrayOfStruct", "");
    }

    if (value_pos > stp->num_defnpv)
      stp->num_defnpv = value_pos;

    assert(stp->defnpv_size >= stp->num_defnpv);

  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
  }

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Defines an output variable.
 * The same as @ref sqlo_define_by_pos, except that the stmt pointer is passed
 * here instead of the statement handle.
 * @see _define_by_pos
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _define_by_pos
#endif
static inline int
DEFUN(_define_by_pos,
       (stp, value_pos, value_type, value_addr, value_size, ind_addr,
        rlen_addr, rcode_addr, is_array),
       sqlo_stmt_struct_ptr_t      stp          AND
       unsigned int                value_pos    AND
       int                         value_type   AND
       const void *                value_addr   AND
       unsigned int                value_size   AND
       short *                     ind_addr     AND
       ub4 *                       rlen_addr    AND
       ub2 *                       rcode_addr   AND
       int                         is_array )
{
  sqlo_db_struct_ptr_t dbp;

  TRACE(3,
        fprintf(_get_trace_fp(stp->dbp),
                "_define_by_pos pos: %u type: %d (%s), size: %u, isa: %d, num_defnpv=%d\n",
                value_pos, value_type, _get_data_type_str(value_type),
                value_size, is_array, stp->num_defnpv););

  assert(value_pos > 0);
  assert( stp->dbp  != NULL );
  dbp = stp->dbp;

  if ( _is_prepared(stp) ) {

    _alloc_definep(stp, (unsigned int)value_pos);
    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_define_by_pos. alloc error", "");

    /* save the datatype */
    stp->ocolsv[value_pos -1].dtype = (ub2)value_type;

    dbp->status = OCIDefineByPos( stp->stmthp,
                                  &stp->defnpv[ value_pos - 1 ],
                                  stp->dbp->errhp,
                                  (ub4) value_pos,
                                  (ub1 *) value_addr,
                                  (sword) value_size,
                                  value_type,
                                  (dvoid *) ind_addr,
                                  (ub2 *) rlen_addr,
                                  (ub2 *) rcode_addr,
                                  OCI_DEFAULT
                                  );

    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_define_by_pos: Cannot define", NULL);

    if (is_array) {
      dbp->status = OCIDefineArrayOfStruct( stp->defnpv[ value_pos - 1 ],
                                            stp->dbp->errhp,
                                            value_size,
                                            ind_addr ? sizeof(short) : 0,
                                            rlen_addr ? sizeof(int) : 0,
                                            rcode_addr ? sizeof(short) : 0
                                      );
      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_define_by_pos BindArrayOfStruct", "");
    }

    if (value_pos > stp->num_defnpv)
      stp->num_defnpv = value_pos;

    assert(stp->defnpv_size >= stp->num_defnpv);

  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
  }

  return (dbp->status);

}



/*---------------------------------------------------------------------------*/
/**
 * Binds the input variables for sqlo_open, sqlo_open2
 * Treats an empty string and a NULL pointer in argv as NULL.
 * Call @ref _bind_by_pos for each entry in argv.
 *
 * @param stp  I - Statement pointer
 * @param argc I - Number of entries in argv
 * @param argv I - bind values
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_bind_argv, (stp, argc, argv),
       sqlo_stmt_struct_ptr_t     stp    AND
       unsigned int               argc   AND
       const char **              argv )
{
  sqlo_db_struct_ptr_t dbp;
  register unsigned int arg_idx;
  register const char ** arg;
  unsigned int size;
  register short * ind_ptr;

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  /* allocate the necessary intput bind variables */
  if (argc > stp->bindpv_size) {
    dbp->status = _alloc_bindp(stp, argc);
    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "Malloc error in _bind_argv", "");
  }

  arg = argv;
  ind_ptr = stp->indpv;
  for (arg_idx = 0 ; arg_idx < argc; ++arg_idx, ++arg, ++ind_ptr) {
    if (!*arg || !**arg ) {    /* treat null pointer or empty string as null */
      *ind_ptr = SQLO_NULL_IND;
      size = 0;
    } else {
      *ind_ptr = SQLO_NOT_NULL_IND;
      size = strlen(*arg) + 1;
    }

    dbp->status = _bind_by_pos(stp, arg_idx + 1, SQLOT_STR, *arg,
                               size, ind_ptr, 0);

    CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "_bind_argv. Cannot bind:",
                             *arg);

  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Calculates the size of the output buffer for a datatype.
 *
 * @param bufsizep  O - The calculated size.
 * @param data_type I - The oracle data type
 * @param prec      I - The precision (number type)
 * @param scale     I - The scale (number type)
 * @param dbsize    I - Size in the database
 *
 * @return SQLO_SUCCESS or SQLO_ERROR for a datatype that could not be
 *         handled here (LOB).
 */
#ifdef CC_PRAGMA_INLINE
#pragma INLINE _calc_obuf_size
#endif
static inline int
DEFUN(_calc_obuf_size, (bufsizep, data_type, prec, scale, dbsize),
      unsigned int *    bufsizep     AND
      unsigned int      data_type    AND
      int               prec         AND
      int               scale        AND
      unsigned int      dbsize )
{

  unsigned int buffer_size = 0;
  int status = SQLO_SUCCESS;

  assert( bufsizep != NULL);

  switch(data_type) {

  case SQLT_NUM:
  case SQLT_INT:
  case SQLT_FLT:
    if (scale > prec)
    {
      buffer_size = (unsigned int) scale + 3; /* sign, comma and \0 */
    }
    else if (prec > 0)
    {
      buffer_size = (unsigned int) prec + 3;
    }
    else
    {
      buffer_size = (unsigned int) (2 * dbsize) + 3;
    }

    /* use a minimum buffer */
    if (buffer_size < (2 * dbsize) + 3 )
      buffer_size = ( 2* dbsize)  + 3;


    break;

  case SQLT_CHR:
  case SQLT_STR:
  case SQLT_AFC:
    buffer_size = (dbsize + 1);
    break;

  case SQLT_RID:
  case SQLT_RDD:
    buffer_size = 32;
    break;

  case SQLT_DAT:
  case SQLT_DATE:
  case SQLT_TIME:
  case SQLT_TIME_TZ:
  case SQLT_TIMESTAMP:
  case SQLT_TIMESTAMP_TZ:
  case SQLT_TIMESTAMP_LTZ:
    buffer_size = 64;
    break;

  case SQLT_LNG:
    buffer_size = _max_long_size;
    break;

  case SQLT_BLOB:
  case SQLT_CLOB:
    buffer_size = 0;
    break;

  case SQLT_BFILEE:
  case SQLT_CFILEE:
    status = SQLO_ERROR;        /* not supported in this mode */
    break;

  default:
    buffer_size = (( 8 * dbsize) + 1);

  } /* end switch */

  *bufsizep = buffer_size;

  return status;

}



/*---------------------------------------------------------------------------*/
/**
 * Get the database data type.
 * @param stp  I - The statement handle pointer
 * @param pos  I - The column position (1 based).
 * @param dtypep O - The database data type.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_get_ocol_db_data_type, (stp, pos, dtypep),
      sqlo_stmt_struct_ptr_t   stp    AND
      unsigned int             pos    AND
      ub2 *                    dtypep )
{
  sqlo_db_struct_ptr_t  dbp;
  OCIParam* paramd;

  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL);
  assert( pos > 0 );
  assert( dtypep != NULL );

  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_data_type: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos
                             );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_data_type", "OCIParamGet");

  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) dtypep,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_DATA_TYPE,
                            (OCIError *)dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_data_type", "OCIAttrGet(datatype)"
                           );

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_data_type: datatype: %d (%s)\n",
                   (int) *dtypep,
                   _get_data_type_str((int)*dtypep)););

  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_data_type",
                           "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Get the database size of the output column
 * @param stp    I - The statement handle pointer
 * @param pos    I - The column position (1 based).
 * @param sizep  O - The database size.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_get_ocol_db_size, (stp, pos, sizep),
      sqlo_stmt_struct_ptr_t   stp   AND
      unsigned int             pos   AND
      ub2 *                    sizep )
{
  sqlo_db_struct_ptr_t  dbp;
  OCIParam* paramd;

  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL);
  assert( pos > 0 );
  assert( sizep != NULL );

  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_size: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos);

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_size", "OCIParamGet");

  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) sizep,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_DATA_SIZE,
                            (OCIError *)dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "_get_ocol_db_size", "OCIAttrGet(datatype)");
  TRACE(3, fprintf(_get_trace_fp(dbp), "_get_ocol_db_size: size: %d \n",
                   (int) *sizep););

  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_size", "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Get the precision of the output column
 * @param stp    I - The statement handle pointer
 * @param pos    I - The column position (1 based).
 * @param precp  O - The precision.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_get_ocol_db_prec, (stp, pos, precp),
      sqlo_stmt_struct_ptr_t   stp    AND
      unsigned int             pos    AND
      ub1 *                    precp )
{
  sqlo_db_struct_ptr_t  dbp;
  OCIParam* paramd;

  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL);
  assert( pos > 0 );
  assert( precp != NULL );

  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_prec: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos);

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_prec", "OCIParamGet");

  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) precp,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_PRECISION,
                            (OCIError *)dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_ocol_db_prec", "OCIAttrGet(datatype)");

  TRACE(3, fprintf(_get_trace_fp(dbp), "_get_ocol_db_prec: prec: %d \n",
                   (int) *precp););

  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_prec", "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Get the scale of the output column
 * @param stp    I - The statement handle pointer
 * @param pos    I - The column position (1 based).
 * @param scalep  O - The precision.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_get_ocol_db_scale, (stp, pos, scalep),
      sqlo_stmt_struct_ptr_t   stp      AND
      unsigned int             pos      AND
      ub1 *                    scalep )
{
  sqlo_db_struct_ptr_t  dbp;
  OCIParam* paramd;

  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( pos > 0 );
  assert( scalep != NULL);

  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_scale: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos
                             );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_scale", "OCIParamGet");

  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) scalep,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_SCALE,
                            (OCIError *)dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_scale", "OCIAttrGet(datatype)");

  TRACE(3, fprintf(_get_trace_fp(dbp), "_get_ocol_db_scale: scale: %d \n",
                   (int) *scalep););

  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_scale", "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Get the is_null attribute of the output column
 * @param stp    I - The statement handle pointer
 * @param pos    I - The column position (1 based).
 * @param is_nullp  O - The is_null attribute
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_get_ocol_db_is_null, (stp, colp, pos, is_nullp),
      sqlo_stmt_struct_ptr_t   stp        AND
      unsigned int             pos        AND
      ub1 *                    is_nullp )
{
  sqlo_db_struct_ptr_t  dbp;
  OCIParam* paramd;

  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( pos > 0 );
  assert( is_nullp != NULL);

  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_ocol_db_is_null: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos
                             );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_is_null", "OCIParamGet");

  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) is_nullp,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_IS_NULL,
                            (OCIError *)dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_is_null", "OCIAttrGet(datatype)");

  TRACE(3, fprintf(_get_trace_fp(dbp), "_get_ocol_db_is_null: is_null: %d \n",
                   (int) *is_nullp););

  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_get_ocol_db_is_null", "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Set the output column name in the col structure.
 * @param stp  I - The statement handle pointer
 * @param colp I - Pointer to the column structure
 * @param pos  I - The column position (1 based).
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_set_ocol_name, (stp, colp, pos),
      sqlo_stmt_struct_ptr_t   stp    AND
      sqlo_col_struct_t *      colp   AND
      unsigned int             pos )
{
  sqlo_db_struct_ptr_t  dbp;
  char * col_name;
  unsigned int col_name_len;
  unsigned int cur_col_name_len;
  register unsigned int col_idx;     /* the index into our arrays */
  OCIParam* paramd;

  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( pos > 0 );
  assert( colp != NULL);

  dbp = stp->dbp;
  col_idx = pos - 1;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_set_ocol_name: sth: %d, pos: %d \n",
                   stp->sth,
                   pos););

  /* allocate a parameter descriptor */
  dbp->status = OCIParamGet( stp->stmthp,
                             OCI_HTYPE_STMT,
                             dbp->errhp,
                             (void **)((dvoid *)&paramd),
                             (ub4) pos
                             );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_set_ocol_name", "OCIParamGet");

  col_name_len = 0;
  col_name = NULL;
  dbp->status = OCIAttrGet( (dvoid*) paramd, (ub4) OCI_DTYPE_PARAM,
                            (dvoid**) &(col_name),
                            (ub4 *) &(col_name_len),
                            (ub4) OCI_ATTR_NAME,
                            (OCIError *) dbp->errhp
                            );

  cur_col_name_len = col_name_len; /* save the real length */
  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_set_ocol_name", "OCIAttrGet(column_name)");

  if (!colp->col_name)
  {

    if (col_name_len < MIN_COL_NAME_LEN)
      col_name_len = MIN_COL_NAME_LEN;

    colp->col_name = (char *) hb_xgrabDebug( __LINE__,  sizeof(char) * (col_name_len + 1) );
    colp->col_name_size = col_name_len;

  }
  else
  {

    if (col_name_len > colp->col_name_size) {
      col_name_len = 2 * col_name_len; /* alloc always twice as much we need */
      colp->col_name = (char *) hb_xreallocDebug( __LINE__, colp->col_name, sizeof(char) * (col_name_len + 1) );

      colp->col_name_size = col_name_len;
    }
  }

  /* copy and terminate the column name */
  strncpy(colp->col_name, col_name, (size_t)cur_col_name_len);
  colp->col_name[ cur_col_name_len ] = '\0';


  TRACE(3, fprintf(_get_trace_fp(dbp), "_set_ocol_name: colname: %.*s (colname_len: %u)\n",
                   (int)colp->col_name_size, colp->col_name, cur_col_name_len););


  /* Point our ocol_namev[i] to the right column name.
   * The name can be fetched by sqlo_ocol_names();
   * Store the length of the column name in ocol_namev_size[i]*/
  stp->ocol_namev[ col_idx ] = colp->col_name;
  stp->ocol_namev_size[ col_idx ] = cur_col_name_len;


  /* free the descriptor */
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_set_ocol_name", "OCIDescriptorFree(paramd)");

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Set all names of the output columns.
 * Calls @ref _set_ocol_name for each output column
 * @param stp  The statement handle pointer
 * @return SQLO_SUCCESS or < 0 on error (dbp->status)
 */
static int
DEFUN(_set_all_ocol_names, (stp), sqlo_stmt_struct_ptr_t stp )
{
  ub4 num_cols;
  unsigned int col_pos;

  sqlo_col_struct_ptr_t colp;
  sqlo_db_struct_ptr_t  dbp;

  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );

  dbp = stp->dbp;

  /* Get number of columns in the select list */
  dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                            (ub4) OCI_HTYPE_STMT,
                            (dvoid *)  &num_cols,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_PARAM_COUNT,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_set_all_ocol_names", "OCIAttrGet(PARAM_COUNT)");

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_set_all_ocol_names: Number of columns in select list: %d\n",
                   (int) num_cols););

                                /* allocate the space for the output */

  /* should already been set by prepare or open */
  _alloc_definep( stp, num_cols );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "_set_all_ocol_names", "Memory allocation error");

  /* define all columns */
  for (col_pos = 1, colp = stp->ocolsv; col_pos <= (unsigned int) num_cols ;
       ++col_pos, ++colp) {

    dbp->status = _set_ocol_name(stp, colp, col_pos);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                            "_set_all_ocol_names", "_set_ocol_name");

  }

  return ( dbp->status );

}



/*---------------------------------------------------------------------------*/
/**
 * Allocates the buffer for the output column.
 *
 * Describe one output variable and create the buffer to store the query result
 * @param stp  I - The statement handle pointer
 * @param pos  I - The column position (1 based).
 * @param buffer_size I - The required size.
 * @return SQLO_SUCCESS or < 0 on error.
 * @note Does not allocate buffer_size. The function allocates always a minimum
 *       size and increases it always to the hold at least twice the buffer_size
 *       For the really allocated size check stp->rlenv[ pos - 1 ]
 */
static int
DEFUN(_alloc_ocol_buffer, (stp, pos, buffer_size),
      sqlo_stmt_struct_ptr_t   stp           AND
      unsigned int             pos           AND
      unsigned int             buffer_size )
{
  int col_idx;
  unsigned int buf_size = buffer_size;
  assert( stp != NULL );
  assert( pos > 0 );
  assert( stp->dbp != NULL );

  col_idx = pos - 1;

  // TraceLog(LOGFILE, "col %i, previously allocated %u, needed buffer_size: %u\n", col_idx, stp->rlenv[ col_idx ], buffer_size );

  TRACE(3, fprintf(_get_trace_fp(stp->dbp),
                   "_alloc_ocol_buffer: sth=%d, column pos=%u, buffer_size: %u\n",
                   stp->sth,
                   pos, buffer_size); );

  if (!stp->outv[ col_idx ])
  {
    if (buf_size)
    {
      /* always allocate a minimum buffer */
      if (buf_size < MIN_OBUF_SIZE)
      {
        buf_size = MIN_OBUF_SIZE;
      }
      stp->outv[ col_idx ] = (char *)hb_xgrabDebug( __LINE__,  (buf_size) * sizeof(char) );
      memset(stp->outv[ col_idx ] , 0, (buf_size) * sizeof(char));
      stp->rlenv[ col_idx ] = buf_size;
    }
    stp->outv_size[ col_idx ] = buf_size;
  }
  else
  {
    if (buf_size > stp->rlenv[ col_idx ])
    {
      /* Alloc always twice as much to avoid lots of reallocs, but don't
       * do this for long columns
       */
      stp->outv_size[ col_idx ] = buf_size;
      if (buf_size < _max_long_size)
      {
        buf_size = 2 * buf_size;
      }
      stp->outv[ col_idx ] = (char *)hb_xreallocDebug( __LINE__, stp->outv[ col_idx ], (buf_size) * sizeof(char) );
      memset(stp->outv[ col_idx ] , 0, (buf_size) * sizeof(char));
      stp->rlenv[ col_idx ] = buf_size;
    }
    else
    {
      stp->outv_size[ col_idx ] = buf_size;
   }
  }
  return SQLO_SUCCESS;
}



/*---------------------------------------------------------------------------*/
/**
 * Defines one output column
 *
 * Describe one output variable and create the buffer to store the query result
 * @param stp  I - The statement handle pointer
 * @param colp I - Pointer to the column structure
 * @param pos  I - The column position (1 based).
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_define_ocol_by_pos, (stp, colp, pos),
      sqlo_stmt_struct_ptr_t   stp    AND
      sqlo_col_struct_t *      colp   AND
      unsigned int             pos )
{
  sqlo_db_struct_ptr_t  dbp;
  unsigned int buffer_size;
  register unsigned int col_idx;     /* the index into our arrays */

  assert( pos > 0 );
  assert( stp->dbp != NULL );

  dbp = stp->dbp;
  col_idx = pos - 1;

  TRACE(3, fprintf(_get_trace_fp(dbp), "Define output column number %u\n", pos););

  /* save index and link to the statement */
  colp->pos = col_idx;
  colp->stp = (struct _sqlo_stmt_struct_t *)stp;

  TRACE(3, fprintf(_get_trace_fp(dbp), "Getting parameters of col: %u\n", pos););

  /* these routines set all dbp->status */
  if (SQLO_SUCCESS ==
      ( _get_ocol_db_data_type(stp, pos, &(colp->database_dtype))    ||
        _set_ocol_name(stp, colp, pos)                  ||
        _get_ocol_db_size(stp, pos, &(colp->dbsize) )   ||
        _get_ocol_db_prec(stp, pos, &(colp->prec))      ||
        _get_ocol_db_scale(stp, pos, &(colp->scale))    ||
        _get_ocol_db_is_null(stp, pos, &(colp->nullok))
       )
      )
  {

    /* If we cannot determine the size, it is caused by an
     * unsupported data type
     */

    _calc_obuf_size( &buffer_size, colp->database_dtype, colp->prec, colp->scale, colp->dbsize);

    if( buffer_size )
    {
       _alloc_ocol_buffer(stp, pos, buffer_size);

       // TraceLog( LOGFILE, "_define_by_pos col %i, IN length %i, buffer %i, allocated %u\n", col_idx, stp->outv_size[ col_idx ], buffer_size, stp->rlenv[ col_idx ]);

	    dbp->status = _define_by_pos(stp,
                                   pos,
                                   SQLT_STR,
                                   stp->outv[ col_idx ],
                                   stp->outv_size[ col_idx ],
                                   (short *) &stp->oindv[ col_idx ],
                                   (ub4 *) &stp->outv_size[ col_idx ],
                                   NULL,
                                   0);
       stp->outv_size[ col_idx ] = stp->outv_size[ col_idx ] - 1;

       //TraceLog( LOGFILE, "_define_by_pos col %i, OUT length %i\n", col_idx, stp->outv_size[ col_idx ]);

    }
    else
    {
      /* Zero buffer means MEMO data type - should alloc LOB descriptor */

      _alloc_ocol_buffer(stp, pos, INITIAL_LOB_ALLOC);

      if (!colp->loblp)
      {
         OCIDescriptorAlloc( (dvoid *) dbp->envhp,
                          (dvoid **) &(colp->loblp),
                          (ub4) OCI_DTYPE_LOB,
                          (size_t) 0,
                          (dvoid **) 0);
         // TraceLog(LOGFILE, "col %i, OCIDescriptorAlloc %p\n", col_idx, colp->loblp );
      }

      dbp->status = _define_by_pos(stp,
                                   pos,
                                   SQLOT_CLOB,
                                   &(colp->loblp),
                                   0,
                                   (short *) &stp->oindv[ col_idx ],
                                   0,
                                   NULL,
                                   0);

    }
    CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_define_ocol_by_pos", "_define_by_pos");
  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Returns the pointer to the statement structure
 *
 * decodes the sth into real sth and dbh and returns the pointer to the statement
 * structure.
 *
 * @return stp or NULL on error.
 */
#if CC_PRAGMA_INLINE
#pragma INLINE _sth2stp
#endif
static inline sqlo_stmt_struct_ptr_t
DEFUN(_sth2stp, (sth, func_name),
      int             sth        AND
      const char *    func_name )
{
  register ub4 real_sth;
  register ub4 dbh;
  register sqlo_db_struct_ptr_t dbp;

  real_sth = DECODE_STH(sth);
  dbh = DECODE_DBH(sth);

  CHECK_DBHANDLE(dbp, dbh, func_name, NULL);

  TRACE(3, fprintf( _get_trace_fp(dbp),
                    "_sth2stp: sth %d -> sth=%u, dbh=%u\n", sth, real_sth, dbh););

  if (real_sth >= dbp->stmtv_size || FALSE == dbp->stmtv[ real_sth ].used) {

    sprintf(dbp->errmsg, "Invalid sth %u passed to %s\n", real_sth, func_name);
    TRACE(1, fprintf( _get_trace_fp(dbp), dbp->errmsg););
      /* make sure we release all locks */
    UNLOCK_ALL;
    return( NULL );
  }

  return ( &(dbp->stmtv[ real_sth ]) );
}



/*---------------------------------------------------------------------------*/
/**
 * Define the output variables.
 *
 * Describe the output variables and create the buffers for the ouptut.
 *
 * @return SQLO_SUCCESS or < 0 on error.
 */
static int
DEFUN(_define_output, (stp), sqlo_stmt_struct_ptr_t  stp)
{
  register unsigned int col_pos; /* The column position (1based) */
  ub4 num_cols;                 /* number of columns in the select list */
  sqlo_col_struct_ptr_t colp;
  sqlo_db_struct_ptr_t  dbp;

  assert( stp->dbp != NULL );

  dbp = stp->dbp;

  /* Already defined ? */
  if (stp->num_defnpv)
    return (SQLO_SUCCESS);

  /* Describe the output variables.
   * REFCURSORs are already exectuted by there parent stmt
   */

  if ( 0 == stp->num_executions && !(stp->cursor_type == REFCURSOR) ) {
    dbp->status = OCIStmtExecute( dbp->svchp,
                                  stp->stmthp,
                                  dbp->errhp,
                                  (ub4) 0,
                                  (ub4) 0,
                                  (OCISnapshot *) 0,
                                  (OCISnapshot *) 0,
                                  (ub4) OCI_DEFAULT);

    if (OCI_STILL_EXECUTING == dbp->status ) {
      return ( dbp->status );
    } else {
      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "_define_output", "OCIStmtExecute(DESCRIBE)");
    }

    stp->num_executions++;
  }

  if (stp->cursor_type == REFCURSOR)
    stp->opened = TRUE;

  /* Get info about the select list */
  dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                            (ub4) OCI_HTYPE_STMT,
                            (dvoid *)  &num_cols,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_PARAM_COUNT,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_define_output", "OCIAttrGet(PARAM_COUNT)");

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "Number of columns in select list: %d\n",
                   (int) num_cols););

  /* allocate the space for the output */
  _alloc_definep( stp, num_cols );
  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_define_output", "Memory allocation error");

  /* define all columns */
  for (col_pos = 1, colp = stp->ocolsv ;
       col_pos <= (unsigned int) num_cols ;
       ++col_pos, ++colp) {

    if (SQLO_SUCCESS !=
        (dbp->status = _define_ocol_by_pos(stp, colp, col_pos))) {
      break;
    }
  }

  return (dbp->status);
}



/*-------------------------------------------------------------------------*/
/**
 * Close all open cursors which are in the still executing status
 * This function returns no status code and all errors returned by @ref sqlo_close
 * are ignored.
 * @param dbp  I - The database pointer
 */
static void
DEFUN(_close_all_executing_cursors, (dbp), const_sqlo_db_struct_ptr_t dbp)
{
  unsigned int stmt_idx;

  for ( stmt_idx = 0; stmt_idx < dbp->stmtv_size; ++stmt_idx ) {

    sqlo_stmt_struct_ptr_t stp = &(dbp->stmtv[ stmt_idx ]);

    if (stp->used && stp->still_executing ) {
      sqlo_close( ENCODE_STH(stp->sth, dbp->dbh) );
    } /* endif is valid and executing */
  } /* end for stmt_idx */
}



/*-------------------------------------------------------------------------*/
/**
 * Close all open cursors on a connection.
 * This function returns no status code and all errors returned by @ref sqlo_close
 * are ignored.
 * @param dbp  I - The database pointer
 */
static void
DEFUN(_close_all_db_cursors, (dbp), const_sqlo_db_struct_ptr_t dbp)
{
  unsigned int stmt_idx;

  for (stmt_idx = 0; stmt_idx < dbp->stmtv_size; ++stmt_idx) {

    sqlo_stmt_struct_ptr_t stp =  &(dbp->stmtv[ stmt_idx ]);
    unsigned int col_idx;

    if (stp->used)
    {
      sqlo_close( ENCODE_STH(stp->sth, dbp->dbh) );

      /* free all allocated memory for a stmt. We do not do this
       * when a cursor is closed to recylce the memory, but in this
       * case the whole session is ended and we free it
       */
      _dealloc_definep( stp );
      XFREE( stp->stmt, __LINE__ );
      stp->stmt_size = 0;
    }

    if( stp->dbp )
    {
      _dealloc_bindp( stp );
    }

    for ( col_idx = 0; col_idx < stp->defnpv_size; ++col_idx )
    {
      if (stp->ocolsv && stp->ocolsv[ col_idx ].col_name)
         XFREE( stp->ocolsv[ col_idx ].col_name, __LINE__ );

      if (stp->ocolsv && stp->ocolsv[ col_idx ].loblp)
      {

        OCIDescriptorFree((dvoid **) &(stp->ocolsv[ col_idx ].loblp), (ub4) OCI_DTYPE_LOB);
        // TraceLog(LOGFILE, "col %i, OCIDescriptorFree 2 %p\n", col_idx, stp->ocolsv[ col_idx ].loblp );

        stp->ocolsv[ col_idx ].loblp = NULL;
      }
      if (stp->outv && stp->outv[ col_idx ])
      {
         XFREE( stp->outv[ col_idx ], __LINE__ );
         stp->outv[ col_idx ] = NULL;
         stp->outv_size[ col_idx ] = 0;
      }
    }

    if (stp->defnpv)
    {
      XFREE( stp->defnpv, __LINE__ );
    }
    if (stp->ocolsv)
    {
      XFREE( stp->ocolsv, __LINE__ );
    }
    if( stp->outv )
    {
      XFREE( stp->outv, __LINE__ );
    }
    if( stp->outv_size )
    {
      XFREE( stp->outv_size, __LINE__ );
    }
    if( stp->oindv )
    {
      XFREE( stp->oindv, __LINE__ );
    }
    if( stp->rlenv )
    {
      XFREE( stp->rlenv, __LINE__ );
    }
    if( stp->ocol_namev )
    {
      XFREE( stp->ocol_namev, __LINE__ );
    }
    if( stp->ocol_namev_size )
    {
      XFREE( stp->ocol_namev_size, __LINE__ );
    }

    if( stp->stmt_size > 0 )
    {
      XFREE( stp->stmt, __LINE__ );
    }

    stp->defnpv = NULL;
    stp->ocolsv = NULL;
    stp->outv = NULL;
    stp->outv_size = NULL;
    stp->oindv = NULL;
    stp->rlenv = NULL;
    stp->ocol_namev_size = NULL;
    stp->stmt_size = 0;
    stp->stmt = NULL;
    stp->defnpv_size = 0;
    stp->num_defnpv  = 0;

  }
}



/*---------------------------------------------------------------------------*/
/**
 * Gets the blocking mode for a dbp.
 * @param dbp           I - The database pointer.
 * @param blockingp     O - SQLO_ON if in blocking mode, SQLO_OFF if in non-blocking
 *                          mode
 *
 * @return dbp->status
 */
static int
DEFUN(_get_blocking_mode, (dbp, blocking),
      sqlo_db_struct_ptr_t   dbp         AND
      unsigned int *         blockingp )
{
  ub1 non_blocking = 0;

  assert( blockingp );

  /* Returns TRUE if the server is in non-blocking mode */
  dbp->status = OCIAttrGet( (dvoid*)dbp->srvhp,
                            (ub4) OCI_HTYPE_SERVER,
                            (dvoid *)  &non_blocking,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_NONBLOCKING_MODE,
                            dbp->errhp);

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                           "_get_blocking_mode", "OCIAttrGet error");

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "_get_blocking_mode: status=%d, non_blocking=%u\n",
                   dbp->status, non_blocking););

  if (OCI_SUCCESS == dbp->status ) {
    if (non_blocking)
      *blockingp = SQLO_OFF;
    else
      *blockingp = SQLO_ON;
  } else {
    dbp->status = SQLO_ERROR;
  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Returns the trace file pointer for the db connection.
 * If dbp is NULL, we return the global trace file pointer,
 * else the connection specific trace file pointer is returned.
 *
 * @param dbp           I - The database pointer.
 *
 * @return <ul>
 * <li>Connection specific or global trace file.
 * <li>NULL, if no trace file is open.
 * </ul>
 */
static FILE *
DEFUN(_get_trace_fp, (dbp), const_sqlo_db_struct_ptr_t dbp)
{
  FILE *fp;
  if (!dbp)
    fp =  _trace_fp;
  else {
    if (dbp->trace_fp)
      fp = dbp->trace_fp;
    else
      fp = _trace_fp;
  }

  if (!fp)
  {
    fp = stderr;
  }

#ifndef NDEBUG
  fflush(fp);
#endif
  return (fp);
}


/*---------------------------------------------------------------------------*/
/** Determines the statement type of an already prepared statement
 * @param stp        I  - The statement handle pointer
 * @param stmt_typep  O - The statement type
 * @return SQLO_SUCCESS or <0 when the stmt is not prepared or OCIAttrGet returned an OCI error.
 */
static int
DEFUN(_get_stmt_type, (stp, stmt_typep),
      sqlo_stmt_struct_ptr_t    stp          AND
      ub2 *                     stmt_typep )
{
  sqlo_db_struct_ptr_t dbp;

  assert( stp != NULL );
  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );

  dbp = stp->dbp;

  if (! _is_prepared(stp) ){
    sprintf(stp->dbp->errmsg,
            "Cannot get statement type for a non-prepared statement (sth %u)",
            stp->sth );
    TRACE(1, (void) fputs(stp->dbp->errmsg, _get_trace_fp(stp->dbp)););
    return SQLO_ERROR;
  }

  if (stmt_typep) {

    /* Identify the statement type */
    dbp->status = OCIAttrGet( (dvoid*) stp->stmthp,
                              (ub4) OCI_HTYPE_STMT,
                              (dvoid*) stmt_typep,
                              (ub4 *) 0,
                              (ub4) OCI_ATTR_STMT_TYPE,
                              (OCIError *) dbp->errhp );

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "_get_stmt_type", "OCIAttrGet");

    TRACE(2, fprintf(_get_trace_fp(dbp),
                     "_get_stmt_type: Statement type (%u): %s\n",
                     (unsigned int) *stmt_typep,
                     _get_stmt_type_str( (int) *stmt_typep) ););

  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Prepares a statement and returns the statement type.
 * If stmt_type is NULL, the statement type is not determined.
 * @param stp       I - The statement pointer
 * @param stmt      I - The sql statement
 * @param stmt_type O - The statement type, if stmt_type is not NULL
 *
 * @return OCI status (dbp->status)
 */
static int
DEFUN(_prepare, (stp, stmt, stmt_type),
      sqlo_stmt_struct_ptr_t    stp         AND
      const char *              stmt        AND
      ub2 *                     stmt_type )
{
  sqlo_db_struct_ptr_t dbp;

  assert( stp != NULL );
  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );

  dbp = stp->dbp;


  dbp->status = OCIStmtPrepare( stp->stmthp,
                                dbp->errhp,
                                (text *)stmt,
                                strlen(stmt),
                                OCI_NTV_SYNTAX,
                                OCI_DEFAULT
                                );

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "_prepare: OCIStmtPrepare: status=%d",
                   dbp->status););

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_prepare", (char*)stmt);

  stp->prepared = TRUE;

  if (stmt_type) {
    dbp->status = _get_stmt_type(stp, stmt_type);
    CHECK_OCI_STATUS_RETURN( dbp, dbp->status, "_prepare", "_get_stmt_type");
  }

  return (dbp->status);
}



/*---------------------------------------------------------------------------*/
/**
 * Reopens a already used cursor and binds the input variables
 *
 * This function reopens an already used cursor with new bind variables.
 * Reopening cursors improve the speed, because no new parse is necessary.
 *
 * @param stp  I - The statement pointer to the open statement.
 * @param argc I - Number of arguments in argv
 * @param argv I - Arguments
 *
 * @return <ul>
 * <li>SQLO_SUCCESS
 * <li>SQLO_STILL_EXECUTING in non-blocking mode
 * <li> < 0 on error
 * </ul>
 *
 */
static int
DEFUN(_sqlo_reopen,  (stp, argc, argv),
       sqlo_stmt_struct_ptr_t    stp    AND
       int                       argc   AND
       const char **             argv )
{

  sqlo_db_struct_ptr_t dbp;

  assert( stp != NULL );
  assert( stp->dbp != NULL );
  assert( stp->dbp->errhp != NULL );

  dbp = stp->dbp;

  if (argc > 0) {

    if (!stp->still_executing) {
      /* Reset number of bindpv. Don't free them! */
      _bindpv_reset(stp);

      dbp->status = _bind_argv(stp, (unsigned int)argc, argv);

      if (SQLO_SUCCESS != dbp->status) {
        sqlo_close( ENCODE_STH(stp->sth, dbp->dbh) );
        return (dbp->status);
      }

      TRACE(2,
            if (argc) {
              fprintf(_get_trace_fp(dbp),
                      "sqlo_reopen [%2u] %s\n",
                      stp->sth,
                      _get_stmt_string(stp) );
            });

      TRACE(3, {
        int z ;
        for ( z=0 ; z < argc; z++) {
          fprintf(_get_trace_fp(dbp),
                  "sqlo_reopen[%d]: arg[%02d]: %s\n", stp->sth, z, argv[z] ? argv[z] : "NULL");
        } });
    }
  } /* end if argc > 0 */

  if ( _is_query(stp) ) {

    TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_reopen[%d] is query == TRUE, num_executions=%d\n",
                     stp->sth, stp->num_executions););

    if (0 == stp->num_executions) {

      dbp->status = _define_output(stp);

      if (SQLO_SUCCESS != dbp->status ) {
        if ( SQLO_STILL_EXECUTING == dbp->status ) {
          stp->still_executing = TRUE;
        } else {
          int status = dbp->status;
          _save_oci_status(dbp, "sqlo_reopen", "_define_output",
                           __LINE__);

          sqlo_close( ENCODE_STH(stp->sth, dbp->dbh));
          dbp->status = status;
          return dbp->status;
        }
      }

    } else {
      /* execute the cursor to open the statement again */
      dbp->status = OCIStmtExecute( dbp->svchp,
                                    stp->stmthp,
                                    dbp->errhp,
                                    (ub4) 0,
                                    (ub4) 0,
                                    (OCISnapshot *) 0,
                                    (OCISnapshot *) 0,
                                    dbp->exec_flags
                                    );

      if (OCI_STILL_EXECUTING != dbp->status) {
        CHECK_OCI_STATUS(dbp, dbp->status,
                         "sqlo_reopen", "OCIStmtExecute");

        if (OCI_SUCCESS != dbp->status) {
          int status = dbp->status;

          sqlo_close( ENCODE_STH(stp->sth, dbp->dbh) );
          dbp->status = status;
        }

      }

    }

    if (OCI_SUCCESS != dbp->status) {

      if (OCI_STILL_EXECUTING == dbp->status) {

        stp->still_executing = TRUE;

      } else {
        int status = dbp->status;
        _save_oci_status(dbp, "sqlo_reopen", "OCIStmtExecute(query)",
                         __LINE__);

        sqlo_close( ENCODE_STH(stp->sth, dbp->dbh) );
        dbp->status = status;
      }
    } else {
      /* OCI_SUCCESS == status */
      stp->still_executing = FALSE;

      if (0 == stp->num_executions ) {
        stp->num_executions++;
        dbp->status = _define_output(stp);
      }
    }
  }

  stp->opened = (dbp->status == SQLO_SUCCESS);

  return ( dbp->status );
}



/*-------------------------------------------------------------------------
 * GLOBAL FUNCTIONS
 *-----------------------------------------------------------------------*/

/*-------------------------------------------------------------------------
 * sqlo_get_stmt
 *------------------------------------------------------------------------*/
const char *
DEFUN(sqlo_get_stmt, (sth), sqlo_stmt_handle_t sth )
{
  sqlo_stmt_struct_ptr_t stp;
  CHECK_STHANDLE(stp, sth, "sqlo_get_stmt", NULL);

  assert(stp != NULL);

  if (_get_stmt_string(stp))
    return stp->stmt;

  return NULL;

}



/*-------------------------------------------------------------------------
 * sqlo_get_stmt_state
 *------------------------------------------------------------------------*/
int
DEFUN(sqlo_get_stmt_state, (sth),
      sqlo_stmt_handle_t sth
     )
{
  sqlo_stmt_struct_ptr_t stp;
  CHECK_STHANDLE(stp, sth, "sqlo_get_stmt_state", SQLO_INVALID_STMT_HANDLE);

  assert(stp != NULL);

  return _get_stmt_state(stp);
}



/*-------------------------------------------------------------------------
 * sqlo_init
 *------------------------------------------------------------------------*/
int
DEFUN(sqlo_init, (threaded_mode, max_db, max_cursors),
      int              threaded_mode  AND
      unsigned         max_db         AND
      unsigned int     max_cursors )
{
  int status = OCI_SUCCESS;


  /* must be called in an Oracle threads mt environment,
   * optional for non-mt
   */
#ifdef ENABLE_ORATHREADS
  OCIThreadProcessInit();
#endif

  /* 16.06.03 (kp) Make sure we do not initialize twice */

  EXEC_WHEN_THREADING( _init_lock(); ); /* start of critical section */

  if (_sqlo_init) {
    EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
    return SQLO_SUCCESS;
  }

  *_errmsg = '\0';

  if (threaded_mode)
    _oci_init_mode = OCI_THREADED;
  else
    _oci_init_mode = OCI_DEFAULT;

  /* get the environement settings */
  if (_sqlo_getenv()) {
    EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
    return(-1);
  }

  if (TRACE_ENABLED && _trace_level > 0)
    _open_global_trace_file();

  /* Initialize OCI library */

  /* Oracle 8.0 OCI is initialized different */
#ifndef HAVE_OCIENVCREATE
  {
    ub4 mode = (ub4) _oci_init_mode;
    if (OCI_SUCCESS != (status = OCIInitialize(mode, 0, 0, 0, 0)))
      return(status);
  }
#endif

  if (max_db <= SQLO_MAX_DB)
    _dbv_size = max_db;
  else
    _dbv_size = SQLO_MAX_DB;

  if (max_cursors <= SQLO_MAX_CURSORS)
    _max_cursors = max_cursors;
  else
    _max_cursors = SQLO_MAX_CURSORS;

  if (THREADS_ENABLED && OCI_THREADED == _oci_init_mode) {

#ifdef ENABLE_ORATHREADS

#  ifdef HAVE_OCIENVCREATE
  status = OCIEnvCreate(&_oci_envhp, _oci_init_mode, NULL, NULL, NULL, NULL, 0, NULL) ;
  if (status) {
    EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
    return (status);
  }

#  else

  if ( (status = OCIEnvInit((dvoid *)&_oci_envhp, _oci_init_mode, 0, (dvoid **)0))) {
    EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
    return (status);
  }
#  endif
  /* Alloc the global handle */
  if ((status = OCIHandleAlloc((dvoid *)_oci_envhp, (dvoid **)&_oci_errhp,
                               OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))) {
    EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
    return (status);
  }

  OCIThreadInit(_oci_envhp, _oci_errhp);

#endif

    if ((status = _init_mutexes()))
      return status;
  }

  /* Allocate the arrays for the pointers to the db structures */
  /* check if it the first time we are called */
  if (!_dbv) {
    /* We need to allocate the array of pointers to sqlo_db_struct_t */
    _dbv = (sqlo_db_struct_t **) hb_xgrabDebug( __LINE__, _dbv_size * sizeof(sqlo_db_struct_ptr_t) );
    if (!_dbv) {
      EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
      return(SQLO_ERRMALLOC);
    }

    TRACE(4, fprintf( _get_trace_fp(NULL),
                     "sqlo_init: Allocated %u bytes for %u db handles\n",
                      (unsigned int) (_dbv_size * sizeof(sqlo_db_struct_t)), _dbv_size););
    memset(_dbv, 0, _dbv_size * sizeof(sqlo_db_struct_ptr_t) );
  }

  _sqlo_init = TRUE;
  EXEC_WHEN_THREADING( _init_unlock(); ); /* end of critical section */
  return (status);
}



/*---------------------------------------------------------------------------
 *         sqlo_trace
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_trace, (dbh, on),
      sqlo_db_handle_t   dbh   AND
      int                on )
{
  int stat;
  if (on)
    stat = sqlo_exec(dbh, "ALTER SESSION SET SQL_TRACE TRUE",NULL);
  else
    stat = sqlo_exec(dbh, "ALTER SESSION SET SQL_TRACE FALSE",NULL);

  return stat;
}



/*---------------------------------------------------------------------------
 *         sqlo_geterror
 *--------------------------------------------------------------------------*/
const char *
DEFUN(sqlo_geterror, (dbh), sqlo_db_handle_t dbh)
{
  static char fatal_error[1024]; /* NOT THREAD SAFE !!! */

  sqlo_db_struct_ptr_t  dbp;

                               /* An invalid handle may be passed to us, if
                                * the connect failed. In this case we return
                                * the global error message.
                                */
  if ( !VALID_DBH_RANGE(dbh) || !_dbv[ dbh ]->used) {
    TRACE(1, fprintf(_get_trace_fp(NULL),
                     "Invalid Database handle %d in sqlo_geterror.\n",
                     dbh););
    if (*_errmsg) {
      TRACE(1, fprintf(_get_trace_fp(NULL), "Return _errmsg (%s)", _errmsg););
      return _errmsg;
    }
    sprintf(fatal_error, "Invalid dbh %d passed to sqlo_geterror", dbh);
    return (fatal_error);

  }  else {

    dbp = _dbv[ dbh ];

    if (!dbp) {
      TRACE(1, fprintf(_get_trace_fp(NULL),
                       "Invalid Database handle %d in sqlo_geterror.\n",
                       dbh););
      if (*_errmsg) {
        TRACE(1, fprintf(_get_trace_fp(NULL), "Return _errmsg (%s)", _errmsg););
        return _errmsg;
      }
      sprintf(fatal_error,
              "Invalid dbh %d passed to sqlo_geterror (points to NULL entry)",
              dbh);
      return (fatal_error);
    }

    /* If the error message is still empty (because of e.g. OCI_STILL_EXECUTING,
       we try to get it here
    */

    if ( '\0'  == *dbp->errmsg ) {
      (void)OCIErrorGet(dbp->errhp,
                        (ub4) 1,
                        (text *) NULL,
                        &dbp->errcode,
                        (text *)dbp->errmsg,
                        (ub4) SQLO_MAX_ERRMSG_LEN * sizeof(char),
                        OCI_HTYPE_ERROR);
#ifndef NDEBUG
      sprintf( dbp->errmsg,
              "%s\n(line: %d)\n", dbp->errmsg, __LINE__);
#endif
    }

    return (const char *) dbp->errmsg;
  }

}



/*---------------------------------------------------------------------------
 * sqlo_geterrcode
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_geterrcode, (dbh), sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_geterrcode", SQLO_INVALID_DB_HANDLE);

  assert( dbp != NULL );

  return(_get_errcode(dbp));

}



/*---------------------------------------------------------------------------
 *         sqlo_exists
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_exists, (dbh, table, colname, colval, where),
      sqlo_db_handle_t   dbh       AND
      const char *       table     AND
      const char *       colname   AND
      const char *       colval    AND
      const char *       where )
{
  char stmt[4096];
  int argc = 0;
  const char * argv[1];
  sqlo_stmt_handle_t sth = SQLO_STH_INIT;
  int status = 0;
  int retcode = SQLO_ERROR;

  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_exists", SQLO_INVALID_DB_HANDLE);

  if ( !table || *table == '\0' ) {
    sprintf(dbp->errmsg,"No table specified for sqlo_exists\n");
    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)); );
    return (SQLO_ERROR);
  }

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_exists: on %s\n", table););

  if (colval && *colval != '\0') {
    argv[0] = colval;
    argc = 1;
    sprintf(stmt, "select 'YES' from %s where %s = :b1",table, colname);
    if (where && *where != '\0') {
      strcat( stmt," and "); strcat (stmt, where);
    }
  } else {
    sprintf ( stmt, "select 'YES' from %s", table);
    if (where && *where != '\0') {
      strcat(stmt, " where "); strcat (stmt, where );
    }
  }

  TRACE(2, fprintf( _get_trace_fp(dbp), "sqlo_exists: %s\n",stmt););

  while (SQLO_STILL_EXECUTING == (status = sqlo_open2(&sth, dbh, stmt, argc, argv))) {
    TRACE(2, fprintf( _get_trace_fp(dbp), "sqlo_exists: sqlo_open2 status=%d (still executing) sth=%d\n",
                      status, sth););
    SQLO_USLEEP;
  }

  if (status < 0) {
      CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_exists", "sqlo_open2");
    }

  while (SQLO_STILL_EXECUTING == (status = sqlo_fetch(sth, 1)))  {
    SQLO_USLEEP;
  }

  if (status < 0) {
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_exists", "sqlo_fetch");
  } else if (status == 0) {
    retcode = SQLO_SUCCESS;           /* exists */
  } else {
    retcode = SQLO_NO_DATA;  /* not exists */
  }

  status = sqlo_close(sth);

  CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_exists", "sqlo_close");

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_exists: %s (%d)\n", retcode ? "NO" : "YES",
                   retcode););

  return retcode;
}



/*---------------------------------------------------------------------------
 *         sqlo_count
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_count, (dbh, table, colname, colval, where),
      sqlo_db_handle_t   dbh       AND
      const char *       table     AND
      const char *       colname   AND
      const char *       colval    AND
      const char *       where )
{
  char stmt[4096];
  int cnt = SQLO_ERROR;
  int argc = 0;
  const char * argv[1];
  const char **v;
  sqlo_stmt_handle_t sth = -1;
  int status;
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_count", SQLO_INVALID_DB_HANDLE);

  if ( !table || *table == '\0' ) {
    sprintf(dbp->errmsg, "No table specified for sqlo_count\n");
    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)); );
    return (SQLO_ERROR);
  }

  if ( colname && *colname != '\0' )
    sprintf (stmt, "select count (*) from %s ", table);
  else
    sprintf (stmt, "select count (*) from %s ", table);

  if (colval && *colval != '\0') {
    argv[0] = colval;
    argc = 1;
    sprintf ( &stmt[strlen ( stmt )], "where %s = :b1", colname);
    if (where && *where != '\0') {
      strcat(stmt, " and "); strcat (stmt, where);
    }
  } else {
    if (where && *where != '\0') {
      strcat( stmt, " where "); strcat (stmt, where);
    }
  }

  status = SQLO_SUCCESS;
  do {
    if (status == SQLO_SUCCESS)
    {
      SQLO_USLEEP;
    }
    status = sqlo_open2(&sth, dbh, stmt, argc, argv);
  } while (SQLO_STILL_EXECUTING == status);

  if (status < 0) {
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_count", "sqlo_open2");
  }

  status = SQLO_SUCCESS;
  do {

    if (status != SQLO_SUCCESS)
    {
      SQLO_USLEEP;
    }
    status = sqlo_fetch(sth, 1);
  } while (SQLO_STILL_EXECUTING == status);

  if (status < 0) {
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_count", "sqlo_fetch");
  } else if (status == 0) {
    v = sqlo_values(sth, NULL, 1);
    cnt = atoi(*v);
  } else {
    cnt = 0;
  }

  status = SQLO_SUCCESS;
  do {
    if (status != SQLO_SUCCESS)
    {
      SQLO_USLEEP;
    }
    status = sqlo_close(sth);
  } while (SQLO_STILL_EXECUTING == status);

  CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_count", "sqlo_close");

  return ( cnt );
}



/*---------------------------------------------------------------------------
 * sqlo_run
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_run, (dbh, stmt, argc, argv),
      sqlo_db_handle_t     dbh     AND
      const char *         stmt    AND
      int                  argc    AND
      const char **        argv )
{
  sqlo_db_struct_ptr_t  dbp;
  int ret;
  int status;
  sqlo_stmt_handle_t sth = -1;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_run", SQLO_INVALID_DB_HANDLE);

  TRACE(2,
  {
    int z ;
    for ( z=0 ; z < argc; z++) {
      fprintf(_get_trace_fp(dbp), "arg[%02d]: %s\n", z, argv[z] ? argv[z] : "NULL");
    }
  });


  status = SQLO_SUCCESS;
  do {
    if (status != SQLO_SUCCESS)
    {
      SQLO_USLEEP;
    }
    status = sqlo_open2(&sth, dbh, stmt, argc, argv);
  }  while (SQLO_STILL_EXECUTING == status);

  if (status < 0)
    return status;

  while (SQLO_STILL_EXECUTING == (ret = sqlo_fetch(sth, 1))) {
    SQLO_USLEEP;
  }

  if (0 <= ret )
        ret = sqlo_prows(sth);

  status = SQLO_SUCCESS;
  do {
    if (status != SQLO_SUCCESS)
    {
      SQLO_USLEEP;
    }
    status = sqlo_close(sth);
  } while ( SQLO_STILL_EXECUTING == status);

  return ret;
}



/*---------------------------------------------------------------------------
 *         sqlo_exec
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_exec, (dbh, stmt,rr),
      sqlo_db_handle_t   dbh   AND
      const char *       stmt  AND
      ub4 * rr )
{
  sqlo_db_struct_ptr_t  dbp;

  ub4 prows;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_exec", SQLO_INVALID_DB_HANDLE);

  assert( dbp != NULL );
  assert( stmt != NULL );

  /* we can use a local variable, because we will never return a
   * SQLO_OCI_STILL_EXECUTING
   */
  /* Use the stmthp in dbp. */

  if (!(dbp->stmthp == NULL))
  {
    (void)OCIHandleFree(dbp->stmthp, OCI_HTYPE_STMT);
    dbp->stmthp = NULL;
  }

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_exec: prepare: %s\n", stmt););

  dbp->status = OCIHandleAlloc( (dvoid *) dbp->envhp,
                                  (dvoid **) &dbp->stmthp,
                                  OCI_HTYPE_STMT,
                                  (size_t) 0,
                                  (dvoid **) 0
                                  );

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                            "sqlo_exec", "OCIHandleAlloc(stmt)");

  dbp->status = OCIStmtPrepare( dbp->stmthp,
                                  dbp->errhp,
                                  (text *)stmt,
                                  strlen(stmt),
                                  OCI_NTV_SYNTAX,
                                  OCI_DEFAULT);

  CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                             "sqlo_exec(prepare)", (char*)stmt);
//  } else {
    /* still executing */
//    ;
//  }

  dbp->status = OCIStmtExecute( dbp->svchp,
                                dbp->stmthp,
                                dbp->errhp,
                                (ub4) 1,
                                (ub4) 0,
                                ( OCISnapshot *) 0,
                                (OCISnapshot *) 0,
                                dbp->exec_flags
                                );

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_exec: OCIStmtExecute returns %d\n", dbp->status););

  if (dbp->status == OCI_SUCCESS) {

    /* finished the call. Get row count */
    dbp->status = OCIAttrGet( (dvoid*)dbp->stmthp,
                              (ub4) OCI_HTYPE_STMT,
                              (dvoid *)  &prows,
                              (ub4 *) 0,
                              (ub4) OCI_ATTR_ROW_COUNT,
                              dbp->errhp
                              );

    if (dbp->status == OCI_SUCCESS ) {
      /* SUCCESS */
      *rr=prows;
      dbp->status =  OCIHandleFree(dbp->stmthp, OCI_HTYPE_STMT);

      CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_exec", "OCIHandleFree");
      dbp->stmthp = NULL;

    }
    else
    {
      /* ERROR */
      (void)OCIHandleFree(dbp->stmthp, OCI_HTYPE_STMT);

      dbp->stmthp = NULL;

      return (dbp->status);
    }
  } else if (dbp->status == OCI_STILL_EXECUTING) { /* Execute still processing= */

    return (SQLO_STILL_EXECUTING);

  }
  else
  {
    /* ERROR IN EXECUTE */
    OCIHandleFree(dbp->stmthp, OCI_HTYPE_STMT);

    dbp->stmthp = NULL;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_exec", "OCIStmtExecute");

  }

  return ( dbp->status );
}



/*---------------------------------------------------------------------------
 * sqlo_open
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_open,(dbh, stmt, argc, argv),
      sqlo_db_handle_t    dbh    AND
      const char *        stmt   AND
      int                 argc   AND
      const char **       argv )
{
  sqlo_db_struct_ptr_t  dbp;
  sqlo_stmt_struct_ptr_t stp;
  int status;
  int ret;
  bool_t bmf = FALSE;                   /* flag indicates change in blocking mode */
  unsigned int blocking=SQLO_STH_INIT;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_open", SQLO_INVALID_DB_HANDLE);

  ret = SQLO_SUCCESS;

  /* if we are in non-blocking mode, we switch back to blocking */
  status = _get_blocking_mode(dbp, &blocking);
  if (status < 0)
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open", "sqlo_set_blocking_i");

  if (SQLO_OFF == blocking) {
    status = sqlo_set_blocking(dbh, 1);
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open", "sqlo_set_blocking(dbh, 1)");
    bmf = TRUE;
  }

  /* Get a statement handle pointer */
  status = _stmt_new(dbp, stmt, &stp);
  CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open", "_stmt_new");

  TRACE(2, fprintf(_get_trace_fp(dbp), "Parse: %s\n", stmt););

  /* prepare the statement and determine the statement type */
  status = _prepare(stp, stmt, &(stp->stype));
  CHECK_OCI_STATUS(dbp, status, "sqlo_open", "_prepare");

  if (OCI_SUCCESS != status) {
    CHECK_OCI_STATUS(dbp,  sqlo_close( ENCODE_STH(stp->sth, dbh) ), "sqlo_open", "sqlo_close");
    return (status);
  }

  /* We cannot handle PL/SQL blocks here */
  if ( _is_plsql(stp) ) {
    status = SQLO_INVALID_STMT_TYPE;

    CHECK_OCI_STATUS(dbp, sqlo_close( ENCODE_STH(stp->sth, dbh)),
                     "sqlo_open", "sqlo_close, ERROR: INVALID STMT TYPE");
    return (status);
  }

  if ( _is_query(stp) ) {

    /* Set the prefetch count to _num_prefetch_rows */
    status = _set_prefetch_rows(stp, _num_prefetch_rows);
    CHECK_OCI_STATUS(dbp, status, "sqlo_open", "_set_prefetch_rows");

    if (OCI_SUCCESS != status) {
      CHECK_OCI_STATUS(dbp,  sqlo_close( ENCODE_STH(stp->sth, dbh) ), "sqlo_open", "sqlo_close");

      return (status);
    }
  }

  ret = _sqlo_reopen(stp, argc, argv);


  /* switch back to non-blocking mode */
  if (bmf) {
    status = sqlo_set_blocking(dbh, 0 );
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open", "sqlo_set_blocking(dbh, 0)");

  }

  return( (SQLO_SUCCESS == ret) ? (int) ENCODE_STH(stp->sth, dbh) : ret);

}



/*---------------------------------------------------------------------------
 * sqlo_open2
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_open2,(sthp, dbh, stmt, argc, argv),
      sqlo_stmt_handle_t *   sthp   AND
      sqlo_db_handle_t       dbh    AND
      const char *           stmt   AND
      int                    argc   AND
      const char **          argv )
{
  sqlo_db_struct_ptr_t  dbp;
  sqlo_stmt_struct_ptr_t stp = NULL;
  int status;
  int ret;
  unsigned int blocking=SQLO_STH_INIT;
  int real_sth;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_open2", SQLO_INVALID_DB_HANDLE);
  if (!sthp)
    return SQLO_INVALID_STMT_HANDLE;

  if (*sthp == SQLO_STH_INIT)
    real_sth = -1;
  else
    real_sth = (int) DECODE_STH(*sthp);

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_open2: dbh=%d sth=%d\n", dbh, real_sth););


  /* check if the statement handle passed to us is valid, if we
   * are in non-blocking mode.*/
  status = _get_blocking_mode(dbp, &blocking);

  if (status < 0)
    CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open2", "_get_blocking_mode");

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_open2: blocking=%d\n", blocking););

  if ( SQLO_OFF == blocking ) {
    stp = &(dbp->stmtv[ real_sth ]);

    if (stp != NULL) {
      TRACE(3,
            fprintf( _get_trace_fp(dbp),
                     "sqlo_open2: sth=%d, used=%d, opened=%d, is_query=%d, still_executing=%d\n",
                     real_sth, stp->used, _is_opened(stp), _is_query(stp), stp->still_executing););
    } else {
      TRACE(1,
            fprintf( _get_trace_fp(dbp),
                     "sqlo_open2: NULL pointer exception: stp is NULL");
            );
    }

    if ( real_sth >= 0 &&
         real_sth < (int) dbp->stmtv_size &&
         (stp != NULL) &&
         stp->used &&
         !_is_opened(stp) &&
         _is_query(stp) &&
         stp->still_executing) {

      return( _sqlo_reopen(stp, argc, argv) );
    }
  }

  /* Get a statement handle pointer */
  status = _stmt_new(dbp, stmt, &stp);
  CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_open2", "_stmt_new");

  TRACE(2, fprintf( _get_trace_fp(dbp), "Parse: %s\n", stmt););

  /* prepare the statement and determine the statement type */
  status = _prepare(stp, stmt, &(stp->stype));
  CHECK_OCI_STATUS(dbp, status, "sqlo_open2", "_prepare");

  if (OCI_SUCCESS != status) {
    CHECK_OCI_STATUS(dbp,  sqlo_close( ENCODE_STH(stp->sth, dbh) ), "sqlo_open2", "sqlo_close");
    return (status);
  }

  /* We cannot handle PL/SQL blocks here */
  if ( _is_plsql(stp) ) {
    status = SQLO_INVALID_STMT_TYPE;

    CHECK_OCI_STATUS(dbp, sqlo_close( ENCODE_STH(stp->sth, dbh)), "sqlo_open2", "sqlo_close, ERROR: INVALID STMT TYPE");

    return (status);
  }

  if ( _is_query(stp) ) {
    /* Set the prefetch count to _num_prefetch_rows */
    status = _set_prefetch_rows(stp, _num_prefetch_rows);

    CHECK_OCI_STATUS(dbp, status, "sqlo_open2", "_set_prefetch_rows");

    if (OCI_SUCCESS != status) {
      CHECK_OCI_STATUS(dbp,  sqlo_close( ENCODE_STH(stp->sth, dbh) ), "sqlo_open2", "sqlo_close");
      return (status);
    }
  }

  ret = _sqlo_reopen( stp, argc, argv);

  if ( SQLO_SUCCESS == ret || SQLO_STILL_EXECUTING == ret) {
    *sthp = ENCODE_STH(stp->sth, dbh);
  }

  return (ret);

}



/*---------------------------------------------------------------------------
 * sqlo_reopen
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_reopen,  (sth, argc, argv),
       sqlo_stmt_handle_t    sth    AND
       int                   argc   AND
       const char **         argv )
{
  sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_reopen", SQLO_INVALID_STMT_HANDLE);

  return _sqlo_reopen(stp, argc, argv);
}



/*---------------------------------------------------------------------------
 * sqlo_fetch
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_fetch, (sth, nrows),
      sqlo_stmt_handle_t    sth    AND
      unsigned int          nrows )
{
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t  dbp;
  register unsigned int col_idx;
  int localStatus;

  CHECK_STHANDLE(stp, sth, "sqlo_fetch", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( stp->dbp->svchp != NULL );

  dbp = stp->dbp;

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_fetch sth=%u, nrows=%u\n",
                   stp->sth, nrows););


  if ( ! _is_query(stp) )
  {
    if (!stp->still_executing)
    {
      TRACE(2, fprintf(_get_trace_fp(dbp), "Exec [%2u] %.65s\n",stp->sth,
                       _get_stmt_string(stp)););
    }

    dbp->status = OCIStmtExecute( dbp->svchp,
                                  stp->stmthp,
                                  dbp->errhp,
                                  (ub4) 1,
                                  (ub4) 0,
                                  (OCISnapshot *) 0,
                                  (OCISnapshot *) 0,
                                  dbp->exec_flags
                                  );

    if (OCI_SUCCESS != dbp->status) {
      if (OCI_STILL_EXECUTING == dbp->status) {
        stp->still_executing = TRUE;
      } else {
        _save_oci_status(dbp, "sqlo_fetch", "OCIStmtExecute(nonquery)",
                         __LINE__);
      }
    } else {
      stp->still_executing = FALSE;
    }
  }
  else
  {
#ifdef HAVE_OCISTMTFETCH2
    dbp->status = OCIStmtFetch2( stp->stmthp,
                                 dbp->errhp,
                                 nrows,
                                 OCI_FETCH_NEXT,
                                 0,
                                 OCI_DEFAULT
                                 );
#else
    /* deprecated in new versions of Oracle (9i) */
    dbp->status = OCIStmtFetch(stp->stmthp, dbp->errhp, nrows,
                               OCI_FETCH_NEXT, OCI_DEFAULT);
#endif
    TRACE(3, fprintf(_get_trace_fp(dbp), "OCIStmtFetch finished with %d\n",
                     dbp->status););

    if ( OCI_STILL_EXECUTING != dbp->status )
    {
      /* Cycle the columns to alloc and retrieve memo fields */
      for (col_idx = 0; col_idx < stp->num_defnpv; ++col_idx)
      {
        if( stp->ocolsv[ col_idx ].database_dtype == SQLT_CLOB || stp->ocolsv[ col_idx ].database_dtype == SQLT_BLOB )
        {
          stp->outv_size[ col_idx ] = 0;

          if( stp->oindv[ col_idx ] != SQLO_NULL_IND )
          {
            if (dbp->status == OCI_SUCCESS || dbp->status == SQLO_SUCCESS_WITH_INFO)
            {
              localStatus = OCILobGetLength( dbp->svchp,
                                             dbp->errhp,
                                             (OCILobLocator *) stp->ocolsv[ col_idx ].loblp,
                                             (ub4*) &(stp->outv_size[ col_idx ]) );

              // TraceLog( LOGFILE, "OCILobGetLength col %i, out length %u, allocated %u\n", col_idx, stp->outv_size[ col_idx ], stp->rlenv[ col_idx ]);

              if( localStatus == OCI_SUCCESS && stp->outv_size[ col_idx ] )
              {
                _alloc_ocol_buffer( stp, col_idx+1, ((unsigned int) (stp->outv_size[ col_idx ])) + 1 );
                stp->outv_size[ col_idx ] = stp->outv_size[ col_idx ] - 1;

                // TraceLog(LOGFILE, "col lob %i allocated %i, used %i at %p\n", col_idx, stp->rlenv[ col_idx ], stp->outv_size[ col_idx ], stp->outv[ col_idx ] );

                localStatus = sqlo_lob_read_buffer(dbp->dbh,
                                    (OCILobLocator *) stp->ocolsv[ col_idx ].loblp,
                                    (unsigned int) stp->outv_size[ col_idx ],
                                    (void *) (stp->outv[ col_idx ]),
                                    (unsigned int) stp->outv_size[ col_idx ] );

                if( localStatus == OCI_ERROR )
                {
                   TraceLog( LOGFILE, "col %i status %i - error reading a %i bytes lob located by %p, dbh %p\n", col_idx, dbp->status, stp->outv_size[ col_idx ], stp->ocolsv[ col_idx ].loblp, dbp->dbh );
                }
              }  /* Success in GetLen() */
              else
              {
                // TraceLog( LOGFILE, "col %i status %i - error reading lob length (read %u)\n", col_idx, dbp->status, stp->outv_size[ col_idx ] );
              }
            }  /* if is there any data */
          }  /* if lob is null */
        }  /* if is lob */
      }  /* for() */
    }

    if ( dbp->status != OCI_SUCCESS && dbp->status != OCI_NO_DATA && dbp->status != SQLO_SUCCESS_WITH_INFO )
    {
      if ( OCI_STILL_EXECUTING == dbp->status )
      {
        stp->still_executing = TRUE;
        return ( SQLO_STILL_EXECUTING );
      }
      CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_fetch", "OCIStmtFetch");
    }
    else
    {
      stp->still_executing = FALSE;
    }
  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_values
 *-------------------------------------------------------------------------*/
const char **
DEFUN(sqlo_values, (sth, num, do_strip_string),
       sqlo_stmt_handle_t   sth               AND
       int *                num               AND
       int                  do_strip_string )
{
  sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_values", NULL);


  if ( !_is_query(stp) || !_is_opened(stp) ) {
    sprintf(stp->dbp->errmsg,
            "Cannot get values for a non-select/non-opened statement (sth %u) passed to sqlo_values\n", stp->sth );
    TRACE(1, (void) fputs(stp->dbp->errmsg, _get_trace_fp(stp->dbp)););
    if (num)
      *num = 0;
        return NULL;
  }


  TRACE(2, fprintf(_get_trace_fp(stp->dbp), "Get values [%2u]. _strip_string: %d\n",
                   stp->sth, do_strip_string););

  _terminate_ocols(stp, do_strip_string);

  if (num)
    *num = (int)stp->num_defnpv;

  return (const char **) stp->outv;
}



/*---------------------------------------------------------------------------
 *        sqlo_prows
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_prows, (sth), sqlo_stmt_handle_t sth)
{
  sqlo_stmt_struct_ptr_t  stp;
  ub4 prows;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_prows", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );

  dbp = stp->dbp;

  if ( !_is_opened(stp) ) {
    sprintf(dbp->errmsg,
            "Cannot get processed rows for a non-executed/fetched statement "
            "(sth %u) passed to sqlo_prows\n", stp->sth);
    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););
    return SQLO_INVALID_STMT_HANDLE;

  }


  dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                            (ub4) OCI_HTYPE_STMT,
                            (dvoid *)  &prows,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_ROW_COUNT,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_prows(GetAttr)", "ROW_COUNT");

  TRACE(2, fprintf(_get_trace_fp(dbp), "Get proc. rows [%2u]: %u\n",
                   stp->sth,
                   prows););

  return( (int) prows);
}



/*---------------------------------------------------------------------------
 *        sqlo_ncols
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_ncols, (sth, in),
      sqlo_stmt_handle_t    sth    AND
      int                   in )
{
  ub4 ncols;
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t  dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_ncols", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( stp->dbp->svchp != NULL );

  dbp = stp->dbp;

  TRACE(2, fprintf(_get_trace_fp(stp->dbp), "Get NCols [%2u] for %s desc\n",
                   stp->sth, in ? "in" : "out" ););

  if (in)
      ncols = (ub4) stp->num_bindpv;
  else {

    if (0 == stp->num_executions && ! (REFCURSOR == stp->cursor_type) ) {
      /* execute to describe the output */
      while (OCI_STILL_EXECUTING ==
             (dbp->status = OCIStmtExecute( dbp->svchp,
                                            stp->stmthp,
                                            dbp->errhp,
                                            (ub4) 0,
                                            (ub4) 0,
                                            (OCISnapshot *) 0,
                                            (OCISnapshot *) 0,
                                            (ub4) OCI_DEFAULT))) {
        SQLO_USLEEP;
      }
      CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_ncols",
                              "OCIStmtExecute(DESCRIBE)");
      ++(stp->num_executions);
    }

    dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                              (ub4) OCI_HTYPE_STMT,
                              (dvoid *)  &ncols,
                              (ub4 *) 0,
                              (ub4) OCI_ATTR_PARAM_COUNT,
                              dbp->errhp);

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_ncols",
                            "OCIAttrGet(NumberOfColumns)");
  }

  TRACE(3, fprintf(_get_trace_fp(dbp), "  NCols: %u\n", (unsigned int) ncols););

  return( (int)ncols);
}


/*---------------------------------------------------------------------------
 *        sqlo_command
 *-------------------------------------------------------------------------*/
const char *
DEFUN(sqlo_command, (sth), sqlo_stmt_handle_t sth)
{
  sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_command", NULL);
  return(_get_stmt_string(stp));

}



/*---------------------------------------------------------------------------
 *        sqlo_close
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_close, (sth), sqlo_stmt_handle_t sth)
{
  sqlo_stmt_struct_ptr_t  stp;            /* Statement pointer */
  sqlo_db_struct_ptr_t dbp;

  unsigned defnp_idx;           /* Loop index for defnp */

  CHECK_STHANDLE(stp, sth, "sqlo_close", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_close [%2u]: %.60s\n",
                   stp->sth,
                   _get_stmt_string(stp)););

  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_close [%2u]: opened=%d, prepared=%d\n",
                   stp->sth, _is_opened(stp), _is_prepared(stp) ););

  dbp->status = SQLO_SUCCESS;

  _bindpv_reset(stp);

  for (defnp_idx = 0; defnp_idx < stp->num_defnpv; ++defnp_idx) {

    /* deallocate memory allocated for LONG columns */
    if (stp->outv_size[ defnp_idx ] >= _max_long_size)
    {
      XFREE( stp->outv[ defnp_idx ], __LINE__ );
      stp->outv[ defnp_idx ] = NULL;
      stp->outv_size[ defnp_idx ] = 0;
    }
/*
    // Pool descriptors !!!

    if( stp->ocolsv[ defnp_idx ].loblp )
    {
      OCIDescriptorFree((dvoid **) &(stp->ocolsv[ defnp_idx ].loblp), (ub4) OCI_DTYPE_LOB);
      // TraceLog(LOGFILE, "col %i, OCIDescriptorFree 3 %p\n", defnp_idx, stp->ocolsv[ defnp_idx ].loblp );
      stp->ocolsv[ defnp_idx ].loblp = NULL;
    }
*/
  } /* end for defnp_idx */

  memset(stp->defnpv, 0, stp->num_defnpv * sizeof(OCIDefine *));
  stp->num_defnpv = 0;

  if ( stp->stmthp ) {
    dbp->status = OCIHandleFree(stp->stmthp, OCI_HTYPE_STMT);

    stp->stmthp = NULL;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_close", "OCIHandleFree(stmthp)");
  }

  _stmt_release( stp );       /* make sth available to others. */

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *        sqlo_print
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_print, (sth), sqlo_stmt_handle_t sth )
{
  sqlo_stmt_struct_ptr_t  stp;
  register unsigned int col_idx;

  CHECK_STHANDLE(stp, sth, "sqlo_print", SQLO_INVALID_STMT_HANDLE);

  printf("Stmt in sth %u: %s\n", stp->sth, stp->stmt);

  if (! _is_opened(stp) )
    printf(" not ");
  printf(" opened, ");

  if (! _is_prepared(stp) )
    printf(" not ");
  printf(" prepared, ");

  printf("Stmt Type: %d (%s)\n", (int)stp->stype, _get_stmt_type_str((int)stp->stype));
  printf("N-Bindpv: %u, (allocated: %u)\n", stp->num_bindpv, stp->bindpv_size);
  printf("prows: %d\n", sqlo_prows(sth));


  if ( _is_query(stp) ) {

    _set_all_ocol_names(stp);        /* make sure the output column names are set */

    for (col_idx = 0; col_idx < stp->num_defnpv; ++col_idx) {
      printf("Colum Name[%02u]  : %s\n", col_idx, stp->ocolsv[ col_idx ].col_name);
      printf("Datatype        : %d (%s)\n", (int)stp->ocolsv[ col_idx ].dtype,
             _get_data_type_str((int)stp->ocolsv[ col_idx ].dtype));
      printf("Buffer size     : %u\n", stp->outv_size[ col_idx ]);
      printf("DB size         : %d\n", (int) stp->ocolsv[ col_idx ].dbsize);
      printf("Prec            : %d\n", (int) stp->ocolsv[ col_idx ].prec);
      printf("Scale           : %d\n", (int) stp->ocolsv[ col_idx ].scale);
      printf("NullOk          : %d\n", (int) stp->ocolsv[ col_idx ].nullok);
    }

  }

  return SQLO_SUCCESS;
}



/*---------------------------------------------------------------------------
 *        sqlo_split_cstring
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_split_cstring, (cstr, uid, pwd, tnsname, bufsize),
      const char *     cstr      AND
      char *           uid       AND
      char *           pwd       AND
      char *           tnsname   AND
      unsigned int     bufsize )
{
  char *c = (char*) cstr;
  unsigned int n;
  char * env_ptr;

  if (!cstr || !uid || !pwd || !tnsname || !bufsize)
    return (SQLO_ERROR);

  /* extract username, password and tnsname from the connect string */
  *uid = '\0';
  *pwd = '\0';
  *tnsname = '\0';

  /* copy username part to uid */
  n = 0;
  while ( *c && *c !='/' ) {
    if (n >=  bufsize )
      return SQLO_ERROR;
    *(uid++) = *(c++);
    ++n;
  }

  *uid = '\0';

  /* copy password part, if present */
  if ( *c == '/') {
    ++c;
    n = 0;
    while( *c && *c != '@')  {
      if (n >= bufsize)
        return SQLO_ERROR;
      *(pwd++) = *(c++);
      ++n;
    }
    *pwd = '\0';
  }

  /* copy tnsname if present, otherwise we use oracle sid */
  if( *c == '@' )  {
    ++c;
    n = 0;
    while ( *c != '\0' ) {
      if (n >= bufsize)
        return SQLO_ERROR;
      *(tnsname++) = *(c++);
      ++n;
    }
    *tnsname = '\0';
  } else {
    if ( (env_ptr = getenv("ORACLE_SID")) ) {
      strncpy(tnsname, env_ptr, bufsize-1);
      tnsname[bufsize-1] = '\0';
    }
  }
  return SQLO_SUCCESS;

}



/*---------------------------------------------------------------------------
 *        sqlo_server_attach
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_server_attach, (dbhp, tnsname),
      sqlo_db_handle_t *   dbhp      AND
      const char *         tnsname )
{
  sqlo_db_struct_ptr_t  dbp;

  if (dbhp)
    *dbhp = -1;

  TRACE(2, fprintf(_get_trace_fp(NULL), "sqlo_server_attach starts\n" ););

  if (!(dbp = _db_add())) {
    TRACE(3, fprintf(_get_trace_fp(NULL),
                     "sqlo_server_attach: Could not alloacte a dbp\n"); );
    return SQLO_ERROR;
  }

  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_server_attach: allocated dbh=%u\n",
                   dbp->dbh); );

  /* if tnsname contains a connect string we split it */
  if (strchr(tnsname, '@') || strchr(tnsname, '/') ) {
    enum {MAX_BUFLEN = 1023};
    char uid[MAX_BUFLEN + 1];
    char pwd[MAX_BUFLEN + 1];
    char dbname[MAX_BUFLEN + 1];

    if (SQLO_SUCCESS != sqlo_split_cstring(tnsname, uid, pwd, dbname, MAX_BUFLEN))
      return SQLO_ERROR;
    dbp->tnsname = strdupx(dbname);

  } else {
    dbp->tnsname = strdupx(tnsname);
  }

  dbp->status = SQLO_SUCCESS;
  dbp->exec_flags = OCI_DEFAULT;
  dbp->errmsg[0] = '\0';

  /* We return the handle even on error, so sql_geterror can return
   * the stored message
   */
  if (dbhp)
    *dbhp = (int) dbp->dbh;

  /*
    We must use OCIEnvCreate instead of OCIInitialize / OCIEnvInit (see Oracle OCI
    Programmer's Guide 8.1.6 pg 15-91 - 15-93). If during the first call to OCIEnvCreate
    an OCI_THREADED parameter is specified then this parameter will apply all along.
    CAFL says fb doesn't have the earlier manual...which says to use
    ocienvinit to create the handle.  The real question is whether to pass
    OCI_NO_MUTEX to it, which would probably be right here where there is one
    environment handle per connection, or OCI_DEFAULT.  I have no guts.
  */

  /* There are roumors that OCIEnvCreate is not thread safe. I'm not sure that this
   * is true, but to be on the safe side, I protect it by a lock
   */
  EXEC_WHEN_THREADING(_env_lock(););

#ifdef HAVE_OCIENVCREATE
  dbp->status = OCIEnvCreate( &dbp->envhp,
                              _oci_init_mode,
                              NULL, NULL, NULL, NULL, 0, NULL
                              ) ;
  CHECK_OCI_STATUS(dbp, dbp->status, "sqlo_server_attach", "OCIEnvCreate" );
#else
  dbp->status = OCIEnvInit( (OCIEnv **)((dvoid *)&dbp->envhp),
                            _oci_init_mode,
                            0, (dvoid **)0
                            );
  CHECK_OCI_STATUS(dbp, dbp->status, "sqlo_server_attach", "OCIEnvInit" );
#endif

  EXEC_WHEN_THREADING( _env_unlock(); );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release( dbp );
    return ( dbp->status );
  }

  /* Alloc the service context handle */
  dbp->status = OCIHandleAlloc( (dvoid *)dbp->envhp,
                                (dvoid **)&dbp->svchp,
                                OCI_HTYPE_SVCCTX,
                                0,
                                (dvoid **) 0
                                );

  CHECK_OCI_STATUS(dbp, dbp->status,
                   "sqlo_server_attach", "OCIEnvHandleAlloc(svchp)" );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release( dbp );
    return (dbp->status);
  }

  /* Alloc the error handle */
  dbp->status = OCIHandleAlloc( (dvoid *)dbp->envhp,
                                (dvoid **)&dbp->errhp,
                                OCI_HTYPE_ERROR,
                                0, (dvoid **) 0
                                );

  CHECK_OCI_STATUS(dbp, dbp->status, "sqlo_server_attach",
                   "OCIHandleAlloc(errhp)" );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release( dbp );
    return ( dbp->status );
  }

  /* Alloc the server handle */
  dbp->status = OCIHandleAlloc( (dvoid *)dbp->envhp,
                                (dvoid **)&dbp->srvhp,
                                OCI_HTYPE_SERVER,
                                0,
                                (dvoid **) 0
                                );

  CHECK_OCI_STATUS(dbp, dbp->status,
                   "sqlo_server_attach", "OCIHandleAlloc(srvhp)" );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release(dbp);
    return (dbp->status);
  }

  /* Create a server context */
  dbp->status = OCIServerAttach( dbp->srvhp,
                                 dbp->errhp,
                                 (text *)dbp->tnsname,
                                 strlen(dbp->tnsname),
                                 OCI_DEFAULT
                                 );
  CHECK_OCI_STATUS(dbp, dbp->status,
                   "sqlo_server_attach", "OCISeverAttach(tnsname)" );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release( dbp );
    return ( dbp->status );
  }

  /* Set the server context in the service context */
  dbp->status = OCIAttrSet( (dvoid *)dbp->svchp,
                            OCI_HTYPE_SVCCTX,
                            (dvoid *)dbp->srvhp,
                            (ub4) 0,
                            OCI_ATTR_SERVER,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS(dbp, dbp->status,
                   "sqlo_server_attach", "OCIAttrSet(server->service)" );

  /* release allocated resources */
  if (OCI_SUCCESS != dbp->status) {
    _db_release( dbp );
    return ( dbp->status );
  }

  /* Allocate a authentication handle */
  dbp->status = OCIHandleAlloc( (dvoid *)dbp->envhp,
                                (dvoid **)&dbp->authp,
                                OCI_HTYPE_SESSION,
                                0,
                                (dvoid **) 0
                                );

  CHECK_OCI_STATUS( dbp, dbp->status,
                   "sqlo_server_attach", "OCIHandleAlloc(authp)" );

  if (SQLO_SUCCESS == dbp->status) {
    TRACE(2,fprintf( _get_trace_fp(dbp), "sqlo_server_attach[%d]: attached\n", *dbhp); );
  } else {
    /* release allocated resources */
    TRACE(2,fprintf( _get_trace_fp(dbp), "sqlo_server_attach[%d]: failed\n", *dbhp); );
    _db_release( dbp );
    return ( dbp->status );
  }

  dbp->used   = TRUE;
  dbp->attached = TRUE;

  return ( dbp->status );
}



/*---------------------------------------------------------------------------
 *        sqlo_session_begin
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_session_begin, (dbh, username, password),
      sqlo_db_handle_t   dbh        AND
      const char *       username   AND
      const char *       password )
{
  sqlo_db_struct_ptr_t  dbp;
  enum {MAX_BUFLEN = 1023};
  char uid[MAX_BUFLEN + 1];
  char pwd[MAX_BUFLEN + 1];
  char tnsname[MAX_BUFLEN + 1];

  if ( !VALID_DBH_RANGE(dbh) || !_dbv[ dbh ]->used || !_dbv[ dbh ]->attached
      || _dbv[ dbh ]->session_created ) {
    TRACE(1, fprintf(_trace_fp, "Invalid Database handle %d in sqlo_session_begin\n",
                     dbh););
    return SQLO_INVALID_DB_HANDLE;
  }

  dbp = _dbv[ dbh ];             /* setup the pointer to the db structure */
  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_session_begin[%d] starts\n", dbh); );

  assert( dbp != NULL );

  if (!username || !password) {
    strcpy(dbp->errmsg, "sqlo_sesson_begin: No username or password specified");
    return SQLO_ERROR;
  }

  if (strchr(username, '/')) {
    if (SQLO_SUCCESS != sqlo_split_cstring(username, uid, pwd, tnsname, MAX_BUFLEN))
      return SQLO_ERROR;
  } else {
    strncpy(uid, username, (size_t)MAX_BUFLEN - 1);
    uid[MAX_BUFLEN] = '\0';

    strncpy(pwd, password, (size_t)MAX_BUFLEN - 1);
    pwd[MAX_BUFLEN] = '\0';
  }

  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_session_begin: uid=%s, pwd=%s\n", uid, pwd););

  /* Set username attribute in session handle */
  dbp->status = OCIAttrSet( (dvoid *)dbp->authp,
                            OCI_HTYPE_SESSION,
                            (dvoid *) uid,
                            (ub4)strlen(uid),
                            OCI_ATTR_USERNAME,
                            dbp->errhp
                            );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_session_begin", "OCIAttrSet(username)");

  /* Set password attribute in user session handle */
  dbp->status = OCIAttrSet( (dvoid *)dbp->authp,
                            OCI_HTYPE_SESSION,
                            (dvoid *) pwd,
                            (ub4)strlen(pwd),
                            OCI_ATTR_PASSWORD,
                            dbp->errhp);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_session_begin", "OCIAttrSet(password)");

  /*
   * In non-blocking mode OCISessionBegin might return OCI_ERROR, but the
   * error code is 3123 (OCI_STILL_EXECUTING).
   * We catch this case here and wait until we are connected
   */
  while ( OCI_STILL_EXECUTING ==
          (dbp->status = OCISessionBegin( (OCISvcCtx *)((dvoid *) dbp->svchp),
                                          dbp->errhp,
                                          dbp->authp,
                                          OCI_CRED_RDBMS,
                                          OCI_DEFAULT))
          || (dbp->status == OCI_ERROR &&
              _get_errcode(dbp) == ((-1) * OCI_STILL_EXECUTING) )) {
    TRACE(2, fprintf(_get_trace_fp(dbp),
                     "sqlo_session_begin: "
                     "Still executing OCISessionBegin\n"););
    SQLO_USLEEP;
  }

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_session_begin", "OCISessionBegin");

  /* Set the authentication handle in the service context */
  while (OCI_STILL_EXECUTING ==
         (dbp->status = OCIAttrSet( (dvoid *)dbp->svchp,
                                    OCI_HTYPE_SVCCTX,
                                    (dvoid *)dbp->authp,
                                    (ub4) 0,
                                    OCI_ATTR_SESSION,
                                    dbp->errhp)) ){
    TRACE(2, fprintf(_get_trace_fp(dbp),"Still executing OCIAttrSet (authp -> svchp)\n"););
    SQLO_USLEEP;
  }

  /* set dbp->errcode */
  sqlo_geterrcode( dbh );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_session_begin", "OCIAttrSet(auth->service)");

  dbp->session_created = TRUE;

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_session_begin[%d]: logged in\n", dbh); );

  if (TRACE_ENABLED && _trace_level > 0)
    _open_session_trace_file(dbp);

  return ( dbp->status );
}



/*---------------------------------------------------------------------------
 *        sqlo_server_detach
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_server_detach, (dbh), sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t  dbp;

  TRACE(2, fprintf(_get_trace_fp(NULL), "sqlo_server_detach starts dbh=%d\n", dbh););

  if ( !VALID_DBH_RANGE(dbh) || !_dbv[ dbh ]->used || !_dbv[ dbh ]->attached ) {
    TRACE(1, fprintf(_trace_fp, "Invalid Database handle %d in sqlo_server_detach\n",
                     dbh););
    return SQLO_INVALID_DB_HANDLE;
  }

  dbp = _dbv[ dbh ];             /* setup the pointer to the db structure */

  assert( dbp != NULL );

  /* forgot to call sqlo_session_end ? */
  if (dbp->session_created) {
    if (SQLO_SUCCESS != sqlo_session_end( dbh ) ) {
      CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                              "sqlo_server_detach", "sqlo_session_end");
    }
  }

  dbp->status = OCIServerDetach(dbp->srvhp, dbp->errhp, OCI_DEFAULT );

  if (OCI_SUCCESS != dbp->status) {
    CHECK_OCI_STATUS(dbp, dbp->status,
                     "sqlo_server_detach", "OCIServerDetach");
    _db_release( dbp );
    return ( dbp->status );
  }

  /* release the allocated resources */
  _db_release( dbp );

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_server_detach[%d]: detached\n", dbh); );

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *      sqlo_server_free (by jop)
 *
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_server_free, (dbh), sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_server_free", SQLO_INVALID_DB_HANDLE);

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_server_free[%d] starts\n", dbh); );
  assert( dbp != NULL );

  if (dbp->attached)
    dbp->status = OCIServerDetach( dbp->srvhp, dbp->errhp, OCI_DEFAULT );

  _db_release( dbp );

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_server_free[%d] finished with success\n", dbh); );

  return dbp->status;
}



/*---------------------------------------------------------------------------
 *        sqlo_session_end
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_session_end, (dbh),  sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t  dbp;

  if ( !VALID_DBH_RANGE(dbh) || !_dbv[ dbh ]->used ||
      !_dbv[ dbh ]->session_created ) {
    TRACE(1, fprintf(_trace_fp, "Invalid Database handle %d in sqlo_session_end\n",
                     dbh););
    return SQLO_INVALID_DB_HANDLE;
  }


  dbp = _dbv[ dbh ];             /* setup the pointer to the db structure */
  assert( dbp != NULL );

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_session_end[%d] starts\n", dbh); );

  /* close all open cursors  on this database connection */
  _close_all_db_cursors( dbp );

  while (OCI_STILL_EXECUTING ==
         (dbp->status = OCISessionEnd( dbp->svchp,
                                       dbp->errhp,
                                       dbp->authp,
                                       0))) {
    TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_session_end: Still executing OCISessionEnd\n"););
    SQLO_USLEEP;
  }

  if (OCI_SUCCESS != dbp->status) {
    dbp->session_created = FALSE; /* mark it as finished */
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_session_end", "OCISessionEnd");
  }

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_session_end[%d] finished with success\n", dbh); );

  dbp->session_created = FALSE;

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *        sqlo_connect
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_connect, (dbhp, cstr),
      sqlo_db_handle_t *   dbhp    AND
      const char *         cstr )
{
  int status;
  enum {MAX_BUFLEN = 1023};
  char uid[MAX_BUFLEN + 1];
  char pwd[MAX_BUFLEN + 1];
  char tnsname[MAX_BUFLEN + 1];

  if (!dbhp)
    return(SQLO_ERROR);

  TRACE(2, fprintf(_get_trace_fp(NULL), "sqlo_connect starts\n"););

  if (SQLO_SUCCESS != sqlo_split_cstring(cstr, uid, pwd, tnsname, MAX_BUFLEN))
    return SQLO_ERROR;

  TRACE(3, fprintf(_get_trace_fp(NULL), "sqlo_connect: uid=%s, pwd=%s, tnsname=%s\n",
                   uid, pwd, tnsname););

  if (SQLO_SUCCESS != (status = sqlo_server_attach(dbhp, tnsname))) {
    /* save the error message, because the db will be released */
    strcpy(_errmsg, sqlo_geterror(*dbhp));

    /* detach and free the resources */
    sqlo_server_free(*dbhp);
    return status;
  }

  if (SQLO_SUCCESS != (status = sqlo_session_begin(*dbhp, uid, pwd))) {
    /* save the error message, because the db will be released */
    strcpy(_errmsg, sqlo_geterror(*dbhp));

    /* detach and free the resources */
    sqlo_server_free(*dbhp);
    return status;
  }

  TRACE(2, fprintf(_get_trace_fp(_dbv[*dbhp]), "sqlo_connect[%d] finished with %d\n", *dbhp, status); );
  return (status);
}



/*---------------------------------------------------------------------------
 *        sqlo_finish
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_finish, (dbh), sqlo_db_handle_t dbh)
{
  int status;
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_finish", SQLO_INVALID_DB_HANDLE);

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_finish[%d] starts\n", dbh); );

  if (dbp->session_created) {

    if ( (status = sqlo_session_end(dbh)) ||
         (status = sqlo_server_detach(dbh)) )
    {
      TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_finish[%d] failed\n", dbh););
      /* Save the error message in the global variable, otherwise
       * sqlo_geterror(dbh) cannot return one when sqlo_server_free
       * cleaned up
       */
      strcpy(_errmsg, sqlo_geterror(dbh));

      /* cleanup all resources here */
      sqlo_server_free(dbh);

      return status;
    }

  } /* end if session created */



/*
  if( _dbv )
  {
    register unsigned int dbv_idx;

    for (dbv_idx = 0; dbv_idx < _dbv_size; ++dbv_idx)
    {
      if (_dbv[ dbv_idx ])  //&& ( _dbv[ dbv_idx ]->used))
      {
        if (_dbv[ dbv_idx ]->stmtv)
        {
          XFREE( _dbv[ dbv_idx ]->stmtv, __LINE__ );
        }
        _db_release( _dbv[ dbv_idx ] );
        XFREE( _dbv[ dbv_idx ], __LINE__ );
        _dbv[ dbv_idx ] = NULL;
      }
    }
    XFREE(_dbv, __LINE__);
  }
*/
  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_finish[%d] finished with success\n", dbh); );

  return SQLO_SUCCESS;
}


/*---------------------------------------------------------------------------
 *        sqlo_freeall
 *-------------------------------------------------------------------------*/
void
DEFUN(sqlo_freeall, (), )
{
  if( _dbv )
  {
    register unsigned int dbv_idx;

    for (dbv_idx = 0; dbv_idx < _dbv_size; ++dbv_idx)
    {
      if (_dbv[ dbv_idx ])  //&& ( _dbv[ dbv_idx ]->used))
      {
        if (_dbv[ dbv_idx ]->stmtv)
        {
          XFREE( _dbv[ dbv_idx ]->stmtv, __LINE__ );
        }
        _db_release( _dbv[ dbv_idx ] );
        XFREE( _dbv[ dbv_idx ], __LINE__ );
        _dbv[ dbv_idx ] = NULL;
      }
    }
    XFREE(_dbv, __LINE__);
    _dbv  = NULL;
    _sqlo_init=0;
  }
}

/*---------------------------------------------------------------------------
 *  sqlo_getdatabase
 *-------------------------------------------------------------------------*/
const char *
DEFUN(sqlo_getdatabase, (dbh) , sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_getdatabase", "Invalid db handle");

  if (!dbp)
    return NULL;

  if (!dbp->tnsname) {
      sprintf(dbp->errmsg, "No tnsname in db structure\n");
      TRACE(2, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)); );
  }

  return dbp->tnsname;
}



/*---------------------------------------------------------------------------
 *  sqlo_commit
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_commit, (dbh), sqlo_db_handle_t dbh )
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_commit", SQLO_INVALID_DB_HANDLE);
  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_commit[%d]:\n", dbh); );

  dbp->status = OCITransCommit(dbp->svchp, dbp->errhp, OCI_DEFAULT);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sql_ocommit", "");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *  sqlo_rollback
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_rollback, (dbh), sqlo_db_handle_t dbh )
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_rollback", SQLO_INVALID_DB_HANDLE);
  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_rollback[%d]:\n", dbh); );

  dbp->status = OCITransRollback(dbp->svchp, dbp->errhp, OCI_DEFAULT);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sql_orollback", "");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *    sqlo_isopen
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_isopen, (sth), sqlo_stmt_handle_t sth)
{
  register sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_isopen", SQLO_INVALID_STMT_HANDLE);

  return ( _is_opened(stp) ? SQLO_SUCCESS : 1 );
}



/*---------------------------------------------------------------------------
 *         sqlo_prepare
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_prepare, (dbh, stmt),
      sqlo_db_handle_t   dbh    AND
      const char *       stmt )
{
  sqlo_db_struct_ptr_t dbp;
  sqlo_stmt_struct_ptr_t stp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_prepare", SQLO_INVALID_DB_HANDLE);
  assert( dbp != NULL );

  _stmt_new(dbp, stmt, &stp);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_prepare", "_stmt_new");

  TRACE(2, fprintf( _get_trace_fp(dbp), "Prepare: %s\n", stmt););

  /* prepare the statement and determine the statement type */
  _prepare(stp, stmt, &(stp->stype));

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_prepare", "_prepare");

  _bindpv_reset(stp);

  if ( _is_query(stp) ) {

    /* Set the prefetch count to _num_prefetch_rows */
    _set_prefetch_rows(stp, _num_prefetch_rows);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_prepare", "_set_prefetch_rows");
  }

  return ((int) ENCODE_STH(stp->sth, dbh) );

}



/*---------------------------------------------------------------------------
 *         sqlo_bind_by_name
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_bind_by_name,
       (sth, param_name, param_type, param_addr, param_size,
        ind_addr, is_array),
      sqlo_stmt_handle_t          sth          AND
      const char *                param_name   AND
      int                         param_type   AND
      const void *                param_addr   AND
      unsigned int                param_size   AND
      short *                     ind_addr     AND
      int                         is_array )
{
  register sqlo_stmt_struct_ptr_t  stp;
  register OCIBind ** bindp_addr;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_bind_by_name", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL);
  assert( stp->dbp->errhp != NULL );
  dbp = stp->dbp;

  TRACE(3,
         fprintf(_get_trace_fp(dbp), "sqlo_bind_by_name [%2d]: "
                  "name: %s type: %d (%s), size: %u\n",
                  sth, param_name, param_type, _get_data_type_str(param_type),
                  param_size););


  if ( _is_prepared(stp) ) {

    /* check for a refcursor */
    if (SQLOT_RSET == param_type) {
      dbp->status = sqlo_bind_ref_cursor(sth, param_name, (int *)param_addr);
      CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                              "sqlo_bind_by_name", "sqlo_bind_refcursor");

    } else {

      if (stp->num_bindpv >= stp->bindpv_size) {
        _alloc_bindp(stp, stp->num_bindpv + 1);

        CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                                "sqlo_bind_by_name. alloc error for",
                                param_name);
      }

      bindp_addr = &stp->bindpv[ stp->num_bindpv ];

      dbp->status = OCIBindByName( stp->stmthp,
                                   bindp_addr,
                                   dbp->errhp,
                                   (text *) param_name,
                                   (sb4) strlen(param_name),
                                   (dvoid *) param_addr,
                                   (sb4) param_size,
                                   (ub2) param_type,
                                   (dvoid *) ind_addr,
                                   (ub2 *) 0,
                                   (ub2) 0,
                                   (ub4) 0,
                                   (ub4 *) 0,
                                   OCI_DEFAULT);
      CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                               "sqlo_bind_by_name. Cannot bind", param_name);

      /* In case of arrays, we setup the skip parameters. */
      if (is_array) {
        dbp->status = OCIBindArrayOfStruct( *bindp_addr,
                                            dbp->errhp,
                                            param_size,
                                            ind_addr ? sizeof(short) : 0,
                                            0,
                                            0);
        CHECK_OCI_STATUS_RETURN( dbp, dbp->status,
                                 "sqlo_bind_by_name. BindArrayOfStruct", param_name);
      }
      stp->num_bindpv++;
    }
  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                            "sqlo_bind_by_name", param_name);
  }

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *         sqlo_bind_ref_cursor
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_bind_ref_cursor, (sth, cursor_name, sth2p),
      sqlo_stmt_handle_t     sth          AND
      const char *           cursor_name  AND
      sqlo_stmt_handle_t *   sth2p )
{
  register sqlo_stmt_struct_ptr_t  stp;
  sqlo_stmt_struct_ptr_t st2p;
  register OCIBind ** bindp_addr;
  register int status = SQLO_SUCCESS;

  CHECK_STHANDLE(stp, sth, "sqlo_bind_ref_cursor", SQLO_INVALID_STMT_HANDLE);

  TRACE(3,
        fprintf(_get_trace_fp(stp->dbp), "sqlo_bind_ref_cursor [%2d]: name: %s\n", sth, cursor_name);)

  if ( _is_prepared(stp) ) {

    if (stp->num_bindpv >= stp->bindpv_size) {
      status = _alloc_bindp(stp, stp->num_bindpv+1);

      CHECK_OCI_STATUS_RETURN(stp->dbp, status, "sqlo_bind_ref_cursor. alloc error for",
                       cursor_name);
    }
    bindp_addr = &stp->bindpv[ stp->num_bindpv ];

    status = _stmt_new(stp->dbp, "", &st2p);
    CHECK_OCI_STATUS_RETURN(stp->dbp, status, "sqlo_bind_ref_cursor", "_stmt_new");

    *sth2p = (int) ENCODE_STH(st2p->sth, st2p->dbp->dbh);


    status = OCIBindByName(stp->stmthp,
                           bindp_addr,
                           stp->dbp->errhp,
                           (text *) cursor_name,
                           strlen(cursor_name),
                           (dvoid *) &st2p->stmthp,
                           (sb4) 0,
                           (ub2) SQLT_RSET,
                           (dvoid *) 0,
                           (ub2 *) 0,
                           (ub2 *) 0,
                           (ub4) 0,
                           (ub4 *) 0,
                           OCI_DEFAULT);

    CHECK_OCI_STATUS_RETURN(stp->dbp, status, "sqlo_bind_ref_cursor. Cannot bind", cursor_name);
    stp->num_bindpv++;

    st2p->cursor_type = REFCURSOR;
    st2p->prepared = TRUE;

  } else {
    CHECK_OCI_STATUS_RETURN(stp->dbp, SQLO_STMT_NOT_PARSED, "sqlo_bind_ref_cursor", cursor_name);
  }

  return (status == OCI_SUCCESS ? SQLO_SUCCESS : status);
}



/*---------------------------------------------------------------------------
 *         sqlo_define_ntable
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_define_ntable, (sth, pos, sth2p),
      sqlo_stmt_handle_t      sth     AND
      unsigned int            pos     AND
      sqlo_stmt_handle_t *    sth2p )
{
  register sqlo_stmt_struct_ptr_t  stp;
  sqlo_stmt_struct_ptr_t st2p;
  sqlo_db_struct_ptr_t dbp;


  CHECK_STHANDLE(stp, sth, "sqlo_define_ntable", SQLO_INVALID_STMT_HANDLE);
  assert( stp != NULL );
  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_define_ntable [%2d]: pos: %u, num_defnpv=%d\n",
                   sth, pos, stp->num_defnpv););

  if ( _is_prepared( stp ) ) {

    _alloc_definep(stp, pos);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_define_ntable. alloc error", "");

    _stmt_new( dbp, "", &st2p);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_define_ntable", "_stmt_new");

    *sth2p = (int) ENCODE_STH(st2p->sth, dbp->dbh);

    dbp->status = OCIDefineByPos( stp->stmthp,
                                  &stp->defnpv[ pos - 1 ],
                                  dbp->errhp,
                                  (ub4) pos,
                                  (dvoid *) &st2p->stmthp,
                                  (sword) 0,
                                  (ub2)SQLT_RSET,
                                  (dvoid *) 0,
                                  (ub2 *) 0,
                                  (ub2 *) 0,
                                  OCI_DEFAULT);

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                            "sqlo_define_ntable: Cannot define", "");
    stp->num_defnpv++;
    assert(stp->defnpv_size >= stp->num_defnpv);

    st2p->cursor_type = NTABLE;
    st2p->prepared = TRUE;

  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_define_ntable", "");
  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *         sqlo_bind_by_pos
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_bind_by_pos,
      (sth, param_pos, param_type, param_addr, param_size, ind_addr, is_array),
      sqlo_stmt_handle_t    sth         AND
      int                   param_pos   AND
      int                   param_type  AND
      const void *          param_addr  AND
      unsigned int          param_size  AND
      short *               ind_addr    AND
      int                   is_array )
{
  register sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_bind_by_pos", SQLO_INVALID_STMT_HANDLE);

  TRACE(3,
      fprintf(_get_trace_fp(stp->dbp),
              "sqlo_bind_by_pos [%2d]: pos: %d type: %d (%s), size: %u\n",
               sth, param_pos, param_type, _get_data_type_str(param_type), param_size););

  return(_bind_by_pos(stp, (unsigned int)param_pos, param_type, param_addr, param_size,
                       ind_addr, is_array));

}



/*---------------------------------------------------------------------------
 *         sqlo_bind_by_pos2
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_bind_by_pos2,
      (sth, param_pos, param_type, param_addr, param_size, ind_addr, rcode_addr,
       skip_size),
       sqlo_stmt_handle_t         sth             AND
       int                        param_pos       AND
       int                        param_type      AND
       const void *               param_addr      AND
       unsigned int               param_size      AND
       short *                    ind_addr        AND
       unsigned short *           rcode_addr      AND
       unsigned int               skip_size
      )
{
  register sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_bind_by_pos2", SQLO_INVALID_STMT_HANDLE);

  TRACE(3,
        fprintf(_get_trace_fp(stp->dbp), "sqlo_bind_by_pos2 [%2d]: "
                "pos: %d type: %d (%s), size: %u, skip_size: %u\n",
                sth, param_pos, param_type, _get_data_type_str(param_type),
                param_size, skip_size););

  return(_bind_by_pos2(stp, (unsigned int)param_pos, param_type, param_addr, param_size,
                       ind_addr, rcode_addr, skip_size));

}



/*---------------------------------------------------------------------------
 *         sqlo_define_by_pos
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_define_by_pos,
       (sth, value_pos, value_type, value_addr, value_size, ind_addr, rlen_addr,
        is_array),
      sqlo_stmt_handle_t    sth              AND
      int                   value_pos        AND
      int                   value_type       AND
      const void *          value_addr       AND
      unsigned int          value_size       AND
      short *               ind_addr         AND
      unsigned int *        rlen_addr        AND
      int                   is_array
      )
{
  register sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_define_by_pos", SQLO_INVALID_STMT_HANDLE);

  TRACE(3,
      fprintf(_get_trace_fp(stp->dbp),
              "sqlo_define_by_pos [%2d]: pos: %d type: %d (%s), "
              "size: %u, is_array: %d\n",
              sth, value_pos, value_type, _get_data_type_str(value_type),
              value_size, is_array););

  /* check for a nested table */
  if (SQLOT_RSET == value_type ) {
    return ( sqlo_define_ntable( sth,
                                 (unsigned int) value_pos,
                                 (int *) value_addr
                                 )
             );
  } else {
    return ( _define_by_pos( stp,
                            (unsigned int) value_pos,
                            value_type,
                            value_addr,
                            value_size,
                            ind_addr,
                            (ub4*)rlen_addr,
                            (ub2*) 0,
                            is_array
                             )
             );
  }
}



/*---------------------------------------------------------------------------
 *         sqlo_define_by_pos2
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_define_by_pos2,
       (sth, value_pos, value_type, value_addr, value_size, ind_addr,
        rlen_addr, rcode_addr, skip_size),
       sqlo_stmt_handle_t        sth               AND
       int                       value_pos         AND
       int                       value_type        AND
       const void *              value_addr        AND
       unsigned int              value_size        AND
       short *                   ind_addr          AND
       unsigned int *            rlen_addr         AND
       unsigned short *          rcode_addr        AND
       unsigned int              skip_size
       )
{
  register sqlo_stmt_struct_ptr_t  stp;

  CHECK_STHANDLE(stp, sth, "sqlo_define_by_pos2", SQLO_INVALID_STMT_HANDLE);
  assert( stp->dbp != NULL );

  TRACE(3,
         fprintf(_get_trace_fp(stp->dbp), "sqlo_define_by_pos [%2d]: pos: %d type: %d (%s), "
                 "size: %u, skip_size: %u\n",
                 sth, value_pos, value_type, _get_data_type_str(value_type),
                 value_size, skip_size););

  return ( _define_by_pos2( stp,
                            (unsigned int) value_pos,
                            value_type,
                            value_addr,
                            value_size,
                            ind_addr,
                            (ub4*)rlen_addr,
                            (ub2*) rcode_addr,
                            skip_size
                            )
           );

}



/*---------------------------------------------------------------------------
 * sqlo_execute
 *--------------------------------------------------------------------------*/
int
DEFUN(sqlo_execute, (sth, iterations),
      sqlo_stmt_handle_t     sth          AND
      unsigned int           iterations )
{
  register sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_execute", SQLO_INVALID_STMT_HANDLE);
  assert( stp->dbp != NULL);
  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_execute [%2d]: iter=%u, stmt=%.80s\n",
                   sth, iterations, _get_stmt_string(stp)););

  /* For REF CURSORS and NESTED TABLES, we determine the statement type and
   * define the output here
   */
  if (DEFAULT != stp->cursor_type && 0 == stp->num_executions ) {
    /* REF CURSOR or NESTED TABLE */
    dbp->status = OCIAttrGet( (dvoid*) stp->stmthp,
                              (ub4) OCI_HTYPE_STMT,
                              (dvoid*) &(stp->stype),
                              (ub4 *) 0,
                              (ub4) OCI_ATTR_STMT_TYPE,
                              (OCIError *) dbp->errhp
                              );

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "GetStmtType");
    dbp->status = _define_output(stp);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "_define_output");

  } else if ( _is_prepared(stp) ) {

    dbp->status = OCIStmtExecute( dbp->svchp,
                                  stp->stmthp,
                                  dbp->errhp,
                                  (ub4) iterations,
                                  (ub4) 0,
                                  (OCISnapshot *) 0,
                                  (OCISnapshot *) 0,
                                  dbp->exec_flags
                                  );

    if (OCI_SUCCESS != dbp->status && OCI_NO_DATA != dbp->status) {
      if (OCI_STILL_EXECUTING == dbp->status) {
        stp->still_executing = TRUE;
        stp->opened          = TRUE;
      } else {

        _save_oci_status(dbp, "sqlo_execute",
                         _get_stmt_string(stp), __LINE__);
      }
    } else {
      stp->opened          = TRUE;
      stp->still_executing = FALSE;

      _bindpv_reset(stp);       /* reset the number of elements in the bindpv.
                                 * The next sqlo_bind_by_name will not have to
                                 * to allocate again memory
                                 */
    } /* end if OCI_SUCCESS != status  */
  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "");
  }

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_execute returns %d\n", dbp->status););

  return (dbp->status);
}

int
DEFUN(sqlo_executeselect, (sth, iterations), 
      sqlo_stmt_handle_t     sth          AND
      unsigned int           iterations )
{
  register sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;
//   int Ret;

  CHECK_STHANDLE(stp, sth, "sqlo_execute", SQLO_INVALID_STMT_HANDLE);
  assert( stp->dbp != NULL);
  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_execute [%2d]: iter=%u, stmt=%.80s\n",
                   sth, iterations, _get_stmt_string(stp)););

  /* For REF CURSORS and NESTED TABLES, we determine the statement type and
   * define the output here
   */
  if (DEFAULT != stp->cursor_type && 0 == stp->num_executions ) {
    /* REF CURSOR or NESTED TABLE */
    dbp->status = OCIAttrGet( (dvoid*) stp->stmthp,
                              (ub4) OCI_HTYPE_STMT,
                              (dvoid*) &(stp->stype),
                              (ub4 *) 0,
                              (ub4) OCI_ATTR_STMT_TYPE,
                              (OCIError *) dbp->errhp
                              );

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "GetStmtType");
    dbp->status = _define_output(stp);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "_define_output");

  } else if ( _is_prepared(stp) ) {
    
//     dbp->status = OCIStmtExecute( dbp->svchp,
//                                   stp->stmthp,
//                                   dbp->errhp,
//                                   (ub4) iterations,
//                                   (ub4) 0,
//                                   (OCISnapshot *) 0,
//                                   (OCISnapshot *) 0,
//                                   dbp->exec_flags
//                                   );
    dbp->status = _define_output(stp); 
//     CHECK_OCI_STATUS_RETURN(dbp, Ret, "sqlo_execute", "_define_output");
    if (OCI_SUCCESS != dbp->status && OCI_NO_DATA != dbp->status) {
      if (OCI_STILL_EXECUTING == dbp->status) {
        stp->still_executing = TRUE;
        stp->opened          = TRUE;
      } else {

        _save_oci_status(dbp, "sqlo_execute",
                         _get_stmt_string(stp), __LINE__);
      }
    } else {
      stp->opened          = TRUE;
      stp->still_executing = FALSE;

      _bindpv_reset(stp);       /* reset the number of elements in the bindpv.
                                 * The next sqlo_bind_by_name will not have to
                                 * to allocate again memory
                                 */
    } /* end if OCI_SUCCESS != status  */
  } else {
    dbp->status = SQLO_STMT_NOT_PARSED;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_execute", "");
  }

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_execute returns %d\n", dbp->status););

  return (dbp->status);
}


/*---------------------------------------------------------------------------
 * sqlo_ocol_names
 *-------------------------------------------------------------------------*/
const char **
DEFUN(sqlo_ocol_names, (sth, num),
      sqlo_stmt_handle_t    sth     AND
      int *                 num )
{

  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_ocol_names", NULL);
  assert( stp->dbp );
  dbp = stp->dbp;

  if ( ! _is_query(stp) || !_is_opened(stp) )  {
    sprintf(dbp->errmsg,
            "Cannot get values for a non-select/non-opened statement "
            "(sth %d) passed to sqlo_ocol_names stype=%u opened=%d\n",
            sth, (unsigned int) stp->stype, _is_opened(stp));

    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););

    if (num)
      *num = 0;
    return NULL;
  }


  if (num)
    *num = (int) stp->num_defnpv;

  _set_all_ocol_names( stp );

  TRACE(2, fprintf(_get_trace_fp( dbp ),
                   "sqlo_ocol_names: Returning %u column names\n",
                   stp->num_defnpv););

  return ((const char **)stp->ocol_namev);
}



/*---------------------------------------------------------------------------
 * sqlo_ocol_names2
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_ocol_names2, (sth, num, ocol_names),
      sqlo_stmt_handle_t    sth           AND
      int *                 num           AND
      const char ***        ocol_names )
{
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_ocol_names2", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  if ( ! _is_query(stp) || !_is_opened(stp) )  {
    sprintf(dbp->errmsg,
            "Cannot get values for a non-select/non-opened statement "
            "(sth %d) passed to sqlo_ocol_names2 stype=%u opened=%d\n",
            sth, (unsigned int) stp->stype, _is_opened(stp) );

    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););
    if (num)
      *num = 0;
    return SQLO_ERROR;
  }

  if (num)
    *num = (int) stp->num_defnpv;

  _set_all_ocol_names(stp);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_ocol_names2", "_set_all_ocol_names");

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_ocol_names: Returning %u column names\n",
                   stp->num_defnpv););

  *ocol_names = (const char **) stp->ocol_namev;

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *        sqlo_ocol_name_lens
 *-------------------------------------------------------------------------*/
const int *
DEFUN(sqlo_ocol_name_lens, (sth, num),
      sqlo_stmt_handle_t     sth    AND
      int *                  num )
{

  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_ocol_name_lens", NULL);
  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  if ( !_is_query(stp) || !_is_opened(stp) ) {
      sprintf(dbp->errmsg,
              "Cannot get values for a non-select/non-opened statement "
              "(sth %d) passed to sqlo_ocol_name_lens\n", sth);

      TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););
        return NULL;
  }

  if (num)
    *num = (int)stp->num_defnpv;

  _set_all_ocol_names(stp);

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_ocol_name_lens: Returning %u column names\n",
                   stp->num_defnpv););

  return ((const int *)stp->ocol_namev_size);
}



/*---------------------------------------------------------------------------
 *        sqlo_value_lens
 *-------------------------------------------------------------------------*/
const unsigned int *
DEFUN(sqlo_value_lens, (sth, num),
      sqlo_stmt_handle_t    sth    AND
      int *                 num )
{
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_value_lens", NULL);
  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  if ( ! _is_query(stp) || !_is_opened(stp) ) {
    sprintf(dbp->errmsg,
            "Cannot get value len for a non-select/non-opened statement "
            "(sth %d) passed to sqlo_value_lens\n", sth);
    TRACE(1, (void) fputs(dbp->errmsg, _get_trace_fp(dbp)););
    if (num)
      *num = 0;
    return NULL;
  }

  if (num)
    *num = (int)stp->num_defnpv;

  TRACE(2, fprintf(_get_trace_fp(dbp),
                   "sqlo_value_lens: Returning %u items\n",
                   stp->num_defnpv););

  return ((const unsigned int *) stp->outv_size);
}


/*---------------------------------------------------------------------------
 *        sqlo_get_oci_handle
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_get_oci_handle, (sqloh, ocihp, type),
      int                        sqloh    AND
      void *                     ocihp    AND
      sqlo_oci_handle_types_e    type )
{
  sqlo_db_struct_ptr_t   dbp;
  sqlo_stmt_struct_ptr_t stp;

  switch (type) {
    /* global handles */
  case SQLO_OCI_HTYPE_ENV:
    CHECK_DBHANDLE(dbp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_DB_HANDLE);
    *((OCIEnv **) ocihp) = dbp->envhp;
    break;

    /* Connection specific handles */
  case SQLO_OCI_HTYPE_ERROR:
    CHECK_DBHANDLE(dbp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_DB_HANDLE);
    *((OCIError **) ocihp) = dbp->errhp;
    break;

  case SQLO_OCI_HTYPE_SVCCTX:
    CHECK_DBHANDLE(dbp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_DB_HANDLE);
    * ((OCISvcCtx **) ocihp) = dbp->svchp;
    break;

  case SQLO_OCI_HTYPE_SERVER:
    CHECK_DBHANDLE(dbp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_DB_HANDLE);
    * ((OCIServer **) ocihp) = dbp->srvhp;
    break;

  case SQLO_OCI_HTYPE_SESSION:
    CHECK_DBHANDLE(dbp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_DB_HANDLE);
    *(( OCISession **) ocihp) = dbp->authp;

    break;

    /* Statement specific handles */
  case SQLO_OCI_HTYPE_STMT:
    CHECK_STHANDLE(stp, sqloh, "sqlo_get_oci_handle", SQLO_INVALID_STMT_HANDLE);
    * ((OCIStmt **) ocihp) = stp->stmthp;
    break;

  default:
    return (SQLO_INVALID_OCI_HANDLE_TYPE);
    break; /* to keep the compiler happy */
  }
  return (SQLO_SUCCESS);
}


/*---------------------------------------------------------------------------
 * sqlo_get_db_handle
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_get_db_handle, (sth), sqlo_stmt_handle_t sth)
{
  sqlo_stmt_struct_ptr_t stp;
  char errmsg[255];

  CHECK_STHANDLE(stp, sth, "sqlo_get_db_handle", SQLO_INVALID_STMT_HANDLE);

  if (!stp->used || NULL == stp->dbp) {
    sprintf(errmsg, "Invalid sth %d passed to sqlo_get_db_handle.\n", sth);

    if (stp->dbp)
      strcpy(stp->dbp->errmsg, errmsg);

    TRACE(1, (void) fputs(errmsg, _trace_fp););
    return (SQLO_INVALID_STMT_HANDLE);
  }
  return( (int) stp->dbp->dbh);
}



/*---------------------------------------------------------------------------
 *        sqlo_version - checks if the version is sufficient
 *
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_version, (version_str), const char * version_str)
{
  unsigned int major, minor, micro;
  enum {TMP_VERSION_LEN = 64};
  char tmp_version[TMP_VERSION_LEN+1];

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  strncpy(tmp_version, version_str, TMP_VERSION_LEN);
  tmp_version[TMP_VERSION_LEN] = '\0';

  if (sscanf(tmp_version, "%u.%u.%u", &major, &minor, &micro) != 3) {
    return SQLO_MALFORMED_VERSION_STR;
   }

  if ( sqlo_major_version ==  major &&
       sqlo_minor_version == minor &&
       sqlo_micro_version >= micro) {
      return SQLO_SUCCESS;
    }
  return SQLO_WRONG_VERSION;
}



/*---------------------------------------------------------------------------
 *        sqlo_set_blocking
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_set_blocking, (dbh, on),
      sqlo_db_handle_t    dbh    AND
      unsigned int        on )
{
  sqlo_db_struct_ptr_t dbp;
  unsigned int new_mode;
  unsigned int blocking=SQLO_STH_INIT;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_set_blocking", SQLO_INVALID_DB_HANDLE);


  TRACE(3, fprintf(_get_trace_fp(dbp),"sqlo_set_blocking: on: %u\n", on););

  _get_blocking_mode(dbp, &blocking);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_set_blocking:", "_get_blocking_mode[1]");

  new_mode = on > 0 ? SQLO_ON : SQLO_OFF;

  if ( blocking != new_mode)
    {
      /* toggle the mode */
      if (OCI_SUCCESS !=
          (dbp->status = OCIAttrSet( (dvoid *) dbp->srvhp,
                                     (ub4) OCI_HTYPE_SERVER,
                                     (dvoid *) 0,
                                     (ub4) 0,
                                     (ub4) OCI_ATTR_NONBLOCKING_MODE,
                                     dbp->errhp))) {
        TRACE(2, fprintf(_get_trace_fp(dbp), "Unable to toggle blocking mode"););
        CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_set_blocking", "");
        return (dbp->status);
      }

    }

  _get_blocking_mode(dbp, &blocking);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_set_blocking:", "_get_blocking_mode[2]");

  TRACE(3, fprintf( _get_trace_fp(dbp),
                    "sqlo_set_blockin_oci_init_mode: mode is %s\n",
                    ( SQLO_ON == blocking ? "blocking" : "non-blocking")););

  return (dbp->status);

}



/*---------------------------------------------------------------------------
 * sqlo_get_blocking
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_get_blocking, (dbh, blocking),
      sqlo_db_handle_t    dbh        AND
      unsigned int *      blocking )
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_get_blocking", SQLO_INVALID_DB_HANDLE);

  _get_blocking_mode(dbp, blocking);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_get_blocking", "OCIAttrGet");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 *        sqlo_break
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_break, (dbh), sqlo_db_handle_t dbh)
{
  sqlo_db_struct_ptr_t  dbp;

  unsigned int blocking=SQLO_STH_INIT;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_break", SQLO_INVALID_DB_HANDLE);

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_break\n"););

  _get_blocking_mode(dbp, &blocking);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_break", "_get_blocking_mode");

  if ( SQLO_OFF == blocking ) {
    dbp->status = OCIBreak(dbp->srvhp, dbp->errhp);
    TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_break OCIBreak returned %d\n",
                     dbp->status););

    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_break", "OCIBreak");

    dbp->status = OCIReset(dbp->srvhp, dbp->errhp);
    TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_break OCIReset returned %d\n",
                     dbp->status););


    /*      CHECK_OCI_STATUS_RETURN(dbp, status, "sqlo_break", "OCIReset");  */

    /* close all open cursors  on this database connection which are in
     * the state "still executing" */
    _close_all_executing_cursors(dbp);

    /* Did sqlo_exec open a cursor */
    if (dbp->stmthp) {
      dbp->status = OCIHandleFree(dbp->stmthp, OCI_HTYPE_STMT);
      dbp->stmthp = 0;
      CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_break", "OCIHandleFree(dbp->stmthp)");
    }

  } else {
    dbp->status = SQLO_SUCCESS;
  }

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_break  returns %d\n", dbp->status););
  return (dbp->status);

}



/*---------------------------------------------------------------------------
 * sqlo_alloc_lob_desc
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_alloc_lob_desc, (dbh, loblpp),
      sqlo_db_handle_t    dbh     AND
      sqlo_lob_desc_t *   loblpp )
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_alloc_lob_desc", SQLO_INVALID_DB_HANDLE);

  dbp->status = OCIDescriptorAlloc( (dvoid *) dbp->envhp,
                                    (dvoid **) loblpp,
                                    (ub4) OCI_DTYPE_LOB,
                                    (size_t) 0,
                                    (dvoid **) 0);

  // TraceLog(LOGFILE, "col ?, OCIDescriptorAlloc 2 %p\n", loblpp );

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_alloc_lob_desc", "OCIDescriptorAlloc");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_free_lob_desc
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_free_lob_desc, (dbh, loblpp),
      sqlo_db_handle_t   dbh       AND
      sqlo_lob_desc_t *  loblpp )
{

  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_free_lob_desc", SQLO_INVALID_DB_HANDLE);

  if (!loblpp || !*loblpp) {
    strcpy(dbp->errmsg, "Invalid lob locator passed to sqlo_free_lob_desc");
    dbp->status = SQLO_ERROR;
    return (dbp->status);
  }

  dbp->status = OCIDescriptorFree((dvoid **) *loblpp, (ub4) OCI_DTYPE_LOB);

  // TraceLog(LOGFILE, "col ?, OCIDescriptorFree 4 %p\n", loblpp );

  *loblpp = NULL;
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_free_lob_desc", "OCIDescriptorAlloc");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_lob_write_buffer
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_lob_write_buffer, (dbh, loblp, loblen, bufp, bufl, piece),
      sqlo_db_handle_t       dbh     AND
      sqlo_lob_desc_t        loblp   AND
      unsigned int           loblen  AND
      const void *           bufp    AND
      unsigned int           bufl    AND
      unsigned int           piece )
{
  sqlo_db_struct_ptr_t  dbp;
  ub4 amtp = loblen;
  ub4 offset = 1;
  ub4 nbytes;
  ub1 p;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_write_buffer", SQLO_INVALID_DB_HANDLE);
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_buffer: loblen=%u, len=%u, %s piece\n", loblen,
                   bufl,
                   (piece == SQLO_ONE_PIECE) ? "ONE" :
                   (piece == SQLO_FIRST_PIECE) ? "FIRST" :
                   (piece == SQLO_NEXT_PIECE) ? "NEXT" :
                   (piece == SQLO_LAST_PIECE) ? "LAST" : "???"
                    ););

  nbytes = (loblen < bufl ? loblen : bufl);

  if (loblen <= bufl &&  SQLO_FIRST_PIECE == piece )
    p = OCI_ONE_PIECE;
  else
    p = (ub1) piece;

  dbp->status = OCILobWrite( dbp->svchp,
                             dbp->errhp,
                             (OCILobLocator *)loblp,
                             &amtp,
                             offset,
                             (dvoid *) bufp,
                             (ub4) nbytes,
                             p,
                             (dvoid *) 0,
                             (OCICallbackLobWrite) NULL,
                             (ub2) 0,
                             (ub1) SQLCS_IMPLICIT);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_write_buffer", "OCILobWrite");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_lob_append_buffer
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_lob_append_buffer, (dbh, loblp, loblen, bufp, bufl, piece),
      sqlo_db_handle_t    dbh      AND
      sqlo_lob_desc_t     loblp    AND
      unsigned int        loblen   AND
      void *              bufp     AND
      unsigned int        bufl     AND
      unsigned int        piece
      )
#ifdef HAVE_OCILOBWRITEAPPEND
{
  sqlo_db_struct_ptr_t  dbp;
  ub4 amtp = loblen;
  ub4 nbytes;
  ub1 p;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_append_buffer", SQLO_INVALID_DB_HANDLE);
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_append_buffer: loblen=%u, len=%u, %s piece\n", loblen,
                   bufl,
                   (piece == SQLO_ONE_PIECE) ? "ONE" :
                   (piece == SQLO_FIRST_PIECE) ? "FIRST" :
                   (piece == SQLO_NEXT_PIECE) ? "NEXT" :
                   (piece == SQLO_LAST_PIECE) ? "LAST" : "???"
                    ););

  nbytes = (loblen < bufl ? loblen : bufl);

  if (loblen <= bufl &&  SQLO_FIRST_PIECE == piece )
    p = OCI_ONE_PIECE;
  else
    p = (ub1) piece;

  dbp->status = OCILobWriteAppend( dbp->svchp,
                                   dbp->errhp,
                                   (OCILobLocator *)loblp,
                                   &amtp,
                                   (dvoid *) bufp,
                                   (ub4) nbytes,
                                   p,
                                   (dvoid *) 0,
                                   (OCICallbackLobWrite) NULL,
                                   (ub2) 0,
                                   (ub1) SQLCS_IMPLICIT);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_append_buffer", "OCILobWriteAppend");

  return (dbp->status);
}
#else
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_append_buffer", SQLO_INVALID_DB_HANDLE);
  dbp->status = SQLO_ERROR;
  /* OCILobWriteAppend is not available. We return an error in this case. */
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_lob_append_buffer",
                          "OCILobWriteAppend not available in this Oracle version");
  return (dbp->status);
}
#endif



/*---------------------------------------------------------------------------
 * sqlo_lob_write_stream
 *-------------------------------------------------------------------------*/
int                             /* O - Status SQLO_SUCCESS or SQLO_INVALID_DB_HANDLE */
DEFUN(sqlo_lob_write_stream, (dbh, loblp, filelen, fp),
      sqlo_db_handle_t       dbh        AND
      sqlo_lob_desc_t        loblp      AND
      unsigned int           filelen    AND
      FILE *                 fp )
{
  sqlo_db_struct_ptr_t  dbp;
  unsigned char buf[MAX_LONG_SIZE]; /* The buffer */
  unsigned int nbytes;          /* number of bytes to read from stream */
  unsigned int remainder = filelen; /* The number of bytes left */
  unsigned int piece;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_write_stream", SQLO_INVALID_DB_HANDLE);
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_stream: filelen=%u\n", filelen););

  if (filelen > MAX_LONG_SIZE)
    nbytes = MAX_LONG_SIZE;
  else
    nbytes = filelen;

  /* get a chunk of data */
  if (fread( buf, (size_t)nbytes, 1, fp) != 1 ) {
    strcpy(dbp->errmsg, "sqlo_lob_write_stream: I/O error. Could not get data from stream");
    dbp->status = SQLO_ERROR;
    return (dbp->status);
  }

  remainder -= nbytes;


  if (0 == remainder) {           /* excatly one piece in the file */
    TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_stream: nbytes=%u remain=%u ONE piece\n",
                     nbytes, remainder
                     ););
    dbp->status = SQLO_SUCCESS;
    piece = SQLO_ONE_PIECE;
    do {

      if (SQLO_STILL_EXECUTING == dbp->status) {
        SQLO_USLEEP;
        ;
      }

      sqlo_lob_write_buffer(dbh, loblp, nbytes, buf, nbytes, piece);
    } while (SQLO_STILL_EXECUTING == dbp->status);

    return dbp->status;

  } else {

#ifdef HAVE_OCILOBOPEN
    /* wrap this by an OCILobOpen()/OCILobClose() pair to make sure
     * the indices are uptdated only once
     */
    dbp->status = OCILobOpen(dbp->svchp, dbp->errhp, loblp, OCI_LOB_READWRITE);

    if (0 > dbp->status) {
      if (_get_errcode(dbp) != ((-1) * SQLO_STILL_EXECUTING) )
        CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                                "slqo_lob_write_stream", "OCILobOpen");
    }
#endif

    TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_stream: "
                     "nbytes=%u remain=%u FIRST piece\n",
                     nbytes, remainder
                     ););
    /* more than one piece, insert first */

    piece = SQLO_FIRST_PIECE;
    /* wait until non-blocking call finished */
    do {

      if (SQLO_STILL_EXECUTING == dbp->status) {
        SQLO_USLEEP;
        ;
      }

      sqlo_lob_write_buffer(dbh, loblp, filelen, buf, nbytes, piece);

    } while (SQLO_STILL_EXECUTING == dbp->status);

    if (SQLO_NEED_DATA != dbp->status) {
      if (0 > dbp->status) {
        CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                                "sqlo_lob_write_stream",
                                "sqlo_lob_write_buffer(FIRST)");
      } else {
        sprintf(dbp->errmsg,
                "sqlo_lob_write_buffer returned %d, expected %d "
                "(SQLO_NEED_DATA)",
                dbp->status, SQLO_NEED_DATA);
        return SQLO_ERROR;
      }
    }

    /* insert remaining pieces */
    piece = SQLO_NEXT_PIECE;
    do {
      if (remainder > MAX_LONG_SIZE)
        nbytes = MAX_LONG_SIZE;
      else {
        nbytes = remainder;
        piece = SQLO_LAST_PIECE;
      }

      if (fread( (void*) buf, (size_t)nbytes, 1, fp) != 1 ) {
        strcpy(dbp->errmsg, "sqlo_lob_write_stream: I/O error. Could not get data from stream");
        TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_stream: "
                         "Error during fread(). Setting piece to SQLO_LAST_PIECE\n"););

        piece = SQLO_LAST_PIECE;
      }

      TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_write_stream: "
                       "nbytes=%u remain=%u, %s piece\n",
                       nbytes, remainder - nbytes,
                       (piece == SQLO_ONE_PIECE) ? "ONE" :
                       (piece == SQLO_FIRST_PIECE) ? "FIRST" :
                       (piece == SQLO_NEXT_PIECE) ? "NEXT" :
                       (piece == SQLO_LAST_PIECE) ? "LAST" : "???"
                       ););
      /* wait until non-blocking call finished */
      do {

        if (SQLO_STILL_EXECUTING == dbp->status) {
          SQLO_USLEEP;
          ;
        }

        sqlo_lob_write_buffer(dbh, loblp, filelen, buf, nbytes, piece);

      } while (SQLO_STILL_EXECUTING == dbp->status);

      if ( 0 > dbp->status) {
        CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_write_stream", "sqlo_lob_write_buffer(NEXT/LAST)");
      }

      remainder -= nbytes;
    } while (dbp->status == SQLO_NEED_DATA && ! feof(fp));

#ifdef HAVE_OCILOBOPEN
    while (SQLO_STILL_EXECUTING ==
           (dbp->status = OCILobClose(dbp->svchp, dbp->errhp, loblp))) {
      SQLO_USLEEP;
    }
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "slqo_lob_write_stream", "OCILobClose");
#endif
  } /* endif remainder == 0 */

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_lob_write_stream", "OCILobWrite");

  return SQLO_SUCCESS;
}



/*---------------------------------------------------------------------------
 * sqlo_lob_get_length
 *-------------------------------------------------------------------------*/
int                             /* O - status */
DEFUN(sqlo_lob_get_length, (dbh, loblp, loblenp),
      sqlo_db_handle_t   dbh        AND
      sqlo_lob_desc_t    loblp      AND
      unsigned int *     loblenp )
{
  sqlo_db_struct_ptr_t dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_get_length", SQLO_INVALID_DB_HANDLE);

  if (!loblenp) {
    strcpy(dbp->errmsg, "sqlo_lob_get_length: NULL pointer passed in loblen");
    return SQLO_ERROR;
  }

  dbp->status = OCILobGetLength( dbp->svchp,
                                 dbp->errhp,
                                 (OCILobLocator *) loblp,
                                 (ub4*) loblenp);
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_get_length", "OCILobGetLength");
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_get_length: loblen=%u\n", *loblenp););
  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_lob_read_buffer
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_lob_read_buffer, (dbh, loblp, loblen, bufp, bufl),
      sqlo_db_handle_t      dbh     AND
      sqlo_lob_desc_t       loblp   AND
      unsigned int          loblen  AND
      void *                bufp    AND
      unsigned int          bufl )
{
  sqlo_db_struct_ptr_t dbp;
  ub4 amtp = 0;
  ub4 offset = 1;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_read_buffer", SQLO_INVALID_DB_HANDLE);
  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_lob_read_buffer: loblen=%u, buflen=%u\n",
                   loblen, bufl););


  amtp = loblen;

  dbp->status = OCILobRead( dbp->svchp,
                            dbp->errhp,
                            (OCILobLocator *)loblp,
                            &amtp,
                            offset,
                            (dvoid *) bufp,
                            (ub4) (loblen < bufl ? loblen : bufl),
                            (dvoid *) 0,
                            (OCICallbackLobRead) NULL,
                            (ub2) 0,
                            (ub1) SQLCS_IMPLICIT);

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_lob_read_buffer: amtp=%u\n", amtp););

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_read", "OCILobRead");

  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_lob_read_stream
 *-------------------------------------------------------------------------*/
int                             /* O - Status SQLO_SUCCESS or SQLO_INVALID_DB_HANDLE */
DEFUN(sqlo_lob_read_stream, (dbh, loblp, loblen, fp),
      sqlo_db_handle_t    dbh      AND
      sqlo_lob_desc_t     loblp    AND
      unsigned int        loblen   AND
      FILE *              fp )
{
  sqlo_db_struct_ptr_t dbp;
  unsigned char buf[MAX_LONG_SIZE]; /* The buffer */
  unsigned int nbytes;              /* number of bytes to read from stream */
  unsigned int remainder;       /* The number of bytes left */

  CHECK_DBHANDLE(dbp, dbh, "sqlo_lob_read_stream", SQLO_INVALID_DB_HANDLE);
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_read_stream: loblen=%u\n", loblen););

  if (loblen > MAX_LONG_SIZE)
    nbytes = MAX_LONG_SIZE;
  else
    nbytes = loblen;

  dbp->status = SQLO_SUCCESS;

  /* get a chunk of data out of the lob */
  do {
    if (SQLO_STILL_EXECUTING == dbp->status) {
      SQLO_USLEEP;
      ;
    }
    sqlo_lob_read_buffer(dbh, loblp, loblen, (void *)buf, nbytes);

  } while (SQLO_STILL_EXECUTING == dbp->status);

  switch (dbp->status) {

  case SQLO_SUCCESS:
    /* got all in one piece */
    TRACE(3, fprintf(_get_trace_fp(dbp),
                     "sqlo_lob_read_stream: "
                     "got all in one piece (%u bytes)\n",
                     loblen););
    (void) fwrite( (void *) buf, (size_t)loblen, 1, fp);
    break;

  case SQLO_ERROR:
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                            "sqlo_lob_read_stream",
                            "sqlo_lob_read_buffer(FIRST)");
    return dbp->status;
    break;

  case SQLO_NEED_DATA:
    remainder = loblen;

    TRACE(3, fprintf(_get_trace_fp(dbp),
                     "sqlo_lob_read_stream: got first piece (%u bytes)\n",
                     nbytes););
    /* write this buffer */
    (void) fwrite( (void *) buf, (size_t)nbytes, 1, fp);

    do {
      memset(buf, '\0', MAX_LONG_SIZE);
      /*      loblen = 0;*/

      remainder -= nbytes;

      /* get a chunk of data out of the lob */
      do {
        if (SQLO_STILL_EXECUTING == dbp->status) {
          SQLO_USLEEP;
          ;
        }

        sqlo_lob_read_buffer(dbh,
                             loblp,
                             loblen, (void *)buf,
                             nbytes);

      } while (SQLO_STILL_EXECUTING == dbp->status);

      /* write the data to the file */
      if (remainder < nbytes) {
        TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_read_stream: "
                         "got last piece (%u bytes)\n", remainder););

        (void) fwrite( (void *) buf, (size_t)remainder, 1, fp);
      } else {

        TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_lob_read_stream: "
                         "got next piece (%u bytes)\n", nbytes););
        (void) fwrite( (void *) buf, (size_t)nbytes, 1, fp);
      }

    } while (SQLO_NEED_DATA == dbp->status);
    break;

  default:
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_lob_read_stream", "");
    break;

  }
  return (dbp->status);
}



/*-------------------------------------------------------------------------
 * sqlo_set_prefetch_rows
 *-----------------------------------------------------------------------*/
int
DEFUN(sqlo_set_prefetch_rows, (sth, nrows),
      sqlo_stmt_handle_t    sth    AND
      unsigned int          nrows )
{
  sqlo_stmt_struct_ptr_t stp;
  sqlo_db_struct_ptr_t dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_set_prefetch_rows", SQLO_INVALID_STMT_HANDLE);
  assert( stp->dbp != NULL );
  dbp = stp->dbp;
  _set_prefetch_rows(stp, nrows);
  CHECK_OCI_STATUS_RETURN(stp->dbp, dbp->status, "sqlo_set_prefetch_rows", "");
  return (dbp->status);
}



/*-------------------------------------------------------------------------
 * sqlo_server_version
 *-----------------------------------------------------------------------*/
int
DEFUN(sqlo_server_version, (dbh, bufp, buflen),
      sqlo_db_handle_t    dbh    AND
      char *              bufp   AND
      unsigned int        buflen )
{
  sqlo_db_struct_ptr_t dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_server_version", SQLO_INVALID_STMT_HANDLE);

  dbp->status = OCIServerVersion( dbp->srvhp,
                                  dbp->errhp,
                                  (text *) bufp,
                                  (ub4) buflen,
                                  OCI_HTYPE_SERVER
                                  );
  CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_server_version", "");
  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_server_version returns: %s\n", bufp););
  return (dbp->status);
}



/*-------------------------------------------------------------------------
 * sqlo_get_ocol_dtype
 *-----------------------------------------------------------------------*/
int
DEFUN(sqlo_get_ocol_dtype, (sth, pos),
      sqlo_stmt_handle_t    sth     AND
      unsigned int          pos )
{
  sqlo_stmt_struct_ptr_t stp;
  OCIParam * paramd;
  ub2 dtype;
  ub4 num_cols;
  sqlo_db_struct_ptr_t dbp;

  assert( pos > 0 );

  CHECK_STHANDLE(stp, sth, "sqlo_get_ocol_dtype", SQLO_INVALID_STMT_HANDLE);
  assert( stp->dbp != NULL );
  dbp = stp->dbp;

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_get_ocol_dtype: Getting datatype of col: %u\n", pos););

  if (0 == stp->num_executions && ! (REFCURSOR == stp->cursor_type) ) {
    /* execute to describe the output */
    while (OCI_STILL_EXECUTING ==
           (dbp->status = OCIStmtExecute( dbp->svchp,
                                          stp->stmthp,
                                          dbp->errhp,
                                          (ub4) 0,
                                          (ub4) 0,
                                          (OCISnapshot *) 0,
                                          (OCISnapshot *) 0,
                                          (ub4) OCI_DEFAULT))) {
      SQLO_USLEEP;
    }
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_nget_ocol_dtype",
                            "OCIStmtExecute(DESCRIBE)");
    ++(stp->num_executions);
  }

  dbp->status = OCIAttrGet( (dvoid*)stp->stmthp,
                            (ub4) OCI_HTYPE_STMT,
                            (dvoid *)  &num_cols,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_PARAM_COUNT,
                            dbp->errhp);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_get_ocol_dtype",
                          "OCIAttrGet(NumberOfColumns)");

  if (pos > (unsigned int) num_cols)
  {
    dbp->status = SQLO_INVALID_COLPOS;
    return (dbp->status);
  }

  /* Get parameters of this statement handle */
  dbp->status = OCIParamGet(stp->stmthp,
                            OCI_HTYPE_STMT,
                            dbp->errhp,
                            (void **)((dvoid *)&paramd),
                            (ub4) pos);

  CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                          "sqlo_get_ocol_dtype", "OCIParamGet");

  /* Get datatype */
  dbp->status = OCIAttrGet( (dvoid*) paramd,
                            (ub4) OCI_DTYPE_PARAM,
                            (dvoid*) &dtype,
                            (ub4 *) 0,
                            (ub4) OCI_ATTR_DATA_TYPE,
                            (OCIError *)dbp->errhp );

  if (OCI_SUCCESS != dbp->status)
  {
    (void)OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status,
                            "sqlo_get_ocol_dtype", "OCIAttrGet(datatype)");
  }

  TRACE(3, fprintf(_get_trace_fp(dbp),
                   "sqlo_get_ocol_dtype: datatype: %d (%s)\n",
                   (int) dtype,
                   _get_data_type_str((int)dtype)););
  dbp->status = OCIDescriptorFree(paramd, OCI_DTYPE_PARAM);
  return( (int) dtype);
}



/*-------------------------------------------------------------------------
 * sqlo_register_int_handler
 *-----------------------------------------------------------------------*/
int
DEFUN(sqlo_register_int_handler, (handle, signal_handler),
      int *                   handle   AND
      sqlo_signal_handler_t   signal_handler )
{
#ifdef HAVE_OSNSUI
  int status;
  status = osnsui(handle, signal_handler, (char *) NULL);
  return status;
#else
  ( void )handle; 
  ( void )signal_handler;
  return (SQLO_ERROR);
#endif
}



/*-------------------------------------------------------------------------
 * sqlo_clear_int_handler
 *-----------------------------------------------------------------------*/
int
DEFUN(sqlo_clear_int_handler, (handle), int handle )
{
#ifdef HAVE_OSNSUI
  int status;
  status = osncui(handle);
  return status;
#else
  ( void )handle;
  return (SQLO_ERROR);
#endif
}



/*-------------------------------------------------------------------------
 * terminate and optionally strip output columns
 *-----------------------------------------------------------------------*/
static void
DEFUN(_terminate_ocols, (stp, do_strip_string),
      sqlo_stmt_struct_ptr_t stp  AND
      int do_strip_string)
{
  register sqlo_col_struct_ptr_t colp;  /* points to the metadata of the column */
  register char ** outpp;
  register unsigned int col_idx;
  register ub4 * lenp;
  register short * indp;

  colp  = stp->ocolsv;
  assert( colp != NULL );

  outpp   = stp->outv;     /* points to the vector of output column values */
  lenp    = stp->outv_size;    /* points to the vector of returned lengths */
  indp    = stp->oindv;    /* points to the vector of indicator variables */

  assert(outpp != NULL);
  assert(lenp != NULL);
  assert(indp != NULL);

  /* scan thru all output columns and terminate and optionally strip them */
  for (col_idx = 0; col_idx < stp->num_defnpv;
       ++col_idx, ++colp, ++outpp, ++lenp, ++indp )
  {
    /*
     * *indp: 0 indicates 'not null' -1 (!= 0 because its ub2))
     * indicates 'null'
     */

    if ( *indp != 0 )
    {
      /* NULL */
      if( (* outpp) && lenp )
      {
        **outpp = '\0';
        *lenp = 0;
      }
      else if( lenp )
      {
      	*lenp = 0;
      }
    }
    else
    {
      /* NOT NULL terminate the output */

      // TraceLog( LOGFILE, "Terminating col %i, *lenp %i, *outpp %p\n", col_idx, *lenp, *outpp );

      (*outpp)[ *lenp ] = '\0';

      if (do_strip_string )
      {
        if (colp->dtype != SQLT_NUM && colp->dtype != SQLT_LNG)
        {
          /* string type */
          _strip_string(*outpp, *lenp);

        }
        else
        {
          /* Strip leading blanks for numbers */
          register char *p;
          unsigned int l;
          l = *lenp + 1;
          for (p = *outpp; *p == ' '; ++p)
          {
            ;
          }

          if (p != *outpp )
          {
            memmove(*outpp, p, l);
          }
        } /* end if dtype != SQLT_NUM */
      }        /* end if do_strip_string */
    } /* end if NULL */
  } /* end for */
}



/*---------------------------------------------------------------------------
 * sqlo_query_result
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_query_result, (sth, ncols, values, value_lens, colnames, colname_lens),
      sqlo_stmt_handle_t    sth    AND
      unsigned int *        ncols  AND
      char ***              values AND
      unsigned int **       value_lens AND
      char ***              colnames AND
      unsigned int **       colname_lens )
{
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t  dbp;

  CHECK_STHANDLE(stp, sth, "sqlo_query_result", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( stp->dbp->svchp != NULL );

  dbp = stp->dbp;

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_query_result sth=%u\n",
                   stp->sth););


  if ( ! _is_query(stp) ) {
    dbp->status = SQLO_INVALID_STMT_TYPE;
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_query_result",
                            "Assertion");
  }

#ifdef HAVE_OCISTMTFETCH2
  dbp->status = OCIStmtFetch2( stp->stmthp,
                               dbp->errhp,
                               1,
                               OCI_FETCH_NEXT,
                               0,
                               OCI_DEFAULT
                               );
#else
  /* deprecated in new versions of Oracle (9i) */
  dbp->status = OCIStmtFetch( stp->stmthp, dbp->errhp, 1,
                              OCI_FETCH_NEXT, OCI_DEFAULT);
#endif

  TRACE(3, fprintf(_get_trace_fp(dbp), "sqlo_query_result[%d]: OCIStmtFetch finished with %d\n",
                   sth,
                   dbp->status););

  if ( dbp->status == OCI_SUCCESS ) {
    stp->still_executing = FALSE;

    _terminate_ocols(stp, 1);        /* terminate and strip */

    if (ncols != NULL)
      *ncols = stp->num_defnpv;

    if (values != NULL)
      *values = stp->outv;

    if (value_lens != NULL)
      *value_lens = stp->outv_size;

    if (colnames != NULL) {
      *colnames  = stp->ocol_namev;
    }

    if (colname_lens != NULL) {
      *colname_lens = stp->ocol_namev_size;
    }

  } else if (dbp->status == OCI_STILL_EXECUTING ) {
    stp->still_executing = TRUE;
    return (SQLO_STILL_EXECUTING);

  } else if (dbp->status == OCI_NO_DATA ) {
    return SQLO_NO_DATA;

  } else {
    CHECK_OCI_STATUS_RETURN(dbp, dbp->status, "sqlo_query_result", "");
  }
  return (dbp->status);
}



/*---------------------------------------------------------------------------
 * sqlo_set_autocommit
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_set_autocommit,(dbh),
      sqlo_db_handle_t       dbh AND
      int                    on)
{
  sqlo_db_struct_ptr_t  dbp;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_set_autocommit", SQLO_INVALID_DB_HANDLE);

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_set_autocommit: dbh=%d on=%d\n",
                   dbh, on););

  if ( SQLO_ON == on )
    dbp->exec_flags |= OCI_COMMIT_ON_SUCCESS;
  else
    dbp->exec_flags ^= OCI_COMMIT_ON_SUCCESS;

  return SQLO_SUCCESS;
}



/*---------------------------------------------------------------------------
 * sqlo_autocommit
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_autocommit,(dbh),
      sqlo_db_handle_t       dbh)
{
  sqlo_db_struct_ptr_t  dbp;
  int retval;

  CHECK_DBHANDLE(dbp, dbh, "sqlo_autocommit", SQLO_INVALID_DB_HANDLE);


  if ( dbp->exec_flags & OCI_COMMIT_ON_SUCCESS )
    retval = SQLO_ON;
  else
    retval = SQLO_OFF;

  TRACE(2, fprintf(_get_trace_fp(dbp), "sqlo_autocommit: dbh=%d returns %d\n",
                   dbh, retval););
  return retval;
}



/*---------------------------------------------------------------------------
 *        sqlo_describecol - Added by Marcelo Lombardo, april 2005
 *-------------------------------------------------------------------------*/
int
DEFUN(sqlo_describecol, (sth, col, dType, name, namelen, prec, scale, dbsize, nullok ),
      sqlo_stmt_handle_t    sth    AND
      int                   col    AND
      unsigned short  *     dType  AND
      char * *              name   AND
      int *                 namelen AND
      int *                 prec   AND
      int *                 scale  AND
      int *                 dbsize  AND
      int *                 nullok )
{
  sqlo_stmt_struct_ptr_t  stp;
  sqlo_db_struct_ptr_t  dbp;
  sqlo_col_struct_ptr_t colp;

  CHECK_STHANDLE(stp, sth, "sqlo_describecol", SQLO_INVALID_STMT_HANDLE);

  assert( stp->dbp != NULL );
  assert( stp->stmthp != NULL );
  assert( stp->dbp->errhp != NULL );
  assert( stp->dbp->svchp != NULL );

  dbp = stp->dbp;

  TRACE(2, fprintf(_get_trace_fp(stp->dbp), "sqlo_describecol [%2u] for col %i \n", stp->sth, col ););

  colp = stp->ocolsv;
  colp += col;

  * dType = (unsigned short) (colp->database_dtype);
  * name = (colp->col_name);
  * namelen = (int) (colp->col_name_size);
  * prec    = (int) (colp->prec);
  * scale   = (int) (colp->scale);
  * dbsize  = (int) (colp->dbsize );
  * nullok  = (int) (colp->nullok);

  TRACE(3, fprintf(_get_trace_fp(dbp), "  sqlo_describecol: %u\n", (unsigned int) col););

  return( 0 );
}
