/*
 * $Id: hbxml.h,v 1.3 2003/06/30 23:06:27 jonnymind Exp $
 */

/*
 * xHarbour Project source code:
 * HBXML - XML DOM oriented routines
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *    See also MXML library related copyright below
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
* MXML (Mini XML) Library related copyright notice.
* (referring to Harbour/xHarbour version).
*
* This source file contains a modified version of MXML (Mini XML)
* library, developed by Giancarlo Niccolai. MXML is released under
* LGPL license; this modified version (called HBXML) is released under
* GPL with HARBOUR exception. HBXML license does not extends into
* MXML; HBXML and any modification to HBXML is to be considered as
* a part of Harbour or xHarbour projects, as it is modified to
* be specifically working in the context of the compiler's RTL.
*
* Original MXML lib can be obtained requesting it at
* Giancarlo Niccolai <giancarlo@niccolai.org>
*/

#ifndef HB_XML_H
#define HB_XML_H

/* Standard definitions */
#ifdef HB_OS_MAC
   #define MXML_LINE_TERMINATOR      '\r'
   #define MXML_SOFT_LINE_TERMINATOR '\n'
#else
   /*Notice, this works for unix AND windows */
   #define MXML_LINE_TERMINATOR      '\n'
   #define MXML_SOFT_LINE_TERMINATOR '\r'
#endif

#define MXML_EOF                  -256

#define MXML_MAX_NAME_LEN     128
#define MXML_MAX_ATTRIB_LEN   256
#define MXML_ALLOC_BLOCK      128
#define MXML_MAX_DEPTH        64

/* Styles */
#define MXML_STYLE_INDENT        0x0001
#define MXML_STYLE_TAB           0x0002
#define MXML_STYLE_THREESPACES   0x0004
#define MXML_STYLE_NOESCAPE      0x0008

/* Status vaules */

typedef enum
{
   MXML_STATUS_ERROR=0,
   MXML_STATUS_OK=1,
   MXML_STATUS_MORE,
   MXML_STATUS_DONE,
   MXML_STATUS_UNDEFINED,
   MXML_STATUS_MALFORMED
} MXML_STATUS;

/* Error codes */
typedef enum
{
   MXML_ERROR_NONE = 0,
   MXML_ERROR_IO = 1,
   MXML_ERROR_NOMEM,

   MXML_ERROR_OUTCHAR,
   MXML_ERROR_INVNODE,
   MXML_ERROR_INVATT,
   MXML_ERROR_MALFATT,
   MXML_ERROR_INVCHAR,
   MXML_ERROR_NAMETOOLONG,
   MXML_ERROR_ATTRIBTOOLONG,
   MXML_ERROR_VALATTOOLONG,
   MXML_ERROR_UNCLOSED,
   MXML_ERROR_UNCLOSEDENTITY,
   MXML_ERROR_WRONGENTITY
} MXML_ERROR_CODE;

/* Node types */

typedef enum
{
   MXML_TYPE_TAG=0,
   MXML_TYPE_COMMENT,
   MXML_TYPE_PI,
   MXML_TYPE_DIRECTIVE,
   MXML_TYPE_DATA,
   MXML_TYPE_DOCUMENT   // used for document level root node
} MXML_NODE_TYPE;

/* Refil function */
struct tag_mxml_refil;
struct tag_mxml_output;

typedef void (*MXML_REFIL_FUNC)( struct tag_mxml_refil *ref );
typedef void (*MXML_OUTPUT_FUNC)( struct tag_mxml_output *out, char *data, int len );

/*************************************************
   Structures holding the XML data
**************************************************/


/* Refiller */

typedef struct tag_mxml_refil
{
   // status variables
   MXML_STATUS status;
   MXML_ERROR_CODE error;

   // buffer for reading data
   unsigned char *buffer;
   int bufsize;  // size of the whole buffer
   int buflen;   // valid characters in the current buffer
   int bufpos;   // current position

   // lenght of the stream for implementing progress indicators
   long int streampos;
   long int streamlen;

   // callback funcs
   MXML_REFIL_FUNC refil_func;

   // ungetc implementation
   int sparechar;

   // data available for callback functions
   void *data;

} MXML_REFIL;


typedef struct tag_mxml_output
{
   // status variables
   MXML_STATUS status;
   MXML_ERROR_CODE error;

   // output operation
   MXML_OUTPUT_FUNC  output_func;

   // data to implement progress indicators
   int node_count;
   int node_done;

   // data available for callback functions
   void *data;

} MXML_OUTPUT;


typedef struct tag_mxml_self_growing_string
{
   char *buffer;
   int allocated;
   int length;
} MXML_SGS;


/* Tag oriented operations */
PHB_ITEM mxml_node_new( void );
PHB_ITEM mxml_node_clone( PHB_ITEM tg );
PHB_ITEM mxml_node_clone_tree( PHB_ITEM tg );

void mxml_node_unlink( PHB_ITEM tag );

void mxml_node_insert_before( PHB_ITEM tg, PHB_ITEM node );
void mxml_node_insert_after( PHB_ITEM tg, PHB_ITEM node );
void mxml_node_insert_below( PHB_ITEM tg, PHB_ITEM node );
void mxml_node_add_below( PHB_ITEM tg, PHB_ITEM node );

MXML_STATUS mxml_node_read( MXML_REFIL *data, PHB_ITEM node, PHB_ITEM doc, int style );

MXML_STATUS mxml_node_write( MXML_OUTPUT *out, PHB_ITEM pNode, int style );


/* Attribute oriented operations */
PHB_ITEM mxml_attribute_new( void );

typedef struct _hbxml_attribute {
   PHB_ITEM pName;
   PHB_ITEM pValue;
} HBXML_ATTRIBUTE, *PHBXML_ATTRIBUTE;

MXML_STATUS mxml_attribute_read( MXML_REFIL *data, PHB_ITEM doc, PHBXML_ATTRIBUTE dest, int style );
MXML_STATUS mxml_attribute_write( MXML_OUTPUT *out, PHBXML_ATTRIBUTE attr, int style );

/* Refil routines */
MXML_REFIL *mxml_refil_new( MXML_REFIL_FUNC func, char *buf, int buflen,
   int bufsize );
MXML_STATUS mxml_refil_setup( MXML_REFIL *ref, MXML_REFIL_FUNC func,
   char *buf, int buflen, int bufsize );
void mxml_refil_destory( MXML_REFIL *ref );

int mxml_refil_getc( MXML_REFIL *ref );
#define mxml_refil_ungetc( ref, ch )  ref->sparechar = ch

void mxml_refill_from_stream_func( MXML_REFIL *ref );
void mxml_refill_from_handle_func( MXML_REFIL *ref );

/* Output routines */
MXML_OUTPUT *mxml_output_new( MXML_OUTPUT_FUNC func, int node_count);
MXML_STATUS mxml_output_setup( MXML_OUTPUT *out, MXML_OUTPUT_FUNC func, int node_count);
void mxml_output_destroy( MXML_OUTPUT *out );
MXML_STATUS mxml_output_char( MXML_OUTPUT *out, int c );
MXML_STATUS mxml_output_string_len( MXML_OUTPUT *out, char *s, int len );
MXML_STATUS mxml_output_string( MXML_OUTPUT *out, char *s );
MXML_STATUS mxml_output_string_escape( MXML_OUTPUT *out, char *s );

void mxml_output_func_to_stream( MXML_OUTPUT *out, char *s, int len );
void mxml_output_func_to_handle( MXML_OUTPUT *out, char *s, int len );
void mxml_output_func_to_sgs( MXML_OUTPUT *out, char *s, int len );


/* Self growing string routines */
MXML_SGS *mxml_sgs_new( void );
void mxml_sgs_destroy( MXML_SGS *sgs );
MXML_STATUS mxml_sgs_append_char( MXML_SGS *sgs, char c );
MXML_STATUS mxml_sgs_append_string_len( MXML_SGS *sgs, char *s, int slen );
MXML_STATUS mxml_sgs_append_string( MXML_SGS *sgs, char *s );


/* Error description */
char *mxml_error_desc( MXML_ERROR_CODE code );

/* Allocator and deletor functions are meant to be redeclared by includers */
#ifndef MXML_ALLOCATOR
   #define MXML_ALLOCATOR  hb_xgrab
#endif

#ifndef MXML_DELETOR
   #define MXML_DELETOR  hb_xfree
#endif

#ifndef MXML_REALLOCATOR
   #define MXML_REALLOCATOR hb_xrealloc
#endif

#endif
