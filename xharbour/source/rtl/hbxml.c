/*
 * $Id$
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

#include <hbxml.h>
#include <stdio.h>
#include <ctype.h>
#include <hbapi.h>

#if defined( HB_OS_UNIX ) || defined( HB_OS_LINUX )
   #include <unistd.h>
#endif

#include <hbapierr.h>
#include <hbapiitm.h>

// just a shortcut
#define MALFORMED_ERROR( elem, val ) \
   elem->status = MXML_STATUS_MALFORMED;\
   elem->error = val;

/***********************************************************
   HBXML lib
   Attribute oriented routines
************************************************************/

MXML_ATTRIBUTE *mxml_attribute_new()
{
   MXML_ATTRIBUTE *attribute = (MXML_ATTRIBUTE *)
         MXML_ALLOCATOR( sizeof( MXML_ATTRIBUTE ) );

   if ( attribute != NULL )
      mxml_attribute_setup( attribute );

   return attribute;
}

MXML_STATUS mxml_attribute_setup( MXML_ATTRIBUTE *attribute )
{
   attribute->name = NULL;
   attribute->value = NULL;
   mxml_attribute_unlink( attribute );

   return MXML_STATUS_OK;
}

/* Clones an attribute list */
MXML_ATTRIBUTE *mxml_attribute_clone( MXML_ATTRIBUTE *attrib )
{
   MXML_ATTRIBUTE *clone_head, *clone_tail;

   if( attrib == NULL )
      return NULL;

   clone_head = clone_tail = mxml_attribute_new();

   while ( attrib != NULL ) {
      if ( attrib->name != NULL ) {
         int nlen = strlen( attrib->name ) + 1;
         clone_tail->name = ( char *) MXML_ALLOCATOR( nlen );
         memcpy( clone_tail->name, attrib->name, nlen );
      }

      if ( attrib->value != NULL ) {
         int nlen = strlen( attrib->value ) + 1;
         clone_tail->value = ( char *) MXML_ALLOCATOR( nlen );
         memcpy( clone_tail->value, attrib->value, nlen );
      }


      attrib = attrib->next;

      if ( attrib != NULL ) {
         clone_tail->next = mxml_attribute_new();
         clone_tail = clone_tail->next;
      }

   }

   return clone_head;
}

void mxml_attribute_destroy( MXML_ATTRIBUTE *attrib )
{
   if ( attrib->name != NULL )
      MXML_DELETOR( attrib->name );

   if ( attrib->value != NULL )
      MXML_DELETOR( attrib->value );

   if ( attrib->next != NULL )
      mxml_attribute_destroy( attrib->next );

   MXML_DELETOR( attrib );
}


void mxml_attribute_unlink( MXML_ATTRIBUTE *attrib )
{
   attrib->next = NULL;
}


MXML_ATTRIBUTE *mxml_attribute_read( MXML_REFIL *ref, MXML_DOCUMENT *doc )
{
   MXML_ATTRIBUTE *ret;
   int chr, quotechr;
   char buf_name[ MXML_MAX_NAME_LEN + 1];
   char buf_attrib[ MXML_MAX_NAME_LEN *2 + 1];
   int iPosn = 0, iPosa = 0;
   int iStatus = 0;

   while ( iStatus < 5 && iPosn <= MXML_MAX_NAME_LEN && iPosa <= MXML_MAX_ATTRIB_LEN ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         // begin
         case 0:
            switch ( chr ) {
               case MXML_LINE_TERMINATOR: doc->iLine++; break;
               // We repeat line terminator here for portability
               case MXML_SOFT_LINE_TERMINATOR: break;
               case ' ': case '\t': break;
               // no attributes found
               case '>': case '/': return NULL;
               default:
                  if ( isalpha( chr ) ) {
                     buf_name[ iPosn++ ] = chr;
                     iStatus = 1;
                  }
                  else {
                     MALFORMED_ERROR( doc,  MXML_ERROR_INVATT );
                     return NULL;
                  }
            }
         break;

         // scanning for a name
         case 1:
            if ( isalnum( chr ) || chr == '_' || chr == '-' || chr == ':' ) {
               buf_name[ iPosn++ ] = chr;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine ++ ;
               iStatus = 2; // waiting for a '='
            }
            // We repeat line terminator here for portability
            else if ( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' ) {
               iStatus = 2;
            }
            else if ( chr == '=' ) {
               iStatus = 3;
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_MALFATT );
               return NULL;
            }
         break;

         // waiting for '='
         case 2:
            if ( chr == '=' ) {
               iStatus = 3;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine ++ ;
            }
            // We repeat line terminator here for portability
            else if ( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' ) {
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_MALFATT );
               return NULL;
            }
         break;

         // waiting for ' or "
         case 3:
            if ( chr == '\'' || chr == '"' ) {
               iStatus = 4;
               quotechr = chr;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine ++ ;
            }
            // We repeat line terminator here for portability
            else if ( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' ) {
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_MALFATT );
               return NULL;
            }
         break;

         // scanning the attribute content ( until next quotechr )
         case 4:
            if ( chr == quotechr ) {
               iStatus = 5;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine ++ ;
               buf_attrib[ iPosa++ ] = chr;
            }
            // We repeat line terminator here for portability
            else {
               buf_attrib[ iPosa++ ] = chr;
            }
         break;

      }

   }

   if ( doc->status != MXML_STATUS_OK )
      return NULL;

   if ( iStatus < 5 ) {
      doc->status = MXML_STATUS_MALFORMED;
      if ( iPosn > MXML_MAX_NAME_LEN )
         doc->error = MXML_ERROR_ATTRIBTOOLONG;
      else if ( iPosa > MXML_MAX_ATTRIB_LEN )
         doc->error = MXML_ERROR_VALATTOOLONG;
      else
         doc->error = MXML_ERROR_MALFATT;

      return NULL;
   }

   // time to create the attribute
   ret = mxml_attribute_new();
   ret->name = (char *) MXML_ALLOCATOR( iPosn + 1);
   memcpy( ret->name, buf_name, iPosn  );
   ret->name[ iPosn ] = 0;

   ret->value = (char *) MXML_ALLOCATOR( iPosa + 1 );
   memcpy( ret->value, buf_attrib, iPosa );
   ret->value[ iPosa ] = 0;

   return ret;
}

MXML_STATUS mxml_attribute_write( MXML_OUTPUT *out, MXML_ATTRIBUTE *attr )
{
   char quote = '"';
   char *p = attr->name;

   // possibly detects the need for quote = '\''
   while ( *p ) {
      if ( *p == '"') {
         quote = '\'';
         break;
      }
      p++;
   }

   mxml_output_string( out, attr->name );
   mxml_output_char( out, '=' );
   mxml_output_char( out, quote );
   mxml_output_string( out, attr->value );
   mxml_output_char( out, quote );

   return out->status;
}

/***********************************************************
   HBXML lib
   Document routines
***********************************************************/

MXML_DOCUMENT *mxml_document_new()
{
   MXML_DOCUMENT *doc =(MXML_DOCUMENT *) MXML_ALLOCATOR( sizeof( MXML_DOCUMENT ) );

   if ( doc != NULL )
      mxml_document_setup( doc );

   return doc;
}

MXML_STATUS mxml_document_setup( MXML_DOCUMENT *doc )
{
   doc->iLine = 1;
   doc->name = NULL;
   doc->root = NULL;

   // Currenty unused: doc->index = mxml_index_new();
   doc->index = NULL;
   doc->node_count = 0;
   doc->root = mxml_node_new();

   if ( doc->root != NULL ) {
      doc->root->type = MXML_TYPE_DOCUMENT;
      doc->status = MXML_STATUS_OK;
      doc->error = MXML_ERROR_NONE;
      return MXML_STATUS_OK;
   }

   doc->status = MXML_STATUS_ERROR;
   doc->error = MXML_ERROR_NOMEM;
   return MXML_STATUS_ERROR;
}

void mxml_document_destroy( MXML_DOCUMENT *doc )
{
   // currently unused
   //mxml_index_destroy( doc->index );
   MXML_DELETOR( doc );
}


MXML_STATUS mxml_document_find( MXML_ITERATOR *it, char *path )
{
   return MXML_STATUS_OK;
}

/***********************************************************
   HBXML lib
   Index routines - to be built
***********************************************************/

MXML_INDEX *mxml_index_new()
{
   MXML_INDEX *index = (MXML_INDEX *) MXML_ALLOCATOR( sizeof( MXML_INDEX ) );

   if ( index != NULL )
      mxml_index_setup( index );

   return index;
}

MXML_STATUS mxml_index_setup( MXML_INDEX *index )
{
   index->length = 0;
   index->allocated = 0;
   index->data = NULL;

   return MXML_STATUS_OK;
}

void mxml_index_destroy( MXML_INDEX *index )
{
   /* Posix free() can be used with NULL, but nothing is known for other
      free() provided by users */
   if ( index->data != NULL )
      MXML_DELETOR( index->data );

   MXML_DELETOR( index );
}

/***********************************************************
   HBXML lib
   Iterators
***********************************************************/

MXML_ITERATOR *mxml_iterator_new( MXML_DOCUMENT *doc )
{
   MXML_ITERATOR *it =(MXML_ITERATOR *) MXML_ALLOCATOR( sizeof( MXML_ITERATOR ) );

   if ( it != NULL )
      mxml_iterator_setup( it, doc );

   return it;
}

MXML_STATUS mxml_iterator_setup( MXML_ITERATOR *it, MXML_DOCUMENT *doc )
{
   it->document = doc;
   it->mode = MXML_ITERATOR_MODE_FORWARD;
   it->root_node = doc->root;

   mxml_iterator_top( it );

   return MXML_STATUS_OK;
}

void mxml_iterator_destroy( MXML_ITERATOR *it )
{
   MXML_DELETOR( it );
}

MXML_NODE *mxml_iterator_top( MXML_ITERATOR *it )
{
   it->node = it->root_node;
   it->level = 0;
   return it->node;
}

void mxml_iterator_set_top( MXML_ITERATOR *it, MXML_NODE *node )
{
   it->root_node = node;
   mxml_iterator_top( it );
}

void mxml_iterator_copy( MXML_ITERATOR *dest, MXML_ITERATOR *src )
{
   dest->document = src->document;
   dest->root_node = src->root_node;
   dest->mode = src->mode;
   dest->node = src->node;
   dest->level = src->level;
}

MXML_ITERATOR *mxml_iterator_clone( MXML_ITERATOR *src )
{
   MXML_ITERATOR *it =(MXML_ITERATOR *) MXML_ALLOCATOR( sizeof( MXML_ITERATOR ) );

   if ( it != NULL )
      mxml_iterator_copy( it, src );

   return it;
}


/**
* Finds the next node in the hyerarcy, scanning all the childrens, then
* all the brothers, and then climbing back to the parent's brothers up
* to esausting all the nodes.
*/

MXML_NODE *mxml_iterator_next( MXML_ITERATOR *it )
{
   MXML_NODE *node = NULL;
   int level = it->level;

   if ( it->node->child != NULL ) {
      node = it->node->child;
      level++;
   }
   else if ( it->node->next != NULL ) {
      node = it->node->next;
   }
   else {
      node = it->node;
      while ( node->parent != NULL ) {
         level--;
         node = node->parent;
         if ( node->next != NULL )
            break;
      }
      node = node->next; // can be NULL
   }

   // have we found a different node to go?
   if ( node != NULL ) {
      it->node = node;
      it->level = level;
      return node;
   }

   // we are done
   return NULL;
}


MXML_NODE *mxml_iterator_next_brother( MXML_ITERATOR *it )
{
   if ( it->node->next != NULL ) {
      it->node = it->node->next;
      return it->node;
   }
   return NULL;
}

MXML_NODE *mxml_iterator_previous_brother( MXML_ITERATOR *it )
{
   if ( it->node->prev != NULL ) {
      it->node = it->node->prev;
      return it->node;
   }
   return NULL;
}

MXML_NODE *mxml_iterator_descend( MXML_ITERATOR *it )
{
   if ( it->node->child != NULL ) {
      it->node = it->node->child;
      it->level++;
      return it->node;
   }
   return NULL;
}

MXML_NODE *mxml_iterator_ascend( MXML_ITERATOR *it )
{
   if ( it->node->parent != NULL ) {
      it->node = it->node->parent;
      it->level--;
      return it->node;
   }
   return NULL;
}

void mxml_iterator_insert_after( MXML_ITERATOR *it, MXML_NODE *node  )
{
   if ( it->node == NULL ) {
      mxml_iterator_add_below( it, node );
   }
   else {
      mxml_node_insert_before( it->node, node );
   }
}

void mxml_iterator_insert_before( MXML_ITERATOR *it, MXML_NODE *node )
{
   if ( it->node == NULL ) {
      mxml_iterator_add_below( it, node );
   }
   else {
      mxml_node_insert_after( it->node, node );
   }
}

/**
* Insert a new node between the iterator pointer and all its children
*/

void mxml_iterator_insert_below( MXML_ITERATOR *it, MXML_NODE *node )
{
   mxml_node_insert_below( it->node , node );
}

/**
* Add a new node (or tree) after the last child of the iterator pointer
* This is the only function capable to add a new node to an empty tree.
*/

void mxml_iterator_add_below( MXML_ITERATOR *it, MXML_NODE *node )
{
   if ( it->node == NULL )
   {
      it->document->root = it->node = node;
      return;
   }

   mxml_node_add_below( it->node, node );
}

/**
* Search for a node with the given name in the rest of the document
* Currently it does not uses indexed search.
*/

MXML_NODE *mxml_iterator_scan_node( MXML_ITERATOR *it, char *name )
{
   MXML_NODE *node = it->node;

   while( node != NULL ) {
      if ( node->name != NULL) {
         if ( strcmp( node->name, name ) == 0 )
            break;
      }
      node = mxml_iterator_next( it );
   }
   /* Notice: it is modified */
   return node;
}

/**
* Search for a node with the given attribute.
* Currently it does not uses indexed search.
*/
MXML_NODE *mxml_iterator_scan_attribute( MXML_ITERATOR *it, char *attrib )
{
   MXML_NODE *node = it->node;

   while( node != NULL ) {
      MXML_ATTRIBUTE *attr = node->attributes;
      while ( attr != NULL) {
         if ( strcmp( attr->name, attrib ) == 0 )
            return node;
         attr = attr->next;
      }
      node = mxml_iterator_next( it );
   }
   /* Notice: it is modified */
   return node;
}



/**
* Search for a node with the given attribute and the given value.
* Currently it does not uses indexed search.
*/
MXML_NODE *mxml_iterator_scan_attribute_value( MXML_ITERATOR *it, char *attrib, char *val )
{
   MXML_NODE *node = it->node;

   while( node != NULL ) {
      MXML_ATTRIBUTE *attr = node->attributes;
      while ( attr != NULL) {
         if ( strcmp( attr->name, attrib ) == 0 &&
            attr->value != NULL &&
            strcmp( attr->value, val ) == 0 )
                  return node;
         attr = attr->next;
      }

      node = mxml_iterator_next( it );
   }
   /* Notice: it is modified */
   return node;
}


/***********************************************************
   HBXML lib
   Item (node) routines
***********************************************************/


MXML_NODE *mxml_node_new()
{
   MXML_NODE *node = (MXML_NODE *) MXML_ALLOCATOR( sizeof( MXML_NODE ) );

   if ( node != NULL )
      mxml_node_setup( node );

   return node;
}

MXML_STATUS mxml_node_setup( MXML_NODE *node )
{
   node->name = NULL;
   node->data = NULL;
   node->data_length = 0;
   node->attributes = NULL;
   node->type = MXML_TYPE_TAG;

   node->parent = NULL;
   node->next = NULL;
   node->prev = NULL;
   node->child = NULL;

   return MXML_STATUS_OK;
}

/**
* The unlink function is used to detach a node from the UPPER and PARENT hyerarcy.
* The node is "removed" so that siblings node are "squished", and possible parents
* are informed of the changes, so that they are able to get a new child to start
* the tree structure under them. The childs of the unlinked nodes are NOT unlinked,
* thus remains attached to the node: is like removing a branch with all its leaves.
*
*/

void mxml_node_unlink( MXML_NODE *node )
{
   if ( node->prev != NULL )
      node->prev->next = node->next;

   if ( node->next != NULL )
      node->next->prev = node->prev;

   if ( node->parent != NULL && node->parent->child == node )
      node->parent->child = node->next;

   node->parent = NULL;
   node->next = NULL;
   node->prev = NULL;
}
/****************************************************************/

void mxml_node_insert_before( MXML_NODE *tg, MXML_NODE *node )
{
   node->prev = tg;
   node->next = tg->next;
   node->parent = tg->parent;
   /* puts the node on top of hyerarcy if needed */
   if ( tg->parent != NULL && tg->parent->child == tg )
      tg->parent->child = node;

   tg->next = node;
}

void mxml_node_insert_after( MXML_NODE *tg, MXML_NODE *node )
{
   node->prev = tg->prev;
   node->next = tg;
   node->parent = tg->parent;
   tg->prev = node;
}

/**
* Creates a new tree level, so that the given node is added between
* tg and its former children.
*/

void mxml_node_insert_below( MXML_NODE *tg, MXML_NODE *node )
{
   MXML_NODE *child;

   node->parent = tg;
   child = node->child = tg->child;

   while ( child != NULL ) {
      child->parent = node;
      child = child->next;
   }

   tg->child = node;
}

/**
* Adds a node to the bottom of the children list of tg.
*/

void mxml_node_add_below( MXML_NODE *tg, MXML_NODE *node )
{
   MXML_NODE *child;

   node->parent = tg;
   child = tg->child;

   if ( child != NULL ) {
      while ( child->next != NULL ) {
         child = child->next;
      }
      child->next = node;
      node->prev = child;
   }
   else
      tg->child = node;
}

/**
* Clones a node, but it does not sets the parent, nor the siblings;
* this clone is "floating" out of the tree hierarcy.
*/

MXML_NODE *mxml_node_clone( MXML_NODE *tg )
{
   MXML_NODE *clone = mxml_node_new();

   clone->type = tg->type;

   if ( tg->name != NULL ) {
      int nlen = strlen( tg->name ) + 1;
      clone->name = ( char *) MXML_ALLOCATOR( nlen );
      memcpy( clone->name, tg->name, nlen );
   }

   if ( tg->data != NULL ) {
      int nlen = tg->data_length > 0 ? tg->data_length +1 : strlen( tg->data ) + 1;
      clone->data = ( char *) MXML_ALLOCATOR( nlen );
      memcpy( clone->data, tg->data, nlen );
      clone->data_length = tg->data_length;
   }

   clone->attributes = mxml_attribute_clone( tg->attributes );

   return clone;
}


/**
* Clones a node and all its subtree, but it does not sets the parent, nor the siblings;
* this clone is "floating" out of the tree hierarcy.
*/

MXML_NODE *mxml_node_clone_tree( MXML_NODE *tg )
{
   MXML_NODE *clone = mxml_node_clone( tg );
   MXML_NODE *node = tg->child;

   if ( node != NULL ) {
      MXML_NODE *clone_child;

      clone->child = mxml_node_clone_tree( node );
      clone_child = clone->child;
      clone_child->parent = clone;
      node = node->next;

      while ( node != NULL ) {
         clone_child->next = mxml_node_clone_tree( node );
         clone_child->next->parent = clone_child->parent;
         clone_child = clone_child->next;
         node = node->next;
      }
   }

   return clone;
}

/**
* Destroy a node, including all its children and brothers in the next branch.
* To destroy a subtree, unlink the root node of the subtree and then destroy it.
* To destroy a single node, unlink it and then move its children elsewhere;
* then destroy the node.
*/

void mxml_node_destroy( MXML_NODE *node )
{
   if ( node->name != NULL )
      MXML_DELETOR( node->name );

   if ( node->data != NULL )
      MXML_DELETOR( node->data );

   if ( node->attributes != NULL )
      mxml_attribute_destroy( node->attributes );

   if ( node->next != NULL )
      mxml_node_destroy( node->next );

   if ( node->child != NULL )
      mxml_node_destroy( node->child );

   MXML_DELETOR( node );
}

/**
* Scans a node for given attribute
*/
char *mxml_node_value_of( MXML_NODE *pNode, char *attrib )
{
   MXML_ATTRIBUTE *attr = pNode->attributes;
   char *ret = NULL;

   while( attr != NULL ) {
      if ( strcmp( attrib, attr->name) == 0 ) {
         ret = attr->value;
         break;
      }
      attr = attr->next;
   }

   return ret;
}

/**
* Scans a node for given attribute, returning a default value if
* the attribute can't be found.
*/
char *mxml_node_value_of_default( MXML_NODE *pNode, char *attrib, char *val_default )
{
   MXML_ATTRIBUTE *attr = pNode->attributes;
   char *ret = val_default;

   while( attr != NULL ) {
      if ( strcmp( attrib, attr->name) == 0 ) {
         ret = attr->value;
         break;
      }
      attr = attr->next;
   }

   return ret;
}

/* reads a data node */
static void mxml_node_read_data( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   char *buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int iAllocated = MXML_ALLOC_BLOCK;
   int iPos = 0;
   int chr;

   chr = mxml_refil_getc( ref );
   while ( chr != MXML_EOF ) {
      if ( chr != '<' ) {
         buf[ iPos++ ] = chr;
         if ( iPos >= iAllocated ) {
            iAllocated += MXML_ALLOC_BLOCK;
            buf = (char *) MXML_REALLOCATOR( buf, iAllocated );
         }

         if ( chr == MXML_LINE_TERMINATOR )
            doc->iLine++;

      }
      else {
         mxml_refil_ungetc( ref, chr );
         break;
      }

      chr = mxml_refil_getc( ref );
   }

   if ( ref->status != MXML_STATUS_OK) {
      doc->status = ref->status;
      doc->error = ref->error;
      return;
   }

   // trimming unneded spaces
   while ( (iPos >1 && buf[iPos-1] == ' ') || buf[iPos-1] == '\t' )
      iPos--;
   buf[ iPos ] = 0;
   pNode->type = MXML_TYPE_DATA;
   pNode->data = buf;
   pNode->data_length = iPos;
}

static MXML_STATUS mxml_node_read_name( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   char buf[ MXML_MAX_NAME_LEN + 1];
   int iPos = 0;
   int chr;
   int iStatus = 0;

   chr = 1;
   while ( iStatus < 2 && iPos < MXML_MAX_NAME_LEN ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         case 0:
            if ( isalpha( chr ) ) {
               buf[ iPos++ ] = chr;
               iStatus = 1;
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
         break;

         case 1:
            if ( isalnum( chr ) || chr == '_' || chr == '-' || chr == ':' ) {
               buf[ iPos++ ] = chr;
            }
            else if ( chr == '>' || chr == ' ' || chr == '/' || chr == '\r'
                  || chr == '\t' || chr == '\n' ) {
               mxml_refil_ungetc( ref, chr );
               iStatus = 2;
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
         break;
      }
  }

   if ( ref->status != MXML_STATUS_OK ) {
      doc->status = ref->status;
      doc->error = ref->error;
      return doc->status;
   }

   if ( iStatus != 2  ) {
      doc->status = MXML_STATUS_MALFORMED;
      doc->error = MXML_ERROR_NAMETOOLONG;
      return MXML_STATUS_ERROR;
   }

   pNode->name = ( char *) MXML_ALLOCATOR( iPos + 1);
   memcpy( pNode->name, buf, iPos );
   pNode->name[ iPos ] = 0;

   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_node_read_attributes( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{

   MXML_ATTRIBUTE *head, *tail;

   head = tail = mxml_attribute_read( ref, doc );

   while ( doc->status == MXML_STATUS_OK && tail != NULL ) {
      tail->next = mxml_attribute_read( ref, doc );
      tail = tail->next;
   }

   if ( ref->status == MXML_STATUS_OK ) {
      pNode->attributes = head;
   }
   else {
      doc->status = ref->status;
      doc->error = ref->error;
   }

   return doc->status;
}

static void mxml_node_read_directive( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   char *buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int iAllocated = MXML_ALLOC_BLOCK;
   int iPos = 0;
   int chr;

   pNode->type = MXML_TYPE_DIRECTIVE;
   if ( mxml_node_read_name( ref, pNode, doc ) == MXML_STATUS_OK ) {
      chr = mxml_refil_getc( ref );
      while ( chr != MXML_EOF && chr != '>') {
         if ( iPos > 0 || ( chr != ' ' && chr != '\t' && chr != '\r' && chr != '\n' ) )
            buf[ iPos++ ] = chr;

         if ( iPos >= iAllocated ) {
            iAllocated += MXML_ALLOC_BLOCK;
            buf = (char *) MXML_REALLOCATOR( buf, iAllocated );
         }

         if ( chr == MXML_LINE_TERMINATOR )
            doc->iLine++;

         chr = mxml_refil_getc( ref );
      }

      if ( ref->status == MXML_STATUS_OK ) {
         buf[ iPos ] = 0;
         pNode->data = buf;
         pNode->data_length = iPos;
      }
      else {
         doc->status = ref->status;
         doc->error = ref->error;
      }

   }
}

static void mxml_node_read_pi( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   int iPos = 0, iAllocated;
   int chr;
   char *buf;
   int iStatus = 0;

   pNode->type = MXML_TYPE_PI;
   // let's read the xml PI instruction
   if ( mxml_node_read_name( ref, pNode, doc ) != MXML_STATUS_OK )
      return;

   // and then we'll put all the "data" into the data member, up to ?>

   buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK ;
   chr = 1;
   while ( iStatus < 2 ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         // scanning for ?>
         case 0:
            if ( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine++;
               buf[ iPos ++ ] = chr;
            }
            else if ( chr == '?' )
               iStatus = 1;
            else {
               if ( iPos > 0 || ( chr != ' ' && chr != '\n' ) )
                  buf[ iPos++ ] = chr;
            }
         break;

         case 1:
            if ( chr == '>' )
               iStatus = 2;
            else {
               iStatus = 0;
               buf[ iPos++ ] = '?';
               mxml_refil_ungetc( ref, chr );
            }
         break;

      }

      if ( iPos == iAllocated ) {
         iAllocated += MXML_ALLOC_BLOCK;
         buf = (char *) MXML_REALLOCATOR( buf, iAllocated );
      }
   }

   if ( ref->status == MXML_STATUS_OK ) {
      buf[iPos] = 0;
      pNode->data = buf;
      pNode->data_length = iPos;
   }
   else {
      doc->status = ref->status;
      doc->error = ref->error;
   }
}

static void mxml_node_read_tag( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   char chr;

   pNode->type = MXML_TYPE_TAG;

   if ( mxml_node_read_name( ref, pNode, doc ) == MXML_STATUS_OK ) {
      mxml_node_read_attributes( ref, pNode, doc );
   }

   // if the list of attributes terminates with a '/', the last '>' is
   // left unread. This means the current node is complete.
   chr = mxml_refil_getc( ref );
   if ( ref->status == MXML_STATUS_OK && chr != '>' ) {
      mxml_refil_ungetc( ref, chr );
      // recurse
      mxml_node_read( ref, pNode, doc );
   }
   else if ( ref->status != MXML_STATUS_OK ) {
      doc->status = ref->status;
      doc->error = ref->error;
   }

   //else the node is complete
}

static void mxml_node_read_comment( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   int iPos = 0, iAllocated;
   int chr;
   char *buf;
   int iStatus = 0;

   pNode->type = MXML_TYPE_COMMENT;
   //  we'll put all the comment into the data member, up to ->

   chr = 1;
   buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK ;

   while ( iStatus < 3 ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         // scanning for ->
         case 0:
            if ( chr == MXML_LINE_TERMINATOR ) {
               doc->iLine++;
               buf[ iPos ++ ] = chr;
            }
            else if ( chr == '-' )
               iStatus = 1;
            else
               buf[ iPos++ ] = chr;
         break;

         case 1:
            if ( chr == '-' )
               iStatus = 2;
            else {
               iStatus = 0;
               buf[ iPos++ ] = '-';
               mxml_refil_ungetc( ref, chr );
            }
         break;

         case 2:
            if ( chr == '>' )
               iStatus = 3;
            else {
               iStatus = 0;
               buf[ iPos++ ] = '-';
               mxml_refil_ungetc( ref, chr );
            }
         break;

      }

      if ( iPos == iAllocated ) {
         iAllocated += MXML_ALLOC_BLOCK;
         buf = (char *) MXML_REALLOCATOR( buf, iAllocated );
      }
   }

   if ( ref->status == MXML_STATUS_OK ) {
      buf[ iPos ] = 0;
      pNode->data = buf;
      pNode->data_length = iPos;
   }
   else {
      doc->status = ref->status;
      doc->error = ref->error;
   }
}

// checking closing tag
static void mxml_node_read_closing( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   char buf[ MXML_MAX_NAME_LEN + 1];
   int iPos = 0;
   int chr;

   chr = 1;
   while ( iPos <= MXML_MAX_NAME_LEN ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF || chr == '>') break;

      buf[ iPos++ ] = chr;
   }

   if ( ref->status != MXML_STATUS_OK ) {
      doc->status = ref->status;
      doc->error = ref->error;
      return;
   }

   if ( iPos > MXML_MAX_NAME_LEN ) {
      doc->status = MXML_STATUS_MALFORMED;
      doc->error = MXML_ERROR_NAMETOOLONG;
   }

   buf[ iPos ] = 0;
   if ( chr != '>' || strcmp( pNode->name, buf ) != 0 ) {
      doc->status = MXML_STATUS_MALFORMED;
      doc->error = MXML_ERROR_UNCLOSED;
   }
   // all fine
}

MXML_STATUS mxml_node_read( MXML_REFIL *ref, MXML_NODE *pNode, MXML_DOCUMENT *doc )
{
   MXML_NODE *node, *child_node, *data_node;
   int chr;
   /* Stateful machine status */
   int iStatus = 0;

   chr = 1;
   while ( iStatus >= 0) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      // resetting new node foundings
      node = NULL;

      switch ( iStatus ) {

         case 0:  // outside nodes
            switch ( chr ) {
               case MXML_LINE_TERMINATOR: doc->iLine++; break;
               // We repeat line terminator here for portability
               case MXML_SOFT_LINE_TERMINATOR: break;
               case ' ': case '\t': break;
               case '<': iStatus = 1; break;
               default:  // it is a data node
                  mxml_refil_ungetc( ref, chr );
                  node = mxml_node_new();
                  mxml_node_read_data( ref, node, doc );
            }
         break;

         case 1: //inside a node, first character
            if ( chr == '/' ) {
               // This can be met only inside current tag
               iStatus = -1; // done
            }
            else if ( chr == '!' ) {
               iStatus = 2;
            }
            else if ( chr == '?' ) {
               node = mxml_node_new();
               mxml_node_read_pi( ref, node, doc );
            }
            else if ( isalpha( chr ) ) {
               mxml_refil_ungetc( ref, chr );
               node = mxml_node_new();
               mxml_node_read_tag( ref, node, doc );
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_INVNODE );
            }
         break;

         case 2: //inside a possible comment (<!-/<!?)
            if ( chr == '-') {
               iStatus = 3;
            }
            else if ( isalpha( chr ) ) {
               node = mxml_node_new();
               mxml_refil_ungetc( ref, chr );
               mxml_node_read_directive( ref, node, doc );
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_INVNODE );
            }
         break;

         case 3:
            if ( chr == '-') {
               node = mxml_node_new();
               mxml_node_read_comment( ref, node, doc );
            }
            else {
               MALFORMED_ERROR( doc, MXML_ERROR_INVNODE );
            }
         break;
      }

      // have I to add a node below our structure ?
      if ( node != NULL ) {
         if ( ref->status == MXML_STATUS_OK ) {
            mxml_node_add_below( pNode, node );
            // beginning again - a new node is born
            doc->node_count++;
            iStatus = 0;
         }
         else {
            doc->status = ref->status;
            doc->error = ref->error;
            mxml_node_destroy( node );
            return doc->status;
         }
      }

   }

   // if we have an hard error on stream
   if ( ref->status != MXML_STATUS_OK ) {
      doc->status = ref->status;
      doc->error = ref->error;
      return doc->status;
   }

   if ( iStatus == -1 ) { // ARE WE DONE?
      /* Time to close current node. We must verify:
         1) If the closing tag is coherent with the opened tag name.
         2) If the tag has just one data node as child.
         if we have only one data node among the children, the data
         node is destroyed and the data element is moved to the
         "data" field of current node, to simplify the tree structure
         in the most common config oriented XML files.
      */
      mxml_node_read_closing( ref, pNode, doc );
      // malformed closing tag?
      if ( ref->status != MXML_STATUS_OK ) {
         doc->status = ref->status;
         doc->error = ref->error;
         return doc->status;
      }

      //checking for data nodes
      child_node = pNode->child;
      data_node = NULL;
      while ( child_node != NULL ) {
         if ( child_node->type == MXML_TYPE_DATA ) {
            // first data node ?
            if ( data_node == NULL )
               data_node = child_node;
            // ... or have we more than a data node?
            else {
               data_node = NULL;
               break;
            }
         }
         child_node = child_node->next;
      }

      if ( data_node != NULL ) {
         pNode->data = data_node->data;
         pNode->data_length = data_node->data_length;
         data_node->data = NULL;

         mxml_node_unlink( data_node );
         mxml_node_destroy( data_node );
         doc->node_count--;
      }

   }

   return MXML_STATUS_OK;
}

void mxml_node_write_attributes( MXML_OUTPUT *out, MXML_ATTRIBUTE *attr )
{
   while ( attr != NULL && out->status == MXML_STATUS_OK) {
      mxml_output_char( out, ' ' );
      mxml_attribute_write( out, attr );
      attr = attr->next;
   }
}

static void mxml_node_file_indent( MXML_OUTPUT *out, int depth, int style )
{
   int i;

   for ( i = 0; i < depth; i++ ) {
      if ( style & MXML_STYLE_TAB )
         mxml_output_char( out, '\t');
      else if (  style & MXML_STYLE_THREESPACES )
         mxml_output_string_len( out, "   ", 3 );
      else
         mxml_output_char( out, ' ' );
   }
}


MXML_STATUS mxml_node_write( MXML_OUTPUT *out, MXML_NODE *pNode, int style )
{
   MXML_NODE *child;
   int depth = 0;
   int mustIndent = 0;

   if ( style & MXML_STYLE_INDENT ) {
      depth = mxml_node_get_path_depth( pNode )-1;
      mxml_node_file_indent( out, depth, style );
   }

   switch( pNode->type ) {
      case MXML_TYPE_TAG:

         mxml_output_char( out, '<' );
         mxml_output_string( out, pNode->name );

         if ( pNode->attributes != NULL )
            mxml_node_write_attributes( out, pNode->attributes );
         if ( pNode->data == NULL && pNode->child == NULL ) {
            mxml_output_string_len( out, " />\n", 4 );
         }
         else {
            mxml_output_char( out, '>' );

            child = pNode->child;
            if ( child != NULL ) {
               mustIndent = 1;
               mxml_output_char( out, '\n' );

               while ( child != NULL ) {
                  mxml_node_write( out, child, style );
                  child = child->next;
               }
            }

            if ( pNode->data != NULL ) {
              if ( mustIndent && ( style & MXML_STYLE_INDENT ) )
                  mxml_node_file_indent( out, depth+1, style );
              mxml_output_string( out, pNode->data );
            }

            if ( mustIndent && ( style & MXML_STYLE_INDENT ))
               mxml_node_file_indent( out, depth, style );

            mxml_output_string_len( out, "</", 2 );
            mxml_output_string( out, pNode->name );
            mxml_output_string_len( out, ">\n", 2 );
         }
      break;

      case MXML_TYPE_COMMENT:
            mxml_output_string_len( out, "<!--", 4 );
            mxml_output_string( out, pNode->data );
            mxml_output_string_len( out, "-->\n", 4 );
      break;

      case MXML_TYPE_DATA:
         mxml_output_string( out, pNode->data );
      break;

      case MXML_TYPE_DIRECTIVE:
         mxml_output_string_len( out, "<!", 2 );
         mxml_output_string( out, pNode->name );
         if ( pNode->data != NULL ) {
            mxml_output_string( out, pNode->data );
         }
         mxml_output_string_len( out, ">\n", 2 );
      break;

      case MXML_TYPE_PI:
         mxml_output_string_len( out, "<?", 2 );
         mxml_output_string( out, pNode->name );
         if( pNode->data != NULL ) {
            mxml_output_char( out, ' ' );
            mxml_output_string( out, pNode->data );
         }
         mxml_output_string_len( out, "?>\n", 3 );
      break;

      case MXML_TYPE_DOCUMENT:
         child = pNode->child;
         while ( child != NULL ) {
               mxml_node_write( out, child, style );
               child = child->next;
         }
         mxml_output_char( out, '\n' );
      break;

   }

   if ( out->status != MXML_STATUS_OK ) {
      return out->status;
   }

   // just for progress indicators
   out->node_done++;

   return MXML_STATUS_OK;
}


/***********************************************************
   HBXML lib
   Virtual stream input/output routines
***********************************************************/

/**
* Creates a new output object
* In this case, the func member is required.
* Node count is optional, but highly wanted for progress indicators.
*/
MXML_OUTPUT *mxml_output_new( MXML_OUTPUT_FUNC func, int node_count)
{
   MXML_OUTPUT * ret = (MXML_OUTPUT* ) MXML_ALLOCATOR( sizeof( MXML_OUTPUT ) );

   if ( ret == NULL )
      return NULL;

   if ( mxml_output_setup( ret, func, node_count ) == MXML_STATUS_OK )
      return ret;

   MXML_DELETOR( ret );
   return NULL;
}

/**
* Sets up output parameters.
* In this case, the func member is required.
* Node count is optional, but highly wanted for progress indicators.
*/

MXML_STATUS mxml_output_setup( MXML_OUTPUT *out, MXML_OUTPUT_FUNC func, int node_count)
{
   if ( func == NULL ) {
      return MXML_STATUS_ERROR;
   }

   out->output_func = func;
   out->node_count = node_count;
   out->node_done = 0;

   out->status = MXML_STATUS_OK;
   out->error = MXML_ERROR_NONE;
   return MXML_STATUS_ERROR;
}

void mxml_output_destroy( MXML_OUTPUT *out )
{
   MXML_DELETOR( out );
}

/**********************************************/
/* output functions                           */

MXML_STATUS mxml_output_char( MXML_OUTPUT *out, int c )
{
   char chr = (char) c;
   out->output_func( out, &chr, 1 );
   return out->status;
}

MXML_STATUS mxml_output_string_len( MXML_OUTPUT *out, char *s, int len )
{
   out->output_func( out, s, len );
   return out->status;
}

MXML_STATUS mxml_output_string( MXML_OUTPUT *out, char *s )
{
   return mxml_output_string_len( out, s, strlen( s ) );
}

/**
* Useful function to output to streams
*/

void mxml_output_func_to_stream( MXML_OUTPUT *out, char *s, int len )
{
   FILE *fp = (FILE *) out->data;

   if ( len == 1 )
      fputc( *s, fp );
   else
      fwrite( s, 1, len, fp );

   if ( ferror( fp ) ) {
      out->status = MXML_STATUS_ERROR;
      out->error = MXML_ERROR_IO;
   }
}

/**
* Useful function to output to file handles
*/
void mxml_output_func_to_handle( MXML_OUTPUT *out, char *s, int len )
{
   int fh = (int) out->data;
   int olen;
   olen = write( fh, s, len );

   if ( olen < len ) {
      out->status = MXML_STATUS_ERROR;
      out->error = MXML_ERROR_IO;
   }
}

/**
* Useful function to output to self growing strings
*/
void mxml_output_func_to_sgs( MXML_OUTPUT *out, char *s, int len )
{
   MXML_SGS *sgs = (MXML_SGS *) out->data;
   MXML_STATUS stat;

   if ( len == 1 )
      stat = mxml_sgs_append_char( sgs, *s );
   else
      stat = mxml_sgs_append_string_len( sgs, s, len );

   if ( stat != MXML_STATUS_OK ) {
      out->status = MXML_STATUS_ERROR;
      out->error = MXML_ERROR_NOMEM;
   }
}

/***********************************************************
   HBXML lib
   XML path oriented operations
   TODO: path finding
***********************************************************/

char *mxml_node_get_path_new( MXML_NODE* node )
{
   int pathlen = 0;
   char *path, *path1;
   MXML_NODE *parent = node;

   if ( node->type == MXML_TYPE_DOCUMENT )
      return NULL;

   while( parent != NULL && parent->name != NULL ) {
      pathlen += strlen( parent->name )+1;
      parent = parent->parent;
   }

   path = (char *) MXML_ALLOCATOR( pathlen + 1 );
   path1 = (char *) MXML_ALLOCATOR( pathlen + 1 );

   path[0] = 0;
   parent = node;

   while ( parent != NULL && parent->name != NULL ) {
      sprintf( path1 , "/%s%s", parent->name, path );
      strcpy( path, path1 );
      parent = parent->parent;
   }

   MXML_DELETOR( path1 );
   return path;
}

int mxml_node_get_path_depth( MXML_NODE* node )
{
   int depth = 0;
   MXML_NODE *parent = node;

   while( parent != NULL && parent->type != MXML_TYPE_DOCUMENT ) {
      depth++;
      parent = parent->parent;
   }

   return depth;
}

int mxml_node_get_path_length( MXML_NODE* node )
{
   int length = 0;
   MXML_NODE *parent = node;

   while( parent != NULL && parent->name != NULL) {
      length += strlen( parent->name )+1;
      parent = parent->parent;
   }

   return length;
}

int mxml_node_get_path( MXML_NODE* node, char *path )
{
   char *reverse_path[ MXML_MAX_DEPTH ];
   int pos = 0;
   int i;
   int plen = 0;
   MXML_NODE *parent = node;

   while( parent != NULL && parent->name != NULL) {
      reverse_path[pos++] = parent->name;
      parent = parent->parent;
   }

   for ( i = pos-1; i >= 0; i-- ) {
      sprintf( path + plen, "/%s", reverse_path[ i ] );
      plen += strlen( reverse_path[ i ] )+1;
   }

   path[ plen ] = 0;
   return plen;
}

/***********************************************************
   HBXML lib
   Refiller routines
***********************************************************/


/**
* Creates a new refiller object.
* If buf is null, then buflen is ignored and set to 0; the first retrival
* of a character will then lead to refil func calling.
* If the function is null, once the data has been read the reader returns
* eof. If both func and buf are NULL, the creation fails, and the function
* retunrs NULL.
*/
MXML_REFIL *mxml_refil_new( MXML_REFIL_FUNC func, char *buf, int buflen,
   int bufsize )
{
   MXML_REFIL * ret = (MXML_REFIL* ) MXML_ALLOCATOR( sizeof( MXML_REFIL ) );

   if ( ret == NULL )
      return NULL;

   if ( mxml_refil_setup( ret, func, buf, buflen, bufsize ) == MXML_STATUS_OK )
      return ret;

   MXML_DELETOR( ret );
   return NULL;
}

/**
* Sets up refiller parameters.
* If buf is null, then buflen is ignored and set to 0; the first retrival
* of a character will then lead to refil func calling. Bufsize is the size
* of the allocated memory, while buflen is the count of currently valid
* characters in that buffer.
* If the function is null, once the data has been read the reader returns
* eof. If both func and buf are NULL, the function fails and returns
* MXML_STATUS_ERROR. On success, returns MXML_STATUS_OK.
* Notice: ref->data member is left to fill to the
* calling program, if this is needed.
*/

MXML_STATUS mxml_refil_setup( MXML_REFIL *ref, MXML_REFIL_FUNC func,
   char *buf, int buflen, int bufsize )
{

   if ( buf == NULL && func == NULL )
      return MXML_STATUS_ERROR;

   ref->refil_func = func;
   ref->buffer = buf;

   ref->status = MXML_STATUS_OK;
   ref->error = MXML_ERROR_NONE;

   if (buf == NULL)
      ref->buflen = ref->bufsize = 0;
   else {
      ref->buflen = buflen;
      ref->bufsize = bufsize;
   }

   ref->bufpos = 0;

   //stream length is left for the program to implement progress indicators
   ref->streamlen = 0;
   ref->streampos = 0;

   //theese are for ungetc operations
   ref->sparechar = MXML_EOF;

   //data is left to fill for the program
   return MXML_STATUS_OK;
}


void mxml_refil_destroy ( MXML_REFIL *ref ) {
   MXML_DELETOR( ref );
}

int mxml_refil_getc( MXML_REFIL *ref )
{
   if ( ref->sparechar != MXML_EOF ) {
      int chr = ref->sparechar;
      ref->sparechar = MXML_EOF;
      return chr;
   }

   if ( ref->bufpos >= ref->buflen ) {
      if ( ref->refil_func != NULL ) {
         ref->refil_func( ref );
         if ( ref->status != MXML_STATUS_OK || ref->buflen == 0)
            return MXML_EOF;
      }
      else
         return MXML_EOF;
   }

   return ref->buffer[ ref->bufpos++ ];
}


/* implemented as a macro
void mxml_refil_ungetc( MXML_REFIL *ref, int chr )
{
   ref->sparechar = chr;
}
*/

/**
* Useful "fill" function that reads from a file handle
*/

void mxml_refill_from_handle_func( MXML_REFIL *ref )
{
   int fh = (int) ref->data;
   int len;

   len = read( fh, ref->buffer, ref->bufsize );

   if ( len == -1 ) {
      ref->status = MXML_STATUS_ERROR;
      ref->error = MXML_ERROR_IO;
   }
   else {
      ref->buflen = len;
      ref->bufpos = 0;
   }
}



/********************************************************
   HBXML lib
   Self growing string routines
*********************************************************/

/**
* Creates a new self growing string, with buffer set to
* minimal buffer length
*/
MXML_SGS *mxml_sgs_new()
{
   MXML_SGS * ret = (MXML_SGS* ) MXML_ALLOCATOR( sizeof( MXML_SGS ) );

   if ( ret == NULL )
      return NULL;

   ret->buffer = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   if ( ret->buffer == NULL ) {
      MXML_DELETOR( ret );
      return NULL;
   }

   ret->allocated = MXML_ALLOC_BLOCK;
   ret->buffer[0] = 0;
   ret->length = 0;

   return ret;
}

void mxml_sgs_destroy( MXML_SGS *sgs )
{
   if ( sgs->buffer != NULL )
      MXML_DELETOR( sgs->buffer );

   MXML_DELETOR( sgs );
}

/****************************************/

MXML_STATUS mxml_sgs_append_char( MXML_SGS *sgs, char c )
{
   char *buf;
   sgs->buffer[ sgs->length++ ] = c;

   if ( sgs->length >= sgs->allocated ) {
      buf = (char *) MXML_REALLOCATOR( sgs->buffer, sgs->allocated + MXML_ALLOC_BLOCK );
      if ( buf == NULL ) {
         return MXML_STATUS_ERROR;
      }
      sgs->allocated += MXML_ALLOC_BLOCK;
      sgs->buffer = buf;
   }

   sgs->buffer[sgs->length] = '\0';

   return MXML_STATUS_OK;
}

MXML_STATUS mxml_sgs_append_string_len( MXML_SGS *sgs, char *s, int slen )
{
   char *buf;

   if ( slen == 0 )
      return MXML_STATUS_OK;

   if ( sgs->length + slen >= sgs->allocated ) {
      int blklen = ( ( sgs->length + slen ) / MXML_ALLOC_BLOCK + 1) * MXML_ALLOC_BLOCK;
      buf = (char *) MXML_REALLOCATOR( sgs->buffer, blklen );

      if ( buf == NULL ) {
         return MXML_STATUS_ERROR;
      }
      sgs->allocated = blklen;
      sgs->buffer = buf;
   }

   memcpy( sgs->buffer + sgs->length , s, slen + 1 ); // include also the trailing space
   sgs->length += slen;

   return MXML_STATUS_OK;
}


MXML_STATUS mxml_sgs_append_string( MXML_SGS *sgs, char *s )
{
   return mxml_sgs_append_string_len( sgs, s, strlen( s ) );
}

/***********************************************************
   HBXML lib
   Error code routines
***********************************************************/

static char *edesc[] =
{
   "Input/output error",
   "Not enough memory",
   "Character outside tags",
   "Invalid character as tag name",
   "Invalid character as attribute name",
   "Malformed attribute definition",
   "Invalid character",
   "Name of tag too long",
   "Name of attribute too long",
   "Value of attribute too long",
   "Unbalanced tag closeure"
};

char *mxml_error_desc( MXML_ERROR_CODE code )
{
   code --;
   if ( code < 0 || code > sizeof( edesc ) / sizeof( char * ) )
      return NULL;

   return edesc[ code ];
}




/***********************************************************
   HBXML lib
   xHarbour RTL & VM interface
***********************************************************/

/**
* Useful function to create an attribute list from an array
*/
static MXML_ATTRIBUTE *hbxml_create_attributes( PHB_ITEM pAttribs )
{
   MXML_ATTRIBUTE *ret = NULL, *tail = NULL;
   PHB_ITEM pAtt;
   int i;

   for( i = 1; i <= hb_arrayLen( pAttribs ); i ++ )
   {
      pAtt = hb_arrayGetItemPtr( pAttribs, i );
      if( pAtt->type != HB_IT_ARRAY || hb_arrayLen( pAtt ) != 2 )
      {
         //TODO: Signal malformed attribute list ?
         return NULL;
      }

      if ( tail == NULL )
      {
         ret = tail = mxml_attribute_new();
      }
      else
      {
         tail->next = mxml_attribute_new();
         tail = tail->next;
      }

      tail->name = hb_arrayGetC( pAtt, 1 );
      tail->value = hb_arrayGetC( pAtt, 2 );

      if( tail->name == NULL || tail->value == NULL )
      {
         // TODO: Signal malformed attribute list ?
         mxml_attribute_destroy( ret );
         return NULL;
      }
   }

   return ret;
}


/**
* Useful function to create a node from PHB_ITEMs
*/
static MXML_NODE *hbxml_create_node( MXML_NODE *origin,
      PHB_ITEM pType, PHB_ITEM pName, PHB_ITEM pAttribs, PHB_ITEM pData )
{
   int type = hb_itemGetNI( pType );
   MXML_NODE *ret = NULL;

   switch( type ) {
      case MXML_TYPE_DATA: case MXML_TYPE_COMMENT:
         // data & comment nodes does not have names nor attributes
         if( pName != NULL )
         {
            return NULL;
         }
         if( pAttribs != NULL && hb_arrayLen( pAttribs ) != 0 )
         {
            return NULL;
         }

         if ( origin != NULL )
         {
            ret = origin;
         }
         else
         {
            ret = mxml_node_new();
         }
         ret->type = type;
         ret->data = hb_itemGetC( pData );
      break;

      case MXML_TYPE_PI: case MXML_TYPE_DIRECTIVE:
         // directives and proc instr have not attribs.
         if( pAttribs != NULL && hb_arrayLen( pAttribs ) != 0 )
         {
            return NULL;
         }

         if ( origin != NULL )
         {
            ret = origin;
         }
         else
         {
            ret = mxml_node_new();
         }
         ret->type = type;
         ret->name = hb_itemGetC( pName );
         ret->data = hb_itemGetC( pData );
      break;

      case MXML_TYPE_TAG:
         if ( origin != NULL )
         {
            ret = origin;
         }
         else
         {
            ret = mxml_node_new();
         }
         ret->type = type;
         ret->name = hb_itemGetC( pName );
         ret->attributes = hbxml_create_attributes( pAttribs );
         ret->data = hb_itemGetC( pData );
      break;

      //note: applications CANNOT create MXML_TYPE_DOCUMENT nodes
   }

   //TODO: signal malformed parameter errors?
   return ret;
}

/**
* Useful function to translate HB parameter(s) into a node
*/
static MXML_NODE *hbxml_node_from_params( int iPos, MXML_NODE *origin )
{
   PHB_ITEM pFirst = hb_param( iPos, HB_IT_ANY );
   MXML_NODE *ret = NULL;

   // If it is an iterator, we must copy all the subtree.
   if( pFirst->type == HB_IT_STRING ) {
      MXML_ITERATOR *iter = (MXML_ITERATOR *) hb_itemGetCPtr( pFirst );
      ret = mxml_node_clone_tree( iter->node );
      if( origin != NULL ) {
         if ( origin->attributes  != NULL )
         {
            mxml_attribute_destroy( origin->attributes );
         }
         memcpy( origin, ret, sizeof ( MXML_NODE ) );
         MXML_DELETOR( ret );
         ret = origin;
      }
   }
   // if it is an integer, we expect a sequence of int, chr, arr, chr, but
   else if ( HB_IS_INTEGER( pFirst ) )
   {
      PHB_ITEM pName =  hb_param( iPos + 1, HB_IT_STRING );
      PHB_ITEM pAttribs =  hb_param( iPos + 2, HB_IT_ARRAY );
      PHB_ITEM pData =  hb_param( iPos + 3, HB_IT_STRING );
      ret = hbxml_create_node( origin, pFirst, pName, pAttribs, pData );
   }
   //else, if it is an array...
   else if ( pFirst->type == HB_IT_ARRAY )
   {
      PHB_ITEM pType = hb_arrayGetItemPtr( pFirst, 1 );
      if ( pType->type == HB_IT_INTEGER ){
         PHB_ITEM pName =  hb_arrayGetItemPtr( pFirst, 2 );
         PHB_ITEM pAttribs =  hb_arrayGetItemPtr( pFirst, 3 );
         PHB_ITEM pData =  hb_arrayGetItemPtr( pFirst, 4 );

         ret = hbxml_create_node(
            origin,
            pType,
            pName->type == HB_IT_STRING ? pName: NULL,
            pAttribs->type == HB_IT_ARRAY ? pAttribs: NULL,
            pData->type == HB_IT_STRING ? pData: NULL
         );
      }
   }

   return ret;
}

/**
* HB_XmlCreate( [xData] ) --> xmlDocument
* xData can be a file handle from which an XML can be read,
* a string containing an XML tree or NIL, in which case the
* document is created empty.
*/

HB_FUNC( HB_XMLCREATE )
{
   PHB_ITEM pParam = hb_param(1, HB_IT_ANY );
   MXML_DOCUMENT *doc;
   MXML_REFIL refil;
   char buffer[512];

   if( pParam != NULL &&
      ( !HB_IS_STRING( pParam ) && !HB_IS_NUMERIC( pParam)) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Parameter must be string or file handle",
         NULL,
         1, pParam);
      return;
   }

   /* empty document */
   doc = mxml_document_new();

   if( pParam != NULL )
   {
      if( pParam->type == HB_IT_STRING )
      {
         mxml_refil_setup( &refil, NULL,
            pParam->item.asString.value,
            pParam->item.asString.length,
            pParam->item.asString.length );
      }
      else // can only be an integer, that is, a file handle
      {
         mxml_refil_setup( &refil,
            mxml_refill_from_handle_func,
            buffer, 0, 512 );

         refil.data = (void *) hb_itemGetNI( pParam );
      }

      mxml_node_read( &refil, doc->root, doc );
   }

   //now we can return our document.
   hb_retptr( doc );
}

/**
* HB_XmlDestroy( xmlDocument ) --> NIL
*
* Destroys a document and all the associated XML data.
* TODO: I would like to do this automatically in garbage collector.
*/

HB_FUNC( HB_XMLDESTROY )
{
   PHB_ITEM pParam = hb_param(1, HB_IT_POINTER );

   if( pParam == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Parameter must be an XML Doucment object",
         NULL,
         1, pParam);
      return;
   }

   mxml_document_destroy( (MXML_DOCUMENT *) hb_itemGetPtr( pParam ) );
}

/**
* HB_XmlStatus( xmlDocument[, nNewStatus] ) --> nStatus
* Returns one of the HBXML_STATUS variables telling what is the
* status of the document. IF nNewStatus is set, the status of the
* document will be updated.
*/

HB_FUNC( HB_XMLSTATUS )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   PHB_ITEM pNewStatus = hb_param(2, HB_IT_INTEGER );
   MXML_DOCUMENT *doc;
   int iStatus;

   if( pDoc == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type/count",
         NULL,
         2, pDoc, pNewStatus );
      return;
   }

   doc = (MXML_DOCUMENT *) hb_itemGetPtr( pDoc );
   iStatus = doc->status;

   if ( pNewStatus != NULL ) {
      doc->status = hb_itemGetNI( pNewStatus );
   }

   hb_retni( iStatus );
}

/**
* HB_XmlError( xmlDocument[, nNewStatus] ) --> nError
* Returns one of the HBXML_ERROR type telling what is the error
* encountered while processing the document. IF nNewStatus is set,
* the status of the document will be updated.
*/

HB_FUNC( HB_XMLERROR )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   PHB_ITEM pNewError = hb_param(2, HB_IT_INTEGER );
   MXML_DOCUMENT *doc;
   int iStatus;

   if( pDoc == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type/count",
         NULL,
         2, pDoc, pNewError );
      return;
   }

   doc = (MXML_DOCUMENT *) hb_itemGetPtr( pDoc );
   iStatus = doc->status;

   if ( pNewError != NULL ) {
      doc->status = hb_itemGetNI( pNewError );
   }

   hb_retni( iStatus );
}


/**
* HB_XmlLine( xmlDocument ) --> nLine
* Returns the lines that have been processed. If an error is encountered,
* it returns the position in the file where the error have been detected.
*/
HB_FUNC( HB_XMLLINE )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   MXML_DOCUMENT *doc;

   if( pDoc == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pDoc );
      return;
   }

   doc = (MXML_DOCUMENT *) hb_itemGetPtr( pDoc );

   hb_retni( doc->iLine );
}

/**
* HB_XmlErrorDesc( nErrorNumber ) --> cErrorDesc
* Returns a descriptive string telling what the error number is meaning.
*/
HB_FUNC( HB_XMLERRORDESC )
{
   PHB_ITEM pNum = hb_param(1, HB_IT_NUMERIC );

   if( pNum == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type (should be anumber)",
         NULL,
         1, pNum );
      return;
   }


   hb_retc( mxml_error_desc( hb_itemGetNI( pNum ) ) );
}


/**
* HB_XmlGetIterator( xmlDocument ) -->xmlIterator
*
* Creates an iterator starting positioned at the root node of the current
* document.
* Notice: since the iterator is a flat pointer, it is stored
* in an xharbour string, so that garbage collector can easily dispose of it
* when needed, and so that it can easily inspected from PRG programs.
*/

HB_FUNC( HB_XMLGETITERATOR )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   MXML_DOCUMENT *doc;
   MXML_ITERATOR *iter;

   if( pDoc == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pDoc );
      return;
   }

   doc = (MXML_DOCUMENT *) hb_itemGetPtr( pDoc );
   iter = mxml_iterator_new( doc );

   hb_retclenAdoptRaw( (char *)iter, sizeof( MXML_ITERATOR ) );
}

/**
* HB_XmlCloneIterator( xmlIterator ) -->xmlIterator
*
* Clones the iterator.
* document.
*/

HB_FUNC( HB_XMLCLONEITERATOR )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter, *clone;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   clone = mxml_iterator_clone( iter );

   hb_retclenAdoptRaw( (char *)clone, sizeof( MXML_ITERATOR ) );
}

/**
* HB_XmlDepth( xmlIterator ) --> nDepth
*
* Returns the depth of the node where the iterator is currently pointing.
* 0 means document root, which is above every node in the document.
* Toplevel nodes have depth 1.
* Notice that if you build an iterator on a document that is built on
* a subtree of another document, it's depth will be relative to the
* new document tree, not to the original one.
*/

HB_FUNC( HB_XMLDEPTH )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );

   hb_retni( iter->level );
}

/**
* HB_XmlAscend( xmlIterator ) --> lCanGo
*
* Climbs up in the current hierarcy of the tree. Will return true if
* this operation is possible, else the iterator won't be moved, and
* the return value will be .F.
*/

HB_FUNC( HB_XMLASCEND )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   MXML_NODE *position;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   position = mxml_iterator_ascend( iter );

   hb_retl( position != NULL );
}

/**
* HB_XmlDescend( xmlIterator ) --> lCanGo
*
* Enters the child of the current node, if it exists. Will return true if
* this operation is possible, else the iterator won't be moved, and
* the return value will be .F.
*/

HB_FUNC( HB_XMLDESCEND )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   MXML_NODE *position;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   position = mxml_iterator_descend( iter );

   hb_retl( position != NULL );
}

/**
* HB_XmlNext( xmlIterator ) --> lCanGo
*
* Moves the iterator to the following brother of the current node, if it
* exists. Will return true if this operation is possible, else the
* iterator won't be moved, and the return value will be .F.
*/

HB_FUNC( HB_XMLNEXT )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   MXML_NODE *position;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   position = mxml_iterator_next_brother( iter );

   hb_retl( position != NULL );
}

/**
* HB_XmlPrevious( xmlIterator ) --> lCanGo
*
* Moves the iterator to the preceding brother of the current node, if it
* exists. Will return true if this operation is possible, else the
* iterator won't be moved, and the return value will be .F.
*/

HB_FUNC( HB_XMLPREVIOUS )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   MXML_NODE *position;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   position = mxml_iterator_previous_brother( iter );

   hb_retl( position != NULL );
}

/**
* HB_XmlNextInTree( xmlIterator ) --> lCanGo
*
* Moves the iterator to the next node in the tree, ascending when necessary.
* This function traverses all the nodes in the tree, if called interactively.
* Will return true if this operation is possible, else the
* iterator won't be moved, and the return value will be .F.
*/

HB_FUNC( HB_XMLNEXTINTREE )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   MXML_NODE *position;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   position = mxml_iterator_next( iter );

   hb_retl( position != NULL );
}

/**
* HB_XmlSetTop( xmlIterator ) --> NIL
*
* Sets the current node of the iterator as the top node: depth of this
* node will be 0, and "next" calls will traverse only the children
* of this node. HB_XmlGoTop will position the iterator at this node.
*/

HB_FUNC( HB_XMLSETTOP )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   mxml_iterator_set_top( iter, iter->node );
}

/**
* HB_XmlGoTop( xmlIterator ) --> NIL
*
* Positions the iterator to its topmost node, that is usually the document
* root node, unless another position has been specified with HB_XMLSetTop().
*/

HB_FUNC( HB_XMLGOTOP )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   mxml_iterator_top( iter );
}



/**
* Generic insertion function.
*/
static void hb_xml_insert( int mode )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_NODE *newnode;
   MXML_ITERATOR *iter;
   int type;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   type = iter->node->type;

   // only if this is a node that can accept children
   if( (mode <= 2 && (type == MXML_TYPE_TAG ||type == MXML_TYPE_DOCUMENT )) ||
       (mode > 2 && type != MXML_TYPE_DOCUMENT) )
   {
      newnode = hbxml_node_from_params( 2, NULL );

      if ( newnode != NULL )
      {
         switch ( mode )
         {
            case 1: mxml_iterator_add_below( iter, newnode ); break;
            case 2: // TODO: check for unwanted children
               mxml_iterator_insert_below( iter, newnode ); break;
            case 3: mxml_iterator_insert_before( iter, newnode ); break;
            case 4: mxml_iterator_insert_after( iter, newnode ); break;
         }

         hb_retl( TRUE );
         return;
      }
   }

   //TODO: Signal error if the target node was not a container node?

   hb_retl( FALSE );
}

/**
* HB_XmlAddBelow( xmlIterator, nNodeType, cNodeName, aNodeAttribs,
*                 cNodeData ) --> lCanDo
* HB_XmlAddBelow( xmlIterator, aNode ) --> lCanDo
* HB_XmlAddBelow( xmlIterator, xmlIterator ) --> lCanDo
*
* Inserts a node as the last child of current node. The node can be passed
* directly with all its components (first grammar), packed in an array
* (second grammar) or can be copied from an iterator, that can point to
* any node of the same tree of the first iterator, or even at another tree.
* The function returns false on failure, that is if the current node cannot
* have childs (like data or comment nodes), or if there isn't enough memory
* left.
*/

HB_FUNC( HB_XMLADDBELOW )
{
   hb_xml_insert( 1 );
}

/**
* HB_XmlInsertBelow( xmlIterator, nNodeType, cNodeName, aNodeAttribs,
*                 cNodeData ) --> lCanDo
* HB_XmlInsertBelow( xmlIterator, aNode ) --> lCanDo
* HB_XmlInsertBelow( xmlIterator, xmlIterator ) --> lCanDo
*
* Inserts a node between the target node pointed by the iterator and
* the first of its children, if it has, thus effectively inserting a
* new level in the tree.
* The inserted node can already have following brothers, but NOT preceding
* brothers NOR children (in case it is pointed by an iterator, last grammar).
* The function returns false on failure, that is if the current node cannot
* have childs (like data or comment nodes), or if there isn't enough memory
* left.
*/

HB_FUNC( HB_XMLINSERTBELOW )
{
   hb_xml_insert( 2 );
}


/**
* HB_XmlInsertBefore( xmlIterator, nNodeType, cNodeName, aNodeAttribs,
*                 cNodeData ) --> lCanDo
* HB_XmlInsertBefore( xmlIterator, aNode ) --> lCanDo
* HB_XmlInsertBefore( xmlIterator, xmlIterator ) --> lCanDo
*
* Inserts a node before currently pointed node. The node must NOT have
* brothers, but can have children (in the last grammar). If the target
* node is the first child of the parent of this node tree, the inserted
* node becomes the new first child.
*
* The function returns false on failure, that is if the current node cannot
* have broters (root document node), or if there isn't enough memory
* left.
*/

HB_FUNC( HB_XMLINSERTBEFORE )
{
   hb_xml_insert( 3 );
}

/**
* HB_XmlInsertAfter( xmlIterator, nNodeType, cNodeName, aNodeAttribs,
*                 cNodeData ) --> lCanDo
* HB_XmlInsertAfter( xmlIterator, aNode ) --> lCanDo
* HB_XmlInsertAfter( xmlIterator, xmlIterator ) --> lCanDo
*
* Inserts a node after currently pointed node. The node must NOT have
* brothers, but can have children (in the last grammar). If the target
* node is the last child of the parent of this node tree, the inserted
* node is appended at the end of the list.
*
* The function returns false on failure, that is if the current node cannot
* have broters (root document node), or if there isn't enough memory
* left.
*/

HB_FUNC( HB_XMLINSERTAFTER )
{
   hb_xml_insert( 4 );
}


/**
* Used by HB_XmlDestroyNode, HB_XmlRemoveNode and HB_XmlCopyNode
*/

static void hb_xml_node_extractor( int mode )
{

   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_NODE *node;
   MXML_ITERATOR *iter;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );

   if( mode <= 2 && iter->node->type == MXML_TYPE_DOCUMENT )
   {
      hb_retl( FALSE );
      return;
   }

   node = iter->node;

   if ( mode <= 2 ) // remove or destroy
   {
      if ( node->next != NULL )
      {
         mxml_iterator_next_brother( iter );
      }
      else
      {
         mxml_iterator_ascend( iter );
      }

      mxml_node_unlink( node );

      if ( mode == 1 ) // destroy
      {
         mxml_node_destroy( node );
         hb_retl( TRUE );
      }
   }

   if ( mode != 1 ) // not destroy!
   {
      MXML_DOCUMENT *doc = mxml_document_new();

      if ( doc != NULL )
      {
         if( node->type == MXML_TYPE_DOCUMENT )
         {
            mxml_node_destroy( doc->root );
            doc->root = node;
         }
         else {
            mxml_node_add_below( doc->root, node );
         }

         hb_retptr( doc );
      }
      else
      {
         hb_ret();
      }
   }

}


/**
* HB_XmlDeleteNode( xmlIterator ) --> lCanDo
*
* Deletes current node and all its children.
* The iterator is then set to the next brother of the destroyed node;
* if the destroyed node has not a next brother, the iterator is set to
* its parent.
* It is not possible to destroy the root node in this way.
*
* The function returns false on failure, that is if the current node
* is the topmost node. Use HB_XmlDestroy() on the document to do that.
*/

HB_FUNC( HB_XMLDELETENODE )
{
   hb_xml_node_extractor( 1 );
}

/**
* HB_XmlRemoveNode( xmlIterator ) --> xmlDocument | NIL
*
* Removes the current node from the tree, and creates a new tree where
* the removed node is the only child of the topmost (document) node.
* The iterator is then set to the next brother of the destroyed node;
* if the destroyed node has not a next brother, the iterator is set to
* its parent.
* It is not possible to remove the root node in this way.
*
* The function returns NIL on failure, that is if the current node
* is the topmost node.
*/

HB_FUNC( HB_XMLREMOVENODE )
{
   hb_xml_node_extractor( 2 );
}

/**
* HB_XmlCopyNode( xmlIterator ) --> xmlDocument | NIL
*
* Removes the current node from the tree, and creates a new tree where
* the removed node is the only child of the topmost (document) node.
* The iterator is not moved.
* In this way, it is also possible to copy an entire XML document, by
* obtaining its iterator and copying it immediately.
*
* The function returns NIL on failure (memory allocation error).
*/
HB_FUNC( HB_XMLCOPYNODE )
{
   hb_xml_node_extractor( 3 );
}

/**
* HB_XmlSetNode( xmlIterator, nNodeType, cNodeName, aNodeAttribs,
*                 cNodeData ) --> lCanDo
* HB_XmlSetNode( xmlIterator, aNode ) --> lCanDo
*
* Sets current node to the required values. Notice that, to ensure
* consistency of the resulting XML document, you have to correctly
* specify all the elements of the node; setting an element to NIL
* or not specifying it will have the effect of destroying it.
* So, if you want to modify an item, first read it with HB_XmlGetNode(),
* them modify it and then set it back.
*
* The function returns false if the current iterator cannot be set
* (the document root element cannot be changed!), or if the passed
* parameters are not consistent.
*/

HB_FUNC( HB_XMLSETNODE )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   PHB_ITEM pSecond = hb_param(2, HB_IT_STRING );
   MXML_NODE *newnode;
   MXML_ITERATOR *iter;
   int type;

   if( pIter == NULL || pSecond != NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         2, pIter, pSecond );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   type = iter->node->type;

   // Document nodes are unmodifiables
   if( type != MXML_TYPE_DOCUMENT )
   {
      newnode = hbxml_node_from_params( 2, iter->node );

      if ( newnode != NULL )
      {
         hb_retl( TRUE );
         return;
      }
   }

   hb_retl( FALSE );
}

/**
* HB_XmlGetNode( xmlIterator ) --> aNode
*
* Returns the current node in an array with format:
* { nType, cName, aAttributes, cData }
*
* aAttribute member having the format:
* { { cName1, cValue1} ... { cNameN, cValueN } }
*
* If the node has not a single data child, then the cData member
* is returned as NIL.
*/

HB_FUNC( HB_XMLGETNODE )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   PHB_ITEM pRet, pAttribs;
   MXML_NODE *node;
   MXML_ITERATOR *iter;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   node = iter->node;

   pRet = &HB_VM_STACK.Return;
   hb_arrayNew( pRet, 4 );

   hb_itemPutNI( hb_arrayGetItemPtr( pRet, 1 ), node->type );

   if ( node->name != NULL )
   {
      hb_itemPutC( hb_arrayGetItemPtr( pRet, 2 ), node->name );
   }

   pAttribs = hb_arrayGetItemPtr( pRet, 3 );
   hb_arrayNew( pAttribs, 0 );

   if ( node->attributes != NULL )
   {
      PHB_ITEM pAttr;
      MXML_ATTRIBUTE *attr = node->attributes;

      while ( attr != NULL )
      {
         pAttr = hb_itemNew( NULL );
         hb_arrayNew( pAttr, 2 );

         hb_itemPutC( hb_arrayGetItemPtr( pAttr, 1 ), attr->name );
         hb_itemPutC( hb_arrayGetItemPtr( pAttr, 2 ), attr->value );

         hb_arrayAddForward( pAttribs, pAttr );
         attr = attr->next;
      }
   }


   if ( node->data != NULL )
   {
      hb_itemPutC( hb_arrayGetItemPtr( pRet, 4 ), node->data );
   }

   // pRet is already == return
}

/**
* HB_XmlGetPath( xmlIterator ) --> cPath | NIL
*
* Returns the path of the current node. Nodes that have not
* a "name" cannot have a path, thus this function return NIL
* if applied on data or comment nodes.
*/

HB_FUNC( HB_XMLGETPATH )
{
   PHB_ITEM pIter = hb_param(1, HB_IT_STRING );
   MXML_ITERATOR *iter;
   char *ret;

   if( pIter == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pIter );
      return;
   }

   iter = (MXML_ITERATOR *) hb_itemGetCPtr( pIter );
   ret = mxml_node_get_path_new( iter->node );

   if ( ret != NULL )
   {
      hb_retcAdopt( ret );
   }
   else
   {
      hb_ret();
   }
}


/**
* HB_XmlToString( xmlDocument [, nStyle] ) --> cXml | NIL
*
* Writes an XML document to a string.
*/

HB_FUNC( HB_XMLTOSTRING )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   PHB_ITEM pStyle = hb_param(2, HB_IT_NUMERIC );
   MXML_DOCUMENT *doc;
   MXML_SGS *sgs;
   MXML_OUTPUT out;
   int iStyle;

   if( pDoc == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pDoc );
      return;
   }

   if ( pStyle == NULL )
   {
      iStyle = 0;
   }
   else
   {
      iStyle = hb_itemGetNI( pStyle );
   }

   doc = hb_itemGetPtr( pDoc );
   sgs = mxml_sgs_new();
   mxml_output_setup( &out, mxml_output_func_to_sgs , doc->node_count );
   out.data = sgs;

   if( mxml_node_write( &out, doc->root, iStyle ) == MXML_STATUS_OK )
   {
      hb_retclenAdopt( sgs->buffer, sgs->length );
      sgs->buffer = NULL; // unlinks the buffer
   }
   else
   {
      hb_ret();
   }

   /* discard the carrier */
   mxml_sgs_destroy( sgs );
}

/**
* HB_XmlWrite( xmlDocument, nFileHandle, nStyle ) --> nStatus
*
* Writes an XML document to a file; returns the HB_XML status.
*/

HB_FUNC( HB_XMLWRITE )
{
   PHB_ITEM pDoc = hb_param(1, HB_IT_POINTER );
   PHB_ITEM pHandle = hb_param( 2, HB_IT_INTEGER );
   PHB_ITEM pStyle = hb_param(3, HB_IT_NUMERIC );
   MXML_DOCUMENT *doc;
   MXML_OUTPUT out;
   int iStyle, iRet;

   if( pDoc == NULL || pHandle == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         2, pDoc, pHandle );
      return;
   }

   if ( pStyle == NULL )
   {
      iStyle = 0;
   }
   else
   {
      iStyle = hb_itemGetNI( pStyle );
   }

   doc = hb_itemGetPtr( pDoc );
   mxml_output_setup( &out, mxml_output_func_to_handle , doc->node_count );
   out.data = (void *) hb_itemGetNI( pHandle );

   iRet = mxml_node_write( &out, doc->root, iStyle );
   hb_retni( iRet );

}

/*
   End of HBXML
*/
