/*
 * $Id: hbxml.c,v 1.4 2003/06/23 20:31:39 jonnymind Exp $
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


#include <stdio.h>
#include <ctype.h>

#include "hbapi.h"
#include "hb_io.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#include "hbxml.h"

// just a shortcut
static void hbxml_set_doc_status( PHB_ITEM doc, int status, int error )
{
   HB_ITEM number;

   number.type = HB_IT_NIL;
   hb_itemPutNI( &number, status );
   hb_objSendMsg( doc, "_NSTATUS", 1 , &number );
   hb_itemPutNI( &number, error );
   hb_objSendMsg( doc, "_NERROR", 1 , &number );
}
   
static void hbxml_doc_new_line( PHB_ITEM pDoc )
{
   HB_ITEM number;
   
   number.type = HB_IT_NIL;
   hb_objSendMsg( pDoc, "NLINE", 0 );
   hb_itemPutNI( &number, HB_VM_STACK.Return.item.asInteger.value + 1);
   hb_objSendMsg( pDoc, "_NLINE", 1 , &number );
}  

static void hbxml_doc_new_node( PHB_ITEM pDoc, int amount )
{
   HB_ITEM number;

   number.type = HB_IT_NIL;
   hb_objSendMsg( pDoc, "NNODECOUNT", 0 );
   hb_itemPutNI( &number, HB_VM_STACK.Return.item.asInteger.value + amount);
   hb_objSendMsg( pDoc, "_NNODECOUNT", 1 , &number );
   
   //TODO: launch a callback
}  
 
/***********************************************************
   HBXML lib
   Attribute oriented routines
************************************************************/


PHB_ITEM mxml_attribute_read( MXML_REFIL *ref, PHB_ITEM pDoc )
{
   PHB_ITEM pRet;
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
               case MXML_LINE_TERMINATOR: hbxml_doc_new_line( pDoc ); break;
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
                     hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_INVATT );
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
               hbxml_doc_new_line( pDoc );
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
               hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
               return NULL;
            }
         break;

         // waiting for '='
         case 2:
            if ( chr == '=' ) {
               iStatus = 3;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               hbxml_doc_new_line( pDoc );
            }
            // We repeat line terminator here for portability
            else if ( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' ) {
            }
            else {
               hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
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
               hbxml_doc_new_line( pDoc );
            }
            // We repeat line terminator here for portability
            else if ( chr == ' ' || chr == '\t' || chr == '\n' || chr == '\r' ) {
            }
            else {
               hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_MALFATT );
               return NULL;
            }
         break;

         // scanning the attribute content ( until next quotechr )
         case 4:
            if ( chr == quotechr ) {
               iStatus = 5;
            }
            else if( chr == MXML_LINE_TERMINATOR ) {
               hbxml_doc_new_line( pDoc );
               buf_attrib[ iPosa++ ] = chr;
            }
            // We repeat line terminator here for portability
            else {
               buf_attrib[ iPosa++ ] = chr;
            }
         break;

      }

   }

   if ( ref->status != MXML_STATUS_OK )
      return NULL;

   if ( iStatus < 5 ) {
      if ( iPosn > MXML_MAX_NAME_LEN )
         hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_ATTRIBTOOLONG);
      else if ( iPosa > MXML_MAX_ATTRIB_LEN )
         hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED, MXML_ERROR_VALATTOOLONG);
      else
         hbxml_set_doc_status( pDoc, MXML_STATUS_MALFORMED,MXML_ERROR_MALFATT);

      return NULL;
   }

   // time to create the attribute
   buf_name[ iPosn ] = 0;
   buf_attrib[ iPosa ] = 0;

   pRet = hb_itemNew( NULL );
   hb_arrayNew( pRet, 2 );
   hb_itemPutCL( hb_arrayGetItemPtr( pRet, 1 ), buf_name, iPosn );
   hb_itemPutCL( hb_arrayGetItemPtr( pRet, 2 ), buf_attrib, iPosa );
   
   return pRet;
}

MXML_STATUS mxml_attribute_write( MXML_OUTPUT *out, PHB_ITEM pAttr )
{
   char quote = '"';
   char *p = hb_arrayGetCPtr( pAttr, 1 );
   char *name = p;

   // possibly detects the need for quote = '\''
   while ( *p ) {
      if ( *p == '"') {
         quote = '\'';
         break;
      }
      p++;
   }

   mxml_output_string_len( out, name, hb_arrayGetCLen( pAttr, 1 ) );
   mxml_output_char( out, '=' );
   mxml_output_char( out, quote );
   mxml_output_string_len(  out,
         hb_arrayGetCPtr( pAttr, 2 ), hb_arrayGetCLen( pAttr, 2 ) );
   mxml_output_char( out, quote );

   return out->status;
}



/***********************************************************
   HBXML lib
   Item (node) routines
***********************************************************/


PHB_ITEM mxml_node_new()
{
   PHB_ITEM pNode;
   PHB_DYNS pExecSym;
   
   pNode = hb_itemNew( NULL );

   pExecSym = hb_dynsymFindName( "TXMLNODE" );
   hb_vmPushSymbol( pExecSym->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   /* The node is in the return */
   hb_objSendMsg( &(HB_VM_STACK.Return), "NEW", 0 );
   hb_itemCopy( pNode, &(HB_VM_STACK.Return) );

   return pNode;
}


/**
* The unlink function is used to detach a node from the UPPER and PARENT hyerarcy.
* The node is "removed" so that siblings node are "squished", and possible parents
* are informed of the changes, so that they are able to get a new child to start
* the tree structure under them. The childs of the unlinked nodes are NOT unlinked,
* thus remains attached to the node: is like removing a branch with all its leaves.
*
*/

void mxml_node_unlink( PHB_ITEM pNode )
{
   HB_ITEM pPrev, pNext, pParent, pNil;

   pPrev.type = HB_IT_NIL;
   pNext.type = HB_IT_NIL;
   pParent.type = HB_IT_NIL;
   // We'll need NIL somewhere
   pNil.type = HB_IT_NIL;

   hb_objSendMsg( pNode, "OPREV", 0 );
   hb_itemCopy( &pPrev, &(HB_VM_STACK.Return) );

   hb_objSendMsg( pNode, "ONEXT", 0 );
   hb_itemCopy( &pNext, &(HB_VM_STACK.Return) );

   hb_objSendMsg( pNode, "OPARENT", 0 );
   hb_itemCopy( &pParent, &(HB_VM_STACK.Return) );

   // Detaching from previous
   if ( pPrev.type != HB_IT_NIL )
   {
      hb_objSendMsg( &pPrev, "_ONEXT", 1, &pNext);
      hb_objSendMsg( pNode, "_ONEXT", 1, &pNil);
   }

   // Detaching from Next
   if ( pNext.type != HB_IT_NIL )
   {
      hb_objSendMsg( &pNext, "_OPREV", 1, &pPrev);
      hb_objSendMsg( pNode, "_OPREV", 1, &pNil);
   }

   // Detaching from parent
   if ( pParent.type != HB_IT_NIL )
   {
      // Eventually set the next node as first child
      if ( pPrev.type == HB_IT_NIL ) //was I the first node?
      {
         hb_objSendMsg( &pParent , "_OCHILD", 1, &pNext );
      }
      hb_objSendMsg( pNode, "_OPARENT", 1, &pNil);
   }
}

HB_FUNC( HBXML_NODE_UNLINK )
{
   mxml_node_unlink( hb_param( 1, HB_IT_OBJECT ) );
}
/****************************************************************/

void mxml_node_insert_before( PHB_ITEM pTg, PHB_ITEM pNode )
{
   HB_ITEM parent;

   parent.type = HB_IT_NIL;

   // Move tg->prev into node->prev
   hb_objSendMsg( pTg, "OPREV", 0 );
   hb_objSendMsg( pNode, "_OPREV", 1, &(HB_VM_STACK.Return) );

   // tg->prev is now pnode!
   hb_objSendMsg( pTg, "_OPREV", 1, pNode );

   // pNode->next is TG
   hb_objSendMsg( pNode, "_ONEXT", 1, pTg );

   // pNode->parent is the same as tg
   hb_objSendMsg( pTg, "OPARENT", 0 );
   hb_itemCopy( &parent, &(HB_VM_STACK.Return) );
   hb_objSendMsg( pNode, "_OPARENT", 1, &parent );

   // if the parent is not null, and if it's child was tg, we must update to node
   if ( parent.type != HB_IT_NIL )
   {
      hb_objSendMsg( &parent, "OCHILD", 0 );
      if( HB_VM_STACK.Return.item.asArray.value == pTg->item.asArray.value )
      {
         hb_objSendMsg( &parent, "_OCHILD", 1, pNode );
      }
   }
}

HB_FUNC( HBXML_NODE_INSERT_BEFORE )
{
   mxml_node_insert_before( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}


void mxml_node_insert_after( PHB_ITEM pTg, PHB_ITEM pNode )
{
   // Move tg->next into node->next
   hb_objSendMsg( pTg, "ONEXT", 0 );
   hb_objSendMsg( pNode, "_ONEXT", 1, &(HB_VM_STACK.Return) );

   // tg->NEXT is now pnode!
   hb_objSendMsg( pTg, "_ONEXT", 1, pNode );

   // pNode->prev is TG
   hb_objSendMsg( pNode, "_OPREV", 1, pTg );

   // pNode->parent is the same as tg
   hb_objSendMsg( pTg, "OPARENT", 0 );
   hb_objSendMsg( pNode, "_OPARENT", 1, &(HB_VM_STACK.Return) );
}

HB_FUNC( HBXML_NODE_INSERT_AFTER )
{
   mxml_node_insert_after( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}

/**
* Creates a new tree level, so that the given node is added between
* tg and its former children. Former children of pNode are discarded
*/

void mxml_node_insert_below( PHB_ITEM pTg, PHB_ITEM pNode )
{
   HB_ITEM child;

   child.type = HB_IT_NIL;

   // Move tg->child into node->child
   hb_objSendMsg( pTg, "OCHILD", 0 );
   hb_itemCopy( &child, &(HB_VM_STACK.Return) );
   hb_objSendMsg( pNode, "_OCHILD", 1, &child );

   // Parent of pNode is now TG
   hb_objSendMsg( pNode, "_OPARENT", 1, pTg );

   // All children parents are moved to pNode
   while ( child.type != HB_IT_NIL ) {
      hb_objSendMsg( &child, "_OPARENT", 1, pNode );
      hb_objSendMsg( &child, "ONEXT", 0);
      hb_itemCopy( &child, &(HB_VM_STACK.Return) );
   }
}


HB_FUNC( HBXML_NODE_INSERT_BELOW )
{
   mxml_node_insert_below( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}
/**
* Adds a node to the bottom of the children list of tg.
*/

void mxml_node_add_below( PHB_ITEM pTg, PHB_ITEM pNode )
{
   HB_ITEM child;

   child.type = HB_IT_NIL;

   // Parent of pNode is now TG
   hb_objSendMsg( pNode, "_OPARENT", 1, pTg );

   // Get the TG child
   hb_objSendMsg( pTg, "OCHILD", 0 );
   hb_itemCopy( &child, &(HB_VM_STACK.Return) );

   if ( child.type != HB_IT_NIL )
   {
      // Scanning up to the last child
      while ( 1 ) {
         hb_objSendMsg( &child, "ONEXT", 0);
         if(HB_VM_STACK.Return.type == HB_IT_NIL )
         {
            break;
         }
         hb_itemCopy( &child, &(HB_VM_STACK.Return) );
      }
      // linking the child with pnode
      hb_objSendMsg( &child, "_ONEXT", 1, pNode );
      hb_objSendMsg( pNode, "_OPREV", 1, &child );
   }
   else
   {
      // if we are the first child, we notify pTg of this
      hb_objSendMsg( pTg, "_OCHILD", 1, pNode );
   }
}

HB_FUNC( HBXML_NODE_ADD_BELOW )
{
   mxml_node_add_below( hb_param( 1, HB_IT_OBJECT ), hb_param( 2, HB_IT_OBJECT ) );
}
/**
* Clones a node, but it does not sets the parent, nor the siblings;
* this clone is "floating" out of the tree hierarcy.
*/

PHB_ITEM mxml_node_clone( PHB_ITEM pTg )
{
   PHB_ITEM pNode = mxml_node_new();
   PHB_ITEM pArrayClone;

   //sets clone type
   hb_objSendMsg( pTg, "NTYPE", 0 );
   hb_objSendMsg( pNode, "_NTYPE", 1, &(HB_VM_STACK.Return) );

   //sets clone name
   hb_objSendMsg( pTg, "CNAME", 0 );
   hb_objSendMsg( pNode, "_CNAME", 1, &(HB_VM_STACK.Return) );

   //sets clone data
   hb_objSendMsg( pTg, "CDATA", 0 );
   hb_objSendMsg( pNode, "_CDATA", 1, &(HB_VM_STACK.Return) );  
   
   // clone attributes
   hb_objSendMsg( pTg, "AATTRIBUTES", 0 );
   pArrayClone = hb_arrayClone( &(HB_VM_STACK.Return), NULL );
   hb_objSendMsg( pNode, "_AATTRIBUTES", 1, pArrayClone );
   
   return pNode;
}

HB_FUNC( HBXML_NODE_CLONE )
{
  mxml_node_clone( hb_param( 1, HB_IT_OBJECT ) );
}

/**
* Clones a node and all its subtree, but it does not sets the parent, nor the siblings;
* this clone is "floating" out of the tree hierarcy.
*/

PHB_ITEM mxml_node_clone_tree( PHB_ITEM pTg )
{
   PHB_ITEM pClone = mxml_node_clone( pTg );
   HB_ITEM node;

   node.type = HB_IT_NIL;
   
   // Get the TG child
   hb_objSendMsg( pTg, "OCHILD", 0 );
   hb_itemCopy( &node, &(HB_VM_STACK.Return) );  

   while ( node.type != HB_IT_NIL ) {
      PHB_ITEM pSubTree;

      pSubTree = mxml_node_clone_tree( &node );
      // the subtree is the child of the clone
      hb_objSendMsg( pClone, "_OCHILD", 1, pSubTree );

      // the parent of the subtree is the clone
      hb_objSendMsg( pSubTree, "_OPARENT", 1, pClone );
      
      // go to the next node            
      hb_objSendMsg( &node, "ONEXT", 0 );
      hb_itemCopy( &node, &(HB_VM_STACK.Return) );  
   }

   return pClone;
}

HB_FUNC( HBXML_NODE_CLONE_TREE )
{
  mxml_node_clone_tree( hb_param( 1, HB_IT_OBJECT ) );
}

/* reads a data node */
static void mxml_node_read_data( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   char *buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int iAllocated = MXML_ALLOC_BLOCK;
   int iPos = 0;
   int chr;
   HB_ITEM hbtemp;

   chr = mxml_refil_getc( ref );
   while ( chr != MXML_EOF ) {
      if ( chr != '<' ) {
         buf[ iPos++ ] = chr;
         if ( iPos >= iAllocated ) {
            iAllocated += MXML_ALLOC_BLOCK;
            buf = (char *) MXML_REALLOCATOR( buf, iAllocated );
         }

         if ( chr == MXML_LINE_TERMINATOR )
         {
            hbxml_doc_new_line( doc );
         }
      }
      else {
         mxml_refil_ungetc( ref, chr );
         break;
      }

      chr = mxml_refil_getc( ref );
   }

   if ( ref->status != MXML_STATUS_OK )
   {
      hbxml_set_doc_status( doc, ref->status, ref->error );
      return;
   }

   // trimming unneded spaces
   while ( iPos >1 && isspace( buf[iPos-1] ) )
      iPos--;
   buf[ iPos ] = 0;

   hbtemp.type = HB_IT_NIL;
   hb_itemPutNI( &hbtemp, MXML_TYPE_DATA );
   hb_objSendMsg( pNode, "_NTYPE", 1, &hbtemp );

   hb_itemPutCPtr( &hbtemp, buf, iPos );
   hb_objSendMsg( pNode,"_CDATA", 1, &hbtemp );
}

static MXML_STATUS mxml_node_read_name( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   HB_ITEM hbtemp;
   char buf[ MXML_MAX_NAME_LEN + 1];
   int iPos = 0;
   int chr;
   int iStatus = 0;

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
               hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
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
               hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
               return MXML_STATUS_MALFORMED;
            }
         break;
      }
  }

   if ( ref->status != MXML_STATUS_OK ) {
      hbxml_set_doc_status( doc, ref->status, ref->error );
      return ref->status;
   }

   if ( iStatus != 2  ) {
      hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_NAMETOOLONG );
      return MXML_STATUS_MALFORMED;
   }

   hbtemp.type = HB_IT_NIL;
   buf[ iPos ] = 0;
   hb_itemPutCL( &hbtemp, buf, iPos );
   hb_objSendMsg( pNode,"_CNAME", 1, &hbtemp );

   return MXML_STATUS_OK;
}

static MXML_STATUS mxml_node_read_attributes( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{

   HB_ITEM attributes;
   PHB_ITEM pAttr;

   attributes.type = HB_IT_NIL;
   hb_arrayNew( &attributes, 0 );

   pAttr = mxml_attribute_read( ref, doc );
   while ( pAttr != NULL ) {
      hb_arrayAddForward( &attributes, pAttr );
      pAttr = mxml_attribute_read( ref, doc );
   }

   hb_objSendMsg( pNode,"_AATTRIBUTES", 1, &attributes );
   if ( ref->status != MXML_STATUS_OK )
   {
      hbxml_set_doc_status( doc, ref->status, ref->error );
   }

   return ref->status;
}

static void mxml_node_read_directive( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   char *buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   int iAllocated = MXML_ALLOC_BLOCK;
   int iPos = 0;
   int chr;

   HB_ITEM hbtemp;

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
         {
            hbxml_doc_new_line( doc );
         }

         chr = mxml_refil_getc( ref );
      }

      if ( ref->status == MXML_STATUS_OK ) {
         buf[ iPos ] = 0;
         hbtemp.type = HB_IT_NIL;
         hb_itemPutNI( &hbtemp, MXML_TYPE_DIRECTIVE );
         hb_objSendMsg( pNode,"_NTYPE", 1, &hbtemp );
         hb_itemPutCPtr( &hbtemp, buf, iPos );
         hb_objSendMsg( pNode,"_CDATA", 1, &hbtemp );
      }
      else {
         hbxml_set_doc_status( doc, ref->status, ref->error );
      }

   }
}

static void mxml_node_read_pi( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   int iPos = 0, iAllocated;
   int chr;
   char *buf;
   int iStatus = 0;
   HB_ITEM hbtemp;

   // let's read the xml PI instruction
   if ( mxml_node_read_name( ref, pNode, doc ) != MXML_STATUS_OK )
      return;

   // and then we'll put all the "data" into the data member, up to ?>

   buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK ;

   while ( iStatus < 2 ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         // scanning for ?>
         case 0:
            if ( chr == MXML_LINE_TERMINATOR )
            {
               hbxml_doc_new_line( doc );
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
      hbtemp.type = HB_IT_NIL;
      hb_itemPutNI( &hbtemp, MXML_TYPE_PI );
      hb_objSendMsg( pNode,"_NTYPE", 1, &hbtemp );
      hbtemp.type = HB_IT_NIL;
      hb_itemPutCPtr( &hbtemp, buf, iPos );
      hb_objSendMsg( pNode,"_CDATA", 1, &hbtemp );
   }
   else {
      hbxml_set_doc_status( doc, ref->status, ref->error );
   }
}

static void mxml_node_read_tag( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   char chr;
   HB_ITEM hbtemp;
   
   hbtemp.type = HB_IT_NIL;
   hb_itemPutNI( &hbtemp, MXML_TYPE_TAG );
   hb_objSendMsg( pNode,"_NTYPE", 1, &hbtemp );
      
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
      hbxml_set_doc_status( doc, ref->status, ref->error );   
   }

   //else the node is complete
}

static void mxml_node_read_comment( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
{
   int iPos = 0, iAllocated;
   int chr;
   char *buf;
   int iStatus = 0;
   HB_ITEM hbtemp;

   hbtemp.type = HB_IT_NIL;
   hb_itemPutNI( &hbtemp, MXML_TYPE_COMMENT );
   hb_objSendMsg( pNode,"_NTYPE", 1, &hbtemp );
   
   //  we'll put all the comment into the data member, up to ->

   buf = (char *) MXML_ALLOCATOR( MXML_ALLOC_BLOCK );
   iAllocated = MXML_ALLOC_BLOCK ;

   while ( iStatus < 3 ) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      switch ( iStatus ) {
         // scanning for ->
         case 0:
            if ( chr == MXML_LINE_TERMINATOR ) 
            {
               hbxml_doc_new_line( doc );
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
      hb_itemPutCPtr( &hbtemp, buf, iPos );
      hb_objSendMsg( pNode,"_CDATA", 1, &hbtemp );
   }
   else {
      hbxml_set_doc_status( doc, ref->status, ref->error );
   }
}

// checking closing tag
static void mxml_node_read_closing( MXML_REFIL *ref, PHB_ITEM pNode, PHB_ITEM doc )
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
      hbxml_set_doc_status( doc, ref->status, ref->error );
      return;
   }

   if ( iPos > MXML_MAX_NAME_LEN ) {
      hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_NAMETOOLONG );
   }

   buf[ iPos ] = 0;

   hb_objSendMsg( pNode,"CNAME", 0 );
   if ( chr != '>' || strcmp( HB_VM_STACK.Return.item.asString.value, buf ) != 0 )
   {
      hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_UNCLOSED );
   }
   // all fine
}

MXML_STATUS mxml_node_read( MXML_REFIL *ref, PHB_ITEM pNode,PHB_ITEM doc )
{
   PHB_ITEM node;
   HB_ITEM child_node, data_node;

   int chr;
   /* Stateful machine status */
   int iStatus = 0;

   while ( iStatus >= 0) {
      chr = mxml_refil_getc( ref );
      if ( chr == MXML_EOF ) break;

      // resetting new node foundings
      node = NULL;

      switch ( iStatus ) {

         case 0:  // outside nodes
            switch ( chr ) {
               case MXML_LINE_TERMINATOR: hbxml_doc_new_line( doc ); break;
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
               hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
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
               hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
            }
         break;

         case 3:
            if ( chr == '-') {
               node = mxml_node_new();
               mxml_node_read_comment( ref, node, doc );
            }
            else {
               hbxml_set_doc_status( doc, MXML_STATUS_MALFORMED, MXML_ERROR_INVNODE );
            }
         break;
      }

      // have I to add a node below our structure ?
      if ( node != NULL ) {
         if ( ref->status == MXML_STATUS_OK ) {
            mxml_node_add_below( pNode, node );
            // beginning again - a new node is born
            hbxml_doc_new_node( doc, 1 );
            iStatus = 0;
         }
         else {
            hbxml_set_doc_status( doc, ref->status, ref->error );
            // node will be destroyed by GC when needed
            return ref->status;
         }
      }

   }


   // if we have an hard error on stream
   if ( ref->status != MXML_STATUS_OK ) {
      hbxml_set_doc_status( doc, ref->status, ref->error );
      return ref->status;
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
         hbxml_set_doc_status( doc, ref->status, ref->error );
         return ref->status;
      }

      //checking for data nodes
      child_node.type = HB_IT_NIL;
      data_node.type = HB_IT_NIL;
      hb_objSendMsg( pNode, "OCHILD", 0 );
      hb_itemCopy( &child_node, &( HB_VM_STACK.Return ) );

      while ( child_node.type != HB_IT_NIL ) {
         hb_objSendMsg( &child_node, "NTYPE", 0 );
         if ( HB_VM_STACK.Return.item.asInteger.value == MXML_TYPE_DATA )
         {
            // first data node ?
            if ( data_node.type == HB_IT_NIL )
            {
               hb_itemCopy( &data_node, &child_node);
            }
            // ... or have we more than a data node?
            else {
               data_node.type = HB_IT_NIL;
               break;
            }
         }
         hb_objSendMsg( &child_node, "ONEXT", 0 );
         hb_itemCopy( &child_node, &( HB_VM_STACK.Return ) );
      }

      if ( data_node.type != HB_IT_NIL ) {
         hb_objSendMsg( &data_node, "CDATA", 0 );
         hb_objSendMsg( pNode, "_CDATA", 1, &( HB_VM_STACK.Return ) );

         mxml_node_unlink( &data_node );
         //garbage will take care of destroying it
         hbxml_doc_new_node( doc, -1 );
      }

   }

   return MXML_STATUS_OK;
}

void mxml_node_write_attributes( MXML_OUTPUT *out, PHB_ITEM pAttr )
{
   ULONG iLen = hb_arrayLen( pAttr );
   ULONG i;

   for ( i = 1; i <= iLen; i ++ )
   {
      mxml_output_char( out, ' ' );
      mxml_attribute_write( out, hb_arrayGetItemPtr( pAttr, i ) );
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


MXML_STATUS mxml_node_write( MXML_OUTPUT *out, PHB_ITEM pNode, int style )
{
   HB_ITEM child;
   HB_ITEM hbtemp;
   int depth = 0;
   int mustIndent = 0;

   hbtemp.type = HB_IT_NIL;
   child.type = HB_IT_NIL;

   if ( style & MXML_STYLE_INDENT ) {
      hb_objSendMsg( pNode, "DEPTH", 0 );
      depth = HB_VM_STACK.Return.item.asInteger.value - 1;
      mxml_node_file_indent( out, depth, style );
   }

   hb_objSendMsg( pNode, "NTYPE", 0 );
   switch( HB_VM_STACK.Return.item.asInteger.value ) {
      case MXML_TYPE_TAG:

         mxml_output_char( out, '<' );
         hb_objSendMsg( pNode,"CNAME", 0 );
         mxml_output_string_len( out,
            HB_VM_STACK.Return.item.asString.value,
            HB_VM_STACK.Return.item.asString.length );

         hb_objSendMsg( pNode, "AATTRIBUTES", 0 );
         mxml_node_write_attributes( out, &(HB_VM_STACK.Return) );

         hb_objSendMsg( pNode, "CDATA", 0 );
         hb_itemCopy( &hbtemp, &(HB_VM_STACK.Return) );
         hb_objSendMsg( pNode, "OCHILD", 0 );
         hb_itemCopy( &child, &(HB_VM_STACK.Return) );

         if ( hbtemp.type == HB_IT_NIL && child.type == HB_IT_NIL )
         {
            mxml_output_string_len( out, " />\n", 4 );
         }
         else
         {
            mxml_output_char( out, '>' );

            if ( child.type != HB_IT_NIL ) {
               mustIndent = 1;
               mxml_output_char( out, '\n' );

               while ( child.type != HB_IT_NIL )
               {
                  mxml_node_write( out, &child, style );
                  hb_objSendMsg( &child, "ONEXT", 0 );
                  hb_itemCopy( &child, &(HB_VM_STACK.Return) );
               }
            }

            if ( hbtemp.type != HB_IT_NIL )
            {
              if ( mustIndent && ( style & MXML_STYLE_INDENT ) )
              {
                  mxml_node_file_indent( out, depth+1, style );
              }
              mxml_output_string_len( out,
                  hbtemp.item.asString.value,
                  hbtemp.item.asString.length );
            }

            if ( mustIndent && ( style & MXML_STYLE_INDENT ))
               mxml_node_file_indent( out, depth, style );


            mxml_output_string_len( out, "</", 2 );
            hb_objSendMsg( pNode, "CNAME", 0 );
            mxml_output_string_len( out,
               HB_VM_STACK.Return.item.asString.value,
               HB_VM_STACK.Return.item.asString.length );
            mxml_output_string_len( out, ">\n", 2 );
         }
      break;

      case MXML_TYPE_COMMENT:
            mxml_output_string_len( out, "<!--", 4 );
            hb_objSendMsg( pNode, "CDATA", 0 );
            mxml_output_string_len( out,
               HB_VM_STACK.Return.item.asString.value,
               HB_VM_STACK.Return.item.asString.length );
            mxml_output_string_len( out, "-->\n", 4 );
      break;

      case MXML_TYPE_DATA:
         hb_objSendMsg( pNode, "CDATA", 0 );
         mxml_output_string_len( out,
            HB_VM_STACK.Return.item.asString.value,
            HB_VM_STACK.Return.item.asString.length );
         mxml_output_char( out, '\n' );
      break;

      case MXML_TYPE_DIRECTIVE:
         mxml_output_string_len( out, "<!", 2 );
         hb_objSendMsg( pNode, "CNAME", 0 );
         mxml_output_string_len( out,
            HB_VM_STACK.Return.item.asString.value,
            HB_VM_STACK.Return.item.asString.length );

         hb_objSendMsg( pNode, "CDATA", 0 );

         if ( HB_VM_STACK.Return.type != HB_IT_NIL ) {
            mxml_output_string_len( out,
               HB_VM_STACK.Return.item.asString.value,
               HB_VM_STACK.Return.item.asString.length );
         }
         mxml_output_string_len( out, ">\n", 2 );
      break;

      case MXML_TYPE_PI:
         mxml_output_string_len( out, "<?", 2 );
         hb_objSendMsg( pNode, "CNAME", 0 );
         mxml_output_string_len( out,
            HB_VM_STACK.Return.item.asString.value,
            HB_VM_STACK.Return.item.asString.length );

         hb_objSendMsg( pNode, "CDATA", 0 );

         if ( HB_VM_STACK.Return.type != HB_IT_NIL ) {
            mxml_output_string_len( out,
               HB_VM_STACK.Return.item.asString.value,
               HB_VM_STACK.Return.item.asString.length );
         }
         mxml_output_string_len( out, "?>\n", 3 );
      break;

      case MXML_TYPE_DOCUMENT:
         hb_objSendMsg( pNode, "OCHILD", 0 );
         hb_itemCopy( &child, &(HB_VM_STACK.Return) );

         while ( child.type != HB_IT_NIL )
         {
            mxml_node_write( out, &child, style );
            hb_objSendMsg( &child, "ONEXT", 0 );
            hb_itemCopy( &child, &(HB_VM_STACK.Return) );
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
   FHANDLE fh = (FHANDLE) out->data;
   int olen;
   olen = hb_fsWrite( fh, (BYTE *) s, len );

   if ( olen < len )
   {
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
   ref->buffer = (BYTE*) buf;

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
   FHANDLE fh = (FHANDLE) ref->data;
   int len;

   len = hb_fsRead( fh, (BYTE *) ref->buffer, ref->bufsize );

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
   // code --;
   code = (MXML_ERROR_CODE) ( code - 1 );
   if ( code < 0 || code > sizeof( edesc ) / sizeof( char * ) )
      return NULL;

   return edesc[ code ];
}




/***********************************************************
   HBXML lib
   xHarbour RTL & VM interface
***********************************************************/


/**
* HB_XmlCreate( [xData] ) --> xmlDocument
* xData can be a file handle from which an XML can be read,
* a string containing an XML tree or NIL, in which case the
* document is created empty.
*/

HB_FUNC( HBXML_DATAREAD )
{
   PHB_ITEM pParam = hb_param(2, HB_IT_ANY );
   PHB_ITEM pDoc = hb_param(1, HB_IT_OBJECT );
   HB_ITEM root;
   MXML_REFIL refil;
   char buffer[512];


   if( pDoc == NULL || pParam == NULL ||
      ( !HB_IS_STRING( pParam ) && !HB_IS_NUMERIC( pParam)) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter count/type",
         NULL,
         2, pDoc, pParam);
      return;
   }


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

   /* Now we can get the root node */
   hb_objSendMsg( pDoc, "OROOT", 0 );
   root.type = HB_IT_NIL;
   hb_itemCopy( &root, &(HB_VM_STACK.Return) );
   hb_retni( mxml_node_read( &refil, &root, pDoc ));

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


   hb_retc( mxml_error_desc( (MXML_ERROR_CODE) hb_itemGetNI( pNum ) ) );
}



/**
* HB_XmlToString( xmlDocument [, nStyle] ) --> cXml | NIL
*
* Writes an XML document to a string.
*/

HB_FUNC( HBXML_NODE_TO_STRING )
{
   PHB_ITEM pNode = hb_param(1, HB_IT_OBJECT );
   PHB_ITEM pStyle = hb_param(2, HB_IT_NUMERIC );
   MXML_SGS *sgs;
   MXML_OUTPUT out;
   int iStyle;

   if( pNode == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         1, pNode );
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


   sgs = mxml_sgs_new();
   mxml_output_setup( &out, mxml_output_func_to_sgs , 0 );
   out.data = sgs;

   if( mxml_node_write( &out, pNode, iStyle ) == MXML_STATUS_OK )
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

HB_FUNC( HBXML_NODE_WRITE )
{
   PHB_ITEM pNode = hb_param(1, HB_IT_OBJECT );
   PHB_ITEM pHandle = hb_param( 2, HB_IT_INTEGER );
   PHB_ITEM pStyle = hb_param(3, HB_IT_NUMERIC );
   MXML_OUTPUT out;
   int iStyle, iRet;

   if( pNode == NULL || pHandle == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter type",
         NULL,
         2, pNode, pHandle );
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

   mxml_output_setup( &out, mxml_output_func_to_handle , 0 );
   out.data = (void *) hb_itemGetNI( pHandle );

   iRet = mxml_node_write( &out, pNode, iStyle );
   hb_retni( iRet );

}

/*
   End of HBXML
*/
