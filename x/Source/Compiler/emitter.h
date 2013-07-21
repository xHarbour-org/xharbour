//
//  emiter.h
//  x
//
//  Created by Ron Pinkas on 7/19/13.
//  Copyright (c) 2013 http://www.xHarbour.com. All rights reserved.
//

#ifndef EMITTER_DEFINED
   #define EMITTER_DEFINED

   char * EmitBoxedNil( PARSER_CONTEXT *Parser_pContext );

   char * EmitBoxedLong( PARSER_CONTEXT *Parser_pContext, long lLong );

   char * EmitBoxedDate( PARSER_CONTEXT *Parser_pContext, long lDate );

   char * EmitBoxedDateTime( PARSER_CONTEXT *Parser_pContext, double dDateTime );

#endif
