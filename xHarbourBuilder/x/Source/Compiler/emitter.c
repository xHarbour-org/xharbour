//
//  emiter.c
//  x
//
//  Created by Ron Pinkas on 7/19/13.
//  Copyright (c) 2013 http://www.xHarbour.com. All rights reserved.
//

#include <stdio.h>
#include "common.h"

/*
BOXEDVALUE Global1  = { .Type = PRG_TYPE_NIL,      .AsBytes = {0} };
BOXEDVALUE Global2  = { .Type = PRG_TYPE_LONG,     .Value.lLong = 1001 };
BOXEDVALUE Global3  = { .Type = PRG_TYPE_DATE,     .Value.lDate = 1002 };
BOXEDVALUE Global4  = { .Type = PRG_TYPE_DATETIME, .Value.dDateTime = 1003.00 };
BOXEDVALUE Global5  = { .Type = PRG_TYPE_STRING,   .Value.sString = "" };
BOXEDVALUE Global6  = { .Type = PRG_TYPE_LOGICAL,  .Value.bLogical = 0 };
BOXEDVALUE Global7  = { .Type = PRG_TYPE_DECIMAL,  .Value.Decimal.dDouble = 1004.00, .Value.Decimal.cWidth = 16, .Value.Decimal.cDec = 2 };
BLOCK GlobalBlock  = { .iArguments = 0, .pFunction = (void *) InitFunction, .iDetached = 1, NULL };
BOXEDVALUE Global8  = { .Type = PRG_TYPE_BLOCK,    .Value.pBlock = &GlobalBlock };
BOXEDVALUE Global9  = { .Type = PRG_TYPE_ARRAY,    .Value.pArray = NULL };
BOXEDVALUE Global10 = { .Type = PRG_TYPE_POINTER,  .Value.pPointer = NULL };
BOXEDVALUE Global11 = { .Type = PRG_TYPE_BYREF,    .Value.pByRef = NULL };

@.str = private unnamed_addr constant [6 x i8] c"Init\0A\00", align 1
@Global1 = global { i32, [4 x i8], { [16 x i8] } } { i32 1, [4 x i8] undef, { [16 x i8] } zeroinitializer }, align 8
@Global2 = global { i32, { { i64, [8 x i8] } } } { i32 9, { { i64, [8 x i8] } } { { i64, [8 x i8] } { i64 1001, [8 x i8] undef } } }, align 8
@Global3 = global { i32, { { i64, [8 x i8] } } } { i32 32, { { i64, [8 x i8] } } { { i64, [8 x i8] } { i64 1002, [8 x i8] undef } } }, align 8
@Global4 = global { i32, { { double, [8 x i8] } } } { i32 33, { { double, [8 x i8] } } { { double, [8 x i8] } { double 1.003000e+03, [8 x i8] undef } } }, align 8
@.str1 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@Global5 = global { i32, { { i8*, [8 x i8] } } } { i32 2, { { i8*, [8 x i8] } } { { i8*, [8 x i8] } { i8* getelementptr inbounds ([1 x i8]* @.str1, i32 0, i32 0), [8 x i8] undef } } }, align 8
@Global6 = global { i32, [4 x i8], { { i32, [12 x i8] } } } { i32 4, [4 x i8] undef, { { i32, [12 x i8] } } { { i32, [12 x i8] } { i32 0, [12 x i8] undef } } }, align 8
@Global7 = global { i32, { { { double, i8, i8, [6 x i8] } } } } { i32 10, { { { double, i8, i8, [6 x i8] } } } { { { double, i8, i8, [6 x i8] } } { { double, i8, i8, [6 x i8] } { double 1.004000e+03, i8 16, i8 2, [6 x i8] undef } } } }, align 8
@GlobalBlock = global %struct._BLOCK { i32 0, i8* bitcast (void ()* @InitFunction to i8*), i32 1, %struct._BOXEDVALUE* null }, align 8
@Global8 = global { i32, { { %struct._BLOCK*, [8 x i8] } } } { i32 64, { { %struct._BLOCK*, [8 x i8] } } { { %struct._BLOCK*, [8 x i8] } { %struct._BLOCK* @GlobalBlock, [8 x i8] undef } } }, align 8
@Global9 = global { i32, { { %struct._BOXEDVALUE*, [8 x i8] } } } { i32 256, { { %struct._BOXEDVALUE*, [8 x i8] } } { { %struct._BOXEDVALUE*, [8 x i8] } { %struct._BOXEDVALUE* null, [8 x i8] undef } } }, align 8
@Global10 = global { i32, { { %struct._BOXEDVALUE*, [8 x i8] } } } { i32 512, { { %struct._BOXEDVALUE*, [8 x i8] } } { { %struct._BOXEDVALUE*, [8 x i8] } { %struct._BOXEDVALUE* null, [8 x i8] undef } } }, align 8
@Global11 = global { i32, { { %struct._BOXEDVALUE*, [8 x i8] } } } { i32 1024, { { %struct._BOXEDVALUE*, [8 x i8] } } { { %struct._BOXEDVALUE*, [8 x i8] } { %struct._BOXEDVALUE* null, [8 x i8] undef } } }, align 8
@.str2 = private unnamed_addr constant [12 x i8] c"Hello World\00", align 1
*/

//@main.Local1 = private unnamed_addr constant { i32, { { i8*, [8 x i8] } } } { i32 3, { { i8*, [8 x i8] } } { { i8*, [8 x i8] } { i8* getelementptr inbounds ([12 x i8]* @.str2, i32 0, i32 0), [8 x i8] undef } } }, align 8

//@llvm.global_ctors = appending global [1 x { i32, void ()* }] [{ i32, void ()* } { i32 65535, void ()* @InitFunction }]


char * EmitBoxedNil( PARSER_CONTEXT *Parser_pContext )
{
   char EmitBuffer[ 1024 ];
   int iResult;
   
   iResult = snprintf( EmitBuffer, sizeof( EmitBuffer ), "{ i32, [4 x i8], { [16 x i8] } } { i32 %i, [4 x i8] undef, { [16 x i8] } zeroinitializer }, align 8", PRG_TYPE_NIL );

   assert(iResult > 0 && iResult < sizeof( EmitBuffer ) );
   
   return strndup( EmitBuffer, iResult );
}

char * EmitBoxedLong( PARSER_CONTEXT *Parser_pContext, long lLong )
{
   char EmitBuffer[ 1024 ];
   int iResult;
   
   iResult = snprintf( EmitBuffer, sizeof( EmitBuffer ), "{ i32, { { i64, [8 x i8] } } } { i32 %i, { { i64, [8 x i8] } } { { i64, [8 x i8] } { i64 %li, [8 x i8] undef } } }, align 8", PRG_TYPE_LONG, lLong );

   assert(iResult > 0 && iResult < sizeof( EmitBuffer ) );
   
   return strndup( EmitBuffer, iResult );
}

char * EmitBoxedDate( PARSER_CONTEXT *Parser_pContext, long lDate )
{
   char EmitBuffer[ 1024 ];
   int iResult;
   
   iResult = snprintf( EmitBuffer, sizeof( EmitBuffer ), "{ i32, { { i64, [8 x i8] } } } { i32 %i, { { i64, [8 x i8] } } { { i64, [8 x i8] } { i64 %li, [8 x i8] undef } } }, align 8", PRG_TYPE_DATE, lDate );
   
   assert(iResult > 0 && iResult < sizeof( EmitBuffer ) );
   
   return strndup( EmitBuffer, iResult );
}

char * EmitBoxedDateTime( PARSER_CONTEXT *Parser_pContext, double dDateTime )
{
   char EmitBuffer[ 1024 ];
   int iResult;
   
   iResult = snprintf( EmitBuffer, sizeof( EmitBuffer ), "{ i32, { { double, [8 x i8] } } } { i32 %i, { { double, [8 x i8] } } { { double, [8 x i8] } { double %e, [8 x i8] undef } } }, align 8", PRG_TYPE_DATETIME, dDateTime );
   
   assert(iResult > 0 && iResult < sizeof( EmitBuffer ) );
   
   return strndup( EmitBuffer, iResult );
}

/* Emit DynUnboxLong( pBox )
%1 = getelementptr inbounds %struct._BOXEDVALUE* %pBox, i32 0, i32 1
%2 = bitcast %union.anon* %1 to %union._VALUE*
%3 = bitcast %union._VALUE* %2 to i64*
%4 = load i64* %3, align 8
ret i64 %4
*/

/* Emit DynBoxLong( lLong )
 %pBox = alloca %struct._BOXEDVALUE*, align 8
 %1 = call i8* @malloc(i64 24)
 %2 = bitcast i8* %1 to %struct._BOXEDVALUE*
 store %struct._BOXEDVALUE* %2, %struct._BOXEDVALUE** %pBox, align 8
 %3 = load %struct._BOXEDVALUE** %pBox, align 8
 %4 = getelementptr inbounds %struct._BOXEDVALUE* %3, i32 0, i32 0
 store i32 9, i32* %4, align 4
 %5 = load %struct._BOXEDVALUE** %pBox, align 8
 %6 = getelementptr inbounds %struct._BOXEDVALUE* %5, i32 0, i32 1
 %7 = bitcast %union.anon* %6 to %union._VALUE*
 %8 = bitcast %union._VALUE* %7 to i64*
 store i64 %lLong, i64* %8, align 8
 %9 = load %struct._BOXEDVALUE** %pBox, align 8
 ret %struct._BOXEDVALUE* %9
*/
