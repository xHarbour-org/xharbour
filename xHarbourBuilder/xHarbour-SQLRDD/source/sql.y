%pure-parser
%name-prefix "sql_yy"
%expect 1
%parse-param { void * stmt }
%lex-param   { void * stmt }

%{
   /*
   * SQLPARSER
   * SQL YACC Rules and Actions
   * Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
   * All Rights Reserved
   */
   #ifdef HB_THREAD_SUPPORT
   #undef HB_THREAD_SUPPORT
   #endif

   #include "hbsql.h"

   #include <stdlib.h>
   #include <stdio.h>
   #include <string.h>
   #include <ctype.h>

   #include "msg.ch"
   #include "hbapi.h"
   #include "hbapiitm.h"

   /* These symbols are used internally in bison.simple */

   #undef alloca
   #define alloca  hb_xgrab
   #undef malloc
   #define malloc  hb_xgrab
   #undef free
   #define free hb_xfree

%}

%union {
    int int_val;
    double real_val;
    PHB_ITEM item_val;
    int param;
    int iOperator;
}

%{
   int yyerror(void * stmt,const char * msg);
   int yyparse(void * stmt);
   int yylex(YYSTYPE* yylvaluep, void* s);
%}

%token ERRORVAL

/* Literal Data types */

%token <int_val>  INTEGERVAL
%token <item_val> STRINGVAL
%token <real_val> REALVAL
%token <item_val> DATEVAL
%token <item_val> BINDVAR
%token <item_val> NULLVAL
%token <param>    PARAM
%token <param>    PARAM_NOT_NULL

/* Pairs */

%token <item_val>   IDENT
%token <item_val>   QUOTED_IDENT

/* Operators */

%token <iOperator> ASTERISK
%token <iOperator> EQUALS
%token <iOperator> COMPARE
%token <iOperator> OPERATOR
%token <iOperator> IS_OP
%left  <iOperator> AND_OP
%left  <iOperator> OR
%token <iOperator> IN_OP

/* SQL Keywords */

%token INSERT
%token UPDATE
%token SELECT
%token DELETE_SQL
%token ALL
%token DISTINCT
%token WHERE
%token ORDER
%token LIMIT
%token ASC
%token DESC
%token FROM
%token INTO
%token BY
%token VALUES
%token SET
%token NOT
%token AS
%token UNION
%token LEFT
%token OUTER
%token JOIN
%token GROUP
%token RIGHT
%token LOCK
%token LIKE

/* Supported Functions */

%token COUNT
%token MAX
%token MIN
%token TOKEN_ISNULL
%token SUBSTR
%token ABS
%token POWER
%token ROUND
%token TRIM
%token SUM
%token AVG
%token CURRENT_DATE

%type <item_val> conditional_expression
%type <item_val> conditional_term
%type <item_val> conditional_factor
%type <item_val> conditional_primary
%type <item_val> simple_condition
%type <item_val> order_by_item
%type <item_val> order_by_item_commalist
%type <item_val> opt_limit
%type <item_val> update_item_commalist
%type <item_val> update_item
%type <item_val> insert_item_commalist
%type <item_val> insert_value_commalist
%type <item_val> opt_as
%type <item_val> table
%type <item_val> table_reference
%type <item_val> table_reference_commalist
%type <item_val> select_item
%type <item_val> column
%type <item_val> select_item_commalist
%type <item_val> col_value
%type <item_val> col_constructor
%type <item_val> col_constructor2
%type <item_val> col_name
%type <item_val> scalar_expression_commalist
%type <item_val> opt_lock
%type <item_val> opt_dist
%type <item_val> col_list_name
%type <item_val> select_final
%type <item_val> insert_col_value
%type <item_val> group_by_item_commalist
%type <item_val> group_by_col_expr
%type <item_val> opt_group
%type <item_val> opt_where
%type <item_val> opt_table_alias
%type <item_val> col_constructor_expr
%type <item_val> insert_item_expression
%type <item_val> insert_final
%type <item_val> update_final
%type <item_val> delete_final
%type <item_val> union_expression
%type <item_val> opt_order_by
%type <item_val> select_expression
%type <item_val> sql_expression
%type <item_val> select_col_constructor

%%

sql_expression:
   select_final opt_having opt_order_by {
      // printf( "Found select_final\n" );
      ((sql_stmt *) stmt)->pArray = (PHB_ITEM) SQLpCodeGenArrayJoin( $1, $3 );
      $$ = ((sql_stmt *) stmt)->pArray;
      YYACCEPT;
   }
   | insert_final {
      // printf( "Found insert_final\n" );
      ((sql_stmt *) stmt)->pArray = $1;
      $$ = $1;
      YYACCEPT;
   }
   | update_final {
      // printf( "Found update_final\n" );
      ((sql_stmt *) stmt)->pArray = $1;
      $$ = $1;
      YYACCEPT;
   }
   | delete_final {
      // printf( "Found delete_final\n" );
      ((sql_stmt *) stmt)->pArray = $1;
      $$ = $1;
      YYACCEPT;
   }
   | error {
      // printf( "Parse error.\n" );
      if (((sql_stmt *) stmt)->pTemp )
      {
         hb_itemRelease(((sql_stmt *) stmt)->pTemp );
      }
      YYABORT;
   }
;

select_final:
   select_expression {
      // printf( "Select expression\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | union_expression  {
      // printf( "Select expression with UNION\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

union_expression:
   union_expression UNION select_expression           {
      // printf( "Double UNION\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_SELECT_UNION ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | union_expression UNION ALL select_expression     {
      // printf( "Double UNION ALL\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_SELECT_UNION_ALL ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | select_expression UNION select_expression        {
      // printf( "UNION\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_SELECT_UNION ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | select_expression UNION ALL select_expression    {
      // printf( "UNION ALL\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_SELECT_UNION_ALL ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

select_expression:
   SELECT opt_lock opt_dist opt_limit select_item_commalist FROM table_reference_commalist opt_where opt_group {
      // printf( "Found a SELECT\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $2, SQL_PCODE_SELECT ), $3 ), $4 ), $5 ), SQL_PCODE_SELECT_FROM ), $7), $8), $9 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_order_by:
   /* NULL */ {
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_NO_ORDER );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | ORDER BY order_by_item_commalist {
      // printf( "opt_order_by\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_ORDER, $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

order_by_item_commalist:
   order_by_item {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | order_by_item_commalist ',' order_by_item {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

order_by_item:
   col_name {
      // printf( "Found order_by_item col_name\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_ORDER_ASC, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name ASC {
      // printf( "Found order_by_item col_name ASC\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_ORDER_ASC, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name DESC {
      // printf( "Found order_by_item col_name DESC\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_ORDER_DESC, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_limit:
   /* NULL */ {
      // printf( "No Limit\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_NO_LIMIT );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | LIMIT INTEGERVAL {
      // printf( "Limit %i\n", $2 );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_LIMIT, SQLpCodeGenInt( (int) $2 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

insert_final:
   INSERT INTO table insert_item_expression VALUES '(' insert_value_commalist ')'  {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_INSERT, $3 ), $4 ), SQL_PCODE_INSERT_VALUES ), SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $7 ), SQL_PCODE_STOP_EXPR ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

update_final:
   UPDATE table_reference SET update_item_commalist opt_where {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_UPDATE, $2 ), $4 ), $5 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

delete_final:
   DELETE_SQL FROM table_reference opt_where {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_DELETE, $3 ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

update_item_commalist:
   update_item {
      // printf( "Update commanlist ITEM\n" );
      $$ =  $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | update_item_commalist ',' update_item {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

update_item:
   col_list_name EQUALS column {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

insert_item_expression:
   /* NULL */ {
      $$ = SQLpCodeGenInt( SQL_PCODE_INSERT_NO_LIST );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | '(' insert_item_commalist ')' {
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

insert_item_commalist:
   col_list_name {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | insert_item_commalist ',' col_list_name {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

insert_value_commalist:
   insert_col_value {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | insert_value_commalist ','  insert_col_value {
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

insert_col_value:
   col_value    {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_dist:
   /* NULL */ {
      // printf( "OPT ALL\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_ALL );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | ALL      {
      // printf( "OPT ALL\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_ALL );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | DISTINCT {
      // printf( "DISTINCT\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_DISTINCT );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

select_item_commalist:
   select_item_commalist ',' select_item  opt_as {
      // printf( "select_item_commalist BY select_item_commalist, select_item\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | select_item opt_as {
      // printf( "select_item_commalist BY select_item\n" );
      $$ = SQLpCodeGenArrayJoin( $1, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

select_item:
   column {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT '.' ASTERISK {
      // printf( "%s.*\n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_SELECT_ITEM_ALIAS_ASTER, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | ASTERISK {
      // printf( "*\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_ITEM_ASTERISK );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_as:
   /* NULL */ {
      // printf( "NO AS\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_COLUMN_NO_AS );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | AS STRINGVAL {
      // printf( "AS %s \n", hb_itemGetCPtr((PHB_ITEM)$2) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_AS, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | AS IDENT  {
      // printf( "AS %s \n", hb_itemGetCPtr((PHB_ITEM)$2) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_AS, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | AS QUOTED_IDENT {
      // printf( "AS_QUOTE %s \n", hb_itemGetCPtr((PHB_ITEM)$2) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_AS, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

column:
   select_col_constructor {
      // printf( "column BY select_col_constructor\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | '(' column ')' {
      // printf( "column BY ( column )\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column OPERATOR select_col_constructor {
      // printf( "column BY column OPERATOR select_col_constructor\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column ASTERISK select_col_constructor {
      // printf( "column BY  column ASTERISK select_col_constructor\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column OPERATOR '(' column ')' {
      // printf( "column BY column OPERATOR ( column )\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $4 ), SQL_PCODE_STOP_EXPR ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column ASTERISK '(' column ')' {
      // printf( "column BY column ASTERISK ( column )\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $4 ), SQL_PCODE_STOP_EXPR ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

select_col_constructor:
   col_constructor    {
      // printf( "col constructor\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | COUNT col_constructor_expr ')'  {
      // printf( "COUNT() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_COUNT, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | COUNT ASTERISK ')'  {
      // printf( "COUNT(*) \n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenInt( SQL_PCODE_FUNC_COUNT_AST ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | ABS col_constructor_expr ')'  {
      // printf( "ABS() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_ABS, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | MAX col_constructor_expr ')'  {
      // printf( "MAX() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_MAX, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | MIN col_constructor_expr ')'  {
      // printf( "MIN() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_MIN, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | TOKEN_ISNULL col_constructor_expr ',' col_constructor_expr ')'  {
      // printf( "ISNULL( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_ISNULL, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | SUBSTR col_constructor_expr ',' col_constructor_expr ',' col_constructor_expr ')'  {
      // printf( "SUBSTR( , , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_SUBSTR2, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $6 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | SUBSTR col_constructor_expr ',' col_constructor_expr ')'  {
      // printf( "SUBSTR( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_SUBSTR, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | POWER col_constructor_expr ','  col_constructor_expr ')'  {
      // printf( "power( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_FUNC_POWER, $2 ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | ROUND col_constructor_expr ','  col_constructor_expr ')'  {
      // printf( "ROUND( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_FUNC_ROUND, $2 ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | TRIM col_constructor_expr ')'  {
      // printf( "TRIM() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_TRIM, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | SUM col_constructor_expr ')'  {
      // printf( "SUM() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_SUM, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | AVG col_constructor_expr ')'  {
      // printf( "AVG() function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_AVG, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | CURRENT_DATE ')'  {
      // printf( "DATE() function\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_FUNC_DATE );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_constructor_expr:
   col_constructor  {
      // printf( "col constructor_expr\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | TOKEN_ISNULL col_constructor_expr ',' col_constructor_expr ')'  {
      // printf( "ISNULL( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_ISNULL, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_constructor_expr OPERATOR col_constructor2  {
      // printf( "col constructor_expr OPERATOR: %i\n", $2 );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_constructor_expr ASTERISK col_constructor2  {
      // printf( "col constructor_expr ASTERISK: %i\n", $2 );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

table_reference_commalist:
   table_reference {
      // printf( "table_reference_commalist \n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | table_reference_commalist ',' table_reference {
      // printf( "table_reference_commalist \n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

table_reference:
   table opt_table_alias {
      // printf( "table_reference\n" );
      $$ = SQLpCodeGenArrayJoin( $1, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | '(' select_final ')' opt_table_alias {
      // printf( "table_reference - SUBQUERY\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $2 ), SQL_PCODE_STOP_EXPR ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_table_alias:
   /* NULL */  {
      // printf( "NO TABLE_ALIAS" );
      $$ = SQLpCodeGenInt( SQL_PCODE_TABLE_NO_ALIAS );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT {
      // printf( "TABLE Alias %s \n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_TABLE_ALIAS, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

table:
   IDENT {
      // printf( "TABLE identifier: %s \n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_TABLE_NAME, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | QUOTED_IDENT {
      // printf( "TABLE identifier (quoted): %s \n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_TABLE_NAME, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | PARAM {
      // printf( "PARAMETER table_reference %i\n", $1 );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_TABLE_PARAM, hb_itemPutNI( hb_itemNew(NULL), $1 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | BINDVAR IDENT {
      // printf( "BINDVAR table_reference: %s\n", hb_itemGetCPtr((PHB_ITEM)$2) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_TABLE_BINDVAR, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_where:
   /* NULL */ {
      // printf( "Do not have a WHERE clause\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_NO_WHERE );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | WHERE conditional_expression {
      // printf( "WHERE clause\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_WHERE, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_group:
   /* NULL */  {
      // printf( "NO GROUP_BY\n" );
      $$ = SQLpCodeGenInt( SQL_PCODE_SELECT_NO_GROUPBY );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | GROUP BY group_by_item_commalist {
      // printf( "opt_group_by\n" );
      $$ = SQLpCodeGenIntArray( SQL_PCODE_SELECT_GROUPBY, $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

group_by_item_commalist:
   group_by_col_expr {
      // printf( "group_by_item_commalist col_name\n" );
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | group_by_item_commalist ',' group_by_col_expr  {
      // printf( "group_by_item_commalist , colname\n" );
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

group_by_col_expr:
   col_name {
      $$ = $1;
   }
   | SUBSTR col_name ',' col_value ',' col_value ')'  {
      // printf( "SUBSTR( , , ) function in ORDER BY\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_SUBSTR2, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $6 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | SUBSTR col_name ',' col_value ')'  {
      // printf( "SUBSTR( , ) function in ORDER BY\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_SUBSTR, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | TRIM col_name ')'  {
      // printf( "TRIM() function in ORDER BY\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_TRIM, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_having:
/*  NULL */
/*  | HAVING conditional_expression */
;

conditional_expression:
  conditional_term  {
      // printf("conditional_expression\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
  | conditional_expression OR conditional_term {
      // printf("OR operator\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_OR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

conditional_term:
   conditional_factor  {
      // printf("conditional_term\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | conditional_term AND_OP conditional_factor {
      // printf("AND operator\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_AND ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

conditional_factor:
   conditional_primary {
      // printf("conditional_primary\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | NOT conditional_primary {
      // printf("NOT conditional_primary\n");
      $$ = SQLpCodeGenIntArray( SQL_PCODE_NOT_EXPR, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

conditional_primary:
   simple_condition {
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | '(' conditional_expression ')' {
      // printf("(conditional_primary_parentisis)\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_START_EXPR, $2 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

simple_condition:
   column EQUALS column {
      // printf("comparison_operator %i\n", $2);
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name LIKE col_value {
      // printf("like_operator %i\n", $2);
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_LIKE ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name NOT LIKE col_value {
      // printf("not like_operator %i\n", $2);
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_NOT_LIKE ), $4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name IN_OP '(' select_final ')'  {
      // printf("in_condition subquery\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayIntInt( $1, SQL_PCODE_OPERATOR_IN, SQL_PCODE_START_EXPR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name IN_OP '(' scalar_expression_commalist ')' {
      // printf("in_condition scalar expression\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayIntInt( $1, SQL_PCODE_OPERATOR_IN, SQL_PCODE_START_EXPR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name NOT IN_OP '(' select_final ')'  {
      // printf("in_condition subquery\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayIntInt( $1, SQL_PCODE_OPERATOR_IN + 1, SQL_PCODE_START_EXPR ), $5 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name NOT IN_OP '(' scalar_expression_commalist ')' {
      // printf("in_condition scalar expression\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayIntInt( $1, SQL_PCODE_OPERATOR_IN + 1, SQL_PCODE_START_EXPR ), $5 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column EQUALS '(' select_expression ')'  {
      // printf("equals subquery\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), SQL_PCODE_START_EXPR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column COMPARE '(' select_expression ')'  {
      // printf("equals subquery\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), SQL_PCODE_START_EXPR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column COMPARE column {
      // printf("comparison_operator %i\n", $2);
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), $2 ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column IS_OP NULLVAL      {
      // printf("comparison_operator == NULL\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_IS_NULL );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_constructor NOT NULLVAL            {
      // printf("comparison_operator == NOT NULL\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_IS_NOT_NULL );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | column IS_OP NOT NULLVAL            {
      // printf("comparison_operator == NOT NULL\n");
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_IS_NOT_NULL );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name JOIN col_name {
      // printf("Simple JOIN\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_BASE, $1 ), SQL_PCODE_OPERATOR_JOIN ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name LEFT OUTER JOIN col_name {
      // printf("LEFT OUTER JOIN\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_LEFT_OUTER_JOIN, $1 ), $5 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name RIGHT OUTER JOIN col_name {
      // printf("RIGHT OUTER JOIN\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenIntArray( SQL_PCODE_OPERATOR_RIGHT_OUTER_JOIN, $1 ), $5 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

scalar_expression_commalist:
   scalar_expression_commalist ',' col_value  {
      // printf("scalar_expression_commalist ,col_value\n");
      $$ = SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( $1, SQL_PCODE_COLUMN_LIST_SEPARATOR ), $3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_value  {
      // printf("scalar_expression_commalist col_value\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

opt_lock:
   /* NULL */ {
      // printf("NO_LOCK\n");
      $$ = SQLpCodeGenInt( SQL_PCODE_NOLOCK );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
      }
   | LOCK {
      // printf("LOCK\n");
      $$ = SQLpCodeGenInt( SQL_PCODE_LOCK );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_constructor2:
   col_constructor  {
      // printf("col_constructor - col value\n");
      $$ = $1;
   }
   | TOKEN_ISNULL col_constructor ',' col_constructor ')'  {
      // printf( "ISNULL( , ) function\n" );
      $$ = SQLpCodeGenArrayInt( SQLpCodeGenArrayJoin( SQLpCodeGenArrayInt( SQLpCodeGenIntArray( SQL_PCODE_FUNC_ISNULL, $2 ), SQL_PCODE_COLUMN_LIST_SEPARATOR ), $4 ), SQL_PCODE_STOP_EXPR );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_constructor:
   col_value    {
      // printf("col_constructor - col value\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | col_name   {
      // printf("col_constructor - col name\n");
      $$ = $1;
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_value:
   INTEGERVAL  {
      // printf("INTEGERVAL col value %i\n", $1);
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BY_VALUE, hb_itemPutNI( hb_itemNew(NULL), $1 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | REALVAL   {
      // printf("REALVAL col value %f\n", $1);
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BY_VALUE, hb_itemPutND( hb_itemNew(NULL), $1 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | STRINGVAL {
      // printf("STRING col value: %s\n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BY_VALUE, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | NULLVAL   {
      // printf("NULLVAL col value\n");
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BY_VALUE, hb_itemNew(NULL) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | DATEVAL   {
      // printf("DATEVAL col value\n");
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BY_VALUE, $1 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | PARAM     {
      // printf("PARAM col value %i\n", $1);
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_PARAM, hb_itemPutNI( hb_itemNew(NULL), $1 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | PARAM_NOT_NULL {
      // printf("PARAM_NOT_NULL col value %i\n", $1);
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_PARAM_NOTNULL, hb_itemPutNI( hb_itemNew(NULL), $1 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | BINDVAR IDENT  {
      // printf("BINDVAR col value : %s\n", hb_itemGetCPtr((PHB_ITEM)$2) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_BINDVAR, $2 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_name:
   IDENT '.' QUOTED_IDENT {
      // printf( "ALIASED_QUOTED_COL_NAME : %s - %s\n", hb_itemGetCPtr((PHB_ITEM)$1), hb_itemGetCPtr((PHB_ITEM)$3) );
      $$ = SQLpCodeGenIntItem2( SQL_PCODE_COLUMN_ALIAS, (PHB_ITEM)$1, SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT '.' IDENT {
      // printf( "ALIASED_COL_NAME : %s - %s\n", hb_itemGetCPtr((PHB_ITEM)$1), hb_itemGetCPtr((PHB_ITEM)$3) );
      $$ = SQLpCodeGenIntItem2( SQL_PCODE_COLUMN_ALIAS, (PHB_ITEM)$1, SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$3 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | QUOTED_IDENT {
      // printf( "QUOTED_COL_NAME : %s\n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$1  );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT {
      // printf( "COL_NAME : %s\n", hb_itemGetCPtr((PHB_ITEM)$1) );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$1  );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT '.' PARAM {
      // printf("ALIASED PARAM col value: %s . %i\n", hb_itemGetCPtr((PHB_ITEM)$1),$3 );
      $$ = SQLpCodeGenIntItem2( SQL_PCODE_COLUMN_ALIAS, (PHB_ITEM)$1, SQL_PCODE_COLUMN_NAME_PARAM, hb_itemPutNI( hb_itemNew(NULL), $3 ) );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | IDENT '.' BINDVAR IDENT {
      // printf("BINDVAR col value : %s\n", hb_itemGetCPtr((PHB_ITEM)$3) );
      $$ = SQLpCodeGenIntItem2( SQL_PCODE_COLUMN_ALIAS, (PHB_ITEM)$1, SQL_PCODE_COLUMN_NAME_BINDVAR, (PHB_ITEM)$4 );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

col_list_name:
   IDENT {
      // printf( "col_list_name" );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$1  );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
   | QUOTED_IDENT {
      // printf( "col_list_name QUOTED IDENT" );
      $$ = SQLpCodeGenIntItem( SQL_PCODE_COLUMN_NAME, (PHB_ITEM)$1  );
      // ((sql_stmt *) stmt)->pTemp = (PHB_ITEM) $$;
   }
;

%%
