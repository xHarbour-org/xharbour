/*
 * ClipNet Project source code:
 * Compiler Error defines.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef ERRORS_DEFINED

   #define ERRORS_DEFINED

   #define PARSER_FAILURE      1

   typedef enum
   {
      PARSER_ERR_OUTSIDE,
      PARSER_ERR_FUNC_DUPL,
      PARSER_ERR_VAR_DUPL,
      PARSER_ERR_FOLLOWS_EXEC,
      PARSER_ERR_OUTER_VAR,
      PARSER_ERR_NUMERIC_FORMAT,
      PARSER_ERR_STRING_TERMINATOR,
      PARSER_ERR_FUNC_RESERVED,
      PARSER_ERR_ILLEGAL_INIT,
      PARSER_ERR_ENDIF,
      PARSER_ERR_ENDDO,
      PARSER_ERR_ENDCASE,
      PARSER_ERR_NEXTFOR,
      PARSER_ERR_UNMATCHED_ELSE,
      PARSER_ERR_UNMATCHED_ELSEIF,
      PARSER_ERR_SYNTAX,
      PARSER_ERR_UNCLOSED_STRU,
      PARSER_ERR_UNMATCHED_EXIT,
      PARSER_ERR_SYNTAX2,
      PARSER_ERR_INCOMPLETE_STMT,
      PARSER_ERR_CHECKING_ARGS,
      PARSER_ERR_INVALID_LVALUE,
      PARSER_ERR_INVALID_REFER,
      PARSER_ERR_PARAMETERS_NOT_ALLOWED,
      PARSER_ERR_EXIT_IN_SEQUENCE,
      PARSER_ERR_UNTERM_ARRAY_INDEX,
      PARSER_ERR_MEMALLOC,
      PARSER_ERR_MEMREALLOC,
      PARSER_ERR_MEMFREE,
      PARSER_ERR_CREATE_OUTPUT,
      PARSER_ERR_CREATE_PPO,
      PARSER_ERR_BADOPTION,
      PARSER_ERR_BADPARAM,
      PARSER_ERR_BADFILENAME,
      PARSER_ERR_MAYHEM_IN_CASE,
      PARSER_ERR_INVALID_TYPE,
      PARSER_ERR_INVALID_ALIAS,
      PARSER_ERR_INVALID_INDEX,
      PARSER_ERR_INVALID_BOUND,
      PARSER_ERR_BAD_MACRO,
      PARSER_ERR_INVALID_SEND,
      PARSER_ERR_FUNC_ANNOUNCE,
      PARSER_ERR_JUMP_NOT_FOUND,
      PARSER_ERR_CASE,
      PARSER_ERR_BLOCK,
      PARSER_ERR_GET_COMPLEX_MACRO,
      PARSER_ERR_INVALID_INLINE,
      PARSER_ERR_TOOMANY_INLINE,
      PARSER_ERR_REQUIRES_C,
      PARSER_ERR_MISSING_ENDTEXT,
      PARSER_ERR_ILLEGAL_CHARACTER
   } PARSER_ERROR;

   #define PARSER_WARN_AMBIGUOUS_VAR              1
   #define PARSER_WARN_MEMVAR_ASSUMED             2
   #define PARSER_WARN_VAR_NOT_USED               3
   #define PARSER_WARN_BLOCKVAR_NOT_USED          4
   #define PARSER_WARN_NO_RETURN_VALUE            5
   #define PARSER_WARN_PROC_RETURN_VALUE          6
   #define PARSER_WARN_FUN_WITH_NO_RETURN         7
   #define PARSER_WARN_ASSIGN_TYPE                8
   #define PARSER_WARN_OPERAND_TYPE               9
   #define PARSER_WARN_OPERANDS_INCOMPATIBLE      10
   #define PARSER_WARN_ASSIGN_SUSPECT             11
   #define PARSER_WARN_OPERAND_SUSPECT            12
   #define PARSER_WARN_NOT_ARRAY                  13
   #define PARSER_WARN_RETURN_TYPE                14
   #define PARSER_WARN_RETURN_SUSPECT             15
   #define PARSER_WARN_PARAM_COUNT                16
   #define PARSER_WARN_PARAM_TYPE                 17
   #define PARSER_WARN_PARAM_SUSPECT              18
   #define PARSER_WARN_DUP_DECLARATION            19
   #define PARSER_WARN_DECLARATION_CONFLICT       20
   #define PARSER_WARN_NOT_INITIALIZED            21
   #define PARSER_WARN_VAL_NOT_USED               22
   #define PARSER_WARN_ARRAY_ASSIGN_TYPE          23
   #define PARSER_WARN_ARRAY_ASSIGN_SUSPECT       24
   #define PARSER_WARN_CLASS_NOT_FOUND            25
   #define PARSER_WARN_MESSAGE_NOT_FOUND          26
   #define PARSER_WARN_MEANINGLESS                27
   #define PARSER_WARN_UNREACHABLE                28
   #define PARSER_WARN_DUPL_ANNOUNCE              29

   #define PP_ERR_CANNOT_OPEN                   1
   #define PP_ERR_DIRECTIVE_ELSE                2
   #define PP_ERR_DIRECTIVE_ENDIF               3
   #define PP_ERR_WRONG_NAME                    4
   #define PP_ERR_DEFINE_ABSENT                 5
   #define PP_ERR_COMMAND_DEFINITION            6
   #define PP_ERR_PATTERN_DEFINITION            7
   #define PP_ERR_RECURSE                       8
   #define PP_ERR_WRONG_DIRECTIVE               9
   #define PP_ERR_EXPLICIT                      10
   #define PP_ERR_MEMALLOC                      11
   #define PP_ERR_MEMREALLOC                    12
   #define PP_ERR_MEMFREE                       13
   #define PP_ERR_PRAGMA_BAD_VALUE              14
   #define PP_ERR_CANNOT_OPEN_RULES             15
   #define PP_ERR_BAD_RULES_FILE_NAME           16
   #define PP_ERR_TOO_MANY_INCLUDES             17
   #define PP_ERR_BUFFER_OVERFLOW               18

   #define PP_WARN_DEFINE_REDEF                 1
   #define PP_WARN_NO_DIRECTIVES                2

   #if defined(__cplusplus)
      extern "C" {
   #endif

   void Parser_GenError( char * asErrors[], char cPrefix, PARSER_ERROR iError, const char * sError1, const char * sError2, PARSER_CONTEXT *Parser_pContext );
   void Parser_GenWarning( char * asWarnings[], char cPrefix, int iWarning, const char * sWarning1, const char * sWarning2, PARSER_CONTEXT *Parser_pContext );

   #if defined(__cplusplus)
      }
   #endif

#endif