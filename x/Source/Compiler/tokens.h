/*
 * ClipNet Project source code:
 * Lexer - Parser Interface and Tokens constants.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef TOKENS_DEFINED

   #define TOKENS_DEFINED

   typedef enum
   {
      TOKEN_FUNC = 256,           /* 256 */
      TOKEN_STATIC_FUNC,          /* 257 */
      TOKEN_INIT_FUNC,            /* 258 */
      TOKEN_EXIT_FUNC,            /* 259 */
      TOKEN_CRITICAL_FUNC,        /* 260 */
      TOKEN_CRITICAL_STATIC_FUNC, /* 261 */
      TOKEN_PROC,                 /* 262 */
      TOKEN_STATIC_PROC,          /* 263 */
      TOKEN_INIT_PROC,            /* 264 */
      TOKEN_EXIT_PROC,            /* 265 */
      TOKEN_CRITICAL_PROC,        /* 266 */
      TOKEN_CRITICAL_STATIC_PROC, /* 267 */
      TOKEN_EXIT,                 /* 268 */
      TOKEN_IDENTIFIER,           /* 269 */
      TOKEN_RETURN,               /* 270 */
      TOKEN_NIL,                  /* 271 */
      TOKEN_INASSIGN,             /* 272 */
      TOKEN_LOCAL,                /* 273 */
      TOKEN_STATIC,               /* 274 */
      TOKEN_GLOBAL,               /* 275 */
      TOKEN_EXTERNGLOBAL,         /* 276 */
      TOKEN_IIF,                  /* 277 */
      TOKEN_IF,                   /* 278 */
      TOKEN_ELSE,                 /* 279 */
      TOKEN_ELSEIF,               /* 280 */
      TOKEN_END,                  /* 281 */
      TOKEN_ENDIF,                /* 282 */
      TOKEN_CONSTANT,             /* 283 */
      TOKEN_ANNOUNCE,             /* 284 */
      TOKEN_EXTERN,               /* 285 */
      TOKEN_AND,                  /* 286 */
      TOKEN_OR,                   /* 287 */
      TOKEN_NOT,                  /* 288 */
      TOKEN_PUBLIC,               /* 289 */
      TOKEN_EQ,                   /* 290 */
      TOKEN_NE,                   /* 291 */
      TOKEN_INC,                  /* 292 */
      TOKEN_DEC,                  /* 293 */
      TOKEN_ALIAS,                /* 294 */
      TOKEN_DOCASE,               /* 295 */
      TOKEN_CASE,                 /* 296 */
      TOKEN_OTHERWISE,            /* 297 */
      TOKEN_ENDCASE,              /* 298 */
      TOKEN_ENDDO,                /* 299 */
      TOKEN_MEMVAR,               /* 300 */
      TOKEN_WHILE,                /* 301 */
      TOKEN_LOOP,                 /* 302 */
      TOKEN_FOR,                  /* 302 */
      TOKEN_NEXT,                 /* 304 */
      TOKEN_TO,                   /* 305 */
      TOKEN_STEP,                 /* 306 */
      TOKEN_FIELD,                /* 307 */
      TOKEN_IN,                   /* 308 */
      TOKEN_PARAMETERS,           /* 309 */
      TOKEN_PLUSEQ,               /* 310 */
      TOKEN_MINUSEQ,              /* 311 */
      TOKEN_MULTEQ,               /* 312 */
      TOKEN_DIVEQ,                /* 313 */
      TOKEN_POWER,                /* 314 */
      TOKEN_EXPEQ,                /* 315 */
      TOKEN_MODEQ,                /* 316 */
      TOKEN_PRIVATE,              /* 317 */
      TOKEN_BEGINSEQ,             /* 318 */
      TOKEN_BREAK,                /* 319 */
      TOKEN_RECOVER,              /* 320 */
      TOKEN_RECOVERUSING,         /* 321 */
      TOKEN_DO,                   /* 322 */
      TOKEN_WITH,                 /* 323 */
      TOKEN_SELF,                 /* 324 */
      TOKEN_LINE,                 /* 325 */
      TOKEN_MACROVAR,             /* 326 */
      TOKEN_MACROTEXT,            /* 327 */
      TOKEN_PROCREQ,              /* 328 */
      TOKEN_GET,                  /* 329 */
      TOKEN_POST,                 /* 330 */
      TOKEN_UNARY,                /* 331 */
      TOKEN_PRE,                  /* 332 */
      TOKEN_DECLARE,              /* 333 */
      TOKEN_OPTIONAL,             /* 334 */
      TOKEN_DECLARE_CLASS,        /* 335 */
      TOKEN_DECLARE_MEMBER,       /* 336 */
      TOKEN_AS_ARRAY,             /* 337 */
      TOKEN_AS_BLOCK,             /* 338 */
      TOKEN_AS_CHARACTER,         /* 339 */
      TOKEN_AS_CLASS,             /* 340 */
      TOKEN_AS_DATE,              /* 341 */
      TOKEN_AS_LOGICAL,           /* 342 */
      TOKEN_AS_NUMERIC,           /* 343 */
      TOKEN_AS_OBJECT,            /* 344 */
      TOKEN_AS_VARIANT,           /* 345 */
      TOKEN_AS_ENUM,              /* 346 */
      TOKEN_AS_ARRAY_ARRAY,       /* 347 */
      TOKEN_AS_BLOCK_ARRAY,       /* 348 */
      TOKEN_AS_CHARACTER_ARRAY,   /* 349 */
      TOKEN_AS_CLASS_ARRAY,       /* 350 */
      TOKEN_AS_DATE_ARRAY,        /* 351 */
      TOKEN_AS_LOGICAL_ARRAY,     /* 352 */
      TOKEN_AS_NUMERIC_ARRAY,     /* 353 */
      TOKEN_AS_OBJECT_ARRAY,      /* 353 */
      TOKEN_AS_ENUM_ARRAY,        /* 354 */
      TOKEN_WITHOBJ,              /* 355 */
      TOKEN_FOREACH,              /* 356 */
      TOKEN_EPSILON,              /* 357 */
      TOKEN_ENUM,                 /* 358 */
      TOKEN_TRY,                  /* 359 */
      TOKEN_CATCH,                /* 360 */
      TOKEN_FINALLY,              /* 361 */
      TOKEN_SWITCH,               /* 362 */
      TOKEN_DEFAULT,              /* 363 */
      TOKEN_CBMARKER,             /* 364 */
      TOKEN_BITAND,               /* 365 */
      TOKEN_BITOR,                /* 366 */
      TOKEN_BITXOR,               /* 367 */
      TOKEN_BITSHIFTR,            /* 368 */
      TOKEN_BITSHIFTL,            /* 369 */
      TOKEN_HASH,                 /* 370 */
      TOKEN_MAX_TOKENS            /* 371 */
   } TOKEN_ID;

#endif