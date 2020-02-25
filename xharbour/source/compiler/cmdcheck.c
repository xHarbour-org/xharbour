/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler command line and HARBOURCMD/CLIPPERCMD checking
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
 *    hb_compChkCompilerScmdwitch()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_compChkEnvironVar()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    PackDateTime()
 *    hb_compChkDefineSwitch()
 *    hb_compChkDefines()
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include <time.h>

#include "hbcomp.h"
#include "hbexemem.h"
#if defined( HB_OS_WIN )
   #include <windows.h>
#endif
static char *  argv     = "";
static char ** s_argv   = &argv;

/* TODO: Add support for this compiler switches
   -r -t || hb_getenv( "TMP" )
 */


/* NOTE: Making the date and time info to fit into 32 bits can only be done
         in a "lossy" way, in practice that means it's not possible to unpack
         the exact date/time info from the resulting ULONG. Since the year
         is only stored in 6 bits, 1980 will result in the same bit pattern
         as 2044. The purpose of this value is only used to *differenciate*
         between the dates ( the exact dates are not significant ), so this
         can be used here without problems. [vszakats] */

/* 76543210765432107654321076543210
   |.......|.......|.......|.......
   |____|                               Year    6 bits
   |__|                           Month   4 bits
   |___|                      Day     5 bits
   |___|                 Hour    5 bits
   |____|           Minute  6 bits
 |____|     Second  6 bits */

static ULONG PackDateTime( void )
{
   BYTE        szString[ 4 ];
   BYTE        nValue;

#if defined( HB_OS_WIN )
   SYSTEMTIME  t;
   GetLocalTime( &t );

   nValue         = ( BYTE ) ( ( t.wYear - 1980 ) & ( 2 ^ 6 ) );  /* 6 bits */
   szString[ 0 ]  = nValue << 2;
   nValue         = ( BYTE ) ( t.wMonth );                        /* 4 bits */
   szString[ 0 ]  |= nValue >> 2;
   szString[ 1 ]  = nValue << 6;
   nValue         = ( BYTE ) ( t.wDay ); /* 5 bits */
   szString[ 1 ]  |= nValue << 1;

   nValue         = ( BYTE ) t.wHour; /* 5 bits */
   szString[ 1 ]  = nValue >> 4;
   szString[ 2 ]  = nValue << 4;
   nValue         = ( BYTE ) t.wMinute; /* 6 bits */
   szString[ 2 ]  |= nValue >> 2;
   szString[ 3 ]  = nValue << 6;
   nValue         = ( BYTE ) t.wSecond; /* 6 bits */
   szString[ 3 ]  |= nValue;

#else
   time_t      t;
   struct tm * oTime;

   time( &t );
   oTime          = localtime( &t );

   nValue         = ( BYTE ) ( ( ( oTime->tm_year + 1900 ) - 1980 ) & ( 2 ^ 6 ) );  /* 6 bits */
   szString[ 0 ]  = nValue << 2;
   nValue         = ( BYTE ) ( oTime->tm_mon + 1 );                                 /* 4 bits */
   szString[ 0 ]  |= nValue >> 2;
   szString[ 1 ]  = nValue << 6;
   nValue         = ( BYTE ) ( oTime->tm_mday ); /* 5 bits */
   szString[ 1 ]  |= nValue << 1;

   nValue         = ( BYTE ) oTime->tm_hour; /* 5 bits */
   szString[ 1 ]  = nValue >> 4;
   szString[ 2 ]  = nValue << 4;
   nValue         = ( BYTE ) oTime->tm_min; /* 6 bits */
   szString[ 2 ]  |= nValue >> 2;
   szString[ 3 ]  = nValue << 6;
   nValue         = ( BYTE ) oTime->tm_sec; /* 6 bits */
   szString[ 3 ]  |= nValue;
#endif
   return HB_MKLONG( szString[ 3 ], szString[ 2 ], szString[ 1 ], szString[ 0 ] );
}

char * hb_exeName( void )
{
   return s_argv[0];
}

static void cmdcheckerror( const char *  s )
{
   if( hb_comp_bLogo )
   {
      hb_compPrintLogo();
   }
   hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADOPTION, s, NULL );
}

void hb_compChkCompilerSwitch( int iArg, char * Args[] )
{
   /* If iArg is passed check the command line options */
   if( iArg )
   {
      int i;

      s_argv = Args;

      /* Check all switches in command line
         They start with an OS_OPT_DELIMITER char
       */
      for( i = 1; i < iArg; i++ )
      {
         hb_xstrcat( hb_Command_Line, Args[ i ], " ", NULL );

         if( ! HB_ISOPTSEP( Args[ i ][ 0 ] ) )
         {
            continue;
         }

         if( Args[ i ][ 0 ] == '-' )
         {
            HB_SIZE  j = 1;
            char     Switch[ 7 ];

            Switch[ 0 ] = '-';

            while( Args[ i ][ j ] )
            {
               Switch[ 1 ] = Args[ i ][ j ];

               if( Args[ i ][ j + 1 ] && Args[ i ][ j + 1 ] == '-' )
               {
                  Switch[ 2 ] = '-';
                  Switch[ 3 ] = '\0';

                  hb_compChkEnvironVar( ( char * ) Switch );

                  j += 2;
                  continue;
               }
               else
               {
                  switch( Switch[ 1 ] )
                  {
                     case 'b':
                     case 'B':
                        if( Args[ i ][ j + 1 ] && HB_TOUPPER( Args[ i ][ j + 1 ] ) == 'U' &&
                            Args[ i ][ j + 2 ] && HB_TOUPPER( Args[ i ][ j + 2 ] ) == 'I' &&
                            Args[ i ][ j + 3 ] && HB_TOUPPER( Args[ i ][ j + 3 ] ) == 'L' &&
                            Args[ i ][ j + 4 ] && HB_TOUPPER( Args[ i ][ j + 4 ] ) == 'D' )
                        {
                           Switch[ 2 ] = 'U';
                           Switch[ 3 ] = 'I';
                           Switch[ 4 ] = 'L';
                           Switch[ 5 ] = 'D';
                           Switch[ 6 ] = '\0';

                           hb_compChkEnvironVar( ( char * ) Switch );

                           j += 5;
                           continue;
                        }
                        else if( ! Args[ i ][ j + 1 ] )
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( ( char * ) Switch );
                           j           += 1;
                           continue;
                        }
                        break;

                     case 'c':
                     case 'C':
                        if( Args[ i ][ j + 1 ] && HB_TOUPPER( Args[ i ][ j + 1 ] ) == 'R' &&
                            Args[ i ][ j + 2 ] && HB_TOUPPER( Args[ i ][ j + 2 ] ) == 'E' &&
                            Args[ i ][ j + 3 ] && HB_TOUPPER( Args[ i ][ j + 3 ] ) == 'D' )
                        {
                           Switch[ 2 ] = 'R';
                           Switch[ 3 ] = 'E';
                           Switch[ 4 ] = 'D';
                           Switch[ 5 ] = '\0';

                           j           += 4;

                           if( Args[ i ][ j ] && HB_TOUPPER( Args[ i ][ j ] ) == 'I' )
                           {
                              j++;

                              if( Args[ i ][ j ] && HB_TOUPPER( Args[ i ][ j ] ) == 'T' )
                              {
                                 j++;

                                 if( Args[ i ][ j ] && HB_TOUPPER( Args[ i ][ j ] ) == 'S' )
                                 {
                                    j++;
                                 }
                              }
                           }

                           hb_compChkEnvironVar( ( char * ) Switch );

                           continue;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           cmdcheckerror( ( const char* ) Switch );
                        }
                        /* fallthrough */
                     case 'd':
                     case 'D':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as part of #define and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'e':
                     case 'E':
                        if( Args[ i ][ j + 1 ] && HB_TOUPPER( ( BYTE ) Args[ i ][ j + 1 ] ) == 'X' )
                        {
                           Switch[ 2 ] = 'X';
                           Switch[ 3 ] = '\0';

                           hb_compChkEnvironVar( ( char * ) Switch );

                           j += 2;
                           continue;
                        }
                        else if( Args[ i ][ j + 1 ] && HB_TOUPPER( ( BYTE ) Args[ i ][ j + 1 ] ) == 'S' && Args[ i ][ j + 2 ] && HB_ISDIGIT( ( BYTE ) Args[ i ][ j + 2 ] ) )
                        {
                           Switch[ 2 ] = 'S';
                           Switch[ 3 ] = Args[ i ][ j + 2 ];
                           Switch[ 4 ] = '\0';

                           hb_compChkEnvironVar( ( char * ) Switch );

                           j += 3;
                           continue;
                        }
#if 0
                        else
                        {
                           Switch[ 2 ] = '\0';
                           cmdcheckerror( ( const char* ) Switch );
                        }
#endif
                        break;

                     case 'g':
                     case 'G':
                        /* Required argument */
                        Switch[ 2 ] = Args[ i ][ j + 1 ];
                        if( HB_ISDIGIT( ( BYTE ) Args[ i ][ j + 2 ] ) )
                        {
                           /* Optional argument */
                           Switch[ 3 ] = Args[ i ][ j + 2 ];
                           Switch[ 4 ] = '\0';
                           j           += 3;
                        }
                        else
                        {
                           /* No optional argument */
                           Switch[ 3 ] = '\0';
                           j           += 2;
                        }
                        hb_compChkEnvironVar( ( char * ) Switch );
                        continue;

                     case 'i':
                     case 'I':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as IncludePath and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'j':
                     case 'J':
                        hb_comp_bI18n  = TRUE;
                        Switch[ 2 ]    = '\0';

                        if( Args[ i ][ 2 ] )
                        {
                           hb_comp_szHILout  = ( char * ) hb_xgrab( strlen( Args[ i ] ) );
                           hb_xstrcpy( hb_comp_szHILout, Args[ i ] + 2, 0 );
                           j                 = strlen( Args[ i ] );
                        }
                        else
                        {
                           hb_comp_szHILout  = NULL;
                           j                 = 2;
                        }

                        /* The file will be eventually created when we find an i18n() */
                        continue;

                     case 'k':
                     case 'K':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as part of #define and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'n':
                     case 'N':
                        /* Required argument */
                        if( Args[ i ][ j + 1 ] )
                        {
                           /* Optional argument */
                           Switch[ 2 ] = Args[ i ][ j + 1 ];
                           Switch[ 3 ] = '\0';
                           j           += 2;
                        }
                        else
                        {
                           /* No optional argument */
                           Switch[ 2 ] = '\0';
                           j           += 1;
                        }
                        hb_compChkEnvironVar( ( char * ) Switch );
                        continue;

                     case 'o':
                     case 'O':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as OutputPath and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'p':
                     case 'P':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as OutputPath and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'q':
                     case 'Q':
                        if( Args[ i ][ j + 1 ] && HB_ISDIGIT( ( BYTE ) Args[ i ][ j + 1 ] ) )
                        {
                           Switch[ 2 ] = Args[ i ][ j + 1 ];
                           Switch[ 3 ] = '\0';

                           hb_compChkEnvironVar( ( char * ) Switch );

                           j += 2;
                           continue;
                        }
                        else
                        {
                           Switch[ 2 ] = '\0';
                           hb_compChkEnvironVar( ( char * ) Switch );
                        }

                        break;

                     case 'u':
                     case 'U':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as part of .CH Path or "undef:<id>" and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     case 'v':
                     case 'V':
                        Args[ i ]   += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );
                        j           = strlen( Args[ i ] );
                        continue;

                     case 'w':
                     case 'W':
                        hb_compChkEnvironVar( ( char * ) Args[ i ] );
                        j = strlen( Args[ i ] );
                        continue;

                     case 'x':
                     case 'X':
                        Args[ i ] += ( j - 1 );
                        hb_compChkEnvironVar( Args[ i ] );

                        /* Accept rest as INIT Symbol and continue with next Args[]. */
                        j = strlen( Args[ i ] );
                        continue;

                     default:
                        Switch[ 2 ] = '\0';
                        hb_compChkEnvironVar( ( char * ) Switch );
                  }
               }

               j++;
            }

            continue;
         }

 CheckMultiSlashSwitch:
         {
            int j = 1;
            while( Args[ i ][ j ] && ! HB_ISOPTSEP( Args[ i ][ j ] ) )
               j++;

            if( Args[ i ][ j ] && Args[ i ][ j ] == '/' )
            {
               char cSep = Args[ i ][ j ];
               Args[ i ][ j ] = 0;

               hb_compChkEnvironVar( Args[ i ] );

               Args[ i ]      += j;
               Args[ i ][ 0 ] = cSep;

               goto CheckMultiSlashSwitch;
            }
            else
            {
               hb_compChkEnvironVar( Args[ i ] );
            }
         }
      }
   }
   else
   {
      /* Chech the environment variables */
      /* NOTE: CLIPPERCMD enviroment variable
               is overriden if HARBOURCMD exists
       */
      char * szStrEnv = hb_getenv( "HARBOURCMD" );

      if( ! szStrEnv || szStrEnv[ 0 ] == '\0' )
      {
         if( szStrEnv )
            hb_xfree( ( void * ) szStrEnv );

         szStrEnv = hb_getenv( "CLIPPERCMD" );
      }

      if( szStrEnv )
      {
         char * szSwitch, * szPtr;

         szPtr = szStrEnv;
         while( *szPtr )
         {
            while( *szPtr == ' ' )
               ++szPtr;
            szSwitch = szPtr;
            if( *szSwitch )
            {
               while( *++szPtr )
               {
                  if( *szPtr == ' ' )
                  {
                     *szPtr++ = '\0';
                     break;
                  }
               }
               hb_compChkEnvironVar( szSwitch );
            }
         }
         hb_xfree( ( void * ) szStrEnv );
      }
   }
}

void hb_compChkEnvironVar( char * szSwitch )
{
   if( szSwitch )
   {
      char * s = szSwitch;

      /* If szSwitch doesn't start with a HB_OSOPTSEP char
         show an error
       */

      //printf( "Switch: %s\n", s );

      if( ! HB_ISOPTSEP( *s ) )
      {
         cmdcheckerror( ( const char* ) s );
      }
      else
      {
         s++;

         switch( *s )
         {
            case 'a':
            case 'A':
               if( *( s + 1 ) == '-' )
               {
                  hb_comp_bAutoMemvarAssume = FALSE;
               }
               else
               {
                  hb_comp_bAutoMemvarAssume = TRUE;
               }
               break;

            case 'b':
            case 'B':
            {
               unsigned int   i        = 0;
               char *         szOption = hb_strupr( hb_strdup( s ) );

               while( i < strlen( szOption ) && ! HB_ISOPTSEP( szOption[ i ] ) )
               {
                  i++;
               }

               szOption[ i ] = '\0';

               if( strcmp( szOption, "BUILD" ) == 0 )
               {
                  hb_comp_bBuildInfo = TRUE;
               }
               else
               {
                  if( *( s + 1 ) == '-' )
                  {
                     hb_comp_bDebugInfo = FALSE;
                  }
                  else
                  {
                     hb_comp_bDebugInfo   = TRUE;
                     hb_comp_bLineNumbers = TRUE;
                  }
               }

               hb_xfree( szOption );
            }
            break;

            case 'c':
            case 'C':
            {
               unsigned int   i        = 0;
               char *         szOption = hb_strupr( hb_strdup( s ) );
               while( i < strlen( szOption ) && ! HB_ISOPTSEP( szOption[ i ] ) )
               {
                  i++;
               }
               szOption[ i ] = '\0';

               if( strcmp( szOption, "CREDITS" ) == 0 ||
                   strcmp( szOption, "CREDIT" ) == 0 ||
                   strcmp( szOption, "CREDI" ) == 0 ||
                   strcmp( szOption, "CRED" ) == 0 )
               {
                  hb_comp_bCredits = TRUE;
               }
               else
               {
                  char _szOption[ 256 ];
                  hb_snprintf( _szOption, sizeof( _szOption ), "%s", szOption );
                  hb_xfree( szOption );
                  cmdcheckerror( ( const char* ) _szOption );
               }

               hb_xfree( szOption );
            }
            break;

            case 'd':
            case 'D':
               /* NOTE: Ignore these -d switches will be processed separately */
               break;

            case 'e':
            case 'E':
               if( ( *( s + 1 ) == 'x' || *( s + 1 ) == 'X' ) && !( *( s + 2 ) ) )
               {
                  hb_comp_createExternList = TRUE;
               }
               else if( *( s + 1 ) == 's' || *( s + 1 ) == 'S' )
               {
                  switch( *( s + 2 ) )
                  {
                     case '\0':
                     case '0':
                        hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT;
                        break;

                     case '1':
                        hb_comp_iExitLevel = HB_EXITLEVEL_SETEXIT;
                        break;

                     case '2':
                        hb_comp_iExitLevel = HB_EXITLEVEL_DELTARGET;
                        break;

                     default:
                        cmdcheckerror( ( const char* ) s );
                  }
               }
               else
               {
                  cmdcheckerror( ( const char* ) s );
               }

               break;

            case 'g':
            case 'G':
               switch( *( s + 1 ) )
               {
                  case 'c':
                  case 'C':
                     hb_comp_iLanguage = LANG_C;

                     if( strchr( s + 2, 's' ) || strchr( s + 2, 'S' ) )
                     {
                        hb_comp_iGenVarList = TRUE;
                     }

                     switch( *( s + 2 ) )
                     {
                        case '0':
                           hb_comp_iGenCOutput = HB_COMPGENC_COMPACT;
                           break;

                        case '1':
                           hb_comp_iGenCOutput = HB_COMPGENC_NORMAL;
                           break;

                        case '2':
                        case '\0':  /* default */
                           hb_comp_iGenCOutput = HB_COMPGENC_VERBOSE;
                           break;

                        case '3':
                           hb_comp_iGenCOutput = HB_COMPGENC_REALCODE;
                           break;

                        case '4':
                           hb_comp_iGenVarList = TRUE;
                           break;

                        default:
                           cmdcheckerror( ( const char* ) s );
                     }
                     break;

                  case 'o':
                  case 'O':
                     hb_comp_iLanguage = LANG_OBJ_MODULE;

                     if( strchr( s + 2, 's' ) || strchr( s + 2, 'S' ) )
                     {
                        hb_comp_iGenVarList = TRUE;
                     }

                     switch( *( s + 2 ) )
                     {
                        case '0':
                        case '\0':  /* default */
                           hb_comp_iGenCOutput = HB_COMPGENC_COMPACT;
                           break;

                        case '1':
                           hb_comp_iGenCOutput = HB_COMPGENC_NORMAL;
                           break;

                        case '2':
                           hb_comp_iGenCOutput = HB_COMPGENC_VERBOSE;
                           break;

                        case '3':
                           hb_comp_iGenCOutput = HB_COMPGENC_REALCODE;
                           break;

                        case '4':
                           hb_comp_iGenVarList = TRUE;
                           break;

                        default:
                           cmdcheckerror( ( const char* ) s );
                     }
                     break;

                  case 'h':
                  case 'H':
                     hb_comp_iLanguage = LANG_PORT_OBJ;
                     break;
#if 0
                  case 'w':
                  case 'W':
                     hb_comp_iLanguage = LANG_OBJ32;
                     break;
#endif

                  default:
                     printf( "\nUnsupported output language option\n" );
                     exit( EXIT_FAILURE );
               }
               break;

            /* NOTE:
               h or H from HELP or help
             */
            case 'h':
            case 'H':
            case '?':
               break;

            /* NOTE:
               It already has support for several include files
             */
            case 'i':
            case 'I':
               hb_pp_addSearchPath( hb_comp_PP, s + 1, FALSE );
               break;

            case 'j':
            case 'J':
               hb_comp_bI18n = TRUE;

               if( s[ 2 ] )
               {
                  hb_comp_szHILout = ( char * ) hb_xgrab( strlen( s ) );
                  hb_xstrcpy( hb_comp_szHILout, s + 1, 0 );
               }
               else
               {
                  hb_comp_szHILout = NULL;
               }

               /* The file will be eventually created when we find an i18n() */
               break;

            case 'k':
            case 'K':
            {
               int i = 1;
               while( s[ i ] )
               {
                  switch( s[ i++ ] )
                  {
                     case '?':
                        hb_compPrintLogo();
                        hb_compPrintModes();
                        hb_comp_bLogo  = FALSE;
                        hb_comp_bQuiet = TRUE;
                        break;

                     case 'h':
                        /* default Harbour mode */
                        hb_comp_Supported |= HB_COMPFLAG_HARBOUR;
                        break;

                     case 'c':
                        /* clear all flags - minimal set of features */
                        hb_comp_Supported = HB_COMPFLAG_OPTJUMP;
                        break;

                     case 'x':
                        hb_comp_Supported |= HB_COMPFLAG_XBASE;
                        break;

                     case 'i':
                        hb_comp_Supported |= HB_COMPFLAG_HB_INLINE;
                        break;

                     case 'J':
                        hb_comp_Supported &= ~HB_COMPFLAG_OPTJUMP;
                        break;

                     case 'r':
                        hb_comp_Supported |= HB_COMPFLAG_RT_MACRO;
                        break;

                     default:
                        cmdcheckerror( ( const char* ) s );
                        break;
                  }
               }
            }
            break;

            case 'l':
            case 'L':
               if( *( s + 1 ) == '-' )
               {
                  hb_comp_bLineNumbers = TRUE;
               }
               else
               {
                  hb_comp_bLineNumbers = FALSE;
               }
               break;

            case 'm':
            case 'M':
               if( *( s + 1 ) == '-' )
               {
                  hb_comp_bAutoOpen = TRUE;
               }
               else
               {
                  hb_comp_bAutoOpen = FALSE;
               }
               break;

            case 'n':
            case 'N':
               /*
                  -n1 no start up procedure and no implicit start up procedure
                */
               if( *( s + 1 ) == '1' )
               {
                  hb_comp_bStartProc   = FALSE;
                  hb_comp_bNoStartUp   = TRUE;
               }
               /*
                  -n2 Explicit app start up procedure.
                */
               else if( *( s + 1 ) == '2' )
               {
                  printf( "Explicit Startup\n" );
                  hb_comp_bStartProc         = FALSE;
                  hb_comp_bExplicitStartProc = TRUE;
               }
               /*
                  -n or -n0 no implicit start up procedure
                */
               else if( ( *( s + 1 ) == '0' ) || ( *( s + 1 ) == '\0' ) )
               {
                  hb_comp_bStartProc = FALSE;
               }
               /*
                  -n- ceates implicit start up procedure
                */
               else if( *( s + 1 ) == '-' )
               {
                  hb_comp_bStartProc = TRUE;
               }
               /*
                  invalid command
                */
               else
               {
                  cmdcheckerror( ( const char* ) s );
               }
               break;

            case 'o':
            case 'O':
            {
               char * szPath = hb_strdup( s + 1 );

               if( hb_comp_pOutPath )
                  hb_xfree( hb_comp_pOutPath );

               hb_comp_pOutPath = hb_fsFNameSplit( szPath );
               hb_xfree( szPath );
            }
            break;

            case 'p':
            case 'P':

               hb_comp_bPPO = 1;

               if( *( s + 1 ) == 't' || *( s + 1 ) == 'T' )
               {
                  if( *( s + 2 ) == '-' )
                  {
                     hb_comp_bPPO      = 0;
                     hb_comp_bTracePP  = 0;
                  }
                  else
                  {
                     hb_comp_bTracePP = 1;

                     if( *( s + 2 ) == 'o' || *( s + 2 ) == 'O' )
                     {
                        if( *( s + 3 ) == '-' )
                        {
                           hb_comp_bPPO      = 0;
                           hb_comp_bTracePP  = 0;
                        }
                        else if( *( s + 3 ) != '-' )
                        {
                           char * szPath = hb_strdup( s + 3 );

                           if( hb_comp_ppo_pOutPath )
                              hb_xfree( hb_comp_ppo_pOutPath );

                           hb_comp_ppo_pOutPath = hb_fsFNameSplit( szPath );
                           hb_xfree( szPath );
                        }
                     }
                  }
               }
               else if( *( s + 1 ) == 'o' || *( s + 1 ) == 'O' )
               {
                  char * szPath = hb_strdup( s + 2 );

                  hb_comp_bTracePP     = 0;

                  if( hb_comp_ppo_pOutPath )
                     hb_xfree( hb_comp_ppo_pOutPath );

                  hb_comp_ppo_pOutPath = hb_fsFNameSplit( szPath );
                  hb_xfree( szPath );
               }
               else if( *( s + 1 ) != 0 )
                 cmdcheckerror( ( const char* ) s );

               break;
#if 0
               if( *( s + 1 ) == 't' || *( s + 1 ) == 'T' )
               {
                  if( *( s + 2 ) == '-' )
                  {
                     hb_comp_bPPO      = 0;
                     hb_comp_bTracePP  = 0;
                  }
                  else
                  {
                     hb_comp_bPPO      = 1;
                     hb_comp_bTracePP  = 1;
                  }
               }
               else
               {
                  if( *( s + 1 ) == '-' )
                  {
                     hb_comp_bPPO = 0;
                  }
                  else
                  {
                     hb_comp_bPPO = 1;
                  }
               }
               break;
#endif
            case 'q':
            case 'Q':
               if( *( s + 1 ) == '0' )
               {
                  hb_comp_bLogo = FALSE;
               }

               hb_comp_bQuiet = TRUE;
               break;

            case 'r':
            case 'R':
               /* TODO: Implement this switch */
               printf( "Not yet supported command line option: %s\n", s );
               break;

            case 's':
            case 'S':
               if( *( s + 1 ) == '-' )
               {
                  hb_comp_bSyntaxCheckOnly = FALSE;
               }
               else
               {
                  hb_comp_bSyntaxCheckOnly = TRUE;
               }
               break;

            case 't':
            case 'T':
               /* TODO: Implement this switch */
               printf( "Not yet supported command line option: %s\n", s );
               break;

            case 'u':
            case 'U':
               if( s[ 1 ] && HB_TOUPPER( s[ 1 ] ) == 'N'
                   && s[ 2 ] && HB_TOUPPER( s[ 2 ] ) == 'D'
                   && s[ 3 ] && HB_TOUPPER( s[ 3 ] ) == 'E'
                   && s[ 4 ] && HB_TOUPPER( s[ 4 ] ) == 'F'
                   && s[ 5 ] == ':' )
               {
                  /* NOTE: Ignore these -undef: switches will be processed separately */
                  break;
               }

               if( s[ 1 ] == '+' )
               {
                  hb_pp_STD_CH_ADDITIVE   = 1;
                  hb_pp_STD_CH            = hb_strdup( s + 2 );
               }
               else
               {
                  hb_pp_STD_CH = hb_strdup( s + 1 );
               }

               break;

            case 'v':
            case 'V':
               if( *( s + 1 ) == '-'  && *( s + 2 ) == 0 )
               {
                  hb_comp_bForceMemvars = FALSE;
               }
               else if( HB_TOUPPER( *( s + 1 ) ) == 'D' && *( s + 2 ) == 0 )
               {
                  hb_comp_autoDeferred = TRUE;
               }
#if defined( HB_AVOID_RESERVED_WORDS )
               else if( HB_TOUPPER( *( s + 1 ) ) == 'X'  && *( s + 2 ) == 0 )
               {
                  hb_comp_bUsePPReservedWord = TRUE;
               }
#endif
               else if( *( s + 1 ) == 0 )
               {
                  hb_comp_bForceMemvars = TRUE;
               }
               else
                  cmdcheckerror( ( const char* ) s );

               break;

            case 'w':
            case 'W':

               //printf( "Warning Switch: '%s'\n", s );

               if( s[ 1 ] == '\0' )
               {
                  hb_comp_iWarnings = 1;
               }
               else if( HB_ISDIGIT( s[ 1 ] ) && s[ 2 ] == '\0' )
               {
                  /*there is -w<0,1,2,3> probably */

                  hb_comp_iWarnings = s[ 1 ] - '0';

                  if( hb_comp_iWarnings >= 2 )
                  {
                     hb_comp_bWarnUnUsedLocals        = TRUE;
                     hb_comp_bWarnUnUsedStatics       = TRUE;
                     hb_comp_bWarnUnUsedGlobals       = TRUE;
                     hb_comp_bWarnUnUsedMemvars       = TRUE;
                     hb_comp_bWarnUnUsedFields        = TRUE;
                     hb_comp_bWarnUnUsedBlockParams   = TRUE;
                  }
               }
               else if( HB_ISALPHA( s[ 1 ] ) )
               {
                  unsigned int   i        = 0;
                  char *         szOption = hb_strupr( hb_strdup( s + 1 ) );
                  BOOL           bDisable = FALSE;

                  while( i < strlen( szOption ) && ! HB_ISOPTSEP( szOption[ i ] ) )
                  {
                     i++;
                  }

                  if( szOption[ i ] == '-' )
                  {
                     bDisable = TRUE;
                  }

                  szOption[ i ] = '\0';

                  if( strcmp( szOption, "L" ) == 0 || strcmp( szOption, "LOCALS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedLocals = ! bDisable;
                  }
                  else if( strcmp( szOption, "S" ) == 0 || strcmp( szOption, "STATICS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedStatics = ! bDisable;
                  }
                  else if( strcmp( szOption, "G" ) == 0 || strcmp( szOption, "GLOBALS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedGlobals = ! bDisable;
                  }
                  else if( strcmp( szOption, "M" ) == 0 || strcmp( szOption, "MEMVARS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedMemvars = ! bDisable;
                  }
                  else if( strcmp( szOption, "F" ) == 0 || strcmp( szOption, "FIELDS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedFields = ! bDisable;
                  }
                  else if( strcmp( szOption, "B" ) == 0 || strcmp( szOption, "BLOCKS" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedBlockParams = ! bDisable;
                  }
                  else if( strcmp( szOption, "S" ) == 0 || strcmp( szOption, "ALL" ) == 0 )
                  {
                     hb_comp_bWarnUnUsedLocals        = ! bDisable;
                     hb_comp_bWarnUnUsedStatics       = ! bDisable;
                     hb_comp_bWarnUnUsedGlobals       = ! bDisable;
                     hb_comp_bWarnUnUsedMemvars       = ! bDisable;
                     hb_comp_bWarnUnUsedFields        = ! bDisable;
                     hb_comp_bWarnUnUsedBlockParams   = ! bDisable;
                  }
                  else
                  {
                     char _szOption[ 256 ];
                     hb_snprintf( _szOption, sizeof( _szOption ), "%s", szOption );
                     hb_xfree( szOption );
                     cmdcheckerror( ( const char* ) _szOption );
                  }

                  hb_xfree( szOption );
               }
               else
               {
                  cmdcheckerror( ( const char* ) s );
               }

               if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 4 )
               {
                  cmdcheckerror( ( const char* ) s );
               }
               else
               {
                  if( hb_comp_bWarnUnUsedLocals ||
                      hb_comp_bWarnUnUsedStatics ||
                      hb_comp_bWarnUnUsedGlobals ||
                      hb_comp_bWarnUnUsedMemvars ||
                      hb_comp_bWarnUnUsedFields ||
                      hb_comp_bWarnUnUsedBlockParams )
                  {
                     if( hb_comp_iWarnings < 2 )
                     {
                        hb_comp_iWarnings = 2;
                     }
                  }
                  else
                  {
                     if( hb_comp_iWarnings >= 2 )
                     {
                        hb_comp_iWarnings = 1;
                     }
                  }
               }

               break;

            case 'x':
            case 'X':
            {
               unsigned int i = 1;
               while( s[ i ] && ! HB_ISOPTSEP( s[ i ] ) &&
                      i < sizeof( hb_comp_szPrefix ) - 1 )
               {
                  ++i;
               }
               if( i > 1 )
               {
                  HB_MEMCPY( hb_comp_szPrefix, s + 1, i - 1 );
                  hb_comp_szPrefix[ i - 1 ]  = '_';
                  hb_comp_szPrefix[ i ]      = '\0';
               }
               else
               {
                  hb_snprintf( hb_comp_szPrefix,
                               sizeof( hb_comp_szPrefix ),
                               "%08lX_", PackDateTime() );
               }
               break;
            }

#ifdef YYDEBUG
            case 'y':
            case 'Y':
               yydebug = TRUE;
               break;
#endif

            case 'z':
            case 'Z':
               if( *( s + 1 ) == '-' )
               {
                  hb_comp_bShortCuts = TRUE;
               }
               else
               {
                  hb_comp_bShortCuts = FALSE;
               }
               break;

            default:
               cmdcheckerror( ( const char* ) s );
               break;
         }
      }
   }
}

void hb_compChkPaths( void )
{
   char * szInclude = hb_getenv( "INCLUDE" );

   if( szInclude )
   {
      if( szInclude[ 0 ] != '\0' )
      {
         hb_pp_addSearchPath( hb_comp_PP, szInclude, FALSE );
      }
      hb_xfree( ( void * ) szInclude );
   }
}

static void hb_compChkDefineSwitch( char * pszSwitch )
{
   if( pszSwitch && HB_ISOPTSEP( pszSwitch[ 0 ] ) )
   {
      if( pszSwitch[ 1 ] == 'd' || pszSwitch[ 1 ] == 'D' )
      {
         char *         szDefText   = hb_strdup( pszSwitch + 2 ), * pAssign;
         unsigned int   i           = 0;

         while( i < strlen( szDefText ) && ! HB_ISOPTSEP( szDefText[ i ] ) )
            i++;

         szDefText[ i ] = '\0';
         if( szDefText )
         {
            pAssign = strchr( szDefText, '=' );
            if( pAssign )
               *pAssign++ = '\0';
            hb_pp_addDefine( hb_comp_PP, szDefText, pAssign );
         }
         hb_xfree( szDefText );
      }
      else if( pszSwitch[ 1 ] && HB_TOUPPER( pszSwitch[ 1 ] ) == 'U'
               && pszSwitch[ 2 ] && HB_TOUPPER( pszSwitch[ 2 ] ) == 'N'
               && pszSwitch[ 3 ] && HB_TOUPPER( pszSwitch[ 3 ] ) == 'D'
               && pszSwitch[ 4 ] && HB_TOUPPER( pszSwitch[ 4 ] ) == 'E'
               && pszSwitch[ 5 ] && HB_TOUPPER( pszSwitch[ 5 ] ) == 'F'
               && pszSwitch[ 6 ] == ':' )
      {
         char *         szDefText   = hb_strdup( pszSwitch + 7 );
         unsigned int   i           = 0;

         while( szDefText[ i ] && ! HB_ISOPTSEP( szDefText[ i ] ) )
         {
            i++;
         }
         szDefText[ i ] = '\0';

         if( szDefText[ 0 ] )
            hb_pp_delDefine( hb_comp_PP, szDefText );
         hb_xfree( szDefText );
      }
   }
}

void hb_compChkDefines( int iArg, char * Args[] )
{
   /* Check the environment variables */
   {
      /* NOTE: CLIPPERCMD enviroment variable is overriden
         if HARBOURCMD exists */
      char * szStrEnv = hb_getenv( "HARBOURCMD" );

      if( ! szStrEnv || szStrEnv[ 0 ] == '\0' )
      {
         if( szStrEnv )
            hb_xfree( ( void * ) szStrEnv );

         szStrEnv = hb_getenv( "CLIPPERCMD" );
      }

      if( szStrEnv )
      {
         char * szSwitch, * szPtr;

         szPtr = szStrEnv;
         while( *szPtr )
         {
            while( *szPtr == ' ' )
               ++szPtr;
            szSwitch = szPtr;
            if( *szSwitch )
            {
               while( *++szPtr )
               {
                  if( *szPtr == ' ' )
                  {
                     *szPtr++ = '\0';
                     break;
                  }
               }
               hb_compChkDefineSwitch( szSwitch );
            }
         }
         hb_xfree( ( void * ) szStrEnv );
      }
   }

   /* Check the command line options */
   {
      int i;

      /* Check all switches in command line They start with an OS_OPT_DELIMITER
         char */
      for( i = 1; i < iArg; i++ )
         hb_compChkDefineSwitch( Args[ i ] );
   }
}
