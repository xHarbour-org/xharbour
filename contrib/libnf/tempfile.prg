/*
 * $Id$
 */

/*
 * File......: TEMPFILE.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.7   28 Sep 1992 23:48:48   GLENN
 * Deleted #define for FLAG_CARRY as Toolkit v2.1's ftint86.ch has it.
 *
 *    Rev 1.6   03 Oct 1991 18:36:28   GLENN
 * Tim Wong from Nantucket pointed out that this DOS function actually
 * leaves a file handle in AX.  In order to preserve the functionality,
 * I now fclose() that handle if the call is succsessful.
 *
 *    Rev 1.5   15 Aug 1991 23:05:04   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.4   17 Jul 1991 22:11:18   GLENN
 * Stripped off chr(0)s in the return value (aRegs[DS])
 *
 *    Rev 1.3   03 Jul 1991 01:08:08   GLENN
 * Changed one line in FT_TEST driver ( cHide == "Y" )
 *
 *    Rev 1.2   14 Jun 1991 19:53:10   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:45:40   GLENN
 * Documentation mods, and convert to new ft_int86() syntax, return value.
 *
 *    Rev 1.0   01 Apr 1991 01:02:24   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_TEMPFIL()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Create a file with a unique name
 *  $SYNTAX$
 *     FT_TEMPFIL( [ <cPath> ] [, <lHide> ] ) -> cFileSpec
 *  $ARGUMENTS$
 *     <cPath> is the directory where you want to create the temporary
 *     file.  If you omit this argument, the root of the current drive
 *     is assumed ("\").
 *
 *     If <lHide> is .T., then the file will be created with the hidden
 *     attribute set.  The default is .F.
 *  $RETURNS$
 *     <cFileSpec> should be your path, including the name of the newly
 *     created unique file.  Use this with FOPEN(), etc.
 *
 *     If a DOS error occurred when trying to create the file, a
 *     null string will be returned.
 *
 *  $DESCRIPTION$
 *     This function uses DOS Interrupt 21, service 5Ah (Create temporary
 *     file) to create a unique filename in a directory you specify.
 *     There will be no extension.  After the file is created, you may
 *     then fopen() it and do any i/o you need (see the test driver
 *     in the source code).
 *
 *     This function requires FT_INT86().
 *  $EXAMPLES$
 *     Create a unique file in the root of the current drive:
 *
 *            myFile := FT_TEMPFIL()
 *
 *     Create a unique file in the current directory and hide it:
 *
 *            myFile := FT_TEMPFIL(".\", .t.)
 *
 *     Create a unique file on another drive, but do not hide it:
 *
 *            myFile := FT_TEMPFIL("e:\nanfor\src\")
 *  $END$
 */

#ifdef HB_OS_DOS
#define FT_TEMPFILE_ORIGINAL
#endif

#ifdef HB_OS_DOS_32
#undef FT_TEMPFILE_ORIGINAL
#endif

#ifdef FT_TEMPFILE_ORIGINAL

#include "ftint86.ch"

#define DOS         33
#define TEMPNAME    90

FUNCTION FT_TEMPFIL( cPath, lHide, nHandle )

   LOCAL  cRet, aRegs[3]

   cPath := iif( ValType( cPath ) != "C",           ;
      Replicate( Chr( 0 ), 13 ) ,            ;
      cPath += Replicate( Chr( 0 ), 13 )   ;
      )

   lHide := iif( ValType( lHide ) != "L", .F. , lHide )
    /*
    aRegs[AX]        := MAKEHI( TEMPNAME )
    aRegs[CX]        := iif( lHide, 2, 0 )
    aRegs[DS]        := cPath
    aRegs[DX]        := REG_DS

    FT_INT86( DOS, aRegs )
    */
   aRegs := _ft_tempfil( cPath, lHide )
    /*  If carry flag is clear, then call succeeded and a file handle is
     *  sitting in AX that needs to be closed.
     */

   IF !ft_isBitOn( aRegs[3], FLAG_CARRY )
      IF hb_isbyref( @nHandle )
         nHandle = aRegs[1]
      ELSE
         FClose( aRegs[1] )
      ENDIF
      cRet := AllTrim( StrTran( aRegs[2], Chr(0 ) ) )
   ELSE
      cRet := ""
   ENDIF

   RETURN cRet

#else

#include "common.ch"

#include "fileio.ch"

FUNCTION FT_TEMPFIL( cPath, lHide, nHandle )

#ifndef __CT_DEPENDENCE__

   LOCAL cFile

   DEFAULT cPath TO ".\"
   DEFAULT lHide TO .F.

   cPath = AllTrim( cPath )

   nHandle := hb_FTempCreate( cPath, nil, if( lHide, FC_HIDDEN, FC_NORMAL ), @cFile )

   IF !hb_isbyref( @nHandle )
      FClose( nHandle )
   ENDIF

   RETURN cFile

#else

   LOCAL nError := 0, cFile

   DEFAULT cPath TO ".\"
   DEFAULT lHide TO .F.

   cPath = AllTrim( cPath )
   IF Right( cPath ) # "\"
      cPath += "\"
   ENDIF


   DO WHILE .T.
      cFile = ntoc( Int( ft_rand1( 65535 ) ), 16 ) + ntoc( Int( ft_rand1( 65535 ) ), 16 )

      nHandle := FOpen( cPath + cFile )  // Use this method because
      // the FILE() function can't see
      // the hidden and system files

      IF FError() = 2   // File not found

         nHandle = FCreate( cPath + cFile, if( lHide, FC_HIDDEN, FC_NORMAL ) )

         IF FError() = 5
            FClose( nHandle )
            LOOP
         ENDIF

         IF FError() # 0
            nError ++
            IF nError > 10
               cFile = ""
               EXIT
            ENDIF
         ENDIF
         FClose( nHandle )

         nHandle = FOpen( cPath + cFile, FO_EXCLUSIVE + FO_READWRITE )
         IF FError() = 0
            EXIT
         ELSE
            nError ++
            IF nError > 10
               cFile = ""
               EXIT
            ENDIF
         ENDIF

      ELSE
         FClose( nHandle )

      ENDIF

   ENDDO

   IF !hb_isbyref( @nHandle )
      FClose( nHandle )
   ENDIF

   RETURN cFile

#endif /* __CT_DEPENDENCE__ */

#endif /* FT_TEMPFILE_ORIGINAL */

#ifdef FT_TEST

FUNCTION MAIN( cPath, cHide )

   LOCAL cFile, nHandle

   cFile := FT_TEMPFIL( cPath, ( cHide == "Y" ) )

   IF !Empty( cFile )
      QOut( cFile )
      nHandle := FOpen( cFile, 1 )
      FWrite( nHandle, "This is a test!" )
      FClose( nHandle )
   ELSE
      QOut( "An error occurred" )
   ENDIF

   RETURN nil

#endif

