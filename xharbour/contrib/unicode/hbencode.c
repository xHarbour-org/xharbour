/*
 * $Id: hbencode.c,v 1.5 2004/02/03 22:11:43 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    UUENCODE_FILE()
 *    UUENCODE_FILE_BY_CHUNK()
 *    B64ENCODE_FILE()
 *    B64ENCODE_FILE_BY_CHUNK()
 *    YYENCODE_FILE()
 *    YYENCODE_FILE_BY_CHUNK()
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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
This functions might be useful for file transfer via SMTP/NNTP as attachment.

UUENCODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      UUEncode a given file
   Parameters:
      cFileInput = source filename to be encoded
      cFileOutput = output filename
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file

UUENCODE_FILE_BY_CHUNK( <cFileInput>, <nLinePerFile>, [<cFileMask>] ) -> int
   Description:
      UUEncode a given file into file chunks whose size are determined
      ny nLinePerFile
   Parameters:
      cFileInput = source filename to be encoded
      nLinePerFile = number of line per file chunk
      cFileMask = output filename mask
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file
      -3 = invalid number of lines per file

B64ENCODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      Base64-Encode a given file
   Parameters:
      cFileInput = source filename to be encoded
      cFileOutput = output filename
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file

B64ENCODE_FILE_BY_CHUNK( <cFileInput>, <nLinePerFile>, [<cFileMask>] ) -> int
   Description:
      Base64-Encode a given file into file chunks whose size are determined
      ny nLinePerFile
   Parameters:
      cFileInput = source filename to be encoded
      nLinePerFile = number of line per file chunk
      cFileMask = output filename mask
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file
      -3 = invalid number of lines per file

YYENCODE_FILE( <cFileInput>, [<cFileOutput>], [<nCharPerLine>] ) -> int
   Description:
      YYEncode a given file
   Parameters:
      cFileInput = source filename to be encoded
      cFileOutput = output filename
      nCharPerLine = number of characters per line of encoded file (defaut=128)
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file
      -3 = Error in reading file for encoding
      -4 = Error in writing encoded file

YYENCODE_FILE_BY_CHUNK( <cFileInput>, <nLinePerFile>, [<cFilemask>], [<nCharPerLine>] ) -> int
   Description:
      YYEncode a given file into file chunks whose size are determined
      ny nLinePerFile
   Parameters:
      cFileInput = source filename to be encoded
      nLinePerFile = number of line per file chunk
      cFileMask = output filename mask
      nCharPerLine = number of characters per line of encoded file (defaut=128)
   Returns:
       0 = success
      -1 = cannot open source file
      -2 = cannot create output file
      -3 = Error in reading file for encoding
      -4 = Error in writing encoded file
*/

/*
  Note: New error codes 8001 up to 8006 are created here.
        Feel free to change them if so needed
*/

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"

static char basis_64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static int crc_val;
static long crc_anz;
static int crc_tab[256] =
{
   0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
   0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
   0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
   0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
   0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
   0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
   0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
   0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
   0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
   0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
   0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
   0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
   0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
   0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
   0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
   0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
   0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
   0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
   0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
   0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
   0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
   0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
   0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
   0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
   0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
   0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
   0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
   0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
   0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
   0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
   0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
   0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

//----------------------------------------------------------------------------//
static void CrcInit( void )
{
   crc_val = -1L ;
   crc_anz = 0L;
}

//----------------------------------------------------------------------------//
static void CrcAdd(int c)
{
   unsigned long ch1,ch2,cc;

   cc= (c) & 0x000000ffL;
   ch1=(crc_val ^ cc) & 0xffL;
   ch1=crc_tab[ch1];
   ch2=(crc_val>>8L) & 0xffffffL;  // Correct version
   crc_val=ch1 ^ ch2;
   crc_anz++;
}

//----------------------------------------------------------------------------//
static int yEncode(FILE * fDes, char * postname, FILE * fSrc, long filelen, int part, long fulllen, int linelength )
{
   long restlen;
   long srclen;
   long deslen;
   unsigned char srcbuf[4100];
   unsigned char desbuf[260];
   unsigned char * srcp;
   unsigned char * desp;
   unsigned char c;
   int id;
   static long pbegin, pend;

   if (part==0)  // SinglePart message
   {
      fprintf(fDes,"=ybegin line=%ld size=%ld name=%s\r\n",linelength,filelen,postname);
   }
   else          // Multipart message
   {
      if (part==1)
      {
         pbegin=1;
         pend=filelen;
      }
      else
      {
         pbegin=pend+1;
         pend=pend+filelen;
      }

      fprintf(fDes,"=ybegin part=%d line=%ld size=%ld name=%s\r\n",part,linelength,fulllen,postname);
      fprintf(fDes,"=ypart begin=%ld end=%ld\r\n",pbegin,pend);
   }

   CrcInit();

   deslen=0;
   desp=desbuf;
   restlen=filelen;

   while (restlen>0)
   {
      srclen=restlen;

      if (srclen>4096)
      {
         srclen=4096;
      }

      id=fread(srcbuf,srclen,1,fSrc);

      if (id != 1)
      {
         return( -3);
      }

      restlen=restlen-srclen;
      srcp=srcbuf;

      while (srclen>0)
      {
         c=*srcp;                    // Get a source byte
         CrcAdd(c);                  // Add it to the CRC
         c=(unsigned char) (c+42);   // and add the secret number
         srcp++;
         srclen--;

         switch (c)  // Solve special in NNTP 'forbidden' characters
         {
            case 0:
            case 9:
            case 10:
            case 13:
            case '=':   // Including the escape character itself
            case '.':   // Some usual servers have problems with a dot in the first column
              *desp='=';
              desp++;
              deslen++;
              c=(unsigned char)(c+64);
         }

         *desp=c;
         desp++;
         deslen++;

         if ((deslen>=linelength)|((srclen==0)&(restlen==0))) // Block full - or end of file
         {
            *desp=13;
            desp++;
            deslen++;
            *desp=10;
            deslen++;
            id=fwrite(desbuf,deslen,1,fDes);

            if (id!=1)
            {
               return(-4);
            }
            deslen=0; desp=desbuf;
         }
      }
   }

   if (part==0)   // Single part message
   {
      fprintf(fDes,"=yend size=%ld crc32=%08lx \r\n",filelen,crc_val ^ 0xFFFFFFFF);
   }
   else           // Multipart message
   {
      // This does not yet support the full crc32 for the entire file
      fprintf(fDes,"=yend size=%ld part=%d pcrc32=%08lx \r\n",filelen,part,crc_val ^ 0xFFFFFFFF);
   }

   return(0);
}

//----------------------------------------------------------------------------//
BYTE *hbcc_getfilename ( BYTE *strFullPath )
{
   USHORT iLen;
   BYTE *strTmp;

   iLen = strlen ( (char*) strFullPath);
   strTmp = (strFullPath + iLen);

   while ( TRUE )
   {
     if (*strTmp == '\\' || !iLen)
     {
        break;
     }

     strTmp--;
     iLen--;
   }

   if (*strTmp == '\\' )
   {
      strTmp++;
   }

   return ( (BYTE*) strTmp ) ;
}

//----------------------------------------------------------------------------//
static void output64chunk( int c1, int c2, int c3, int pads, FILE *outfile)
{
    putc(basis_64[c1>>2], outfile);
    putc(basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)], outfile);

    if (pads == 2)
    {
       putc('=', outfile);
       putc('=', outfile);
    }
    else if (pads)
    {
       putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
       putc('=', outfile);
    }
    else
    {
       putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
       putc(basis_64[c3 & 0x3F], outfile);
    }
}

//----------------------------------------------------------------------------//
static void b64header( FILE * outfile, BYTE* strIn )
{
   fprintf(outfile,"Content-Type: application/octet-stream; name=\"");
   fprintf(outfile, (char*) hbcc_getfilename (strIn));
   fprintf(outfile,"\"\n");
   fprintf(outfile,"Content-Transfer-Encoding: base64\n\n");

   /*
   Attachment attributes are intentionally ignored here. To use B64 encoded
   file as an email or NNTP article attachment, some more lines have to be
   added, like:

   "Content-Disposition: attachment; filename=myfile.zip"
   */
}

//----------------------------------------------------------------------------//
static int b64encode_file( BYTE *strIn, BYTE *strOut )
{
   FILE *infile, *outfile;
   int c1, c2, c3, ct = 0;

   infile = fopen( (char*) strIn, "rb");

   if ( !infile )
   {
      return -1;
   }

   outfile = fopen ( (char*) strOut, "wb");

   if ( !outfile )
   {
      fclose( infile );
      return -2;
   }

   b64header( outfile, strIn );

   while ((c1 = getc(infile)) != EOF)
   {
      c2 = getc(infile);

      if (c2 == EOF)
      {
         output64chunk(c1, 0, 0, 2, outfile);
      }
      else
      {
         c3 = getc(infile);

         if (c3 == EOF)
         {
            output64chunk(c1, c2, 0, 1, outfile);
         }
         else
         {
            output64chunk(c1, c2, c3, 0, outfile);
         }
      }

      ct += 4;

      if (ct > 71)
      {
         putc('\n', outfile);
         ct = 0;
      }
   }

   if (ct)
   {
     putc('\n', outfile);
   }

   fclose (infile);
   fclose (outfile);

   return 0;
}

//----------------------------------------------------------------------------//
static int b64encode_file_by_chunk ( BYTE *strIn, BYTE *strOut, ULONG lines )
{
   FILE *infile, *outfile;
   int c1, c2, c3, ct=0, filenumber=1;
   LONG nlinesperfile=lines;
   char cfile[256];
   int nResult = 0;

   if ( lines < 4 )
   {
      return -3;
   }

   sprintf( cfile, "%s%02d.b64", strOut, filenumber );

   infile = fopen( (char*) strIn, "rb");

   if ( !infile )
   {
      return -1;
   }

   outfile = fopen ( (char*) cfile, "wb");

   if ( !outfile )
   {
      fclose( infile );
      return -2;
   }

   b64header( outfile, strIn );

   lines -= 3;

   while ((c1 = getc(infile)) != EOF)
   {
      c2 = getc(infile);

      if (c2 == EOF)
      {
         output64chunk(c1, 0, 0, 2, outfile);
      }
      else
      {
         c3 = getc(infile);

         if (c3 == EOF)
         {
            output64chunk(c1, c2, 0, 1, outfile);
         }
         else
         {
            output64chunk(c1, c2, c3, 0, outfile);
         }
      }

      ct += 4;

      if (ct > 71)
      {
         putc('\n',outfile);

         lines --;

         if ( lines == 0 )
         {
            lines = nlinesperfile;
            fclose( outfile );
            filenumber ++;
            *cfile = '\0';
            sprintf( cfile, "%s%02d.b64", strOut, filenumber );
            outfile = fopen ( cfile, "wb");

            if ( !outfile )
            {
               nResult = -2;
               break;
            }
         }

         ct = 0;
      }
   }

   fclose (infile);

   if ( outfile )
   {
      if ( ct )
      {
         putc('\n',outfile);
      }
      fclose (outfile);
   }

   return nResult;
}

//----------------------------------------------------------------------------//
static void putgroup ( char *strgroup, FILE *fp )
{
   USHORT ichr1, ichr2, ichr3, ichr4;

   ichr1 =   strgroup [0] >> 2;
   ichr2 = ((strgroup [0] << 4) & 0x030) | ((strgroup [1] >> 4) & 0x00f);
   ichr3 = ((strgroup [1] << 2) & 0x03c) | ((strgroup [2] >> 6) & 0x003);
   ichr4 =   strgroup [2] & 0x03f;

   fputc( ((ichr1)?((ichr1)&077)+' ':'`'), fp);
   fputc( ((ichr2)?((ichr2)&077)+' ':'`'), fp);
   fputc( ((ichr3)?((ichr3)&077)+' ':'`'), fp);
   fputc( ((ichr4)?((ichr4)&077)+' ':'`'), fp);
}

//----------------------------------------------------------------------------//
static int uuencode_file ( BYTE *strIn, BYTE *strOut )
{
   char strLine[46];
   USHORT iCnt, iLineLen;
   FILE *fpin, *fpOutFile;

   fpin = fopen( (char*) strIn, "rb" );

   if (!fpin)
   {
      return -1;
   }

   fpOutFile = fopen ( (char*) strOut, "wb" );

   if (!fpOutFile)
   {
      fclose (fpin);
      return -2;
   }

   fprintf (fpOutFile, "begin 666 %s\n", hbcc_getfilename (strIn));

   while ( TRUE )
   {
      iLineLen = fread (strLine, sizeof (char), 45, fpin);

      if (iLineLen <= 0)
      {
         break;
      }

      fputc (((iLineLen) ? ((iLineLen) & 077) + ' ': '`'), fpOutFile);

      for (iCnt = 0; iCnt < iLineLen; iCnt += 3)
      {
         putgroup (&strLine[iCnt], fpOutFile);
      }

      putc('\n',fpOutFile);
   }

   fprintf (fpOutFile, "end\n");
   fclose (fpin);
   fclose (fpOutFile);

   return 0;
}

//----------------------------------------------------------------------------//
static int uuencode_file_by_chunk ( BYTE *strIn, BYTE *sMask, ULONG nlines )
{
   char strLine[46];
   USHORT iCnt, iLineLen, filenumber=1;
   FILE *fpin, *fpOutFile;
   ULONG nlinedone=0;
   char cfile[256];
   int nResult = 0;

   if ( nlines < 4 )
   {
      return -3;
   }

   fpin = fopen ( (char*) strIn, "rb" );

   if ( !fpin )
   {
      return -1;
   }

   sprintf( cfile, "%s%02d.uue", sMask, filenumber );

   fpOutFile = fopen ( cfile, "wb" );

   if ( !fpOutFile )
   {
      fclose( fpin );
      return -2;
   }

   fprintf (fpOutFile, "begin 666 %s\n", hbcc_getfilename (strIn));

   while ( TRUE )
   {
     iLineLen = fread (strLine, sizeof (char), 45, fpin);

     if (iLineLen <= 0)
     {
        break;
     }

     fputc ( ((iLineLen) ? ((iLineLen) & 077) + ' ': '`'), fpOutFile);

     for (iCnt = 0; iCnt < iLineLen; iCnt += 3)
     {
       putgroup (&strLine[iCnt], fpOutFile);
     }

     if ( ++nlinedone >= (nlines-1) )
     {
        putc('\n', fpOutFile);
        fclose( fpOutFile );
        nlinedone = -1;
        filenumber ++;

        sprintf( cfile, "%s%02d.uue", sMask, filenumber );

        fpOutFile = fopen ( cfile, "wb");

        if ( !fpOutFile )
        {
           nResult = -2;
           break;
        }
     }
     else
     {
        putc('\n', fpOutFile);
     }

   }

   fclose (fpin);

   if ( fpOutFile )
   {
      fprintf (fpOutFile, "end\n");
      fclose (fpOutFile);
   }

   return nResult;
}

//----------------------------------------------------------------------------//
HB_FUNC( UUENCODE_FILE )
{
   PHB_ITEM pIn  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pOut = hb_param( 2, HB_IT_STRING );
   PHB_FNAME pFileName = NULL;
   char szUUEFileName[ _POSIX_PATH_MAX ] ;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8001, NULL, "UUENCODE_FILE", 1, hb_paramError( 1 ) );
   }

   if ( !pOut )
   {
      pFileName = hb_fsFNameSplit(pIn->item.asString.value);
      pFileName->szExtension = ".uue";
      hb_fsFNameMerge( szUUEFileName, pFileName );
   }
   else
   {
      strcpy( szUUEFileName, pOut->item.asString.value );
   }

   hb_retni( uuencode_file( (BYTE*) pIn->item.asString.value, (BYTE*) szUUEFileName ) );

   if ( pFileName )
   {
      hb_xfree( pFileName );
   }
}

//----------------------------------------------------------------------------//
HB_FUNC( UUENCODE_FILE_BY_CHUNK )
{
   PHB_ITEM pIn   = hb_param( 1, HB_IT_STRING  );
   PHB_ITEM pLine = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pOut  = hb_param( 3, HB_IT_STRING  );
   PHB_FNAME pFileName = NULL;
   char szUUEFileName[ _POSIX_PATH_MAX ] ;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8003, NULL, "UUENCODE_FILE_BY_CHUNK", 1, hb_paramError( 1 ) );
   }

   if ( !pLine )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8003, NULL, "UUENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if ( pLine->item.asLong.value <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8003, NULL, "UUENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if ( !pOut )
   {
      pFileName = hb_fsFNameSplit(pIn->item.asString.value);
      pFileName->szExtension = "";
      hb_fsFNameMerge( szUUEFileName, pFileName );
   }
   else
   {
      strcpy( szUUEFileName, pOut->item.asString.value );
   }

   hb_retni( uuencode_file_by_chunk( (BYTE*) pIn->item.asString.value, (BYTE*) szUUEFileName , (ULONG) pLine->item.asLong.value ) );

   if ( pFileName )
   {
      hb_xfree( pFileName );
   }
}

//----------------------------------------------------------------------------//
HB_FUNC( B64ENCODE_FILE )
{
   PHB_ITEM pIn  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pOut = hb_param( 2, HB_IT_STRING );
   PHB_FNAME pFileName = NULL;
   char szUUEFileName[ _POSIX_PATH_MAX ] ;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8002, NULL, "B64ENCODE_FILE", 1, hb_paramError( 1 ) );
   }

   if ( !pOut )
   {
      pFileName = hb_fsFNameSplit(pIn->item.asString.value);
      pFileName->szExtension = ".b64";
      hb_fsFNameMerge( szUUEFileName, pFileName );
   }
   else
   {
      strcpy( szUUEFileName, pOut->item.asString.value );
   }

   hb_retni( b64encode_file( (BYTE*) pIn->item.asString.value, (BYTE*) szUUEFileName ) );

   if ( pFileName )
   {
      hb_xfree( pFileName );
   }
}

//----------------------------------------------------------------------------//
HB_FUNC( B64ENCODE_FILE_BY_CHUNK )
{
   PHB_ITEM pIn   = hb_param( 1, HB_IT_STRING  );
   PHB_ITEM pLine = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pOut  = hb_param( 3, HB_IT_STRING  );
   PHB_FNAME pFileName = NULL;
   char szUUEFileName[ _POSIX_PATH_MAX ] ;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8004, NULL, "B64ENCODE_FILE_BY_CHUNK", 1, hb_paramError( 1 ) );
   }

   if ( !pLine )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8004, NULL, "B64ENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if ( pLine->item.asLong.value <= 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8004, NULL, "B64ENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if ( !pOut )
   {
      pFileName = hb_fsFNameSplit(pIn->item.asString.value);
      pFileName->szExtension = "";
      hb_fsFNameMerge( szUUEFileName, pFileName );
   }
   else
   {
      strcpy( szUUEFileName, pOut->item.asString.value );
   }

   hb_retni( b64encode_file_by_chunk ( (BYTE *) pIn->item.asString.value, (BYTE *) szUUEFileName, (ULONG) pLine->item.asLong.value ) );

   if ( pFileName )
   {
      hb_xfree( pFileName );
   }
}

//----------------------------------------------------------------------------//
HB_FUNC( YYENCODE_FILE )
{
   FILE * fDes;
   FILE * fSrc;
   LONG filelen;
   PHB_FNAME pFileName;
   PHB_ITEM pIn  = hb_param(1,HB_IT_STRING);
   PHB_ITEM pOut = hb_param(2,HB_IT_STRING);
   PHB_ITEM pLineLength = hb_param(3,HB_IT_NUMERIC);
   char szYYEFileName[ _POSIX_PATH_MAX ] ;
   char pszFileName[ _POSIX_PATH_MAX ];
   USHORT YYELineLength = 128;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8005, NULL, "YYENCODE_FILE", 1, hb_paramError( 1 ) );
   }

   pFileName = hb_fsFNameSplit(pIn->item.asString.value);
   sprintf( pszFileName, "%s%s",pFileName->szName,pFileName->szExtension);
   filelen = hb_fsFSize( (BYTE *) pIn->item.asString.value, TRUE );

   if ( !pOut )
   {
      pFileName->szExtension = ".yye";
      hb_fsFNameMerge( szYYEFileName, pFileName );
   }
   else
   {
      strcpy( szYYEFileName, pOut->item.asString.value );
   }

   fSrc = fopen( pIn->item.asString.value, "rb" );

   if ( !fSrc )
   {
      hb_retni( -1 );
      return;
   }

   fDes = fopen( szYYEFileName, "wb" );

   if ( !fDes )
   {
      fclose( fSrc );
      hb_retni( -2 );
      return;
   }

   if ( pLineLength )
   {
      if (( pLineLength->item.asInteger.value > 0 ) &&
          ( pLineLength->item.asInteger.value <= 255 ))
      {
         YYELineLength = pLineLength->item.asInteger.value;
      }
      else if ( pLineLength->item.asInteger.value > 255 )
      {
         YYELineLength = 255;
      }
   }

   hb_retni( yEncode( fDes, pszFileName, fSrc, filelen, 0, 0, YYELineLength ) );

   hb_xfree( pFileName );

   fclose( fDes );
   fclose( fSrc );

}

//----------------------------------------------------------------------------//
HB_FUNC( YYENCODE_FILE_BY_CHUNK )
{
   FILE * fDes;
   FILE * fSrc;
   LONG filelen;
   LONG nBytes = 0;
   LONG nTotalEncoded = 0;
   PHB_FNAME pFileName;
   PHB_ITEM pIn   = hb_param(1,HB_IT_STRING);
   PHB_ITEM pLine = hb_param(2,HB_IT_NUMERIC);
   PHB_ITEM pOut  = hb_param(3,HB_IT_STRING);
   PHB_ITEM pLineLength = hb_param(4,HB_IT_NUMERIC);
   char szYYEFileName[ _POSIX_PATH_MAX ] ;
   char pszFileName[ _POSIX_PATH_MAX ];
   char *cMask;
   USHORT YYELineLength = 128;
   USHORT iPart = 1;
   int iResult = 0;

   if ( !pIn )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8006, NULL, "YYENCODE_FILE_BY_CHUNK", 1, hb_paramError( 1 ) );
   }

   if ( !pLine )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8006, NULL, "YYENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   if ( pLine->item.asLong.value < 5 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 8006, "Value too small", "YYENCODE_FILE_BY_CHUNK", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   pFileName = hb_fsFNameSplit(pIn->item.asString.value);
   sprintf( pszFileName, "%s%s",pFileName->szName,pFileName->szExtension);
   filelen = hb_fsFSize( (BYTE *) pIn->item.asString.value, TRUE );

   if ( !pOut )
   {
      cMask = pFileName->szName ;
   }
   else
   {
      cMask = pOut->item.asString.value;
   }

   fSrc = fopen( pIn->item.asString.value, "rb" );

   if ( !fSrc )
   {
      hb_xfree( pFileName );
      hb_retni( -1 );
      return;
   }

   if ( pLineLength )
   {
      if (( pLineLength->item.asInteger.value > 0 ) &&
          ( pLineLength->item.asInteger.value <= 255 ))
      {
         YYELineLength = pLineLength->item.asInteger.value;
      }
      else if ( pLineLength->item.asInteger.value > 255 )
      {
         YYELineLength = 255;
      }
   }

   do
   {
      nTotalEncoded += nBytes;

      /*
      This (nBytes) is a rough calculation of bytes to be written based on the
      supplied line number per chunk. YYencoded line is determined by
      YYELineLegth which is 128 bytes by default.
      */

      nBytes = pLine->item.asLong.value * YYELineLength;

      if (( nTotalEncoded + nBytes ) > filelen )
      {
         nBytes = filelen - nTotalEncoded;
      }

      if ( nBytes )
      {
         sprintf( szYYEFileName, "%s%02d%s", cMask, iPart,".yye" );
         fDes = fopen( szYYEFileName, "wb");

         if ( fDes )
         {
            iResult = yEncode( fDes, pszFileName, fSrc, nBytes, iPart, filelen, YYELineLength );
            fclose( fDes );

            if ( iResult != 0 )
            {
               break;
            }

            iPart ++;
         }
         else
         {
            iResult = -2;
            break;
         }
      }
   } while ( nTotalEncoded < filelen );

   fclose( fSrc );

   hb_retni( iResult ) ;

   hb_xfree( pFileName );

}
