/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2006 Thomas Merz and PDFlib GmbH. All rights reserved. |
 +---------------------------------------------------------------------------+
 |                                                                           |
 |    This software is subject to the PDFlib license. It is NOT in the       |
 |    public domain. Extended versions and commercial licenses are           |
 |    available, please check http://www.pdflib.com.                         |
 |                                                                           |
 *---------------------------------------------------------------------------*/

/* $Id$
 *
 * Various file routines
 *
 */

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #ifndef _CRT_SECURE_NO_WARNINGS
      #define _CRT_SECURE_NO_WARNINGS
   #endif
   #ifndef _CRT_SECURE_NO_DEPRECATE
      #define _CRT_SECURE_NO_DEPRECATE
   #endif
#endif

#include <errno.h>

#include "pc_util.h"
#include "pc_md5.h"
#include "pc_file.h"


/* headers for getpid() or _getpid().
*/
#if defined(WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <io.h>
#define PDC_FILENO _fileno
#else
#define PDC_FILENO fileno
#if defined(MAC)
#include <MacErrors.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif
#endif

#ifndef WINCE
#include <time.h>
#else
#include <winbase.h>
#endif

#ifdef VMS
#include <errno.h>
#include <descrip.h>
#include <ssdef.h>
#endif

#ifndef _IOB_ENTRIES
#define _IOB_ENTRIES 20
#endif

#if defined(WIN32) && !defined(__BORLANDC__) && !defined(__POCC__) && !defined(__WATCOMC__) && !defined(__DMC__)

#define PDC_MAXFILEHANDLES 2048

/* set a new maximum for the number of simultaneously open files.
 * if return = -1 error occured
 */
int
pdc_set_maxfilehandles(pdc_core *pdc, int maxfps)
{
    const char *stemp = pdc_errprintf(pdc, "%d", maxfps);

    if (maxfps < _IOB_ENTRIES)
        pdc_error(pdc, PDC_E_IO_TOOFEW_REQFILEHDLS, stemp,
                  pdc_errprintf(pdc, "%d", _IOB_ENTRIES), 0, 0);

    if (maxfps > PDC_MAXFILEHANDLES)
        pdc_error(pdc, PDC_E_IO_TOOMANY_REQFILEHDLS, stemp,
                  pdc_errprintf(pdc, "%d", PDC_MAXFILEHANDLES), 0, 0);

    return _setmaxstdio(maxfps);
}

int
pdc_get_maxfilehandles(void)
{
    return _getmaxstdio();
}

#endif

/* platform independent wrapper functions for 64-bit file handling.
*/
int
pdc__fseek(FILE *fp, pdc_off_t offset, int whence)
{
#if defined(_LARGEFILE_SOURCE)
    #if defined(WIN32)
        #if defined(__POCC__) && (__POCC__ <=600 )
           #define _telli64  _tell
           #define _lseeki64 _lseek
        #endif
	switch (whence)
	{
	    case SEEK_SET:
            #if defined(__POCC__)
		return fsetpos(fp, (const fpos_t *) &offset);
            #else
		return fsetpos(fp, &offset);
            #endif

	    case SEEK_CUR:
	    {
		pdc_off_t pos;

            #if defined(__POCC__)
		fgetpos(fp, (fpos_t * restrict) &pos);
            #else
		fgetpos(fp, &pos);
            #endif
		pos += offset;
            #if defined(__POCC__)
		return fsetpos(fp, (const fpos_t *) &pos);
            #else
		return fsetpos(fp, &pos);
            #endif
	    }

	    case SEEK_END:
	    {
		pdc_off_t pos, len;

		pos = _telli64(PDC_FILENO(fp));
		_lseeki64(PDC_FILENO(fp), 0, SEEK_END);
		len = _telli64(PDC_FILENO(fp));
		_lseeki64(PDC_FILENO(fp), pos, SEEK_SET);

		len += offset;
            #if defined(__POCC__)
		return fsetpos(fp, (const fpos_t *) &len);
            #else
		return fsetpos(fp, &len);
            #endif
	    }

	    default:
		return -1;
	}
    #else
	return fseeko(fp, offset, whence);
    #endif
#else
    return fseek(fp, offset, whence);
#endif
}

pdc_off_t
pdc__ftell(FILE *fp)
{
#if defined(_LARGEFILE_SOURCE)
    #if defined(WIN32)
	pdc_off_t pos;

        #if defined(__POCC__)
           fgetpos(fp, (fpos_t * restrict) &pos);
        #else
           fgetpos(fp, &pos);
        #endif
	return pos;
    #else
	return ftello(fp);
    #endif
#else
    return ftell(fp);
#endif
}


/* we had troubles writing a block of 80 MB via network on a windows platform.
** this could be fixed by breaking down the entire block into 1 MB pieces.
*/
#undef	PDC_BLOCKSIZE
#define	PDC_BLOCKSIZE	1048576

size_t
pdc__fread(void *ptr, size_t size, size_t nmemb, FILE *fp)
{
    char * cp = (char *) ptr;
    size_t total = size * nmemb;
    size_t left = total;
    size_t n, r;

    do
    {
	n = (left < PDC_BLOCKSIZE) ? left : PDC_BLOCKSIZE;
	r = fread(cp, 1, n, fp);

	left -= r;
	cp += r;
    } while (r == n && left != 0);

    return total - left;
}

size_t
pdc__fwrite(const void *ptr, size_t size, size_t nmemb, FILE *fp)
{
    const char *cp = (const char *) ptr;
    size_t total = size * nmemb;
    size_t left = total;
    size_t n, r;

    do
    {
	n = (left < PDC_BLOCKSIZE) ? left : PDC_BLOCKSIZE;
	r = fwrite(cp, 1, n, fp);

	left -= r;
	cp += r;
    } while (r == n && left != 0);

    return total - left;
}

#undef	PDC_BLOCKSIZE


struct pdc_file_s
{
    pdc_core *pdc;         /* pdcore struct */
    char *filename;        /* file name */
    FILE *fp;              /* file struct or NULL. Then data != NULL: */
    pdc_bool wrmode;       /* writing mode */
    pdc_byte *data;        /* file data or NULL. Then fp != NULL */
    pdc_byte *end;         /* first byte above data buffer */
    pdc_byte *pos;         /* current file position in data buffer */
    pdc_byte *limit;       /* limit of file buffer in writing mode */
};

FILE   *
pdc_get_fileptr(pdc_file *sfp)
{
    return sfp->fp;
}

pdc_core   *
pdc_get_pdcptr(pdc_file *sfp)
{
    return sfp->pdc;
}

#if defined(_MSC_VER) && defined(_MANAGED)
#pragma unmanaged
#endif
int
pdc_get_fopen_errnum(pdc_core *pdc, int errnum)
{
    int outnum = errnum, isread;

    (void) pdc;

    isread = (errnum == PDC_E_IO_RDOPEN);

#if defined(VMS)
   /*
    * On the vms system, when a system error occurs which is not
    * mapped into the unix styled errno values, errno is set EVMSERR
    * and a VMS error code is set in vaxc$errno.
    */
    switch (errno)
    {
        case EVMSERR:
        {
            /* unmapped VMS runtime error - check vaxc$errno */
            switch (vaxc$errno)
            {
                case 100052: /* old style RMS file specification syntax error */
                outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NF;
            }
        }
        return outnum;
    }
#endif /* VMS */

#if defined(MVS)

    switch (errno)
    {
        case 49:
        outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NF;
    }
    return outnum;

#elif defined(WIN32)
    {
	DWORD lasterror = GetLastError();
        switch (lasterror)
        {
            case ERROR_FILE_NOT_FOUND:
            outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NF;
            break;

            case ERROR_ACCESS_DENIED:
            case ERROR_INVALID_PASSWORD:
            case ERROR_NETWORK_ACCESS_DENIED:
            outnum = isread ? PDC_E_IO_RDOPEN_PD : PDC_E_IO_WROPEN_PD;
            break;

            case ERROR_INVALID_NAME:
            outnum = isread ? PDC_E_IO_RDOPEN_IS : PDC_E_IO_WROPEN_IS;
            break;

            case ERROR_PATH_NOT_FOUND:
            case ERROR_INVALID_DRIVE:
            case ERROR_BAD_NETPATH:
            case ERROR_BAD_UNIT:
            outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NP;
            break;

            case ERROR_TOO_MANY_OPEN_FILES:
            case ERROR_SHARING_BUFFER_EXCEEDED:
            outnum = isread ? PDC_E_IO_RDOPEN_TM : PDC_E_IO_WROPEN_TM;
            break;

            case ERROR_BAD_COMMAND:
            outnum = isread ? PDC_E_IO_RDOPEN_BC : PDC_E_IO_WROPEN_BC;
            break;

            case ERROR_FILE_EXISTS:
            outnum = PDC_E_IO_WROPEN_AE;
            break;

            case ERROR_BUFFER_OVERFLOW:
            outnum = PDC_E_IO_WROPEN_TL;
            break;

            case ERROR_WRITE_FAULT:
            case ERROR_CANNOT_MAKE:
            outnum = PDC_E_IO_WROPEN_NC;
            break;

            case ERROR_HANDLE_DISK_FULL:
            case ERROR_DISK_FULL:
            outnum = PDC_E_IO_WROPEN_NS;
            break;

            case ERROR_SHARING_VIOLATION:
            outnum = isread ? PDC_E_IO_RDOPEN_SV : PDC_E_IO_WROPEN_SV;
            break;

            /* This code arises after opening a existing PDF or log file.
             * Because the file could be opened, we can ignore this code.
             */
            case ERROR_ALREADY_EXISTS:
            lasterror = 0;
            outnum = 0;
            break;
        }

        if (lasterror)
        {
            errno = (int) lasterror;
            return outnum;
        }

        /* if lasterror == 0 we must look for errno (see .NET) */
    }

#endif /* WIN32 */

    switch (errno)
    {
#ifdef EACCES
        case EACCES:
        outnum = isread ? PDC_E_IO_RDOPEN_PD : PDC_E_IO_WROPEN_PD;
        break;
#endif
#ifdef EMACOSERR
        case EMACOSERR:
#if defined(MAC) && defined(PDF_ALLOW_MAC_DEPR_FUNCS)

        switch (__MacOSErrNo)
        {
            case fnfErr:
            case dirNFErr:
            case resFNotFound:
            case afpDirNotFound:
            outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NF;
            break;

            case permErr:
            case wrPermErr:
            case wPrErr:
            case afpAccessDenied:
            case afpVolLocked:
            outnum = isread ? PDC_E_IO_RDOPEN_PD : PDC_E_IO_WROPEN_PD;
            break;

            case nsvErr:
            case afpObjectTypeErr:
            outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_IS;
            break;

            case tmfoErr:
            case afpTooManyFilesOpen:
            outnum = isread ? PDC_E_IO_RDOPEN_TM : PDC_E_IO_WROPEN_TM;
            break;

            case opWrErr:
            outnum = PDC_E_IO_WROPEN_AE;
            break;

            case dirFulErr:
            case dskFulErr:
            case afpDiskFull:
            outnum = PDC_E_IO_WROPEN_NS;
            break;

            case fLckdErr:
            case afpLockErr:
            outnum = isread ? PDC_E_IO_RDOPEN_SV : PDC_E_IO_WROPEN_SV;
            break;

            default:
            break;
        }

        if (__MacOSErrNo)
        {
            return outnum;
        }
#else
        outnum = errnum;
        break;
#endif
        break;
#endif
#ifdef ENOENT
        case ENOENT:
        outnum = isread ? PDC_E_IO_RDOPEN_NF : PDC_E_IO_WROPEN_NF;
        break;
#endif
#ifdef EMFILE
        case EMFILE:
        outnum = isread ? PDC_E_IO_RDOPEN_TM : PDC_E_IO_WROPEN_TM;
        break;
#endif
#ifdef ENFILE
        case ENFILE:
        outnum = isread ? PDC_E_IO_RDOPEN_TM : PDC_E_IO_WROPEN_TM;
        break;
#endif
#ifdef EISDIR
        case EISDIR:
        outnum = isread ? PDC_E_IO_RDOPEN_ID : PDC_E_IO_WROPEN_ID;
        break;
#endif
#ifdef EDQUOT
        case EDQUOT:
        outnum = isread ? PDC_E_IO_RDOPEN_QU : PDC_E_IO_WROPEN_QU;
        break;
#endif
#ifdef EEXIST
        case EEXIST:
        outnum = PDC_E_IO_WROPEN_AE;
        break;
#endif
#ifdef ENAMETOOLONG
        case ENAMETOOLONG:
        outnum = PDC_E_IO_WROPEN_TL;
        break;
#endif
#ifdef ENOSPC
        case ENOSPC:
        outnum = PDC_E_IO_WROPEN_NS;
        break;
#endif
        default:

        /* observed on Solaris because of thread unsafe errno */
        if (errno == 0)
            pdc_error(pdc, PDC_E_INT_BADERRNO, 0, 0, 0, 0);

        outnum = errnum;
        break;
    }

    return outnum;
}
#if defined(_MSC_VER) && defined(_MANAGED)
#pragma managed
#endif

void
pdc_set_fopen_errmsg(pdc_core *pdc, int errnum, const char *qualifier,
                     const char *filename)
{
    const char *stemp1 = NULL;
    const char *stemp2 = NULL;
    int errno1 = errno;

#if defined(EMACOSERR)
#if defined(MAC) && defined(PDF_ALLOW_MAC_DEPR_FUNCS)
    errno1 = (int) __MacOSErrNo;
#endif
#endif

    errnum = pdc_get_fopen_errnum(pdc, errnum);
    if (errnum == PDC_E_IO_RDOPEN)
        errnum = PDC_E_IO_RDOPEN_CODE;
    else if (errnum == PDC_E_IO_WROPEN)
        errnum = PDC_E_IO_WROPEN_CODE;
    if (errnum == PDC_E_IO_RDOPEN_CODE || errnum == PDC_E_IO_WROPEN_CODE)
    {
        stemp1 = pdc_errprintf(pdc, "%d", errno1);

#ifdef PDC_HAS_STRERROR
        stemp2 = strerror(errno1);
        if (stemp2 != NULL)
        {
            if (errnum == PDC_E_IO_RDOPEN_CODE)
                errnum = PDC_E_IO_RDOPEN_CODETEXT;
            else if (errnum == PDC_E_IO_WROPEN_CODE)
                errnum = PDC_E_IO_WROPEN_CODETEXT;
        }
#endif
    }

    pdc_set_errmsg(pdc, errnum, qualifier, filename, stemp1, stemp2);
}

void
pdc_set_fwrite_errmsg(pdc_core *pdc, const char *filename)
{
    const char *stemp1 = NULL;
    const char *stemp2 = NULL;
    int errno1 = errno;
    int errnum = PDC_E_IO_WRITE;

#if defined(EMACOSERR)
#if defined(MAC) && defined(PDF_ALLOW_MAC_DEPR_FUNCS)
    errno1 = (int) __MacOSErrNo;
#endif
#endif

    stemp1 = pdc_errprintf(pdc, "%d", errno1);

#ifdef PDC_HAS_STRERROR
    stemp2 = strerror(errno1);
    if (stemp2 != NULL)
        errnum = PDC_E_IO_WRITE_CODETEXT;
#endif

    pdc_set_errmsg(pdc, errnum, filename, stemp1, stemp2, 0);
}

pdc_bool
pdc_check_fopen_errmsg(pdc_core *pdc, pdc_bool requested)
{
    return (requested || pdc_get_errnum(pdc) != PDC_E_IO_RDOPEN_NF) ?
           pdc_false : pdc_undef;
}

static void
pdc_logg_openclose(pdc_core *pdc, FILE *fp, pdc_bool opened)
{
    int errno1 = errno, errno2 = 0;

    if (pdc_logg_is_enabled(pdc, 3, trc_filesearch))
    {
#if defined(WIN32)
        errno2 = (int) GetLastError();
#elif defined(MAC) && defined(PDF_ALLOW_MAC_DEPR_FUNCS)
        errno2 = __MacOSErrNo;
#endif
        pdc_logg(pdc, "\t%p", fp);
        if (opened)
            pdc_logg(pdc, " opened");
        else
            pdc_logg(pdc, " closed");
#if PDC_FILENO_EXISTS
        if (fp != NULL && opened)
            pdc_logg(pdc, ", fileno=%d", PDC_FILENO(fp));
#endif
        pdc_logg(pdc, ", errno=%d", errno1);
        if (errno2 != 0)
            pdc_logg(pdc, ", errno2=%d", errno2);
        pdc_logg(pdc, "\n");

        /* because of logfile IO */
        if (errno != errno1)
            errno = errno1;
    }
}

void *
pdc_read_file(pdc_core *pdc, FILE *fp, pdc_off_t *o_filelen, int incore)
{
    static const char fn[] = "pdc_read_file";
    pdc_off_t filelen = 0, len = 0;
    char *content = NULL;


#if !defined(MVS) || !defined(I370)

    pdc__fseek(fp, 0, SEEK_END);
    filelen = pdc__ftell(fp);
    pdc__fseek(fp, 0, SEEK_SET);

    if (incore && filelen)
    {
        content = (char *) pdc_malloc(pdc, (size_t) (filelen + 1), fn);
        len = (pdc_off_t) pdc__fread(content, 1, (size_t) filelen, fp);

/* because pdc__ftell lies! */
filelen = len;
        if (!filelen)
        {
            pdc_free(pdc, content);
            filelen = 0;
            content = NULL;
        }
    }

#endif

    if (content) content[filelen] = 0;
    *o_filelen = filelen;
    return (void *) content;
}


/*
 * In the case of systems which are not capable of Unicode file names:
 *
 * File name can be converted to Latin-1: The incoming char pointer
 * will be deleted and a new char pointer to the Latin-1 string will
 * be returned. Otherwise an execption will be thrown.
 *
 * Returned string is temporary allocated.
 */
char *
pdc_check_filename(pdc_core *pdc, char *filename)
{
#if !defined(PDC_UNICODE_FILENAME)
    char *ffname = pdc_utf8_to_hostbytes(pdc, pdc->honorlang, filename);

    pdc_free_tmp(pdc, filename);
    if (ffname == NULL)
        pdc_error(pdc, PDC_E_IO_UNSUPP_UNINAME, 0, 0, 0, 0);
    filename = (char *) ffname;
#endif
    ( void ) pdc;
    return filename;
}

/*
 * Returned string is temporary allocated.
 */
char *
pdc_get_filename(pdc_core *pdc, char *filename)
{
    char *ffname;

#if defined(PDC_UNICODE_FILENAME)
    static const char fn[] = "pdc_get_filename";

    ffname = pdc_strdup_ext(pdc, filename, PDC_CONV_TMPALLOC, fn);
#else
    ffname = pdc_hostbytes_to_utf8(pdc, pdc->honorlang, filename);
#endif

    return ffname;
}

/*
 * pdc_convert_filename_ext converts a file name as string of name type
 * (see function pdc_convert_name_ext) to a [EBCDIC-]UTF-8 string with or
 * without a BOM. If the compiler doesn't allow Unicode filenames
 * (see define PDC_UNICODE_FILENAME) the filename is Latin-1 encoded
 * if possible or an exception will be thrown.
 *
 * Returned string is temporary allocated.
 *
 */
const char *
pdc_convert_filename_ext(pdc_core *pdc, const char *filename, int len,
                         const char *paramname, pdc_encoding enc, int codepage,
                         int flags)
{
    char *fname = NULL;
    const char *outfilename = NULL;
    int i = 0;

    if (filename == NULL)
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, paramname, 0, 0, 0);

    /* temporary allocation will be enforced */
    flags |= PDC_CONV_TMPALLOC;

    fname = pdc_convert_name_ext(pdc, filename, len, enc, codepage, flags);

    if (fname == NULL || *fname == '\0')
        pdc_error(pdc, PDC_E_ILLARG_EMPTY, paramname, 0, 0, 0);

    if (pdc_is_utf8_bytecode(fname))
    {
#if defined(PDC_UNICODE_FILENAME)
        i = 3;
#else
        fname = pdc_check_filename(pdc, fname);
#endif
    }

    outfilename = &fname[i];
    return outfilename;
}

/*
 * Returned string is temporary allocated.
 */
const char *
pdc_convert_filename(pdc_core *pdc, const char *filename, int len,
                     const char *paramname, pdc_bool withbom)
{
    int flags = PDC_CONV_EBCDIC;

    if (withbom)
        flags |= PDC_CONV_WITHBOM;

    return pdc_convert_filename_ext(pdc, filename, len, paramname,
                                    pdc_invalidenc, 0, flags);
}

/*
 * pdc_fopen_logg opens a file. The function expects a UTF-8 encoded file name.
 * (see function pdc_convert_filename), if define PDC_UNICODE_FILENAME is set.
 *
 */
FILE *
pdc_fopen_logg(pdc_core *pdc, const char *filename, const char *mode)
{
    FILE *fp = NULL;
    int i = 0;


#if defined(PDC_UNICODE_FILENAME)

    pdc_byte *outfilename = NULL;
    pdc_text_format nameformat = PDC_UTF8;
    pdc_text_format targetnameformat = pdc_utf16;
    int len = (int) pdc_strlen(filename);
    int outlen = 0;

    if (pdc_is_utf16be_unicode(filename))
        nameformat = pdc_utf16be;

    /* convert filename from UTF-8 / UTF-16BE to UTF-16 or Latin-1 */
    pdc_convert_string(pdc, nameformat, 0, NULL, (pdc_byte *) filename, len,
                       &targetnameformat, NULL, &outfilename, &outlen,
                       PDC_CONV_TRYBYTES | PDC_CONV_NOBOM, pdc_true);

    if (targetnameformat == pdc_bytes)
    {
        fp = fopen((const char *) outfilename, mode);
    }
    else
    {
        wchar_t wmode[8];

        len = (int) strlen(mode);
        for (i = 0; i < len; i++)
            wmode[i] = (wchar_t) mode[i];
        wmode[len] = 0;

        fp = _wfopen((wchar_t *) outfilename, wmode);
    }

    pdc_free(pdc, outfilename);

#else
    (void) pdc;

    /* due to honorlang, codeset of LANG: UTF-8 */
    if (pdc_is_utf8_bytecode(filename))
        i = 3;

    fp = fopen(&filename[i], mode);
#endif

    pdc_logg_openclose(pdc, fp, pdc_true);



    return fp;
}

pdc_file *
pdc_fopen(pdc_core *pdc, const char *filename, const char *qualifier,
          const pdc_byte *data, size_t size, int flags)
{
    static const char fn[] = "pdc_fopen";
    pdc_file *sfp;

    /* reset error number */
    pdc_set_errmsg(pdc, 0, 0, 0, 0, 0);

    sfp = (pdc_file *) pdc_calloc(pdc, sizeof(pdc_file), fn);

    /* initialize */
    sfp->pdc = pdc;
    sfp->filename = pdc_strdup_ext(pdc, filename, 0, fn);

    if (flags & PDC_FILE_WRITEMODE || flags & PDC_FILE_APPENDMODE)
        sfp->wrmode = pdc_true;


    if (data != NULL || size > 0)
    {
        /* virtual file */
        if (sfp->wrmode)
        {
            sfp->data = (pdc_byte *) pdc_calloc(pdc, size, fn);
            if (data != NULL)
            {
                /* append mode */
                memcpy(sfp->data, data, size);
                sfp->pos = sfp->data + size;
            }
            else
            {
                sfp->pos = sfp->data;
            }
            sfp->end = sfp->pos;
            sfp->limit = sfp->data + size;
        }
        else
        {
            sfp->data = (pdc_byte *) data;
            sfp->pos = sfp->data;
            sfp->end = sfp->data + size;
        }
    }
    else
    {
        const char *mode;


        /* disk file */
        if (flags & PDC_FILE_BINARY)
            mode = READBMODE;
        else
            mode = READTMODE;
        if (flags & PDC_FILE_APPENDMODE)
            mode = APPENDMODE;
        else if (flags & PDC_FILE_WRITEMODE)
            mode = WRITEMODE;

        sfp->fp = pdc_fopen_logg(pdc, filename, mode);
        if (sfp->fp == NULL)
        {
            pdc_fclose(sfp);

            if (qualifier == NULL)
                qualifier = "";
            pdc_set_fopen_errmsg(pdc, PDC_E_IO_RDOPEN, qualifier, filename);
            return NULL;
        }
    }

    return sfp;
}

pdc_bool
pdc_file_isvirtual(pdc_file *sfp)
{
    return sfp->fp ? pdc_false : pdc_true;
}

char *
pdc_file_name(pdc_file *sfp)
{
    return sfp->filename;
}

pdc_core *
pdc_file_getpdc(pdc_file *sfp)
{
    return sfp->pdc;
}

pdc_off_t
pdc_file_size(pdc_file *sfp)
{
    pdc_off_t filelen;

    if (sfp->fp)
    {
        pdc_off_t pos = pdc__ftell(sfp->fp);

        pdc_read_file(sfp->pdc, sfp->fp, &filelen, 0);
        pdc__fseek(sfp->fp, pos, SEEK_SET);
    }
    else
        filelen = (pdc_off_t) (sfp->end - sfp->data);

    return filelen;
}

const void *
pdc_freadall(pdc_file *sfp, size_t *filelen, pdc_bool *ismem)
{
    const void *result = NULL;

    *filelen = 0;

    pdc_logg_cond(sfp->pdc, 1, trc_filesearch,
        "\tAttempting to read whole file \"%s\"\n", sfp->filename);

    if (sfp->fp)
    {
        pdc_off_t flen;     /* TODO2GB: >2GB on 32-bit platforms? */

        result = pdc_read_file(sfp->pdc, sfp->fp, &flen, 1);

        if (ismem)
            *ismem = pdc_false;
	*filelen = (size_t) flen;
    }
    else
    {
        result = sfp->data;

        if (ismem)
            *ismem = pdc_true;
        *filelen = (size_t) (sfp->end - sfp->data);
    }

    pdc_logg_cond(sfp->pdc, 1, trc_filesearch,
        "\t%d bytes read from %s file, contents=%p\n",
        (int) (*filelen),
        (sfp->fp) ? "disk" : "memory",
        result);

    return result;
}

char *
pdc_fgetline(char *s, int size, pdc_file *sfp)
{
    int i, c;


    c = pdc_fgetc(sfp);
    if (c == EOF)
        return NULL;


    size--;
    for (i = 0; i < size; i++)
    {
        if (c == '\n' || c == '\r')
            break;
        s[i] = (char) c;

        c = pdc_fgetc(sfp);
        if (c == EOF)
        {
            i++;
            break;
        }

    }
    s[i] = 0;

    /* Skip windows line end \r\n */
    if (c == '\r')
    {
        c = pdc_fgetc(sfp);
        if (c != EOF)
        {
            if (c != '\n')
            {
                if (sfp->fp)
                {

                    ungetc(c, sfp->fp);
                }
                else
                {
                    pdc_fseek(sfp, -1, SEEK_CUR);
                }
            }
        }
    }
    return s;
}

/*
 * Emulation of C file functions - relevant for PDFlib
 */

pdc_off_t
pdc_ftell(pdc_file *sfp)
{
    if (sfp->fp)
        return pdc__ftell(sfp->fp);

    return (pdc_off_t) (sfp->pos - sfp->data);
}

int
pdc_fseek(pdc_file *sfp, pdc_off_t offset, int whence)
{
    static const char fn[] = "pdc_fseek";

    if (sfp->fp)
        return pdc__fseek(sfp->fp, offset, whence);

    switch (whence)
    {
        case SEEK_SET:
        sfp->pos = sfp->data + offset;
        break;

        case SEEK_CUR:
        sfp->pos += offset;
        break;

        case SEEK_END:
        sfp->pos = sfp->end;
        break;
    }

    if (sfp->pos > sfp->end)
    {
        /* extend file in writing mode */
        if (sfp->wrmode)
        {
            size_t nbytes = (size_t) (sfp->pos - sfp->end);

            if (sfp->pos > sfp->limit)
            {
                size_t size = (size_t) (sfp->pos - sfp->data);

                sfp->data = (pdc_byte *) pdc_realloc(sfp->pdc, sfp->data, size,
                                                     fn);
                sfp->end = sfp->data + size;
                sfp->pos = sfp->end;
                sfp->limit = sfp->end;
            }

            memset(sfp->pos - nbytes, 0, nbytes);
        }
        else
        {
            return -1;
        }
    }
    else if (sfp->pos < sfp->data)
    {
        return -1;
    }

    return 0;
}

size_t
pdc_fread(void *ptr, size_t size, size_t nmemb, pdc_file *sfp)
{
    size_t nbytes = 0;

    if (sfp->fp)
        return pdc__fread(ptr, size, nmemb, sfp->fp);

    nbytes = size * nmemb;
    if (sfp->pos + nbytes > sfp->end)
    {
        nbytes = (size_t) (sfp->end - sfp->pos);
        nmemb = nbytes / size;
        nbytes = nmemb *size;
    }

    if (nbytes)
        memcpy(ptr, sfp->pos, nbytes);
    sfp->pos += nbytes;

    return nmemb;
}

size_t
pdc_fwrite(const void *ptr, size_t size, size_t nmemb, pdc_file *sfp)
{
    static const char fn[] = "pdc_fwrite";

    if (sfp->wrmode)
    {
        size_t poslen, nbytes = 0;

        if (sfp->fp)
        {
            size_t total = pdc__fwrite(ptr, size, nmemb, sfp->fp);

            if (total < size * nmemb)
            {
                pdc_set_fwrite_errmsg(sfp->pdc, sfp->filename);
                PDC_RETHROW(sfp->pdc);
            }

            return total;
        }

        nbytes = size * nmemb;
        if (sfp->pos + nbytes > sfp->limit)
        {
            poslen = (size_t) (sfp->pos - sfp->data);
            size = poslen + nbytes;

            sfp->data = (pdc_byte *) pdc_realloc(sfp->pdc, sfp->data, size, fn);
            sfp->pos = sfp->data + poslen;
            sfp->end = sfp->data + size;
            sfp->limit = sfp->end;
        }
        memcpy(sfp->pos, ptr, nbytes);
        sfp->pos += nbytes;
        if (sfp->pos > sfp->end)
            sfp->end = sfp->pos;
    }
    else
    {
        nmemb = 0;
    }

    return nmemb;
}

void
pdc_freset(pdc_file *sfp, size_t size)
{
    static const char fn[] = "pdc_freset";

    if (sfp->wrmode && !sfp->fp)
    {
        if (size > (size_t) (sfp->limit - sfp->data))
        {
            sfp->data = (pdc_byte *) pdc_realloc(sfp->pdc, sfp->data, size, fn);
            sfp->limit = sfp->data + size;
        }

        sfp->pos = sfp->data;
        sfp->end = sfp->data;
    }
}

int
pdc_fgetc(pdc_file *sfp)
{
    int ch = 0;

    if (sfp->fp)
        return pdc__fgetc(sfp->fp);

    if (sfp->pos < sfp->end)
    {
        ch = (int) *sfp->pos;
        sfp->pos++;
    }
    else
    {
        ch = EOF;
    }

    return ch;
}

int
pdc_feof(pdc_file *sfp)
{
    if (sfp->fp)
        return pdc__feof(sfp->fp);

    return (sfp->pos >= sfp->end) ? 1 : 0;
}

void
pdc_fclose_logg(pdc_core *pdc, FILE *fp)
{
    fclose(fp);
    pdc_logg_openclose(pdc, fp, pdc_false);
}

void
pdc_fclose(pdc_file *sfp)
{
    if (sfp)
    {
        if (sfp->fp)
        {
            pdc_fclose_logg(sfp->pdc, sfp->fp);
            sfp->fp = NULL;
        }
        else if (sfp->wrmode)
        {
            if (sfp->data)
            {
                pdc_free(sfp->pdc, sfp->data);
                sfp->data = NULL;
            }
        }

        if (sfp->filename)
        {
            pdc_free(sfp->pdc, sfp->filename);
            sfp->filename = NULL;
        }

        pdc_free(sfp->pdc, sfp);
    }
}

/*
 * Concatenating a directory name with a file base name to a full valid
 * file name of maximal length PDC_FILENAMELEN - 1.
 *
 * On MVS platforms an extension at the end of basename will be discarded.
 */
void
pdc_file_fullname(pdc_core *pdc, const char *dirname, const char *basename,
                  char *fullname)
{
    const char *pathsep = PDC_PATHSEP;
    const char *stemp = NULL;
    pdc_bool dirhasbom = (dirname != NULL && pdc_is_utf8_bytecode(dirname));
    pdc_bool basehasbom = (basename != NULL && pdc_is_utf8_bytecode(basename));
    const char *sdirname = dirname;
    const char *sbasename = basename;
    size_t len = 0;

    fullname[0] = 0;
    if (dirhasbom || basehasbom)
    {
        len = 3;
        strcat(fullname, PDC_UTF8_STRING);
        if (dirhasbom)
            sdirname = &dirname[3];
        if (basehasbom)
            sbasename = &basename[3];
    }

#ifdef MVS
    pdc_bool lastterm = pdc_false;
#endif

    if (sdirname == NULL || !sdirname[0])
    {
        len += strlen(sbasename);
        if (len >= PDC_FILENAMELEN)
            goto PDC_FILE_ERROR;

        strcat(fullname, sbasename);
    }
    else
    {
#ifdef MVS
        if (strncmp(sdirname, PDC_FILEQUOT, 1))
            strcat(fullname, PDC_FILEQUOT);
#endif

        len += strlen(sdirname);
        if (len >= PDC_FILENAMELEN)
            goto PDC_FILE_ERROR;

        strcat(fullname, sdirname);

#ifdef VMS
        /* look for logical name or whose result */
        if(getenv(sdirname))
            pathsep = PDC_PATHSEP_LOG;
        else if (fullname[strlen(fullname)-1] == ']')
            pathsep = "";
#endif

        len += strlen(pathsep) + strlen(sbasename);
        if (len >= PDC_FILENAMELEN)
            goto PDC_FILE_ERROR;

        strcat(fullname, pathsep);
        strcat(fullname, sbasename);

#ifdef MVS
        lastterm = pdc_true;
#endif
    }

#ifdef MVS
    {
        int ie, len;

        len = strlen(fullname);
        for (ie = len - 1; ie >= 0; ie--)
        {
            if (fullname[ie] == pathsep[0])
                break;

            if (fullname[ie] == '.')
            {
                fullname[ie] = 0;
                break;
            }
        }
        if (lastterm)
        {
            len += strlen(PDC_PATHTERM) + strlen(PDC_FILEQUOT);
            if (len >= PDC_FILENAMELEN)
                goto PDC_FILE_ERROR;

            strcat(fullname, PDC_PATHTERM);
            strcat(fullname, PDC_FILEQUOT);
        }
    }
#endif

    return;

    PDC_FILE_ERROR:

    if (sdirname == NULL || !sdirname[0])
        stemp = pdc_errprintf(pdc, "%s", basename);
    else
        stemp = pdc_errprintf(pdc, "%s%s%s", dirname, pathsep, basename);
    pdc_error(pdc, PDC_E_IO_TOOLONG_FULLNAME, stemp, 0, 0, 0);
}

#define EXTRA_SPACE     32     /* extra space for separators, FILEQUOT etc. */

char *
pdc_file_fullname_mem(pdc_core *pdc, const char *dirname, const char *basename)
{
    static const char fn[] = "pdc_file_fullname_mem";
    char *fullname;
    size_t len;

    len = strlen(basename);
    if (dirname && dirname[0])
        len += strlen(dirname);
    len += EXTRA_SPACE;
    fullname = (char *) pdc_malloc(pdc, len, fn);

    pdc_file_fullname(pdc, dirname, basename, fullname);

    return fullname;
}

/*
 * Returns the full specified path name in a new memory.
 * The full path name can be concatened by a path name,
 * file name and extension (incl. dot).
 */
char *
pdc_file_concat(pdc_core *pdc, const char *dirname, const char *basename,
                const char *extension)
{
    static const char fn[] = "pdc_file_concat";
    char *pathname = pdc_file_fullname_mem(pdc, dirname, basename);
    size_t len = strlen(pathname) + 1;

    if (extension != NULL)
        len += strlen(extension);

    pathname = (char *) pdc_realloc(pdc, pathname, len, fn);

    if (extension != NULL)
        strcat(pathname, extension);

    return pathname;
}

/*
 * Returns the file basename of a full specified path name in the same memory.
 */
const char *
pdc_file_strip_dirs(const char *pathname)
{
    const char *scan = pathname + strlen(pathname);

    while (pathname <= --scan)
    {
        if (*scan == PDC_PATHSEP[0] || *scan == PDC_PATHSEP_ALT[0])
            return scan + 1;
    }

    return pathname;
}


/*
 * Returns the file path of a full specified path name in the same memory.
 */
char *
pdc_file_strip_name(char *pathname)
{
    char *scan = pathname + strlen(pathname);

    while (pathname <= --scan)
    {
        if (*scan == PDC_PATHSEP[0] || *scan == PDC_PATHSEP_ALT[0])
        {
            *scan = 0;
            break;
        }
    }

    return pathname;
}


/*
 * Returns the file extension of a path name in the same memory.
 */
char *
pdc_file_strip_ext(char *pathname)
{
    char *scan = pathname + strlen(pathname);

    while (pathname <= --scan)
    {
        if (*scan == '.')
        {
            *scan = 0;
            break;
        }
    }

    return pathname;
}


/*
 * Function reads a text file and creates a string list
 * of all no-empty and no-comment lines. The strings are stripped
 * by leading and trailing white space characters.
 *
 * The caller is responsible for freeing the resultated string list
 * by calling the function pdc_cleanup_stringlist.
 *
 * Not for unicode strings.
 *
 * Return value: Number of strings
 */

#define PDC_ARGV_CHUNKSIZE 256

int
pdc_read_textfile(pdc_core *pdc, pdc_file *sfp, int flags, char ***linelist)
{
    static const char fn[] = "pdc_read_textfile";
    char buf[PDC_BUFSIZE];
    char *content = NULL;
    char **strlist = NULL;
    int nlines = 0;
    pdc_off_t filelen;
    size_t len = 0, sumlen = 0, maxl = 0;
    pdc_bool tocont = pdc_false;
    int i, nbs, is = -1;

    /* get file length */
    filelen = pdc_file_size(sfp);
    if (filelen)
    {
        /* allocate content array */
        content = (char *) pdc_calloc(pdc, (size_t) filelen, fn);

        /* read loop */
        while (pdc_fgetline(buf, PDC_BUFSIZE, sfp) != NULL)
        {
            /* trim white spaces */
            if (tocont)
                pdc_strtrim(buf);
            else
                pdc_str2trim(buf);

            /* skip blank and comment lines */
            if (buf[0] == 0 || buf[0] == '%')
            {
                tocont = pdc_false;
                continue;
            }

            /* register new line */
            if (!tocont)
            {
                if (nlines)
                    pdc_logg_cond(pdc, 2, trc_filesearch,
                        "\t\tLine %d; \"%s\"\n", nlines, strlist[nlines - 1]);

                if (nlines >= (int) maxl)
                {
                    maxl += PDC_ARGV_CHUNKSIZE;
                    strlist = (strlist == NULL) ?
                            (char **)pdc_malloc(pdc, maxl * sizeof(char *), fn):
                            (char **)pdc_realloc(pdc, strlist, maxl *
                                                 sizeof(char *), fn);
                }

                is += ( int ) sumlen + 1;
                strlist[nlines] = &content[is];
                nlines++;
                sumlen = 0;
            }

            /* process new line */
            nbs = 0;
            len = strlen(buf);
            for (i = 0; i < (int) len; i++)
            {
                /* backslash found */
                if (buf[i] == '\\')
                {
                    nbs++;
                }
                else
                {
                    /* comment sign found */
                    if (buf[i] == '%')
                    {
                        if (nbs % 2)
                        {
                            /* masked */
                            memmove(&buf[i-1], &buf[i], (size_t) (len-i));
                            len--;
                            buf[len] = 0;
                        }
                        else
                        {
                            buf[i] = 0;
                            len = strlen(buf);
                        }
                    }
                    nbs = 0;
                }
            }

            /* continuation line */
            tocont = (nbs % 2) ? pdc_true : pdc_false;
            if (tocont)
            {
                if (flags & PDC_FILE_KEEPLF)
                    buf[len - 1] = '\n';
                else
                    len--;
            }
            buf[len] = '\0';

            /* backslash substitution */
            if (flags & PDC_FILE_BSSUBST)
            {
                len = (size_t) pdc_subst_backslash(pdc, (pdc_byte *) buf,
                                          (int) len, NULL, pdc_bytes, pdc_true);
            }

            /* concatenate line */
            strcat(&content[is], buf);

            sumlen += len;
        }

        if (!strlist) pdc_free(pdc, content);
    }

    if (nlines)
        pdc_logg_cond(pdc, 2, trc_filesearch,
            "\t\tLine %d; \"%s\"\n", nlines, strlist[nlines - 1]);

    *linelist = strlist;
    return nlines;
}


/* generate a temporary file name from the current time, pid, 'dirname',
** and the data in 'inbuf' using MD5. prepend 'dirname' to the file name.
** the result is written to 'outbuf'. if 'outbuf' is NULL, memory will be
** allocated and must be freed by the caller. otherwise, 'pdc' can be set
** to NULL.
**
** if 'dirname' isn't specified the function looks for an environment
** variable via the define PDC_TMPDIR_ENV. This define is set in
** pc_config.h. If the environment variable has a value and if the
** directory exists (check with the temporary file together) the
** directory will be used.
*/

#ifdef MVS
#define TMP_NAME_LEN	9
#define TMP_SUFFIX	""
#define TMP_SUFF_LEN	0
#else
#define TMP_NAME_LEN	14
#define TMP_SUFFIX	".TMP"
#define TMP_SUFF_LEN	4
#endif

char *
pdc_temppath(
    pdc_core *pdc,
    char *outbuf,
    const char *inbuf,
    size_t inlen,
    const char *dirname)
{
    char		name[TMP_NAME_LEN + TMP_SUFF_LEN + 1];
    MD5_CTX             md5;
    time_t              timer;
    unsigned char       digest[MD5_DIGEST_LENGTH];
    int                 i;
    size_t              dirlen;
#ifdef VMS
    char               *tmpdir = NULL;
#endif /* VMS */

#if defined(WIN32)
#if defined(__BORLANDC__) || defined(__WATCOMC__)
    int pid = getpid();
#else
    int pid = _getpid();
#endif
#else
#if !defined(MAC)
    pid_t pid = getpid();
#endif
#endif

#ifdef PDC_TMPDIR_ENV
    if (!dirname)
    {
        dirname = (char *) getenv(PDC_TMPDIR_ENV);
    }
#endif /* !PDC_TMPDIR_ENV */

    time(&timer);

    MD5_Init(&md5);
#if !defined(MAC)
    MD5_Update(&md5, (unsigned char *) &pid, sizeof pid);
#endif
    MD5_Update(&md5, (unsigned char *) &timer, sizeof timer);

    if (inlen == 0 && inbuf != (const char *) 0)
	inlen = strlen(inbuf);

    if (inlen != 0)
	MD5_Update(&md5, (unsigned char *) inbuf, (unsigned int) inlen );

    dirlen = dirname ? strlen(dirname) : 0;
    if (dirlen)
	MD5_Update(&md5, (const unsigned char *) dirname, (unsigned int) dirlen);

    MD5_Final(digest, &md5);

    for (i = 0; i < TMP_NAME_LEN - 1; ++i)
        name[i] = (char) (PDF_A + digest[i % MD5_DIGEST_LENGTH] % 26);

    name[i] = 0;
    strcat(name, TMP_SUFFIX);

    if (!outbuf)
        outbuf = pdc_file_fullname_mem(pdc, dirname, name);
    else
        pdc_file_fullname(pdc, dirname, name, outbuf);
    return outbuf;
}

/* Write function depending on pdc->asciifile.
 */
size_t
pdc_fwrite_ascii(pdc_core *pdc, const char *str, size_t len, FILE *fp)
{

    (void) pdc;
    len = pdc__fwrite(str, 1, len, fp);


    return len;
}

/* Creates a file depending on PDC_FILE_ASCII and pdc->asciifile.
*/
size_t
pdc_write_file(
    pdc_core *pdc,
    const char *filename,
    const char *qualifier,
    const char *content,
    size_t len,
    int flags)
{
    size_t wlen = 0;
    pdc_file *sfp;


    sfp = pdc_fopen(pdc, filename, qualifier, NULL, 0, flags);
    if (sfp != NULL)
    {
        wlen = pdc_fwrite_ascii(pdc, content, len, sfp->fp);
        if (wlen < len)
        {
            pdc_set_fwrite_errmsg(pdc, filename);
            PDC_RETHROW(pdc);
        }
        pdc_fclose(sfp);
    }


    return wlen;
}




