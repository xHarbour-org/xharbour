/* PDFlib GmbH cvsid:
 * $Id$ */

/*
 * Copyright (c) 1988-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

/* Pacify MSVS2005 and above */
#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

/*
 * TIFF Library UNIX-specific Routines. These are should also work with the
 * Windows Common RunTime Library.
 */
#include "pdflite_tiffconf.h"

#include <stdlib.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef HAVE_IO_H
# include <io.h>
#endif

#include "pdflite_tiffiop.h"
#include "pc_config.h"

static tsize_t
_tiffReadProc(void* fd, tdata_t buf, tsize_t size)
{
	return ((tsize_t) fread(buf, 1, (size_t) size, (FILE *)fd));
}

static tsize_t
_tiffWriteProc(void* fd, tdata_t buf, tsize_t size)
{
    return ((tsize_t) fwrite(buf, 1, (size_t) size, (FILE *)fd));
}

static toff_t
_tiffSeekProc(void* fd, toff_t off, int whence)
{
    toff_t retval;
    retval = fseek((FILE *)fd, (long) off, whence);
    if (retval == 0)
    {
        return (ftell((FILE *)fd));
    }
    else
    {
        return (toff_t)(-1);
    }

    /* return ((toff_t) fseek((FILE *)fd, (long) off, whence));*/
}

static int
_tiffCloseProc(void* fd)
{
        return (fclose((FILE *)fd));
}

static toff_t
_tiffSizeProc(void* fd)
{
#if defined(MVS) && defined(I370)
#define PDF_FTELL_BUSIZE        32000

	char buf[PDF_FTELL_BUSIZE];
	size_t filelen = 0;

        fseek((FILE *)fd, 0, SEEK_SET);

        while (!feof((FILE *)fd))
                filelen += fread((void *) buf, 1, PDF_FTELL_BUSIZE, (FILE *)fd);

	return filelen;
#else
        fseek((FILE *)fd, 0, SEEK_END);
        return (toff_t) ftell((FILE *)fd);
#endif
}

#ifdef HAVE_MMAP
#include <sys/mman.h>

static int
_tiffMapProc(void* fd, tdata_t* pbase, toff_t* psize)
{
        toff_t size = _tiffSizeProc((FILE *)fd);
	if (size != (toff_t) -1) {
		*pbase = (tdata_t)
                    mmap(0, size, PROT_READ, MAP_SHARED, (FILE *) fd, 0);
		if (*pbase != (tdata_t) -1) {
			*psize = size;
			return (1);
		}
	}
	return (0);
}

static void
_tiffUnmapProc(void* fd, tdata_t base, toff_t size)
{
	(void) fd;
	(void) munmap(base, (off_t) size);
}
#else /* !HAVE_MMAP */
static int
_tiffMapProc(void* fd, tdata_t* pbase, toff_t* psize)
{
	(void) fd; (void) pbase; (void) psize;
	return (0);
}

static void
_tiffUnmapProc(void* fd, tdata_t base, toff_t size)
{
	(void) fd; (void) base; (void) size;
}
#endif /* !HAVE_MMAP */

/*
 * Open a TIFF file descriptor for read/writing.
 */
TIFF*
TIFFFdOpen(void* fd, const char* name, const char* mode, void* pdflib_opaque,
        TIFFmallocHandler malloc_h, TIFFreallocHandler realloc_h,
        TIFFfreeHandler free_h, TIFFErrorHandler error_h,
        TIFFErrorHandler warn_h)
{
        TIFF* tif;

        tif = TIFFClientOpen(name, mode, fd,
            _tiffReadProc, _tiffWriteProc,
            _tiffSeekProc, _tiffCloseProc, _tiffSizeProc,
            _tiffMapProc, _tiffUnmapProc, pdflib_opaque,
            malloc_h, realloc_h, free_h, error_h, warn_h);
        if (tif)
                tif->tif_fd = (FILE *)fd;
        return (tif);
}

/*
 * Open a TIFF file for read/writing.
 */
TIFF*
TIFFOpen(const char* name, const char* mode, void* pdflib_opaque,
        TIFFmallocHandler malloc_h, TIFFreallocHandler realloc_h,
        TIFFfreeHandler free_h, TIFFErrorHandler error_h,
        TIFFErrorHandler warn_h)
{
        static const char module[] = "TIFFOpen";
        FILE *fd;
	TIFF *tif = NULL;
	int m;

	m = _TIFFgetMode(mode, module);
	if (m == -1)
		return ((TIFF*)0);

	if (m == O_RDONLY) {
	    fd = fopen(name, READBMODE);
	} else {
	    fd = fopen(name, WRITEMODE);
	}

        if (fd == NULL) {
                _TIFFError(tif, module, "%s: Cannot open", name);
                return ((TIFF *)0);
        }
        tif = TIFFFdOpen(fd, name, mode, pdflib_opaque,
                        malloc_h, realloc_h, free_h, error_h, warn_h);
	if(!tif)
		fclose(fd);
	return tif;
}

#ifdef __WIN32__
#include <windows.h>
/*
 * Open a TIFF file with a Unicode filename, for read/writing.
 */
TIFF*
TIFFOpenW(const wchar_t* name, const char* mode, void* pdflib_opaque,
        TIFFmallocHandler malloc_h, TIFFreallocHandler realloc_h,
        TIFFfreeHandler free_h, TIFFErrorHandler error_h,
        TIFFErrorHandler warn_h)
{
	static const char module[] = "TIFFOpenW";
	FILE *fd;
	int mbsize;
	char *mbname;
	TIFF* tif = (TIFF*) 0;
	int m;

	m = _TIFFgetMode(mode, module);
	if (m == -1)
		return ((TIFF*)0);

	if (m == O_RDONLY) {
	    fd = _wfopen(name, (unsigned short const *)READBMODE);
	} else {
	    fd = _wfopen(name, (unsigned short const *)WRITEMODE);
	}
        if (fd == NULL) {
		_TIFFError(tif, module, "%s: Cannot open", name);
		return ((TIFF *)0);
	}

	mbname = NULL;
	mbsize = WideCharToMultiByte(CP_ACP, 0, name, -1, NULL, 0, NULL, NULL);
	if (mbsize > 0) {
		mbname = _TIFFmalloc(mbsize);
		if (!mbname) {
			_TIFFError(tif, module,
			"Can't allocate space for filename conversion buffer");
			return ((TIFF*)0);
		}

		WideCharToMultiByte(CP_ACP, 0, name, -1, mbname, mbsize,
				    NULL, NULL);
	}

        tif = TIFFFdOpen(fd, (mbname != NULL) ? mbname : "<unknown>",
			mode, pdflib_opaque,
                        malloc_h, realloc_h, free_h, error_h, warn_h);
	
	_TIFFfree(mbname);
	
	if(!tif)
		fclose(fd);
	return tif;
}
#endif

void*
TIFFmalloc(TIFF* tif, tsize_t s)
{
        if (tif->pdflib_malloc != NULL)
            return (tif->pdflib_malloc(tif, (size_t) s));
        else
            return (malloc((size_t) s));
}

void
TIFFfree(TIFF* tif, tdata_t p)
{
        if (tif->pdflib_free != NULL)
            tif->pdflib_free(tif, p);
        else
            free(p);
}

void*
TIFFrealloc(TIFF* tif, tdata_t p, tsize_t s)
{
        if (tif->pdflib_realloc != NULL)
            return (tif->pdflib_realloc(tif, p, (size_t) s));
        else
            return (realloc(p, (size_t) s));
}

void
_TIFFmemset(tdata_t p, int v, tsize_t c)
{
	memset(p, v, (size_t) c);
}

void
_TIFFmemcpy(tdata_t d, const tdata_t s, tsize_t c)
{
	memcpy(d, s, (size_t) c);
}

int
_TIFFmemcmp(const tdata_t p1, const tdata_t p2, tsize_t c)
{
	return (memcmp(p1, p2, (size_t) c));
}

static void
unixWarningHandler(TIFF* tif, const char* module, const char* fmt, va_list ap)
{
	(void) tif;

	if (module != NULL)
		fprintf(stderr, "%s: ", module);
	fprintf(stderr, "Warning, ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, ".\n");
}
TIFFErrorHandler _TIFFwarningHandler = unixWarningHandler;

static void
unixErrorHandler(TIFF* tif, const char* module, const char* fmt, va_list ap)
{
	(void) tif;

	if (module != NULL)
		fprintf(stderr, "%s: ", module);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, ".\n");
}
TIFFErrorHandler _TIFFerrorHandler = unixErrorHandler;
