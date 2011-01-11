#################################################################
#                                                               #
#         Makefile for Pelles 32-bit C runtime library.         #
#                                                               #
#            Copyright (c) 1999-2002, Pelle Orinius.            #
#                                                               #
#################################################################

#PELLESCDIR = c:\progra~1\pellesc

!IFDEF __CE__
OUT = ceobjs
!ELSEIFDEF __MT__
OUT = mtobjs
!ELSE
OUT = objs
!ENDIF

crtbase = ..

!IFDEF __CE__
cflags = -D_WINCE -TARM-COFF
aflags =
!ELSEIFDEF __MT__
cflags = -MT -Ze
aflags = -define:__MT__
!ELSE
cflags =
aflags =
!ENDIF

cflags = $(cflags) -DNDEBUG -Zl -Ot -W1 -X -I$(crtbase)\include -I"$(PELLESCDIR)\INCLUDE\WIN"
aflags = $(aflags) -format:coff

cc = pocc $(cflags)
lib = polib
as = $(crtbase)\utils\poasm $(aflags)

.SUFFIXES: .c .asm .obj

{}.c{$(OUT)}.obj:
    $(cc) -Fo$(OUT)\%|fF.obj $<

!IFDEF __CE__
{wince}.asm{$(OUT)}.obj:
    $(as) -out:$(OUT)\%|fF.obj $< -include:$(crtbase)\src\wince
!ELSE
{win32}.c{$(OUT)}.obj:
    $(cc) -Fo$(OUT)\%|fF.obj $< -Ze
{win32}.asm{$(OUT)}.obj:
    $(as) -out:$(OUT)\%|fF.obj $< -include:$(crtbase)\src\win32
!ENDIF

FLTOBJS = \
    $(OUT)\acos.obj \
    $(OUT)\acosf.obj \
    $(OUT)\acosl.obj \
    $(OUT)\acosh.obj \
    $(OUT)\acoshf.obj \
    $(OUT)\acoshl.obj \
    $(OUT)\asin.obj \
    $(OUT)\asinf.obj \
    $(OUT)\asinl.obj \
    $(OUT)\asinh.obj \
    $(OUT)\asinhf.obj \
    $(OUT)\asinhl.obj \
    $(OUT)\atan.obj \
    $(OUT)\atanf.obj \
    $(OUT)\atanl.obj \
    $(OUT)\atanh.obj \
    $(OUT)\atanhf.obj \
    $(OUT)\atanhl.obj \
    $(OUT)\atan2.obj \
    $(OUT)\atan2f.obj \
    $(OUT)\atan2l.obj \
    $(OUT)\ceil.obj \
    $(OUT)\ceilf.obj \
    $(OUT)\ceill.obj \
    $(OUT)\copysign.obj \
    $(OUT)\copysignf.obj \
    $(OUT)\copysignl.obj \
    $(OUT)\cos.obj \
    $(OUT)\cosf.obj \
    $(OUT)\cosl.obj \
    $(OUT)\exp.obj \
    $(OUT)\expf.obj \
    $(OUT)\expl.obj \
    $(OUT)\exp2.obj \
    $(OUT)\exp2f.obj \
    $(OUT)\exp2l.obj \
    $(OUT)\expm1.obj \
    $(OUT)\expm1f.obj \
    $(OUT)\expm1l.obj \
    $(OUT)\fabs.obj \
    $(OUT)\fabsf.obj \
    $(OUT)\fabsl.obj \
    $(OUT)\fdim.obj \
    $(OUT)\fdimf.obj \
    $(OUT)\fdiml.obj \
    $(OUT)\floor.obj \
    $(OUT)\floorf.obj \
    $(OUT)\floorl.obj \
    $(OUT)\fma.obj \
    $(OUT)\fmaf.obj \
    $(OUT)\fmal.obj \
    $(OUT)\fmax.obj \
    $(OUT)\fmaxf.obj \
    $(OUT)\fmaxl.obj \
    $(OUT)\fmin.obj \
    $(OUT)\fminf.obj \
    $(OUT)\fminl.obj \
    $(OUT)\fmod.obj \
    $(OUT)\fmodf.obj \
    $(OUT)\fmodl.obj \
    $(OUT)\hypot.obj \
    $(OUT)\hypotf.obj \
    $(OUT)\hypotl.obj \
    $(OUT)\ilogb.obj \
    $(OUT)\ilogbf.obj \
    $(OUT)\ilogbl.obj \
    $(OUT)\llrint.obj \
    $(OUT)\llrintf.obj \
    $(OUT)\llrintl.obj \
    $(OUT)\log.obj \
    $(OUT)\logf.obj \
    $(OUT)\logl.obj \
    $(OUT)\log10.obj \
    $(OUT)\log10f.obj \
    $(OUT)\log10l.obj \
    $(OUT)\log1p.obj \
    $(OUT)\log1pf.obj \
    $(OUT)\log1pl.obj \
    $(OUT)\log2.obj \
    $(OUT)\log2f.obj \
    $(OUT)\log2l.obj \
    $(OUT)\logb.obj \
    $(OUT)\logbf.obj \
    $(OUT)\logbl.obj \
    $(OUT)\lrint.obj \
    $(OUT)\lrintf.obj \
    $(OUT)\lrintl.obj \
    $(OUT)\remainder.obj \
    $(OUT)\remainderf.obj \
    $(OUT)\remainderl.obj \
    $(OUT)\rint.obj \
    $(OUT)\rintf.obj \
    $(OUT)\rintl.obj \
    $(OUT)\scalbn.obj \
    $(OUT)\scalbnf.obj \
    $(OUT)\scalbnl.obj \
    $(OUT)\sin.obj \
    $(OUT)\sinf.obj \
    $(OUT)\sinl.obj \
    $(OUT)\sqrt.obj \
    $(OUT)\sqrtf.obj \
    $(OUT)\sqrtl.obj \
    $(OUT)\tan.obj \
    $(OUT)\tanf.obj \
    $(OUT)\tanl.obj

!IFDEF __CE__
crtce.lib: \
!ELSEIFDEF __MT__
crtmt.lib: \
!ELSE
crt.lib: \
!ENDIF
    $(OUT)\abort.obj \
    $(OUT)\abs.obj \
    $(OUT)\asctime.obj \
    $(OUT)\atexit.obj \
    $(OUT)\atof.obj \
    $(OUT)\atoi.obj \
    $(OUT)\atol.obj \
    $(OUT)\atoll.obj \
    $(OUT)\bsearch.obj \
    $(OUT)\btowc.obj \
    $(OUT)\calloc.obj \
    $(OUT)\cbrt.obj \
    $(OUT)\cbrtf.obj \
    $(OUT)\cbrtl.obj \
    $(OUT)\clearerr.obj \
    $(OUT)\cosh.obj \
    $(OUT)\coshf.obj \
    $(OUT)\coshl.obj \
    $(OUT)\ctime.obj \
    $(OUT)\difftime.obj \
    $(OUT)\div.obj \
    $(OUT)\erf.obj \
    $(OUT)\erfc.obj \
    $(OUT)\erfcf.obj \
    $(OUT)\erfcl.obj \
    $(OUT)\erff.obj \
    $(OUT)\erfl.obj \
    $(OUT)\errno.obj \
    $(OUT)\exit.obj \
    $(OUT)\fclose.obj \
    $(OUT)\feof.obj \
    $(OUT)\ferror.obj \
    $(OUT)\fflush.obj \
    $(OUT)\fgetc.obj \
    $(OUT)\fgetpos.obj \
    $(OUT)\fgets.obj \
    $(OUT)\fgetwc.obj \
    $(OUT)\fgetws.obj \
    $(OUT)\fopen.obj \
    $(OUT)\fprintf.obj \
    $(OUT)\fputc.obj \
    $(OUT)\fputs.obj \
    $(OUT)\fputwc.obj \
    $(OUT)\fputws.obj \
    $(OUT)\fread.obj \
    $(OUT)\free.obj \
    $(OUT)\freopen.obj \
    $(OUT)\frexp.obj \
    $(OUT)\frexpf.obj \
    $(OUT)\frexpl.obj \
    $(OUT)\fscanf.obj \
    $(OUT)\fseek.obj \
    $(OUT)\fsetpos.obj \
    $(OUT)\ftell.obj \
    $(OUT)\fwide.obj \
    $(OUT)\fwprintf.obj \
    $(OUT)\fwrite.obj \
    $(OUT)\fwscanf.obj \
    $(OUT)\getc.obj \
    $(OUT)\getchar.obj \
    $(OUT)\getenv.obj \
    $(OUT)\gets.obj \
    $(OUT)\getwc.obj \
    $(OUT)\getwchar.obj \
    $(OUT)\gmtime.obj \
    $(OUT)\imaxabs.obj \
    $(OUT)\imaxdiv.obj \
    $(OUT)\isalnum.obj \
    $(OUT)\isalpha.obj \
    $(OUT)\isblank.obj \
    $(OUT)\iscntrl.obj \
    $(OUT)\isdigit.obj \
    $(OUT)\isgraph.obj \
    $(OUT)\islower.obj \
    $(OUT)\isprint.obj \
    $(OUT)\ispunct.obj \
    $(OUT)\isspace.obj \
    $(OUT)\isupper.obj \
    $(OUT)\iswalnum.obj \
    $(OUT)\iswalpha.obj \
    $(OUT)\iswblank.obj \
    $(OUT)\iswcntrl.obj \
    $(OUT)\iswctype.obj \
    $(OUT)\iswdigit.obj \
    $(OUT)\iswgraph.obj \
    $(OUT)\iswlower.obj \
    $(OUT)\iswprint.obj \
    $(OUT)\iswpunct.obj \
    $(OUT)\iswspace.obj \
    $(OUT)\iswupper.obj \
    $(OUT)\iswxdigit.obj \
    $(OUT)\isxdigit.obj \
    $(OUT)\labs.obj \
    $(OUT)\ldexp.obj \
    $(OUT)\ldexpf.obj \
    $(OUT)\ldexpl.obj \
    $(OUT)\ldiv.obj \
    $(OUT)\lgamma.obj \
    $(OUT)\lgammaf.obj \
    $(OUT)\lgammal.obj \
    $(OUT)\llabs.obj \
    $(OUT)\lldiv.obj \
    $(OUT)\llround.obj \
    $(OUT)\llroundf.obj \
    $(OUT)\llroundl.obj \
    $(OUT)\localeconv.obj \
    $(OUT)\localtime.obj \
    $(OUT)\lround.obj \
    $(OUT)\lroundf.obj \
    $(OUT)\lroundl.obj \
    $(OUT)\malloc.obj \
    $(OUT)\mblen.obj \
    $(OUT)\mbrlen.obj \
    $(OUT)\mbrtowc.obj \
    $(OUT)\mbsinit.obj \
    $(OUT)\mbsrtowc.obj \
    $(OUT)\mbstowcs.obj \
    $(OUT)\mbtowc.obj \
    $(OUT)\memchr.obj \
    $(OUT)\memcmp.obj \
    $(OUT)\memcpy.obj \
    $(OUT)\memmove.obj \
    $(OUT)\memset.obj \
    $(OUT)\mktime.obj \
    $(OUT)\modf.obj \
    $(OUT)\modff.obj \
    $(OUT)\modfl.obj \
    $(OUT)\msize.obj \
    $(OUT)\nan.obj \
    $(OUT)\nanf.obj \
    $(OUT)\nanl.obj \
    $(OUT)\nearbyint.obj \
    $(OUT)\nearbyintf.obj \
    $(OUT)\nearbyintl.obj \
    $(OUT)\nextafter.obj \
    $(OUT)\nextafterf.obj \
    $(OUT)\nextafterl.obj \
    $(OUT)\nexttoward.obj \
    $(OUT)\nexttowardf.obj \
    $(OUT)\nexttowardl.obj \
    $(OUT)\perror.obj \
    $(OUT)\pow.obj \
    $(OUT)\powf.obj \
    $(OUT)\powl.obj \
    $(OUT)\printf.obj \
    $(OUT)\prof.obj \
    $(OUT)\putc.obj \
    $(OUT)\putchar.obj \
    $(OUT)\puts.obj \
    $(OUT)\putwc.obj \
    $(OUT)\putwchar.obj \
    $(OUT)\qsort.obj \
    $(OUT)\rand.obj \
    $(OUT)\realloc.obj \
    $(OUT)\remove.obj \
    $(OUT)\remquo.obj \
    $(OUT)\remquof.obj \
    $(OUT)\remquol.obj \
    $(OUT)\rewind.obj \
    $(OUT)\round.obj \
    $(OUT)\roundf.obj \
    $(OUT)\roundl.obj \
    $(OUT)\scalbln.obj \
    $(OUT)\scalblnf.obj \
    $(OUT)\scalblnl.obj \
    $(OUT)\scanf.obj \
    $(OUT)\setbuf.obj \
    $(OUT)\setlocale.obj \
    $(OUT)\setvbuf.obj \
    $(OUT)\sinh.obj \
    $(OUT)\sinhf.obj \
    $(OUT)\sinhl.obj \
    $(OUT)\snprintf.obj \
    $(OUT)\sprintf.obj \
    $(OUT)\srand.obj \
    $(OUT)\sscanf.obj \
    $(OUT)\strcat.obj \
    $(OUT)\strchr.obj \
    $(OUT)\strcmp.obj \
    $(OUT)\strcoll.obj \
    $(OUT)\strcpy.obj \
    $(OUT)\strcspn.obj \
    $(OUT)\strdup.obj \
    $(OUT)\strerror.obj \
    $(OUT)\strftime.obj \
    $(OUT)\stricmp.obj \
    $(OUT)\strlen.obj \
    $(OUT)\strlwr.obj \
    $(OUT)\strncat.obj \
    $(OUT)\strncmp.obj \
    $(OUT)\strncpy.obj \
    $(OUT)\strnicmp.obj \
    $(OUT)\strpbrk.obj \
    $(OUT)\strrchr.obj \
    $(OUT)\strspn.obj \
    $(OUT)\strstr.obj \
    $(OUT)\strtod.obj \
    $(OUT)\strtof.obj \
    $(OUT)\strtoimax.obj \
    $(OUT)\strtok.obj \
    $(OUT)\strtol.obj \
    $(OUT)\strtold.obj \
    $(OUT)\strtoll.obj \
    $(OUT)\strtoul.obj \
    $(OUT)\strtoull.obj \
    $(OUT)\strtoumax.obj \
    $(OUT)\strupr.obj \
    $(OUT)\strxfrm.obj \
    $(OUT)\swprintf.obj \
    $(OUT)\swscanf.obj \
    $(OUT)\tanh.obj \
    $(OUT)\tanhf.obj \
    $(OUT)\tanhl.obj \
    $(OUT)\tgamma.obj \
    $(OUT)\tgammaf.obj \
    $(OUT)\tgammal.obj \
    $(OUT)\tmpfile.obj \
    $(OUT)\tolower.obj \
    $(OUT)\toupper.obj \
    $(OUT)\towctrans.obj \
    $(OUT)\towlower.obj \
    $(OUT)\towupper.obj \
    $(OUT)\trunc.obj \
    $(OUT)\truncf.obj \
    $(OUT)\truncl.obj \
    $(OUT)\ungetc.obj \
    $(OUT)\ungetwc.obj \
    $(OUT)\verify.obj \
    $(OUT)\vfprintf.obj \
    $(OUT)\vfscanf.obj \
    $(OUT)\vfwprintf.obj \
    $(OUT)\vfwscanf.obj \
    $(OUT)\vprintf.obj \
    $(OUT)\vscanf.obj \
    $(OUT)\vsnprintf.obj \
    $(OUT)\vsprintf.obj \
    $(OUT)\vsscanf.obj \
    $(OUT)\vswprintf.obj \
    $(OUT)\vswscanf.obj \
    $(OUT)\vwprintf.obj \
    $(OUT)\vwscanf.obj \
    $(OUT)\wcrtomb.obj \
    $(OUT)\wcscat.obj \
    $(OUT)\wcschr.obj \
    $(OUT)\wcscmp.obj \
    $(OUT)\wcscoll.obj \
    $(OUT)\wcscpy.obj \
    $(OUT)\wcscspn.obj \
    $(OUT)\wcsdup.obj \
    $(OUT)\wcsftime.obj \
    $(OUT)\wcsicmp.obj \
    $(OUT)\wcslen.obj \
    $(OUT)\wcslwr.obj \
    $(OUT)\wcsncat.obj \
    $(OUT)\wcsncmp.obj \
    $(OUT)\wcsncpy.obj \
    $(OUT)\wcsnicmp.obj \
    $(OUT)\wcspbrk.obj \
    $(OUT)\wcsrchr.obj \
    $(OUT)\wcsrtomb.obj \
    $(OUT)\wcsspn.obj \
    $(OUT)\wcsstr.obj \
    $(OUT)\wcstod.obj \
    $(OUT)\wcstof.obj \
    $(OUT)\wcstoimax.obj \
    $(OUT)\wcstok.obj \
    $(OUT)\wcstol.obj \
    $(OUT)\wcstold.obj \
    $(OUT)\wcstoll.obj \
    $(OUT)\wcstombs.obj \
    $(OUT)\wcstoul.obj \
    $(OUT)\wcstoull.obj \
    $(OUT)\wcstoumax.obj \
    $(OUT)\wcsupr.obj \
    $(OUT)\wcsxfrm.obj \
    $(OUT)\wctob.obj \
    $(OUT)\wctomb.obj \
    $(OUT)\wctrans.obj \
    $(OUT)\wctype.obj \
    $(OUT)\wmemchr.obj \
    $(OUT)\wmemcmp.obj \
    $(OUT)\wmemcpy.obj \
    $(OUT)\wmemmove.obj \
    $(OUT)\wmemset.obj \
    $(OUT)\wprintf.obj \
    $(OUT)\wscanf.obj \
    $(OUT)\_assert.obj \
    $(OUT)\_atan.obj \
    $(OUT)\_atanf.obj \
    $(OUT)\_atanl.obj \
    $(OUT)\_atexit.obj \
    $(OUT)\_btowc.obj \
    $(OUT)\_cosh.obj \
    $(OUT)\_coshf.obj \
    $(OUT)\_coshl.obj \
    $(OUT)\_ctype.obj \
    $(OUT)\_erfc.obj \
    $(OUT)\_erfcf.obj \
    $(OUT)\_erfcl.obj \
    $(OUT)\_exp.obj \
    $(OUT)\_expf.obj \
    $(OUT)\_expl.obj \
    $(OUT)\_feraise.obj \
    $(OUT)\_fvalues.obj \
    $(OUT)\_hypotf.obj \
    $(OUT)\_files.obj \
    $(OUT)\_logf.obj \
    $(OUT)\_fopen.obj \
    $(OUT)\_fpclass.obj \
    $(OUT)\_fpclassf.obj \
    $(OUT)\_fpclassl.obj \
    $(OUT)\_fpcomp.obj \
    $(OUT)\_fpcompf.obj \
    $(OUT)\_fpcompl.obj \
    $(OUT)\_fpint.obj \
    $(OUT)\_fpintf.obj \
    $(OUT)\_fpintl.obj \
    $(OUT)\_fpnorm.obj \
    $(OUT)\_fpnormf.obj \
    $(OUT)\_fpnorml.obj \
    $(OUT)\_fppow10.obj \
    $(OUT)\_fppow10f.obj \
    $(OUT)\_fppow10l.obj \
    $(OUT)\_fpscale.obj \
    $(OUT)\_fpscalef.obj \
    $(OUT)\_fpscalel.obj \
    $(OUT)\_fpsign.obj \
    $(OUT)\_fpsignf.obj \
    $(OUT)\_fpsignl.obj \
    $(OUT)\_fptest.obj \
    $(OUT)\_fptestf.obj \
    $(OUT)\_fptestl.obj \
    $(OUT)\_fpunscale.obj \
    $(OUT)\_fpunscalef.obj \
    $(OUT)\_fpunscalel.obj \
    $(OUT)\_fread.obj \
    $(OUT)\_fslot.obj \
    $(OUT)\_fwrite.obj \
    $(OUT)\_genld.obj \
    $(OUT)\_gentime.obj \
    $(OUT)\_getdst.obj \
    $(OUT)\_getfield.obj \
    $(OUT)\_getflt.obj \
    $(OUT)\_getint.obj \
    $(OUT)\_getlocale.obj \
    $(OUT)\_getstr.obj \
    $(OUT)\_gettime.obj \
    $(OUT)\_getzone.obj \
    $(OUT)\_hypot.obj \
    $(OUT)\_hypotl.obj \
    $(OUT)\_isdst.obj \
    $(OUT)\_ldtob.obj \
    $(OUT)\_lgamma.obj \
    $(OUT)\_lgammaf.obj \
    $(OUT)\_lgammal.obj \
    $(OUT)\_litob.obj \
    $(OUT)\_log.obj \
    $(OUT)\_logl.obj \
    $(OUT)\_lvalues.obj \
    $(OUT)\_matherr.obj \
    $(OUT)\_mbtowc.obj \
    $(OUT)\_poly.obj \
    $(OUT)\_polyf.obj \
    $(OUT)\_polyl.obj \
    $(OUT)\_printf.obj \
    $(OUT)\_putfield.obj \
    $(OUT)\_putstr.obj \
    $(OUT)\_puttxt.obj \
    $(OUT)\_scanf.obj \
    $(OUT)\_setlocale.obj \
    $(OUT)\_sin.obj \
    $(OUT)\_sinf.obj \
    $(OUT)\_sinl.obj \
    $(OUT)\_sinh.obj \
    $(OUT)\_sinhf.obj \
    $(OUT)\_sinhl.obj \
    $(OUT)\_state.obj \
    $(OUT)\_stod.obj \
    $(OUT)\_stof.obj \
    $(OUT)\_stoflt.obj \
    $(OUT)\_stold.obj \
    $(OUT)\_stoll.obj \
    $(OUT)\_stopfx.obj \
    $(OUT)\_stoul.obj \
    $(OUT)\_stoull.obj \
    $(OUT)\_stoxflt.obj \
    $(OUT)\_strerror.obj \
    $(OUT)\_strftime.obj \
    $(OUT)\_strxfrm.obj \
    $(OUT)\_tgamma.obj \
    $(OUT)\_tgammaf.obj \
    $(OUT)\_tgammal.obj \
    $(OUT)\_tolowertab.obj \
    $(OUT)\_touppertab.obj \
    $(OUT)\_towctrans.obj \
    $(OUT)\_ttotm.obj \
    $(OUT)\_values.obj \
    $(OUT)\_wcsftime.obj \
    $(OUT)\_wcstod.obj \
    $(OUT)\_wcstof.obj \
    $(OUT)\_wcstold.obj \
    $(OUT)\_wcstoll.obj \
    $(OUT)\_wcstoul.obj \
    $(OUT)\_wcstoull.obj \
    $(OUT)\_wcsxfrm.obj \
    $(OUT)\_wctob.obj \
    $(OUT)\_wctomb.obj \
    $(OUT)\_wctranstab.obj \
    $(OUT)\_wctype.obj \
    $(OUT)\_wctypetab.obj \
    $(OUT)\_wfread.obj \
    $(OUT)\_wfwrite.obj \
    $(OUT)\_wgenld.obj \
    $(OUT)\_wgetfield.obj \
    $(OUT)\_wgetflt.obj \
    $(OUT)\_wgetint.obj \
    $(OUT)\_wgetstr.obj \
    $(OUT)\_wldtob.obj \
    $(OUT)\_wlitob.obj \
    $(OUT)\_wprintf.obj \
    $(OUT)\_wputfield.obj \
    $(OUT)\_wputstr.obj \
    $(OUT)\_wputtxt.obj \
    $(OUT)\_wscanf.obj \
    $(OUT)\_wstoflt.obj \
    $(OUT)\_wstopfx.obj \
    $(OUT)\_wstoxflt.obj \
    $(OUT)\_exit.obj \
    $(OUT)\access.obj \
    $(OUT)\chdir.obj \
    $(OUT)\chdrive.obj \
    $(OUT)\chmod.obj \
    $(OUT)\chsize.obj \
    $(OUT)\clock.obj \
    $(OUT)\close.obj \
    $(OUT)\commit.obj \
    $(OUT)\creat.obj \
    $(OUT)\dup.obj \
    $(OUT)\dup2.obj \
    $(OUT)\eof.obj \
    $(OUT)\feclearexcept.obj \
    $(OUT)\fegetenv.obj \
    $(OUT)\fegetexceptflag.obj \
    $(OUT)\fegetround.obj \
    $(OUT)\feholdexcept.obj \
    $(OUT)\feraiseexcept.obj \
    $(OUT)\fesetenv.obj \
    $(OUT)\fesetexceptflag.obj \
    $(OUT)\fesetround.obj \
    $(OUT)\fetestexcept.obj \
    $(OUT)\feupdateenv.obj \
    $(OUT)\fileno.obj \
    $(OUT)\findfile.obj \
    $(OUT)\flength.obj \
    $(OUT)\fullpath.obj \
    $(OUT)\getch.obj \
    $(OUT)\getcwd.obj \
    $(OUT)\getdrive.obj \
    $(OUT)\isatty.obj \
    $(OUT)\locking.obj \
    $(OUT)\lseek.obj \
    $(OUT)\mkdir.obj \
    $(OUT)\ms.obj \
    $(OUT)\open.obj \
    $(OUT)\pipe.obj \
    $(OUT)\putch.obj \
    $(OUT)\raise.obj \
    $(OUT)\read.obj \
    $(OUT)\rename.obj \
    $(OUT)\rmdir.obj \
    $(OUT)\searchenv.obj \
    $(OUT)\setmode.obj \
    $(OUT)\signal.obj \
    $(OUT)\spawnl.obj \
    $(OUT)\spawnle.obj \
    $(OUT)\spawnlp.obj \
    $(OUT)\spawnlpe.obj \
    $(OUT)\spawnv.obj \
    $(OUT)\spawnve.obj \
    $(OUT)\spawnvp.obj \
    $(OUT)\spawnvpe.obj \
    $(OUT)\splitpath.obj \
    $(OUT)\stat.obj \
    $(OUT)\system.obj \
    $(OUT)\tell.obj \
    $(OUT)\thread.obj \
    $(OUT)\threadex.obj \
    $(OUT)\time.obj \
    $(OUT)\tmpnam.obj \
    $(OUT)\unlink.obj \
    $(OUT)\wait.obj \
    $(OUT)\write.obj \
    $(OUT)\_xcptfilter.obj \
    $(OUT)\_crt0.obj \
    $(OUT)\_dllcrt0.obj \
    $(OUT)\_envarg.obj \
    $(OUT)\_fgetpos.obj \
    $(OUT)\__fopen.obj \
    $(OUT)\_fsetpos.obj \
    $(OUT)\_ftime.obj \
    $(OUT)\_getmem.obj \
    $(OUT)\_getpath.obj \
    $(OUT)\_ioinit.obj \
    $(OUT)\_maperr.obj \
    $(OUT)\_mtlock.obj \
    $(OUT)\_osfinfo.obj \
    $(OUT)\_setargv.obj \
    $(OUT)\_setenvp.obj \
    $(OUT)\_spawn.obj \
    $(OUT)\_tidtable.obj \
    $(OUT)\_tzone.obj \
    $(OUT)\_wincmdln.obj \
    $(OUT)\longjmp.obj \
    $(OUT)\seh1.obj \
    $(OUT)\seh2.obj \
    $(OUT)\setjmp.obj \
    $(OUT)\_chkstk.obj \
    $(OUT)\_initex.obj \
    $(OUT)\_ftol.obj \
    $(OUT)\_ftoll.obj \
    $(OUT)\_llcmp.obj \
    $(OUT)\_lldiv.obj \
    $(OUT)\_llmod.obj \
    $(OUT)\_llmul.obj \
    $(OUT)\_llshl.obj \
    $(OUT)\_llshr.obj \
    $(OUT)\_ulldiv.obj \
    $(OUT)\_ullmod.obj \
    $(OUT)\_ullshr.obj \
    $(OUT)\_wincrt0.obj \
    $(FLTOBJS)
    $(lib) -out:$@ @<<
$**
<<

############################################################################
# Generic modules.
#
$(OUT)\abort.obj:               abort.c
$(OUT)\abs.obj:                 abs.c
$(OUT)\acos.obj:                acos.c xxdftype.h xxacos.h
$(OUT)\acosf.obj:               acosf.c xxfftype.h xxacos.h
$(OUT)\acosl.obj:               acosl.c xxlftype.h xxacos.h
$(OUT)\acosh.obj:               acosh.c xxdftype.h xxacosh.h
$(OUT)\acoshf.obj:              acoshf.c xxfftype.h xxacosh.h
$(OUT)\acoshl.obj:              acoshl.c xxlftype.h xxacosh.h
$(OUT)\asctime.obj:             asctime.c
$(OUT)\asin.obj:                asin.c xxdftype.h xxasin.h
$(OUT)\asinf.obj:               asinf.c xxfftype.h xxasin.h
$(OUT)\asinl.obj:               asinl.c xxlftype.h xxasin.h
$(OUT)\asinh.obj:               asinh.c xxdftype.h xxasinh.h
$(OUT)\asinhf.obj:              asinhf.c xxfftype.h xxasinh.h
$(OUT)\asinhl.obj:              asinhl.c xxlftype.h xxasinh.h
$(OUT)\atan.obj:                atan.c xxdftype.h xxatan.h
$(OUT)\atanf.obj:               atanf.c xxfftype.h xxatan.h
$(OUT)\atanl.obj:               atanl.c xxlftype.h xxatan.h
$(OUT)\atan2.obj:               atan2.c xxdftype.h xxatan2.h
$(OUT)\atan2f.obj:              atan2f.c xxfftype.h xxatan2.h
$(OUT)\atan2l.obj:              atan2l.c xxlftype.h xxatan2.h
$(OUT)\atanh.obj:               atanh.c xxdftype.h xxatanh.h
$(OUT)\atanhf.obj:              atanhf.c xxfftype.h xxatanh.h
$(OUT)\atanhl.obj:              atanhl.c xxlftype.h xxatanh.h
$(OUT)\atexit.obj:              atexit.c
$(OUT)\atof.obj:                atof.c
$(OUT)\atoi.obj:                atoi.c
$(OUT)\atol.obj:                atol.c
$(OUT)\atoll.obj:               atoll.c
$(OUT)\bsearch.obj:             bsearch.c
$(OUT)\btowc.obj:               btowc.c
$(OUT)\calloc.obj:              calloc.c
$(OUT)\cbrt.obj:                cbrt.c xxdftype.h xxcbrt.h
$(OUT)\cbrtf.obj:               cbrtf.c xxfftype.h xxcbrt.h
$(OUT)\cbrtl.obj:               cbrtl.c xxlftype.h xxcbrt.h
$(OUT)\ceil.obj:                ceil.c xxdftype.h xxceil.h
$(OUT)\ceilf.obj:               ceilf.c xxfftype.h xxceil.h
$(OUT)\ceill.obj:               ceill.c xxlftype.h xxceil.h
$(OUT)\clearerr.obj:            clearerr.c
$(OUT)\copysign.obj:            copysign.c
$(OUT)\copysignf.obj:           copysignf.c
$(OUT)\copysignl.obj:           copysignl.c
$(OUT)\cos.obj:                 cos.c xxdftype.h xxcos.h
$(OUT)\cosf.obj:                cosf.c xxfftype.h xxcos.h
$(OUT)\cosl.obj:                cosl.c xxlftype.h xxcos.h
$(OUT)\cosh.obj:                cosh.c xxdftype.h xxcosh.h
$(OUT)\coshf.obj:               coshf.c xxfftype.h xxcosh.h
$(OUT)\coshl.obj:               coshl.c xxlftype.h xxcosh.h
$(OUT)\ctime.obj:               ctime.c
$(OUT)\difftime.obj:            difftime.c
$(OUT)\div.obj:                 div.c
$(OUT)\erf.obj:                 erf.c xxdftype.h xxerf.h
$(OUT)\erfc.obj:                erfc.c xxdftype.h xxerfc.h
$(OUT)\erfcf.obj:               erfcf.c xxfftype.h xxerfc.h
$(OUT)\erfcl.obj:               erfcl.c xxlftype.h xxerfc.h
$(OUT)\erff.obj:                erff.c xxfftype.h xxerf.h
$(OUT)\erfl.obj:                erfl.c xxlftype.h xxerf.h
$(OUT)\errno.obj:               errno.c
$(OUT)\exit.obj:                exit.c
$(OUT)\exp.obj:                 exp.c xxdftype.h xxexp.h
$(OUT)\expf.obj:                expf.c xxfftype.h xxexp.h
$(OUT)\expl.obj:                expl.c xxlftype.h xxexp.h
$(OUT)\exp2.obj:                exp2.c xxdftype.h xxexp2.h
$(OUT)\exp2f.obj:               exp2f.c xxfftype.h xxexp2.h
$(OUT)\exp2l.obj:               exp2l.c xxlftype.h xxexp2.h
$(OUT)\expm1.obj:               expm1.c xxdftype.h xxexpm1.h
$(OUT)\expm1f.obj:              expm1f.c xxfftype.h xxexpm1.h
$(OUT)\expm1l.obj:              expm1l.c xxlftype.h xxexpm1.h
$(OUT)\fabs.obj:                fabs.c xxdftype.h xxfabs.h
$(OUT)\fabsf.obj:               fabsf.c xxfftype.h xxfabs.h
$(OUT)\fabsl.obj:               fabsl.c xxlftype.h xxfabs.h
$(OUT)\fclose.obj:              fclose.c
$(OUT)\fdim.obj:                fdim.c xxdftype.h xxfdim.h
$(OUT)\fdimf.obj:               fdimf.c xxfftype.h xxfdim.h
$(OUT)\fdiml.obj:               fdiml.c xxlftype.h xxfdim.h
$(OUT)\feof.obj:                feof.c
$(OUT)\ferror.obj:              ferror.c
$(OUT)\fflush.obj:              fflush.c
$(OUT)\fgetc.obj:               fgetc.c
$(OUT)\fgetpos.obj:             fgetpos.c
$(OUT)\fgets.obj:               fgets.c
$(OUT)\fgetwc.obj:              fgetwc.c
$(OUT)\fgetws.obj:              fgetws.c
$(OUT)\floor.obj:               floor.c xxdftype.h xxfloor.h
$(OUT)\floorf.obj:              floorf.c xxfftype.h xxfloor.h
$(OUT)\floorl.obj:              floorl.c xxlftype.h xxfloor.h
$(OUT)\fma.obj:                 fma.c xxdftype.h xxfma.h
$(OUT)\fmaf.obj:                fmaf.c xxfftype.h xxfma.h
$(OUT)\fmal.obj:                fmal.c xxlftype.h xxfma.h
$(OUT)\fmax.obj:                fmax.c xxdftype.h xxfmax.h
$(OUT)\fmaxf.obj:               fmaxf.c xxfftype.h xxfmax.h
$(OUT)\fmaxl.obj:               fmaxl.c xxlftype.h xxfmax.h
$(OUT)\fmin.obj:                fmin.c xxdftype.h xxfmin.h
$(OUT)\fminf.obj:               fminf.c xxfftype.h xxfmin.h
$(OUT)\fminl.obj:               fminl.c xxlftype.h xxfmin.h
$(OUT)\fmod.obj:                fmod.c xxdftype.h xxfmod.h
$(OUT)\fmodf.obj:               fmodf.c xxfftype.h xxfmod.h
$(OUT)\fmodl.obj:               fmodl.c xxlftype.h xxfmod.h
$(OUT)\fopen.obj:               fopen.c
$(OUT)\fprintf.obj:             fprintf.c
$(OUT)\fputc.obj:               fputc.c
$(OUT)\fputs.obj:               fputs.c
$(OUT)\fputwc.obj:              fputwc.c
$(OUT)\fputws.obj:              fputws.c
$(OUT)\fread.obj:               fread.c
$(OUT)\free.obj:                free.c
$(OUT)\freopen.obj:             freopen.c
$(OUT)\frexp.obj:               frexp.c xxdftype.h xxfrexp.h
$(OUT)\frexpf.obj:              frexpf.c xxfftype.h xxfrexp.h
$(OUT)\frexpl.obj:              frexpl.c xxlftype.h xxfrexp.h
$(OUT)\fscanf.obj:              fscanf.c
$(OUT)\fseek.obj:               fseek.c
$(OUT)\fsetpos.obj:             fsetpos.c
$(OUT)\ftell.obj:               ftell.c
$(OUT)\fwide.obj:               fwide.c
$(OUT)\fwprintf.obj:            fwprintf.c
$(OUT)\fwrite.obj:              fwrite.c
$(OUT)\fwscanf.obj:             fwscanf.c
$(OUT)\getc.obj:                getc.c
$(OUT)\getchar.obj:             getchar.c
$(OUT)\getenv.obj:              getenv.c
$(OUT)\gets.obj:                gets.c
$(OUT)\getwc.obj:               getwc.c
$(OUT)\getwchar.obj:            getwchar.c
$(OUT)\gmtime.obj:              gmtime.c
$(OUT)\hypot.obj:               hypot.c xxdftype.h xxhypot.h
$(OUT)\hypotf.obj:              hypotf.c xxfftype.h xxhypot.h
$(OUT)\hypotl.obj:              hypotl.c xxlftype.h xxhypot.h
$(OUT)\ilogb.obj:               ilogb.c xxdftype.h xxilogb.h
$(OUT)\ilogbf.obj:              ilogbf.c xxfftype.h xxilogb.h
$(OUT)\ilogbl.obj:              ilogbl.c xxlftype.h xxilogb.h
$(OUT)\imaxabs.obj:             imaxabs.c
$(OUT)\imaxdiv.obj:             imaxdiv.c
$(OUT)\isalnum.obj:             isalnum.c
$(OUT)\isalpha.obj:             isalpha.c
$(OUT)\isblank.obj:             isblank.c
$(OUT)\iscntrl.obj:             iscntrl.c
$(OUT)\isdigit.obj:             isdigit.c
$(OUT)\isgraph.obj:             isgraph.c
$(OUT)\islower.obj:             islower.c
$(OUT)\isprint.obj:             isprint.c
$(OUT)\ispunct.obj:             ispunct.c
$(OUT)\isspace.obj:             isspace.c
$(OUT)\isupper.obj:             isupper.c
$(OUT)\iswalnum.obj:            iswalnum.c
$(OUT)\iswalpha.obj:            iswalpha.c
$(OUT)\iswblank.obj:            iswblank.c
$(OUT)\iswcntrl.obj:            iswcntrl.c
$(OUT)\iswctype.obj:            iswctype.c
$(OUT)\iswdigit.obj:            iswdigit.c
$(OUT)\iswgraph.obj:            iswgraph.c
$(OUT)\iswlower.obj:            iswlower.c
$(OUT)\iswprint.obj:            iswprint.c
$(OUT)\iswpunct.obj:            iswpunct.c
$(OUT)\iswspace.obj:            iswspace.c
$(OUT)\iswupper.obj:            iswupper.c
$(OUT)\iswxdigit.obj:           iswxdigit.c
$(OUT)\isxdigit.obj:            isxdigit.c
$(OUT)\labs.obj:                labs.c
$(OUT)\ldexp.obj:               ldexp.c xxdftype.h xxldexp.h
$(OUT)\ldexpf.obj:              ldexpf.c xxfftype.h xxldexp.h
$(OUT)\ldexpl.obj:              ldexpl.c xxlftype.h xxldexp.h
$(OUT)\ldiv.obj:                ldiv.c
$(OUT)\lgamma.obj:              lgamma.c xxdftype.h xxlgamma.h
$(OUT)\lgammaf.obj:             lgammaf.c xxfftype.h xxlgamma.h
$(OUT)\lgammal.obj:             lgammal.c xxlftype.h xxlgamma.h
$(OUT)\llabs.obj:               llabs.c
$(OUT)\lldiv.obj:               lldiv.c
$(OUT)\llrint.obj:              llrint.c xxdftype.h xxllrint.h
$(OUT)\llrintf.obj:             llrintf.c xxfftype.h xxllrint.h
$(OUT)\llrintl.obj:             llrintl.c xxlftype.h xxllrint.h
$(OUT)\llround.obj:             llround.c xxdftype.h xxllround.h
$(OUT)\llroundf.obj:            llroundf.c xxfftype.h xxllround.h
$(OUT)\llroundl.obj:            llroundl.c xxlftype.h xxllround.h
$(OUT)\localeconv.obj:          localeconv.c
$(OUT)\localtime.obj:           localtime.c
$(OUT)\log.obj:                 log.c xxdftype.h xxlog.h
$(OUT)\logf.obj:                logf.c xxfftype.h xxlog.h
$(OUT)\logl.obj:                logl.c xxlftype.h xxlog.h
$(OUT)\log10.obj:               log10.c xxdftype.h xxlog10.h
$(OUT)\log10f.obj:              log10f.c xxfftype.h xxlog10.h
$(OUT)\log10l.obj:              log10l.c xxlftype.h xxlog10.h
$(OUT)\log1p.obj:               log1p.c xxdftype.h xxlog1p.h
$(OUT)\log1pf.obj:              log1pf.c xxfftype.h xxlog1p.h
$(OUT)\log1pl.obj:              log1pl.c xxlftype.h xxlog1p.h
$(OUT)\log2.obj:                log2.c xxdftype.h xxlog2.h
$(OUT)\log2f.obj:               log2f.c xxfftype.h xxlog2.h
$(OUT)\log2l.obj:               log2l.c xxlftype.h xxlog2.h
$(OUT)\logb.obj:                logb.c xxdftype.h xxlogb.h
$(OUT)\logbf.obj:               logbf.c xxfftype.h xxlogb.h
$(OUT)\logbl.obj:               logbl.c xxlftype.h xxlogb.h
$(OUT)\lrint.obj:               lrint.c xxdftype.h xxlrint.h
$(OUT)\lrintf.obj:              lrintf.c xxfftype.h xxlrint.h
$(OUT)\lrintl.obj:              lrintl.c xxlftype.h xxlrint.h
$(OUT)\lround.obj:              lround.c xxdftype.h xxlround.h
$(OUT)\lroundf.obj:             lroundf.c xxfftype.h xxlround.h
$(OUT)\lroundl.obj:             lroundl.c xxlftype.h xxlround.h
$(OUT)\malloc.obj:              malloc.c
$(OUT)\mblen.obj:               mblen.c
$(OUT)\mbrlen.obj:              mbrlen.c
$(OUT)\mbrtowc.obj:             mbrtowc.c
$(OUT)\mbsinit.obj:             mbsinit.c
$(OUT)\mbsrtowc.obj:            mbsrtowc.c
$(OUT)\mbstowcs.obj:            mbstowcs.c
$(OUT)\mbtowc.obj:              mbtowc.c
$(OUT)\memchr.obj:              memchr.c
$(OUT)\memcmp.obj:              memcmp.c
$(OUT)\memcpy.obj:              memcpy.c
$(OUT)\memmove.obj:             memmove.c
$(OUT)\memset.obj:              memset.c
$(OUT)\mktime.obj:              mktime.c
$(OUT)\modf.obj:                modf.c xxdftype.h xxmodf.h
$(OUT)\modff.obj:               modff.c xxfftype.h xxmodf.h
$(OUT)\modfl.obj:               modfl.c xxlftype.h xxmodf.h
$(OUT)\msize.obj:               msize.c
$(OUT)\nan.obj:                 nan.c xxdftype.h xxnan.h
$(OUT)\nanf.obj:                nanf.c xxfftype.h xxnan.h
$(OUT)\nanl.obj:                nanl.c xxlftype.h xxnan.h
$(OUT)\nearbyint.obj:           nearbyint.c xxdftype.h xxnearbyint.h
$(OUT)\nearbyintf.obj:          nearbyintf.c xxfftype.h xxnearbyint.h
$(OUT)\nearbyintl.obj:          nearbyintl.c xxlftype.h xxnearbyint.h
$(OUT)\nextafter.obj:           nextafter.c xxdftype.h xxnextafter.h
$(OUT)\nextafterf.obj:          nextafterf.c xxfftype.h xxnextafter.h
$(OUT)\nextafterl.obj:          nextafterl.c xxlftype.h xxnextafter.h
$(OUT)\nexttoward.obj:          nexttoward.c
$(OUT)\nexttowardf.obj:         nexttowardf.c
$(OUT)\nexttowardl.obj:         nexttowardl.c
$(OUT)\perror.obj:              perror.c
$(OUT)\pow.obj:                 pow.c xxdftype.h xxpow.h
$(OUT)\powf.obj:                powf.c xxfftype.h xxpow.h
$(OUT)\powl.obj:                powl.c xxlftype.h xxpow.h
$(OUT)\printf.obj:              printf.c
$(OUT)\prof.obj:                prof.c
$(OUT)\putc.obj:                putc.c
$(OUT)\putchar.obj:             putchar.c
$(OUT)\puts.obj:                puts.c
$(OUT)\putwc.obj:               putwc.c
$(OUT)\putwchar.obj:            putwchar.c
$(OUT)\qsort.obj:               qsort.c
$(OUT)\rand.obj:                rand.c
$(OUT)\realloc.obj:             realloc.c
$(OUT)\remainder.obj:           remainder.c xxdftype.h xxremainder.h
$(OUT)\remainderf.obj:          remainderf.c xxfftype.h xxremainder.h
$(OUT)\remainderl.obj:          remainderl.c xxlftype.h xxremainder.h
$(OUT)\remove.obj:              remove.c
$(OUT)\remquo.obj:              remquo.c xxdftype.h xxremquo.h
$(OUT)\remquof.obj:             remquof.c xxfftype.h xxremquo.h
$(OUT)\remquol.obj:             remquol.c xxlftype.h xxremquo.h
$(OUT)\rewind.obj:              rewind.c
$(OUT)\rint.obj:                rint.c xxdftype.h xxrint.h
$(OUT)\rintf.obj:               rintf.c xxfftype.h xxrint.h
$(OUT)\rintl.obj:               rintl.c xxlftype.h xxrint.h
$(OUT)\round.obj:               round.c xxdftype.h xxround.h
$(OUT)\roundf.obj:              roundf.c xxfftype.h xxround.h
$(OUT)\roundl.obj:              roundl.c xxlftype.h xxround.h
$(OUT)\scalbln.obj:             scalbln.c xxdftype.h xxscalbln.h
$(OUT)\scalblnf.obj:            scalblnf.c xxfftype.h xxscalbln.h
$(OUT)\scalblnl.obj:            scalblnl.c xxlftype.h xxscalbln.h
$(OUT)\scalbn.obj:              scalbn.c xxdftype.h xxscalbn.h
$(OUT)\scalbnf.obj:             scalbnf.c xxfftype.h xxscalbn.h
$(OUT)\scalbnl.obj:             scalbnl.c xxlftype.h xxscalbn.h
$(OUT)\scanf.obj:               scanf.c
$(OUT)\setbuf.obj:              setbuf.c
$(OUT)\setlocale.obj:           setlocale.c
$(OUT)\setvbuf.obj:             setvbuf.c
$(OUT)\sin.obj:                 sin.c xxdftype.h xxsin.h
$(OUT)\sinf.obj:                sinf.c xxfftype.h xxsin.h
$(OUT)\sinl.obj:                sinl.c xxlftype.h xxsin.h
$(OUT)\sinh.obj:                sinh.c xxdftype.h xxsinh.h
$(OUT)\sinhf.obj:               sinhf.c xxfftype.h xxsinh.h
$(OUT)\sinhl.obj:               sinhl.c xxlftype.h xxsinh.h
$(OUT)\snprintf.obj:            snprintf.c
$(OUT)\sprintf.obj:             sprintf.c
$(OUT)\sqrt.obj:                sqrt.c xxdftype.h xxsqrt.h
$(OUT)\sqrtf.obj:               sqrtf.c xxfftype.h xxsqrt.h
$(OUT)\sqrtl.obj:               sqrtl.c xxlftype.h xxsqrt.h
$(OUT)\srand.obj:               srand.c
$(OUT)\sscanf.obj:              sscanf.c
$(OUT)\strcat.obj:              strcat.c
$(OUT)\strchr.obj:              strchr.c
$(OUT)\strcmp.obj:              strcmp.c
$(OUT)\strcoll.obj:             strcoll.c
$(OUT)\strcpy.obj:              strcpy.c
$(OUT)\strcspn.obj:             strcspn.c
$(OUT)\strdup.obj:              strdup.c
$(OUT)\strerror.obj:            strerror.c
$(OUT)\strftime.obj:            strftime.c
$(OUT)\stricmp.obj:             stricmp.c
$(OUT)\strlen.obj:              strlen.c
$(OUT)\strlwr.obj:              strlwr.c
$(OUT)\strncat.obj:             strncat.c
$(OUT)\strncmp.obj:             strncmp.c
$(OUT)\strncpy.obj:             strncpy.c
$(OUT)\strnicmp.obj:            strnicmp.c
$(OUT)\strpbrk.obj:             strpbrk.c
$(OUT)\strrchr.obj:             strrchr.c
$(OUT)\strspn.obj:              strspn.c
$(OUT)\strstr.obj:              strstr.c
$(OUT)\strtod.obj:              strtod.c
$(OUT)\strtof.obj:              strtof.c
$(OUT)\strtoimax.obj:           strtoimax.c
$(OUT)\strtok.obj:              strtok.c
$(OUT)\strtol.obj:              strtol.c
$(OUT)\strtold.obj:             strtold.c
$(OUT)\strtoll.obj:             strtoll.c
$(OUT)\strtoul.obj:             strtoul.c
$(OUT)\strtoull.obj:            strtoull.c
$(OUT)\strtoumax.obj:           strtoumax.c
$(OUT)\strupr.obj:              strupr.c
$(OUT)\strxfrm.obj:             strxfrm.c
$(OUT)\swprintf.obj:            swprintf.c
$(OUT)\swscanf.obj:             swscanf.c
$(OUT)\tan.obj:                 tan.c xxdftype.h xxtan.h
$(OUT)\tanf.obj:                tanf.c xxfftype.h xxtan.h
$(OUT)\tanl.obj:                tanl.c xxlftype.h xxtan.h
$(OUT)\tanh.obj:                tanh.c xxdftype.h xxtanh.h
$(OUT)\tanhf.obj:               tanhf.c xxfftype.h xxtanh.h
$(OUT)\tanhl.obj:               tanhl.c xxlftype.h xxtanh.h
$(OUT)\tgamma.obj:              tgamma.c xxdftype.h xxtgamma.h
$(OUT)\tgammaf.obj:             tgammaf.c xxfftype.h xxtgamma.h
$(OUT)\tgammal.obj:             tgammal.c xxlftype.h xxtgamma.h
$(OUT)\tmpfile.obj:             tmpfile.c
$(OUT)\tolower.obj:             tolower.c
$(OUT)\toupper.obj:             toupper.c
$(OUT)\towctrans.obj:           towctrans.c
$(OUT)\towlower.obj:            towlower.c
$(OUT)\towupper.obj:            towupper.c
$(OUT)\trunc.obj:               trunc.c xxdftype.h xxtrunc.h
$(OUT)\truncf.obj:              truncf.c xxfftype.h xxtrunc.h
$(OUT)\truncl.obj:              truncl.c xxlftype.h xxtrunc.h
$(OUT)\ungetc.obj:              ungetc.c
$(OUT)\ungetwc.obj:             ungetwc.c
$(OUT)\verify.obj:              verify.c
$(OUT)\vfprintf.obj:            vfprintf.c
$(OUT)\vfscanf.obj:             vfscanf.c
$(OUT)\vfwprintf.obj:           vfwprintf.c
$(OUT)\vfwscanf.obj:            vfwscanf.c
$(OUT)\vprintf.obj:             vprintf.c
$(OUT)\vscanf.obj:              vscanf.c
$(OUT)\vsnprintf.obj:           vsnprintf.c
$(OUT)\vsprintf.obj:            vsprintf.c
$(OUT)\vsscanf.obj:             vsscanf.c
$(OUT)\vswprintf.obj:           vswprintf.c
$(OUT)\vswscanf.obj:            vswscanf.c
$(OUT)\vwprintf.obj:            vwprintf.c
$(OUT)\vwscanf.obj:             vwscanf.c
$(OUT)\wcrtomb.obj:             wcrtomb.c
$(OUT)\wcscat.obj:              wcscat.c
$(OUT)\wcschr.obj:              wcschr.c
$(OUT)\wcscmp.obj:              wcscmp.c
$(OUT)\wcscoll.obj:             wcscoll.c
$(OUT)\wcscpy.obj:              wcscpy.c
$(OUT)\wcscspn.obj:             wcscspn.c
$(OUT)\wcsdup.obj:              wcsdup.c
$(OUT)\wcsftime.obj:            wcsftime.c
$(OUT)\wcsicmp.obj:             wcsicmp.c
$(OUT)\wcslen.obj:              wcslen.c
$(OUT)\wcslwr.obj:              wcslwr.c
$(OUT)\wcsncat.obj:             wcsncat.c
$(OUT)\wcsncmp.obj:             wcsncmp.c
$(OUT)\wcsncpy.obj:             wcsncpy.c
$(OUT)\wcsnicmp.obj:            wcsnicmp.c
$(OUT)\wcspbrk.obj:             wcspbrk.c
$(OUT)\wcsrchr.obj:             wcsrchr.c
$(OUT)\wcsrtomb.obj:            wcsrtomb.c
$(OUT)\wcsspn.obj:              wcsspn.c
$(OUT)\wcsstr.obj:              wcsstr.c
$(OUT)\wcstod.obj:              wcstod.c
$(OUT)\wcstof.obj:              wcstof.c
$(OUT)\wcstoimax.obj:           wcstoimax.c
$(OUT)\wcstok.obj:              wcstok.c
$(OUT)\wcstol.obj:              wcstol.c
$(OUT)\wcstold.obj:             wcstold.c
$(OUT)\wcstoll.obj:             wcstoll.c
$(OUT)\wcstombs.obj:            wcstombs.c
$(OUT)\wcstoul.obj:             wcstoul.c
$(OUT)\wcstoull.obj:            wcstoull.c
$(OUT)\wcstoumax.obj:           wcstoumax.c
$(OUT)\wcsupr.obj:              wcsupr.c
$(OUT)\wcsxfrm.obj:             wcsxfrm.c
$(OUT)\wctob.obj:               wctob.c
$(OUT)\wctomb.obj:              wctomb.c
$(OUT)\wctrans.obj:             wctrans.c
$(OUT)\wctype.obj:              wctype.c
$(OUT)\wmemchr.obj:             wmemchr.c
$(OUT)\wmemcmp.obj:             wmemcmp.c
$(OUT)\wmemcpy.obj:             wmemcpy.c
$(OUT)\wmemmove.obj:            wmemmove.c
$(OUT)\wmemset.obj:             wmemset.c
$(OUT)\wprintf.obj:             wprintf.c
$(OUT)\wscanf.obj:              wscanf.c
$(OUT)\_assert.obj:             _assert.c
$(OUT)\_atan.obj:               _atan.c xxdftype.h xxxatan.h
$(OUT)\_atanf.obj:              _atanf.c xxfftype.h xxxatan.h
$(OUT)\_atanl.obj:              _atanl.c xxlftype.h xxxatan.h
$(OUT)\_atexit.obj:             _atexit.c
$(OUT)\_btowc.obj:              _btowc.c
$(OUT)\_cosh.obj:               _cosh.c xxdftype.h xxxcosh.h
$(OUT)\_coshf.obj:              _coshf.c xxfftype.h xxxcosh.h
$(OUT)\_coshl.obj:              _coshl.c xxlftype.h xxxcosh.h
$(OUT)\_ctype.obj:              _ctype.c
$(OUT)\_erfc.obj:               _erfc.c xxdftype.h xxxerfc.h
$(OUT)\_erfcf.obj:              _erfcf.c xxfftype.h xxxerfc.h
$(OUT)\_erfcl.obj:              _erfcl.c xxlftype.h xxxerfc.h
$(OUT)\_exp.obj:                _exp.c xxdftype.h xxxexp.h
$(OUT)\_expf.obj:               _expf.c xxfftype.h xxxexp.h
$(OUT)\_expl.obj:               _expl.c xxlftype.h xxxexp.h
$(OUT)\_feraise.obj:            _feraise.c
$(OUT)\_fvalues.obj:            _fvalues.c
$(OUT)\_files.obj:              _files.c
$(OUT)\_fopen.obj:              _fopen.c
$(OUT)\_fpclass.obj:            _fpclass.c xxdftype.h xxxdclass.h
$(OUT)\_fpclassf.obj:           _fpclassf.c xxfftype.h xxxdclass.h
$(OUT)\_fpclassl.obj:           _fpclassl.c xxlftype.h xxxdclass.h
$(OUT)\_fpcomp.obj:             _fpcomp.c xxdftype.h xxxfpcomp.h
$(OUT)\_fpcompf.obj:            _fpcompf.c xxfftype.h xxxfpcomp.h
$(OUT)\_fpcompl.obj:            _fpcompl.c xxlftype.h xxxfpcomp.h
$(OUT)\_fpint.obj:              _fpint.c
$(OUT)\_fpintf.obj:             _fpintf.c
$(OUT)\_fpintl.obj:             _fpintl.c
$(OUT)\_fpnorm.obj:             _fpnorm.c
$(OUT)\_fpnormf.obj:            _fpnormf.c
$(OUT)\_fpnorml.obj:            _fpnorml.c
$(OUT)\_fppow10.obj:            _fppow10.c xxdftype.h xxxdtent.h
$(OUT)\_fppow10f.obj:           _fppow10f.c xxfftype.h xxxdtent.h
$(OUT)\_fppow10l.obj:           _fppow10l.c xxlftype.h xxxdtent.h
$(OUT)\_fpscale.obj:            _fpscale.c
$(OUT)\_fpscalef.obj:           _fpscalef.c
$(OUT)\_fpscalel.obj:           _fpscalel.c
$(OUT)\_fpsign.obj:             _fpsign.c xxdftype.h xxxdsign.h
$(OUT)\_fpsignf.obj:            _fpsignf.c xxfftype.h xxxdsign.h
$(OUT)\_fpsignl.obj:            _fpsignl.c xxlftype.h xxxdsign.h
$(OUT)\_fptest.obj:             _fptest.c
$(OUT)\_fptestf.obj:            _fptestf.c
$(OUT)\_fptestl.obj:            _fptestl.c
$(OUT)\_fpunscale.obj:          _fpunscale.c
$(OUT)\_fpunscalef.obj:         _fpunscalef.c
$(OUT)\_fpunscalel.obj:         _fpunscalel.c
$(OUT)\_fread.obj:              _fread.c
$(OUT)\_fslot.obj:              _fslot.c
$(OUT)\_fwrite.obj:             _fwrite.c
$(OUT)\_genld.obj:              _genld.c
$(OUT)\_gentime.obj:            _gentime.c
$(OUT)\_getdst.obj:             _getdst.c
$(OUT)\_getfield.obj:           _getfield.c
$(OUT)\_getflt.obj:             _getflt.c
$(OUT)\_getint.obj:             _getint.c
$(OUT)\_getlocale.obj:          _getlocale.c
$(OUT)\_getstr.obj:             _getstr.c
$(OUT)\_gettime.obj:            _gettime.c
$(OUT)\_getzone.obj:            _getzone.c
$(OUT)\_hypot.obj:              _hypot.c xxdftype.h xxxhypot.h
$(OUT)\_hypotf.obj:             _hypotf.c xxfftype.h xxxhypot.h
$(OUT)\_hypotl.obj:             _hypotl.c xxlftype.h xxxhypot.h
$(OUT)\_isdst.obj:              _isdst.c
$(OUT)\_ldtob.obj:              _ldtob.c
$(OUT)\_litob.obj:              _litob.c
$(OUT)\_lgamma.obj:             _lgamma.c xxdftype.h xxxlgamma.h
$(OUT)\_lgammaf.obj:            _lgammaf.c xxfftype.h xxxlgamma.h
$(OUT)\_lgammal.obj:            _lgammal.c xxlftype.h xxxlgamma.h
$(OUT)\_log.obj:                _log.c xxdftype.h xxxlog.h
$(OUT)\_logf.obj:               _logf.c xxfftype.h xxxlog.h
$(OUT)\_logl.obj:               _logl.c xxlftype.h xxxlog.h
$(OUT)\_lvalues.obj:            _lvalues.c
$(OUT)\_matherr.obj:            _matherr.c
$(OUT)\_mbtowc.obj:             _mbtowc.c
$(OUT)\_poly.obj:               _poly.c xxdftype.h xxxpoly.h
$(OUT)\_polyf.obj:              _polyf.c xxfftype.h xxxpoly.h
$(OUT)\_polyl.obj:              _polyl.c xxlftype.h xxxpoly.h
$(OUT)\_printf.obj:             _printf.c
$(OUT)\_putfield.obj:           _putfield.c
$(OUT)\_putstr.obj:             _putstr.c
$(OUT)\_puttxt.obj:             _puttxt.c
$(OUT)\_scanf.obj:              _scanf.c
$(OUT)\_setlocale.obj:          _setlocale.c
$(OUT)\_sin.obj:                _sin.c xxdftype.h xxxsin.h
$(OUT)\_sinf.obj:               _sinf.c xxfftype.h xxxsin.h
$(OUT)\_sinl.obj:               _sinl.c xxlftype.h xxxsin.h
$(OUT)\_sinh.obj:               _sinh.c xxdftype.h xxxsinh.h
$(OUT)\_sinhf.obj:              _sinhf.c xxfftype.h xxxsinh.h
$(OUT)\_sinhl.obj:              _sinhl.c xxlftype.h xxxsinh.h
$(OUT)\_state.obj:              _state.c
$(OUT)\_stod.obj:               _stod.c xxcctype.h xxdftype.h xxstod.h
$(OUT)\_stof.obj:               _stof.c xxcctype.h xxfftype.h xxstod.h
$(OUT)\_stoflt.obj:             _stoflt.c
$(OUT)\_stold.obj:              _stold.c xxcctype.h xxlftype.h xxstod.h
$(OUT)\_stoll.obj:              _stoll.c
$(OUT)\_stopfx.obj:             _stopfx.c
$(OUT)\_stoul.obj:              _stoul.c
$(OUT)\_stoull.obj:             _stoull.c
$(OUT)\_stoxflt.obj:            _stoxflt.c
$(OUT)\_strerror.obj:           _strerror.c
$(OUT)\_strftime.obj:           _strftime.c
$(OUT)\_strxfrm.obj:            _strxfrm.c
$(OUT)\_tgamma.obj:             _tgamma.c xxdftype.h xxxtgamma.h
$(OUT)\_tgammaf.obj:            _tgammaf.c xxfftype.h xxxtgamma.h
$(OUT)\_tgammal.obj:            _tgammal.c xxlftype.h xxxtgamma.h
$(OUT)\_tolowertab.obj:         _tolowertab.c
$(OUT)\_touppertab.obj:         _touppertab.c
$(OUT)\_towctrans.obj:          _towctrans.c
$(OUT)\_ttotm.obj:              _ttotm.c
$(OUT)\_values.obj:             _values.c
$(OUT)\_wcsftime.obj:           _wcsftime.c
$(OUT)\_wcstod.obj:             _wcstod.c xxwctype.h xxdftype.h xxstod.h
$(OUT)\_wcstof.obj:             _wcstof.c xxwctype.h xxfftype.h xxstod.h
$(OUT)\_wcstold.obj:            _wcstold.c xxwctype.h xxlftype.h xxstod.h
$(OUT)\_wcstoll.obj:            _wcstoll.c
$(OUT)\_wcstoul.obj:            _wcstoul.c
$(OUT)\_wcstoull.obj:           _wcstoull.c
$(OUT)\_wcsxfrm.obj:            _wcsxfrm.c
$(OUT)\_wctob.obj:              _wctob.c
$(OUT)\_wctomb.obj:             _wctomb.c
$(OUT)\_wctranstab.obj:         _wctranstab.c
$(OUT)\_wctype.obj:             _wctype.c
$(OUT)\_wctypetab.obj:          _wctypetab.c
$(OUT)\_wfread.obj:             _wfread.c
$(OUT)\_wfwrite.obj:            _wfwrite.c
$(OUT)\_wgenld.obj:             _wgenld.c
$(OUT)\_wgetfield.obj:          _wgetfield.c
$(OUT)\_wgetflt.obj:            _wgetflt.c
$(OUT)\_wgetint.obj:            _wgetint.c
$(OUT)\_wgetstr.obj:            _wgetstr.c
$(OUT)\_wldtob.obj:             _wldtob.c
$(OUT)\_wlitob.obj:             _wlitob.c
$(OUT)\_wprintf.obj:            _wprintf.c
$(OUT)\_wputfield.obj:          _wputfield.c
$(OUT)\_wputstr.obj:            _wputstr.c
$(OUT)\_wputtxt.obj:            _wputtxt.c
$(OUT)\_wscanf.obj:             _wscanf.c
$(OUT)\_wstoflt.obj:            _wstoflt.c
$(OUT)\_wstopfx.obj:            _wstopfx.c
$(OUT)\_wstoxflt.obj:           _wstoxflt.c

############################################################################
# Win32 version modules.
#
$(OUT)\_exit.obj:               win32\_exit.c
$(OUT)\access.obj:              win32\access.c
$(OUT)\chdir.obj:               win32\chdir.c
$(OUT)\chdrive.obj:             win32\chdrive.c
$(OUT)\chmod.obj:               win32\chmod.c
$(OUT)\chsize.obj:              win32\chsize.c
$(OUT)\clock.obj:               win32\clock.c
$(OUT)\close.obj:               win32\close.c
$(OUT)\commit.obj:              win32\commit.c
$(OUT)\creat.obj:               win32\creat.c
$(OUT)\dup.obj:                 win32\dup.c
$(OUT)\dup2.obj:                win32\dup2.c
$(OUT)\eof.obj:                 win32\eof.c
$(OUT)\feclearexcept.obj:       win32\feclearexcept.c
$(OUT)\fegetenv.obj:            win32\fegetenv.c
$(OUT)\fegetexceptflag.obj:     win32\fegetexceptflag.c
$(OUT)\fegetround.obj:          win32\fegetround.c
$(OUT)\feholdexcept.obj:        win32\feholdexcept.c
$(OUT)\feraiseexcept.obj:       win32\feraiseexcept.c
$(OUT)\fesetenv.obj:            win32\fesetenv.c
$(OUT)\fesetexceptflag.obj:     win32\fesetexceptflag.c
$(OUT)\fesetround.obj:          win32\fesetround.c
$(OUT)\fetestexcept.obj:        win32\fetestexcept.c
$(OUT)\feupdateenv.obj:         win32\feupdateenv.c
$(OUT)\fileno.obj:              win32\fileno.c
$(OUT)\findfile.obj:            win32\findfile.c
$(OUT)\flength.obj:             win32\flength.c
$(OUT)\fullpath.obj:            win32\fullpath.c
$(OUT)\getch.obj:               win32\getch.c
$(OUT)\getcwd.obj:              win32\getcwd.c
$(OUT)\getdrive.obj:            win32\getdrive.c
$(OUT)\isatty.obj:              win32\isatty.c
$(OUT)\locking.obj:             win32\locking.c
$(OUT)\lseek.obj:               win32\lseek.c
$(OUT)\mkdir.obj:               win32\mkdir.c
$(OUT)\ms.obj:                  win32\ms.c
$(OUT)\open.obj:                win32\open.c
$(OUT)\pipe.obj:                win32\pipe.c
$(OUT)\putch.obj:               win32\putch.c
$(OUT)\raise.obj:               win32\raise.c
$(OUT)\read.obj:                win32\read.c
$(OUT)\rename.obj:              win32\rename.c
$(OUT)\rmdir.obj:               win32\rmdir.c
$(OUT)\searchenv.obj:           win32\searchenv.c
$(OUT)\setmode.obj:             win32\setmode.c
$(OUT)\signal.obj:              win32\signal.c
$(OUT)\spawnl.obj:              win32\spawnl.c
$(OUT)\spawnle.obj:             win32\spawnle.c
$(OUT)\spawnlp.obj:             win32\spawnlp.c
$(OUT)\spawnlpe.obj:            win32\spawnlpe.c
$(OUT)\spawnv.obj:              win32\spawnv.c
$(OUT)\spawnve.obj:             win32\spawnve.c
$(OUT)\spawnvp.obj:             win32\spawnvp.c
$(OUT)\spawnvpe.obj:            win32\spawnvpe.c
$(OUT)\splitpath.obj:           win32\splitpath.c
$(OUT)\stat.obj:                win32\stat.c
$(OUT)\system.obj:              win32\system.c
$(OUT)\tell.obj:                win32\tell.c
$(OUT)\thread.obj:              win32\thread.c
$(OUT)\threadex.obj:            win32\threadex.c
$(OUT)\time.obj:                win32\time.c
$(OUT)\tmpnam.obj:              win32\tmpnam.c
$(OUT)\unlink.obj:              win32\unlink.c
$(OUT)\wait.obj:                win32\wait.c
$(OUT)\write.obj:               win32\write.c
$(OUT)\_xcptfilter.obj:         win32\_xcptfilter.c
$(OUT)\_crt0.obj:               win32\_crt0.c
$(OUT)\_dllcrt0.obj:            win32\_dllcrt0.c
$(OUT)\_envarg.obj:             win32\_envarg.c
$(OUT)\_fgetpos.obj:            win32\_fgetpos.c
$(OUT)\__fopen.obj:             win32\__fopen.c
$(OUT)\_fsetpos.obj:            win32\_fsetpos.c
$(OUT)\_ftime.obj:              win32\_ftime.c
$(OUT)\_getmem.obj:             win32\_getmem.c
$(OUT)\_getpath.obj:            win32\_getpath.c
$(OUT)\_ioinit.obj:             win32\_ioinit.c
$(OUT)\_maperr.obj:             win32\_maperr.c
$(OUT)\_mtlock.obj:             win32\_mtlock.c
$(OUT)\_osfinfo.obj:            win32\_osfinfo.c
$(OUT)\_setargv.obj:            win32\_setargv.c
$(OUT)\_setenvp.obj:            win32\_setenvp.c
$(OUT)\_spawn.obj:              win32\_spawn.c
$(OUT)\_tidtable.obj:           win32\_tidtable.c
$(OUT)\_tzone.obj:              win32\_tzone.c
$(OUT)\_wincmdln.obj:           win32\_wincmdln.c
$(OUT)\longjmp.obj:             win32\longjmp.asm
$(OUT)\seh1.obj:                win32\seh1.asm
$(OUT)\seh2.obj:                win32\seh2.asm
$(OUT)\setjmp.obj:              win32\setjmp.asm
$(OUT)\_chkstk.obj:             win32\_chkstk.asm
$(OUT)\_initex.obj:             win32\_initex.asm
$(OUT)\_ftol.obj:               win32\_ftol.asm
$(OUT)\_ftoll.obj:              win32\_ftoll.asm
$(OUT)\_llcmp.obj:              win32\_llcmp.asm
$(OUT)\_lldiv.obj:              win32\_lldiv.asm
$(OUT)\_llmod.obj:              win32\_llmod.asm
$(OUT)\_llmul.obj:              win32\_llmul.asm
$(OUT)\_llshl.obj:              win32\_llshl.asm
$(OUT)\_llshr.obj:              win32\_llshr.asm
$(OUT)\_ulldiv.obj:             win32\_ulldiv.asm
$(OUT)\_ullmod.obj:             win32\_ullmod.asm
$(OUT)\_ullshr.obj:             win32\_ullshr.asm
$(OUT)\_wincrt0.obj:            win32\_crt0.c
    $(cc) -Fo$(OUT)\_wincrt0.obj win32\_crt0.c -Ze -D__WINMAIN__

