/****************************************************************************
 *                                                                          *
 * File    : main.c                                                         *
 *                                                                          *
 * Purpose : Win32 Linker; main module.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           02-06-16  All messages moved to the resources.                 *
 *           03-10-07  Added support for /NOEXPOBJ and /NOIMPLIB.           *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <limits.h>

#include "link.h"

enum opttok
{
    C_NOTHING,
    C_ALIGN,
    C_BASE,
    C_DEBUG,
    C_DEBUGTYPE,
    C_DEFAULTLIB,
    C_DELAY,
    C_DELAYLOAD,
    C_DISALLOWLIB,
    C_DLL,
    C_ENTRY,
    C_EXPORT,
    C_FIXED,
    C_FORCE,
    C_HEAP,
    C_HELP,
    C_INCLUDE,
    C_LARGEADDRESSAWARE,
    C_LIBPATH,
    C_MACHINE,
    C_MAP,
    C_MAPINFO,
    C_MERGE,
    C_NODEFAULTLIB,
    C_NOENTRY,
    C_NOEXPOBJ,
    C_NOIMPLIB,
    C_NOLOGO,
    C_OLDIMPLIB,
    C_OPT,
    C_OUT,
    C_RELEASE,
    C_SECTION,
    C_STACK,
    C_STUB,
    C_SUBSYSTEM,
    C_SWAPRUN,
    C_TSAWARE,
    C_UNMANGLE,
    C_VERBOSE,
    C_VERSION,
    C_WS
};

struct opts
{
    enum opttok tok;
    char *name;
    bool_t has_args;
};

/* global options array */
static const struct opts opts[] =
{
    C_ALIGN,        "ALIGN",        TRUE,
    C_BASE,         "BASE",         TRUE,
    C_DEBUGTYPE,    "DEBUGTYPE",    TRUE,
    C_DEBUG,        "DEBUG",        FALSE,
    C_DEFAULTLIB,   "DEFAULTLIB",   TRUE,
    C_DELAYLOAD,    "DELAYLOAD",    TRUE,
    C_DELAY,        "DELAY",        TRUE,
    C_DISALLOWLIB,  "DISALLOWLIB",  TRUE,
    C_DLL,          "DLL",          FALSE,
    C_ENTRY,        "ENTRY",        TRUE,
    C_EXPORT,       "EXPORT",       TRUE,
    C_FIXED,        "FIXED",        FALSE,
    C_FORCE,        "FORCE",        TRUE,
    C_HEAP,         "HEAP",         TRUE,
    C_HELP,         "HELP",         FALSE,
    C_HELP,         "?",            FALSE,
    C_INCLUDE,      "INCLUDE",      TRUE,
    C_LARGEADDRESSAWARE, "LARGEADDRESSAWARE", FALSE,
    C_LIBPATH,      "LIBPATH",      TRUE,
    C_MACHINE,      "MACHINE",      TRUE,
    C_MAPINFO,      "MAPINFO",      TRUE,
    C_MAP,          "MAP",          FALSE,
    C_MERGE,        "MERGE",        TRUE,
    C_NODEFAULTLIB, "NODEFAULTLIB", FALSE,
    C_NOENTRY,      "NOENTRY",      FALSE,
    C_NOEXPOBJ,     "NOEXPOBJ",     FALSE,
    C_NOIMPLIB,     "NOIMPLIB",     FALSE,
    C_NOLOGO,       "NOLOGO",       FALSE,  /* undocumented; MS compatibility */
    C_OLDIMPLIB,    "OLDIMPLIB",    FALSE,
    C_OPT,          "OPT",          TRUE,
    C_OUT,          "OUT",          TRUE,
    C_RELEASE,      "RELEASE",      FALSE,
    C_SECTION,      "SECTION",      TRUE,
    C_STACK,        "STACK",        TRUE,
    C_STUB,         "STUB",         TRUE,
    C_SUBSYSTEM,    "SUBSYSTEM",    TRUE,
    C_SWAPRUN,      "SWAPRUN",      TRUE,
    C_TSAWARE,      "TSAWARE",      FALSE,
    C_UNMANGLE,     "UNMANGLE",     FALSE,
    C_VERBOSE,      "VERBOSE",      FALSE,
    C_VERSION,      "VERSION",      TRUE,
    C_WS,           "WS",           TRUE
};

int nerrs = 0;

FILEINFO *obj_file_list = NULL;
FILEINFO *lib_file_list = NULL;
FILEINFO *exe_file = NULL;
FILEINFO *map_file = NULL;
FILEINFO *exp_file = NULL;  /* export object */
FILEINFO *lib_file = NULL;  /* import library */
FILEINFO *stub_file = NULL;

long stub_size = 0;

LIBENTRY *lib_path_list = NULL;
MODENTRY *module_list = NULL;
SYMENTRY *public_list = NULL;
SCNENTRY *section_list = NULL;
ATTENTRY *attrib_list = NULL;
GRPENTRY *group_list = NULL;
RENENTRY *alias_list = NULL;
DLLENTRY *delay_list = NULL;

char *entry_point = NULL;
SYMENTRY *pub_entry = NULL;

size_t unresolved_count = 0;

EXPENTRY **export_list = NULL;
size_t export_count = 0;
size_t export_maxcount = 0;

SEGENTRY *seg_dbgdir = NULL;  /* debug directory */
SCNENTRY *scn_sym = NULL;     /* $$SYMBOLS section */
SCNENTRY *scn_type = NULL;    /* $$TYPES section */
SCNENTRY *scn_fpo = NULL;     /* frame pointer omission section */

/* Time stamp for output files */
time_t time_stamp;

/* Program options */
struct options options = {0};

#ifdef PRERELEASE
size_t public_count = 0;
size_t static_count = 0;
size_t tot_size = 0;
size_t cur_size = 0;
#endif

static HANDLE hThread;

/* Static function prototypes */
static void link_init_1(void);
static void link_init_2(void);
static enum opttok lookup_option(char **);
static bool_t add_libfile_to_list(const char *, bool_t);
static void add_file_to_list(FILEINFO **, const char *);
static void print_usage(void);
static BOOL WINAPI ctrl_handler(DWORD);

/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point, of course!                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-11-24  Support for debug info added.                        *
 *           99-02-13  Warn about unreferenced /DELAYLOAD libraries.        *
 *                                                                          *
 ****************************************************************************/

int __cdecl main(int argc, char **argv)
{
    DLLENTRY *dll;

    link_init_1();

    /* Process command line args */
    link_args(--argc, ++argv, FALSE);

    link_init_2();

    /* Process objects and archives */
    process_object_modules();
    process_archives();

    if (unresolved_count != 0)
        apperror(RCFATAL(ERROR_UNRESOLVED_COUNT), unresolved_count);

    for (dll = delay_list; dll != NULL; dll = dll->next)
        if (!dll->flags.referenced) apperror(RCWARNING(
            ERROR_UNKNOWN_DELAY_MODULE), dll->name, dll->name);

    /* Process export files */
    process_exports();

    /* Assign space for communals */
    post_process_common_symbols();

    /* Allocate debug directory */
    alloc_debug_directory();

    if (nerrs != 0)
        errorexit(1);

    /* Order and fixup sections */
    order_sections();
    fixup_relocations();

    if (nerrs != 0)
        errorexit(1);

    /* Write the executable image */
    write_executable_image();

    /* Write the map file */
    write_map_image();

    if (nerrs != 0)
        errorexit(1);

#ifdef PRERELEASE
    dump_public_table();
    printf("Max allocation: %u\n", tot_size);
    printf("Number of publics: %u\n", public_count);
    printf("Number of statics: %u\n", static_count);
#endif

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: link_init_1                                                    *
 *                                                                          *
 * Purpose : Program initialization; part 1.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-09-10  Variable file alignment option added.                *
 *           99-08-14  Thread handle duplication added.                     *
 *                                                                          *
 ****************************************************************************/

static void link_init_1(void)
{
    /* Make a copy of the thread handle, for ctrl_handler() */
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
        GetCurrentProcess(), &hThread, 0, FALSE, DUPLICATE_SAME_ACCESS))
        apperror(RCFATAL(ERROR_INTERNAL), "link_init_1");

    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    options.machine = MACHINE_UNKNOWN;

    options.major_os_version = options.major_subsystem_version = 4;
    options.minor_os_version = options.minor_subsystem_version = 0;

    options.stack_reserve = DEFAULT_STACK_RESERVE;
    options.stack_commit = DEFAULT_STACK_COMMIT;

    options.heap_reserve = DEFAULT_HEAP_RESERVE;
    options.heap_commit = DEFAULT_HEAP_COMMIT;

    options.section_alignment = DEFAULT_ALIGNMENT;
    options.file_alignment = FILE_ALIGNMENT_WIN32;
    options.image_base = DEFAULT_IMAGE_BASE_EXE;
    options.subsystem = COFF_SS_WINCUI;

    options.debug = options.debug_cv = options.debug_coff = FALSE;
    options.fixed = TRUE;   /* default from 2.0 */
    options.old_implib = FALSE;  /* default from 2.1 */

    options.delay_bind = TRUE;
    options.delay_unload = FALSE;

    options.discard_unreferenced = TRUE;

    options.no_export_object = FALSE;
    options.no_import_archive = FALSE;

    /* start search in current dir */
    lookup_libpath(".");

    time_stamp = time(NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: link_init_2                                                    *
 *                                                                          *
 * Purpose : Program initialization; part 2.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-12  Change default image base for a DLL.                 *
 *           98-12-05  Update debugging flags.                              *
 *           00-11-16  Change default image base for a Windows CE EXE.      *
 *           02-11-17  Change default image base for a WinCE DLL.           *
 *                                                                          *
 ****************************************************************************/

static void link_init_2(void)
{
    if (!exe_file)
    {
        char fname[MAX_PATH];

        if (!obj_file_list)
            apperror(RCFATAL(ERROR_NO_OUTPUT_FILE));

        strcpy(fname, obj_file_list->name);
        update_extension_in_file(fname, (options.DLL) ? EXT_DLL : EXT_EXE);
        lookup_file(&exe_file, fname);
    }

    /* Set image base, if not specified by the user */
    if (options.subsystem == COFF_SS_WCEGUI && options.DLL && options.image_base == DEFAULT_IMAGE_BASE_EXE)
        options.image_base = DEFAULT_IMAGE_BASE_DLL_WINCE;
    else if (options.DLL && options.image_base == DEFAULT_IMAGE_BASE_EXE)
        options.image_base = DEFAULT_IMAGE_BASE_DLL;
    else if (options.subsystem == COFF_SS_WCEGUI && options.image_base == DEFAULT_IMAGE_BASE_EXE)
        options.image_base = DEFAULT_IMAGE_BASE_EXE_WINCE;

    if (!options.debug)
        options.debug_cv = options.debug_coff = FALSE;
    else if (!options.debug_cv && !options.debug_coff)
        options.debug_cv = TRUE;

    /* Calculate stub size (guard page) */
    stub_size = ROUNDUP((stub_file ? stub_file->size : 0x40), INTEL_PAGE_SIZE);

#ifdef PRERELEASE
    printf("ignore_libraries = %d\n", options.ignore_libraries);
    printf("verbose = %d\n", options.verbose);
    printf("DLL = %d\n", options.DLL);
    printf("mapfile = %d\n", options.mapfile);
    printf("mapfile_fixups = %d\n", options.mapfile_fixups);
    printf("mapfile_exports = %d\n", options.mapfile_exports);
    printf("mapfile_lines = %d\n", options.mapfile_lines);
    printf("debug = %d\n", options.debug);
    printf("debug_coff = %d\n", options.debug_coff);
    printf("debug_cv = %d\n", options.debug_cv);
    printf("swaprun_net = %d\n", options.swaprun_net);
    printf("swaprun_cd = %d\n", options.swaprun_cd);
    printf("aggressive_ws_trim = %d\n", options.aggressive_ws_trim);
    printf("fixed = %d\n", options.fixed);
    printf("no_entry = %d\n", options.no_entry);
    printf("old_implib = %d\n", options.old_implib);
    printf("delay_bind = %d\n", options.delay_bind);
    printf("delay_unload = %d\n", options.delay_unload);
    printf("discard_unreferenced = %d\n", options.discard_unreferenced);
    printf("force_multiple = %d\n", options.force_multiple);
    printf("unmangle_names = %d\n", options.unmangle_names);
    printf("large_address_aware = %d\n", options.large_address_aware);
    printf("terminal_server_aware = %d\n", options.terminal_server_aware);
    printf("no_export_object = %d\n", options.no_export_object);
    printf("no_import_archive = %d\n", options.no_import_archive);
    printf("stack_reserve = 0x%lX\n", options.stack_reserve);
    printf("stack_commit = 0x%lX\n", options.stack_commit);
    printf("heap_reserve = 0x%lX\n", options.heap_reserve);
    printf("heap_commit = 0x%lX\n", options.heap_commit);
    printf("section_alignment = 0x%lX\n", options.section_alignment);
    printf("file_alignment = 0x%lX\n", options.file_alignment);
    printf("image_base = 0x%lX\n", options.image_base);
    printf("os_version = %hu.%hu\n", options.major_os_version, options.minor_os_version);
    printf("subsystem_version = %hu.%hu\n", options.major_subsystem_version, options.minor_subsystem_version);
    printf("subsystem = %hu\n\n", options.subsystem);
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: link_args                                                      *
 *                                                                          *
 * Purpose : Decode command line or environment options.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-04-09  Argument 'NO' added to option /FIXED.                *
 *           98-09-10  Option /OPT (:WIN98) added.                          *
 *           98-11-08  Option /OLDIMPLIB added.                             *
 *           98-11-24  Option /DEBUG added; option /RELEASE official.       *
 *           98-12-05  Option /DEBUGTYPE:{CV|COFF|BOTH} added.              *
 *           98-12-12  Option /MAPINFO added.                               *
 *           98-12-28  Option /NOLOGO added (undocumented).                 *
 *           99-01-29  Response file handling improved.                     *
 *           99-02-06  Bugfix: don't call print_usage if empty directives.  *
 *           99-02-13  Option /DELAYLOAD:dll added.                         *
 *           99-02-14  Option /DELAY:{NOBIND|UNLOAD} added.                 *
 *           99-02-15  Option /MACHINE added (undocumented).                *
 *           99-08-18  Option /OPT:{REF|NOREF} added.                       *
 *           99-08-20  Option /LARGEADDRESSAWARE[:NO] added.                *
 *           00-05-01  Option /TSAWARE added.                               *
 *           00-11-16  Option /MACHINE now official. Added ARM.             *
 *           00-11-16  Windows CE subsystem added.                          *
 *           00-11-16  Handling of quoted names in DEFAULTLIB added.        *
 *           01-05-13  Option /STUB:filename added.                         *
 *           01-09-25  Option /SECTION:name,[E][R][W][S][D] added.          *
 *           03-06-17  Option /LIBPATH:path added.                          *
 *           03-06-27  Option /FORCE:MULTIPLE added.                        *
 *           03-06-30  Option /UNMANGLE added (undocumented).               *
 *           03-10-07  Options /NOEXPOBJ and /NOIMPLIB added.               *
 *                                                                          *
 ****************************************************************************/

void link_args(int argc, char **argv, bool_t drectve)
{
    bool_t skip_unknown_option;
    char fname[MAX_PATH];
    char *s;

    if (argc == 0 && !drectve)
        print_usage();

    for (; argc && *argv; argv++, argc--)
    {
        if (**argv == '-' || **argv == '/')
        {
            s = ++*argv;

            /*
             * Special syntax "-?option" means that we should
             * ignore this option if we don't understand it.
             */
            skip_unknown_option = (drectve && *s == '?');
            if (skip_unknown_option) s++;

            switch (lookup_option(&s))
            {
                case C_ALIGN:
                {
                    long alignment;

                    if (sscanf(s, "%li", &alignment) != 1)
                        apperror(RCFATAL(ERROR_ALIGN_OPTION));
                    if (alignment == 0 || (alignment & (alignment-1)) != 0)
                        apperror(RCFATAL(ERROR_ALIGN_OPTION));
                    options.section_alignment = alignment;
                    break;
                }

                case C_BASE:
                {
                    addr_t base;

                    if (sscanf(s, "%li", &base) != 1)
                        apperror(RCFATAL(ERROR_BASE_OPTION));
                    options.image_base = ROUNDUP(base, IMAGE_BASE_ALIGNMENT);
                    break;
                }

                case C_DEBUG:
                    options.debug = TRUE;
                    break;

                case C_DEBUGTYPE:
                    if (_stricmp(s, "CV") == 0)
                        options.debug_cv = TRUE;
                    else if (_stricmp(s, "COFF") == 0)
                        options.debug_coff = TRUE;
                    else if (_stricmp(s, "BOTH") == 0)
                        options.debug_coff = options.debug_cv = TRUE;
                    else
                        apperror(RCFATAL(ERROR_DEBUGTYPE_OPTION));
                    break;

                case C_DEFAULTLIB:
                    if (drectve && options.ignore_libraries) break;
                    if (*s == '\"' && (s = strchr(strcpy(fname, s+1), '\"')) != NULL)
                        *s = '\0';
                    else
                        strcpy(fname, s);
                    add_extension_to_file(fname, EXT_LIB);
                    if (add_libfile_to_list(fname, drectve) && options.verbose)
                        printmsg(MSG_PROCESSED_DEFLIB, fname);
                    break;

                case C_DELAYLOAD:
                    /* The user must specify a full filename... */
                    lookup_delay_import_module(s, TRUE);
                    break;

                case C_DELAY:
                    if (_stricmp(s, "NOBIND") == 0)
                        options.delay_bind = FALSE;
                    else if (_stricmp(s, "UNLOAD") == 0)
                        options.delay_unload = TRUE;
                    else
                        apperror(RCFATAL(ERROR_DELAY_OPTION));
                    break;

                case C_DISALLOWLIB:
                    /* Silently ignore Microsoft switch... */
                    break;

                case C_DLL:
                    options.DLL = TRUE;
                    options.fixed = FALSE;
                    break;

                case C_ENTRY:
                    if (entry_point == NULL || !drectve)
                        entry_point = tstrcpy(s);
                    break;

                case C_EXPORT:
                {
                    char expname[80];
                    char symname[80] = "";
                    int ordinal = 0;
                    bool_t is_func = TRUE;
                    EXPENTRY *exp;

                    if (sscanf(s, "%[^=,]", expname) != 1)
                        apperror(RCFATAL(ERROR_EXPORT_OPTION));
                    s += strlen(expname);
                    if (*s == '=')
                    {
                        if (sscanf(++s, "%[^,]", symname) != 1)
                            apperror(RCFATAL(ERROR_EXPORT_OPTION));
                        s += strlen(symname);
                    }
                    if (*s == ',')
                    {
                        if (*++s == '@')
                        {
                            if (sscanf(++s, "%i", &ordinal) != 1)
                                apperror(RCFATAL(ERROR_EXPORT_OPTION));
                            if (ordinal < 1 || ordinal > USHRT_MAX)
                                apperror(RCFATAL(ERROR_EXPORT_OPTION));
                        }
                        else if (_stricmp(s, "DATA") == 0)
                            is_func = FALSE;
                        else
                            apperror(RCFATAL(ERROR_EXPORT_OPTION));
                    }
                    exp = lookup_export(expname, (*symname) ? symname : expname);
                    exp->ordinal = ordinal;
                    exp->sym->flags.function = is_func;
                    break;
                }

                case C_FIXED:
                    options.fixed = TRUE;
                    if (*s++ == ':')
                    {
                        if (_stricmp(s, "NO") != 0)
                            apperror(RCFATAL(ERROR_FIXED_OPTION));
                        options.fixed = FALSE;
                    }
                    break;

                case C_FORCE:
                    if (_stricmp(s, "MULTIPLE") == 0)
                        options.force_multiple = TRUE;
                    else
                        apperror(RCFATAL(ERROR_FORCE_OPTION));
                    break;

                case C_HEAP:
                {
                    long reserve, commit;

                    switch (sscanf(s, "%li,%li", &reserve, &commit))
                    {
                        default: apperror(RCFATAL(ERROR_HEAP_OPTION));
                        case 2: options.heap_commit = ROUNDUP_DWORD(commit);
                        case 1: options.heap_reserve = ROUNDUP_DWORD(reserve);
                    }
                    break;
                }

                case C_INCLUDE:
                {
                    SYMENTRY *pub = lookup_public(s);  /* unmangled! */
                    pub->flags.referenced = TRUE;
                    if (pub->type == unused)
                    {
                        pub->type = unresolved;
                        unresolved_count++;
                    }
                    break;
                }

                case C_LARGEADDRESSAWARE:
                    options.large_address_aware = TRUE;
                    if (*s++ == ':')
                    {
                        if (_stricmp(s, "NO") != 0)
                            apperror(RCFATAL(ERROR_LARGEADDRESSAWARE_OPTION));
                        options.large_address_aware = FALSE;
                    }
                    break;

                case C_LIBPATH:
                    if (*s != '\0')
                        lookup_libpath(s);
                    break;

                case C_MACHINE:
                    if (_stricmp(s, "IX86") == 0)
                        options.machine = MACHINE_X86;
                    else if (_stricmp(s, "ARM") == 0)
                        options.machine = MACHINE_ARM;
                    else
                        apperror(RCFATAL(ERROR_MACHINE_OPTION));
                    break;

                case C_MAP:
                    if (*s++ == ':')
                    {
                        add_extension_to_file(strcpy(fname, s), EXT_MAP);
                        remove_file(&map_file, map_file);
                        lookup_file(&map_file, fname);
                    }
                    options.mapfile = TRUE;
                    break;

                case C_MAPINFO:
                    if (_stricmp(s, "EXPORTS") == 0)
                        options.mapfile = options.mapfile_exports = TRUE;
                    else if (_stricmp(s, "FIXUPS") == 0)
                        options.mapfile = options.mapfile_fixups = TRUE;
                    else if (_stricmp(s, "LINES") == 0)
                        options.mapfile = options.mapfile_lines = TRUE;
                    else
                        apperror(RCFATAL(ERROR_MAPINFO_OPTION));
                    break;

                case C_MERGE:
                {
                    char from_name[80];
                    char to_name[80];

                    if (sscanf(s, "%[^=]=%s", from_name, to_name) != 2)
                        apperror(RCFATAL(ERROR_MERGE_OPTION));
                    lookup_section_alias(from_name, to_name);
                    break;
                }

                case C_NODEFAULTLIB:
                    options.ignore_libraries = TRUE;
                    break;

                case C_NOENTRY:
                    options.no_entry = TRUE;
                    break;

                case C_NOEXPOBJ:
                    options.no_export_object = TRUE;
                    break;

                case C_NOIMPLIB:
                    options.no_import_archive = TRUE;
                    break;

                case C_NOLOGO:
                    /* Silently ignore Microsoft switch... */
                    break;

                case C_OLDIMPLIB:
                    options.old_implib = TRUE;
                    break;

                case C_OPT:
                    if (_stricmp(s, "WIN98") == 0)
                        options.file_alignment = FILE_ALIGNMENT_WIN98;
                    else if (_stricmp(s, "NOWIN98") == 0)
                        options.file_alignment = FILE_ALIGNMENT_WIN32;
                    else if (_stricmp(s, "REF") == 0)
                        options.discard_unreferenced = TRUE;
                    else if (_stricmp(s, "NOREF") == 0)
                        options.discard_unreferenced = FALSE;
                    else
                        apperror(RCFATAL(ERROR_OPT_OPTION));
                    break;

                case C_OUT:
                    remove_file(&exe_file, exe_file);
                    lookup_file(&exe_file, s);
                    break;

                case C_RELEASE:
                    options.debug = FALSE;
                    break;

                case C_SECTION:
                {
                    ATTENTRY *att;
                    char name[80];
                    char attrs[80], *p;

                    if (sscanf(s, "%[^,],%s", name, attrs) != 2)
                        apperror(RCFATAL(ERROR_SECTION_OPTION));

                    att = lookup_section_attributes(name);
                    att->memflags = att->addflags = 0;
                    for (p = attrs; *p != '\0'; p++)
                    {
                        if (*p == 'e' || *p == 'E')
                            att->memflags |= COFF_STYP_EXEC;
                        else if (*p == 'r' || *p == 'R')
                            att->memflags |= COFF_STYP_READ;
                        else if (*p == 'w' || *p == 'W')
                            att->memflags |= COFF_STYP_WRITE;
                        else if (*p == 's' || *p == 'S')
                            att->addflags |= COFF_STYP_SHARE;
                        else if (*p == 'd' || *p == 'D')
                            att->addflags |= COFF_STYP_DISCARD;
                        else
                            apperror(RCFATAL(ERROR_SECTION_OPTION));
                    }
                    break;
                }

                case C_STACK:
                {
                    long reserve;
                    long commit;

                    switch (sscanf(s, "%li,%li", &reserve, &commit))
                    {
                        default: apperror(RCFATAL(ERROR_STACK_OPTION));
                        case 2: options.stack_commit = ROUNDUP_DWORD(commit);
                        case 1: options.stack_reserve = ROUNDUP_DWORD(reserve);
                    }
                    break;
                }

                case C_STUB:
                    /* use list to get the file size */
                    add_file_to_list(&stub_file, s);
                    break;

                case C_SUBSYSTEM:
                {
                    char name[80];
                    ushort_t major;
                    ushort_t minor;

                    switch (sscanf(s, "%[^,],%hi.%hi", name, &major, &minor))
                    {
                        default: apperror(RCFATAL(ERROR_SUBSYSTEM_OPTION));
                        case 3: options.minor_subsystem_version = minor;
                        case 2: options.major_subsystem_version = major;
                        case 1: break;
                    }
                    if (_stricmp(name, "NATIVE") == 0)
                        options.subsystem = COFF_SS_NATIVE;
                    else if (_stricmp(name, "WINDOWS") == 0)
                        options.subsystem = COFF_SS_WINGUI;
                    else if (_stricmp(name, "WINDOWSCE") == 0)
                        options.subsystem = COFF_SS_WCEGUI;
                    else if (_stricmp(name, "CONSOLE") == 0)
                        options.subsystem = COFF_SS_WINCUI;
                    else
                        apperror(RCFATAL(ERROR_SUBSYSTEM_OPTION));
                    break;
                }

                case C_SWAPRUN:
                    if (_stricmp(s, "NET") == 0)
                        options.swaprun_net = TRUE;
                    else if (_stricmp(s, "CD") == 0)
                        options.swaprun_cd = TRUE;
                    else
                        apperror(RCFATAL(ERROR_SWAPRUN_OPTION));
                    break;

                case C_TSAWARE:
                    options.terminal_server_aware = TRUE;
                    if (*s++ == ':')
                    {
                        if (_stricmp(s, "NO") != 0)
                            apperror(RCFATAL(ERROR_TSAWARE_OPTION));
                        options.terminal_server_aware = FALSE;
                    }
                    break;

                case C_UNMANGLE:
                    options.unmangle_names = TRUE;
                    break;

                case C_VERBOSE:
                    options.verbose = TRUE;
                    break;

                case C_VERSION:
                {
                    ushort_t major;
                    ushort_t minor;

                    switch (sscanf(s, "%hi.%hi", &major, &minor))
                    {
                        default: apperror(RCFATAL(ERROR_VERSION_OPTION));
                        case 2: options.minor_os_version = minor;
                        case 1: options.major_os_version = major;
                    }
                    break;
                }

                case C_WS:
                    if (_stricmp(s, "AGGRESSIVE") != 0)
                        apperror(RCFATAL(ERROR_WS_OPTION));
                    options.aggressive_ws_trim = TRUE;
                    break;

                case C_HELP:
                    print_usage();
                    break;

                default:
                    if (!skip_unknown_option)
                        apperror(RCWARNING(ERROR_UNKNOWN_OPTION), s);
                    break;
            }
        }
        else
        {
            if (**argv == '@')
            {
                char **eargv;
                int eargc;

                s = read_respfile((*argv)+1);
                if (!s) apperror(RCFATAL(ERROR_CANT_READ_CMDFILE), (*argv)+1);

                tokenize(s, &eargc, &eargv);
                link_args(eargc, eargv, FALSE);
                my_free(eargv);

                my_free(s);
            }
            else
            {
                s = strrchr(*argv, '.');
                if (s != NULL && _stricmp(s, EXT_LIB) == 0)
                {
                    add_libfile_to_list(*argv, FALSE);
                }
                else
                {
                    add_extension_to_file(strcpy(fname, *argv), EXT_OBJ);
                    add_file_to_list(&obj_file_list, fname);
                }
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_option                                                  *
 *                                                                          *
 * Purpose : Lookup a option in the options array.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum opttok lookup_option(char **opt)
{
    int i;

    for (i = 0; i < NELEMS(opts); i++)
    {
        if (_strnicmp(*opt, opts[i].name, strlen(opts[i].name)) == 0)
        {
            (*opt) += strlen(opts[i].name);

            /* Special treatment of options with arguments */
            if (opts[i].has_args && (**opt != ':' || *++*opt == '\0'))
            {
                apperror(RCFATAL(ERROR_OPTION_ARG_MISSING), opts[i].name);
            }

            return opts[i].tok;
        }
    }

    return C_NOTHING;
}

/****************************************************************************
 *                                                                          *
 * Function: add_libfile_to_list                                            *
 *                                                                          *
 * Purpose : Add a new archive to the global list.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-10-04  Rewritten; argument 'drectve' added.                 *
 *           99-08-14  Return value added; new_file global removed.         *
 *           03-06-17  Scanning through lib_path_list added.                *
 *                                                                          *
 ****************************************************************************/

static bool_t add_libfile_to_list(const char *filename, bool_t drectve)
{
    char fname[MAX_PATH];
    FILEINFO *lib_file;

    /* Search previously found libraries */
    for (lib_file = lib_file_list;
         lib_file != NULL && drectve;
         lib_file = lib_file->next)
    {
        if (_stricmp(basename(lib_file->name), basename(filename)) == 0)
            return FALSE;
    }

    /* Don't bother if we already have a path or wildcards */
    if (!strpbrk(filename, "\\:*?"))
    {
        LIBENTRY *lib_path;

        for (lib_path = lib_path_list;
             lib_path != NULL;
             lib_path = lib_path->next)
        {
            struct _finddata_t find;
            long handle;
            size_t plen;

            /* Combine current path and filename */
            plen = strlen(strcpy(fname, lib_path->name));
            if (plen > 0 && fname[plen-1] != '\\' && fname[plen-1] != ':')
                fname[plen++] = '\\';
            strcpy(fname + plen, filename);

            handle = _findfirst(fname, &find);
            if (handle != -1)
            {
                _findclose(handle);
                add_file_to_list(&lib_file_list, fname);
                return TRUE;
            }
        }
    }

    _searchenv(filename, "LIB", fname);
    add_file_to_list(&lib_file_list, (*fname) ? fname : filename);

    return drectve;
}

/****************************************************************************
 *                                                                          *
 * Function: add_file_to_list                                               *
 *                                                                          *
 * Purpose : Add a new filename to the given list. Expand wildcards.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-10-16  Bugfix: didn't handle drive without backslash.       *
 *                                                                          *
 ****************************************************************************/

static void add_file_to_list(FILEINFO **list, const char *filename)
{
    struct _finddata_t find;
    long handle;

    handle = _findfirst(filename, &find);
    if (handle == -1)
    {
        apperror(RCFATAL(ERROR_OPEN_FILE), filename);
    }

    do
    {
        char fname[MAX_PATH];
        char *s;
        size_t plen;
        FILEINFO *file;

        /* Combine path and current filename */
        plen = (s = strrchr(filename, '\\')) != NULL || (s = strrchr(filename, ':')) != NULL ? (s-filename+1) : 0;
        strcpy(strncpy(fname, filename, plen) + plen, find.name);

        file = lookup_file(list, fname);
        file->size = find.size;

    } while (_findnext(handle, &find) == 0);
    _findclose(handle);
}

/****************************************************************************
 *                                                                          *
 * Function: print_usage                                                    *
 *                                                                          *
 * Purpose : Display usage information.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void print_usage(void)
{
    printmsg(MSG_USAGE_TITLE);
    printmsg(MSG_USAGE_COPYRIGHT);
    printmsg(MSG_USAGE_SYNTAX, PROGRAM_NAME);
    printmsg(MSG_USAGE_OPTIONS);
    printmsg(MSG_USAGE_OPTION_ALIGN);
    printmsg(MSG_USAGE_OPTION_BASE);
    printmsg(MSG_USAGE_OPTION_DEBUG);
    printmsg(MSG_USAGE_OPTION_DEBUGTYPE);
    printmsg(MSG_USAGE_OPTION_DEFAULTLIB);
    printmsg(MSG_USAGE_OPTION_DELAY);
    printmsg(MSG_USAGE_OPTION_DELAYLOAD);
    printmsg(MSG_USAGE_OPTION_DLL);
    printmsg(MSG_USAGE_OPTION_ENTRY);
    printmsg(MSG_USAGE_OPTION_EXPORT);
    printmsg(MSG_USAGE_OPTION_FIXED);
    printmsg(MSG_USAGE_OPTION_FORCE);
    printmsg(MSG_USAGE_OPTION_HEAP);
    printmsg(MSG_USAGE_OPTION_INCLUDE);
    printmsg(MSG_USAGE_OPTION_LARGEADDRESSAWARE);
    printmsg(MSG_USAGE_OPTION_LIBPATH);
    printmsg(MSG_USAGE_OPTION_MACHINE);
    printmsg(MSG_USAGE_OPTION_MAP);
    printmsg(MSG_USAGE_OPTION_MAPINFO);
    printmsg(MSG_USAGE_OPTION_MERGE);
    printmsg(MSG_USAGE_OPTION_NODEFAULTLIB);
    printmsg(MSG_USAGE_OPTION_NOENTRY);
    printmsg(MSG_USAGE_OPTION_NOEXPOBJ);
    printmsg(MSG_USAGE_OPTION_NOIMPLIB);
    printmsg(MSG_USAGE_OPTION_OLDIMPLIB);
    printmsg(MSG_USAGE_OPTION_OPT);
    printmsg(MSG_USAGE_OPTION_OUT);
    printmsg(MSG_USAGE_OPTION_RELEASE);
    printmsg(MSG_USAGE_OPTION_SECTION);
    printmsg(MSG_USAGE_OPTION_STACK);
    printmsg(MSG_USAGE_OPTION_STUB);
    printmsg(MSG_USAGE_OPTION_SUBSYSTEM);
    printmsg(MSG_USAGE_OPTION_SWAPRUN);
    printmsg(MSG_USAGE_OPTION_TSAWARE);
    printmsg(MSG_USAGE_OPTION_UNMANGLE);
    printmsg(MSG_USAGE_OPTION_VERBOSE);
    printmsg(MSG_USAGE_OPTION_VERSION);
    printmsg(MSG_USAGE_OPTION_WS);

    errorexit(1);
}

/****************************************************************************
 *                                                                          *
 * Function: apperror                                                       *
 *                                                                          *
 * Purpose : Display error message. Terminate program if it's fatal.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           02-06-16  Rewritten to support resource messages.              *
 *                                                                          *
 ****************************************************************************/

void apperror(WINERR err, ...)
{
    if (LOWORD(err) != ERROR_SUCCESS)
    {
        char buf[512];
        va_list ap;

        printmsg(MSG_ERRTEXT_PROGNAME, PROGRAM_NAME);

        if (ISFATAL(err)) printmsg(MSG_ERRTEXT_FATAL);
        if (ISERROR(err)) printmsg(MSG_ERRTEXT_ERROR);
        if (ISWARNING(err)) printmsg(MSG_ERRTEXT_WARNING);

        va_start(ap, err);
        if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_FROM_SYSTEM, NULL,
            err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING),
            0, buf, NELEMS(buf), &ap))
        {
            sprintf(buf, "*** No message for error 0x%X ***", err);
        }
        va_end(ap);

        CharToOemA(buf, buf);
        printf(buf);

        if (ISFATAL(err)) errorexit(1);
        if (ISERROR(err)) nerrs++;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: printmsg                                                       *
 *                                                                          *
 * Purpose : Display private message.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

void printmsg(int msg, ...)
{
    char buf[512];
    va_list ap;

    va_start(ap, msg);
    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, NULL, msg, 0, buf, NELEMS(buf), &ap))
        *buf = '\0';
    va_end(ap);

    CharToOemA(buf, buf);
    printf(buf);
}

/****************************************************************************
 *                                                                          *
 * Function: errorexit                                                      *
 *                                                                          *
 * Purpose : Bail out.                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void errorexit(int code)
{
    FILEINFO *file;

    if (exe_file != NULL)
    {
        my_closemap(exe_file, FALSE);
        my_deletefile(exe_file->name);
    }

    if (map_file != NULL)
    {
        my_closemap(map_file, FALSE);
        my_deletefile(map_file->name);
    }

    if (exp_file != NULL)  /* export object */
    {
        my_closemap(exp_file, FALSE);
        my_deletefile(exp_file->name);
    }

    if (lib_file != NULL)  /* import library */
    {
        my_closemap(lib_file, FALSE);
        my_deletefile(lib_file->name);
    }

    for (file = lib_file_list; file != NULL; file = file->next)
        my_closemap(file, FALSE);

    for (file = obj_file_list; file != NULL; file = file->next)
        my_closemap(file, FALSE);

    exit(code);
}

/****************************************************************************
 *                                                                          *
 * Function: ctrl_handler                                                   *
 *                                                                          *
 * Purpose : Ctrl+C and Ctrl+Break handler for console apps.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-08-14  Suspension of main thread added.                     *
 *                                                                          *
 ****************************************************************************/

static BOOL WINAPI ctrl_handler(DWORD dwCtrlType)
{
    UNREFERENCED_PARAMETER(dwCtrlType);

    printmsg(MSG_CTRL_BREAK, PROGRAM_NAME);

    /*
     * By definition (according to Microsoft), a Ctrl-Break
     * handler is running as a *new* thread.
     *
     * Make sure we suspend the main thread, so it doesn't
     * continue to run when we call the cleanup code, in
     * errorexit(). If we don't do this, the app will very
     * likely blow up, due to some access violation.
     *
     * Note that this requires single threading, or the
     * following code will dead-lock (no time to investigate
     * why, right now).
     */
    SuspendThread(hThread);

#ifdef _MT
//Ron Pinkas
//#error ctrl_handler() requires single threading.
#endif

    errorexit(1);
    return 1;
}

