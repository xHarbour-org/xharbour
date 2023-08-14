/****************************************************************************
 *                                                                          *
 * File    : dllmain.c                                                      *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; DLL main module.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *           02-06-16  All messages moved to the resources.                 *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

HANDLE hmod;
HANDLE hfres = NULL;

int nerrs = 0;
int nident = 0;
int codepage = 0;
int idemode = 0;
int quitrun = 0;

jmp_buf jumpbuf;

FILEINFO *fileinfo_list = NULL;
INCLINFO *inclinfo_list = NULL;

errfunc error_callback = NULL;
inffunc info_callback = NULL;

/****************************************************************************
 *                                                                          *
 * Function: DllMain                                                        *
 *                                                                          *
 * Purpose : DLL entry and exit procedure.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

BOOL WINAPI DllMain(HANDLE hDLL, DWORD dwReason, LPVOID lpReserved)
{
    UNREFERENCED_PARAMETER(hDLL);
    UNREFERENCED_PARAMETER(lpReserved);

    hmod = hDLL;

    switch (dwReason)
    {
        case DLL_PROCESS_ATTACH:
        {
            setup_scanner();
            codepage = GetACP();

            /* Before -I or -D arguments */
            pp_init();

            return TRUE;
        }

        case DLL_PROCESS_DETACH:
            res_close();

            while (cursource != NULL)
                unset_source();

            return TRUE;

        default:
            return TRUE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: set_error_function                                             *
 *                                                                          *
 * Purpose : Interface: Set address of error callback function.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI set_error_function(errfunc error_function, int mode)
{
    error_callback = error_function;
    idemode = mode;
}

/****************************************************************************
 *                                                                          *
 * Function: set_information_function                                       *
 *                                                                          *
 * Purpose : Interface: Set address of information callback function.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI set_information_function(inffunc info_function)
{
    info_callback = info_function;
}

/****************************************************************************
 *                                                                          *
 * Function: delete_standard_include_paths                                  *
 *                                                                          *
 * Purpose : Interface: Delete paths for 'standard places' (-X option).     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI delete_standard_include_paths(void)
{
    delete_includes();
}

/****************************************************************************
 *                                                                          *
 * Function: add_include_path                                               *
 *                                                                          *
 * Purpose : Interface: Add a new search path (-I option).                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI add_include_path(const char *filename)
{
    add_include(filename, INC_STDPLACE, FALSE);
}

/****************************************************************************
 *                                                                          *
 * Function: set_codepage                                                   *
 *                                                                          *
 * Purpose : Interface: Set current codepage (-C option).                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI int WINAPI set_codepage(int cp)
{
    if (!IsValidCodePage(cp))
        return 0;

    codepage = cp;
    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: set_language                                                   *
 *                                                                          *
 * Purpose : Interface: Set current language (-L option).                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI int WINAPI set_language(int lang)
{
    commontail.language = lang;
    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: define_symbol                                                  *
 *                                                                          *
 * Purpose : Interface: Define a preprocessor symbol (-D option).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI define_symbol(const char *name)
{
    pp_define(name);
}

/****************************************************************************
 *                                                                          *
 * Function: compile_script                                                 *
 *                                                                          *
 * Purpose : Interface: Compile a resoure script.                           *
 *                                                                          *
 * Comment : The resfile argument can be NULL, for no output.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI int WINAPI compile_script(const char *rcfile, const char *resfile)
{
    if (setjmp(jumpbuf) == 0)
    {
        /* compile the resource script */
        rc_main(rcfile, resfile);
    }
    else
    {
        /* fatal error */
        nerrs++;
    }

    if (nerrs != 0)
    {
        /* cleanup after errors */
        res_close();
        my_deletefile(resfile);

        while (cursource != NULL)
            unset_source();
    }

    return nerrs == 0;
}

/****************************************************************************
 *                                                                          *
 * Function: enum_file_info                                                 *
 *                                                                          *
 * Purpose : Interface: Enumerate through all FILEINFO nodes.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI int WINAPI enum_file_info(enumfilefunc enum_function, void *arg)
{
    FILEINFO *info;

    for (info = fileinfo_list; info != NULL; info = info->next)
    {
        if (!enum_function(info->type, info->name, info->language, info->filename, arg))
            return 0;
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: enum_include_info                                              *
 *                                                                          *
 * Purpose : Interface: Enumerate through all INCLINFO nodes.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI int WINAPI enum_include_info(enuminclfunc enum_function, void *arg)
{
    INCLINFO *info;

    for (info = inclinfo_list; info != NULL; info = info->next)
    {
        if (!enum_function(info->filename, arg))
            return 0;
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: set_abort_flag                                                 *
 *                                                                          *
 * Purpose : Interface: Set flag to abort compilation.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

RESDLLAPI void WINAPI set_abort_flag(void)
{
    quitrun = 1;
}

/****************************************************************************
 *                                                                          *
 * Function: apperror                                                       *
 *                                                                          *
 * Purpose : Display error message. Terminate program if it's fatal.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           01-11-09  Rewritten when compiler were converted to DLL.       *
 *           02-06-17  Rewritten to support resource messages.              *
 *                                                                          *
 ****************************************************************************/

void apperror(WINERR err, ...)
{
    if (err != 0)
    {
        if (error_callback != 0)
        {
            enum errclass class = 0;
            char buf[512];
            va_list ap;

            if (ISFATAL(err)) class = ERRCLASS_FATAL;
            if (ISERROR(err)) class = ERRCLASS_ERROR;
            if (ISWARNING(err)) class = ERRCLASS_WARNING;

            va_start(ap, err);
            if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_FROM_SYSTEM, hmod,
                err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING),
                0, buf, NELEMS(buf), &ap))
            {
                sprintf(buf, "*** No message for error 0x%X ***", err);
            }
            va_end(ap);

            error_callback(class, (s.line != 0) ? s.filename : NULL, s.line, buf);
        }

        if (ISFATAL(err) || idemode) longjmp(jumpbuf, 1);
        if (ISERROR(err)) nerrs++;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: sprintmsg                                                      *
 *                                                                          *
 * Purpose : Display private message.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

int sprintmsg(char *buf, int msg, ...)
{
    va_list ap;

    va_start(ap, msg);
    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, hmod, msg, 0, buf, 512, &ap))
        *buf = '\0';
    va_end(ap);

    return strlen(buf);
}

