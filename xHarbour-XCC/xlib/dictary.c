/****************************************************************************
 *                                                                          *
 * File    : dictary.c                                                      *
 *                                                                          *
 * Purpose : Win32 Library Manager; dictionary management.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lib.h"

#define SYMBOL_ARRAY_INCREMENT  1024

SYMENTRY **symbol_list = NULL;
size_t symbol_count = 0;
size_t symbol_maxcount = 0;

#define EXPORT_ARRAY_INCREMENT  1024

EXPENTRY **export_list = NULL;
size_t export_count = 0;
size_t export_maxcount = 0;

/****************************************************************************
 *                                                                          *
 * Function: lookup_symbol                                                  *
 *                                                                          *
 * Purpose : Lookup a symbol in the symbol table.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-03-22  Changed to dynamic allocation of array.              *
 *                                                                          *
 ****************************************************************************/

SYMENTRY *lookup_symbol(const char *name)
{
    long lo = 0;
    long hi = symbol_count-1;
    SYMENTRY *sym;

    /*
     * Search for the given symbol.
     */
    while (hi >= lo)
    {
        long this = (lo + hi) / 2;
        int cond = strcmp(name, symbol_list[this]->name);

        if (cond < 0)
            hi = this-1;
        else if (cond > 0)
            lo = this+1;
        else
            return symbol_list[this];
    }

    /*
     * Didn't find the symbol. Add it (sorted).
     */
    if (symbol_count == symbol_maxcount)
    {
        symbol_maxcount += SYMBOL_ARRAY_INCREMENT;
        symbol_list = (SYMENTRY **)my_realloc(symbol_list, symbol_maxcount * sizeof(SYMENTRY *));
    }

    sym = (SYMENTRY *)my_alloc(sizeof(SYMENTRY) + strlen(name));
    sym->offset = 0;
    sym->defined = FALSE;
    strcpy(sym->name, name);

    memmove(&symbol_list[lo+1], &symbol_list[lo], (symbol_count - lo) * sizeof(SYMENTRY *));

    symbol_list[lo] = sym;
    symbol_count++;

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_export                                                  *
 *                                                                          *
 * Purpose : Lookup an exported symbol in the export table.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-01-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

EXPENTRY *lookup_export(const char *name, int ordinal, bool_t is_func)
{
    long lo = 0;
    long hi = export_count-1;
    EXPENTRY *exp;

    /*
     * Search for the given export.
     */
    while (hi >= lo)
    {
        long this = (lo + hi) / 2;
        int cond = strcmp(name, export_list[this]->name);

        if (cond < 0)
            hi = this-1;
        else if (cond > 0)
            lo = this+1;
        else
            return export_list[this];
    }

    /*
     * Didn't find the export. Add it (sorted).
     */
    if (export_count == export_maxcount)
    {
        export_maxcount += EXPORT_ARRAY_INCREMENT;
        export_list = (EXPENTRY **)my_realloc(export_list, export_maxcount * sizeof(EXPENTRY *));
    }

    exp = (EXPENTRY *)my_alloc(sizeof(EXPENTRY) + strlen(name));
    exp->ordinal = ordinal;
    exp->defined = FALSE;
    exp->is_func = is_func;
    strcpy(exp->name, name);

    memmove(&export_list[lo+1], &export_list[lo], (export_count - lo) * sizeof(EXPENTRY *));

    export_list[lo] = exp;
    export_count++;

    return exp;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_file                                                    *
 *                                                                          *
 * Purpose : Lookup a file in the given list.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

FILEINFO *lookup_file(FILEINFO **list, const char *name)
{
    FILEINFO *file;
    FILEINFO *file_last;

    /*
     * Search for the given file.
     */
    for (file = *list, file_last = NULL;
         file != NULL;
         file_last = file, file = file->next)
    {
        if (strcmp(name, file->name) == 0)
            return file;
    }

    /*
     * Didn't find the file. Add it.
     */
    file = (FILEINFO *)my_alloc(sizeof(FILEINFO) + strlen(name));
    file->next = NULL;
    file->hf = NULL;
    file->hmap = NULL;
    file->base = NULL;
    file->time = 0;
    file->size = 0;
    strcpy(file->name, name);

    if (file_last == NULL)
    {
        /* This is the first node. Start the list */
        *list = file;
    }
    else
    {
        /* Append the new node to the list */
        file_last->next = file;
    }

    return file;
}

/****************************************************************************
 *                                                                          *
 * Function: remove_file_from_list                                          *
 *                                                                          *
 * Purpose : Remove a file from the given list.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void remove_file_from_list(FILEINFO **list, FILEINFO *file_free)
{
    FILEINFO *file;
    FILEINFO *file_prev;

    /*
     * Find the existing node and get it's previous link.
     */
    for (file = *list, file_prev = NULL;
         file && file != file_free;
         file_prev = file, file = file->next)
        ;

    /*
     * Did we find the node?
     */
    if (file != NULL)
    {
        if (file_prev)
            file_prev->next = file_free->next;
        else
            *list = file_free->next;

        my_free(file_free);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_name                                                    *
 *                                                                          *
 * Purpose : Lookup a name in the given list.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

NAMENTRY *lookup_name(NAMENTRY **list, const char *name)
{
    NAMENTRY *nam;
    NAMENTRY *nam_last;

    /*
     * Search for the given name.
     */
    for (nam = *list, nam_last = NULL;
         nam != NULL;
         nam_last = nam, nam = nam->next)
    {
        if (strcmp(name, nam->name) == 0)
            return nam;
    }

    /*
     * Didn't find the name. Add it.
     */
    nam = (NAMENTRY *)my_alloc(sizeof(NAMENTRY) + strlen(name));
    nam->next = NULL;
    strcpy(nam->name, name);

    if (nam_last == NULL)
    {
        /* This is the first node. Start the list */
        *list = nam;
    }
    else
    {
        /* Append the new node to the list */
        nam_last->next = nam;
    }

    return nam;
}

/****************************************************************************
 *                                                                          *
 * Function: remove_name_from_list                                          *
 *                                                                          *
 * Purpose : Remove a name from the given list.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void remove_name_from_list(NAMENTRY **list, NAMENTRY *nam_free)
{
    NAMENTRY *nam;
    NAMENTRY *nam_prev;

    /*
     * Find the existing node and get it's previous link.
     */
    for (nam = *list, nam_prev = NULL;
         nam && nam != nam_free;
         nam_prev = nam, nam = nam->next)
        ;

    /*
     * Did we find the node?
     */
    if (nam != NULL)
    {
        if (nam_prev)
            nam_prev->next = nam_free->next;
        else
            *list = nam_free->next;

        my_free(nam_free);
    }
}

