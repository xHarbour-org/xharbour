/****************************************************************************
 *                                                                          *
 * File    : codeview.c                                                     *
 *                                                                          *
 * Purpose : CodeView debugging support - for x86.md.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-16  Added function cvtype_dimarray().                    *
 *                                                                          *
 ****************************************************************************/

#define CVTYPE_FWDREF   USHRT_MAX

/*
 * Things that should be fixed:
 * a) We currently ignore const and volatile modifiers.
 */

static ushort_t typidx = 0;

/* Static function prototypes */
static ushort_t cv_register(const char *);
static ushort_t cv_typeno(TYPE *);
static ushort_t cvtype(TYPE *);
static ushort_t cvtype_pointer(TYPE *);
static ushort_t cvtype_array(TYPE *);
static ushort_t cvtype_dimarray(TYPE *);
static ushort_t cvtype_function(TYPE *);
static ushort_t cvtype_structure(TYPE *);
static ushort_t cvtype_bitfield(FIELD *);

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_objname                                                *
 *                                                                          *
 * Purpose : Emit a CodeView S_OBJNAME symbol record.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_objname(char *outfile)
{
    /*
     * (0x0009) Object File Name
     *
     * This symbol specifies the name of the object file for this module.
     *
     * 2       2          4          *
     * length  S_OBJNAME  signature  name
     *
     * signature    Signature for the CodeView information.
     *
     * name         Length prefixed name of the object file *without*
     *              any path information prepended to the name, says
     *              Micro$oft and outputs the name with a path...
     */
    size_t len = strlen(outfile);

    print("dw %u,%u\n", 7+len, CODEVIEW_SYMTYPE_OBJNAME);
    print("dd 0\n");
    print("db %u,\"%s\"\n", len, outfile);
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_version                                                *
 *                                                                          *
 * Purpose : Emit a CodeView S_COMPILE symbol record.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-08-19  Modified to load version string from resources.      *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_version(void)
{
    /*
     * (0x0001) Compile Flag
     *
     * This symbol communicates to CodeView compile time information
     * on a per module basis, such as the language and version number
     * of the language processor, the ambient model for code and
     * data, and the target processor.
     *
     * 2       2          1        3      *
     * length  S_COMPILE  machine  flags  version
     *
     * machine      enumeration specifying target processor. Values
     *              not specified below are reserved.
     *
     *              0x00    Intel 8080
     *              0x01    Intel 8086
     *              0x02    Intel 80286
     *              0x03    Intel 80386
     *              0x04    Intel 80486
     *              0x05    Intel Pentium
     *              0x10    MIPS R4000
     *              0x11    Reserved for future MIPS processor
     *              0x12    Reserved for future MIPS processor
     *              0x20    MC68000
     *              0x21    MC68010
     *              0x22    MC68020
     *              0x23    MC68030
     *              0x24    MC68040
     *              0x30    DEC Alpha
     *
     *  flags       flags showing compile time options
     *
     *              Language       :8
     *              CodePresent    :1
     *              FloatPrecision :2
     *              FloatPackage   :2
     *              AmbientData    :3
     *              AmbientCode    :3
     *              Mode32         :1 Compiled for 32 bit addresses
     *              Reserved       :4
     *
     *              Language enumeration
     *
     *              0   C
     *              1   C++
     *              2   Fortran
     *              3   Masm
     *              4   Pascal
     *              5   Basic
     *              6   Cobol
     *              7 - Reserved
     *              255
     *
     *              Ambient code and data memory model enumeration
     *
     *              0   Near
     *              1   Far
     *              2   Huge
     *              3 - Reserved
     *              7
     *
     *              Floating package enumeration
     *
     *              0   Hardware processor (80x87 for Intel 80x86)
     *              1   Emulator
     *              2   Altmath
     *              3   Reserved
     *
     *              The FloatPrecision flag is set to 1 if the compiler
     *              follows the ANSI C floating point precision rules.
     *              This is specified to Microsoft C compilers by setting
     *              the -Op option.
     *
     * version      Length-prefixed string specifying language processor
     *              version. Language processors can place additional data
     *              in version string if desired.
     */
    char buf[128], *s;
    size_t len = FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_IGNORE_INSERTS,
        hmod, MSG_USAGE_TITLE, 0, buf, NELEMS(buf), NULL);

    if ((s = strstr(buf, "\r\n")) != NULL)
    {
        len = s - buf;
        buf[len] = '\0';
    }

    print("dw %u,%u\n", 7+len, CODEVIEW_SYMTYPE_COMPILE);
    print("db %u\n", CODEVIEW_SYMBOL_MACHINE_PENTIUM);
    print("db %u\n", CODEVIEW_SYMBOL_LANGUAGE_C);
    print("db 0,0\n");
    print("db %u,\"%s\"\n", len, buf);
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_proc32                                                 *
 *                                                                          *
 * Purpose : Emit a CodeView S_LPROC32 or S_GPROC32 symbol record.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_proc32(SYMBOL *sym)
{
    /*
     * (0x100a) Local Procedure Start 16:32
     *
     * This symbol record defines local (file static) procedure
     * definition. For C/C++, functions that are declared static
     * to a module are emitted as Local Procedure symbols.
     * Functions not specifically declared static are emitted as
     * Global Procedures (see below.)
     *
     * 2       2       4        4     4      4        4      4
     * length  symbol  pParent  pEnd  pNext  proclen  debug  debug
     *                                                start  end
     * 4          4       2        1      *
     * @proctype  offset  segment  flags  name
     *
     * symbol       S_LPROC32 or S_GPROC32.
     *
     * pParent      See the section on lexical scope linking.
     * pEnd         See the section on lexical scope linking.
     * pNext        See the section on lexical scope linking.
     *
     * proclen      Length in bytes of this procedure.
     *
     * debug start  Offset in bytes from the start of the procedure to
     *              the point where the stack frame has been set up.
     *              Parameter and frame variables can be viewed at
     *              this point.
     *
     * debug end    Offset in bytes from the start of the procedure to
     *              the point where the procedure is ready to return
     *              and has calculated its return value, if any.
     *              Frame and register variables can still be viewed.
     *
     * @proctype    Type of the procedure type record.
     *
     * offset       Offset portion of the procedure address.
     * segment      Segment portion of the procedure address.
     *
     * flags        Procedure flags:
     *
     *              fpo       :1  true if function has frame pointer omitted.
     *              interrupt :1  true if function is interrupt fcn.
     *              return    :1  true if function performs far return.
     *              never     :1  true if function never returns.
     *              unused    :4
     *
     * name         Length-prefixed name of procedure.
     *
     *
     * (0x100b) Global Procedure Start 16:32
     *
     * This symbol is used for procedures that are not specifically
     * declared static to a module. The format is the same as the
     * Local Procedure Start 16:32 symbol (see above.)
     */
    ushort_t typeno = cv_typeno(sym->type);  /* might emit to CVTYPS */
    size_t len = strlen(sym->name);

    (*IR->segment)(CVSYMS);

    print("dw %u,%u\n", 38+len, (sym->sclass == STATIC) ?
        CODEVIEW_SYMTYPE_LPROC32_16T : CODEVIEW_SYMTYPE_GPROC32_16T);
    print("dd 0,0,0\n");  /* parent/end/next ptr */
    print("dd ..?X%s-%s\n", sym->x.name, sym->x.name);  /* proclen */
    print("dd ..?S%s-%s\n", sym->x.name, sym->x.name);  /* debug start */
    print("dd ..?E%s-%s\n", sym->x.name, sym->x.name);  /* debug end */
    print("dw %u,0\n", typeno);  /*proctype */
    print("dd %s\n", sym->x.name);
    print("dw %s\n", sym->x.name);
    print("db %u\n", sym->attr.noreturn ? 0x08 : 0);  /* flags */    // ? phase problem with FPO flag
    print("db %u,\"%s\"\n", len, sym->name);
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_bprel32                                                *
 *                                                                          *
 * Purpose : Emit a CodeView S_BPREL32 symbol record.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_bprel32(SYMBOL *sym)
{
    /*
     * (0x1006) BP Relative 16:32
     *
     * This symbol specifies symbols that are allocated on the stack
     * for a procedure. For C/C++, these include the actual parameters
     * to a function and the local nonstatic variables of functions.
     *
     * 2       2              4       4      *
     * length  S_BPREL32_16t  offset  @type  name
     *
     * offset   Signed offset relative to EBP. If offset is 0, then the
     *          symbol was assigned to a register or never instantiated
     *          by the optimizer and cannot be evaluated because its
     *          location is unknown.
     *
     * @type    Type of the symbol.
     *
     * name     Length-prefixed name of the symbol.
     */
    ushort_t typeno = cv_typeno(sym->type);  /* might emit to CVTYPS */
    size_t len = strlen(sym->name);

    (*IR->segment)(CVSYMS);

    print("dw %u,%u\n", 11+len, CODEVIEW_SYMTYPE_BPREL32_16T);
    print("dd %d\n", sym->x.offset);
    print("dw %u,0\n", typeno);
    print("db %u,\"%s\"\n", len, sym->name);
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_register                                               *
 *                                                                          *
 * Purpose : Emit a CodeView S_REGISTER symbol record.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_register(SYMBOL *sym)
{
    /*
     * (0x0002) Register
     *
     * This symbol record describes a symbol that has been
     * enregistered. Provisions for enabling future implementation
     * tracking of a symbol into and out of registers is provided in
     * this symbol. When the symbol processor is examining a register
     * symbol, the length field of the symbol is compared with the
     * offset of the byte following the symbol name field. If these
     * are the same, there is no register tracking information. If the
     * length and offset are different, the byte following the end of
     * the symbol name is examined. If the byte is zero, there is no
     * register tracking information following the symbol. If the byte
     * is not zero, then the byte indexes into the list of stack
     * machine implementations and styles of register tracking.
     * Microsoft does not currently emit or process register tracking
     * information.
     *
     * 2       2           2      2         *     *
     * length  S_REGISTER  @type  register  name  tracking
     *
     * @type        The type of the symbol.
     *
     * register     Enumerate of the registers in which the symbol value
     *              is stored. This field is treated as two bytes. The
     *              high order byte specifies the register in which the
     *              high order part of the value is stored. The low byte
     *              specifies the register for the low order part of the
     *              value. If the value is not stored in two registers
     *              then high order register field contains the enumerate
     *              value for no register. For register enumeration values,
     *              see Section 6. The register index enumeration is
     *              specific to the processor model for the module.
     *
     * name         Length-prefixed name of symbol stored in the register.
     *
     * tracking     Register tracking information. Format unspecified.
     */
    ushort_t typeno = cv_typeno(sym->type);  /* might emit to CVTYPS */
    size_t len = strlen(sym->name);

    (*IR->segment)(CVSYMS);

    print("dw %u,%u\n", 7+len, CODEVIEW_SYMTYPE_REGISTER_16T);
    print("dw %u\n", typeno);
    print("dw %u\n", cv_register(sym->x.name));  // ? should handle multiple registers
    print("db %u,\"%s\"\n", len, sym->name);
}

/****************************************************************************
 *                                                                          *
 * Subfunction: cv_register                                                 *
 *                                                                          *
 ****************************************************************************/

static ushort_t cv_register(const char *name)
{
    if (strcmp(name, "eax") == 0) return CODEVIEW_SYMBOL_REGISTER_EAX;
    if (strcmp(name, "ebx") == 0) return CODEVIEW_SYMBOL_REGISTER_EBX;
    if (strcmp(name, "ecx") == 0) return CODEVIEW_SYMBOL_REGISTER_ECX;
    if (strcmp(name, "edx") == 0) return CODEVIEW_SYMBOL_REGISTER_EDX;
    if (strcmp(name, "edi") == 0) return CODEVIEW_SYMBOL_REGISTER_EDI;
    if (strcmp(name, "esi") == 0) return CODEVIEW_SYMBOL_REGISTER_ESI;
    if (strcmp(name, "ax") == 0) return CODEVIEW_SYMBOL_REGISTER_AX;
    if (strcmp(name, "bx") == 0) return CODEVIEW_SYMBOL_REGISTER_BX;
    if (strcmp(name, "cx") == 0) return CODEVIEW_SYMBOL_REGISTER_CX;
    if (strcmp(name, "dx") == 0) return CODEVIEW_SYMBOL_REGISTER_DX;
    if (strcmp(name, "di") == 0) return CODEVIEW_SYMBOL_REGISTER_DI;
    if (strcmp(name, "si") == 0) return CODEVIEW_SYMBOL_REGISTER_SI;
    if (strcmp(name, "al") == 0) return CODEVIEW_SYMBOL_REGISTER_AL;
    if (strcmp(name, "bl") == 0) return CODEVIEW_SYMBOL_REGISTER_BL;
    if (strcmp(name, "cl") == 0) return CODEVIEW_SYMBOL_REGISTER_CL;
    if (strcmp(name, "dl") == 0) return CODEVIEW_SYMBOL_REGISTER_DL;

    return CODEVIEW_SYMBOL_REGISTER_NONE;
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_data32                                                 *
 *                                                                          *
 * Purpose : Emit a CodeView S_LDATA32 or S_GDATA32 symbol record.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_data32(SYMBOL *sym)
{
    /*
     * (0x1007) Local Data 16:32
     *
     * These symbols are used for data that is not exported from a
     * module. In C/C++, symbols that are declared static are emitted
     * as Local Data symbols. Symbols that are emitted as Local Data
     * cannot be moved by CVPACK into the global symbol table for the
     * executable file.
     *
     * 2       2              4      4       2        *
     * length  S_LDATA32_16t  @type  offset  segment  name
     *
     * @type        Type index of the symbol.
     * offset       Offset portion of the symbol address.
     * segment      Segment portion of the symbol address.
     * name         Length-prefixed name of symbol.
     *
     *
     * (0x1008) Global Data Symbol 16:32
     *
     * This symbol record has the same format as the Local Data 16:32
     * except that the record type is S_GDATA32. For C/C++, symbols
     * that are not specifically declared static are emitted as Global
     * Data Symbols and can be compacted by CVPACK into the global
     * symbol table.
     */
    ushort_t typeno = cv_typeno(sym->type);  /* might emit to CVTYPS */
    size_t len = strlen(sym->name);

    (*IR->segment)(CVSYMS);

    print("dw %u,%u\n", 13+len, (sym->sclass == STATIC) ?
        CODEVIEW_SYMTYPE_LDATA32_16T : CODEVIEW_SYMTYPE_GDATA32_16T);
    print("dw %u,0\n", typeno);
    print("dd %s\n", sym->x.name);
    print("dw %s\n", sym->x.name);
    print("db %u,\"%s\"\n", len, sym->name);
}

/****************************************************************************
 *                                                                          *
 * Function: emit_cv_udt32                                                  *
 *                                                                          *
 * Purpose : Emit a CodeView S_UDT symbol record.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void emit_cv_udt32(SYMBOL *sym)
{
    /*
     * (0x1003) User-defined type
     *
     * This specifies a C typedef or user-defined type, such as
     * classes, structures, unions, or enums.
     *
     * 2       2      4      *
     * length  S_UDT  @type  name
     *
     * @type    Type of symbol.
     * name     Length-prefixed name of the user defined type.
     */
    ushort_t typeno = cv_typeno(sym->type);  /* might emit to CVTYPS */

    (*IR->segment)(CVSYMS);

    print("dw %u,%u\n", 5, CODEVIEW_SYMTYPE_UDT_16T);
    print("dw %u\n", typeno);
    print("db 0\n");  /* use name in type record */
}

/****************************************************************************
 *                                                                          *
 * Function: cv_typeno                                                      *
 *                                                                          *
 * Purpose : Entry point for mapping TYPE records to CodeView types.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ushort_t cv_typeno(TYPE *ty)
{
    int oldseg = cseg;
    ushort_t typeno = cvtype(ty);
    (*IR->segment)(oldseg);
    return typeno;
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype                                                         *
 *                                                                          *
 * Purpose : Return CodeView type for the given TYPE record.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-05-22  Bugfix: _Bool is now 8 bits, not 32!                 *
 *           03-08-16  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype(TYPE *ty)
{
    if (!ty) return CODEVIEW_TYPE_NOTYPE;

    ty = unqual(ty);

    if (isfunc(ty)) return cvtype_function(ty);
    if (isstruct(ty)) return cvtype_structure(ty);  /* struct or union */
    if (isvla(ty)) return cvtype_dimarray(ty);
    if (isarray(ty)) return cvtype_array(ty);
    if (isenum(ty)) return CODEVIEW_TYPE_INT4;

    if (isptr(ty))
    {
        TYPE *tp = unqual(ty->type);

        if (tp == booltype) return CODEVIEW_TYPE_32PBOOL08;
        if (tp == chartype) return CODEVIEW_TYPE_32PCHAR;
        if (tp == doubletype) return CODEVIEW_TYPE_32PREAL64;
        if (tp == floattype) return CODEVIEW_TYPE_32PREAL32;
        if (tp == inttype) return CODEVIEW_TYPE_32PINT4;
        if (tp == longdoubletype) return CODEVIEW_TYPE_32PREAL64;  /* same as double (in MSVC) */
        if (tp == longtype) return CODEVIEW_TYPE_32PINT4;
        if (tp == longlongtype) return CODEVIEW_TYPE_32PINT8;
        if (tp == shorttype) return CODEVIEW_TYPE_32PINT2;
        if (tp == signedchartype) return CODEVIEW_TYPE_32PCHAR;
        if (tp == unsignedchartype) return CODEVIEW_TYPE_32PUCHAR;
        if (tp == unsignedlongtype) return CODEVIEW_TYPE_32PUINT4;
        if (tp == unsignedlonglongtype) return CODEVIEW_TYPE_32PUINT8;
        if (tp == unsignedshorttype) return CODEVIEW_TYPE_32PUINT2;
        if (tp == unsignedtype) return CODEVIEW_TYPE_32PUINT4;
        if (tp == voidtype) return CODEVIEW_TYPE_32PVOID;
        if (tp == widechartype) return CODEVIEW_TYPE_32PWCHAR;
#ifdef HAS_C99_COMPLEX
        if (tp == complexdoubletype) return  CODEVIEW_TYPE_32PCPLX64;
        if (tp == complexfloattype) return  CODEVIEW_TYPE_32PCPLX32;
        if (tp == complexlongdoubletype) return  CODEVIEW_TYPE_32PCPLX64;
#endif
        return cvtype_pointer(ty);
    }
    else
    {
        if (ty == booltype) return CODEVIEW_TYPE_BOOL08;
        if (ty == chartype) return CODEVIEW_TYPE_CHAR;
        if (ty == doubletype) return CODEVIEW_TYPE_REAL64;
        if (ty == floattype) return CODEVIEW_TYPE_REAL32;
        if (ty == inttype) return CODEVIEW_TYPE_INT4;
        if (ty == longdoubletype) return CODEVIEW_TYPE_REAL64;  /* same as double (in MSVC) */
        if (ty == longtype) return CODEVIEW_TYPE_INT4;
        if (ty == longlongtype) return CODEVIEW_TYPE_INT8;
        if (ty == shorttype) return CODEVIEW_TYPE_INT2;
        if (ty == signedchartype) return CODEVIEW_TYPE_CHAR;
        if (ty == unsignedchartype) return CODEVIEW_TYPE_UCHAR;
        if (ty == unsignedlongtype) return CODEVIEW_TYPE_UINT4;
        if (ty == unsignedlonglongtype) return CODEVIEW_TYPE_UINT8;
        if (ty == unsignedshorttype) return CODEVIEW_TYPE_UINT2;
        if (ty == unsignedtype) return CODEVIEW_TYPE_UINT4;
        if (ty == voidtype) return CODEVIEW_TYPE_VOID;
        if (ty == widechartype) return CODEVIEW_TYPE_WCHAR;
#ifdef HAS_C99_COMPLEX
        if (ty == complexdoubletype) return  CODEVIEW_TYPE_CPLX64;
        if (ty == complexfloattype) return  CODEVIEW_TYPE_CPLX32;
        if (ty == complexlongdoubletype) return  CODEVIEW_TYPE_CPLX64;
#endif
    }

    return CODEVIEW_TYPE_NOTYPE;  /* what the hell is it? */
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_pointer                                                 *
 *                                                                          *
 * Purpose : Emit a CodeView LF_POINTER type record.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-12-14  Bugfix: must check ty->x.typeno again after cvtype.  *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_pointer(TYPE *ty)
{
    /*
     * (0x1002) Pointer
     *
     * This record is the generic pointer type record. It supports the
     * C++ reference type, pointer to data member, and pointer to
     * method. It also conveys the const and volatile pointer
     * information.
     *
     * 2           4      4          *
     * LF_POINTER  @type  attribute  variant
     *
     * @type        Type index of object pointed to.
     *
     * attribute    Consists of five bit fields:
     *
     *              ptrtype :5  Ordinal specifying mode of pointer
     *              0   Near
     *              1   Far
     *              2   Huge
     *              3   Based on segment (OBSOLETE)
     *              4   Based on value
     *              5   Based on segment of value (OBSOLETE)
     *              6   Based on address of symbol (OBSOLETE)
     *              7   Based on segment of symbol address (OBSOLETE)
     *              8   Based on type
     *              9   Based on self (OBSOLETE)
     *              10  Near 32 bit pointer
     *              11  Far 32 bit pointer
     *              12  64 bit pointer
     *              13- Reserved
     *              31
     *
     *              ptrmode :3  Ordinal specifying pointer mode
     *              0   Pointer
     *              1   Reference
     *              2   Pointer to data member
     *              3   Pointer to method
     *              4-7 Reserved
     *
     *              isflat32  :1   True if 16:32 pointer
     *              volatile  :1   True if pointer is volatile
     *              const     :1   True if pointer is const
     *              unaligned :1   True if pointer is unaligned
     *              restrict  :1   True if pointer is restricted
     *              unused    :19  Unused and reserved
     *
     * variant      variant portion of the record, depending upon
     *              the pointer type.
     */
    if (ty->x.typeno == 0)
    {
        ushort_t typeno = cvtype(unqual(ty->type));

        if (ty->x.typeno == 0)
        {
            (*IR->segment)(CVTYPS);

            print("dw %u\n", 10);  /* 2+10 = dword aligned */
            print("dw %u\n", CODEVIEW_LEAFTYPE_POINTER);
            print("dd %u\n", typeno);
            print("dd 10\n");  /* near 32-bit pointer */

            ty->x.typeno = 0x1000 + typidx++;
        }
    }

    return ty->x.typeno;
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_array                                                   *
 *                                                                          *
 * Purpose : Emit a CodeView LF_ARRAY type record.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_array(TYPE *ty)
{
    /*
     * (0x1003) Simple Array
     *
     * 2         4          4         *       *
     * LF_ARRAY  @elemtype  @idxtype  length  name
     *
     * @elemtype    Type index of each array element.
     * @idxtype     Type index of indexing variable.
     * length       Length of array in bytes.
     * name         Length-prefixed name of array.
     */
    if (ty->x.typeno == 0)
    {
        ushort_t typeno = cvtype(ty->type);

        (*IR->segment)(CVTYPS);

        print("dw %u\n", (ty->size < CODEVIEW_LEAFTYPE_NUMERIC) ? 14 : 18);
        print("dw %u\n", CODEVIEW_LEAFTYPE_ARRAY);
        print("dd %u,%u\n", typeno, CODEVIEW_TYPE_INT4);
        if (ty->size < CODEVIEW_LEAFTYPE_NUMERIC)
            print("dw %u\n", ty->size);
        else
            print("dw %u\ndd %u\n", CODEVIEW_LEAFTYPE_LONG, ty->size);
        print("dw 0\n");

        ty->x.typeno = 0x1000 + typidx++;
    }

    return ty->x.typeno;
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_dimarray                                                *
 *                                                                          *
 * Purpose : Emit a CodeView LF_DIMARRAY type record.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_dimarray(TYPE *ty)
{
    /*
     * (0x100C) Multiply Dimensioned Array
     *
     * 2            4          4         *
     * LF_DIMARRAY  @elemtype  @diminfo  name
     *
     * @elemtype    Type index of each array element.
     * @diminfo     Type index of record containing the dimension information.
     * name         Length-prefixed name of the array.
     *
     *
     * (0x120A) Dimensioned Array with Variable Upper Bound
     *
     * 2           2        4         4*rank
     * LF_DIMVARU  rank     @index    @var
     *
     * rank         Number of dimensions.
     * @index       Type of the index.
     * @var         Array of type index of LF_REFSYM record describing the variable upper bound.
     *
     *
     * (0x020C) Referenced Symbol
     *
     * 2          *
     * LF_REFSYM  sym
     *
     * sym          Copy of the referenced symbol including the length field.
     */
    if (ty->x.typeno == 0)
    {
        TYPE *ty2 = ty;
        ushort_t typref = typidx;
        ushort_t typeno;

        (*IR->segment)(CVTYPS);

        /*
         * Emit symbol reference records for all dimensions.
         */
        do
        {
            print("dw %u\n", 15);
            print("dw %u\n", CODEVIEW_LEAFTYPE_REFSYM);
            print("dw %u,%u\n", 11, CODEVIEW_SYMTYPE_BPREL32_16T);
            print("dd %d\n", ty2->u.arr.sym ? ty2->u.arr.sym->x.offset : -1);
            print("dw %u,0\n", CODEVIEW_TYPE_INT4);
            print("db 0\n");

            typidx++;

        } while (ty2 = ty2->type, isarray(ty2));

        /*
         * Emit variable dimensions record.
         */
        print("dw %u\n", 10 + 4 * (typidx - typref));
        print("dw %u\n", CODEVIEW_LEAFTYPE_DIMVARU);
        print("dd %u\n", typidx - typref);
        print("dd %u\n", CODEVIEW_TYPE_INT4);
        while (typref < typidx)
            print("dd %u\n", 0x1000 + typref++);

        typidx++;

        typeno = cvtype(ty2);  /* might emit to CVTYPS */

        /*
         * Emit variable array record.
         */
        print("dw %u\n", 12);
        print("dw %u\n", CODEVIEW_LEAFTYPE_DIMARRAY);
        print("dd %u\n", typeno);
        print("dd %u\n", 0x1000 + typidx-1);
        print("dw 0\n");

        ty->x.typeno = 0x1000 + typidx++;
    }

    return ty->x.typeno;
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_function                                                *
 *                                                                          *
 * Purpose : Emit a CodeView LF_ARGLIST and LF_PROCEDURE type record.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-16  Bugfix: only emit argument record if arguments!      *
 *           04-07-02  Support for fastcall calling convention added.       *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_function(TYPE *ty)
{
    /*
     * (0x1008) Procedure
     *
     * 2             4        1     1         2       4
     * LF_PROCEDURE  @rvtype  call  reserved  #parms  @arglist
     *
     * @rvtype  Type index of the value returned by the procedure.
     *
     * call     Calling convention of the procedure:
     *          0  Near C
     *          1  Far C
     *          2  Near pascal
     *          3  Far pascal
     *          4  Near fastcall
     *          5  Far fastcall
     *          6  Reserved
     *          7  Near stdcall
     *          8  Far stdcall
     *          9  Near syscall
     *          10 Far syscall
     *          11 This call
     *          12 MIPS call
     *          13 Generic
     *          14-255 Reserved
     *
     * #parms   Number of parameters.
     * @arglist Type index of argument list type record.
     */
    if (ty->x.typeno == 0)
    {
        ushort_t typeno = cvtype(ty->type);  /* might emit to CVTYPS */
        ushort_t *cvtypes;
        int nargs;
        int i;

        (*IR->segment)(CVTYPS);

        nargs = 0; cvtypes = NULL;
        if (ty->u.fcn.prototype)
        {
            while (ty->u.fcn.prototype[nargs])
                nargs++;

            cvtypes = my_alloc(nargs * sizeof(ushort_t));

            for (i = 0; i < nargs; i++)
                cvtypes[i] = cvtype(ty->u.fcn.prototype[i]);
        }

        if (nargs > 0)
        {
            /*
             * Emit argument list record.
             */
            print("dw %u\n", 6 + nargs * 4);  /* 2+6 = dword aligned */
            print("dw %u\n", CODEVIEW_LEAFTYPE_ARGLIST);
            print("dd %u\n", nargs);
            for (i = 0; i < nargs; i++)
                print("dd %u\n", cvtypes[i]);

            typidx++;
        }

        my_free(cvtypes);
        cvtypes = NULL;

        /*
         * Emit procedure record.
         */
        print("dw %u\n", 14);  /* 2+14 = dword aligned */
        print("dw %u\n", CODEVIEW_LEAFTYPE_PROCEDURE);
        print("dd %u\n", typeno);
        print("db %u,0\n", iscdecl(ty) ? 0 : isfast(ty) ? 4 : 7);  /* C, fastcall or stdcall */
        print("dw %u\n", nargs);
        print("dd %u\n", 0x1000 + typidx-1);

        ty->x.typeno = 0x1000 + typidx++;
    }

    return ty->x.typeno;
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_structure                                               *
 *                                                                          *
 * Purpose : Emit a CodeView LF_FIELDLIST and LF_STRUCTURE type record or   *
 *           emit a CodeView LF_FIELDLIST and LF_UNION type record.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-19  Handling of NULL field names added.                  *
 *           03-05-27  Handling of bitfields added.                         *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_structure(TYPE *ty)
{
    /*
     * (0x1005) Structures
     *
     * 2             2      2         4       4       4        *       *
     * LF_STRUCTURE  count  property  @field  @dList  @vshape  length  name
     *
     * count        Number of elements in the class or structure. This
     *              count includes direct, virtual, and indirect virtual
     *              bases, and methods including overloads, data members,
     *              static data members, friends, and so on.
     *
     * property     Property bit field
     *
     *              packed   :1 Structure is packed.
     *              ctor     :1 Class has constructors and/or destructors.
     *              overops  :1 Class has overloaded operators.
     *              isnested :1 Class is a nested class.
     *              cnested  :1 Class contains nested classes.
     *              opassign :1 Class has overloaded assignment.
     *              opcast   :1 Class has casting methods.
     *              fwdref   :1 Class/structure is a incomplete reference.
     *              scoped   :1 This is a scoped definition.
     *              reserved :7
     *
     * @field       Type index of the field list for this class.
     *
     * @dList       Type index of the derivation list. This is output by
     *              the compiler as 0x0000 and is filled in by the CVPACK
     *              utility to a LF_DERIVED record containing the type
     *              indices of those classes which immediately inherit the
     *              current class. A zero index indicates that no derivation
     *              information is available. A LF_NULL index indicates
     *              that the class is not inherited by other classes.
     *
     * @vshape      Type index of the virtual function table shape
     *              descriptor.
     *
     * length       Numeric leaf specifying size in bytes of the structure.
     *
     * name         Length-prefixed name this type.
     *
     *
     * (0x1006) Unions
     *
     * 2         2       2         4       *       *
     * LF_UNION  count   property  @field  length  name
     *
     * count        Number of fields in the union.
     * property     Property bit field.
     * @field       Type index of field list.
     * length       Numeric leaf specifying size in bytes of the union.
     * name         Length-prefixed name of union.
     *
     */
    if (ty->x.typeno == 0)
    {
        ushort_t *cvtypes;
        size_t len;
        int reclen;
        int totlen;
        int pad;
        int nelems;
        FIELD *field;

        ty->x.typeno = CVTYPE_FWDREF;

        (*IR->segment)(CVTYPS);

        /*
         * Initialize type array for the field list.
         */
        nelems = 0;
        for (field = ty->u.sym->u.s.flist; field != 0; field = field->link)
            nelems++;

        cvtypes = my_alloc(nelems * sizeof(ushort_t));

        nelems = 0; totlen = 0;
        for (field = ty->u.sym->u.s.flist; field != 0; field = field->link)
        {
            len = field->name ? strlen(field->name) : 0;
            reclen = len + ((field->offset < CODEVIEW_LEAFTYPE_NUMERIC) ? 11 : 15);
            totlen += roundup(reclen,4);

            cvtypes[nelems] = field->bitsize ? cvtype_bitfield(field) : cvtype(field->type);
            nelems++;
        }

        /*
         * Emit field list record.
         */
        print("dw %u\n", 2 + totlen);
        print("dw %u\n", CODEVIEW_LEAFTYPE_FIELDLIST);
        nelems = 0;
        for (field = ty->u.sym->u.s.flist; field != 0; field = field->link)
        {
            len = field->name ? strlen(field->name) : 0;
            reclen = len + ((field->offset < CODEVIEW_LEAFTYPE_NUMERIC) ? 11 : 15);
            pad = roundup(reclen,4) - reclen;

            print("dw %u\n", CODEVIEW_LEAFTYPE_MEMBER);
            print("dw %u\n", 0);  /* fldattr */
            print("dd %u\n", cvtypes[nelems++]);
            if (field->offset < CODEVIEW_LEAFTYPE_NUMERIC)
                print("dw %u\n", field->offset);
            else
                print("dw %u\ndd %u\n", CODEVIEW_LEAFTYPE_LONG, field->offset);
            print("db %u,\"%s\"\n", len, field->name ? field->name : "");

            while (pad) print("db %u\n", CODEVIEW_LEAFTYPE_PAD0|pad--);
        }

        typidx++;

        my_free(cvtypes);
        cvtypes = NULL;

        /*
         * Emit structure or union record.
         */
        if (isunion(ty))
        {
            len = strlen(ty->u.sym->name);
            reclen = len + ((ty->size < CODEVIEW_LEAFTYPE_NUMERIC) ? 15 : 19);
            pad = roundup(reclen,4) - reclen;

            print("dw %u\n", reclen-2 + pad);
            print("dw %u\n", CODEVIEW_LEAFTYPE_UNION);
            print("dw %u\n", nelems);
            print("dw 0\n");
            print("dd %u\n", 0x1000 + typidx-1);
        }
        else /* structure */
        {
            len = strlen(ty->u.sym->name);
            reclen = len + ((ty->size < CODEVIEW_LEAFTYPE_NUMERIC) ? 23 : 27);
            pad = roundup(reclen,4) - reclen;

            print("dw %u\n", reclen-2 + pad);
            print("dw %u\n", CODEVIEW_LEAFTYPE_STRUCTURE);
            print("dw %u\n", nelems);
            print("dw 0\n");
            print("dd %u\n", 0x1000 + typidx-1);
            print("dd 0\n");
            print("dd 0\n");
        }
        if (ty->size < CODEVIEW_LEAFTYPE_NUMERIC)
            print("dw %u\n", ty->size);
        else
            print("dw %u\ndd %u\n", CODEVIEW_LEAFTYPE_LONG, ty->size);
        print("db %u,\"%s\"\n", len, ty->u.sym->name);

        while (pad) print("db %u\n", CODEVIEW_LEAFTYPE_PAD0|pad--);

        return ty->x.typeno = 0x1000 + typidx++;
    }
    else if (ty->x.typeno == CVTYPE_FWDREF && ty->x.xtypeno == 0)
    {
        size_t len;
        int reclen;
        int pad;

        /*
         * Emit *incomplete* structure or union record.
         */
        if (isunion(ty))
        {
            len = strlen(ty->u.sym->name);
            reclen = len + 15;
            pad = roundup(reclen,4) - reclen;

            print("dw %u\n", reclen-2 + pad);
            print("dw %u\n", CODEVIEW_LEAFTYPE_UNION);
            print("dw 0\n");
            print("dw 0x80\n");  /* fwdref */
            print("dd 0\n");
        }
        else /* structure */
        {
            len = strlen(ty->u.sym->name);
            reclen = len + 23;
            pad = roundup(reclen,4) - reclen;

            print("dw %u\n", reclen-2 + pad);
            print("dw %u\n", CODEVIEW_LEAFTYPE_STRUCTURE);
            print("dw 0\n");
            print("dw 0x80\n");  /* fwdref */
            print("dd 0\n");
            print("dd 0\n");
            print("dd 0\n");
        }
        print("dw %u\n", 0);
        print("db %u,\"%s\"\n", len, ty->u.sym->name);

        while (pad) print("db %u\n", CODEVIEW_LEAFTYPE_PAD0|pad--);

        return ty->x.xtypeno = 0x1000 + typidx++;
    }
    else if (ty->x.typeno == CVTYPE_FWDREF)
    {
        /* definition not done, use forward reference */
        return ty->x.xtypeno;
    }
    else
    {
        /* use previously generated definition */
        return ty->x.typeno;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: cvtype_bitfield                                                *
 *                                                                          *
 * Purpose : Emit a CodeView LF_BITFIELD type record.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-05-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ushort_t cvtype_bitfield(FIELD *field)
{
    /*
     * (0x1205) Bitfield
     *
     * 2            4      1       1
     * LF_BITFIELD  @type  length  position
     *
     * @type    Type of the symbol.
     * length   The length in bits of the object.
     * position Starting position (from bit 0) of the object in the word.
     */
    ushort_t typeno = cvtype(field->type);
    int pad = 2;

    (*IR->segment)(CVTYPS);

    print("dw %u\n", 8 + pad);
    print("dw %u\n", CODEVIEW_LEAFTYPE_BITFIELD);
    print("dd %u\n", typeno);
    print("db %u\n", fieldsize(field));
    print("db %u\n", fieldright(field));

    while (pad) print("db %u\n", CODEVIEW_LEAFTYPE_PAD0|pad--);

    return 0x1000 + typidx++;
}
