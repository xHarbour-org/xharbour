/*---------------------------------------------------------------------------*
 |              PDFlib - A library for generating PDF on the fly             |
 +---------------------------------------------------------------------------+
 | Copyright (c) 1997-2009 Thomas Merz and PDFlib GmbH. All rights reserved. |
 +---------------------------------------------------------------------------+
 |                                                                           |
 |    This software is subject to the PDFlib license. It is NOT in the       |
 |    public domain. Extended versions and commercial licenses are           |
 |    available, please check http://www.pdflib.com.                         |
 |                                                                           |
 *---------------------------------------------------------------------------*/

/* $Id$
 *
 * PDCORE error messages
 *
 */

#if	pdc_genNames
#define gen(n, num, nam, msg)	PDC_E_##nam = num,
#elif	pdc_genInfo
#define gen(n, num, nam, msg)	{ n, num, msg, (const char *) 0 },

#else
#error	invalid inclusion of generator file
#endif

/* -------------------------------------------------------------------- */
/* Configuration, memory, and I/O			(10xx)		*/
/* -------------------------------------------------------------------- */

gen(1, 1000, MEM_OUT,	"Out of memory in function $1")

gen(1, 1008, IO_ILLFILENAME, "Bad file name '$1'")

gen(4, 1009, IO_RDOPEN_CODETEXT,
    "Couldn't open $1file '$2' for reading (system error code $3: $4)")

gen(2, 1010, IO_RDOPEN, "Couldn't open $1file '$2' for reading")

gen(3, 1011, IO_RDOPEN_CODE,
    "Couldn't open $1file '$2' for reading (system error code $3)")

gen(2, 1012, IO_WROPEN, "Couldn't open $1file '$2' for writing")

gen(3, 1013, IO_WROPEN_CODE,
    "Couldn't open $1file '$2' for writing (system error code $3)")

gen(0, 1014, IO_NOWRITE, "Couldn't write output")

gen(4, 1015, IO_WROPEN_CODETEXT,
    "Couldn't open $1file '$2' for writing (system error code $3: $4)")

gen(2, 1016, IO_RDOPEN_NF,
    "Couldn't open $1file '$2' for reading (file not found)")

gen(2, 1017, IO_RDOPEN_BC,
    "Couldn't open $1file '$2' for reading (device (e.g. URL) not supported)")

gen(2, 1018, IO_WROPEN_NF,
    "Couldn't open $1file '$2' for writing (no such directory)")

gen(2, 1019, IO_WROPEN_BC,
    "Couldn't open $1file '$2' for writing (device (e.g. URL) not supported)")

gen(2, 1020, IO_RDOPEN_PD,
    "Couldn't open $1file '$2' for reading (permission denied)")

gen(2, 1022, IO_WROPEN_PD,
    "Couldn't open $1file '$2' for writing (permission denied)")

gen(2, 1024, IO_RDOPEN_TM,
    "Couldn't open $1file '$2' for reading (too many open files)")

gen(2, 1026, IO_WROPEN_TM,
    "Couldn't open $1file '$2' for writing (too many open files)")

gen(2, 1028, IO_RDOPEN_ID,
    "Couldn't open $1file '$2' for reading (is a directory)")

gen(2, 1030, IO_WROPEN_ID,
    "Couldn't open $1file '$2' for writing (is a directory)")

gen(2, 1032, IO_WROPEN_AE,
    "Couldn't open $1file '$2' for writing (file already exists)")

gen(2, 1034, IO_WROPEN_TL,
    "Couldn't open $1file '$2' for writing (file name too long)")

gen(2, 1036, IO_WROPEN_NS,
    "Couldn't open $1file '$2' for writing (no space left on device)")

gen(2, 1037, IO_RDOPEN_IS,
    "Couldn't open $1file '$2' for reading (file name syntax incorrect)")

gen(2, 1038, IO_WROPEN_IS,
    "Couldn't open $1file '$2' for writing (file name syntax incorrect)")

gen(2, 1040, IO_WROPEN_NC,
    "Couldn't open $1file '$2' for writing (file cannot be created)")

gen(2, 1042, IO_WROPEN_NP,
    "Couldn't open $1file '$2' for writing (path not found)")

gen(2, 1044, IO_RDOPEN_SV,
    "Couldn't open $1file '$2' for reading (used by another process)")

gen(2, 1046, IO_WROPEN_SV,
   "Couldn't open $1file '$2' for writing (used by another process)")

gen(0, 1048, IO_UNSUPP_UNINAME,
    "Unicode file names are not supported on this platform")

gen(0, 1049, IO_UNSUPP_PDFUNINAME,
    "Unicode file names require PDF 1.7")

gen(1, 1050, IO_COMPRESS, "Compression error ($1)")

gen(0, 1052, IO_NOBUFFER, "Don't fetch buffer contents when writing to file")

gen(2, 1054, IO_BADFORMAT, "'$1' does not appear to be a $2 file")

gen(2, 1055, IO_WRITE,
    "Error writing data to file '$1' (system error code $2)")

gen(1, 1056, IO_READ, "Error reading data from file '$1'")

gen(3, 1057, IO_WRITE_CODETEXT,
    "Error writing data to file '$1' (system error code $2: $3)")

gen(3, 1058, IO_ILLSYNTAX, "$1file '$2': Syntax error in line $3")

gen(1, 1060, PVF_NAMEEXISTS,
    "Couldn't create virtual file '$1' (name already exists)")

gen(2, 1062, IO_FILE_EMPTY, "$1file '$2' is empty")

gen(2, 1064, IO_RDOPEN_QU,
    "Couldn't open $1file '$2' for reading (quota exceeded)")

gen(2, 1066, IO_WROPEN_QU,
    "Couldn't open $1file '$2' for writing (quota exceeded)")

gen(1, 1068, IO_TOOLONG_FULLNAME,
    "Fully specified file name too long (>= 1024 bytes): '$1'")


gen(2, 1080, IO_TOOFEW_REQFILEHDLS,
    "Too few requested file handles $1 (< $2)")

gen(2, 1081, IO_TOOMANY_REQFILEHDLS,
    "Too many requested file handles $1 (> $2)")


/* -------------------------------------------------------------------- */
/* Invalid arguments                                        (11xx)	*/
/* -------------------------------------------------------------------- */

gen(1, 1100, ILLARG_EMPTY, "Parameter '$1' is empty")

gen(2, 1101, ILLARG_FLOAT_ZERO,
    "Floating point parameter '$1' has bad value $2 (too close to 0)")

/* Unused. See 1107
gen(1, 1102, ILLARG_POSITIVE, "Parameter '$1' must be positive")
*/

gen(2, 1104, ILLARG_BOOL, "Boolean parameter '$1' has bad value '$2'")

gen(2, 1106, ILLARG_INT, "Integer parameter '$1' has bad value $2")

gen(3, 1107, ILLARG_FLOAT_TOOSMALL,
    "Floating point parameter '$1' has bad value $2 (minimum $3)")

gen(2, 1108, ILLARG_FLOAT, "Floating-point parameter '$1' has bad value $2")

gen(3, 1109, ILLARG_FLOAT_TOOLARGE,
    "Floating point parameter '$1' has bad value $2 (maximum $3)")

gen(2, 1110, ILLARG_STRING, "String parameter '$1' has bad value '$2'")

gen(1, 1111, ILLARG_FLOAT_NAN,
    "Floating point parameter '$1' has bad value (not a number)")

/* Unused. See 1504
gen(1, 1112, ILLARG_UTF, "Illegal UTF-$1 sequence in string")
*/

gen(2, 1114, ILLARG_MATRIX, "Matrix [$1] is degenerated")

gen(2, 1116, ILLARG_TOOLONG,
    "String parameter '$1' is limited to $2 characters")

gen(2, 1117, ILLARG_STRINGLEN,
    "String length has bad value $1 (minimum 0, maximum $2)")

gen(2, 1118, ILLARG_HANDLE,
    "Handle parameter or option of type '$1' has bad value $2")

/* Unused. See 1107
gen(1, 1120, ILLARG_NONNEG, "Parameter '$1' must not be negative")
*/

gen(1, 1122, ILLARG_LANG_CODE, "Unsupported language code '$1'")

gen(2, 1124, ILLARG_TOOMANY, "Too many '$1' parameters (maximum $2)")


/* -------------------------------------------------------------------- */
/* Parameters and values                                    (12xx)	*/
/* -------------------------------------------------------------------- */

gen(0, 1200, PAR_EMPTYKEY, "Empty key")

gen(1, 1202, PAR_UNKNOWNKEY, "Unknown key '$1'")

gen(0, 1204, PAR_EMPTYVALUE, "Empty parameter value")

gen(2, 1206, PAR_ILLPARAM, "Bad parameter '$1' for key '$2'")

gen(2, 1208, PAR_ILLVALUE, "Bad value $1 for key '$2'")

gen(2, 1210, PAR_SCOPE_GET, "Can't get parameter '$1' in scope '$2'")

gen(2, 1212, PAR_SCOPE_SET, "Can't set parameter '$1' in scope '$2'")

gen(2, 1214, PAR_VERSION, "Parameter '$1' requires PDF $2")

gen(1, 1216, PAR_ILLKEY, "Illegal attempt to set parameter '$1'")

gen(1, 1217, RES_BADCAT, "Bad resource category '$1'")

gen(2, 1218, RES_BADRES, "Bad resource specification '$1' for category '$2'")

gen(3, 1219, RES_BADRES2,
    "Bad resource specification '$1 = $2' for category '$3'")

gen(1, 1220, PAR_UNSUPPKEY, "Unknown or unsupported key '$1'")

gen(1, 1250, PAR_ILLSECT, "Illegal section '$1'")

gen(1, 1260, PAR_NODATA, "No data supplied ($1)")




/* -------------------------------------------------------------------- */
/* Options and values                                       (14xx)      */
/* -------------------------------------------------------------------- */

gen(1, 1400, OPT_UNKNOWNKEY, "Unknown option '$1'")

gen(2, 1402, OPT_TOOFEWVALUES, "Option '$1' has too few values (< $2)")

gen(2, 1404, OPT_TOOMANYVALUES, "Option '$1' has too many values (> $2)")

gen(1, 1406, OPT_NOVALUES, "Option '$1' doesn't have a value")

gen(2, 1408, OPT_ILLBOOLEAN, "Option '$1' has bad boolean value '$2'")

gen(2, 1410, OPT_ILLINTEGER, "Option '$1' has bad integer value '$2'")

gen(2, 1412, OPT_ILLNUMBER, "Option '$1' has bad number value '$2'")

gen(2, 1414, OPT_ILLKEYWORD, "Option '$1' has bad keyword '$2'")

gen(2, 1415, OPT_ILLCHAR,
    "Option '$1' has bad Unicode value or character name '$2'")

gen(3, 1416, OPT_TOOSMALLVAL,
    "Value $2 for option '$1' is too small (minimum $3)")

gen(2, 1417, OPT_TOOSMALLPERCVAL,
    "Value $2% for option '$1' is too small (minimum 0%)")

gen(3, 1418, OPT_TOOBIGVAL,
    "Value $2 for option '$1' is too large (maximum $3)")

gen(2, 1419, OPT_TOOBIGPERCVAL,
    "Value $2% for option '$1' is too large (maximum 100%)")

gen(2, 1420, OPT_ZEROVAL, "Option '$1' has bad value $2")

gen(3, 1422, OPT_TOOSHORTSTR,
    "String value '$2' for option '$1' is too short (minimum $3)")

gen(3, 1424, OPT_TOOLONGSTR,
    "String value '$2' for option '$1' is too long (maximum $3)")

gen(2, 1426, OPT_ILLSPACES,
    "Option '$1' has bad string value '$2' (contains whitespace)")

gen(1, 1428, OPT_NOTFOUND, "Required option '$1' is missing")

gen(1, 1430, OPT_IGNORED, "Option '$1' ignored")

gen(2, 1432, OPT_VERSION, "Option '$1' is not supported in PDF $2")

gen(3, 1434, OPT_ILLHANDLE, "Option '$1' has bad $3 handle $2")

gen(2, 1436, OPT_IGNORE,
    "Option '$1' will be ignored (specified option '$2' is dominant)")

gen(1, 1438, OPT_UNSUPP, "Option '$1' not supported in PDFlib Lite")

gen(1, 1439, OPT_UNSUPP_CONFIG,
    "Option '$1' not supported in this configuration")

gen(1, 1440, OPT_NOTBAL, "Braces aren't balanced in option list '$1'")

gen(1, 1442, OPT_ODDNUM, "Option '$1' has odd number of values")

gen(1, 1444, OPT_EVENNUM, "Option '$1' has even number of values")

gen(1, 1446, OPT_ILLCOMB, "Option '$1' contains a bad combination of keywords")

gen(1, 1448, OPT_ILL7BITASCII, "Option '$1' contains bad 7-bit ASCII string")

gen(2, 1450, OPT_COMBINE, "Option '$1' must not be combined with option '$2'")

gen(2, 1452, OPT_ILLBOX, "Option '$1' has bad box '$2'")

gen(2, 1454, OPT_ILLEXP, "Option '$1' has bad expression '$2'")

gen(1, 1456, OPT_TOOMANYPERCVALS,
    "Option '$1' contains too many percentage values (> 32)")

gen(2, 1458, OPT_ILLPOLYLINE,
    "Option '$1' has bad polyline '$2' (too few vertices <= 2)")


/* -------------------------------------------------------------------- */
/* String conversion and encoding functions                 (15xx)      */
/* -------------------------------------------------------------------- */

gen(0, 1500, CONV_ILLUTF16,  "Invalid UTF-16 string (odd byte count)")

gen(2, 1501, CONV_ILLUTF16SUR,  "Invalid UTF-16 surrogate pair <U+$1,U+$2>")

gen(0, 1502, CONV_MEMOVERFLOW,  "Out of memory in UTF string conversion")

gen(0, 1503, CONV_ILLUTF32,
    "Invalid UTF-32 string (byte count not a multiple of four)")

gen(1, 1504, CONV_ILLUTF, "Invalid UTF-$1 string")

gen(1, 1505, CONV_ILLUTF32CHAR, "Invalid UTF-32 character U+$1")

gen(1, 1506, CONV_ILL_MBTEXTSTRING,
    "Invalid text string according to the current codepage '$1'")

gen(2, 1507, CONV_ILLUTF8SEQU,
    "Invalid UTF-8 sequence $1 at byte index $2")

gen(1, 1508, CONV_UNSUPP_MBTEXTFORM,
    "Multi byte text format (codepage $1) not supported on this platform")

gen(0, 1510, CONV_LIST_MEMOVERFLOW,
    "Buffer overflow while converting code to destination code list")

gen(1, 1520, CONV_CHARREF_TOOLONG,
    "Illegal character reference '$1' (too long)")

gen(1, 1521, CONV_HTML_ILLCODE,
    "Illegal HTML character entity '$1' (illegal character code)")

gen(1, 1522, CONV_HTML_ILLNAME,
    "Illegal HTML character entity '$1' (illegal character name)")

gen(1, 1523, CONV_CHARREF_MISSDELIMIT,
    "Illegal character reference '$1' (delimiter ';' missing)")

gen(1, 1524, CONV_CHARREF_UNKNOWN,
    "Illegal character reference '$1' (unknown glyph name)")

gen(1, 1525, CONV_CHARREF_NOTUNIQUE,
    "Illegal character reference '$1' (more than one Unicode value)")

gen(2, 1550, ENC_TOOLONG, "Encoding name '$1' too long (max. $2)")

gen(2, 1551, ENC_BADLINE, "Syntax error in encoding file '$1' (line '$2')")

gen(1, 1552, ENC_NOTFOUND, "Couldn't find encoding '$1'")

gen(2, 1554, ENC_NOTDEF_UNICODE, "Unicode U+$1 not defined in encoding '$2'")

gen(2, 1556, ENC_NOTDEF_CODE, "Code $1 has no Unicode value in encoding '$2'")

gen(1, 1558, ENC_UNSUPP_LANG,
    "Codeset '$1' in LANG environment variable not supported")

gen(2, 1570, GLL_BADLINE, "Syntax error in glyph list file '$1' (line '$2')")

gen(2, 1572, CDL_BADLINE, "Syntax error in code list file '$1' (line '$2')")

gen(1, 1574, STR_ILL_ESCSEQ, "Illegal escape sequence '$1'")

gen(1, 1576, STR_ILL_UNIESCSEQ,
    "Illegal UTF-16 escape sequence (character U+$1 > U+00FF)")






/* -------------------------------------------------------------------- */
/* Internal                                                 (19xx)	*/
/* -------------------------------------------------------------------- */

/* Unused.
gen(1, 1900, INT_NULLARG, "Invalid NULL argument in function $1")
*/

gen(0, 1902, INT_XSTACK, "Exception stack underflow")

gen(1, 1904, INT_UNUSEDOBJ, "Object $1 allocated but not used")

gen(1, 1906, INT_FLOATTOOLARGE, "Floating point number $1 too large for PDF")

gen(0, 1907, INT_ILLFLOAT, "Bad floating point number for PDF")

gen(2, 1908, INT_BADFORMAT, "Unknown vsprintf() format '$1' ($2)")

gen(0, 1909, INT_FORMOVERFLOW, "Buffer overflow in formatting function")

gen(1, 1910, INT_ALLOC0,
    "Tried to allocate 0 or negative number of bytes in function $1")

/* Unused. See 1502
gen(1, 1912, INT_UNICODEMEM, "Too few bytes allocated in Unicode function $1")
 */

gen(1, 1914, INT_INVMATRIX, "Matrix [$1] not invertible")

gen(1, 1916, INT_REALLOC_TMP, "Illegal call to realloc_tmp() in function $1")

gen(0, 1918, INT_FREE_TMP, "Illegal call to free_tmp()")

gen(2, 1920, INT_ILLSWITCH, "Unexpected switch value $1 in function $2")

gen(2, 1922, INT_ARRIDX, "Illegal array index $1 in function $2")

gen(1, 1924, INT_ILLARG, "Invalid argument(s) in function $1")

gen(2, 1926, INT_ASSERT,
    "Internal error: assertion failed in file '$1', line $2")

gen(1, 1928, INT_STACK_UNDER, "Stack underflow in function $1")

gen(0, 1930, INT_TOOMUCH_SARE, "Too many save/restore operations")

gen(1, 1932, INT_TOOMUCH_INDOBJS, "Too many indirect objects (> $1)")

gen(1, 1934, INT_TOOLONG_TEXTSTR, "Text string too long (> $1)")

gen(1, 1940, INT_BADERRNO,
    "System IO error (file pointer = NULL, errno = 0); "
    "contact support@pdflib.com")

gen(1, 1950, INT_LONGNAME_MISSING, "Long name is missing at index $1")

gen(1, 1970, INT_ILLDOCTYPE, "Illegal document type '$1'")




#undef	gen
#undef	pdc_genNames
#undef	pdc_genInfo






