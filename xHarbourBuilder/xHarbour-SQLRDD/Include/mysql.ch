
#ifndef MYSQL_CH

#define ifndef MYSQL_CH


// MySQL field types

#define  MYSQL_DECIMAL_TYPE      0
#define  MYSQL_TINY_TYPE         1
#define  MYSQL_SHORT_TYPE        2
#define  MYSQL_LONG_TYPE         3
#define  MYSQL_FLOAT_TYPE        4
#define  MYSQL_DOUBLE_TYPE       5
#define  MYSQL_NULL_TYPE         6
#define  MYSQL_TIMESTAMP_TYPE    7
#define  MYSQL_LONGLONG_TYPE     8
#define  MYSQL_INT24_TYPE        9
#define  MYSQL_DATE_TYPE         10
#define  MYSQL_TIME_TYPE         11
#define  MYSQL_DATETIME_TYPE     12
#define  MYSQL_YEAR_TYPE         13
#define  MYSQL_NEWDATE_TYPE      14
#define  MYSQL_NEWDECIMAL_TYPE   246
#define  MYSQL_ENUMTYPE          247
#define  MYSQL_SET_TYPE          248
#define  MYSQL_TINY_BLOB_TYPE    249
#define  MYSQL_MEDIUM_BLOB_TYPE  250
#define  MYSQL_LONG_BLOB_TYPE    251
#define  MYSQL_BLOB_TYPE         252
#define  MYSQL_VAR_STRING_TYPE   253
#define  MYSQL_STRING_TYPE       254

#define  MYSQL_FS_NAME           1     /* Name of column */
#define  MYSQL_FS_TABLE          2     /* Table of column if column was a field */
#define  MYSQL_FS_DEF            3     /* Default value (set by mysql_list_fields) */
#define  MYSQL_FS_TYPE           4     /* Type of field. Se mysql_com.h for types */
#define  MYSQL_FS_LENGTH         5     /* Width of column */
#define  MYSQL_FS_MAXLEN         6     /* Max width of selected set */
#define  MYSQL_FS_FLAGS          7     /* Div flags */
#define  MYSQL_FS_DECIMALS       8     /* Number of decimals in field */

// MySQL field flags

#define  NOT_NULL_FLAG           1     /* Field can't be NULL */
#define  PRI_KEY_FLAG            2     /* Field is part of a primary key */
#define  UNIQUE_KEY_FLAG         4		/* Field is part of a unique key */
#define  MULTIPLE_KEY_FLAG       8		/* Field is part of a key */
#define  BLOB_FLAG               16		/* Field is a blob */
#define  UNSIGNED_FLAG           32		/* Field is unsigned */
#define  ZEROFILL_FLAG           64		/* Field is zerofill */
#define  BINARY_FLAG             128

#endif
