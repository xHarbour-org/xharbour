/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SevenZip xHarbour Interface - Definition File
 *
 * Copyright 2011 Andi Jahja <andi.jahja@yahoo.co.id>
 * www - http://www.xharbour.org
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
#ifndef __T7ZIP_INCLUDED
#define __T7ZIP_INCLUDED

#include "hbclass.ch"

#define ERROR_OK                	0x0000
#define ERROR_DISK_SPACE		0x8005
#define ERROR_READ_ONLY			0x8006
#define ERROR_USER_SKIP			0x8007
#define ERROR_UNKNOWN_TYPE		0x8008
#define ERROR_METHOD			0x8009
#define ERROR_PASSWORD_FILE		0x800A
#define ERROR_VERSION			0x800B
#define ERROR_FILE_CRC			0x800C
#define ERROR_FILE_OPEN			0x800D
#define ERROR_MORE_FRESH		0x800E
#define ERROR_NOT_EXIST			0x800F
#define ERROR_ALREADY_EXIST		0x8010
#define ERROR_TOO_MANY_FILES	        0x8011
#define ERROR_MAKEDIRECTORY		0x8012
#define ERROR_CANNOT_WRITE		0x8013
#define ERROR_HUFFMAN_CODE		0x8014
#define ERROR_COMMENT_HEADER	        0x8015
#define ERROR_HEADER_CRC		0x8016
#define ERROR_HEADER_BROKEN		0x8017
#define ERROR_ARC_FILE_OPEN		0x8018
#define ERROR_NOT_ARC_FILE		0x8019
#define ERROR_CANNOT_READ		0x801A
#define ERROR_FILE_STYLE		0x801B
#define ERROR_COMMAND_NAME		0x801C
#define ERROR_MORE_HEAP_MEMORY	        0x801D
#define ERROR_ENOUGH_MEMORY		0x801E
#define ERROR_ALREADY_RUNNING	        0x801F
#define ERROR_USER_CANCEL		0x8020
#define ERROR_HARC_ISNOT_OPENED	        0x8021
#define ERROR_NOT_SEARCH_MODE	        0x8022
#define ERROR_NOT_SUPPORT		0x8023
#define ERROR_TIME_STAMP		0x8024
#define ERROR_TMP_OPEN			0x8025
#define ERROR_LONG_FILE_NAME	        0x8026
#define ERROR_ARC_READ_ONLY		0x8027
#define ERROR_SAME_NAME_FILE	        0x8028
#define ERROR_NOT_FIND_ARC_FILE         0x8029
#define ERROR_RESPONSE_READ		0x802A
#define ERROR_NOT_FILENAME		0x802B
#define ERROR_TMP_COPY			0x802C
#define ERROR_EOF			0x802D
#define ERROR_ADD_TO_LARC		0x802E
#define ERROR_TMP_BACK_SPACE	        0x802F
#define ERROR_SHARING			0x8030
#define ERROR_NOT_FIND_FILE		0x8031
#define ERROR_LOG_FILE			0x8032
#define	ERROR_NO_DEVICE			0x8033
#define ERROR_GET_ATTRIBUTES	        0x8034
#define ERROR_SET_ATTRIBUTES	        0x8035
#define ERROR_GET_INFORMATION	        0x8036
#define ERROR_GET_POINT			0x8037
#define ERROR_SET_POINT			0x8038
#define ERROR_CONVERT_TIME		0x8039
#define ERROR_GET_TIME			0x803a
#define ERROR_SET_TIME			0x803b
#define ERROR_CLOSE_FILE		0x803c
#define ERROR_HEAP_MEMORY		0x803d
#define ERROR_HANDLE			0x803e
#define ERROR_TIME_STAMP_RANGE	        0x803f
#define ERROR_MAKE_ARCHIVE		0x8040
#define ERROR_NOT_CONFIRM_NAME	        0x8041
#define ERROR_UNEXPECTED_EOF	        0x8042
#define ERROR_INVALID_END_MARK	        0x8043
#define ERROR_INVOLVED_LZH		0x8044
#define ERROR_NO_END_MARK		0x8045
#define ERROR_HDR_INVALID_SIZE	        0x8046
#define ERROR_UNKNOWN_LEVEL		0x8047
#define ERROR_BROKEN_DATA		0x8048
#define ERROR_7ZIP_START        		0x8100
#define ERROR_WARNING			        0x8101
#define ERROR_FATAL				0x8102
#define ERROR_DURING_DECOMPRESSION		0x8103
#define ERROR_DIR_FILE_WITH_64BIT_SIZE		0x8104
#define ERROR_FILE_CHANGED_DURING_OPERATION	0x8105

STATIC AERRDEF := {;
     {"ERROR_DISK_SPACE",0x8005},{"ERROR_READ_ONLY",0x8006},;
     {"ERROR_USER_SKIP",0x8007},{"ERROR_UNKNOWN_TYPE",0X8008},;
     {"ERROR_METHOD",0x8009},{"ERROR_PASSWORD_FILE",0X800A},;
     {"ERROR_VERSION",0x800B},{"ERROR_FILE_CRC",0x800C},;
     {"ERROR_FILE_OPEN",0x800D},{"ERROR_MORE_FRESH",0x800E},;
     {"ERROR_NOT_EXIST",0x800F},{"ERROR_ALREADY_EXIST",0X8010},;
     {"ERROR_TOO_MANY_FILES",0X8011},{"ERROR_MAKEDIRECTORY",0X8012},;
     {"ERROR_CANNOT_WRITE",0X8013},{"ERROR_HUFFMAN_CODE",0X8014},;
     {"ERROR_COMMENT_HEADER",0X8015},{"ERROR_HEADER_CRC",0x8016},;
     {"ERROR_HEADER_BROKEN",0X8017},{"ERROR_ARC_FILE_OPEN",0X8018},;
     {"ERROR_NOT_ARC_FILE",0X8019},{"ERROR_CANNOT_READ",0X801A},;
     {"ERROR_FILE_STYLE",0x801B},{"ERROR_COMMAND_NAME",0X801C},;
     {"ERROR_MORE_HEAP_MEMORY",0X801D},{"ERROR_ENOUGH_MEMORY",0X801E},;
     {"ERROR_ALREADY_RUNNING",0X801F},{"ERROR_USER_CANCEL",0X8020},;
     {"ERROR_HARC_ISNOT_OPENED",0X8021},{"ERROR_NOT_SEARCH_MODE",0X8022},;
     {"ERROR_NOT_SUPPORT",0X8023},{"ERROR_TIME_STAMP",0x8024},;
     {"ERROR_TMP_OPEN",0x8025},{"ERROR_LONG_FILE_NAME",0X8026},;
     {"ERROR_ARC_READ_ONLY",0X8027},{"ERROR_SAME_NAME_FILE",0X8028},;
     {"ERROR_NOT_FIND_ARC_FILE",0X8029},{"ERROR_RESPONSE_READ",0X802A},;
     {"ERROR_NOT_FILENAME",0X802B},{"ERROR_TMP_COPY",0x802C},;
     {"ERROR_EOF",0x802D},{"ERROR_ADD_TO_LARC",0X802E},;
     {"ERROR_TMP_BACK_SPACE",0X802F},{"ERROR_SHARING",0x8030},;
     {"ERROR_NOT_FIND_FILE",0X8031},{"ERROR_LOG_FILE",0x8032},;
     {"ERROR_NO_DEVICE",0x8033},{"ERROR_GET_ATTRIBUTES",0X8034},;
     {"ERROR_SET_ATTRIBUTES",0X8035},{"ERROR_GET_INFORMATION",0X8036},;
     {"ERROR_GET_POINT",0x8037},{"ERROR_SET_POINT",0x8038},;
     {"ERROR_CONVERT_TIME",0X8039},{"ERROR_GET_TIME",0x803A},;
     {"ERROR_SET_TIME",0x803B},{"ERROR_CLOSE_FILE",0x803C},{"ERROR_HEAP_MEMORY",0X803D},;
     {"ERROR_HANDLE",0x803e},{"ERROR_TIME_STAMP_RANGE",0X803F},;
     {"ERROR_MAKE_ARCHIVE",0X8040},{"ERROR_NOT_CONFIRM_NAME",0X8041},;
     {"ERROR_UNEXPECTED_EOF",0X8042},{"ERROR_INVALID_END_MARK",0X8043},;
     {"ERROR_INVOLVED_LZH",0X8044},{"ERROR_NO_END_MARK",0X8045},;
     {"ERROR_HDR_INVALID_SIZE",0X8046},{"ERROR_UNKNOWN_LEVEL",0X8047},;
     {"ERROR_BROKEN_DATA",0X8048},{"ERROR_7ZIP_START",0x8100},{"ERROR_WARNING",0x8101},;
     {"ERROR_FATAL",0x8102},{"ERROR_DURING_DECOMPRESSION",0X8103},;
     {"ERROR_DIR_FILE_WITH_64BIT_SIZE",0X8104},{"ERROR_FILE_CHANGED_DURING_OPERATION",0X8105}}

STATIC aArcType   := { "7z", "zip", "gzip", "bzip2", "tar", "iso", "udf" }
STATIC aArcMethod := { "LZMA", "LZMA2", "PPMd", "BZip2", "Deflate", "Copy", "Deflate64" }

#define ARCTYPE_7Z     1
#define ARCTYPE_ZIP    2
#define ARCTYPE_GZIP   3
#define ARCTYPE_BZIP   4
#define ARCTYPE_TAR    5
#define ARCTYPE_ISO    6
#define ARCTYPE_UDF    7

#define CMPMETHOD_LZMA        1  // LZ-based algorithm
#define CMPMETHOD_LZMA2       2  // LZMA-based algorithm
#define CMPMETHOD_PPMD        3  // Dmitry Shkarin's PPMdH with small changes
#define CMPMETHOD_BZIP2       4  // BWT algorithm
#define CMPMETHOD_DEFLATE     5  // LZ+Huffman
#define CMPMETHOD_COPY        6  // No compression
#define CMPMETHOD_DEFLATE64   7  // LZ+Huffman

#endif /* __T7ZIP_INCLUDED */
