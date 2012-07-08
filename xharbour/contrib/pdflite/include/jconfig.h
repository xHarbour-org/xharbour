/* jconfig.h.  Generated automatically by configure.  */
/* jconfig.cfg --- source file edited by configure script */
/* see jconfig.doc for explanations */

#define HAVE_PROTOTYPES
#define HAVE_UNSIGNED_CHAR
#define HAVE_UNSIGNED_SHORT
#undef void
#undef const
#undef CHAR_IS_UNSIGNED
/* #define HAVE_STDDEF_H PDFlib not needed */
#ifndef HAVE_STDLIB_H
#define HAVE_STDLIB_H
#endif
#undef NEED_BSD_STRINGS
#undef NEED_SYS_TYPES_H
#undef NEED_FAR_POINTERS
#undef NEED_SHORT_EXTERNAL_NAMES
/* Define this if you get warnings about undefined structures. */
#undef INCOMPLETE_TYPES_BROKEN

/* PDFlib GmbH: Avoid the the following message:
 *
 * "JPEGLib: JPEG parameter struct mismatch: library thinks size is 432,
   caller expects 464"
 */
#if defined(WIN32) && !defined(HAVE_BOOLEAN)
/* Define "boolean" as unsigned char, not int, per Windows custom */
#ifndef __RPCNDR_H__            /* don't conflict if rpcndr.h already read */
typedef unsigned char boolean;
#endif
#define HAVE_BOOLEAN            /* prevent jmorecfg.h from redefining it */
#endif  /* WIN32 */

#ifdef JPEG_INTERNALS

#undef RIGHT_SHIFT_IS_UNSIGNED
#define INLINE /**/
/* These are for configuring the JPEG memory manager. */
#undef DEFAULT_MAX_MEM
#undef NO_MKTEMP

#endif /* JPEG_INTERNALS */

#ifdef JPEG_CJPEG_DJPEG

#define BMP_SUPPORTED		/* BMP image file format */
#define GIF_SUPPORTED		/* GIF image file format */
#define PPM_SUPPORTED		/* PBMPLUS PPM/PGM image file format */
#undef RLE_SUPPORTED		/* Utah RLE image file format */
#define TARGA_SUPPORTED		/* Targa image file format */

#undef TWO_FILE_COMMANDLINE
#undef NEED_SIGNAL_CATCHER
#undef DONT_USE_B_MODE

/* Define this if you want percent-done progress reports from cjpeg/djpeg. */
#undef PROGRESS_REPORT

#endif /* JPEG_CJPEG_DJPEG */

/*
 * PDFlib GmbH: overrides define in jmemmgr.c
 * Otherwise program crashes because of not aligned pointers
 */
#define NO_GETENV               /* RJS makes problems in windows debug builds */

#if defined __ILEC400__ && !defined AS400
#define AS400
#endif
#if defined(AS400)
#ifndef ALIGN_TYPE              /* overrides define in jmemmgr.c */
#define ALIGN_TYPE  char *
#endif
#endif

/*
 * PDFlib GmbH:  we allow the use of PDFlib inside of programs using
 * another instance of libjpeg
 */

#ifdef PDFLIB_TET_BUILD
#define JPEG_PREFIX(x) tet_##x
#else
#define JPEG_PREFIX(x) pdf_##x
#endif /* PDFLIB_TET_BUILD */


#define jpeg_abort_compress	 JPEG_PREFIX(jpeg_abort_compress)
#define jpeg_CreateCompress	 JPEG_PREFIX(jpeg_CreateCompress)
#define jpeg_destroy_compress	 JPEG_PREFIX(jpeg_destroy_compress)
#define jpeg_finish_compress	 JPEG_PREFIX(jpeg_finish_compress)
#define jpeg_suppress_tables	 JPEG_PREFIX(jpeg_suppress_tables)
#define jpeg_write_marker	 JPEG_PREFIX(jpeg_write_marker)
#define jpeg_write_m_byte	 JPEG_PREFIX(jpeg_write_m_byte)
#define jpeg_write_m_header	 JPEG_PREFIX(jpeg_write_m_header)
#define jpeg_write_tables	 JPEG_PREFIX(jpeg_write_tables)
#define jpeg_start_compress	 JPEG_PREFIX(jpeg_start_compress)
#define jpeg_write_raw_data	 JPEG_PREFIX(jpeg_write_raw_data)
#define jpeg_write_scanlines	 JPEG_PREFIX(jpeg_write_scanlines)
#define jinit_c_coef_controller	 JPEG_PREFIX(jinit_c_coef_controller)
#define jinit_color_converter	 JPEG_PREFIX(jinit_color_converter)
#define jinit_forward_dct	 JPEG_PREFIX(jinit_forward_dct)
#define jinit_huff_encoder	 JPEG_PREFIX(jinit_huff_encoder)
#define jpeg_gen_optimal_table	 JPEG_PREFIX(jpeg_gen_optimal_table)
#define jpeg_make_c_derived_tbl	 JPEG_PREFIX(jpeg_make_c_derived_tbl)
#define jinit_compress_master	 JPEG_PREFIX(jinit_compress_master)
#define jinit_c_main_controller	 JPEG_PREFIX(jinit_c_main_controller)
#define jinit_marker_writer	 JPEG_PREFIX(jinit_marker_writer)
#define jinit_c_master_control	 JPEG_PREFIX(jinit_c_master_control)
#define jpeg_abort	 JPEG_PREFIX(jpeg_abort)
#define jpeg_alloc_huff_table	 JPEG_PREFIX(jpeg_alloc_huff_table)
#define jpeg_alloc_quant_table	 JPEG_PREFIX(jpeg_alloc_quant_table)
#define jpeg_destroy	 JPEG_PREFIX(jpeg_destroy)
#define jpeg_add_quant_table	 JPEG_PREFIX(jpeg_add_quant_table)
#define jpeg_default_colorspace	 JPEG_PREFIX(jpeg_default_colorspace)
#define jpeg_quality_scaling	 JPEG_PREFIX(jpeg_quality_scaling)
#define jpeg_set_colorspace	 JPEG_PREFIX(jpeg_set_colorspace)
#define jpeg_set_defaults	 JPEG_PREFIX(jpeg_set_defaults)
#define jpeg_set_linear_quality	 JPEG_PREFIX(jpeg_set_linear_quality)
#define jpeg_set_quality	 JPEG_PREFIX(jpeg_set_quality)
#define jpeg_simple_progression	 JPEG_PREFIX(jpeg_simple_progression)
#define jinit_phuff_encoder	 JPEG_PREFIX(jinit_phuff_encoder)
#define jinit_c_prep_controller	 JPEG_PREFIX(jinit_c_prep_controller)
#define jinit_downsampler	 JPEG_PREFIX(jinit_downsampler)
#define jpeg_copy_critical_parameters	 JPEG_PREFIX(jpeg_copy_critical_param)
#define jpeg_write_coefficients	 JPEG_PREFIX(jpeg_write_coefficients)
#define jpeg_abort_decompress	 JPEG_PREFIX(jpeg_abort_decompress)
#define jpeg_consume_input	 JPEG_PREFIX(jpeg_consume_input)
#define jpeg_CreateDecompress	 JPEG_PREFIX(jpeg_CreateDecompress)
#define jpeg_destroy_decompress	 JPEG_PREFIX(jpeg_destroy_decompress)
#define jpeg_finish_decompress	 JPEG_PREFIX(jpeg_finish_decompress)
#define jpeg_has_multiple_scans	 JPEG_PREFIX(jpeg_has_multiple_scans)
#define jpeg_input_complete	 JPEG_PREFIX(jpeg_input_complete)
#define jpeg_read_header	 JPEG_PREFIX(jpeg_read_header)
#define jpeg_finish_output	 JPEG_PREFIX(jpeg_finish_output)
#define jpeg_read_raw_data	 JPEG_PREFIX(jpeg_read_raw_data)
#define jpeg_read_scanlines	 JPEG_PREFIX(jpeg_read_scanlines)
#define jpeg_start_decompress	 JPEG_PREFIX(jpeg_start_decompress)
#define jpeg_start_output	 JPEG_PREFIX(jpeg_start_output)
#define jpeg_stdio_dest	 JPEG_PREFIX(jpeg_stdio_dest)
#define jpeg_stdio_src	 JPEG_PREFIX(jpeg_stdio_src)
#define jinit_d_coef_controller	 JPEG_PREFIX(jinit_d_coef_controller)
#define jinit_color_deconverter	 JPEG_PREFIX(jinit_color_deconverter)
#define jinit_inverse_dct	 JPEG_PREFIX(jinit_inverse_dct)
#define jinit_huff_decoder	 JPEG_PREFIX(jinit_huff_decoder)
#define jpeg_fill_bit_buffer	 JPEG_PREFIX(jpeg_fill_bit_buffer)
#define jpeg_huff_decode	 JPEG_PREFIX(jpeg_huff_decode)
#define jpeg_make_d_derived_tbl	 JPEG_PREFIX(jpeg_make_d_derived_tbl)
#define jpeg_reset_huff_decode	 JPEG_PREFIX(jpeg_reset_huff_decode)
#define jinit_input_controller	 JPEG_PREFIX(jinit_input_controller)
#define jinit_d_main_controller	 JPEG_PREFIX(jinit_d_main_controller)
#define jinit_marker_reader	 JPEG_PREFIX(jinit_marker_reader)
#define jpeg_resync_to_restart	 JPEG_PREFIX(jpeg_resync_to_restart)
#define jpeg_save_markers	 JPEG_PREFIX(jpeg_save_markers)
#define jpeg_set_marker_processor JPEG_PREFIX(jpeg_set_marker_processor)
#define jinit_master_decompress	 JPEG_PREFIX(jinit_master_decompress)
#define jpeg_calc_output_dimensions JPEG_PREFIX(jpeg_calc_output_dimensions)
#define jpeg_new_colormap	 JPEG_PREFIX(jpeg_new_colormap)
#define jinit_merged_upsampler	 JPEG_PREFIX(jinit_merged_upsampler)
#define jinit_phuff_decoder	 JPEG_PREFIX(jinit_phuff_decoder)
#define jinit_d_post_controller	 JPEG_PREFIX(jinit_d_post_controller)
#define jinit_upsampler	 JPEG_PREFIX(jinit_upsampler)
#define jpeg_read_coefficients	 JPEG_PREFIX(jpeg_read_coefficients)
#define jpeg_std_error	 JPEG_PREFIX(jpeg_std_error)
#define jpeg_std_message_table	 JPEG_PREFIX(jpeg_std_message_table)
#define jpeg_fdct_float	 JPEG_PREFIX(jpeg_fdct_float)
#define jpeg_fdct_ifast	 JPEG_PREFIX(jpeg_fdct_ifast)
#define jpeg_fdct_islow	 JPEG_PREFIX(jpeg_fdct_islow)
#define jpeg_idct_float	 JPEG_PREFIX(jpeg_idct_float)
#define jpeg_idct_ifast	 JPEG_PREFIX(jpeg_idct_ifast)
#define jpeg_idct_islow	 JPEG_PREFIX(jpeg_idct_islow)
#define jpeg_idct_1x1	 JPEG_PREFIX(jpeg_idct_1x1)
#define jpeg_idct_2x2	 JPEG_PREFIX(jpeg_idct_2x2)
#define jpeg_idct_4x4	 JPEG_PREFIX(jpeg_idct_4x4)
#define jinit_memory_mgr	 JPEG_PREFIX(jinit_memory_mgr)
#define jpeg_free_large	 JPEG_PREFIX(jpeg_free_large)
#define jpeg_free_small	 JPEG_PREFIX(jpeg_free_small)
#define jpeg_get_large	 JPEG_PREFIX(jpeg_get_large)
#define jpeg_get_small	 JPEG_PREFIX(jpeg_get_small)
#define jpeg_mem_available	 JPEG_PREFIX(jpeg_mem_available)
#define jpeg_mem_init	 JPEG_PREFIX(jpeg_mem_init)
#define jpeg_mem_term	 JPEG_PREFIX(jpeg_mem_term)
#define jpeg_open_backing_store	 JPEG_PREFIX(jpeg_open_backing_store)
#define jinit_1pass_quantizer	 JPEG_PREFIX(jinit_1pass_quantizer)
#define jinit_2pass_quantizer	 JPEG_PREFIX(jinit_2pass_quantizer)
#define jcopy_block_row	 JPEG_PREFIX(jcopy_block_row)
#define jcopy_sample_rows	 JPEG_PREFIX(jcopy_sample_rows)
#define jdiv_round_up	 JPEG_PREFIX(jdiv_round_up)
#define jpeg_natural_order	 JPEG_PREFIX(jpeg_natural_order)
#define jround_up	 JPEG_PREFIX(jround_up)
#define jzero_far	 JPEG_PREFIX(jzero_far)

#if defined(__BORLANDC__)
   #pragma warn -prc
   #pragma warn -pia
   #pragma warn -rch
   #pragma warn -csu
   #pragma warn -aus
   #pragma warn -ccc
   #pragma warn -def
   #pragma warn -sig
   #pragma warn -sus
   #pragma warn -use
   #pragma warn -eff
   #pragma warn -dup
#endif

#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif
