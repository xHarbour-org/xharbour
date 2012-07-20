/*
   $Id$
   Sample program for CAB file creation
   Andi Jahja
*/

#message ""
#message ".---------------------------------."
#message "| This program requires HBCAB.LIB |"
#message "`---------------------------------'"
#message ""

EXTERNAL HB_GT_WIN_DEFAULT

/* CAB file info */
#define CABINET_FILE_FILE_NAME         1
#define TOTAL_LENGTH_OF_CABINET_FILE   2
#define NUMBER_OF_FOLDERS_IN_CABINET   3
#define NUMBER_OF_FILES_IN_CABINET     4
#define CABINET_SET_ID                 5
#define CABINET_NUMBER_IN_SET          6
#define RESERVE_AREA_IN_CABINET        7
#define CHAINED_TO_PREV_CABINET        8
#define CHAINED_TO_NEXT_CABINET        9

STATIC cs := { 0 , 0 }

#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)

PROCEDURE MAIN()

   LOCAL aFile := { "cabtest1.txt", "cabtest2.txt", "cabtest3.txt", "cabtest4.txt" }
   /* aFile is an array containing files to be compressed */
   LOCAL szMask := "CABTEST" /* self-explanatory */
   LOCAL szDirPath  /* "E:\TEMP" self explanatory -> NIL = same dir as EXE */
   LOCAL cResult
   LOCAL nSegment := 500000 /* CAB file size */
   LOCAL nFlushBuffer := 1000000 /* Flush buffer each of this interval */
   LOCAL nCabID := 54321 /* cabID -> integer */
   LOCAL cCabStringID := "xHarbour"
   LOCAL bCallBack := { | p1, p2, p3, p4, p5 | if( p5 == 1, FILE_PLACED( p1, p2, p3, p4 ), STATUS( p1, p2, p3 ) ) }
   LOCAL aInfo
   /*
      Attention on Callback parameter (p5)
      1 = CAB processing
      2 = Status of compression
      Please refer to compress.c where the codeblock is executed
      This is user-defined, and optional
   */

   cs := { 0 , 0 } // Initialized for status meter

   /*
      HB_CREATECAB() returns string "OK" if success, else error message.
   */
   cResult := HB_CREATECAB(;
                 aFile       ,;
                 szMask      ,;
                 szDirPath   ,;
                 nSegment    ,;
                 nFlushBuffer,;
                 bCallBack   ,;
                 nCabID      ,;
                 cCabStringID )
   ?
   ? cResult

   IF cResult == "OK"
      IF !EMPTY( aInfo := HB_CABINFO( "cabtest1.cab" ) )
         ? "Information on cabinet file :", aInfo[CABINET_FILE_FILE_NAME      ]
         ? "Total length of cabinet file:", aInfo[TOTAL_LENGTH_OF_CABINET_FILE]
         ? "Number of folders in cabinet:", aInfo[NUMBER_OF_FOLDERS_IN_CABINET]
         ? "Number of files in cabinet  :", aInfo[NUMBER_OF_FILES_IN_CABINET  ]
         ? "Cabinet set ID              :", aInfo[CABINET_SET_ID              ]
         ? "Cabinet number in set       :", aInfo[CABINET_NUMBER_IN_SET       ]
         ? "RESERVE area in cabinet?    :", IF(aInfo[RESERVE_AREA_IN_CABINET] == 1, "yes", "no" )
         ? "Chained to prev cabinet?    :", IF(aInfo[CHAINED_TO_PREV_CABINET] == 1, "yes", "no" )
         ? "Chained to next cabinet?    :", IF(aInfo[CHAINED_TO_NEXT_CABINET] == 1, "yes", "no" )
      ENDIF
   ENDIF

   RETURN

/*
   Attention on Callback parameters:
   p1 = CAB file name in progress (string)
   p2 = file being processed (string)
   p3 = size of file being processed (integer)
   p4 = status of continuation in multiple CABs (boolean)
   This is user-defined, and optional
*/
STATIC FUNCTION FILE_PLACED( p1, p2, p3, p4 )

   ? sprintf( "   placed file '%s' (size %d) on cabinet '%s'", p2, p3, p1 )

   IF p4
      ? sprintf( "      (Above file is a later segment of a continued file)" )
   ENDIF

   RETURN 0

/*
   Attention on Callback parameters:
   p1 = Status ( integer ) -> 0 = compressing File, 1 = creating CAB file
   p2 = compressed file ( integer )
   p3 = uncompressed file ( integer )
   This is user-defined, and optional
*/
STATIC FUNCTION STATUS( p1, p2, p3 )

   LOCAL PERCENTAGE

   IF ( p1 == 0 )
      cs[1] += p2
      cs[2] += p3
      ? sprintf( "Compressing: %l -> %l", cs[1], cs[2] )
   ELSEIF ( p1 == 1 )
      percentage = get_percentage( p2, p3 )
      ? sprintf( "Copying folder to cabinet: %d%%", percentage )
   ENDIF

   RETURN 0

STATIC FUNCTION get_percentage( a, b )

   IF( b == 0 )
      RETURN 0
   ENDIF

   RETURN ( a * 100 ) / b
