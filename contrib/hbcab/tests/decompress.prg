/*
   $Id$
   Sample program for extracting files from CAB file
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

/* State of extracting process for callback function */
#define CABINET_INFO    0
#define PARTIAL_FILE    1
#define COPY_FILE       2
#define CLOSE_FILE_INFO 3
#define NEXT_CABINET    4

#define EOL hb_OSNewLine()
#command ? => OutStd(EOL)
#command ? <xx,...> => OutStd(EOL);OutStd(<xx>)
#command ?? <xx,...> => OutStd(<xx>)

PROCEDURE MAIN()

   LOCAL cCabFile := "CABTEST1.CAB"
   LOCAL cDestDir /* := "E:\DEV\TESTS" if NIL, extract to EXE folder */
   LOCAL cResult
   LOCAL bCallBack := {|;
     nNotificationType,cNextCabinet,cNextDisk,cCabinetPath,nCabinetID,nNoCabinet,nUncompressedFileSize|;
     MYCALLBACK(nNotificationType,cNextCabinet,cNextDisk,cCabinetPath,nCabinetID,nNoCabinet,nUncompressedFileSize)}

   /*
      HB_DECOMPRESSCAB() returns string "OK" if success, else error message.
   */

   cResult := HB_DECOMPRESSCAB ( cCabFile, cDestDir, bCallBack )

   ?
   ? cResult
   ?

   RETURN

STATIC FUNCTION MYCALLBACK ( ;
        nNotificationType,;
        cNextCabinet,;
        cNextDisk,;
        cCabinetPath,;
        nCabinetID,;
        nNoCabinet,;
        nUncompressedFileSize)

   switch( nNotificationType )

   case CABINET_INFO
      ? "Notification       =", nNotificationType
      ? "  next cabinet     =", cNextCabinet
      ? "  next disk        =", cNextDisk
      ? "  cabinet path     =", cCabinetPath
      ? "  cabinet set ID   =", nCabinetID
      ? "  cabinet # in set =", nNoCabinet, " (zero based)"
      exit

   case PARTIAL_FILE
      ? "Notification                         =", nNotificationType
      ? "   name of continued file            =", cNextCabinet
      ? "   name of cabinet where file starts =", cNextDisk
      ? "   name of disk where file starts    =", cCabinetPath
      exit

   case COPY_FILE
      ? "Notification             =", nNotificationType
      ? "  file name in cabinet   =", cNextCabinet
      ? "  uncompressed file size =", nUncompressedFileSize
      exit

   case CLOSE_FILE_INFO
      ? "Notification            =", nNotificationType
      ? "   file name in cabinet =", cNextCabinet
      exit

   case NEXT_CABINET
      ? "Notification                                  =", nNotificationType
      ?  "   name of next cabinet where file continued =", cNextCabinet
      ?  "   name of next disk where file continued    =", cNextDisk
      ?  "   cabinet path name                         =", cCabinetPath
      exit
   end

   RETURN 0
