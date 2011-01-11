Short memo about Source Scanner
--------------------------------

Syntax:
 scanner <Path> ["<Comment>"]

Arguments:
 - Path: path with the files that need to be scanned. The scanner asks if the scanning
   should be recursive or not (scan subfolders or not).
 - Comment: a line of comment to add to the scanned documentation. When this argument is 
   empty a default value will be generated. The default value is "Uploaded with 
   SourceScanner 2.0 on dd/mm/yy hh:mm:ss by CURRENT LOGGED ON USERNAME"

Examples:
 scanner myfiles_to_scan\*.* "Initial scan by user usr124"
 scanner myfiles_to_scan\*.c "Scan only .c files in the folder"
 scanner myfiles_to_scan\*.prg "Initial scan of .prg files"
 scanner myfiles_to_scan\ "Scan all .c, .txt, .prg files in the folder"

Notes:
 - Use sqlrdd.ini to make new connectionstrings available to Source Scanner. Templates of 
   connectionstrings can be found on www.connectionstrings.com.