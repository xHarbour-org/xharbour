set PATH=d:\softools\mingw\bin
set HB_INSTALL=\harbour
set HWGUI_INSTALL=..

   %HB_INSTALL%\bin\w32\harbour %1.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include %2
   gcc -I. -I%HB_INSTALL%\include -mno-cygwin -Wall -c %1.c -o%1.o
   gcc -Wall -o%1.exe %1.o  -Ld:\softools\mingw\lib -L%HB_INSTALL%\lib\w32 -L%HWGUI_INSTALL%\lib -mno-cygwin -ldebug -lvm -lrdd -lrtl -lvm -lmacro -lpp -llang -lcommon -lnulsys  -ldbfntx  -ldbfcdx -lgtwin -lhwgui -lprocmisc -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdi32 -lrtl
   del %1.c
   del %1.o
