#include "html.ch"
#include "default.ch"


PROC TestHtmlOutput()
LOCAL i,j,oHtm
LOCAL aDir := DIRECTORY( "C:\*.*" )



DEFINE HTML ;
       FILE "test.htm" ;
       TITLE "Test HTML page" ;
       OF oHtm

oHtm:setCenter(.T.)

DEFINE FONT "verdana" BOLD SIZE 3 of ohtm

DEFINE TABLE ;
       BORDER 1 ;
       COLS 6 ;
       WIDTH 80 ;
       COLORBG LIGHTGREY ;
       COLORFORE "black" ;
       3D ;
       OF oHtm

oHtm:newtablerow()
DEFINE CELL COLOR "yellow" FONTCOLOR "black";
       COLSPAN 6 ;
       CENTER ;
       OF oHtm
oHtm:qout("<c>colspan 1</c>" )
oHtm:EndtableCell()
oHtm:EndtableRow()
oHtm:newtablerow()
DEFINE CELL COLOR "red" FONTCOLOR "white";
       COLSPAN 3 ;
       OF oHtm
oHtm:qout("colspan 3" )
oHtm:EndtableCell()
DEFINE CELL COLOR "green" FONTCOLOR "white";
       COLSPAN 3 ;
       OF oHtm
oHtm:qout("colspan 3" )
oHtm:EndtableCell()
oHtm:Endtablerow()

FOR i=1 TO LEN( aDir )

    oHtm:newtablerow()
      for j=1 to 4
          DEFINE CELL WIDTH 20 ;
          OF oHtm
          oHtm:qout( any2str(aDir[i,j]) )
oHtm:EndtableCell()
     next j
          DEFINE CELL FONTCOLOR "blue" SIZE 2 WIDTH 40 ;
          OF oHtm
          oHtm:qout( any2str(aDir[i,1]) )
oHtm:EndtableCell()
          DEFINE CELL FONTCOLOR "white" ;
          WIDTH 20 ;
          OF oHtm
          oHtm:qout( any2str(aDir[i,2]) )
oHtm:EndtableCell()
    oHtm:Endtablerow()
NEXT i

oHtm:newtablerow()
TABLE CELL COLOR "black" FONTCOLOR "yellow";
       COLSPAN 6 ;
       RIGHT ;
       OF oHtm
oHtm:qout(memoread("c:\config.sys" ))
oHtm:EndtableCell()
oHtm:EndtableRow()


oHtm:endTable()
oHtm:close()

//? 'START IEXPLORE ' +CURDIR()+'\TEST.HTM'
//RUN ('START IEXPLORE ' +CURDIR()+'\TEST.HTM')
RUN ('START C:\' +CURDIR()+'\TEST.HTM')
RETURN
