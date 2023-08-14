    C:\
    CD \
    RD C:\xHarbour.org /S /Q

    XCOPY C:\xHarbour\Changelog.*               xHarbour.org /y /i
    XCOPY C:\xHarbour\Copying.*                 xHarbour.org /y /i
                                                               

    XCOPY C:\xHarbour\Source\*.c                xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.prg              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.ch               xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.sl?              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.h                xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.y                xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.sfc              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.cpp              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.src              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.def              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.gcc              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.xpm              xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.h.generic        xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\*.dist             xHarbour.org\Source  /y /i /s
    XCOPY C:\xHarbour\Source\Changelog.*        xHarbour.org\Source  /y /i /s
                                              
                                              
    XCOPY C:\xHarbour\Contrib\rdd_ADS\*.prg     xHarbour.org\Contrib\rdd_ADS /y /i /s
    XCOPY C:\xHarbour\Contrib\rdd_ADS\*.c       xHarbour.org\Contrib\rdd_ADS /y /i /s
    XCOPY C:\xHarbour\Contrib\rdd_ADS\*.ch      xHarbour.org\Contrib\rdd_ADS /y /i /s
    XCOPY C:\xHarbour\Contrib\rdd_ADS\*.h       xHarbour.org\Contrib\rdd_ADS /y /i /s
    XCOPY C:\xHarbour\Contrib\rdd_ADS\*.txt     xHarbour.org\Contrib\rdd_ADS /y /i /s
                                              
    XCOPY C:\xHarbour\Contrib\GD\*.prg          xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.c            xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.html         xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.url          xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.txt          xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.ch           xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.h            xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\*.gif          xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\Changelog.*    xHarbour.org\Contrib\GD /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\Samples\*.*    xHarbour.org\Contrib\GD\Samples /y /i /s
    XCOPY C:\xHarbour\Contrib\GD\Tests\*.*      xHarbour.org\Contrib\GD\Tests /y /i /s

    XCOPY C:\xHarbour\Contrib\LibNF\*.prg       xHarbour.org\Contrib\LibNF /y /i /s
    XCOPY C:\xHarbour\Contrib\LibNF\*.c         xHarbour.org\Contrib\LibNF /y /i /s
    XCOPY C:\xHarbour\Contrib\LibNF\*.ch        xHarbour.org\Contrib\LibNF /y /i /s
    XCOPY C:\xHarbour\Contrib\LibNF\*.h         xHarbour.org\Contrib\LibNF /y /i /s
    XCOPY C:\xHarbour\Contrib\LibNF\Samples\*.* xHarbour.org\Contrib\LibNF\Samples /y /i /s

    XCOPY C:\xHarbour\Contrib\gtWVG\*.prg       xHarbour.org\Contrib\gtWVG /y /i /s
    XCOPY C:\xHarbour\Contrib\gtWVG\*.c         xHarbour.org\Contrib\gtWVG /y /i /s
    XCOPY C:\xHarbour\Contrib\gtWVG\*.ch        xHarbour.org\Contrib\gtWVG /y /i /s
    XCOPY C:\xHarbour\Contrib\gtWVG\*.h         xHarbour.org\Contrib\gtWVG /y /i /s
    XCOPY C:\xHarbour\Contrib\gtWVG\Tests\*.*   xHarbour.org\Contrib\gtWVG\Tests /y /i /s

    XCOPY C:\xHarbour\Tests\*.*                 xHarbour.org\Tests /y /i /s
    XCOPY C:\xHarbour\Utils\*.*                 xHarbour.org\Utils /y /i /s
    XCOPY C:\xHarbour\Doc\*.txt                 xHarbour.org\Doc   /y /i /s
    
    RDCVS xHarbour.org
    rar a -r %1 c:\xHarbour.org\