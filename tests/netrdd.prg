ANNOUNCE RDDSYS

REQUEST DBFCDX,REDBFCDX,BMDBFCDX

FUNCTION MAIN( cRdd, cIP )
    LOCAL n,t,m,p,J, pConn
    LOCAL aBuffSize, nBuffSize, nNewTime, nTime, nBestBuffSize

    IF Empty( cRdd )
        cRdd := "REDBFCDX"
    ENDIF

    IF Empty( cIP )
        cIP := "127.0.0.1"
    ELSE
        cIP := AllTrim( cIP )
    ENDIF

    SET DATE FORMAT "DD/MM/YYYY"
    RDDSETDEFAULT( cRdd )

    SET DELETED ON
    SET AUTOPEN ON

    IF cRdd == 'REDBFCDX'
        ? "CONECTED TO IP: " + cIP + ":2813"
        pConn := NET_OPENCONNECTION( cIP, 2813 )
        IF Empty( pConn )
            ? "SERVER DON'T WORK"
            RETURN
        END
    ENDIF

    IF ! HB_DBExists( "TMPTEST" )
        ? "CREATE TABLE"
        DBCREATE( "TMPTEST", { { "A1", "C", 10, 0 } }, cRdd )
    ENDIF

    USE TMPTEST SHARED NEW ALIAS "ONE" VIA ( cRdd )

    IF LastRec() < 50000
        ? "Adding 50000 Rregisters"
        p := seconds()
        FOR n := 1 TO 50000
            APPEND BLANK
            REPLACE FIELD->A1 WITH "0123456789"
        NEXT
        ?? " -> " + AllTrim( Str( seconds() - p ) ) + " seconds"
    ENDIF

    SET OPTIMIZE ON
    IF HB_DBExists( "TMPTEST.CDX" )
        OrdSetFocus( "TG1" )
    ELSE
        INDEX ON FIELD->A1 TAG tg1 TO ("TMPTEST.CDX")
    ENDIF
    
    // Determine best buffer size for transmisions
    IF cRdd == 'REDBFCDX'
       ? " "
       aBuffSize := { 1024, 2048, 4096, 8192, 16384, 32768, 63488 }
       FOR EACH nBuffSize IN aBuffSize
          Net_SetBufferSize( nBuffSize )
          p := seconds()
          NET_COPYFROM( "TMPTEST.DBF", "TMPTEST.DBF" )
          nNewTime := seconds() - p
          IF Empty( nTime ) .OR. nNewTime <= nTime
             nTime := nNewTime
             nBestBuffSize := nBuffSize
          ENDIF
          ? " with buffer " + AllTrim( Str( nBuffSize / 1024 ) ) + " Kb " + AllTrim( Str( nNewTime ) ) + " seconds"
       NEXT
       ? " "
       ? "Established " + AllTrim( Str( nBestBuffSize / 1024 ) ) + " Kb of buffer"
       Net_SetBufferSize( nBestBuffSize )
       ? " "
    ENDIF
    

    ? "Index active : " + ORDKEY()
    ? "Filtering by : " + "RecNo() > 100 .AND. RecNo() < 200"
    p := seconds()
    DbSetFilter( {|| RecNo() > 100 .AND. RecNo() < 200 }, "RecNo() > 100 .AND. RecNo() < 200" )
    ?? " -> " + AllTrim( Str( seconds() - p ) ) + " seconds"

    WAIT
    p := t := seconds()
    FOR M := 1 TO 10
        ? "PASO " + AllTrim( str( M ) )
        DBGOTOP()
        FOR n := 1 TO 50000
            J := ORDKEYNO()
            J := ORDKEYCOUNT()
            DBSKIP()
        NEXT
        ?? " en " + AllTrim( Str( seconds() - p ) ) + " seconds"
        p := seconds()
    NEXT

    DBCLOSEALL()

    NET_CLOSECONNECTION( pConn )

    ? " TOTAL " + AllTrim( Str( seconds() - t ) ) + " seconds"
    WAIT

RETURN NIL



