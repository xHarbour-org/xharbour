/* 
 * $id$
 *
 * This sample show howto use asynchronous/nonblocking queries
 *
 */

Function main()
    Local conn, res, aTemp, i, xTime
    
    CLEAR SCREEN
        
    ? "Connect", conn := PQConnect('test', '200.207.51.149', 'postgres', 'moreno', 5432)
                
    ? "Status Conexao", PQerrorMessage(conn), PQstatus(conn)
    
    ? "PQSendQuery", PQsendQuery(conn, 'SELECT codigo, emisao, vencto, saldup FROM duplic limit 5000')

    xTime := time()

    do while lastkey() != 27
        DevPos(Row(), 20)
        DevOut("Processing: " + Elaptime(xtime, time()))
        inkey(5)
        
        if PQconsumeInput(conn)
            if ! PQisBusy(conn)
                exit
            endif                    
        endif                
    enddo        
    
    if lastkey() != 27
        ? "PQgetResult", valtoprg(res := PQgetResult(conn))
    
        for x := 1 to PQlastrec(res)
            ? 
            for y := 1 to PQfcount(res)
                ?? PQgetvalue(res, x, y), " "
            next            
        next
    
        PQclear(res)
    else        
        ? "Canceling Query", PQrequestCancel(conn)
    endif
            
    PQclose(conn)

    return nil
    
