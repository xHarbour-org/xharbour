/*
 *
 * $Id: test.prg,v 1.5 2004/04/30 18:23:35 rodrigo_moreno Exp $
 *
 */

#include "..\postgres.ch"

Function main()
    Local conn, res, aTemp, i
    
    CLEAR SCREEN
    
    conn := PQConnect('test', 'localhost', 'rodrigo', 'moreno', 5432)
    ? PQstatus(conn), PQerrormessage(conn)
    
    if PQstatus(conn) != CONNECTION_OK
        quit
    endif
   
    res := PQexecParams(conn, 'insert into products(product_no, name, price) values ($1, $2, $3)', {'2', 'bread', '10.95'})    
    ? PQresultStatus(res), PQresultErrorMessage(res)
    
    if PQresultStatus(res) != PGRES_COMMAND_OK
        PQclear(res)
        PQclose(conn)
        quit
    endif
    PQclear(res)
    
    res := PQexec(conn, 'select price, name, product_no as "produto" from products')
    ? PQresultStatus(res), PQresultErrorMessage(res)
    
    if PQresultStatus(res) != PGRES_TUPLES_OK
        PQclear(res)
        PQclose(conn)
        quit
    endif
    
    aTemp := PQmetadata(res)
    
    for x := 1 to len(aTemp)
        ? "Linha 1: "
        for y := 1 to 6
            ?? aTemp[x,y], ", "
        next
    next
                
    ? PQFcount(res)
    
    ? PQlastrec(res)
    
    ? PQGetvalue(res,1, 2)

    ? PQclear(res)    
    
    ? PQClose(conn)
    return nil
    
