/*
 * Chr(36) + "Id" + Chr(36)
 */

Function main()
    Local conn, res, aTemp, i
    
    CLEAR SCREEN
    
    ? conn := PQConnect('test', '192.168.1.20', 'rodrigo', 'moreno', 5432)
   
    ? res := PQexecParams(conn, 'insert into products(product_no, name, price) values ($1, $2, $3)', {'2', 'bread', '10.95'})    
    ? res := PQexec(conn, 'select price, name, product_no as "produto" from products')
    
    aTemp := PQmetadata(res)
    
    for x := 1 to len(aTemp)
        ? "Linha 1: "
        for y := 1 to 7
            ?? aTemp[x,y], ", "
        next
    next
            
    
    ? PQFcount(res)
    
    ? PQlastrec(res)
    
    ? PQGetvalue(res,1, 2)

    ? PQclear(res)    
    
    ? PQClose(conn)
    return nil
    
