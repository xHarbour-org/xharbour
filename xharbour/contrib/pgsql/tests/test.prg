#define CONNECTION_OK                   0
#define CONNECTION_BAD                  1
#define CONNECTION_STARTED              2
#define CONNECTION_MADE                 3
#define CONNECTION_AWAITING_RESPONSE    4
#define CONNECTION_AUTH_OK              5
#define CONNECTION_SETENV               6
#define CONNECTION_SSL_STARTUP          7
#define CONNECTION_NEEDED               8

#define PGRES_EMPTY_QUERY               0
#define PGRES_COMMAND_OK                1
#define PGRES_TUPLES_OK                 2
#define PGRES_COPY_OUT                  3
#define PGRES_COPY_IN                   4
#define PGRES_BAD_RESPONSE              5
#define PGRES_NONFATAL_ERROR            6
#define PGRES_FATAL_ERROR               7

#define PQTRANS_IDLE                    0
#define PQTRANS_ACTIVE                  1
#define PQTRANS_INTRANS                 2
#define PQTRANS_INERROR                 3
#define PQTRANS_UNKNOWN                 4

Function main()
    Local conn, res, aTemp, i
    
    CLEAR SCREEN
    
    conn := PQConnect('test', 'localhost', 'rodrigo', 'moreno', 5432)
    ? PQstatus(conn), PQerrormessage(conn)
    
    if PQstatus(conn) != CONNECTION_OK
        quit
    endif
   
    res := PQexecParams(conn, 'insert into products(product_no, name, price) values ($1, $2, $3)', {'2', 'bread', '10.95'})    
    ? PQresultStatus(res), PQresultErrorMessage(conn)
    
    if PQresultStatus(res) != PGRES_COMMAND_OK
        quit
    endif
    PQclear(res)
    
    res := PQexec(conn, 'select price, name, product_no as "produto" from products')
    ? PQresultStatus(res), PQresultErrorMessage(conn)
    
    if PQresultStatus(res) != PGRES_TUPLES_OK
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
    
