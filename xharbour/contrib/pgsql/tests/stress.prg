/*
 * Chr(36) + "Id" + Chr(36)
 */

#include "common.ch"
/* VERY IMPORTANT: Don't use this querys as sample, they are used for stress tests !!! */

Function Main()
    Local oServer, oQuery, oRow, i, x

    Local cServer := '192.168.1.20' 
    Local cDatabase := 'test'
    Local cUser := 'rodrigo'
    Local cPass := 'moreno'
    Local cQuery

    CLEAR SCREEN

    ? 'Connecting....'    
    oServer := PQconnect(cDatabase, cServer, cUser, cPass, 5432)

    if ISCHAR(oServer)
        ? oServer
        quit
    end
    
    ? 'Dropping table...'
    
    ? oQuery := PQexec(oServer, 'DROP TABLE test')
    
    ? PQclear(oQuery)

    ? 'Creating test table...'
    cQuery := 'CREATE TABLE test('
    cQuery += '     Code integer not null primary key, '
    cQuery += '     dept Integer, '
    cQuery += '     Name Varchar(40), '
    cQuery += '     Sales boolean, '
    cQuery += '     Tax Float4, '
    cQuery += '     Salary Double Precision, '
    cQuery += '     Budget Numeric(12,2), '
    cQuery += '     Discount Numeric (5,2), '
    cQuery += '     Creation Date, '
    cQuery += '     Description text ) '

    ? oQuery := PQexec(oServer, cQuery)
    
    ? PQclear(oQuery)

    ? oQuery := PQexec(oServer, 'SELECT code, dept, name, sales, salary, creation FROM test')

    ? PQclear(oQuery)

    ? oQuery := PQexec(oserver, 'BEGIN')    
    
    ? PQclear(oQuery)
     
    For i := 1 to 10000
        @ 15,0 say 'Inserting values....' + str(i)
        
        cQuery := 'INSERT INTO test(code, dept, name, sales, salary, creation) '
        cQuery += 'VALUES( ' + str(i) + ',' + str(i+1) + ", 'DEPARTMENT NAME " + strzero(i) + "', 'y', " + str(300.49+i) + ", '2003-12-28' )"

        ? oQuery := PQexec(oServer, cQuery)
    
        ? PQclear(oQuery)
                
        if mod(i,100) == 0
            ? oQuery := PQexec(oserver, 'COMMIT')        
            ? PQclear(oQuery)
            
            ? oQuery := PQexec(oserver, 'BEGIN')    
            ? PQclear(oQuery)
        end
    Next
    
    For i := 5000 to 7000
        @ 16,0 say 'Deleting values....' + str(i)

        cQuery := 'DELETE FROM test WHERE code = ' + str(i)
        ? oQuery := PQexec(oServer, cQuery)
    
        ? PQclear(oQuery)
        
        if mod(i,100) == 0
            ? oQuery := PQexec(oserver, 'COMMIT')        
            ? PQclear(oQuery)

            ? oQuery := PQexec(oserver, 'BEGIN')    
            ? PQclear(oQuery)
        end
    Next
    
    For i := 2000 to 3000
        @ 17,0 say 'Updating values....' + str(i)

        cQuery := 'UPDATE FROM test SET salary = 400 WHERE code = ' + str(i)
        ? oQuery := PQexec(oServer, cQuery)
        
        if mod(i,100) == 0
            ? oQuery := PQexec(oserver, 'COMMIT')        
            ? PQclear(oQuery)

            ? oQuery := PQexec(oserver, 'BEGIN')    
            ? PQclear(oQuery)
        end
    Next

    ? oQuery := PQexec(oServer, 'SELECT sum(salary) as sum_salary FROM test WHERE code between 1 and 4000')

    if ! ISCHAR(oquery)
        @ 18,0 say 'Sum values....' + PQgetvalue(oquery, 1, 1)    
    end

    ? PQclear(oQuery)

    x := 0
    For i := 1 to 4000
        oQuery := PQexec(oServer, 'SELECT salary FROM test WHERE code = ' + str(i))
        
        if ! ISCHAR(oQuery)
            x += val(PQgetvalue(oquery, 1, 1))    
            
            @ 19,0 say 'Sum values....' + str(x)
        end            
    Next   
    
    ? "Closing..."
    PQclose(oServer)
    
return nil
