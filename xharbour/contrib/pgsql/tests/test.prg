Function main()
    Local conn, res, aTemp, i
    
    CLEAR SCREEN
    
    aTemp := Struct( {"Rodrigo", "Juliana", "Moreno"} )
    
    For i := 1 to len(aTemp)
        ? aTemp[i]    
    Next
    
    quit
    
    ? conn := PQConnect('test', '192.168.1.20', 'rodrigo', 'moreno', 5432)
    
    ? res := PQexec(conn, 'select price, name, product_no as "produto" from products')
    
    aTemp := PQmetadata(res)
    
    for x := 1 to len(aTemp)
        ? "Linha 1: "
        for y := 1 to 7
            ?? aTemp[x,y], ", "
        next
    next
            
    
    //? PQFcount(res)
    
    //? PQlastrec(res)
    
    //? PQGetvalue(res,1, 2)

    ? PQclear(res)    
    
    ? PQClose(conn)
    return nil
    
/*

select relname from pg_class where relfilenode = 17143

select * from pg_class order by relfilenode

select * from pg_attribute order by atttypid

select * from pg_type order by typelem

select * from pg_constraint

select a.attname as column, c.conname as constraint, d.relname as table
from pg_attribute a, pg_index b, pg_constraint c, pg_class d
where a.attrelid = b.indexrelid and 
      b.indrelid = c.conrelid and
      b.indrelid = d.relfilenode 
      
select a.relname, b.*
from pg_class a, pg_type b
where a.relfilenode = b.typrelid and
      a.relname in ('test', 'products')

select a.relname, b.attname, c.typname, b.atttypmod, b.attnum
from pg_class a, pg_attribute b, pg_type c
where a.relfilenode = b.attrelid and 
      a.relname in ('test', 'products') and b.attname not in ( 'ctid', 'oid', 'xmin', 'cmin', 'xmax', 'cmax', 'tableoid') and
      b.atttypid = c.typelem and c.typname not in ('point', 'line')
order by 1, 5

select table_name, table_schema from information_schema.tables where table_name = 'test' and table_type = 'BASE TABLE'

select * from information_schema.columns where table_name = 'test'

select * from information_schema.key_column_usage


SELECT a.ordinal_position, a.column_name, b.constraint_name, a.data_type, a.character_maximum_length, a.numeric_precision, a.numeric_scale
FROM information_schema.columns a 
LEFT OUTER JOIN information_schema.key_column_usage b ON (a.ordinal_position = b.ordinal_position and b.table_schema = 'public' and b.table_name = 'test')
WHERE a.table_schema = 'public' and a.table_name = 'test' 
ORDER BY a.ordinal_position

*/