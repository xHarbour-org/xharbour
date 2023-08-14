#ifdef XDB_SQLRDD
   REQUEST SQLRDD
   REQUEST SR_MYSQL
   REQUEST SR_FIREBIRD
   REQUEST SR_ODBC
#endif

REQUEST DBFCDX
REQUEST DBFFPT
REQUEST HB_CODEPAGE_DE850
REQUEST DESCEND

FUNCTION Main( ... )

//----------------------------------- [BEGIN SYSTEM CODE] ------------------------------------//
   __xDB( NIL ):Run( Form1( NIL ), HB_aParams() )
//------------------------------------ [END SYSTEM CODE] -------------------------------------//

RETURN NIL