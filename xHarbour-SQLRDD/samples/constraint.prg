/*
* SQLRDD Server Side Constraints Sample
* Copyright (c) 2005 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"

#define RECORDS_IN_TEST                   1000
#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

/*------------------------------------------------------------------------*/

Function Main( cDSN, lLog )

   local aStruct1 := {{"DEPARTMENT_ID_PK","C",8,0 },{"DEPARTMENT_DESCR","C",50,0}, {"COST_CENTER_ID","C",10,0}}
   local aStruct2 := {{"COST_CENTER_ID_PK","C",10,0 },{"COST_CENTER_DESCR","C",50,0}}
   local aStruct3 := {{"EMPLOYEE_ID_PK","C",8,0 },{"DEPARTMENT_ID","C",8,0 },{"EMPLOYEE_FIRSTNAME","C",50,0},{"EMPLOYEE_LASTNAME","C",50,0},{"EMPLOYEE_EMAIL","C",80,0}}

   local nCnn, i, oErr

   ? ""
   ? "constraint.exe"
   ? ""
   ? "Server Side Constraints Sample"
   ? "(c) 2005 - Marcelo Lombardo"
   ? ""

   Alert( "In current version, this sample works only with MySQL, MSSQL Server, Oracle and Postgres." )

   Connect( cDSN )    // see connect.prg

   SR_UseDeleteds( .F. )      // Don't keep deleted records in database

   ? "Connected to        :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )

   If lLog != NIL
      ? "Starting LOG", SR_GetActiveConnection(), SR_StartLog()
   endif

   RddSetDefault( "SQLRDD" )

   // Please note table creation order is VERY important.
   // If you chage it, you would not run sample twice

   ? "Creating table EMPLOYEE    :", dbCreate( "EMPLOYEE", aStruct3 )
   ? "Creating EMPLOYEE PRIMARY KEY..."
   USE "EMPLOYEE" NEW
   INDEX ON EMPLOYEE_ID_PK TAG EMPLOYEE_ID CONSTRAINT EMPLOYEE_PK TARGET EMPLOYEE KEY EMPLOYEE_ID_PK
   ? ""

   ? "Creating table DEPARTMENT  :", dbCreate( "DEPARTMENT", aStruct1 )
   ? "Creating DEPARTMENT PRIMARY KEY..."
   USE "DEPARTMENT" NEW
   INDEX ON DEPARTMENT_ID_PK TAG DEPARTMENT_ID CONSTRAINT DEPARTMENT_PK TARGET DEPARTMENT KEY DEPARTMENT_ID_PK
   ? ""

   ? "Creating table COST_CENTER :", dbCreate( "COST_CENTER", aStruct2 )
   ? "Creating COST_CENTER PRIMARY KEY..."
   USE "COST_CENTER" NEW
   INDEX ON COST_CENTER_ID_PK TAG COST_CENTER_ID CONSTRAINT COST_CENTER_PK TARGET COST_CENTER KEY COST_CENTER_ID_PK
   ? ""

   SELECT EMPLOYEE

   ? "Creating EMPLOYEE -> DEPARTMENT FOREIGN KEY..."
   INDEX ON DEPARTMENT_ID TAG EMPLOYEE_ID CONSTRAINT EMPLOYEE_FK1 TARGET DEPARTMENT KEY DEPARTMENT_ID_PK
   ? ""

   ? "Creating remaining EMPLOYEE indexes..."
   INDEX ON EMPLOYEE_LASTNAME  TAG EMPLOYEE_LASTNAME
   INDEX ON EMPLOYEE_FIRSTNAME TAG EMPLOYEE_FIRSTNAME
   ? ""

   SELECT COST_CENTER

   ? "Creating remaining COST_CENTER indexes..."
   INDEX ON COST_CENTER_DESCR  TAG COST_CENTER_DESCR
   ? ""

   SELECT DEPARTMENT

   ? "Creating DEPARTMENT -> COST_CENTER FOREIGN KEY..."
   INDEX ON COST_CENTER_ID TAG COST_CENTER_ID CONSTRAINT DEPARTMENT_FK1 TARGET COST_CENTER KEY COST_CENTER_ID_PK
   ? ""

   ? "Creating remaining DEPARTMENT indexes..."
   INDEX ON DEPARTMENT_DESCR  TAG DEPARTMENT_DESCR
   ? ""

   ? "Add some cost centers..."
   SELECT COST_CENTER
   Append Blank
   Replace COST_CENTER_ID_PK  with "1.01.001"
   Replace COST_CENTER_DESCR  with "Sales"
   Append Blank
   Replace COST_CENTER_ID_PK  with "1.01.002"
   Replace COST_CENTER_DESCR  with "Manufacturing"
   Append Blank
   Replace COST_CENTER_ID_PK  with "1.01.003"
   Replace COST_CENTER_DESCR  with "Administration"
   dbUnlock()
   dbCommit()

   ? ""
   ? "We have PRIMERY KEY defined, so it will NOT allow duplicate cost center ID"
   ? ""
   ? "   **** Run time error MUST happen ****"
   ? ""

   TRY
      // Try to push a DUPLICATE record
      Append Blank
      Replace COST_CENTER_ID_PK  with "1.01.001"      // DUPLICATED ID
      Replace COST_CENTER_DESCR  with "Sales"
      dbCommit()
   CATCH oErr
      ? oErr:Description
   END
   wait

   clear screen
   ? "Add some cost departments..."
   SELECT DEPARTMENT
   Append Blank
   Replace DEPARTMENT_ID_PK  with "001"
   Replace DEPARTMENT_DESCR  with "Commercial"
   Replace COST_CENTER_ID    with "1.01.001"

   Append Blank
   Replace DEPARTMENT_ID_PK  with "003"
   Replace DEPARTMENT_DESCR  with "Accounting"
   Replace COST_CENTER_ID    with "1.01.003"
   dbUnlock()
   dbCommit()

   ? ""
   ? "We have FOREIGN KEY defined, so it will NOT allow to add a department"
   ? "in a cost center that DOES NOT EXIST"
   ? ""
   ? "   **** Run time error MUST happen ****"
   ? ""

   TRY
      Append Blank
      Replace DEPARTMENT_ID_PK  with "005"
      Replace DEPARTMENT_DESCR  with "Advertising"
      Replace COST_CENTER_ID    with "1.01.005"
      dbCommit()
   CATCH oErr
      ? oErr:Description
   END
   wait

   clear screen
   ? "Add some cost employees..."
   SELECT EMPLOYEE
   Append Blank
   Replace EMPLOYEE_ID_PK     with "0001"
   Replace DEPARTMENT_ID      with "001"
   Replace EMPLOYEE_FIRSTNAME with "James"
   Replace EMPLOYEE_LASTNAME  with "Labrie"

   Append Blank
   Replace EMPLOYEE_ID_PK     with "0002"
   Replace DEPARTMENT_ID      with "001"
   Replace EMPLOYEE_FIRSTNAME with "John"
   Replace EMPLOYEE_LASTNAME  with "Petrucci"

   Append Blank
   Replace EMPLOYEE_ID_PK     with "0003"
   Replace DEPARTMENT_ID      with "001"
   Replace EMPLOYEE_FIRSTNAME with "Mark"
   Replace EMPLOYEE_LASTNAME  with "Portnoy"

   Append Blank
   Replace EMPLOYEE_ID_PK     with "0004"
   Replace DEPARTMENT_ID      with "003"
   Replace EMPLOYEE_FIRSTNAME with "Jodan"
   Replace EMPLOYEE_LASTNAME  with "Rudess"

   Append Blank
   Replace EMPLOYEE_ID_PK     with "0005"
   Replace DEPARTMENT_ID      with "003"
   Replace EMPLOYEE_FIRSTNAME with "Jhon"
   Replace EMPLOYEE_LASTNAME  with "Myung"
   dbUnlock()
   dbCommit()

   clear screen
   ? ""
   ? "We have FOREIGN KEY defined, so it will NOT allow to DELETE a department"
   ? "where there are employees connected"
   ? ""
   ? "   **** Run time error MUST happen ****"
   ? ""

   SELECT DEPARTMENT
   Set Order to "DEPARTMENT_ID"
   Seek "001"

   If Found()
      TRY
         dbDelete()
         dbCommit()
      CATCH oErr
         ? oErr:Description
      END
   Else
      ? "Department NOT FOUND"
   EndIf
   wait

Return NIL

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/