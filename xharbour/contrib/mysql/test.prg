/*
 * $Id: test.prg,v 1.0 2001/04/12 18:24:40 dholm Exp $
 */

/*
 * xHarbour Project source code:
 * MySQL DBMS test program
 *
 * Copyright 2003 Walter Negro <anegro@overnet.com.ar>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "dbstruct.ch"

procedure main( )

   local aArg := {}, oServer, oQuery, oQuery2, oRow, i, aStru, cRow, aField
   local lDelTest := .f., server := "localhost", user := "root", password := ""

   SET CENTURY ON

   i := 1
   do while !empty( hb_argv( i ) )
      do case
      case hb_argv( i ) == "-d"
         lDelTest := .t.
      case hb_argv( i ) == "-s"
         server := hb_argv( ++i )
      case hb_argv( i ) == "-u"
         user := hb_argv( ++i )
      case hb_argv( i ) == "-p"
         password := hb_argv( ++i )
      endcase
      i++
   enddo

   oServer := TMySQLServer():New(server, user, password)
   if oServer:NetErr()
      Alert(oServer:Error())
   endif

   if !oServer:DBExist( "zoo" )
      oServer:CreateDatabase( "zoo" )
   endif

   oServer:SelectDB("zoo")

   dbUseArea(.T.,, 'animal', "animal", .F.)


   if "-d" IN cArg
      if oServer:TableExist( "test" )
         if oServer:DeleteTable("test")
            Alert("test deleted successfully")
         else
            Alert(oServer:Error())
         endif
      endif
   endif


   if oServer:TableExist( "test" )

      aStru := dbStruct()        // Load the structure of the dbf in an array

      for each aField in aStru
         if HB_EnumIndex() <= 2
            AADD( aField, .t. )  // Add a new column to the array of definitions 
                                 // of the first two fields. That it indicates 
                                 // that these columns are NOT NULL. 
                                 // This is to be able to include them like part 
                                 // of the primary key.
         else
            exit
         endif
      next

      // Make a table called "test", with the structure contained in aStru and indicate 
      // it that the columns NAME and OWNER are key primary.
      //
      if oServer:CreateTable("test", aStru, "NAME,OWNER")
         Alert("test created successfully")
      else
         Alert(oServer:Error())
      endif

   endif


/*** I remove this part because not all will have created the animal table, ***
 *** but it serves to the didactic ends as showing the reading of a table   ***
 *** of MySQL and the archive of data in a dbf.                             ***

   oQuery:=oServer:Query("SELECT * from animal", .f. )
//   oQuery:GetRow( oQuery:nCurRow )

   if oQuery:LastRec() > 0
      zap                  // Delete all of register in the database
                           // for new test..
      do while !oQuery:eof() .and. inkey()#-99
         append blank

//         replace name       with oQuery:FieldGet( oQuery:FieldPos( "NAME" ) )
//         replace owner      with oQuery:FieldGet( oQuery:FieldPos( "OWNER" ) )
//         replace species    with oQuery:FieldGet( oQuery:FieldPos( "SPECIES" ) )
//         replace sex        with oQuery:FieldGet( oQuery:FieldPos( "SEX" ) )
//         replace birth      with oQuery:FieldGet( oQuery:FieldPos( "BIRTH" ) )
//         replace death      with oQuery:FieldGet( oQuery:FieldPos( "DEATH" ) )

         // You will see that this form of writing is more readable 
         // that the one of up.  
         // What is making is to load in the dbf the data of the 
         // animal table that it is in the server of MySQL.
         //
         replace name       with oQuery:NAME
         replace owner      with oQuery:OWNER
         replace species    with oQuery:SPECIES
         replace sex        with oQuery:SEX
         replace birth      with oQuery:BIRTH
         replace death      with oQuery:DEATH

         oQuery:skip()

      enddo
   endif

*/

   animal->(dbgotop())

   while !animal->(eof())

      oQuery2 := oServer:Query("SELECT * from test where NAME='" + animal->NAME + "' and OWNER='" + animal->OWNER + "'", .f. )

      WITH OBJECT oQuery2         // This generate a "cache" of this object
                                  // that permit access more quickly than using
                                  // oQuery2:..., and it's more readable.
         :GetRow()

         ? animal->name,"|",animal->owner,"*"

         if :LastRec() > 0

            ? " --> modification of data <--"

            // In this routine, we are updating the data of the table in the server. 
            // When we find a fact that exists in the dbf and in the table, 
            // we update the table in MySQL.

/*
            oRow := :GetRow()

            oRow:FieldPut(oRow:FieldPos("SPECIES"), animal->SPECIES)
            oRow:FieldPut(oRow:FieldPos("SEX"),     animal->SEX)
            oRow:FieldPut(oRow:FieldPos("BIRTH"),   animal->BIRTH)
            oRow:FieldPut(oRow:FieldPos("DEATH"),   animal->DEATH)

            if !:Update(oRow)       // Order of update the data contained 
                                    // in the object oRow.
               Alert(:Error())
            endif
*/

            // Another time we have a sample of like it is much more readable 
            // this form of accessing to the data.
            // This example is when saving the data in the table of MySQL.
            // Up, the previous form.
            //
            :SPECIES    := animal->SPECIES
            :SEX        := animal->SEX
            :BIRTH      := animal->BIRTH
            :DEATH      := animal->DEATH

            if !:Update()           // Order of update the data contained 
                                    // in the object oRow.
               Alert(:Error())
            endif

         else

            ? " --> append of data <--"

            // In this other part, we are adding the data that
            // don't exist in the table "test".

/*
            // The work form with oRow, is not discarded and it is not obsolete.
            // It is another form of making the same thing.
            // To use oRow is good to have several "cached" registers, 
            // to avoid to have to move until them to see them or to modify them.
            // (ex: Browse)
            // It is also good to modify or to append several registers 
            // and then to save them in another routine to the server.
            // The access to oRow with the format oRow:NAME like oQuery:NAME
            // is not still implemented.
            // The advantages that it had the oRow use, were that they allowed 
            // a quicker access especially to the data in listings, 
            // since the fields could be accessed of the form oRow:aRow[n].
            // Now also it is possible to use oQuery:aRow[n].

            oRow := :GetBlankRow()

            ? "FieldPos(Name)",oRow:FieldPos("NAME")

            oRow:FieldPut(oRow:FieldPos("NAME"),    animal->NAME)
            oRow:FieldPut(oRow:FieldPos("OWNER"),   animal->OWNER)
            oRow:FieldPut(oRow:FieldPos("SPECIES"), animal->SPECIES)
            oRow:FieldPut(oRow:FieldPos("SEX"),     animal->SEX)
            oRow:FieldPut(oRow:FieldPos("BIRTH"),   animal->BIRTH)
            oRow:FieldPut(oRow:FieldPos("DEATH"),   animal->DEATH)

            if !:Append(oRow)       // Order of add the data (INSERT) contained 
                                    // in the object oRow.
               Alert(:Error())
            endif
*/

            :GetBlankRow( )

            :NAME       := animal->NAME
            :OWNER      := animal->OWNER
            :SPECIES    := animal->SPECIES
            :SEX        := animal->SEX
            :BIRTH      := animal->BIRTH
            :DEATH      := animal->DEATH


            if !:Append()           // Order of add the data (INSERT) contained 
                                    // in the object oRow.
               Alert(:Error())
            endif

         endif
      END // WITH

      animal->(dbSkip())

   enddo


   animal->(dbCloseArea())

   oServer:Destroy()
return

