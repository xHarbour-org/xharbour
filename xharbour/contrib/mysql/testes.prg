/*
 * $Id: testes.prg,v 1.0 2001/04/12 18:24:40 dholm Exp $
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

      aStru := dbStruct()        // Cargo la estructura del dbf en un array

      for each aField in aStru
         if HB_EnumIndex() <= 2
            AADD( aField, .t. )  // Agrego una columna m s al array con
                                 // las definiciones de los dos primeros
                                 // campos, que indica que estas columnas
                                 // son NOT NULL. Esto es para poder incluirlas
                                 // como parte de la clave primaria.
         else
            exit
         endif
      next

      // Creo una tabla llamada "test", con la estructura contenida en aStru
      // y le indico que las columnas NAME y OWNER son clave primaria.
      //
      if oServer:CreateTable("test", aStru, "NAME,OWNER")
         Alert("test created successfully")
      else
         Alert(oServer:Error())
      endif

   endif


/*** Anulo esta parte porque no todos van a tener la tabla animal    ***
 *** creada, pero sirve a los fines did cticos de mostrar la lectura ***
 *** de una tabla de MySQL y el guardado de los datos en un dbf.     ***

   oQuery:=oServer:Query("SELECT * from animal", .f. )
//   oQuery:GetRow( oQuery:nCurRow )

   if oQuery:LastRec() > 0
      zap                  // Borramos todos los registros de la dbf
                           // para comenzar nuevamente el test.
      do while !oQuery:eof() .and. inkey()#-99
         append blank

//         replace name       with oQuery:FieldGet( oQuery:FieldPos( "NAME" ) )
//         replace owner      with oQuery:FieldGet( oQuery:FieldPos( "OWNER" ) )
//         replace species    with oQuery:FieldGet( oQuery:FieldPos( "SPECIES" ) )
//         replace sex        with oQuery:FieldGet( oQuery:FieldPos( "SEX" ) )
//         replace birth      with oQuery:FieldGet( oQuery:FieldPos( "BIRTH" ) )
//         replace death      with oQuery:FieldGet( oQuery:FieldPos( "DEATH" ) )

         // Ver n que esta forma de escribir es mucho m s legible
         // que la de arriba.
         // Lo que est  haciendo es cargar en la dbf los datos de la
         // tabla animal que est  en el server de MySQL
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

            // En esta rutina, estamos actualizando los datos de la tabla
            // en el server. Cuando encontramos un dato que existe en el dbf
            // y en la tabla, lo actualizamos.

/*
            oRow := :GetRow()

            oRow:FieldPut(oRow:FieldPos("SPECIES"), animal->SPECIES)
            oRow:FieldPut(oRow:FieldPos("SEX"),     animal->SEX)
            oRow:FieldPut(oRow:FieldPos("BIRTH"),   animal->BIRTH)
            oRow:FieldPut(oRow:FieldPos("DEATH"),   animal->DEATH)

            if !:Update(oRow)       // Orden de actualizar los datos
                                    // del registro contenido en oRow.
               Alert(:Error())
            endif
*/

            // Otra vez tenemos una muestra de como es mucho m s legible
            // esta forma de acceder a los datos. Esta vez el ejemplo es
            // a la hora de guardar los datos en la tabla de MySQL.
            // Arriba, la forma anterior. Que por compatibilidad se mantiene.
            //
            :SPECIES    := animal->SPECIES
            :SEX        := animal->SEX
            :BIRTH      := animal->BIRTH
            :DEATH      := animal->DEATH

            if !:Update()           // Orden de actualizar los datos
                                    // del registro actual.
               Alert(:Error())
            endif

         else

            ? " --> append of data <--"

            // En esta otra parte, estamos agregando los datos que no
            // existen en la tabla "test".

/*
            // La forma de trabajo con oRow, no est  descartada y no es obsoleta.
            // Es otra forma de hacer las cosas.
            // Usar oRow sirve para tener varios registros podriamos decir
            // "cacheados", para evitar tener que movernos hasta ellos para
            // verlos o modificarlos.
            // Tambi‚n sirve para modificar o dar de alta unos cuantos registros
            // y luego volcarlos en otra rutina a todos juntos en el server.
            // A£n no est  implementado el acceso a oRow con el formato
            // oRow:NAME, como s¡ se puede usar oQuery:NAME.
            // Las ventajas que ten¡a el uso de oRow, eran que permit¡an un
            // acceso m s r pido a los datos en especial en listados, ya que
            // los campos pod¡an ser accedidos de la forma oRow:aRow[n].
            // Ahora tambi‚n es posible hacer oQuery:aRow[n].

            oRow := :GetBlankRow()

            ? "FieldPos(Name)",oRow:FieldPos("NAME")

            oRow:FieldPut(oRow:FieldPos("NAME"),    animal->NAME)
            oRow:FieldPut(oRow:FieldPos("OWNER"),   animal->OWNER)
            oRow:FieldPut(oRow:FieldPos("SPECIES"), animal->SPECIES)
            oRow:FieldPut(oRow:FieldPos("SEX"),     animal->SEX)
            oRow:FieldPut(oRow:FieldPos("BIRTH"),   animal->BIRTH)
            oRow:FieldPut(oRow:FieldPos("DEATH"),   animal->DEATH)

            if !:Append(oRow)       // Orden de agregar (INSERT) los
                                    // datos del registro en oRow.
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


            if !:Append()           // Orden de agregar (INSERT) los
                                    // datos del registro actual.
               Alert(:Error())
            endif

         endif
      END // WITH

      animal->(dbSkip())

   enddo


   animal->(dbCloseArea())

   oServer:Destroy()
return

